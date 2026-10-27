(*
	The query graph of the compilation server (docs/incremental-compilation.md,
	phase 6): for each context, the fingerprint of every node that a module
	produced (header and members of its types, signature and inline body of its
	fields) and what each computation read (phase 5), with the fingerprint it
	saw.

	A module of the cache whose file did not change is valid when everything
	its computations read is still what they saw: a declaration with the same
	fingerprint, a lookup that finds the same file, a node with the same
	fingerprint whose own computation is valid in turn (it would compute the
	same value again, phase 5). A change in a module therefore only invalidates
	the modules that read what changed, instead of every module that depends on
	it (early cutoff).

	A computation that ran a macro also depends on every declaration of its
	file, on its own module and on what the macro context executed for it: the
	bodies of the functions, the members of the classes instantiated, checked
	in the graph of the macro context (or, when it is not known, on the
	module of the macro and its dependencies).

	Nodes are records updated in place when their module is typed again, so
	that edges refer to them directly: validating needs no lookup by key.
*)
open Globals
open Type

let trace = Sys.getenv_opt "HAXE_GRAPH_TRACE" <> None

(* HAXE_GRAPH_TRACE=/path/to/file appends the trace to that file *)
let trace_line =
	match Sys.getenv_opt "HAXE_GRAPH_TRACE" with
	| Some file when String.length file > 0 && file.[0] = '/' ->
		let ch = open_out_gen [Open_append;Open_creat] 0o644 file in
		(fun s -> output_string ch s; output_char ch '\n'; Stdlib.flush ch)
	| _ -> prerr_endline

type node = {
	n_key : string;
	mutable n_owner : path;
	(* the fingerprint of the node, None if its module does not produce it *)
	mutable n_fp : TFingerprint.t option;
	(* what the node read, when it is a computation *)
	mutable n_edges : edge array option;
	(* the computation that produces the node: itself, the body (or inferred
	   signature) of a field for its inline body, the declared signature or the
	   members for a signature (found on first use) *)
	mutable n_by : node option;
	mutable n_by_known : bool;
	(* validity in the current compilation *)
	mutable n_step : int;
	mutable n_valid : bool;
	(* the compilation (validation step) that last stored the node *)
	mutable n_stored : int;
}

and edge =
	(* a declaration of a file ("name\tnode") and the fingerprint seen
	   (both lanes, [no_fp] if it did not exist): unboxed, the graph is big *)
	| Leaf of file * string * int * int
	(* the lookup of a module path, and the file it found ("" if none) *)
	| Lookup of lookup * string
	(* a node and the fingerprint seen *)
	| Node of node * int * int
	(* a module that must not be typed again (it holds instances of @:generic
	   methods made for the computation, or it ran macros) *)
	| Module of path
	(* the module of a macro that ran, in the macro context (its signature):
	   when what the expansion executed is not known *)
	| MacroModule of string * path
	(* a node of the macro context (its signature) and the fingerprint seen:
	   the body of a function an expansion executed, the members of a class it
	   instantiated (phase 7) *)
	| MacroNode of string * node * int * int
	(* the members of a class (its node) have no field of a name (a member,
	   or a static): a field looked up and not found there *)
	| Absent of node * string * bool

and file = {
	f_path : string;
	(* the module typed from the file, the time of the file and the
	   fingerprints of its declarations ("name\tnode") *)
	mutable f_module : path;
	mutable f_time : float;
	mutable f_decls : (string,TFingerprint.t) Hashtbl.t option;
	(* the current declarations, in the current compilation, and whether they
	   are the ones stored (the file did not change) *)
	mutable f_step : int;
	mutable f_current : (string,TFingerprint.t) Hashtbl.t option;
	mutable f_unchanged : bool;
	(* the keys of the leaves of the file, shared by the edges *)
	f_keys : (string,string) Hashtbl.t;
}

and lookup = {
	l_path : path;
	mutable l_step : int;
	mutable l_current : string;
	(* the directory epoch of the graph when [l_current] was resolved *)
	mutable l_epoch : int;
}

type graph = {
	nodes : (string,node) Hashtbl.t;
	module_nodes : (path,node list) Hashtbl.t;
	files : (string,file) Hashtbl.t;
	(* the file of each module stored *)
	module_files : (path,file) Hashtbl.t;
	lookups : (path,lookup) Hashtbl.t;
	mutable step : int;
	(* the nodes assumed to keep their fingerprint in the current compilation,
	   although their module is typed again (early cutoff), with that fingerprint *)
	mutable assumptions : assumption list;
	(* incremented when a file may have been added or removed in the class
	   paths (a directory changed, or the class paths did): until then the
	   lookups find the same files *)
	mutable dirs_epoch : int;
	mutable class_paths : string list;
}

(* Early cutoff: a node keeps its fingerprint, the members of a class still
   have no field of a name. *)
and assumption =
	| ANode of node * TFingerprint.t option
	| AAbsent of node * string * bool

(* A compilation made assumptions that turned out false: it is done again,
   without assumptions. *)
exception Retry

(* The nodes known to have changed in the current compilation: an assumption
   about them failed in a previous attempt, which found their new fingerprint.
   They cannot be assumed again, and an edge to them is checked against that
   fingerprint (the graph keeps the previous compilation's until a compilation
   succeeds). *)
let known_changed : (string,TFingerprint.t option) Hashtbl.t = Hashtbl.create 0

let absent_key n name static = n.n_key ^ "\t" ^ DepRecord.absent_node name static

(* The number of attempts of the current compilation that failed an
   assumption. *)
let attempts = ref 0
(* Whether the current attempt checked modules through the graph, and whether
   the next one checks them through their module dependencies only: a
   computation that the graph finds valid can fail when typed again, through
   what it does not record (a build macro failing on a body of the class it
   builds), and the cold compilation reports the errors of what reads it. *)
let graph_used = ref false
let modules_only = ref false
(* A display request (--display) can send its result before typing is done:
   it is not done again for its errors. *)
let display_request = ref false

let clear_current = ref (fun () -> ())
let clear_macro_validation = ref (fun () -> ())

(* The files of the lexer at the end of the previous compilation: a position
   in a file the lexer did not read again is still right. *)
let lexer_files : (string,Lexer.lexer_file) Hashtbl.t = Hashtbl.create 0

let end_compilation () =
	!clear_macro_validation ();
	DepRecord.reused := [];
	!clear_current ();
	Hashtbl.reset lexer_files;
	ignore (ThreadSafeHashtbl.fold (fun file lf () -> Hashtbl.replace lexer_files file lf) Lexer.all_files ());
	attempts := 0;
	graph_used := false;
	modules_only := false;
	Hashtbl.reset known_changed

let graphs : (string,graph) Hashtbl.t = Hashtbl.create 0

let get sign =
	try Hashtbl.find graphs sign with Not_found ->
		let g = {
			nodes = Hashtbl.create 0;
			module_nodes = Hashtbl.create 0;
			files = Hashtbl.create 0;
			module_files = Hashtbl.create 0;
			lookups = Hashtbl.create 0;
			step = -1;
			assumptions = [];
			dirs_epoch = 0;
			class_paths = [];
		} in
		Hashtbl.add graphs sign g;
		g

let find_opt sign = Hashtbl.find_opt graphs sign

let clear () = Hashtbl.reset graphs

let node g key =
	try Hashtbl.find g.nodes key with Not_found ->
		let n = { n_key = key; n_owner = ([],""); n_fp = None; n_edges = None; n_by = None; n_by_known = false; n_step = -1; n_valid = false; n_stored = -1 } in
		Hashtbl.add g.nodes key n;
		n

let file g path =
	try Hashtbl.find g.files path with Not_found ->
		let f = { f_path = path; f_module = ([],""); f_time = 0.; f_decls = None; f_step = -1; f_current = None; f_unchanged = false; f_keys = Hashtbl.create 0 } in
		Hashtbl.add g.files path f;
		f

let lookup g path =
	try Hashtbl.find g.lookups path with Not_found ->
		let l = { l_path = path; l_step = -1; l_current = ""; l_epoch = -1 } in
		Hashtbl.add g.lookups path l;
		l

(* The edges to build, of the previous stores (see [store]). *)
let pending : (unit -> unit) list ref = ref []

let flush () =
	let l = List.rev !pending in
	pending := [];
	List.iter (fun f -> f ()) l

let path_of_key key =
	match List.rev (String.split_on_char '.' key) with
	| name :: pack -> List.rev pack,name
	| [] -> [],key

let is_computation n = n.n_edges <> None

(* a fingerprint that may not exist, as two ints *)
let no_fp = min_int
let fp_lanes = function Some (a,b) -> a,b | None -> no_fp,no_fp
let same_fp o a b = match o with Some (a',b') -> a = a' && b = b' | None -> a = no_fp

(* The computation that produces node [key], among the computations of its module. *)
let producer_of g key =
	let find l = List.find_map (fun k -> match Hashtbl.find_opt g.nodes k with Some n when is_computation n -> Some n | _ -> None) l in
	match String.index_opt key '\t' with
	| None -> None
	| Some i ->
		let k = String.sub key 0 i and node = String.sub key (i + 1) (String.length key - i - 1) in
		match node with
		| "inline" | "exec" ->
			find [k ^ "\tbody";k ^ "\tsig"]
		| "sig" ->
			begin match String.rindex_opt k '(' with
			| Some paren when paren > 1 ->
				let base = String.sub k 0 (paren - 1) in
				let tkey =
					if String.length k > 13 && String.sub k (String.length k - 13) 13 = "(constructor)" then base
					else match String.rindex_opt base '.' with Some j -> String.sub base 0 j | None -> base
				in
				find [key;k ^ "\tdeclared sig";tkey ^ "\tmembers"]
			| _ ->
				find [key]
			end
		| _ ->
			find [key]

(* Recording, after typing *)

let decl_table file decls =
	let h = Hashtbl.create 0 in
	List.iter (fun (name,node,fp) -> Hashtbl.replace h (name ^ "\t" ^ node) fp) (TypeloadParse.find_decl_fingerprints file decls);
	h

(* Whether this compilation found macro module [path] of the context of
   signature [sign] valid, without typing it again (set by the validation). *)
let macro_module_checked : (string -> path -> bool) ref = ref (fun _ _ -> false)

(* Whether expression [e] (of the macro context) makes values holding its
   positions: reified expressions ([$__mk_pos__]), [haxe.PosInfos]. *)
let uses_macro_positions e =
	let rec loop e = match e.eexpr with
		| TIdent "$__mk_pos__" -> raise Exit
		| TObjectDecl fl when List.exists (fun ((n,_,_),_) -> n = "lineNumber") fl -> raise Exit
		| _ -> Type.iter loop e
	in
	try loop e; false with Exit -> true

(* Whether node [n] of the graph of the macro context [sign] was found valid
   by the validation of this compilation, with the fingerprint seen. *)
(* The check of a node of the macro context not checked (or forgotten) by
   the validation: set once the validation is defined. *)
let check_macro_node : (string -> node -> int -> int -> bool) ref = ref (fun _ _ _ _ -> false)

let macro_node_checked sign n a b =
	match Hashtbl.find_opt graphs sign with
	| None -> false
	| Some g ->
		same_fp n.n_fp a b &&
		let p = if is_computation n then Some n else if n.n_by_known then n.n_by else None in
		match p with
		| Some p when is_computation p && p.n_step = g.step -> p.n_valid
		| None when n.n_step = g.step -> n.n_valid
		| _ -> !check_macro_node sign n a b

(* Prepares the storage of the nodes of the modules of [com] that were typed in
   this compilation (the computations recorded by [DepRecord]). [resolve path]
   is the file a lookup of [path] finds ("" if none), [decls_of m] the
   declarations [m] was typed from, [is_typed m] whether [m] was typed by this
   compilation. The result checks the assumptions of the compilation (the
   nodes that failed, with their new fingerprint), and stores. *)
let prepare_store com resolve decls_of is_typed =
	flush ();
	let sign = CommonCache.get_cache_sign com in
	let g = get sign in
	(* the modules typed in this compilation, with their computations *)
	let typed = Hashtbl.create 0 in
	com.module_lut#iter (fun path m ->
		if m.m_extra.m_sign = sign && (m.m_extra.m_kind = MCode || m.m_extra.m_kind = MMacro) && is_typed m then Hashtbl.replace typed path []
	);
	List.iter (fun c ->
		let m = c.DepRecord.c_module in
		match Hashtbl.find_opt typed m.m_path with
		| Some l when (match com.module_lut#find_opt m.m_path with Some m' -> m' == m | None -> false) -> Hashtbl.replace typed m.m_path (c :: l)
		| _ -> ()
	) !DepRecord.consumers;
	let modules = Hashtbl.fold (fun path cl acc -> (com.module_lut#find path,cl) :: acc) typed [] in
	DepRecord.last_typed := List.fold_left (fun acc (m,_) -> s_type_path m.m_path :: acc) !DepRecord.last_typed modules;
	let t_fps = Timer.start_timer com.timer_ctx ["server";"query graph";"fingerprints"] in
	List.iter (fun (m,_) -> TFingerprint.force_lazies m) modules;
	(* the fingerprints of their nodes and declarations, in parallel: the
	   types are only read once typing is done *)
	let computed = Parallel.run_with_pool com.sctx.pool (fun pool ->
		Parallel.ParallelArray.map pool (fun (m,_) ->
			let fps = ref [] in
			EntityKey.iter_module (fun key e ->
				let k = EntityKey.to_string key in
				let set node fp = fps := (k ^ "\t" ^ node,fp) :: !fps in
				match e with
				| EntityKey.EType mt ->
					set "header" (TFingerprint.type_header mt);
					set "members" (TFingerprint.type_members mt)
				| EntityKey.EField (c,cf) ->
					set "sig" (TFingerprint.field_sig cf);
					if TFingerprint.has_inline_body c cf then set "inline" (TFingerprint.field_inline_body cf);
					(* the code the expansions run *)
					if com.is_macro_context then Option.may (fun e -> set "exec" (TFingerprint.with_state ~positions:false (fun h -> TFingerprint.add_expr h e))) cf.cf_expr
				| EntityKey.EAnonField (_,cf) ->
					set "sig" (TFingerprint.field_sig cf)
				| EntityKey.EEnumField _ | EntityKey.ETypeParam _ ->
					()
			) m;
			let file = Path.get_full_path (Path.UniqueKey.lazy_path m.m_extra.m_file) in
			List.rev !fps,file,Option.map (decl_table file) (decls_of m)
		) (LargeArray.of_list modules) ([],"",None)
	) in
	t_fps ();
	(* the assumptions that do not hold: the node is not produced again by a
	   module typed in this compilation, or with another fingerprint *)
	let failed_assumptions () =
		if g.assumptions = [] then []
		else begin
			let fps = Hashtbl.create 0 and typed_paths = Hashtbl.create 0 in
			List.iteri (fun i (m,_) ->
				Hashtbl.replace typed_paths m.m_path ();
				let l,_,_ = computed.(i) in
				List.iter (fun (key,fp) -> Hashtbl.replace fps key fp) l
			) modules;
			(* the classes of the typed modules, for the absences *)
			let classes = Hashtbl.create 0 in
			List.iter (fun (m,_) -> List.iter (function
				| TClassDecl c -> Hashtbl.replace classes (s_type_path c.cl_path ^ "\tmembers") c
				| _ -> ()
			) m.m_types) modules;
			let absent_holds n name static =
				match Hashtbl.find_opt classes n.n_key with
				| Some c ->
					if name = "new" && not static then
						(* a constructor inherited from the parent (added after typing it) is what
						   the computation found: the one of the parent *)
						(match c.cl_constructor with None -> true | Some cf -> has_class_field_flag cf CfInheritedConstructor)
					else not (PMap.mem name (if static then c.cl_statics else c.cl_fields))
				| None ->
					false
			in
			(* The fingerprint of a node after typing: the one computed if its module
			   was typed again, else the one it had. *)
			let fp_now n = if Hashtbl.mem typed_paths n.n_owner then Hashtbl.find_opt fps n.n_key else n.n_fp in
			(* Whether computation [p] read the same as when it ran: what it computes,
			   kept by a module not typed again, holds. The validation found it invalid
			   on what it knew then (modules found changed later, assumptions); after
			   typing, the fingerprints are known (a cycle reads the same). *)
			let same_inputs_memo = Hashtbl.create 0 in
			let rec same_inputs p =
				match Hashtbl.find_opt same_inputs_memo p.n_key with
				| Some b -> b
				| None ->
					Hashtbl.replace same_inputs_memo p.n_key true;
					let edges = match p.n_edges with Some l -> l | None -> [||] in
					let b = Array.for_all (fun e -> let r = (match e with
						| Leaf (f,key,a,b) ->
							f.f_step = g.step && (f.f_unchanged || same_fp (match f.f_current with Some h -> Hashtbl.find_opt h key | None -> None) a b)
						| Lookup (l,seen) ->
							l.l_step = g.step && l.l_current = seen
						| Node (n,a,b) ->
							same_fp (fp_now n) a b && (Hashtbl.mem typed_paths n.n_owner || same_produced n)
						| Absent (n,name,static) ->
							if Hashtbl.mem typed_paths n.n_owner then absent_holds n name static else same_produced n
						| Module path ->
							not (Hashtbl.mem typed_paths path)
						| MacroModule (sign,path) ->
							!macro_module_checked sign path
						| MacroNode (sign,n,a,b) ->
							macro_node_checked sign n a b
					) in
					if trace && not r then trace_line (Printf.sprintf "[graph] read changed by %s: %s" p.n_key (match e with Node (n,_,_) | MacroNode (_,n,_,_) | Absent (n,_,_) -> n.n_key | Leaf (_,k,_,_) -> k | _ -> "?"));
					r) edges in
					if trace && not b then trace_line (Printf.sprintf "[graph] read changed: %s" p.n_key);
					Hashtbl.replace same_inputs_memo p.n_key b;
					b
			(* node [n] of a module not typed again keeps its fingerprint: it is
			   the same value if what computes it read the same *)
			and same_produced n =
				let p = if n.n_by_known then n.n_by else producer_of g n.n_key in
				match p with
				| Some p when is_computation p -> same_inputs p
				| _ -> true
			in
			List.filter_map (function
				| ANode (n,seen) ->
					let now = Hashtbl.find_opt fps n.n_key in
					(* a node no module produces (no fingerprint before or after) keeps it *)
					if (Hashtbl.mem typed_paths n.n_owner || seen = None) && now = seen then None
					(* a node of a module that was not typed again (found valid before the
					   computation of the node was found invalid) keeps its fingerprint if
					   that computation read the same: depending on the order in which
					   modules are checked (several domains load them in any order), this
					   happens and compiling again would type more for nothing *)
					else if not (Hashtbl.mem typed_paths n.n_owner) && n.n_fp = seen && same_produced n then None
					else begin
						if trace then trace_line (Printf.sprintf "[graph] assumption failed: %s (seen %s, now %s, owner %s typed %b)" n.n_key (match seen with Some fp -> TFingerprint.to_string fp | None -> "none") (match now with Some fp -> TFingerprint.to_string fp | None -> "none") (s_type_path n.n_owner) (Hashtbl.mem typed_paths n.n_owner));
						Some (n.n_key,now)
					end
				| AAbsent (n,name,static) ->
					(* the members of a class not typed again keep their names if what
					   computes them read the same *)
					if absent_holds n name static || (not (Hashtbl.mem typed_paths n.n_owner) && same_produced n) then None
					else begin
						if trace then trace_line ("[graph] assumption failed: " ^ absent_key n name static);
						Some (absent_key n name static,None)
					end
			) g.assumptions
		end
	in
	(* the computations kept from the previous compilation (bodies not typed
	   again): they keep what they read *)
	let reused = Hashtbl.create 0 in
	List.iter (fun key -> Hashtbl.replace reused key ()) !DepRecord.reused;
	DepRecord.reused := [];
	let apply () =
	(* their nodes: the previous ones are emptied, the records stay *)
	List.iteri (fun i (m,cl) ->
		let fps,file_path,decls = computed.(i) in
		let kept = ref [] in
		List.iter (fun n ->
			if Hashtbl.mem reused n.n_key && n.n_edges <> None then kept := n :: !kept
			else begin n.n_fp <- None; n.n_edges <- None; n.n_by <- None; n.n_by_known <- false end;
			n.n_stored <- g.step
		) (try Hashtbl.find g.module_nodes m.m_path with Not_found -> []);
		let nodes = ref !kept in
		let seen = Hashtbl.create 16 in
		List.iter (fun n -> Hashtbl.replace seen n.n_key ()) !kept;
		let add key =
			let n = node g key in
			n.n_owner <- m.m_path;
			n.n_stored <- g.step;
			if not (Hashtbl.mem seen key) then begin
				Hashtbl.add seen key ();
				nodes := n :: !nodes
			end;
			n
		in
		List.iter (fun (key,fp) -> (add key).n_fp <- Some fp) fps;
		(* the computations are known before their edges are, for [producer_of] *)
		List.iter (fun c -> (add (c.DepRecord.c_key ^ "\t" ^ c.c_node)).n_edges <- Some [||]) cl;
		Hashtbl.replace g.module_nodes m.m_path !nodes;
		let f = file g file_path in
		f.f_module <- m.m_path;
		Hashtbl.replace g.module_files m.m_path f;
		f.f_time <- m.m_extra.m_time;
		f.f_decls <- decls;
		f.f_step <- -1
	) modules;

	(* line numbers in an inline body: read by the modules inlining it (the body
	   of another field is only read by its own module); known now, before the
	   filters change the expressions *)
	let uses_position = Hashtbl.create 0 in
	List.iter (fun (_,cl) -> List.iter (fun c ->
		match c.DepRecord.c_field with
		| Some cf when (match cf.cf_kind with Method MethInline | Var { v_read = AccInline } -> true | _ -> false) && (match cf.cf_expr with Some e -> TFingerprint.uses_line_numbers e | None -> false) ->
			Hashtbl.replace uses_position (c.c_key ^ "\t" ^ c.c_node) ()
		(* in the macro context, the body of a function whose values hold its
		   positions (reified expressions, PosInfos): the expansions running it
		   read them *)
		| Some cf when com.is_macro_context && c.c_node = "body" && (match cf.cf_expr with Some e -> uses_macro_positions e | None -> false) ->
			Hashtbl.replace uses_position (c.c_key ^ "\t" ^ c.c_node) ()
		| _ -> ()
	) cl) modules;
	(* the lookups, resolved now: in the compilation's directory and class paths *)
	let lookups = Hashtbl.create 0 in
	List.iter (fun (_,cl) -> List.iter (fun c ->
		DepRecord.iter_raw_deps (function
			| DepRecord.DType(path,"lookup") when not (Hashtbl.mem lookups (path_of_key (s_type_path path))) ->
				let path = path_of_key (s_type_path path) in
				Hashtbl.add lookups path (resolve path)
			| _ -> ()
		) c
	) cl) modules;
	(* The edges are built later (once the server answered, or before the next
	   compilation uses the graph): they only need the fingerprints, computed
	   above, and the computations. *)
	let build_edges () =
	(* each dependency read is converted once *)
	let dep_nodes = DepRecord.DepTbl.create 0 in
	(* the macro context: its graph, the module of a type *)
	let mcom = match com.get_macros () with Some mcom -> mcom | None -> com in
	let msign = CommonCache.get_cache_sign mcom in
	let mgraph = get msign in
	let macro_module ?(why="") tpath =
		if trace && why <> "" then trace_line ("[graph] macro module of " ^ s_type_path tpath ^ ": " ^ why);
		let mpath = try (mcom.module_lut#find_by_type tpath).m_path with Not_found -> tpath in
		MacroModule (msign,mpath)
	in
	(* what their computations read *)
	let resolve path = try Hashtbl.find lookups path with Not_found -> "" in
	List.iter (fun (m,cl) ->
		let f = file g (Path.get_full_path (Path.UniqueKey.lazy_path m.m_extra.m_file)) in
		(* the leaves of the file, shared by its computations *)
		let leaves = Hashtbl.create 16 in
		let leaf_key key =
			try Hashtbl.find leaves key with Not_found ->
				let key = try Hashtbl.find f.f_keys key with Not_found -> Hashtbl.add f.f_keys key key; key in
				let e = match f.f_decls with
					| Some h -> let a,b = fp_lanes (Hashtbl.find_opt h key) in Leaf (f,key,a,b)
					| None -> Leaf (f,key,0,0)
				in
				Hashtbl.add leaves key e;
				e
		in
		let leaf name node = leaf_key (name ^ "\t" ^ node) in
		(* the declared signatures of the fields of each type, and every declaration *)
		let signatures = Hashtbl.create 16 in
		let all_decls = ref [] in
		begin match f.f_decls with
		| Some h ->
			Hashtbl.iter (fun k _ ->
				match String.index_opt k '\t' with
				| Some i ->
					let name = String.sub k 0 i and node = String.sub k (i + 1) (String.length k - i - 1) in
					if node = "decls" then all_decls := leaf_key k :: !all_decls
					else if node = "decl signature" then begin match String.index_opt name '.' with
						| Some j when not (String.contains_from name (j + 1) '.') ->
							let tname = String.sub name 0 j in
							Hashtbl.replace signatures tname (leaf_key k :: (try Hashtbl.find signatures tname with Not_found -> []))
						| _ -> ()
					end
				| None -> ()
			) h
		| None -> ()
		end;
		List.iter (fun c ->
			(* a declared signature reads the declaration without its body *)
			let decl_node = if c.DepRecord.c_node = "declared sig" then "decl signature" else "decls" in
			let edges = ref [leaf c.c_decl decl_node;leaf "(imports)" "decls";leaf "(types)" "types"] in
			if c.c_node = "members" then begin
				edges := leaf c.c_decl "fields" :: !edges;
				List.iter (fun e -> edges := e :: !edges) (try Hashtbl.find signatures c.c_decl with Not_found -> [])
			end;
			if Hashtbl.mem uses_position (c.c_key ^ "\t" ^ c.c_node) then edges := leaf c.c_decl "position" :: !edges;
			let self = c.c_key ^ "\t" ^ c.c_node in
			(* a macro can read every declaration of the file (build macros read
			   the bodies of the fields), and what the module depends on besides
			   code: the files the macros declared (registerModuleDependency) *)
			let expansion = ref false and executed = ref false in
			DepRecord.iter_raw_deps (function
				(* in the macro context, a macro is not run (macro-in-macro) *)
				| DepRecord.DKey(_,"expansion") -> if not com.is_macro_context then expansion := true
				| DepRecord.DKey(_,("exec" | "exec-members" | "exec-init")) -> executed := true
				| _ -> ()
			) c;
			if !expansion then begin
				edges := Module m.m_path :: !edges;
				List.iter (fun e -> edges := e :: !edges) !all_decls
			end;
			DepRecord.iter_raw_deps (fun d ->
				match d with
				| DepRecord.DAbsent (path,name,static) ->
					let n = node g (s_type_path path ^ "\tmembers") in
					if n.n_key <> self then edges := Absent (n,name,static) :: !edges
				| _ ->
				match DepRecord.DepTbl.find_opt dep_nodes d with
				| Some (Some n) -> if n.n_key <> self then begin let a,b = fp_lanes n.n_fp in edges := Node (n,a,b) :: !edges end
				| Some None -> ()
				| None ->
					let key,node_kind = DepRecord.dep_key d in
					let k = key ^ "\t" ^ node_kind in
					begin match node_kind with
					| "lookup" -> let path = path_of_key key in edges := Lookup (lookup g path,resolve path) :: !edges
					| "generic" -> edges := Module (path_of_key key) :: !edges
					| "expansion" ->
						(* "macro pack.Type.field": what it executed is known; a macro loaded
						   and not run (the signature of a macro function) gives its
						   signature *)
						if not !executed && not com.is_macro_context then begin
							let name = String.sub key 6 (String.length key - 6) in
							let sig_node static = Hashtbl.find_opt mgraph.nodes (Printf.sprintf "%s (%s)\tsig" name (if static then "static" else "member")) in
							edges := (match (match sig_node true with Some n -> Some n | None -> sig_node false) with
								| Some n when n.n_fp <> None -> let a,b = fp_lanes n.n_fp in MacroNode (msign,n,a,b)
								| _ -> macro_module ~why:("expansion " ^ key ^ " in " ^ self) (path_of_key (match String.rindex_opt key '.' with Some j when String.length key > 6 -> String.sub key 6 (j - 6) | _ -> key))
							) :: !edges
						end
					| "exec" ->
						(* "pack.Type.field (static)", "pack.Type (constructor)": its code,
						   produced by its body (or its signature when its type is
						   inferred) *)
						edges := (match Hashtbl.find_opt mgraph.nodes (key ^ "\texec") with
							| Some n when n.n_fp <> None -> let a,b = fp_lanes n.n_fp in MacroNode (msign,n,a,b)
							| _ ->
								let base = match String.rindex_opt key ' ' with Some j -> String.sub key 0 j | None -> key in
								let tpath = if String.ends_with ~suffix:"(constructor)" key then base else match String.rindex_opt base '.' with Some j -> String.sub base 0 j | None -> base in
								macro_module ~why:key (path_of_key tpath)
						) :: !edges
					| "exec-members" ->
						edges := (match Hashtbl.find_opt mgraph.nodes (key ^ "\tmembers") with
							| Some n when n.n_fp <> None -> let a,b = fp_lanes n.n_fp in MacroNode (msign,n,a,b)
							| _ -> macro_module ~why:"members" (path_of_key key)
						) :: !edges
					| "exec-init" ->
						edges := macro_module ~why:"init" (path_of_key key) :: !edges
					| "signature" when key = "(defines)" -> DepRecord.DepTbl.add dep_nodes d None
					| _ ->
						let n = node g k in
						DepRecord.DepTbl.add dep_nodes d (Some n);
						if k <> self then begin let a,b = fp_lanes n.n_fp in edges := Node (n,a,b) :: !edges end
					end
			) c;
			(node g self).n_edges <- Some (LargeArray.of_list !edges)
		) cl
	) modules;
	in
	pending := build_edges :: !pending;
	g.assumptions <- [];
	if trace then
		trace_line (Printf.sprintf "[graph] stored %d modules, %d nodes" (List.length modules) (Hashtbl.length g.nodes))
	in
	failed_assumptions,apply

(* Validation, before typing *)

type validation = {
	v_graph : graph;
	(* the current declarations of a file, None if it cannot be parsed *)
	v_decls : file -> (string,TFingerprint.t) Hashtbl.t option;
	v_resolve : path -> string;
	(* whether a module checks as valid, in this context and in the macro context *)
	v_module_valid : path -> bool;
	v_macro_module_valid : string -> path -> bool;
	(* the validation of the graph of the macro context of a signature *)
	v_macro : string -> validation option;
	(* whether a node without computation is valid when the declarations of
	   its type did not change (the macro context, which is not typed again to
	   check it) *)
	v_untracked : bool;
	(* whether a module still holds what its nodes were stored from (not
	   invalidated by the server since) *)
	v_owner_valid : path -> bool;
	(* the computations found valid while the module being checked is not
	   known to be valid (cycles): forgotten if it turns out invalid *)
	mutable v_provisional : node list;
	(* the first computation found invalid *)
	mutable v_failed : string option;
	(* whether nodes whose module is typed again can be assumed to keep their fingerprint *)
	v_assume : bool;
}



(* HAXE_GRAPH=off: modules are checked through their module dependencies only *)
let disabled = Sys.getenv_opt "HAXE_GRAPH" = Some "off"

let current_decls v f =
	if f.f_step <> v.v_graph.step then begin
		f.f_step <- v.v_graph.step;
		f.f_current <- v.v_decls f;
		f.f_unchanged <- (match f.f_current,f.f_decls with Some h,Some h' -> h == h' | _ -> false)
	end;
	f.f_current

let current_lookup v l =
	if l.l_step <> v.v_graph.step then begin
		l.l_step <- v.v_graph.step;
		if l.l_epoch <> v.v_graph.dirs_epoch then begin
			l.l_epoch <- v.v_graph.dirs_epoch;
			l.l_current <- v.v_resolve l.l_path
		end
	end;
	l.l_current

let s_fp = function Some fp -> TFingerprint.to_string fp | None -> "none"

let s_edge v = function
	| Leaf (f,key,a,b) ->
		let cur = match current_decls v f with Some h -> Hashtbl.find_opt h key | None -> None in
		Printf.sprintf "%s:%s (%s, now %s)" f.f_path key (s_fp (if a = no_fp then None else Some (a,b))) (s_fp cur)
	| Lookup (l,seen) ->
		Printf.sprintf "lookup %s (%s, now %s)" (s_type_path l.l_path) seen (current_lookup v l)
	| Node (n,a,b) ->
		Printf.sprintf "node %s (%s, now %s)" n.n_key (s_fp (if a = no_fp then None else Some (a,b))) (s_fp n.n_fp)
	| Module path ->
		"module " ^ s_type_path path
	| MacroModule (_,path) ->
		"macro module " ^ s_type_path path
	| MacroNode (_,n,a,b) ->
		Printf.sprintf "macro node %s (%s, now %s)" n.n_key (s_fp (if a = no_fp then None else Some (a,b))) (s_fp n.n_fp)
	| Absent (n,name,static) ->
		absent_key n name static

(* What changed for an expansion: "executed pack.Type.field (static)" for the
   code of a function, "macro pack.Type	members" for another node. *)
let macro_reason n =
	match String.index_opt n.n_key '\t' with
	| Some i when String.sub n.n_key i (String.length n.n_key - i) = "\texec" -> "executed " ^ String.sub n.n_key 0 i
	| _ -> "macro " ^ n.n_key

(* The validation of the graph of the macro context of the current
   compilation, once made (see [check_macro_node]). *)
let macro_validation : (string * validation) option ref = ref None
let () = clear_macro_validation := (fun () -> macro_validation := None)

(* The computations found valid while one being checked was assumed valid:
   checked again (it was not). *)
let forget_provisional_nodes v =
	List.iter (fun n -> n.n_step <- -1) v.v_provisional;
	v.v_provisional <- []

let rec valid_node v n =
	if not n.n_by_known then begin
		n.n_by <- producer_of v.v_graph n.n_key;
		n.n_by_known <- true
	end;
	match n.n_by with
	| Some p when is_computation p ->
		valid_computation v p
	| _ ->
		if n.n_fp = None then true
		else if v.v_untracked && untracked_unchanged v n then true
		else begin
			let b = v.v_module_valid n.n_owner in
			if trace then trace_line (Printf.sprintf "[graph] %s: owner %s %s" n.n_key (s_type_path n.n_owner) (if b then "valid" else "changed"));
			b
		end

and valid_computation v n =
	if n.n_step = v.v_graph.step then begin
		if not n.n_valid && v.v_failed = None then v.v_failed <- Some n.n_key;
		n.n_valid
	end else begin
		let edges = match n.n_edges with Some l -> l | None -> [||] in
		if trace then trace_line (Printf.sprintf "[graph] check %s (%d edges): %s" n.n_key (Array.length edges) (String.concat ", " (Array.to_list (Array.map (s_edge v) edges))));
		(* assumed valid while it is checked *)
		n.n_step <- v.v_graph.step;
		n.n_valid <- true;
		(* what does not lead to other computations of the graph first: a
		   computation found changed by them is not assumed valid while the ones
		   it reads are checked (a cycle back to it would find it valid) *)
		let local = function Leaf _ | Lookup _ | MacroNode _ | MacroModule _ -> true | Node _ | Module _ | Absent _ -> false in
		let b = Array.for_all (fun e -> not (local e) || valid_edge v e) edges && Array.for_all (fun e -> local e || valid_edge v e) edges in
		n.n_valid <- b;
		if b then v.v_provisional <- n :: v.v_provisional;
		b
	end

and valid_edge v e =
	let b = match e with
		| Leaf (f,key,a,b) ->
			(* the leaves of a file are the ones of the computations typed from it:
			   built again when it changes *)
			let current = current_decls v f in
			f.f_unchanged || same_fp (match current with Some h -> Hashtbl.find_opt h key | None -> None) a b
		| Lookup (l,seen) ->
			current_lookup v l = seen
		| Node (n,a,b) ->
			(* [known_changed] is empty but when a compilation is done again *)
			begin match (if Hashtbl.length known_changed = 0 then None else Hashtbl.find_opt known_changed n.n_key) with
			| Some now -> same_fp now a b && valid_node v n
			| None ->
				same_fp n.n_fp a b &&
				let failed = v.v_failed in
				valid_node v n || (assume v n n.n_fp && (v.v_failed <- failed; true))
			end
		| Module path ->
			v.v_module_valid path
		| MacroModule (sign,path) ->
			v.v_macro_module_valid sign path
		| MacroNode (sign,n,a,b) ->
			begin match v.v_macro sign with
			| Some vm ->
				let failed = vm.v_failed in
				vm.v_failed <- None;
				let r = vm.v_owner_valid n.n_owner && valid_edge vm (Node (n,a,b)) in
				(* what was found valid assuming a computation being checked was *)
				if r then vm.v_provisional <- [] else forget_provisional_nodes vm;
				if trace && not r then trace_line (Printf.sprintf "[graph] macro node %s invalid: %s" n.n_key (Option.default "?" vm.v_failed));
				if not r && v.v_failed = None then v.v_failed <- Some (macro_reason n);
				vm.v_failed <- failed;
				r
			| None ->
				false
			end
		| Absent (n,name,static) ->
			(Hashtbl.length known_changed = 0 || not (Hashtbl.mem known_changed (absent_key n name static))) &&
			let failed = v.v_failed in
			valid_node v n || (assume_absent v n name static && (v.v_failed <- failed; true))
			|| (v.v_untracked && absent_in_decls v n name && (v.v_failed <- failed; true))
	in
	if not b then begin
		if trace then trace_line ("[graph] changed: " ^ s_edge v e);
		if v.v_failed = None then v.v_failed <- Some (match e with
			| Leaf (f,key,_,_) -> Filename.basename f.f_path ^ ":" ^ key
			| Lookup (l,_) -> "lookup " ^ s_type_path l.l_path
			| Node (n,_,_) -> n.n_key
			| Module path -> "module " ^ s_type_path path
			| MacroModule (_,path) -> "macro module " ^ s_type_path path
			| MacroNode (_,n,_,_) -> macro_reason n
			| Absent (n,name,static) -> absent_key n name static)
	end;
	b

(* [valid ()], or an assumption ([assumed ()]) makes the edge valid: then
   what was found invalid while checking it is not the reason why the module
   is typed again. *)
and rescued v valid assumed =
	let failed = v.v_failed in
	valid () || (assumed () && (v.v_failed <- failed; true))

(* Early cutoff for an absence: the class keeps having no field [name], checked
   after typing. *)
and assume_absent v n name static =
	v.v_assume && (match n.n_by with Some p -> is_computation p | None -> false) && begin
		if trace then trace_line ("[graph] assumed absent: " ^ absent_key n name static);
		v.v_graph.assumptions <- AAbsent (n,name,static) :: v.v_graph.assumptions;
		true
	end

and is_members n =
	let l = String.length n.n_key in
	l > 8 && String.sub n.n_key (l - 8) 8 = "\tmembers"

(* The names of the fields of a type, which computation [p] reads (its
   members), changed: what it produces changes too, it is not assumed to keep
   its fingerprint (which would only cost a compilation done again). A declared
   signature does not tell as much: it holds the body of a function whose type
   is inferred. *)
and declaration_changed v p =
	match p.n_edges with
	| None -> false
	| Some edges ->
		Array.exists (function
			| Leaf (f,key,a,b) ->
				let len = String.length key in
				let ends s = let l = String.length s in len >= l && String.sub key (len - l) l = s in
				ends "\tfields" && not (same_fp (match current_decls v f with Some h -> Hashtbl.find_opt h key | None -> None) a b)
			| _ -> false
		) edges

(* A declaration that computation [p] read is gone (a field removed): what it
   produces changes, it is not assumed to keep its fingerprint. *)
and declaration_removed v p =
	match p.n_edges with
	| None -> false
	| Some edges ->
		Array.exists (function
			| Leaf (f,key,a,_) when a <> no_fp && (String.ends_with ~suffix:"\tdecls" key || String.ends_with ~suffix:"\tdecl signature" key) ->
				(match current_decls v f with Some h -> not (Hashtbl.mem h key) | None -> false)
			| _ -> false
		) edges

(* A node that no computation produces (the header and the members of a type,
   made from its declarations when its module is loaded): the declarations of
   its type did not change, its positions in the file aside (lines added
   before it). *)
and untracked_unchanged v n =
	(* [n_step] and [n_valid] memoize it: they are the ones of a computation otherwise *)
	if n.n_step <> v.v_graph.step then begin
		n.n_step <- v.v_graph.step;
		n.n_valid <- untracked_decls_unchanged v n
	end;
	n.n_valid

and untracked_decls_unchanged v n =
	match Hashtbl.find_opt v.v_graph.module_files n.n_owner, String.index_opt n.n_key '\t' with
	| Some f, Some i ->
		let path = String.sub n.n_key 0 i in
		let tname = match String.rindex_opt path '.' with Some j -> String.sub path (j + 1) (String.length path - j - 1) | None -> path in
		begin match f.f_decls, current_decls v f with
		| Some old, Some cur ->
			let lt = String.length tname in
			let relevant key =
				String.length key > lt && String.sub key 0 lt = tname && (key.[lt] = '\t' || key.[lt] = '.')
				&& not (String.ends_with ~suffix:"\tposition" key)
			in
			let included tbl tbl' = Hashtbl.fold (fun key fp ok -> ok && (not (relevant key) || Hashtbl.find_opt tbl' key = Some fp)) tbl true in
			included old cur && included cur old
		| _ ->
			false
		end
	| _ ->
		false

(* The class of [n] (its members) has no declaration of a field [name] in its
   file now (the macro context, where the classes are seldom built by macros:
   an absence is checked on the declarations instead of typing again). *)
and absent_in_decls v n name =
	match Hashtbl.find_opt v.v_graph.module_files n.n_owner, String.index_opt n.n_key '\t' with
	| Some f, Some i ->
		let path = String.sub n.n_key 0 i in
		let tname = match String.rindex_opt path '.' with Some j -> String.sub path (j + 1) (String.length path - j - 1) | None -> path in
		begin match current_decls v f with
		| Some cur -> Hashtbl.mem cur (tname ^ "\tdecls") && not (Hashtbl.mem cur (tname ^ "." ^ name ^ "\tdecls"))
		| None -> false
		end
	| _ ->
		false

(* Early cutoff: node [n] keeps fingerprint [seen] if its computation, which
   is not valid, computes the same value again when its module is typed
   again; or, for a node without computation, if the declarations of its type
   did not change. This is checked after typing ([check_assumptions]). *)
and assume v n seen =
	v.v_assume && (match n.n_by with
		| Some p -> is_computation p && not (is_members n && declaration_changed v p) && not (declaration_removed v p)
		| None -> seen <> None && untracked_unchanged v n) && begin
		if trace then trace_line ("[graph] assumed unchanged: " ^ n.n_key);
		v.v_graph.assumptions <- ANode (n,seen) :: v.v_graph.assumptions;
		true
	end

let () = check_macro_node := (fun sign n a b ->
	match !macro_validation with
	| Some (s,vm) when s = sign ->
		let failed = vm.v_failed in
		let r = vm.v_owner_valid n.n_owner && valid_edge vm (Node (n,a,b)) in
		if r then vm.v_provisional <- [] else forget_provisional_nodes vm;
		vm.v_failed <- failed;
		r
	| _ ->
		false
)

(* The validation of the current compilation, with the signature of its
   context, for [reusable]. *)
let current : (string * validation * (string,bool) Hashtbl.t) option ref = ref None
let reuse_mutex = Mutex.create ()
let () = clear_current := (fun () -> current := None)

(* Whether computation [n] is valid without assumptions, with its own memo
   (the one of the validation holds results made under assumptions, which are
   checked after typing): what it read did not change, the nodes produced by
   valid computations or by modules found valid. *)
let rec strictly_valid v memo n =
	match Hashtbl.find_opt memo n.n_key with
	| Some b -> b
	| None ->
		(* a cycle is not valid *)
		Hashtbl.replace memo n.n_key false;
		let edges = match n.n_edges with Some l -> l | None -> [||] in
		let valid_produced n' =
			if not n'.n_by_known then begin
				n'.n_by <- producer_of v.v_graph n'.n_key;
				n'.n_by_known <- true
			end;
			match n'.n_by with
			| Some p when is_computation p -> strictly_valid v memo p
			| _ -> n'.n_fp = None || v.v_module_valid n'.n_owner
		in
		let b = Hashtbl.length known_changed = 0 && Array.for_all (fun e -> let r = (match e with
			| Leaf (f,key,a,b) ->
				let current = current_decls v f in
				f.f_unchanged || same_fp (match current with Some h -> Hashtbl.find_opt h key | None -> None) a b
			| Lookup (l,seen) -> current_lookup v l = seen
			| Node (n',a,b) -> same_fp n'.n_fp a b && valid_produced n'
			| Absent (n',_,_) -> valid_produced n'
			| Module _ | MacroModule _ | MacroNode _ -> false) in
			if not r && trace then trace_line ("[reuse] " ^ n.n_key ^ ": " ^ s_edge v e);
			r
		) edges in
		Hashtbl.replace memo n.n_key b;
		b

(* Computation [n], which the module being typed again had, is still valid and
   can be kept: the paths of the modules it read, else None. A computation that
   ran a macro or made instances of a @:generic is typed again. *)
let reusable v memo n =
	match n.n_edges with
	| None -> None
	| Some edges ->
		if Array.exists (function Module _ | MacroModule _ | MacroNode _ -> true | _ -> false) edges then None else begin
			if not (strictly_valid v memo n) then None else begin
				let paths = Hashtbl.create 8 in
				Array.iter (function
					| Node (n',_,_) | Absent (n',_,_) -> if n'.n_owner <> n.n_owner then Hashtbl.replace paths n'.n_owner ()
					| Lookup (l,seen) -> if seen <> "" then Hashtbl.replace paths l.l_path ()
					| Leaf _ | Module _ | MacroModule _ | MacroNode _ -> ()
				) edges;
				Some (Hashtbl.fold (fun p () acc -> p :: acc) paths [])
			end
		end

(* Whether module [path] is traced by the graph: its computations are known. *)
let is_traced g path =
	match Hashtbl.find_opt g.module_nodes path with
	| None -> false
	| Some nodes -> List.exists is_computation nodes

(* [dirs_changed]: whether a directory of the class paths changed since the
   previous compilation *)
let begin_validation g step dirs_changed class_paths =
	flush ();
	if g.step <> step then begin
		let class_paths = class_paths () in
		if dirs_changed () || class_paths <> g.class_paths then begin
			g.dirs_epoch <- g.dirs_epoch + 1;
			g.class_paths <- class_paths
		end;
		g.step <- step;
		g.assumptions <- []
	end

(* Whether the computations of module [path] are valid. *)
let module_valid v path =
	let nodes = try Hashtbl.find v.v_graph.module_nodes path with Not_found -> [] in
	let b = List.for_all (fun n -> not (is_computation n) || valid_computation v n) nodes in
	if trace then trace_line (Printf.sprintf "[graph] module %s: %s" (s_type_path path) (if b then "valid" else "changed"));
	b

(* The module being checked turned out invalid: the computations found valid
   on the assumption that it was are checked again. *)
let forget_provisional v =
	List.iter (fun n -> n.n_step <- -1) v.v_provisional;
	v.v_provisional <- []
