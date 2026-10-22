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
	file, on its own module and on the module of the macro, in the macro
	context (phase 7 traces macros precisely).

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
	(* the module of a macro that ran, in the macro context (its signature) *)
	| MacroModule of string * path
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
}

type graph = {
	nodes : (string,node) Hashtbl.t;
	module_nodes : (path,node list) Hashtbl.t;
	files : (string,file) Hashtbl.t;
	lookups : (path,lookup) Hashtbl.t;
	mutable step : int;
	(* the nodes assumed to keep their fingerprint in the current compilation,
	   although their module is typed again (early cutoff), with that fingerprint *)
	mutable assumptions : assumption list;
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
   assumption, and whether the next one makes none (a compilation that stopped
   on an error, which a false assumption may have caused). *)
let attempts = ref 0
let no_assumptions = ref false

let end_compilation () =
	attempts := 0;
	no_assumptions := false;
	Hashtbl.reset known_changed

let graphs : (string,graph) Hashtbl.t = Hashtbl.create 0

let get sign =
	try Hashtbl.find graphs sign with Not_found ->
		let g = {
			nodes = Hashtbl.create 0;
			module_nodes = Hashtbl.create 0;
			files = Hashtbl.create 0;
			lookups = Hashtbl.create 0;
			step = -1;
			assumptions = [];
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
		let l = { l_path = path; l_step = -1; l_current = "" } in
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
		| "inline" ->
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
					if TFingerprint.has_inline_body c cf then set "inline" (TFingerprint.field_inline_body cf)
				| EntityKey.EAnonField (_,cf) ->
					set "sig" (TFingerprint.field_sig cf)
				| EntityKey.EEnumField _ | EntityKey.ETypeParam _ ->
					()
			) m;
			let file = Path.get_full_path (Path.UniqueKey.lazy_path m.m_extra.m_file) in
			List.rev !fps,file,Option.map (decl_table file) (decls_of m)
		) (Array.of_list modules) ([],"",None)
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
			List.filter_map (function
				| ANode (n,seen) ->
					let now = Hashtbl.find_opt fps n.n_key in
					if Hashtbl.mem typed_paths n.n_owner && now = seen then None
					else begin
						if trace then trace_line ("[graph] assumption failed: " ^ n.n_key);
						Some (n.n_key,now)
					end
				| AAbsent (n,name,static) ->
					match Hashtbl.find_opt classes n.n_key with
					| Some c when (if name = "new" && not static then
							(* a constructor inherited from the parent (added after typing it) is what
							   the computation found: the one of the parent *)
							(match c.cl_constructor with None -> true | Some cf -> has_class_field_flag cf CfInheritedConstructor)
						else not (PMap.mem name (if static then c.cl_statics else c.cl_fields))) -> None
					| _ ->
						if trace then trace_line ("[graph] assumption failed: " ^ absent_key n name static);
						Some (absent_key n name static,None)
			) g.assumptions
		end
	in
	let apply () =
	(* their nodes: the previous ones are emptied, the records stay *)
	List.iteri (fun i (m,cl) ->
		let fps,file_path,decls = computed.(i) in
		List.iter (fun n -> n.n_fp <- None; n.n_edges <- None; n.n_by <- None; n.n_by_known <- false; n.n_stored <- g.step) (try Hashtbl.find g.module_nodes m.m_path with Not_found -> []);
		let nodes = ref [] in
		let seen = Hashtbl.create 16 in
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
			let expansion = ref false in
			DepRecord.iter_raw_deps (function DepRecord.DKey(_,"expansion") -> expansion := true | _ -> ()) c;
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
						(* "macro pack.Type.field" *)
						let tpath = path_of_key (match String.rindex_opt key '.' with Some j when String.length key > 6 -> String.sub key 6 (j - 6) | _ -> key) in
						let mcom = match com.get_macros () with Some mcom -> mcom | None -> com in
						let mpath = try (mcom.module_lut#find_by_type tpath).m_path with Not_found -> tpath in
						edges := MacroModule (CommonCache.get_cache_sign mcom,mpath) :: !edges
					| "signature" when key = "(defines)" -> DepRecord.DepTbl.add dep_nodes d None
					| _ ->
						let n = node g k in
						DepRecord.DepTbl.add dep_nodes d (Some n);
						if k <> self then begin let a,b = fp_lanes n.n_fp in edges := Node (n,a,b) :: !edges end
					end
			) c;
			(node g self).n_edges <- Some (Array.of_list !edges)
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
		l.l_current <- v.v_resolve l.l_path
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
	| Absent (n,name,static) ->
		absent_key n name static

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
		let b = Array.for_all (valid_edge v) edges in
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
			begin match Hashtbl.find_opt known_changed n.n_key with
			| Some now -> same_fp now a b && valid_node v n
			| None -> same_fp n.n_fp a b && rescued v (fun () -> valid_node v n) (fun () -> assume v n n.n_fp)
			end
		| Module path ->
			v.v_module_valid path
		| MacroModule (sign,path) ->
			v.v_macro_module_valid sign path
		| Absent (n,name,static) ->
			not (Hashtbl.mem known_changed (absent_key n name static)) && rescued v (fun () -> valid_node v n) (fun () -> assume_absent v n name static)
	in
	if not b then begin
		if trace then trace_line ("[graph] changed: " ^ s_edge v e);
		if v.v_failed = None then v.v_failed <- Some (match e with
			| Leaf (f,key,_,_) -> Filename.basename f.f_path ^ ":" ^ key
			| Lookup (l,_) -> "lookup " ^ s_type_path l.l_path
			| Node (n,_,_) -> n.n_key
			| Module path -> "module " ^ s_type_path path
			| MacroModule (_,path) -> "macro module " ^ s_type_path path
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

(* Early cutoff: node [n] keeps fingerprint [seen] if its computation, which
   is not valid, computes the same value again when its module is typed
   again. This is checked after typing ([check_assumptions]). *)
and assume v n seen =
	v.v_assume && (match n.n_by with Some p -> is_computation p && not (is_members n && declaration_changed v p) | None -> false) && begin
		if trace then trace_line ("[graph] assumed unchanged: " ^ n.n_key);
		v.v_graph.assumptions <- ANode (n,seen) :: v.v_graph.assumptions;
		true
	end

(* Whether module [path] is traced by the graph: its computations are known. *)
let is_traced g path =
	match Hashtbl.find_opt g.module_nodes path with
	| None -> false
	| Some nodes -> List.exists is_computation nodes

let begin_validation g step =
	flush ();
	if g.step <> step then begin
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
