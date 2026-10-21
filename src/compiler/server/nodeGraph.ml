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

type node = {
	n_key : string;
	mutable n_owner : path;
	(* the fingerprint of the node, None if its module does not produce it *)
	mutable n_fp : TFingerprint.t option;
	(* what the node read, when it is a computation *)
	mutable n_edges : edge list option;
	(* the computation that produces the node: itself, the body (or inferred
	   signature) of a field for its inline body, the declared signature or the
	   members for a signature *)
	mutable n_by : node option;
	(* validity in the current compilation *)
	mutable n_step : int;
	mutable n_valid : bool;
	(* the compilation (validation step) that last stored the node *)
	mutable n_stored : int;
}

and edge =
	(* a declaration of a file ("name\tnode") and the fingerprint seen, if it existed *)
	| Leaf of file * string * TFingerprint.t option
	(* the lookup of a module path, and the file it found ("" if none) *)
	| Lookup of lookup * string
	(* a node and the fingerprint seen, if it had one *)
	| Node of node * TFingerprint.t option
	(* a module that must not be typed again (it holds instances of @:generic
	   methods made for the computation, or it ran macros) *)
	| Module of path
	(* the module of a macro that ran, in the macro context (its signature) *)
	| MacroModule of string * path

and file = {
	f_path : string;
	(* the module typed from the file, the time of the file and the
	   fingerprints of its declarations ("name\tnode") *)
	mutable f_module : path;
	mutable f_time : float;
	mutable f_decls : (string,TFingerprint.t) Hashtbl.t option;
	(* the current declarations, in the current compilation *)
	mutable f_step : int;
	mutable f_current : (string,TFingerprint.t) Hashtbl.t option;
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
	mutable assumptions : (node * TFingerprint.t option) list;
}

(* A compilation made assumptions that turned out false: it is done again,
   without assumptions. *)
exception Retry

let retrying = ref false

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
		let n = { n_key = key; n_owner = ([],""); n_fp = None; n_edges = None; n_by = None; n_step = -1; n_valid = false; n_stored = -1 } in
		Hashtbl.add g.nodes key n;
		n

let file g path =
	try Hashtbl.find g.files path with Not_found ->
		let f = { f_path = path; f_module = ([],""); f_time = 0.; f_decls = None; f_step = -1; f_current = None } in
		Hashtbl.add g.files path f;
		f

let lookup g path =
	try Hashtbl.find g.lookups path with Not_found ->
		let l = { l_path = path; l_step = -1; l_current = "" } in
		Hashtbl.add g.lookups path l;
		l

let path_of_key key =
	match List.rev (String.split_on_char '.' key) with
	| name :: pack -> List.rev pack,name
	| [] -> [],key

let is_computation n = n.n_edges <> None

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

(* Stores the nodes of the modules of [com] that were typed in this compilation
   (the computations recorded by [DepRecord]). [resolve path] is the file a
   lookup of [path] finds ("" if none), [decls_of m] the declarations [m] was
   typed from, [is_typed m] whether [m] was typed by this compilation. *)
let store com resolve decls_of is_typed =
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
	(* their nodes: the previous ones are emptied, the records stay *)
	List.iter (fun (m,cl) ->
		List.iter (fun n -> n.n_fp <- None; n.n_edges <- None; n.n_by <- None; n.n_stored <- g.step) (try Hashtbl.find g.module_nodes m.m_path with Not_found -> []);
		let nodes = ref [] in
		let add key =
			let n = node g key in
			n.n_owner <- m.m_path;
			n.n_stored <- g.step;
			if not (List.memq n !nodes) then nodes := n :: !nodes;
			n
		in
		EntityKey.iter_module (fun key e ->
			let k = EntityKey.to_string key in
			let set node fp = (add (k ^ "\t" ^ node)).n_fp <- Some fp in
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
		(* the computations are known before their edges are, for [producer_of] *)
		List.iter (fun c -> (add (c.DepRecord.c_key ^ "\t" ^ c.c_node)).n_edges <- Some []) cl;
		Hashtbl.replace g.module_nodes m.m_path !nodes;
		let f = file g (Path.get_full_path (Path.UniqueKey.lazy_path m.m_extra.m_file)) in
		f.f_module <- m.m_path;
		f.f_time <- m.m_extra.m_time;
		f.f_decls <- Option.map (decl_table f.f_path) (decls_of m);
		f.f_step <- -1
	) modules;
	List.iter (fun (m,_) ->
		List.iter (fun n -> n.n_by <- producer_of g n.n_key) (Hashtbl.find g.module_nodes m.m_path)
	) modules;
	(* what their computations read *)
	let lookups = Hashtbl.create 0 in
	let resolve path =
		try Hashtbl.find lookups path with Not_found ->
			let r = resolve path in
			Hashtbl.add lookups path r;
			r
	in
	List.iter (fun (m,cl) ->
		let f = file g (Path.get_full_path (Path.UniqueKey.lazy_path m.m_extra.m_file)) in
		let leaf name node =
			let key = name ^ "\t" ^ node in
			match f.f_decls with
			| Some h -> Leaf (f,key,Hashtbl.find_opt h key)
			| None -> Leaf (f,key,Some (0,0))
		in
		List.iter (fun c ->
			(* a declared signature reads the declaration without its body *)
			let decl_node = if c.DepRecord.c_node = "declared sig" then "decl signature" else "decls" in
			let edges = ref [leaf c.c_decl decl_node;leaf "(imports)" "decls";leaf "(types)" "types"] in
			if c.c_node = "members" then begin
				edges := leaf c.c_decl "fields" :: !edges;
				match f.f_decls with
				| Some h ->
					let prefix = c.c_decl ^ "." in
					let n = String.length prefix in
					Hashtbl.iter (fun k _ ->
						match String.index_opt k '\t' with
						| Some i when i > n && String.sub k 0 n = prefix && String.sub k i (String.length k - i) = "\tdecl signature" ->
							let name = String.sub k 0 i in
							if not (String.contains (String.sub name n (i - n)) '.') then edges := leaf name "decl signature" :: !edges
						| _ ->
							()
					) h
				| None -> ()
			end;
			begin match c.c_field with
			| Some cf when (match cf.cf_expr with Some e -> TFingerprint.uses_line_numbers e | None -> false) ->
				edges := leaf c.c_decl "position" :: !edges
			| _ ->
				()
			end;
			let self = c.c_key ^ "\t" ^ c.c_node in
			(* a macro can read every declaration of the file (build macros read
			   the bodies of the fields), and what the module depends on besides
			   code: the files the macros declared (registerModuleDependency) *)
			if Hashtbl.fold (fun k () acc -> acc || String.ends_with ~suffix:"\texpansion" k) c.c_deps false then begin
				edges := Module m.m_path :: !edges;
				match f.f_decls with
				| Some h -> Hashtbl.iter (fun k _ -> match String.index_opt k '\t' with
					| Some i when String.sub k i (String.length k - i) = "\tdecls" -> edges := leaf (String.sub k 0 i) "decls" :: !edges
					| _ -> ()) h
				| None -> ()
			end;
			Hashtbl.iter (fun k () ->
				match String.index_opt k '\t' with
				| Some i ->
					let key = String.sub k 0 i and node_kind = String.sub k (i + 1) (String.length k - i - 1) in
					begin match node_kind with
					| "lookup" -> let path = path_of_key key in edges := Lookup (lookup g path,resolve path) :: !edges
					| "generic" -> edges := Module (path_of_key key) :: !edges
					| "expansion" ->
						(* "macro pack.Type.field" *)
						let tpath = path_of_key (match String.rindex_opt key '.' with Some j when String.length key > 6 -> String.sub key 6 (j - 6) | _ -> key) in
						let mcom = match com.get_macros () with Some mcom -> mcom | None -> com in
						let mpath = try (mcom.module_lut#find_by_type tpath).m_path with Not_found -> tpath in
						edges := MacroModule (CommonCache.get_cache_sign mcom,mpath) :: !edges
					| "signature" when key = "(defines)" -> ()
					| _ when k = self -> ()
					| _ -> let n = node g k in edges := Node (n,n.n_fp) :: !edges
					end
				| None ->
					()
			) c.c_deps;
			(node g self).n_edges <- Some !edges
		) cl
	) modules

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

let trace = Sys.getenv_opt "HAXE_GRAPH_TRACE" <> None

(* HAXE_GRAPH=off: modules are checked through their module dependencies only *)
let disabled = Sys.getenv_opt "HAXE_GRAPH" = Some "off"

let current_decls v f =
	if f.f_step <> v.v_graph.step then begin
		f.f_step <- v.v_graph.step;
		f.f_current <- v.v_decls f
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
	| Leaf (f,key,seen) ->
		let cur = match current_decls v f with Some h -> Hashtbl.find_opt h key | None -> None in
		Printf.sprintf "%s:%s (%s, now %s)" f.f_path key (s_fp seen) (s_fp cur)
	| Lookup (l,seen) ->
		Printf.sprintf "lookup %s (%s, now %s)" (s_type_path l.l_path) seen (current_lookup v l)
	| Node (n,seen) ->
		Printf.sprintf "node %s (%s, now %s)" n.n_key (s_fp seen) (s_fp n.n_fp)
	| Module path ->
		"module " ^ s_type_path path
	| MacroModule (_,path) ->
		"macro module " ^ s_type_path path

let rec valid_node v n =
	match n.n_by with
	| Some p when is_computation p ->
		valid_computation v p
	| _ ->
		if n.n_fp = None then true
		else begin
			let b = v.v_module_valid n.n_owner in
			if trace then prerr_endline (Printf.sprintf "[graph] %s: owner %s %s" n.n_key (s_type_path n.n_owner) (if b then "valid" else "changed"));
			b
		end

and valid_computation v n =
	if n.n_step = v.v_graph.step then begin
		if not n.n_valid && v.v_failed = None then v.v_failed <- Some n.n_key;
		n.n_valid
	end else begin
		let edges = match n.n_edges with Some l -> l | None -> [] in
		if trace then prerr_endline (Printf.sprintf "[graph] check %s (%d edges): %s" n.n_key (List.length edges) (String.concat ", " (List.map (s_edge v) edges)));
		(* assumed valid while it is checked *)
		n.n_step <- v.v_graph.step;
		n.n_valid <- true;
		let b = List.for_all (valid_edge v) edges in
		n.n_valid <- b;
		if b then v.v_provisional <- n :: v.v_provisional;
		b
	end

and valid_edge v e =
	let b = match e with
		| Leaf (f,key,seen) ->
			(match current_decls v f with Some h -> Hashtbl.find_opt h key | None -> None) = seen
		| Lookup (l,seen) ->
			current_lookup v l = seen
		| Node (n,seen) ->
			n.n_fp = seen && (valid_node v n || assume v n seen)
		| Module path ->
			v.v_module_valid path
		| MacroModule (sign,path) ->
			v.v_macro_module_valid sign path
	in
	if not b then begin
		if trace then prerr_endline ("[graph] changed: " ^ s_edge v e);
		if v.v_failed = None then v.v_failed <- Some (match e with
			| Leaf (f,key,_) -> Filename.basename f.f_path ^ ":" ^ key
			| Lookup (l,_) -> "lookup " ^ s_type_path l.l_path
			| Node (n,_) -> n.n_key
			| Module path -> "module " ^ s_type_path path
			| MacroModule (_,path) -> "macro module " ^ s_type_path path)
	end;
	b

(* Early cutoff: node [n] keeps fingerprint [seen] if its computation, which
   is not valid, computes the same value again when its module is typed
   again. This is checked after typing ([check_assumptions]). *)
and assume v n seen =
	v.v_assume && (match n.n_by with Some p -> is_computation p | None -> false) && begin
		if trace then prerr_endline ("[graph] assumed unchanged: " ^ n.n_key);
		v.v_graph.assumptions <- (n,seen) :: v.v_graph.assumptions;
		true
	end

(* Whether the assumptions of this compilation hold: the nodes were stored
   again with the fingerprints assumed. *)
let check_assumptions g =
	let ok = List.for_all (fun (n,seen) ->
		let b = n.n_stored = g.step && n.n_fp = seen in
		if not b && trace then prerr_endline ("[graph] assumption failed: " ^ n.n_key);
		b
	) g.assumptions in
	g.assumptions <- [];
	ok

(* Whether module [path] is traced by the graph: its computations are known. *)
let is_traced g path =
	match Hashtbl.find_opt g.module_nodes path with
	| None -> false
	| Some nodes -> List.exists is_computation nodes

let begin_validation g step =
	if g.step <> step then begin
		g.step <- step;
		g.assumptions <- []
	end

(* Whether the computations of module [path] are valid. *)
let module_valid v path =
	let nodes = try Hashtbl.find v.v_graph.module_nodes path with Not_found -> [] in
	let b = List.for_all (fun n -> not (is_computation n) || valid_computation v n) nodes in
	if trace then prerr_endline (Printf.sprintf "[graph] module %s: %s" (s_type_path path) (if b then "valid" else "changed"));
	b

(* The module being checked turned out invalid: the computations found valid
   on the assumption that it was are checked again. *)
let forget_provisional v =
	List.iter (fun n -> n.n_step <- -1) v.v_provisional;
	v.v_provisional <- []
