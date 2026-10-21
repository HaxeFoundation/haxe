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
	file and on the module of the macro, in the macro context (phase 7 traces
	macros precisely).
*)
open Globals
open Type

type edge =
	(* a declaration of a file: (file, name, node) and the fingerprint seen, if it existed *)
	| Leaf of string * string * string * TFingerprint.t option
	(* the lookup of a module path, and the file it found ("" if none) *)
	| Lookup of path * string
	(* a node and the fingerprint seen, if it had one *)
	| Node of string * TFingerprint.t option
	(* a module that must not be typed again (it holds instances of @:generic
	   methods made for the computation) *)
	| Module of path
	(* the module of a macro that ran, in the macro context (its signature) *)
	| MacroModule of string * path

type producer = {
	p_module : path;
	(* the computation ran a macro *)
	p_macro : bool;
	p_edges : edge list;
}

type file_info = {
	fi_module : path;
	fi_time : float;
	(* (name ^ "\t" ^ node) -> fingerprint *)
	fi_decls : (string,TFingerprint.t) Hashtbl.t;
}

type graph = {
	(* "key\tnode" -> owner module and fingerprint *)
	fps : (string,path * TFingerprint.t) Hashtbl.t;
	(* "key\tnode" of a computation -> what it read *)
	producers : (string,producer) Hashtbl.t;
	(* the nodes and computations of a module *)
	module_keys : (path,string list) Hashtbl.t;
	(* the declarations of the files of the modules *)
	files : (string,file_info) Hashtbl.t;
	(* validity of the computations in the current compilation *)
	mutable step : int;
	valid : (string,bool) Hashtbl.t;
	(* statistics of the last compilation *)
	mutable checked_modules : int;
	mutable kept_modules : int;
}

let graphs : (string,graph) Hashtbl.t = Hashtbl.create 0

let get sign =
	try Hashtbl.find graphs sign with Not_found ->
		let g = {
			fps = Hashtbl.create 0;
			producers = Hashtbl.create 0;
			module_keys = Hashtbl.create 0;
			files = Hashtbl.create 0;
			step = -1;
			valid = Hashtbl.create 0;
			checked_modules = 0;
			kept_modules = 0;
		} in
		Hashtbl.add graphs sign g;
		g

let find_opt sign = Hashtbl.find_opt graphs sign

let clear () = Hashtbl.reset graphs

let path_of_key key =
	match List.rev (String.split_on_char '.' key) with
	| name :: pack -> List.rev pack,name
	| [] -> [],key

(* The computation that produces node [key]: itself, the body (or inferred
   signature) of a field for its inline body, the members of a type for a
   declared signature. *)
let producer_of g key =
	if Hashtbl.mem g.producers key then Some key
	else match String.index_opt key '\t' with
	| None -> None
	| Some i ->
		let k = String.sub key 0 i and node = String.sub key (i + 1) (String.length key - i - 1) in
		let find l = List.find_opt (Hashtbl.mem g.producers) l in
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
				find [k ^ "\tsig";k ^ "\tdeclared sig";tkey ^ "\tmembers"]
			| _ ->
				None
			end
		| _ ->
			None

(* Recording, after typing *)

let decl_table file decls =
	let h = Hashtbl.create 0 in
	List.iter (fun (name,node,fp) -> Hashtbl.replace h (name ^ "\t" ^ node) fp) (TypeloadParse.find_decl_fingerprints file decls);
	h

(* Stores the nodes of the modules of [com] that were typed in this compilation
   (the computations recorded by [DepRecord]). [resolve path] is the file a
   lookup of [path] finds ("" if none), [decls_of m] the declarations [m] was
   typed from. *)
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
	(* their nodes *)
	List.iter (fun (m,_) ->
		List.iter (fun k -> Hashtbl.remove g.fps k; Hashtbl.remove g.producers k) (try Hashtbl.find g.module_keys m.m_path with Not_found -> []);
		let keys = ref [] in
		let add k node fp =
			let key = k ^ "\t" ^ node in
			keys := key :: !keys;
			Hashtbl.replace g.fps key (m.m_path,fp)
		in
		EntityKey.iter_module (fun key e ->
			let k = EntityKey.to_string key in
			match e with
			| EntityKey.EType mt ->
				add k "header" (TFingerprint.type_header mt);
				add k "members" (TFingerprint.type_members mt)
			| EntityKey.EField (c,cf) ->
				add k "sig" (TFingerprint.field_sig cf);
				if TFingerprint.has_inline_body c cf then add k "inline" (TFingerprint.field_inline_body cf)
			| EntityKey.EAnonField (_,cf) ->
				add k "sig" (TFingerprint.field_sig cf)
			| EntityKey.EEnumField _ | EntityKey.ETypeParam _ ->
				()
		) m;
		Hashtbl.replace g.module_keys m.m_path !keys;
		let file = Path.UniqueKey.lazy_path m.m_extra.m_file in
		match decls_of m with
		| Some decls -> Hashtbl.replace g.files (Path.get_full_path file) { fi_module = m.m_path; fi_time = m.m_extra.m_time; fi_decls = decl_table file decls }
		| None -> Hashtbl.remove g.files (Path.get_full_path file)
	) modules;
	(* what their computations read *)
	let lookups = Hashtbl.create 0 in
	let lookup path =
		try Hashtbl.find lookups path with Not_found ->
			let r = resolve path in
			Hashtbl.add lookups path r;
			r
	in
	List.iter (fun (m,cl) ->
		let file = Path.get_full_path (Path.UniqueKey.lazy_path m.m_extra.m_file) in
		let decls = try Some (Hashtbl.find g.files file).fi_decls with Not_found -> None in
		let leaf name node = match decls with
			| Some h -> Leaf (file,name,node,Hashtbl.find_opt h (name ^ "\t" ^ node))
			| None -> Leaf (file,name,node,Some (0,0))
		in
		let keys = ref (try Hashtbl.find g.module_keys m.m_path with Not_found -> []) in
		List.iter (fun c ->
			let macro = ref false in
			(* a declared signature reads the declaration without its body *)
			let decl_node = if c.DepRecord.c_node = "declared sig" then "decl signature" else "decls" in
			let edges = ref [leaf c.c_decl decl_node;leaf "(imports)" "decls";leaf "(types)" "types"] in
			if c.c_node = "members" then begin
				edges := leaf c.c_decl "fields" :: !edges;
				match decls with
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
			   the bodies of the fields); the macro's own module is checked
			   through the module dependencies on the macro context *)
			if Hashtbl.fold (fun k () acc -> acc || String.ends_with ~suffix:"\texpansion" k) c.c_deps false then begin
				(* and what the module depends on besides code: the files the
				   macros declared (registerModuleDependency), the macros *)
				edges := Module m.m_path :: !edges;
				match decls with
				| Some h -> Hashtbl.iter (fun k _ -> match String.index_opt k '\t' with
					| Some i when String.sub k i (String.length k - i) = "\tdecls" -> edges := leaf (String.sub k 0 i) "decls" :: !edges
					| _ -> ()) h
				| None -> ()
			end;
			Hashtbl.iter (fun k () ->
				match String.index_opt k '\t' with
				| Some i ->
					let key = String.sub k 0 i and node = String.sub k (i + 1) (String.length k - i - 1) in
					begin match node with
					| "lookup" -> let path = path_of_key key in edges := Lookup (path,lookup path) :: !edges
					| "generic" -> edges := Module (path_of_key key) :: !edges
					| "expansion" ->
						macro := true;
						(* "macro pack.Type.field" *)
						let tpath = path_of_key (match String.rindex_opt key '.' with Some j when String.length key > 6 -> String.sub key 6 (j - 6) | _ -> key) in
						let mcom = match com.get_macros () with Some mcom -> mcom | None -> com in
						let mpath = try (mcom.module_lut#find_by_type tpath).m_path with Not_found -> tpath in
						edges := MacroModule (CommonCache.get_cache_sign mcom,mpath) :: !edges
					| "signature" when key = "(defines)" -> ()
					| _ when k = self -> ()
					| _ -> edges := Node (k,Option.map snd (Hashtbl.find_opt g.fps k)) :: !edges
					end
				| None ->
					()
			) c.c_deps;
			keys := self :: !keys;
			Hashtbl.replace g.producers self { p_module = m.m_path; p_macro = !macro; p_edges = !edges }
		) cl;
		Hashtbl.replace g.module_keys m.m_path !keys
	) modules

(* Validation, before typing *)

type validation = {
	v_graph : graph;
	(* the current fingerprint of a declaration, None if the file cannot be parsed *)
	v_decl : string -> string -> TFingerprint.t option;
	v_resolve : path -> string;
	(* whether a module checks as valid, in this context and in the macro context *)
	v_module_valid : path -> bool;
	v_macro_module_valid : string -> path -> bool;
	(* the computations found valid while the module being checked is not
	   known to be valid (cycles): forgotten if it turns out invalid *)
	mutable v_provisional : string list;
	(* the first computation found invalid *)
	mutable v_failed : string option;
}

let trace = Sys.getenv_opt "HAXE_GRAPH_TRACE" <> None

(* HAXE_GRAPH=off: modules are checked through their module dependencies only *)
let disabled = Sys.getenv_opt "HAXE_GRAPH" = Some "off"

let rec valid_node v key =
	match producer_of v.v_graph key with
	| Some pk ->
		valid_producer v pk
	| None ->
		match Hashtbl.find_opt v.v_graph.fps key with
		| Some (owner,_) ->
			let b = v.v_module_valid owner in
			if trace then prerr_endline (Printf.sprintf "[graph] %s: owner %s %s" key (s_type_path owner) (if b then "valid" else "changed"));
			b
		| None -> true

and valid_producer v pk =
	match Hashtbl.find_opt v.v_graph.valid pk with
	| Some b ->
		if not b && v.v_failed = None then v.v_failed <- Some pk;
		b
	| None ->
		let p = Hashtbl.find v.v_graph.producers pk in
		if trace then prerr_endline (Printf.sprintf "[graph] check %s (%d edges%s): %s" pk (List.length p.p_edges) (if p.p_macro then ", macro" else "") (String.concat ", " (List.map (s_edge v) p.p_edges)));
		(* assumed valid while it is checked *)
		Hashtbl.replace v.v_graph.valid pk true;
		let b = List.for_all (valid_edge v) p.p_edges in
		Hashtbl.replace v.v_graph.valid pk b;
		if b then v.v_provisional <- pk :: v.v_provisional
		else if v.v_failed = None then v.v_failed <- Some pk;
		b

and valid_edge v e =
	let b = match e with
		| Leaf (file,name,node,seen) ->
			v.v_decl file (name ^ "\t" ^ node) = seen
		| Lookup (path,seen) ->
			v.v_resolve path = seen
		| Node (key,seen) ->
			Option.map snd (Hashtbl.find_opt v.v_graph.fps key) = seen && valid_node v key
		| Module path ->
			v.v_module_valid path
		| MacroModule (sign,path) ->
			v.v_macro_module_valid sign path
	in
	if not b && trace then prerr_endline ("[graph] changed: " ^ s_edge v e);
	b

and s_edge v = function
	| Leaf (file,name,node,seen) ->
		let cur = match v.v_decl file (name ^ "\t" ^ node) with Some fp -> TFingerprint.to_string fp | None -> "none" in
		Printf.sprintf "%s:%s %s (%s, now %s)" file name node (match seen with Some fp -> TFingerprint.to_string fp | None -> "none") cur
	| Lookup (path,seen) ->
		Printf.sprintf "lookup %s (%s, now %s)" (s_type_path path) seen (v.v_resolve path)
	| Node (key,seen) ->
		let cur = match Hashtbl.find_opt v.v_graph.fps key with Some (_,fp) -> TFingerprint.to_string fp | None -> "none" in
		Printf.sprintf "node %s (%s, now %s)" key (match seen with Some fp -> TFingerprint.to_string fp | None -> "none") cur
	| Module path ->
		"module " ^ s_type_path path
	| MacroModule (_,path) ->
		"macro module " ^ s_type_path path

(* Whether module [path] is traced by the graph: its computations are known
   and none ran a macro. *)
let is_traced g path =
	match Hashtbl.find_opt g.module_keys path with
	| None -> false
	| Some keys -> List.exists (fun k -> Hashtbl.mem g.producers k) keys

let begin_validation g step =
	if g.step <> step then begin
		g.step <- step;
		Hashtbl.reset g.valid;
		g.checked_modules <- 0;
		g.kept_modules <- 0
	end

(* Whether the computations of module [path] are valid. *)
let module_valid v path =
	let g = v.v_graph in
	g.checked_modules <- g.checked_modules + 1;
	let keys = try Hashtbl.find g.module_keys path with Not_found -> [] in
	let b = List.for_all (fun k -> not (Hashtbl.mem g.producers k) || valid_producer v k) keys in
	if trace then prerr_endline (Printf.sprintf "[graph] module %s: %s (%d computations)" (s_type_path path) (if b then "valid" else "changed") (List.length (List.filter (Hashtbl.mem g.producers) keys)));
	if b then g.kept_modules <- g.kept_modules + 1;
	b

(* The module being checked turned out invalid: the computations found valid
   on the assumption that it was are checked again. *)
let forget_provisional v =
	List.iter (fun k -> Hashtbl.remove v.v_graph.valid k) v.v_provisional;
	v.v_provisional <- []
