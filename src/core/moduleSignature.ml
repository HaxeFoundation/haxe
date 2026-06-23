(*
	Module signatures — the *diff layer* of a module header (Phase 1 prototype, increment 1).

	A [module_signature] reduces a typed module's public, dependent-observable surface to per-entry
	canonical strings, keyed so the field-dependency edges (dep_field) map onto entries 1:1:
	  - a structural string per type declaration (the non-field signature), and
	  - a string per field, keyed "m:"/"s:"/"c:" (member/static/constructor), "e:" (enum constructor).

	This is ONLY the change-detection layer; it is not the header object itself (which is a module_def
	skeleton). Two signatures — including one rebuilt from a previous session — can be diffed entry by
	entry to decide which dependents observe a change.

	NOTE (increment 1): impl-field bodies (inline/macro/@:generic), which are dependent-observable, are
	NOT yet captured here; determinism of the type printer is not yet hardened. Both are follow-ups.
*)

open Globals
open Ast
open Type

type decl_signature = {
	ds_struct : string;                    (* non-field structural signature of the declaration *)
	ds_fields : (string,string) PMap.t;    (* field key -> canonical field signature *)
}

type module_signature = {
	msig_path : path;
	msig_decls : (string,decl_signature) PMap.t;   (* type tail name -> its signature *)
}

(* ---------------------------------------------------------------------- *)
(* Leaf printers                                                          *)

let s_field_kind s_type = function
	| Method MethNormal -> "fun"
	| Method MethInline -> "inline fun"
	| Method MethDynamic -> "dynamic fun"
	| Method MethMacro -> "macro fun"
	| Var v -> Printf.sprintf "var(%s,%s)" (TPrinting.s_access true v.v_read) (TPrinting.s_access false v.v_write)

let observable_field_flags = [
	CfPublic,"public"; CfStatic,"static"; CfExtern,"extern"; CfFinal,"final";
	CfOverride,"override"; CfAbstract,"abstract"; CfOverload,"overload";
	CfImpl,"impl"; CfEnum,"enum"; CfGeneric,"generic";
]

let s_field_flags cf =
	String.concat "," (List.filter_map (fun (fl,name) ->
		if has_class_field_flag cf fl then Some name else None
	) observable_field_flags)

let observable_class_flags = [
	CExtern,"extern"; CFinal,"final"; CInterface,"interface"; CAbstract,"abstract";
]

let s_class_flags c =
	String.concat "," (List.filter_map (fun (fl,name) ->
		if has_class_flag c fl then Some name else None
	) observable_class_flags)

(* Signature-affecting meta is rendered by name (sorted, deterministic). Increment 1 ignores args. *)
let s_meta meta =
	match meta with
	| [] -> ""
	| _ -> String.concat " " (List.sort compare (List.map (fun (m,_,_) -> Meta.to_string m) meta))

let s_type_params s_type params =
	match params with
	| [] -> ""
	| _ -> "<" ^ String.concat "," (List.map (TPrinting.s_type_param s_type) params) ^ ">"

let s_inst s_type (c,tl) = s_type (TInst(c,tl))

(* ---------------------------------------------------------------------- *)
(* Field + declaration signatures                                         *)

let s_field s_type cf =
	Printf.sprintf "%s%s:%s|%s|%s"
		(s_field_kind s_type cf.cf_kind)
		(s_type_params s_type cf.cf_params)
		(s_type cf.cf_type)
		(s_field_flags cf)
		(s_meta cf.cf_meta)

let sep = "\x1f"

let s_class_struct s_type c =
	String.concat sep [
		"class";
		(if c.cl_private then "priv" else "");
		s_class_flags c;
		s_type_params s_type c.cl_params;
		"super=" ^ (match c.cl_super with None -> "" | Some ct -> s_inst s_type ct);
		"impl=" ^ String.concat "," (List.map (s_inst s_type) c.cl_implements);
		"meta=" ^ s_meta c.cl_meta;
	]

let s_enum_struct s_type en =
	String.concat sep [
		"enum";
		(if en.e_private then "priv" else "");
		(if has_enum_flag en EnExtern then "extern" else "");
		s_type_params s_type en.e_params;
		"names=" ^ String.concat "," en.e_names;
		"meta=" ^ s_meta en.e_meta;
	]

let s_typedef_struct s_type td =
	String.concat sep [
		"typedef";
		(if td.t_private then "priv" else "");
		s_type_params s_type td.t_params;
		"type=" ^ s_type td.t_type;
		"meta=" ^ s_meta td.t_meta;
	]

let s_abstract_struct s_type a =
	String.concat sep [
		"abstract";
		(if a.a_private then "priv" else "");
		s_type_params s_type a.a_params;
		"this=" ^ s_type a.a_this;
		"from=" ^ String.concat "," (List.map s_type a.a_from);
		"to=" ^ String.concat "," (List.map s_type a.a_to);
		"impl=" ^ (match a.a_impl with None -> "" | Some c -> s_type_path c.cl_path);
		"meta=" ^ s_meta a.a_meta;
	]

let add_field prefix s_type cf fields =
	PMap.add (prefix ^ cf.cf_name) (s_field s_type cf) fields

let class_decl s_type c =
	let fields = PMap.empty in
	let fields = List.fold_left (fun acc cf -> add_field "m:" s_type cf acc) fields c.cl_ordered_fields in
	let fields = List.fold_left (fun acc cf -> add_field "s:" s_type cf acc) fields c.cl_ordered_statics in
	let fields = match c.cl_constructor with None -> fields | Some cf -> PMap.add "c:" (s_field s_type cf) fields in
	{ ds_struct = s_class_struct s_type c; ds_fields = fields }

let enum_decl s_type en =
	let fields = List.fold_left (fun acc name ->
		let ef = PMap.find name en.e_constrs in
		let sg = Printf.sprintf "%d|%s|%s|%s" ef.ef_index (s_type_params s_type ef.ef_params) (s_type ef.ef_type) (s_meta ef.ef_meta) in
		PMap.add ("e:" ^ name) sg acc
	) PMap.empty en.e_names in
	{ ds_struct = s_enum_struct s_type en; ds_fields = fields }

let decl_of_module_type s_type mt =
	let name = snd (t_path mt) in
	let decl = match mt with
		| TClassDecl c -> class_decl s_type c
		| TEnumDecl en -> enum_decl s_type en
		| TTypeDecl td -> { ds_struct = s_typedef_struct s_type td; ds_fields = PMap.empty }
		| TAbstractDecl a -> { ds_struct = s_abstract_struct s_type a; ds_fields = PMap.empty }
	in
	(name,decl)

let of_module m =
	let s_type = Type.s_type (Type.print_context()) in
	let decls = List.fold_left (fun acc mt ->
		let (name,decl) = decl_of_module_type s_type mt in
		PMap.add name decl acc
	) PMap.empty m.m_types in
	{ msig_path = m.m_path; msig_decls = decls }

(* ---------------------------------------------------------------------- *)
(* Diffing                                                                *)

type sig_change =
	| ScTypeAdded of string
	| ScTypeRemoved of string
	| ScStructural of string
	| ScFieldAdded of string * string
	| ScFieldRemoved of string * string
	| ScFieldChanged of string * string

let s_sig_change = function
	| ScTypeAdded s -> "+type " ^ s
	| ScTypeRemoved s -> "-type " ^ s
	| ScStructural s -> "~struct " ^ s
	| ScFieldAdded(t,f) -> Printf.sprintf "+field %s.%s" t f
	| ScFieldRemoved(t,f) -> Printf.sprintf "-field %s.%s" t f
	| ScFieldChanged(t,f) -> Printf.sprintf "~field %s.%s" t f

let diff_decl name old_d new_d acc =
	let acc = if old_d.ds_struct <> new_d.ds_struct then ScStructural name :: acc else acc in
	let acc = PMap.foldi (fun key new_sig acc ->
		match (try Some (PMap.find key old_d.ds_fields) with Not_found -> None) with
		| None -> ScFieldAdded(name,key) :: acc
		| Some old_sig -> if old_sig <> new_sig then ScFieldChanged(name,key) :: acc else acc
	) new_d.ds_fields acc in
	PMap.foldi (fun key _ acc ->
		if PMap.mem key new_d.ds_fields then acc else ScFieldRemoved(name,key) :: acc
	) old_d.ds_fields acc

let diff old_sig new_sig =
	let acc = PMap.foldi (fun name new_d acc ->
		match (try Some (PMap.find name old_sig.msig_decls) with Not_found -> None) with
		| None -> ScTypeAdded name :: acc
		| Some old_d -> diff_decl name old_d new_d acc
	) new_sig.msig_decls [] in
	PMap.foldi (fun name _ acc ->
		if PMap.mem name new_sig.msig_decls then acc else ScTypeRemoved name :: acc
	) old_sig.msig_decls acc

(* ---------------------------------------------------------------------- *)
(* Rendering (for the dump diagnostic)                                    *)

let render sg =
	let buf = Buffer.create 256 in
	let decls = List.sort (fun (a,_) (b,_) -> compare a b) (PMap.foldi (fun k v acc -> (k,v) :: acc) sg.msig_decls []) in
	List.iter (fun (name,decl) ->
		Buffer.add_string buf (Printf.sprintf "=== %s ===\n" name);
		Buffer.add_string buf (Printf.sprintf "  struct: %s\n" decl.ds_struct);
		let fields = List.sort (fun (a,_) (b,_) -> compare a b) (PMap.foldi (fun k v acc -> (k,v) :: acc) decl.ds_fields []) in
		List.iter (fun (k,v) -> Buffer.add_string buf (Printf.sprintf "  %s = %s\n" k v)) fields
	) decls;
	Buffer.contents buf
