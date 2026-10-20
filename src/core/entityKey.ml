(*
	Stable entity keys (docs/incremental-compilation.md, §3.1 and phase 2,
	item 1).

	An entity is an object other modules can reference by pointer: a module
	type, a class field (including constructors and overloads), an enum
	constructor, a field of a typedef's structure or a type parameter. Its key
	identifies it independently of its memory address, so that the entities of
	two typings of the same module can be matched (grafting, phase 2, item 4).
*)
open Globals
open Type

type kind =
	| KType
	| KStatic
	| KMember
	| KConstructor
	| KEnumField
	| KAnonField (* field of the structure a typedef declares *)
	| KTypeParam

type t = {
	ek_module : path;
	ek_type : path;
	ek_kind : kind;
	(* field name, or "" for the type itself, its constructor and its type parameters *)
	ek_name : string;
	(* overload index, 0 for the main field *)
	ek_overload : int;
	(* type parameter index, -1 if the entity is not a type parameter *)
	ek_param : int;
}

type entity =
	| EType of module_type
	| EField of tclass * tclass_field
	| EEnumField of tenum * tenum_field
	| EAnonField of tdef * tclass_field
	| ETypeParam of typed_type_param

let s_kind = function
	| KType -> "type"
	| KStatic -> "static"
	| KMember -> "member"
	| KConstructor -> "constructor"
	| KEnumField -> "enum field"
	| KAnonField -> "structure field"
	| KTypeParam -> "type parameter"

let to_string k =
	let base = if k.ek_name = "" then s_type_path k.ek_type else s_type_path k.ek_type ^ "." ^ k.ek_name in
	let overload = if k.ek_overload > 0 then Printf.sprintf ", overload %d" k.ek_overload else "" in
	match k.ek_kind with
	| KType -> base
	| KTypeParam -> Printf.sprintf "%s (type parameter %d%s)" base k.ek_param overload
	| kind -> Printf.sprintf "%s (%s%s)" base (s_kind kind) overload

(* Calls [f key entity] for every entity of module [m]. *)
let iter_module f m =
	let mk ty kind name overload param =
		{ ek_module = m.m_path; ek_type = ty; ek_kind = kind; ek_name = name; ek_overload = overload; ek_param = param }
	in
	let params ty name overload l =
		List.iteri (fun i ttp -> f (mk ty KTypeParam name overload i) (ETypeParam ttp)) l
	in
	let field c ty kind cf =
		let name = if kind = KConstructor then "" else cf.cf_name in
		List.iteri (fun i cf ->
			f (mk ty kind name i (-1)) (EField(c,cf));
			params ty name i cf.cf_params
		) (cf :: cf.cf_overloads)
	in
	List.iter (fun mt ->
		let ty = (t_infos mt).mt_path in
		f (mk ty KType "" 0 (-1)) (EType mt);
		match mt with
		| TClassDecl c ->
			params ty "" 0 c.cl_params;
			List.iter (field c ty KStatic) c.cl_ordered_statics;
			List.iter (field c ty KMember) c.cl_ordered_fields;
			Option.may (field c ty KConstructor) c.cl_constructor
		| TEnumDecl en ->
			params ty "" 0 en.e_params;
			PMap.iter (fun _ ef ->
				f (mk ty KEnumField ef.ef_name 0 (-1)) (EEnumField(en,ef));
				params ty ef.ef_name 0 ef.ef_params
			) en.e_constrs
		| TTypeDecl td ->
			params ty "" 0 td.t_params;
			begin match td.t_type with
			| TAnon an ->
				PMap.iter (fun _ cf -> f (mk ty KAnonField cf.cf_name 0 (-1)) (EAnonField(td,cf))) an.a_fields
			| _ ->
				()
			end
		| TAbstractDecl a ->
			params ty "" 0 a.a_params
	) m.m_types

(* The [key -> entity] index of module [m]. Raises [Failure] if two entities
   of the module get the same key. *)
let index_module m =
	let h = Hashtbl.create 0 in
	iter_module (fun key e ->
		if Hashtbl.mem h key then failwith ("Duplicate entity key " ^ to_string key);
		Hashtbl.add h key e
	) m;
	h

(* Statistics of grafting (src/typing/graft.ml) for the current compilation:
   entities that reused the object of the previous version of their module,
   and entities created as new objects. *)
let graft_reused = ref 0
let graft_created = ref 0

let same_entity e1 e2 = match e1,e2 with
	| EType mt1,EType mt2 -> (t_infos mt1) == (t_infos mt2)
	| EField(_,cf1),EField(_,cf2) | EAnonField(_,cf1),EAnonField(_,cf2) -> cf1 == cf2
	| EEnumField(_,ef1),EEnumField(_,ef2) -> ef1 == ef2
	| ETypeParam ttp1,ETypeParam ttp2 -> ttp1 == ttp2
	| _ -> false
