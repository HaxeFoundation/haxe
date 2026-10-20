(*
	Grafting (docs/incremental-compilation.md, phase 2, items 4 and 5).

	When the compilation server retypes a module whose previous version is in
	the in-memory cache, the entities of the new version (types, fields, enum
	constructors) reuse the objects of the previous version that have the same
	entity key: the content of each new object is copied into the old object
	as soon as the new object is created, before anything can refer to it, and
	the old object is used from then on. Other modules keep valid pointers to
	the module's entities without being retyped, and no reference has to be
	remapped. Entities without a counterpart in the previous version (added
	entities) are new objects; entities of the previous version without a
	counterpart (removed entities) are dropped: references to them are stale,
	which is a shape change that requires retyping the modules using them.

	Enabled with `-D incremental-graft`.
*)
open Globals
open Type
open EntityKey

type module_graft = {
	(* the [key -> entity] index of the previous version of the module *)
	index : (EntityKey.t,entity) Hashtbl.t;
	(* old entities already reused, so that an old object is never given to two entities *)
	reused : (EntityKey.t,unit) Hashtbl.t;
	(* number of fields created so far per (type, kind, name), giving overload indexes *)
	overloads : (path * kind * string,int) Hashtbl.t;
}

(* Grafts of the modules typed by the current compilation, by module id. *)
let grafts : (int,module_graft) Hashtbl.t = Hashtbl.create 0
let mutex = Mutex.create ()

let reset () =
	Mutex.protect mutex (fun () -> Hashtbl.reset grafts);
	EntityKey.graft_reused := 0;
	EntityKey.graft_created := 0

(* Copies the content of [fresh] into [old], which have the same record type. *)
let copy_into (old : 'a) (fresh : 'a) =
	let o = Obj.repr old and n = Obj.repr fresh in
	assert (Obj.tag o = 0 && Obj.tag n = 0 && Obj.size o = Obj.size n);
	for i = 0 to Obj.size o - 1 do
		Obj.set_field o i (Obj.field n i)
	done

(* Registers module [m], about to be typed, for grafting if the previous version
   of its path is in the in-memory cache. *)
let begin_module com m =
	if Common.defined com Define.IncrementalGraft then begin
		let cc = CommonCache.get_cache com in
		match cc#find_module_opt m.m_path with
		| Some m_old when m_old != m ->
			(* Classes of the previous version may hold their post-processed
			   (e.g. DCE-filtered) state: restore the typed one first. *)
			List.iter (fun mt -> (t_infos mt).mt_restore ()) m_old.m_types;
			let index = try Some (EntityKey.index_module m_old) with Failure _ -> None in
			Option.may (fun index ->
				Mutex.protect mutex (fun () ->
					Hashtbl.replace grafts m.m_id { index; reused = Hashtbl.create 0; overloads = Hashtbl.create 0 }
				)
			) index
		| _ ->
			()
	end

let find_old m key =
	Mutex.protect mutex (fun () ->
		match Hashtbl.find_opt grafts m.m_id with
		| None ->
			None
		| Some g ->
			match Hashtbl.find_opt g.index key with
			| Some e when not (Hashtbl.mem g.reused key) ->
				Hashtbl.add g.reused key ();
				Some e
			| _ ->
				None
	)

let mk_key m ty kind name overload =
	{ ek_module = m.m_path; ek_type = ty; ek_kind = kind; ek_name = name; ek_overload = overload; ek_param = -1 }

let reuse found fresh = match found with
	| Some old ->
		copy_into old fresh;
		incr EntityKey.graft_reused;
		old
	| None ->
		incr EntityKey.graft_created;
		fresh

let graft_class m (c : tclass) =
	let c' = reuse (match find_old m (mk_key m c.cl_path KType "" 0) with Some (EType (TClassDecl c')) -> Some c' | _ -> None) c in
	(* the only self-reference [mk_class] creates: the type of the class's statics *)
	if c' != c then c'.cl_type <- TType(TFunctions.class_module_type c',[]);
	c'

let graft_enum m (e : tenum) =
	reuse (match find_old m (mk_key m e.e_path KType "" 0) with Some (EType (TEnumDecl e')) -> Some e' | _ -> None) e

let graft_typedef m (t : tdef) =
	reuse (match find_old m (mk_key m t.t_path KType "" 0) with Some (EType (TTypeDecl t')) -> Some t' | _ -> None) t

let graft_abstract m (a : tabstract) =
	reuse (match find_old m (mk_key m a.a_path KType "" 0) with Some (EType (TAbstractDecl a')) -> Some a' | _ -> None) a

(* [cf], a field of class [c] being created; constructors have kind [KConstructor]. *)
let graft_field (c : tclass) kind (cf : tclass_field) =
	let m = c.cl_module in
	if not (Hashtbl.mem grafts m.m_id) then cf else begin
		let name = if kind = KConstructor then "" else cf.cf_name in
		let overload = Mutex.protect mutex (fun () ->
			match Hashtbl.find_opt grafts m.m_id with
			| None ->
				0
			| Some g ->
				let k = (c.cl_path,kind,name) in
				let n = Option.default 0 (Hashtbl.find_opt g.overloads k) in
				Hashtbl.replace g.overloads k (n + 1);
				n
		) in
		reuse (match find_old m (mk_key m c.cl_path kind name overload) with Some (EField(_,cf')) -> Some cf' | _ -> None) cf
	end

let graft_enum_field (e : tenum) (ef : tenum_field) =
	let m = e.e_module in
	reuse (match find_old m (mk_key m e.e_path KEnumField ef.ef_name 0) with Some (EEnumField(_,ef')) -> Some ef' | _ -> None) ef
