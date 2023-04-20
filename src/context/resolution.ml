open Globals
open Ast
open Type

type resolution_kind =
	| RTypeImport of module_type
	| RClassFieldImport of tclass * tclass_field
	| RAbstractFieldImport of tabstract * tclass * tclass_field
	| REnumConstructorImport of tenum * tenum_field
	| RWildcardPackage of string list
	| RLazy of (unit -> resolution list)

and resolution = {
	r_alias : placed_name;
	r_kind : resolution_kind;
	r_pos : pos;
}

let mk_resolution alias kind p = {
	r_alias = alias;
	r_kind = kind;
	r_pos = p;
}

let s_resolution_kind = function
	| RTypeImport mt -> Printf.sprintf "RTypeImport(%s)" (s_type_path (t_infos mt).mt_path)
	| RClassFieldImport(c,cf) -> Printf.sprintf "RClassFieldImport(%s, %s)" (s_type_path c.cl_path) cf.cf_name
	| RAbstractFieldImport(a,c,cf) -> Printf.sprintf "RAbstractFieldImport(%s, %s)" (s_type_path a.a_path) cf.cf_name
	| REnumConstructorImport(en,ef) -> Printf.sprintf "REnumConstructorImport(%s, %s)" (s_type_path en.e_path) ef.ef_name
	| RWildcardPackage sl -> Printf.sprintf "RWildcardPackage(%s)" (String.concat "." sl)
	| RLazy f -> "RLazy"

class resolution_list (l : resolution list) = object(self)
	val mutable l = l
	val mutable expanded = false
	val cache = Hashtbl.create 0

	method add (res : resolution) =
		expanded <- false;
		l <- res :: l;
		(* If we import a type, we automatically want to import all its constructors in case of
		   enums and enum abstracts. We add a RLazy in front of the list so that it takes priority
		   over the type itself. When resolved, it will insert its fields into the resolution list. *)
		begin match res.r_kind with
		| RTypeImport mt ->
			let f () = self#expand_enum_constructors mt in
			l <- (mk_resolution ("",null_pos) (RLazy f) null_pos) :: l
		| _ ->
			()
		end

	method add_l (rl : resolution list) =
		List.iter self#add (List.rev rl)

	method resolve (i : string) =
		self#check_expand;
		Hashtbl.find cache i

	method expand_enum_constructors (mt : module_type) = match mt with
		| TAbstractDecl ({a_impl = Some c} as a) when a.a_enum ->
			ignore(c.cl_build());
			List.fold_left (fun acc cf ->
				if not (has_class_field_flag cf CfEnum) then
					acc
				else
					(mk_resolution (cf.cf_name,null_pos) (RAbstractFieldImport(a,c,cf)) null_pos) :: acc
			) [] c.cl_ordered_statics
		| TTypeDecl t ->
			begin match follow t.t_type with
				| TEnum (e,_) -> self#expand_enum_constructors (TEnumDecl e)
				| TAbstract (a,_) when a.a_enum -> self#expand_enum_constructors (TAbstractDecl a)
				| _ -> []
			end
		| TEnumDecl en ->
			List.fold_left (fun acc n ->
				let ef = PMap.find n en.e_constrs in
				(mk_resolution (ef.ef_name,null_pos) (REnumConstructorImport(en,ef)) null_pos) :: acc
			) [] en.e_names
		| TClassDecl _ | TAbstractDecl _ ->
			[]

	method check_expand =
		if not expanded then begin
			expanded <- true;
			Hashtbl.clear cache;
			let rec loop acc l = match l with
				| [] ->
					List.rev acc
				| {r_kind = RLazy f} :: l ->
					loop acc (f() @ l)
				| res :: l ->
					let key = fst res.r_alias in
					if not (Hashtbl.mem cache key) then
						Hashtbl.add cache key res;
					loop (res :: acc) l
			in
			l <- loop [] l
		end

	method save =
		let l' = l in
		(fun () ->
			l <- l';
			expanded <- false;
		)

	method get_list =
		self#check_expand;
		l

	method find_type_import check =
		let rec loop = function
		| [] ->
			raise Not_found
		| res :: l ->
			match res.r_kind with
			| RTypeImport mt ->
				if check (fst res.r_alias) mt then (mt,res.r_pos) else loop l
			| _ ->
				loop l
		in
		loop l

	(* TODO: remove this *)
	method extract_type_imports =
		ExtList.List.filter_map (fun res -> match res.r_kind with
			| RTypeImport mt ->
				Some (mt,res.r_pos)
			| _ ->
				None
		) l

	method extract_field_imports =
		self#check_expand;
		List.fold_left (fun acc res -> match res.r_kind with
			| RClassFieldImport(c,cf) ->
				PMap.add (fst res.r_alias) ((TClassDecl c),cf.cf_name,res.r_pos) acc
			| _ ->
				acc
		) PMap.empty l

	method extract_wildcard_packages =
		ExtList.List.filter_map (fun res -> match res.r_kind with
			| RWildcardPackage sl ->
				Some (sl,res.r_pos)
			| _ ->
				None
		) l
end