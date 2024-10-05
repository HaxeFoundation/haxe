open Globals
open Type

type resolution_kind =
	| RTypeImport of string * module_type
	| RClassFieldImport of string * tclass * tclass_field
	| RAbstractFieldImport of string * tabstract * tclass * tclass_field
	| REnumConstructorImport of string * tenum * tenum_field
	| RWildcardPackage of string list
	| RClassStatics of tclass
	| REnumStatics of tenum
	| RLazy of (unit -> resolution option)

and resolution = {
	r_kind : resolution_kind;
	r_pos : pos;
}

let mk_resolution kind p = {
	r_kind = kind;
	r_pos = p;
}

let lazy_resolution f =
	mk_resolution (RLazy f) null_pos

let module_type_resolution mt alias p =
	mk_resolution (RTypeImport((Option.default (t_name mt) alias),mt)) p

let static_field_resolution c cf alias p =
	mk_resolution (RClassFieldImport((Option.default cf.cf_name alias),c,cf)) p

let static_abstract_field_resolution a c cf alias p =
	mk_resolution (RAbstractFieldImport((Option.default cf.cf_name alias),a,c,cf)) p

let enum_constructor_resolution en ef alias p =
	mk_resolution (REnumConstructorImport((Option.default ef.ef_name alias),en,ef)) p

let class_statics_resolution c p =
	mk_resolution (RClassStatics c) p

let enum_statics_resolution en p =
	mk_resolution (REnumStatics en) p

let wildcard_package_resolution sl p =
	mk_resolution (RWildcardPackage sl) p

let as_importable_static c cf p =
	if not (has_meta Meta.NoImportGlobal cf.cf_meta) then begin match c.cl_kind with
		| KAbstractImpl a ->
			if a.a_enum && not (has_class_field_flag cf CfEnum) then
				None
			else
				Some (cf.cf_name,static_abstract_field_resolution a c cf None p)
		| _ ->
			Some (cf.cf_name,static_field_resolution c cf None p)
	end else
		None

let s_resolution_kind = function
	| RTypeImport(_,mt) -> Printf.sprintf "RTypeImport(%s)" (s_type_path (t_infos mt).mt_path)
	| RClassFieldImport(_,c,cf) -> Printf.sprintf "RClassFieldImport(%s, %s)" (s_type_path c.cl_path) cf.cf_name
	| RAbstractFieldImport(_,a,c,cf) -> Printf.sprintf "RAbstractFieldImport(%s, %s)" (s_type_path a.a_path) cf.cf_name
	| REnumConstructorImport(_,en,ef) -> Printf.sprintf "REnumConstructorImport(%s, %s)" (s_type_path en.e_path) ef.ef_name
	| RWildcardPackage sl -> Printf.sprintf "RWildcardPackage(%s)" (String.concat "." sl)
	| RClassStatics c -> Printf.sprintf "RClassStatics(%s)" (s_type_path c.cl_path)
	| REnumStatics en -> Printf.sprintf "REnumStatics(%s)" (s_type_path en.e_path)
	| RLazy _ -> "RLazy"

class resolution_list (id : string list) = object(self)
	val mutable l = []
	val mutable resolved_lazies = true
	val mutable cached_type_imports = true
	val mutable type_import_cache = StringMap.empty

	method add (res : resolution) =
		l <- res :: l;
		(* If we import a type, we automatically want to import all its constructors in case of
		   enums and enum abstracts. We add a RLazy in front of the list so that it takes priority
		   over the type itself. When resolved, it will insert its fields into the resolution list. *)
		begin match res.r_kind with
		| RTypeImport(_,mt) ->
			Option.may (fun res -> l <- res :: l) (self#expand_enum_constructors mt);
			cached_type_imports <- false;
		| RLazy _ ->
			resolved_lazies <- false;
		| _ ->
			()
		end

	method resolve_lazies =
		let rec loop acc l = match l with
			| {r_kind = RLazy f} :: l ->
				begin match f() with
				| None ->
					loop acc l
				| Some res ->
					loop acc (res :: l)
				end
			| res :: l ->
				loop (res :: acc) l
			| [] ->
				List.rev acc
		in
		if not resolved_lazies then begin
			resolved_lazies <- true;
			l <- loop [] l;
		end

	method resolve' (i : string) : resolution =
		let rec loop l = match l with
			| [] ->
				raise Not_found
			| res :: l ->
				begin match res.r_kind with
				| RClassStatics c ->
					ignore(c.cl_build());
					begin try
						let cf = PMap.find i c.cl_statics in
						begin match as_importable_static c cf res.r_pos with
						| None ->
							loop l
						| Some(_,res) ->
							res
						end;
					with Not_found ->
						loop l
					end
				| REnumStatics en ->
					begin try
						let ef = PMap.find i en.e_constrs in
						if not (has_meta Meta.NoImportGlobal ef.ef_meta) then
							enum_constructor_resolution en ef None res.r_pos
						else
							loop l
					with Not_found ->
						loop l
					end
				| RTypeImport(alias,_) | RClassFieldImport(alias,_,_) | RAbstractFieldImport(alias,_,_,_) | REnumConstructorImport(alias,_,_) ->
					if alias = i then
						res
					else
						loop l
				| RLazy _ | RWildcardPackage _ ->
					loop l
				end
		in
		loop l

	method resolve (i : string) : resolution =
		self#resolve_lazies;
		self#resolve' i

	method expand_enum_constructors (mt : module_type) = match mt with
		| TAbstractDecl ({a_impl = Some c} as a) when a.a_enum ->
			Some (class_statics_resolution c null_pos)
		| TEnumDecl en ->
			Some (enum_statics_resolution en null_pos)
		| TTypeDecl t ->
			let f () =
				begin match follow t.t_type with
					| TEnum (e,_) -> self#expand_enum_constructors (TEnumDecl e)
					| TAbstract (a,_) when a.a_enum -> self#expand_enum_constructors (TAbstractDecl a)
					| _ -> None
				end
			in
			resolved_lazies <- false;
			Some (lazy_resolution f)
		| TClassDecl _ | TAbstractDecl _ ->
			None

	method save =
		let l' = l in
		let resolved_lazies' = resolved_lazies in
		(fun () ->
			l <- l';
			resolved_lazies <- resolved_lazies';
		)

	method get_list =
		l

	method cache_type_imports =
		let rec loop = function
		| [] ->
			()
		| res :: l ->
			(* loop first to retain correct order *)
			loop l;
			match res.r_kind with
			| RTypeImport(alias,mt) ->
				type_import_cache <- StringMap.add alias (mt,res.r_pos) type_import_cache;
			| _ ->
				()
		in
		if not cached_type_imports then begin
			cached_type_imports <- true;
			type_import_cache <- StringMap.empty;
			loop l
		end;

	method find_type_import alias =
		self#cache_type_imports;
		StringMap.find alias type_import_cache

	method extract_type_imports =
		ExtList.List.filter_map (fun res -> match res.r_kind with
			| RTypeImport(_,mt) ->
				Some (mt,res.r_pos)
			| _ ->
				None
		) l

	method extract_field_imports =
		self#resolve_lazies;
		let l = List.fold_left (fun acc res -> match res.r_kind with
			| RClassFieldImport(alias,c,cf) ->
				PMap.add alias ((TClassDecl c),cf.cf_name,res.r_pos) acc
			| RClassStatics c ->
				List.fold_left (fun acc cf ->
					begin match as_importable_static c cf null_pos with
					| Some (alias,res) ->
						PMap.add alias ((TClassDecl c),cf.cf_name,res.r_pos) acc
					| _ ->
						acc
					end
				) acc c.cl_ordered_statics
			| _ ->
				acc
		) PMap.empty l in
		l

	method extract_wildcard_packages =
		ExtList.List.filter_map (fun res -> match res.r_kind with
			| RWildcardPackage sl ->
				Some (sl,res.r_pos)
			| _ ->
				None
		) l
end