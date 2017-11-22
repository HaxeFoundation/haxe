open Common
open Type
open Typecore
open Common.IdentifierType

let explore_class_paths ctx class_paths recusive f_pack f_module f_type =
	let rec loop dir pack =
		try
			let entries = Sys.readdir dir in
			Array.iter (fun file ->
				match file with
					| "." | ".." ->
						()
					| _ when Sys.is_directory (dir ^ file) && file.[0] >= 'a' && file.[0] <= 'z' ->
						f_pack file;
						if recusive then loop (dir ^ file ^ "/") (file :: pack)
					| _ ->
						let l = String.length file in
						if l > 3 && String.sub file (l - 3) 3 = ".hx" then begin
							try
								let name = String.sub file 0 (l - 3) in
								let path = (List.rev pack,name) in
								let md = ctx.g.do_load_module ctx path Globals.null_pos in
								f_module md;
								List.iter (fun mt -> f_type mt) md.m_types
							with _ ->
								()
						end
			) entries;
		with Sys_error _ ->
			()
	in
	List.iter (fun dir -> loop dir []) class_paths

let collect ctx only_types =
	let acc = DynArray.create () in
	let add x = DynArray.add acc x in

	if not only_types then begin
		(* locals *)
		PMap.iter (fun _ v ->
			if not (is_gen_local v) then
				add (ITLocal v)
		) ctx.locals;

		(* member vars *)
		if ctx.curfun <> FunStatic then begin
			let rec loop c =
				List.iter (fun cf ->
					if not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITMember(ctx.curclass,cf))
				) c.cl_ordered_fields;
				match c.cl_super with
					| None ->
						()
					| Some (csup,tl) ->
						loop csup; (* TODO: type parameters *)
			in
			loop ctx.curclass;
			(* TODO: local using? *)
		end;

		(* statics *)
		List.iter (fun cf ->
			if not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITStatic(ctx.curclass,cf))
		) ctx.curclass.cl_ordered_statics;

		(* enum constructors *)
		let rec enum_ctors t =
			match t with
			| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta ->
				List.iter (fun cf ->
					if (Meta.has Meta.Enum cf.cf_meta) && not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITEnumAbstract(a,cf));
				) c.cl_ordered_statics
			| TClassDecl _ | TAbstractDecl _ ->
				()
			| TTypeDecl t ->
				begin match follow t.t_type with
					| TEnum (e,_) -> enum_ctors (TEnumDecl e)
					| _ -> ()
				end
			| TEnumDecl e ->
				PMap.iter (fun _ ef ->
					add (ITEnum(e,ef))
				) e.e_constrs;
		in
		List.iter enum_ctors ctx.m.curmod.m_types;
		List.iter enum_ctors (List.map fst ctx.m.module_types);

		(* imported globals *)
		PMap.iter (fun _ (mt,s,_) ->
			try
				let t = match resolve_typedef mt with
					| TClassDecl c -> (PMap.find s c.cl_statics).cf_type
					| TEnumDecl en -> (PMap.find s en.e_constrs).ef_type
					| TAbstractDecl {a_impl = Some c} -> (PMap.find s c.cl_statics).cf_type
					| _ -> raise Not_found
				in
				add (ITGlobal(mt,s,t))
			with Not_found ->
				()
		) ctx.m.module_globals;

		(* literals *)
		add (ITLiteral "null");
		add (ITLiteral "true");
		add (ITLiteral "false");
	end;

	let module_types = ref [] in

	let add_type mt =
		match mt with
		| TClassDecl {cl_kind = KAbstractImpl _} -> ()
		| _ ->
			let path = (t_infos mt).mt_path in
			if not (List.exists (fun mt2 -> (t_infos mt2).mt_path = path) !module_types) then begin
				(match mt with
				| TClassDecl c | TAbstractDecl { a_impl = Some c } when Meta.has Meta.CoreApi c.cl_meta ->
					!merge_core_doc_ref ctx c
				| _ -> ());
				module_types := mt :: !module_types
			end
	in

	(* module types *)
	List.iter add_type ctx.m.curmod.m_types;

	(* module imports *)
	List.iter add_type (List.map fst ctx.m.module_types);

	(* module using *)
	List.iter (fun (c,_) ->
		add_type (TClassDecl c)
	) ctx.m.module_using;

	(* TODO: wildcard packages. How? *)

	(* packages and toplevel types *)
	let class_paths = ctx.com.class_path in
	let class_paths = List.filter (fun s -> s <> "") class_paths in

	let packages = ref [] in
	let add_package pack =
		try
			begin match PMap.find pack ctx.com.package_rules with
				| Forbidden ->
					()
				| _ ->
					raise Not_found
			end
		with Not_found ->
			if not (List.mem pack !packages) then packages := pack :: !packages
	in

	let maybe_add_type mt = if not (t_infos mt).mt_private then add_type mt in

	explore_class_paths ctx class_paths false add_package (fun _ -> ()) maybe_add_type;

	List.iter (fun pack ->
		add (ITPackage pack)
	) !packages;

	List.iter (fun mt ->
		add (ITType mt)
	) !module_types;

	(* type params *)
	List.iter (fun (_,t) ->
		add (ITType (module_type_of_type t))
	) ctx.type_params;

	DynArray.to_list acc

let handle_unresolved_identifier ctx i p only_types =
	let l = collect ctx only_types in
	let cl = List.map (fun it ->
		let s = IdentifierType.get_name it in
		(s,it),StringError.levenshtein i s
	) l in
	let cl = List.sort (fun (_,c1) (_,c2) -> compare c1 c2) cl in
	let cl = StringError.filter_similar (fun (s,_) r -> r > 0 && r <= (min (String.length s) (String.length i)) / 3) cl in
	ctx.com.display_information.unresolved_identifiers <- (i,p,cl) :: ctx.com.display_information.unresolved_identifiers
