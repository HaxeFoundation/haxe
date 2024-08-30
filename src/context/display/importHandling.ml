open Globals
open Ast
open DisplayPosition
open Common
open Type
open Error
open Typecore
open Resolution

type import_display_kind =
	| IDKPackage of string list
	| IDKModule of string list * string
	| IDKSubType of string list * string * string
	| IDKModuleField of string list * string * string
	| IDKSubTypeField of string list * string * string * string
	| IDK

type import_display = import_display_kind * pos

let convert_import_to_something_usable pt path =
	let rec loop pack m t = function
		| (s,p) :: l ->
			let is_lower = is_lower_ident s in
			let is_display_pos = encloses_position pt p in
			begin match is_lower,m,t with
				| _,None,Some _ ->
					die "" __LOC__ (* impossible, I think *)
				| true,Some m,None ->
					if is_display_pos then (IDKModuleField(List.rev pack,m,s),p)
					else (IDK,p) (* assume that we're done *)
				| _,Some m,Some t ->
					if is_display_pos then (IDKSubTypeField(List.rev pack,m,t,s),p)
					else (IDK,p)
				| true,None,None ->
					if is_display_pos then (IDKPackage (List.rev (s :: pack)),p)
					else loop (s :: pack) m t l
				| false,Some sm,None ->
					if is_display_pos then (IDKSubType (List.rev pack,sm,s),p)
					else loop pack m (Some s) l
				| false,None,None ->
					if is_display_pos then (IDKModule (List.rev pack,s),p)
					else loop pack (Some s) None l
			end
		| [] ->
			(IDK,null_pos)
	in
	loop [] None None path

let add_import_position ctx p path =
	let infos = ctx.m.curmod.m_extra.m_display in
	if not (PMap.mem p infos.m_import_positions) then
		infos.m_import_positions <- PMap.add p (ref false) infos.m_import_positions

let mark_import_position ctx p =
	try
		let r = PMap.find p ctx.m.curmod.m_extra.m_display.m_import_positions in
		r := true
	with Not_found ->
		()

let commit_import ctx path mode p =
	ctx.m.import_statements <- (path,mode) :: ctx.m.import_statements;
	if Filename.basename p.pfile <> "import.hx" then add_import_position ctx p path

let init_import ctx path mode p =
	let rec loop acc = function
		| x :: l when is_lower_ident (fst x) -> loop (x::acc) l
		| rest -> List.rev acc, rest
	in
	let pack, rest = loop [] path in
	(match rest with
	| [] ->
		(match mode with
		| IAll ->
			ctx.m.import_resolution#add (wildcard_package_resolution (List.map fst pack) p)
		| _ ->
			(match List.rev path with
			(* p spans `import |` (to the display position), so we take the pmax here *)
			| [] -> DisplayException.raise_fields (DisplayToplevel.collect ctx TKType NoValue true) CRImport (DisplayTypes.make_subject None {p with pmin = p.pmax})
			| (_,p) :: _ -> Error.raise_typing_error "Module name must start with an uppercase letter" p))
	| (tname,p2) :: rest ->
		let p1 = (match pack with [] -> p2 | (_,p1) :: _ -> p1) in
		let p_type = punion p1 p2 in
		let md = ctx.g.do_load_module ~origin:MDepFromImport ctx (List.map fst pack,tname) p_type in
		let types = md.m_types in
		let not_private mt = not (t_infos mt).mt_private in
		let error_private p = raise_typing_error "Importing private declarations from a module is not allowed" p in
		let chk_private t p = if ctx.m.curmod != (t_infos t).mt_module && (t_infos t).mt_private then error_private p in
		let has_name name t = snd (t_infos t).mt_path = name in

		let fail_usefully name p =
			let target_kind,candidates = match String.get name 0 with
				(* TODO: cleaner way to get module fields? *)
				| 'a'..'z' -> "field", PMap.foldi (fun n _ acc -> n :: acc) (try (Option.get md.m_statics).cl_statics with | _ -> PMap.empty) []
				| _ -> "type", List.map (fun mt -> snd (t_infos mt).mt_path) types
			in
			raise_typing_error (StringError.string_error name
				candidates
				("Module " ^ s_type_path md.m_path ^ " does not define " ^ target_kind ^ " " ^ name)
			) p
		in

		let find_type tname = List.find (has_name tname) types in
		let get_type tname =
			let t = try
				find_type tname
			with Not_found ->
				fail_usefully tname p_type
			in
			chk_private t p_type;
			t
		in
		let check_alias mt name pname =
			if not (name.[0] >= 'A' && name.[0] <= 'Z') then
				raise_typing_error "Type aliases must start with an uppercase letter" pname;
			if ctx.m.is_display_file && DisplayPosition.display_position#enclosed_in pname then
				DisplayEmitter.display_alias ctx name (type_of_module_type mt) pname;
		in
		let add_static_init t name s =
			match resolve_typedef t with
			| TClassDecl c ->
				ignore(c.cl_build());
				let cf = PMap.find s c.cl_statics in
				static_field_resolution c cf name p
			| TAbstractDecl ({a_impl = Some c} as a) ->
				ignore(c.cl_build());
				let cf = PMap.find s c.cl_statics in
				static_abstract_field_resolution a c cf name p
			| TEnumDecl en ->
				let ef = PMap.find s en.e_constrs in
				enum_constructor_resolution en ef name p
			| _ ->
				raise Not_found
		in
		let add_lazy_resolution f =
			ctx.m.import_resolution#add (lazy_resolution f)
		in
		(match mode with
		| INormal | IAsName _ ->
			let name = (match mode with IAsName n -> Some n | _ -> None) in
			(match rest with
			| [] ->
				begin match name with
				| None ->
					List.iter (fun mt ->
						if not_private mt then
							ctx.m.import_resolution#add (module_type_resolution mt None p)
					) (List.rev types);
					Option.may (fun c ->
						ctx.m.import_resolution#add (class_statics_resolution c p)
					) md.m_statics
				| Some(newname,pname) ->
					let mt = get_type tname in
					check_alias mt newname pname;
					ctx.m.import_resolution#add (module_type_resolution mt (Some newname) p)
				end
			| [tsub,p2] ->
				let pu = punion p1 p2 in
				(try
					let tsub = List.find (has_name tsub) types in
					chk_private tsub pu;
					let alias = match name with
						| None ->
							None
						| Some(name,pname) ->
							check_alias tsub name pname;
							Some name
					in
					ctx.m.import_resolution#add (module_type_resolution tsub alias p);
				with Not_found ->
					(* this might be a static property, wait later to check *)
					let find_main_type_static () =
						try
							let tmain = find_type tname in
							begin try
								Some (add_static_init tmain (Option.map fst name) tsub)
							with Not_found ->
								let parent,target_kind,candidates = match resolve_typedef tmain with
									| TClassDecl c ->
										"Class<" ^ (s_type_path c.cl_path) ^ ">",
										"field",
										PMap.foldi (fun name _ acc -> name :: acc) c.cl_statics []
									| TAbstractDecl {a_impl = Some c} ->
										"Abstract<" ^ (s_type_path md.m_path) ^ ">",
										"field",
										PMap.foldi (fun name _ acc -> name :: acc) c.cl_statics []
									| TEnumDecl e ->
										"Enum<" ^ s_type_path md.m_path ^ ">",
										"field",
										PMap.foldi (fun name _ acc -> name :: acc) e.e_constrs []
									| _ ->
										"Module " ^ s_type_path md.m_path,
										"field or subtype",
										(* TODO: cleaner way to get module fields? *)
										PMap.foldi (fun n _ acc -> n :: acc) (try (Option.get md.m_statics).cl_statics with | _ -> PMap.empty) []
								in
								display_error ctx.com (StringError.string_error tsub candidates (parent ^ " has no " ^ target_kind ^ " " ^ tsub)) p;
								None
							end
						with Not_found ->
							fail_usefully tsub p
					in
					add_lazy_resolution (fun() ->
						match md.m_statics with
						| Some c ->
							(try
								ignore(c.cl_build());
								let rec loop fl =
									match fl with
									| [] -> raise Not_found
									| cf :: rest ->
										if cf.cf_name = tsub then
											if not (has_class_field_flag cf CfPublic) then
												error_private p
											else
												Some (static_field_resolution c cf (Option.map fst name) p)
										else
											loop rest
								in
								loop c.cl_ordered_statics
							with Not_found ->
								find_main_type_static ())
						| None ->
							find_main_type_static ()
					)
				)
			| (tsub,p2) :: (fname,p3) :: rest ->
				(match rest with
				| [] -> ()
				| (n,p) :: _ -> raise_typing_error ("Unexpected " ^ n) p);
				let tsub = get_type tsub in
				add_lazy_resolution (fun() ->
					try
						Some (add_static_init tsub (Option.map fst name) fname)
					with Not_found ->
						display_error ctx.com (s_type_path (t_infos tsub).mt_path ^ " has no field " ^ fname) (punion p p3);
						None
				);
			)
		| IAll ->
			let t = (match rest with
				| [] -> get_type tname
				| [tsub,_] -> get_type tsub
				| _ :: (n,p) :: _ -> raise_typing_error ("Unexpected " ^ n) p
			) in
			add_lazy_resolution (fun() ->
				match resolve_typedef t with
				| TClassDecl c
				| TAbstractDecl {a_impl = Some c} ->
					Some (class_statics_resolution c p)
				| TEnumDecl en ->
					Some (enum_statics_resolution en p)
				| _ ->
					raise_typing_error "No statics to import from this type" p
			)
		))

let handle_using ctx path p =
	let t = match List.rev path with
		| (s1,_) :: (s2,_) :: sl ->
			if is_lower_ident s2 then mk_type_path ((List.rev (s2 :: List.map fst sl)),s1)
			else mk_type_path ~sub:s1 (List.rev (List.map fst sl),s2)
		| (s1,_) :: sl ->
			mk_type_path (List.rev (List.map fst sl),s1)
		| [] ->
			DisplayException.raise_fields (DisplayToplevel.collect ctx TKType NoValue true) CRUsing (DisplayTypes.make_subject None {p with pmin = p.pmax});
	in
	let types = (match t.tsub with
		| None ->
			let md = ctx.g.do_load_module ~origin:MDepFromImport ctx (t.tpackage,t.tname) p in
			let types = List.filter (fun t -> not (t_infos t).mt_private) md.m_types in
			Option.map_default (fun c -> (TClassDecl c) :: types) types md.m_statics
		| Some _ ->
			let t = ctx.g.do_load_type_def ctx p t in
			[t]
	) in
	let filter_classes types =
		let rec loop acc types = match types with
			| td :: l ->
				(match resolve_typedef td with
				| TClassDecl c | TAbstractDecl({a_impl = Some c}) ->
					loop ((c,p) :: acc) l
				| td ->
					loop acc l)
			| [] ->
				acc
		in
		loop [] types
	in
	types,filter_classes

let init_using ctx path p =
	let types,filter_classes = handle_using ctx path p in
	(* do the import first *)
	List.iter (fun mt ->
		ctx.m.import_resolution#add (module_type_resolution mt None p)
	) (List.rev types);
	(* delay the using since we need to resolve typedefs *)
	delay_late ctx.g PConnectField (fun () -> ctx.m.module_using <- filter_classes types @ ctx.m.module_using)
