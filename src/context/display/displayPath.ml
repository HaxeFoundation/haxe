open Globals
open Typecore
open Type
open Common
open CompletionItem
open ClassFieldOrigin
open DisplayException
open Display
open DisplayEmitter
open DisplayPosition
open ImportHandling

module TypePathHandler = struct
	let unique l =
		let rec _unique = function
			| [] -> []
			| x1 :: x2 :: l when x1 = x2 -> _unique (x2 :: l)
			| x :: l -> x :: _unique l
		in
		_unique (List.sort compare l)

	let rec read_type_path com p =
		let classes = ref [] in
		let packages = ref [] in
		let p = (match p with
			| x :: l ->
				(try
					match PMap.find x com.package_rules with
					| Directory d -> d :: l
					| Remap s -> s :: l
					| _ -> p
				with
					Not_found -> p)
			| _ -> p
		) in
		List.iter (fun path ->
			let dir = path ^ String.concat "/" p in
			let r = (try Sys.readdir dir with _ -> [||]) in
			Array.iter (fun f ->
				if (try (Unix.stat (dir ^ "/" ^ f)).Unix.st_kind = Unix.S_DIR with _ -> false) then begin
					if f.[0] >= 'a' && f.[0] <= 'z' then begin
						if p = ["."] then
							match read_type_path com [f] with
							| [] , [] -> ()
							| _ ->
								try
									match PMap.find f com.package_rules with
									| Forbidden -> ()
									| Remap f -> packages := f :: !packages
									| Directory _ -> raise Not_found
								with Not_found ->
									packages := f :: !packages
						else
							packages := f :: !packages
					end;
				end else if file_extension f = "hx" && f <> "import.hx" then begin
					let c = Filename.chop_extension f in
					try
						ignore(String.index c '.')
					with Not_found ->
						if String.length c < 2 || String.sub c (String.length c - 2) 2 <> "__" then classes := c :: !classes;
				end;
			) r;
		) com.class_path;
		let process_lib lib =
			List.iter (fun (path,name) ->
				if path = p then classes := name :: !classes else
				let rec loop p1 p2 =
					match p1, p2 with
					| [], _ -> ()
					| x :: _, [] -> packages := x :: !packages
					| a :: p1, b :: p2 -> if a = b then loop p1 p2
				in
				loop path p
			) lib#list_modules;
		in
		List.iter process_lib com.native_libs.swf_libs;
		List.iter process_lib com.native_libs.net_libs;
		List.iter process_lib com.native_libs.java_libs;
		unique !packages, unique !classes

	(** raise field completion listing packages and modules in a given package *)
	let complete_type_path com p =
		let packs, modules = read_type_path com p in
		if packs = [] && modules = [] then
			(abort ("No modules found in " ^ String.concat "." p) null_pos)
		else
			let packs = List.map (fun n -> make_ci_package (p,n) []) packs in
			let modules = List.map (fun n -> make_ci_module (p,n)) modules in
			Some (packs @ modules)

	(** raise field completion listing module sub-types and static fields *)
	let complete_type_path_inner ctx p c cur_package is_import =
		try
			let sl_pack,s_module = match List.rev p with
				| s :: sl when s.[0] >= 'A' && s.[0] <= 'Z' -> List.rev sl,s
				| _ -> p,c
			in
			(* This is a bit wacky: We want to reset the display position so that revisiting the display file
			   does not raise another TypePath exception. However, we still want to have it treated like the
			   display file, so we just set the position to 0 (#6558). *)
			let old = display_position#get in
			display_position#set {old with pmin = 0; pmax = 0};
			let rec lookup p =
				try
					ctx.g.do_load_module ctx (p,s_module) null_pos
				with e ->
					if cur_package then
						match List.rev p with
						| [] -> raise e
						| _ :: p -> lookup (List.rev p)
					else
						raise e
			in
			let m = Std.finally (fun () -> display_position#set old) lookup sl_pack in
			let statics = ref None in
			let enum_statics = ref None in
			let public_types = List.filter (fun t ->
				let tinfos = t_infos t in
				let is_module_type = snd tinfos.mt_path = c in
				if is_import && is_module_type then begin match t with
					| TClassDecl c | TAbstractDecl {a_impl = Some c} ->
						ignore(c.cl_build());
						statics := Some c
					| TEnumDecl en ->
						enum_statics := Some en
					| _ -> ()
				end;
				not tinfos.mt_private
			) m.m_types in
			let types =
				if c <> s_module then
					[]
				else
					List.map (fun mt ->
						make_ci_type (CompletionItem.CompletionModuleType.of_module_type mt) ImportStatus.Imported None
					) public_types
			in
			let class_origin c = match c.cl_kind with
				| KAbstractImpl a -> Self (TAbstractDecl a)
				| _ -> Self (TClassDecl c)
			in
			let tpair t =
				(t,CompletionType.from_type (get_import_status ctx) t)
			in
			let make_field_doc c cf =
				make_ci_class_field (CompletionClassField.make cf CFSStatic (class_origin c) true) (tpair cf.cf_type)
			in
			let fields = match !statics with
				| None -> types
				| Some c -> types @ (List.map (make_field_doc c) (List.filter (fun cf -> has_class_field_flag cf CfPublic) c.cl_ordered_statics))
			in
			let fields = match !enum_statics with
				| None -> fields
				| Some en -> PMap.fold (fun ef acc ->
					make_ci_enum_field (CompletionEnumField.make ef (Self (TEnumDecl en)) true) (tpair ef.ef_type) :: acc
				) en.e_constrs fields
			in
			Some fields
		with _ ->
			abort ("Could not load module " ^ (s_type_path (p,c))) null_pos
end

let resolve_position_by_path ctx path p =
	let mt = ctx.g.do_load_type_def ctx p path in
	let p = (t_infos mt).mt_pos in
	raise_positions [p]


let handle_path_display ctx path p =
	let class_field c name =
		ignore(c.cl_build());
		let cf = PMap.find name c.cl_statics in
		let origin = match c.cl_kind with
			| KAbstractImpl a -> Self (TAbstractDecl a)
			| _ -> Self (TClassDecl c)
		in
		display_field ctx origin CFSStatic cf p
	in
	match ImportHandling.convert_import_to_something_usable display_position#get path,ctx.com.display.dms_kind with
		| (IDKPackage [s],p),DMDefault ->
			DisplayToplevel.collect_and_raise ctx TKType WithType.no_value CRImport (s,p) p
		| (IDKPackage sl,p),DMDefault ->
			let sl = match List.rev sl with
				| s :: sl -> List.rev sl
				| [] -> die "" __LOC__
			in
			raise (Parser.TypePath(sl,None,true,p))
		| (IDKPackage _,_),_ ->
			() (* ? *)
		| (IDKModule(sl,s),_),(DMDefinition | DMTypeDefinition) ->
			(* We assume that we want to go to the module file, not a specific type
			   which might not even exist anyway. *)
			let mt = ctx.g.do_load_module ctx (sl,s) p in
			let p = { pfile = mt.m_extra.m_file; pmin = 0; pmax = 0} in
			raise_positions [p]
		| (IDKModule(sl,s),_),DMHover ->
			let m = ctx.g.do_load_module ctx (sl,s) p in
			begin try
				let mt = List.find (fun mt -> snd (t_infos mt).mt_path = s) m.m_types in
				display_module_type ctx mt p;
			with Not_found ->
				()
			end
		| (IDKSubType(sl,sm,st),p),DMHover ->
			(* TODO: remove code duplication once load_type_def change is in *)
			let m = ctx.g.do_load_module ctx (sl,sm) p in
			begin try
				let mt = List.find (fun mt -> snd (t_infos mt).mt_path = st) m.m_types in
				display_module_type ctx mt p;
			with Not_found ->
				()
			end
		| (IDKModule(sl,s),p),_ ->
			raise (Parser.TypePath(sl,None,true,p))
		| (IDKSubType(sl,sm,st),p),(DMDefinition | DMTypeDefinition) ->
			resolve_position_by_path ctx (Ast.mk_type_path ~sub:st (sl,sm)) p
		| (IDKSubType(sl,sm,st),p),_ ->
			raise (Parser.TypePath(sl,Some(sm,false),true,p))
		| ((IDKSubTypeField(sl,sm,st,sf) | IDKModuleField(sl,(sm as st),sf)),p),DMDefault ->
			raise (Parser.TypePath(sl @ [sm],Some(st,false),true,p));
		| ((IDKSubTypeField(sl,sm,st,sf) | IDKModuleField(sl,(sm as st),sf)),p),_ ->
			let m = ctx.g.do_load_module ctx (sl,sm) p in
			List.iter (fun t -> match t with
				| TClassDecl c when snd c.cl_path = st ->
					class_field c sf
				| TAbstractDecl {a_impl = Some c; a_path = (_,st')} when st' = st ->
					class_field c sf
				| _ ->
					()
			) m.m_types;
		| (IDK,_),_ ->
			()