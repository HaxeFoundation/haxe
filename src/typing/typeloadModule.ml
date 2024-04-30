(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

(* Initial typing of modules and their types. *)

open Globals
open Ast
open Type
open Typecore
open DisplayTypes.DisplayMode
open Common
open Typeload
open Error
open Resolution

let get_policy g mpath =
	let sl1 = full_dot_path2 mpath mpath in
	List.fold_left (fun acc (sl2,policy,recursive) -> if match_path recursive sl1 sl2 then policy @ acc else acc) [] g.module_check_policies

let field_of_static_definition d p =
	{
		cff_name = d.d_name;
		cff_doc = d.d_doc;
		cff_pos = p;
		cff_meta = d.d_meta;
		cff_access = (AStatic,null_pos) :: d.d_flags;
		cff_kind = d.d_data;
	}

module ModuleLevel = struct
	let make_module com g mpath file loadp =
		let m = {
			m_id = alloc_mid();
			m_path = mpath;
			m_types = [];
			m_statics = None;
			m_extra = module_extra (Path.get_full_path file) (Define.get_signature com.defines) (file_time file) (if com.is_macro_context then MMacro else MCode) com.compilation_step (get_policy g mpath);
		} in
		m

	let add_module com m p =
		com.module_lut#add m.m_path m

	(*
		Build module structure : should be atomic - no type loading is possible
	*)
	let create_module_types ctx_m m tdecls loadp =
		let com = ctx_m.com in
		let imports_and_usings = DynArray.create () in
		let module_types = DynArray.create () in
		let declarations = DynArray.create () in
		let add_declaration decl mt =
			DynArray.add declarations (decl,mt);
			ctx_m.com.module_lut#add_module_type m mt;
			DynArray.add module_types mt;
		in
		let add_import_declaration i =
			DynArray.add imports_and_usings i
		in
		let statics = ref [] in
		let check_name name meta also_statics p =
			DeprecationCheck.check_is com ctx_m.m.curmod meta [] name meta p;
			let error prev_pos =
				raise_typing_error_ext (make_error (Custom ("Name " ^ name ^ " is already defined in this module")) ~sub:[
					make_error ~depth:1 (Custom (compl_msg "Previous declaration here")) prev_pos
				] p);
			in
			DynArray.iter (fun t2 ->
				if snd (t_path t2) = name then error (t_infos t2).mt_name_pos
			) module_types;
			if also_statics then
				List.iter (fun (d,_) ->
					if fst d.d_name = name then error (snd d.d_name)
				) !statics
		in
		let make_path name priv meta p =
			check_name name meta true p;
			if priv then (fst m.m_path @ ["_" ^ snd m.m_path], name) else (fst m.m_path, name)
		in
		let has_declaration = ref false in
		let check_type_name type_name meta p =
			let module_name = snd m.m_path in
			if type_name <> module_name && not (Meta.has Meta.Native meta) then Naming.check_uppercase_identifier_name ctx_m.com type_name "type" p;
		in
		let handle_class_decl d p =
			let name = fst d.d_name in
			has_declaration := true;
			let priv = List.mem HPrivate d.d_flags in
			let path = make_path name priv d.d_meta (snd d.d_name) in
			let c = mk_class m path p (pos d.d_name) in
			(* we shouldn't load any other type until we propertly set cl_build *)
			c.cl_build <- (fun() -> raise_typing_error (s_type_path c.cl_path ^ " is not ready to be accessed, separate your type declarations in several files") p);
			c.cl_module <- m;
			c.cl_private <- priv;
			c.cl_doc <- d.d_doc;
			c.cl_meta <- d.d_meta;
			if List.mem HAbstract d.d_flags then add_class_flag c CAbstract;
			List.iter (function
				| HExtern -> add_class_flag c CExtern
				| HInterface -> add_class_flag c CInterface
				| HFinal -> add_class_flag c CFinal
				| _ -> ()
			) d.d_flags;
			if not (has_class_flag c CExtern) then check_type_name name d.d_meta p;
			if has_class_flag c CAbstract then begin
				if has_class_flag c CInterface then display_error com "An interface may not be abstract" c.cl_name_pos;
				if has_class_flag c CFinal then display_error com "An abstract class may not be final" c.cl_name_pos;
			end;
			c
		in
		let make_decl decl =
			let p = snd decl in
			match fst decl with
				| EImport _ | EUsing _ ->
					if !has_declaration then raise_typing_error "import and using may not appear after a declaration" p;
					add_import_declaration decl
				| EStatic d ->
					check_name (fst d.d_name) d.d_meta false (snd d.d_name);
					has_declaration := true;
					statics := (d,p) :: !statics;
				| EClass d ->
					add_declaration decl (TClassDecl (handle_class_decl d p))
				| EEnum d ->
					let name = fst d.d_name in
					has_declaration := true;
					let priv = List.mem EPrivate d.d_flags in
					let path = make_path name priv d.d_meta p in
					if Meta.has (Meta.Custom ":fakeEnum") d.d_meta then raise_typing_error "@:fakeEnum enums is no longer supported in Haxe 4, use extern enum abstract instead" p;
					let e = {
						(mk_enum m path p (pos d.d_name)) with
						e_doc = d.d_doc;
						e_meta = d.d_meta;
						e_private = priv;
						e_extern = List.mem EExtern d.d_flags;
					} in
					if not e.e_extern then check_type_name name d.d_meta p;
					add_declaration decl (TEnumDecl e)
				| ETypedef d ->
					let name = fst d.d_name in
					check_type_name name d.d_meta p;
					has_declaration := true;
					let priv = List.mem TDPrivate d.d_flags in
					let path = make_path name priv d.d_meta p in
					let t = {(mk_typedef m path p (pos d.d_name) (mk_mono())) with
						t_doc = d.d_doc;
						t_private = priv;
						t_meta = d.d_meta;
					} in
					(* failsafe in case the typedef is not initialized (see #3933) *)
					delay ctx_m.g PBuildModule (fun () ->
						match t.t_type with
						| TMono r -> (match r.tm_type with None -> Monomorph.bind r com.basic.tvoid | _ -> ())
						| _ -> ()
					);
					add_declaration decl (TTypeDecl t)
				| EAbstract d ->
					let name = fst d.d_name in
					check_type_name name d.d_meta p;
					let priv = List.mem AbPrivate d.d_flags in
					let path = make_path name priv d.d_meta p in
					let p_enum_meta = Meta.maybe_get_pos Meta.Enum d.d_meta in
					let a = {
						a_path = path;
						a_private = priv;
						a_module = m;
						a_pos = p;
						a_name_pos = pos d.d_name;
						a_doc = d.d_doc;
						a_params = [];
						a_using = [];
						a_restore = (fun () -> ());
						a_meta = d.d_meta;
						a_from = [];
						a_to = [];
						a_from_field = [];
						a_to_field = [];
						a_ops = [];
						a_unops = [];
						a_impl = None;
						a_array = [];
						a_this = mk_mono();
						a_read = None;
						a_write = None;
						a_call = None;
						a_extern = List.mem AbExtern d.d_flags;
						a_enum = List.mem AbEnum d.d_flags || p_enum_meta <> None;
					} in
					begin match p_enum_meta with
						| None when a.a_enum -> a.a_meta <- (Meta.Enum,[],null_pos) :: a.a_meta; (* HAXE5: remove *)
						| None -> ()
						| Some p ->
							let options = Warning.from_meta d.d_meta in
							module_warning com ctx_m.m.curmod WDeprecatedEnumAbstract options "`@:enum abstract` is deprecated in favor of `enum abstract`" p
					end;
					add_declaration decl (TAbstractDecl a);
					begin match d.d_data with
					| [] when Meta.has Meta.CoreType a.a_meta ->
						a.a_this <- t_dynamic;
					| fields ->
						let a_t =
							let params = List.map (fun t -> TPType (make_ptp_th (mk_type_path ([],fst t.tp_name)) null_pos)) d.d_params in
							make_ptp_ct_null (mk_type_path ~params ([],fst d.d_name)),null_pos
						in
						let rec loop = function
							| [] -> a_t
							| AbOver t :: _ -> t
							| _ :: l -> loop l
						in
						let this_t = loop d.d_flags in
						let fields = List.map (TypeloadFields.transform_abstract_field com this_t a_t a) fields in
						let meta = ref [] in
						if has_meta Meta.Dce a.a_meta then meta := (Meta.Dce,[],null_pos) :: !meta;
						let c_decl = { d_name = (fst d.d_name) ^ "_Impl_",snd d.d_name; d_flags = [HPrivate]; d_data = fields; d_doc = None; d_params = []; d_meta = !meta } in
						let c = handle_class_decl c_decl p in
						a.a_impl <- Some c;
						c.cl_kind <- KAbstractImpl a;
						add_class_flag c CFinal;
						add_declaration (EClass c_decl,p) (TClassDecl c);
					end;
		in
		List.iter make_decl tdecls;
		begin match !statics with
			| [] ->
				()
			| statics ->
				let first_pos = ref null_pos in
				let fields = List.map (fun (d,p) ->
					first_pos := p;
					field_of_static_definition d p;
				) statics in
				let p = let p = !first_pos in { p with pmax = p.pmin } in
				let c_def = {
					d_name = (snd m.m_path) ^ "_Fields_", null_pos;
					d_flags = [HPrivate];
					d_data = List.rev fields;
					d_doc = None;
					d_params = [];
					d_meta = []
				} in
				let c = handle_class_decl c_def p in
				assert (m.m_statics = None);
				m.m_statics <- Some c;
				c.cl_kind <- KModuleFields m;
				add_class_flag c CFinal;
				add_declaration (EClass c_def,p) (TClassDecl c);
		end;
		(* During the initial module_lut#add in type_module, m has no m_types yet by design.
		   We manually add them here. This and module_lut#add itself should be the only places
		   in the compiler that call add_module_type. *)
		m.m_types <- m.m_types @ (DynArray.to_list module_types);
		DynArray.to_list imports_and_usings,DynArray.to_list declarations

	let handle_import_hx com g m decls p =
		let path_split = match List.rev (Path.get_path_parts (Path.UniqueKey.lazy_path m.m_extra.m_file)) with
			| [] -> []
			| _ :: l -> l
		in
		let join l = String.concat Path.path_sep (List.rev ("import.hx" :: l)) in
		let rec loop path pack = match path,pack with
			| _,[] -> [join path]
			| (p :: path),(_ :: pack) -> (join (p :: path)) :: (loop path pack)
			| _ -> []
		in
		let candidates = loop path_split (fst m.m_path) in
		let make_import_module path r =
			com.parser_cache#add path r;
			(* We use the file path as module name to make it unique. This may or may not be a good idea... *)
			let m_import = make_module com g ([],path) path p in
			m_import.m_extra.m_kind <- MImport;
			m_import
		in
		List.fold_left (fun acc path ->
			let decls = try
				let r = com.parser_cache#find path in
				let mimport = com.module_lut#find ([],path) in
				if mimport.m_extra.m_kind <> MFake then add_dependency m mimport;
				r
			with Not_found ->
				if Sys.file_exists path then begin
					let _,r = match !TypeloadParse.parse_hook com (ClassPaths.create_resolved_file path com.empty_class_path) p with
						| ParseSuccess(data,_,_) -> data
						| ParseError(_,(msg,p),_) -> Parser.error msg p
					in
					List.iter (fun (d,p) -> match d with EImport _ | EUsing _ -> () | _ -> raise_typing_error "Only import and using is allowed in import.hx files" p) r;
					let m_import = make_import_module path r in
					add_module com m_import p;
					add_dependency m m_import;
					r
				end else begin
					let r = [] in
					(* Add empty decls so we don't check the file system all the time. *)
					(make_import_module path r).m_extra.m_kind <- MFake;
					r
				end
			in
			decls @ acc
		) decls candidates

	let init_type_params ctx_m decls =
		(* here is an additional PASS 1 phase, which define the type parameters for all module types.
		 Constraints are handled lazily (no other type is loaded) because they might be recursive anyway *)
		 List.iter (fun d ->
			match d with
			| ((EClass d, p),TClassDecl c) ->
				c.cl_params <- type_type_params ctx_m TPHType c.cl_path p d.d_params;
				if Meta.has Meta.Generic c.cl_meta && c.cl_params <> [] then c.cl_kind <- KGeneric;
				if Meta.has Meta.FunctionalInterface c.cl_meta then begin
					if not (has_class_flag c CInterface) then
						raise_typing_error "@:functionalInterface is only allowed on interfaces, as the name implies" c.cl_name_pos
					else
						add_class_flag c CFunctionalInterface
				end;
				if Meta.has Meta.GenericBuild c.cl_meta then begin
					if ctx_m.com.is_macro_context then raise_typing_error "@:genericBuild cannot be used in macros" c.cl_pos;
					c.cl_kind <- KGenericBuild d.d_data;
				end;
				if c.cl_path = (["haxe";"macro"],"MacroType") then c.cl_kind <- KMacroType;
			| ((EEnum d, p),TEnumDecl e) ->
				e.e_params <- type_type_params ctx_m TPHType e.e_path p d.d_params;
			| ((ETypedef d, p),TTypeDecl t) ->
				t.t_params <- type_type_params ctx_m TPHType t.t_path p d.d_params;
			| ((EAbstract d, p),TAbstractDecl a) ->
				a.a_params <- type_type_params ctx_m TPHType a.a_path p d.d_params;
			| _ ->
				die "" __LOC__
		) decls
end

module TypeLevel = struct
	let load_enum_field ctx_en e et is_flat index c =
		let p = c.ec_pos in
		let params = type_type_params ctx_en TPHEnumConstructor ([],fst c.ec_name) c.ec_pos c.ec_params in
		let ctx_ef = TyperManager.clone_for_enum_field ctx_en (params @ ctx_en.type_params) in
		let rt = (match c.ec_type with
			| None -> et
			| Some (t,pt) ->
				let t = load_complex_type ctx_ef true LoadReturn (t,pt) in
				(match follow t with
				| TEnum (te,_) when te == e ->
					()
				| _ ->
					raise_typing_error "Explicit enum type must be of the same enum type" pt);
				t
		) in
		let t = (match c.ec_args with
			| [] ->
				rt
			| l ->
				is_flat := false;
				let pnames = ref PMap.empty in
				TFun (List.map (fun (s,opt,(t,tp)) ->
					(match t with CTPath({path = {tpackage=[];tname="Void"}}) -> raise_typing_error "Arguments of type Void are not allowed in enum constructors" tp | _ -> ());
					if PMap.mem s (!pnames) then raise_typing_error ("Duplicate argument `" ^ s ^ "` in enum constructor " ^ fst c.ec_name) p;
					pnames := PMap.add s () (!pnames);
					s, opt, load_type_hint ~opt ctx_ef p LoadNormal (Some (t,tp))
				) l, rt)
		) in
		let f = {
			ef_name = fst c.ec_name;
			ef_type = t;
			ef_pos = p;
			ef_name_pos = snd c.ec_name;
			ef_doc = c.ec_doc;
			ef_index = !index;
			ef_params = params;
			ef_meta = c.ec_meta;
		} in
		DeprecationCheck.check_is ctx_ef.com ctx_ef.m.curmod e.e_meta f.ef_meta f.ef_name f.ef_meta f.ef_name_pos;
		if ctx_ef.m.is_display_file && DisplayPosition.display_position#enclosed_in f.ef_name_pos then
			DisplayEmitter.display_enum_field ctx_ef e f p;
		f

	let init_class ctx_m c d p =
		if ctx_m.m.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx_m (match c.cl_kind with KAbstractImpl a -> TAbstractDecl a | _ -> TClassDecl c) (pos d.d_name);
		(match c.cl_kind with
			| KAbstractImpl _ -> ()
			| _ -> TypeloadCheck.check_global_metadata ctx_m c.cl_meta (fun m -> c.cl_meta <- m :: c.cl_meta) c.cl_module.m_path c.cl_path None
		);
		let herits = d.d_flags in
		List.iter (fun (m,_,p) ->
			if m = Meta.Final then begin
				add_class_flag c CFinal;
			end
		) d.d_meta;
		let prev_build_count = ref (ctx_m.g.build_count - 1) in
		let cctx = TypeloadFields.create_class_context c p in
		let ctx_c = TypeloadFields.create_typer_context_for_class ctx_m cctx p in
		let build() =
			c.cl_build <- (fun()-> Building [c]);
			let fl = TypeloadCheck.Inheritance.set_heritance ctx_c c herits p in
			let rec build() =
				c.cl_build <- (fun()-> Building [c]);
				try
					List.iter (fun f -> f()) fl;
					TypeloadFields.init_class ctx_c cctx c p d.d_flags d.d_data;
					c.cl_build <- (fun()-> Built);
					ctx_c.g.build_count <- ctx_c.g.build_count + 1;
					List.iter (fun tp -> ignore(follow tp.ttp_type)) c.cl_params;
					Built;
				with TypeloadCheck.Build_canceled state ->
					c.cl_build <- make_pass ctx_c build;
					let rebuild() =
						delay_late ctx_c.g PBuildClass (fun() -> ignore(c.cl_build()));
					in
					(match state with
					| Built -> die "" __LOC__
					| Building cl ->
						if ctx_c.g.build_count = !prev_build_count then raise_typing_error ("Loop in class building prevent compiler termination (" ^ String.concat "," (List.map (fun c -> s_type_path c.cl_path) cl) ^ ")") c.cl_pos;
						prev_build_count := ctx_c.g.build_count;
						rebuild();
						Building (c :: cl)
					| BuildMacro f ->
						f := rebuild :: !f;
						state);
				| exn ->
					c.cl_build <- (fun()-> Built);
					raise exn
			in
			build()
		in
		c.cl_build <- make_pass ctx_m build;
		delay ctx_m.g PBuildClass (fun() -> ignore(c.cl_build()));
		if Meta.has Meta.InheritDoc c.cl_meta then
			delay ctx_m.g PConnectField (fun() -> InheritDoc.build_class_doc ctx_m c);
		if (ctx_m.com.platform = Jvm) && not (has_class_flag c CExtern) then
			delay ctx_m.g PTypeField (fun () ->
				let metas = StrictMeta.check_strict_meta ctx_m c.cl_meta in
				if metas <> [] then c.cl_meta <- metas @ c.cl_meta;
				let rec run_field cf =
					let metas = StrictMeta.check_strict_meta ctx_m cf.cf_meta in
					if metas <> [] then cf.cf_meta <- metas @ cf.cf_meta;
					List.iter run_field cf.cf_overloads
				in
				List.iter run_field c.cl_ordered_statics;
				List.iter run_field c.cl_ordered_fields;
				match c.cl_constructor with
					| Some f -> run_field f
					| _ -> ()
			)

	let init_enum ctx_m e d p =
		if ctx_m.m.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx_m (TEnumDecl e) (pos d.d_name);
		let ctx_en = TyperManager.clone_for_enum ctx_m e in
		TypeloadCheck.check_global_metadata ctx_en e.e_meta (fun m -> e.e_meta <- m :: e.e_meta) e.e_module.m_path e.e_path None;
		let constructs = ref d.d_data in
		let get_constructs() =
			List.map (fun c ->
				{
					cff_name = c.ec_name;
					cff_doc = c.ec_doc;
					cff_meta = c.ec_meta;
					cff_pos = c.ec_pos;
					cff_access = [];
					cff_kind = (match c.ec_args, c.ec_params with
						| [], [] -> FVar (c.ec_type,None)
						| _ -> FFun { f_params = c.ec_params; f_type = c.ec_type; f_expr = None; f_args = List.map (fun (n,o,t) -> (n,null_pos),o,[],Some t,None) c.ec_args });
				}
			) (!constructs)
		in
		TypeloadFields.build_module_def ctx_en (TEnumDecl e) e.e_meta get_constructs (fun (e,p) ->
			match e with
			| EVars [{ ev_type = Some (CTAnonymous fields,p); ev_expr = None }] ->
				constructs := List.map (fun f ->
					let args, params, t = (match f.cff_kind with
					| FVar (t,None) -> [], [], t
					| FFun { f_params = pl; f_type = t; f_expr = (None|Some (EBlock [],_)); f_args = al } ->
						let al = List.map (fun ((n,_),o,_,t,_) -> match t with None -> raise_typing_error "Missing function parameter type" f.cff_pos | Some t -> n,o,t) al in
						al, pl, t
					| _ ->
						raise_typing_error "Invalid enum constructor in @:build result" p
					) in
					{
						ec_name = f.cff_name;
						ec_doc = f.cff_doc;
						ec_meta = f.cff_meta;
						ec_pos = f.cff_pos;
						ec_args = args;
						ec_params = params;
						ec_type = t;
					}
				) fields
			| _ -> raise_typing_error "Enum build macro must return a single variable with anonymous object fields" p
		);
		let et = TEnum (e,extract_param_types e.e_params) in
		let names = ref [] in
		let index = ref 0 in
		let is_flat = ref true in
		List.iter (fun c ->
			if PMap.mem (fst c.ec_name) e.e_constrs then raise_typing_error ("Duplicate constructor " ^ fst c.ec_name) (pos c.ec_name);
			let f = load_enum_field ctx_en e et is_flat index c in
			e.e_constrs <- PMap.add f.ef_name f e.e_constrs;
			incr index;
			names := (fst c.ec_name) :: !names;
			if Meta.has Meta.InheritDoc f.ef_meta then
				delay ctx_en.g PConnectField (fun() -> InheritDoc.build_enum_field_doc ctx_en f);
		) (!constructs);
		e.e_names <- List.rev !names;
		e.e_extern <- e.e_extern;
		unify ctx_en (TType(enum_module_type e,[])) e.e_type p;
		if !is_flat then e.e_meta <- (Meta.FlatEnum,[],null_pos) :: e.e_meta;
		if Meta.has Meta.InheritDoc e.e_meta then
			delay ctx_en.g PConnectField (fun() -> InheritDoc.build_enum_doc ctx_en e);
		if (ctx_en.com.platform = Jvm) && not e.e_extern then
			delay ctx_en.g PTypeField (fun () ->
				let metas = StrictMeta.check_strict_meta ctx_en e.e_meta in
				e.e_meta <- metas @ e.e_meta;
				PMap.iter (fun _ ef ->
					let metas = StrictMeta.check_strict_meta ctx_en ef.ef_meta in
					if metas <> [] then ef.ef_meta <- metas @ ef.ef_meta
				) e.e_constrs
			)

	let init_typedef ctx_m t d p =
		if ctx_m.m.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx_m (TTypeDecl t) (pos d.d_name);
		TypeloadCheck.check_global_metadata ctx_m t.t_meta (fun m -> t.t_meta <- m :: t.t_meta) t.t_module.m_path t.t_path None;
		let ctx_td = TyperManager.clone_for_typedef ctx_m t in
		let tt = load_complex_type ctx_td true LoadNormal d.d_data in
		let tt = (match fst d.d_data with
		| CTExtend _ -> tt
		| CTPath { path = {tpackage = ["haxe";"macro"]; tname = "MacroType" }} ->
			(* we need to follow MacroType immediately since it might define other module types that we will load afterwards *)
			if t.t_type == follow tt then raise_typing_error "Recursive typedef is not allowed" p;
			tt
		| _ ->
			if (Meta.has Meta.Eager d.d_meta) then
				follow tt
			else begin
				let rec check_rec tt =
					if tt == t.t_type then raise_typing_error "Recursive typedef is not allowed" p;
					match tt with
					| TMono r ->
						(match r.tm_type with
						| None -> ()
						| Some t -> check_rec t)
					| TLazy f ->
						check_rec (lazy_type f);
					| TType (td,tl) ->
						if td == t then raise_typing_error "Recursive typedef is not allowed" p;
						check_rec (apply_typedef td tl)
					| _ ->
						()
				in
				let r = make_lazy ctx_td.g tt (fun r ->
					check_rec tt;
					tt
				) "typedef_rec_check" in
				TLazy r
			end
		) in
		(match t.t_type with
		| TMono r ->
			(match r.tm_type with
			| None -> Monomorph.bind r tt;
			| Some t' -> die (Printf.sprintf "typedef %s is already initialized to %s, but new init to %s was attempted" (s_type_path t.t_path) (s_type_kind t') (s_type_kind tt)) __LOC__);
		| _ -> die "" __LOC__);
		TypeloadFields.build_module_def ctx_td (TTypeDecl t) t.t_meta (fun _ -> []) (fun _ -> ())

	let init_abstract ctx_m a d p =
		if ctx_m.m.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx_m (TAbstractDecl a) (pos d.d_name);
		TypeloadCheck.check_global_metadata ctx_m a.a_meta (fun m -> a.a_meta <- m :: a.a_meta) a.a_module.m_path a.a_path None;
		Option.may (fun c ->
			List.iter (fun m -> match m with
				| ((Meta.Using | Meta.Build | Meta.CoreApi | Meta.Allow | Meta.Access | Meta.Enum | Meta.Dce | Meta.Native | Meta.HlNative | Meta.JsRequire | Meta.PythonImport | Meta.Expose | Meta.Deprecated | Meta.PhpGlobal | Meta.PublicFields),_,_) ->
					c.cl_meta <- m :: c.cl_meta;
				| _ ->
					()
			) a.a_meta;
		) a.a_impl;
		let ctx_a = TyperManager.clone_for_abstract ctx_m a in
		let is_type = ref false in
		let load_type t from =
			let _, pos = t in
			let t = load_complex_type ctx_a true LoadNormal t in
			let t = if not (Meta.has Meta.CoreType a.a_meta) then begin
				if !is_type then begin
					let r = make_lazy ctx_a.g t (fun r ->
						(try (if from then Type.unify t a.a_this else Type.unify a.a_this t) with Unify_error _ -> raise_typing_error "You can only declare from/to with compatible types" pos);
						t
					) "constraint" in
					TLazy r
				end else
					raise_typing_error "Missing underlying type declaration or @:coreType declaration" p;
			end else begin
				if Meta.has Meta.Callable a.a_meta then
					raise_typing_error "@:coreType abstracts cannot be @:callable" p;
				t
			end in
			t
		in
		List.iter (function
			| AbFrom t -> a.a_from <- (load_type t true) :: a.a_from
			| AbTo t -> a.a_to <- (load_type t false) :: a.a_to
			| AbOver t ->
				if a.a_impl = None then raise_typing_error "Abstracts with underlying type must have an implementation" a.a_pos;
				if Meta.has Meta.CoreType a.a_meta then raise_typing_error "@:coreType abstracts cannot have an underlying type" p;
				let at = load_complex_type ctx_a true LoadNormal t in
				delay ctx_a.g PForce (fun () ->
					let rec loop stack t =
						match follow t with
						| TAbstract(a,_) when not (Meta.has Meta.CoreType a.a_meta) ->
							if List.memq a stack then
								raise_typing_error "Abstract underlying type cannot be recursive" a.a_pos
							else
								loop (a :: stack) a.a_this
						| _ -> ()
					in
					loop [] at
				);
				a.a_this <- at;
				is_type := true;
			| AbExtern ->
				(match a.a_impl with Some c -> add_class_flag c CExtern | None -> (* Hmmmm.... *) ())
			| AbPrivate | AbEnum -> ()
		) d.d_flags;
		a.a_from <- List.rev a.a_from;
		a.a_to <- List.rev a.a_to;
		if not !is_type then begin
			if Meta.has Meta.CoreType a.a_meta then
				a.a_this <- TAbstract(a,extract_param_types a.a_params)
			else
				raise_typing_error "Abstract is missing underlying type declaration" a.a_pos
		end;
		if Meta.has Meta.InheritDoc a.a_meta then
			delay ctx_a.g PConnectField (fun() -> InheritDoc.build_abstract_doc ctx_a a)

	(*
		In this pass, we can access load and access other modules types, but we cannot follow them or access their structure
		since they have not been setup. We also build a list that will be evaluated the first time we evaluate
		an expression into the context
	*)
	let init_module_type ctx_m ((decl,p),tdecl) =
		match decl with
		| EClass d ->
			let c = (match tdecl with TClassDecl c -> c | _ -> die "" __LOC__) in
			init_class ctx_m c d p
		| EEnum d ->
			let e = (match tdecl with TEnumDecl e -> e | _ -> die "" __LOC__) in
			init_enum ctx_m e d p
		| ETypedef d ->
			let t = (match tdecl with TTypeDecl t -> t | _ -> die "" __LOC__) in
			init_typedef ctx_m t d p
		| EAbstract d ->
			let a = (match tdecl with TAbstractDecl a -> a | _ -> die "" __LOC__) in
			init_abstract ctx_m a d p
		| _ ->
			die "" __LOC__

	let init_imports_or_using ctx_m (decl,p) =
		let com = ctx_m.com in
		let check_path_display path p =
			if DisplayPosition.display_position#is_in_file (com.file_keys#get p.pfile) then DisplayPath.handle_path_display ctx_m path p
		in
		match decl with
		| EImport (path,mode) ->
			begin try
				check_path_display path p;
				ImportHandling.init_import ctx_m path mode p;
				ImportHandling.commit_import ctx_m path mode p;
			with Error err ->
				display_error_ext com err
			end
		| EUsing path ->
			check_path_display path p;
			ImportHandling.init_using ctx_m path p
		| _ ->
			die "" __LOC__
end

let make_curmod com g m =
	let rl = new resolution_list ["import";s_type_path m.m_path] in
	List.iter (fun mt ->
		rl#add (module_type_resolution mt None null_pos))
	(List.rev g.std_types.m_types);
	{
		curmod = m;
		import_resolution = rl;
		own_resolution = None;
		enum_with_type = None;
		module_using = [];
		import_statements = [];
		is_display_file = (com.display.dms_kind <> DMNone && DisplayPosition.display_position#is_in_file (Path.UniqueKey.lazy_key m.m_extra.m_file));
	}

(*
	Creates a module context for [m] and types [tdecls] using it.
*)
let type_types_into_module com g m tdecls p =
	let ctx_m = TyperManager.clone_for_module g.root_typer (make_curmod com g m) in
	let imports_and_usings,decls = ModuleLevel.create_module_types ctx_m m tdecls p in
	(* define the per-module context for the next pass *)
	if ctx_m.g.std_types != null_module then begin
		add_dependency m ctx_m.g.std_types;
		(* this will ensure both String and (indirectly) Array which are basic types which might be referenced *)
		ignore(load_instance ctx_m (make_ptp (mk_type_path (["std"],"String")) null_pos) ParamNormal LoadNormal)
	end;
	ModuleLevel.init_type_params ctx_m decls;
	List.iter (TypeLevel.init_imports_or_using ctx_m) imports_and_usings;
	(* setup module types *)
	List.iter (TypeLevel.init_module_type ctx_m) decls;
	(* Make sure that we actually init the context at some point (issue #9012) *)
	delay ctx_m.g PConnectField (fun () -> ctx_m.m.import_resolution#resolve_lazies);
	ctx_m

(*
	Creates a new module and types [tdecls] into it.
*)
let type_module com g mpath file ?(dont_check_path=false) ?(is_extern=false) tdecls p =
	let m = ModuleLevel.make_module com g mpath file p in
	com.module_lut#add m.m_path m;
	let tdecls = ModuleLevel.handle_import_hx com g m tdecls p in
	let ctx_m = type_types_into_module com g m tdecls p in
	if is_extern then m.m_extra.m_kind <- MExtern else if not dont_check_path then Naming.check_module_path ctx_m.com m.m_path p;
	m

(* let type_module ctx mpath file ?(is_extern=false) tdecls p =
	let timer = Timer.timer ["typing";"type_module"] in
	Std.finally timer (type_module ctx mpath file ~is_extern tdecls) p *)

class hxb_reader_api_typeload
	(com : context)
	(g : typer_globals)
	(load_module : context -> typer_globals -> path -> pos -> module_def)
	(p : pos)
= object(self)
	method make_module (path : path) (file : string) =
		let m = ModuleLevel.make_module com g path file p in
		m.m_extra.m_processed <- 1;
		m

	method add_module (m : module_def) =
		com.module_lut#add m.m_path m

	method resolve_type (pack : string list) (mname : string) (tname : string) =
		let m = load_module com g (pack,mname) p in
		List.find (fun t -> snd (t_path t) = tname) m.m_types

	method resolve_module (path : path) =
		load_module com g path p

	method basic_types =
		com.basic

	method get_var_id (i : int) =
		(* The v_id in .hxb has no relation to this context, make a new one. *)
		let uid = fst alloc_var' in
		incr uid;
		!uid

	method read_expression_eagerly (cf : tclass_field) =
		com.is_macro_context || match cf.cf_kind with
			| Var _ ->
				true
			| Method _ ->
				delay g PTypeField (fun () -> ignore(follow cf.cf_type));
				false
end

let rec load_hxb_module com g path p =
	let read file bytes =
		try
			let api = (new hxb_reader_api_typeload com g load_module' p :> HxbReaderApi.hxb_reader_api) in
			let reader = new HxbReader.hxb_reader path com.hxb_reader_stats in
			let read = reader#read api bytes in
			let m = read EOT in
			delay g PConnectField (fun () ->
				ignore(read EOM);
			);
			m
		with e ->
			Printf.eprintf "\x1b[30;41mError loading %s from %s\x1b[0m\n" (snd path) file;
			let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
			Printf.eprintf " => %s\n%s\n" msg stack;
			raise e
	in
	let target = Common.platform_name_macro com in
	let rec loop l = match l with
		| hxb_lib :: l ->
			begin match hxb_lib#get_bytes target path with
				| Some bytes ->
					read hxb_lib#get_file_path bytes
				| None ->
					loop l
			end
		| [] ->
			raise Not_found
	in
	loop com.hxb_libs

and load_module' com g m p =
	try
		(* Check current context *)
		com.module_lut#find m
	with Not_found ->
		(* Check cache *)
		match !TypeloadCacheHook.type_module_hook com (delay g PConnectField) m p with
		| GoodModule m ->
			m
		| BinaryModule _ ->
			die "" __LOC__ (* The server builds those *)
		| NoModule | BadModule _ -> try
			load_hxb_module com g m p
		with Not_found ->
			let raise_not_found () = raise_error_msg (Module_not_found m) p in
			if com.module_nonexistent_lut#mem m then raise_not_found();
			if g.load_only_cached_modules then raise_not_found();
			let is_extern = ref false in
			let file, decls = try
				(* Try parsing *)
				let rfile,decls = TypeloadParse.parse_module com m p in
				rfile.file,decls
			with Not_found ->
				(* Nothing to parse, try loading extern type *)
				let rec loop = function
					| [] ->
						com.module_nonexistent_lut#add m true;
						raise_not_found()
					| (file,load) :: l ->
						match load m p with
						| None -> loop l
						| Some (_,a) -> file, a
				in
				is_extern := true;
				loop com.load_extern_type
			in
			let is_extern = !is_extern in
			type_module com g m file ~is_extern decls p

let load_module ctx m p =
	let m2 = load_module' ctx.com ctx.g m p in
	add_dependency ~skip_postprocess:true ctx.m.curmod m2;
	if ctx.pass = PTypeField then flush_pass ctx.g PConnectField ("load_module",fst m @ [snd m]);
	m2

(* let load_module ctx m p =
	let timer = Timer.timer ["typing";"load_module"] in
	Std.finally timer (load_module ctx m) p *)

;;
