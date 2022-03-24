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
open Filename
open Type
open Typecore
open DisplayTypes.DisplayMode
open DisplayTypes.CompletionResultKind
open Common
open Typeload
open Error

let get_policy ctx mpath =
	let sl1 = full_dot_path2 mpath mpath in
	List.fold_left (fun acc (sl2,policy,recursive) -> if match_path recursive sl1 sl2 then policy @ acc else acc) [] ctx.g.module_check_policies

let make_module ctx mpath file loadp =
	let m = {
		m_id = alloc_mid();
		m_path = mpath;
		m_types = [];
		m_statics = None;
		m_extra = module_extra (Path.get_full_path file) (Define.get_signature ctx.com.defines) (file_time file) (if ctx.com.is_macro_context then MMacro else MCode) (get_policy ctx mpath);
	} in
	m

let add_module ctx m p =
	List.iter (TypeloadCheck.check_module_types ctx m p) m.m_types;
	Hashtbl.add ctx.g.modules m.m_path m

module StrictMeta = struct
	let get_native_repr md pos =
		let path, meta = match md with
			| TClassDecl cl -> cl.cl_path, cl.cl_meta
			| TEnumDecl e -> e.e_path, e.e_meta
			| TTypeDecl t -> t.t_path, t.t_meta
			| TAbstractDecl a -> a.a_path, a.a_meta
		in
		let rec loop acc = function
			| (Meta.JavaCanonical,[EConst(String(pack,_)),_; EConst(String(name,_)),_],_) :: _ ->
				ExtString.String.nsplit pack ".", name
			| (Meta.Native,[EConst(String(name,_)),_],_) :: meta ->
				loop (Ast.parse_path name) meta
			| _ :: meta ->
				loop acc meta
			| [] ->
				acc
		in
		let pack, name = loop path meta in
		match pack with
			| [] ->
				(EConst(Ident(name)), pos)
			| hd :: tl ->
				let rec loop pack expr = match pack with
					| hd :: tl ->
						loop tl (efield(expr,hd),pos)
					| [] ->
						(efield(expr,name),pos)
				in
				loop tl (EConst(Ident(hd)),pos)

	let rec process_meta_argument ?(toplevel=true) ctx expr = match expr.eexpr with
		| TField(e,f) ->
			(efield(process_meta_argument ~toplevel:false ctx e,field_name f),expr.epos)
		| TConst(TInt i) ->
			(EConst(Int (Int32.to_string i, None)), expr.epos)
		| TConst(TFloat f) ->
			(EConst(Float (f, None)), expr.epos)
		| TConst(TString s) ->
			(EConst(String(s,SDoubleQuotes)), expr.epos)
		| TConst TNull ->
			(EConst(Ident "null"), expr.epos)
		| TConst(TBool b) ->
			(EConst(Ident (string_of_bool b)), expr.epos)
		| TCast(e,_) | TMeta(_,e) | TParenthesis(e) ->
			process_meta_argument ~toplevel ctx e
		| TTypeExpr md when toplevel ->
			let p = expr.epos in
			if ctx.com.platform = Cs then
				(ECall( (EConst(Ident "typeof"), p), [get_native_repr md expr.epos] ), p)
			else
				(efield(get_native_repr md expr.epos, "class"), p)
		| TTypeExpr md ->
			get_native_repr md expr.epos
		| _ ->
			display_error ctx "This expression is too complex to be a strict metadata argument" expr.epos;
			(EConst(Ident "null"), expr.epos)

	let handle_fields ctx fields_to_check with_type_expr =
		List.map (fun ((name,_,_),expr) ->
			let pos = snd expr in
			let field = (efield(with_type_expr,name), pos) in
			let fieldexpr = (EConst(Ident name),pos) in
			let left_side = match ctx.com.platform with
				| Cs -> field
				| Java -> (ECall(field,[]),pos)
				| _ -> die "" __LOC__
			in

			let left = type_expr ctx left_side NoValue in
			let right = type_expr ctx expr (WithType.with_type left.etype) in
			unify ctx left.etype right.etype (snd expr);
			(EBinop(Ast.OpAssign,fieldexpr,process_meta_argument ctx right), pos)
		) fields_to_check

	let make_meta ctx texpr extra =
		match texpr.eexpr with
			| TNew(c,_,el) ->
				ECall(get_native_repr (TClassDecl c) texpr.epos, (List.map (process_meta_argument ctx) el) @ extra), texpr.epos
			| TTypeExpr(md) ->
				ECall(get_native_repr md texpr.epos, extra), texpr.epos
			| _ ->
				display_error ctx "Unexpected expression" texpr.epos; die "" __LOC__

	let get_strict_meta ctx meta params pos =
		let pf = ctx.com.platform in
		let changed_expr, fields_to_check, ctype = match params with
			| [ECall(ef, el),p] ->
				let tpath = field_to_type_path ctx ef in
				begin match pf with
				| Cs ->
					let el, fields = match List.rev el with
						| (EObjectDecl(decl),_) :: el ->
							List.rev el, decl
						| _ ->
							el, []
					in
					(ENew((tpath,snd ef), el), p), fields, CTPath tpath
				| Java ->
					let fields = match el with
					| [EObjectDecl(fields),_] ->
						fields
					| [] ->
						[]
					| (_,p) :: _ ->
						display_error ctx "Object declaration expected" p;
						[]
					in
					ef, fields, CTPath tpath
				| _ ->
					Error.typing_error "@:strict is not supported on this target" p
				end
			| [EConst(Ident i),p as expr] ->
				let tpath = { tpackage=[]; tname=i; tparams=[]; tsub=None } in
				if pf = Cs then
					(ENew((tpath,p), []), p), [], CTPath tpath
				else
					expr, [], CTPath tpath
			| [ (EField(_),p as field) ] ->
				let tpath = field_to_type_path ctx field in
				if pf = Cs then
					(ENew((tpath,p), []), p), [], CTPath tpath
				else
					field, [], CTPath tpath
			| _ ->
				display_error ctx "A @:strict metadata must contain exactly one parameter. Please check the documentation for more information" pos;
				raise Exit
		in
		let texpr = type_expr ctx changed_expr NoValue in
		let with_type_expr = (ECheckType( (EConst (Ident "null"), pos), (ctype,null_pos) ), pos) in
		let extra = handle_fields ctx fields_to_check with_type_expr in
		meta, [make_meta ctx texpr extra], pos

	let check_strict_meta ctx metas =
		let pf = ctx.com.platform in
		match pf with
			| Cs | Java ->
				let ret = ref [] in
				List.iter (function
					| Meta.AssemblyStrict,params,pos -> (try
						ret := get_strict_meta ctx Meta.AssemblyMeta params pos :: !ret
					with | Exit -> ())
					| Meta.Strict,params,pos -> (try
						ret := get_strict_meta ctx Meta.Meta params pos :: !ret
					with | Exit -> ())
					| _ -> ()
				) metas;
				!ret
			| _ -> []
end

let field_of_static_definition d p =
	{
		cff_name = d.d_name;
		cff_doc = d.d_doc;
		cff_pos = p;
		cff_meta = d.d_meta;
		cff_access = (AStatic,null_pos) :: d.d_flags;
		cff_kind = d.d_data;
	}

(*
	Build module structure : should be atomic - no type loading is possible
*)
let module_pass_1 ctx m tdecls loadp =
	let com = ctx.com in
	let decls = ref [] in
	let statics = ref [] in
	let check_name name meta also_statics p =
		DeprecationCheck.check_is com name meta p;
		let error prev_pos =
			display_error ctx ("Name " ^ name ^ " is already defined in this module") p;
			typing_error (compl_msg "Previous declaration here") prev_pos;
		in
		List.iter (fun (t2,(_,p2)) ->
			if snd (t_path t2) = name then error (t_infos t2).mt_name_pos
		) !decls;
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
	let rec make_decl acc decl =
		let p = snd decl in
		let check_type_name type_name meta =
			let module_name = snd m.m_path in
			if type_name <> module_name && not (Meta.has Meta.Native meta) then Typecore.check_uppercase_identifier_name ctx type_name "type" p;
		in
		let acc = (match fst decl with
		| EImport _ | EUsing _ ->
			if !has_declaration then typing_error "import and using may not appear after a declaration" p;
			acc
		| EStatic d ->
			check_name (fst d.d_name) d.d_meta false (snd d.d_name);
			has_declaration := true;
			statics := (d,p) :: !statics;
			acc;
		| EClass d ->
			let name = fst d.d_name in
			has_declaration := true;
			let priv = List.mem HPrivate d.d_flags in
			let path = make_path name priv d.d_meta (snd d.d_name) in
			let c = mk_class m path p (pos d.d_name) in
			(* we shouldn't load any other type until we propertly set cl_build *)
			c.cl_build <- (fun() -> typing_error (s_type_path c.cl_path ^ " is not ready to be accessed, separate your type declarations in several files") p);
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
			if not (has_class_flag c CExtern) then check_type_name name d.d_meta;
			if has_class_flag c CAbstract then begin
				if has_class_flag c CInterface then display_error ctx "An interface may not be abstract" c.cl_name_pos;
				if has_class_flag c CFinal then display_error ctx "An abstract class may not be final" c.cl_name_pos;
			end;
			decls := (TClassDecl c, decl) :: !decls;
			acc
		| EEnum d ->
			let name = fst d.d_name in
			has_declaration := true;
			let priv = List.mem EPrivate d.d_flags in
			let path = make_path name priv d.d_meta p in
			if Meta.has (Meta.Custom ":fakeEnum") d.d_meta then typing_error "@:fakeEnum enums is no longer supported in Haxe 4, use extern enum abstract instead" p;
			let e = {
				e_path = path;
				e_module = m;
				e_pos = p;
				e_name_pos = (pos d.d_name);
				e_doc = d.d_doc;
				e_meta = d.d_meta;
				e_params = [];
				e_using = [];
				e_private = priv;
				e_extern = List.mem EExtern d.d_flags;
				e_constrs = PMap.empty;
				e_names = [];
				e_type = enum_module_type m path p;
			} in
			if not e.e_extern then check_type_name name d.d_meta;
			decls := (TEnumDecl e, decl) :: !decls;
			acc
		| ETypedef d ->
			let name = fst d.d_name in
			check_type_name name d.d_meta;
			has_declaration := true;
			let priv = List.mem EPrivate d.d_flags in
			let path = make_path name priv d.d_meta p in
			let t = {(mk_typedef m path p (pos d.d_name) (mk_mono())) with
				t_doc = d.d_doc;
				t_private = priv;
				t_meta = d.d_meta;
			} in
			(* failsafe in case the typedef is not initialized (see #3933) *)
			delay ctx PBuildModule (fun () ->
				match t.t_type with
				| TMono r -> (match r.tm_type with None -> Monomorph.bind r com.basic.tvoid | _ -> ())
				| _ -> ()
			);
			decls := (TTypeDecl t, decl) :: !decls;
			acc
		 | EAbstract d ->
		 	let name = fst d.d_name in
			check_type_name name d.d_meta;
			let priv = List.mem AbPrivate d.d_flags in
			let path = make_path name priv d.d_meta p in
			let a = {
				a_path = path;
				a_private = priv;
				a_module = m;
				a_pos = p;
				a_name_pos = pos d.d_name;
				a_doc = d.d_doc;
				a_params = [];
				a_using = [];
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
				a_enum = List.mem AbEnum d.d_flags || Meta.has Meta.Enum d.d_meta;
			} in
			if a.a_enum && not (Meta.has Meta.Enum a.a_meta) then a.a_meta <- (Meta.Enum,[],null_pos) :: a.a_meta;
			decls := (TAbstractDecl a, decl) :: !decls;
			match d.d_data with
			| [] when Meta.has Meta.CoreType a.a_meta ->
				a.a_this <- t_dynamic;
				acc
			| fields ->
				let a_t =
					let params = List.map (fun t -> TPType (CTPath (mk_type_path ([],fst t.tp_name)),null_pos)) d.d_params in
					CTPath (mk_type_path ~params ([],fst d.d_name)),null_pos
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
				let acc = make_decl acc (EClass { d_name = (fst d.d_name) ^ "_Impl_",snd d.d_name; d_flags = [HPrivate]; d_data = fields; d_doc = None; d_params = []; d_meta = !meta },p) in
				(match !decls with
				| (TClassDecl c,_) :: _ ->
					List.iter (fun m -> match m with
						| ((Meta.Using | Meta.Build | Meta.CoreApi | Meta.Allow | Meta.Access | Meta.Enum | Meta.Dce | Meta.Native | Meta.HlNative | Meta.JsRequire | Meta.PythonImport | Meta.Expose | Meta.Deprecated | Meta.PhpGlobal | Meta.PublicFields),_,_) ->
							c.cl_meta <- m :: c.cl_meta;
						| _ ->
							()
					) a.a_meta;
					a.a_impl <- Some c;
					c.cl_kind <- KAbstractImpl a;
					add_class_flag c CFinal;
				| _ -> die "" __LOC__);
				acc
		) in
		decl :: acc
	in
	let tdecls = List.fold_left make_decl [] tdecls in
	let tdecls =
		match !statics with
		| [] ->
			tdecls
		| statics ->
			let first_pos = ref null_pos in
			let fields = List.map (fun (d,p) ->
				first_pos := p;
				field_of_static_definition d p;
			) statics in
			let p = let p = !first_pos in { p with pmax = p.pmin } in
			let c = EClass {
				d_name = (snd m.m_path) ^ "_Fields_", null_pos;
				d_flags = [HPrivate];
				d_data = List.rev fields;
				d_doc = None;
				d_params = [];
				d_meta = []
			} in
			let tdecls = make_decl tdecls (c,p) in
			(match !decls with
			| (TClassDecl c,_) :: _ ->
				assert (m.m_statics = None);
				m.m_statics <- Some c;
				c.cl_kind <- KModuleFields m;
				add_class_flag c CFinal;
			| _ -> assert false);
			tdecls

	in
	let decls = List.rev !decls in
	decls, List.rev tdecls

let load_enum_field ctx e et is_flat index c =
	let p = c.ec_pos in
	let params = ref [] in
	params := type_type_params ctx TPHEnumConstructor ([],fst c.ec_name) (fun() -> !params) c.ec_pos c.ec_params;
	let params = !params in
	let ctx = { ctx with type_params = params @ ctx.type_params } in
	let rt = (match c.ec_type with
		| None -> et
		| Some (t,pt) ->
			let t = load_complex_type ctx true (t,pt) in
			(match follow t with
			| TEnum (te,_) when te == e ->
				()
			| _ ->
				typing_error "Explicit enum type must be of the same enum type" pt);
			t
	) in
	let t = (match c.ec_args with
		| [] -> rt
		| l ->
			is_flat := false;
			let pnames = ref PMap.empty in
			TFun (List.map (fun (s,opt,(t,tp)) ->
				(match t with CTPath({tpackage=[];tname="Void"}) -> typing_error "Arguments of type Void are not allowed in enum constructors" tp | _ -> ());
				if PMap.mem s (!pnames) then typing_error ("Duplicate argument `" ^ s ^ "` in enum constructor " ^ fst c.ec_name) p;
				pnames := PMap.add s () (!pnames);
				s, opt, load_type_hint ~opt ctx p (Some (t,tp))
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
	DeprecationCheck.check_is ctx.com f.ef_name f.ef_meta f.ef_name_pos;
	let cf = {
		(mk_field f.ef_name f.ef_type p f.ef_name_pos) with
		cf_kind = (match follow f.ef_type with
			| TFun _ -> Method MethNormal
			| _ -> Var { v_read = AccNormal; v_write = AccNo }
		);
		cf_doc = f.ef_doc;
		cf_params = f.ef_params;
	} in
	if ctx.is_display_file && DisplayPosition.display_position#enclosed_in f.ef_name_pos then
		DisplayEmitter.display_enum_field ctx e f p;
	f,cf

(*
	In this pass, we can access load and access other modules types, but we cannot follow them or access their structure
	since they have not been setup. We also build a context_init list that will be evaluated the first time we evaluate
	an expression into the context
*)
let init_module_type ctx context_init (decl,p) =
	let get_type name =
		try List.find (fun t -> snd (t_infos t).mt_path = name) ctx.m.curmod.m_types with Not_found -> die "" __LOC__
	in
	let check_path_display path p =
		if DisplayPosition.display_position#is_in_file (ctx.com.file_keys#get p.pfile) then DisplayPath.handle_path_display ctx path p
		in
	match decl with
	| EImport (path,mode) ->
		begin try
			check_path_display path p;
			ImportHandling.init_import ctx context_init path mode p;
			ImportHandling.commit_import ctx path mode p;
		with Error(err,p) ->
			display_error ctx (Error.error_msg err) p
		end
	| EUsing path ->
		check_path_display path p;
		ImportHandling.init_using ctx context_init path p
	| EClass d ->
		let c = (match get_type (fst d.d_name) with TClassDecl c -> c | _ -> die "" __LOC__) in
		if ctx.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx (match c.cl_kind with KAbstractImpl a -> TAbstractDecl a | _ -> TClassDecl c) (pos d.d_name);
		TypeloadCheck.check_global_metadata ctx c.cl_meta (fun m -> c.cl_meta <- m :: c.cl_meta) c.cl_module.m_path c.cl_path None;
		let herits = d.d_flags in
		List.iter (fun (m,_,p) ->
			if m = Meta.Final then begin
				add_class_flag c CFinal;
			end
		) d.d_meta;
		let prev_build_count = ref (!build_count - 1) in
		let build() =
			c.cl_build <- (fun()-> Building [c]);
			let fl = TypeloadCheck.Inheritance.set_heritance ctx c herits p in
			let rec build() =
				c.cl_build <- (fun()-> Building [c]);
				try
					List.iter (fun f -> f()) fl;
					TypeloadFields.init_class ctx c p context_init d.d_flags d.d_data;
					c.cl_build <- (fun()-> Built);
					incr build_count;
					List.iter (fun tp -> ignore(follow tp.ttp_type)) c.cl_params;
					Built;
				with TypeloadCheck.Build_canceled state ->
					c.cl_build <- make_pass ctx build;
					let rebuild() =
						delay_late ctx PBuildClass (fun() -> ignore(c.cl_build()));
					in
					(match state with
					| Built -> die "" __LOC__
					| Building cl ->
						if !build_count = !prev_build_count then typing_error ("Loop in class building prevent compiler termination (" ^ String.concat "," (List.map (fun c -> s_type_path c.cl_path) cl) ^ ")") c.cl_pos;
						prev_build_count := !build_count;
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
		ctx.pass <- PBuildClass;
		ctx.curclass <- c;
		c.cl_build <- make_pass ctx build;
		ctx.pass <- PBuildModule;
		ctx.curclass <- null_class;
		delay ctx PBuildClass (fun() -> ignore(c.cl_build()));
		if Meta.has Meta.InheritDoc c.cl_meta then
				delay ctx PConnectField (fun() -> InheritDoc.build_class_doc ctx c);
		if (ctx.com.platform = Java || ctx.com.platform = Cs) && not (has_class_flag c CExtern) then
			delay ctx PTypeField (fun () ->
				let metas = StrictMeta.check_strict_meta ctx c.cl_meta in
				if metas <> [] then c.cl_meta <- metas @ c.cl_meta;
				let rec run_field cf =
					let metas = StrictMeta.check_strict_meta ctx cf.cf_meta in
					if metas <> [] then cf.cf_meta <- metas @ cf.cf_meta;
					List.iter run_field cf.cf_overloads
				in
				List.iter run_field c.cl_ordered_statics;
				List.iter run_field c.cl_ordered_fields;
				match c.cl_constructor with
					| Some f -> run_field f
					| _ -> ()
			);
	| EEnum d ->
		let e = (match get_type (fst d.d_name) with TEnumDecl e -> e | _ -> die "" __LOC__) in
		if ctx.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx (TEnumDecl e) (pos d.d_name);
		let ctx = { ctx with type_params = e.e_params } in
		let h = (try Some (Hashtbl.find ctx.g.type_patches e.e_path) with Not_found -> None) in
		TypeloadCheck.check_global_metadata ctx e.e_meta (fun m -> e.e_meta <- m :: e.e_meta) e.e_module.m_path e.e_path None;
		(match h with
		| None -> ()
		| Some (h,hcl) ->
			Hashtbl.iter (fun _ _ -> typing_error "Field type patch not supported for enums" e.e_pos) h;
			e.e_meta <- e.e_meta @ hcl.tp_meta);
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
		TypeloadFields.build_module_def ctx (TEnumDecl e) e.e_meta get_constructs context_init (fun (e,p) ->
			match e with
			| EVars [{ ev_type = Some (CTAnonymous fields,p); ev_expr = None }] ->
				constructs := List.map (fun f ->
					let args, params, t = (match f.cff_kind with
					| FVar (t,None) -> [], [], t
					| FFun { f_params = pl; f_type = t; f_expr = (None|Some (EBlock [],_)); f_args = al } ->
						let al = List.map (fun ((n,_),o,_,t,_) -> match t with None -> typing_error "Missing function parameter type" f.cff_pos | Some t -> n,o,t) al in
						al, pl, t
					| _ ->
						typing_error "Invalid enum constructor in @:build result" p
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
			| _ -> typing_error "Enum build macro must return a single variable with anonymous object fields" p
		);
		let et = TEnum (e,extract_param_types e.e_params) in
		let names = ref [] in
		let index = ref 0 in
		let is_flat = ref true in
		let fields = ref PMap.empty in
		List.iter (fun c ->
			if PMap.mem (fst c.ec_name) e.e_constrs then typing_error ("Duplicate constructor " ^ fst c.ec_name) (pos c.ec_name);
			let f,cf = load_enum_field ctx e et is_flat index c in
			e.e_constrs <- PMap.add f.ef_name f e.e_constrs;
			fields := PMap.add cf.cf_name cf !fields;
			incr index;
			names := (fst c.ec_name) :: !names;
			if Meta.has Meta.InheritDoc f.ef_meta then
				delay ctx PConnectField (fun() -> InheritDoc.build_enum_field_doc ctx f);
		) (!constructs);
		e.e_names <- List.rev !names;
		e.e_extern <- e.e_extern;
		e.e_type.t_params <- e.e_params;
		e.e_type.t_type <- mk_anon ~fields:!fields (ref (EnumStatics e));
		if !is_flat then e.e_meta <- (Meta.FlatEnum,[],null_pos) :: e.e_meta;
		if Meta.has Meta.InheritDoc e.e_meta then
			delay ctx PConnectField (fun() -> InheritDoc.build_enum_doc ctx e);
		if (ctx.com.platform = Java || ctx.com.platform = Cs) && not e.e_extern then
			delay ctx PTypeField (fun () ->
				let metas = StrictMeta.check_strict_meta ctx e.e_meta in
				e.e_meta <- metas @ e.e_meta;
				PMap.iter (fun _ ef ->
					let metas = StrictMeta.check_strict_meta ctx ef.ef_meta in
					if metas <> [] then ef.ef_meta <- metas @ ef.ef_meta
				) e.e_constrs
			);
	| ETypedef d ->
		let t = (match get_type (fst d.d_name) with TTypeDecl t -> t | _ -> die "" __LOC__) in
		if ctx.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx (TTypeDecl t) (pos d.d_name);
		TypeloadCheck.check_global_metadata ctx t.t_meta (fun m -> t.t_meta <- m :: t.t_meta) t.t_module.m_path t.t_path None;
		let ctx = { ctx with type_params = t.t_params } in
		let tt = load_complex_type ctx true d.d_data in
		let tt = (match fst d.d_data with
		| CTExtend _ -> tt
		| CTPath { tpackage = ["haxe";"macro"]; tname = "MacroType" } ->
			(* we need to follow MacroType immediately since it might define other module types that we will load afterwards *)
			if t.t_type == follow tt then typing_error "Recursive typedef is not allowed" p;
			tt
		| _ ->
			if (Meta.has Meta.Eager d.d_meta) then
				follow tt
			else begin
				let rec check_rec tt =
					if tt == t.t_type then typing_error "Recursive typedef is not allowed" p;
					match tt with
					| TMono r ->
						(match r.tm_type with
						| None -> ()
						| Some t -> check_rec t)
					| TLazy f ->
						check_rec (lazy_type f);
					| TType (td,tl) ->
						if td == t then typing_error "Recursive typedef is not allowed" p;
						check_rec (apply_typedef td tl)
					| _ ->
						()
				in
				let r = exc_protect ctx (fun r ->
					r := lazy_processing (fun() -> tt);
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
			| Some _ -> die "" __LOC__);
		| _ -> die "" __LOC__);
		TypeloadFields.build_module_def ctx (TTypeDecl t) t.t_meta (fun _ -> []) context_init (fun _ -> ());
		if ctx.com.platform = Cs && t.t_meta <> [] then
			delay ctx PTypeField (fun () ->
				let metas = StrictMeta.check_strict_meta ctx t.t_meta in
				if metas <> [] then t.t_meta <- metas @ t.t_meta;
			);
	| EAbstract d ->
		let a = (match get_type (fst d.d_name) with TAbstractDecl a -> a | _ -> die "" __LOC__) in
		if ctx.is_display_file && DisplayPosition.display_position#enclosed_in (pos d.d_name) then
			DisplayEmitter.display_module_type ctx (TAbstractDecl a) (pos d.d_name);
		TypeloadCheck.check_global_metadata ctx a.a_meta (fun m -> a.a_meta <- m :: a.a_meta) a.a_module.m_path a.a_path None;
		let ctx = { ctx with type_params = a.a_params } in
		let is_type = ref false in
		let load_type t from =
			let _, pos = t in
			let t = load_complex_type ctx true t in
			let t = if not (Meta.has Meta.CoreType a.a_meta) then begin
				if !is_type then begin
					let r = exc_protect ctx (fun r ->
						r := lazy_processing (fun() -> t);
						(try (if from then Type.unify t a.a_this else Type.unify a.a_this t) with Unify_error _ -> typing_error "You can only declare from/to with compatible types" pos);
						t
					) "constraint" in
					TLazy r
				end else
					typing_error "Missing underlying type declaration or @:coreType declaration" p;
			end else begin
				if Meta.has Meta.Callable a.a_meta then
					typing_error "@:coreType abstracts cannot be @:callable" p;
				t
			end in
			t
		in
		List.iter (function
			| AbFrom t -> a.a_from <- (load_type t true) :: a.a_from
			| AbTo t -> a.a_to <- (load_type t false) :: a.a_to
			| AbOver t ->
				if a.a_impl = None then typing_error "Abstracts with underlying type must have an implementation" a.a_pos;
				if Meta.has Meta.CoreType a.a_meta then typing_error "@:coreType abstracts cannot have an underlying type" p;
				let at = load_complex_type ctx true t in
				delay ctx PForce (fun () ->
					let rec loop stack t =
						match follow t with
						| TAbstract(a,_) when not (Meta.has Meta.CoreType a.a_meta) ->
							if List.memq a stack then
								typing_error "Abstract underlying type cannot be recursive" a.a_pos
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
				typing_error "Abstract is missing underlying type declaration" a.a_pos
		end;
		if Meta.has Meta.InheritDoc a.a_meta then
			delay ctx PConnectField (fun() -> InheritDoc.build_abstract_doc ctx a);
	| EStatic _ ->
		(* nothing to do here as module fields are collected into a special EClass *)
		()

let module_pass_2 ctx m decls tdecls p =
	(* here is an additional PASS 1 phase, which define the type parameters for all module types.
		 Constraints are handled lazily (no other type is loaded) because they might be recursive anyway *)
	List.iter (fun d ->
		match d with
		| (TClassDecl c, (EClass d, p)) ->
			c.cl_params <- type_type_params ctx TPHType c.cl_path (fun() -> c.cl_params) p d.d_params;
			if Meta.has Meta.Generic c.cl_meta && c.cl_params <> [] then c.cl_kind <- KGeneric;
			if Meta.has Meta.GenericBuild c.cl_meta then begin
				if ctx.com.is_macro_context then typing_error "@:genericBuild cannot be used in macros" c.cl_pos;
				c.cl_kind <- KGenericBuild d.d_data;
			end;
			if c.cl_path = (["haxe";"macro"],"MacroType") then c.cl_kind <- KMacroType;
		| (TEnumDecl e, (EEnum d, p)) ->
			e.e_params <- type_type_params ctx TPHType e.e_path (fun() -> e.e_params) p d.d_params;
		| (TTypeDecl t, (ETypedef d, p)) ->
			t.t_params <- type_type_params ctx TPHType t.t_path (fun() -> t.t_params) p d.d_params;
		| (TAbstractDecl a, (EAbstract d, p)) ->
			a.a_params <- type_type_params ctx TPHType a.a_path (fun() -> a.a_params) p d.d_params;
		| _ ->
			die "" __LOC__
	) decls;
	(* setup module types *)
	let context_init = new TypeloadFields.context_init in
	List.iter (init_module_type ctx context_init) tdecls;
	(* Make sure that we actually init the context at some point (issue #9012) *)
	delay ctx PConnectField (fun () -> context_init#run)

(*
	Creates a module context for [m] and types [tdecls] using it.
*)
let type_types_into_module ?(check=true) ctx m tdecls p =
	let decls, tdecls = module_pass_1 ctx m tdecls p in
	let types = List.map fst decls in
	if check then List.iter (TypeloadCheck.check_module_types ctx m p) types;
	m.m_types <- m.m_types @ types;
	(* define the per-module context for the next pass *)
	let ctx = {
		com = ctx.com;
		g = ctx.g;
		t = ctx.t;
		m = {
			curmod = m;
			module_imports = List.map (fun t -> t,null_pos) ctx.g.std.m_types;
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			import_statements = [];
		};
		is_display_file = (ctx.com.display.dms_kind <> DMNone && DisplayPosition.display_position#is_in_file (Path.UniqueKey.lazy_key m.m_extra.m_file));
		bypass_accessor = 0;
		meta = [];
		this_stack = [];
		with_type_stack = [];
		call_argument_stack = [];
		pass = PBuildModule;
		get_build_infos = (fun() -> None);
		on_error = (fun ctx msg p -> ctx.com.error msg p);
		macro_depth = ctx.macro_depth;
		curclass = null_class;
		curfield = null_field;
		tthis = ctx.tthis;
		ret = ctx.ret;
		locals = PMap.empty;
		type_params = [];
		curfun = FunStatic;
		untyped = false;
		in_display = false;
		in_function = false;
		in_loop = false;
		opened = [];
		in_call_args = false;
		in_overload_call_args = false;
		delayed_display = None;
		monomorphs = {
			perfunction = [];
		};
		vthis = None;
		memory_marker = Typecore.memory_marker;
	} in
	if ctx.g.std != null_module then begin
		add_dependency m ctx.g.std;
		(* this will ensure both String and (indirectly) Array which are basic types which might be referenced *)
		ignore(load_core_type ctx "String");
	end;
	module_pass_2 ctx m decls tdecls p;
	ctx

let handle_import_hx ctx m decls p =
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
		Hashtbl.replace ctx.com.parser_cache path r;
		(* We use the file path as module name to make it unique. This may or may not be a good idea... *)
		let m_import = make_module ctx ([],path) path p in
		m_import.m_extra.m_kind <- MImport;
		add_module ctx m_import p;
		m_import
	in
	List.fold_left (fun acc path ->
		let decls = try
			let r = Hashtbl.find ctx.com.parser_cache path in
			let mimport = Hashtbl.find ctx.g.modules ([],path) in
			if mimport.m_extra.m_kind <> MFake then add_dependency m mimport;
			r
		with Not_found ->
			if Sys.file_exists path then begin
				let _,r = match !TypeloadParse.parse_hook ctx.com path p with
					| ParseSuccess(data,_,_) -> data
					| ParseError(_,(msg,p),_) -> Parser.error msg p
				in
				List.iter (fun (d,p) -> match d with EImport _ | EUsing _ -> () | _ -> typing_error "Only import and using is allowed in import.hx files" p) r;
				add_dependency m (make_import_module path r);
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

(*
	Creates a new module and types [tdecls] into it.
*)
let type_module ctx mpath file ?(dont_check_path=false) ?(is_extern=false) tdecls p =
	let m = make_module ctx mpath file p in
	Hashtbl.add ctx.g.modules m.m_path m;
	let tdecls = handle_import_hx ctx m tdecls p in
	let ctx = type_types_into_module ctx m tdecls p in
	if is_extern then m.m_extra.m_kind <- MExtern else if not dont_check_path then Typecore.check_module_path ctx m.m_path p;
	m

(* let type_module ctx mpath file ?(is_extern=false) tdecls p =
	let timer = Timer.timer ["typing";"type_module"] in
	Std.finally timer (type_module ctx mpath file ~is_extern tdecls) p *)

let type_module_hook = ref (fun _ _ _ -> None)

let load_module ctx m p =
	let m2 = (try
		Hashtbl.find ctx.g.modules m
	with
		Not_found ->
			match !type_module_hook ctx m p with
			| Some m -> m
			| None ->
			let is_extern = ref false in
			let file, decls = (try
				TypeloadParse.parse_module ctx m p
			with Not_found ->
				let rec loop = function
					| [] ->
						raise (Error (Module_not_found m,p))
					| (file,load) :: l ->
						match load m p with
						| None -> loop l
						| Some (_,a) -> file, a
				in
				is_extern := true;
				loop ctx.com.load_extern_type
			) in
			let is_extern = !is_extern in
			try
				type_module ctx m file ~is_extern decls p
			with Forbid_package (inf,pl,pf) when p <> null_pos ->
				raise (Forbid_package (inf,p::pl,pf))
	) in
	add_dependency ctx.m.curmod m2;
	if ctx.pass = PTypeField then flush_pass ctx PConnectField "load_module";
	m2

(* let load_module ctx m p =
	let timer = Timer.timer ["typing";"load_module"] in
	Std.finally timer (load_module ctx m) p *)

;;
