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

(* Type instance and type parameter handling. *)

open Ast
open Common
open DisplayTypes.DisplayMode
open DisplayTypes.CompletionResultKind
open CompletionItem
open CompletionModuleType
open CompletionModuleKind
open ClassFieldOrigin
open DisplayException
open Type
open Typecore
open Error
open Globals

let type_function_params_ref = ref (fun _ _ _ _ _ -> die "" __LOC__)

let check_field_access ctx cff =
	let display_access = ref None in
	let rec loop p0 acc l =
		let check_display p1 =
			let pmid = {p0 with pmin = p0.pmax; pmax = p1.pmin} in
			if DisplayPosition.display_position#enclosed_in pmid then match acc with
			| access :: _ -> display_access := Some access;
			| [] -> ()
		in
		match l with
		| [] ->
			(* This is a bit dodgy. Ideally we would use the position of the `function` keyword, but we don't have that...
			   Using the name means this is going to complete within the `function` keyword too. Not sure what we
			   can do about it. *)
			check_display (pos (cff.cff_name))
		| (access,p1) :: l ->
			check_display p1;
			try
				let _,p2 = List.find (fun (access',_) -> access = access') acc in
				if p1 <> null_pos && p2 <> null_pos then begin
					display_error_ext ctx.com (make_error (Custom (Printf.sprintf "Duplicate access modifier %s" (Ast.s_access access))) ~sub:([
						(make_error ~depth:1 (Custom (compl_msg "Previously defined here")) p2);
					]) p1);
				end;
				loop p1 acc l
			with Not_found -> match access with
				| APublic | APrivate ->
					begin try
						let _,p2 = List.find (fun (access',_) -> match access' with APublic | APrivate -> true | _ -> false) acc in
						display_error_ext ctx.com (make_error (Custom (Printf.sprintf "Conflicting access modifier %s" (Ast.s_access access))) ~sub:([
							(make_error ~depth:1 (Custom (compl_msg "Conflicts with this")) p2);
						]) p1);
						loop p1 acc l
					with Not_found ->
						loop p1 ((access,p1) :: acc) l
					end
				| _ ->
					loop p1 ((access,p1) :: acc) l
	in
	let pmin = {cff.cff_pos with pmax = cff.cff_pos.pmin} in
	loop pmin [] cff.cff_access;
	!display_access

let find_type_in_module m tname =
	List.find (fun mt ->
		let infos = t_infos mt in
		not infos.mt_private && snd infos.mt_path = tname
	) m.m_types

(* raises Type_not_found *)
let find_type_in_module_raise ctx m tname p =
	try
		List.find (fun mt ->
			let infos = t_infos mt in
			if snd infos.mt_path = tname then
				if ctx.m.curmod != infos.mt_module && infos.mt_private then
					raise_typing_error_ext (make_error (Type_not_found (m.m_path,tname,Private_type)) p)
				else
					true
			else
				false
		) m.m_types
	with Not_found ->
		raise_typing_error_ext (make_error (Type_not_found (m.m_path,tname,Not_defined)) p)

(** since load_type_def and load_instance are used in PASS2, they should not access the structure of a type **)

let find_type_in_current_module_context ctx pack name =
	if pack = [] then begin
		try
			(* Check the types in our own module *)
			List.find (fun mt -> t_name mt = name) ctx.m.curmod.m_types
		with Not_found ->
			let t,pi = ctx.m.import_resolution#find_type_import name in
			ImportHandling.mark_import_position ctx pi;
			t
	end else begin
		(* All this is very weird *)
		try
			List.find (fun mt -> t_path mt = (pack,name)) ctx.m.curmod.m_types
		with Not_found ->
			(* see also https://github.com/HaxeFoundation/haxe/issues/9150 *)
			let t,pi = ctx.m.import_resolution#find_type_import_weirdly pack name in
			ImportHandling.mark_import_position ctx pi;
		t
	end

let find_in_wildcard_imports ctx mname p f =
	let rec loop l =
		match l with
		| [] ->
			raise Not_found
		| (pack,ppack) :: l ->
			begin
			try
				let path = (pack,mname) in
				let m =
					try
						ctx.g.do_load_module ctx path p
					with Error { err_message = Module_not_found mpath } when mpath = path ->
						raise Not_found
				in
				let r = f m ~resume:true in
				ImportHandling.mark_import_position ctx ppack;
				r
			with Not_found ->
				loop l
			end
	in
	loop (ctx.m.import_resolution#extract_wildcard_packages)

(* TODO: move these generic find functions into a separate module *)
let find_in_modules_starting_from_current_package ~resume ctx mname p f =
	let rec loop l =
		let path = (List.rev l,mname) in
		match l with
		| [] ->
			let m =
				try
					ctx.g.do_load_module ctx path p
				with Error { err_message = Module_not_found mpath } when resume && mpath = path ->
					raise Not_found
			in
			f m ~resume:resume
		| _ :: sl ->
			try
				let m =
					try
						ctx.g.do_load_module ctx path p
					with Error { err_message = Module_not_found mpath } when mpath = path ->
						raise Not_found
					in
				f m ~resume:true;
			with Not_found ->
				loop sl
	in
	let pack = fst ctx.m.curmod.m_path in
	loop (List.rev pack)

let find_in_unqualified_modules ctx name p f ~resume =
	try
		find_in_wildcard_imports ctx name p f
	with Not_found ->
		find_in_modules_starting_from_current_package ctx name p f ~resume:resume

let load_unqualified_type_def ctx mname tname p =
	let find_type m ~resume =
		if resume then
			find_type_in_module m tname
		else
			find_type_in_module_raise ctx m tname p
	in
	find_in_unqualified_modules ctx mname p find_type ~resume:false

let load_module ctx path p =
	try
		ctx.g.do_load_module ctx path p
	with Error { err_message = Module_not_found mpath } as exc when mpath = path ->
		match path with
		| ("std" :: pack, name) ->
			ctx.g.do_load_module ctx (pack,name) p
		| _ ->
			raise exc

let load_qualified_type_def ctx pack mname tname p =
	let m = load_module ctx (pack,mname) p in
	find_type_in_module_raise ctx m tname p

let load_type_def' ctx pack mname tname p =
	if pack = [] then
		load_unqualified_type_def ctx mname tname p
	else
		load_qualified_type_def ctx pack mname tname p

(*
	load a type or a subtype definition
*)
let load_type_def ctx p t =
	if t = Parser.magic_type_path then
		raise_fields (DisplayToplevel.collect ctx TKType NoValue true) CRTypeHint (DisplayTypes.make_subject None p);
	(* The type name is the module name or the module sub-type name *)
	let tname = match t.tsub with None -> t.tname | Some n -> n in

	try
		(* If there's a sub-type, there's no reason to look in our module or its imports *)
		if t.tsub <> None then raise Not_found;
		find_type_in_current_module_context ctx t.tpackage tname
	with Not_found ->
		load_type_def' ctx t.tpackage t.tname tname p

(* let load_type_def ctx p t =
	let timer = Timer.timer ["typing";"load_type_def"] in
	Std.finally timer (load_type_def ctx p) t *)

let generate_args_meta com cls_opt add_meta args =
	let values = List.fold_left (fun acc ((name,p),_,_,_,eo) -> match eo with Some e -> ((name,p,NoQuotes),e) :: acc | _ -> acc) [] args in
	(match values with
		| [] -> ()
		| _ -> add_meta (Meta.Value,[EObjectDecl values,null_pos],null_pos)
	);
	if List.exists (fun (_,_,m,_,_) -> m <> []) args then
		let fn = { f_params = []; f_args = args; f_type = None; f_expr = None } in
		add_meta (Meta.HaxeArguments,[EFunction(FKAnonymous,fn),null_pos],null_pos)

let is_redefined ctx cf1 fields p =
	try
		let cf2 = PMap.find cf1.cf_name fields in
		let st = s_type (print_context()) in
		if not (type_iseq cf1.cf_type cf2.cf_type) then begin
			raise_typing_error_ext (make_error (Custom ("Cannot redefine field " ^ cf1.cf_name ^ " with different type")) ~sub:([
				(make_error ~depth:1 (Custom (compl_msg ("Second type was " ^ (st cf2.cf_type)))) cf2.cf_pos);
				(make_error ~depth:1 (Custom (compl_msg ("First type was " ^ (st cf1.cf_type)))) cf1.cf_pos);
			]) p)
		end else
			true
	with Not_found ->
		false

let make_extension_type ctx tl =
	let mk_extension fields (t,p) = match follow t with
		| TAnon a ->
			PMap.fold (fun cf fields ->
				if not (is_redefined ctx cf fields p) then PMap.add cf.cf_name cf fields
				else fields
			) a.a_fields fields
		| _ ->
			raise_typing_error "Can only extend structures" p
	in
	let fields = List.fold_left mk_extension PMap.empty tl in
	let tl = List.map (fun (t,_) -> t) tl in
	let ta = mk_anon ~fields (ref (Extend tl)) in
	ta

let check_param_constraints ctx t map ttp p =
	List.iter (fun ti ->
		let ti = map ti in
		try
			unify_raise t ti p
		with Error ({ err_message = Unify l } as err) ->
			let fail() =
				if not ctx.f.untyped then display_error_ext ctx.com { err with err_message = (Unify (Constraint_failure (s_type_path ttp.ttp_class.cl_path) :: l)) }
			in
			match follow t with
			| TInst({cl_kind = KExpr e},_) ->
				let ctx = TyperManager.clone_for_type_parameter_expression ctx in
				let e = type_expr ctx e (WithType.with_type ti) in
				begin try unify_raise e.etype ti p
				with Error { err_message = Unify _ } -> fail() end
			| _ ->
				fail()

	) (get_constraints ttp)

type load_instance_param_mode =
	| ParamNormal
	| ParamSpawnMonos
	| ParamCustom of (build_info -> Type.t list option -> Type.t list)

type load_instance_mode =
	| LoadNormal
	| LoadReturn
	| LoadAny (* We don't necessarily know why we're loading, so let's just load anything *)

let rec maybe_build_instance ctx t0 get_params p =
	let rec loop t = match t with
		| TInst({cl_kind = KGeneric} as c,tl) ->
			let info = ctx.g.get_build_info ctx (TClassDecl c) p in
			let tl = match get_params with
				| ParamNormal | ParamSpawnMonos ->
					tl
				| ParamCustom f ->
					f info (Some tl)
			in
			maybe_build_instance ctx (info.build_apply tl) get_params p
		| TType(td,tl) ->
			loop (apply_typedef td tl)
		| TMono {tm_type = Some t} ->
			loop t
		| _ ->
			t0
	in
	loop t0

let rec load_params ctx info params p =
	let is_rest = info.build_kind = BuildGenericBuild && (match info.build_params with [{ttp_name="Rest"}] -> true | _ -> false) in
	let is_java_rest = ctx.com.platform = Jvm && info.build_extern in
	let is_rest = is_rest || is_java_rest in
	let load_param t =
		match t with
		| TPExpr e ->
			let name = (match fst e with
				| EConst (String(s,_)) -> "S" ^ s
				| EConst (Int (_,_) as c) -> "I" ^ s_constant c
				| EConst (Float (_,_) as c) -> "F" ^ s_constant c
				| EDisplay _ ->
					ignore(type_expr ctx e WithType.value);
					"Expr"
				| _ -> "Expr"
			) in
			let c = mk_class ctx.m.curmod ([],name) p (pos e) in
			c.cl_kind <- KExpr e;
			TInst (c,[]),pos e
		| TPType t ->
			load_complex_type ctx true LoadNormal t,pos t
	in
	let checks = DynArray.create () in
	let rec loop tl1 tl2 is_rest = match tl1,tl2 with
		| t :: tl1,ttp:: tl2 ->
			let name = ttp.ttp_name in
			let t,pt = load_param t in
			let check_const c =
				let is_expression = (match t with TInst ({ cl_kind = KExpr _ },_) -> true | _ -> false) in
				let expects_expression = name = "Const" || Meta.has Meta.Const c.cl_meta in
				let accepts_expression = name = "Rest" in
				if is_expression then begin
					if not expects_expression && not accepts_expression then
						raise_typing_error "Constant value unexpected here" p
				end else if expects_expression then
					raise_typing_error "Type parameter is expected to be a constant value" p
			in
			let is_rest = is_rest || name = "Rest" && info.build_kind = BuildGenericBuild in
			let t = match ttp.ttp_constraints with
				| None when (match info.build_kind with BuildGeneric _ -> false | _ -> true) ->
					check_const ttp.ttp_class;
					t
				| _ ->
					check_const ttp.ttp_class;
					DynArray.add checks (t,ttp,pt);
					t
			in
			t :: loop tl1 tl2 is_rest
		| [],[] ->
			[]
		| [],[{ttp_name="Rest"}] when info.build_kind = BuildGenericBuild ->
			[]
		| [],({ttp_type=t;ttp_default=def}) :: tl ->
			if is_java_rest then
				t_dynamic :: loop [] tl is_rest
			else begin match def with
				| None ->
					if ignore_error ctx.com then
						t :: loop [] tl is_rest
					else
						raise_typing_error ("Not enough type parameters for " ^ s_type_path info.build_path) p
				| Some t ->
					t :: loop [] tl is_rest
			end
		| t :: tl,[] ->
			let t,pt = load_param t in
			if is_rest then
				t :: loop tl [] true
			else if ignore_error ctx.com then
				[]
			else
				raise_typing_error ("Too many type parameters for " ^ s_type_path info.build_path) pt
	in
	let params = loop params info.build_params false in
	if not is_rest then begin
		let map t =
			let t = apply_params info.build_params params t in
			maybe_build_instance ctx t ParamNormal p;
		in
		delay ctx.g PCheckConstraint (fun () ->
			DynArray.iter (fun (t,c,p) ->
				check_param_constraints ctx t map c p
			) checks
		);
	end;
	params

(* build an instance from a full type *)
and load_instance' ctx ptp get_params mode =
	let t = ptp.path in
	try
		if t.tpackage <> [] || t.tsub <> None then raise Not_found;
		let pt = lookup_param t.tname ctx.type_params in
		if t.tparams <> [] then raise_typing_error ("Class type parameter " ^ t.tname ^ " can't have parameters") ptp.pos_full;
		pt
	with Not_found ->
		let mt = load_type_def ctx (if ptp.pos_path == null_pos then ptp.pos_full else ptp.pos_path) t in
		let info = ctx.g.get_build_info ctx mt ptp.pos_full in
		if info.build_path = ([],"Dynamic") then match t.tparams with
			| [] -> t_dynamic
			| [TPType t] -> TDynamic (Some (load_complex_type ctx true LoadNormal t))
			| _ -> raise_typing_error "Too many parameters for Dynamic" ptp.pos_full
		else if info.build_params = [] then begin match t.tparams with
			| [] ->
				info.build_apply []
			|  tp :: _ ->
				let pt = match tp with
					| TPType(_,p) | TPExpr(_,p) -> p
				in
				display_error ctx.com ("Too many type parameters for " ^ s_type_path info.build_path) pt;
				info.build_apply []
		end else begin
			(* TODO: this is currently duplicated, but it seems suspcious anyway... *)
			let is_rest = info.build_kind = BuildGenericBuild && (match info.build_params with [{ttp_name="Rest"}] -> true | _ -> false) in
			let tl = if t.tparams = [] && not is_rest then begin match get_params with
				| ParamNormal ->
					load_params ctx info t.tparams ptp.pos_full
				| ParamSpawnMonos ->
					Monomorph.spawn_constrained_monos (fun t -> t) info.build_params
				| ParamCustom f ->
					f info None
			end else
				load_params ctx info t.tparams ptp.pos_full
			in
			let t = info.build_apply tl in
			maybe_build_instance ctx t get_params ptp.pos_full
		end

and load_instance ctx ?(allow_display=false) ptp get_params mode =
	try
		let t = load_instance' ctx ptp get_params mode in
		if allow_display then DisplayEmitter.check_display_type ctx t ptp;
		t
	with Error { err_message = Module_not_found path } when ctx.e.macro_depth <= 0 && (ctx.com.display.dms_kind = DMDefault) && DisplayPosition.display_position#enclosed_in ptp.pos_path ->
		let s = s_type_path path in
		DisplayToplevel.collect_and_raise ctx TKType NoValue CRTypeHint (s,ptp.pos_full) ptp.pos_path

(*
	build an instance from a complex type
*)
and load_complex_type' ctx allow_display mode (t,p) =
	match t with
	| CTParent t -> load_complex_type ctx allow_display mode t
	| CTPath { path = {tpackage = ["$"]; tname = "_hx_mono" }} -> spawn_monomorph ctx.e p
	| CTPath ptp -> load_instance ~allow_display ctx ptp ParamNormal mode
	| CTOptional _ -> raise_typing_error "Optional type not allowed here" p
	| CTNamed _ -> raise_typing_error "Named type not allowed here" p
	| CTIntersection tl ->
		let tl = List.map (fun (t,pn) ->
			try
				(load_complex_type ctx allow_display LoadNormal (t,pn),pn)
			with DisplayException(DisplayFields ({fkind = CRTypeHint} as r)) ->
				let l = List.filter (fun item -> match item.ci_kind with
					| ITType({kind = Struct},_) -> true
					| _ -> false
				) r.fitems in
				raise_fields l (CRStructExtension true) r.fsubject
		) tl in
		let tr = Monomorph.create() in
		let t = TMono tr in
		let r = make_lazy ctx.g t (fun r ->
			let ta = make_extension_type ctx tl in
			Monomorph.bind tr ta;
			ta
		) "constraint" in
		TLazy r
	| CTExtend (tl,l) ->
		begin match load_complex_type ctx allow_display LoadNormal (CTAnonymous l,p) with
		| TAnon a as ta ->
			let mk_extension (t,p) =
				match follow t with
				| TInst ({cl_kind = KTypeParameter _},_) ->
					raise_typing_error "Cannot structurally extend type parameters" p
				| TMono _ ->
					raise_typing_error "Loop found in cascading signatures definitions. Please change order/import" p
				| TAnon a2 ->
					PMap.iter (fun _ cf -> ignore(is_redefined ctx cf a2.a_fields p)) a.a_fields;
					mk_anon ~fields:(PMap.foldi PMap.add a.a_fields a2.a_fields) (ref (Extend [t]))
				| _ -> raise_typing_error "Can only extend structures" p
			in
			let loop (t,p) = match follow t with
				| TAnon a2 ->
					PMap.iter (fun f cf ->
						if not (is_redefined ctx cf a.a_fields p) then
							a.a_fields <- PMap.add f cf a.a_fields
					) a2.a_fields
				| _ ->
					raise_typing_error "Can only extend structures" p
			in
			let il = List.map (fun ptp ->
				try
					(load_instance ctx ~allow_display ptp ParamNormal LoadNormal,ptp.pos_full)
				with DisplayException(DisplayFields ({fkind = CRTypeHint} as r)) ->
					let l = List.filter (fun item -> match item.ci_kind with
						| ITType({kind = Struct},_) -> true
						| _ -> false
					) r.fitems in
					raise_fields l (CRStructExtension false) r.fsubject
			) tl in
			let tr = Monomorph.create() in
			let t = TMono tr in
			let r = make_lazy ctx.g t (fun r ->
				Monomorph.bind tr (match il with
					| [i] ->
						mk_extension i
					| _ ->
						List.iter loop il;
						a.a_status := Extend (List.map (fun(t,_) -> t) il);
						ta);
				t
			) "constraint" in
			TLazy r
		| _ -> die "" __LOC__
		end
	| CTAnonymous l ->
		let displayed_field = ref None in
		let loop acc f =
			let n = fst f.cff_name in
			let pf = snd f.cff_name in
			let p = f.cff_pos in
			if PMap.mem n acc then raise_typing_error ("Duplicate field declaration : " ^ n) pf;
			let topt mode = function
				| None -> raise_typing_error ("Explicit type required for field " ^ n) p
				| Some t -> load_complex_type ctx allow_display mode t
			in
			if n = "new" then warning ctx WDeprecated "Structures with new are deprecated, use haxe.Constraints.Constructible instead" p;
			let no_expr = function
				| None -> ()
				| Some (_,p) -> raise_typing_error "Expression not allowed here" p
			in
			let pub = ref true in
			let dyn = ref false in
			let params = ref [] in
			let final = ref false in
			ignore(check_field_access ctx f); (* TODO: do we want to do anything with this? *)
			List.iter (fun a ->
				match fst a with
				| APublic -> ()
				| APrivate ->
					let p = pos a in
					if Filename.basename p.pfile <> "NativeIterable.hx" then (* Terrible workaround for #7436 *)
						warning ctx WDeprecated "private structure fields are deprecated" p;
					pub := false;
				| ADynamic when (match f.cff_kind with FFun _ -> true | _ -> false) -> dyn := true
				| AFinal -> final := true
				| AStatic | AOverride | AInline | ADynamic | AMacro | AExtern | AAbstract | AOverload | AEnum as a -> raise_typing_error ("Invalid access " ^ Ast.s_access a) p
			) f.cff_access;
			let t , access = (match f.cff_kind with
				| FVar(t,e) when !final ->
					no_expr e;
					let t = (match t with None -> raise_typing_error "Type required for structure property" p | Some t -> t) in
					load_complex_type ctx allow_display LoadNormal t, Var { v_read = AccNormal; v_write = AccNever }
				| FVar (Some (CTPath({path = {tpackage=[];tname="Void"}}),_), _)  | FProp (_,_,Some (CTPath({path = {tpackage=[];tname="Void"}}),_),_) ->
					raise_typing_error "Fields of type Void are not allowed in structures" p
				| FVar (t, e) ->
					no_expr e;
					topt LoadNormal t, Var { v_read = AccNormal; v_write = AccNormal }
				| FFun fd ->
					params := (!type_function_params_ref) ctx fd TPHAnonField (fst f.cff_name) p;
					no_expr fd.f_expr;
					let old = ctx.type_params in
					ctx.type_params <- !params @ old;
					let args = List.map (fun ((name,_),o,_,t,e) -> no_expr e; name, o, topt LoadNormal t) fd.f_args in
					let t = TFun (args,topt LoadReturn fd.f_type), Method (if !dyn then MethDynamic else MethNormal) in
					ctx.type_params <- old;
					t
				| FProp (i1,i2,t,e) ->
					no_expr e;
					let access (m,_) get =
						match m with
						| "null" -> AccNo
						| "never" -> AccNever
						| "default" -> AccNormal
						| "dynamic" -> AccCall
						| "get" when get -> AccCall
						| "set" when not get -> AccCall
						| x when get && x = "get_" ^ n -> AccCall
						| x when not get && x = "set_" ^ n -> AccCall
						| _ ->
							raise_typing_error "Custom property access is no longer supported in Haxe 3" f.cff_pos;
					in
					let t = (match t with None -> raise_typing_error "Type required for structure property" p | Some t -> t) in
					load_complex_type ctx allow_display LoadNormal t, Var { v_read = access i1 true; v_write = access i2 false }
			) in
			let t = if Meta.has Meta.Optional f.cff_meta then ctx.t.tnull t else t in
			let cf = {
				(mk_field n ~public:!pub t p (pos f.cff_name)) with
				cf_kind = access;
				cf_params = !params;
				cf_doc = f.cff_doc;
				cf_meta = f.cff_meta;
			} in
			if !final then add_class_field_flag cf CfFinal;
			init_meta_overloads ctx None cf;
			if ctx.m.is_display_file then begin
				DisplayEmitter.check_display_metadata ctx cf.cf_meta;
				if DisplayPosition.display_position#enclosed_in cf.cf_name_pos then displayed_field := Some cf;
			end;
			PMap.add n cf acc
		in
		let a = { a_fields = (List.fold_left loop PMap.empty l); a_status = ref Closed; } in
		begin match !displayed_field with
		| None ->
			()
		| Some cf ->
			delay ctx.g PBuildClass (fun () -> DisplayEmitter.display_field ctx (AnonymousStructure a) CFSMember cf cf.cf_name_pos);
		end;
		TAnon a
	| CTFunction (args,r) ->
		match args with
		| [CTPath { path = {tpackage = []; tparams = []; tname = "Void" }},_] ->
			TFun ([],load_complex_type ctx allow_display  LoadReturn r)
		| _ ->
			TFun (List.map (fun t ->
				let t, opt = (match fst t with CTOptional t | CTParent((CTOptional t,_)) -> t, true | _ -> t,false) in
				let n,t = (match fst t with CTNamed (n,t) -> (fst n), t | _ -> "", t) in
				n,opt,load_complex_type ctx allow_display LoadNormal t
			) args,load_complex_type ctx allow_display LoadReturn r)

and load_complex_type ctx allow_display mode (t,pn) =
	try
		load_complex_type' ctx allow_display mode (t,pn)
	with Error ({ err_message = Module_not_found(([],name)) } as err) ->
		if Diagnostics.error_in_diagnostics_run ctx.com err.err_pos then begin
			delay ctx.g PForce (fun () -> DisplayToplevel.handle_unresolved_identifier ctx name err.err_pos true);
			t_dynamic
		end else
			raise (Error err)

and init_meta_overloads ctx co cf =
	let overloads = ref [] in
	let filter_meta m = match m with
		| ((Meta.Overload | Meta.Value),_,_) -> false
		| _ -> true
	in
	let cf_meta = List.filter filter_meta cf.cf_meta in
	cf.cf_meta <- List.filter (fun m ->
		match m with
		| (Meta.Overload,[(EFunction (kind,f),p)],_)  ->
			(match kind with FKNamed _ -> raise_typing_error "Function name must not be part of @:overload" p | _ -> ());
			(match f.f_expr with Some (EBlock [], _) -> () | _ -> raise_typing_error "Overload must only declare an empty method body {}" p);
			(match cf.cf_kind with
				| Method MethInline -> raise_typing_error "Cannot @:overload inline function" p
				| _ -> ());
			let old = ctx.type_params in
			begin match cf.cf_params with
				| [] ->
					()
				| l ->
					ctx.type_params <- List.filter (fun ttp ->
						ttp.ttp_host <> TPHMethod
					) ctx.type_params
			end;
			let params : type_params = (!type_function_params_ref) ctx f TPHMethod cf.cf_name p in
			ctx.type_params <- params @ ctx.type_params;
			let topt mode = function None -> raise_typing_error "Explicit type required" p | Some t -> load_complex_type ctx true mode t in
			let args =
				List.map
					(fun ((a,_),opt,_,t,cto) ->
						let t = if opt then ctx.t.tnull (topt LoadNormal t) else topt LoadNormal t in
						let opt = opt || cto <> None in
						a,opt,t
					)
					f.f_args
			in
			let cf = { cf with cf_type = TFun (args,topt LoadReturn f.f_type); cf_params = params; cf_meta = cf_meta} in
			generate_args_meta ctx.com co (fun meta -> cf.cf_meta <- meta :: cf.cf_meta) f.f_args;
			overloads := cf :: !overloads;
			ctx.type_params <- old;
			false
		| (Meta.Overload,[],_) when ctx.com.config.pf_overload ->
			add_class_field_flag cf CfOverload;
			let topt (n,_,t) = match t with | TMono t when t.tm_type = None -> raise_typing_error ("Explicit type required for overload functions\n... For function argument '" ^ n ^ "'") cf.cf_pos | _ -> () in
			(match follow cf.cf_type with
			| TFun (args,_) -> List.iter topt args
			| _ -> () (* could be a variable *));
			true
		| (Meta.Overload,[],p) ->
			raise_typing_error "This platform does not support this kind of overload declaration. Try @:overload(function()... {}) instead" p
		| (Meta.Overload,_,p) ->
			raise_typing_error "Invalid @:overload metadata format" p
		| _ ->
			true
	) cf.cf_meta;
	cf.cf_overloads <- (List.rev !overloads)

let t_iterator ctx p =
	match load_qualified_type_def ctx [] "StdTypes" "Iterator" p with
	| TTypeDecl t ->
		add_dependency ctx.m.curmod t.t_module MDepFromTyping;
		let pt = spawn_monomorph ctx.e p in
		apply_typedef t [pt], pt
	| _ ->
		die "" __LOC__

(*
	load either a type t or Null<Unknown> if not defined
*)
let load_type_hint ?(opt=false) ctx pcur mode t =
	let t = match t with
		| None -> spawn_monomorph ctx.e pcur
		| Some (t,p) ->	load_complex_type ctx true mode (t,p)
	in
	if opt then ctx.t.tnull t else t

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let rec type_type_param ctx host path p tp =
	let n = fst tp.tp_name in
	let c = mk_class ctx.m.curmod (fst path @ [snd path],n) (pos tp.tp_name) (pos tp.tp_name) in
	c.cl_params <- type_type_params ctx host c.cl_path p tp.tp_params;
	c.cl_meta <- tp.Ast.tp_meta;
	let ttp = mk_type_param c host None None in
	if ctx.m.is_display_file && DisplayPosition.display_position#enclosed_in (pos tp.tp_name) then
		DisplayEmitter.display_type ctx ttp.ttp_type (pos tp.tp_name);
	ttp

and type_type_params ctx host path p tpl =
	let names = ref [] in
	let param_pairs = List.map (fun tp ->
		if List.exists (fun name -> name = fst tp.tp_name) !names then display_error ctx.com ("Duplicate type parameter name: " ^ fst tp.tp_name) (pos tp.tp_name);
		names := (fst tp.tp_name) :: !names;
		tp,type_type_param ctx host path p tp
	) tpl in
	let params = List.map snd param_pairs in
	let ctx = TyperManager.clone_for_type_params ctx (params @ ctx.type_params) in
	List.iter (fun (tp,ttp) ->
		begin match tp.tp_default with
			| None ->
				()
			| Some ct ->
				let r = make_lazy ctx.g ttp.ttp_type (fun r ->
					let t = load_complex_type ctx true LoadNormal ct in
					begin match host with
						| TPHType ->
							()
						| TPHConstructor
						| TPHMethod
						| TPHEnumConstructor
						| TPHAnonField
						| TPHLocal
						| TPHUnbound ->
							display_error ctx.com "Default type parameters are only supported on types" (pos ct)
					end;
					check_param_constraints ctx t (fun t -> t) ttp (pos ct);
					t
				) "default" in
				ttp.ttp_default <- Some (TLazy r)
		end;
		match tp.tp_constraints with
		| None ->
			()
		| Some th ->
			let constraints = lazy (
				let rec loop th = match fst th with
					| CTIntersection tl -> List.map (load_complex_type ctx true LoadNormal) tl
					| CTParent ct -> loop ct
					| _ -> [load_complex_type ctx true LoadNormal th]
				in
				let constr = loop th in
				(* check against direct recursion *)
				let rec loop t =
					match follow t with
					| TInst (c2,_) when ttp.ttp_class == c2 ->
						raise_typing_error "Recursive constraint parameter is not allowed" p
					| TInst ({ cl_kind = KTypeParameter ttp },_) ->
						List.iter loop (get_constraints ttp)
					| _ ->
						()
				in
				List.iter loop constr;
				constr
			) in
			delay ctx.g PConnectField (fun () -> ignore (Lazy.force constraints));
			ttp.ttp_constraints <- Some constraints;
	) param_pairs;
	params

let load_core_class ctx c =
	let ctx2 = (match ctx.g.core_api with
		| None ->
			let com2 = Common.clone ctx.com ctx.com.is_macro_context in
			com2.defines.Define.values <- PMap.empty;
			Common.define com2 Define.CoreApi;
			Common.define com2 Define.Sys;
			Define.raw_define_value com2.defines "target.threaded" "true"; (* hack because we check this in sys.thread classes *)
			if ctx.com.is_macro_context then Common.define com2 Define.Macro;
			com2.class_paths#lock_context (platform_name_macro ctx.com) true;
			com2.class_paths#modify (fun cp -> match cp#scope with
				| Std ->
					[cp#clone]
				| _ ->
					[]
			) ctx.com.class_paths#as_list;
			if com2.display.dms_check_core_api then com2.display <- {com2.display with dms_check_core_api = false};
			CommonCache.lock_signature com2 "load_core_class";
			let ctx2 = !create_context_ref com2 ctx.g.macros in
			ctx.g.core_api <- Some ctx2;
			ctx2
		| Some c ->
			c
	) in
	let tpath = match c.cl_kind with
		| KAbstractImpl a -> a.a_path
		| _ -> c.cl_path
	in
	let t = load_type_def' ctx2 (fst c.cl_module.m_path) (snd c.cl_module.m_path) (snd tpath) null_pos in
	flush_pass ctx2.g PFinal ("core_final",(fst c.cl_path @ [snd c.cl_path]));
	match t with
	| TClassDecl ccore | TAbstractDecl {a_impl = Some ccore} ->
		ccore
	| _ ->
		die "" __LOC__

let init_core_api ctx c =
	let ccore = load_core_class ctx c in
	begin try
		List.iter2 (fun ttp1 ttp2 ->
			try
				List.iter2 (fun t1 t2 -> type_eq EqCoreType t2 t1) (get_constraints ttp1) (get_constraints ttp2)
			with
				| Invalid_argument _ ->
					raise_typing_error "Type parameters must have the same number of constraints as core type" c.cl_pos
				| Unify_error l ->
					display_error_ext ctx.com (make_error (Custom ("Type parameter " ^ ttp2.ttp_name ^ " has different constraint than in core type")) ~sub:([
						(make_error ~depth:1 (Custom (compl_msg (error_msg (Unify l)))) c.cl_pos);
					]) c.cl_pos);
		) ccore.cl_params c.cl_params;
	with Invalid_argument _ ->
		raise_typing_error "Class must have the same number of type parameters as core type" c.cl_pos
	end;
	(match c.cl_doc with
	| None -> c.cl_doc <- ccore.cl_doc
	| Some _ -> ());
	let compare_fields f f2 =
		let p = (match f2.cf_expr with None -> c.cl_pos | Some e -> e.epos) in
		(try
			type_eq EqCoreType (apply_params ccore.cl_params (extract_param_types c.cl_params) f.cf_type) f2.cf_type
		with Unify_error l ->
			display_error_ext ctx.com (make_error (Custom ("Field " ^ f.cf_name ^ " has different type than in core type")) ~sub:([
				(make_error ~depth:1 (Custom (compl_msg (error_msg (Unify l)))) p);
			]) p));
		if (has_class_field_flag f2 CfPublic) <> (has_class_field_flag f CfPublic) then raise_typing_error ("Field " ^ f.cf_name ^ " has different visibility than core type") p;
		(match f2.cf_doc with
		| None -> f2.cf_doc <- f.cf_doc
		| Some _ -> ());
		if f2.cf_kind <> f.cf_kind then begin
			match f2.cf_kind, f.cf_kind with
			| Method MethInline, Method MethNormal -> () (* allow to add 'inline' *)
			| Method MethNormal, Method MethInline -> () (* allow to disable 'inline' *)
			| _ ->
				raise_typing_error ("Field " ^ f.cf_name ^ " has different property access than core type") p;
		end;
		(match follow f.cf_type, follow f2.cf_type with
		| TFun (pl1,_), TFun (pl2,_) ->
			if List.length pl1 != List.length pl2 then raise_typing_error "Argument count mismatch" p;
			List.iter2 (fun (n1,_,_) (n2,_,_) ->
				if n1 <> n2 then raise_typing_error ("Method parameter name '" ^ n2 ^ "' should be '" ^ n1 ^ "'") p;
			) pl1 pl2;
		| _ -> ());
	in
	let check_fields fcore fl =
		PMap.iter (fun i f ->
			if not (has_class_field_flag f CfPublic) then () else
			let f2 = try PMap.find f.cf_name fl with Not_found -> raise_typing_error ("Missing field " ^ i ^ " required by core type") c.cl_pos in
			compare_fields f f2;
		) fcore;
		PMap.iter (fun i f ->
			let p = (match f.cf_expr with None -> c.cl_pos | Some e -> e.epos) in
			if (has_class_field_flag f CfPublic) && not (Meta.has Meta.Hack f.cf_meta) && not (PMap.mem f.cf_name fcore) && not (has_class_field_flag f CfOverride) then raise_typing_error ("Public field " ^ i ^ " is not part of core type") p;
		) fl;
	in
	check_fields ccore.cl_fields c.cl_fields;
	check_fields ccore.cl_statics c.cl_statics;
	(match ccore.cl_constructor, c.cl_constructor with
	| None, None -> ()
	| Some cf, _ when not (has_class_field_flag cf CfPublic) -> ()
	| Some f, Some f2 -> compare_fields f f2
	| None, Some cf when not (has_class_field_flag cf CfPublic) -> ()
	| _ -> raise_typing_error "Constructor differs from core type" c.cl_pos)
