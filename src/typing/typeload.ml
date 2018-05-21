(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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
open CompletionItem.CompletionModuleType
open CompletionItem.CompletionModuleKind
open DisplayException
open Type
open Typecore
open Error
open Globals

let build_count = ref 0

let type_function_params_rec = ref (fun _ _ _ _ -> assert false)

let check_field_access ctx acc =
	ignore(List.fold_left (fun acc (access,p1) ->
		try
			let _,p2 = List.find (fun (access',_) -> access = access') acc in
			if p1 <> null_pos && p2 <> null_pos then begin
				display_error ctx (Printf.sprintf "Duplicate access modifier %s" (Ast.s_access access)) p1;
				display_error ctx "Previously defined here" p2;
			end;
			acc
		with Not_found -> match access with
			| APublic | APrivate ->
				begin try
					let _,p2 = List.find (fun (access',_) -> match access' with APublic | APrivate -> true | _ -> false) acc in
					display_error ctx (Printf.sprintf "Conflicting access modifier %s" (Ast.s_access access)) p1;
					display_error ctx "Conflicts with this" p2;
					acc
				with Not_found ->
					(access,p1) :: acc
				end
			| _ ->
				(access,p1) :: acc
	) [] acc)

(** since load_type_def and load_instance are used in PASS2, they should not access the structure of a type **)

(*
	load a type or a subtype definition
*)
let rec load_type_def ctx p t =
	let no_pack = t.tpackage = [] in
	let tname = (match t.tsub with None -> t.tname | Some n -> n) in
	if tname = "" then raise_fields (DisplayToplevel.collect ctx true NoValue) CRTypeHint None false;
	try
		if t.tsub <> None then raise Not_found;
		let path_matches t2 =
			let tp = t_path t2 in
			tp = (t.tpackage,tname) || (no_pack && snd tp = tname)
		in
		try
			List.find path_matches ctx.m.curmod.m_types
		with Not_found ->
			let t,pi = List.find (fun (t2,pi) -> path_matches t2) ctx.m.module_types in
			ImportHandling.mark_import_position ctx.com pi;
			t
	with
		Not_found ->
			let next() =
				let t, m = (try
					t, ctx.g.do_load_module ctx (t.tpackage,t.tname) p
				with Error (Module_not_found _,p2) as e when p == p2 ->
					match t.tpackage with
					| "std" :: l ->
						let t = { t with tpackage = l } in
						t, ctx.g.do_load_module ctx (t.tpackage,t.tname) p
					| _ -> raise e
				) in
				let tpath = (t.tpackage,tname) in
				try
					List.find (fun t -> not (t_infos t).mt_private && t_path t = tpath) m.m_types
				with
					Not_found -> raise (Error (Type_not_found (m.m_path,tname),p))
			in
			(* lookup in wildcard imported packages *)
			try
				if not no_pack then raise Exit;
				let rec loop l = match l with
					| [] -> raise Exit
					| (wp,pi) :: l ->
						try
							let t = load_type_def ctx p { t with tpackage = wp } in
							ImportHandling.mark_import_position ctx.com pi;
							t
						with
							| Error (Module_not_found _,p2)
							| Error (Type_not_found _,p2) when p == p2 -> loop l
				in
				loop ctx.m.wildcard_packages
			with Exit ->
			(* lookup in our own package - and its upper packages *)
			let rec loop = function
				| [] -> raise Exit
				| (_ :: lnext) as l ->
					try
						load_type_def ctx p { t with tpackage = List.rev l }
					with
						| Error (Module_not_found _,p2)
						| Error (Type_not_found _,p2) when p == p2 -> loop lnext
			in
			try
				if not no_pack then raise Exit;
				(match fst ctx.m.curmod.m_path with
				| [] -> raise Exit
				| x :: _ ->
					(* this can occur due to haxe remoting : a module can be
						already defined in the "js" package and is not allowed
						to access the js classes *)
					try
						(match PMap.find x ctx.com.package_rules with
						| Forbidden -> raise Exit
						| _ -> ())
					with Not_found -> ());
				loop (List.rev (fst ctx.m.curmod.m_path));
			with
				Exit -> next()

let resolve_position_by_path ctx path p =
	let mt = load_type_def ctx p path in
	let p = (t_infos mt).mt_pos in
	raise_position [p]

let check_param_constraints ctx types t pl c p =
	match follow t with
	| TMono _ -> ()
	| _ ->
		let ctl = (match c.cl_kind with KTypeParameter l -> l | _ -> []) in
		List.iter (fun ti ->
			let ti = apply_params types pl ti in
			let ti = (match follow ti with
				| TInst ({ cl_kind = KGeneric } as c,pl) ->
					(* if we solve a generic contraint, let's substitute with the actual generic instance before unifying *)
					let _,_, f = ctx.g.do_build_instance ctx (TClassDecl c) p in
					f pl
				| _ -> ti
			) in
			try
				unify_raise ctx t ti p
			with Error(Unify l,p) ->
				let fail() =
					if not ctx.untyped then display_error ctx (error_msg (Unify (Constraint_failure (s_type_path c.cl_path) :: l))) p;
				in
				match follow t with
				| TInst({cl_kind = KExpr e},_) ->
					let e = type_expr {ctx with locals = PMap.empty} e (WithType ti) in
					begin try unify_raise ctx e.etype ti p
					with Error (Unify _,_) -> fail() end
				| _ ->
					fail()

		) ctl

let requires_value_meta com co =
	Common.defined com Define.DocGen || (match co with
		| None -> false
		| Some c -> c.cl_extern || Meta.has Meta.Rtti c.cl_meta)

let generate_value_meta com co cf args =
	if requires_value_meta com co then begin
		let values = List.fold_left (fun acc ((name,p),_,_,_,eo) -> match eo with Some e -> ((name,p,NoQuotes),e) :: acc | _ -> acc) [] args in
		match values with
			| [] -> ()
			| _ -> cf.cf_meta <- ((Meta.Value,[EObjectDecl values,cf.cf_pos],null_pos) :: cf.cf_meta)
	end

let pselect p1 p2 =
	if p1 = null_pos then p2 else p1

(* build an instance from a full type *)
let rec load_instance' ctx (t,pn) allow_no_params p =
	let p = pselect pn p in
	let t = try
		if t.tpackage <> [] || t.tsub <> None then raise Not_found;
		let pt = List.assoc t.tname ctx.type_params in
		if t.tparams <> [] then error ("Class type parameter " ^ t.tname ^ " can't have parameters") p;
		pt
	with Not_found ->
		let mt = load_type_def ctx p t in
		let is_generic,is_generic_build = match mt with
			| TClassDecl {cl_kind = KGeneric} -> true,false
			| TClassDecl {cl_kind = KGenericBuild _} -> false,true
			| _ -> false,false
		in
		let types , path , f = ctx.g.do_build_instance ctx mt p in
		let is_rest = is_generic_build && (match types with ["Rest",_] -> true | _ -> false) in
		if allow_no_params && t.tparams = [] && not is_rest then begin
			let pl = ref [] in
			pl := List.map (fun (name,t) ->
				match follow t with
				| TInst (c,_) ->
					let t = mk_mono() in
					if c.cl_kind <> KTypeParameter [] || is_generic then delay ctx PCheckConstraint (fun() -> check_param_constraints ctx types t (!pl) c p);
					t;
				| _ -> assert false
			) types;
			f (!pl)
		end else if path = ([],"Dynamic") then
			match t.tparams with
			| [] -> t_dynamic
			| [TPType t] -> TDynamic (load_complex_type ctx true p t)
			| _ -> error "Too many parameters for Dynamic" p
		else begin
			if not is_rest && ctx.com.display.dms_error_policy <> EPIgnore && List.length types <> List.length t.tparams then error ("Invalid number of type parameters for " ^ s_type_path path) p;
			let tparams = List.map (fun t ->
				match t with
				| TPExpr e ->
					let name = (match fst e with
						| EConst (String s) -> "S" ^ s
						| EConst (Int i) -> "I" ^ i
						| EConst (Float f) -> "F" ^ f
						| EDisplay _ ->
							ignore(type_expr ctx e Value);
							"Expr"
						| _ -> "Expr"
					) in
					let c = mk_class ctx.m.curmod ([],name) p (pos e) in
					c.cl_kind <- KExpr e;
					TInst (c,[])
				| TPType t -> load_complex_type ctx true p t
			) t.tparams in
			let rec loop tl1 tl2 is_rest = match tl1,tl2 with
				| t :: tl1,(name,t2) :: tl2 ->
					let check_const c =
						let is_expression = (match t with TInst ({ cl_kind = KExpr _ },_) -> true | _ -> false) in
						let expects_expression = name = "Const" || Meta.has Meta.Const c.cl_meta in
						let accepts_expression = name = "Rest" in
						if is_expression then begin
							if not expects_expression && not accepts_expression then
								error "Constant value unexpected here" p
						end else if expects_expression then
							error "Type parameter is expected to be a constant value" p
					in
					let is_rest = is_rest || name = "Rest" && is_generic_build in
					let t = match follow t2 with
						| TInst ({ cl_kind = KTypeParameter [] } as c, []) when not is_generic ->
							check_const c;
							t
						| TInst (c,[]) ->
							check_const c;
							let r = exc_protect ctx (fun r ->
								r := lazy_available t;
								delay ctx PCheckConstraint (fun() -> check_param_constraints ctx types t tparams c p);
								t
							) "constraint" in
							TLazy r
						| _ -> assert false
					in
					t :: loop tl1 tl2 is_rest
				| [],[] ->
					[]
				| [],["Rest",_] when is_generic_build ->
					[]
				| [],(_,t) :: tl when ctx.com.display.dms_error_policy = EPIgnore ->
					t :: loop [] tl is_rest
				| [],_ ->
					error ("Not enough type parameters for " ^ s_type_path path) p
				| t :: tl,[] ->
					if is_rest then
						t :: loop tl [] true
					else
						error ("Too many parameters for " ^ s_type_path path) p
			in
			let params = loop tparams types false in
			f params
		end
	in
	t

and load_instance ctx ?(allow_display=false) (t,pn) allow_no_params p =
	try
		let t = load_instance' ctx (t,pn) allow_no_params p in
		if allow_display then DisplayEmitter.check_display_type ctx t pn;
		t
	with Error (Module_not_found path,_) when (ctx.com.display.dms_kind = DMDefault) && Display.is_display_position pn ->
		let s = s_type_path path in
		raise_fields (DisplayToplevel.collect ctx false NoValue) CRTypeHint (Some {pn with pmin = pn.pmax - String.length s;}) false

(*
	build an instance from a complex type
*)
and load_complex_type ctx allow_display p (t,pn) =
	let p = pselect pn p in
	match t with
	| CTParent t -> load_complex_type ctx allow_display p t
	| CTPath t -> load_instance ~allow_display ctx (t,pn) false p
	| CTOptional _ -> error "Optional type not allowed here" p
	| CTNamed _ -> error "Named type not allowed here" p
	| CTExtend (tl,l) ->
		begin match load_complex_type ctx allow_display p (CTAnonymous l,p) with
		| TAnon a as ta ->
			let is_redefined cf1 a2 =
				try
					let cf2 = PMap.find cf1.cf_name a2.a_fields in
					let st = s_type (print_context()) in
					if not (type_iseq cf1.cf_type cf2.cf_type) then begin
						display_error ctx ("Cannot redefine field " ^ cf1.cf_name ^ " with different type") p;
						display_error ctx ("First type was " ^ (st cf1.cf_type)) cf1.cf_pos;
						error ("Second type was " ^ (st cf2.cf_type)) cf2.cf_pos
					end else
						true
				with Not_found ->
					false
			in
			let mk_extension t =
				match follow t with
				| TInst ({cl_kind = KTypeParameter _},_) ->
					error "Cannot structurally extend type parameters" p
				| TMono _ ->
					error "Loop found in cascading signatures definitions. Please change order/import" p
				| TAnon a2 ->
					PMap.iter (fun _ cf -> ignore(is_redefined cf a2)) a.a_fields;
					TAnon { a_fields = (PMap.foldi PMap.add a.a_fields a2.a_fields); a_status = ref (Extend [t]); }
				| _ -> error "Can only extend structures" p
			in
			let loop t = match follow t with
				| TAnon a2 ->
					PMap.iter (fun f cf ->
						if not (is_redefined cf a) then
							a.a_fields <- PMap.add f cf a.a_fields
					) a2.a_fields
				| _ ->
					error "Can only extend structures" p
			in
			let il = List.map (fun (t,pn) ->
				try
					load_instance ctx ~allow_display (t,pn) false p
				with DisplayException(DisplayFields(l,CRTypeHint,p,b)) ->
					let l = List.filter (function
						| ITType({kind = Struct},_) -> true
						| _ -> false
					) l in
					raise_fields l CRStructExtension p b
			) tl in
			let tr = ref None in
			let t = TMono tr in
			let r = exc_protect ctx (fun r ->
				r := lazy_processing (fun() -> t);
				tr := Some (match il with
					| [i] ->
						mk_extension i
					| _ ->
						List.iter loop il;
						a.a_status := Extend il;
						ta);
				t
			) "constraint" in
			TLazy r
		| _ -> assert false
		end
	| CTAnonymous l ->
		let rec loop acc f =
			let n = fst f.cff_name in
			let p = f.cff_pos in
			if PMap.mem n acc then error ("Duplicate field declaration : " ^ n) p;
			let topt = function
				| None -> error ("Explicit type required for field " ^ n) p
				| Some t -> load_complex_type ctx allow_display p t
			in
			if n = "new" then ctx.com.warning "Structures with new are deprecated, use haxe.Constraints.Constructible instead" p;
			let no_expr = function
				| None -> ()
				| Some (_,p) -> error "Expression not allowed here" p
			in
			let pub = ref true in
			let dyn = ref false in
			let params = ref [] in
			let final = ref false in
			check_field_access ctx f.cff_access;
			List.iter (fun a ->
				match fst a with
				| APublic -> ()
				| APrivate -> pub := false;
				| ADynamic when (match f.cff_kind with FFun _ -> true | _ -> false) -> dyn := true
				| AFinal -> final := true
				| AStatic | AOverride | AInline | ADynamic | AMacro | AExtern as a -> error ("Invalid access " ^ Ast.s_access a) p
			) f.cff_access;
			let t , access = (match f.cff_kind with
				| FVar(t,e) when !final ->
					no_expr e;
					let t = (match t with None -> error "Type required for structure property" p | Some t -> t) in
					load_complex_type ctx allow_display p t, Var { v_read = AccNormal; v_write = AccNever }
				| FVar (Some (CTPath({tpackage=[];tname="Void"}),_), _)  | FProp (_,_,Some (CTPath({tpackage=[];tname="Void"}),_),_) ->
					error "Fields of type Void are not allowed in structures" p
				| FVar (t, e) ->
					no_expr e;
					topt t, Var { v_read = AccNormal; v_write = AccNormal }
				| FFun fd ->
					params := (!type_function_params_rec) ctx fd (fst f.cff_name) p;
					no_expr fd.f_expr;
					let old = ctx.type_params in
					ctx.type_params <- !params @ old;
					let args = List.map (fun ((name,_),o,_,t,e) -> no_expr e; name, o, topt t) fd.f_args in
					let t = TFun (args,topt fd.f_type), Method (if !dyn then MethDynamic else MethNormal) in
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
							error "Custom property access is no longer supported in Haxe 3" f.cff_pos;
					in
					let t = (match t with None -> error "Type required for structure property" p | Some t -> t) in
					load_complex_type ctx allow_display p t, Var { v_read = access i1 true; v_write = access i2 false }
			) in
			let t = if Meta.has Meta.Optional f.cff_meta then ctx.t.tnull t else t in
			let cf = {
				(mk_field n t p (pos f.cff_name)) with
				cf_public = !pub;
				cf_kind = access;
				cf_params = !params;
				cf_doc = f.cff_doc;
				cf_meta = f.cff_meta;
			} in
			init_meta_overloads ctx None cf;
			if ctx.is_display_file then begin
				DisplayEmitter.check_display_metadata ctx cf.cf_meta;
				DisplayEmitter.maybe_display_field ctx None cf cf.cf_name_pos;
			end;
			PMap.add n cf acc
		in
		mk_anon (List.fold_left loop PMap.empty l)
	| CTFunction (args,r) ->
		match args with
		| [CTPath { tpackage = []; tparams = []; tname = "Void" },_] ->
			TFun ([],load_complex_type ctx allow_display p r)
		| _ ->
			TFun (List.map (fun t ->
				let t, opt = (match fst t with CTOptional t -> t, true | _ -> t,false) in
				let n,t = (match fst t with CTNamed (n,t) -> (fst n), t | _ -> "", t) in
				n,opt,load_complex_type ctx allow_display p t
			) args,load_complex_type ctx allow_display p r)

and init_meta_overloads ctx co cf =
	let overloads = ref [] in
	let filter_meta m = match m with
		| ((Meta.Overload | Meta.Value),_,_) -> false
		| _ -> true
	in
	let cf_meta = List.filter filter_meta cf.cf_meta in
	cf.cf_meta <- List.filter (fun m ->
		match m with
		| (Meta.Overload,[(EFunction (fname,f),p)],_)  ->
			if fname <> None then error "Function name must not be part of @:overload" p;
			(match f.f_expr with Some (EBlock [], _) -> () | _ -> error "Overload must only declare an empty method body {}" p);
			let old = ctx.type_params in
			(match cf.cf_params with
			| [] -> ()
			| l -> ctx.type_params <- List.filter (fun t -> not (List.mem t l)) ctx.type_params);
			let params = (!type_function_params_rec) ctx f cf.cf_name p in
			ctx.type_params <- params @ ctx.type_params;
			let topt = function None -> error "Explicit type required" p | Some t -> load_complex_type ctx true p t in
			let args = List.map (fun ((a,_),opt,_,t,_) -> a,opt,topt t) f.f_args in
			let cf = { cf with cf_type = TFun (args,topt f.f_type); cf_params = params; cf_meta = cf_meta} in
			generate_value_meta ctx.com co cf f.f_args;
			overloads := cf :: !overloads;
			ctx.type_params <- old;
			false
		| (Meta.Overload,[],_) when ctx.com.config.pf_overload ->
			let topt (n,_,t) = match t with | TMono t when !t = None -> error ("Explicit type required for overload functions\nFor function argument '" ^ n ^ "'") cf.cf_pos | _ -> () in
			(match follow cf.cf_type with
			| TFun (args,_) -> List.iter topt args
			| _ -> () (* could be a variable *));
			true
		| (Meta.Overload,[],p) ->
				error "This platform does not support this kind of overload declaration. Try @:overload(function()... {}) instead" p
		| (Meta.Overload,_,p) ->
				error "Invalid @:overload metadata format" p
		| _ ->
			true
	) cf.cf_meta;
	cf.cf_overloads <- (List.rev !overloads)

let hide_params ctx =
	let old_m = ctx.m in
	let old_type_params = ctx.type_params in
	let old_deps = ctx.g.std.m_extra.m_deps in
	ctx.m <- {
		curmod = ctx.g.std;
		module_types = [];
		module_using = [];
		module_globals = PMap.empty;
		wildcard_packages = [];
		module_imports = [];
	};
	ctx.type_params <- [];
	(fun() ->
		ctx.m <- old_m;
		ctx.type_params <- old_type_params;
		(* restore dependencies that might be have been wronly inserted *)
		ctx.g.std.m_extra.m_deps <- old_deps;
	)

(*
	load a type while ignoring the current imports or local types
*)
let load_core_type ctx name =
	let show = hide_params ctx in
	let t = load_instance ctx ({ tpackage = []; tname = name; tparams = []; tsub = None; },null_pos) false null_pos in
	show();
	add_dependency ctx.m.curmod (match t with
	| TInst (c,_) -> c.cl_module
	| TType (t,_) -> t.t_module
	| TAbstract (a,_) -> a.a_module
	| TEnum (e,_) -> e.e_module
	| _ -> assert false);
	t

let t_iterator ctx =
	let show = hide_params ctx in
	match load_type_def ctx null_pos { tpackage = []; tname = "Iterator"; tparams = []; tsub = None } with
	| TTypeDecl t ->
		show();
		add_dependency ctx.m.curmod t.t_module;
		if List.length t.t_params <> 1 then assert false;
		let pt = mk_mono() in
		apply_params t.t_params [pt] t.t_type, pt
	| _ ->
		assert false

(*
	load either a type t or Null<Unknown> if not defined
*)
let load_type_hint ?(opt=false) ctx pcur t =
	let t = match t with
		| None -> mk_mono()
		| Some (t,p) ->
			try
				load_complex_type ctx true pcur (t,p)
			with Error(Module_not_found(([],name)),p) as exc ->
				if Diagnostics.is_diagnostics_run p then DisplayToplevel.handle_unresolved_identifier ctx name p true;
				(* Default to Dynamic in display mode *)
				if ctx.com.display.dms_display then t_dynamic else raise exc
	in
	if opt then ctx.t.tnull t else t

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let field_to_type_path ctx e =
	let rec loop e pack name = match e with
		| EField(e,f),p when Char.lowercase (String.get f 0) <> String.get f 0 -> (match name with
			| [] | _ :: [] ->
				loop e pack (f :: name)
			| _ -> (* too many name paths *)
				display_error ctx ("Unexpected " ^ f) p;
				raise Exit)
		| EField(e,f),_ ->
			loop e (f :: pack) name
		| EConst(Ident f),_ ->
			let pack, name, sub = match name with
				| [] ->
					let fchar = String.get f 0 in
					if Char.uppercase fchar = fchar then
						pack, f, None
					else begin
						display_error ctx "A class name must start with an uppercase character" (snd e);
						raise Exit
					end
				| [name] ->
					f :: pack, name, None
				| [name; sub] ->
					f :: pack, name, Some sub
				| _ ->
					assert false
			in
			{ tpackage=pack; tname=name; tparams=[]; tsub=sub }
		| _,pos ->
			display_error ctx "Unexpected expression when building strict meta" pos;
			raise Exit
	in
	loop e [] []

let rec type_type_param ?(enum_constructor=false) ctx path get_params p tp =
	let n = fst tp.tp_name in
	let c = mk_class ctx.m.curmod (fst path @ [snd path],n) (pos tp.tp_name) (pos tp.tp_name) in
	c.cl_params <- type_type_params ctx c.cl_path get_params p tp.tp_params;
	c.cl_kind <- KTypeParameter [];
	c.cl_meta <- tp.Ast.tp_meta;
	if enum_constructor then c.cl_meta <- (Meta.EnumConstructorParam,[],null_pos) :: c.cl_meta;
	let t = TInst (c,List.map snd c.cl_params) in
	if ctx.is_display_file && Display.is_display_position (pos tp.tp_name) then
		DisplayEmitter.display_type ctx t (pos tp.tp_name);
	match tp.tp_constraints with
	| [] ->
		n, t
	| _ ->
		let r = exc_protect ctx (fun r ->
			r := lazy_processing (fun() -> t);
			let ctx = { ctx with type_params = ctx.type_params @ get_params() } in
			let constr = List.map (load_complex_type ctx true p) tp.tp_constraints in
			(* check against direct recursion *)
			let rec loop t =
				match follow t with
				| TInst (c2,_) when c == c2 -> error "Recursive constraint parameter is not allowed" p
				| TInst ({ cl_kind = KTypeParameter cl },_) ->
					List.iter loop cl
				| _ ->
					()
			in
			List.iter loop constr;
			c.cl_kind <- KTypeParameter constr;
			t
		) "constraint" in
		n, TLazy r

and type_type_params ?(enum_constructor=false) ctx path get_params p tpl =
	let names = ref [] in
	List.map (fun tp ->
		if List.exists (fun name -> name = fst tp.tp_name) !names then display_error ctx ("Duplicate type parameter name: " ^ fst tp.tp_name) (pos tp.tp_name);
		names := (fst tp.tp_name) :: !names;
		type_type_param ~enum_constructor ctx path get_params p tp
	) tpl

let load_core_class ctx c =
	let ctx2 = (match ctx.g.core_api with
		| None ->
			let com2 = Common.clone ctx.com in
			com2.defines.Define.values <- PMap.empty;
			Common.define com2 Define.CoreApi;
			Common.define com2 Define.Sys;
			if ctx.in_macro then Common.define com2 Define.Macro;
			com2.class_path <- ctx.com.std_path;
			let ctx2 = ctx.g.do_create com2 in
			ctx.g.core_api <- Some ctx2;
			ctx2
		| Some c ->
			c
	) in
	let tpath = match c.cl_kind with
		| KAbstractImpl a -> { tpackage = fst a.a_path; tname = snd a.a_path; tparams = []; tsub = None; }
		| _ -> { tpackage = fst c.cl_path; tname = snd c.cl_path; tparams = []; tsub = None; }
	in
	let t = load_instance ctx2 (tpath,c.cl_pos) true c.cl_pos in
	flush_pass ctx2 PFinal "core_final";
	match t with
	| TInst (ccore,_) | TAbstract({a_impl = Some ccore}, _) ->
		ccore
	| _ ->
		assert false

let init_core_api ctx c =
	let ccore = load_core_class ctx c in
	begin try
		List.iter2 (fun (n1,t1) (n2,t2) -> match follow t1, follow t2 with
			| TInst({cl_kind = KTypeParameter l1},_),TInst({cl_kind = KTypeParameter l2},_) ->
				begin try
					List.iter2 (fun t1 t2 -> type_eq EqCoreType t2 t1) l1 l2
				with
					| Invalid_argument _ ->
						error "Type parameters must have the same number of constraints as core type" c.cl_pos
					| Unify_error l ->
						display_error ctx ("Type parameter " ^ n2 ^ " has different constraint than in core type") c.cl_pos;
						display_error ctx (error_msg (Unify l)) c.cl_pos
				end
			| t1,t2 ->
				Printf.printf "%s %s" (s_type (print_context()) t1) (s_type (print_context()) t2);
				assert false
		) ccore.cl_params c.cl_params;
	with Invalid_argument _ ->
		error "Class must have the same number of type parameters as core type" c.cl_pos
	end;
	(match c.cl_doc with
	| None -> c.cl_doc <- ccore.cl_doc
	| Some _ -> ());
	let compare_fields f f2 =
		let p = (match f2.cf_expr with None -> c.cl_pos | Some e -> e.epos) in
		(try
			type_eq EqCoreType (apply_params ccore.cl_params (List.map snd c.cl_params) f.cf_type) f2.cf_type
		with Unify_error l ->
			display_error ctx ("Field " ^ f.cf_name ^ " has different type than in core type") p;
			display_error ctx (error_msg (Unify l)) p);
		if f2.cf_public <> f.cf_public then error ("Field " ^ f.cf_name ^ " has different visibility than core type") p;
		(match f2.cf_doc with
		| None -> f2.cf_doc <- f.cf_doc
		| Some _ -> ());
		if f2.cf_kind <> f.cf_kind then begin
			match f2.cf_kind, f.cf_kind with
			| Method MethInline, Method MethNormal -> () (* allow to add 'inline' *)
			| Method MethNormal, Method MethInline -> () (* allow to disable 'inline' *)
			| _ ->
				error ("Field " ^ f.cf_name ^ " has different property access than core type") p;
		end;
		(match follow f.cf_type, follow f2.cf_type with
		| TFun (pl1,_), TFun (pl2,_) ->
			if List.length pl1 != List.length pl2 then error "Argument count mismatch" p;
			List.iter2 (fun (n1,_,_) (n2,_,_) ->
				if n1 <> n2 then error ("Method parameter name '" ^ n2 ^ "' should be '" ^ n1 ^ "'") p;
			) pl1 pl2;
		| _ -> ());
	in
	let check_fields fcore fl =
		PMap.iter (fun i f ->
			if not f.cf_public then () else
			let f2 = try PMap.find f.cf_name fl with Not_found -> error ("Missing field " ^ i ^ " required by core type") c.cl_pos in
			compare_fields f f2;
		) fcore;
		PMap.iter (fun i f ->
			let p = (match f.cf_expr with None -> c.cl_pos | Some e -> e.epos) in
			if f.cf_public && not (Meta.has Meta.Hack f.cf_meta) && not (PMap.mem f.cf_name fcore) && not (List.memq f c.cl_overrides) then error ("Public field " ^ i ^ " is not part of core type") p;
		) fl;
	in
	check_fields ccore.cl_fields c.cl_fields;
	check_fields ccore.cl_statics c.cl_statics;
	(match ccore.cl_constructor, c.cl_constructor with
	| None, None -> ()
	| Some { cf_public = false }, _ -> ()
	| Some f, Some f2 -> compare_fields f f2
	| None, Some { cf_public = false } -> ()
	| _ -> error "Constructor differs from core type" c.cl_pos)

let string_list_of_expr_path (e,p) =
	try string_list_of_expr_path_raise (e,p)
	with Exit -> error "Invalid path" p

let handle_path_display ctx path p =
	let open ImportHandling in
	match ImportHandling.convert_import_to_something_usable !Parser.resume_display path,ctx.com.display.dms_kind with
		| (IDKPackage sl,_),_ ->
			raise (Parser.TypePath(sl,None,true))
		| (IDKModule(sl,s),_),DMDefinition ->
			(* We assume that we want to go to the module file, not a specific type
			   which might not even exist anyway. *)
			let mt = ctx.g.do_load_module ctx (sl,s) p in
			let p = { pfile = mt.m_extra.m_file; pmin = 0; pmax = 0} in
			DisplayException.raise_position [p]
		| (IDKModule(sl,s),_),_ ->
			(* TODO: wait till nadako requests @type display for these, then implement it somehow *)
			raise (Parser.TypePath(sl,Some(s,false),true))
		| (IDKSubType(sl,sm,st),p),DMDefinition ->
			resolve_position_by_path ctx { tpackage = sl; tname = sm; tparams = []; tsub = Some st} p
		| (IDKSubType(sl,sm,st),_),_ ->
			raise (Parser.TypePath(sl @ [sm],Some(st,false),true))
		| ((IDKSubTypeField(sl,sm,st,sf) | IDKModuleField(sl,(sm as st),sf)),p),_ ->
			let m = ctx.g.do_load_module ctx (sl,sm) p in
			List.iter (fun t -> match t with
				| TClassDecl c when snd c.cl_path = st ->
					ignore(c.cl_build());
					let cf = PMap.find sf c.cl_statics in
					DisplayEmitter.display_field ctx (Some c) cf p
				| _ ->
					()
			) m.m_types;
		| (IDK,_),_ ->
			()