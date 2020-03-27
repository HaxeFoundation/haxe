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

(* Typing of functions and their arguments. *)

open Globals
open Ast
open Type
open Typecore
open DisplayTypes.DisplayMode
open DisplayException
open Common
open Error

let type_function_arg ctx t e opt p =
	(* TODO https://github.com/HaxeFoundation/haxe/issues/8461 *)
	(* delay ctx PTypeField (fun() ->
		if ExtType.is_void (follow t) then
			error "Arguments of type Void are not allowed" p
	); *)
	if opt then
		let e = (match e with None -> Some (EConst (Ident "null"),null_pos) | _ -> e) in
		ctx.t.tnull t, e
	else
		let t = match e with Some (EConst (Ident "null"),null_pos) -> ctx.t.tnull t | _ -> t in
		t, e

let save_field_state ctx =
	let old_ret = ctx.ret in
	let old_fun = ctx.curfun in
	let old_opened = ctx.opened in
	let locals = ctx.locals in
	(fun () ->
		ctx.locals <- locals;
		ctx.ret <- old_ret;
		ctx.curfun <- old_fun;
		ctx.opened <- old_opened;
	)

let type_var_field ctx t e stat do_display p =
	if stat then ctx.curfun <- FunStatic else ctx.curfun <- FunMember;
	let e = if do_display then Display.ExprPreprocessing.process_expr ctx.com e else e in
	let e = type_expr ctx e (WithType.with_type t) in
	let e = AbstractCast.cast_or_unify ctx t e p in
	match t with
	| TType ({ t_path = ([],"UInt") },[]) | TAbstract ({ a_path = ([],"UInt") },[]) when stat -> { e with etype = t }
	| _ -> e

let type_var_field ctx t e stat do_display p =
	let save = save_field_state ctx in
	Std.finally save (type_var_field ctx t e stat do_display) p

let type_function_params ctx fd fname p =
	let params = ref [] in
	params := Typeload.type_type_params ctx ([],fname) (fun() -> !params) p fd.f_params;
	!params

let type_function_arg_value ctx t c do_display =
	match c with
		| None -> None
		| Some e ->
			let p = pos e in
			let e = if do_display then Display.ExprPreprocessing.process_expr ctx.com e else e in
			let e = ctx.g.do_optimize ctx (type_expr ctx e (WithType.with_type t)) in
			unify ctx e.etype t p;
			let rec loop e = match e.eexpr with
				| TConst _ -> Some e
				| TField({eexpr = TTypeExpr _},FEnum _) -> Some e
				| TField({eexpr = TTypeExpr _},FStatic({cl_kind = KAbstractImpl a},cf)) when Meta.has Meta.Enum a.a_meta && Meta.has Meta.Enum cf.cf_meta -> Some e
				| TCast(e,None) -> loop e
				| _ ->
					if ctx.com.display.dms_kind = DMNone || ctx.com.display.dms_inline && ctx.com.display.dms_error_policy = EPCollect then
						display_error ctx "Parameter default value should be constant" p;
					None
			in
			loop e

let process_function_arg ctx n t c do_display p =
	if starts_with n '$' then error "Function argument names starting with a dollar are not allowed" p;
	type_function_arg_value ctx t c do_display

let type_function ctx args ret fmode f do_display p =
	let fargs = List.map2 (fun (n,c,t) ((_,pn),_,m,_,_) ->
		let c = process_function_arg ctx n t c do_display pn in
		let v = add_local_with_origin ctx TVOArgument n t pn in
		v.v_meta <- v.v_meta @ m;
		if do_display && DisplayPosition.display_position#enclosed_in pn then
			DisplayEmitter.display_variable ctx v pn;
		if n = "this" then v.v_meta <- (Meta.This,[],null_pos) :: v.v_meta;
		v,c
	) args f.f_args in
	ctx.curfun <- fmode;
	ctx.ret <- ret;
	ctx.opened <- [];
	let e = match f.f_expr with
		| None ->
			if ctx.com.display.dms_error_policy = EPIgnore then
				(* when we don't care because we're in display mode, just act like
				   the function has an empty block body. this is fine even if function
				   defines a return type, because returns aren't checked in this mode
				*)
				EBlock [],p
			else
				error "Function body required" p
		| Some e -> e
	in
	let is_position_debug = Meta.has (Meta.Custom ":debug.position") ctx.curfield.cf_meta in
	let e = if not do_display then begin
		if is_position_debug then print_endline ("syntax:\n" ^ (Expr.dump_with_pos e));
		type_expr ctx e NoValue
	end else begin
		let is_display_debug = Meta.has (Meta.Custom ":debug.display") ctx.curfield.cf_meta in
		if is_display_debug then print_endline ("before processing:\n" ^ (Expr.dump_with_pos e));
		let e = if !Parser.had_resume then e else Display.ExprPreprocessing.process_expr ctx.com e in
		if is_display_debug then print_endline ("after processing:\n" ^ (Expr.dump_with_pos e));
		type_expr ctx e NoValue
	end in
	let e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), ({eexpr = TBlock el} as e1)) -> e1
		| _ -> e
	in
	let has_return e =
		let rec loop e =
			match e.eexpr with
			| TReturn (Some _) -> raise Exit
			| TFunction _ -> ()
			| _ -> Type.iter loop e
		in
		try loop e; false with Exit -> true
	in
	begin match follow ret with
		| TAbstract({a_path=[],"Void"},_) -> ()
		(* We have to check for the presence of return expressions here because
		   in the case of Dynamic ctx.ret is still a monomorph. If we indeed
		   don't have a return expression we can link the monomorph to Void. We
		   can _not_ use type_iseq to avoid the Void check above because that
		   would turn Dynamic returns to Void returns. *)
		| TMono t when not (has_return e) -> ignore(link t ret ctx.t.tvoid)
		| _ when ctx.com.display.dms_error_policy = EPIgnore -> ()
		| _ -> (try TypeloadCheck.return_flow ctx e with Exit -> ())
	end;
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper },_) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let has_super_constr() =
		match ctx.curclass.cl_super with
		| None ->
			None
		| Some (csup,tl) ->
			try
				let _,cf = get_constructor (fun f->f.cf_type) csup in
				Some (Meta.has Meta.CompilerGenerated cf.cf_meta,TInst(csup,tl))
			with Not_found ->
				None
	in
	let e = if fmode <> FunConstructor then
		e
	else begin
		delay ctx PForce (fun () -> TypeloadCheck.check_final_vars ctx e);
		match has_super_constr() with
		| Some (was_forced,t_super) ->
			(try
				loop e;
				if was_forced then
					let e_super = mk (TConst TSuper) t_super e.epos in
					let e_super_call = mk (TCall(e_super,[])) ctx.t.tvoid e.epos in
					concat e_super_call e
				else begin
					display_error ctx "Missing super constructor call" p;
					e
				end
			with
				Exit -> e);
		| None ->
			e
	end in
	let e = match ctx.curfun, ctx.vthis with
		| (FunMember|FunConstructor), Some v ->
			let ev = mk (TVar (v,Some (mk (TConst TThis) ctx.tthis p))) ctx.t.tvoid p in
			(match e.eexpr with
			| TBlock l ->
				if ctx.com.config.pf_this_before_super then
					{ e with eexpr = TBlock (ev :: l) }
				else begin
					let rec has_v e = match e.eexpr with
						| TLocal v' when v == v -> true
						| _ -> check_expr has_v e
					in
					let rec loop el = match el with
						| e :: el ->
							if has_v e then
								ev :: e :: el
							else
								e :: loop el
						| [] ->
							(* should not happen... *)
							[]
					in
					{ e with eexpr = TBlock (loop l) }
				end
			| _ -> mk (TBlock [ev;e]) e.etype p)
		| _ -> e
	in
	List.iter (fun r -> r := Closed) ctx.opened;
	let rec check_args fargs ast_args first =
		match fargs, ast_args with
		| (v,e_opt) :: rest_fargs, (_,opt,_,_,_) :: rest_ast_args ->
			if ExtType.is_rest (follow v.v_type) then begin
				if opt then error "Rest argument cannot be optional" v.v_pos;
				if Option.is_some e_opt then error "Rest argument cannot have default value" v.v_pos;
				if rest_fargs <> [] && not (first && fmode = FunMemberAbstract) then
					error "Rest should only be used for the last function argument" v.v_pos;
			end;
			check_args rest_fargs rest_ast_args false
		| [], [] -> ()
		| _ -> assert false
	in
	check_args fargs f.f_args true;
	if is_position_debug then print_endline ("typing:\n" ^ (Texpr.dump_with_pos "" e));
	e , fargs

let type_function ctx args ret fmode f do_display p =
	let save = save_field_state ctx in
	Std.finally save (type_function ctx args ret fmode f do_display) p

let add_constructor ctx c force_constructor p =
	match c.cl_constructor, c.cl_super with
	| None, Some ({ cl_constructor = Some cfsup } as csup,cparams) when not c.cl_extern ->
		let cf = {
			cfsup with
			cf_pos = p;
			cf_meta = List.filter (fun (m,_,_) -> m = Meta.CompilerGenerated) cfsup.cf_meta;
			cf_doc = None;
			cf_expr = None;
		} in
		let r = exc_protect ctx (fun r ->
			let t = mk_mono() in
			r := lazy_processing (fun() -> t);
			let ctx = { ctx with
				curfield = cf;
				pass = PTypeField;
			} in
			ignore (follow cfsup.cf_type); (* make sure it's typed *)
			(if ctx.com.config.pf_overload then List.iter (fun cf -> ignore (follow cf.cf_type)) cf.cf_overloads);
			let map_arg (v,def) =
				(*
					let's optimize a bit the output by not always copying the default value
					into the inherited constructor when it's not necessary for the platform
				*)
				let null () = Some (Texpr.Builder.make_null v.v_type v.v_pos) in
				match ctx.com.platform, def with
				| _, Some _ when not ctx.com.config.pf_static -> v, null()
				| Flash, Some ({eexpr = TConst (TString _)}) -> v, null()
				| Cpp, Some ({eexpr = TConst (TString _)}) -> v, def
				| Cpp, Some _ -> { v with v_type = ctx.t.tnull v.v_type }, null()
				| _ -> v, def
			in
			let args = (match cfsup.cf_expr with
				| Some { eexpr = TFunction f } ->
					List.map map_arg f.tf_args
				| _ ->
					let values = get_value_meta cfsup.cf_meta in
					match follow cfsup.cf_type with
					| TFun (args,_) ->
						List.map (fun (n,o,t) ->
							let def = try
								type_function_arg_value ctx t (Some (PMap.find n values)) false
							with Not_found ->
								if o then Some (Texpr.Builder.make_null t null_pos) else None
							in
							map_arg (alloc_var (VUser TVOArgument) n (if o then ctx.t.tnull t else t) p,def) (* TODO: var pos *)
						) args
					| _ -> assert false
			) in
			let p = c.cl_pos in
			let vars = List.map (fun (v,def) -> alloc_var (VUser TVOArgument) v.v_name (apply_params csup.cl_params cparams v.v_type) v.v_pos, def) args in
			let super_call = mk (TCall (mk (TConst TSuper) (TInst (csup,cparams)) p,List.map (fun (v,_) -> mk (TLocal v) v.v_type p) vars)) ctx.t.tvoid p in
			let constr = mk (TFunction {
				tf_args = vars;
				tf_type = ctx.t.tvoid;
				tf_expr = super_call;
			}) (TFun (List.map (fun (v,c) -> v.v_name, c <> None, v.v_type) vars,ctx.t.tvoid)) p in
			cf.cf_expr <- Some constr;
			cf.cf_type <- t;
			unify ctx t constr.etype p;
			t
		) "add_constructor" in
		cf.cf_type <- TLazy r;
		c.cl_constructor <- Some cf;
	| None,_ when force_constructor ->
		let constr = mk (TFunction {
			tf_args = [];
			tf_type = ctx.t.tvoid;
			tf_expr = mk (TBlock []) ctx.t.tvoid p;
		}) (tfun [] ctx.t.tvoid) p in
		let cf = mk_field "new" constr.etype p null_pos in
		cf.cf_expr <- Some constr;
		cf.cf_type <- constr.etype;
		cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos];
		cf.cf_kind <- Method MethNormal;
		c.cl_constructor <- Some cf;
	| _ ->
		(* nothing to do *)
		()
;;
Typeload.type_function_params_rec := type_function_params