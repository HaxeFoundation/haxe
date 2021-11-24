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
open Option
open Common
open Globals
open Codegen
open Texpr.Builder
open Ast
open Type
open Gencommon

(* ******************************************* *)
(* Closures To Class *)
(* ******************************************* *)
(*

	This is a very important filter. It will take all anonymous functions from the AST, will search for all captured variables, and will create a class
	that implements an abstract interface for calling functions. This is very important for targets that don't support anonymous functions to work correctly.
	Also it is possible to implement some strategies to avoid value type boxing, such as NaN tagging or double/object arguments. All this will be abstracted away
	from this interface.


	dependencies:
		must run after dynamic field access, because of conflicting ways to deal with invokeField
		(module filter) must run after OverloadingConstructor so we can also change the dynamic function expressions

		uses TArray expressions for array. TODO see interaction
		uses TThrow expressions.
*)
let name = "closures_to_class"
let priority = solve_deps name [ DAfter DynamicFieldAccess.priority ]

type closures_ctx = {
	func_class : tclass;

	(*
		this is what will actually turn the function into class field.
		The standard implementation by default will already take care of creating the class, and setting the captured variables.

		It will also return the super arguments to be called
	*)
	closure_to_classfield : tfunc->t->pos->tclass_field * (texpr list);

	(*
		when a dynamic function call is made, we need to convert it as if it were calling the dynamic function interface.

		TCall expr -> new TCall expr
	*)
	dynamic_fun_call : texpr->texpr;

	(*
		Provide a toolchain so we can easily create classes that extend Function and add more functionality on top of it.

		arguments:
			tclass -> subject (so we know the type of this)
			( int -> (int->t->tconstant option->texpr) -> ( (tvar * tconstant option) list * texpr) )
				int -> current arity of the function whose member will be mapped; -1 for dynamic function. It is guaranteed that dynamic function will be called last
				t -> the return type of the function
				(int->t->tconstant option->texpr) -> api to get exprs that unwrap arguments correctly
					int -> argument wanted to unwrap
					t -> solicited type
					tconstant option -> map to this default value if null
					returns a texpr that tells how the default
				should return a list with additional arguments (only works if is_function_base = true)
				and the underlying function expression
	*)
	map_base_classfields : tclass->( int -> t -> (tvar list) -> (int->t->tconstant option->texpr) -> texpr )->tclass_field list;
}

type map_info = {
	in_unsafe : bool;
	in_unused : bool;
}

let null_map_info = { in_unsafe = false; in_unused = false; }

(*
	the default implementation will take 3 transformation functions:
		* one that will transform closures that are not called immediately (instance.myFunc).
			normally on this case it's best to have a runtime handler that will take the instance, the function and call its invokeField when invoked
		* one that will actually handle the anonymous functions themselves.
		* one that will transform calling a dynamic function. So for example, dynFunc(arg1, arg2) might turn into dynFunc.apply2(arg1, arg2);
		( suspended ) * an option to match papplied functions
		* handling parameterized anonymous function declaration (optional - tparam_anon_decl and tparam_anon_acc)
*)

let rec cleanup_delegate e = match e.eexpr with
	| TParenthesis e | TMeta(_,e)
	| TCast(e,_) -> cleanup_delegate e
	| _ -> e

let funct gen t = match follow (run_follow gen t) with
	| TFun(args,ret) -> args,ret
	| _ -> raise Not_found

let mk_conversion_fun gen e =
	let args, ret = funct gen e.etype in
	let i = ref 0 in
	let tf_args = List.map (fun (n,o,t) ->
		let n = if n = "" then ("arg" ^ string_of_int(!i)) else n in
		incr i;
		alloc_var n t,None) args
	in
	let block, local = match e.eexpr with
		| TLocal v ->
			add_var_flag v VCaptured;
			[],e
		| _ ->
			let tmp = mk_temp "delegate_conv" e.etype in
			add_var_flag tmp VCaptured;
			[{ eexpr = TVar(tmp,Some e); etype = gen.gcon.basic.tvoid; epos = e.epos }], mk_local tmp e.epos
	in
	let body = {
		eexpr = TCall(local, List.map (fun (v,_) -> mk_local v e.epos) tf_args);
		etype = ret;
		epos = e.epos;
	} in
	let body = if not (ExtType.is_void ret) then
		mk_return body
	else
		body
	in
	let body = {
		eexpr = TBlock([body]);
		etype = body.etype;
		epos = body.epos;
	} in
	block, {
		tf_args = tf_args;
		tf_expr = body;
		tf_type = ret;
	}

let traverse gen ?tparam_anon_decl ?tparam_anon_acc (handle_anon_func:texpr->tfunc->map_info->t option->texpr) (dynamic_func_call:texpr->texpr) e =
	let info = ref null_map_info in
	let rec run e =
		match e.eexpr with
			| TCast({ eexpr = TCall({ eexpr = TIdent "__delegate__" } as local, [del] ) } as e2, _) ->
				let e2 = { e2 with etype = e.etype } in
				let replace_delegate ex =
					{ e with eexpr = TCast({ e2 with eexpr = TCall(local, [ex]) }, None) }
				in
				(* found a delegate; let's see if it's a closure or not *)
				let clean = cleanup_delegate del in
				(match clean.eexpr with
					| TField( ef, (FClosure _ as f)) | TField( ef, (FStatic _ as f)) ->
						(* a closure; let's leave this unchanged for FilterClosures to handle it *)
						replace_delegate { clean with eexpr = TField( run ef, f ) }
					| TFunction tf ->
						(* handle like we'd handle a normal function, but create an unchanged closure field for it *)
						let ret = handle_anon_func clean { tf with tf_expr = run tf.tf_expr } !info (Some e.etype) in
						replace_delegate ret
					| _ -> try
						let block, tf = mk_conversion_fun gen del in
						let block = List.map run block in
						let tf = { tf with tf_expr = run tf.tf_expr } in
						let ret = handle_anon_func { clean with eexpr = TFunction(tf) } { tf with tf_expr = run tf.tf_expr } !info (Some e.etype) in
						let ret = replace_delegate ret in
						if block = [] then
							ret
						else
							{ ret with eexpr = TBlock(block @ [ret]) }
					with Not_found ->
						gen.gcon.error "This delegate construct is unsupported" e.epos;
						replace_delegate (run clean))

			| TCall(({ eexpr = TIdent "__unsafe__" } as local), [arg]) ->
				let old = !info in
				info := { !info with in_unsafe = true };
				let arg2 = run arg in
				info := old;
				{ e with eexpr = TCall(local,[arg2]) }
			(* parameterized functions handling *)
			| TVar(vv, ve) -> (match tparam_anon_decl with
				| None -> Type.map_expr run e
				| Some tparam_anon_decl ->
					(match (vv, ve) with
						| ({ v_extra = Some({v_params = _ :: _}) } as v), Some ({ eexpr = TFunction tf } as f)
						| ({ v_extra = Some({v_params = _ :: _}) } as v), Some { eexpr = TArrayDecl([{ eexpr = TFunction tf } as f]) | TCall({ eexpr = TIdent "__array__" }, [{ eexpr = TFunction tf } as f]) } -> (* captured transformation *)
							tparam_anon_decl v f { tf with tf_expr = run tf.tf_expr };
							{ e with eexpr = TBlock([]) }
						| _ ->
							Type.map_expr run { e with eexpr = TVar(vv, ve) })
					)
			| TBinop(OpAssign, { eexpr = TLocal({ v_extra = Some({v_params = _ :: _}) } as v)}, ({ eexpr= TFunction tf } as f)) when is_some tparam_anon_decl ->
				(match tparam_anon_decl with
					| None -> die "" __LOC__
					| Some tparam_anon_decl ->
						tparam_anon_decl v f { tf with tf_expr = run tf.tf_expr };
						{ e with eexpr = TBlock([]) }
				)
			| TLocal ({ v_extra = Some({v_params =  _ :: _}) } as v) ->
				(match tparam_anon_acc with
				| None -> Type.map_expr run e
				| Some tparam_anon_acc -> tparam_anon_acc v e false)
			| TArray ( ({ eexpr = TLocal ({ v_extra = Some({v_params =  _ :: _}) } as v) } as expr), _) -> (* captured transformation *)
				(match tparam_anon_acc with
				| None -> Type.map_expr run e
				| Some tparam_anon_acc -> tparam_anon_acc v { expr with etype = e.etype } false)
			| TMeta((Meta.Custom ":tparamcall",_,_),({ eexpr=TLocal ({ v_extra = Some({v_params = _ :: _}) } as v) } as expr)) ->
				(match tparam_anon_acc with
				| None -> Type.map_expr run e
				| Some tparam_anon_acc -> tparam_anon_acc v expr true)
			| TCall( { eexpr = TField(_, FEnum _) }, _ ) ->
				Type.map_expr run e
			(* if a TClosure is being call immediately, there's no need to convert it to a TClosure *)
			| TCall(( { eexpr = TField(ecl,f) } as e1), params) ->
				(* check to see if called field is known and if it is a MethNormal (only MethNormal fields can be called directly) *)
				(* let name = field_name f in *)
				(match field_access_esp gen (gen.greal_type ecl.etype) f with
					| FClassField(_,_,_,cf,_,_,_) ->
						(match cf.cf_kind with
							| Method MethNormal
							| Method MethInline ->
								{ e with eexpr = TCall({ e1 with eexpr = TField(run ecl, f) }, List.map run params) }
							| _ ->
								match gen.gfollow#run_f e1.etype with
									| TFun _ ->
										dynamic_func_call { e with eexpr = TCall(run e1, List.map run params) }
									| _ ->
										let i = ref 0 in
										let t = TFun(List.map (fun e -> incr i; "arg" ^ (string_of_int !i), false, e.etype) params, e.etype) in
										dynamic_func_call { e with eexpr = TCall( mk_castfast t (run e1), List.map run params ) }
						)
					(* | FNotFound ->
						{ e with eexpr = TCall({ e1 with eexpr = TField(run ecl, f) }, List.map run params) }
							(* expressions by now may have generated invalid expressions *) *)
					| _ ->
						match gen.gfollow#run_f e1.etype with
							| TFun _ ->
								dynamic_func_call { e with eexpr = TCall(run e1, List.map run params) }
							| _ ->
								let i = ref 0 in
								let t = TFun(List.map (fun e -> incr i; "arg" ^ (string_of_int !i), false, e.etype) params, e.etype) in
								dynamic_func_call { e with eexpr = TCall( mk_castfast t (run e1), List.map run params ) }
				)
			| TFunction tf ->
				handle_anon_func e { tf with tf_expr = run tf.tf_expr } !info None
			| TCall({ eexpr = TConst(TSuper) }, _) ->
				Type.map_expr run e
			| TCall({ eexpr = TIdent s }, args) when String.get s 0 = '_' && Hashtbl.mem gen.gspecial_vars s ->
				Type.map_expr run e
			| TCall(tc,params) ->
				let i = ref 0 in
				let may_cast = match gen.gfollow#run_f tc.etype with
					| TFun _ -> fun e -> e
					| _ ->
						let t = TFun(List.map (fun e ->
								incr i;
								("p" ^ (string_of_int !i), false, e.etype)
							) params, e.etype)
						in
						fun e -> mk_castfast t e
				in
				dynamic_func_call { e with eexpr = TCall(run (may_cast tc), List.map run params) }
			| _ -> Type.map_expr run e
	in

	(match e.eexpr with
		| TFunction(tf) -> Type.map_expr run e
		| _ -> run e)

let rec get_type_params acc t =
	match t with
		| TInst(( { cl_kind = KTypeParameter constraints } as cl), []) ->
			let params = List.fold_left get_type_params acc constraints in
			List.filter (fun t -> not (List.memq t acc)) (cl :: params) @ acc;
		| TFun (params,tret) ->
			List.fold_left get_type_params acc ( tret :: List.map (fun (_,_,t) -> t) params )
		| TDynamic t ->
			(match t with | TDynamic _ -> acc | _ -> get_type_params acc t)
		| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				get_type_params acc ( Abstract.get_underlying_type a pl)
		| TAnon a ->
			PMap.fold (fun cf acc ->
				let params = List.map (fun (_,t,_) -> match follow t with
					| TInst(c,_) -> c
					| _ -> die "" __LOC__) cf.cf_params
				in
				List.filter (fun t -> not (List.memq t params)) (get_type_params acc cf.cf_type)
			) a.a_fields acc
		| TType(_, [])
		| TAbstract (_, [])
		| TInst(_, [])
		| TEnum(_, []) ->
			acc
		| TType(_, params)
		| TAbstract(_, params)
		| TEnum(_, params)
		| TInst(_, params) ->
			List.fold_left get_type_params acc params
		| TMono r -> (match r.tm_type with
			| Some t -> get_type_params acc t
			| None -> acc)
		| _ -> get_type_params acc (follow_once t)

let get_captured expr =
	let ret = Hashtbl.create 1 in
	let ignored = Hashtbl.create 0 in

	let params = ref [] in
	let check_params t = params := get_type_params !params t in
	let rec traverse expr =
		match expr.eexpr with
			| TFor (v, _, _) ->
				Hashtbl.add ignored v.v_id v;
				check_params v.v_type;
				Type.iter traverse expr
			| TFunction(tf) ->
				List.iter (fun (v,_) -> Hashtbl.add ignored v.v_id v) tf.tf_args;
				(match follow expr.etype with
					| TFun(args,ret) ->
						List.iter (fun (_,_,t) ->
							check_params t
						) args;
						check_params ret
					| _ -> ());
				Type.iter traverse expr
			| TVar (v, opt) ->
				(match v.v_extra with
					| Some({v_params = _ :: _}) -> ()
					| _ ->
						check_params v.v_type);
				Hashtbl.add ignored v.v_id v;
				ignore(Option.map traverse opt)
			| TLocal { v_extra = Some({v_params = (_ :: _ )}) } ->
				()
			| TLocal v when has_var_flag v VCaptured ->
				(if not (Hashtbl.mem ignored v.v_id || Hashtbl.mem ret v.v_id) then begin check_params v.v_type; Hashtbl.replace ret v.v_id expr end);
			| _ -> Type.iter traverse expr
	in traverse expr;
	ret, !params

(*
	OPTIMIZEME:

	Take off from Codegen the code that wraps captured variables,

	traverse through all variables, looking for their use (just like local_usage)
	three possible outcomes for captured variables:
		- become a function member variable <- best performance.
			Will not work on functions that can be created more than once (functions inside a loop or functions inside functions)
			The function will have to be created on top of the block, so its variables can be filled in instead of being declared
		- single-element array - the most compatible way, though also creates a slight overhead.
	- we'll have some labels for captured variables:
		- used in loop
*)

(*
	The default implementation will impose a naming convention:
		invoke(arity)_(o for returning object/d for returning double) when arity < max_arity
		invoke_dynamic_(o/d) when arity > max_arity

	This means that it also imposes that the dynamic function return types may only be Dynamic or Float, and all other basic types must be converted to/from it.
*)
let configure gen ft =

	let tvar_to_cdecl = Hashtbl.create 0 in

	let handle_anon_func fexpr ?tvar tfunc mapinfo delegate_type : texpr * (tclass * texpr list) =
		let fexpr = match fexpr.eexpr with
			| TFunction(_) ->
				{ fexpr with eexpr = TFunction(tfunc) }
			| _ ->
				gen.gcon.error "Function expected" fexpr.epos;
				fexpr
		in
		let in_unsafe = mapinfo.in_unsafe || match gen.gcurrent_class, gen.gcurrent_classfield with
			| Some c, _ when Meta.has Meta.Unsafe c.cl_meta -> true
			| _, Some cf when Meta.has Meta.Unsafe cf.cf_meta -> true
			| _ -> false
		in
		(* get all captured variables it uses *)
		let captured_ht, tparams = get_captured fexpr in
		let captured = Hashtbl.fold (fun _ e acc -> e :: acc) captured_ht [] in
		let captured = List.sort (fun e1 e2 -> match e1, e2 with
			| { eexpr = TLocal v1 }, { eexpr = TLocal v2 } ->
				compare v1.v_name v2.v_name
			| _ -> die "" __LOC__) captured
		in

		(*let cltypes = List.map (fun cl -> (snd cl.cl_path, TInst(map_param cl, []) )) tparams in*)
		let cltypes = List.map (fun cl -> (snd cl.cl_path, TInst(cl, []), None )) tparams in

		(* create a new class that extends abstract function class, with a ctor implementation that will setup all captured variables *)
		let cfield = match gen.gcurrent_classfield with
			| None -> "Anon"
			| Some cf -> cf.cf_name
		in
		let cur_line = Lexer.get_error_line fexpr.epos in
		let name = match tvar with
			| None ->
				Printf.sprintf "%s_%s_%d__Fun" (snd gen.gcurrent_path) cfield cur_line
			| Some (v) ->
				Printf.sprintf "%s_%s_%d__Fun" (snd gen.gcurrent_path) v.v_name cur_line
		in
		let path = (fst gen.gcurrent_path, name) in
		let cls = mk_class (get gen.gcurrent_class).cl_module path tfunc.tf_expr.epos in
		if in_unsafe then cls.cl_meta <- (Meta.Unsafe,[],null_pos) :: cls.cl_meta;

		(* forward NativeGen meta for Cs target *)
		if (Common.platform gen.gcon Cs) && not(is_hxgen (TClassDecl (get gen.gcurrent_class))) && Meta.has(Meta.NativeGen) (get gen.gcurrent_class).cl_meta then
			cls.cl_meta <- (Meta.NativeGen,[],null_pos) :: cls.cl_meta;

		if Common.defined gen.gcon Define.EraseGenerics then begin
			cls.cl_meta <- (Meta.HaxeGeneric,[],null_pos) :: cls.cl_meta
		end;
		cls.cl_module <- (get gen.gcurrent_class).cl_module;
		cls.cl_params <- cltypes;

		let mk_this v pos =
			{
				(mk_field_access gen { eexpr = TConst TThis; etype = TInst(cls, extract_param_types cls.cl_params); epos = pos } v.v_name pos)
				with etype = v.v_type
			}
		in

		let mk_this_assign v pos =
		{
			eexpr = TBinop(OpAssign, mk_this v pos, { eexpr = TLocal(v); etype = v.v_type; epos = pos });
			etype = v.v_type;
			epos = pos
		} in

		(* mk_class_field name t public pos kind params *)
		let ctor_args, ctor_sig, ctor_exprs = List.fold_left (fun (ctor_args, ctor_sig, ctor_exprs) lexpr ->
			match lexpr.eexpr with
				| TLocal(v) ->
					let cf = mk_class_field v.v_name v.v_type false lexpr.epos (Var({ v_read = AccNormal; v_write = AccNormal; })) [] in
					cls.cl_fields <- PMap.add v.v_name cf cls.cl_fields;
					cls.cl_ordered_fields <- cf :: cls.cl_ordered_fields;

					let ctor_v = alloc_var v.v_name v.v_type in
					((ctor_v, None) :: ctor_args, (v.v_name, false, v.v_type) :: ctor_sig, (mk_this_assign v cls.cl_pos) :: ctor_exprs)
				| _ -> die "" __LOC__
		) ([],[],[]) captured in

		(* change all captured variables to this.capturedVariable *)
		let rec change_captured e =
			match e.eexpr with
				| TLocal v when has_var_flag v VCaptured && Hashtbl.mem captured_ht v.v_id ->
					mk_this v e.epos
				| _ -> Type.map_expr change_captured e
		in
		let func_expr = change_captured tfunc.tf_expr in

		let invokecf, invoke_field, super_args = match delegate_type with
			| None -> (* no delegate *)
				let ifield, sa = ft.closure_to_classfield { tfunc with tf_expr = func_expr } fexpr.etype fexpr.epos in
				ifield,ifield,sa
			| Some _ ->
				let pos = cls.cl_pos in
				let cf = mk_class_field "Delegate" (TFun(fun_args tfunc.tf_args, tfunc.tf_type)) true pos (Method MethNormal) [] in
				cf.cf_expr <- Some { fexpr with eexpr = TFunction { tfunc with tf_expr = func_expr }; };
				add_class_field_flag cf CfFinal;
				cls.cl_ordered_fields <- cf :: cls.cl_ordered_fields;
				cls.cl_fields <- PMap.add cf.cf_name cf cls.cl_fields;
				(* invoke function body: call Delegate function *)
				let ibody = {
					eexpr = TCall({
						eexpr = TField({
							eexpr = TConst TThis;
							etype = TInst(cls, extract_param_types cls.cl_params);
							epos = pos;
						}, FInstance(cls, extract_param_types cls.cl_params, cf));
						etype = cf.cf_type;
						epos = pos;
					}, List.map (fun (v,_) -> mk_local v pos) tfunc.tf_args);
					etype = tfunc.tf_type;
					epos = pos
				} in
				let ibody = if not (ExtType.is_void tfunc.tf_type) then
					mk_return ibody
				else
					ibody
				in
				let ifield, sa = ft.closure_to_classfield { tfunc with tf_expr = ibody } fexpr.etype fexpr.epos in
				cf,ifield,sa
		in

		(* create the constructor *)
		(* todo properly abstract how type var is set *)

		cls.cl_super <- Some(ft.func_class, []);
		let pos = cls.cl_pos in
		let super_call =
		{
			eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(ft.func_class,[]); epos = pos }, super_args);
			etype = gen.gcon.basic.tvoid;
			epos = pos;
		} in

		let ctor_type = (TFun(ctor_sig, gen.gcon.basic.tvoid)) in
		let ctor = mk_class_field "new" ctor_type true cls.cl_pos (Method(MethNormal)) [] in
		ctor.cf_expr <- Some(
		{
			eexpr = TFunction(
			{
				tf_args = ctor_args;
				tf_type = gen.gcon.basic.tvoid;
				tf_expr = { eexpr = TBlock(super_call :: ctor_exprs); etype = gen.gcon.basic.tvoid; epos = cls.cl_pos }
			});
			etype = ctor_type;
			epos = cls.cl_pos;
		});
		cls.cl_constructor <- Some(ctor);

		(* add invoke function to the class *)
		cls.cl_ordered_fields <- invoke_field :: cls.cl_ordered_fields;
		cls.cl_fields <- PMap.add invoke_field.cf_name invoke_field cls.cl_fields;
		add_class_field_flag invoke_field CfOverride;

		(match tvar with
		| None -> ()
		| Some ({ v_extra = Some({v_params = _ :: _}) } as v) ->
			Hashtbl.add tvar_to_cdecl v.v_id (cls,captured)
		| _ -> ());

		(* set priority as priority + 0.00001 so that this filter runs again *)
		gen.gadd_to_module (TClassDecl cls) (priority +. 0.000001);

		(* if there are no captured variables, we can create a cache so subsequent calls don't need to create a new function *)
		let expr, clscapt =
			match captured, tparams with
			| [], [] ->
				let cache_var = mk_internal_name "hx" "current" in
				let cache_cf = mk_class_field ~static:true cache_var (TInst(cls,[])) false func_expr.epos (Var({ v_read = AccNormal; v_write = AccNormal })) [] in
				cls.cl_ordered_statics <- cache_cf :: cls.cl_ordered_statics;
				cls.cl_statics <- PMap.add cache_var cache_cf cls.cl_statics;

				(* if (FuncClass.hx_current != null) FuncClass.hx_current; else (FuncClass.hx_current = new FuncClass()); *)

				(* let mk_static_field_access cl field fieldt pos = *)
				let hx_current = mk_static_field_access cls cache_var (TInst(cls,[])) func_expr.epos in

				let pos = func_expr.epos in
				{ fexpr with
					etype = hx_current.etype;
					eexpr = TIf(
						{
							eexpr = TBinop(OpNotEq, hx_current, null (TInst(cls,[])) pos);
							etype = gen.gcon.basic.tbool;
							epos = pos;
						},
						hx_current,
						Some(
						{
							eexpr = TBinop(OpAssign, hx_current, { fexpr with eexpr = TNew(cls, [], captured) });
							etype = (TInst(cls,[]));
							epos = pos;
						}))
				}, (cls,captured)
			| _ ->
				(* change the expression so it will be a new "added class" ( captured variables arguments ) *)
				{ fexpr with eexpr = TNew(cls, List.map (fun cl -> TInst(cl,[])) tparams, List.rev captured) }, (cls,captured)
		in
		match delegate_type with
		| None ->
			expr,clscapt
		| Some _ ->
			{
				eexpr = TField(expr, FClosure(Some (cls,[]),invokecf)); (* TODO: FClosure change *)
				etype = invokecf.cf_type;
				epos = cls.cl_pos
			}, clscapt
	in


	let run = traverse
		gen
		~tparam_anon_decl:(fun v e fn ->
			let _, (cls,captured) = handle_anon_func e ~tvar:v fn null_map_info None in
			Hashtbl.add tvar_to_cdecl v.v_id (cls,captured)
		)
		~tparam_anon_acc:(fun v e in_tparam -> try
			let cls, captured = Hashtbl.find tvar_to_cdecl v.v_id in
			let captured = List.sort (fun e1 e2 -> match e1, e2 with
				| { eexpr = TLocal v1 }, { eexpr = TLocal v2 } ->
					compare v1.v_name v2.v_name
				| _ -> die "" __LOC__) captured
			in
			let types = match v.v_extra with
				| Some ve -> ve.v_params
				| _ -> die "" __LOC__
			in
			let monos = List.map (fun _ -> mk_mono()) types in
			let vt = match follow v.v_type with
				| TInst(_, [v]) -> v
				| v -> v
			in
			let et = match follow e.etype with
				| TInst(_, [v]) -> v
				| v -> v
			in
			let original = apply_params types monos vt in
			unify et original;

			let monos = List.map (fun t -> apply_params types (List.map (fun _ -> t_dynamic) types) t) monos in

			let same_cl t1 t2 = match follow t1, follow t2 with
				| TInst(c,_), TInst(c2,_) -> c == c2
				| _ -> false
			in
			let passoc = List.map2 (fun (_,t,_) m -> t,m) types monos in
			let cltparams = List.map (fun (_,t,_) ->
				try
					snd (List.find (fun (t2,_) -> same_cl t t2) passoc)
				with | Not_found -> t) cls.cl_params
			in
			{ e with eexpr = TNew(cls, cltparams, List.rev captured) }
		with
			| Not_found ->
				if in_tparam then begin
					gen.gcon.warning "This expression may be invalid" e.epos;
					e
				end else
					(* It is possible that we are recursively calling a function
					   that has type parameters. In this case, we must leave it be
					   because as soon as the new class is added to the module,
					   this filter will run again. By this time, the tvar-to-cdecl
					   hashtable will be already filled with all functions, so
					   it should run correctly. In this case, if it keeps failing.
					   we will add the "Expression may be invalid warning" like we did
					   before (see Issue #7118) *)
					{ e with eexpr = TMeta(
						(Meta.Custom(":tparamcall"), [], e.epos), e
					) }
			| Unify_error el ->
				List.iter (fun el -> gen.gcon.warning (Error.unify_error_msg (print_context()) el) e.epos) el;
				gen.gcon.warning "This expression may be invalid" e.epos;
				e
		)
		(* (handle_anon_func:texpr->tfunc->texpr) (dynamic_func_call:texpr->texpr->texpr list->texpr) *)
		(fun e f info delegate_type -> fst (handle_anon_func e f info delegate_type))
		ft.dynamic_fun_call
		(* (dynamic_func_call:texpr->texpr->texpr list->texpr) *)
	in
	gen.gexpr_filters#add name (PCustom priority) run

(*
	this submodule will provide the default implementation for the C# and Java targets.

	it will have two return types: double and dynamic, and
*)
module DoubleAndDynamicClosureImpl =
struct
	let get_ctx gen parent_func_class max_arity mk_arg_exception (* e.g. new haxe.lang.ClassClosure *) =
		let basic = gen.gcon.basic in

		let func_args_i i =
			let rec loop i (acc) =
				if i = 0 then (acc) else begin
					let vfloat = alloc_var (mk_internal_name "fn" ("float" ^ string_of_int i)) basic.tfloat in
					let vdyn = alloc_var (mk_internal_name "fn" ("dyn" ^ string_of_int i)) t_dynamic in

					loop (i - 1) ((vfloat, None) :: (vdyn, None) :: acc)
				end
			in
			loop i []
		in

		let args_real_to_func args =
			let arity = List.length args in
			if arity >= max_arity then
				[ alloc_var (mk_internal_name "fn" "dynargs") (gen.gclasses.nativearray t_dynamic), None ]
			else func_args_i arity
		in

		let func_sig_i i =
			let rec loop i acc =
				if i = 0 then acc else begin
					let vfloat = mk_internal_name "fn" ("float" ^ string_of_int i) in
					let vdyn = mk_internal_name "fn" ("dyn" ^ string_of_int i) in

					loop (i - 1) ( (vfloat,false,basic.tfloat) :: (vdyn,false,t_dynamic) :: acc )
				end
			in
			loop i []
		in

		let args_real_to_func_sig args =
			let arity = List.length args in
			if arity >= max_arity then
				[mk_internal_name "fn" "dynargs", false, gen.gclasses.nativearray t_dynamic]
			else begin
				func_sig_i arity
			end
		in

		let rettype_real_to_func t = match run_follow gen t with
			| TAbstract({ a_path = [],"Null" }, _) ->
				0,t_dynamic
			| _ when like_float t && not (like_i64 t) ->
				(1, basic.tfloat)
			| _ ->
				(0, t_dynamic)
		in

		let args_real_to_func_call el (pos:pos) =
			if List.length el >= max_arity then
				[mk_nativearray_decl gen t_dynamic el pos]
			else begin
				List.fold_left (fun acc e ->
					if like_float (gen.greal_type e.etype) && not (like_i64 (gen.greal_type e.etype)) then
						( e :: undefined e.epos :: acc )
					else
						( null basic.tfloat e.epos :: e :: acc )
				) ([]) (List.rev el)
			end
		in

		let get_args_func args changed_args pos =
			let arity = List.length args in
			let mk_const const elocal t =
				match const with
				| None ->
					mk_cast t elocal
				| Some const ->
					{ eexpr = TIf(
						{ elocal with eexpr = TBinop(Ast.OpEq, elocal, null elocal.etype elocal.epos); etype = basic.tbool },
						const,
						Some ( mk_cast t elocal )
					); etype = t; epos = elocal.epos }
			in

			if arity >= max_arity then begin
				let varray = match changed_args with | [v,_] -> v | _ -> die "" __LOC__ in
				let varray_local = mk_local varray pos in
				let mk_varray i = { eexpr = TArray(varray_local, make_int gen.gcon.basic i pos); etype = t_dynamic; epos = pos } in
				let el =
					snd (List.fold_left (fun (count,acc) (v,const) ->
						(count + 1, (mk (TVar(v, Some(mk_const const (mk_varray count) v.v_type))) basic.tvoid pos) :: acc)
					) (0, []) args)
				in
				List.rev el
			end else begin
				let _, dyn_args, float_args = List.fold_left (fun (count,fargs, dargs) arg ->
					if count land 1 = 0 then
						(count + 1, fargs, arg :: dargs)
					else
						(count + 1, arg :: fargs, dargs)
				) (1,[],[]) (List.rev changed_args) in

				let rec loop acc args fargs dargs =
					match args, fargs, dargs with
						| [], [], [] -> acc
						| (v,const) :: args, (vf,_) :: fargs, (vd,_) :: dargs ->
							let acc = { eexpr = TVar(v, Some(
								{
									eexpr = TIf(
										{ eexpr = TBinop(Ast.OpEq, mk_local vd pos, undefined pos); etype = basic.tbool; epos = pos },
										mk_cast v.v_type (mk_local vf pos),
										Some ( mk_const const (mk_local vd pos) v.v_type )
									);
									etype = v.v_type;
									epos = pos
								} )); etype = basic.tvoid; epos = pos } :: acc in
							loop acc args fargs dargs
						| _ -> die "" __LOC__
				in

				loop [] args float_args dyn_args
			end
		in

		let closure_to_classfield tfunc old_sig pos =
			(* change function signature *)
			let old_args = tfunc.tf_args in
			let changed_args = args_real_to_func old_args in

			(*
				FIXME properly handle int64 cases, which will break here (because of inference to int)
				UPDATE: the fix will be that Int64 won't be a typedef to Float/Int
			*)
			let changed_sig, arity, type_number, changed_sig_ret, is_void, is_dynamic_func = match follow old_sig with
				| TFun(_sig, ret) ->
					let type_n, ret_t = rettype_real_to_func ret in
					let arity = List.length _sig in
					let is_dynamic_func = arity >= max_arity in
					let ret_t = if is_dynamic_func then t_dynamic else ret_t in

					(TFun(args_real_to_func_sig _sig, ret_t), arity, type_n, ret_t, ExtType.is_void ret, is_dynamic_func)
				| _ -> (print_endline (s_type (print_context()) (follow old_sig) )); die "" __LOC__
			in

			let tf_expr = if is_void then begin
				let rec map e =
					match e.eexpr with
						| TReturn None ->
							mk_return (null t_dynamic e.epos)
						| _ -> Type.map_expr map e
				in
				let e = mk_block (map tfunc.tf_expr) in
				match e.eexpr with
					| TBlock bl -> { e with eexpr = TBlock (bl @ [mk_return (null t_dynamic e.epos)]) }
					| _ -> die "" __LOC__
			end else tfunc.tf_expr in

			let changed_sig_ret = if is_dynamic_func then t_dynamic else changed_sig_ret in

			(* get real arguments on top of function body *)
			let get_args = get_args_func tfunc.tf_args changed_args pos in
			(*
				FIXME HACK: in order to be able to run the filters that have already ran for this piece of code,
				we will cheat and run it as if it was the whole code
				We could just make ClosuresToClass run before TArrayTransform, but we cannot because of the
				dependency between ClosuresToClass (after DynamicFieldAccess, and before TArrayTransform)

				maybe a way to solve this would be to add an "until" field to run_from
			*)
			let real_get_args = gen.gexpr_filters#run (mk (TBlock get_args) basic.tvoid pos) in

			let func_expr = Type.concat real_get_args tf_expr in

			(* set invoke function *)
			(* todo properly abstract how naming for invoke is made *)
			let invoke_name = if is_dynamic_func then "invokeDynamic" else ("invoke" ^ (string_of_int arity) ^ (if type_number = 0 then "_o" else "_f")) in
			let invoke_name = mk_internal_name "hx" invoke_name in
			let invoke_field = mk_class_field invoke_name changed_sig false func_expr.epos (Method(MethNormal)) [] in
			let invoke_fun = {
				eexpr = TFunction {
					tf_args = changed_args;
					tf_type = changed_sig_ret;
					tf_expr = func_expr;
				};
				etype = changed_sig;
				epos = func_expr.epos;
			} in
			invoke_field.cf_expr <- Some invoke_fun;

			invoke_field, [
				make_int gen.gcon.basic arity pos;
				make_int gen.gcon.basic type_number pos;
			]
		in

		let dynamic_fun_call call_expr =
			let tc, params = match call_expr.eexpr with
				| TCall(tc, params) -> tc,wrap_rest_args gen tc.etype params tc.epos
				| _ -> die "" __LOC__
			in
			let ct = gen.greal_type call_expr.etype in
			let postfix, ret_t =
				if like_float ct && not (like_i64 ct) then
						"_f", gen.gcon.basic.tfloat
				else
					"_o", t_dynamic
			in
			let params_len = List.length params in
			let ret_t = if params_len >= max_arity then t_dynamic else ret_t in

			let invoke_fun = if params_len >= max_arity then "invokeDynamic" else "invoke" ^ (string_of_int params_len) ^ postfix in
			let invoke_fun = mk_internal_name "hx" invoke_fun in
			let fun_t = match follow tc.etype with
				| TFun(_sig, _) ->
					TFun(args_real_to_func_sig _sig, ret_t)
				| _ ->
					let i = ref 0 in
					let _sig = List.map (fun p -> let name = "arg" ^ (string_of_int !i) in incr i; (name,false,p.etype) ) params in
					TFun(args_real_to_func_sig _sig, ret_t)
			in

			let may_cast = match follow call_expr.etype with
				| TAbstract ({ a_path = ([], "Void") },[]) -> (fun e -> e)
				| _ -> mk_cast call_expr.etype
			in

			may_cast
			{
				eexpr = TCall(
					{ (mk_field_access gen { tc with etype = gen.greal_type tc.etype } invoke_fun tc.epos) with etype = fun_t },
					args_real_to_func_call params call_expr.epos
				);
				etype = ret_t;
				epos = call_expr.epos
			}
		in

		let iname i is_float =
			let postfix = if is_float then "_f" else "_o" in
			mk_internal_name "hx" ("invoke" ^ string_of_int i) ^ postfix
		in

		let map_base_classfields cl map_fn =
			let pos = cl.cl_pos in
			let this_t = TInst(cl,extract_param_types cl.cl_params) in
			let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
			let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

			let mk_invoke_i i is_float =
				let cf = mk_class_field (iname i is_float) (TFun(func_sig_i i, if is_float then basic.tfloat else t_dynamic)) false pos (Method MethNormal) [] in
				cf
			in

			let type_name = mk_internal_name "fn" "type" in
			let dynamic_arg = alloc_var (mk_internal_name "fn" "dynargs") (gen.gclasses.nativearray t_dynamic) in

			let mk_invoke_complete_i i is_float =

				(* let arity = i in *)
				let args = func_args_i i in

				(* api fn *)

				(* only cast if needed *)
				let mk_cast tto efrom = gen.ghandle_cast (gen.greal_type tto) (gen.greal_type efrom.etype) efrom in
				let api i t const =
					let vf, _ = List.nth args (i * 2) in
					let vo, _ = List.nth args (i * 2 + 1) in

					let needs_cast, is_float = match t, like_float t && not (like_i64 t) with
						| TAbstract({ a_path = ([], "Float") },[]), _ -> false, true
						| _, true -> true, true
						| _ -> false,false
					in

					let olocal = mk_local vo pos in
					let flocal = mk_local vf pos in

					let get_from_obj e = match const with
						| None -> mk_cast t e
						| Some tc ->
							{
								eexpr = TIf(
									{ eexpr = TBinop(Ast.OpEq, olocal, null t_dynamic pos); etype = basic.tbool; epos = pos } ,
									{ eexpr = TConst(tc); etype = t; epos = pos },
									Some (mk_cast t e)
								);
								etype = t;
								epos = pos;
							}
					in

					{
						eexpr = TIf(
							{ eexpr = TBinop(Ast.OpEq, olocal, undefined pos); etype = basic.tbool; epos = pos },
							(if needs_cast then mk_cast t flocal else flocal),
							Some ( get_from_obj olocal )
						);
						etype = t;
						epos = pos
					}
				in
				(* end of api fn *)

				let ret = if is_float then basic.tfloat else t_dynamic in

				let fn_expr = map_fn i ret (List.map fst args) api in

				let t = TFun(fun_args args, ret) in

				let tfunction =
					{
						eexpr = TFunction({
							tf_args = args;
							tf_type = ret;
							tf_expr =
							mk_block fn_expr
						});
						etype = t;
						epos = pos;
					}
				in

				let cf = mk_invoke_i i is_float in
				cf.cf_expr <- Some tfunction;
				cf
			in

			let rec loop i cfs =
				if i < 0 then cfs else begin
					(*let mk_invoke_complete_i i is_float =*)
					(mk_invoke_complete_i i false) :: (mk_invoke_complete_i i true) :: (loop (i-1) cfs)
				end
			in

			let cfs = loop max_arity [] in

			let switch =
				let api i t const =
					match i with
						| -1 ->
							mk_local dynamic_arg pos
						| _ ->
							mk_cast t {
								eexpr = TArray(
									mk_local dynamic_arg pos,
									{ eexpr = TConst(TInt(Int32.of_int i)); etype = basic.tint; epos = pos });
								etype = t;
								epos = pos;
							}
				in
				map_fn (-1) t_dynamic [dynamic_arg] api
			in

			let args = [dynamic_arg, None] in
			let dyn_t = TFun(fun_args args, t_dynamic) in
			let dyn_cf = mk_class_field (mk_internal_name "hx" "invokeDynamic") dyn_t false pos (Method MethNormal) [] in

			dyn_cf.cf_expr <- Some {
				eexpr = TFunction {
					tf_args = args;
					tf_type = t_dynamic;
					tf_expr = mk_block switch
				};
				etype = dyn_t;
				epos = pos;
			};

			let additional_cfs = begin
				let new_t = TFun(["arity", false, basic.tint; "type", false, basic.tint],basic.tvoid) in
				let new_cf = mk_class_field "new" (new_t) true pos (Method MethNormal) [] in
				let v_arity, v_type = alloc_var "arity" basic.tint, alloc_var "type" basic.tint in
				let mk_assign v field = mk (TBinop (OpAssign, mk_this field v.v_type, mk_local v pos)) v.v_type pos in

				let arity_name = mk_internal_name "hx" "arity" in
				new_cf.cf_expr <- Some {
					eexpr = TFunction({
						tf_args = [v_arity, None; v_type, None];
						tf_type = basic.tvoid;
						tf_expr =
						{
							eexpr = TBlock([
								mk_assign v_type type_name;
								mk_assign v_arity arity_name
							]);
							etype = basic.tvoid;
							epos = pos;
						}
					});
					etype = new_t;
					epos = pos;
				};

				[
					new_cf;
					mk_class_field type_name basic.tint true pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
					mk_class_field arity_name basic.tint true pos (Var { v_read = AccNormal; v_write = AccNormal }) [];
				]
			end in

			dyn_cf :: (additional_cfs @ cfs)
		in

		begin
			(*
				setup fields for the abstract implementation of the Function class

				new(arity, type)
				{
					this.arity = arity;
					this.type = type;
				}

				hx::invokeX_f|o (where X is from 0 to max_arity) (args)
				{
					if (this.type == 0|1) return invokeX_o|f(args); else throw "Invalid number of arguments."
				}

				hx::invokeDynamic, which will work in the same way
			*)
			let cl = parent_func_class in
			let pos = cl.cl_pos in

			let rec mk_dyn_call arity api =
				let zero = make_float gen.gcon.basic "0.0" pos in
				let rec loop i acc =
					if i = 0 then
						acc
					else begin
						let arr = api (i - 1) t_dynamic None in
						loop (i - 1) (zero :: arr :: acc)
					end
				in
				loop arity []
			in

			let this = mk (TConst TThis) (TInst (cl, extract_param_types cl.cl_params)) pos in
			let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

			let mk_invoke_switch i api =
				let t = TFun (func_sig_i i, t_dynamic) in
				(* case i: return this.invokeX_o(0, 0, 0, 0, 0, ... arg[0], args[1]....); *)
				[make_int gen.gcon.basic i pos], mk_return (mk (TCall(mk_this (iname i false) t, mk_dyn_call i api)) t_dynamic pos)
			in
			let rec loop_cases api arity acc =
				if arity < 0 then
					acc
				else
					loop_cases api (arity - 1) (mk_invoke_switch arity api :: acc)
			in

			let type_name = mk_internal_name "fn" "type" in
			let mk_expr i is_float vars =
				let call_expr =
					let call_t = TFun(List.map (fun v -> (v.v_name, false, v.v_type)) vars, if is_float then t_dynamic else basic.tfloat) in
					{
						eexpr = TCall(mk_this (iname i (not is_float)) call_t, List.map (fun v -> mk_local v pos) vars);
						etype = if is_float then t_dynamic else basic.tfloat;
						epos = pos
					}
				in
				{
					eexpr = TIf(
						mk (TBinop (Ast.OpNotEq, mk_this type_name basic.tint, (make_int gen.gcon.basic (if is_float then 0 else 1) pos))) basic.tbool pos,
						make_throw (mk_arg_exception "Wrong number of arguments" pos) pos,
						Some (mk_return call_expr)
					);
					etype = t_dynamic;
					epos = pos;
				}
			in

			let arities_processed = Hashtbl.create 10 in
			let max_arity = ref 0 in

			let map_fn cur_arity fun_ret_type vars (api:int->t->tconstant option->texpr) =
				let is_float = like_float fun_ret_type && not (like_i64 fun_ret_type) in
				match cur_arity with
				| -1 ->
					let dynargs = api (-1) t_dynamic None in

					(* (dynargs == null) ? 0 : dynargs.length *)
					let switch_cond = {
						eexpr = TIf(
							mk (TBinop (OpEq, dynargs, null dynargs.etype pos)) basic.tbool pos,
							mk (TConst (TInt Int32.zero)) basic.tint pos,
							Some (gen.gclasses.nativearray_len dynargs pos));
						etype = basic.tint;
						epos = pos;
					} in

					{
						eexpr = TSwitch(
							switch_cond,
							loop_cases api !max_arity [],
							Some(make_throw (mk_arg_exception "Too many arguments" pos) pos));
						etype = basic.tvoid;
						epos = pos;
					}
				| _ ->
					if not (Hashtbl.mem arities_processed cur_arity) then begin
						Hashtbl.add arities_processed cur_arity true;
						if cur_arity > !max_arity then max_arity := cur_arity
					end;

					mk_expr cur_arity is_float vars
			in

			let cfs = map_base_classfields cl map_fn in
			List.iter (fun cf ->
				if cf.cf_name = "new" then
					parent_func_class.cl_constructor <- Some cf
				else
					parent_func_class.cl_fields <- PMap.add cf.cf_name cf parent_func_class.cl_fields
			) cfs;
			parent_func_class.cl_ordered_fields <- (List.filter (fun cf -> cf.cf_name <> "new") cfs) @ parent_func_class.cl_ordered_fields
		end;

		{
			func_class = parent_func_class;
			closure_to_classfield = closure_to_classfield;
			dynamic_fun_call = dynamic_fun_call;
			map_base_classfields = map_base_classfields;
		}
end;;
