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
open Type
open Gencommon

(* ******************************************* *)
(* FixOverrides *)
(* ******************************************* *)
(*

	Covariant return types, contravariant function arguments and applied type parameters may change
	in a way that expected implementations / overrides aren't recognized as such.
	This filter will fix that.

	dependencies:
		FixOverrides expects that the target platform is able to deal with overloaded functions
		It must run after DefaultArguments, otherwise code added by the default arguments may be invalid

*)
let name = "fix_overrides"
let priority = solve_deps name []

(*
	if the platform allows explicit interface implementation (C#),
	specify a explicit_fn_name function (tclass->string->string)
	Otherwise, it expects the platform to be able to handle covariant return types
*)
let run ~explicit_fn_name ~get_vmtype gen =
	let implement_explicitly = is_some explicit_fn_name in
	let run md = match md with
		| TClassDecl c when (has_class_flag c CInterface) && not (has_class_flag c CExtern) ->
			(* overrides can be removed from interfaces *)
			c.cl_ordered_fields <- List.filter (fun f ->
				try
					if has_class_field_flag f CfOverload then raise Not_found;
					let f2 = Codegen.find_field gen.gcon c f in
					if f2 == f then raise Not_found;
					c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
					false;
				with Not_found ->
					true
			) c.cl_ordered_fields;
			md
		| TClassDecl c when not (has_class_flag c CExtern) ->
			let this = { eexpr = TConst TThis; etype = TInst(c,extract_param_types c.cl_params); epos = c.cl_pos } in
			(* look through all interfaces, and try to find a type that applies exactly *)
			let rec loop_iface (iface:tclass) itl =
				List.iter (fun (s,stl) -> loop_iface s (List.map (apply_params iface.cl_params itl) stl)) iface.cl_implements;
				let real_itl = gen.greal_type_param (TClassDecl iface) itl in
				let rec loop_f f =
					List.iter loop_f f.cf_overloads;
					let ftype = apply_params iface.cl_params itl f.cf_type in
					let real_ftype = get_real_fun gen (apply_params iface.cl_params real_itl f.cf_type) in
					replace_mono real_ftype;
					let overloads = Overloads.collect_overloads (fun t -> t) c f.cf_name in
					try
						let t2, f2 =
							match overloads with
							| (_, cf) :: _ when has_class_field_flag cf CfOverload -> (* overloaded function *)
								(* try to find exact function *)
								List.find (fun (t,f2) ->
									Overloads.same_overload_args ~get_vmtype ftype t f f2
								) overloads
							| _ :: _ ->
								(match field_access gen (TInst(c, extract_param_types c.cl_params)) f.cf_name with
								| FClassField(_,_,_,f2,false,t,_) -> t,f2 (* if it's not an overload, all functions should have the same signature *)
								| _ -> raise Not_found)
							| [] -> raise Not_found
						in
						replace_mono t2;
						(* if we find a function with the exact type of real_ftype, it means this interface has already been taken care of *)
						if not (type_iseq (get_real_fun gen (apply_params f2.cf_params (extract_param_types f.cf_params) t2)) real_ftype) then begin
							(match f.cf_kind with | Method (MethNormal | MethInline) -> () | _ -> raise Not_found);
							let t2 = get_real_fun gen t2 in
							if List.length f.cf_params <> List.length f2.cf_params then raise Not_found;
							replace_mono t2;
							match follow (apply_params f2.cf_params (extract_param_types f.cf_params) t2), follow real_ftype with
							| TFun(a1,r1), TFun(a2,r2) when not implement_explicitly && not (type_iseq r1 r2) && Overloads.same_overload_args ~get_vmtype real_ftype t2 f f2 ->
								(* different return types are the trickiest cases to deal with *)
								(* check for covariant return type *)
								let is_covariant = match follow r1, follow r2 with
									| _, TDynamic _ -> false
									| r1, r2 -> try
										unify r1 r2;
										if like_int r1 then like_int r2 else true
									with | Unify_error _ -> false
								in
								(* we only have to worry about non-covariant issues *)
								if not is_covariant then begin
									(* override return type and cast implemented function *)
									let args, newr = match follow t2, follow (apply_params f.cf_params (extract_param_types f2.cf_params) real_ftype) with
										| TFun(a,_), TFun(_,r) -> a,r
										| _ -> Globals.die "" __LOC__
									in
									f2.cf_type <- TFun(args,newr);
									(match f2.cf_expr with
									| Some ({ eexpr = TFunction tf } as e) ->
											f2.cf_expr <- Some { e with eexpr = TFunction { tf with tf_type = newr } }
									| _ -> ())
								end
							| TFun(a1,r1), TFun(a2,r2) ->
								(* just implement a function that will call the main one *)
								let name, is_explicit = match explicit_fn_name with
									| Some fn when not (type_iseq r1 r2) && Overloads.same_overload_args ~get_vmtype real_ftype t2 f f2 ->
											fn iface itl f.cf_name, true
									| _ -> f.cf_name, false
								in
								let p = f2.cf_pos in
								let newf = mk_class_field name real_ftype true f.cf_pos (Method MethNormal) f.cf_params in
								(* make sure that there isn't already an overload with the same exact type *)
								if List.exists (fun (t,f2) ->
									type_iseq (get_real_fun gen t) real_ftype
								) overloads then raise Not_found;
								let vars = List.map (fun (n,_,t) -> alloc_var n t) a2 in

								let args = List.map2 (fun v (_,_,t) -> mk_cast t (mk_local v f2.cf_pos)) vars a1 in
								let field = { eexpr = TField(this, FInstance(c,extract_param_types c.cl_params,f2)); etype = TFun(a1,r1); epos = p } in
								let call = { eexpr = TCall(field, args); etype = r1; epos = p } in
								(* let call = gen.gparam_func_call call field (List.map snd f.cf_params) args in *)
								let is_void = ExtType.is_void r2 in

								newf.cf_expr <- Some {
									eexpr = TFunction({
										tf_args = List.map (fun v -> v,None) vars;
										tf_type = r2;
										tf_expr = if is_void then call else (Texpr.Builder.mk_return (mk_cast r2 call));
									});
									etype = real_ftype;
									epos = p;
								};
								(try
									let fm = PMap.find name c.cl_fields in
									fm.cf_overloads <- newf :: fm.cf_overloads
								with | Not_found ->
									c.cl_fields <- PMap.add name newf c.cl_fields;
									c.cl_ordered_fields <- newf :: c.cl_ordered_fields)
							| _ -> Globals.die "" __LOC__
						end
					with | Not_found -> ()
				in
				List.iter (fun f -> match f.cf_kind with | Var _ -> () | _ -> loop_f f) iface.cl_ordered_fields
			in
			List.iter (fun (iface,itl) -> loop_iface iface itl) c.cl_implements;
			(* now go through all overrides, *)
			let check_f f =
				(* find the first declared field *)
				let is_overload = has_class_field_flag f CfOverload in
				let decl = if is_overload then
					find_first_declared_field gen c ~get_vmtype ~exact_field:f f.cf_name
				else
					find_first_declared_field gen c ~get_vmtype f.cf_name
				in
				match decl with
				| Some(f2,actual_t,_,t,declared_cl,_,_)
					when not (Overloads.same_overload_args ~get_vmtype actual_t (get_real_fun gen f.cf_type) f2 f) ->
						(match f.cf_expr with
						| Some({ eexpr = TFunction(tf) } as e) ->
							let actual_args, _ = get_fun (get_real_fun gen actual_t) in
							let new_args, vars_to_declare = List.fold_left2 (fun (args,vdecl) (v,_) (_,_,t) ->
								if not (type_iseq (gen.greal_type v.v_type) (gen.greal_type t)) then begin
									let new_var = mk_temp v.v_name t in
									(new_var,None) :: args, (v, Some(mk_cast v.v_type (mk_local new_var f.cf_pos))) :: vdecl
								end else
									(v,None) :: args, vdecl
							) ([],[]) tf.tf_args actual_args in
							let block = { eexpr = TBlock(List.map (fun (v,ve) ->
								{
									eexpr = TVar(v,ve);
									etype = gen.gcon.basic.tvoid;
									epos = tf.tf_expr.epos
								}) vars_to_declare);
								etype = gen.gcon.basic.tvoid;
								epos = tf.tf_expr.epos
							} in
							let has_contravariant_args = match (get_real_fun gen f.cf_type, actual_t) with
								| TFun(current_args,_), TFun(original_args,_) ->
										List.exists2 (fun (_,_,cur_arg) (_,_,orig_arg) -> try
											unify orig_arg cur_arg;
											try
												unify cur_arg orig_arg;
												false
											with Unify_error _ ->
												true
										with Unify_error _ -> false) current_args original_args
								| _ -> Globals.die "" __LOC__
							in
							if (not (has_class_field_flag f CfOverload) && has_contravariant_args) then
								add_class_field_flag f CfOverload;
							if has_class_field_flag f CfOverload then begin
								(* if it is overload, create another field with the requested type *)
								let f3 = mk_class_field f.cf_name t (has_class_field_flag f CfPublic) f.cf_pos f.cf_kind f.cf_params in
								let p = f.cf_pos in
								let old_args, old_ret = get_fun f.cf_type in
								let args, ret = get_fun t in
								let tf_args = List.rev new_args in
								let f3_mk_return = if ExtType.is_void ret then (fun e -> e) else (fun e -> Texpr.Builder.mk_return (mk_cast ret e)) in
								f3.cf_expr <- Some {
									eexpr = TFunction({
										tf_args = tf_args;
										tf_type = ret;
										tf_expr = Type.concat block (mk_block (f3_mk_return {
											eexpr = TCall(
												{
													eexpr = TField(
														{ eexpr = TConst TThis; etype = TInst(c, extract_param_types c.cl_params); epos = p },
														FInstance(c,extract_param_types c.cl_params,f));
													etype = f.cf_type;
													epos = p
												},
												List.map2 (fun (v,_) (_,_,t) -> mk_cast t (mk_local v p)) tf_args old_args);
											etype = old_ret;
											epos = p
										}))
									});
									etype = t;
									epos = p;
								};
								(* make sure we skip cast detect - otherwise this new function will make the overload detection go crazy *)
								f3.cf_meta <- (Meta.Custom(":skipCastDetect"), [], f3.cf_pos) :: f3.cf_meta;
								gen.gafter_expr_filters_ended <- ((fun () ->
									f.cf_overloads <- f3 :: f.cf_overloads;
								) :: gen.gafter_expr_filters_ended);
								f3
							end else begin
								(* if it's not overload, just cast the vars *)
								if vars_to_declare <> [] then
								f.cf_expr <- Some({ e with
									eexpr = TFunction({ tf with
										tf_args = List.rev new_args;
										tf_expr = Type.concat block tf.tf_expr
									});
								});
								f
							end
						| _ -> f)
				| _ -> f
			in
			if not (has_class_flag c CExtern) then
				List.iter (fun f ->
					if has_class_field_flag f CfOverride then begin
						remove_class_field_flag f CfOverride;
						let f2 = check_f f in
						add_class_field_flag f2 CfOverride
					end
				) c.cl_ordered_fields;
			md
		| _ -> md
	in
	run

let configure ?explicit_fn_name ~get_vmtype gen =
	let delay () =
		Hashtbl.clear gen.greal_field_types
	in
	gen.gafter_mod_filters_ended <- delay :: gen.gafter_mod_filters_ended;
	let run = run ~explicit_fn_name ~get_vmtype gen in
	gen.gmodule_filters#add name (PCustom priority) run
