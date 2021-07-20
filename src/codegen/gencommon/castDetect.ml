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
open Ast
open Globals
open Type
open Codegen
open Gencommon

(* ******************************************* *)
(* Casts detection v2 *)
(* ******************************************* *)
(*
	Will detect implicit casts and add TCast for them. Since everything is already followed by follow_all, typedefs are considered a new type altogether

	Types shouldn't be cast if:
		* When an instance is being coerced to a superclass or to an implemented interface
		* When anything is being coerced to Dynamic

	edit:
		As a matter of performance, we will also run the type parameters casts in here. Otherwise the exact same computation would have to be performed twice,
		with maybe even some loss of information

		* TAnon / TDynamic will call
		* Type parameter handling will be abstracted

	dependencies:
		Must run before ExpressionUnwrap

*)
let name = "cast_detect"
let priority = solve_deps name [DBefore RealTypeParams.priority; DBefore ExpressionUnwrap.priority]

(* ******************************************* *)
(* ReturnCast *)
(* ******************************************* *)
(*
	Cast detection for return types can't be done at CastDetect time, since we need an
	unwrapped expression to make sure we catch all return cast detections. So this module
	is specifically to deal with that, and is configured automatically by CastDetect
*)
module ReturnCast =
struct
	let name = "return_cast"
	let priority = solve_deps name [DAfter priority; DAfter ExpressionUnwrap.priority]

	let default_implementation gen =
		let rec extract_expr e = match e.eexpr with
			| TParenthesis e
			| TMeta (_,e)
			| TCast(e,_) -> extract_expr e
			| _ -> e
		in
		let current_ret_type = ref None in
		let handle e tto tfrom = gen.ghandle_cast (gen.greal_type tto) (gen.greal_type tfrom) e in
		let in_value = ref false in
		let binop_right_expr_type op actual_type =
			match op with
			| OpShr | OpShl | OpUShr | OpAssignOp (OpShr | OpShl | OpUShr) ->
				(match follow actual_type with
				| TAbstract ({ a_path = (["cs"], "Int64") }, _) -> gen.gcon.basic.tint
				| _ -> actual_type)
			| _ -> actual_type
		in

		let rec run e =
			let was_in_value = !in_value in
			in_value := true;
			match e.eexpr with
			| TReturn (eopt) ->
				(* a return must be inside a function *)
				let ret_type = match !current_ret_type with | Some(s) -> s | None -> gen.gcon.error "Invalid return outside function declaration." e.epos; die "" __LOC__ in
				(match eopt with
				| None when not (ExtType.is_void ret_type) ->
					Texpr.Builder.mk_return (null ret_type e.epos)
				| None -> e
				| Some eret ->
					Texpr.Builder.mk_return (handle (run eret) ret_type eret.etype))
			| TFunction(tfunc) ->
				let last_ret = !current_ret_type in
				current_ret_type := Some(tfunc.tf_type);
				let ret = Type.map_expr run e in
				current_ret_type := last_ret;
				ret
			| TBlock el ->
				{ e with eexpr = TBlock ( List.map (fun e -> in_value := false; run e) el ) }
			| TBinop ( (Ast.OpAssign as op),e1,e2)
			| TBinop ( (Ast.OpAssignOp _ as op),e1,e2) when was_in_value ->
				let e1 = extract_expr (run e1) in
				let r = { e with eexpr = TBinop(op, e1, handle (run e2) e1.etype e2.etype); etype = e1.etype } in
				handle r e.etype e1.etype
			| TBinop ( (Ast.OpAssign as op),({ eexpr = TField(tf, f) } as e1), e2 )
			| TBinop ( (Ast.OpAssignOp _ as op),({ eexpr = TField(tf, f) } as e1), e2 ) ->
				(match field_access_esp gen (gen.greal_type tf.etype) (f) with
					| FClassField(cl,params,_,_,is_static,actual_t,_) ->
						let actual_t = if is_static then actual_t else apply_params cl.cl_params params actual_t in
						let actual_t = binop_right_expr_type op actual_t in
						let e1 = extract_expr (run e1) in
						{ e with eexpr = TBinop(op, e1, handle (run e2) actual_t e2.etype); etype = e1.etype }
					| _ ->
						let e1 = extract_expr (run e1) in
						let actual_t = binop_right_expr_type op e2.etype in
						{ e with eexpr = TBinop(op, e1, handle (run e2) e1.etype actual_t); etype = e1.etype }
				)
			| TBinop ( (Ast.OpAssign as op),e1,e2)
			| TBinop ( (Ast.OpAssignOp _ as op),e1,e2) ->
				let e1 = extract_expr (run e1) in
				let actual_t = binop_right_expr_type op e2.etype in
				{ e with eexpr = TBinop(op, e1, handle (run e2) e1.etype actual_t); etype = e1.etype }
			| _ -> Type.map_expr run e
		in
		run

	let configure gen =
		let map = default_implementation gen in
		gen.gsyntax_filters#add name (PCustom priority) map
end;;

(*
	Since this function is applied under native-context only, the type paraters will already be changed
*)
let map_cls gen also_implements fn super =
	let rec loop c tl =
		if c == super then
			fn c tl
		else
			(match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					let tls = gen.greal_type_param (TClassDecl cs) tls in
					loop cs (List.map (apply_params c.cl_params tl) tls)
			)
			||
			(if also_implements then
				List.exists (fun (cs,tls) -> loop cs (List.map (apply_params c.cl_params tl) tls)) c.cl_implements
			else
				false)
	in
	loop

let follow_dyn t = match follow t with
	| TMono _ | TLazy _ -> t_dynamic
	| t -> t

(*
	this has a slight change from the type.ml version, in which it doesn't
	change a TMono into the other parameter
*)
let rec type_eq gen param a b =
	if a == b then
		()
	else match follow_dyn (gen.greal_type a) , follow_dyn (gen.greal_type b) with
	| TEnum (e1,tl1) , TEnum (e2,tl2) ->
		if e1 != e2 && not (param = EqCoreType && e1.e_path = e2.e_path) then Type.error [cannot_unify a b];
		List.iter2 (type_eq gen param) tl1 tl2
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		if a1 != a2 && not (param = EqCoreType && a1.a_path = a2.a_path) then Type.error [cannot_unify a b];
		List.iter2 (type_eq gen param) tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		if c1 != c2 && not (param = EqCoreType && c1.cl_path = c2.cl_path) && (match c1.cl_kind, c2.cl_kind with KExpr _, KExpr _ -> false | _ -> true) then Type.error [cannot_unify a b];
		List.iter2 (type_eq gen param) tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			type_eq gen param r1 r2;
			List.iter2 (fun (n,o1,t1) (_,o2,t2) ->
				if o1 <> o2 then Type.error [Not_matching_optional n];
				type_eq gen param t1 t2
			) l1 l2
		with
			Unify_error l -> Type.error (cannot_unify a b :: l))
	| TDynamic a , TDynamic b ->
		type_eq gen param a b
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f1 ->
				try
					let f2 = PMap.find n a2.a_fields in
					if f1.cf_kind <> f2.cf_kind && (param = EqStrict || param = EqCoreType || not (unify_kind f1.cf_kind f2.cf_kind)) then Type.error [invalid_kind n f1.cf_kind f2.cf_kind];
					try
						type_eq gen param f1.cf_type f2.cf_type
					with
						Unify_error l -> Type.error (invalid_field n :: l)
				with
					Not_found ->
						Type.error [has_no_field b n];
			) a1.a_fields;
			PMap.iter (fun n f2 ->
				if not (PMap.mem n a1.a_fields) then begin
					Type.error [has_no_field a n];
				end;
			) a2.a_fields;
		with
			Unify_error l -> Type.error (cannot_unify a b :: l))
	| _ , _ ->
		if b == t_dynamic && (param = EqRightDynamic || param = EqBothDynamic) then
			()
		else if a == t_dynamic && param = EqBothDynamic then
			()
		else
			Type.error [cannot_unify a b]

let type_iseq gen a b =
	try
		type_eq gen EqStrict a b;
		true
	with
		Unify_error _ -> false

(* will return true if both arguments are compatible. If it's not the case, a runtime error is very likely *)
let is_cl_related gen cl tl super superl =
	let is_cl_related cl tl super superl = map_cls gen (match cl.cl_kind,super.cl_kind with KTypeParameter _, _ | _,KTypeParameter _ -> false | _ -> true) (fun _ _ -> true) super cl tl in
	is_cl_related cl tl super superl || is_cl_related super superl cl tl

let is_exactly_basic gen t1 t2 =
	match gen.gfollow#run_f t1, gen.gfollow#run_f t2 with
		| TAbstract(a1, []), TAbstract(a2, []) ->
			a1 == a2 && Common.defined gen.gcon Define.FastCast
		| TInst(c1, []), TInst(c2, []) ->
			c1 == c2 && Common.defined gen.gcon Define.FastCast
		| TEnum(e1, []), TEnum(e2, []) ->
			e1 == e2 && Common.defined gen.gcon Define.FastCast
		| _ ->
			false

let rec is_unsafe_cast gen to_t from_t =
	match (follow to_t, follow from_t) with
		| TInst(cl_to, to_params), TInst(cl_from, from_params) ->
			not (is_cl_related gen cl_from from_params cl_to to_params)
		| TEnum(e_to, _), TEnum(e_from, _) ->
			e_to.e_path <> e_from.e_path
		| TFun _, TFun _ ->
			(* functions are never unsafe cast by default. This behavior might be changed *)
			(* with a later AST pass which will run through TFun to TFun casts *)
			false
		| TMono _, _
		| _, TMono _
		| TDynamic _, _
		| _, TDynamic _ ->
			false
		| TAnon _, _
		| _, TAnon _ ->
			(* anonymous are never unsafe also. *)
			(* Though they will generate a cast, so if this cast is unneeded it's better to avoid them by tweaking gen.greal_type *)
			false
		| TAbstract _, _
		| _, TAbstract _ ->
			(try
				unify from_t to_t;
				false
			with | Unify_error _ ->
				try
					unify to_t from_t; (* still not unsafe *)
					false
				with | Unify_error _ ->
					true)
		| _ -> true

let unifies tfrom tto = try
	unify tfrom tto;
	true
with | _ ->
	false

let do_unsafe_cast gen from_t to_t e	=
	let t_path t =
		match t with
			| TInst(cl, _) -> cl.cl_path
			| TEnum(e, _) -> e.e_path
			| TType(t, _) -> t.t_path
			| TAbstract(a, _) -> a.a_path
			| TDynamic _ -> ([], "Dynamic")
			| _ -> raise Not_found
	in
	match gen.gfollow#run_f from_t, gen.gfollow#run_f to_t with
	| TInst({ cl_kind = KTypeParameter tl },_), t2 when List.exists (fun t -> unifies t t2) tl ->
		mk_cast to_t (mk_cast t_dynamic e)
	| from_t, to_t when gen.gspecial_needs_cast to_t from_t ->
		mk_cast to_t e
	| _ ->
		let do_default () =
			gen.gon_unsafe_cast to_t e.etype e.epos;
			mk_cast to_t (mk_cast t_dynamic e)
		in
		(* TODO: there really should be a better way to write that *)
		try
			if (Hashtbl.find gen.gsupported_conversions (t_path from_t)) from_t to_t then
				mk_cast to_t e
			else
				do_default()
		with
			| Not_found ->
				try
					if (Hashtbl.find gen.gsupported_conversions (t_path to_t)) from_t to_t then
						mk_cast to_t e
					else
						do_default()
				with
					| Not_found -> do_default()

(* ****************************** *)
(* cast handler *)
(* decides if a cast should be emitted, given a from and a to type *)
(*
	this function is like a mini unify, without e.g. subtyping, which makes sense
	at the backend level, since most probably Anons and TInst will have a different representation there
*)
let rec handle_cast gen e real_to_t real_from_t =
	let do_unsafe_cast () = do_unsafe_cast gen real_from_t real_to_t { e with etype = real_from_t } in
	let to_t, from_t = real_to_t, real_from_t in

	let mk_cast fast t e =
		match e.eexpr with
			(* TThrow is always typed as Dynamic, we just need to type it accordingly *)
			| TThrow _ -> { e with etype = t }
			| _ -> if fast then mk_castfast t e else mk_cast t e
	in

	let e = { e with etype = real_from_t } in
	if try fast_eq real_to_t real_from_t with Invalid_argument _ -> false then e else
	match real_to_t, real_from_t with
		(* string is the only type that can be implicitly converted from any other *)
		| TInst( { cl_path = ([], "String") }, []), TInst( { cl_path = ([], "String") }, [] ) ->
			mk_cast true to_t e
		| TInst( { cl_path = ([], "String") }, []), _ ->
			mk_cast false to_t e
		| TInst( ({ cl_path = (["cs"|"java"], "NativeArray") } as c_array), [tp_to] ), TInst({ cl_path = (["cs"|"java"], "NativeArray") }, [tp_from]) when not (type_iseq gen (gen.greal_type tp_to) (gen.greal_type tp_from)) ->
			(* see #5751 . NativeArray is special because of its ties to Array. We could potentially deal with this for all *)
			(* TNew expressions, but it's not that simple, since we don't want to retype the whole expression list with the *)
			(* updated type. *)
			(match e.eexpr with
				| TNew(c,_,el) when c == c_array ->
					mk_cast false (TInst(c_array,[tp_to])) { e with eexpr = TNew(c, [tp_to], el); etype = TInst(c_array,[tp_to]) }
				| _ ->
					e
			)
		| TInst(cl_to, params_to), TInst(cl_from, params_from) ->
			let ret = ref None in
			(*
				this is a little confusing:
				we are here mapping classes until we have the same to and from classes, applying the type parameters in each step, so we can
				compare the type parameters;

				If a class is found - meaning that the cl_from can be converted without a cast into cl_to,
				we still need to check their type parameters.
			*)
			ignore (map_cls gen (match cl_from.cl_kind,cl_to.cl_kind with KTypeParameter _, _ | _,KTypeParameter _ -> false | _ -> true) (fun _ tl ->
				try
					(* type found, checking type parameters *)
					List.iter2 (type_eq gen EqStrict) tl params_to;
					ret := Some e;
					true
				with | Unify_error _ ->
					(* type parameters need casting *)
					if gen.ghas_tparam_cast_handler then begin
						(*
							if we are already handling type parameter casts on other part of code (e.g. RealTypeParameters),
							we'll just make a cast to indicate that this place needs type parameter-involved casting
						*)
						ret := Some (mk_cast true to_t e);
						true
					end else
						(*
							if not, we're going to check if we only need a simple cast,
							or if we need to first cast into the dynamic version of it
						*)
						try
							List.iter2 (type_eq gen EqRightDynamic) tl params_to;
							ret := Some (mk_cast true to_t e);
							true
						with | Unify_error _ ->
							ret := Some (mk_cast true to_t (mk_cast true (TInst(cl_to, List.map (fun _ -> t_dynamic) params_to)) e));
							true
			) cl_to cl_from params_from);
			if is_some !ret then
				get !ret
			else if is_cl_related gen cl_from params_from cl_to params_to then
				mk_cast true to_t e
			else
				(* potential unsafe cast *)
				(do_unsafe_cast ())
		| TMono _, TMono _
		| TMono _, TDynamic _
		| TDynamic _, TDynamic _
		| TDynamic _, TMono _ ->
			e
		| TMono _, _
		| TDynamic _, _
		| TAnon _, _ when gen.gneeds_box real_from_t ->
			mk_cast false to_t e
		| TMono _, _
		| TDynamic _, _ -> e
		| _, TMono _
		| _, TDynamic _ -> mk_cast false to_t e
		| TAnon (a_to), TAnon (a_from) ->
			if a_to == a_from then
				e
			else if type_iseq gen to_t from_t then (* FIXME apply unify correctly *)
				e
			else
				mk_cast true to_t e
		| _, TAnon(anon) -> (try
			let p2 = match !(anon.a_status) with
			| Statics c -> TInst(c,List.map (fun _ -> t_dynamic) c.cl_params)
			| EnumStatics e -> TEnum(e, List.map (fun _ -> t_dynamic) e.e_params)
			| AbstractStatics a -> TAbstract(a, List.map (fun _ -> t_dynamic) a.a_params)
			| _ -> raise Not_found
			in
			let tclass = match get_type gen ([],"Class") with
			| TAbstractDecl(a) -> a
			| _ -> die "" __LOC__ in
			handle_cast gen e real_to_t (gen.greal_type (TAbstract(tclass, [p2])))
		with | Not_found ->
			mk_cast false to_t e)
		| TAbstract (a_to, _), TAbstract(a_from, _) when a_to == a_from ->
			e
		| TAbstract _, TInst({ cl_kind = KTypeParameter _ }, _)
		| TInst({ cl_kind = KTypeParameter _ }, _), TAbstract _ ->
			do_unsafe_cast()
		| TAbstract _, _
		| _, TAbstract _ ->
			(try
				unify from_t to_t;
				mk_cast true to_t e
			with | Unify_error _ ->
				try
					unify to_t from_t;
					mk_cast true to_t e
				with | Unify_error _ ->
					do_unsafe_cast())
		| TEnum(e_to, []), TEnum(e_from, []) ->
			if e_to == e_from then
				e
			else
				(* potential unsafe cast *)
				(do_unsafe_cast ())
		| TEnum(e_to, params_to), TEnum(e_from, params_from) when e_to.e_path = e_from.e_path ->
			(try
					List.iter2 (type_eq gen (if gen.gallow_tp_dynamic_conversion then EqRightDynamic else EqStrict)) params_from params_to;
					e
				with
					| Unify_error _ -> do_unsafe_cast ()
			)
		| TEnum(en, params_to), TInst(cl, params_from)
			| TInst(cl, params_to), TEnum(en, params_from) ->
				(* this is here for max compatibility with EnumsToClass module *)
			if en.e_path = cl.cl_path && Meta.has Meta.Class en.e_meta then begin
				(try
					List.iter2 (type_eq gen (if gen.gallow_tp_dynamic_conversion then EqRightDynamic else EqStrict)) params_from params_to;
					e
				with
					| Invalid_argument _ ->
						(*
							this is a hack for RealTypeParams. Since there is no way at this stage to know if the class is the actual
							EnumsToClass derived from the enum, we need to imply from possible ArgumentErrors (because of RealTypeParams interfaces),
							that they would only happen if they were a RealTypeParams created interface
						*)
						e
					| Unify_error _ -> do_unsafe_cast ()
				)
			end else
				do_unsafe_cast ()
		| TType(t_to, params_to), TType(t_from, params_from) when t_to == t_from ->
			if gen.gspecial_needs_cast real_to_t real_from_t then
				(try
					List.iter2 (type_eq gen (if gen.gallow_tp_dynamic_conversion then EqRightDynamic else EqStrict)) params_from params_to;
					e
				with
					| Unify_error _ -> do_unsafe_cast ()
				)
			else
				e
		| TType(t_to, _), TType(t_from,_) ->
			if gen.gspecial_needs_cast real_to_t real_from_t then
				mk_cast false to_t e
			else
				e
		| TType _, _ when gen.gspecial_needs_cast real_to_t real_from_t ->
			mk_cast false to_t e
		| _, TType _ when gen.gspecial_needs_cast real_to_t real_from_t ->
			mk_cast false to_t e
		(*| TType(t_to, _), TType(t_from, _) ->
			if t_to.t_path = t_from.t_path then
				e
			else if is_unsafe_cast gen real_to_t real_from_t then (* is_unsafe_cast will already follow both *)
				(do_unsafe_cast ())
			else
				mk_cast to_t e*)
		| TType _, _
		| _, TType _ ->
			if is_unsafe_cast gen real_to_t real_from_t then (* is_unsafe_cast will already follow both *)
				(do_unsafe_cast ())
			else
				mk_cast false to_t e
		| TAnon anon, _ ->
			if PMap.is_empty anon.a_fields then
				e
			else
				mk_cast true to_t e
		| TFun(args, ret), TFun(args2, ret2) ->
			let get_args = List.map (fun (_,_,t) -> t) in
			(try List.iter2 (type_eq gen (EqBothDynamic)) (ret :: get_args args) (ret2 :: get_args args2); e with | Unify_error _ | Invalid_argument _ -> mk_cast true to_t e)
		| _, _ ->
			do_unsafe_cast ()

(* end of cast handler *)
(* ******************* *)

let is_static_overload c name =
	match c.cl_super with
	| None -> false
	| Some (sup,_) ->
		let rec loop c =
			(PMap.mem name c.cl_statics) || (match c.cl_super with
				| None -> false
				| Some (sup,_) -> loop sup)
		in
		loop sup

(* this is a workaround for issue #1743, as FInstance() is returning the incorrect classfield *)
let rec clean_t t = match follow t with
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		clean_t (Abstract.get_underlying_type a tl)
	| t -> t

let select_overload gen applied_f overloads types params =
	let rec check_arg arglist elist =
		match arglist, elist with
			| [], [] -> true (* it is valid *)
			| (_,_,t) :: [], elist when ExtType.is_rest t ->
				(match follow t with
				| TAbstract({ a_path = (["haxe"],"Rest") }, [t]) ->
				List.for_all (fun (_,_,et) -> Type.type_iseq (clean_t et) (clean_t t)) elist
				| _ -> die "" __LOC__)
			| (_,_,t) :: arglist, (_,_,et) :: elist when Type.type_iseq (clean_t et) (clean_t t) ->
				check_arg arglist elist
			| _ -> false
	in
	match follow applied_f with
	| TFun _ ->
		replace_mono applied_f;
		let args, _ = get_fun applied_f in
		let elist = List.rev args in
		let rec check_overload overloads =
			match overloads with
			| (t, cf) :: overloads ->
					let cft = apply_params types params t in
					let cft = monomorphs cf.cf_params cft in
					let args, _ = get_fun cft in
					if check_arg (List.rev args) elist then
						cf,t,false
					else if overloads = [] then
						cf,t,true (* no compatible overload was found *)
					else
						check_overload overloads
			| [] -> die "" __LOC__
		in
		check_overload overloads
	| _ -> match overloads with  (* issue #1742 *)
	| (t,cf) :: [] -> cf,t,true
	| (t,cf) :: _ -> cf,t,false
	| _ -> die "" __LOC__

let rec cur_ctor c tl =
	match c.cl_constructor with
	| Some ctor ->
		ctor, c, tl
	| None ->
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (sup,stl) ->
			cur_ctor sup (List.map (apply_params c.cl_params tl) stl)

let choose_ctor gen cl tparams etl maybe_empty_t p =
	let ctor, sup, stl = cur_ctor cl tparams in
	(* get returned stl, with Dynamic as t_empty *)
	let rec get_changed_stl c tl =
		if c == sup then
			tl
		else match c.cl_super with
		| None -> stl
		| Some(sup,stl) -> get_changed_stl sup (List.map (apply_params c.cl_params tl) stl)
	in
	let ret_tparams = List.map (fun t -> match follow t with
		| TDynamic _ | TMono _ -> t_empty
		| _ -> t) tparams
	in
	let ret_stl = get_changed_stl cl ret_tparams in
	let ctors = ctor :: ctor.cf_overloads in
	List.iter replace_mono etl;
	(* first filter out or select outright maybe_empty *)
	let ctors, is_overload = match etl, maybe_empty_t with
		| [t], Some empty_t ->
			let count = ref 0 in
			let is_empty_call = Type.type_iseq t empty_t in
			let ret = List.filter (fun cf -> match follow cf.cf_type with
				| TFun([_,_,t],_) ->
					replace_mono t; incr count; is_empty_call = (Type.type_iseq t empty_t)
				| _ -> false) ctors
			in
			ret, !count > 1
		| _ ->
			let len = List.length etl in
			let ret = List.filter (fun cf -> List.length (fst (get_fun cf.cf_type)) <= len) ctors in
			ret, (match ret with | _ :: [] -> false | _ -> true)
	in
	let rec check_arg arglist elist =
		match arglist, elist with
		| [], [] -> true
		| [(_,_,t)], elist when ExtType.is_rest (follow t) ->
			let is_rest_array arg_t =
				Type.fast_eq (Abstract.follow_with_abstracts t) (Abstract.follow_with_abstracts arg_t)
			in
			(match elist with
			| [arg_t] when is_rest_array arg_t -> true
			| _ ->
				match follow t with
				| TAbstract ({ a_path = ["haxe"],"Rest" }, [t1]) ->
					let t1 = run_follow gen t1 in
					(try
						List.iter (fun et -> unify et t1) elist;
						true
					with Unify_error _ ->
						false
					)
				| _ -> die "" __LOC__
			)
		| (_,_,t) :: arglist, et :: elist ->
			(try
				let t = run_follow gen t in
				unify et t;
				check_arg arglist elist
			with Unify_error el ->
				(* List.iter (fun el -> gen.gcon.warning (Error.unify_error_msg (print_context()) el) p) el; *)
				false
			)
		| _ ->
			false
	in
	let rec check_cf cf =
		let t = apply_params sup.cl_params stl cf.cf_type in
		replace_mono t;
		let args, _ = get_fun t in
		check_arg args etl
	in
	match is_overload, ctors with
		| false, [c] ->
			false, c, sup, ret_stl
		| _ ->
			is_overload, List.find check_cf ctors, sup, ret_stl

let change_rest tfun elist =
	let expects_rest_args = ref false in
	let rec loop acc arglist elist = match arglist, elist with
		| (_,_,t) as arg :: [], elist when ExtType.is_rest t ->
			(match elist with
			| [{ eexpr = TUnop (Spread,Prefix,e) }] ->
				List.rev (arg :: acc)
			| _ ->
				(match follow t with
				| TAbstract({ a_path = (["haxe"],"Rest") },[t1]) ->
					let is_rest_array e =
						Type.fast_eq (Abstract.follow_with_abstracts t) (Abstract.follow_with_abstracts e.etype)
					in
					(match elist with
					| [e] when is_rest_array e ->
						List.rev (("rest",false,t) :: acc)
					| _ ->
						expects_rest_args := true;
						List.rev (List.map (fun _ -> "rest",false,t1) elist @ acc)
					)
				| _ -> die "" __LOC__)
			)
		| (n,o,t) :: arglist, _ :: elist ->
			loop ((n,o,t) :: acc) arglist elist
		| _, _ ->
			List.rev acc
	in
	let args,ret = get_fun tfun in
	let args_types = loop [] args elist in
	!expects_rest_args,TFun(args_types, ret)

let fastcast_if_needed gen expr real_to_t real_from_t =
	if Common.defined gen.gcon Define.FastCast then begin
		if type_iseq gen real_to_t real_from_t then
			{ expr with etype = real_to_t }
		else
			mk_castfast real_to_t { expr with etype=real_from_t }
	end else
		handle_cast gen expr real_to_t real_from_t

(*
	Type parameter handling
	It will detect if/what type parameters were used, and call the cast handler
	It will handle both TCall(TField) and TCall by receiving a texpr option field: e
	Also it will transform the type parameters with greal_type_param and make

	handle_impossible_tparam - should cases where the type parameter is impossible to be determined from the called parameters be Dynamic?
	e.g. static function test<T>():T {}
*)

(* match e.eexpr with | TCall( ({ eexpr = TField(ef, f) }) as e1, elist ) -> *)
let handle_type_parameter gen e e1 ef ~clean_ef ~overloads_cast_to_base f elist calls_parameters_explicitly =
	(* the ONLY way to know if this call has parameters is to analyze the calling field. *)
	(* To make matters a little worse, on both C# and Java only in some special cases that type parameters will be used *)
	(* Namely, when using reflection type parameters are useless, of course. This also includes anonymous types *)
	(* this will have to be handled by gparam_func_call *)

	let return_var efield =
		match e with
			| None ->
				efield
			| Some ecall ->
				match follow efield.etype with
					| TFun(_,ret) ->
						(* closures will be handled by the closure handler. So we will just hint what's the expected type *)
						(* FIXME: should closures have also its arguments cast correctly? In the current implementation I think not. TO_REVIEW *)
						handle_cast gen { ecall with eexpr = TCall(efield, elist) } (gen.greal_type ecall.etype) ret
					| _ ->
						{ ecall with eexpr = TCall(efield, elist) }
	in

	(* this function will receive the original function argument, the applied function argument and the original function parameters. *)
	(* from this info, it will infer the applied tparams for the function *)
	let infer_params pos (original_args:((string * bool * t) list * t)) (applied_args:((string * bool * t) list * t)) (params:(string * t) list) calls_parameters_explicitly : tparams =
		match params with
		| [] -> []
		| _ ->
			let args_list args = (if not calls_parameters_explicitly then t_dynamic else snd args) :: (List.map (fun (n,o,t) -> t) (fst args)) in

			let monos = List.map (fun _ -> mk_mono()) params in
			let original = args_list (get_fun (apply_params params monos (TFun(fst original_args,snd original_args)))) in
			let applied = args_list applied_args in

			(try
				List.iter2 (fun a o ->
					unify a o
					(* type_eq EqStrict a o *)
				) applied original
				(* unify applied original *)
			with
				| Unify_error el ->
						(match el with
						(*
							Don't emit a warning for abstracts if underlying type is the same as the second type.
							This situation is caused by `Normalize.filter_param` not "unpacking" abstracts.
						*)
						| [Cannot_unify (TAbstract(a,params), b)]
						| [Cannot_unify (b, TAbstract(a,params))] ->
							let a = apply_params a.a_params params a.a_this in
							if not (shallow_eq a b) then
								gen.gcon.warning ("This expression may be invalid") pos
						| _ ->
							gen.gcon.warning ("This expression may be invalid") pos
						)
				| Invalid_argument _ ->
						gen.gcon.warning ("This expression may be invalid") pos
			);

			List.map (fun t ->
				match follow_without_null t with
					| TMono _ ->	t_empty
					| t -> t
			) monos
	in

	let real_type = gen.greal_type ef.etype in
	(* this part was rewritten at roughly r6477 in order to correctly support overloads *)
	(match field_access_esp gen real_type (f) with
	| FClassField (cl, params, _, cf, is_static, actual_t, declared_t) when e <> None && (cf.cf_kind = Method MethNormal || cf.cf_kind = Method MethInline) ->
			(* C# target changes params with a real_type function *)
			let params = match follow clean_ef.etype with
				| TInst(_,params) -> params
				| _ -> params
			in
			let local_mk_cast t expr =
				(* handle_cast gen expr t expr.etype *)
				if is_exactly_basic gen t expr.etype then
					expr
				else
					mk_castfast t expr
			in

			let ecall = get e in
			let ef = ref ef in
			let is_overload = cf.cf_overloads <> [] || has_class_field_flag cf CfOverload || (is_static && is_static_overload cl (field_name f)) in
			let cf, actual_t, error = match is_overload with
				| false ->
						(* since actual_t from FClassField already applies greal_type, we're using the get_overloads helper to get this info *)
						let t = if cf.cf_params = [] then (* this if statement must be eliminated - it's a workaround for #3516 + infer params. *)
							actual_t
						else
							declared_t
						in
						cf,t,false
				| true ->
				let (cf, actual_t, error), is_static = match f with
					| FInstance(c,_,cf) | FClosure(Some (c,_),cf) ->
						(* get from overloads *)
						(* FIXME: this is a workaround for issue #1743 . Uncomment this code after it was solved *)
						(* let t, cf = List.find (fun (t,cf2) -> cf == cf2) (Overloads.get_overloads cl (field_name f)) in *)
						(* cf, t, false *)
						select_overload gen e1.etype (Overloads.collect_overloads (fun t -> t) cl (field_name f)) cl.cl_params params, false
					| FStatic(c,f) ->
						(* workaround for issue #1743 *)
						(* f,f.cf_type, false *)
						select_overload gen e1.etype ((f.cf_type,f) :: List.map (fun f -> f.cf_type,f) f.cf_overloads) [] [], true
					| _ ->
						gen.gcon.warning "Overloaded classfield typed as anonymous" ecall.epos;
						(cf, actual_t, true), true
				in

				if not (is_static || error) then match find_first_declared_field gen cl ~exact_field:{ cf with cf_type = actual_t } cf.cf_name with
				| Some(cf_orig,actual_t,_,_,declared_cl,tl,tlch) ->
					let rec is_super e = match e.eexpr with
						| TConst TSuper -> true
						| TParenthesis p | TMeta(_,p) -> is_super p
						| _ -> false
					in
					if declared_cl != cl && overloads_cast_to_base && not (is_super !ef) then begin
						let pos = (!ef).epos in
						ef := {
							eexpr = TCall(
								{ eexpr = TIdent "__as__"; etype = t_dynamic; epos = pos },
								[!ef]);
							etype = TInst(declared_cl,List.map (apply_params cl.cl_params params) tl);
							epos = pos
						}
					end;
					{ cf_orig with cf_name = cf.cf_name },actual_t,false
				| None ->
					gen.gcon.warning "Cannot find matching overload" ecall.epos;
					cf, actual_t, true
				else
					cf,actual_t,error
			in

			(* take off Rest param *)
			let _,actual_t = change_rest actual_t elist in
			(* set the real (selected) class field *)
			let f = match f with
				| FInstance(c,tl,_) -> FInstance(c,tl,cf)
				| FClosure(c,_) -> FClosure(c,cf)
				| FStatic(c,_) -> FStatic(c,cf)
				| f -> f
			in
			let error = error || (match follow actual_t with | TFun _ -> false | _ -> true) in
			if error then (* if error, ignore arguments *)
				if ExtType.is_void ecall.etype then
					{ ecall with eexpr = TCall({ e1 with eexpr = TField(!ef, f) }, elist ) }
				else
					local_mk_cast ecall.etype { ecall with eexpr = TCall({ e1 with eexpr = TField(!ef, f) }, elist ) }
			else begin
				(* infer arguments *)
				(* let called_t = TFun(List.map (fun e -> "arg",false,e.etype) elist, ecall.etype) in *)
				let called_t = match follow e1.etype with | TFun _ -> e1.etype | _ -> TFun(List.map (fun e -> "arg",false,e.etype) elist, ecall.etype)	in (* workaround for issue #1742 *)
				let expects_rest_args,called_t = change_rest called_t elist in
				let original = (get_fun (apply_params cl.cl_params params actual_t)) in
				let applied = (get_fun called_t) in
				let fparams = infer_params ecall.epos original applied cf.cf_params calls_parameters_explicitly in
				(* get what the backend actually sees *)
				(* actual field's function *)
				let actual_t = get_real_fun gen actual_t in
				let real_params = gen.greal_type_param (TClassDecl cl) params in
				let function_t = apply_params cl.cl_params real_params actual_t in
				let real_fparams = if calls_parameters_explicitly then
					gen.greal_type_param (TClassDecl cl) fparams
				else
					gen.greal_type_param (TClassDecl cl) (infer_params ecall.epos (get_fun function_t) (get_fun (get_real_fun gen called_t)) cf.cf_params calls_parameters_explicitly) in
				let function_t = get_real_fun gen (apply_params cf.cf_params real_fparams function_t) in
				let args_ft, ret_ft = get_fun function_t in
				(* applied function *)
				let applied = elist in
				(* check types list *)
				let new_ecall, elist = try
					let fn = fun funct applied  ->
						match is_overload || real_fparams <> [], applied.eexpr with
						| true, TConst TNull ->
							mk_castfast (gen.greal_type funct) applied
						| true, _ -> (* when not (type_iseq gen (gen.greal_type applied.etype) funct) -> *)
							let ret = handle_cast gen applied (funct) (gen.greal_type applied.etype) in
							(match ret.eexpr with
							| TCast _ -> ret
							| _ -> local_mk_cast (funct) ret)
						| _ ->
							handle_cast gen applied (funct) (gen.greal_type applied.etype)
					in
					let rec loop args_ft applied =
						match args_ft, applied with
						| [], [] -> []
						| [(_,_,funct)], _ when expects_rest_args ->
							(match funct, applied with
							| TInst({ cl_path = (_,"NativeArray") },[_]),[a] when Type.does_unify funct a.etype ->
								[fn funct a]
							| TInst({ cl_path = (_,"NativeArray") },[funct]),_ ->
								List.map (fn funct) applied
							| _ ->
								raise (Invalid_argument "Unexpected rest arguments type")
							)
						| (_,_,funct)::args_ft, a::applied ->
							(fn funct a) :: loop args_ft applied
						| _ -> raise (Invalid_argument "Args length mismatch")
					in
					let elist = loop args_ft applied in
					{ ecall with
						eexpr = TCall(
							{ e1 with eexpr = TField(!ef, f) },
							elist);
					}, elist
				with Invalid_argument _ ->
					gen.gcon.warning ("This expression may be invalid" ) ecall.epos;
					{ ecall with eexpr = TCall({ e1 with eexpr = TField(!ef, f) }, elist) }, elist
				in
				let new_ecall = if fparams <> [] then gen.gparam_func_call new_ecall { e1 with eexpr = TField(!ef, f) } fparams elist else new_ecall in
				let ret = handle_cast gen new_ecall (gen.greal_type ecall.etype) (gen.greal_type ret_ft) in
				(match gen.gcon.platform, cf.cf_params, ret.eexpr with
					| _, _, TCast _ -> ret
					| Java, _ :: _, _ ->
						(* this is a workaround for a javac openjdk issue with unused type parameters and return type inference *)
						(* see more at issue #3123 *)
						mk_cast (gen.greal_type ret_ft) new_ecall
					| _ -> ret)
			end
	| FClassField (cl,params,_,{ cf_kind = (Method MethDynamic | Var _) },_,actual_t,_) ->
		(* if it's a var, we will just try to apply the class parameters that have been changed with greal_type_param *)
		let t = apply_params cl.cl_params (gen.greal_type_param (TClassDecl cl) params) (gen.greal_type actual_t) in
		return_var (handle_cast gen { e1 with eexpr = TField(ef, f) } (gen.greal_type e1.etype) (gen.greal_type t))
	| FClassField (cl,params,_,cf,_,actual_t,_) ->
		return_var (handle_cast gen { e1 with eexpr = TField({ ef with etype = t_dynamic }, f) } e1.etype t_dynamic) (* force dynamic and cast back to needed type *)
	| FEnumField (en, efield, true) ->
		let ecall = match e with | None -> trace (field_name f); trace efield.ef_name; gen.gcon.error "This field should be called immediately" ef.epos; die "" __LOC__ | Some ecall -> ecall in
		(match en.e_params with
			(*
			| [] ->
				let args, ret = get_fun (efield.ef_type) in
				let ef = { ef with eexpr = TTypeExpr( TEnumDecl en ); etype = TEnum(en, []) } in
				handle_cast gen { ecall with eexpr = TCall({ e1 with eexpr = TField(ef, FEnum(en, efield)) }, List.map2 (fun param (_,_,t) -> handle_cast gen param (gen.greal_type t) (gen.greal_type param.etype)) elist args) } (gen.greal_type ecall.etype) (gen.greal_type ret)
		*)
			| _ ->
				let pt = match e with | None -> real_type | Some _ -> snd (get_fun e1.etype) in
				let _params = match follow pt with | TEnum(_, p) -> p | _ -> gen.gcon.warning (debug_expr e1) e1.epos; die "" __LOC__ in
				let args, ret = get_fun efield.ef_type in
				let actual_t = TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret) in
				(*
					because of differences on how <Dynamic> is handled on the platforms, this is a hack to be able to
					correctly use class field type parameters with RealTypeParams
				*)
				let cf_params = List.map (fun t -> match follow t with | TDynamic _ -> t_empty | _ -> t) _params in
				let t = apply_params en.e_params (gen.greal_type_param (TEnumDecl en) cf_params) actual_t in
				let t = apply_params efield.ef_params (List.map (fun _ -> t_dynamic) efield.ef_params) t in

				let args, ret = get_fun t in

				let elist = List.map2 (fun param (_,_,t) -> handle_cast gen (param) (gen.greal_type t) (gen.greal_type param.etype)) elist args in
				let e1 = { e1 with eexpr = TField({ ef with eexpr = TTypeExpr( TEnumDecl en ); etype = TEnum(en, _params) }, FEnum(en, efield) ) } in
				let new_ecall = gen.gparam_func_call ecall e1 _params elist in

				handle_cast gen new_ecall (gen.greal_type ecall.etype) (gen.greal_type ret)
		)
	| FEnumField _ when is_some e -> die "" __LOC__
	| FEnumField (en,efield,_) ->
			return_var { e1 with eexpr = TField({ ef with eexpr = TTypeExpr( TEnumDecl en ); },FEnum(en,efield)) }
	(* no target by date will uses this.so this code may not be correct at all *)
	| FAnonField cf ->
		let t = gen.greal_type cf.cf_type in
		return_var (handle_cast gen { e1 with eexpr = TField(ef, f) } (gen.greal_type e1.etype) t)
	| FNotFound
	| FDynamicField _ ->
		if is_some e then
			return_var { e1 with eexpr = TField(ef, f) }
		else
			return_var (handle_cast gen { e1 with eexpr = TField({ ef with etype = t_dynamic }, f) } e1.etype t_dynamic) (* force dynamic and cast back to needed type *)
	)

(* end of type parameter handling *)
(* ****************************** *)

(** overloads_cast_to_base argument will cast overloaded function types to the class that declared it. **)
(**			This is necessary for C#, and if true, will require the target to implement __as__, as a `quicker` form of casting **)
let configure gen ?(overloads_cast_to_base = false) maybe_empty_t calls_parameters_explicitly =
	let handle e t1 t2 = handle_cast gen e (gen.greal_type t1) (gen.greal_type t2) in

	let in_value = ref false in

	let rec clean_cast e = match e.eexpr with
		| TCast(e,_) -> clean_cast e
		| TParenthesis(e) | TMeta(_,e) -> clean_cast e
		| _ -> e
	in

	let get_abstract_impl t = match t with
		| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
			Abstract.get_underlying_type a pl
		| t -> t
	in

	let rec is_abstract_to_struct t = match t with
		| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
			is_abstract_to_struct (Abstract.get_underlying_type a pl)
		| TInst(c,_) when Meta.has Meta.Struct c.cl_meta ->
			true
		| _ -> false
	in

	let binop_type fast_cast op main_expr e1 e2 =
		let name = platform_name gen.gcon.platform in
		let basic = gen.gcon.basic in
		(* If either operand is of type decimal, the other operand is converted to type decimal, or a compile-time error occurs if the other operand is of type float or double.
			* Otherwise, if either operand is of type double, the other operand is converted to type double.
			* Otherwise, if either operand is of type float, the other operand is converted to type float.
			* Otherwise, if either operand is of type ulong, the other operand is converted to type ulong, or a compile-time error occurs if the other operand is of type sbyte, short, int, or long.
			* Otherwise, if either operand is of type long, the other operand is converted to type long.
			* Otherwise, if either operand is of type uint and the other operand is of type sbyte, short, or int, both operands are converted to type long.
			* Otherwise, if either operand is of type uint, the other operand is converted to type uint.
			* Otherwise, both operands are converted to type int.
			*  *)
		let t1, t2 = follow (run_follow gen e1.etype), follow (run_follow gen e2.etype) in
		let result =
			match t1, t2 with
			| TAbstract(a1,[]), TAbstract(a2,[]) when fast_cast && a1 == a2 ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
			| TInst(i1,[]), TInst(i2,[]) when fast_cast && i1 == i2 ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
			| TInst({ cl_path = ([],"String") },[]), _ when fast_cast && op = OpAdd ->
				{ main_expr with eexpr = TBinop(op, e1, mk_cast basic.tstring e2); etype = basic.tstring }
			| _, TInst({ cl_path = ([],"String") },[]) when fast_cast && op = OpAdd ->
				{ main_expr with eexpr = TBinop(op, mk_cast basic.tstring e1, e2); etype = basic.tstring }
			| TAbstract({ a_path = ([], "Float") }, []), _ when fast_cast ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
			| _, TAbstract({ a_path = ([], "Float") }, []) when fast_cast ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
			| TAbstract({ a_path = ([], "Single") }, []), _ when fast_cast ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
			| _, TAbstract({ a_path = ([], "Single") }, []) when fast_cast ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
			| TAbstract({ a_path = ([pf], "UInt64") }, []), _ when fast_cast && pf = name ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
			| _, TAbstract({ a_path = ([pf], "UInt64") }, []) when fast_cast && pf = name ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
			| TAbstract({ a_path = ([pf], "Int64") }, []), _ when fast_cast && pf = name ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
			| _, TAbstract({ a_path = ([pf], "Int64") }, []) when fast_cast && pf = name ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
			| TAbstract({ a_path = ([], "UInt") }, []), tother when like_int tother ->
				let ti64 = mt_to_t_dyn ( get_type gen ([name], "Int64") ) in
				let ret = { main_expr with eexpr = TBinop(op, e1, e2); etype = ti64 } in
				if op <> OpDiv then
					mk_cast t1 ret
				else
					ret
			| tother, TAbstract({ a_path = ([], "UInt") }, []) when like_int tother ->
				let ti64 = mt_to_t_dyn ( get_type gen ([name], "Int64") ) in
				let ret = { main_expr with eexpr = TBinop(op, e1, e2); etype = ti64 } in
				if op <> OpDiv then
					mk_cast t2 ret
				else
					ret
			| TAbstract({ a_path = ([], "UInt") }, []), _ ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e1.etype }
			| _, TAbstract({ a_path = ([], "UInt") }, []) ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = e2.etype }
			| TAbstract(a1,[]), TAbstract(a2,[]) when fast_cast ->
				{ main_expr with eexpr = TBinop(op, e1, e2); etype = basic.tint }
			| _ ->
				{ main_expr with eexpr = TBinop(op, e1, e2) }
		in
		(* maintain nullability *)
		match follow_without_null main_expr.etype, follow_without_null result.etype with
		| TAbstract ({ a_path = ([],"Null") },_), TAbstract ({ a_path = ([],"Null") },_) ->
			result
		| TAbstract ({ a_path = ([],"Null") } as null,_), _ ->
			{ result with etype = TAbstract(null, [result.etype]) }
		| _, TAbstract ({ a_path = ([],"Null") },[t]) ->
			{ result with etype = t }
		| _ -> result
	in
	let binop_type = binop_type (Common.defined gen.gcon Define.FastCast) in

	let rec run ?(just_type = false) e =
		let handle = if not just_type then handle else fun e t1 t2 -> { e with etype = gen.greal_type t2 } in
		let was_in_value = !in_value in
		in_value := true;
		match e.eexpr with
			| TConst ( TInt _ | TFloat _ | TBool _ as const ) ->
				(* take off any Null<> that it may have *)
				let t = follow (run_follow gen e.etype) in
				(* do not allow constants typed as Single - need to cast them *)
				let real_t = match const with
					| TInt _ -> gen.gcon.basic.tint
					| TFloat _ -> gen.gcon.basic.tfloat
					| TBool _ -> gen.gcon.basic.tbool
					| _ -> die "" __LOC__
				in
				handle e t real_t
			| TCast( { eexpr = TConst TNull }, _ ) ->
				{ e with eexpr = TConst TNull }
			| TCast( { eexpr = TCall( { eexpr = TIdent "__delegate__" } as local, [del] ) } as e2, _) ->
				{ e with eexpr = TCast({ e2 with eexpr = TCall(local, [Type.map_expr run del]) }, None) }

			| TBinop (OpAssignOp (Ast.OpShl | Ast.OpShr | Ast.OpUShr as op), e1, e2 ) ->
				let e1 = run ~just_type:true e1 in
				let e2 = handle (run e2) (gen.gcon.basic.tint) e2.etype in
				let rett = binop_type op e e1 e2 in
				{ e with eexpr = TBinop(OpAssignOp op, e1, e2); etype = rett.etype }
			| TBinop ( (Ast.OpAssign | Ast.OpAssignOp _ as op), e1, e2 ) ->
				let e1 = run ~just_type:true e1 in
				let e2 = handle (run e2) e1.etype e2.etype in
				{ e with eexpr = TBinop(op, clean_cast e1, e2) }
			| TBinop ( (Ast.OpShl | Ast.OpShr | Ast.OpUShr as op), e1, e2 ) ->
				let e1 = run e1 in
				let e2 = handle (run e2) (gen.gcon.basic.tint) e2.etype in
				let rett = binop_type op e e1 e2 in
				{ e with eexpr = TBinop(op, e1, e2); etype = rett.etype }
			| TBinop( (OpAdd | OpMult | OpDiv | OpSub | OpAnd | OpOr | OpXor | OpMod) as op, e1, e2 ) ->
				binop_type op e (run e1) (run e2)
			| TBinop( (OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte | OpBoolAnd | OpBoolOr) as op, e1, e2 ) ->
				handle { e with eexpr = TBinop(op, run e1, run e2) } e.etype gen.gcon.basic.tbool
			| TField(ef, f) ->
				handle_type_parameter gen None e (run ef) ~clean_ef:ef ~overloads_cast_to_base:overloads_cast_to_base f [] calls_parameters_explicitly
			| TArrayDecl el ->
				let et = e.etype in
				let base_type = match follow et with
					| TInst({ cl_path = ([], "Array") } as cl, bt) -> gen.greal_type_param (TClassDecl cl) bt
					| _ ->
						gen.gcon.warning (debug_type et) e.epos;
						(match gen.gcurrent_class with
							| Some cl -> print_endline (s_type_path cl.cl_path)
							| _ -> ());
						die "" __LOC__
				in
				let base_type = List.hd base_type in
				{ e with eexpr = TArrayDecl( List.map (fun e -> handle (run e) base_type e.etype) el ); etype = et }
			| TCall ({ eexpr = TIdent "__array__" } as arr_local, el) ->
				let et = e.etype in
				let base_type = match follow et with
					| TInst(cl, bt) -> gen.greal_type_param (TClassDecl cl) bt
					| _ -> die "" __LOC__
				in
				let base_type = List.hd base_type in
				{ e with eexpr = TCall(arr_local, List.map (fun e -> handle (run e) base_type e.etype) el ); etype = et }
			| TCall( ({ eexpr = TIdent s } as local), params ) when String.get s 0 = '_' && String.get s 1 = '_' && Hashtbl.mem gen.gspecial_vars s ->
				{ e with eexpr = TCall(local, List.map (fun e -> (match e.eexpr with TBlock _ -> in_value := false | _ -> ()); run e) params) }
			| TCall( ({ eexpr = TField(ef, f) }) as e1, elist ) ->
				handle_type_parameter gen (Some e) (e1) (run ef) ~clean_ef:ef ~overloads_cast_to_base:overloads_cast_to_base f (List.map run elist) calls_parameters_explicitly

			| TCall( { eexpr = TConst TSuper } as ef, eparams ) ->
				let cl, tparams = match follow ef.etype with
				| TInst(cl,p) ->
					cl,p
				| _ -> die "" __LOC__ in
				(try
					let is_overload, cf, sup, stl = choose_ctor gen cl tparams (List.map (fun e -> e.etype) eparams) maybe_empty_t e.epos in
					let handle e t1 t2 =
						if is_overload then
							let ret = handle e t1 t2 in
							match ret.eexpr with
							| TCast _ -> ret
							| _ -> mk_cast (gen.greal_type t1) e
						else
							handle e t1 t2
					in
					let stl = gen.greal_type_param (TClassDecl sup) stl in
					let args,rt = get_fun (apply_params sup.cl_params stl cf.cf_type) in
					let eparams = List.map2 (fun e (_,_,t) ->
						handle (run e) t e.etype
					) (wrap_rest_args gen (TFun (args,rt)) eparams e.epos) args in
					{ e with eexpr = TCall(ef, eparams) }
				with | Not_found ->
					gen.gcon.warning "No overload found for this constructor call" e.epos;
					{ e with eexpr = TCall(ef, List.map run eparams) })
			| TCall (ef, eparams) ->
				(match ef.etype with
					| TFun(p, ret) ->
						handle ({ e with eexpr = TCall(run ef, List.map2 (fun param (_,_,t) -> handle (run param) t param.etype) eparams p) }) e.etype ret
					| _ -> Type.map_expr run e
				)
			| TNew ({ cl_kind = KTypeParameter _ }, _, _) ->
				Type.map_expr run e
			| TNew (cl, tparams, eparams) -> (try
				let is_overload, cf, sup, stl = choose_ctor gen cl tparams (List.map (fun e -> e.etype) eparams) maybe_empty_t e.epos in
				let handle e t1 t2 =
					if is_overload then
						let ret = handle e t1 t2 in
						match ret.eexpr with
						| TCast _ -> ret
						| _ -> mk_cast (gen.greal_type t1) e
					else
						handle e t1 t2
				in
				let stl = gen.greal_type_param (TClassDecl sup) stl in
				let args,rt = get_fun (apply_params sup.cl_params stl cf.cf_type) in
				let eparams = List.map2 (fun e (_,_,t) ->
					handle (run e) t e.etype
				) (wrap_rest_args gen (TFun (args,rt)) eparams e.epos) args in
				{ e with eexpr = TNew(cl, tparams, eparams) }
			with | Not_found ->
				gen.gcon.warning "No overload found for this constructor call" e.epos;
				{ e with eexpr = TNew(cl, tparams, List.map run eparams) })
			| TUnop((Increment | Decrement) as op, flag, ({ eexpr = TArray (arr, idx) } as e2))
				when (match follow arr.etype with TInst({ cl_path = ["cs"],"NativeArray" },_) -> true | _ -> false) ->
				{ e with eexpr = TUnop(op, flag, { e2 with eexpr = TArray(run arr, idx) })}
			| TArray(arr, idx) ->
				let arr_etype = match follow arr.etype with
					| (TInst _ as t) -> t
					| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
						follow (Abstract.get_underlying_type a pl)
					| t -> t
				in
				let idx = run idx in
				let idx = match gen.greal_type idx.etype with
					| TAbstract({ a_path = [],"Int" },_) -> idx
					| _ -> match handle idx gen.gcon.basic.tint (gen.greal_type idx.etype) with
						| ({ eexpr = TCast _ } as idx) -> idx
						| idx -> mk_cast gen.gcon.basic.tint idx
				in
				let e = { e with eexpr = TArray(run arr, idx) } in
				(* get underlying class (if it's a class *)
				(match arr_etype with
					| TInst({ cl_path = ["cs"],"NativeArray" }, _) when
							(match Abstract.follow_with_abstracts e.etype with TInst _ | TEnum _ -> true | _ -> false)
							|| Common.defined gen.gcon Define.EraseGenerics
						->
						mk_cast e.etype e
					| TInst(cl, params) ->
						(* see if it implements ArrayAccess *)
						(match cl.cl_array_access with
							| None -> e
							| Some t ->
								(* if it does, apply current parameters (and change them) *)
								(* let real_t = apply_params_internal (List.map (gen.greal_type_param (TClassDecl cl))) cl params t in *)
								let param = apply_params cl.cl_params (gen.greal_type_param (TClassDecl cl) params) t in
								let real_t = apply_params cl.cl_params params param in
								(* see if it needs a cast *)

								fastcast_if_needed gen e (gen.greal_type e.etype) (gen.greal_type real_t)
								(* handle (e) (gen.greal_type e.etype) (gen.greal_type real_t) *)
						)
					| _ -> Type.map_expr run e)
			| TVar (v, eopt) ->
				{ e with eexpr = TVar (v, match eopt with
						| None -> eopt
						| Some e -> Some( handle (run e) v.v_type e.etype ))
				}
			(* FIXME deal with in_value when using other statements that may not have a TBlock wrapped on them *)
			| TIf (econd, ethen, Some(eelse)) when was_in_value ->
				{ e with eexpr = TIf (handle (run econd) gen.gcon.basic.tbool econd.etype, handle (run ethen) e.etype ethen.etype, Some( handle (run eelse) e.etype eelse.etype ) ) }
			| TIf (econd, ethen, eelse) ->
				{ e with eexpr = TIf (handle (run econd) gen.gcon.basic.tbool econd.etype, (in_value := false; run (mk_block ethen)), Option.map (fun e -> in_value := false; run (mk_block e)) eelse) }
			| TWhile (econd, e1, flag) ->
				{ e with eexpr = TWhile (handle (run econd) gen.gcon.basic.tbool econd.etype, (in_value := false; run (mk_block e1)), flag) }
			| TSwitch (cond, el_e_l, edef) ->
				{ e with eexpr = TSwitch(run cond, List.map (fun (el,e) -> (List.map run el, (in_value := false; run (mk_block e)))) el_e_l, Option.map (fun e -> in_value := false; run (mk_block e)) edef) }
			| TFor (v,cond,e1) ->
				{ e with eexpr = TFor(v, run cond, (in_value := false; run (mk_block e1))) }
			| TTry (e, ve_l) ->
				{ e with eexpr = TTry((in_value := false; run (mk_block e)), List.map (fun (v,e) -> in_value := false; (v, run (mk_block e))) ve_l) }
			| TBlock el ->
				let i = ref 0 in
				let len = List.length el in
				{ e with eexpr = TBlock ( List.map (fun e ->
					incr i;
					if !i <> len || not was_in_value then
						in_value := false;
					run e
				) el ) }
			| TCast (expr, md) when ExtType.is_void (follow e.etype) ->
				run expr
			| TCast (expr, md) ->
				let rec get_null e =
					match e.eexpr with
					| TConst TNull -> Some e
					| TParenthesis e | TMeta(_,e) -> get_null e
					| _ -> None
				in

				(match get_null expr with
				| Some enull ->
						if gen.gcon.platform = Cs then
							{ enull with etype = gen.greal_type e.etype }
						else
							mk_cast (gen.greal_type e.etype) enull
				| _ when is_abstract_to_struct expr.etype && type_iseq gen e.etype (get_abstract_impl expr.etype) ->
					run { expr with etype = expr.etype }
				| _ when is_exactly_basic gen expr.etype e.etype ->
					run { expr with etype = expr.etype }
				| _ ->
					match gen.greal_type e.etype, gen.greal_type expr.etype with
						| (TInst(c,tl) as tinst1), TAbstract({ a_path = ["cs"],"Pointer" }, [tinst2]) when type_iseq gen tinst1 (gen.greal_type tinst2) ->
							run expr
						| _ ->
							let expr = run expr in
							let last_unsafe = gen.gon_unsafe_cast in
							gen.gon_unsafe_cast <- (fun t t2 pos -> ());
							let ret = handle expr e.etype expr.etype in
							gen.gon_unsafe_cast <- last_unsafe;
							match ret.eexpr with
								| TCast _ -> { ret with etype = gen.greal_type e.etype }
								| _ -> { e with eexpr = TCast(ret,md); etype = gen.greal_type e.etype }
				)
			(*| TCast _ ->
				(* if there is already a cast, we should skip this cast check *)
				Type.map_expr run e*)
			| TFunction f ->
				in_value := false;
				Type.map_expr run e

			| _ -> Type.map_expr run e
	in
	gen.ghandle_cast <- (fun tto tfrom expr -> handle_cast gen expr (gen.greal_type tto) (gen.greal_type tfrom));
	let map e =
		match gen.gcurrent_classfield with
		| Some cf when Meta.has (Meta.Custom ":skipCastDetect") cf.cf_meta ->
			e
		| _ ->
			run e
	in
	gen.gsyntax_filters#add name (PCustom priority) map;
	ReturnCast.configure gen
