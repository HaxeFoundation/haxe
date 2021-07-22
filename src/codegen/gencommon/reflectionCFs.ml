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
open Type
open Codegen
open Texpr.Builder
open Gencommon
open ClosuresToClass

(* ******************************************* *)
(* Reflection-enabling Class fields *)
(* ******************************************* *)
(*
	This is the most hardcore codegen part of the code. There's much to improve so this code can be more readable, but at least it's running correctly right now! This will be improved. (TODO)

	This module will create class fields that enable reflection for targets that have a slow or inexistent reflection abilities. Because of the similarity
	of strategies between what should have been different modules, they are all unified in this reflection-enabling class fields.

	They include:
		* Get(throwErrors, isCheck) / Set fields . Remember to allow implements Dynamic also.
		* Invoke fields() -> You need to configure how many invoke_field fields there will be. + invokeDynamic
		* Has field -> parameter in get field that returns __undefined__ if it doesn't exist.

		* GetType -> return the current Class<> / Enum<>
		* Fields() -> returns all the fields / static fields. Remember to allow implements Dynamic also

		* Create(arguments array), CreateEmpty - calls new() or create empty
		* getInstanceFields / getClassFields -> show even function fields, everything!

		* deleteField -> only for implements Dynamic

		for enums:
		* createEnum -> invokeField for classes
		* createEnumIndex -> use invokeField as well, and use numbers e.g. "0", "1", "2" .... For this, use "@:alias" metadata
		* getEnumConstructs -> fields()

		need to be solved outside:
		* getEnumName
		* enumIndex
		*

		need to be solved by haxe code:
		* enumParameters -> for (field in Reflect.fields(enum)) arr.push(Reflect.field(enum, field))

	Standard:
		if a class contains a @:$enum metadata, it's treated as a converted enum to class


	Optimizations:
		* if optimize is true, all fields will be hashed by the same hashing function as neko (31 bits int : always positive). Every function that expects a string for the field will expect also an int, for the hash
			a string (which is nullable for compile-time hashes) + an int.
			At compile-time, a collision will throw an error (like neko).
			At runtime, a collision will make a negative int. Negative ints will always resolve to a special Hash<> field which takes a string.
		* if optimize is true, Reflect.field/setField will be replaced by either the runtime version (with already hashed string), either by the own .Field()/.SetField() HxObject's version,
			if the type is detected to already be hxgen
		* TODO: if for() optimization for arrays is disabled, we can replace for(field in Reflect.fields(obj)) to:
			for (field in ( (Std.is(obj, HxObject) ? ((HxObject)obj).Fields() : Reflect.fields(obj)) )) // no array copying . for further optimization this could be guaranteed to return
			the already hashed fields.

	Mappings:
		* if create Dynamic class is true, TObjectDecl will be mapped to new DynamicClass(fields, [hashedFields], values)
		*

	dependencies:
		There is no big dependency from this target. Though it should be a syntax filter, mainly one of the first so most expression generation has already been done,
		while the AST has its meaning close to haxe's.
		Should run before InitFunction so it detects variables containing expressions as "always-execute" expressions, even when using CreateEmpty

		* Must run before switch() syntax changes

*)
let name = "reflection_cfs"

type rcf_hash_conflict_ctx = {
	t : t;
	add_names : texpr->texpr->texpr;
	get_conflict : texpr->texpr->texpr->texpr;
	set : texpr->texpr->texpr->texpr->texpr;
	delete : texpr->texpr->texpr->texpr;
}

type rcf_ctx =
{
	rcf_gen : generator_ctx;
	rcf_ft : ClosuresToClass.closures_ctx;
	rcf_optimize : bool;

	rcf_object_iface : tclass;
	rcf_dynamic_data_class : tclass option;

	rcf_max_func_arity : int;

	(*
		the hash lookup function. can be an inlined expr or simply a function call.
		its only needed features is that it should return the index of the key if found, and the
		complement of the index of where it should be inserted if not found (Ints).

		hash->hash_array->length->returning expression
	*)
	rcf_hash_function : texpr->texpr->texpr->texpr;

	rcf_lookup_function : texpr->texpr;

	(* hash_array->length->pos->value *)
	rcf_insert_function : texpr->texpr->texpr->texpr->texpr;

	(* hash_array->length->pos->value *)
	rcf_remove_function : texpr->texpr->texpr->texpr;

	rcf_hash_fields : (int, string) Hashtbl.t;

	rcf_hash_paths : (Globals.path * int, string) Hashtbl.t;

	rcf_hash_conflict_ctx : rcf_hash_conflict_ctx option;

	rcf_mk_exception : string -> pos -> texpr;

	(*
		main expr -> field expr -> field string -> possible hash int (if optimize) -> possible set expr -> should_throw_exceptions -> changed expression

		Changes a get / set field to the runtime resolution function
	*)
	rcf_on_getset_field : texpr->texpr->string->int32 option->texpr option->bool->texpr;

	rcf_on_call_field : texpr->texpr->string->int32 option->texpr list->texpr;
}

let new_ctx gen ft object_iface ?dynamic_data_class optimize dynamic_getset_field dynamic_call_field hash_function lookup_function insert_function remove_function hash_conflict_ctx rcf_mk_exception =
	{
		rcf_gen = gen;
		rcf_ft = ft;

		rcf_optimize = optimize;
		rcf_dynamic_data_class = dynamic_data_class;

		rcf_object_iface = object_iface;

		rcf_max_func_arity = 10;

		rcf_hash_function = hash_function;
		rcf_lookup_function = lookup_function;

		rcf_insert_function = insert_function;
		rcf_remove_function = remove_function;

		rcf_hash_fields = Hashtbl.create 100;
		rcf_hash_paths = Hashtbl.create 100;

		rcf_on_getset_field = dynamic_getset_field;
		rcf_on_call_field = dynamic_call_field;
		rcf_hash_conflict_ctx = hash_conflict_ctx;
		rcf_mk_exception = rcf_mk_exception;
	}

(*
	methods as a bool option is a little laziness of my part.
		None means that methods are included with normal fields;
		Some(true) means collect only methods
		Some(false) means collect only fields (and MethDynamic fields)
*)
let collect_fields cl (methods : bool option) =
	let collected = Hashtbl.create 0 in
	let collect cf acc =
		if Meta.has Meta.CompilerGenerated cf.cf_meta || Meta.has Meta.SkipReflection cf.cf_meta then
			acc
		else match methods, cf.cf_kind with
			| None, _ when not (Hashtbl.mem collected cf.cf_name) -> Hashtbl.add collected cf.cf_name true; ([cf.cf_name], cf) :: acc
			| Some true, Method MethDynamic -> acc
			| Some true, Method _ when not (Hashtbl.mem collected cf.cf_name) -> Hashtbl.add collected cf.cf_name true; ([cf.cf_name], cf) :: acc
			| Some false, Method MethDynamic
			| Some false, Var _ when not (Hashtbl.mem collected cf.cf_name) -> Hashtbl.add collected cf.cf_name true; ([cf.cf_name], cf) :: acc
			| _ -> acc
	in
	let collect_cfs cfs acc =
		let rec loop cfs acc =
			match cfs with
				| [] -> acc
				| hd :: tl -> loop tl (collect hd acc)
		in
		loop cfs acc
	in
	let rec loop cl acc =
		let acc = collect_cfs cl.cl_ordered_fields acc in
		match cl.cl_super with
			| None -> acc
			| Some(cl,_) ->
				if not (is_hxgen (TClassDecl cl)) then loop cl acc else acc
	in

	loop cl []

let hash_field ctx f pos =
	let h = hash f in
	(try
		let f2 = Hashtbl.find ctx.rcf_hash_paths (ctx.rcf_gen.gcurrent_path, h) in
		if f <> f2 then ctx.rcf_gen.gcon.error ("Field conflict between " ^ f ^ " and " ^ f2) pos
	with Not_found ->
		Hashtbl.add ctx.rcf_hash_paths (ctx.rcf_gen.gcurrent_path, h) f;
		Hashtbl.replace ctx.rcf_hash_fields h f);
	h

(* ( tf_args, switch_var ) *)
let field_type_args ctx pos =
	match ctx.rcf_optimize with
		| true ->
			let field_name, field_hash = alloc_var "field" ctx.rcf_gen.gcon.basic.tstring, alloc_var "hash" ctx.rcf_gen.gcon.basic.tint in

			[field_name, None; field_hash, None], field_hash
		| false ->
			let field_name = alloc_var "field" ctx.rcf_gen.gcon.basic.tstring in
			[field_name, None], field_name

let hash_field_i32 ctx pos field_name =
	let i = hash_field ctx field_name pos in
	let i = Int32.of_int (i) in
	if i < Int32.zero then
		Int32.logor (Int32.logand i (Int32.of_int 0x3FFFFFFF)) (Int32.shift_left Int32.one 30)
	else i

let switch_case ctx pos field_name =
	match ctx.rcf_optimize with
		| true ->
			let i = hash_field_i32 ctx pos field_name in
			mk (TConst (TInt i)) ctx.rcf_gen.gcon.basic.tint pos
		| false ->
			make_string ctx.rcf_gen.gcon.basic field_name pos

let call_super ctx fn_args ret_t cf cl this_t pos =
	{
		eexpr = TCall({
			eexpr = TField({ eexpr = TConst(TSuper); etype = this_t; epos = pos }, FInstance(cl,List.map snd cl.cl_params,cf));
			etype = TFun(fun_args fn_args, ret_t);
			epos = pos;
		}, List.map (fun (v,_) -> mk_local v pos) fn_args);
		etype = ret_t;
		epos = pos;
	}


let enumerate_dynamic_fields ctx cl when_found base_arr =
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let pos = cl.cl_pos in

	let vtmp = alloc_var "i" basic.tint in

	let mk_for arr len =
		let t = if ctx.rcf_optimize then basic.tint else basic.tstring in
		let convert_str e = if ctx.rcf_optimize then ctx.rcf_lookup_function e else e in
		let tmpinc = { eexpr = TUnop(Ast.Increment, Ast.Postfix, mk_local vtmp pos); etype = basic.tint; epos = pos } in
		[
			{ eexpr = TBinop(OpAssign, mk_local vtmp pos, make_int ctx.rcf_gen.gcon.basic 0 pos); etype = basic.tint; epos = pos };
			{
				eexpr = TWhile (
					{ eexpr = TBinop(Ast.OpLt, mk_local vtmp pos, len); etype = basic.tbool; epos = pos },
					mk_block (when_found (convert_str { eexpr = TArray (arr, tmpinc); etype = t; epos = pos })),
					Ast.NormalWhile
				);
				etype = basic.tvoid;
				epos = pos
			}
		]
	in

	let this_t = TInst(cl, List.map snd cl.cl_params) in
	let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
	let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

	{ eexpr = TVar (vtmp,None); etype = basic.tvoid; epos = pos }
	::
	if ctx.rcf_optimize then
		mk_for (mk_this (mk_internal_name "hx" "hashes") (gen.gclasses.nativearray basic.tint)) (mk_this (mk_internal_name "hx" "length") basic.tint)
		@
		mk_for (mk_this (mk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray basic.tint)) (mk_this (mk_internal_name "hx" "length_f") basic.tint)
		@
		(
			let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
			let ehead = mk_this (mk_internal_name "hx" "conflicts") conflict_ctx.t in
			[conflict_ctx.add_names ehead base_arr]
		)
	else
		mk_for (mk_this (mk_internal_name "hx" "hashes") (gen.gclasses.nativearray basic.tstring)) (mk_this (mk_internal_name "hx" "length") basic.tint)
		@
		mk_for (mk_this (mk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray basic.tstring)) (mk_this (mk_internal_name "hx" "length_f") basic.tint)

(* *********************
		Dynamic lookup
		*********************

		This is the behavior of standard <implements Dynamic> classes. It will replace the error throwing
		if a field doesn't exists when looking it up.

		In order for it to work, an implementation for hash_function must be created.
		hash_function is the function to be called/inlined that will allow us to lookup the hash into a sorted array of hashes.
		A binary search or linear search algorithm may be implemented. The only need is that if not found, the NegBits of
		the place where it should be inserted must be returned.
*)
let abstract_dyn_lookup_implementation ctx this field_local hash_local may_value is_float pos =
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
	let a_t = if ctx.rcf_optimize then basic.tint else basic.tstring in
	let hx_hashes = mk_this (mk_internal_name "hx" "hashes") (gen.gclasses.nativearray a_t) in
	let hx_hashes_f = mk_this (mk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray a_t) in
	let hx_dynamics = mk_this (mk_internal_name "hx" "dynamics") (gen.gclasses.nativearray t_empty) in
	let hx_dynamics_f = mk_this (mk_internal_name "hx" "dynamics_f") (gen.gclasses.nativearray basic.tfloat) in
	let hx_length = mk_this (mk_internal_name "hx" "length") (basic.tint) in
	let hx_length_f = mk_this (mk_internal_name "hx" "length_f") (basic.tint) in
	let res = alloc_var "res" basic.tint in
	let fst_hash, snd_hash, fst_dynamics, snd_dynamics, fst_length, snd_length =
		if is_float then
			hx_hashes_f, hx_hashes, hx_dynamics_f, hx_dynamics, hx_length_f, hx_length
		else
			hx_hashes, hx_hashes_f, hx_dynamics, hx_dynamics_f, hx_length, hx_length_f
	in
	let res_local = mk_local res pos in
	let gte = {
		eexpr = TBinop(Ast.OpGte, res_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
		etype = basic.tbool;
		epos = pos;
	} in
	let mk_tarray arr idx =
		{
			eexpr = TArray(arr, idx);
			etype = gen.gclasses.nativearray_type arr.etype;
			epos = pos;
		}
	in
	let ret_t = if is_float then basic.tfloat else t_dynamic in

	match may_value with
		| None ->
			(*
				var res = lookup(this.__hx_hashes/f, hash);
				if (res < 0)
				{
					res = lookup(this.__hx_hashes_f/_, hash);
					if(res < 0)
						return null;
					else
						return __hx_dynamics_f[res];
				} else {
					return __hx_dynamics[res];
				}
			*)
			let block =
			[
				{ eexpr = TVar(res, Some(ctx.rcf_hash_function hash_local fst_hash fst_length)); etype = basic.tvoid; epos = pos };
				{ eexpr = TIf(gte, mk_return (mk_tarray fst_dynamics res_local), Some({
					eexpr = TBlock(
					[
						{ eexpr = TBinop(Ast.OpAssign, res_local, ctx.rcf_hash_function hash_local snd_hash snd_length); etype = basic.tint; epos = pos };
						{ eexpr = TIf(gte, mk_return (mk_tarray snd_dynamics res_local), None); etype = ret_t; epos = pos }
					]);
					etype = ret_t;
					epos = pos;
				})); etype = ret_t; epos = pos }
			] in

			if ctx.rcf_optimize then
				let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
				let ehead = mk_this (mk_internal_name "hx" "conflicts") conflict_ctx.t in
				let vconflict = alloc_var "conflict" conflict_ctx.t in
				let local_conflict = mk_local vconflict pos in
				[mk (TIf (
					mk (TBinop (OpLt, hash_local, make_int gen.gcon.basic 0 pos)) basic.tbool pos,
					mk (TBlock [
						mk (TVar (vconflict, Some (conflict_ctx.get_conflict ehead hash_local field_local))) basic.tvoid pos;
						mk (TIf (
							mk (TBinop (OpNotEq, local_conflict, mk (TConst TNull) local_conflict.etype pos)) basic.tbool pos,
							mk_return (field local_conflict "value" t_dynamic pos),
							None
						)) basic.tvoid pos;
					]) basic.tvoid pos,
					Some (mk (TBlock block) basic.tvoid pos)
				)) basic.tvoid pos]
			else
				block
		| Some value_local ->
			(*
				//if is not float:
				//if (isNumber(value_local)) return this.__hx_setField_f(field, getNumber(value_local), false(not static));
				var res = lookup(this.__hx_hashes/f, hash);
				if (res >= 0)
				{
					return __hx_dynamics/f[res] = value_local;
				} else {
					res = lookup(this.__hx_hashes_f/_, hash);
					if (res >= 0)
					{
						__hx_dynamics_f/_.splice(res,1);
						__hx_hashes_f/_.splice(res,1);
					}
				}

				__hx_hashses/_f.insert(~res, hash);
				__hx_dynamics/_f.insert(~res, value_local);
				return value_local;
			*)
			let neg_res = { eexpr = TUnop(Ast.NegBits, Ast.Prefix, res_local); etype = basic.tint; epos = pos } in

			let res2 = alloc_var "res2" basic.tint in
			let res2_local = mk_local res2 pos in
			let gte2 = {
				eexpr = TBinop(Ast.OpGte, res2_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
				etype = basic.tbool;
				epos = pos;
			} in

			let block =
			[
				{ eexpr = TVar(res, Some(ctx.rcf_hash_function hash_local fst_hash fst_length)); etype = basic.tvoid; epos = pos };
				{
					eexpr = TIf(gte,
						mk_return { eexpr = TBinop(Ast.OpAssign, mk_tarray fst_dynamics res_local, value_local); etype = value_local.etype; epos = pos },
						Some({ eexpr = TBlock([
							{ eexpr = TVar( res2, Some(ctx.rcf_hash_function hash_local snd_hash snd_length)); etype = basic.tvoid; epos = pos };
							{
								eexpr = TIf(gte2, { eexpr = TBlock([
									ctx.rcf_remove_function snd_hash snd_length res2_local;
									ctx.rcf_remove_function snd_dynamics snd_length res2_local;
									mk (TUnop(Decrement,Postfix,snd_length)) basic.tint pos
								]); etype = t_dynamic; epos = pos }, None);
								etype = t_dynamic;
								epos = pos;
							}
						]); etype = t_dynamic; epos = pos }));
					etype = t_dynamic;
					epos = pos;
				};
				ctx.rcf_insert_function fst_hash fst_length neg_res hash_local;
				ctx.rcf_insert_function fst_dynamics fst_length neg_res value_local;
				mk (TUnop(Increment,Postfix,fst_length)) basic.tint pos;
			] in

			let block =
				if ctx.rcf_optimize then
					let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
					let ehead = mk_this (mk_internal_name "hx" "conflicts") conflict_ctx.t in
					[mk (TIf (
						mk (TBinop (OpLt, hash_local, make_int gen.gcon.basic 0 pos)) basic.tbool pos,
						conflict_ctx.set ehead hash_local field_local value_local,
						Some (mk (TBlock block) basic.tvoid pos)
					)) basic.tvoid pos]
				else
					block
			in
			block @ [mk_return value_local]

let get_delete_field ctx cl is_dynamic =
	let pos = cl.cl_pos in
	let this_t = TInst(cl, List.map snd cl.cl_params) in
	let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let tf_args, switch_var = field_type_args ctx pos in
	let local_switch_var = mk_local switch_var pos in
	let fun_type = TFun(fun_args tf_args,basic.tbool) in
	let cf = mk_class_field (mk_internal_name "hx" "deleteField") fun_type false pos (Method MethNormal) [] in
	let body = if is_dynamic then begin
		let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in
		let a_t = if ctx.rcf_optimize then basic.tint else basic.tstring in
		let hx_hashes = mk_this (mk_internal_name "hx" "hashes") (gen.gclasses.nativearray a_t) in
		let hx_hashes_f = mk_this (mk_internal_name "hx" "hashes_f") (gen.gclasses.nativearray a_t) in
		let hx_dynamics = mk_this (mk_internal_name "hx" "dynamics") (gen.gclasses.nativearray t_empty) in
		let hx_dynamics_f = mk_this (mk_internal_name "hx" "dynamics_f") (gen.gclasses.nativearray basic.tfloat) in
		let hx_length = mk_this (mk_internal_name "hx" "length") (basic.tint) in
		let hx_length_f = mk_this (mk_internal_name "hx" "length_f") (basic.tint) in
		let res = alloc_var "res" basic.tint in
		let res_local = mk_local res pos in
		let gte = {
			eexpr = TBinop(Ast.OpGte, res_local, { eexpr = TConst(TInt(Int32.zero)); etype = basic.tint; epos = pos });
			etype = basic.tbool;
			epos = pos;
		} in
		(*
			var res = lookup(this.__hx_hashes, hash);
			if (res >= 0)
			{
				__hx_dynamics.splice(res,1);
				__hx_hashes.splice(res,1);

				return true;
			} else {
				res = lookup(this.__hx_hashes_f, hash);
				if (res >= 0)
				{
					__hx_dynamics_f.splice(res,1);
					__hx_hashes_f.splice(res,1);

					return true;
				}
			}

			return false;
		*)
		let common = [
			{ eexpr = TVar(res,Some(ctx.rcf_hash_function local_switch_var hx_hashes hx_length)); etype = basic.tvoid; epos = pos };
			{
				eexpr = TIf(gte, { eexpr = TBlock([
					ctx.rcf_remove_function hx_hashes hx_length res_local;
					ctx.rcf_remove_function hx_dynamics hx_length res_local;
					mk (TUnop(Decrement,Postfix,hx_length)) basic.tint pos;
					mk_return { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos }
				]); etype = t_dynamic; epos = pos }, Some({ eexpr = TBlock([
					{ eexpr = TBinop(Ast.OpAssign, res_local, ctx.rcf_hash_function local_switch_var hx_hashes_f hx_length_f); etype = basic.tint; epos = pos };
					{ eexpr = TIf(gte, { eexpr = TBlock([
						ctx.rcf_remove_function hx_hashes_f hx_length_f res_local;
						ctx.rcf_remove_function hx_dynamics_f hx_length_f res_local;
						mk (TUnop(Decrement,Postfix,hx_length_f)) basic.tint pos;
						mk_return { eexpr = TConst(TBool true); etype = basic.tbool; epos = pos }
					]); etype = t_dynamic; epos = pos }, None); etype = t_dynamic; epos = pos }
				]); etype = t_dynamic; epos = pos }));
				etype = t_dynamic;
				epos = pos;
			};
			mk_return { eexpr = TConst(TBool false); etype = basic.tbool; epos = pos }
		] in

		if ctx.rcf_optimize then
			let v_name = match tf_args with (v,_) :: _ -> v | _ -> Globals.die "" __LOC__ in
			let local_name = mk_local v_name pos in
			let conflict_ctx = Option.get ctx.rcf_hash_conflict_ctx in
			let ehead = mk_this (mk_internal_name "hx" "conflicts") conflict_ctx.t in
			(mk (TIf (
				binop OpLt local_switch_var (make_int gen.gcon.basic 0 pos) basic.tbool pos,
				mk_return (conflict_ctx.delete ehead local_switch_var local_name),
				None
			)) basic.tvoid pos) :: common
		else
			common
	end else
	[
		mk_return { eexpr = TConst(TBool false); etype = basic.tbool; epos = pos }
	] in

	(* create function *)
	let fn =
	{
		tf_args = tf_args;
		tf_type = basic.tbool;
		tf_expr = { eexpr = TBlock(body); etype = t_dynamic; epos = pos }
	} in
	cf.cf_expr <- Some({ eexpr = TFunction(fn); etype = fun_type; epos = pos });
	cf

let is_override cl = match cl.cl_super with
	| Some (cl, _) when is_hxgen (TClassDecl cl) -> true
	| _ -> false

(* WARNING: this will only work if overloading contructors is possible on target language *)
let implement_dynamic_object_ctor ctx cl =
	let rec is_side_effects_free e =
		match e.eexpr with
			| TConst _
			| TLocal _
			| TFunction _
			| TTypeExpr _ ->
				true
			| TNew(clnew,[],params) when clnew == cl ->
				List.for_all is_side_effects_free params
			| TUnop(Increment,_,_)
			| TUnop(Decrement,_,_)
			| TBinop(OpAssign,_,_)
			| TBinop(OpAssignOp _,_,_) ->
				false
			| TUnop(_,_,e) ->
				is_side_effects_free e
			| TArray(e1,e2)
			| TBinop(_,e1,e2) ->
				is_side_effects_free e1 && is_side_effects_free e2
			| TIf(cond,e1,Some e2) ->
				is_side_effects_free cond && is_side_effects_free e1 && is_side_effects_free e2
			| TField(e,_)
			| TParenthesis e | TMeta(_,e) -> is_side_effects_free e
			| TArrayDecl el -> List.for_all is_side_effects_free el
			| TCast(e,_) -> is_side_effects_free e
			| _ -> false
	in

	let pos = cl.cl_pos in
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let hasht = if ctx.rcf_optimize then basic.tint else basic.tstring in

	(* and finally we will return a function that transforms a TObjectDecl into a new DynamicObject() call *)
	let rec loop objdecl acc acc_f =
		match objdecl with
			| [] -> acc,acc_f
			| (name,expr) :: tl ->
				let real_t = gen.greal_type expr.etype in
				match follow expr.etype with
					| TInst ( { cl_path = ["haxe"], "Int64" }, [] ) ->
						loop tl ((name, gen.ghandle_cast t_dynamic real_t expr) :: acc) acc_f
					| _ ->
						if like_float real_t && not (like_i64 real_t) then
							loop tl acc ((name, gen.ghandle_cast basic.tfloat real_t expr) :: acc_f)
						else
							loop tl ((name, gen.ghandle_cast t_dynamic real_t expr) :: acc) acc_f
	in

	let may_hash_field s =
		if ctx.rcf_optimize then begin
			mk (TConst (TInt (hash_field_i32 ctx pos s))) basic.tint pos
		end else begin
			make_string gen.gcon.basic s pos
		end
	in

	let do_objdecl e objdecl =
		let exprs_before = ref [] in
		let rec change_exprs decl acc = match decl with
			| ((name,_,_),expr) :: tl ->
				if is_side_effects_free expr then
					change_exprs tl ((name,expr) :: acc)
				else begin
					let var = mk_temp "odecl" expr.etype in
					exprs_before := { eexpr = TVar(var,Some expr); etype = basic.tvoid; epos = expr.epos } :: !exprs_before;
					change_exprs tl ((name,mk_local var expr.epos) :: acc)
				end
			| [] -> acc
		in
		let objdecl = change_exprs objdecl [] in

		let odecl, odecl_f = loop objdecl [] [] in
		let changed_expr = List.map (fun (s,e) -> (may_hash_field s,e)) in
		let odecl, odecl_f = changed_expr odecl, changed_expr odecl_f in
		let sort_fn (e1,_) (e2,_) =
			match e1.eexpr, e2.eexpr with
				| TConst(TInt i1), TConst(TInt i2) -> compare i1 i2
				| TConst(TString s1), TConst(TString s2) -> compare s1 s2
				| _ -> Globals.die "" __LOC__
		in

		let odecl, odecl_f = List.sort sort_fn odecl, List.sort sort_fn odecl_f in
		let ret = {
			e with eexpr = TNew(cl,[],
				[
					mk_nativearray_decl gen hasht (List.map fst odecl) pos;
					mk_nativearray_decl gen t_empty (List.map snd odecl) pos;
					mk_nativearray_decl gen hasht (List.map fst odecl_f) pos;
					mk_nativearray_decl gen basic.tfloat (List.map snd odecl_f) pos;
				]);
		} in
		match !exprs_before with
			| [] -> ret
			| block ->
				{
					eexpr = TBlock(List.rev block @ [ret]);
					etype = ret.etype;
					epos = ret.epos;
				}
	in
	do_objdecl

(*
	Implements:
		__hx_lookupField(field:String, throwErrors:Bool, isCheck:Bool, handleProperties:Bool, isFirst:Bool):Dynamic

		__hx_lookupField_f(field:String, throwErrors:Bool, handleProperties:Bool, isFirst:Bool):Float

		__hx_lookupSetField(field:String, value:Dynamic, handleProperties:Bool, isFirst:Bool):Dynamic;

		__hx_lookupSetField(field:String, value:Float, handleProperties:Bool, isFirst:Bool):Float;
*)
let implement_final_lookup ctx cl =
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let pos = cl.cl_pos in
	let is_override = is_override cl in

	(* let this = { eexpr = TConst(TThis); etype = TInst(cl, List.map snd cl.cl_params); epos = pos } in *)

	let mk_throw str pos =
		let e = ctx.rcf_mk_exception str pos in
		make_throw e pos
	in

	(*
		this function will create the class fields and call callback for each version

		callback : is_float fields_args switch_var throw_errors_option is_check_option value_option : texpr list
	*)
	let create_cfs is_dynamic callback =
		let create_cf is_float is_set =
			let name = mk_internal_name "hx" ( (if is_set then "lookupSetField" else "lookupField") ^ (if is_float then "_f" else "") ) in
			let field_args, switch_var = field_type_args ctx pos in
			let ret_t = if is_float then basic.tfloat else t_dynamic in
			let tf_args, throw_errors_opt =
				if is_set then
					field_args, None
				else
					let v = alloc_var "throwErrors" basic.tbool in
					field_args @ [v,None], Some v
			in
			let tf_args, is_check_opt =
				if is_set || is_float then
					tf_args, None
				else
					let v = alloc_var "isCheck" basic.tbool in
					tf_args @ [v,None], Some v
			in
			let tf_args, value_opt =
				if not is_set then
					tf_args, None
				else
					let v = alloc_var "value" ret_t in
					field_args @ [v,None], Some v
			in

			let fun_t = TFun(fun_args tf_args, ret_t) in
			let cf = mk_class_field name fun_t false pos (Method MethNormal) [] in
			let block = callback is_float field_args switch_var throw_errors_opt is_check_opt value_opt in
			let block = if not is_set then let tl = begin
				let throw_errors_local = mk_local (get throw_errors_opt) pos in
				let mk_check_throw msg =
				{
					eexpr = TIf(throw_errors_local, mk_throw msg pos, Some (mk_return (null ret_t pos)));
					etype = ret_t;
					epos = pos
				} in

				let mk_may_check_throw msg = if is_dynamic then mk_return (null ret_t pos) else mk_check_throw msg in
				if is_float then begin
					[
						mk_may_check_throw "Field not found or incompatible field type.";
					]
				end else begin
					let is_check_local = mk_local (get is_check_opt) pos in
					[
						{
							eexpr = TIf(is_check_local, mk_return (undefined pos), Some( mk_may_check_throw "Field not found." ));
							etype = ret_t;
							epos = pos;
						}
					]
				end
			end in block @ tl else block in
			cf.cf_expr <- Some(
				{
					eexpr = TFunction({
						tf_args = tf_args;
						tf_type = ret_t;
						tf_expr = { eexpr = TBlock(block); etype = ret_t; epos = pos }
					});
					etype = fun_t;
					epos = pos
				}
			);
			cf
		in
		let cfs =
		[
			create_cf false false;
			create_cf true false;
			create_cf false true;
			create_cf true true
		] in
		cl.cl_ordered_fields <- cl.cl_ordered_fields @ cfs;
		List.iter (fun cf ->
			cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
			if is_override then add_class_field_flag cf CfOverride
		) cfs
	in
	if not is_override then begin
		create_cfs false (fun is_float fields_args switch_var _ _ value_opt ->
			match value_opt with
			| None -> (* is not set *)
				[]
			| Some _ -> (* is set *)
				if is_float then
					[ mk_throw "Cannot access field for writing or incompatible type." pos ]
				else
					[ mk_throw "Cannot access field for writing." pos ]
		)
	end

(* *)
let implement_get_set ctx cl =
	let gen = ctx.rcf_gen in
	let mk_cfield is_set is_float =
		let pos = cl.cl_pos in
		let basic = ctx.rcf_gen.gcon.basic in
		let tf_args, switch_var = field_type_args ctx pos in
		let field_args = tf_args in
		let local_switch_var = { eexpr = TLocal(switch_var); etype = switch_var.v_type; epos = pos } in

		let handle_prop = alloc_var "handleProperties" basic.tbool in
		let handle_prop_local = mk_local handle_prop pos in

		let this = { eexpr = TConst TThis; etype = TInst(cl, List.map snd cl.cl_params); epos = pos } in
		let mk_this_call_raw name fun_t params =
			{ eexpr = TCall( { (mk_field_access gen this name pos) with etype = fun_t; }, params ); etype = snd (get_fun fun_t); epos = pos }
		in

		let fun_type = ref (TFun([], basic.tvoid)) in
		let fun_name = mk_internal_name "hx" ( (if is_set then "setField" else "getField") ^ (if is_float then "_f" else "") ) in
		let cfield = mk_class_field fun_name !fun_type false pos (Method MethNormal) [] in

		let maybe_cast e = e in

		let t = TInst(cl, List.map snd cl.cl_params) in

		(* if it's not latest hxgen class -> check super *)
		let mk_do_default args do_default =
			match cl.cl_super with
				| None -> fun () -> maybe_cast (do_default ())
				| Some (super, sparams) when not (is_hxgen (TClassDecl super)) ->
					fun () -> maybe_cast (do_default ())
				| _ ->
					fun () ->
						mk_return {
							eexpr = TCall(
								{ eexpr = TField({ eexpr = TConst TSuper; etype = t; epos = pos }, FInstance(cl, List.map snd cl.cl_params, cfield)); etype = !fun_type; epos = pos },
								(List.map (fun (v,_) -> mk_local v pos) args) );
							etype = if is_float then basic.tfloat else t_dynamic;
							epos = pos;
						};
		in

		(* if it is set function, there are some different set fields to do *)
		let do_default, do_field, tf_args = if is_set then begin
			let value_var = alloc_var "value" (if is_float then basic.tfloat else t_dynamic) in
			let value_local = { eexpr = TLocal(value_var); etype = value_var.v_type; epos = pos } in
			let tf_args = tf_args @ [value_var,None; handle_prop, None; ] in
			let lookup_name = mk_internal_name "hx" ("lookupSetField" ^ if is_float then "_f" else "") in

			let do_default =
					fun () ->
						mk_return (mk_this_call_raw lookup_name (TFun(fun_args (field_args @ [value_var,None]),value_var.v_type)) ( List.map (fun (v,_) -> mk_local v pos) field_args @ [ value_local ] ))
			in

			let do_field cf cf_type =
				let get_field ethis = { eexpr = TField (ethis, FInstance(cl, List.map snd cl.cl_params, cf)); etype = cf_type; epos = pos } in
				let this = { eexpr = TConst(TThis); etype = t; epos = pos } in
				let value_local = if is_float then match follow cf_type with
					| TInst({ cl_kind = KTypeParameter _ }, _) ->
						mk_cast t_dynamic value_local
					| _ ->
						value_local
					else
						value_local
				in

				let ret =
				{
					eexpr = TBlock([
						{
							eexpr = TBinop(Ast.OpAssign,
								get_field this,
								mk_cast cf_type value_local);
							etype = cf_type;
							epos = pos;
						};
						mk_return value_local
					]);
					etype = cf_type;
					epos = pos;
				} in
				match cf.cf_kind with
					| Var { v_write = AccCall } ->
						let bl =
						[
							mk_this_call_raw ("set_" ^ cf.cf_name) (TFun(["value",false,cf.cf_type], cf.cf_type)) [ value_local ];
							mk_return value_local
						] in
						if not (Type.is_physical_field cf) then
							{ eexpr = TBlock bl; etype = value_local.etype; epos = pos }
						else
							{
								eexpr = TIf(
									handle_prop_local,
									{ eexpr = TBlock bl; etype = value_local.etype; epos = pos },
									Some ret);
								etype = value_local.etype;
								epos = pos;
							}
					| _ ->
						ret
			in

			(mk_do_default tf_args do_default, do_field, tf_args)
		end else begin
			let throw_errors = alloc_var "throwErrors" basic.tbool in
			let throw_errors_local = mk_local throw_errors pos in
			let do_default, tf_args = if not is_float then begin
				let is_check = alloc_var "isCheck" basic.tbool in
				let is_check_local = mk_local is_check pos in

				let tf_args = tf_args @ [ throw_errors,None; ] in

				(* default: if (isCheck) return __undefined__ else if(throwErrors) throw "Field not found"; else return null; *)
				let lookup_name = mk_internal_name "hx" "lookupField" in
				let do_default =
						fun () ->
							mk_return (mk_this_call_raw lookup_name (TFun(fun_args (field_args @ [throw_errors,None;is_check,None; ]),t_dynamic)) ( List.map (fun (v,_) -> mk_local v pos) field_args @ [ throw_errors_local; is_check_local; ] ))
				in

				(do_default, tf_args @ [ is_check,None; handle_prop,None; ])
			end else begin
				let tf_args = tf_args @ [ throw_errors,None; ] in

				let lookup_name = mk_internal_name "hx" "lookupField_f" in
				let do_default =
						fun () ->
							mk_return (mk_this_call_raw lookup_name (TFun(fun_args (field_args @ [throw_errors,None; ]),basic.tfloat)) ( List.map (fun (v,_) -> mk_local v pos) field_args @ [ throw_errors_local; ] ))
				in

				(do_default, tf_args @ [ handle_prop,None; ])
			end in

			let get_field cf cf_type ethis cl name =
				match cf.cf_kind with
					| Var { v_read = AccCall } when not (Type.is_physical_field cf) ->
						mk_this_call_raw ("get_" ^ cf.cf_name) (TFun(["value",false,cf.cf_type], cf.cf_type)) []
					| Var { v_read = AccCall } ->
						{
							eexpr = TIf(
								handle_prop_local,
								mk_this_call_raw ("get_" ^ cf.cf_name) (TFun(["value",false,cf.cf_type], cf.cf_type)) [],
								Some { eexpr = TField (ethis, FInstance(cl, List.map snd cl.cl_params, cf)); etype = cf_type; epos = pos }
							);
							etype = cf_type;
							epos = pos;
						}
					| Var _
					| Method MethDynamic -> { eexpr = TField (ethis, FInstance(cl,List.map snd cl.cl_params,cf)); etype = cf_type; epos = pos }
					| _ ->
							{ eexpr = TField (this, FClosure(Some (cl,List.map snd cl.cl_params), cf)); etype = cf_type; epos = pos }
			in

			let do_field cf cf_type =
				let this = { eexpr = TConst(TThis); etype = t; epos = pos } in
				match is_float, follow cf_type with
					| true, TInst( { cl_kind = KTypeParameter _ }, _ ) ->
						mk_return (mk_cast basic.tfloat (mk_cast t_dynamic (get_field cf cf_type this cl cf.cf_name)))
					| _ ->
						mk_return (maybe_cast (get_field cf cf_type this cl cf.cf_name ))
			in
			(mk_do_default tf_args do_default, do_field, tf_args)
		end in

		let get_fields() =
			let ret = collect_fields cl ( if is_float || is_set then Some (false) else None ) in
			let ret = if is_set then List.filter (fun (_,cf) ->
				match cf.cf_kind with
				(* | Var { v_write = AccNever } -> false *)
				| _ -> not (Meta.has Meta.ReadOnly cf.cf_meta)) ret
			else
				List.filter (fun (_,cf) ->
				match cf.cf_kind with
				(* | Var { v_read = AccNever } -> false *)
				| _ -> true) ret in
			if is_float then
				List.filter (fun (_,cf) -> (* TODO: maybe really apply_params in cf.cf_type. The benefits would be limited, though *)
					match follow (ctx.rcf_gen.greal_type (ctx.rcf_gen.gfollow#run_f cf.cf_type)) with
						| TDynamic _ | TMono _
						| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
						| t when like_float t && not (like_i64 t) -> true
						| _ -> false
				) ret
			else
				(* dynamic will always contain all references *)
				ret
		in

		(* now we have do_default, do_field and tf_args *)
		(* so create the switch expr *)
		fun_type := TFun(List.map (fun (v,_) -> (v.v_name, false, v.v_type)) tf_args, if is_float then basic.tfloat else t_dynamic );
		let has_fields = ref false in

		let content =
			let fields = get_fields() in
			let fields = List.filter
				(fun (_, cf) -> match is_set, cf.cf_kind with
					| true, Var { v_write = AccCall } -> true
					| false, Var { v_read = AccCall } -> true
					| _ -> Type.is_physical_field cf && not (has_meta Meta.ReadOnly cf.cf_meta)
				)
				fields
			in
			(if fields <> [] then has_fields := true);
			let cases = List.map (fun (names, cf) ->
				(if names = [] then Globals.die "" __LOC__);
				(List.map (switch_case ctx pos) names, do_field cf cf.cf_type)
			) fields in
			let default = Some(do_default()) in

			mk_block { eexpr = TSwitch(local_switch_var, cases, default); etype = basic.tvoid; epos = pos }
		in

		let is_override = match cl.cl_super with
			| Some (cl, _) when is_hxgen (TClassDecl cl) -> true
			| _ -> false
		in

		if !has_fields || (not is_override) then begin
			let func =
			{
				tf_args = tf_args;
				tf_type = if is_float then basic.tfloat else t_dynamic;
				tf_expr = content;
			} in

			let func = { eexpr = TFunction(func); etype = !fun_type; epos = pos } in

			cfield.cf_type <- !fun_type;
			cfield.cf_expr <- Some func;

			cl.cl_ordered_fields <- cl.cl_ordered_fields @ [cfield];
			cl.cl_fields <- PMap.add fun_name cfield cl.cl_fields;

			(if is_override then add_class_field_flag cfield CfOverride)
		end else ()
	in
	mk_cfield true true;
	mk_cfield true false;
	mk_cfield false false;
	mk_cfield false true

let implement_getFields ctx cl =
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let pos = cl.cl_pos in

	(*
		function __hx_getFields(baseArr:Array<String>)
		{
			//add all variable fields
			//then:
			super.__hx_getFields(baseArr);
		}
	*)
	let name = mk_internal_name "hx" "getFields" in
	let v_base_arr = alloc_var "baseArr" (basic.tarray basic.tstring) in
	let base_arr = mk_local v_base_arr pos in

	let tf_args = [(v_base_arr,None)] in
	let t = TFun(fun_args tf_args, basic.tvoid) in
	let cf = mk_class_field name t false pos (Method MethNormal) [] in

	let e_pushfield = mk_field_access gen base_arr "push" pos in
	let mk_push value = mk (TCall (e_pushfield, [value])) basic.tint pos in

	let has_value = ref false in
	let map_fields =
		List.map (fun (_,cf) ->
			match cf.cf_kind with
				| Var _
				| Method MethDynamic when not (has_class_field_flag cf CfOverride) ->
					has_value := true;
					mk_push (make_string gen.gcon.basic cf.cf_name pos)
				| _ -> null basic.tvoid pos
		)
	in

	(*
		if it is first_dynamic, then we need to enumerate the dynamic fields
	*)
	let exprs =
		if is_override cl then
			let tparams = List.map snd cl.cl_params in
			let esuper = mk (TConst TSuper) (TInst(cl, tparams)) pos in
			let efield = mk (TField (esuper, FInstance (cl, tparams, cf))) t pos in
			[mk (TCall (efield, [base_arr])) basic.tvoid pos]
		else
			[]
	in

	let exprs = map_fields (collect_fields cl (Some false)) @ exprs in

	cf.cf_expr <- Some {
		eexpr = TFunction({
			tf_args = tf_args;
			tf_type = basic.tvoid;
			tf_expr = mk (TBlock exprs) basic.tvoid pos
		});
		etype = t;
		epos = pos
	};

	if !has_value || not (is_override cl) then begin
		cl.cl_ordered_fields <- cl.cl_ordered_fields @ [cf];
		cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields;
		(if is_override cl then add_class_field_flag cf CfOverride)
	end


let implement_invokeField ctx slow_invoke cl =
	(*
		There are two ways to implement an haxe reflection-enabled class:
		When we extend a non-hxgen class, and when we extend the base HxObject class.

		Because of the added boiler plate we'd add every time we extend a non-hxgen class to implement a big IHxObject
		interface, we'll handle the cases differently when implementing each interface.

		At the IHxObject interface, there's only invokeDynamic(field, args[]), while at the HxObject class there are
		the other, more optimized methods, that follow the Function class interface.

		Since this will only be called by the Closure class, this conversion can be properly dealt with later.

		TODO: create the faster version. By now only invokeDynamic will be implemented
	*)
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let pos = cl.cl_pos in

	let has_method = ref false in

	let is_override = ref false in
	let rec extends_hxobject cl =
		match cl.cl_super with
			| None -> true
			| Some (cl,_) when is_hxgen (TClassDecl cl) -> is_override := true; extends_hxobject cl
			| _ -> false
	in

	let field_args, switch_var = field_type_args ctx cl.cl_pos in
	let field_args_exprs = List.map (fun (v,_) -> mk_local v pos) field_args in

	let dynamic_arg = alloc_var "dynargs" (gen.gclasses.nativearray t_dynamic) in
	let all_args = field_args @ [ dynamic_arg, None ] in
	let fun_t = TFun(fun_args all_args, t_dynamic) in

	let this_t = TInst(cl, List.map snd cl.cl_params) in
	let this = { eexpr = TConst(TThis); etype = this_t; epos = pos } in

	let mk_this_call_raw name fun_t params =
		{ eexpr = TCall( { (mk_field_access gen this name pos) with etype = fun_t }, params ); etype = snd (get_fun fun_t); epos = pos }
	in

	let extends_hxobject = extends_hxobject cl in
	ignore extends_hxobject;

	(* creates a invokeField of the class fields listed here *)
	(*
		function invokeField(field, dynargs)
		{
			switch(field)
			{
				case "a": this.a(dynargs[0], dynargs[1], dynargs[2]...);
				default: super.invokeField //or this.getField(field).invokeDynamic(dynargs)
			}
		}
	*)

	let dyn_fun = mk_class_field (mk_internal_name "hx" "invokeField") fun_t false cl.cl_pos (Method MethNormal) [] in

	let mk_switch_dyn cfs old =
		let get_case (names,cf) =
			has_method := true;
			let i = ref 0 in
			let dyn_arg_local = mk_local dynamic_arg pos in
			let length_name = match ctx.rcf_gen.gcon.platform with Cs -> "Length" | _ -> "length" in
			let dyn_arg_length = field dyn_arg_local length_name ctx.rcf_gen.gcon.basic.tint pos in
			let cases = List.map (switch_case ctx pos) names in

			let mk_this_call cf params =
				let t = apply_params cf.cf_params (List.map (fun _ -> t_dynamic) cf.cf_params) cf.cf_type in
				mk_this_call_raw cf.cf_name t params
			in
			(cases,
				mk_return (
					mk_this_call cf (List.map (fun (name,optional,t) ->
						let idx = make_int ctx.rcf_gen.gcon.basic !i pos in
						let ret = { eexpr = TArray(dyn_arg_local, idx); etype = t_dynamic; epos = pos } in
						let ret =
							if ExtType.is_rest t then
								{ ret with eexpr = TUnop(Spread,Prefix,{ ret with etype = t }) }
							else
								ret
						in
						incr i;
						if optional then
							let condition = binop OpGt dyn_arg_length idx ctx.rcf_gen.gcon.basic.tbool pos in
							mk (TIf (condition, ret, Some (make_null ret.etype pos))) ret.etype pos
						else
							ret
					) (fst (get_fun (cf.cf_type))))
				)
			)
		in

		let cfs = List.filter (fun (_,cf) -> match cf.cf_kind with
			| Method _ -> if has_class_field_flag cf CfOverride then false else true
			| _ -> true) cfs
		in

		let cases = List.map get_case cfs in
		let cases = match old with
			| [] -> cases
			| _ ->
				let ncases = List.map (fun cf -> switch_case ctx pos cf.cf_name) old in
				( ncases, mk_return (slow_invoke this (mk_local (fst (List.hd field_args)) pos) (mk_local dynamic_arg pos)) ) :: cases
		in

		let default = if !is_override then
			mk_return (call_super ctx all_args t_dynamic dyn_fun cl this_t pos)
		else (
			let field = begin
				let fun_name = mk_internal_name "hx" "getField" in
				let tf_args, _ = field_type_args ctx pos in
				let tf_args, args = fun_args tf_args, field_args_exprs in

				let tf_args, args = tf_args @ ["throwErrors",false, basic.tbool],       args @ [make_bool gen.gcon.basic true pos] in
				let tf_args, args = tf_args @ ["isCheck", false, basic.tbool],          args @ [make_bool gen.gcon.basic false pos] in
				let tf_args, args = tf_args @ ["handleProperties", false, basic.tbool], args @ [make_bool gen.gcon.basic false pos] in

				mk (TCall ({ (mk_field_access gen this fun_name pos) with etype = TFun(tf_args, t_dynamic) }, args)) t_dynamic pos
			end in
			let field = mk_cast (TInst(ctx.rcf_ft.func_class,[])) field in
			mk_return {
				eexpr = TCall(
					mk_field_access gen field (mk_internal_name "hx" "invokeDynamic") pos,
					[mk_local dynamic_arg pos]);
				etype = t_dynamic;
				epos = pos
			} )
		in

		{
			eexpr = TSwitch(mk_local switch_var pos, cases, Some default);
			etype = basic.tvoid;
			epos = pos;
		}
	in

	let contents =
		let nonstatics = collect_fields cl (Some true) in

		let old_nonstatics = ref [] in

		let nonstatics =
			List.filter (fun (n,cf) ->
				let is_old = not (PMap.mem cf.cf_name cl.cl_fields) || has_class_field_flag cf CfOverride in
				(if is_old then old_nonstatics := cf :: !old_nonstatics);
				not is_old
			) nonstatics
		in

		mk_switch_dyn nonstatics !old_nonstatics
	in

	dyn_fun.cf_expr <- Some
		{
			eexpr = TFunction(
			{
				tf_args = all_args;
				tf_type = t_dynamic;
				tf_expr = mk_block contents;
			});
			etype = TFun(fun_args all_args, t_dynamic);
			epos = pos;
		};
	if !is_override && not (!has_method) then () else begin
		cl.cl_ordered_fields <- cl.cl_ordered_fields @ [dyn_fun];
		cl.cl_fields <- PMap.add dyn_fun.cf_name dyn_fun cl.cl_fields;
		(if !is_override then add_class_field_flag dyn_fun CfOverride)
	end

let implement_varargs_cl ctx cl =
	let pos = cl.cl_pos in
	let gen = ctx.rcf_gen in

	let this_t = TInst(cl, List.map snd cl.cl_params) in
	let this = { eexpr = TConst(TThis); etype = this_t ; epos = pos } in
	let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

	let invokedyn = mk_internal_name "hx" "invokeDynamic" in
	let idyn_t = TFun([mk_internal_name "fn" "dynargs", false, gen.gclasses.nativearray t_dynamic], t_dynamic) in
	let this_idyn = mk_this invokedyn idyn_t in

	let map_fn arity ret vars api =

		let rec loop i acc =
			if i < 0 then
				acc
			else
				let obj = api i t_dynamic None in
				loop (i - 1) (obj :: acc)
		in

		let call_arg = if arity = (-1) then
			api (-1) t_dynamic None
		else if arity = 0 then
			null (gen.gclasses.nativearray t_empty) pos
		else
			mk_nativearray_decl gen t_empty (loop (arity - 1) []) pos
		in

		let expr = {
			eexpr = TCall(
				this_idyn,
				[ call_arg ]
			);
			etype = t_dynamic;
			epos = pos
		} in

		let expr = if like_float ret && not (like_int ret) then mk_cast ret expr else expr in

		mk_return expr
	in

	let all_cfs = List.filter (fun cf -> cf.cf_name <> "new" && cf.cf_name <> (invokedyn) && match cf.cf_kind with Method _ -> true | _ -> false) (ctx.rcf_ft.map_base_classfields cl map_fn) in

	cl.cl_ordered_fields <- cl.cl_ordered_fields @ all_cfs;
	List.iter (fun cf ->
		cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
	) all_cfs;

	List.iter (fun cf ->
		add_class_field_flag cf CfOverride
	) cl.cl_ordered_fields

let implement_closure_cl ctx cl =
	let pos = cl.cl_pos in
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in

	let field_args, _ = field_type_args ctx pos in
	let obj_arg = alloc_var "target" (TInst(ctx.rcf_object_iface, [])) in

	let this_t = TInst(cl, List.map snd cl.cl_params) in
	let this = { eexpr = TConst(TThis); etype = this_t ; epos = pos } in
	let mk_this field t = { (mk_field_access gen this field pos) with etype = t } in

	let tf_args = field_args @ [obj_arg, None] in
	let cfs, ctor_body = List.fold_left (fun (acc_cf,acc_expr) (v,_) ->
		let cf = mk_class_field v.v_name v.v_type false pos (Var { v_read = AccNormal; v_write = AccNormal } ) [] in
		let expr = { eexpr = TBinop(Ast.OpAssign, mk_this v.v_name v.v_type, mk_local v pos); etype = v.v_type; epos = pos } in
		(cf :: acc_cf, expr :: acc_expr)
	) ([], [])	tf_args in

	let map_fn arity ret vars api =
		let this_obj = mk_this "target" (TInst(ctx.rcf_object_iface, [])) in

		let rec loop i acc =
			if i < 0 then
				acc
			else
				let obj = api i t_dynamic None in
				loop (i - 1) (obj :: acc)
		in

		let call_arg = if arity = (-1) then
			api (-1) t_dynamic None
		else if arity = 0 then
			null (gen.gclasses.nativearray t_empty) pos
		else
			mk_nativearray_decl gen t_empty  (loop (arity - 1) []) pos
		in

		let expr = {
			eexpr = TCall(
				mk_field_access gen this_obj (mk_internal_name "hx" "invokeField") pos,
				(List.map (fun (v,_) -> mk_this v.v_name v.v_type) field_args) @ [ call_arg ]
			);
			etype = t_dynamic;
			epos = pos
		} in

		let expr = if like_float ret && not (like_int ret) then mk_cast ret expr else expr in

		mk_return expr
	in

	let all_cfs = List.filter (fun cf -> cf.cf_name <> "new" && match cf.cf_kind with Method _ -> true | _ -> false) (ctx.rcf_ft.map_base_classfields cl map_fn) in

	List.iter (fun cf ->
		add_class_field_flag cf CfOverride
	) all_cfs;
	let all_cfs = cfs @ all_cfs in

	cl.cl_ordered_fields <- cl.cl_ordered_fields @ all_cfs;
	List.iter (fun cf ->
		cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
	) all_cfs;

	let ctor_t = TFun(fun_args tf_args, basic.tvoid) in
	let ctor_cf = mk_class_field "new" ctor_t true pos (Method MethNormal) [] in
	ctor_cf.cf_expr <- Some {
		eexpr = TFunction({
			tf_args = tf_args;
			tf_type = basic.tvoid;
			tf_expr = { eexpr = TBlock({
				eexpr = TCall({ eexpr = TConst(TSuper); etype = TInst(cl,[]); epos = pos }, [make_int ctx.rcf_gen.gcon.basic (-1) pos; make_int ctx.rcf_gen.gcon.basic (-1) pos]);
				etype = basic.tvoid;
				epos = pos
			} :: ctor_body); etype = basic.tvoid; epos = pos }
		});
		etype = ctor_t;
		epos = pos
	};

	cl.cl_constructor <- Some ctor_cf;

	let closure_fun eclosure e field is_static =
		let f = make_string gen.gcon.basic field eclosure.epos in
		let args = if ctx.rcf_optimize then [ f; { eexpr = TConst(TInt (hash_field_i32 ctx eclosure.epos field)); etype = basic.tint; epos = eclosure.epos } ] else [ f ] in
		let args = args @ [ mk_cast (TInst(ctx.rcf_object_iface, [])) e ] in

		{ eclosure with eexpr = TNew(cl,[],args) }
	in
	closure_fun

let get_closure_func ctx closure_cl =
	let gen = ctx.rcf_gen in
	let basic = gen.gcon.basic in
	let closure_func eclosure e field is_static =
		mk_cast eclosure.etype { eclosure with
			eexpr = TNew(closure_cl, [], [
				e;
				make_string gen.gcon.basic field eclosure.epos
			] @ (
				if ctx.rcf_optimize then [ { eexpr = TConst(TInt (hash_field_i32 ctx eclosure.epos field)); etype = basic.tint; epos = eclosure.epos } ] else []
			));
			etype = TInst(closure_cl,[])
		}
	in
	closure_func

(*
		main expr -> field expr -> field string -> possible set expr -> should_throw_exceptions -> changed expression

		Changes a get / set
	*
	mutable rcf_on_getset_field : texpr->texpr->string->texpr option->bool->texpr;*)

let configure_dynamic_field_access ctx =
	let gen = ctx.rcf_gen in
	let is_dynamic fexpr field =
		match (field_access_esp gen (gen.greal_type fexpr.etype) field) with
		| FEnumField _
		| FClassField _ -> false
		| _ -> true
	in

	let maybe_hash = if ctx.rcf_optimize then fun str pos -> Some (hash_field_i32 ctx pos str) else fun str pos -> None in
	DynamicFieldAccess.configure gen is_dynamic
		(fun expr fexpr field set is_unsafe ->
			let hash = maybe_hash field fexpr.epos in
			ctx.rcf_on_getset_field expr fexpr field hash set is_unsafe
		)
		(fun ecall fexpr field call_list ->
			let hash = maybe_hash field fexpr.epos in
			ctx.rcf_on_call_field ecall fexpr field hash call_list
		);
	()


(* ******************************************* *)
(* UniversalBaseClass *)
(* ******************************************* *)
(*
	Sets the universal base class for hxgen types (HxObject / IHxObject)

	dependencies:
		As a rule, it should be one of the last module filters to run so any @:hxgen class created in the process
		-Should- only run after RealTypeParams.Modf
*)
module UniversalBaseClass =
struct
	let name = "rcf_universal_base_class"
	let priority = min_dep +. 10.

	let configure gen baseclass baseinterface basedynamic =
		let rec run md =
			if is_hxgen md then
				match md with
				| TClassDecl cl when (has_class_flag cl CInterface) && cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && cl.cl_path <> basedynamic.cl_path ->
					cl.cl_implements <- (baseinterface, []) :: cl.cl_implements
				| TClassDecl ({ cl_kind = KAbstractImpl _ | KModuleFields _ }) ->
					(* don't add any base classes to abstract implementations and module field containers *)
					()
				| TClassDecl ({ cl_super = None } as cl) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && cl.cl_path <> basedynamic.cl_path ->
					cl.cl_super <- Some (baseclass,[])
				| TClassDecl ({ cl_super = Some(super,_) } as cl) when cl.cl_path <> baseclass.cl_path && cl.cl_path <> baseinterface.cl_path && not (is_hxgen (TClassDecl super)) ->
					cl.cl_implements <- (baseinterface, []) :: cl.cl_implements
				| _ ->
					()
		in
		let map md = run md; md in
		gen.gmodule_filters#add name (PCustom priority) map
end;;


(*
	Priority: must run AFTER UniversalBaseClass
*)
let priority = solve_deps name [DAfter UniversalBaseClass.priority]

let has_field_override cl name =
	try
		let cf = PMap.find name cl.cl_fields in
		add_class_field_flag cf CfOverride;
		true
	with | Not_found ->
		false

let configure ctx baseinterface ~slow_invoke =
	let run md =
		(match md with
		| TClassDecl cl when not (has_class_flag cl CExtern) && is_hxgen md && ( not (has_class_flag cl CInterface) || cl.cl_path = baseinterface.cl_path ) && (match cl.cl_kind with KAbstractImpl _ | KModuleFields _ -> false | _ -> true) ->
			if is_some cl.cl_super then begin
				ignore (has_field_override cl (mk_internal_name "hx" "setField"));
				ignore (has_field_override cl (mk_internal_name "hx" "setField_f"));
				ignore (has_field_override cl (mk_internal_name "hx" "getField_f"));
			end;

			if not (has_field_override cl (mk_internal_name "hx" "lookupField")) then implement_final_lookup ctx cl;
			if not (has_field_override cl (mk_internal_name "hx" "getField")) then implement_get_set ctx cl;
			if not (has_field_override cl (mk_internal_name "hx" "invokeField")) then implement_invokeField ctx slow_invoke cl;
			if not (has_field_override cl (mk_internal_name "hx" "getFields")) then implement_getFields ctx cl;
		| _ -> ());
		md
	in
	ctx.rcf_gen.gmodule_filters#add name (PCustom priority) run
