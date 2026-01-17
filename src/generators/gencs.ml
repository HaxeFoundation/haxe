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

(* C# code generator - main coordinator *)

open Globals
open Ast
open Type
open Gctx
open CsGlobals
open CsAst
open CsSignature
open CsPrinter

(* Get the native name of a class field (respects @:native metadata) *)
let get_native_field_name cf =
	if Meta.has Meta.Native cf.cf_meta then
		let _, args, _ = Meta.get Meta.Native cf.cf_meta in
		match args with
		| [(EConst (String (s, _)), _)] -> s
		| _ -> escape_identifier cf.cf_name
	else
		escape_identifier cf.cf_name

(* Get field name for a class, handling C# restriction where member names
   cannot be the same as the enclosing type name. *)
let get_cs_field_name c cf =
	let class_name = snd c.cl_path in
	let base_name = get_native_field_name cf in
	if base_name = class_name then base_name ^ "_" else base_name

(* Generation context *)
type gen_context = {
	com : Gctx.t;
	mutable generated_types : cs_type_def list;
	mutable main_class : path option;
}

let create_context com = {
	com = com;
	generated_types = [];
	main_class = None;
}

(* Convert Haxe binop to C# binop *)
let rec cs_binop_of_binop = function
	| OpAdd -> CsOpAdd
	| OpSub -> CsOpSub
	| OpMult -> CsOpMul
	| OpDiv -> CsOpDiv
	| OpMod -> CsOpMod
	| OpAnd -> CsOpAnd
	| OpOr -> CsOpOr
	| OpXor -> CsOpXor
	| OpShl -> CsOpShl
	| OpShr -> CsOpShr
	| OpUShr -> CsOpShr  (* C# doesn't have unsigned shift, handle separately *)
	| OpEq -> CsOpEq
	| OpNotEq -> CsOpNotEq
	| OpLt -> CsOpLt
	| OpLte -> CsOpLte
	| OpGt -> CsOpGt
	| OpGte -> CsOpGte
	| OpAssign -> CsOpAssign
	| OpBoolAnd -> CsOpBoolAnd
	| OpBoolOr -> CsOpBoolOr
	| OpInterval -> failwith "Interval operator not supported"
	| OpArrow -> failwith "Arrow operator not supported"
	| OpIn -> failwith "In operator not supported"
	| OpNullCoal -> CsOpNullCoalesce
	| OpAssignOp op -> CsOpAssignOp (cs_binop_of_binop op)

(* Convert Haxe unop to C# unop *)
let cs_unop_of_unop = function
	| Increment -> CsOpIncrement
	| Decrement -> CsOpDecrement
	| Not -> CsOpNot
	| Neg -> CsOpNeg
	| NegBits -> CsOpBitNot
	| Spread -> failwith "Spread operator not supported"

(* Expression generation context *)
type expr_context = {
	gctx : gen_context;
	mutable local_vars : (int * string) list;  (* tvar.v_id -> generated name *)
	mutable used_names : string list;  (* names already used in current scope *)
	mutable temp_count : int;
}

let create_expr_context gctx = {
	gctx = gctx;
	local_vars = [];
	used_names = [];
	temp_count = 0;
}

(* Result type for expressions that may need prefix statements *)
type cs_expr_result = {
	er_stmts : cs_stmt list;  (* prefix statements to emit before the expression *)
	er_expr : cs_expr;        (* the actual expression value *)
}

(* Get or generate name for local variable *)
let get_local_name ectx v =
	try
		List.assoc v.v_id ectx.local_vars
	with Not_found ->
		(* Find a unique name - append suffix if already used *)
		let base_name = escape_identifier v.v_name in
		let rec find_unique_name name suffix =
			let candidate = if suffix = 0 then name else Printf.sprintf "%s_%d" name suffix in
			if List.mem candidate ectx.used_names then
				find_unique_name name (suffix + 1)
			else
				candidate
		in
		let name = find_unique_name base_name 0 in
		ectx.local_vars <- (v.v_id, name) :: ectx.local_vars;
		ectx.used_names <- name :: ectx.used_names;
		name

(* Generate fresh temp variable name *)
let fresh_temp ectx =
	ectx.temp_count <- ectx.temp_count + 1;
	Printf.sprintf "_hx_tmp%d" ectx.temp_count

(* Convert Haxe constant to C# constant *)
let cs_const_of_tconst = function
	| TInt i -> CsConstInt i
	| TFloat s -> CsConstDouble (float_of_string s)
	| TString s -> CsConstString s
	| TBool b -> CsConstBool b
	| TNull -> CsConstNull
	| TThis -> failwith "TThis is not a constant"
	| TSuper -> failwith "TSuper is not a constant"

(* Convert Haxe expression to C# expression - mutually recursive with cs_stmt_of_texpr *)
let rec cs_expr_of_texpr ectx e =
	match e.eexpr with
	| TConst TThis -> CsThis
	| TConst TSuper -> CsBase
	| TConst TNull ->
		(* For Null<T> types and generic type params, generate default(T) instead of null *)
		let cs_type = cs_type_of_type ectx.gctx e.etype in
		begin match cs_type with
		| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault cs_type
		| CsTypeGenericParam _ -> CsDefault cs_type  (* C# requires default(T) for generic params *)
		| _ -> CsNull
		end
	| TConst c -> CsConst (cs_const_of_tconst c)
	| TLocal v -> CsLocal (get_local_name ectx v)
	| TArray (e1, e2) ->
		(* Check if this is array access on haxe.root.Array<T> - if so, access __a directly *)
		let is_haxe_array = match follow e1.etype with
			| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
			| _ -> false
		in
		if is_haxe_array then
			(* arr[i] -> arr.__a[i] for haxe Array *)
			CsArrayAccess (CsField (cs_expr_of_texpr ectx e1, "__a"), cs_expr_of_texpr ectx e2)
		else
			CsArrayAccess (cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
	| TBinop (op, e1, e2) ->
		(* Special handling for Null<T> comparisons with null and generic type param equality *)
		let is_null_type t = match cs_type_of_type ectx.gctx t with
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
			| _ -> false
		in
		let is_generic_param t = match cs_type_of_type ectx.gctx t with
			| CsTypeGenericParam _ -> true
			| _ -> false
		in
		let is_null_expr e = match e.eexpr with
			| TConst TNull -> true
			| _ -> false
		in
		(* Check if this is an assignment to a Haxe Array element *)
		let is_haxe_array_assign = match op, e1.eexpr with
			| OpAssign, TArray (arr, _) ->
				begin match follow arr.etype with
				| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
				| _ -> false
				end
			| _ -> false
		in
		begin match op with
		| OpAssign when is_haxe_array_assign ->
			(* Haxe Array assignment: arr[i] = v  ->  arr.__set(i, v) with return value v *)
			begin match e1.eexpr with
			| TArray (arr, idx) ->
				(* __set doesn't return a value, but assignment should evaluate to v *)
				(* We generate: (arr.__set(i, v), v) if we need the value, but for now just the call *)
				let arr_cs = cs_expr_of_texpr ectx arr in
				let idx_cs = cs_expr_of_texpr ectx idx in
				let val_cs = cs_expr_of_texpr ectx e2 in
				CsCall (CsField (arr_cs, "__set"), [idx_cs; val_cs])
			| _ -> CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
			end
		| OpAssign ->
			(* Check for dynamic field assignment: dynObj.field = value -> Runtime.SetField(obj, "field", value) *)
			begin match e1.eexpr with
			| TField (obj, FDynamic name) ->
				let obj_expr = cs_expr_of_texpr ectx obj in
				let raw_type = Type.follow_once obj.etype in
				let obj_expr = match raw_type with
					| TAbstract ({ a_path = ([], "Null") }, _) ->
						CsField (obj_expr, "value")
					| _ -> obj_expr
				in
				let val_cs = cs_expr_of_texpr ectx e2 in
				CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "SetField", [obj_expr; CsConst (CsConstString name); val_cs])
			| _ -> CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
			end
		| OpEq when is_null_type e1.etype && is_null_expr e2 ->
			(* x == null  ->  !x.hasValue *)
			CsUnop (CsOpNot, false, CsField (cs_expr_of_texpr ectx e1, "hasValue"))
		| OpEq when is_null_expr e1 && is_null_type e2.etype ->
			(* null == x  ->  !x.hasValue *)
			CsUnop (CsOpNot, false, CsField (cs_expr_of_texpr ectx e2, "hasValue"))
		| OpNotEq when is_null_type e1.etype && is_null_expr e2 ->
			(* x != null  ->  x.hasValue *)
			CsField (cs_expr_of_texpr ectx e1, "hasValue")
		| OpNotEq when is_null_expr e1 && is_null_type e2.etype ->
			(* null != x  ->  x.hasValue *)
			CsField (cs_expr_of_texpr ectx e2, "hasValue")
		| OpEq when is_generic_param e1.etype || is_generic_param e2.etype ->
			(* T == T  ->  object.Equals(a, b) for generic type params *)
			CsStaticCall (CsTypeObject, "Equals", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
		| OpNotEq when is_generic_param e1.etype || is_generic_param e2.etype ->
			(* T != T  ->  !object.Equals(a, b) for generic type params *)
			CsUnop (CsOpNot, false, CsStaticCall (CsTypeObject, "Equals", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
		| _ ->
			CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
		end
	| TUnop (Spread, _, e) ->
		(* Spread operator: in C#, this is used for Rest/params arguments.
		   The spread just unwraps the array - pass through the inner expression. *)
		cs_expr_of_texpr ectx e
	| TUnop (op, Prefix, e) ->
		CsUnop (cs_unop_of_unop op, false, cs_expr_of_texpr ectx e)
	| TUnop (op, Postfix, e) ->
		CsUnop (cs_unop_of_unop op, true, cs_expr_of_texpr ectx e)
	| TField (e, FInstance ({ cl_path = (["cs"], "NativeArray") }, _, { cf_name = "length" })) ->
		(* NativeArray.length -> array.Length *)
		CsField (cs_expr_of_texpr ectx e, "Length")
	| TField (e, FInstance (_, _, { cf_name = "length" })) when (match cs_type_of_type ectx.gctx e.etype with CsTypeString -> true | _ -> false) ->
		(* String.length -> string.Length (C# uses uppercase) *)
		CsField (cs_expr_of_texpr ectx e, "Length")
	| TField (e, FInstance (c, tl, cf)) ->
		(* Check if expression type is Null<T> - if so, access .value to unwrap *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj_expr = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let obj_expr = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				(* Null<T> -> access .value to unwrap *)
				CsField (obj_expr, "value")
			| _ -> obj_expr
		in
		CsField (obj_expr, escape_identifier cf.cf_name)
	| TField (e, FClosure (Some (c, tl), cf)) ->
		(* Check if expression type is Null<T> - if so, access .value to unwrap *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj_expr = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let obj_expr = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				(* Null<T> -> access .value to unwrap *)
				CsField (obj_expr, "value")
			| _ -> obj_expr
		in
		CsField (obj_expr, escape_identifier cf.cf_name)
	| TField (e, FClosure (None, cf)) ->
		CsField (cs_expr_of_texpr ectx e, escape_identifier cf.cf_name)
	| TField ({ etype = field_type }, FStatic (c, cf)) ->
		let path = cs_path_of_path c.cl_path in
		(* For generic classes, try to infer type arguments from field type *)
		let type_params = match follow field_type with
			| TFun (_, ret) -> begin match follow ret with
				| TInst (ret_class, ret_params) when ret_class.cl_path = c.cl_path ->
					List.map (cs_type_of_type ectx.gctx) ret_params
				| _ ->
					List.map (fun _ -> CsTypeObject) c.cl_params
			end
			| _ ->
				List.map (fun _ -> CsTypeObject) c.cl_params
		in
		CsStaticField (CsTypeClass (path, type_params), get_cs_field_name c cf)
	| TField (e, FAnon cf) ->
		(* Anonymous object field access *)
		(* First, check if expression type is Null<T> - if so, unwrap via .value *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj_expr = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let inner_type, obj_expr = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				(* Null<T> -> access .value to unwrap *)
				(inner, CsField (obj_expr, "value"))
			| _ -> (e.etype, obj_expr)
		in
		(* Check if the (unwrapped) C# type maps to a concrete class. If so, generate direct field access. *)
		let cs_type = cs_type_of_type ectx.gctx inner_type in
		begin match cs_type with
		| CsTypeClass ((["haxe"; "iterators"], "ArrayIterator"), _)
		| CsTypeClass ((["haxe"; "iterators"], "MapKeyValueIterator"), _) ->
			(* Known iterator class - generate direct field access *)
			CsField (obj_expr, escape_identifier cf.cf_name)
		| CsTypeClass ((["haxe"; "root"], "HaxeDynamicObject"), _) ->
			(* Truly anonymous - use _hx_getField *)
			let field_call = CsCall (CsField (obj_expr, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| _ -> CsCast (target_type, field_call)
			end
		| CsTypeClass (_, _) ->
			(* Some other concrete class - try direct access *)
			CsField (obj_expr, escape_identifier cf.cf_name)
		| _ ->
			(* Fallback to dynamic dispatch *)
			let field_call = CsCall (CsField (obj_expr, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| _ -> CsCast (target_type, field_call)
			end
		end
	| TField (e, FDynamic name) ->
		(* Dynamic field access - need to use reflection since C# object doesn't have arbitrary fields *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj_expr = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let obj_expr = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				(* Null<T> -> access .value to unwrap *)
				CsField (obj_expr, "value")
			| _ -> obj_expr
		in
		(* Use haxe.lang.Runtime.GetField for dynamic field access *)
		CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "GetField", [obj_expr; CsConst (CsConstString name)])
	| TField (_, FEnum (en, ef)) ->
		let path = cs_path_of_path en.e_path in
		(* Get type arguments from the expression type for generic enums like Option<T> *)
		let type_args = match follow e.etype with
			| TEnum (_, params) -> List.map (cs_type_of_type ectx.gctx) params
			| _ -> []
		in
		(* For parameterless enum constructors, access via: new EnumType<T>.ConstructorName<T>()
		   For generic enums, both the enum and inner class need type params *)
		if type_args = [] && en.e_params = [] then
			(* Non-generic enum - use static field if ef has no params *)
			CsStaticField (CsTypeClass (path, []), escape_identifier ef.ef_name)
		else begin
			(* Generic enum - need to construct the value using new NestedClass<T>() *)
			let ctor_name = escape_identifier ef.ef_name in
			let nested_path = (fst path @ [snd path], ctor_name) in
			CsNew (CsTypeClass (nested_path, type_args), [])
		end
	| TCall ({ eexpr = TField (_, FEnum (en, ef)) }, args) ->
		(* Enum constructor with parameters -> new EnumType.ConstructorName<T>(...) *)
		let enum_path = cs_path_of_path en.e_path in
		let ctor_name = escape_identifier ef.ef_name in
		(* Nested class path: EnumType.ConstructorName *)
		let nested_path = (fst enum_path @ [snd enum_path], ctor_name) in
		let args = List.map (cs_expr_of_texpr ectx) args in
		(* Get type arguments from the TCall's result type (e.etype) for generic enums *)
		let type_args = match follow e.etype with
			| TEnum (_, params) -> List.map (cs_type_of_type ectx.gctx) params
			| _ -> []
		in
		CsNew (CsTypeClass (nested_path, type_args), args)
	| TCall ({ eexpr = TField (e, FInstance (_, _, cf)) }, args)
	| TCall ({ eexpr = TField (e, FClosure (_, cf)) }, args) ->
		(* Check if expression type is Null<T> - if so, access .value to unwrap *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let obj = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				(* Null<T> -> access .value to unwrap *)
				CsField (obj, "value")
			| _ -> obj
		in
		let args = List.map (cs_expr_of_texpr ectx) args in
		CsCall (CsField (obj, get_native_field_name cf), args)
	| TCall ({ eexpr = TField (e, FAnon cf) }, args) ->
		(* Method call on anonymous/structural type.
		   First, check if expression type is Null<T> - if so, unwrap via .value *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let inner_type, obj = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				(* Null<T> -> access .value to unwrap *)
				(inner, CsField (obj, "value"))
			| _ -> (e.etype, obj)
		in
		let args = List.map (cs_expr_of_texpr ectx) args in
		(* Check if the (unwrapped) C# type maps to a concrete class (like ArrayIterator).
		   If so, we can generate direct method calls instead of dynamic dispatch. *)
		let cs_type = cs_type_of_type ectx.gctx inner_type in
		begin match cs_type with
		| CsTypeClass ((["haxe"; "iterators"], "ArrayIterator"), _)
		| CsTypeClass ((["haxe"; "iterators"], "MapKeyValueIterator"), _) ->
			(* Known iterator class - generate direct method call *)
			CsCall (CsField (obj, escape_identifier cf.cf_name), args)
		| CsTypeClass ((["haxe"; "root"], "HaxeDynamicObject"), _) ->
			(* Truly anonymous - use _hx_getField *)
			let field_call = CsCall (CsField (obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let func_type = cs_type_of_type ectx.gctx cf.cf_type in
			let casted = CsCast (func_type, field_call) in
			CsCall (casted, args)
		| CsTypeClass (path, _) ->
			(* Some other concrete class - try direct call *)
			CsCall (CsField (obj, escape_identifier cf.cf_name), args)
		| _ ->
			(* Fallback to dynamic dispatch *)
			let field_call = CsCall (CsField (obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let func_type = cs_type_of_type ectx.gctx cf.cf_type in
			let casted = CsCast (func_type, field_call) in
			CsCall (casted, args)
		end
	| TCall ({ eexpr = TField (_, FStatic ({ cl_path = (["cs"], "Syntax") }, cf)) }, args) ->
		(* cs.Syntax.code or cs.Syntax.plainCode - inline C# code injection *)
		begin match cf.cf_name, args with
		| "code", { eexpr = TConst (TString template) } :: rest ->
			(* cs.Syntax.code("template {0} {1}", arg0, arg1) - interpolated *)
			let cs_args = List.map (cs_expr_of_texpr ectx) rest in
			CsInlineCode (template, cs_args)
		| "plainCode", [{ eexpr = TConst (TString code) }] ->
			(* cs.Syntax.plainCode("raw code") - no interpolation *)
			CsRaw code
		| "code", _ ->
			CsRaw "/* invalid cs.Syntax.code call - first arg must be string literal */"
		| "plainCode", _ ->
			CsRaw "/* invalid cs.Syntax.plainCode call - arg must be string literal */"
		| meth, _ ->
			CsRaw ("/* unknown cs.Syntax method: " ^ meth ^ " */")
		end
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args) ->
		let return_type = e.etype in  (* Use the TCall's etype, not TField's *)
		let path = cs_path_of_path c.cl_path in
		(* For generic classes, infer type arguments from return type if possible *)
		let class_type_params = match follow return_type with
			| TInst (_, ret_params) when List.length ret_params = List.length c.cl_params ->
				(* Return type is same generic class - use its type params *)
				List.map (cs_type_of_type ectx.gctx) ret_params
			| _ ->
				(* Fallback: use object for each type parameter *)
				List.map (fun _ -> CsTypeObject) c.cl_params
		in
		(* Get expected parameter types for casting Dynamic args to expected types *)
		let param_types = match follow cf.cf_type with
			| TFun (params, _) -> List.map (fun (_, _, t) -> t) params
			| _ -> []
		in
		let args = List.mapi (fun i arg ->
			let cs_arg = cs_expr_of_texpr ectx arg in
			(* Cast Dynamic args to expected parameter type if needed *)
			if i < List.length param_types then
				let arg_is_dynamic = match follow arg.etype with
					| TDynamic _ -> true
					| TInst ({ cl_path = ([], "Array") }, [elem_type]) ->
						(* Array element access from Array<Dynamic> *)
						begin match follow elem_type with
						| TDynamic _ -> true
						| _ -> false
						end
					| TAbstract ({ a_path = ([], "Dynamic") }, _) -> true
					| _ -> false
				in
				let expected_type = List.nth param_types i in
				let expected_is_basic = match follow expected_type with
					| TAbstract ({ a_path = ([], ("Int" | "Float" | "Bool" | "Single")) }, _) -> true
					| TInst ({ cl_path = ([], "String") }, _) -> true
					| _ -> false
				in
				if arg_is_dynamic && expected_is_basic then
					CsCast (cs_type_of_type ectx.gctx expected_type, cs_arg)
				else
					cs_arg
			else
				cs_arg
		) args in
		(* Check if method has its own type parameters *)
		if cf.cf_params <> [] then begin
			(* Method has type params - infer from return type or method signature *)
			(* Check if the return type is a type parameter T (i.e., the method returns T directly) *)
			let returns_type_param = match cf.cf_type with
				| TFun (_, ret) ->
					begin match follow ret with
					| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
					| _ -> false
					end
				| _ -> false
			in
			let method_type_params = match follow return_type with
				| TInst (_, ret_params) when List.length ret_params = List.length cf.cf_params ->
					(* Generic return type with matching arity - use its type params *)
					List.map (cs_type_of_type ectx.gctx) ret_params
				| TInst _ | TEnum _ when returns_type_param && List.length cf.cf_params = 1 ->
					(* Method returns T directly (like createInstance<T>():T), and return type is concrete *)
					(* Use the return type itself as the type argument *)
					[cs_type_of_type ectx.gctx return_type]
				| _ -> List.map (fun _ -> CsTypeObject) cf.cf_params
			in
			CsStaticCallGeneric (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, method_type_params, args)
		end else
			CsStaticCall (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, args)
	| TCall ({ eexpr = TIdent "__cs__" }, args) ->
		(* Inline C# code: untyped __cs__("code", arg1, arg2, ...) *)
		begin match args with
		| { eexpr = TConst (TString template) } :: rest ->
			let cs_args = List.map (cs_expr_of_texpr ectx) rest in
			CsInlineCode (template, cs_args)
		| _ ->
			CsRaw "/* invalid __cs__ call */"
		end
	| TCall ({ eexpr = TConst TSuper }, _) ->
		(* super() calls should be extracted and converted to : base() initializer *)
		(* If we get here, the super call is in an unexpected location *)
		CsRaw "/* ERROR: super() call in unexpected location */"
	| TCall (e_callee, args) ->
		(* Check if calling a Dynamic-typed expression *)
		let is_dynamic_call = match follow e_callee.etype with
			| TDynamic _ -> true
			| TFun _ -> false  (* Typed function - use direct call *)
			| _ ->
				(* Check if calling a local variable typed as Dynamic *)
				begin match e_callee.eexpr with
				| TLocal v -> begin match follow v.v_type with
					| TDynamic _ -> true
					| _ -> false
					end
				| _ -> false
				end
		in
		if is_dynamic_call then begin
			(* Dynamic call: use haxe.lang.Runtime.InvokeDelegate(func, args) *)
			let func_expr = cs_expr_of_texpr ectx e_callee in
			let args_exprs = List.map (cs_expr_of_texpr ectx) args in
			(* Build an array of arguments: new haxe.root.Array<object>(new object[] { ... }) *)
			let args_array = if args_exprs = [] then
				CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
			else
				let native_array = CsNewArray (CsTypeObject, args_exprs) in
				CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
			in
			let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [func_expr; args_array]) in
			(* Cast the result to the expected return type *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			begin match result_type with
			| CsTypeObject | CsTypeDynamic -> call_expr  (* No cast needed for Dynamic/object *)
			| _ -> CsCast (result_type, call_expr)
			end
		end else begin
			let func = cs_expr_of_texpr ectx e_callee in
			(* Get expected parameter types from the function type to handle null -> Null<T> conversion *)
			let param_types = match follow e_callee.etype with
				| TFun (params, _) -> List.map (fun (_, opt, t) ->
					(* For optional params, the param type in C# should be Null<T> *)
					if opt then
						let is_already_null = match t with
							| TAbstract ({ a_path = ([], "Null") }, _) -> true
							| _ -> false
						in
						if is_already_null then
							cs_type_of_type ectx.gctx t
						else
							get_boxed_type (cs_type_of_type ectx.gctx t)
					else
						cs_type_of_type ectx.gctx t
				) params
				| _ -> []
			in
			(* Convert args, handling null -> default(Null<T>) when needed *)
			let args = List.mapi (fun i arg ->
				let is_null = match arg.eexpr with TConst TNull -> true | _ -> false in
				let expected_is_null_type = if i < List.length param_types then
					match List.nth param_types i with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
					| _ -> false
				else
					false
				in
				if is_null && expected_is_null_type then
					(* Use default(Null<T>) instead of null for Null<T> params *)
					CsDefault (List.nth param_types i)
				else
					cs_expr_of_texpr ectx arg
			) args in
			CsCall (func, args)
		end
	| TNew ({ cl_path = (["cs"], "NativeArray") }, [t], [size_expr]) ->
		(* cs.NativeArray<T>(size) -> new T[size] *)
		let elem_type = cs_type_of_type ectx.gctx t in
		let size = cs_expr_of_texpr ectx size_expr in
		CsNewArraySize (elem_type, size)
	| TNew (c, params, args) ->
		let path = cs_path_of_path c.cl_path in
		let type_params = List.map (cs_type_of_type ectx.gctx) params in
		let args = List.map (cs_expr_of_texpr ectx) args in
		CsNew (CsTypeClass (path, type_params), args)
	| TObjectDecl fields ->
		(* Create HaxeDynamicObject with initial field values using _hx_create *)
		if fields = [] then
			(* Empty object: just new HaxeDynamicObject() *)
			CsNew (CsTypeClass (NativeTypes.haxe_dynamic_object_path, []), [])
		else begin
			(* Non-empty: HaxeDynamicObject._hx_create(new object[] { "name1", val1, ... }) *)
			let field_args = List.fold_left (fun acc ((name, _, _), e) ->
				let name_expr = CsConst (CsConstString name) in
				let val_expr = cs_expr_of_texpr ectx e in
				val_expr :: name_expr :: acc
			) [] fields in
			let field_args = List.rev field_args in
			(* Create native object array with the field name/value pairs *)
			let array_expr = CsNewArray (CsTypeObject, field_args) in
			(* Wrap in haxe.root.Array<object>.ofNative for the Haxe Array type *)
			let haxe_array = CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [array_expr]) in
			CsStaticCall (CsTypeClass (NativeTypes.haxe_dynamic_object_path, []), "_hx_create", [haxe_array])
		end
	| TArrayDecl items ->
		(* Array literal [] creates a haxe.root.Array<T>, not a native C# array *)
		let array_type = cs_type_of_type ectx.gctx e.etype in
		if items = [] then
			(* Empty array: new haxe.root.Array<T>() *)
			CsNew (array_type, [])
		else begin
			(* Non-empty array: new haxe.root.Array<T>(new T[] { ... }) or use ofNative *)
			let elem_type = match follow e.etype with
				| TInst (_, [t]) -> cs_type_of_type ectx.gctx t
				| _ -> CsTypeObject
			in
			let cs_items = List.map (cs_expr_of_texpr ectx) items in
			(* Create native array and wrap with ofNative *)
			CsStaticCall (array_type, "ofNative", [CsNewArray (elem_type, cs_items)])
		end
	| TTypeExpr mt ->
		(* Convert module type to Haxe type, then to C# type *)
		let t = type_of_module_type mt in
		let cs_type = cs_type_of_type ectx.gctx t in
		(* Special case: for boxed primitives like Int/Float/Bool in typeof,
		   we need the boxed version (object) for runtime type checking *)
		let cs_type = match cs_type with
			| CsTypeInt | CsTypeDouble | CsTypeBool ->
				(* For primitives in typeof context (used in isOfType), use object
				   because Haxe's isOfType boxes values. But actually, C# can use
				   typeof(int) with is/GetType - let's keep the primitive type. *)
				cs_type
			| _ -> cs_type
		in
		CsTypeOf cs_type
	| TParenthesis e ->
		CsParens (cs_expr_of_texpr ectx e)
	| TCast (inner_e, _) ->
		(* The target type is the outer expression's type (e.etype), not the inner expression's type *)
		let target_type = cs_type_of_type ectx.gctx e.etype in
		CsCast (target_type, cs_expr_of_texpr ectx inner_e)
	| TThrow e ->
		CsThrow (cs_expr_of_texpr ectx e)
	| TMeta (_, e) ->
		cs_expr_of_texpr ectx e
	| TIf (cond, then_expr, Some else_expr) ->
		(* Check if this is a void expression - can't use ternary for void in C# *)
		if ExtType.is_void (follow e.etype) then begin
			(* Void if/else in expression context: wrap in immediately invoked Action *)
			(* ((Action)(() => { if (cond) { then } else { else } }))() *)
			let cond_cs = cs_expr_of_texpr ectx cond in
			let then_stmt = cs_stmt_of_texpr ectx then_expr in
			let else_stmt = cs_stmt_of_texpr ectx else_expr in
			let if_stmt = CsIf (cond_cs, then_stmt, Some else_stmt) in
			let lambda = CsLambda ([], CsLambdaBlock [if_stmt]) in
			let action_type = CsTypeAction [] in
			CsCall (CsCast (action_type, lambda), [])
		end else begin
			(* Ternary expression: cond ? then : else *)
			let cond = cs_expr_of_texpr ectx cond in
			let then_e = cs_expr_of_texpr ectx then_expr in
			let else_e = cs_expr_of_texpr ectx else_expr in
			CsTernary (cond, then_e, else_e)
		end
	| TBlock [] ->
		(* Empty block as expression - return default value of expected type *)
		CsDefault (cs_type_of_type ectx.gctx e.etype)
	| TBlock [single] ->
		(* Single expression block - just unwrap *)
		cs_expr_of_texpr ectx single
	| TBlock [first; { eexpr = TLocal v }] ->
		(* Common pattern from inline abstract constructors: { temp = expr; temp; }
		   Optimize to just the expression if first assigns to the same variable *)
		begin match first.eexpr with
		| TVar (v2, Some init) when v.v_id = v2.v_id ->
			(* { var temp = expr; temp; } -> just expr *)
			cs_expr_of_texpr ectx init
		| TBinop (OpAssign, { eexpr = TLocal v2 }, rhs) when v.v_id = v2.v_id ->
			(* { temp = expr; temp; } -> just expr *)
			cs_expr_of_texpr ectx rhs
		| _ ->
			(* Fall through to general block handling *)
			cs_expr_of_texpr_block ectx e [first; { eexpr = TLocal v; etype = v.v_type; epos = e.epos }]
		end
	| TBlock [{ eexpr = TVar (v1, None) }; { eexpr = TBinop (OpAssign, { eexpr = TLocal v2 }, rhs) }; last]
		when v1.v_id = v2.v_id ->
		(* Common pattern: { var temp; temp = expr; temp; } or { var temp; temp = expr; (T)temp; } -> just expr *)
		let last_var_id = match last.eexpr with
			| TLocal v3 -> Some v3.v_id
			| TCast ({ eexpr = TLocal v3 }, _) -> Some v3.v_id
			| TParenthesis { eexpr = TLocal v3 } -> Some v3.v_id
			| TParenthesis { eexpr = TCast ({ eexpr = TLocal v3 }, _) } -> Some v3.v_id
			| _ -> None
		in
		begin match last_var_id with
		| Some vid when vid = v2.v_id ->
			(* Optimize: just the rhs expression, possibly with a cast *)
			begin match last.eexpr with
			| TCast (_, _) | TParenthesis { eexpr = TCast (_, _) } ->
				(* Preserve the cast type from the last expression *)
				let target_type = cs_type_of_type ectx.gctx last.etype in
				CsCast (target_type, cs_expr_of_texpr ectx rhs)
			| _ ->
				cs_expr_of_texpr ectx rhs
			end
		| _ ->
			(* Fall through to general block handling *)
			cs_expr_of_texpr_block ectx e [{ eexpr = TVar (v1, None); etype = v1.v_type; epos = e.epos }; { eexpr = TBinop (OpAssign, { eexpr = TLocal v2; etype = v2.v_type; epos = e.epos }, rhs); etype = rhs.etype; epos = e.epos }; last]
		end
	| TBlock exprs ->
		cs_expr_of_texpr_block ectx e exprs
	| TIf _  (* TIf without else, handled as statement-as-expression *)
	| TWhile _
	| TSwitch _
	| TTry _ ->
		(* Statement used as expression - wrap in immediately invoked lambda *)
		(* ((Func<T>)(() => { <statement>; return <value>; }))() *)
		cs_expr_of_stmt_as_expr ectx e
	| TReturn _
	| TBreak
	| TContinue
	| TVar _ ->
		(* These shouldn't appear as expression values in normal code *)
		CsRaw (Printf.sprintf "/* Unsupported: %s as expression */" (match e.eexpr with
			| TReturn _ -> "TReturn"
			| TBreak -> "TBreak"
			| TContinue -> "TContinue"
			| TVar _ -> "TVar"
			| _ -> "Unknown"
		))
	| TFunction tf ->
		(* Lambda/closure *)
		(* Filter out Void parameters - C# doesn't allow 'void' as a parameter type *)
		let params = List.filter_map (fun (v, _) ->
			if ExtType.is_void (follow v.v_type) then None
			else Some {
				p_name = get_local_name ectx v;
				p_type = Some (cs_type_of_type ectx.gctx v.v_type);
				p_default = None;
				p_modifier = None;
			}
		) tf.tf_args in
		let body = match tf.tf_expr.eexpr with
			| TBlock exprs -> CsLambdaBlock (List.map (cs_stmt_of_texpr ectx) exprs)
			| TReturn (Some e) -> CsLambdaExpr (cs_expr_of_texpr ectx e)
			| _ -> CsLambdaBlock [cs_stmt_of_texpr ectx tf.tf_expr]
		in
		CsLambda (params, body)
	| TEnumParameter (e, ef, i) ->
		(* Access enum constructor parameter - need to cast to the proper subclass *)
		let obj = cs_expr_of_texpr ectx e in
		let param_name = match ef.ef_type with
			| TFun (args, _) when i < List.length args ->
				let (name, _, _) = List.nth args i in
				name
			| _ -> Printf.sprintf "_hx_p%d" i
		in
		(* Get the enum path and cast to the constructor subclass *)
		let enum_path = match follow e.etype with
			| TEnum (en, _) -> cs_path_of_path en.e_path
			| _ -> ([], "object")
		in
		let ctor_name = escape_identifier ef.ef_name in
		let nested_path = (fst enum_path @ [snd enum_path], ctor_name) in
		let cast_expr = CsCast (CsTypeClass (nested_path, []), obj) in
		CsField (cast_expr, escape_identifier param_name)
	| TEnumIndex e ->
		CsField (cs_expr_of_texpr ectx e, "_hx_index")
	| TIdent s ->
		CsLocal (escape_identifier s)

(* Helper to wrap a statement-as-expression in an immediately invoked lambda.
   Used for TIf, TSwitch, TTry, TWhile when they appear in expression context.
   Generates: ((Func<T>)(() => { <stmt as return>; }))() *)
and cs_expr_of_stmt_as_expr ectx e =
	let return_type = cs_type_of_type ectx.gctx e.etype in
	let is_void = ExtType.is_void (follow e.etype) in
	(* Convert the statement, but we need to extract the "value" from it.
	   For TTry, TIf, TSwitch, etc., the value is the last expression in each branch. *)
	let stmt_with_return = cs_stmt_with_return_inner ectx is_void e in
	let lambda = CsLambda ([], CsLambdaBlock [stmt_with_return]) in
	(* For void expressions, use Action instead of Func<void> *)
	let func_type = if is_void then CsTypeAction [] else CsTypeFunc ([], return_type) in
	CsCall (CsCast (func_type, lambda), [])

(* Convert a statement to have explicit returns for expression-as-statement conversion.
   This makes the last expression in each branch into a return statement.
   is_void: if true, don't generate return statements with values (just emit the statement) *)
and cs_stmt_with_return_inner ectx is_void e =
	match e.eexpr with
	| TTry (e1, catches) ->
		let try_body = cs_stmt_with_return_inner ectx is_void e1 in
		let catch_clauses = List.map (fun (v, catch_expr) ->
			let catch_body = cs_stmt_with_return_inner ectx is_void catch_expr in
			{
				catch_type = Some (cs_type_of_type ectx.gctx v.v_type);
				catch_name = Some (get_local_name ectx v);
				catch_when = None;
				catch_body = catch_body;
			}
		) catches in
		CsTry (try_body, catch_clauses, None)
	| TIf (cond, e_then, e_else_opt) ->
		let cond_cs = cs_expr_of_texpr ectx cond in
		let then_body = cs_stmt_with_return_inner ectx is_void e_then in
		let else_body = Option.map (cs_stmt_with_return_inner ectx is_void) e_else_opt in
		CsIf (cond_cs, then_body, else_body)
	| TSwitch sw ->
		let switch_expr = cs_expr_of_texpr ectx sw.switch_subject in
		let cs_sections = List.map (fun case ->
			let cs_labels = List.map (fun v -> CsCaseConst (cs_expr_of_texpr ectx v)) case.case_patterns in
			let case_body_with_return = cs_stmt_with_return_inner ectx is_void case.case_expr in
			{ sw_labels = cs_labels; sw_body = [case_body_with_return] }
		) sw.switch_cases in
		let cs_sections = match sw.switch_default with
			| Some def_expr ->
				let def_body = cs_stmt_with_return_inner ectx is_void def_expr in
				cs_sections @ [{ sw_labels = [CsCaseDefault]; sw_body = [def_body] }]
			| None -> cs_sections
		in
		CsSwitch (switch_expr, cs_sections)
	| TBlock exprs ->
		let (init_exprs, last_opt) = split_last exprs in
		let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
		let final_stmt = match last_opt with
			| Some last_expr -> cs_stmt_with_return_inner ectx is_void last_expr
			| None -> if is_void then CsEmpty else CsReturn None
		in
		CsBlock (init_stmts @ [final_stmt])
	| TWhile _ ->
		(* While loops as expressions are unusual - just emit the loop *)
		if is_void then
			cs_stmt_of_texpr ectx e
		else
			CsBlock [cs_stmt_of_texpr ectx e; CsReturn (Some (CsDefault (cs_type_of_type ectx.gctx e.etype)))]
	| _ ->
		(* For simple expressions, return them (or just emit as statement if void) *)
		if is_void then
			CsExprStmt (cs_expr_of_texpr ectx e)
		else
			CsReturn (Some (cs_expr_of_texpr ectx e))

(* Helper to split list into init elements and last element *)
and split_last = function
	| [] -> ([], None)
	| [x] -> ([], Some x)
	| x :: xs -> let (rest, last) = split_last xs in (x :: rest, last)

(* Convert block expression to prefix statements + final expression value.
   This is used in statement contexts where we can emit: { stmts...; var x = finalExpr; }
   instead of wrapping in a lambda. *)
and cs_expr_with_prefix ectx e : cs_expr_result =
	match e.eexpr with
	| TBlock [] ->
		{ er_stmts = []; er_expr = CsDefault (cs_type_of_type ectx.gctx e.etype) }
	| TBlock [single] ->
		cs_expr_with_prefix ectx single
	| TBlock exprs ->
		let (init_exprs, last_opt) = split_last exprs in
		let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
		begin match last_opt with
		| Some last_expr ->
			(* Recursively handle the last expression - it might also be a block *)
			let last_result = cs_expr_with_prefix ectx last_expr in
			{ er_stmts = init_stmts @ last_result.er_stmts; er_expr = last_result.er_expr }
		| None ->
			{ er_stmts = init_stmts; er_expr = CsDefault (cs_type_of_type ectx.gctx e.etype) }
		end
	| _ ->
		(* Not a block - just return the expression with no prefix statements *)
		{ er_stmts = []; er_expr = cs_expr_of_texpr ectx e }

(* Helper for block expressions that need lambda wrapping (used in pure expression context) *)
and cs_expr_of_texpr_block ectx e exprs =
	(* Block expression with multiple statements - use immediately invoked lambda *)
	(* (() => { stmt1; stmt2; return lastValue; })() *)
	let (init_exprs, last_opt) = split_last exprs in
	let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
	let return_type = cs_type_of_type ectx.gctx e.etype in
	let body_stmts = match last_opt with
		| Some last_expr ->
			let last_cs = cs_expr_of_texpr ectx last_expr in
			init_stmts @ [CsReturn (Some last_cs)]
		| None ->
			init_stmts @ [CsReturn None]
	in
	let lambda = CsLambda ([], CsLambdaBlock body_stmts) in
	(* Cast to Func<T> and invoke: ((Func<T>)(() => { ... }))() *)
	let func_type = CsTypeFunc ([], return_type) in
	CsCall (CsCast (func_type, lambda), [])

(* Convert Haxe expression to C# statement - mutually recursive with cs_expr_of_texpr *)
and cs_stmt_of_texpr ectx e =
	match e.eexpr with
	| TBlock exprs ->
		CsBlock (List.map (cs_stmt_of_texpr ectx) exprs)
	| TVar (v, init) ->
		let name = get_local_name ectx v in
		let var_type = cs_type_of_type ectx.gctx v.v_type in
		begin match init with
		| None -> CsVarDecl (name, Some var_type, None)
		| Some init_expr ->
			(* Use cs_expr_with_prefix to handle block expressions smartly *)
			let result = cs_expr_with_prefix ectx init_expr in
			let init_cs = result.er_expr in
			(* Handle type conversions *)
			let init_type = cs_type_of_type ectx.gctx init_expr.etype in
			let init_cs = match init_type, var_type with
				| (CsTypeObject | CsTypeDynamic), (CsTypeInt | CsTypeFloat | CsTypeBool | CsTypeString | CsTypeClass _) ->
					(* Dynamic -> specific type: need runtime cast *)
					CsCast (var_type, init_cs)
				| CsTypeClass ((["haxe"; "lang"], "Null"), _), (CsTypeObject | CsTypeDynamic) ->
					(* Null<T> -> object/Dynamic: unwrap via .value to get the inner value *)
					CsField (init_cs, "value")
				| _ -> init_cs
			in
			if result.er_stmts = [] then
				(* No prefix statements - just emit the variable declaration *)
				CsVarDecl (name, Some var_type, Some init_cs)
			else
				(* Has prefix statements - emit:
				   T x;
				   {
				       prefix_stmts;
				       x = finalExpr;
				   }
				   This ensures the variable is accessible after the block. *)
				CsStmtList [
					CsVarDecl (name, Some var_type, None);
					CsBlock (result.er_stmts @ [CsExprStmt (CsBinop (CsOpAssign, CsLocal name, init_cs))])
				]
		end
	| TIf (cond, then_expr, else_expr) ->
		let cond = cs_expr_of_texpr ectx cond in
		let then_stmt = cs_stmt_of_texpr ectx then_expr in
		let else_stmt = Option.map (cs_stmt_of_texpr ectx) else_expr in
		CsIf (cond, then_stmt, else_stmt)
	| TWhile (cond, body, NormalWhile) ->
		let cond = cs_expr_of_texpr ectx cond in
		let body = cs_stmt_of_texpr ectx body in
		CsWhile (cond, body)
	| TWhile (cond, body, DoWhile) ->
		let cond = cs_expr_of_texpr ectx cond in
		let body = cs_stmt_of_texpr ectx body in
		CsDoWhile (body, cond)
	| TSwitch sw ->
		let cond = cs_expr_of_texpr ectx sw.switch_subject in
		let sections = List.map (fun case ->
			let labels = List.map (fun p ->
				CsCaseConst (cs_expr_of_texpr ectx p)
			) case.case_patterns in
			let body_stmts = [cs_stmt_of_texpr ectx case.case_expr; CsBreak] in
			{ sw_labels = labels; sw_body = body_stmts }
		) sw.switch_cases in
		let sections = match sw.switch_default with
			| Some e ->
				let default_section = {
					sw_labels = [CsCaseDefault];
					sw_body = [cs_stmt_of_texpr ectx e; CsBreak]
				} in
				sections @ [default_section]
			| None ->
				(* Even for exhaustive switches, C# needs a default case for definite assignment.
				   Throw an exception to satisfy the compiler while preserving safety. *)
				let default_section = {
					sw_labels = [CsCaseDefault];
					sw_body = [CsThrowStmt (CsNew (CsTypeClass ((["System"], "InvalidOperationException"), []),
						[CsConst (CsConstString "Unexpected value")]))]
				} in
				sections @ [default_section]
		in
		CsSwitch (cond, sections)
	| TTry (body, catches) ->
		let body = cs_stmt_of_texpr ectx body in
		let catches = List.map (fun (v, e) ->
			let name = get_local_name ectx v in
			let cs_type = cs_type_of_type ectx.gctx v.v_type in
			let body = cs_stmt_of_texpr ectx e in
			{
				catch_type = Some cs_type;
				catch_name = Some name;
				catch_when = None;
				catch_body = body;
			}
		) catches in
		CsTry (body, catches, None)
	| TReturn None ->
		CsReturn None
	| TReturn (Some e) ->
		CsReturn (Some (cs_expr_of_texpr ectx e))
	| TBreak ->
		CsBreak
	| TContinue ->
		CsContinue
	| TThrow e ->
		CsThrowStmt (cs_expr_of_texpr ectx e)
	| _ ->
		(* Expression statement *)
		CsExprStmt (cs_expr_of_texpr ectx e)

(* Generate method body *)
(* param_cs_names: optional list of C# parameter names (in order) from the method signature.
   This ensures the body uses the same parameter names as the C# method signature.
   Without this, abstract @this parameters may be named differently (e.g., "this1" in AST
   but "@this" in the signature). The mapping is by position. *)
let generate_method_body gctx ?(param_cs_names=[]) e =
	let ectx = create_expr_context gctx in
	match e.eexpr with
	| TFunction tf ->
		(* Unwrap TFunction - this happens for dynamic function assignments *)
		(* Register parameter names in context, using provided names when available (by position) *)
		let param_cs_names_array = Array.of_list param_cs_names in
		List.iteri (fun i (v, _) ->
			(* Use the C# name from the signature if available at this position *)
			let param_name =
				if i < Array.length param_cs_names_array then
					param_cs_names_array.(i)
				else
					escape_identifier v.v_name
			in
			(* Directly register this name for this variable id *)
			ectx.local_vars <- (v.v_id, param_name) :: ectx.local_vars;
			ectx.used_names <- param_name :: ectx.used_names
		) tf.tf_args;
		(* Generate the inner body *)
		begin match tf.tf_expr.eexpr with
		| TBlock exprs -> List.map (cs_stmt_of_texpr ectx) exprs
		| _ -> [cs_stmt_of_texpr ectx tf.tf_expr]
		end
	| TBlock exprs ->
		List.map (cs_stmt_of_texpr ectx) exprs
	| _ ->
		[cs_stmt_of_texpr ectx e]

(* Check if a method overrides a parent class method *)
let is_override cf =
	has_class_field_flag cf CfOverride

(* Check if a non-static method should be marked virtual *)
let should_be_virtual c cf =
	(* Can't have virtual in a sealed/final class *)
	if has_class_flag c CFinal then false
	(* Can't mark as virtual if it's already an override or final *)
	else if has_class_field_flag cf CfFinal then false
	else if has_class_field_flag cf CfOverride then false
	else true

(* Find ALL interface methods that this field implements with different types.
   Returns a list of (interface_type, interface_args, interface_return_type, method_type_params)
   for each interface where the return type OR parameter types differ from the implementation.
   This is needed because C# doesn't support:
   - Covariant return types in interface implementations
   - Contravariant parameter types in interface implementations
   We need to generate explicit interface implementations for each such interface. *)
let find_variant_interface_methods gctx c cf =
	let rec check_interface acc map_parent (c_int, params) =
		(* First apply any parent type mapping to the params.
		   This is needed when traversing parent interfaces:
		   e.g., B<T> extends A<T>, and C implements B<int>
		   When checking A, params is [T] but we need [int]. *)
		let params = List.map map_parent params in
		let map_type = apply_params c_int.cl_params params in
		let acc = try
			let cf_int = PMap.find cf.cf_name c_int.cl_fields in
			match cf_int.cf_kind with
			| Method _ ->
				(* Found interface method with same name *)
				let int_type = map_type cf_int.cf_type in
				begin match follow int_type, follow cf.cf_type with
				| TFun (int_args, int_ret), TFun (impl_args, impl_ret) ->
					(* Check if return types differ (covariant return) *)
					let ret_differs = not (Type.type_iseq int_ret impl_ret) in
					(* Check if any parameter types differ (contravariant params) *)
					let args_differ =
						try
							List.exists2 (fun (_, _, int_t) (_, _, impl_t) ->
								not (Type.type_iseq int_t impl_t)
							) int_args impl_args
						with Invalid_argument _ -> true (* Different arg counts *)
					in
					if ret_differs || args_differ then
						(* Build the interface type with applied params *)
						let iface_cs_type = cs_type_of_type gctx (TInst (c_int, params)) in
						(* Get interface method's type parameters *)
						let method_type_params = List.map (fun ttp -> ttp.ttp_name) cf_int.cf_params in
						(iface_cs_type, int_args, int_ret, method_type_params) :: acc
					else
						acc
				| _ -> acc
				end
			| _ -> acc
		with Not_found -> acc
		in
		(* Also check parent interfaces - they may require explicit implementations too.
		   Pass map_type so parent interface params get properly substituted. *)
		List.fold_left (fun acc iface -> check_interface acc map_type iface) acc c_int.cl_implements
	in
	(* Check all implemented interfaces, starting with identity mapping *)
	List.fold_left (fun acc iface -> check_interface acc (fun t -> t) iface) [] c.cl_implements

(* Generate explicit interface implementations for variant return/param types.
   For each interface method with different types, we generate:
   ReturnType InterfaceName.MethodName(params) { return this.MethodName(params); }
   This delegates to the actual implementation method. *)
let generate_explicit_interface_impls gctx c cf =
	match cf.cf_kind with
	| Method MethNormal | Method MethInline ->
		let variant_interfaces = find_variant_interface_methods gctx c cf in
		List.map (fun (iface_type, int_args, int_ret, method_type_params) ->
			(* Generate params for the explicit implementation (no defaults needed) *)
			let params = List.filter_map (fun (n, _, t) ->
				if ExtType.is_void (follow t) then None
				else Some {
					p_name = escape_identifier n;
					p_type = Some (cs_type_of_type gctx t);
					p_default = None;
					p_modifier = None;
				}
			) int_args in
			(* Body: call this.MethodName(args) *)
			let arg_exprs = List.map (fun p -> CsLocal p.p_name) params in
			let call_expr = CsCall (CsField (CsThis, escape_identifier cf.cf_name), arg_exprs) in
			(* Use return for non-void, expression statement for void *)
			let is_void = ExtType.is_void (follow int_ret) in
			let body_stmt = if is_void then
				CsExprStmt call_expr
			else
				CsReturn (Some call_expr)
			in
			CsMemberMethod {
				m_name = escape_identifier cf.cf_name;
				m_return_type = cs_type_of_type gctx int_ret;
				m_access = AccessModifier.Public; (* ignored for explicit interface impl *)
				m_modifiers = [];
				m_type_params = method_type_params;
				m_params = params;
				m_body = Some [body_stmt];
				m_constraints = [];
				m_explicit_interface = Some iface_type;
			}
		) variant_interfaces
	| _ -> []

(* Generate class field as C# member *)
let generate_field gctx c cf is_static =
	(* Use get_cs_field_name which handles C# restriction where member names
	   cannot be the same as the enclosing type name. *)
	let name = get_cs_field_name c cf in
	let cs_type = cs_type_of_type gctx cf.cf_type in
	let modifiers = if is_static then [MemberModifier.Static] else [] in

	match cf.cf_kind with
	| Var { v_read = AccNormal; v_write = AccNormal }
	| Var { v_read = AccNormal; v_write = AccNever } ->
		(* Simple field *)
		let value = match cf.cf_expr with
			| Some e ->
				let ectx = create_expr_context gctx in
				Some (cs_expr_of_texpr ectx e)
			| None -> None
		in
		Some (CsMemberField {
			f_name = name;
			f_type = cs_type;
			f_access = AccessModifier.Public;
			f_modifiers = modifiers;
			f_value = value;
		})
	| Var { v_read = AccCall; _ } | Var { v_write = AccCall; _ } ->
		(* Property with custom getter/setter - skip the property declaration.
		   The getter/setter methods (get_xxx, set_xxx) are generated separately
		   as methods. In C#, auto-properties reserve the get_/set_ method names,
		   so we can't have both an auto-property AND explicit getter/setter methods. *)
		None
	| Var _ ->
		(* Simple property with no custom accessors *)
		Some (CsMemberProperty {
			prop_name = name;
			prop_type = cs_type;
			prop_access = AccessModifier.Public;
			prop_modifiers = modifiers;
			prop_getter = Some { acc_access = None; acc_body = None };
			prop_setter = Some { acc_access = None; acc_body = None };
			prop_init = None;
		})
	| Method MethNormal | Method MethInline ->
		(* Regular method *)
		let args, ret = match follow cf.cf_type with
			| TFun (args, ret) -> args, ret
			| _ -> [], cf.cf_type
		in
		(* For override methods, we need to use the parent's parameter types
		   to ensure C# compatibility. C# requires exact type match for overrides. *)
		let args, ret =
			if not is_static && is_override cf then
				let rec find_parent_types c_super tl =
					let map_type = apply_params c_super.cl_params tl in
					try
						let cf_super = PMap.find cf.cf_name c_super.cl_fields in
						match cf_super.cf_kind with
						| Method _ ->
							begin match follow (map_type cf_super.cf_type) with
							| TFun (parent_args, parent_ret) -> Some (parent_args, parent_ret)
							| _ -> None
							end
						| _ -> None
					with Not_found ->
						match c_super.cl_super with
						| Some (grandparent, tl2) -> find_parent_types grandparent (List.map map_type tl2)
						| None -> None
				in
				match c.cl_super with
				| Some (c_super, tl) ->
					begin match find_parent_types c_super tl with
					| Some (parent_args, parent_ret) -> parent_args, parent_ret
					| None -> args, ret
					end
				| None -> args, ret
			else
				args, ret
		in
		(* C# requires that all optional parameters come after all required ones.
		   Mark which optional params can have defaults (only trailing optionals).
		   If an optional param has a required param after it, it cannot have a default. *)
		let args_with_index = List.mapi (fun i arg -> (i, arg)) args in
		let last_required_index = List.fold_left (fun acc (i, (_, opt, _)) ->
			if not opt then i else acc
		) (-1) args_with_index in
		(* Filter out Void parameters - C# doesn't allow 'void' as a parameter type *)
		let filtered_args = List.filter (fun (_, _, t) -> not (ExtType.is_void (follow t))) args in
		(* Build list of C# parameter names in order.
		   This is needed because abstract methods have @this parameters where the
		   original name is "this" but gets escaped to "@this" in C#.
		   The body expression may use a renamed variable (e.g., "this1") which
		   needs to map to the escaped parameter name "@this". *)
		let param_cs_names = List.map (fun (n, _, _) ->
			escape_identifier n
		) filtered_args in
		let params = List.mapi (fun i (n, opt, t) ->
			let param_type = cs_type_of_type gctx t in
			(* Only add default for trailing optionals (after last required param) *)
			let default_val = if opt && i > last_required_index then
				(* For haxe.lang.Null<T> (struct), must use default instead of null *)
				Some (CsDefault param_type)
			else
				None
			in
			{
				p_name = escape_identifier n;
				p_type = Some param_type;
				p_default = default_val;
				p_modifier = None;
			}
		) filtered_args in
		let body = match cf.cf_expr with
			| Some e -> Some (generate_method_body gctx ~param_cs_names e)
			| None -> None
		in
		(* Extract method-level type parameters from cf.cf_params *)
		let explicit_type_params = List.map (fun ttp -> ttp.ttp_name) cf.cf_params in
		(* Get class type parameters to exclude from method type params *)
		let class_type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in
		(* Also collect type parameters used in the method signature.
		   This is needed for _Impl_ classes where the abstract's type params
		   become method-level params (like Rest<T>.append uses T but _Impl_ has no class params).
		   However, we must exclude type params that belong to the enclosing class. *)
		let return_cs_type = cs_type_of_type gctx ret in
		let param_cs_types = List.map (fun p -> p.p_type) params in
		let param_cs_types = List.filter_map (fun t -> t) param_cs_types in
		let inferred_type_params = CsSignature.get_method_type_params param_cs_types return_cs_type in
		(* Filter out class type params from inferred params *)
		let inferred_type_params = List.filter (fun p ->
			not (List.mem p class_type_params)
		) inferred_type_params in
		(* Combine explicit and inferred params, explicit first, avoiding duplicates *)
		let method_type_params = explicit_type_params @ (List.filter (fun p ->
			not (List.mem p explicit_type_params)
		) inferred_type_params) in
		(* Add virtual/override/abstract modifiers for instance methods *)
		let method_modifiers =
			if is_static then modifiers
			else if is_override cf then modifiers @ [MemberModifier.Override]
			(* Methods without a body should be abstract, not virtual *)
			else if body = None then modifiers @ [MemberModifier.Abstract]
			else if should_be_virtual c cf then modifiers @ [MemberModifier.Virtual]
			else modifiers
		in
		Some (CsMemberMethod {
			m_name = name;
			m_return_type = cs_type_of_type gctx ret;
			m_access = AccessModifier.Public;
			m_modifiers = method_modifiers;
			m_type_params = method_type_params;
			m_params = params;
			m_body = body;
			m_constraints = [];
			m_explicit_interface = None;
		})
	| Method MethDynamic ->
		(* Dynamic method - generate as delegate field
		   In C#, this becomes Action<...> (void return) or Func<...> (non-void return)
		   cs_type already handles TFun -> CsTypeAction or CsTypeFunc *)
		let value = match cf.cf_expr with
			| Some e ->
				let ectx = create_expr_context gctx in
				Some (cs_expr_of_texpr ectx e)
			| None -> None
		in
		Some (CsMemberField {
			f_name = name;
			f_type = cs_type;  (* cs_type_of_type handles TFun -> delegate type *)
			f_access = AccessModifier.Public;
			f_modifiers = modifiers;
			f_value = value;
		})
	| Method MethMacro ->
		(* Macro method - skip *)
		None

(* Extract super() call and remaining body from constructor expression *)
(* Returns (Some super_args, rest) if super call found, (None, body) otherwise *)
let rec extract_super_call e =
	match e.eexpr with
	| TCall ({ eexpr = TConst TSuper }, args) ->
		(* Found super call - return args and empty body *)
		(Some args, None)
	| TBlock el ->
		(* Look for super call in block - it should be the first statement *)
		extract_super_from_block el
	| TFunction tf ->
		(* Unwrap TFunction and look inside *)
		extract_super_call tf.tf_expr
	| _ ->
		(None, Some e)

and extract_super_from_block el =
	match el with
	| [] -> (None, None)
	| e :: rest ->
		begin match e.eexpr with
		| TCall ({ eexpr = TConst TSuper }, args) ->
			(* Found super call - return args and remaining statements *)
			let rest_expr = if rest = [] then None else Some { e with eexpr = TBlock rest } in
			(Some args, rest_expr)
		| TBlock inner ->
			(* Nested block - look inside *)
			let (super_args, inner_rest) = extract_super_from_block inner in
			begin match super_args with
			| Some args ->
				let rest_block = match inner_rest with
					| None -> rest
					| Some r -> r :: rest
				in
				let rest_expr = if rest_block = [] then None else Some { e with eexpr = TBlock rest_block } in
				(Some args, rest_expr)
			| None ->
				let rest_expr = Some { e with eexpr = TBlock el } in
				(None, rest_expr)
			end
		| _ ->
			(* Not a super call - return full block *)
			(None, Some { e with eexpr = TBlock el })
		end

(* Generate constructor - follows legacy C# target pattern *)
(* Constructor body goes directly in constructor, super() becomes : base() initializer *)
let generate_constructor gctx c cf =
	let args = match follow cf.cf_type with
		| TFun (args, _) -> args
		| _ -> []
	in
	(* C# requires that all optional parameters come after all required ones.
	   Mark which optional params can have defaults (only trailing optionals). *)
	let args_with_index = List.mapi (fun i arg -> (i, arg)) args in
	let last_required_index = List.fold_left (fun acc (i, (_, opt, _)) ->
		if not opt then i else acc
	) (-1) args_with_index in
	(* Parameters for the constructor - use actual types *)
	let ctor_params = List.mapi (fun i (n, opt, t) ->
		let param_type = cs_type_of_type gctx t in
		(* Only add default for trailing optionals (after last required param) *)
		let default_val = if opt && i > last_required_index then
			(* For haxe.lang.Null<T> (struct), must use default instead of null *)
			Some (CsDefault param_type)
		else
			None
		in
		{
			p_name = escape_identifier n;
			p_type = Some param_type;
			p_default = default_val;
			p_modifier = None;
		}
	) args in
	(* Extract super call and body from constructor expression *)
	let (super_args, body_expr) = match cf.cf_expr with
		| Some e -> extract_super_call e
		| None -> (None, None)
	in
	(* Get base class constructor types for casting super args *)
	let base_ctor_types = match c.cl_super with
		| Some (sc, _) ->
			begin match sc.cl_constructor with
			| Some ctor_cf ->
				begin match follow ctor_cf.cf_type with
				| TFun (base_args, _) -> List.map (fun (_, _, t) -> cs_type_of_type gctx t) base_args
				| _ -> []
				end
			| None -> []
			end
		| None -> []
	in
	(* Convert super args to base call, casting to expected types *)
	let base_call = match super_args with
		| Some args ->
			let ectx = create_expr_context gctx in
			let cs_args = List.mapi (fun i arg ->
				let cs_arg = cs_expr_of_texpr ectx arg in
				(* Cast to expected type if we know it *)
				if i < List.length base_ctor_types then
					let expected_type = List.nth base_ctor_types i in
					CsCast (expected_type, CsParens cs_arg)
				else
					cs_arg
			) args in
			Some cs_args
		| None -> None
	in
	(* Generate constructor body *)
	let ctor_body = match body_expr with
		| Some e -> generate_method_body gctx e
		| None -> []
	in
	[
		CsMemberConstructor {
			ctor_access = AccessModifier.Public;
			ctor_modifiers = [];
			ctor_params = ctor_params;
			ctor_base_call = base_call;
			ctor_this_call = None;
			ctor_body = ctor_body;
		}
	]

(* Check if a class inherits from HaxeObject (directly or through Haxe superclass chain) *)
let rec extends_haxe_object c =
	match c.cl_super with
	| None -> true  (* No superclass means we add HaxeObject as base, so it inherits from HaxeObject *)
	| Some (sc, _) ->
		(* Check if the superclass is a native extern class or has @:native metadata *)
		if has_class_flag sc CExtern then
			false  (* Extern classes don't inherit from HaxeObject *)
		else if Meta.has Meta.Native sc.cl_meta then
			false  (* Classes with @:native map to native C# classes, not HaxeObject *)
		else
			extends_haxe_object sc  (* Check the superclass *)

(* Generate _hx_getField, _hx_setField, _hx_getFields override methods for AOT compatibility *)
let generate_field_accessors gctx c =
	(* Only generate field accessors if the class inherits from HaxeObject *)
	if not (extends_haxe_object c) then
		[]
	else
		(* Get list of instance fields with their native names *)
		let instance_fields = List.filter_map (fun cf ->
			match cf.cf_kind with
			| Var { v_read = AccNormal; v_write = AccNormal }
			| Var { v_read = AccNormal; v_write = AccNever } ->
				Some (get_native_field_name cf, cs_type_of_type gctx cf.cf_type)
			| _ -> None
		) c.cl_ordered_fields in

		if instance_fields = [] then
			[]  (* No fields, no need for accessors *)
		else
		let field_names = List.map fst instance_fields in

		(* Generate _hx_getField override:
		   public override object _hx_getField(string name) {
		       switch (name) {
		           case "field1": return this.field1;
		           ...
		           default: return base._hx_getField(name);
		       }
		   }
		*)
		let get_field_sections = List.map (fun (name, _) ->
			{
				sw_labels = [CsCaseConst (CsConst (CsConstString name))];
				sw_body = [CsReturn (Some (CsField (CsThis, name)))];
			}
		) instance_fields in
		let get_field_default = {
			sw_labels = [CsCaseDefault];
			sw_body = [CsReturn (Some (CsCall (CsField (CsBase, "_hx_getField"), [CsLocal "name"])))];
		} in
		let get_field_method = CsMemberMethod {
			m_name = "_hx_getField";
			m_return_type = CsTypeObject;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Override];
			m_type_params = [];
			m_params = [{ p_name = "name"; p_type = Some CsTypeString; p_default = None; p_modifier = None }];
			m_body = Some [CsSwitch (CsLocal "name", get_field_sections @ [get_field_default])];
			m_constraints = [];
			m_explicit_interface = None;
		} in

		(* Generate _hx_setField override:
		   public override void _hx_setField(string name, object value) {
		       switch (name) {
		           case "field1": this.field1 = (FieldType)value; return;
		           ...
		           default: base._hx_setField(name, value); return;
		       }
		   }
		*)
		let set_field_sections = List.map (fun (name, field_type) ->
			{
				sw_labels = [CsCaseConst (CsConst (CsConstString name))];
				sw_body = [
					CsExprStmt (CsBinop (CsOpAssign, CsField (CsThis, name), CsCast (field_type, CsLocal "value")));
					CsReturn None;
				];
			}
		) instance_fields in
		let set_field_default = {
			sw_labels = [CsCaseDefault];
			sw_body = [
				CsExprStmt (CsCall (CsField (CsBase, "_hx_setField"), [CsLocal "name"; CsLocal "value"]));
				CsReturn None;
			];
		} in
		let set_field_method = CsMemberMethod {
			m_name = "_hx_setField";
			m_return_type = CsTypeVoid;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Override];
			m_type_params = [];
			m_params = [
				{ p_name = "name"; p_type = Some CsTypeString; p_default = None; p_modifier = None };
				{ p_name = "value"; p_type = Some CsTypeObject; p_default = None; p_modifier = None };
			];
			m_body = Some [CsSwitch (CsLocal "name", set_field_sections @ [set_field_default])];
			m_constraints = [];
			m_explicit_interface = None;
		} in

		(* Generate _hx_getFields override:
		   public override Array<string> _hx_getFields() {
		       return new Array<string>(new string[] { "field1", "field2", ... });
		   }
		*)
		let field_name_exprs = List.map (fun name -> CsConst (CsConstString name)) field_names in
		let array_type = CsTypeClass ((["haxe"; "root"], "Array"), [CsTypeString]) in
		let get_fields_method = CsMemberMethod {
			m_name = "_hx_getFields";
			m_return_type = array_type;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Override];
			m_type_params = [];
			m_params = [];
			(* Create Array<string> from native array: Array<string>.ofNative(new string[] {...}) *)
			m_body = Some [CsReturn (Some (CsStaticCallGeneric (array_type, "ofNative", [CsTypeString], [CsNewArray (CsTypeString, field_name_exprs)])))];
			m_constraints = [];
			m_explicit_interface = None;
		} in

		[get_field_method; set_field_method; get_fields_method]

(* Generate C# class from Haxe class *)
let generate_class gctx c =
	let path = cs_path_of_path c.cl_path in

	(* Check if this is the main class *)
	begin match gctx.com.main.main_path with
	| Some mp when mp = c.cl_path -> gctx.main_class <- Some c.cl_path
	| _ -> ()
	end;

	(* Determine modifiers *)
	let modifiers =
		(if (has_class_flag c CFinal) then [TypeModifier.Sealed] else []) @
		(if (has_class_flag c CAbstract) then [TypeModifier.Abstract] else [])
	in

	(* Generate base class reference *)
	(* If no explicit superclass, use HaxeObject as the base class for dynamic field support *)
	let base_class = match c.cl_super with
		| Some (sc, params) ->
			let path = cs_path_of_path sc.cl_path in
			let params = List.map (cs_type_of_type gctx) params in
			Some (CsTypeClass (path, params))
		| None ->
			(* All Haxe classes extend HaxeObject for _hx_getField support *)
			Some (CsTypeClass ((["haxe"; "root"], "HaxeObject"), []))
	in

	(* Generate interface references *)
	let interfaces = List.map (fun (i, params) ->
		let path = cs_path_of_path i.cl_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	) c.cl_implements in

	(* Generate members *)
	let members = ref [] in

	(* Constructor - returns a list of members (constructor + _hx_ctor method) *)
	begin match c.cl_constructor with
	| Some cf -> members := generate_constructor gctx c cf @ !members
	| None -> ()
	end;

	(* Fields *)
	List.iter (fun cf ->
		match generate_field gctx c cf false with
		| Some m ->
			members := m :: !members;
			(* Generate explicit interface implementations for covariant return types *)
			members := generate_explicit_interface_impls gctx c cf @ !members
		| None -> ()
	) c.cl_ordered_fields;

	(* Static fields *)
	List.iter (fun cf ->
		match generate_field gctx c cf true with
		| Some m -> members := m :: !members
		| None -> ()
	) c.cl_ordered_statics;

	(* Generate _hx_getField, _hx_setField, _hx_getFields for AOT-compatible dynamic field access *)
	members := generate_field_accessors gctx c @ !members;

	(* Extract type parameter names *)
	let type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in

	CsClassDef {
		c_path = path;
		c_access = AccessModifier.Public;
		c_modifiers = modifiers;
		c_type_params = type_params;
		c_base = base_class;
		c_interfaces = interfaces;
		c_constraints = [];
		c_members = List.rev !members;
	}

(* Generate C# interface from Haxe interface *)
let generate_interface gctx c =
	let path = cs_path_of_path c.cl_path in

	(* Generate base interfaces *)
	let base_interfaces = List.map (fun (i, params) ->
		let path = cs_path_of_path i.cl_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	) c.cl_implements in

	(* Generate members *)
	let members = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Method MethNormal ->
			let args, ret = match follow cf.cf_type with
				| TFun (args, ret) -> args, ret
				| _ -> [], cf.cf_type
			in
			let params = List.map (fun (n, _, t) ->
				{
					p_name = escape_identifier n;
					p_type = Some (cs_type_of_type gctx t);
					p_default = None;
					p_modifier = None;
				}
			) args in
			(* Get method-level type parameters *)
			let method_type_params = List.map (fun ttp -> ttp.ttp_name) cf.cf_params in
			Some (CsMemberMethod {
				m_name = escape_identifier cf.cf_name;
				m_return_type = cs_type_of_type gctx ret;
				m_access = AccessModifier.Public;
				m_modifiers = [];
				m_type_params = method_type_params;
				m_params = params;
				m_body = None;  (* Interface methods have no body *)
				m_constraints = [];
				m_explicit_interface = None;
			})
		| _ -> None
	) c.cl_ordered_fields in

	(* Extract type parameter names *)
	let type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in

	CsInterfaceDef {
		i_path = path;
		i_access = AccessModifier.Public;
		i_modifiers = [];
		i_type_params = type_params;
		i_base = base_interfaces;
		i_constraints = [];
		i_members = members;
	}

(* Generate C# enum from Haxe enum *)
let generate_enum gctx (e : tenum) =
	let path = cs_path_of_path e.e_path in

	(* Extract type parameter names from the enum *)
	let type_params = List.map (fun ttp -> ttp.ttp_name) e.e_params in
	(* Create C# type references for the type params (used when inheriting from parent) *)
	let type_param_refs = List.map (fun name -> CsTypeGenericParam name) type_params in

	(* Check if this is a simple enum (no constructors with parameters AND no type params) *)
	let is_simple = type_params = [] && PMap.fold (fun ef acc ->
		acc && (match ef.ef_type with TFun _ -> false | _ -> true)
	) e.e_constrs true in

	if is_simple then
		(* Generate as C# enum *)
		let members = PMap.fold (fun ef acc ->
			{ em_name = escape_identifier ef.ef_name; em_value = None } :: acc
		) e.e_constrs [] in
		CsEnumDef {
			e_path = path;
			e_access = AccessModifier.Public;
			e_underlying = None;
			e_members = List.rev members;
		}
	else
		(* Generate as abstract class with nested classes *)
		let members = PMap.fold (fun ef acc ->
			match ef.ef_type with
			| TFun (args, _) ->
				(* Nested class for constructor with parameters *)
				let class_name = escape_identifier ef.ef_name in
				(* GADT support: constructor may have its own type parameters (ef.ef_params) *)
				let ctor_type_params = List.map (fun ttp -> ttp.ttp_name) ef.ef_params in
				(* Filter out constructor params that shadow parent params (same name) *)
				let extra_type_params = List.filter (fun name ->
					not (List.mem name type_params)
				) ctor_type_params in
				(* Combined type params: parent enum params + constructor's unique params *)
				let nested_type_params = type_params @ extra_type_params in
				let fields = List.mapi (fun i (name, _, t) ->
					CsMemberField {
						f_name = escape_identifier name;
						f_type = cs_type_of_type gctx t;
						f_access = AccessModifier.Public;
						f_modifiers = [];
						f_value = None;
					}
				) args in
				let ctor_params = List.map (fun (name, _, t) ->
					{
						p_name = escape_identifier name;
						p_type = Some (cs_type_of_type gctx t);
						p_default = None;
						p_modifier = None;
					}
				) args in
				let ctor_body = List.map (fun (name, _, _) ->
					let esc_name = escape_identifier name in
					CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, esc_name),
						CsLocal esc_name))
				) args @ [
					CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, "_hx_index"),
						CsConst (CsConstInt (Int32.of_int ef.ef_index))))
				] in
				let ctor = CsMemberConstructor {
					ctor_access = AccessModifier.Public;
					ctor_modifiers = [];
					ctor_params = ctor_params;
					ctor_base_call = None;
					ctor_this_call = None;
					ctor_body = ctor_body;
				} in
				(* Nested class has parent's type params + constructor's own type params (GADT) *)
				let nested_class = CsClassDef {
					c_path = (fst path, class_name);
					c_access = AccessModifier.Public;
					c_modifiers = [];
					c_type_params = nested_type_params;  (* Parent + constructor's own type params *)
					c_base = Some (CsTypeClass (path, type_param_refs));  (* Reference parent with parent's type params only *)
					c_interfaces = [];
					c_constraints = [];
					c_members = fields @ [ctor];
				} in
				CsMemberNestedType nested_class :: acc
			| _ ->
				(* Simple constructor (no parameters) - need a nested class with singleton instance *)
				let class_name = escape_identifier ef.ef_name in
				(* For generic enums, simple constructors also need the type params *)
				if type_params <> [] then begin
					(* Generic enum - nested class inherits type params, no singleton possible *)
					let ctor_body = [
						CsExprStmt (CsBinop (CsOpAssign,
							CsField (CsThis, "_hx_index"),
							CsConst (CsConstInt (Int32.of_int ef.ef_index))))
					] in
					let ctor = CsMemberConstructor {
						ctor_access = AccessModifier.Public;
						ctor_modifiers = [];
						ctor_params = [];
						ctor_base_call = None;
						ctor_this_call = None;
						ctor_body = ctor_body;
					} in
					let nested_class = CsClassDef {
						c_path = (fst path, class_name);
						c_access = AccessModifier.Public;
						c_modifiers = [];
						c_type_params = type_params;  (* Inherit type params from parent *)
						c_base = Some (CsTypeClass (path, type_param_refs));  (* Reference parent with type params *)
						c_interfaces = [];
						c_constraints = [];
						c_members = [ctor];
					} in
					CsMemberNestedType nested_class :: acc
				end else begin
					(* Non-generic enum - use singleton pattern *)
					(* Suffix to avoid name collision between nested class and static field *)
					let nested_class_name = class_name ^ "_Impl_" in
					(* Constructor that sets _hx_index *)
					let ctor_body = [
						CsExprStmt (CsBinop (CsOpAssign,
							CsField (CsThis, "_hx_index"),
							CsConst (CsConstInt (Int32.of_int ef.ef_index))))
					] in
					let ctor = CsMemberConstructor {
						ctor_access = AccessModifier.Public;
						ctor_modifiers = [];
						ctor_params = [];
						ctor_base_call = None;
						ctor_this_call = None;
						ctor_body = ctor_body;
					} in
					(* Path for nested class: add parent class name to namespace *)
					let nested_path = (fst path @ [snd path], nested_class_name) in
					let instance = CsMemberField {
						f_name = "Instance";
						f_type = CsTypeClass (nested_path, []);
						f_access = AccessModifier.Public;
						f_modifiers = [MemberModifier.Static; MemberModifier.Readonly];
						f_value = Some (CsNew (CsTypeClass (nested_path, []), []));
					} in
					let nested_class = CsClassDef {
						c_path = (fst path, nested_class_name);  (* use short path for declaration *)
						c_access = AccessModifier.Public;
						c_modifiers = [];
						c_type_params = [];
						c_base = Some (CsTypeClass (path, []));
						c_interfaces = [];
						c_constraints = [];
						c_members = [ctor; instance];
					} in
					(* Static field on parent that returns the instance *)
					let field = CsMemberField {
						f_name = class_name;
						f_type = CsTypeClass (path, []);
						f_access = AccessModifier.Public;
						f_modifiers = [MemberModifier.Static; MemberModifier.Readonly];
						f_value = Some (CsStaticField (CsTypeClass (nested_path, []), "Instance"));
					} in
					CsMemberNestedType nested_class :: field :: acc
				end
		) e.e_constrs [] in

		(* Add _hx_index field *)
		let index_field = CsMemberField {
			f_name = "_hx_index";
			f_type = CsTypeInt;
			f_access = AccessModifier.Public;
			f_modifiers = [];
			f_value = None;
		} in

		CsClassDef {
			c_path = path;
			c_access = AccessModifier.Public;
			c_modifiers = [TypeModifier.Abstract];
			c_type_params = type_params;  (* Add type params to parent class *)
			c_base = None;
			c_interfaces = [];
			c_constraints = [];
			c_members = index_field :: List.rev members;
		}

(* Generate type *)
let generate_type gctx mt =
	match mt with
	| TClassDecl c when has_class_flag c CExtern ->
		(* Skip extern classes - they map to native types *)
		None
	| TClassDecl c when not (has_class_flag c CInterface) ->
		Some (generate_class gctx c)
	| TClassDecl c ->
		Some (generate_interface gctx c)
	| TEnumDecl e when has_enum_flag e EnExtern ->
		(* Skip extern enums *)
		None
	| TEnumDecl e ->
		Some (generate_enum gctx e)
	| TTypeDecl _ | TAbstractDecl _ ->
		None

(* Write file to disk *)
let write_file base_path rel_path content =
	let full_path = base_path ^ "/" ^ rel_path in
	Path.mkdir_from_path full_path;
	let ch = open_out_bin full_path in
	output_string ch content;
	close_out ch

(* Main generation entry point *)
let generate com =
	let gctx = create_context com in

	(* Generate all types *)
	List.iter (fun mt ->
		match generate_type gctx mt with
		| Some td -> gctx.generated_types <- td :: gctx.generated_types
		| None -> ()
	) com.types;

	(* Group types by namespace and write files *)
	let files = List.map (fun td ->
		let path = match td with
			| CsClassDef c -> c.c_path
			| CsStructDef s -> s.s_path
			| CsInterfaceDef i -> i.i_path
			| CsEnumDef e -> e.e_path
			| CsDelegateDef d -> d.d_path
		in
		let namespace = fst path in
		let name = snd path in
		let file = {
			file_usings = [
				CsUsingNamespace ["System"];
				CsUsingNamespace ["System"; "Collections"; "Generic"];
			];
			file_namespace = if namespace = [] then None else Some namespace;
			file_types = [td];
		} in
		let rel_path = String.concat "/" namespace ^ "/" ^ name ^ ".cs" in
		(rel_path, generate_file file)
	) (List.rev gctx.generated_types) in

	(* Write all files *)
	List.iter (fun (rel_path, content) ->
		write_file com.file rel_path content
	) files;

	(* Generate Program.cs with Main entry point if we have a main class *)
	begin match gctx.main_class with
	| Some main_path ->
		let main_class_path = cs_path_of_path main_path in
		let program_content = Printf.sprintf
"// Generated by Haxe C# target
using System;

public class Program
{
    public static void Main(string[] args)
    {
        %s.main();
    }
}
" (s_cs_path main_class_path) in
		write_file com.file "Program.cs" program_content
	| None -> ()
	end;

	(* Generate .csproj *)
	let proj = {
		proj_name = "HaxeProject";
		proj_target_framework = "net8.0";
		proj_output_type = "Exe";
	} in
	write_file com.file "Project.csproj" (generate_csproj proj);

	(* Copy runtime support files from std/cs/_cs/ *)
	let find_file f = (com.class_paths#find_file f).file in
	let copy_runtime_file src_path dest_path =
		let content = Std.input_file ~bin:true (find_file src_path) in
		write_file com.file dest_path content
	in
	copy_runtime_file "cs/_cs/haxe/root/HaxeObject.cs" "haxe/root/HaxeObject.cs";
	copy_runtime_file "cs/_cs/haxe/root/HaxeDynamicObject.cs" "haxe/root/HaxeDynamicObject.cs";
	copy_runtime_file "cs/_cs/haxe/lang/Null.cs" "haxe/lang/Null.cs";
	copy_runtime_file "cs/_cs/haxe/lang/Runtime.cs" "haxe/lang/Runtime.cs";
	copy_runtime_file "cs/_cs/haxe/lang/Function.cs" "haxe/lang/Function.cs";

