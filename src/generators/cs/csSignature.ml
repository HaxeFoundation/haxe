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

(* Haxe type to C# type conversion *)

open CsAst
open CsGlobals

(* Common C# type paths *)
module NativeTypes = struct
	let object_path = (["System"], "Object")
	let string_path = (["System"], "String")
	let void_path = ([], "void")

	let boolean_path = (["System"], "Boolean")
	let byte_path = (["System"], "Byte")
	let sbyte_path = (["System"], "SByte")
	let char_path = (["System"], "Char")
	let int16_path = (["System"], "Int16")
	let uint16_path = (["System"], "UInt16")
	let int32_path = (["System"], "Int32")
	let uint32_path = (["System"], "UInt32")
	let int64_path = (["System"], "Int64")
	let uint64_path = (["System"], "UInt64")
	let single_path = (["System"], "Single")
	let double_path = (["System"], "Double")
	let decimal_path = (["System"], "Decimal")

	let nullable_path = (["System"], "Nullable")

	let array_path = (["System"], "Array")
	let list_path = (["System"; "Collections"; "Generic"], "List")
	let dictionary_path = (["System"; "Collections"; "Generic"], "Dictionary")
	let hashset_path = (["System"; "Collections"; "Generic"], "HashSet")

	let action_path = (["System"], "Action")
	let func_path = (["System"], "Func")

	let type_path = (["System"], "Type")
	let exception_path = (["System"], "Exception")
	let method_info_path = (["System"; "Reflection"], "MethodInfo")
	let field_info_path = (["System"; "Reflection"], "FieldInfo")

	(* Haxe runtime types *)
	let haxe_object_path = (["haxe"; "root"], "HaxeObject")
	let haxe_dynamic_object_path = (["haxe"; "root"], "HaxeDynamicObject")
	let haxe_function_path = (["haxe"; "root"], "HaxeFunction")
	let haxe_closure_path = (["haxe"; "root"], "HaxeClosure")
	let haxe_enum_path = (["haxe"; "root"], "HaxeEnum")
	let haxe_exception_path = (["haxe"], "Exception")
end

(* Check if a C# type is a primitive/value type *)
let is_value_type = function
	| CsTypeBool | CsTypeByte | CsTypeSByte
	| CsTypeChar | CsTypeShort | CsTypeUShort
	| CsTypeInt | CsTypeUInt | CsTypeLong | CsTypeULong
	| CsTypeFloat | CsTypeDouble | CsTypeDecimal ->
		true
	| CsTypeClass((["System"], "DateTime"), _)
	| CsTypeClass((["System"], "TimeSpan"), _)
	| CsTypeClass((["System"], "Guid"), _) ->
		true
	| _ ->
		false

(* Check if a type is dynamic at runtime (like JVM's is_dynamic_at_runtime) *)
let is_dynamic_at_runtime = function
	| CsTypeObject
	| CsTypeDynamic
	| CsTypeGenericParam _ ->
		true
	| _ ->
		false

(* Box a value type to its nullable wrapper *)
let get_boxed_type csig =
	if is_value_type csig then
		CsTypeNullable csig
	else
		csig

(* Unbox a nullable type to its underlying value type *)
let get_unboxed_type = function
	| CsTypeNullable t -> t
	| t -> t

(* Convert Haxe path to C# path *)
let cs_path_of_path (pack, name) =
	let pack = match pack with
		| [] -> ["haxe"; "root"]  (* Default namespace for unpackaged types *)
		| _ -> pack
	in
	(List.map escape_identifier pack, escape_identifier name)

(* Main type conversion: Haxe type to C# type *)
let rec cs_type_of_type gctx t =
	let open Type in
	match t with
	| TAbstract ({ a_path = ([], "Void") }, _) ->
		CsTypeVoid
	| TAbstract ({ a_path = ([], "Bool") }, _) ->
		CsTypeBool
	| TAbstract ({ a_path = ([], "Int") }, _) ->
		CsTypeInt
	| TAbstract ({ a_path = ([], "Float") }, _) ->
		CsTypeDouble
	| TAbstract ({ a_path = ([], "Single") }, _) ->
		CsTypeFloat
	| TInst ({ cl_path = ([], "String") }, _) ->
		CsTypeString
	| TAbstract ({ a_path = ([], "Null") }, [t]) ->
		(* Null<T> -> T? for value types, T for reference types *)
		let inner = cs_type_of_type gctx t in
		if is_value_type inner then
			CsTypeNullable inner
		else
			inner
	| TDynamic _ ->
		CsTypeDynamic
	| TAnon _ ->
		(* Anonymous objects -> dynamic or HaxeDynamicObject *)
		CsTypeClass (NativeTypes.haxe_dynamic_object_path, [])
	| TInst ({ cl_path = ([], "Array") }, [t]) ->
		let inner = cs_type_of_type gctx t in
		CsTypeClass (NativeTypes.list_path, [inner])
	| TInst ({ cl_kind = KTypeParameter _ }, _) ->
		(* Type parameter -> object at runtime *)
		CsTypeObject
	| TInst (c, params) ->
		let path = cs_path_of_path c.cl_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	| TEnum (e, params) ->
		let path = cs_path_of_path e.e_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	| TType (td, params) ->
		(* Typedef - follow it *)
		cs_type_of_type gctx (Type.apply_typedef td params)
	| TFun (args, ret) ->
		let arg_types = List.map (fun (_, _, t) -> cs_type_of_type gctx t) args in
		let ret_type = cs_type_of_type gctx ret in
		begin match ret_type with
		| CsTypeVoid ->
			if List.length arg_types = 0 then
				CsTypeClass (NativeTypes.action_path, [])
			else
				CsTypeAction arg_types
		| _ ->
			CsTypeFunc (arg_types, ret_type)
		end
	| TAbstract (a, params) when Meta.has Meta.CoreType a.a_meta ->
		(* Core type abstract - handle specially *)
		begin match a.a_path with
		| ([], "Int64") -> CsTypeLong
		| ([], "UInt") -> CsTypeUInt
		| ([], "UInt64") -> CsTypeULong
		| ([], "Int8") -> CsTypeSByte
		| ([], "UInt8") -> CsTypeByte
		| ([], "Int16") -> CsTypeShort
		| ([], "UInt16") -> CsTypeUShort
		| _ ->
			let path = cs_path_of_path a.a_path in
			let params = List.map (cs_type_of_type gctx) params in
			CsTypeClass (path, params)
		end
	| TAbstract (a, params) ->
		(* Non-core abstract - follow underlying type *)
		cs_type_of_type gctx (Abstract.get_underlying_type a params)
	| TLazy f ->
		cs_type_of_type gctx (lazy_type f)
	| TMono r ->
		begin match r.tm_type with
		| None -> CsTypeDynamic
		| Some t -> cs_type_of_type gctx t
		end

(* Convert function signature *)
let cs_method_sig gctx args ret =
	let arg_types = List.map (fun (name, opt, t) ->
		let csig = cs_type_of_type gctx t in
		(* Optional parameters get boxed to allow null *)
		let csig = if opt then get_boxed_type csig else csig in
		(name, csig, opt)
	) args in
	let ret_type = cs_type_of_type gctx ret in
	(arg_types, ret_type)

(* Type to string for generated C# code *)
let rec s_cs_type = function
	| CsTypeVoid -> "void"
	| CsTypeBool -> "bool"
	| CsTypeByte -> "byte"
	| CsTypeSByte -> "sbyte"
	| CsTypeChar -> "char"
	| CsTypeShort -> "short"
	| CsTypeUShort -> "ushort"
	| CsTypeInt -> "int"
	| CsTypeUInt -> "uint"
	| CsTypeLong -> "long"
	| CsTypeULong -> "ulong"
	| CsTypeFloat -> "float"
	| CsTypeDouble -> "double"
	| CsTypeDecimal -> "decimal"
	| CsTypeString -> "string"
	| CsTypeObject -> "object"
	| CsTypeDynamic -> "dynamic"
	| CsTypeNullable t -> s_cs_type t ^ "?"
	| CsTypeArray (t, None) -> s_cs_type t ^ "[]"
	| CsTypeArray (t, Some rank) ->
		s_cs_type t ^ "[" ^ String.make (rank - 1) ',' ^ "]"
	| CsTypeClass (([], name), []) -> name
	| CsTypeClass ((pack, name), []) ->
		String.concat "." pack ^ "." ^ name
	| CsTypeClass (path, params) ->
		s_cs_path path ^ "<" ^ String.concat ", " (List.map s_cs_type params) ^ ">"
	| CsTypeGenericParam name -> name
	| CsTypeFunc (args, ret) ->
		"Func<" ^ String.concat ", " (List.map s_cs_type args @ [s_cs_type ret]) ^ ">"
	| CsTypeAction [] ->
		"Action"
	| CsTypeAction args ->
		"Action<" ^ String.concat ", " (List.map s_cs_type args) ^ ">"
	| CsTypeVar -> "var"

(* Comparison for types *)
let rec cs_type_equals t1 t2 = match t1, t2 with
	| CsTypeVoid, CsTypeVoid
	| CsTypeBool, CsTypeBool
	| CsTypeByte, CsTypeByte
	| CsTypeSByte, CsTypeSByte
	| CsTypeChar, CsTypeChar
	| CsTypeShort, CsTypeShort
	| CsTypeUShort, CsTypeUShort
	| CsTypeInt, CsTypeInt
	| CsTypeUInt, CsTypeUInt
	| CsTypeLong, CsTypeLong
	| CsTypeULong, CsTypeULong
	| CsTypeFloat, CsTypeFloat
	| CsTypeDouble, CsTypeDouble
	| CsTypeDecimal, CsTypeDecimal
	| CsTypeString, CsTypeString
	| CsTypeObject, CsTypeObject
	| CsTypeDynamic, CsTypeDynamic
	| CsTypeVar, CsTypeVar ->
		true
	| CsTypeNullable a, CsTypeNullable b ->
		cs_type_equals a b
	| CsTypeArray (a, r1), CsTypeArray (b, r2) ->
		cs_type_equals a b && r1 = r2
	| CsTypeClass (p1, args1), CsTypeClass (p2, args2) ->
		p1 = p2 && List.length args1 = List.length args2 &&
		List.for_all2 cs_type_equals args1 args2
	| CsTypeGenericParam n1, CsTypeGenericParam n2 ->
		n1 = n2
	| CsTypeFunc (a1, r1), CsTypeFunc (a2, r2) ->
		cs_type_equals r1 r2 &&
		List.length a1 = List.length a2 &&
		List.for_all2 cs_type_equals a1 a2
	| CsTypeAction a1, CsTypeAction a2 ->
		List.length a1 = List.length a2 &&
		List.for_all2 cs_type_equals a1 a2
	| _ ->
		false
