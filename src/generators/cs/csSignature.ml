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
	let haxe_function_path = (["haxe"; "lang"], "Function")
	let haxe_closure_path = (["haxe"; "root"], "HaxeClosure")
	let haxe_enum_path = (["haxe"; "root"], "HaxeEnum")
	let haxe_exception_path = (["haxe"], "Exception")
	let haxe_array_path = (["haxe"; "root"], "Array")
	let haxe_null_path = (["haxe"; "lang"], "Null")
	let haxe_runtime_path = (["haxe"; "lang"], "Runtime")
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

(* Check if a C# type is inherently nullable (reference type that can hold null).
   These types don't need the Null<T> struct wrapper because they can represent null natively.
   This is used to strip redundant Null<T> wrappers at the type level. *)
let is_inherently_nullable = function
	(* Reference types - can hold null *)
	| CsTypeObject | CsTypeDynamic | CsTypeString -> true
	| CsTypeArray _ -> true  (* Arrays are reference types *)
	(* Classes are reference types (except structs, but we don't distinguish here)
	   EXCEPT: haxe.lang.Null itself is a struct, don't strip Null<Null<T>> here
	   (that's handled separately by flattening). Also exclude value-like types. *)
	| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> false  (* Null<T> struct - not inherently nullable *)
	| CsTypeClass ((["System"], "DateTime"), _) -> false  (* struct *)
	| CsTypeClass ((["System"], "TimeSpan"), _) -> false  (* struct *)
	| CsTypeClass ((["System"], "Guid"), _) -> false  (* struct *)
	| CsTypeClass _ -> true  (* Other classes are reference types *)
	(* Primitives are NOT nullable - they need Null<T> wrapper *)
	| CsTypeBool | CsTypeByte | CsTypeSByte
	| CsTypeChar | CsTypeShort | CsTypeUShort
	| CsTypeInt | CsTypeUInt | CsTypeLong | CsTypeULong
	| CsTypeFloat | CsTypeDouble | CsTypeDecimal -> false
	(* Other types - assume not nullable to be safe *)
	| CsTypeVoid | CsTypeVar | CsTypeNullable _ | CsTypeNested _ | CsTypeNestedGeneric _
	| CsTypeFunc _ | CsTypeAction _ | CsTypeGenericParam _ -> false

(* Box a type to haxe.lang.Null<T> wrapper, unless it's already inherently nullable.
   Reference types (classes, arrays, strings) can represent null natively in C#
   and don't need the Null<T> struct wrapper. *)
let get_boxed_type csig =
	if is_inherently_nullable csig then
		csig  (* Already nullable, no wrapper needed *)
	else
		CsTypeClass ((["haxe"; "lang"], "Null"), [csig])

(* Unbox a nullable type to its underlying value type *)
let get_unboxed_type = function
	| CsTypeNullable t -> t
	| CsTypeClass ((["haxe"; "lang"], "Null"), [t]) -> t
	| t -> t

(* Convert Haxe path to C# path *)
let cs_path_of_path (pack, name) =
	let pack = match pack with
		| [] -> ["haxe"; "root"]  (* Default namespace for unpackaged types *)
		| _ -> pack
	in
	(List.map escape_identifier pack, escape_identifier name)

(* Main type conversion: Haxe type to C# type
   Uses a stack parameter to detect and break cycles, following JVM's approach *)
let rec cs_type_of_type_inner gctx stack t =
	let open Type in
	(* Check for recursive types - if we've already seen this type, return object to break cycle *)
	if List.exists (Type.fast_eq t) stack then CsTypeObject else
	(* Shadow cs_type_of_type_inner to include current type in stack *)
	let cs_type_of_type_inner = cs_type_of_type_inner gctx (t :: stack) in
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
	| TInst ({ cl_path = pack, name }, _) when (pack = [] || pack = ["haxe"; "root"]) && (name = "String" || name = "string") ->
		(* Due to @:native, the usual String path doesn't always match - also match lowercase "string" from @:native *)
		CsTypeString
	| TAbstract ({ a_path = ([], "Null") }, [t]) ->
		(* Null<T> handling with smart stripping for inherently nullable types.

		   DESIGN PRINCIPLE: If the underlying C# type can already represent null
		   (reference types like classes, arrays, strings), then the Null<T> struct
		   wrapper is redundant and should be stripped at the TYPE level.

		   This ensures consistent handling everywhere the type is used, rather than
		   special-casing at each usage site (assignments, comparisons, etc.).

		   Null<Null<T>> is also flattened to Null<T> - "nullable nullable int" = "nullable int". *)
		let inner = cs_type_of_type_inner t in
		begin match inner with
		| CsTypeVoid -> CsTypeObject  (* Null<Void> -> object (void not allowed as type arg) *)
		(* Flatten nested Null<Null<T>> to Null<T> *)
		| CsTypeClass ((["haxe"; "lang"], "Null"), [inner_inner]) ->
			CsTypeClass ((["haxe"; "lang"], "Null"), [inner_inner])
		(* Strip Null<T> when T is inherently nullable in C# (reference types) *)
		| _ when is_inherently_nullable inner -> inner
		(* Keep Null<T> for value types (int, double, bool, etc.) *)
		| _ -> CsTypeClass ((["haxe"; "lang"], "Null"), [inner])
		end
	| TDynamic _ ->
		(* Dynamic -> object (not dynamic, to avoid runtime dispatch overhead) *)
		CsTypeObject
	| TAnon _ ->
		(* Anonymous objects / structural types -> object
		   This allows any object to be passed where structural types are expected.
		   Field access on these types uses runtime dispatch via Reflect or casts. *)
		CsTypeObject
	| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, [t]) ->
		(* Array<T> stays as haxe.root.Array, not List<T> *)
		let inner = cs_type_of_type_inner t in
		CsTypeClass (NativeTypes.haxe_array_path, [inner])
	| TInst ({ cl_path = (["cs"], "NativeArray") }, [t]) ->
		(* cs.NativeArray<T> -> T[] *)
		let inner = cs_type_of_type_inner t in
		CsTypeArray (inner, None)
	| TInst ({ cl_path = (["haxe"; "lang"], "Null") }, [t]) ->
		(* haxe.lang.Null<T> class instance - same stripping/flattening rules as abstract Null *)
		let inner = cs_type_of_type_inner t in
		begin match inner with
		| CsTypeVoid -> CsTypeObject
		| CsTypeClass ((["haxe"; "lang"], "Null"), [inner_inner]) ->
			CsTypeClass ((["haxe"; "lang"], "Null"), [inner_inner])
		| _ when is_inherently_nullable inner -> inner
		| _ -> CsTypeClass ((["haxe"; "lang"], "Null"), [inner])
		end
	| TInst ({ cl_kind = KTypeParameter ttp }, _) ->
		(* Type parameter -> preserve as generic param for C# generics *)
		CsTypeGenericParam ttp.ttp_name
	| TInst (c, params) ->
		let path = cs_path_of_path c.cl_path in
		(* Convert type params, respecting constraints.
		   If a param maps to object but the class param has a constraint,
		   use the constraint bound instead (C# requires type args satisfy constraints).
		   Note: params and c.cl_params may have different lengths (e.g., when type is
		   erased or partially specified). Only apply constraint substitution when lengths match. *)
		let params =
			if List.length params = List.length c.cl_params then
				List.map2 (fun hx_type ttp ->
					let cs_type = cs_type_of_type_inner hx_type in
					match cs_type with
					| CsTypeObject ->
						let constraints = TFunctions.get_constraints ttp in
						begin match constraints with
						| first_constraint :: _ ->
							let constraint_cs = cs_type_of_type_inner first_constraint in
							if constraint_cs <> CsTypeObject then constraint_cs else cs_type
						| [] -> cs_type
						end
					| _ -> cs_type
				) params c.cl_params
			else
				(* Lengths don't match - just map the params without constraint substitution *)
				List.map cs_type_of_type_inner params
		in
		CsTypeClass (path, params)
	| TEnum (e, params) ->
		let path = cs_path_of_path e.e_path in
		let params = List.map cs_type_of_type_inner params in
		CsTypeClass (path, params)
	| TType (td, params) ->
		(* Check for well-known typedefs first *)
		begin match td.t_path with
		| ([], "Iterator") ->
			(* Iterator<T> is a structural typedef - any object with hasNext()/next() methods.
			   Map to object and use Reflect.field + Runtime.InvokeDelegate for method calls.
			   This works for all iterator implementations (ArrayIterator, MapIterator, custom, etc.).
			   NOTE: We use object instead of HaxeObject because the for-loop desugaring creates
			   variables with the same name for iterator and loop element, and mapping to a specific
			   class type causes assignment type conflicts in the generated C# code. *)
			CsTypeObject
		| ([], "KeyValueIterator") ->
			(* KeyValueIterator<K,V> is a structural typedef. Unlike Iterator, we can't
			   map it to a specific class because implementations vary:
			   - Map.keyValueIterator() returns MapKeyValueIterator
			   - Array.iterator() on array of {key:K, value:V} returns ArrayIterator
			   So we follow to the underlying anonymous type, which becomes object in C#. *)
			cs_type_of_type_inner (Type.apply_typedef td params)
		| _ ->
			(* Other typedefs: follow to underlying type *)
			cs_type_of_type_inner (Type.apply_typedef td params)
		end
	| TFun (args, ret) ->
		(* Function types map to haxe.lang.Function class.
		   Following JVM's approach: all function types use the base Function class,
		   not C#'s Func<>/Action<> delegates.
		   This simplifies code generation and avoids delegate conversion issues.

		   For optional parameters in TFun:
		   - Explicit type annotations like (Int, ?Int, Int)->Int have raw type + opt flag
		   - Inferred types from lambdas already have Null<T> in the type itself
		   We need to wrap in Null<T> only if opt=true AND type isn't already Null<T>
		   This information is preserved in closure generation but not in the type itself.

		   All function types become haxe.lang.Function - the actual signatures are
		   preserved in the generated closure classes' invoke methods. *)
		ignore args; ignore ret;
		CsTypeClass (NativeTypes.haxe_function_path, [])
	| TAbstract (a, params) when Meta.has Meta.CoreType a.a_meta ->
		(* Core type abstract - handle specially *)
		begin match a.a_path with
		| ([], "Int64") | (["cs"], "Int64") -> CsTypeLong
		| ([], "UInt") -> CsTypeUInt
		| ([], "UInt64") | (["cs"], "UInt64") -> CsTypeULong
		| ([], "Int8") | (["cs"], "Int8") -> CsTypeSByte
		| ([], "UInt8") | (["cs"], "UInt8") -> CsTypeByte
		| ([], "Int16") | (["cs"], "Int16") -> CsTypeShort
		| ([], "UInt16") | (["cs"], "UInt16") -> CsTypeUShort
		| ([], "Dynamic") ->
			(* Dynamic -> object in C# *)
			CsTypeObject
		| ([], "Class") ->
			(* Class<T> -> System.Type in C# *)
			CsTypeClass ((["System"], "Type"), [])
		| ([], "Enum") ->
			(* Enum<T> -> System.Type in C# (enums are also represented as Type) *)
			CsTypeClass ((["System"], "Type"), [])
		| ([], "EnumValue") ->
			(* EnumValue -> object in C# (any enum instance) *)
			CsTypeObject
		| _ ->
			let path = cs_path_of_path a.a_path in
			let params = List.map cs_type_of_type_inner params in
			CsTypeClass (path, params)
		end
	| TAbstract (a, params) ->
		(* Non-core abstract - follow to underlying type using follow_with_abstracts_without_null
		   to preserve Null<T> wrappers. This ensures that abstract types wrapping Null<Int> etc.
		   keep the nullable semantics in C#. *)
		let t_followed = Abstract.follow_with_abstracts_without_null (Type.TAbstract (a, params)) in
		(* If follow didn't resolve (e.g., recursive abstract), use object *)
		begin match t_followed with
		| Type.TAbstract (a2, _) when a2 == a ->
			(* Recursive abstract - map to object to break cycle *)
			CsTypeObject
		| _ ->
			cs_type_of_type_inner t_followed
		end
	| TLazy f ->
		cs_type_of_type_inner (lazy_type f)
	| TMono r ->
		begin match r.tm_type with
		| None -> CsTypeObject  (* Unresolved monomorph -> object *)
		| Some t -> cs_type_of_type_inner t
		end

(* Public entry point with empty stack *)
let cs_type_of_type gctx t = cs_type_of_type_inner gctx [] t

(* Entry point that doesn't require gctx - for use in places where we just need
   to check what C# type a Haxe type maps to (e.g., is_null_wrapper_type).
   gctx is not actually used in the type conversion logic. *)
let cs_type_of_type_without_gctx t = cs_type_of_type_inner () [] t

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
		(* Use global:: prefix to avoid namespace conflicts.
		   This ensures haxe.root.HaxeObject is always the global namespace path,
		   not relative to the current namespace (e.g., unit.spec.haxe.root) *)
		"global::" ^ String.concat "." pack ^ "." ^ name
	| CsTypeClass (([], name), params) ->
		(* No package, just type with params *)
		name ^ "<" ^ String.concat ", " (List.map s_cs_type params) ^ ">"
	| CsTypeClass ((pack, name), params) ->
		(* Package with params - use global:: *)
		"global::" ^ String.concat "." pack ^ "." ^ name ^ "<" ^ String.concat ", " (List.map s_cs_type params) ^ ">"
	| CsTypeNested (parent, nested_name) ->
		(* Nested type: ParentType<T>.NestedClass *)
		s_cs_type parent ^ "." ^ nested_name
	| CsTypeNestedGeneric (parent, nested_name, params) ->
		(* Nested generic type: ParentType<T>.NestedClass<C> *)
		s_cs_type parent ^ "." ^ nested_name ^ "<" ^ String.concat ", " (List.map s_cs_type params) ^ ">"
	| CsTypeGenericParam name -> name
	| CsTypeFunc (args, ret) ->
		(* In C#, void cannot be used as a type argument, so Func<..., void> is invalid.
		   Instead, use Action<...> for void-returning delegates. *)
		begin match ret with
		| CsTypeVoid ->
			begin match args with
			| [] -> "Action"
			| _ -> "Action<" ^ String.concat ", " (List.map s_cs_type args) ^ ">"
			end
		| _ ->
			begin match args with
			| [] -> "Func<" ^ s_cs_type ret ^ ">"
			| _ -> "Func<" ^ String.concat ", " (List.map s_cs_type args @ [s_cs_type ret]) ^ ">"
			end
		end
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

(* Extract all generic type parameter names from a C# type.
   This is used to detect type parameters used in method signatures
   that need to become method-level type params in C# _Impl_ classes. *)
let rec collect_type_params acc cstype =
	match cstype with
	| CsTypeGenericParam name ->
		if List.mem name acc then acc else name :: acc
	| CsTypeNullable t | CsTypeArray (t, _) ->
		collect_type_params acc t
	| CsTypeClass (_, params) ->
		List.fold_left collect_type_params acc params
	| CsTypeFunc (args, ret) ->
		let acc = List.fold_left collect_type_params acc args in
		collect_type_params acc ret
	| CsTypeAction args ->
		List.fold_left collect_type_params acc args
	| _ ->
		acc

(* Get all type parameters used in a method signature *)
let get_method_type_params param_types ret_type =
	let acc = List.fold_left collect_type_params [] param_types in
	let acc = collect_type_params acc ret_type in
	(* Reverse to maintain order of first appearance *)
	List.rev acc

(* Erase type parameters to object.
   Used in typeof() expressions where type parameters are not in scope.
   C# doesn't allow typeof(SomeGeneric<T>) unless T is defined in the current context.
   We replace T with object: typeof(SomeGeneric<object>) *)
let rec erase_type_params cstype =
	match cstype with
	| CsTypeGenericParam _ ->
		(* Type parameter -> object *)
		CsTypeObject
	| CsTypeNullable t ->
		CsTypeNullable (erase_type_params t)
	| CsTypeArray (t, rank) ->
		CsTypeArray (erase_type_params t, rank)
	| CsTypeClass (path, params) ->
		CsTypeClass (path, List.map erase_type_params params)
	| CsTypeFunc (args, ret) ->
		CsTypeFunc (List.map erase_type_params args, erase_type_params ret)
	| CsTypeAction args ->
		CsTypeAction (List.map erase_type_params args)
	| CsTypeNested (parent, name) ->
		CsTypeNested (erase_type_params parent, name)
	| _ ->
		(* Primitive types, object, string, etc. - no change *)
		cstype

(* Erase type parameters that are NOT in scope.
   If a type parameter's name is in the in_scope list, it is kept.
   Otherwise, it is replaced with object.
   This is used when generating casts to avoid using type parameters
   that are not defined in the current method/class context. *)
let rec erase_out_of_scope_type_params in_scope cstype =
	match cstype with
	| CsTypeGenericParam name ->
		(* Keep if in scope, otherwise erase to object *)
		if List.mem name in_scope then cstype else CsTypeObject
	| CsTypeNullable t ->
		CsTypeNullable (erase_out_of_scope_type_params in_scope t)
	| CsTypeArray (t, rank) ->
		CsTypeArray (erase_out_of_scope_type_params in_scope t, rank)
	| CsTypeClass (path, params) ->
		CsTypeClass (path, List.map (erase_out_of_scope_type_params in_scope) params)
	| CsTypeFunc (args, ret) ->
		CsTypeFunc (List.map (erase_out_of_scope_type_params in_scope) args, erase_out_of_scope_type_params in_scope ret)
	| CsTypeAction args ->
		CsTypeAction (List.map (erase_out_of_scope_type_params in_scope) args)
	| CsTypeNested (parent, name) ->
		CsTypeNested (erase_out_of_scope_type_params in_scope parent, name)
	| CsTypeNestedGeneric (parent, name, params) ->
		CsTypeNestedGeneric (erase_out_of_scope_type_params in_scope parent, name, List.map (erase_out_of_scope_type_params in_scope) params)
	| _ ->
		(* Primitive types, object, string, etc. - no change *)
		cstype

(* Substitute type parameters using a name -> type mapping.
   This is used to apply method type arguments to parameter types
   when Haxe's apply_params doesn't work due to physical identity issues
   with type param instances (common with abstract impl methods). *)
let rec substitute_type_params subst cstype =
	match cstype with
	| CsTypeGenericParam name ->
		(* Look up substitution by name *)
		begin try List.assoc name subst with Not_found -> cstype end
	| CsTypeNullable t ->
		CsTypeNullable (substitute_type_params subst t)
	| CsTypeArray (t, rank) ->
		CsTypeArray (substitute_type_params subst t, rank)
	| CsTypeClass (path, params) ->
		CsTypeClass (path, List.map (substitute_type_params subst) params)
	| CsTypeFunc (args, ret) ->
		CsTypeFunc (List.map (substitute_type_params subst) args, substitute_type_params subst ret)
	| CsTypeAction args ->
		CsTypeAction (List.map (substitute_type_params subst) args)
	| CsTypeNested (parent, name) ->
		CsTypeNested (substitute_type_params subst parent, name)
	| CsTypeNestedGeneric (parent, name, params) ->
		CsTypeNestedGeneric (substitute_type_params subst parent, name, List.map (substitute_type_params subst) params)
	| _ ->
		(* Primitive types, object, string, etc. - no change *)
		cstype

(* Erase out-of-scope type parameters in an expression.
   Recursively processes the expression tree and erases type params
   in any types embedded in the expression (casts, new, generic calls, etc.). *)
let rec erase_out_of_scope_type_params_in_expr in_scope expr =
	let erase_type = erase_out_of_scope_type_params in_scope in
	let erase_expr = erase_out_of_scope_type_params_in_expr in_scope in
	let erase_lambda_body = function
		| CsLambdaExpr e -> CsLambdaExpr (erase_expr e)
		| CsLambdaBlock stmts -> CsLambdaBlock stmts  (* Don't recurse into statement blocks *)
	in
	match expr with
	| CsCast (t, e) -> CsCast (erase_type t, erase_expr e)
	| CsDefault t -> CsDefault (erase_type t)
	| CsTypeOf t -> CsTypeOf (erase_type t)
	| CsSizeOf t -> CsSizeOf (erase_type t)
	| CsNew (t, args) -> CsNew (erase_type t, List.map erase_expr args)
	| CsNewArray (t, args) -> CsNewArray (erase_type t, List.map erase_expr args)
	| CsNewArraySize (t, e) -> CsNewArraySize (erase_type t, erase_expr e)
	| CsCallGeneric (e, targs, args) -> CsCallGeneric (erase_expr e, List.map erase_type targs, List.map erase_expr args)
	| CsStaticCallGeneric (t, name, targs, args) -> CsStaticCallGeneric (erase_type t, name, List.map erase_type targs, List.map erase_expr args)
	| CsStaticField (t, name) -> CsStaticField (erase_type t, name)
	| CsStaticCall (t, name, args) -> CsStaticCall (erase_type t, name, List.map erase_expr args)
	| CsBinop (op, e1, e2) -> CsBinop (op, erase_expr e1, erase_expr e2)
	| CsUnop (op, post, e) -> CsUnop (op, post, erase_expr e)
	| CsTernary (cond, then_e, else_e) -> CsTernary (erase_expr cond, erase_expr then_e, erase_expr else_e)
	| CsField (e, name) -> CsField (erase_expr e, name)
	| CsArrayAccess (e1, e2) -> CsArrayAccess (erase_expr e1, erase_expr e2)
	| CsCall (e, args) -> CsCall (erase_expr e, List.map erase_expr args)
	| CsParens e -> CsParens (erase_expr e)
	| CsAs (e, t) -> CsAs (erase_expr e, erase_type t)
	| CsIs (e, t) -> CsIs (erase_expr e, erase_type t)
	| CsIsPattern (e, t, name) -> CsIsPattern (erase_expr e, erase_type t, name)
	| CsUnchecked e -> CsUnchecked (erase_expr e)
	| CsAwait e -> CsAwait (erase_expr e)
	| CsThrow e -> CsThrow (erase_expr e)
	| CsLambda (params, body) -> CsLambda (params, erase_lambda_body body)
	| CsNullConditionalField (e, name) -> CsNullConditionalField (erase_expr e, name)
	| CsNullConditionalCall (e, args) -> CsNullConditionalCall (erase_expr e, List.map erase_expr args)
	| CsNullConditionalIndex (e1, e2) -> CsNullConditionalIndex (erase_expr e1, erase_expr e2)
	| CsInterpolatedString parts ->
		CsInterpolatedString (List.map (function
			| CsInterpLiteral s -> CsInterpLiteral s
			| CsInterpExpr (e, fmt) -> CsInterpExpr (erase_expr e, fmt)
		) parts)
	| CsInlineCode (template, args) -> CsInlineCode (template, List.map erase_expr args)
	(* Simple expressions that don't contain types or sub-expressions *)
	| CsConst _ | CsLocal _ | CsThis | CsBase | CsNull | CsNameOf _ | CsRaw _ -> expr
