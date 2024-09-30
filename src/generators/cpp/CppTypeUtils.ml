(* Various helper functions to run checks on haxe classes and various other ast types *)
(* functions in here operate on standard haxe ast types, not gencpp ast types *)

open Extlib_leftovers
open Ast
open Type
open Error
open Common
open Globals

let follow = Abstract.follow_with_abstracts

let is_native_gen_class class_def =
   Meta.has Meta.NativeGen class_def.cl_meta ||
   match class_def.cl_kind with
   | KAbstractImpl abstract_def -> Meta.has Meta.NativeGen abstract_def.a_meta
   | _ -> false
 
let is_native_gen_module = function
   | TClassDecl class_def -> is_native_gen_class class_def
   | _ -> false
 
let is_extern_class class_def =
   has_class_flag class_def CExtern ||
   Meta.has Meta.Extern class_def.cl_meta ||
   match class_def.cl_kind with
   | KAbstractImpl abstract_def -> Meta.has Meta.Extern abstract_def.a_meta
   | _ -> false
 
let is_extern_enum enum_def =
   has_enum_flag enum_def EnExtern || Meta.has Meta.Extern enum_def.e_meta

(* The internal classes are implemented by the core hxcpp system, so the cpp classes should not be generated *)
let is_internal_class = function
   | [], "Int"
   | [], "Void"
   | [], "String"
   | [], "Null"
   | [], "Float"
   | [], "Array"
   | [], "Class"
   | [], "Enum"
   | [], "Bool"
   | [], "Dynamic"
   | [], "ArrayAccess"
   | [], "Math"
   | [], "Single"
   | [ "cpp" ], "FastIterator"
   | [ "cpp" ], "Pointer"
   | [ "cpp" ], "ConstPointer"
   | [ "cpp" ], "RawPointer"
   | [ "cpp" ], "RawConstPointer"
   | [ "cpp" ], "Function"
   | [ "cpp" ], "VirtualArray"
   | [ "cpp" ], "Int8"
   | [ "cpp" ], "UInt8"
   | [ "cpp" ], "Char"
   | [ "cpp" ], "Int16"
   | [ "cpp" ], "UInt16"
   | [ "cpp" ], "Int32"
   | [ "cpp" ], "UInt32"
   | [ "cpp" ], "Int64"
   | [ "cpp" ], "UInt64"
   | [ "cpp" ], "Float32"
   | [ "cpp" ], "Float64" ->
      true
   | _ ->
      false

let is_native_class class_def =
   (is_extern_class class_def || is_native_gen_class class_def) && not (is_internal_class class_def.cl_path)

let is_interface_type t =
   match follow t with
   | TInst (klass,params) -> (has_class_flag klass CInterface)
   | _ -> false

let rec is_objc_type t =
   match t with
   | TInst(cl,_) -> (has_class_flag cl CExtern) && Meta.has Meta.Objc cl.cl_meta
   | TType(td,_) -> (Meta.has Meta.Objc td.t_meta)
   | TAbstract (a,_) -> (Meta.has Meta.Objc a.a_meta)
   | TMono r -> (match r.tm_type with | Some t -> is_objc_type t | _ -> false)
   | TLazy f -> is_objc_type (lazy_type f)
   | _ -> false

let is_objc_type t =
   match t with
   | TInst(cl,_) -> (has_class_flag cl CExtern) && Meta.has Meta.Objc cl.cl_meta
   | TType(td,_) -> (Meta.has Meta.Objc td.t_meta)
   | TAbstract (a,_) -> (Meta.has Meta.Objc a.a_meta)
   | TMono r -> (match r.tm_type with | Some t -> is_objc_type t | _ -> false)
   | TLazy f -> is_objc_type (lazy_type f)
   | _ -> false

let is_dynamic_type_param class_kind =
   match class_kind with
   | KTypeParameter _ -> true
   | _ -> false

let is_dynamic_array_param t =
   match follow t with
   | TAbstract ({ a_path = ([],"Dynamic") },[]) -> true
   | TInst (klass, params) ->
      (match klass with
      | { cl_path = ([], "Array") }
      | { cl_path = ([], "Class") }
      | { cl_path = (["cpp"], "FastIterator") }
      | { cl_path = (["cpp"], "RawPointer") }
      | { cl_path = (["cpp"], "ConstRawPointer") }
      | { cl_path = (["cpp"], "Pointer") }
      | { cl_path = (["cpp"], "ConstPointer") }
      | { cl_path = (["cpp"], "Function") } -> false
      | { cl_kind = KTypeParameter _ } -> true
      | _ -> false)
   | _ -> false

let is_numeric t =
   match follow t with
   | TAbstract({ a_path = ([], "Int") }, [])
   | TAbstract({ a_path = ([], "Float") }, [])
   | TAbstract({ a_path = ([], "Single") }, [])
   | TAbstract({ a_path = (["cpp"], "Char") }, [])
   | TAbstract({ a_path = (["cpp"], "Float32") }, [])
   | TAbstract({ a_path = (["cpp"], "Float64") }, [])
   | TAbstract({ a_path = (["cpp"], "Int8") }, [])
   | TAbstract({ a_path = (["cpp"], "Int16") }, [])
   | TAbstract({ a_path = (["cpp"], "Int32") }, [])
   | TAbstract({ a_path = (["cpp"], "Int64") }, [])
   | TAbstract({ a_path = (["cpp"], "UInt8") }, [])
   | TAbstract({ a_path = (["cpp"], "UInt16") }, [])
   | TAbstract({ a_path = (["cpp"], "UInt32") }, [])
   | TAbstract({ a_path = (["cpp"], "UInt64") }, [])
      -> true
   | _
      -> false
   
let is_cpp_function_instance t =
   match follow t with
   | TInst ({ cl_path = (["cpp"], "Function") }, _) -> true
   | _ -> false

let is_objc_class klass =
   has_class_flag klass CExtern && Meta.has Meta.Objc klass.cl_meta

let is_var_field field =
   match field.cf_kind with
   | Var _
   | Method MethDynamic -> true
   | _ -> false

let is_pointer haxe_type includeRaw =
   match follow haxe_type with
   | TInst (klass,params) ->
      (match klass.cl_path with
      | ["cpp"] , "Pointer"
      | ["cpp"] , "ConstPointer"
      | ["cpp"] , "Function" -> true
      | ["cpp"] , "RawPointer" when includeRaw -> true
      | ["cpp"] , "RawConstPointer" when includeRaw -> true
      | _ -> false )
   | TType (type_def,params) ->
      (match type_def.t_path with
      | ["cpp"] , "Pointer"
      | ["cpp"] , "ConstPointer"
      | ["cpp"] , "Function" -> true
      | ["cpp"] , "RawPointer" when includeRaw -> true
      | ["cpp"] , "RawConstPointer" when includeRaw -> true
      | _ -> false )
   | _ -> false
   ;;

let is_array haxe_type =
   match follow haxe_type with
   | TInst ({ cl_path = ([], "Array") }, params)
   | TType ({ t_path = ([], "Array") }, params) ->
      not (is_dynamic_array_param (List.hd params))
   | _ -> false

let is_array_or_dyn_array haxe_type =
   match follow haxe_type with
   | TInst ({ cl_path = ([], "Array") }, _)
   | TType ({ t_path = ([], "Array")}, _) -> true
   | _ -> false

let is_array_implementer haxe_type =
   match follow haxe_type with
   | TInst ({ cl_array_access = Some _ }, _) -> true
   | _ -> false

let rec has_rtti_interface c interface =
   List.exists (function (t,pl) ->
      (snd t.cl_path) = interface && (match fst t.cl_path with | ["cpp";"rtti"] -> true | _ -> false )
   ) c.cl_implements ||
      (match c.cl_super with None -> false | Some (c,_) -> has_rtti_interface c interface)

let has_field_integer_lookup class_def =
   has_rtti_interface class_def "FieldIntegerLookup"

let has_field_integer_numeric_lookup class_def =
   has_rtti_interface class_def "FieldNumericIntegerLookup"

let should_implement_field x = is_physical_field x

let is_scalar_abstract abstract_def =
   Meta.has Meta.Scalar abstract_def.a_meta && Meta.has Meta.CoreType abstract_def.a_meta

let is_real_function field =
   match field.cf_kind with
   | Method MethNormal | Method MethInline-> true
   | _ -> false