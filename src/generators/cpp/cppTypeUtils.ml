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

let get_nth_type field index =
   match follow field.ef_type with
   | TFun (args,_) ->
      let rec nth l index = match l with
      | [] -> raise Not_found
      | (_,_,t)::rest ->
            if index = 0 then t
            else nth rest (index-1)
      in
      nth args index
   | _ -> raise Not_found

let is_dynamic_haxe_method f =
   match f.cf_expr, f.cf_kind with
   | Some { eexpr = TFunction _ }, (Var _ | Method MethDynamic) -> true
   | _ -> false

let has_dynamic_member_functions class_def =
   List.fold_left (fun result field ->
      match field.cf_expr with
      | Some { eexpr = TFunction function_def } when is_dynamic_haxe_method field -> true
      | _ -> result ) false class_def.cl_ordered_fields

let has_field_init field =
   match field.cf_expr with
   (* Function field *)
   | Some { eexpr = TFunction function_def } -> is_dynamic_haxe_method field
   (* Data field *)
   | Some _ -> true
   | _ -> false

let is_data_member field =
   match field.cf_kind with
   | Var _ | Method MethDynamic -> true
   | _ -> false

let is_override field =
   has_class_field_flag field CfOverride

let rec unreflective_type t =
   match follow t with
   | TInst (klass,_) ->  Meta.has Meta.Unreflective klass.cl_meta
   | TFun (args,ret) ->
      List.fold_left (fun result (_,_,t) -> result || (unreflective_type t)) (unreflective_type ret) args;
   | _ -> false

let reflective class_def field = not (
   (Meta.has Meta.NativeGen class_def.cl_meta) ||
   (Meta.has Meta.Unreflective class_def.cl_meta) ||
   (Meta.has Meta.Unreflective field.cf_meta) ||
   unreflective_type field.cf_type)

let has_init_field class_def =
   match TClass.get_cl_init class_def with
   | Some _ -> true
   | _ -> false

let is_abstract_impl class_def = match class_def.cl_kind with
   | KAbstractImpl _ -> true
   | _ -> false

let variable_field field =
   match field.cf_expr with
   | Some { eexpr = TFunction function_def } -> is_dynamic_haxe_method field
   | None when has_class_field_flag field CfAbstract -> false
   | _ -> true

let is_readable class_def field =
   match field.cf_kind with
   | Var { v_read = AccNever } when not (is_physical_field field) -> false
   | Var { v_read = AccInline } -> false
   | Var _ when is_abstract_impl class_def -> false
   | _ -> true

let is_writable class_def field =
   match field.cf_kind with
   | Var { v_write = AccNever } when not (is_physical_field field) -> false
   | Var { v_read = AccInline } -> false
   | Var _ when is_abstract_impl class_def -> false
   | _ -> true

let statics_except_meta class_def = (List.filter (fun static -> static.cf_name <> "__meta__" && static.cf_name <> "__rtti") class_def.cl_ordered_statics);;

let has_set_member_field class_def =
   let reflect_fields = List.filter (reflective class_def) (class_def.cl_ordered_fields) in
   let reflect_writable = List.filter (is_writable class_def) reflect_fields in
   List.exists variable_field reflect_writable

let has_set_static_field class_def =
   let reflect_fields = List.filter (reflective class_def) (statics_except_meta class_def) in
   let reflect_writable = List.filter (is_writable class_def) reflect_fields in
   List.exists variable_field reflect_writable

let has_get_fields class_def =
   let is_data_field field = (match follow field.cf_type with | TFun _ -> false | _ -> true) in
   List.exists is_data_field class_def.cl_ordered_fields

let has_get_member_field class_def =
   let reflect_fields = List.filter (reflective class_def) (class_def.cl_ordered_fields) in
   List.exists (is_readable class_def) reflect_fields

let has_get_static_field class_def =
   let reflect_fields = List.filter (reflective class_def) (statics_except_meta class_def) in
   List.exists (is_readable class_def) reflect_fields

let has_compare_field class_def =
   List.exists (fun f -> f.cf_name="__compare") class_def.cl_ordered_fields

let has_boot_field class_def =
   match TClass.get_cl_init class_def with
   | None -> List.exists has_field_init (List.filter should_implement_field class_def.cl_ordered_statics)
   | _ -> true

(*
   Functions are added in reverse order (oldest on right), then list is reversed because this is easier in ocaml
   The order is important because cppia looks up functions by index
*)
let current_virtual_functions_rev clazz base_functions =
   List.fold_left (fun result elem -> match follow elem.cf_type, elem.cf_kind  with
      | _, Method MethDynamic -> result
      | TFun (args,return_type), Method _  ->
          if (is_override elem ) then
            if List.exists (fun (e,a,r) -> e.cf_name=elem.cf_name ) result then
               result
            else
               (elem,args,return_type) :: result
          else
             (elem,args,return_type) :: result
      | _,_ -> result
    ) base_functions clazz.cl_ordered_fields

let all_virtual_functions clazz =
  let rec all_virtual_functions_rec clazz =
   current_virtual_functions_rev clazz (match clazz.cl_super with
       | Some def -> all_virtual_functions_rec (fst def)
       | _ -> []
     )
   in
   List.rev (all_virtual_functions_rec clazz)

let class_name class_def =
  let (_, class_path) = class_def.cl_path in
  let nativeGen       = Meta.has Meta.NativeGen class_def.cl_meta in
  class_path ^ if nativeGen then "" else "_obj"

let class_pointer class_def = "::hx::ObjectPtr< " ^ class_name class_def ^ " >"