open Extlib_leftovers
open Ast
open Type
open Error
open Common
open Globals
open CppAst
open CppTypeUtils

let follow = Abstract.follow_with_abstracts

(*
   A class_path is made from a package (array of strings) and a class name.
   Join these together, inclding a separator.  eg, "/" for includes : pack1/pack2/Name or "::"
   for namespace "pack1::pack2::Name"
*)
let join_class_path path separator =
  let result =
    match (fst path, snd path) with
    | [], s -> s
    | el, s -> String.concat separator el ^ separator ^ s
  in
  if String.contains result '+' then
    let idx = String.index result '+' in
    String.sub result 0 idx
    ^ String.sub result (idx + 1) (String.length result - idx - 1)
  else result

let class_text path = "::" ^ join_class_path path "::"

let is_internal_member member =
  member = "toString" || String.length member > 1 && String.sub member 0 2 = "__" &&
  match member with
  | "__ArgCount"
  | "__ArrayImplRef"
  | "__CStr"
  | "__Compare"
  | "__Create"
  | "__CreateEmpty"
  | "__FieldRef"
  | "__FindArgCount"
  | "__GetFieldMap"
  | "__GetHandle"
  | "__GetItem"
  | "__GetScriptCallable"
  | "__GetScriptVTable"
  | "__Param"
  | "__Remove"
  | "__SGetClass"
  | "__Set"
  | "__SetItem"
  | "__TArrayImplRef"
  | "__ToDouble"
  | "__ToInt"
  | "__ToInterface"
  | "__ToObject"
  | "__Visit"
  | "__WCStr"
  | "__a"
  | "__blit"
  | "__boot"
  | "__boot_all"
  | "__compare"
  | "__concat"
  | "__construct"
  | "__copy"
  | "__filter"
  | "__get_args"
  | "__hx_dump_stack"
  | "__hx_field_iter"
  | "__hxt_gc_new"
  | "__indexOf"
  | "__insert"
  | "__instanceof"
  | "__int"
  | "__iterator"
  | "__join"
  | "__lastIndexOf"
  | "__loadprim"
  | "__mClass"
  | "__mDynamicFields"
  | "__map"
  | "__memcmp"
  | "__new"
  | "__pop"
  | "__prime"
  | "__push"
  | "__qsort"
  | "__unshift"
  | "__unsafeStringReference"
  | "__time_stamp"
  | "__superString"
  | "__splice"
  | "__shift"
  | "__slice"
  | "__sort"
  | "__s_id"
  | "__run"
  | "__root"
  | "__register"
  | "__remove"
  | "__removeAt"
  | "__reverse"
  | "__zero"
  | "__Field"
  | "__IField"
  | "__Run"
  | "__Is"
  | "__GetClass"
  | "__GetType"
  | "__ToString"
  | "__s"
  | "__GetPtr"
  | "__SetField"
  | "__length"
  | "__IsArray"
  | "__SetThis"
  | "__Internal"
  | "__EnumParams"
  | "__Index"
  | "__Tag"
  | "__GetFields"
  | "__HasField"
  | "__get"
  | "__set"
  | "__unsafe_get"
  | "__unsafe_set"
  | "__global__"
  | "__SetSize"
  | "__trace"
  | "__GetRealObject"
  | "__SetSizeExact"
  | "__cpp__"
  | "__URLEncode"
  | "__URLDecode"
  | "__IsEnum" ->
    true
  | _ ->
    String.length member > 4 && String.sub member 0 4 = "__hx"

let is_known_member member =
  match member with "__meta__" | "__rtti" | "_Compare" -> true | _ -> false

(* Convert function names that can't be written in c++ ... *)
let keyword_remap name =
  if is_internal_member name || is_known_member name then name
  else if String.length name > 1 && String.sub name 0 2 = "__" then
    "_hx_" ^ name
  else
    match name with
    | "int"
    | "Int"
    | "Bool"
    | "super"
    | "auto"
    | "char"
    | "const"
    | "delete"
    | "double"
    | "Float"
    | "enum"
    | "extern"
    | "float"
    | "friend"
    | "goto"
    | "long"
    | "operator"
    | "protected"
    | "register"
    | "short"
    | "signed"
    | "sizeof"
    | "template"
    | "typedef"
    | "union"
    | "unsigned"
    | "void"
    | "volatile"
    | "or"
    | "and"
    | "xor"
    | "or_eq"
    | "not"
    | "and_eq"
    | "xor_eq"
    | "typeof"
    | "stdin"
    | "stdout"
    | "stderr"
    | "system"
    | "BIG_ENDIAN"
    | "LITTLE_ENDIAN"
    | "assert"
    | "NULL"
    | "wchar_t"
    | "EOF"
    | "bool"
    | "const_cast"
    | "dynamic_cast"
    | "explicit"
    | "export"
    | "mutable"
    | "namespace"
    | "reinterpret_cast"
    | "static_cast"
    | "typeid"
    | "typename"
    | "virtual"
    | "_Complex"
    | "INFINITY"
    | "NAN"
    | "INT_MIN"
    | "INT_MAX"
    | "INT8_MIN"
    | "INT8_MAX"
    | "UINT8_MAX"
    | "INT16_MIN"
    | "INT16_MAX"
    | "UINT16_MAX"
    | "INT32_MIN"
    | "INT32_MAX"
    | "UINT32_MAX"
    | "asm"
    | "near"
    | "far"
    | "_w64"
    | "HX_"
    | "HXLINE"
    | "HXDLIN"
    | "NO"
    | "YES"
    | "abstract"
    | "decltype"
    | "finally"
    | "nullptr"
    | "static_assert"
    | "struct"
    | "_Atomic"
    | "constexpr"
    | "consteval"
    | "constinit"
    | "co_await"
    | "co_return"
    | "co_yield"
    | "alignas"
    | "alignof"
    | "_Alignas"
    | "_Alignof"
    | "requires" ->
      "_hx_" ^ name
    | x -> x

let remap_class_path class_path =
  let path_remap with_keywords name =
    let len = String.length name in
    if len > 3 && String.sub name 0 3 = " ::" then String.sub name 3 (len - 3)
    else if len > 2 && String.sub name 0 2 = "::" then
      String.sub name 2 (len - 2)
    else if with_keywords then keyword_remap name
    else name
  in
  ( List.map (path_remap true) (fst class_path),
    path_remap false (snd class_path) )

let join_class_path_remap path separator =
  match join_class_path (remap_class_path path) separator with
  | "Class" -> "hx::Class"
  | x -> x

let rec s_tcpp = function
  | CppInt _ -> "CppInt"
  | CppFloat _ -> "CppFloat"
  | CppString _ -> "CppString"
  | CppBool _ -> "CppBool"
  | CppNull -> "CppNull"
  | CppNil -> "CppNil"
  | CppThis _ -> "CppThis"
  | CppSuper _ -> "CppSuper"
  | CppCode _ -> "CppCode"
  | CppClosure _ -> "CppClosure"
  | CppVar (VarLocal _) -> "CppVarLocal"
  | CppVar (VarClosure _) -> "CppVarClosure"
  | CppVar (VarThis _) -> "CppVarThis"
  | CppVar (VarInstance (expr, field, clazz, op)) ->
      "CppVarInstance(" ^ clazz ^ "::" ^ op ^ field.cf_name ^ ")"
  | CppVar (VarInterface _) -> "CppVarInterface"
  | CppVar (VarStatic (_, true, _)) -> "CppObjcVarStatic"
  | CppVar (VarStatic _) -> "CppVarStatic"
  | CppVar (VarInternal _) -> "CppVarInternal"
  | CppDynamicField _ -> "CppDynamicField"
  | CppExtern _ -> "CppExtern"
  | CppFunction _ -> "CppFunction"
  | CppEnumIndex _ -> "CppEnumIndex"
  | CppEnumField _ -> "CppEnumField"
  | CppNullAccess -> "CppNullAccess"
  | CppCall (FuncThis _, _) -> "CppCallThis"
  | CppCall (FuncInstance (obj, inst, field), _) ->
      (match inst with
      | InstObjC -> "CppCallObjCInstance("
      | InstPtr -> "CppCallInstance("
      | _ -> "CppCallStruct(")
      ^ tcpp_to_string obj.cpptype ^ "," ^ field.cf_name ^ ")"
  | CppCall (FuncInterface _, _) -> "CppCallInterface"
  | CppCall (FuncStatic (_, objC, _), _) ->
      if objC then "CppCallStaticObjC" else "CppCallStatic"
  | CppCall (FuncTemplate _, _) -> "CppCallTemplate"
  | CppCall (FuncEnumConstruct _, _) -> "CppCallEnumConstruct"
  | CppCall (FuncSuperConstruct _, _) -> "CppCallSuperConstruct"
  | CppCall (FuncSuper _, _) -> "CppCallSuper"
  | CppCall (FuncNew _, _) -> "CppCallNew"
  | CppCall (FuncExpression _, _) -> "CppCallExpression"
  | CppCall (FuncInternal _, _) -> "CppCallInternal"
  | CppCall (FuncExtern _, _) -> "CppCallExtern"
  | CppCall (FuncFromStaticFunction, _) -> "CppCallFromStaticFunction"
  | CppNewNative _ -> "CppNewNative"
  | CppAddressOf _ -> "CppAddressOf"
  | CppDereference _ -> "CppDereference"
  | CppFunctionAddress _ -> "CppFunctionAddress"
  | CppArray _ -> "CppArray"
  | CppCrement _ -> "CppCrement"
  | CppSet _ -> "CppSet"
  | CppModify _ -> "CppModify"
  | CppBinop _ -> "CppBinop"
  | CppCompare _ -> "CppCompare"
  | CppNullCompare _ -> "CppNullCompare"
  | CppObjectDecl _ -> "CppObjectDecl"
  | CppPosition _ -> "CppPosition"
  | CppArrayDecl _ -> "CppArrayDecl"
  | CppUnop _ -> "CppUnop"
  | CppVarDecl _ -> "CppVarDecl"
  | CppBlock _ -> "CppBlock"
  | CppFor _ -> "CppFor"
  | CppIf _ -> "CppIf"
  | CppWhile _ -> "CppWhile"
  | CppIntSwitch _ -> "CppIntSwitch"
  | CppSwitch _ -> "CppSwitch"
  | CppTry _ -> "CppTry"
  | CppBreak -> "CppBreak"
  | CppContinue -> "CppContinue"
  | CppClassOf _ -> "CppClassOf"
  | CppGoto _ -> "CppGoto"
  | CppReturn _ -> "CppReturn"
  | CppThrow _ -> "CppThrow"
  | CppEnumParameter _ -> "CppEnumParameter"
  | CppTCast _ -> "CppTCast"
  | CppCast _ -> "CppCast"
  | CppCastStatic _ -> "CppCastStatic"
  | CppCastScalar _ -> "CppCastScalar"
  | CppCastVariant _ -> "CppCastVariant"
  | CppCastObjC _ -> "CppCastObjC"
  | CppCastObjCBlock _ -> "CppCastObjCBlock"
  | CppCastProtocol _ -> "CppCastProtocol"
  | CppCastNative _ -> "CppCastNative"

and tcpp_to_string_suffix suffix tcpp =
  match tcpp with
  | TCppDynamic -> " ::Dynamic"
  | TCppUnchanged -> " ::Dynamic/*Unchanged*/"
  | TCppObject -> " ::Dynamic"
  | TCppObjectPtr -> " ::hx::Object *"
  | TCppReference t -> tcpp_to_string t ^ " &"
  | TCppStruct t -> "cpp::Struct< " ^ tcpp_to_string t ^ " >"
  | TCppStar (t, const) ->
      (if const then "const " else "") ^ tcpp_to_string t ^ " *"
  | TCppVoid -> "void"
  | TCppVoidStar -> "void *"
  | TCppRest _ -> "vaarg_list"
  | TCppVarArg -> "vararg"
  | TCppAutoCast -> "::cpp::AutoCast"
  | TCppVariant -> "::cpp::Variant"
  | TCppEnum enum -> " ::" ^ join_class_path_remap enum.e_path "::" ^ suffix
  | TCppScalar scalar -> scalar
  | TCppString -> "::String"
  | TCppFastIterator it ->
      "::cpp::FastIterator" ^ suffix ^ "< " ^ tcpp_to_string it ^ " >"
  | TCppPointer (ptrType, valueType) ->
      "::cpp::" ^ ptrType ^ "< " ^ tcpp_to_string valueType ^ " >"
  | TCppRawPointer (constName, valueType) ->
      constName ^ tcpp_to_string valueType ^ "*"
  | TCppFunction (argTypes, retType, abi) ->
      let args = String.concat "," (List.map tcpp_to_string argTypes) in
      "::cpp::Function< " ^ tcpp_to_string retType ^ " " ^ abi ^ " (" ^ args
      ^ ") >"
  | TCppObjCBlock (argTypes, retType) ->
      tcpp_objc_block_struct argTypes retType ^ "::t"
  | TCppDynamicArray -> "::cpp::VirtualArray" ^ suffix
  | TCppObjectArray _ -> "::Array" ^ suffix ^ "< ::Dynamic>"
  | TCppWrapped _ -> " ::Dynamic"
  | TCppScalarArray value ->
      "::Array" ^ suffix ^ "< " ^ tcpp_to_string value ^ " >"
  | TCppObjC klass ->
      let path = join_class_path_remap klass.cl_path "::" in
      if has_class_flag klass CInterface then "id < " ^ path ^ ">"
      else path ^ " *"
  | TCppProtocol interface ->
      let path =
        match get_meta_string interface.cl_meta Meta.ObjcProtocol with
        | Some p -> p
        | None -> join_class_path_remap interface.cl_path "::"
      in
      "id < " ^ path ^ ">"
  | TCppNativePointer klass ->
      let name = join_class_path_remap klass.cl_path "::" in
      if suffix = "_obj" then name else "::hx::Native< " ^ name ^ "* >"
  | TCppInst (klass, p) ->
      cpp_class_path_of klass p ^ if is_native_class klass then "" else suffix
  | TCppInterface klass when suffix = "_obj" ->
      cpp_class_path_of klass [] ^ suffix
  | TCppInterface _ -> "::Dynamic"
  | TCppClass -> "::hx::Class" ^ suffix
  | TCppGlobal -> "::Dynamic"
  | TCppNull -> " ::Dynamic"
  | TCppCode _ -> "Code"

and tcpp_objc_block_struct argTypes retType =
  let args = String.concat "," (List.map tcpp_to_string argTypes) in
  let ret = tcpp_to_string retType in
  let suffix = string_of_int (List.length argTypes) in
  if ret = "void" then
    if List.length argTypes = 0 then "::hx::TObjcBlockVoidVoid"
    else "::hx::TObjcBlockVoidArgs" ^ suffix ^ "< " ^ args ^ " >"
  else if List.length argTypes = 0 then "::hx::TObjcBlockRetVoid< " ^ ret ^ " >"
  else "::hx::TObjcBlockRetArgs" ^ suffix ^ "< " ^ ret ^ "," ^ args ^ " >"

and tcpp_to_string tcpp = tcpp_to_string_suffix "" tcpp

and cpp_class_path_of klass params =
  match get_meta_string klass.cl_meta Meta.Native with
  | Some s ->
      let typeParams =
        match params with
        | [] -> ""
        | _ -> "< " ^ String.concat "," (List.map tcpp_to_string params) ^ " >"
      in
      " " ^ join_class_path_remap klass.cl_path "::" ^ typeParams
  | None -> " ::" ^ join_class_path_remap klass.cl_path "::"

(*  Get a string to represent a type.
   The "suffix" will be nothing or "_obj", depending if we want the name of the
   pointer class or the pointee (_obj class *)
let rec class_string klass suffix params remap =
   let type_string = type_string_remap remap in
   let join_class_path_remap = if remap then join_class_path_remap else join_class_path in
   (match klass.cl_path with
   (* Array class *)
   |  ([],"Array") when is_dynamic_array_param (List.hd params) ->
           "cpp::ArrayBase" ^ suffix
           (*"cpp::VirtualArray" ^ suffix*)
   |  ([],"Array") -> (snd klass.cl_path) ^ suffix ^ "< " ^ (String.concat ","
               (List.map array_element_type params) ) ^ " >"
   (* FastIterator class *)
   |  (["cpp"],"FastIterator") -> "::cpp::FastIterator" ^ suffix ^ "< " ^ (String.concat ","
               (List.map type_string  params) ) ^ " >"
   |  (["cpp"],"Pointer")
   |  (["cpp"],"ConstPointer") ->
        "::cpp::Pointer< " ^ (String.concat "," (List.map type_string params) ) ^ " >"
   |  (["cpp"],"RawPointer") ->
        " " ^ (String.concat "," (List.map type_string params) ) ^ " * "
   |  (["cpp"],"RawConstPointer") ->
        " const " ^ (String.concat "," (List.map type_string params) ) ^ " * "
   |  (["cpp"],"Function") ->
        "::cpp::Function< " ^ (cpp_function_signature_params params) ^ " >"
   | _ when is_dynamic_type_param klass.cl_kind -> "Dynamic"
   |  ([],"#Int") -> "/* # */int"
   |  (["cpp"],"UInt8") -> "unsigned char"
   |  ([],"Class") -> "::hx::Class"
   |  ([],"EnumValue") -> "Dynamic"
   |  ([],"Null") -> (match params with
         | [t] ->
            (match follow t with
            | TAbstract ({ a_path = [],"Int" },_)
            | TAbstract ({ a_path = [],"Float" },_)
            | TAbstract ({ a_path = [],"Bool" },_) -> "Dynamic"
            | TAbstract ({ a_path = ["cpp"],"UInt8" },_) -> "Dynamic"
            | t when type_has_meta_key Meta.NotNull t -> "Dynamic"
            | _ -> "/*NULL*/" ^ (type_string t) )
         | _ -> die "" __LOC__);
   (* Objective-C class *)
   | path when is_objc_type (TInst(klass,[])) ->
      let str = join_class_path_remap klass.cl_path "::" in
      if suffix = "_obj" then
         str
      else if (has_class_flag klass CInterface) then
         "id < " ^ str ^ ">"
      else
         str ^ " *"
   (* Native interface - use pointer *)
   | _ when (has_class_flag klass CInterface) && is_native_gen_class klass ->
            (join_class_path_remap klass.cl_path "::") ^ " *"
   (* Normal class *)
   | _ when is_native_class klass ->
      let class_params = match params with
      | [] -> ""
      | _ -> "< " ^ (String.concat "," (List.map type_string params)) ^ " >" in
      (join_class_path_remap klass.cl_path "::") ^ class_params
   | _ ->
      let globalNamespace = match get_meta_string klass.cl_meta Meta.Native with
      | Some s -> s
      | None -> "::" in
      globalNamespace ^ (join_class_path_remap klass.cl_path "::") ^ suffix)

and type_has_meta_key key haxe_type =
   match follow haxe_type with
   | TInst (klass,_) -> Meta.has key klass.cl_meta
   | TType (type_def,_) -> Meta.has key type_def.t_meta
   | TEnum (enum_def,_) -> Meta.has key enum_def.e_meta
   | _ -> false

and type_string_suff suffix haxe_type remap =
   let type_string = type_string_remap remap in
   let join_class_path_remap = if remap then join_class_path_remap else join_class_path in
   (match haxe_type with
   | TMono r -> (match r.tm_type with None -> "Dynamic" ^ suffix | Some t -> type_string_suff suffix t remap)
   | TAbstract ({ a_path = ([],"Void") },[]) -> "Void"
   | TAbstract ({ a_path = ([],"Bool") },[]) -> "bool"
   | TAbstract ({ a_path = ([],"Float") },[]) -> "Float"
   | TAbstract ({ a_path = ([],"Int") },[]) -> "int"
   | TAbstract ({ a_path = (["cpp"],"UInt8") },[]) -> "unsigned char"
   | TAbstract( { a_path = ([], "EnumValue") }, _  ) -> "Dynamic"
   | TAbstract ({ a_path = ([],"Null") }, [t]) ->
    (match follow t with
    | TAbstract ({ a_path = [],"Int" },_)
    | TAbstract ({ a_path = [],"Float" },_)
    | TAbstract ({ a_path = [],"Bool" },_) -> "Dynamic" ^ suffix
    | t when type_has_meta_key Meta.NotNull t -> "Dynamic" ^ suffix
    | _ -> type_string_suff suffix t remap)
   | TEnum (enum,_) ->  (cpp_enum_path_of enum) ^ suffix
   | TInst (klass,params) ->  (class_string klass suffix params remap)
   | TType (type_def,params) ->
      (match type_def.t_path with
      | [] , "Array" ->
         (match params with
         | [t] when (type_string (follow t) ) = "Dynamic" -> "Dynamic"
         | [t] -> "Array< " ^ (type_string (follow t) ) ^ " >"
         | _ -> die "" __LOC__)
      | ["cpp"] , "FastIterator" ->
         (match params with
         | [t] -> "::cpp::FastIterator< " ^ (type_string (follow t) ) ^ " >"
         | _ -> die "" __LOC__)
      | ["cpp"] , "Pointer"
      | ["cpp"] , "ConstPointer" ->
         (match params with
         | [t] -> "::cpp::Pointer< " ^ (type_string (follow t) ) ^ " >"
         | _ -> die "" __LOC__)
      | ["cpp"] , "RawPointer" ->
         (match params with
         | [t] -> " " ^ (type_string (follow t) ) ^ " *"
         | _ -> die "" __LOC__)
      | ["cpp"] , "RawConstPointer" ->
         (match params with
         | [t] -> "const " ^ (type_string (follow t) ) ^ " *"
         | _ -> die "" __LOC__)
      | ["cpp"] , "Function" ->
         "::cpp::Function< " ^ (cpp_function_signature_params params ) ^ " >"
      | _ ->  type_string_suff suffix (apply_typedef type_def params) remap
      )
   | TFun (args,haxe_type) -> "Dynamic" ^ suffix
   | TAnon a -> "Dynamic"
      (*
      (match !(a.a_status) with
      | ClassStatics c -> type_string_suff suffix (TInst (c,List.map snd c.cl_params))
      | EnumStatics e -> type_string_suff suffix (TEnum (e,List.map snd e.e_params))
      | _ -> "Dynamic"  ^ suffix )
      *)
   | TDynamic haxe_type -> "Dynamic" ^ suffix
   | TLazy func -> type_string_suff suffix (lazy_type func) remap
   | TAbstract (abs,pl) when abs.a_impl <> None ->
      type_string_suff suffix (Abstract.get_underlying_type abs pl) remap
   | TAbstract (abs,pl) ->
      "::" ^ (join_class_path_remap abs.a_path "::") ^ suffix
   )

and type_string_remap remap haxe_type =
   type_string_suff "" haxe_type remap

and type_string haxe_type =
   type_string_suff "" haxe_type true
   
and cpp_enum_path_of enum =
   let globalNamespace =
      match get_meta_string enum.e_meta Meta.Native with
      | Some s -> s
      | None -> "::" in
   globalNamespace ^ (join_class_path_remap enum.e_path "::")

and array_element_type haxe_type =
   match type_string haxe_type with
   | x when cant_be_null haxe_type -> x
   | x when is_interface_type (follow haxe_type) -> x
   | "::String" -> "::String"
   | _ -> "::Dynamic"

and cpp_function_signature tfun abi =
   match follow tfun with
   | TFun(args,ret) -> (type_string ret) ^ " " ^ abi ^ "(" ^ (gen_tfun_interface_arg_list args) ^ ")"
   | _ -> "void *"

and cpp_function_signature_params params = match params with
   | [t; abi] -> (match follow abi with
       | TInst (klass,_) -> cpp_function_signature t (get_meta_string klass.cl_meta Meta.Abi |> Option.default "")
       | _ -> print_endline (type_string abi);
           die "" __LOC__ )
   | _ ->
      print_endline ("Params:" ^ (String.concat "," (List.map type_string params) ));
      die "" __LOC__;

and gen_interface_arg_type_name name opt typ =
   let type_str = (type_string typ) in
   (* type_str may have already converted Null<X> to Dynamic because of NotNull tag ... *)
   (if (opt && (cant_be_null typ) && type_str<>"Dynamic" ) then
      "::hx::Null< " ^ type_str ^ " > "
   else
      type_str ) ^ " " ^ (keyword_remap name)

and gen_tfun_interface_arg_list args =
   String.concat "," (List.map (fun (name,opt,typ) -> gen_interface_arg_type_name name opt typ) args)
   
and cant_be_null haxe_type =
   is_numeric haxe_type || (type_has_meta_key Meta.NotNull haxe_type )

let is_cpp_scalar cpp_type =
   match cpp_type with
   | TCppScalar(_) -> true
   | _ -> false

let is_cpp_array_implementer cppType =
   match cppType with
   | TCppInst ({ cl_array_access = Some _ }, _)
   | TCppInterface ({ cl_array_access = Some _ }) ->
      true
   | _ -> false

let rec cpp_is_struct_access t =
   match t with
   | TCppFunction _ -> true
   | TCppStruct _-> false
   | TCppInst (class_def, _) -> Meta.has Meta.StructAccess class_def.cl_meta
   | TCppReference (r) -> cpp_is_struct_access r
   | _ -> false

let rec cpp_is_native_array_access t =
   match t with
   | TCppStruct s -> cpp_is_native_array_access s
   | TCppReference s -> cpp_is_native_array_access s
   | TCppInst ({ cl_array_access = Some _ } as klass, _) when is_extern_class klass && Meta.has Meta.NativeArrayAccess klass.cl_meta -> true
   | _ -> false

let cpp_is_dynamic_type = function
   | TCppDynamic | TCppObject | TCppVariant | TCppWrapped _ | TCppGlobal | TCppNull
   | TCppInterface _
      -> true
   | _ -> false

let is_object_element member_type =
  match member_type with
   | TCppInst (x, _)
   | TCppInterface x
       -> not (is_extern_class x)
   | TCppDynamic
   | TCppObject
   | TCppObjectPtr
   | TCppEnum _
   | TCppString
   | TCppFunction _
   | TCppDynamicArray
   | TCppObjectArray _
   | TCppWrapped _
   | TCppScalarArray _
   | TCppClass
       -> true
   | _ -> false

let cpp_variant_type_of t = match t with
  | TCppDynamic
  | TCppUnchanged
  | TCppObject
  | TCppObjectPtr
  | TCppReference _
  | TCppStruct _
  | TCppStar _
  | TCppVoid
  | TCppFastIterator _
  | TCppDynamicArray
  | TCppObjectArray _
  | TCppScalarArray _
  | TCppWrapped _
  | TCppObjC _
  | TCppObjCBlock _
  | TCppRest _
  | TCppInst _
  | TCppInterface _
  | TCppProtocol _
  | TCppCode _
  | TCppClass
  | TCppGlobal
  | TCppNull
  | TCppEnum _ -> TCppDynamic
  | TCppString -> TCppString
  | TCppFunction _
  | TCppNativePointer _
  | TCppPointer _
  | TCppRawPointer _
  | TCppAutoCast
  | TCppVarArg
  | TCppVoidStar -> TCppVoidStar
  | TCppScalar "Int"
  | TCppScalar "bool"
  | TCppScalar "Float"  -> t
  | TCppScalar "::cpp::Int64" -> TCppScalar("Int64")
  | TCppScalar "double"
  | TCppScalar "float" -> TCppScalar("Float")
  | TCppScalar _  -> TCppScalar("int")
  | TCppVariant -> TCppVariant

let cpp_cast_variant_type_of t = match t with
  | TCppObjectArray _
  | TCppScalarArray _
  | TCppDynamicArray
  | TCppClass
  | TCppEnum _
  | TCppInst _ -> t
  | _ -> cpp_variant_type_of t

let enum_getter_type t =
  match cpp_variant_type_of t with
  | TCppString -> "String"
  | TCppScalar "int"  -> "Int"
  | TCppScalar "bool"  -> "Bool"
  | TCppScalar x  -> x
  | _  -> "Object"