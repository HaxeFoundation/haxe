open Extlib_leftovers
open Ast
open Type
open Error
open Common
open Globals
open CppExprUtils
open CppTypeUtils
open CppAst
open CppAstTools
open CppContext

let rec cpp_type_of stack haxe_type =
  if List.exists (fast_eq haxe_type) stack then TCppDynamic
  else
    let stack = haxe_type :: stack in
    match haxe_type with
    | TMono r -> (
        match r.tm_type with
        | None -> TCppDynamic
        | Some t -> cpp_type_of stack t)
    | TEnum (enum, params) -> TCppEnum enum
    | TInst ({ cl_path = [], "Array"; cl_kind = KTypeParameter _ }, _) ->
        TCppObject
    | TInst ({ cl_kind = KTypeParameter _ }, _) -> TCppDynamic
    | TInst (klass, params) -> cpp_instance_type stack klass params
    | TAbstract (abs, pl) when not (Meta.has Meta.CoreType abs.a_meta) ->
        cpp_type_from_path stack abs.a_path pl (fun () ->
            cpp_type_of stack
              (Abstract.get_underlying_type ~return_first:true abs pl))
    | TAbstract (a, params) ->
        cpp_type_from_path stack a.a_path params (fun () ->
            if is_scalar_abstract a then
              match get_meta_string a.a_meta Meta.Native with
              | Some s -> TCppScalar s
              | None -> TCppScalar (join_class_path a.a_path "::")
            else TCppDynamic)
    | TType (type_def, params) ->
        cpp_type_from_path stack type_def.t_path params (fun () ->
            cpp_type_of stack (apply_typedef type_def params))
    | TFun _ -> TCppObject
    | TAnon _ -> TCppObject
    | TDynamic _ -> TCppDynamic
    | TLazy func -> cpp_type_of stack (lazy_type func)

and cpp_type_from_path stack path params default =
  match (path, params) with
  | ([], "Void"), _ -> TCppVoid
  | ([], "void"), _ -> TCppVoid (* for old code with @:void *)
  | ([], "Bool"), _ -> TCppScalar "bool"
  | ([], "Float"), _ -> TCppScalar "Float"
  | ([], "Int"), _ -> TCppScalar "int"
  | ([], "EnumValue"), _ -> TCppObject
  | ([], "Class"), _ -> TCppClass
  | ([], "Enum"), _ -> TCppClass
  | ([], "Single"), _ -> TCppScalar "float"
  | ([ "cpp" ], "Char"), _ -> TCppScalar "char"
  | ([ "cpp" ], "Object"), _ -> TCppObjectPtr
  | ([ "cpp" ], "Float32"), _ -> TCppScalar "float"
  | ([ "cpp" ], "Float64"), _ -> TCppScalar "double"
  | ([ "cpp" ], "Int8"), _ -> TCppScalar "signed char"
  | ([ "cpp" ], "Int16"), _ -> TCppScalar "short"
  | ([ "cpp" ], "Int32"), _ -> TCppScalar "int"
  | ([ "cpp" ], "Int64"), _ -> TCppScalar "::cpp::Int64"
  | ([ "cpp" ], "UInt8"), _ -> TCppScalar "unsigned char"
  | ([ "cpp" ], "UInt16"), _ -> TCppScalar "unsigned short"
  | ([ "cpp" ], "UInt32"), _ -> TCppScalar "unsigned int"
  | ([ "cpp" ], "UInt64"), _ -> TCppScalar "::cpp::UInt64"
  | ([ "cpp" ], "VarArg"), _ -> TCppVarArg
  | ([ "cpp" ], "AutoCast"), _ -> TCppAutoCast
  | ([], "String"), [] -> TCppString
  (* Things with type parameters hxcpp knows about ... *)
  | ([ "cpp" ], "FastIterator"), [ p ] -> TCppFastIterator (cpp_type_of stack p)
  | ([ "cpp" ], "Pointer"), [ p ] -> TCppPointer ("Pointer", cpp_type_of stack p)
  | ([ "cpp" ], "ConstPointer"), [ p ] ->
      TCppPointer ("ConstPointer", cpp_type_of stack p)
  | ([ "cpp" ], "RawPointer"), [ p ] -> TCppRawPointer ("", cpp_type_of stack p)
  | ([ "cpp" ], "RawConstPointer"), [ p ] ->
      TCppRawPointer ("const ", cpp_type_of stack p)
  | ([ "cpp" ], "Function"), [ function_type; abi ] ->
      cpp_function_type_of stack function_type abi
  | ([ "cpp" ], "Callable"), [ function_type ]
  | ([ "cpp" ], "CallableData"), [ function_type ] ->
      cpp_function_type_of_string stack function_type ""
  | ("cpp" :: [ "objc" ], "ObjcBlock"), [ function_type ] ->
      let args, ret = cpp_function_type_of_args_ret stack function_type in
      TCppObjCBlock (args, ret)
  | ([ "cpp" ], "Rest"), [ rest ] -> TCppRest (cpp_type_of stack rest)
  | ("cpp" :: [ "objc" ], "Protocol"), [ interface_type ] -> (
      match follow interface_type with
      | TInst (klass, []) when has_class_flag klass CInterface ->
          TCppProtocol klass
      (* TODO - get the line number here *)
      | _ ->
          print_endline "cpp.objc.Protocol must refer to an interface";
          die "" __LOC__)
  | ([ "cpp" ], "Reference"), [ param ] ->
      TCppReference (cpp_type_of stack param)
  | ([ "cpp" ], "Struct"), [ param ] -> TCppStruct (cpp_type_of stack param)
  | ([ "cpp" ], "Star"), [ param ] ->
      TCppStar (cpp_type_of_pointer stack param, false)
  | ([ "cpp" ], "ConstStar"), [ param ] ->
      TCppStar (cpp_type_of_pointer stack param, true)
  | ([], "Array"), [ p ] -> (
      let arrayOf = cpp_type_of stack p in
      match arrayOf with
      | TCppVoid (* ? *) | TCppDynamic -> TCppDynamicArray
      | TCppObject | TCppObjectPtr | TCppReference _ | TCppStruct _ | TCppStar _
      | TCppEnum _ | TCppInst _ | TCppInterface _ | TCppProtocol _ | TCppClass
      | TCppDynamicArray | TCppObjectArray _ | TCppScalarArray _ ->
          TCppObjectArray arrayOf
      | _ -> TCppScalarArray arrayOf)
  | ([], "Null"), [ p ] -> cpp_type_of_null stack p
  | _ -> default ()

and cpp_type_of_null stack p =
  let baseType = cpp_type_of stack p in
  if type_has_meta_key Meta.NotNull p || is_cpp_scalar baseType then TCppObject
  else baseType

and cpp_type_of_pointer stack p =
  match p with
  | TAbstract ({ a_path = [], "Null" }, [ t ]) -> cpp_type_of stack t
  | x -> cpp_type_of stack x

(* Optional types are Dynamic if they norally could not be null *)
and cpp_fun_arg_type_of stack tvar opt =
  match opt with
  | Some _ -> cpp_type_of_null stack tvar.t_type
  | _ -> cpp_type_of stack tvar.t_type

and cpp_tfun_arg_type_of stack opt t =
  if opt then cpp_type_of_null stack t else cpp_type_of stack t

and cpp_function_type_of stack function_type abi =
  let abi =
    match follow abi with
    | TInst (klass1, _) ->
        get_meta_string klass1.cl_meta Meta.Abi |> Option.default ""
    | _ -> die "" __LOC__
  in
  cpp_function_type_of_string stack function_type abi

and cpp_function_type_of_string stack function_type abi_string =
  let args, ret = cpp_function_type_of_args_ret stack function_type in
  TCppFunction (args, ret, abi_string)

and cpp_function_type_of_args_ret stack function_type =
  match follow function_type with
  | TFun (args, ret) ->
      (* Optional types are Dynamic if they norally could not be null *)
      let cpp_arg_type_of (_, optional, haxe_type) =
        if optional then cpp_type_of_null stack haxe_type
        else cpp_type_of stack haxe_type
      in
      (List.map cpp_arg_type_of args, cpp_type_of stack ret)
  | _ ->
      (* ? *)
      ([ TCppVoid ], TCppVoid)

and cpp_instance_type stack klass params =
  cpp_type_from_path stack klass.cl_path params (fun () ->
      if is_objc_class klass then TCppObjC klass
      else if has_class_flag klass CInterface && is_native_gen_class klass then
        TCppNativePointer klass
      else if has_class_flag klass CInterface then TCppInterface klass
      else if
        has_class_flag klass CExtern && not (is_internal_class klass.cl_path)
      then
        let tcpp_params = List.map (cpp_type_of stack) params in
        TCppInst (klass, tcpp_params)
      else
        let tcpp_params = List.map (cpp_type_of stack) params in
        TCppInst (klass, tcpp_params))

let cpp_type_of = cpp_type_of []
let cpp_type_from_path = cpp_type_from_path []
let cpp_type_of_null = cpp_type_of_null []
let cpp_type_of_pointer = cpp_type_of_pointer []
let cpp_tfun_arg_type_of = cpp_tfun_arg_type_of []
let cpp_function_type_of = cpp_function_type_of []
let cpp_function_type_of_string = cpp_function_type_of_string []
let cpp_function_type_of_args_ret = cpp_function_type_of_args_ret []
let cpp_instance_type = cpp_instance_type []

let expression ctx request_type function_args function_type expression_tree forInjection =
  let rev_closures = ref [] in
  let closureId = ref 0 in
  let declarations = ref (Hashtbl.create 0) in
  let undeclared = ref (Hashtbl.create 0) in
  let uses_this = ref None in
  let gc_stack = ref false in
  let injection = ref forInjection in
  let this_real = ref (if ctx.ctx_real_this_ptr then ThisReal else ThisDynamic) in
  let file_id = ctx.ctx_file_id in
  let function_return_type = ref (cpp_type_of function_type) in
  let loop_stack = ref [] in
  let forCppia = Common.defined ctx.ctx_common Define.Cppia in
  let alloc_file_id () =
    incr file_id;
    !file_id
  in
  let begin_loop () =
    loop_stack := (alloc_file_id (), ref false) :: !loop_stack;
    fun () ->
      match !loop_stack with
      | (label_id, used) :: tl ->
          loop_stack := tl;
          if !used then label_id else -1
      | [] -> abort "Invalid inernal loop handling" expression_tree.epos
  in

  (* '__trace' is at the top-level *)
  Hashtbl.add !declarations "__trace" ();
  List.iter (fun arg -> Hashtbl.add !declarations arg.v_name ()) function_args;

  (* Helper functions *)

  let cpp_const_type cval =
    match cval with
    | TInt i -> (CppInt i, TCppScalar "int")
    | TBool b -> (CppBool b, TCppScalar "bool")
    | TFloat f -> (CppFloat (Texpr.replace_separators f ""), TCppScalar "Float")
    | TString s -> (CppString s, TCppString)
    | _ ->
        (* TNull, TThis & TSuper should already be handled *)
        (CppNull, TCppNull)
  in

  let cpp_return_type haxe_type =
    match haxe_type with TFun (_, ret) -> cpp_type_of ret | _ -> TCppDynamic
  in

  let cpp_member_return_type member = cpp_return_type member.cf_type in

  let is_cpp_objc_type cpptype =
    match cpptype with TCppObjC _ -> true | _ -> false
  in

  let cpp_is_real_array obj =
    match obj.cpptype with
    | TCppScalarArray _ | TCppObjectArray _ -> true
    | _ -> false
  in

  let rec to_lvalue value =
    match value.cppexpr with
    | CppVar (VarClosure var as varloc)
      when is_gc_element ctx (cpp_type_of var.v_type) ->
        (CppVarRef varloc, true)
    | CppVar (VarThis (member, _) as varloc)
      when is_gc_element ctx (cpp_type_of member.cf_type) ->
        (CppVarRef varloc, true)
    | CppVar (VarInstance (obj, member, _, "->") as varloc)
      when is_gc_element ctx (cpp_type_of member.cf_type) ->
        (CppVarRef varloc, true)
    | CppVar varloc -> (CppVarRef varloc, false)
    | CppArray arrayloc ->
        ( CppArrayRef arrayloc,
          match arrayloc with
          | ArrayObject (arrayObj, index, _) when is_gc_element ctx TCppDynamic
            ->
              true
          | ArrayTyped (arrayObj, index, t) when is_gc_element ctx t -> true
          | _ -> false )
    | CppDynamicField (expr, name) -> (CppDynamicRef (expr, name), false)
    | CppTCast (cppExpr, _)
    | CppCast (cppExpr, _)
    | CppCastStatic (cppExpr, _)
    | CppCastObjC (cppExpr, _)
    | CppCastObjCBlock (cppExpr, _, _)
    | CppCastScalar (cppExpr, _) ->
        to_lvalue cppExpr
    | CppCastVariant cppExpr -> to_lvalue cppExpr
    | CppExtern (name, isGlobal) -> (CppExternRef (name, isGlobal), false)
    | _ ->
        abort
          ("Could not convert expression to l-value (" ^ s_tcpp value.cppexpr
         ^ ")")
          value.cpppos
  in

  let is_array_splice_call obj member =
    match (obj.cpptype, member.cf_name) with
    | TCppScalarArray _, "splice" | TCppObjectArray _, "splice" -> true
    | _, _ -> false
  in

  let is_map_get_call obj member =
    member.cf_name = "get"
    &&
    match obj.cpptype with
    | TCppInst ({ cl_path = [ "cpp" ], "Int64Map" }, _) -> true
    | TCppInst ({ cl_path = [ "haxe"; "ds" ], "IntMap" }, _) -> true
    | TCppInst ({ cl_path = [ "haxe"; "ds" ], "StringMap" }, _) -> true
    | TCppInst ({ cl_path = [ "haxe"; "ds" ], "ObjectMap" }, _) -> true
    | _ -> false
  in

  let is_map_set_call obj member =
    member.cf_name = "set"
    &&
    match obj.cpptype with
    | TCppInst ({ cl_path = [ "cpp" ], "Int64Map" }, _) -> true
    | TCppInst ({ cl_path = [ "haxe"; "ds" ], "IntMap" }, _) -> true
    | TCppInst ({ cl_path = [ "haxe"; "ds" ], "StringMap" }, _) -> true
    | TCppInst ({ cl_path = [ "haxe"; "ds" ], "ObjectMap" }, _) -> true
    | _ -> false
  in

  let is_array_concat_call obj member =
    match (obj.cpptype, member.cf_name) with
    | TCppScalarArray _, "concat" | TCppObjectArray _, "concat" -> true
    | _, _ -> false
  in

  let cpp_can_static_cast funcType inferredType =
    match funcType with
    | TCppReference _ | TCppStar _ | TCppStruct _ -> false
    | _ -> (
        match inferredType with
        | TCppInst (cls, _) when is_extern_class cls -> false
        | TCppEnum e when is_extern_enum e -> false
        | TCppInst _ | TCppClass | TCppEnum _ ->
            tcpp_to_string funcType <> tcpp_to_string inferredType
        | _ -> false)
  in

  let cpp_is_templated_call ctx member =
    Meta.has Meta.TemplatedCall member.cf_meta
  in

  let is_complex_compare = function
    | TCppScalar _ -> false
    | TCppString -> false
    | _ -> true
  in

  let is_pointer_compare = function
    | TCppObjectArray _ | TCppScalarArray _ | TCppDynamicArray | TCppClass
    | TCppEnum _ ->
        true
    | _ -> false
  in

  let is_instance_compare = function
    | TCppInterface _ | TCppInst _ -> true
    | _ -> false
  in

  let cpp_append_block block expr =
    match block.cppexpr with
    | CppBlock (expr_list, closures, gc_stack) ->
        {
          block with
          cppexpr = CppBlock (expr_list @ [ expr ], closures, gc_stack);
        }
    | _ -> abort "Internal error appending expression" block.cpppos
  in

  let rec const_int_of expr =
    match expr.eexpr with
    | TConst TInt x -> x
    | TConst TBool x -> Int32.of_int (if x then 1 else 0)
    | TParenthesis e -> const_int_of e
    | _ -> raise Not_found
  in

  (* Core Retyping *)
  let rec retype return_type expr =
    let cpp_type_of t = cpp_type_of t in
    let mk_cppexpr newExpr newType =
      { cppexpr = newExpr; cpptype = newType; cpppos = expr.epos }
    in
    let retype_function_args args arg_types =
      let rec map_pair args types result =
        match (args, types) with
        | args, [ TCppRest rest ] ->
            List.rev (List.map (retype rest) args) @ result
        | [], [] -> result
        | a :: arest, t :: trest -> map_pair arest trest (retype t a :: result)
        | _, [] -> abort "Too many args" expr.epos
        | [], _ -> abort "Too many types" expr.epos
      in
      List.rev (map_pair args arg_types [])
    in

    let retypedExpr, retypedType =
      match expr.eexpr with
      | TEnumParameter (enumObj, enumField, enumIndex) ->
          let retypedObj = retype TCppDynamic enumObj in
          ( CppEnumParameter (retypedObj, enumField, enumIndex),
            cpp_cast_variant_type_of
              (cpp_type_of (get_nth_type enumField enumIndex)) )
      | TEnumIndex enumObj ->
          let retypedObj = retype TCppDynamic enumObj in
          (CppEnumIndex retypedObj, TCppScalar "int")
      | TConst TThis ->
          uses_this := Some !this_real;
          ( CppThis !this_real,
            if !this_real = ThisDynamic then TCppDynamic
            else cpp_type_of expr.etype )
      | TConst TSuper ->
          uses_this := Some !this_real;
          ( CppSuper !this_real,
            if !this_real = ThisDynamic then TCppDynamic
            else cpp_type_of expr.etype )
      | TConst TNull when is_objc_type expr.etype -> (CppNil, TCppNull)
      | TConst x -> cpp_const_type x
      | TIdent "__global__" ->
          (* functions/vars will appear to be members of the virtual global object *)
          (CppClassOf (([], ""), false), TCppGlobal)
      | TLocal tvar ->
          let name = tvar.v_name in
          if Hashtbl.mem !declarations name then
            (*print_endline ("Using existing tvar " ^ tvar.v_name);*)
            (CppVar (VarLocal tvar), cpp_type_of tvar.v_type)
          else (
            (*print_endline ("Missing tvar " ^ tvar.v_name);*)
            Hashtbl.replace !undeclared name tvar;
            if has_var_flag tvar VCaptured then
              (CppVar (VarClosure tvar), cpp_type_of tvar.v_type)
            else (CppExtern (name, false), cpp_type_of tvar.v_type))
      | TIdent name -> (CppExtern (name, false), return_type)
      | TBreak -> (
          if forCppia then (CppBreak, TCppVoid)
          else
            match !loop_stack with
            | [] -> (CppBreak, TCppVoid)
            | (label_id, used) :: _ ->
                used := true;
                (CppGoto label_id, TCppVoid))
      | TContinue -> (CppContinue, TCppVoid)
      | TThrow e1 -> (CppThrow (retype TCppDynamic e1), TCppVoid)
      | TMeta ((Meta.Fixed, _, _), e) -> (
          let cppType = retype return_type e in
          match cppType.cppexpr with
          | CppObjectDecl (def, false) ->
              (CppObjectDecl (def, true), cppType.cpptype)
          | _ -> (cppType.cppexpr, cppType.cpptype))
      | TMeta (_, e) | TParenthesis e ->
          let cppType = retype return_type e in
          (cppType.cppexpr, cppType.cpptype)
      | TField (obj, field) -> (
          match field with
          | FInstance (clazz, params, member)
          | FClosure (Some (clazz, params), member) -> (
              let funcReturn = cpp_member_return_type member in
              let clazzType = cpp_instance_type clazz params in
              let retypedObj = retype clazzType obj in
              let exprType = cpp_type_of member.cf_type in
              let is_objc = is_cpp_objc_type retypedObj.cpptype in

              if retypedObj.cpptype = TCppNull then (CppNullAccess, TCppDynamic)
              else if
                retypedObj.cpptype = TCppDynamic
                && not (has_class_flag clazz CInterface)
              then
                if is_internal_member member.cf_name then
                  ( CppFunction
                      (FuncInstance (retypedObj, InstPtr, member), funcReturn),
                    exprType )
                else (CppDynamicField (retypedObj, member.cf_name), TCppVariant)
              else if cpp_is_struct_access retypedObj.cpptype then
                match retypedObj.cppexpr with
                | CppThis ThisReal ->
                    (CppVar (VarThis (member, retypedObj.cpptype)), exprType)
                | CppSuper this ->
                    ( CppFunction
                        ( FuncSuper (this, retypedObj.cpptype, member),
                          funcReturn ),
                      exprType )
                | _ ->
                    if is_var_field member then
                      ( CppVar
                          (VarInstance
                             (retypedObj, member, tcpp_to_string clazzType, ".")),
                        exprType )
                    else
                      ( CppFunction
                          ( FuncInstance (retypedObj, InstStruct, member),
                            funcReturn ),
                        exprType )
              else if is_var_field member then
                let exprType =
                  match (retypedObj.cpptype, exprType) with
                  | TCppPointer (_, t), TCppDynamic
                  | ( TCppRawPointer (_, t),
                      TCppDynamic
                      (* the 'type parameter' will show up as Dynamic *) ) ->
                      t
                  | _ -> exprType
                in

                match retypedObj.cppexpr with
                | CppThis ThisReal ->
                    (CppVar (VarThis (member, retypedObj.cpptype)), exprType)
                | _ -> (
                    match (retypedObj.cpptype, member.cf_name) with
                    (* Special variable remapping ... *)
                    | TCppDynamicArray, "length" when not forCppia ->
                        ( CppCall
                            (FuncInternal (retypedObj, "get_length", "->"), []),
                          exprType )
                    | TCppInterface _, _ | TCppDynamic, _ ->
                        ( CppDynamicField (retypedObj, member.cf_name),
                          TCppVariant )
                    | TCppObjC _, _ ->
                        ( CppVar
                            (VarInstance
                               ( retypedObj,
                                 member,
                                 tcpp_to_string clazzType,
                                 "." )),
                          exprType )
                    | _ ->
                        let operator =
                          if
                            cpp_is_struct_access retypedObj.cpptype
                            || retypedObj.cpptype = TCppString
                          then "."
                          else "->"
                        in
                        ( CppVar
                            (VarInstance
                               ( retypedObj,
                                 member,
                                 tcpp_to_string clazzType,
                                 operator )),
                          exprType ))
              else if
                has_class_flag clazz CInterface
                && not is_objc (* Use instance call for objc interfaces *)
              then
                ( CppFunction
                    (FuncInterface (retypedObj, clazz, member), funcReturn),
                  exprType )
              else
                let isArrayObj =
                  match retypedObj.cpptype with
                  | TCppDynamicArray | TCppObjectArray _ | TCppScalarArray _ ->
                      true
                  | _ -> false
                in
                (* Special array return values *)
                let funcReturn =
                  if isArrayObj then
                    match member.cf_name with
                    | "map" -> TCppDynamicArray
                    | "splice" | "slice" | "concat" | "copy" | "filter" ->
                        retypedObj.cpptype
                    | _ -> funcReturn
                  else
                    match (retypedObj.cpptype, funcReturn) with
                    | TCppPointer (_, t), TCppDynamic
                    | ( TCppRawPointer (_, t),
                        TCppDynamic
                        (* the 'type parameter' will show up as Dynamic *) ) ->
                        t
                    | _ -> funcReturn
                in
                match retypedObj.cppexpr with
                | CppThis ThisReal ->
                    ( CppFunction
                        (FuncThis (member, retypedObj.cpptype), funcReturn),
                      exprType )
                | CppSuper this ->
                    ( CppFunction
                        ( FuncSuper (this, retypedObj.cpptype, member),
                          funcReturn ),
                      exprType )
                | _ ->
                    ( CppFunction
                        ( FuncInstance
                            ( retypedObj,
                              (if is_objc then InstObjC else InstPtr),
                              member ),
                          funcReturn ),
                      exprType ))
          | FStatic (_, ({ cf_name = "nativeFromStaticFunction" } as member)) ->
              let funcReturn = cpp_member_return_type member in
              let exprType = cpp_type_of member.cf_type in
              (CppFunction (FuncFromStaticFunction, funcReturn), exprType)
          | FStatic (clazz, member) ->
              let funcReturn = cpp_member_return_type member in
              let exprType = cpp_type_of member.cf_type in
              let objC = is_objc_class clazz in
              if is_var_field member then
                (CppVar (VarStatic (clazz, objC, member)), exprType)
              else
                ( CppFunction (FuncStatic (clazz, objC, member), funcReturn),
                  exprType )
          | FClosure (None, field) | FAnon field ->
              let obj = retype TCppDynamic obj in
              let fieldName = field.cf_name in
              if obj.cpptype = TCppGlobal then
                (CppExtern (fieldName, true), cpp_type_of expr.etype)
              else if obj.cpptype = TCppNull then (CppNullAccess, TCppDynamic)
              else if is_internal_member fieldName then
                let cppType = cpp_return_type expr.etype in
                if obj.cpptype = TCppString then
                  ( CppFunction (FuncInternal (obj, fieldName, "."), cppType),
                    cppType )
                else
                  ( CppFunction (FuncInternal (obj, fieldName, "->"), cppType),
                    cppType )
              else (CppDynamicField (obj, field.cf_name), TCppVariant)
          | FDynamic fieldName ->
              let obj = retype TCppDynamic obj in
              if obj.cpptype = TCppNull then (CppNullAccess, TCppDynamic)
              else if fieldName = "cca" && obj.cpptype = TCppString then
                ( CppFunction (FuncInternal (obj, "cca", "."), TCppScalar "int"),
                  TCppDynamic )
              else if fieldName = "__s" && obj.cpptype = TCppString then
                ( CppVar (VarInternal (obj, ".", "utf8_str()")),
                  TCppRawPointer ("const ", TCppScalar "char") )
              else if fieldName = "__Index" then
                (CppEnumIndex obj, TCppScalar "int")
              else if is_internal_member fieldName || cpp_is_real_array obj then
                let cppType = cpp_return_type expr.etype in
                if obj.cpptype = TCppString then
                  ( CppFunction (FuncInternal (obj, fieldName, "."), cppType),
                    cppType )
                else
                  ( CppFunction (FuncInternal (obj, fieldName, "->"), cppType),
                    cppType )
              else if obj.cpptype = TCppGlobal then
                (CppExtern (fieldName, true), cpp_type_of expr.etype)
              else if obj.cpptype = TCppClass then
                match obj.cppexpr with
                | CppClassOf (path, _) ->
                    ( CppExtern
                        ( join_class_path_remap path "::" ^ "_obj::" ^ fieldName,
                          true ),
                      cpp_type_of expr.etype )
                | _ ->
                    ( CppVar (VarInternal (obj, "->", fieldName)),
                      cpp_type_of expr.etype )
              else (CppDynamicField (obj, fieldName), TCppVariant)
          | FEnum (enum, enum_field) ->
              (CppEnumField (enum, enum_field), TCppEnum enum))
      | TCall ({ eexpr = TIdent "__cpp__" }, arg_list) ->
          let cppExpr =
            match arg_list with
            | [ { eexpr = TConst (TString code) } ] -> CppCode (code, [])
            | { eexpr = TConst (TString code) } :: remaining ->
                let retypedArgs =
                  List.map
                    (fun arg -> retype (TCppCode (cpp_type_of arg.etype)) arg)
                    remaining
                in
                CppCode (code, retypedArgs)
            | _ -> abort "__cpp__'s first argument must be a string" expr.epos
          in
          (cppExpr, TCppCode (cpp_type_of expr.etype))
      | TCall (func, args) -> (
          let retypedFunc = retype TCppUnchanged func in
          match retypedFunc.cpptype with
          | TCppNull -> (CppNullAccess, TCppDynamic)
          | TCppFunction (argTypes, retType, _) ->
              let retypedArgs = retype_function_args args argTypes in
              (CppCall (FuncExpression retypedFunc, retypedArgs), retType)
          | TCppObjCBlock (argTypes, retType) ->
              let retypedArgs = retype_function_args args argTypes in
              (CppCall (FuncExpression retypedFunc, retypedArgs), retType)
          | _ -> (
              let cppType = cpp_type_of expr.etype in
              match retypedFunc.cppexpr with
              | CppFunction (FuncFromStaticFunction, returnType) -> (
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  match retypedArgs with
                  | [
                   {
                     cppexpr =
                       CppFunction
                         (FuncStatic (clazz, false, member), funcReturn);
                   };
                  ] ->
                      (CppFunctionAddress (clazz, member), funcReturn)
                  | _ ->
                      abort
                        "cpp.Function.fromStaticFunction must be called on \
                         static function"
                        expr.epos)
              | CppEnumIndex _ ->
                  (* Not actually a TCall...*)
                  (retypedFunc.cppexpr, retypedFunc.cpptype)
              | CppFunction (FuncInstance (obj, InstPtr, member), _)
                when (not forCppia) && return_type = TCppVoid
                     && is_array_splice_call obj member ->
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  ( CppCall
                      ( FuncInstance
                          (obj, InstPtr, { member with cf_name = "removeRange" }),
                        retypedArgs ),
                    TCppVoid )
              | CppFunction (FuncInstance (obj, InstPtr, member), _)
                when is_array_concat_call obj member ->
                  let retypedArgs = List.map (retype obj.cpptype) args in
                  ( CppCall (FuncInstance (obj, InstPtr, member), retypedArgs),
                    return_type )
              | CppFunction (FuncStatic (obj, false, member), _)
                when member.cf_name = "::hx::AddressOf" ->
                  let arg = retype TCppUnchanged (List.hd args) in
                  let rawType =
                    match arg.cpptype with TCppReference x -> x | x -> x
                  in
                  (CppAddressOf arg, TCppRawPointer ("", rawType))
              | CppFunction (FuncStatic (obj, false, member), _)
                when member.cf_name = "::hx::StarOf" ->
                  let arg = retype TCppUnchanged (List.hd args) in
                  let rawType =
                    match arg.cpptype with TCppReference x -> x | x -> x
                  in
                  (CppAddressOf arg, TCppStar (rawType, false))
              | CppFunction (FuncStatic (obj, false, member), _)
                when member.cf_name = "::hx::Dereference" ->
                  let arg = retype TCppUnchanged (List.hd args) in
                  let rawType =
                    match arg.cpptype with TCppStar (x, _) -> x | x -> x
                  in
                  (CppDereference arg, TCppReference rawType)
              | CppFunction (FuncStatic (obj, false, member), _)
                when member.cf_name = "_hx_create_array_length" -> (
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  (* gc_stack - not needed yet *)
                  match return_type with
                  | TCppObjectArray _ | TCppScalarArray _ ->
                      (CppCall (FuncNew return_type, retypedArgs), return_type)
                  | _ ->
                      ( CppCall (FuncNew TCppDynamicArray, retypedArgs),
                        return_type ))
              | CppFunction (FuncStatic (obj, false, member), returnType)
                when cpp_is_templated_call ctx member -> (
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  match retypedArgs with
                  | { cppexpr = CppClassOf (path, native) } :: rest ->
                      ( CppCall (FuncTemplate (obj, member, path, native), rest),
                        returnType )
                  | _ ->
                      abort
                        "First parameter of template function must be a Class"
                        retypedFunc.cpppos)
              | CppFunction (FuncInstance (obj, InstPtr, member), _)
                when is_map_get_call obj member ->
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  let fname, cppType =
                    match return_type with
                    | TCppVoid | TCppScalar "bool" ->
                        ( (if forCppia then "getBool" else "get_bool"),
                          return_type )
                    | TCppScalar "int" ->
                        ((if forCppia then "getInt" else "get_int"), return_type)
                    | TCppScalar "::cpp::Int64" ->
                        ( (if forCppia then "getInt64" else "get_int64"),
                          return_type )
                    | TCppScalar "Float" ->
                        ( (if forCppia then "getFloat" else "get_float"),
                          return_type )
                    | TCppString ->
                        ( (if forCppia then "getString" else "get_string"),
                          return_type )
                    | _ -> ("get", TCppDynamic)
                  in
                  let func =
                    FuncInstance (obj, InstPtr, { member with cf_name = fname })
                  in
                  (*
                   if  cpp_can_static_cast cppType return_type then begin
                      let call = mk_cppexpr (CppCall(func,retypedArgs)) cppType in
                      CppCastStatic(call, cppType), cppType
                   end else
                   *)
                  (CppCall (func, retypedArgs), cppType)
              | CppFunction (FuncInstance (obj, InstPtr, member), _)
                when forCppia && is_map_set_call obj member ->
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  let fname =
                    match retypedArgs with
                    | [ _; { cpptype = TCppScalar "bool" } ] -> "setBool"
                    | [ _; { cpptype = TCppScalar "int" } ] -> "setInt"
                    | [ _; { cpptype = TCppScalar "::cpp::Int64" } ] ->
                        "setInt64"
                    | [ _; { cpptype = TCppScalar "Float" } ] -> "setFloat"
                    | [ _; { cpptype = TCppString } ] -> "setString"
                    | _ -> "set"
                  in
                  let func =
                    FuncInstance (obj, InstPtr, { member with cf_name = fname })
                  in
                  (CppCall (func, retypedArgs), cppType)
              | CppFunction
                  ((FuncInstance (obj, InstPtr, member) as func), returnType)
                when cpp_can_static_cast returnType cppType ->
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  let call =
                    mk_cppexpr (CppCall (func, retypedArgs)) returnType
                  in
                  (CppCastStatic (call, cppType), cppType)
              (*
                let error_printer file line = Printf.sprintf "%s:%d:" file line in
                let epos = Lexer.get_error_pos error_printer expr.epos in
                print_endline ( "fixed override " ^ member.cf_name ^ " @ " ^  epos ^ " " ^ (tcpp_to_string returnType) ^ "->" ^ (ctx_type_string ctx expr.etype) );
                CppCall(func,retypedArgs), returnType
              *)
              (* Other functions ... *)
              | CppFunction
                  ( (FuncInstance
                       (_, InstStruct, { cf_type = TFun (arg_types, _) }) as
                     func),
                    return_type ) ->
                  (* For struct access classes use the types of the arguments instead of the function argument types *)
                  (* In the case of generic extern classes a TFun arg type could be `MyClass.T` instead of the real type *)
                  let map_args func_arg passed_arg =
                    let name, opt, _ = func_arg in
                    (name, opt, passed_arg.etype)
                  in
                  let real_types = List.map2 map_args arg_types args in
                  let arg_types =
                    List.map
                      (fun (_, opt, t) -> cpp_tfun_arg_type_of opt t)
                      real_types
                  in
                  let retypedArgs = retype_function_args args arg_types in
                  (CppCall (func, retypedArgs), return_type)
              | CppFunction
                  ( (FuncInstance (_, _, { cf_type = TFun (arg_types, _) }) as
                     func),
                    returnType )
              | CppFunction
                  ( (FuncStatic (_, _, { cf_type = TFun (arg_types, _) }) as func),
                    returnType )
              | CppFunction
                  ( (FuncThis ({ cf_type = TFun (arg_types, _) }, _) as func),
                    returnType ) ->
                  let arg_types =
                    List.map
                      (fun (_, opt, t) -> cpp_tfun_arg_type_of opt t)
                      arg_types
                  in
                  (* retype args specifically (not just CppDynamic) *)
                  let retypedArgs = retype_function_args args arg_types in
                  (CppCall (func, retypedArgs), returnType)
              | CppFunction (func, returnType) ->
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  (CppCall (func, retypedArgs), returnType)
              | CppEnumField (enum, field) ->
                  (* TODO - proper re-typing *)
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  ( CppCall (FuncEnumConstruct (enum, field), retypedArgs),
                    cppType )
              | CppSuper _ ->
                  (* TODO - proper re-typing *)
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  ( CppCall (FuncSuperConstruct retypedFunc.cpptype, retypedArgs),
                    TCppVoid )
              | CppDynamicField (expr, name) -> (
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  (* Special function calls *)
                  match (expr.cpptype, name) with
                  | TCppGlobal, _ ->
                      let retypedArgs = List.map (retype TCppUnchanged) args in
                      (CppCall (FuncExtern (name, true), retypedArgs), cppType)
                  | TCppString, _ ->
                      ( CppCall (FuncInternal (expr, name, "."), retypedArgs),
                        cppType )
                  | _, "__Tag" ->
                      ( CppCall
                          (FuncInternal (expr, "_hx_getTag", "->"), retypedArgs),
                        cppType )
                  | _, name when is_internal_member name ->
                      ( CppCall (FuncInternal (expr, name, "->"), retypedArgs),
                        cppType )
                  | _ ->
                      (* not special *)
                      ( CppCall (FuncExpression retypedFunc, retypedArgs),
                        TCppDynamic ))
              | CppExtern (name, isGlobal) ->
                  let retypedArgs = List.map (retype TCppUnchanged) args in
                  (CppCall (FuncExtern (name, isGlobal), retypedArgs), cppType)
              | _ ->
                  let retypedArgs = List.map (retype TCppDynamic) args in
                  ( CppCall (FuncExpression retypedFunc, retypedArgs),
                    TCppDynamic )))
      | TNew (class_def, params, args) ->
          let constructor_type =
            match
              OverloadResolution.maybe_resolve_constructor_overload class_def
                params args
            with
            | None -> abort "Could not find overload" expr.epos
            | Some (_, constructor, _) -> constructor.cf_type
          in
          let arg_types, _ = cpp_function_type_of_args_ret constructor_type in
          let retypedArgs = retype_function_args args arg_types in
          let created_type = cpp_type_of expr.etype in
          (gc_stack :=
             !gc_stack
             ||
             match created_type with
             | TCppInst (t, _) -> not (is_native_class t)
             | _ -> false);
          (CppCall (FuncNew created_type, retypedArgs), created_type)
      | TFunction func ->
          let old_this_real = !this_real in
          this_real := ThisFake;
          (* TODO - this_dynamic ? *)
          let old_undeclared = Hashtbl.copy !undeclared in
          let old_declarations = Hashtbl.copy !declarations in
          let old_uses_this = !uses_this in
          let old_gc_stack = !gc_stack in
          let old_return_type = !function_return_type in
          let ret = cpp_type_of func.tf_type in
          function_return_type := ret;
          uses_this := None;
          undeclared := Hashtbl.create 0;
          declarations := Hashtbl.create 0;
          List.iter
            (fun (tvar, _) -> Hashtbl.add !declarations tvar.v_name ())
            func.tf_args;
          let cppExpr = retype TCppVoid (mk_block func.tf_expr) in
          let result =
            {
              close_expr = cppExpr;
              close_id = !closureId;
              close_undeclared = !undeclared;
              close_type = ret;
              close_args = func.tf_args;
              close_this = !uses_this;
            }
          in
          incr closureId;
          declarations := old_declarations;
          undeclared := old_undeclared;
          Hashtbl.iter
            (fun name tvar ->
              if not (Hashtbl.mem !declarations name) then
                Hashtbl.replace !undeclared name tvar)
            result.close_undeclared;
          function_return_type := old_return_type;
          this_real := old_this_real;
          uses_this :=
            if !uses_this != None then Some old_this_real else old_uses_this;
          gc_stack := old_gc_stack;
          rev_closures := result :: !rev_closures;
          (CppClosure result, TCppDynamic)
      | TArray (e1, e2) ->
          let arrayExpr, elemType =
            match cpp_is_native_array_access (cpp_type_of e1.etype) with
            | true ->
                let retypedObj = retype TCppUnchanged e1 in
                let retypedIdx = retype (TCppScalar "int") e2 in
                ( CppArray (ArrayRawPointer (retypedObj, retypedIdx)),
                  cpp_type_of expr.etype )
            | false -> (
                let retypedObj = retype TCppDynamic e1 in
                let retypedIdx = retype (TCppScalar "int") e2 in
                match retypedObj.cpptype with
                | TCppScalarArray scalar ->
                    ( CppArray (ArrayTyped (retypedObj, retypedIdx, scalar)),
                      scalar )
                | TCppPointer (_, elem) ->
                    (CppArray (ArrayPointer (retypedObj, retypedIdx)), elem)
                | TCppRawPointer (_, elem) ->
                    (CppArray (ArrayRawPointer (retypedObj, retypedIdx)), elem)
                | TCppObjectArray TCppDynamic ->
                    ( CppArray
                        (ArrayObject (retypedObj, retypedIdx, TCppDynamic)),
                      TCppDynamic )
                | TCppObjectArray elem ->
                    (CppArray (ArrayObject (retypedObj, retypedIdx, elem)), elem)
                | TCppInst (({ cl_array_access = Some _ } as klass), _) ->
                    ( CppArray (ArrayImplements (klass, retypedObj, retypedIdx)),
                      cpp_type_of expr.etype )
                | TCppDynamicArray ->
                    ( CppArray (ArrayVirtual (retypedObj, retypedIdx)),
                      TCppDynamic )
                | _ ->
                    ( CppArray (ArrayDynamic (retypedObj, retypedIdx)),
                      TCppDynamic ))
          in
          let returnType = cpp_type_of expr.etype in
          if cpp_can_static_cast elemType returnType then
            ( CppCastStatic (mk_cppexpr arrayExpr returnType, returnType),
              returnType )
          else (arrayExpr, elemType)
      | TTypeExpr module_type ->
          (* If we try and use the coreType / runtimeValue cpp.Int64 abstract with Class<T> then we get a class decl of the abstract *)
          (* as that abstract has functions in its declaration *)
          (* Intercept it and replace it with the path of the actual int64 type so the generated cpp is correct *)
          let path =
            match module_type with
            | TClassDecl { cl_path = [ "cpp"; "_Int64" ], "Int64_Impl_" } ->
                ([ "cpp" ], "Int64")
            | _ -> t_path module_type
          in
          (CppClassOf (path, is_native_gen_module module_type), TCppClass)
      | TBinop (op, left, right) -> (
          let binOpType =
            match op with
            | OpDiv -> TCppScalar "Float"
            | OpBoolAnd | OpBoolOr -> TCppScalar "bool"
            | OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr -> TCppScalar "int"
            | OpAssign -> (retype TCppUnchanged left).cpptype
            | OpMult | OpSub -> cpp_type_of expr.etype
            | _ -> TCppUnchanged
          in
          let e1 = retype binOpType left in
          let e2 = retype binOpType right in

          let complex =
            is_complex_compare e1.cpptype || is_complex_compare e2.cpptype
          in
          let pointer =
            is_pointer_compare e1.cpptype || is_pointer_compare e2.cpptype
          in
          let instance =
            is_instance_compare e1.cpptype || is_instance_compare e2.cpptype
          in
          let e1_null = e1.cpptype = TCppNull in
          let e2_null = e2.cpptype = TCppNull in
          let reference =
            match op with
            | OpAssign ->
                let lvalue, gc = to_lvalue e1 in
                if gc then gc_stack := true;
                CppSet (lvalue, e2)
            | OpAssignOp op ->
                let lvalue, gc = to_lvalue e1 in
                if gc then gc_stack := true;
                CppModify (op, lvalue, e2)
            | OpEq when e1_null && e2_null -> CppBool true
            | OpGte when e1_null && e2_null -> CppBool true
            | OpLte when e1_null && e2_null -> CppBool true
            | OpNotEq when e1_null && e2_null -> CppBool false
            | _ when e1_null && e2_null -> CppBool false
            | OpEq when e1_null -> CppNullCompare ("IsNull", e2)
            | OpGte when e1_null -> CppNullCompare ("IsNull", e2)
            | OpLte when e1_null -> CppNullCompare ("IsNull", e2)
            | OpNotEq when e1_null -> CppNullCompare ("IsNotNull", e2)
            | OpEq when e2_null -> CppNullCompare ("IsNull", e1)
            | OpGte when e2_null -> CppNullCompare ("IsNull", e1)
            | OpLte when e2_null -> CppNullCompare ("IsNull", e1)
            | OpNotEq when e2_null -> CppNullCompare ("IsNotNull", e1)
            | OpEq when instance -> CppCompare ("IsInstanceEq", e1, e2, op)
            | OpNotEq when instance -> CppCompare ("IsInstanceNotEq", e1, e2, op)
            | OpEq when pointer -> CppCompare ("IsPointerEq", e1, e2, op)
            | OpNotEq when pointer -> CppCompare ("IsPointerNotEq", e1, e2, op)
            | OpEq when complex -> CppCompare ("IsEq", e1, e2, op)
            | OpNotEq when complex -> CppCompare ("IsNotEq", e1, e2, op)
            | OpGte when complex -> CppCompare ("IsGreaterEq", e1, e2, op)
            | OpLte when complex -> CppCompare ("IsLessEq", e1, e2, op)
            | OpGt when complex -> CppCompare ("IsGreater", e1, e2, op)
            | OpLt when complex -> CppCompare ("IsLess", e1, e2, op)
            | _ -> CppBinop (op, e1, e2)
          in
          match (op, e1.cpptype, e2.cpptype) with
          (* Variant + Variant = Variant *)
          | OpAdd, _, TCppVariant | OpAdd, TCppVariant, _ ->
              (reference, TCppVariant)
          | _, _, _ -> (reference, cpp_type_of expr.etype))
      | TUnop (op, pre, e1) ->
          let targetType =
            match op with
            | Not -> TCppScalar "bool"
            | NegBits -> TCppScalar "int"
            | _ -> cpp_type_of e1.etype
          in

          let e1 = retype targetType e1 in
          let reference =
            match op with
            | Increment ->
                let lvalue, gc = to_lvalue e1 in
                if gc then gc_stack := true;
                CppCrement (CppIncrement, pre, lvalue)
            | Decrement ->
                let lvalue, gc = to_lvalue e1 in
                if gc then gc_stack := true;
                CppCrement (CppDecrement, pre, lvalue)
            | Neg -> CppUnop (CppNeg, e1)
            | Not -> CppUnop (CppNot, e1)
            | NegBits -> CppUnop (CppNegBits, e1)
            | Spread -> die ~p:expr.epos "Unexpected spread operator" __LOC__
          in
          (reference, cpp_type_of expr.etype)
      | TFor (v, init, block) ->
          let old_declarations = Hashtbl.copy !declarations in
          Hashtbl.add !declarations v.v_name ();
          let init = retype (cpp_type_of v.v_type) init in
          let block = retype TCppVoid (mk_block block) in
          declarations := old_declarations;
          (CppFor (v, init, block), TCppVoid)
      | TWhile (e1, e2, flag) ->
          let condition = retype (TCppScalar "bool") e1 in
          let close = begin_loop () in
          let block = retype TCppVoid (mk_block e2) in
          (CppWhile (condition, block, flag, close ()), TCppVoid)
      | TArrayDecl el ->
          let retypedEls = List.map (retype TCppDynamic) el in
          (CppArrayDecl retypedEls, cpp_type_of expr.etype)
      | TBlock expr_list ->
          let inject = !injection in
          injection := false;
          if return_type <> TCppVoid && not forCppia then
            print_endline
              ("Value from a block not handled " ^ expr.epos.pfile ^ " "
              ^ string_of_int (Lexer.get_error_line expr.epos));

          let old_declarations = Hashtbl.copy !declarations in
          let old_closures = !rev_closures in
          rev_closures := [];
          let local_closures = ref [] in
          let remaining = ref (List.length expr_list) in
          let cppExprs =
            List.map
              (fun expr ->
                let targetType =
                  if inject && !remaining = 1 then cpp_type_of expr.etype
                  else TCppVoid
                in
                decr remaining;
                let result = retype targetType expr in
                local_closures := !rev_closures @ !local_closures;
                rev_closures := [];
                result)
              expr_list
          in
          declarations := old_declarations;
          rev_closures := old_closures;

          (CppBlock (cppExprs, List.rev !local_closures, !gc_stack), TCppVoid)
      | TObjectDecl
          [
            (("fileName", _, _), { eexpr = TConst (TString file) });
            (("lineNumber", _, _), { eexpr = TConst (TInt line) });
            (("className", _, _), { eexpr = TConst (TString class_name) });
            (("methodName", _, _), { eexpr = TConst (TString meth) });
          ] ->
          (CppPosition (file, line, class_name, meth), TCppDynamic)
      | TObjectDecl el -> (
          let retypedEls =
            List.map (fun ((v, _, _), e) -> (v, retype TCppDynamic e)) el
          in
          match return_type with
          | TCppVoid -> (CppObjectDecl (retypedEls, false), TCppVoid)
          | _ -> (CppObjectDecl (retypedEls, false), TCppDynamic))
      | TVar (v, eo) ->
          let varType = cpp_type_of v.v_type in
          let init =
            match eo with None -> None | Some e -> Some (retype varType e)
          in
          Hashtbl.add !declarations v.v_name ();
          (CppVarDecl (v, init), varType)
      | TIf (ec, e1, e2) ->
          let ec = retype (TCppScalar "bool") ec in
          let blockify =
            if return_type != TCppVoid then fun e -> e else mk_block
          in
          let e1 = retype return_type (blockify e1) in
          let e2 =
            match e2 with
            | None -> None
            | Some e -> Some (retype return_type (blockify e))
          in
          ( CppIf (ec, e1, e2),
            if return_type = TCppVoid then TCppVoid else cpp_type_of expr.etype
          )
      (* Switch internal return - wrap whole thing in block  *)
      | TSwitch
          {
            switch_subject = condition;
            switch_cases = cases;
            switch_default = def;
          } -> (
          if return_type <> TCppVoid then
            abort "Value from a switch not handled" expr.epos;

          let conditionType = cpp_type_of condition.etype in
          let condition = retype conditionType condition in
          let cppDef =
            match def with
            | None -> None
            | Some e -> Some (retype TCppVoid (mk_block e))
          in
          if forCppia then
            let cases =
              List.map
                (fun { case_patterns = el; case_expr = e2 } ->
                  let cppBlock = retype TCppVoid (mk_block e2) in
                  (List.map (retype conditionType) el, cppBlock))
                cases
            in
            (CppSwitch (condition, conditionType, cases, cppDef, -1), TCppVoid)
          else
            try
              (match conditionType with
              | TCppScalar "int" | TCppScalar "bool" -> ()
              | _ -> raise Not_found);
              let cases =
                List.map
                  (fun { case_patterns = el; case_expr = e2 } ->
                    (List.map const_int_of el, retype TCppVoid (mk_block e2)))
                  cases
              in
              (CppIntSwitch (condition, cases, cppDef), TCppVoid)
            with Not_found ->
              let label = alloc_file_id () in
              (* do something better maybe ... *)
              let cases =
                List.map
                  (fun { case_patterns = el; case_expr = e2 } ->
                    let cppBlock = retype TCppVoid (mk_block e2) in
                    let gotoExpr =
                      {
                        cppexpr = CppGoto label;
                        cpptype = TCppVoid;
                        cpppos = e2.epos;
                      }
                    in
                    let cppBlock = cpp_append_block cppBlock gotoExpr in
                    (List.map (retype conditionType) el, cppBlock))
                  cases
              in
              ( CppSwitch (condition, conditionType, cases, cppDef, label),
                TCppVoid ))
      | TTry (try_block, catches) ->
          (* TTry internal return - wrap whole thing in block ? *)
          if return_type <> TCppVoid then
            abort "Value from a try-block not handled" expr.epos;
          let cppBlock = retype TCppVoid try_block in
          let cppCatches =
            List.map
              (fun (tvar, catch_block) ->
                let old_declarations = Hashtbl.copy !declarations in
                Hashtbl.add !declarations tvar.v_name ();
                let cppCatchBlock = retype TCppVoid catch_block in
                declarations := old_declarations;
                (tvar, cppCatchBlock))
              catches
          in
          (CppTry (cppBlock, cppCatches), TCppVoid)
      | TReturn eo ->
          ( CppReturn
              (match eo with
              | None -> None
              | Some e -> Some (retype !function_return_type e)),
            TCppVoid )
      | TCast (base, None) -> (
          (* Use auto-cast rules *)
          let return_type = cpp_type_of expr.etype in
          let baseCpp = retype return_type base in
          let baseStr = tcpp_to_string baseCpp.cpptype in
          let returnStr = tcpp_to_string return_type in
          if baseStr = returnStr then
            (baseCpp.cppexpr, baseCpp.cpptype (* nothing to do *))
          else
            match return_type with
            | TCppObjC k -> (CppCastObjC (baseCpp, k), return_type)
            | TCppPointer (_, _)
            | TCppRawPointer (_, _)
            | TCppStar _ | TCppInst _ ->
                (CppCast (baseCpp, return_type), return_type)
            | TCppString -> (CppCastScalar (baseCpp, "::String"), return_type)
            | TCppCode t when baseStr <> tcpp_to_string t ->
                (CppCast (baseCpp, t), t)
            | TCppNativePointer klass -> (CppCastNative baseCpp, return_type)
            | TCppObjCBlock (args, ret) ->
                (CppCastObjCBlock (baseCpp, args, ret), return_type)
            | TCppProtocol p -> (CppCastProtocol (baseCpp, p), return_type)
            | TCppDynamic when baseCpp.cpptype = TCppClass ->
                (CppCast (baseCpp, TCppDynamic), TCppDynamic)
            | _ -> (baseCpp.cppexpr, baseCpp.cpptype (* use autocasting rules *))
          )
      | TCast (base, Some t) -> (
          let baseCpp = retype (cpp_type_of base.etype) base in
          let baseStr = tcpp_to_string baseCpp.cpptype in
          let default_return_type =
            if return_type = TCppUnchanged then cpp_type_of expr.etype
            else return_type
          in
          let return_type =
            cpp_type_from_path (t_path t) [] (fun () -> default_return_type)
          in
          let returnStr = tcpp_to_string return_type in

          if baseStr = returnStr then
            (baseCpp.cppexpr, baseCpp.cpptype (* nothing to do *))
          else
            match return_type with
            | TCppNativePointer klass -> (CppCastNative baseCpp, return_type)
            | TCppVoid ->
                (CppTCast (baseCpp, cpp_type_of expr.etype), return_type)
            | TCppDynamic -> (baseCpp.cppexpr, baseCpp.cpptype)
            | _ -> (CppTCast (baseCpp, return_type), return_type))
    in
    let cppExpr = mk_cppexpr retypedExpr retypedType in

    (* Autocast rules... *)
    if return_type = TCppVoid then mk_cppexpr retypedExpr TCppVoid
    else if return_type = TCppVarArg then
      match cpp_variant_type_of cppExpr.cpptype with
      | TCppVoidStar | TCppScalar _ -> cppExpr
      | TCppString ->
          mk_cppexpr
            (CppVar (VarInternal (cppExpr, ".", "raw_ptr()")))
            (TCppPointer ("ConstPointer", TCppScalar "char"))
      | TCppDynamic -> mk_cppexpr (CppCastNative cppExpr) TCppVoidStar
      | _ ->
          let toDynamic =
            mk_cppexpr (CppCast (cppExpr, TCppDynamic)) TCppDynamic
          in
          mk_cppexpr (CppCastNative toDynamic) TCppVoidStar
    else if
      cppExpr.cpptype = TCppVariant
      || cppExpr.cpptype = TCppDynamic
      || cppExpr.cpptype == TCppObject
    then
      match return_type with
      | TCppUnchanged -> cppExpr
      | TCppInst (t, _) when Meta.has Meta.StructAccess t.cl_meta ->
          let structType = TCppStruct (TCppInst (t, [])) in
          let structCast =
            mk_cppexpr (CppCast (cppExpr, structType)) structType
          in
          mk_cppexpr (CppCast (structCast, TCppInst (t, []))) (TCppInst (t, []))
      | TCppObjectArray _ | TCppScalarArray _ | TCppNativePointer _
      | TCppDynamicArray | TCppObjectPtr | TCppVarArg | TCppInst _ ->
          mk_cppexpr (CppCast (cppExpr, return_type)) return_type
      | TCppObjC k -> mk_cppexpr (CppCastObjC (cppExpr, k)) return_type
      | TCppObjCBlock (ret, args) ->
          mk_cppexpr (CppCastObjCBlock (cppExpr, ret, args)) return_type
      | TCppScalar scalar ->
          mk_cppexpr (CppCastScalar (cppExpr, scalar)) return_type
      | TCppString ->
          mk_cppexpr (CppCastScalar (cppExpr, "::String")) return_type
      | TCppInterface _ when cppExpr.cpptype = TCppVariant ->
          mk_cppexpr (CppCastVariant cppExpr) return_type
      | TCppDynamic when cppExpr.cpptype = TCppVariant ->
          mk_cppexpr (CppCastVariant cppExpr) return_type
      | TCppStar (t, const) ->
          let ptrType =
            TCppPointer ((if const then "ConstPointer" else "Pointer"), t)
          in
          let ptrCast = mk_cppexpr (CppCast (cppExpr, ptrType)) ptrType in
          mk_cppexpr
            (CppCast (ptrCast, TCppStar (t, const)))
            (TCppStar (t, const))
      | _ -> cppExpr
    else
      match (cppExpr.cpptype, return_type) with
      | _, TCppUnchanged -> cppExpr
      (*
        Using the 'typedef hack', where we use typedef X<T> = T, allows the
        haxe compiler to use these types interchangeably. We then work
        out the correct way to convert between them when one is expected, but another provided.

        TCppFunction: these do not really interact with the haxe function type, T
        Since they are implemented with cpp::Function, conversion to/from Dynamic should happen automatically
          CallableData<T> = T;
          FunctionData<T,ABI> = T;

        TCppObjCBlock can move in and out of Dyanmic
          ObjcBlock<T> = T;

        TCppProtocol can move in and out of Dyanmic, via delegate creation
          Protocol<T /*:interface*/ > = T;

        Explicitly wrapped type - already interacts well with Dynamic and T
          Struct<T> = T;

        TCppStar, TCppStruct, TCppReference - for interacting with native code
          Star<T> = T;
          ConstStar<T> = T;
          Reference<T> = T;
          T may be an extern class, with @:structAccess - in which case
            Dynamic interaction must be handled explicitly
        These types, plus Dynamic can be used interchangeably by haxe
        Derived/inherited types may also be mixed in
      *)
      | TCppAutoCast, _ | TCppObjC _, TCppDynamic | TCppObjCBlock _, TCppDynamic
        ->
          mk_cppexpr (CppCast (cppExpr, return_type)) return_type
      (* Infer type from right-hand-side for pointer or reference to Dynamic *)
      | TCppReference TCppDynamic, TCppReference _ -> cppExpr
      | TCppReference TCppDynamic, t -> mk_cppexpr retypedExpr (TCppReference t)
      | TCppStar (TCppDynamic, _), TCppStar (_, _) -> cppExpr
      | TCppStar (TCppDynamic, const), t ->
          mk_cppexpr retypedExpr (TCppStar (t, const))
      | TCppStar (t, const), TCppDynamic ->
          let ptrType =
            TCppPointer ((if const then "ConstPointer" else "Pointer"), t)
          in
          let ptrCast = mk_cppexpr (CppCast (cppExpr, ptrType)) ptrType in
          mk_cppexpr (CppCast (ptrCast, TCppDynamic)) TCppDynamic
      | TCppStar (t, const), TCppReference _
      | TCppStar (t, const), TCppInst _
      | TCppStar (t, const), TCppStruct _ ->
          mk_cppexpr (CppDereference cppExpr) return_type
      | TCppInst (t, _), TCppStar _
        when is_native_class t
             &&
             match cppExpr.cppexpr with
             | CppCall (FuncNew _, _) -> true
             | _ -> false ->
          mk_cppexpr (CppNewNative cppExpr) return_type
      | TCppInst _, TCppStar (p, const) | TCppStruct _, TCppStar (p, const) ->
          mk_cppexpr (CppAddressOf cppExpr) return_type
      | TCppObjectPtr, TCppObjectPtr -> cppExpr
      | TCppObjectPtr, _ ->
          mk_cppexpr (CppCast (cppExpr, TCppDynamic)) TCppDynamic
      | TCppProtocol _, TCppProtocol _ -> cppExpr
      | t, TCppProtocol protocol ->
          mk_cppexpr (CppCastProtocol (cppExpr, protocol)) return_type
      | TCppInst (t, _), TCppDynamic when Meta.has Meta.StructAccess t.cl_meta
        ->
          let structType = TCppStruct (TCppInst (t, [])) in
          let structCast =
            mk_cppexpr (CppCast (cppExpr, structType)) structType
          in
          mk_cppexpr (CppCast (structCast, TCppDynamic)) TCppDynamic
      | _, TCppObjectPtr ->
          mk_cppexpr (CppCast (cppExpr, TCppObjectPtr)) TCppObjectPtr
      | TCppDynamicArray, TCppScalarArray _
      | TCppDynamicArray, TCppObjectArray _
      | TCppScalarArray _, TCppDynamicArray
      | TCppObjectArray _, TCppDynamicArray
        when forCppia ->
          mk_cppexpr (CppCast (cppExpr, return_type)) return_type
      | TCppScalar from, TCppScalar too when from <> too ->
          mk_cppexpr (CppCastScalar (cppExpr, too)) return_type
      | _ -> cppExpr
  in
  retype request_type expression_tree
