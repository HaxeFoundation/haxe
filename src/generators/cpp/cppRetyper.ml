open Ast
open Type
open Error
open Globals
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

type retyper_ctx = {
  closure_id : int;
  closures : tcpp_closure list;
  injection : bool;
  declarations : unit StringMap.t;
  undeclared : tvar StringMap.t;
  uses_this : tcppthis option;
  this_real : tcppthis;
  gc_stack : bool;
  function_return_type : tcpp;
  goto_id : int;
  loop_stack : (int * bool) list;
}

let expression ctx request_type function_args function_type expression_tree forInjection =
  let forCppia = Gctx.defined ctx.ctx_common Define.Cppia in
  let initial_ctx = {
    closures = [];
    closure_id = 0;
    injection = forInjection;
    undeclared = StringMap.empty;
    declarations = function_args |> List.map (fun a -> a.v_name, ()) |> string_map_of_list |> StringMap.add "__trace" (); (* '__trace' is at the top-level *)
    uses_this = None;
    this_real = if ctx.ctx_real_this_ptr then ThisReal else ThisDynamic;
    gc_stack = false;
    function_return_type = cpp_type_of function_type;
    goto_id = 0;
    loop_stack = [];
  } in

  (* Helper functions *)

  let alloc_file_id retyper_ctx =
    ({ retyper_ctx with goto_id = retyper_ctx.goto_id + 1 }, retyper_ctx.goto_id + 1)
  in

  let begin_loop retyper_ctx =
    let new_ctx = {
      retyper_ctx with
        goto_id    = retyper_ctx.goto_id + 1;
        loop_stack = (retyper_ctx.goto_id + 1, false) :: retyper_ctx.loop_stack
    } in
    let resolver =
      fun retyper_ctx ->
        match retyper_ctx.loop_stack with
        | (label_id, used) :: tl ->
          { retyper_ctx with loop_stack = tl }, if used then label_id else -1
        | [] ->
          abort "Invalid inernal loop handling" expression_tree.epos
    in

    new_ctx, resolver
  in

  let cpp_const_type retyper_ctx cval =
    match cval with
    | TInt i -> (retyper_ctx, CppInt i, TCppScalar "int")
    | TBool b -> (retyper_ctx, CppBool b, TCppScalar "bool")
    | TFloat f -> (retyper_ctx, CppFloat (Texpr.replace_separators f ""), TCppScalar "Float")
    | TString s -> (retyper_ctx, CppString s, TCppString)
    | _ ->
        (* TNull, TThis & TSuper should already be handled *)
        (retyper_ctx, CppNull, TCppNull)
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
  let rec retype retyper_ctx return_type expr =
    let cpp_type_of t = cpp_type_of t in
    let mk_cppexpr newExpr newType =
      { cppexpr = newExpr; cpptype = newType; cpppos = expr.epos }
    in
    let retype_function_args retyper_ctx args arg_types =
      let folder (acc_ctx, acc_exprs) arg t =
        let new_ctx, new_expr = retype acc_ctx t arg in
        new_ctx, new_expr :: acc_exprs
      in

      arg_types
        |> ExtList.List.fold_left2 folder (retyper_ctx, []) args
        |> fun (ctx, acc) -> (ctx, List.rev acc)
    in

    let retyper_ctx, retypedExpr, retypedType =
      match expr.eexpr with
      | TEnumParameter (enumObj, enumField, enumIndex) ->
          let retyper_ctx, retypedObj = retype retyper_ctx TCppDynamic enumObj in
          ( retyper_ctx,
            CppEnumParameter (retypedObj, enumField, enumIndex),
            cpp_cast_variant_type_of
              (cpp_type_of (get_nth_type enumField enumIndex)) )
      | TEnumIndex enumObj ->
          let retyper_ctx, retypedObj = retype retyper_ctx TCppDynamic enumObj in
          (retyper_ctx, CppEnumIndex retypedObj, TCppScalar "int")
      | TConst TThis ->
          let retyper_ctx = { retyper_ctx with uses_this = Some retyper_ctx.this_real } in
          ( retyper_ctx,
            CppThis retyper_ctx.this_real,
            if retyper_ctx.this_real = ThisDynamic then TCppDynamic
            else cpp_type_of expr.etype )
      | TConst TSuper ->
        let retyper_ctx = { retyper_ctx with uses_this = Some retyper_ctx.this_real } in
          ( retyper_ctx,
            CppSuper retyper_ctx.this_real,
            if retyper_ctx.this_real = ThisDynamic then TCppDynamic
            else cpp_type_of expr.etype )
      | TConst TNull when is_objc_type expr.etype -> (retyper_ctx, CppNil, TCppNull)
      | TConst x -> cpp_const_type retyper_ctx x
      | TIdent "__global__" ->
          (* functions/vars will appear to be members of the virtual global object *)
          (retyper_ctx, CppClassOf (([], ""), false), TCppGlobal)
      | TLocal tvar ->
          let name = tvar.v_name in
          if StringMap.mem name retyper_ctx.declarations then
            (retyper_ctx, CppVar (VarLocal tvar), cpp_type_of tvar.v_type)
          else (
            let new_ctx = { retyper_ctx with undeclared = StringMap.add name tvar retyper_ctx.undeclared } in
            if has_var_flag tvar VCaptured then
              (new_ctx, CppVar (VarClosure tvar), cpp_type_of tvar.v_type)
            else
              (new_ctx, CppExtern (name, false), cpp_type_of tvar.v_type))
      | TIdent name -> (retyper_ctx, CppExtern (name, false), return_type)
      | TBreak -> (
          if forCppia then
            (retyper_ctx, CppBreak, TCppVoid)
          else
            match retyper_ctx.loop_stack with
            | [] ->
              (retyper_ctx, CppBreak, TCppVoid)
            | (label_id, used) :: tl ->
              ({ retyper_ctx with loop_stack = (label_id, true) :: tl }, CppGoto label_id, TCppVoid))
      | TContinue -> (retyper_ctx, CppContinue, TCppVoid)
      | TThrow e1 ->
        let retyper_ctx, retyped_expr = retype retyper_ctx TCppDynamic e1 in
        (retyper_ctx, CppThrow retyped_expr, TCppVoid)
      | TMeta ((Meta.Fixed, _, _), e) -> (
          let retyper_ctx, cppType = retype retyper_ctx return_type e in
          match cppType.cppexpr with
          | CppObjectDecl (def, false) ->
            (retyper_ctx, CppObjectDecl (def, true), cppType.cpptype)
          | _ ->
            (retyper_ctx, cppType.cppexpr, cppType.cpptype))
      | TMeta (_, e) | TParenthesis e ->
          let retyper_ctx, cppType = retype retyper_ctx return_type e in
          (retyper_ctx, cppType.cppexpr, cppType.cpptype)
      | TField (obj, field) -> (
          match field with
          | FInstance (clazz, params, member)
          | FClosure (Some (clazz, params), member) -> (
            let funcReturn = cpp_member_return_type member in
            let clazzType = cpp_instance_type clazz params in
            let retyper_ctx, retypedObj = retype retyper_ctx clazzType obj in
            let exprType = cpp_type_of member.cf_type in
            let is_objc = is_cpp_objc_type retypedObj.cpptype in

            if retypedObj.cpptype = TCppNull then
              (retyper_ctx, CppNullAccess, TCppDynamic)
            else if retypedObj.cpptype = TCppDynamic && not (has_class_flag clazz CInterface) then
              if is_internal_member member.cf_name then
                ( retyper_ctx,
                  CppFunction (FuncInstance (retypedObj, InstPtr, member), funcReturn),
                  exprType )
              else
                (retyper_ctx, CppDynamicField (retypedObj, member.cf_name), TCppVariant)
            else if cpp_is_struct_access retypedObj.cpptype then
              match retypedObj.cppexpr with
              | CppThis ThisReal ->
                (retyper_ctx, CppVar (VarThis (member, retypedObj.cpptype)), exprType)
              | CppSuper this ->
                ( retyper_ctx,
                  CppFunction ( FuncSuper (this, retypedObj.cpptype, member), funcReturn ),
                  exprType )
              | _ ->
                if is_var_field member then
                  ( retyper_ctx,
                    CppVar (VarInstance (retypedObj, member, tcpp_to_string clazzType, ".")),
                    exprType )
                else
                  ( retyper_ctx,
                    CppFunction ( FuncInstance (retypedObj, InstStruct, member), funcReturn ),
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
                (retyper_ctx, CppVar (VarThis (member, retypedObj.cpptype)), exprType)
              | _ -> (
                match (retypedObj.cpptype, member.cf_name) with
                (* Special variable remapping ... *)
                | TCppDynamicArray, "length" when not forCppia ->
                  ( retyper_ctx,
                    CppCall (FuncInternal (retypedObj, "get_length", "->"), []),
                    exprType )
                | TCppInterface _, _ | TCppDynamic, _ ->
                  ( retyper_ctx,
                    CppDynamicField (retypedObj, member.cf_name),
                    TCppVariant )
                | TCppObjC _, _ ->
                  ( retyper_ctx,
                    CppVar (VarInstance ( retypedObj, member, tcpp_to_string clazzType, "." )),
                    exprType )
                | _ ->
                  let operator =
                    if cpp_is_struct_access retypedObj.cpptype || retypedObj.cpptype = TCppString then
                      "."
                    else
                      "->"
                  in
                  ( retyper_ctx,
                    CppVar (VarInstance ( retypedObj, member, tcpp_to_string clazzType, operator )),
                    exprType ))
            else if has_class_flag clazz CInterface && not is_objc (* Use instance call for objc interfaces *) then
              ( retyper_ctx,
                CppFunction (FuncInterface (retypedObj, clazz, member), funcReturn),
                exprType )
            else
              let isArrayObj =
                match retypedObj.cpptype with
                | TCppDynamicArray | TCppObjectArray _ | TCppScalarArray _ ->
                  true
                | _ ->
                  false
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
                ( retyper_ctx,
                  CppFunction (FuncThis (member, retypedObj.cpptype), funcReturn),
                  exprType )
              | CppSuper this ->
                ( retyper_ctx,
                  CppFunction ( FuncSuper (this, retypedObj.cpptype, member), funcReturn ),
                  exprType )
              | _ ->
                ( retyper_ctx,
                  CppFunction
                    ( FuncInstance
                      ( retypedObj,
                        (if is_objc then InstObjC else InstPtr),
                        member ),
                      funcReturn ),
                  exprType ))
          | FStatic (_, ({ cf_name = "nativeFromStaticFunction" } as member)) ->
            let funcReturn = cpp_member_return_type member in
            let exprType   = cpp_type_of member.cf_type in
            (retyper_ctx, CppFunction (FuncFromStaticFunction, funcReturn), exprType)
          | FStatic (clazz, member) ->
            let funcReturn = cpp_member_return_type member in
            let exprType   = cpp_type_of member.cf_type in
            let objC       = is_objc_class clazz in
            if is_var_field member then
              (retyper_ctx, CppVar (VarStatic (clazz, objC, member)), exprType)
            else
              ( retyper_ctx,
                CppFunction (FuncStatic (clazz, objC, member), funcReturn),
                exprType )
          | FClosure (None, field) | FAnon field ->
            let retyper_ctx, obj = retype retyper_ctx TCppDynamic obj in
            let fieldName = field.cf_name in
            if obj.cpptype = TCppGlobal then
              (retyper_ctx, CppExtern (fieldName, true), cpp_type_of expr.etype)
            else if obj.cpptype = TCppNull then (retyper_ctx, CppNullAccess, TCppDynamic)
            else if is_internal_member fieldName then
              let cppType = cpp_return_type expr.etype in
              if obj.cpptype = TCppString then
                ( retyper_ctx,
                  CppFunction (FuncInternal (obj, fieldName, "."), cppType),
                  cppType )
              else
                ( retyper_ctx,
                  CppFunction (FuncInternal (obj, fieldName, "->"), cppType),
                  cppType )
            else (retyper_ctx, CppDynamicField (obj, field.cf_name), TCppVariant)
          | FDynamic fieldName ->
              let retyper_ctx, obj = retype retyper_ctx TCppDynamic obj in
              if obj.cpptype = TCppNull then (retyper_ctx, CppNullAccess, TCppDynamic)
              else if fieldName = "cca" && obj.cpptype = TCppString then
                ( retyper_ctx,
                  CppFunction (FuncInternal (obj, "cca", "."), TCppScalar "int"),
                  TCppDynamic )
              else if fieldName = "__s" && obj.cpptype = TCppString then
                ( retyper_ctx,
                  CppVar (VarInternal (obj, ".", "utf8_str()")),
                  TCppRawPointer ("const ", TCppScalar "char") )
              else if fieldName = "__Index" then
                (retyper_ctx, CppEnumIndex obj, TCppScalar "int")
              else if is_internal_member fieldName || cpp_is_real_array obj then
                let cppType = cpp_return_type expr.etype in
                if obj.cpptype = TCppString then
                  ( retyper_ctx,
                    CppFunction (FuncInternal (obj, fieldName, "."), cppType),
                    cppType )
                else
                  ( retyper_ctx,
                    CppFunction (FuncInternal (obj, fieldName, "->"), cppType),
                    cppType )
              else if obj.cpptype = TCppGlobal then
                (retyper_ctx, CppExtern (fieldName, true), cpp_type_of expr.etype)
              else if obj.cpptype = TCppClass then
                match obj.cppexpr with
                | CppClassOf (path, _) ->
                  ( retyper_ctx,
                    CppExtern ( join_class_path_remap path "::" ^ "_obj::" ^ fieldName, true ),
                    cpp_type_of expr.etype )
                | _ ->
                  ( retyper_ctx,
                    CppVar (VarInternal (obj, "->", fieldName)),
                    cpp_type_of expr.etype )
              else (retyper_ctx, CppDynamicField (obj, fieldName), TCppVariant)
          | FEnum (enum, enum_field) ->
            (retyper_ctx, CppEnumField (enum, enum_field), TCppEnum enum))
      | TCall ({ eexpr = TIdent "__cpp__" }, arg_list) ->
        let retyper_ctx, cppExpr =
          match arg_list with
          | [ { eexpr = TConst (TString code) } ] -> retyper_ctx, CppCode (code, [])
          | { eexpr = TConst (TString code) } :: remaining ->
            let folder (cur_ctx, args) arg =
              let new_ctx, new_arg = retype cur_ctx (TCppCode (cpp_type_of arg.etype)) arg in
              new_ctx, new_arg :: args
            in
            let retyper_ctx, retypedArgs = List.fold_left folder (retyper_ctx, []) remaining in
            retyper_ctx, CppCode (code, List.rev retypedArgs)
          | _ -> abort "__cpp__'s first argument must be a string" expr.epos
        in
        (retyper_ctx, cppExpr, TCppCode (cpp_type_of expr.etype))
      | TCall (func, args) -> (
          let retyper_ctx, retypedFunc = retype retyper_ctx TCppUnchanged func in
          match retypedFunc.cpptype with
          | TCppNull -> (retyper_ctx, CppNullAccess, TCppDynamic)
          | TCppFunction (argTypes, retType, _) ->
            let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args argTypes in
            (retyper_ctx, CppCall (FuncExpression retypedFunc, retypedArgs), retType)
          | TCppObjCBlock (argTypes, retType) ->
            let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args argTypes in
            (retyper_ctx, CppCall (FuncExpression retypedFunc, retypedArgs), retType)
          | _ -> (
            let cppType = cpp_type_of expr.etype in
            match retypedFunc.cppexpr with
            | CppFunction (FuncFromStaticFunction, returnType) -> (
              let arg_types = List.map (fun _ -> TCppDynamic) args in
              let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
              match retypedArgs with
              | [
                {
                  cppexpr =
                    CppFunction
                      (FuncStatic (clazz, false, member), funcReturn);
                };
              ] ->
                  (retyper_ctx, CppFunctionAddress (clazz, member), funcReturn)
              | _ ->
                  abort
                    "cpp.Function.fromStaticFunction must be called on \
                      static function"
                    expr.epos)
            | CppEnumIndex _ ->
                (* Not actually a TCall...*)
                (retyper_ctx, retypedFunc.cppexpr, retypedFunc.cpptype)
            | CppFunction (FuncInstance (obj, InstPtr, member), _)
              when (not forCppia) && return_type = TCppVoid && is_array_splice_call obj member ->
                let arg_types = List.map (fun _ -> TCppDynamic) args in
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                ( retyper_ctx,
                  CppCall ( FuncInstance (obj, InstPtr, { member with cf_name = "removeRange" }), retypedArgs ),
                  TCppVoid )
            | CppFunction (FuncInstance (obj, InstPtr, member), _)
              when is_array_concat_call obj member ->
                let arg_types = List.map (fun _ -> obj.cpptype) args in
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                ( retyper_ctx,
                  CppCall (FuncInstance (obj, InstPtr, member), retypedArgs),
                  return_type )
            | CppFunction (FuncStatic (obj, false, member), _)
              when member.cf_name = "::hx::AddressOf" ->
                let retyper_ctx, arg = retype retyper_ctx TCppUnchanged (List.hd args) in
                let rawType = match arg.cpptype with TCppReference x -> x | x -> x in
                (retyper_ctx, CppAddressOf arg, TCppRawPointer ("", rawType))
            | CppFunction (FuncStatic (obj, false, member), _)
              when member.cf_name = "::hx::StarOf" ->
                let retyper_ctx, arg = retype retyper_ctx TCppUnchanged (List.hd args) in
                let rawType = match arg.cpptype with TCppReference x -> x | x -> x in
                (retyper_ctx, CppAddressOf arg, TCppStar (rawType, false))
            | CppFunction (FuncStatic (obj, false, member), _)
              when member.cf_name = "::hx::Dereference" ->
                let retyper_ctx, arg = retype retyper_ctx TCppUnchanged (List.hd args) in
                let rawType = match arg.cpptype with TCppStar (x, _) -> x | x -> x in
                (retyper_ctx, CppDereference arg, TCppReference rawType)
            | CppFunction (FuncStatic (obj, false, member), _)
              when member.cf_name = "_hx_create_array_length" -> (
                let arg_types = List.map (fun _ -> TCppDynamic) args in
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                (* gc_stack - not needed yet *)
                match return_type with
                | TCppObjectArray _ | TCppScalarArray _ ->
                  (retyper_ctx, CppCall (FuncNew return_type, retypedArgs), return_type)
                | _ ->
                  ( retyper_ctx, CppCall (FuncNew TCppDynamicArray, retypedArgs), return_type ))
            | CppFunction (FuncStatic (obj, false, member), returnType)
              when cpp_is_templated_call ctx member -> (
                let arg_types = List.map (fun _ -> TCppDynamic) args in
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                match retypedArgs with
                | { cppexpr = CppClassOf (path, native) } :: rest ->
                  ( retyper_ctx, CppCall (FuncTemplate (obj, member, path, native), rest), returnType )
                | _ ->
                  abort
                    "First parameter of template function must be a Class"
                    retypedFunc.cpppos)
            | CppFunction (FuncInstance (obj, InstPtr, member), _)
              when is_map_get_call obj member ->
                let arg_types = List.map (fun _ -> TCppDynamic) args in
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
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
                (retyper_ctx, CppCall (func, retypedArgs), cppType)
            | CppFunction (FuncInstance (obj, InstPtr, member), _)
              when forCppia && is_map_set_call obj member ->
                let arg_types = List.map (fun _ -> TCppDynamic) args in
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
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
                let func = FuncInstance (obj, InstPtr, { member with cf_name = fname }) in
                (retyper_ctx, CppCall (func, retypedArgs), cppType)
            | CppFunction
                ((FuncInstance (obj, InstPtr, member) as func), returnType)
              when cpp_can_static_cast returnType cppType ->
                let arg_types = List.map (fun _ -> TCppDynamic) args in
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                let call =
                  mk_cppexpr (CppCall (func, retypedArgs)) returnType
                in
                (retyper_ctx, CppCastStatic (call, cppType), cppType)
            (*
              let error_printer file line = Printf.sprintf "%s:%d:" file line in
              let epos = Lexer.get_error_pos error_printer expr.epos in
              print_endline ( "fixed override " ^ member.cf_name ^ " @ " ^  epos ^ " " ^ (tcpp_to_string returnType) ^ "->" ^ (ctx_type_string ctx expr.etype) );
              CppCall(func,retypedArgs), returnType
            *)
            (* Other functions ... *)
            | CppFunction ( (FuncInstance (_, InstStruct, { cf_type = TFun (arg_types, _) }) as func), return_type ) ->
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
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                (retyper_ctx, CppCall (func, retypedArgs), return_type)
            | CppFunction ( (FuncInstance (_, _, { cf_type = TFun (arg_types, _) }) as func), returnType )
            | CppFunction ( (FuncStatic (_, _, { cf_type = TFun (arg_types, _) }) as func), returnType )
            | CppFunction ( (FuncThis ({ cf_type = TFun (arg_types, _) }, _) as func), returnType ) ->
                let arg_types =
                  List.map
                    (fun (_, opt, t) -> cpp_tfun_arg_type_of opt t)
                    arg_types
                in
                (* retype args specifically (not just CppDynamic) *)
                let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                (retyper_ctx, CppCall (func, retypedArgs), returnType)
            | CppFunction (func, returnType) ->
              let arg_types = List.map (fun _ -> TCppDynamic) args in
              let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
              (retyper_ctx, CppCall (func, retypedArgs), returnType)
            | CppEnumField (enum, field) ->
              (* TODO - proper re-typing *)
              let arg_types = List.map (fun _ -> TCppDynamic) args in
              let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
              ( retyper_ctx, CppCall (FuncEnumConstruct (enum, field), retypedArgs), cppType )
            | CppSuper _ ->
              (* TODO - proper re-typing *)
              let arg_types = List.map (fun _ -> TCppDynamic) args in
              let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
              ( retyper_ctx, CppCall (FuncSuperConstruct retypedFunc.cpptype, retypedArgs), TCppVoid )
            | CppDynamicField (expr, name) -> (
              let arg_types = List.map (fun _ -> TCppDynamic) args in
              let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
                (* Special function calls *)
                match (expr.cpptype, name) with
                | TCppGlobal, _ ->
                  (retyper_ctx, CppCall (FuncExtern (name, true), retypedArgs), cppType)
                | TCppString, _ ->
                  ( retyper_ctx,
                    CppCall (FuncInternal (expr, name, "."), retypedArgs),
                    cppType )
                | _, "__Tag" ->
                  ( retyper_ctx,
                    CppCall (FuncInternal (expr, "_hx_getTag", "->"), retypedArgs),
                    cppType )
                | _, name when is_internal_member name ->
                  ( retyper_ctx, CppCall (FuncInternal (expr, name, "->"), retypedArgs), cppType )
                | _ ->
                  (* not special *)
                  ( retyper_ctx, CppCall (FuncExpression retypedFunc, retypedArgs), TCppDynamic ))
            | CppExtern (name, isGlobal) ->
              let arg_types = List.map (fun _ -> TCppUnchanged) args in
              let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
              (retyper_ctx, CppCall (FuncExtern (name, isGlobal), retypedArgs), cppType)
            | _ ->
              let arg_types = List.map (fun _ -> TCppDynamic) args in
              let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
              ( retyper_ctx,
                CppCall (FuncExpression retypedFunc, retypedArgs),
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
        let retyper_ctx, retypedArgs = retype_function_args retyper_ctx args arg_types in
        let created_type = cpp_type_of expr.etype in
        let gc_stack =
          retyper_ctx.gc_stack || match created_type with
          | TCppInst (t, _) -> not (is_native_class t)
          | _ -> false in
        ({ retyper_ctx with gc_stack = gc_stack }, CppCall (FuncNew created_type, retypedArgs), created_type)
      | TFunction func ->
        (* TODO - this_dynamic ? *)

        let new_ctx = {
          retyper_ctx with
            declarations = func.tf_args |> List.map (fun (a, _) -> a.v_name, ()) |> string_map_of_list;
            undeclared   = StringMap.empty;
            this_real    = ThisFake;
            uses_this    = None;
            function_return_type = cpp_type_of func.tf_type;
        } in
        let new_ctx, cppExpr = retype new_ctx TCppVoid (mk_block func.tf_expr) in

        let result =
          {
            close_expr = cppExpr;
            close_id = retyper_ctx.closure_id;
            close_undeclared = new_ctx.undeclared;
            close_type = new_ctx.function_return_type;
            close_args = func.tf_args;
            close_this = new_ctx.uses_this;
          }
        in
        let folder acc (name, tvar) =
          if not (StringMap.mem name retyper_ctx.declarations) then
            StringMap.add name tvar acc
          else
            acc
          in
        let new_undeclared =
          List.fold_left
            folder
            retyper_ctx.undeclared
            (StringMap.bindings new_ctx.undeclared)
          in

        let retyper_ctx = {
          retyper_ctx with
            closure_id = retyper_ctx.closure_id + 1;
            closures   = result :: retyper_ctx.closures;
            undeclared = new_undeclared;
            uses_this  = if new_ctx.uses_this != None then Some retyper_ctx.this_real else retyper_ctx.uses_this;
        } in

        (retyper_ctx, CppClosure result, TCppDynamic)
      | TArray (e1, e2) ->
          let retyper_ctx, arrayExpr, elemType =
            match cpp_is_native_array_access (cpp_type_of e1.etype) with
            | true ->
                let retyper_ctx, retypedObj = retype retyper_ctx TCppUnchanged e1 in
                let retyper_ctx, retypedIdx = retype retyper_ctx (TCppScalar "int") e2 in
                ( retyper_ctx,
                  CppArray (ArrayRawPointer (retypedObj, retypedIdx)),
                  cpp_type_of expr.etype )
            | false -> (
                let retyper_ctx, retypedObj = retype retyper_ctx TCppDynamic e1 in
                let retyper_ctx, retypedIdx = retype retyper_ctx (TCppScalar "int") e2 in
                match retypedObj.cpptype with
                | TCppScalarArray scalar ->
                    ( retyper_ctx,
                      CppArray (ArrayTyped (retypedObj, retypedIdx, scalar)),
                      scalar )
                | TCppPointer (_, elem) ->
                    (retyper_ctx, CppArray (ArrayPointer (retypedObj, retypedIdx)), elem)
                | TCppRawPointer (_, elem) ->
                    (retyper_ctx, CppArray (ArrayRawPointer (retypedObj, retypedIdx)), elem)
                | TCppObjectArray TCppDynamic ->
                    ( retyper_ctx,
                      CppArray (ArrayObject (retypedObj, retypedIdx, TCppDynamic)),
                      TCppDynamic )
                | TCppObjectArray elem ->
                    (retyper_ctx, CppArray (ArrayObject (retypedObj, retypedIdx, elem)), elem)
                | TCppInst (({ cl_array_access = Some _ } as klass), _) ->
                    ( retyper_ctx, CppArray (ArrayImplements (klass, retypedObj, retypedIdx)),
                      cpp_type_of expr.etype )
                | TCppDynamicArray ->
                    ( retyper_ctx,
                      CppArray (ArrayVirtual (retypedObj, retypedIdx)),
                      TCppDynamic )
                | _ ->
                    ( retyper_ctx,
                      CppArray (ArrayDynamic (retypedObj, retypedIdx)),
                      TCppDynamic ))
          in
          let returnType = cpp_type_of expr.etype in
          if cpp_can_static_cast elemType returnType then
            ( retyper_ctx,
              CppCastStatic (mk_cppexpr arrayExpr returnType, returnType),
              returnType )
          else
            (retyper_ctx, arrayExpr, elemType)
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
          (retyper_ctx, CppClassOf (path, is_native_gen_module module_type), TCppClass)
      | TBinop (op, left, right) -> (
          let retyper_ctx, binOpType =
            match op with
            | OpDiv -> retyper_ctx, TCppScalar "Float"
            | OpBoolAnd | OpBoolOr -> retyper_ctx, TCppScalar "bool"
            | OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr -> retyper_ctx, TCppScalar "int"
            | OpAssign ->
              let retyper_ctx, retyped_expr = (retype retyper_ctx TCppUnchanged left) in
              (retyper_ctx, retyped_expr.cpptype)
            | OpMult | OpSub ->
              retyper_ctx, cpp_type_of expr.etype
            | _ -> retyper_ctx, TCppUnchanged
          in
          let retyper_ctx, e1 = retype retyper_ctx binOpType left in
          let retyper_ctx, e2 = retype retyper_ctx binOpType right in

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
          let retyper_ctx, reference =
            match op with
            | OpAssign ->
                let lvalue, gc = to_lvalue e1 in
                let new_ctx = if gc then { retyper_ctx with gc_stack = true } else retyper_ctx in
                new_ctx, CppSet (lvalue, e2)
            | OpAssignOp op ->
                let lvalue, gc = to_lvalue e1 in
                let new_ctx = if gc then { retyper_ctx with gc_stack = true } else retyper_ctx in
                new_ctx, CppModify (op, lvalue, e2)
            | OpEq when e1_null && e2_null -> retyper_ctx, CppBool true
            | OpGte when e1_null && e2_null -> retyper_ctx, CppBool true
            | OpLte when e1_null && e2_null -> retyper_ctx, CppBool true
            | OpNotEq when e1_null && e2_null -> retyper_ctx, CppBool false
            | _ when e1_null && e2_null -> retyper_ctx, CppBool false
            | OpEq when e1_null -> retyper_ctx, CppNullCompare ("IsNull", e2)
            | OpGte when e1_null -> retyper_ctx, CppNullCompare ("IsNull", e2)
            | OpLte when e1_null -> retyper_ctx, CppNullCompare ("IsNull", e2)
            | OpNotEq when e1_null -> retyper_ctx, CppNullCompare ("IsNotNull", e2)
            | OpEq when e2_null -> retyper_ctx, CppNullCompare ("IsNull", e1)
            | OpGte when e2_null -> retyper_ctx, CppNullCompare ("IsNull", e1)
            | OpLte when e2_null -> retyper_ctx, CppNullCompare ("IsNull", e1)
            | OpNotEq when e2_null -> retyper_ctx, CppNullCompare ("IsNotNull", e1)
            | OpEq when instance -> retyper_ctx, CppCompare ("IsInstanceEq", e1, e2, op)
            | OpNotEq when instance -> retyper_ctx, CppCompare ("IsInstanceNotEq", e1, e2, op)
            | OpEq when pointer -> retyper_ctx, CppCompare ("IsPointerEq", e1, e2, op)
            | OpNotEq when pointer -> retyper_ctx, CppCompare ("IsPointerNotEq", e1, e2, op)
            | OpEq when complex -> retyper_ctx, CppCompare ("IsEq", e1, e2, op)
            | OpNotEq when complex -> retyper_ctx, CppCompare ("IsNotEq", e1, e2, op)
            | OpGte when complex -> retyper_ctx, CppCompare ("IsGreaterEq", e1, e2, op)
            | OpLte when complex -> retyper_ctx, CppCompare ("IsLessEq", e1, e2, op)
            | OpGt when complex -> retyper_ctx, CppCompare ("IsGreater", e1, e2, op)
            | OpLt when complex -> retyper_ctx, CppCompare ("IsLess", e1, e2, op)
            | _ -> retyper_ctx, CppBinop (op, e1, e2)
          in
          match (op, e1.cpptype, e2.cpptype) with
          (* Variant + Variant = Variant *)
          | OpAdd, _, TCppVariant | OpAdd, TCppVariant, _ ->
            (retyper_ctx, reference, TCppVariant)
          | _, _, _ ->
            (retyper_ctx, reference, cpp_type_of expr.etype))
      | TUnop (op, pre, e1) ->
          let targetType =
            match op with
            | Not -> TCppScalar "bool"
            | NegBits -> TCppScalar "int"
            | _ -> cpp_type_of e1.etype
          in

          let retyper_ctx, e1 = retype retyper_ctx targetType e1 in
          let retyper_ctx, reference =
            match op with
            | Increment ->
                let lvalue, gc = to_lvalue e1 in
                let new_ctx = if gc then { retyper_ctx with gc_stack = true } else retyper_ctx in
                new_ctx, CppCrement (CppIncrement, pre, lvalue)
            | Decrement ->
                let lvalue, gc = to_lvalue e1 in
                let new_ctx = if gc then { retyper_ctx with gc_stack = true } else retyper_ctx in
                new_ctx, CppCrement (CppDecrement, pre, lvalue)
            | Neg -> retyper_ctx, CppUnop (CppNeg, e1)
            | Not -> retyper_ctx, CppUnop (CppNot, e1)
            | NegBits -> retyper_ctx, CppUnop (CppNegBits, e1)
            | Spread -> die ~p:expr.epos "Unexpected spread operator" __LOC__
          in
          (retyper_ctx, reference, cpp_type_of expr.etype)
      | TWhile (e1, e2, flag) ->
          let retyper_ctx, condition = retype retyper_ctx (TCppScalar "bool") e1 in
          let retyper_ctx, close = begin_loop retyper_ctx in
          let retyper_ctx, block = retype retyper_ctx TCppVoid (mk_block e2) in
          let retyper_ctx, id = close retyper_ctx in
          (retyper_ctx, CppWhile (condition, block, flag, id), TCppVoid)
      | TArrayDecl el ->
          let el_types = List.map (fun _ -> TCppDynamic) el in
          let retyper_ctx, retypedEls = retype_function_args retyper_ctx el el_types in
          (retyper_ctx, CppArrayDecl retypedEls, cpp_type_of expr.etype)
      | TBlock expr_list ->
          if return_type <> TCppVoid && not forCppia then
            print_endline
              ("Value from a block not handled " ^ expr.epos.pfile ^ " "
              ^ string_of_int (Lexer.get_error_line expr.epos));

          let new_ctx = { retyper_ctx with closures = []; injection = false } in
          let new_ctx, cppExprs, _ =
            List.fold_left
              (fun (cur_ctx, exprs, remaining) expr ->
                let targetType =
                  if retyper_ctx.injection && remaining = 1 then
                    cpp_type_of expr.etype
                  else
                    TCppVoid in
                let new_ctx, result = retype cur_ctx targetType expr in
                new_ctx, result :: exprs, remaining - 1)
              (new_ctx, [], List.length expr_list)
              expr_list
          in

          (* Add back any undeclared variables *)
          (* Needed for tracking variables captured by variables *)
          let folder acc (name, tvar) =
            if not (StringMap.mem name retyper_ctx.declarations) then
              StringMap.add name tvar acc
            else
              acc
            in
          let new_undeclared =
            List.fold_left
              folder
              retyper_ctx.undeclared
              (StringMap.bindings new_ctx.undeclared)
            in

          (
            { new_ctx with
              declarations = retyper_ctx.declarations;
              undeclared   = new_undeclared;
              closures     = retyper_ctx.closures },
            CppBlock (List.rev cppExprs, List.rev new_ctx.closures, new_ctx.gc_stack),
            TCppVoid
          )
      | TObjectDecl
          [
            (("fileName", _, _), { eexpr = TConst (TString file) });
            (("lineNumber", _, _), { eexpr = TConst (TInt line) });
            (("className", _, _), { eexpr = TConst (TString class_name) });
            (("methodName", _, _), { eexpr = TConst (TString meth) });
          ] ->
          (retyper_ctx, CppPosition (file, line, class_name, meth), TCppDynamic)
      | TObjectDecl el -> (
          let el_exprs = List.map (fun ((_, _, _), e) -> e) el in
          let el_names = List.map (fun ((v, _, _), _) -> v) el in

          let retyper_ctx, retyped_els =
            List.map (fun _ -> TCppDynamic) el |> retype_function_args retyper_ctx el_exprs
          in
          let joined = List.combine el_names retyped_els in

          match return_type with
          | TCppVoid -> (retyper_ctx, CppObjectDecl (joined, false), TCppVoid)
          | _ -> (retyper_ctx, CppObjectDecl (joined, false), TCppDynamic))
      | TVar (v, eo) ->
          let varType = cpp_type_of v.v_type in
          let retyper_ctx, init =
            match eo with
            | None -> retyper_ctx, None
            | Some e -> retype retyper_ctx varType e |> (fun (new_ctx, expr) -> new_ctx, Some expr)
          in
          let retyper_ctx = { retyper_ctx with declarations = StringMap.add v.v_name () retyper_ctx.declarations } in
          (retyper_ctx, CppVarDecl (v, init), varType)
      | TIf (ec, e1, e2) ->
          let retyper_ctx, ec = retype retyper_ctx (TCppScalar "bool") ec in
          let blockify =
            if return_type != TCppVoid then fun e -> e else mk_block
          in
          let retyper_ctx, e1 = retype retyper_ctx return_type (blockify e1) in
          let retyper_ctx, e2 =
            match e2 with
            | None -> retyper_ctx, None
            | Some e -> retype retyper_ctx return_type (blockify e) |> (fun (new_ctx, expr) -> new_ctx, Some expr)
          in
          ( retyper_ctx,
            CppIf (ec, e1, e2),
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
          let retyper_ctx, condition = retype retyper_ctx conditionType condition in
          let retyper_ctx, cppDef =
            match def with
            | None -> retyper_ctx, None
            | Some e -> retype retyper_ctx TCppVoid (mk_block e) |> (fun (new_ctx, expr) -> new_ctx, Some expr)
          in
          if forCppia then
            let retyper_ctx, cases =
              List.fold_left
                (fun (cur_ctx, acc) { case_patterns = el; case_expr = e2 } ->
                  let new_ctx, cppBlock = retype cur_ctx TCppVoid (mk_block e2) in
                  let new_ctx, blocks =
                    List.fold_left
                      (fun (cur_ctx, acc) t -> retype cur_ctx conditionType t |> (fun (new_ctx, expr) -> new_ctx, expr :: acc))
                      (new_ctx, [])
                      el
                  in
                  new_ctx, (List.rev blocks, cppBlock) :: acc)
                (retyper_ctx, [])
                cases
            in
            (retyper_ctx, CppSwitch (condition, conditionType, List.rev cases, cppDef, -1), TCppVoid)
          else
            try
              (match conditionType with
              | TCppScalar "int" | TCppScalar "bool" -> ()
              | _ -> raise Not_found);
              let retyper_ctx, cases =
                List.fold_left
                  (fun (cur_ctx, acc) { case_patterns = el; case_expr = e2 } ->
                    let new_ctx, expr = retype cur_ctx TCppVoid (mk_block e2) in
                    new_ctx, (List.map const_int_of el, expr) :: acc)
                  (retyper_ctx, [])
                  cases
              in
              (retyper_ctx, CppIntSwitch (condition, List.rev cases, cppDef), TCppVoid)
            with Not_found ->
              let retyper_ctx, label = alloc_file_id retyper_ctx in
              (* do something better maybe ... *)
              let retyper_ctx, cases =
                List.fold_left
                  (fun (cur_ctx, acc) { case_patterns = el; case_expr = e2 } ->
                    let new_ctx, cppBlock = retype cur_ctx TCppVoid (mk_block e2) in
                    let gotoExpr =
                      {
                        cppexpr = CppGoto label;
                        cpptype = TCppVoid;
                        cpppos = e2.epos;
                      }
                    in
                    let cppBlock = cpp_append_block cppBlock gotoExpr in
                    let new_ctx, blocks =
                      List.fold_left
                        (fun (cur_ctx, acc) t -> retype cur_ctx conditionType t |> (fun (new_ctx, expr) -> new_ctx, expr :: acc))
                        (new_ctx, [])
                        el in
                    new_ctx, (List.rev blocks, cppBlock) :: acc)
                  (retyper_ctx, [])
                  cases
              in
              ( retyper_ctx,
                CppSwitch (condition, conditionType, List.rev cases, cppDef, label),
                TCppVoid ))
      | TTry (try_block, catches) ->
          (* TTry internal return - wrap whole thing in block ? *)
          if return_type <> TCppVoid then
            abort "Value from a try-block not handled" expr.epos;
          let retyper_ctx, cppBlock = retype retyper_ctx TCppVoid try_block in
          let retyper_ctx, cppCatches =
            List.fold_left
              (fun (retyper_ctx, acc) (tvar, catch_block) ->
                let retyper_ctx = { retyper_ctx with declarations = StringMap.add tvar.v_name () retyper_ctx.declarations } in
                let retyper_ctx, cppCatchBlock = retype retyper_ctx TCppVoid catch_block in
                let retyper_ctx = { retyper_ctx with declarations = StringMap.remove tvar.v_name retyper_ctx.declarations } in
                retyper_ctx, (tvar, cppCatchBlock) :: acc)
              (retyper_ctx, [])
              catches
          in
          (retyper_ctx, CppTry (cppBlock, List.rev cppCatches), TCppVoid)
      | TReturn eo ->
          let retyper_ctx, expr = match eo with
          | None -> retyper_ctx, None
          | Some e -> retype retyper_ctx retyper_ctx.function_return_type e |> (fun (new_ctx, expr) -> new_ctx, Some expr) in
          ( retyper_ctx,
            CppReturn expr,
            TCppVoid )
      | TCast (base, None) -> (
          (* Use auto-cast rules *)
          let return_type = cpp_type_of expr.etype in
          let retyper_ctx, baseCpp = retype retyper_ctx return_type base in
          let baseStr = tcpp_to_string baseCpp.cpptype in
          let returnStr = tcpp_to_string return_type in
          if baseStr = returnStr then
            (retyper_ctx, baseCpp.cppexpr, baseCpp.cpptype (* nothing to do *))
          else
            match return_type with
            | TCppObjC k -> (retyper_ctx, CppCastObjC (baseCpp, k), return_type)
            | TCppPointer (_, _)
            | TCppRawPointer (_, _)
            | TCppStar _ | TCppInst _ ->
                (retyper_ctx, CppCast (baseCpp, return_type), return_type)
            | TCppString -> (retyper_ctx, CppCastScalar (baseCpp, "::String"), return_type)
            | TCppCode t when baseStr <> tcpp_to_string t ->
                (retyper_ctx, CppCast (baseCpp, t), t)
            | TCppNativePointer klass -> (retyper_ctx, CppCastNative baseCpp, return_type)
            | TCppObjCBlock (args, ret) ->
                (retyper_ctx, CppCastObjCBlock (baseCpp, args, ret), return_type)
            | TCppProtocol p -> (retyper_ctx, CppCastProtocol (baseCpp, p), return_type)
            | TCppDynamic when baseCpp.cpptype = TCppClass ->
                (retyper_ctx, CppCast (baseCpp, TCppDynamic), TCppDynamic)
            | _ -> (retyper_ctx, baseCpp.cppexpr, baseCpp.cpptype (* use autocasting rules *))
          )
      | TCast (base, Some t) -> (
          let retyper_ctx, baseCpp = retype retyper_ctx (cpp_type_of base.etype) base in
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
            (retyper_ctx, baseCpp.cppexpr, baseCpp.cpptype (* nothing to do *))
          else
            match return_type with
            | TCppNativePointer klass ->
              ( retyper_ctx, CppCastNative baseCpp, return_type)
            | TCppVoid ->
              (retyper_ctx, CppTCast (baseCpp, cpp_type_of expr.etype), return_type)
            | TCppDynamic ->
              (retyper_ctx, baseCpp.cppexpr, baseCpp.cpptype)
            | _ ->
              (retyper_ctx, CppTCast (baseCpp, return_type), return_type))
    in
    let cppExpr = mk_cppexpr retypedExpr retypedType in

    (* Autocast rules... *)
    if return_type = TCppVoid then
      retyper_ctx, mk_cppexpr retypedExpr TCppVoid
    else if return_type = TCppVarArg then
      match cpp_variant_type_of cppExpr.cpptype with
      | TCppVoidStar | TCppScalar _ -> retyper_ctx, cppExpr
      | TCppString ->
        retyper_ctx, mk_cppexpr
            (CppVar (VarInternal (cppExpr, ".", "raw_ptr()")))
            (TCppPointer ("ConstPointer", TCppScalar "char"))
      | TCppDynamic -> retyper_ctx, mk_cppexpr (CppCastNative cppExpr) TCppVoidStar
      | _ ->
          let toDynamic =
            mk_cppexpr (CppCast (cppExpr, TCppDynamic)) TCppDynamic
          in
          retyper_ctx, mk_cppexpr (CppCastNative toDynamic) TCppVoidStar
    else if
      cppExpr.cpptype = TCppVariant
      || cppExpr.cpptype = TCppDynamic
      || cppExpr.cpptype == TCppObject
    then
      match return_type with
      | TCppUnchanged -> retyper_ctx, cppExpr
      | TCppInst (t, _) when Meta.has Meta.StructAccess t.cl_meta ->
          let structType = TCppStruct (TCppInst (t, [])) in
          let structCast =
            mk_cppexpr (CppCast (cppExpr, structType)) structType
          in
          retyper_ctx, mk_cppexpr (CppCast (structCast, TCppInst (t, []))) (TCppInst (t, []))
      | TCppObjectArray _ | TCppScalarArray _ | TCppNativePointer _
      | TCppDynamicArray | TCppObjectPtr | TCppVarArg | TCppInst _ ->
        retyper_ctx, mk_cppexpr (CppCast (cppExpr, return_type)) return_type
      | TCppObjC k -> retyper_ctx, mk_cppexpr (CppCastObjC (cppExpr, k)) return_type
      | TCppObjCBlock (ret, args) ->
        retyper_ctx, mk_cppexpr (CppCastObjCBlock (cppExpr, ret, args)) return_type
      | TCppScalar scalar ->
        retyper_ctx, mk_cppexpr (CppCastScalar (cppExpr, scalar)) return_type
      | TCppString ->
        retyper_ctx, mk_cppexpr (CppCastScalar (cppExpr, "::String")) return_type
      | TCppInterface _ when cppExpr.cpptype = TCppVariant ->
        retyper_ctx, mk_cppexpr (CppCastVariant cppExpr) return_type
      | TCppDynamic when cppExpr.cpptype = TCppVariant ->
        retyper_ctx, mk_cppexpr (CppCastVariant cppExpr) return_type
      | TCppStar (t, const) ->
          let ptrType =
            TCppPointer ((if const then "ConstPointer" else "Pointer"), t)
          in
          let ptrCast = mk_cppexpr (CppCast (cppExpr, ptrType)) ptrType in
          retyper_ctx, mk_cppexpr
            (CppCast (ptrCast, TCppStar (t, const)))
            (TCppStar (t, const))
      | _ -> retyper_ctx, cppExpr
    else
      match (cppExpr.cpptype, return_type) with
      | _, TCppUnchanged -> retyper_ctx, cppExpr
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
          retyper_ctx, mk_cppexpr (CppCast (cppExpr, return_type)) return_type
      (* Infer type from right-hand-side for pointer or reference to Dynamic *)
      | TCppReference TCppDynamic, TCppReference _ -> retyper_ctx, cppExpr
      | TCppReference TCppDynamic, t -> retyper_ctx, mk_cppexpr retypedExpr (TCppReference t)
      | TCppStar (TCppDynamic, _), TCppStar (_, _) -> retyper_ctx, cppExpr
      | TCppStar (TCppDynamic, const), t ->
        retyper_ctx, mk_cppexpr retypedExpr (TCppStar (t, const))
      | TCppStar (t, const), TCppDynamic ->
          let ptrType =
            TCppPointer ((if const then "ConstPointer" else "Pointer"), t)
          in
          let ptrCast = mk_cppexpr (CppCast (cppExpr, ptrType)) ptrType in
          retyper_ctx, mk_cppexpr (CppCast (ptrCast, TCppDynamic)) TCppDynamic
      | TCppStar (t, const), TCppReference _
      | TCppStar (t, const), TCppInst _
      | TCppStar (t, const), TCppStruct _ ->
        retyper_ctx, mk_cppexpr (CppDereference cppExpr) return_type
      | TCppInst (t, _), TCppStar _
        when is_native_class t
             &&
             match cppExpr.cppexpr with
             | CppCall (FuncNew _, _) -> true
             | _ -> false ->
              retyper_ctx, mk_cppexpr (CppNewNative cppExpr) return_type
      | TCppInst _, TCppStar (p, const) | TCppStruct _, TCppStar (p, const) ->
        retyper_ctx, mk_cppexpr (CppAddressOf cppExpr) return_type
      | TCppObjectPtr, TCppObjectPtr -> retyper_ctx, cppExpr
      | TCppObjectPtr, _ ->
        retyper_ctx, mk_cppexpr (CppCast (cppExpr, TCppDynamic)) TCppDynamic
      | TCppProtocol _, TCppProtocol _ -> retyper_ctx, cppExpr
      | t, TCppProtocol protocol ->
        retyper_ctx, mk_cppexpr (CppCastProtocol (cppExpr, protocol)) return_type
      | TCppInst (t, _), TCppDynamic when Meta.has Meta.StructAccess t.cl_meta
        ->
          let structType = TCppStruct (TCppInst (t, [])) in
          let structCast =
            mk_cppexpr (CppCast (cppExpr, structType)) structType
          in
          retyper_ctx, mk_cppexpr (CppCast (structCast, TCppDynamic)) TCppDynamic
      | _, TCppObjectPtr ->
        retyper_ctx, mk_cppexpr (CppCast (cppExpr, TCppObjectPtr)) TCppObjectPtr
      | TCppDynamicArray, TCppScalarArray _
      | TCppDynamicArray, TCppObjectArray _
      | TCppScalarArray _, TCppDynamicArray
      | TCppObjectArray _, TCppDynamicArray
        when forCppia ->
          retyper_ctx, mk_cppexpr (CppCast (cppExpr, return_type)) return_type
      | TCppScalar from, TCppScalar too when from <> too ->
        retyper_ctx, mk_cppexpr (CppCastScalar (cppExpr, too)) return_type
      | _ -> retyper_ctx, cppExpr
  in
  retype initial_ctx request_type expression_tree |> snd

let get_id path ids =
  let class_name = class_text path in
  let needs_new_id id =
    (* IDs less than 100 are reserved for hxcpp internal classes *)
    (* If the map already contains this ID we also need a new one *)
    id < Int32.of_int 100 || ObjectIds.collision id ids
  in

  let rec make_id seed =
    let id = CppStrings.gen_hash32 seed class_name in
    if needs_new_id id then
      make_id (seed + 100)
    else
      id
  in

  match ObjectIds.find_opt path ids with
  | Some existing ->
    (existing, ids)
  | None ->
    let new_id = make_id 0 in
    (new_id, ObjectIds.add path new_id ids)
  
let native_field_name_remap field =
  match get_meta_string field.cf_meta Meta.Native with
  | Some nativeImpl ->
    keyword_remap nativeImpl
  | None ->
    keyword_remap field.cf_name

let rec tcpp_class_from_tclass ctx ids slots class_def class_params =
  let scriptable = Gctx.defined ctx.ctx_common Define.Scriptable in

  let create_function field func = {
    tcf_field = field;
    tcf_name = native_field_name_remap field;
    tcf_func = func;
    tcf_is_virtual = not (has_meta Meta.NonVirtual field.cf_meta);
    tcf_is_reflective = reflective class_def field;
    tcf_is_external = not (is_internal_member field.cf_name);
    tcf_is_overriding = is_override field;
    tcf_is_scriptable = scriptable;
  } in

  let create_variable field = {
    tcv_field = field;
    tcv_name = native_field_name_remap field;
    tcv_type = field.cf_type;
    tcv_default = None;

    tcv_is_stackonly = has_meta Meta.StackOnly field.cf_meta;
    tcv_is_reflective = reflective class_def field;
    tcv_is_gc_element = cpp_type_of field.cf_type |> is_gc_element ctx;
  } in

  let filter_functions is_static field =
    if should_implement_field field then
      match (field.cf_kind, field.cf_expr) with
      | Method (MethNormal | MethInline), Some { eexpr = TFunction func } ->
        Some (create_function field func)
      | Method MethNormal, _ when has_class_field_flag field CfAbstract ->
        (* We need to fetch the default values for abstract functions from the @:Value meta *)
        let abstract_tfunc =
          match field.cf_type with
          | TFun (args, ret) ->
            let get_default_value name =
              try
                match Meta.get Meta.Value field.cf_meta with
                | _, [ (EObjectDecl decls, _) ], _ ->
                  Some
                    (decls
                      |> List.find (fun ((n, _, _), _) -> n = name)
                      |> snd
                      |> type_constant_value ctx.ctx_common.basic)
                | _ -> None
              with Not_found -> None
            in

            (* Generate a no op tfunc for our abstract *)
            (* This allows it to go through the rest of the generator with no special cases *)
            (* We can't implement abstract functions as pure virtual due to cppia needing to construct the class *)
            let map_arg (name, _, t) =
              ( (alloc_var VGenerated name t null_pos), (get_default_value name) ) in
            let expr = 
              match follow ret with
              | TAbstract ({ a_path = ([], "Void") }, _) ->
                { eexpr = TReturn None; etype = ret; epos = null_pos }
              | _ ->
                let zero_val = Some { eexpr = TConst (TInt Int32.zero); etype = ret; epos = null_pos } in
                { eexpr = TReturn zero_val; etype = ret; epos = null_pos } in
            
            {
              tf_args = args |> List.map map_arg;
              tf_type = ret;
              tf_expr = expr;
            }
          | _ ->
            die "expected abstract field type to be TFun" __LOC__
        in

        Some (create_function field abstract_tfunc)
      | _ ->
        None
    else
      None
    in

  let filter_dynamic_functions func_for_static_field field =
    if should_implement_field field then
      match (field.cf_kind, field.cf_expr) with
      | Method MethDynamic, Some { eexpr = TFunction func } ->
        Some (create_function field func)
      (* static variables with a default function value get a dynamic function generated as the implementation *)
      | Var _, Some { eexpr = TFunction func } when func_for_static_field ->
        Some (create_function field func)
      | _ ->
        None
    else
      None
  in

  let filter_variables field =
    if is_physical_field field then
      match (field.cf_kind, field.cf_expr) with
      | Var _, _ ->
        Some (create_variable field)
      (* Dynamic methods are implemented as a physical field holding a closure *)
      | Method MethDynamic, Some { eexpr = TFunction func } -> 
        Some (create_variable { field with cf_expr = None; cf_kind = Var ({ v_read = AccNormal; v_write = AccNormal }) })
      (* Below should cause abstracts which have functions with no implementation to be generated as a field *)
      (* See Int32.hx as an example *)
      | Method (MethNormal | MethInline), None when not (has_class_field_flag field CfAbstract) ->
        Some (create_variable field)
      | _ ->
        None
    else
      None
  in

  let filter_properties field =
    match field.cf_kind with
    | Var _ when not (is_physical_var_field field) ->
      Some (create_variable field)
    | _ ->
      None in

  let id, ids = get_id class_def.cl_path ids in
  
  let static_functions =
    class_def.cl_ordered_statics
    |> List.filter_map (filter_functions true) in

  let static_dynamic_functions =
    class_def.cl_ordered_statics
    |> List.filter_map (filter_dynamic_functions true) in

  let static_variables =
    class_def.cl_ordered_statics
    |> List.filter (fun field -> field.cf_name <> "__meta__" && field.cf_name <> "__rtti")
    |> List.filter_map filter_variables in

  let static_properties =
    class_def.cl_ordered_statics
    |> List.filter (fun field -> field.cf_name <> "__meta__" && field.cf_name <> "__rtti")
    |> List.filter_map filter_properties in

  let functions = 
    class_def.cl_ordered_fields
    |> List.filter_map (filter_functions true) in

  let dynamic_functions =
    class_def.cl_ordered_fields
    |> List.filter_map (filter_dynamic_functions false) in

  let variables =
    class_def.cl_ordered_fields
    |> List.filter_map filter_variables in

  let properties =
    class_def.cl_ordered_fields
    |> List.filter_map filter_properties in

  (* All interfaces (and sub-interfaces) implemented *)
  let rec folder (slots, haxe, native) (interface, _) =
    let slots, retyped = tcpp_interface_from_tclass ctx slots interface in
    let acc = if is_native_class interface then
      List.fold_left folder (slots, haxe, PathMap.add interface.cl_path retyped native) interface.cl_implements
    else
      List.fold_left folder (slots, PathMap.add interface.cl_path retyped haxe, native) interface.cl_implements in

    match interface.cl_super with
    | Some super -> folder acc super
    | None -> acc
  in
  let values (slots, haxe, native) =
    slots, haxe |> PathMap.bindings |> List.map (fun (_, v) -> v), native |> PathMap.bindings |> List.map (fun (_, v) -> v) in

  let (slots, ids, parent) =
    match class_def.cl_super with
    | Some (cls, params) ->
      let slots, ids, parent = tcpp_class_from_tclass ctx ids slots cls params in
      (slots, ids, Some parent)
    | None ->
      (slots, ids, None)
    in
  let slots, haxe_implementations, native_implementations =
    class_def.cl_implements
    |> real_interfaces
    |> List.fold_left folder (slots, PathMap.empty, PathMap.empty)
    |> values in

  let gc_container_type =
    let type_cant_be_null t =
      match cpp_type_of t with TCppScalar _ -> true | _ -> false in

    let rec gc_container variables super v =
      match List.exists (fun v -> not (type_cant_be_null v.tcv_type)) variables, super with
      | true, _ -> Some v
      | false, Some super -> gc_container super.tcl_variables super.tcl_super Parent
      | false, None -> None
    in

    gc_container variables parent Current
  in

  let flags = 0
    |> (fun f -> if scriptable && not class_def.cl_private then set_tcpp_class_flag f Scriptable else f)
    |> (fun f -> if can_quick_alloc class_def then set_tcpp_class_flag f QuickAlloc else f)
    |> (fun f -> if has_get_member_field class_def then set_tcpp_class_flag f MemberGet else f)
    |> (fun f -> if has_set_member_field class_def then set_tcpp_class_flag f MemberSet else f)
    |> (fun f -> if has_get_static_field class_def then set_tcpp_class_flag f StaticGet else f)
    |> (fun f -> if has_set_static_field class_def then set_tcpp_class_flag f StaticSet else f)
    |> (fun f -> if has_get_fields class_def then set_tcpp_class_flag f GetFields else f)
    |> (fun f -> if has_compare_field class_def then set_tcpp_class_flag f Compare else f)
    |> (fun f -> if has_boot_field class_def then set_tcpp_class_flag f Boot else f)
  in

  let meta_field = List.find_opt (fun field -> field.cf_name = "__meta__") class_def.cl_ordered_statics |> Option.map (fun f -> Option.get f.cf_expr) in
  let rtti_field = List.find_opt (fun field -> field.cf_name = "__rtti") class_def.cl_ordered_statics |> Option.map (fun f -> Option.get f.cf_expr) in

  let cls = {
    tcl_class = class_def;
    tcl_params = class_params;
    tcl_id = id;
    tcl_name = class_name class_def;
    tcl_flags = flags;
    tcl_super = parent;
    tcl_container = gc_container_type;
    tcl_debug_level = if Meta.has Meta.NoDebug class_def.cl_meta || Gctx.defined ctx.ctx_common Define.NoDebug then 0 else ctx.ctx_debug_level;
    tcl_static_variables = static_variables;
    tcl_static_properties = static_properties;
    tcl_static_functions = static_functions;
    tcl_static_dynamic_functions = static_dynamic_functions;
    tcl_variables = variables;
    tcl_properties = properties;
    tcl_functions = functions;
    tcl_dynamic_functions = dynamic_functions;
    tcl_haxe_interfaces = haxe_implementations;
    tcl_native_interfaces = native_implementations;
    tcl_meta = meta_field;
    tcl_rtti = rtti_field;
    tcl_init = TClass.get_cl_init class_def;
  } in

  (slots, ids, cls)

and tcpp_interface_from_tclass ctx slots class_def =

  let scriptable = Gctx.defined ctx.ctx_common Define.Scriptable && not class_def.cl_private in

  let function_filter (slots, fields) field =
    match (field.cf_type, field.cf_kind) with
    | TFun (args, ret), Method _ ->
      let slots = if scriptable then
        CppAst.InterfaceSlots.add field.cf_name slots
      else
        slots
      in
      let retyped = {
        iff_field       = field;
        iff_name        = native_field_name_remap field;
        iff_args        = args |> List.map (fun (name, opt, t) -> (keyword_remap name, opt, t));
        iff_return      = ret;
        iff_script_slot = CppAst.InterfaceSlots.find_opt field.cf_name slots
      } in
        (slots, retyped :: fields)
    | _ ->
      (slots, fields)
  in
  let variable_filter field =
    match field.cf_kind with
    | Var _ when is_physical_var_field field -> true
    | _ -> false
  in

  let debug_level = if Meta.has Meta.NoDebug class_def.cl_meta || Gctx.defined ctx.ctx_common Define.NoDebug then 0 else ctx.ctx_debug_level in
  let meta_field  = List.find_opt (fun field -> field.cf_name = "__meta__") class_def.cl_ordered_statics |> Option.map (fun f -> Option.get f.cf_expr) in
  let rtti_field  = List.find_opt (fun field -> field.cf_name = "__rtti") class_def.cl_ordered_statics |> Option.map (fun f -> Option.get f.cf_expr) in
  let slots, extends =
    match class_def.cl_super with
    | Some (s, _) ->
      let extra, iface = tcpp_interface_from_tclass ctx slots s in
      (extra, Some iface)
    | None ->
      (slots, None)
  in

  let slots, functions = List.fold_left function_filter (slots, []) class_def.cl_ordered_fields in

  let iface = {
    if_class = class_def;
    if_name = class_name class_def;
    if_hash = CppStrings.gen_hash 0 (join_class_path class_def.cl_path "::");
    if_debug_level = debug_level;
    if_functions = functions |> List.rev;
    if_variables = List.filter variable_filter class_def.cl_ordered_fields;
    if_meta = meta_field;
    if_rtti = rtti_field;
    if_extends = extends;
    if_scriptable = scriptable;
  } in

  (slots, iface)

and tcpp_enum_from_tenum ctx ids enum_def =
  let sort_constructors f1 f2 =
    f1.ef_index - f2.ef_index in

  let self_id, ids = get_id enum_def.e_path ids in
  let strq         = CppStrings.strq ctx.ctx_common in
  let constructors =
    enum_def.e_constrs
    |> pmap_values
    |> List.sort sort_constructors
    |> List.map (fun f -> { tef_field = f; tef_name = keyword_remap f.ef_name; tef_hash = strq f.ef_name})
  in
  let enum = { te_enum = enum_def; te_id = self_id; te_constructors = constructors } in

  (ids, enum)