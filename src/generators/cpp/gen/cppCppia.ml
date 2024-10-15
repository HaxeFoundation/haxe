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
open CppSourceWriter
open CppContext

let cpp_type_of = CppRetyper.cpp_type_of

let script_type t optional = if optional then begin
  match type_string t with
  | "::String" -> "String"
  | _ -> "Object"
  end else match type_string t with
  | "bool" -> "Int"
  | "int" | "::cpp::Int32" -> "Int"
  | "Float" -> "Float"
  | "::String" -> "String"
  | "Null" -> "Void"
  | "Void" -> "Void"
  | "float" | "::cpp::Float32" | "::cpp::Float64" -> "Float"
  | "::cpp::Int64" | "::cpp::UInt64" -> "Object"
  | _ -> "Object"

let script_signature t optional = match script_type t optional with
  | "Bool" -> "b"
  | "Int" -> "i"
  | "Float" -> "f"
  | "String" -> "s"
  | "Void" -> "v"
  | "void" -> "v"
  | _ -> "o"

let script_size_type t optional = match script_type t optional with
  | "Object" -> "void *"
  | "Int" -> "int"
  | "Bool" -> "bool"
  | x -> x

let rec script_type_string haxe_type =
  match haxe_type with
  | TAbstract ({ a_path = [], "Null" }, [ t ]) -> (
      match follow t with
      | TAbstract ({ a_path = [], "Int" }, _)
      | TAbstract ({ a_path = [], "Float" }, _)
      | TAbstract ({ a_path = [], "Bool" }, _) ->
          "Dynamic"
      | _ -> script_type_string t)
  | TInst ({ cl_path = [], "Null" }, [ t ]) -> (
      match follow t with
      | TAbstract ({ a_path = [], "Int" }, _)
      | TAbstract ({ a_path = [], "Float" }, _)
      | TAbstract ({ a_path = [], "Bool" }, _) ->
          "Dynamic"
      | _ -> script_type_string t)
  | _ -> (
      match follow haxe_type with
      | TType ({ t_path = [], "Array" }, params) -> "Array"
      | TInst ({ cl_path = [], "Array" }, params) -> (
          match params with
          | [ t ] -> (
              match type_string_suff "" t false with
              | "int" -> "Array.int"
              | "Float" -> "Array.Float"
              | "bool" -> "Array.bool"
              | "::String" -> "Array.String"
              | "unsigned char" -> "Array.unsigned char"
              | "::cpp::UInt8" -> "Array.unsigned char"
              | "Dynamic" -> "Array.Any"
              | _ -> "Array.Object")
          | _ -> "Array.Object")
      | TAbstract (abs, pl) when abs.a_impl <> None ->
          script_type_string (Abstract.get_underlying_type abs pl)
      | _ -> type_string_suff "" haxe_type false)

let rec script_cpptype_string cppType =
  match cppType with
  | TCppDynamic | TCppUnchanged | TCppWrapped _ | TCppObject -> "Dynamic"
  | TCppObjectPtr -> ".*.hx.Object*"
  | TCppReference t -> ".ref." ^ script_cpptype_string t
  | TCppStruct t -> ".struct." ^ script_cpptype_string t
  | TCppStar (t, _) -> "*." ^ script_cpptype_string t
  | TCppVoid -> "void"
  | TCppVoidStar -> "*.void"
  | TCppRest _ -> "vaarg_list"
  | TCppVarArg -> "vararg"
  | TCppAutoCast -> ".cpp.AutoCast"
  | TCppVariant -> ".cpp.Variant"
  | TCppEnum enum -> join_class_path enum.e_path "."
  | TCppScalar scalar -> scalar
  | TCppString -> "String"
  | TCppFastIterator it -> "cpp.FastIterator." ^ script_cpptype_string it
  | TCppPointer (_, valueType) ->
      "cpp.Pointer." ^ script_cpptype_string valueType
  | TCppRawPointer (_, valueType) ->
      "cpp.RawPointer." ^ script_cpptype_string valueType
  | TCppFunction _ -> "cpp.Function"
  | TCppObjCBlock _ -> "cpp.ObjCBlock"
  | TCppDynamicArray -> "Array.Any"
  | TCppObjectArray _ -> "Array.Object"
  | TCppScalarArray value -> "Array." ^ script_cpptype_string value
  | TCppObjC _ -> "cpp.ObjC"
  | TCppProtocol _ -> "cpp.ObjC.Protocol"
  | TCppNativePointer klass ->
      "cpp.Pointer." ^ join_class_path klass.cl_path "."
  | TCppInterface klass -> join_class_path klass.cl_path "."
  | TCppInst (klass, _) -> join_class_path klass.cl_path "."
  | TCppClass -> "Class"
  | TCppGlobal -> "?global"
  | TCppNull -> "null"
  | TCppCode _ -> "Dynamic"

type array_of =
  | ArrayInterface of int
  | ArrayData of string
  | ArrayObject
  | ArrayAny
  | ArrayNone

let is_template_type t = false

type cppia_op =
  | IaFunction
  | IaVar
  | IaToInterface
  | IaToDynArray
  | IaToDataArray
  | IaToInterfaceArray
  | IaFun
  | IaCast
  | IaTCast
  | IaBlock
  | IaBreak
  | IaContinue
  | IaIsNull
  | IaNotNull
  | IaSet
  | IaCall
  | IaCallGlobal
  | IaCallStatic
  | IaCallMember
  | IaCallSuper
  | IaCallThis
  | IaCallSuperNew
  | IaCreateEnum
  | IaADef
  | IaIf
  | IaIfElse
  | IaFStatic
  | IaFName
  | IaFThisInst
  | IaFLink
  | IaFThisName
  | IaFEnum
  | IaThrow
  | IaArrayI
  | IaPlusPlus
  | IaPlusPlusPost
  | IaMinusMinus
  | IaMinusMinusPost
  | IaNeg
  | IaBitNot
  | IaLogicNot
  | IaTVars
  | IaVarDecl
  | IaVarDeclI
  | IaNew
  | IaReturn
  | IaRetVal
  | IaPosInfo
  | IaObjDef
  | IaClassOf
  | IaWhile
  | IaFor
  | IaEnumI
  | IaSwitch
  | IaTry
  | IaImplDynamic
  | IaConstInt
  | IaConstFloat
  | IaConstString
  | IaConstFalse
  | IaConstTrue
  | IaConstNull
  | IaConsThis
  | IaConstSuper
  | IaCastInt
  | IaCastBool
  | IaInterface
  | IaClass
  | IaAccessNormal
  | IaAccessNot
  | IaAccessResolve
  | IaAccessCall
  | IaEnum
  | IaInline
  | IaMain
  | IaNoMain
  | IaResources
  | IaReso
  | IaNoCast
  | IaAccessCallNative
  | IaBinOp of Ast.binop

let cppia_op_info = function
  | IaFunction -> ("FUNCTION", 1)
  | IaVar -> ("VAR", 2)
  | IaToInterface -> ("TOINTERFACE", 3)
  | IaToDynArray -> ("TODYNARRAY", 4)
  | IaToDataArray -> ("TODATAARRAY", 5)
  | IaToInterfaceArray -> ("TOINTERFACEARRAY", 6)
  | IaFun -> ("FUN", 7)
  | IaCast -> ("CAST", 8)
  | IaBlock -> ("BLOCK", 9)
  | IaBreak -> ("BREAK", 10)
  | IaContinue -> ("CONTINUE", 11)
  | IaIsNull -> ("ISNULL", 12)
  | IaNotNull -> ("NOTNULL", 13)
  | IaSet -> ("SET", 14)
  | IaCall -> ("CALL", 15)
  | IaCallGlobal -> ("CALLGLOBAL", 16)
  | IaCallStatic -> ("CALLSTATIC", 17)
  | IaCallMember -> ("CALLMEMBER", 18)
  | IaCallSuper -> ("CALLSUPER", 19)
  | IaCallThis -> ("CALLTHIS", 20)
  | IaCallSuperNew -> ("CALLSUPERNEW", 21)
  | IaCreateEnum -> ("CREATEENUM", 22)
  | IaADef -> ("ADEF", 23)
  | IaIf -> ("IF", 24)
  | IaIfElse -> ("IFELSE", 25)
  | IaFName -> ("FNAME", 27)
  | IaFStatic -> ("FSTATIC", 28)
  | IaFThisInst -> ("FTHISINST", 29)
  | IaFLink -> ("FLINK", 30)
  | IaFThisName -> ("FTHISNAME", 31)
  | IaFEnum -> ("FENUM", 32)
  | IaThrow -> ("THROW", 33)
  | IaArrayI -> ("ARRAYI", 34)
  | IaPlusPlus -> ("++", 35)
  | IaPlusPlusPost -> ("+++", 36)
  | IaMinusMinus -> ("--", 37)
  | IaMinusMinusPost -> ("---", 38)
  | IaNeg -> ("NEG", 39)
  | IaBitNot -> ("~", 40)
  | IaLogicNot -> ("!", 41)
  | IaTVars -> ("TVARS", 42)
  | IaVarDecl -> ("VARDECL", 43)
  | IaVarDeclI -> ("VARDECLI", 44)
  | IaNew -> ("NEW", 45)
  | IaReturn -> ("RETURN", 46)
  | IaRetVal -> ("RETVAL", 47)
  | IaPosInfo -> ("POSINFO", 48)
  | IaObjDef -> ("OBJDEF", 49)
  | IaClassOf -> ("CLASSOF", 50)
  | IaWhile -> ("WHILE", 51)
  | IaFor -> ("FOR", 52)
  | IaEnumI -> ("ENUMI", 53)
  | IaSwitch -> ("SWITCH", 54)
  | IaTry -> ("TRY", 55)
  | IaImplDynamic -> ("IMPLDYNAMIC", 56)
  | IaConstInt -> ("i", 57)
  | IaConstFloat -> ("f", 58)
  | IaConstString -> ("s", 59)
  | IaConstFalse -> ("false", 60)
  | IaConstTrue -> ("true", 61)
  | IaConstNull -> ("NULL", 62)
  | IaConsThis -> ("THIS", 63)
  | IaConstSuper -> ("SUPER", 64)
  | IaCastInt -> ("CASTINT", 65)
  | IaCastBool -> ("CASTBOOL", 66)
  | IaInterface -> ("INTERFACE", 67)
  | IaClass -> ("CLASS", 68)
  | IaAccessNormal -> ("N", 69)
  | IaAccessNot -> ("n", 70)
  | IaAccessResolve -> ("R", 71)
  | IaAccessCall -> ("C", 72)
  | IaEnum -> ("ENUM", 73)
  | IaInline -> ("INLINE", 74)
  | IaMain -> ("MAIN", 75)
  | IaNoMain -> ("NOMAIN", 76)
  | IaResources -> ("RESOURCES", 77)
  | IaReso -> ("RESO", 78)
  | IaNoCast -> ("NOCAST", 79)
  | IaAccessCallNative -> ("V", 80)
  | IaBinOp OpAdd -> ("+", 101)
  | IaBinOp OpMult -> ("*", 102)
  | IaBinOp OpDiv -> ("/", 103)
  | IaBinOp OpSub -> ("-", 104)
  | IaBinOp OpAssign -> ("=", 105)
  | IaBinOp OpEq -> ("==", 106)
  | IaBinOp OpNotEq -> ("!=", 107)
  | IaBinOp OpGte -> (">=", 108)
  | IaBinOp OpLte -> ("<=", 109)
  | IaBinOp OpGt -> (">", 110)
  | IaBinOp OpLt -> ("<", 111)
  | IaBinOp OpAnd -> ("&", 112)
  | IaBinOp OpOr -> ("|", 113)
  | IaBinOp OpXor -> ("^", 114)
  | IaBinOp OpBoolAnd -> ("&&", 115)
  | IaBinOp OpBoolOr -> ("||", 116)
  | IaBinOp OpShr -> (">>", 117)
  | IaBinOp OpUShr -> (">>>", 118)
  | IaBinOp OpShl -> ("<<", 119)
  | IaBinOp OpMod -> ("%", 120)
  | IaBinOp OpInterval -> ("...", 121)
  | IaBinOp OpArrow -> ("=>", 122)
  | IaBinOp OpIn -> (" in ", 123)
  | IaBinOp OpNullCoal -> ("??", 124)
  | IaBinOp (OpAssignOp OpAdd) -> ("+=", 201)
  | IaBinOp (OpAssignOp OpMult) -> ("*=", 202)
  | IaBinOp (OpAssignOp OpDiv) -> ("/=", 203)
  | IaBinOp (OpAssignOp OpSub) -> ("-=", 204)
  | IaBinOp (OpAssignOp OpAnd) -> ("&=", 212)
  | IaBinOp (OpAssignOp OpOr) -> ("|=", 213)
  | IaBinOp (OpAssignOp OpXor) -> ("^=", 214)
  | IaBinOp (OpAssignOp OpBoolAnd) -> ("&&=", 215)
  | IaBinOp (OpAssignOp OpBoolOr) -> ("||=", 216)
  | IaBinOp (OpAssignOp OpShr) -> (">>=", 217)
  | IaBinOp (OpAssignOp OpUShr) -> (">>>=", 218)
  | IaBinOp (OpAssignOp OpShl) -> ("<<=", 219)
  | IaBinOp (OpAssignOp OpMod) -> ("%=", 220)
  | IaBinOp (OpAssignOp OpIn)
  | IaBinOp (OpAssignOp OpNullCoal)
  | IaBinOp (OpAssignOp OpInterval)
  | IaBinOp (OpAssignOp OpAssign)
  | IaBinOp (OpAssignOp OpEq)
  | IaBinOp (OpAssignOp OpNotEq)
  | IaBinOp (OpAssignOp OpGte)
  | IaBinOp (OpAssignOp OpLte)
  | IaBinOp (OpAssignOp OpGt)
  | IaBinOp (OpAssignOp OpLt)
  | IaBinOp (OpAssignOp (OpAssignOp _))
  | IaBinOp (OpAssignOp OpArrow) ->
      die "" __LOC__
  | IaTCast -> ("TCAST", 221)

let follow = Abstract.follow_with_abstracts

let is_matching_interface_type t0 t1 =
  match (follow t0, follow t1) with
  | TInst (k0, _), TInst (k1, _) -> k0 == k1
  | _ -> false

let rec is_null expr =
  match expr.eexpr with
  | TConst TNull -> true
  | TParenthesis expr | TMeta (_, expr) -> is_null expr
  | TCast (e, None) -> is_null e
  | _ -> false

let is_virtual_array expr = type_string expr.etype = "cpp::VirtualArray"

let is_this expression =
  match (remove_parens expression).eexpr with
  | TConst TThis -> true
  | _ -> false

let is_super expression =
  match (remove_parens expression).eexpr with
  | TConst TSuper -> true
  | _ -> false

let is_native_pointer expr =
  let t = type_string expr.etype in
  let l = String.length t in
  l > 1 && String.sub t (l - 1) 1 = "*"

let is_extern_class_instance obj =
  match follow obj.etype with
  | TInst (klass, params) -> has_class_flag klass CExtern
  | _ -> false

let rec is_dynamic_in_cpp ctx expr =
  let expr_type =
    type_string
      (match follow expr.etype with TFun (args, ret) -> ret | _ -> expr.etype)
  in
  if expr_type = "Dynamic" || expr_type = "cpp::ArrayBase" then true
  else
    let result =
      match expr.eexpr with
      | TEnumParameter (obj, _, index) -> true (* TODO? *)
      | TField (obj, field) ->
          is_dynamic_member_lookup_in_cpp ctx obj field
          || is_dynamic_member_return_in_cpp ctx obj field
      | TArray (obj, index) -> is_dynamic_in_cpp ctx obj || is_virtual_array obj
      | TTypeExpr _ -> false
      | TCall (func, args) -> (
          let is_IaCall =
            match (remove_parens_cast func).eexpr with
            | TField ({ eexpr = TIdent "__global__" }, field) -> false
            | TField (obj, FStatic (class_def, field))
              when is_real_function field ->
                false
            | TField (obj, FInstance (_, _, field))
              when is_this obj && is_real_function field ->
                false
            | TField (obj, FInstance (_, _, field)) when is_super obj -> false
            | TField (obj, FInstance (_, _, field))
              when field.cf_name = "_hx_getIndex" ->
                false
            | TField (obj, FInstance (_, _, field))
              when field.cf_name = "__Index"
                   || (not (is_dynamic_in_cppia ctx obj))
                      && is_real_function field ->
                false
            | TField (obj, FDynamic name)
              when is_internal_member name
                   || (type_string obj.etype = "::String" && name = "cca") ->
                false
            | TConst TSuper -> false
            | TField (_, FEnum (enum, field)) -> false
            | _ -> true
          in
          if is_IaCall then true
          else
            match follow func.etype with
            | TFun (args, ret) -> is_dynamic_in_cpp ctx func
            | _ -> true)
      | TParenthesis expr | TMeta (_, expr) -> is_dynamic_in_cpp ctx expr
      | TCast (e, None) -> type_string expr.etype = "Dynamic"
      | TIdent "__global__" -> false
      | TConst TNull -> true
      | _ -> false (* others ? *)
    in
    result

and is_dynamic_member_lookup_in_cpp (ctx : context) field_object field =
  let member = field_name field in
  if is_internal_member member then false
  else if is_native_pointer field_object then false
  else if is_pointer field_object.etype true then false
  else if match field_object.eexpr with TTypeExpr _ -> true | _ -> false then
    false
  else if is_dynamic_in_cpp ctx field_object then true
  else if is_array field_object.etype then false
  else
    let tstr = type_string field_object.etype in
    match tstr with
    (* Internal classes have no dynamic members *)
    | "::String" | "Null" | "::hx::Class" | "::Enum" | "::Math"
    | "::ArrayAccess" ->
        false
    | "Dynamic" -> true
    | name ->
        let full_name = name ^ "." ^ member in
        if Hashtbl.mem ctx.ctx_class_member_types full_name then false
        else not (is_extern_class_instance field_object)

and is_dynamic_member_return_in_cpp ctx field_object field =
  let member = field_name field in
  if is_array field_object.etype then false
  else if is_pointer field_object.etype true then false
  else if is_internal_member member then false
  else
    match field_object.eexpr with
    | TTypeExpr t -> (
        let full_name =
          "::" ^ join_class_path_remap (t_path t) "::" ^ "." ^ member
        in
        try
          let mem_type = Hashtbl.find ctx.ctx_class_member_types full_name in
          mem_type = "Dynamic"
          || mem_type = "cpp::ArrayBase"
          || mem_type = "cpp::VirtualArray"
        with Not_found -> true)
    | _ -> (
        let tstr = type_string field_object.etype in
        match tstr with
        (* Internal classes have no dynamic members *)
        | "::String" | "Null" | "::hx::Class" | "::Enum" | "::Math"
        | "::ArrayAccess" ->
            false
        | "Dynamic" | "cpp::ArrayBase" | "cpp::VirtualArray" -> true
        | name -> (
            let full_name = name ^ "." ^ member in
            try
              let mem_type =
                Hashtbl.find ctx.ctx_class_member_types full_name
              in
              mem_type = "Dynamic"
              || mem_type = "cpp::ArrayBase"
              || mem_type = "cpp::VirtualArray"
            with Not_found -> true))

and is_dynamic_in_cppia ctx expr =
  match expr.eexpr with
  | TCast (_, None) -> true
  | _ -> is_dynamic_in_cpp ctx expr

class script_writer ctx filename asciiOut =
  object (this)
    val debug = asciiOut

    val doComment =
      asciiOut && Common.defined ctx.ctx_common Define.AnnotateSource

    val indent_str = if asciiOut then "\t" else ""
    val mutable indent = ""
    val mutable indents = []
    val mutable just_finished_block = false
    val mutable classCount = 0
    val mutable return_type = TMono (Monomorph.create ())
    val buffer = Buffer.create 0
    val identTable = Hashtbl.create 0
    val fileTable = Hashtbl.create 0
    val identBuffer = Buffer.create 0
    val cppiaAst = not (Common.defined ctx.ctx_common Define.NoCppiaAst)

    method stringId name =
      try Hashtbl.find identTable name
      with Not_found ->
        let size = Hashtbl.length identTable in
        Hashtbl.add identTable name size;
        Buffer.add_string identBuffer
          (string_of_int (String.length name) ^ " " ^ name ^ "\n");
        size

    method incClasses = classCount <- classCount + 1
    method stringText name = string_of_int (this#stringId name) ^ " "
    val typeTable = Hashtbl.create 0
    val typeBuffer = Buffer.create 0

    method typeId name =
      let name = if name = "::hx::Class" then "::Class" else name in
      try Hashtbl.find typeTable name
      with Not_found ->
        let size = Hashtbl.length typeTable in
        Hashtbl.add typeTable name size;
        Buffer.add_string typeBuffer
          (string_of_int (String.length name) ^ " " ^ name ^ "\n");
        size

    method write str =
      (if asciiOut then Buffer.add_string buffer str
       else
         let push i = Buffer.add_char buffer (Char.chr i) in
         let pushI32 i =
           push (Int32.to_int (Int32.logand i (Int32.of_int 255)))
         in
         List.iter
           (fun i ->
             if
               Int32.compare i Int32.zero >= 0
               && Int32.compare i (Int32.of_int 254) < 0
             then pushI32 i
             else if
               Int32.compare i Int32.zero >= 0
               && Int32.compare i (Int32.of_int 65536) < 0
             then (
               push 254;
               pushI32 i;
               pushI32 (Int32.shift_right i 8))
             else (
               push 255;
               pushI32 i;
               pushI32 (Int32.shift_right i 8);
               pushI32 (Int32.shift_right i 16);
               pushI32 (Int32.shift_right i 24)))
           (List.map Int32.of_string (Str.split (Str.regexp "[\n\t ]+") str)));
      just_finished_block <- false

    method comment text = if doComment then this#write ("# " ^ text ^ "\n")
    method commentOf text = if doComment then " # " ^ text else ""
    method typeTextString typeName = string_of_int (this#typeId typeName) ^ " "

    method typeText typeT =
      let tname =
        if cppiaAst then script_cpptype_string (cpp_type_of typeT)
        else script_type_string typeT
      in
      string_of_int (this#typeId tname) ^ " "

    method astType cppType =
      string_of_int (this#typeId (script_cpptype_string cppType)) ^ " "

    method writeType typeT = this#write (this#typeText typeT)

    method toCppType etype =
      string_of_int (this#typeId (script_cpptype_string (cpp_type_of etype)))
      ^ " "

    method boolText value = if value then "1" else "0"
    method writeBool value = this#write (if value then "1 " else "0 ")
    method staticText value = if value then "1" else "0"
    method writeData str = Buffer.add_string buffer str
    method wint ival = this#write (string_of_int ival ^ " ")
    method ident name = this#wint (this#stringId name)

    method cppInstText clazz =
      match clazz.cl_path with
      | [], "Array" -> this#typeTextString "Array"
      | x -> this#typeTextString (join_class_path x ".")

    method instText clazz =
      match clazz.cl_path with
      | [], "Array" -> string_of_int (this#typeId "Array< ::Dynamic >") ^ " "
      | _ -> this#typeText (TInst (clazz, []))

    method instName clazz =
      this#write
        (if cppiaAst then this#cppInstText clazz else this#instText clazz)

    method enumText e = this#typeText (TEnum (e, []))

    method close =
      let out_file = open_out_bin filename in
      output_string out_file (if asciiOut then "CPPIA\n" else "CPPIB\n");
      let idents = Buffer.contents identBuffer in
      output_string out_file (string_of_int (Hashtbl.length identTable) ^ "\n");
      output_string out_file idents;
      let types = Buffer.contents typeBuffer in
      output_string out_file (string_of_int (Hashtbl.length typeTable) ^ "\n");
      output_string out_file types;
      output_string out_file (string_of_int classCount ^ "\n");
      let contents = Buffer.contents buffer in
      output_string out_file contents;
      close_out out_file

    method fileId file =
      try Hashtbl.find fileTable file
      with Not_found ->
        let stripped_file = strip_file ctx.ctx_common file in
        let result = this#stringId stripped_file in
        Hashtbl.add fileTable file result;
        result

    method constText c =
      match c with
      | TInt i -> this#op IaConstInt ^ Printf.sprintf "%ld " i
      | TFloat f ->
          this#op IaConstFloat ^ this#stringText (Texpr.replace_separators f "")
      | TString s -> this#op IaConstString ^ this#stringText s
      | TBool true -> this#op IaConstTrue
      | TBool false -> this#op IaConstFalse
      | TNull -> this#op IaConstNull
      | TThis -> this#op IaConsThis
      | TSuper -> this#op IaConstSuper

    method get_array_type t =
      match follow t with
      | TInst ({ cl_path = [], "Array" }, [ param ]) -> (
          let typeName = type_string_suff "" param false in
          match typeName with
          | "::String" -> ArrayData "String"
          | "int" | "Float" | "bool" | "String" | "unsigned char"
          | "::cpp::UInt8" ->
              ArrayData typeName
          | "cpp::ArrayBase" | "cpp::VirtualArray" | "Dynamic" -> ArrayAny
          | _ when is_interface_type param ->
              ArrayInterface (this#typeId (script_type_string param))
          | _ -> ArrayObject)
      | TAbstract (abs, pl) when abs.a_impl <> None ->
          this#get_array_type (Abstract.get_underlying_type abs pl)
      | _ -> ArrayNone

    method pushReturn inType =
      let oldReturnType = return_type in
      return_type <- inType;
      fun () -> return_type <- oldReturnType

    method fileText file = string_of_int (this#fileId file)
    method indent_one = this#write indent_str

    method push_indent =
      indents <- indent_str :: indents;
      indent <- String.concat "" indents

    method pop_indent =
      match indents with
      | h :: tail ->
          indents <- tail;
          indent <- String.concat "" indents
      | [] -> indent <- "/*?*/"

    method write_i x = this#write (indent ^ x)
    method get_indent = indent
    method begin_expr = this#push_indent

    method end_expr =
      if not just_finished_block then this#write "\n";
      this#pop_indent;
      just_finished_block <- true

    method op x =
      match cppia_op_info x with
      | name, index -> (if debug then name else string_of_int index) ^ " "

    method writeOp o = this#write (this#op o)
    method writeOpLine o = this#write (this#op o ^ "\n")

    method voidFunc isStatic isDynamic funcName fieldExpression =
      this#comment funcName;
      this#write
        (this#op IaFunction ^ this#staticText isStatic ^ " "
       ^ this#boolText isDynamic ^ " " ^ this#stringText funcName ^ " ");
      this#write (this#typeTextString "Void" ^ "0\n");
      this#gen_expression fieldExpression

    method func isStatic isDynamic funcName ret args isInterface fieldExpression
        abstractPos =
      this#comment funcName;
      this#write
        (this#op IaFunction ^ this#staticText isStatic ^ " "
       ^ this#boolText isDynamic ^ " " ^ this#stringText funcName ^ " ");
      this#write (this#typeText ret ^ string_of_int (List.length args) ^ " ");
      List.iter
        (fun (name, opt, typ) ->
          this#write
            (this#stringText name ^ this#boolText opt ^ " " ^ this#typeText typ
           ^ " "))
        args;
      this#write "\n";
      if not isInterface then
        match fieldExpression with
        | Some ({ eexpr = TFunction function_def } as e) ->
            if cppiaAst then (
              let args = List.map fst function_def.tf_args in
              let cppExpr =
                CppRetyper.expression ctx TCppVoid args function_def.tf_type
                  function_def.tf_expr false
              in
              this#begin_expr;
              this#writePos function_def.tf_expr;
              this#write
                (this#op IaFun
                ^ this#typeText function_def.tf_type
                ^ string_of_int (List.length args)
                ^ "\n");
              let close = this#gen_func_args function_def.tf_args in
              this#gen_expression_tree cppExpr;
              this#end_expr;
              close ())
            else this#gen_expression e
        | _ ->
            (* Abstract function - dummp implementation that (should) not get called *)
            this#begin_expr;
            this#wpos abstractPos;
            this#writeOpLine IaReturn;
            this#end_expr

    method var readAcc writeAcc isExtern isStatic name varType varExpr =
      this#write
        (this#op IaVar ^ this#staticText isStatic ^ " " ^ this#op readAcc
       ^ this#op writeAcc ^ this#boolText isExtern ^ " " ^ this#stringText name
       ^ this#typeText varType
        ^ (match varExpr with Some _ -> "1" | _ -> "0")
        ^ if doComment then " # " ^ name ^ "\n" else "\n");
      match varExpr with
      | Some expression ->
          if cppiaAst then
            let varType = cpp_type_of expression.etype in
            let cppExpr =
              CppRetyper.expression ctx varType [] t_dynamic expression false
            in
            this#gen_expression_tree cppExpr
          else this#gen_expression expression
      | _ -> ()

    method implDynamic = this#writeOpLine IaImplDynamic

    method writeVar v =
      this#ident v.v_name;
      this#wint v.v_id;
      this#writeBool (has_var_flag v VCaptured);
      this#writeType v.v_type

    method writeList prefix len =
      this#write (prefix ^ " " ^ string_of_int len ^ "\n")

    method wpos p =
      if debug then
        this#write
          (this#fileText p.pfile ^ "\t"
          ^ string_of_int (Lexer.get_error_line p)
          ^ indent)

    method writePos expr = this#wpos expr.epos
    method writeCppPos expr = this#wpos expr.cpppos

    method checkCast toType expr forceCast fromGenExpression =
      let write_cast text =
        if not fromGenExpression then this#writePos expr;
        this#write (text ^ "\n");
        this#begin_expr;
        this#gen_expression expr;
        this#end_expr;
        true
      in
      let was_cast =
        if is_interface_type toType then
          if is_dynamic_in_cppia ctx expr then
            write_cast
              (this#op IaToInterface ^ this#typeText toType ^ " "
              ^ this#typeTextString "Dynamic")
          else if not (is_matching_interface_type toType expr.etype) then
            write_cast
              (this#op IaToInterface ^ this#typeText toType ^ " "
             ^ this#typeText expr.etype)
          else false
        else
          let get_array_expr_type expr =
            if is_dynamic_in_cppia ctx expr then ArrayNone
            else this#get_array_type expr.etype
          in
          match (this#get_array_type toType, get_array_expr_type expr) with
          | ArrayAny, _ -> false
          | ArrayObject, ArrayData _ -> write_cast (this#op IaToDynArray)
          | ArrayObject, ArrayObject -> false
          | ArrayObject, ArrayNone | ArrayObject, ArrayAny ->
              write_cast
                (this#op IaToDataArray ^ this#typeTextString "Array.Object")
          | ArrayData t, ArrayNone
          | ArrayData t, ArrayObject
          | ArrayData t, ArrayAny ->
              write_cast
                (this#op IaToDataArray ^ this#typeTextString ("Array." ^ t))
          | ArrayInterface t, ArrayNone | ArrayInterface t, ArrayAny ->
              write_cast (this#op IaToInterfaceArray ^ string_of_int t)
          | _, _ ->
              (* a0,a1 ->
                    let arrayString a =
                       match a with
                       | ArrayNone -> "ArrayNone"
                       | ArrayAny -> "ArrayAny"
                       | ArrayObject -> "ArrayObject"
                       | ArrayData _ -> "ArrayData"
                       | ArrayInterface _ -> "ArrayInterface"
                 in
                 this#write ("NOCAST " ^ (arrayString a0) ^ "=" ^ (arrayString a1)); *)
              false
      in

      if not was_cast then (
        (if forceCast then
           let op =
             match type_string expr.etype with
             | "int" -> IaCastInt
             | "bool" -> IaCastBool
             | _ when is_interface_type toType -> IaNoCast
             | _ -> IaCast
           in
           this#writeOpLine op);
        this#gen_expression expr)

    method gen_func_args args =
      let gen_inits = ref [] in
      List.iter
        (fun (arg, init) ->
          this#write (indent ^ indent_str);
          this#writeVar arg;
          match init with
          | Some { eexpr = TConst TNull } -> this#write "0\n"
          | Some const ->
              let argType = cpp_type_of const.etype in
              if is_cpp_scalar argType || argType == TCppString then (
                this#write "1 ";
                this#gen_expression_only const;
                this#write "\n")
              else (
                gen_inits := (arg, const) :: !gen_inits;
                this#write "0\n")
          | _ -> this#write "0\n")
        args;

      if List.length !gen_inits == 0 then fun () -> ()
      else (
        this#begin_expr;
        this#writePos (snd (List.hd !gen_inits));
        this#writeList (this#op IaBlock) (List.length !gen_inits + 1);
        List.iter
          (fun (arg, const) ->
            let start_expr () =
              this#begin_expr;
              this#writePos const
            in
            let local_var () =
              this#begin_expr;
              this#writePos const;
              this#write
                (this#op IaVar ^ string_of_int arg.v_id
               ^ this#commentOf arg.v_name);
              this#end_expr
            in

            start_expr ();
            this#writeOpLine IaIf;
            start_expr ();
            this#writeOpLine IaIsNull;
            local_var ();
            this#end_expr;
            start_expr ();
            this#writeOpLine IaSet;
            local_var ();
            this#gen_expression const;
            this#end_expr;
            this#begin_expr)
          !gen_inits;
        fun () -> this#end_expr)

    method gen_expression expr =
      this#begin_expr;
      this#writePos expr;
      this#gen_expression_only expr;
      this#end_expr

    method gen_expression_only expr =
      (* { *)
      let expression = remove_parens expr in
      match expression.eexpr with
      | TFunction function_def ->
          this#write
            (this#op IaFun
            ^ this#typeText function_def.tf_type
            ^ string_of_int (List.length function_def.tf_args)
            ^ "\n");
          let close = this#gen_func_args function_def.tf_args in
          let pop = this#pushReturn function_def.tf_type in
          this#gen_expression function_def.tf_expr;
          pop ();
          close ()
      | TBlock expr_list ->
          this#writeList (this#op IaBlock) (List.length expr_list);
          List.iter this#gen_expression expr_list
      | TConst const -> this#write (this#constText const)
      | TBreak -> this#writeOp IaBreak
      | TContinue -> this#writeOp IaContinue
      | TBinop (op, e1, e2) when op = OpAssign ->
          this#writeOpLine IaSet;
          this#gen_expression e1;
          this#checkCast e1.etype e2 false false
      | TBinop (OpEq, e1, { eexpr = TConst TNull }) ->
          this#writeOpLine IaIsNull;
          this#gen_expression e1
      | TBinop (OpNotEq, e1, { eexpr = TConst TNull }) ->
          this#writeOpLine IaNotNull;
          this#gen_expression e1
      | TBinop (OpEq, { eexpr = TConst TNull }, e1) ->
          this#writeOpLine IaIsNull;
          this#gen_expression e1
      | TBinop (OpNotEq, { eexpr = TConst TNull }, e1) ->
          this#writeOpLine IaNotNull;
          this#gen_expression e1
      | TBinop (op, e1, e2) ->
          this#writeOpLine (IaBinOp op);
          this#gen_expression e1;
          this#gen_expression e2
      | TThrow e ->
          this#writeOpLine IaThrow;
          this#gen_expression e
      | TArrayDecl expr_list ->
          this#write
            (this#op IaADef
            ^ this#typeText expression.etype
            ^ " "
            ^ string_of_int (List.length expr_list)
            ^ "\n");
          List.iter this#gen_expression expr_list
      | TIf (e, e1, e2) -> (
          match e2 with
          | None ->
              this#writeOpLine IaIf;
              this#gen_expression e;
              this#gen_expression e1
          | Some elze ->
              this#writeOpLine IaIfElse;
              this#gen_expression e;
              this#gen_expression e1;
              this#gen_expression elze)
      | TCall (func, arg_list) -> (
          let argN = string_of_int (List.length arg_list) ^ " " in
          let gen_call () =
            (match (remove_parens_cast func).eexpr with
            | TField ({ eexpr = TIdent "__global__" }, field) ->
                this#write
                  (this#op IaCallGlobal
                  ^ this#stringText (field_name field)
                  ^ argN
                  ^ this#commentOf (field_name field)
                  ^ "\n")
            | TField (obj, FStatic (class_def, field))
              when is_real_function field ->
                this#write
                  (this#op IaCallStatic ^ this#instText class_def ^ " "
                  ^ this#stringText field.cf_name
                  ^ argN
                  ^ this#commentOf
                      (join_class_path class_def.cl_path "."
                      ^ "." ^ field.cf_name)
                  ^ "\n")
            | TField (obj, FInstance (_, _, field))
              when is_this obj && is_real_function field ->
                this#write
                  (this#op IaCallThis ^ this#typeText obj.etype ^ " "
                  ^ this#stringText field.cf_name
                  ^ argN
                  ^ this#commentOf field.cf_name
                  ^ "\n")
            | TField (obj, FInstance (_, _, field)) when is_super obj ->
                this#write
                  (this#op IaCallSuper ^ this#typeText obj.etype ^ " "
                  ^ this#stringText field.cf_name
                  ^ argN
                  ^ this#commentOf field.cf_name
                  ^ "\n")
            (* Cppia does not have a "GetEnumIndex" op code - must use IaCallMember ::hx::EnumBase.__Index *)
            | TField (obj, FInstance (_, _, field))
              when field.cf_name = "_hx_getIndex"
                   && script_type_string obj.etype = "::hx::EnumBase" ->
                this#write
                  (this#op IaCallMember
                  ^ this#typeTextString "::hx::EnumBase"
                  ^ " " ^ this#stringText "__Index" ^ argN
                  ^ this#commentOf "Enum index"
                  ^ "\n");
                this#gen_expression obj
            | TField (obj, FInstance (_, _, field))
              when field.cf_name = "__Index"
                   || (not (is_dynamic_in_cppia ctx obj))
                      && is_real_function field ->
                this#write
                  (this#op IaCallMember ^ this#typeText obj.etype ^ " "
                  ^ this#stringText field.cf_name
                  ^ argN
                  ^ this#commentOf field.cf_name
                  ^ "\n");
                this#gen_expression obj
            | TField (obj, FDynamic name)
              when is_internal_member name
                   || (type_string obj.etype = "::String" && name = "cca") ->
                this#write
                  (this#op IaCallMember ^ this#typeText obj.etype ^ " "
                 ^ this#stringText name ^ argN ^ this#commentOf name ^ "\n");
                this#gen_expression obj
            | TConst TSuper ->
                this#write
                  (this#op IaCallSuperNew ^ this#typeText func.etype ^ " "
                 ^ argN ^ "\n")
            | TField (_, FEnum (enum, field)) ->
                this#write
                  (this#op IaCreateEnum ^ this#enumText enum ^ " "
                  ^ this#stringText field.ef_name
                  ^ argN
                  ^ this#commentOf field.ef_name
                  ^ "\n")
            | _ ->
                this#write (this#op IaCall ^ argN ^ "\n");
                this#gen_expression func);
            let matched_args =
              match func.etype with
              | TFun (args, _) -> (
                  try
                    List.iter2
                      (fun (_, _, protoT) arg ->
                        this#checkCast protoT arg false false)
                      args arg_list;
                    true
                  with Invalid_argument _ ->
                    (*print_endline "Bad count?";*) false)
              | _ -> false
            in
            if not matched_args then List.iter this#gen_expression arg_list
          in
          match (remove_parens_cast func).eexpr with
          | TField (obj, field)
            when is_array_or_dyn_array obj.etype && field_name field = "map"
            -> (
              match this#get_array_type expression.etype with
              | ArrayData t ->
                  this#write
                    (this#op IaToDataArray
                    ^ this#typeTextString ("Array." ^ t)
                    ^ "\n");
                  this#begin_expr;
                  this#writePos func;
                  gen_call ();
                  this#end_expr
              | ArrayInterface t ->
                  this#write
                    (this#op IaToInterfaceArray ^ string_of_int t ^ "\n");
                  this#begin_expr;
                  this#writePos func;
                  gen_call ();
                  this#end_expr
              | _ -> gen_call ())
          | _ -> gen_call ())
      | TField (obj, acc) -> (
          let objType =
            if is_dynamic_in_cppia ctx obj then "Dynamic"
            else script_type_string obj.etype
          in
          let typeText =
            if is_dynamic_in_cppia ctx obj then this#typeTextString "Dynamic"
            else this#typeText obj.etype
          in
          match acc with
          | FDynamic name ->
              this#write
                (this#op IaFName ^ typeText ^ " " ^ this#stringText name
               ^ this#commentOf name ^ "\n");
              this#gen_expression obj
          | FStatic (class_def, field) ->
              this#write
                (this#op IaFStatic ^ this#instText class_def ^ " "
                ^ this#stringText field.cf_name
                ^ this#commentOf field.cf_name)
          | FInstance (_, _, field) when is_this obj ->
              this#write
                (this#op IaFThisInst ^ typeText ^ " "
                ^ this#stringText field.cf_name
                ^ this#commentOf field.cf_name)
          | FInstance (_, _, field) ->
              this#write
                (this#op IaFLink ^ typeText ^ " "
                ^ this#stringText field.cf_name
                ^ this#commentOf (objType ^ "." ^ field.cf_name)
                ^ "\n");
              this#gen_expression obj
          | FClosure (_, field) when is_this obj ->
              this#write
                (this#op IaFThisName ^ typeText ^ " "
                ^ this#stringText field.cf_name
                ^ "\n")
          | FAnon field when is_this obj ->
              this#write
                (this#op IaFThisName ^ typeText ^ " "
                ^ this#stringText field.cf_name
                ^ this#commentOf field.cf_name
                ^ "\n")
          | FClosure (_, field) | FAnon field ->
              this#write
                (this#op IaFName ^ typeText ^ " "
                ^ this#stringText field.cf_name
                ^ this#commentOf field.cf_name
                ^ "\n");
              this#gen_expression obj
          | FEnum (enum, field) ->
              this#write
                (this#op IaFEnum ^ this#enumText enum ^ " "
                ^ this#stringText field.ef_name
                ^ this#commentOf field.ef_name))
      | TArray (e1, e2) ->
          this#write (this#op IaArrayI ^ this#typeText e1.etype ^ "\n");
          this#gen_expression e1;
          this#gen_expression e2
      | TUnop (op, flag, e) ->
          this#writeOpLine
            (match (op, flag) with
            | Increment, Prefix -> IaPlusPlus
            | Increment, _ -> IaPlusPlusPost
            | Decrement, Prefix -> IaMinusMinus
            | Decrement, _ -> IaMinusMinusPost
            | Not, _ -> IaLogicNot
            | Neg, _ -> IaNeg
            | Spread, _ -> die ~p:e.epos "Unexpected spread operator" __LOC__
            | NegBits, _ -> IaBitNot);
          this#gen_expression e
      (* TODO - lval op-assign local/member/array *)
      | TLocal var ->
          this#write
            (this#op IaVar ^ string_of_int var.v_id ^ this#commentOf var.v_name)
      | TVar (tvar, optional_init) -> (
          this#write
            (this#op IaTVars ^ string_of_int 1
            ^ this#commentOf (tvar.v_name ^ ":" ^ script_type_string tvar.v_type)
            ^ "\n");
          this#write ("\t\t" ^ indent);
          match optional_init with
          | None ->
              this#writeOp IaVarDecl;
              this#writeVar tvar
          | Some init ->
              this#writeOp IaVarDeclI;
              let init = remove_parens init in
              this#writeVar tvar;
              this#write (" " ^ this#typeText init.etype);
              this#write "\n";
              this#checkCast tvar.v_type init false false)
      | TNew (clazz, params, arg_list) -> (
          this#write
            (this#op IaNew
            ^ this#typeText (TInst (clazz, params))
            ^ string_of_int (List.length arg_list)
            ^ "\n");
          try
            match
              OverloadResolution.maybe_resolve_constructor_overload clazz params
                arg_list
            with
            | Some (_, { cf_type = TFun (args, _) }, _) ->
                List.iter2
                  (fun (_, _, protoT) arg ->
                    this#checkCast protoT arg false false)
                  args arg_list
            | _ -> raise (Invalid_argument "")
          with Invalid_argument _ -> List.iter this#gen_expression arg_list)
      | TReturn optval -> (
          match optval with
          | None -> this#writeOpLine IaReturn
          | Some value ->
              this#write (this#op IaRetVal ^ this#typeText value.etype ^ "\n");
              this#checkCast return_type value false false)
      | TObjectDecl
          [
            (("fileName", _, _), { eexpr = TConst (TString file) });
            (("lineNumber", _, _), { eexpr = TConst (TInt line) });
            (("className", _, _), { eexpr = TConst (TString class_name) });
            (("methodName", _, _), { eexpr = TConst (TString meth) });
          ] ->
          this#write
            (this#op IaPosInfo ^ this#stringText file
           ^ Printf.sprintf "%ld" line ^ " " ^ this#stringText class_name ^ " "
           ^ this#stringText meth)
      | TObjectDecl values ->
          this#write (this#op IaObjDef ^ string_of_int (List.length values));
          this#write " ";
          List.iter
            (fun ((name, _, _), _) -> this#write (this#stringText name))
            values;
          this#write "\n";
          List.iter (fun (_, e) -> this#gen_expression e) values
      | TTypeExpr type_expr ->
          let klass = "::" ^ join_class_path (t_path type_expr) "::" in
          this#write (this#op IaClassOf ^ string_of_int (this#typeId klass))
      | TWhile (e1, e2, flag) ->
          this#write
            (this#op IaWhile ^ (if flag = NormalWhile then "1" else "0") ^ "\n");
          this#gen_expression e1;
          this#gen_expression e2
      | TFor (tvar, init, loop) ->
          this#writeOp IaFor;
          this#writeVar tvar;
          this#write "\n";
          this#gen_expression init;
          this#gen_expression loop
      | TEnumParameter (expr, ef, i) ->
          let enum =
            match follow ef.ef_type with
            | TEnum (en, _) | TFun (_, TEnum (en, _)) -> en
            | _ -> die "" __LOC__
          in
          this#write
            (this#op IaEnumI
            ^ this#typeText (TEnum (enum, []))
            ^ string_of_int i ^ "\n");
          this#gen_expression expr
      | TEnumIndex expr ->
          this#write
            (this#op IaCallMember
            ^ this#typeTextString "::hx::EnumBase"
            ^ " " ^ this#stringText "__Index" ^ "0"
            ^ this#commentOf "Enum index"
            ^ "\n");
          this#gen_expression expr
      | TSwitch
          {
            switch_subject = condition;
            switch_cases = cases;
            switch_default = optional_default;
          } -> (
          this#write
            (this#op IaSwitch
            ^ string_of_int (List.length cases)
            ^ " "
            ^ (match optional_default with None -> "0" | Some _ -> "1")
            ^ "\n");
          this#gen_expression condition;
          List.iter
            (fun { case_patterns = cases_list; case_expr = expression } ->
              this#writeList ("\t\t\t" ^ indent) (List.length cases_list);
              List.iter (fun value -> this#gen_expression value) cases_list;
              this#gen_expression expression)
            cases;
          match optional_default with
          | None -> ()
          | Some expr -> this#gen_expression expr)
      | TTry (e, catches) ->
          this#writeList (this#op IaTry) (List.length catches);
          this#gen_expression e;
          List.iter
            (fun (tvar, catch_expr) ->
              this#write ("\t\t\t" ^ indent);
              this#writeVar tvar;
              this#write "\n";
              this#gen_expression catch_expr)
            catches
      | TCast (cast, Some (TClassDecl t)) ->
          this#write (this#op IaTCast ^ this#typeText (TInst (t, [])) ^ "\n");
          this#gen_expression cast
      | TCast (cast, _) -> this#checkCast expression.etype cast true true
      | TParenthesis _ -> abort "Unexpected parens" expression.epos
      | TMeta (_, _) -> abort "Unexpected meta" expression.epos
      | TIdent _ -> abort "Unexpected ident" expression.epos

    (* } *)
    method gen_expression_tree expression_tree =
      (* { *)
      let rec gen_expression expression =
        this#begin_expr;
        this#writeCppPos expression;
        let rec match_expr expression =
          match expression.cppexpr with
          | CppBlock (exprs, closures, _) ->
              this#writeList (this#op IaBlock) (List.length exprs);
              List.iter gen_expression exprs
          | CppVarDecl (var, init) -> (
              let name = CppGen.cpp_var_name_of var in
              this#write
                (this#op IaTVars ^ string_of_int 1
                ^ this#commentOf (name ^ ":" ^ script_type_string var.v_type)
                ^ "\n");
              this#write ("\t\t" ^ indent);
              match init with
              | None ->
                  this#writeOp IaVarDecl;
                  this#writeVar var
              | Some init ->
                  this#writeOp IaVarDeclI;
                  this#writeVar var;
                  this#write (" " ^ this#astType init.cpptype);
                  this#write "\n";
                  gen_expression init)
          | CppInt i -> this#write (this#op IaConstInt ^ Printf.sprintf "%ld " i)
          | CppFloat float_as_string ->
              this#write (this#op IaConstFloat ^ this#stringText float_as_string)
          | CppString s -> this#write (this#op IaConstString ^ this#stringText s)
          | CppBool false -> this#writeOp IaConstFalse
          | CppBool true -> this#writeOp IaConstTrue
          | CppNull -> this#writeOp IaConstNull
          | CppNil -> abort "Nil not supported in cppia" expression.cpppos
          | CppThis _ -> this#writeOp IaConsThis
          | CppSuper _ -> this#writeOp IaConstSuper
          | CppBreak -> this#writeOp IaBreak
          | CppContinue -> this#writeOp IaContinue
          | CppGoto label ->
              abort "Goto not supported in cppia" expression.cpppos
          | CppReturn None -> this#writeOpLine IaReturn
          | CppReturn (Some value) ->
              this#write (this#op IaRetVal ^ this#astType value.cpptype ^ "\n");
              gen_expression value
          | CppWhile (condition, block, while_flag, _) ->
              this#write
                (this#op IaWhile
                ^ (if while_flag = NormalWhile then "1" else "0")
                ^ "\n");
              gen_expression condition;
              gen_expression block
          | CppIf (condition, block, None) ->
              this#writeOpLine IaIf;
              gen_expression condition;
              gen_expression block
          | CppIf (condition, block, Some elze) ->
              this#writeOpLine IaIfElse;
              gen_expression condition;
              gen_expression block;
              gen_expression elze
          | CppBinop (op, left, right) ->
              this#writeOpLine (IaBinOp op);
              gen_expression left;
              gen_expression right
          | CppVar var -> gen_var_loc var
          | CppExtern (name, _) ->
              abort
                ("Unexpected global '" ^ name ^ "' in cppia")
                expression.cpppos
          | CppSet (lvalue, rvalue) ->
              this#writeOpLine IaSet;
              gen_lvalue lvalue expression.cpppos;
              gen_expression rvalue
          | CppCall (func, args) ->
              let argN = string_of_int (List.length args) ^ " " in
              (match func with
              | FuncThis (field, inst) ->
                  let name = field.cf_name in
                  this#write
                    (this#op IaCallThis ^ this#astType inst ^ " "
                   ^ this#stringText name ^ argN ^ this#commentOf name ^ "\n")
              | FuncInstance (expr, _, field) | FuncInterface (expr, _, field)
                ->
                  this#write
                    (this#op IaCallMember ^ this#astType expr.cpptype ^ " "
                    ^ this#stringText field.cf_name
                    ^ argN
                    ^ this#commentOf field.cf_name
                    ^ "\n");
                  gen_expression expr
              | FuncStatic (class_def, _, field) ->
                  this#write
                    (this#op IaCallStatic ^ this#cppInstText class_def ^ " "
                    ^ this#stringText field.cf_name
                    ^ argN
                    ^ this#commentOf
                        (join_class_path class_def.cl_path "."
                        ^ "." ^ field.cf_name)
                    ^ "\n")
              | FuncTemplate _ ->
                  abort "Templated function call not supported in cppia"
                    expression.cpppos
              | FuncFromStaticFunction ->
                  abort "Unexpected FuncFromStaticFunction" expression.cpppos
              | FuncEnumConstruct (enum, field) ->
                  this#write
                    (this#op IaCreateEnum ^ this#enumText enum ^ " "
                    ^ this#stringText field.ef_name
                    ^ argN
                    ^ this#commentOf field.ef_name
                    ^ "\n")
              | FuncSuperConstruct (TCppInst (klass, _))
                when is_native_gen_class klass && is_native_class klass ->
                  abort "Unsupported super for native class constructor"
                    expression.cpppos
              | FuncSuperConstruct childType ->
                  this#write
                    (this#op IaCallSuperNew ^ this#astType childType ^ " "
                   ^ argN ^ "\n")
              | FuncSuper (_, TCppInst (klass, _), _)
                when is_native_gen_class klass && is_native_class klass ->
                  abort "Unsupported super for native class method"
                    expression.cpppos
              | FuncSuper (_, objType, field) ->
                  this#write
                    (this#op IaCallSuper ^ this#astType objType ^ " "
                    ^ this#stringText field.cf_name
                    ^ argN
                    ^ this#commentOf field.cf_name
                    ^ "\n")
              | FuncExtern (name, _) ->
                  this#write
                    (this#op IaCallGlobal ^ this#stringText name ^ argN
                   ^ this#commentOf name ^ "\n")
              | FuncNew newType ->
                  this#write (this#op IaNew ^ this#astType newType ^ argN ^ "\n")
              | FuncInternal (obj, "cca", ".") when obj.cpptype = TCppString ->
                  this#write
                    (this#op IaCallMember ^ this#astType obj.cpptype ^ " "
                   ^ this#stringText "cca" ^ argN ^ this#commentOf "cca" ^ "\n"
                    );
                  gen_expression obj
              | FuncInternal (obj, name, join) ->
                  (* abort ("Internal function call '" ^ name ^ "' not supported in cppia") expression.cpppos; *)
                  this#write
                    (this#op IaCallMember ^ this#astType obj.cpptype ^ " "
                   ^ this#stringText name ^ argN ^ this#commentOf name ^ "\n");
                  gen_expression obj
              | FuncExpression expr ->
                  this#write (this#op IaCall ^ argN ^ "\n");
                  gen_expression expr);
              List.iter gen_expression args
          | CppFunction (func, _) -> (
              match func with
              | FuncThis (field, inst) ->
                  this#write
                    (this#op IaFThisName ^ this#astType inst ^ " "
                    ^ this#stringText field.cf_name
                    ^ this#commentOf
                        (script_cpptype_string inst ^ "." ^ field.cf_name))
              | FuncInternal (expr, name, _) ->
                  this#write
                    (this#op IaFLink ^ this#astType expr.cpptype ^ " "
                   ^ this#stringText name
                    ^ this#commentOf
                        ("Internal "
                        ^ script_cpptype_string expr.cpptype
                        ^ "." ^ name)
                    ^ "\n");
                  gen_expression expr
              | FuncInstance (expr, _, field) | FuncInterface (expr, _, field)
                ->
                  this#write
                    (this#op IaFName ^ this#astType expr.cpptype ^ " "
                    ^ this#stringText field.cf_name
                    ^ this#commentOf
                        (script_cpptype_string expr.cpptype
                        ^ "." ^ field.cf_name)
                    ^ "\n");
                  gen_expression expr
              | FuncStatic (class_def, _, field) ->
                  this#write
                    (this#op IaFStatic ^ this#cppInstText class_def ^ " "
                    ^ this#stringText field.cf_name
                    ^ this#commentOf field.cf_name)
              | FuncExpression expr -> match_expr expr
              | FuncExtern (name, _) ->
                  abort
                    ("Can't create extern " ^ name ^ " closure")
                    expression.cpppos
              | FuncSuper _ | FuncSuperConstruct _ ->
                  abort "Can't create super closure" expression.cpppos
              | FuncNew _ -> abort "Can't create new closure" expression.cpppos
              | FuncEnumConstruct _ ->
                  abort "Enum constructor outside of CppCall" expression.cpppos
              | FuncFromStaticFunction ->
                  abort "Can't create cpp.Function.fromStaticFunction closure"
                    expression.cpppos
              | FuncTemplate _ ->
                  abort "Can't create template function closure"
                    expression.cpppos)
          | CppPosition (file, line, class_name, meth) ->
              this#write
                (this#op IaPosInfo ^ this#stringText file
               ^ Printf.sprintf "%ld" line ^ " " ^ this#stringText class_name
               ^ " " ^ this#stringText meth)
          | CppNullCompare ("IsNull", e) ->
              this#writeOpLine IaIsNull;
              gen_expression e
          | CppNullCompare (_, e) ->
              this#writeOpLine IaNotNull;
              gen_expression e
          | CppCompare (_, left, right, op) ->
              this#writeOpLine (IaBinOp op);
              gen_expression left;
              gen_expression right
          | CppArray arrayLoc -> gen_array arrayLoc expression.cpppos
          | CppArrayDecl exprList ->
              this#write
                (this#op IaADef
                ^ this#astType expression.cpptype
                ^ " "
                ^ string_of_int (List.length exprList)
                ^ "\n");
              List.iter gen_expression exprList
          | CppEnumField (enum, field) ->
              this#write
                (this#op IaFEnum ^ this#enumText enum ^ " "
                ^ this#stringText field.ef_name
                ^ this#commentOf field.ef_name)
          | CppEnumIndex obj ->
              (* Cppia does not have a "GetEnumIndex" op code - must use IaCallMember ::hx::EnumBase.__Index *)
              this#write
                (this#op IaCallMember
                ^ this#typeTextString "::hx::EnumBase"
                ^ " " ^ this#stringText "__Index" ^ "0"
                ^ this#commentOf "Enum index"
                ^ "\n");
              gen_expression obj
          | CppDynamicField (obj, name) ->
              this#write
                (this#op IaFName
                ^ this#typeTextString "Dynamic"
                ^ " " ^ this#stringText name ^ this#commentOf name ^ "\n");
              gen_expression obj
          | CppClassOf (path, native) ->
              let klass = join_class_path path "." in
              this#write
                (this#op IaClassOf ^ this#typeTextString klass
               ^ this#commentOf klass)
          | CppEnumParameter (obj, field, index) ->
              this#write
                (this#op IaEnumI
                ^ this#typeTextString "Dynamic"
                ^ string_of_int index ^ "\n");
              gen_expression obj
          | CppClosure closure ->
              this#write
                (this#op IaFun
                ^ this#astType closure.close_type
                ^ string_of_int (List.length closure.close_args)
                ^ "\n");
              let close = this#gen_func_args closure.close_args in
              gen_expression closure.close_expr;
              close ()
          | CppObjectDecl (values, isStruct) ->
              this#write (this#op IaObjDef ^ string_of_int (List.length values));
              this#write " ";
              List.iter
                (fun (name, _) -> this#write (this#stringText name))
                values;
              this#write "\n";
              List.iter (fun (_, e) -> gen_expression e) values
          | CppCrement (incFlag, preFlag, lvalue) ->
              let op =
                match (incFlag, preFlag) with
                | CppIncrement, Prefix -> IaPlusPlus
                | CppIncrement, Postfix -> IaPlusPlusPost
                | CppDecrement, Prefix -> IaMinusMinus
                | CppDecrement, Postfix -> IaMinusMinusPost
              in
              this#writeOpLine op;
              gen_lvalue lvalue expression.cpppos
          | CppModify (op, lvalue, rvalue) ->
              this#writeOpLine (IaBinOp (OpAssignOp op));
              gen_lvalue lvalue expression.cpppos;
              gen_expression rvalue
          | CppUnop (op, expr) ->
              let op =
                match op with
                | CppNot -> IaLogicNot
                | CppNeg -> IaNeg
                | CppNegBits -> IaBitNot
              in
              this#writeOpLine op;
              gen_expression expr
          | CppThrow value ->
              this#writeOpLine IaThrow;
              gen_expression value
          | CppTry (block, catches) ->
              this#writeList (this#op IaTry) (List.length catches);
              gen_expression block;
              List.iter
                (fun (tvar, catch_expr) ->
                  this#write ("\t\t\t" ^ indent);
                  this#writeVar tvar;
                  this#write "\n";
                  gen_expression catch_expr)
                catches
          | CppIntSwitch _ ->
              abort "CppIntSwitch not supported in cppia" expression.cpppos
          | CppSwitch (condition, _, cases, optional_default, _) -> (
              this#write
                (this#op IaSwitch
                ^ string_of_int (List.length cases)
                ^ " "
                ^ (match optional_default with None -> "0" | Some _ -> "1")
                ^ "\n");
              gen_expression condition;
              List.iter
                (fun (cases_list, expression) ->
                  this#writeList ("\t\t\t" ^ indent) (List.length cases_list);
                  List.iter (fun value -> gen_expression value) cases_list;
                  gen_expression expression)
                cases;
              match optional_default with
              | None -> ()
              | Some expr -> gen_expression expr)
          | CppTCast (expr, toType) ->
              this#write (this#op IaTCast ^ this#astType toType ^ "\n");
              gen_expression expr
          | CppCast (expr, toType) -> (
              match toType with
              | TCppDynamicArray ->
                  this#write (this#op IaToDynArray ^ "\n");
                  gen_expression expr
              | TCppObjectArray _ ->
                  this#write
                    (this#op IaToDataArray
                    ^ this#typeTextString "Array.Object"
                    ^ "\n");
                  gen_expression expr
              | TCppScalarArray t ->
                  this#write
                    (this#op IaToDataArray
                    ^ this#typeTextString ("Array." ^ script_cpptype_string t)
                    ^ "\n");
                  gen_expression expr
              | _ -> match_expr expr)
          | CppCastScalar (expr, "bool") ->
              this#writeOpLine IaCastBool;
              gen_expression expr
          | CppCastScalar (expr, "int") ->
              this#writeOpLine IaCastInt;
              gen_expression expr
          | CppCastScalar (expr, "Float") ->
              this#write
                (this#op IaTCast ^ this#astType (TCppScalar "Float") ^ "\n");
              gen_expression expr
          | CppCastScalar (expr, _) -> match_expr expr
          | CppCastVariant expr -> match_expr expr
          | CppCastStatic (expr, _) -> match_expr expr
          | CppNullAccess ->
              this#writeOpLine IaThrow;
              this#begin_expr;
              this#writeCppPos expression;
              this#write (this#op IaConstString ^ this#stringText "Null access");
              this#end_expr
          | CppCode _ | CppFunctionAddress _ | CppNewNative _ | CppDereference _
          | CppAddressOf _ | CppFor _ | CppCastObjC _ | CppCastObjCBlock _
          | CppCastProtocol _ | CppCastNative _ ->
              abort
                ("Unsupported operation in cppia :" ^ s_tcpp expression.cppexpr)
                expression.cpppos
          (*| x -> print_endline ("Unknown cppexpr " ^ (s_tcpp x) );*)
        in

        match_expr expression;
        this#end_expr
      and gen_array arrayLoc pos =
        match arrayLoc with
        | ArrayObject (arrayObj, index, _) | ArrayTyped (arrayObj, index, _) ->
            this#write (this#op IaArrayI ^ this#astType arrayObj.cpptype ^ "\n");
            gen_expression arrayObj;
            gen_expression index
        | ArrayPointer (_, _) | ArrayRawPointer (_, _) ->
            abort "Unvalid array access in cppia" pos
        | ArrayVirtual (arrayObj, index)
        | ArrayImplements (_, arrayObj, index)
        | ArrayDynamic (arrayObj, index) ->
            this#write (this#op IaArrayI ^ this#astType arrayObj.cpptype ^ "\n");
            gen_expression arrayObj;
            gen_expression index
      and gen_lvalue lvalue pos =
        this#begin_expr;
        this#wpos pos;
        (match lvalue with
        | CppVarRef varLoc -> gen_var_loc varLoc
        | CppArrayRef arrayLoc -> gen_array arrayLoc pos
        | CppExternRef (name, _) ->
            abort ("Unsupported extern '" ^ name ^ "' in cppia") pos
        | CppDynamicRef (expr, name) ->
            let typeText = this#typeTextString "Dynamic" in
            this#write
              (this#op IaFName ^ typeText ^ " " ^ this#stringText name
             ^ this#commentOf name ^ "\n");
            gen_expression expr);
        this#end_expr
      and gen_var_loc loc =
        match loc with
        | VarClosure var | VarLocal var ->
            this#write
              (this#op IaVar ^ string_of_int var.v_id
             ^ this#commentOf var.v_name)
        | VarStatic (class_def, _, field) ->
            this#write
              (this#op IaFStatic ^ this#cppInstText class_def ^ " "
              ^ this#stringText field.cf_name
              ^ this#commentOf field.cf_name)
        | VarThis (field, thisType) ->
            this#write
              (this#op IaFThisInst ^ this#astType thisType ^ " "
              ^ this#stringText field.cf_name
              ^ this#commentOf field.cf_name)
        | VarInstance (obj, field, _, _) | VarInterface (obj, field) ->
            let objType = script_cpptype_string obj.cpptype in
            this#write
              (this#op IaFLink ^ this#astType obj.cpptype ^ " "
              ^ this#stringText field.cf_name
              ^ this#commentOf (objType ^ "." ^ field.cf_name)
              ^ "\n");
            gen_expression obj
        | VarInternal (obj, _, name) ->
            let objType = script_cpptype_string obj.cpptype in
            this#write
              (this#op IaFLink ^ this#astType obj.cpptype ^ " "
             ^ this#stringText name
              ^ this#commentOf (objType ^ "." ^ name)
              ^ "\n");
            gen_expression obj
        (*
      and get_array_type elem =
         this#stringText (script_cpptype_string elem.cpptype);
      *)
      in
      gen_expression expression_tree
  end

let generate_script_class common_ctx script class_def =
  script#incClasses;
  let classText = join_class_path class_def.cl_path "." in
  script#comment ("Class " ^ classText);
  script#writeOp
    (if has_class_flag class_def CInterface then IaInterface else IaClass);
  script#instName class_def;
  (match class_def.cl_super with
  | None -> script#ident ""
  | Some (c, _) -> script#instName c);
  script#wint (List.length class_def.cl_implements);
  List.iter (fun (c, _) -> script#instName c) class_def.cl_implements;
  script#write "\n";
  (* Looks like some map impl classes have their bodies discarded - not sure best way to filter *)
  let non_dodgy_function allow_empty field =
    has_class_flag class_def CInterface
    ||
    match (field.cf_kind, field.cf_expr) with
    | Var _, _ -> true
    | Method MethDynamic, _ -> true
    | Method MethNormal, None when allow_empty -> true
    | Method _, Some _ -> true
    | _ -> false
  in
  let ordered_statics =
    List.filter (non_dodgy_function false) class_def.cl_ordered_statics
  in
  let ordered_fields =
    List.filter (non_dodgy_function true) class_def.cl_ordered_fields
  in
  script#write
    (string_of_int
       (List.length ordered_fields
       + List.length ordered_statics
       + (match class_def.cl_constructor with Some _ -> 1 | _ -> 0)
       + match TClass.get_cl_init class_def with Some _ -> 1 | _ -> 0)
    ^ "\n");

  let generate_field isStatic field =
    match (field.cf_kind, follow field.cf_type) with
    | Var { v_read = AccInline; v_write = AccNever }, _ ->
        script#writeOpLine IaInline
    | Var v, _ ->
        let mode_code mode =
          match mode with
          | AccNormal | AccCtor -> IaAccessNormal
          | AccNo -> IaAccessNot
          | AccNever -> IaAccessNot
          | AccCall ->
              if
                Meta.has Meta.NativeProperty class_def.cl_meta
                || Meta.has Meta.NativeProperty field.cf_meta
                || Common.defined common_ctx Define.ForceNativeProperty
              then IaAccessCallNative
              else IaAccessCall
          | AccInline -> IaAccessNormal
          | AccRequire (_, _) -> IaAccessNormal
        in
        let isExtern = not (is_physical_field field) in
        script#var (mode_code v.v_read) (mode_code v.v_write) isExtern isStatic
          field.cf_name field.cf_type field.cf_expr
    | Method MethDynamic, TFun (args, ret) ->
        script#func isStatic true field.cf_name ret args
          (has_class_flag class_def CInterface)
          field.cf_expr field.cf_pos
    | Method _, TFun (args, ret) when field.cf_name = "new" ->
        script#func true false "new"
          (TInst (class_def, []))
          args false field.cf_expr field.cf_pos
    | Method _, TFun (args, ret) ->
        script#func isStatic false field.cf_name ret args
          (has_class_flag class_def CInterface)
          field.cf_expr field.cf_pos
    | Method _, _ ->
        print_endline
          ("Unknown method type "
          ^ join_class_path class_def.cl_path "."
          ^ "." ^ field.cf_name)
  in
  (match class_def.cl_constructor with
  | Some field -> generate_field true field
  | _ -> ());
  (match TClass.get_cl_init class_def with
  | Some expression -> script#voidFunc true false "__init__" expression
  | _ -> ());

  List.iter (generate_field false) ordered_fields;
  List.iter (generate_field true) ordered_statics;
  script#write "\n"

let generate_script_enum script enum_def meta =
  script#incClasses;
  let sorted_items =
    List.sort
      (fun f1 f2 -> f1.ef_index - f2.ef_index)
      (pmap_values enum_def.e_constrs)
  in
  script#writeList
    (script#op IaEnum ^ script#enumText enum_def)
    (List.length sorted_items);

  List.iter
    (fun constructor ->
      let name = script#stringText constructor.ef_name in
      match constructor.ef_type with
      | TFun (args, _) ->
          script#write (name ^ " " ^ string_of_int (List.length args));
          List.iter
            (fun (arg, _, t) ->
              script#write
                (" " ^ script#stringText arg ^ " " ^ script#typeText t))
            args;
          script#write "\n"
      | _ -> script#write (name ^ " 0\n"))
    sorted_items;

  match meta with
  | Some expr ->
      script#write "1\n";
      script#gen_expression expr
  | _ ->
      script#write "0\n";
      script#write "\n"

let generate_cppia ctx =
  let common_ctx = ctx.ctx_common in
  let debug = ctx.ctx_debug_level in
  Path.mkdir_from_path common_ctx.file;
  let script = new script_writer ctx common_ctx.file common_ctx.debug in
  ignore (script#stringId "");
  ignore (script#typeId "");

  List.iter
    (fun object_def ->
      match object_def with
      | TClassDecl class_def when has_class_flag class_def CExtern ->
          () (*if (gen_externs) then gen_extern_class common_ctx class_def;*)
      | TClassDecl class_def ->
          let is_internal = is_internal_class class_def.cl_path in
          if is_internal || Meta.has Meta.Macro class_def.cl_meta then (
            if debug >= 4 then
              print_endline
                (" internal class " ^ join_class_path class_def.cl_path "."))
          else generate_script_class common_ctx script class_def
      | TEnumDecl enum_def when has_enum_flag enum_def EnExtern -> ()
      | TEnumDecl enum_def ->
          let is_internal = is_internal_class enum_def.e_path in
          if is_internal then (
            if debug >= 4 then
              print_endline
                (" internal enum " ^ join_class_path enum_def.e_path "."))
          else
            let meta = Texpr.build_metadata common_ctx.basic object_def in
            if has_enum_flag enum_def EnExtern then
              if debug >= 4 then
                print_endline
                  ("external enum " ^ join_class_path enum_def.e_path ".");
            generate_script_enum script enum_def meta
      | TTypeDecl _ | TAbstractDecl _ -> (* already done *) ())
    common_ctx.types;

  (match common_ctx.main.main_expr with
  | None -> script#writeOpLine IaNoMain
  | Some e ->
      script#writeOpLine IaMain;
      script#gen_expression e);

  script#write
    (script#op IaResources
    ^ string_of_int (Hashtbl.length common_ctx.resources)
    ^ "\n");
  Hashtbl.iter
    (fun name data ->
      script#write
        (script#op IaReso ^ script#stringText name
        ^ string_of_int (String.length data)
        ^ "\n"))
    common_ctx.resources;
  Hashtbl.iter (fun _ data -> script#writeData data) common_ctx.resources;

  script#close
