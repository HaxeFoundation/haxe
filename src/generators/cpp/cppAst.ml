open Type
open Globals

module PathMap = Map.Make(struct type t = path let compare i1 i2 = String.compare (s_type_path i2) (s_type_path i1) end)

module ObjectIds = struct
  type t = {
    ids : int32 PathMap.t;
    cache : unit Int32Map.t;
  }

  let empty = { ids = PathMap.empty; cache = Int32Map.empty }

  let add path id store =
    { ids = PathMap.add path id store.ids; cache = Int32Map.add id () store.cache }

  let find_opt path store =
    PathMap.find_opt path store.ids

  let collision id store =
    Int32Map.mem id store.cache
end

module InterfaceSlots = struct
  type t = {
    hash : int StringMap.t;
    next : int;
  }

  let empty = { hash = StringMap.empty; next = 2 }

  let add name slots =
    match StringMap.find_opt name slots.hash with
    | Some slot ->
      slots
    | None ->
      { hash = StringMap.add name slots.next slots.hash; next = slots.next + 1 }

  let find_opt name slots =
    StringMap.find_opt name slots.hash
end

type tcpp =
  | TCppDynamic
  | TCppUnchanged
  | TCppObject
  | TCppObjectPtr
  | TCppVoid
  | TCppNull
  | TCppEnum of tenum
  | TCppScalar of string
  | TCppString
  | TCppFastIterator of tcpp
  | TCppPointer of string * tcpp
  | TCppRawPointer of string * tcpp
  | TCppFunction of tcpp list * tcpp * string
  | TCppObjCBlock of tcpp list * tcpp
  | TCppRest of tcpp
  | TCppReference of tcpp
  | TCppStruct of tcpp
  | TCppStar of tcpp * bool
  | TCppVoidStar
  | TCppVarArg
  | TCppAutoCast
  | TCppDynamicArray
  | TCppObjectArray of tcpp
  | TCppWrapped of tcpp
  | TCppScalarArray of tcpp
  | TCppObjC of tclass
  | TCppNativePointer of tclass
  | TCppVariant
  | TCppCode of tcpp
  | TCppInst of tclass * tcpp list
  | TCppInterface of tclass
  | TCppProtocol of tclass
  | TCppClass
  | TCppGlobal

and tcppexpr = { cppexpr : tcpp_expr_expr; cpptype : tcpp; cpppos : pos }

and tcpp_closure = {
  close_type : tcpp;
  close_args : (tvar * texpr option) list;
  close_expr : tcppexpr;
  close_id : int;
  close_undeclared : tvar StringMap.t;
  close_this : tcppthis option;
}

and tcppcrementop = CppIncrement | CppDecrement
and tcppunop = CppNeg | CppNegBits | CppNot
and tcppthis = ThisReal | ThisFake | ThisDynamic

and tcppvarloc =
  | VarLocal of tvar
  | VarClosure of tvar
  | VarThis of tclass_field * tcpp
  | VarInstance of tcppexpr * tclass_field * string * string
  | VarInterface of tcppexpr * tclass_field
  | VarStatic of tclass * bool * tclass_field
  | VarInternal of tcppexpr * string * string

and tcppinst = InstPtr | InstObjC | InstStruct

and tcppfuncloc =
  | FuncThis of tclass_field * tcpp
  | FuncInstance of tcppexpr * tcppinst * tclass_field
  | FuncStatic of tclass * bool * tclass_field
  | FuncTemplate of tclass * tclass_field * path * bool
  | FuncInterface of tcppexpr * tclass * tclass_field
  | FuncEnumConstruct of tenum * tenum_field
  | FuncSuperConstruct of tcpp
  | FuncSuper of tcppthis * tcpp * tclass_field
  | FuncNew of tcpp
  | FuncExpression of tcppexpr
  | FuncInternal of tcppexpr * string * string
  | FuncExtern of string * bool
  | FuncFromStaticFunction

and tcpparrayloc =
  | ArrayTyped of tcppexpr * tcppexpr * tcpp
  | ArrayPointer of tcppexpr * tcppexpr
  | ArrayRawPointer of tcppexpr * tcppexpr
  | ArrayObject of tcppexpr * tcppexpr * tcpp
  | ArrayVirtual of tcppexpr * tcppexpr
  | ArrayImplements of tclass * tcppexpr * tcppexpr
  | ArrayDynamic of tcppexpr * tcppexpr

and tcpplvalue =
  | CppVarRef of tcppvarloc
  | CppArrayRef of tcpparrayloc
  | CppDynamicRef of tcppexpr * string
  | CppExternRef of string * bool

and tcpp_expr_expr =
  | CppInt of int32
  | CppFloat of string
  | CppString of string
  | CppBool of bool
  | CppNull
  | CppNullAccess
  | CppNil
  | CppThis of tcppthis
  | CppSuper of tcppthis
  | CppCode of string * tcppexpr list
  | CppClosure of tcpp_closure
  | CppVar of tcppvarloc
  | CppExtern of string * bool
  | CppDynamicField of tcppexpr * string
  | CppFunction of tcppfuncloc * tcpp
  | CppEnumIndex of tcppexpr
  | CppEnumField of tenum * tenum_field
  | CppCall of tcppfuncloc * tcppexpr list
  | CppFunctionAddress of tclass * tclass_field
  | CppNewNative of tcppexpr
  | CppAddressOf of tcppexpr
  | CppDereference of tcppexpr
  | CppArray of tcpparrayloc
  | CppCrement of tcppcrementop * Ast.unop_flag * tcpplvalue
  | CppSet of tcpplvalue * tcppexpr
  | CppModify of Ast.binop * tcpplvalue * tcppexpr
  | CppBinop of Ast.binop * tcppexpr * tcppexpr
  | CppCompare of string * tcppexpr * tcppexpr * Ast.binop
  | CppNullCompare of string * tcppexpr
  | CppObjectDecl of (string * tcppexpr) list * bool
  | CppPosition of string * int32 * string * string
  | CppArrayDecl of tcppexpr list
  | CppUnop of tcppunop * tcppexpr
  | CppVarDecl of tvar * tcppexpr option
  | CppBlock of tcppexpr list * tcpp_closure list * bool
  | CppFor of tvar * tcppexpr * tcppexpr
  | CppIf of tcppexpr * tcppexpr * tcppexpr option
  | CppWhile of tcppexpr * tcppexpr * Ast.while_flag * int
  | CppIntSwitch of tcppexpr * (Int32.t list * tcppexpr) list * tcppexpr option
  | CppSwitch of
      tcppexpr * tcpp * (tcppexpr list * tcppexpr) list * tcppexpr option * int
  | CppTry of tcppexpr * (tvar * tcppexpr) list
  | CppBreak
  | CppContinue
  | CppClassOf of path * bool
  | CppGoto of int
  | CppReturn of tcppexpr option
  | CppThrow of tcppexpr
  | CppEnumParameter of tcppexpr * tenum_field * int
  | CppTCast of tcppexpr * tcpp
  | CppCast of tcppexpr * tcpp
  | CppCastStatic of tcppexpr * tcpp
  | CppCastScalar of tcppexpr * string
  | CppCastVariant of tcppexpr
  | CppCastObjC of tcppexpr * tclass
  | CppCastObjCBlock of tcppexpr * tcpp list * tcpp
  | CppCastProtocol of tcppexpr * tclass
  | CppCastNative of tcppexpr

and tcpp_class_container =
  | Current (* If the current class holds GC variables *)
  | Parent (* If one of the current classes parents holds GC variables *)

and tcpp_class_flags =
  | QuickAlloc
  | Scriptable
  | MemberGet
  | MemberSet
  | StaticGet
  | StaticSet
  | GetFields
  | Compare
  | Boot

and tcpp_class_function = {
  tcf_field : tclass_field;
  tcf_name : string;
  tcf_func : tfunc;

  tcf_is_virtual : bool;
  tcf_is_reflective : bool;
  tcf_is_external : bool;
  tcf_is_scriptable : bool;
  tcf_is_overriding : bool;
}

and tcpp_class_variable = {
  tcv_field : tclass_field;
  tcv_name : string;
  tcv_type : t;
  tcv_default : texpr option;

  tcv_is_stackonly : bool;
  tcv_is_gc_element : bool;
  tcv_is_reflective : bool;
}

and tcpp_class = {
  tcl_class : tclass;
  tcl_params : tparams;
  tcl_name : string;
  tcl_id : int32;
  tcl_flags : int;
  tcl_debug_level : int;
  tcl_super : tcpp_class option;
  tcl_container : tcpp_class_container option;

  tcl_haxe_interfaces : tcpp_interface list;
  tcl_native_interfaces : tcpp_interface list;

  tcl_static_variables : tcpp_class_variable list;
  tcl_static_properties : tcpp_class_variable list;
  tcl_static_functions : tcpp_class_function list;
  tcl_static_dynamic_functions : tcpp_class_function list;

  tcl_variables : tcpp_class_variable list;
  tcl_properties : tcpp_class_variable list;
  tcl_functions : tcpp_class_function list;
  tcl_dynamic_functions : tcpp_class_function list;

  tcl_meta : texpr option;
  tcl_rtti : texpr option;
  tcl_init : texpr option;
}

and tcpp_interface_function = {
  iff_field : tclass_field;
  iff_name : string;
  iff_args : (string * bool * t) list;
  iff_return : t;
  iff_script_slot : int option;
}

and tcpp_interface = {
  if_class : tclass;
  if_name : string;
  if_hash : string;
  if_debug_level : int;
  if_functions : tcpp_interface_function list;
  if_variables : tclass_field list;
  if_extends : tcpp_interface option;
  if_meta : texpr option;
  if_rtti : texpr option;
  if_scriptable : bool;
}

and tcpp_enum_field = {
  tef_field : tenum_field;
  tef_name : string;
  tef_hash : string;
}

and tcpp_enum = {
  te_enum : tenum;
  te_id : int32;
  te_constructors : tcpp_enum_field list;
}

and tcpp_decl =
  | ManagedClass of tcpp_class
  | NativeClass of tcpp_class
  | ManagedInterface of tcpp_interface
  | NativeInterface of tcpp_interface
  | Enum of tcpp_enum
