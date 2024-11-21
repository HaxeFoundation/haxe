open Extlib_leftovers
open Ast
open Type
open Error
open Common
open Globals

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
  close_undeclared : (string, tvar) Hashtbl.t;
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