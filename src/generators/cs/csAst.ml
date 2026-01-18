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

(* C# Abstract Syntax Tree types *)

open CsGlobals

(* C# attribute - represents [AttributeName(arg1, arg2, ...)] *)
type cs_attribute = {
	attr_name : string;        (* Full type name e.g. "System.Obsolete" *)
	attr_args : string list;   (* Arguments as raw C# expressions *)
}

(* C# type path: namespace parts and type name *)
type cs_path = string list * string

(* C# type representation *)
type cs_type =
	| CsTypeVoid
	| CsTypeBool
	| CsTypeByte
	| CsTypeSByte
	| CsTypeChar
	| CsTypeShort
	| CsTypeUShort
	| CsTypeInt
	| CsTypeUInt
	| CsTypeLong
	| CsTypeULong
	| CsTypeFloat
	| CsTypeDouble
	| CsTypeDecimal
	| CsTypeString
	| CsTypeObject
	| CsTypeDynamic
	| CsTypeNullable of cs_type
	| CsTypeArray of cs_type * int option  (* element type, optional rank *)
	| CsTypeClass of cs_path * cs_type list  (* path, type arguments *)
	| CsTypeNested of cs_type * string  (* parent type (with its type args), nested class name *)
	| CsTypeGenericParam of string  (* T, K, etc. *)
	| CsTypeFunc of cs_type list * cs_type  (* argument types, return type *)
	| CsTypeAction of cs_type list  (* void-returning delegate *)
	| CsTypeVar  (* var keyword for type inference *)

(* Constant values *)
type cs_const =
	| CsConstNull
	| CsConstBool of bool
	| CsConstInt of int32
	| CsConstLong of int64
	| CsConstFloat of float
	| CsConstDouble of float
	| CsConstString of string
	| CsConstChar of char

(* Expression AST *)
type cs_expr =
	| CsConst of cs_const
	| CsLocal of string
	| CsThis
	| CsBase
	| CsNull
	| CsDefault of cs_type
	| CsTypeOf of cs_type
	| CsNameOf of string
	| CsSizeOf of cs_type
	| CsBinop of cs_binop * cs_expr * cs_expr
	| CsUnop of cs_unop * bool * cs_expr  (* op, is_postfix, expr *)
	| CsTernary of cs_expr * cs_expr * cs_expr
	| CsField of cs_expr * string
	| CsStaticField of cs_type * string
	| CsArrayAccess of cs_expr * cs_expr
	| CsCall of cs_expr * cs_expr list
	| CsCallGeneric of cs_expr * cs_type list * cs_expr list  (* generic method call: expr<T1,T2>(args) *)
	| CsStaticCall of cs_type * string * cs_expr list
	| CsStaticCallGeneric of cs_type * string * cs_type list * cs_expr list  (* Class.method<T1,T2>(args) *)
	| CsNew of cs_type * cs_expr list
	| CsNewArray of cs_type * cs_expr list  (* new T[] { ... } *)
	| CsNewArraySize of cs_type * cs_expr  (* new T[size] *)
	| CsCast of cs_type * cs_expr
	| CsAs of cs_expr * cs_type
	| CsIs of cs_expr * cs_type
	| CsIsPattern of cs_expr * cs_type * string option  (* expr is Type name *)
	| CsLambda of cs_param list * cs_lambda_body
	| CsParens of cs_expr
	| CsNullConditionalField of cs_expr * string  (* expr?.field *)
	| CsNullConditionalCall of cs_expr * cs_expr list  (* expr?.method() *)
	| CsNullConditionalIndex of cs_expr * cs_expr  (* expr?[index] *)
	| CsAwait of cs_expr
	| CsThrow of cs_expr
	| CsInterpolatedString of cs_interpolated_part list
	| CsRaw of string  (* raw C# code, escape hatch *)
	| CsInlineCode of string * cs_expr list  (* template string with {0}, {1}, etc. and arguments *)

and cs_param = {
	p_name : string;
	p_type : cs_type option;
	p_default : cs_expr option;
	p_modifier : cs_param_modifier option;
}

and cs_param_modifier =
	| CsParamRef
	| CsParamOut
	| CsParamIn
	| CsParamParams

and cs_lambda_body =
	| CsLambdaExpr of cs_expr
	| CsLambdaBlock of cs_stmt list

and cs_interpolated_part =
	| CsInterpLiteral of string
	| CsInterpExpr of cs_expr * string option  (* expr, optional format *)

(* Statement AST *)
and cs_stmt =
	| CsExprStmt of cs_expr
	| CsBlock of cs_stmt list
	| CsStmtList of cs_stmt list  (* multiple statements without braces, emitted sequentially *)
	| CsVarDecl of string * cs_type option * cs_expr option
	| CsMultiVarDecl of (string * cs_expr option) list * cs_type
	| CsIf of cs_expr * cs_stmt * cs_stmt option
	| CsSwitch of cs_expr * cs_switch_section list
	| CsWhile of cs_expr * cs_stmt
	| CsDoWhile of cs_stmt * cs_expr
	| CsFor of cs_stmt option * cs_expr option * cs_expr option * cs_stmt
	| CsForeach of cs_type * string * cs_expr * cs_stmt
	| CsReturn of cs_expr option
	| CsBreak
	| CsContinue
	| CsThrowStmt of cs_expr
	| CsTry of cs_stmt * cs_catch list * cs_stmt option  (* try, catches, finally *)
	| CsUsing of cs_expr * cs_stmt
	| CsUsingDecl of cs_type * string * cs_expr * cs_stmt
	| CsLock of cs_expr * cs_stmt
	| CsLabel of string
	| CsGoto of string
	| CsEmpty
	| CsComment of string
	| CsRawStmt of string  (* raw C# code *)

and cs_switch_section = {
	sw_labels : cs_switch_label list;
	sw_body : cs_stmt list;
}

and cs_switch_label =
	| CsCaseConst of cs_expr
	| CsCasePattern of cs_type * string option  (* case Type name: *)
	| CsCaseWhen of cs_type * string option * cs_expr  (* case Type name when expr: *)
	| CsCaseDefault  (* default: in switch *)

and cs_catch = {
	catch_type : cs_type option;
	catch_name : string option;
	catch_when : cs_expr option;
	catch_body : cs_stmt;
}

(* Member definitions *)
type cs_field_def = {
	f_name : string;
	f_type : cs_type;
	f_access : AccessModifier.t;
	f_modifiers : MemberModifier.t list;
	f_value : cs_expr option;
}

type cs_property_def = {
	prop_name : string;
	prop_type : cs_type;
	prop_access : AccessModifier.t;
	prop_modifiers : MemberModifier.t list;
	prop_getter : cs_accessor option;
	prop_setter : cs_accessor option;
	prop_init : cs_expr option;
	prop_explicit_interface : cs_type option;  (* For explicit interface implementation: IInterface.Property *)
}

and cs_accessor = {
	acc_access : AccessModifier.t option;
	acc_body : cs_stmt list option;  (* None = auto, Some [] = expression body *)
}

type cs_method_def = {
	m_name : string;
	m_return_type : cs_type;
	m_access : AccessModifier.t;
	m_modifiers : MemberModifier.t list;
	m_type_params : string list;
	m_params : cs_param list;
	m_body : cs_stmt list option;  (* None = abstract/extern *)
	m_constraints : (string * cs_type list) list;  (* where T : constraints *)
	m_explicit_interface : cs_type option;  (* For explicit interface implementation: InterfaceType.MethodName *)
	m_attributes : cs_attribute list;  (* C# attributes like [Obsolete], [DllImport], etc. *)
}

type cs_ctor_def = {
	ctor_access : AccessModifier.t;
	ctor_modifiers : MemberModifier.t list;
	ctor_params : cs_param list;
	ctor_base_call : cs_expr list option;  (* : base(args) *)
	ctor_this_call : cs_expr list option;  (* : this(args) *)
	ctor_body : cs_stmt list;
}

type cs_event_def = {
	ev_name : string;
	ev_type : cs_type;
	ev_access : AccessModifier.t;
	ev_modifiers : MemberModifier.t list;
	ev_add : cs_stmt list option;
	ev_remove : cs_stmt list option;
}

type cs_indexer_def = {
	idx_type : cs_type;
	idx_access : AccessModifier.t;
	idx_modifiers : MemberModifier.t list;
	idx_params : cs_param list;
	idx_getter : cs_accessor option;
	idx_setter : cs_accessor option;
}

type cs_operator_def = {
	op_kind : string;  (* +, -, ==, etc. or "implicit", "explicit" *)
	op_return_type : cs_type;
	op_access : AccessModifier.t;
	op_modifiers : MemberModifier.t list;
	op_params : cs_param list;
	op_body : cs_stmt list;
}

(* Class/struct/interface member *)
type cs_member =
	| CsMemberField of cs_field_def
	| CsMemberProperty of cs_property_def
	| CsMemberMethod of cs_method_def
	| CsMemberConstructor of cs_ctor_def
	| CsMemberStaticConstructor of cs_stmt list  (* static ClassName() { ... } *)
	| CsMemberEvent of cs_event_def
	| CsMemberIndexer of cs_indexer_def
	| CsMemberOperator of cs_operator_def
	| CsMemberNestedType of cs_type_def

(* Enum member *)
and cs_enum_member = {
	em_name : string;
	em_value : cs_expr option;
}

(* Type definitions *)
and cs_class_def = {
	c_path : cs_path;
	c_access : AccessModifier.t;
	c_modifiers : TypeModifier.t list;
	c_type_params : string list;
	c_base : cs_type option;
	c_interfaces : cs_type list;
	c_constraints : (string * cs_type list) list;
	c_members : cs_member list;
}

and cs_struct_def = {
	s_path : cs_path;
	s_access : AccessModifier.t;
	s_modifiers : TypeModifier.t list;
	s_type_params : string list;
	s_interfaces : cs_type list;
	s_constraints : (string * cs_type list) list;
	s_members : cs_member list;
}

and cs_interface_def = {
	i_path : cs_path;
	i_access : AccessModifier.t;
	i_modifiers : TypeModifier.t list;
	i_type_params : string list;
	i_base : cs_type list;
	i_constraints : (string * cs_type list) list;
	i_members : cs_member list;
}

and cs_enum_def = {
	e_path : cs_path;
	e_access : AccessModifier.t;
	e_underlying : cs_type option;  (* : int, : byte, etc. *)
	e_members : cs_enum_member list;
}

and cs_delegate_def = {
	d_path : cs_path;
	d_access : AccessModifier.t;
	d_return_type : cs_type;
	d_type_params : string list;
	d_params : cs_param list;
	d_constraints : (string * cs_type list) list;
}

and cs_type_def =
	| CsClassDef of cs_class_def
	| CsStructDef of cs_struct_def
	| CsInterfaceDef of cs_interface_def
	| CsEnumDef of cs_enum_def
	| CsDelegateDef of cs_delegate_def

(* Using directives *)
type cs_using =
	| CsUsingNamespace of string list  (* using System.Collections; *)
	| CsUsingStatic of cs_path  (* using static System.Math; *)
	| CsUsingAlias of string * cs_type  (* using Alias = Type; *)

(* A complete C# source file *)
type cs_file = {
	file_usings : cs_using list;
	file_namespace : string list option;
	file_types : cs_type_def list;
	file_top_level_types : cs_type_def list;  (* Types outside the namespace, e.g. closures *)
}

(* A complete C# project *)
type cs_project = {
	proj_name : string;
	proj_target_framework : string;
	proj_output_type : string;  (* Exe, Library *)
}
