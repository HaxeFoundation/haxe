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

(* C# code generator global types and utilities *)

(* Access modifiers for C# *)
module AccessModifier = struct
	type t =
		| Public
		| Private
		| Protected
		| Internal
		| ProtectedInternal
		| PrivateProtected

	let to_string = function
		| Public -> "public"
		| Private -> "private"
		| Protected -> "protected"
		| Internal -> "internal"
		| ProtectedInternal -> "protected internal"
		| PrivateProtected -> "private protected"
end

(* Member modifiers for C# *)
module MemberModifier = struct
	type t =
		| Static
		| Readonly
		| Const
		| Volatile
		| Abstract
		| Virtual
		| Override
		| Sealed
		| Extern
		| New
		| Async
		| Partial

	let to_string = function
		| Static -> "static"
		| Readonly -> "readonly"
		| Const -> "const"
		| Volatile -> "volatile"
		| Abstract -> "abstract"
		| Virtual -> "virtual"
		| Override -> "override"
		| Sealed -> "sealed"
		| Extern -> "extern"
		| New -> "new"
		| Async -> "async"
		| Partial -> "partial"
end

(* Type modifiers for C# classes/structs/interfaces *)
module TypeModifier = struct
	type t =
		| Abstract
		| Sealed
		| Static
		| Partial

	let to_string = function
		| Abstract -> "abstract"
		| Sealed -> "sealed"
		| Static -> "static"
		| Partial -> "partial"
end

(* Binary operators *)
type cs_binop =
	| CsOpAdd
	| CsOpSub
	| CsOpMul
	| CsOpDiv
	| CsOpMod
	| CsOpAnd
	| CsOpOr
	| CsOpXor
	| CsOpShl
	| CsOpShr
	| CsOpEq
	| CsOpNotEq
	| CsOpLt
	| CsOpLte
	| CsOpGt
	| CsOpGte
	| CsOpBoolAnd
	| CsOpBoolOr
	| CsOpNullCoalesce
	| CsOpAssign
	| CsOpAssignOp of cs_binop

let rec binop_to_string = function
	| CsOpAdd -> "+"
	| CsOpSub -> "-"
	| CsOpMul -> "*"
	| CsOpDiv -> "/"
	| CsOpMod -> "%"
	| CsOpAnd -> "&"
	| CsOpOr -> "|"
	| CsOpXor -> "^"
	| CsOpShl -> "<<"
	| CsOpShr -> ">>"
	| CsOpEq -> "=="
	| CsOpNotEq -> "!="
	| CsOpLt -> "<"
	| CsOpLte -> "<="
	| CsOpGt -> ">"
	| CsOpGte -> ">="
	| CsOpBoolAnd -> "&&"
	| CsOpBoolOr -> "||"
	| CsOpNullCoalesce -> "??"
	| CsOpAssign -> "="
	| CsOpAssignOp op -> binop_to_string op ^ "="

(* Unary operators *)
type cs_unop =
	| CsOpNeg
	| CsOpNot
	| CsOpBitNot
	| CsOpIncrement
	| CsOpDecrement

let unop_to_string = function
	| CsOpNeg -> "-"
	| CsOpNot -> "!"
	| CsOpBitNot -> "~"
	| CsOpIncrement -> "++"
	| CsOpDecrement -> "--"

(* Error helper *)
let cs_error s =
	failwith s

(* Reserved keywords in C# that need escaping *)
let cs_keywords = [
	"abstract"; "as"; "base"; "bool"; "break"; "byte"; "case"; "catch";
	"char"; "checked"; "class"; "const"; "continue"; "decimal"; "default";
	"delegate"; "do"; "double"; "else"; "enum"; "event"; "explicit"; "extern";
	"false"; "finally"; "fixed"; "float"; "for"; "foreach"; "goto"; "if";
	"implicit"; "in"; "int"; "interface"; "internal"; "is"; "lock"; "long";
	"namespace"; "new"; "null"; "object"; "operator"; "out"; "override";
	"params"; "private"; "protected"; "public"; "readonly"; "ref"; "return";
	"sbyte"; "sealed"; "short"; "sizeof"; "stackalloc"; "static"; "string";
	"struct"; "switch"; "this"; "throw"; "true"; "try"; "typeof"; "uint";
	"ulong"; "unchecked"; "unsafe"; "ushort"; "using"; "virtual"; "void";
	"volatile"; "while"
]

let is_cs_keyword s =
	List.mem s cs_keywords

(* Escape identifier if it's a C# keyword *)
let escape_identifier s =
	if is_cs_keyword s then "@" ^ s else s

(* Convert Haxe path to C# namespace *)
let path_to_cs (pack, name) =
	let pack = List.map escape_identifier pack in
	let name = escape_identifier name in
	(pack, name)

(* Generate fully qualified C# name *)
let s_cs_path (pack, name) =
	match pack with
	| [] -> name
	| _ -> String.concat "." pack ^ "." ^ name

(* Maximum function arity for invoke overloads (like JVM) *)
let max_arity = 8
