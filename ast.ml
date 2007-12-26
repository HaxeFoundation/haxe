(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

type keyword =
	| Function
	| Class
	| Var
	| If
	| Else
	| While
	| Do
	| For
	| Break
	| Continue
	| Return
	| Extends
	| Implements
	| Import
	| Switch
	| Case
	| Default
	| Static
	| Public
	| Private
	| Try
	| Catch
	| New
	| This
	| Throw
	| Extern
	| Enum
	| In
	| Interface
	| Untyped
	| Cast
	| Override
	| Typedef
	| F9Dynamic
	| Package
	| Callback
	| Inline

type binop =
	| OpAdd
	| OpMult
	| OpDiv
	| OpSub
	| OpAssign
	| OpEq
	| OpPhysEq
	| OpNotEq
	| OpPhysNotEq
	| OpGt
	| OpGte
	| OpLt
	| OpLte
	| OpAnd
	| OpOr
	| OpXor
	| OpBoolAnd
	| OpBoolOr
	| OpShl
	| OpShr
	| OpUShr
	| OpMod
	| OpAssignOp of binop
	| OpInterval

type unop =
	| Increment
	| Decrement
	| Not
	| Neg
	| NegBits

type constant =
	| Int of string
	| Float of string
	| String of string
	| Ident of string
	| Type of string
	| Regexp of string * string

type token =
	| Eof
	| Const of constant
	| Kwd of keyword
	| Comment of string
	| CommentLine of string
	| Binop of binop
	| Unop of unop
	| Semicolon
	| Comma
	| BrOpen
	| BrClose
	| BkOpen
	| BkClose
	| POpen
	| PClose
	| Dot
	| DblDot
	| Arrow
	| IntInterval of string
	| Macro of string
	| Question

type unop_flag =
	| Prefix
	| Postfix

type while_flag =
	| NormalWhile
	| DoWhile

type type_path_normal = {
	tpackage : string list;
	tname : string;
	tparams : type_param_or_const list;
}

and type_param_or_const =
	| TPType of type_path
	| TPConst of constant

and anonymous_field =
	| AFVar of type_path
	| AFProp of type_path * string * string
	| AFFun of (string * bool * type_path) list * type_path

and type_path =
	| TPNormal of type_path_normal
	| TPFunction of type_path list * type_path
	| TPAnonymous of (string * bool option * anonymous_field * pos) list
	| TPParent of type_path
	| TPExtend of type_path_normal * (string * bool option * anonymous_field * pos) list

type func = {
	f_args : (string * bool * type_path option) list;
	f_type : type_path option;
	f_expr : expr;
}

and expr_def =
	| EConst of constant
	| EArray of expr * expr
	| EBinop of binop * expr * expr
	| EField of expr * string
	| EType of expr * string
	| EParenthesis of expr
	| EObjectDecl of (string * expr) list
	| EArrayDecl of expr list
	| ECall of expr * expr list
	| ENew of type_path_normal * expr list
	| EUnop of unop * unop_flag * expr
	| EVars of (string * type_path option * expr option) list
	| EFunction of func
	| EBlock of expr list
	| EFor of string * expr * expr
	| EIf of expr * expr * expr option
	| EWhile of expr * expr * while_flag
	| ESwitch of expr * (expr list * expr) list * expr option
	| ETry of expr * (string * type_path * expr) list
	| EReturn of expr option
	| EBreak
	| EContinue
	| EUntyped of expr
	| EThrow of expr
	| ECast of expr * type_path option
	| EDisplay of expr
	| EDisplayNew of type_path_normal
	| ETernary of expr * expr * expr

and expr = expr_def * pos

type type_param = string * type_path_normal list

type documentation = string option

type access =
	| APublic
	| APrivate
	| AStatic
	| AOverride
	| AF9Dynamic
	| AInline

type class_field =
	| FVar of string * documentation * access list * type_path option * expr option
	| FFun of string * documentation * access list * type_param list * func
	| FProp of string * documentation * access list * string * string * type_path

type enum_flag =
	| EPrivate
	| EExtern

type class_flag =
	| HInterface
	| HExtern
	| HPrivate
	| HExtends of type_path_normal
	| HImplements of type_path_normal

type enum_constructor = string * documentation * (string * bool * type_path) list * pos

type ('a,'b) definition = {
	d_name : string;
	d_doc : documentation;
	d_params : type_param list;
	d_flags : 'a list;
	d_data : 'b;
}

type type_def =
	| EClass of (class_flag, (class_field * pos) list) definition
	| EEnum of (enum_flag, enum_constructor list) definition
	| ETypedef of (enum_flag, type_path) definition
	| EImport of string list * string * string option

type type_decl = type_def * pos

type package = string list * type_decl list

let pos = snd

let is_postfix (e,_) = function
	| Increment | Decrement -> (match e with EConst _ | EField _ | EType _ | EArray _ -> true | _ -> false)
	| Not | Neg | NegBits -> false

let is_prefix = function
	| Increment | Decrement -> true
	| Not | Neg | NegBits -> true

let base_class_name = snd

let null_pos = { pfile = "<null>"; pmin = -1; pmax = -1 }

let punion p p2 =
	{
		pfile = p.pfile;
		pmin = min p.pmin p2.pmin;
		pmax = max p.pmax p2.pmax;
	}

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let s_escape s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let s_constant = function
	| Int s -> s
	| Float s -> s
	| String s -> "\"" ^ s_escape s ^ "\""
	| Ident s -> s
	| Type s -> s
	| Regexp (r,o) -> "~/" ^ r ^ "/"

let s_keyword = function
	| Function -> "function"
	| Class -> "class"
	| Static -> "static"
	| Var -> "var"
	| If -> "if"
	| Else -> "else"
	| While -> "while"
	| Do -> "do"
	| For -> "for"
	| Break -> "break"
	| Return -> "return"
	| Continue -> "continue"
	| Extends -> "extends"
	| Implements -> "implements"
	| Import -> "import"
	| Switch -> "switch"
	| Case -> "case"
	| Default -> "default"
	| Private -> "private"
	| Public -> "public"
	| Try -> "try"
	| Catch -> "catch"
	| New -> "new"
	| This -> "this"
	| Throw -> "throw"
	| Extern -> "extern"
	| Enum -> "enum"
	| In -> "in"
	| Interface -> "interface"
	| Untyped -> "untyped"
	| Cast -> "cast"
	| Override -> "override"
	| Typedef -> "typedef"
	| F9Dynamic -> "f9dynamic"
	| Package -> "package"
	| Callback -> "callback"
	| Inline -> "inline"

let rec s_binop = function
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpPhysEq -> "==="
	| OpNotEq -> "!="
	| OpPhysNotEq -> "!=="
	| OpGte -> ">="
	| OpLte -> "<="
	| OpGt -> ">"
	| OpLt -> "<"
	| OpAnd -> "&"
	| OpOr -> "|"
	| OpXor -> "^"
	| OpBoolAnd -> "&&"
	| OpBoolOr -> "||"
	| OpShr -> ">>"
	| OpUShr -> ">>>"
	| OpShl -> "<<"
	| OpMod -> "%"
	| OpAssignOp op -> s_binop op ^ "="
	| OpInterval -> "..."

let s_unop = function
	| Increment -> "++"
	| Decrement -> "--"
	| Not -> "!"
	| Neg -> "-"
	| NegBits -> "~"

let s_token = function
	| Eof -> "<end of file>"
	| Const c -> s_constant c
	| Kwd k -> s_keyword k
	| Comment s -> "/*"^s^"*/"
	| CommentLine s -> "//"^s
	| Binop o -> s_binop o
	| Unop o -> s_unop o
	| Semicolon -> ";"
	| Comma -> ","
	| BkOpen -> "["
	| BkClose -> "]"
	| BrOpen -> "{"
	| BrClose -> "}"
	| POpen -> "("
	| PClose -> ")"
	| Dot -> "."
	| DblDot -> ":"
	| Arrow -> "->"
	| IntInterval s -> s ^ "..."
	| Macro s -> "#" ^ s
	| Question -> "?"

let unescape s =
	let b = Buffer.create 0 in
	let rec loop esc i =
		if i = String.length s then
			()
		else
			let c = s.[i] in
			if esc then begin
				let inext = ref (i + 1) in
				(match c with
				| 'n' -> Buffer.add_char b '\n'
				| 'r' -> Buffer.add_char b '\r'
				| 't' -> Buffer.add_char b '\t'
				| '"' | '\'' | '\\' -> Buffer.add_char b c
				| '0'..'3' ->
					let c = (try char_of_int (int_of_string ("0o" ^ String.sub s i 3)) with _ -> raise Exit) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| 'x' ->
					let c = (try char_of_int (int_of_string ("0x" ^ String.sub s (i+1) 2)) with _ -> raise Exit) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| _ ->
					raise Exit);
				loop false !inext;
			end else
				match c with
				| '\\' -> loop true (i + 1)
				| c ->
					Buffer.add_char b c;
					loop false (i + 1)
	in
	loop false 0;
	Buffer.contents b
