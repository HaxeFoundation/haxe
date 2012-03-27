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
	| Dynamic
	| Package
	| Callback
	| Inline
	| Using
	| Null
	| True
	| False

type binop =
	| OpAdd
	| OpMult
	| OpDiv
	| OpSub
	| OpAssign
	| OpEq
	| OpNotEq
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
	| At
	| Dollar of string

type unop_flag =
	| Prefix
	| Postfix

type while_flag =
	| NormalWhile
	| DoWhile

type type_path = {
	tpackage : string list;
	tname : string;
	tparams : type_param_or_const list;
	tsub : string option;
}

and type_param_or_const =
	| TPType of complex_type
	| TPExpr of expr

and complex_type =
	| CTPath of type_path
	| CTFunction of complex_type list * complex_type
	| CTAnonymous of class_field list
	| CTParent of complex_type
	| CTExtend of type_path * class_field list
	| CTOptional of complex_type

and func = {
	f_params : type_param list;
	f_args : (string * bool * complex_type option * expr option) list;
	f_type : complex_type option;
	f_expr : expr option;
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
	| ENew of type_path * expr list
	| EUnop of unop * unop_flag * expr
	| EVars of (string * complex_type option * expr option) list
	| EFunction of string option * func
	| EBlock of expr list
	| EFor of expr * expr
	| EIn of expr * expr
	| EIf of expr * expr * expr option
	| EWhile of expr * expr * while_flag
	| ESwitch of expr * (expr list * expr) list * expr option
	| ETry of expr * (string * complex_type * expr) list
	| EReturn of expr option
	| EBreak
	| EContinue
	| EUntyped of expr
	| EThrow of expr
	| ECast of expr * complex_type option
	| EDisplay of expr * bool
	| EDisplayNew of type_path
	| ETernary of expr * expr * expr
	| ECheckType of expr * complex_type

and expr = expr_def * pos

and type_param = string * complex_type list

and documentation = string option

and metadata = (string * expr list * pos) list

and access =
	| APublic
	| APrivate
	| AStatic
	| AOverride
	| ADynamic
	| AInline

and class_field_kind =
	| FVar of complex_type option * expr option
	| FFun of func
	| FProp of string * string * complex_type * expr option

and class_field = {
	cff_name : string;
	cff_doc : documentation;
	cff_pos : pos;
	mutable cff_meta : metadata;
	mutable cff_access : access list;
	mutable cff_kind : class_field_kind;
}

type enum_flag =
	| EPrivate
	| EExtern

type class_flag =
	| HInterface
	| HExtern
	| HPrivate
	| HExtends of type_path
	| HImplements of type_path

type enum_constructor = string * documentation * metadata * (string * bool * complex_type) list * pos

type ('a,'b) definition = {
	d_name : string;
	d_doc : documentation;
	d_params : type_param list;
	d_meta : metadata;
	d_flags : 'a list;
	d_data : 'b;
}

type type_def =
	| EClass of (class_flag, class_field list) definition
	| EEnum of (enum_flag, enum_constructor list) definition
	| ETypedef of (enum_flag, complex_type) definition
	| EImport of type_path
	| EUsing of type_path

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

let null_pos = { pfile = "?"; pmin = -1; pmax = -1 }

let punion p p2 =
	{
		pfile = p.pfile;
		pmin = min p.pmin p2.pmin;
		pmax = max p.pmax p2.pmax;
	}

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let parse_path s =
	match List.rev (ExtString.String.nsplit s ".") with
	| [] -> failwith "Invalid empty path"
	| x :: l -> List.rev l, x

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

let s_access = function
	| APublic -> "public"
	| APrivate -> "private"
	| AStatic -> "static"
	| AOverride -> "override"
	| ADynamic -> "dynamic"
	| AInline -> "inline"

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
	| Dynamic -> "dynamic"
	| Package -> "package"
	| Callback -> "callback"
	| Inline -> "inline"
	| Using -> "using"
	| Null -> "null"
	| True -> "true"
	| False -> "false"

let rec s_binop = function
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpNotEq -> "!="
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
	| At -> "@"
	| Dollar v -> "$" ^ v

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


let map_expr loop (e,p) =
	let opt f o =
		match o with None -> None | Some v -> Some (f v)
	in
	let rec tparam = function
		| TPType t -> TPType (ctype t)
		| TPExpr e -> TPExpr (loop e)
	and cfield f = 
		{ f with cff_kind = (match f.cff_kind with
			| FVar (t,e) -> FVar (opt ctype t, opt loop e)
			| FFun f -> FFun (func f)
			| FProp (get,set,t,e) -> FProp (get,set,ctype t,opt loop e))
		}
	and ctype = function
		| CTPath t -> CTPath (tpath t)
		| CTFunction (cl,c) -> CTFunction (List.map ctype cl, ctype c)
		| CTAnonymous fl -> CTAnonymous (List.map cfield fl)
		| CTParent t -> CTParent (ctype t)
		| CTExtend (t,fl) -> CTExtend (tpath t, List.map cfield fl)
		| CTOptional t -> CTOptional (ctype t)
	and func f =
		{	
			f_params = List.map (fun (n,tl) -> n,List.map ctype tl)  f.f_params;
			f_args = List.map (fun (n,o,t,e) -> n,o,opt ctype t,opt loop e) f.f_args;
			f_type = opt ctype f.f_type;
			f_expr = opt loop f.f_expr;
		}
	and tpath t = { t with tparams = List.map tparam t.tparams }
	in
	let e = (match e with
	| EConst _ -> e
	| EArray (e1,e2) -> EArray (loop e1, loop e2)
	| EBinop (op,e1,e2) -> EBinop (op,loop e1, loop e2)
	| EField (e,f) -> EField (loop e, f)
	| EType (e,f) -> EType (loop e, f)
	| EParenthesis e -> EParenthesis (loop e)
	| EObjectDecl fl -> EObjectDecl (List.map (fun (f,e) -> f,loop e) fl)
	| EArrayDecl el -> EArrayDecl (List.map loop el)
	| ECall (e,el) -> ECall (loop e, List.map loop el)
	| ENew (t,el) -> ENew (tpath t,List.map loop el)
	| EUnop (op,f,e) -> EUnop (op,f,loop e)
	| EVars vl -> EVars (List.map (fun (n,t,eo) -> n,opt ctype t,opt loop eo) vl)
	| EFunction (n,f) -> EFunction (n,func f)
	| EBlock el -> EBlock (List.map loop el)
	| EFor (e1,e2) -> EFor (loop e1, loop e2)
	| EIn (e1,e2) -> EIn (loop e1, loop e2)
	| EIf (e,e1,e2) -> EIf (loop e, loop e1, opt loop e2)
	| EWhile (econd,e,f) -> EWhile (loop econd, loop e, f)
	| ESwitch (e,cases,def) -> ESwitch (loop e, List.map (fun (el,e) -> List.map loop el, loop e) cases, opt loop def)
	| ETry (e, catches) -> ETry (loop e, List.map (fun (n,t,e) -> n,ctype t,loop e) catches)
	| EReturn e -> EReturn (opt loop e)
	| EBreak -> EBreak
	| EContinue -> EContinue
	| EUntyped e -> EUntyped (loop e)
	| EThrow e -> EThrow (loop e)
	| ECast (e,t) -> ECast (loop e,opt ctype t)
	| EDisplay (e,f) -> EDisplay (loop e,f)
	| EDisplayNew t -> EDisplayNew (tpath t)
	| ETernary (e1,e2,e3) -> ETernary (loop e1,loop e2,loop e3)
	| ECheckType (e,t) -> ECheckType (loop e, ctype t)
	) in
	(e,p)