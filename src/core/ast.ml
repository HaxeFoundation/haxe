(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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

open Globals

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
	| Inline
	| Using
	| Null
	| True
	| False
	| Abstract
	| Macro
	| Final

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
	| OpArrow
	| OpIn

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
	| Sharp of string
	| Question
	| At
	| Dollar of string

type unop_flag =
	| Prefix
	| Postfix

type while_flag =
	| NormalWhile
	| DoWhile

type quote_status =
	| NoQuotes
	| DoubleQuotes

type type_path = {
	tpackage : string list;
	tname : string;
	tparams : type_param_or_const list;
	tsub : string option;
}

and placed_type_path = type_path * pos

and type_param_or_const =
	| TPType of type_hint
	| TPExpr of expr

and complex_type =
	| CTPath of type_path
	| CTFunction of type_hint list * type_hint
	| CTAnonymous of class_field list
	| CTParent of type_hint
	| CTExtend of placed_type_path list * class_field list
	| CTOptional of type_hint
	| CTNamed of placed_name * type_hint

and type_hint = complex_type * pos

and func = {
	f_params : type_param list;
	f_args : (placed_name * bool * metadata * type_hint option * expr option) list;
	f_type : type_hint option;
	f_expr : expr option;
}

and placed_name = string * pos

and expr_def =
	| EConst of constant
	| EArray of expr * expr
	| EBinop of binop * expr * expr
	| EField of expr * string
	| EParenthesis of expr
	| EObjectDecl of ((string * pos * quote_status) * expr) list
	| EArrayDecl of expr list
	| ECall of expr * expr list
	| ENew of placed_type_path * expr list
	| EUnop of unop * unop_flag * expr
	| EVars of (placed_name * type_hint option * expr option) list
	| EFunction of string option * func
	| EBlock of expr list
	| EFor of expr * expr
	| EIf of expr * expr * expr option
	| EWhile of expr * expr * while_flag
	| ESwitch of expr * (expr list * expr option * expr option * pos) list * (expr option * pos) option
	| ETry of expr * (placed_name * type_hint * expr * pos) list
	| EReturn of expr option
	| EBreak
	| EContinue
	| EUntyped of expr
	| EThrow of expr
	| ECast of expr * type_hint option
	| EDisplay of expr * bool
	| EDisplayNew of placed_type_path
	| ETernary of expr * expr * expr
	| ECheckType of expr * type_hint
	| EMeta of metadata_entry * expr
	| EComplexType of complex_type

and expr = expr_def * pos

and type_param = {
	tp_name : placed_name;
	tp_params :	type_param list;
	tp_constraints : type_hint list;
	tp_meta : metadata;
}

and documentation = string option

and metadata_entry = (Meta.strict_meta * expr list * pos)
and metadata = metadata_entry list

and access =
	| APublic
	| APrivate
	| AStatic
	| AOverride
	| ADynamic
	| AInline
	| AMacro
	| AFinal

and class_field_kind =
	| FVar of type_hint option * expr option
	| FFun of func
	| FProp of placed_name * placed_name * type_hint option * expr option

and class_field = {
	cff_name : placed_name;
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
	| HExtends of placed_type_path
	| HImplements of placed_type_path

type abstract_flag =
	| APrivAbstract
	| AFromType of type_hint
	| AToType of type_hint
	| AIsType of type_hint
	| AExtern

type enum_constructor = {
	ec_name : placed_name;
	ec_doc : documentation;
	ec_meta : metadata;
	ec_args : (string * bool * type_hint) list;
	ec_pos : pos;
	ec_params : type_param list;
	ec_type : type_hint option;
}

type ('a,'b) definition = {
	d_name : placed_name;
	d_doc : documentation;
	d_params : type_param list;
	d_meta : metadata;
	d_flags : 'a list;
	d_data : 'b;
}

type import_mode =
	| INormal
	| IAsName of string
	| IAll

type import = placed_name list * import_mode

type type_def =
	| EClass of (class_flag, class_field list) definition
	| EEnum of (enum_flag, enum_constructor list) definition
	| ETypedef of (enum_flag, type_hint) definition
	| EAbstract of (abstract_flag, class_field list) definition
	| EImport of import
	| EUsing of placed_name list

type type_decl = type_def * pos

type package = string list * type_decl list

let is_lower_ident i =
	let rec loop p =
		match String.unsafe_get i p with
		| 'a'..'z' -> true
		| '_' -> if p + 1 < String.length i then loop (p + 1) else true
		| _ -> false
	in
	loop 0

let pos = snd

let rec is_postfix (e,_) op = match op with
	| Increment | Decrement | Not -> true
	| Neg | NegBits -> false

let is_prefix = function
	| Increment | Decrement -> true
	| Not | Neg | NegBits -> true

let base_class_name = snd

let punion p p2 =
	{
		pfile = p.pfile;
		pmin = min p.pmin p2.pmin;
		pmax = max p.pmax p2.pmax;
	}

let rec punion_el el = match el with
	| [] ->
		null_pos
	| (_,p) :: [] ->
		p
	| (_,p) :: el ->
		punion p (punion_el el)

let parse_path s =
	match List.rev (ExtString.String.nsplit s ".") with
	| [] -> [],"" (* This is how old extlib behaved. *)
	| x :: l -> List.rev l, x

let s_escape ?(hex=true) s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| c when int_of_char c < 32 && hex -> Buffer.add_string b (Printf.sprintf "\\x%.2X" (int_of_char c))
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let s_constant = function
	| Int s -> s
	| Float s -> s
	| String s -> "\"" ^ s_escape s ^ "\""
	| Ident s -> s
	| Regexp (r,o) -> "~/" ^ r ^ "/"

let s_access = function
	| APublic -> "public"
	| APrivate -> "private"
	| AStatic -> "static"
	| AOverride -> "override"
	| ADynamic -> "dynamic"
	| AInline -> "inline"
	| AMacro -> "macro"
	| AFinal -> "final"

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
	| Inline -> "inline"
	| Using -> "using"
	| Null -> "null"
	| True -> "true"
	| False -> "false"
	| Abstract -> "abstract"
	| Macro -> "macro"
	| Final -> "final"

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
	| OpArrow -> "=>"
	| OpIn -> " in "

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
	| Sharp s -> "#" ^ s
	| Question -> "?"
	| At -> "@"
	| Dollar v -> "$" ^ v

exception Invalid_escape_sequence of char * int

let unescape s =
	let b = Buffer.create 0 in
	let rec loop esc i =
		if i = String.length s then
			()
		else
			let c = s.[i] in
			let fail () = raise (Invalid_escape_sequence(c,i)) in
			if esc then begin
				let inext = ref (i + 1) in
				(match c with
				| 'n' -> Buffer.add_char b '\n'
				| 'r' -> Buffer.add_char b '\r'
				| 't' -> Buffer.add_char b '\t'
				| '"' | '\'' | '\\' -> Buffer.add_char b c
				| '0'..'3' ->
					let c = (try char_of_int (int_of_string ("0o" ^ String.sub s i 3)) with _ -> fail()) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| 'x' ->
					let c = (try char_of_int (int_of_string ("0x" ^ String.sub s (i+1) 2)) with _ -> fail()) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| 'u' ->
					let (u, a) =
						try
							(int_of_string ("0x" ^ String.sub s (i+1) 4), 4)
						with _ -> try
							assert (s.[i+1] = '{');
							let l = String.index_from s (i+3) '}' - (i+2) in
							let u = int_of_string ("0x" ^ String.sub s (i+2) l) in
							assert (u <= 0x10FFFF);
							(u, l+2)
						with _ ->
							fail()
					in
					let ub = UTF8.Buf.create 0 in
					UTF8.Buf.add_char ub (UChar.uchar_of_int u);
					Buffer.add_string b (UTF8.Buf.contents ub);
					inext := !inext + a;
				| _ ->
					fail());
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
		| TPType t -> TPType (type_hint t)
		| TPExpr e -> TPExpr (loop e)
	and cfield f =
		{ f with cff_kind = (match f.cff_kind with
			| FVar (t,e) ->
				let t = opt type_hint t in
				let e = opt loop e in
				FVar (t,e)
			| FFun f -> FFun (func f)
			| FProp (get,set,t,e) ->
				let t = opt type_hint t in
				let e = opt loop e in
				FProp (get,set,t,e))
		}
	and type_hint (t,p) = (match t with
		| CTPath t -> CTPath { t with tparams = List.map tparam t.tparams }
		| CTFunction (cl,c) ->
			let cl = List.map type_hint cl in
			let c = type_hint c in
			CTFunction (cl,c)
		| CTAnonymous fl -> CTAnonymous (List.map cfield fl)
		| CTParent t -> CTParent (type_hint t)
		| CTExtend (tl,fl) ->
			let tl = List.map tpath tl in
			let fl = List.map cfield fl in
			CTExtend (tl,fl)
		| CTOptional t -> CTOptional (type_hint t)
		| CTNamed (n,t) -> CTNamed (n,type_hint t)
		),p
	and tparamdecl t =
		let constraints = List.map type_hint t.tp_constraints in
		let params = List.map tparamdecl t.tp_params in
		{ tp_name = t.tp_name; tp_constraints = constraints; tp_params = params; tp_meta = t.tp_meta }
	and func f =
		let params = List.map tparamdecl f.f_params in
		let args = List.map (fun (n,o,m,t,e) ->
			let t = opt type_hint t in
			let e = opt loop e in
			n,o,m,t,e
		) f.f_args in
		let t = opt type_hint f.f_type in
		let e = opt loop f.f_expr in
		{
			f_params = params;
			f_args = args;
			f_type = t;
			f_expr = e;
		}
	and tpath (t,p) = { t with tparams = List.map tparam t.tparams },p
	in
	let e = (match e with
	| EConst _ | EComplexType _ -> e
	| EArray (e1,e2) ->
		let e1 = loop e1 in
		let e2 = loop e2 in
		EArray (e1,e2)
	| EBinop (op,e1,e2) ->
		let e1 = loop e1 in
		let e2 = loop e2 in
		EBinop (op,e1,e2)
	| EField (e,f) -> EField (loop e, f)
	| EParenthesis e -> EParenthesis (loop e)
	| EObjectDecl fl -> EObjectDecl (List.map (fun (k,e) -> k,loop e) fl)
	| EArrayDecl el -> EArrayDecl (List.map loop el)
	| ECall (e,el) ->
		let e = loop e in
		let el = List.map loop el in
		ECall (e,el)
	| ENew (t,el) ->
		let t = tpath t in
		let el = List.map loop el in
		ENew (t,el)
	| EUnop (op,f,e) -> EUnop (op,f,loop e)
	| EVars vl ->
		EVars (List.map (fun (n,t,eo) ->
			let t = opt type_hint t in
			let eo = opt loop eo in
			n,t,eo
		) vl)
	| EFunction (n,f) -> EFunction (n,func f)
	| EBlock el -> EBlock (List.map loop el)
	| EFor (e1,e2) ->
		let e1 = loop e1 in
		let e2 = loop e2 in
		EFor (e1,e2)
	| EIf (e,e1,e2) ->
		let e = loop e in
		let e1 = loop e1 in
		let e2 = opt loop e2 in
		EIf (e,e1,e2)
	| EWhile (econd,e,f) ->
		let econd = loop econd in
		let e = loop e in
		EWhile (econd,e,f)
	| ESwitch (e,cases,def) ->
		let e = loop e in
		let cases = List.map (fun (el,eg,e,p) ->
			let el = List.map loop el in
			let eg = opt loop eg in
			let e = opt loop e in
			el,eg,e,p
		) cases in
		let def = opt (fun (eo,p) -> opt loop eo,p) def in
		ESwitch (e, cases, def)
	| ETry (e,catches) ->
		let e = loop e in
		let catches = List.map (fun (n,t,e,p) -> n,type_hint t,loop e,p) catches in
		ETry (e,catches)
	| EReturn e -> EReturn (opt loop e)
	| EBreak -> EBreak
	| EContinue -> EContinue
	| EUntyped e -> EUntyped (loop e)
	| EThrow e -> EThrow (loop e)
	| ECast (e,t) ->
		let e = loop e in
		let t = opt type_hint t in
		ECast (e,t)
	| EDisplay (e,f) -> EDisplay (loop e,f)
	| EDisplayNew t -> EDisplayNew (tpath t)
	| ETernary (e1,e2,e3) ->
		let e1 = loop e1 in
		let e2 = loop e2 in
		let e3 = loop e3 in
		ETernary (e1,e2,e3)
	| ECheckType (e,t) ->
		let e = loop e in
		let t = type_hint t in
		ECheckType (e,t)
	| EMeta (m,e) -> EMeta(m, loop e)
	) in
	(e,p)

let iter_expr loop (e,p) =
	let opt eo = match eo with None -> () | Some e -> loop e in
	let exprs = List.iter loop in
	match e with
	| EConst _ | EContinue | EBreak | EDisplayNew _ | EReturn None | EComplexType _ -> ()
	| EParenthesis e1 | EField(e1,_) | EUnop(_,_,e1) | EReturn(Some e1) | EThrow e1 | EMeta(_,e1)
	| ECheckType(e1,_) | EDisplay(e1,_) | ECast(e1,_) | EUntyped e1 -> loop e1;
	| EArray(e1,e2) | EBinop(_,e1,e2) | EFor(e1,e2) | EWhile(e1,e2,_) | EIf(e1,e2,None) -> loop e1; loop e2;
	| ETernary(e1,e2,e3) | EIf(e1,e2,Some e3) -> loop e1; loop e2; loop e3;
	| EArrayDecl el | ENew(_,el) | EBlock el -> List.iter loop el
	| ECall(e1,el) -> loop e1; exprs el;
	| EObjectDecl fl -> List.iter (fun (_,e) -> loop e) fl;
	| ETry(e1,catches) ->
		loop e1;
		List.iter (fun (_,_,e,_) -> loop e) catches
	| ESwitch(e1,cases,def) ->
		loop e1;
		List.iter (fun (el,eg,e,_) ->
			exprs el;
			opt eg;
			opt e;
		) cases;
		(match def with None -> () | Some (e,_) -> opt e);
	| EFunction(_,f) ->
		List.iter (fun (_,_,_,_,eo) -> opt eo) f.f_args;
		opt f.f_expr
	| EVars vl -> List.iter (fun (_,_,eo) -> opt eo) vl

let s_object_key_name name =  function
	| DoubleQuotes -> "\"" ^ s_escape name ^ "\""
	| NoQuotes -> name

let s_expr e =
	let rec s_expr_inner tabs (e,_) =
		match e with
		| EConst c -> s_constant c
		| EArray (e1,e2) -> s_expr_inner tabs e1 ^ "[" ^ s_expr_inner tabs e2 ^ "]"
		| EBinop (op,e1,e2) -> s_expr_inner tabs e1 ^ " " ^ s_binop op ^ " " ^ s_expr_inner tabs e2
		| EField (e,f) -> s_expr_inner tabs e ^ "." ^ f
		| EParenthesis e -> "(" ^ (s_expr_inner tabs e) ^ ")"
		| EObjectDecl fl -> "{ " ^ (String.concat ", " (List.map (fun ((n,_,qs),e) -> (s_object_key_name n qs) ^ " : " ^ (s_expr_inner tabs e)) fl)) ^ " }"
		| EArrayDecl el -> "[" ^ s_expr_list tabs el ", " ^ "]"
		| ECall (e,el) -> s_expr_inner tabs e ^ "(" ^ s_expr_list tabs el ", " ^ ")"
		| ENew (t,el) -> "new " ^ s_complex_type_path tabs t ^ "(" ^ s_expr_list tabs el ", " ^ ")"
		| EUnop (op,Postfix,e) -> s_expr_inner tabs e ^ s_unop op
		| EUnop (op,Prefix,e) -> s_unop op ^ s_expr_inner tabs e
		| EFunction (Some n,f) -> "function " ^ n ^ s_func tabs f
		| EFunction (None,f) -> "function" ^ s_func tabs f
		| EVars vl -> "var " ^ String.concat ", " (List.map (s_var tabs) vl)
		| EBlock [] -> "{ }"
		| EBlock el -> s_block tabs el "{" "\n" "}"
		| EFor (e1,e2) -> "for (" ^ s_expr_inner tabs e1 ^ ") " ^ s_expr_inner tabs e2
		| EIf (e,e1,None) -> "if (" ^ s_expr_inner tabs e ^ ") " ^ s_expr_inner tabs e1
		| EIf (e,e1,Some e2) -> "if (" ^ s_expr_inner tabs e ^ ") " ^ s_expr_inner tabs e1 ^ " else " ^ s_expr_inner tabs e2
		| EWhile (econd,e,NormalWhile) -> "while (" ^ s_expr_inner tabs econd ^ ") " ^ s_expr_inner tabs e
		| EWhile (econd,e,DoWhile) -> "do " ^ s_expr_inner tabs e ^ " while (" ^ s_expr_inner tabs econd ^ ")"
		| ESwitch (e,cases,def) -> "switch " ^ s_expr_inner tabs e ^ " {\n\t" ^ tabs ^ String.concat ("\n\t" ^ tabs) (List.map (s_case tabs) cases) ^
			(match def with None -> "" | Some (def,_) -> "\n\t" ^ tabs ^ "default:" ^
			(match def with None -> "" | Some def -> s_expr_omit_block tabs def)) ^ "\n" ^ tabs ^ "}"
		| ETry (e,catches) -> "try " ^ s_expr_inner tabs e ^ String.concat "" (List.map (s_catch tabs) catches)
		| EReturn e -> "return" ^ s_opt_expr tabs e " "
		| EBreak -> "break"
		| EContinue -> "continue"
		| EUntyped e -> "untyped " ^ s_expr_inner tabs e
		| EThrow e -> "throw " ^ s_expr_inner tabs e
		| ECast (e,Some (t,_)) -> "cast (" ^ s_expr_inner tabs e ^ ", " ^ s_complex_type tabs t ^ ")"
		| ECast (e,None) -> "cast " ^ s_expr_inner tabs e
		| ETernary (e1,e2,e3) -> s_expr_inner tabs e1 ^ " ? " ^ s_expr_inner tabs e2 ^ " : " ^ s_expr_inner tabs e3
		| ECheckType (e,(t,_)) -> "(" ^ s_expr_inner tabs e ^ " : " ^ s_complex_type tabs t ^ ")"
		| EMeta (m,e) -> s_metadata tabs m ^ " " ^ s_expr_inner tabs e
		| EComplexType t -> ":" ^ s_complex_type tabs t
		| EDisplay (e1,iscall) -> Printf.sprintf "#DISPLAY(%s, %b)" (s_expr_inner tabs e1) iscall
		| EDisplayNew tp -> Printf.sprintf "#DISPLAY_NEW(%s)" (s_complex_type_path tabs tp)
	and s_expr_list tabs el sep =
		(String.concat sep (List.map (s_expr_inner tabs) el))
	and s_complex_type_path tabs (t,_) =
		Printf.sprintf "%s%s%s"
			(s_type_path (t.tpackage,t.tname))
			(Option.map_default (fun s -> "." ^ s) "" t.tsub)
			(s_type_param_or_consts tabs t.tparams)
	and s_type_param_or_consts tabs pl =
		if List.length pl > 0
		then "<" ^ (String.concat "," (List.map (s_type_param_or_const tabs) pl)) ^ ">"
		else ""
	and s_type_param_or_const tabs p =
		match p with
		| TPType (t,_) -> s_complex_type tabs t
		| TPExpr e -> s_expr_inner tabs e
	and s_complex_type tabs ct =
		match ct with
		| CTPath t -> s_complex_type_path tabs (t,null_pos)
		| CTFunction (cl,(c,_)) -> if List.length cl > 0 then String.concat " -> " (List.map (fun (t,_) -> s_complex_type tabs t) cl) else "Void" ^ " -> " ^ s_complex_type tabs c
		| CTAnonymous fl -> "{ " ^ String.concat "; " (List.map (s_class_field tabs) fl) ^ "}";
		| CTParent(t,_) -> "(" ^ s_complex_type tabs t ^ ")"
		| CTOptional(t,_) -> "?" ^ s_complex_type tabs t
		| CTNamed((n,_),(t,_)) -> n ^ ":" ^ s_complex_type tabs t
		| CTExtend (tl, fl) -> "{> " ^ String.concat " >, " (List.map (s_complex_type_path tabs) tl) ^ ", " ^ String.concat ", " (List.map (s_class_field tabs) fl) ^ " }"
	and s_class_field tabs f =
		match f.cff_doc with
		| Some s -> "/**\n\t" ^ tabs ^ s ^ "\n**/\n"
		| None -> "" ^
		if List.length f.cff_meta > 0 then String.concat ("\n" ^ tabs) (List.map (s_metadata tabs) f.cff_meta) else "" ^
		if List.length f.cff_access > 0 then String.concat " " (List.map s_access f.cff_access) else "" ^
		match f.cff_kind with
		| FVar (t,e) -> "var " ^ (fst f.cff_name) ^ s_opt_type_hint tabs t " : " ^ s_opt_expr tabs e " = "
		| FProp ((get,_),(set,_),t,e) -> "var " ^ (fst f.cff_name) ^ "(" ^ get ^ "," ^ set ^ ")" ^ s_opt_type_hint tabs t " : " ^ s_opt_expr tabs e " = "
		| FFun func -> "function " ^ (fst f.cff_name) ^ s_func tabs func
	and s_metadata tabs (s,e,_) =
		"@" ^ Meta.to_string s ^ if List.length e > 0 then "(" ^ s_expr_list tabs e ", " ^ ")" else ""
	and s_opt_expr tabs e pre =
		match e with
		| Some s -> pre ^ s_expr_inner tabs s
		| None -> ""
	and s_opt_type_hint tabs t pre =
		match t with
		| Some(t,_) -> pre ^ s_complex_type tabs t
		| None -> ""
	and s_func tabs f =
		s_type_param_list tabs f.f_params ^
		"(" ^ String.concat ", " (List.map (s_func_arg tabs) f.f_args) ^ ")" ^
		s_opt_type_hint tabs f.f_type ":" ^
		s_opt_expr tabs f.f_expr " "
	and s_type_param tabs t =
		fst (t.tp_name) ^ s_type_param_list tabs t.tp_params ^
		if List.length t.tp_constraints > 0 then ":(" ^ String.concat ", " (List.map ((fun (t,_) -> s_complex_type tabs t)) t.tp_constraints) ^ ")" else ""
	and s_type_param_list tabs tl =
		if List.length tl > 0 then "<" ^ String.concat ", " (List.map (s_type_param tabs) tl) ^ ">" else ""
	and s_func_arg tabs ((n,_),o,_,t,e) =
		if o then "?" else "" ^ n ^ s_opt_type_hint tabs t ":" ^ s_opt_expr tabs e " = "
	and s_var tabs ((n,_),t,e) =
		n ^ (s_opt_type_hint tabs t ":") ^ s_opt_expr tabs e " = "
	and s_case tabs (el,e1,e2,_) =
		"case " ^ s_expr_list tabs el ", " ^
		(match e1 with None -> ":" | Some e -> " if (" ^ s_expr_inner tabs e ^ "):") ^
		(match e2 with None -> "" | Some e -> s_expr_omit_block tabs e)
	and s_catch tabs ((n,_),(t,_),e,_) =
		" catch(" ^ n ^ ":" ^ s_complex_type tabs t ^ ") " ^ s_expr_inner tabs e
	and s_block tabs el opn nl cls =
		 opn ^ "\n\t" ^ tabs ^ (s_expr_list (tabs ^ "\t") el (";\n\t" ^ tabs)) ^ ";" ^ nl ^ tabs ^ cls
	and s_expr_omit_block tabs e =
		match e with
		| (EBlock [],_) -> ""
		| (EBlock el,_) -> s_block (tabs ^ "\t") el "" "" ""
		| _ -> s_expr_inner (tabs ^ "\t") e ^ ";"
	in s_expr_inner "" e

let get_value_meta meta =
	try
		begin match Meta.get Meta.Value meta with
			| (_,[EObjectDecl values,_],_) -> List.fold_left (fun acc ((s,_,_),e) -> PMap.add s e acc) PMap.empty values
			| _ -> raise Not_found
		end
	with Not_found ->
		PMap.empty

(* Type path related functions *)

let rec string_list_of_expr_path_raise (e,p) =
	match e with
	| EConst (Ident i) -> [i]
	| EField (e,f) -> f :: string_list_of_expr_path_raise e
	| _ -> raise Exit

let expr_of_type_path (sl,s) p =
	match sl with
	| [] -> (EConst(Ident s),p)
	| s1 :: sl ->
		let e1 = (EConst(Ident s1),p) in
		let e = List.fold_left (fun e s -> (EField(e,s),p)) e1 sl in
		EField(e,s),p

let match_path recursive sl sl_pattern =
	let rec loop sl1 sl2 = match sl1,sl2 with
		| [],[] ->
			true
		(* always recurse into types of package paths *)
		| (s1 :: s11 :: _),[s2] when is_lower_ident s2 && not (is_lower_ident s11)->
			s1 = s2
		| [_],[""] ->
			true
		| _,([] | [""]) ->
			recursive
		| [],_ ->
			false
		| (s1 :: sl1),(s2 :: sl2) ->
			s1 = s2 && loop sl1 sl2
	in
	loop sl sl_pattern

let full_dot_path mpath tpath =
	if mpath = tpath then
		(fst tpath) @ [snd tpath]
	else
		(fst mpath) @ [snd mpath;snd tpath]

let safe_for_all2 f a b =
	try List.for_all2 f a b with _ -> false

let rec remove_duplicates f l = match l with
	| [] -> []
	| x :: l -> x :: (remove_duplicates f (List.filter (fun x' -> f x x') l))

module Expr = struct
	let ensure_block e = match fst e with
		| EBlock _ -> e
		| _ -> (EBlock [e],pos e)

	let field_assoc name fl =
		let rec loop fl = match fl with
			| ((name',_,_),e) :: fl -> if name' = name then e else loop fl
			| [] -> raise Not_found
		in
		loop fl

	let field_mem_assoc name fl =
		let rec loop fl = match fl with
			| ((name',_,_),e) :: fl -> if name' = name then raise Exit else loop fl
			| [] -> false
		in
		try
			loop fl
		with Exit ->
			true
end
