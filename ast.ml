(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

module IntMap = Map.Make(struct type t = int let compare a b = a - b end)

module Meta = struct
	type strict_meta =
		| Abi
		| Abstract
		| Access
		| Accessor
		| Allow
		| Analyzer
		| Annotation
		| ArrayAccess
		| Ast
		| AutoBuild
		| Bind
		| Bitmap
		| BridgeProperties
		| Build
		| BuildXml
		| Callable
		| Class
		| ClassCode
		| Commutative
		| CompilerGenerated
		| CoreApi
		| CoreType
		| CppFileCode
		| CppInclude
		| CppNamespaceCode
		| CsNative
		| Dce
		| Debug
		| Decl
		| DefParam
		| Delegate
		| Depend
		| Deprecated
		| DirectlyUsed
		| DynamicObject
		| Enum
		| EnumConstructorParam
		| Event
		| Exhaustive
		| Expose
		| Extern
		| FakeEnum
		| File
		| Final
		| FlatEnum
		| Font
		| Forward
		| From
		| FunctionCode
		| FunctionTailCode
		| Generic
		| GenericBuild
		| GenericInstance
		| Getter
		| Hack
		| HasUntyped
		| HaxeGeneric
		| HeaderClassCode
		| HeaderCode
		| HeaderInclude
		| HeaderNamespaceCode
		| HxGen
		| IfFeature
		| Impl
		| PythonImport
		| ImplicitCast
		| Include
		| InitPackage
		| Internal
		| IsVar
		| JavaCanonical
		| JavaNative
		| JsRequire
		| Keep
		| KeepInit
		| KeepSub
		| LibType
		| Meta
		| Macro
		| MaybeUsed
		| MergeBlock
		| MultiType
		| Native
		| NativeChildren
		| NativeGen
		| NativeGeneric
		| NativeProperty
		| NoCompletion
		| NoDebug
		| NoDoc
		| NoExpr
		| NoImportGlobal
		| NonVirtual
		| NoPackageRestrict
		| NoPrivateAccess
		| NoStack
		| NotNull
		| NoUsing
		| Ns
		| Op
		| Optional
		| Overload
		| PrivateAccess
		| Property
		| Protected
		| Public
		| PublicFields
		| QuotedField
		| ReadOnly
		| RealPath
		| Remove
		| Require
		| RequiresAssign
		| Resolve
		| ReplaceReflection
		| Rtti
		| Runtime
		| RuntimeValue
		| SelfCall
		| Setter
		| SkipCtor
		| SkipReflection
		| Sound
		| SourceFile
		| StoredTypedExpr
		| Strict
		| Struct
		| StructAccess
		| SuppressWarnings
		| This
		| Throws
		| To
		| ToString
		| Transient
		| ValueUsed
		| Volatile
		| Unbound
		| UnifyMinDynamic
		| Unreflective
		| Unsafe
		| Usage
		| Used
		| Value
		| Void
		| Last
		(* do not put any custom metadata after Last *)
		| Dollar of string
		| Custom of string

	let has m ml = List.exists (fun (m2,_,_) -> m = m2) ml
	let get m ml = List.find (fun (m2,_,_) -> m = m2) ml

	let to_string_ref = ref (fun _ -> assert false)
	let to_string (m : strict_meta) : string = !to_string_ref m
end

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
	| CTExtend of type_path list * class_field list
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
	| ESwitch of expr * (expr list * expr option * expr option) list * expr option option
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
	| EMeta of metadata_entry * expr

and expr = expr_def * pos

and type_param = {
	tp_name : string;
	tp_params :	type_param list;
	tp_constraints : complex_type list;
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

and class_field_kind =
	| FVar of complex_type option * expr option
	| FFun of func
	| FProp of string * string * complex_type option * expr option

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

type abstract_flag =
	| APrivAbstract
	| AFromType of complex_type
	| AToType of complex_type
	| AIsType of complex_type

type enum_constructor = {
	ec_name : string;
	ec_doc : documentation;
	ec_meta : metadata;
	ec_args : (string * bool * complex_type) list;
	ec_pos : pos;
	ec_params : type_param list;
	ec_type : complex_type option;
}

type ('a,'b) definition = {
	d_name : string;
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

type type_def =
	| EClass of (class_flag, class_field list) definition
	| EEnum of (enum_flag, enum_constructor list) definition
	| ETypedef of (enum_flag, complex_type) definition
	| EAbstract of (abstract_flag, class_field list) definition
	| EImport of (string * pos) list * import_mode
	| EUsing of type_path

type type_decl = type_def * pos

type package = string list * type_decl list

exception Error of string * pos

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
	| Increment | Decrement -> true
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

let rec punion_el el = match el with
	| [] ->
		null_pos
	| (_,p) :: [] ->
		p
	| (_,p) :: el ->
		punion p (punion_el el)

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let parse_path s =
	match List.rev (ExtString.String.nsplit s ".") with
	| [] -> failwith "Invalid empty path"
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
							raise Exit
					in
					let ub = UTF8.Buf.create 0 in
					UTF8.Buf.add_char ub (UChar.uchar_of_int u);
					Buffer.add_string b (UTF8.Buf.contents ub);
					inext := !inext + a;
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
			| FProp (get,set,t,e) -> FProp (get,set,opt ctype t,opt loop e))
		}
	and ctype = function
		| CTPath t -> CTPath (tpath t)
		| CTFunction (cl,c) -> CTFunction (List.map ctype cl, ctype c)
		| CTAnonymous fl -> CTAnonymous (List.map cfield fl)
		| CTParent t -> CTParent (ctype t)
		| CTExtend (tl,fl) -> CTExtend (List.map tpath tl, List.map cfield fl)
		| CTOptional t -> CTOptional (ctype t)
	and tparamdecl t =
		{ tp_name = t.tp_name; tp_constraints = List.map ctype t.tp_constraints; tp_params = List.map tparamdecl t.tp_params; tp_meta = t.tp_meta }
	and func f =
		{
			f_params = List.map tparamdecl f.f_params;
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
	| ESwitch (e,cases,def) -> ESwitch (loop e, List.map (fun (el,eg,e) -> List.map loop el, opt loop eg, opt loop e) cases, opt (opt loop) def)
	| ETry (e,catches) -> ETry (loop e, List.map (fun (n,t,e) -> n,ctype t,loop e) catches)
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
	| EMeta (m,e) -> EMeta(m, loop e)
	) in
	(e,p)

let s_expr e =
	let rec s_expr_inner tabs (e,_) =
		match e with
		| EConst c -> s_constant c
		| EArray (e1,e2) -> s_expr_inner tabs e1 ^ "[" ^ s_expr_inner tabs e2 ^ "]"
		| EBinop (op,e1,e2) -> s_expr_inner tabs e1 ^ " " ^ s_binop op ^ " " ^ s_expr_inner tabs e2
		| EField (e,f) -> s_expr_inner tabs e ^ "." ^ f
		| EParenthesis e -> "(" ^ (s_expr_inner tabs e) ^ ")"
		| EObjectDecl fl -> "{ " ^ (String.concat ", " (List.map (fun (n,e) -> n ^ " : " ^ (s_expr_inner tabs e)) fl)) ^ " }"
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
		| EIn (e1,e2) -> s_expr_inner tabs e1 ^ " in " ^ s_expr_inner tabs e2
		| EIf (e,e1,None) -> "if (" ^ s_expr_inner tabs e ^ ") " ^ s_expr_inner tabs e1
		| EIf (e,e1,Some e2) -> "if (" ^ s_expr_inner tabs e ^ ") " ^ s_expr_inner tabs e1 ^ " else " ^ s_expr_inner tabs e2
		| EWhile (econd,e,NormalWhile) -> "while (" ^ s_expr_inner tabs econd ^ ") " ^ s_expr_inner tabs e
		| EWhile (econd,e,DoWhile) -> "do " ^ s_expr_inner tabs e ^ " while (" ^ s_expr_inner tabs econd ^ ")"
		| ESwitch (e,cases,def) -> "switch " ^ s_expr_inner tabs e ^ " {\n\t" ^ tabs ^ String.concat ("\n\t" ^ tabs) (List.map (s_case tabs) cases) ^
			(match def with None -> "" | Some def -> "\n\t" ^ tabs ^ "default:" ^
			(match def with None -> "" | Some def -> s_expr_omit_block tabs def)) ^ "\n" ^ tabs ^ "}"
		| ETry (e,catches) -> "try " ^ s_expr_inner tabs e ^ String.concat "" (List.map (s_catch tabs) catches)
		| EReturn e -> "return" ^ s_opt_expr tabs e " "
		| EBreak -> "break"
		| EContinue -> "continue"
		| EUntyped e -> "untyped " ^ s_expr_inner tabs e
		| EThrow e -> "throw " ^ s_expr_inner tabs e
		| ECast (e,Some t) -> "cast (" ^ s_expr_inner tabs e ^ ", " ^ s_complex_type tabs t ^ ")"
		| ECast (e,None) -> "cast " ^ s_expr_inner tabs e
		| ETernary (e1,e2,e3) -> s_expr_inner tabs e1 ^ " ? " ^ s_expr_inner tabs e2 ^ " : " ^ s_expr_inner tabs e3
		| ECheckType (e,t) -> "(" ^ s_expr_inner tabs e ^ " : " ^ s_complex_type tabs t ^ ")"
		| EMeta (m,e) -> s_metadata tabs m ^ " " ^ s_expr_inner tabs e
		| _ -> ""
	and s_expr_list tabs el sep =
		(String.concat sep (List.map (s_expr_inner tabs) el))
	and s_complex_type_path tabs t =
		(String.concat "." t.tpackage) ^ if List.length t.tpackage > 0 then "." else "" ^
		t.tname ^
		match t.tsub with
		| Some s -> "." ^ s
		| None -> "" ^
		s_type_param_or_consts tabs t.tparams
	and s_type_param_or_consts tabs pl =
		if List.length pl > 0
		then "<" ^ (String.concat "," (List.map (s_type_param_or_const tabs) pl)) ^ ">"
		else ""
	and s_type_param_or_const tabs p =
		match p with
		| TPType t -> s_complex_type tabs t
		| TPExpr e -> s_expr_inner tabs e
	and s_complex_type tabs ct =
		match ct with
		| CTPath t -> s_complex_type_path tabs t
		| CTFunction (cl,c) -> if List.length cl > 0 then String.concat " -> " (List.map (s_complex_type tabs) cl) else "Void" ^ " -> " ^ s_complex_type tabs c
		| CTAnonymous fl -> "{ " ^ String.concat "; " (List.map (s_class_field tabs) fl) ^ "}";
		| CTParent t -> "(" ^ s_complex_type tabs t ^ ")"
		| CTOptional t -> "?" ^ s_complex_type tabs t
		| CTExtend (tl, fl) -> "{> " ^ String.concat " >, " (List.map (s_complex_type_path tabs) tl) ^ ", " ^ String.concat ", " (List.map (s_class_field tabs) fl) ^ " }"
	and s_class_field tabs f =
		match f.cff_doc with
		| Some s -> "/**\n\t" ^ tabs ^ s ^ "\n**/\n"
		| None -> "" ^
		if List.length f.cff_meta > 0 then String.concat ("\n" ^ tabs) (List.map (s_metadata tabs) f.cff_meta) else "" ^
		if List.length f.cff_access > 0 then String.concat " " (List.map s_access f.cff_access) else "" ^
		match f.cff_kind with
		| FVar (t,e) -> "var " ^ f.cff_name ^ s_opt_complex_type tabs t " : " ^ s_opt_expr tabs e " = "
		| FProp (get,set,t,e) -> "var " ^ f.cff_name ^ "(" ^ get ^ "," ^ set ^ ")" ^ s_opt_complex_type tabs t " : " ^ s_opt_expr tabs e " = "
		| FFun func -> "function " ^ f.cff_name ^ s_func tabs func
	and s_metadata tabs (s,e,_) =
		"@" ^ Meta.to_string s ^ if List.length e > 0 then "(" ^ s_expr_list tabs e ", " ^ ")" else ""
	and s_opt_complex_type tabs t pre =
		match t with
		| Some s -> pre ^ s_complex_type tabs s
		| None -> ""
	and s_opt_expr tabs e pre =
		match e with
		| Some s -> pre ^ s_expr_inner tabs s
		| None -> ""
	and s_func tabs f =
		s_type_param_list tabs f.f_params ^
		"(" ^ String.concat ", " (List.map (s_func_arg tabs) f.f_args) ^ ")" ^
		s_opt_complex_type tabs f.f_type ":" ^
		s_opt_expr tabs f.f_expr " "
	and s_type_param tabs t =
		t.tp_name ^ s_type_param_list tabs t.tp_params ^
		if List.length t.tp_constraints > 0 then ":(" ^ String.concat ", " (List.map (s_complex_type tabs) t.tp_constraints) ^ ")" else ""
	and s_type_param_list tabs tl =
		if List.length tl > 0 then "<" ^ String.concat ", " (List.map (s_type_param tabs) tl) ^ ">" else ""
	and s_func_arg tabs (n,o,t,e) =
		if o then "?" else "" ^ n ^ s_opt_complex_type tabs t ":" ^ s_opt_expr tabs e " = "
	and s_var tabs (n,t,e) =
		n ^ s_opt_complex_type tabs t ":" ^ s_opt_expr tabs e " = "
	and s_case tabs (el,e1,e2) =
		"case " ^ s_expr_list tabs el ", " ^
		(match e1 with None -> ":" | Some e -> " if (" ^ s_expr_inner tabs e ^ "):") ^
		(match e2 with None -> "" | Some e -> s_expr_omit_block tabs e)
	and s_catch tabs (n,t,e) =
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
			| (_,[EObjectDecl values,_],_) -> List.fold_left (fun acc (s,e) -> PMap.add s e acc) PMap.empty values
			| _ -> raise Not_found
		end
	with Not_found ->
		PMap.empty

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