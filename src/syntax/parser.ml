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

open Ast
open Globals
open DisplayPosition

type preprocessor_error =
	| InvalidEnd
	| InvalidElse
	| InvalidElseif
	| UnclosedConditional

type error_msg =
	| Unexpected of token
	| Duplicate_default
	| Missing_semicolon
	| Preprocessor_error of preprocessor_error
	| Unimplemented
	| Missing_type
	| Expected of string list
	| StreamError of string
	| Custom of string

type decl_flag =
	| DPrivate
	| DExtern
	| DFinal
	| DMacro
	| DDynamic
	| DInline
	| DPublic
	| DStatic
	| DOverload

type type_decl_completion_mode =
	| TCBeforePackage
	| TCAfterImport
	| TCAfterType

type syntax_completion =
	| SCComment
	| SCClassRelation
	| SCInterfaceRelation
	| SCTypeDecl of type_decl_completion_mode
	| SCAfterTypeFlag of decl_flag list

type 'a sequence_parsing_result =
	| Success of 'a
	| End of pos
	| Error of string

type syntax_completion_on = syntax_completion * DisplayTypes.completion_subject

exception Error of error_msg * pos
exception TypePath of string list * (string * bool) option * bool (* in import *) * pos
exception SyntaxCompletion of syntax_completion_on

type parser_config = {
	defines : Define.define;
	in_display : bool;
	in_display_file : bool;
	display_mode : DisplayTypes.DisplayMode.t;
	was_auto_triggered : bool;
	special_identifier_files : (Path.UniqueKey.t,string) ThreadSafeHashtbl.t option;
}

type parser_ctx = {
	lexer_ctx : Lexer.lexer_ctx;
	mutable syntax_errors : (error_msg * pos) list;
	mutable last_doc : (string * int) option;
	in_macro : bool;
	code : Sedlexing.lexbuf;
	mutable had_resume : bool;
	mutable delayed_syntax_completion : syntax_completion_on option;
	cache : (token * pos) DynArray.t;
	config : parser_config;
}

let error_msg = function
	| Unexpected (Kwd k) -> "Unexpected keyword \""^(s_keyword k)^"\""
	| Unexpected t -> "Unexpected "^(s_token t)
	| Duplicate_default -> "Duplicate default"
	| Missing_semicolon -> "Missing ;"
	| Preprocessor_error ppe ->
		begin match ppe with
			| UnclosedConditional -> "Unclosed conditional compilation block"
			| InvalidEnd -> "Invalid #end"
			| InvalidElse -> "Invalid #else"
			| InvalidElseif -> "Invalid #elseif"
		end
	| Unimplemented -> "Not implemented for current platform"
	| Missing_type -> "Missing type declaration"
	| Expected sl -> "Expected " ^ (String.concat " or " sl)
	| StreamError s -> s
	| Custom s -> s

type parse_data = string list * (type_def * pos) list

type parse_error = (error_msg * pos)

type parser_display_information = {
	pd_errors : parse_error list;
	pd_dead_blocks : (pos * expr) list;
	pd_conditions : expr list;
	pd_was_display_file : bool;
	pd_had_resume : bool;
	pd_delayed_syntax_completion : syntax_completion_on option;
}

type 'a parse_result =
	| ParseSuccess of 'a * parser_display_information
	| ParseError of 'a * parse_error * parse_error list

let create_context lexer_ctx config in_macro code = {
	lexer_ctx;
	syntax_errors = [];
	last_doc = None;
	in_macro;
	code;
	had_resume = false;
	delayed_syntax_completion = None;
	cache = DynArray.create ();
	config;
}

let create_config defines in_display in_display_file display_mode was_auto_triggered special_identifier_files = {
	defines;
	in_display;
	in_display_file;
	display_mode;
	was_auto_triggered;
	special_identifier_files;
}

let s_decl_flag = function
	| DPrivate -> "private"
	| DExtern -> "extern"
	| DFinal -> "final"
	| DMacro -> "macro"
	| DDynamic -> "dynamic"
	| DInline -> "inline"
	| DPublic -> "public"
	| DStatic -> "static"
	| DOverload -> "overload"

let syntax_completion kind so p =
	raise (SyntaxCompletion(kind,DisplayTypes.make_subject so p))

let error m p = raise (Error (m,p))

let last_token ctx s =
	let n = Stream.count s in
	DynArray.get ctx.cache (if n = 0 then 0 else n - 1)

let last_pos ctx s = pos (last_token ctx s)

let next_token ctx s = match Stream.peek s with
	| Some (Eof,p) ->
		(Eof,p)
	| Some tk -> tk
	| None ->
		let last_pos = pos (last_token ctx s) in
		(Eof,last_pos)

let next_pos ctx s = pos (next_token ctx s)

let reset_state () =
	display_position#reset

let syntax_error_with_pos ctx error_msg p v =
	let p = if p.pmax = max_int then {p with pmax = p.pmin + 1} else p in
	if not ctx.config.in_display then error error_msg p;
	ctx.syntax_errors <- (error_msg,p) :: ctx.syntax_errors;
	v

let syntax_error ctx error_msg ?(pos=None) s v =
	let p = (match pos with Some p -> p | None -> next_pos ctx s) in
	syntax_error_with_pos ctx error_msg p v

let handle_stream_error ctx msg s =
	let err,pos = if msg = "Parse error." then begin
		let tk,pos = next_token ctx s in
		(Unexpected tk),Some pos
	end else
		(StreamError msg),None
	in
	syntax_error ctx err ~pos s ()

let get_doc ctx s =
	(* do the peek first to make sure we fetch the doc *)
	match Stream.peek s with
	| None -> None
	| Some (tk,p) ->
		match ctx.last_doc with
		| None -> None
		| Some (d,pos) ->
			ctx.last_doc <- None;
			Some d

let unsupported_decl_flag decl flag pos ctx =
	let msg = (s_decl_flag flag) ^ " modifier is not supported for " ^ decl in
	syntax_error_with_pos ctx (Custom msg) pos None

let unsupported_decl_flag_class = unsupported_decl_flag "classes"
let unsupported_decl_flag_enum = unsupported_decl_flag "enums"
let unsupported_decl_flag_abstract = unsupported_decl_flag "abstracts"
let unsupported_decl_flag_typedef = unsupported_decl_flag "typedefs"
let unsupported_decl_flag_module_field = unsupported_decl_flag "module-level fields"

let decl_flag_to_class_flag ctx (flag,p) = match flag with
	| DPrivate -> Some HPrivate
	| DExtern -> Some HExtern
	| DFinal -> Some HFinal
	| DMacro | DDynamic | DInline | DPublic | DStatic | DOverload -> unsupported_decl_flag_class flag p ctx

let decl_flag_to_enum_flag ctx (flag,p) = match flag with
	| DPrivate -> Some EPrivate
	| DExtern -> Some EExtern
	| DFinal | DMacro | DDynamic | DInline | DPublic | DStatic | DOverload -> unsupported_decl_flag_enum flag p ctx

let decl_flag_to_abstract_flag ctx (flag,p) = match flag with
	| DPrivate -> Some AbPrivate
	| DExtern -> Some AbExtern
	| DFinal | DMacro | DDynamic | DInline | DPublic | DStatic | DOverload -> unsupported_decl_flag_abstract flag p ctx

let decl_flag_to_typedef_flag ctx (flag,p) = match flag with
	| DPrivate -> Some TDPrivate
	| DExtern -> Some TDExtern
	| DFinal | DMacro | DDynamic | DInline | DPublic | DStatic | DOverload -> unsupported_decl_flag_typedef flag p ctx

let decl_flag_to_module_field_flag ctx (flag,p) = match flag with
	| DPrivate -> Some (APrivate,p)
	| DMacro -> Some (AMacro,p)
	| DDynamic -> Some (ADynamic,p)
	| DInline -> Some (AInline,p)
	| DOverload -> Some (AOverload,p)
	| DExtern -> Some (AExtern,p)
	| DFinal | DPublic | DStatic -> unsupported_decl_flag_module_field flag p ctx

let serror() = raise (Stream.Error "Parse error.")

let magic_display_field_name = " - display - "
let magic_type_path = { tpackage = []; tname = ""; tparams = []; tsub = None }

let magic_type_ct p = make_ptp_ct magic_type_path p

let magic_type_th p = magic_type_ct p,p

let delay_syntax_completion ctx kind so p =
	ctx.delayed_syntax_completion <- Some(kind,DisplayTypes.make_subject so p)

let type_path sl in_import p = match sl with
	| n :: l when n.[0] >= 'A' && n.[0] <= 'Z' -> raise (TypePath (List.rev l,Some (n,false),in_import,p));
	| _ -> raise (TypePath (List.rev sl,None,in_import,p))

let would_skip_display_position ctx p1 plus_one s =
	if ctx.config.in_display_file then match Stream.npeek 1 s with
		| [ (_,p2) ] ->
			let p2 = {p2 with pmin = p1.pmax + (if plus_one then 1 else 0)} in
			display_position#enclosed_in p2
		| _ -> false
	else false

let cut_pos_at_display p = display_position#cut p

let is_dollar_ident e = match fst e with
	| EConst (Ident n) when starts_with n '$' ->
		true
	| _ ->
		false

let rev_concat s sl = String.concat s (List.rev sl)

let precedence op =
	let left = true and right = false in
	match op with
	| OpIn -> 0, right
	| OpMod -> 1, left
	| OpMult | OpDiv -> 2, left
	| OpAdd | OpSub -> 3, left
	| OpShl | OpShr | OpUShr -> 4, left
	| OpOr | OpAnd | OpXor -> 5, left
	| OpNullCoal -> 6, left
	| OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte -> 7, left
	| OpInterval -> 8, left
	| OpBoolAnd -> 9, left
	| OpBoolOr -> 10, left
	| OpArrow -> 11, right
	| OpAssign | OpAssignOp _ -> 12, right

let is_higher_than_ternary = function
	| OpAssign | OpAssignOp _ | OpArrow -> false
	| _ -> true

let swap op1 op2 =
	let p1, left1 = precedence op1 in
	let p2, _ = precedence op2 in
	left1 && p1 <= p2

let rec make_binop op e ((v,p2) as e2) =
	match v with
	| EBinop (_op,_e,_e2) when swap op _op ->
		let _e = make_binop op e _e in
		EBinop (_op,_e,_e2) , punion (pos _e) (pos _e2)
	| ETernary (e1,e2,e3) when is_higher_than_ternary op ->
		let e = make_binop op e e1 in
		ETernary (e,e2,e3) , punion (pos e) (pos e3)
	| _ ->
		EBinop (op,e,e2) , punion (pos e) (pos e2)

let rec make_unop op ((v,p2) as e) p1 =
	let neg s =
		if s.[0] = '-' then String.sub s 1 (String.length s - 1) else "-" ^ s
	in
	match v with
	| EBinop (bop,e,e2) -> EBinop (bop, make_unop op e p1 , e2) , (punion p1 p2)
	| ETernary (e1,e2,e3) -> ETernary (make_unop op e1 p1 , e2, e3), punion p1 p2
	| EIs (e, t) -> EIs (make_unop op e p1, t), punion p1 p2
	| EConst (Int (i, suffix)) when op = Neg -> EConst (Int (neg i, suffix)),punion p1 p2
	| EConst (Float (j, suffix)) when op = Neg -> EConst (Float (neg j, suffix)),punion p1 p2
	| _ -> EUnop (op,Prefix,e), punion p1 p2

let rec make_meta name params ((v,p2) as e) p1 =
	match v with
	| EBinop ((OpAssign | OpAssignOp _),_,_) -> EMeta((name,params,p1),e),punion p1 p2
	| EBinop (bop,e,e2) -> EBinop (bop, make_meta name params e p1 , e2) , (punion p1 p2)
	| ETernary (e1,e2,e3) -> ETernary (make_meta name params e1 p1 , e2, e3), punion p1 p2
	| _ -> EMeta((name,params,p1),e),punion p1 p2

let handle_xml_literal ctx p1 =
	Lexer.reset ctx.lexer_ctx;
	let i = Lexer.lex_xml ctx.lexer_ctx p1.pmin ctx.code in
	let xml = Lexer.contents ctx.lexer_ctx in
	let e = EConst (String(xml,SDoubleQuotes)),{p1 with pmax = i} in (* STRINGTODO: distinct kind? *)
	let e = make_meta Meta.Markup [] e p1 in
	e

let punion_next ctx p1 s =
	let _,p2 = next_token ctx s in
	{
		pfile = p1.pfile;
		pmin = p1.pmin;
		pmax = p2.pmax - 1;
	}

let mk_null_expr p = (EConst(Ident "null"),p)

let mk_display_expr e dk = (EDisplay(e,dk),(pos e))

let is_completion ctx =
	ctx.config.display_mode = DMDefault

let is_signature_display ctx =
	ctx.config.display_mode = DMSignature

let check_resume ctx p fyes fno =
	if is_completion ctx && ctx.config.in_display_file && p.pmax = (display_position#get).pmin then begin
		ctx.had_resume <- true;
		fyes()
	end else
		fno()

let check_resume_range ctx p s fyes fno =
	if is_completion ctx && ctx.config.in_display_file then begin
		let pnext = next_pos ctx s in
		if p.pmin < (display_position#get).pmin && pnext.pmin >= (display_position#get).pmax then
			fyes pnext
		else
			fno()
	end else
		fno()

let check_completion ctx p0 plus_one s =
	match Stream.peek s with
	| Some((Const(Ident name),p)) when display_position#enclosed_in p ->
		Stream.junk s;
		(Some(Some name,p))
	| _ ->
		if would_skip_display_position ctx p0 plus_one s then
			Some(None,DisplayPosition.display_position#with_pos p0)
		else
			None

let check_type_decl_flag_completion ctx mode flags s =
	if not ctx.config.in_display_file || not (is_completion ctx) then raise Stream.Failure;
	let mode () = match flags with
		| [] ->
			SCTypeDecl mode
		| flags ->
			let flags = List.map fst flags in
			SCAfterTypeFlag flags
	in
	match Stream.peek s with
		(* If there's an identifier coming up, it's probably an incomplete type
			declaration. Let's just raise syntax completion in that case because
			the parser would fail otherwise anyway. *)
		| Some((Const(Ident name),p)) when display_position#enclosed_in p -> syntax_completion (mode()) (Some name) p
		| _ -> match flags with
			| (_,p) :: _ when would_skip_display_position ctx p true s ->
				let flags = List.map fst flags in
				syntax_completion (SCAfterTypeFlag flags) None (DisplayPosition.display_position#with_pos p)
			| _ ->
				raise Stream.Failure

let check_type_decl_completion ctx mode pmax s =
	if ctx.config.in_display_file && is_completion ctx then begin
		let pmin = match Stream.peek s with
			| Some (Eof,_) | None -> max_int
			| Some tk -> (pos tk).pmin
		in
		let p = display_position#get in
		(* print_endline (Printf.sprintf "(%i <= %i) (%i >= %i)" pmax p.pmin pmin p.pmax); *)
		if pmax <= p.pmin && pmin >= p.pmax then begin
			let so,p = match Stream.peek s with
			| Some((Const(Ident name),p)) when display_position#enclosed_in p -> (Some name),p
			| Some(e,p) -> None,p
			| _ -> None,p
			in
			delay_syntax_completion ctx (SCTypeDecl mode) so p
		end
	end

let check_signature_mark ctx e p1 p2 =
	if not (is_signature_display ctx) then e
	else begin
		let p = punion p1 p2 in
		if true || not ctx.config.was_auto_triggered then begin (* TODO: #6383 *)
			if encloses_position_gt display_position#get p then (mk_display_expr e DKMarked)
			else e
		end else begin
			if (display_position#get).pmin = p1.pmax then (mk_display_expr e DKMarked)
			else e
		end
	end

let convert_abstract_flags ctx flags =
	ExtList.List.filter_map (decl_flag_to_abstract_flag ctx) flags

let no_keyword what s =
	match Stream.peek s with
	| Some (Kwd kwd,p) -> error (Custom ("Keyword " ^ (s_keyword kwd) ^ " cannot be used as " ^ what)) p
	| _ -> raise Stream.Failure
