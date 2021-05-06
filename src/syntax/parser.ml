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
open Reification
open DisplayTypes.DisplayMode
open DisplayPosition

type error_msg =
	| Unexpected of token
	| Duplicate_default
	| Missing_semicolon
	| Unclosed_conditional
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

exception Error of error_msg * pos
exception TypePath of string list * (string * bool) option * bool (* in import *) * pos
exception SyntaxCompletion of syntax_completion * DisplayTypes.completion_subject

let error_msg = function
	| Unexpected (Kwd k) -> "Unexpected keyword \""^(s_keyword k)^"\""
	| Unexpected t -> "Unexpected "^(s_token t)
	| Duplicate_default -> "Duplicate default"
	| Missing_semicolon -> "Missing ;"
	| Unclosed_conditional -> "Unclosed conditional compilation block"
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
}

type 'a parse_result =
	(* Parsed non-display-file without errors. *)
	| ParseSuccess of 'a * bool * parser_display_information
	(* Parsed non-display file with errors *)
	| ParseError of 'a * parse_error * parse_error list

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

let special_identifier_files : (Path.UniqueKey.t,string) Hashtbl.t = Hashtbl.create 0

module TokenCache = struct
	let cache = ref (DynArray.create ())
	let add (token : (token * pos)) = DynArray.add (!cache) token
	let get index = DynArray.get (!cache) index
	let clear () =
		let old_cache = !cache in
		cache := DynArray.create ();
		(fun () -> cache := old_cache)
end

let last_token s =
	let n = Stream.count s in
	TokenCache.get (if n = 0 then 0 else n - 1)

let last_pos s = pos (last_token s)

let next_token s = match Stream.peek s with
	| Some (Eof,p) ->
		(Eof,p)
	| Some tk -> tk
	| None ->
		let last_pos = pos (last_token s) in
		(Eof,last_pos)

let next_pos s = pos (next_token s)

(* Global state *)

let in_display = ref false
let was_auto_triggered = ref false
let display_mode = ref DMNone
let in_macro = ref false
let had_resume = ref false
let code_ref = ref (Sedlexing.Utf8.from_string "")
let delayed_syntax_completion : (syntax_completion * DisplayTypes.completion_subject) option ref = ref None

let reset_state () =
	in_display := false;
	was_auto_triggered := false;
	display_mode := DMNone;
	display_position#reset;
	in_macro := false;
	had_resume := false;
	code_ref := Sedlexing.Utf8.from_string "";
	delayed_syntax_completion := None

(* Per-file state *)

let in_display_file = ref false
let last_doc : (string * int) option ref = ref None
let syntax_errors = ref []

let syntax_error_with_pos error_msg p v =
	let p = if p.pmax = max_int then {p with pmax = p.pmin + 1} else p in
	if not !in_display then error error_msg p;
	syntax_errors := (error_msg,p) :: !syntax_errors;
	v

let syntax_error error_msg ?(pos=None) s v =
	let p = (match pos with Some p -> p | None -> next_pos s) in
	syntax_error_with_pos error_msg p v

let handle_stream_error msg s =
	let err,pos = if msg = "" then begin
		let tk,pos = next_token s in
		(Unexpected tk),Some pos
	end else
		(StreamError msg),None
	in
	syntax_error err ~pos s ()

let get_doc s =
	(* do the peek first to make sure we fetch the doc *)
	match Stream.peek s with
	| None -> None
	| Some (tk,p) ->
		match !last_doc with
		| None -> None
		| Some (d,pos) ->
			last_doc := None;
			if pos = p.pmin then Some d else None

let unsupported_decl_flag decl flag pos =
	let msg = (s_decl_flag flag) ^ " modifier is not supported for " ^ decl in
	syntax_error_with_pos (Custom msg) pos None

let unsupported_decl_flag_class = unsupported_decl_flag "classes"
let unsupported_decl_flag_enum = unsupported_decl_flag "enums"
let unsupported_decl_flag_abstract = unsupported_decl_flag "abstracts"
let unsupported_decl_flag_module_field = unsupported_decl_flag "module-level fields"

let decl_flag_to_class_flag (flag,p) = match flag with
	| DPrivate -> Some HPrivate
	| DExtern -> Some HExtern
	| DFinal -> Some HFinal
	| DMacro | DDynamic | DInline | DPublic | DStatic | DOverload -> unsupported_decl_flag_class flag p

let decl_flag_to_enum_flag (flag,p) = match flag with
	| DPrivate -> Some EPrivate
	| DExtern -> Some EExtern
	| DFinal | DMacro | DDynamic | DInline | DPublic | DStatic | DOverload -> unsupported_decl_flag_enum flag p

let decl_flag_to_abstract_flag (flag,p) = match flag with
	| DPrivate -> Some AbPrivate
	| DExtern -> Some AbExtern
	| DFinal | DMacro | DDynamic | DInline | DPublic | DStatic | DOverload -> unsupported_decl_flag_abstract flag p

let decl_flag_to_module_field_flag (flag,p) = match flag with
	| DPrivate -> Some (APrivate,p)
	| DMacro -> Some (AMacro,p)
	| DDynamic -> Some (ADynamic,p)
	| DInline -> Some (AInline,p)
	| DOverload -> Some (AOverload,p)
	| DExtern -> Some (AExtern,p)
	| DFinal | DPublic | DStatic -> unsupported_decl_flag_module_field flag p

let serror() = raise (Stream.Error "")

let magic_display_field_name = " - display - "
let magic_type_path = { tpackage = []; tname = ""; tparams = []; tsub = None }

let delay_syntax_completion kind so p =
	delayed_syntax_completion := Some(kind,DisplayTypes.make_subject so p)

let type_path sl in_import p = match sl with
	| n :: l when n.[0] >= 'A' && n.[0] <= 'Z' -> raise (TypePath (List.rev l,Some (n,false),in_import,p));
	| _ -> raise (TypePath (List.rev sl,None,in_import,p))

let would_skip_display_position p1 plus_one s =
	if !in_display_file then match Stream.npeek 1 s with
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
	| OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte -> 6, left
	| OpInterval -> 7, left
	| OpBoolAnd -> 8, left
	| OpBoolOr -> 9, left
	| OpArrow -> 10, right
	| OpAssign | OpAssignOp _ -> 11, right

let is_not_assign = function
	| OpAssign | OpAssignOp _ -> false
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
	| ETernary (e1,e2,e3) when is_not_assign op ->
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
	| EConst (Int i) when op = Neg -> EConst (Int (neg i)),punion p1 p2
	| EConst (Float j) when op = Neg -> EConst (Float (neg j)),punion p1 p2
	| _ -> EUnop (op,Prefix,e), punion p1 p2

let rec make_meta name params ((v,p2) as e) p1 =
	match v with
	| EBinop ((OpAssign | OpAssignOp _),_,_) -> EMeta((name,params,p1),e),punion p1 p2
	| EBinop (bop,e,e2) -> EBinop (bop, make_meta name params e p1 , e2) , (punion p1 p2)
	| ETernary (e1,e2,e3) -> ETernary (make_meta name params e1 p1 , e2, e3), punion p1 p2
	| _ -> EMeta((name,params,p1),e),punion p1 p2

let handle_xml_literal p1 =
	Lexer.reset();
	let i = Lexer.lex_xml p1.pmin !code_ref in
	let xml = Lexer.contents() in
	let e = EConst (String(xml,SDoubleQuotes)),{p1 with pmax = i} in (* STRINGTODO: distinct kind? *)
	let e = make_meta Meta.Markup [] e p1 in
	e

let punion_next p1 s =
	let _,p2 = next_token s in
	{
		pfile = p1.pfile;
		pmin = p1.pmin;
		pmax = p2.pmax - 1;
	}

let mk_null_expr p = (EConst(Ident "null"),p)

let mk_display_expr e dk = (EDisplay(e,dk),(pos e))

let is_completion () =
	!display_mode = DMDefault

let is_signature_display () =
	!display_mode = DMSignature

let check_resume p fyes fno =
	if is_completion () && !in_display_file && p.pmax = (display_position#get).pmin then begin
		had_resume := true;
		fyes()
	end else
		fno()

let check_resume_range p s fyes fno =
	if is_completion () && !in_display_file then begin
		let pnext = next_pos s in
		if p.pmin < (display_position#get).pmin && pnext.pmin >= (display_position#get).pmax then
			fyes pnext
		else
			fno()
	end else
		fno()

let check_completion p0 plus_one s =
	match Stream.peek s with
	| Some((Const(Ident name),p)) when display_position#enclosed_in p ->
		Stream.junk s;
		(Some(Some name,p))
	| _ ->
		if would_skip_display_position p0 plus_one s then
			Some(None,DisplayPosition.display_position#with_pos p0)
		else
			None

let check_type_decl_flag_completion mode flags s =
	if not !in_display_file || not (is_completion()) then raise Stream.Failure;
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
			| (_,p) :: _ when would_skip_display_position p true s ->
				let flags = List.map fst flags in
				syntax_completion (SCAfterTypeFlag flags) None (DisplayPosition.display_position#with_pos p)
			| _ ->
				raise Stream.Failure

let check_type_decl_completion mode pmax s =
	if !in_display_file && is_completion() then begin
		let pmin = match Stream.peek s with
			| Some (Eof,_) | None -> max_int
			| Some tk -> (pos tk).pmin
		in
		let p = display_position#get in
		(* print_endline (Printf.sprintf "(%i <= %i) (%i >= %i)" pmax p.pmin pmin p.pmax); *)
		if pmax <= p.pmin && pmin >= p.pmax then begin
			let so,p = match Stream.peek s with
			| Some((Const(Ident name),p)) when display_position#enclosed_in p -> (Some name),p
			| Some(e,p) -> print_endline (s_token e); None,p
			| _ -> None,p
			in
			delay_syntax_completion (SCTypeDecl mode) so p
		end
	end

let check_signature_mark e p1 p2 =
	if not (is_signature_display()) then e
	else begin
		let p = punion p1 p2 in
		if true || not !was_auto_triggered then begin (* TODO: #6383 *)
			if encloses_position_gt display_position#get p then (mk_display_expr e DKMarked)
			else e
		end else begin
			if (display_position#get).pmin = p1.pmax then (mk_display_expr e DKMarked)
			else e
		end
	end

let convert_abstract_flags flags =
	ExtList.List.filter_map decl_flag_to_abstract_flag flags