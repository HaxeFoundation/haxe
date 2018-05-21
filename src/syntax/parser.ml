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

open Ast
open Globals
open Reification
open DisplayTypes.DisplayMode

type error_msg =
	| Unexpected of token
	| Duplicate_default
	| Missing_semicolon
	| Unclosed_macro
	| Unimplemented
	| Missing_type
	| Custom of string

exception Error of error_msg * pos
exception TypePath of string list * (string * bool) option * bool (* in import *)
exception Display of expr

let error_msg = function
	| Unexpected t -> "Unexpected "^(s_token t)
	| Duplicate_default -> "Duplicate default"
	| Missing_semicolon -> "Missing ;"
	| Unclosed_macro -> "Unclosed macro"
	| Unimplemented -> "Not implemented for current platform"
	| Missing_type -> "Missing type declaration"
	| Custom s -> s

let error m p = raise (Error (m,p))
let display_error : (error_msg -> pos -> unit) ref = ref (fun _ _ -> assert false)

let special_identifier_files : (string,string) Hashtbl.t = Hashtbl.create 0

type decl_flag =
	| DPrivate
	| DExtern

let decl_flag_to_class_flag = function
	| DPrivate -> HPrivate
	| DExtern -> HExtern

let decl_flag_to_enum_flag = function
	| DPrivate -> EPrivate
	| DExtern -> EExtern

let decl_flag_to_abstract_flag = function
	| DPrivate -> AbPrivate
	| DExtern -> AbExtern

module TokenCache = struct
	let cache = ref (DynArray.create ())
	let add (token : (token * pos)) = DynArray.add (!cache) token
	let get index = DynArray.get (!cache) index
	let clear () =
		let old_cache = !cache in
		cache := DynArray.create ();
		(fun () -> cache := old_cache)
end

let last_doc : (string * int) option ref = ref None
let use_doc = ref false
let was_auto_triggered = ref false
let display_mode = ref DMNone
let resume_display = ref null_pos
let in_macro = ref false
let had_resume = ref false

let reset_state () =
	last_doc := None;
	use_doc := false;
	was_auto_triggered := false;
	display_mode := DMNone;
	resume_display := null_pos;
	in_macro := false;
	had_resume := false

let last_token s =
	let n = Stream.count s in
	TokenCache.get (if n = 0 then 0 else n - 1)

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

let serror() = raise (Stream.Error "")

let do_resume() = !resume_display <> null_pos

let display e = raise (Display e)

let type_path sl in_import = match sl with
	| n :: l when n.[0] >= 'A' && n.[0] <= 'Z' -> raise (TypePath (List.rev l,Some (n,false),in_import));
	| _ -> raise (TypePath (List.rev sl,None,in_import))

let is_resuming_file file =
	Path.unique_full_path file = !resume_display.pfile

let is_resuming p =
	let p2 = !resume_display in
	p.pmax = p2.pmin && is_resuming_file p.pfile

let set_resume p =
	resume_display := { p with pfile = Path.unique_full_path p.pfile }

let encloses_resume p =
	p.pmin < !resume_display.pmin && p.pmax >= !resume_display.pmax

let would_skip_resume p1 s =
	match Stream.npeek 1 s with
	| [ (_,p2) ] ->
		is_resuming_file p2.pfile && encloses_resume (punion p1 p2)
	| _ ->
		false

let is_dollar_ident e = match fst e with
	| EConst (Ident n) when n.[0] = '$' ->
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
	| EConst (Int i) when op = Neg -> EConst (Int (neg i)),punion p1 p2
	| EConst (Float j) when op = Neg -> EConst (Float (neg j)),punion p1 p2
	| _ -> EUnop (op,Prefix,e), punion p1 p2

let rec make_meta name params ((v,p2) as e) p1 =
	match v with
	| EBinop ((OpAssign | OpAssignOp _),_,_) -> EMeta((name,params,p1),e),punion p1 p2
	| EBinop (bop,e,e2) -> EBinop (bop, make_meta name params e p1 , e2) , (punion p1 p2)
	| ETernary (e1,e2,e3) -> ETernary (make_meta name params e1 p1 , e2, e3), punion p1 p2
	| _ -> EMeta((name,params,p1),e),punion p1 p2

let make_is e (t,p_t) p p_is =
	let e_is = EField((EConst(Ident "Std"),null_pos),"is"),p_is in
	let e2 = expr_of_type_path (t.tpackage,t.tname) p_t in
	ECall(e_is,[e;e2]),p

let next_token s = match Stream.peek s with
	| Some tk -> tk
	| _ -> last_token s

let next_pos s = pos (next_token s)

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
	if is_completion () && is_resuming p then (had_resume := true; fyes()) else fno()