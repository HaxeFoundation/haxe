(*
 *  Neko AST for OCaml
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
	psource : string;
	pline : int;
}

type constant =
	| True
	| False
	| Null
	| This
	| Int of int
	| Float of string
	| String of string
	| Builtin of string
	| Ident of string
	| Int32 of int32

type while_flag =
	| NormalWhile
	| DoWhile

type expr_decl =
	| EConst of constant
	| EBlock of expr list
	| EParenthesis of expr
	| EField of expr * string
	| ECall of expr * expr list
	| EArray of expr * expr	
	| EVars of (string * expr option) list
	| EWhile of expr * expr * while_flag
	| EIf of expr * expr * expr option
	| ETry of expr * string * expr
	| EFunction of string list * expr
	| EBinop of string * expr * expr
	| EReturn of expr option
	| EBreak of expr option
	| EContinue
	| ENext of expr * expr
	| EObject of (string * expr) list
	| ELabel of string
	| ESwitch of expr * (expr * expr) list * expr option
	| ENeko of string

and expr = expr_decl * pos

let pos = snd

let null_pos = { pline = 0; psource = "<null pos>" }

let mk_call v args p = ECall (v,args) , p
let mk_call0 v p = ECall (v,[]) , p
let mk_call1 v a p = ECall (v,[a]) , p
let mk_ident i p = EConst (Ident i) , p
let mk_builtin b p = EConst (Builtin b) , p
let mk_int i p = EConst (Int i) , p
let mk_string s p = EConst (String s) , p
let mk_binop op e1 e2 p = EBinop (op,e1,e2) , p

let map f (e,p) =
	(match e with
	| EBlock el -> EBlock (List.map f el)
	| EParenthesis e -> EParenthesis (f e)
	| EField (e,s) -> EField (f e, s)
	| ECall (e,el) -> ECall (f e, List.map f el)
	| EArray (e1,e2) -> EArray (f e1, f e2)
	| EVars vl -> EVars (List.map (fun (v,e) -> v , match e with None -> None | Some e -> Some (f e)) vl)
	| EWhile (e1,e2,flag) -> EWhile (f e1, f e2, flag)
	| EIf (e,e1,e2) -> EIf (f e, f e1, match e2 with None -> None | Some e -> Some (f e))
	| ETry (e,ident,e2) -> ETry (f e, ident, f e2)
	| EFunction (params,e) -> EFunction (params, f e)
	| EBinop (op,e1,e2) -> EBinop (op, f e1, f e2)
	| EReturn (Some e) -> EReturn (Some (f e))
	| EBreak (Some e) -> EBreak (Some (f e))
	| ENext (e1,e2) -> ENext (f e1,f e2)
	| EObject fl -> EObject (List.map (fun (s,e) -> s , f e) fl)
	| ESwitch (e,cases,def) -> ESwitch (f e,List.map (fun(e1,e2) -> f e1, f e2) cases,match def with None -> None | Some e -> Some (f e))
	| EReturn None
	| EBreak None
	| EContinue
	| ENeko _
	| ELabel _
	| EConst _ as x -> x) , p

let iter f (e,p) =
	match e with
	| EBlock el -> List.iter f el
	| EParenthesis e -> f e
	| EField (e,s) -> f e
	| ECall (e,el) -> f e; List.iter f el
	| EArray (e1,e2) -> f e1; f e2
	| EVars vl -> List.iter (fun (_,e) -> match e with None -> () | Some e -> f e) vl
	| EWhile (e1,e2,_) -> f e1; f e2
	| EIf (e,e1,e2) -> f e; f e1; (match e2 with None -> () | Some e -> f e)
	| ETry (e1,_,e2) -> f e1; f e2
	| EFunction (_,e) -> f e
	| EBinop (_,e1,e2) -> f e1; f e2
	| EReturn (Some e) -> f e
	| EBreak (Some e) -> f e
	| ENext (e1,e2) -> f e1; f e2
	| EObject fl -> List.iter (fun (_,e) -> f e) fl
	| ESwitch (e,cases,def) -> f e; List.iter (fun(e1,e2) -> f e1; f e2) cases; (match def with None -> () | Some e -> f e) 
	| EReturn None
	| EBreak None
	| EContinue
	| ENeko _
	| ELabel _
	| EConst _ -> ()

let is_printable c = c >= '\032' && c <= '\126'

let escape s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '\\' -> Buffer.add_string b "\\\\"
		| c when c == '"' || not (is_printable c) -> Buffer.add_string b (Printf.sprintf "\\%.3d" (int_of_char c))
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let s_constant = function
	| True -> "true"
	| False -> "false"
	| Null -> "null"
	| This -> "this"
	| Int i -> string_of_int i
	| Float s -> s
	| String s -> "\"" ^ escape s ^ "\""
	| Builtin s -> "$" ^ s
	| Ident s -> s
	| Int32 i -> Int32.to_string i

