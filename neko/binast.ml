(*
 *  Neko Binary AST for OCaml
 *  Copyright (c)2005-2007 Nicolas Cannasse
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
open Nast

type context = {
	ch : unit IO.output;
	mutable curfile : string;
	mutable curline : int;
	mutable scount : int;
	strings : (string,int) Hashtbl.t;
}

let b ctx n =
	IO.write_byte ctx.ch n

let write_ui24 ctx n =
	IO.write_byte ctx.ch n;
	IO.write_byte ctx.ch (n lsr 8);
	IO.write_byte ctx.ch (n lsr 16)

let write_string ctx s =
	try
		let x = ctx.scount - Hashtbl.find ctx.strings s in
		if x > 0xFF then raise Not_found;
		b ctx x;
	with
		Not_found ->
			Hashtbl.replace ctx.strings s ctx.scount;
			ctx.scount <- ctx.scount + 1;
			b ctx 0;
			IO.write_ui16 ctx.ch (String.length s);
			IO.nwrite ctx.ch s

let write_constant ctx = function
	| True -> b ctx 0
	| False -> b ctx 1
	| Null -> b ctx 2
	| This -> b ctx 3
	| Int n ->
		if n >= 0 && n <= 0xFF then begin
			b ctx 4;
			b ctx n;
		end else begin
			b ctx 5;
			IO.write_i32 ctx.ch n;
		end
	| Float s ->
		b ctx 6;
		write_string ctx s
	| String s ->
		b ctx 7;
		write_string ctx s
	| Builtin s ->
		b ctx 8;
		write_string ctx s
	| Ident s ->
		b ctx 9;
		write_string ctx s
	| Int32 n ->
		b ctx 5; (* same as Int *)
		IO.write_real_i32 ctx.ch n

let write_op ctx op =
	b ctx (match op with
	| "+" -> 0
	| "-" -> 1
	| "/" -> 2
	| "*" -> 3
	| "%" -> 4
	| "<<" -> 5
	| ">>" -> 6
	| ">>>" -> 7
	| "|" -> 8
	| "&" -> 9
	| "^" -> 10
	| "==" -> 11
	| "!=" -> 12
	| ">" -> 13
	| ">=" -> 14
	| "<" -> 15
	| "<=" -> 16
	| "=" -> 17
	| "&&" -> 18
	| "||" -> 19
	| "++=" -> 20
	| "--=" -> 21
	| "+=" -> 22
	| "-=" -> 23
	| "/=" -> 24
	| "*=" -> 25
	| "%=" -> 26
	| "<<=" -> 27
	| ">>=" -> 28
	| ">>>=" -> 29
	| "|=" -> 30
	| "&=" -> 31
	| "^=" -> 32
	| op -> failwith ("Invalid neko ast op " ^ op))

let rec write_expr_opt ctx = function
	| None ->
		b ctx 0;
	| Some e ->
		b ctx 1;
		write_expr ctx e

and write_expr ctx (e,p) =
	if p.psource <> ctx.curfile then begin
		b ctx 0;
		write_string ctx p.psource;
		write_ui24 ctx p.pline;
		ctx.curfile <- p.psource;
		ctx.curline <- p.pline;
	end else if p.pline <> ctx.curline then begin
		b ctx 1;
		write_ui24 ctx p.pline;
		ctx.curline <- p.pline;
	end;
	match e with
	| EConst c ->
		b ctx 2;
		write_constant ctx c
	| EBlock el ->
		let n = List.length el in
		if n <= 0xFF then begin
			b ctx 3;
			b ctx n;
		end else begin
			b ctx 4;
			write_ui24 ctx n;
		end;
		List.iter (write_expr ctx) el
	| EParenthesis e ->
		b ctx 5;
		write_expr ctx e;
	| EField (e,f) ->
		b ctx 6;
		write_expr ctx e;
		write_string ctx f;
	| ECall (e,el) ->
		let n = List.length el in
		if n <= 0xFF then begin
			b ctx 7;
			write_expr ctx e;
			b ctx n;
		end else begin
			b ctx 28;
			write_expr ctx e;
			write_ui24 ctx n;
		end;
		List.iter (write_expr ctx) el;
	| EArray (e1,e2) ->
		b ctx 8;
		write_expr ctx e1;
		write_expr ctx e2;
	| EVars vl ->
		b ctx 9;
		b ctx (List.length vl);
		List.iter (fun (v,e) ->
			write_string ctx v;
			write_expr_opt ctx e;
		) vl;
	| EWhile (e1,e2,NormalWhile) ->
		b ctx 10;
		write_expr ctx e1;
		write_expr ctx e2;
	| EWhile (e1,e2,DoWhile) ->
		b ctx 11;
		write_expr ctx e1;
		write_expr ctx e2;
	| EIf (e1,e2,eo) ->
		b ctx 12;
		write_expr ctx e1;
		write_expr ctx e2;
		write_expr_opt ctx eo;
	| ETry (e1,v,e2) ->
		b ctx 13;
		write_expr ctx e1;
		write_string ctx v;
		write_expr ctx e2;
	| EFunction (pl,e) ->
		b ctx 14;
		b ctx (List.length pl);
		List.iter (write_string ctx) pl;
		write_expr ctx e;
	| EBinop (op,e1,e2) ->
		b ctx 15;
		write_op ctx op;
		write_expr ctx e1;
		write_expr ctx e2;
	| EReturn None ->
		b ctx 16;
	| EReturn (Some e) ->
		b ctx 17;
		write_expr ctx e;
	| EBreak None ->
		b ctx 18;
	| EBreak (Some e) ->
		b ctx 19;
		write_expr ctx e;
	| EContinue ->
		b ctx 20;
	| ENext (e1,e2) ->
		b ctx 21;
		write_expr ctx e1;
		write_expr ctx e2;
	| EObject fl ->
		let n = List.length fl in
		if n <= 0xFF then begin
			b ctx 22;
			b ctx n;
		end else begin
			b ctx 23;
			write_ui24 ctx n;
		end;
		List.iter (fun (f,e) ->
			write_string ctx f;
			write_expr ctx e;
		) fl;
	| ELabel l ->
		b ctx 24;
		write_string ctx l;
	| ESwitch (e,cases,eo) ->
		let n = List.length cases in
		if n <= 0xFF then begin
			b ctx 25;
			b ctx n;
		end else begin
			b ctx 26;
			write_ui24 ctx n;
		end;
		write_expr ctx e;
		List.iter (fun (e1,e2) ->
			write_expr ctx e1;
			write_expr ctx e2;
		) cases;
		write_expr_opt ctx eo;
	| ENeko s ->
		b ctx 27;
		write_ui24 ctx (String.length s);
		IO.nwrite ctx.ch s

let write ch e =
	let ctx = {
		ch = ch;
		curfile = "";
		curline = -1;
		scount = 0;
		strings = Hashtbl.create 0;
	} in
	IO.nwrite ctx.ch "NBA\001";
	write_expr ctx e

