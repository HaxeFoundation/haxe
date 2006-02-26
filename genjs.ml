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
open Type

type ctx = {
	buf : Buffer.t;
	packages : (string list,unit) Hashtbl.t;
	mutable statics : (tclass * string * texpr) list;
	mutable tabs : string;
}

let s_path = function
	| ([],"@Main") -> "$Main"
	| p -> Ast.s_type_path p

let kwds = 
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) ["instanceof";"int"];
	h

let field s = if Hashtbl.mem kwds s then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "$" ^ s else s

let spr ctx s = Buffer.add_string ctx.buf s
let print ctx = Printf.ksprintf (fun s -> Buffer.add_string ctx.buf s)

let newline ctx = 
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx ";\n%s" ctx.tabs

let rec concat ctx s f = function
	| [] -> ()
	| [x] -> f x
	| x :: l ->
		f x;
		spr ctx s;
		concat ctx s f l

let parent e = 
	match e.eexpr with
	| TParenthesis _ -> e
	| _ -> mk (TParenthesis e) e.etype e.epos

let gen_constant ctx = function
	| TInt s
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" s
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx "this"
	| TSuper -> assert false

let rec gen_expr ctx e =
	match e.eexpr with
	| TConst c -> gen_constant ctx c
	| TLocal s -> spr ctx (ident s)
	| TMember s -> print ctx "this%s" (field s)
	| TEnumField (e,s) -> print ctx "%s%s" (s_path e.e_path) (field s)
	| TArray (e1,e2) -> 
		gen_expr ctx e1;
		spr ctx "[";
		gen_expr ctx e2;
		spr ctx "]";
	| TBinop (op,e1,e2) ->
		gen_expr ctx e1;
		print ctx " %s " (Ast.s_binop op);
		gen_expr ctx e2;
	| TField (e,s) ->
		gen_expr ctx e;
		spr ctx (field s)
	| TType t ->
		spr ctx (s_path (t_path t))
	| TParenthesis e ->
		spr ctx "(";
		gen_expr ctx e;
		spr ctx ")";
	| TReturn eo ->
		(match eo with
		| None ->
			spr ctx "return"
		| Some e ->
			spr ctx "return ";
			gen_expr ctx e);
	| TBreak ->
		spr ctx "break"
	| TContinue ->
		spr ctx "continue"
	| TBlock el ->
		let oldt = ctx.tabs in
		print ctx "{";
		ctx.tabs <- "\t" ^ ctx.tabs;		
		List.iter (fun e -> newline ctx; gen_expr ctx e) el;
		ctx.tabs <- oldt;
		newline ctx;
		print ctx "}";
	| TFunction f ->
		print ctx "function(%s)" (String.concat "," (List.map ident (List.map fst f.tf_args)));
		gen_expr ctx f.tf_expr;
	| TCall (e,el) ->
		gen_expr ctx e;
		spr ctx "(";
		concat ctx "," (gen_expr ctx) el;
		spr ctx ")"
	| TArrayDecl el ->
		spr ctx "[";
		concat ctx "," (gen_expr ctx) el;
		spr ctx "]"
	| TThrow e ->
		spr ctx "throw ";
		gen_expr ctx e;
	| TVars [] ->
		()
	| TVars vl ->
		spr ctx "var ";
		concat ctx ", " (fun (n,_,e) -> 
			spr ctx (ident n);
			match e with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				gen_expr ctx e
		) vl;
	| TNew (c,_,el) ->
		print ctx "new %s(" (s_path c.cl_path);
		concat ctx "," (gen_expr ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_expr ctx (parent cond);
		spr ctx " ";
		gen_expr ctx e;
		(match eelse with
		| None -> ()
		| Some e -> 
			spr ctx "; else ";
			gen_expr ctx e);
	| TUnop (op,Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_expr ctx e
	| TUnop (op,Ast.Postfix,e) ->
		gen_expr ctx e;
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		gen_expr ctx (parent cond);
		gen_expr ctx e;
	| TWhile (cond,e,Ast.DoWhile) ->
		spr ctx "do ";
		gen_expr ctx e;
		spr ctx " while";
		gen_expr ctx (parent cond);		
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s : " f; gen_expr ctx e) fields;
		spr ctx "}"
	| TFor (v,it,e) ->
		spr ctx "var $it = ";
		gen_expr ctx it;
		newline ctx;
		print ctx "while( $it.hasNext() ) { var %s = $it.next()" (ident v);
		newline ctx;
		gen_expr ctx e;
		newline ctx;
		spr ctx "}"
	| TTry (e,catchs) ->
		spr ctx "try ";
		gen_expr ctx e;
		newline ctx;
		spr ctx "catch( $e ) {";
		newline ctx;
		(* TODO : CATCHES *)
		spr ctx "}";
	| TMatch _ ->
		assert false (* handled in TSwitch *)
	| TSwitch (e,cases,def) ->
		spr ctx "null"

let generate_package_create ctx (p,_) =
	let rec loop acc = function
		| [] -> ()
		| p :: l when Hashtbl.mem ctx.packages (p :: acc) -> loop (p :: acc) l
		| p :: l ->			
			Hashtbl.add ctx.packages (p :: acc) ();
			print ctx "%s%s = {}" (String.concat "." (List.rev acc)) (field p);
			newline ctx;
			loop (p :: acc) l
	in
	loop [] p

let gen_class_static_field ctx c f =
	match f.cf_expr with
	| None -> 
		print ctx "%s%s = null" (s_path c.cl_path) (field f.cf_name);
		newline ctx
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			print ctx "%s%s = " (s_path c.cl_path) (field f.cf_name);
			gen_expr ctx e;
			newline ctx
		| _ ->
			ctx.statics <- (c,f.cf_name,e) :: ctx.statics

let gen_class_field ctx c f =
	print ctx "%s.prototype%s = " (s_path c.cl_path) (field f.cf_name);
	match f.cf_expr with
	| None ->
		print ctx "null";
		newline ctx
	| Some e ->
		gen_expr ctx e;
		newline ctx

let generate_class ctx c = 
	generate_package_create ctx c.cl_path;
	print ctx "%s = " (s_path c.cl_path);
	(match c.cl_constructor with
	| Some { cf_expr = Some e } ->
		gen_expr ctx e;
	| _ ->
		print ctx "function() { }"
	);
	newline ctx;
	List.iter (gen_class_static_field ctx c) c.cl_ordered_statics;
	PMap.iter (fun _ f -> gen_class_field ctx c f) c.cl_fields

let generate_enum ctx e =
	generate_package_create ctx e.e_path;
	print ctx "%s = " (s_path e.e_path);
	print ctx "null";
	newline ctx

let generate_static ctx (c,f,e) =
	print ctx "%s%s = " (s_path c.cl_path) (field f);
	gen_expr ctx e;
	newline ctx

let generate_type ctx = function
	| TClassDecl c -> if not c.cl_extern then generate_class ctx c
	| TEnumDecl e -> generate_enum ctx e

let generate file types =
	let ctx = {
		buf = Buffer.create 16000;
		packages = Hashtbl.create 0;
		statics = [];
		tabs = "";
	} in
	List.iter (generate_type ctx) types;
	List.iter (generate_static ctx) (List.rev ctx.statics);
	let ch = open_out file in
	output_string ch (Buffer.contents ctx.buf);
	close_out ch
