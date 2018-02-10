(*
 * Copyright (C)2005-2018 Haxe Foundation
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

open Globals
open Ast
open Type
open Common

type ctx = {
	com : Common.context;
	mutable ch : out_channel;
	mutable buf : Rbuffer.t;
	mutable tabs : string;
	mutable separator : bool;
	dirs : (string list, bool) Hashtbl.t;
	mutable vars : (int, bool) Hashtbl.t;
	mutable current_module : path;
}

type ml_type =
	| MUnit
	| MInt
	| MFloat
	| MBool
	| MString
	| MOption of ml_type
	| MFun of ml_type list
	| MInst of path
	| MParams of ml_type * ml_type list

let sprintf = Printf.sprintf

let ident i = i

let is_extern_field f =
	not (Type.is_physical_field f) || Meta.has Meta.Extern f.cf_meta

let flush ctx =
	Rbuffer.output_buffer ctx.ch ctx.buf;
	Rbuffer.clear ctx.buf

let spr ctx s =
	ctx.separator <- false;
	Rbuffer.add_string ctx.buf s

let print ctx =
	ctx.separator <- false;
	Printf.kprintf (fun s -> Rbuffer.add_string ctx.buf s)

let newline ctx =
	match Rbuffer.nth ctx.buf (Rbuffer.length ctx.buf - 1) with
	| '}' | '{' | ':' | ';' -> print ctx "\n%s" ctx.tabs
	| _ when ctx.separator -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx ";\n%s" ctx.tabs

let begin_module ctx (path,name) =
	if not (Hashtbl.mem ctx.dirs path) then begin
		Path.mkdir_recursive ctx.com.file path;
		Hashtbl.add ctx.dirs path true;
	end;
	let file = ctx.com.file ^ (match path with [] -> "" | _ -> "/" ^ String.concat "/" path) ^ "/" ^ name ^ ".ml" in
	ctx.ch <- open_out_bin file;
	ctx.current_module <- (path,name)

let flush ctx =
	Rbuffer.output_buffer ctx.ch ctx.buf;
	Rbuffer.clear ctx.buf

let end_module ctx =
	flush ctx;
	close_out ctx.ch;
	ctx.ch <- stdout

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let rec to_type ctx t p =
	match t with
	| TMono r ->
		(match !r with
		| None -> abort "Unbound monomorph" p
		| Some t -> to_type ctx t p)
	| TLazy f ->
		to_type ctx (lazy_type f) p
	| TAbstract ({a_path = [],"Null"},[t1]) ->
		MOption (to_type ctx t1 p)
	| TFun (args, ret) ->
		MFun (List.map (fun (_,o,t) -> to_type ctx t p) args @ [to_type ctx ret p])
	| TInst ({ cl_path = [],"String" },_) ->
		MString
	| TInst (c,[]) ->
		MInst c.cl_path
	| TInst (c,pl) ->
		MParams (MInst c.cl_path, List.map (fun t -> to_type ctx t p) pl)
	| TEnum (e,[]) ->
		MInst e.e_path
	| TEnum (e,pl) ->
		MParams (MInst e.e_path, List.map (fun t -> to_type ctx t p) pl)
	| TAbstract (a,pl) ->
		if Meta.has Meta.CoreType a.a_meta then
			(match a.a_path with
			| [], "Void" -> MUnit
			| [], "Int" | [], "UInt" -> MInt
			| [], "Float" -> MFloat
			| [], "Bool" -> MBool
			| _ -> abort ("Unknown core type " ^ s_type_path a.a_path) p)
		else
			to_type ctx (Abstract.get_underlying_type a pl) p
	| _ ->
		abort ("Unsupported type " ^ s_type (print_context()) t) p


let module_path ctx path =
	if path = ctx.current_module then
		""
	else
		snd path ^ "."

let rec type_str ctx = function
	| MUnit -> "unit"
	| MInt -> "int"
	| MFloat -> "float"
	| MBool -> "bool"
	| MString -> "string"
	| MOption t -> type_str ctx t ^ " option"
	| MFun tl -> String.concat " -> " (List.map (type_str ctx) tl)
	| MInst path -> module_path ctx path ^ "t"
	| MParams (t,[]) -> type_str ctx t
	| MParams (t,[p]) -> type_str ctx p ^ " " ^ type_str ctx p
	| MParams (t,pl) -> "(" ^ String.concat ", " (List.map (type_str ctx) pl) ^ ") " ^ type_str ctx t

let rec def_value ctx = function
	| MUnit -> "()"
	| MInt -> "0"
	| MFloat -> "0."
	| MBool -> "false"
	| MString -> "\"\""
	| MOption t -> "None"
	| MFun tl -> "(fun " ^ String.concat " " (List.map (fun _ -> "_") tl) ^ " -> assert false)"
	| MParams (t,_) -> def_value ctx t
	| MInst path -> "Obj.magic 0"

let s_type ctx t p = type_str ctx (to_type ctx t p)

let scan_vars ctx e =
	let old = ctx.vars in
	ctx.vars <- Hashtbl.create 0;
	let rec loop e =
		(match e.eexpr with
		| TBinop ((OpAssign | OpAssignOp _), { eexpr = TLocal v },_) -> Hashtbl.replace ctx.vars v.v_id true
		| _ -> ());
		Type.iter loop e
	in
	loop e;
	(fun() -> ctx.vars <- old)

let gen_list ctx sep f list =
	let first = ref true in
	List.iter (fun e ->
		if !first then first := false else spr ctx sep;
		f ctx e
	) list

let rec gen_expr ctx e is_final =
	match e.eexpr with
	| TConst c ->
		(match c with
		| TInt i -> print ctx "%ld" i
		| TFloat s -> print ctx "%s." s
		| TString s -> print ctx "\"%s\"" (String.escaped s)
		| TBool b -> spr ctx (if b then "true" else "false")
		| TNull -> spr ctx "None"
		| TThis -> spr ctx "this"
		| TSuper -> assert false)
	| TLocal v ->
		let vid = ident v.v_name in
		if Hashtbl.mem ctx.vars v.v_id then print ctx "!%s" vid else spr ctx vid
	| TVar (v,init) ->
		let mut = Hashtbl.mem ctx.vars v.v_id in
		let t = to_type ctx v.v_type e.epos in
		print ctx "let %s : %s = %s" (ident v.v_name) (type_str ctx t) (if mut then "ref " else "");
		(match init with None -> spr ctx (def_value ctx t) | Some e -> gen_expr ctx e false);
		spr ctx " in ";
		ctx.separator <- true;
	| TBlock [] ->
		spr ctx "()"
	| TBlock el ->
		spr ctx "begin";
		ctx.separator <- true;
		let b = open_block ctx in
		let rec loop = function
			| [] -> assert false
			| [e] ->
				newline ctx;
				gen_expr ctx e is_final
			| e :: el ->
				newline ctx;
				(match follow e.etype with TAbstract ({ a_path = [],"Void" },_) -> () | _ -> spr ctx "ignore ");
				gen_expr ctx e false;
				loop el
		in
		loop el;
		b();
		newline ctx;
		spr ctx "end";
	| TCall ({ eexpr = TField (_,FEnum _) } as e, (_ :: _ :: _ as pl)) ->
		gen_expr ctx e false;
		spr ctx "(";
		gen_list ctx ", " (fun ctx e -> gen_expr ctx e false) pl;
		spr ctx ")"
	| TCall (e, pl) ->
		gen_expr ctx e false;
		if pl = [] then spr ctx "()";
		List.iter (fun e ->
			spr ctx " ";
			gen_expr ctx e false;
		) pl;
	| TField (e, ft) ->
		(match ft with
		| FInstance _ -> assert false
		| FEnum (e,ef) ->
			print ctx "%s%s" (module_path ctx e.e_path) (ident ef.ef_name)
		| FStatic (c,cf) ->
			let rec loop = function
				| (Meta.Custom ":mlNative",[EConst (String s),_],_) :: _ ->
					spr ctx s
				| _ :: l -> loop l
				| [] ->
					print ctx "%s%s" (module_path ctx c.cl_path) (ident cf.cf_name)
			in
			loop cf.cf_meta
		| FAnon f -> assert false
		| FDynamic _ -> assert false
		| FClosure _ -> assert false);
	| TReturn _ when not is_final ->
		abort "Unallowed not final return" e.epos
	| TReturn None ->
		spr ctx "()"
	| TReturn (Some e) ->
		gen_expr ctx e is_final
	| _ ->
		abort ("Unsupported expr " ^ s_expr_kind e) e.epos

let generate_class ctx c =
	if c.cl_super <> None then abort "Inheritance not yet supported" c.cl_pos;
	let fields = List.fold_left (fun acc f ->
		if is_extern_field f then
			acc
		else match f.cf_kind with
		| Var _ -> f :: acc
		| _ -> acc
	) [] c.cl_ordered_fields in
	if fields <> [] then begin
		print ctx "type t = {";
		let b = open_block ctx in
		List.iter (fun f ->
			newline ctx;
			print ctx "mutable %s : %s" (ident f.cf_name) (s_type ctx f.cf_type f.cf_pos);
		) (List.rev fields);
		b();
		newline ctx;
		print ctx "}";
		newline ctx;
		spr ctx "\n\n"
	end;
	List.iter (fun f ->
		match f.cf_kind with
		| Var v -> assert false
		| Method _ ->
			let args, ret = (match follow f.cf_type with TFun (args, ret) -> args, ret | _ -> assert false) in
			let tret = to_type ctx ret f.cf_pos in
			print ctx "let %s %s : %s = " (ident f.cf_name) (if args = [] then "()" else String.concat " " (List.map (fun (n,o,t) ->
				if o then abort "Unsupported optional arg" f.cf_pos;
				sprintf "(%s:%s)" (ident n) (s_type ctx t f.cf_pos)
			) args)) (type_str ctx tret);
			(match f.cf_expr with
			| Some { eexpr = TFunction f } ->
				let e = f.tf_expr in
				let old = scan_vars ctx e in
				if tret = MUnit && to_type ctx e.etype e.epos <> MUnit then spr ctx "ignore ";
				gen_expr ctx e true;
				old()
			| _ -> assert false);
			spr ctx "\n"
	) c.cl_ordered_statics

let generate_enum ctx e =
	print ctx "type t =";
	List.iter (fun n ->
		let c = PMap.find n e.e_constrs in
		print ctx "\n\t| %s" (ident n);
		(match follow c.ef_type with
		| TFun (args,_) -> print ctx " of %s" (String.concat " * " (List.map (fun (n,o,t) -> s_type ctx t c.ef_pos) args))
		| _ -> ())
	) e.e_names;
	spr ctx "\n\n"

let generate_type ctx t =
	match t with
	| TClassDecl { cl_extern = true } ->
		()
	| TClassDecl c ->
		begin_module ctx c.cl_path;
		generate_class ctx c;
		end_module ctx;
	| TAbstractDecl { a_impl = None } ->
		() (* core type *)
	| TTypeDecl td ->
		begin_module ctx td.t_path;
		spr ctx "TODO";
		end_module ctx;
	| TEnumDecl e ->
		begin_module ctx e.e_path;
		generate_enum ctx e;
		end_module ctx;
	| _ ->
		abort "Unsupported module type"  (t_infos t).mt_pos

let generate com =
	let ctx = {
		com = com;
		ch = stdout;
		tabs = "";
		separator = false;
		dirs = Hashtbl.create 0;
		buf = Rbuffer.create 65536;
		vars = Hashtbl.create 0;
		current_module = [],"";
	} in
	(try Unix.mkdir ctx.com.file 0o755 with _ -> ());
	List.iter (generate_type ctx) com.types;
	(match com.main with
	| None -> ()
	| Some e ->
		begin_module ctx ([],"MLBoot");
		gen_expr ctx e true;
		newline ctx;
		end_module ctx)
