(*
 *  Haxe Compiler
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
open Type
open Common

type context_infos = {
	com : Common.context;
}

type context = {
	inf : context_infos;
	ch : out_channel;
	buf : Buffer.t;
	path : path;
	mutable get_sets : (string * bool,string) Hashtbl.t;
	mutable curclass : tclass;
	mutable tabs : string;
	mutable in_value : string option;
	mutable in_static : bool;
	mutable handle_break : bool;
	mutable imports : (string,string list list) Hashtbl.t;
	mutable locals : (string,string) PMap.t;
	mutable inv_locals : (string,string) PMap.t;
	mutable local_types : t list;
	mutable constructor_block : bool;
}

let protect name =
	match name with
	| "Error" | "Namespace" -> "_" ^ name
	| _ -> name

let s_path ctx stat path p =
	match path with
	| ([],name) ->
		(match name with
		| "Int" -> "int"
		| "Float" -> "Number"
		| "Dynamic" -> "Object"
		| "Bool" -> "Boolean"
		| "Enum" -> "Class"
		| _ -> name)
	| (["flash"],"FlashXml__") ->
		"Xml"
	| (["flash";"errors"],"Error") ->
		"Error"
	| (["flash"],"Vector") ->
		"Vector"
	| (["flash";"xml"],"XML") ->
		"XML"
	| (["flash";"xml"],"XMLList") ->
		"XMLList"
	| ["flash";"utils"],"QName" ->
		"QName"
	| ["flash";"utils"],"Namespace" ->
		"Namespace"
	| (["haxe"],"Int32") when not stat ->
		"int"
	| (pack,name) ->
		let name = protect name in
		let packs = (try Hashtbl.find ctx.imports name with Not_found -> []) in
		if not (List.mem pack packs) then Hashtbl.replace ctx.imports name (pack :: packs);
		Ast.s_type_path (pack,name)

let reserved =
	let h = Hashtbl.create 0 in
	List.iter (fun l -> Hashtbl.add h l ())
	(* these ones are defined in order to prevent recursion in some Std functions *)
	["is";"as";"int";"uint";"const";"getTimer";"typeof";"parseInt";"parseFloat";
	(* AS3 keywords which are not haXe ones *)
	"each";"label";"finally";"with";"final";"internal";"native";"const";"namespace";"include";"delete";
	(* some globals give some errors with Flex SDK as well *)
	"print";
	(* we don't include get+set since they are not 'real' keywords, but they can't be used as method names *)
	];
	h

let s_ident n =
	if Hashtbl.mem reserved n then "_" ^ n else n

let init infos path =
	let rec create acc = function
		| [] -> ()
		| d :: l ->
			let dir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
			create (d :: acc) l
	in
	let dir = infos.com.file :: fst path in
	create [] dir;
	let ch = open_out (String.concat "/" dir ^ "/" ^ snd path ^ ".as") in
	let imports = Hashtbl.create 0 in
	Hashtbl.add imports (snd path) [fst path];
	{
		inf = infos;
		tabs = "";
		ch = ch;
		path = path;
		buf = Buffer.create (1 lsl 14);
		in_value = None;
		in_static = false;
		handle_break = false;
		imports = imports;
		curclass = null_class;
		locals = PMap.empty;
		inv_locals = PMap.empty;
		local_types = [];
		get_sets = Hashtbl.create 0;
		constructor_block = false;
	}

let close ctx =
	output_string ctx.ch (Printf.sprintf "package %s {\n" (String.concat "." (fst ctx.path)));
	Hashtbl.iter (fun name paths ->
		List.iter (fun pack ->
			let path = pack, name in
			if path <> ctx.path then output_string ctx.ch ("\timport " ^ Ast.s_type_path path ^ ";\n");
		) paths
	) ctx.imports;
	output_string ctx.ch (Buffer.contents ctx.buf);
	close_out ctx.ch

let save_locals ctx =
	let old = ctx.locals in
	(fun() -> ctx.locals <- old)

let define_local ctx l =
	let rec loop n =
		let name = (if n = 1 then s_ident l else l ^ string_of_int n) in
		if PMap.mem name ctx.inv_locals then
			loop (n+1)
		else begin
			ctx.locals <- PMap.add l name ctx.locals;
			ctx.inv_locals <- PMap.add name l ctx.inv_locals;
			name
		end
	in
	loop 1

let spr ctx s = Buffer.add_string ctx.buf s
let print ctx = Printf.kprintf (fun s -> Buffer.add_string ctx.buf s)

let unsupported p = error "This expression cannot be generated to AS3" p

let newline ctx =
	let rec loop p =
		match Buffer.nth ctx.buf p with
		| '}' | '{' | ':' -> print ctx "\n%s" ctx.tabs
		| '\n' | '\t' -> loop (p - 1)
		| _ -> print ctx ";\n%s" ctx.tabs
	in
	loop (Buffer.length ctx.buf - 1)

let rec concat ctx s f = function
	| [] -> ()
	| [x] -> f x
	| x :: l ->
		f x;
		spr ctx s;
		concat ctx s f l

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let parent e =
	match e.eexpr with
	| TParenthesis _ -> e
	| _ -> mk (TParenthesis e) e.etype e.epos

let default_value tstr =
	match tstr with
	| "int" | "uint" -> "0"
	| "Number" -> "NaN"
	| "Boolean" -> "false"
	| _ -> "null"

let rec type_str ctx t p =
	match t with
	| TEnum _ | TInst _ when List.memq t ctx.local_types ->
		"*"
	| TEnum (e,_) ->
		if e.e_extern then (match e.e_path with
			| [], "Void" -> "void"
			| [], "Bool" -> "Boolean"
			| _ -> 
				let rec loop = function
					| [] -> "Object"
					| (":fakeEnum",[Ast.EConst (Ast.Type n),_],_) :: _ ->
						(match n with
						| "Int" -> "int"
						| "UInt" -> "uint"
						| _ -> n)
					| _ :: l -> loop l
				in
				loop e.e_meta
		) else
			s_path ctx true e.e_path p
	| TInst ({ cl_path = ["flash"],"Vector" },[pt]) ->
		"Vector.<" ^ type_str ctx pt p ^ ">"
	| TInst (c,_) ->		
		(match c.cl_kind with
		| KNormal | KGeneric | KGenericInstance _ -> s_path ctx false c.cl_path p
		| KTypeParameter | KExtension _ | KConstant _  -> "*")
	| TFun _ ->
		"Function"
	| TMono r ->
		(match !r with None -> "*" | Some t -> type_str ctx t p)
	| TAnon _ | TDynamic _ ->
		"*"
	| TType (t,args) ->
		(match t.t_path with
		| [], "UInt" -> "uint"
		| [] , "Null" ->
			(match args with
			| [t] ->
				(match follow t with
				| TInst ({ cl_path = [],"Int" },_)
				| TInst ({ cl_path = [],"Float" },_)
				| TEnum ({ e_path = [],"Bool" },_) -> "*"
				| _ -> type_str ctx t p)
			| _ -> assert false);
		| _ -> type_str ctx (apply_params t.t_types args t.t_type) p)
	| TLazy f ->
		type_str ctx ((!f)()) p

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ | TMatch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let handle_break ctx e =
	let old_handle = ctx.handle_break in
	try
		iter_switch_break false e;
		ctx.handle_break <- false;
		(fun() -> ctx.handle_break <- old_handle)
	with
		Exit ->
			spr ctx "try {";
			let b = open_block ctx in
			newline ctx;
			ctx.handle_break <- true;
			(fun() ->
				b();
				ctx.handle_break <- old_handle;
				newline ctx;
				spr ctx "} catch( e : * ) { if( e != \"__break__\" ) throw e; }";
			)

let this ctx = if ctx.in_value <> None then "$this" else "this"

let escape_bin s =
	let b = Buffer.create 0 in
	for i = 0 to String.length s - 1 do
		match Char.code (String.unsafe_get s i) with
		| c when c < 32 -> Buffer.add_string b (Printf.sprintf "\\x%.2X" c)
		| c -> Buffer.add_char b (Char.chr c)
	done;
	Buffer.contents b

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" (escape_bin (Ast.s_escape s))
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx (this ctx)
	| TSuper -> spr ctx "super"

let gen_function_header ctx name f params p =
	let old = ctx.in_value in
	let old_l = ctx.locals in
	let old_li = ctx.inv_locals in
	let old_t = ctx.local_types in
	ctx.in_value <- None;
	ctx.local_types <- List.map snd params @ ctx.local_types;
	print ctx "function%s(" (match name with None -> "" | Some (n,meta) ->
		let rec loop = function
			| [] -> n
			| (":getter",[Ast.EConst (Ast.Ident i | Ast.Type i),_],_) :: _ -> "get " ^ i
			| (":setter",[Ast.EConst (Ast.Ident i | Ast.Type i),_],_) :: _ -> "set " ^ i
			| _ :: l -> loop l
		in
		" " ^ loop meta
	);
	concat ctx "," (fun (arg,c,t) ->
		let arg = define_local ctx arg in
		let tstr = type_str ctx t p in
		print ctx "%s : %s" arg tstr;
		match c with
		| None ->
			if ctx.constructor_block then print ctx " = %s" (default_value tstr);
		| Some c ->
			spr ctx " = ";
			gen_constant ctx p c
	) f.tf_args;
	print ctx ") : %s " (type_str ctx f.tf_type p);
	(fun () ->
		ctx.in_value <- old;
		ctx.locals <- old_l;
		ctx.inv_locals <- old_li;
		ctx.local_types <- old_t;
	)

let rec gen_call ctx e el r =
	match e.eexpr , el with
	| TCall (x,_) , el ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TLocal "__is__" , [e1;e2] ->
		gen_value ctx e1;
		spr ctx " is ";
		gen_value ctx e2;
	| TLocal "__as__" , [e1;e2] ->
		gen_value ctx e1;
		spr ctx " as ";
		gen_value ctx e2;
	| TLocal "__int__" , [e] ->
		spr ctx "int(";
		gen_value ctx e;
		spr ctx ")";
	| TLocal "__float__" , [e] ->
		spr ctx "Number(";
		gen_value ctx e;
		spr ctx ")";
	| TLocal "__typeof__", [e] ->
		spr ctx "typeof ";
		gen_value ctx e;
	| TLocal "__keys__", [e] ->
		let ret = (match ctx.in_value with None -> assert false | Some r -> r) in
		print ctx "%s = new Array()" ret;
		newline ctx;
		let b = save_locals ctx in
		let tmp = define_local ctx "$k" in
		print ctx "for(var %s : String in " tmp;
		gen_value ctx e;
		print ctx ") %s.push(%s)" ret tmp;
		b();
	| TLocal "__hkeys__", [e] ->
		let ret = (match ctx.in_value with None -> assert false | Some r -> r) in
		print ctx "%s = new Array()" ret;
		newline ctx;
		let b = save_locals ctx in
		let tmp = define_local ctx "$k" in
		print ctx "for(var %s : String in " tmp;
		gen_value ctx e;
		print ctx ") %s.push(%s.substr(1))" ret tmp;
		b();
	| TLocal "__foreach__", [e] ->
		let ret = (match ctx.in_value with None -> assert false | Some r -> r) in
		print ctx "%s = new Array()" ret;
		newline ctx;
		let b = save_locals ctx in
		let tmp = define_local ctx "$k" in
		print ctx "for each(var %s : * in " tmp;
		gen_value ctx e;
		print ctx ") %s.push(%s)" ret tmp;
		b();
	| TLocal "__new__", e :: args ->
		spr ctx "new ";
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) args;
		spr ctx ")";
	| TLocal "__delete__", [e;f] ->
		spr ctx "delete(";
		gen_value ctx e;
		spr ctx "[";
		gen_value ctx f;
		spr ctx "]";
		spr ctx ")";
	| TLocal "__unprotect__", [e] ->
		gen_value ctx e
	| TLocal "__vector__", [e] ->
		spr ctx (type_str ctx r e.epos);
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")"
	| TField ({ eexpr = TTypeExpr (TClassDecl { cl_path = (["flash"],"Lib") }) },f), args ->
		(match f, args with
		| "as", [e1;e2] ->
			gen_value ctx e1;
			spr ctx " as ";
			gen_value ctx e2
		| _ ->
			gen_value ctx e;
			spr ctx "(";
			concat ctx "," (gen_value ctx) el;
			spr ctx ")")
	| TField ({ eexpr = TTypeExpr (TClassDecl { cl_path = (["flash"],"Vector") }) },f), args ->
		(match f, args with
		| "ofArray", [e] | "convert", [e] ->
			(match follow r with
			| TInst ({ cl_path = (["flash"],"Vector") },[t]) ->
				print ctx "Vector.<%s>(" (type_str ctx t e.epos);
				gen_value ctx e;
				print ctx ")";
			| _ -> assert false)
		| _ -> assert false)
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

and gen_value_op ctx e =
	match e.eexpr with
	| TBinop (op,_,_) when op = Ast.OpAnd || op = Ast.OpOr || op = Ast.OpXor ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| _ ->
		gen_value ctx e

and gen_field_access ctx t s =
	let field c =
		match fst c.cl_path, snd c.cl_path, s with
		| [], "Math", "NaN"
		| [], "Math", "NEGATIVE_INFINITY"
		| [], "Math", "POSITIVE_INFINITY"
		| [], "Math", "isFinite"
		| [], "Math", "isNaN"
		| [], "Date", "now"
		| [], "Date", "fromTime"
		| [], "Date", "fromString"
		| [], "String", "charCodeAt"
		->
			print ctx "[\"%s\"]" s
		| [], "Date", "toString" ->
			print ctx "[\"toStringHX\"]"
		| [], "String", "cca" ->
			print ctx ".charCodeAt"
		| ["flash";"xml"], "XML", "namespace" ->
			print ctx ".namespace"
		| _ ->
			print ctx ".%s" (s_ident s)
	in
	match follow t with
	| TInst (c,_) -> field c
	| TAnon a ->
		(match !(a.a_status) with
		| Statics c -> field c
		| _ -> print ctx ".%s" (s_ident s))
	| _ ->
		print ctx ".%s" (s_ident s)

and gen_expr ctx e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx e.epos c
	| TLocal s ->
		spr ctx (try PMap.find s ctx.locals with Not_found -> error ("Unknown local " ^ s) e.epos)
	| TEnumField (en,s) ->
		print ctx "%s.%s" (s_path ctx true en.e_path e.epos) (s_ident s)
	| TArray ({ eexpr = TLocal "__global__" },{ eexpr = TConst (TString s) }) ->
		let path = Ast.parse_path s in
		spr ctx (s_path ctx false path e.epos)
	| TArray (e1,e2) ->
		gen_value ctx e1;
		spr ctx "[";
		gen_value ctx e2;
		spr ctx "]";
	| TBinop (op,{ eexpr = TField (e1,s) },e2) ->
		gen_value_op ctx e1;
		gen_field_access ctx e1.etype s;
		print ctx " %s " (Ast.s_binop op);
		gen_value_op ctx e2;
	| TBinop (op,e1,e2) ->
		gen_value_op ctx e1;
		print ctx " %s " (Ast.s_binop op);
		gen_value_op ctx e2;
	| TField ({ eexpr = TTypeExpr t },s) when t_path t = ctx.curclass.cl_path && not (PMap.mem s ctx.locals) ->
		print ctx "%s" (s_ident s)
	| TField (e,s) | TClosure (e,s) ->
   		gen_value ctx e;
		gen_field_access ctx e.etype s
	| TTypeExpr t ->
		spr ctx (s_path ctx true (t_path t) e.epos)
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| TReturn eo ->
		if ctx.in_value <> None then unsupported e.epos;
		(match eo with
		| None ->
			spr ctx "return"
		| Some e when (match follow e.etype with TEnum({ e_path = [],"Void" },[]) -> true | _ -> false) ->
			gen_value ctx e;
			newline ctx;
			spr ctx "return"
		| Some e ->
			spr ctx "return ";
			gen_value ctx e);
	| TBreak ->
		if ctx.in_value <> None then unsupported e.epos;
		if ctx.handle_break then spr ctx "throw \"__break__\"" else spr ctx "break"
	| TContinue ->
		if ctx.in_value <> None then unsupported e.epos;
		spr ctx "continue"
	| TBlock [] ->
		spr ctx "null"
	| TBlock el ->
		let b = save_locals ctx in
		print ctx "{";
		let bend = open_block ctx in
		let cb = (if not ctx.constructor_block then
			(fun () -> ())
		else if not (Codegen.constructor_side_effects e) then begin
			ctx.constructor_block <- false;
			(fun () -> ())
		end else begin
			ctx.constructor_block <- false;
			print ctx " if( !%s.skip_constructor ) {" (s_path ctx true (["flash"],"Boot") e.epos);
            (fun() -> print ctx "}")
		end) in
		List.iter (fun e -> newline ctx; gen_expr ctx e) el;
		bend();
		newline ctx;
		cb();
		print ctx "}";
		b();
	| TFunction f ->
		let h = gen_function_header ctx None f [] e.epos in
		let old = ctx.in_static in
		ctx.in_static <- true;
		gen_expr ctx (mk_block f.tf_expr);
		ctx.in_static <- old;
		h();
	| TCall (v,el) ->
		gen_call ctx v el e.etype
	| TArrayDecl el ->
		spr ctx "[";
		concat ctx "," (gen_value ctx) el;
		spr ctx "]"
	| TThrow e ->
		spr ctx "throw ";
		gen_value ctx e;
	| TVars [] ->
		()
	| TVars vl ->
		spr ctx "var ";
		concat ctx ", " (fun (n,t,v) ->
			let n = define_local ctx n in
			print ctx "%s : %s" n (type_str ctx t e.epos);
			match v with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				gen_value ctx e
		) vl;
	| TNew (c,params,el) ->
		(match c.cl_path, params with
		| (["flash"],"Vector"), [pt] -> print ctx "new Vector.<%s>(" (type_str ctx pt e.epos)
		| _ -> print ctx "new %s(" (s_path ctx true c.cl_path e.epos));
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_value ctx (parent cond);
		spr ctx " ";
		gen_expr ctx e;
		(match eelse with
		| None -> ()
		| Some e ->
			newline ctx;
			spr ctx "else ";
			gen_expr ctx e);
	| TUnop (op,Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_value ctx e
	| TUnop (op,Ast.Postfix,e) ->
		gen_value ctx e;
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "while";
		gen_value ctx (parent cond);
		spr ctx " ";
		gen_expr ctx e;
		handle_break();
	| TWhile (cond,e,Ast.DoWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "do ";
		gen_expr ctx e;
		spr ctx " while";
		gen_value ctx (parent cond);
		handle_break();
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s : " (s_ident f); gen_value ctx e) fields;
		spr ctx "}"
	| TFor (v,t,it,e) ->
		let handle_break = handle_break ctx e in
		let b = save_locals ctx in
		let tmp = define_local ctx "$it" in
		print ctx "{ var %s : * = " tmp;
		gen_value ctx it;
		newline ctx;
		let v = define_local ctx v in
		print ctx "while( %s.hasNext() ) { var %s : %s = %s.next()" tmp v (type_str ctx t e.epos) tmp;
		newline ctx;
		gen_expr ctx e;
		newline ctx;
		spr ctx "}}";
		b();
		handle_break();
	| TTry (e,catchs) ->
		spr ctx "try ";
		gen_expr ctx (mk_block e);
		List.iter (fun (v,t,e) ->
			newline ctx;
			let b = save_locals ctx in
			let v = define_local ctx v in
			print ctx "catch( %s : %s )" v (type_str ctx t e.epos);
			gen_expr ctx (mk_block e);
			b();
		) catchs;
	| TMatch (e,_,cases,def) ->
		print ctx "{";
		let bend = open_block ctx in
		newline ctx;
		let b = save_locals ctx in
		let tmp = define_local ctx "$e" in
		print ctx "var %s : enum = " tmp;
		gen_value ctx e;
		newline ctx;
		print ctx "switch( %s.index ) {" tmp;
		newline ctx;
		List.iter (fun (cl,params,e) ->
			List.iter (fun c ->
				print ctx "case %d:" c;
				newline ctx;
			) cl;
			let b = save_locals ctx in
			(match params with
			| None | Some [] -> ()
			| Some l ->
				let n = ref (-1) in
				let l = List.fold_left (fun acc (v,t) -> incr n; match v with None -> acc | Some v -> (v,t,!n) :: acc) [] l in
				match l with
				| [] -> ()
				| l ->
					spr ctx "var ";
					concat ctx ", " (fun (v,t,n) ->
						let v = define_local ctx v in
						print ctx "%s : %s = %s.params[%d]" v (type_str ctx t e.epos) tmp n;
					) l;
					newline ctx);
			gen_expr ctx (mk_block e);
			print ctx "break";
			newline ctx;
			b()
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			gen_expr ctx (mk_block e);
			print ctx "break";
			newline ctx;
		);
		spr ctx "}";
		bend();
		newline ctx;
		spr ctx "}";
		b()
	| TSwitch (e,cases,def) ->
		spr ctx "switch";
		gen_value ctx (parent e);
		spr ctx " {";
		newline ctx;
		List.iter (fun (el,e2) ->
			List.iter (fun e ->
				spr ctx "case ";
				gen_value ctx e;
				spr ctx ":";
			) el;
			gen_expr ctx (mk_block e2);
			print ctx "break";
			newline ctx;
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			gen_expr ctx (mk_block e);
			print ctx "break";
			newline ctx;
		);
		spr ctx "}"
	| TCast (e1,None) ->
		spr ctx "((";
		gen_expr ctx e1;
		print ctx ") as %s)" (type_str ctx e.etype e.epos);
	| TCast (e1,Some t) ->
		gen_expr ctx (Codegen.default_cast ctx.inf.com e1 t e.etype e.epos)

and gen_value ctx e =
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> assert false | Some v -> "$r")) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let value block =
		let old = ctx.in_value in
		let t = type_str ctx e.etype e.epos in
		let locs = save_locals ctx in
		let tmp = define_local ctx "$r" in
		ctx.in_value <- Some tmp;
		if ctx.in_static then
			print ctx "function() : %s " t
		else
			print ctx "(function($this:%s) : %s " (snd ctx.path) t;
		let b = if block then begin
			spr ctx "{";
			let b = open_block ctx in
			newline ctx;
			print ctx "var %s : %s" tmp t;
			newline ctx;
			b
		end else
			(fun() -> ())
		in
		(fun() ->
			if block then begin
				newline ctx;
				print ctx "return %s" tmp;
				b();
				newline ctx;
				spr ctx "}";
			end;
			ctx.in_value <- old;
			locs();
			if ctx.in_static then
				print ctx "()"
			else
				print ctx "(%s))" (this ctx)
		)
	in
	match e.eexpr with
	| TCall ({ eexpr = TLocal "__keys__" },_) | TCall ({ eexpr = TLocal "__hkeys__" },_) ->
		let v = value true in
		gen_expr ctx e;
		v()
	| TConst _
	| TLocal _
	| TEnumField _
	| TArray _
	| TBinop _
	| TField _
	| TClosure _
	| TTypeExpr _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TCall _
	| TNew _
	| TUnop _
	| TFunction _ ->
		gen_expr ctx e
	| TCast (e1,t) ->
		gen_value ctx (match t with None -> e1 | Some t -> Codegen.default_cast ctx.inf.com e1 t e.etype e.epos)
	| TReturn _
	| TBreak
	| TContinue ->
		unsupported e.epos
	| TVars _
	| TFor _
	| TWhile _
	| TThrow _ ->
		(* value is discarded anyway *)
		let v = value true in
		gen_expr ctx e;
		v()
	| TBlock [e] ->
		gen_value ctx e
	| TBlock el ->
		let v = value true in
		let rec loop = function
			| [] ->
				spr ctx "return null";
			| [e] ->
				gen_expr ctx (assign e);
			| e :: l ->
				gen_expr ctx e;
				newline ctx;
				loop l
		in
		loop el;
		v();
	| TIf (cond,e,eo) ->
		spr ctx "(";
		gen_value ctx cond;
		spr ctx "?";
		gen_value ctx e;
		spr ctx ":";
		(match eo with
		| None -> spr ctx "null"
		| Some e -> gen_value ctx e);
		spr ctx ")"
	| TSwitch (cond,cases,def) ->
		let v = value true in
		gen_expr ctx (mk (TSwitch (cond,
			List.map (fun (e1,e2) -> (e1,assign e2)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TMatch (cond,enum,cases,def) ->
		let v = value true in
		gen_expr ctx (mk (TMatch (cond,enum,
			List.map (fun (constr,params,e) -> (constr,params,assign e)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TTry (b,catchs) ->
		let v = value true in
		gen_expr ctx (mk (TTry (assign b,
			List.map (fun (v,t,e) -> v, t , assign e) catchs
		)) e.etype e.epos);
		v()

let generate_field ctx static f =
	newline ctx;
	ctx.in_static <- static;
	ctx.locals <- PMap.empty;
	ctx.inv_locals <- PMap.empty;
	let public = f.cf_public || Hashtbl.mem ctx.get_sets (f.cf_name,static) || (f.cf_name = "main" && static) || f.cf_name = "resolve" in
	let rights = (if static then "static " else "") ^ (if public then "public" else "protected") in
	let p = ctx.curclass.cl_pos in
	match f.cf_expr, f.cf_kind with
	| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
		print ctx "%s " rights;
		let rec loop c =
			match c.cl_super with
			| None -> ()
			| Some (c,_) ->
				if PMap.mem f.cf_name c.cl_fields then
					spr ctx "override "
				else
					loop c
		in
		if not static then loop ctx.curclass;
		let h = gen_function_header ctx (Some (s_ident f.cf_name, f.cf_meta)) fd f.cf_params p in
		gen_expr ctx (mk_block fd.tf_expr);
		h();
		newline ctx
	| _ ->
		let is_getset = (match f.cf_kind with Var { v_read = AccCall _ } | Var { v_write = AccCall _ } -> true | _ -> false) in
		if ctx.curclass.cl_interface then
			match follow f.cf_type with
			| TFun (args,r) ->
				print ctx "function %s(" f.cf_name;
				concat ctx "," (fun (arg,o,t) ->
					let tstr = type_str ctx t p in
					print ctx "%s : %s" arg tstr;
					if o then print ctx " = %s" (default_value tstr);
				) args;
				print ctx ") : %s " (type_str ctx r p);
			| _ when is_getset -> 
				let t = type_str ctx f.cf_type p in
				let id = s_ident f.cf_name in
				(match f.cf_kind with
				| Var v ->
					(match v.v_read with
					| AccNormal | AccCall _ -> print ctx "function get %s() : %s;" id t;
					| _ -> ());
					(match v.v_write with
					| AccNormal | AccCall _ -> print ctx "function set %s( __v : %s ) : void;" id t;
					| _ -> ());
				| _ -> assert false)
			| _ -> ()
		else
		if is_getset then begin
			let t = type_str ctx f.cf_type p in
			let id = s_ident f.cf_name in
			let v = (match f.cf_kind with Var v -> v | _ -> assert false) in
			(match v.v_read with
			| AccNormal ->
				print ctx "%s function get %s() : %s { return $%s; }" rights id t id;
				newline ctx
			| AccCall m ->
				print ctx "%s function get %s() : %s { return %s(); }" rights id t m;
				newline ctx
			| AccNo | AccNever ->
				print ctx "%s function get %s() : %s { return $%s; }" (if v.v_read = AccNo then "protected" else "private") id t id;
				newline ctx
			| _ ->
				());
			(match v.v_write with
			| AccNormal ->
				print ctx "%s function set %s( __v : %s ) : void { $%s = __v; }" rights id t id;
				newline ctx
			| AccCall m ->
				print ctx "%s function set %s( __v : %s ) : void { %s(__v); }" rights id t m;
				newline ctx
			| AccNo | AccNever ->
				print ctx "%s function set %s( __v : %s ) : void { $%s = __v; }" (if v.v_write = AccNo then "protected" else "private") id t id;
				newline ctx
			| _ -> ());
			print ctx "protected var $%s : %s" (s_ident f.cf_name) (type_str ctx f.cf_type p);
		end else begin
			print ctx "%s var %s : %s" rights (s_ident f.cf_name) (type_str ctx f.cf_type p);
			match f.cf_expr with
			| None -> ()
			| Some e ->
				print ctx " = ";
				gen_value ctx e
		end

let rec define_getset ctx stat c =
	let def f name =
		Hashtbl.add ctx.get_sets (name,stat) f.cf_name
	in
	let field f =
		match f.cf_kind with
		| Method _ -> ()
		| Var v ->
			(match v.v_read with AccCall m -> def f m | _ -> ());
			(match v.v_write with AccCall m -> def f m | _ -> ())
	in
	List.iter field (if stat then c.cl_ordered_statics else c.cl_ordered_fields);
	match c.cl_super with
	| Some (c,_) when not stat -> define_getset ctx stat c
	| _ -> ()


let generate_class ctx c =
	ctx.curclass <- c;
	define_getset ctx true c;
	define_getset ctx false c;
	ctx.local_types <- List.map snd c.cl_types;
	let pack = open_block ctx in
	print ctx "\tpublic %s%s %s " (match c.cl_dynamic with None -> "" | Some _ -> if c.cl_interface then "" else "dynamic ") (if c.cl_interface then "interface" else "class") (snd c.cl_path);
	(match c.cl_super with
	| None -> ()
	| Some (csup,_) -> print ctx "extends %s " (s_path ctx true csup.cl_path c.cl_pos));
	(match c.cl_implements with
	| [] -> ()
	| l ->
		spr ctx (if c.cl_interface then "extends " else "implements ");
		concat ctx ", " (fun (i,_) -> print ctx "%s" (s_path ctx true i.cl_path c.cl_pos)) l);
	spr ctx "{";
	let cl = open_block ctx in
	(match c.cl_constructor with
	| None -> ()
	| Some f ->
		let f = { f with
			cf_name = snd c.cl_path;
			cf_public = true;
			cf_kind = Method MethNormal;
		} in
		ctx.constructor_block <- true;
		generate_field ctx false f;
	);
	List.iter (generate_field ctx false) c.cl_ordered_fields;
	List.iter (generate_field ctx true) c.cl_ordered_statics;
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_main ctx inits =
	ctx.curclass <- { null_class with cl_path = [],"__main__" };
	let pack = open_block ctx in
	print ctx "\timport flash.Lib";
	newline ctx;
	print ctx "public class __main__ extends %s {" (s_path ctx true (["flash"],"Boot") Ast.null_pos);
	let cl = open_block ctx in
	newline ctx;
	spr ctx "public function __main__() {";
	let fl = open_block ctx in
	newline ctx;
	spr ctx "super()";
	newline ctx;
	spr ctx "flash.Lib.current = this";
	List.iter (fun e -> newline ctx; gen_expr ctx e) inits;
	fl();
	newline ctx;
	print ctx "}";
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_enum ctx e =
	ctx.local_types <- List.map snd e.e_types;
	let pack = open_block ctx in
	let ename = snd e.e_path in
	print ctx "\tpublic class %s extends enum {" ename;
	let cl = open_block ctx in
	newline ctx;
	print ctx "public static const __isenum : Boolean = true";
	newline ctx;
	print ctx "public function %s( t : String, index : int, p : Array = null ) : void { this.tag = t; this.index = index; this.params = p; }" ename;
	PMap.iter (fun _ c ->
		newline ctx;
		match c.ef_type with
		| TFun (args,_) ->
			print ctx "public static function %s(" c.ef_name;
			concat ctx ", " (fun (a,o,t) ->
				print ctx "%s : %s" (s_ident a) (type_str ctx t c.ef_pos);
				if o then spr ctx " = null";
			) args;
			print ctx ") : %s {" ename;
			print ctx " return new %s(\"%s\",%d,[" ename c.ef_name c.ef_index;
			concat ctx "," (fun (a,_,_) -> spr ctx (s_ident a)) args;
			print ctx "]); }";
		| _ ->
			print ctx "public static var %s : %s = new %s(\"%s\",%d)" c.ef_name ename ename c.ef_name c.ef_index;
	) e.e_constrs;
	newline ctx;
	(match Codegen.build_metadata ctx.inf.com (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "public static var __meta__ : * = ";
		gen_expr ctx e;
		newline ctx);
	print ctx "public static var __constructs__ : Array = [%s];" (String.concat "," (List.map (fun s -> "\"" ^ Ast.s_escape s ^ "\"") e.e_names));	
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_base_enum ctx =
	let pack = open_block ctx in
	spr ctx "\tpublic class enum {";
	let cl = open_block ctx in
	newline ctx;
	spr ctx "public var tag : String";
	newline ctx;
	spr ctx "public var index : int";
	newline ctx;
	spr ctx "public var params : Array";
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate com =
	let infos = {
		com = com;
	} in
	let ctx = init infos ([],"enum") in
	generate_base_enum ctx;
	close ctx;
	let inits = ref [] in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			let c = (match c.cl_path with
				| ["flash"],"FlashXml__" -> { c with cl_path = [],"Xml" }
				| (pack,name) -> { c with cl_path = (pack,protect name) }
			) in
			(match c.cl_init with
			| None -> ()
			| Some e -> inits := e :: !inits);
			if c.cl_extern then
				()
			else
				let ctx = init infos c.cl_path in
				generate_class ctx c;
				close ctx
		| TEnumDecl e ->
			let pack,name = e.e_path in
			let e = { e with e_path = (pack,protect name) } in
			if e.e_extern && e.e_path <> ([],"Void") then
				()
			else
				let ctx = init infos e.e_path in
				generate_enum ctx e;
				close ctx
		| TTypeDecl t ->
			()
	) com.types;
	(match com.main with
	| None -> ()
	| Some e -> inits := e :: !inits);
	let ctx = init infos ([],"__main__") in
	generate_main ctx (List.rev !inits);
	close ctx
