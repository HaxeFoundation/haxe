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
open Ast
open Type
open Common

type pos = Ast.pos

type sourcemap = {
	sources : (string) DynArray.t;
	sources_hash : (string, int) Hashtbl.t;
	mappings : Buffer.t;

	mutable source_last_line : int;
	mutable source_last_col : int;
	mutable source_last_file : int;
	mutable print_comma : bool;
	mutable output_last_col : int;
	mutable output_current_col : int;
}

type ctx = {
	com : Common.context;
	buf : Buffer.t;
	packages : (string list,unit) Hashtbl.t;
	smap : sourcemap;
	js_modern : bool;

	mutable current : tclass;
	mutable statics : (tclass * string * texpr) list;
	mutable inits : texpr list;
	mutable tabs : string;
	mutable in_value : tvar option;
	mutable in_loop : bool;
	mutable handle_break : bool;
	mutable id_counter : int;
	mutable type_accessor : module_type -> string;
	mutable separator : bool;
	mutable found_expose : bool;
}

let s_path ctx = Ast.s_type_path

let kwds =
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) [
		"abstract"; "as"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "continue"; "const";
		"debugger"; "default"; "delete"; "do"; "double"; "else"; "enum"; "export"; "extends"; "false"; "final";
		"finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int";
		"interface"; "is"; "long"; "namespace"; "native"; "new"; "null"; "package"; "private"; "protected";
		"public"; "return"; "short"; "static"; "super"; "switch"; "synchronized"; "this"; "throw"; "throws";
		"transient"; "true"; "try"; "typeof"; "use"; "var"; "void"; "volatile"; "while"; "with"
	];
	h

let valid_js_ident s =
	try
		for i = 0 to String.length s - 1 do
			match String.unsafe_get s i with
			| 'a'..'z' | 'A'..'Z' | '$' | '_' -> ()
			| '0'..'9' when i > 0 -> ()
			| _ -> raise Exit
		done;
		true
	with Exit ->
		false

let field s = if Hashtbl.mem kwds s then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "$" ^ s else s
let anon_field s = if Hashtbl.mem kwds s || not (valid_js_ident s) then "'" ^ s ^ "'" else s

let handle_newlines ctx str =
	if ctx.com.debug then
		let rec loop from =
			try begin
				let next = String.index_from str from '\n' + 1 in
				Buffer.add_char ctx.smap.mappings ';';
				ctx.smap.output_last_col <- 0;
				ctx.smap.print_comma <- false;
				loop next
			end with Not_found ->
				ctx.smap.output_current_col <- String.length str - from
		in
		loop 0
	else ()

let spr ctx s =
	ctx.separator <- false;
	handle_newlines ctx s;
	Buffer.add_string ctx.buf s

let print ctx =
	ctx.separator <- false;
	Printf.kprintf (fun s -> begin
		handle_newlines ctx s;
		Buffer.add_string ctx.buf s
	end)

let unsupported p = error "This expression cannot be compiled to Javascript" p

let add_mapping ctx pos =
	let smap = ctx.smap in
	let file = try
		Hashtbl.find smap.sources_hash pos.pfile
	with Not_found ->
		let length = DynArray.length smap.sources in
		Hashtbl.replace smap.sources_hash pos.pfile length;
		DynArray.add smap.sources pos.pfile;
		length
	in
	let line, col = Lexer.find_pos pos in
	let line = line - 1 in
	let col = col - 1 in
	if smap.source_last_file != file || smap.source_last_line != line || smap.source_last_col != col then begin
		if smap.print_comma then
			Buffer.add_char smap.mappings ','
		else
			smap.print_comma <- true;

		let base64_vlq number =
			let encode_digit digit =
				let chars = [|
					'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
					'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
					'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
					'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
				|] in
				Array.unsafe_get chars digit
			in
			let to_vlq number =
				if number < 0 then
					((-number) lsl 1) + 1
				else
					number lsl 1
			in
			let rec loop vlq =
				let shift = 5 in
				let base = 1 lsl shift in
				let mask = base - 1 in
				let continuation_bit = base in
				let digit = vlq land mask in
				let next = vlq asr shift in
				Buffer.add_char smap.mappings (encode_digit (
					if next > 0 then digit lor continuation_bit else digit));
				if next > 0 then loop next else ()
			in
			loop (to_vlq number)
		in

		base64_vlq (smap.output_current_col - smap.output_last_col);
		base64_vlq (file - smap.source_last_file);
		base64_vlq (line - smap.source_last_line);
		base64_vlq (col - smap.source_last_col);

		smap.source_last_file <- file;
		smap.source_last_line <- line;
		smap.source_last_col <- col;
		smap.output_last_col <- smap.output_current_col
	end

let basename path =
	try
		let idx = String.rindex path '/' in
		String.sub path (idx + 1) (String.length path - idx - 1)
	with Not_found -> path

let write_mappings ctx =
	let basefile = basename ctx.com.file in
	print ctx "\n//@ sourceMappingURL=%s.map" basefile;
	let channel = open_out_bin (ctx.com.file ^ ".map") in
	let sources = DynArray.to_list ctx.smap.sources in
	let to_url file =
		ExtString.String.map (fun c -> if c == '\\' then '/' else c) (Common.get_full_path file)
	in
	output_string channel "{\n";
	output_string channel "\"version\":3,\n";
	output_string channel ("\"file\":\"" ^ basefile ^ "\",\n");
	output_string channel ("\"sourceRoot\":\"file://\",\n");
	output_string channel ("\"sources\":[" ^
		(String.concat "," (List.map (fun s -> "\"" ^ to_url s ^ "\"") sources)) ^
		"],\n");
	output_string channel "\"names\":[],\n";
	output_string channel "\"mappings\":\"";
	Buffer.output_buffer channel ctx.smap.mappings;
	output_string channel "\"\n";
	output_string channel "}";
	close_out channel

let newline ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' | '{' | ':' when not ctx.separator -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx ";\n%s" ctx.tabs

let newprop ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '{' -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx "\n%s," ctx.tabs

let semicolon ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' when not ctx.separator -> ()
	| _ -> spr ctx ";"

let rec concat ctx s f = function
	| [] -> ()
	| [x] -> f x
	| x :: l ->
		f x;
		spr ctx s;
		concat ctx s f l

let fun_block ctx f p =
	let e = List.fold_left (fun e (a,c) ->
		match c with
		| None | Some TNull -> e
		| Some c -> Codegen.concat (Codegen.set_default ctx.com a c p) e
	) f.tf_expr f.tf_args in
	e

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let rec has_return e =
	match e.eexpr with
	| TBlock [] -> false
	| TBlock el -> has_return (List.hd (List.rev el))
	| TReturn _ -> true
	| _ -> false

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ | TMatch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let handle_break ctx e =
	let old = ctx.in_loop, ctx.handle_break in
	ctx.in_loop <- true;
	try
		iter_switch_break false e;
		ctx.handle_break <- false;
		(fun() ->
			ctx.in_loop <- fst old;
			ctx.handle_break <- snd old;
		)
	with
		Exit ->
			spr ctx "try {";
			let b = open_block ctx in
			newline ctx;
			ctx.handle_break <- true;
			(fun() ->
				b();
				ctx.in_loop <- fst old;
				ctx.handle_break <- snd old;
				newline ctx;
				spr ctx "} catch( e ) { if( e != \"__break__\" ) throw e; }";
			)

let handle_expose ctx path meta =
	let rec loop = function
		| (":expose", args, pos) :: l ->
			ctx.found_expose <- true;
			let exposed_path = (match args with
				| [EConst (String s), _] -> s
				| [] -> path
				| _ -> error "Invalid @:expose parameters" pos
			) in
			print ctx "$hxExpose(%s, \"%s\")" path exposed_path;
			newline ctx
		| _ :: l -> loop l
		| [] -> ()
	in
	loop meta

let this ctx = match ctx.in_value with None -> "this" | Some _ -> "$this"

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s ->
		if String.contains s '\000' then error "A String cannot contain \\0 characters" p;
		print ctx "\"%s\"" (Ast.s_escape s)
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx (this ctx)
	| TSuper -> assert false

let rec gen_call ctx e el =
	match e.eexpr , el with
	| TConst TSuper , params ->
		(match ctx.current.cl_super with
		| None -> error "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			print ctx "%s.call(%s" (ctx.type_accessor (TClassDecl c)) (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TField ({ eexpr = TConst TSuper },name) , params ->
		(match ctx.current.cl_super with
		| None -> error "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			print ctx "%s.prototype%s.call(%s" (ctx.type_accessor (TClassDecl c)) (field name) (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TCall (x,_) , el when (match x.eexpr with TLocal { v_name = "__js__" } -> false | _ -> true) ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TLocal { v_name = "__new__" }, { eexpr = TConst (TString cl) } :: params ->
		print ctx "new %s(" cl;
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__new__" }, e :: params ->
		spr ctx "new ";
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__js__" }, [{ eexpr = TConst (TString code) }] ->
		spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
	| TLocal { v_name = "__resources__" }, [] ->
		spr ctx "[";
		concat ctx "," (fun (name,data) ->
			spr ctx "{ ";
			spr ctx "name : ";
			gen_constant ctx e.epos (TString name);
			spr ctx ", data : ";
			gen_constant ctx e.epos (TString (Codegen.bytes_serialize data));
			spr ctx "}"
		) (Hashtbl.fold (fun name data acc -> (name,data) :: acc) ctx.com.resources []);
		spr ctx "]";
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

and gen_expr ctx e =
	match e.eexpr with
	| TConst c -> gen_constant ctx e.epos c
	| TLocal v -> spr ctx (ident v.v_name)
	| TEnumField (e,s) ->
		print ctx "%s%s" (ctx.type_accessor (TEnumDecl e)) (field s)
	| TArray (e1,e2) ->
		gen_value ctx e1;
		spr ctx "[";
		gen_value ctx e2;
		spr ctx "]";
	| TBinop (op,e1,e2) ->
		gen_value ctx e1;
		print ctx " %s " (Ast.s_binop op);
		gen_value ctx e2;
	| TField (x,s) ->
		gen_value ctx x;
		spr ctx (field s)
	| TClosure ({ eexpr = TTypeExpr _ } as x,s) ->
		gen_value ctx x;
		spr ctx (field s)
	| TClosure (x,s) ->
		(match x.eexpr with
		| TConst _ | TLocal _ ->  
			gen_value ctx x; 
			print ctx ".%s.$bind(" s; 
			gen_value ctx x; 
			print ctx ")"
		| _ -> 
			print ctx "($_=";
			gen_value ctx x;
			print ctx ",$_.%s.$bind($_))" s)
	| TTypeExpr t ->
		spr ctx (ctx.type_accessor t)
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| TReturn eo ->
		if ctx.in_value <> None then unsupported e.epos;
		(match eo with
		| None ->
			spr ctx "return"
		| Some e ->
			spr ctx "return ";
			gen_value ctx e);
	| TBreak ->
		if not ctx.in_loop then unsupported e.epos;
		if ctx.handle_break then spr ctx "throw \"__break__\"" else spr ctx "break"
	| TContinue ->
		if not ctx.in_loop then unsupported e.epos;
		spr ctx "continue"
	| TBlock el ->
		print ctx "{";
		let bend = open_block ctx in
		List.iter (gen_block ctx) el;
		bend();
		newline ctx;
		print ctx "}";
	| TFunction f ->
		let old = ctx.in_value, ctx.in_loop in
		ctx.in_value <- None;
		ctx.in_loop <- false;
		print ctx "function(%s) " (String.concat "," (List.map ident (List.map arg_name f.tf_args)));
		gen_expr ctx (fun_block ctx f e.epos);
		ctx.in_value <- fst old;
		ctx.in_loop <- snd old;
		ctx.separator <- true
	| TCall (e,el) ->
		gen_call ctx e el
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
		concat ctx ", " (fun (v,e) ->
			spr ctx (ident v.v_name);
			match e with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				gen_value ctx e
		) vl;
	| TNew (c,_,el) ->
		print ctx "new %s(" (ctx.type_accessor (TClassDecl c));
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_value ctx cond;
		spr ctx " ";
		gen_expr ctx e;
		(match eelse with
		| None -> ()
		| Some e2 ->
			(match e.eexpr with
			| TObjectDecl _ -> ctx.separator <- false
			| _ -> ());
			semicolon ctx;
			spr ctx " else ";
			gen_expr ctx e2);
	| TUnop (op,Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_value ctx e
	| TUnop (op,Ast.Postfix,e) ->
		gen_value ctx e;
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "while";
		gen_value ctx cond;
		spr ctx " ";
		gen_expr ctx e;
		handle_break();
	| TWhile (cond,e,Ast.DoWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "do ";
		gen_expr ctx e;
		semicolon ctx;
		spr ctx " while";
		gen_value ctx cond;
		handle_break();
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s : " (anon_field f); gen_value ctx e) fields;
		spr ctx "}";
		ctx.separator <- true
	| TFor (v,it,e) ->
		let handle_break = handle_break ctx e in
		let it = (match it.eexpr with
			| TLocal v -> v.v_name
			| _ ->
				let id = ctx.id_counter in
				ctx.id_counter <- ctx.id_counter + 1;
				let name = "$it" ^ string_of_int id in
				print ctx "var %s = " name;
				gen_value ctx it;
				newline ctx;
				name
		) in
		print ctx "while( %s.hasNext() ) {" it;
		let bend = open_block ctx in
		newline ctx;
		print ctx "var %s = %s.next()" (ident v.v_name) it;
		gen_block ctx e;
		bend();
		newline ctx;
		spr ctx "}";
		handle_break();
	| TTry (e,catchs) ->
		spr ctx "try ";
		gen_expr ctx e;
		let vname = (match catchs with [(v,_)] -> v.v_name | _ ->
			let id = ctx.id_counter in
			ctx.id_counter <- ctx.id_counter + 1;
			"$e" ^ string_of_int id
		) in
		print ctx " catch( %s ) {" vname;
		let bend = open_block ctx in
		let last = ref false in
		let else_block = ref false in
		List.iter (fun (v,e) ->
			if !last then () else
			let t = (match follow v.v_type with
			| TEnum (e,_) -> Some (TEnumDecl e)
			| TInst (c,_) -> Some (TClassDecl c)
			| TFun _
			| TLazy _
			| TType _
			| TAnon _ ->
				assert false
			| TMono _
			| TDynamic _ ->
				None
			) in
			match t with
			| None ->
				last := true;
				if !else_block then print ctx "{";
				if vname <> v.v_name then begin
					newline ctx;
					print ctx "var %s = %s" v.v_name vname;
				end;
				gen_block ctx e;
				if !else_block then begin
					newline ctx;
					print ctx "}";
				end
			| Some t ->
				if not !else_block then newline ctx;
				print ctx "if( %s.__instanceof(%s," (ctx.type_accessor (TClassDecl { null_class with cl_path = ["js"],"Boot" })) vname;
				gen_value ctx (mk (TTypeExpr t) (mk_mono()) e.epos);
				spr ctx ") ) {";
				let bend = open_block ctx in
				if vname <> v.v_name then begin
					newline ctx;
					print ctx "var %s = %s" v.v_name vname;
				end;
				gen_block ctx e;
				bend();
				newline ctx;
				spr ctx "} else ";
				else_block := true
		) catchs;
		if not !last then print ctx "throw(%s)" vname;
		bend();
		newline ctx;
		spr ctx "}";
	| TMatch (e,(estruct,_),cases,def) ->
		let evar = (if List.for_all (fun (_,pl,_) -> pl = None) cases then begin
			spr ctx "switch( ";
			gen_value ctx (if Optimizer.need_parent e then Codegen.mk_parent e else e);
			spr ctx "[1] ) {";
			"???"
		end else begin
			let v = (match e.eexpr with
				| TLocal v -> v.v_name
				| _ ->
					spr ctx "var $e = ";
					gen_value ctx e;
					newline ctx;
					"$e"
			) in
			print ctx "switch( %s[1] ) {" v;
			v
		end) in
		List.iter (fun (cl,params,e) ->
			List.iter (fun c ->
				newline ctx;
				print ctx "case %d:" c;
			) cl;
			let bend = open_block ctx in
			(match params with
			| None -> ()
			| Some l ->
				let n = ref 1 in
				let l = List.fold_left (fun acc v -> incr n; match v with None -> acc | Some v -> (v.v_name,!n) :: acc) [] l in
				newline ctx;
				spr ctx "var ";
				concat ctx ", " (fun (v,n) ->
					print ctx "%s = %s[%d]" (ident v) evar n;
				) l);
			gen_block ctx e;
			if not (has_return e) then begin
				newline ctx;
				print ctx "break";
			end;
			bend();
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			newline ctx;
			spr ctx "default:";
			let bend = open_block ctx in
			gen_block ctx e;
			bend();
		);
		newline ctx;
		spr ctx "}"
	| TSwitch (e,cases,def) ->
		spr ctx "switch";
		gen_value ctx e;
		spr ctx " {";
		newline ctx;
		List.iter (fun (el,e2) ->
			List.iter (fun e ->
				match e.eexpr with
				| TConst(c) when c = TNull ->
					spr ctx "case null: case undefined:";
				| _ ->
					spr ctx "case ";
					gen_value ctx e;
					spr ctx ":"
			) el;
			let bend = open_block ctx in
			gen_block ctx e2;
			if not (has_return e2) then begin
				newline ctx;
				print ctx "break";
			end;
			bend();
			newline ctx;
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			let bend = open_block ctx in
			gen_block ctx e;
			bend();
			newline ctx;
		);
		spr ctx "}"
	| TCast (e,None) ->
		gen_expr ctx e
	| TCast (e1,Some t) ->
		gen_expr ctx (Codegen.default_cast ctx.com e1 t e.etype e.epos)

and gen_block ctx e =
	match e.eexpr with
	| TBlock el -> List.iter (gen_block ctx) el
	| _ -> newline ctx; gen_expr ctx e

and gen_value ctx e =
	if ctx.com.debug && e.epos.pmin >= 0 then
		add_mapping ctx e.epos;
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> assert false | Some v -> v)) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let value() =
		let old = ctx.in_value, ctx.in_loop in
		let r = alloc_var "$r" t_dynamic in
		ctx.in_value <- Some r;
		ctx.in_loop <- false;
		spr ctx "(function($this) ";
		spr ctx "{";
		let b = open_block ctx in
		newline ctx;
		spr ctx "var $r";
		newline ctx;
		(fun() ->
			newline ctx;
			spr ctx "return $r";
			b();
			newline ctx;
			spr ctx "}";
			ctx.in_value <- fst old;
			ctx.in_loop <- snd old;
			print ctx "(%s))" (this ctx)
		)
	in
	match e.eexpr with
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
	| TReturn _
	| TBreak
	| TContinue ->
		unsupported e.epos
	| TCast (e1,t) ->
		gen_value ctx (match t with None -> e1 | Some t -> Codegen.default_cast ctx.com e1 t e.etype e.epos)
	| TVars _
	| TFor _
	| TWhile _
	| TThrow _ ->
		(* value is discarded anyway *)
		let v = value() in
		gen_expr ctx e;
		v()
	| TBlock [e] ->
		gen_value ctx e
	| TBlock el ->
		let v = value() in
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
		(* remove parenthesis unless it's an operation with higher precedence than ?: *)
		let cond = (match cond.eexpr with
			| TParenthesis { eexpr = TBinop ((Ast.OpAssign | Ast.OpAssignOp _),_,_) } -> cond
			| TParenthesis e -> e
			| _ -> cond
		) in
		gen_value ctx cond;
		spr ctx "?";
		gen_value ctx e;
		spr ctx ":";
		(match eo with
		| None -> spr ctx "null"
		| Some e -> gen_value ctx e);
	| TSwitch (cond,cases,def) ->
		let v = value() in
		gen_expr ctx (mk (TSwitch (cond,
			List.map (fun (e1,e2) -> (e1,assign e2)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TMatch (cond,enum,cases,def) ->
		let v = value() in
		gen_expr ctx (mk (TMatch (cond,enum,
			List.map (fun (constr,params,e) -> (constr,params,assign e)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TTry (b,catchs) ->
		let v = value() in
		let block e = mk (TBlock [e]) e.etype e.epos in
		gen_expr ctx (mk (TTry (block (assign b),
			List.map (fun (v,e) -> v, block (assign e)) catchs
		)) e.etype e.epos);
		v()

let generate_package_create ctx (p,_) =
	let rec loop acc = function
		| [] -> ()
		| p :: l when Hashtbl.mem ctx.packages (p :: acc) -> loop (p :: acc) l
		| p :: l ->
			Hashtbl.add ctx.packages (p :: acc) ();
			(match acc with
			| [] ->
				if ctx.js_modern then
					print ctx "var %s = {}" p
				else
					print ctx "var %s = %s || {}" p p
			| _ ->
				let p = String.concat "." (List.rev acc) ^ (field p) in
				if ctx.js_modern then
					print ctx "%s = {}" p
				else
					print ctx "if(!%s) %s = {}" p p
			);
			newline ctx;
			loop (p :: acc) l
	in
	match p with
	| [] -> print ctx "var "
	| _ -> loop [] p

let check_field_name c f =
	match f.cf_name with
	| "prototype" | "__proto__" | "constructor" ->
		error ("The field name '" ^ f.cf_name ^ "'  is not allowed in JS") (match f.cf_expr with None -> c.cl_pos | Some e -> e.epos);
	| _ -> ()

let gen_class_static_field ctx c f =
	check_field_name c f;
	match f.cf_expr with
	| None ->
		print ctx "%s%s = null" (s_path ctx c.cl_path) (field f.cf_name);
		newline ctx
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			let path = (s_path ctx c.cl_path) ^ (field f.cf_name) in
			ctx.id_counter <- 0;
			print ctx "%s = " path;
			gen_value ctx e;
			ctx.separator <- false;
			newline ctx;
			handle_expose ctx path f.cf_meta
		| _ ->
			ctx.statics <- (c,f.cf_name,e) :: ctx.statics

let gen_class_field ctx c f =
	check_field_name c f;
	newprop ctx;
	print ctx "%s: " (anon_field f.cf_name);
	match f.cf_expr with
	| None ->
		print ctx "null";
	| Some e ->
		ctx.id_counter <- 0;
		gen_value ctx e;
		ctx.separator <- false

let generate_class ctx c =
	ctx.current <- c;
	ctx.id_counter <- 0;
	(match c.cl_path with
	| [],"Function" -> error "This class redefine a native one" c.cl_pos
	| _ -> ());
	let p = s_path ctx c.cl_path in
	generate_package_create ctx c.cl_path;
	if ctx.js_modern then
		print ctx "%s = " p
	else
		print ctx "%s = $hxClasses[\"%s\"] = " p p;
	(match c.cl_constructor with
	| Some { cf_expr = Some e } -> gen_expr ctx e
	| _ -> print ctx "function() { }");
	newline ctx;
	if ctx.js_modern then begin
		print ctx "$hxClasses[\"%s\"] = %s" p p;
		newline ctx;
	end;
	handle_expose ctx p c.cl_meta;
	print ctx "%s.__name__ = [%s]" p (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst c.cl_path @ [snd c.cl_path])));
	newline ctx;
	(match c.cl_implements with
	| [] -> ()
	| l ->
		print ctx "%s.__interfaces__ = [%s]" p (String.concat "," (List.map (fun (i,_) -> s_path ctx i.cl_path) l));
		newline ctx;
	);

	let gen_props props = 
		String.concat "," (List.map (fun (p,v) -> p ^":\""^v^"\"") props)
	in

	(match Codegen.get_properties c.cl_ordered_statics with
	| [] -> ()
	| props ->
		print ctx "%s.__properties__ = {%s}" p (gen_props props);
		newline ctx);

	List.iter (gen_class_static_field ctx c) c.cl_ordered_statics;

	(match c.cl_super with
	| None -> print ctx "%s.prototype = {" p;
	| Some (csup,_) ->
		let psup = s_path ctx csup.cl_path in
		print ctx "%s.__super__ = %s" p psup;
		newline ctx;
		print ctx "%s.prototype = $extend(%s.prototype,{" p psup;
	);

	let bend = open_block ctx in
	List.iter (fun f -> match f.cf_kind with Var { v_read = AccResolve } -> () | _ -> gen_class_field ctx c f) c.cl_ordered_fields;
	newprop ctx;
	print ctx "__class__: %s" p;

	let props = Codegen.get_properties c.cl_ordered_fields in
	(match c.cl_super with
	| _ when props = [] -> ()
	| Some (csup,_) when Codegen.has_properties csup ->
		newprop ctx;
		let psup = s_path ctx csup.cl_path in
		print ctx "__properties__: $extend(%s.prototype.__properties__,{%s})" psup (gen_props props)
	| _ ->
		newprop ctx;
		print ctx "__properties__: {%s}" (gen_props props));
	
	bend();
	print ctx "\n}";
	(match c.cl_super with None -> () | _ -> print ctx ")");
	newline ctx

let generate_enum ctx e =
	let p = s_path ctx e.e_path in
	generate_package_create ctx e.e_path;
	let ename = List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst e.e_path @ [snd e.e_path]) in
	print ctx "%s = $hxClasses[\"%s\"] = { __ename__ : [%s], __constructs__ : [%s] }" p p (String.concat "," ename) (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" s) e.e_names));
	newline ctx;
	List.iter (fun n ->
		let f = PMap.find n e.e_constrs in
		print ctx "%s%s = " p (field f.ef_name);
		(match f.ef_type with
		| TFun (args,_) ->
			let sargs = String.concat "," (List.map (fun (n,_,_) -> n) args) in
			print ctx "function(%s) { var $x = [\"%s\",%d,%s]; $x.__enum__ = %s; $x.toString = $estr; return $x; }" sargs f.ef_name f.ef_index sargs p;
		| _ ->
			print ctx "[\"%s\",%d]" f.ef_name f.ef_index;
			newline ctx;
			print ctx "%s%s.toString = $estr" p (field f.ef_name);
			newline ctx;
			print ctx "%s%s.__enum__ = %s" p (field f.ef_name) p;
		);
		newline ctx
	) e.e_names;
	match Codegen.build_metadata ctx.com (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "%s.__meta__ = " p;
		gen_expr ctx e;
		newline ctx

let generate_static ctx (c,f,e) =
	print ctx "%s%s = " (s_path ctx c.cl_path) (field f);
	gen_value ctx e;
	newline ctx

let generate_type ctx = function
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e -> ctx.inits <- e :: ctx.inits);
		if not c.cl_extern then generate_class ctx c
	| TEnumDecl e when e.e_extern ->
		()
	| TEnumDecl e -> generate_enum ctx e
	| TTypeDecl _ -> ()

let set_current_class ctx c =
	ctx.current <- c

let alloc_ctx com =
	let ctx = {
		com = com;
		buf = Buffer.create 16000;
		packages = Hashtbl.create 0;
		smap = {
			source_last_line = 0;
			source_last_col = 0;
			source_last_file = 0;
			print_comma = false;
			output_last_col = 0;
			output_current_col = 0;
			sources = DynArray.create();
			sources_hash = Hashtbl.create 0;
			mappings = Buffer.create 16;
		};
		js_modern = Common.defined com "js_modern";
		statics = [];
		inits = [];
		current = null_class;
		tabs = "";
		in_value = None;
		in_loop = false;
		handle_break = false;
		id_counter = 0;
		type_accessor = (fun _ -> assert false);
		separator = false;
		found_expose = false;
	} in
	ctx.type_accessor <- (fun t -> s_path ctx (t_path t));
	ctx

let gen_single_expr ctx e expr =
	if expr then gen_expr ctx e else gen_value ctx e;
	let str = Buffer.contents ctx.buf in
	Buffer.reset ctx.buf;
	ctx.id_counter <- 0;
	str

let generate com =
	let t = Common.timer "generate js" in
	(match com.js_gen with
	| Some g -> g()
	| None ->
	let ctx = alloc_ctx com in
	if ctx.js_modern then begin
		(* Additional ES5 strict mode keywords. *)
		List.iter (fun s -> Hashtbl.replace kwds s ()) [ "arguments"; "eval" ];

		(* Wrap output in a closure. *)
		print ctx "(function () { \"use strict\"";
		newline ctx;
	end;

	let hxClasses = if ctx.js_modern then "{}" else "$hxClasses || {}" in
	print ctx "var $_, $hxClasses = %s, $estr = function() { return js.Boot.__string_rec(this,''); }
function $extend(from, fields) {
	function inherit() {}; inherit.prototype = from; var proto = new inherit();
	for (var name in fields) proto[name] = fields[name];
	return proto;
}" hxClasses;
	newline ctx;
	List.iter (generate_type ctx) com.types;
	print ctx "js.Boot.__res = {}";
	newline ctx;
	print ctx "js.Boot.__init()";
	newline ctx;
	List.iter (fun e ->
		gen_expr ctx e;
		newline ctx;
	) (List.rev ctx.inits);
	List.iter (generate_static ctx) (List.rev ctx.statics);
	(match com.main with
	| None -> ()
	| Some e -> gen_expr ctx e);
	if ctx.found_expose then begin
		newline ctx;
		print ctx
"function $hxExpose(src, path) {
	var o = window;
	var parts = path.split(\".\");
	for(var ii = 0; ii < parts.length-1; ++ii) {
		var p = parts[ii];
		if(typeof o[p] == \"undefined\") o[p] = {};
		o = o[p];
	}
	o[parts[parts.length-1]] = src;
}";
	end;
	if ctx.js_modern then begin
		newline ctx;
		print ctx "})()";
	end;
	if com.debug then write_mappings ctx;
	let ch = open_out_bin com.file in
	output_string ch (Buffer.contents ctx.buf);
	close_out ch);
	t()

