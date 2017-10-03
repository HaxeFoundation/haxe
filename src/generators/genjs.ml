(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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

open Globals
open Ast
open Type
open Common

type sourcemap = {
	sources : (string) DynArray.t;
	sources_hash : (string, int) Hashtbl.t;
	mappings : Rbuffer.t;

	mutable source_last_line : int;
	mutable source_last_col : int;
	mutable source_last_file : int;
	mutable print_comma : bool;
	mutable output_last_col : int;
	mutable output_current_col : int;
	mutable current_expr : texpr option;
}

type ctx = {
	com : Common.context;
	buf : Rbuffer.t;
	chan : out_channel;
	packages : (string list,unit) Hashtbl.t;
	smap : sourcemap option;
	js_modern : bool;
	js_flatten : bool;
	es_version : int;
	store_exception_stack : bool;
	mutable current : tclass;
	mutable statics : (tclass * string * texpr) list;
	mutable inits : texpr list;
	mutable tabs : string;
	mutable in_value : tvar option;
	mutable in_loop : bool;
	mutable id_counter : int;
	mutable type_accessor : module_type -> string;
	mutable separator : bool;
	mutable found_expose : bool;
}

type object_store = {
	os_name : string;
	mutable os_fields : object_store list;
}

let get_exposed ctx path meta =
	try
		let (_, args, pos) = Meta.get Meta.Expose meta in
		(match args with
			| [ EConst (String (s,_)), _ ] -> [s]
			| [] -> [path]
			| _ -> abort "Invalid @:expose parameters" pos)
	with Not_found -> []

let dot_path = s_type_path

let s_path ctx = if ctx.js_flatten then Path.flat_path else dot_path

let kwds = Hashtbl.create 0

let setup_kwds lst =
	List.iter (fun s -> Hashtbl.add kwds s ()) lst

let es3kwds = [
	"abstract"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue";
	"debugger"; "default"; "delete"; "do"; "double"; "else"; "enum"; "export"; "extends"; "false"; "final";
	"finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int";
	"interface"; "long"; "native"; "new"; "null"; "package"; "private"; "protected";
	"public"; "return"; "short"; "static"; "super"; "switch"; "synchronized"; "this"; "throw"; "throws";
	"transient"; "true"; "try"; "typeof"; "var"; "void"; "volatile"; "while"; "with"
]

let es5kwds = [
	"arguments"; "break"; "case"; "catch"; "class"; "const"; "continue";
	"debugger"; "default"; "delete"; "do"; "else"; "enum"; "eval"; "export"; "extends"; "false";
	"finally"; "for"; "function"; "if"; "implements"; "import"; "in"; "instanceof";
	"interface"; "let"; "new"; "null"; "package"; "private"; "protected";
	"public"; "return"; "static"; "super"; "switch"; "this"; "throw";
	"true"; "try"; "typeof"; "var"; "void"; "while"; "with"; "yield"
]

(* Identifiers Haxe reserves to make the JS output cleaner. These can still be used in untyped code (TLocal),
   but are escaped upon declaration. *)
let kwds2 =
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) [
		(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects *)
		"Infinity"; "NaN"; "decodeURI"; "decodeURIComponent"; "encodeURI"; "encodeURIComponent";
		"escape"; "eval"; "isFinite"; "isNaN"; "parseFloat"; "parseInt"; "undefined"; "unescape";

		"JSON"; "Number"; "Object"; "console"; "window"; "require";
	];
	h

let valid_js_ident s =
	String.length s > 0 && try
		for i = 0 to String.length s - 1 do
			match String.unsafe_get s i with
			| 'a'..'z' | 'A'..'Z' | '$' | '_' -> ()
			| '0'..'9' when i > 0 -> ()
			| _ -> raise Exit
		done;
		true
	with Exit ->
		false

let field s = if Hashtbl.mem kwds s || not (valid_js_ident s) then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "$" ^ s else s
let check_var_declaration v = if Hashtbl.mem kwds2 v.v_name then v.v_name <- "$" ^ v.v_name

let anon_field s = if Hashtbl.mem kwds s || not (valid_js_ident s) then "'" ^ s ^ "'" else s
let static_field c s =
	match s with
	| "length" | "name" when not c.cl_extern || Meta.has Meta.HxGen c.cl_meta-> ".$" ^ s
	| s -> field s

let has_feature ctx = Common.has_feature ctx.com
let add_feature ctx = Common.add_feature ctx.com

let unsupported p = abort "This expression cannot be compiled to Javascript" p


let add_mapping smap force e =
	if e.epos.pmin < 0 then () else
	let pos = e.epos in
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
	if force || smap.source_last_file != file || smap.source_last_line != line || smap.source_last_col != col then begin
		smap.current_expr <- Some e;
		if smap.print_comma then
			Rbuffer.add_char smap.mappings ','
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
				Rbuffer.add_char smap.mappings (encode_digit (
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

let handle_newlines ctx str =
	Option.may (fun smap ->
		let rec loop from =
			try begin
				let next = String.index_from str from '\n' + 1 in
				Rbuffer.add_char smap.mappings ';';
				smap.output_last_col <- 0;
				smap.output_current_col <- 0;
				smap.print_comma <- false;
				Option.may (fun e -> add_mapping smap true e) smap.current_expr;
				loop next
			end with Not_found ->
				smap.output_current_col <- smap.output_current_col + (String.length str - from);
		in
		loop 0
	) ctx.smap

let flush ctx =
	Rbuffer.output_buffer ctx.chan ctx.buf;
	Rbuffer.clear ctx.buf

let spr ctx s =
	ctx.separator <- false;
	handle_newlines ctx s;
	Rbuffer.add_string ctx.buf s

let print ctx =
	ctx.separator <- false;
	Printf.kprintf (fun s -> begin
		handle_newlines ctx s;
		Rbuffer.add_string ctx.buf s
	end)

let write_mappings ctx smap =
	let basefile = Filename.basename ctx.com.file in
	print ctx "\n//# sourceMappingURL=%s.map" basefile;
	let channel = open_out_bin (ctx.com.file ^ ".map") in
	let sources = DynArray.to_list smap.sources in
	let to_url file =
		ExtString.String.map (fun c -> if c == '\\' then '/' else c) (Path.get_full_path file)
	in
	output_string channel "{\n";
	output_string channel "\"version\":3,\n";
	output_string channel ("\"file\":\"" ^ (String.concat "\\\\" (ExtString.String.nsplit basefile "\\")) ^ "\",\n");
	output_string channel ("\"sourceRoot\":\"file:///\",\n");
	output_string channel ("\"sources\":[" ^
		(String.concat "," (List.map (fun s -> "\"" ^ to_url s ^ "\"") sources)) ^
		"],\n");
	if Common.defined ctx.com Define.SourceMapContent then begin
		output_string channel ("\"sourcesContent\":[" ^
			(String.concat "," (List.map (fun s -> try "\"" ^ Ast.s_escape (Std.input_file ~bin:true s) ^ "\"" with _ -> "null") sources)) ^
			"],\n");
	end;
	output_string channel "\"names\":[],\n";
	output_string channel "\"mappings\":\"";
	Rbuffer.output_buffer channel smap.mappings;
	output_string channel "\"\n";
	output_string channel "}";
	close_out channel

let newline ctx =
	match Rbuffer.nth ctx.buf (Rbuffer.length ctx.buf - 1) with
	| '}' | '{' | ':' | ';' when not ctx.separator -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx ";\n%s" ctx.tabs

let newprop ctx =
	match Rbuffer.nth ctx.buf (Rbuffer.length ctx.buf - 1) with
	| '{' -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx "\n%s," ctx.tabs

let semicolon ctx =
	match Rbuffer.nth ctx.buf (Rbuffer.length ctx.buf - 1) with
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
		| Some c -> Type.concat (Codegen.set_default ctx.com a c p) e
	) f.tf_expr f.tf_args in
	e

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let rec needs_switch_break e =
	match e.eexpr with
	| TBlock [] -> true
	| TBlock el -> needs_switch_break (List.hd (List.rev el))
	| TMeta ((Meta.LoopLabel,_,_), {eexpr = TBreak})
	| TContinue | TReturn _ | TThrow _ -> false
	| _ -> true

let this ctx = match ctx.in_value with None -> "this" | Some _ -> "$this"

let is_dynamic_iterator ctx e =
	let check x =
		let rec loop t = match follow t with
			| TInst ({ cl_path = [],"Array" },_)
			| TInst ({ cl_kind = KTypeParameter _}, _)
			| TAnon _
			| TDynamic _
			| TMono _ ->
				true
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				loop (Abstract.get_underlying_type a tl)
			| _ -> false
		in
		has_feature ctx "HxOverrides.iter" && loop x.etype
	in
	match e.eexpr with
	| TField (x,f) when field_name f = "iterator" -> check x
	| _ ->
		false

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" (Ast.s_escape s)
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx (this ctx)
	| TSuper -> assert false

let rec gen_call ctx e el in_value =
	match e.eexpr , el with
	| TConst TSuper , params ->
		(match ctx.current.cl_super with
		| None -> abort "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			print ctx "%s.call(%s" (ctx.type_accessor (TClassDecl c)) (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TField ({ eexpr = TConst TSuper },f) , params ->
		(match ctx.current.cl_super with
		| None -> abort "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			let name = field_name f in
			print ctx "%s.prototype%s.call(%s" (ctx.type_accessor (TClassDecl c)) (field name) (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TCall (x,_) , el when (match x.eexpr with TIdent "__js__" -> false | _ -> true) ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TIdent "__new__", { eexpr = TConst (TString cl) } :: params ->
		print ctx "new %s(" cl;
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TIdent "__new__", e :: params ->
		spr ctx "new ";
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TIdent "__js__", [{ eexpr = TConst (TString "this") }] ->
		spr ctx (this ctx)
	| TIdent "__js__", [{ eexpr = TConst (TString code) }] ->
		spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
	| TIdent "__js__", { eexpr = TConst (TString code); epos = p } :: tl ->
		Codegen.interpolate_code ctx.com code tl (spr ctx) (gen_expr ctx) p
	| TIdent "__instanceof__",  [o;t] ->
		spr ctx "(";
		gen_value ctx o;
		print ctx " instanceof ";
		gen_value ctx t;
		spr ctx ")";
	| TIdent "__typeof__",  [o] ->
		spr ctx "typeof(";
		gen_value ctx o;
		spr ctx ")";
	| TIdent "__strict_eq__" , [x;y] ->
		(* add extra parenthesis here because of operator precedence *)
		spr ctx "((";
		gen_value ctx x;
		spr ctx ") === ";
		gen_value ctx y;
		spr ctx ")";
	| TIdent "__strict_neq__" , [x;y] ->
		(* add extra parenthesis here because of operator precedence *)
		spr ctx "((";
		gen_value ctx x;
		spr ctx ") !== ";
		gen_value ctx y;
		spr ctx ")";
	| TIdent "__define_feature__", [_;e] ->
		gen_expr ctx e
	| TIdent "__feature__", { eexpr = TConst (TString f) } :: eif :: eelse ->
		(if has_feature ctx f then
			gen_value ctx eif
		else match eelse with
			| [] -> ()
			| e :: _ -> gen_value ctx e)
	| TIdent "__rethrow__", [] ->
		spr ctx "throw $hx_rethrow";
	| TIdent "__resources__", [] ->
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
	| TIdent "`trace", [e;infos] ->
		if has_feature ctx "haxe.Log.trace" then begin
			let t = (try List.find (fun t -> t_path t = (["haxe"],"Log")) ctx.com.types with _ -> assert false) in
			spr ctx (ctx.type_accessor t);
			spr ctx ".trace(";
			gen_value ctx e;
			spr ctx ",";
			gen_value ctx infos;
			spr ctx ")";
		end else begin
			spr ctx "console.log(";
			(match infos.eexpr with
			| TObjectDecl (
				(("fileName",_,_) , { eexpr = (TConst (TString file)) }) ::
				(("lineNumber",_,_) , { eexpr = (TConst (TInt line)) }) :: _) ->
					print ctx "\"%s:%i:\"," file (Int32.to_int line)
			| _ ->
				());
			gen_value ctx e;
			spr ctx ")";
		end
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

(*
	this wraps {} in parenthesis which is required to produce valid js code for field and array access to it.
	the case itself is very rare and most probably comes from redundant code fused by the analyzer,
	but we still have to support it.
*)
and add_objectdecl_parens e =
	let rec loop e = match e.eexpr with
		| TCast(e1,None) | TMeta(_,e1) -> loop e1 (* TODO: do we really want to lose these? *)
		| TObjectDecl _ -> {e with eexpr = TParenthesis e}
		| _ -> e
	in
	loop e

and gen_expr ctx e =
	Option.may (fun smap -> add_mapping smap false e) ctx.smap;
	(match e.eexpr with
	| TConst c -> gen_constant ctx e.epos c
	| TLocal v -> spr ctx (ident v.v_name)
	(*| TArray (e1,{ eexpr = TConst (TString s) }) when valid_js_ident s && (match e1.eexpr with TConst (TInt _|TFloat _) -> false | _ -> true) ->
		gen_value ctx (add_objectdecl_parens e1);
		spr ctx (field s)*)
	| TArray (e1,e2) ->
		gen_value ctx (add_objectdecl_parens e1);
		spr ctx "[";
		gen_value ctx e2;
		spr ctx "]";
	| TBinop (op,{ eexpr = TField (x,f) },e2) when field_name f = "iterator" ->
		gen_value ctx x;
		spr ctx (field "iterator");
		print ctx " %s " (Ast.s_binop op);
		gen_value ctx e2;
	| TBinop (op,e1,e2) ->
		gen_value ctx e1;
		print ctx " %s " (Ast.s_binop op);
		gen_value ctx e2;
	| TField (x,f) when field_name f = "iterator" && is_dynamic_iterator ctx e ->
		add_feature ctx "use.$iterator";
		print ctx "$iterator(";
		gen_value ctx x;
		print ctx ")";
	| TField (x,FClosure (Some ({cl_path=[],"Array"},_), {cf_name="push"})) ->
		(* see https://github.com/HaxeFoundation/haxe/issues/1997 *)
		add_feature ctx "use.$arrayPush";
		add_feature ctx "use.$bind";
		print ctx "$bind(";
		gen_value ctx x;
		print ctx ",$arrayPush)"
	| TField (x,FClosure (_,f)) ->
		add_feature ctx "use.$bind";
		(match x.eexpr with
		| TConst _ | TLocal _ ->
			print ctx "$bind(";
			gen_value ctx x;
			print ctx ",";
			gen_value ctx x;
			print ctx "%s)" (if Meta.has Meta.SelfCall f.cf_meta then "" else (field f.cf_name))
		| _ ->
			print ctx "($_=";
			gen_value ctx x;
			print ctx ",$bind($_,$_%s))" (if Meta.has Meta.SelfCall f.cf_meta then "" else (field f.cf_name)))
	| TEnumIndex x ->
		gen_value ctx x;
		if Common.defined ctx.com Define.JsEnumsAsObjects then
		print ctx "._hx_index"
		else
		print ctx "[1]"
	| TEnumParameter (x,f,i) ->
		gen_value ctx x;
		if Common.defined ctx.com Define.JsEnumsAsObjects then
			let fname = (match f.ef_type with TFun((args,_)) -> let fname,_,_ = List.nth args i in  fname | _ -> assert false ) in
			print ctx ".%s" (fname)
		else
			print ctx "[%i]" (i + 2)
	| TField (_, FStatic ({cl_path = [],""},f)) ->
		spr ctx f.cf_name;
	| TField (x, (FInstance(_,_,f) | FStatic(_,f) | FAnon(f))) when Meta.has Meta.SelfCall f.cf_meta ->
		gen_value ctx x;
	| TField (x,f) ->
		let rec skip e = match e.eexpr with
			| TCast(e1,None) | TMeta(_,e1) -> skip e1
			| TConst(TInt _ | TFloat _) | TObjectDecl _ -> {e with eexpr = TParenthesis e}
			| _ -> e
		in
		let x = skip x in
		gen_value ctx x;
		let name = field_name f in
		spr ctx (match f with FStatic(c,_) -> static_field c name | FEnum _ | FInstance _ | FAnon _ | FDynamic _ | FClosure _ -> field name)
	| TTypeExpr t ->
		spr ctx (ctx.type_accessor t)
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| TMeta ((Meta.LoopLabel,[(EConst(Int n),_)],_), e) ->
		(match e.eexpr with
		| TWhile _ | TFor _ ->
			print ctx "_hx_loop%s: " n;
			gen_expr ctx e
		| TBreak ->
			print ctx "break _hx_loop%s" n;
		| _ -> assert false)
	| TMeta (_,e) ->
		gen_expr ctx e
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
		spr ctx "break"
	| TContinue ->
		if not ctx.in_loop then unsupported e.epos;
		spr ctx "continue"
	| TBlock el ->
		print ctx "{";
		let bend = open_block ctx in
		List.iter (gen_block_element ctx) el;
		bend();
		newline ctx;
		print ctx "}";
	| TFunction f ->
		let old = ctx.in_value, ctx.in_loop in
		ctx.in_value <- None;
		ctx.in_loop <- false;
		let args = List.map (fun (v,_) ->
			check_var_declaration v;
			ident v.v_name
		) f.tf_args in
		print ctx "function(%s) " (String.concat "," args);
		gen_expr ctx (fun_block ctx f e.epos);
		ctx.in_value <- fst old;
		ctx.in_loop <- snd old;
		ctx.separator <- true
	| TCall (e,el) ->
		gen_call ctx e el false
	| TArrayDecl el ->
		spr ctx "[";
		concat ctx "," (gen_value ctx) el;
		spr ctx "]"
	| TThrow e ->
		spr ctx "throw ";
		gen_value ctx e;
	| TVar (v,eo) ->
		spr ctx "var ";
		check_var_declaration v;
		spr ctx (ident v.v_name);
		begin match eo with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				gen_value ctx e
		end
	| TNew ({ cl_path = [],"Array" },_,[]) ->
		print ctx "[]"
	| TNew (c,_,el) ->
		(match c.cl_constructor with
		| Some cf when Meta.has Meta.SelfCall cf.cf_meta -> ()
		| _ -> print ctx "new ");
		print ctx "%s(" (ctx.type_accessor (TClassDecl c));
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_value ctx cond;
		spr ctx " ";
		gen_expr ctx (mk_block e);
		(match eelse with
		| None -> ()
		| Some e2 ->
			(match e.eexpr with
			| TObjectDecl _ -> ctx.separator <- false
			| _ -> ());
			semicolon ctx;
			spr ctx " else ";
			gen_expr ctx (match e2.eexpr with | TIf _ -> e2 | _ -> mk_block e2));
	| TUnop (op,Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_value ctx e
	| TUnop (op,Ast.Postfix,e) ->
		gen_value ctx e;
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		let old_in_loop = ctx.in_loop in
		ctx.in_loop <- true;
		spr ctx "while";
		gen_value ctx cond;
		spr ctx " ";
		gen_expr ctx e;
		ctx.in_loop <- old_in_loop
	| TWhile (cond,e,Ast.DoWhile) ->
		let old_in_loop = ctx.in_loop in
		ctx.in_loop <- true;
		spr ctx "do ";
		gen_expr ctx e;
		semicolon ctx;
		spr ctx " while";
		gen_value ctx cond;
		ctx.in_loop <- old_in_loop
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun ((f,_,qs),e) -> (match qs with
			| DoubleQuotes -> print ctx "\"%s\" : " (Ast.s_escape f);
			| NoQuotes -> print ctx "%s : " (anon_field f));
			gen_value ctx e
		) fields;
		spr ctx "}";
		ctx.separator <- true
	| TFor (v,it,e) ->
		check_var_declaration v;
		let old_in_loop = ctx.in_loop in
		ctx.in_loop <- true;
		let it = ident (match it.eexpr with
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
		gen_block_element ctx e;
		bend();
		newline ctx;
		spr ctx "}";
		ctx.in_loop <- old_in_loop
	| TTry (e,catchs) ->
		spr ctx "try ";
		gen_expr ctx e;
		let vname = (match catchs with [(v,_)] -> check_var_declaration v; v.v_name | _ ->
			let id = ctx.id_counter in
			ctx.id_counter <- ctx.id_counter + 1;
			"$e" ^ string_of_int id
		) in
		print ctx " catch( %s ) {" vname;
		let bend = open_block ctx in
		let last = ref false in
		let else_block = ref false in

		if ctx.store_exception_stack then begin
			newline ctx;
			print ctx "%s.lastException = %s" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["haxe"],"CallStack" })) vname
		end;

		if (has_feature ctx "js.Lib.rethrow") then begin
			let has_rethrow (_,e) =
				let rec loop e = match e.eexpr with
				| TCall({eexpr = TIdent "__rethrow__"}, []) -> raise Exit
				| _ -> Type.iter loop e
				in
				try (loop e; false) with Exit -> true
			in
			if List.exists has_rethrow catchs then begin
				newline ctx;
				print ctx "var $hx_rethrow = %s" vname;
			end
		end;

		if (has_feature ctx "js.Boot.HaxeError") then begin
			let catch_var_used =
				try
					List.iter (fun (v,e) ->
						match follow v.v_type with
						| TDynamic _ -> (* Dynamic catch - unrap if the catch value is used *)
							let rec loop e = match e.eexpr with
							| TLocal v2 when v2 == v -> raise Exit
							| _ -> Type.iter loop e
							in
							loop e
						| _ -> (* not a Dynamic catch - we need to unwrap the error for type-checking *)
							raise Exit
					) catchs;
					false
				with Exit ->
					true
			in
			if catch_var_used then begin
				newline ctx;
				print ctx "if (%s instanceof %s) %s = %s.val" vname (ctx.type_accessor (TClassDecl { null_class with cl_path = ["js";"_Boot"],"HaxeError" })) vname vname;
			end;
		end;

		List.iter (fun (v,e) ->
			if !last then () else
			let t = (match follow v.v_type with
			| TEnum (e,_) -> Some (TEnumDecl e)
			| TInst (c,_) -> Some (TClassDecl c)
			| TAbstract (a,_) -> Some (TAbstractDecl a)
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
				gen_block_element ctx e;
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
				gen_block_element ctx e;
				bend();
				newline ctx;
				spr ctx "} else ";
				else_block := true
		) catchs;
		if not !last then print ctx "throw(%s)" vname;
		bend();
		newline ctx;
		spr ctx "}";
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
			gen_block_element ctx e2;
			if needs_switch_break e2 then begin
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
			gen_block_element ctx e;
			bend();
			newline ctx;
		);
		spr ctx "}"
	| TCast (e,None) ->
		gen_expr ctx e
	| TCast (e1,Some t) ->
		print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["js"],"Boot" }));
		gen_expr ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"
	| TIdent s ->
		spr ctx s
	);
	Option.may (fun smap -> smap.current_expr <- None) ctx.smap


and gen_block_element ?(after=false) ctx e =
	match e.eexpr with
	| TBlock el ->
		List.iter (gen_block_element ~after ctx) el
	| TCall ({ eexpr = TIdent "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse) ->
		if has_feature ctx f then
			gen_block_element ~after ctx eif
		else (match eelse with
			| [] -> ()
			| [e] -> gen_block_element ~after ctx e
			| _ -> assert false)
	| TFunction _ ->
		gen_block_element ~after ctx (mk (TParenthesis e) e.etype e.epos)
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> gen_block_element ~after ctx e) fl
	| _ ->
		if not after then newline ctx;
		gen_expr ctx e;
		if after then newline ctx

and gen_value ctx e =
	Option.may (fun smap -> add_mapping smap false e) ctx.smap;
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> assert false | Some v -> v)) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let value() =
		let old = ctx.in_value, ctx.in_loop in
		let r = alloc_var "$r" t_dynamic e.epos in
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
	(match e.eexpr with
	| TConst _
	| TLocal _
	| TArray _
	| TBinop _
	| TField _
	| TEnumParameter _
	| TEnumIndex _
	| TTypeExpr _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TNew _
	| TUnop _
	| TFunction _
	| TIdent _ ->
		gen_expr ctx e
	| TMeta (_,e1) ->
		gen_value ctx e1
	| TCall (e,el) ->
		gen_call ctx e el true
	| TReturn _
	| TBreak
	| TContinue ->
		unsupported e.epos
	| TCast (e1, None) ->
		gen_value ctx e1
	| TCast (e1, Some t) ->
		print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["js"],"Boot" }));
		gen_value ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"
	| TVar _
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
			| TParenthesis { eexpr = TBinop ((Ast.OpAssign | Ast.OpAssignOp _),_,_) | TIf _ } -> cond
			| TParenthesis e -> e
			| _ -> cond
		) in
		gen_value ctx cond;
		spr ctx " ? ";
		gen_value ctx e;
		spr ctx " : ";
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
	| TTry (b,catchs) ->
		let v = value() in
		let block e = mk (TBlock [e]) e.etype e.epos in
		gen_expr ctx (mk (TTry (block (assign b),
			List.map (fun (v,e) -> v, block (assign e)) catchs
		)) e.etype e.epos);
		v());
	Option.may (fun smap -> smap.current_expr <- None) ctx.smap


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
			ctx.separator <- true;
			newline ctx;
			loop (p :: acc) l
	in
	match p with
	| [] -> print ctx "var "
	| _ -> loop [] p

let check_field_name c f =
	match f.cf_name with
	| "prototype" | "__proto__" | "constructor" ->
		abort ("The field name '" ^ f.cf_name ^ "'  is not allowed in JS") (match f.cf_expr with None -> c.cl_pos | Some e -> e.epos);
	| _ -> ()

(* convert a.b.c to ["a"]["b"]["c"] *)
let path_to_brackets path =
	let parts = ExtString.String.nsplit path "." in
	"[\"" ^ (String.concat "\"][\"" parts) ^ "\"]"

let gen_class_static_field ctx c f =
	match f.cf_expr with
	| None | Some { eexpr = TConst TNull } when not (has_feature ctx "Type.getClassFields") ->
		()
	| None when not (is_physical_field f) ->
		()
	| None ->
		print ctx "%s%s = null" (s_path ctx c.cl_path) (static_field c f.cf_name);
		newline ctx
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			let path = (s_path ctx c.cl_path) ^ (static_field c f.cf_name) in
			let dot_path = (dot_path c.cl_path) ^ (static_field c f.cf_name) in
			ctx.id_counter <- 0;
			print ctx "%s = " path;
			(match (get_exposed ctx dot_path f.cf_meta) with [s] -> print ctx "$hx_exports%s = " (path_to_brackets s) | _ -> ());
			gen_value ctx e;
			newline ctx;
		| _ ->
			ctx.statics <- (c,f.cf_name,e) :: ctx.statics

let can_gen_class_field ctx = function
	| { cf_expr = (None | Some { eexpr = TConst TNull }) } when not (has_feature ctx "Type.getInstanceFields") ->
		false
	| f ->
		is_physical_field f

let gen_class_field ctx c f =
	check_field_name c f;
	match f.cf_expr with
	| None ->
		newprop ctx;
		print ctx "%s: " (anon_field f.cf_name);
		print ctx "null";
	| Some e ->
		newprop ctx;
		print ctx "%s: " (anon_field f.cf_name);
		ctx.id_counter <- 0;
		gen_value ctx e;
		ctx.separator <- false

let generate_class___name__ ctx c =
	if has_feature ctx "js.Boot.isClass" then begin
		let p = s_path ctx c.cl_path in
		print ctx "%s.__name__ = " p;
		if has_feature ctx "Type.getClassName" then
			print ctx "[%s]" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst c.cl_path @ [snd c.cl_path])))
		else
			print ctx "true";
		newline ctx;
	end

let generate_class ctx c =
	ctx.current <- c;
	ctx.id_counter <- 0;
	(match c.cl_path with
	| [],"Function" -> abort "This class redefine a native one" c.cl_pos
	| _ -> ());
	let p = s_path ctx c.cl_path in
	let hxClasses = has_feature ctx "Type.resolveClass" in
	if ctx.js_flatten then
		print ctx "var "
	else
		generate_package_create ctx c.cl_path;
	if ctx.js_modern || not hxClasses then
		print ctx "%s = " p
	else
		print ctx "%s = $hxClasses[\"%s\"] = " p (dot_path c.cl_path);
	(match (get_exposed ctx (dot_path c.cl_path) c.cl_meta) with [s] -> print ctx "$hx_exports%s = " (path_to_brackets s) | _ -> ());
	(match c.cl_kind with
		| KAbstractImpl _ ->
			(* abstract implementations only contain static members and don't need to have constructor functions *)
			print ctx "{}"; ctx.separator <- true
		| _ ->
			(match c.cl_constructor with
			| Some { cf_expr = Some e } -> gen_expr ctx e
			| _ -> (print ctx "function() { }"); ctx.separator <- true)
	);
	newline ctx;
	if ctx.js_modern && hxClasses then begin
		print ctx "$hxClasses[\"%s\"] = %s" (dot_path c.cl_path) p;
		newline ctx;
	end;
	generate_class___name__ ctx c;
	(match c.cl_implements with
	| [] -> ()
	| l ->
		print ctx "%s.__interfaces__ = [%s]" p (String.concat "," (List.map (fun (i,_) -> ctx.type_accessor (TClassDecl i)) l));
		newline ctx;
	);

	let gen_props props =
		String.concat "," (List.map (fun (p,v) -> p ^":\""^v^"\"") props) in
	let has_property_reflection =
		(has_feature ctx "Reflect.getProperty") || (has_feature ctx "Reflect.setProperty") in

	if has_property_reflection then begin
		(match Codegen.get_properties c.cl_ordered_statics with
		| [] -> ()
		| props ->
			print ctx "%s.__properties__ = {%s}" p (gen_props props);
			ctx.separator <- true;
			newline ctx);
	end;

	List.iter (gen_class_static_field ctx c) c.cl_ordered_statics;

	let has_class = has_feature ctx "js.Boot.getClass" && (c.cl_super <> None || c.cl_ordered_fields <> [] || c.cl_constructor <> None) in
	let has_prototype = c.cl_super <> None || has_class || List.exists (can_gen_class_field ctx) c.cl_ordered_fields in
	if has_prototype then begin
		(match c.cl_super with
		| None -> print ctx "%s.prototype = {" p;
		| Some (csup,_) ->
			let psup = ctx.type_accessor (TClassDecl csup) in
			print ctx "%s.__super__ = %s" p psup;
			newline ctx;
			print ctx "%s.prototype = $extend(%s.prototype,{" p psup;
		);

		let bend = open_block ctx in
		List.iter (fun f -> if can_gen_class_field ctx f then gen_class_field ctx c f) c.cl_ordered_fields;
		if has_class then begin
			newprop ctx;
			print ctx "__class__: %s" p;
		end;

		if has_property_reflection then begin
			let props = Codegen.get_properties c.cl_ordered_fields in
			(match c.cl_super with
			| _ when props = [] -> ()
			| Some (csup,_) when Codegen.has_properties csup ->
				newprop ctx;
				let psup = ctx.type_accessor (TClassDecl csup) in
				print ctx "__properties__: $extend(%s.prototype.__properties__,{%s})" psup (gen_props props)
			| _ ->
				newprop ctx;
				print ctx "__properties__: {%s}" (gen_props props));
		end;

		bend();
		print ctx "\n}";
		(match c.cl_super with None -> ctx.separator <- true | _ -> print ctx ")");
		newline ctx
	end;
	flush ctx

let generate_enum ctx e =
	let p = s_path ctx e.e_path in
	let ename = List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst e.e_path @ [snd e.e_path]) in
	if ctx.js_flatten then
		print ctx "var "
	else
		generate_package_create ctx e.e_path;
	print ctx "%s = " p;
	if has_feature ctx "Type.resolveEnum" then print ctx "$hxClasses[\"%s\"] = " (dot_path e.e_path);
	print ctx "{";
	if has_feature ctx "js.Boot.isEnum" then print ctx " __ename__ : %s," (if has_feature ctx "Type.getEnumName" then "[" ^ String.concat "," ename ^ "]" else "true");
	print ctx " __constructs__ : [%s] }" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" s) e.e_names));
	ctx.separator <- true;
	let as_objects = Common.defined ctx.com Define.JsEnumsAsObjects in
	if as_objects then begin
		newline ctx;
		print ctx "$hxEnums[\"%s\"] = %s" p p
	end;
	newline ctx;
	List.iter (fun n ->
		let f = PMap.find n e.e_constrs in
		print ctx "%s%s = " p (field f.ef_name);
		(match f.ef_type with
		| TFun (args,_) ->
			let sargs = String.concat "," (List.map (fun (n,_,_) -> ident n) args) in begin
			if as_objects then
				let sfields = String.concat "," (List.map (fun (n,_,_) -> (ident n) ^ ":" ^ (ident n) ) args) in
				print ctx "function(%s) { var $x = {_hx_index:%d,%s,__enum__:\"%s\"};" sargs f.ef_index sfields p;
			else
				print ctx "function(%s) { var $x = [\"%s\",%d,%s]; $x.__enum__ = %s;" sargs f.ef_name f.ef_index sargs p;
			end;
			if has_feature ctx "has_enum" then
				spr ctx " $x.toString = $estr;";
			spr ctx " return $x; }";
			if as_objects then begin
				let sparams = String.concat "," (List.map (fun (n,_,_) -> "\"" ^ (ident n) ^ "\"" ) args) in
				newline ctx;
				print ctx "%s%s.__params__ = [%s];" p (field f.ef_name) sparams
			end;
			ctx.separator <- true;
		| _ ->
			if as_objects then
				print ctx "{_hx_index:%d};" f.ef_index
			else
				print ctx "[\"%s\",%d]" f.ef_name f.ef_index;
			newline ctx;
			if has_feature ctx "has_enum" then begin
				print ctx "%s%s.toString = $estr" p (field f.ef_name);
				newline ctx;
			end;
			print ctx "%s%s.__enum__ = %s" p (field f.ef_name) (if as_objects then "\"" ^ p ^"\"" else p);
		);
		newline ctx
	) e.e_names;
	if has_feature ctx "Type.allEnums" then begin
		let ctors_without_args = List.filter (fun s ->
			let ef = PMap.find s e.e_constrs in
			match follow ef.ef_type with
				| TFun _ -> false
				| _ -> true
		) e.e_names in
		print ctx "%s.__empty_constructs__ = [%s]" p (String.concat "," (List.map (fun s -> Printf.sprintf "%s.%s" p s) ctors_without_args));
		newline ctx
	end;
	begin match Codegen.build_metadata ctx.com (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "%s.__meta__ = " p;
		gen_expr ctx e;
		newline ctx
	end;
	flush ctx

let generate_static ctx (c,f,e) =
	print ctx "%s%s = " (s_path ctx c.cl_path) (static_field c f);
	gen_value ctx e;
	newline ctx

let generate_require ctx path meta =
	let _, args, mp = Meta.get Meta.JsRequire meta in
	let p = (s_path ctx path) in

	if ctx.js_flatten then
		spr ctx "var "
	else
		generate_package_create ctx path;

	(match args with
	| [(EConst(String(module_name,_)),_)] ->
		print ctx "%s = require(\"%s\")" p module_name
	| [(EConst(String(module_name,_)),_) ; (EConst(String(object_path,_)),_)] ->
		print ctx "%s = require(\"%s\").%s" p module_name object_path
	| _ ->
		abort "Unsupported @:jsRequire format" mp);

	newline ctx

let generate_type ctx = function
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e ->
			ctx.inits <- e :: ctx.inits);
		(* Special case, want to add Math.__name__ only when required, handle here since Math is extern *)
		let p = s_path ctx c.cl_path in
		if p = "Math" then generate_class___name__ ctx c;
		(* Another special case for Std because we do not want to generate it if it's empty. *)
		if p = "Std" && c.cl_ordered_statics = [] then
			()
		else if not c.cl_extern then
			generate_class ctx c
		else if Meta.has Meta.JsRequire c.cl_meta && is_directly_used ctx.com c.cl_meta then
			generate_require ctx c.cl_path c.cl_meta
		else if not ctx.js_flatten && Meta.has Meta.InitPackage c.cl_meta then
			(match c.cl_path with
			| ([],_) -> ()
			| _ -> generate_package_create ctx c.cl_path)
	| TEnumDecl e when e.e_extern ->
		if Meta.has Meta.JsRequire e.e_meta && is_directly_used ctx.com e.e_meta then
			generate_require ctx e.e_path e.e_meta
	| TEnumDecl e -> generate_enum ctx e
	| TTypeDecl _ | TAbstractDecl _ -> ()

let set_current_class ctx c =
	ctx.current <- c

let alloc_ctx com =
	let smap =
		if com.debug || Common.defined com Define.JsSourceMap then
			Some {
				source_last_line = 0;
				source_last_col = 0;
				source_last_file = 0;
				print_comma = false;
				output_last_col = 0;
				output_current_col = 0;
				sources = DynArray.create();
				sources_hash = Hashtbl.create 0;
				mappings = Rbuffer.create 16;
				current_expr = None;
			}
		else
			None
	in
	let ctx = {
		com = com;
		buf = Rbuffer.create 16000;
		chan = open_out_bin com.file;
		packages = Hashtbl.create 0;
		smap = smap;
		js_modern = not (Common.defined com Define.JsClassic);
		js_flatten = not (Common.defined com Define.JsUnflatten);
		es_version = (try int_of_string (Common.defined_value com Define.JsEs) with _ -> 0);
		store_exception_stack = if Common.has_dce com then (Common.has_feature com "haxe.CallStack.exceptionStack") else List.exists (function TClassDecl { cl_path=["haxe"],"CallStack" } -> true | _ -> false) com.types;
		statics = [];
		inits = [];
		current = null_class;
		tabs = "";
		in_value = None;
		in_loop = false;
		id_counter = 0;
		type_accessor = (fun _ -> assert false);
		separator = false;
		found_expose = false;
	} in
	ctx.type_accessor <- (fun t ->
		let p = t_path t in
		match t with
		| TClassDecl ({ cl_extern = true } as c) when not (Meta.has Meta.JsRequire c.cl_meta)
			-> dot_path p
		| TEnumDecl ({ e_extern = true } as e) when not (Meta.has Meta.JsRequire e.e_meta)
			-> dot_path p
		| _ -> s_path ctx p);
	ctx

let gen_single_expr ctx e expr =
	if expr then gen_expr ctx e else gen_value ctx e;
	let str = Rbuffer.unsafe_contents ctx.buf in
	Rbuffer.reset ctx.buf;
	ctx.id_counter <- 0;
	str

let generate com =
	(match com.js_gen with
	| Some g -> g()
	| None ->
	let ctx = alloc_ctx com in
	Codegen.map_source_header com (fun s -> print ctx "// %s\n" s);
	if has_feature ctx "Class" || has_feature ctx "Type.getClassName" then add_feature ctx "js.Boot.isClass";
	if has_feature ctx "Enum" || has_feature ctx "Type.getEnumName" then add_feature ctx "js.Boot.isEnum";

	let nodejs = Common.raw_defined com "nodejs" in

	setup_kwds (if ctx.es_version >= 5 then es5kwds else es3kwds);

	let exposed = List.concat (List.map (fun t ->
		match t with
			| TClassDecl c ->
				let path = dot_path c.cl_path in
				let class_exposed = get_exposed ctx path c.cl_meta in
				let static_exposed = List.map (fun f ->
					get_exposed ctx (path ^ static_field c f.cf_name) f.cf_meta
				) c.cl_ordered_statics in
				List.concat (class_exposed :: static_exposed)
			| _ -> []
		) com.types) in
	let anyExposed = exposed <> [] in
	let exportMap = ref (PMap.create String.compare) in
	let exposedObject = { os_name = ""; os_fields = [] } in
	let toplevelExposed = ref [] in
	List.iter (fun path -> (
		let parts = ExtString.String.nsplit path "." in
		let rec loop p pre = match p with
			| f :: g :: ls ->
				let path = match pre with "" -> f | pre -> (pre ^ "." ^ f) in
				if not (PMap.exists path !exportMap) then (
					let elts = { os_name = f; os_fields = [] } in
					exportMap := PMap.add path elts !exportMap;
					let cobject = match pre with "" -> exposedObject | pre -> PMap.find pre !exportMap in
					cobject.os_fields <- elts :: cobject.os_fields
				);
				loop (g :: ls) path;
			| f :: [] when pre = "" ->
				toplevelExposed := f :: !toplevelExposed;
			| _ -> ()
		in loop parts "";
	)) exposed;

	let include_files = List.rev com.include_files in

	List.iter (fun file ->
		match file with
		| path, "top" ->
			let file_content = Std.input_file ~bin:true (fst file) in
			print ctx "%s\n" file_content;
			()
		| _ -> ()
	) include_files;

	let var_console = (
		"console",
		"typeof console != \"undefined\" ? console : {log:function(){}}"
	) in

	let var_exports = (
		"$hx_exports",
		"typeof exports != \"undefined\" ? exports : typeof window != \"undefined\" ? window : typeof self != \"undefined\" ? self : this"
	) in

	let var_global = (
		"$global",
		"typeof window != \"undefined\" ? window : typeof global != \"undefined\" ? global : typeof self != \"undefined\" ? self : this"
	) in

	let closureArgs = [] in
	let closureArgs = if has_feature ctx "js.Lib.global" then
		var_global :: closureArgs
	else
		closureArgs
	in
	let closureArgs = if (anyExposed && not (Common.defined com Define.ShallowExpose)) then
		var_exports :: closureArgs
	else
		closureArgs
	in
	(* Provide console for environments that may not have it. *)
	let closureArgs = if ctx.es_version < 5 then
		var_console :: closureArgs
	else
		closureArgs
	in

	if nodejs then
		(* Add node globals to pseudo-keywords, so they are not shadowed by local vars *)
		List.iter (fun s -> Hashtbl.replace kwds2 s ()) [ "global"; "process"; "__filename"; "__dirname"; "module" ];

	if (anyExposed && ((Common.defined com Define.ShallowExpose) || not ctx.js_modern)) then (
		print ctx "var %s = %s" (fst var_exports) (snd var_exports);
		ctx.separator <- true;
		newline ctx
	);

	if ctx.js_modern then begin
		(* Wrap output in a closure *)
		print ctx "(function (%s) { \"use strict\"" (String.concat ", " (List.map fst closureArgs));
		newline ctx;
	end;

	let rec print_obj f root = (
		let path = root ^ (path_to_brackets f.os_name) in
		print ctx "%s = %s || {}" path path;
		ctx.separator <- true;
		newline ctx;
		concat ctx ";" (fun g -> print_obj g path) f.os_fields
	)
	in
	List.iter (fun f -> print_obj f "$hx_exports") exposedObject.os_fields;

	List.iter (fun file ->
		match file with
		| path, "closure" ->
			let file_content = Std.input_file ~bin:true (fst file) in
			print ctx "%s\n" file_content;
			()
		| _ -> ()
	) include_files;

	(* If ctx.js_modern, console is defined in closureArgs. *)
	if (not ctx.js_modern) && (ctx.es_version < 5) then
		add_feature ctx "js.Lib.global"; (* console polyfill will check console from $global *)

	if (not ctx.js_modern) && (has_feature ctx "js.Lib.global") then
		print ctx "var %s = %s;\n" (fst var_global) (snd var_global);

	if (not ctx.js_modern) && (ctx.es_version < 5) then
		spr ctx "var console = $global.console || {log:function(){}};\n";

	(* TODO: fix $estr *)
	let vars = [] in
	let vars = (if has_feature ctx "Type.resolveClass" || has_feature ctx "Type.resolveEnum" then ("$hxClasses = " ^ (if ctx.js_modern then "{}" else "$hxClasses || {}")) :: vars else vars) in
	let vars = if has_feature ctx "has_enum"
		then ("$estr = function() { return " ^ (ctx.type_accessor (TClassDecl { null_class with cl_path = ["js"],"Boot" })) ^ ".__string_rec(this,''); }") :: vars
		else vars in
	let vars = if Common.defined com Define.JsEnumsAsObjects then "$hxEnums = {}" :: vars else vars in
	(match List.rev vars with
	| [] -> ()
	| vl ->
		print ctx "var %s" (String.concat "," vl);
		ctx.separator <- true;
		newline ctx
	);
	if List.exists (function TClassDecl { cl_extern = false; cl_super = Some _ } -> true | _ -> false) com.types then begin
		print ctx "function $extend(from, fields) {
	function Inherit() {} Inherit.prototype = from; var proto = new Inherit();
	for (var name in fields) proto[name] = fields[name];
	if( fields.toString !== Object.prototype.toString ) proto.toString = fields.toString;
	return proto;
}
";
	end;
	List.iter (generate_type ctx) com.types;
	let rec chk_features e =
		if is_dynamic_iterator ctx e then add_feature ctx "use.$iterator";
		match e.eexpr with
		| TField (_,FClosure _) ->
			add_feature ctx "use.$bind"
		| _ ->
			Type.iter chk_features e
	in
	List.iter chk_features ctx.inits;
	List.iter (fun (_,_,e) -> chk_features e) ctx.statics;
	if has_feature ctx "use.$iterator" then begin
		add_feature ctx "use.$bind";
		print ctx "function $iterator(o) { if( o instanceof Array ) return function() { return HxOverrides.iter(o); }; return typeof(o.iterator) == 'function' ? $bind(o,o.iterator) : o.iterator; }";
		newline ctx;
	end;
	if has_feature ctx "use.$bind" then begin
		print ctx "var $_, $fid = 0";
		newline ctx;
		print ctx "function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $fid++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; }";
		newline ctx;
	end;
	if has_feature ctx "use.$arrayPush" then begin
		print ctx "function $arrayPush(x) { this.push(x); }";
		newline ctx
	end;
	List.iter (gen_block_element ~after:true ctx) (List.rev ctx.inits);
	List.iter (generate_static ctx) (List.rev ctx.statics);
	(match com.main with
	| None -> ()
	| Some e -> gen_expr ctx e; newline ctx);
	if ctx.js_modern then begin
		print ctx "})(%s)" (String.concat ", " (List.map snd closureArgs));
		newline ctx;
	end;

	if (anyExposed && (Common.defined com Define.ShallowExpose)) then (
		List.iter (fun f ->
			print ctx "var %s = $hx_exports%s" f.os_name (path_to_brackets f.os_name);
			ctx.separator <- true;
			newline ctx
		) exposedObject.os_fields;
		List.iter (fun f ->
			print ctx "var %s = $hx_exports%s" f (path_to_brackets f);
			ctx.separator <- true;
			newline ctx
		) !toplevelExposed
	);

	(match ctx.smap with
	| Some smap -> write_mappings ctx smap
	| None -> try Sys.remove (com.file ^ ".map") with _ -> ());
	flush ctx;
	close_out ctx.chan)

