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
open Extlib_leftovers
open Globals
open Ast
open Type
open Common

type sourcemap = {
	sources : (string) DynArray.t;
	sources_hash : (string, int) Hashtbl.t;
	mappings : Rbuffer.t;

	mutable source_last_pos : sourcemap_pos;
	mutable print_comma : bool;
	mutable output_last_col : int;
	mutable output_current_col : int;
	mutable current_expr : sourcemap_pos option;
}

and sourcemap_pos = {
	file : int;
	line : int;
	col : int;
}

type ctx = {
	com : Common.context;
	buf : Rbuffer.t;
	chan : out_channel;
	packages : (string list,unit) Hashtbl.t;
	smap : sourcemap option;
	js_modern : bool;
	js_flatten : bool;
	has_resolveClass : bool;
	has_interface_check : bool;
	es_version : int;
	mutable current : tclass;
	mutable statics : (tclass * tclass_field * texpr) list;
	mutable inits : texpr list;
	mutable tabs : string;
	mutable in_value : tvar option;
	mutable in_loop : bool;
	mutable id_counter : int;
	mutable type_accessor : module_type -> string;
	mutable separator : bool;
	mutable found_expose : bool;
	mutable catch_vars : texpr list;
}

type object_store = {
	os_name : string;
	mutable os_fields : object_store list;
}

let process_expose meta f_default f =
	try
		let (_, args, pos) = Meta.get Meta.Expose meta in
		match args with
		| [ EConst (String(s,_)), _ ] -> f s
		| [] -> f (f_default ())
		| _ -> abort "Invalid @:expose parameters" pos
	with Not_found ->
		()

let dot_path = s_type_path

let s_path ctx = if ctx.js_flatten then Path.flat_path else dot_path

let kwds = Hashtbl.create 0

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

let setup_kwds com =
	Hashtbl.reset kwds;
	let es_version = get_es_version com in
	let lst = if es_version >= 5 then es5kwds else es3kwds in
	List.iter (fun s -> Hashtbl.add kwds s ()) lst

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

let field s = if not (valid_js_ident s) then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "$" ^ s else s
let check_var_declaration v = if Hashtbl.mem kwds2 v.v_name then v.v_name <- "$" ^ v.v_name

let anon_field s = if Hashtbl.mem kwds s || not (valid_js_ident s) then "'" ^ s ^ "'" else s
let static_field ctx c f =
	let s = f.cf_name in
	match s with
	| "length" | "name" when not (has_class_flag c CExtern) || Meta.has Meta.HxGen c.cl_meta ->
		(match f.cf_kind with
		| Method _ when ctx.es_version >= 6 ->
			"." ^ s
		| _ ->
			".$" ^ s)
	| s ->
		field s

let module_field m f =
	try
		fst (TypeloadCheck.get_native_name f.cf_meta)
	with Not_found ->
		Path.flat_path m.m_path ^ "_" ^ f.cf_name

let module_field_expose_path mpath f =
	try
		fst (TypeloadCheck.get_native_name f.cf_meta)
	with Not_found ->
		(dot_path mpath) ^ "." ^ f.cf_name

let has_feature ctx = Common.has_feature ctx.com
let add_feature ctx = Common.add_feature ctx.com

let unsupported p = abort "This expression cannot be compiled to Javascript" p

let encode_mapping smap pos =
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
	base64_vlq (pos.file - smap.source_last_pos.file);
	base64_vlq (pos.line - smap.source_last_pos.line);
	base64_vlq (pos.col - smap.source_last_pos.col);

	smap.source_last_pos <- pos;
	smap.output_last_col <- smap.output_current_col

let noop () = ()

let add_mapping smap pos =
	if pos.pmin < 0 then noop else

	let file = try
		Hashtbl.find smap.sources_hash pos.pfile
	with Not_found ->
		let length = DynArray.length smap.sources in
		Hashtbl.replace smap.sources_hash pos.pfile length;
		DynArray.add smap.sources pos.pfile;
		length
	in

	let pos =
		let line, col = Lexer.find_pos pos in
		let line = line - 1 in
		{ file = file; line = line; col = col }
	in

	if smap.source_last_pos <> pos then begin
		let old_current_expr = smap.current_expr in
		smap.current_expr <- Some pos;
		encode_mapping smap pos;
		(fun () -> smap.current_expr <- old_current_expr)
	end else
		noop

let add_mapping ctx e =
	Option.map_default (fun smap -> add_mapping smap e.epos) noop ctx.smap

let handle_newlines ctx str =
	Option.may (fun smap ->
		let rec loop from =
			try begin
				let next = String.index_from str from '\n' + 1 in
				Rbuffer.add_char smap.mappings ';';
				smap.output_last_col <- 0;
				smap.output_current_col <- 0;
				smap.print_comma <- false;
				Option.may (encode_mapping smap) smap.current_expr;
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
	print ctx "\n//# sourceMappingURL=%s.map" (url_encode_s basefile);
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
			(String.concat "," (List.map (fun s -> try "\"" ^ StringHelper.s_escape (Std.input_file ~bin:true s) ^ "\"" with _ -> "null") sources)) ^
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

(**
	Produce expressions to declare arguments of a function with `Rest<T>` trailing argument.
	Used for ES5 and older standards, which don't support "rest parameters" syntax.
	`args` is a list of explicitly defined arguments.
	`rest_arg` is the argument of `Rest<T>` type.

	This implementation copies rest arguments into a new array in a loop.
	It's the only way to avoid disabling javascript VM optimizations of functions
	with rest arguments.
*)
let declare_rest_args_legacy com args rest_arg =
	let arguments = mk (TIdent "arguments") t_dynamic null_pos
	and index i = mk (TConst (TInt (Int32.of_int i))) com.basic.tint null_pos in
	let rec loop args i =
		match args with
		| [] -> []
		| (v,e_opt) :: rest ->
			(* var v = arguments[i]; *)
			let value = mk (TArray (arguments,index i)) v.v_type null_pos in
			let decl = mk (TVar (v,Some value)) com.basic.tvoid v.v_pos in

			match e_opt with
			| None -> decl :: loop rest (i + 1)
			| Some e -> decl :: Texpr.set_default com.basic v e v.v_pos :: loop rest (i + 1)
	in
	let i = string_of_int (List.length args) in
	let new_array = mk (TIdent ("new Array($l-"^ i ^")")) t_dynamic rest_arg.v_pos
	and populate = mk (TIdent ("for(var $i=" ^ i ^ ";$i<$l;++$i){" ^ (ident rest_arg.v_name) ^ "[$i-" ^ i ^ "]=arguments[$i];}")) com.basic.tvoid rest_arg.v_pos
	in
	loop args 0
	@ [
		mk (TIdent ("var $l=arguments.length")) com.basic.tvoid rest_arg.v_pos;
		mk (TVar (rest_arg,Some new_array)) com.basic.tvoid rest_arg.v_pos;
		populate
	]

let fun_block ctx f p =
	let e = List.fold_left (fun e (a,c) ->
		match c with
		| None | Some {eexpr = TConst TNull} -> e
		| Some c -> Type.concat (Texpr.set_default ctx.com.basic a c p) e
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

let is_iterator_field_access fa = match field_name fa with
	| "iterator" | "keyValueIterator" -> true
	| _ -> false

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
		has_feature ctx "haxe.iterators.ArrayIterator.*" && loop x.etype
	in
	match e.eexpr with
	| TField (x,f) when is_iterator_field_access f -> check x
	| _ ->
		false

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" (StringHelper.s_escape s)
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx (this ctx)
	| TSuper -> assert (ctx.es_version >= 6); spr ctx "super"

let print_deprecation_message = DeprecationCheck.warn_deprecation

let is_code_injection_function e =
	match e.eexpr with
	| TIdent "__js__"
	| TField (_, FStatic ({ cl_path = ["js"],"Syntax" }, { cf_name = "code" | "plainCode" }))
		-> true
	| _ ->
		false

let var ctx =
	if ctx.es_version >= 6 then "let" else "var"

let rec gen_call ctx e el in_value =
	let apply,el =
		if ctx.es_version < 6 then
			match List.rev el with
			| [{ eexpr = TUnop (Spread,Ast.Prefix,rest) }] ->
				true,[rest]
			| { eexpr = TUnop (Spread,Ast.Prefix,rest) } :: args_rev ->
				(* [arg1, arg2, ..., argN].concat(rest) *)
				let arr = mk (TArrayDecl (List.rev args_rev)) t_dynamic null_pos in
				let concat = mk (TField (arr, FDynamic "concat")) t_dynamic null_pos in
				true,[mk (TCall (concat, [rest])) t_dynamic null_pos]
			| _ ->
				false,el
		else
			false,el
	in
	match e.eexpr , el with
	| TConst TSuper , params when ctx.es_version < 6 ->
		(match ctx.current.cl_super with
		| None -> abort "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			let call = if apply then "apply" else "call" in
			print ctx "%s.%s(%s" (ctx.type_accessor (TClassDecl c)) call (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TField ({ eexpr = TConst TSuper },f) , params when ctx.es_version < 6 ->
		(match ctx.current.cl_super with
		| None -> abort "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			let name = field_name f in
			let call = if apply then "apply" else "call" in
			print ctx "%s.prototype%s.%s(%s" (ctx.type_accessor (TClassDecl c)) (field name) call (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TCall (x,_) , el when not (is_code_injection_function x) ->
		if apply then
			gen_call_with_apply ctx e el
		else begin
			spr ctx "(";
			gen_value ctx e;
			spr ctx ")";
			spr ctx "(";
			concat ctx "," (gen_value ctx) el;
			spr ctx ")";
		end
	| TField (_, FStatic ({ cl_path = ["js"],"Syntax" }, { cf_name = meth })), args ->
		gen_syntax ctx meth args e.epos
	| TField (_, FStatic ({ cl_path = ["js"],"Lib" }, { cf_name = "rethrow" })), [] ->
		(match ctx.catch_vars with
			| e :: _ ->
				spr ctx "throw ";
				gen_value ctx e
			| _ ->
				abort "js.Lib.rethrow can only be called inside a catch block" e.epos
		)
	| TField (_, FStatic ({ cl_path = ["js"],"Lib" }, { cf_name = "getOriginalException" })), [] ->
		(match ctx.catch_vars with
			| e :: _ ->
				gen_value ctx e
			| _ ->
				abort "js.Lib.getOriginalException can only be called inside a catch block" e.epos
		)
	| TIdent "__new__", args ->
		print_deprecation_message ctx.com "__new__ is deprecated, use js.Syntax.construct instead" e.epos;
		gen_syntax ctx "construct" args e.epos
	| TIdent "__js__", args ->
		print_deprecation_message ctx.com "__js__ is deprecated, use js.Syntax.code instead" e.epos;
		gen_syntax ctx "code" args e.epos
	| TIdent "__instanceof__",  args ->
		print_deprecation_message ctx.com "__instanceof__ is deprecated, use js.Syntax.instanceof instead" e.epos;
		gen_syntax ctx "instanceof" args e.epos
	| TIdent "__typeof__",  args ->
		print_deprecation_message ctx.com "__typeof__ is deprecated, use js.Syntax.typeof instead" e.epos;
		gen_syntax ctx "typeof" args e.epos
	| TIdent "__strict_eq__" , args ->
		print_deprecation_message ctx.com "__strict_eq__ is deprecated, use js.Syntax.strictEq instead" e.epos;
		gen_syntax ctx "strictEq" args e.epos
	| TIdent "__strict_neq__" , args ->
		print_deprecation_message ctx.com "__strict_neq__ is deprecated, use js.Syntax.strictNeq instead" e.epos;
		gen_syntax ctx "strictNeq" args e.epos
	| TIdent "__define_feature__", [_;e] ->
		gen_expr ctx e
	| TIdent "__feature__", { eexpr = TConst (TString f) } :: eif :: eelse ->
		(if has_feature ctx f then
			gen_value ctx eif
		else match eelse with
			| [] -> ()
			| e :: _ -> gen_value ctx e)
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
			let t = (try List.find (fun t -> t_path t = (["haxe"],"Log")) ctx.com.types with _ -> die "" __LOC__) in
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
	| TField (x,f), [] when field_name f = "iterator" && is_dynamic_iterator ctx e ->
		add_feature ctx "use.$getIterator";
		print ctx "$getIterator(";
		gen_value ctx x;
		print ctx ")";
	| TField (x,f), [] when field_name f = "keyValueIterator" && is_dynamic_iterator ctx e ->
		add_feature ctx "use.$getKeyValueIterator";
		print ctx "$getKeyValueIterator(";
		gen_value ctx x;
		print ctx ")";
	| _ ->
		if apply then
			gen_call_with_apply ctx e el
		else begin
			gen_value ctx e;
			spr ctx "(";
			concat ctx "," (gen_value ctx) el;
			spr ctx ")"
		end

and gen_call_with_apply ctx target args =
	(match args with
	| [_] -> ()
	| _ -> die ~p:target.epos "`args` for `gen_call_with_apply` must contain exactly one item" __LOC__
	);
	match target.eexpr with
	| TField (this, (FInstance (_,_,{ cf_name = field }) | FAnon { cf_name = field } | FDynamic field | FClosure (_,{ cf_name = field }))) ->
		add_feature ctx "thisForCallWithRestArgs";
		spr ctx "($_=";
		gen_value ctx this;
		spr ctx (",$_." ^ field ^ ".apply($_,");
		concat ctx "," (gen_value ctx) args;
		spr ctx "))"
	| _ ->
		gen_value ctx target;
		spr ctx ".apply(null,";
		concat ctx "," (gen_value ctx) args;
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
	let clear_mapping = add_mapping ctx e in
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
	| TBinop (op,{ eexpr = TField (x,f) },e2) when is_iterator_field_access f ->
		gen_value ctx x;
		spr ctx (field (field_name f));
		print ctx " %s " (Ast.s_binop op);
		gen_value ctx e2;
	| TBinop ((OpEq | OpNotEq) as op,{ eexpr = TField (x,(FClosure _ as f)) },{ eexpr = TConst TNull }) ->
		gen_value ctx x;
		spr ctx (field (field_name f));
		print ctx " %s null" (Ast.s_binop op)
	| TBinop ((OpEq | OpNotEq) as op,{ eexpr = TConst TNull },{ eexpr = TField (x,(FClosure _ as f)) }) ->
		print ctx "null %s " (Ast.s_binop op);
		gen_value ctx x;
		spr ctx (field (field_name f))
	| TBinop (op,e1,e2) ->
		gen_value ctx e1;
		print ctx " %s " (Ast.s_binop op);
		gen_value ctx e2;
	| TField (x,f) when field_name f = "iterator" && is_dynamic_iterator ctx e ->
		add_feature ctx "use.$iterator";
		print ctx "$iterator(";
		gen_value ctx x;
		print ctx ")";
	| TField (x,f) when field_name f = "keyValueIterator" && is_dynamic_iterator ctx e ->
		add_feature ctx "use.$keyValueIterator";
		print ctx "$keyValueIterator(";
		gen_value ctx x;
		print ctx ")";
	(* Don't generate `$iterator(value)` for exprs like `value.iterator--` *)
	| TUnop (op,flag,({eexpr = TField (x,f)} as fe)) when is_iterator_field_access f && is_dynamic_iterator ctx fe ->
		(match flag with
			| Prefix ->
				spr ctx (Ast.s_unop op);
				gen_value ctx x;
				print ctx ".%s" (field_name f)
			| Postfix ->
				gen_value ctx x;
				print ctx ".%s" (field_name f);
				spr ctx (Ast.s_unop op))
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
		if not (Common.defined ctx.com Define.JsEnumsAsArrays) then
		print ctx "._hx_index"
		else
		print ctx "[1]"
	| TEnumParameter (x,f,i) ->
		gen_value ctx x;
		if not (Common.defined ctx.com Define.JsEnumsAsArrays) then
			let fname = (match f.ef_type with TFun((args,_)) -> let fname,_,_ = List.nth args i in  fname | _ -> die "" __LOC__ ) in
			print ctx ".%s" (ident fname)
		else
			print ctx "[%i]" (i + 2)
	| TField (_, FStatic ({cl_path = [],""},f)) ->
		spr ctx f.cf_name;
	| TField (x, (FInstance(_,_,f) | FStatic(_,f) | FAnon(f))) when Meta.has Meta.SelfCall f.cf_meta ->
		gen_value ctx x;
	| TField (_,FStatic ({ cl_kind = KModuleFields m },f)) ->
		spr ctx (module_field m f)
	| TField (x,f) ->
		let rec skip e = match e.eexpr with
			| TCast(e1,None) | TMeta(_,e1) -> skip e1
			| TConst(TInt _ | TFloat _) | TObjectDecl _ -> {e with eexpr = TParenthesis e}
			| _ -> e
		in
		let x = skip x in
		gen_value ctx x;
		spr ctx (match f with FStatic(c,f) -> static_field ctx c f | FEnum _ | FInstance _ | FAnon _ | FDynamic _ | FClosure _ -> field (field_name f))
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
		| _ -> die "" __LOC__)
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
		gen_function ctx f e.epos
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
		spr ctx ((var ctx) ^ " ");
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
			| DoubleQuotes -> print ctx "\"%s\" : " (StringHelper.s_escape f);
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
				print ctx "%s %s = " (var ctx) name;
				gen_value ctx it;
				newline ctx;
				name
		) in
		print ctx "while( %s.hasNext() ) {" it;
		let bend = open_block ctx in
		newline ctx;
		print ctx "%s %s = %s.next()" (var ctx) (ident v.v_name) it;
		gen_block_element ctx e;
		bend();
		newline ctx;
		spr ctx "}";
		ctx.in_loop <- old_in_loop
	| TTry (etry,[(v,ecatch)]) ->
		spr ctx "try ";
		gen_expr ctx etry;
		check_var_declaration v;
		print ctx " catch( %s ) " v.v_name;
		ctx.catch_vars <- (mk (TLocal v) v.v_type v.v_pos) :: ctx.catch_vars;
		gen_expr ctx ecatch;
		ctx.catch_vars <- List.tl ctx.catch_vars
	| TTry _ ->
		abort "Unhandled try/catch, please report" e.epos
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
		gen_value ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"
	| TIdent s ->
		spr ctx s
	);
	clear_mapping ()

and gen_function ?(keyword="function") ctx f pos =
	let old = ctx.in_value, ctx.in_loop in
	ctx.in_value <- None;
	ctx.in_loop <- false;
	let f,args =
		match List.rev f.tf_args with
		| (v,None) :: args_rev when ExtType.is_rest (follow v.v_type) ->
			(* Use ES6 rest args syntax: `...arg` *)
			if ctx.es_version >= 6 then
				f, List.map (fun (a,_) ->
					check_var_declaration a;
					if a == v then ("..." ^ ident a.v_name)
					else ident a.v_name
				) f.tf_args
			(* Resort to `arguments` special object for ES < 6 *)
			else
				let args_decl = declare_rest_args_legacy ctx.com (List.rev args_rev) v in
				let body =
					let el =
						match f.tf_expr.eexpr with
						| TBlock el -> args_decl @ el
						| _ -> args_decl @ [f.tf_expr]
					in
					mk (TBlock el) f.tf_expr.etype f.tf_expr.epos
				in
				{ f with tf_args = []; tf_expr = body }, []
		| _ ->
			f, List.map (fun (v,_) ->
				check_var_declaration v;
				ident v.v_name
			) f.tf_args
	in
	print ctx "%s(%s) " keyword (String.concat "," args);
	gen_expr ctx (fun_block ctx f pos);
	ctx.in_value <- fst old;
	ctx.in_loop <- snd old;
	ctx.separator <- true

and gen_block_element ?(newline_after=false) ?(keep_blocks=false) ctx e =
	match e.eexpr with
	| TBlock el when not keep_blocks ->
		List.iter (gen_block_element ~newline_after ctx) el
	| TCall ({ eexpr = TIdent "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse) ->
		if has_feature ctx f then
			gen_block_element ~newline_after ctx eif
		else (match eelse with
			| [] -> ()
			| [e] -> gen_block_element ~newline_after ctx e
			| _ -> die "" __LOC__)
	| TFunction _ ->
		gen_block_element ~newline_after ctx (mk (TParenthesis e) e.etype e.epos)
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> gen_block_element ~newline_after ctx e) fl
	| _ ->
		if not newline_after then newline ctx;
		gen_expr ctx e;
		if newline_after then newline ctx

and gen_value ctx e =
	let clear_mapping = add_mapping ctx e in
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> die "" __LOC__ | Some v -> v)) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let value() =
		let old = ctx.in_value, ctx.in_loop in
		let r = alloc_var VGenerated "$r" t_dynamic e.epos in
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
	clear_mapping ()

and gen_syntax ctx meth args pos =
	match meth, args with
	| "construct", cl :: params ->
		spr ctx "new ";
		begin
			match cl.eexpr with
			| TConst (TString cl) ->
				spr ctx cl
			| _ ->
				gen_value ctx cl
		end;
		spr ctx "(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")"
	| "instanceof", [o;t] ->
		spr ctx "((";
		gen_value ctx o;
		print ctx ") instanceof ";
		gen_value ctx t;
		spr ctx ")"
	| "typeof", [o] ->
		spr ctx "typeof(";
		gen_value ctx o;
		spr ctx ")"
	| "strictEq" , [x;y] ->
		(* add extra parenthesis here because of operator precedence *)
		spr ctx "((";
		gen_value ctx x;
		spr ctx ") === ";
		gen_value ctx y;
		spr ctx ")";
	| "strictNeq" , [x;y] ->
		(* add extra parenthesis here because of operator precedence *)
		spr ctx "((";
		gen_value ctx x;
		spr ctx ") !== ";
		gen_value ctx y;
		spr ctx ")";
	| "delete" , [o;f] ->
		spr ctx "delete(";
		gen_value ctx o;
		spr ctx "[";
		gen_value ctx f;
		spr ctx "]";
		spr ctx ")";
	| "code", code :: args ->
		let code, code_pos =
			match code.eexpr with
			| TConst (TString s) -> s, code.epos
			| _ -> abort "The `code` argument for js.Syntax.code must be a string constant" code.epos
		in
		begin
			match args with
			| [] when code = "this" ->
				spr ctx (this ctx)
			| _ ->
				let rec reveal_expr expr =
					match expr.eexpr with
						| TCast (e, _) | TMeta (_, e) -> reveal_expr e
						| _ -> expr
				in
				let args = List.map
					(fun arg ->
						match (reveal_expr arg).eexpr with
							| TIf _ | TBinop _ | TUnop _ -> { arg with eexpr = TParenthesis arg }
							| _ -> arg
					)
					args
				in
				Codegen.interpolate_code ctx.com code args (spr ctx) (gen_value ctx) code_pos
		end
	| "plainCode", [code] ->
		let code =
			match code.eexpr with
			| TConst (TString s) -> s
			| _ -> abort "The `code` argument for js.Syntax.plainCode must be a string constant" code.epos
		in
		spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
	| "field" , [eobj;efield] ->
		gen_value ctx eobj;
		(match Texpr.skip efield with
		| { eexpr = TConst(TString(s)) } when valid_js_ident s ->
			spr ctx ".";
			spr ctx s;
		| _ ->
			spr ctx "[";
			gen_value ctx efield;
			spr ctx "]";
		)
	| _ ->
		abort (Printf.sprintf "Unknown js.Syntax method `%s` with %d arguments" meth (List.length args)) pos

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

let gen_module_fields ctx m c fl =
	List.iter (fun f ->
		let name = module_field m f in
		match f.cf_expr with
		| None when not (is_physical_field f) ->
			()
		| None ->
			print ctx "var %s = null" name;
			newline ctx
		| Some e ->
			match e.eexpr with
			| TFunction fn ->
				ctx.id_counter <- 0;
				print ctx "function %s" name;
				gen_function ~keyword:"" ctx fn e.epos;
				ctx.separator <- false;
				newline ctx;
				process_expose f.cf_meta (fun () -> module_field_expose_path m.m_path f) (fun s ->
					print ctx "$hx_exports%s = %s" (path_to_brackets s) name;
					newline ctx
				)
			| _ ->
				ctx.statics <- (c,f,e) :: ctx.statics
	) fl

let gen_class_static_field ctx c cl_path f =
	match f.cf_expr with
	| None | Some { eexpr = TConst TNull } when not (has_feature ctx "Type.getClassFields") ->
		()
	| None when not (is_physical_field f) ->
		()
	| None ->
		print ctx "%s%s = null" (s_path ctx cl_path) (static_field ctx c f);
		newline ctx
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			let path = (s_path ctx cl_path) ^ (static_field ctx c f) in
			ctx.id_counter <- 0;
			print ctx "%s = " path;
			process_expose f.cf_meta (fun () -> (dot_path cl_path) ^ "." ^ f.cf_name) (fun s -> print ctx "$hx_exports%s = " (path_to_brackets s));
			gen_value ctx e;
			newline ctx;
		| _ ->
			ctx.statics <- (c,f,e) :: ctx.statics

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

let generate_class___name__ ctx cl_path =
	if has_feature ctx "js.Boot.isClass" then begin
		let p = s_path ctx cl_path in
		print ctx "%s.__name__ = " p;
		(match has_feature ctx "Type.getClassName", cl_path with
			| true, _
			| _, ([], ("Array" | "String")) ->
				print ctx "\"%s\"" (dot_path cl_path)
			| _ ->
				print ctx "true"
		);
		newline ctx;
	end

let generate_class___isInterface__ ctx c =
	if (has_class_flag c CInterface) && has_feature ctx "js.Boot.isInterface" then begin
		let p = s_path ctx c.cl_path in
		print ctx "%s.__isInterface__ = true" p;
		newline ctx;
	end

let get_generated_class_path = function
	(* we want to generate abstract implementations with the path of the abstract itself, unless there is @:native involved *)
	| { cl_kind = KAbstractImpl a; cl_meta = m } when not (Meta.has Meta.Native m) ->
		a.a_path
	| { cl_path = p } ->
		p

let is_abstract_impl c = match c.cl_kind with KAbstractImpl _ -> true | _ -> false

let generate_class_es3 ctx c =
	let cl_path = get_generated_class_path c in
	let is_abstract_impl = is_abstract_impl c in

	if ctx.js_flatten then
		print ctx "var "
	else
		generate_package_create ctx cl_path;

	let p = s_path ctx cl_path in
	let dotp = dot_path cl_path in

	let added_to_hxClasses = ctx.has_resolveClass && not is_abstract_impl in

	if ctx.js_modern || not added_to_hxClasses then
		print ctx "%s = " p
	else
		print ctx "%s = $hxClasses[\"%s\"] = " p dotp;

	process_expose c.cl_meta (fun () -> dotp) (fun s -> print ctx "$hx_exports%s = " (path_to_brackets s));

	if is_abstract_impl then begin
		(* abstract implementations only contain static members and don't need to have constructor functions *)
		print ctx "{}";
		ctx.separator <- true
	end else begin
		match c.cl_constructor with
		| Some { cf_expr = Some e } -> gen_expr ctx e
		| _ -> (print ctx "function() { }"); ctx.separator <- true
	end;

	newline ctx;

	if ctx.js_modern && added_to_hxClasses then begin
		print ctx "$hxClasses[\"%s\"] = %s" dotp p;
		newline ctx;
	end;

	if not is_abstract_impl then begin
		generate_class___name__ ctx cl_path;
		generate_class___isInterface__ ctx c;
	end;

	if ctx.has_interface_check then
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

	List.iter (gen_class_static_field ctx c cl_path) c.cl_ordered_statics;

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

let generate_class_es6 ctx c =
	let cl_path = get_generated_class_path c in
	let p = s_path ctx cl_path in
	let dotp = dot_path cl_path in

	let cls_name =
		if not ctx.js_flatten && (fst cl_path) <> [] then begin
			generate_package_create ctx cl_path;
			print ctx "%s = " p;
			Path.flat_path cl_path
		end else
			p
	in
	if (has_meta Meta.Expose c.cl_meta) then print ctx "export ";
	print ctx "class %s" cls_name;

	Option.may (fun (csup,_) ->
		let psup = ctx.type_accessor (TClassDecl csup) in
		print ctx " extends %s" psup
	) c.cl_super;

	spr ctx " {";
	let close_block = open_block ctx in

	(match c.cl_constructor with
	| Some { cf_expr = Some ({ eexpr = TFunction f; epos = p }) } ->
		newline ctx;
		gen_function ~keyword:"constructor" ctx f p;
		ctx.separator <- false
	| _ -> ());

	let method_def_name cf =
		if valid_js_ident cf.cf_name then cf.cf_name else "\"" ^ cf.cf_name ^ "\""
	in

	let nonmethod_fields =
		List.filter (fun cf ->
			match cf.cf_kind, cf.cf_expr with
			| Method _, Some { eexpr = TFunction f; epos = pos } ->
				check_field_name c cf;
				newline ctx;
				gen_function ~keyword:(method_def_name cf) ctx f pos;
				ctx.separator <- false;
				false
			| _ ->
				true
		) c.cl_ordered_fields
	in

	let exposed_static_methods = ref [] in
	let nonmethod_statics =
		List.filter (fun cf ->
			match cf.cf_kind, cf.cf_expr with
			| Method _, Some { eexpr = TFunction f; epos = pos } ->
				check_field_name c cf;
				newline ctx;
				gen_function ~keyword:("static " ^ (method_def_name cf)) ctx f pos;
				ctx.separator <- false;

				process_expose cf.cf_meta (fun () -> dotp  ^ "." ^ cf.cf_name) (fun s -> exposed_static_methods := (s,cf.cf_name) :: !exposed_static_methods);

				false
			| _ -> true
		) c.cl_ordered_statics
	in

	close_block ();
	newline ctx;
	spr ctx "}";
	newline ctx;

	List.iter (fun (path,name) ->
		print ctx "$hx_exports%s = %s.%s;" (path_to_brackets path) p name;
		newline ctx
	) !exposed_static_methods;

	List.iter (gen_class_static_field ctx c cl_path) nonmethod_statics;

	let is_abstract_impl = is_abstract_impl c in

	begin
		let added = ref false in
		if ctx.has_resolveClass && not is_abstract_impl then begin
			added := true;
			print ctx "$hxClasses[\"%s\"] = " dotp
		end;
		process_expose c.cl_meta (fun () -> dotp) (fun s -> added := true; print ctx "$hx_exports%s = " (path_to_brackets s));
		if !added then begin
			spr ctx p;
			newline ctx;
		end;
	end;

	if not is_abstract_impl then begin
		generate_class___name__ ctx cl_path;
		generate_class___isInterface__ ctx c;
	end;

	if ctx.has_interface_check then
		(match c.cl_implements with
		| [] -> ()
		| l ->
			print ctx "%s.__interfaces__ = [%s]" p (String.concat "," (List.map (fun (i,_) -> ctx.type_accessor (TClassDecl i)) l));
			newline ctx;
		);

	let has_property_reflection =
		(has_feature ctx "Reflect.getProperty") || (has_feature ctx "Reflect.setProperty")
	in
	let gen_props props =
		String.concat "," (List.map (fun (p,v) -> p ^": \""^v^"\"") props)
	in

	if has_property_reflection then begin
		(match Codegen.get_properties nonmethod_statics with
		| [] -> ()
		| props ->
			print ctx "%s.__properties__ = {%s}" p (gen_props props);
			ctx.separator <- true;
			newline ctx);
	end;

	(match c.cl_super with
	| Some (csup,_) ->
		if ctx.has_interface_check || has_feature ctx "Type.getSuperClass" then begin
			let psup = ctx.type_accessor (TClassDecl csup) in
			print ctx "%s.__super__ = %s" p psup;
			newline ctx
		end
	| None -> ());

	let has_class = has_feature ctx "js.Boot.getClass" && (c.cl_super <> None || c.cl_ordered_fields <> [] || c.cl_constructor <> None) in
	let props_to_generate = if has_property_reflection then Codegen.get_properties c.cl_ordered_fields else [] in
	let fields_to_generate =
		if has_feature ctx "Type.getInstanceFields" then
			if (has_class_flag c CInterface) then
				List.filter is_physical_field c.cl_ordered_fields
			else
				List.filter is_physical_var_field nonmethod_fields
		else
			[]
	in

	if has_class || props_to_generate <> [] || fields_to_generate <> [] then begin
		print ctx "Object.assign(%s.prototype, {" p;
		let bend = open_block ctx in

		if has_class then begin
			newprop ctx;
			print ctx "__class__: %s" p
		end;

		if fields_to_generate <> [] then begin
			List.iter (gen_class_field ctx c) fields_to_generate;
		end;

		if props_to_generate <> [] then begin
			newprop ctx;
			match c.cl_super with
			| Some (csup, _) when Codegen.has_properties csup ->
				let psup = ctx.type_accessor (TClassDecl csup) in
				print ctx "__properties__: Object.assign({}, %s.prototype.__properties__, {%s})" psup (gen_props props_to_generate)
			| _ ->
				print ctx "__properties__: {%s}" (gen_props props_to_generate)
		end;

		bend();
		print ctx "\n})";
		newline ctx
	end;

	flush ctx

let generate_class ctx c =
	ctx.current <- c;
	ctx.id_counter <- 0;
	(match c.cl_path with
	| [],"Function" -> abort "This class redefine a native one" c.cl_pos
	| _ -> ());
	match c.cl_kind with
	| KModuleFields m ->
		gen_module_fields ctx m c c.cl_ordered_statics
	| _ ->
		if ctx.es_version >= 6 then
			generate_class_es6 ctx c
		else
			generate_class_es3 ctx c

let generate_enum ctx e =
	let p = s_path ctx e.e_path in
	let dotp = dot_path e.e_path in
	let has_enum_feature = has_feature ctx "has_enum" in
	if ctx.js_flatten then
		print ctx "var "
	else
		generate_package_create ctx e.e_path;
	print ctx "%s = " p;
	let as_objects = not (Common.defined ctx.com Define.JsEnumsAsArrays) in
	(if as_objects then
		print ctx "$hxEnums[\"%s\"] = " dotp
	else if has_feature ctx "Type.resolveEnum" then
		print ctx "$hxClasses[\"%s\"] = " dotp);
	spr ctx "{";
	if has_feature ctx "js.Boot.isEnum" then print ctx " __ename__:%s," (if has_feature ctx "Type.getEnumName" then "\"" ^ dotp ^ "\"" else "true");
	if as_objects then
		print ctx "__constructs__:null"
	else
		print ctx "__constructs__:[%s]" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" s) e.e_names));
	let bend =
		if not as_objects then begin
			spr ctx " }";
			ctx.separator <- true;
			newline ctx;
			fun () -> ()
		end else begin
			open_block ctx
		end;
	in
	List.iter (fun n ->
		let f = PMap.find n e.e_constrs in
		if as_objects then begin
			newprop ctx;
			print ctx "%s: " (anon_field f.ef_name)
		end else
			print ctx "%s%s = " p (field f.ef_name);
		(match f.ef_type with
		| TFun (args,_) ->
			let sargs = String.concat "," (List.map (fun (n,_,_) -> ident n) args) in begin
			if as_objects then begin
				let sfields = String.concat "," (List.map (fun (n,_,_) -> (ident n) ^ ":" ^ (ident n) ) args) in
				let sparams = String.concat "," (List.map (fun (n,_,_) -> "\"" ^ (ident n) ^ "\"" ) args) in
				print ctx "($_=function(%s) { return {_hx_index:%d,%s,__enum__:\"%s\"" sargs f.ef_index sfields dotp;
				if has_enum_feature then
					spr ctx ",toString:$estr";
				print ctx "}; },$_._hx_name=\"%s\",$_.__params__ = [%s],$_)" f.ef_name sparams
			end else begin
				print ctx "function(%s) { var $x = [\"%s\",%d,%s]; $x.__enum__ = %s;" sargs f.ef_name f.ef_index sargs p;
				if has_enum_feature then
					spr ctx " $x.toString = $estr;";
				spr ctx " return $x; }";
			end end;
		| _ ->
			if as_objects then
				print ctx "{_hx_name:\"%s\",_hx_index:%d,__enum__:\"%s\"%s}" f.ef_name f.ef_index dotp (if has_enum_feature then ",toString:$estr" else "")
			else begin
				print ctx "[\"%s\",%d]" f.ef_name f.ef_index;
				newline ctx;
				if has_feature ctx "has_enum" then begin
					print ctx "%s%s.toString = $estr" p (field f.ef_name);
					newline ctx;
				end;
				print ctx "%s%s.__enum__ = %s" p (field f.ef_name) p;
			end
		);
		if not as_objects then
			newline ctx
	) e.e_names;
	bend();
	if as_objects then begin
		spr ctx "\n}";
		ctx.separator <- true;
		newline ctx;
		print ctx "%s.__constructs__ = [%s]" p (String.concat "," (List.map (fun s -> Printf.sprintf "%s%s" p (field s)) e.e_names));
		newline ctx;
	end;
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
	begin match Texpr.build_metadata ctx.com.basic (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "%s.__meta__ = " p;
		gen_expr ctx e;
		newline ctx
	end;
	flush ctx

let generate_static ctx (c,f,e) =
	begin
	match c.cl_kind with
	| KModuleFields m ->
		print ctx "var %s = " (module_field m f);
		process_expose f.cf_meta (fun () -> module_field_expose_path m.m_path f) (fun s -> print ctx "$hx_exports%s = " (path_to_brackets s));
	| _ ->
		let cl_path = get_generated_class_path c in
		process_expose f.cf_meta (fun () -> (dot_path cl_path) ^ "." ^ f.cf_name) (fun s -> print ctx "$hx_exports%s = " (path_to_brackets s));
		print ctx "%s%s = " (s_path ctx cl_path) (static_field ctx c f);
	end;
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

let need_to_generate_interface ctx cl_iface =
	ctx.has_resolveClass (* generate so we can resolve it for whatever reason *)
	|| ctx.has_interface_check (* generate because we need __interfaces__ for run-time type checks *)
	|| is_directly_used ctx.com cl_iface.cl_meta (* generate because it's just directly accessed in code *)

let generate_type ctx = function
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e ->
			ctx.inits <- e :: ctx.inits);
		(* Special case, want to add Math.__name__ only when required, handle here since Math is extern *)
		let p = s_path ctx c.cl_path in
		if p = "Math" then generate_class___name__ ctx c.cl_path;
		(* Another special case for Std because we do not want to generate it if it's empty. *)
		if p = "Std" && c.cl_ordered_statics = [] then
			()
		else if not (has_class_flag c CExtern) then begin
			if (not (has_class_flag c CInterface)) || (need_to_generate_interface ctx c) then
				generate_class ctx c
		end else if Meta.has Meta.JsRequire c.cl_meta && is_directly_used ctx.com c.cl_meta then
			generate_require ctx (get_generated_class_path c) c.cl_meta
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

let alloc_ctx com es_version =
	let smap =
		if com.debug || Common.defined com Define.JsSourceMap || Common.defined com Define.SourceMap then
			Some {
				source_last_pos = { file = 0; line = 0; col = 0};
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
		has_resolveClass = Common.has_feature com "Type.resolveClass";
		has_interface_check = Common.has_feature com "js.Boot.__interfLoop";
		es_version = es_version;
		statics = [];
		inits = [];
		current = null_class;
		tabs = "";
		in_value = None;
		in_loop = false;
		id_counter = 0;
		type_accessor = (fun _ -> die "" __LOC__);
		separator = false;
		found_expose = false;
		catch_vars = [];
	} in

	ctx.type_accessor <- (fun t ->
		match t with
		| TEnumDecl ({ e_extern = true } as e) when not (Meta.has Meta.JsRequire e.e_meta) ->
			dot_path e.e_path
		| TClassDecl c ->
			let p = get_generated_class_path c in
			if (has_class_flag c CExtern) && not (Meta.has Meta.JsRequire c.cl_meta) then
				dot_path p
			else
				s_path ctx p
		| _ ->
			s_path ctx (t_path t)
	);
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

	let es_version = get_es_version com in

	if es_version >= 6 then
		ES6Ctors.rewrite_ctors com;

	let ctx = alloc_ctx com es_version in
	Codegen.map_source_header com (fun s -> print ctx "// %s\n" s);
	if has_feature ctx "Class" || has_feature ctx "Type.getClassName" then add_feature ctx "js.Boot.isClass";
	if has_feature ctx "Enum" || has_feature ctx "Type.getEnumName" then add_feature ctx "js.Boot.isEnum";

	let nodejs = Common.raw_defined com "nodejs" in

	setup_kwds com;

	let exposed = begin
		let r = ref [] in
		List.iter (
			function
			| TClassDecl c ->
				let add s = r := s :: !r in
				let get_expose_path =
					match c.cl_kind with
					| KModuleFields m ->
						module_field_expose_path m.m_path
					| _ ->
						let path = dot_path c.cl_path in
						process_expose c.cl_meta (fun () -> path) add;
						fun f -> path ^ "." ^ f.cf_name
				in
				List.iter (fun f ->
					process_expose f.cf_meta (fun () -> get_expose_path f) add
				) c.cl_ordered_statics
			| _ -> ()
		) com.types;
		!r
	end in
	let anyExposed = exposed <> [] in
	let exposedObject = { os_name = ""; os_fields = [] } in
	let toplevelExposed = ref [] in
	if anyExposed then begin
		let exportMap = Hashtbl.create 0 in
		List.iter (fun path ->
			let parts = ExtString.String.nsplit path "." in
			let rec loop p pre =
				match p with
				| f :: g :: ls ->
					let path = match pre with "" -> f | pre -> (pre ^ "." ^ f) in
					if not (Hashtbl.mem exportMap path) then (
						let elts = { os_name = f; os_fields = [] } in
						Hashtbl.add exportMap path elts;
						let cobject = match pre with "" -> exposedObject | pre -> Hashtbl.find exportMap pre in
						cobject.os_fields <- elts :: cobject.os_fields
					);
					loop (g :: ls) path;
				| f :: [] when pre = "" ->
					toplevelExposed := f :: !toplevelExposed;
				| _ -> ()
			in
			loop parts ""
		) exposed
	end;

	let include_files = List.rev com.include_files in

	List.iter (fun file ->
		match file with
		| path, "top" ->
			let file_content = Std.input_file ~bin:true (fst file) in
			print ctx "%s\n;" file_content;
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

	let closureArgs = [var_global] in
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
			print ctx "%s\n;" file_content;
			()
		| _ -> ()
	) include_files;

	(* If ctx.js_modern, console is defined in closureArgs. *)
	if (not ctx.js_modern) && (ctx.es_version < 5) then
		add_feature ctx "js.Lib.global"; (* console polyfill will check console from $global *)

	if (not ctx.js_modern) then
		print ctx "var %s = %s;\n" (fst var_global) (snd var_global);

	if (not ctx.js_modern) && (ctx.es_version < 5) then
		spr ctx "var console = $global.console || {log:function(){}};\n";

	let enums_as_objects = not (Common.defined com Define.JsEnumsAsArrays) in

	(* TODO: fix $estr *)
	let vars = [] in
	let vars = (if ctx.has_resolveClass || (not enums_as_objects && has_feature ctx "Type.resolveEnum") then ("$hxClasses = " ^ (if ctx.js_modern then "{}" else "$hxClasses || {}")) :: vars else vars) in
	let vars = if has_feature ctx "has_enum"
		then ("$estr = function() { return " ^ (ctx.type_accessor (TClassDecl { null_class with cl_path = ["js"],"Boot" })) ^ ".__string_rec(this,''); }") :: vars
		else vars in
	let vars = if (enums_as_objects && (has_feature ctx "has_enum" || has_feature ctx "Type.resolveEnum")) then "$hxEnums = $hxEnums || {}" :: vars else vars in
	let vars,has_dollar_underscore =
		if List.exists (function TEnumDecl { e_extern = false } -> true | _ -> false) com.types then
			"$_" :: vars,ref true
		else
			vars,ref false
	in
	(match List.rev vars with
	| [] -> ()
	| vl ->
		print ctx "var %s" (String.concat "," vl);
		ctx.separator <- true;
		newline ctx
	);
	if ctx.es_version < 6 && List.exists (function TClassDecl ({ cl_super = Some _ } as c) -> not (has_class_flag c CExtern) | _ -> false) com.types then begin
		let extend_code =
			"function $extend(from, fields) {\n" ^
			(
				if ctx.es_version < 5 then
					"	function Inherit() {} Inherit.prototype = from; var proto = new Inherit();\n"
				else
					"	var proto = Object.create(from);\n"
			) ^
			"	for (var name in fields) proto[name] = fields[name];\n" ^
			"	if( fields.toString !== Object.prototype.toString ) proto.toString = fields.toString;\n" ^
			"	return proto;\n" ^
			"}\n"
		in
		spr ctx extend_code
	end;
	List.iter (generate_type ctx) com.types;
	let rec chk_features e =
		if is_dynamic_iterator ctx e then add_feature ctx "use.$iterator";
		match e.eexpr with
		| TField (_,FClosure _) ->
			add_feature ctx "use.$bind"
		| TCall ({ eexpr = TField (_,f) } as ef, []) when field_name f = "iterator" && is_dynamic_iterator ctx ef ->
			add_feature ctx "use.$getIterator";
		| _ ->
			Type.iter chk_features e
	in
	List.iter chk_features ctx.inits;
	List.iter (fun (_,_,e) -> chk_features e) ctx.statics;
	if has_feature ctx "use.$iterator" then begin
		add_feature ctx "use.$bind";
		let array_iterator = s_path ctx (["haxe"; "iterators"], "ArrayIterator") in
		print ctx "function $iterator(o) { if( o instanceof Array ) return function() { return new %s(o); }; return typeof(o.iterator) == 'function' ? $bind(o,o.iterator) : o.iterator; }" array_iterator;
		newline ctx;
	end;
	if has_feature ctx "use.$keyValueIterator" then begin
		add_feature ctx "use.$bind";
		print ctx "function $keyValueIterator(o) { if( o instanceof Array ) return function() { return HxOverrides.keyValueIter(o); }; return typeof(o.keyValueIterator) == 'function' ? $bind(o,o.keyValueIterator) : o.keyValueIterator; }";
		newline ctx;
	end;
	if has_feature ctx "use.$getIterator" then begin
		let array_iterator = s_path ctx (["haxe"; "iterators"], "ArrayIterator") in
		print ctx "function $getIterator(o) { if( o instanceof Array ) return new %s(o); else return o.iterator(); }" array_iterator;
		newline ctx;
	end;
	if has_feature ctx "use.$getKeyValueIterator" then begin
		print ctx "function $getKeyValueIterator(o) { if( o instanceof Array ) return HxOverrides.keyValueIter(o); else return o.keyValueIterator(); }";
		newline ctx;
	end;
	if has_feature ctx "use.$bind" then begin
		add_feature ctx "$global.$haxeUID";
		if not !has_dollar_underscore then begin
			print ctx "var $_";
			newline ctx;
			has_dollar_underscore := true
		end;
		(if ctx.es_version < 5 then
			print ctx "function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $global.$haxeUID++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; }"
		else
			print ctx "function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $global.$haxeUID++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = m.bind(o); o.hx__closures__[m.__id__] = f; } return f; }"
		);
		newline ctx;
	end;
	if has_feature ctx "use.$arrayPush" then begin
		print ctx "function $arrayPush(x) { this.push(x); }";
		newline ctx
	end;
	if has_feature ctx "$global.$haxeUID" then begin
		add_feature ctx "js.Lib.global";
		print ctx "$global.$haxeUID |= 0;\n";
	end;
	if not !has_dollar_underscore && has_feature ctx "thisForCallWithRestArgs" then begin
		print ctx "var $_";
		newline ctx;
		has_dollar_underscore := true
	end;
	List.iter (gen_block_element ~newline_after:true ~keep_blocks:(ctx.es_version >= 6) ctx) (List.rev ctx.inits);
	List.iter (generate_static ctx) (List.rev ctx.statics);
	(match com.main with
	| None -> ()
	| Some e -> gen_expr ctx e; newline ctx);
	if ctx.js_modern then begin
		let closureArgs =
			if has_feature ctx "js.Lib.global" then
				closureArgs
			else
				(* no need for `typeof window != "undefined" ? window : typeof global != "undefined" ? <...>` *)
				match List.rev closureArgs with
					| (global_name,global_value) :: rest ->
						List.rev ((global_name,"{}") :: rest)
					| _ ->
						closureArgs
		in
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

