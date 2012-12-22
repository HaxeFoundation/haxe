(*
 *  haXe/PHP Compiler
 *  Copyright (c)2008 Franco Ponticelli
 *  based on and including code by (c)2005-2008 Nicolas Cannasse
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

type method_name = {
	mutable mpath : path;
	mutable mname : string;
}

type inline_method = {
	iname       : string;
	iindex      : int;
	iexpr       : texpr;
	ihasthis    : bool;
	iin_block   : bool;
	iarguments  : string list;
	ilocals     : (string,string) PMap.t;
	iinv_locals : (string,string) PMap.t;
}

type context = {
	com : Common.context;
	ch : out_channel;
	buf : Buffer.t;
	path : path;
	stack : Codegen.stack_context;
	mutable nested_loops : int;
	mutable inline_index : int;
	mutable curclass : tclass;
	mutable curmethod : string;
	mutable tabs : string;
	mutable in_value : string option;
	mutable in_loop : bool;
	mutable in_block : bool;
	mutable in_instance_method : bool;
	mutable imports : (string,string list list) Hashtbl.t;
	mutable extern_required_paths : (string list * string) list;
	mutable extern_classes_with_init : path list;
	mutable locals : (string,string) PMap.t;
	mutable inv_locals : (string,string) PMap.t;
	mutable local_types : t list;
	mutable inits : texpr list;
	mutable constructor_block : bool;
	mutable all_dynamic_methods: method_name list;
	mutable dynamic_methods: tclass_field list;
	mutable is_call : bool;
	mutable cwd : string;
	mutable inline_methods : inline_method list;
	mutable lib_path : string;
}

let join_class_path path separator =
	let result = match fst path, snd path with
	| [], s -> s
	| el, s -> String.concat separator el ^ separator ^ s in
	if (String.contains result '+') then begin
		let idx = String.index result '+' in
		(String.sub result 0 idx) ^ (String.sub result (idx+1) ((String.length result) - idx -1 ) )
	end else
		result;;

(*  Get a string to represent a type.
	 The "suffix" will be nothing or "_obj", depending if we want the name of the
	 pointer class or the pointee (_obj class *)
let rec class_string klass suffix params =
	(match klass.cl_path with
	(* Array class *)
	|  ([],"Array") -> (snd klass.cl_path) ^ suffix ^ "<" ^ (String.concat ","
					 (List.map type_string  params) ) ^ " >"
	| _ when (match klass.cl_kind with KTypeParameter _ -> true | _ -> false) -> "Dynamic"
	|  ([],"#Int") -> "/* # */int"
	|  (["haxe";"io"],"Unsigned_char__") -> "unsigned char"
	|  ([],"Class") -> "Class"
	|  ([],"Null") -> (match params with
			| [t] ->
				(match follow t with
				| TInst ({ cl_path = [],"Int" },_)
				| TInst ({ cl_path = [],"Float" },_)
				| TEnum ({ e_path = [],"Bool" },_) -> "Dynamic"
				| _ -> "/*NULL*/" ^ (type_string t) )
			| _ -> assert false);
	(* Normal class *)
	| _ -> (join_class_path klass.cl_path "::") ^ suffix
	)
and type_string_suff suffix haxe_type =
	(match haxe_type with
	| TMono r -> (match !r with None -> "Dynamic" | Some t -> type_string_suff suffix t)
	| TAbstract ({ a_path = [],"Int" },[]) -> "int"
	| TAbstract ({ a_path = [],"Float" },[]) -> "double"
	| TAbstract ({ a_path = [],"Bool" },[]) -> "bool"
	| TAbstract ({ a_path = [],"Void" },[]) -> "Void"
	| TEnum ({ e_path = ([],"Void") },[]) -> "Void"
	| TEnum ({ e_path = ([],"Bool") },[]) -> "bool"
	| TInst ({ cl_path = ([],"Float") },[]) -> "double"
	| TInst ({ cl_path = ([],"Int") },[]) -> "int"
	| TEnum (enum,params) ->  (join_class_path enum.e_path "::") ^ suffix
	| TInst (klass,params) ->  (class_string klass suffix params)
	| TAbstract (abs,params) ->  (join_class_path abs.a_path "::") ^ suffix
	| TType (type_def,params) ->
		(match type_def.t_path with
		| [] , "Null" ->
			(match params with
			| [t] ->
				(match follow t with
				| TInst ({ cl_path = [],"Int" },_)
				| TInst ({ cl_path = [],"Float" },_)
				| TEnum ({ e_path = [],"Bool" },_) -> "Dynamic"
				| _ -> type_string_suff suffix t)
			| _ -> assert false);
		| [] , "Array" ->
			(match params with
			| [t] -> "Array<" ^ (type_string (follow t) ) ^ " >"
			| _ -> assert false)
		| _ ->  type_string_suff suffix (apply_params type_def.t_types params type_def.t_type)
		)
	| TFun (args,haxe_type) -> "Dynamic"
	| TAnon anon -> "Dynamic"
	| TDynamic haxe_type -> "Dynamic"
	| TLazy func -> type_string_suff suffix ((!func)())
	)
and type_string haxe_type =
	type_string_suff "" haxe_type;;

let debug_expression expression type_too =
	"/* " ^ Type.s_expr_kind expression ^ (if (type_too) then " = " ^ (type_string (follow expression.etype)) else "") ^ " */";;

let rec register_extern_required_path ctx path =
	if (List.exists(fun p -> p = path) ctx.extern_classes_with_init) && not (List.exists(fun p -> p = path) ctx.extern_required_paths) then
		ctx.extern_required_paths <- path :: ctx.extern_required_paths

let s_expr_expr = Type.s_expr_kind

let s_expr_name e =
	s_type (print_context()) (follow e.etype)

let s_type_name t =
	s_type (print_context()) t

let rec is_uncertain_type t =
	match follow t with
	| TInst (c, _) -> c.cl_interface
	| TMono _ -> true
	| TAnon a ->
	  (match !(a.a_status) with
	  | Statics _
	  | EnumStatics _ -> false
	  | _ -> true)
	| TDynamic _ -> true
	| _ -> false

let is_uncertain_expr e =
	is_uncertain_type e.etype

let rec is_anonym_type t =
	match follow t with
	| TAnon a ->
	  (match !(a.a_status) with
	  | Statics _
	  | EnumStatics _ -> false
	  | _ -> true)
	| TDynamic _ -> true
	| _ -> false

let is_anonym_expr e = is_anonym_type e.etype

let rec is_unknown_type t =
	match follow t with
	| TMono r ->
		(match !r with
		| None -> true
		| Some t -> is_unknown_type t)
	| _ -> false

let is_unknown_expr e =	is_unknown_type e.etype

let rec is_string_type t =
	match follow t with
	| TInst ({cl_path = ([], "String")}, _) -> true
	| TAnon a ->
	   (match !(a.a_status) with
	   | Statics ({cl_path = ([], "String")}) -> true
	   | _ -> false)
	| _ -> false

let is_string_expr e = is_string_type e.etype

let to_string ctx e =
	let v = alloc_var "__call__" t_dynamic in
	let f = mk (TLocal v) t_dynamic e.epos in
	mk (TCall (f, [ Codegen.string ctx.com "_hx_string_rec" e.epos; e; Codegen.string ctx.com "" e.epos])) ctx.com.basic.tstring e.epos

let as_string_expr ctx e =
	match e.eexpr with
	| TConst (TNull) ->
		to_string ctx e
	| _ when not (is_string_expr e) ->
		to_string ctx e
	| _ -> e

let spr ctx s = Buffer.add_string ctx.buf s
let print ctx = Printf.kprintf (fun s -> Buffer.add_string ctx.buf s)

(*--php-prefix - added by skial bainn*)
let prefix_class com name =
	match com.php_prefix with
	| Some prefix_class (* when not (String.length name <= 2 || String.sub name 0 2 = "__") *) ->
		prefix_class ^ name
	| _ ->
		name

let prefix_init_replace com code =
	let r = Str.regexp "php_Boot" in
	Str.global_replace r ("php_" ^ (prefix_class com "Boot")) code

let s_path ctx path isextern p =
	if isextern then begin
		register_extern_required_path ctx path;
		snd path
	end else begin
		(match path with
		(*--php-prefix*)
		| ([],"List")			-> (prefix_class ctx.com "HList")
		(*--php-prefix*)
		| ([],name)				-> (prefix_class ctx.com name)
		| (pack,name) ->
			(try
				(match Hashtbl.find ctx.imports name with
				| [p] when p = pack ->
					()
				| packs ->
					if not (List.mem pack packs) then Hashtbl.replace ctx.imports name (pack :: packs))
			with Not_found ->
				Hashtbl.add ctx.imports name [pack]);
			(*--php-prefix*)
			String.concat "_" pack ^ "_" ^ (prefix_class ctx.com name))
	end

let s_path_haxe path =
	match fst path, snd path with
	| [], s -> s
	| el, s -> String.concat "." el ^ "." ^ s

let s_ident n =
	let suf = "h" in
(*
haxe reserved words that match php ones: break, case, class, continue, default, do, else, extends, for, function, if, new, return, static, switch, var, while, interface, implements, public, private, try, catch, throw
 *)
(* PHP only (for future use): cfunction, old_function *)
	match String.lowercase n with
	| "and" | "or" | "xor" | "__file__" | "exception" | "__line__" | "array"
	| "as" | "const" | "declare" | "die" | "echo"| "elseif" | "empty"
	| "enddeclare" | "endfor" | "endforeach" | "endif" | "endswitch"
	| "endwhile" | "eval" | "exit" | "foreach"| "global" | "include"
	| "include_once" | "isset" | "list" | "namespace" | "print" | "require" | "require_once"
	| "unset" | "use" | "__function__" | "__class__" | "__method__" | "final"
	| "php_user_filter" | "protected" | "abstract" | "__set" | "__get" | "__call"
	| "clone" | "instanceof" -> suf ^ n
	| _ -> n

let s_ident_local n =
	let suf = "h" in
	match String.lowercase n with
	| "globals" | "_server" | "_get" | "_post" | "_cookie" | "_files"
	| "_env" | "_request" | "_session" -> suf ^ n
	| _ -> n

let create_directory com ldir =
 	let atm_path = ref (String.create 0) in
 	atm_path := com.file;
 	if not (Sys.file_exists com.file) then (Unix.mkdir com.file 0o755);
 	(List.iter (fun p -> atm_path := !atm_path ^ "/" ^ p; if not (Sys.file_exists !atm_path) then (Unix.mkdir !atm_path 0o755);) ldir)

let write_resource dir name data =
	let i = ref 0 in
	String.iter (fun c ->
		if c = '\\' || c = '/' || c = ':' || c = '*' || c = '?' || c = '"' || c = '<' || c = '>' || c = '|' then String.blit "_" 0 name !i 1;
		incr i
	) name;
	let rdir = dir ^ "/res" in
	if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
	if not (Sys.file_exists rdir) then Unix.mkdir rdir 0o755;
	let ch = open_out_bin (rdir ^ "/" ^ name) in
	output_string ch data;
	close_out ch

let stack_init com use_add =
	Codegen.stack_context_init com "GLOBALS['%s']" "GLOBALS['%e']" "»spos" "»tmp" use_add null_pos

let init com cwd path def_type =
	let rec create acc = function
		| [] -> ()
		| d :: l ->
			let pdir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists pdir) then Unix.mkdir pdir 0o755;
			create (d :: acc) l
	in
	let dir = if cwd <> "" then com.file :: (cwd :: fst path) else com.file :: fst path; in
	create [] dir;
	let filename path =
		prefix_class com (match path with
		| [], "List" -> "HList";
		| _, s -> s) in
	(*--php-prefix*)
	let ch = open_out_bin (String.concat "/" dir ^ "/" ^ (filename path) ^ (if def_type = 0 then ".class" else if def_type = 1 then ".enum"  else if def_type = 2 then ".interface" else ".extern") ^ ".php") in
	let imports = Hashtbl.create 0 in
	Hashtbl.add imports (snd path) [fst path];
	{
		com = com;
		stack = stack_init com false;
		tabs = "";
		ch = ch;
		path = path;
		buf = Buffer.create (1 lsl 14);
		in_value = None;
		in_loop = false;
		in_instance_method = false;
		imports = imports;
		extern_required_paths = [];
		extern_classes_with_init = [];
		curclass = null_class;
		curmethod = "";
		locals = PMap.empty;
		inv_locals = PMap.empty;
		local_types = [];
		inits = [];
		constructor_block = false;
		dynamic_methods = [];
		all_dynamic_methods = [];
		is_call = false;
		cwd = cwd;
		inline_methods = [];
		nested_loops = 0;
		inline_index = 0;
		in_block = false;
		lib_path = match com.php_lib with None -> "lib" | Some s -> s;
	}
let unsupported msg p = error ("This expression cannot be generated to PHP: " ^ msg) p

let newline ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' | '{' | ':' | ' ' -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx ";\n%s" ctx.tabs

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

let inc_extern_path ctx path =
	let rec slashes n =
		if n = 0 then "" else ("../" ^ slashes (n-1))
	in
	let pre = if ctx.cwd = "" then ctx.lib_path ^ "/" else "" in
	match path with
		| ([],name) ->
		pre ^ (slashes (List.length (fst ctx.path))) ^ (prefix_class ctx.com name) ^ ".extern.php"
		| (pack,name) ->
		pre ^ (slashes (List.length (fst ctx.path))) ^ String.concat "/" pack ^ "/" ^ (prefix_class ctx.com name) ^ ".extern.php"

let close ctx =
	output_string ctx.ch "<?php\n";
	List.iter (fun path ->
		if path <> ctx.path then output_string ctx.ch ("require_once dirname(__FILE__).'/" ^ (inc_extern_path ctx path) ^ "';\n");
	) (List.rev ctx.extern_required_paths);
	output_string ctx.ch "\n";
	output_string ctx.ch (Buffer.contents ctx.buf);
	close_out ctx.ch

let save_locals ctx =
	let old = ctx.locals in
	let old_inv = ctx.inv_locals in
	(fun() -> ctx.locals <- old; ctx.inv_locals <- old_inv)

let define_local ctx l =
	let rec loop n =
	let name = (if n = 1 then s_ident_local l else s_ident_local (l ^ string_of_int n)) in
	if PMap.mem name ctx.inv_locals then
		loop (n+1)
	else begin
		ctx.locals <- PMap.add l name ctx.locals;
		ctx.inv_locals <- PMap.add name l ctx.inv_locals;
		name
	end
	in
	loop 1

let this ctx =
	if ctx.in_value <> None then "$»this" else "$this"

let escape_bin s =
	let b = Buffer.create 0 in
	for i = 0 to String.length s - 1 do
		match Char.code (String.unsafe_get s i) with
		| c when c = Char.code('\\') or c = Char.code('"') or c = Char.code('$') ->
			Buffer.add_string b "\\";
			Buffer.add_char b (Char.chr c)
		| c when c < 32 ->
			Buffer.add_string b (Printf.sprintf "\\x%.2X" c)
		| c ->
			Buffer.add_char b (Char.chr c)
	done;
	Buffer.contents b

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s ->
		print ctx "\"%s\"" (escape_bin s)
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx (this ctx)
	| TSuper -> spr ctx "ERROR /* unexpected call to super in gen_constant */"

let arg_is_opt c =
	match c with
	| Some _ -> true
	| None -> false

let s_funarg ctx arg t p o =
	let byref = if (String.length arg > 7 && String.sub arg 0 7 = "byref__") then "&" else "" in
	print ctx "%s$%s" byref (s_ident_local arg);
	if o then spr ctx " = null"
(*
	match c with
	| _, Some _ -> spr ctx " = null"
	| _, None -> ()
*)
let is_in_dynamic_methods ctx e s =
	List.exists (fun dm ->
		(* TODO: I agree, this is a mess ... but after hours of trials and errors I gave up; maybe in a calmer day *)
		((String.concat "." ((fst dm.mpath) @ ["#" ^ (snd dm.mpath)])) ^ "." ^ dm.mname) = (s_type_name e.etype ^ "." ^ s)
	) ctx.all_dynamic_methods

let is_dynamic_method f =
	(match f.cf_kind with
		| Var _ -> true
		| Method MethDynamic -> true
		| _ -> false)

let fun_block ctx f p =
	let e = (match f.tf_expr with { eexpr = TBlock [{ eexpr = TBlock _ } as e] } -> e | e -> e) in
	let e = List.fold_left (fun e (v,c) ->
		match c with
		| None | Some TNull -> e
		| Some c -> Codegen.concat (Codegen.set_default ctx.com v c p) e
	) e f.tf_args in
	if ctx.com.debug then begin
		Codegen.stack_block ctx.stack ctx.curclass ctx.curmethod e
	end else
		mk_block e

let rec gen_array_args ctx lst =
	match lst with
	| [] -> ()
	| h :: t ->
		spr ctx "[";
		gen_value ctx h;
		spr ctx "]";
		gen_array_args ctx t

and gen_call ctx e el =
	let rec genargs lst =
		(match lst with
		| [] -> ()
		| h :: [] ->
			spr ctx " = ";
			gen_value ctx h;
		| h :: t ->
			spr ctx "[";
			gen_value ctx h;
			spr ctx "]";
			genargs t)
	in
	match e.eexpr , el with
	| TConst TSuper , params ->
		(match ctx.curclass.cl_super with
		| None -> assert false
		| Some (c,_) ->
			spr ctx "parent::__construct(";
			concat ctx "," (gen_value ctx) params;
			spr ctx ")";
		);
	| TField ({ eexpr = TConst TSuper },name) , params ->
		(match ctx.curclass.cl_super with
		| None -> assert false
		| Some (c,_) ->
			print ctx "parent::%s(" (s_ident name);
			concat ctx "," (gen_value ctx) params;
			spr ctx ")";
		);
	| TLocal { v_name = "__set__" }, { eexpr = TConst (TString code) } :: el ->
		print ctx "$%s" code;
		genargs el;
	| TLocal { v_name = "__set__" }, e :: el ->
		gen_value ctx e;
		genargs el;
	| TLocal { v_name = "__setfield__" }, e :: (f :: el) ->
		gen_value ctx e;
		spr ctx "->{";
		gen_value ctx f;
		spr ctx "}";
		genargs el;
	| TLocal { v_name = "__field__" }, e :: ({ eexpr = TConst (TString code) } :: el) ->
		gen_value ctx e;
		spr ctx "->";
		spr ctx code;
		gen_array_args ctx el;
	| TLocal { v_name = "__field__" }, e :: (f :: el) ->
		gen_value ctx e;
		spr ctx "->";
		gen_value ctx f;
		gen_array_args ctx el;
	| TLocal { v_name = "__prefix__" }, [] ->
		(match ctx.com.php_prefix with
		| Some prefix ->
			print ctx "\"%s\"" prefix
		| None ->
			spr ctx "null")
	| TLocal { v_name = "__var__" }, { eexpr = TConst (TString code) } :: el ->
		print ctx "$%s" code;
		gen_array_args ctx el;
	| TLocal { v_name = "__var__" }, e :: el ->
		gen_value ctx e;
		gen_array_args ctx el;
	| TLocal { v_name = "__call__" }, { eexpr = TConst (TString code) } :: el ->
		spr ctx code;
		spr ctx "(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")";
	| TLocal { v_name = "__php__" }, [{ eexpr = TConst (TString code) }] ->
		(*--php-prefix*)
		spr ctx (prefix_init_replace ctx.com code)
	| TLocal { v_name = "__instanceof__" },  [e1;{ eexpr = TConst (TString t) }] ->
		gen_value ctx e1;
		print ctx " instanceof %s" t;
	| TLocal { v_name = "__physeq__" },  [e1;e2] ->
		spr ctx "(";
		gen_value ctx e1;
		spr ctx " === ";
		gen_value ctx e2;
		spr ctx ")"
	| TLocal _, []
	| TFunction _, []
	| TCall _, []
	| TParenthesis _, []
	| TBlock _, [] ->
		ctx.is_call <- true;
		spr ctx "call_user_func(";
		gen_value ctx e;
		ctx.is_call <- false;
		spr ctx ")";
	| TLocal _, el
	| TFunction _, el
	| TCall _, el
	| TParenthesis _, el
	| TBlock _, el ->
		ctx.is_call <- true;
		spr ctx "call_user_func_array(";
		gen_value ctx e;
		ctx.is_call <- false;
		spr ctx ", array(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx "))"
(*
	| TCall (x,_), el when (match x.eexpr with | TLocal _ -> false | _ -> true) ->
		ctx.is_call <- true;
		spr ctx "call_user_func_array(";
		gen_value ctx e;
		ctx.is_call <- false;
		spr ctx ", array(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx "))"
*)
	| _ ->
		ctx.is_call <- true;
		gen_value ctx e;
		ctx.is_call <- false;
		spr ctx "(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")";

and could_be_string_var s =
	s = "length"

and gen_uncertain_string_var ctx s e =
	match s with
	| "length" ->
		spr ctx "_hx_len(";
		gen_value ctx e;
		spr ctx ")"
	| _ ->
		gen_field_access ctx true e s;

and gen_string_var ctx s e =
	match s with
	| "length" ->
		spr ctx "strlen(";
		gen_value ctx e;
		spr ctx ")"
	| _ ->
		unsupported "gen_string_var " e.epos;

and gen_string_static_call ctx s e el =
	match s with
	| "fromCharCode" ->
		spr ctx "chr(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")";
	| _ -> unsupported "gen_string_static_call " e.epos;

and could_be_string_call s =
	s = "substr" || s = "substring" || s = "charAt" || s = "charCodeAt" || s = "indexOf" ||
	s = "lastIndexOf" || s = "split" || s = "toLowerCase" || s = "toString" || s = "toUpperCase"

and gen_string_call ctx s e el =
	match s with
	| "substr" ->
		spr ctx "_hx_substr(";
		gen_value ctx e;
		spr ctx ", ";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")"
	| "substring" ->
		spr ctx "_hx_substring(";
		gen_value ctx e;
		spr ctx ", ";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")"
	| "charAt" ->
		spr ctx "_hx_char_at(";
		gen_value ctx e;
		spr ctx ", ";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")"
	| "cca" ->
		spr ctx "ord(substr(";
		gen_value ctx e;
		spr ctx ",";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ",1))"
	| "charCodeAt" ->
		spr ctx "_hx_char_code_at(";
		gen_value ctx e;
		spr ctx ", ";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")"
	| "indexOf" ->
		spr ctx "_hx_index_of(";
		gen_value ctx e;
		spr ctx ", ";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")"
	| "lastIndexOf" ->
		spr ctx "_hx_last_index_of(";
		gen_value ctx e;
		spr ctx ", ";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ")"
	| "split" ->
		spr ctx "_hx_explode(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx ", ";
		gen_value ctx e;
		spr ctx ")"
	| "toLowerCase" ->
		spr ctx "strtolower(";
		gen_value ctx e;
		spr ctx ")"
	| "toUpperCase" ->
		spr ctx "strtoupper(";
		gen_value ctx e;
		spr ctx ")"
	| "toString" ->
		gen_value ctx e;
	| _ ->
		unsupported "gen_string_call" e.epos;

and gen_uncertain_string_call ctx s e el =
	spr ctx "_hx_string_call(";
	gen_value ctx e;
	print ctx ", \"%s\", array(" s;
	concat ctx ", " (gen_value ctx) el;
	spr ctx "))"

and gen_field_op ctx e =
	match e.eexpr with
	| TField (f,s) ->
		(match follow e.etype with
		| TFun _ ->
			gen_field_access ctx true f s
		| _ ->
			gen_value_op ctx e)
	| _ ->
		gen_value_op ctx e

and gen_value_op ctx e =
	match e.eexpr with
	| TBinop (op,_,_) when op = Ast.OpAnd || op = Ast.OpOr || op = Ast.OpXor ->
		gen_value ctx e;
	| _ ->
		gen_value ctx e

and is_static t =
	match follow t with
	| TAnon a -> (match !(a.a_status) with
		| Statics c -> true
		| _ -> false)
	| _ -> false

and gen_member_access ctx isvar e s =
	match follow e.etype with
	| TAnon a ->
		(match !(a.a_status) with
		| EnumStatics _
		| Statics _ -> print ctx "::%s%s" (if isvar then "$" else "") (s_ident s)
		| _ -> print ctx "->%s" (s_ident s))
	| _ -> print ctx "->%s" (s_ident s)

and gen_field_access ctx isvar e s =
	match e.eexpr with
	| TTypeExpr t ->
		spr ctx (s_path ctx (t_path t) false e.epos);
		gen_member_access ctx isvar e s
	| TLocal _ ->
		gen_expr ctx e;
		print ctx "->%s" (s_ident s)
	| TArray (e1,e2) ->
		spr ctx "_hx_array_get(";
		gen_value ctx e1;
		spr ctx ", ";
		gen_value ctx e2;
		spr ctx ")";
		gen_member_access ctx isvar e s
	| TBlock _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TNew _ ->
		spr ctx "_hx_deref(";
		ctx.is_call <- false;
		gen_value ctx e;
		spr ctx ")";
		gen_member_access ctx isvar e s
	| TCast (ec, _) when (match ec.eexpr with | TNew _ | TArrayDecl _ -> true | _ -> false) ->
		spr ctx "_hx_deref(";
		ctx.is_call <- false;
		gen_value ctx e;
		spr ctx ")";
		gen_member_access ctx isvar e s
	| _ ->
		gen_expr ctx e;
		gen_member_access ctx isvar e s

and gen_dynamic_function ctx isstatic name f params p =
	let old = ctx.in_value in
	let old_l = ctx.locals in
	let old_li = ctx.inv_locals in
	let old_t = ctx.local_types in
	ctx.in_value <- None;
	ctx.local_types <- List.map snd params @ ctx.local_types;
	let byref = if (String.length name > 9 && String.sub name 0 9 = "__byref__") then "&" else "" in
	print ctx "function %s%s(" byref name;
	concat ctx ", " (fun (v,c) ->
		let arg = define_local ctx v.v_name in
		s_funarg ctx arg v.v_type p (arg_is_opt c);
	) f.tf_args;
	spr ctx ") {";

	if (List.length f.tf_args) > 0 then begin
		if isstatic then
			print ctx " return call_user_func_array(self::$%s, array("  name
		else
			print ctx " return call_user_func_array($this->%s, array("  name;
		concat ctx ", " (fun (v,_) ->
			spr ctx ("$" ^ v.v_name)
		) f.tf_args;
		print ctx ")); }";
	end else if isstatic then
		print ctx " return call_user_func(self::$%s); }"  name
	else
		print ctx " return call_user_func($this->%s); }"  name;

	newline ctx;
	if isstatic then
		print ctx "public static $%s = null" name
	else
		print ctx "public $%s = null" name;
	ctx.in_value <- old;
	ctx.locals <- old_l;
	ctx.inv_locals <- old_li;
	ctx.local_types <- old_t

and gen_function ctx name f params p =
	let old = ctx.in_value in
	let old_l = ctx.locals in
	let old_li = ctx.inv_locals in
	let old_t = ctx.local_types in
	ctx.in_value <- None;
	ctx.local_types <- List.map snd params @ ctx.local_types;
	let byref = if (String.length name > 9 && String.sub name 0 9 = "__byref__") then "&" else "" in
	print ctx "function %s%s(" byref name;
	concat ctx ", " (fun (v,o) ->
		let arg = define_local ctx v.v_name in
		s_funarg ctx arg v.v_type p (arg_is_opt o);
	) f.tf_args;
	print ctx ") ";
	gen_expr ctx (fun_block ctx f p);
	ctx.in_value <- old;
	ctx.locals <- old_l;
	ctx.inv_locals <- old_li;
	ctx.local_types <- old_t


and gen_inline_function ctx f hasthis p =
	ctx.nested_loops <- ctx.nested_loops - 1;
	let old = ctx.in_value in
	let old_l = ctx.locals in
	let old_li = ctx.inv_locals in
	let old_t = ctx.local_types in
	ctx.in_value <- Some "closure";

	let args a = List.map (fun (v,_) -> v.v_name) a in
	let arguments = ref [] in

	if hasthis then begin arguments := "this" :: !arguments end;

	PMap.iter (fun n _ -> arguments := !arguments @ [n]) old_li;

	spr ctx "array(new _hx_lambda(array(";

	let c = ref 0 in

	List.iter (fun a ->
		if !c > 0 then spr ctx ", ";
		incr c;
		print ctx "&$%s" a;
	) (remove_internals !arguments);

	spr ctx "), \"";

	spr ctx (inline_function ctx (args f.tf_args) hasthis (fun_block ctx f p));
	print ctx "\"), 'execute')";

	ctx.in_value <- old;
	ctx.locals <- old_l;
	ctx.inv_locals <- old_li;
	ctx.local_types <- old_t;
	ctx.nested_loops <- ctx.nested_loops + 1;

and unset_locals ctx old_l =
	let lst = ref [] in
	PMap.iter (fun n _ ->
		if not (PMap.exists n old_l) then
			lst := ["$" ^  n] @ !lst;
	) ctx.inv_locals;
	if (List.length !lst) > 0 then begin
		newline ctx;
		spr ctx "unset(";
		concat ctx "," (fun (s) -> spr ctx s; ) !lst;
		spr ctx ")"
	end

and gen_while_expr ctx e =
	let old_loop = ctx.in_loop in
	ctx.in_loop <- true;
	let old_nested_loops = ctx.nested_loops in
	ctx.nested_loops <- 1;
	let old_l = ctx.inv_locals in
	let b = save_locals ctx in
	(match e.eexpr with
	| TBlock (el) ->
		List.iter (fun e -> newline ctx; gen_expr ctx e) el;
	| _ ->
		newline ctx;
		gen_expr ctx e);
	unset_locals ctx old_l;
	b();
	ctx.nested_loops <- old_nested_loops;
	ctx.in_loop <- old_loop

and gen_expr ctx e =
	let in_block = ctx.in_block in
	ctx.in_block <- false;
	let restore_in_block ctx inb =
		if inb then ctx.in_block <- true
	in
	match e.eexpr with
	| TConst c ->
		gen_constant ctx e.epos c
	| TLocal v ->
		spr ctx ("$" ^ (try PMap.find v.v_name ctx.locals with Not_found -> (s_ident_local v.v_name)))
	| TEnumField (en,s) ->
		(match (try PMap.find s en.e_constrs with Not_found -> error ("Unknown local " ^ s) e.epos).ef_type with
		| TFun (args,_) -> print ctx "%s::%s" (s_path ctx en.e_path en.e_extern e.epos) (s_ident s)
		| _ -> print ctx "%s::$%s" (s_path ctx en.e_path en.e_extern e.epos) (s_ident s))
	| TArray (e1,e2) ->
		(match e1.eexpr with
		| TCall _
		| TBlock _
		| TParenthesis _
		| TArrayDecl _ ->
			spr ctx "_hx_array_get(";
			gen_value ctx e1;
			spr ctx ", ";
			gen_value ctx e2;
			spr ctx ")";
		| TCast (ec, _) when (match ec.eexpr with | TArrayDecl _  | TBlock _ -> true | _ -> false) ->
			spr ctx "_hx_array_get(";
			gen_value ctx e1;
			spr ctx ", ";
			gen_value ctx e2;
			spr ctx ")";
		| _ ->
			gen_value ctx e1;
			spr ctx "[";
			gen_value ctx e2;
			spr ctx "]");
	| TBinop (op,e1,e2) ->
		(* these operators are non-assoc in php, let let's make sure to separate them with parenthesises *)
		let non_assoc = function
			| (Ast.OpEq | Ast.OpNotEq | Ast.OpGt | Ast.OpGte | Ast.OpLt | Ast.OpLte) -> true
			| _ -> false
		in
		(match e1.eexpr with
		| TBinop (op2,_,_) when non_assoc op && non_assoc op2 ->
			gen_expr ctx { e with eexpr = TBinop (op,mk (TParenthesis e1) e1.etype e1.epos,e2) }
		| _ ->
		let leftside e =
			(match e.eexpr with
			| TArray(te1, te2) ->
				gen_value ctx te1;
				spr ctx "->»a[";
				gen_value ctx te2;
				spr ctx "]";
			| _ ->
				gen_field_op ctx e1;) in
		let leftsidec e =
			(match e.eexpr with
			| TArray(te1, te2) ->
				gen_value ctx te1;
				spr ctx "->»a[";
				gen_value ctx te2;
				spr ctx "]";
			| TField (e1,s) ->
				gen_field_access ctx true e1 s
			| _ ->
				gen_field_op ctx e1;) in
		let leftsidef e =
			(match e.eexpr with
			| TField (e1,s) ->
				gen_field_access ctx true e1 s;
			| _ ->
				gen_field_op ctx e1;
				) in
		(match op with
		| Ast.OpMod ->
			spr ctx "_hx_mod(";
			gen_value_op ctx e1;
			spr ctx ", ";
			gen_value_op ctx e2;
			spr ctx ")";
		| Ast.OpAssign ->
			(match e1.eexpr with
			| TArray(te1, te2) when (match te1.eexpr with | TCall _ | TParenthesis _ -> true | _ -> false) ->
				spr ctx "_hx_array_assign(";
				gen_value ctx te1;
				spr ctx ", ";
				gen_value ctx te2;
				spr ctx ", ";
				gen_value_op ctx e2;
				spr ctx ")";
			| _ ->
				leftsidef e1;
				spr ctx " = ";
				gen_value_op ctx e2;
			)
		| Ast.OpAssignOp(Ast.OpAdd) when (is_uncertain_expr e1 && is_uncertain_expr e2) ->
			(match e1.eexpr with
			| TArray(te1, te2) ->
				spr ctx "_hx_array_assign($»t1 = ";
				gen_value ctx te1;
				spr ctx ", $»t2 = ";
				gen_value ctx te2;
				spr ctx ", $»t1->»a[$»t2] + ";
				gen_value_op ctx e2;
				spr ctx ")";
			| _ ->
				leftside e1;
				spr ctx " = ";
				spr ctx "_hx_add(";
				gen_value_op ctx e1;
				spr ctx ", ";
				gen_value_op ctx e2;
				spr ctx ")";
			)
		| Ast.OpAssignOp(Ast.OpAdd) when (is_string_expr e1 || is_string_expr e2) ->
			leftside e1;
			spr ctx " .= ";
			gen_value_op ctx (as_string_expr ctx e2);
		| Ast.OpAssignOp(Ast.OpShl) ->
			leftside e1;
			spr ctx " <<= ";
			gen_value_op ctx e2;
		| Ast.OpAssignOp(Ast.OpUShr) ->
			leftside e1;
			spr ctx " = ";
			spr ctx "_hx_shift_right(";
			gen_value_op ctx e1;
			spr ctx ", ";
			gen_value_op ctx e2;
			spr ctx ")";
		| Ast.OpAssignOp(Ast.OpMod) ->
			leftside e1;
			spr ctx " = ";
			spr ctx "_hx_mod(";
			gen_value_op ctx e1;
			spr ctx ", ";
			gen_value_op ctx e2;
			spr ctx ")";
		| Ast.OpAssignOp(_) ->
			leftsidec e1;
			print ctx " %s " (Ast.s_binop op);
			gen_value_op ctx e2;
		| Ast.OpAdd when (is_uncertain_expr e1 && is_uncertain_expr e2) ->
			spr ctx "_hx_add(";
			gen_value_op ctx e1;
			spr ctx ", ";
			gen_value_op ctx e2;
			spr ctx ")";
		| Ast.OpAdd when (is_string_expr e1 || is_string_expr e2) ->
			gen_value_op ctx (as_string_expr ctx e1);
			spr ctx " . ";
			gen_value_op ctx (as_string_expr ctx e2);
		| Ast.OpShl ->
			gen_value_op ctx e1;
			spr ctx " << ";
			gen_value_op ctx e2;
		| Ast.OpUShr ->
			spr ctx "_hx_shift_right(";
			gen_value_op ctx e1;
			spr ctx ", ";
			gen_value_op ctx e2;
			spr ctx ")";
		| Ast.OpNotEq
		| Ast.OpEq ->
			let s_op = if op = Ast.OpNotEq then " != " else " == " in
			let s_phop = if op = Ast.OpNotEq then " !== " else " === " in
			let se1 = s_expr_name e1 in
			let se2 = s_expr_name e2 in
			if
				e1.eexpr = TConst (TNull)
				|| e2.eexpr = TConst (TNull)
			then begin
				(match e1.eexpr with
				| TField (f, s) when is_anonym_expr e1 || is_unknown_expr e1 ->
					spr ctx "_hx_field(";
					gen_value ctx f;
					print ctx ", \"%s\")" s;
				| _ ->
					gen_field_op ctx e1;
				);
				spr ctx s_phop;

				(match e2.eexpr with
				| TField (f, s) when is_anonym_expr e2 || is_unknown_expr e2 ->
					spr ctx "_hx_field(";
					gen_value ctx f;
					print ctx ", \"%s\")" s;
				| _ ->
					gen_field_op ctx e2);
			end else if
					((se1 = "Int" || se1 = "Null<Int>") && (se2 = "Int" || se2 = "Null<Int>"))
					|| ((se1 = "Float" || se1 = "Null<Float>") && (se2 = "Float" || se2 = "Null<Float>"))
			then begin
				gen_field_op ctx e1;
				spr ctx s_phop;
				gen_field_op ctx e2;
			end else if
				   ((se1 = "Int" || se1 = "Float" || se1 = "Null<Int>" || se1 = "Null<Float>")
				&& (se1 = "Int" || se1 = "Float" || se1 = "Null<Int>" || se1 = "Null<Float>"))
				|| (is_unknown_expr e1 && is_unknown_expr e2)
				|| is_anonym_expr e1
				|| is_anonym_expr e2
			then begin
				if op = Ast.OpNotEq then spr ctx "!";
				spr ctx "_hx_equal(";
				gen_field_op ctx e1;
				spr ctx ", ";
				gen_field_op ctx e2;
				spr ctx ")";
			end else if
				(
					   se1 == se2
					|| (match e1.eexpr with | TConst _ | TLocal _ | TArray _  | TNew _ -> true | _ -> false)
					|| (match e2.eexpr with | TConst _ | TLocal _ | TArray _  | TNew _ -> true | _ -> false)
					|| is_string_expr e1
					|| is_string_expr e2
					|| is_anonym_expr e1
					|| is_anonym_expr e2
					|| is_unknown_expr e1
					|| is_unknown_expr e2
				)
				&& (type_string (follow e1.etype)) <> "Dynamic"
				&& (type_string (follow e2.etype)) <> "Dynamic"
			then begin
				gen_field_op ctx e1;
				spr ctx s_phop;
				gen_field_op ctx e2;
			end else begin
				gen_field_op ctx e1;
				spr ctx s_op;
				gen_field_op ctx e2;
			end
		| _ ->
			leftside e1;
			print ctx " %s " (Ast.s_binop op);
			gen_value_op ctx e2;
		));
	| TField (e1,s)
	| TClosure (e1,s) ->
		(match follow e.etype with
		| TFun (args, _) ->
			(if ctx.is_call then begin
				gen_field_access ctx false e1 s
	  		end else if is_in_dynamic_methods ctx e1 s then begin
	  			gen_field_access ctx true e1 s;
	  		end else begin
				let ob ex =
					(match ex with
					| TTypeExpr t ->
						print ctx "\"";
						spr ctx (s_path ctx (t_path t) false e1.epos);
						print ctx "\""
					| _ ->
						gen_expr ctx e1) in

				spr ctx "(isset(";
				gen_field_access ctx true e1 s;
				spr ctx ") ? ";
				gen_field_access ctx true e1 s;
				spr ctx ": array(";
				ob e1.eexpr;
				print ctx ", \"%s\"))" (s_ident s);

			end)
		| TMono _ ->
			if ctx.is_call then
				gen_field_access ctx false e1 s
			else
				gen_uncertain_string_var ctx s e1
		| _ ->
			if is_string_expr e1 then
				gen_string_var ctx s e1
			else if is_uncertain_expr e1 then
				gen_uncertain_string_var ctx s e1
			else
				gen_field_access ctx true e1 s
		)

	| TTypeExpr t ->
		print ctx "_hx_qtype(\"%s\")" (s_path_haxe (t_path t))
	| TParenthesis e ->
		(match e.eexpr with
		| TParenthesis _
		| TReturn _ ->
			gen_value ctx e;
		| _ ->
			spr ctx "(";
			gen_value ctx e;
			spr ctx ")"
		);
	| TReturn eo ->
		(match eo with
		| None ->
			spr ctx "return"
		| Some e when (match follow e.etype with TEnum({ e_path = [],"Void" },[]) | TAbstract ({ a_path = [],"Void" },[]) -> true | _ -> false) ->
			gen_value ctx e;
			newline ctx;
			spr ctx "return"
		| Some e ->
			(match e.eexpr with
			| TThrow _ -> ()
			| _ -> spr ctx "return ");
			gen_value ctx e;
			);
	| TBreak ->
		if ctx.in_loop then spr ctx "break" else print ctx "break %d" ctx.nested_loops
	| TContinue ->
		if ctx.in_loop then spr ctx "continue" else print ctx "continue %d" ctx.nested_loops
	| TBlock [] ->
		spr ctx ""
	| TBlock el ->
		let old_l = ctx.inv_locals in
		let b = save_locals ctx in
		print ctx "{";
		let bend = open_block ctx in
		let cb = (
			if not ctx.constructor_block then
				(fun () -> ())
			else begin
				ctx.constructor_block <- false;
				if List.length ctx.dynamic_methods > 0 then newline ctx else spr ctx " ";
				List.iter (fun (f) ->
					let name = f.cf_name in
					match f.cf_expr with
					| Some { eexpr = TFunction fd } ->
						print ctx "if(!isset($this->%s)) $this->%s = " name name;
						gen_inline_function ctx fd true e.epos;
						newline ctx;
					| _ -> ()
				) ctx.dynamic_methods;
				if Codegen.constructor_side_effects e then begin
					print ctx "if(!%s::$skip_constructor) {" (s_path ctx (["php"],"Boot") false e.epos);
					(fun() -> print ctx "}")
				end else
					(fun() -> ());
			end) in
		let remaining = ref (List.length el) in
		let build e =
			(match e.eexpr with
			| TBlock [] -> ()
			| _ -> newline ctx);
			if (in_block && !remaining = 1) then begin
				(match e.eexpr with
				| TIf _
				| TSwitch _
				| TThrow _
				| TWhile _
				| TFor _
				| TMatch _
				| TTry _
				| TBreak
				| TBlock _ ->
					restore_in_block ctx in_block;
					gen_expr ctx e;
					unset_locals ctx old_l;
				| TReturn (Some e1) ->
					(match e1.eexpr with
					| TIf _
					| TSwitch _
					| TThrow _
					| TWhile _
					| TFor _
					| TMatch _
					| TTry _
					| TBlock _ -> ()
					| _ ->
						spr ctx "return "
					);
					gen_expr ctx e1;
				| _ ->
					spr ctx "return ";
					gen_value ctx e;
				)
			end else begin
				gen_expr ctx e;
			end;
			decr remaining;
		in
		List.iter build el;

		if ctx.in_loop then begin
			unset_locals ctx old_l;
		end;
		bend();
		newline ctx;

		cb();
		print ctx "}";
		b();
	| TFunction f ->
		let old = ctx.in_value, ctx.in_loop in
		let old_meth = ctx.curmethod in
		ctx.in_value <- None;
		ctx.in_loop <- false;
		ctx.curmethod <- ctx.curmethod ^ "@" ^ string_of_int (Lexer.get_error_line e.epos);
		gen_inline_function ctx f false e.epos;
		ctx.curmethod <- old_meth;
		ctx.in_value <- fst old;
		ctx.in_loop <- snd old;
	| TCall (ec,el) ->
		(match ec.eexpr with
		| TArray _ ->
			spr ctx "call_user_func_array(";
			gen_value ctx ec;
			spr ctx ", array(";
			concat ctx ", " (gen_value ctx) el;
			spr ctx "))";
		| TField (ef,s) when is_static ef.etype && is_string_expr ef ->
			gen_string_static_call ctx s ef el
		| TField (ef,s) when is_string_expr ef ->
			gen_string_call ctx s ef el
		| TField (ef,s) when is_anonym_expr ef && could_be_string_call s ->
			gen_uncertain_string_call ctx s ef el
		| _ ->
			gen_call ctx ec el);
	| TArrayDecl el ->
		spr ctx "new _hx_array(array(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx "))";
	| TThrow e ->
		spr ctx "throw new HException(";
		gen_value ctx e;
		spr ctx ")";
	| TVars [] ->
		()
	| TVars vl ->
		spr ctx "$";
		concat ctx ("; $") (fun (v,e) ->
			let restore = save_locals ctx in
			let n = define_local ctx v.v_name in
			let restore2 = save_locals ctx in
			restore();
			(match e with
			| None ->
				print ctx "%s = null" (s_ident_local n)
			| Some e ->
				print ctx "%s = " (s_ident_local n);
				gen_value ctx e);
			restore2()
		) vl;
	| TNew (c,_,el) ->
		(match c.cl_path, el with
		| ([], "String"), _ ->
			concat ctx "" (gen_value ctx) el
		| ([], "Array"), el ->
			spr ctx "new _hx_array(array(";
			concat ctx ", " (gen_value ctx) el;
			spr ctx "))"
		| (_, _), _ ->
			print ctx "new %s(" (s_path ctx c.cl_path c.cl_extern e.epos);
			let count = ref (-1) in
			concat ctx ", " (fun e ->
				incr count;
				match c.cl_constructor with
				| Some f ->
					gen_value ctx e;
				| _ -> ();
			) el;
			spr ctx ")")
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_value ctx (parent cond);
		spr ctx " ";
		restore_in_block ctx in_block;
		gen_expr ctx (mk_block e);
		(match eelse with
		| None -> ()
		| Some e when e.eexpr = TConst(TNull) -> ()
		| Some e ->
			spr ctx " else ";
			restore_in_block ctx in_block;
			gen_expr ctx (mk_block e));
	| TUnop (op,Ast.Prefix,e) ->
		(match e.eexpr with
		| TArray(te1, te2) ->
			(match op with
			| Increment ->
				spr ctx "_hx_array_increment(";
				gen_value ctx te1;
				spr ctx ",";
				gen_value ctx te2;
				spr ctx ")";
			| Decrement ->
				spr ctx "_hx_array_decrement(";
				gen_value ctx te1;
				spr ctx ",";
				gen_value ctx te2;
				spr ctx ")";
(*
				let t = define_local ctx "»t" in
				gen_value ctx te1;
				print ctx "[%s = " t;
				gen_value ctx te2;
				spr ctx "] = ";
				gen_value ctx te1;
				print ctx "[%s]" t;
				print ctx "[%s]" t;
*)
			| _ ->
				spr ctx (Ast.s_unop op);
				gen_value ctx te1;
				spr ctx "[";
				gen_value ctx te2;
				spr ctx "]";
			);
		| TField (e1,s) ->
			spr ctx (Ast.s_unop op);
			gen_field_access ctx true e1 s
		| _ ->
			spr ctx (Ast.s_unop op);
			gen_value ctx e)
	| TUnop (op,Ast.Postfix,e) ->
		(match e.eexpr with
		| TArray(te1, te2) ->
			gen_value ctx te1;
			spr ctx "->»a[";
			gen_value ctx te2;
			spr ctx "]";
		| TField (e1,s) ->
			gen_field_access ctx true e1 s
		| _ ->
			gen_value ctx e);
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		let old = save_locals ctx in
		spr ctx "while";
		gen_value ctx (parent cond);
		spr ctx " {";
		let bend = open_block ctx in
		gen_while_expr ctx e;
		bend();
		newline ctx;
		spr ctx "}";
		old()
	| TWhile (cond,e,Ast.DoWhile) ->
		let old = save_locals ctx in
		spr ctx "do {";
		let bend = open_block ctx in
		gen_while_expr ctx e;
		bend();
		newline ctx;
		spr ctx "} while";
		gen_value ctx (parent cond);
		old()
	| TObjectDecl fields ->
		spr ctx "_hx_anonymous(array(";
		concat ctx ", " (fun (f,e) -> print ctx "\"%s\" => " (escape_bin f); gen_value ctx e) fields;
		spr ctx "))"
	| TFor (v,it,e) ->
		let b = save_locals ctx in
		let tmp = define_local ctx "»it" in
		let v = define_local ctx v.v_name in
		(match it.eexpr with
		| TCall (e,_) ->
			(match e.eexpr with
			| TField (e,f) ->
				spr ctx "if(null == ";
				gen_value ctx e;
				spr ctx ") throw new HException('null iterable')";
				newline ctx;
			| _ ->
				());
		| _ -> ()
		);
		print ctx "$%s = " tmp;
		gen_value ctx it;
		newline ctx;
		print ctx "while($%s->hasNext()) {" tmp;
		let bend = open_block ctx in
		newline ctx;
		print ctx "$%s = $%s->next()" v tmp;
		gen_while_expr ctx e;
		bend();
		newline ctx;
		spr ctx "}";
		b();
	| TTry (e,catchs) ->
		spr ctx "try ";
		restore_in_block ctx in_block;
		gen_expr ctx (mk_block e);
		let old = save_locals ctx in
		let ex = define_local ctx "»e" in
		print ctx "catch(Exception $%s) {" ex;
		let bend = open_block ctx in
		let first = ref true in
		let catchall = ref false in
		let evar = define_local ctx "_ex_" in
		newline ctx;
		print ctx "$%s = ($%s instanceof HException) ? $%s->e : $%s" evar ex ex ex;
		old();
		List.iter (fun (v,e) ->
			let ev = define_local ctx v.v_name in
			newline ctx;

			let b = save_locals ctx in
			if not !first then spr ctx "else ";
			(match follow v.v_type with
			| TEnum (te,_) -> (match te.e_path with
				| [], "Bool"   -> print ctx "if(is_bool($%s = $%s))" ev evar
				| _ -> print ctx "if(($%s = $%s) instanceof %s)" ev evar (s_path ctx te.e_path te.e_extern e.epos));
				restore_in_block ctx in_block;
				gen_expr ctx (mk_block e);
			| TInst (tc,_) -> (match tc.cl_path with
				| [], "Int"	-> print ctx "if(is_int($%s = $%s))"		ev evar
				| [], "Float"  -> print ctx "if(is_numeric($%s = $%s))"	ev evar
				| [], "String" -> print ctx "if(is_string($%s = $%s))"	ev evar
				| [], "Array"  -> print ctx "if(($%s = $%s) instanceof _hx_array)"	ev evar
				| _ -> print ctx "if(($%s = $%s) instanceof %s)"    ev evar (s_path ctx tc.cl_path tc.cl_extern e.epos));
				restore_in_block ctx in_block;
				gen_expr ctx (mk_block e);
			| TAbstract (ta,_) -> (match ta.a_path with
				| [], "Int"	-> print ctx "if(is_int($%s = $%s))"		ev evar
				| [], "Float"  -> print ctx "if(is_numeric($%s = $%s))"	ev evar
				| [], "Bool"   -> print ctx "if(is_bool($%s = $%s))" ev evar
				| _ -> print ctx "if(($%s = $%s) instanceof %s)"    ev evar (s_path ctx ta.a_path false e.epos));
				restore_in_block ctx in_block;
				gen_expr ctx (mk_block e);
			| TFun _
			| TLazy _
			| TType _
			| TAnon _ ->
				assert false
			| TMono _
			| TDynamic _ ->
				catchall := true;
				if not !first then spr ctx "{ ";
				print ctx "$%s = $%s" ev evar;
				newline ctx;
				restore_in_block ctx in_block;
				gen_expr ctx (mk_block e);
				if not !first then spr ctx "}"
			);
			b();
			first := false;
		) catchs;
		if not !catchall then
			print ctx " else throw $%s;" ex;
		bend();
		newline ctx;
		spr ctx "}"
	| TMatch (e,_,cases,def) ->
		let b = save_locals ctx in
		let tmp = define_local ctx "»t" in
		print ctx "$%s = " tmp;
		gen_value ctx e;
		newline ctx;
		print ctx "switch($%s->index) {" tmp;
		let old_loop = ctx.in_loop in
		ctx.in_loop <- false;
		ctx.nested_loops <- ctx.nested_loops + 1;
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
				let l = List.fold_left (fun acc v -> incr n; match v with None -> acc | Some v -> (v.v_name,v.v_type,!n) :: acc) [] l in
				match l with
				| [] -> ()
				| l ->
					concat ctx "; " (fun (v,t,n) ->
						let v = define_local ctx v in
						print ctx "$%s = $%s->params[%d]" v tmp n;
					) l;
					newline ctx);
			restore_in_block ctx in_block;
			gen_expr ctx (mk_block e);
			print ctx "break";
			newline ctx;
			b()
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			restore_in_block ctx in_block;
			gen_expr ctx (mk_block e);
			print ctx "break";
			newline ctx;
		);
		ctx.nested_loops <- ctx.nested_loops - 1;
		ctx.in_loop <- old_loop;
		spr ctx "}";
		b()
	| TSwitch (e,cases,def) ->
		let old_loop = ctx.in_loop in
		ctx.in_loop <- false;
		ctx.nested_loops <- ctx.nested_loops + 1;
		let old = save_locals ctx in
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
			restore_in_block ctx in_block;
			gen_expr ctx (mk_block e2);
			print ctx "break";
			newline ctx;
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			restore_in_block ctx in_block;
			gen_expr ctx (mk_block e);
			print ctx "break";
			newline ctx;
		);
		spr ctx "}";
		ctx.nested_loops <- ctx.nested_loops - 1;
		ctx.in_loop <- old_loop;
		old()
	| TCast (e,None) ->
		gen_expr ctx e
	| TCast (e1,Some t) ->
		let mk_texpr = function
			| TClassDecl c -> TAnon { a_fields = PMap.empty; a_status = ref (Statics c) }
			| TEnumDecl e -> TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }
			| TAbstractDecl a -> TAnon { a_fields = PMap.empty; a_status = ref (AbstractStatics a) }
			| TTypeDecl _ -> assert false
		in
		spr ctx "_hx_cast(";
		gen_expr ctx e1;
		spr ctx ", ";
		gen_expr ctx (mk (TTypeExpr t) (mk_texpr t) e1.epos);
		spr ctx ")"

and argument_list_from_locals include_this in_var l =
	let lst = ref [] in
	if (include_this && in_var) then lst := "»this" :: !lst
	else if include_this then lst := "this" :: !lst;
	PMap.iter (fun n _ ->
		lst := !lst @ [n];
	) l;
	!lst

and remove_internals args =
	List.filter (fun a -> a = "»this" or '»' <> String.get a 0) args;

and inline_block ctx e =
		let index = ctx.inline_index in
		ctx.inline_index <- ctx.inline_index + 1;
		let block = {
			iname = (s_path ctx ctx.curclass.cl_path ctx.curclass.cl_extern ctx.curclass.cl_pos) ^ "_" ^ string_of_int index;
			iindex = index;
			ihasthis = ctx.in_instance_method; (* param this *)
			iarguments = [];
			iexpr = e;
			ilocals = ctx.locals;
			iin_block = true;
			iinv_locals = ctx.inv_locals;
		} in

		print ctx "%s(" block.iname;
		let in_value = (match ctx.in_value with Some _ -> true | _ -> false) in
		(match remove_internals (argument_list_from_locals ctx.in_instance_method in_value ctx.locals) with
		| [] -> ()
		| l -> print ctx "$%s" (String.concat ", $" l)
		);
		spr ctx ")";

		ctx.inline_methods <- ctx.inline_methods @ [block]

and inline_function ctx args hasthis e =
		let index = ctx.inline_index in
		ctx.inline_index <- ctx.inline_index + 1;
		let block = {
			iname = (s_path ctx ctx.curclass.cl_path ctx.curclass.cl_extern ctx.curclass.cl_pos) ^ "_" ^ string_of_int index;
			iindex = index;
			ihasthis = hasthis; (* param this *)
			iarguments = args;
			iexpr = e;
			ilocals = ctx.locals;
			iin_block = false;
			iinv_locals = ctx.inv_locals;
		} in

		ctx.inline_methods <- ctx.inline_methods @ [block];
		block.iname

and canbe_ternary_param e =
	match e.eexpr with
	| TTypeExpr _
	| TConst _
	| TLocal _
	| TEnumField _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TCall _
	| TUnop _
	| TNew _
	| TCast (_, _)
	| TBlock [_] ->
		true
	| TIf (_,e,eelse) ->
		cangen_ternary e eelse
	| _ ->
		false

and cangen_ternary e eelse =
	match eelse with
	| Some other ->
		(canbe_ternary_param e) && (canbe_ternary_param other)
	| _ ->
		false

and gen_value ctx e =
	match e.eexpr with
	| TTypeExpr _
	| TConst _
	| TLocal _
	| TEnumField _
	| TArray _
	| TBinop _
	| TField _
	| TClosure _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TCall _
	| TUnop _
	| TNew _
	| TFunction _ ->
		gen_expr ctx e
	| TBlock [] ->
		()
	| TCast (e, _)
	| TBlock [e] ->
		gen_value ctx e
	| TIf (cond,e,eelse) when (cangen_ternary e eelse) ->
		spr ctx "(";
		gen_value ctx cond;
		spr ctx " ? ";
		gen_value ctx e;

		(match eelse with
		| Some e ->
			spr ctx " : ";
			gen_value ctx e
		| _ ->());
		spr ctx ")";

(*
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_value ctx (parent cond);
		spr ctx " ";
		restore_in_block ctx in_block;
		gen_expr ctx (mk_block e);
		(match eelse with
		| None -> ()
		| Some e when e.eexpr = TConst(TNull) -> ()
		| Some e ->
			spr ctx " else ";
			restore_in_block ctx in_block;
			gen_expr ctx (mk_block e));
*)
	| TBlock _
	| TBreak
	| TContinue
	| TVars _
	| TReturn _
	| TWhile _
	| TThrow _
	| TSwitch _
	| TFor _
	| TMatch _
	| TIf _
	| TTry _ ->
		inline_block ctx e

let rec is_instance_method_defined cls m =
	if PMap.exists m cls.cl_fields then
		true
	else
		match cls.cl_super with
		| Some (scls, _) ->
			is_instance_method_defined scls m
		| None ->
			false

let is_method_defined ctx m static =
	if static then
		PMap.exists m ctx.curclass.cl_statics
	else
		is_instance_method_defined ctx.curclass m

let generate_self_method ctx rights m static setter =
	if setter then (
		if static then
			print ctx "%s function %s($v) { return call_user_func(self::$%s, $v); }" rights (s_ident m) (s_ident m)
		else
			print ctx "%s function %s($v) { return call_user_func($this->%s, $v); }" rights (s_ident m) (s_ident m)
	) else (
		if static then
			print ctx "%s function %s() { return call_user_func(self::$%s); }" rights (s_ident m) (s_ident m)
		else
			print ctx "%s function %s() { return call_user_func($this->%s); }" rights (s_ident m) (s_ident m)
	);
	newline ctx

let gen_assigned_value ctx eo =	match eo with
	| Some ({eexpr = TConst _} as e) ->
		print ctx " = ";
		gen_value ctx e
	| _ ->
		()

let generate_field ctx static f =
	newline ctx;
	ctx.locals <- PMap.empty;
	ctx.inv_locals <- PMap.empty;
	ctx.in_instance_method <- not static;
	let rights = if static then "static" else "public" in
	let p = ctx.curclass.cl_pos in
	match f.cf_expr with
	| Some { eexpr = TFunction fd } ->
		if f.cf_name = "__construct" then
			ctx.curmethod <- "new"
		else
			ctx.curmethod <- f.cf_name;
		spr ctx (rights ^ " ");
		if is_dynamic_method f then
			gen_dynamic_function ctx static (s_ident f.cf_name) fd f.cf_params p
		else
			gen_function ctx (s_ident f.cf_name) fd f.cf_params p
	| _ ->
		if ctx.curclass.cl_interface then
			match follow f.cf_type, f.cf_kind with
			| TFun (args,r), Method _ ->
				print ctx "function %s(" (s_ident f.cf_name);
				concat ctx ", " (fun (arg,o,t) ->
					s_funarg ctx arg t p o;
				) args;
				print ctx ")";
			| _ -> spr ctx "//"; ()
		else if
			(match f.cf_kind with
			| Var v ->
				(match v.v_read, v.v_write with
				| AccCall m1, AccCall m2 ->
					if not (is_method_defined ctx m1 static) then (
						generate_self_method ctx rights m1 static false;
						print ctx "%s $%s" rights (s_ident m1);
						if not (is_method_defined ctx m2 static) then
							newline ctx);
					if not (is_method_defined ctx m2 static) then (
						generate_self_method ctx rights m2 static true;
						print ctx "%s $%s" rights (s_ident m2);
						newline ctx);
					false
				| AccCall m, _ ->
					if not (is_method_defined ctx m static) then generate_self_method ctx rights m static false;
					print ctx "%s $%s" rights (s_ident f.cf_name);
					gen_assigned_value ctx f.cf_expr;
					true
				| _, AccCall m ->
					if not (is_method_defined ctx m static) then generate_self_method ctx rights m static true;
					print ctx "%s $%s" rights (s_ident f.cf_name);
					gen_assigned_value ctx f.cf_expr;
					true
				| _ ->
					false)
			| _ -> false) then
				()
		else begin
			let name = s_ident f.cf_name in
			if static then
				(match f.cf_kind with
				| Var _ ->
					(match follow f.cf_type with
					| TFun _
					| TDynamic _ ->
						print ctx "static function %s() { $»args = func_get_args(); return call_user_func_array(self::$%s, $»args); }" name name;
						newline ctx;
					| _ ->
						()
					)
				| _ ->
					()
				);
			print ctx "%s $%s" rights name;
			gen_assigned_value ctx f.cf_expr
		end

let generate_static_field_assign ctx path f =
	let p = ctx.curclass.cl_pos in
	if not ctx.curclass.cl_interface then
		(match f.cf_expr with
		| None -> ()
		| Some e ->
			match e.eexpr with
			| TConst _ -> ()
			| TFunction fd ->
				(match f.cf_kind with
				| Var _ when
						(match follow f.cf_type with
						| TFun _
						| TDynamic _ ->
							true;
						| _ ->
							false) ->
					newline ctx;
					print ctx "%s::$%s = " (s_path ctx path false p) (s_ident f.cf_name);
					gen_value ctx e
				| Method MethDynamic ->
					newline ctx;
					print ctx "%s::$%s = " (s_path ctx path false p) (s_ident f.cf_name);
					gen_value ctx e
				| _ -> ())
			| _ ->
				newline ctx;
				print ctx "%s::$%s = " (s_path ctx path false p) (s_ident f.cf_name);
				gen_value ctx e)

let rec super_has_dynamic c =
	match c.cl_super with
	| None -> false
	| Some (csup, _) -> (match csup.cl_dynamic with
		| Some _ -> true
		| _ -> super_has_dynamic csup)

let generate_inline_method ctx c m =
	(match ctx.inline_methods with
	| [] -> ()
	| h :: t -> ctx.inline_methods <- t
	);
	ctx.curclass <- c;

	let old = save_locals ctx in
	ctx.in_value <- Some m.iname;
	ctx.in_block <- m.iin_block;
	ctx.in_loop <- false;
	ctx.locals <- m.ilocals;
	ctx.inv_locals <- m.iinv_locals;

	newline ctx;
	print ctx "function %s(" m.iname;
	(* arguments *)
	let in_value = (match ctx.in_value with Some _ -> true | _ -> false) in
	let arguments = remove_internals (argument_list_from_locals m.ihasthis in_value ctx.locals) in
	let arguments = match arguments with
	| [h] when h = "this" -> ["»this"]
	| h :: t when h = "this" -> "»this" :: t
	| _ -> arguments
	in

	let marguments = List.map (define_local ctx) m.iarguments in
	let arguments =  (List.map (fun a -> "&$" ^ a) arguments) @ (List.map (fun a -> "$" ^ a) marguments) in

	(match arguments with
	| [] -> ()
	| l  -> spr ctx (String.concat ", " arguments)
	);
	spr ctx ") {";
	ctx.nested_loops <- ctx.nested_loops - 1;
	let block = open_block ctx in
	newline ctx;

	(* blocks *)
	if ctx.com.debug then begin
		(*--php-prefix*)
(*		print_endline (s_path_haxe c.cl_path); *)
		spr ctx "$»spos = $GLOBALS['%s']->length";
		newline ctx;
	end;
(*
	if ctx.com.debug then begin
		(*--php-prefix*)
		print_endline (s_path_haxe c.cl_path);
		print ctx "\t$GLOBALS['%s']->push('%s:lambda_%d')" "%s" (s_path_haxe c.cl_path) m.iindex;
		newline ctx;
		spr ctx "\t$»spos = $GLOBALS['%s']->length";
		newline ctx;
	end;
*)
	gen_expr ctx m.iexpr;
	block();
	old();
	ctx.nested_loops <- ctx.nested_loops + 1;
	newline ctx;
	spr ctx "}"

let generate_class ctx c =
	let requires_constructor = ref true in
	ctx.curclass <- c;
	ctx.local_types <- List.map snd c.cl_types;

	print ctx "%s %s " (if c.cl_interface then "interface" else "class") (s_path ctx c.cl_path c.cl_extern c.cl_pos);
	(match c.cl_super with
	| None -> ()
	| Some (csup,_) ->
		requires_constructor := false;
		print ctx "extends %s " (s_path ctx csup.cl_path csup.cl_extern c.cl_pos));
	let implements = ExtList.List.unique ~cmp:(fun a b -> (fst a).cl_path = (fst b).cl_path) c.cl_implements in
	(match implements with
	| [] -> ()
	| l ->
		spr ctx (if c.cl_interface then "extends " else "implements ");
		concat ctx ", " (fun (i,_) ->
		print ctx "%s" (s_path ctx i.cl_path i.cl_extern c.cl_pos)) l);
	spr ctx "{";

	let get_dynamic_methods = List.filter is_dynamic_method c.cl_ordered_fields in

	if not ctx.curclass.cl_interface then ctx.dynamic_methods <- get_dynamic_methods;

	let cl = open_block ctx in
	(match c.cl_constructor with
	| None ->
		if !requires_constructor && not c.cl_interface then begin
			newline ctx;
			spr ctx "public function __construct(){}"
		end;
	| Some f ->
		let f = { f with
			cf_name = "__construct";
			cf_public = true;
		} in
		ctx.constructor_block <- true;
		generate_field ctx false f;
	);

	List.iter (generate_field ctx false) c.cl_ordered_fields;

	(match c.cl_dynamic with
		| Some _ when not c.cl_interface && not (super_has_dynamic c) ->
			newline ctx;
			spr ctx "public $»dynamics = array();\n\tpublic function __get($n) {\n\t\tif(isset($this->»dynamics[$n]))\n\t\t\treturn $this->»dynamics[$n];\n\t}\n\tpublic function __set($n, $v) {\n\t\t$this->»dynamics[$n] = $v;\n\t}\n\tpublic function __call($n, $a) {\n\t\tif(isset($this->»dynamics[$n]) && is_callable($this->»dynamics[$n]))\n\t\t\treturn call_user_func_array($this->»dynamics[$n], $a);\n\t\tif('toString' == $n)\n\t\t\treturn $this->__toString();\n\t\tthrow new HException(\"Unable to call «\".$n.\"»\");\n\t}"
		| Some _
		| _ ->
			if List.length ctx.dynamic_methods > 0 then begin
				newline ctx;
				spr ctx "public function __call($m, $a) {\n\t\tif(isset($this->$m) && is_callable($this->$m))\n\t\t\treturn call_user_func_array($this->$m, $a);\n\t\telse if(isset($this->»dynamics[$m]) && is_callable($this->»dynamics[$m]))\n\t\t\treturn call_user_func_array($this->»dynamics[$m], $a);\n\t\telse if('toString' == $m)\n\t\t\treturn $this->__toString();\n\t\telse\n\t\t\tthrow new HException('Unable to call «'.$m.'»');\n\t}";
			end;
	);

	List.iter (generate_field ctx true) c.cl_ordered_statics;

	let gen_props props =
		String.concat "," (List.map (fun (p,v) -> "\"" ^ p ^ "\" => \"" ^ v ^ "\"") props)
	in

	let rec fields c =
		let list = Codegen.get_properties (c.cl_ordered_statics @ c.cl_ordered_fields) in
		match c.cl_super with
		| Some (csup, _) ->
			list @ fields csup
		| None ->
			list
	in

	if not c.cl_interface then (match fields c with
		| [] ->
			()
		| props ->
			newline ctx;
			print ctx "static $__properties__ = array(%s)" (gen_props props);
		);


	cl();
	newline ctx;

	if PMap.exists "__toString" c.cl_fields then
		()
	else if PMap.exists "toString" c.cl_fields && (not c.cl_interface) && (not c.cl_extern) then begin
		print ctx "\tfunction __toString() { return $this->toString(); }";
		newline ctx
	end else if (not c.cl_interface) && (not c.cl_extern) then begin
		print ctx "\tfunction __toString() { return '%s'; }" (s_path_haxe c.cl_path) ;
		newline ctx
	end;

	print ctx "}"


let createmain com e =
	let filename = match com.php_front with None -> "index.php" | Some n -> n in
	let ctx = {
		com = com;
		stack = stack_init com false;
		tabs = "";
		ch = open_out_bin (com.file ^ "/" ^ filename);
		path = ([], "");
		buf = Buffer.create (1 lsl 14);
		in_value = None;
		in_loop = false;
		in_instance_method = false;
		imports = Hashtbl.create 0;
		extern_required_paths = [];
		extern_classes_with_init = [];
		curclass = null_class;
		curmethod = "";
		locals = PMap.empty;
		inv_locals = PMap.empty;
		local_types = [];
		inits = [];
		constructor_block = false;
		dynamic_methods = [];
		all_dynamic_methods = [];
		is_call = false;
		cwd = "";
		inline_methods = [];
		nested_loops = 0;
		inline_index = 0;
		in_block = false;
		lib_path = match com.php_lib with None -> "lib" | Some s -> s;
	} in

	spr ctx "if(version_compare(PHP_VERSION, '5.1.0', '<')) {
    exit('Your current PHP version is: ' . PHP_VERSION . '. Haxe/PHP generates code for version 5.1.0 or later');
}";
	newline ctx;
	newline ctx;
	spr ctx ("require_once dirname(__FILE__).'/" ^ ctx.lib_path ^ "/php/" ^ (prefix_class com "Boot.class.php';\n\n"));
	gen_value ctx e;
	newline ctx;
	spr ctx "\n?>";
	close ctx

let generate_main ctx c =
	(match c.cl_ordered_statics with
	| [{ cf_expr = Some e }] ->
		gen_value ctx e;
	| _ -> assert false);
		newline ctx

let generate_enum ctx e =
	ctx.local_types <- List.map snd e.e_types;
	let pack = open_block ctx in
	let ename = s_path ctx e.e_path e.e_extern e.e_pos in

	print ctx "class %s extends Enum {" ename;
	PMap.iter (fun _ c ->
		newline ctx;
		match c.ef_type with
		| TFun (args,_) ->
			print ctx "public static function %s($" c.ef_name;
			concat ctx ", $" (fun (a,o,t) ->
				spr ctx a;
				if o then spr ctx " = null";
			) args;
			spr ctx ") {";
			print ctx " return new %s(\"%s\", %d, array($" ename c.ef_name c.ef_index;
			concat ctx ", $" (fun (a,_,_) -> spr ctx a) args;
			print ctx ")); }";
		| _ ->
			print ctx "public static $%s" c.ef_name;
	) e.e_constrs;
	newline ctx;

	spr ctx "public static $__constructors = array(";

	let first = ref true in
	PMap.iter (fun _ c ->
		if not !first then spr ctx ", ";
		print ctx "%d => '%s'" c.ef_index c.ef_name;
		first := false;
	) e.e_constrs;

	spr ctx ")";

	newline ctx;

	(match Codegen.build_metadata ctx.com (TEnumDecl e) with
	| None -> ()
	| Some _ ->
		spr ctx "public static $__meta__";
		newline ctx);

	pack();

	print ctx "}";

	PMap.iter (fun _ c ->
		match c.ef_type with
		| TFun (args,_) ->
			();
		| _ ->
			newline ctx;
			print ctx "%s::$%s = new %s(\"%s\", %d)" ename c.ef_name ename c.ef_name  c.ef_index;
	) e.e_constrs;

	newline ctx;

	match Codegen.build_metadata ctx.com (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "%s::$__meta__ = " ename;
		gen_expr ctx e;
		newline ctx

let generate com =
	let all_dynamic_methods = ref [] in
	let extern_classes_with_init = ref [] in
	let php_lib_path = (match com.php_lib with None -> "lib" | Some n -> n) in
 	create_directory com (Str.split (Str.regexp "/")  php_lib_path);
	(* check for methods with the same name but different case *)
	let check_class_fields c =
		let lc_names = ref [] in
		let special_cases = ["toString"] in
		let loop c lst static =
			let in_special_cases name =
				(List.exists (fun n -> String.lowercase n = name) (special_cases @ c.cl_overrides))
			in
			List.iter(fun cf ->
				let name = String.lowercase cf.cf_name in
				let prefixed_name s = (if s then "s_" else "i_") ^ name in
				match cf.cf_kind, cf.cf_expr with
				| (Method _, Some e) when not (in_special_cases name) ->
					(try
						let lc = List.find (fun n ->
							let n = snd n in
							if static then
								(n = (prefixed_name false))
							else
								((n = (prefixed_name false)) or (n = (prefixed_name true)))
						) !lc_names in
						unsupported ("method '" ^ (s_type_path c.cl_path) ^ "." ^ cf.cf_name ^ "' already exists here '" ^ (fst lc) ^ "' (different case?)") c.cl_pos
					with Not_found ->
						lc_names := ((s_type_path c.cl_path) ^ "." ^ cf.cf_name, prefixed_name static) :: !lc_names)
				| _ ->
					()
			) lst
		in
		let rec _check_class_fields cl =
			(match cl.cl_super with
			| Some (s,_) -> _check_class_fields s
			| _ -> ());
			loop cl cl.cl_ordered_statics true;
			loop cl cl.cl_ordered_fields false
		in
		_check_class_fields c
	in
	List.iter (fun t ->
		(match t with
		| TClassDecl c ->
			check_class_fields c
		| TEnumDecl e ->
			let e_names = ref [] in
			List.iter(fun en -> (
				if List.exists (fun n -> n = String.lowercase en) !e_names then
					unsupported ("'" ^ en ^ "' constructor exists with different case") e.e_pos
				else
					e_names := (String.lowercase en) :: !e_names
			)) (e.e_names)
		| _ -> ())
	) com.types;
	List.iter (fun t ->
		(match t with
		| TClassDecl c ->
			let dynamic_methods_names lst =
				List.map (fun fd -> {
					mpath = c.cl_path;
					mname = fd.cf_name;
				}) (List.filter is_dynamic_method lst)
			in
			all_dynamic_methods := dynamic_methods_names c.cl_ordered_fields @ !all_dynamic_methods;

			if c.cl_extern then
				(match c.cl_init with
				| Some _ ->
					extern_classes_with_init := c.cl_path :: !extern_classes_with_init;
				| _ ->
					())
			else
				all_dynamic_methods := dynamic_methods_names c.cl_ordered_statics @ !all_dynamic_methods;
		| _ -> ())
	) com.types;
	List.iter (Codegen.fix_abstract_inheritance com) com.types;
	List.iter (fun t ->
		(match t with
		| TClassDecl c ->
			if c.cl_extern then begin
				(match c.cl_init with
				| None -> ()
				| Some e ->
					let ctx = init com php_lib_path c.cl_path 3 in
					gen_expr ctx e;
					newline ctx;
					close ctx;
					);
			end else
				let ctx = init com php_lib_path c.cl_path (if c.cl_interface then 2 else 0) in
				ctx.extern_classes_with_init <- !extern_classes_with_init;
				ctx.all_dynamic_methods <- !all_dynamic_methods;

				generate_class ctx c;

				(match c.cl_init with
				| None -> ()
				| Some e ->
					newline ctx;
					gen_expr ctx e);
				List.iter (generate_static_field_assign ctx c.cl_path) c.cl_ordered_statics;
				if c.cl_path = (["php"], "Boot") & com.debug then begin
					newline ctx;
					print ctx "$%s = new _hx_array(array())" ctx.stack.Codegen.stack_var;
					newline ctx;
					print ctx "$%s = new _hx_array(array())" ctx.stack.Codegen.stack_exc_var;
				end;

				let rec loop l =
					match l with
					| [] -> ()
					| h :: _ ->
						generate_inline_method ctx c h;
						loop ctx.inline_methods
				in
				loop ctx.inline_methods;
				newline ctx;
				close ctx
		| TEnumDecl e ->
			if e.e_extern then
				()
			else
				let ctx = init com php_lib_path e.e_path 1 in
			generate_enum ctx e;
			close ctx
		| TTypeDecl _ | TAbstractDecl _ ->
			());
	) com.types;
	(match com.main with
	| None -> ()
	| Some e -> createmain com e);
	Hashtbl.iter (fun name data ->
		write_resource com.file name data
	) com.resources;
