(*
 * Copyright (C)2005-2013 Haxe Foundation
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

open Ast
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
	mutable in_value : tvar option;
	mutable in_static : bool;
	mutable imports : (string,string list list) Hashtbl.t;
	mutable gen_uid : int;
	mutable local_types : t list;
	mutable constructor_block : bool;
	mutable in_interface: bool;
	mutable block_inits : (unit -> unit) option;
}

let has_feature ctx = Common.has_feature ctx.inf.com
let add_feature ctx = Common.add_feature ctx.inf.com

let is_var_field f =
	match f with
	| FStatic (_,f) | FInstance (_,f) ->
		(match f.cf_kind with Var _ -> true | _ -> false)
	| _ ->
		false
let is_var f =
	match f.cf_kind with
	| Var _ | Method MethDynamic -> true
	| _ -> false
let is_special_compare e1 e2 =
	match e1.eexpr, e2.eexpr with
	| TConst TNull, _  | _ , TConst TNull -> None
	| _ ->
	match follow e1.etype, follow e2.etype with
	| TInst ({ cl_path = [],"Xml" } as c,_) , _ | _ , TInst ({ cl_path = [],"Xml" } as c,_) -> Some c
	| _ -> None

let protect name =
	match name with
	| "lib::HxObject" | "HxEnum" -> "_" ^ name
	| _ -> name

let type_path (p, s) =
	match p with [] -> s | _ -> String.concat "::" p ^ "::" ^ s

let is_core path =
	match path with
	| (["rust"], "Int8") | (["rust"], "Int16") | (["rust"], "Char16") | ([], "Math") | ([], "UInt") | ([], "Array") | ([], "ArrayAccess") | ([], "Int") | ([], "Float") | ([], "Single") | ([], "Void") | ([], "Dynamic") | ([], "Bool") | ([], "String") | (["rust"], "Tuple2") | (["rust"], "Tuple3") ->
		true
	| _ ->
		false

let is_package name =
	((String.lowercase (String.sub name 0 1)) = (String.sub name 0 1))

let reserved =
	let h = Hashtbl.create 0 in
	List.iter (fun l -> Hashtbl.add h l ())
	(* these ones are defined in order to prevent recursion in some Std functions *)
	["is";"as";"i32";"uint";"const";"getTimer";"typeof";"parseInt";"parseFloat";
	(* Rust keywords which are not Haxe ones *)
	"loop";"with";"int";"float";"extern";"let";"mod";"impl";"trait";"struct";"_";"i8";"ui8";"i16";"ui16";
	(* we don't include get+set since they are not 'real' keywords, but they can't be used as method names *)
	"function";"class";"var";"if";"else";"while";"do";"for";"break";"continue";"return";"extends";"implements";
	"import";"switch";"case";"default";"static";"public";"private";"try";"catch";"this";"throw";"interface";
	"override";"package";"null";"true";"false";"()"
	];
	h

	(* "each", "label" : removed (actually allowed in locals and fields accesses) *)

let s_ident n =
	if Hashtbl.mem reserved n then (if n = "_" then "z" else "_") ^ n else n

let rec create_dir acc = function
	| [] -> ()
	| d :: l ->
		let dir = String.concat "/" (List.rev (d :: acc)) in
		if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
		create_dir (d :: acc) l

let init infos path =
	let dir = infos.com.file :: fst path in
	create_dir [] dir;
	let ch = open_out (String.concat "/" dir ^ "/" ^ snd path ^ ".rs") in
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
		imports = imports;
		curclass = null_class;
		gen_uid = 0;
		local_types = [];
		get_sets = Hashtbl.create 0;
		constructor_block = false;
		in_interface = false;
		block_inits = None;
	}

let print ctx = Printf.kprintf (fun s -> Buffer.add_string ctx.buf s)

let s_path ctx path =
	match path with
	| (pack,name) ->
		let notcore = not (is_core path) in
		let name = protect name in
		let packs = (try Hashtbl.find ctx.imports name with Not_found -> []) in
		if not (List.mem pack packs) && notcore && not (is_package name) then Hashtbl.replace ctx.imports name (pack :: packs);
		type_path path ^ (if (ctx.path = path) || (is_package name) || (List.length packs = 0) then "" else "::" ^ name)

let soft_newline ctx =
	print ctx "\n%s" ctx.tabs

let gen_local ctx l =
	ctx.gen_uid <- ctx.gen_uid + 1;
	if ctx.gen_uid = 1 then l else l ^ string_of_int ctx.gen_uid

let spr ctx s = Buffer.add_string ctx.buf s

let unsupported p = error "This expression cannot be generated into Rust" p

let block e =
	match e.eexpr with
	| TBlock(el) -> e
	| _ -> mk (TBlock [e]) e.etype e.epos

let newline ctx =
	let rec loop p =
		match Buffer.nth ctx.buf p with
		| '}' | '{' | ':' -> print ctx "\n%s" ctx.tabs
		| '\t' | ' ' -> loop (p - 1)
		| '\n' -> ()
		| _ -> print ctx ";\n%s" ctx.tabs
	in
	try loop (Buffer.length ctx.buf - 1) with _ -> ()

let close ctx =
	let should_gen path =
		match !path with
		| (["lib"], _) -> false
		| (name, paths) -> !path <> ctx.path
	in
	let is_mod path =
		match !path with
		| ([], _) -> true
		| _ -> false
	in
	let is_lib = match ctx.path with
		| (["lib"], _) -> true
		| ([], "lib") -> true
		| _ -> false
	in
	Hashtbl.iter (fun name paths ->
		let path = ref ([], name) in
		if List.length paths > 0 then
			List.iter (fun pack ->
				path := (pack, name);
			) paths;
		if not (is_mod path) && should_gen path then
			output_string ctx.ch ("use " ^ type_path !path ^ ";\n");
	) ctx.imports;
	if not is_lib then (
		output_string ctx.ch "mod lib;\n";
		Hashtbl.iter (fun name paths ->
			let path = ref ([], name) in
			if List.length paths > 0 then
				List.iter (fun pack ->
					path := (pack, name);
				) paths;
			if is_mod path && should_gen path then
				output_string ctx.ch ("mod " ^ type_path !path ^ ";\n");
		) ctx.imports;
	);
	output_string ctx.ch (Buffer.contents ctx.buf);
	close_out ctx.ch

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

let rec default_value tstr =
	match tstr.[0] with
	| '@' -> "@" ^ default_value (String.sub tstr 1 ((String.length tstr)-1))
	| '~' -> "~" ^ default_value (String.sub tstr 1 ((String.length tstr)-1))
	| '&' -> "&" ^ default_value (String.sub tstr 1 ((String.length tstr)-1))
	| _ -> (match tstr with
		| "i32"  -> "0i32"
		| "ui32" -> "0ui32"
		| "f32" -> "f32::NaN"
		| "f64" -> "f64::NaN"
		| "bool" -> "false"
		| "str" -> "\"\""
		| "()" -> "()"
		| _ -> "None")

let get_params cl_types =
	match cl_types with
		| [] ->
			""
		| _ ->
			"<"^(String.concat ", " (List.map (fun (_, tcl) ->
				match follow tcl with
				| TInst(cl, _) -> snd cl.cl_path
				| _ -> assert false) cl_types)
			) ^ ">"

let rec type_str ctx t p =
	let s_type_params params =
		(match params with
		| [] -> ""
		| l -> "<" ^ String.concat ", " (List.map (fun param -> type_str ctx param p) params) ^ ">") in
	let extern = ctx.curclass.cl_extern in
	let reftype = (if ctx.curclass.cl_extern then "~" else "@") in (* The default reference type *)
	let canwrap = ref false in
	let value = (match t with
	| TAbstract ({ a_impl = Some _ } as a,pl) ->
		type_str ctx (apply_params a.a_types pl a.a_this) p
	| TAbstract (a,_) ->
		(match a.a_path with
		| [], "UInt" -> "ui32"
		| [], "Int" -> "i32"
		| ["rust"], "Int8" -> "i8"
		| ["rust"], "Int16" -> "i16"
		| ["rust"], "Char16" -> "char"
		| [], "Float" -> "f64"
		| [], "Single" -> "f32"
		| [], "Bool" -> "bool"
		| [], "Void" -> "()"
		| _ -> if (Meta.has Meta.Extern a.a_meta) then (snd a.a_path) else (s_path ctx a.a_path))
	| TEnum (e, params) ->
		if e.e_extern then (match e.e_path with
			| [], "Bool" -> "bool"
			| _ ->
				let rec loop = function
					| (Ast.Meta.FakeEnum,[Ast.EConst (Ast.Ident n),_],_) :: _ ->
						(match n with
						| "Int" -> "i32"
						| "UInt" -> "ui32"
						| "Float" -> "f64"
						| "Single" -> "f32"
						| "String" -> "str"
						| "Bool" -> "bool"
						| _ -> n)
					| [] -> "lib::HxObject"
					| _ :: l -> loop l
				in
				(loop e.e_meta) ^ (s_type_params params)
		) else
			s_path ctx e.e_path
	| TInst ({ cl_path = [], "Class"}, [pt]) ->
		reftype ^ "lib::HxObject"
	| TInst ({ cl_path = [], "String"}, _) ->
		reftype ^ "str"
	| TInst ({ cl_path = [], "Array"},[pt]) ->
		let typed = type_str ctx pt p in
		"~[" ^ typed ^ "]"
	| TInst ({ cl_path = ["rust"], "Tuple2"}, [pa; pb]) ->
		let at = type_str ctx pa p in
		let bt = type_str ctx pb p in
		reftype ^ "(" ^ at ^ ", " ^ bt ^ ")"
	| TInst ({ cl_path = ["rust"], "Tuple3"}, [pa; pb; pc]) ->
		let at = type_str ctx pa p in
		let bt = type_str ctx pb p in
		let ct = type_str ctx pc p in
		reftype ^ "(" ^ at ^ ", " ^ bt ^ ", " ^ ct ^")"
	| TInst (c,params) when c.cl_extern ->
		let name = snd c.cl_path in
		reftype ^ name
	| TInst (c,params) ->
		let ps = s_type_params params in
		let name = ((match c.cl_kind with
			| KTypeParameter _ -> snd c.cl_path
			| KNormal | KGeneric | KGenericInstance _ | KAbstractImpl _ -> (s_path ctx c.cl_path) ^ ps
			| KExtension _ | KExpr _ | KMacroType -> "lib::HxObject") ^ ps) in
		reftype ^ name
	| TFun (args, haxe_type) ->
		let nargs = List.map (fun (arg,o,t) -> (type_str ctx t p)) args in
		let asfun = "fn(" ^ (String.concat ", " nargs) ^ ")->" ^ type_str ctx haxe_type p in
		reftype ^ asfun
	| TMono r ->
		(match !r with
		| None -> 
			reftype ^ "lib::HxObject"
		| Some t -> type_str ctx t p)
	| TAnon _ | TDynamic _ ->
		reftype ^ "lib::HxObject"
	| TType (t,args) ->
		(match t.t_path with
		| _ -> type_str ctx (apply_params t.t_types args t.t_type) p)
	| TLazy f ->
		type_str ctx ((!f)()) p
	) in
	if is_nullable t && value <> "()" && not extern then
		"Option<"^value^">"
	else
		value

let rec s_tparams ctx params p =
	if List.length params > 0 then
		"<" ^ (String.concat ", " (List.map(fun t ->
			match t with
			| TInst({cl_kind = KTypeParameter pl; cl_path = path}, ps) ->
				(snd path) ^ (s_tparams ctx ps p)
			| TInst(c, ps) ->
				(s_path ctx c.cl_path) ^ (s_tparams ctx ps p)
			| _ ->
				(type_str ctx t pos)
		) params)) ^ ">"
	else
		""


let rec is_nullable_ext ?(no_lazy=false) = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_nullable t)
	| TType ({ t_path = ([],"Null") },[_]) ->
		true
	| TLazy f ->
		if no_lazy then raise Exit else is_nullable (!f())
	| TType (t,tl) ->
		is_nullable (apply_params t.t_types tl t.t_type)
	| TFun _ ->
		false
(*
	Type parameters will most of the time be nullable objects, so we don't want to make it hard for users
	to have to specify Null<T> all over the place, so while they could be a basic type, let's assume they will not.

	This will still cause issues with inlining and haxe.rtti.Generic. In that case proper explicit Null<T> is required to
	work correctly with basic types. This could still be fixed by redoing a nullability inference on the typed AST.

	| TInst ({ cl_kind = KTypeParameter },_) -> false
*)
	| _ ->
		false

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ | TMatch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let escape_bin s =
	let b = Buffer.create 0 in
	for i = 0 to String.length s - 1 do
		match Char.code (String.unsafe_get s i) with
		| c when c < 32 -> Buffer.add_string b (Printf.sprintf "\\x%.2X" c)
		| c -> Buffer.add_char b (Char.chr c)
	done;
	Buffer.contents b

let generate_resources ctx infos =
	if Hashtbl.length infos.com.resources <> 0 then begin
		let add_resource name data =
			print ctx "%s => Some(\"" name;
			for i = 0 to String.length data - 1 do
				let code = Char.code (String.unsafe_get data i) in
				print ctx "0x%.2x, " code;
				if ( (i mod 10) = 9) then soft_newline ctx;
			done;
			spr ctx "\")";
		in
		let ctx = init infos ([],"resources") in
		spr ctx "pub fn get(name:@str) -> Option<@str> {";
		let getfn = open_block ctx in
		newline ctx;
		spr ctx "return match(name) {";
		let mtc = open_block ctx in
		newline ctx;
		Hashtbl.iter (fun name data -> add_resource name data) infos.com.resources;
		spr ctx "_ => None";
		mtc();
		newline ctx;
		spr ctx "}";
		getfn();
		newline ctx;
		spr ctx "};";
		newline ctx;
		close ctx;
	end

let gen_constant ctx p = function
	| TInt i -> print ctx "%ldi32" i
	| TFloat s -> print ctx "%sf64" s
	| TString s -> print ctx "Some(@\"%s\")" (escape_bin (Ast.s_escape s))
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "None"
	| TThis -> spr ctx "self"
	| TSuper -> spr ctx "self.super()"

let gen_function_header ctx name f params p =
	let old = ctx.in_value in
	let old_t = ctx.local_types in
	let old_bi = ctx.block_inits in
	let closure = (match name with
	| None -> true
	| _ -> false) in
	ctx.in_value <- None;
	ctx.local_types <- List.map snd params @ ctx.local_types;
	let init () =
		List.iter (fun (v,o) -> match o with
			| Some c when is_nullable v.v_type && c <> TNull ->
				newline ctx;
				print ctx "%s = match %s {" v.v_name v.v_name;
				let mtc = open_block ctx in
				newline ctx;
				spr ctx "None => ";
				gen_constant ctx p c;
				spr ctx ",";
				newline ctx;
				spr ctx "Some(v) => v";
				mtc();
				spr ctx "}";
			| _ -> ()
		) f.tf_args;
		ctx.block_inits <- None;
	in
	ctx.block_inits <- Some init;
	if not closure then
		print ctx "fn%s%s(" (match name with None -> "" | Some (n,meta) ->
			let rec loop = function
				| [] -> n
				| (Ast.Meta.Getter,[Ast.EConst (Ast.Ident i),_],_) :: _ -> "get " ^ i
				| (Ast.Meta.Setter,[Ast.EConst (Ast.Ident i),_],_) :: _ -> "set " ^ i
				| _ :: l -> loop l
			in
			" " ^ loop meta
		) (get_params params);
	if closure then
		spr ctx "|";
	if not ctx.in_static && not ctx.constructor_block && not closure then
		print ctx "&mut self";
	if (not ctx.in_static) && List.length f.tf_args > 0 && not ctx.constructor_block && not closure then
		print ctx ", ";
	concat ctx ", " (fun (v,c) ->
		let tstr = type_str ctx v.v_type p in
		print ctx "%s: %s" (s_ident v.v_name) tstr;
	) f.tf_args;
	print ctx "%s -> %s " (if closure then "|" else ")") (type_str ctx (if ctx.constructor_block then TInst(ctx.curclass, []) else f.tf_type) p);
	(fun () ->
		ctx.in_value <- old;
		ctx.local_types <- old_t;
		ctx.block_inits <- old_bi;
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
	| TLocal { v_name = "__rust__" }, [{ eexpr = TConst (TString code) }] ->
		spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
	| TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" })), [num] ->
		spr ctx "((";
		unwrap ctx num;
		spr ctx ") as i32)";
	| TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "string" })), [obj] ->
		spr ctx "Some(@(";
		gen_value ctx obj;
		spr ctx ".to_str()))";
	| TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "parseFloat" })), [st] ->
		spr ctx "f64::from_str(";
		unwrap ctx st;
		spr ctx ")";
	| TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "parseInt" })), [st] ->
		spr ctx "i32::from_str(";
		unwrap ctx st;
		spr ctx ")";
	| TField (ee,f), args when is_var_field f ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TField( _, FStatic({ cl_extern = true }, { cf_name = name; cf_type=t })), args ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (fun arg ->
			 if (is_nullable_ext t) && not (is_nullable arg.etype) then
			 	wrap ctx arg
			 else if not (is_nullable_ext t) && (is_nullable arg.etype) then
			 	unwrap ctx arg
			 else
			 	gen_value ctx arg
		) args;
		spr ctx ")"
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

and unwrap ctx e =
	if (is_nullable e.etype) then (
		spr ctx "(";
		gen_value ctx e;
		spr ctx ").unwrap()";
	) else
		gen_value ctx e;

and wrap ctx e =
	if (is_nullable e.etype) then (
		gen_value ctx e;
	) else (
		spr ctx "Some(";
		gen_value ctx e;
		spr ctx ")";
	)

and gen_value_op ctx e =
	match e.eexpr with
	| TBinop (op,_,_) when op = Ast.OpAnd || op = Ast.OpOr || op = Ast.OpXor ->
		spr ctx "(";
		unwrap ctx e;
		spr ctx ")";
	| _ ->
		unwrap ctx e

and gen_field_access ctx t s =
	let static = match follow t with
	| TInst (c,_) -> false
	| TAnon a ->
		(match !(a.a_status) with
		| Statics c -> true
		| _ -> false)
	| TEnum _ -> true
	| _ ->
		false in
	print ctx "%s%s" (if static then "::" else ".") (s_ident s)

and gen_expr ctx e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx e.epos c
	| TLocal v ->
		spr ctx (s_ident v.v_name)
	| TArray (e1,e2) ->
		unwrap ctx e1;
		spr ctx "[";
		unwrap ctx e2;
		spr ctx "]";
	| TBinop (op,e1,e2) ->
		(match op with
		| Ast.OpAssign | Ast.OpAssignOp _ ->
			spr ctx "{";
			let temp = open_block ctx in (* for cleanliness *)
			soft_newline ctx;
			gen_value_op ctx e1;
			print ctx " %s " (Ast.s_binop op);
			gen_value_op ctx e2;
			newline ctx;
			gen_value_op ctx e1;
			temp();
			soft_newline ctx;
			spr ctx "}";
		| _ ->
			gen_value_op ctx e1;
			print ctx " %s " (Ast.s_binop op);
			gen_value_op ctx e2;)
	| TField( e, FStatic({ cl_path = ([], "Math") }, { cf_name = "PI" })) ->
		spr ctx "3.1415926589f64"
	| TField( e, FStatic({ cl_path = ([], "Math") }, { cf_name = "NaN" })) ->
		spr ctx "f64::NaN"
	| TField( e, FStatic({ cl_path = ([], "Math") }, { cf_name = "isNaN" })) ->
		spr ctx "f64::is_NaN";
	| TField( e, FStatic({ cl_path = ([], "Math") }, { cf_name = "isFinite" })) ->
		spr ctx "f64::is_finite";
	| TField( e, FStatic({ cl_path = ([], "Math") }, { cf_name = "POSITIVE_INFINITY" })) ->
		spr ctx "f64::infinity";
	| TField( e, FStatic({ cl_path = ([], "Math") }, { cf_name = "NEGATIVE_INFINITY" })) ->
		spr ctx "f64::neg_infinity";
	| TField( e, FInstance({ cl_path = ([], "String") }, { cf_name = "length" }) ) ->
		unwrap ctx e;
		spr ctx ".len()"
	| TField( e, FInstance({ cl_path = ([], "Array") }, { cf_name = "length" }) ) ->
		unwrap ctx e;
		spr ctx ".len()"
	| TField( e, FInstance({ cl_path = (["rust"], "Tuple2"); cl_types = [(_, t1); (_, t2)] }, f) ) when f.cf_name = "a" || f.cf_name = "b" ->
		let tmp = gen_local ctx "_r" in
		spr ctx "{";
		let bl = open_block ctx in
		let typ = match f.cf_name with
		| "a" -> t1
		| _ -> t2
		in
		newline ctx;
		print ctx "let mut %s = None" tmp;
		newline ctx;
		spr ctx "match *";
		unwrap ctx e;
		spr ctx " {";
		let mtc = open_block ctx in
		newline ctx;
		(match f.cf_name with
		| "a" -> print ctx "(a, _) => %s = a" tmp;
		| _ -> print ctx "(_, b) => %s = b" tmp;
		);
		mtc();
		soft_newline ctx;
		spr ctx "}";
		newline ctx;
		spr ctx tmp;
		bl();
		soft_newline ctx;
		spr ctx "}"
	| TField( e, FInstance({ cl_path = (["rust"], "Tuple3"); cl_types = [(_, t1); (_, t2); (_, t3)] }, f) ) when f.cf_name = "a" || f.cf_name = "b" || f.cf_name = "c" ->
		let tmp = gen_local ctx "_r" in
		spr ctx "{";
		let bl = open_block ctx in
		let typ = match f.cf_name with
		| "a" -> t1
		| "b" -> t2
		| _ -> t3
		in
		newline ctx;
		print ctx "let mut %s = None" tmp;
		newline ctx;
		spr ctx "match *";
		unwrap ctx e;
		spr ctx " {";
		let mtc = open_block ctx in
		newline ctx;
		(match f.cf_name with
		| "a" -> print ctx "(a, _, _) => %s = a" tmp;
		| "b" -> print ctx "(_, b, _) => %s = b" tmp;
		| _ -> print ctx "(_, _, c) => %s = c" tmp;
		);
		mtc();
		soft_newline ctx;
		spr ctx "}";
		newline ctx;
		spr ctx tmp;
		bl();
		soft_newline ctx;
		spr ctx "}";
	| TField (e, FStatic(c, f)) when (match f.cf_kind with | Var _ -> true | _ -> false) && ctx.curclass.cl_path = c.cl_path ->
		spr ctx f.cf_name
	| TField (e, FEnum(m, f)) ->
		print ctx "%s::%s" (s_path ctx m.e_path) f.ef_name;
	| TField (e, FStatic(c, f)) ->
		spr ctx (s_path ctx c.cl_path);
		gen_field_access ctx e.etype f.cf_name
	| TField (e,s) ->
		unwrap ctx e;
		gen_field_access ctx e.etype (field_name s)
	| TTypeExpr t ->
		spr ctx (s_path ctx (t_path t))
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")"
	| TReturn eo ->
		if ctx.in_value <> None then unsupported e.epos;
		(match eo with
		| None ->
			spr ctx "return"
		| Some e when (match follow e.etype with TEnum({ e_path = [],"Void" },[]) | TAbstract ({ a_path = [],"Void" },[]) -> true | _ -> false) ->
			print ctx "{";
			let bend = open_block ctx in
			newline ctx;
			gen_value ctx e;
			newline ctx;
			spr ctx "return";
			bend();
			newline ctx;
			print ctx "}";
		| Some e ->
			spr ctx "return ";
			gen_value ctx e);
	| TBreak ->
		if ctx.in_value <> None then unsupported e.epos;
		spr ctx "break"
	| TContinue ->
		if ctx.in_value <> None then unsupported e.epos;
		spr ctx "continue"
	| TBlock el ->
		spr ctx "{";
		let bend = open_block ctx in
		(match ctx.block_inits with None -> () | Some i -> i());
		if ctx.constructor_block then (
			let c = ctx.curclass in
			newline ctx;
			let name = match ctx.curclass.cl_path with
			| _, n -> n in
			print ctx "let mut self = %s {" name;
			let obj_fields = List.filter is_var c.cl_ordered_fields in
			concat ctx ", " (fun f ->
				spr ctx f.cf_name;
				spr ctx ": ";
				match(f.cf_expr) with
				| None -> spr ctx (default_value (type_str ctx f.cf_type e.epos));
				| Some v -> gen_expr ctx v;
			) obj_fields;
			spr ctx "};";
			soft_newline ctx;
		);
		List.iter (fun e -> newline ctx; gen_expr ctx e) el;
		if ctx.constructor_block then (
			soft_newline ctx;
			spr ctx "return Some(@self)";
		) else
			newline ctx;
		bend();
		spr ctx "}";
		soft_newline ctx;
	| TFunction f ->
		spr ctx "Some(@";
		let h = gen_function_header ctx None f [] e.epos in
		let old = ctx.in_static in
		ctx.in_static <- true;
		gen_expr ctx (block f.tf_expr);
		ctx.in_static <- old;
		h();
		spr ctx ")";
	| TCall (v,el) ->
		gen_call ctx v el e.etype
	| TArrayDecl el ->
		spr ctx "Some(~[";
		concat ctx "," (gen_value ctx) el;
		spr ctx "])"
	| TThrow e ->
		spr ctx "fail!(";
		gen_value ctx e;
		spr ctx ")";
	| TVars [] ->
		()
	| TVars vl ->
		spr ctx "let mut ";
		concat ctx ", " (fun (v,eo) ->
			print ctx "%s: %s = " (s_ident v.v_name) (type_str ctx v.v_type e.epos);
			match eo with
			| None ->
				spr ctx (default_value (type_str ctx v.v_type e.epos))
			| Some e ->
				gen_value ctx e
		) vl;
	| TNew ({ cl_path = ([], "Array") },_, el) ->
		spr ctx "[]";
	| TNew ({ cl_path = (["rust"], "Tuple2") },_ ,el) ->
		spr ctx "Some(@(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx "))";
	| TNew ({ cl_path = (["rust"], "Tuple3") },_ ,el) ->
		spr ctx "Some(@(";
		concat ctx ", " (gen_value ctx) el;
		spr ctx "))";
	| TNew (c,params,el) ->
		print ctx "%s::new(" (s_path ctx c.cl_path);
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if ";
		unwrap ctx (parent cond);
		spr ctx " ";
		gen_expr ctx (block e);
		(match eelse with
		| None -> ()
		| Some e ->
			newline ctx;
			spr ctx "else ";
			gen_expr ctx (block e));
	| TUnop (Increment,fix,e) ->
		let post = match fix with
		| Postfix -> true
		| Prefix -> false in
		spr ctx "{";
		let temp = open_block ctx in (* for cleanliness *)
		soft_newline ctx;
		gen_value ctx e;
		spr ctx " += 1";
		newline ctx;
		gen_value ctx e;
		if post then
			spr ctx " - 1";
		temp();
		soft_newline ctx;
		spr ctx "};";
		soft_newline ctx;
	| TUnop (Decrement,fix,e) ->
		let post = match fix with
		| Postfix -> true
		| Prefix -> false in
		spr ctx "{";
		let temp = open_block ctx in (* for cleanliness *)
		soft_newline ctx;
		gen_value ctx e;
		spr ctx " -= 1";
		newline ctx;
		gen_value ctx e;
		if post then
			spr ctx " + 1";
		temp();
		soft_newline ctx;
		spr ctx "};";
		soft_newline ctx;
	| TUnop (op, Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_value ctx e;
	| TUnop (op, Ast.Postfix,e) ->
		gen_value ctx e;
		spr ctx (Ast.s_unop op);
	| TWhile ({ eexpr = TConst (TBool true) }, e, _) ->
		spr ctx "loop ";
		gen_expr ctx (block e);
	| TWhile (cond,e,Ast.NormalWhile) ->
		spr ctx "while ";
		gen_value ctx (parent cond);
		spr ctx " ";
		gen_expr ctx (block e);
	| TWhile (cond,e,Ast.DoWhile) ->
		spr ctx "do ";
		gen_expr ctx (block e);
		spr ctx " while";
		gen_value ctx (parent cond);
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s : " (s_ident f); gen_value ctx e) fields;
		spr ctx "}"
	| TFor (v,it,e) ->
		let tmp = gen_local ctx "_it" in
		print ctx "let mut %s = " tmp;
		gen_value ctx it;
		newline ctx;
		print ctx "while %s.hasNext() {" tmp;
		let lup = open_block ctx in
		newline ctx;
		print ctx "let %s:%s = %s.next()" (s_ident v.v_name) (type_str ctx v.v_type e.epos) tmp;
		newline ctx;
		gen_expr ctx (block e);
		lup();
		newline ctx;
		spr ctx "}";
	| TTry (e,catchs) ->
		spr ctx "{";
		let tblock = open_block ctx in
		soft_newline ctx;
		let tmp = gen_local ctx "err" in
		print ctx "let %s = do task::try " tmp;
		gen_expr ctx (block e);
		spr ctx ";";
		soft_newline ctx;
		let mto = ref false in
		List.iter (fun (v,e) ->
			newline ctx;
			if !mto then
				spr ctx "else";
			print ctx "if (lib::is(%s, %s)) {" tmp (type_str ctx v.v_type e.epos);
			let errb = open_block ctx in
			newline ctx;
			print ctx "let mut %s = %s" (s_ident v.v_name) tmp;
			newline ctx;
			gen_expr ctx e;
			errb();
			newline ctx;
			spr ctx "};";
			mto := true;
		) catchs;
		tblock();
		soft_newline ctx;
		spr ctx "}";
	| TMatch (e,_,cases,def) ->
		spr ctx "match ";
		let bend = open_block ctx in
		unwrap ctx e;
		spr ctx " {";
		List.iter (fun (cl,params,e) ->
			List.iter (fun c ->
				soft_newline ctx;
				print ctx "%d =>" c;
			) cl;
			(match params with
			| None | Some [] -> ()
			| Some l ->
				let n = ref (-1) in
				let l = List.fold_left (fun acc v -> incr n; match v with None -> acc | Some v -> (v,!n) :: acc) [] l in
				match l with
				| [] -> ()
				| l ->
					soft_newline ctx;
					spr ctx "var ";
					concat ctx ", " (fun (v,n) ->
						print ctx "%s : %s" (s_ident v.v_name) (type_str ctx v.v_type e.epos);
						()
					) l);
			gen_expr ctx e;
			print ctx ",";
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			soft_newline ctx;
			spr ctx "_ => ";
			gen_block ctx e;
		);
		bend();
		soft_newline ctx;
		spr ctx "}";
	| TSwitch (e,cases,def) ->
		spr ctx "match ";
		gen_value ctx (parent e);
		spr ctx " {";
		let mtc = open_block ctx in
		newline ctx;
		List.iter (fun (el,e2) ->
			List.iter (fun e ->
				gen_value ctx e;
			) el;
			spr ctx " => ";
			gen_expr ctx e2;
			spr ctx ",";
			soft_newline ctx;
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "_ => ";
			gen_expr ctx e;
		);
		mtc();
		soft_newline ctx;
		spr ctx "}"
	| TCast (e, None) ->
		spr ctx "(";
		gen_value ctx e;
		print ctx " as %s)" (type_str ctx e.etype e.epos);
	| TCast (e, Some t) ->
		gen_expr ctx (Codegen.default_cast ctx.inf.com e t e.etype e.epos)

and gen_block ctx e =
	newline ctx;
	match e.eexpr with
	| TBlock [] -> ()
	| _ ->
		gen_expr ctx e;
		newline ctx

and gen_value ctx e =
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> assert false | Some r -> r)) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let value block =
		let old = ctx.in_value in
		let t = type_str ctx e.etype e.epos in
		let r = alloc_var (gen_local ctx "_r") e.etype in
		ctx.in_value <- Some r;
		let b = if block then begin
			spr ctx "{";
			let b = open_block ctx in
			newline ctx;
			print ctx "let mut %s : %s" r.v_name t;
			newline ctx;
			b
		end else
			(fun() -> ())
		in
		(fun() ->
			if block then begin
				newline ctx;
				print ctx "%s" r.v_name;
				b();
				newline ctx;
				spr ctx "}";
			end;
			ctx.in_value <- old;
			if ctx.in_static then
				print ctx "()"
			else
				spr ctx "self";
		)
	in
	match e.eexpr with
	| TConst _
	| TLocal _
	| TArray _
	| TBinop _
	| TField _
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
	| TBlock [] ->
		spr ctx "None"
	| TBlock [e] ->
		gen_value ctx e
	| TBlock el ->
		let v = value true in
		let rec loop = function
			| [] ->
				spr ctx "return None";
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
		spr ctx "if ";
		gen_value ctx (mk (TBlock [cond]) e.etype e.epos);
		spr ctx " ";
		gen_value ctx e;
		spr ctx " else ";
		(match eo with
		| None -> spr ctx "None"
		| Some e -> gen_value ctx e);
		newline ctx
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
		gen_expr ctx (mk (TTry (block (assign b),
			List.map (fun (v,e) -> v, block (assign e)) catchs
		)) e.etype e.epos);
		v()

let get_meta_string meta key =
	let rec loop = function
		| [] -> ""
		| (k,[Ast.EConst (Ast.String name),_],_) :: _  when k=key-> name
		| _ :: l -> loop l
		in
	loop meta

let get_code meta key =
	let code = get_meta_string meta key in
	if (code<>"") then code ^ "\n" else code

let generate_field ctx static f =
	ctx.in_static <- static;
	ctx.gen_uid <- 0;
	List.iter (fun(m,pl,_) ->
		match m,pl with
		| _ -> ()
	) f.cf_meta;
	let public = f.cf_public || Hashtbl.mem ctx.get_sets (f.cf_name,static) || (f.cf_name = "main" && static) || f.cf_name = "resolve" || Ast.Meta.has Ast.Meta.Public f.cf_meta in
	let rights = (if public then "pub" else "priv") in
	let p = ctx.curclass.cl_pos in
	match f.cf_expr, f.cf_kind with
	| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
		soft_newline ctx;
		print ctx "%s " rights;
		let rec loop c =
			match c.cl_super with
			| None -> ()
			| Some (c,_) ->
				loop c
		in
		if not static then loop ctx.curclass;
		let h = gen_function_header ctx (Some (s_ident f.cf_name, f.cf_meta)) fd f.cf_params p in
		let code = (get_code f.cf_meta Meta.FunctionCode) in
		if (not ctx.in_interface) && String.length code > 0 then (
			spr ctx "{";
			let fn = open_block ctx in
			newline ctx;
			spr ctx code;
			soft_newline ctx;
			fn();
			spr ctx "}";
		);
		if (not ctx.in_interface) && String.length code = 0 then
			gen_expr ctx fd.tf_expr;
		h()
	| _ ->
		let is_getset = (match f.cf_kind with Var { v_read = AccCall _ } | Var { v_write = AccCall _ } -> true | _ -> false) in
		if ctx.curclass.cl_interface then
			match follow f.cf_type with
			| TFun (args,r) ->
				let rec loop = function
					| [] -> f.cf_name
					| (Ast.Meta.Getter,[Ast.EConst (Ast.String name),_],_) :: _ -> "get_" ^ name
					| (Ast.Meta.Setter,[Ast.EConst (Ast.String name),_],_) :: _ -> "set_" ^ name
					| _ :: l -> loop l
				in
				print ctx "fn %s(" (loop f.cf_meta);
				concat ctx "," (fun (arg,o,t) ->
					let tstr = type_str ctx t p in
					print ctx "%s : %s" arg tstr;
				) args;
				print ctx ") : %s" (type_str ctx r p);
			| _ when is_getset ->
				let t = type_str ctx f.cf_type p in
				let id = s_ident f.cf_name in
				(match f.cf_kind with
				| Var v ->
					(match v.v_read with
					| AccNormal -> print ctx "fn get_%s() : %s" id t;
					| AccCall s -> print ctx "fn %s() : %s" s t;
					| _ -> ());
					(match v.v_write with
					| AccNormal -> print ctx "fn set_%s( __v : %s )" id t;
					| AccCall s -> print ctx "fn %s( __v : %s ) : %s" s t t;
					| _ -> ());
				| _ -> assert false)
			| _ when not static ->
				print ctx "%s: %s" (s_ident f.cf_name) (type_str ctx f.cf_type p);
				()
			| _ when static ->
				print ctx "%s static %s:%s = %s" rights (s_ident f.cf_name) (type_str ctx f.cf_type p) (default_value (type_str ctx f.cf_type p));
				()
			| _ -> ()
		else
		if not is_getset && not static then begin
			print ctx "%s: %s" (s_ident f.cf_name) (type_str ctx f.cf_type p);
			soft_newline ctx;
		end else if not is_getset && static then begin
			print ctx "%s static %s:%s = %s" rights (s_ident f.cf_name) (type_str ctx f.cf_type p) (default_value (type_str ctx f.cf_type p));
			newline ctx;
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

let generate_obj_impl ctx c =
	ctx.curclass <- c;
	ctx.local_types <- List.map snd c.cl_types;
	let obj_fields = List.filter (is_var) c.cl_ordered_fields in
	let obj_methods = List.filter (fun x -> not (is_var x)) c.cl_ordered_fields in
	let static_fields = List.filter(is_var) c.cl_ordered_statics in
	let static_methods = List.filter (fun x -> not (is_var x)) c.cl_ordered_statics in
	let params = get_params c.cl_types in
	ctx.in_interface <- c.cl_interface;
	print ctx "impl%s %s for %s%s {" params (if ctx.path = ([], "lib") then "HxObject" else "lib::HxObject") (type_path c.cl_path) params;
	let impl = open_block ctx in
	if (has_feature ctx "Reflect.field") then (
		newline ctx;
		spr ctx "pub fn __get_field(&self, &field:str)->Option<@HxObject> {";
		let fn = open_block ctx in
		soft_newline ctx;
		if ((List.length obj_fields) = 0) then
			spr ctx "return None"
		else (
			spr ctx "return match(field) {";
			let mtc = open_block ctx in
			List.iter(fun f ->
				soft_newline ctx;
				print ctx "\"%s\" => " f.cf_name;
				if not (is_nullable f.cf_type) then
					spr ctx "Some(";
				print ctx "self.%s" f.cf_name;
				if not (is_nullable f.cf_type) then
					spr ctx ")";
				spr ctx ",";
			) (List.append obj_fields obj_methods);
			soft_newline ctx;
			spr ctx "_ => None";
			mtc();
			soft_newline ctx;
			spr ctx "}"
		);
		fn();
		newline ctx;
		spr ctx "}";
	);
	if (has_feature ctx "Reflect.setField") then (
		newline ctx;
		spr ctx "pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {";
		let fn = open_block ctx in
		soft_newline ctx;
		if ((List.length obj_fields) > 0) then (
			spr ctx "match(field) {";
			let mtc = open_block ctx in
			List.iter(fun f ->
				soft_newline ctx;
				print ctx "\"%s\" => self.%s = value," f.cf_name f.cf_name;
			) obj_fields;
			soft_newline ctx;
			spr ctx "_ => None";
			mtc();
			soft_newline ctx;
			spr ctx "}"
		);
		fn();
		newline ctx;
		spr ctx "}";
	);
	if (has_feature ctx "Reflect.fields") then (
		newline ctx;
		spr ctx "pub fn __fields(&mut self) -> Option<@[@str]> {";
		let fn = open_block ctx in
		newline ctx;
		spr ctx "return __instance_fields()";
		fn();
		newline ctx;
		spr ctx "}";
	);
	if (has_feature ctx "Type.getInstanceFields") then (
		newline ctx;
		spr ctx "pub fn __instance_fields() -> Option<@[@str]> {";
		let fn = open_block ctx in
		newline ctx;
		spr ctx "return Some(@[";
		concat ctx ", " (fun f ->
			print ctx "@\"%s\"" f.cf_name;
		) obj_fields;
		spr ctx "])";
		fn();
		newline ctx;
		spr ctx "}";
	);
	if (has_feature ctx "Type.getClassName") then (
		newline ctx;
		spr ctx "pub fn __name() -> Option<@str> {";
		let fn = open_block ctx in
		newline ctx;
		print ctx "return Some(@\"%s\")" (s_type_path c.cl_path);
		fn();
		newline ctx;
		spr ctx "}";
	);
	impl();
	newline ctx;
	spr ctx "}";
	newline ctx

let generate_class ctx c =
	ctx.curclass <- c;
	define_getset ctx true c;
	define_getset ctx false c;
	ctx.local_types <- List.map snd c.cl_types;
	ctx.constructor_block <- false;
	let obj_fields = List.filter (is_var) c.cl_ordered_fields in
	let obj_methods = List.filter(fun f ->
		let x = ref false in
		List.iter(fun ifacef ->
			x := !x || (f.cf_name = ifacef.cf_name);
		) c.cl_ordered_fields;
		!x && (not (is_var f))
	) c.cl_ordered_fields in
	let static_fields = List.filter(is_var) c.cl_ordered_statics in
	let static_methods = List.filter (fun x -> not (is_var x)) c.cl_ordered_statics in
	let params = get_params c.cl_types in
	let path = snd c.cl_path in
	ctx.in_interface <- c.cl_interface;
	print ctx "pub struct %s%s" path params;
	if ((List.length obj_fields) > 0) then (
		spr ctx " {";
		let st = open_block ctx in
		concat ctx ", " (fun f ->
			soft_newline ctx;
			generate_field ctx false f;
		) obj_fields;
		st();
		soft_newline ctx;
		spr ctx "}";
	);
	newline ctx;
	List.iter (generate_field ctx true) static_fields;
	if ((c.cl_constructor <> None) || ((List.length obj_methods) > 0) || (List.length c.cl_ordered_statics) > 0) && not c.cl_interface then (
		print ctx "pub impl%s %s%s {" params path params;
		let cl = open_block ctx in
		List.iter (generate_field ctx false) obj_methods;
		List.iter (generate_field ctx true) static_methods;
		(match c.cl_constructor with
			| None -> ();
			| Some f ->
				let f = { f with
					cf_name = "new";
					cf_public = true;
					cf_kind = Method MethNormal;
				} in
				ctx.constructor_block <- true;
				generate_field ctx false f;
				ctx.constructor_block <- false;
		);
		cl();
		newline ctx;
		print ctx "}";
		newline ctx;
	);
	if c.cl_interface then (
		print ctx "pub trait%s %s%s {" params path params;
		let tr = open_block ctx in
		soft_newline ctx;
		List.iter (generate_field ctx false) obj_methods;
		List.iter (generate_field ctx true) c.cl_ordered_statics;
		tr();
		newline ctx;
		spr ctx "}";
		newline ctx;
	);
	List.iter (fun (iface, iface_params) ->
		let tparams = s_tparams ctx iface_params c.cl_pos in
		let iface_path = snd iface.cl_path in
		print ctx "impl %s%s for %s%s {" iface_path tparams path params;
		let i = open_block ctx in
		let iface_fields = List.filter(fun f ->
			let x = ref false in
			List.iter(fun ifacef ->
				x := !x || (f.cf_name = ifacef.cf_name);
			) iface.cl_ordered_fields;
			!x && (not (is_var f))
		) c.cl_ordered_fields in
		List.iter(generate_field ctx false) iface_fields;
		i();
		newline ctx;
		spr ctx "}";
		newline ctx;
	) c.cl_implements;
	generate_obj_impl ctx c;
	()

let generate_enum ctx e =
	ctx.local_types <- List.map snd e.e_types;
	let params = get_params e.e_types in
	let ename = snd e.e_path in
	print ctx "pub enum %s%s {" ename params;
	let cl = open_block ctx in
	let not_first = ref false in
	PMap.iter (fun name c ->
		if !not_first then
			spr ctx ",";
		soft_newline ctx;
		(match c.ef_type with
		| TFun (args,_) ->
			print ctx "%s(" c.ef_name;
			concat ctx ", " (fun (a,o,t) ->
				print ctx "%s" (type_str ctx t c.ef_pos);
			) args;
			print ctx ")";
		| _ ->
			spr ctx c.ef_name;
		);
		not_first := true;
	) e.e_constrs;
	cl();
	soft_newline ctx;
	spr ctx "}";
	newline ctx

let generate_base_enum ctx com =
	spr ctx "pub trait HxEnum {";
	let trait = open_block ctx in
	newline ctx;
	spr ctx "pub fn __name() -> Option<@str>";
	newline ctx;
	spr ctx "pub fn __get_index(ind:i32) -> Self";
	newline ctx;
	spr ctx "pub fn __parameters(&self) -> Option<~[@HxObject]>";
	newline ctx;
	spr ctx "pub fn __index(&self) -> i32";
	newline ctx;
	spr ctx "pub fn __constructor(&self) -> @str";
	trait();
	newline ctx;
	spr ctx "}";
	newline ctx


let generate_base_object ctx com =
	spr ctx "pub trait HxObject {";
	let trait = open_block ctx in
	newline ctx;
	spr ctx "pub fn __name() -> Option<@str>";
	newline ctx;
	spr ctx "pub fn __field(&self, field:@str) -> Option<HxObject>";
	newline ctx;
	spr ctx "pub fn __set_field(&mut self, field:@str, value:Option<@HxObject>) -> Option<@HxObject>";
	newline ctx;
	spr ctx "pub fn toString(&self) -> @str";
	trait();
	newline ctx;
	spr ctx "}";
	newline ctx;
	spr ctx "impl ToStr for HxObject {";
	let obj = open_block ctx in
	newline ctx;
	spr ctx "pub fn to_str(&self) -> ~str {";
	let fn = open_block ctx in
	newline ctx;
	spr ctx "return self.toString()";
	fn();
	newline ctx;
	spr ctx "}";
	obj();
	newline ctx;
	spr ctx "}";
	newline ctx;
	List.iter(fun t ->
		match t with
		| TClassDecl c ->
			if c.cl_extern && not (is_package (snd c.cl_path)) && not (Meta.has Meta.NativeGen c.cl_meta) && Meta.has Meta.Native c.cl_meta then (
				let c = (match c.cl_path with
					| (pack,name) -> { c with cl_path = (pack,protect name) }
				) in
				generate_obj_impl ctx c
			)
		| _ -> ()
	) com.types;
	newline ctx

let generate_base_lib ctx com =
	newline ctx

let generate com =
	let infos = {
		com = com;
	} in
	let ctx = init infos ([], "lib") in
	generate_resources ctx infos;
	generate_base_enum ctx com;
	generate_base_object ctx com;
	generate_base_lib ctx com;
	close ctx;
	let inits = ref [] in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			let c = (match c.cl_path with
				| (pack,name) -> { c with cl_path = (pack,protect name) }
			) in
			(match c.cl_init with
			| None -> ()
			| Some e -> inits := e :: !inits);
			if not c.cl_extern then (
				let ctx = init infos c.cl_path in
				generate_class ctx c;
				close ctx
			)
		| TEnumDecl e ->
			let pack,name = e.e_path in
			let e = { e with e_path = (pack,protect name) } in
			if not e.e_extern then
				let ctx = init infos e.e_path in
				generate_enum ctx e;
				close ctx
		| TTypeDecl _ | TAbstractDecl _ ->
			()
	) com.types;
	(match com.main with
	| None -> ()
	| Some e -> inits := e :: !inits);
	let command cmd = try com.run_command cmd with _ -> -1 in
	if (command ("rustc \""^com.file^"/Test.rs\"") <> 0) then
		(failwith "Failed to compile Rust program");