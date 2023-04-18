(*
 * Copyright (C)2005-2019 Haxe Foundation
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
open Hlcode

type comparison =
	| CEq
	| CNeq
	| CLt
	| CGt
	| CLte
	| CGte

type output_options =
	| OOLabel
	| OOCase of int
	| OODefault
	| OOIncreaseIndent
	| OODecreaseIndent
	| OOBeginBlock
	| OOEndBlock
	| OOBreak

type code_module = {
	m_name : string;
	mutable m_functions : function_entry list;
	mutable m_types : ttype list;
}

and function_entry = {
	fe_index : int;
	mutable fe_name : string;
	mutable fe_decl : fundecl option;
	mutable fe_args : ttype list;
	mutable fe_ret : ttype;
	mutable fe_module : code_module option;
	mutable fe_called_by : function_entry list;
	mutable fe_calling : function_entry list;
}

type context = {
	version : int;
	out : Buffer.t;
	mutable tabs : string;
	hash_cache : (int, int32) Hashtbl.t;
	hash_mem : (int32, bool) Hashtbl.t;
	mutable hash_cache_list : int list;
	hlcode : code;
	dir : string;
	mutable curfile : string;
	mutable cfiles : string list;
	ftable : function_entry array;
	htypes : (ttype, string) PMap.t;
	gnames : string array;
	bytes_names : string array;
	mutable defines : string list;
	defined_funs : (int, unit) Hashtbl.t;
	hdefines : (string, unit) Hashtbl.t;
	mutable defined_types : (ttype, unit) PMap.t;
	mutable file_prefix : string;
	mutable fun_index : int;
	mutable type_module : (ttype, code_module) PMap.t;
	gcon : Common.context;
}

let sprintf = Printf.sprintf

let keywords =
	let c_kwds = [
	"auto";"break";"case";"char";"const";"continue";"default";"do";"double";"else";"enum";"extern";"float";"for";"goto";
	"if";"int";"long";"register";"return";"short";"signed";"sizeof";"static";"struct";"switch";"typedef";"union";"unsigned";
	"void";"volatile";"while";
	(* Values *)
	"NULL";"true";"false";
	(* MS specific *)
	"__asm";"dllimport2";"__int8";"naked2";"__based1";"__except";"__int16";"__stdcall";"__cdecl";"__fastcall";"__int32";
	"thread2";"__declspec";"__finally";"__int64";"__try";"dllexport2";"__inline";"__leave";"asm";
	(* reserved by HLC *)
	"t";
	(* GCC *)
	"typeof";
	(* C11 *)
	"_Alignas";"_Alignof";"_Atomic";"_Bool";"_Complex";"_Generic";"_Imaginary";"_Noreturn";"_Static_assert";"_Thread_local";"_Pragma";
	"inline";"restrict"
	] in
	let h = Hashtbl.create 0 in
	List.iter (fun i -> Hashtbl.add h i ()) c_kwds;
	h

let ident i = if Hashtbl.mem keywords i then "_" ^ i else i

let s_comp = function
	| CLt -> "<"
	| CGt -> ">"
	| CEq -> "=="
	| CLte -> "<="
	| CGte -> ">="
	| CNeq -> "!="

let core_types =
	let vp = { vfields = [||]; vindex = PMap.empty } in
	let ep = { ename = ""; eid = 0; eglobal = None; efields = [||] } in
	[HVoid;HUI8;HUI16;HI32;HI64;HF32;HF64;HBool;HBytes;HDyn;HFun ([],HVoid);HObj null_proto;HArray;HType;HRef HVoid;HVirtual vp;HDynObj;HAbstract ("",0);HEnum ep;HNull HVoid;HMethod ([],HVoid);HStruct null_proto]

let tname str =
	let n = String.concat "__" (ExtString.String.nsplit str ".") in
	if Hashtbl.mem keywords ("_" ^ n) then "__" ^ n else n

let is_gc_ptr = function
	| HVoid | HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64 | HBool | HType | HRef _ | HMethod _ | HPacked _ -> false
	| HBytes | HDyn | HFun _ | HObj _ | HArray | HVirtual _ | HDynObj | HAbstract _ | HEnum _ | HNull _ | HStruct _ -> true

let is_ptr = function
	| HVoid | HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64 | HBool -> false
	| _ -> true

let rec ctype_no_ptr = function
	| HVoid -> "void",0
	| HUI8 -> "unsigned char",0
	| HUI16 -> "unsigned short",0
	| HI32 -> "int",0
	| HI64 -> "int64",0
	| HF32 -> "float",0
	| HF64 -> "double",0
	| HBool -> "bool",0
	| HBytes -> "vbyte",1
	| HDyn -> "vdynamic",1
	| HFun _ -> "vclosure",1
	| HObj p | HStruct p -> tname p.pname,0
	| HArray -> "varray",1
	| HType -> "hl_type",1
	| HRef t -> let s,i = ctype_no_ptr t in s,i + 1
	| HVirtual _ -> "vvirtual",1
	| HDynObj -> "vdynobj",1
	| HAbstract (name,_) -> name,1
	| HEnum _ -> "venum",1
	| HNull _ -> "vdynamic",1
	| HMethod _ -> "void",1
	| HPacked t ->
		let name,v = ctype_no_ptr t in
		"struct _" ^ name, v

let ctype t =
	let t, nptr = ctype_no_ptr t in
	if nptr = 0 then t else t ^ String.make nptr '*'

let args_repr args =
	if args = [] then "void" else String.concat "," (List.map ctype args)

let cast_fun s args t =
	sprintf "((%s (*)(%s))%s)" (ctype t) (args_repr args) s

let dyn_value_field t =
	"->v." ^ match t with
	| HUI8 -> "ui8"
	| HUI16 -> "ui16"
	| HI32 -> "i"
	| HI64 -> "i64"
	| HF32 -> "f"
	| HF64 -> "d"
	| HBool -> "b"
	| _ -> "ptr"

let type_id t =
	match t with
	| HVoid -> "HVOID"
	| HUI8 -> "HUI8"
	| HUI16 -> "HUI16"
	| HI32 -> "HI32"
	| HI64 -> "HI64"
	| HF32 -> "HF32"
	| HF64 -> "HF64"
	| HBool -> "HBOOL"
	| HBytes -> "HBYTES"
	| HDyn -> "HDYN"
	| HFun _ -> "HFUN"
	| HObj _ -> "HOBJ"
	| HArray -> "HARRAY"
	| HType -> "HTYPE"
	| HRef _ -> "HREF"
	| HVirtual _ -> "HVIRTUAL"
	| HDynObj -> "HDYNOBJ"
	| HAbstract _ -> "HABSTRACT"
	| HEnum _ -> "HENUM"
	| HNull _ -> "HNULL"
	| HMethod _ -> "HMETHOD"
	| HStruct _  -> "HSTRUCT"
	| HPacked _ -> "HPACKED"

let var_type n t =
	ctype t ^ " " ^ ident n

let block ctx =
	ctx.tabs <- ctx.tabs ^ "\t"

let unblock ctx =
	ctx.tabs <- String.sub ctx.tabs 0 (String.length ctx.tabs - 1)

let hash ctx sid =
	try
		Hashtbl.find ctx.hash_cache sid
	with Not_found ->
		let rec loop h =
			if Hashtbl.mem ctx.hash_mem h then loop (Int32.add h Int32.one) else h
		in
		let h = loop (hl_hash ctx.hlcode.strings.(sid)) in
		Hashtbl.add ctx.hash_cache sid h;
		Hashtbl.add ctx.hash_mem h true;
		ctx.hash_cache_list <- sid :: ctx.hash_cache_list;
		h

let type_name ctx t =
	try PMap.find t ctx.htypes with Not_found -> Globals.die (tstr t) __LOC__

let define ctx s =
	if not (Hashtbl.mem ctx.hdefines s) then begin
		ctx.defines <- s :: ctx.defines;
		Hashtbl.add ctx.hdefines s ();
	end

let rec define_type ctx t =
	match t with
	| HVoid | HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64 | HBool | HBytes | HDyn | HArray | HType | HDynObj | HNull _ | HRef _ -> ()
	| HAbstract _ ->
		define ctx "#include <hl/natives.h>";
	| HFun (args,ret) | HMethod (args,ret) ->
		List.iter (define_type ctx) args;
		define_type ctx ret
	| HEnum _ | HObj _ | HStruct _ when not (PMap.exists t ctx.defined_types) ->
		ctx.defined_types <- PMap.add t () ctx.defined_types;
		define ctx (sprintf "#include <%s.h>" (try PMap.find t ctx.type_module with Not_found -> Globals.die "" __LOC__).m_name)
	| HVirtual vp when not (PMap.exists t ctx.defined_types) ->
		ctx.defined_types <- PMap.add t () ctx.defined_types;
		Array.iter (fun (_,_,t) -> define_type ctx t) vp.vfields
	| HPacked t ->
		define_type ctx t
	| HEnum _ | HObj _ | HStruct _ | HVirtual _ ->
		()

let type_value ctx t =
	let n = type_name ctx t in
	define ctx (sprintf "extern hl_type %s;" n);
	"&" ^ n

let enum_constr_type ctx e i =
	define_type ctx (HEnum e);
	let cname,_, tl = e.efields.(i) in
	if Array.length tl = 0 then
		"venum"
	else
	let name = if e.eid = 0 then
		let name = (try PMap.find (HEnum e) ctx.htypes with Not_found -> Globals.die "" __LOC__) in
		"Enum" ^ name
	else
		String.concat "_" (ExtString.String.nsplit e.ename ".")
	in
	if cname = "" then
		name
	else
		name ^ "_" ^ cname

let output ctx str =
	Buffer.add_string ctx.out str

let output_char ctx c =
	Buffer.add_char ctx.out c

let line ctx str =
	output ctx ctx.tabs;
	output ctx str;
	output_char ctx '\n'

let expr ctx str =
	output ctx ctx.tabs;
	output ctx str;
	output ctx ";\n"

let unamed_field fid = "f$" ^ string_of_int fid

let obj_field fid name =
	if name = "" then unamed_field fid else ident name

let bom = "\xEF\xBB\xBF"

let close_file ctx =
	let out = Buffer.contents ctx.out in
	let defines = List.rev ctx.defines in
	let content = (match defines with [] -> out | l -> String.concat "\n" l ^ "\n\n" ^ out) in
	let str = if ctx.curfile = "hlc.json" then content else bom ^ content in
	ctx.defines <- [];
	ctx.defined_types <- PMap.empty;
	Hashtbl.clear ctx.hdefines;
	Hashtbl.clear ctx.defined_funs;
	Buffer.reset ctx.out;
	let fpath = ctx.dir ^ "/" ^ ctx.curfile in
	if String.sub ctx.curfile (String.length ctx.curfile - 2) 2 = ".c" then ctx.cfiles <- ctx.curfile :: ctx.cfiles;
	ctx.curfile <- "";
	let fcontent = (try Std.input_file ~bin:true fpath with _ -> "") in
	if fcontent <> str then begin
		Path.mkdir_recursive "" (ExtString.String.nsplit (Filename.dirname fpath) "/");
		let ch = open_out_bin fpath in
		output_string ch str;
		close_out ch;
	end

let define_global ctx g =
	let t = ctx.hlcode.globals.(g) in
	define_type ctx t;
	define ctx (sprintf "extern %s;" (var_type ctx.gnames.(g) t))

let define_function ctx fid =
	let ft = ctx.ftable.(fid) in
	let fid = if ft.fe_decl = None then -1 else fid in
	if not (Hashtbl.mem ctx.defined_funs fid) then begin
		Hashtbl.add ctx.defined_funs fid ();
		(match ft.fe_decl with
		| None ->
			define ctx "#include <hl/natives.h>"
		| Some f ->
			define_type ctx f.ftype;
			ctx.defines <- sprintf "%s %s(%s);" (ctype ft.fe_ret) ft.fe_name (args_repr ft.fe_args) :: ctx.defines);
	end;
	ft.fe_name

let short_digest str =
	String.sub (Digest.to_hex (Digest.string str)) 0 7

let open_file ctx file =
	if ctx.curfile <> "" then close_file ctx;
	if file <> "hlc.json" then
		Codegen.map_source_header ctx.gcon (fun s -> define ctx (sprintf "// %s" s));
	ctx.curfile <- file;
	ctx.fun_index <- 0;
	ctx.file_prefix <- (short_digest file) ^ "_"

let string_data_limit = 64

let string ctx sid =
	let s = ctx.hlcode.strings.(sid) in
	if String.length s < string_data_limit then
		sprintf "USTR(\"%s\")" (StringHelper.s_escape s)
	else begin
		let id = short_digest s in
		define ctx (sprintf "extern vbyte string$%s[];" id);
		sprintf "string$%s" id
	end

let generate_reflection ctx =
	let line = line ctx and expr = expr ctx in
	let sline fmt = Printf.ksprintf line fmt and sexpr fmt = Printf.ksprintf expr fmt in

	let funByArgs = Hashtbl.create 0 in
	let type_kind t =
		match t with
		| HVoid | HF32 | HF64 | HI64 -> t
		| HBool | HUI8 | HUI16 | HI32 -> HI32
		| _ -> HDyn
	in
	let type_kind_id t =
		match t with
		| HVoid -> 0
		| HBool | HUI8 | HUI16 | HI32 -> 1 (* same int representation *)
		| HF32 -> 2
		| HF64 -> 3
		| HI64 -> 4
		| _ -> 5
	in
	let add_fun args t =
		let nargs = List.length args in
		let kargs = List.map type_kind args in
		let kt = type_kind t in
		let h = try Hashtbl.find funByArgs nargs with Not_found -> let h = Hashtbl.create 0 in Hashtbl.add funByArgs nargs h; h in
		Hashtbl.replace h (kargs,kt) ()
	in
	Array.iter (fun f ->
		Array.iter (fun op ->
			match op with
			| OSafeCast (dst,_) | ODynGet (dst,_,_) ->
				(match f.regs.(dst) with
				| HFun (args, t) -> add_fun args t
				| _ -> ())
			| _ -> ()
		) f.code
	) ctx.hlcode.functions;
	Array.iter (fun f -> add_fun f.fe_args f.fe_ret) ctx.ftable;
	let argsCounts = List.sort compare (Hashtbl.fold (fun i _ acc -> i :: acc) funByArgs []) in
	sexpr "static int TKIND[] = {%s}" (String.concat "," (List.map (fun t -> string_of_int (type_kind_id (type_kind t))) core_types));
	line "";
	line "void *hlc_static_call( void *fun, hl_type *t, void **args, vdynamic *out ) {";
	block ctx;
	sexpr "int chk = TKIND[t->fun->ret->kind]";
	sexpr "vdynamic *d";
	line "switch( t->fun->nargs ) {";
	List.iter (fun nargs ->
		sline "case %d:" nargs;
		block ctx;
		if nargs > 9 then sexpr "hl_fatal(\"Too many arguments, TODO:use more bits\")" else begin
		for i = 0 to nargs-1 do
			sexpr "chk |= TKIND[t->fun->args[%d]->kind] << %d" i ((i + 1) * 3);
		done;
		line "switch( chk ) {";
		Hashtbl.iter (fun (args,t) _ ->
			let s = ref (-1) in
			let chk = List.fold_left (fun chk t -> incr s; chk lor ((type_kind_id t) lsl (!s * 3))) 0 (t :: args) in
			sline "case %d:" chk;
			block ctx;
			let idx = ref (-1) in
			let vargs = List.map (fun t ->
				incr idx;
				if is_ptr t then
					sprintf "(%s)args[%d]" (ctype t) !idx
				else
					sprintf "*(%s*)args[%d]" (ctype t) !idx
			) args in
			let call = sprintf "%s(%s)" (cast_fun "fun" args t) (String.concat "," vargs) in
			if is_ptr t then
				sexpr "return %s" call
			else if t = HVoid then begin
				expr call;
				expr "return NULL";
			end else begin
				sexpr "out%s = %s" (dyn_value_field t) call;
				sexpr "return &out%s" (dyn_value_field t);
			end;
			unblock ctx;
		) (Hashtbl.find funByArgs nargs);
		sline "}";
		expr "break";
		end;
		unblock ctx;
	) argsCounts;
	line "}";
	sexpr "hl_fatal(\"Unsupported dynamic call\")";
	sexpr "return NULL";
	unblock ctx;
	line "}";
	line "";
	let wrap_char = function
		| HVoid -> "v"
		| HUI8 | HUI16 | HBool | HI32 -> "i"
		| HF32 -> "f"
		| HF64 -> "d"
		| HI64 -> "i64"
		| _ -> "p"
	in
	let make_wrap_name args t =
		String.concat "" (List.map wrap_char args) ^ "_" ^ wrap_char t
	in
	List.iter (fun nargs ->
		Hashtbl.iter (fun (args,t) _ ->
			let name = make_wrap_name args t in
			sline "static %s wrap_%s(void *value%s) {" (ctype t) name (String.concat "" (list_mapi (fun i t -> "," ^ var_type ("p" ^ string_of_int i) t) args));
			block ctx;
			if args <> [] then sexpr "void *args[] = {%s}" (String.concat "," (list_mapi (fun i t ->
				if not (is_ptr t) then
					sprintf "&p%d" i
				else
					sprintf "p%d" i
			) args));
			let vargs = if args = [] then "NULL" else "args" in
			if t = HVoid then
				sexpr "hl_wrapper_call(value,%s,NULL)" vargs
			else if is_ptr t then
				sexpr "return hl_wrapper_call(value,%s,NULL)" vargs
			else begin
				expr "vdynamic ret";
				sexpr "hl_wrapper_call(value,%s,&ret)" vargs;
				sexpr "return ret.v.%s" (wrap_char t);
			end;
			unblock ctx;
			line "}";
		) (Hashtbl.find funByArgs nargs);
	) argsCounts;
	line "";
	line "void *hlc_get_wrapper( hl_type *t ) {";
	block ctx;
	sexpr "int chk = TKIND[t->fun->ret->kind]";
	line "switch( t->fun->nargs ) {";
	List.iter (fun nargs ->
		sline "case %d:" nargs;
		block ctx;
		if nargs > 9 then sexpr "hl_fatal(\"Too many arguments, TODO:use more bits\")" else begin
		for i = 0 to nargs-1 do
			sexpr "chk |= TKIND[t->fun->args[%d]->kind] << %d" i ((i + 1) * 3);
		done;
		line "switch( chk ) {";
		Hashtbl.iter (fun (args,t) _ ->
			let s = ref (-1) in
			let chk = List.fold_left (fun chk t -> incr s; chk lor ((type_kind_id t) lsl (!s * 3))) 0 (t :: args) in
			sexpr "case %d: return wrap_%s" chk (make_wrap_name args t);
		) (Hashtbl.find funByArgs nargs);
		sline "}";
		expr "break";
		end;
		unblock ctx;
	) argsCounts;
	line "}";
	sexpr "return NULL";
	unblock ctx;
	line "}";
	line ""

let generate_function ctx f =
	let line = line ctx and expr = expr ctx in
	let sline fmt = Printf.ksprintf line fmt and sexpr fmt = Printf.ksprintf expr fmt in
	let define fmt = Printf.ksprintf (define ctx) fmt in
	let block() = block ctx and unblock() = unblock ctx in
	let type_value = type_value ctx in
	let code = ctx.hlcode in

	let rid = ref (-1) in
	let cl_id = ref 0 in
	let reg id = "r" ^ string_of_int id in

	let label p = sprintf "label$%s%d_%d" ctx.file_prefix ctx.fun_index p in
	ctx.fun_index <- ctx.fun_index + 1;
	Hashtbl.add ctx.defined_funs f.findex ();
	Array.iter (define_type ctx) f.regs;
	define_type ctx f.ftype;

	let rtype r = f.regs.(r) in

	let funname fid = define_function ctx fid in

	let rcast r t =
		if tsame (rtype r) t then (reg r)
		else Printf.sprintf "((%s)%s)" (ctype t) (reg r)
	in

	let rfun r args t =
		cast_fun (reg r ^ "->fun") args t
	in

	let rassign r t =
		let rt = rtype r in
		if t = HVoid then "" else
		let assign = reg r ^ " = " in
		if tsame t rt then assign else
		if not (safe_cast t rt) then Globals.die "" __LOC__
		else
			let cast = assign ^ "(" ^ ctype rt ^ ")" in
			(match t with HPacked _ -> cast ^ "&" | _ -> cast)
	in

	let ocall r fid args =
		let ft = ctx.ftable.(fid) in
		let rstr = rassign r ft.fe_ret in
		sexpr "%s%s(%s)" rstr (funname fid) (String.concat "," (List.map2 rcast args ft.fe_args))
	in


	let dyn_prefix = function
		| HUI8 | HUI16 | HI32 | HBool -> "i"
		| HF32 -> "f"
		| HF64 -> "d"
		| HI64 -> "i64"
		| _ -> "p"
	in

	let type_value_opt t =
		match t with HF32 | HF64 | HI64 -> "" | _ -> "," ^ type_value t
	in

	let dyn_call r f pl =
		line "{";
		block();
		if pl <> [] then sexpr "vdynamic *args[] = {%s}" (String.concat "," (List.map (fun p ->
			match rtype p with
			| HDyn ->
				reg p
			| t ->
				if is_dynamic t then
					sprintf "(vdynamic*)%s" (reg p)
				else
					sprintf "hl_make_dyn(&%s,%s)" (reg p) (type_value t)
		) pl));
		let rt = rtype r in
		let ret = if rt = HVoid then "" else if is_dynamic rt then sprintf "%s = (%s)" (reg r) (ctype rt) else "vdynamic *ret = " in
		sexpr "%shl_dyn_call((vclosure*)%s,%s,%d)" ret (reg f) (if pl = [] then "NULL" else "args") (List.length pl);
		if rt <> HVoid && not (is_dynamic rt) then sexpr "%s = (%s)hl_dyn_cast%s(&ret,&hlt_dyn%s)" (reg r) (ctype rt) (dyn_prefix rt) (type_value_opt rt);
		unblock();
		line "}";
	in

	let mcall r fid = function
		| [] -> Globals.die "" __LOC__
		| o :: args ->
			match rtype o with
			| HObj _ | HStruct _ ->
				let vfun = cast_fun (sprintf "%s->$type->vobj_proto[%d]" (reg o) fid) (rtype o :: List.map rtype args) (rtype r) in
				sexpr "%s%s(%s)" (rassign r (rtype r)) vfun (String.concat "," (List.map reg (o::args)))
			| HVirtual vp ->
				let rt = rtype r in
				let meth = sprintf "hl_vfields(%s)[%d]" (reg o) fid in
				let meth = cast_fun meth (HDyn :: List.map rtype args) rt in
				sline "if( hl_vfields(%s)[%d] ) %s%s(%s); else {" (reg o) fid (rassign r rt) meth (String.concat "," ((reg o ^ "->value") :: List.map reg args));
				block();
				if args <> [] then sexpr "void *args[] = {%s}" (String.concat "," (List.map (fun p ->
					let t = rtype p in
					if is_ptr t then
						reg p
					else
						sprintf "&%s" (reg p)
				) args));
				let rt = rtype r in
				let ret = if rt = HVoid then "" else if is_ptr rt then sprintf "%s = (%s)" (reg r) (ctype rt) else begin sexpr "vdynamic ret"; ""; end in
				let fname, fid, ft = vp.vfields.(fid) in
				sexpr "%shl_dyn_call_obj(%s->value,%s,%ld/*%s*/,%s,%s)" ret (reg o) (type_value ft) (hash ctx fid) fname (if args = [] then "NULL" else "args") (if is_ptr rt || rt == HVoid then "NULL" else "&ret");
				if rt <> HVoid && not (is_ptr rt) then sexpr "%s = (%s)ret.v.%s" (reg r) (ctype rt) (dyn_prefix rt);
				unblock();
				sline "}"
			| _ ->
				Globals.die "" __LOC__
	in

	let set_field obj fid v =
		match rtype obj with
		| HObj o | HStruct o ->
			let name, t = resolve_field o fid in
			sexpr "%s->%s = %s" (reg obj) (obj_field fid name) (rcast v t)
		| HVirtual vp ->
			let name, nid, t = vp.vfields.(fid) in
			let dset = sprintf "hl_dyn_set%s(%s->value,%ld/*%s*/%s,%s)" (dyn_prefix t) (reg obj) (hash ctx nid) name (type_value_opt (rtype v)) (reg v) in
			sexpr "if( hl_vfields(%s)[%d] ) *(%s*)(hl_vfields(%s)[%d]) = (%s)%s; else %s" (reg obj) fid (ctype t) (reg obj) fid (ctype t) (reg v) dset
		| _ ->
			Globals.die "" __LOC__
	in

	let get_field r obj fid =
		match rtype obj with
		| HObj o | HStruct o ->
			let name, t = resolve_field o fid in
			sexpr "%s%s->%s" (rassign r t) (reg obj) (obj_field fid name)
		| HVirtual v ->
			let name, nid, t = v.vfields.(fid) in
			let dget = sprintf "(%s)hl_dyn_get%s(%s->value,%ld/*%s*/%s)" (ctype t) (dyn_prefix t) (reg obj) (hash ctx nid) name (type_value_opt t) in
			sexpr "%shl_vfields(%s)[%d] ? (*(%s*)(hl_vfields(%s)[%d])) : %s" (rassign r t) (reg obj) fid (ctype t) (reg obj) fid dget
		| _ ->
			Globals.die "" __LOC__
	in

	let fret = (match f.ftype with
	| HFun (args,t) ->
		sline "%s %s(%s) {" (ctype t) (funname f.findex) (String.concat "," (List.map (fun t -> incr rid; var_type (reg !rid) t) args));
		t
	| _ ->
		Globals.die "" __LOC__
	) in
	block();
	let var_map = Hashtbl.create 0 in
	Array.iteri (fun i t ->
		if i <= !rid || t = HVoid then ()
		else
			let key = ctype_no_ptr t in
			Hashtbl.replace var_map key (try (reg i) :: Hashtbl.find var_map key with Not_found -> [reg i])
	) f.regs;
	Hashtbl.iter (fun (s,i) il ->
		let prefix = String.make i '*' in
		let il = List.rev_map (fun s -> prefix ^ s) il in
		sexpr "%s %s" s (String.concat ", " il)
	) var_map;
	let output_options = Array.make (Array.length f.code + 1) [] in
	let output_at i oo = output_options.(i) <- oo :: output_options.(i) in
	let output_at2 i ool = List.iter (output_at i) ool in
	let has_label i = List.exists (function OOLabel -> true | _ -> false) output_options.(i) in

	let trap_depth = ref 0 in
	let max_trap_depth = ref 0 in
	Array.iter (fun op ->
		match op with
		| OTrap _ ->
			incr trap_depth;
			if !trap_depth > !max_trap_depth then max_trap_depth := !trap_depth
		| OEndTrap true ->
			decr trap_depth
		| OStaticClosure (_, fid) ->
			let ft = ctx.ftable.(fid) in
			sexpr "static vclosure cl$%d = { %s, %s, 0 }" (!cl_id) (type_value (HFun (ft.fe_args,ft.fe_ret))) (funname fid);
			incr cl_id;
		| _ ->
			()
	) f.code;
	for i = 0 to !max_trap_depth - 1 do
		sexpr "hl_trap_ctx trap$%d" i;
	done;
	cl_id := 0;

	let flush_options i =
		match output_options.(i) with
		| [] -> ()
		| opts ->
			(* put label after } *)
			let opts = if has_label i && List.mem OOEndBlock opts then OOLabel :: List.filter (fun i -> i <> OOLabel) opts else opts in
			let opts = List.rev opts in
			List.iter (function
				| OOLabel -> sline "%s:" (label i)
				| OOCase i -> sline "case %i:" i
				| OODefault -> line "default:"
				| OOBreak -> line "break;";
				| OOIncreaseIndent -> block()
				| OODecreaseIndent -> unblock()
				| OOBeginBlock ->  line "{"
				| OOEndBlock -> line "}"
			) opts
	in

	Array.iteri (fun i op ->
		flush_options i;
		let label delta =
			let addr = delta + i + 1 in
			let label = label addr in
			if not (has_label addr) then output_at addr OOLabel;
			label
		in
		let todo() =
			sexpr "hl_fatal(\"%s\")" (ostr (fun id -> "f" ^ string_of_int id) op)
		in
		let rec compare_op op a b d =
			let phys_compare() =
				sexpr "if( %s %s %s ) goto %s" (reg a) (s_comp op) (rcast b (rtype a)) (label d)
			in
			(*
				safe_cast is already checked
				two ways (same type) for eq
				one way for comparisons
			*)
			match rtype a, rtype b with
			| (HUI8 | HUI16 | HI32 | HF32 | HF64 | HBool | HI64), (HUI8 | HUI16 | HI32 | HF32 | HF64 | HBool | HI64) ->
				phys_compare()
			| HType, HType ->
				sexpr "if( hl_same_type(%s,%s) %s 0 ) {} else goto %s" (reg a) (reg b) (s_comp op) (label d)
			| HNull t, HNull _ ->
				let field = dyn_value_field t in
				let pcompare = sprintf "(%s%s %s %s%s)" (reg a) field (s_comp op) (reg b) field in
				if op = CEq then
					sexpr "if( %s == %s || (%s && %s && %s) ) goto %s" (reg a) (reg b) (reg a) (reg b) pcompare (label d)
				else if op = CNeq then
					sexpr "if( %s != %s && (!%s || !%s || %s) ) goto %s" (reg a) (reg b) (reg a) (reg b) pcompare (label d)
				else
					sexpr "if( %s && %s && %s ) goto %s" (reg a) (reg b) pcompare (label d)
			| (HDyn | HFun _), _ | _, (HDyn | HFun _) ->
				let inv = if op = CGt || op = CGte then "&& i != hl_invalid_comparison " else "" in
				sexpr "{ int i = hl_dyn_compare((vdynamic*)%s,(vdynamic*)%s); if( i %s 0 %s) goto %s; }" (reg a) (reg b) (s_comp op) inv (label d)
			| HObj oa, HObj _ ->
				(try
					let fid = PMap.find "__compare" oa.pfunctions in
					if op = CEq then
						sexpr "if( %s == %s || (%s && %s && %s(%s,(vdynamic*)%s) == 0) ) goto %s" (reg a) (reg b) (reg a) (reg b) (funname fid) (reg a) (reg b) (label d)
					else if op = CNeq then
						sexpr "if( %s != %s && (!%s || !%s || %s(%s,(vdynamic*)%s) != 0) ) goto %s" (reg a) (reg b) (reg a) (reg b) (funname fid) (reg a) (reg b) (label d)
					else
						sexpr "if( %s && %s && %s(%s,(vdynamic*)%s) %s 0 ) goto %s" (reg a) (reg b) (funname fid) (reg a) (reg b) (s_comp op) (label d)
				with Not_found ->
					phys_compare())
			| HStruct _, HStruct _ ->
				phys_compare()
			| HVirtual _, HVirtual _ ->
				if op = CEq then
					sexpr "if( %s == %s || (%s && %s && %s->value && %s->value && %s->value == %s->value) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg a) (reg b) (reg a) (reg b) (label d)
				else if op = CNeq then
					sexpr "if( %s != %s && (!%s || !%s || !%s->value || !%s->value || %s->value != %s->value) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg a) (reg b) (reg a) (reg b) (label d)
				else
					Globals.die "" __LOC__
			| HEnum _, HEnum _ | HDynObj, HDynObj | HAbstract _, HAbstract _ ->
				phys_compare()
			| HVirtual _, HObj _->
				if op = CEq then
					sexpr "if( %s ? (%s && %s->value == (vdynamic*)%s) : (%s == NULL) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg b) (label d)
				else if op = CNeq then
					sexpr "if( %s ? (%s == NULL || %s->value != (vdynamic*)%s) : (%s != NULL) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg b) (label d)
				else
					Globals.die "" __LOC__
			| HObj _, HVirtual _ ->
				compare_op op b a d
			| ta, tb ->
				failwith ("Don't know how to compare " ^ tstr ta ^ " and " ^ tstr tb ^ " (hlc)")
		in
		match op with
		| OMov (r,v) ->
			if rtype r <> HVoid then sexpr "%s = %s" (reg r) (rcast v (rtype r))
		| OInt (r,idx) ->
			if code.ints.(idx) = 0x80000000l then
				sexpr "%s = 0x80000000" (reg r)
			else
				sexpr "%s = %ld" (reg r) code.ints.(idx)
		| OFloat (r,idx) ->
			let fstr = sprintf "%.19g" code.floats.(idx) in
			sexpr "%s = %s" (reg r) (if String.contains fstr '.' || String.contains fstr 'e' then fstr else fstr ^ ".")
		| OBool (r,b) ->
			sexpr "%s = %s" (reg r) (if b then "true" else "false")
		| OBytes (r,idx) ->
			define "extern vbyte %s[];" ctx.bytes_names.(idx);
			sexpr "%s = %s" (reg r) ctx.bytes_names.(idx)
		| OString (r,idx) ->
			sexpr "%s = (vbyte*)%s" (reg r) (string ctx idx)
		| ONull r ->
			sexpr "%s = NULL" (reg r)
		| OAdd (r,a,b) ->
			sexpr "%s = %s + %s" (reg r) (reg a) (reg b)
		| OSub (r,a,b) ->
			sexpr "%s = %s - %s" (reg r) (reg a) (reg b)
		| OMul (r,a,b) ->
			sexpr "%s = %s * %s" (reg r) (reg a) (reg b)
		| OSDiv (r,a,b) ->
			(match rtype r with
			| HUI8 | HUI16 | HI32 ->
				sexpr "%s = %s == 0 ? 0 : %s / %s" (reg r) (reg b) (reg a) (reg b)
			| _ ->
				sexpr "%s = %s / %s" (reg r) (reg a) (reg b))
		| OUDiv (r,a,b) ->
			sexpr "%s = %s == 0 ? 0 : ((unsigned)%s) / ((unsigned)%s)" (reg r) (reg b) (reg a) (reg b)
		| OSMod (r,a,b) ->
			(match rtype r with
			| HUI8 | HUI16 | HI32 ->
				sexpr "%s = %s == 0 ? 0 : %s %% %s" (reg r) (reg b) (reg a) (reg b)
			| HF32 ->
				sexpr "%s = fmodf(%s,%s)" (reg r) (reg a) (reg b)
			| HF64 ->
				sexpr "%s = fmod(%s,%s)" (reg r) (reg a) (reg b)
			| _ ->
				Globals.die "" __LOC__)
		| OUMod (r,a,b) ->
			sexpr "%s = %s == 0 ? 0 : ((unsigned)%s) %% ((unsigned)%s)" (reg r) (reg b) (reg a) (reg b)
		| OShl (r,a,b) ->
			sexpr "%s = %s << %s" (reg r) (reg a) (reg b)
		| OSShr (r,a,b) ->
			sexpr "%s = %s >> %s" (reg r) (reg a) (reg b)
		| OUShr (r,a,b) ->
			sexpr "%s = ((unsigned)%s) >> %s" (reg r) (reg a) (reg b)
		| OAnd (r,a,b) ->
			sexpr "%s = %s & %s" (reg r) (reg a) (reg b)
		| OOr (r,a,b) ->
			sexpr "%s = %s | %s" (reg r) (reg a) (reg b)
		| OXor (r,a,b) ->
			sexpr "%s = %s ^ %s" (reg r) (reg a) (reg b)
		| ONeg (r,v) ->
			sexpr "%s = -%s" (reg r) (reg v)
		| ONot (r,v) ->
			sexpr "%s = !%s" (reg r) (reg v)
		| OIncr r ->
			sexpr "++%s" (reg r)
		| ODecr r ->
			sexpr "--%s" (reg r)
		| OCall0 (r,fid) ->
			ocall r fid []
		| OCall1 (r,fid,a) ->
			ocall r fid [a]
		| OCall2 (r,fid,a,b) ->
			ocall r fid [a;b]
		| OCall3 (r,fid,a,b,c) ->
			ocall r fid [a;b;c]
		| OCall4 (r,fid,a,b,c,d) ->
			ocall r fid [a;b;c;d]
		| OCallN (r,fid,rl) ->
			ocall r fid rl
		| OCallMethod (r,fid,pl) ->
			mcall r fid pl
		| OCallThis (r,fid,pl) ->
			mcall r fid (0 :: pl)
		| OCallClosure (r,cl,pl) ->
			(match rtype cl with
			| HDyn ->
				dyn_call r cl pl
			| HFun (args,ret) ->
				let sargs = String.concat "," (List.map2 rcast pl args) in
				sexpr "%s%s->hasValue ? %s((vdynamic*)%s->value%s) : %s(%s)" (rassign r ret) (reg cl) (rfun cl (HDyn :: args) ret) (reg cl) (if sargs = "" then "" else "," ^ sargs) (rfun cl args ret) sargs
			| _ ->
				Globals.die "" __LOC__)
		| OStaticClosure (r,fid) ->
			sexpr "%s = &cl$%d" (reg r) (!cl_id);
			incr cl_id
		| OInstanceClosure (r,fid,ptr) ->
			let ft = ctx.ftable.(fid) in
			sexpr "%s = hl_alloc_closure_ptr(%s,%s,%s)" (reg r) (type_value (HFun (ft.fe_args,ft.fe_ret))) (funname fid) (reg ptr)
		| OVirtualClosure (r,o,m) ->
			(match rtype o with
			| HObj p ->
				let ft = ctx.ftable.(p.pvirtuals.(m)) in
				let s = sprintf "%s->$type->vobj_proto[%d]" (reg o) m in
				sexpr "%s = hl_alloc_closure_ptr(%s,%s,%s)" (reg r) (type_value (HFun(ft.fe_args,ft.fe_ret))) s (reg o)
			| _ ->
				todo())
		| OGetGlobal (r,g) ->
			define_global ctx g;
			sexpr "%s = (%s)%s" (reg r) (ctype (rtype r)) ctx.gnames.(g)
		| OSetGlobal (g,r) ->
			define_global ctx g;
			sexpr "%s = (%s)%s" ctx.gnames.(g) (ctype code.globals.(g)) (reg r)
		| ORet r ->
			if rtype r = HVoid then expr "return" else sexpr "return %s" (rcast r fret)
		| OJTrue (r,d) | OJNotNull (r,d) ->
			sexpr "if( %s ) goto %s" (reg r) (label d)
		| OJFalse (r,d) | OJNull (r,d) ->
			sexpr "if( !%s ) goto %s" (reg r) (label d)
		| OJSLt (a,b,d) ->
			compare_op CLt a b d
		| OJSGte (a,b,d) ->
			compare_op CGte a b d
		| OJSGt (a,b,d) ->
			compare_op CGt a b d
		| OJSLte (a,b,d) ->
			compare_op CLte a b d
		| OJULt (a,b,d) ->
			sexpr "if( ((unsigned)%s) < ((unsigned)%s) ) goto %s" (reg a) (reg b) (label d)
		| OJUGte (a,b,d) ->
			sexpr "if( ((unsigned)%s) >= ((unsigned)%s) ) goto %s" (reg a) (reg b) (label d)
		| OJNotLt (a,b,d) ->
			sexpr "if( !(%s < %s) ) goto %s" (reg a) (reg b) (label d)
		| OJNotGte (a,b,d) ->
			sexpr "if( !(%s >= %s) ) goto %s" (reg a) (reg b) (label d)
		| OJEq (a,b,d) ->
			compare_op CEq a b d
		| OJNotEq (a,b,d) ->
			compare_op CNeq a b d
		| OJAlways d ->
			sexpr "goto %s" (label d)
		| OLabel _ ->
			if not (has_label i) then sline "%s:" (label (-1))
		| OToDyn (r,v) when rtype v = HBool ->
			sexpr "%s = hl_alloc_dynbool(%s)" (reg r) (reg v)
		| OToDyn (r,v) ->
			if is_ptr (rtype v) then begin
				sline "if( %s == NULL ) %s = NULL; else {" (reg v) (reg r);
				block();
			end;
			sexpr "%s = hl_alloc_dynamic(%s)" (reg r) (type_value (rtype v));
			(match rtype v with
			| HUI8 | HUI16 | HI32 | HBool ->
				sexpr "%s->v.i = %s" (reg r) (reg v)
			| HI64 ->
				sexpr "%s->v.i64 = %s" (reg r) (reg v)
			| HF32 ->
				sexpr "%s->v.f = %s" (reg r) (reg v)
			| HF64 ->
				sexpr "%s->v.d = %s" (reg r) (reg v)
			| _ ->
				sexpr "%s->v.ptr = %s" (reg r) (reg v));
			if is_ptr (rtype v) then begin
				unblock();
				line "}";
			end;
		| OToSFloat (r,v) ->
			sexpr "%s = (%s)%s" (reg r) (ctype (rtype r)) (reg v)
		| OToUFloat (r,v) ->
			sexpr "%s = (%s)(unsigned)%s" (reg r) (ctype (rtype r)) (reg v)
		| OToInt (r,v) ->
			sexpr "%s = (int)%s" (reg r) (reg v)
		| ONew r ->
			(match rtype r with
			| HObj o | HStruct o -> sexpr "%s = (%s)hl_alloc_obj(%s)" (reg r) (tname o.pname) (type_value (rtype r))
			| HDynObj -> sexpr "%s = hl_alloc_dynobj()" (reg r)
			| HVirtual _ as t -> sexpr "%s = hl_alloc_virtual(%s)" (reg r) (type_value t)
			| _ -> Globals.die "" __LOC__)
		| OField (r,obj,fid) ->
			get_field r obj fid
		| OSetField (obj,fid,v) ->
			set_field obj fid v
		| OGetThis (r,fid) ->
			get_field r 0 fid
		| OSetThis (fid,r) ->
			set_field 0 fid r
		| OThrow r ->
			sexpr "hl_throw((vdynamic*)%s)" (reg r)
		| ORethrow r ->
			sexpr "hl_rethrow((vdynamic*)%s)" (reg r)
		| OGetUI8 (r,b,idx) ->
			sexpr "%s = *(unsigned char*)(%s + %s)" (reg r) (reg b) (reg idx)
		| OGetUI16 (r,b,idx) ->
			sexpr "%s = *(unsigned short*)(%s + %s)" (reg r) (reg b) (reg idx)
		| OGetMem (r,b,idx) ->
			sexpr "%s = *(%s*)(%s + %s)" (reg r) (ctype (rtype r)) (reg b) (reg idx)
		| OGetArray (r, arr, idx) ->
			sexpr "%s = ((%s*)(%s + 1))[%s]" (reg r) (ctype (rtype r)) (reg arr) (reg idx)
		| OSetUI8 (b,idx,r) ->
			sexpr "*(unsigned char*)(%s + %s) = (unsigned char)%s" (reg b) (reg idx) (reg r)
		| OSetUI16 (b,idx,r) ->
			sexpr "*(unsigned short*)(%s + %s) = (unsigned short)%s" (reg b) (reg idx) (reg r)
		| OSetMem (b,idx,r) ->
			sexpr "*(%s*)(%s + %s) = %s" (ctype (rtype r)) (reg b) (reg idx) (reg r)
		| OSetArray (arr,idx,v) ->
			sexpr "((%s*)(%s + 1))[%s] = %s" (ctype (rtype v)) (reg arr) (reg idx) (reg v)
		| OSafeCast (r,v) ->
			let tsrc = rtype v in
			let t = rtype r in
			if tsrc = HNull t then
				sexpr "%s = %s ? %s%s : 0" (reg r) (reg v) (reg v) (dyn_value_field t)
			else
				sexpr "%s = (%s)hl_dyn_cast%s(&%s,%s%s)" (reg r) (ctype t) (dyn_prefix t) (reg v) (type_value (rtype v)) (type_value_opt t)
		| OUnsafeCast (r,v) ->
			sexpr "%s = (%s)%s" (reg r) (ctype (rtype r)) (reg v)
		| OArraySize (r,a) ->
			sexpr "%s = %s->size" (reg r) (reg a)
		| OType (r,t) ->
			sexpr "%s = %s" (reg r) (type_value t)
		| OGetType (r,v) ->
			sexpr "%s = %s ? ((vdynamic*)%s)->t : &hlt_void" (reg r) (reg v) (reg v)
		| OGetTID (r,v) ->
			sexpr "%s = %s->kind" (reg r) (reg v)
		| ORef (r,v) ->
			sexpr "%s = &%s" (reg r) (reg v)
		| OUnref (r,v) ->
			sexpr "%s = *%s" (reg r) (reg v)
		| OSetref (r,v) ->
			sexpr "*%s = %s" (reg r) (reg v)
		| OToVirtual (r,v) ->
			sexpr "%s = hl_to_virtual(%s,(vdynamic*)%s)" (reg r) (type_value (rtype r)) (reg v)
		| ODynGet (r,o,sid) ->
			let t = rtype r in
			let h = hash ctx sid in
			sexpr "%s = (%s)hl_dyn_get%s((vdynamic*)%s,%ld/*%s*/%s)" (reg r) (ctype t) (dyn_prefix t) (reg o) h code.strings.(sid) (type_value_opt t)
		| ODynSet (o,sid,v) ->
			let h = hash ctx sid in
			sexpr "hl_dyn_set%s((vdynamic*)%s,%ld/*%s*/%s,%s)" (dyn_prefix (rtype v)) (reg o) h code.strings.(sid) (type_value_opt (rtype v)) (reg v)
		| OMakeEnum (r,cid,rl) ->
			let e, et = (match rtype r with HEnum e -> e, enum_constr_type ctx e cid | _ -> Globals.die "" __LOC__) in
			let need_tmp = List.mem r rl in
			let tmp = if not need_tmp then reg r else begin
				sexpr "{ venum *tmp";
				"tmp"
			end in
			sexpr "%s = hl_alloc_enum(%s,%d)" tmp (type_value (rtype r)) cid;
			let _,_,tl = e.efields.(cid) in
			list_iteri (fun i v ->
				sexpr "((%s*)%s)->p%d = %s" et tmp i (rcast v tl.(i))
			) rl;
			if need_tmp then sexpr "%s = tmp; }" (reg r)
		| OEnumAlloc (r,cid) ->
			sexpr "%s = hl_alloc_enum(%s,%d)" (reg r) (type_value (rtype r)) cid
		| OEnumIndex (r,v) ->
			sexpr "%s = HL__ENUM_INDEX__(%s)" (reg r) (reg v)
		| OEnumField (r,e,cid,pid) ->
			let tname,(_,_,tl) = (match rtype e with HEnum e -> enum_constr_type ctx e cid, e.efields.(cid) | _ -> Globals.die "" __LOC__) in
			sexpr "%s((%s*)%s)->p%d" (rassign r tl.(pid)) tname (reg e) pid
		| OSetEnumField (e,pid,r) ->
			let tname, (_,_,tl) = (match rtype e with HEnum e -> enum_constr_type ctx e 0, e.efields.(0) | _ -> Globals.die "" __LOC__) in
			sexpr "((%s*)%s)->p%d = (%s)%s" tname (reg e) pid (ctype tl.(pid)) (reg r)
		| OSwitch (r,idx,eend) ->
			sline "switch(%s) {" (reg r);
			block();
			let pend = i+1+eend in
			(* insert at end if we have another switch case here *)
			let old = output_options.(pend) in
			output_options.(pend) <- [];
			(* insert cases *)
			output_at2 (i + 1) [OODefault;OOIncreaseIndent];
			Array.iteri (fun k delta ->
				output_at2 (delta + i + 1) [OODecreaseIndent;OOCase k;OOIncreaseIndent];
				if delta = eend then output_at pend OOBreak;
			) idx;
			(* insert end switch *)
			output_at2 pend ([OODecreaseIndent;OODecreaseIndent;OOEndBlock] @ List.rev old);
		| ONullCheck r ->
			sexpr "if( %s == NULL ) hl_null_access()" (reg r)
		| OTrap (r,d) ->
			sexpr "hl_trap(trap$%d,%s,%s)" !trap_depth (reg r) (label d);
			incr trap_depth
		| OEndTrap b ->
			sexpr "hl_endtrap(trap$%d)" (!trap_depth - 1);
			if b then decr trap_depth;
		| OAssert _ ->
			sexpr "hl_assert()"
		| ORefData (r,d) ->
			(match rtype d with
			| HArray ->
				sexpr "%s = (%s)hl_aptr(%s,void*)" (reg r) (ctype (rtype r)) (reg d)
			| _ ->
				Globals.die "" __LOC__)
		| ORefOffset (r,r2,off) ->
			sexpr "%s = %s + %s" (reg r) (reg r2) (reg off)
		| ONop _ ->
			()
	) f.code;
	flush_options (Array.length f.code);
	unblock();
	line "}";
	line ""

type type_desc =
	| DSimple of ttype
	| DFun of type_desc list * type_desc * bool
	| DNamed of string
	| DVirtual of (string * type_desc) array
	| DContext of type_desc array

let valid_ident =
	let e = Str.regexp "[^A-Za-z0-9_]+" in
	(fun str -> Str.global_replace e "_" str)

let native_name str =
	if str.[0] = '?' then String.sub str 1 (String.length str - 1) else str

let make_types_idents htypes =
	let types_descs = ref PMap.empty in
	let rec make_desc t =
		match t with
		| HVoid | HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64 | HBool | HBytes | HDyn | HArray | HType | HRef _ | HDynObj | HNull _ ->
			DSimple t
		| HFun (tl,t) ->
			DFun (List.map make_desc tl, make_desc t, true)
		| HMethod (tl, t) ->
			DFun (List.map make_desc tl, make_desc t, false)
		| HObj p | HStruct p ->
			DNamed p.pname
		| HPacked t ->
			(match make_desc t with
			| DNamed n -> DNamed ("packed_" ^ n)
			| _ -> Globals.die "" __LOC__)
		| HAbstract (n,_) ->
			DNamed n
		| HEnum e when e.ename = "" ->
			let _,_,tl = e.efields.(0) in
			DContext (Array.map make_desc tl)
		| HEnum e ->
			DNamed e.ename
		| HVirtual vp ->
			try
				PMap.find vp (!types_descs)
			with Not_found ->
				let arr = Array.create (Array.length vp.vfields) ("",DSimple HVoid) in
				let td = DVirtual arr in
				types_descs := PMap.add vp td (!types_descs);
				Array.iteri (fun i (f,_,t) -> arr.(i) <- (f,make_desc t)) vp.vfields;
				td
	in
	let hashes = Hashtbl.create 0 in
	let make_sign d =
		let dig = Digest.to_hex (Digest.bytes (Marshal.to_bytes d [Marshal.Compat_32])) in
		let h = String.sub dig 0 7 in
		let h = if Hashtbl.mem hashes h then dig else h in
		Hashtbl.add hashes h ();
		h
	in
	let rec desc_string d =
		match d with
		| DSimple (HNull t) ->
			"t$nul_" ^ tstr t
		| DSimple (HRef t) ->
			"t$ref_" ^ (match make_desc t with DSimple _ -> tstr t | d -> desc_string d)
		| DSimple t ->
			"t$_" ^ tstr t
		| DFun _ ->
			"t$fun_" ^ make_sign d
		| DNamed n ->
			"t$" ^ (String.concat "_" (ExtString.String.nsplit n "."))
		| DVirtual _ ->
			"t$vrt_" ^ (make_sign d)
		| DContext _ ->
			"t$ctx_" ^ (make_sign d)
	in
	PMap.mapi (fun t _ -> desc_string (make_desc t)) htypes

let make_global_names code gnames =
	let hstrings = Hashtbl.create 0 in
	let is_cstr = Hashtbl.create 0 in
	Array.iter (fun (g,vl) ->
		match code.globals.(g) with
		| HObj { pname = "String" } ->
			let str = code.strings.(vl.(0)) in
			let v = valid_ident str in
			Hashtbl.replace hstrings v (Hashtbl.mem hstrings v);
			Hashtbl.add is_cstr g ();
			gnames.(g) <- str
		| _ -> ()
	) code.constants;
	let gids = Array.mapi (fun i n -> (n,i)) gnames in
	Array.sort (fun (n1,g1) (n2,g2) -> let d = compare n1 n2 in if d = 0 then compare g1 g2 else d) gids;
	let gnames_used = Hashtbl.create 0 in
	let gnames = Hashtbl.create 0 in
	Array.iter (fun (str,g) ->
		let id = (if Hashtbl.mem is_cstr g then "s$" else "g$") ^ (if String.length str > 32 then short_digest str else let i = valid_ident str in if i = "_" || (try Hashtbl.find hstrings i with Not_found -> false) then short_digest str else i) in
		let rec loop id k =
			let rid = if k = 0 then id else id ^ "_" ^ string_of_int k in
			if Hashtbl.mem gnames_used rid then loop id (k+1) else rid
		in
		let id = loop id 0 in
		Hashtbl.add gnames_used id ();
		Hashtbl.add gnames g id;
	) gids;
	Array.init (Array.length code.globals) (fun i -> Hashtbl.find gnames i)

let make_function_table code =
	let new_entry i = { fe_index = i; fe_args = []; fe_ret = HVoid; fe_name = ""; fe_module = None; fe_calling = []; fe_called_by = []; fe_decl = None; } in
	let ftable = Array.init (Array.length code.functions + Array.length code.natives) new_entry in
	Array.iter (fun (lib,name,t,idx) ->
		let fname =
			let lib = code.strings.(lib) in
			let lib = if lib.[0] = '?' then String.sub lib 1 (String.length lib - 1) else lib in
 			let lib = if lib = "std" then "hl" else lib in
			let str = lib ^ "_" ^ code.strings.(name) in
			(* create wrappers defines for invalid definitions *)
			if str = "hl_tls_get" then str ^ "_w" else str
		in
		match t with
		| HFun (args, t) ->
			let ft = ftable.(idx) in
			ft.fe_name <- fname;
			ft.fe_args <- args;
			ft.fe_ret <- t
		| _ ->
			Globals.die "" __LOC__
	) code.natives;
	Array.iter (fun f ->
		let fname = String.concat "_" (ExtString.String.nsplit (fundecl_name f) ".") in
		let ft = ftable.(f.findex) in
		ft.fe_name <- fname;
		(match f.ftype with
		| HFun (args,t) ->
			ft.fe_args <- args;
			ft.fe_ret <- t;
		| _ ->
			Globals.die "" __LOC__);
		ft.fe_decl <- Some f;
		Array.iter (fun op ->
			match op with
			| OCall0 (_,fid)
			| OCall1 (_,fid,_)
			| OCall2 (_,fid,_,_)
			| OCall3 (_,fid,_,_,_)
			| OCall4 (_,fid,_,_,_,_)
			| OCallN (_,fid,_)
			| OStaticClosure (_,fid)
			| OInstanceClosure (_,fid,_) ->
				let ft2 = ftable.(fid) in
				if not (List.memq ft ft2.fe_called_by) then begin
					ft2.fe_called_by <- ft :: ft2.fe_called_by;
					ft.fe_calling <- ft2 :: ft.fe_calling;
				end;
			| _ ->
				()
		) f.code;
	) code.functions;
	ftable

let make_modules ctx all_types =
	let modules = Hashtbl.create 0 in
	let all_modules = ref [] in
	let get_module name =
		try
			Hashtbl.find modules name
		with Not_found ->
			let m = {
				m_name = name;
				m_functions = [];
				m_types = [];
			} in
			Hashtbl.add modules name m;
			all_modules := m :: !all_modules;
			m
	in
	let add m fid =
		let f = ctx.ftable.(fid) in
		if f.fe_module <> None then Globals.die "" __LOC__;
		f.fe_module <- Some m;
		m.m_functions <- f :: m.m_functions;
	in
	let add_type m t =
		m.m_types <- t :: m.m_types;
		ctx.type_module <- PMap.add t m ctx.type_module;
	in
	let mk_name path =
		let base_name, path = match List.rev (ExtString.String.nsplit path ".") with
			| [] -> "enums", ["hl"]
			| name :: acc -> (if name.[0] = '$' then String.sub name 1 (String.length name - 1) else name), List.rev acc
		in
		let path = path @ [base_name] in
		let path = List.map (fun n -> if String.length n > 128 then Digest.to_hex (Digest.string n) else n) path in
		let path = (match path with [name] -> ["_std";name] | _ -> path) in
		String.concat "/" path
	in
	let all_contexts = ref [] in
	Array.iter (fun t ->
		match t with
		| HObj o | HStruct o ->
			let m = get_module (mk_name o.pname) in
			Array.iter (fun p -> add m p.fmethod) o.pproto;
			List.iter (fun (_,mid) -> add m mid) o.pbindings;
			add_type m t
		| HEnum e when e.ename = "" ->
			all_contexts := t :: !all_contexts
		| HEnum e ->
			let m = get_module (mk_name e.ename) in
			add_type m t
		| _ ->
			()
	) all_types;
	let ep = ctx.hlcode.entrypoint in
	if ep >= 0 then begin
		let m = get_module "hl/init" in
		add m ep;
		ctx.ftable.(ep).fe_name <- "fun$init";
	end;
	List.iter (fun m ->
		let rec get_deps acc = function
			| [] -> acc
			| fm :: fl ->
				let counter = ref 1 in
				let rec loop acc = function
					| [] -> acc
					| f :: l when f.fe_module = None && List.length f.fe_called_by = 1 && f.fe_decl <> None ->
						f.fe_name <- fm.fe_name ^ "__$" ^ (string_of_int !counter);
						incr counter;
						f.fe_module <- Some m;
						loop (append f acc) l
					| _ :: l ->
						loop acc l
				and append f acc =
					loop (f :: acc) (List.rev f.fe_calling)
				in
				get_deps (append fm acc) fl
		in
		m.m_functions <- get_deps [] m.m_functions
	) !all_modules;
	let contexts = ref PMap.empty in
	Array.iter (fun f ->
		if f.fe_module = None && ExtString.String.starts_with f.fe_name "fun$" then f.fe_name <- "wrap" ^ type_name ctx (match f.fe_decl with None -> Globals.die "" __LOC__ | Some f -> f.ftype);
		(* assign context to function module *)
		match f.fe_args with
		| (HEnum e) as t :: _ when e.ename = "" ->
			(try
				let r = PMap.find t !contexts in
				(match r with
				| None -> ()
				| Some m when (match f.fe_module with Some m2 -> m == m2 | _ -> false) -> ()
				| _ -> contexts := PMap.add t None !contexts) (* multiple contexts *)
			with Not_found ->
				contexts := PMap.add t f.fe_module !contexts)
		| _ -> ()
	) ctx.ftable;
	List.iter (fun t ->
		let m = (try PMap.find t !contexts with Not_found -> None) in
		let m = (match m with
			| None ->
				let tname = PMap.find t ctx.htypes in
				get_module ("hl/ctx/" ^ String.sub tname 8 (String.length tname - 8))
			| Some m ->
				m
		) in
		add_type m t
	) (List.rev !all_contexts);
	!all_modules

let generate_module_types ctx m =
	let def_name = "INC_" ^ String.concat "__" (ExtString.String.nsplit m.m_name "/") in
	let line = line ctx and expr = expr ctx and sexpr fmt = Printf.ksprintf (expr ctx) fmt in
	let type_name t =
		match t with
		| HObj o | HStruct o -> o.pname
		| HEnum e -> e.ename
		| _ -> ""
	in
	let types = List.sort (fun t1 t2 -> compare (type_name t1) (type_name t2)) m.m_types in
	define ctx (sprintf "#ifndef %s" def_name);
	define ctx (sprintf "#define %s" def_name);
	List.iter (fun t ->
		match t with
		| HObj o | HStruct o ->
			let name = tname o.pname in
			ctx.defined_types <- PMap.add t () ctx.defined_types;
			define ctx (sprintf "typedef struct _%s *%s;" name name);
		| _ -> ()
	) types;
	line "";
	List.iter (fun t ->
		match t with
		| HObj op | HStruct op ->
			let name = tname op.pname in
			line ("struct _" ^ name ^ " {");
			block ctx;
			let rec loop o =
				(match o.psuper with
				| None ->
					if not (is_struct t) then expr ("hl_type *$type");
				| Some c ->
					define_type ctx (if is_struct t then HStruct c else HObj c);
					loop c);
				Array.iteri (fun i (n,_,t) ->
					let rec abs_index p v =
						match p with
						| None -> v
						| Some o -> abs_index o.psuper (Array.length o.pfields + v)
					in
					define_type ctx t;
					expr (var_type (if n = "" then unamed_field (abs_index o.psuper i) else n) t)
				) o.pfields;
			in
			loop op;
			unblock ctx;
			expr "}";
		| HEnum e ->
			Array.iteri (fun i (_,_,pl) ->
				if Array.length pl <> 0 then begin
					line ("typedef struct {");
					block ctx;
					line "HL__ENUM_CONSTRUCT__";
					Array.iteri (fun i t ->
						define_type ctx t;
						expr (var_type ("p" ^ string_of_int i) t)
					) pl;
					unblock ctx;
					sexpr "} %s" (enum_constr_type ctx e i);
				end;
			) e.efields
		| _ ->
			()
	) types;
	line "#endif";
	line ""

let write_c com file (code:code) gnames =

	let all_types, htypes = gather_types code in
	let types_ids = make_types_idents htypes in
	let gnames = make_global_names code gnames in
	let bnames = Array.map (fun b -> "bytes$" ^ short_digest (Digest.to_hex (Digest.bytes b))) code.bytes in

	let ctx = {
		version = com.Common.version;
		out = Buffer.create 1024;
		tabs = "";
		hlcode = code;
		hash_cache = Hashtbl.create 0;
		hash_mem = Hashtbl.create 0;
		hash_cache_list = [];
		dir = (match Filename.dirname file with "" -> "." | dir -> String.concat "/" (ExtString.String.nsplit dir "\\"));
		curfile = "";
		cfiles = [];
		ftable = make_function_table code;
		htypes = types_ids;
		gnames = gnames;
		bytes_names = bnames;
		defines = [];
		hdefines = Hashtbl.create 0;
		defined_funs = Hashtbl.create 0;
		defined_types = PMap.empty;
		file_prefix = "";
		fun_index = 0;
		type_module = PMap.empty;
		gcon = com;
	} in
	let modules = make_modules ctx all_types in

	let line = line ctx and expr = expr ctx in
	let sline fmt = Printf.ksprintf line fmt and sexpr fmt = Printf.ksprintf expr fmt in

	open_file ctx "hl/natives.h";
	define ctx "#ifndef HL_NATIVES_H";
	define ctx "#define HL_NATIVES_H";
	define ctx "// Abstract decls";
	let rec get_abstracts = function
		| [] -> []
		| HAbstract (("hl_tls" | "hl_mutex" | "hl_thread"), _) :: l -> get_abstracts l (* ignore / defined in hl.h already *)
		| HAbstract (name,_) :: l -> name :: get_abstracts l
		| _ :: l -> get_abstracts l
	in
	let abstracts = List.sort compare (get_abstracts (Array.to_list all_types)) in
	List.iter (fun name -> define ctx (sprintf "typedef struct _%s %s;" name name)) abstracts;
	define ctx "";
	line "// Natives functions";
	let native_libs = Hashtbl.create 0 in
	let sorted_natives = Array.copy code.natives in
	Array.sort (fun n1 n2 -> let mk (lib,name,_,_) = code.strings.(lib), code.strings.(name) in compare (mk n1) (mk n2)) sorted_natives;
	Array.iter (fun (lib,_,_,idx) ->
		let name = code.strings.(lib) in
		let name = if name.[0] = '?' then String.sub name 1 (String.length name - 1) else name in
		Hashtbl.replace native_libs name ();
		let ft = ctx.ftable.(idx) in
		define_type ctx (HFun (ft.fe_args,ft.fe_ret));
		match ft.fe_name with
		| "hl_tls_get_w" ->
			define ctx "#define hl_tls_get_w(tls) ((vdynamic*)hl_tls_get(tls))";
		| "hl_tls_set" ->
			() (* don't redefine *)
		| _ ->
			sexpr "HL_API %s %s(%s)" (ctype ft.fe_ret) ft.fe_name (args_repr ft.fe_args);
	) sorted_natives;
	line "#endif";
	line "";

	open_file ctx "hl/globals.c";
	define ctx "#define HLC_BOOT";
	define ctx "#include <hlc.h>";
	line "// Globals";
	Array.iteri (fun i t ->
		let name = gnames.(i) in
		define_type ctx t;
		sexpr "%s = 0" (var_type name t)
	) code.globals;
	Array.iter (fun (g,fields) ->
		let t = code.globals.(g) in
		let name = "const_" ^ gnames.(g) in
		let field_value t idx =
			match t with
			| HI32 ->
				Int32.to_string code.ints.(idx)
			| HBytes ->
				"(vbyte*)" ^ string ctx idx
			| _ ->
				Globals.die "" __LOC__
		in
		let fields = match t with
			| HObj o | HStruct o ->
				let fields = List.map2 field_value (List.map (fun (_,_,t) -> t) (Array.to_list o.pfields)) (Array.to_list fields) in
				if is_struct t then fields else type_value ctx t :: fields
			| _ ->
				Globals.die "" __LOC__
		in
		sexpr "static struct _%s %s = {%s}" (ctype t) name (String.concat "," fields);
	) code.constants;
	line "";
	line "void hl_init_roots() {";
	block ctx;
	let is_const = Hashtbl.create 0 in
	Array.iter (fun (g,fields) ->
		sexpr "%s = &const_%s" gnames.(g) gnames.(g);
		Hashtbl.add is_const g true;
	) code.constants;
	Array.iteri (fun i t ->
		if is_ptr t && not (Hashtbl.mem is_const i) then sexpr "hl_add_root((void**)&%s)" gnames.(i);
	) code.globals;
	unblock ctx;
	line "}";

	let output_bytes f str =
		for i = 0 to String.length str - 1 do
			if (i+1) mod 0x80 = 0 then f "\\\n\t";
			if i > 0 then f ",";
			f (string_of_int (int_of_char str.[i]));
		done
	in
	Array.iteri (fun i str ->
		if String.length str >= string_data_limit then begin
			let s = Common.utf8_to_utf16 str true in
			sline "// %s..." (String.escaped (String.sub str 0 (string_data_limit-4)));
			output ctx (Printf.sprintf "vbyte string$%s[] = {" (short_digest str));
			output_bytes (output ctx) s;
			sexpr "}";
		end
	) code.strings;
	Array.iteri (fun i bytes ->
		if Bytes.length bytes > 1000 then begin
			let bytes_file = "hl/bytes_" ^ (Digest.to_hex (Digest.bytes bytes)) ^ ".h" in
			let abs_file = ctx.dir ^ "/" ^ bytes_file in
			if not (Sys.file_exists abs_file) then begin
				let ch = open_out_bin abs_file in
				output_bytes (output_string ch) (Bytes.to_string bytes);
				close_out ch;
			end;
			sline "vbyte %s[] = {" ctx.bytes_names.(i);
			output ctx (Printf.sprintf "#%s  include \"%s\"\n" ctx.tabs bytes_file);
			sexpr "}";
		end else begin
			output ctx (Printf.sprintf "vbyte %s[] = {" ctx.bytes_names.(i));
			output_bytes (output ctx) (Bytes.to_string bytes);
			sexpr "}";
		end
	) code.bytes;

	let type_value ctx t = "&" ^ type_name ctx t in (* no auto import *)
	open_file ctx "hl/types.c";
	define ctx "#define HLC_BOOT";
	define ctx "#include <hlc.h>";
	line "// Types values";
	Array.iteri (fun i t ->
		match t with
		| HMethod _ | HFun _ | HVirtual _ ->
			sexpr "hl_type %s = { %s } /* %s */" (type_name ctx t) (type_id t) (tstr t);
		| _ ->
			sexpr "hl_type %s = { %s }" (type_name ctx t) (type_id t);
	) all_types;

	line "";
	line "// Types values data";
	Array.iter (fun t ->
		let field_value (_,name_id,t) =
			sprintf "{(const uchar*)%s, %s, %ld}" (string ctx name_id) (type_value ctx t) (hash ctx name_id)
		in
		match t with
		| HObj o | HStruct o ->
			let name = type_name ctx t in
			let proto_value p =
				sprintf "{(const uchar*)%s, %d, %d, %ld}" (string ctx p.fid) p.fmethod (match p.fvirtual with None -> -1 | Some i -> i) (hash ctx p.fid)
			in
			let fields =
				if Array.length o.pfields = 0 then "NULL" else
				let name = sprintf "fields%s" name in
				sexpr "static hl_obj_field %s[] = {%s}" name (String.concat "," (List.map field_value (Array.to_list o.pfields)));
				name
			in
			let proto =
				if Array.length o.pproto = 0 then "NULL" else
				let name = sprintf "proto%s" name in
				sexpr "static hl_obj_proto %s[] = {%s}" name (String.concat "," (List.map proto_value (Array.to_list o.pproto)));
				name
			in
			let bindings =
				if o.pbindings = [] then "NULL" else
				let name = sprintf "bindings%s" name in
				sexpr "static int %s[] = {%s}" name (String.concat "," (List.map (fun (fid,fidx) -> string_of_int fid ^ "," ^ string_of_int fidx) o.pbindings));
				name
			in
			let ofields = [
				string_of_int (Array.length o.pfields);
				string_of_int (Array.length o.pproto);
				string_of_int (List.length o.pbindings);
				sprintf "(const uchar*)%s" (string ctx o.pid);
				(match o.psuper with None -> "NULL" | Some c -> type_value ctx (match t with HObj _ -> HObj c | _ -> HStruct c));
				fields;
				proto;
				bindings
			] in
			sexpr "static hl_type_obj obj%s = {%s}" name (String.concat "," ofields);
		| HEnum e ->
			let ename = type_name ctx t in
			let constr_value cid (name,nid,tl) =
				let tval = if Array.length tl = 0 then "NULL" else
					let name = sprintf "econstruct%s_%d" ename cid in
					sexpr "static hl_type *%s[] = {%s}" name (String.concat "," (List.map (type_value ctx) (Array.to_list tl)));
					name
				in
				let size = if Array.length tl = 0 then "0" else sprintf "sizeof(%s)" (enum_constr_type ctx e cid) in
				let offsets = if Array.length tl = 0 then "NULL" else
					let name = sprintf "eoffsets%s_%d" ename cid in
					sexpr "static int %s[] = {%s}" name (String.concat "," (List.map (fun _ -> "0") (Array.to_list tl)));
					name
				in
				let has_ptr = List.exists is_gc_ptr (Array.to_list tl) in
				sprintf "{(const uchar*)%s, %d, %s, %s, %s, %s}" (string ctx nid) (Array.length tl) tval size (if has_ptr then "true" else "false") offsets
			in
			let constr_name = if Array.length e.efields = 0 then "NULL" else begin
				let name = sprintf "econstruct%s" ename in
				sexpr "static hl_enum_construct %s[] = {%s}" name (String.concat "," (Array.to_list (Array.mapi constr_value e.efields)));
				name;
			end in
			let efields = [
				if e.eid = 0 then "NULL" else sprintf "(const uchar*)%s" (string ctx e.eid);
				string_of_int (Array.length e.efields);
				constr_name
			] in
			sexpr "static hl_type_enum enum%s = {%s}" ename (String.concat "," efields);
		| HVirtual v ->
			let vname = type_name ctx t in
			let fields_name =
				if Array.length v.vfields = 0 then "NULL" else
				let name = sprintf "vfields%s" vname in
				sexpr "static hl_obj_field %s[] = {%s}" name (String.concat "," (List.map field_value (Array.to_list v.vfields)));
				name
			in
			let vfields = [
				fields_name;
				string_of_int (Array.length v.vfields)
			] in
			sexpr "static hl_type_virtual virt%s = {%s}" vname (String.concat "," vfields);
		| HFun (args,ret) | HMethod(args,ret) ->
			let fname = type_name ctx t in
			let aname = if args = [] then "NULL" else
				let name = sprintf "fargs%s" fname in
				sexpr "static hl_type *%s[] = {%s}" name (String.concat "," (List.map (type_value ctx) args));
				name
			in
			sexpr "static hl_type_fun tfun%s = {%s,%s,%d}" fname aname (type_value ctx ret) (List.length args)
		| _ ->
			()
	) all_types;

	line "";
	line "void hl_init_types( hl_module_context *ctx ) {";
	block ctx;
	Array.iter (fun t ->
		match t with
		| HObj o | HStruct o ->
			let name = type_name ctx t in
			sexpr "obj%s.m = ctx" name;
			(match o.pclassglobal with
			| None -> ()
			| Some g ->
				define_global ctx g;
				sexpr "obj%s.global_value = (void**)&%s" name gnames.(g));
			sexpr "%s.obj = &obj%s" name name
		| HNull r | HRef r | HPacked r ->
			sexpr "%s.tparam = %s" (type_name ctx t) (type_value ctx r)
		| HEnum e ->
			let name = type_name ctx t in
			sexpr "%s.tenum = &enum%s" name name;
			(match e.eglobal with
			| None -> ()
			| Some g ->
				define_global ctx g;
				sexpr "enum%s.global_value = (void**)&%s" name gnames.(g));
			sexpr "hl_init_enum(&%s,ctx)" name;
		| HVirtual _ ->
			let name = type_name ctx t in
			sexpr "%s.virt = &virt%s" name name;
			sexpr "hl_init_virtual(&%s,ctx)" name;
		| HFun _ | HMethod _ ->
			let name = type_name ctx t in
			sexpr "%s.fun = &tfun%s" name name
		| _ ->
			()
	) all_types;
	unblock ctx;
	line "}";

	open_file ctx "hl/reflect.c";
	define ctx "#define HLC_BOOT";
	define ctx "#include <hlc.h>";
	line "// Reflection helpers";
	generate_reflection ctx;

	List.iter (fun m ->
		let defined_types = ref PMap.empty in
		if m.m_types <> [] then begin
			open_file ctx (m.m_name ^ ".h");
			generate_module_types ctx m;
			defined_types := ctx.defined_types;
		end;
		if m.m_functions <> [] then begin
			open_file ctx (m.m_name ^ ".c");
			ctx.defined_types <- !defined_types;
			define ctx "#define HLC_BOOT";
			define ctx "#include <hlc.h>";
			if m.m_types <> [] then define ctx (sprintf "#include <%s.h>" m.m_name);
			let file_pos f =
				match f.fe_decl with
				| Some f when Array.length f.debug > 0 ->
					let fid, p = f.debug.(Array.length f.debug - 1) in
					(code.strings.(fid), p)
				| _ ->
					("",0)
			in
			let funcs = List.sort (fun f1 f2 -> compare (file_pos f1) (file_pos f2)) m.m_functions in
			List.iter (fun fe -> match fe.fe_decl with None -> () | Some f -> generate_function ctx f) funcs;
		end;
	) modules;

	open_file ctx "hl/functions.c";
	define ctx "#define HLC_BOOT";
	define ctx "#include <hlc.h>";
	sexpr "void *hl_functions_ptrs[] = {%s}" (String.concat ",\\\n\t" (List.map (fun f ->
		let name = define_function ctx f.fe_index in
		if name = "hl_tls_get_w" then "hl_tls_get" else name
	) (Array.to_list ctx.ftable)));
	let rec loop i =
		if i = Array.length ctx.ftable then [] else
		let ft = ctx.ftable.(i) in
		let n = type_name ctx (HFun (ft.fe_args,ft.fe_ret)) in
		define ctx (sprintf "extern hl_type %s;" n);
		("&" ^ n) :: loop (i + 1)
	in
	sexpr "hl_type *hl_functions_types[] = {%s}" (String.concat ",\\\n\t" (loop 0));
	line "";
	Array.iter (fun f ->
		if f.fe_module = None then (match f.fe_decl with None -> () | Some f -> generate_function ctx f);
	) ctx.ftable;

	open_file ctx "hl/hashes.c";
	define ctx "#define HLC_BOOT";
	define ctx "#include <hlc.h>";
	line "";
	line "void hl_init_hashes() {";
	block ctx;
	List.iter (fun i -> sexpr "hl_hash((vbyte*)%s)" (string ctx i)) (List.rev ctx.hash_cache_list);
	unblock ctx;
	line "}";

	open_file ctx (Filename.basename file);
	define ctx "#define HLC_BOOT";
	define ctx "#include <hlc.h>";
	line "#include <hlc_main.c>";
	line "";
	line "#ifndef HL_MAKE";
	List.iter (sline "#  include <%s>") ctx.cfiles;
	line "#endif";
	line "";
	expr "void hl_init_hashes()";
	expr "void hl_init_roots()";
	expr "void hl_init_types( hl_module_context *ctx )";
	expr "extern void *hl_functions_ptrs[]";
	expr "extern hl_type *hl_functions_types[]";
	line "";
	line "// Entry point";
	line "void hl_entry_point() {";
	block ctx;
	expr "hl_module_context ctx";
	expr "hl_alloc_init(&ctx.alloc)";
	expr "ctx.functions_ptrs = hl_functions_ptrs";
	expr "ctx.functions_types = hl_functions_types";
	expr "hl_init_types(&ctx)";
	expr "hl_init_hashes()";
	expr "hl_init_roots()";
	if code.entrypoint >= 0 then sexpr "%s()" (define_function ctx code.entrypoint);
	unblock ctx;
	line "}";
	line "";

	open_file ctx "hlc.json";

	line "{";
	block ctx;
	sline "\"version\" : %d," ctx.version;
	sline "\"libs\" : [%s]," (String.concat "," (Hashtbl.fold (fun k _ acc -> sprintf "\"%s\"" k :: acc) native_libs []));
	let defines = Buffer.create 64 in
	PMap.iter (fun key value ->
		Printf.bprintf defines "\n\t\t\"%s\" : \"%s\"," (String.escaped key) (String.escaped value);
	) com.defines.values;
	Buffer.truncate defines (Buffer.length defines - 1);
	sline "\"defines\" : {%s\n\t}," (Buffer.contents defines);
	sline "\"files\" : [%s\n\t]" (String.concat "," (List.map (sprintf "\n\t\t\"%s\"") ctx.cfiles));
	unblock ctx;
	line "}";

	close_file ctx
