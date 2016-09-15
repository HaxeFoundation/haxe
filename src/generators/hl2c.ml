(*
 * Copyright (C)2005-2016 Haxe Foundation
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

let c_kwds = [
"auto";"break";"case";"char";"const";"continue";"default";"do";"double";"else";"enum";"extern";"float";"for";"goto";
"if";"int";"long";"register";"return";"short";"signed";"sizeof";"static";"struct";"switch";"typedef";"union";"unsigned";
"void";"volatile";"while";
(* MS specific *)
"__asm";"dllimport2";"__int8";"naked2";"__based1";"__except";"__int16";"__stdcall";"__cdecl";"__fastcall";"__int32";
"thread2";"__declspec";"__finally";"__int64";"__try";"dllexport2";"__inline";"__leave";
(* reserved by HLC *)
"t"
]

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
	[HVoid;HUI8;HUI16;HI32;HF32;HF64;HBool;HBytes;HDyn;HFun ([],HVoid);HObj null_proto;HArray;HType;HRef HVoid;HVirtual vp;HDynObj;HAbstract ("",0);HEnum ep;HNull HVoid]

let write_c version file (code:code) =
	let tabs = ref "" in
	let file_count = ref 1 in
	let line_count = ref 0 in
	let main_ch = IO.output_channel (open_out_bin file) in
	let ch = ref main_ch in
	let end_ch = ref [(fun() -> IO.close_out main_ch)] in
	let block() = tabs := !tabs ^ "\t" in
	let unblock() = tabs := String.sub (!tabs) 0 (String.length (!tabs) - 1) in
	let line str =
		incr line_count;
		IO.write_line !ch (!tabs ^ str)
	in
	let expr str = line (str ^ ";") in
	let sexpr fmt = Printf.ksprintf expr fmt in
	let sline fmt = Printf.ksprintf line fmt in
	let sprintf = Printf.sprintf in

	let flush_file() =
		if !line_count > 60000 then begin
			incr file_count;
			let nfile = String.sub file 0 (String.length file - 2) ^ string_of_int !file_count ^ ".c" in
			ch := main_ch;
			sline "#include \"%s\"" (match List.rev (ExtString.String.nsplit (String.concat "/" (ExtString.String.nsplit nfile "\\")) "/") with file :: _ -> file | _ -> assert false);
			let nch = IO.output_channel (open_out_bin nfile) in
 			ch := nch;
			sline "#ifdef HLC_H";
			end_ch := (fun() -> IO.write_line nch "#endif"; IO.close_out nch) :: !end_ch;
			line_count := 0;
		end
	in

	let hash_cache = Hashtbl.create 0 in
	let hash sid =
		try
			Hashtbl.find hash_cache sid
		with Not_found ->
			let h = hl_hash code.strings.(sid) in
			Hashtbl.add hash_cache sid h;
			h
	in

	let keywords =
		let h = Hashtbl.create 0 in
		List.iter (fun i -> Hashtbl.add h i ()) c_kwds;
		h
	in

	let ident i = if Hashtbl.mem keywords i then "_" ^ i else i in

	let tname str = String.concat "__" (ExtString.String.nsplit str ".") in

	let is_gc_ptr = function
		| HVoid | HUI8 | HUI16 | HI32 | HF32 | HF64 | HBool | HType | HRef _ -> false
		| HBytes | HDyn | HFun _ | HObj _ | HArray | HVirtual _ | HDynObj | HAbstract _ | HEnum _ | HNull _ -> true
	in

	let is_ptr = function
		| HVoid | HUI8 | HUI16 | HI32 | HF32 | HF64 | HBool -> false
		| _ -> true
	in

	let rec ctype_no_ptr = function
		| HVoid -> "void",0
		| HUI8 -> "unsigned char",0
		| HUI16 -> "unsigned short",0
		| HI32 -> "int",0
		| HF32 -> "float",0
		| HF64 -> "double",0
		| HBool -> "bool",0
		| HBytes -> "vbyte",1
		| HDyn -> "vdynamic",1
		| HFun _ -> "vclosure",1
		| HObj p -> tname p.pname,0
		| HArray -> "varray",1
		| HType -> "hl_type",1
		| HRef t -> let s,i = ctype_no_ptr t in s,i + 1
		| HVirtual _ -> "vvirtual",1
		| HDynObj -> "vdynobj",1
		| HAbstract (name,_) -> name,1
		| HEnum _ -> "venum",1
		| HNull _ -> "vdynamic",1
	in

	let ctype t =
		let t, nptr = ctype_no_ptr t in
		if nptr = 0 then t else t ^ String.make nptr '*'
	in

	let type_id t =
		match t with
		| HVoid -> "HVOID"
		| HUI8 -> "HUI8"
		| HUI16 -> "HUI16"
		| HI32 -> "HI32"
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
	in

	let var_type n t =
		ctype t ^ " " ^ ident n
	in

	let version_major = version / 1000 in
	let version_minor = (version mod 1000) / 100 in
	let version_revision = (version mod 100) in
	let ver_str = Printf.sprintf "%d.%d.%d" version_major version_minor version_revision in
	line ("// Generated by HLC " ^ ver_str ^ " (HL v" ^ string_of_int code.version ^")");
	line "#define HLC_BOOT";
	line "#include <hlc.h>";
	let all_types, htypes = gather_types code in
	let tfuns = Array.create (Array.length code.functions + Array.length code.natives) ([],HVoid) in
	let funnames = Array.create (Array.length code.functions + Array.length code.natives) "" in

	let cast_fun s args t =
		sprintf "((%s (*)(%s))%s)" (ctype t) (String.concat "," (List.map ctype args)) s
	in

	let enum_constr_type e i =
		let cname,_, tl = e.efields.(i) in
		if Array.length tl = 0 then
			"venum"
		else
		let name = if e.eid = 0 then
			let index = (try PMap.find (HEnum e) htypes with Not_found -> assert false) in
			"Enum$" ^ string_of_int index
		else
			String.concat "_" (ExtString.String.nsplit e.ename ".")
		in
		if cname = "" then
			name
		else
			name ^ "_" ^ cname
	in

	let dyn_value_field t =
		"->v." ^ match t with
		| HUI8 -> "ui8"
		| HUI16 -> "ui16"
		| HI32 -> "i"
		| HF32 -> "f"
		| HF64 -> "d"
		| HBool -> "b"
		| _ -> "ptr"
	in

	let used_closures = Hashtbl.create 0 in
	let bytes_strings = Hashtbl.create 0 in
	Array.iter (fun f ->
		Array.iteri (fun i op ->
			match op with
			| OStaticClosure (_,fid) ->
				Hashtbl.replace used_closures fid ()
			| OBytes (_,sid) ->
				Hashtbl.replace bytes_strings sid ()
			| _ ->
				()
		) f.code
	) code.functions;


	line "";
	line "// Types definitions";
	Array.iter (fun t ->
		match t with
		| HObj o ->
			let name = tname o.pname in
			expr ("typedef struct _" ^ name ^ " *" ^ name);
		| HAbstract (name,_) ->
			expr ("typedef struct _" ^ name ^ " "  ^ name);
		| _ ->
			()
	) all_types;

	line "";
	line "// Types implementation";
	Array.iter (fun t ->
		match t with
		| HObj o ->
			let name = tname o.pname in
			line ("struct _" ^ name ^ " {");
			block();
			let rec loop o =
				(match o.psuper with
				| None -> expr ("hl_type *$type");
				| Some c -> loop c);
				Array.iter (fun (n,_,t) ->
					expr (var_type n t)
				) o.pfields;
			in
			loop o;
			unblock();
			expr "}";
		| HEnum e ->
			Array.iteri (fun i (_,_,pl) ->
				if Array.length pl <> 0 then begin
					line ("typedef struct {");
					block();
					expr "int index";
					Array.iteri (fun i t ->
						expr (var_type ("p" ^ string_of_int i) t)
					) pl;
					unblock();
					sexpr "} %s" (enum_constr_type e i);
				end;
			) e.efields
		| _ ->
			()
	) all_types;

	line "";
	line "// Types values declaration";
	Array.iteri (fun i t ->
		sexpr "static hl_type type$%d = { %s } /* %s */" i (type_id t) (tstr t);
		match t with
		| HObj o ->
			sline "#define %s__val &type$%d" (tname o.pname) i
		| _ ->
			()
	) all_types;

	line "";
	line "// Globals";
	Array.iteri (fun i t ->
		let name = "global$" ^ string_of_int i in
		sexpr "static %s = 0" (var_type name t)
	) code.globals;

	line "";
	line "// Natives functions";
	Array.iter (fun (lib,name,t,idx) ->
		match t with
		| HFun (args,t) ->
			let fname =
				let lib = code.strings.(lib) in
				let lib = if lib = "std" then "hl" else lib in
				lib ^ "_" ^ code.strings.(name)
			in
			sexpr "HL_API %s %s(%s)" (ctype t) fname (String.concat "," (List.map ctype args));
			funnames.(idx) <- fname;
			Array.set tfuns idx (args,t)
		| _ ->
			assert false
	) code.natives;

	line "";
	line "// Functions declaration";
	Array.iter (fun f ->
		match f.ftype with
		| HFun (args,t) ->
			sexpr "static %s %s(%s)" (ctype t) (fundecl_name f) (String.concat "," (List.map ctype args));
			Array.set tfuns f.findex (args,t);
			funnames.(f.findex) <- fundecl_name f;
		| _ ->
			assert false
	) code.functions;

	line "";
	line "// Strings";
	Array.iteri (fun i str ->
		let rec loop s i =
			if i = String.length s then [] else
			let c = String.get s i in
			string_of_int (int_of_char c) :: loop s (i+1)
		in
		if Hashtbl.mem bytes_strings i then
			sexpr "static vbyte bytes$%d[] = {%s}" i (String.concat "," (loop str 0))
		else
			let s = utf8_to_utf16 str in
			sexpr "static vbyte string$%d[] = {%s} /* %s */" i (String.concat "," (loop s 0)) (String.escaped (String.concat "* /" (ExtString.String.nsplit str "*/")))
	) code.strings;

	let type_value t =
		let index = (try PMap.find t htypes with Not_found -> assert false) in
		"&type$" ^ string_of_int index
	in

	line "";
	line "// Types values data";
	Array.iteri (fun i t ->
		let field_value (name,name_id,t) =
			sprintf "{(const uchar*)string$%d, %s, %ld}" name_id (type_value t) (hash name_id)
		in
		match t with
		| HObj o ->
			let proto_value p =
				sprintf "{(const uchar*)string$%d, %d, %d, %ld}" p.fid p.fmethod (match p.fvirtual with None -> -1 | Some i -> i) (hash p.fid)
			in
			let fields =
				if Array.length o.pfields = 0 then "NULL" else
				let name = sprintf "fields$%d" i in
				sexpr "static hl_obj_field %s[] = {%s}" name (String.concat "," (List.map field_value (Array.to_list o.pfields)));
				name
			in
			let proto =
				if Array.length o.pproto = 0 then "NULL" else
				let name = sprintf "proto$%d" i in
				sexpr "static hl_obj_proto %s[] = {%s}" name (String.concat "," (List.map proto_value (Array.to_list o.pproto)));
				name
			in
			let ofields = [
				string_of_int (Array.length o.pfields);
				string_of_int (Array.length o.pproto);
				sprintf "(const uchar*)string$%d" o.pid;
				(match o.psuper with None -> "NULL" | Some c -> sprintf "%s__val" (tname c.pname));
				fields;
				proto
			] in
			sexpr "static hl_type_obj obj$%d = {%s}" i (String.concat "," ofields);
		| HEnum e ->
			let constr_name = sprintf "econstructs$%d" i in
			let constr_value cid (_,nid,tl) =
				let tval = if Array.length tl = 0 then "NULL" else
					let name = sprintf "econstruct$%d_%d" i cid in
					sexpr "static hl_type *%s[] = {%s}" name (String.concat "," (List.map type_value (Array.to_list tl)));
					name
				in
				let size = if Array.length tl = 0 then "0" else sprintf "sizeof(%s)" (enum_constr_type e cid) in
				let offsets = if Array.length tl = 0 then "NULL" else
					let name = sprintf "eoffsets$%d_%d" i cid in
					sexpr "static int %s[] = {%s}" name (String.concat "," (List.map (fun _ -> "0") (Array.to_list tl)));
					name
				in
				let has_ptr = List.exists is_gc_ptr (Array.to_list tl) in
				sprintf "{(const uchar*)string$%d, %d, %s, %s, %s, %s}" nid (Array.length tl) tval size (if has_ptr then "true" else "false") offsets
			in
			sexpr "static hl_enum_construct %s[] = {%s}" constr_name (String.concat "," (Array.to_list (Array.mapi constr_value e.efields)));
			let efields = [
				if e.eid = 0 then "NULL" else sprintf "(const uchar*)string$%d" e.eid;
				string_of_int (Array.length e.efields);
				constr_name
			] in
			sexpr "static hl_type_enum enum$%d = {%s}" i (String.concat "," efields);
		| HVirtual v ->
			let fields_name =
				if Array.length v.vfields = 0 then "NULL" else
				let name = sprintf "vfields$%d" i in
				sexpr "static hl_obj_field %s[] = {%s}" name (String.concat "," (List.map field_value (Array.to_list v.vfields)));
				name
			in
			let vfields = [
				fields_name;
				string_of_int (Array.length v.vfields)
			] in
			sexpr "static hl_type_virtual virt$%d = {%s}" i (String.concat "," vfields);
		| HFun (args,t) ->
			let aname = if args = [] then "NULL" else
				let name = sprintf "fargs$%d" i in
				sexpr "static hl_type *%s[] = {%s}" name (String.concat "," (List.map type_value args));
				name
			in
			sexpr "static hl_type_fun tfun$%d = {%s,%s,%d}" i aname (type_value t) (List.length args)
		| _ ->
			()
	) all_types;

	line "";
	line "// Static data";
	Hashtbl.iter (fun fid _ ->
		let args, t = tfuns.(fid) in
		sexpr "static vclosure cl$%d = { %s, %s, 0 }" fid (type_value (HFun (args,t))) funnames.(fid);
	) used_closures;

	line "";
	line "// Reflection helpers";
	let funByArgs = Hashtbl.create 0 in
	let type_kind t =
		match t with
		| HVoid | HF32 | HF64 -> t
		| HBool | HUI8 | HUI16 | HI32 -> HI32
		| HBytes | HDyn | HFun _ | HObj _ | HArray | HType | HRef _ | HVirtual _ | HDynObj | HAbstract _ | HEnum _ | HNull _ -> HDyn
	in
	let type_kind_id t =
		match t with
		| HVoid -> 0
		| HBool | HUI8 | HUI16 | HI32 -> 1 (* same int representation *)
		| HF32 -> 2
		| HF64 -> 3
		| _ -> 4
	in
	Array.iter (fun (args,t) ->
		let nargs = List.length args in
		let kargs = List.map type_kind args in
		let kt = type_kind t in
		let h = try Hashtbl.find funByArgs nargs with Not_found -> let h = Hashtbl.create 0 in Hashtbl.add funByArgs nargs h; h in
		Hashtbl.replace h (kargs,kt) ()
	) tfuns;
	let argsCounts = List.sort compare (Hashtbl.fold (fun i _ acc -> i :: acc) funByArgs []) in
	sexpr "static int TKIND[] = {%s}" (String.concat "," (List.map (fun t -> string_of_int (type_kind_id (type_kind t))) core_types));
	line "";
	line "void *hlc_static_call( void *fun, hl_type *t, void **args, vdynamic *out ) {";
	block();
	sexpr "int chk = TKIND[t->fun->ret->kind]";
	sexpr "vdynamic *d";
	line "switch( t->fun->nargs ) {";
	List.iter (fun nargs ->
		sline "case %d:" nargs;
		block();
		if nargs > 9 then sexpr "hl_fatal(\"Too many arguments, TODO:use more bits\")" else begin
		for i = 0 to nargs-1 do
			sexpr "chk |= TKIND[t->fun->args[%d]->kind] << %d" i ((i + 1) * 3);
		done;
		line "switch( chk ) {";
		Hashtbl.iter (fun (args,t) _ ->
			let s = ref (-1) in
			let chk = List.fold_left (fun chk t -> incr s; chk lor ((type_kind_id t) lsl (!s * 3))) 0 (t :: args) in
			sline "case %d:" chk;
			block();
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
			unblock();
		) (Hashtbl.find funByArgs nargs);
		sline "}";
		expr "break";
		end;
		unblock();
	) argsCounts;
	line "}";
	sexpr "hl_fatal(\"Unsupported dynamic call\")";
	sexpr "return NULL";
	unblock();
	line "}";
	line "";
	let wrap_char = function
		| HVoid -> "v"
		| HUI8 | HUI16 | HBool | HI32 -> "i"
		| HF32 -> "f"
		| HF64 -> "d"
		| _ -> "p"
	in
	let make_wrap_name args t =
		String.concat "" (List.map wrap_char args) ^ "_" ^ wrap_char t
	in
	List.iter (fun nargs ->
		Hashtbl.iter (fun (args,t) _ ->
			let name = make_wrap_name args t in
			sline "static %s wrap_%s(void *value%s) {" (ctype t) name (String.concat "" (list_mapi (fun i t -> "," ^ var_type ("p" ^ string_of_int i) t) args));
			block();
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
			unblock();
			line "}";
		) (Hashtbl.find funByArgs nargs);
	) argsCounts;
	line "";
	line "void *hlc_get_wrapper( hl_type *t ) {";
	block();
	sexpr "int chk = TKIND[t->fun->ret->kind]";
	line "switch( t->fun->nargs ) {";
	List.iter (fun nargs ->
		sline "case %d:" nargs;
		block();
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
		unblock();
	) argsCounts;
	line "}";
	sexpr "return NULL";
	unblock();
	line "}";
	line "";
	line "// Functions code";
	Array.iter (fun f ->

		flush_file();

		let rid = ref (-1) in
		let reg id = "r" ^ string_of_int id in

		let label id = "label$" ^ string_of_int f.findex ^ "$" ^ string_of_int id in

		let rtype r = f.regs.(r) in

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
			if not (safe_cast t rt) then assert false
			else assign ^ "(" ^ ctype rt ^ ")"
		in

		let ocall r fid args =
			let targs, rt = tfuns.(fid) in
			let rstr = rassign r rt in
			sexpr "%s%s(%s)" rstr funnames.(fid) (String.concat "," (List.map2 rcast args targs))
		in


		let dyn_prefix = function
			| HUI8 | HUI16 | HI32 | HBool -> "i"
			| HF32 -> "f"
			| HF64 -> "d"
			| _ -> "p"
		in

		let type_value_opt t =
			match t with HF32 | HF64 -> "" | _ -> "," ^ type_value t
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
			| [] -> assert false
			| o :: args ->
				match rtype o with
				| HObj _ ->
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
					sexpr "%shl_dyn_call_obj(%s->value,%s,%ld/*%s*/,%s,%s)" ret (reg o) (type_value ft) (hash fid) fname (if args = [] then "NULL" else "args") (if is_ptr rt || rt == HVoid then "NULL" else "&ret");
					if rt <> HVoid && not (is_ptr rt) then sexpr "%s = (%s)ret.v.%s" (reg r) (ctype rt) (dyn_prefix rt);
					unblock();
					sline "}"
				| _ ->
					assert false
		in

		let set_field obj fid v =
			match rtype obj with
			| HObj o ->
				let name, t = resolve_field o fid in
				sexpr "%s->%s = %s" (reg obj) (ident name) (rcast v t)
			| HVirtual vp ->
				let name, nid, t = vp.vfields.(fid) in
				let dset = sprintf "hl_dyn_set%s((vdynamic*)%s,%ld/*%s*/%s,%s)" (dyn_prefix t) (reg obj) (hash nid) name (type_value_opt (rtype v)) (reg v) in
				(match t with
				| HFun _ -> expr dset
				| _ -> sexpr "if( hl_vfields(%s)[%d] ) *(%s*)(hl_vfields(%s)[%d]) = (%s)%s; else %s" (reg obj) fid (ctype t) (reg obj) fid (ctype t) (reg v) dset)
			| _ ->
				assert false
		in

		let get_field r obj fid =
			match rtype obj with
			| HObj o ->
				let name, t = resolve_field o fid in
				sexpr "%s%s->%s" (rassign r t) (reg obj) (ident name)
			| HVirtual v ->
				let name, nid, t = v.vfields.(fid) in
				let dget = sprintf "(%s)hl_dyn_get%s((vdynamic*)%s,%ld/*%s*/%s)" (ctype t) (dyn_prefix t) (reg obj) (hash nid) name (type_value_opt t) in
				(match t with
				| HFun _ -> sexpr "%s%s" (rassign r t) dget
				| _ -> sexpr "%shl_vfields(%s)[%d] ? (*(%s*)(hl_vfields(%s)[%d])) : %s" (rassign r t) (reg obj) fid (ctype t) (reg obj) fid dget)
			| _ ->
				assert false
		in

		let fret = (match f.ftype with
		| HFun (args,t) ->
			sline "static %s %s(%s) {" (ctype t) (fundecl_name f) (String.concat "," (List.map (fun t -> incr rid; var_type (reg !rid) t) args));
			t
		| _ ->
			assert false
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
		let output_options = Array.make (Array.length f.code) [] in
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
			| _ ->
				()
		) f.code;
		for i = 0 to !max_trap_depth - 1 do
			sexpr "hl_trap_ctx trap$%d" i;
		done;

		Array.iteri (fun i op ->
			(match output_options.(i) with
			| [] -> ()
			| opts ->
				(* put label after } *)
				let opts = if has_label i && List.mem OOEndBlock opts then OOLabel :: List.filter (fun i -> i <> OOLabel) opts else opts in
				let opts = List.rev opts in
				List.iter (function
					| OOLabel -> sline "%s:" (label i)
					| OOCase i -> sline "case %i:" i
					| OODefault -> line "default:"
					| OOIncreaseIndent -> block()
					| OODecreaseIndent -> unblock()
					| OOBeginBlock ->  line "{"
					| OOEndBlock -> line "}"
				) opts);
			let label delta =
				let addr = delta + i + 1 in
				let label = label addr in
				if not (has_label addr) then output_at addr OOLabel;
				label
			in
			let todo() =
				sexpr "hl_fatal(\"%s\")" (ostr (fun id -> "f" ^ string_of_int id) op)
			in
			let compare_op op a b d =
				let phys_compare() =
					sexpr "if( %s %s %s ) goto %s" (reg a) (s_comp op) (rcast b (rtype a)) (label d)
				in
				(*
					safe_cast is already checked
					two ways (same type) for eq
					one way for comparisons
				*)
				match rtype a, rtype b with
				| (HUI8 | HUI16 | HI32 | HF32 | HF64 | HBool), (HUI8 | HUI16 | HI32 | HF32 | HF64 | HBool) ->
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
				| HDyn , _ | _, HDyn ->
					let inv = if op = CGt || op = CGte then "&& i != hl_invalid_comparison " else "" in
					sexpr "{ int i = hl_dyn_compare((vdynamic*)%s,(vdynamic*)%s); if( i %s 0 %s) goto %s; }" (reg a) (reg b) (s_comp op) inv (label d)
				| HObj oa, HObj _ ->
					(try
						let fid = PMap.find "__compare" oa.pfunctions in
						if op = CEq then
							sexpr "if( %s == %s || (%s && %s && %s(%s,%s) == 0) ) goto %s" (reg a) (reg b) (reg a) (reg b) funnames.(fid) (reg a) (reg b) (label d)
						else if op = CNeq then
							sexpr "if( %s != %s && (!%s || !%s || %s(%s,%s) != 0) ) goto %s" (reg a) (reg b) (reg a) (reg b) funnames.(fid) (reg a) (reg b) (label d)
						else
							sexpr "if( %s && %s && %s(%s,%s) %s 0 ) goto %s" (reg a) (reg b) funnames.(fid) (reg a) (reg b) (s_comp op) (label d)
					with Not_found ->
						phys_compare())
				| HEnum _, HEnum _ | HVirtual _, HVirtual _ | HDynObj, HDynObj ->
					phys_compare()
				| HVirtual _, HObj _->
					if op = CEq then
						sexpr "if( (void*)%s == (void*)%s || (%s && %s && %s->value == (vdynamic*)%s) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg a) (reg b) (label d)
					else if op = CNeq then
						sexpr "if( (void*)%s != (void*)%s && (!%s || !%s || %s->value != (vdynamic*)%s) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg a) (reg b) (label d)
					else
						assert false
				| HObj _, HVirtual _ ->
					if op = CEq then
						sexpr "if( (void*)%s == (void*)%s || (%s && %s && %s->value == (vdynamic*)%s) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg b) (reg a) (label d)
					else if op = CNeq then
						sexpr "if( (void*)%s != (void*)%s && (!%s || !%s || %s->value != (vdynamic*)%s) ) goto %s" (reg a) (reg b) (reg a) (reg b) (reg b) (reg a) (label d)
					else
						assert false
				| HFun _, HFun _ ->
					phys_compare()
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
				sexpr "%s = %.19g" (reg r) code.floats.(idx)
			| OBool (r,b) ->
				sexpr "%s = %s" (reg r) (if b then "true" else "false")
			| OBytes (r,idx) ->
				sexpr "%s = bytes$%d" (reg r) idx
			| OString (r,idx) ->
				sexpr "%s = string$%d" (reg r) idx
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
					assert false)
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
					assert false)
			| OStaticClosure (r,fid) ->
				sexpr "%s = &cl$%d" (reg r) fid
			| OInstanceClosure (r,fid,ptr) ->
				let args, t = tfuns.(fid) in
				sexpr "%s = hl_alloc_closure_ptr(%s,%s,%s)" (reg r) (type_value (HFun (args,t))) funnames.(fid) (reg ptr)
			| OVirtualClosure (r,o,m) ->
				(match rtype o with
				| HObj p ->
					let tl,t = tfuns.(p.pvirtuals.(m)) in
					let s = sprintf "%s->$type->vobj_proto[%d]" (reg o) m in
					sexpr "%s = hl_alloc_closure_ptr(%s,%s,%s)" (reg r) (type_value (HFun(tl,t))) s (reg o)
				| _ ->
					todo())
			| OGetGlobal (r,g) ->
				sexpr "%s = (%s)global$%d" (reg r) (ctype (rtype r)) g
			| OSetGlobal (g,r) ->
				sexpr "global$%d = (%s)%s" g (ctype code.globals.(g)) (reg r)
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
			| OJEq (a,b,d) ->
				compare_op CEq a b d
			| OJNotEq (a,b,d) ->
				compare_op CNeq a b d
			| OJAlways d ->
				sexpr "goto %s" (label d)
			| OLabel _ ->
				if not (has_label i) then sline "%s:" (label (-1))
			| OToDyn (r,v) ->
				if is_ptr (rtype v) then begin
					sline "if( %s == NULL ) %s = NULL; else {" (reg v) (reg r);
					block();
				end;
				sexpr "%s = hl_alloc_dynamic(%s)" (reg r) (type_value (rtype v));
				(match rtype v with
				| HUI8 | HUI16 | HI32 | HBool ->
					sexpr "%s->v.i = %s" (reg r) (reg v)
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
				| HObj o -> sexpr "%s = (%s)hl_alloc_obj(%s)" (reg r) (tname o.pname) (tname o.pname ^ "__val")
				| HDynObj -> sexpr "%s = hl_alloc_dynobj()" (reg r)
				| HVirtual _ as t -> sexpr "%s = hl_alloc_virtual(%s)" (reg r) (type_value t)
				| _ -> assert false)
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
			| OGetI8 (r,b,idx) ->
				sexpr "%s = *(unsigned char*)(%s + %s)" (reg r) (reg b) (reg idx)
			| OGetI16 (r,b,idx) ->
				sexpr "%s = *(unsigned short*)(%s + %s)" (reg r) (reg b) (reg idx)
			| OGetI32 (r,b,idx) ->
				sexpr "%s = *(int*)(%s + %s)" (reg r) (reg b) (reg idx)
			| OGetF32 (r,b,idx) ->
				sexpr "%s = *(float*)(%s + %s)" (reg r) (reg b) (reg idx)
			| OGetF64 (r,b,idx) ->
				sexpr "%s = *(double*)(%s + %s)" (reg r) (reg b) (reg idx)
			| OGetArray (r, arr, idx) ->
				sexpr "%s = ((%s*)(%s + 1))[%s]" (reg r) (ctype (rtype r)) (reg arr) (reg idx)
			| OSetI8 (b,idx,r) ->
				sexpr "*(unsigned char*)(%s + %s) = (unsigned char)%s" (reg b) (reg idx) (reg r)
			| OSetI16 (b,idx,r) ->
				sexpr "*(unsigned short*)(%s + %s) = (unsigned short)%s" (reg b) (reg idx) (reg r)
			| OSetI32 (b,idx,r) ->
				sexpr "*(int*)(%s + %s) = %s" (reg b) (reg idx) (reg r)
			| OSetF32 (b,idx,r) ->
				sexpr "*(float*)(%s + %s) = (float)%s" (reg b) (reg idx) (reg r)
			| OSetF64 (b,idx,r) ->
				sexpr "*(double*)(%s + %s) = %s" (reg b) (reg idx) (reg r)
			| OSetArray (arr,idx,v) ->
				sexpr "((%s*)(%s + 1))[%s] = %s" (ctype (rtype v)) (reg arr) (reg idx) (reg v)
			| OSafeCast (r,v) ->
				let t = rtype r in
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
				let h = hash sid in
				sexpr "%s = (%s)hl_dyn_get%s((vdynamic*)%s,%ld/*%s*/%s)" (reg r) (ctype t) (dyn_prefix t) (reg o) h code.strings.(sid) (type_value_opt t)
			| ODynSet (o,sid,v) ->
				let h = hash sid in
				sexpr "hl_dyn_set%s((vdynamic*)%s,%ld/*%s*/%s,%s)" (dyn_prefix (rtype v)) (reg o) h code.strings.(sid) (type_value_opt (rtype v)) (reg v)
			| OMakeEnum (r,cid,rl) ->
				let e, et = (match rtype r with HEnum e -> e, enum_constr_type e cid | _ -> assert false) in
				let has_ptr = List.exists (fun r -> is_gc_ptr (rtype r)) rl in
				sexpr "%s = (venum*)hl_gc_alloc%s(sizeof(%s))" (reg r) (if has_ptr then "" else "_noptr") et;
				sexpr "%s->index = %d" (reg r) cid;
				let _,_,tl = e.efields.(cid) in
				list_iteri (fun i v ->
					sexpr "((%s*)%s)->p%d = %s" et (reg r) i (rcast v tl.(i))
				) rl;
			| OEnumAlloc (r,cid) ->
				let et, (_,_,tl) = (match rtype r with HEnum e -> enum_constr_type e cid, e.efields.(cid) | _ -> assert false) in
				let has_ptr = List.exists is_gc_ptr (Array.to_list tl) in
				sexpr "%s = (venum*)hl_gc_alloc%s(sizeof(%s))" (reg r) (if has_ptr then "" else "_noptr") et;
				sexpr "memset(%s,0,sizeof(%s))" (reg r) et;
				if cid <> 0 then sexpr "%s->index = %d" (reg r) cid
			| OEnumIndex (r,v) ->
				(match rtype v with
				| HEnum _ ->
					sexpr "%s = %s->index" (reg r) (reg v)
				| HDyn ->
					sexpr "%s = ((venum*)%s->v.ptr)->index" (reg r) (reg v)
				| _ ->
					assert false)
			| OEnumField (r,e,cid,pid) ->
				let tname,(_,_,tl) = (match rtype e with HEnum e -> enum_constr_type e cid, e.efields.(cid) | _ -> assert false) in
				sexpr "%s((%s*)%s)->p%d" (rassign r tl.(pid)) tname (reg e) pid
			| OSetEnumField (e,pid,r) ->
				let tname, (_,_,tl) = (match rtype e with HEnum e -> enum_constr_type e 0, e.efields.(0) | _ -> assert false) in
				sexpr "((%s*)%s)->p%d = (%s)%s" tname (reg e) pid (ctype tl.(pid)) (reg r)
			| OSwitch (r,idx,eend) ->
				sline "switch(%s) {" (reg r);
				block();
				output_at2 (i + 1) [OODefault;OOIncreaseIndent];
				Array.iteri (fun k delta -> output_at2 (delta + i + 1) [OODecreaseIndent;OOCase k;OOIncreaseIndent]) idx;
				output_at2 (i + 1 + eend) [OODecreaseIndent;OODecreaseIndent;OOEndBlock];
			| ONullCheck r ->
				sexpr "if( %s == NULL ) hl_null_access()" (reg r)
			| OTrap (r,d) ->
				sexpr "hl_trap(trap$%d,%s,%s)" !trap_depth (reg r) (label d);
				incr trap_depth
			| OEndTrap b ->
				sexpr "hl_endtrap(trap$%d)" (!trap_depth - 1);
				if b then decr trap_depth;
			| ODump r ->
				todo()
		) f.code;
		unblock();
		line "}";
		line "";
	) code.functions;

	line "";
	line "// Entry point";
	line "void hl_entry_point() {";
	block();
	sexpr "static void *functions_ptrs[] = {%s}" (String.concat "," (Array.to_list funnames));
	let rec loop i =
		if i = Array.length code.functions + Array.length code.natives then [] else
		let args, t = tfuns.(i) in
		(type_value (HFun (args,t))) :: loop (i + 1)
	in
	sexpr "static hl_type *functions_types[] = {%s}" (String.concat "," (loop 0));
	expr "hl_module_context ctx";
	expr "hl_alloc_init(&ctx.alloc)";
	expr "ctx.functions_ptrs = functions_ptrs";
	expr "ctx.functions_types = functions_types";
	Hashtbl.iter (fun i _ -> sexpr "hl_hash(string$%d)" i) hash_cache;
	Array.iteri (fun i t ->
		match t with
		| HObj o ->
			sexpr "obj$%d.m = &ctx" i;
			(match o.pclassglobal with None -> () | Some g -> sexpr "obj$%d.global_value = (void**)&global$%d" i g);
			sexpr "type$%d.obj = &obj$%d" i i
		| HNull t | HRef t ->
			sexpr "type$%d.tparam = %s" i (type_value t)
		| HEnum e ->
			sexpr "type$%d.tenum = &enum$%d" i i;
			(match e.eglobal with None -> () | Some g -> sexpr "enum$%d.global_value = (void**)&global$%d" i g);
			Array.iteri (fun cid (_,_,tl) ->
				if Array.length tl > 0 then begin
					line "{";
					block();
					sexpr "%s *_e = NULL" (enum_constr_type e cid);
					Array.iteri (fun pid _ -> sexpr "eoffsets$%d_%d[%d] = (int)(int_val)&_e->p%d" i cid pid pid) tl;
					unblock();
					line "}";
				end
			) e.efields
		| HVirtual _ ->
			sexpr "type$%d.virt = &virt$%d" i i;
			sexpr "hl_init_virtual(&type$%d,&ctx)" i;
		| HFun _ ->
			sexpr "type$%d.fun = &tfun$%d" i i
		| _ ->
			()
	) all_types;
	Array.iteri (fun i t ->
		if is_ptr t then sexpr "hl_add_root(&global$%d)" i;
	) code.globals;
	sexpr "%s()" funnames.(code.entrypoint);
	unblock();
	line "}";
	line "";
	List.iter (fun f -> f()) !end_ch
