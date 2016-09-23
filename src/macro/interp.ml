(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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
open Common
open Nast
open Unix
open Type

(* ---------------------------------------------------------------------- *)
(* TYPES *)

type value =
	| VNull
	| VBool of bool
	| VInt of int
	| VFloat of float
	| VString of string
	| VObject of vobject
	| VArray of value array
	| VAbstract of vabstract
	| VFunction of vfunction
	| VClosure of value list * (value list -> value list -> value)
	| VInt32 of int32

and vobject = {
	mutable ofields : (int * value) array;
	mutable oproto : vobject option;
}

and vabstract =
	| ADeallocated of int ref
	| AKind of vabstract
	| AHash of (value, value) Hashtbl.t
	| ARandom of Random.State.t ref
	| ABuffer of Buffer.t
	| APos of Globals.pos
	| AFRead of (in_channel * bool ref)
	| AFWrite of out_channel
	| AReg of regexp
	| AZipI of zlib
	| AZipD of zlib
	| AUtf8 of UTF8.Buf.buf
	| ASocket of Unix.file_descr
	| ATDecl of module_type
	| AUnsafe of Obj.t
	| ALazyType of (unit -> Type.t) ref
	| ANekoAbstract of Extc.value
	| ANekoBuffer of value
	| ACacheRef of value
	| AInt32Kind
	| ATls of value ref
	| AProcess of Process.process

and vfunction =
	| Fun0 of (unit -> value)
	| Fun1 of (value -> value)
	| Fun2 of (value -> value -> value)
	| Fun3 of (value -> value -> value -> value)
	| Fun4 of (value -> value -> value -> value -> value)
	| Fun5 of (value -> value -> value -> value -> value -> value)
	| FunVar of (value list -> value)

and regexp = {
	r : Str.regexp;
	mutable r_string : string;
	mutable r_groups : (int * int) option array;
}

and zlib = {
	z : Extc.zstream;
	mutable z_flush : Extc.zflush;
}

type cmp =
	| CEq
	| CSup
	| CInf
	| CUndef

type extern_api = {
	pos : Globals.pos;
	get_com : unit -> Common.context;
	get_type : string -> Type.t option;
	get_module : string -> Type.t list;
	after_typing : (module_type list -> unit) -> unit;
	on_generate : (Type.t list -> unit) -> unit;
	after_generate : (unit -> unit) -> unit;
	on_type_not_found : (string -> value) -> unit;
	parse_string : string -> Globals.pos -> bool -> Ast.expr;
	type_expr : Ast.expr -> Type.texpr;
	resolve_type  : Ast.complex_type -> Globals.pos -> t;
	type_macro_expr : Ast.expr -> Type.texpr;
	store_typed_expr : Type.texpr -> Ast.expr;
	allow_package : string -> unit;
	type_patch : string -> string -> bool -> string option -> unit;
	meta_patch : string -> string -> string option -> bool -> unit;
	set_js_generator : (value -> unit) -> unit;
	get_local_type : unit -> t option;
	get_expected_type : unit -> t option;
	get_call_arguments : unit -> Ast.expr list option;
	get_local_method : unit -> string;
	get_local_imports : unit -> Ast.import list;
	get_local_using : unit -> tclass list;
	get_local_vars : unit -> (string, Type.tvar) PMap.t;
	get_build_fields : unit -> value;
	get_pattern_locals : Ast.expr -> Type.t -> (string,Type.tvar * Globals.pos) PMap.t;
	define_type : value -> unit;
	define_module : string -> value list -> ((string * Globals.pos) list * Ast.import_mode) list -> Ast.type_path list -> unit;
	module_dependency : string -> string -> bool -> unit;
	current_module : unit -> module_def;
	mutable current_macro_module : unit -> module_def;
	delayed_macro : int -> (unit -> (unit -> value));
	use_cache : unit -> bool;
	format_string : string -> Globals.pos -> Ast.expr;
	cast_or_unify : Type.t -> texpr -> Globals.pos -> Type.texpr;
	add_global_metadata : string -> string -> (bool * bool * bool) -> unit;
	add_module_check_policy : string list -> int list -> bool -> int -> unit;
}

type callstack = {
	cpos : pos;
	cthis : value;
	cstack : int;
	cenv : value array;
}

type context = {
	mutable gen : Genneko.context;
	types : (Type.path,int) Hashtbl.t;
	prototypes : (string list, vobject) Hashtbl.t;
	fields_cache : (int,string) Hashtbl.t;
	mutable error : bool;
	mutable error_proto : vobject;
	mutable enums : (value * string) array array;
	mutable do_call : value -> value -> value list -> pos -> value;
	mutable do_string : value -> string;
	mutable do_loadprim : value -> value -> value;
	mutable do_compare : value -> value -> cmp;
	mutable loader : value;
	mutable exports : value;
	(* runtime *)
	mutable stack : value DynArray.t;
	mutable callstack : callstack list;
	mutable callsize : int;
	mutable exc : pos list;
	mutable vthis : value;
	mutable venv : value array;
	(* context *)
	mutable curapi : extern_api;
	mutable on_reused : (unit -> bool) list;
	mutable is_reused : bool;
	(* eval *)
	mutable locals_map : (string, int) PMap.t;
	mutable locals_count : int;
	mutable locals_barrier : int;
	mutable locals_env : string DynArray.t;
	mutable globals : (string, value ref) PMap.t;
}

type access =
	| AccThis
	| AccLocal of int
	| AccGlobal of value ref
	| AccEnv of int
	| AccField of (unit -> value) * string
	| AccArray of (unit -> value) * (unit -> value)

exception Runtime of value
exception Builtin_error

exception Error of string * Globals.pos list

exception Abort
exception Continue
exception Break of value
exception Return of value
exception Invalid_expr
exception Sys_exit of int

(* ---------------------------------------------------------------------- *)
(* UTILS *)

let get_ctx_ref = ref (fun() -> assert false)
let encode_complex_type_ref = ref (fun t -> assert false)
let decode_complex_type_ref = ref (fun t -> assert false)
let decode_pos_ref = ref (fun v -> assert false)
let encode_type_ref = ref (fun t -> assert false)
let decode_type_ref = ref (fun t -> assert false)
let encode_expr_ref = ref (fun e -> assert false)
let decode_expr_ref = ref (fun e -> assert false)
let encode_texpr_ref = ref (fun e -> assert false)
let decode_texpr_ref = ref (fun e -> assert false)
let encode_clref_ref = ref (fun c -> assert false)
let encode_module_type_ref = ref (fun mt -> assert false)
let enc_hash_ref = ref (fun h -> assert false)
let enc_array_ref = ref (fun l -> assert false)
let dec_array_ref = ref (fun v -> assert false)
let enc_string_ref = ref (fun s -> assert false)
let encode_tvar_ref = ref (fun _ -> assert false)
let decode_path_ref = ref (fun _ -> assert false)
let decode_import_ref = ref (fun _ -> assert false)
let encode_import_ref = ref (fun _ -> assert false)
let eval_expr_ref : (context -> texpr -> value option) ref = ref (fun _ _ -> assert false)
let get_ctx() = (!get_ctx_ref)()
let enc_array (l:value list) : value = (!enc_array_ref) l
let dec_array (l:value) : value list = (!dec_array_ref) l

let decode_complex_type (v:value) : Ast.type_hint = (!decode_complex_type_ref) v
let encode_complex_type (t:Ast.type_hint) : value = (!encode_complex_type_ref) t
let decode_pos (v:value) : Globals.pos = (!decode_pos_ref) v
let encode_type (t:Type.t) : value = (!encode_type_ref) t
let decode_type (v:value) : Type.t = (!decode_type_ref) v
let encode_expr (e:Ast.expr) : value = (!encode_expr_ref) e
let decode_expr (e:value) : Ast.expr = (!decode_expr_ref) e
let encode_texpr (e:Type.texpr) : value = (!encode_texpr_ref) e
let decode_texpr (v:value) : Type.texpr = (!decode_texpr_ref) v
let encode_clref (c:tclass) : value = (!encode_clref_ref) c
let enc_hash (h:('a,'b) Hashtbl.t) : value = (!enc_hash_ref) h
let enc_string (s:string) : value = (!enc_string_ref) s
let encode_tvar (v:tvar) : value = (!encode_tvar_ref) v
let decode_path (v:value) : Ast.placed_type_path = (!decode_path_ref) v
let encode_import (i:Ast.import) : value = (!encode_import_ref) i
let decode_import (v:value) : Ast.import = (!decode_import_ref) v

let to_int f = Int32.of_float (mod_float f 2147483648.0)
let need_32_bits i = Int32.compare (Int32.logand (Int32.add i 0x40000000l) 0x80000000l) Int32.zero <> 0
let best_int i = if need_32_bits i then VInt32 i else VInt (Int32.to_int i)

let make_pos p =
	let low = p.pline land 0xFFFFF in
	{
		Globals.pfile = p.psource;
		Globals.pmin = low;
		Globals.pmax = low + (p.pline lsr 20);
	}

let warn ctx msg p =
	(ctx.curapi.get_com()).Common.warning msg (make_pos p)

let rec pop ctx n =
	if n > 0 then begin
		DynArray.delete_last ctx.stack;
		pop ctx (n - 1);
	end

let pop_ret ctx f n =
	let v = f() in
	pop ctx n;
	v

let push ctx v =
	DynArray.add ctx.stack v

let hash f =
	let h = ref 0 in
	for i = 0 to String.length f - 1 do
		h := !h * 223 + int_of_char (String.unsafe_get f i);
	done;
	if Sys.word_size = 64 then Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int !h) 1) 1) else !h

let constants =
	let h = Hashtbl.create 0 in
	List.iter (fun f -> Hashtbl.add h (hash f) f)
	["done";"read";"write";"min";"max";"file";"args";"loadprim";"loadmodule";"__a";"__s";"h";
	"tag";"index";"length";"message";"pack";"name";"params";"sub";"doc";"kind";"meta";"access";
	"constraints";"opt";"type";"value";"ret";"expr";"field";"values";"get";"__string";"toString";
	"$";"add";"remove";"has";"__t";"module";"isPrivate";"isPublic";"isExtern";"isInterface";"exclude";
	"constructs";"names";"superClass";"interfaces";"fields";"statics";"constructor";"init";"t";
	"gid";"uid";"atime";"mtime";"ctime";"dev";"ino";"nlink";"rdev";"size";"mode";"pos";"len";
	"binops";"unops";"from";"to";"array";"op";"isPostfix";"impl";"resolve";
	"id";"capture";"extra";"v";"ids";"vars";"en";"overrides";"status";"overloads";"path";"namePos"];
	h

let h_get = hash "__get" and h_set = hash "__set"
and h_add = hash "__add" and h_radd = hash "__radd"
and h_sub = hash "__sub" and h_rsub = hash "__rsub"
and h_mult = hash "__mult" and h_rmult = hash "__rmult"
and h_div = hash "__div" and h_rdiv = hash "__rdiv"
and h_mod = hash "__mod" and h_rmod = hash "__rmod"
and h_string = hash "__string" and h_compare = hash "__compare"

and h_constructs = hash "__constructs__" and h_a = hash "__a" and h_s = hash "__s"
and h_class = hash "__class__"

let exc v =
	raise (Runtime v)

let s_value_kind = function
	| VNull -> "VNull"
	| VBool _ -> "VBool"
	| VInt _ -> "VInt"
	| VFloat _ -> "VFloat"
	| VString _ -> "VString"
	| VObject _ -> "VObject"
	| VArray _ -> "VArray"
	| VAbstract _ -> "VAbstract"
	| VFunction _ -> "VFunction"
	| VClosure _ -> "VClosure"
	| VInt32 _ -> "VInt32"

let hash_field ctx f =
	let h = hash f in
	(try
		let f2 = Hashtbl.find ctx.fields_cache h in
		if f <> f2 then exc (VString ("Field conflict between " ^ f ^ " and " ^ f2));
	with Not_found ->
		Hashtbl.add ctx.fields_cache h f);
	h

let field_name ctx fid =
	try
		Hashtbl.find ctx.fields_cache fid
	with Not_found ->
		"???"

let obj hash fields =
	let fields = Array.of_list (List.map (fun (k,v) -> hash k, v) fields) in
	Array.sort (fun (k1,_) (k2,_) -> compare k1 k2) fields;
	{
		ofields = fields;
		oproto = None;
	}

let parse_int s =
	let rec loop_hex i =
		if i = String.length s then s else
		match String.unsafe_get s i with
		| '0'..'9' | 'a'..'f' | 'A'..'F' -> loop_hex (i + 1)
		| _ -> String.sub s 0 i
	in
	let rec loop sp i =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| '0'..'9' -> loop sp (i + 1)
		| ' ' when sp = i -> loop (sp + 1) (i + 1)
		| '-' when i = 0 -> loop sp (i + 1)
		| ('x' | 'X') when i = 1 && String.get s 0 = '0' -> loop_hex (i + 1)
		| _ -> String.sub s sp (i - sp)
	in
	best_int (Int32.of_string (loop 0 0))

let parse_float s =
	let rec loop sp i =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| ' ' when sp = i -> loop (sp + 1) (i + 1)
		| '0'..'9' | '-' | '+' | 'e' | 'E' | '.' -> loop sp (i + 1)
		| _ -> String.sub s sp (i - sp)
	in
	float_of_string (loop 0 0)

let find_sub str sub start =
	let sublen = String.length sub in
	if sublen = 0 then
		0
	else
		let found = ref 0 in
		let len = String.length str in
		try
			for i = start to len - sublen do
				let j = ref 0 in
				while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
					incr j;
					if !j = sublen then begin found := i; raise Exit; end;
				done;
			done;
			raise Not_found
		with
			Exit -> !found

let nargs = function
	| Fun0 _ -> 0
	| Fun1 _ -> 1
	| Fun2 _ -> 2
	| Fun3 _ -> 3
	| Fun4 _ -> 4
	| Fun5 _ -> 5
	| FunVar _ -> -1

let rec get_field o fid =
	let rec loop min max =
		if min < max then begin
			let mid = (min + max) lsr 1 in
			let cid, v = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else
				v
		end else
			match o.oproto with
			| None -> VNull
			| Some p -> get_field p fid
	in
	loop 0 (Array.length o.ofields)

let set_field o fid v =
	let rec loop min max =
		let mid = (min + max) lsr 1 in
		if min < max then begin
			let cid, _ = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else
				Array.unsafe_set o.ofields mid (cid,v)
		end else
			let fields = Array.make (Array.length o.ofields + 1) (fid,v) in
			Array.blit o.ofields 0 fields 0 mid;
			Array.blit o.ofields mid fields (mid + 1) (Array.length o.ofields - mid);
			o.ofields <- fields
	in
	loop 0 (Array.length o.ofields)

let rec remove_field o fid =
	let rec loop min max =
		let mid = (min + max) lsr 1 in
		if min < max then begin
			let cid, v = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else begin
				let fields = Array.make (Array.length o.ofields - 1) (fid,VNull) in
				Array.blit o.ofields 0 fields 0 mid;
				Array.blit o.ofields (mid + 1) fields mid (Array.length o.ofields - mid - 1);
				o.ofields <- fields;
				true
			end
		end else
			false
	in
	loop 0 (Array.length o.ofields)

let rec get_field_opt o fid =
	let rec loop min max =
		if min < max then begin
			let mid = (min + max) lsr 1 in
			let cid, v = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else
				Some v
		end else
			match o.oproto with
			| None -> None
			| Some p -> get_field_opt p fid
	in
	loop 0 (Array.length o.ofields)

let catch_errors ctx ?(final=(fun() -> ())) f =
	let n = DynArray.length ctx.stack in
	try
		let v = f() in
		final();
		Some v
	with Runtime v ->
		pop ctx (DynArray.length ctx.stack - n);
		final();
		let rec loop o =
			if o == ctx.error_proto then true else match o.oproto with None -> false | Some p -> loop p
		in
		(match v with
		| VObject o when loop o ->
			(match get_field o (hash "message"), get_field o (hash "pos") with
			| VObject msg, VAbstract (APos pos) ->
				(match get_field msg h_s with
				| VString msg -> raise (Error.Error (Error.Custom msg,pos))
				| _ -> ());
			| _ -> ());
		| _ -> ());
		raise (Error (ctx.do_string v,List.map (fun s -> make_pos s.cpos) ctx.callstack))
	| Abort ->
		pop ctx (DynArray.length ctx.stack - n);
		final();
		None

let make_library fl =
	let h = Hashtbl.create 0 in
	List.iter (fun (n,f) -> Hashtbl.add h n f) fl;
	h

(* ---------------------------------------------------------------------- *)
(* NEKO INTEROP *)

type primitive = (string * Extc.value * int)

type neko_context = {
	load : string -> int -> primitive;
	call : primitive -> value list -> value;
}

(* try to load dl in order *)
let rec dlopen dls =
	let null = Extc.dlint 0 in
	match dls with
	| dl_path :: dls ->
		let dl = Extc.dlopen dl_path in
		if (Obj.magic dl) == null then
			dlopen dls
		else
			Some dl;
	| _ ->
		None

let neko =
	match dlopen (if Globals.is_windows then
		["neko.dll"]
	else
		(*
			By defualt, the makefile of neko produces libneko.so,
			however, the debian package creates libneko.so.0 without libneko.so...
			The fedora rpm package creates libneko.so linked to libneko.so.1.
		*)
		["libneko.so"; "libneko.so.0"; "libneko.so.1"; "libneko.so.2"; "libneko.dylib"]
	) with
	| None ->
		None
	| Some(neko) ->
	let null = Extc.dlint 0 in
	let load v =
		let s = Extc.dlsym neko v in
		if (Obj.magic s) == null then failwith ("Could not load neko." ^ v);
		s
	in
	ignore(Extc.dlcall0 (load "neko_global_init"));
	let vm = Extc.dlcall1 (load "neko_vm_alloc") null in
	ignore(Extc.dlcall1 (load "neko_vm_select") vm);
	let loader = Extc.dlcall2 (load "neko_default_loader") null null in
	let loadprim =
		let l1 = load "neko_val_field" in
		let l2 = Extc.dlcall1 (load "neko_val_id") (Extc.dlstring "loadprim") in
		Extc.dlcall2 (l1) loader (l2) in

	let callN = load "neko_val_callN" in
	let callEx = load "neko_val_callEx" in
	let copy_string = load "neko_copy_string" in

	let alloc_root = load "neko_alloc_root" in
	let free_root = load "neko_free_root" in

	let alloc_root v =
		let r = Extc.dlcall1 alloc_root (Extc.dlint 1) in
		Extc.dlsetptr r v;
		r
	in
	let free_root r =
		ignore(Extc.dlcall1 free_root r)
	in

	ignore(alloc_root vm);
	ignore(alloc_root loader);
	ignore(alloc_root loadprim);

	let alloc_string s =
		Extc.dlcall2 copy_string (Extc.dlstring s) (Extc.dlint (String.length s))
	in
	let alloc_int (i:int) : Extc.value =
		Obj.magic i
	in
	let loadprim n args =
		let exc = ref null in
		let vargs = [|alloc_string n;alloc_int args|] in
		let p = Extc.dlcall5 callEx loader loadprim (Obj.magic vargs) (Extc.dlint 2) (Obj.magic exc) in
		if !exc != null then failwith ("Failed to load " ^ n ^ ":" ^ string_of_int args);
		ignore(alloc_root p);
		(n,p,args)
	in
	let call_raw_prim (_,p,nargs) (args:Extc.value array) =
		Extc.dlcall3 callN p (Obj.magic args) (Extc.dlint nargs)
	in

	(* a bit tricky since load "val_true" does not work as expected on Windows *)
	let unser = try loadprim "std@unserialize" 2 with _ -> ("",null,0) in

	(* did we fail to load std.ndll ? *)
	if (match unser with ("",_,_) -> true | _ -> false) then None else

	let val_true = call_raw_prim unser [|alloc_string "T";loader|] in
	let val_false = call_raw_prim unser [|alloc_string "F";loader|] in
	let val_null = call_raw_prim unser [|alloc_string "N";loader|] in

	let is_64 = call_raw_prim (loadprim "std@sys_is64" 0) [||] == val_true in
	let alloc_i32, is_v2 = (try load "neko_alloc_int32", true with _ -> Obj.magic 0, false) in
	let alloc_i32 = if is_v2 then
		(fun i -> Extc.dlcall1 alloc_i32 (Extc.dlint32 i))
	else
		(fun i -> alloc_int (Int32.to_int (if Int32.compare i Int32.zero < 0 then Int32.logand i 0x7FFFFFFFl else Int32.logor i 0x80000000l)))
	in
	let tag_bits = if is_v2 then 4 else 3 in
	let tag_mask = (1 lsl tag_bits) - 1 in
	let ptr_size = if is_64 then 8 else 4 in
	let val_field v i = Extc.dladdr v ((i + 1) * ptr_size) in
	let val_str v = Extc.dladdr v 4 in
	let val_fun_env v = Extc.dladdr v (8 + ptr_size) in

	(* alloc support *)

	let alloc_function = load "neko_alloc_function" in
	let alloc_array = load "neko_alloc_array" in
	let alloc_float = load "neko_alloc_float" in
	let alloc_object = load "neko_alloc_object" in
	let alloc_field = load "neko_alloc_field" in
	let alloc_abstract = load "neko_alloc_abstract" in
	let val_gc = load "neko_val_gc" in
	let val_field_name = load "neko_val_field_name" in
	let val_iter_fields = load "neko_val_iter_fields" in
	let gen_callback = Extc.dlcaml_callback 2 in

	(* roots *)

	let on_abstract_gc = Extc.dlcaml_callback 1 in
	let root_index = ref 0 in
	let roots = Hashtbl.create 0 in
	Callback.register "dlcallb1" (fun a ->
		let index : int = Obj.magic (Extc.dlptr (val_field a 1)) in
		Hashtbl.remove roots index;
		null
	);

	(* wrapping *)

	let copy_string v =
		let head = Extc.dltoint (Extc.dlptr v) in
		let size = head asr tag_bits in
		let s = String.create size in
		Extc.dlmemcpy (Extc.dlstring s) (val_str v) size;
		s
	in

	let buffers = ref [] in

	let rec value_neko ?(obj=VNull) = function
		| VNull -> val_null
		| VBool b -> if b then val_true else val_false
		| VInt i -> alloc_int i
		| VAbstract (ANekoAbstract a) -> a
		| VAbstract (ANekoBuffer (VString buf)) ->
			let v = value_neko (VString buf) in
			buffers := (buf,v) :: !buffers;
			v
		| VString s ->
			let v = alloc_string s in (* make a copy *)
			ignore(copy_string v);
			v
		| VObject o as obj ->
			let vo = Extc.dlcall1 alloc_object null in
			Array.iter (fun (id,v) ->
				ignore(Extc.dlcall3 alloc_field vo (Extc.dlint id) (value_neko ~obj v))
			) o.ofields;
			vo
		| VClosure _ ->
			failwith "Closure not supported"
		| VFunction f ->
			let callb = Extc.dlcall3 alloc_function gen_callback (Extc.dlint (-1)) (Obj.magic "<callback>") in
			let index = !root_index in
			incr root_index;
			Hashtbl.add roots index (f,obj);
			let a = Extc.dlcall2 alloc_abstract null (Obj.magic index) in
			if Extc.dlptr (val_field a 1) != Obj.magic index then assert false;
			ignore(Extc.dlcall2 val_gc a on_abstract_gc);
			Extc.dlsetptr (val_fun_env callb) a;
			callb
		| VArray a ->
			let va = Extc.dlcall1 alloc_array (Extc.dlint (Array.length a)) in
			Array.iteri (fun i v ->
				Extc.dlsetptr (val_field va i) (value_neko v)
			) a;
			va
		| VFloat f ->
			Extc.dlcall1 alloc_float (Obj.magic f)
		| VAbstract _ ->
			failwith "Abstract not supported"
		| VInt32 i ->
			alloc_i32 i
	in
	let obj_r = ref [] in
	let obj_fun = (fun v id -> obj_r := (v,id) :: !obj_r; val_null) in
	let rec neko_value (v:Extc.value) =
		if Obj.is_int (Obj.magic v) then
			VInt (Obj.magic v)
		else
			let head = Extc.dltoint (Extc.dlptr v) in
			match head land tag_mask with
			| 0 -> VNull
			| 2 -> VBool (v == val_true)
			| 3 -> VString (copy_string v)
			| 4 ->
				ignore(Extc.dlcall3 val_iter_fields v (Extc.dlcallback 2) (Obj.magic obj_fun));
				let r = !obj_r in
				obj_r := [];
				let ctx = get_ctx() in
				let fields = List.rev_map (fun (v,id) ->
					let iid = Extc.dltoint id in
					if not (Hashtbl.mem ctx.fields_cache iid) then begin
						let name = copy_string (Extc.dlcall1 val_field_name id) in
						ignore(hash_field ctx name);
					end;
					iid, neko_value v
				) r in
				VObject { ofields = Array.of_list fields; oproto = None }
			| 5 ->
				VArray (Array.init (head asr tag_bits) (fun i -> neko_value (Extc.dlptr (val_field v i))))
			| 7 ->
				let r = alloc_root v in
				let a = ANekoAbstract v in
				Gc.finalise (fun _ -> free_root r) a;
				VAbstract a
			| t ->
				failwith ("Unsupported Neko value tag " ^ string_of_int t)
	in

	Callback.register "dlcallb2" (fun args nargs ->
		(* get back the VM env, which was set in value_neko *)
		let env = Extc.dlptr (Extc.dladdr vm (2 * ptr_size)) in
		(* extract the index stored in abstract data *)
		let index : int = Obj.magic (Extc.dlptr (val_field env 1)) in
		let f, obj = (try Hashtbl.find roots index with Not_found -> assert false) in
		let nargs = Extc.dltoint nargs in
		let rec loop i =
			if i = nargs then [] else neko_value (Extc.dlptr (Extc.dladdr args (i * ptr_size))) :: loop (i + 1)
		in
		let v = (get_ctx()).do_call obj (VFunction f) (loop 0) { psource = "<callback>"; pline = 0; } in
		value_neko v
	);

	let callprim (n,p,nargs) args =
		let arr = Array.of_list (List.map value_neko args) in
		let exc = ref null in
		if Array.length arr <> nargs then failwith n;
		let ret = Extc.dlcall5 callEx val_null p (Obj.magic arr) (Extc.dlint nargs) (Obj.magic exc) in
		if !exc != null then raise (Runtime (neko_value !exc));
		(match !buffers with
		| [] -> ()
		| l ->
			buffers := [];
			(* copy back data *)
			List.iter (fun (buf,v) ->
				Extc.dlmemcpy (Extc.dlstring buf) (val_str v) (String.length buf);
			) l);
		neko_value ret
	in
	Some {
		load = loadprim;
		call = callprim;
	}

(* ---------------------------------------------------------------------- *)
(* BUILTINS *)

let builtins =
	let p = { psource = "<builtin>"; pline = 0 } in
	let error() =
		raise Builtin_error
	in
	let vint = function
		| VInt n -> n
		| _ -> error()
	in
	let varray = function
		| VArray a -> a
		| _ -> error()
	in
	let vstring = function
		| VString s -> s
		| _ -> error()
	in
	let vobj = function
		| VObject o -> o
		| _ -> error()
	in
	let vfun = function
		| VFunction f -> f
		| VClosure (cl,f) -> FunVar (f cl)
		| _ -> error()
	in
	let vhash = function
		| VAbstract (AHash h) -> h
		| _ -> error()
	in
	let build_stack sl =
		let make p =
			let p = make_pos p in
			VArray [|VString p.Globals.pfile;VInt (Lexer.get_error_line p)|]
		in
		VArray (Array.of_list (List.map make sl))
	in
	let do_closure args args2 =
		match args with
		| f :: obj :: args ->
			(get_ctx()).do_call obj f (args @ args2) p
		| _ ->
			assert false
	in
	let funcs = [
	(* array *)
		"array", FunVar (fun vl -> VArray (Array.of_list vl));
		"amake", Fun1 (fun v -> VArray (Array.create (vint v) VNull));
		"acopy", Fun1 (fun a -> VArray (Array.copy (varray a)));
		"asize", Fun1 (fun a -> VInt (Array.length (varray a)));
		"asub", Fun3 (fun a p l -> VArray (Array.sub (varray a) (vint p) (vint l)));
		"ablit", Fun5 (fun dst dstp src p l ->
			Array.blit (varray src) (vint p) (varray dst) (vint dstp) (vint l);
			VNull
		);
		"aconcat", Fun1 (fun arr ->
			let arr = Array.map varray (varray arr) in
			VArray (Array.concat (Array.to_list arr))
		);
	(* string *)
		"string", Fun1 (fun v -> VString ((get_ctx()).do_string v));
		"smake", Fun1 (fun l -> VString (String.make (vint l) '\000'));
		"ssize", Fun1 (fun s -> VInt (String.length (vstring s)));
		"scopy", Fun1 (fun s -> VString (String.copy (vstring s)));
		"ssub", Fun3 (fun s p l -> VString (String.sub (vstring s) (vint p) (vint l)));
		"sget", Fun2 (fun s p ->
			try VInt (int_of_char (String.get (vstring s) (vint p))) with Invalid_argument _ -> VNull
		);
		"sset", Fun3 (fun s p c ->
			let c = char_of_int ((vint c) land 0xFF) in
			try
				String.set (vstring s) (vint p) c;
				VInt (int_of_char c)
			with Invalid_argument _ -> VNull);
		"sblit", Fun5 (fun dst dstp src p l ->
			String.blit (vstring src) (vint p) (vstring dst) (vint dstp) (vint l);
			VNull
		);
		"sfind", Fun3 (fun src pos pat ->
			try VInt (find_sub (vstring src) (vstring pat) (vint pos)) with Not_found -> VNull
		);
	(* object *)
		"new", Fun1 (fun o ->
			match o with
			| VNull -> VObject { ofields = [||]; oproto = None }
			| VObject o -> VObject { ofields = Array.copy o.ofields; oproto = o.oproto }
			| _ -> error()
		);
		"objget", Fun2 (fun o f ->
			match o with
			| VObject o -> get_field o (vint f)
			| _ -> VNull
		);
		"objset", Fun3 (fun o f v ->
			match o with
			| VObject o -> set_field o (vint f) v; v
			| _ -> VNull
		);
		"objcall", Fun3 (fun o f pl ->
			match o with
			| VObject oo ->
				(get_ctx()).do_call o (get_field oo (vint f)) (Array.to_list (varray pl)) p
			| _ -> VNull
		);
		"objfield", Fun2 (fun o f ->
			match o with
			| VObject o ->
				let p = o.oproto in
				o.oproto <- None;
				let v = get_field_opt o (vint f) in
				o.oproto <- p;
				VBool (v <> None)
			| _ -> VBool false
		);
		"objremove", Fun2 (fun o f ->
			VBool (remove_field (vobj o) (vint f))
		);
		"objfields", Fun1 (fun o ->
			VArray (Array.map (fun (fid,_) -> VInt fid) (vobj o).ofields)
		);
		"hash", Fun1 (fun v -> VInt (hash_field (get_ctx()) (vstring v)));
		"fasthash", Fun1 (fun v -> VInt (hash (vstring v)));
		"field", Fun1 (fun v ->
			try VString (Hashtbl.find (get_ctx()).fields_cache (vint v)) with Not_found -> VNull
		);
		"objsetproto", Fun2 (fun o p ->
			let o = vobj o in
			(match p with
			| VNull -> o.oproto <- None
			| VObject p -> o.oproto <- Some p
			| _ -> error());
			VNull;
		);
		"objgetproto", Fun1 (fun o ->
			match (vobj o).oproto with
			| None -> VNull
			| Some p -> VObject p
		);
	(* function *)
		"nargs", Fun1 (fun f ->
			VInt (nargs (vfun f))
		);
		"call", Fun3 (fun f o args ->
			(get_ctx()).do_call o f (Array.to_list (varray args)) p
		);
		"closure", FunVar (fun vl ->
			match vl with
			| VFunction f :: _ :: _ ->
				VClosure (vl, do_closure)
			| _ -> exc (VString "Can't create closure : value is not a function")
		);
		"apply", FunVar (fun vl ->
			match vl with
			| f :: args ->
				let f = vfun f in
				VFunction (FunVar (fun args2 -> (get_ctx()).do_call VNull (VFunction f) (args @ args2) p))
			| _ -> exc (VString "Invalid closure arguments number")
		);
		"varargs", Fun1 (fun f ->
			match f with
			| VFunction (FunVar _) | VFunction (Fun1 _) | VClosure _ ->
				VFunction (FunVar (fun vl -> (get_ctx()).do_call VNull f [VArray (Array.of_list vl)] p))
			| _ ->
				error()
		);
	(* numbers *)
		(* skip iadd, isub, idiv, imult *)
		"isnan", Fun1 (fun f ->
			match f with
			| VFloat f -> VBool (f <> f)
			| _ -> VBool false
		);
		"isinfinite", Fun1 (fun f ->
			match f with
			| VFloat f -> VBool (f = infinity || f = neg_infinity)
			| _ -> VBool false
		);
		"int", Fun1 (fun v ->
			match v with
			| VInt _ | VInt32 _ -> v
			| VFloat f -> best_int (to_int f)
			| VString s -> (try parse_int s with _ -> VNull)
			| _ -> VNull
		);
		"float", Fun1 (fun v ->
			match v with
			| VInt i -> VFloat (float_of_int i)
			| VInt32 i -> VFloat (Int32.to_float i)
			| VFloat _ -> v
			| VString s -> (try VFloat (parse_float s) with _ -> VNull)
			| _ -> VNull
		);
	(* abstract *)
		"getkind", Fun1 (fun v ->
			match v with
			| VAbstract a -> VAbstract (AKind a)
			| VInt32 _ -> VAbstract (AKind AInt32Kind)
			| _ -> error()
		);
		"iskind", Fun2 (fun v k ->
			match v, k with
			| VAbstract a, VAbstract (AKind k) -> VBool (Obj.tag (Obj.repr a) = Obj.tag (Obj.repr k))
			| VInt32 _, VAbstract (AKind AInt32Kind) -> VBool true
			| _, VAbstract (AKind _) -> VBool false
			| _ -> error()
		);
	(* hash *)
		"hkey", Fun1 (fun v -> VInt (Hashtbl.hash v));
		"hnew", Fun1 (fun v ->
			VAbstract (AHash (match v with
			| VNull -> Hashtbl.create 0
			| VInt n -> Hashtbl.create n
			| _ -> error()))
		);
		"hresize", Fun1 (fun v -> VNull);
		"hget", Fun3 (fun h k cmp ->
			if cmp <> VNull then assert false;
			(try Hashtbl.find (vhash h) k with Not_found -> VNull)
		);
		"hmem", Fun3 (fun h k cmp ->
			if cmp <> VNull then assert false;
			VBool (Hashtbl.mem (vhash h) k)
		);
		"hremove", Fun3 (fun h k cmp ->
			if cmp <> VNull then assert false;
			let h = vhash h in
			let old = Hashtbl.mem h k in
			if old then Hashtbl.remove h k;
			VBool old
		);
		"hset", Fun4 (fun h k v cmp ->
			if cmp <> VNull then assert false;
			let h = vhash h in
			let old = Hashtbl.mem h k in
			Hashtbl.replace h k v;
			VBool (not old);
		);
		"hadd", Fun3 (fun h k v ->
			let h = vhash h in
			let old = Hashtbl.mem h k in
			Hashtbl.add h k v;
			VBool (not old);
		);
		"hiter", Fun2 (fun h f -> Hashtbl.iter (fun k v -> ignore ((get_ctx()).do_call VNull f [k;v] p)) (vhash h); VNull);
		"hcount", Fun1 (fun h -> VInt (Hashtbl.length (vhash h)));
		"hsize", Fun1 (fun h -> VInt (Hashtbl.length (vhash h)));
	(* misc *)
		"print", FunVar (fun vl -> List.iter (fun v ->
			let ctx = get_ctx() in
			let com = ctx.curapi.get_com() in
			com.print (ctx.do_string v)
		) vl; VNull);
		"throw", Fun1 (fun v -> exc v);
		"rethrow", Fun1 (fun v ->
			let ctx = get_ctx() in
			ctx.callstack <- List.rev (List.map (fun p -> { cpos = p; cthis = ctx.vthis; cstack = DynArray.length ctx.stack; cenv = ctx.venv }) ctx.exc) @ ctx.callstack;
			exc v
		);
		"istrue", Fun1 (fun v ->
			match v with
			| VNull | VInt 0 | VBool false | VInt32 0l -> VBool false
			| _ -> VBool true
		);
		"not", Fun1 (fun v ->
			match v with
			| VNull | VInt 0 | VBool false | VInt32 0l -> VBool true
			| _ -> VBool false
		);
		"typeof", Fun1 (fun v ->
			VInt (match v with
			| VNull -> 0
			| VInt _ | VInt32 _ -> 1
			| VFloat _ -> 2
			| VBool _ -> 3
			| VString _ -> 4
			| VObject _ -> 5
			| VArray _ -> 6
			| VFunction _ | VClosure _ -> 7
			| VAbstract _ -> 8)
		);
		"compare", Fun2 (fun a b ->
			match (get_ctx()).do_compare a b with
			| CUndef -> VNull
			| CEq -> VInt 0
			| CSup -> VInt 1
			| CInf -> VInt (-1)
		);
		"pcompare", Fun2 (fun a b ->
			assert false
		);
		"excstack", Fun0 (fun() ->
			build_stack (get_ctx()).exc
		);
		"callstack", Fun0 (fun() ->
			build_stack (List.map (fun s -> s.cpos) (get_ctx()).callstack)
		);
		"version", Fun0 (fun() ->
			VInt 200
		);
	(* extra *)
		"use_neko_dll", Fun0 (fun() ->
			VBool (neko <> None)
		);
	] in
	let vals = [
		"tnull", VInt 0;
		"tint", VInt 1;
		"tfloat", VInt 2;
		"tbool", VInt 3;
		"tstring", VInt 4;
		"tobject", VInt 5;
		"tarray", VInt 6;
		"tfunction", VInt 7;
		"tabstract", VInt 8;
	] in
	let h = Hashtbl.create 0 in
	List.iter (fun (n,f) -> Hashtbl.add h n (VFunction f)) funcs;
	List.iter (fun (n,v) -> Hashtbl.add h n v) vals;
	h

(* ---------------------------------------------------------------------- *)
(* STD LIBRARY *)

let free_abstract a =
	match a with
	| VAbstract vp -> Obj.set_tag (Obj.repr vp) 0 (* this will mute it as Deallocated *)
	| _ -> assert false

let std_lib =
	let p = { psource = "<stdlib>"; pline = 0 } in
	let error() =
		raise Builtin_error
	in
	let make_list l =
		let rec loop acc = function
			| [] -> acc
			| x :: l -> loop (VArray [|x;acc|]) l
		in
		loop VNull (List.rev l)
	in
	let num = function
		| VInt i -> float_of_int i
		| VInt32 i -> Int32.to_float i
		| VFloat f -> f
		| _ -> error()
	in
	let make_date f =
		VInt32 (Int32.of_float f)
	in
	let date = function
		| VInt32 i -> Int32.to_float i
		| VInt i -> float_of_int i
		| _ -> error()
	in
	let make_i32 i =
		VInt32 i
	in
	let int32 = function
		| VInt i -> Int32.of_int i
		| VInt32 i -> i
		| _ -> error()
	in
	let vint = function
		| VInt n -> n
		| _ -> error()
	in
	let vstring = function
		| VString s -> s
		| _ -> error()
	in
	let int32_addr h =
		let base = Int32.to_int (Int32.logand h 0xFFFFFFl) in
		let str = Printf.sprintf "%ld.%d.%d.%d" (Int32.shift_right_logical h 24) (base lsr 16) ((base lsr 8) land 0xFF) (base land 0xFF) in
		Unix.inet_addr_of_string str
	in
	let int32_op op = Fun2 (fun a b -> make_i32 (op (int32 a) (int32 b))) in
	make_library ([
	(* math *)
		"math_atan2", Fun2 (fun a b -> VFloat (atan2 (num a) (num b)));
		"math_pow", Fun2 (fun a b -> VFloat ((num a) ** (num b)));
		"math_abs", Fun1 (fun v ->
			match v with
			| VInt i -> VInt (abs i)
			| VInt32 i -> VInt32 (Int32.abs i)
			| VFloat f -> VFloat (abs_float f)
			| _ -> error()
		);
		"math_ceil", Fun1 (fun v -> match v with VInt _ | VInt32 _ -> v | _ -> best_int (to_int (ceil (num v))));
		"math_floor", Fun1 (fun v -> match v with VInt _ | VInt32 _ -> v | _ -> best_int (to_int (floor (num v))));
		"math_round", Fun1 (fun v -> match v with VInt _ | VInt32 _ -> v | _ -> best_int (to_int (floor (num v +. 0.5))));
		"math_pi", Fun0 (fun() -> VFloat (4.0 *. atan 1.0));
		"math_sqrt", Fun1 (fun v -> VFloat (sqrt (num v)));
		"math_atan", Fun1 (fun v -> VFloat (atan (num v)));
		"math_cos", Fun1 (fun v -> VFloat (cos (num v)));
		"math_sin", Fun1 (fun v -> VFloat (sin (num v)));
		"math_tan", Fun1 (fun v -> VFloat (tan (num v)));
		"math_log", Fun1 (fun v -> VFloat (Pervasives.log (num v)));
		"math_exp", Fun1 (fun v -> VFloat (exp (num v)));
		"math_acos", Fun1 (fun v -> VFloat (acos (num v)));
		"math_asin", Fun1 (fun v -> VFloat (asin (num v)));
		"math_fceil", Fun1 (fun v -> VFloat (ceil (num v)));
		"math_ffloor", Fun1 (fun v -> VFloat (floor (num v)));
		"math_fround", Fun1 (fun v -> VFloat (floor (num v +. 0.5)));
		"math_int", Fun1 (fun v ->
			match v with
			| VInt _ | VInt32 _ -> v
			| VFloat f -> best_int (to_int (if f < 0. then ceil f else floor f))
			| _ -> error()
		);
	(* buffer *)
		"buffer_new", Fun0 (fun() ->
			VAbstract (ABuffer (Buffer.create 0))
		);
		"buffer_add", Fun2 (fun b v ->
			match b with
			| VAbstract (ABuffer b) -> Buffer.add_string b ((get_ctx()).do_string v); VNull
			| _ -> error()
		);
		"buffer_add_char", Fun2 (fun b v ->
			match b, v with
			| VAbstract (ABuffer b), VInt n when n >= 0 && n < 256 -> Buffer.add_char b (char_of_int n); VNull
			| _ -> error()
		);
		"buffer_add_sub", Fun4 (fun b s p l ->
			match b, s, p, l with
			| VAbstract (ABuffer b), VString s, VInt p, VInt l -> (try Buffer.add_substring b s p l; VNull with _ -> error())
			| _ -> error()
		);
		"buffer_string", Fun1 (fun b ->
			match b with
			| VAbstract (ABuffer b) -> VString (Buffer.contents b)
			| _ -> error()
		);
		"buffer_reset", Fun1 (fun b ->
			match b with
			| VAbstract (ABuffer b) -> Buffer.reset b; VNull;
			| _ -> error()
		);
		"buffer_get_length", Fun1 (fun b ->
			match b with
			| VAbstract (ABuffer b) -> VInt (Buffer.length b)
			| _ -> error()
		);
	(* date *)
		"date_now", Fun0 (fun () ->
			make_date (Unix.time())
		);
		"date_new", Fun1 (fun v ->
			make_date (match v with
			| VNull -> Unix.time()
			| VString s ->
				(match String.length s with
				| 19 ->
					let r = Str.regexp "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)$" in
					if not (Str.string_match r s 0) then exc (VString ("Invalid date format : " ^ s));
					let t = Unix.localtime (Unix.time()) in
					let t = { t with
						tm_year = int_of_string (Str.matched_group 1 s) - 1900;
						tm_mon = int_of_string (Str.matched_group 2 s) - 1;
						tm_mday = int_of_string (Str.matched_group 3 s);
						tm_hour = int_of_string (Str.matched_group 4 s);
						tm_min = int_of_string (Str.matched_group 5 s);
						tm_sec = int_of_string (Str.matched_group 6 s);
					} in
					fst (Unix.mktime t)
				| 10 ->
					assert false
				| 8 ->
					assert false
				| _ ->
					exc (VString ("Invalid date format : " ^ s)));
			| _ -> error())
		);
		"date_set_hour", Fun4 (fun d h m s ->
			let d = date d in
			let t = Unix.localtime d in
			make_date (fst (Unix.mktime { t with tm_hour = vint h; tm_min = vint m; tm_sec = vint s }))
		);
		"date_set_day", Fun4 (fun d y m da ->
			let d = date d in
			let t = Unix.localtime d in
			make_date (fst (Unix.mktime { t with tm_year = vint y - 1900; tm_mon = vint m - 1; tm_mday = vint da }))
		);
		"date_format", Fun2 (fun d fmt ->
			match fmt with
			| VNull ->
				let t = Unix.localtime (date d) in
				VString (Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec)
			| VString "%w" ->
				(* week day *)
				let t = Unix.localtime (date d) in
				VString (string_of_int t.tm_wday)
			| VString _ ->
				exc (VString "Custom date format is not supported") (* use native Haxe implementation *)
			| _ ->
				error()
		);
		"date_get_hour", Fun1 (fun d ->
			let t = Unix.localtime (date d) in
			let o = obj (hash_field (get_ctx())) [
				"h", VInt t.tm_hour;
				"m", VInt t.tm_min;
				"s", VInt t.tm_sec;
			] in
			VObject o
		);
		"date_get_day", Fun1 (fun d ->
			let t = Unix.localtime (date d) in
			let o = obj (hash_field (get_ctx())) [
				"d", VInt t.tm_mday;
				"m", VInt (t.tm_mon + 1);
				"y", VInt (t.tm_year + 1900);
			] in
			VObject o
		);
	(* string *)
		"string_split", Fun2 (fun s d ->
			make_list (match s, d with
			| VString "", VString _ -> [VString ""]
			| VString s, VString "" -> Array.to_list (Array.init (String.length s) (fun i -> VString (String.make 1 (String.get s i))))
			| VString s, VString d -> List.map (fun s -> VString s) (ExtString.String.nsplit s d)
			| _ -> error())
		);
		"url_encode", Fun1 (fun s ->
			let s = vstring s in
			let b = Buffer.create 0 in
			let hex = "0123456789ABCDEF" in
			for i = 0 to String.length s - 1 do
				let c = String.unsafe_get s i in
				match c with
				| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' | '.' ->
					Buffer.add_char b c
				| _ ->
					Buffer.add_char b '%';
					Buffer.add_char b (String.unsafe_get hex (int_of_char c lsr 4));
					Buffer.add_char b (String.unsafe_get hex (int_of_char c land 0xF));
			done;
			VString (Buffer.contents b)
		);
		"url_decode", Fun1 (fun s ->
			let s = vstring s in
			let b = Buffer.create 0 in
			let len = String.length s in
			let decode c =
				match c with
				| '0'..'9' -> Some (int_of_char c - int_of_char '0')
				| 'a'..'f' -> Some (int_of_char c - int_of_char 'a' + 10)
				| 'A'..'F' -> Some (int_of_char c - int_of_char 'A' + 10)
				| _ -> None
			in
			let rec loop i =
				if i = len then () else
				let c = String.unsafe_get s i in
				match c with
				| '%' ->
					let p1 = (try decode (String.get s (i + 1)) with _ -> None) in
					let p2 = (try decode (String.get s (i + 2)) with _ -> None) in
					(match p1, p2 with
					| Some c1, Some c2 ->
						Buffer.add_char b (char_of_int ((c1 lsl 4) lor c2));
						loop (i + 3)
					| _ ->
						loop (i + 1));
				| '+' ->
					Buffer.add_char b ' ';
					loop (i + 1)
				| c ->
					Buffer.add_char b c;
					loop (i + 1)
			in
			loop 0;
			VString (Buffer.contents b)
		);
		"base_encode", Fun2 (fun s b ->
			match s, b with
			| VString s, VString "0123456789abcdef" when String.length s = 16 ->
				VString (Digest.to_hex s)
			| VString s, VString b ->
				if String.length b <> 64 then assert false;
				let tbl = Array.init 64 (String.unsafe_get b) in
				VString (Base64.str_encode ~tbl s)
			| _ -> error()
		);
		"base_decode", Fun2 (fun s b ->
			let s = vstring s in
			let b = vstring b in
			if String.length b <> 64 then assert false;
			let tbl = Array.init 64 (String.unsafe_get b) in
			VString (Base64.str_decode ~tbl:(Base64.make_decoding_table tbl) s)
		);
		"make_md5", Fun1 (fun s ->
			VString (Digest.string (vstring s))
		);
		(* sprintf *)
	(* int32 *)
		"int32_new", Fun1 (fun v ->
			match v with
			| VInt32 _ -> v
			| VInt i -> make_i32 (Int32.of_int i)
			| VFloat f -> make_i32 (Int32.of_float f)
			| _ -> error()
		);
		"int32_to_int", Fun1 (fun v ->
			let v = int32 v in
			let i = Int32.to_int v in
			if Int32.compare (Int32.of_int i) v <> 0 then error();
			VInt i
		);
		"int32_to_float", Fun1 (fun v ->
			VFloat (Int32.to_float (int32 v))
		);
		"int32_compare", Fun2 (fun a b ->
			VInt (Int32.compare (int32 a) (int32 b))
		);
		"int32_add", int32_op Int32.add;
		"int32_sub", int32_op Int32.sub;
		"int32_mul", int32_op Int32.mul;
		"int32_div", int32_op Int32.div;
		"int32_shl", int32_op (fun a b -> Int32.shift_left a (Int32.to_int b));
		"int32_shr", int32_op (fun a b -> Int32.shift_right a (Int32.to_int b));
		"int32_ushr", int32_op (fun a b -> Int32.shift_right_logical a (Int32.to_int b));
		"int32_mod", int32_op Int32.rem;
		"int32_or", int32_op Int32.logor;
		"int32_and", int32_op Int32.logand;
		"int32_xor", int32_op Int32.logxor;
		"int32_neg", Fun1 (fun v -> make_i32 (Int32.neg (int32 v)));
		"int32_complement", Fun1 (fun v -> make_i32 (Int32.lognot (int32 v)));
	(* misc *)
		"same_closure", Fun2 (fun a b ->
			VBool (match a, b with
			| VClosure (la,fa), VClosure (lb,fb) ->
				fa == fb && List.length la = List.length lb && List.for_all2 (fun a b -> (get_ctx()).do_compare a b = CEq) la lb
			| VFunction a, VFunction b -> a == b
			| _ -> false)
		);
		"double_bytes", Fun2 (fun f big ->
			let f = (match f with VFloat f -> f | VInt i -> float_of_int i | _ -> error()) in
			match big with
			| VBool big ->
				let ch = IO.output_string() in
				if big then IO.BigEndian.write_double ch f else IO.write_double ch f;
				VString (IO.close_out ch)
			| _ ->
				error()
		);
		"float_bytes", Fun2 (fun f big ->
			let f = (match f with VFloat f -> f | VInt i -> float_of_int i | _ -> error()) in
			match big with
			| VBool big ->
				let ch = IO.output_string() in
				let i = Int32.bits_of_float f in
				if big then IO.BigEndian.write_real_i32 ch i else IO.write_real_i32 ch i;
				VString (IO.close_out ch)
			| _ ->
				error()
		);
		"double_of_bytes", Fun2 (fun s big ->
			match s, big with
			| VString s, VBool big when String.length s = 8 ->
				let ch = IO.input_string s in
				VFloat (if big then IO.BigEndian.read_double ch else IO.read_double ch)
			| _ ->
				error()
		);
		"float_of_bytes", Fun2 (fun s big ->
			match s, big with
			| VString s, VBool big when String.length s = 4 ->
				let ch = IO.input_string s in
				VFloat (Int32.float_of_bits (if big then IO.BigEndian.read_real_i32 ch else IO.read_real_i32 ch))
			| _ ->
				error()
		);
	(* random *)
		"random_new", Fun0 (fun() -> VAbstract (ARandom (ref (Random.State.make_self_init()))));
		"random_set_seed", Fun2 (fun r s ->
			match r, s with
			| VAbstract (ARandom r), VInt seed -> r := Random.State.make [|seed|]; VNull
			| VAbstract (ARandom r), VInt32 seed -> r := Random.State.make [|Int32.to_int seed|]; VNull
			| _ -> error()
		);
		"random_int", Fun2 (fun r s ->
			match r, s with
			| VAbstract (ARandom r), VInt max -> VInt (Random.State.int (!r) (if max <= 0 then 1 else max))
			| _ -> error()
		);
		"random_float", Fun1 (fun r ->
			match r with
			| VAbstract (ARandom r) -> VFloat (Random.State.float (!r) 1.0)
			| _ -> error()
		);
	(* file *)
		"file_open", Fun2 (fun f r ->
			match f, r with
			| VString f, VString r ->
				let perms = 0o666 in
				VAbstract (match r with
					| "r" -> AFRead (open_in_gen [Open_rdonly] 0 f,ref false)
					| "rb" -> AFRead (open_in_gen [Open_rdonly;Open_binary] 0 f,ref false)
					| "w" -> AFWrite (open_out_gen [Open_wronly;Open_creat;Open_trunc] perms f)
					| "wb" -> AFWrite (open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] perms f)
					| "a" -> AFWrite (open_out_gen [Open_append;Open_creat] perms f)
					| "ab" -> AFWrite (open_out_gen [Open_append;Open_creat;Open_binary] perms f)
					| _ -> error())
			| _ -> error()
		);
		"file_close", Fun1 (fun vf ->
			(match vf with
			| VAbstract (AFRead (f,_)) -> close_in f; free_abstract vf;
			| VAbstract (AFWrite f) -> close_out f; free_abstract vf;
			| _ -> error());
			VNull
		);
		(* file_name *)
		"file_write", Fun4 (fun f s p l ->
			match f, s, p, l with
			| VAbstract (AFWrite f), VString s, VInt p, VInt l -> output f s p l; VInt l
			| _ -> error()
		);
		"file_read", Fun4 (fun f s p l ->
			match f, s, p, l with
			| VAbstract (AFRead (f,r)), VString s, VInt p, VInt l ->
				let n = input f s p l in
				if n = 0 then begin
					r := true;
					exc (VArray [|VString "file_read"|]);
				end;
				VInt n
			| _ -> error()
		);
		"file_write_char", Fun2 (fun f c ->
			match f, c with
			| VAbstract (AFWrite f), VInt c -> output_char f (char_of_int c); VNull
			| _ -> error()
		);
		"file_read_char", Fun1 (fun f ->
			match f with
			| VAbstract (AFRead (f,r)) -> VInt (int_of_char (try input_char f with _ -> r := true; exc (VArray [|VString "file_read_char"|])))
			| _ -> error()
		);
		"file_seek", Fun3 (fun f pos mode ->
			match f, pos, mode with
			| VAbstract (AFRead (f,r)), VInt pos, VInt mode ->
				r := false;
				seek_in f (match mode with 0 -> pos | 1 -> pos_in f + pos | 2 -> in_channel_length f + pos | _ -> error());
				VNull;
			| VAbstract (AFWrite f), VInt pos, VInt mode ->
				seek_out f (match mode with 0 -> pos | 1 -> pos_out f + pos | 2 -> out_channel_length f + pos | _ -> error());
				VNull;
			| _ -> error()
		);
		"file_tell", Fun1 (fun f ->
			match f with
			| VAbstract (AFRead (f,_)) -> VInt (pos_in f)
			| VAbstract (AFWrite f) -> VInt (pos_out f)
			| _ -> error()
		);
		"file_eof", Fun1 (fun f ->
			match f with
			| VAbstract (AFRead (f,r)) ->
				VBool !r
			| _ -> error()
		);
		"file_flush", Fun1 (fun f ->
			(match f with
			| VAbstract (AFWrite f) -> flush f
			| _ -> error());
			VNull
		);
		"file_contents", Fun1 (fun f ->
			match f with
			| VString f -> VString (Std.input_file ~bin:true f)
			| _ -> error()
		);
		"file_stdin", Fun0 (fun() -> VAbstract (AFRead (Pervasives.stdin, ref false)));
		"file_stdout", Fun0 (fun() -> VAbstract (AFWrite Pervasives.stdout));
		"file_stderr", Fun0 (fun() -> VAbstract (AFWrite Pervasives.stderr));
	(* serialize *)
		(* TODO *)
	(* socket *)
		"socket_init", Fun0 (fun() -> VNull);
		"socket_new", Fun1 (fun v ->
			match v with
			| VBool b -> VAbstract (ASocket (Unix.socket PF_INET (if b then SOCK_DGRAM else SOCK_STREAM) 0));
			| _ -> error()
		);
		"socket_close", Fun1 (fun vs ->
			match vs with
			| VAbstract (ASocket s) -> Unix.close s; free_abstract vs; VNull
			| _ -> error()
		);
		"socket_send_char", Fun2 (fun s c ->
			match s, c with
			| VAbstract (ASocket s), VInt c when c >= 0 && c <= 255 ->
				ignore(Unix.send s (String.make 1 (char_of_int c)) 0 1 []);
				VNull
			| _ -> error()
		);
		"socket_send", Fun4 (fun s buf pos len ->
			match s, buf, pos, len with
			| VAbstract (ASocket s), VString buf, VInt pos, VInt len -> VInt (Unix.send s buf pos len [])
			| _ -> error()
		);
		"socket_recv", Fun4 (fun s buf pos len ->
			match s, buf, pos, len with
			| VAbstract (ASocket s), VString buf, VInt pos, VInt len -> VInt (Unix.recv s buf pos len [])
			| _ -> error()
		);
		"socket_recv_char", Fun1 (fun s ->
			match s with
			| VAbstract (ASocket s) ->
				let buf = String.make 1 '\000' in
				ignore(Unix.recv s buf 0 1 []);
				VInt (int_of_char (String.unsafe_get buf 0))
			| _ -> error()
		);
		"socket_write", Fun2 (fun s str ->
			match s, str with
			| VAbstract (ASocket s), VString str ->
				let pos = ref 0 in
				let len = ref (String.length str) in
				while !len > 0 do
					let k = Unix.send s str (!pos) (!len) [] in
					pos := !pos + k;
					len := !len - k;
				done;
				VNull
			| _ -> error()
		);
		"socket_read", Fun1 (fun s ->
			match s with
			| VAbstract (ASocket s) ->
				let tmp = String.make 1024 '\000' in
				let buf = Buffer.create 0 in
				let rec loop() =
					let k = (try Unix.recv s tmp 0 1024 [] with Unix_error _ -> 0) in
					if k > 0 then begin
						Buffer.add_substring buf tmp 0 k;
						loop();
					end
				in
				loop();
				VString (Buffer.contents buf)
			| _ -> error()
		);
		"host_resolve", Fun1 (fun s ->
			let h = (try Unix.gethostbyname (vstring s) with Not_found -> error()) in
			let addr = Unix.string_of_inet_addr h.h_addr_list.(0) in
			let a, b, c, d = Scanf.sscanf addr "%d.%d.%d.%d" (fun a b c d -> a,b,c,d) in
			VInt32 (Int32.logor (Int32.shift_left (Int32.of_int a) 24) (Int32.of_int (d lor (c lsl 8) lor (b lsl 16))))
		);
		"host_to_string", Fun1 (fun h ->
			match h with
			| VInt32 h -> VString (Unix.string_of_inet_addr (int32_addr h));
			| _ -> error()
		);
		"host_reverse", Fun1 (fun h ->
			match h with
			| VInt32 h -> VString (gethostbyaddr (int32_addr h)).h_name
			| _ -> error()
		);
		"host_local", Fun0 (fun() ->
			VString (Unix.gethostname())
		);
		"socket_connect", Fun3 (fun s h p ->
			match s, h, p with
			| VAbstract (ASocket s), VInt32 h, VInt p ->
				Unix.connect s (ADDR_INET (int32_addr h,p));
				VNull
			| _ -> error()
		);
		"socket_listen", Fun2 (fun s l ->
			match s, l with
			| VAbstract (ASocket s), VInt l ->
				Unix.listen s l;
				VNull
			| _ -> error()
		);
		"socket_set_timeout", Fun2 (fun s t ->
			match s with
			| VAbstract (ASocket s) ->
				let t = (match t with VNull -> 0. | VInt t -> float_of_int t | VFloat f -> f | _ -> error()) in
				Unix.setsockopt_float s SO_RCVTIMEO t;
				Unix.setsockopt_float s SO_SNDTIMEO t;
				VNull
			| _ -> error()
		);
		"socket_shutdown", Fun3 (fun s r w ->
			match s, r, w with
			| VAbstract (ASocket s), VBool r, VBool w ->
				Unix.shutdown s (match r, w with true, true -> SHUTDOWN_ALL | true, false -> SHUTDOWN_RECEIVE | false, true -> SHUTDOWN_SEND | _ -> error());
				VNull
			| _ -> error()
		);
		(* TODO : select, bind, accept, peer, host *)
		(* poll_alloc, poll : not planned *)
	(* system *)
		"get_env", Fun1 (fun v ->
			try VString (Unix.getenv (vstring v)) with _ -> VNull
		);
		"put_env", Fun2 (fun e v ->
			Unix.putenv (vstring e) (vstring v);
			VNull
		);
		"sys_sleep", Fun1 (fun f ->
			match f with
			| VFloat f -> ignore(Unix.select [] [] [] f); VNull
			| _ -> error()
		);
		"set_time_locale", Fun1 (fun l ->
			match l with
			| VString s -> VBool false (* always fail *)
			| _ -> error()
		);
		"get_cwd", Fun0 (fun() ->
			let dir = Unix.getcwd() in
			let l = String.length dir in
			VString (if l = 0 then "./" else match dir.[l - 1] with '/' | '\\' -> dir | _ -> dir ^ "/")
		);
		"set_cwd", Fun1 (fun s ->
			Unix.chdir (vstring s);
			VNull;
		);
		"sys_string", (
			let cached_sys_name = ref None in
			Fun0 (fun() ->
				VString (match Sys.os_type with
				| "Unix" ->
					(match !cached_sys_name with
					| Some n -> n
					| None ->
						let ic = Unix.open_process_in "uname" in
						let uname = (match input_line ic with
							| "Darwin" -> "Mac"
							| n -> n
						) in
						close_in ic;
						cached_sys_name := Some uname;
						uname)
				| "Win32" | "Cygwin" -> "Windows"
				| s -> s)
			)
		);
		"sys_is64", Fun0 (fun() ->
			VBool (Sys.word_size = 64)
		);
		"sys_command", Fun1 (fun cmd ->
			VInt (((get_ctx()).curapi.get_com()).run_command (vstring cmd))
		);
		"sys_exit", Fun1 (fun code ->
			if (get_ctx()).curapi.use_cache() then raise (Error.Fatal_error ("",Globals.null_pos));
			raise (Sys_exit(vint code));
		);
		"sys_exists", Fun1 (fun file ->
			VBool (Sys.file_exists (vstring file))
		);
		"file_delete", Fun1 (fun file ->
			Sys.remove (vstring file);
			VNull;
		);
		"sys_rename", Fun2 (fun file target ->
			Sys.rename (vstring file) (vstring target);
			VNull;
		);
		"sys_stat", Fun1 (fun file ->
			let s = Unix.stat (vstring file) in
			VObject (obj (hash_field (get_ctx())) [
				"gid", VInt s.st_gid;
				"uid", VInt s.st_uid;
				"atime", VInt32 (Int32.of_float s.st_atime);
				"mtime", VInt32 (Int32.of_float s.st_mtime);
				"ctime", VInt32 (Int32.of_float s.st_ctime);
				"dev", VInt s.st_dev;
				"ino", VInt s.st_ino;
				"nlink", VInt s.st_nlink;
				"rdev", VInt s.st_rdev;
				"size", VInt s.st_size;
				"mode", VInt s.st_perm;
			])
		);
		"sys_file_type", Fun1 (fun file ->
			VString (match (Unix.stat (vstring file)).st_kind with
			| S_REG -> "file"
			| S_DIR -> "dir"
			| S_CHR -> "char"
			| S_BLK -> "block"
			| S_LNK -> "symlink"
			| S_FIFO -> "fifo"
			| S_SOCK -> "sock")
		);
		"sys_create_dir", Fun2 (fun dir mode ->
			Unix.mkdir (vstring dir) (vint mode);
			VNull
		);
		"sys_remove_dir", Fun1 (fun dir ->
			Unix.rmdir (vstring dir);
			VNull;
		);
		"sys_time", Fun0 (fun() ->
			VFloat (Unix.gettimeofday())
		);
		"sys_cpu_time", Fun0 (fun() ->
			VFloat (Sys.time())
		);
		"sys_read_dir", Fun1 (fun dir ->
			let d = Sys.readdir (vstring dir) in
			let rec loop acc i =
				if i < 0 then
					acc
				else
					loop (VArray [|VString d.(i);acc|]) (i - 1)
			in
			loop VNull (Array.length d - 1)
		);
		"file_full_path", Fun1 (fun file ->
			VString (try Extc.get_full_path (vstring file) with _ -> error())
		);
		"sys_exe_path", Fun0 (fun() ->
			VString (Sys.argv.(0))
		);
		"sys_program_path", Fun0 (fun() ->
			let ctx = get_ctx() in
			let com = ctx.curapi.get_com() in
			match com.main_class with
			| None -> error()
			| Some p ->
				match ctx.curapi.get_type (s_type_path p) with
				| Some(TInst (c, _)) ->
					VString (Extc.get_full_path c.cl_pos.Globals.pfile)
				| _ -> error();
		);
		"sys_env", Fun0 (fun() ->
			let env = Unix.environment() in
			let rec loop acc i =
				if i < 0 then
					acc
				else
					let e, v = ExtString.String.split env.(i) "=" in
					loop (VArray [|VString e;VString v;acc|]) (i - 1)
			in
			loop VNull (Array.length env - 1)
		);
		"sys_getch", Fun1 (fun echo ->
			match echo with
			| VBool b -> VInt (Extc.getch b)
			| _ -> error()
		);
		"sys_get_pid", Fun0 (fun() ->
			VInt (Unix.getpid())
		);
	(* utf8 *)
		"utf8_buf_alloc", Fun1 (fun v ->
			VAbstract (AUtf8 (UTF8.Buf.create (vint v)))
		);
		"utf8_buf_add", Fun2 (fun b c ->
			match b with
			| VAbstract (AUtf8 buf) -> UTF8.Buf.add_char buf (UChar.chr_of_uint (vint c)); VNull
			| _ -> error()
		);
		"utf8_buf_content", Fun1 (fun b ->
			match b with
			| VAbstract (AUtf8 buf) -> VString (UTF8.Buf.contents buf);
			| _ -> error()
		);
		"utf8_buf_length", Fun1 (fun b ->
			match b with
			| VAbstract (AUtf8 buf) -> VInt (UTF8.length (UTF8.Buf.contents buf));
			| _ -> error()
		);
		"utf8_buf_size", Fun1 (fun b ->
			match b with
			| VAbstract (AUtf8 buf) -> VInt (String.length (UTF8.Buf.contents buf));
			| _ -> error()
		);
		"utf8_validate", Fun1 (fun s ->
			VBool (try UTF8.validate (vstring s); true with UTF8.Malformed_code -> false)
		);
		"utf8_length", Fun1 (fun s ->
			VInt (UTF8.length (vstring s))
		);
		"utf8_sub", Fun3 (fun s p l ->
			let buf = UTF8.Buf.create 0 in
			let pos = ref (-1) in
			let p = vint p and l = vint l in
			UTF8.iter (fun c ->
				incr pos;
				if !pos >= p && !pos < p + l then UTF8.Buf.add_char buf c;
			) (vstring s);
			VString (UTF8.Buf.contents buf)
		);
		"utf8_get", Fun2 (fun s p ->
			VInt (UChar.uint_code (try UTF8.get (vstring s) (vint p) with _ -> error()))
		);
		"utf8_iter", Fun2 (fun s f ->
			let ctx = get_ctx() in
			UTF8.iter (fun c ->
				ignore(ctx.do_call VNull f [VInt (UChar.uint_code c)] p);
			) (vstring s);
			VNull;
		);
		"utf8_compare", Fun2 (fun s1 s2 ->
			VInt (UTF8.compare (vstring s1) (vstring s2))
		);
	(* thread *)
		"thread_create", Fun2 (fun f p ->
			exc (VString "Can't create thread from within a macro");
		);
		"tls_create", Fun0 (fun() ->
			VAbstract (ATls (ref VNull))
		);
		"tls_get", Fun1 (fun t ->
			match t with
			| VAbstract (ATls r) -> !r
			| _ -> error();
		);
		"tls_set", Fun2 (fun t v ->
			match t with
			| VAbstract (ATls r) -> r := v; VNull
			| _ -> error();
		);
		(* lock, mutex, deque : not implemented *)
	(* process *)
		"process_run", (Fun2 (fun p args ->
			match p, args with
			| VString p, VArray args -> VAbstract (AProcess (Process.run p (Some (Array.map vstring args))))
			| VString p, _ -> VAbstract (AProcess (Process.run p None))
			| _ -> error()
		));
		"process_stdout_read", (Fun4 (fun p str pos len ->
			match p, str, pos, len with
			| VAbstract (AProcess p), VString str, VInt pos, VInt len -> VInt (Process.read_stdout p str pos len)
			| _ -> error()
		));
		"process_stderr_read", (Fun4 (fun p str pos len ->
			match p, str, pos, len with
			| VAbstract (AProcess p), VString str, VInt pos, VInt len -> VInt (Process.read_stderr p str pos len)
			| _ -> error()
		));
		"process_stdin_write", (Fun4 (fun p str pos len ->
			match p, str, pos, len with
			| VAbstract (AProcess p), VString str, VInt pos, VInt len -> VInt (Process.write_stdin p str pos len)
			| _ -> error()
		));
		"process_stdin_close", (Fun1 (fun p ->
			match p with
			| VAbstract (AProcess p) -> Process.close_stdin p; VNull
			| _ -> error()
		));
		"process_exit", (Fun1 (fun p ->
			match p with
			| VAbstract (AProcess p) -> VInt (Process.exit p)
			| _ -> error()
		));
		"process_pid", (Fun1 (fun p ->
			match p with
			| VAbstract (AProcess p) -> VInt (Process.pid p)
			| _ -> error()
		));
		"process_close", (Fun1 (fun vp ->
			match vp with
			| VAbstract (AProcess p) -> Process.close p; free_abstract vp; VNull
			| _ -> error()
		));
		"process_kill", (Fun1 (fun vp ->
			match vp with
			| VAbstract (AProcess p) -> Process.kill p; free_abstract vp; VNull
			| _ -> error()
		));
	(* xml *)
		"parse_xml", (match neko with
		| None -> Fun2 (fun str o ->
			match str, o with
			| VString str, VObject events ->
				let ctx = get_ctx() in
				let p = { psource = "parse_xml"; pline = 0 } in
				let xml = get_field events (hash "xml") in
				let don = get_field events (hash "done") in
				let pcdata = get_field events (hash "pcdata") in
				(*

				Since we use the Xml parser, we don't have support for
				- CDATA
				- comments, prolog, doctype (allowed but skipped)

				let cdata = get_field events (hash "cdata") in
				let comment = get_field events (hash "comment") in
				*)
				let rec loop = function
					| Xml.Element (node, attribs, children) ->
						ignore(ctx.do_call o xml [VString node;VObject (obj (hash_field ctx) (List.map (fun (a,v) -> a, VString v) attribs))] p);
						List.iter loop children;
						ignore(ctx.do_call o don [] p);
					| Xml.PCData s ->
						ignore(ctx.do_call o pcdata [VString s] p);
				in
				let x = XmlParser.make() in
				XmlParser.check_eof x false;
				loop (try
					XmlParser.parse x (XmlParser.SString str)
				with Xml.Error e -> failwith ("Parser failure (" ^ Xml.error e ^ ")")
				| e -> failwith ("Parser failure (" ^ Printexc.to_string e ^ ")"));
				VNull
			| _ -> error())
		| Some neko ->
			let parse_xml = neko.load "std@parse_xml" 2 in
			Fun2 (fun str o -> neko.call parse_xml [str;o])
		);
	(* memory, module : not planned *)
	]
	(* process *)
	@ (match neko with
	| None -> []
	| Some neko ->
		let win_ec = (try Some (neko.load "std@win_env_changed" 0) with _ -> None) in
	[
		"win_env_changed", (Fun0 (fun() -> match win_ec with None -> error() | Some f -> neko.call f []));
	]))


(* ---------------------------------------------------------------------- *)
(* REGEXP LIBRARY *)

let reg_lib =
	let error() =
		raise Builtin_error
	in
	(* try to load regexp first : we might fail if pcre is not installed *)
	let neko = (match neko with
		| None -> None
		| Some neko ->
			(try ignore(neko.load "regexp@regexp_new_options" 2); Some neko with _ -> None)
	) in
	match neko with
	| None ->
	make_library [
		(* regexp_new : deprecated *)
		"regexp_new_options", Fun2 (fun str opt ->
			match str, opt with
			| VString str, VString opt ->
				let case_sensitive = ref true in
				List.iter (function
					| 'm' -> () (* always ON ? *)
					| 'i' -> case_sensitive := false
					| c -> failwith ("Unsupported regexp option '" ^ String.make 1 c ^ "'")
				) (ExtString.String.explode opt);
				let buf = Buffer.create 0 in
				let rec loop prev esc = function
					| [] -> ()
					| c :: l when esc ->
						(match c with
						| 'n' -> Buffer.add_char buf '\n'
						| 'r' -> Buffer.add_char buf '\r'
						| 't' -> Buffer.add_char buf '\t'
						| 'd' -> Buffer.add_string buf "[0-9]"
						| '\\' -> Buffer.add_string buf "\\\\"
						| '(' | ')' -> Buffer.add_char buf c
						| '1'..'9' | '+' | '$' | '^' | '*' | '?' | '.' | '[' | ']' ->
							Buffer.add_char buf '\\';
							Buffer.add_char buf c;
						| _ -> failwith ("Unsupported escaped char '" ^ String.make 1 c ^ "'"));
						loop c false l
					| c :: l ->
						match c with
						| '\\' -> loop prev true l
						| '(' | '|' | ')' ->
							Buffer.add_char buf '\\';
							Buffer.add_char buf c;
							loop c false l
						| '?' when prev = '(' && (match l with ':' :: _ -> true | _ -> false) ->
							failwith "Non capturing groups '(?:' are not supported in macros"
						| '?' when prev = '*' ->
							failwith "Ungreedy *? are not supported in macros"
						| _ ->
							Buffer.add_char buf c;
							loop c false l
				in
				loop '\000' false (ExtString.String.explode str);
				let str = Buffer.contents buf in
				let r = {
					r = if !case_sensitive then Str.regexp str else Str.regexp_case_fold str;
					r_string = "";
					r_groups = [||];
				} in
				VAbstract (AReg r)
			| _ -> error()
		);
		"regexp_match", Fun4 (fun r str pos len ->
			match r, str, pos, len with
			| VAbstract (AReg r), VString str, VInt pos, VInt len ->
				let nstr, npos, delta = (if len = String.length str - pos then str, pos, 0 else String.sub str pos len, 0, pos) in
				(try
					ignore(Str.search_forward r.r nstr npos);
					let rec loop n =
						if n = 9 then
							[]
						else try
							(Some (Str.group_beginning n + delta, Str.group_end n + delta)) :: loop (n + 1)
						with Not_found ->
							None :: loop (n + 1)
						| Invalid_argument _ ->
							[]
					in
					r.r_string <- str;
					r.r_groups <- Array.of_list (loop 0);
					VBool true;
				with Not_found ->
					VBool false)
			| _ -> error()
		);
		"regexp_matched", Fun2 (fun r n ->
			match r, n with
			| VAbstract (AReg r), VInt n ->
				(match (try r.r_groups.(n) with _ -> failwith ("Invalid group " ^ string_of_int n)) with
				| None -> VNull
				| Some (pos,pend) -> VString (String.sub r.r_string pos (pend - pos)))
			| _ -> error()
		);
		"regexp_matched_pos", Fun2 (fun r n ->
			match r, n with
			| VAbstract (AReg r), VInt n ->
				(match (try r.r_groups.(n) with _ -> failwith ("Invalid group " ^ string_of_int n)) with
				| None -> VNull
				| Some (pos,pend) -> VObject (obj (hash_field (get_ctx())) ["pos",VInt pos;"len",VInt (pend - pos)]))
			| _ -> error()
		);
		(* regexp_replace : not used by Haxe *)
		(* regexp_replace_all : not used by Haxe *)
		(* regexp_replace_fun : not used by Haxe *)
	]
	| Some neko ->
	let regexp_new_options = neko.load "regexp@regexp_new_options" 2 in
	let regexp_match = neko.load "regexp@regexp_match" 4 in
	let regexp_matched = neko.load "regexp@regexp_matched" 2 in
	let regexp_matched_pos = neko.load "regexp@regexp_matched_pos" 2 in
	make_library [
		"regexp_new_options", Fun2 (fun str opt -> neko.call regexp_new_options [str;opt]);
		"regexp_match", Fun4 (fun r str pos len -> neko.call regexp_match [r;str;pos;len]);
		"regexp_matched", Fun2 (fun r n -> neko.call regexp_matched [r;n]);
		"regexp_matched_pos", Fun2 (fun r n -> neko.call regexp_matched_pos [r;n]);
	]


(* ---------------------------------------------------------------------- *)
(* ZLIB LIBRARY *)

let z_lib =
	let error() =
		raise Builtin_error
	in
	make_library [
		"inflate_init", Fun1 (fun f ->
			let z = Extc.zlib_inflate_init2 (match f with VNull -> 15 | VInt i -> i | _ -> error()) in
			VAbstract (AZipI { z = z; z_flush = Extc.Z_NO_FLUSH })
		);
		"deflate_init", Fun1 (fun f ->
			let z = Extc.zlib_deflate_init (match f with VInt i -> i | _ -> error()) in
			VAbstract (AZipD { z = z; z_flush = Extc.Z_NO_FLUSH })
		);
		"deflate_end", Fun1 (fun vz ->
			match vz with
			| VAbstract (AZipD z) -> Extc.zlib_deflate_end z.z; free_abstract vz; VNull;
			| _ -> error()
		);
		"inflate_end", Fun1 (fun vz ->
			match vz with
			| VAbstract (AZipI z) -> Extc.zlib_inflate_end z.z; free_abstract vz; VNull;
			| _ -> error()
		);
		"set_flush_mode", Fun2 (fun z f ->
			match z, f with
			| VAbstract (AZipI z | AZipD z), VString s ->
				z.z_flush <- (match s with
					| "NO" -> Extc.Z_NO_FLUSH
					| "SYNC" -> Extc.Z_SYNC_FLUSH
					| "FULL" -> Extc.Z_FULL_FLUSH
					| "FINISH" -> Extc.Z_FINISH
					| "BLOCK" -> Extc.Z_PARTIAL_FLUSH
					| _ -> error());
				VNull;
			| _ -> error()
		);
		"inflate_buffer", Fun5 (fun z src pos dst dpos ->
			match z, src, pos, dst, dpos with
			| VAbstract (AZipI z), VString src, VInt pos, VString dst, VInt dpos ->
				let r = Extc.zlib_inflate z.z src pos (String.length src - pos) dst dpos (String.length dst - dpos) z.z_flush in
				VObject (obj (hash_field (get_ctx())) [
					"done", VBool r.Extc.z_finish;
					"read", VInt r.Extc.z_read;
					"write", VInt r.Extc.z_wrote;
				])
			| _ -> error()
		);
		"deflate_buffer", Fun5 (fun z src pos dst dpos ->
			match z, src, pos, dst, dpos with
			| VAbstract (AZipD z), VString src, VInt pos, VString dst, VInt dpos ->
				let r = Extc.zlib_deflate z.z src pos (String.length src - pos) dst dpos (String.length dst - dpos) z.z_flush in
				VObject (obj (hash_field (get_ctx())) [
					"done", VBool r.Extc.z_finish;
					"read", VInt r.Extc.z_read;
					"write", VInt r.Extc.z_wrote;
				])
			| _ -> error()
		);
		"deflate_bound", Fun2 (fun z size ->
			match z, size with
			| VAbstract (AZipD z), VInt size -> VInt (size + 1024)
			| _ -> error()
		);
	]

(* ---------------------------------------------------------------------- *)
(* MACRO LIBRARY *)

(* convert float value to haxe expression, handling inf/-inf/nan *)
let haxe_float f p =
	let std = (Ast.EConst (Ast.Ident "std"), p) in
	let math = (Ast.EField (std, "Math"), p) in
	if (f = infinity) then
		(Ast.EField (math, "POSITIVE_INFINITY"), p)
	else if (f = neg_infinity) then
		(Ast.EField (math, "NEGATIVE_INFINITY"), p)
	else if (f <> f) then
		(Ast.EField (math, "NaN"), p)
	else
		(Ast.EConst (Ast.Float (float_repres f)), p)

let macro_lib =
	let error() =
		raise Builtin_error
	in
	let ccom() =
		(get_ctx()).curapi.get_com()
	in
	make_library [
		"curpos", Fun0 (fun() -> VAbstract (APos (get_ctx()).curapi.pos));
		"error", Fun2 (fun msg p ->
			match msg, p with
			| VString s, VAbstract (APos p) ->
				(ccom()).Common.error s p;
				raise Abort
			| _ -> error()
		);
		"fatal_error", Fun2 (fun msg p ->
			match msg, p with
			| VString s, VAbstract (APos p) ->
				raise (Error.Fatal_error (s,p))
			| _ -> error()
		);
		"warning", Fun2 (fun msg p ->
			match msg, p with
			| VString s, VAbstract (APos p) ->
				(ccom()).warning s p;
				VNull;
			| _ -> error()
		);
		"class_path", Fun0 (fun() ->
			VArray (Array.of_list (List.map (fun s -> VString s) (ccom()).class_path));
		);
		"resolve", Fun1 (fun file ->
			match file with
			| VString s -> VString (try Common.find_file (ccom()) s with Not_found -> failwith ("File not found '" ^ s ^ "'"))
			| _ -> error();
		);
		"define", Fun1 (fun s ->
			match s with
			| VString s -> Common.raw_define (ccom()) s; VNull
			| _ -> error();
		);
		"defined", Fun1 (fun s ->
			match s with
			| VString s -> VBool (Common.raw_defined (ccom()) s)
			| _ -> error();
		);
		"defined_value", Fun1 (fun s ->
			match s with
			| VString s -> (try VString (Common.raw_defined_value (ccom()) s) with Not_found -> VNull)
			| _ -> error();
		);
		"get_defines", Fun0 (fun() ->
			let defines = (ccom()).defines in
			let h = Hashtbl.create 0 in
			PMap.iter (fun n v -> Hashtbl.replace h (VString n) (VString v)) defines;
			enc_hash h
		);
		"get_type", Fun1 (fun s ->
			match s with
			| VString s ->
				(match (get_ctx()).curapi.get_type s with
				| None -> failwith ("Type not found '" ^ s ^ "'")
				| Some t -> encode_type t)
			| _ -> error()
		);
		"get_module", Fun1 (fun s ->
			match s with
			| VString s ->
				enc_array (List.map encode_type ((get_ctx()).curapi.get_module s))
			| _ -> error()
		);
		"after_typing", Fun1 (fun f ->
			match f with
			| VFunction (Fun1 _) | VClosure _ ->
				let ctx = get_ctx() in
				ctx.curapi.after_typing (fun tl ->
					ignore(catch_errors ctx (fun() -> ctx.do_call VNull f [enc_array (List.map !encode_module_type_ref tl)] null_pos));
				);
				VNull
			| _ -> error()
		);
		"on_generate", Fun1 (fun f ->
			match f with
			| VFunction (Fun1 _) | VClosure _ ->
				let ctx = get_ctx() in
				ctx.curapi.on_generate (fun tl ->
					ignore(catch_errors ctx (fun() -> ctx.do_call VNull f [enc_array (List.map encode_type tl)] null_pos));
				);
				VNull
			| _ -> error()
		);
		"after_generate", Fun1 (fun f ->
			match f with
			| VFunction (Fun0 _) ->
				let ctx = get_ctx() in
				ctx.curapi.after_generate (fun () ->
					ignore(catch_errors ctx (fun() -> ctx.do_call VNull f [] null_pos));
				);
				VNull
			| _ -> error()
		);
		"on_type_not_found", Fun1 (fun f ->
			match f with
			| VFunction (Fun1 _) ->
				let ctx = get_ctx() in
				ctx.curapi.on_type_not_found (fun path ->
					match catch_errors ctx (fun () -> ctx.do_call VNull f [enc_string path] null_pos) with
					| Some v -> v
					| None -> VNull
				);
				VNull
			| _ -> error()
		);
		"parse", Fun3 (fun s p b ->
			match s, p, b with
			| VString s, VAbstract (APos p), VBool b when s <> "" ->
				(try encode_expr ((get_ctx()).curapi.parse_string s p b) with Invalid_expr -> error())
			| _ -> error()
		);
		"make_expr", Fun2 (fun v p ->
			match p with
			| VAbstract (APos p) ->
				let h_enum = hash "__enum__" and h_et = hash "__et__" and h_ct = hash "__ct__" in
				let h_tag = hash "tag" and h_args = hash "args" in
				let h_length = hash "length" in
				let ctx = get_ctx() in
				let error v = failwith ("Unsupported value " ^ ctx.do_string v) in
				let make_path t =
					let rec loop = function
						| [] -> assert false
						| [name] -> (Ast.EConst (Ast.Ident name),p)
						| name :: l -> (Ast.EField (loop l,name),p)
					in
					let t = t_infos t in
					loop (List.rev (if t.mt_module.m_path = t.mt_path then fst t.mt_path @ [snd t.mt_path] else fst t.mt_module.m_path @ [snd t.mt_module.m_path;snd t.mt_path]))
				in
				let rec loop = function
					| VNull -> (Ast.EConst (Ast.Ident "null"),p)
					| VBool b -> (Ast.EConst (Ast.Ident (if b then "true" else "false")),p)
					| VInt i -> (Ast.EConst (Ast.Int (string_of_int i)),p)
					| VInt32 i -> (Ast.EConst (Ast.Int (Int32.to_string i)),p)
					| VFloat f -> haxe_float f p
					| VAbstract (APos p) ->
						(Ast.EObjectDecl (
							("fileName" , (Ast.EConst (Ast.String p.Globals.pfile) , p)) ::
							("lineNumber" , (Ast.EConst (Ast.Int (string_of_int (Lexer.get_error_line p))),p)) ::
							("className" , (Ast.EConst (Ast.String ("")),p)) ::
							[]
						), p)
					| VString _ | VArray _ | VAbstract _ | VFunction _ | VClosure _ as v -> error v
					| VObject o as v ->
						match o.oproto with
						| None ->
							(match get_field_opt o h_ct with
							| Some (VAbstract (ATDecl t)) ->
								make_path t
							| _ ->
								let fields = List.fold_left (fun acc (fid,v) -> (field_name ctx fid, loop v) :: acc) [] (Array.to_list o.ofields) in
								(Ast.EObjectDecl fields, p))
						| Some proto ->
							match get_field_opt proto h_enum, get_field_opt o h_a, get_field_opt o h_s, get_field_opt o h_length with
							| _, Some (VArray a), _, Some (VInt len) ->
								(Ast.EArrayDecl (List.map loop (Array.to_list (Array.sub a 0 len))),p)
							| _, _, Some (VString s), _ ->
								(Ast.EConst (Ast.String s),p)
							| Some (VObject en), _, _, _ ->
								(match get_field en h_et, get_field o h_tag with
								| VAbstract (ATDecl t), VString tag ->
									let e = (Ast.EField (make_path t,tag),p) in
									(match get_field_opt o h_args with
									| Some (VArray args) ->
										let args = List.map loop (Array.to_list args) in
										(Ast.ECall (e,args),p)
									| _ -> e)
								| _ ->
									error v)
							| _ ->
								error v
				in
				encode_expr (loop v)
			| _ -> error()
		);
		"signature", Fun1 (fun v ->
			let cache = ref [] in
			let cache_count = ref 0 in
			let hfiles = Hashtbl.create 0 in
			let get_file f =
				try
					Hashtbl.find hfiles f
				with Not_found ->
					let ff = Path.unique_full_path f in
					Hashtbl.add hfiles f ff;
					ff
			in
			let do_cache (v:value) (v2:value) =
				(*
					tricky : we need to have a quick not-linear cache based on objects address
					but we can't use address since the GC might be triggered here.
					Instead let's mutate the object temporary.
				*)
				let vt = Obj.repr v in
				let old = Obj.tag vt in
				let old_val = Obj.field vt 0 in
				let abstract_tag = 7 in
				Obj.set_tag vt abstract_tag;
				Obj.set_field vt 0 (Obj.repr (ACacheRef v2));
				cache := (vt,old,old_val) :: !cache;
				incr cache_count
			in
			let rec loop v =
				match v with
				| VNull | VBool _ | VInt _ | VFloat _ | VString _ | VInt32 _ -> v
				| VObject o ->
					let o2 = { ofields = [||]; oproto = None } in
					let v2 = VObject o2 in
					do_cache v v2;
					Array.iter (fun (f,v) -> if f <> h_class then set_field o2 f (loop v)) o.ofields;
					(match o.oproto with
					| None -> ()
					| Some p -> (match loop (VObject p) with VObject p2 -> o2.oproto <- Some p2 | _ -> assert false));
					v2
				| VArray a ->
					let a2 = Array.create (Array.length a) VNull in
					let v2 = VArray a2 in
					do_cache v v2;
					for i = 0 to Array.length a - 1 do
						a2.(i) <- loop a.(i);
					done;
					v2
				| VFunction f ->
					let v2 = VFunction (Obj.magic !cache_count) in
					do_cache v v2;
					v2
				| VClosure (vl,f) ->
					let rl = ref [] in
					let v2 = VClosure (Obj.magic rl, Obj.magic !cache_count) in
					(* in ocaml 4.0+ it was reported some stack overflow, related to vl being GC'ed or mutated in do_cache.
					   let's make sure to have a real pointer to it first. The fix will trigger an alloc which might have simply moved the problem away *)
					let vl = VNull :: vl in
					do_cache v v2;
					rl := List.map loop vl;
					v2
				| VAbstract (APos p) -> VAbstract (APos { p with Globals.pfile = get_file p.Globals.pfile })
				| VAbstract (ACacheRef v) -> v
				| VAbstract (AHash h) ->
					let h2 = Hashtbl.create 0 in
					let v2 = VAbstract (AHash h2) in
					do_cache v v2;
					Hashtbl.iter (fun k v -> Hashtbl.add h2 k (loop v)) h2;
					v2
				| VAbstract _ ->
					let v2 = VAbstract (Obj.magic !cache_count) in
					do_cache v v2;
					v2
			in
			let v = loop v in
			(* restore *)
			List.iter (fun (vt,tag,field) ->
				Obj.set_tag vt tag;
				Obj.set_field vt 0 field;
			) !cache;
			VString (Digest.to_hex (Digest.string (Marshal.to_string v [Marshal.Closures])))
		);
		"to_complex", Fun1 (fun v ->
			try	encode_complex_type (TExprToExpr.convert_type' (decode_type v))
			with Exit -> VNull
		);
		"unify", Fun2 (fun t1 t2 ->
			let e1 = mk (TObjectDecl []) (decode_type t1) Globals.null_pos in
			try ignore(((get_ctx()).curapi.cast_or_unify) (decode_type t2) e1 Globals.null_pos); VBool true
			with Error.Error (Error.Unify _,_) -> VBool false
		);
		"typeof", Fun1 (fun v ->
			encode_type ((get_ctx()).curapi.type_expr (decode_expr v)).etype
		);
		"type_expr", Fun1 (fun v ->
			encode_texpr ((get_ctx()).curapi.type_expr (decode_expr v))
		);
		"resolve_type", Fun2 (fun t p ->
			encode_type ((get_ctx()).curapi.resolve_type (fst (decode_complex_type t)) (decode_pos p));
		);
		"s_type", Fun1 (fun v ->
			VString (Type.s_type (print_context()) (decode_type v))
		);
		"s_expr", Fun2 (fun v b ->
			let f = match b with VBool true -> Type.s_expr_pretty false "" false | _ -> Type.s_expr_ast true "" in
			VString (f (Type.s_type (print_context())) (decode_texpr v))
		);
		"is_fmt_string", Fun1 (fun v ->
			match v with
			| VAbstract (APos p) -> VBool(Lexer.is_fmt_string p)
			| _ -> VNull
		);
		"format_string", Fun2 (fun s p ->
			match s,p with
			| VString(s),VAbstract(APos p) -> encode_expr ((get_ctx()).curapi.format_string s p)
			| _ -> VNull
		);
		"allow_package", Fun1 (fun v ->
			match v with
			| VString s ->
				(get_ctx()).curapi.allow_package s;
				VNull
			| _ -> error());
		"type_patch", Fun4 (fun t f s v ->
			let p = (get_ctx()).curapi.type_patch in
			(match t, f, s, v with
			| VString t, VString f, VBool s, VString v -> p t f s (Some v)
			| VString t, VString f, VBool s, VNull -> p t f s None
			| _ -> error());
			VNull
		);
		"meta_patch", Fun4 (fun m t f s ->
			let p = (get_ctx()).curapi.meta_patch in
			(match m, t, f, s with
			| VString m, VString t, VString f, VBool s -> p m t (Some f) s
			| VString m, VString t, VNull, VBool s -> p m t None s
			| _ -> error());
			VNull
		);
		"add_global_metadata", Fun5 (fun v1 v2 v3 v4 v5 ->
			match v1,v2,v3,v4,v5 with
				| VString s1,VString s2,VBool b1,VBool b2,VBool b3 ->
					(get_ctx()).curapi.add_global_metadata s1 s2 (b1,b2,b3);
					VNull
				| _ ->
					error()
		);
		"custom_js", Fun1 (fun f ->
			match f with
			| VFunction (Fun1 _) ->
				let ctx = get_ctx() in
				ctx.curapi.set_js_generator (fun api ->
					ignore(catch_errors ctx (fun() -> ctx.do_call VNull f [api] null_pos));
				);
				VNull
			| _ -> error()
		);
		"get_pos_infos", Fun1 (fun p ->
			match p with
			| VAbstract (APos p) -> VObject (obj (hash_field (get_ctx())) ["min",VInt p.Globals.pmin;"max",VInt p.Globals.pmax;"file",VString p.Globals.pfile])
			| _ -> error()
		);
		"make_pos", Fun3 (fun min max file ->
			match min, max, file with
			| VInt min, VInt max, VString file -> VAbstract (APos { Globals.pmin = min; Globals.pmax = max; Globals.pfile = file })
			| _ -> error()
		);
		"add_resource", Fun2 (fun name data ->
			match name, data with
			| VString name, VString data ->
				Hashtbl.replace (ccom()).resources name data;
				if name = "" then failwith "Empty resource name";
				let m = if name.[0] = '$' then (get_ctx()).curapi.current_macro_module() else (get_ctx()).curapi.current_module() in
				m.m_extra.m_binded_res <- PMap.add name data m.m_extra.m_binded_res;
				VNull
			| _ -> error()
		);
		"get_resources", Fun0 (fun() ->
			let res = (ccom()).resources in
			let h = Hashtbl.create 0 in
			Hashtbl.iter (fun n v -> Hashtbl.replace h (VString n) (VString v)) res;
			enc_hash h
		);
		"local_module", Fun0 (fun() ->
			let m = (get_ctx()).curapi.current_module() in
			VString (s_type_path m.m_path);
		);
		"local_type", Fun0 (fun() ->
			match (get_ctx()).curapi.get_local_type() with
			| None -> VNull
			| Some t -> encode_type t
		);
		"expected_type", Fun0 (fun() ->
			match (get_ctx()).curapi.get_expected_type() with
			| None -> VNull
			| Some t -> encode_type t
		);
		"call_arguments", Fun0 (fun() ->
			match (get_ctx()).curapi.get_call_arguments() with
			| None -> VNull
			| Some el -> enc_array (List.map encode_expr el)
		);
		"local_method", Fun0 (fun() ->
			VString ((get_ctx()).curapi.get_local_method())
		);
		"local_using", Fun0 (fun() ->
			enc_array (List.map encode_clref ((get_ctx()).curapi.get_local_using()))
		);
		"local_imports", Fun0 (fun() ->
			enc_array (List.map encode_import ((get_ctx()).curapi.get_local_imports()))
		);
		"local_vars", Fun1 (fun as_var ->
			let as_var = match as_var with
				| VNull | VBool false -> false
				| VBool true -> true
				| _ -> error()
			in
			let vars = (get_ctx()).curapi.get_local_vars() in
			let h = Hashtbl.create 0 in
			if as_var then
				PMap.iter (fun n v -> Hashtbl.replace h (VString n) (encode_tvar v)) vars
			else
				PMap.iter (fun n v -> Hashtbl.replace h (VString n) (encode_type v.v_type)) vars;
			enc_hash h
		);
		"follow_with_abstracts", Fun2 (fun v once ->
			let t = decode_type v in
			let follow_once t =
				match t with
				| TMono r ->
					(match !r with
					| None -> t
					| Some t -> t)
				| TAbstract (a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
					Abstract.get_underlying_type a tl
				| TAbstract _ | TEnum _ | TInst _ | TFun _ | TAnon _ | TDynamic _ ->
					t
				| TType (t,tl) ->
					apply_params t.t_params tl t.t_type
				| TLazy f ->
					(!f)()
			in
			encode_type (match once with VNull | VBool false -> Abstract.follow_with_abstracts t | VBool true -> follow_once t | _ -> error())
		);
		"follow", Fun2 (fun v once ->
			let t = decode_type v in
			let follow_once t =
				match t with
				| TMono r ->
					(match !r with
					| None -> t
					| Some t -> t)
				| TAbstract _ | TEnum _ | TInst _ | TFun _ | TAnon _ | TDynamic _ ->
					t
				| TType (t,tl) ->
					apply_params t.t_params tl t.t_type
				| TLazy f ->
					(!f)()
			in
			encode_type (match once with VNull | VBool false -> follow t | VBool true -> follow_once t | _ -> error())
		);
		"build_fields", Fun0 (fun() ->
			(get_ctx()).curapi.get_build_fields()
		);
		"define_type", Fun1 (fun v ->
			(get_ctx()).curapi.define_type v;
			VNull
		);
		"define_module", Fun4 (fun p v i u ->
			match p, v, i, u with
			| VString path, VArray vl, VArray ui, VArray ul ->
				(get_ctx()).curapi.define_module path (Array.to_list vl) (List.map decode_import (Array.to_list ui)) (List.map fst (List.map decode_path (Array.to_list ul)));
				VNull
			| _ ->
				error()
		);
		"add_class_path", Fun1 (fun v ->
			match v with
			| VString cp ->
				let com = ccom() in
				let cp = Path.add_trailing_slash cp in
				com.class_path <- cp :: com.class_path;
				(match com.get_macros() with
					| Some(mcom) ->
						mcom.class_path <- cp :: com.class_path;
					| None ->
						());
				Hashtbl.clear com.file_lookup_cache;
				VNull
			| _ ->
				error()
		);
		"add_native_lib", Fun1 (fun v ->
			match v with
			| VString file ->
				let com = ccom() in
				(match com.platform with
				| Flash -> Genswf.add_swf_lib com file false
				| Java -> Genjava.add_java_lib com file false
				| Cs ->
					let file, is_std = match ExtString.String.nsplit file "@" with
						| [file] ->
							file,false
						| [file;"std"] ->
							file,true
						| _ -> failwith ("unsupported file@`std` format: " ^ file)
					in
					Gencs.add_net_lib com file is_std
				| _ -> failwith "Unsupported platform");
				VNull
			| _ ->
				error()
		);
		"add_native_arg", Fun1 (fun v ->
			match v with
			| VString arg ->
				let com = ccom() in
				(match com.platform with
				| Java | Cs | Cpp ->
					com.c_args <- arg :: com.c_args
				| _ -> failwith "Unsupported platform");
				VNull
			| _ ->
				error()
		);
		"module_dependency", Fun2 (fun m file ->
			match m, file with
			| VString m, VString file ->
				(get_ctx()).curapi.module_dependency m file false;
				VNull
			| _ -> error()
		);
		"module_reuse_call", Fun2 (fun m mcall ->
			match m, mcall with
			| VString m, VString mcall ->
				(get_ctx()).curapi.module_dependency m mcall true;
				VNull
			| _ -> error()
		);
		"get_typed_expr", Fun1 (fun e ->
			let e = decode_texpr e in
			encode_expr (TExprToExpr.convert_expr e)
		);
		"store_typed_expr", Fun1 (fun e ->
			let e = try decode_texpr e with Invalid_expr -> error() in
			encode_expr ((get_ctx()).curapi.store_typed_expr e)
		);
		"get_output", Fun0 (fun() ->
			VString (ccom()).file
		);
		"set_output", Fun1 (fun s ->
			match s with
			| VString s -> (ccom()).file <- s; VNull
			| _ -> error()
		);
		"get_display_pos", Fun0 (fun() ->
			let p = !Parser.resume_display in
			if p = Globals.null_pos then
				VNull
			else
				VObject (obj (hash_field (get_ctx())) ["file",VString p.Globals.pfile;"pos",VInt p.Globals.pmin])
		);
		"pattern_locals", Fun2 (fun e t ->
			let loc = (get_ctx()).curapi.get_pattern_locals (decode_expr e) (decode_type t) in
			let h = Hashtbl.create 0 in
			PMap.iter (fun n (v,_) -> Hashtbl.replace h (VString n) (encode_type v.v_type)) loc;
			enc_hash h
		);
		"macro_context_reused", Fun1 (fun c ->
			match c with
			| VFunction (Fun0 _) ->
				let ctx = get_ctx() in
				ctx.on_reused <- (fun() -> catch_errors ctx (fun() -> ctx.do_call VNull c [] null_pos) = Some (VBool true)) :: ctx.on_reused;
				VNull
			| _ -> error()
		);
		"apply_params", Fun3 (fun tpl tl t ->
			let tpl = List.map (fun v ->
				match v with
				| VObject o ->
					let name = match get_field o (hash "name") with VString s -> s | _ -> assert false in
					let t = decode_type (get_field o (hash "t")) in
					name,t
				| _ -> assert false
			) (dec_array tpl) in
			let tl = List.map decode_type (dec_array tl) in
			let rec map t = match t with
				| TInst({cl_kind = KTypeParameter _},_) ->
					begin try
						(* use non-physical equality check here to make apply_params work *)
						snd (List.find (fun (_,t2) -> type_iseq t t2) tpl)
					with Not_found ->
						Type.map map t
					end
				| _ -> Type.map map t
			in
			encode_type (apply_params tpl tl (map (decode_type t)))
		);
		"eval", Fun1 (fun v ->
			let e = decode_expr v in
			let e = ((get_ctx()).curapi.type_macro_expr e) in
 			match !eval_expr_ref (get_ctx()) e with
			| Some v -> v
			| None -> VNull
		);
		"include_file", Fun2 (fun file position ->
			match file, position with
			| VString file, VString position ->
				let file = if Sys.file_exists file then
					file
				else try Common.find_file (ccom()) file with
					| Not_found ->
						failwith ("unable to find file for inclusion: " ^ file)
				in
				(ccom()).include_files <- (file, position) :: (ccom()).include_files;
				VNull
			| _ ->
				error()
		);
		(* Compilation server *)
		"server_add_module_check_policy", Fun4 (fun filter policy recursive context_options ->
			match filter,policy,recursive,context_options with
			| VArray vl1, VArray vl2, VBool b, VInt i ->
				let sl = Array.fold_left (fun acc v -> match v with VString s -> (s :: acc) | _ -> error()) [] vl1 in
				let il = Array.fold_left (fun acc v -> match v with VInt i -> (i :: acc) | _ -> error()) [] vl2 in
				(get_ctx()).curapi.add_module_check_policy sl il b i;
				VNull
			| _ ->
				error()
		);
		"server_invalidate_files", Fun1 (fun a -> match a with
			| VArray vl ->
				let cs = match CompilationServer.get() with Some cs -> cs | None -> failwith "compilation server not running" in
				Array.iter (fun v -> match v with
					| VString s ->
						let s = Path.unique_full_path s in
						CompilationServer.taint_modules cs s;
						CompilationServer.remove_files cs s;
					| _ ->
						error()
				) vl;
				VNull
			| _ ->
				error()
		);
	]

(* ---------------------------------------------------------------------- *)
(* EVAL *)

let throw ctx p msg =
	ctx.callstack <- { cpos = p; cthis = ctx.vthis; cstack = DynArray.length ctx.stack; cenv = ctx.venv } :: ctx.callstack;
	exc (VString msg)

let declare ctx var =
	ctx.locals_map <- PMap.add var ctx.locals_count ctx.locals_map;
	ctx.locals_count <- ctx.locals_count + 1

let save_locals ctx =
	let old, oldcount = ctx.locals_map, ctx.locals_count in
	(fun() ->
		let n = ctx.locals_count - oldcount in
		ctx.locals_count <- oldcount;
		ctx.locals_map <- old;
		n;
	)

let get_ident ctx s =
	try
		let index = PMap.find s ctx.locals_map in
		if index >= ctx.locals_barrier then
			AccLocal (ctx.locals_count - index)
		else (try
			AccEnv (DynArray.index_of (fun s2 -> s = s2) ctx.locals_env)
		with Not_found ->
			let index = DynArray.length ctx.locals_env in
			DynArray.add ctx.locals_env s;
			AccEnv index
		)
	with Not_found -> try
		AccGlobal (PMap.find s ctx.globals)
	with Not_found ->
		let g = ref VNull in
		ctx.globals <- PMap.add s g ctx.globals;
		AccGlobal g

let no_env = [||]

let rec eval_expr ctx e =
	let e = Genneko.gen_expr ctx.gen e in
	catch_errors ctx (fun() -> (eval ctx e)())

and eval ctx (e,p) =
	match e with
	| EConst c ->
		(match c with
		| True -> (fun() -> VBool true)
		| False -> (fun() -> VBool false)
		| Null -> (fun() -> VNull)
		| This -> (fun() -> ctx.vthis)
		| Int i -> (fun() -> VInt i)
		| Int32 i -> (fun() -> VInt32 i)
		| Float f ->
			let f = float_of_string f in
			(fun() -> VFloat f)
		| String s -> (fun() -> VString s)
		| Builtin "loader" ->
			(fun() -> ctx.loader)
		| Builtin "exports" ->
			(fun() -> ctx.exports)
		| Builtin s ->
			let b = (try Hashtbl.find builtins s with Not_found -> throw ctx p ("Builtin not found '" ^ s ^ "'")) in
			(fun() -> b)
		| Ident s ->
			acc_get ctx p (get_ident ctx s))
	| EBlock el ->
		let old = save_locals ctx in
		let el = List.map (eval ctx) el in
		let n = old() in
		let rec loop = function
			| [] -> VNull
			| [e] -> e()
			| e :: l ->
				ignore(e());
				loop l
		in
		(fun() ->
			let v = loop el in
			pop ctx n;
			v)
	| EParenthesis e ->
		eval ctx e
	| EField (e,f) ->
		let e = eval ctx e in
		let h = hash_field ctx f in
		(fun() ->
			match e() with
			| VObject o -> get_field o h
			| _ -> throw ctx p ("Invalid field access : " ^ f)
		)
	| ECall ((EConst (Builtin "mk_pos"),_),[(ECall (_,[EConst (String file),_]),_);(EConst (Int min),_);(EConst (Int max),_)]) ->
		let pos = VAbstract (APos { Globals.pfile = file; Globals.pmin = min; Globals.pmax = max }) in
		(fun() -> pos)
	| ECall ((EConst (Builtin "typewrap"),_),[t]) ->
		(fun() -> VAbstract (ATDecl (Obj.magic t)))
	| ECall ((EConst (Builtin "delay_call"),_),[EConst (Int index),_]) ->
		let f = ctx.curapi.delayed_macro index in
		let fbuild = ref None in
		let old = { ctx with gen = ctx.gen } in
		let compile_delayed_call() =
			let oldl, oldc, oldb, olde = ctx.locals_map, ctx.locals_count, ctx.locals_barrier, ctx.locals_env in
			ctx.locals_map <- old.locals_map;
			ctx.locals_count <- old.locals_count;
			ctx.locals_barrier <- old.locals_barrier;
			ctx.locals_env <- DynArray.copy old.locals_env;
			let save = save_locals ctx in
			let e = f() in
			let n = save() in
			let e = if DynArray.length ctx.locals_env = DynArray.length old.locals_env then
				e
			else
				let n = DynArray.get ctx.locals_env (DynArray.length ctx.locals_env - 1) in
				(fun() -> exc (VString ("Macro-in-macro call can't access to closure variable '" ^ n ^ "'")))
			in
			ctx.locals_map <- oldl;
			ctx.locals_count <- oldc;
			ctx.locals_barrier <- oldb;
			ctx.locals_env <- olde;
			(fun() ->
				let v = e() in
				pop ctx n;
				v
			)
		in
		(fun() ->
			let e = (match !fbuild with
			| Some e -> e
			| None ->
				let e = compile_delayed_call() in
				fbuild := Some e;
				e
			) in
			e())
	| ECall (e,el) ->
		let el = List.map (eval ctx) el in
		(match fst e with
		| EField (e,f) ->
			let e = eval ctx e in
			let h = hash_field ctx f in
			(fun() ->
				let pl = List.map (fun f -> f()) el in
				let o = e() in
				let f = (match o with
				| VObject o -> get_field o h
				| _ -> throw ctx p ("Invalid field access : " ^ f)
				) in
				call ctx o f pl p
			)
		| _ ->
			let e = eval ctx e in
			(fun() ->
				let pl = List.map (fun f -> f()) el in
				call ctx ctx.vthis (e()) pl p
			))
	| EArray (e1,e2) ->
		let e1 = eval ctx e1 in
		let e2 = eval ctx e2 in
		let acc = AccArray (e1,e2) in
		acc_get ctx p acc
	| EVars vl ->
		let vl = List.map (fun (v,eo) ->
			let eo = (match eo with None -> (fun() -> VNull) | Some e -> eval ctx e) in
			declare ctx v;
			eo
		) vl in
		(fun() ->
			List.iter (fun e -> push ctx (e())) vl;
			VNull
		)
	| EWhile (econd,e,NormalWhile) ->
		let econd = eval ctx econd in
		let e = eval ctx e in
		let rec loop st =
			match econd() with
			| VBool true ->
				let v = (try
					ignore(e()); None
				with
					| Continue -> pop ctx (DynArray.length ctx.stack - st); None
					| Break v -> pop ctx (DynArray.length ctx.stack - st); Some v
				) in
				(match v with
				| None -> loop st
				| Some v -> v)
			| _ ->
				VNull
		in
		(fun() -> try loop (DynArray.length ctx.stack) with Sys.Break -> throw ctx p "Ctrl+C")
	| EWhile (econd,e,DoWhile) ->
		let e = eval ctx e in
		let econd = eval ctx econd in
		let rec loop st =
			let v = (try
				ignore(e()); None
			with
				| Continue -> pop ctx (DynArray.length ctx.stack - st); None
				| Break v -> pop ctx (DynArray.length ctx.stack - st); Some v
			) in
			match v with
			| Some v -> v
			| None ->
				match econd() with
				| VBool true -> loop st
				| _ -> VNull
		in
		(fun() -> loop (DynArray.length ctx.stack))
	| EIf (econd,eif,eelse) ->
		let econd = eval ctx econd in
		let eif = eval ctx eif in
		let eelse = (match eelse with None -> (fun() -> VNull) | Some e -> eval ctx e) in
		(fun() ->
			match econd() with
			| VBool true -> eif()
			| _ -> eelse()
		)
	| ETry (e,exc,ecatch) ->
		let old = save_locals ctx in
		let e = eval ctx e in
		let n1 = old() in
		declare ctx exc;
		let ecatch = eval ctx ecatch in
		let n2 = old() in
		(fun() ->
			let vthis = ctx.vthis in
			let venv = ctx.venv in
			let stack = ctx.callstack in
			let csize = ctx.callsize in
			let size = DynArray.length ctx.stack in
			try
				pop_ret ctx e n1
			with Runtime v ->
				let rec loop n l =
					if n = 0 then List.map (fun s -> s.cpos) l else
					match l with
					| [] -> []
					| _ :: l -> loop (n - 1) l
				in
				ctx.exc <- loop (List.length stack) (List.rev ctx.callstack);
				ctx.callstack <- stack;
				ctx.callsize <- csize;
				ctx.vthis <- vthis;
				ctx.venv <- venv;
				pop ctx (DynArray.length ctx.stack - size);
				push ctx v;
				pop_ret ctx ecatch n2
			)
	| EFunction (pl,e) ->
		let old = save_locals ctx in
		let oldb, oldenv = ctx.locals_barrier, ctx.locals_env in
		ctx.locals_barrier <- ctx.locals_count;
		ctx.locals_env <- DynArray.create();
		List.iter (declare ctx) pl;
		let e = eval ctx e in
		ignore(old());
		let env = ctx.locals_env in
		ctx.locals_barrier <- oldb;
		ctx.locals_env <- oldenv;
		let env = DynArray.to_array (DynArray.map (fun s ->
			acc_get ctx p (get_ident ctx s)) env
		) in
		let init_env = if Array.length env = 0 then
			(fun() -> no_env)
		else
			(fun() -> Array.map (fun e -> e()) env)
		in
		(match pl with
		| [] ->
			(fun() ->
				let env = init_env() in
				VFunction (Fun0 (fun() ->
					ctx.venv <- env;
					e())))
		| [a] ->
			(fun() ->
				let env = init_env() in
				VFunction (Fun1 (fun v ->
					ctx.venv <- env;
					push ctx v;
					e();
				)))
		| [a;b] ->
			(fun() ->
				let env = init_env() in
				VFunction (Fun2 (fun va vb ->
					ctx.venv <- env;
					push ctx va;
					push ctx vb;
					e();
				)))
		| [a;b;c] ->
			(fun() ->
				let env = init_env() in
				VFunction (Fun3 (fun va vb vc ->
					ctx.venv <- env;
					push ctx va;
					push ctx vb;
					push ctx vc;
					e();
				)))
		| [a;b;c;d] ->
			(fun() ->
				let env = init_env() in
				VFunction (Fun4 (fun va vb vc vd ->
					ctx.venv <- env;
					push ctx va;
					push ctx vb;
					push ctx vc;
					push ctx vd;
					e();
				)))
		| [a;b;c;d;pe] ->
			(fun() ->
				let env = init_env() in
				VFunction (Fun5 (fun va vb vc vd ve ->
					ctx.venv <- env;
					push ctx va;
					push ctx vb;
					push ctx vc;
					push ctx vd;
					push ctx ve;
					e();
				)))
		| _ ->
			(fun() ->
				let env = init_env() in
				VFunction (FunVar (fun vl ->
					if List.length vl != List.length pl then exc (VString "Invalid call");
					ctx.venv <- env;
					List.iter (push ctx) vl;
					e();
				)))
		)
	| EBinop (op,e1,e2) ->
		eval_op ctx op e1 e2 p
	| EReturn None ->
		(fun() -> raise (Return VNull))
	| EReturn (Some e) ->
		let e = eval ctx e in
		(fun() -> raise (Return (e())))
	| EBreak None ->
		(fun() -> raise (Break VNull))
	| EBreak (Some e) ->
		let e = eval ctx e in
		(fun() -> raise (Break (e())))
	| EContinue ->
		(fun() -> raise Continue)
	| ENext (e1,e2) ->
		let e1 = eval ctx e1 in
		let e2 = eval ctx e2 in
		(fun() -> ignore(e1()); e2())
	| EObject fl ->
		let fl = List.map (fun (f,e) -> hash_field ctx f, eval ctx e) fl in
		let fields = Array.of_list (List.map (fun (f,_) -> f,VNull) fl) in
		Array.sort (fun (f1,_) (f2,_) -> compare f1 f2) fields;
		(fun() ->
			let o = {
				ofields = Array.copy fields;
				oproto = None;
			} in
			List.iter (fun (f,e) -> set_field o f (e())) fl;
			VObject o
		)
	| ELabel l ->
		assert false
	| ESwitch (e1,el,eo) ->
		let e1 = eval ctx e1 in
		let el = List.map (fun (cond,e) -> cond, eval ctx cond, eval ctx e) el in
		let eo = (match eo with None -> (fun() -> VNull) | Some e -> eval ctx e) in
		let cases = (try
			let max = ref (-1) in
			let ints = List.map (fun (cond,_,e) ->
				match fst cond with
				| EConst (Int i) -> if i < 0 then raise Exit; if i > !max then max := i; i, e
				| _ -> raise Exit
			) el in
			let a = Array.create (!max + 1) eo in
			List.iter (fun (i,e) -> a.(i) <- e) (List.rev ints);
			Some a;
		with
			Exit -> None
		) in
		let def v =
			let rec loop = function
				| [] -> eo()
				| (_,c,e) :: l ->
					if ctx.do_compare v (c()) = CEq then e() else loop l
			in
			loop el
		in
		(match cases with
		| None -> (fun() -> def (e1()))
		| Some t ->
			(fun() ->
				match e1() with
				| VInt i -> if i >= 0 && i < Array.length t then t.(i)() else eo()
				| v -> def v
			))
	| ENeko _ ->
		throw ctx p "Inline neko code unsupported"

and eval_oop ctx p o field (params:value list) =
	match get_field_opt o field with
	| None -> None
	| Some f -> Some (call ctx (VObject o) f params p)

and eval_access ctx (e,p) =
	match e with
	| EField (e,f) ->
		let v = eval ctx e in
		AccField (v,f)
	| EArray (e,eindex) ->
		let v = eval ctx e in
		let idx = eval ctx eindex in
		AccArray (v,idx)
	| EConst (Ident s) ->
		get_ident ctx s
	| EConst This ->
		AccThis
	| _ ->
		throw ctx p "Invalid assign"

and eval_access_get_set ctx (e,p) =
	match e with
	| EField (e,f) ->
		let v = eval ctx e in
		let cache = ref VNull in
		AccField ((fun() -> cache := v(); !cache),f), AccField((fun() -> !cache), f)
	| EArray (e,eindex) ->
		let v = eval ctx e in
		let idx = eval ctx eindex in
		let vcache = ref VNull and icache = ref VNull in
		AccArray ((fun() -> vcache := v(); !vcache),(fun() -> icache := idx(); !icache)), AccArray ((fun() -> !vcache),(fun() -> !icache))
	| EConst (Ident s) ->
		let acc = get_ident ctx s in
		acc, acc
	| EConst This ->
		AccThis, AccThis
	| _ ->
		throw ctx p "Invalid assign"

and acc_get ctx p = function
	| AccField (v,f) ->
		let h = hash_field ctx f in
		(fun() ->
			match v() with
			| VObject o -> get_field o h
			| _ -> throw ctx p ("Invalid field access : " ^ f))
	| AccArray (e,index) ->
		(fun() ->
			let e = e() in
			let index = index() in
			(match index, e with
			| VInt i, VArray a -> (try Array.get a i with _ -> VNull)
			| VInt32 _, VArray _ -> VNull
			| _, VObject o ->
				(match eval_oop ctx p o h_get [index] with
				| None -> throw ctx p "Invalid array access"
				| Some v -> v)
			| _ -> throw ctx p "Invalid array access"))
	| AccLocal i ->
		(fun() -> DynArray.get ctx.stack (DynArray.length ctx.stack - i))
	| AccGlobal g ->
		(fun() -> !g)
	| AccThis ->
		(fun() -> ctx.vthis)
	| AccEnv i ->
		(fun() -> ctx.venv.(i))

and acc_set ctx p acc value =
	match acc with
	| AccField (v,f) ->
		let h = hash_field ctx f in
		(fun() ->
			let v = v() in
			let value = value() in
			match v with
			| VObject o -> set_field o h value; value
			| _ -> throw ctx p ("Invalid field access : " ^ f))
	| AccArray (e,index) ->
		(fun() ->
			let e = e() in
			let index = index() in
			let value = value() in
			(match index, e with
			| VInt i, VArray a -> (try Array.set a i value; value with _ -> value)
			| VInt32 _, VArray _ -> value
			| _, VObject o ->
				(match eval_oop ctx p o h_set [index;value] with
				| None -> throw ctx p "Invalid array access"
				| Some _ -> value);
			| _ -> throw ctx p "Invalid array access"))
	| AccLocal i ->
		(fun() ->
			let value = value() in
			DynArray.set ctx.stack (DynArray.length ctx.stack - i) value;
			value)
	| AccGlobal g ->
		(fun() ->
			let value = value() in
			g := value;
			value)
	| AccThis ->
		(fun() ->
			let value = value() in
			ctx.vthis <- value;
			value)
	| AccEnv i ->
		(fun() ->
			let value = value() in
			ctx.venv.(i) <- value;
			value)

and number_op ctx p sop iop fop oop rop v1 v2 =
	(fun() ->
		let v1 = v1() in
		let v2 = v2() in
		exc_number_op ctx p sop iop fop oop rop v1 v2)

and exc_number_op ctx p sop iop fop oop rop v1 v2 =
	match v1, v2 with
	| VInt a, VInt b -> best_int (iop (Int32.of_int a) (Int32.of_int b))
	| VInt32 a, VInt b -> best_int (iop a (Int32.of_int b))
	| VInt a, VInt32 b -> best_int (iop (Int32.of_int a) b)
	| VFloat a, VInt b -> VFloat (fop a (float_of_int b))
	| VFloat a, VInt32 b -> VFloat (fop a (Int32.to_float b))
	| VInt a, VFloat b -> VFloat (fop (float_of_int a) b)
	| VInt32 a, VFloat b -> VFloat (fop (Int32.to_float a) b)
	| VFloat a, VFloat b -> VFloat (fop a b)
	| VInt32 a, VInt32 b -> best_int (iop a b)
	| VObject o, _ ->
		(match eval_oop ctx p o oop [v2] with
		| Some v -> v
		| None ->
			match v2 with
			| VObject o ->
				(match eval_oop ctx p o rop [v1] with
				| Some v -> v
				| None -> throw ctx p sop)
			| _ ->
				throw ctx p sop)
	| _ , VObject o ->
		(match eval_oop ctx p o rop [v1] with
		| Some v -> v
		| None -> throw ctx p sop)
	| _ ->
		throw ctx p sop

and int_op ctx p op iop v1 v2 =
	(fun() ->
		let v1 = v1() in
		let v2 = v2() in
		match v1, v2 with
		| VInt a, VInt b -> best_int (iop (Int32.of_int a) (Int32.of_int b))
		| VInt32 a, VInt b -> best_int (iop a (Int32.of_int b))
		| VInt a, VInt32 b -> best_int (iop (Int32.of_int a) b)
		| VInt32 a, VInt32 b -> best_int (iop a b)
		| _ -> throw ctx p op)

and base_op ctx op v1 v2 p =
	match op with
	| "+" ->
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match v1, v2 with
			| (VInt _ | VInt32 _), (VInt _ | VInt32 _) | (VInt _ | VInt32 _), VFloat _ | VFloat _ , (VInt _ | VInt32 _) | VFloat _ , VFloat _ | VObject _ , _ | _ , VObject _ -> exc_number_op ctx p op Int32.add (+.) h_add h_radd v1 v2
			| VString a, _ -> VString (a ^ ctx.do_string v2)
			| _, VString b -> VString (ctx.do_string v1 ^ b)
			| _ -> throw ctx p op)
	| "-" ->
		number_op ctx p op Int32.sub (-.) h_sub h_rsub v1 v2
	| "*" ->
		number_op ctx p op Int32.mul ( *. ) h_mult h_rmult v1 v2
	| "/" ->
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match v1, v2 with
			| VInt i, VInt j -> VFloat ((float_of_int i) /. (float_of_int j))
			| VInt i, VInt32 j -> VFloat ((float_of_int i) /. (Int32.to_float j))
			| VInt32 i, VInt j -> VFloat ((Int32.to_float i) /. (float_of_int j))
			| VInt32 i, VInt32 j -> VFloat ((Int32.to_float i) /. (Int32.to_float j))
			| _ -> exc_number_op ctx p op Int32.div (/.) h_div h_rdiv v1 v2)
	| "%" ->
		number_op ctx p op (fun x y -> if y = 0l then throw ctx p op; Int32.rem x y) mod_float h_mod h_rmod v1 v2
	| "&" ->
		int_op ctx p op Int32.logand v1 v2
	| "|" ->
		int_op ctx p op Int32.logor v1 v2
	| "^" ->
		int_op ctx p op Int32.logxor v1 v2
	| "<<" ->
		int_op ctx p op (fun x y -> Int32.shift_left x (Int32.to_int y)) v1 v2
	| ">>" ->
		int_op ctx p op (fun x y -> Int32.shift_right x (Int32.to_int y)) v1 v2
	| ">>>" ->
		int_op ctx p op (fun x y -> Int32.shift_right_logical x (Int32.to_int y)) v1 v2
	| _ ->
		throw ctx p op

and eval_op ctx op e1 e2 p =
	match op with
	| "=" ->
		let acc = eval_access ctx e1 in
		let v = eval ctx e2 in
		acc_set ctx p acc v
	| "==" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match ctx.do_compare v1 v2 with
			| CEq -> VBool true
			| _ -> VBool false)
	| "!=" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match ctx.do_compare v1 v2 with
			| CEq -> VBool false
			| _ -> VBool true)
	| ">" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match ctx.do_compare v1 v2 with
			| CSup -> VBool true
			| _ -> VBool false)
	| ">=" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match ctx.do_compare v1 v2 with
			| CSup | CEq -> VBool true
			| _ -> VBool false)
	| "<" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match ctx.do_compare v1 v2 with
			| CInf -> VBool true
			| _ -> VBool false)
	| "<=" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(fun() ->
			let v1 = v1() in
			let v2 = v2() in
			match ctx.do_compare v1 v2 with
			| CInf | CEq -> VBool true
			| _ -> VBool false)
	| "+" | "-" | "*" | "/" | "%" | "|" | "&" | "^" | "<<" | ">>" | ">>>" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		base_op ctx op v1 v2 p
	| "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | ">>>=" | "|=" | "&=" | "^=" ->
		let aset, aget = eval_access_get_set ctx e1 in
		let v1 = acc_get ctx p aget in
		let v2 = eval ctx e2 in
		let v = base_op ctx (String.sub op 0 (String.length op - 1)) v1 v2 p in
		acc_set ctx p aset v
	| "&&" ->
		let e1 = eval ctx e1 in
		let e2 = eval ctx e2 in
		(fun() ->
			match e1() with
			| VBool true -> e2()
			| _ -> VBool false)
	| "||" ->
		let e1 = eval ctx e1 in
		let e2 = eval ctx e2 in
		(fun() ->
			match e1() with
			| VBool true as v -> v
			| _ -> e2())
	| "++=" | "--=" ->
		let aset, aget = eval_access_get_set ctx e1 in
		let v1 = acc_get ctx p aget in
		let v2 = eval ctx e2 in
		let vcache = ref VNull in
		let v = base_op ctx (String.sub op 0 1) (fun() -> vcache := v1(); !vcache) v2 p in
		let set = acc_set ctx p aset v in
		(fun() -> ignore(set()); !vcache)
	| _ ->
		throw ctx p ("Unsupported " ^ op)

and call ctx vthis vfun pl p =
	let oldthis = ctx.vthis in
	let stackpos = DynArray.length ctx.stack in
	let oldstack = ctx.callstack in
	let oldsize = ctx.callsize in
	let oldenv = ctx.venv in
	ctx.vthis <- vthis;
	ctx.callstack <- { cpos = p; cthis = oldthis; cstack = stackpos; cenv = oldenv } :: ctx.callstack;
	ctx.callsize <- oldsize + 1;
	if oldsize > 600 then exc (VString "Stack overflow");
	let ret = (try
		(match vfun with
		| VClosure (vl,f) ->
			f vl pl
		| VFunction f ->
			(match pl, f with
			| [], Fun0 f -> f()
			| [a], Fun1 f -> f a
			| [a;b], Fun2 f -> f a b
			| [a;b;c], Fun3 f -> f a b c
			| [a;b;c;d], Fun4 f -> f a b c d
			| [a;b;c;d;e], Fun5 f -> f a b c d e
			| _, FunVar f -> f pl
			| _ -> exc (VString (Printf.sprintf "Invalid call (%d args instead of %d)" (List.length pl) (nargs f))))
		| VAbstract (ALazyType f) ->
			encode_type ((!f)())
		| _ ->
			exc (VString "Invalid call"))
	with Return v -> v
		| Stack_overflow -> exc (VString "Compiler Stack overflow")
		| Sys_error msg | Failure msg -> exc (VString msg)
		| Unix.Unix_error (_,cmd,msg) -> exc (VString ("Error " ^ cmd ^ " " ^ msg))
		| Invalid_expr -> exc (VString "Invalid input value")
		| Builtin_error | Invalid_argument _ -> exc (VString "Invalid call")) in
	ctx.vthis <- oldthis;
	ctx.venv <- oldenv;
	ctx.callstack <- oldstack;
	ctx.callsize <- oldsize;
	pop ctx (DynArray.length ctx.stack - stackpos);
	ret

(* ---------------------------------------------------------------------- *)
(* OTHERS *)

let rec to_string ctx n v =
	if n > 5 then
		"<...>"
	else let n = n + 1 in
	match v with
	| VNull -> "null"
	| VBool true -> "true"
	| VBool false -> "false"
	| VInt i -> string_of_int i
	| VInt32 i -> Int32.to_string i
	| VFloat f ->
		let s = float_repres f in
		let len = String.length s in
		if String.unsafe_get s (len - 1) = '.' then String.sub s 0 (len - 1) else s
	| VString s -> s
	| VArray vl -> "[" ^ String.concat "," (Array.to_list (Array.map (to_string ctx n) vl)) ^ "]"
	| VAbstract a ->
		(match a with
		| APos p -> "#pos(" ^ Lexer.get_error_pos (Printf.sprintf "%s:%d:") p ^ ")"
		| _ -> "#abstract")
	| VFunction f -> "#function:"  ^ string_of_int (nargs f)
	| VClosure _ -> "#function:-1"
	| VObject o ->
		match eval_oop ctx null_pos o h_string [] with
		| Some (VString s) -> s
		| _ ->
			let b = Buffer.create 0 in
			let first = ref true in
			Buffer.add_char b '{';
			Array.iter (fun (f,v) ->
				if !first then begin
					Buffer.add_char b ' ';
					first := false;
				end else
					Buffer.add_string b ", ";
				Buffer.add_string b (field_name ctx f);
				Buffer.add_string b " => ";
				Buffer.add_string b (to_string ctx n v);
			) o.ofields;
			Buffer.add_string b (if !first then "}" else " }");
			Buffer.contents b

let rec compare ctx a b =
	let fcmp (a:float) b = if a = b then CEq else if a < b then CInf else if a > b then CSup else CUndef in
	let scmp (a:string) b = if a = b then CEq else if a < b then CInf else CSup in
	let icmp (a:int32) b = let l = Int32.compare a b in if l = 0 then CEq else if l < 0 then CInf else CSup in
	match a, b with
	| VNull, VNull -> CEq
	| VInt a, VInt b -> if a = b then CEq else if a < b then CInf else CSup
	| VInt32 a, VInt32 b -> icmp a b
	| VInt a, VInt32 b -> icmp (Int32.of_int a) b
	| VInt32 a, VInt b -> icmp a (Int32.of_int b)
	| VFloat a, VFloat b -> fcmp a b
	| VFloat a, VInt b -> fcmp a (float_of_int b)
	| VFloat a, VInt32 b -> fcmp a (Int32.to_float b)
	| VInt a, VFloat b -> fcmp (float_of_int a) b
	| VInt32 a, VFloat b -> fcmp (Int32.to_float a) b
	| VBool a, VBool b -> if a = b then CEq else if a then CSup else CInf
	| VString a, VString b -> scmp a b
	| VInt _ , VString s
	| VInt32 _, VString s
	| VFloat _ , VString s
	| VBool _ , VString s -> scmp (to_string ctx 0 a) s
	| VString s, VInt _
	| VString s, VInt32 _
	| VString s, VFloat _
	| VString s, VBool _ -> scmp s (to_string ctx 0 b)
	| VObject oa, VObject ob ->
		if oa == ob then CEq else
			(match eval_oop ctx null_pos oa h_compare [b] with
			| Some (VInt i) -> if i = 0 then CEq else if i < 0 then CInf else CSup
			| _ -> CUndef)
	| VAbstract a, VAbstract b ->
		if a == b then CEq else CUndef
	| VArray a, VArray b ->
		if a == b then CEq else CUndef
	| VFunction a, VFunction b ->
		if a == b then CEq else CUndef
	| VClosure (la,fa), VClosure (lb,fb) ->
		if la == lb && fa == fb then CEq else CUndef
	| _ ->
		CUndef

let select ctx =
	get_ctx_ref := (fun() -> ctx)

let value_match_failure s expected actual =
	let sl = String.concat ", " in
	let slv l = sl (List.map s_value_kind l) in
	Printf.sprintf "%s (expected [%s], got [%s])" s (sl expected) (slv actual)

let load_prim ctx f n =
	match f, n with
	| VString f, VInt n ->
		let lib, fname = (try ExtString.String.split f "@" with _ -> "", f) in
		(try
			let f = (match lib with
			| "std" -> Hashtbl.find std_lib fname
			| "macro" -> Hashtbl.find macro_lib fname
			| "regexp" -> Hashtbl.find reg_lib fname
			| "zlib" -> Hashtbl.find z_lib fname
			| _ -> failwith ("You cannot use the library '" ^ lib ^ "' inside a macro");
			) in
			if nargs f <> n then raise Not_found;
			VFunction f
		with Not_found ->
			VFunction (FunVar (fun _ -> exc (VString ("Primitive not found " ^ f ^ ":" ^ string_of_int n)))))
	| _ ->
		exc (VString (value_match_failure "Invalid call" ["VString";"VInt"] [f;n]))

let create com api =
	let loader = obj hash [
		"args",VArray (Array.of_list (List.map (fun s -> VString s) com.sys_args));
		"loadprim",VFunction (Fun2 (fun a b -> (get_ctx()).do_loadprim a b));
		"loadmodule",VFunction (Fun2 (fun a b -> assert false));
	] in
	let ctx = {
		gen = Genneko.new_context com 2 true;
		types = Hashtbl.create 0;
		error = false;
		error_proto = { ofields = [||]; oproto = None };
		prototypes = Hashtbl.create 0;
		enums = [||];
		(* eval *)
		locals_map = PMap.empty;
		locals_count = 0;
		locals_barrier = 0;
		locals_env = DynArray.create();
		globals = PMap.empty;
		(* runtime *)
		callstack = [];
		callsize = 0;
		stack = DynArray.create();
		exc = [];
		vthis = VNull;
		venv = [||];
		fields_cache = Hashtbl.copy constants;
		(* api *)
		do_call = Obj.magic();
		do_string = Obj.magic();
		do_loadprim = Obj.magic();
		do_compare = Obj.magic();
		(* context *)
		curapi = api;
		loader = VObject loader;
		on_reused = [];
		is_reused = true;
		exports = VObject { ofields = [||]; oproto = None };
	} in
	ctx.do_call <- call ctx;
	ctx.do_string <- to_string ctx 0;
	ctx.do_loadprim <- load_prim ctx;
	ctx.do_compare <- compare ctx;
	select ctx;
	List.iter (fun e -> ignore((eval ctx e)())) (Genneko.header());
	ctx

let clear ctx com =
	Hashtbl.clear ctx.types;
	Hashtbl.clear ctx.prototypes;
	ctx.gen <- Genneko.new_context com 2 true;
	ctx.locals_map <- PMap.empty;
	ctx.locals_count <- 0;
	ctx.locals_barrier <- 0;
	ctx.locals_env <- DynArray.create();
	ctx.globals <- PMap.empty;
	ctx.callstack <- [];
	ctx.callsize <- 0;
	ctx.stack <- DynArray.create();
	ctx.exc <- [];
	ctx.vthis <- VNull;
	ctx.venv <- [||];
	select ctx;
	List.iter (fun e -> ignore((eval ctx e)())) (Genneko.header())

let do_reuse ctx api =
	ctx.is_reused <- false;
	ctx.curapi <- api

let can_reuse ctx types =
	let has_old_version t =
		let inf = Type.t_infos t in
		try
			Hashtbl.find ctx.types inf.mt_path <> inf.mt_module.m_id
		with Not_found ->
			false
	in
	if List.exists has_old_version types then
		false
	else if ctx.is_reused then
		true
	else if not (List.for_all (fun f -> f()) ctx.on_reused) then
		false
	else begin
		ctx.is_reused <- true;
		true;
	end

let add_types ctx types ready =
	let types = List.filter (fun t -> match t with
		| TAbstractDecl a when not (Meta.has Meta.CoreType a.a_meta) ->
			(* A @:native on an abstract causes the implementation class and the abstract
			   to have the same path. Let's skip all abstracts so this doesn't matter. *)
			false
		| _ ->
			let path = Type.t_path t in
			if Hashtbl.mem ctx.types path then false else begin
				Hashtbl.add ctx.types path (Type.t_infos t).mt_module.m_id;
				true;
			end
	) types in
	List.iter ready types;
	let e = (EBlock (Genneko.build ctx.gen types), null_pos) in
	ignore(catch_errors ctx (fun() -> ignore((eval ctx e)())))

let get_path ctx path p =
	let rec loop = function
		| [] -> assert false
		| [x] -> (EConst (Ident x),p)
		| x :: l -> (EField (loop l,x),p)
	in
	(eval ctx (loop (List.rev path)))()

let set_error ctx e =
	ctx.error <- e

let call_path ctx path f vl api =
	if ctx.error then
		None
	else let old = ctx.curapi in
	ctx.curapi <- api;
	let p = Genneko.pos ctx.gen api.pos in
	catch_errors ctx ~final:(fun() -> ctx.curapi <- old) (fun() ->
		match get_path ctx path p with
		| VObject o ->
			let f = get_field o (hash f) in
			call ctx (VObject o) f vl p
		| v ->
			print_endline (value_match_failure ("Unexpected value for " ^ (String.concat "." path)) ["VObject"] [v]);
			assert false
	)

(* ---------------------------------------------------------------------- *)
(* EXPR ENCODING *)

type enum_index =
	| IExpr
	| IBinop
	| IUnop
	| IConst
	| ITParam
	| ICType
	| IField
	| IType
	| IFieldKind
	| IMethodKind
	| IVarAccess
	| IAccess
	| IClassKind
	| ITypedExpr
	| ITConstant
	| IModuleType
	| IFieldAccess
	| IAnonStatus
	| IImportMode

let enum_name = function
	| IExpr -> "ExprDef"
	| IBinop -> "Binop"
	| IUnop -> "Unop"
	| IConst -> "Constant"
	| ITParam -> "TypeParam"
	| ICType -> "ComplexType"
	| IField -> "FieldType"
	| IType -> "Type"
	| IFieldKind -> "FieldKind"
	| IMethodKind -> "MethodKind"
	| IVarAccess -> "VarAccess"
	| IAccess -> "Access"
	| IClassKind -> "ClassKind"
	| ITypedExpr -> "TypedExprDef"
	| ITConstant -> "TConstant"
	| IModuleType -> "ModuleType"
	| IFieldAccess -> "FieldAccess"
	| IAnonStatus -> "AnonStatus"
	| IImportMode -> "ImportMode"

let init ctx =
	let enums = [IExpr;IBinop;IUnop;IConst;ITParam;ICType;IField;IType;IFieldKind;IMethodKind;IVarAccess;IAccess;IClassKind;ITypedExpr;ITConstant;IModuleType;IFieldAccess;IAnonStatus;IImportMode] in
	let get_enum_proto e =
		match get_path ctx ["haxe";"macro";enum_name e] null_pos with
		| VObject e ->
			(match get_field e h_constructs with
			| VObject cst ->
				(match get_field cst h_a with
				| VArray a ->
					Array.map (fun s ->
						match s with
						| VObject s -> (match get_field s h_s with VString s -> get_field e (hash s),s | _ -> assert false)
						| _ -> assert false
					) a
				| _ -> assert false)
			| _ -> assert false)
		| _ -> failwith ("haxe.macro." ^ enum_name e ^ " does not exists")
	in
	ctx.enums <- Array.of_list (List.map get_enum_proto enums);
	ctx.error_proto <- (match get_path ctx ["haxe";"macro";"Error";"prototype"] null_pos with VObject p -> p | _ -> failwith ("haxe.macro.Error does not exists"))

open Ast

let null f = function
	| None -> VNull
	| Some v -> f v

let encode_pos p =
	VAbstract (APos p)

let enc_inst path fields =
	let ctx = get_ctx() in
	let p = (try Hashtbl.find ctx.prototypes path with Not_found -> try
		(match get_path ctx (path@["prototype"]) Nast.null_pos with
		| VObject o -> Hashtbl.add ctx.prototypes path o; o
		| _ -> raise (Runtime VNull))
	with Runtime _ ->
		failwith ("Prototype not found " ^ String.concat "." path)
	) in
	let o = obj hash fields in
	o.oproto <- Some p;
	VObject o

let enc_array l =
	let a = Array.of_list l in
	enc_inst ["Array"] [
		"__a", VArray a;
		"length", VInt (Array.length a);
	]

let enc_string s =
	enc_inst ["String"] [
		"__s", VString s;
		"length", VInt (String.length s)
	]

let enc_hash h =
	enc_inst ["haxe";"ds";"StringMap"] [
		"h", VAbstract (AHash h);
	]

let enc_obj l = VObject (obj hash l)

let enc_enum ?(pos=None) (i:enum_index) index pl =
	let eindex : int = Obj.magic i in
	let edef = (get_ctx()).enums.(eindex) in
	if pl = [] then
		fst edef.(index)
	else
		enc_inst ["haxe";"macro";enum_name i] (
			("tag", VString (snd edef.(index))) ::
			("index", VInt index) ::
			("args", VArray (Array.of_list pl)) ::
			(match pos with None -> [] | Some p -> ["pos", encode_pos p])
		)

let compiler_error msg pos =
	exc (enc_inst ["haxe";"macro";"Error"] [("message",enc_string msg);("pos",encode_pos pos)])

let encode_const c =
	let tag, pl = match c with
	| Int s -> 0, [enc_string s]
	| Float s -> 1, [enc_string s]
	| String s -> 2, [enc_string s]
	| Ident s -> 3, [enc_string s]
	| Regexp (s,opt) -> 4, [enc_string s;enc_string opt]
	in
	enc_enum IConst tag pl

let rec encode_binop op =
	let tag, pl = match op with
	| OpAdd -> 0, []
	| OpMult -> 1, []
	| OpDiv -> 2, []
	| OpSub -> 3, []
	| OpAssign -> 4, []
	| OpEq -> 5, []
	| OpNotEq -> 6, []
	| OpGt -> 7, []
	| OpGte -> 8, []
	| OpLt -> 9, []
	| OpLte -> 10, []
	| OpAnd -> 11, []
	| OpOr -> 12, []
	| OpXor -> 13, []
	| OpBoolAnd -> 14, []
	| OpBoolOr -> 15, []
	| OpShl -> 16, []
	| OpShr -> 17, []
	| OpUShr -> 18, []
	| OpMod -> 19, []
	| OpAssignOp op -> 20, [encode_binop op]
	| OpInterval -> 21, []
	| OpArrow -> 22, []
	in
	enc_enum IBinop tag pl

let encode_unop op =
	let tag = match op with
	| Increment -> 0
	| Decrement -> 1
	| Not -> 2
	| Neg -> 3
	| NegBits -> 4
	in
	enc_enum IUnop tag []

let encode_import (path,mode) =
	let tag,pl = match mode with
		| INormal -> 0, []
		| IAsName s -> 1, [enc_string s]
		| IAll -> 2,[]
	in
	let mode = enc_enum IImportMode tag pl in
	enc_obj [
		"path", enc_array (List.map (fun (name,p) -> enc_obj [ "pos", encode_pos p; "name", enc_string name]) path);
		"mode", mode
	]

let encode_placed_name (s,p) =
	enc_string s

let rec encode_path (t,_) =
	let fields = [
		"pack", enc_array (List.map enc_string t.tpackage);
		"name", enc_string t.tname;
		"params", enc_array (List.map encode_tparam t.tparams);
	] in
	enc_obj (match t.tsub with
		| None ->  fields
		| Some s -> ("sub", enc_string s) :: fields)

and encode_tparam = function
	| TPType t -> enc_enum ITParam 0 [encode_ctype t]
	| TPExpr e -> enc_enum ITParam 1 [encode_expr e]

and encode_access a =
	let tag = match a with
		| APublic -> 0
		| APrivate -> 1
		| AStatic -> 2
		| AOverride -> 3
		| ADynamic -> 4
		| AInline -> 5
		| AMacro -> 6
	in
	enc_enum IAccess tag []

and encode_meta_entry (m,ml,p) =
	enc_obj [
		"name", enc_string (Meta.to_string m);
		"params", enc_array (List.map encode_expr ml);
		"pos", encode_pos p;
	]

and encode_meta_content m =
	enc_array (List.map encode_meta_entry m)

and encode_field (f:class_field) =
	let tag, pl = match f.cff_kind with
		| FVar (t,e) -> 0, [null encode_ctype t; null encode_expr e]
		| FFun f -> 1, [encode_fun f]
		| FProp (get,set, t, e) -> 2, [encode_placed_name get; encode_placed_name set; null encode_ctype t; null encode_expr e]
	in
	enc_obj [
		"name",encode_placed_name f.cff_name;
		"name_pos", encode_pos (pos f.cff_name);
		"doc", null enc_string f.cff_doc;
		"pos", encode_pos f.cff_pos;
		"kind", enc_enum IField tag pl;
		"meta", encode_meta_content f.cff_meta;
		"access", enc_array (List.map encode_access f.cff_access);
	]

and encode_ctype t =
	let tag, pl = match fst t with
	| CTPath p ->
		0, [encode_path (p,Globals.null_pos)]
	| CTFunction (pl,r) ->
		1, [enc_array (List.map encode_ctype pl);encode_ctype r]
	| CTAnonymous fl ->
		2, [enc_array (List.map encode_field fl)]
	| CTParent t ->
		3, [encode_ctype t]
	| CTExtend (tl,fields) ->
		4, [enc_array (List.map encode_path tl); enc_array (List.map encode_field fields)]
	| CTOptional t ->
		5, [encode_ctype t]
	in
	enc_enum ~pos:(Some (pos t)) ICType tag pl

and encode_tparam_decl tp =
	enc_obj [
		"name", encode_placed_name tp.tp_name;
		"name_pos", encode_pos (pos tp.tp_name);
		"params", enc_array (List.map encode_tparam_decl tp.tp_params);
		"constraints", enc_array (List.map encode_ctype tp.tp_constraints);
		"meta", encode_meta_content tp.tp_meta;
	]

and encode_fun f =
	enc_obj [
		"params", enc_array (List.map encode_tparam_decl f.f_params);
		"args", enc_array (List.map (fun (n,opt,m,t,e) ->
			enc_obj [
				"name", encode_placed_name n;
				"name_pos", encode_pos (pos n);
				"opt", VBool opt;
				"meta", encode_meta_content m;
				"type", null encode_ctype t;
				"value", null encode_expr e;
			]
		) f.f_args);
		"ret", null encode_ctype f.f_type;
		"expr", null encode_expr f.f_expr
	]

and encode_expr e =
	let rec loop (e,p) =
		let tag, pl = match e with
			| EConst c ->
				0, [encode_const c]
			| EArray (e1,e2) ->
				1, [loop e1;loop e2]
			| EBinop (op,e1,e2) ->
				2, [encode_binop op;loop e1;loop e2]
			| EField (e,f) ->
				3, [loop e;enc_string f]
			| EParenthesis e ->
				4, [loop e]
			| EObjectDecl fl ->
				5, [enc_array (List.map (fun (f,e) -> enc_obj [
					"field",enc_string f;
					"expr",loop e;
				]) fl)]
			| EArrayDecl el ->
				6, [enc_array (List.map loop el)]
			| ECall (e,el) ->
				7, [loop e;enc_array (List.map loop el)]
			| ENew (p,el) ->
				8, [encode_path p; enc_array (List.map loop el)]
			| EUnop (op,flag,e) ->
				9, [encode_unop op; VBool (match flag with Prefix -> false | Postfix -> true); loop e]
			| EVars vl ->
				10, [enc_array (List.map (fun (v,t,eo) ->
					enc_obj [
						"name",encode_placed_name v;
						"name_pos",encode_pos (pos v);
						"type",null encode_ctype t;
						"expr",null loop eo;
					]
				) vl)]
			| EFunction (name,f) ->
				11, [null enc_string name; encode_fun f]
			| EBlock el ->
				12, [enc_array (List.map loop el)]
			| EFor (e,eloop) ->
				13, [loop e;loop eloop]
			| EIn (e1,e2) ->
				14, [loop e1;loop e2]
			| EIf (econd,e,eelse) ->
				15, [loop econd;loop e;null loop eelse]
			| EWhile (econd,e,flag) ->
				16, [loop econd;loop e;VBool (match flag with NormalWhile -> true | DoWhile -> false)]
			| ESwitch (e,cases,eopt) ->
				17, [loop e;enc_array (List.map (fun (ecl,eg,e,p) ->
					enc_obj [
						"values",enc_array (List.map loop ecl);
						"guard",null loop eg;
						"expr",null loop e;
						"pos",encode_pos p;
					]
				) cases);null (fun (e,_) -> encode_null_expr e) eopt]
			| ETry (e,catches) ->
				18, [loop e;enc_array (List.map (fun (v,t,e,p) ->
					enc_obj [
						"name",encode_placed_name v;
						"name_pos",encode_pos (pos v);
						"type",encode_ctype t;
						"expr",loop e;
						"pos",encode_pos p
					]
				) catches)]
			| EReturn eo ->
				19, [null loop eo]
			| EBreak ->
				20, []
			| EContinue ->
				21, []
			| EUntyped e ->
				22, [loop e]
			| EThrow e ->
				23, [loop e]
			| ECast (e,t) ->
				24, [loop e; null encode_ctype t]
			| EDisplay (e,flag) ->
				25, [loop e; VBool flag]
			| EDisplayNew t ->
				26, [encode_path t]
			| ETernary (econd,e1,e2) ->
				27, [loop econd;loop e1;loop e2]
			| ECheckType (e,t) ->
				28, [loop e; encode_ctype t]
			| EMeta (m,e) ->
				29, [encode_meta_entry m;loop e]
		in
		enc_obj [
			"pos", encode_pos p;
			"expr", enc_enum IExpr tag pl;
		]
	in
	loop e

and encode_null_expr e =
	match e with
	| None ->
		enc_obj ["pos", VNull;"expr",VNull]
	| Some e ->
		encode_expr e

(* ---------------------------------------------------------------------- *)
(* EXPR DECODING *)

let opt f v =
	match v with
	| VNull -> None
	| _ -> Some (f v)

let opt_list f v =
	match v with
	| VNull -> []
	| _ -> f v

let decode_pos = function
	| VAbstract (APos p) -> p
	| _ -> raise Invalid_expr

let field v f =
	match v with
	| VObject o -> get_field o (hash f)
	| _ -> raise Invalid_expr

let decode_enum v =
	match field v "index", field v "args" with
	| VInt i, VNull -> i, []
	| VInt i, VArray a -> i, Array.to_list a
	| _ -> raise Invalid_expr

let decode_enum_with_pos v =
	(match field v "index", field v "args" with
	| VInt i, VNull -> i, []
	| VInt i, VArray a -> i, Array.to_list a
	| _ -> raise Invalid_expr),(match field v "pos" with
		| VAbstract(APos p) -> p
		| _ -> Globals.null_pos) (* Can happen from reification and other sources. *)

let dec_bool = function
	| VBool b -> b
	| _ -> raise Invalid_expr

let dec_string v =
	match field v "__s" with
	| VString s -> s
	| _ -> raise Invalid_expr

let dec_array v =
	match field v "__a", field v "length" with
	| VArray a, VInt l -> Array.to_list (if Array.length a = l then a else Array.sub a 0 l)
	| _ -> raise Invalid_expr

let decode_const c =
	match decode_enum c with
	| 0, [s] -> Int (dec_string s)
	| 1, [s] -> Float (dec_string s)
	| 2, [s] -> String (dec_string s)
	| 3, [s] -> Ident (dec_string s)
	| 4, [s;opt] -> Regexp (dec_string s, dec_string opt)
	| 5, [s] -> Ident (dec_string s) (** deprecated CType, keep until 3.0 release **)
	| _ -> raise Invalid_expr

let rec decode_op op =
	match decode_enum op with
	| 0, [] -> OpAdd
	| 1, [] -> OpMult
	| 2, [] -> OpDiv
	| 3, [] -> OpSub
	| 4, [] -> OpAssign
	| 5, [] -> OpEq
	| 6, [] -> OpNotEq
	| 7, [] -> OpGt
	| 8, [] -> OpGte
	| 9, [] -> OpLt
	| 10, [] -> OpLte
	| 11, [] -> OpAnd
	| 12, [] -> OpOr
	| 13, [] -> OpXor
	| 14, [] -> OpBoolAnd
	| 15, [] -> OpBoolOr
	| 16, [] -> OpShl
	| 17, [] -> OpShr
	| 18, [] -> OpUShr
	| 19, [] -> OpMod
	| 20, [op] -> OpAssignOp (decode_op op)
	| 21, [] -> OpInterval
	| 22,[] -> OpArrow
	| _ -> raise Invalid_expr

let decode_unop op =
	match decode_enum op with
	| 0, [] -> Increment
	| 1, [] -> Decrement
	| 2, [] -> Not
	| 3, [] -> Neg
	| 4, [] -> NegBits
	| _ -> raise Invalid_expr

let decode_import_mode t =
	match decode_enum t with
	| 0, [] -> INormal
	| 1, [alias] -> IAsName (dec_string alias)
	| 2, [] -> IAll
	| _ -> raise Invalid_expr

let decode_import t = (List.map (fun o -> ((dec_string (field o "name")), (decode_pos (field o "pos")))) (dec_array (field t "path")), decode_import_mode (field t "mode"))

let maybe_decode_pos v = match v with
	| VAbstract (APos p) -> p
	| _ -> Globals.null_pos

let decode_placed_name vp v =
	dec_string v,maybe_decode_pos vp

let rec decode_path t =
	{
		tpackage = List.map dec_string (dec_array (field t "pack"));
		tname = dec_string (field t "name");
		tparams = (match field t "params" with VNull -> [] | a -> List.map decode_tparam (dec_array a));
		tsub = opt dec_string (field t "sub");
	},Globals.null_pos

and decode_tparam v =
	match decode_enum v with
	| 0,[t] -> TPType (decode_ctype t)
	| 1,[e] -> TPExpr (decode_expr e)
	| _ -> raise Invalid_expr

and decode_tparams = function
	| VNull -> []
	| a -> List.map decode_tparam_decl (dec_array a)

and decode_tparam_decl v =
	{
		tp_name = decode_placed_name (field v "name_pos") (field v "name");
		tp_constraints = (match field v "constraints" with VNull -> [] | a -> List.map decode_ctype (dec_array a));
		tp_params = decode_tparams (field v "params");
		tp_meta = decode_meta_content (field v "meta");
	}

and decode_fun v =
	{
		f_params = decode_tparams (field v "params");
		f_args = List.map (fun o ->
			decode_placed_name (field o "name_pos") (field o "name"),
			(match field o "opt" with VNull -> false | v -> dec_bool v),
			decode_meta_content (field o "meta"),
			opt decode_ctype (field o "type"),
			opt decode_expr (field o "value")
		) (dec_array (field v "args"));
		f_type = opt decode_ctype (field v "ret");
		f_expr = opt decode_expr (field v "expr");
	}

and decode_access v =
	match decode_enum v with
	| 0, [] -> APublic
	| 1, [] -> APrivate
	| 2, [] -> AStatic
	| 3, [] -> AOverride
	| 4, [] -> ADynamic
	| 5, [] -> AInline
	| 6, [] -> AMacro
	| _ -> raise Invalid_expr

and decode_meta_entry v =
	Meta.from_string (dec_string (field v "name")), (match field v "params" with VNull -> [] | a -> List.map decode_expr (dec_array a)), decode_pos (field v "pos")

and decode_meta_content = function
	| VNull -> []
	| v -> List.map decode_meta_entry (dec_array v)

and decode_field v =
	let fkind = match decode_enum (field v "kind") with
		| 0, [t;e] ->
			FVar (opt decode_ctype t, opt decode_expr e)
		| 1, [f] ->
			FFun (decode_fun f)
		| 2, [get;set; t; e] ->
			FProp (decode_placed_name VNull get, decode_placed_name VNull set, opt decode_ctype t, opt decode_expr e)
		| _ ->
			raise Invalid_expr
	in
	{
		cff_name = decode_placed_name (field v "name_pos") (field v "name");
		cff_doc = opt dec_string (field v "doc");
		cff_pos = decode_pos (field v "pos");
		cff_kind = fkind;
		cff_access = List.map decode_access (opt_list dec_array (field v "access"));
		cff_meta = opt_list decode_meta_content (field v "meta");
	}

and decode_ctype t =
	let (i,args),p = decode_enum_with_pos t in
	(match i,args with
	| 0, [p] ->
		CTPath (fst (decode_path p))
	| 1, [a;r] ->
		CTFunction (List.map decode_ctype (dec_array a), decode_ctype r)
	| 2, [fl] ->
		CTAnonymous (List.map decode_field (dec_array fl))
	| 3, [t] ->
		CTParent (decode_ctype t)
	| 4, [tl;fl] ->
		CTExtend (List.map decode_path (dec_array tl), List.map decode_field (dec_array fl))
	| 5, [t] ->
		CTOptional (decode_ctype t)
	| _ ->
		raise Invalid_expr),p

let rec decode_expr v =
	let rec loop v =
		let p = decode_pos (field v "pos") in
		(decode (field v "expr") p, p)
	and decode e p =
		match decode_enum e with
		| 0, [c] ->
			EConst (decode_const c)
		| 1, [e1;e2] ->
			EArray (loop e1, loop e2)
		| 2, [op;e1;e2] ->
			EBinop (decode_op op, loop e1, loop e2)
		| 3, [e;f] ->
			EField (loop e, dec_string f)
		| 4, [e] ->
			EParenthesis (loop e)
		| 5, [a] ->
			EObjectDecl (List.map (fun o ->
				(dec_string (field o "field"), loop (field o "expr"))
			) (dec_array a))
		| 6, [a] ->
			EArrayDecl (List.map loop (dec_array a))
		| 7, [e;el] ->
			ECall (loop e,List.map loop (dec_array el))
		| 8, [t;el] ->
			ENew (decode_path t,List.map loop (dec_array el))
		| 9, [op;VBool f;e] ->
			EUnop (decode_unop op,(if f then Postfix else Prefix),loop e)
		| 10, [vl] ->
			EVars (List.map (fun v ->
				((decode_placed_name (field v "name_pos") (field v "name")),opt decode_ctype (field v "type"),opt loop (field v "expr"))
			) (dec_array vl))
		| 11, [fname;f] ->
			EFunction (opt dec_string fname,decode_fun f)
		| 12, [el] ->
			EBlock (List.map loop (dec_array el))
		| 13, [e1;e2] ->
			EFor (loop e1, loop e2)
		| 14, [e1;e2] ->
			EIn (loop e1, loop e2)
		| 15, [e1;e2;e3] ->
			EIf (loop e1, loop e2, opt loop e3)
		| 16, [e1;e2;VBool flag] ->
			EWhile (loop e1,loop e2,if flag then NormalWhile else DoWhile)
		| 17, [e;cases;eo] ->
			let cases = List.map (fun c ->
				(List.map loop (dec_array (field c "values")),opt loop (field c "guard"),opt loop (field c "expr"),maybe_decode_pos (field c "pos"))
			) (dec_array cases) in
			ESwitch (loop e,cases,opt (fun v -> decode_null_expr v,Globals.null_pos) eo)
		| 18, [e;catches] ->
			let catches = List.map (fun c ->
				((decode_placed_name (field c "name_pos") (field c "name")),(decode_ctype (field c "type")),loop (field c "expr"),maybe_decode_pos (field c "pos"))
			) (dec_array catches) in
			ETry (loop e, catches)
		| 19, [e] ->
			EReturn (opt loop e)
		| 20, [] ->
			EBreak
		| 21, [] ->
			EContinue
		| 22, [e] ->
			EUntyped (loop e)
		| 23, [e] ->
			EThrow (loop e)
		| 24, [e;t] ->
			ECast (loop e,opt decode_ctype t)
		| 25, [e;f] ->
			EDisplay (loop e,dec_bool f)
		| 26, [t] ->
			EDisplayNew (decode_path t)
		| 27, [e1;e2;e3] ->
			ETernary (loop e1,loop e2,loop e3)
		| 28, [e;t] ->
			ECheckType (loop e, (decode_ctype t))
		| 29, [m;e] ->
			EMeta (decode_meta_entry m,loop e)
		| 30, [e;f] ->
			EField (loop e, dec_string f) (*** deprecated EType, keep until haxe 3 **)
		| _ ->
			raise Invalid_expr
	in
	try
		loop v
	with Stack_overflow ->
		raise Invalid_expr

and decode_null_expr v =
	match field v "expr" with
	| VNull -> None
	| _ -> Some (decode_expr v)


(* ---------------------------------------------------------------------- *)
(* TYPE ENCODING *)

let encode_ref v convert tostr =
	enc_obj [
		"get", VFunction (Fun0 (fun() -> convert v));
		"__string", VFunction (Fun0 (fun() -> VString (tostr())));
		"toString", VFunction (Fun0 (fun() -> enc_string (tostr())));
		"$", VAbstract (AUnsafe (Obj.repr v));
	]

let decode_ref v : 'a =
	match field v "$" with
	| VAbstract (AUnsafe t) -> Obj.obj t
	| _ -> raise Invalid_expr

let encode_pmap convert m =
	let h = Hashtbl.create 0 in
	PMap.iter (fun k v -> Hashtbl.add h (VString k) (convert v)) m;
	enc_hash h

let encode_pmap_array convert m =
	let l = ref [] in
	PMap.iter (fun _ v -> l := !l @ [(convert v)]) m;
	enc_array !l

let encode_array convert l =
	enc_array (List.map convert l)

let encode_meta m set =
	let meta = ref m in
	enc_obj [
		"get", VFunction (Fun0 (fun() ->
			encode_meta_content (!meta)
		));
		"add", VFunction (Fun3 (fun k vl p ->
			(try
				let el = List.map decode_expr (dec_array vl) in
				meta := (Meta.from_string (dec_string k), el, decode_pos p) :: !meta;
				set (!meta)
			with Invalid_expr ->
				failwith "Invalid expression");
			VNull
		));
		"extract", VFunction (Fun1 (fun k ->
			let k = Meta.from_string (try dec_string k with Invalid_expr -> raise Builtin_error) in
			encode_array encode_meta_entry (List.filter (fun (m,_,_) -> m = k) (!meta))
		));
		"remove", VFunction (Fun1 (fun k ->
			let k = Meta.from_string (try dec_string k with Invalid_expr -> raise Builtin_error) in
			meta := List.filter (fun (m,_,_) -> m <> k) (!meta);
			set (!meta);
			VNull
		));
		"has", VFunction (Fun1 (fun k ->
			let k = Meta.from_string (try dec_string k with Invalid_expr -> raise Builtin_error) in
			VBool (List.exists (fun (m,_,_) -> m = k) (!meta));
		));
	]

let rec encode_mtype t fields =
	let i = t_infos t in
	enc_obj ([
		"__t", 	VAbstract (ATDecl t);
		"pack", enc_array (List.map enc_string (fst i.mt_path));
		"name", enc_string (snd i.mt_path);
		"pos", encode_pos i.mt_pos;
		"module", enc_string (s_type_path i.mt_module.m_path);
		"isPrivate", VBool i.mt_private;
		"meta", encode_meta i.mt_meta (fun m -> i.mt_meta <- m);
		"doc", null enc_string i.mt_doc;
		"params", encode_type_params i.mt_params;
	] @ fields)

and encode_type_params tl =
	enc_array (List.map (fun (n,t) -> enc_obj ["name",enc_string n;"t",encode_type t]) tl)

and encode_tenum e =
	encode_mtype (TEnumDecl e) [
		"isExtern", VBool e.e_extern;
		"exclude", VFunction (Fun0 (fun() -> e.e_extern <- true; VNull));
		"constructs", encode_pmap encode_efield e.e_constrs;
		"names", enc_array (List.map enc_string e.e_names);
	]

and encode_tabstract a =
	encode_mtype (TAbstractDecl a) [
		"type", encode_type a.a_this;
		"impl", (match a.a_impl with None -> VNull | Some c -> encode_clref c);
		"binops", enc_array (List.map (fun (op,cf) -> enc_obj [ "op",encode_binop op; "field",encode_cfield cf]) a.a_ops);
		"unops", enc_array (List.map (fun (op,postfix,cf) -> enc_obj [ "op",encode_unop op; "isPostfix",VBool (match postfix with Postfix -> true | Prefix -> false); "field",encode_cfield cf]) a.a_unops);
		"from", enc_array ((List.map (fun t -> enc_obj [ "t",encode_type t; "field",VNull]) a.a_from) @ (List.map (fun (t,cf) -> enc_obj [ "t",encode_type t; "field",encode_cfield cf]) a.a_from_field));
		"to", enc_array ((List.map (fun t -> enc_obj [ "t",encode_type t; "field",VNull]) a.a_to) @ (List.map (fun (t,cf) -> enc_obj [ "t",encode_type t; "field",encode_cfield cf]) a.a_to_field));
		"array", enc_array (List.map encode_cfield a.a_array);
		"resolve", (match a.a_resolve with None -> VNull | Some cf -> encode_cfref cf)
	]

and encode_efield f =
	enc_obj [
		"name", enc_string f.ef_name;
		"type", encode_type f.ef_type;
		"pos", encode_pos f.ef_pos;
		"namePos", encode_pos f.ef_name_pos;
		"index", VInt f.ef_index;
		"meta", encode_meta f.ef_meta (fun m -> f.ef_meta <- m);
		"doc", null enc_string f.ef_doc;
		"params", encode_type_params f.ef_params;
	]

and encode_cfield f =
	enc_obj [
		"name", enc_string f.cf_name;
		"type", (match f.cf_kind with Method _ -> encode_lazy_type f.cf_type | _ -> encode_type f.cf_type);
		"isPublic", VBool f.cf_public;
		"params", encode_type_params f.cf_params;
		"meta", encode_meta f.cf_meta (fun m -> f.cf_meta <- m);
		"expr", (VFunction (Fun0 (fun() -> ignore(follow f.cf_type); (match f.cf_expr with None -> VNull | Some e -> encode_texpr e))));
		"kind", encode_field_kind f.cf_kind;
		"pos", encode_pos f.cf_pos;
		"namePos",encode_pos f.cf_name_pos;
		"doc", null enc_string f.cf_doc;
		"overloads", encode_ref f.cf_overloads (encode_array encode_cfield) (fun() -> "overloads");
	]

and encode_field_kind k =
	let tag, pl = (match k with
		| Type.Var v -> 0, [encode_var_access v.v_read; encode_var_access v.v_write]
		| Method m -> 1, [encode_method_kind m]
	) in
	enc_enum IFieldKind tag pl

and encode_var_access a =
	let tag, pl = (match a with
		| AccNormal -> 0, []
		| AccNo -> 1, []
		| AccNever -> 2, []
		| AccResolve -> 3, []
		| AccCall -> 4, []
		| AccInline	-> 5, []
		| AccRequire (s,msg) -> 6, [enc_string s; null enc_string msg]
	) in
	enc_enum IVarAccess tag pl

and encode_method_kind m =
	let tag, pl = (match m with
		| MethNormal -> 0, []
		| MethInline -> 1, []
		| MethDynamic -> 2, []
		| MethMacro -> 3, []
	) in
	enc_enum IMethodKind tag pl

and encode_class_kind k =
	let tag, pl = (match k with
		| KNormal -> 0, []
		| KTypeParameter pl -> 1, [encode_tparams pl]
		(* KExtension was here *)
		| KExpr e -> 3, [encode_expr e]
		| KGeneric -> 4, []
		| KGenericInstance (cl, params) -> 5, [encode_clref cl; encode_tparams params]
		| KMacroType -> 6, []
		| KAbstractImpl a -> 7, [encode_abref a]
		| KGenericBuild cfl -> 8, []
	) in
	enc_enum IClassKind tag pl

and encode_tclass c =
	ignore(c.cl_build());
	encode_mtype (TClassDecl c) [
		"kind", encode_class_kind c.cl_kind;
		"isExtern", VBool c.cl_extern;
		"exclude", VFunction (Fun0 (fun() -> c.cl_extern <- true; c.cl_init <- None; VNull));
		"isInterface", VBool c.cl_interface;
		"superClass", (match c.cl_super with
			| None -> VNull
			| Some (c,pl) -> enc_obj ["t",encode_clref c;"params",encode_tparams pl]
		);
		"interfaces", enc_array (List.map (fun (c,pl) -> enc_obj ["t",encode_clref c;"params",encode_tparams pl]) c.cl_implements);
		"fields", encode_ref c.cl_ordered_fields (encode_array encode_cfield) (fun() -> "class fields");
		"statics", encode_ref c.cl_ordered_statics (encode_array encode_cfield) (fun() -> "class fields");
		"constructor", (match c.cl_constructor with None -> VNull | Some cf -> encode_cfref cf);
		"init", (match c.cl_init with None -> VNull | Some e -> encode_texpr e);
		"overrides", (enc_array (List.map encode_cfref c.cl_overrides))
	]

and encode_ttype t =
	encode_mtype (TTypeDecl t) [
		"isExtern", VBool false;
		"exclude", VFunction (Fun0 (fun() -> VNull));
		"type", encode_type t.t_type;
	]

and encode_tanon a =
	enc_obj [
		"fields", encode_pmap_array encode_cfield a.a_fields;
		"status", encode_anon_status !(a.a_status);
	]

and encode_anon_status s =
	let tag, pl = (match s with
		| Closed -> 0, []
		| Opened -> 1, []
		| Type.Const -> 2, []
		| Extend tl -> 3, [encode_ref tl (fun tl -> enc_array (List.map encode_type tl)) (fun() -> "<extended types>")]
		| Statics cl -> 4, [encode_clref cl]
		| EnumStatics en -> 5, [encode_enref en]
		| AbstractStatics ab -> 6, [encode_abref ab]
	)
	in
	enc_enum IAnonStatus tag pl

and encode_tparams pl =
	enc_array (List.map encode_type pl)

and encode_clref c =
	encode_ref c encode_tclass (fun() -> s_type_path c.cl_path)

and encode_enref en =
	encode_ref en encode_tenum (fun() -> s_type_path en.e_path)

and encode_cfref cf =
	encode_ref cf encode_cfield (fun() -> cf.cf_name)

and encode_abref ab =
	encode_ref ab encode_tabstract (fun() -> s_type_path ab.a_path)

and encode_type t =
	let rec loop = function
		| TMono r ->
			(match !r with
			| None -> 0, [encode_ref r (fun r -> match !r with None -> VNull | Some t -> encode_type t) (fun() -> "<mono>")]
			| Some t -> loop t)
		| TEnum (e, pl) ->
			1 , [encode_ref e encode_tenum (fun() -> s_type_path e.e_path); encode_tparams pl]
		| TInst (c, pl) ->
			2 , [encode_clref c; encode_tparams pl]
		| TType (t,pl) ->
			3 , [encode_ref t encode_ttype (fun() -> s_type_path t.t_path); encode_tparams pl]
		| TFun (pl,ret) ->
			let pl = List.map (fun (n,o,t) ->
				enc_obj [
					"name",enc_string n;
					"opt",VBool o;
					"t",encode_type t
				]
			) pl in
			4 , [enc_array pl; encode_type ret]
		| TAnon a ->
			5, [encode_ref a encode_tanon (fun() -> "<anonymous>")]
		| TDynamic tsub as t ->
			if t == t_dynamic then
				6, [VNull]
			else
				6, [encode_type tsub]
		| TLazy f ->
			loop (!f())
		| TAbstract (a, pl) ->
			8, [encode_abref a; encode_tparams pl]
	in
	let tag, pl = loop t in
	enc_enum IType tag pl

and encode_lazy_type t =
	let rec loop = function
		| TMono r ->
			(match !r with
			| Some t -> loop t
			| _ -> encode_type t)
		| TLazy f ->
			enc_enum IType 7 [VAbstract (ALazyType f)]
		| _ ->
			encode_type t
	in
	loop t

and decode_type t =
	match decode_enum t with
	| 0, [r] -> TMono (decode_ref r)
	| 1, [e; pl] -> TEnum (decode_ref e, List.map decode_type (dec_array pl))
	| 2, [c; pl] -> TInst (decode_ref c, List.map decode_type (dec_array pl))
	| 3, [t; pl] -> TType (decode_ref t, List.map decode_type (dec_array pl))
	| 4, [pl; r] -> TFun (List.map (fun p -> dec_string (field p "name"), dec_bool (field p "opt"), decode_type (field p "t")) (dec_array pl), decode_type r)
	| 5, [a] -> TAnon (decode_ref a)
	| 6, [VNull] -> t_dynamic
	| 6, [t] -> TDynamic (decode_type t)
	| 7, [VAbstract (ALazyType f)] -> TLazy f
	| 8, [a; pl] -> TAbstract (decode_ref a, List.map decode_type (dec_array pl))
	| _ -> raise Invalid_expr

let decode_tdecl v =
	match v with
	| VObject o ->
		(match get_field o (hash "__t") with
		| VAbstract (ATDecl t) -> t
		| _ -> raise Invalid_expr)
	| _ -> raise Invalid_expr

(* ---------------------------------------------------------------------- *)
(* TEXPR Encoding *)

let vopt f v = match v with
	| None -> VNull
	| Some v -> f v

let rec encode_tconst c =
	let tag, pl = match c with
		| TInt i -> 0,[best_int i]
		| TFloat f -> 1,[enc_string f]
		| TString s -> 2,[enc_string s]
		| TBool b -> 3,[VBool b]
		| TNull -> 4,[]
		| TThis -> 5,[]
		| TSuper -> 6,[]
	in
	enc_enum ITConstant tag pl

and encode_tvar v =
	let f_extra (pl,e) =
		enc_obj [
			"params",encode_type_params pl;
			"expr",vopt encode_texpr e
		]
	in
	enc_obj [
		"id", VInt v.v_id;
		"name", enc_string v.v_name;
		"t", encode_type v.v_type;
		"capture", VBool v.v_capture;
		"extra", vopt f_extra v.v_extra;
		"meta", encode_meta v.v_meta (fun m -> v.v_meta <- m);
		"$", VAbstract (AUnsafe (Obj.repr v));
	]

and encode_module_type mt =
	let tag,pl = match mt with
		| TClassDecl c -> 0,[encode_clref c]
		| TEnumDecl e -> 1,[encode_enref e]
		| TTypeDecl t -> 2,[encode_ref t encode_ttype (fun () -> s_type_path t.t_path)]
		| TAbstractDecl a -> 3,[encode_abref a]
	in
	enc_enum IModuleType tag pl

and encode_tfunc func =
	enc_obj [
		"args",enc_array (List.map (fun (v,c) ->
			enc_obj [
				"v",encode_tvar v;
				"value",match c with None -> VNull | Some c -> encode_tconst c
			]
		) func.tf_args);
		"t",encode_type func.tf_type;
		"expr",encode_texpr func.tf_expr
	]

and encode_field_access fa =
	let encode_instance c tl =
		enc_obj [
			"c",encode_clref c;
			"params",encode_tparams tl
		]
	in
	let tag,pl = match fa with
		| FInstance(c,tl,cf) -> 0,[encode_clref c;encode_tparams tl;encode_cfref cf]
		| FStatic(c,cf) -> 1,[encode_clref c;encode_cfref cf]
		| FAnon(cf) -> 2,[encode_cfref cf]
		| FDynamic(s) -> 3,[enc_string s]
		| FClosure(co,cf) -> 4,[(match co with Some (c,tl) -> encode_instance c tl | None -> VNull);encode_cfref cf]
		| FEnum(en,ef) -> 5,[encode_enref en;encode_efield ef]
	in
	enc_enum IFieldAccess tag pl

and encode_texpr e =
	let rec loop e =
		let tag, pl = match e.eexpr with
			| TConst c -> 0,[encode_tconst c]
			| TLocal v -> 1,[encode_tvar v]
			| TArray(e1,e2) -> 2,[loop e1; loop e2]
			| TBinop(op,e1,e2) -> 3,[encode_binop op;loop e1;loop e2]
			| TField(e1,fa) -> 4,[loop e1;encode_field_access fa]
			| TTypeExpr mt -> 5,[encode_module_type mt]
			| TParenthesis e1 -> 6,[loop e1]
			| TObjectDecl fl -> 7, [enc_array (List.map (fun (f,e) ->
				enc_obj [
					"name",enc_string f;
					"expr",loop e;
				]) fl)]
			| TArrayDecl el -> 8,[encode_texpr_list el]
			| TCall(e1,el) -> 9,[loop e1;encode_texpr_list el]
			| TNew(c,pl,el) -> 10,[encode_clref c;encode_tparams pl;encode_texpr_list el]
			| TUnop(op,flag,e1) -> 11,[encode_unop op;VBool (flag = Postfix);loop e1]
			| TFunction func -> 12,[encode_tfunc func]
			| TVar (v,eo) -> 13,[encode_tvar v;vopt encode_texpr eo]
			| TBlock el -> 14,[encode_texpr_list el]
			| TFor(v,e1,e2) -> 15,[encode_tvar v;loop e1;loop e2]
			| TIf(eif,ethen,eelse) -> 16,[loop eif;loop ethen;vopt encode_texpr eelse]
			| TWhile(econd,e1,flag) -> 17,[loop econd;loop e1;VBool (flag = NormalWhile)]
			| TSwitch(e1,cases,edef) -> 18,[
				loop e1;
				enc_array (List.map (fun (el,e) -> enc_obj ["values",encode_texpr_list el;"expr",loop e]) cases);
				vopt encode_texpr edef
				]
			| TTry(e1,catches) -> 19,[
				loop e1;
				enc_array (List.map (fun (v,e) ->
					enc_obj [
						"v",encode_tvar v;
						"expr",loop e
					]) catches
				)]
			| TReturn e1 -> 20,[vopt encode_texpr e1]
			| TBreak -> 21,[]
			| TContinue -> 22,[]
			| TThrow e1 -> 23,[loop e1]
			| TCast(e1,mt) -> 24,[loop e1;match mt with None -> VNull | Some mt -> encode_module_type mt]
			| TMeta(m,e1) -> 25,[encode_meta_entry m;loop e1]
			| TEnumParameter(e1,ef,i) -> 26,[loop e1;encode_efield ef;VInt i]
		in
		enc_obj [
			"pos", encode_pos e.epos;
			"expr", enc_enum ITypedExpr tag pl;
			"t", encode_type e.etype
		]
	in
	loop e

and encode_texpr_list el =
	enc_array (List.map encode_texpr el)

(* ---------------------------------------------------------------------- *)
(* TEXPR Decoding *)

let decode_tconst c =
	match decode_enum c with
	| 0, [s] -> TInt (match s with VInt i -> Int32.of_int i | VInt32 i -> i | _ -> raise Invalid_expr)
	| 1, [s] -> TFloat (dec_string s)
	| 2, [s] -> TString (dec_string s)
	| 3, [s] -> TBool (dec_bool s)
	| 4, [] -> TNull
	| 5, [] -> TThis
	| 6, [] -> TSuper
	| _ -> raise Invalid_expr

let decode_type_params v =
	List.map (fun v -> dec_string (field v "name"),decode_type (field v "t")) (dec_array v)

let decode_tvar v =
	match field v "$" with
	| VAbstract (AUnsafe t) -> Obj.obj t
	| _ -> raise Invalid_expr

let decode_var_access v =
	match decode_enum v with
	| 0, [] -> AccNormal
	| 1, [] -> AccNo
	| 2, [] -> AccNever
	| 3, [] -> AccResolve
	| 4, [] -> AccCall
	| 5, [] -> AccInline
	| 6, [s1;s2] -> AccRequire(dec_string s1, opt dec_string s2)
	| _ -> raise Invalid_expr

let decode_method_kind v =
	match decode_enum v with
	| 0, [] -> MethNormal
	| 1, [] -> MethInline
	| 2, [] -> MethDynamic
	| 3, [] -> MethMacro
	| _ -> raise Invalid_expr

let decode_field_kind v =
	match decode_enum v with
	| 0, [vr;vw] -> Type.Var({v_read = decode_var_access vr; v_write = decode_var_access vw})
	| 1, [m] -> Method (decode_method_kind m)
	| _ -> raise Invalid_expr

let decode_cfield v =
	{
		cf_name = dec_string (field v "name");
		cf_type = decode_type (field v "type");
		cf_public = dec_bool (field v "isPublic");
		cf_pos = decode_pos (field v "pos");
		cf_name_pos = decode_pos (field v "namePos");
		cf_doc = opt dec_string (field v "doc");
		cf_meta = []; (* TODO *)
		cf_kind = decode_field_kind (field v "kind");
		cf_params = decode_type_params (field v "params");
		cf_expr = None;
		cf_expr_unoptimized = None;
		cf_overloads = decode_ref (field v "overloads");
	}

let decode_efield v =
	{
		ef_name = dec_string (field v "name");
		ef_type = decode_type (field v "type");
		ef_pos = decode_pos (field v "pos");
		ef_name_pos = decode_pos (field v "namePos");
		ef_index = (match field v "index" with VInt i -> i | _ -> raise Invalid_expr);
		ef_meta = []; (* TODO *)
		ef_doc = opt dec_string (field v "doc");
		ef_params = decode_type_params (field v "params")
	}

let decode_field_access v =
	match decode_enum v with
	| 0, [c;tl;cf] ->
		let c = decode_ref c in
		FInstance(c,List.map decode_type (dec_array tl),decode_ref cf)
	| 1, [c;cf] -> FStatic(decode_ref c,decode_ref cf)
	| 2, [cf] -> FAnon(decode_ref cf)
	| 3, [s] -> FDynamic(dec_string s)
	| 4, [co;cf] ->
		let co = match co with
			| VNull -> None
			| _ -> Some (decode_ref (field co "c"),List.map decode_type (dec_array (field co "params")))
		in
		FClosure(co,decode_ref cf)
	| 5, [e;ef] -> FEnum(decode_ref e,decode_efield ef)
	| _ -> raise Invalid_expr

let decode_module_type v =
	match decode_enum v with
	| 0, [c] -> TClassDecl (decode_ref c)
	| 1, [en] -> TEnumDecl (decode_ref en)
	| 2, [t] -> TTypeDecl (decode_ref t)
	| 3, [a] -> TAbstractDecl (decode_ref a)
	| _ -> raise Invalid_expr

let decode_tfunc v =
	{
		tf_args = List.map (fun v -> decode_tvar (field v "v"),opt decode_tconst (field v "value")) (dec_array (field v "args"));
		tf_type = decode_type (field v "t");
		tf_expr = decode_texpr (field v "expr")
	}

let rec decode_texpr v =
	let rec loop v =
		mk (decode (field v "expr")) (decode_type (field v "t")) (decode_pos (field v "pos"))
	and decode e =
		match decode_enum e with
		| 0, [c] ->	TConst(decode_tconst c)
		| 1, [v] -> TLocal(decode_tvar v)
		| 2, [v1;v2] -> TArray(loop v1,loop v2)
		| 3, [op;v1;v2] -> TBinop(decode_op op,loop v1,loop v2)
		| 4, [v1;fa] -> TField(loop v1,decode_field_access fa)
		| 5, [mt] -> TTypeExpr(decode_module_type mt)
		| 6, [v1] -> TParenthesis(loop v1)
		| 7, [v] -> TObjectDecl(List.map (fun v -> dec_string (field v "name"),loop (field v "expr")) (dec_array v))
		| 8, [vl] -> TArrayDecl(List.map loop (dec_array vl))
		| 9, [v1;vl] -> TCall(loop v1,List.map loop (dec_array vl))
		| 10, [c;tl;vl] -> TNew(decode_ref c,List.map decode_type (dec_array tl),List.map loop (dec_array vl))
		| 11, [op;pf;v1] -> TUnop(decode_unop op,(if dec_bool pf then Postfix else Prefix),loop v1)
		| 12, [f] -> TFunction(decode_tfunc f)
		| 13, [v;eo] -> TVar(decode_tvar v,opt loop eo)
		| 14, [vl] -> TBlock(List.map loop (dec_array vl))
		| 15, [v;v1;v2] -> TFor(decode_tvar v,loop v1,loop v2)
		| 16, [vif;vthen;velse] -> TIf(loop vif,loop vthen,opt loop velse)
		| 17, [vcond;v1;b] -> TWhile(loop vcond,loop v1,if dec_bool b then NormalWhile else DoWhile)
		| 18, [v1;cl;vdef] -> TSwitch(loop v1,List.map (fun v -> List.map loop (dec_array (field v "values")),loop (field v "expr")) (dec_array cl),opt loop vdef)
		| 19, [v1;cl] -> TTry(loop v1,List.map (fun v -> decode_tvar (field v "v"),loop (field v "expr")) (dec_array cl))
		| 20, [vo] -> TReturn(opt loop vo)
		| 21, [] -> TBreak
		| 22, [] -> TContinue
		| 23, [v1] -> TThrow(loop v1)
		| 24, [v1;mto] -> TCast(loop v1,opt decode_module_type mto)
		| 25, [m;v1] -> TMeta(decode_meta_entry m,loop v1)
		| 26, [v1;ef;i] -> TEnumParameter(loop v1,decode_efield ef,match i with VInt i -> i | _ -> raise Invalid_expr)
		| i,el -> Printf.printf "%i %i\n" i (List.length el); raise Invalid_expr
	in
	try
		loop v
	with Stack_overflow ->
		raise Invalid_expr

(* ---------------------------------------------------------------------- *)
(* TYPE DEFINITION *)

let decode_type_def v =
	let pack = List.map dec_string (dec_array (field v "pack")) in
	let name = decode_placed_name (field v "name_pos") (field v "name") in
	let meta = decode_meta_content (field v "meta") in
	let pos = decode_pos (field v "pos") in
	let isExtern = (match field v "isExtern" with VNull -> false | v -> dec_bool v) in
	let fields = List.map decode_field (dec_array (field v "fields")) in
	let mk fl dl =
		{
			d_name = name;
			d_doc = None;
			d_params = decode_tparams (field v "params");
			d_meta = meta;
			d_flags = fl;
			d_data = dl;
		}
	in
	let tdef = (match decode_enum (field v "kind") with
	| 0, [] ->
		let conv f =
			let loop ((n,_),opt,_,t,_) =
				match t with
				| None -> raise Invalid_expr
				| Some t -> n, opt, t
			in
			let args, params, t = (match f.cff_kind with
				| FVar (t,None) -> [], [], t
				| FFun f -> List.map loop f.f_args, f.f_params, f.f_type
				| _ -> raise Invalid_expr
			) in
			{
				ec_name = f.cff_name;
				ec_doc = f.cff_doc;
				ec_meta = f.cff_meta;
				ec_pos = f.cff_pos;
				ec_args = args;
				ec_params = params;
				ec_type = t;
			}
		in
		EEnum (mk (if isExtern then [EExtern] else []) (List.map conv fields))
	| 1, [] ->
		ETypedef (mk (if isExtern then [EExtern] else []) (CTAnonymous fields,Globals.null_pos))
	| 2, [ext;impl;interf] ->
		let flags = if isExtern then [HExtern] else [] in
		let flags = (match interf with VNull | VBool false -> flags | VBool true -> HInterface :: flags | _ -> raise Invalid_expr) in
		let flags = (match opt decode_path ext with None -> flags | Some t -> HExtends t :: flags) in
		let flags = (match opt (fun v -> List.map decode_path (dec_array v)) impl with None -> flags | Some l -> List.map (fun t -> HImplements t) l @ flags) in
		EClass (mk flags fields)
	| 3, [t] ->
		ETypedef (mk (if isExtern then [EExtern] else []) (decode_ctype t))
	| 4, [tthis;tfrom;tto] ->
		let flags = match opt dec_array tfrom with None -> [] | Some ta -> List.map (fun t -> AFromType (decode_ctype t)) ta in
		let flags = match opt dec_array tto with None -> flags | Some ta -> (List.map (fun t -> AToType (decode_ctype t)) ta) @ flags in
		let flags = match opt decode_ctype tthis with None -> flags | Some t -> (AIsType t) :: flags in
		EAbstract(mk flags fields)
	| _ ->
		raise Invalid_expr
	) in
	(* if our package ends with an uppercase letter, then it's the module name *)
	let pack,name = (match List.rev pack with
		| last :: l when not (is_lower_ident last) -> List.rev l, last
		| _ -> pack, fst name
	) in
	(pack, name), tdef, pos

(* ---------------------------------------------------------------------- *)
(* VALUE-TO-CONSTANT *)

let rec make_const e =
	match e.eexpr with
	| TConst c ->
		(match c with
		| TInt i -> best_int i
		| TFloat s -> VFloat (float_of_string s)
		| TString s -> enc_string s
		| TBool b -> VBool b
		| TNull -> VNull
		| TThis | TSuper -> raise Exit)
	| TParenthesis e | TMeta(_,e) | TCast(e,None) ->
		make_const e
	| TObjectDecl el ->
		VObject (obj (hash_field (get_ctx())) (List.map (fun (f,e) -> f, make_const e) el))
	| TArrayDecl al ->
		enc_array (List.map make_const al)
	| _ ->
		raise Exit

(* ---------------------------------------------------------------------- *)
(* TEXPR-TO-AST-EXPR *)



;;
encode_complex_type_ref := encode_ctype;
decode_complex_type_ref := decode_ctype;
decode_pos_ref := decode_pos;
enc_array_ref := enc_array;
dec_array_ref := dec_array;
encode_type_ref := encode_type;
decode_type_ref := decode_type;
encode_expr_ref := encode_expr;
decode_expr_ref := decode_expr;
encode_clref_ref := encode_clref;
enc_string_ref := enc_string;
enc_hash_ref := enc_hash;
encode_texpr_ref := encode_texpr;
decode_texpr_ref := decode_texpr;
encode_tvar_ref := encode_tvar;
decode_path_ref := decode_path;
encode_import_ref := encode_import;
decode_import_ref := decode_import;
eval_expr_ref := eval_expr;
encode_import_ref := encode_import;
encode_module_type_ref := encode_module_type;