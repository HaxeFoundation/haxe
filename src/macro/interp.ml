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
open Common
open Nast
open Unix
open Type
open MacroApi

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
	| ALazyType of ((unit -> Type.t) ref) * (unit -> value)
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
	r : Pcre.regexp;
	mutable r_string : string;
	mutable r_groups : int array;
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

type callstack = {
	cpos : pos;
	cthis : value;
	cstack : int;
	cenv : value array;
}

type context = {
	gen : Genneko.context;
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
	mutable curapi : value MacroApi.compiler_api;
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

exception Continue
exception Break of value
exception Return of value
exception Sys_exit of int

(* ---------------------------------------------------------------------- *)
(* UTILS *)

let get_ctx_ref = ref (fun() -> assert false)
let get_ctx() = (!get_ctx_ref)()

let to_int f = Int32.of_float (mod_float f 2147483648.0)
let need_32_bits i = Int32.compare (Int32.logand (Int32.add i 0x40000000l) 0x80000000l) Int32.zero <> 0
let best_int i = if need_32_bits i then VInt32 i else VInt (Int32.to_int i)

let macro_lib = Hashtbl.create 0

let setup get_api =
	let api = get_api (fun() -> (get_ctx()).curapi.get_com()) (fun() -> (get_ctx()).curapi) in
	List.iter (fun (n,v) -> Hashtbl.replace macro_lib n (match v with VFunction v -> v | _ -> assert false)) api

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

let exc_string str =
	exc (VString str)

let select ctx =
	get_ctx_ref := (fun() -> ctx)

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
	let prev = get_ctx() in (* switch context in case we have an older one, see #5676 *)
	select ctx;
	try
		let v = f() in
		final();
		select prev;
		Some v
	with Runtime v ->
		pop ctx (DynArray.length ctx.stack - n);
		final();
		select prev;
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
		select prev;
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
			VBool false
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
		"math_sqrt", Fun1 (fun v -> let v = num v in VFloat (if v < 0. then nan else sqrt v));
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
		"parse_xml",
			(Fun2 (fun str o ->
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
		);
	(* memory, module : not planned *)
	])

(* ---------------------------------------------------------------------- *)
(* REGEXP LIBRARY *)

let reg_lib =
	let error() =
		raise Builtin_error
	in
	let open Pcre in
	let string_of_pcre_error = function
		| BadPattern(s,i) -> Printf.sprintf "at %i: %s" i s
		| Partial -> "Partial"
		| BadPartial -> "BadPartial"
		| BadUTF8 -> "BadUTF8"
		| BadUTF8Offset -> "BadUTF8Offset"
		| MatchLimit -> "MatchLimit"
		| RecursionLimit -> "RecursionLimit"
		| InternalError s -> "InternalError: " ^ s
	in
	make_library [
		"regexp_new_options", Fun2 (fun str opt ->
			match str,opt with
			| VString str,VString opt ->
				let flags = List.map (function
					| 'i' -> `CASELESS
					| 's' -> `DOTALL
					| 'm' -> `MULTILINE
					| 'u' -> `UTF8
					| 'g' -> `UNGREEDY
					| c -> failwith ("Unsupported regexp option '" ^ String.make 1 c ^ "'")
				) (ExtString.String.explode opt) in
				let r = try regexp ~flags str with Error error -> failwith (string_of_pcre_error error) in
				let pcre = {
					r = r;
					r_string = "";
					r_groups = [||]
				} in
				VAbstract (AReg pcre)
			| _ -> error()
		);
		"regexp_match", Fun4 (fun rex str pos len -> match rex, str, pos, len with
			| VAbstract (AReg rex),VString str,VInt pos,VInt len ->
				begin try
					(* A bit awkward, but the PCRE bindings don't seem to expose the len argument... *)
					if pos + len > String.length str then raise Not_found;
					let str = String.sub str 0 (pos + len) in
					let a = pcre_exec ~rex:rex.r ~pos str in
					rex.r_string <- str;
					rex.r_groups <- a;
					VBool true
				with Not_found ->
					VBool false
				end
			| _ -> error()
		);
		"regexp_matched", Fun2 (fun r n -> match r, n with
			| VAbstract (AReg r),VInt n ->
				let pos = r.r_groups.(n * 2) in
				let len = r.r_groups.(n * 2 + 1) - pos in
				if pos = -1 then VNull
				else VString (String.sub r.r_string pos len)
			| _ -> error()
		);
		"regexp_matched_pos", Fun2 (fun r n -> match r, n with
			| VAbstract (AReg r),VInt n ->
				let pos = r.r_groups.(n * 2) in
				let len = r.r_groups.(n * 2 + 1) - pos in
				VObject (obj (hash_field (get_ctx())) ["pos",VInt pos;"len",VInt len])
			| _ -> error()
		);
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
	| ECall ((EConst (Builtin "__mk_pos__"),_),[(ECall (_,[EConst (String file),_]),_);(EConst (Int min),_);(EConst (Int max),_)]) ->
		let pos = VAbstract (APos { Globals.pfile = file; Globals.pmin = min; Globals.pmax = max }) in
		(fun() -> pos)
	| ECall ((EConst (Builtin "typewrap"),_),[t]) ->
		(fun() -> VAbstract (ATDecl (Obj.magic t)))
	| ECall ((EConst (Builtin "__delayed_call__"),_),[EConst (Int index),_]) ->
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
		| VAbstract (ALazyType (f,get)) ->
			get()
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

let eval_delayed ctx e =
	eval ctx (Genneko.gen_expr ctx.gen e)

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
		exports = VObject { ofields = [||]; oproto = None };
	} in
	ctx.do_call <- call ctx;
	ctx.do_string <- to_string ctx 0;
	ctx.do_loadprim <- load_prim ctx;
	ctx.do_compare <- compare ctx;
	select ctx;
	List.iter (fun e -> ignore((eval ctx e)())) (Genneko.header());
	ctx

let do_reuse ctx api =
	ctx.curapi <- api

let can_reuse ctx types =
	let has_old_version t =
		let inf = Type.t_infos t in
		try
			Hashtbl.find ctx.types inf.mt_path <> inf.mt_module.m_id
		with Not_found ->
			false
	in
	not (List.exists has_old_version types)

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

let init ctx =
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
	ctx.enums <- Array.of_list (List.map get_enum_proto MacroApi.all_enums);
	ctx.error_proto <- (match get_path ctx ["haxe";"macro";"Error";"prototype"] null_pos with VObject p -> p | _ -> failwith ("haxe.macro.Error does not exists"))

(* ---------------------------------------------------------------------- *)
(* MACRO API IMPL *)

let vnull = VNull

let vint i = VInt i
let vint32 i = VInt32 i
let vfun0 f = VFunction (Fun0 f)
let vfun1 f = VFunction (Fun1 f)
let vfun2 f = VFunction (Fun2 f)
let vfun3 f = VFunction (Fun3 f)
let vfun4 f = VFunction (Fun4 f)
let vfun5 f = VFunction (Fun5 f)
let vbool b = VBool b
let vfloat f = VFloat f

let encode_unsafe o = VAbstract (AUnsafe o)

let value_string v = to_string (get_ctx()) 0 v

let field v f =
	match v with
	| VObject o -> get_field o (hash f)
	| _ -> raise Invalid_expr

let decode_string v =
	match field v "__s" with
	| VString s -> s
	| _ -> raise Invalid_expr

let decode_bytes v =
	match field v "b" with
	| VString s -> s
	| _ -> raise Invalid_expr

let decode_array v =
	match field v "__a", field v "length" with
	| VArray a, VInt l -> Array.to_list (if Array.length a = l then a else Array.sub a 0 l)
	| _ -> raise Invalid_expr

let decode_lazytype = function
	| VAbstract (ALazyType (t,_)) -> t
	| _ -> raise Invalid_expr

let encode_lazytype t f = VAbstract (ALazyType (t,f))
let encode_tdecl t = VAbstract (ATDecl t)

let decode_tdecl = function
	| VAbstract (ATDecl t) -> t
	| _ -> raise Invalid_expr

let decode_int = function
	| VInt i -> i
	| VInt32 i -> Int32.to_int i
	| _ -> raise Invalid_expr

let decode_i32 = function
	| VInt i -> Int32.of_int i
	| VInt32 i -> i
	| _ -> raise Invalid_expr

let encode_pos p =
	VAbstract (APos p)

let encode_inst path fields =
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

let encode_array l =
	let a = Array.of_list l in
	encode_inst ["Array"] [
		"__a", VArray a;
		"length", VInt (Array.length a);
	]

let encode_string s =
	encode_inst ["String"] [
		"__s", VString s;
		"length", VInt (String.length s)
	]

let encode_bytes s =
	encode_inst ["haxe";"io";"Bytes"] [
		"b", VString s;
		"length", VInt (String.length s)
	]

let encode_hash h =
	encode_inst ["haxe";"ds";"StringMap"] [
		"h", VAbstract (AHash h);
	]

let encode_obj _ l = VObject (obj hash l)

let encode_enum (i:enum_type) pos index pl =
	let eindex : int = Obj.magic i in
	let edef = (get_ctx()).enums.(eindex) in
	if pl = [] then
		fst edef.(index)
	else
		encode_inst ["haxe";"macro";enum_name i] (
			("tag", VString (snd edef.(index))) ::
			("index", VInt index) ::
			("args", VArray (Array.of_list pl)) ::
			(match pos with None -> [] | Some p -> ["pos", encode_pos p])
		)

let encode_ref v convert tostr =
	encode_obj O__Const [
		"get", VFunction (Fun0 (fun() -> convert v));
		"__string", VFunction (Fun0 (fun() -> VString (tostr())));
		"toString", VFunction (Fun0 (fun() -> encode_string (tostr())));
		"$", VAbstract (AUnsafe (Obj.repr v));
	]

let decode_ref v : 'a =
	match field v "$" with
	| VAbstract (AUnsafe t) -> Obj.obj t
	| _ -> raise Invalid_expr

let encode_string_map convert m =
	let h = Hashtbl.create 0 in
	PMap.iter (fun k v -> Hashtbl.add h (VString k) (convert v)) m;
	encode_hash h

let decode_pos = function
	| VAbstract (APos p) -> p
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

let decode_bool = function
	| VBool b -> b
	| _ -> raise Invalid_expr

let decode_unsafe = function
	| VAbstract (AUnsafe o) -> o
	| _ -> raise Invalid_expr

let compiler_error msg pos =
	exc (encode_inst ["haxe";"macro";"Error"] [("message",encode_string msg);("pos",encode_pos pos)])

let value_to_expr v p =
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
				(("fileName",Globals.null_pos) , (Ast.EConst (Ast.String p.Globals.pfile) , p)) ::
				(("lineNumber",Globals.null_pos) , (Ast.EConst (Ast.Int (string_of_int (Lexer.get_error_line p))),p)) ::
				(("className",Globals.null_pos) , (Ast.EConst (Ast.String ("")),p)) ::
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
					let fields = List.fold_left (fun acc (fid,v) -> ((field_name ctx fid,Globals.null_pos), loop v) :: acc) [] (Array.to_list o.ofields) in
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
	loop v

let value_signature v =
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
	Digest.string (Marshal.to_string v [Marshal.Closures])

let prepare_callback v n =
	match v with
	| VFunction _ | VClosure _ ->
		let ctx = get_ctx() in
		(fun args ->
			match catch_errors ctx (fun() -> ctx.do_call VNull v args null_pos) with
			| None -> VNull
			| Some v -> v
		)
	| _ ->
		raise Invalid_expr
