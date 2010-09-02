(*
 *  Haxe Compiler
 *  Copyright (c)2005-2010 Nicolas Cannasse
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
open Nast
open Unix

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

and vobject = {
	ofields : (string,value) Hashtbl.t;
	mutable oproto : vobject option;
}

and vabstract =
	| AKind of vabstract
	| AInt32 of int32
	| AHash of (value, value) Hashtbl.t
	| ARandom of Random.State.t
	| ABuffer of Buffer.t
	| APos of Ast.pos

and vfunction =
	| Fun0 of (unit -> value)
	| Fun1 of (value -> value)
	| Fun2 of (value -> value -> value)
	| Fun3 of (value -> value -> value -> value)
	| Fun4 of (value -> value -> value -> value -> value)
	| Fun5 of (value -> value -> value -> value -> value -> value)
	| FunVar of (value list -> value)

type cmp =
	| CEq
	| CSup
	| CInf
	| CUndef

type context = {
	com : Common.context;
	gen : Genneko.context;
	packages : (string list,unit) Hashtbl.t;
	types : (Type.path,bool) Hashtbl.t;
	globals : (string, value) Hashtbl.t;
	mutable do_call : value -> value -> value list -> pos -> value;
	mutable do_string : value -> string;
	mutable do_loadprim : value -> value -> value;
	mutable do_compare : value -> value -> cmp;
	mutable locals : (string, value ref) PMap.t;
	mutable stack : pos list;
	mutable exc : pos list;
	mutable vthis : value;
}

type access =
	| AccField of value * string
	| AccArray of value * value
	| AccVar of string

exception Runtime of value
exception Builtin_error

exception Error of string * Ast.pos list

exception Continue
exception Break of value
exception Return of value

let get_ctx_ref = ref (fun() -> assert false)
let get_ctx() = (!get_ctx_ref)()

let to_int f = int_of_float (mod_float f 2147483648.0)

let make_pos p =
	{
		Ast.pfile = p.psource;
		Ast.pmin = if p.pline < 0 then 0 else p.pline land 0xFFFF;
		Ast.pmax = if p.pline < 0 then 0 else p.pline lsr 16;
	}

let warn ctx msg p =
	ctx.com.Common.warning msg (make_pos p)

let obj fields =
	let h = Hashtbl.create 0 in
	List.iter (fun (k,v) -> Hashtbl.replace h k v) fields;
	{
		ofields = h;
		oproto = None;
	}

let exc v =
	raise (Runtime v)

let parse_int s =
	let rec loop_hex i =
		if i = String.length s then s else
		match String.unsafe_get s i with
		| '0'..'9' | 'a'..'f' | 'A'..'F' -> loop_hex (i + 1)
		| _ -> String.sub s 0 i
	in
	let rec loop i =
		if i = String.length s then s else
		match String.unsafe_get s i with
		| '0'..'9' | '-' -> loop (i + 1)
		| 'x' when i = 1 && String.get s 0 = '0' -> loop_hex (i + 1)
		| _ -> String.sub s 0 i
	in
	int_of_string (loop 0)

let parse_float s =
	let rec loop i =
		if i = String.length s then s else
		match String.unsafe_get s i with
		| '0'..'9' | '-' | 'e' | 'E' | '.' -> loop (i + 1)
		| _ -> String.sub s 0 i
	in
	float_of_string (loop 0)

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

let rec get_field o fname =
	try
		Hashtbl.find o.ofields fname
	with Not_found ->
		match o.oproto with
		| None -> VNull
		| Some p -> get_field p fname

let rec get_field_opt o fname =
	try
		Some (Hashtbl.find o.ofields fname)
	with Not_found ->
		match o.oproto with
		| None -> None
		| Some p -> get_field_opt p fname

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
			VArray [|VString p.Ast.pfile;VInt (Lexer.get_error_line p)|]
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
			| VNull -> VObject (obj [])
			| VObject o -> VObject { ofields = Hashtbl.copy o.ofields; oproto = o.oproto }
			| _ -> error()
		);
		"objget", Fun2 (fun o f ->
			match o with
			| VObject o -> get_field o (vstring f)
			| _ -> VNull
		);
		"objset", Fun3 (fun o f v ->
			match o with
			| VObject o -> Hashtbl.replace o.ofields (vstring f) v; v
			| _ -> VNull
		);
		"objcall", Fun3 (fun o f pl ->
			match o with
			| VObject oo ->
				(get_ctx()).do_call o (get_field oo (vstring f)) (Array.to_list (varray pl)) p
			| _ -> VNull
		);
		"objfield", Fun2 (fun o f ->
			match o with
			| VObject o -> VBool (Hashtbl.mem o.ofields (vstring f))
			| _ -> VBool false
		);
		"objremove", Fun2 (fun o f ->
			let o = vobj o in
			let f =  vstring f in
			if Hashtbl.mem o.ofields f then begin
				Hashtbl.remove o.ofields f;
				VBool true
			end else
				VBool false
		);
		"objfields", Fun1 (fun o ->
			let fl = Hashtbl.fold (fun f _ acc -> VString f :: acc) (vobj o).ofields [] in
			VArray (Array.of_list fl)
		);
		"hash", Fun1 (fun v -> VString (String.copy (vstring v)));
		"field", Fun1 (fun v -> VString (vstring v));
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
			| _ -> exc (VString "Invalid closure arguments number")
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
			| VInt i -> v
			| VFloat f -> VInt (to_int f)
			| VString s -> (try VInt (parse_int s) with _ -> VNull)
			| _ -> VNull
		);
		"float", Fun1 (fun v ->
			match v with
			| VInt i -> VFloat (float_of_int i)
			| VFloat _ -> v
			| VString s -> (try VFloat (parse_float s) with _ -> VNull)
			| _ -> VNull
		);
	(* abstract *)
		"getkind", Fun1 (fun v ->
			match v with
			| VAbstract a -> VAbstract (AKind a)
			| _ -> error()
		);
		"iskind", Fun2 (fun v k ->
			match v, k with
			| VAbstract a, VAbstract (AKind k) ->
				VBool (match a, k with
				| AKind _, AKind _ -> true
				| AInt32 _, AInt32 _ -> true
				| AHash _, AHash _ -> true
				| ARandom _, ARandom _ -> true
				| ABuffer _, ABuffer _ -> true
				| _ -> false)
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
		"hadd", Fun4 (fun h k v cmp ->
			if cmp <> VNull then assert false;
			let h = vhash h in
			let old = Hashtbl.mem h k in
			Hashtbl.add h k v;
			VBool (not old);
		);
		"hiter", Fun2 (fun h f -> Hashtbl.iter (fun k v -> ignore ((get_ctx()).do_call VNull f [k;v] p)) (vhash h); VNull);
		"hcount", Fun1 (fun h -> VInt (Hashtbl.length (vhash h)));
		"hsize", Fun1 (fun h -> VInt (Hashtbl.length (vhash h)));
	(* misc *)
		"print", FunVar (fun vl -> List.iter (fun v -> print_string ((get_ctx()).do_string v)) vl; VNull);
		"throw", Fun1 (fun v -> exc v);
		"rethrow", Fun1 (fun v -> exc v);
		"istrue", Fun1 (fun v ->
			match v with
			| VNull | VInt 0 | VBool false -> VBool false
			| _ -> VBool true
		);
		"not", Fun1 (fun v ->
			match v with
			| VNull | VInt 0 | VBool false -> VBool true
			| _ -> VBool false
		);
		"typeof", Fun1 (fun v ->
			VInt (match v with
			| VNull -> 0
			| VInt _ -> 1
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
	 		build_stack (get_ctx()).stack
	 	);
	 	"version", Fun0 (fun() ->
	 		VInt 0
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
	let loader = obj [
		"args",VArray [||];
		"loadprim",VFunction (Fun2 (fun a b -> (get_ctx()).do_loadprim a b));
		"loadmodule",VFunction (Fun2 (fun a b -> assert false));
	] in
	Hashtbl.add h "loader" (VObject loader);
	Hashtbl.add h "exports" (VObject { ofields = Hashtbl.create 0; oproto = None });
	h

let std_lib =
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
		| VFloat f -> f
		| _ -> error()
	in
	let make_date f =
		VAbstract (AInt32 (Int32.of_float f))
	in
	let date = function
		| VAbstract (AInt32 i) -> Int32.to_float i
		| VInt i -> float_of_int i
		| _ -> error()
	in
	let make_i32 i =
		VAbstract (AInt32 i)
	in
	let int32 = function
		| VInt i -> Int32.of_int i
		| VAbstract (AInt32 i) -> i
		| _ -> error()
	in
	let int32_op op = Fun2 (fun a b -> make_i32 (op (int32 a) (int32 b))) in
	let funcs = [
	(* math *)
		"math_atan2", Fun2 (fun a b -> VFloat (atan2 (num a) (num b)));
		"math_pow", Fun2 (fun a b -> VFloat ((num a) ** (num b)));
		"math_abs", Fun1 (fun v ->
			match v with
			| VInt i -> VInt (abs i)
			| VFloat f -> VFloat (abs_float f)
			| _ -> error()
		);
		"math_ceil", Fun1 (fun v -> VInt (to_int (ceil (num v))));
		"math_floor", Fun1 (fun v -> VInt (to_int (floor (num v))));
		"math_round", Fun1 (fun v -> VInt (to_int (floor (num v +. 0.5))));
		"math_pi", Fun0 (fun() -> VFloat (4.0 *. atan 1.0));
		"math_sqrt", Fun1 (fun v -> VFloat (sqrt (num v)));
		"math_atan", Fun1 (fun v -> VFloat (atan (num v)));
		"math_cos", Fun1 (fun v -> VFloat (cos (num v)));
		"math_sin", Fun1 (fun v -> VFloat (sin (num v)));
		"math_tan", Fun1 (fun v -> VFloat (tan (num v)));
		"math_log", Fun1 (fun v -> VFloat (log (num v)));
		"math_exp", Fun1 (fun v -> VFloat (exp (num v)));
		"math_acos", Fun1 (fun v -> VFloat (acos (num v)));
		"math_asin", Fun1 (fun v -> VFloat (asin (num v)));
		"math_fceil", Fun1 (fun v -> VFloat (ceil (num v)));
		"math_ffloor", Fun1 (fun v -> VFloat (floor (num v)));
		"math_fround", Fun1 (fun v -> VFloat (floor (num v +. 0.5)));
		"math_int", Fun1 (fun v ->
			match v with
			| VInt n -> v
			| VFloat f -> VInt (to_int (if f < 0. then ceil f else floor f))
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
			match h, m, s with
			| VInt h, VInt m, VInt s ->
				let t = Unix.localtime d in
				make_date (fst (Unix.mktime { t with tm_hour = h; tm_min = m; tm_sec = s }))
			| _ -> error()
		);
		"date_set_day", Fun4 (fun d y m da ->
			let d = date d in
			match y, m, da with
			| VInt y, VInt m, VInt da ->
				let t = Unix.localtime d in
				make_date (fst (Unix.mktime { t with tm_year = y - 1900; tm_mon = m - 1; tm_mday = da }))
			| _ -> error()
		);
		"date_format", Fun2 (fun d fmt ->
			match fmt with
			| VNull ->
				let t = Unix.localtime (date d) in
				VString (Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec)
			| VString _ ->
				exc (VString "Custom date format is not supported") (* use native haXe implementation *)
			| _ ->
				error()
		);
		"date_get_hour", Fun1 (fun d ->
			let t = Unix.localtime (date d) in
			let o = obj [
				"h", VInt t.tm_hour;
				"m", VInt t.tm_min;
				"s", VInt t.tm_sec;
			] in
			VObject o
		);
		"date_get_day", Fun1 (fun d ->
			let t = Unix.localtime (date d) in
			let o = obj [
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
			match s with
			| VString s ->
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
			| _ -> error()
		);
		"url_decode", Fun1 (fun s ->
			match s with
			| VString s ->
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
			| _ -> error()
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
			match s, b with
			| VString s, VString b ->
				if String.length b <> 64 then assert false;
				let tbl = Array.init 64 (String.unsafe_get b) in
				VString (Base64.str_decode ~tbl:(Base64.make_decoding_table tbl) s)
			| _ -> error()
		);
		"make_md5", Fun1 (fun s ->
			match s with
			| VString s -> VString (Digest.string s)
			| _ -> error()
		);
		(* sprintf *)
	(* int32 *)
		"int32_new", Fun1 (fun v ->
			match v with
			| VAbstract (AInt32 i) -> v
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
			match f, big with
			| VFloat f, VBool big ->
				let ch = IO.output_string() in
				if big then IO.BigEndian.write_double ch f else IO.write_double ch f;
				VString (IO.close_out ch)
			| _ ->
				error()
		);
		"float_bytes", Fun2 (fun f big ->
			match f, big with
			| VFloat f, VBool big ->
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
		"random_new", Fun0 (fun() -> VAbstract (ARandom (Random.State.make_self_init())));
		(* TODO *)
	(* file *)
		(* TODO *)
	(* serialize *)
		(* TODO *)
	(* socket *)
		(* TODO *)
	(* system *)
		"get_env", Fun1 (fun v ->
			match v with
			| VString s -> (try VString (Sys.getenv s) with _ -> VNull)
			| _ -> error()
		);
		(* TODO *)
	(* utf8 *)
		(* TODO *)
	(* xml *)
		(* TODO *)
	(* process *)
		(* TODO *)
	] in
	let h = Hashtbl.create 0 in
	List.iter (fun (n,f) -> Hashtbl.add h n f) funcs;
	h

let throw ctx p msg =
	ctx.stack <- p :: ctx.stack;
	exc (VString msg)

let local ctx var value =
	ctx.locals <- PMap.add var (ref value) ctx.locals

let get_ident ctx s =
	try
		!(PMap.find s ctx.locals)
	with Not_found -> try
		Hashtbl.find ctx.globals s
	with Not_found ->
		VNull

let rec eval ctx (e,p) =
	match e with
	| EConst c ->
		(match c with
		| True -> VBool true
		| False -> VBool false
		| Null -> VNull
		| This -> ctx.vthis
		| Int i -> VInt i
		| Float f -> VFloat (float_of_string f)
		| String s -> VString s
		| Builtin s -> (try Hashtbl.find builtins s with Not_found -> throw ctx p ("Builtin not found '" ^ s ^ "'"))
		| Ident s -> get_ident ctx s)
	| EBlock el ->
		let rec loop = function
			| [] -> VNull
			| [e] -> eval ctx e
			| e :: l ->
				ignore(eval ctx e);
				loop l
		in
		let old = ctx.locals in
		let v = loop el in
		ctx.locals <- old;
		v
	| EParenthesis e ->
		eval ctx e
	| EField (e,f) ->
		(match eval ctx e with
		| VObject o -> get_field o f
		| _ -> throw ctx p ("Invalid field access : " ^ f))
	| ECall (e,el) ->
		let pl = List.map (eval ctx) el in
		(match fst e with
		| EField (e,f) ->
			let o = eval ctx e in
			let f = (match o with
				| VObject o -> get_field o f
				| _ -> throw ctx p ("Invalid field access : " ^ f)
			) in
			call ctx o f pl p
		| _ ->
			call ctx ctx.vthis (eval ctx e) pl p)
	| EArray (e1,e2) ->
		let index = eval ctx e2 in
		acc_get ctx p (AccArray (eval ctx e1,index));
	| EVars vl ->
		List.iter (fun (v,eo) ->
			let value = (match eo with None -> VNull | Some e -> eval ctx e) in
			local ctx v value
		) vl;
		VNull
	| EWhile (econd,e,NormalWhile) ->
		let rec loop() =
			match eval ctx econd with
			| VBool true ->
				let v = (try
					ignore(eval ctx e); None
				with
					| Continue -> None
					| Break v -> Some v
				) in
				(match v with
				| None -> loop()
				| Some v -> v)
			| _ ->
				VNull
		in
		loop()
	| EWhile (econd,e,DoWhile) ->
		let rec loop() =
			let v = (try
				ignore(eval ctx e); None
			with
				| Continue -> None
				| Break v -> Some v
			) in
			match v with
			| Some v -> v
			| None ->
				match eval ctx econd with
				| VBool true -> loop()
				| _ -> VNull
		in
		loop()
	| EIf (econd,eif,eelse) ->
		(match eval ctx econd with
		| VBool true -> eval ctx eif
		| _ -> match eelse with
			| None -> VNull
			| Some e -> eval ctx e)
	| ETry (e,exc,ecatch) ->
		let locals = ctx.locals in
		let vthis = ctx.vthis in
		let stack = ctx.stack in
		(try
			eval ctx e
		with Runtime v ->
			let rec loop n l =
				if n = 0 then l else
				match l with
				| [] -> []
				| _ :: l -> loop (n - 1) l
			in
			ctx.exc <- loop (List.length stack) (List.rev ctx.stack);
			ctx.stack <- stack;
			ctx.locals <- locals;
			ctx.vthis <- vthis;
			local ctx exc v;
			eval ctx ecatch);
	| EFunction (pl,e) ->
		let locals = ctx.locals in
		VFunction (match pl with
		| [] ->
			Fun0 (fun() ->
				ctx.locals <- locals;
				eval ctx e
			)
		| [a] ->
			Fun1 (fun v ->
				ctx.locals <- locals;
				local ctx a v;
				eval ctx e
			)
		| [a;b] ->
			Fun2 (fun va vb ->
				ctx.locals <- locals;
				local ctx a va;
				local ctx b vb;
				eval ctx e
			)
		| [a;b;c] ->
			Fun3 (fun va vb vc ->
				ctx.locals <- locals;
				local ctx a va;
				local ctx b vb;
				local ctx c vc;
				eval ctx e
			)
		| [a;b;c;d] ->
			Fun4 (fun va vb vc vd ->
				ctx.locals <- locals;
				local ctx a va;
				local ctx b vb;
				local ctx c vc;
				local ctx d vd;
				eval ctx e
			)
		| [a;b;c;d;pe] ->
			Fun5 (fun va vb vc vd ve ->
				ctx.locals <- locals;
				local ctx a va;
				local ctx b vb;
				local ctx c vc;
				local ctx d vd;
				local ctx pe ve;
				eval ctx e
			)
		| pl ->
			FunVar (fun vl ->
				if List.length vl != List.length pl then exc (VString "Invalid call");
				ctx.locals <- locals;
				List.iter2 (local ctx) pl vl;
				eval ctx e
			)
		)
	| EBinop (op,e1,e2) ->
		eval_op ctx op e1 e2 p
	| EReturn None ->
		raise (Return VNull)
	| EReturn (Some e) ->
		raise (Return (eval ctx e))
	| EBreak None ->
		raise (Break VNull)
	| EBreak (Some e) ->
		raise (Break (eval ctx e))
	| EContinue ->
		raise Continue
	| ENext (e1,e2) ->
		ignore(eval ctx e1);
		eval ctx e2
	| EObject fl ->
		let o = {
			ofields = Hashtbl.create 0;
			oproto = None;
		} in
		List.iter (fun (f,e) ->
			Hashtbl.add o.ofields f (eval ctx e)
		) fl;
		VObject o
	| ELabel l ->
		assert false
	| ESwitch (e,el,eo) ->
		let v = eval ctx e in
		let rec loop = function
			| [] ->
				(match eo with
				| None -> VNull
				| Some e -> eval ctx e)
			| (c,e) :: l ->
				if ctx.do_compare v (eval ctx c) = CEq then eval ctx e else loop l
		in
		loop el
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
		let idx = eval ctx eindex in
		let v = eval ctx e in
		AccArray (v,idx)
	| EConst (Ident s) ->
		AccVar s
	| _ ->
		throw ctx p "Invalid assign"

and acc_get ctx p = function
	| AccField (v,f) ->
		(match v with
		| VObject o -> get_field o f
		| _ -> throw ctx p ("Invalid field access : " ^ f))
	| AccArray (e,index) ->
		(match index, e with
		| VInt i, VArray a -> (try Array.get a i with _ -> VNull)
		| _, VObject o ->
			(match eval_oop ctx p o "__get" [index] with
			| None -> throw ctx p "Invalid array access"
			| Some v -> v)
		| _ -> throw ctx p "Invalid array access")
	| AccVar s ->
		get_ident ctx s

and acc_set ctx p acc value =
	match acc with
	| AccField (v,f) ->
		(match v with
		| VObject o -> Hashtbl.replace o.ofields f value; value
		| _ -> throw ctx p ("Invalid field access : " ^ f))
	| AccArray (e,index) ->
		(match index, e with
		| VInt i, VArray a -> (try Array.set a i value; value with _ -> throw ctx p "Invalid array access")
		| _, VObject o ->
			(match eval_oop ctx p o "__set" [index;value] with
			| None -> throw ctx p "Invalid array access"
			| Some _ -> value);
		| _ -> throw ctx p "Invalid array access")
	| AccVar s ->
		(try
			let v = PMap.find s ctx.locals in
			v := value;
		with Not_found ->
			Hashtbl.replace ctx.globals s value);
		value

and number_op ctx p sop iop fop oop rop v1 v2 =
	match v1, v2 with
	| VInt a, VInt b -> VInt (iop a b)
	| VFloat a, VInt b -> VFloat (fop a (float_of_int b))
	| VInt a, VFloat b -> VFloat (fop (float_of_int a) b)
	| VFloat a, VFloat b -> VFloat (fop a b)
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
	match v1, v2 with
	| VInt a, VInt b -> VInt (iop a b)
	| _ -> throw ctx p op

and base_op ctx op v1 v2 p =
	match op with
	| "+" ->
		(match v1, v2 with
		| VInt _, VInt _ | VInt _ , VFloat _ | VFloat _ , VInt _ | VFloat _ , VFloat _ | VObject _ , _ | _ , VObject _ -> number_op ctx p op (+) (+.) "__add" "__radd" v1 v2
		| VString a, _ -> VString (a ^ ctx.do_string v2)
		| _, VString b -> VString (ctx.do_string v1 ^ b)
		| _ -> throw ctx p op)
	| "-" ->
		number_op ctx p op (-) (-.) "__sub" "__rsub" v1 v2
	| "*" ->
		number_op ctx p op ( * ) ( *. ) "__mult" "__rmul" v1 v2
	| "/" ->
		number_op ctx p op (/) (/.) "__div" "__rdiv" v1 v2
	| "%" ->
		number_op ctx p op (mod) mod_float "__mod" "__rmod" v1 v2
	| "&" ->
		int_op ctx p op (land) v1 v2
	| "|" ->
		int_op ctx p op (lor) v1 v2
	| "^" ->
		int_op ctx p op (lxor) v1 v2
	| "<<" ->
		int_op ctx p op (lsl) v1 v2
	| ">>" ->
		int_op ctx p op (asr) v1 v2
	| ">>>" ->
		int_op ctx p op (fun x y ->
			if x >= 0 then x lsr y else Int32.to_int (Int32.shift_right_logical (Int32.of_int x) y)
		) v1 v2
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
		(match ctx.do_compare v1 v2 with
		| CEq -> VBool true
		| _ -> VBool false)
	| "!=" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(match ctx.do_compare v1 v2 with
		| CEq -> VBool false
		| _ -> VBool true)
	| ">" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(match ctx.do_compare v1 v2 with
		| CSup -> VBool true
		| _ -> VBool false)
	| ">=" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(match ctx.do_compare v1 v2 with
		| CSup | CEq -> VBool true
		| _ -> VBool false)
	| "<" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(match ctx.do_compare v1 v2 with
		| CInf -> VBool true
		| _ -> VBool false)
	| "<=" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		(match ctx.do_compare v1 v2 with
		| CInf | CEq -> VBool true
		| _ -> VBool false)
	| "+" | "-" | "*" | "/" | "%" | "|" | "&" | "^" | "<<" | ">>" | ">>>" ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		base_op ctx op v1 v2 p
	| "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | ">>>=" | "|=" | "&=" | "^=" ->
		let acc = eval_access ctx e1 in
		let v1 = acc_get ctx p acc in
		let v2 = eval ctx e2 in
		let v = base_op ctx (String.sub op 0 (String.length op - 1)) v1 v2 p in
		acc_set ctx p acc v
	| "&&" ->
		(match eval ctx e1 with
		| VBool false as v -> v
		| _ -> eval ctx e2)
	| "||" ->
		(match eval ctx e1 with
		| VBool true as v -> v
		| _ -> eval ctx e2)
	| "++=" | "--=" ->
		let acc = eval_access ctx e1 in
		let v1 = acc_get ctx p acc in
		let v2 = eval ctx e2 in
		let v = base_op ctx (String.sub op 0 1) v1 v2 p in
		ignore(acc_set ctx p acc v);
		v1
	| _ ->
		throw ctx p ("Unsupported " ^ op)

and call ctx vthis vfun pl p =
	let oldthis = ctx.vthis in
	let locals = ctx.locals in
	let oldstack = ctx.stack in
	ctx.locals <- PMap.empty;
	ctx.vthis <- vthis;
	ctx.stack <- p :: ctx.stack;
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
			| _ -> exc (VString "Invalid call"))
		| _ ->
			exc (VString "Invalid call"))
	with Return v -> v
		| Builtin_error | Invalid_argument _ -> exc (VString "Invalid call")) in
	ctx.locals <- locals;
	ctx.vthis <- oldthis;
	ctx.stack <- oldstack;
	ret

let rec to_string ctx n v =
	if n > 5 then
		"<...>"
	else let n = n + 1 in
	match v with
	| VNull -> "null"
	| VBool true -> "true"
	| VBool false -> "false"
	| VInt i -> string_of_int i
	| VFloat f -> string_of_float f
	| VString s -> s
	| VArray vl -> "[" ^ String.concat "," (Array.to_list (Array.map (to_string ctx n) vl)) ^ "]"
	| VAbstract a ->
		(match a with
		| AInt32 i -> Int32.to_string i
		| _ -> "#abstract")
	| VFunction f -> "#function:"  ^ string_of_int (nargs f)
	| VClosure _ -> "#function:-1"
	| VObject o ->
		match eval_oop ctx null_pos o "__string" [] with
		| Some (VString s) -> s
		| _ ->
			let b = Buffer.create 0 in
			let first = ref true in
			Buffer.add_char b '{';
			Hashtbl.iter (fun f v ->
				if !first then begin
					Buffer.add_char b ' ';
					first := false;
				end else
					Buffer.add_string b ", ";
				Buffer.add_string b f;
				Buffer.add_string b " => ";
				Buffer.add_string b (to_string ctx n v);
			) o.ofields;
			Buffer.add_string b (if !first then "}" else " }");
			Buffer.contents b

let rec compare ctx a b =
	let fcmp (a:float) b = if a = b then CEq else if a < b then CInf else CSup in
	let scmp (a:string) b = if a = b then CEq else if a < b then CInf else CSup in
	match a, b with
	| VNull, VNull -> CEq
	| VInt a, VInt b -> if a = b then CEq else if a < b then CInf else CSup
	| VFloat a, VFloat b -> fcmp a b
	| VFloat a, VInt b -> fcmp a (float_of_int b)
	| VInt a, VFloat b -> fcmp (float_of_int a) b
	| VBool a, VBool b -> if a = b then CEq else if a then CSup else CInf
	| VString a, VString b -> scmp a b
	| VInt _ , VString s
	| VFloat _ , VString s
	| VBool _ , VString s -> scmp (to_string ctx 0 a) s
	| VString s, VInt _
	| VString s, VFloat _
	| VString s, VBool _ -> scmp s (to_string ctx 0 b)
	| VObject oa, VObject ob ->
		if oa == ob then CEq else
			(match eval_oop ctx null_pos oa "__compare" [b] with
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

let load_prim ctx f n =
	match f, n with
	| VString f, VInt n ->
		let lib, fname = (try ExtString.String.split f "@" with _ -> "", f) in
		(try
			let f = (match lib with
			| "std" -> Hashtbl.find std_lib fname
			| _ -> raise Not_found
			) in
			if nargs f <> n then raise Not_found;
			VFunction f
		with Not_found ->
			VFunction (FunVar (fun _ -> exc (VString ("Primitive not found " ^ f ^ ":" ^ string_of_int n)))))
	| _ ->
		exc (VString "Invalid call")

let create com =
	let ctx = {
		com = com;
		gen = Genneko.new_context com true;
		packages = Hashtbl.create 0;
		types = Hashtbl.create 0;
		globals = Hashtbl.create 0;
		locals = PMap.empty;
		stack = [];
		exc = [];
		vthis = VNull;
		(* api *)
		do_call = Obj.magic();
		do_string = Obj.magic();
		do_loadprim = Obj.magic();
		do_compare = Obj.magic();
	} in
	ctx.do_call <- call ctx;
	ctx.do_string <- to_string ctx 0;
	ctx.do_loadprim <- load_prim ctx;
	ctx.do_compare <- compare ctx;
	select ctx;
	List.iter (fun e -> ignore(eval ctx e)) (Genneko.header());
	ctx

let add_types ctx types =
	let t = Common.timer "macro execution" in
	let packs = List.concat (List.map (Genneko.gen_package ctx.gen ctx.packages) types) in
	let names = List.fold_left (Genneko.gen_name ctx.gen) [] types in
	let methods = List.rev (List.fold_left (fun acc t -> Genneko.gen_type ctx.gen t acc) [] types) in
	let boot = Genneko.gen_boot ctx in
	let inits = List.map (fun (c,e) ->
		ctx.gen.Genneko.curclass <- Ast.s_type_path c.Type.cl_path;
		ctx.gen.Genneko.curmethod <- "__init__";
		Genneko.gen_expr ctx.gen e
	) (List.rev ctx.gen.Genneko.inits) in
	let vars = List.concat (List.map (Genneko.gen_static_vars ctx.gen) types) in
	let e = (EBlock (packs @ methods @ boot :: names @ inits @ vars), null_pos) in
	(try
		ignore(eval ctx e);
	with Runtime v ->
		raise (Error (to_string ctx 0 v,List.map make_pos ctx.stack))
	);
	t();


open Ast

type enum_index =
	| IExpr
	| IBinop
	| IUnop
	| IConst
	| ITParam
	| IType
	| IField

let null f = function
	| None -> VNull
	| Some v -> f v

let encode_pos p =
	VAbstract (APos p)	

let enc_array l = VArray (Array.of_list l)

let enc_obj l = VObject (obj l)

let enc_enum (i:enum_index) tag pl =
	let eindex : int = Obj.magic i in
	enc_array (VInt eindex :: VInt tag :: pl)

let encode_const c =
	let tag, pl = match c with
	| Int s -> 0, [VString s]
	| Float s -> 1, [VString s]
	| String s -> 2, [VString s]
	| Ident s -> 3, [VString s]
	| Type s -> 4, [VString s]
	| Regexp (s,opt) -> 5, [VString s;VString opt]
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

let rec encode_path t =
	enc_obj [
		"pack", enc_array (List.map (fun s -> VString s) t.tpackage);
		"name", VString t.tname;
		"params", enc_array (List.map encode_tparam t.tparams);
		"sub", null (fun s -> VString s) t.tsub;
	]

and encode_tparam = function
	| TPType t -> enc_enum ITParam 0 [encode_type t]
	| TPConst c -> enc_enum ITParam 1 [encode_const c]

and encode_field (f,pub,field,pos) =
	let tag, pl = match field with
		| AFVar t -> 0, [encode_type t]
		| AFProp (t,get,set) -> 1, [encode_type t; VString get; VString set]
		| AFFun (pl,t) -> 2, [enc_array (List.map (fun (n,opt,t) ->
			enc_obj [
				"name", VString n;
				"opt", VBool opt;
				"type", encode_type t
			]
		) pl); encode_type t]
	in
	enc_obj [
		"name",VString f;
		"isPublic",null (fun b -> VBool b) pub;
		"type", enc_enum IField tag pl;
		"pos", encode_pos pos;
	]

and encode_type t =
	let tag, pl = match t with
	| CTPath p ->
		0, [encode_path p]
	| CTFunction (pl,r) ->
		1, [enc_array (List.map encode_type pl);encode_type r]
	| CTAnonymous fl -> 
		2, [enc_array (List.map encode_field fl)]
	| CTParent t -> 
		3, [encode_type t]
	| CTExtend (t,fields) ->
		4, [encode_path t; enc_array (List.map encode_field fields)]
	in
	enc_enum IType tag pl

let encode_expr e =
	let rec loop (e,p) =
		let tag, pl = match e with
			| EConst c ->
				0, [encode_const c]
			| EArray (e1,e2) ->
				1, [loop e1;loop e2]
			| EBinop (op,e1,e2) ->
				2, [encode_binop op;loop e1;loop e2]
			| EField (e,f) ->
				3, [VString f]
			| EType (e,f) ->
				4, [VString f]
			| EParenthesis e ->
				5, [loop e]
			| EObjectDecl fl ->
				6, [enc_array (List.map (fun (f,e) -> enc_obj [
					"field",VString f;
					"expr",loop e;
				]) fl)]
			| EArrayDecl el ->
				7, [enc_array (List.map loop el)]
			| ECall (e,el) ->
				8, [loop e;enc_array (List.map loop el)]
			| ENew (p,el) ->
				9, [encode_path p; enc_array (List.map loop el)]
			| EUnop (op,flag,e) ->
				10, [encode_unop op; VBool (match flag with Prefix -> false | Postfix -> true); loop e]
			| EVars vl ->
				11, [enc_array (List.map (fun (v,t,eo) ->
					enc_obj [
						"name",VString v;
						"ret",null encode_type t;
						"expr",null loop eo;
					]
				) vl)]
			| EFunction f ->
				12, [enc_obj [
					"args", enc_array (List.map (fun (n,opt,t,e) ->
						enc_obj [
							"name", VString n;
							"opt", VBool opt;
							"type", null encode_type t;
							"value", null loop e;
						]
					) f.f_args);
					"ret", null encode_type f.f_type;
					"expr", loop f.f_expr
				]]
			| EBlock el ->
				13, [enc_array (List.map loop el)]
			| EFor (v,e,eloop) ->
				14, [VString v;loop e;loop eloop]
			| EIf (econd,e,eelse) ->
				15, [loop econd;loop e;null loop eelse]
			| EWhile (econd,e,flag) ->
				16, [loop econd;loop e;VBool (match flag with NormalWhile -> true | DoWhile -> false)]
			| ESwitch (e,cases,eopt) ->
				17, [loop e;enc_array (List.map (fun (ecl,e) ->
					enc_obj [
						"values",enc_array (List.map loop ecl);
						"expr",loop e
					]
				) cases);null loop eopt]
			| ETry (e,catches) ->
				18, [loop e;enc_array (List.map (fun (v,t,e) ->
					enc_obj [
						"name",VString v;
						"type",encode_type t;
						"expr",loop e
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
				24, [loop e; null encode_type t]
			| ETernary (econd,e1,e2) ->
				25, [loop econd;loop e1;loop e2]
			| EDisplay _ | EDisplayNew _ ->
				assert false
		in
		enc_obj [
			"pos", encode_pos p;
			"expr", enc_enum IExpr tag pl;
		]
	in
	loop e

