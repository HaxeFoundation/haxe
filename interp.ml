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

let make_pos p =
	{
		Ast.pfile = p.psource;
		Ast.pmin = if p.pline < 0 then 0 else p.pline land 0xFFFF;
		Ast.pmax = if p.pline < 0 then 0 else p.pline lsr 16;
	}

let warn ctx msg p =
	ctx.com.Common.warning msg (make_pos p)

let exc v =
	raise (Runtime v)

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
	let funcs = [
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
		"string", Fun1 (fun v -> VString ((get_ctx()).do_string v));
		"smake", Fun1 (fun l -> VString (String.create (vint  l)));
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
		"new", Fun1 (fun o ->
			match o with
			| VNull -> VObject { ofields = Hashtbl.create 0; oproto = None }
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
		"nargs", Fun1 (fun f ->
			VInt (nargs (vfun f))
		);
		"call", Fun3 (fun f o args ->
			(get_ctx()).do_call o f (Array.to_list (varray args)) p
		);
		"closure", FunVar (fun vl ->
			match vl with
			| f :: obj :: args ->
				let f = vfun f in
				VFunction (FunVar (fun args2 -> (get_ctx()).do_call obj (VFunction f) (args @ args2) p))
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
			| VFunction (FunVar _) | VFunction (Fun1 _) ->
				VFunction (FunVar (fun vl -> (get_ctx()).do_call VNull f [VArray (Array.of_list vl)] p))
			| _ ->
				error()
		);
		(* skip iadd, isub, idiv, imult *)
		"isnan", Fun1 (fun f ->
			match f with
			| VFloat f -> VBool (f <> f)
			| _ -> VBool false
		);
		"isinfinite", Fun1 (fun f ->
			assert false
		);
		"int", Fun1 (fun v ->
			match v with
			| VInt i -> v
			| VFloat f -> VInt (int_of_float f)
			| VString s -> (try VInt (int_of_string s) with _ -> VNull)
			| _ -> VNull
		);
		"float", Fun1 (fun v ->
			match v with
			| VInt i -> VFloat (float_of_int i)
			| VFloat _ -> v
			| VString s -> (try VFloat (float_of_string s) with _ -> VNull)
			| _ -> VNull
		);
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
				| _ -> false)
			| _ -> error()
		);
		"hkey", Fun1 (fun v -> VInt (Hashtbl.hash v));
		"hnew", Fun1 (fun v ->
			VAbstract (AHash (match v with
			| VNull -> Hashtbl.create 0
			| VInt n -> Hashtbl.create n
			| _ -> error()))
		);
		"hresize", Fun1 (fun v -> VNull);
		(* TODO : $h functions *)
		"hiter", Fun2 (fun h f -> Hashtbl.iter (fun v k -> ignore ((get_ctx()).do_call VNull f [v;k] p)) (vhash h); VNull);
		"hcount", Fun1 (fun h -> VInt (Hashtbl.length (vhash h)));
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
			| VFunction _ -> 7
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
	let loader = {
		ofields = Hashtbl.create 0;
		oproto = None;
	} in
	Hashtbl.add loader.ofields "loadprim" (VFunction (Fun2 (fun a b -> (get_ctx()).do_loadprim a b)));
	Hashtbl.add loader.ofields "loadmodule" (VFunction (Fun2 (fun a b -> assert false)));
	Hashtbl.add loader.ofields "args" (VArray [||]);
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
	let funcs = [
		"random_new", Fun0 (fun() -> VAbstract (ARandom (Random.State.make_self_init())));
	(* math *)
		"math_atan2", Fun2 (fun a b -> VFloat (atan2 (num a) (num b)));
		"math_pow", Fun2 (fun a b -> VFloat ((num a) ** (num b)));
		"math_abs", Fun1 (fun v ->
			match v with
			| VInt i -> VInt (abs i)
			| VFloat f -> VFloat (abs_float f)
			| _ -> error()
		);
		"math_ceil", Fun1 (fun v -> VInt (int_of_float (ceil (num v))));
		"math_floor", Fun1 (fun v -> VInt (int_of_float (floor (num v))));
		"math_round", Fun1 (fun v -> VInt (int_of_float (floor (num v +. 0.5))));
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
			| VFloat f -> VInt (int_of_float (if f < 0. then ceil f else floor f))
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
	(* system *)
		"get_env", Fun1 (fun v ->
			match v with
			| VString s -> (try VString (Sys.getenv s) with _ -> VNull)
			| _ -> error()
		);
	(* string *)
		"string_split", Fun2 (fun s d ->
			make_list (match s, d with
			| VString "", VString _ -> [VString ""]
			| VString s, VString "" -> Array.to_list (Array.init (String.length s) (fun i -> VString (String.make 1 (String.get s i))))
			| VString s, VString d -> List.map (fun s -> VString s) (ExtString.String.nsplit s d)
			| _ -> error())
		);
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
		VFunction (match pl with
		| [] -> Fun0 (fun() -> eval ctx e)
		| [a] -> Fun1 (fun v -> local ctx a v; eval ctx e)
		| [a;b] -> Fun2 (fun va vb -> local ctx a va; local ctx b vb; eval ctx e)
		| [a;b;c] ->
			Fun3 (fun va vb vc ->
				local ctx a va;
				local ctx b vb;
				local ctx c vc;
				eval ctx e
			)
		| [a;b;c;d] ->
			Fun4 (fun va vb vc vd ->
				local ctx a va;
				local ctx b vb;
				local ctx c vc;
				local ctx d vd;
				eval ctx e
			)
		| [a;b;c;d;pe] ->
			Fun5 (fun va vb vc vd ve ->
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
			| Some v -> v);
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
		int_op ctx p op (lsr) v1 v2
	| ">>>" ->
		int_op ctx p op (asr) v1 v2
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
	if n > 200 then exc (VString "Stack overflow");
	let n = n + 1 in
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
	

