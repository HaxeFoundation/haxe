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

and vfunction =
	| Fun0 of (unit -> value)
	| Fun1 of (value -> value)
	| Fun2 of (value -> value -> value)
	| Fun3 of (value -> value -> value -> value)
	| Fun4 of (value -> value -> value -> value -> value)
	| Fun5 of (value -> value -> value -> value -> value -> value)
	| FunVar of (value list -> value)

type context = {
	gen : Genneko.context;
	types : (Type.path,bool) Hashtbl.t;
	globals : (string, value) Hashtbl.t;
	mutable locals : (string, value) PMap.t;
	mutable stack : pos list;
	mutable exc : pos list;
	mutable vthis : value;
}

exception Runtime of value
exception Builtin_error

exception Continue
exception Break of value
exception Return of value

let do_call_ref = ref (fun vthis vfun pl p -> assert false)
let do_call (vthis:value) (vfun:value) (pl:value list) (p:pos) : value = (!do_call_ref) vthis vfun pl p

let get_ctx_ref = ref (fun() -> assert false)
let get_ctx() = (!get_ctx_ref)()

type cmp =
	| CEq
	| CSup
	| CInf
	| CUndef

let do_compare a b = assert false

let to_string v =
	assert false

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
		"string", Fun1 (fun v -> VString (to_string v));
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
				do_call o (get_field oo (vstring f)) (Array.to_list (varray pl)) p
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
			do_call o f (Array.to_list (varray args)) p
		);
		"closure", FunVar (fun vl ->
			match vl with
			| f :: obj :: args ->
				let f = vfun f in
				VFunction (FunVar (fun args2 -> do_call obj (VFunction f) (args @ args2) p))
			| _ -> exc (VString "Invalid closure arguments number")
		);
		"apply", FunVar (fun vl ->
			match vl with
			| f :: args ->
				let f = vfun f in
				VFunction (FunVar (fun args2 -> do_call VNull (VFunction f) (args @ args2) p))
			| _ -> exc (VString "Invalid closure arguments number")
		);
		"varargs", Fun1 (fun f ->
			match f with
			| VFunction (FunVar _) | VFunction (Fun1 _) ->
				VFunction (FunVar (fun vl -> do_call VNull f [VArray (Array.of_list vl)] p))
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
		"hiter", Fun2 (fun h f -> Hashtbl.iter (fun v k -> ignore (do_call VNull f [v;k] p)) (vhash h); VNull);
		"hcount", Fun1 (fun h -> VInt (Hashtbl.length (vhash h)));
		"print", FunVar (fun vl -> List.iter (fun v -> print_string (to_string v)) vl; VNull);
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
			match do_compare a b with
			| CUndef -> VNull
			| CEq -> VInt 0
			| CSup -> VInt 1
			| CInf -> VInt (-1)
		);
		"pcompare", Fun2 (fun a b ->
	 		assert false
	 	);
	 	"excstack", Fun0 (fun() ->
	 		assert false
	 	);
	 	"callstack", Fun0 (fun() ->
	 		assert false
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
	h

let throw ctx p msg =
	ctx.stack <- p :: ctx.stack;
	exc (VString msg)

let local ctx var value =
	ctx.locals <- PMap.add var value ctx.locals

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
		| Ident s ->
			(try
				PMap.find s ctx.locals
			with Not_found -> try
				Hashtbl.find ctx.globals s
			with Not_found ->
				VNull))
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
		(match index, eval ctx e1 with
		| VInt i, VArray a -> (try Array.get a i with _ -> VNull)
		| _, VObject o -> oop ctx p index o "__get"
		| _ -> throw ctx p "Invalid array access")
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
		(try
			eval ctx e
		with Runtime v ->
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
		assert false
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
		VNull
	| ESwitch (e,el,eo) ->
		let v = eval ctx e in
		let rec loop = function
			| [] ->
				(match eo with
				| None -> VNull
				| Some e -> eval ctx e)
			| (c,e) :: l ->
				if do_compare v (eval ctx c) = CEq then eval ctx e else loop l
		in
		loop el
	| ENeko _ ->
		throw ctx p "Inline neko code unsupported"

and oop ctx o param field p =
	assert false

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
	with Return v -> v) in
	ctx.locals <- locals;
	ctx.vthis <- oldthis;
	ctx.stack <- oldstack;
	ret

let select ctx =
	get_ctx_ref := (fun() -> ctx);
	do_call_ref := call ctx

let create com =
	let ctx = {
		gen = Genneko.new_context com;
		types = Hashtbl.create 0;
		globals = Hashtbl.create 0;
		locals = PMap.empty;
		stack = [];
		exc = [];
		vthis = VNull;
	} in
	select ctx;
	List.iter (fun e -> ignore(eval ctx e)) (Genneko.header());
	ctx
