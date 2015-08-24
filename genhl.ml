(*
 * Copyright (C)2005-2015 Haxe Foundation
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

(*

	Steps

	- compile Main.fib into bytecode
	- add debug informations (reg names, etc.)
	- compact output bytecode format + dump
	- load in C + interp?
	- jit x86 / x86_64
	- GC
	- complete opcodes
	- FFI / Dynlink
	- pass unit tests (closures etc.)

	Design problems :

	- declaring all regs in each function : easy, but can take too much space ? (what about array before we get a pointer ?)
			using ORegs/OPopRegs for several register spaces is good, but needs tracking wrt to jumps
			what about register allocation ?

	- which regs should be selected for native regs ?
			first ones : might lose some slots because some args are not used
			last ones : will often require to mov args there
			we need them sorted by order of importance ! (# of native regs vary, easier to assign)
			--> each arg needs an index into reg table, IE not direct matching ?

*)

type ttype =
	| TVoid
	| TUI8
	| TI32
	| TF32
	| TF64
	| TBool
	| TFun of ttype list * ttype
	| TAny

(*

only implements what's needed for Haxe ! -- KISS

	| TType
	| TDynamic
	| TAbstract of string
	| TStruct of (string * ttype) list
	| TArray of ttype * int
	| TSlice of ttype
	| TRef of ttype
	| TNull of ttype
	| TNamed of string * ttype

*)

type reg = int
type global = int

type opcode =
	| OMov of reg * reg
	| OInt of reg * int32
	| OFloat of reg * float
	| OBool of reg * bool
	| OAdd of reg * reg * reg
	| OSub of reg * reg * reg
	| OIncr of reg
	| ODecr of reg
	| OCall0 of reg * global
	| OCall1 of reg * global * reg
	| OCall2 of reg * global * reg * reg
	| OCall3 of reg * global * reg * reg * reg
	| OCallN of reg * reg * reg list
	| OGetGlobal of reg * global
	| OSetGlobal of global * reg
	| OEq of reg * reg * reg
	| ONotEq of reg * reg * reg
	| OLt of reg * reg * reg
	| OGte of reg * reg * reg
	| ORet of reg
	| OJTrue of reg * int
	| OJFalse of reg * int
	| OJNull of reg * int
	| OJNotNull of reg * int
	| OJAlways of int
	| OToAny of reg * reg

type fundecl = {
	index : global;
	regs : ttype array;
	code : opcode array;
}

type code = {
	version : int;
	entrypoint : global;
	globals : ttype array;
	functions : fundecl array;
	natives : (string * int) array;
}

type method_context = {
	mregs : ttype DynArray.t;
	mops : opcode DynArray.t;
	mutable hregs : (int, int) PMap.t;
}

type context = {
	com : Common.context;
	mutable hglobals : (string, int) PMap.t;
	cglobals : ttype DynArray.t;
	cfunctions : fundecl DynArray.t;
	cnatives : (string * int) DynArray.t;
	mutable m : method_context;
}

let rec tstr t =
	match t with
	| TVoid -> "void"
	| TUI8 -> "ui8"
	| TI32 -> "i32"
	| TF32 -> "f32"
	| TF64 -> "f64"
	| TBool -> "bool"
	| TAny -> "any"
	| TFun (args,ret) -> "(" ^ String.concat "," (List.map tstr args) ^ "):" ^ tstr ret

let method_context() =
	{
		mregs = DynArray.create();
		mops = DynArray.create();
		hregs = PMap.empty;
	}

let field_name c f =
	s_type_path c.cl_path ^ ":" ^ f.cf_name

let rec to_type t =
	match t with
	| TMono r ->
		(match !r with
		| None -> TAny
		| Some t -> to_type t)
	| TType (t,tl) ->
		to_type (apply_params t.t_params tl t.t_type)
	| TLazy f ->
		to_type (!f())
	| Type.TFun (args, ret) ->
		TFun (List.map (fun (_,_,t) -> to_type t) args, to_type ret)
	| TAnon _ ->
		TAny
	| TDynamic _ ->
		TAny
	| TEnum (e,_) ->
		assert false
	| TInst (c,_) ->
		assert false
	| TAbstract (a,pl) ->
		if Meta.has Meta.CoreType a.a_meta then
			(match a.a_path with
			| [], "Void" -> TVoid
			| [], "Int" -> TI32
			| _ -> failwith ("Unknown core type " ^ s_type_path a.a_path))
		else
			to_type (Abstract.get_underlying_type a pl)

let alloc_global ctx name t =
	try
		PMap.find name ctx.hglobals
	with Not_found ->
		let gid = DynArray.length ctx.cglobals in
		DynArray.add ctx.cglobals (to_type t);
		ctx.hglobals <- PMap.add name gid  ctx.hglobals;
		gid

let alloc_reg ctx v =
	try
		PMap.find v.v_id ctx.m.hregs
	with Not_found ->
		let rid = DynArray.length ctx.m.mregs in
		DynArray.add ctx.m.mregs (to_type v.v_type);
		ctx.m.hregs <- PMap.add v.v_id rid ctx.m.hregs;
		rid

let alloc_tmp ctx t =
	let rid = DynArray.length ctx.m.mregs in
	DynArray.add ctx.m.mregs t;
	rid

let op ctx o =
	DynArray.add ctx.m.mops o

let jump ctx f =
	let pos = DynArray.length ctx.m.mops in
	DynArray.add ctx.m.mops (OJAlways (-1)); (* loop *)
	(fun() -> DynArray.set ctx.m.mops pos (f (DynArray.length ctx.m.mops - pos - 1)))

let rtype ctx r =
	DynArray.get ctx.m.mregs r

let rec eval_expr ctx e =
	match e.eexpr with
	| TConst c ->
		(match c with
		| TInt i ->
			let r = alloc_tmp ctx TI32 in
			op ctx (OInt (r,i));
			r
		| TFloat f ->
			let r = alloc_tmp ctx TF64 in
			op ctx (OFloat (r,float_of_string f));
			r
		| Type.TBool b ->
			let r = alloc_tmp ctx TBool in
			op ctx (OBool (r,b));
			r
		| _ ->
			failwith ("TODO " ^ s_const c))
	| TLocal v ->
		alloc_reg ctx v
	| TReturn None ->
		let r = alloc_tmp ctx TVoid in
		op ctx (ORet r);
		r
	| TReturn (Some e) ->
		let r = eval_expr ctx e in
		op ctx (ORet r);
		alloc_tmp ctx TVoid
	| TParenthesis e ->
		eval_expr ctx e
	| TBlock el ->
		let rec loop = function
			| [e] -> eval_expr ctx e
			| [] -> alloc_tmp ctx TVoid
			| e :: l ->
				ignore(eval_expr ctx e);
				loop l
		in
		loop el
	| TCall (ec,el) ->
		let r = eval_expr ctx ec in
		let el = List.map2 (fun e t -> eval_to ctx e t) el (match rtype ctx r with TFun (args,_) -> args | _ -> assert false) in
		let ret = alloc_tmp ctx (to_type e.etype) in
		op ctx (OCallN (ret, r, el));
		ret
	| TField (f,a) ->
		(match a with
		| FStatic (c,f) ->
			let g = alloc_global ctx (field_name c f) f.cf_type in
			let r = alloc_tmp ctx (to_type f.cf_type) in
			op ctx (OGetGlobal (r,g));
			r
		| _ -> assert false)
	| TObjectDecl o ->
		(* TODO *)
		alloc_tmp ctx TVoid
	| TIf (cond,eif,eelse) ->
		let out = alloc_tmp ctx (to_type e.etype) in
		let r = eval_expr ctx cond in
		let j = jump ctx (fun i -> OJFalse (r,i)) in
		op ctx (OMov (out,eval_expr ctx eif));
		(match eelse with
		| None -> j()
		| Some e ->
			let jexit = jump ctx (fun i -> OJAlways i) in
			j();
			op ctx (OMov (out,eval_expr ctx e));
			jexit());
		out
	| TBinop (bop, e1, e2) ->
		(match bop with
		| OpLte ->
			let r = alloc_tmp ctx TBool in
			let a = eval_expr ctx e1 in
			let b = eval_expr ctx e2 in
			op ctx (OGte (r,b,a));
			r
		| OpAdd ->
			let t = to_type e.etype in
			let r = alloc_tmp ctx t in
			(match t with
			| TI32 ->
				let a = eval_expr ctx e1 in
				let b = eval_expr ctx e2 in
				op ctx (OAdd (r,a,b));
				r
			| _ ->
				assert false)
		| OpSub ->
			let t = to_type e.etype in
			let r = alloc_tmp ctx t in
			(match t with
			| TI32 ->
				let a = eval_expr ctx e1 in
				let b = eval_expr ctx e2 in
				op ctx (OSub (r,a,b));
				r
			| _ ->
				assert false)
		| _ ->
			failwith ("TODO " ^ s_expr (s_type (print_context())) e))
	| _ ->
		failwith ("TODO " ^ s_expr (s_type (print_context())) e)

and eval_to ctx e t =
	let r = eval_expr ctx e in
	cast_to ctx r t

and cast_to ctx r t =
	let rt = rtype ctx r in
	if t = rt then r else
	match rt, t with
	| _ , TAny ->
		let tmp = alloc_tmp ctx TAny in
		op ctx (OToAny (tmp, r));
		tmp
	| _ -> failwith ("Don't know how to cast " ^ tstr rt ^ " to " ^ tstr t)

let make_fun ctx f idx =
	let old = ctx.m in
	ctx.m <- method_context();
	List.iter (fun (v,o) ->
		let r = alloc_reg ctx v in
		match o with
		| None | Some TNull -> ()
		| Some c ->
			op ctx (OJNotNull (r,1));
			match c with
			| TNull | TThis | TSuper -> assert false
			| TInt i -> op ctx (OInt (r, i))
			| TFloat s -> op ctx (OFloat (r, float_of_string s))
			| Type.TBool b -> op ctx (OBool (r, b))
			| TString s -> assert false (* TODO *)
	) f.tf_args;
	ignore(eval_expr ctx f.tf_expr);
	if to_type f.tf_type = TVoid then op ctx (ORet (alloc_tmp ctx TVoid));
	let f = {
		index = idx;
		regs = DynArray.to_array ctx.m.mregs;
		code = DynArray.to_array ctx.m.mops;
	} in
	ctx.m <- old;
	DynArray.add ctx.cfunctions f

let generate_static ctx c f =
	match f.cf_kind with
	| Var v -> assert false
	| Method m ->
		let gid = alloc_global ctx (field_name c f) f.cf_type in
		make_fun ctx (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> assert false) gid

let generate_type ctx t =
	match t with
	| TClassDecl c when c.cl_extern ->
		List.iter (fun f ->
			List.iter (fun (name,args,pos) ->
				match name, args with
				| Meta.Custom ":hlNative", [EConst(String(name)),_] ->
					let g = alloc_global ctx (field_name c f) f.cf_type in
					DynArray.add ctx.cnatives (name,g);
				| _ -> ()
			) f.cf_meta
		) c.cl_ordered_statics
	| TClassDecl c ->
		List.iter (generate_static ctx c) c.cl_ordered_statics
	| TTypeDecl _ ->
		()
	| TAbstractDecl a when a.a_impl = None ->
		()
	| TEnumDecl _ | TAbstractDecl _ ->
		failwith (s_type_path (t_infos t).mt_path)

(* ------------------------------- INTERP --------------------------------------------- *)

type value =
	| VNull
	| VInt of int32
	| VFloat of float
	| VFun of fundecl
	| VBool of bool
	| VAny of value * ttype
	| VNativeFun of (value list -> value)

exception Return of value

let rec default t =
	match t with
	| TVoid | TFun _ | TAny -> VNull
	| TI32 | TUI8 -> VInt Int32.zero
	| TF32 | TF64 -> VFloat 0.
	| TBool -> VBool false

let rec str v =
	match v with
	| VNull -> "null"
	| VInt i -> Int32.to_string i ^ "i"
	| VFloat f -> string_of_float f ^ "f"
	| VFun f -> "fun#" ^ string_of_int f.index
	| VBool b -> if b then "true" else "false"
	| VAny (v,t) -> "any(" ^ str v ^ ":" ^ tstr t ^ ")"
	| VNativeFun _ -> "native"

let ostr o =
	match o with
	| OMov (a,b) -> Printf.sprintf "mov %d,%d" a b
	| OInt (r,i) -> Printf.sprintf "int %d,%ld" r i
	| OFloat (r,f) -> Printf.sprintf "float %d,%f" r f
	| OBool (r,b) -> if b then Printf.sprintf "true %d" r else Printf.sprintf "false %d" r
	| OAdd (r,a,b) -> Printf.sprintf "add %d,%d,%d" r a b
	| OSub (r,a,b) -> Printf.sprintf "sub %d,%d,%d" r a b
	| OIncr r -> Printf.sprintf "incr %d" r
	| ODecr r -> Printf.sprintf "decr %d" r
	| OCall0 (r,g) -> Printf.sprintf "call %d, %d()" r g
	| OCall1 (r,g,a) -> Printf.sprintf "call %d, %d(%d)" r g a
	| OCall2 (r,g,a,b) -> Printf.sprintf "call %d, %d(%d,%d)" r g a b
	| OCall3 (r,g,a,b,c) -> Printf.sprintf "call %d, %d(%d,%d,%d)" r g a b c
	| OCallN (r,g,rl) -> Printf.sprintf "call %d, %d(%s)" r g (String.concat "," (List.map string_of_int rl))
	| OGetGlobal (r,g) -> Printf.sprintf "global %d, %d" r g
	| OSetGlobal (g,r) -> Printf.sprintf "setglobal %d, %d" g r
	| OEq (r,a,b) -> Printf.sprintf "eq %d,%d,%d" r a b
	| ONotEq (r,a,b)  -> Printf.sprintf "noteq %d,%d,%d" r a b
	| OLt (r,a,b) -> Printf.sprintf "lt %d,%d,%d" r a b
	| OGte (r,a,b) -> Printf.sprintf "gte %d,%d,%d" r a b
	| ORet r -> Printf.sprintf "ret %d" r
	| OJTrue (r,d) -> Printf.sprintf "jtrue %d,%d" r d
	| OJFalse (r,d) -> Printf.sprintf "jfalse %d,%d" r d
	| OJNull (r,d) -> Printf.sprintf "jnull %d,%d" r d
	| OJNotNull (r,d) -> Printf.sprintf "jnotnull %d,%d" r d
	| OJAlways d -> Printf.sprintf "jalways %d" d
	| OToAny (r,a) -> Printf.sprintf "toany %d,%d" r a

let interp code =

	let check f =
		let pos = ref 0 in
		let error msg =
			failwith ("In function " ^ string_of_int f.index ^ "@" ^ string_of_int (!pos) ^ " : " ^ msg)
		in
		let targs, tret = (match code.globals.(f.index) with TFun (args,ret) -> args, ret | _ -> assert false) in
		let rtype i = f.regs.(i) in
		let reg r t =
			if rtype r <> t then error ("Register " ^ string_of_int r ^ " should be " ^ tstr t ^ " and not " ^ tstr (rtype r))
		in
		let numeric r =
			match rtype r with
			| TUI8 | TI32 | TF32 | TF64 -> ()
			| _ -> error ("Register " ^ string_of_int r ^ " should be numeric")
		in
		let int r =
			match rtype r with
			| TUI8 | TI32 -> ()
			| _ -> error ("Register " ^ string_of_int r ^ " should be integral")
		in
		let call f args r =
			match code.globals.(f) with
			| TFun (targs, tret) ->
				if List.length args <> List.length targs then assert false;
				List.iter2 reg args targs;
				reg r tret
			| _ -> assert false
		in
		let can_jump delta =
			if !pos + 1 + delta < 0 || !pos + 1 + delta >= Array.length f.code then failwith "Jump outside function bounds";
		in
		List.iteri reg targs;
		Array.iteri (fun i op ->
			pos := i;
			match op with
			| OMov (a,b) ->
				reg a (rtype b)
			| OInt (r,i) ->
				(match rtype r with
				| TUI8 -> if Int32.to_int i < 0 || Int32.to_int i > 0xFF then reg r TI32
				| TI32 -> ()
				| _ -> reg r TI32)
			| OFloat (r,_) ->
				if rtype r <> TF32 then reg r TF64
			| OBool (r,_) ->
				reg r TBool
			| OAdd (r,a,b) ->
				numeric r;
				reg a (rtype r);
				reg b (rtype r);
			| OSub (r,a,b) ->
				numeric r;
				reg a (rtype r);
				reg b (rtype r);
			| OIncr r ->
				int r
			| ODecr r ->
				int r
			| OCall0 (r,f) ->
				call f [] r
			| OCall1 (r, f, a) ->
				call f [a] r
			| OCall2 (r, f, a, b) ->
				call f [a;b] r
			| OCall3 (r, f, a, b, c) ->
				call f [a;b;c] r
			| OCallN (r,f,rl) ->
				(match rtype f with
				| TFun (targs,tret) when List.length targs = List.length rl -> List.iter2 reg rl targs; reg r tret
				| _ -> reg f (TFun(List.map rtype rl,rtype r)))
			| OGetGlobal (r,g) | OSetGlobal (g,r) ->
				reg r code.globals.(g)
			| OEq (r,a,b) | ONotEq (r, a, b) | OLt (r, a, b) | OGte (r, a, b) ->
				reg r TBool;
				reg a (rtype b)
			| ORet r ->
				reg r tret
			| OJTrue (r,delta) | OJFalse (r,delta) ->
				reg r TBool;
				can_jump delta
			| OJNull (r,delta) | OJNotNull (r,delta) ->
				ignore(rtype r);
				can_jump delta
			| OJAlways d ->
				can_jump d
			| OToAny (r,a) ->
				ignore(rtype a);
				reg r TAny
		) f.code
		(* TODO : check that all path correctly initialize NULL values and reach a return *)
	in

	let globals = Array.map default code.globals in

	let rec call f args =
		let regs = Array.map default f.regs in
		List.iteri (fun i v -> regs.(i) <- v) args;
		let pos = ref 0 in
		let rtype i = f.regs.(i) in
		let set r v = Array.unsafe_set regs r v in
		let get r = Array.unsafe_get regs r in
		let global g = Array.unsafe_get globals g in
		let numop iop fop a b =
			match rtype a with
			| TUI8 ->
				(match regs.(a), regs.(b) with
				| VInt a, VInt b -> VInt (Int32.logand (iop a b) 0xFFl)
				| _ -> assert false)
			| TI32 ->
				(match regs.(a), regs.(b) with
				| VInt a, VInt b -> VInt (iop a b)
				| _ -> assert false)
			| TF32 | TF64 ->
				(match regs.(a), regs.(b) with
				| VFloat a, VFloat b -> VFloat (fop a b)
				| _ -> assert false)
			| _ ->
				assert false
		in
		let iunop iop r =
			match rtype r with
			| TUI8 ->
				(match regs.(r) with
				| VInt a -> VInt (Int32.logand (iop a) 0xFFl)
				| _ -> assert false)
			| TI32 ->
				(match regs.(r) with
				| VInt a -> VInt (iop a)
				| _ -> assert false)
			| _ ->
				assert false
		in
		let rec loop() =
			let op = f.code.(!pos) in
			incr pos;
			(match op with
			| OMov (a,b) -> set a (get b)
			| OInt (r,i) -> set r (VInt i)
			| OFloat (r,f) -> set r (VFloat f)
			| OBool (r,b) -> set r (VBool b)
			| OAdd (r,a,b) -> set r (numop Int32.add ( +. ) a b)
			| OSub (r,a,b) -> set r (numop Int32.sub ( -. ) a b)
			| OIncr r -> set r (iunop (fun i -> Int32.add i 1l) r)
			| ODecr r -> set r (iunop (fun i -> Int32.sub i 1l) r)
			| OCall0 (r,f) -> set r (call (match global f with VFun f -> f | _ -> assert false) [])
			| OCall1 (r,f,r1) -> set r (call (match global f with VFun f -> f | _ -> assert false) [get r1])
			| OCall2 (r,f,r1,r2) -> set r (call (match global f with VFun f -> f | _ -> assert false) [get r1;get r2])
			| OCall3 (r,f,r1,r2,r3) -> set r (call (match global f with VFun f -> f | _ -> assert false) [get r1;get r2;get r3])
			| OCallN (r,f,rl) ->
				(match get f with
				| VFun f -> set r (call f (List.map get rl))
				| VNativeFun f -> set r (f (List.map get rl))
				| _ -> assert false)
			| OGetGlobal (r,g) -> set r (global g)
			| OSetGlobal (g,r) -> Array.unsafe_set globals g (get r)
			| OEq (r,a,b) -> set r (VBool (get a = get b))
			| ONotEq (r,a,b) -> set r (VBool (get a <> get b))
			| OGte (r,a,b) -> set r (VBool (get a >= get b))
			| OLt (r,a,b) -> set r (VBool (get a < get b))
			| OJTrue (r,i) -> if get r = VBool true then pos := !pos + i
			| OJFalse (r,i) -> if get r = VBool false then pos := !pos + i
			| ORet r -> raise (Return regs.(r))
			| OJNull (r,i) -> if get r == VNull then pos := !pos + i
			| OJNotNull (r,i) -> if get r != VNull then pos := !pos + i
			| OJAlways i -> pos := !pos + i
			| OToAny (r,a) -> set r (VAny (get a, f.regs.(a)))
			);
			loop()
		in
		try
			loop()
		with
			Return v -> v
	in
	let load_native name =
		match name with
		| "std@log" -> VNativeFun (fun args -> print_endline (str (List.hd args)); VNull);
		| _ -> failwith ("Unresolved native " ^ name)
	in
	Array.iter check code.functions;
	Array.iter (fun f -> globals.(f.index) <- VFun f) code.functions;
	Array.iter (fun (name,idx) -> globals.(idx) <- load_native name) code.natives;
	match code.globals.(code.entrypoint), globals.(code.entrypoint) with
	| TFun ([],_), VFun f -> call f []
	| _ -> assert false

(* --------------------------------------------------------------------------------------------------------------------- *)

let dump code =
	let lines = ref [] in
	let pr s =
		lines := s :: !lines
	in
	pr ("hl v" ^ string_of_int code.version);
	pr (string_of_int (Array.length code.globals) ^ " globals");
	Array.iteri (fun i g ->
		pr ("	@" ^ string_of_int i ^ " : " ^ tstr g);
	) code.globals;
	pr (string_of_int (Array.length code.functions) ^ " functions");
	Array.iter (fun f ->
		pr ("	fun " ^ string_of_int f.index ^ " : " ^ (try tstr code.globals.(f.index) with _ -> "???"));
		Array.iteri (fun i r ->
			pr ("		r" ^ string_of_int i ^ " " ^ tstr r);
		) f.regs;
		Array.iteri (fun i o ->
			pr ("		@"  ^ string_of_int i ^ " " ^ ostr o);
		) f.code;
	) code.functions;
	pr (string_of_int (Array.length code.natives) ^ " natives");
	Array.iter (fun (name,index) ->
		pr ("	native " ^ name ^ " @" ^ string_of_int index ^ " : " ^ (try tstr code.globals.(index) with _ -> "???"));
	) code.natives;
	pr ("entry @" ^ string_of_int code.entrypoint);
	String.concat "\n" (List.rev !lines)


(* --------------------------------------------------------------------------------------------------------------------- *)

let generate com =
	let ctx = {
		com = com;
		m = method_context();
		cglobals = DynArray.create();
		cfunctions = DynArray.create();
		cnatives = DynArray.create();
		hglobals = PMap.empty;
	} in
	List.iter (generate_type ctx) com.types;
	let ep = (match com.main_class with
		| None -> assert false (* TODO *)
		| Some c ->
			alloc_global ctx (s_type_path c ^ ":" ^ "main") t_dynamic
	) in
	let code = {
		version = 1;
		entrypoint = ep;
		globals = DynArray.to_array ctx.cglobals;
		functions = DynArray.to_array ctx.cfunctions;
		natives = DynArray.to_array ctx.cnatives;
	} in
	prerr_endline (dump code);
	ignore(interp code)

