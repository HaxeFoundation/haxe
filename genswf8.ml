(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
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
open Swf
open Ast
open Type

type register =
	| NoReg
	| Reg of int

type context = {
	(* code *)
	opcodes : actions;
	mutable code_pos : int;
	mutable stack_size : int;
	mutable opt_push : bool;
	mutable ident_count : int;

	(* management *)
	packages : (string list,unit) Hashtbl.t;
	idents : (string * bool,int) Hashtbl.t;
	mutable movieclips : module_path list;
	mutable inits : texpr list;
	mutable statics : (tclass * bool * string * texpr) list;
	mutable regs : (string,register) PMap.t;
	mutable reg_count : int;
	mutable reg_max : int;
	mutable fun_stack : int;
	version : int;
	mutable curclass : (string list * string);
	mutable curmethod : string;
	mutable fun_pargs : (int * bool list) list;

	(* loops *)
	mutable cur_block : texpr list;
	mutable breaks : (unit -> unit) list;
	mutable continues : (int -> unit) list;
	mutable loop_stack : int;
	mutable in_loop : bool;
}

let error p = Typer.error "Invalid expression" p
let stack_error p = Typer.error "Stack error" p
let protect_all = ref true
let extern_boot = ref false

(* -------------------------------------------------------------- *)
(* Bytecode Helpers *)

type kind =
	| VarReg of int
	| VarStr
	| VarObj
	| VarClosure

type push_style =
	| VStr of string * bool
	| VInt of int
	| VInt32 of int32
	| VFloat of float
	| VReg of int
	| VThis
	| VNull
	| VSuper

let stack_delta = function
	| APush l -> List.length l
	| ASetReg _ -> 0
	| AAdd | ADivide | ASubtract | AMultiply | AMod | AStringAdd -> -1
	| AAnd | AOr | AXor | AShl | AShr | AAsr -> -1
	| ACompare | AGreater -> -1
	| AEval | ANot | AJump _ | AToInt | AToNumber | AToString | ATry _ | ASwap -> 0
	| ACondJump _ -> -1
	| AEqual | APhysEqual -> -1
	| ANew -> -1 (** only if 0 params **)
	| AObject | AInitArray -> 0 (** calculated outside **)
	| ASet -> -2
	| APop -> -1
	| AFunction _ | AFunction2 _ -> 1
	| ADup -> 1
	| AWith _ -> -1
	| AObjGet -> -1
	| AObjSet -> -3
	| ALocalVar -> -1
	| ALocalAssign -> -2
	| AReturn -> -1
	| AGetURL2 _ -> -2
	| ADeleteObj | AInstanceOf | ACast -> -1
	| AExtends | AImplements -> -2
	| AEnum2 | ATrace | AThrow -> -1
	| AGetTimer -> 1
	| AIncrement | ADecrement | AChr | AOrd | ARandom | ADelete | ATypeOf | ATargetPath -> 0
	| AObjCall | ACall | ANewMethod -> assert false
	| AStringPool _ -> 0
	| op -> failwith ("Unknown stack delta for " ^ (ActionScript.action_string (fun _ -> "") 0 op))

let overflow ctx =
	failwith ("In or near the method " ^ s_type_path ctx.curclass ^ "." ^ ctx.curmethod ^
	" too much code is causing an overflow that can't be handled by the SWF format. " ^
	"Please split your code in several methods so it can be correctly compiled.")

let write ctx op =
	let write b op =
		DynArray.add ctx.opcodes op;
		ctx.code_pos <- ctx.code_pos + 1;
		ctx.stack_size <- ctx.stack_size + stack_delta op;
		ctx.opt_push <- b
	in
	match op with
	| APush l when ctx.opt_push ->
		(match DynArray.last ctx.opcodes with
		| (APush l2) as a ->
			ctx.code_pos <- ctx.code_pos - 1;
			ctx.stack_size <- ctx.stack_size - stack_delta a;
			DynArray.delete_last ctx.opcodes;
			write true (APush (l2 @ l))
		| _ ->
			assert false)
	| APush _ ->
		write true op
	| _ ->
		write false op

let call ctx kind n =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			AObjCall , n + 2
		| VarStr ->
			ACall , n + 1
		| VarClosure | VarObj ->
			AObjCall , n + 2
	) in
	DynArray.add ctx.opcodes op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let new_call ctx kind n  =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			ANewMethod , n + 2
		| VarStr ->
			ANew , n + 1
		| VarClosure | VarObj ->
			ANewMethod , n + 2
	) in
	DynArray.add ctx.opcodes op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let always_protected = function
	| "prototype" | "toString" | "__resolve" | "__constructor__" | "__proto__" | "iterator" -> true
	(*// haxe.PosInfos *)
	| "fileName" | "lineNumber" | "className" | "methodName" | "customParams" -> true
	| s ->
		if String.length s > 1 && s.[0] = '_' && s.[1] != '_' then
			true
		else
			false

let unprotect a = !protect_all || always_protected a

let is_protected_name path ext =
	match path with
	| ["flash"] , "Boot" | ["flash"] , "Lib" -> false
	| "flash" :: _ , _ -> ext
	| [] , "Array" | [] , "Math" | [] , "Date" | [] , "String" -> true
	| _ -> false

let rec is_protected ctx t check_mt =
	match t with
	| TInst (c,_) ->
		let rec loop c =
			is_protected_name c.cl_path c.cl_extern
			|| (check_mt && List.exists (fun (i,_) -> i.cl_path = (["mt"],"Protect")) c.cl_implements)
			|| match c.cl_super with None -> false | Some (c,_) -> loop c
		in
		loop c
	| TMono r ->
		(match !r with
		| None -> assert false
		| Some t -> is_protected ctx t check_mt)
	| TLazy f ->
		is_protected ctx ((!f)()) check_mt
	| TSign (s,_) ->
		(match s.s_static with
		| None -> is_protected ctx s.s_type check_mt
		| Some c -> is_protected_name c.cl_path c.cl_extern)
	| _ -> false

let push ctx items =
	write ctx (APush (List.map (fun i ->
		match i with
		| VStr (str,flag) ->
			let flag = if flag || unprotect str then true else flag in
			let n = (try
				Hashtbl.find ctx.idents (str,flag)
			with Not_found ->
				let n = ctx.ident_count in
				ctx.ident_count <- n + 1;
				Hashtbl.add ctx.idents (str,flag) n;
				n
			) in
			if n <= 0xFF then
				PStack n
			else
				PStack2 n
		| VInt n ->
			PInt (Int32.of_int n)
		| VInt32 n ->
			PInt n
		| VFloat f ->
			PDouble f
		| VThis ->
			PReg 1
		| VNull ->
			PNull
		| VSuper ->
			PReg 2
		| VReg n ->
			PReg n
	) items))

let pop ctx n dec =
	let rec loop n =
		if n <> 0 then begin
			write ctx APop;
			loop (n - 1)
		end;
	in
	if n < 0 then assert false;
	let old_s = ctx.stack_size in
	loop n;
	if not dec then ctx.stack_size <- old_s

let cjmp ctx =
	write ctx (ACondJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.opcodes op_pos (ACondJump delta);
		ctx.opt_push <- false
	)

let jmp ctx =
	write ctx (AJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.opcodes op_pos (AJump delta);
		ctx.opt_push <- false
	)

let pos ctx =
	ctx.opt_push <- false;
	let start_pos = ctx.code_pos in
	(fun ~cond ->
		let delta = start_pos - (ctx.code_pos + 1) in
		write ctx (if cond then ACondJump delta else AJump delta);
	)

let jmp_pos ctx cond =
	write ctx (AJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun pos ->
		let delta = pos - start_pos in
		DynArray.set ctx.opcodes op_pos (if cond then ACondJump delta else AJump delta);
		ctx.opt_push <- false
	)

let setvar ?(retval=false) ctx = function
	| VarReg (-1) -> assert false (** true, false, null **)
	| VarReg n -> write ctx (ASetReg n); if not retval then write ctx APop
	| VarStr
	| VarObj
	| VarClosure as s ->
		if retval then write ctx (ASetReg 0);
		write ctx (if s = VarStr then ASet else AObjSet);
		if retval then push ctx [VReg 0]

let getvar ctx = function
	| VarReg (-1) -> () (** true, false, null **)
	| VarReg n -> push ctx [VReg n]
	| VarStr -> write ctx AEval
	| VarObj -> write ctx AObjGet
	| VarClosure ->
		push ctx [VInt 2; VStr ("@closure",false)];
		call ctx VarStr 2

let gen_path ctx (p,t) is_extern =
	let flag = is_protected_name (p,t) is_extern in
	match p with
	| [] ->
		push ctx [VStr (t,flag)];
		VarStr
	| x :: l ->
		push ctx [VStr (x,flag)];
		write ctx AEval;
		List.iter (fun x ->
			push ctx [VStr (x,flag)];
			write ctx AObjGet;
		) l;
		push ctx [VStr (t,flag)];
		VarObj

let func ctx need_super need_args args =
	if ctx.version = 6 then
		let f = {
			f_name = "";
			Swf.f_args = List.map snd args;
			f_codelen = 0;
		} in
		write ctx (AFunction f);
		let start_pos = ctx.code_pos in
		let old_stack = ctx.fun_stack in
		ctx.fun_stack <- ctx.stack_size;
		(fun() ->
			let delta = ctx.code_pos - start_pos in
			f.f_codelen <- delta;
			let codesize = ActionScript.jump_index_to_size ctx.opcodes (start_pos-1) delta in
			if codesize >= 1 lsl 16 then overflow ctx;
			if ctx.fun_stack <> ctx.stack_size then assert false;
			ctx.fun_stack <- old_stack;
		)
	else
	let default_flags = ThisRegister :: (if need_args then [] else [ArgumentsNoVar]) in
	let f = {
		f2_name = "";
		f2_args = args;
		f2_codelen = 0;
		f2_nregs = 0;
		f2_flags = (if need_super then SuperRegister :: default_flags else SuperNoVar :: default_flags);
	} in
	write ctx (AFunction2 f);
	let start_pos = ctx.code_pos in
	let old_stack = ctx.fun_stack in
	let old_rmax = ctx.reg_max in
	ctx.fun_stack <- ctx.stack_size;
	ctx.reg_max <- ctx.reg_count;
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		f.f2_codelen <- delta;
		f.f2_nregs <- ctx.reg_max + 1;
		let codesize = ActionScript.jump_index_to_size ctx.opcodes (start_pos-1) delta in
		if codesize >= 1 lsl 16 then overflow ctx;
		if ctx.fun_stack <> ctx.stack_size then assert false;
		ctx.fun_stack <- old_stack;
		ctx.reg_max <- old_rmax;
	)

let open_block ctx =
	let old_regs = ctx.regs in
	let old_rcount = ctx.reg_count in
	let old_block = ctx.cur_block in
	(fun() ->
		ctx.regs <- old_regs;
		ctx.reg_count <- old_rcount;
		ctx.cur_block <- old_block;
	)

let begin_loop ctx =
	let old_breaks = ctx.breaks in
	let old_cont = ctx.continues in
	let old_stack = ctx.loop_stack in
	let old_loop = ctx.in_loop in
	ctx.breaks <- [];
	ctx.continues <- [];
	ctx.loop_stack <- ctx.stack_size;
	ctx.in_loop <- true;
	(fun pos ->
		List.iter (fun f -> f()) ctx.breaks;
		List.iter (fun f -> f pos) ctx.continues;
		ctx.breaks <- old_breaks;
		ctx.continues <- old_cont;
		ctx.loop_stack <- old_stack;
		ctx.in_loop <- old_loop;
	)

let alloc_reg ctx =
	ctx.reg_count <- ctx.reg_count + 1;
	if ctx.reg_count > ctx.reg_max then ctx.reg_max <- ctx.reg_count;
	ctx.reg_count

let free_reg ctx r p =
	if r <> ctx.reg_count then stack_error p;
	ctx.reg_count <- ctx.reg_count - 1

(* -------------------------------------------------------------- *)
(* Generation Helpers *)

let define_var ctx v ef exprs =
	if ctx.version = 6 || List.exists (Transform.local_find false v) exprs then begin
		push ctx [VStr (v,false)];
		ctx.regs <- PMap.add v NoReg ctx.regs;
		match ef with
		| None ->
			write ctx ALocalVar
		| Some f ->
			f();
			write ctx ALocalAssign
	end else begin
		let r = alloc_reg ctx in
		ctx.regs <- PMap.add v (Reg r) ctx.regs;
		match ef with
		| None -> ()
		| Some f ->
			f();
			setvar ctx (VarReg r)
	end

let no_value ctx retval =
	(* does not push a null but still increment the stack like if
	   a real value was pushed *)
	if retval then ctx.stack_size <- ctx.stack_size + 1

let gen_try ctx =
	let tdata = {
		tr_style = TryRegister 0;
		tr_trylen = 0;
		tr_catchlen = None;
		tr_finallylen = None;
	} in
	write ctx (ATry tdata);
	let start = ctx.code_pos in
	(fun() ->
		let jump_end = jmp ctx in
		tdata.tr_trylen <- ctx.code_pos - start;
		let start = ctx.code_pos in
		(fun() ->
			if ctx.code_pos <> start then tdata.tr_catchlen <- Some (ctx.code_pos - start);
			jump_end()
		)
	)

(* -------------------------------------------------------------- *)
(* Generation *)

let rec gen_big_string ctx s =
	let len = String.length s in
	let max = 65000 in
	if len <= max then
		write ctx (APush [PString s])
	else begin
		write ctx (APush [PString (String.sub s 0 max)]);
		ctx.opt_push <- false;
		gen_big_string ctx (String.sub s max (len - max));
		write ctx AAdd;
	end

let rec gen_constant ctx c p =
	match c with
	| TInt i -> push ctx [VInt32 i]
	| TFloat s -> push ctx [VFloat (try float_of_string s with _ -> error p)]
	| TString s ->
		if String.contains s '\000' then Typer.error "A String cannot contain \\0 characters" p;
		push ctx [VStr (s,true)]
	| TBool b -> write ctx (APush [PBool b])
	| TNull -> push ctx [VNull]
	| TThis
	| TSuper -> assert false

let access_local ctx s =
	match (try PMap.find s ctx.regs , false with Not_found -> NoReg, true) with
	| NoReg , flag ->
		push ctx [VStr (s,flag)];
		VarStr
	| Reg r , _ ->
		VarReg r

let rec gen_access ctx forcall e =
	match e.eexpr with
	| TConst TSuper ->
		(* for superconstructor *)
		if ctx.version = 6 then begin
			push ctx [VStr ("super",true)];
			VarStr
		end else if forcall then begin
			push ctx [VSuper];
			write ctx (APush [PUndefined]);
			VarObj
		end else
			VarReg 2
	| TConst TThis ->
		if ctx.version = 6 then begin
			push ctx [VStr ("this",true)];
			VarStr
		end else
			VarReg 1
	| TLocal "__arguments__" ->
		push ctx [VStr ("arguments",true)];
		VarStr
	| TLocal s ->
		access_local ctx s
	| TField (e2,f) ->
		gen_expr ctx true e2;
		push ctx [VStr (f,is_protected ctx e2.etype true)];
		(match follow e.etype with
		| TFun _ -> VarClosure
		| _ -> VarObj)
	| TArray (ea,eb) ->
		gen_expr ctx true ea;
		gen_expr ctx true eb;
		VarObj
	| TEnumField (en,f) ->
		getvar ctx (gen_path ctx en.e_path false);
		push ctx [VStr (f,false)];
		(match follow e.etype with
		| TFun _ -> VarClosure
		| _ -> VarObj)
	| TType t ->
		(match t with
		| TClassDecl c -> gen_path ctx c.cl_path c.cl_extern
		| TEnumDecl e -> gen_path ctx e.e_path false
		| TSignatureDecl _ -> assert false)
	| _ ->
		if not forcall then error e.epos;
		gen_expr ctx true e;
		write ctx (APush [PUndefined]);
		VarObj

and gen_try_catch ctx retval e catchs =
	let start_try = gen_try ctx in
	gen_expr ctx retval e;
	let end_try = start_try() in
	let end_throw = ref true in
	let jumps = List.map (fun (name,t,e) ->
		if not !end_throw then
			(fun () -> ())
		else let t = (match follow t with
			| TEnum (e,_) -> Some (TEnumDecl e)
			| TInst (c,_) -> Some (TClassDecl c)
			| TFun _
			| TLazy _
			| TSign _
			| TAnon _ ->
				assert false
			| TMono _
			| TDynamic _ ->
				None
		) in
		let next_catch = (match t with
		| None ->
			end_throw := false;
			(fun() -> ())
		| Some t ->
			getvar ctx (gen_access ctx false (mk (TType t) (mk_mono()) e.epos));
			push ctx [VReg 0; VInt 2; VStr ("@instanceof",false)];
			call ctx VarStr 2;
			write ctx ANot;
			cjmp ctx
		) in
		(* @exc.pop() *)
		push ctx [VInt 0;VStr ("@exc",false)];
		write ctx AEval;
		push ctx [VStr ("pop",true)];
		call ctx VarObj 0;
		write ctx APop;
		let block = open_block ctx in
		define_var ctx name (Some (fun() -> push ctx [VReg 0])) [e];
		gen_expr ctx retval e;
		block();
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		let j = jmp ctx in
		next_catch();
		j
	) catchs in
	if !end_throw && catchs <> [] then begin
		push ctx [VReg 0];
		write ctx AThrow;
	end;
	List.iter (fun j -> j()) jumps;
	end_try()

and gen_switch ctx retval e cases def =
	gen_expr ctx true e;
	let r = alloc_reg ctx in
	write ctx (ASetReg r);
	let rec loop = function
		| [] ->
			write ctx APop;
			[]
		| [(e,x)] ->
			gen_expr ctx true e;
			write ctx AEqual;
			[cjmp ctx,x]
		| (e,x) :: l ->
			gen_expr ctx true e;
			write ctx AEqual;
			let j = cjmp ctx in
			push ctx [VReg r];
			(j,x) :: loop l
	in
	let dispatch = loop cases in
	(match def with
	| None -> if retval then push ctx [VNull]
	| Some e -> gen_expr ctx retval e);
	let jend = jmp ctx in
	let jends = List.map (fun (j,e) ->
		j();
		gen_expr ctx retval e;
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		jmp ctx;
	) dispatch in
	jend();
	free_reg ctx r e.epos;
	List.iter (fun j -> j()) jends

and gen_match ctx retval e cases def =
	gen_expr ctx true e;
	let renum = alloc_reg ctx in
	write ctx (ASetReg renum);
	push ctx [VInt 0];
	write ctx AObjGet;
	let rtag = alloc_reg ctx in
	write ctx (ASetReg rtag);
	let rec loop = function
		| [] ->
			write ctx APop;
			[]
		| [(constr,args,e)] ->
			push ctx [VStr (constr,false)];
			write ctx APhysEqual;
			[cjmp ctx,args,e]
		| (constr,args,e) :: l ->
			push ctx [VStr (constr,false)];
			write ctx APhysEqual;
			let j = cjmp ctx in
			push ctx [VReg rtag];
			(j,args,e) :: loop l
	in
	let dispatch = loop cases in
	free_reg ctx rtag e.epos;
	(match def with
	| None -> if retval then push ctx [VNull]
	| Some e -> gen_expr ctx retval e);
	let jend = jmp ctx in
	let jends = List.map (fun (j,args,e) ->
		let regs = ctx.regs in
		let nregs = ctx.reg_count in
		j();
		let n = ref 0 in
		List.iter (fun (a,t) ->
			incr n;
			match a with
			| None -> ()
			| Some a ->
				define_var ctx a (Some (fun() ->
					push ctx [VReg renum; VInt !n];
					write ctx AObjGet
				)) [e]
		) (match args with None -> [] | Some l -> l);
		gen_expr ctx retval e;
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		ctx.regs <- regs;
		ctx.reg_count <- nregs;
		jmp ctx;
	) dispatch in
	jend();
	free_reg ctx renum e.epos;
	List.iter (fun j -> j()) jends

and gen_binop ctx retval op e1 e2 =
	let gen a =
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx a
	in
	match op with
	| OpAssign ->
		let k = gen_access ctx false e1 in
		gen_expr ctx true e2;
		setvar ~retval ctx k
	| OpAssignOp op ->
		let k = gen_access ctx false e1 in
		gen_binop ctx true op e1 e2;
		setvar ~retval ctx k
	| OpAdd -> gen AAdd
	| OpMult -> gen AMultiply
	| OpDiv -> gen ADivide
	| OpSub -> gen ASubtract
	| OpEq -> gen AEqual
	| OpPhysEq -> gen APhysEqual
	| OpPhysNotEq ->
		gen APhysEqual;
		write ctx ANot
	| OpNotEq ->
		gen AEqual;
		write ctx ANot
	| OpGt -> gen AGreater
	| OpGte ->
		gen ACompare;
		write ctx ANot
	| OpLt -> gen ACompare
	| OpLte ->
		gen AGreater;
		write ctx ANot
	| OpAnd -> gen AAnd
	| OpOr -> gen AOr
	| OpXor -> gen AXor
	| OpBoolAnd ->
		gen_expr ctx true e1;
		write ctx ADup;
		write ctx ANot;
		let jump_end = cjmp ctx in
		write ctx APop;
		gen_expr ctx true e2;
		jump_end()
	| OpBoolOr ->
		gen_expr ctx true e1;
		write ctx ADup;
		let jump_end = cjmp ctx in
		write ctx APop;
		gen_expr ctx true e2;
		jump_end()
	| OpShl -> gen AShl
	| OpShr -> gen AShr
	| OpUShr -> gen AAsr
	| OpMod -> gen AMod
	| OpInterval ->
		(* handled by typer *)
		assert false

and gen_unop ctx retval op flag e =
	match op with
	| Not ->
		gen_expr ctx true e;
		write ctx ANot
	| Neg ->
		push ctx [VInt 0];
		gen_expr ctx true e;
		write ctx ASubtract
	| NegBits ->
		gen_expr ctx true e;
		push ctx [VInt (-1)];
		write ctx AXor
	| Increment
	| Decrement ->
		if retval && flag = Postfix then begin
			let k = gen_access ctx false e in
			getvar ctx k
		end;
		ignore(gen_access ctx false e);
		let k = gen_access ctx false e in
		getvar ctx k;
		write ctx (match op with Increment -> AIncrement | Decrement -> ADecrement | _ -> assert false);
		setvar ~retval:(retval && flag = Prefix) ctx k

and gen_call ctx e el =
	match e.eexpr, el with
	| TLocal "__instanceof__" ,  [e1;e2] ->
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx AInstanceOf
	| TLocal "__typeof__" , [e] ->
		gen_expr ctx true e;
		write ctx ATypeOf
	| TLocal "__delete__" , [e1; e2] ->
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx ADeleteObj
	| TLocal "__random__" , [e] ->
		gen_expr ctx true e;
		write ctx ARandom
	| TLocal "__trace__" , [e] ->
		gen_expr ctx true e;
		write ctx ATrace
	| TLocal "__eval__" , [e] ->
		gen_expr ctx true e;
		write ctx AEval
	| TLocal "__gettimer__", [] ->
		write ctx AGetTimer
	| TLocal "__geturl__" , url :: target :: post ->
		gen_expr ctx true url;
		gen_expr ctx true target;
		write ctx (AGetURL2 (match post with [] -> 0 | [{ eexpr = TConst (TString "GET") }] -> 1 | _ -> 2))
	| TLocal "__new__", e :: el ->
		let nargs = List.length el in
		List.iter (gen_expr ctx true) el;
		push ctx [VInt nargs];
		let k = gen_access ctx true e in
		new_call ctx k nargs
	| TLocal "__keys__", [e] ->
		let r = alloc_reg ctx in
		push ctx [VInt 0; VStr ("Array",true)];
		new_call ctx VarStr 0;
		write ctx (ASetReg r);
		write ctx APop;
		gen_expr ctx true e;
		write ctx AEnum2;
		ctx.stack_size <- ctx.stack_size + 1; (* fake *)
		let loop = pos ctx in
		write ctx (ASetReg 0);
		push ctx [VNull];
		write ctx AEqual;
		let jump_end = cjmp ctx in
		push ctx [VReg 0; VInt 1; VReg r; VStr ("push",true)];
		call ctx VarObj 1;
		write ctx APop;
		loop false;
		jump_end();
		push ctx [VReg r];
		free_reg ctx r e.epos;
	| TLocal "__unprotect__", [{ eexpr = TConst (TString s) }] ->
		push ctx [VStr (s,false)]
	| _ , _ ->
		let nargs = List.length el in
		List.iter (gen_expr ctx true) (List.rev el);
		push ctx [VInt nargs];
		let k = gen_access ctx true e in
		call ctx k nargs


and gen_expr_2 ctx retval e =
	match e.eexpr with
	| TConst TSuper
	| TConst TThis
	| TField _
	| TArray _
	| TLocal _
	| TType _
	| TEnumField _ ->
		getvar ctx (gen_access ctx false e)
	| TConst c ->
		gen_constant ctx c e.epos
	| TParenthesis e ->
		gen_expr ctx retval e
	| TBlock el ->
		let rec loop = function
			| [] ->
				if retval then push ctx [VNull]
			| [e] ->
				ctx.cur_block <- [];
				gen_expr ctx retval e
			| e :: l ->
				ctx.cur_block <- l;
				gen_expr ctx false e;
				loop l
		in
		let b = open_block ctx in
		loop el;
		b()
	| TVars vl ->
		List.iter (fun (v,t,e) ->
			define_var ctx v (match e with None -> None | Some e -> Some (fun() -> gen_expr ctx true e)) ctx.cur_block
		) vl;
		if retval then push ctx [VNull]
	| TArrayDecl el ->
		let nitems = List.length el in
		List.iter (gen_expr ctx true) (List.rev el);
		push ctx [VInt nitems];
		write ctx AInitArray;
		ctx.stack_size <- ctx.stack_size - nitems;
	| TObjectDecl fl ->
		let nfields = List.length fl in
		List.iter (fun (s,v) ->
			push ctx [VStr (s,false)];
			gen_expr ctx true v
		) fl;
		push ctx [VInt nfields];
		write ctx AObject;
		ctx.stack_size <- ctx.stack_size - (nfields * 2);
	| TFunction f ->
		let block = open_block ctx in
		let old_in_loop = ctx.in_loop in
		let reg_super = Transform.local_find true "super" f.tf_expr in
		(* only keep None bindings, for protect *)
		ctx.regs <- PMap.foldi (fun v x acc ->
			match x with
			| NoReg -> PMap.add v x acc
			| Reg _ -> acc
		) ctx.regs PMap.empty;
		ctx.reg_count <- (if reg_super then 2 else 1);
		ctx.in_loop <- false;
		let pargs = ref [] in
		let rargs = List.map (fun (a,_,t) ->
			let no_reg = ctx.version = 6 || Transform.local_find false a f.tf_expr in
			if no_reg then begin
				ctx.regs <- PMap.add a NoReg ctx.regs;
				pargs := unprotect a :: !pargs;
				0 , a
			end else begin
				let r = alloc_reg ctx in
				ctx.regs <- PMap.add a (Reg r) ctx.regs;
				pargs := false :: !pargs;
				r , ""
			end
		) f.tf_args in
		let tf = func ctx reg_super (Transform.local_find true "__arguments__" f.tf_expr) rargs in
		ctx.fun_pargs <- (ctx.code_pos, List.rev !pargs) :: ctx.fun_pargs;
		gen_expr ctx false f.tf_expr;
		ctx.in_loop <- old_in_loop;
		tf();
		block();
	| TIf (cond,e,None) ->
		if retval then assert false;
		gen_expr ctx true cond;
		write ctx ANot;
		let j = cjmp ctx in
		gen_expr ctx retval e;
		j()
	| TIf (cond,e,Some e2) ->
		gen_expr ctx true cond;
		let j = cjmp ctx in
		gen_expr ctx retval e2;
		let jend = jmp ctx in
		j();
		gen_expr ctx retval e;
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		jend()
	| TWhile (cond,e,Ast.NormalWhile) ->
		let loop_end = begin_loop ctx in
		let cont_pos = ctx.code_pos in
		let loop = pos ctx in
		gen_expr ctx true cond;
		write ctx ANot;
		let jend = cjmp ctx in
		gen_expr ctx false e;
		loop false;
		jend();
		loop_end cont_pos
	| TWhile (cond,e,Ast.DoWhile) ->
		let loop_end = begin_loop ctx in
		let p = pos ctx in
		gen_expr ctx false e;
		let cont_pos = ctx.code_pos in
		gen_expr ctx true cond;
		p true;
		loop_end cont_pos
	| TReturn None ->
		pop ctx (ctx.stack_size - ctx.fun_stack) false;
		write ctx (APush [PUndefined]);
		write ctx AReturn;
		no_value ctx retval
	| TReturn (Some e) ->
		pop ctx (ctx.stack_size - ctx.fun_stack) false;
		gen_expr ctx true e;
		write ctx AReturn;
		no_value ctx retval
	| TBreak ->
		pop ctx (ctx.stack_size - ctx.loop_stack) false;
		ctx.breaks <- jmp ctx :: ctx.breaks;
		no_value ctx retval
	| TContinue ->
		pop ctx (ctx.stack_size - ctx.loop_stack) false;
		ctx.continues <- jmp_pos ctx false :: ctx.continues;
		no_value ctx retval
	| TCall (e,el) ->
		gen_call ctx e el
	| TNew (c,_,el) ->
		let nargs = List.length el in
		List.iter (gen_expr ctx true) (List.rev el);
		push ctx [VInt nargs];
		let acc = gen_path ctx c.cl_path c.cl_extern in
		new_call ctx acc nargs
	| TSwitch (e,cases,def) ->
		gen_switch ctx retval e cases def
	| TThrow e ->
		(* call @exc.push(e) *)
		gen_expr ctx true e;
		write ctx (ASetReg 0);
		push ctx [VInt 1; VStr ("@exc",false)];
		write ctx AEval;
		push ctx [VStr ("push",true)];
		call ctx VarObj 1;
		write ctx APop;
		push ctx [VReg 0];
		write ctx AThrow;
		no_value ctx retval
	| TTry (e,catchs) ->
		gen_try_catch ctx retval e catchs
	| TBinop (op,e1,e2) ->
		gen_binop ctx retval op e1 e2
	| TUnop (op,flag,e) ->
		gen_unop ctx retval op flag e
	| TMatch (e,_,cases,def) ->
		gen_match ctx retval e cases def
	| TFor (v,it,e) ->
		gen_expr ctx true it;
		let r = alloc_reg ctx in
		write ctx (ASetReg r);
		write ctx APop;
		let loop_end = begin_loop ctx in
		let cont_pos = ctx.code_pos in
		let j_begin = pos ctx in
		push ctx [VInt 0; VReg r; VStr ("hasNext",false)];
		call ctx VarObj 0;
		write ctx ANot;
		let j_end = cjmp ctx in
		let b = open_block ctx in
		define_var ctx v (Some (fun() ->
			push ctx [VInt 0; VReg r; VStr ("next",false)];
			call ctx VarObj 0;
		)) [e];
		gen_expr ctx false e;
		j_begin false;
		j_end();
		loop_end cont_pos;
		if retval then getvar ctx (access_local ctx v);
		b();
		free_reg ctx r null_pos

and gen_expr ctx retval e =
	let old = ctx.stack_size in
	gen_expr_2 ctx retval e;
	if old <> ctx.stack_size then begin
		if old + 1 <> ctx.stack_size then stack_error e.epos;
		if not retval then write ctx APop;
	end else if retval then stack_error e.epos

let gen_class_static_field ctx c flag f =
	match f.cf_expr with
	| None ->
		push ctx [VReg 0; VStr (f.cf_name,flag); VNull];
		setvar ctx VarObj
	| Some e ->
		let e = Transform.block_vars e in
		match e.eexpr with
		| TFunction _ ->
			push ctx [VReg 0; VStr (f.cf_name,flag)];
			ctx.curmethod <- f.cf_name;
			gen_expr ctx true e;
			setvar ctx VarObj
		| _ ->
			ctx.statics <- (c,flag,f.cf_name,e) :: ctx.statics

let gen_class_static_init ctx (c,flag,name,e) =
	ctx.curclass <- c.cl_path;
	ctx.curmethod <- name;
	getvar ctx (gen_path ctx c.cl_path c.cl_extern);
	push ctx [VStr (name,flag)];
	gen_expr ctx true e;
	setvar ctx VarObj

let gen_class_field ctx f flag =
	push ctx [VReg 1; VStr (f.cf_name,flag)];
	(match f.cf_expr with
	| None ->
		push ctx [VNull]
	| Some e ->
		ctx.curmethod <- f.cf_name;
		gen_expr ctx true (Transform.block_vars e));
	setvar ctx VarObj

let gen_enum_field ctx e f =
	push ctx [VReg 0; VStr (f.ef_name,false)];
	(match follow f.ef_type with
	| TFun (args,r) ->
		ctx.regs <- PMap.empty;
		ctx.reg_count <- 1;
		let rargs = List.map (fun _ -> alloc_reg ctx , "") args in
		let nregs = List.length rargs + 1 in
		let tf = func ctx false false rargs in
		push ctx (List.map (fun (r,_) -> VReg r) (List.rev rargs));
		push ctx [VStr (f.ef_name,false); VInt nregs];
		write ctx AInitArray;
		write ctx ADup;
		push ctx [VStr ("__enum__",false); VThis];
		write ctx AObjSet;
		ctx.stack_size <- ctx.stack_size - nregs;
		write ctx AReturn;
		tf();
	| t ->
		push ctx [VStr (f.ef_name,false); VInt 1];
		write ctx AInitArray;
		ctx.stack_size <- ctx.stack_size - 1;
		write ctx ADup;
		push ctx [VStr ("__enum__",false); VReg 0];
		write ctx AObjSet;
	);
	write ctx AObjSet

let init_name ctx path enum =
	push ctx [VReg 0; VStr ((if enum then "__ename__" else "__name__"),false)];
	let name = fst path @ [snd path] in
	let nitems = List.length name in
	push ctx (List.map (fun s -> VStr (s,false)) (List.rev name));
	push ctx [VInt nitems];
	write ctx AInitArray;
	ctx.stack_size <- ctx.stack_size - nitems;
	setvar ctx VarObj

let gen_package ctx p =
	let rec loop acc = function
		| [] -> ()
		| x :: l ->
			let p = x :: acc in
			if not (Hashtbl.mem ctx.packages p) then begin
				(* create the package and copy the _global one if exists *)
				Hashtbl.add ctx.packages p ();

				(* create the package *)
				let path = (List.rev acc,x) in
				let acc = gen_path ctx path false in
				push ctx [VInt 0; VStr ("Object",true)];
				write ctx ANew;
				write ctx (ASetReg 1);
				setvar ctx acc;

				(* copy the content of the _global package if exists *)
				getvar ctx (gen_path ctx ("_global" :: fst path,snd path) false);
				write ctx (ASetReg 2);
				write ctx AEnum2;
				ctx.stack_size <- ctx.stack_size + 1; (* fake *)
				let back = pos ctx in
				write ctx (ASetReg 0);
				push ctx [VNull];
				write ctx AEqual;
				let jend = cjmp ctx in
				push ctx [VReg 1; VReg 0];
				push ctx [VReg 2; VReg 0];
				write ctx AObjGet;
				write ctx AObjSet;
				back false;
				jend();

				write ctx APop;
			end;
			loop p l
	in
	loop [] p

let gen_type_def ctx t =
	match t with
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e -> ctx.inits <- Transform.block_vars e :: ctx.inits);
		gen_package ctx (fst c.cl_path);
		if c.cl_extern then
			()
		else
		let have_constr = ref false in
		if c.cl_path = (["flash"] , "Boot") then extern_boot := false;
		let acc = gen_path ctx c.cl_path false in
		let rec loop s =
			match s.cl_super with
			| None -> ()
			| Some (s,_) ->
				if s.cl_path = (["flash"],"MovieClip") then
					ctx.movieclips <- c.cl_path :: ctx.movieclips
				else
					loop s
		in
		loop c;
		ctx.curclass <- c.cl_path;
		(match c.cl_constructor with
		| Some { cf_expr = Some e } ->
			have_constr := true;
			ctx.curmethod <- "new";
			gen_expr ctx true (Transform.block_vars e)
		| _ ->
			let f = func ctx true false [] in
			f());
		write ctx (ASetReg 0);
		setvar ctx acc;
		if !have_constr then begin
			push ctx [VReg 0; VStr ("__construct__",false); VReg 0];
			setvar ctx VarObj
		end;
		init_name ctx c.cl_path false;
		push ctx [VReg 0; VStr ("toString",true); VStr ("@class_str",false)];
		write ctx AEval;
		setvar ctx VarObj;
		(match c.cl_super with
		| None ->
			push ctx [VReg 0; VStr ("__super__",false); VNull];
			setvar ctx VarObj
		| Some (csuper,_) ->
			let path = (match csuper.cl_path with (["flash"],x) when csuper.cl_extern -> (["_global"],x) | p -> p) in
			push ctx [VReg 0; VStr ("__super__",false)];
			getvar ctx (gen_path ctx path csuper.cl_extern);
			setvar ctx VarObj;
			if ctx.version = 6 then begin
				(* myclass.prototype.__proto__ = superclass.prototype *)
				push ctx [VReg 0; VStr ("prototype",true)];
				getvar ctx VarObj;
				push ctx [VStr ("__proto__",true)];
				getvar ctx (gen_path ctx path csuper.cl_extern);
				push ctx [VStr ("prototype",true)];
				getvar ctx VarObj;
				setvar ctx VarObj;
				(* myclass.prototype.__constructor__ = superclass *)
				push ctx [VReg 0; VStr ("prototype",true)];
				getvar ctx VarObj;
				push ctx [VStr ("__constructor__",true)];
				getvar ctx (gen_path ctx path csuper.cl_extern);
				setvar ctx VarObj
			end else begin
				push ctx [VReg 0];
				getvar ctx (gen_path ctx path csuper.cl_extern);
				write ctx AExtends;
			end;
		);
		(match c.cl_implements with
		| [] ->
			push ctx [VReg 0; VStr ("__interfaces__",false); VInt 0];
			write ctx AInitArray;
			setvar ctx VarObj;
		| l ->
			let nimpl = List.length l in
			push ctx [VReg 0; VStr ("__interfaces__",false)];
			List.iter (fun (c,_) -> getvar ctx (gen_path ctx c.cl_path c.cl_extern)) l;
			push ctx [VInt nimpl];
			write ctx AInitArray;
			setvar ctx VarObj;
			ctx.stack_size <- ctx.stack_size - nimpl;
			if ctx.version > 6 then begin
				List.iter (fun (c,_) -> getvar ctx (gen_path ctx c.cl_path c.cl_extern)) l;
				push ctx [VInt nimpl; VReg 0];
				write ctx AImplements;
				ctx.stack_size <- ctx.stack_size - nimpl;
			end);
		push ctx [VReg 0; VStr ("prototype",true)];
		getvar ctx VarObj;
		write ctx (ASetReg 1);
		write ctx APop;
		push ctx [VReg 1; VStr ("__class__",false); VReg 0];
		setvar ctx VarObj;
		let flag = is_protected ctx (TInst (c,[])) true in
		List.iter (gen_class_static_field ctx c flag) c.cl_ordered_statics;
		PMap.iter (fun _ f -> gen_class_field ctx f flag) c.cl_fields;
	| TEnumDecl { e_path = ([],"Bool") } ->
		()
	| TEnumDecl e when PMap.is_empty e.e_constrs ->
		()
	| TEnumDecl e ->
		gen_package ctx (fst e.e_path);
		let acc = gen_path ctx e.e_path false in
		push ctx [VInt 0; VStr ("Object",true)];
		write ctx ANew;
		write ctx (ASetReg 0);
		setvar ctx acc;
		init_name ctx e.e_path true;
		PMap.iter (fun _ f -> gen_enum_field ctx e f) e.e_constrs
	| TSignatureDecl _ ->
		()

let gen_boot ctx hres =
	(* r0 = Boot *)
	getvar ctx (gen_path ctx (["flash"],"Boot") (!extern_boot));
	write ctx (ASetReg 0);
	write ctx APop;
	(* r0.__init(eval("this")) *)
	push ctx [VStr ("this",true)];
	write ctx AEval;
	push ctx [VInt 1; VReg 0; VStr ("__init",false)];
	call ctx VarObj 0;
	write ctx APop;
	(* r0.__res = hres *)
	push ctx [VReg 0; VStr ("__res",false)];
	let count = ref 0 in
	Hashtbl.iter (fun name data ->
		if String.contains data '\000' then failwith ("Resource " ^ name ^ " contains \\0 character than can't be used in Flash");
		push ctx [VStr (name,true)];
		gen_big_string ctx data;
		incr count;
	) hres;
	push ctx [VInt (!count)];
	write ctx AObject;
	ctx.stack_size <- ctx.stack_size - (!count * 2);
	write ctx AObjSet

let gen_movieclip ctx m =
	getvar ctx (gen_path ctx m false);
	push ctx [VStr (s_type_path m,true); VInt 2; VStr ("Object",true)];
	write ctx AEval;
	push ctx [VStr ("registerClass",true)];
	call ctx VarObj 2;
	write ctx APop

let to_utf8 str =
	try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code ->
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
			UTF8.Buf.contents b

let convert_header ver (w,h,fps,bg) =
	{
		h_version = ver;
		h_size = {
			rect_nbits = if (max w h) >= 820 then 16 else 15;
			left = 0;
			top = 0;
			right = w * 20;
			bottom = h * 20;
		};
		h_frame_count = 1;
		h_fps = to_float16 fps;
		h_compressed = true;
	} , bg

let default_header ver =
	convert_header ver (400,300,30.,0xFFFFFF)

let generate file ver header infile types hres =
	let ctx = {
		opcodes = DynArray.create();
		code_pos = 0;
		stack_size = 0;
		ident_count = 0;
		opt_push = false;
		idents = Hashtbl.create 0;
		packages = Hashtbl.create 0;
		regs = PMap.empty;
		reg_count = 0;
		reg_max = 0;
		cur_block = [];
		breaks = [];
		continues = [];
		loop_stack = 0;
		fun_stack = 0;
		statics = [];
		movieclips = [];
		inits = [];
		version = ver;
		curclass = ([],"");
		curmethod = "";
		fun_pargs = [];
		in_loop = false;
	} in
	write ctx (AStringPool []);
	protect_all := not (Plugin.defined "swf-mark");
	extern_boot := true;
	push ctx [VStr ("@class_str",false)];
	let f = func ctx false false [] in
	push ctx [VStr (".",true); VInt 1];
	if ctx.version = 6 then begin
		push ctx [VStr ("this",true)];
		write ctx AEval;
	end else
		push ctx [VThis];
	push ctx [VStr ("__name__",false)];
	getvar ctx VarObj;
	push ctx [VStr ("join",true)];
	call ctx VarObj 1;
	write ctx AReturn;
	ctx.reg_max <- ctx.reg_max + 1;
	f();
	write ctx ASet;
	List.iter (fun t -> gen_type_def ctx t) types;
	gen_boot ctx hres;
	List.iter (fun m -> gen_movieclip ctx m) ctx.movieclips;
	let global_try = gen_try ctx in
	List.iter (gen_expr ctx false) (List.rev ctx.inits);
	List.iter (gen_class_static_init ctx) (List.rev ctx.statics);
	let end_try = global_try() in
	(* flash.Boot.__trace(exc) *)
	push ctx [VStr ("fileName",false); VStr ("(uncaught exception)",true); VInt 1];
	write ctx AObject;
	ctx.stack_size <- ctx.stack_size - 2;
	push ctx [VReg 0; VInt 2];
	getvar ctx (gen_path ctx (["flash"],"Boot") (!extern_boot));
	push ctx [VStr ("__trace",false)];
	call ctx VarObj 2;
	end_try();
	let idents = ctx.idents in
	let idents = Hashtbl.fold (fun ident pos acc -> (ident,pos) :: acc) idents [] in
	let idents = List.sort (fun (_,p1) (_,p2) -> compare p1 p2) idents in
	let pidents = List.map (fun ((_,flag),_) -> flag) idents in
	let idents = AStringPool (List.map (fun ((id,_),_) -> to_utf8 id) idents) in
	if ActionScript.action_length idents >= 1 lsl 16 then failwith "The SWF can't handle more than a total size of 64K of identifers and literal strings. Try reducing this number by using external data files loaded at runtime";
	DynArray.set ctx.opcodes 0 idents;
	let tag ?(ext=false) d = {
		tid = 0;
		textended = ext;
		tdata = d;
	} in
	let tagcode = tag (TDoAction ctx.opcodes) in
	let base_id = ref 0x5000 in
	let tagclips() = List.fold_left (fun acc m ->
		incr base_id;
		tag ~ext:true (TClip { c_id = !base_id; c_frame_count = 1; c_tags = [] }) ::
		tag ~ext:true (TExport [{ exp_id = !base_id; exp_name = s_type_path m }]) ::
		acc
	) [] ctx.movieclips in
	let swf = (match infile with
		| None ->
			let header , bg = (match header with None -> default_header ver | Some h -> convert_header ver h) in
			let tagbg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
			let tagshow = tag TShowFrame in
			(header,tagbg :: tagclips() @ [tagcode;tagshow])
		| Some file ->
			let file = (try Plugin.find_file file with Not_found -> failwith ("File not found : " ^ file)) in
			let ch = IO.input_channel (open_in_bin file) in
			let h, swf = (try Swf.parse ch with _ -> failwith ("The input swf " ^ file ^ " is corrupted")) in
			let header , tagbg = (match header with
				| None -> h , None
				| Some h ->
					let h , bg = convert_header ver h in
					let tagbg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
					h , Some tagbg
			) in
			IO.close_in ch;
			let rec loop = function
				| [] ->
					failwith ("Frame 1 not found in " ^ file)
				| { tdata = TUnknown (0x1A,_) } :: l  (*// PlaceObject2 *)
				| { tdata = TUnknown (0x46,_) } :: l  (*// PlaceObject3 *)
				| { tdata = TPlaceObject2 _ } :: l
				| { tdata = TPlaceObject3 _ } :: l
				| { tdata = TRemoveObject2 _ } :: l
				| { tdata = TRemoveObject _ } :: l when not (Plugin.defined "flash_use_stage") ->
					loop l
				| ({ tdata = TSetBgColor _ } as t) :: l ->
					(match tagbg with
					| None -> t :: loop l
					| Some bg -> bg :: loop l)
				| ({ tdata = TShowFrame } as t) :: l ->
					tagclips() @ tagcode :: t :: l
				| t :: l ->
					(match t.tdata with
					| TExport l ->
						List.iter (fun e ->
							ctx.movieclips <- List.filter (fun x -> s_type_path x <> e.exp_name) ctx.movieclips
						) l
					| _ -> ());
					t :: loop l
			in
			(header , loop swf)
	) in
	if Plugin.defined "swf-mark" then begin
		let ch = IO.output_channel (open_out_bin (Filename.chop_extension file ^ ".mark")) in
		IO.write_i32 ch (List.length ctx.fun_pargs);
		List.iter (fun (id,l) ->
			IO.write_i32 ch id;
			IO.write_i32 ch (List.length l);
			List.iter (fun f -> IO.write_byte ch (if f then 1 else 0)) l;
		) ctx.fun_pargs;
		IO.write_i32 ch (List.length pidents);
		List.iter (fun f -> IO.write_byte ch (if f then 1 else 0)) pidents;
		IO.close_out ch;
	end;
	let ch = IO.output_channel (open_out_bin file) in
	Swf.write ch swf;
	IO.close_out ch

;;
SwfParser.init SwfZip.inflate SwfZip.deflate;
SwfParser.full_parsing := false;
Swf.warnings := false;
