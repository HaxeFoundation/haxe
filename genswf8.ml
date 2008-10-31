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
open Common

type register =
	| NoReg
	| Reg of int

type context = {

	stack : Codegen.stack_context;

	(* segs *)
	mutable segs : (actions * (string * bool, int) Hashtbl.t) list;

	(* code *)
	mutable opcodes : actions;
	mutable code_pos : int;
	mutable stack_size : int;
	mutable opt_push : bool;
	mutable ident_count : int;
	mutable ident_size : int;

	(* management *)
	com : Common.context;
	packages : (string list,unit) Hashtbl.t;
	flash6 : bool;
	mutable idents : (string * bool,int) Hashtbl.t;
	mutable movieclips : path list;
	mutable inits : texpr list;
	mutable statics : (tclass * bool * string * texpr) list;
	mutable regs : (string,register) PMap.t;
	mutable reg_count : int;
	mutable reg_max : int;
	mutable fun_stack : int;
	mutable curclass : tclass;
	mutable curmethod : (string * bool);
	mutable fun_pargs : (int * bool list) list;
	mutable static_init : bool;

	(* loops *)
	mutable cur_block : texpr list;
	mutable breaks : (unit -> unit) list;
	mutable continues : (int -> unit) list;
	mutable loop_stack : int;
	mutable in_loop : bool;
}

let invalid_expr p = error "Invalid expression" p
let stack_error p = error "Stack error" p
let protect_all = ref true
let extern_boot = ref false
let debug_pass = ref ""

(* -------------------------------------------------------------- *)
(* Bytecode Helpers *)

type tmp_variable =
	| TmpReg of int
	| TmpVar of string * int

type kind =
	| VarReg of int
	| VarStr
	| VarObj
	| VarClosure
	| VarVolatile

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
	| AFSCommand2 -> 0
	| op -> failwith ("Unknown stack delta for " ^ (ActionScript.action_string (fun _ -> "") 0 op))

let overflow ctx =
	failwith ("In or near the method " ^ s_type_path ctx.curclass.cl_path ^ "." ^ fst ctx.curmethod ^
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
		| VarVolatile ->
			assert false
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
		| VarVolatile ->
			assert false
	) in
	DynArray.add ctx.opcodes op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let unprotect a = !protect_all || a = "" || a = "_" || (a.[0] = '_' && a.[1] != '_')

let rec is_protected_path path ext =
	match path with
	| ["flash"] , "Boot" | ["flash"] , "Lib" -> false
	| "flash" :: _ , _ | [] , "flash" -> ext
	| [] , "Array" | [] , "Math" | [] , "Date" | [] , "String" | [] , "Bool" -> true
	| [] , "Int" | [] , "Float" -> true
	| "_global" :: l , n -> is_protected_path (l,n) ext
	| _ -> false

let rec is_protected ctx ?(stat=false) t field =
	match follow t with
	| TInst (c,_) ->
		let rec loop c =
			(is_protected_path c.cl_path c.cl_extern && PMap.mem field (if stat then c.cl_statics else c.cl_fields))
			|| List.exists (fun (i,_) -> i.cl_path = (["mt"],"Protect") || (not stat && loop i)) c.cl_implements
			|| (not stat && match c.cl_super with None -> false | Some (c,_) -> loop c)
		in
		loop c
	| TAnon a ->
		(match !(a.a_status) with
		| Statics c -> is_protected ctx ~stat:true (TInst (c,[])) field
		| _ -> false)
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
				ctx.ident_size <- ctx.ident_size + 1 + String.length str;
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

let init_array ctx n =
	push ctx [VInt n];
	write ctx AInitArray;
	ctx.stack_size <- ctx.stack_size - n

let setvar ?(retval=false) ctx = function
	| VarReg (-1) -> assert false (** true, false, null **)
	| VarReg n -> write ctx (ASetReg n); if not retval then write ctx APop
	| VarStr
	| VarObj
	| VarClosure as s ->
		if retval then write ctx (ASetReg 0);
		write ctx (if s = VarStr then ASet else AObjSet);
		if retval then push ctx [VReg 0]
	| VarVolatile ->
		if retval then write ctx (ASetReg 0);
		init_array ctx 1;
		write ctx AObjSet;
		if retval then push ctx [VReg 0]

let getvar ctx = function
	| VarReg (-1) -> () (** true, false, null **)
	| VarReg n -> push ctx [VReg n]
	| VarStr -> write ctx AEval
	| VarObj -> write ctx AObjGet
	| VarClosure ->
		push ctx [VInt 2; VStr ("@closure",false)];
		call ctx VarStr 2
	| VarVolatile ->
		write ctx AObjGet;
		push ctx [VInt 0];
		write ctx AObjGet

let gen_path ctx ?(protect=false) (p,t) is_extern =
	let flag = is_protected_path (p,t) is_extern in
	match p with
	| [] ->
		push ctx [VStr (t,flag)];
		VarStr
	| x :: l ->
		push ctx [VStr (x,protect && flag)];
		write ctx AEval;
		List.iter (fun x ->
			push ctx [VStr (x,protect && flag)];
			write ctx AObjGet;
		) l;
		push ctx [VStr (t,flag)];
		VarObj

let begin_func ctx need_super need_args args =
	if ctx.flash6 then
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
	let old_sinit = ctx.static_init in
	ctx.fun_stack <- ctx.stack_size;
	ctx.reg_max <- ctx.reg_count;
	ctx.static_init <- false;
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		f.f2_codelen <- delta;
		f.f2_nregs <- ctx.reg_max + 1;
		let codesize = ActionScript.jump_index_to_size ctx.opcodes (start_pos-1) delta in
		if codesize >= 1 lsl 16 then overflow ctx;
		if ctx.fun_stack <> ctx.stack_size then assert false;
		ctx.fun_stack <- old_stack;
		ctx.reg_max <- old_rmax;
		ctx.static_init <- old_sinit;
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

let segment ctx =
	ctx.segs <- (ctx.opcodes,ctx.idents) :: ctx.segs;
	ctx.opcodes <- DynArray.create();
	ctx.idents <- Hashtbl.create 0;
	ctx.ident_count <- 0;
	ctx.ident_size <- 0;
	ctx.code_pos <- 0;
	write ctx (AStringPool [])

(* -------------------------------------------------------------- *)
(* Generation Helpers *)

let define_var ctx v ef exprs =
	if ctx.flash6 || List.exists (Codegen.local_find false v) exprs || ctx.static_init then begin
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

let alloc_tmp ctx =
	let r = alloc_reg ctx in
	if ctx.flash6 then
		let name = "$" ^ string_of_int r in
		define_var ctx name None [];
		TmpVar (name,r);
	else
		TmpReg r

let get_tmp ctx = function
	| TmpVar (v,_) ->
		push ctx [VStr (v,false)];
		write ctx AEval;
	| TmpReg r ->
		push ctx [VReg r]

let set_tmp ctx = function
	| TmpVar (v,_) ->
		write ctx ADup;
		push ctx [VStr (v,false)];
		write ctx ASwap;
		write ctx ASet
	| TmpReg r ->
		write ctx (ASetReg r)

let free_tmp ctx v p =
	match v with
	| TmpVar (v,r) ->
		ctx.regs <- PMap.remove v ctx.regs;
		free_reg ctx r p
	| TmpReg r ->
		free_reg ctx r p

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
	ctx.opt_push <- false;
	if len <= max then
		write ctx (APush [PString s])
	else begin
		write ctx (APush [PString (String.sub s 0 max)]);
		gen_big_string ctx (String.sub s max (len - max));
		write ctx AAdd;
	end

let rec gen_constant ctx c p =
	match c with
	| TInt i -> push ctx [VInt32 i]
	| TFloat s -> push ctx [VFloat (try float_of_string s with _ -> invalid_expr p)]
	| TString s ->
		if String.contains s '\000' then error "A String cannot contain \\0 characters" p;
		push ctx [VStr (s,true)]
	| TBool b -> write ctx (APush [PBool b])
	| TNull -> push ctx [VNull]
	| TThis
	| TSuper -> assert false

let access_local ctx s =
	match (try PMap.find s ctx.regs , false with Not_found -> NoReg, s <> "Enum") with
	| NoReg , flag ->
		push ctx [VStr (s,flag)];
		VarStr
	| Reg r , _ ->
		VarReg r

let rec gen_access ?(read_write=false) ctx forcall e =
	match e.eexpr with
	| TConst TSuper ->
		(* for superconstructor *)
		if ctx.flash6 then begin
			push ctx [VStr ("super",true)];
			VarStr
		end else if forcall then begin
			push ctx [VSuper];
			write ctx (APush [PUndefined]);
			VarObj
		end else
			VarReg 2
	| TConst TThis ->
		if ctx.flash6 then begin
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
		if read_write then write ctx ADup;
		let p = VStr (f,is_protected ctx e2.etype f) in
		push ctx [p];
		if read_write then begin
			write ctx ASwap;
			push ctx [p];
		end;
		(match follow e.etype with
		| TFun _ -> VarClosure
		| _ ->
			if not !protect_all && Codegen.is_volatile e.etype then
				VarVolatile
			else
				VarObj)
	| TArray (ea,eb) ->
		if read_write then 
			try 
				let r = (match ea.eexpr with TLocal l -> (match PMap.find l ctx.regs with Reg r -> r | _ -> raise Not_found) | _ -> raise Not_found) in
				push ctx [VReg r];
				gen_expr ctx true eb;
				write ctx ADup;
				push ctx [VReg r];
				write ctx ASwap;
			with Not_found ->
				gen_expr ctx true eb;
				gen_expr ctx true ea;
				write ctx (ASetReg 0);
				write ctx ASwap;
				write ctx ADup;
				push ctx [VReg 0];
				write ctx ASwap;
		else begin
			gen_expr ctx true ea;
			gen_expr ctx true eb;
		end;
		VarObj
	| TEnumField (en,f) ->
		getvar ctx (gen_path ctx en.e_path false);
		push ctx [VStr (f,false)];
		(match follow e.etype with
		| TFun _ -> VarClosure
		| _ -> VarObj)
	| TTypeExpr t ->
		(match t with
		| TClassDecl c -> gen_path ctx c.cl_path c.cl_extern
		| TEnumDecl e -> gen_path ctx e.e_path false
		| TTypeDecl _ -> assert false)
	| _ ->
		if not forcall then invalid_expr e.epos;
		gen_expr ctx true e;
		write ctx (APush [PUndefined]);
		VarObj

and gen_access_rw ctx e =
	match e.eexpr with
	| TField ({ eexpr = TLocal _ },_) | TArray ({ eexpr = TLocal _ },{ eexpr = TConst _ }) | TArray ({ eexpr = TLocal _ },{ eexpr = TLocal _ }) ->
		ignore(gen_access ctx false e);
		gen_access ctx false e		
	| TField _ | TArray _ ->
		gen_access ~read_write:true ctx false e
	| _ ->
		ignore(gen_access ctx false e);
		gen_access ctx false e

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
			| TType _
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
			getvar ctx (gen_access ctx false (mk (TTypeExpr t) (mk_mono()) e.epos));
			push ctx [VReg 0; VInt 2; VStr ("@instanceof",false)];
			call ctx VarStr 2;
			write ctx ANot;
			cjmp ctx
		) in
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
	let r = alloc_tmp ctx in
	set_tmp ctx r;
	let first = ref true in
	let dispatch = List.map (fun (el,x) ->
		List.map (fun e ->
			if !first then first := false else get_tmp ctx r;
			gen_expr ctx true e;
			write ctx AEqual;
			cjmp ctx
		) el , x
	) cases in
	if !first then write ctx APop;
	(match def with
	| None -> if retval then push ctx [VNull]
	| Some e -> gen_expr ctx retval e);
	let jend = jmp ctx in
	let jends = List.map (fun (jl,e) ->
		List.iter (fun j -> j()) jl;
		gen_expr ctx retval e;
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		jmp ctx;
	) dispatch in
	jend();
	free_tmp ctx r e.epos;
	List.iter (fun j -> j()) jends

and gen_match ctx retval e cases def =
	gen_expr ctx true e;
	let renum = alloc_tmp ctx in
	set_tmp ctx renum;
	push ctx [VInt 1];
	write ctx AObjGet;
	let rtag = alloc_tmp ctx in
	set_tmp ctx rtag;
	let first = ref true in
	let dispatch = List.map (fun (cl,params,e) ->
		List.map (fun c ->
			if !first then first := false else get_tmp ctx rtag;
			push ctx [VInt c];
			write ctx APhysEqual;
			cjmp ctx
		) cl, params, e
	) cases in
	if !first then write ctx APop;
	free_tmp ctx rtag e.epos;
	(match def with
	| None -> if retval then push ctx [VNull]
	| Some e -> gen_expr ctx retval e);
	let jend = jmp ctx in
	let jends = List.map (fun (jl,args,e) ->
		let regs = ctx.regs in
		let nregs = ctx.reg_count in
		List.iter (fun j -> j()) jl;
		let n = ref 1 in
		List.iter (fun (a,t) ->
			incr n;
			match a with
			| None -> ()
			| Some a ->
				define_var ctx a (Some (fun() ->
					get_tmp ctx renum;
					push ctx [VInt !n];
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
	free_tmp ctx renum e.epos;
	List.iter (fun j -> j()) jends

and gen_binop ctx retval op e1 e2 =
	let gen a =
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx a
	in
	let make_op = function
		| OpAdd -> AAdd
		| OpMult -> AMultiply
		| OpDiv -> ADivide
		| OpSub -> ASubtract
		| OpAnd -> AAnd
		| OpOr -> AOr
		| OpXor -> AXor
		| OpShl -> AShl
		| OpShr -> AShr
		| OpUShr -> AAsr
		| OpMod -> AMod
		| _ -> assert false
	in
	match op with
	| OpAssign ->
		let k = gen_access ctx false e1 in
		gen_expr ctx true e2;
		setvar ~retval ctx k
	| OpAssignOp op ->
		let k = gen_access_rw ctx e1 in
		getvar ctx k;
		gen_expr ctx true e2;
		write ctx (make_op op);
		setvar ~retval ctx k
	| OpAdd | OpMult | OpDiv | OpSub | OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr | OpMod ->
		gen (make_op op)
	| OpEq ->
		gen AEqual
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
		let k = gen_access_rw ctx e in
		getvar ctx k;
		(* store preincr value for later access *)
		if retval && flag = Postfix then write ctx (ASetReg 0);		
		write ctx (match op with Increment -> AIncrement | Decrement -> ADecrement | _ -> assert false);
		setvar ~retval:(retval && flag = Prefix) ctx k;
		if retval && flag = Postfix then push ctx [VReg 0]

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
	| TLocal "__keys__", [e2]
	| TLocal "__hkeys__", [e2] ->
		let r = alloc_tmp ctx in
		push ctx [VInt 0; VStr ("Array",true)];
		new_call ctx VarStr 0;
		set_tmp ctx r;
		write ctx APop;
		gen_expr ctx true e2;
		write ctx AEnum2;
		ctx.stack_size <- ctx.stack_size + 1; (* fake *)
		let loop = pos ctx in
		write ctx (ASetReg 0);
		push ctx [VNull];
		write ctx AEqual;
		let jump_end = cjmp ctx in
		if e.eexpr = TLocal "__hkeys__" then begin
			push ctx [VInt 1; VInt 1; VReg 0; VStr ("substr",true)];
			call ctx VarObj 1;
		end else begin
			push ctx [VReg 0];
		end;
		push ctx [VInt 1];
		get_tmp ctx r;
		push ctx [VStr ("push",true)];
		call ctx VarObj 1;
		write ctx APop;
		loop false;
		jump_end();
		get_tmp ctx r;
		free_tmp ctx r e2.epos;
	| TLocal "__physeq__" ,  [e1;e2] ->
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx APhysEqual;
	| TLocal "__unprotect__", [{ eexpr = TConst (TString s) }] ->
		push ctx [VStr (s,false)]
	| TLocal "__resources__", [] ->
		let count = ref 0 in
		Hashtbl.iter (fun name data ->
			incr count;
			push ctx [VStr ("name",false);VStr (name,true);VStr ("data",false)];
			gen_big_string ctx (Codegen.bytes_serialize data);
			push ctx [VInt 2];
			write ctx AObject;
			ctx.stack_size <- ctx.stack_size - 4;
		) ctx.com.resources;
		init_array ctx !count
	| TLocal "__FSCommand2__", l ->
		let nargs = List.length l in
		List.iter (gen_expr ctx true) (List.rev l);
		push ctx [VInt nargs];
		write ctx AFSCommand2;
		ctx.stack_size <- ctx.stack_size - nargs
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
	| TTypeExpr _
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
		List.iter (gen_expr ctx true) (List.rev el);
		init_array ctx (List.length el);
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
		let old_meth = ctx.curmethod in
		let reg_super = Codegen.local_find true "super" f.tf_expr in
		if snd ctx.curmethod then
			ctx.curmethod <- (fst ctx.curmethod ^ "@" ^ string_of_int (Lexer.find_line_index ctx.com.lines e.epos), true)
		else
			ctx.curmethod <- (fst ctx.curmethod, true);
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
			let no_reg = ctx.flash6 || Codegen.local_find false a f.tf_expr in
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
		let tf = begin_func ctx reg_super (Codegen.local_find true "__arguments__" f.tf_expr) rargs in
		ctx.fun_pargs <- (ctx.code_pos, List.rev !pargs) :: ctx.fun_pargs;
		List.iter (fun (a,c,t) ->
			match c with
			| None | Some TNull -> ()
			| Some c -> gen_expr ctx false (Codegen.set_default ctx.com a c t e.epos)
		) f.tf_args;
		if ctx.com.debug then begin
			gen_expr ctx false (ctx.stack.Codegen.stack_push ctx.curclass (fst ctx.curmethod));
			gen_expr ctx false ctx.stack.Codegen.stack_save_pos;
			let start_try = gen_try ctx in
			gen_expr ctx false (Codegen.stack_block_loop ctx.stack f.tf_expr);
			gen_expr ctx false ctx.stack.Codegen.stack_pop;
			let end_try = start_try() in
			(* if $spos == 1 , then no upper call, so report as uncaught *)
			getvar ctx (access_local ctx ctx.stack.Codegen.stack_pos_var);
			push ctx [VInt 1];
			write ctx AEqual;
			write ctx ANot;
			let j = cjmp ctx in
			push ctx [VReg 0];
			push ctx [VInt 1];
			getvar ctx (gen_path ctx (["flash"],"Boot") (!extern_boot));
			push ctx [VStr ("__exc",false)];
			call ctx VarObj 1;
			write ctx AReturn;
			j();
			push ctx [VReg 0];
			write ctx AThrow;
			end_try();
		end else
			gen_expr ctx false f.tf_expr;
		ctx.in_loop <- old_in_loop;
		ctx.curmethod <- old_meth;
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
		gen_expr ctx true e;
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
	| TFor (v,_,it,e) ->
		gen_expr ctx true it;
		let r = alloc_tmp ctx in
		set_tmp ctx r;
		write ctx APop;
		let loop_end = begin_loop ctx in
		let cont_pos = ctx.code_pos in
		let j_begin = pos ctx in
		push ctx [VInt 0];
		get_tmp ctx r;
		push ctx [VStr ("hasNext",false)];
		call ctx VarObj 0;
		write ctx ANot;
		let j_end = cjmp ctx in
		let b = open_block ctx in
		define_var ctx v (Some (fun() ->
			push ctx [VInt 0];
			get_tmp ctx r;
			push ctx [VStr ("next",false)];
			call ctx VarObj 0;
		)) [e];
		gen_expr ctx false e;
		j_begin false;
		j_end();
		loop_end cont_pos;
		if retval then getvar ctx (access_local ctx v);
		b();
		free_tmp ctx r null_pos

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
		match e.eexpr with
		| TFunction _ ->
			push ctx [VReg 0; VStr (f.cf_name,flag)];
			ctx.curmethod <- (f.cf_name,false);
			gen_expr ctx true e;
			setvar ctx VarObj
		| _ ->
			ctx.statics <- (c,flag,f.cf_name,e) :: ctx.statics

let gen_class_static_init ctx (c,flag,name,e) =
	ctx.curclass <- c;
	ctx.curmethod <- (name,false);
	getvar ctx (gen_path ctx c.cl_path c.cl_extern);
	push ctx [VStr (name,flag)];
	gen_expr ctx true e;
	setvar ctx VarObj

let gen_class_field ctx flag f =
	push ctx [VReg 1; VStr (f.cf_name,flag)];
	(match f.cf_expr with
	| None ->
		push ctx [VNull]
	| Some e ->
		ctx.curmethod <- (f.cf_name,false);
		gen_expr ctx true e);
	setvar ctx VarObj

let gen_enum_field ctx e f =
	push ctx [VReg 0; VStr (f.ef_name,false)];
	(match follow f.ef_type with
	| TFun (args,r) ->
		ctx.regs <- PMap.empty;
		ctx.reg_count <- 1;
		let no_reg = ctx.flash6 in
		let rargs = List.map (fun (n,_,_) -> if no_reg then 0, n else alloc_reg ctx , "") args in
		let nregs = List.length rargs + 2 in
		let tf = begin_func ctx false false rargs in
		List.iter (fun (r,name) ->
			if no_reg then begin
				push ctx [VStr (name,false)];
				write ctx AEval;
			end else
				push ctx [VReg r]
		) (List.rev rargs);
		push ctx [VInt f.ef_index; VStr (f.ef_name,false)];
		init_array ctx nregs;
		write ctx ADup;
		write ctx ADup;
		push ctx [VStr ("__enum__",false); VThis];
		write ctx AObjSet;
		push ctx [VStr ("toString",false); VStr ("@estr",false)];
		write ctx AEval;
		write ctx AObjSet;
		write ctx AReturn;
		tf();
	| t ->
		push ctx [VInt f.ef_index; VStr (f.ef_name,false)];
		init_array ctx 2;
		write ctx ADup;
		write ctx ADup;
		push ctx [VStr ("__enum__",false); VReg 0];
		write ctx AObjSet;
		push ctx [VStr ("toString",false); VStr ("@estr",false)];
		write ctx AEval;
		write ctx AObjSet;
	);
	write ctx AObjSet

let init_name ctx path enum =
	push ctx [VReg 0; VStr ((if enum then "__ename__" else "__name__"),false)];
	let name = fst path @ [snd path] in
	push ctx (List.map (fun s -> VStr (s,false)) (List.rev name));
	init_array ctx (List.length name);
	setvar ctx VarObj

let gen_package ctx path ext =
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
				getvar ctx (gen_path ctx ~protect:true ("_global" :: fst path,snd path) ext);
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
	loop [] (fst path)

let gen_type_def ctx t =
	if ctx.ident_size > 50000 then segment ctx;
	match t with
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e -> ctx.inits <- e :: ctx.inits);
		gen_package ctx c.cl_path c.cl_extern;
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
		ctx.curclass <- c;
		(match c.cl_constructor with
		| Some { cf_expr = Some e } ->
			have_constr := true;
			ctx.curmethod <- ("new",false);
			gen_expr ctx true e
		| _ ->
			let f = begin_func ctx true false [] in
			f());
		write ctx (ASetReg 0);
		setvar ctx acc;
		init_name ctx c.cl_path false;
		(match c.cl_super with
		| None -> ()
		| Some (csuper,_) ->
			let path = (match csuper.cl_path with (["flash"],x) when csuper.cl_extern -> (["_global"],x) | p -> p) in
			push ctx [VReg 0; VStr ("__super__",false)];
			getvar ctx (gen_path ctx path csuper.cl_extern);
			setvar ctx VarObj;
			if ctx.flash6 then begin
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
		| [] -> ()
		| l ->
			let nimpl = List.length l in
			push ctx [VReg 0; VStr ("__interfaces__",false)];
			List.iter (fun (c,_) -> getvar ctx (gen_path ctx c.cl_path c.cl_extern)) l;
			init_array ctx nimpl;
			setvar ctx VarObj;
			if not ctx.flash6 then begin
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
		(* true if implements mt.Protect *)
		let flag = is_protected ctx ~stat:true (TInst (c,[])) "" in
		List.iter (gen_class_static_field ctx c flag) c.cl_ordered_statics;
		let flag = is_protected ctx (TInst (c,[])) "" in
		PMap.iter (fun _ f -> if f.cf_get <> ResolveAccess then gen_class_field ctx flag f) c.cl_fields;
	| TEnumDecl e when e.e_extern ->
		()
	| TEnumDecl e ->
		gen_package ctx e.e_path e.e_extern;
		let acc = gen_path ctx e.e_path false in
		push ctx [VInt 0; VStr ("Object",true)];
		write ctx ANew;
		write ctx (ASetReg 0);
		setvar ctx acc;
		init_name ctx e.e_path true;
		push ctx [VReg 0; VStr ("__constructs__",true)];
		List.iter (fun s -> push ctx [VStr (s,false)]) (List.rev e.e_names);
		init_array ctx (List.length e.e_names);
		write ctx AObjSet;
		PMap.iter (fun _ f -> gen_enum_field ctx e f) e.e_constrs
	| TTypeDecl _ ->
		()

let gen_boot ctx =
	(* r0 = Boot *)
	getvar ctx (gen_path ctx (["flash"],"Boot") (!extern_boot));
	write ctx (ASetReg 0);
	write ctx APop;
	(* r0.__init(eval("this")) *)
	push ctx [VStr ("this",true)];
	write ctx AEval;
	push ctx [VInt 1; VReg 0; VStr ("__init",false)];
	call ctx VarObj 0;
	write ctx APop

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

let build_tag (opcodes,idents) =
	let idents = Hashtbl.fold (fun ident pos acc -> (ident,pos) :: acc) idents [] in
	let idents = List.sort (fun (_,p1) (_,p2) -> compare p1 p2) idents in
	let pidents = List.map (fun ((_,flag),_) -> flag) idents in
	let idents = AStringPool (List.map (fun ((id,_),_) -> to_utf8 id) idents) in
	if ActionScript.action_length idents >= 1 lsl 16 then failwith "The SWF can't handle more than a total size of 64K of identifers and literal strings. Try reducing this number by using external data files loaded at runtime";
	DynArray.set opcodes 0 idents;
	TDoAction opcodes , pidents

let convert_header ctx ver (w,h,fps,bg) =
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
		h_fps = to_float16 (if fps > 127.0 then 127.0 else fps);
		h_compressed = not (Common.defined ctx "no-swf-compress");
	} , bg

let default_header ctx ver =
	convert_header ctx ver (400,300,30.,0xFFFFFF)

let generate com =
	let ctx = {
		com = com;
		stack = Codegen.stack_init com true;
		flash6 = com.flash_version = 6;
		segs = [];
		opcodes = DynArray.create();
		code_pos = 0;
		stack_size = 0;
		ident_count = 0;
		ident_size = 0;
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
		curclass = null_class;
		curmethod = ("",false);
		fun_pargs = [];
		in_loop = false;
		static_init = false;
	} in
	write ctx (AStringPool []);
	protect_all := not (Common.defined com "swf-mark");
	extern_boot := true;
	if com.debug then begin
		push ctx [VStr (ctx.stack.Codegen.stack_var,false); VInt 0];
		write ctx AInitArray;
		write ctx ASet;
		push ctx [VStr (ctx.stack.Codegen.stack_exc_var,false); VInt 0];
		write ctx AInitArray;
		write ctx ASet;
	end;
	(* ----- @estr = function() { return flash.Boot.__string_rec(this,""); } ---- *)
	push ctx [VStr ("@estr",false)];
	ctx.reg_count <- 1;
	let f = begin_func ctx false false [] in
	push ctx [VStr ("xx",false); VThis; VInt 2];
	getvar ctx (gen_path ctx (["flash"],"Boot") false);
	push ctx [VStr ("__string_rec",false)];
	call ctx VarObj 2;
	write ctx AReturn;
	f();
	write ctx ASet;
	ctx.reg_count <- 0;
	(* ---- *)
	List.iter (fun t -> gen_type_def ctx t) com.types;
	gen_boot ctx;
	List.iter (fun m -> gen_movieclip ctx m) ctx.movieclips;
	ctx.static_init <- true;
	List.iter (gen_expr ctx false) (List.rev ctx.inits);
	let global_try = gen_try ctx in
	List.iter (gen_class_static_init ctx) (List.rev ctx.statics);
	ctx.static_init <- false;
	let end_try = global_try() in
	(* flash.Boot.__trace(exc) *)
	push ctx [VReg 0; VInt 1];
	getvar ctx (gen_path ctx (["flash"],"Boot") (!extern_boot));
	push ctx [VStr ("__exc",false)];
	call ctx VarObj 1;
	write ctx APop;
	end_try();
	let segs = List.rev ((ctx.opcodes,ctx.idents) :: ctx.segs) in
	let tags = List.map build_tag segs in
	if Common.defined com "swf-mark" then begin
		if List.length segs > 1 then assert false;
		let pidents = snd (List.hd tags) in
		let ch = IO.output_channel (open_out_bin (Filename.chop_extension com.file ^ ".mark")) in
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
	List.map fst tags , ctx.movieclips
