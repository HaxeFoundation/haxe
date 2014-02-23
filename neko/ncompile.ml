(*
 *  Neko Compiler
 *  Copyright (c)2005 Motion-Twin
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License or the LICENSE file for more details.
 *)
open Nast
open Nbytecode

type access =
	| XEnv of int
	| XStack of int
	| XGlobal of int
	| XField of string
	| XIndex of int
	| XArray
	| XThis

type label = {
	lname : string;
	ltraps : int list;
	lstack : int;
	mutable lpos : int option;
	mutable lwait : (unit -> unit) list;
}

type globals = {
	globals : (global,int) Hashtbl.t;
	gobjects : (string list,int) Hashtbl.t;
	mutable functions : (opcode DynArray.t * (int * int) DynArray.t * int * int) list;
	mutable gtable : global DynArray.t;
	labels : (string,label) Hashtbl.t;
	hfiles : (string,int) Hashtbl.t;
	files : string DynArray.t;
}

type context = {
	g : globals;
	version : int;
	mutable ops : opcode DynArray.t;
	mutable locals : (string,int) PMap.t;
	mutable env : (string,int) PMap.t;
	mutable nenv : int;
	mutable stack : int;
	mutable loop_limit : int;
	mutable loop_traps : int;
	mutable limit : int;
	mutable traps : int list;
	mutable breaks : ((unit -> unit) * pos) list;
	mutable continues : ((unit -> unit) * pos) list;
	mutable pos : (int * int) DynArray.t;
	mutable curpos : (int * int);
	mutable curfile : string;
}

type error_msg = string

exception Error of error_msg * pos

let error e p =
	raise (Error(e,p))

let error_msg s =
	s

let stack_delta o =
	match o with
	| AccNull
	| AccTrue
	| AccFalse
	| AccThis
	| AccInt _
	| AccInt32 _
	| AccStack _
	| AccGlobal _
	| AccEnv _
	| AccField _
	| AccBuiltin _
	| AccIndex _
	| JumpIf _
	| JumpIfNot _
	| Jump _
	| JumpTable _
	| Ret _
	| SetGlobal _
	| SetStack _
	| SetEnv _
	| SetThis
	| Bool
	| IsNull
	| IsNotNull
	| Not
	| Hash
	| TypeOf
	| New
	| AccStack0
	| AccStack1
	| AccIndex0
	| AccIndex1
	| Loop
		-> 0
	| Add
	| Sub
	| Mult
	| Div
	| Mod
	| Shl
	| Shr
	| UShr
	| Or
	| And
	| Xor
	| Eq
	| Neq
	| Gt
	| Gte
	| Lt
	| Lte
	| PhysCompare
		-> -1
	| AccArray -> -1
	| SetField _ | SetIndex _ | Compare -> -1
	| SetArray -> -2
	| Push -> 1
	| Pop x -> -x
	| Apply nargs | Call nargs | TailCall (nargs,_) -> -nargs
	| ObjCall nargs -> -(nargs + 1)
	| MakeEnv size | MakeArray size -> -size
	| Trap _ -> trap_stack_delta
	| EndTrap -> -trap_stack_delta

let check_stack ctx stack p =
	if ctx.stack <> stack then error "Stack alignment failure" p

let pos ctx =
	DynArray.length ctx.ops

let real_null_pos =
	{ pline = 0; psource = "<null>" }

let set_pos ctx p =
	if p.psource = ctx.curfile then begin
		if p.pline <> snd ctx.curpos then ctx.curpos <- (fst ctx.curpos, p.pline);
	end else if p = real_null_pos then
		()
	else
		let fid = (try
			Hashtbl.find ctx.g.hfiles p.psource
		with Not_found ->
			let fid = DynArray.length ctx.g.files in
			DynArray.add ctx.g.files p.psource;
			Hashtbl.add ctx.g.hfiles p.psource fid;
			fid
		) in
		ctx.curfile <- p.psource;
		ctx.curpos <- (fid,p.pline)

let write ctx op =
	ctx.stack <- ctx.stack + stack_delta op;
	DynArray.add ctx.pos ctx.curpos;
	if op_param op then DynArray.add ctx.pos ctx.curpos;
	DynArray.add ctx.ops op

let jmp ctx =
	let p = pos ctx in
	write ctx (Jump 0);
	(fun() -> DynArray.set ctx.ops p (Jump(pos ctx - p)))

let cjmp cond ctx =
	let p = pos ctx in
	write ctx (Jump 0);
	(fun() -> DynArray.set ctx.ops p (if cond then JumpIf(pos ctx - p) else JumpIfNot(pos ctx - p)))

let trap ctx =
	let p = pos ctx in
	write ctx (Trap 0);
	(fun() -> DynArray.set ctx.ops p (Trap(pos ctx - p)))

let goto ctx p =
	write ctx (Jump(p - pos ctx))

let global ctx g =
	let ginf = ctx.g in
	try
		Hashtbl.find ginf.globals g
	with Not_found ->
		let gid = DynArray.length ginf.gtable in
		Hashtbl.add ginf.globals g gid;
		DynArray.add ginf.gtable g;
		gid

let save_breaks ctx =
	let oldc = ctx.continues in
	let oldb = ctx.breaks in
	let oldl = ctx.loop_limit in
	let oldt = ctx.loop_traps in
	ctx.loop_traps <- List.length ctx.traps;
	ctx.loop_limit <- ctx.stack;
	ctx.breaks <- [];
	ctx.continues <- [];
	(ctx , oldc, oldb , oldl, oldt)

let process_continues (ctx,oldc,_,_,_) =
	List.iter (fun (f,_) -> f()) ctx.continues;
	ctx.continues <- oldc

let process_breaks (ctx,_,oldb,oldl, oldt) =
	List.iter (fun (f,_) -> f()) ctx.breaks;
	ctx.loop_limit <- oldl;
	ctx.loop_traps <- oldt;
	ctx.breaks <- oldb

let check_breaks ctx =
	List.iter (fun (_,p) -> error "Break outside a loop" p) ctx.breaks;
	List.iter (fun (_,p) -> error "Continue outside a loop" p) ctx.continues

let make_array p el =
	(ECall ((EConst (Builtin "array"),p),el), p)

let get_cases_ints(cases) =
	let max = ref (-1) in
	let l = List.map (fun (e,e2) ->
		match e with
		| (EConst (Int n),_) when n >= 0 ->
			if n > !max then max := n;
			(n,e2)
		| _ -> raise Exit
	) cases in
	(* // only create jump table if small or >10% cases matched *)
	let nmatches = List.length l in
	if nmatches < 3 then raise Exit;
	if !max >= 16 && (nmatches * 100) / (!max + 1) < 10 then raise Exit;
	if !max > 512 then raise Exit;
	(l,!max + 1)

let rec scan_labels ctx supported in_block e =
	match fst e with
	| EFunction (args,e) ->
		let nargs = List.length args in
		let traps = ctx.traps in
		ctx.traps <- [];
		ctx.stack <- ctx.stack + nargs;
		scan_labels ctx supported false e;
		ctx.stack <- ctx.stack - nargs;
		ctx.traps <- traps
	| EBlock _ ->
		let old = ctx.stack in
		Nast.iter (scan_labels ctx supported true) e;
		ctx.stack <- old
	| EVars l ->
		if not in_block then error "Variable declaration must be done inside a block" (snd e);
		List.iter (fun (_,e) ->
			(match e with
			| None -> ()
			| Some e -> scan_labels ctx supported false e);
			ctx.stack <- ctx.stack + 1
		) l
	| ELabel l when not supported ->
		error "Label is not supported in this part of the program" (snd e);
	| ELabel l when Hashtbl.mem ctx.g.labels l ->
		error ("Duplicate label " ^ l) (snd e)
	| ELabel l ->
		let label = {
			lname = l;
			ltraps = List.rev ctx.traps;
			lstack = ctx.stack;
			lpos = None;
			lwait = [];
		} in
		Hashtbl.add ctx.g.labels l label
	| ETry (e,_,e2) ->
		ctx.stack <- ctx.stack + trap_stack_delta;
		ctx.traps <- ctx.stack :: ctx.traps;
		scan_labels ctx supported false e;
		ctx.stack <- ctx.stack - trap_stack_delta;
		ctx.traps <- (match ctx.traps with [] -> assert false | _ :: l -> l);
		ctx.stack <- ctx.stack + 1;
		scan_labels ctx supported false e2;
		ctx.stack <- ctx.stack - 1;
	| EBinop ("=",e1,e2) ->
		let rec is_extended (e,_) =
			match e with
			| EParenthesis e -> is_extended e
			| EArray _
			| EField _ ->
				true
			| _ ->
				false
		in
		let ext = is_extended e1 in
		if ext then ctx.stack <- ctx.stack + 1;
		scan_labels ctx supported false e2;
		ctx.stack <- ctx.stack + 1;
		scan_labels ctx supported false e1;
		ctx.stack <- ctx.stack - (if ext then 2 else 1);
	| ECall ((EConst (Builtin "array"),_),e :: el) ->
		if ctx.version >= 2 then begin
			scan_labels ctx supported false e;
			List.iter (fun e ->
				ctx.stack <- ctx.stack + 1;
				scan_labels ctx supported false e;
			) el;
			ctx.stack <- ctx.stack - List.length el
		end else begin
			List.iter (fun e ->
				scan_labels ctx supported false e;
				ctx.stack <- ctx.stack + 1;
			) el;
			scan_labels ctx supported false e;
			ctx.stack <- ctx.stack - List.length el
		end
	| ECall ((EConst (Builtin x),_),el) when x <> "apply" ->
		Nast.iter (scan_labels ctx false false) e
	| ECall ((EConst (Builtin "apply"),_),e :: el)
	| ECall(e,el) ->
		List.iter (fun e ->
			scan_labels ctx supported false e;
			ctx.stack <- ctx.stack + 1;
		) el;
		scan_labels ctx supported false e;
		ctx.stack <- ctx.stack - List.length el
	| EObject fl ->
		ctx.stack <- ctx.stack + 2;
		List.iter (fun (s,e) ->
			scan_labels ctx supported false e
		) fl;
		ctx.stack <- ctx.stack - 2;
	| ESwitch (ee,[(econd,exec)],eo) ->
		let p = snd e in
		scan_labels ctx supported false (EIf ((EBinop ("==",ee,econd),p),exec,eo),p)
	| ESwitch (e,cases,eo) ->
		scan_labels ctx supported false e;
		let delta = (try ignore(get_cases_ints cases); 0 with Exit -> 1) in
		ctx.stack <- ctx.stack + delta;
		List.iter (fun (e1,e2) ->
			ctx.stack <- ctx.stack + delta;
			scan_labels ctx supported false e1;
			ctx.stack <- ctx.stack - delta;
			scan_labels ctx supported false e2;
		) cases;
		(match eo with
		| None -> ()
		| Some e -> scan_labels ctx supported false e);
		ctx.stack <- ctx.stack - delta;
	| ENext (e1,e2) ->
		scan_labels ctx supported in_block e1;
		scan_labels ctx supported in_block e2;
	| EConst _
	| EContinue
	| EBreak _
	| EReturn _
	| EIf _
	| EWhile _
	| EParenthesis _ ->
		Nast.iter (scan_labels ctx supported false) e
	| EBinop (_,_,_)
	| EArray _
	| EField _
		->
		Nast.iter (scan_labels ctx false false) e
	| ENeko _ ->
		assert false

let compile_constant ctx c p =
	match c with
	| True -> write ctx AccTrue
	| False -> write ctx AccFalse
	| Null -> write ctx AccNull
	| This -> write ctx AccThis
	| Int n -> write ctx (AccInt n)
	| Int32 n -> write ctx (AccInt32 n)
	| Float f -> write ctx (AccGlobal (global ctx (GlobalFloat f)))
	| String s -> write ctx (AccGlobal (global ctx (GlobalString s)))
	| Builtin s ->
		(match s with
		| "tnull" -> write ctx (AccInt 0)
		| "tint" -> write ctx (AccInt 1)
		| "tfloat" -> write ctx (AccInt 2)
		| "tbool" -> write ctx (AccInt 3)
		| "tstring" -> write ctx (AccInt 4)
		| "tobject" -> write ctx (AccInt 5)
		| "tarray" -> write ctx (AccInt 6)
		| "tfunction" -> write ctx (AccInt 7)
		| "tabstract" -> write ctx (AccInt 8)
		| s ->
			write ctx (AccBuiltin s))
	| Ident s ->
		try
			let l = PMap.find s ctx.locals in
			if l <= ctx.limit then
				let e = (try
					PMap.find s ctx.env
				with Not_found ->
					let e = ctx.nenv in
					ctx.nenv <- ctx.nenv + 1;
					ctx.env <- PMap.add s e ctx.env;
					e
				) in
				write ctx (AccEnv e);
			else
				let p = ctx.stack - l in
				write ctx (if p = 0 then AccStack0 else if p = 1 then AccStack1 else AccStack p);
		with Not_found ->
			let g = global ctx (GlobalVar s) in
			write ctx (AccGlobal g)

let rec compile_access ctx e =
	match fst e with
	| EConst (Ident s) ->
		(try
			let l = PMap.find s ctx.locals in
			if l <= ctx.limit then
				let e = (try
					PMap.find s ctx.env
				with Not_found ->
					let e = ctx.nenv in
					ctx.nenv <- ctx.nenv + 1;
					ctx.env <- PMap.add s e ctx.env;
					e
				) in
				XEnv e
			else
				XStack l
		with Not_found ->
			let g = global ctx (GlobalVar s) in
			XGlobal g)
	| EField (e,f) ->
		compile ctx false e;
		write ctx Push;
		XField f
	| EArray (e1,(EConst (Int n),_)) ->
		compile ctx false e1;
		write ctx Push;
		XIndex n
	| EArray (ea,ei) ->
		compile ctx false ei;
		write ctx Push;
		compile ctx false ea;
		write ctx Push;
		XArray
	| EConst This ->
		XThis
	| _ ->
		error "Invalid access" (snd e)

and compile_access_set ctx a =
	match a with
	| XEnv n -> write ctx (SetEnv n)
	| XStack l -> write ctx (SetStack (ctx.stack - l))
	| XGlobal g -> write ctx (SetGlobal g)
	| XField f -> write ctx (SetField f)
	| XIndex i -> write ctx (SetIndex i)
	| XThis -> write ctx SetThis
	| XArray -> write ctx SetArray

and compile_access_get ctx a =
	match a with
	| XEnv n -> write ctx (AccEnv n)
	| XStack l -> write ctx (AccStack (ctx.stack - l))
	| XGlobal g -> write ctx (AccGlobal g)
	| XField f -> write ctx (AccField f)
	| XIndex i -> write ctx (AccIndex i)
	| XThis -> write ctx AccThis
	| XArray ->
		write ctx Push;
		write ctx (AccStack 2);
		write ctx AccArray

and write_op ctx op p =
	match op with
	| "+" -> write ctx Add
	| "-" -> write ctx Sub
	| "/" -> write ctx Div
	| "*" -> write ctx Mult
	| "%" -> write ctx Mod
	| "<<" -> write ctx Shl
	| ">>" -> write ctx Shr
	| ">>>" -> write ctx UShr
	| "|" -> write ctx Or
	| "&" -> write ctx And
	| "^" -> write ctx Xor
	| "==" -> write ctx Eq
	| "!=" -> write ctx Neq
	| ">" -> write ctx Gt
	| ">=" -> write ctx Gte
	| "<" -> write ctx Lt
	| "<=" -> write ctx Lte
	| _ -> error "Unknown operation" p

and compile_binop ctx tail op e1 e2 p =
	match op with
	| "=" ->
		let a = compile_access ctx e1 in
		compile ctx false e2;
		compile_access_set ctx a
	| "&&" ->
		compile ctx false e1;
		let jnext = cjmp false ctx in
		compile ctx tail e2;
		jnext()
	| "||" ->
		compile ctx false e1;
		let jnext = cjmp true ctx in
		compile ctx tail e2;
		jnext()
	| "++="
	| "--=" ->
		write ctx Push;
		let base = ctx.stack in
		let a = compile_access ctx e1 in
		compile_access_get ctx a;
		write ctx (SetStack(ctx.stack - base));
		write ctx Push;
		compile ctx false e2;
		write_op ctx (String.sub op 0 (String.length op - 2)) p;
		compile_access_set ctx a;
		write ctx (AccStack 0);
		write ctx (Pop 1);
	| "+="
	| "-="
	| "/="
	| "*="
	| "%="
	| "<<="
	| ">>="
	| ">>>="
	| "|="
	| "&="
	| "^=" ->
		let a = compile_access ctx e1 in
		compile_access_get ctx a;
		write ctx Push;
		compile ctx false e2;
		write_op ctx (String.sub op 0 (String.length op - 1)) p;
		compile_access_set ctx a
	| _ ->
		match (op , e1 , e2) with
		| ("==" , _ , (EConst Null,_)) ->
			compile ctx false e1;
			write ctx IsNull
		| ("!=" , _ , (EConst Null,_)) ->
			compile ctx false e1;
			write ctx IsNotNull
		| ("==" , (EConst Null,_) , _) ->
			compile ctx false e2;
			write ctx IsNull
		| ("!=" , (EConst Null,_) , _) ->
			compile ctx false e2;
			write ctx IsNotNull
		| ("-", (EConst (Int 0),_) , (EConst (Int i),_)) ->
			compile ctx tail (EConst (Int (-i)),p)
		| _ ->
			compile ctx false e1;
			write ctx Push;
			compile ctx false e2;
			write_op ctx op p

and compile_function main params e =
	let ctx = {
		g = main.g;
		(* // reset *)
		ops = DynArray.create();
		pos = DynArray.create();
		breaks = [];
		continues = [];
		env = PMap.empty;
		nenv = 0;
		traps = [];
		loop_traps = 0;
		limit = main.stack;
		(* // dup *)
		version = main.version;
		stack = main.stack;
		locals = main.locals;
		loop_limit = main.loop_limit;
		curpos = main.curpos;
		curfile = main.curfile;
	} in
	List.iter (fun v ->
		ctx.stack <- ctx.stack + 1;
		ctx.locals <- PMap.add v ctx.stack ctx.locals;
	) params;
	let s = ctx.stack in
	compile ctx true e;
	write ctx (Ret (ctx.stack - ctx.limit));
	check_stack ctx s (snd e);
	check_breaks ctx;
	(* // add let *)
	let gid = DynArray.length ctx.g.gtable in
	ctx.g.functions <- (ctx.ops,ctx.pos,gid,List.length params) :: ctx.g.functions;
	DynArray.add ctx.g.gtable (GlobalFunction(gid,-1));
	(* // environment *)
	if ctx.nenv > 0 then
		let a = Array.make ctx.nenv "" in
		PMap.iter (fun v i -> a.(i) <- v) ctx.env;
		Array.iter (fun v ->
			compile_constant main (Ident v) (snd e);
			write main Push;
		) a;
		write main (AccGlobal gid);
		write main (MakeEnv ctx.nenv);
	else
		write main (AccGlobal gid);

and compile_builtin ctx tail b el p =
	match (b , el) with
	| ("istrue" , [e]) ->
		compile ctx false e;
		write ctx Bool
	| ("not" , [e]) ->
		compile ctx false e;
		write ctx Not
	| ("typeof" , [e]) ->
		compile ctx false e;
		write ctx TypeOf
	| ("hash" , [e]) ->
		compile ctx false e;
		write ctx Hash
	| ("new" , [e]) ->
		compile ctx false e;
		write ctx New
	| ("compare" , [e1;e2]) ->
		compile ctx false e1;
		write ctx Push;
		compile ctx false e2;
		write ctx Compare
	| ("pcompare" , [e1;e2]) ->
		compile ctx false e1;
		write ctx Push;
		compile ctx false e2;
		write ctx PhysCompare
	| ("goto" , [(EConst (Ident l) , _)] ) ->
		let l = (try Hashtbl.find ctx.g.labels l with Not_found -> error ("Unknown label " ^ l) p) in
		let os = ctx.stack in
		let rec loop l1 l2 =
			match l1, l2 with
			| x :: l1 , y :: l2 when x == y -> loop l1 l2
			| _ -> (l1,l2)
		in
		let straps , dtraps = loop (List.rev ctx.traps) l.ltraps in
		List.iter (fun l ->
			if ctx.stack <> l then write ctx (Pop(ctx.stack - l));
			write ctx EndTrap;
		) (List.rev straps);
		let dtraps = List.map (fun l ->
			let l = l - trap_stack_delta in
			if l < ctx.stack then write ctx (Pop(ctx.stack - l));
			while ctx.stack < l do
				write ctx Push;
			done;
			trap ctx
		) dtraps in
		if l.lstack < ctx.stack then write ctx (Pop(ctx.stack - l.lstack));
		while l.lstack > ctx.stack do
			write ctx Push;
		done;
		ctx.stack <- os;
		(match l.lpos with
		| None -> l.lwait <- jmp ctx :: l.lwait
		| Some p -> write ctx (Jump p));
		List.iter (fun t ->
			t();
			write ctx Push;
			compile_constant ctx (Builtin "raise") p;
			write ctx (Call 1);
			(* // insert an infinite loop in order to
			// comply with bytecode checker *)
			let _ = jmp ctx in
			()
		) dtraps;
	| ("goto" , _) ->
		error "Invalid $goto statement" p
	| ("array",e :: el) ->
		let count = List.length el in
		(* // a single let can't have >128 stack *)
		if count > 120 - ctx.stack && count > 8 then begin
			(* // split in 8 and recurse *)
			let part = count lsr 3 in
			let rec loop el acc count =
				match el with
				| [] -> [List.rev acc]
				| e :: l ->
					if count == part then
						(List.rev acc) :: loop el [] 0
					else
						loop l (e :: acc) (count + 1)
			in
			let arr = make_array p (List.map (make_array p) (loop (e :: el) [] 0)) in
			compile_builtin ctx tail "aconcat" [arr] p;
		end else if ctx.version >= 2 then begin
			compile ctx false e;
			List.iter (fun e ->
				write ctx Push;
				compile ctx false e;
			) el;
			write ctx (MakeArray count);
		end else begin
			List.iter (fun e ->
				compile ctx false e;
				write ctx Push;
			) el;
			compile ctx false e;
			write ctx (MakeArray count);
		end
	| ("apply",e :: el) ->
		List.iter (fun e ->
			compile ctx false e;
			write ctx Push;
		) el;
		compile ctx false e;
		let nargs = List.length el in
		if nargs > 0 then write ctx (Apply nargs);
	| _ ->
		List.iter (fun e ->
			compile ctx false e;
			write ctx Push;
		) el;
		compile_constant ctx (Builtin b) p;
		if tail then
			write ctx (TailCall(List.length el,ctx.stack - ctx.limit))
		else
			write ctx (Call (List.length el))

and compile ctx tail (e,p) =
	set_pos ctx p;
	match e with
	| EConst c ->
		compile_constant ctx c p
	| EBlock [] ->
		write ctx AccNull
	| EBlock el ->
		let locals = ctx.locals in
		let stack = ctx.stack in
		let rec loop(el) =
			match el with
			| [] -> assert false
			| [e] -> compile ctx tail e
			| [e; (ELabel _,_) as f] ->
				compile ctx tail e;
				compile ctx tail f
			| e :: el ->
				compile ctx false e;
				loop el
		in
		loop el;
		if stack < ctx.stack then write ctx (Pop (ctx.stack - stack));
		check_stack ctx stack p;
		ctx.locals <- locals
	| EParenthesis e ->
		compile ctx tail e
	| EField (e,f) ->
		compile ctx false e;
		write ctx (AccField f)
	| ECall (e,a :: b :: c :: d :: x1 :: x2 :: l) when (match e with (EConst (Builtin "array"),_) -> false | _ -> true) ->
		let call = (EConst (Builtin "call"),p) in
		let args = (ECall ((EConst (Builtin "array"),p),(a :: b :: c :: d :: x1 :: x2 :: l)),p) in
		(match e with
		| (EField (e,name) , p2) ->
			let locals = ctx.locals in
			let etmp = (EConst (Ident "$tmp"),p2) in
			compile ctx false (EVars [("$tmp",Some e)],p2);
			compile ctx tail (ECall (call,[(EField (etmp,name),p2);etmp;args]), p);
			write ctx (Pop 1);
			ctx.locals <- locals
		| _ ->
			compile ctx tail (ECall (call,[e; (EConst This,p); args]),p))
	| ECall ((EConst (Builtin b),_),el) ->
		compile_builtin ctx tail b el p
	| ECall ((EField (e,f),_),el) ->
		List.iter (fun e ->
			compile ctx false e;
			write ctx Push;
		) el;
		compile ctx false e;
		write ctx Push;
		write ctx (AccField f);
		write ctx (ObjCall(List.length el))
	| ECall (e,el) ->
		List.iter (fun e ->
			compile ctx false e;
			write ctx Push;
		) el;
		compile ctx false e;
		if tail then
			write ctx (TailCall(List.length el,ctx.stack - ctx.limit))
		else
			write ctx (Call(List.length el))
	| EArray (e1,(EConst (Int n),_)) ->
		compile ctx false e1;
		write ctx (if n == 0 then AccIndex0 else if n == 1 then AccIndex1 else AccIndex n)
	| EArray (e1,e2) ->
		compile ctx false e1;
		write ctx Push;
		compile ctx false e2;
		write ctx AccArray
	| EVars vl ->
		List.iter (fun (v,o) ->
			(match o with
			| None -> write ctx AccNull
			| Some e -> compile ctx false e);
			write ctx Push;
			ctx.locals <- PMap.add v ctx.stack ctx.locals;
		) vl
	| EWhile (econd,e,NormalWhile) ->
		let start = pos ctx in
		if ctx.version >= 2 then write ctx Loop;
		compile ctx false econd;
		let jend = cjmp false ctx in
		let save = save_breaks ctx in
		compile ctx false e;
		process_continues save;
		goto ctx start;
		process_breaks save;
		jend();
	| EWhile (econd,e,DoWhile) ->
		let start = pos ctx in
		if ctx.version >= 2 then write ctx Loop;
		let save = save_breaks ctx in
		compile ctx false e;
		process_continues save;
		compile ctx false econd;
		write ctx (JumpIf (start - pos ctx));
		process_breaks save
	| EIf (e,e1,e2) ->
		let stack = ctx.stack in
		compile ctx false e;
		let jelse = cjmp false ctx in
		compile ctx tail e1;
		check_stack ctx stack p;
		(match e2 with
		| None ->
			jelse()
		| Some e2 ->
			let jend = jmp ctx in
			jelse();
			compile ctx tail e2;
			check_stack ctx stack p;
			jend())
	| ETry (e,v,ecatch) ->
		let trap = trap ctx in
		ctx.traps <- ctx.stack :: ctx.traps;
		compile ctx false e;
		write ctx EndTrap;
		ctx.traps <- (match ctx.traps with [] -> assert false | _ :: l -> l);
		let jend = jmp ctx in
		trap();
		write ctx Push;
		let locals = ctx.locals in
		ctx.locals <- PMap.add v ctx.stack ctx.locals;
		compile ctx tail ecatch;
		write ctx (Pop 1);
		ctx.locals <- locals;
		jend()
	| EBinop (op,e1,e2) ->
		compile_binop ctx tail op e1 e2 p
	| EReturn e ->
		(match e with None -> write ctx AccNull | Some e -> compile ctx (ctx.traps == []) e);
		let stack = ctx.stack in
		List.iter (fun t ->
			if ctx.stack > t then write ctx (Pop(ctx.stack - t));
			write ctx EndTrap;
		) ctx.traps;
		write ctx (Ret (ctx.stack - ctx.limit));
		ctx.stack <- stack
	| EBreak e ->
		(match e with
		| None -> ()
		| Some e -> compile ctx false e);
		let s = ctx.stack in
		let n = ref (List.length ctx.traps - ctx.loop_traps) in
		List.iter (fun t ->
			if !n > 0 then begin
				decr n;
				if ctx.stack > t then write ctx (Pop(ctx.stack - t));
				write ctx EndTrap;
			end
		) ctx.traps;
		if ctx.loop_limit <> ctx.stack then write ctx (Pop(ctx.stack - ctx.loop_limit));
		ctx.stack <- s;
		ctx.breaks <- (jmp ctx , p) :: ctx.breaks
	| EContinue ->
		let s = ctx.stack in
		let n = ref (List.length ctx.traps - ctx.loop_traps) in
		List.iter (fun t ->
			if !n > 0 then begin
				decr n;
				if ctx.stack > t then write ctx (Pop(ctx.stack - t));
				write ctx EndTrap;
			end
		) ctx.traps;
		if ctx.loop_limit <> ctx.stack then write ctx (Pop(ctx.stack - ctx.loop_limit));
		ctx.stack <- s;
		ctx.continues <- (jmp ctx , p) :: ctx.continues
	| EFunction (params,e) ->
		compile_function ctx params e
	| ENext (e1,e2) ->
		compile ctx false e1;
		compile ctx tail e2
	| EObject [] ->
		write ctx AccNull;
		write ctx New
	| EObject fl ->
		let fields = List.sort compare (List.map fst fl) in
		let id = (try
			Hashtbl.find ctx.g.gobjects fields
		with Not_found ->
			let id = global ctx (GlobalVar ("o:" ^ string_of_int (Hashtbl.length ctx.g.gobjects))) in
			Hashtbl.add ctx.g.gobjects fields id;
			id
		) in
		write ctx (AccGlobal id);
		write ctx New;
		write ctx Push;
		List.iter (fun (f,e) ->
			write ctx Push;
			compile ctx false e;
			write ctx (SetField f);
			write ctx AccStack0;
		) fl;
		write ctx (Pop 1)
	| ELabel l ->
		let l = (try Hashtbl.find ctx.g.labels l with Not_found -> assert false) in
		if ctx.stack <> l.lstack || List.rev ctx.traps <> l.ltraps then error (Printf.sprintf "Label failure %d %d" ctx.stack l.lstack) p;
		List.iter (fun f -> f()) l.lwait;
		l.lwait <- [];
		l.lpos <- Some (pos ctx)
	| ESwitch (e,[(econd,exec)],eo) ->
		compile ctx tail (EIf ((EBinop ("==",e,econd),p),exec,eo),p)
	| ENeko _ ->
		assert false
	| ESwitch (e,cases,eo) ->
		try
			let ints , size = get_cases_ints cases in
			compile ctx false e;
			write ctx (JumpTable size);
			let tbl = Array.make size None in
			List.iter (fun (i,e) ->
				tbl.(i) <- Some e;
			) ints;
			let tbl = Array.map (fun e -> (jmp ctx,e)) tbl in
			Array.iter (fun (j,e) ->
				if e == None then j()
			) tbl;
			(match eo with
			| None -> write ctx AccNull
			| Some e -> compile ctx tail e);
			let jump_end = jmp ctx in
			let tbl = Array.map (fun (j,e) ->
				match e with
				| Some e ->
					j();
					compile ctx tail e;
					jmp ctx
				| None ->
					(fun() -> ())
			) tbl in
			jump_end();
			Array.iter (fun j -> j()) tbl
		with Exit ->
			compile ctx false e;
			write ctx Push;
			let jumps = List.map (fun (e1,e2) ->
				write ctx AccStack0;
				write ctx Push;
				compile ctx false e1;
				write ctx Eq;
				(cjmp true ctx , e2)
			) cases in
			(match eo with
			| None -> write ctx AccNull
			| Some e -> compile ctx tail (EBlock [e],p));
			let jump_end = jmp ctx in
			let jumps = List.map (fun (j,e) ->
				j();
				compile ctx tail (EBlock [e],p);
				jmp ctx;
			) jumps in
			jump_end();
			List.iter (fun j -> j()) jumps;
			write ctx (Pop 1)

let compile version ast =
	let g = {
		globals = Hashtbl.create 0;
		gobjects = Hashtbl.create 0;
		gtable = DynArray.create();
		functions = [];
		labels = Hashtbl.create 0;
		hfiles = Hashtbl.create 0;
		files = DynArray.create();
	} in
	let ctx = {
		g = g;
		version = version;
		stack = 0;
		loop_limit = 0;
		loop_traps = 0;
		limit = -1;
		locals = PMap.empty;
		ops = DynArray.create();
		breaks = [];
		continues = [];
		env = PMap.empty;
		nenv = 0;
		traps = [];
		pos = DynArray.create();
		curpos = (0,0);
		curfile = "_";
	} in
	if version >= 2 then DynArray.add g.gtable (GlobalVersion version);
	scan_labels ctx true true ast;
	compile ctx false ast;
	check_breaks ctx;
	if g.functions <> [] || Hashtbl.length g.gobjects <> 0 then begin
		let ctxops = ctx.ops in
		let ctxpos = ctx.pos in
		let ops = DynArray.create() in
		let pos = DynArray.create() in
		ctx.pos <- pos;
		ctx.ops <- ops;
		write ctx (Jump 0);
		List.iter (fun (fops,fpos,gid,nargs) ->
			DynArray.set g.gtable gid (GlobalFunction(DynArray.length ops,nargs));
			DynArray.append fops ops;
			DynArray.append fpos pos;
		) (List.rev g.functions);
		DynArray.set ops 0 (Jump (DynArray.length ops));
		let objects = DynArray.create() in
		Hashtbl.iter (fun fl g -> DynArray.add objects (fl,g)) g.gobjects;
		let objects = DynArray.to_array objects in
		Array.sort (fun (_,g1) (_,g2) -> g1 - g2) objects;
		Array.iter (fun (fl,g) ->
			write ctx AccNull;
			write ctx New;
			write ctx (SetGlobal g);
			List.iter (fun f ->
				write ctx (AccGlobal g);
				write ctx Push;
				write ctx (SetField f);
			) fl
		) objects;
		DynArray.append ctxpos pos;
		DynArray.append ctxops ops;
	end;
	DynArray.add g.gtable (GlobalDebug (DynArray.to_array ctx.g.files,DynArray.to_array ctx.pos));
	(DynArray.to_array g.gtable, DynArray.to_array ctx.ops)

