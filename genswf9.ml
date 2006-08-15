(*
 *  Haxe Compiler
 *  Copyright (c)2006 Nicolas Cannasse
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
open Ast
open Type
open As3

type ('a,'b) gen_lookup = {
	h : ('a,'b) Hashtbl.t;
	a : 'a DynArray.t;
	c : int -> 'b;
}

type 'a lookup = ('a,'a index) gen_lookup
type 'a lookup_nz = ('a,'a index_nz) gen_lookup

type read = Read
type write = Unused__ | Write

type 'a access =
	| VReg of reg
	| VId of type_index
	| VGlobal of type_index
	| VArray
	| VScope of int

type local =
	| LReg of reg
	| LScope of int
	| LGlobal of type_index

type code_infos = {
	mutable iregs : int;
	mutable imaxregs : int;
	mutable ipos : int;
	mutable istack : int;
	mutable imax : int;
	mutable iscopes : int;
	mutable imaxscopes : int;
	mutable iloop : int;
}

type context = {
	(* globals *)
	strings : string lookup;
	ints : int32 lookup;
	floats : float lookup;
	brights : as3_base_right lookup;
	rights : as3_rights lookup;
	types : as3_type lookup;
	mtypes : as3_method_type lookup_nz;
	mutable classes : as3_class list;
	mutable statics : as3_static list;
	functions : as3_function lookup;
	rpublic : as3_base_right index;
	gpublic : as3_rights index;

	(* per-function *)
	mutable locals : (string,local) PMap.t;
	mutable code : as3_opcode DynArray.t;
	mutable infos : code_infos;
	mutable trys : (int * int * int * t) list;
	mutable breaks : (unit -> unit) list;
	mutable continues : (int -> unit) list;
	mutable in_static : bool;
	mutable curblock : texpr list;
	mutable block_vars : (int * string) list;
}

let error p = Typer.error "Invalid expression" p
let stack_error p = Typer.error "Stack error" p

let stack_delta = function
	| A3Throw -> -1
	| A3GetSuper _ -> 1
	| A3SetSuper _ -> -1
	| A3RegReset _ -> 0
	| A3Nop -> 0
	| A3Jump (cond,_) ->
		(match cond with
		| J3Always -> 0
		| J3True
		| J3False -> -1
		| _ -> -2)
	| A3Switch _ -> -1
	| A3PopScope -> 0
	| A3XmlOp3 -> assert false
	| A3ForIn | A3ForEach -> -1
	| A3Null
	| A3Undefined
	| A3SmallInt _
	| A3Int _
	| A3True
	| A3False
	| A3String _
	| A3IntRef _
	| A3Function _
	| A3Float _
	| A3NaN -> 1
	| A3Pop -> -1
	| A3Dup -> 1
	| A3CatchDone -> assert false
	| A3Scope -> -1
	| A3Next _ -> 1
	| A3StackCall n -> -(n + 1)
	| A3StackNew n -> -n
	| A3SuperCall (_,n) -> -n
	| A3Call (_,n) -> -n
	| A3RetVoid -> 0
	| A3Ret -> -1
	| A3SuperConstr n -> -(n + 1)
	| A3New (_,n) -> -n
	| A3SuperCallUnknown (_,n) -> -(n + 1)
	| A3CallUnknown (_,n) -> -(n + 1)
	| A3Object n -> -(n * 2) + 1
	| A3Array n -> -n + 1
	| A3NewBlock -> 1
	| A3ClassDef _ -> 0
	| A3XmlOp1 _ -> assert false
	| A3Catch _ -> assert false
	| A3GetInf _ -> 1
	| A3SetInf _ -> 1
	| A3GetProp _ -> 1
	| A3SetProp _ -> -2
	| A3Reg _ -> 1
	| A3SetReg _ -> -1
	| A3GetScope0 | A3GetScope _ -> 1
	| A3Get _ -> 0
	| A3Set _ -> -2
	| A3Delete _ -> -1
	| A3GetSlot _ -> 0
	| A3SetSlot _ -> -2
	| A3ToXml
	| A3ToInt
	| A3ToUInt
	| A3ToNumber
	| A3ToObject
	| A3ToString
	| A3ToBool -> 0
	| A3XmlOp2 -> assert false
	| A3Cast _ -> 0
	| A3Typeof
	| A3InstanceOf -> -1
	| A3IncrReg _ -> 0
	| A3This -> 1
	| A3DebugReg _
	| A3DebugLine _
	| A3DebugFile _ -> 0
	| A3Op op ->
		(match op with
		| A3ONeg | A3OIncr | A3ODecr | A3ONot | A3OBitNot | A3OIIncr | A3OIDecr -> 0
		| _ -> -1)
	| A3Unk _ -> assert false

let index_int (x : int) : 'a index = Obj.magic (x + 1)
let index_nz_int (x : int) : 'a index_nz = Obj.magic x
let tid (x : 'a index) : int = Obj.magic x

let new_lookup() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_int }
let new_lookup_nz() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_nz_int }

let jsize = As3code.length (A3Jump (J3Always,0))

let lookup i w =
	try
		Hashtbl.find w.h i
	with
		Not_found ->
			let id = w.c (DynArray.length w.a) in
			Hashtbl.add w.h i id;
			DynArray.add w.a i;
			id

let add i w =
	let id = w.c (DynArray.length w.a) in
	DynArray.add w.a i;
	id

let lookup_array w = DynArray.to_array w.a

let string ctx i = lookup i ctx.strings

let write ctx op =
	DynArray.add ctx.code op;
	ctx.infos.ipos <- As3code.length op + ctx.infos.ipos;
	let s = ctx.infos.istack + stack_delta op in
	ctx.infos.istack <- s;
	if s > ctx.infos.imax then ctx.infos.imax <- s;
	match op with
	| A3Scope ->
		let n = ctx.infos.iscopes + 1 in
		ctx.infos.iscopes <- n;
		if n > ctx.infos.imaxscopes then ctx.infos.imaxscopes <- n
	| A3PopScope ->
		ctx.infos.iscopes <- ctx.infos.iscopes - 1
	| _ ->
		()

let debug ctx ?file line =
	(match file with None -> () | Some f -> write ctx (A3DebugFile (string ctx f)));
	write ctx (A3DebugLine line)

let jump ctx cond =
	let op = DynArray.length ctx.code in
	write ctx (A3Jump (cond,-4));
	let p = ctx.infos.ipos in
	(fun () ->
		let delta = ctx.infos.ipos - p in
		DynArray.set ctx.code op (A3Jump (cond,delta))
	)

let jump_back ctx =
	let j = jump ctx J3Always in
	let p = ctx.infos.ipos in
	write ctx A3Nop;
	j , (fun cond ->
		let delta = p + -(ctx.infos.ipos + jsize) in
		write ctx (A3Jump (cond,delta))
	)

let type_path ctx ?(getclass=false) (pack,name) =
	let pid = string ctx (String.concat "." pack) in
	let nameid = string ctx name in
	let pid = lookup (A3RPublic (Some pid)) ctx.brights in
	let tid = lookup (if getclass then A3TClassInterface (Some nameid,lookup [pid] ctx.rights) else A3TMethodVar (nameid,pid)) ctx.types in
	tid

let fake_type_path ctx ?(getclass=false) path =
	type_path ctx ~getclass (match path with
		| [] , "Int" -> [] , "int"
		| [] , "Float" -> [] , "Number"
		| [] , "Bool" -> [] , "Boolean"
		| _ -> path)

let ident ctx i = type_path ctx ([],i)

let default_infos() =
	{ ipos = 0; istack = 0; imax = 0; iregs = 0; imaxregs = 0; iscopes = 0; imaxscopes = 0; iloop = -1 }

let alloc_reg ctx =
	let r = ctx.infos.iregs + 1 in
	ctx.infos.iregs <- r;
	if ctx.infos.imaxregs < r then ctx.infos.imaxregs <- r;
	r

let free_reg ctx r =
	if ctx.infos.iregs <> r then assert false;
	ctx.infos.iregs <- r - 1

let pop ctx n =
	let rec loop n =
		if n > 0 then begin
			write ctx A3Pop;
			loop (n - 1)
		end
	in
	if n < 0 then assert false;
	let old = ctx.infos.istack in
	loop n;
	ctx.infos.istack <- old

let define_local ctx name el =
	let l = (if List.exists (Transform.local_find false name) el then begin
			let pos = (try
				fst (List.find (fun (_,x) -> name = x) ctx.block_vars)
			with
				Not_found ->
					let n = List.length ctx.block_vars + 1 in
					ctx.block_vars <- (n,name) :: ctx.block_vars;
					n
			) in
			LScope pos
		end else
			LReg (alloc_reg ctx)
	) in
	ctx.locals <- PMap.add name l ctx.locals

let is_set v = (Obj.magic v) = Write

let gen_local_access ctx name p (forset : 'a)  : 'a access =
	match (try PMap.find name ctx.locals with Not_found -> error p) with
	| LReg r -> VReg r
	| LScope n -> write ctx (A3GetScope 1); VScope n
	| LGlobal id ->
		if is_set forset then write ctx (A3SetInf id);
		VGlobal id

let rec setvar ctx (acc : write access) retval =
	match acc with
	| VReg r ->
		if retval then write ctx A3Dup;
		write ctx (A3SetReg r);
	| VGlobal g ->
		if retval then write ctx A3Dup;
		write ctx (A3SetProp g);
	| VId _ | VArray | VScope _ when retval ->
		let r = alloc_reg ctx in
		write ctx A3Dup;
		write ctx (A3SetReg r);
		setvar ctx acc false;
		write ctx (A3Reg r);
		free_reg ctx r
	| VId id ->
		write ctx (A3Set id)
	| VArray ->
		let id_aset = lookup (A3TArrayAccess ctx.gpublic) ctx.types in
		write ctx (A3Set id_aset);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (A3SetSlot n)

let getvar ctx (acc : read access) =
	match acc with
	| VReg r ->
		write ctx (A3Reg r)
	| VId id ->
		write ctx (A3Get id)
	| VGlobal g ->
		write ctx (A3GetProp g)
	| VArray ->
		let id_aget = lookup (A3TArrayAccess ctx.gpublic) ctx.types in
		write ctx (A3Get id_aget);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (A3GetSlot n)

let open_block ctx el =
	let old_stack = ctx.infos.istack in
	let old_regs = ctx.infos.iregs in
	let old_locals = ctx.locals in
	let old_block = ctx.curblock in
	ctx.curblock <- el;
	(fun() ->
		if ctx.infos.istack <> old_stack then assert false;
		ctx.infos.iregs <- old_regs;
		ctx.locals <- old_locals;
		ctx.curblock <- old_block;
	)

let begin_fun ctx ?(varargs=false) args el stat =
	let old_locals = ctx.locals in
	let old_code = ctx.code in
	let old_infos = ctx.infos in
	let old_trys = ctx.trys in
	let old_bvars = ctx.block_vars in
	let old_static = ctx.in_static in
	ctx.infos <- default_infos();
	ctx.code <- DynArray.create();
	ctx.trys <- [];
	ctx.block_vars <- [];
	ctx.in_static <- stat;
	ctx.locals <- PMap.foldi (fun name l acc ->
		match l with
		| LReg _ -> acc
		| LScope _ -> PMap.add name (LGlobal (type_path ctx ~getclass:true ([],name))) acc
		| LGlobal _ -> PMap.add name l acc
	) ctx.locals PMap.empty;
	List.iter (fun name ->
		define_local ctx name el;
		match gen_local_access ctx name null_pos Write with
		| VReg _ -> ()
		| acc ->
			write ctx (A3Reg (alloc_reg ctx));
			setvar ctx acc false
	) args;
	(fun () ->
		let hasblock = ctx.block_vars <> [] in
		let mt = {
			mt3_ret = None;
			mt3_args = List.map (fun _ -> None) args;
			mt3_native = false;
			mt3_var_args = varargs;
			mt3_debug_name = None;
			mt3_dparams = None;
			mt3_pnames = None;
			mt3_new_block = hasblock;
			mt3_unk_flags = (false,false,false);
		} in
		let code = DynArray.to_list ctx.code in
		let code , delta = (
			if hasblock then
				A3This :: A3Scope :: A3NewBlock :: A3Scope :: code , 4
			else if not stat then
				A3This :: A3Scope :: code , 2
			else
				code , 0
		) in
		let f = {
			fun3_id = add mt ctx.mtypes;
			fun3_stack_size = ctx.infos.imax;
			fun3_nregs = ctx.infos.imaxregs + 1;
			fun3_unk3 = 1;
			fun3_max_scope = ctx.infos.imaxscopes + 1 + (if hasblock then 2 else if not stat then 1 else 0);
			fun3_code = code;
			fun3_trys = Array.of_list (List.map (fun (p,size,cp,t) ->
				{
					tc3_start = p + delta;
					tc3_end = size + delta;
					tc3_handle = cp + delta;
					tc3_type = (match follow t with
						| TInst (c,_) -> Some (fake_type_path ctx c.cl_path)
						| TEnum (e,_) -> Some (fake_type_path ctx e.e_path)
						| TDynamic _ -> None
						| _ -> assert false);
					tc3_name = None;
				}
			) (List.rev ctx.trys));
			fun3_locals = Array.of_list (List.map (fun (id,name) ->
				{
					f3_name = ident ctx name;
					f3_slot = id;
					f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false };
					f3_metas = None;
				}
			) ctx.block_vars);
		} in
		ignore(add f ctx.functions);
		ctx.locals <- old_locals;
		ctx.code <- old_code;
		ctx.infos <- old_infos;
		ctx.trys <- old_trys;
		ctx.block_vars <- old_bvars;
		ctx.in_static <- old_static;
		f.fun3_id
	)

let empty_method ctx =
	let f = begin_fun ctx [] [] true in
	write ctx A3RetVoid;
	f()

let begin_loop ctx =
	let old_loop = ctx.infos.iloop in
	let old_breaks = ctx.breaks in
	let old_conts = ctx.continues in
	ctx.infos.iloop <- ctx.infos.istack;
	(fun cont_pos ->
		if ctx.infos.istack <> ctx.infos.iloop then assert false;
		List.iter (fun j -> j()) ctx.breaks;
		List.iter (fun j -> j cont_pos) ctx.continues;
		ctx.infos.iloop <- old_loop;
		ctx.breaks <- old_breaks;
		ctx.continues <- old_conts;
	)

let gen_constant ctx c =
	match c with
	| TInt i ->
		if Int32.compare i (-128l) > 0 && Int32.compare i 128l < 0 then
			write ctx (A3SmallInt (Int32.to_int i))
		else
			write ctx (A3IntRef (lookup i ctx.ints));
		write ctx A3ToObject
	| TFloat f ->
		let f = float_of_string f in
		write ctx (A3Float (lookup f ctx.floats));
		write ctx A3ToObject
	| TString s ->
		write ctx (A3String (lookup s ctx.strings));
		write ctx A3ToObject
	| TBool b ->
		write ctx (if b then A3True else A3False)
	| TNull ->
		write ctx A3Null
	| TThis ->
		write ctx A3This
	| TSuper ->
		assert false

let no_value ctx retval =
	(* does not push a null but still increment the stack like if
	   a real value was pushed *)
	if retval then ctx.infos.istack <- ctx.infos.istack + 1

let gen_expr_ref = ref (fun _ _ _ -> assert false)
let gen_expr ctx e retval = (!gen_expr_ref) ctx e retval

let gen_access ctx e (forset : 'a) : 'a access =
	match e.eexpr with
	| TLocal i ->
		gen_local_access ctx i e.epos forset
	| TField ({ eexpr = TLocal "__native__" },f) ->
		let nameid = string ctx f in
		let adobeid = string ctx "http://adobe.com/AS3/2006/builtin" in
		let pid = lookup (A3RUnknown1 adobeid) ctx.brights in
		let id = lookup (A3TMethodVar (nameid,pid)) ctx.types in
		write ctx (A3GetInf id);
		VId id
	| TField (e,f) ->
		let id = ident ctx f in
		(match e.eexpr with
		| TConst TThis when not ctx.in_static -> write ctx (A3GetInf id)
		| _ -> gen_expr ctx true e);
		VId id
	| TArray ({ eexpr = TLocal "__global__" },{ eexpr = TConst (TString s) }) ->
		let path = (match List.rev (ExtString.String.nsplit s ".") with [] -> assert false | x :: l -> List.rev l, x) in
		let id = type_path ctx path in
		if is_set forset then write ctx A3GetScope0;
		VGlobal id
	| TArray (e,eindex) ->
		gen_expr ctx true e;
		gen_expr ctx true eindex;
		VArray
	| TTypeExpr t ->
		let id = type_path ctx ~getclass:true (t_path t) in
		if is_set forset then write ctx A3GetScope0;
		VGlobal id
	| _ ->
		error e.epos

let rec gen_expr_content ctx retval e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx c
	| TThrow e ->
		gen_expr ctx true e;
		write ctx A3Throw;
		no_value ctx retval;
	| TParenthesis e ->
		gen_expr ctx retval e
	| TEnumField (e,s) ->
		write ctx A3GetScope0;
		write ctx (A3Get (type_path ctx e.e_path));
		write ctx (A3Get (ident ctx s));
	| TObjectDecl fl ->
		List.iter (fun (name,e) ->
			write ctx (A3String (lookup name ctx.strings));
			gen_expr ctx true e
		) fl;
		write ctx (A3Object (List.length fl))
	| TArrayDecl el ->
		List.iter (gen_expr ctx true) el;
		write ctx (A3Array (List.length el))
	| TBlock el ->
		let rec loop = function
			| [] ->
				if retval then write ctx A3Null
			| [e] ->
				ctx.curblock <- [];
				gen_expr ctx retval e
			| e :: l ->
				ctx.curblock <- l;
				gen_expr ctx false e;
				loop l
		in
		let b = open_block ctx [] in
		loop el;
		b();
	| TVars vl ->
		List.iter (fun (v,_,ei) ->
			define_local ctx v ctx.curblock;
			(match ei with
			| None -> ()
			| Some e ->
				let acc = gen_local_access ctx v e.epos Write in
				gen_expr ctx true e;
				setvar ctx acc false)
		) vl
	| TReturn None ->
		write ctx A3RetVoid;
		no_value ctx retval
	| TReturn (Some e) ->
		gen_expr ctx true e;
		write ctx A3Ret;
		no_value ctx retval
	| TField _
	| TLocal _
	| TTypeExpr _
	| TArray _ ->
		getvar ctx (gen_access ctx e Read)
	| TBinop (op,e1,e2) ->
		gen_binop ctx retval op e1 e2
	| TCall (e,el) ->
		gen_call ctx e el
	| TNew (c,_,pl) ->
		let id = type_path ctx c.cl_path in
		write ctx (A3GetInf id);
		List.iter (gen_expr ctx true) pl;
		write ctx (A3New (id,List.length pl))
	| TFunction f ->
		write ctx (A3Function (generate_function ctx f true))
	| TIf (e,e1,e2) ->
		gen_expr ctx true e;
		let j = jump ctx J3False in
		gen_expr ctx retval e1;
		(match e2 with
		| None -> j()
		| Some e ->
			(* two expresssions, but one per branch *)
			if retval then ctx.infos.istack <- ctx.infos.istack - 1;
			let jend = jump ctx J3Always in
			j();
			gen_expr ctx retval e;
			jend())
	| TWhile (econd,e,flag) ->
		let jstart = (match flag with NormalWhile -> (fun()->()) | DoWhile -> jump ctx J3Always) in
		let end_loop = begin_loop ctx in
		let continue_pos = ctx.infos.ipos + jsize in
		let here, loop = jump_back ctx in
		here();
		gen_expr ctx true econd;
		let jend = jump ctx J3False in
		jstart();
		gen_expr ctx false e;
		loop J3Always;
		jend();
		end_loop continue_pos;
		if retval then write ctx A3Null
	| TUnop (op,flag,e) ->
		gen_unop ctx retval op flag e
	| TTry (e,cases) ->
		let p = ctx.infos.ipos in
		gen_expr ctx retval e;
		let pend = ctx.infos.ipos in
		let jend = jump ctx J3Always in
		let rec loop ncases = function
			| [] -> []
			| (ename,t,e) :: l ->
				let b = open_block ctx [e] in
				let r = alloc_reg ctx in
				ctx.trys <- (p,pend,ctx.infos.ipos,t) :: ctx.trys;
				ctx.infos.istack <- ctx.infos.istack + 1;
				if ctx.infos.imax < ctx.infos.istack then ctx.infos.imax <- ctx.infos.istack;
				write ctx A3This;
				write ctx A3Scope;
				write ctx (A3SetReg r);
				define_local ctx ename [e];
				let acc = gen_local_access ctx ename e.epos Write in
				write ctx (A3Reg r);
				setvar ctx acc false;
				gen_expr ctx retval e;
				b();
				match l with
				| [] -> []
				| _ ->
					let j = jump ctx J3Always in
					j :: loop (ncases + 1) l
		in
		let loops = loop (List.length ctx.trys) cases in
		List.iter (fun j -> j()) loops;
		jend()
	| TFor (v,it,e) ->
		gen_expr ctx true it;
		let r = alloc_reg ctx in
		write ctx (A3SetReg r);
		let b = open_block ctx [e] in
		define_local ctx v [e];
		let end_loop = begin_loop ctx in
		let continue_pos = ctx.infos.ipos + jsize in
		let here, start = jump_back ctx in
		here();
		write ctx (A3Reg r);
		write ctx (A3Call (ident ctx "hasNext",0));
		let jend = jump ctx J3False in
		let acc = gen_local_access ctx v e.epos Write in
		write ctx (A3Reg r);
		write ctx (A3Call (ident ctx "next",0));
		setvar ctx acc false;
		gen_expr ctx false e;

		start J3Always;
		end_loop continue_pos;
		jend();
		if retval then getvar ctx (gen_local_access ctx v e.epos Read);
		b();
		free_reg ctx r;
	| TBreak ->
		pop ctx (ctx.infos.istack - ctx.infos.iloop);
		ctx.breaks <- jump ctx J3Always :: ctx.breaks;
		no_value ctx retval
	| TContinue ->
		pop ctx (ctx.infos.istack - ctx.infos.iloop);
		let op = DynArray.length ctx.code in
		write ctx (A3Jump (J3Always,-4));
		let p = ctx.infos.ipos in
		ctx.continues <- (fun target -> DynArray.set ctx.code op (A3Jump (J3Always,target - p))) :: ctx.continues;
		no_value ctx retval
	| TSwitch (e,el,eo) ->
		let r = alloc_reg ctx in
		gen_expr ctx true e;
		write ctx (A3SetReg r);
		let prev = ref (fun () -> ()) in
		let jend = List.map (fun (v,e) ->
			(!prev)();
			write ctx (A3Reg r);
			gen_expr ctx true v;
			prev := jump ctx J3Neq;
			gen_expr ctx retval e;
			jump ctx J3Always
		) el in
		(!prev)();
		free_reg ctx r;
		(match eo with
		| None -> if retval then write ctx A3Null
		| Some e -> gen_expr ctx retval e);
		List.iter (fun j -> j()) jend;
	| TMatch (e,_,cases,def) ->
		let rparams = alloc_reg ctx in
		let rtag = alloc_reg ctx in
		gen_expr ctx true e;
		write ctx A3Dup;
		write ctx (A3Get (ident ctx "tag"));
		write ctx (A3SetReg rtag);
		write ctx (A3Get (ident ctx "params"));
		write ctx (A3SetReg rparams);
		let prev = ref (fun () -> ()) in
		let jend = List.map (fun (tag,params,e) ->
			(!prev)();
			write ctx (A3Reg rtag);
			write ctx (A3String (lookup tag ctx.strings));
			prev := jump ctx J3Neq;
			let b = open_block ctx [e] in
			(match params with
			| None -> ()
			| Some l ->
				let p = ref (-1) in
				List.iter (fun (name,_) ->
					incr p;
					match name with
					| None -> ()
					| Some v ->
						define_local ctx v [e];
						let acc = gen_local_access ctx v e.epos Write in
						write ctx (A3Reg rparams);
						write ctx (A3SmallInt !p);
						getvar ctx VArray;
						setvar ctx acc false
				) l
			);
			gen_expr ctx retval e;
			b();
			jump ctx J3Always;
		) cases in
		(!prev)();
		(match def with
		| None -> if retval then write ctx A3Null
		| Some e -> gen_expr ctx retval e);
		List.iter (fun j -> j()) jend;
		free_reg ctx rtag;
		free_reg ctx rparams

and gen_call ctx e el =
	match e.eexpr , el with
	| TLocal "__is__" , [e;t] ->
		gen_expr ctx true e;
		gen_expr ctx true t;
		write ctx (A3Op A3OIs)
	| TLocal "__keys__" , [e] ->
		let racc = alloc_reg ctx in
		let rcounter = alloc_reg ctx in
		let rtmp = alloc_reg ctx in
		write ctx (A3SmallInt 0);
		write ctx (A3SetReg rcounter);
		write ctx (A3Array 0);
		write ctx (A3SetReg racc);
		gen_expr ctx true e;
		write ctx (A3SetReg rtmp);
		let start, loop = jump_back ctx in
		write ctx (A3Reg racc);
		write ctx (A3Reg rtmp);
		write ctx (A3Reg rcounter);
		write ctx A3ForIn;
		write ctx (A3Call (ident ctx "push",1));
		write ctx A3Pop;
		start();
		write ctx (A3Next (rtmp,rcounter));
		loop J3True;
		write ctx (A3Reg racc);
		free_reg ctx rtmp;
		free_reg ctx rcounter;
		free_reg ctx racc;
	| TLocal "__new__" , e :: el ->
		gen_expr ctx true e;
		List.iter (gen_expr ctx true) el;
		write ctx (A3StackNew (List.length el))
	| TConst TSuper , _ ->
		write ctx A3This;
		List.iter (gen_expr ctx true) el;
		write ctx (A3SuperConstr (List.length el));
	| TField ({ eexpr = TConst TSuper },f) , _ ->
		let id = ident ctx f in
		write ctx (A3GetInf id);
		List.iter (gen_expr ctx true) el;
		write ctx (A3SuperCall (id,List.length el));
	| TField ({ eexpr = TConst TThis },f) , _ when not ctx.in_static ->
		let id = ident ctx f in
		write ctx (A3GetInf id);
		List.iter (gen_expr ctx true) el;
		write ctx (A3Call (id,List.length el));
	| TField (e,f) , _ ->
		gen_expr ctx true e;
		List.iter (gen_expr ctx true) el;
		write ctx (A3Call (ident ctx f,List.length el));
	| _ ->
		gen_expr ctx true e;
		write ctx A3GetScope0;
		List.iter (gen_expr ctx true) el;
		write ctx (A3StackCall (List.length el))

and gen_unop ctx retval op flag e =
	match op with
	| Not ->
		gen_expr ctx true e;
		write ctx (A3Op A3ONot);
	| Neg ->
		gen_expr ctx true e;
		write ctx (A3Op A3ONeg);
	| NegBits ->
		gen_expr ctx true e;
		write ctx (A3Op A3OBitNot);
	| Increment
	| Decrement ->
		let incr = (op = Increment) in
		let acc = gen_access ctx e Write in (* for set *)
		getvar ctx (gen_access ctx e Read);
		match flag with
		| Postfix when retval ->
			let r = alloc_reg ctx in
			write ctx A3Dup;
			write ctx (A3SetReg r);
			write ctx (A3Op (if incr then A3OIncr else A3ODecr));
			write ctx A3ToObject;
			setvar ctx acc false;
			write ctx (A3Reg r);
			free_reg ctx r
		| Postfix | Prefix ->
			write ctx (A3Op (if incr then A3OIncr else A3ODecr));
			write ctx A3ToObject;
			setvar ctx acc retval

and gen_binop ctx retval op e1 e2 =
	let gen_op o =
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx (A3Op o)
	in
	match op with
	| OpAssign ->
		let acc = gen_access ctx e1 Write in
		gen_expr ctx true e2;
		setvar ctx acc retval
	| OpBoolAnd ->
		gen_expr ctx true e1;
		write ctx A3Dup;
		let j = jump ctx J3False in
		write ctx A3Pop;
		gen_expr ctx true e2;
		j();
	| OpBoolOr ->
		gen_expr ctx true e1;
		write ctx A3Dup;
		let j = jump ctx J3True in
		write ctx A3Pop;
		gen_expr ctx true e2;
		j();
	| OpAssignOp op ->
		let acc = gen_access ctx e1 Write in
		gen_binop ctx true op e1 e2;
		setvar ctx acc retval
	| OpAdd ->
		gen_op A3OAdd
	| OpMult ->
		gen_op A3OMul
	| OpDiv ->
		gen_op A3ODiv
	| OpSub ->
		gen_op A3OSub
	| OpEq ->
		gen_op A3OEq
	| OpPhysEq ->
		gen_op A3OPhysEq
	| OpNotEq ->
		gen_op A3OEq;
		write ctx (A3Op A3ONot)
	| OpPhysNotEq ->
		gen_op A3OPhysEq;
		write ctx (A3Op A3ONot)
	| OpGt ->
		gen_op A3OGt
	| OpGte ->
		gen_op A3OGte
	| OpLt ->
		gen_op A3OLt
	| OpLte ->
		gen_op A3OLte
	| OpAnd ->
		gen_op A3OAnd
	| OpOr ->
		gen_op A3OOr
	| OpXor ->
		gen_op A3OXor
	| OpShl ->
		gen_op A3OShl
	| OpShr ->
		gen_op A3OShr
	| OpUShr ->
		gen_op A3OUShr
	| OpMod ->
		gen_op A3OMod
	| OpInterval ->
		assert false

and gen_expr ctx retval e =
	let old = ctx.infos.istack in
	gen_expr_content ctx retval e;
	if old <> ctx.infos.istack then begin
		if old + 1 <> ctx.infos.istack then stack_error e.epos;
		if not retval then write ctx A3Pop;
	end else if retval then stack_error e.epos

and generate_function ctx fdata stat =
	let f = begin_fun ctx (List.map (fun (name,_,_) -> name) fdata.tf_args) [fdata.tf_expr] stat in
	gen_expr ctx false fdata.tf_expr;
	write ctx A3RetVoid;
	f()

let generate_construct ctx args =
	let f = begin_fun ctx args [] false in
	write ctx A3This;
	let r = ref 0 in
	List.iter (fun _ -> incr r; write ctx (A3Reg !r)) args;
	write ctx (A3SuperConstr (List.length args));
	write ctx A3RetVoid;
	f()

let generate_method ctx stat rproto f =
	match f.cf_expr with
	| Some { eexpr = TFunction fdata } ->
		let fid = generate_function ctx fdata stat in
		if stat then
			write ctx A3Dup
		else
			(match !rproto with
			| None ->
				let r = alloc_reg ctx in
				write ctx A3Dup;
				write ctx (A3Get (ident ctx "prototype"));
				write ctx A3Dup;
				write ctx (A3SetReg r);
				rproto := Some r
			| Some r ->
				write ctx (A3Reg r)
			);
		write ctx (A3Function fid);
		write ctx (A3Set (ident ctx f.cf_name));
	| _ ->
		()

let generate_class_init ctx c slot =
	let rproto = ref None in
	write ctx A3GetScope0;
	if c.cl_interface then
		write ctx A3Null
	else begin
		let path = (match c.cl_super with None -> ([],"Object") | Some (sup,_) -> sup.cl_path) in
		write ctx (A3GetProp (type_path ctx path));
		write ctx A3Scope;
		write ctx (A3GetProp (type_path ctx ~getclass:true path));
	end;
	write ctx (A3ClassDef slot);
	PMap.iter (fun name f -> generate_method ctx false rproto f) c.cl_fields;
	List.iter (generate_method ctx true rproto) c.cl_ordered_statics;
	if not c.cl_interface then write ctx A3PopScope;
	(match !rproto with None -> () | Some r -> free_reg ctx r);
	write ctx (A3Set (type_path ctx c.cl_path))

let generate_class_statics ctx c =
	let r = alloc_reg ctx in
	let first = ref true in
	let nslot = ref 0 in
	List.iter (fun f ->
		incr nslot;
		match f.cf_expr with
		| Some { eexpr = TFunction _ } | None -> ()
		| Some e ->
			if !first then begin
				write ctx A3GetScope0;
				write ctx (A3Get (type_path ctx c.cl_path));
				write ctx (A3SetReg r);
				first := false;
			end;
			write ctx (A3Reg r);
			gen_expr ctx true e;
			write ctx (A3SetSlot !nslot);
	) c.cl_ordered_statics;
	free_reg ctx r

let generate_enum_init ctx e slot =
	let path = ([],"Object") in
	let name_id = type_path ctx e.e_path in
	write ctx A3GetScope0;
	write ctx (A3GetProp (type_path ctx path));
	write ctx A3Scope;
	write ctx (A3GetProp (type_path ~getclass:true ctx path));
	write ctx (A3ClassDef slot);
	write ctx A3PopScope;
	let r = alloc_reg ctx in
	write ctx A3Dup;
	write ctx (A3SetReg r);
	write ctx (A3Set name_id);
	let nslot = ref 0 in
	PMap.iter (fun _ f ->
		incr nslot;
		match f.ef_type with
		| TFun _ -> ()
		| _ ->
			write ctx (A3Reg r);
			write ctx (A3GetInf name_id);
			write ctx (A3String (lookup f.ef_name ctx.strings));
			write ctx A3Null;
			write ctx (A3New (name_id,2));
			write ctx (A3SetSlot !nslot);
	) e.e_constrs;
	free_reg ctx r

let generate_field_kind ctx f c stat =
	match f.cf_expr with
	| Some { eexpr = TFunction fdata } ->
		let rec loop c =
			match c.cl_super with
			| None -> false
			| Some (c,_) ->
				PMap.exists f.cf_name c.cl_fields || loop c
		in
		if not stat && loop c then
			None
		else
			Some (A3FVar {
				v3_type = None;
				v3_value = A3VNone;
				v3_const = false;
			})
	| _ when c.cl_interface && not stat ->
		None
	| _ ->
		Some (A3FVar {
			v3_type = None;
			v3_value = A3VNone;
			v3_const = false;
		})

let generate_class ctx c =
	let name_id = type_path ctx c.cl_path in
	let st_id = empty_method ctx in
	let cid = (match c.cl_constructor with
		| None ->
			let rec loop c =
				match c.cl_super with
				| None ->
					if c.cl_interface then begin
						let mt0 = {
							mt3_ret = None;
							mt3_args = [];
							mt3_native = false;
							mt3_var_args = false;
							mt3_new_block = false;
							mt3_debug_name = None;
							mt3_dparams = None;
							mt3_pnames = None;
							mt3_unk_flags = (false,false,false);
						} in
						add mt0 ctx.mtypes
					end else
						generate_construct ctx []
				| Some (csup,_) ->
					match csup.cl_constructor with
					| None -> loop csup
					| Some co ->
						let args = (match follow co.cf_type with
							| TFun (l,_) -> List.map (fun (name,_,_) -> name) l
							| _ -> assert false
						) in
						generate_construct ctx args
			in
			loop c
		| Some f ->
			match f.cf_expr with
			| Some { eexpr = TFunction f } -> generate_function ctx f false
			| _ -> assert false
	) in
	let fields = Array.of_list (PMap.fold (fun f acc ->
		match generate_field_kind ctx f c false with
		| None -> acc
		| Some k ->
			{
				f3_name = ident ctx f.cf_name;
				f3_slot = 0;
				f3_kind = k;
				f3_metas = None;
			} :: acc
	) c.cl_fields []) in
	let sc = {
		cl3_name = name_id;
		cl3_super = (if c.cl_interface then None else Some (type_path ctx (match c.cl_super with None -> [],"Object" | Some (c,_) -> c.cl_path)));
		cl3_sealed = true;
		cl3_final = false;
		cl3_interface = c.cl_interface;
		cl3_rights = None;
		cl3_implements = Array.of_list (List.map (fun (c,_) ->
			if not c.cl_interface then Typer.error "Can't implement class in Flash9" c.cl_pos;
			type_path ctx c.cl_path
		) c.cl_implements);
		cl3_construct = cid;
		cl3_fields = fields;
	} in
	let st_count = ref 0 in
	let st = {
		st3_method = st_id;
		st3_fields = Array.of_list (List.map (fun f ->
			incr st_count;
			{
				f3_name = ident ctx f.cf_name;
				f3_slot = !st_count;
				f3_kind = (match generate_field_kind ctx f c true with None -> assert false | Some k -> k);
				f3_metas = None;
			}
		) c.cl_ordered_statics)
	} in
	ctx.classes <- sc :: ctx.classes;
	ctx.statics <- st :: ctx.statics

let generate_enum ctx e =
	let name_id = type_path ctx e.e_path in
	let st_id = empty_method ctx in
	let f = begin_fun ctx ["tag";"params"] [] false in
	let tag_id = ident ctx "tag" in
	let params_id = ident ctx "params" in
	write ctx (A3SetInf tag_id);
	write ctx (A3Reg 1);
	write ctx (A3Set tag_id);
	write ctx (A3SetInf params_id);
	write ctx (A3Reg 2);
	write ctx (A3Set params_id);
	write ctx A3RetVoid;
	let construct = f() in
	let f = begin_fun ctx [] [] true in
	write ctx (A3GetProp (type_path ctx ~getclass:true (["flash"],"Boot")));
	write ctx A3This;
	write ctx (A3Call (ident ctx "enum_to_string",1));
	write ctx A3Ret;
	let tostring = f() in
	let sc = {
		cl3_name = name_id;
		cl3_super = Some (type_path ctx ([],"Object"));
		cl3_sealed = true;
		cl3_final = false;
		cl3_interface = false;
		cl3_rights = None;
		cl3_implements = [||];
		cl3_construct = construct;
		cl3_fields = [|
			{ f3_name = tag_id; f3_slot = 0; f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false; }; f3_metas = None };
			{ f3_name = params_id; f3_slot = 0; f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false; }; f3_metas = None };
			{
				f3_name = ident ctx "toString";
				f3_slot = 0;
				f3_kind = A3FMethod {
					m3_type = tostring;
					m3_final = false;
					m3_override = false;
					m3_kind = MK3Normal;
				};
				f3_metas = None;
			};
		|];
	} in
	let st_count = ref 0 in
	let st = {
		st3_method = st_id;
		st3_fields = Array.of_list (PMap.fold (fun f acc ->
			incr st_count;
			{
				f3_name = ident ctx f.ef_name;
				f3_slot = !st_count;
				f3_kind = (match f.ef_type with
					| TFun (args,_) ->
						let fdata = begin_fun ctx (List.map (fun (name,_,_) -> name) args) [] true in
						write ctx (A3GetInf name_id);
						write ctx (A3String (lookup f.ef_name ctx.strings));
						let n = ref 0 in
						List.iter (fun _ -> incr n; write ctx (A3Reg !n)) args;
						write ctx (A3Array (!n));
						write ctx (A3New (name_id,2));
						write ctx A3Ret;
						let fid = fdata() in
						A3FMethod {
							m3_type = fid;
							m3_final = false;
							m3_override = false;
							m3_kind = MK3Normal;
						}
					| _ ->
						A3FVar { v3_type = (Some name_id); v3_value = A3VNone; v3_const = false; }
				);
				f3_metas = None;
			} :: acc
		) e.e_constrs [])
	} in
	ctx.classes <- sc :: ctx.classes;
	ctx.statics <- st :: ctx.statics

let is_core_type = function
	| [] , "Bool" | [] , "Void" | [] , "Dynamic" -> true
	| _ -> false


let generate_type ctx t =
	match t with
	| TClassDecl c -> if not c.cl_extern then generate_class ctx c
	| TTypeDecl _ -> ()
	| TEnumDecl e -> if not (is_core_type e.e_path) then generate_enum ctx e

let generate_inits ctx types =
	let f = begin_fun ctx [] [] false in
	let slot = ref 0 in
	let classes = List.fold_left (fun acc t ->
		match t with
		| TClassDecl c when not c.cl_extern ->
			incr slot;
			generate_class_init ctx c (!slot - 1);
			{
				f3_name = type_path ctx c.cl_path;
				f3_slot = !slot;
				f3_kind = A3FClass (index_nz_int (!slot - 1));
				f3_metas = None;
			} :: acc
		| TEnumDecl e when not (is_core_type e.e_path) ->
			incr slot;
			generate_enum_init ctx e (!slot - 1);
			{
				f3_name = type_path ctx e.e_path;
				f3_slot = !slot;
				f3_kind = A3FClass (index_nz_int (!slot - 1));
				f3_metas = None;
			} :: acc
		| _ ->
			acc
	) [] types in

	(* define flash.Boot.init method *)
	write ctx A3GetScope0;
	write ctx (A3Get (type_path ctx (["flash"],"Boot")));
	let finit = begin_fun ctx [] [] true in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			(match c.cl_init with
			| None -> ()
			| Some e -> gen_expr ctx false e);
		| _ -> ()
	) types;
	List.iter (fun t ->
		match t with
		| TClassDecl c -> generate_class_statics ctx c
		| _ -> ()
	) types;
	write ctx A3RetVoid;
	write ctx (A3Function (finit()));
	write ctx (A3Set (ident ctx "init"));

	write ctx A3RetVoid;
	{
		st3_method = f();
		st3_fields = Array.of_list (List.rev classes);
	}

let generate types hres =
	let brights = new_lookup() in
	let strings = new_lookup() in
	let rights = new_lookup() in
	let empty_id = lookup "" strings in
	let rpublic = lookup (A3RPublic (Some empty_id)) brights in
	let ctx = {
		strings = strings;
		ints = new_lookup();
		floats = new_lookup();
		brights = brights;
		rights = rights;
		types = new_lookup();
		mtypes = new_lookup_nz();
		rpublic = rpublic;
		gpublic = lookup [rpublic] rights;
		classes = [];
		statics = [];
		functions = new_lookup();
		code = DynArray.create();
		locals = PMap.empty;
		infos = default_infos();
		trys = [];
		breaks = [];
		continues = [];
		curblock = [];
		block_vars = [];
		in_static = false;
	} in
	List.iter (generate_type ctx) types;
	Hashtbl.iter (fun _ _ -> assert false) hres;
	let init = generate_inits ctx types in
	let a = {
		as3_ints = lookup_array ctx.ints;
		as3_floats = lookup_array ctx.floats;
		as3_idents = lookup_array ctx.strings;
		as3_base_rights = lookup_array ctx.brights;
		as3_rights = lookup_array ctx.rights;
		as3_types = lookup_array ctx.types;
		as3_method_types = lookup_array ctx.mtypes;
		as3_metadatas = [||];
		as3_classes = Array.of_list (List.rev ctx.classes);
		as3_statics = Array.of_list (List.rev ctx.statics);
		as3_inits = [|init|];
		as3_functions = lookup_array ctx.functions;
		as3_unknown = "";
	} in
	[Swf.TActionScript3 (None,a); Swf.TSwf9Name [0,"flash.Boot"]]


(* ----------------------------------------------------------------------------------------

	HX generation

   ---------------------------------------------------------------------------------------- *)

let ident ctx p =
	As3code.iget ctx.as3_idents p

let package ctx idx =
	match As3code.iget ctx.as3_base_rights idx with
	| A3RPrivate (Some id)
	| A3RPublic (Some id)
	| A3RInternal (Some id)
	| A3RProtected id
	| A3RUnknown1 id
	| A3RUnknown2 (Some id) ->
		let pack = ident ctx id in
		ExtString.String.nsplit pack "."
	| A3RPrivate None | A3RPublic None | A3RInternal None | A3RUnknown2 None ->
		[]

let real_type_path ctx p =
	match As3code.iget ctx.as3_types p with
	| A3TMethodVar (id,pack) ->
		let name = ident ctx id in
		let pack = package ctx pack in
		pack , name
	| A3TClassInterface (Some id,pack) ->
		let name = ident ctx id in
		let pack = package ctx (List.hd (As3code.iget ctx.as3_rights pack)) in
		pack , name
	| A3TClassInterface (None,_) ->
		[] , "$ClassInterfaceNone"
	| A3TArrayAccess _ ->
		[] , "$ArrayAccess"
	| A3TUnknown1 _ ->
		[] , "$Unknown1"
	| A3TUnknown2 _ ->
		[] , "$Unknown2"

let type_path ctx p =
	match real_type_path ctx p with
	| [] , "Object" -> [] , "Dynamic"
	| [] , "Boolean" -> [] , "Bool"
	| [] , "int" -> [] , "Int"
	| [] , "uint" -> [] , "UInt"
	| [] , "Number" -> [] , "Float"
	| [] , "Array" -> [] , "Array<Dynamic>"
	| [] , "void" -> [] , "Void"
	| [] , "Function" -> [] , "Dynamic"
	| path -> path

let ident_rights ctx id =
	match As3code.iget ctx.as3_types id with
	| A3TMethodVar (id,r) ->
		let name = ident ctx id in
		let r = (match As3code.iget ctx.as3_base_rights r with
			| A3RPublic _ | A3RUnknown1 _ -> false
			| _ -> true
		) in
		r , name
	| _ -> false, "???"

let rec create_dir acc = function
	| [] -> ()
	| d :: l ->
		let path = acc ^ "/" ^ d in
		(try Unix.mkdir path 0o777 with _ -> ());
		create_dir path l

let value_type = function
	| A3VNone
	| A3VNull -> "Dynamic"
	| A3VBool _ -> "Bool"
	| A3VString _ -> "String"
	| A3VInt _ -> "Int"
	| A3VFloat _ -> "Float"
	| A3VNamespace _ -> "$Namespace"

let type_val ctx t v =
	match t with
	| None ->
		(match v with
		| None -> "Dynamic"
		| Some v -> value_type v)
	| Some t ->
		s_type_path (type_path ctx t)

let has_getset ml f m =
	List.exists (fun f2 ->
		match f2.f3_kind with
		| A3FMethod m2 when f.f3_name = f2.f3_name ->
			(match m.m3_kind , m2.m3_kind with
			| MK3Getter , MK3Setter | MK3Setter , MK3Getter -> true
			| _ -> false)
		| _ -> false
	) ml

let gen_method ctx ch name mt =
	let m = As3code.iget ctx.as3_method_types (As3parse.no_nz mt) in
	let ret = (match m.mt3_ret with
		| None -> "Void"
		| Some t -> s_type_path (type_path ctx t)
	) in
	let p = ref 0 in
	let params = List.map (fun a ->
		let name = (match m.mt3_pnames with
			| None -> "p" ^ string_of_int !p
			| Some l -> ident ctx (List.nth l (!p))
		) in
		let opt_val = (match m.mt3_dparams with
			| None -> None
			| Some l ->
				try
					Some (List.nth l (!p - List.length m.mt3_args + List.length l))
				with
					_ -> None
		) in
		let t = type_val ctx a opt_val in
		incr p;
		(if opt_val <> None then "?" else "") ^ name ^ " : " ^ t
	) m.mt3_args in
	let vargs = if m.mt3_var_args then " /* ...arguments */" else "" in
	IO.printf ch "function %s(%s%s) : %s;\n" name (String.concat ", " params) vargs ret

let gen_fields ctx ch fields stat =
	let fields = List.sort (fun f1 f2 -> compare (ident_rights ctx f1.f3_name) (ident_rights ctx f2.f3_name)) (Array.to_list fields) in
	List.iter (fun f ->
		match f.f3_kind with
		| A3FMethod m ->
			if m.m3_override then
				()
			else
			let priv , name = ident_rights ctx f.f3_name in
			(match m.m3_kind with
			| MK3Normal ->
				IO.printf ch "\t";
				if priv then IO.printf ch "private ";
				if stat then IO.printf ch "static ";
				gen_method ctx ch name m.m3_type
			| MK3Getter ->
				let set = has_getset fields f m in
				let set_str = if set then "" else "(default,null)" in
				let m = As3code.iget ctx.as3_method_types (As3parse.no_nz m.m3_type) in
				let t = (match m.mt3_ret with None -> "Dynamic" | Some t -> s_type_path (type_path ctx t)) in
				IO.printf ch "\t%s%svar %s%s : %s;\n" (if priv then "private " else "") (if stat then "static " else "") name set_str t
			| MK3Setter ->
				let get = has_getset fields f m in
				if not get then begin
					let m = As3code.iget ctx.as3_method_types (As3parse.no_nz m.m3_type) in
					let t = (match m.mt3_ret with None -> "Dynamic" | Some t -> s_type_path (type_path ctx t)) in
					IO.printf ch "\t%s%svar %s(null,default) : %s;\n" (if priv then "private " else "") (if stat then "static " else "") name t
				end;
			)
		| A3FVar v ->
			let t = type_val ctx v.v3_type (Some v.v3_value) in
			let priv , n = ident_rights ctx f.f3_name in
			IO.printf ch "\t%s%svar %s : %s;\n" (if priv then "private " else "") (if stat then "static " else "") n t
		| A3FClass _ ->
			IO.printf ch "\t// ????\n"
	) fields

let genhx_class ctx c s =
	let pack , name = real_type_path ctx c.cl3_name in
	prerr_string ("import " ^ s_type_path (pack,name));
	create_dir "tmp" pack;
	let f = open_out ("tmp/" ^ (match pack with [] -> "" | l -> String.concat "/" l ^ "/") ^ name ^ ".hx") in
	let ch = IO.output_channel f in
	if pack <> [] then IO.printf ch "package %s;\n\n" (String.concat "." pack);
	IO.printf ch "extern %s %s" (if c.cl3_interface then "interface" else "class") name;
	let prev = ref (match c.cl3_super with
	| None -> false
	| Some p ->
		match type_path ctx p with
		| [] , "Dynamic" -> false
		| path ->
			IO.printf ch " extends %s" (s_type_path path);
			true
	) in
	Array.iter (fun i ->
		if !prev then IO.printf ch ",";
		prev := true;
		IO.printf ch " implements %s" (s_type_path (type_path ctx i));
	) c.cl3_implements;
	IO.printf ch " {\n";
	IO.printf ch "\t"; gen_method ctx ch "new" c.cl3_construct;
	gen_fields ctx ch c.cl3_fields false;
	gen_fields ctx ch s.st3_fields true;
	IO.printf ch "}\n";
	prerr_endline ";";
	IO.close_out ch

let genhx file =
	let file = (try Plugin.find_file file with Not_found -> failwith ("File not found : " ^ file)) in
	let ch = IO.input_channel (open_in_bin file) in
	SwfParser.full_parsing := true;
	let _, swf = Swf.parse ch in
	SwfParser.full_parsing := false;
	IO.close_in ch;
	List.iter (fun t ->
		match t.Swf.tdata with
		| Swf.TActionScript3 (_,t) -> Array.iteri (fun i c -> genhx_class t c t.as3_statics.(i)) t.as3_classes
		| _ -> ()
	) swf

;;
gen_expr_ref := gen_expr
