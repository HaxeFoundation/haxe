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

type tkind =
	| KInt
	| KUInt
	| KFloat
	| KBool
	| KType of as3_name
	| KDynamic

type register = {
	rid : int;
	rtype : tkind;
	mutable rused : bool;
	mutable rinit : bool;
	mutable rcond : bool;
}

type 'a access =
	| VReg of register
	| VId of as3_name
	| VCast of as3_name * tkind
	| VGlobal of as3_name
	| VArray
	| VScope of as3_slot

type local =
	| LReg of register
	| LScope of as3_slot
	| LGlobal of as3_name

type code_infos = {
	mutable iregs : register DynArray.t;
	mutable ipos : int;
	mutable istack : int;
	mutable imax : int;
	mutable iscopes : int;
	mutable imaxscopes : int;
	mutable iloop : int;
	mutable icond : bool;
}

type try_infos = {
	tr_pos : int;
	tr_end : int;
	tr_catch_pos : int;
	tr_type : t;
}

type context = {
	(* globals *)
	strings : string lookup;
	ints : int32 lookup;
	floats : float lookup;
	namespaces : as3_namespace lookup;
	nsets : as3_ns_set lookup;
	names : as3_multi_name lookup;
	mtypes : as3_method_type lookup_nz;
	mutable classes : as3_class list;
	mutable statics : as3_static list;
	functions : as3_function lookup;
	rpublic : as3_namespace index;
	gpublic : as3_ns_set index;
	debug : bool;
	mutable last_line : int;
	boot : string;

	(* per-function *)
	mutable locals : (string,local) PMap.t;
	mutable code : as3_opcode DynArray.t;
	mutable infos : code_infos;
	mutable trys : try_infos list;
	mutable breaks : (unit -> unit) list;
	mutable continues : (int -> unit) list;
	mutable in_static : bool;
	mutable curblock : texpr list;
	mutable block_vars : (as3_slot * string) list;
	mutable try_scope_reg : register option;
}

let error p = Typer.error "Invalid expression" p
let stack_error p = Typer.error "Stack error" p

let index_int (x : int) : 'a index = Obj.magic (x + 1)
let index_nz_int (x : int) : 'a index_nz = Obj.magic x
let tid (x : 'a index) : int = Obj.magic x

let new_lookup() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_int }
let new_lookup_nz() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_nz_int }

let jsize = As3code.length (A3Jump (J3Always,0))

let t_void = TEnum ({
		e_path = [],"Void";
		e_pos = null_pos;
		e_doc = None;
		e_private = false;
		e_extern = false;
		e_types = [];
		e_constrs = PMap.empty;
		e_names = [];
	},[])

let t_string = TInst (mk_class ([],"String") null_pos None false,[])
let t_int = TInst (mk_class ([],"Int") null_pos None false,[])

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
	let s = ctx.infos.istack + As3code.stack_delta op in
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
	write ctx A3Label;
	j , (fun cond ->
		let delta = p + -(ctx.infos.ipos + jsize) in
		write ctx (A3Jump (cond,delta))
	)

let type_path ctx path =
	let pack, name = (match path with
		| [] , "Int" -> [] , "int"
		| [] , "UInt" -> [] , "uint"
		| [] , "Float" -> [] , "Number"
		| [] , "Bool" -> [] , "Boolean"
		| [] , "Void" -> [] , "void"
		| [] , "Enum" -> [] , "Class"
		| ["flash"] , "FlashXml__" -> [] , "Xml"
		| ["flash"] , "Boot" -> [] , ctx.boot
		| _ -> path
	) in
	let pid = string ctx (String.concat "." pack) in
	let nameid = string ctx name in
	let pid = lookup (A3NPublic (Some pid)) ctx.namespaces in
	let tid = lookup (A3MName (nameid,pid)) ctx.names in
	tid

let rec follow_basic t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow_basic t
		| _ -> t)
	| TLazy f ->
		follow_basic (!f())
	| TType (t,tl) when t.t_path <> ([],"Null") && t.t_path <> ([],"UInt") ->
		follow_basic (apply_params t.t_types tl t.t_type)
	| _ -> t

let type_id ctx t =
	match follow_basic t with
	| TEnum ({ e_path = path; e_extern = false },_) ->
		type_path ctx path
	| TInst ({ cl_shadow = true } as c,_) ->
		(match c.cl_implements with
		| [] ->
			(match c.cl_super with
			| None -> type_path ctx ([],"Object")
			| Some (csup,_) -> type_path ctx csup.cl_path)
		| [csup,_] -> type_path ctx csup.cl_path
		| _ -> type_path ctx ([],"Object"))
	| TInst (c,_) ->
		type_path ctx c.cl_path
	| TFun _ ->
		type_path ctx ([],"Function")
	| TEnum ({ e_path = ([],"Class") as path },_)
	| TEnum ({ e_path = ([],"Void") as path },_)
	| TEnum ({ e_path = ([],"Bool") as path },_)
	| TType ({ t_path = ([],"UInt") as path },_) ->
		type_path ctx path
	| _ ->
		type_path ctx ([],"Object")

let classify ctx t =
	match follow_basic t with
	| TInst ({ cl_path = [],"Int" },_) ->
		KInt
	| TInst ({ cl_path = [],"Float" },_) ->
		KFloat
	| TEnum ({ e_path = [],"Bool" },_) ->
		KBool
	| TEnum _
	| TInst _ ->
		KType (type_id ctx t)
	| TType ({ t_path = [],"UInt" },_) ->
		KUInt
	| TFun _
	| TMono _
	| TAnon _
	| TType _
	| TDynamic _ ->
		KDynamic
	| TLazy _ ->
		assert false

let ident ctx i = type_path ctx ([],i)

let as3 ctx p =
	let pid = string ctx p in
	let ns = string ctx "http://adobe.com/AS3/2006/builtin" in
	let nsid = lookup (A3NNamespace ns) ctx.namespaces in
	let tid = lookup (A3MName (pid,nsid)) ctx.names in
	tid

let property ctx p t =
	match follow t with
	| TInst ({ cl_path = [],"Array" },_) ->
		(match p with
		| "length" | "copy" | "insert" | "remove" | "iterator" -> ident ctx p
		| _ -> as3 ctx p);
	| TInst ({ cl_path = [],"String" },_) ->
		(match p with
		| "length" | "charCodeAt" (* use haXe version *) -> ident ctx p
		| _ -> as3 ctx p);
	| _ ->
		ident ctx p

let default_infos() =
	{
		ipos = 0;
		istack = 0;
		imax = 0;
		iregs = DynArray.create();
		iscopes = 0;
		imaxscopes = 0;
		iloop = -1;
		icond = false;
	}

let alloc_reg ctx k =
	let regs = ctx.infos.iregs in
	try
		let p = DynArray.index_of (fun r -> not r.rused && k = r.rtype) regs in
		let r = DynArray.unsafe_get regs p in
		r.rused <- true;
		r.rinit <- false;
		r
	with
		Not_found ->
			let r = {
				rid = DynArray.length regs + 1;
				rused = true;
				rinit = false;
				rtype = k;
				rcond = false;
			} in
			DynArray.add regs r;
			r

let coerce ctx t =
	(* it would be useful to know if we don't already have
	   this type on the stack (as detected by the bytecode verifier)...
	   maybe this get removed at JIT, so it's only useful to reduce codesize
	*)
	write ctx (match t with
		| KInt -> A3ToInt
		| KUInt -> A3ToUInt
		| KFloat -> A3ToNumber
		| KBool -> A3ToBool
		| KType t -> A3AsType t
		| KDynamic -> A3AsAny
	)

let set_reg ctx r =
	if not r.rinit then begin
		r.rinit <- true;
		if ctx.infos.icond then r.rcond <- true;
	end;
	coerce ctx r.rtype;
	write ctx (A3SetReg r.rid)

let free_reg ctx r =
	r.rused <- false

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

let define_local ctx ?(init=false) name t el =
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
			let r = alloc_reg ctx (classify ctx t) in
			r.rinit <- init;
			LReg r
	) in
	ctx.locals <- PMap.add name l ctx.locals

let is_set v = (Obj.magic v) = Write

let gen_local_access ctx name p (forset : 'a)  : 'a access =
	match (try PMap.find name ctx.locals with Not_found -> Typer.error ("Unbound variable " ^ name) p) with
	| LReg r ->
		VReg r
	| LScope n ->
		write ctx (A3GetScope 1);
		VScope n
	| LGlobal id ->
		if is_set forset then write ctx (A3FindProp id);
		VGlobal id

let rec setvar ctx (acc : write access) retval =
	match acc with
	| VReg r ->
		if retval then write ctx A3Dup;
		set_reg ctx r;
	| VGlobal g ->
		if retval then write ctx A3Dup;
		write ctx (A3SetProp g);
	| VId _ | VCast _ | VArray | VScope _ when retval ->
		let r = alloc_reg ctx KDynamic in
		write ctx A3Dup;
		set_reg ctx r;
		setvar ctx acc false;
		write ctx (A3Reg r.rid);
		free_reg ctx r
	| VId id | VCast (id,_) ->
		write ctx (A3InitProp id)
	| VArray ->
		let id_aset = lookup (A3MMultiNameLate ctx.gpublic) ctx.names in
		write ctx (A3SetProp id_aset);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (A3SetSlot n)

let getvar ctx (acc : read access) =
	match acc with
	| VReg r ->
		if not r.rinit then begin
			r.rinit <- true;
			r.rcond <- true;
		end;
		write ctx (A3Reg r.rid)
	| VId id ->
		write ctx (A3GetProp id)
	| VCast (id,t) ->
		write ctx (A3GetProp id);
		coerce ctx t
	| VGlobal g ->
		write ctx (A3GetLex g);
	| VArray ->
		let id_aget = lookup (A3MMultiNameLate ctx.gpublic) ctx.names in
		write ctx (A3GetProp id_aget);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (A3GetSlot n)

let open_block ctx el retval =
	let old_stack = ctx.infos.istack in
	let old_regs = DynArray.map (fun r -> r.rused) ctx.infos.iregs in
	let old_locals = ctx.locals in
	let old_block = ctx.curblock in
	ctx.curblock <- el;
	(fun() ->
		if ctx.infos.istack <> old_stack + (if retval then 1 else 0) then assert false;
		let rcount = DynArray.length old_regs + 1 in
		DynArray.iter (fun r ->
			if r.rid < rcount then
				r.rused <- DynArray.unsafe_get old_regs (r.rid - 1)
			else
				r.rused <- false
		) ctx.infos.iregs;
		ctx.locals <- old_locals;
		ctx.curblock <- old_block;
	)

let begin_branch ctx =
	if ctx.infos.icond then
		(fun() -> ())
	else begin
		ctx.infos.icond <- true;
		(fun() -> ctx.infos.icond <- false)
	end

let debug ctx p =
	let line = Lexer.get_error_line p in
	if ctx.last_line <> line then begin
		write ctx (A3DebugLine line);
		ctx.last_line <- line;
	end

let begin_fun ctx args tret el stat =
	let old_locals = ctx.locals in
	let old_code = ctx.code in
	let old_infos = ctx.infos in
	let old_trys = ctx.trys in
	let old_bvars = ctx.block_vars in
	let old_static = ctx.in_static in
	let last_line = ctx.last_line in
	let old_treg = ctx.try_scope_reg in
	ctx.infos <- default_infos();
	ctx.code <- DynArray.create();
	ctx.trys <- [];
	ctx.block_vars <- [];
	ctx.in_static <- stat;
	ctx.last_line <- -1;
	(match el with
	| [] -> ()
	| e :: _ ->
		if ctx.debug then begin
			write ctx (A3DebugFile (lookup e.epos.pfile ctx.strings));
			debug ctx e.epos
		end);
	ctx.locals <- PMap.foldi (fun name l acc ->
		match l with
		| LReg _ -> acc
		| LScope _ -> PMap.add name (LGlobal (type_path ctx ([],name))) acc
		| LGlobal _ -> PMap.add name l acc
	) ctx.locals PMap.empty;
	List.iter (fun (name,_,t) ->
		define_local ctx name ~init:true t el;
		match gen_local_access ctx name null_pos Write with
		| VReg _ -> ()
		| acc ->
			let r = alloc_reg ctx (classify ctx t) in
			write ctx (A3Reg r.rid);
			setvar ctx acc false
	) args;
	let args, varargs = (match args with
		| ["__arguments__",_,_] -> [], true
		| _ -> args, false
	) in
	let rec loop_try e =
		match e.eexpr with
		| TFunction _ -> ()
		| TTry _ -> raise Exit
		| _ -> Type.iter loop_try e
	in
	ctx.try_scope_reg <- (try List.iter loop_try el; None with Exit -> Some (alloc_reg ctx KDynamic));
	(fun () ->
		let hasblock = ctx.block_vars <> [] || ctx.trys <> [] in
		let dparams = ref None in
		List.iter (fun (_,opt,t) ->
			match !dparams with
			| None -> if opt then dparams := Some [A3VNone]
			| Some l -> dparams := Some (A3VNone :: l)
		) args;
		let mt = {
			mt3_ret = Some (type_id ctx tret);
			mt3_args = List.map (fun (_,_,t) -> Some (type_id ctx t)) args;
			mt3_native = false;
			mt3_var_args = varargs;
			mt3_debug_name = None;
			mt3_dparams = !dparams;
			mt3_pnames = None;
			mt3_new_block = hasblock;
			mt3_unused_flag = false;
			mt3_arguments_defined = false;
			mt3_uses_dxns = false;
		} in
		let code = DynArray.to_list ctx.code in
		let extra = (
			if hasblock then begin
				let scope = (match ctx.try_scope_reg with
					| None -> [A3Scope]
					| Some r -> [A3Dup; A3SetReg r.rid; A3Scope]
				) in
				A3This :: A3Scope :: A3NewBlock :: scope
			end else if not stat then
				[A3This; A3Scope]
			else
				[]
		) in
		(* add dummy registers initialization *)
		let extra = extra @ List.concat (List.map (fun r ->
			if not r.rcond then
				[]
			else
			let s = [A3SetReg r.rid] in
			match r.rtype with
			| KInt -> A3SmallInt 0 :: s
			| KUInt -> A3SmallInt 0 :: A3ToUInt :: s
			| KFloat -> A3NaN :: s
			| KBool -> A3False :: s
			| KType t -> A3Null :: A3AsType t :: s
			| KDynamic -> A3Null :: A3AsAny :: s
		) (DynArray.to_list ctx.infos.iregs)) in
		let delta = List.fold_left (fun acc r -> As3code.length r + acc) 0 extra in
		let f = {
			fun3_id = add mt ctx.mtypes;
			fun3_stack_size = (if ctx.infos.imax = 0 && (hasblock || not stat) then 1 else ctx.infos.imax);
			fun3_nregs = DynArray.length ctx.infos.iregs + 1;
			fun3_init_scope = 1;
			fun3_max_scope = ctx.infos.imaxscopes + 1 + (if hasblock then 2 else if not stat then 1 else 0);
			fun3_code = extra @ code;
			fun3_trys = Array.of_list (List.map (fun t ->
				{
					tc3_start = t.tr_pos + delta;
					tc3_end = t.tr_end + delta;
					tc3_handle = t.tr_catch_pos + delta;
					tc3_type = (match follow t.tr_type with
						| TInst (c,_) -> Some (type_path ctx c.cl_path)
						| TEnum (e,_) -> Some (type_path ctx e.e_path)
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
		ctx.last_line <- last_line;
		ctx.try_scope_reg <- old_treg;
		f.fun3_id
	)

let empty_method ctx =
	let f = begin_fun ctx [] t_void [] true in
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

let gen_constant ctx c t p =
	match c with
	| TInt i ->
		if Int32.compare i (-128l) > 0 && Int32.compare i 128l < 0 then
			write ctx (A3SmallInt (Int32.to_int i))
		else
			write ctx (A3IntRef (lookup i ctx.ints));
	| TFloat f ->
		let f = float_of_string f in
		write ctx (A3Float (lookup f ctx.floats));
	| TString s ->
		write ctx (A3String (lookup s ctx.strings));
	| TBool b ->
		write ctx (if b then A3True else A3False);
	| TNull ->
		write ctx A3Null;
		(match classify ctx t with
		| KInt | KBool | KUInt | KFloat ->
			Typer.error ("In Flash9, null can't be used as basic type " ^ s_type (print_context()) t) p
		| x -> coerce ctx x)
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
	| TField (e1,f) ->
		let id = property ctx f e1.etype in
		(match e1.eexpr with
		| TConst TThis when not ctx.in_static -> write ctx (A3FindPropStrict id)
		| _ -> gen_expr ctx true e1);
		(match follow e1.etype with
		| TInst _ | TEnum _ -> VId id
		| TAnon a when (match !(a.a_status) with Statics _ | EnumStatics _ -> true | _ -> false) -> VId id
		| _ -> VCast (id,classify ctx e.etype))
	| TArray ({ eexpr = TLocal "__global__" },{ eexpr = TConst (TString s) }) ->
		let path = (match List.rev (ExtString.String.nsplit s ".") with [] -> assert false | x :: l -> List.rev l, x) in
		let id = type_path ctx path in
		if is_set forset then write ctx A3GetGlobalScope;
		VGlobal id
	| TArray (e,eindex) ->
		gen_expr ctx true e;
		gen_expr ctx true eindex;
		VArray
	| TTypeExpr t ->
		let id = type_path ctx (t_path t) in
		if is_set forset then write ctx A3GetGlobalScope;
		VGlobal id
	| _ ->
		error e.epos

let rec gen_expr_content ctx retval e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx c e.etype e.epos
	| TThrow e ->
		gen_expr ctx true e;
		write ctx A3Throw;
		no_value ctx retval;
	| TParenthesis e ->
		gen_expr ctx retval e
	| TEnumField (e,s) ->
		let id = type_path ctx e.e_path in
		write ctx (A3GetLex id);
		write ctx (A3GetProp (ident ctx s));
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
		let b = open_block ctx [] retval in
		loop el;
		b();
	| TVars vl ->
		List.iter (fun (v,t,ei) ->
			define_local ctx v t ctx.curblock;
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
	| TTypeExpr _ ->
		getvar ctx (gen_access ctx e Read)
	| TArray _ ->
		getvar ctx (gen_access ctx e Read);
		coerce ctx (classify ctx e.etype)
	| TBinop (op,e1,e2) ->
		gen_binop ctx retval op e1 e2 e.etype
	| TCall (e,el) ->
		gen_call ctx retval e el
	| TNew (c,_,pl) ->
		let id = type_path ctx c.cl_path in
		write ctx (A3FindPropStrict id);
		List.iter (gen_expr ctx true) pl;
		write ctx (A3ConstructProperty (id,List.length pl))
	| TFunction f ->
		write ctx (A3Function (generate_function ctx f true))
	| TIf (e0,e1,e2) ->
		gen_expr ctx true e0;
		let branch = begin_branch ctx in
		let j = jump ctx J3False in
		gen_expr ctx retval e1;
		let t = classify ctx e.etype in
		if retval && classify ctx e1.etype <> t then coerce ctx t;
		(match e2 with
		| None -> j()
		| Some e ->
			(* two expresssions, but one per branch *)
			if retval then ctx.infos.istack <- ctx.infos.istack - 1;
			let jend = jump ctx J3Always in
			j();
			gen_expr ctx retval e;
			if retval && classify ctx e.etype <> t then coerce ctx t;
			jend());
		branch();
	| TWhile (econd,e,flag) ->
		let jstart = (match flag with NormalWhile -> (fun()->()) | DoWhile -> jump ctx J3Always) in
		let end_loop = begin_loop ctx in
		let branch = begin_branch ctx in
		let continue_pos = ctx.infos.ipos + jsize in
		let here, loop = jump_back ctx in
		here();
		gen_expr ctx true econd;
		let jend = jump ctx J3False in
		jstart();
		gen_expr ctx false e;
		loop J3Always;
		jend();
		branch();
		end_loop continue_pos;
		if retval then write ctx A3Null
	| TUnop (op,flag,e) ->
		gen_unop ctx retval op flag e
	| TTry (e2,cases) ->
		if ctx.infos.istack <> 0 then Typer.error "Cannot compile try/catch as a right-side expression in Flash9" e.epos;
		let branch = begin_branch ctx in
		let p = ctx.infos.ipos in
		gen_expr ctx retval e2;
		let pend = ctx.infos.ipos in
		let jend = jump ctx J3Always in
		let rec loop ncases = function
			| [] -> []
			| (ename,t,e) :: l ->
				let b = open_block ctx [e] retval in
				ctx.trys <- {
					tr_pos = p;
					tr_end = pend;
					tr_catch_pos = ctx.infos.ipos;
					tr_type = t;
				} :: ctx.trys;
				ctx.infos.istack <- ctx.infos.istack + 1;
				if ctx.infos.imax < ctx.infos.istack then ctx.infos.imax <- ctx.infos.istack;
				write ctx A3This;
				write ctx A3Scope;
				write ctx (A3Reg (match ctx.try_scope_reg with None -> assert false | Some r -> r.rid));
				write ctx A3Scope;
				define_local ctx ename t [e];
				let r = (try match PMap.find ename ctx.locals with LReg r -> Some (alloc_reg ctx r.rtype) | _ -> None with Not_found -> assert false) in
				(match r with None -> () | Some r -> set_reg ctx r);
				let acc = gen_local_access ctx ename e.epos Write in
				(match r with None -> () | Some r -> write ctx (A3Reg r.rid));
				setvar ctx acc false;
				gen_expr ctx retval e;
				b();
				if retval then ctx.infos.istack <- ctx.infos.istack - 1;
				match l with
				| [] -> []
				| _ ->
					let j = jump ctx J3Always in
					j :: loop (ncases + 1) l
		in
		let loops = loop (List.length ctx.trys) cases in
		List.iter (fun j -> j()) loops;
		branch();
		jend()
	| TFor (v,t,it,e) ->
		gen_expr ctx true it;
		let r = alloc_reg ctx KDynamic in
		set_reg ctx r;
		let branch = begin_branch ctx in
		let b = open_block ctx [e] retval in
		define_local ctx v t [e];
		let end_loop = begin_loop ctx in
		let continue_pos = ctx.infos.ipos + jsize in
		let here, start = jump_back ctx in
		here();
		write ctx (A3Reg r.rid);
		write ctx (A3CallProperty (ident ctx "hasNext",0));
		let jend = jump ctx J3False in
		let acc = gen_local_access ctx v e.epos Write in
		write ctx (A3Reg r.rid);
		write ctx (A3CallProperty (ident ctx "next",0));
		setvar ctx acc false;
		gen_expr ctx false e;
		start J3Always;
		end_loop continue_pos;
		jend();
		if retval then getvar ctx (gen_local_access ctx v e.epos Read);
		b();
		branch();
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
	| TSwitch (e0,el,eo) ->
		let t = classify ctx e.etype in
		let r = alloc_reg ctx (classify ctx e0.etype) in
		gen_expr ctx true e0;
		set_reg ctx r;
		let branch = begin_branch ctx in
		let prev = ref (fun () -> ()) in
		let jend = List.map (fun (vl,e) ->
			(!prev)();
			let rec loop = function
				| [] ->
					assert false
				| [v] ->
					write ctx (A3Reg r.rid);
					gen_expr ctx true v;
					prev := jump ctx J3Neq;
				| v :: l ->
					write ctx (A3Reg r.rid);
					gen_expr ctx true v;
					let j = jump ctx J3Eq in
					loop l;
					j()
			in
			loop vl;
			gen_expr ctx retval e;
			if retval then begin
				if classify ctx e.etype <> t then coerce ctx t;
				ctx.infos.istack <- ctx.infos.istack - 1;
			end;
			jump ctx J3Always
		) el in
		(!prev)();
		free_reg ctx r;
		(match eo with
		| None ->
			if retval then begin
				write ctx A3Null;
				coerce ctx t;
			end;
		| Some e ->
			gen_expr ctx retval e;
			if retval && classify ctx e.etype <> t then coerce ctx t;
		);
		List.iter (fun j -> j()) jend;
		branch();
	| TMatch (e0,_,cases,def) ->
		let t = classify ctx e.etype in
		let rparams = alloc_reg ctx KDynamic in
		let rindex = alloc_reg ctx KInt in
		gen_expr ctx true e0;
		write ctx A3Dup;
		write ctx (A3GetProp (ident ctx "index"));
		set_reg ctx rindex;
		write ctx (A3GetProp (ident ctx "params"));
		set_reg ctx rparams;
		let branch = begin_branch ctx in
		let prev = ref (fun () -> ()) in
		let jend = List.map (fun (cl,params,e) ->
			(!prev)();
			let rec loop = function
				| [] ->
					assert false
				| [tag] ->
					write ctx (A3Reg rindex.rid);
					write ctx (A3Int tag);
					prev := jump ctx J3Neq;
				| tag :: l ->
					write ctx (A3Reg rindex.rid);
					write ctx (A3Int tag);
					let j = jump ctx J3Eq in
					loop l;
					j()
			in
			loop cl;
			let b = open_block ctx [e] retval in
			(match params with
			| None -> ()
			| Some l ->
				let p = ref (-1) in
				List.iter (fun (name,t) ->
					incr p;
					match name with
					| None -> ()
					| Some v ->
						define_local ctx v t [e];
						let acc = gen_local_access ctx v e.epos Write in
						write ctx (A3Reg rparams.rid);
						write ctx (A3SmallInt !p);
						getvar ctx VArray;
						setvar ctx acc false
				) l
			);
			gen_expr ctx retval e;
			b();
			if retval then begin
				ctx.infos.istack <- ctx.infos.istack - 1;
				if classify ctx e.etype <> t then coerce ctx t;
			end;
			jump ctx J3Always;
		) cases in
		(!prev)();
		(match def with
		| None ->
			if retval then begin
				write ctx A3Null;
				coerce ctx t;
			end;
		| Some e ->
			gen_expr ctx retval e;
			if retval && classify ctx e.etype <> t then coerce ctx t;
		);
		List.iter (fun j -> j()) jend;
		branch();
		free_reg ctx rindex;
		free_reg ctx rparams

and gen_call ctx retval e el =
	match e.eexpr , el with
	| TLocal "__is__" , [e;t] ->
		gen_expr ctx true e;
		gen_expr ctx true t;
		write ctx (A3Op A3OIs)
	| TLocal "__as__" , [e;t] ->
		gen_expr ctx true e;
		gen_expr ctx true t;
		write ctx (A3Op A3OAs)
	| TLocal "__keys__" , [e] ->
		let racc = alloc_reg ctx (KType (type_path ctx ([],"Array"))) in
		let rcounter = alloc_reg ctx KInt in
		let rtmp = alloc_reg ctx KDynamic in
		write ctx (A3SmallInt 0);
		set_reg ctx rcounter;
		write ctx (A3Array 0);
		set_reg ctx racc;
		gen_expr ctx true e;
		set_reg ctx rtmp;
		let start, loop = jump_back ctx in
		write ctx (A3Reg racc.rid);
		write ctx (A3Reg rtmp.rid);
		write ctx (A3Reg rcounter.rid);
		write ctx A3ForIn;
		write ctx (A3CallPropVoid (ident ctx "push",1));
		start();
		write ctx (A3Next (rtmp.rid,rcounter.rid));
		loop J3True;
		write ctx (A3Reg racc.rid);
		free_reg ctx rtmp;
		free_reg ctx rcounter;
		free_reg ctx racc;
	| TLocal "__new__" , e :: el ->
		gen_expr ctx true e;
		List.iter (gen_expr ctx true) el;
		write ctx (A3Construct (List.length el))
	| TLocal "__delete__" , [o;f] ->
		gen_expr ctx true o;
		gen_expr ctx true f;
		write ctx (A3DeleteProp (lookup (A3MMultiNameLate ctx.gpublic) ctx.names));
	| TLocal "__unprotect__" , [e] ->
		gen_expr ctx true e
	| TLocal "__typeof__", [e] ->
		gen_expr ctx true e;
		write ctx A3Typeof
	| TLocal "__in__", [e; f] ->
		gen_expr ctx true e;
		gen_expr ctx true f;
		write ctx (A3Op A3OIn)
	| TArray ({ eexpr = TLocal "__global__" },{ eexpr = TConst (TString s) }), _ ->
		(match gen_access ctx e Read with
		| VGlobal id ->
			write ctx (A3FindPropStrict id);
			List.iter (gen_expr ctx true) el;
			write ctx (A3CallProperty (id,List.length el));
		| _ -> assert false)
	| TConst TSuper , _ ->
		write ctx A3This;
		List.iter (gen_expr ctx true) el;
		write ctx (A3ConstructSuper (List.length el));
	| TField ({ eexpr = TConst TSuper },f) , _ ->
		let id = ident ctx f in
		write ctx (A3FindPropStrict id);
		List.iter (gen_expr ctx true) el;
		write ctx (A3CallSuper (id,List.length el));
	| TField ({ eexpr = TConst TThis },f) , _ when not ctx.in_static ->
		let id = ident ctx f in
		write ctx (A3FindPropStrict id);
		List.iter (gen_expr ctx true) el;
		write ctx (if retval then A3CallProperty (id,List.length el) else A3CallPropVoid (id,List.length el));
	| TField (e1,f) , _ ->
		gen_expr ctx true e1;
		List.iter (gen_expr ctx true) el;
		if not retval then
			write ctx (A3CallPropVoid (property ctx f e1.etype,List.length el))
		else
			let coerce() =
				match follow e.etype with
				| TFun (_,r) -> coerce ctx (classify ctx r)
				| _ -> ()
			in
			write ctx (A3CallProperty (property ctx f e1.etype,List.length el));
			(match follow e1.etype with
			| TInst ({ cl_path = [],"Array" },_) -> 
				(match f with
				| "copy" | "remove" -> coerce()
				| _ -> ())
			| TInst ({ cl_path = [],"Date" },_) -> 
				coerce() (* all date methods are typed as Number in AS3 and Int in haXe *) 
			| TAnon a when (match !(a.a_status) with Statics { cl_path = ([],"Date") } -> true | _ -> false) ->
				(match f with
				| "now" | "fromString" | "fromTime"  -> coerce()
				| _ -> ())
			| TAnon a when (match !(a.a_status) with Statics { cl_path = ([],"Math") } -> true | _ -> false) ->
				(match f with
				| "isFinite" | "isNaN" -> coerce()
				| _ -> ())
			| _ -> ())		
	| TEnumField (e,f) , _ ->
		let id = type_path ctx e.e_path in
		write ctx (A3GetLex id);
		List.iter (gen_expr ctx true) el;
		write ctx (A3CallProperty (ident ctx f,List.length el));
	| _ ->
		gen_expr ctx true e;
		write ctx A3GetGlobalScope;
		List.iter (gen_expr ctx true) el;
		write ctx (A3CallStack (List.length el))

and gen_unop ctx retval op flag e =
	let k = classify ctx e.etype in
	match op with
	| Not ->
		gen_expr ctx true e;
		write ctx (A3Op A3ONot);
	| Neg ->
		gen_expr ctx true e;
		write ctx (A3Op (if k = KInt then A3OINeg else A3ONeg));
	| NegBits ->
		gen_expr ctx true e;
		write ctx (A3Op A3OBitNot);
	| Increment
	| Decrement ->
		let incr = (op = Increment) in
		let acc = gen_access ctx e Write in (* for set *)
		match acc with
		| VReg r when not retval && r.rtype = KInt ->
			if not r.rinit then r.rcond <- true;
			write ctx (if incr then A3IncrIReg r.rid else A3DecrIReg r.rid)
		| _ ->
		getvar ctx (gen_access ctx e Read);
		match flag with
		| Postfix when retval ->
			let r = alloc_reg ctx KDynamic in
			write ctx A3Dup;
			set_reg ctx r;
			write ctx (A3Op (if incr then A3OIncr else A3ODecr));
			setvar ctx acc false;
			write ctx (A3Reg r.rid);
			free_reg ctx r
		| Postfix | Prefix ->
			write ctx (A3Op (if incr then A3OIncr else A3ODecr));
			setvar ctx acc retval

and gen_binop ctx retval op e1 e2 t =
	let gen_op ?iop o =
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		match iop with
		| Some iop -> 
			let k1 = classify ctx e1.etype in
			let k2 = classify ctx e2.etype in
			if k1 = KInt && k2 = KInt then
				write ctx (A3Op iop)
			else begin
				write ctx (A3Op o);
				if o = A3OAdd then coerce ctx (classify ctx t);
			end;
		| _ ->
			write ctx (A3Op o)
	in
	match op with
	| OpAssign ->
		let acc = gen_access ctx e1 Write in
		gen_expr ctx true e2;
		setvar ctx acc retval
	| OpBoolAnd ->
		write ctx A3False;
		gen_expr ctx true e1;
		let j = jump ctx J3False in
		write ctx A3Pop;
		gen_expr ctx true e2;
		coerce ctx KBool;
		j();
	| OpBoolOr ->
		write ctx A3True;
		gen_expr ctx true e1;
		let j = jump ctx J3True in
		write ctx A3Pop;
		gen_expr ctx true e2;
		coerce ctx KBool;
		j();
	| OpAssignOp op ->
		let acc = gen_access ctx e1 Write in
		gen_binop ctx true op e1 e2 t;
		setvar ctx acc retval
	| OpAdd ->
		gen_op ~iop:A3OIAdd A3OAdd
	| OpMult ->
		gen_op ~iop:A3OIMul A3OMul
	| OpDiv ->
		gen_op A3ODiv
	| OpSub ->
		gen_op ~iop:A3OISub A3OSub
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
	if ctx.debug then debug ctx e.epos;
	gen_expr_content ctx retval e;
	if old <> ctx.infos.istack then begin
		if old + 1 <> ctx.infos.istack then stack_error e.epos;
		if not retval then write ctx A3Pop;
	end else if retval then stack_error e.epos

and generate_function ctx fdata stat =
	let f = begin_fun ctx fdata.tf_args fdata.tf_type [fdata.tf_expr] stat in
	gen_expr ctx false fdata.tf_expr;
	write ctx A3RetVoid;
	f()

let generate_method ctx fdata stat =
	generate_function ctx { fdata with tf_expr = Transform.block_vars fdata.tf_expr } stat

let generate_construct ctx fdata cfields =
	(* make all args optional to allow no-param constructor *)
	let f = begin_fun ctx (List.map (fun (a,o,t) -> a,true,t) fdata.tf_args) fdata.tf_type [fdata.tf_expr] false in
	(* if skip_constructor, then returns immediatly *)
	let id = ident ctx "skip_constructor" in
	getvar ctx (VGlobal (type_path ctx ([],ctx.boot)));
	getvar ctx (VId id);
	let j = jump ctx J3False in
	write ctx A3RetVoid;
	j();
	(* --- *)
	PMap.iter (fun _ f ->
		match f.cf_expr with
		| Some { eexpr = TFunction fdata } when f.cf_set = NormalAccess ->
			let id = ident ctx f.cf_name in
			write ctx (A3FindProp id);
			write ctx (A3Function (generate_method ctx fdata false));
			write ctx (A3InitProp id);
		| _ -> ()
	) cfields;
	gen_expr ctx false (Transform.block_vars fdata.tf_expr);
	write ctx A3RetVoid;
	f() , List.length fdata.tf_args

let generate_class_init ctx c slot =
	write ctx A3GetGlobalScope;
	if c.cl_interface then
		write ctx A3Null
	else begin
		let path = (match c.cl_super with None -> ([],"Object") | Some (sup,_) -> sup.cl_path) in
		write ctx (A3GetLex (type_path ctx path));
		write ctx A3Scope;
		write ctx (A3GetLex (type_path ctx path));
	end;
	write ctx (A3ClassDef (As3parse.magic_index slot));
	List.iter (fun f ->
		match f.cf_expr with
		| Some { eexpr = TFunction fdata } when f.cf_set = NormalAccess ->
			write ctx A3Dup;
			write ctx (A3Function (generate_method ctx fdata true));
			write ctx (A3InitProp (ident ctx f.cf_name));
		| _ -> ()
	) c.cl_ordered_statics;
	if not c.cl_interface then write ctx A3PopScope;
	write ctx (A3InitProp (type_path ctx c.cl_path))

let generate_class_statics ctx c =
	let r = alloc_reg ctx KDynamic in
	let first = ref true in
	let nslot = ref 0 in
	List.iter (fun f ->
		match f.cf_expr with
		| Some { eexpr = TFunction _ } when f.cf_set <> NormalAccess -> ()
		| Some e ->
			incr nslot;
			if !first then begin
				write ctx A3GetGlobalScope;
				write ctx (A3GetProp (type_path ctx c.cl_path));
				write ctx (A3SetReg r.rid); (* needed for setslot *)
				first := false;
			end;
			write ctx (A3Reg r.rid);
			gen_expr ctx true (Transform.block_vars e);
			write ctx (A3SetSlot !nslot);
		| _ ->
			incr nslot
	) c.cl_ordered_statics;
	free_reg ctx r

let generate_enum_init ctx e slot =
	let path = ([],"Object") in
	let name_id = type_path ctx e.e_path in
	write ctx A3GetGlobalScope;
	write ctx (A3GetLex (type_path ctx path));
	write ctx A3Scope;
	write ctx (A3GetLex (type_path ctx path));
	write ctx (A3ClassDef (As3parse.magic_index slot));
	write ctx A3PopScope;
	let r = alloc_reg ctx KDynamic in
	write ctx A3Dup;
	write ctx (A3SetReg r.rid); (* needed for setslot *)
	write ctx (A3InitProp name_id);
	let nslot = ref 0 in
	PMap.iter (fun _ f ->
		incr nslot;
		match f.ef_type with
		| TFun _ -> ()
		| _ ->
			write ctx (A3Reg r.rid);
			write ctx (A3FindPropStrict name_id);
			write ctx (A3String (lookup f.ef_name ctx.strings));
			write ctx (A3Int f.ef_index);
			write ctx A3Null;
			write ctx (A3ConstructProperty (name_id,3));
			write ctx (A3SetSlot !nslot);
	) e.e_constrs;
	write ctx (A3Reg r.rid);
	List.iter (fun n -> write ctx (A3String (lookup n ctx.strings))) e.e_names;
	write ctx (A3Array (List.length e.e_names));
	write ctx (A3SetSlot (!nslot + 1));
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
		if f.cf_set = NormalAccess then
			Some (A3FVar {
				v3_type = None;
				v3_value = A3VNone;
				v3_const = false;
			})
		else
			Some (A3FMethod {
				m3_type = generate_method ctx fdata stat;
				m3_final = stat;
				m3_override = not stat && loop c;
				m3_kind = MK3Normal;
			})
	| _ when c.cl_interface && not stat ->
		None
	| _ when f.cf_get = ResolveAccess ->
		None
	| _ ->
		Some (A3FVar {
			v3_type = Some (type_id ctx f.cf_type);
			v3_value = A3VNone;
			v3_const = false;
		})

let generate_class ctx c =
	let name_id = type_path ctx c.cl_path in
	let st_id = empty_method ctx in
	let cid , cnargs = (match c.cl_constructor with
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
					mt3_arguments_defined = false;
					mt3_uses_dxns = false;
					mt3_unused_flag = false;
				} in
				add mt0 ctx.mtypes, 0
			end else
				generate_construct ctx {
					tf_args = [];
					tf_type = t_void;
					tf_expr = {
						eexpr = TBlock [];
						etype = t_void;
						epos = null_pos;
					}
				} c.cl_fields
		| Some f ->
			match f.cf_expr with
			| Some { eexpr = TFunction fdata } -> generate_construct ctx fdata c.cl_fields
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
		cl3_sealed = c.cl_path <> (["flash"],"Boot") && c.cl_dynamic = None;
		cl3_final = false;
		cl3_interface = c.cl_interface;
		cl3_namespace = None;
		cl3_implements = Array.of_list (List.map (fun (c,_) ->
			if not c.cl_interface then Typer.error "Can't implement class in Flash9" c.cl_pos;
			type_path ctx c.cl_path
		) c.cl_implements);
		cl3_construct = cid;
		cl3_fields = fields;
	} in
	let st_field_count = ref 0 in
	let st_meth_count = ref 0 in
	let st = {
		st3_method = st_id;
		st3_fields = Array.of_list (List.map (fun f ->
			let k = (match generate_field_kind ctx f c true with None -> assert false | Some k -> k) in
			let count = (match k with A3FMethod _ -> st_meth_count | A3FVar _ -> st_field_count | _ -> assert false) in
			incr count;
			{
				f3_name = ident ctx f.cf_name;
				f3_slot = !count;
				f3_kind = k;
				f3_metas = None;
			}
		) c.cl_ordered_statics)
	} in
	ctx.classes <- sc :: ctx.classes;
	ctx.statics <- st :: ctx.statics

let generate_enum ctx e =
	let name_id = type_path ctx e.e_path in
	let st_id = empty_method ctx in
	let f = begin_fun ctx [("tag",false,t_string);("index",false,t_int);("params",false,mk_mono())] t_void [] false in
	let tag_id = ident ctx "tag" in
	let index_id = ident ctx "index" in
	let params_id = ident ctx "params" in
	write ctx (A3FindProp tag_id);
	write ctx (A3Reg 1);
	write ctx (A3InitProp tag_id);
	write ctx (A3FindProp index_id);
	write ctx (A3Reg 2);
	write ctx (A3InitProp index_id);
	write ctx (A3FindProp params_id);
	write ctx (A3Reg 3);
	write ctx (A3InitProp params_id);
	write ctx A3RetVoid;
	let construct = f() in
	let f = begin_fun ctx [] t_string [] true in
	write ctx (A3GetLex (type_path ctx ([],ctx.boot)));
	write ctx A3This;
	write ctx (A3CallProperty (ident ctx "enum_to_string",1));
	write ctx A3Ret;
	let tostring = f() in
	let sc = {
		cl3_name = name_id;
		cl3_super = Some (type_path ctx ([],"Object"));
		cl3_sealed = true;
		cl3_final = true;
		cl3_interface = false;
		cl3_namespace = None;
		cl3_implements = [||];
		cl3_construct = construct;
		cl3_fields = [|
			{ f3_name = tag_id; f3_slot = 0; f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false; }; f3_metas = None };
			{ f3_name = index_id; f3_slot = 0; f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false; }; f3_metas = None };
			{ f3_name = params_id; f3_slot = 0; f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false; }; f3_metas = None };
			{ f3_name = ident ctx "__enum__"; f3_slot = 0; f3_kind = A3FVar { v3_type = None; v3_value = A3VBool true; v3_const = true }; f3_metas = None };
			{
				f3_name = ident ctx "toString";
				f3_slot = 0;
				f3_kind = A3FMethod {
					m3_type = tostring;
					m3_final = true;
					m3_override = false;
					m3_kind = MK3Normal;
				};
				f3_metas = None;
			};
		|];
	} in
	let st_count = ref 0 in
	let constrs = PMap.fold (fun f acc ->
		incr st_count;
		{
			f3_name = ident ctx f.ef_name;
			f3_slot = !st_count;
			f3_kind = (match f.ef_type with
				| TFun (args,_) ->
					let fdata = begin_fun ctx args (TEnum (e,[])) [] true in
					write ctx (A3FindPropStrict name_id);
					write ctx (A3String (lookup f.ef_name ctx.strings));
					write ctx (A3Int f.ef_index);
					let n = ref 0 in
					List.iter (fun _ -> incr n; write ctx (A3Reg !n)) args;
					write ctx (A3Array (!n));
					write ctx (A3ConstructProperty (name_id,3));
					write ctx A3Ret;
					let fid = fdata() in
					A3FMethod {
						m3_type = fid;
						m3_final = true;
						m3_override = false;
						m3_kind = MK3Normal;
					}
				| _ ->
					A3FVar { v3_type = (Some name_id); v3_value = A3VNone; v3_const = false; }
			);
			f3_metas = None;
		} :: acc
	) e.e_constrs [] in
	let st = {
		st3_method = st_id;
		st3_fields = Array.of_list ({
			f3_name = ident ctx "__isenum";
			f3_slot = !st_count + 2;
			f3_kind = A3FVar { v3_type = None; v3_value = A3VBool true; v3_const = true; };
			f3_metas = None;
		} :: {
			f3_name = ident ctx "__constructs__";
			f3_slot = !st_count + 1;
			f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false; };
			f3_metas = None;
		} :: constrs)
	} in
	ctx.classes <- sc :: ctx.classes;
	ctx.statics <- st :: ctx.statics

let generate_type ctx t =
	match t with
	| TClassDecl c -> if not c.cl_extern then generate_class ctx c
	| TTypeDecl _ -> ()
	| TEnumDecl e -> if not e.e_extern then generate_enum ctx e

let generate_resources ctx hres =
	write ctx A3GetGlobalScope;
	write ctx (A3GetProp (type_path ctx ([],ctx.boot)));
	let id = type_path ctx (["flash";"utils"],"Dictionary") in
	write ctx (A3FindPropStrict id);
	write ctx (A3ConstructProperty (id,0));
	let r = alloc_reg ctx (KType id) in
	set_reg ctx r;
	Hashtbl.iter (fun name data ->
		write ctx (A3Reg r.rid);
		write ctx (A3String (lookup name ctx.strings));
		write ctx (A3String (lookup data ctx.strings));
		setvar ctx VArray false;
	) hres;
	write ctx (A3Reg r.rid);
	write ctx (A3InitProp (ident ctx "__res"))

let generate_inits ctx types hres =
	let f = begin_fun ctx [] t_void [] false in
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
		| TEnumDecl e when not e.e_extern ->
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
	write ctx A3GetGlobalScope;
	write ctx (A3GetProp (type_path ctx ([],ctx.boot)));
	let finit = begin_fun ctx [] t_void [] true in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			(match c.cl_init with
			| None -> ()
			| Some e -> gen_expr ctx false (Transform.block_vars e));
		| _ -> ()
	) types;
	List.iter (fun t ->
		match t with
		| TClassDecl c -> generate_class_statics ctx c
		| _ -> ()
	) types;
	write ctx A3RetVoid;
	write ctx (A3Function (finit()));
	write ctx (A3InitProp (ident ctx "init"));

	(* generate resources *)
	generate_resources ctx hres;

	write ctx A3RetVoid;
	{
		st3_method = f();
		st3_fields = Array.of_list (List.rev classes);
	}

let generate types hres =
	let namespaces = new_lookup() in
	let strings = new_lookup() in
	let nsets = new_lookup() in
	let empty_id = lookup "" strings in
	let rpublic = lookup (A3NPublic (Some empty_id)) namespaces in
	Random.self_init();
	let ctx = {
		boot = "Boot_" ^ Printf.sprintf "%X" (Random.int 0xFFFFFF);
		strings = strings;
		ints = new_lookup();
		floats = new_lookup();
		namespaces = namespaces;
		nsets = nsets;
		names = new_lookup();
		mtypes = new_lookup_nz();
		rpublic = rpublic;
		gpublic = lookup [rpublic] nsets;
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
		debug = Plugin.defined "debug";
		last_line = -1;
		try_scope_reg = None;
	} in
	List.iter (generate_type ctx) types;
	let init = generate_inits ctx types hres in
	let a = {
		as3_ints = lookup_array ctx.ints;
		as3_uints = [||];
		as3_floats = lookup_array ctx.floats;
		as3_idents = lookup_array ctx.strings;
		as3_namespaces = lookup_array ctx.namespaces;
		as3_nsets = lookup_array ctx.nsets;
		as3_names = lookup_array ctx.names;
		as3_method_types = lookup_array ctx.mtypes;
		as3_metadatas = [||];
		as3_classes = Array.of_list (List.rev ctx.classes);
		as3_statics = Array.of_list (List.rev ctx.statics);
		as3_inits = [|init|];
		as3_functions = lookup_array ctx.functions;
		as3_unknown = "";
	} in
	[Swf.TActionScript3 (Some (0,""),a)], ctx.boot

;;
gen_expr_ref := gen_expr
