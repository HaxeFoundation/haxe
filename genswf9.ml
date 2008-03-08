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
open As3hl

type read = Read
type write = Unused__ | Write

type tkind =
	| KInt
	| KUInt
	| KFloat
	| KBool
	| KType of hl_name
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
	| VId of hl_name
	| VCast of hl_name * tkind
	| VGlobal of hl_name
	| VArray
	| VScope of hl_slot

type local =
	| LReg of register
	| LScope of hl_slot
	| LGlobal of hl_name

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
	debug : bool;
	mutable last_line : int;
	boot : string;
	(* per-function *)
	mutable locals : (string,local) PMap.t;
	mutable code : hl_opcode DynArray.t;
	mutable infos : code_infos;
	mutable trys : try_infos list;
	mutable breaks : (unit -> unit) list;
	mutable continues : (int -> unit) list;
	mutable in_static : bool;
	mutable curblock : texpr list;
	mutable block_vars : (hl_slot * string * t) list;
	mutable try_scope_reg : register option;
}

let error p = Typer.error "Invalid expression" p
let stack_error p = Typer.error "Stack error" p

let index_int (x : int) : 'a index = Obj.magic (x + 1)
let index_nz_int (x : int) : 'a index_nz = Obj.magic x
let tid (x : 'a index) : int = Obj.magic x

let ethis = mk (TConst TThis) (mk_mono()) null_pos
let dynamic_prop = HMMultiNameLate [HNPublic (Some "")]

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

let write ctx op =
	DynArray.add ctx.code op;
	ctx.infos.ipos <- ctx.infos.ipos + 1;
	let s = ctx.infos.istack + As3hlparse.stack_delta op in
	ctx.infos.istack <- s;
	if s > ctx.infos.imax then ctx.infos.imax <- s;
	match op with
	| HScope ->
		let n = ctx.infos.iscopes + 1 in
		ctx.infos.iscopes <- n;
		if n > ctx.infos.imaxscopes then ctx.infos.imaxscopes <- n
	| HPopScope ->
		ctx.infos.iscopes <- ctx.infos.iscopes - 1
	| _ ->
		()

let jump ctx cond =
	let op = DynArray.length ctx.code in
	let p = ctx.infos.ipos in
	write ctx (HJump (cond,0));
	(fun () ->
		let delta = ctx.infos.ipos - p in
		DynArray.set ctx.code op (HJump (cond,delta))
	)

let jump_back ctx =
	let p = ctx.infos.ipos in
	write ctx HLabel;
	(fun cond ->
		let delta = p - ctx.infos.ipos in
		write ctx (HJump (cond,delta))
	)

let type_path ctx path =
	let pack, name = (match path with
		| [] , "Int" -> [] , "int"
		| [] , "UInt" -> [] , "uint"
		| [] , "Float" -> [] , "Number"
		| [] , "Bool" -> [] , "Boolean"
		| [] , "Enum" -> [] , "Class"
		| ["flash";"xml"], "XML" -> [], "XML"
		| ["flash";"xml"], "XMLList" -> [], "XMLList"
		| ["flash"] , "FlashXml__" -> [] , "Xml"
		| ["flash"] , "Boot" -> [] , ctx.boot
		| _ -> path
	) in
	HMPath (pack,name)

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
	| TInst (c,_) ->
		(match c.cl_kind with
		| KTypeParameter ->
			(match c.cl_implements with
			| [csup,_] -> type_path ctx csup.cl_path
			| _ -> type_path ctx ([],"Object"))
		| KExtension (c,_) ->
			type_path ctx c.cl_path
		| _ ->
			type_path ctx c.cl_path)
	| TFun _ ->
		type_path ctx ([],"Function")
	| TEnum ({ e_path = ([],"Class") as path },_)
	| TEnum ({ e_path = ([],"Bool") as path },_)
	| TType ({ t_path = ([],"UInt") as path },_) ->
		type_path ctx path
	| _ ->
		HMPath ([],"Object")

let type_void ctx t =
	match follow t with
	| TEnum ({ e_path = [],"Void" },_) -> HMPath ([],"void")
	| _ -> type_id ctx t

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
	| TFun _ ->
		KType (HMPath ([],"Function"))
	| TMono _
	| TAnon _
	| TType _
	| TDynamic _ ->
		KDynamic
	| TLazy _ ->
		assert false

let ident i = HMPath ([],i)

let as3 p =
	HMName (p,HNNamespace "http://adobe.com/AS3/2006/builtin")

let property p t =
	match follow t with
	| TInst ({ cl_path = [],"Array" },_) ->
		(match p with
		| "length" -> ident p, Some KInt
		| "copy" | "insert" | "remove" | "iterator" | "toString" -> ident p , None
		| _ -> as3 p, None);
	| TInst ({ cl_path = [],"String" },_) ->
		(match p with
		| "length" | "charCodeAt" (* use haXe version *) -> ident p, None
		| _ -> as3 p, None);
	| _ ->
		ident p, None

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
		| KInt -> HToInt
		| KUInt -> HToUInt
		| KFloat -> HToNumber
		| KBool -> HToBool
		| KType t -> HCast t
		| KDynamic -> HAsAny
	)

let set_reg ctx r =
	if not r.rinit then begin
		r.rinit <- true;
		if ctx.infos.icond then r.rcond <- true;
	end;
	coerce ctx r.rtype;
	write ctx (HSetReg r.rid)

let free_reg ctx r =
	r.rused <- false

let pop ctx n =
	let rec loop n =
		if n > 0 then begin
			write ctx HPop;
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
				let slot , _ , _ = (List.find (fun (_,x,_) -> name = x) ctx.block_vars) in
				slot
			with
				Not_found ->
					let n = List.length ctx.block_vars + 1 in
					ctx.block_vars <- (n,name,t) :: ctx.block_vars;
					n
			) in
			LScope pos
		end else
			let r = alloc_reg ctx (classify ctx t) in
			if ctx.debug then write ctx (HDebugReg (name, r.rid, ctx.last_line));
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
		write ctx (HGetScope 1);
		VScope n
	| LGlobal p ->
		if is_set forset then write ctx (HFindProp p);
		VGlobal p

let rec setvar ctx (acc : write access) retval =
	match acc with
	| VReg r ->
		if retval then write ctx HDup;
		set_reg ctx r;
	| VGlobal g ->
		if retval then write ctx HDup;
		write ctx (HSetProp g);
	| VId _ | VCast _ | VArray | VScope _ when retval ->
		let r = alloc_reg ctx KDynamic in
		write ctx HDup;
		set_reg ctx r;
		setvar ctx acc false;
		write ctx (HReg r.rid);
		free_reg ctx r
	| VId id | VCast (id,_) ->
		write ctx (HInitProp id)
	| VArray ->
		write ctx (HSetProp dynamic_prop);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (HSetSlot n)

let getvar ctx (acc : read access) =
	match acc with
	| VReg r ->
		if not r.rinit then begin
			r.rinit <- true;
			r.rcond <- true;
		end;
		write ctx (HReg r.rid)
	| VId id ->
		write ctx (HGetProp id)
	| VCast (id,t) ->
		write ctx (HGetProp id);
		coerce ctx t
	| VGlobal g ->
		write ctx (HGetLex g);
	| VArray ->
		write ctx (HGetProp dynamic_prop);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (HGetSlot n)

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
		write ctx (HDebugLine line);
		ctx.last_line <- line;
	end

let begin_fun ctx args tret el stat p =
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
	if ctx.debug then begin
		write ctx (HDebugFile p.pfile);
		debug ctx p
	end;
	let rec find_this e =
		match e.eexpr with
		| TFunction _ -> ()
		| TConst TThis | TConst TSuper -> raise Exit
		| _ -> Transform.iter find_this e
	in
	let this_reg = try List.iter find_this el; false with Exit -> true in
	ctx.locals <- PMap.foldi (fun name l acc ->
		match l with
		| LReg _ -> acc
		| LScope _ -> PMap.add name (LGlobal (ident name)) acc
		| LGlobal _ -> PMap.add name l acc
	) ctx.locals PMap.empty;
	List.iter (fun (name,_,t) ->
		define_local ctx name ~init:true t el;
		match gen_local_access ctx name null_pos Write with
		| VReg _ -> ()
		| acc ->
			let r = alloc_reg ctx (classify ctx t) in
			write ctx (HReg r.rid);
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
			| None -> if opt then dparams := Some [HVNone]
			| Some l -> dparams := Some (HVNone :: l)
		) args;
		let code = DynArray.to_list ctx.code in
		let extra = (
			if hasblock then begin
				let scope = (match ctx.try_scope_reg with
					| None -> [HScope]
					| Some r -> [HDup; HSetReg r.rid; HScope]
				) in
				HThis :: HScope :: HNewBlock :: scope
			end else if this_reg then
				[HThis; HScope]
			else
				[]
		) in
		(* add dummy registers initialization *)
		let extra = extra @ List.concat (List.map (fun r ->
			if not r.rcond then
				[]
			else
			let s = [HSetReg r.rid] in
			match r.rtype with
			| KInt -> HSmallInt 0 :: s
			| KUInt -> HSmallInt 0 :: HToUInt :: s
			| KFloat -> HNaN :: s
			| KBool -> HFalse :: s
			| KType t -> HNull :: HAsType t :: s
			| KDynamic -> HNull :: HAsAny :: s
		) (DynArray.to_list ctx.infos.iregs)) in
		let delta = List.length extra in
		let f = {
			hlf_stack_size = (if ctx.infos.imax = 0 && (hasblock || this_reg) then 1 else ctx.infos.imax);
			hlf_nregs = DynArray.length ctx.infos.iregs + 1;
			hlf_init_scope = 1;
			hlf_max_scope = ctx.infos.imaxscopes + 1 + (if hasblock then 2 else if this_reg then 1 else 0);
			hlf_code = Array.of_list (extra @ code);
			hlf_trys = Array.of_list (List.map (fun t ->
				{
					hltc_start = t.tr_pos + delta;
					hltc_end = t.tr_end + delta;
					hltc_handle = t.tr_catch_pos + delta;
					hltc_type = (match follow t.tr_type with
						| TInst (c,_) -> Some (type_path ctx c.cl_path)
						| TEnum (e,_) -> Some (type_path ctx e.e_path)
						| TDynamic _ -> None
						| _ -> assert false);
					hltc_name = None;
				}
			) (List.rev ctx.trys));
			hlf_locals = Array.of_list (List.map (fun (id,name,t) -> ident name, Some (type_id ctx t), id) ctx.block_vars);
		} in
		let mt = {
			hlmt_mark = As3hlparse.alloc_mark();
			hlmt_ret = Some (type_void ctx tret);
			hlmt_args = List.map (fun (_,_,t) -> Some (type_id ctx t)) args;
			hlmt_native = false;
			hlmt_var_args = varargs;
			hlmt_debug_name = None;
			hlmt_dparams = !dparams;
			hlmt_pnames = None;
			hlmt_new_block = hasblock;
			hlmt_unused_flag = false;
			hlmt_arguments_defined = false;
			hlmt_uses_dxns = false;
			hlmt_function = Some f;
		} in
		ctx.locals <- old_locals;
		ctx.code <- old_code;
		ctx.infos <- old_infos;
		ctx.trys <- old_trys;
		ctx.block_vars <- old_bvars;
		ctx.in_static <- old_static;
		ctx.last_line <- last_line;
		ctx.try_scope_reg <- old_treg;
		mt
	)

let empty_method ctx p =
	let f = begin_fun ctx [] t_void [] true p in
	write ctx HRetVoid;
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
			write ctx (HSmallInt (Int32.to_int i))
		else
			write ctx (HIntRef i);
	| TFloat f ->
		let f = float_of_string f in
		write ctx (HFloat f);
	| TString s ->
		write ctx (HString s);
	| TBool b ->
		write ctx (if b then HTrue else HFalse);
	| TNull ->
		write ctx HNull;
		(match classify ctx t with
		| KInt | KBool | KUInt | KFloat ->
			Typer.error ("In Flash9, null can't be used as basic type " ^ s_type (print_context()) t) p
		| x -> coerce ctx x)
	| TThis ->
		write ctx HThis
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
		let id, k = property f e1.etype in
		(match e1.eexpr with
		| TConst TThis when not ctx.in_static -> write ctx (HFindPropStrict id)
		| _ -> gen_expr ctx true e1);
		(match k with
		| Some t -> VCast (id,t)
		| None ->
		match follow e1.etype with
		| TInst _ | TEnum _ -> VId id
		| TAnon a when (match !(a.a_status) with Statics _ | EnumStatics _ -> true | _ -> false) -> VId id
		| _ -> VCast (id,classify ctx e.etype))
	| TArray ({ eexpr = TLocal "__global__" },{ eexpr = TConst (TString s) }) ->
		let path = (match List.rev (ExtString.String.nsplit s ".") with [] -> assert false | x :: l -> List.rev l, x) in
		let id = type_path ctx path in
		if is_set forset then write ctx HGetGlobalScope;
		VGlobal id
	| TArray (e,eindex) ->
		gen_expr ctx true e;
		gen_expr ctx true eindex;
		VArray
	| TTypeExpr t ->
		let id = type_path ctx (t_path t) in
		if is_set forset then write ctx HGetGlobalScope;
		VGlobal id
	| _ ->
		error e.epos

let rec gen_expr_content ctx retval e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx c e.etype e.epos
	| TThrow e ->
		gen_expr ctx true e;
		write ctx HThrow;
		no_value ctx retval;
	| TParenthesis e ->
		gen_expr ctx retval e
	| TEnumField (e,s) ->
		let id = type_path ctx e.e_path in
		write ctx (HGetLex id);
		write ctx (HGetProp (ident s));
	| TObjectDecl fl ->
		List.iter (fun (name,e) ->
			write ctx (HString name);
			gen_expr ctx true e
		) fl;
		write ctx (HObject (List.length fl))
	| TArrayDecl el ->
		List.iter (gen_expr ctx true) el;
		write ctx (HArray (List.length el))
	| TBlock el ->
		let rec loop = function
			| [] ->
				if retval then write ctx HNull
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
		write ctx HRetVoid;
		no_value ctx retval
	| TReturn (Some e) ->
		gen_expr ctx true e;
		write ctx HRet;
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
	| TNew ({ cl_path = [],"Array" },_,[]) ->
		(* it seems that [] is 4 time faster than new Array() *)
		write ctx (HArray 0)
	| TNew (c,_,pl) ->
		let id = type_path ctx c.cl_path in
		write ctx (HFindPropStrict id);
		List.iter (gen_expr ctx true) pl;
		write ctx (HConstructProperty (id,List.length pl))
	| TFunction f ->
		write ctx (HFunction (generate_function ctx f true))
	| TIf (e0,e1,e2) ->
		let j = jump_expr ctx e0 false in
		let branch = begin_branch ctx in
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
		let continue_pos = ctx.infos.ipos in
		let loop = jump_back ctx in
		let jend = jump_expr ctx econd false in
		jstart();
		gen_expr ctx false e;
		loop J3Always;
		jend();
		branch();
		end_loop continue_pos;
		if retval then write ctx HNull
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
				write ctx HThis;
				write ctx HScope;
				write ctx (HReg (match ctx.try_scope_reg with None -> assert false | Some r -> r.rid));
				write ctx HScope;
				define_local ctx ename t [e];
				let r = (try match PMap.find ename ctx.locals with LReg r -> Some (alloc_reg ctx r.rtype) | _ -> None with Not_found -> assert false) in
				(match r with None -> () | Some r -> set_reg ctx r);
				let acc = gen_local_access ctx ename e.epos Write in
				(match r with None -> () | Some r -> write ctx (HReg r.rid));
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
		let continue_pos = ctx.infos.ipos in
		let start = jump_back ctx in
		write ctx (HReg r.rid);
		write ctx (HCallProperty (ident "hasNext",0));
		let jend = jump ctx J3False in
		let acc = gen_local_access ctx v e.epos Write in
		write ctx (HReg r.rid);
		write ctx (HCallProperty (ident "next",0));
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
		let p = ctx.infos.ipos in
		write ctx (HJump (J3Always,0));
		ctx.continues <- (fun target -> DynArray.set ctx.code op (HJump (J3Always,target - p))) :: ctx.continues;
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
					write ctx (HReg r.rid);
					gen_expr ctx true v;
					prev := jump ctx J3Neq;
				| v :: l ->
					write ctx (HReg r.rid);
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
				write ctx HNull;
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
		let rparams = alloc_reg ctx (KType (type_path ctx ([],"Array"))) in
		let has_params = List.exists (fun (_,p,_) -> p <> None) cases in
		gen_expr ctx true e0;
		if has_params then begin
			write ctx HDup;
			write ctx (HGetProp (ident "params"));
			set_reg ctx rparams;
		end;
		write ctx (HGetProp (ident "index"));
		write ctx HToInt;
		let branch = begin_branch ctx in
		let switch_index = DynArray.length ctx.code in
		let switch_pos = ctx.infos.ipos in
		write ctx (HSwitch (0,[]));
		(match def with
		| None ->
			if retval then begin
				write ctx HNull;
				coerce ctx t;
			end;
		| Some e ->
			gen_expr ctx retval e;
			if retval && classify ctx e.etype <> t then coerce ctx t;
		);
		let constructs = ref [] in
		let max = ref 0 in
		let jends = List.map (fun (cl,params,e) ->
			let j = jump ctx J3Always in
			List.iter (fun tag ->
				if tag > !max then max := tag;
				constructs := (tag,ctx.infos.ipos) :: !constructs;
			) cl;
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
						write ctx (HReg rparams.rid);
						write ctx (HSmallInt !p);
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
			j
		) cases in
		let cases = Array.create (!max + 1) 1 in
		List.iter (fun (tag,pos) -> Array.set cases tag (pos - switch_pos)) !constructs;
		List.iter (fun j -> j()) jends;
		DynArray.set ctx.code switch_index (HSwitch (1,Array.to_list cases));
		branch();
		free_reg ctx rparams

and gen_call ctx retval e el =
	match e.eexpr , el with
	| TLocal "__is__" , [e;t] ->
		gen_expr ctx true e;
		gen_expr ctx true t;
		write ctx (HOp A3OIs)
	| TLocal "__as__" , [e;t] ->
		gen_expr ctx true e;
		gen_expr ctx true t;
		write ctx (HOp A3OAs)
	| TLocal "__int__", [e] ->
		gen_expr ctx true e;
		write ctx HToInt
	| TLocal "__float__", [e] ->
		gen_expr ctx true e;
		write ctx HToNumber
	| TLocal "__hkeys__" , [e2]
	| TLocal "__keys__" , [e2] ->
		let racc = alloc_reg ctx (KType (type_path ctx ([],"Array"))) in
		let rcounter = alloc_reg ctx KInt in
		let rtmp = alloc_reg ctx KDynamic in
		write ctx (HSmallInt 0);
		set_reg ctx rcounter;
		write ctx (HArray 0);
		set_reg ctx racc;
		gen_expr ctx true e2;
		set_reg ctx rtmp;
		let start = jump ctx J3Always in
		let loop = jump_back ctx in
		write ctx (HReg racc.rid);
		write ctx (HReg rtmp.rid);
		write ctx (HReg rcounter.rid);
		write ctx HForIn;
		if e.eexpr = TLocal "__hkeys__" then begin
			write ctx (HSmallInt 1);
			write ctx (HCallProperty (as3 "substr",1));
		end;
		write ctx (HCallPropVoid (as3 "push",1));
		start();
		write ctx (HNext (rtmp.rid,rcounter.rid));
		loop J3True;
		write ctx (HReg racc.rid);
		free_reg ctx rtmp;
		free_reg ctx rcounter;
		free_reg ctx racc;
	| TLocal "__new__" , e :: el ->
		gen_expr ctx true e;
		List.iter (gen_expr ctx true) el;
		write ctx (HConstruct (List.length el))
	| TLocal "__delete__" , [o;f] ->
		gen_expr ctx true o;
		gen_expr ctx true f;
		write ctx (HDeleteProp dynamic_prop);
	| TLocal "__unprotect__" , [e] ->
		gen_expr ctx true e
	| TLocal "__typeof__", [e] ->
		gen_expr ctx true e;
		write ctx HTypeof
	| TLocal "__in__", [e; f] ->
		gen_expr ctx true e;
		gen_expr ctx true f;
		write ctx (HOp A3OIn)
	| TArray ({ eexpr = TLocal "__global__" },{ eexpr = TConst (TString s) }), _ ->
		(match gen_access ctx e Read with
		| VGlobal id ->
			write ctx (HFindPropStrict id);
			List.iter (gen_expr ctx true) el;
			write ctx (HCallProperty (id,List.length el));
		| _ -> assert false)
	| TConst TSuper , _ ->
		write ctx HThis;
		List.iter (gen_expr ctx true) el;
		write ctx (HConstructSuper (List.length el));
	| TField ({ eexpr = TConst TSuper },f) , _ ->
		let id = ident f in
		write ctx (HFindPropStrict id);
		List.iter (gen_expr ctx true) el;
		write ctx (HCallSuper (id,List.length el));
	| TField ({ eexpr = TConst TThis },f) , _ when not ctx.in_static ->
		let id = ident f in
		write ctx (HFindPropStrict id);
		List.iter (gen_expr ctx true) el;
		write ctx (if retval then HCallProperty (id,List.length el) else HCallPropVoid (id,List.length el));
	| TField (e1,f) , _ ->
		gen_expr ctx true e1;
		List.iter (gen_expr ctx true) el;
		let id , _ = property f e1.etype in
		if not retval then
			write ctx (HCallPropVoid (id,List.length el))
		else
			let coerce() =
				match follow e.etype with
				| TFun (_,r) -> coerce ctx (classify ctx r)
				| _ -> ()
			in
			write ctx (HCallProperty (id,List.length el));
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
		write ctx (HGetLex id);
		List.iter (gen_expr ctx true) el;
		write ctx (HCallProperty (ident f,List.length el));
	| _ ->
		gen_expr ctx true e;
		write ctx HGetGlobalScope;
		List.iter (gen_expr ctx true) el;
		write ctx (HCallStack (List.length el))

and gen_unop ctx retval op flag e =
	let k = classify ctx e.etype in
	match op with
	| Not ->
		gen_expr ctx true e;
		write ctx (HOp A3ONot);
	| Neg ->
		gen_expr ctx true e;
		write ctx (HOp (if k = KInt then A3OINeg else A3ONeg));
	| NegBits ->
		gen_expr ctx true e;
		write ctx (HOp A3OBitNot);
	| Increment
	| Decrement ->
		let incr = (op = Increment) in
		let acc = gen_access ctx e Write in (* for set *)
		match acc with
		| VReg r when r.rtype = KInt ->
			if not r.rinit then r.rcond <- true;
			if retval && flag = Postfix then getvar ctx (gen_access ctx e Read);
			write ctx (if incr then HIncrIReg r.rid else HDecrIReg r.rid);
			if retval && flag = Prefix then getvar ctx (gen_access ctx e Read);
		| _ ->
		getvar ctx (gen_access ctx e Read);
		match flag with
		| Postfix when retval ->
			let r = alloc_reg ctx k in
			write ctx HDup;
			set_reg ctx r;
			write ctx (HOp (if incr then A3OIncr else A3ODecr));
			setvar ctx acc false;
			write ctx (HReg r.rid);
			free_reg ctx r
		| Postfix | Prefix ->
			write ctx (HOp (if incr then A3OIncr else A3ODecr));
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
				write ctx (HOp iop)
			else begin
				write ctx (HOp o);
				if o = A3OAdd then coerce ctx (classify ctx t);
			end;
		| _ ->
			write ctx (HOp o)
	in
	match op with
	| OpAssign ->
		let acc = gen_access ctx e1 Write in
		gen_expr ctx true e2;
		setvar ctx acc retval
	| OpBoolAnd ->
		write ctx HFalse;
		let j = jump_expr ctx e1 false in
		write ctx HPop;
		gen_expr ctx true e2;
		coerce ctx KBool;
		j();
	| OpBoolOr ->
		write ctx HTrue;
		let j = jump_expr ctx e1 true in
		write ctx HPop;
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
		write ctx (HOp A3ONot)
	| OpPhysNotEq ->
		gen_op A3OPhysEq;
		write ctx (HOp A3ONot)
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
		gen_op A3OMod;
		if	classify ctx e1.etype = KInt && classify ctx e2.etype = KInt then coerce ctx (classify ctx t);
	| OpInterval ->
		assert false

and gen_expr ctx retval e =
	let old = ctx.infos.istack in
	if ctx.debug then debug ctx e.epos;
	gen_expr_content ctx retval e;
	if old <> ctx.infos.istack then begin
		if old + 1 <> ctx.infos.istack then stack_error e.epos;
		if not retval then write ctx HPop;
	end else if retval then stack_error e.epos

and generate_function ctx fdata stat =
	let f = begin_fun ctx fdata.tf_args fdata.tf_type [fdata.tf_expr] stat fdata.tf_expr.epos in
	gen_expr ctx false fdata.tf_expr;
	(match follow fdata.tf_type with
	| TEnum ({ e_path = [],"Void" },[]) -> write ctx HRetVoid
	| _ ->
		(* check that we have a return that can be accepted by Flash9 VM *)
		let rec loop e =
			match e.eexpr with
			| TBlock [] -> false
			| TBlock l -> loop (List.hd (List.rev l))
			| TReturn None -> true
			| TReturn (Some e) ->
				let rec inner_loop e =
					match e.eexpr with
					| TSwitch _ | TMatch _ | TFor _ | TWhile _ | TTry _ -> false
					| TIf _ -> loop e
					| TParenthesis e -> inner_loop e
					| _ -> true
				in
				inner_loop e
			| TIf (_,e1,Some e2) -> loop e1 && loop e2
			| TSwitch (_,_,Some e) -> loop e
			| TParenthesis e -> loop e
			| _ -> false
		in
		if not (loop fdata.tf_expr) then write ctx HRetVoid;
	);
	f()

and jump_expr ctx e jif =
	match e.eexpr with
	| TParenthesis e -> jump_expr ctx e jif
	| TBinop (op,e1,e2) ->
		let j t f =
			gen_expr ctx true e1;
			gen_expr ctx true e2;
			jump ctx (if jif then t else f)
		in
		(match op with
		| OpEq -> j J3Eq J3Neq
		| OpNotEq -> j J3Neq J3Eq
		| OpPhysEq -> j J3PhysEq J3PhysNeq
		| OpPhysNotEq -> j J3PhysNeq J3PhysEq
		| OpGt -> j J3Gt J3NotGt
		| OpGte -> j J3Gte J3NotGte
		| OpLt -> j J3Lt J3NotLt
		| OpLte -> j J3Lte J3NotLte
		| _ ->
			gen_expr ctx true e;
			jump ctx (if jif then J3True else J3False))
	| _ ->
		gen_expr ctx true e;
		jump ctx (if jif then J3True else J3False)

let generate_method ctx fdata stat =
	generate_function ctx { fdata with tf_expr = Transform.block_vars fdata.tf_expr } stat

let generate_construct ctx fdata c =
	(* make all args optional to allow no-param constructor *)
	let f = begin_fun ctx (List.map (fun (a,o,t) -> a,true,t) fdata.tf_args) fdata.tf_type [ethis;fdata.tf_expr] false fdata.tf_expr.epos in
	(* if skip_constructor, then returns immediatly *)
	if c.cl_kind <> KGenericInstance then begin
		let id = ident "skip_constructor" in
		getvar ctx (VGlobal (type_path ctx ([],ctx.boot)));
		getvar ctx (VId id);
		let j = jump ctx J3False in
		write ctx HRetVoid;
		j();
	end;
	(* --- *)
	PMap.iter (fun _ f ->
		match f.cf_expr with
		| Some { eexpr = TFunction fdata } when f.cf_set = NormalAccess ->
			let id = ident f.cf_name in
			write ctx (HFindProp id);
			write ctx (HFunction (generate_method ctx fdata false));
			write ctx (HInitProp id);
		| _ -> ()
	) c.cl_fields;
	gen_expr ctx false (Transform.block_vars fdata.tf_expr);
	write ctx HRetVoid;
	f() , List.length fdata.tf_args

let generate_class_init ctx c hc =
	write ctx HGetGlobalScope;
	if c.cl_interface then
		write ctx HNull
	else begin
		let path = (match c.cl_super with None -> ([],"Object") | Some (sup,_) -> sup.cl_path) in
		write ctx (HGetLex (type_path ctx path));
		write ctx HScope;
		write ctx (HGetLex (type_path ctx path));
	end;
	write ctx (HClassDef hc);
	List.iter (fun f ->
		match f.cf_expr with
		| Some { eexpr = TFunction fdata } when f.cf_set = NormalAccess ->
			write ctx HDup;
			write ctx (HFunction (generate_method ctx fdata true));
			write ctx (HInitProp (ident f.cf_name));
		| _ -> ()
	) c.cl_ordered_statics;
	if not c.cl_interface then write ctx HPopScope;
	write ctx (HInitProp (type_path ctx c.cl_path))

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
				write ctx HGetGlobalScope;
				write ctx (HGetProp (type_path ctx c.cl_path));
				write ctx (HSetReg r.rid); (* needed for setslot *)
				first := false;
			end;
			write ctx (HReg r.rid);
			gen_expr ctx true (Transform.block_vars e);
			write ctx (HSetSlot !nslot);
		| _ ->
			incr nslot
	) c.cl_ordered_statics;
	free_reg ctx r

let generate_enum_init ctx e hc =
	let path = ([],"Object") in
	let name_id = type_path ctx e.e_path in
	write ctx HGetGlobalScope;
	write ctx (HGetLex (type_path ctx path));
	write ctx HScope;
	write ctx (HGetLex (type_path ctx path));
	write ctx (HClassDef hc);
	write ctx HPopScope;
	let r = alloc_reg ctx KDynamic in
	write ctx HDup;
	write ctx (HSetReg r.rid); (* needed for setslot *)
	write ctx (HInitProp name_id);
	let nslot = ref 0 in
	PMap.iter (fun _ f ->
		incr nslot;
		match f.ef_type with
		| TFun _ -> ()
		| _ ->
			write ctx (HReg r.rid);
			write ctx (HFindPropStrict name_id);
			write ctx (HString f.ef_name);
			write ctx (HInt f.ef_index);
			write ctx HNull;
			write ctx (HConstructProperty (name_id,3));
			write ctx (HSetSlot !nslot);
	) e.e_constrs;
	write ctx (HReg r.rid);
	List.iter (fun n -> write ctx (HString n)) e.e_names;
	write ctx (HArray (List.length e.e_names));
	write ctx (HSetSlot (!nslot + 1));
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
			Some (HFVar {
				hlv_type = Some (type_path ctx ([],"Function"));
				hlv_value = HVNone;
				hlv_const = false;
			})
		else
			Some (HFMethod {
				hlm_type = generate_method ctx fdata stat;
				hlm_final = stat;
				hlm_override = not stat && loop c;
				hlm_kind = MK3Normal;
			})
	| _ when c.cl_interface && not stat ->
		None
	| _ when f.cf_get = ResolveAccess ->
		None
	| _ ->
		Some (HFVar {
			hlv_type = Some (type_id ctx f.cf_type);
			hlv_value = HVNone;
			hlv_const = false;
		})

let generate_class ctx c =
	let name = type_path ctx c.cl_path in
	let cid , cnargs = (match c.cl_constructor with
		| None ->
			if c.cl_interface then
				{ (empty_method ctx null_pos) with hlmt_function = None }, 0
			else
				generate_construct ctx {
					tf_args = [];
					tf_type = t_void;
					tf_expr = {
						eexpr = TBlock [];
						etype = t_void;
						epos = null_pos;
					}
				} c
		| Some f ->
			match f.cf_expr with
			| Some { eexpr = TFunction fdata } -> generate_construct ctx fdata c
			| _ -> assert false
	) in
	let fields = Array.of_list (PMap.fold (fun f acc ->
		match generate_field_kind ctx f c false with
		| None -> acc
		| Some k ->
			{
				hlf_name = ident f.cf_name;
				hlf_slot = 0;
				hlf_kind = k;
				hlf_metas = None;
			} :: acc
	) c.cl_fields []) in
	let st_field_count = ref 0 in
	let st_meth_count = ref 0 in
	{
		hlc_name = name;
		hlc_super = (if c.cl_interface then None else Some (type_path ctx (match c.cl_super with None -> [],"Object" | Some (c,_) -> c.cl_path)));
		hlc_sealed = c.cl_path <> (["flash"],"Boot") && c.cl_dynamic = None;
		hlc_final = false;
		hlc_interface = c.cl_interface;
		hlc_namespace = None;
		hlc_implements = Array.of_list (List.map (fun (c,_) ->
			if not c.cl_interface then Typer.error "Can't implement class in Flash9" c.cl_pos;
			type_path ctx c.cl_path
		) c.cl_implements);
		hlc_construct = cid;
		hlc_fields = fields;
		hlc_static_construct = empty_method ctx c.cl_pos;
		hlc_static_fields = Array.of_list (List.map (fun f ->
			let k = (match generate_field_kind ctx f c true with None -> assert false | Some k -> k) in
			let count = (match k with HFMethod _ -> st_meth_count | HFVar _ -> st_field_count | _ -> assert false) in
			incr count;
			{
				hlf_name = ident f.cf_name;
				hlf_slot = !count;
				hlf_kind = k;
				hlf_metas = None;
			}
		) c.cl_ordered_statics);
	}

let generate_enum ctx e =
	let name_id = type_path ctx e.e_path in
	let f = begin_fun ctx [("tag",false,t_string);("index",false,t_int);("params",false,mk_mono())] t_void [ethis] false e.e_pos in
	let tag_id = ident "tag" in
	let index_id = ident "index" in
	let params_id = ident "params" in
	write ctx (HFindProp tag_id);
	write ctx (HReg 1);
	write ctx (HInitProp tag_id);
	write ctx (HFindProp index_id);
	write ctx (HReg 2);
	write ctx (HInitProp index_id);
	write ctx (HFindProp params_id);
	write ctx (HReg 3);
	write ctx (HInitProp params_id);
	write ctx HRetVoid;
	let construct = f() in
	let f = begin_fun ctx [] t_string [] true e.e_pos in
	write ctx (HGetLex (type_path ctx ([],ctx.boot)));
	write ctx HThis;
	write ctx (HCallProperty (ident "enum_to_string",1));
	write ctx HRet;
	let tostring = f() in
	let st_count = ref 0 in
	let constrs = PMap.fold (fun f acc ->
		incr st_count;
		{
			hlf_name = ident f.ef_name;
			hlf_slot = !st_count;
			hlf_kind = (match f.ef_type with
				| TFun (args,_) ->
					let fdata = begin_fun ctx args (TEnum (e,[])) [] true f.ef_pos in
					write ctx (HFindPropStrict name_id);
					write ctx (HString f.ef_name);
					write ctx (HInt f.ef_index);
					let n = ref 0 in
					List.iter (fun _ -> incr n; write ctx (HReg !n)) args;
					write ctx (HArray (!n));
					write ctx (HConstructProperty (name_id,3));
					write ctx HRet;
					let fid = fdata() in
					HFMethod {
						hlm_type = fid;
						hlm_final = true;
						hlm_override = false;
						hlm_kind = MK3Normal;
					}
				| _ ->
					HFVar { hlv_type = (Some name_id); hlv_value = HVNone; hlv_const = false; }
			);
			hlf_metas = None;
		} :: acc
	) e.e_constrs [] in
	{
		hlc_name = name_id;
		hlc_super = Some (type_path ctx ([],"Object"));
		hlc_sealed = true;
		hlc_final = true;
		hlc_interface = false;
		hlc_namespace = None;
		hlc_implements = [||];
		hlc_construct = construct;
		hlc_fields = [|
			{ hlf_name = tag_id; hlf_slot = 0; hlf_kind = HFVar { hlv_type = None; hlv_value = HVNone; hlv_const = false; }; hlf_metas = None };
			{ hlf_name = index_id; hlf_slot = 0; hlf_kind = HFVar { hlv_type = None; hlv_value = HVNone; hlv_const = false; }; hlf_metas = None };
			{ hlf_name = params_id; hlf_slot = 0; hlf_kind = HFVar { hlv_type = None; hlv_value = HVNone; hlv_const = false; }; hlf_metas = None };
			{ hlf_name = ident "__enum__"; hlf_slot = 0; hlf_kind = HFVar { hlv_type = None; hlv_value = HVBool true; hlv_const = true }; hlf_metas = None };
			{
				hlf_name = ident "toString";
				hlf_slot = 0;
				hlf_kind = HFMethod {
					hlm_type = tostring;
					hlm_final = true;
					hlm_override = false;
					hlm_kind = MK3Normal;
				};
				hlf_metas = None;
			};
		|];
		hlc_static_construct = empty_method ctx e.e_pos;
		hlc_static_fields = Array.of_list ({
			hlf_name = ident "__isenum";
			hlf_slot = !st_count + 2;
			hlf_kind = HFVar { hlv_type = None; hlv_value = HVBool true; hlv_const = true; };
			hlf_metas = None;
		} :: {
			hlf_name = ident "__constructs__";
			hlf_slot = !st_count + 1;
			hlf_kind = HFVar { hlv_type = None; hlv_value = HVNone; hlv_const = false; };
			hlf_metas = None;
		} :: constrs);
	}

let generate_type ctx t =
	match t with
	| TClassDecl c ->
		if c.cl_extern then
			None
		else
			Some (generate_class ctx c)
	| TEnumDecl e ->
		if e.e_extern then
			None
		else
			Some (generate_enum ctx e)
	| TTypeDecl _ ->
		None

let generate_resources ctx hres =
	write ctx HGetGlobalScope;
	write ctx (HGetProp (type_path ctx ([],ctx.boot)));
	let id = type_path ctx (["flash";"utils"],"Dictionary") in
	write ctx (HFindPropStrict id);
	write ctx (HConstructProperty (id,0));
	let r = alloc_reg ctx (KType id) in
	set_reg ctx r;
	Hashtbl.iter (fun name data ->
		write ctx (HReg r.rid);
		write ctx (HString name);
		write ctx (HString data);
		setvar ctx VArray false;
	) hres;
	write ctx (HReg r.rid);
	write ctx (HInitProp (ident "__res"))

let generate_inits ctx types hres =
	let f = begin_fun ctx [] t_void [ethis] false null_pos in
	let slot = ref 0 in
	let classes = List.fold_left (fun acc (t,hc) ->
		match hc with
		| None -> acc
		| Some hc ->
			match t with
			| TClassDecl c ->
				incr slot;
				generate_class_init ctx c hc;
				{
					hlf_name = type_path ctx c.cl_path;
					hlf_slot = !slot;
					hlf_kind = HFClass hc;
					hlf_metas = None;
				} :: acc
			| TEnumDecl e ->
				incr slot;
				generate_enum_init ctx e hc;
				{
					hlf_name = type_path ctx e.e_path;
					hlf_slot = !slot;
					hlf_kind = HFClass hc;
					hlf_metas = None;
				} :: acc
			| _ ->
				acc
	) [] types in

	(* define flash.Boot.init method *)
	write ctx HGetGlobalScope;
	write ctx (HGetProp (type_path ctx ([],ctx.boot)));
	let finit = begin_fun ctx [] t_void [] true null_pos in
	List.iter (fun (t,_) ->
		match t with
		| TClassDecl c ->
			(match c.cl_init with
			| None -> ()
			| Some e -> gen_expr ctx false (Transform.block_vars e));
		| _ -> ()
	) types;
	List.iter (fun (t,_) ->
		match t with
		| TClassDecl c -> generate_class_statics ctx c
		| _ -> ()
	) types;
	write ctx HRetVoid;
	write ctx (HFunction (finit()));
	write ctx (HInitProp (ident "init"));

	(* generate resources *)
	generate_resources ctx hres;

	write ctx HRetVoid;
	{
		hls_method = f();
		hls_fields = Array.of_list (List.rev classes);
	}

let generate types hres =
	let ctx = {
		boot = "Boot_" ^ Printf.sprintf "%X" (Random.int 0xFFFFFF);
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
	let classes = List.map (fun t -> (t,generate_type ctx t)) types in
	let init = generate_inits ctx classes hres in
	[init], ctx.boot, (fun () -> empty_method ctx null_pos)

;;
Random.self_init();
gen_expr_ref := gen_expr
