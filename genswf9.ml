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
open As3
open Type

type ('a,'b) gen_lookup = {
	h : ('a,'b) Hashtbl.t;
	a : 'a DynArray.t;
	c : int -> 'b;
}

type 'a lookup = ('a,'a index) gen_lookup
type 'a lookup_nz = ('a,'a index_nz) gen_lookup

type code_infos = {
	mutable iregs : int;
	mutable imaxregs : int;
	mutable ipos : int;
	mutable istack : int;
	mutable imax : int;
	mutable iscopes : int;
	mutable imaxscopes : int;
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
	mutable locals : (string,int) PMap.t;
	mutable code : as3_opcode DynArray.t;
	mutable infos : code_infos;	
}

let error p = Typer.error "Invalid expression" p
let stack_error p = Typer.error "Stack error" p

let stack_delta = function
	| A3Throw -> -1
	| A3GetSuper _ -> 1
	| A3SetSuper _ -> -1
	| A3RegReset _ -> 0
	| A3Nop -> 0
	| A3Jump _ -> 0
	| A3Switch _ -> -1
	| A3PopScope -> 0
	| A3XmlOp3 -> assert false
	| A3ForIn | A3ForEach -> assert false
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
	| A3Next _ -> 0
	| A3StackCall n -> -(n + 2)
	| A3StackNew n -> -(n + 2)
	| A3SuperCall (_,n) -> -(n + 1)
	| A3Call (_,n) -> -(n + 1)
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
	| A3SetProp _ -> -1
	| A3Reg _ -> 1
	| A3SetReg _ -> -1
	| A3GetScope _ -> 1	
	| A3Get _ -> 0
	| A3Set _ -> -2
	| A3Delete _ -> -1
	| A3GetSlot _ -> 0
	| A3SetSlot _ -> -2
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
		| A3Neg | A3Incr | A3Decr | A3Not | A3BitNot | A3IIncr | A3IDecr -> 0
		| _ -> -1)
	| A3Unk _ -> assert false

let index_int (x : int) : 'a index = Obj.magic (x + 1)
let index_nz_int (x : int) : 'a index_nz = Obj.magic x
let tid (x : 'a index) : int = Obj.magic x

let new_lookup() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_int }
let new_lookup_nz() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_nz_int }

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

let acc_ident ctx i =
	write ctx (A3Reg (PMap.find i ctx.locals))

let type_path ctx ?(getclass=false) (pack,name) =
	let pid = string ctx (String.concat "." pack) in
	let nameid = string ctx name in
	let pid = lookup (A3RPublic (Some pid)) ctx.brights in	
	let tid = lookup (if getclass then A3TClassInterface (Some nameid,lookup [pid] ctx.rights) else A3TMethodVar (nameid,pid)) ctx.types in
	tid

let ident ctx i = type_path ctx ([],i)

let default_infos n =
	{ ipos = 0; istack = 0; imax = 0; iregs = n; imaxregs = n; iscopes = 0; imaxscopes = 0 }

let alloc_reg ctx =
	let r = ctx.infos.iregs + 1 in
	ctx.infos.iregs <- r;
	if ctx.infos.imaxregs < r then ctx.infos.imaxregs <- r;
	r

let open_block ctx =
	let old_stack = ctx.infos.istack in
	let old_regs = ctx.infos.iregs in
	(fun() ->
		if ctx.infos.istack <> old_stack then assert false;
		ctx.infos.iregs <- old_regs
	)

let begin_fun ctx args =
	let mt = {
		mt3_ret = None;
		mt3_args = List.map (fun _ -> None) args;
		mt3_native = false;
		mt3_var_args = false;
		mt3_debug_name = None;
		mt3_dparams = None;
		mt3_pnames = None;
		mt3_unk_flags = (false,false,false,false);
	} in
	let old_locals = ctx.locals in
	let old_code = ctx.code in
	let old_infos = ctx.infos in
	ctx.infos <- default_infos (List.length args);
	ctx.code <- DynArray.create();
	ctx.locals <- List.fold_left (fun acc name -> PMap.add name (alloc_reg ctx) acc) PMap.empty args;	
	(fun () ->
		let f = {
			fun3_id = add mt ctx.mtypes;
			fun3_stack_size = ctx.infos.imax;
			fun3_nregs = ctx.infos.imaxregs + 1;
			fun3_unk3 = 1;
			fun3_max_scope = ctx.infos.imaxscopes + 1;
			fun3_code = DynArray.to_list ctx.code;
			fun3_trys = [||];
			fun3_locals = [||];
		} in
		ignore(add f ctx.functions);
		ctx.locals <- old_locals;
		ctx.code <- old_code;
		ctx.infos <- old_infos;
		f.fun3_id
	)

let gen_constant ctx c =
	match c with
	| TInt i ->
		if Int32.compare i (-128l) > 0 && Int32.compare i 128l < 0 then
			write ctx (A3SmallInt (Int32.to_int i))
		else
			write ctx (A3IntRef (lookup i ctx.ints))
	| TFloat f ->
		let f = float_of_string f in
		write ctx (A3Float (lookup f ctx.floats))
	| TString s ->
		write ctx (A3String (lookup s ctx.strings))
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

let rec gen_expr_content ctx retval e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx c
	| TThrow e ->
		gen_expr ctx true e;
		write ctx A3Throw;
		no_value ctx retval;
	| TField (e,f) ->
		gen_expr ctx true e;
		write ctx (A3Get (ident ctx f))
	| TTypeExpr t ->
		write ctx (A3GetScope (0,true));
		write ctx (A3Get (type_path ctx (t_path t)));
	| TParenthesis e ->
		gen_expr ctx retval e
	| TLocal s ->
		(try 
			acc_ident ctx s
		with
			Not_found -> assert false)
	| TEnumField (e,s) ->
		write ctx (A3GetScope (0,true));
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
			| [] -> if retval then write ctx A3Null
			| [e] -> gen_expr ctx retval e
			| e :: l ->
				gen_expr ctx false e;
				loop l
		in
		let b = open_block ctx in
		loop el;
		b();
	| TVars vl ->
		List.iter (fun (v,_,e) ->
			let r = alloc_reg ctx in
			ctx.locals <- PMap.add v r ctx.locals;
			match e with
			| None -> ()
			| Some e ->
				gen_expr ctx true e;
				write ctx (A3SetReg r)
		) vl
	| TReturn None ->
		write ctx A3RetVoid;
		no_value ctx retval
	| TReturn (Some e) ->
		gen_expr ctx true e;
		write ctx A3Ret;
		no_value ctx retval
	| TArray (e,eindex) ->
		gen_expr ctx true e;
		gen_expr ctx true eindex;
		write ctx (A3Get (lookup (A3TArrayAccess ctx.gpublic) ctx.types));
		ctx.infos.istack <- ctx.infos.istack - 1;

(*
	| TBinop of Ast.binop * texpr * texpr
	| TCall of texpr * texpr list
	| TNew of tclass * t list * texpr list
	| TUnop of Ast.unop * Ast.unop_flag * texpr
	| TFunction of tfunc
	| TFor of string * texpr * texpr
	| TIf of texpr * texpr * texpr option
	| TWhile of texpr * texpr * Ast.while_flag
	| TSwitch of texpr * (texpr * texpr) list * texpr option
	| TMatch of texpr * (tenum * t list) * (string * (string option * t) list option * texpr) list * texpr option
	| TTry of texpr * (string * t * texpr) list
	| TBreak
	| TContinue
*)
	| _ ->
		assert false

and gen_expr ctx retval e =
	let old = ctx.infos.istack in
	gen_expr_content ctx retval e;
	if old <> ctx.infos.istack then begin
		if old + 1 <> ctx.infos.istack then stack_error e.epos;
		if not retval then write ctx A3Pop;
	end else if retval then stack_error e.epos

let generate_construct ctx args =
	let f = begin_fun ctx args in
	write ctx A3NaN;
	write ctx A3Throw;
	write ctx A3This;
	write ctx A3Scope;
	write ctx A3This;
	(try List.iter (acc_ident ctx) args with Not_found -> assert false);
	write ctx (A3SuperConstr (List.length args));
	write ctx A3RetVoid;
	f()

let generate_class_init ctx c slot =
	write ctx (A3GetScope (0,true));
	let path = (match c.cl_super with None -> ([],"Object") | Some (sup,_) -> sup.cl_path) in
	write ctx (A3GetProp (type_path ctx path));
	write ctx A3Scope;
	write ctx (A3GetProp (type_path ~getclass:true ctx path));
	write ctx (A3ClassDef slot);
	write ctx A3PopScope;
	let r = alloc_reg ctx in
	write ctx A3Dup;
	write ctx (A3SetReg r);	
	write ctx (A3Set (type_path ctx c.cl_path));
	let nslot = ref 0 in
	List.iter (fun f ->
		incr nslot;
		match f.cf_expr with
		| Some { eexpr = TFunction _ } | None -> ()
		| Some e ->
			write ctx (A3Reg r);
			gen_expr ctx true e;
			write ctx (A3SetSlot !nslot);
	) c.cl_ordered_statics

let generate_class_static ctx c =
	let f = begin_fun ctx [] in
	write ctx A3RetVoid;
	f()

let generate_class ctx c =
	let name_id = type_path ctx c.cl_path in
	let st_id = generate_class_static ctx c in
	let cid = (match c.cl_constructor with
		| None ->
			let rec loop c =
				match c.cl_super with
				| None ->
					generate_construct ctx []
				| Some (csup,_) ->
					match csup.cl_constructor with
					| None -> loop csup
					| Some co -> generate_construct ctx (match follow co.cf_type with TFun (l,_) -> List.map (fun (name,_,_) -> name) l | _ -> assert false)
			in
			loop c
		| Some f -> assert false
	) in
	let fields = [||] in
	let sc = {
		cl3_name = name_id;
		cl3_super = Some (type_path ctx (match c.cl_super with None -> [],"Object" | Some (c,_) -> c.cl_path));
		cl3_sealed = true;
		cl3_final = false;
		cl3_interface = false;
		cl3_rights = None;
		cl3_implements = [||];
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
				f3_kind = A3FVar { v3_type = None; v3_value = A3VNone; v3_const = false };				
				f3_metas = None;
			}
		) c.cl_ordered_statics)
	} in
	ctx.classes <- sc :: ctx.classes;
	ctx.statics <- st :: ctx.statics;
	()

let generate_type ctx t =
	match t with
	| TClassDecl c -> if not c.cl_extern then generate_class ctx c
	| TTypeDecl _ -> ()
	| TEnumDecl e ->
		match e.e_path with
		| [] , "Void" | [] , "Bool" | [] , "Dynamic" -> ()
		| _ ->
			failwith (Ast.s_type_path e.e_path)

let generate_inits ctx types =
	let f = begin_fun ctx [] in
	write ctx A3This;
	write ctx A3Scope;
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
		| _ -> acc
	) [] types in
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
		infos = default_infos 0;
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
	[Swf.TActionScript3 (None,a)]
