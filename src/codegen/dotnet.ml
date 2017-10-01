(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*)
open ExtString
open Common
open Globals
open Ast
open IlData
open IlMeta

(* see http://msdn.microsoft.com/en-us/library/2sk3x8a7(v=vs.71).aspx *)
let cs_binops =
	[Ast.OpAdd, "op_Addition";
	Ast.OpSub, "op_Subtraction";
	Ast.OpMult, "op_Multiply";
	Ast.OpDiv, "op_Division";
	Ast.OpMod, "op_Modulus";
	Ast.OpXor, "op_ExclusiveOr";
	Ast.OpOr, "op_BitwiseOr";
	Ast.OpAnd, "op_BitwiseAnd";
	Ast.OpBoolAnd, "op_LogicalAnd";
	Ast.OpBoolOr, "op_LogicalOr";
	Ast.OpAssign, "op_Assign";
	Ast.OpShl, "op_LeftShift";
	Ast.OpShr, "op_RightShift";
	Ast.OpShr, "op_SignedRightShift";
	Ast.OpUShr, "op_UnsignedRightShift";
	Ast.OpEq, "op_Equality";
	Ast.OpGt, "op_GreaterThan";
	Ast.OpLt, "op_LessThan";
	Ast.OpNotEq, "op_Inequality";
	Ast.OpGte, "op_GreaterThanOrEqual";
	Ast.OpLte, "op_LessThanOrEqual";
	Ast.OpAssignOp Ast.OpMult, "op_MultiplicationAssignment";
	Ast.OpAssignOp Ast.OpSub, "op_SubtractionAssignment";
	Ast.OpAssignOp Ast.OpXor, "op_ExclusiveOrAssignment";
	Ast.OpAssignOp Ast.OpShl, "op_LeftShiftAssignment";
	Ast.OpAssignOp Ast.OpMod, "op_ModulusAssignment";
	Ast.OpAssignOp Ast.OpAdd, "op_AdditionAssignment";
	Ast.OpAssignOp Ast.OpAnd, "op_BitwiseAndAssignment";
	Ast.OpAssignOp Ast.OpOr, "op_BitwiseOrAssignment";
	(* op_Comma *)
	Ast.OpAssignOp Ast.OpDiv, "op_DivisionAssignment";]

let cs_unops =
	[Ast.Decrement, "op_Decrement";
	Ast.Increment, "op_Increment";
	Ast.Neg, "op_UnaryNegation";
	Ast.Not, "op_LogicalNot";
	Ast.NegBits, "op_OnesComplement"]

let netname_to_hx name =
	let len = String.length name in
	let chr = String.get name 0 in
	String.make 1 (Char.uppercase chr) ^ (String.sub name 1 (len-1))

(* -net-lib implementation *)

type net_lib_ctx = {
	nstd : bool;
	ncom : Common.context;
	nil : IlData.ilctx;
}

let is_haxe_keyword = function
	| "cast" | "extern" | "function" | "in" | "typedef" | "using" | "var" | "untyped" | "inline" -> true
	| _ -> false

let hxpath_to_net ctx path =
	try
		Hashtbl.find ctx.ncom.net_path_map path
	with
	 | Not_found ->
			[],[],"Not_found"

let add_cs = function
	| "haxe" :: ns -> "haxe" :: ns
	| "std" :: ns -> "std" :: ns
	| "cs" :: ns -> "cs" :: ns
	| "system" :: ns -> "cs" :: "system" :: ns
	| ns -> ns

let escape_chars =
	String.replace_chars (fun chr ->
		if (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') || (chr >= '0' && chr <= '9') || chr = '_' then
			Char.escaped chr
		else
			"_x" ^ (string_of_int (Char.code chr)) ^ "_")

let netcl_to_hx cl =
	let cl = if String.length cl > 0 && String.get cl 0 >= 'a' && String.get cl 0 <= 'z' then
			Char.escaped (Char.uppercase (String.get cl 0)) ^ (String.sub cl 1 (String.length cl - 1))
		else
			cl
	in
	try
		let cl, nargs = String.split cl "`" in
		(escape_chars cl) ^ "_" ^ nargs
	with | Invalid_string ->
		escape_chars cl

let netpath_to_hx std = function
	| [],[], cl -> [], netcl_to_hx cl
	| ns,[], cl ->
		let ns = (List.map (fun s -> String.lowercase (escape_chars s)) ns) in
		add_cs ns, netcl_to_hx cl
	| ns,(nhd :: ntl as nested), cl ->
		let nested = List.map (netcl_to_hx) nested in
		let ns = (List.map (fun s -> String.lowercase (escape_chars s)) ns) @ [nhd] in
		add_cs ns, String.concat "_" nested ^ "_" ^ netcl_to_hx cl

let lookup_ilclass std com ilpath =
	let path = netpath_to_hx std ilpath in
	List.fold_right (fun (_,_,_,get_raw_class) acc ->
		match acc with
		| None -> get_raw_class path
		| Some p -> acc
	) com.net_libs None

let discard_nested = function
	| (ns,_),cl -> (ns,[]),cl

let mk_type_path ctx path params =
	let pack, sub, name = match path with
		| ns,[], cl ->
			ns, None, netcl_to_hx cl
		| ns, (nhd :: ntl as nested), cl ->
			let nhd = netcl_to_hx nhd in
			let nested = List.map (netcl_to_hx) nested in
			ns, Some (String.concat "_" nested ^ "_" ^ netcl_to_hx cl), nhd
	in
	CTPath {
		tpackage = fst (netpath_to_hx ctx.nstd (pack,[],""));
		Ast.tname = name;
		tparams = params;
		tsub = sub;
	}

let raw_type_path ctx path params =
	{
		tpackage = fst path;
		Ast.tname = snd path;
		tparams = params;
		tsub = None;
	}

let rec convert_signature ctx p = function
	| LVoid ->
		mk_type_path ctx ([],[],"Void") []
	| LBool ->
		mk_type_path ctx ([],[],"Bool") []
	| LChar ->
		mk_type_path ctx (["cs";"types"],[],"Char16") []
	| LInt8 ->
		mk_type_path ctx (["cs";"types"],[],"Int8") []
	| LUInt8 ->
		mk_type_path ctx (["cs";"types"],[],"UInt8") []
	| LInt16 ->
		mk_type_path ctx (["cs";"types"],[],"Int16") []
	| LUInt16 ->
		mk_type_path ctx (["cs";"types"],[],"UInt16") []
	| LInt32 ->
		mk_type_path ctx ([],[],"Int") []
	| LUInt32 ->
		mk_type_path ctx ([],[],"UInt") []
	| LInt64 ->
		mk_type_path ctx (["haxe"],[],"Int64") []
	| LUInt64 ->
		mk_type_path ctx (["cs";"types"],[],"UInt64") []
	| LFloat32 ->
		mk_type_path ctx ([],[],"Single") []
	| LFloat64 ->
		mk_type_path ctx ([],[],"Float") []
	| LString ->
		mk_type_path ctx (["std"],[],"String") []
	| LObject ->
		mk_type_path ctx ([],[],"Dynamic") []
	| LPointer s | LManagedPointer s ->
		mk_type_path ctx (["cs"],[],"Pointer") [ TPType (convert_signature ctx p s,null_pos) ]
	| LTypedReference ->
		mk_type_path ctx (["cs";"system"],[],"TypedReference") []
	| LIntPtr ->
		mk_type_path ctx (["cs";"system"],[],"IntPtr") []
	| LUIntPtr ->
		mk_type_path ctx (["cs";"system"],[],"UIntPtr") []
	| LValueType (s,args) | LClass (s,args) ->
		mk_type_path ctx s (List.map (fun s -> TPType (convert_signature ctx p s,null_pos)) args)
	| LTypeParam i ->
		mk_type_path ctx ([],[],"T" ^ string_of_int i) []
	| LMethodTypeParam i ->
		mk_type_path ctx ([],[],"M" ^ string_of_int i) []
	| LVector s ->
		mk_type_path ctx (["cs"],[],"NativeArray") [TPType (convert_signature ctx p s,null_pos)]
	(* | LArray of ilsig_norm * (int option * int option) array *)
	| LMethod (_,ret,args) ->
		CTFunction (List.map (fun v -> convert_signature ctx p v,null_pos) args, (convert_signature ctx p ret,null_pos))
	| _ -> mk_type_path ctx ([],[], "Dynamic") []

let ilpath_s = function
	| ns,[], name -> s_type_path (ns,name)
	| [],nested,name -> String.concat "." nested ^ "." ^ name
	| ns, nested, name -> String.concat "." ns ^ "." ^ String.concat "." nested ^ "." ^ name

let get_cls = function
	| _,_,c -> c

(* TODO: When possible on Haxe, use this to detect flag enums, and make an abstract with @:op() *)
(* that behaves like an enum, and with an enum as its underlying type *)
let enum_is_flag ilcls =
	let check_flag name ns = name = "FlagsAttribute" && ns = ["System"] in
	List.exists (fun a ->
		match a.ca_type with
			| TypeRef r ->
				check_flag r.tr_name r.tr_namespace
			| TypeDef d ->
				check_flag d.td_name d.td_namespace
			| Method m ->
				(match m.m_declaring with
					| Some d ->
						check_flag d.td_name d.td_namespace
					| _ -> false)
			| MemberRef r ->
				(match r.memr_class with
					| TypeRef r ->
						check_flag r.tr_name r.tr_namespace
					| TypeDef d ->
						check_flag d.td_name d.td_namespace
					| _ -> false)
			| _ ->
				false
	) ilcls.cattrs

let convert_ilenum ctx p ?(is_flag=false) ilcls =
	let meta = ref [
		Meta.Native, [EConst (String (ilpath_s ilcls.cpath,Double) ), p], p;
		Meta.CsNative, [], p;
	] in

	let data = ref [] in
	List.iter (fun f -> match f.fname with
		| "value__" -> ()
		| _ when not (List.mem CStatic f.fflags.ff_contract) -> ()
		| _ ->
			let meta, const = match f.fconstant with
				| Some IChar i
				| Some IByte i
				| Some IShort i ->
					[Meta.CsNative, [EConst (Int (string_of_int i) ), p], p ], Int64.of_int i
				| Some IInt i ->
					[Meta.CsNative, [EConst (Int (Int32.to_string i) ), p], p ], Int64.of_int32 i
				| Some IFloat32 f | Some IFloat64 f ->
					[], Int64.of_float f
				| Some IInt64 i ->
					[], i
				| _ ->
					[], Int64.zero
			in
			data := ( { ec_name = f.fname,null_pos; ec_doc = None; ec_meta = meta; ec_args = []; ec_pos = p; ec_params = []; ec_type = None; }, const) :: !data;
	) ilcls.cfields;
	let data = List.stable_sort (fun (_,i1) (_,i2) -> Int64.compare i1 i2) (List.rev !data) in

	let _, c = netpath_to_hx ctx.nstd ilcls.cpath in
	let name = netname_to_hx c in
	EEnum {
		d_name = (if is_flag then name ^ "_FlagsEnum" else name),null_pos;
		d_doc = None;
		d_params = []; (* enums never have type parameters *)
		d_meta = !meta;
		d_flags = [EExtern];
		d_data = List.map fst data;
	}

let rec has_unmanaged = function
	| LPointer _ -> true
	| LManagedPointer s -> has_unmanaged s
	| LValueType (p,pl) -> List.exists (has_unmanaged) pl
	| LClass (p,pl) -> List.exists (has_unmanaged) pl
	| LVector s -> has_unmanaged s
	| LArray (s,a) -> has_unmanaged s
	| LMethod (c,r,args) -> has_unmanaged r || List.exists (has_unmanaged) args
	| _ -> false

let convert_ilfield ctx p field =
	if not (Common.defined ctx.ncom Define.Unsafe) && has_unmanaged field.fsig.snorm then raise Exit;
	let p = { p with pfile =	p.pfile ^" (" ^field.fname ^")" } in
	let cff_doc = None in
	let cff_pos = p in
	let cff_meta = ref [] in
	let cff_name = match field.fname with
		| name when String.length name > 5 ->
				(match String.sub name 0 5 with
				| "__hx_" -> raise Exit
				| _ -> name)
		| name -> name
	in
	let cff_access = match field.fflags.ff_access with
		| FAFamily | FAFamOrAssem -> APrivate
		| FAPublic -> APublic
		| _ -> raise Exit (* private instances aren't useful on externs *)
	in
	let readonly, acc = List.fold_left (fun (readonly,acc) -> function
		| CStatic -> readonly, AStatic :: acc
		| CInitOnly | CLiteral -> true, acc
		| _ -> readonly,acc
	) (false,[cff_access]) field.fflags.ff_contract in
	if PMap.mem "net_loader_debug" ctx.ncom.defines.Define.values then
		Printf.printf "\t%sfield %s : %s\n" (if List.mem AStatic acc then "static " else "") cff_name (IlMetaDebug.ilsig_s field.fsig.ssig);
	let kind = match readonly with
		| true ->
			cff_meta := (Meta.ReadOnly, [], cff_pos) :: !cff_meta;
			FProp (("default",null_pos), ("never",null_pos), Some (convert_signature ctx p field.fsig.snorm,null_pos), None)
		| false ->
			FVar (Some (convert_signature ctx p field.fsig.snorm,null_pos), None)
	in
	let cff_name, cff_meta =
		if String.get cff_name 0 = '%' then
			let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
			"_" ^ name,
			(Meta.Native, [EConst (String (name,Double) ), cff_pos], cff_pos) :: !cff_meta
		else
			cff_name, !cff_meta
	in
	{
		cff_name = cff_name,null_pos;
		cff_doc = cff_doc;
		cff_pos = cff_pos;
		cff_meta = cff_meta;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_ilevent ctx p ev =
	let p = { p with pfile =	p.pfile ^" (" ^ev.ename ^")" } in
	let name = ev.ename in
	let kind = FVar (Some (convert_signature ctx p ev.esig.snorm,null_pos), None) in
	let meta = [Meta.Event, [], p; Meta.Keep,[],p; Meta.SkipReflection,[],p] in
	let acc = [APrivate] in
	let add_m acc m = match m with
		| None -> acc
		| Some (name,flags) ->
			if List.mem (CMStatic) flags.mf_contract then
				AStatic :: acc
			else
				acc
	in
	if PMap.mem "net_loader_debug" ctx.ncom.defines.Define.values then
		Printf.printf "\tevent %s : %s\n" name (IlMetaDebug.ilsig_s ev.esig.ssig);
	let acc = add_m acc ev.eadd in
	let acc = add_m acc ev.eremove in
	let acc = add_m acc ev.eraise in
	{
		cff_name = name,null_pos;
		cff_doc = None;
		cff_pos = p;
		cff_meta = meta;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_ilmethod ctx p m is_explicit_impl =
	if not (Common.defined ctx.ncom Define.Unsafe) && has_unmanaged m.msig.snorm then raise Exit;
	let force_check = Common.defined ctx.ncom Define.ForceLibCheck in
	let p = { p with pfile =	p.pfile ^" (" ^m.mname ^")" } in
	let cff_doc = None in
	let cff_pos = p in
	let cff_name = match m.mname with
		| ".ctor" -> "new"
		| ".cctor"-> raise Exit (* __init__ field *)
		| "Equals" | "GetHashCode" -> raise Exit
		| name when String.length name > 5 ->
				(match String.sub name 0 5 with
				| "__hx_" -> raise Exit
				| _ -> name)
		| name -> name
	in
	let meta = [Meta.Overload, [], p] in
	let acc, meta = match m.mflags.mf_access with
		| FAFamily | FAFamOrAssem ->
			APrivate, ((Meta.Protected, [], p) :: meta)
		(* | FAPrivate -> APrivate *)
		| FAPublic when List.mem SGetter m.msemantics || List.mem SSetter m.msemantics ->
			APrivate, meta
		| FAPublic -> APublic, meta
		| _ ->
			if PMap.mem "net_loader_debug" ctx.ncom.defines.Define.values then
				Printf.printf "\tmethod %s (skipped) : %s\n" cff_name (IlMetaDebug.ilsig_s m.msig.ssig);
			raise Exit
	in
	let is_static = ref false in
	let acc, is_final = List.fold_left (fun (acc,is_final) -> function
		| CMStatic when cff_name <> "new" -> is_static := true; AStatic :: acc, is_final
		| CMVirtual when is_final = None -> acc, Some false
		| CMFinal -> acc, Some true
		| _ -> acc, is_final
	) ([acc],None) m.mflags.mf_contract in
	if PMap.mem "net_loader_debug" ctx.ncom.defines.Define.values then
		Printf.printf "\t%smethod %s : %s\n" (if !is_static then "static " else "") cff_name (IlMetaDebug.ilsig_s m.msig.ssig);

	let meta = match is_final with
		| None | Some true when not force_check ->
			(Meta.Final,[],p) :: meta
		| _ ->
			meta
	in
	let meta = if is_explicit_impl then
			(Meta.NoCompletion,[],p) :: (Meta.SkipReflection,[],p) :: meta
		else
			meta
	in
	(* let meta = if List.mem OSynchronized m.mflags.mf_interop then *)
	(*	(Meta.Synchronized,[],p) :: meta *)
	(* else *)
	(*	meta *)
	(* in *)

	let rec change_sig = function
		| LManagedPointer s -> LManagedPointer (change_sig s)
		| LPointer s -> LPointer (change_sig s)
		| LValueType (p,pl) -> LValueType(p, List.map change_sig pl)
		| LClass (p,pl) -> LClass(p, List.map change_sig pl)
		| LTypeParam i -> LObject
		| LVector s -> LVector (change_sig s)
		| LArray (s,a) -> LArray (change_sig s, a)
		| LMethod (c,r,args) -> LMethod (c, change_sig r, List.map change_sig args)
		| p -> p
	in
	let change_sig = if !is_static then change_sig else (fun s -> s) in

	let ret =
		if String.length cff_name > 4 && String.sub cff_name 0 4 = "set_" then
			match m.mret.snorm, m.margs with
			| LVoid, [_,_,s] ->
				s.snorm
			| _ -> m.mret.snorm
		else
			m.mret.snorm
	in

	let kind =
		let args = List.map (fun (name,flag,s) ->
			let t = match s.snorm with
				| LManagedPointer s ->
					let is_out = List.mem POut flag.pf_io && not (List.mem PIn flag.pf_io) in
					let name = if is_out then "Out" else "Ref" in
					mk_type_path ctx (["cs"],[],name) [ TPType (convert_signature ctx p s,null_pos) ]
				| _ ->
					convert_signature ctx p (change_sig s.snorm)
			in
			(name,null_pos),false,[],Some (t,null_pos),None) m.margs
		in
		let ret = convert_signature ctx p (change_sig ret) in
		let types = List.map (fun t ->
			{
				tp_name = "M" ^ string_of_int t.tnumber,null_pos;
				tp_params = [];
				tp_constraints = [];
				tp_meta = [];
			}
		) m.mtypes in
		FFun {
			f_params = types;
			f_args = args;
			f_type = Some (ret,null_pos);
			f_expr = None;
		}
	in
	let cff_name, cff_meta =
		if String.get cff_name 0 = '%' then
			let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
			"_" ^ name,
			(Meta.Native, [EConst (String (name,Double) ), cff_pos], cff_pos) :: meta
		else
			cff_name, meta
	in
	let acc = match m.moverride with
		| None -> acc
		| _ when cff_name = "new" -> acc
		| Some (path,s) -> match lookup_ilclass ctx.nstd ctx.ncom path with
			| Some ilcls when not (List.mem SInterface ilcls.cflags.tdf_semantics) ->
				AOverride :: acc
			| None when ctx.ncom.verbose ->
				prerr_endline ("(net-lib) A referenced assembly for path " ^ ilpath_s path ^ " was not found");
				acc
			| _ -> acc
	in
	{
		cff_name = cff_name,null_pos;
		cff_doc = cff_doc;
		cff_pos = cff_pos;
		cff_meta = cff_meta;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_ilprop ctx p prop is_explicit_impl =
	if not (Common.defined ctx.ncom Define.Unsafe) && has_unmanaged prop.psig.snorm then raise Exit;
	let p = { p with pfile =	p.pfile ^" (" ^prop.pname ^")" } in
	let pmflags = match prop.pget, prop.pset with
		| Some(_,fl1), _ -> Some fl1
		| _, Some(_,fl2) -> Some fl2
		| _ -> None
	in
	let cff_access = match pmflags with
		| Some { mf_access = FAFamily | FAFamOrAssem } -> APrivate
		| Some { mf_access = FAPublic } -> APublic
		| _ -> raise Exit (* non-public / protected fields don't interest us *)
	in
	let access acc = acc.mf_access in
	let cff_access = match pmflags with
		| Some m when List.mem CMStatic m.mf_contract ->
			[AStatic;cff_access]
		| _ -> [cff_access]
	in
	let get = match prop.pget with
		| None -> "never"
		| Some(s,_) when String.length s <= 4 || String.sub s 0 4 <> "get_" ->
			raise Exit (* special (?) getter; not used *)
		| Some(_,m) when access m <> FAPublic -> (match access m with
			| FAFamily
			| FAFamOrAssem -> "null"
			| _ -> "never")
		| Some _ -> "get"
	in
	let set = match prop.pset with
		| None -> "never"
		| Some(s,_) when String.length s <= 4 || String.sub s 0 4 <> "set_" ->
			raise Exit (* special (?) getter; not used *)
		| Some(_,m) when access m <> FAPublic -> (match access m with
			| FAFamily
			| FAFamOrAssem -> "never"
			| _ -> "never");
		| Some _ -> "set"
	in
	if PMap.mem "net_loader_debug" ctx.ncom.defines.Define.values then
		Printf.printf "\tproperty %s (%s,%s) : %s\n" prop.pname get set (IlMetaDebug.ilsig_s prop.psig.ssig);
	let ilsig = match prop.psig.snorm with
		| LMethod (_,ret,[]) -> ret
		| s -> raise Exit
	in

	let meta = if is_explicit_impl then
			[ Meta.NoCompletion,[],p; Meta.SkipReflection,[],p ]
		else
			[]
	in

	let kind =
		FProp ((get,null_pos), (set,null_pos), Some(convert_signature ctx p ilsig,null_pos), None)
	in
	{
		cff_name = prop.pname,null_pos;
		cff_doc = None;
		cff_pos = p;
		cff_meta = meta;
		cff_access = cff_access;
		cff_kind = kind;
	}

let get_type_path ctx ct = match ct with | CTPath p -> p | _ -> assert false

let is_explicit ctx ilcls i =
	let s = match i with
		| LClass(path,_) | LValueType(path,_) -> ilpath_s path
		| _ -> assert false
	in
	let len = String.length s in
	List.exists (fun m ->
		String.length m.mname > len && String.sub m.mname 0 len = s
	) ilcls.cmethods

let mke e p = (e,p)

let mk_special_call name p args =
	mke (ECast( mke (EUntyped( mke (ECall( mke (EConst(Ident name)) p, args )) p )) p , None)) p

let mk_this_call name p args =
	mke (ECall( mke (EField(mke (EConst(Ident "this")) p ,name)) p, args )) p

let mk_metas metas p =
	List.map (fun m -> m,[],p) metas

let mk_abstract_fun name p kind metas acc =
	let metas = mk_metas metas p in
	{
		cff_name = name,null_pos;
		cff_doc = None;
		cff_pos = p;
		cff_meta = metas;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_fun_arg ctx p = function
	| LManagedPointer s ->
		mk_type_path ctx (["cs"],[],"Ref") [ TPType (convert_signature ctx p s,null_pos) ],p
	| s ->
		convert_signature ctx p s,p

let convert_fun ctx p ret args =
	let args = List.map (convert_fun_arg ctx p) args in
	CTFunction(args, (convert_signature ctx p ret,null_pos))

let get_clsname ctx cpath =
	match netpath_to_hx ctx.nstd cpath with
		| (_,n) -> n

let convert_delegate ctx p ilcls =
	let p = { p with pfile =	p.pfile ^" (abstract delegate)" } in
	(* will have the following methods: *)
	(* - new (haxeType:Func) *)
	(* - FromHaxeFunction(haxeType) *)
	(* - Invoke() *)
	(* - AsDelegate():Super *)
	(* - @:op(A+B) Add(d:absType) *)
	(* - @:op(A-B) Remove(d:absType) *)
	let abs_type = mk_type_path ctx (ilcls.cpath) (List.map (fun t -> TPType (mk_type_path ctx ([],[],"T" ^ string_of_int t.tnumber) [],null_pos)) ilcls.ctypes) in
	let invoke = List.find (fun m -> m.mname = "Invoke") ilcls.cmethods in
	let ret = invoke.mret.snorm in
	let args = List.map (fun (_,_,s) -> s.snorm) invoke.margs in
	let haxe_type = convert_fun ctx p ret args in
	let types = List.map (fun t ->
		{
			tp_name = ("T" ^ string_of_int t.tnumber),null_pos;
			tp_params = [];
			tp_constraints = [];
			tp_meta = [];
		}
	) ilcls.ctypes in
	let mk_op_fn op name p =
		let fn_name = List.assoc op cs_binops in
		let clsname = match ilcls.cpath with
			| (ns,inner,n) -> get_clsname ctx (ns,inner,"Delegate_"^n)
		in
		let expr = (ECall( (EField( (EConst(Ident (clsname)),p), fn_name ),p), [(EConst(Ident"arg1"),p);(EConst(Ident"arg2"),p)]),p) in
		FFun {
			f_params = types;
			f_args = [("arg1",null_pos),false,[],Some (abs_type,null_pos),None;("arg2",null_pos),false,[],Some (abs_type,null_pos),None];
			f_type = Some (abs_type,null_pos);
			f_expr = Some ( (EReturn (Some expr), p) );
		}
	in
	let mk_op op name =
		let p = { p with pfile = p.pfile ^" (op " ^ name ^ ")" } in
		{
			cff_name = name,null_pos;
			cff_doc = None;
			cff_pos = p;
			cff_meta = [ Meta.Extern,[],p ; Meta.Op, [ (EBinop(op, (EConst(Ident"A"),p), (EConst(Ident"B"),p)),p) ], p ];
			cff_access = [APublic;AInline;AStatic];
			cff_kind = mk_op_fn op name p;
		}
	in
	let params = (List.map (fun s ->
		TPType (mk_type_path ctx ([],[],fst s.tp_name) [],null_pos)
	) types) in
	let underlying_type = match ilcls.cpath with
		| ns,inner,name ->
			mk_type_path ctx (ns,inner,"Delegate_" ^ name) params
	in

	let fn_new = FFun {
		f_params = [];
		f_args = [("hxfunc",null_pos),false,[],Some (haxe_type,null_pos),None];
		f_type = None;
		f_expr = Some ( EBinop(Ast.OpAssign, (EConst(Ident "this"),p), (mk_special_call "__delegate__" p [EConst(Ident "hxfunc"),p]) ), p );
	} in
	let fn_from_hx = FFun {
		f_params = types;
		f_args = [("hxfunc",null_pos),false,[],Some (haxe_type,null_pos),None];
		f_type = Some( mk_type_path ctx ilcls.cpath params,null_pos );
		f_expr = Some( EReturn( Some (mk_special_call "__delegate__" p [EConst(Ident "hxfunc"),p] )), p);
	} in
	let fn_asdel = FFun {
		f_params = [];
		f_args = [];
		f_type = None;
		f_expr = Some(
			EReturn( Some ( EConst(Ident "this"), p ) ), p
		);
	} in
	let fn_new = mk_abstract_fun "new" p fn_new [Meta.Extern] [APublic;AInline] in
	let fn_from_hx = mk_abstract_fun "FromHaxeFunction" p fn_from_hx [Meta.Extern;Meta.From] [APublic;AInline;AStatic] in
	let fn_asdel = mk_abstract_fun "AsDelegate" p fn_asdel [Meta.Extern] [APublic;AInline] in
	let _, c = netpath_to_hx ctx.nstd ilcls.cpath in
	EAbstract {
		d_name = netname_to_hx c,null_pos;
		d_doc = None;
		d_params = types;
		d_meta = mk_metas [Meta.Delegate; Meta.Forward] p;
		d_flags = [AIsType (underlying_type,null_pos)];
		d_data = [fn_new;fn_from_hx;fn_asdel;mk_op Ast.OpAdd "Add";mk_op Ast.OpSub "Remove"];
	}

let convert_ilclass ctx p ?(delegate=false) ilcls = match ilcls.csuper with
	| Some { snorm = LClass ((["System"],[],"Enum"),[]) } ->
		convert_ilenum ctx p ilcls
	| _ ->
		let flags = ref [HExtern] in
		(* todo: instead of CsNative, use more specific definitions *)
		if PMap.mem "net_loader_debug" ctx.ncom.defines.Define.values then begin
			let sup = match ilcls.csuper with | None -> [] | Some c -> [IlMetaDebug.ilsig_s c.ssig] in
			let sup = sup @ List.map (fun i -> IlMetaDebug.ilsig_s i.ssig) ilcls.cimplements in
			print_endline ("converting " ^ ilpath_s ilcls.cpath ^ " : " ^ (String.concat ", " sup))
		end;
		let meta = ref [Meta.CsNative, [], p; Meta.Native, [EConst (String (ilpath_s ilcls.cpath,Double) ), p], p] in
		let force_check = Common.defined ctx.ncom Define.ForceLibCheck in
		if not force_check then
			meta := (Meta.LibType,[],p) :: !meta;

		let is_interface = ref false in
		List.iter (fun f -> match f with
			| SSealed -> meta := (Meta.Final, [], p) :: !meta
			| SInterface ->
				is_interface := true;
				flags := HInterface :: !flags
			| SAbstract -> meta := (Meta.Abstract, [], p) :: !meta
			| _ -> ()
		) ilcls.cflags.tdf_semantics;

		(* (match ilcls.cflags.tdf_vis with *)
		(*	| VPublic | VNestedFamOrAssem | VNestedFamily -> () *)
		(*	| _ -> raise Exit); *)
		(match ilcls.csuper with
			| Some { snorm = LClass ( (["System"],[],"Object"), [] ) } -> ()
			| Some ({ snorm = LClass ( (["System"],[],"ValueType"), [] ) } as s) ->
				flags := HExtends (get_type_path ctx (convert_signature ctx p s.snorm),null_pos) :: !flags;
				meta := (Meta.Struct,[],p) :: !meta
			| Some { snorm = LClass ( (["haxe";"lang"],[],"HxObject"), [] ) } ->
				meta := (Meta.HxGen,[],p) :: !meta
			| Some s ->
				flags := HExtends (get_type_path ctx (convert_signature ctx p s.snorm),null_pos) :: !flags
			| _ -> ());

			let has_explicit_ifaces = ref false in
			List.iter (fun i ->
				match i.snorm with
				| LClass ( (["haxe";"lang"],[], "IHxObject"), _ ) ->
					meta := (Meta.HxGen,[],p) :: !meta
				(* | i when is_explicit ctx ilcls i -> () *)
				| i ->
					if is_explicit ctx ilcls i then has_explicit_ifaces := true;
					flags := if !is_interface then
						HExtends (get_type_path ctx (convert_signature ctx p i),null_pos) :: !flags
					else
						HImplements (get_type_path ctx (convert_signature ctx p i),null_pos) :: !flags
			) ilcls.cimplements;
			(* this is needed because of explicit interfaces. see http://msdn.microsoft.com/en-us/library/aa288461(v=vs.71).aspx *)
			(* explicit interfaces can't be mapped into Haxe in any way - since their fields can't be accessed directly, but they still implement that interface *)
			if !has_explicit_ifaces && force_check then (* do not check on this specific case *)
				meta := (Meta.LibType,[],p) :: !meta;

			(* ArrayAccess *)
			ignore (List.exists (function
			| { psig = { snorm = LMethod(_,ret,[v]) } } ->
				flags := if !is_interface then
					(HExtends( raw_type_path ctx ([],"ArrayAccess") [ TPType (convert_signature ctx p ret,null_pos) ],null_pos) :: !flags)
				else
					(HImplements( raw_type_path ctx ([],"ArrayAccess") [ TPType (convert_signature ctx p ret,null_pos) ],null_pos) :: !flags);
				true
			| _ -> false) ilcls.cprops);

			let fields = ref [] in
			let run_fields fn f =
				List.iter (fun f ->
					try
						fields := fn f :: !fields
					with
						| Exit -> ()
				) f
			in
			let meths = if !is_interface then
					List.filter (fun m -> m.moverride = None) ilcls.cmethods
				else
					ilcls.cmethods
			in
			run_fields (fun m ->
				convert_ilmethod ctx p m (List.exists (fun m2 -> m != m2 && String.get m2.mname 0 <> '.' && String.ends_with m2.mname ("." ^ m.mname)) meths)
			) meths;
			run_fields (convert_ilfield ctx p) ilcls.cfields;
			run_fields (fun prop ->
				convert_ilprop ctx p prop (List.exists (fun p2 -> prop != p2 && String.get p2.pname 0 <> '.' && String.ends_with p2.pname ("." ^ prop.pname)) ilcls.cprops)
			) ilcls.cprops;
			run_fields (convert_ilevent ctx p) ilcls.cevents;

			let params = List.map (fun p ->
				{
					tp_name = "T" ^ string_of_int p.tnumber,null_pos;
					tp_params = [];
					tp_constraints = [];
					tp_meta = [];
				}) ilcls.ctypes
			in

			if delegate then begin
				(* add op_Addition and op_Subtraction *)
				let path = ilcls.cpath in
				let thist = mk_type_path ctx path (List.map (fun t -> TPType (mk_type_path ctx ([],[],"T" ^ string_of_int t.tnumber) [],null_pos)) ilcls.ctypes) in
				let op name =
					{
						cff_name = name,null_pos;
						cff_doc = None;
						cff_pos = p;
						cff_meta = [];
						cff_access = [APublic;AStatic];
						cff_kind = FFun {
							f_params = params;
							f_args = [("arg1",null_pos),false,[],Some (thist,null_pos),None;("arg2",null_pos),false,[],Some (thist,null_pos),None];
							f_type = Some (thist,null_pos);
							f_expr = None;
						};
					}
				in
				fields := op "op_Addition" :: op "op_Subtraction" :: !fields;
			end;
			let path = match ilcls.cpath with
				| ns,inner,name when delegate ->
					ns,inner,"Delegate_"^name
				| _ -> ilcls.cpath
			in
			let _, c = netpath_to_hx ctx.nstd path in
			EClass {
				d_name = netname_to_hx c,null_pos;
				d_doc = None;
				d_params = params;
				d_meta = !meta;
				d_flags = !flags;
				d_data = !fields;
			}

type il_any_field =
	| IlField of ilfield
	| IlMethod of ilmethod
	| IlProp of ilprop

let get_fname = function
	| IlField f -> f.fname
	| IlMethod m -> m.mname
	| IlProp p -> p.pname

let is_static = function
	| IlField f ->
		List.mem CStatic f.fflags.ff_contract
	| IlMethod m ->
		List.mem CMStatic m.mflags.mf_contract
	| IlProp p ->
		List.exists (function
		 | None -> false
		 | Some (_,m) -> List.mem CMStatic m.mf_contract
		) [p.pget;p.pset]
	(* | _ -> false *)

let change_name name = function
	| IlField f -> IlField { f with fname = name }
	| IlMethod m -> IlMethod { m with mname = name }
	| IlProp p -> IlProp { p with pname = name }

let compatible_methods m1 m2 = match m1,m2 with
	| IlMethod { msig = { snorm = LMethod(_,ret1,args1) } }, IlMethod { msig = { snorm = LMethod(_,ret2,args2) } } ->
		ret1 = ret2 && args1 = args2
	| _ -> false

let ilcls_from_ilsig ctx ilsig =
	let path, params = match ilsig with
		| LClass(path, params) | LValueType(path, params) ->
			path, params
		| LObject ->
			(["System"],[],"Object"),[]
		| _ -> raise Not_found (* all other types won't appear as superclass *)
	in
	match lookup_ilclass ctx.nstd ctx.ncom path with
	| None -> raise Not_found
	| Some c ->
		c, params

let rec ilapply_params params = function
	| LManagedPointer s -> LManagedPointer (ilapply_params params s)
	| LPointer s -> LPointer (ilapply_params params s)
	| LValueType (p,pl) -> LValueType(p, List.map (ilapply_params params) pl)
	| LClass (p,pl) -> LClass(p, List.map (ilapply_params params) pl)
	| LTypeParam i ->
		List.nth params i (* TODO: maybe i - 1? *)
	| LVector s -> LVector (ilapply_params params s)
	| LArray (s,a) -> LArray (ilapply_params params s, a)
	| LMethod (c,r,args) -> LMethod (c, ilapply_params params r, List.map (ilapply_params params) args)
	| p -> p

let ilcls_with_params ctx cls params =
	match cls.ctypes with
	| [] -> cls
	| _ ->
		{ cls with
			cfields = List.map (fun f -> { f with fsig = { f.fsig with snorm = ilapply_params params f.fsig.snorm } }) cls.cfields;
			cmethods = List.map (fun m -> { m with
				msig = { m.msig with snorm = ilapply_params params m.msig.snorm };
				margs = List.map (fun (n,f,s) -> (n,f,{ s with snorm = ilapply_params params s.snorm })) m.margs;
				mret = { m.mret with snorm = ilapply_params params m.mret.snorm };
			}) cls.cmethods;
			cprops = List.map (fun p -> { p with psig = { p.psig with snorm = ilapply_params params p.psig.snorm } }) cls.cprops;
			csuper = Option.map (fun s -> { s with snorm = ilapply_params params s.snorm } ) cls.csuper;
			cimplements = List.map (fun s -> { s with snorm = ilapply_params params s.snorm } ) cls.cimplements;
		}

let rec compatible_params t1 t2 = match t1,t2 with
	| LManagedPointer(s1), LManagedPointer(s2) -> compatible_params s1 s2
	| LManagedPointer(s1), s2 | s1, LManagedPointer(s2) ->
		compatible_params s1 s2
	| _ -> t1 = t2

let compatible_methods m1 m2 = match m1, m2 with
	| LMethod(_,r1,a1), LMethod(_,r2,a2) -> (try
		List.for_all2 (fun a1 a2 -> compatible_params a1 a2) a1 a2
	with | Invalid_argument _ ->
		false)
	| _ -> false

let compatible_field f1 f2 = match f1, f2 with
	| IlMethod { msig = { snorm = LMethod(_,_,a1) } },
		IlMethod { msig = { snorm = LMethod(_,_,a2) } } ->
			a1 = a2
	| IlProp p1, IlProp p2 ->
			(* p1.psig.snorm = p2.psig.snorm *)
			true
	| IlField f1, IlField f2 ->
			(* f1.fsig.snorm = f2.fsig.snorm *)
			true
	| _ -> false

let get_all_fields cls =
	let all_fields = List.map (fun f -> IlField f, cls.cpath, f.fname, List.mem CStatic f.fflags.ff_contract) cls.cfields in
	let all_fields = all_fields @ List.map (fun m -> IlMethod m, cls.cpath, m.mname, List.mem CMStatic m.mflags.mf_contract) cls.cmethods in
	let all_fields = all_fields @ List.map (fun p -> IlProp p, cls.cpath, p.pname, is_static (IlProp p)) cls.cprops in
	all_fields

let normalize_ilcls ctx cls =
	let force_check = Common.defined ctx.ncom Define.ForceLibCheck in
	(* first filter out overloaded fields of same signature *)
	let rec loop acc = function
		| [] -> acc
		| m :: cmeths ->
			let static = List.mem CMStatic m.mflags.mf_contract in
			if List.exists (fun m2 -> m.mname = m2.mname && List.mem CMStatic m2.mflags.mf_contract = static && compatible_methods m.msig.snorm m2.msig.snorm) cmeths then
				loop acc cmeths
			else
				loop (m :: acc) cmeths
	in
	let meths = loop [] cls.cmethods in
	(* fix overrides *)
	(* get only the methods that aren't declared as override, but may be *)
	let meths = List.map (fun v -> ref v) meths in
	let no_overrides = List.filter (fun m ->
		let m = !m in
		not (List.mem CMStatic m.mflags.mf_contract)
	) meths in
	let no_overrides = ref no_overrides in

	let all_fields = ref [] in
	let all_events_name = Hashtbl.create 0 in
	(* avoid naming collision between events and functions *)
	let add_cls_events_collision cls =
		List.iter (fun m -> if not (List.mem CMStatic m.mflags.mf_contract) then Hashtbl.replace all_events_name m.mname true) cls.cmethods;
		List.iter (fun p -> if not (is_static (IlProp p)) then Hashtbl.replace all_events_name p.pname true) cls.cprops;
	in

	let rec loop cls = try
		match cls.csuper with
		| Some { snorm = LClass((["System"],[],"Object"),_) }
		| Some { snorm = LObject } | None -> ()
		| Some s ->
			let cls, params = ilcls_from_ilsig ctx s.snorm in
			let cls = ilcls_with_params ctx cls params in
			if force_check then no_overrides := List.filter (fun v ->
				let m = !v in
				let is_override_here = List.exists (fun m2 ->
					m2.mname = m.mname && not (List.mem CMStatic m2.mflags.mf_contract) && compatible_methods m.msig.snorm m2.msig.snorm
				) cls.cmethods in
				if is_override_here then v := { m with moverride = Some(cls.cpath, m.mname) };
				not is_override_here
			) !no_overrides;
			all_fields := get_all_fields cls @ !all_fields;

			add_cls_events_collision cls;
			List.iter (fun ev -> Hashtbl.replace all_events_name ev.ename true) cls.cevents;

			loop cls
		with | Not_found -> ()
	in
	loop cls;

	add_cls_events_collision cls;
	if force_check then List.iter (fun v -> v := { !v with moverride = None }) !no_overrides;
	let added = ref [] in

	let current_all = ref (get_all_fields cls @ !all_fields) in
	(* look for interfaces and add missing implementations (some methods' implementation is optional) *)
	let rec loop_interface cls iface = try
		match iface.snorm with
		| LClass((["System"],[],"Object"),_) | LObject -> ()
		| LClass(path,_) when path = cls.cpath -> ()
		| s ->
			let cif, params = ilcls_from_ilsig ctx s in
			let cif = ilcls_with_params ctx cif params in
			List.iter (function
				| (f,_,name,false) as ff ->
					(* look for compatible fields *)
					if not (List.exists (function
						| (f2,_,name2,false) when (name = name2 || String.ends_with name2 ("." ^ name)) -> (* consider explicit implementations as implementations *)
							compatible_field f f2
						| _ -> false
					) !current_all) then begin
						current_all := ff :: !current_all;
						added := ff :: !added
					end else
						(* ensure it's public *)
						List.iter (fun mref -> match !mref with
							| m when m.mname = name && compatible_field f (IlMethod m) ->
								mref := { m with mflags = { m.mflags with mf_access = FAPublic } }
							| _ -> ()
						) meths
				| _ -> ()
			) (get_all_fields cif);
			List.iter (loop_interface cif) cif.cimplements
		with | Not_found -> ()
	in
	List.iter (loop_interface cls) cls.cimplements;
	let added = List.map (function
		| (IlMethod m,a,name,b) when m.mflags.mf_access <> FAPublic ->
			(IlMethod { m with mflags = { m.mflags with mf_access = FAPublic } },a,name,b)
		| (IlField f,a,name,b) when f.fflags.ff_access <> FAPublic ->
			(IlField { f with fflags = { f.fflags with ff_access = FAPublic } },a,name,b)
		| s -> s
	) !added in

	(* filter out properties that were already declared *)
	let props = if force_check then List.filter (function
			| p ->
				let static = is_static (IlProp p) in
				let name = p.pname in
				not (List.exists (function (IlProp _,_,n,s) -> s = static && name = n | _ -> false) !all_fields)
			(* | _ -> false *)
		) cls.cprops
		else
			cls.cprops
	in
	let cls = { cls with cmethods = List.map (fun v -> !v) meths; cprops = props } in

	let clsfields = (get_all_fields cls) @ added in
	let super_fields = !all_fields in
	all_fields := clsfields @ !all_fields;
	let refclsfields = (List.map (fun v -> ref v) clsfields) in
	(* search static / non-static name clash *)
	(* change field name to not collide with haxe keywords *)
	let fold_field acc v =
		let f, p, name, is_static = !v in
		let change, copy = match name with
		| _ when is_haxe_keyword name ->
			true, false
		| _ ->
			((is_static && List.exists (function | (f,_,n,false) -> name = n | _ -> false) !all_fields) ||
			(not is_static && match f with (* filter methods that have the same name as fields *)
			| IlMethod _ ->
				List.exists (function | ( (IlProp _ | IlField _),_,n,false) -> name = n | _ -> false) super_fields ||
				List.exists (function | ( (IlProp _ | IlField _),_,n,s) -> name = n | _ -> false) clsfields
			| _ -> false)), true
		in
		if change then begin
			let name = "%" ^ name in
			let changed = change_name name f, p, name, is_static in
			if not copy then
				v := changed;
			if copy then
				v :: ref changed :: acc
			else
				v :: acc
		end else
			v :: acc
	in
	let refclsfields = List.fold_left fold_field [] refclsfields in

	let rec fold (fields,methods,props) f = match !f with
		| IlField f,_,_,_ -> f :: fields,methods,props
		| IlMethod m,_,_,_ -> fields,m :: methods,props
		| IlProp p,_,_,_ -> fields,methods,p :: props
	in
	let fields, methods, props = List.fold_left fold ([],[],[]) refclsfields in
	{ cls with
		cfields = fields;
		cprops = props;
		cmethods = methods;
		cevents = List.filter (fun ev -> not (Hashtbl.mem all_events_name ev.ename)) cls.cevents;
	}

let add_net_std com file =
	com.net_std <- file :: com.net_std

let add_net_lib com file std =
	let ilctx = ref None in
	let netpath_to_hx = netpath_to_hx std in
	let real_file = ref file in
	let get_ctx () =
		match !ilctx with
		| Some c ->
			c
		| None ->
			let file = if Sys.file_exists file then
				file
			else try Common.find_file com file with
				| Not_found -> try Common.find_file com (file ^ ".dll") with
				| Not_found ->
					failwith (".NET lib " ^ file ^ " not found")
			in
			real_file := file;
			let r = PeReader.create_r (open_in_bin file) com.defines.Define.values in
			let ctx = PeReader.read r in
			let clr_header = PeReader.read_clr_header ctx in
			let cache = IlMetaReader.create_cache () in
			let meta = IlMetaReader.read_meta_tables ctx clr_header cache in
			close_in (r.PeReader.ch);
			if PMap.mem "net_loader_debug" com.defines.Define.values then
				print_endline ("for lib " ^ file);
			let il_typedefs = Hashtbl.copy meta.il_typedefs in
			Hashtbl.clear meta.il_typedefs;

			Hashtbl.iter (fun _ td ->
				let path = IlMetaTools.get_path (TypeDef td) in
				if PMap.mem "net_loader_debug" com.defines.Define.values then
					Printf.printf "found %s\n" (s_type_path (netpath_to_hx path));
				Hashtbl.replace com.net_path_map (netpath_to_hx path) path;
				Hashtbl.replace meta.il_typedefs path td
			) il_typedefs;
			let meta = { nstd = std; ncom = com; nil = meta } in
			ilctx := Some meta;
			meta
	in

	let cache = Hashtbl.create 0 in
	let lookup path =
		try
			Hashtbl.find cache path
		with | Not_found -> try
			let ctx = get_ctx() in
			let ns, n, cl = hxpath_to_net ctx path in
			let cls = IlMetaTools.convert_class ctx.nil (ns,n,cl) in
			let cls = normalize_ilcls ctx cls in
			Hashtbl.add cache path (Some cls);
			Some cls
		with | Not_found ->
			Hashtbl.add cache path None;
			None
	in

	let all_files () =
		Hashtbl.fold (fun path _ acc -> match path with
			| _,_ :: _, _ -> acc
			| _ -> netpath_to_hx path :: acc) (get_ctx()).nil.il_typedefs []
	in

	let build path =
		let p = { pfile = !real_file ^ " @ " ^ s_type_path path; pmin = 0; pmax = 0; } in
		let pack = match fst path with | ["haxe";"root"] -> [] | p -> p in
		let cp = ref [] in
		let rec build path = try
			if PMap.mem "net_loader_debug" com.defines.Define.values then
				Printf.printf "looking up %s\n" (s_type_path path);
			match lookup path with
			| Some({csuper = Some{snorm = LClass( (["System"],[],("Delegate"|"MulticastDelegate")),_)}} as cls)
				when List.mem SSealed cls.cflags.tdf_semantics ->
				let ctx = get_ctx() in
				let hxcls = convert_ilclass ctx p ~delegate:true cls in
				let delegate = convert_delegate ctx p cls in
				cp := (hxcls,p) :: (delegate,p) :: !cp;
				List.iter (fun ilpath ->
					let path = netpath_to_hx ilpath in
					build path
				) cls.cnested
			| Some cls ->
				let ctx = get_ctx() in
				let hxcls = convert_ilclass ctx p cls in
				cp := (hxcls,p) :: !cp;
				List.iter (fun ilpath ->
					let path = netpath_to_hx ilpath in
					build path
				) cls.cnested
			| _ -> ()
		with | Not_found | Exit ->
			()
		in
		build path;
		match !cp with
			| [] -> None
			| cp -> Some (!real_file, (pack,cp))
	in
	let build path p =
		build path
	in
	com.load_extern_type <- com.load_extern_type @ [build];
	com.net_libs <- (file, std, all_files, lookup) :: com.net_libs


let before_generate com =
	(* net version *)
	let net_ver = try
			int_of_string (PMap.find "net_ver" com.defines.Define.values)
		with | Not_found ->
			Common.define_value com Define.NetVer "20";
			20
	in
	if net_ver < 20 then
		failwith (
			".NET version is defined to target .NET "
			^ string_of_int net_ver
			^ ", but the compiler can only output code to versions equal or superior to .NET 2.0 (defined as 20)"
		);
	let rec loop = function
		| v :: acc when v <= net_ver ->
			Common.raw_define com ("NET_" ^ string_of_int v);
			loop acc
		| _ -> ()
	in
	loop [20;21;30;35;40;45];

	(* net target *)
	let net_target = try
			String.lowercase (PMap.find "net_target" com.defines.Define.values)
		with | Not_found ->
			"net"
	in
	Common.define_value com Define.NetTarget net_target;
	Common.raw_define com net_target;

	(* std dirs *)
	let stds = match com.net_std with
		| [] -> ["netlib"]
		| s -> s
	in
	(* look for all dirs that have the digraph NET_TARGET-NET_VER *)
	let digraph = net_target ^ "-" ^ string_of_int net_ver in
	let matched = ref [] in
	List.iter (fun f -> try
		let f = Common.find_file com (f ^ "/" ^ digraph) in
		matched := (f, Unix.opendir f) :: !matched
	with | _ -> ()) stds;

	if !matched = [] then failwith (
		"No .NET std lib directory with the pattern '" ^ digraph ^ "' was found in the -net-std search path. " ^
		"Try updating the hxcs lib to the latest version, or specifying another -net-std path.");
	List.iter (fun (path,f) ->
		let rec loop () =
			try
				let f = Unix.readdir f in
				let finsens = String.lowercase f in
				if String.ends_with finsens ".dll" then
					add_net_lib com (path ^ "/" ^ f) true;
				loop()
			with | End_of_file ->
				Unix.closedir f
		in
		loop()
	) !matched;

	(* now force all libraries to initialize *)
	List.iter (function (_,_,_,lookup) -> ignore (lookup ([],""))) com.net_libs
