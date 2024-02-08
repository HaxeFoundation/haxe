open Globals
open Ast
open Type
open HxbData
open HxbReaderApi

type field_reader_context = {
	t_pool : Type.t Array.t;
	pos : pos ref;
	vars : tvar Array.t;
	mutable tthis : Type.t option;
}

let create_field_reader_context p ts vars tthis = {
	t_pool = ts;
	pos = ref p;
	vars = vars;
	tthis = tthis;
}

type hxb_reader_stats = {
	modules_fully_restored : int ref;
	modules_partially_restored : int ref;
}

let create_hxb_reader_stats () = {
	modules_fully_restored = ref 0;
	modules_partially_restored = ref 0;
}

module ClassFieldInfo = struct
	type t = {
		type_parameters : typed_type_param array;
	}

	let create params = {
		type_parameters = params;
	}
end

module ClassFieldInfos = struct
	type t = {
		infos : ClassFieldInfo.t DynArray.t;
	}

	let meta = Meta.HxbId

	let create () = {
		infos = DynArray.create ()
	}

	let get infos cf =
		let _,_,p = Meta.get meta cf.cf_meta in
		DynArray.get infos.infos p.pmin

	let unset infos cf =
		cf.cf_meta <- Meta.remove meta cf.cf_meta

	let set infos info cf =
		let index = DynArray.length infos.infos in
		DynArray.add infos.infos info;
		cf.cf_meta <- (meta,[],{null_pos with pmin = index}) :: cf.cf_meta
end

module BytesWithPosition = struct
	type t = {
		bytes : bytes;
		mutable pos : int;
	}

	let create bytes = {
		bytes;
		pos = 0;
	}

	let read_byte b =
		let i = Bytes.unsafe_get b.bytes b.pos in
		b.pos <- b.pos + 1;
		int_of_char i

	let read_bytes b length =
		let out = Bytes.create length in
		Bytes.blit b.bytes b.pos out 0 length;
		b.pos <- b.pos + length;
		out

	let read_i16 i =
		let ch2 = read_byte i in
		let ch1 = read_byte i in
		let n = ch1 lor (ch2 lsl 8) in
		if ch2 land 128 <> 0 then
			n - 65536
		else
			n

	let read_real_i32 ch =
		let ch1 = read_byte ch in
		let ch2 = read_byte ch in
		let ch3 = read_byte ch in
		let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
		let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
		Int32.logor base big

		let read_i64 ch =
			let big = Int64.of_int32 (read_real_i32 ch) in
			let ch4 = read_byte ch in
			let ch3 = read_byte ch in
			let ch2 = read_byte ch in
			let ch1 = read_byte ch in
			let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
			let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
			Int64.logor (Int64.shift_left big 32) small

	let read_double ch =
		Int64.float_of_bits (read_i64 ch)
end

open BytesWithPosition

let rec read_uleb128 ch =
	let b = read_byte ch in
	if b >= 0x80 then
		(b land 0x7F) lor ((read_uleb128 ch) lsl 7)
	else
		b

let read_leb128 ch =
	let rec read acc shift =
		let b = read_byte ch in
		let acc = ((b land 0x7F) lsl shift) lor acc in
		if b >= 0x80 then
			read acc (shift + 7)
		else
			(b, acc, shift + 7)
	in
	let last, acc, shift = read 0 0 in
	let res = (if (last land 0x40) <> 0 then
		acc lor ((lnot 0) lsl shift)
	else
		acc) in
	res

let dump_stats name stats =
	print_endline (Printf.sprintf "hxb_reader stats for %s" name);
	print_endline (Printf.sprintf "  modules partially restored: %i" (!(stats.modules_fully_restored) - !(stats.modules_partially_restored)));
	print_endline (Printf.sprintf "  modules fully restored: %i" !(stats.modules_fully_restored));

class hxb_reader
	(mpath : path)
	(stats : hxb_reader_stats)
= object(self)
	val mutable api = Obj.magic ""
	val mutable current_module = null_module

	val mutable ch = BytesWithPosition.create (Bytes.create 0)
	val mutable string_pool = Array.make 0 ""
	val mutable doc_pool = Array.make 0 ""

	val mutable classes = Array.make 0 null_class
	val mutable abstracts = Array.make 0 null_abstract
	val mutable enums = Array.make 0 null_enum
	val mutable typedefs = Array.make 0 null_typedef
	val mutable anons = Array.make 0 null_tanon
	val mutable anon_fields = Array.make 0 null_field
	val mutable tmonos = Array.make 0 (mk_mono())
	val mutable class_fields = Array.make 0 null_field
	val mutable enum_fields = Array.make 0 null_enum_field

	val mutable type_type_parameters = Array.make 0 (mk_type_param null_class TPHType None None)
	val mutable field_type_parameters = Array.make 0 (mk_type_param null_class TPHMethod None None)
	val mutable local_type_parameters = Array.make 0 (mk_type_param null_class TPHLocal None None)

	val mutable field_type_parameter_offset = 0
	val empty_anon = mk_anon (ref Closed)

	method resolve_type pack mname tname =
		try
			api#resolve_type pack mname tname
		with Not_found ->
			dump_backtrace();
			error (Printf.sprintf "[HXB] [%s] Cannot resolve type %s" (s_type_path current_module.m_path) (s_type_path ((pack @ [mname]),tname)))

	(* Primitives *)

	method read_i32 =
		read_real_i32 ch

	method read_i16 =
		read_i16 ch

	method read_f64 =
		read_double ch

	method read_bool =
		read_byte ch <> 0

	method read_from_string_pool pool =
		pool.(read_uleb128 ch)

	method read_string =
		self#read_from_string_pool string_pool

	method read_raw_string =
		let l = read_uleb128 ch in
		Bytes.unsafe_to_string (read_bytes ch l)

	(* Basic compounds *)

	method read_list : 'a . (unit -> 'a) -> 'a list = fun f ->
		let l = read_uleb128 ch in
		List.init l (fun _ -> f ())

	method read_option : 'a . (unit -> 'a) -> 'a option = fun f ->
		match read_byte ch with
		| 0 ->
			None
		| _ ->
			Some (f())

	method read_path =
		let pack = self#read_list (fun () -> self#read_string) in
		let name = self#read_string in
		(pack,name)

	method read_full_path =
		let pack = self#read_list (fun () -> self#read_string) in
		let mname = self#read_string in
		let tname = self#read_string in
		(pack,mname,tname)

	method read_documentation =
		let doc_own = self#read_option (fun () ->
			self#read_from_string_pool doc_pool
		) in
		let doc_inherited = self#read_list (fun () ->
			self#read_from_string_pool doc_pool
		) in
		{doc_own;doc_inherited}

	method read_pos =
		let file = self#read_string in
		let min = read_leb128 ch in
		let max = read_leb128 ch in
		let pos = {
			pfile = file;
			pmin = min;
			pmax = max;
		} in
		pos

	method read_pos_pair =
		let file = self#read_string in
		let min1 = read_leb128 ch in
		let max1 = read_leb128 ch in
		let min2 = read_leb128 ch in
		let max2 = read_leb128 ch in
		let pos1 = {
			pfile = file;
			pmin = min1;
			pmax = max1;
		} in
		let pos2 = {
			pos1 with
			pmin = pos1.pmin + min2;
			pmax = pos1.pmin + max2;
		} in
		pos1,pos2

	method read_metadata_entry : metadata_entry =
		let name = self#read_string in
		let p = self#read_pos in
		let el = self#read_list (fun () -> self#read_expr) in
		(Meta.from_string name,el,p)

	method read_metadata =
		self#read_list (fun () -> self#read_metadata_entry)

	(* References *)

	method read_class_ref =
		classes.(read_uleb128 ch)

	method read_abstract_ref =
		abstracts.(read_uleb128 ch)

	method read_enum_ref =
		enums.(read_uleb128 ch)

	method read_typedef_ref =
		typedefs.(read_uleb128 ch)

	method read_field_ref =
		class_fields.(read_uleb128 ch)

	method read_enum_field_ref =
		enum_fields.(read_uleb128 ch)

	method read_anon_ref =
		match read_byte ch with
		| 0 ->
			anons.(read_uleb128 ch)
		| 1 ->
			let an = anons.(read_uleb128 ch) in
			self#read_anon an
		| _ ->
			assert false

	method read_anon_field_ref =
		match read_byte ch with
		| 0 ->
			anon_fields.(read_uleb128 ch)
		| 1 ->
			let cf = anon_fields.(read_uleb128 ch) in
			self#read_class_field_and_overloads_data cf;
			cf
		| _ ->
			assert false

	(* Expr *)

	method get_binop i = match i with
		| 0 -> OpAdd
		| 1 -> OpMult
		| 2 -> OpDiv
		| 3 -> OpSub
		| 4 -> OpAssign
		| 5 -> OpEq
		| 6 -> OpNotEq
		| 7 -> OpGt
		| 8 -> OpGte
		| 9 -> OpLt
		| 10 -> OpLte
		| 11 -> OpAnd
		| 12 -> OpOr
		| 13 -> OpXor
		| 14 -> OpBoolAnd
		| 15 -> OpBoolOr
		| 16 -> OpShl
		| 17 -> OpShr
		| 18 -> OpUShr
		| 19 -> OpMod
		| 20 -> OpInterval
		| 21 -> OpArrow
		| 22 -> OpIn
		| 23 -> OpNullCoal
		| _ -> OpAssignOp (self#get_binop (i - 30))

	method get_unop i = match i with
		| 0 -> Increment,Prefix
		| 1 -> Decrement,Prefix
		| 2 -> Not,Prefix
		| 3 -> Neg,Prefix
		| 4 -> NegBits,Prefix
		| 5 -> Spread,Prefix
		| 6 -> Increment,Postfix
		| 7 -> Decrement,Postfix
		| 8 -> Not,Postfix
		| 9 -> Neg,Postfix
		| 10 -> NegBits,Postfix
		| 11 -> Spread,Postfix
		| _ -> assert false

	method read_placed_name =
		let s = self#read_string in
		let p = self#read_pos in
		(s,p)

	method read_type_path =
		let pack = self#read_list (fun () -> self#read_string) in
		let name = self#read_string in
		let tparams = self#read_list (fun () -> self#read_type_param_or_const) in
		let tsub = self#read_option (fun () -> self#read_string) in
		{
			tpackage = pack;
			tname = name;
			tparams = tparams;
			tsub = tsub;
		}

	method read_placed_type_path =
		let tp = self#read_type_path in
		let pfull,ppath = self#read_pos_pair in
		{
			path = tp;
			pos_full = pfull;
			pos_path = ppath;
		}

	method read_type_param =
		let pn = self#read_placed_name in
		let ttp = self#read_list (fun () -> self#read_type_param) in
		let tho = self#read_option (fun () -> self#read_type_hint) in
		let def = self#read_option (fun () -> self#read_type_hint) in
		let meta = self#read_metadata in
		{
			tp_name = pn;
			tp_params = ttp;
			tp_constraints = tho;
			tp_meta = meta;
			tp_default = def;
		}

	method read_type_param_or_const =
		match read_byte ch with
		| 0 -> TPType (self#read_type_hint)
		| 1 -> TPExpr (self#read_expr)
		| _ -> assert false

	method read_func_arg =
		let pn = self#read_placed_name in
		let b = self#read_bool in
		let meta = self#read_metadata in
		let tho = self#read_option (fun () -> self#read_type_hint) in
		let eo = self#read_option (fun () -> self#read_expr) in
		(pn,b,meta,tho,eo)

	method read_func =
		let params = self#read_list (fun () -> self#read_type_param) in
		let args = self#read_list (fun () -> self#read_func_arg) in
		let tho = self#read_option (fun () -> self#read_type_hint) in
		let eo = self#read_option (fun () -> self#read_expr) in
		{
			f_params = params;
			f_args = args;
			f_type = tho;
			f_expr = eo;
		}

	method read_complex_type =
		match read_byte ch with
		| 0 -> CTPath (self#read_placed_type_path)
		| 1 ->
			let thl = self#read_list (fun () -> self#read_type_hint) in
			let th = self#read_type_hint in
			CTFunction(thl,th)
		| 2 -> CTAnonymous (self#read_list (fun () -> self#read_cfield))
		| 3 -> CTParent (self#read_type_hint)
		| 4 ->
			let ptp = self#read_list (fun () -> self#read_placed_type_path) in
			let cffl = self#read_list (fun () -> self#read_cfield) in
			CTExtend(ptp,cffl)
		| 5 -> CTOptional (self#read_type_hint)
		| 6 ->
			let pn = self#read_placed_name in
			let th = self#read_type_hint in
			CTNamed(pn,th)
		| 7 -> CTIntersection (self#read_list (fun () -> self#read_type_hint))
		| _ -> assert false

	method read_type_hint =
		let ct = self#read_complex_type in
		let p = self#read_pos in
		(ct,p)

	method read_access =
		match read_byte ch with
		| 0 -> APublic
		| 1 -> APrivate
		| 2 -> AStatic
		| 3 -> AOverride
		| 4 -> ADynamic
		| 5 -> AInline
		| 6 -> AMacro
		| 7 -> AFinal
		| 8 -> AExtern
		| 9 -> AAbstract
		| 10 -> AOverload
		| 11 -> AEnum
		| _ -> assert false

	method read_placed_access =
		let ac = self#read_access in
		let p = self#read_pos in
		(ac,p)

	method read_cfield_kind =
		match read_byte ch with
		| 0 ->
			let tho = self#read_option (fun () -> self#read_type_hint) in
			let eo = self#read_option (fun () -> self#read_expr) in
			FVar(tho,eo)
		| 1 -> FFun (self#read_func)
		| 2 ->
			let pn1 = self#read_placed_name in
			let pn2 = self#read_placed_name in
			let tho = self#read_option (fun () -> self#read_type_hint) in
			let eo = self#read_option (fun () -> self#read_expr) in
			FProp(pn1,pn2,tho,eo)
		| _ -> assert false

	method read_cfield =
		let pn = self#read_placed_name in
		let doc = self#read_option (fun () -> self#read_documentation) in
		let pos = self#read_pos in
		let meta = self#read_metadata in
		let access = self#read_list (fun () -> self#read_placed_access) in
		let kind = self#read_cfield_kind in
		{
			cff_name = pn;
			cff_doc = doc;
			cff_pos = pos;
			cff_meta = meta;
			cff_access = access;
			cff_kind = kind;
		}

	method read_expr =
		let p = self#read_pos in
		let e = match read_byte ch with
		| 0 ->
			let s = self#read_string in
			let suffix = self#read_option (fun () -> self#read_string) in
			EConst (Int (s, suffix))
		| 1 ->
			let s = self#read_string in
			let suffix = self#read_option (fun () -> self#read_string) in
			EConst (Float (s, suffix))
		| 2 ->
			let s = self#read_string in
			let qs = begin match read_byte ch with
			| 0 -> SDoubleQuotes
			| 1 -> SSingleQuotes
			| _ -> assert false
			end in
			EConst (String (s,qs))
		| 3 ->
			EConst (Ident (self#read_string))
		| 4 ->
			let s1 = self#read_string in
			let s2 = self#read_string in
			EConst (Regexp(s1,s2))
		| 5 ->
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			EArray(e1,e2)
		| 6 ->
			let op = self#get_binop (read_byte ch) in
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			EBinop(op,e1,e2)
		| 7 ->
			let e = self#read_expr in
			let s = self#read_string in
			let kind = begin match read_byte ch with
			| 0 -> EFNormal
			| 1 -> EFSafe
			| _ -> assert false
			end in
			EField(e,s,kind)
		| 8 ->
			EParenthesis (self#read_expr)
		| 9 ->
			let fields = self#read_list (fun () ->
				let n = self#read_string in
				let p = self#read_pos in
				let qs = begin match read_byte ch with
				| 0 -> NoQuotes
				| 1 -> DoubleQuotes
				| _ -> assert false
				end in
				let e = self#read_expr in
				((n,p,qs),e)
			) in
			EObjectDecl fields
		| 10 ->
			let el = self#read_list (fun () -> self#read_expr) in
			EArrayDecl el
		| 11 ->
			let e = self#read_expr in
			let el = self#read_list (fun () -> self#read_expr) in
			ECall(e,el)
		| 12 ->
			let ptp = self#read_placed_type_path in
			let el = self#read_list (fun () -> self#read_expr) in
			ENew(ptp,el)
		| 13 ->
			let (op,flag) = self#get_unop (read_byte ch) in
			let e = self#read_expr in
			EUnop(op,flag,e)
		| 14 ->
			let vl = self#read_list (fun () ->
				let name = self#read_placed_name in
				let final = self#read_bool in
				let static = self#read_bool in
				let t = self#read_option (fun () -> self#read_type_hint) in
				let expr = self#read_option (fun () -> self#read_expr) in
				let meta = self#read_metadata in
				{
					ev_name = name;
					ev_final = final;
					ev_static = static;
					ev_type = t;
					ev_expr = expr;
					ev_meta = meta;
				}
			) in
			EVars vl
		| 15 ->
			let fk = begin match read_byte ch with
			| 0 -> FKAnonymous
			| 1 ->
				let pn = self#read_placed_name in
				let b = self#read_bool in
				FKNamed(pn,b)
			| 2 -> FKArrow
			| _ -> assert false end in
			let f = self#read_func in
			EFunction(fk,f)
		| 16 ->
			EBlock (self#read_list (fun () -> self#read_expr))
		| 17 ->
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			EFor(e1,e2)
		| 18 ->
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			EIf(e1,e2,None)
		| 19 ->
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			let e3 = self#read_expr in
			EIf(e1,e2,Some e3)
		| 20 ->
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			EWhile(e1,e2,NormalWhile)
		| 21 ->
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			EWhile(e1,e2,DoWhile)
		| 22 ->
			let e1 = self#read_expr in
			let cases = self#read_list (fun () ->
				let el = self#read_list (fun () -> self#read_expr) in
				let eg = self#read_option (fun () -> self#read_expr) in
				let eo = self#read_option (fun () -> self#read_expr) in
				let p = self#read_pos in
				(el,eg,eo,p)
			) in
			let def = self#read_option (fun () ->
				let eo = self#read_option (fun () -> self#read_expr) in
				let p = self#read_pos in
				(eo,p)
			) in
			ESwitch(e1,cases,def)
		| 23 ->
			let e1 = self#read_expr in
			let catches = self#read_list (fun () ->
				let pn = self#read_placed_name in
				let th = self#read_option (fun () -> self#read_type_hint) in
				let e = self#read_expr in
				let p = self#read_pos in
				(pn,th,e,p)
			) in
			ETry(e1,catches)
		| 24 -> EReturn None
		| 25 -> EReturn (Some (self#read_expr))
		| 26 -> EBreak
		| 27 -> EContinue
		| 28 -> EUntyped (self#read_expr)
		| 29 -> EThrow (self#read_expr)
		| 30 -> ECast ((self#read_expr),None)
		| 31 ->
			let e1 = self#read_expr in
			let th = self#read_type_hint in
			ECast(e1,Some th)
		| 32 ->
			let e1 = self#read_expr in
			let th = self#read_type_hint in
			EIs(e1,th)
		| 33 ->
			let e1 = self#read_expr in
			let dk = begin match read_byte ch with
			| 0 -> DKCall
			| 1 -> DKDot
			| 2 -> DKStructure
			| 3 -> DKMarked
			| 4 -> DKPattern (self#read_bool)
			| _ -> assert false end in
			EDisplay(e1,dk)
		| 34 ->
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			let e3 = self#read_expr in
			ETernary(e1,e2,e3)
		| 35 ->
			let e1 = self#read_expr in
			let th = self#read_type_hint in
			ECheckType(e1,th)
		| 36 ->
			let m = self#read_metadata_entry in
			let e = self#read_expr in
			EMeta(m,e)
		| _ -> assert false
		in
		(e,p)

	(* Type instances *)

	method resolve_ttp_ref = function
		| 1 ->
			let i = read_uleb128 ch in
			(type_type_parameters.(i))
		| 2 ->
			let i = read_uleb128 ch in
			(field_type_parameters.(i))
		| 3 ->
			let k = read_uleb128 ch in
			local_type_parameters.(k)
		| _ ->
			die "" __LOC__

	method read_type_instance =
		let read_fun_arg () =
			let name = self#read_string in
			let opt = self#read_bool in
			let t = self#read_type_instance in
			(name,opt,t)
		in
		match (read_byte ch) with
		| 0 ->
			let i = read_uleb128 ch in
			tmonos.(i)
		| 1 ->
			let i = read_uleb128 ch in
			(type_type_parameters.(i)).ttp_type
		| 2 ->
			let i = read_uleb128 ch in
			(field_type_parameters.(i)).ttp_type
		| 3 ->
			let k = read_uleb128 ch in
			local_type_parameters.(k).ttp_type
		| 4 ->
			t_dynamic
		| 10 ->
			let c = self#read_class_ref in
			c.cl_type
		| 11 ->
			let en = self#read_enum_ref in
			en.e_type
		| 12 ->
			let a = self#read_abstract_ref in
			TType(abstract_module_type a [],[])
		| 13 ->
			let e = self#read_expr in
			let c = {null_class with cl_kind = KExpr e; cl_module = current_module } in
			TInst(c, [])
		| 20 ->
			TFun([],api#basic_types.tvoid)
		| 21 ->
			let arg1 = read_fun_arg () in
			TFun([arg1],api#basic_types.tvoid)
		| 22 ->
			let arg1 = read_fun_arg () in
			let arg2 = read_fun_arg () in
			TFun([arg1;arg2],api#basic_types.tvoid)
		| 23 ->
			let arg1 = read_fun_arg () in
			let arg2 = read_fun_arg () in
			let arg3 = read_fun_arg () in
			TFun([arg1;arg2;arg3],api#basic_types.tvoid)
		| 24 ->
			let arg1 = read_fun_arg () in
			let arg2 = read_fun_arg () in
			let arg3 = read_fun_arg () in
			let arg4 = read_fun_arg () in
			TFun([arg1;arg2;arg3;arg4],api#basic_types.tvoid)
		| 29 ->
			let args = self#read_list read_fun_arg in
			TFun(args,api#basic_types.tvoid)
		| 30 ->
			let ret = self#read_type_instance in
			TFun([],ret)
		| 31 ->
			let arg1 = read_fun_arg () in
			let ret = self#read_type_instance in
			TFun([arg1],ret)
		| 32 ->
			let arg1 = read_fun_arg () in
			let arg2 = read_fun_arg () in
			let ret = self#read_type_instance in
			TFun([arg1;arg2],ret)
		| 33 ->
			let arg1 = read_fun_arg () in
			let arg2 = read_fun_arg () in
			let arg3 = read_fun_arg () in
			let ret = self#read_type_instance in
			TFun([arg1;arg2;arg3],ret)
		| 34 ->
			let arg1 = read_fun_arg () in
			let arg2 = read_fun_arg () in
			let arg3 = read_fun_arg () in
			let arg4 = read_fun_arg () in
			let ret = self#read_type_instance in
			TFun([arg1;arg2;arg3;arg4],ret)
		| 39 ->
			let args = self#read_list read_fun_arg in
			let ret = self#read_type_instance in
			TFun(args,ret)
		| 40 ->
			let c = self#read_class_ref in
			TInst(c,[])
		| 41 ->
			let c = self#read_class_ref in
			let t1 = self#read_type_instance in
			TInst(c,[t1])
		| 42 ->
			let c = self#read_class_ref in
			let t1 = self#read_type_instance in
			let t2 = self#read_type_instance in
			TInst(c,[t1;t2])
		| 49 ->
			let c = self#read_class_ref in
			let tl = self#read_types in
			TInst(c,tl)
		| 50 ->
			let en = self#read_enum_ref in
			TEnum(en,[])
		| 51 ->
			let en = self#read_enum_ref in
			let t1 = self#read_type_instance in
			TEnum(en,[t1])
		| 52 ->
			let en = self#read_enum_ref in
			let t1 = self#read_type_instance in
			let t2 = self#read_type_instance in
			TEnum(en,[t1;t2])
		| 59 ->
			let e = self#read_enum_ref in
			let tl = self#read_types in
			TEnum(e,tl)
		| 60 ->
			let td = self#read_typedef_ref in
			TType(td,[])
		| 61 ->
			let td = self#read_typedef_ref in
			let t1 = self#read_type_instance in
			TType(td,[t1])
		| 62 ->
			let td = self#read_typedef_ref in
			let t1 = self#read_type_instance in
			let t2 = self#read_type_instance in
			TType(td,[t1;t2])
		| 69 ->
			let t = self#read_typedef_ref in
			let tl = self#read_types in
			TType(t,tl)
		| 70 ->
			let a = self#read_abstract_ref in
			TAbstract(a,[])
		| 71 ->
			let a = self#read_abstract_ref in
			let t1 = self#read_type_instance in
			TAbstract(a,[t1])
		| 72 ->
			let a = self#read_abstract_ref in
			let t1 = self#read_type_instance in
			let t2 = self#read_type_instance in
			TAbstract(a,[t1;t2])
		| 79 ->
			let a = self#read_abstract_ref in
			let tl = self#read_types in
			TAbstract(a,tl)
		| 80 ->
			empty_anon
		| 81 ->
			TAnon self#read_anon_ref
		| 89 ->
			TDynamic (Some self#read_type_instance)
		| 100 ->
			api#basic_types.tvoid
		| 101 ->
			api#basic_types.tint
		| 102 ->
			api#basic_types.tfloat
		| 103 ->
			api#basic_types.tbool
		| 104 ->
			api#basic_types.tstring
		| i ->
			error (Printf.sprintf "Bad type instance id: %i" i)

	method read_types =
		self#read_list (fun () -> self#read_type_instance)

	method read_type_parameters_forward =
		let length = read_uleb128 ch in
		Array.init length (fun _ ->
			let path = self#read_path in
			let pos = self#read_pos in
			let host = match read_byte ch with
				| 0 -> TPHType
				| 1 -> TPHConstructor
				| 2 -> TPHMethod
				| 3 -> TPHEnumConstructor
				| 4 -> TPHAnonField
				| 5 -> TPHLocal
				| i -> die (Printf.sprintf "Invalid type paramter host: %i" i) __LOC__
			in
			let c = mk_class current_module path pos pos in
			mk_type_param c host None None
		)

	method read_type_parameters_data (a : typed_type_param array) =
		Array.iter (fun ttp ->
			let meta = self#read_metadata in
			let constraints = self#read_types in
			let def = self#read_option (fun () -> self#read_type_instance) in
			let c = ttp.ttp_class in
			ttp.ttp_default <- def;
			ttp.ttp_constraints <- Some (Lazy.from_val constraints);
			c.cl_meta <- meta;
		) a

	(* Fields *)

	method read_field_kind = match read_byte ch with
		| 0 -> Method MethNormal
		| 1 -> Method MethInline
		| 2 -> Method MethDynamic
		| 3 -> Method MethMacro
		| 10 -> Var {v_read = AccNormal;v_write = AccNormal}
		| 11 -> Var {v_read = AccNormal;v_write = AccNo}
		| 12 -> Var {v_read = AccNormal;v_write = AccNever}
		| 13 -> Var {v_read = AccNormal;v_write = AccCtor}
		| 14 -> Var {v_read = AccNormal;v_write = AccCall}
		| 20 -> Var {v_read = AccInline;v_write = AccNever}
		| 30 -> Var {v_read = AccCall;v_write = AccNormal}
		| 31 -> Var {v_read = AccCall;v_write = AccNo}
		| 32 -> Var {v_read = AccCall;v_write = AccNever}
		| 33 -> Var {v_read = AccCall;v_write = AccCtor}
		| 34 -> Var {v_read = AccCall;v_write = AccCall}
		| 100 ->
			let f = function
				| 0 -> AccNormal
				| 1 -> AccNo
				| 2 -> AccNever
				| 3 -> AccCtor
				| 4 -> AccCall
				| 5 -> AccInline
				| 6 ->
					let s = self#read_string in
					let so = self#read_option (fun () -> self#read_string) in
					AccRequire(s,so)
				| i ->
					error (Printf.sprintf "Bad accessor kind: %i" i)
			in
			let r = f (read_byte ch) in
			let w = f (read_byte ch) in
			Var {v_read = r;v_write = w}
		| i ->
			error (Printf.sprintf "Bad field kind: %i" i)

	method read_var_kind =
		match read_byte ch with
			| 0 -> VUser TVOLocalVariable
			| 1 -> VUser TVOArgument
			| 2 -> VUser TVOForVariable
			| 3 -> VUser TVOPatternVariable
			| 4 -> VUser TVOCatchVariable
			| 5 -> VUser TVOLocalFunction
			| 6 -> VGenerated
			| 7 -> VInlined
			| 8 -> VInlinedConstructorVariable
			| 9 -> VExtractorVariable
			| 10 -> VAbstractThis
			| _ -> assert false

	method read_var =
		let id = read_uleb128 ch in
		let name = self#read_string in
		let kind = self#read_var_kind in
		let flags = read_uleb128 ch in
		let meta = self#read_metadata in
		let pos = self#read_pos in
		let v = {
			v_id = api#get_var_id id;
			v_name = name;
			v_type = t_dynamic;
			v_kind = kind;
			v_meta = meta;
			v_pos = pos;
			v_extra = None;
			v_flags = flags;
		} in
		v

	method read_texpr fctx =

		let declare_local () =
			let v = fctx.vars.(read_uleb128 ch) in
			v.v_extra <- self#read_option (fun () ->
				let params = self#read_list (fun () ->
					let i = read_uleb128 ch in
					local_type_parameters.(i)
				) in
				let vexpr = self#read_option (fun () -> self#read_texpr fctx) in
				{
					v_params = params;
					v_expr = vexpr;
				};
			);
			v.v_type <- self#read_type_instance;
			v
		in
		let update_pmin () =
			fctx.pos := {!(fctx.pos) with pmin = read_leb128 ch};
		in
		let update_pmax () =
			fctx.pos := {!(fctx.pos) with pmax = read_leb128 ch};
		in
		let update_pminmax () =
			let pmin = read_leb128 ch in
			let pmax = read_leb128 ch in
			fctx.pos := {!(fctx.pos) with pmin; pmax};
		in
		let update_p () =
			fctx.pos := self#read_pos;
		in
		let read_relpos () =
			begin match read_byte ch with
				| 0 ->
					()
				| 1 ->
					update_pmin ()
				| 2 ->
					update_pmax ()
				| 3 ->
					update_pminmax ()
				| 4 ->
					update_p ()
				| _ ->
					assert false
			end;
			!(fctx.pos)
		in
		let rec loop () =
			let loop2 () =
				match read_byte ch with
					(* values 0-19 *)
					| 0 -> TConst TNull,None
					| 1 -> TConst TThis,fctx.tthis
					| 2 -> TConst TSuper,None
					| 3 -> TConst (TBool false),(Some api#basic_types.tbool)
					| 4 -> TConst (TBool true),(Some api#basic_types.tbool)
					| 5 -> TConst (TInt self#read_i32),(Some api#basic_types.tint)
					| 6 -> TConst (TFloat self#read_string),(Some api#basic_types.tfloat)
					| 7 -> TConst (TString self#read_string),(Some api#basic_types.tstring)
					| 13 -> TConst (TBool false),None
					| 14 -> TConst (TBool true),None
					| 15 -> TConst (TInt self#read_i32),None
					| 16 -> TConst (TFloat self#read_string),None
					| 17 -> TConst (TString self#read_string),None

					(* vars 20-29 *)
					| 20 ->
						TLocal (fctx.vars.(read_uleb128 ch)),None
					| 21 ->
						let v = declare_local () in
						TVar (v,None),(Some api#basic_types.tvoid)
					| 22 ->
						let v = declare_local () in
						let e = loop () in
						TVar (v, Some e),(Some api#basic_types.tvoid)

					(* blocks 30-49 *)
					| 30 ->
						TBlock [],None
					| 31 | 32 | 33 | 34 | 35 as i ->
						let l = i - 30 in
						let el = List.init l (fun _ -> loop ()) in
						TBlock el,None
					| 36 ->
						let l = read_byte ch in
						let el = List.init l (fun _ -> loop ()) in
						TBlock el,None
					| 39 ->
						let el = self#read_list loop in
						TBlock el,None

					(* function 50-59 *)
					| 50 ->
						let read_tfunction_arg () =
							let v = declare_local () in
							let cto = self#read_option loop in
							(v,cto)
						in
						let args = self#read_list read_tfunction_arg in
						let r = self#read_type_instance in
						let e = loop () in
						TFunction {
							tf_args = args;
							tf_type = r;
							tf_expr = e;
						},None
					(* texpr compounds 60-79 *)
					| 60 ->
						let e1 = loop () in
						let e2 = loop () in
						TArray (e1,e2),None
					| 61 ->
						let e = loop () in
						TParenthesis e,Some e.etype
					| 62 ->
						TArrayDecl (loop_el()),None
					| 63 ->
						let fl = self#read_list (fun () ->
							let name = self#read_string in
							let p = self#read_pos in
							let qs = match read_byte ch with
								| 0 -> NoQuotes
								| 1 -> DoubleQuotes
								| _ -> assert false
							in
							let e = loop () in
							((name,p,qs),e)
						) in
						TObjectDecl fl,None
					| 65 ->
						let m = self#read_metadata_entry in
						let e1 = loop () in
						TMeta (m,e1),None

					(* calls 70 - 79 *)
					| 70 ->
						let e1 = loop () in
						TCall(e1,[]),None
					| 71 | 72 | 73 | 74 as i ->
						let e1 = loop () in
						let el = List.init (i - 70) (fun _ -> loop ()) in
						TCall(e1,el),None
					| 79 ->
						let e1 = loop () in
						let el = self#read_list loop in
						TCall(e1,el),None

					(* branching 80-89 *)
					| 80 ->
						let e1 = loop () in
						let e2 = loop () in
						TIf(e1,e2,None),(Some api#basic_types.tvoid)
					| 81 ->
						let e1 = loop () in
						let e2 = loop () in
						let e3 = loop () in
						TIf(e1,e2,Some e3),None
					| 82 ->
						let subject = loop () in
						let cases = self#read_list (fun () ->
							let patterns = loop_el() in
							let ec = loop () in
							{ case_patterns = patterns; case_expr = ec}
						) in
						let def = self#read_option (fun () -> loop ()) in
						TSwitch {
							switch_subject = subject;
							switch_cases = cases;
							switch_default = def;
							switch_exhaustive = true;
						},None
					| 83 ->
						let e1 = loop () in
						let catches = self#read_list (fun () ->
							let v = declare_local () in
							let e = loop () in
							(v,e)
						) in
						TTry(e1,catches),None
					| 84 ->
						let e1 = loop () in
						let e2 = loop () in
						TWhile(e1,e2,NormalWhile),(Some api#basic_types.tvoid)
					| 85 ->
						let e1 = loop () in
						let e2 = loop () in
						TWhile(e1,e2,DoWhile),(Some api#basic_types.tvoid)
					| 86 ->
						let v  = declare_local () in
						let e1 = loop () in
						let e2 = loop () in
						TFor(v,e1,e2),(Some api#basic_types.tvoid)

					(* control flow 90-99 *)
					| 90 ->
						TReturn None,None
					| 91 ->
						TReturn (Some (loop ())),None
					| 92 ->
						TContinue,None
					| 93 ->
						TBreak,None
					| 94 ->
						TThrow (loop ()),None

					(* access 100-119 *)
					| 100 ->
						TEnumIndex (loop ()),(Some api#basic_types.tint)
					| 101 ->
						let e1 = loop () in
						let ef = self#read_enum_field_ref in
						let i = read_uleb128 ch in
						TEnumParameter(e1,ef,i),None
					| 102 ->
						let e1 = loop () in
						let c = self#read_class_ref in
						let tl = self#read_types in
						let cf = self#read_field_ref in
						TField(e1,FInstance(c,tl,cf)),None
					| 103 ->
						let e1 = loop () in
						let c = self#read_class_ref in
						let cf = self#read_field_ref in
						TField(e1,FStatic(c,cf)),None
					| 104 ->
						let e1 = loop () in
						let cf = self#read_anon_field_ref in
						TField(e1,FAnon(cf)),None
					| 105 ->
						let e1 = loop () in
						let c = self#read_class_ref in
						let tl = self#read_types in
						let cf = self#read_field_ref in
						TField(e1,FClosure(Some(c,tl),cf)),None
					| 106 ->
						let e1 = loop () in
						let cf = self#read_anon_field_ref in
						TField(e1,FClosure(None,cf)),None
					| 107 ->
						let e1 = loop () in
						let en = self#read_enum_ref in
						let ef = self#read_enum_field_ref in
						TField(e1,FEnum(en,ef)),None
					| 108 ->
						let e1 = loop () in
						let s = self#read_string in
						TField(e1,FDynamic s),None

					| 110 ->
						let p = read_relpos () in
						let c = self#read_class_ref in
						let cf = self#read_field_ref in
						let e1 = Texpr.Builder.make_static_this c p in
						TField(e1,FStatic(c,cf)),None
					| 111 ->
						let p = read_relpos () in
						let c = self#read_class_ref in
						let tl = self#read_types in
						let cf = self#read_field_ref in
						let ethis = mk (TConst TThis) (Option.get fctx.tthis) p in
						TField(ethis,FInstance(c,tl,cf)),None

					(* module types 120-139 *)
					| 120 ->
						let c = self#read_class_ref in
						TTypeExpr (TClassDecl c),(Some c.cl_type)
					| 121 ->
						let en = self#read_enum_ref in
						TTypeExpr (TEnumDecl en),(Some en.e_type)
					| 122 ->
						TTypeExpr (TAbstractDecl self#read_abstract_ref),None
					| 123 ->
						TTypeExpr (TTypeDecl self#read_typedef_ref),None
					| 124 ->
						TCast(loop (),None),None
					| 125 ->
						let e1 = loop () in
						let (pack,mname,tname) = self#read_full_path in
						let mt = self#resolve_type pack mname tname in
						TCast(e1,Some mt),None
					| 126 ->
						let c = self#read_class_ref in
						let tl = self#read_types in
						let el = loop_el() in
						TNew(c,tl,el),None
					| 127 ->
						let ttp = self#resolve_ttp_ref (read_uleb128 ch) in
						let tl = self#read_types in
						let el = loop_el() in
						TNew(ttp.ttp_class,tl,el),None
					| 128 ->
						let ttp = self#resolve_ttp_ref (read_uleb128 ch) in
						TTypeExpr (TClassDecl ttp.ttp_class),None

					(* unops 140-159 *)
					| i when i >= 140 && i < 160 ->
						let (op,flag) = self#get_unop (i - 140) in
						let e = loop () in
						TUnop(op,flag,e),None

					(* binops 160-219 *)
					| i when i >= 160 && i < 220 ->
						let op = self#get_binop (i - 160) in
						let e1 = loop () in
						let e2 = loop () in
						TBinop(op,e1,e2),None
					(* rest 250-254 *)
					| 250 ->
						TIdent (self#read_string),None

					| i ->
						die (Printf.sprintf "  [ERROR] Unhandled texpr %d at:" i) __LOC__
				in
				let e,t = loop2 () in
				let t = match t with
					| None -> fctx.t_pool.(read_uleb128 ch)
					| Some t -> t
				in
				let p = read_relpos () in
				let e = {
					eexpr = e;
					etype = t;
					epos = p;
				} in
				e
		and loop_el () =
			self#read_list loop
		in
		loop()

	method read_class_field_forward =
		let name = self#read_string in
		let pos,name_pos = self#read_pos_pair in
		let overloads = self#read_list (fun () -> self#read_class_field_forward) in
		{ null_field with cf_name = name; cf_pos = pos; cf_name_pos = name_pos; cf_overloads = overloads }

	method start_texpr =
		begin match read_byte ch with
			| 0 ->
				()
			| 1 ->
				let a = self#read_type_parameters_forward in
				local_type_parameters <- a;
				self#read_type_parameters_data a;
			| i ->
				die "" __LOC__
		end;
		let tthis = self#read_option (fun () -> self#read_type_instance) in
		let l = read_uleb128 ch in
		let ts = Array.init l (fun _ ->
			self#read_type_instance
		) in
		let l = read_uleb128 ch in
		let vars = Array.init l (fun _ ->
			self#read_var
		) in
		create_field_reader_context self#read_pos ts vars tthis

	method read_field_type_parameters =
		let num_params = read_uleb128 ch in
		begin match read_byte ch with
			| 0 ->
				()
			| 1 ->
				let a = self#read_type_parameters_forward in
				field_type_parameters <- a;
				self#read_type_parameters_data a;
				field_type_parameter_offset <- 0; (* num_params is added below *)
			| i ->
				die "" __LOC__
		end;
		let params = List.init num_params (fun offset ->
			field_type_parameters.(field_type_parameter_offset + offset)
		) in
		field_type_parameter_offset <- field_type_parameter_offset + num_params;
		params

	method read_expression (fctx : field_reader_context) =
		let e = self#read_texpr fctx in
		let e_unopt = self#read_option (fun () -> self#read_texpr fctx) in
		e,e_unopt

	val class_field_infos = ClassFieldInfos.create ()

	method read_class_field_data (cf : tclass_field) : unit =
		let params = self#read_field_type_parameters in

		let t = self#read_type_instance in

		let flags = read_uleb128 ch in

		let doc = self#read_option (fun () -> self#read_documentation) in
		cf.cf_meta <- self#read_metadata;
		let kind = self#read_field_kind in

		let expr,expr_unoptimized = match read_byte ch with
			| 0 ->
				None,None
			| 1 ->
				let fctx = self#start_texpr in
				let e,e_unopt = self#read_expression fctx in
				(Some e,e_unopt)
			| 2 ->
				(* store type parameter info for EXD *)
				let info = ClassFieldInfo.create field_type_parameters in
				ClassFieldInfos.set class_field_infos info cf;
				None,None
			| _ ->
				die "" __LOC__
		in

		cf.cf_type <- t;
		cf.cf_doc <- doc;
		cf.cf_kind <- kind;
		cf.cf_expr <- expr;
		cf.cf_expr_unoptimized <- expr_unoptimized;
		cf.cf_params <- params;
		cf.cf_flags <- flags

	method read_class_field_and_overloads_data (cf : tclass_field) =
		let rec loop depth cfl = match cfl with
			| cf :: cfl ->
				assert (depth > 0);
				self#read_class_field_data cf;
				loop (depth - 1) cfl
			| [] ->
				assert (depth = 0)
		in
		loop (read_uleb128 ch) (cf :: cf.cf_overloads);

	method select_class_type_parameters (c: tclass) =
		match c.cl_kind with
		| KAbstractImpl a ->
			type_type_parameters <- Array.of_list a.a_params
		| _ ->
			type_type_parameters <- Array.of_list c.cl_params

	method read_class_fields (c : tclass) =
		self#select_class_type_parameters c;
		let _ = self#read_option (fun f ->
			let cf = Option.get c.cl_constructor in
			self#read_class_field_and_overloads_data cf
		) in
		let _ = self#read_option (fun f ->
			let cf = Option.get c.cl_init in
			self#read_class_field_and_overloads_data cf
		) in
		let rec loop ref_kind num cfl = match cfl with
			| cf :: cfl ->
				assert (num > 0);
				self#read_class_field_and_overloads_data cf;
				loop ref_kind (num - 1) cfl
			| [] ->
				assert (num = 0)
		in
		loop CfrMember (read_uleb128 ch) c.cl_ordered_fields;
		loop CfrStatic (read_uleb128 ch) c.cl_ordered_statics;
		(match c.cl_kind with KModuleFields md -> md.m_statics <- Some c; | _ -> ());

	method read_enum_fields (e : tenum) =
		type_type_parameters <- Array.of_list e.e_params;
		ignore(self#read_list (fun () ->
			let name = self#read_string in
			let ef = PMap.find name e.e_constrs in
			ef.ef_params <- self#read_field_type_parameters;
			ef.ef_type <- self#read_type_instance;
			ef.ef_doc <- self#read_option (fun () -> self#read_documentation);
			ef.ef_meta <- self#read_metadata;
		))

	(* Module types *)

	method read_common_module_type (infos : tinfos) =
		infos.mt_private <- self#read_bool;
		infos.mt_doc <- self#read_option (fun () -> self#read_documentation);
		infos.mt_meta <- self#read_metadata;
		let params = Array.of_list infos.mt_params in
		type_type_parameters <- params;
		self#read_type_parameters_data params;
		infos.mt_params <- Array.to_list type_type_parameters;
		infos.mt_using <- self#read_list (fun () ->
			let c = self#read_class_ref in
			let p = self#read_pos in
			(c,p)
		)

	method read_class_kind = match read_byte ch with
		| 0 -> KNormal
		| 1 -> die "" __LOC__
		| 2 -> KExpr self#read_expr
		| 3 -> KGeneric
		| 4 ->
			let c = self#read_class_ref in
			let tl = self#read_types in
			KGenericInstance(c,tl)
		| 5 -> KMacroType
		| 6 -> KGenericBuild (self#read_list (fun () -> self#read_cfield))
		| 7 -> KAbstractImpl self#read_abstract_ref
		| 8 -> KModuleFields current_module
		| i ->
			error (Printf.sprintf "Invalid class kind id: %i" i)

	method read_class (c : tclass) =
		self#read_common_module_type (Obj.magic c);
		c.cl_kind <- self#read_class_kind;
		let read_relation () =
			let c = self#read_class_ref in
			let tl = self#read_types in
			(c,tl)
		in
		c.cl_super <- self#read_option read_relation;
		c.cl_implements <- self#read_list read_relation;
		c.cl_dynamic <- self#read_option (fun () -> self#read_type_instance);
		c.cl_array_access <- self#read_option (fun () -> self#read_type_instance);

	method read_abstract (a : tabstract) =
		self#read_common_module_type (Obj.magic a);
		a.a_impl <- self#read_option (fun () -> self#read_class_ref);
		begin match read_byte ch with
			| 0 ->
				a.a_this <- TAbstract(a,extract_param_types a.a_params)
			| _ ->
				a.a_this <- self#read_type_instance;
		end;
		a.a_from <- self#read_list (fun () -> self#read_type_instance);
		a.a_to <- self#read_list (fun () -> self#read_type_instance);
		a.a_enum <- self#read_bool;

	method read_abstract_fields (a : tabstract) =
		a.a_array <- self#read_list (fun () -> self#read_field_ref);
		a.a_read <- self#read_option (fun () -> self#read_field_ref);
		a.a_write <- self#read_option (fun () -> self#read_field_ref);
		a.a_call <- self#read_option (fun () -> self#read_field_ref);

		a.a_ops <- self#read_list (fun () ->
			let i = read_byte ch in
			let op = self#get_binop i in
			let cf = self#read_field_ref in
			(op, cf)
		);

		a.a_unops <- self#read_list (fun () ->
			let i = read_byte ch in
			let (op, flag) = self#get_unop i in
			let cf = self#read_field_ref in
			(op, flag, cf)
		);

		a.a_from_field <- self#read_list (fun () ->
			let cf = self#read_field_ref in
			let t = match cf.cf_type with
				| TFun((_,_,t) :: _, _) -> t
				| _ -> die "" __LOC__
			in
			(t,cf)
		);

		a.a_to_field <- self#read_list (fun () ->
			let cf = self#read_field_ref in
			let t = match cf.cf_type with
				| TFun(_, t) -> t
				| _ -> die "" __LOC__
			in
			(t,cf)
		);

	method read_enum (e : tenum) =
		self#read_common_module_type (Obj.magic e);
		e.e_extern <- self#read_bool;
		e.e_names <- self#read_list (fun () -> self#read_string);

	method read_typedef (td : tdef) =
		self#read_common_module_type (Obj.magic td);
		let t = self#read_type_instance in
		match td.t_type with
		| TMono r ->
			(match r.tm_type with
			| None -> Monomorph.bind r t;
			| Some t' -> die (Printf.sprintf "typedef %s is already initialized to %s, but new init to %s was attempted" (s_type_path td.t_path) (s_type_kind t') (s_type_kind t)) __LOC__)
		| _ ->
			die "" __LOC__

	(* Chunks *)

	method read_string_pool =
		let l = read_uleb128 ch in
		Array.init l (fun i ->
			self#read_raw_string;
		);

	method read_efr =
		let l = read_uleb128 ch in
		let a = Array.init l (fun i ->
			let en = self#read_enum_ref in
			let name = self#read_string in
			PMap.find name en.e_constrs
		) in
		enum_fields <- a

	method read_ofr =
		let l = read_uleb128 ch in
		let a = Array.init l (fun _ -> self#read_class_field_forward) in
		anon_fields <- a

	method read_ofd =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let index = read_uleb128 ch in
			let cf = anon_fields.(index) in
			self#read_class_field_and_overloads_data cf;
		done

	method read_cfr =
		let l = read_uleb128 ch in
		let a = Array.init l (fun i ->
			let c = self#read_class_ref in
			let kind = match read_byte ch with
				| 0 -> CfrStatic
				| 1 -> CfrMember
				| 2 -> CfrConstructor
				| 3 -> CfrInit
				| _ -> die "" __LOC__
			in
			let cf =  match kind with
				| CfrStatic ->
					let name = self#read_string in
					begin try
						PMap.find name c.cl_statics
					with Not_found ->
						raise (HxbFailure (Printf.sprintf "Could not read static field %s on %s while hxbing %s" name (s_type_path c.cl_path) (s_type_path current_module.m_path)))
					end;
				| CfrMember ->
					let name = self#read_string in
					begin try
						PMap.find name c.cl_fields
					with Not_found ->
						raise (HxbFailure (Printf.sprintf "Could not read instance field %s on %s while hxbing %s" name (s_type_path c.cl_path) (s_type_path current_module.m_path)))
					end
				| CfrConstructor ->
					Option.get c.cl_constructor
				| CfrInit ->
					Option.get c.cl_init
			in
			let pick_overload cf depth =
				let rec loop depth cfl = match cfl with
					| cf :: cfl ->
						if depth = 0 then
							cf
						else
							loop (depth - 1) cfl
					| [] ->
						raise (HxbFailure (Printf.sprintf "Bad overload depth for %s on %s: %i" cf.cf_name (s_type_path c.cl_path) depth))
				in
				let cfl = cf :: cf.cf_overloads in
				loop depth cfl
			in
			let depth = read_uleb128 ch in
			if depth = 0 then
				cf
			else
				pick_overload cf depth;
		) in
		class_fields <- a

	method read_cfd =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let c = classes.(i) in
			self#read_class_fields c;
		done

	method read_exd =
		ignore(self#read_list (fun () ->
			let c = self#read_class_ref in
			self#read_list (fun () ->
				let cf = self#read_field_ref in
				let length = read_uleb128 ch in
				let bytes = read_bytes ch length in
				let ch_cf = BytesWithPosition.create bytes in
				let read_expressions () =
					self#select_class_type_parameters c;
					field_type_parameters <- (ClassFieldInfos.get class_field_infos cf).type_parameters;
					ClassFieldInfos.unset class_field_infos cf;
					field_type_parameter_offset <- 0;
					let old = ch in
					ch <- ch_cf;
					let fctx = self#start_texpr in
					let e,e_unopt = self#read_expression fctx in
					ch <- old;
					cf.cf_expr <- Some e;
					cf.cf_expr_unoptimized <- e_unopt;
				in
				if api#read_expression_eagerly cf then
					read_expressions ()
				else begin
					let t = cf.cf_type in
					let r = ref (lazy_available t) in
					r := lazy_wait (fun() ->
						cf.cf_type <- t;
						r := lazy_available t;
						read_expressions ();
						t
					);
					cf.cf_type <- TLazy r
				end
			)
		))

	method read_afd =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let a = abstracts.(i) in
			self#read_abstract_fields a;
		done

	method read_cld =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let c = classes.(i) in
			self#read_class c;
		done

	method read_abd =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let a = abstracts.(i) in
			self#read_abstract a;
		done

	method read_end =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let en = enums.(i) in
			self#read_enum en;
		done

	method read_efd =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let e = enums.(i) in
			self#read_enum_fields e;
			Type.unify (TType(enum_module_type e,[])) e.e_type
		done

	method read_anon an =
		let read_fields () =
			let rec loop acc i =
				if i = 0 then
					acc
				else begin
					let cf = self#read_anon_field_ref in
					loop (PMap.add cf.cf_name cf acc) (i - 1)
				end
			in
			an.a_fields <- loop PMap.empty (read_uleb128 ch)
		in

		begin match read_byte ch with
		| 0 ->
			an.a_status := Closed;
			read_fields ()
		| 1 ->
			an.a_status := Const;
			read_fields ()
		| 2 ->
			an.a_status := Extend self#read_types;
			read_fields ()
		| _ -> assert false
		end;

		an

	method read_tdd =
		let l = read_uleb128 ch in
		for i = 0 to l - 1 do
			let t = typedefs.(i) in
			self#read_typedef t;
		done

	method read_clr =
		let l = read_uleb128 ch in
		classes <- (Array.init l (fun i ->
				let (pack,mname,tname) = self#read_full_path in
				match self#resolve_type pack mname tname with
				| TClassDecl c ->
					c
				| _ ->
					error ("Unexpected type where class was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_abr =
		let l = read_uleb128 ch in
		abstracts <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			match self#resolve_type pack mname tname with
			| TAbstractDecl a ->
				a
			| _ ->
				error ("Unexpected type where abstract was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_enr =
		let l = read_uleb128 ch in
		enums <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			match self#resolve_type pack mname tname with
			| TEnumDecl en ->
				en
			| _ ->
				error ("Unexpected type where enum was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_tdr =
		let l = read_uleb128 ch in
		typedefs <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			match self#resolve_type pack mname tname with
			| TTypeDecl tpd ->
				tpd
			| _ ->
				error ("Unexpected type where typedef was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_mdr =
		let length = read_uleb128 ch in
		for _ = 0 to length - 1 do
			let path = self#read_path in
			ignore(api#resolve_module path)
		done

	method read_mtf =
		self#read_list (fun () ->
			let kind = read_byte ch in
			let path = self#read_path in
			let pos,name_pos = self#read_pos_pair in
			let params = self#read_type_parameters_forward in
			let mt = match kind with
			| 0 ->
				let c = mk_class current_module path pos name_pos in
				c.cl_params <- Array.to_list params;
				c.cl_flags <- read_uleb128 ch;

				let read_field () =
					self#read_class_field_forward;
				in

				c.cl_constructor <- self#read_option read_field;
				c.cl_init <- self#read_option read_field;
				let read_fields i =
					let rec loop acc_l acc_pm i =
						if i = 0 then
							acc_l,acc_pm
						else begin
							let cf = self#read_class_field_forward in
							loop (cf :: acc_l) (PMap.add cf.cf_name cf acc_pm) (i - 1)
						end
					in
					loop [] PMap.empty i
				in
				let num_fields = read_uleb128 ch in
				let num_statics = read_uleb128 ch in
				let l,pm = read_fields num_fields in
				c.cl_ordered_fields <- l;
				c.cl_fields <- pm;
				let l,pm = read_fields num_statics in
				c.cl_ordered_statics <- l;
				c.cl_statics <- pm;

				TClassDecl c
			| 1 ->
				let en = mk_enum current_module path pos name_pos in
				en.e_params <- Array.to_list params;

				let read_field () =
					let name = self#read_string in
					let pos,name_pos = self#read_pos_pair in
					let index = read_byte ch in

					{ null_enum_field with
						ef_name = name;
						ef_pos = pos;
						ef_name_pos = name_pos;
						ef_index = index;
					}
				in
				let rec loop acc i =
					if i = 0 then
						acc
					else begin
						let ef = read_field () in
						loop (PMap.add ef.ef_name ef acc) (i - 1)
					end
				in
				en.e_constrs <- loop PMap.empty (read_uleb128 ch);
				TEnumDecl en
			| 2 ->
				let td = mk_typedef current_module path pos name_pos (mk_mono()) in
				td.t_params <- Array.to_list params;
				typedefs <- Array.append typedefs (Array.make 1 td);
				TTypeDecl td
			| 3 ->
				let a = mk_abstract current_module path pos name_pos in
				a.a_params <- Array.to_list params;
				abstracts <- Array.append abstracts (Array.make 1 a);
				TAbstractDecl a
			| _ ->
				error ("Invalid type kind: " ^ (string_of_int kind));
			in
			mt
		)

	method read_mdf =
		let path = self#read_path in
		let file = self#read_string in

		let l = read_uleb128 ch in
		anons <- Array.init l (fun _ -> { a_fields = PMap.empty; a_status = ref Closed });
		tmonos <- Array.init (read_uleb128 ch) (fun _ -> mk_mono());
		api#make_module path file

	method private read_chunk_prefix =
		let name = Bytes.unsafe_to_string (read_bytes ch 3) in
		let size = Int32.to_int self#read_i32 in
		(name,size)

	method private read_chunk_data' (kind : chunk_kind) =
		match kind with
		| STR ->
			string_pool <- self#read_string_pool;
		| DOC ->
			doc_pool <- self#read_string_pool;
		| MDF ->
			current_module <- self#read_mdf;
		| MTF ->
			current_module.m_types <- self#read_mtf;
			api#add_module current_module;
		| MDR ->
			self#read_mdr;
		| CLR ->
			self#read_clr;
		| ENR ->
			self#read_enr;
		| ABR ->
			self#read_abr;
		| TDR ->
			self#read_tdr;
		| OFR ->
			self#read_ofr;
		| OFD ->
			self#read_ofd;
		| CLD ->
			self#read_cld;
		| END ->
			self#read_end;
		| ABD ->
			self#read_abd;
		| TDD ->
			self#read_tdd;
		| EOT ->
			()
		| EFR ->
			self#read_efr;
		| CFR ->
			self#read_cfr;
		| CFD ->
			self#read_cfd;
		| EFD ->
			self#read_efd;
		| AFD ->
			self#read_afd;
		| EOF ->
			()
		| EXD ->
			self#read_exd;
		| EOM ->
			incr stats.modules_fully_restored;

	method private read_chunk_data kind =
		let path = String.concat "_" (ExtLib.String.nsplit (s_type_path mpath) ".") in
		let id = ["hxb";"read";string_of_chunk_kind kind;path] in
		let close = Timer.timer id in
		self#read_chunk_data' kind;
		close()

	method read_chunks (new_api : hxb_reader_api) (chunks : cached_chunks) =
		fst (self#read_chunks_until new_api chunks EOM)

	method read_chunks_until (new_api : hxb_reader_api) (chunks : cached_chunks) end_chunk =
		api <- new_api;
		let rec loop = function
			| (kind,data) :: chunks ->
				ch <- BytesWithPosition.create data;
				self#read_chunk_data kind;
				if kind = end_chunk then chunks else loop chunks
			| [] -> die "" __LOC__
		in
		let remaining = loop chunks in
		(current_module, remaining)

	method read (new_api : hxb_reader_api) (bytes : bytes) =
		api <- new_api;
		ch <- BytesWithPosition.create bytes;
		if (Bytes.to_string (read_bytes ch 3)) <> "hxb" then
			raise (HxbFailure "magic");
		let version = read_byte ch in
		if version <> hxb_version then
			raise (HxbFailure (Printf.sprintf "version mismatch: hxb version %i, reader version %i" version hxb_version));
		(fun end_chunk ->
			let rec loop () =
				let (name,size) = self#read_chunk_prefix in
				let kind = chunk_kind_of_string name in
				self#read_chunk_data kind;
				if kind <> end_chunk then begin
					loop()
				end
			in
			loop();
			current_module
		)
end
