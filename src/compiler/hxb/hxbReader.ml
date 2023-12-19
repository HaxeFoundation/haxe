open Globals
open Ast
open Type
open HxbData

(* Debug utils *)
let no_color = false
let c_reset = if no_color then "" else "\x1b[0m"
let c_bold = if no_color then "" else "\x1b[1m"
let c_dim = if no_color then "" else "\x1b[2m"
let todo = "\x1b[33m[TODO]" ^ c_reset
let todo_error = "\x1b[31m[TODO] error:" ^ c_reset

let debug_msg msg =
	prerr_endline msg

let print_stacktrace () =
	let stack = Printexc.get_callstack 10 in
	let lines = Printf.sprintf "%s\n" (Printexc.raw_backtrace_to_string stack) in
	match (ExtString.String.split_on_char '\n' lines) with
		| (_ :: (_ :: lines)) -> prerr_endline (Printf.sprintf "%s" (ExtString.String.join "\n" lines))
		| _ -> die "" __LOC__

class hxb_reader
	(* (com : Common.context) *)
	(make_module : path -> string -> module_def)
	(add_module : module_def -> unit)
	(resolve_type : string -> string list -> string -> string -> module_type)
	(flush_fields : unit -> unit)
= object(self)

	val mutable m = null_module
	val mutable ch = IO.input_bytes Bytes.empty
	val mutable string_pool = Array.make 0 ""
	val mutable doc_pool = Array.make 0 ""

	val mutable classes = Array.make 0 null_class
	val mutable abstracts = Array.make 0 null_abstract
	val mutable enums = Array.make 0 null_enum
	val mutable typedefs = Array.make 0 null_typedef
	val mutable anons = Array.make 0 null_tanon
	val mutable anon_fields = Array.make 0 null_field

	val vars = Hashtbl.create 0
	val mutable type_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)
	val field_type_parameters = Hashtbl.create 0
	val mutable local_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)

	method resolve_type sign pack mname tname =
		try resolve_type sign pack mname tname with
		| Bad_module (path, reason) -> raise (Bad_module (m.m_path, DependencyDirty (path, reason)))
		| Not_found -> error (Printf.sprintf "Cannot resolve type %s" (s_type_path ((pack @ [mname]),tname)))

	val mutable tvoid = None
	method get_tvoid =
		match tvoid with
		| Some tvoid -> tvoid
		| None ->
				let t = type_of_module_type (self#resolve_type m.m_extra.m_sign [] "StdTypes" "Void") in
				tvoid <- Some t;
				t

	(* Primitives *)

	method read_u8 =
		IO.read_byte ch

	method read_u32 =
		IO.read_real_i32 ch

	method read_i16 =
		IO.read_i16 ch

	method read_f64 =
		IO.read_double ch

	method read_uleb128 =
		let b = self#read_u8 in
		if b >= 0x80 then
			(b land 0x7F) lor ((self#read_uleb128) lsl 7)
		else
			b

	method read_leb128 =
		let rec read acc shift =
			let b = self#read_u8 in
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

	method read_bool =
		self#read_u8 <> 0

	method read_from_string_pool pool =
		let l = self#read_uleb128 in
		try pool.(l) with e ->
			prerr_endline (Printf.sprintf "  Failed getting string #%d" l);
			raise e

	method read_string =
		self#read_from_string_pool string_pool

	method read_raw_string =
		let l = self#read_uleb128 in
		Bytes.unsafe_to_string (IO.nread ch l)

	(* Basic compounds *)

	method read_list : 'a . (unit -> 'a) -> 'a list = fun f ->
		let l = self#read_uleb128 in
		let a = Array.init l (fun _ -> f ()) in
		Array.to_list a

	method read_option : 'a . (unit -> 'a) -> 'a option = fun f ->
		match self#read_u8 with
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
		(* prerr_endline (Printf.sprintf "    Read full path %s" (ExtString.String.join "." (pack @ [mname; tname]))); *)
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
		let min = self#read_leb128 in
		let max = self#read_leb128 in
		let pos = {
			pfile = file;
			pmin = min;
			pmax = max;
		} in
		(* prerr_endline (Printf.sprintf "Read pos: %s" (Printer.s_pos pos)); *)
		(* MessageReporting.display_source_at com pos; *)
		pos

	method read_metadata_entry : metadata_entry =
		let name = self#read_string in
		let p = self#read_pos in
		let el = self#read_list (fun () -> self#read_expr) in
		(Meta.from_string name,el,p)

	method read_metadata =
		self#read_list (fun () -> self#read_metadata_entry)

	(* References *)

	method read_class_ref =
		let i = self#read_uleb128 in
		try classes.(i) with e ->
			prerr_endline (Printf.sprintf "[%s] %s reading class ref %i" (s_type_path m.m_path) todo_error i);
			raise e

	method read_abstract_ref =
		let i = self#read_uleb128 in
		try abstracts.(i) with e ->
			prerr_endline (Printf.sprintf "[%s] %s reading abstract ref %i" (s_type_path m.m_path) todo_error i);
			raise e

	method read_enum_ref =
		let i = self#read_uleb128 in
		try enums.(i) with e ->
			prerr_endline (Printf.sprintf "[%s] %s reading enum ref %i" (s_type_path m.m_path) todo_error i);
			raise e

	method read_typedef_ref =
		let i = self#read_uleb128 in
		try typedefs.(i) with e ->
			prerr_endline (Printf.sprintf "[%s] %s reading typedef ref %i" (s_type_path m.m_path) todo_error i);
			raise e

	method read_field_ref fields =
		let name = self#read_string in
		try PMap.find name fields with e ->
			trace (Printf.sprintf "[%s]  %s reading field %s" (s_type_path m.m_path) todo_error name);
			trace (Printf.sprintf "    Available fields: %s" (PMap.fold (fun f acc -> acc ^ " " ^ f.cf_name) fields ""));
			(* print_stacktrace (); *)
			(* null_field *)
			raise e

	method read_enum_field_ref en =
		let name = self#read_string in
		try PMap.find name en.e_constrs with e ->
			prerr_endline (Printf.sprintf "  %s reading enum field ref for %s.%s" todo_error (s_type_path en.e_path) name);
			prerr_endline (Printf.sprintf "    Available fields: %s" (PMap.fold (fun ef acc -> acc ^ " " ^ ef.ef_name) en.e_constrs ""));
			(* print_stacktrace (); *)
			(* null_enum_field *)
			raise e

	method read_anon_ref =
		match IO.read_byte ch with
		| 0 ->
			let index = self#read_uleb128 in
			(try anons.(index) with e ->
				trace (Printf.sprintf "[%s] %s reading anon (0) ref %i" (s_type_path m.m_path) todo_error index);
				raise e
			)
		| 1 ->
			let index = self#read_uleb128 in
			let an = (try anons.(index) with e ->
				trace (Printf.sprintf "[%s] %s reading anon (1) ref %i" (s_type_path m.m_path) todo_error index);
				trace (Printexc.to_string e);
				raise e
			) in
			self#read_anon an
		| _ ->
			assert false

	method read_anon_field_ref =
		match IO.read_byte ch with
		| 0 ->
			let index = self#read_uleb128 in
			(try anon_fields.(index) with e ->
				prerr_endline (Printf.sprintf "[%s] %s reading anon field (0) ref %i" (s_type_path m.m_path) todo_error index);
				raise e
			)
		| 1 ->
			let index = self#read_uleb128 in
			(try begin
				let cf = self#read_class_field true in
				anon_fields.(index) <- cf;
				cf
			end with e ->
				prerr_endline (Printf.sprintf "[%s] %s reading anon field (1) ref %i" (s_type_path m.m_path) todo_error index);
				raise e
			)
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
		let pfull = self#read_pos in
		let ppath = self#read_pos in
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
		match IO.read_byte ch with
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
		match IO.read_byte ch with
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
		match self#read_u8 with
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
		match self#read_u8 with
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
		let e = match self#read_u8 with
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
			let qs = begin match self#read_u8 with
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
			let op = self#get_binop (self#read_u8) in
			let e1 = self#read_expr in
			let e2 = self#read_expr in
			EBinop(op,e1,e2)
		| 7 ->
			let e = self#read_expr in
			let s = self#read_string in
			let kind = begin match self#read_u8 with
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
				let qs = begin match self#read_u8 with
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
			let (op,flag) = self#get_unop (self#read_u8) in
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
			let fk = begin match self#read_u8 with
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
			let dk = begin match self#read_u8 with
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

	method read_type_parameter_ref = function
		| 5 ->
			let p = self#read_path in
			(try
				let ttp = Hashtbl.find field_type_parameters p in
				(match follow ttp.ttp_type with
					| TInst(c, _) ->
						if c.cl_path <> p then begin
							Printf.eprintf "Error loading ftp: %s <> %s\n" (s_type_path c.cl_path) (s_type_path p);
							die "" __LOC__
						end
					| _ -> die "" __LOC__
				);
				ttp.ttp_type
			with _ ->
				Printf.eprintf "Error loading ftp for %s\n" (s_type_path p);
				die "" __LOC__
			)
		| 6 ->
			let i = self#read_uleb128 in
			(type_type_parameters.(i)).ttp_type
		| 7 ->
			let k = self#read_uleb128 in
			local_type_parameters.(k).ttp_type
		| _ ->
			die "" __LOC__

	method read_type_instance =
		let kind = self#read_u8 in
		(* prerr_endline (Printf.sprintf "   Read type instance %d" kind); *)

		match kind with
		| 0 ->
			(* prerr_endline (Printf.sprintf "  %s identity" todo); *)
			mk_mono() (* TODO: identity *)
		| 1 ->
			(* prerr_endline (Printf.sprintf "  %s TMono Some" todo); *)
			let t = self#read_type_instance in
			let tmono = !monomorph_create_ref () in (* TODO identity *)
			tmono.tm_type <- Some t;
			TMono tmono;
		| 5 | 6 | 7 -> self#read_type_parameter_ref kind
		| 8 ->
			let e = self#read_expr in
			TInst({null_class with cl_kind = KExpr e}, [])
		| 10 ->
			TInst(self#read_class_ref,[])
		| 11 ->
			TEnum(self#read_enum_ref,[])
		| 12 ->
			let tp = self#read_path in
			begin match self#read_u8 with
				| 0 -> TType({null_typedef with t_type = (mk_anon (ref Closed)); t_path = tp },[])
				| 1 -> TType({null_typedef with t_type = (TAnon self#read_anon_ref); t_path = tp },[])
				| 4 ->
					let c = self#read_class_ref in
					let t_tmp = class_module_type c in
					TType(t_tmp,[])
				| 5 ->
					let e = self#read_enum_ref in
					let t_tmp = enum_module_type e.e_module e.e_path e.e_pos in
					TType(t_tmp,[])
				| 6 ->
					let a = self#read_abstract_ref in
					let t_tmp = abstract_module_type a [] in
					TType(t_tmp,[])
				| _ ->
					TType(self#read_typedef_ref,[])
			end
		| 13 ->
			TAbstract(self#read_abstract_ref,[])
		| 14 ->
			let c = self#read_class_ref in
			let tl = self#read_types in
			TInst(c,tl)
		| 15 ->
			let e = self#read_enum_ref in
			let tl = self#read_types in
			TEnum(e,tl)
		| 16 ->
			let tp = self#read_path in
			begin match self#read_u8 with
				| 0 ->
					let an = mk_anon (ref Closed) in
					let tl = self#read_types in
					let td = { null_typedef with t_type = an; t_path = tp } in
					TType(td,tl)
				| 1 ->
					let an = TAnon self#read_anon_ref in
					let tl = self#read_types in
					let td = { null_typedef with t_type = an; t_path = tp } in
					TType(td,tl)
				| 4 ->
					let c = self#read_class_ref in
					let t_tmp = class_module_type c in
					TType(t_tmp,[])
				| 5 ->
					let e = self#read_enum_ref in
					let t_tmp = enum_module_type e.e_module e.e_path e.e_pos in
					TType(t_tmp,[])
				| 6 ->
					let a = self#read_abstract_ref in
					let t_tmp = abstract_module_type a [] in
					TType(t_tmp,[])
				(* TODO: does this help with anything? *)
				(* | 2 -> *)
				(* 	let t = self#read_type_instance in *)
				(* 	let tl = self#read_types in *)
				(* 	let tmono = !monomorph_create_ref () in (1* TODO identity *1) *)
				(* 	tmono.tm_type <- Some t; *)
				(* 	let td = { null_typedef with t_type = TMono tmono; t_path = tp } in *)
				(* 	TType(td,tl) *)
				| _ ->
					(* let t = self#read_typedef_ref in *)
					let t = self#read_type_instance in
					let tl = self#read_types in
					let td = { null_typedef with t_type = t; t_path = tp } in
					(* let td = { null_typedef with t_type = t; t_path = ([], "708") } in *)
					TType(td,tl)
					(* TType(t,tl) *)
			end
		| 17 ->
			let a = self#read_abstract_ref in
			let tl = self#read_types in
			TAbstract(a,tl)
		| 30 -> TFun([],self#get_tvoid)
		| 31 ->
			let f () =
				let name = self#read_string in
				(* prerr_endline (Printf.sprintf "  Read type instance for %s" name); *)
				let opt = self#read_bool in
				let t = self#read_type_instance in
				(name,opt,t)
			in
			let args = self#read_list f in
			TFun(args,self#get_tvoid)
		| 32 ->
			let f () =
				let name = self#read_string in
				(* prerr_endline (Printf.sprintf "  Read type instance for %s" name); *)
				let opt = self#read_bool in
				let t = self#read_type_instance in
				(name,opt,t)
			in
			let args = self#read_list f in
			(* prerr_endline (Printf.sprintf "  Read type instance for TFun"); *)
			let ret = self#read_type_instance in
			TFun(args,ret)
		| 33 ->
			let t = self#read_type_instance in
			(* TLazy (ref (LAvailable t)) *)
			t
		| 40 ->
			t_dynamic
		| 41 ->
			TDynamic (Some self#read_type_instance)
		| 50 ->
			let empty = self#read_bool in
			if empty then mk_anon (ref Closed)
			else TAnon self#read_anon_ref
		| 51 -> TAnon self#read_anon_ref
		| i -> error (Printf.sprintf "Bad type instance id: %i" i)

	method read_types =
		self#read_list (fun () -> self#read_type_instance)

	(* Fields *)

	method add_field_type_parameters a = Array.iter (fun ttp ->
			(match follow ttp.ttp_type with
				| TInst(c,_) -> Hashtbl.add field_type_parameters c.cl_path ttp
				| _ -> die "" __LOC__
			)
		) a

	method read_type_parameters (path : path) (f : typed_type_param array -> unit) =
		let l = self#read_uleb128 in
		let a = Array.init l (fun _ ->
			let name = self#read_string in
			let pos = self#read_pos in
			(* prerr_endline (Printf.sprintf "      Read ttp pos for %s: %s" name (Printer.s_pos pos)); *)
			(* prerr_endline (Printf.sprintf "      - Path was %s" (s_type_path path)); *)
			let c = mk_class m (fst path @ [snd path],name) pos pos in
			mk_type_param name (TInst(c,[])) None
		) in
		f a;
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let tl1 = self#read_types in
			let tl2 = self#read_types in
			let meta = self#read_metadata in
			begin match a.(i) with
			| {ttp_type = TInst(c,_)} as ttp ->
				c.cl_kind <- KTypeParameter tl1;
				c.cl_meta <- meta;
				a.(i) <- {ttp with ttp_type = (TInst(c,tl2))}
			| _ ->
				die "" __LOC__
			end;
		done;

	method read_field_kind = match self#read_u8 with
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
			let r = f self#read_u8 in
			let w = f self#read_u8 in
			Var {v_read = r;v_write = w}
		| i ->
			error (Printf.sprintf "Bad field kind: %i" i)

	method read_tfunction_arg =
		let v = self#read_var in
		let cto = self#read_option (fun () -> self#read_texpr) in
		(v,cto)

	method read_tfunction =
		let args = self#read_list (fun () -> self#read_tfunction_arg) in
		let r = self#read_type_instance in
		let e = self#read_texpr in
		{
			tf_args = args;
			tf_type = r;
			tf_expr = e;
		}

	method read_var_kind =
		match IO.read_byte ch with
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
		let id = IO.read_i32 ch in
		let name = self#read_string in
		let extra = self#read_option (fun () ->
			let params = self#read_list (fun () ->
				let i = self#read_uleb128 in
				local_type_parameters.(i)
			) in
			let vexpr = self#read_option (fun () -> self#read_texpr) in
			{
				v_params = params;
				v_expr = vexpr;
			};
		) in
		let t = self#read_type_instance in
		let kind = self#read_var_kind in
		let flags = IO.read_i32 ch in
		let meta = self#read_metadata in
		let pos = self#read_pos in
		let v = {
			v_id = id;
			v_name = name;
			v_type = t;
			v_kind = kind;
			v_meta = meta;
			v_pos = pos;
			v_extra = extra;
			v_flags = flags;
		} in
		Hashtbl.add vars id v;
		v

	method read_texpr =
		let t = self#read_type_instance in
		let pos = self#read_pos in

		let i = IO.read_byte ch in
		(* prerr_endline (Printf.sprintf "      -- texpr [%d] --" i); *)
		let e = match i with
			(* values 0-19 *)
			| 0 -> TConst TNull
			| 1 -> TConst TThis
			| 2 -> TConst TSuper
			| 3 -> TConst (TBool false)
			| 4 -> TConst (TBool true)
			| 5 -> TConst (TInt (IO.read_real_i32 ch))
			| 6 -> TConst (TFloat self#read_string)
			| 7 -> TConst (TString self#read_string)

			(* vars 20-29 *)
			| 20 -> TLocal (Hashtbl.find vars (IO.read_i32 ch))
			| 21 ->
				let v = self#read_var in
				TVar (v,None)
			| 22 ->
					let v = self#read_var in
					let e = self#read_texpr in
					TVar (v, Some e)

			(* blocks 30-49 *)
			| 30 -> TBlock []
			| 31 | 32 | 33 | 34 | 35 ->
				let l = i - 30 in
				let el = List.init l (fun _ -> self#read_texpr) in
				TBlock el;
			| 36 ->
				let l = IO.read_byte ch in
				let el = List.init l (fun _ -> self#read_texpr) in
				TBlock el;
			| 37 ->
				let l = IO.read_ui16 ch in
				let el = List.init l (fun _ -> self#read_texpr) in
				TBlock el;
			| 38 ->
				let l = IO.read_i32 ch in
				let el = List.init l (fun _ -> self#read_texpr) in
				TBlock el;

			(* function 50-59 *)
			| 50 -> TFunction self#read_tfunction

			(* texpr compounds 60-79 *)
			| 60 ->
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TArray (e1,e2)
			| 61 -> TParenthesis self#read_texpr
			| 62 -> TArrayDecl self#read_texpr_list
			| 63 ->
				let fl = self#read_list (fun () ->
					let name = self#read_string in
					let p = self#read_pos in
					let qs = match IO.read_byte ch with
						| 0 -> NoQuotes
						| 1 -> DoubleQuotes
						| _ -> assert false
					in
					let e = self#read_texpr in
					((name,p,qs),e)
				) in
				TObjectDecl fl
			| 64 ->
				let e1 = self#read_texpr in
				let el = self#read_texpr_list in
				TCall(e1,el)
			| 65 ->
				let m = self#read_metadata_entry in
				let e1 = self#read_texpr in
				TMeta (m,e1)

			(* branching 80-89 *)
			| 80 ->
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TIf(e1,e2,None)
			| 81 ->
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				let e3 = self#read_texpr in
				TIf(e1,e2,Some e3)
			| 82 ->
				let subject = self#read_texpr in
				let cases = self#read_list (fun () ->
					let patterns = self#read_texpr_list in
					let ec = self#read_texpr in
					{ case_patterns = patterns; case_expr = ec}
				) in
				let def = self#read_option (fun () -> self#read_texpr) in
				TSwitch {
					switch_subject = subject;
					switch_cases = cases;
					switch_default = def;
					switch_exhaustive = true;
				}
			| 83 ->
				let e1 = self#read_texpr in
				let catches = self#read_list (fun () ->
					let v = self#read_var in
					let e = self#read_texpr in
					(v,e)
				) in
				TTry(e1,catches)
			| 84 ->
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TWhile(e1,e2,NormalWhile)
			| 85 ->
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TWhile(e1,e2,DoWhile)
			| 86 ->
				let v  = self#read_var in
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TFor(v,e1,e2)

			(* control flow 90-99 *)
			| 90 -> TReturn None
			| 91 -> TReturn (Some self#read_texpr)
			| 92 -> TContinue
			| 93 -> TBreak
			| 94 -> TThrow (self#read_texpr)

			(* access 100-119 *)
			| 100 -> TEnumIndex (self#read_texpr)
			| 101 ->
				let e1 = self#read_texpr in
				let en = self#read_enum_ref in
				let ef = self#read_enum_field_ref en in
				let i = IO.read_i32 ch in
				TEnumParameter(e1,ef,i)
			| 102 ->
				let e1 = self#read_texpr in
				let c = self#read_class_ref in
				let tl = self#read_types in
				let cf = self#read_field_ref c.cl_fields in
				TField(e1,FInstance(c,tl,cf))
			| 103 ->
				let e1 = self#read_texpr in
				let c = self#read_class_ref in
				let cf = self#read_field_ref c.cl_statics in
				TField(e1,FStatic(c,cf))
			| 104 ->
				let e1 = self#read_texpr in
				let cf = self#read_anon_field_ref in
				TField(e1,FAnon(cf))
			| 105 ->
				let e1 = self#read_texpr in
				let c = self#read_class_ref in
				let tl = self#read_types in
				let cf = self#read_field_ref c.cl_fields in
				TField(e1,FClosure(Some(c,tl),cf))
			| 106 ->
				let e1 = self#read_texpr in
				let cf = self#read_anon_field_ref in
				TField(e1,FClosure(None,cf))
			| 107 ->
				let e1 = self#read_texpr in
				let en = self#read_enum_ref in
				let ef = self#read_enum_field_ref en in
				let params = ref [] in
				self#read_type_parameters ([],ef.ef_name) (fun a -> params := Array.to_list a);
				ef.ef_params <- !params;
				TField(e1,FEnum(en,ef))
			| 108 ->
				let e1 = self#read_texpr in
				let s = self#read_string in
				TField(e1,FDynamic s)

			(* module types 120-139 *)
			| 120 -> TTypeExpr (TClassDecl self#read_class_ref)
			| 121 -> TTypeExpr (TEnumDecl self#read_enum_ref)
			| 122 -> TTypeExpr (TAbstractDecl self#read_abstract_ref)
			| 123 -> TTypeExpr (TTypeDecl self#read_typedef_ref)
			| 124 -> TCast(self#read_texpr,None)
			| 125 ->
				let e1 = self#read_texpr in
				let (pack,mname,tname) = self#read_full_path in
				let sign = self#read_string in
				let md = self#resolve_type sign pack mname tname in
				TCast(e1,Some md)
			| 126 ->
				let c = self#read_class_ref in
				let tl = self#read_types in
				let el = self#read_texpr_list in
				TNew(c,tl,el)
			| 127 ->
				(* TODO: this is giga awkward *)
				let t = self#read_type_parameter_ref self#read_uleb128 in
				let c = match t with | TInst(c,_) -> c | _ -> die "" __LOC__ in
				let tl = self#read_types in
				let el = self#read_texpr_list in
				TNew(c,tl,el)
			| 128 ->
				(* TODO: this is giga awkward *)
				let t = self#read_type_parameter_ref self#read_uleb128 in
				let c = match t with | TInst(c,_) -> c | _ -> die "" __LOC__ in
				TTypeExpr (TClassDecl c)

			(* unops 140-159 *)
			| _ when i >= 140 && i < 160 ->
				let (op,flag) = self#get_unop (i - 140) in
				let e = self#read_texpr in
				TUnop(op,flag,e)

			(* binops 160-219 *)
			| _ when i >= 160 && i < 220 ->
				let op = self#get_binop (i - 160) in
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TBinop(op,e1,e2)

			(* rest 250-254 *)
			| 250 -> TIdent (self#read_string)

			| i ->
				prerr_endline (Printf.sprintf "  [ERROR] Unhandled texpr %d at:" i);
				(* MessageReporting.display_source_at com pos; *)
				assert false
		in

		(* prerr_endline (Printf.sprintf "   Done reading texpr at:"); *)
		(* MessageReporting.display_source_at com pos; *)

		let e = {
			eexpr = e;
			etype = t;
			epos = pos;
		} in

		if (Printer.s_pos e.epos = "src/alchimix/core/GameSolver.hx: 5047-5070") then begin
			trace (Printer.s_pos e.epos);
			trace (s_expr_debug e);
		end;

		e

	method read_texpr_list =
		let len = IO.read_ui16 ch in
		List.init len (fun _ -> self#read_texpr);

	method read_class_field_forward =
		let name = self#read_string in
		let pos = self#read_pos in
		let name_pos = self#read_pos in
		let overloads = self#read_list (fun () -> self#read_class_field_forward) in
		{ null_field with cf_name = name; cf_pos = pos; cf_name_pos = name_pos; cf_overloads = overloads }

	method read_class_field_data (nested : bool) (cf : tclass_field) : unit =
		let name = cf.cf_name in
		(* prerr_endline (Printf.sprintf "  Read class field %s" name); *)

		if not nested then Hashtbl.clear field_type_parameters;
		let params = ref [] in
		self#read_type_parameters ([],name) (fun a ->
			Array.iter (fun ttp ->
				params := ttp :: !params;
				(match follow ttp.ttp_type with
					| TInst(c,_) -> Hashtbl.add field_type_parameters c.cl_path ttp
					| _ -> die "" __LOC__
				)
			) a
		);
		self#read_type_parameters ([],name) (fun a ->
			local_type_parameters <- if nested then Array.append local_type_parameters a else a
		);
		let t = self#read_type_instance in

		let flags = IO.read_i32 ch in

		let doc = self#read_option (fun () -> self#read_documentation) in
		let meta = self#read_metadata in
		let kind = self#read_field_kind in

		let expr = try
			self#read_option (fun () -> self#read_texpr)
		with e ->
			prerr_endline (Printf.sprintf "Error reading field expr for %s" cf.cf_name);
			raise e
		in
		let expr_unoptimized = self#read_option (fun () -> self#read_texpr) in

		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let f = List.nth cf.cf_overloads i in
			self#read_class_field_data true f
		done;

		cf.cf_type <- t;
		cf.cf_doc <- doc;
		cf.cf_meta <- meta;
		cf.cf_kind <- kind;
		(* ignore(expr); *)
		(* ignore(expr_unoptimized); *)
		cf.cf_expr <- expr;
		(* cf.cf_expr <- expr_unoptimized; *)
		cf.cf_expr_unoptimized <- expr_unoptimized;
		cf.cf_params <- !params;
		cf.cf_flags <- flags;

	method read_class_field (nested : bool) =
		let cf = self#read_class_field_forward in
		self#read_class_field_data nested cf;
		cf

	method read_class_fields (c : tclass) =
		begin match c.cl_kind with
		| KAbstractImpl a ->
			type_type_parameters <- Array.of_list a.a_params
		| _ ->
			type_type_parameters <- Array.of_list c.cl_params
		end;
		(* prerr_endline (Printf.sprintf "  read class fields with type parameters for %s: %d" (s_type_path c.cl_path) (Array.length type_type_parameters); *)
		(* prerr_endline (Printf.sprintf "    own class params: %d" (List.length c.cl_params); *)
		let _ = self#read_option (fun f ->
			self#read_class_field_data false (Option.get c.cl_constructor)
		) in
		let f fields =
			let name = self#read_string in
			let cf = PMap.find name fields in
			self#read_class_field_data false cf
		in
		let _ = self#read_list (fun () -> f c.cl_fields) in
		let _ = self#read_list (fun () -> f c.cl_statics) in
		c.cl_init <- self#read_option (fun () -> self#read_texpr);
		(match c.cl_kind with KModuleFields md -> md.m_statics <- Some c; | _ -> ());

	method read_enum_fields (e : tenum) =
		type_type_parameters <- Array.of_list e.e_params;
		ignore(self#read_list (fun () ->
			let name = self#read_string in
			(* prerr_endline (Printf.sprintf "  Read enum field %s" name); *)
			let ef = PMap.find name e.e_constrs in
			let params = ref [] in
			self#read_type_parameters ([],name) (fun a ->
				Array.iter (fun ttp ->
					params := ttp :: !params;
					(match follow ttp.ttp_type with
						| TInst(c,_) -> Hashtbl.add field_type_parameters c.cl_path ttp
						| _ -> die "" __LOC__
					)
				) a
			);
			ef.ef_params <- !params;
			ef.ef_type <- self#read_type_instance;
			ef.ef_doc <- self#read_option (fun () -> self#read_documentation);
			ef.ef_meta <- self#read_metadata;
		))

	(* Module types *)

	method read_common_module_type (infos : tinfos) =
		(* if (snd m.m_path) = "Issue9149" then *)
		(* prerr_endline (Printf.sprintf "[%s] Read module type %s" (s_type_path m.m_path) (s_type_path infos.mt_path)); *)
		infos.mt_private <- self#read_bool;
		infos.mt_doc <- self#read_option (fun () -> self#read_documentation);
		infos.mt_meta <- self#read_metadata;
		self#read_type_parameters infos.mt_path (fun a -> type_type_parameters <- a);
		infos.mt_params <- Array.to_list type_type_parameters;
		infos.mt_using <- self#read_list (fun () ->
			let c = self#read_class_ref in
			let p = self#read_pos in
			(c,p)
		)

	method read_class_kind = match self#read_u8 with
		| 0 -> KNormal
		| 1 -> KTypeParameter self#read_types
		| 2 -> KExpr self#read_expr
		| 3 -> KGeneric
		| 4 ->
			let c = self#read_class_ref in
			let tl = self#read_types in
			KGenericInstance(c,tl)
		| 5 -> KMacroType
		| 6 -> KGenericBuild (self#read_list (fun () -> self#read_cfield))
		| 7 -> KAbstractImpl self#read_abstract_ref
		| 8 -> KModuleFields m
		| i ->
			error (Printf.sprintf "Invalid class kind id: %i" i)

	method read_class (c : tclass) =
		self#read_common_module_type (Obj.magic c);
		c.cl_kind <- self#read_class_kind;
		c.cl_flags <- (Int32.to_int self#read_u32);
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
		let impl = match a.a_impl with None -> null_class | Some c -> c in
		a.a_this <- self#read_type_instance;
		a.a_from <- self#read_list (fun () -> self#read_type_instance);
		a.a_from_field <- self#read_list (fun () ->
			let name = self#read_string in
			self#read_type_parameters ([],name) self#add_field_type_parameters;
			let t = self#read_type_instance in
			let cf = self#read_field_ref impl.cl_statics in
			(t,cf)
		);
		a.a_to <- self#read_list (fun () -> self#read_type_instance);
		a.a_to_field <- self#read_list (fun () ->
			let name = self#read_string in
			self#read_type_parameters ([],name) self#add_field_type_parameters;
			let t = self#read_type_instance in
			let cf = self#read_field_ref impl.cl_statics in
			(t,cf)
		);

		a.a_array <- self#read_list (fun () -> self#read_field_ref impl.cl_statics);
		a.a_read <- self#read_option (fun () -> self#read_field_ref impl.cl_statics);
		a.a_write <- self#read_option (fun () -> self#read_field_ref impl.cl_statics);
		a.a_call <- self#read_option (fun () -> self#read_field_ref impl.cl_statics);
		a.a_enum <- self#read_bool;

	method read_enum (e : tenum) =
		self#read_common_module_type (Obj.magic e);
		(match self#read_u8 with
		| 0 -> e.e_type.t_type <- (mk_anon (ref Closed))
		| _ -> e.e_type.t_type <- TAnon self#read_anon_ref);
		e.e_extern <- self#read_bool;
		e.e_names <- self#read_list (fun () -> self#read_string);

	method read_typedef (td : tdef) =
		self#read_common_module_type (Obj.magic td);
		td.t_type <- self#read_type_instance;

	(* Chunks *)

	method read_string_pool =
		let l = self#read_uleb128 in
		(* prerr_endline (Printf.sprintf "  Read string pool of size %d" l); *)
		Array.init l (fun i ->
			self#read_raw_string;
		);

	method read_chunk =
		let size = Int32.to_int self#read_u32 in
		let name = Bytes.unsafe_to_string (IO.nread ch 4) in
		let data = IO.nread ch size in
		let crc = self#read_u32 in
		ignore(crc); (* TODO *)
		(* prerr_endline (Printf.sprintf "%s check crc (%d)" todo (Int32.to_int crc)); *)
		let kind = chunk_kind_of_string name in
		(kind,data)

	method read_cfld =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let c = classes.(i) in
			self#read_class_fields c;
		done

	method read_clsd =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let c = classes.(i) in
			self#read_class c;
		done

	method read_absd =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let a = abstracts.(i) in
			self#read_abstract a;
		done

	method read_enmd =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let en = enums.(i) in
			self#read_enum en;
		done

	method read_efld =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let e = enums.(i) in
			self#read_enum_fields e;
		done

	method read_anon an =
		let old = type_type_parameters in
		self#read_type_parameters ([],"") (fun a -> type_type_parameters <- Array.append type_type_parameters a);
		let read_fields () =
			let fields = self#read_list (fun () ->
				let cf = self#read_class_field_forward in
				self#read_class_field_data true cf;
				cf
			) in
			List.iter (fun cf -> an.a_fields <- PMap.add cf.cf_name cf an.a_fields) fields;
		in

		begin match self#read_u8 with
		| 0 ->
			an.a_status := Closed;
			read_fields ()
		| 1 ->
			an.a_status := Const;
			read_fields ()
		| 2 ->
			an.a_status := Extend self#read_types;
			read_fields ()
		| 3 ->
			an.a_status := ClassStatics self#read_class_ref;
		| 4 ->
			an.a_status := EnumStatics self#read_enum_ref;
			read_fields ()
		| 5 ->
			an.a_status := AbstractStatics self#read_abstract_ref;
			read_fields ()
		| _ -> assert false
		end;

		type_type_parameters <- old;
		an

	method read_tpdd =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let t = typedefs.(i) in
			self#read_typedef t;
		done

	method read_clsr =
		let l = self#read_uleb128 in
		classes <- (Array.init l (fun i ->
				let (pack,mname,tname) = self#read_full_path in
				let sign = self#read_string in
				match self#resolve_type sign pack mname tname with
				| TClassDecl c ->
					c
				| _ ->
					error ("Unexpected type where class was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_absr =
		let l = self#read_uleb128 in
		abstracts <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			let sign = self#read_string in
			match self#resolve_type sign pack mname tname with
			| TAbstractDecl a ->
				a
			| _ ->
				error ("Unexpected type where abstract was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_enmr =
		let l = self#read_uleb128 in
		enums <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			let sign = self#read_string in
			match self#resolve_type sign pack mname tname with
			| TEnumDecl en ->
				en
			| _ ->
				error ("Unexpected type where enum was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_tpdr =
		let l = self#read_uleb128 in
		typedefs <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			let sign = self#read_string in
			match self#resolve_type sign pack mname tname with
			| TTypeDecl tpd ->
				tpd
			| _ ->
				error ("Unexpected type where typedef was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_typf =
		self#read_list (fun () ->
			let kind = self#read_u8 in
			(* let path = self#read_path in *)
			let (pack,_,tname) = self#read_full_path in
			let path = (pack, tname) in
			let pos = self#read_pos in
			let name_pos = self#read_pos in
			let mt = match kind with
			| 0 ->
				let c = mk_class m path pos name_pos in
				classes <- Array.append classes (Array.make 1 c);

				let read_field () =
					self#read_class_field_forward;
				in

				c.cl_constructor <- self#read_option read_field;
				c.cl_ordered_fields <- self#read_list read_field;
				c.cl_ordered_statics <- self#read_list read_field;
				(* prerr_endline (Printf.sprintf "  Forward declare %s with %d fields, %d statics\n" (s_type_path path) (List.length c.cl_ordered_fields) (List.length c.cl_ordered_statics)); *)
				List.iter (fun cf -> c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields) c.cl_ordered_fields;
				List.iter (fun cf -> c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics) c.cl_ordered_statics;

				TClassDecl c
			| 1 ->
				let en = mk_enum m path pos name_pos in
				enums <- Array.append enums (Array.make 1 en);

				let read_field () =
					let name = self#read_string in
					let pos = self#read_pos in
					let name_pos = self#read_pos in
					let index = self#read_u8 in

					{ null_enum_field with
						ef_name = name;
						ef_pos = pos;
						ef_name_pos = name_pos;
						ef_index = index;
					}
				in

				List.iter (fun ef -> en.e_constrs <- PMap.add ef.ef_name ef en.e_constrs) (self#read_list read_field);
				TEnumDecl en
			| 2 ->
				let td = mk_typedef m path pos name_pos (mk_mono()) in
				typedefs <- Array.append typedefs (Array.make 1 td);
				TTypeDecl td
			| 3 ->
				let a = mk_abstract m path pos name_pos in
				abstracts <- Array.append abstracts (Array.make 1 a);
				TAbstractDecl a
			| _ ->
				error ("Invalid type kind: " ^ (string_of_int kind));
			in
			mt
		)

	method read_hhdr =
		let path = self#read_path in
		let file = self#read_string in
		(* prerr_endline (Printf.sprintf "Read hxb module %s" (s_type_path path)); *)

		let l = self#read_uleb128 in
		(* trace (Printf.sprintf "%d anons available" l); *)
		anons <- Array.init l (fun _ -> { a_fields = PMap.empty; a_status = ref Closed });

		anon_fields <- Array.make (self#read_uleb128) null_field;
		make_module path file

	method read (file_ch : IO.input) (debug : bool) (p : pos) =
		(* TODO: add magic & version to writer! *)
		(* if (Bytes.to_string (IO.nread ch 3)) <> "hxb" then *)
		(* 	raise (HxbFailure "magic"); *)
		(* let version = self#read_u8 in *)
		(* ignore(version); *)
		let rec loop acc =
			ch <- file_ch;
			let chunk = self#read_chunk in
			match fst chunk with
			| HEND ->
				List.rev acc
			| _ ->
				loop (chunk :: acc)
		in
		let chunks = loop [] in
		let chunks = List.sort (fun (kind1,_) (kind2,_) ->
			(Obj.magic kind1) - (Obj.magic kind2)
		) chunks in
		let rec pass_0 chunks = match chunks with
			| [] ->
				error "Missing HHDR chunk"
			| (kind,data) :: chunks ->
				ch <- IO.input_bytes data;
				match kind with
				| STRI ->
					string_pool <- self#read_string_pool;
					pass_0 chunks
				| DOCS ->
					doc_pool <- self#read_string_pool;
					pass_0 chunks
				| HHDR ->
					m <- self#read_hhdr;
					chunks
				| _ ->
					error ("Unexpected early chunk: " ^ (string_of_chunk_kind kind))
		in
		let chunks = pass_0 chunks in
		assert(m != null_module);
		(* trace (Printf.sprintf " Reading module %s from hxb" (s_type_path m.m_path)); *)
		List.iter (fun (kind,data) ->
			(* prerr_endline (Printf.sprintf " Reading chunk %s" (string_of_chunk_kind kind)); *)
			ch <- IO.input_bytes data;
			match kind with
			| TYPF ->
				m.m_types <- self#read_typf;
				add_module m;
			| CLSR ->
				self#read_clsr;
			| ABSR ->
				self#read_absr;
			| TPDR ->
				self#read_tpdr;
			| ENMR ->
				self#read_enmr;
			| CLSD ->
				self#read_clsd;
			| ABSD ->
				self#read_absd;
			| CFLD ->
				flush_fields ();
				self#read_cfld;
			| TPDD ->
				self#read_tpdd;
			| ENMD ->
				self#read_enmd;
			| EFLD ->
				self#read_efld;
			| _ ->
				error ("Unexpected late chunk: " ^ (string_of_chunk_kind kind))
		) chunks;
		(* prerr_endline (Printf.sprintf "Done reading hxb module %s" (s_type_path m.m_path)); *)
		m
end
