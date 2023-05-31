open Globals
open Ast
open Type
open HxbData

class hxb_reader
	(file_ch : IO.input)
	(make_module : path -> string -> module_def)
	(add_module : module_def -> unit)
	(resolve_type : string list -> string -> string -> module_type)
= object(self)

	val mutable ch = file_ch
	val mutable string_pool = Array.make 0 ""
	val mutable doc_pool = Array.make 0 ""

	val mutable classes = Array.make 0 null_class
	val mutable abstracts = Array.make 0 null_abstract
	(* val mutable enums = Array.make 0 null_enum *)
	val mutable typedefs = Array.make 0 null_tdef
	(* val mutable class_fields = Array.make 0 null_class_field *)
	(* val mutable abstract_fields = Array.make 0 null_abstract_field *)
	(* val mutable enum_fields = Array.make 0 null_enum_field *)

	val mutable type_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)
	val mutable field_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)

	method set_input file_ch =
		ch <- file_ch

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
		(* Printf.eprintf "  Get string #%d\n" l; *)
		try begin
			let s = pool.(l) in
			(* Printf.eprintf "  Read string %s\n" s; *)
			s
		end with e -> begin
			Printf.eprintf "  Failed getting string #%d\n" l;
			(* "" *)
			raise e
		end

	method read_string =
		self#read_from_string_pool string_pool

	method read_raw_string =
		let l = self#read_uleb128 in
		let s = Bytes.unsafe_to_string (IO.nread ch l) in
		(* Printf.eprintf "    Read raw string %s\n" s; *)
		s

	(* Basic compounds *)

	method read_list8 : 'a . (unit -> 'a) -> 'a list = fun f ->
		let l = self#read_leb128 in
		let a = Array.init l (fun _ -> f ()) in
		Array.to_list a

	method read_list16 : 'a . (unit -> 'a) -> 'a list = fun f ->
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
		let pack = self#read_list8 (fun () -> self#read_string) in
		let name = self#read_string in
		(pack,name)

	method read_full_path =
		let pack = self#read_list8 (fun () -> self#read_string) in
		let mname = self#read_string in
		let tname = self#read_string in
		Printf.eprintf "    Read full path %s.%s.%s\n" (ExtString.String.join "." pack) mname tname;
		(pack,mname,tname)

	method read_documentation =
		let doc_own = self#read_option (fun () ->
			(* TODO fix that *)
			(* let _ = self#read_uleb128 in *)
			(* doc_pool.(self#read_uleb128) *)
			self#read_from_string_pool doc_pool
			(* "" *)
		) in
		let doc_inherited = [] in
		(* let doc_inherited = self#read_list8 (fun () -> *)
		(* 	(1* TODO fix that *1) *)
		(* 	(1* let i = self#read_uleb128 in *1) *)
		(* 	(1* let _ = self#read_uleb128 in *1) *)
		(* 	(1* Printf.eprintf "    Read doc string %d\n" i; *1) *)
		(* 	(1* doc_pool.(i) *1) *)
		(* 	(1* doc_pool.(self#read_uleb128) *1) *)
		(* 	self#read_from_string_pool doc_pool *)
		(* 	(1* "" *1) *)
		(* ) in *)
		{doc_own;doc_inherited}

	method read_pos =
		let file = self#read_string in
		let min = self#read_leb128 in
		let max = self#read_leb128 in
		{
			pfile = file;
			pmin = min;
			pmax = max;
		}

	method read_metadata_entry : metadata_entry =
		let name = self#read_string in
		(* TODO: el *)
		let p = self#read_pos in
		(Meta.from_string name,[],p)

	method read_metadata =
		self#read_list16 (fun () -> self#read_metadata_entry)

	(* References *)

	method read_class_ref =
		let i = self#read_uleb128 in
		Printf.eprintf "  Reading class ref %d...\n" i;
		try
			classes.(i)
		with e ->
			Printf.eprintf "  Failed to read class ref %d\n" i;
			raise e

	method read_abstract_ref =
		let i = self#read_uleb128 in
		Printf.eprintf "  Reading class ref %d...\n" i;
		try
			abstracts.(i)
		with e ->
			Printf.eprintf "  Failed to read abstract ref %d\n" i;
			raise e

	method read_enum_ref =
		assert false (* TODO *)

	method read_typedef_ref =
		typedefs.(self#read_uleb128)

	method read_field_ref =
		null_field (* TODO *)

	method read_enum_field_ref =
		assert false (* TODO *)

	method read_anon_field_ref =
		assert false (* TODO *)

	(* Type instances *)

	method read_type_instance =
		match self#read_u8 with
		| 0 ->
			Printf.eprintf "  TODO identity\n";
			mk_mono() (* TODO: identity *)
		| 1 ->
			self#read_type_instance
		| 5 ->
			(field_type_parameters.(self#read_uleb128)).ttp_type
		| 6 ->
			(type_type_parameters.(self#read_uleb128)).ttp_type
		| 10 ->
			TInst(self#read_class_ref,[])
		| 13 ->
			TAbstract(self#read_abstract_ref,[])
		| 11
		| 12 ->
			ignore(self#read_uleb128);
			Printf.eprintf "  TODO TType\n";
			t_dynamic (* TODO *)
		| 14 ->
			let c = self#read_class_ref in
			let tl = self#read_types in
			TInst(c,tl)
		| 15
		| 16
		| 17 ->
			ignore(self#read_uleb128);
			let _ = self#read_types in
			Printf.eprintf "  TODO TAbstract\n";
			t_dynamic (* TODO *)
		| 32 ->
			let f () =
				let name = self#read_string in
				let opt = self#read_bool in
				let t = self#read_type_instance in
				(name,opt,t)
			in
			(* TODO check list 8 vs 16 *)
			let args = self#read_list16 f in
			let ret = self#read_type_instance in
			TFun(args,ret)
		| 40 ->
			t_dynamic
		| 41 ->
			TDynamic (Some self#read_type_instance)
		| 50 ->
			mk_anon (ref Closed)
		| 51 ->
			ignore(self#read_uleb128);
			Printf.eprintf "  TODO TAnon\n";
			t_dynamic (* TODO *)
		| i ->
			error (Printf.sprintf "Bad type instance id: %i" i)

	method read_types =
			(* TODO check list 8 vs 16 *)
		self#read_list8 (fun () -> self#read_type_instance)
		(* self#read_list16 (fun () -> self#read_type_instance) *)

	(* Fields *)

	method read_type_parameters (m : module_def) (path : path) (f : typed_type_param array -> unit) =
		let a = Array.init (self#read_uleb128) (fun _ ->
			let name = self#read_string in
			let pos = self#read_pos in
			let c = mk_class m (fst path @ [snd path],name) pos pos in
			mk_type_param name (TInst(c,[])) None
		) in
		f a;
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let tl1 = self#read_types in
			let tl2 = self#read_types in
			begin match a.(i) with
			| {ttp_type = TInst(c,_)} as ttp ->
				c.cl_kind <- KTypeParameter tl1;
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

	(* method read_type_parameter = *)
	(* 	let name = self#read_string in *)
	(* 	let c = self#read_class true in *)
	(* 	(name,TInst(c,[])) *)

	method read_binop =
		match IO.read_byte ch with
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
			| 20 -> OpAssignOp(self#read_binop)
			| 21 -> OpInterval
			| 22 -> OpArrow
			| 23 -> OpIn
			| _ -> assert false

	method read_unop =
		match IO.read_byte ch with
		| 0 -> Increment
		| 1 -> Decrement
		| 2 -> Not
		| 3 -> Neg
		| 4 -> NegBits
		| _ -> assert false

	method read_quote_status =
		match IO.read_byte ch with
		| 0 -> NoQuotes
		| 1 -> DoubleQuotes
		| _ -> assert false

	method read_tconstant =
		match IO.read_byte ch with
		| 0 -> TBool(false)
		| 1 -> TBool(true)
		| 2 -> TNull
		| 3 -> TThis
		| 4 -> TSuper
		| 5 -> TInt(IO.read_real_i32 ch)
		| 6 -> TFloat(self#read_string)
		| 7 -> TString(self#read_string)
		| _ -> assert false

	method read_tvar_def =
		let read_tvar_extra =
			(* TODO type parameters *)
			(* list_8 *)
			(* let params = self#read_list8 (fun () -> self#read_type_parameter) in *)
			let params = [] in
			let e = self#read_option (fun () -> self#read_texpr) in
			{
				v_params = params;
				v_expr = e;
			}
		in
		(* let i = self#read_lut in *)
		let id = IO.read_i32 ch in
		let name = self#read_string in
		let t = self#read_type_instance in
		(* let capture = self#read_bool in *)
		let _ = self#read_bool in
		let extra = self#read_option (fun () -> read_tvar_extra) in
		(* list 8 *)
		let meta = self#read_metadata in
		let p = self#read_pos in
		let v = {
			v_id = id;
			v_name = name;
			v_type = t;
			(* TODO *)
			(* v_capture = capture; *)
			v_kind = VGenerated; (* TODO *)
			v_extra = extra;
			(* TODO *)
			v_flags = 0;
			v_meta = meta;
			v_pos = p;
		} in
		(* vars.(i) <- v; *)
		v

	method read_object_field_key =
		let name = self#read_string in
		let p = self#read_pos in
		let quotes = self#read_quote_status in
		(name,p,quotes)

	method read_object_field =
		let k = self#read_object_field_key in
		let e = self#read_texpr in
		(k,e)

	method read_tfunction_arg =
		let v = self#read_tvar_def in
		let cto = self#read_option (fun () -> self#read_texpr) in
		(* let cto = self#read_option (fun () -> self#read_tconstant) in *)
		(v,cto)

	method read_tfunction =
		(* list_8 *)
		let args = self#read_list8 (fun () -> self#read_tfunction_arg) in
		let r = self#read_type_instance in
		let e = self#read_texpr in
		{
			tf_args = args;
			tf_type = r;
			tf_expr = e;
		}

	method read_switch_case =
		(* list_8 *)
		let el = self#read_list8 (fun () -> self#read_texpr) in
		let e = self#read_texpr in
		{
			case_patterns = el;
			case_expr = e;
		}

	method read_catch =
		let v = self#read_tvar_def in
		let e = self#read_texpr in
		(v,e)

	method read_tfield_access =
		match IO.read_byte ch with
		| 0 ->
			let c = self#read_class_ref in
			let tl = self#read_types in
			let cf = self#read_field_ref in
			FInstance(c,tl,cf)
		| 1 ->
			let c = self#read_class_ref in
			let cf = self#read_field_ref in
			FStatic(c,cf)
		| 2 -> FAnon(self#read_anon_field_ref)
		| 3 -> FDynamic(self#read_string)
		| 4 -> FClosure(None,self#read_field_ref)
		| 5 ->
			let c = self#read_class_ref in
			let tl = self#read_types in
			let cf = self#read_field_ref in
			FClosure(Some(c,tl),cf)
		| 6 ->
			let en = self#read_enum_ref in
			let ef = self#read_enum_field_ref in
			FEnum(en,ef)
		| _ -> assert false

	method read_texpr =
		let pos = self#read_pos in

		let update_pos =
			let dmin = self#read_i16 in
			let dmax = self#read_i16 in

			(* TODO return updated pos *)
			ignore(dmin);
			ignore(dmax);
			pos
		in

		let loop () =
		(* let rec loop () = *)
			let t = self#read_type_instance in
			let pos = update_pos in

			let e = match IO.read_byte ch with
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
				(* TODO retrieve local by v_vid *)
				(* | 20 -> TLocal (vars.self#read_lut)) *)
				(* TODO read_var *)
				(* | 21 -> TVar (self#read_var,None) *)
				(* | 22 -> TVar (self#read_var, Some (loop ())) *)

				(* blocks 30-49 *)

				| i ->
					Printf.eprintf "  [ERROR] Unhandled texpr %d\n" i;
					assert false
			in

			{
				eexpr = e;
				etype = t;
				epos = pos;
			}
		in

		loop ();

			(* let e = match IO.read_byte ch with *)
			(* 	| 0 -> TConst(self#read_tconstant) *)
			(* 	| 1 -> assert false *)
			(* 	(1* | 1 -> TLocal (vars.(self#read_lut)) (2* TODO *2) *1) *)
			(* 	| 2 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let e2 = self#read_texpr in *)
			(* 		TArray(e1,e2) *)
			(* 	| 3 -> *)
			(* 		let op = self#read_binop in *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let e2 = self#read_texpr in *)
			(* 		TBinop(op,e1,e2) *)
			(* 	| 4 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let fa = self#read_tfield_access in *)
			(* 		TField(e1,fa) *)
			(* 	| 5 -> TTypeExpr (TClassDecl (self#read_class_ref)) *)
			(* 	| 6 -> TTypeExpr (TEnumDecl (self#read_enum_ref)) *)
			(* 	| 7 -> TTypeExpr (TTypeDecl (self#read_typedef_ref)) *)
			(* 	| 8 -> TTypeExpr (TAbstractDecl (self#read_abstract_ref)) *)
			(* 	| 9 -> TParenthesis(self#read_texpr) *)
			(* 	| 10 -> TObjectDecl(self#read_list8 (fun () -> self#read_object_field)) *)
			(* 	| 11 -> TArrayDecl(self#read_list8 (fun () -> self#read_texpr)) *)
			(* 	| 12 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let el = self#read_list8 (fun () -> self#read_texpr) in *)
			(* 		TCall(e1,el) *)
			(* 	| 13 -> *)
			(* 		let c = self#read_class_ref in *)
			(* 		let tl = self#read_types in *)
			(* 		let el = self#read_list8 (fun () -> self#read_texpr) in *)
			(* 		TNew(c,tl,el) *)
			(* 	| 14 -> *)
			(* 		let op = self#read_unop in *)
			(* 		let e1 = self#read_texpr in *)
			(* 		TUnop(op,Prefix,e1) *)
			(* 	| 15 -> *)
			(* 		let op = self#read_unop in *)
			(* 		let e1 = self#read_texpr in *)
			(* 		TUnop(op,Postfix,e1) *)
			(* 	| 16 -> TFunction(self#read_tfunction) *)
			(* 	| 17 -> *)
			(* 		let v = self#read_tvar_def in *)
			(* 		let eo = self#read_option (fun () -> self#read_texpr) in *)
			(* 		TVar(v,eo) *)
			(* 	(1* TODO check list 8 vs 16 *1) *)
			(* 	| 18 -> TBlock(self#read_list16 (fun () -> self#read_texpr)) *)
			(* 	| 19 -> *)
			(* 		let v = self#read_tvar_def in *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let e2 = self#read_texpr in *)
			(* 		TFor(v,e1,e2) *)
			(* 	| 20 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let e2 = self#read_texpr in *)
			(* 		TIf(e1,e2,None) *)
			(* 	| 21 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let e2 = self#read_texpr in *)
			(* 		let e3 = self#read_texpr in *)
			(* 		TIf(e1,e2,Some e3) *)
			(* 	| 22 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let e2 = self#read_texpr in *)
			(* 		TWhile(e1,e2,NormalWhile) *)
			(* 	| 23 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let e2 = self#read_texpr in *)
			(* 		TWhile(e1,e2,DoWhile) *)
			(* 	| 24 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 	(1* TODO check list 8 vs 16 *1) *)
			(* 		let cases = self#read_list16 (fun () -> self#read_switch_case) in *)
			(* 		let eo = self#read_option (fun () -> self#read_texpr) in *)

			(* 		TSwitch { *)
			(* 			switch_subject = e1; *)
			(* 			switch_cases = cases; *)
			(* 			switch_default = eo; *)
			(* 			switch_exhaustive = true; (1* I guess, if this was saved? *1) *)
			(* 		} *)
			(* 	| 25 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let catches = self#read_list8 (fun () -> self#read_catch) in *)
			(* 		TTry(e1,catches) *)
			(* 	| 26 -> TReturn None *)
			(* 	| 27 -> TReturn (Some (self#read_texpr)) *)
			(* 	| 28 -> TBreak *)
			(* 	| 29 -> TContinue *)
			(* 	| 30 -> TThrow (self#read_texpr) *)
			(* 	| 31 -> TCast(self#read_texpr,None) *)
			(* 	| 32 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let c = self#read_class_ref in *)
			(* 		TCast(e1,Some (TClassDecl c)) *)
			(* 	| 33 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let en = self#read_enum_ref in *)
			(* 		TCast(e1,Some (TEnumDecl en)) *)
			(* 	| 34 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let td = self#read_typedef_ref in *)
			(* 		TCast(e1,Some (TTypeDecl td)) *)
			(* 	| 35 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let a = self#read_abstract_ref in *)
			(* 		TCast(e1,Some (TAbstractDecl a)) *)
			(* 	| 36 -> *)
			(* 		let m = self#read_metadata_entry in *)
			(* 		let e1 = self#read_texpr in *)
			(* 		TMeta(m,e1) *)
			(* 	| 37 -> *)
			(* 		let e1 = self#read_texpr in *)
			(* 		let ef = self#read_enum_field_ref in *)
			(* 		let i = IO.read_i32 ch in *)
			(* 		TEnumParameter(e1,ef,i) *)
			(* 	| 38 -> TEnumIndex(self#read_texpr) *)
			(* 	| 39 -> TIdent(self#read_string) *)
			(* 	| _ -> assert false *)
			(* in *)

		method read_class_field (m : module_def) : tclass_field =
			let name = self#read_string in
			Printf.eprintf "    Read class field %s\n" name;
			(* TODO read_list? *)
			(* self#read_type_parameters m ([],name) (fun a -> *)
			(* 	field_type_parameters <- a *)
			(* ); *)
			let t = self#read_type_instance in
			let flags = IO.read_i32 ch in
			let pos = self#read_pos in
			let name_pos = self#read_pos in
			let doc = self#read_option (fun () -> self#read_documentation) in
			let meta = self#read_metadata in
			(* self#read_type_parameters m ([],name) (fun a -> *)
			(* 	field_type_parameters <- a *)
			(* ); *)
			let params = Array.to_list field_type_parameters in
			let kind = self#read_field_kind in
			let expr = self#read_option (fun () -> self#read_texpr) in
			let expr_unoptimized = self#read_option (fun () -> self#read_texpr) in
			let overloads = self#read_list16 (fun () -> self#read_class_field m) in
			{
				cf_name = name;
				cf_type = t;
				cf_pos = pos;
				cf_name_pos = name_pos;
				cf_doc = doc;
				cf_meta = meta;
				cf_kind = kind;
				(* cf_expr = None; *)
				(* cf_expr_unoptimized = None; *)
				cf_expr = expr;
				cf_expr_unoptimized = expr_unoptimized;
				cf_params = params;
				cf_overloads = overloads;
				cf_flags = flags;
			}

		method read_class_fields (m : module_def) (c : tclass) =
			Printf.eprintf "  Read class fields for %s\n" (snd m.m_path);
			let f () = self#read_class_field m in
			begin match c.cl_kind with
			| KAbstractImpl a ->
				type_type_parameters <- Array.of_list a.a_params
			| _ ->
				type_type_parameters <- Array.of_list c.cl_params
			end;
			Printf.eprintf "  Read constructor for %s\n" (snd m.m_path);
			c.cl_constructor <- self#read_option f;
			Printf.eprintf "  Read ordered fields for %s\n" (snd m.m_path);
				(* TODO check list 8 vs 16 *)
			c.cl_ordered_fields <- self#read_list8 f;
				(* TODO check list 8 vs 16 *)
			Printf.eprintf "  Read ordered statics for %s\n" (snd m.m_path);
			c.cl_ordered_statics <- self#read_list8 f;
			List.iter (fun cf -> c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics) c.cl_ordered_statics;

		(* Module types *)

		method read_common_module_type (m : module_def) (infos : tinfos) =
			infos.mt_private <- self#read_bool;
			(* TODO: fix that *)
			(* infos.mt_doc <- self#read_option (fun () -> self#read_documentation); *)
			infos.mt_meta <- self#read_metadata;
			(* TODO update to new type param format? *)
			self#read_type_parameters m infos.mt_path (fun a ->
				type_type_parameters <- a
			);
			infos.mt_params <- Array.to_list type_type_parameters;
				(* TODO check list 8 vs 16 *)
			infos.mt_using <- self#read_list16 (fun () ->
				let c = self#read_class_ref in
				let p = self#read_pos in
				(c,p)
			)

		method read_class_kind = match self#read_u8 with
			| 0 ->
				KNormal
			| 1 ->
				KTypeParameter self#read_types
			| 2 ->
				KExpr ((EBlock []),null_pos) (* TODO *)
			| 3 ->
				KGeneric
			| 4 ->
				let c = self#read_class_ref in
				let tl = self#read_types in
				KGenericInstance(c,tl)
			| 5 ->
				KMacroType
			| 6 ->
				KGenericBuild [] (* TODO *)
			| 7 ->
				KAbstractImpl self#read_abstract_ref
			| 8 ->
				(* TODO *)
				KNormal
			| i ->
				error (Printf.sprintf "Invalid class kind id: %i" i)

		method read_class (m : module_def) (c : tclass) =
			self#read_common_module_type m (Obj.magic c);
			c.cl_kind <- self#read_class_kind;
			c.cl_flags <- (Int32.to_int self#read_u32);
			let read_relation () =
				let c = self#read_class_ref in
				let tl = self#read_types in
				(c,tl)
			in
			c.cl_super <- self#read_option read_relation;
				(* TODO check list 8 vs 16 *)
			c.cl_implements <- self#read_list16 read_relation;
			c.cl_dynamic <- self#read_option (fun () -> self#read_type_instance);
			c.cl_array_access <- self#read_option (fun () -> self#read_type_instance);

		method read_abstract (m : module_def) (a : tabstract) =
			self#read_common_module_type m (Obj.magic a);
			a.a_impl <- self#read_option (fun () -> self#read_class_ref);
			a.a_this <- self#read_type_instance;
				(* TODO check list 8 vs 16 *)
			a.a_from <- self#read_list16 (fun () -> self#read_type_instance);
				(* TODO check list 8 vs 16 *)
			a.a_from_field <- self#read_list16 (fun () ->
				let t = self#read_type_instance in
				let cf = self#read_field_ref in
				(t,cf)
			);
				(* TODO check list 8 vs 16 *)
			a.a_to <- self#read_list16 (fun () -> self#read_type_instance);
				(* TODO check list 8 vs 16 *)
			a.a_to_field <- self#read_list16 (fun () ->
				let t = self#read_type_instance in
				let cf = self#read_field_ref in
				(t,cf)
			);
				(* TODO check list 8 vs 16 *)
			a.a_array <- self#read_list16 (fun () -> self#read_field_ref);
			a.a_read <- self#read_option (fun () -> self#read_field_ref);
			a.a_write <- self#read_option (fun () -> self#read_field_ref);
			a.a_call <- self#read_option (fun () -> self#read_field_ref);
			a.a_enum <- self#read_bool

		(* Chunks *)

		method read_string_pool =
			let l = self#read_uleb128 in
			(* Printf.eprintf "  Read string pool of size %d\n" l; *)
			Array.init l (fun i ->
				self#read_raw_string;
			);

		method read_chunk =
			let size = Int32.to_int self#read_u32 in
			let name = Bytes.unsafe_to_string (IO.nread ch 4) in
			let data = IO.nread ch size in
			let crc = self#read_u32 in
			ignore(crc); (* TODO *)
			let kind = chunk_kind_of_string name in
			(kind,data)

		method read_cfld (m : module_def) =
			let l = self#read_uleb128 in
			for i = 0 to l - 1 do
				let c = classes.(i) in
				self#read_class_fields m c;
			done

		method read_clsd (m : module_def) =
			let l = self#read_uleb128 in
			for i = 0 to l - 1 do
				let c = classes.(i) in
				self#read_class m c;
			done

		method read_absd (m : module_def) =
			let l = self#read_uleb128 in
			for i = 0 to l - 1 do
				let a = abstracts.(i) in
				self#read_abstract m a;
			done

		method read_clsr =
			let l = self#read_uleb128 in
			classes <- Array.append classes (Array.init l (fun i ->
				let (pack,mname,tname) = self#read_full_path in
				Printf.eprintf "  Read clsr %d of %d for %s.%s\n" i (l-1) mname tname;
				match resolve_type pack mname tname with
				| TClassDecl c ->
					c
				| _ ->
					error ("Unexpected type where class was expected: " ^ (s_type_path (pack,tname)))
			))
			(* classes <- self#read_list16 (fun () -> *)
			(* 	let (pack,mname,tname) = self#read_full_path in *)
			(* 	match resolve_type pack mname tname with *)
			(* 	| TClassDecl c -> *)
			(* 		c *)
			(* 	| _ -> *)
			(* 		error ("Unexpected type where class was expected: " ^ (s_type_path (pack,tname))) *)
			(* ); *)

		method read_absr =
			let l = self#read_uleb128 in
			abstracts <- Array.append abstracts (Array.init l (fun i ->
				let (pack,mname,tname) = self#read_full_path in
				Printf.eprintf "  Read absr %d of %d for abstract %s\n" i l tname;
				match resolve_type pack mname tname with
				| TAbstractDecl a ->
					a
				| _ ->
					error ("Unexpected type where class was expected: " ^ (s_type_path (pack,tname)))
			))

		method read_typf (m : module_def) =
				(* TODO check list 8 vs 16 *)
			self#read_list16 (fun () ->
				let kind = self#read_u8 in
				let path = self#read_path in
				let pos = self#read_pos in
				let name_pos = self#read_pos in
				let mt = match kind with
				| 0 ->
					let c = mk_class m path pos name_pos in
					TClassDecl c
				| 1 ->
					let en = mk_enum m path pos name_pos in
					TEnumDecl en
				| 2 ->
					let td = mk_typedef m path pos name_pos (mk_mono()) in
					TTypeDecl td
				| 3 ->
					let a = mk_abstract m path pos name_pos in
					TAbstractDecl a
				| _ ->
					error ("Invalid type kind: " ^ (string_of_int kind));
				in
				mt
			)

		method read_hhdr =
			let path = self#read_path in
			let file = self#read_string in
			let m = make_module path file in
			m

		method read (debug : bool) (p : pos) =
			(* TODO: add it to writer! *)
			(* if (Bytes.to_string (IO.nread ch 3)) <> "hxb" then *)
			(* 	raise (HxbFailure "magic"); *)
			(* let version = self#read_u8 in *)
			(* ignore(version); *)
			let rec loop acc =
				(* ch <- file_ch; *)
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
					raise (HxbFailure "Missing HHDR chunk")
				| (kind,data) :: chunks ->
					ch <- IO.input_bytes data;
					match kind with
					| HHDR ->
						let m = self#read_hhdr in
						m,chunks
					| STRI ->
						(* string_pool <- Array.concat [string_pool; self#read_string_pool]; *)
						string_pool <- self#read_string_pool;
						(* Array.iteri (fun i s -> *)
						(* 	Printf.eprintf "  [Pool] string #%d %s\n" i s; *)
						(* ) string_pool; *)
						pass_0 chunks
					| DOCS ->
						(* doc_pool <- Array.concat [doc_pool; self#read_string_pool]; *)
						doc_pool <- self#read_string_pool;
						(* Array.iteri (fun i s -> *)
						(* 	Printf.eprintf "  [Pool] doc string #%d %s\n" i s; *)
						(* ) doc_pool; *)
						pass_0 chunks
					| _ ->
						raise (HxbFailure ("Unexpected early chunk: " ^ (string_of_chunk_kind kind)))
			in
			let m,chunks = pass_0 chunks in
			List.iter (fun (kind,data) ->
				Printf.eprintf "Reading chunk %s\n" (string_of_chunk_kind kind);
				ch <- IO.input_bytes data;
				match kind with
				| TYPF ->
					m.m_types <- self#read_typf m;
					add_module m;
				| CLSR ->
					self#read_clsr;
				| ABSR ->
					self#read_absr;
				| CLSD ->
					self#read_clsd m;
				| CFLD ->
					Printf.eprintf "  Read class fields\n";
					self#read_cfld m;
				| ABSD ->
					self#read_absd m;
				| _ ->
					raise (HxbFailure ("Unexpected late chunk: " ^ (string_of_chunk_kind kind)))
			) chunks;
			m
	end
