open Globals
open Ast
open Type
open HxbData
open TPrinting

class hxb_reader
	(com : Common.context)
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
	val mutable enums = Array.make 0 null_enum
	val mutable typedefs = Array.make 0 null_typedef
	(* val mutable class_fields = Array.make 0 null_class_field *)
	(* val mutable abstract_fields = Array.make 0 null_abstract_field *)
	(* val mutable enum_fields = Array.make 0 null_enum_field *)

	val vars = Hashtbl.create 0
	(* val mutable vars = Array.make 0 null_tvar *)
	val mutable type_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)
	val mutable field_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)

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
			Printf.eprintf "  Failed getting string #%d\n" l;
			raise e

	method read_string =
		self#read_from_string_pool string_pool

	method read_raw_string =
		let l = self#read_uleb128 in
		Bytes.unsafe_to_string (IO.nread ch l)

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
		let pack = self#read_list16 (fun () -> self#read_string) in
		let mname = self#read_string in
		let tname = self#read_string in
		Printf.eprintf "    Read full path %s\n" (ExtString.String.join "." (pack @ [mname; tname]));
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
		let pos = {
			pfile = file;
			pmin = min;
			pmax = max;
		} in
		(* Printf.eprintf "Read pos: %s\n" (Printer.s_pos pos); *)
		(* MessageReporting.display_source_at com pos; *)
		pos

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
		classes.(i)

	method read_abstract_ref =
		let i = self#read_uleb128 in
		abstracts.(i)

	method read_enum_ref =
		let i = self#read_uleb128 in
		enums.(i)

	method read_typedef_ref =
		let i = self#read_uleb128 in
		typedefs.(i)

	method read_field_ref fields =
		let name = self#read_string in
		try PMap.find name fields with e ->
			Printf.eprintf "  TODO error reading field ref for %s\n" name;
			(* raise e *)
			null_field

	method read_enum_field_ref =
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
			let i = self#read_uleb128 in
			(* Printf.eprintf "     Get field type param %d\n" i; *)
			(field_type_parameters.(i)).ttp_type
		| 6 ->
			let i = self#read_uleb128 in
			(* Printf.eprintf "     Get type type param %d\n" i; *)
			(type_type_parameters.(i)).ttp_type
		| 10 ->
			TInst(self#read_class_ref,[])
		| 11 ->
			let i = self#read_uleb128 in
			Printf.eprintf "  TODO enum ref #%d\n" i;
			t_dynamic (* TODO *)
		| 12 ->
			let i = self#read_uleb128 in
			Printf.eprintf "  TODO typedef ref #%d\n" i;
			t_dynamic (* TODO *)
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
			let t = self#read_typedef_ref in
			let tl = self#read_types in
			TType(t,tl)
		| 17 ->
			let a = self#read_abstract_ref in
			let tl = self#read_types in
			TAbstract(a,tl)
		| 32 ->
			let f () =
				let name = self#read_string in
				(* Printf.eprintf "  Read type instance for %s\n" name; *)
				let opt = self#read_bool in
				let t = self#read_type_instance in
				(name,opt,t)
			in
			let args = self#read_list16 f in
			(* Printf.eprintf "  Read type instance for TFun\n"; *)
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
		self#read_list16 (fun () -> self#read_type_instance)

	(* Fields *)

	method read_type_parameters (m : module_def) (path : path) (f : typed_type_param array -> unit) =
		let l = self#read_uleb128 in
		let a = Array.init l (fun _ ->
			let name = self#read_string in
			let pos = self#read_pos in
			Printf.eprintf "      Read ttp pos for %s: %s\n" name (Printer.s_pos pos);
			Printf.eprintf "      - Path was %s\n" (s_type_path path);

			(* This is wrong for field ttp (why again?) *)
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

	(* method read_quote_status = *)
	(* 	match IO.read_byte ch with *)
	(* 	| 0 -> NoQuotes *)
	(* 	| 1 -> DoubleQuotes *)
	(* 	| _ -> assert false *)

	(* method read_object_field_key = *)
	(* 	let name = self#read_string in *)
	(* 	let p = self#read_pos in *)
	(* 	let quotes = self#read_quote_status in *)
	(* 	(name,p,quotes) *)

	(* method read_object_field = *)
	(* 	Printf.eprintf "   read_object_field\n"; *)
	(* 	let k = self#read_object_field_key in *)
	(* 	let e = self#read_texpr in *)
	(* 	(k,e) *)

	method read_tfunction_arg =
		let v = self#read_var in
		let cto = self#read_option (fun () -> self#read_texpr) in
		(v,cto)

	method read_tfunction =
		let args = self#read_list16 (fun () -> self#read_tfunction_arg) in
		let r = self#read_type_instance in
		let e = self#read_texpr in
		{
			tf_args = args;
			tf_type = r;
			tf_expr = e;
		}

	(* method read_switch_case = *)
	(* 	(1* list_8 *1) *)
	(* 	(1* Printf.eprintf "   read_switch_case\n"; *1) *)
	(* 	let el = self#read_list8 (fun () -> self#read_texpr) in *)
	(* 	let e = self#read_texpr in *)
	(* 	{ *)
	(* 		case_patterns = el; *)
	(* 		case_expr = e; *)
	(* 	} *)

	(* method read_catch = *)
	(* 	(1* Printf.eprintf "   read_catch\n"; *1) *)
	(* 	let v = self#read_var in *)
	(* 	let e = self#read_texpr in *)
	(* 	(v,e) *)

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
		let t = self#read_type_instance in
		let kind = self#read_var_kind in
		(* TODO flags / extra *)
		let meta = self#read_metadata in
		let pos = self#read_pos in
		let v = {
			v_id = id;
			v_name = name;
			v_type = t;
			v_kind = kind;
			v_meta = meta;
			v_pos = pos;
			v_extra = None;
			v_flags = 0;
		} in
		(* vars.(id) <- v; *)
		Hashtbl.add vars id v;
		(* vars.(id) <- v; *)
		v

	method read_texpr =
		let t = self#read_type_instance in
		let pos = self#read_pos in

		let i = IO.read_byte ch in
		(* Printf.eprintf "      -- texpr [%d] --\n" i; *)
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
			| 21 -> TVar (self#read_var,None)
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
				let fl = self#read_list16 (fun () ->
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
				(* TODO TSwitch *)
				assert false
			| 83 ->
				(* TODO TTry *)
				assert false
			| 84 ->
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TWhile(e1,e2,NormalWhile)
			| 85 ->
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TWhile(e1,e2,DoWhile)
			| 86 ->
				let v = self#read_var in
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
				let ef = self#read_enum_field_ref in
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
				(* TODO (see writer) *)
				(* TODO TField(e1,FAnon(cf)) *)
				e1.eexpr
			| 105 ->
				let e1 = self#read_texpr in
				let c = self#read_class_ref in
				let tl = self#read_types in
				let cf = self#read_field_ref c.cl_fields in
				TField(e1,FClosure(Some(c,tl),cf))
			| 106 ->
				let e1 = self#read_texpr in
				(* TODO (see writer) *)
				(* TODO TField(e1,FClosure(None,cf)) *)
				e1.eexpr
			| 107 ->
				let e1 = self#read_texpr in
				let en = self#read_enum_ref in
				let ef = self#read_enum_field_ref in
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
				let path = self#read_path in
				(* TODO retrieve md from path *)
				(* TCast(e1,Some path) *)
				assert false
			| 126 ->
				let c = self#read_class_ref in
				let tl = self#read_types in
				let el = self#read_texpr_list in
				TNew(c,tl,el)

			(* unops 140-159 *)
			| _ when i >= 140 && i < 160 ->
				let get_unop i = match i with
					| 0 -> Increment,Prefix
					| 1 -> Decrement,Prefix
					| 2 -> Not,Prefix
					| 3 -> Neg,Prefix
					| 4 -> NegBits,Prefix
					| 5 -> Spread,Prefix
					| 6 -> Increment,Prefix
					| 7 -> Decrement,Prefix
					| 8 -> Not,Prefix
					| 9 -> Neg,Prefix
					| 10 -> NegBits,Prefix
					| 11 -> Spread,Prefix
					| _ -> assert false
				in

				let (op,flag) = get_unop (i - 140) in
				let e = self#read_texpr in
				TUnop(op,flag,e)

			(* binops 160-219 *)
			| _ when i >= 160 && i < 220 ->
				let rec get_binop i = match i with
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
					| _ -> OpAssignOp (get_binop (i - 30))
				in

				let op = get_binop (i - 160) in
				let e1 = self#read_texpr in
				let e2 = self#read_texpr in
				TBinop(op,e1,e2)

			(* rest 250-254 *)
			| 250 -> TIdent (self#read_string)

			| i ->
				Printf.eprintf "  [ERROR] Unhandled texpr %d at:\n" i;
				MessageReporting.display_source_at com pos;
				assert false
		in

		(* Printf.eprintf "   Done reading texpr at:\n"; *)
		(* MessageReporting.display_source_at com pos; *)

		{
			eexpr = e;
			etype = t;
			epos = pos;
		}

	method read_texpr_list =
		let len = IO.read_ui16 ch in
		List.init len (fun _ -> self#read_texpr);

	method read_class_field (m : module_def) : tclass_field =
		let name = self#read_string in
		Printf.eprintf "  field type parameters for %s\n" name;
		self#read_type_parameters m ([],name) (fun a ->
			field_type_parameters <- a
		);
		let params = Array.to_list field_type_parameters in
		(* Printf.eprintf "  read type instance for %s\n" name; *)
		let t = self#read_type_instance in
		(* Printf.eprintf "  flags for %s (done) \n" name; *)
		let flags = IO.read_i32 ch in
		let pos = self#read_pos in
		let name_pos = self#read_pos in

		(* TODO fix doc *)
		(* let doc = self#read_option (fun () -> self#read_documentation) in *)
		let doc = None in

		let meta = self#read_metadata in
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
			cf_expr = expr;
			cf_expr_unoptimized = expr_unoptimized;
			cf_params = params;
			cf_overloads = overloads;
			cf_flags = flags;
		}

	method read_class_fields (m : module_def) (c : tclass) =
		let f () = self#read_class_field m in
		begin match c.cl_kind with
		| KAbstractImpl a ->
			type_type_parameters <- Array.of_list a.a_params
		| _ ->
			type_type_parameters <- Array.of_list c.cl_params
		end;
		Printf.eprintf "  read class fields with type parameters for %s: %d\n" (snd c.cl_path) (Array.length type_type_parameters);
		Printf.eprintf "    own class params: %d\n" (List.length c.cl_params);
		c.cl_constructor <- self#read_option f;
		c.cl_ordered_fields <- self#read_list16 f;
		c.cl_ordered_statics <- self#read_list16 f;
		List.iter (fun cf -> c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics) c.cl_ordered_statics;

	(* Module types *)

	method read_common_module_type (m : module_def) (infos : tinfos) =
		infos.mt_private <- self#read_bool;
		(* TODO: fix that *)
		(* infos.mt_doc <- self#read_option (fun () -> self#read_documentation); *)
		infos.mt_meta <- self#read_metadata;
		Printf.eprintf "  read type parameters for %s\n" (snd infos.mt_path);
		self#read_type_parameters m infos.mt_path (fun a ->
			Printf.eprintf "  read type parameters for %s: %d\n" (snd infos.mt_path) (Array.length a);
			type_type_parameters <- a
		);
		infos.mt_params <- Array.to_list type_type_parameters;
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
		c.cl_implements <- self#read_list16 read_relation;
		c.cl_dynamic <- self#read_option (fun () -> self#read_type_instance);
		c.cl_array_access <- self#read_option (fun () -> self#read_type_instance);

	method read_abstract (m : module_def) (a : tabstract) =
		Printf.eprintf "  Read abstract %s\n" (snd a.a_path);
		self#read_common_module_type m (Obj.magic a);
		Printf.eprintf "1";
		a.a_impl <- self#read_option (fun () -> self#read_class_ref);
		Printf.eprintf "2";
		a.a_this <- self#read_type_instance;
		Printf.eprintf "3";
		a.a_from <- self#read_list16 (fun () -> self#read_type_instance);
		Printf.eprintf "4";
		a.a_from_field <- self#read_list16 (fun () ->
			let name = self#read_string in
			self#read_type_parameters m ([],name) (fun a ->
				field_type_parameters <- a
			);
			let t = self#read_type_instance in
			let cf = self#read_field_ref (Option.get a.a_impl).cl_statics in
			(t,cf)
		);
		Printf.eprintf "5";
		a.a_to <- self#read_list16 (fun () -> self#read_type_instance);
		Printf.eprintf "6";
		a.a_to_field <- self#read_list16 (fun () ->
			let name = self#read_string in
			self#read_type_parameters m ([],name) (fun a ->
				field_type_parameters <- a
			);
			let t = self#read_type_instance in
			let cf = self#read_field_ref (Option.get a.a_impl).cl_fields in
			(t,cf)
		);
		Printf.eprintf "7";
		a.a_array <- self#read_list16 (fun () -> self#read_field_ref (Option.get a.a_impl).cl_statics);
		Printf.eprintf "8";
		a.a_read <- self#read_option (fun () -> self#read_field_ref (Option.get a.a_impl).cl_fields);
		Printf.eprintf "9";
		a.a_write <- self#read_option (fun () -> self#read_field_ref (Option.get a.a_impl).cl_fields);
		Printf.eprintf "10";
		a.a_call <- self#read_option (fun () -> self#read_field_ref (Option.get a.a_impl).cl_fields);
		Printf.eprintf "11";
		a.a_enum <- self#read_bool;
		Printf.eprintf "12\n";

	method read_enum (m : module_def) (e : tenum) =
		(* TODO *)
		()

	method read_typedef (m : module_def) (t : tdef) =
		(* TODO *)
		()

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

	method read_enmd (m : module_def) =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let en = enums.(i) in
			self#read_enum m en;
		done

	method read_tpdd (m : module_def) =
		let l = self#read_uleb128 in
		for i = 0 to l - 1 do
			let t = typedefs.(i) in
			self#read_typedef m t;
		done

	method read_clsr =
		let l = self#read_uleb128 in
		(* classes <- Array.append classes (Array.init l (fun i -> *)
		classes <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			(* Printf.eprintf "  Read clsr %d of %d for %s.%s\n" i (l-1) mname tname; *)
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
		abstracts <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			(* Printf.eprintf "  Read absr %d of %d for abstract %s\n" i l tname; *)
			match resolve_type pack mname tname with
			| TAbstractDecl a ->
				a
			| _ ->
				error ("Unexpected type where abstract was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_enmr =
		let l = self#read_uleb128 in
		enums <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			Printf.eprintf "  Read enmr %d of %d for abstract %s\n" i l tname;
			match resolve_type pack mname tname with
			| TEnumDecl en ->
				en
			| _ ->
				error ("Unexpected type where enum was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_tpdr =
		let l = self#read_uleb128 in
		typedefs <- (Array.init l (fun i ->
			let (pack,mname,tname) = self#read_full_path in
			Printf.eprintf "  Read tpdr %d of %d for typedef %s.%s\n" i l mname tname;
			match resolve_type pack mname tname with
			| TTypeDecl tpd ->
				tpd
			| _ ->
				error ("Unexpected type where typedef was expected: " ^ (s_type_path (pack,tname)))
		))

	method read_typf (m : module_def) =
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
				error "Missing HHDR chunk"
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
					error ("Unexpected early chunk: " ^ (string_of_chunk_kind kind))
		in
		let m,chunks = pass_0 chunks in
		List.iter (fun (kind,data) ->
			Printf.eprintf " Reading chunk %s\n" (string_of_chunk_kind kind);
			ch <- IO.input_bytes data;
			match kind with
			| TYPF ->
				m.m_types <- self#read_typf m;
				add_module m;
			| CLSR ->
				self#read_clsr;
			| ABSR ->
				self#read_absr;
			| ENMR ->
				self#read_enmr;
			| TPDR ->
				self#read_tpdr;
			| ABSD ->
				self#read_absd m;
			| CLSD ->
				self#read_clsd m;
			| CFLD ->
				self#read_cfld m;
			| ENMD ->
				self#read_enmd m;
			| TPDD ->
				self#read_tpdd m;
			| _ ->
				error ("Unexpected late chunk: " ^ (string_of_chunk_kind kind))
		) chunks;
		m
end
