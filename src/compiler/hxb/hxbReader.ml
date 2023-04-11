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

	val mutable type_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)
	val mutable field_type_parameters = Array.make 0 (mk_type_param "" t_dynamic None)

	(* Primitives *)

	method read_u8 =
		IO.read_byte ch

	method read_u32 =
		IO.read_real_i32 ch

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

	method read_string =
		let l = self#read_uleb128 in
		string_pool.(l)

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
		(pack,mname,tname)

	method read_documentation =
		let doc_own = self#read_option (fun () ->
			doc_pool.(self#read_uleb128)
		) in
		let doc_inherited = self#read_list (fun () ->
			doc_pool.(self#read_uleb128)
		) in
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
		(Meta.parse name,[],p)

	method read_metadata =
		self#read_list (fun () -> self#read_metadata_entry)

	(* References *)

	method read_class_ref =
		classes.(self#read_uleb128)

	method read_abstract_ref =
		abstracts.(self#read_uleb128)

	method read_field_ref =
		null_field (* TODO *)

	(* Type instances *)

	method read_type_instance =
		match self#read_u8 with
		| 0 ->
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
			t_dynamic (* TODO *)
		| 32 ->
			let f () =
				let name = self#read_string in
				let opt = self#read_bool in
				let t = self#read_type_instance in
				(name,opt,t)
			in
			let args = self#read_list f in
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
			t_dynamic (* TODO *)
		| i ->
			error (Printf.sprintf "Bad type instance id: %i" i)

	method read_types =
		self#read_list (fun () -> self#read_type_instance)

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

	method read_class_field (m : module_def) : tclass_field =
		let name = self#read_string in
		self#read_type_parameters m ([],name) (fun a ->
			field_type_parameters <- a
		);
		let params = Array.to_list field_type_parameters in
		let t = self#read_type_instance in
		let flags = Int32.to_int self#read_u32 in
		let pos = self#read_pos in
		let name_pos = self#read_pos in
		let doc = self#read_option (fun () -> self#read_documentation) in
		let meta = self#read_metadata in
		let kind = self#read_field_kind in
		let overloads = self#read_list (fun () -> self#read_class_field m) in
		{
			cf_name = name;
			cf_type = t;
			cf_pos = pos;
			cf_name_pos = name_pos;
			cf_doc = doc;
			cf_meta = meta;
			cf_kind = kind;
			cf_expr = None;
			cf_expr_unoptimized = None;
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
		c.cl_constructor <- self#read_option f;
		c.cl_ordered_fields <- self#read_list f;
		c.cl_ordered_statics <- self#read_list f;
		List.iter (fun cf -> c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics) c.cl_ordered_statics;

	(* Module types *)

	method read_common_module_type (m : module_def) (infos : tinfos) =
		infos.mt_private <- self#read_bool;
		infos.mt_doc <- self#read_option (fun () -> self#read_documentation);
		infos.mt_meta <- self#read_metadata;
		self#read_type_parameters m infos.mt_path (fun a ->
			type_type_parameters <- a
		);
		infos.mt_params <- Array.to_list type_type_parameters;
		infos.mt_using <- self#read_list (fun () ->
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
		c.cl_implements <- self#read_list read_relation;
		c.cl_dynamic <- self#read_option (fun () -> self#read_type_instance);
		c.cl_array_access <- self#read_option (fun () -> self#read_type_instance);

	method read_abstract (m : module_def) (a : tabstract) =
		self#read_common_module_type m (Obj.magic a);
		a.a_impl <- self#read_option (fun () -> self#read_class_ref);
		a.a_this <- self#read_type_instance;
		a.a_from <- self#read_list (fun () -> self#read_type_instance);
		a.a_from_field <- self#read_list (fun () ->
			let t = self#read_type_instance in
			let cf = self#read_field_ref in
			(t,cf)
		);
		a.a_to <- self#read_list (fun () -> self#read_type_instance);
		a.a_to_field <- self#read_list (fun () ->
			let t = self#read_type_instance in
			let cf = self#read_field_ref in
			(t,cf)
		);
		a.a_array <- self#read_list (fun () -> self#read_field_ref);
		a.a_read <- self#read_option (fun () -> self#read_field_ref);
		a.a_write <- self#read_option (fun () -> self#read_field_ref);
		a.a_call <- self#read_option (fun () -> self#read_field_ref);
		a.a_enum <- self#read_bool

	(* Chunks *)

	method read_string_pool =
		let l = self#read_uleb128 in
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
		classes <- Array.init l (fun _ ->
			let (pack,mname,tname) = self#read_full_path in
			match resolve_type pack mname tname with
			| TClassDecl c ->
				c
			| _ ->
				error ("Unexpected type where class was expected: " ^ (s_type_path (pack,tname)))
		)

	method read_absr =
		let l = self#read_uleb128 in
		abstracts <- Array.init l (fun _ ->
			let (pack,mname,tname) = self#read_full_path in
			match resolve_type pack mname tname with
			| TAbstractDecl a ->
				a
			| _ ->
				error ("Unexpected type where class was expected: " ^ (s_type_path (pack,tname)))
		)

	method read_typf (m : module_def) =
		self#read_list (fun () ->
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
		if (Bytes.to_string (IO.nread ch 3)) <> "hxb" then
			raise (HxbFailure "magic");
		let version = self#read_u8 in
		ignore(version);
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
				raise (HxbFailure "Missing HHDR chunk")
			| (kind,data) :: chunks ->
				ch <- IO.input_bytes data;
				match kind with
				| HHDR ->
					let m = self#read_hhdr in
					m,chunks
				| STRI ->
					string_pool <- self#read_string_pool;
					pass_0 chunks
				| DOCS ->
					doc_pool <- self#read_string_pool;
					pass_0 chunks
				| _ ->
					raise (HxbFailure ("Unexpected early chunk: " ^ (string_of_chunk_kind kind)))
		in
		let m,chunks = pass_0 chunks in
		List.iter (fun (kind,data) ->
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
				self#read_cfld m;
			| ABSD ->
				self#read_absd m;
			| _ ->
				raise (HxbFailure ("Unexpected late chunk: " ^ (string_of_chunk_kind kind)))
		) chunks;
		m
end