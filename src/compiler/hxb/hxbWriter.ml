open Globals
open Ast
open Type
open Genshared
open HxbData

type field_source =
	| ClassStatic of tclass
	| ClassMember of tclass
	| CLassConstructor of tclass

class ['key,'value] pool = object(self)
	val lut = Hashtbl.create 0
	val items = DynArray.create ()

	method add (key : 'key) (value : 'value) =
		let index = DynArray.length items in
		DynArray.add items value;
		Hashtbl.add lut key index;
		index

	method extract (key : 'key) =
		DynArray.get items (self#get key)

	method get (key : 'key) =
		Hashtbl.find lut key

	method get_or_add (key : 'key) (value : 'value) =
		try
			self#get key
		with Not_found ->
			self#add key value

	method is_empty =
		DynArray.length items = 0

	method to_list =
		DynArray.to_list items

	method items = items
end

class abstract_chunk
	(name : string) =
object(self)
	val ch = IO.output_bytes()

	(* Primitives *)

	method write_u8 v =
		IO.write_byte ch v

	method write_u32 v =
		IO.write_real_i32 ch v

	method write_f64 v =
		IO.write_double ch v

	method write_uleb128 v =
		let b = v land 0x7F in
		let rest = v lsr 7 in
		if rest = 0 then
			self#write_u8 b
		else begin
			self#write_u8 (b lor 0x80);
			self#write_uleb128 rest
		end

	method write_leb128 v =
		let b = v land 0x7F in
		let rest = v asr 7 in
		if (rest = 0 && (b land 0x40 = 0)) || (rest = -1 && (b land 0x40 = 0x40)) then
			self#write_u8 b
		else begin
			self#write_u8 (b lor 0x80);
			self#write_leb128 rest
		end

	method write_byte b =
		IO.write_byte ch b;

	method write_bytes b =
		self#write_uleb128 (Bytes.length b);
		IO.nwrite ch b;

	method write_bool b =
		self#write_byte (if b then 1 else 0)

	method write_ui16 i =
		IO.write_ui16 ch i;

	method write_i16 i =
		IO.write_i16 ch i;

	method write_i32 i =
		IO.write_real_i32 ch (Int32.of_int i);

	method write_float f =
		IO.write_double ch f

	method export : 'a . 'a IO.output -> unit = fun chex ->
		let bytes = IO.close_out ch in
		IO.write_real_i32 chex (Int32.of_int (Bytes.length bytes));
		IO.nwrite chex (Bytes.unsafe_of_string name);
		IO.nwrite chex bytes;
		let crc = Int32.of_int 0x1234567 in (* TODO *)
		IO.write_real_i32 chex crc
end

class string_pool (kind : chunk_kind) = object(self)
	inherit abstract_chunk (string_of_chunk_kind kind) as super

	val pool = new pool

	method get (s : string) =
		pool#get_or_add s s

	method is_empty =
		pool#is_empty

	method export : 'a . 'a IO.output -> unit = fun chex ->
		self#write_uleb128 (DynArray.length pool#items);
		DynArray.iter (fun s ->
			let b = Bytes.unsafe_of_string s in
			self#write_bytes b;
		) pool#items;
		super#export chex
end

class chunk
	(kind : chunk_kind)
	(cp : string_pool)
= object(self)
	inherit abstract_chunk (string_of_chunk_kind kind)

	method write_string s =
		self#write_uleb128 (cp#get s);

	method write_list : 'b . 'b list -> ('b -> unit) -> unit = fun l f ->
		self#write_uleb128 (List.length l);
		List.iter f l;

	method write_option : 'b . 'b option -> ('b -> unit) -> unit = fun v f -> match v with
		| None ->
			self#write_byte 0
		| Some v ->
			self#write_byte 1;
			f v

	method kind =
		kind
end

class ['a] hxb_writer
	(anon_id : Type.t tanon_identification)
= object(self)

	val chunks = DynArray.create ()
	val cp = new string_pool STRI
	val docs = new string_pool DOCS

	val mutable chunk = Obj.magic ()

	val classes = new pool
	val enums = new pool
	val typedefs = new pool
	val abstracts = new pool

	val fields = new pool

	val own_classes = new pool
	val own_abstracts = new pool

	val anons = new pool

	val type_param_lut = new pool
	val mutable type_type_parameters = new pool
	val mutable field_type_parameters = new pool

	(* Chunks *)

	method start_chunk (kind : chunk_kind) =
		let new_chunk = new chunk kind cp in
		DynArray.add chunks new_chunk;
		chunk <- new_chunk

	(* Basic compounds *)

	method write_path (path : path) =
		chunk#write_list (fst path) chunk#write_string;
		chunk#write_string (snd path);

	method write_full_path (pack : string list) (mname : string) (tname : string) =
		chunk#write_list pack chunk#write_string;
		chunk#write_string mname;
		chunk#write_string tname;

	method write_documentation (doc : doc_block) =
		chunk#write_option doc.doc_own (fun s ->
			chunk#write_uleb128 (docs#get s)
		);
		chunk#write_list doc.doc_inherited (fun s ->
			chunk#write_uleb128 (docs#get s)
		);

	method write_pos (p : pos) =
		chunk#write_string p.pfile;
		chunk#write_leb128 p.pmin;
		chunk#write_leb128 p.pmax;

	method write_metadata_entry ((meta,el,p) : metadata_entry) =
		chunk#write_string (Meta.to_string meta);
		(* TODO: el -_- *)
		self#write_pos p

	method write_metadata ml =
		chunk#write_list ml self#write_metadata_entry

	(* References *)

	method write_class_ref (c : tclass) =
		chunk#write_uleb128 (classes#get_or_add c.cl_path c)

	method write_enum_ref (en : tenum) =
		chunk#write_uleb128 (enums#get_or_add en.e_path en)

	method write_typedef_ref (td : tdef) =
		chunk#write_uleb128 (typedefs#get_or_add td.t_path td)

	method write_abstract_ref (a : tabstract) =
		chunk#write_uleb128 (abstracts#get_or_add a.a_path a)

	method write_field_ref (source : field_source) (cf : tclass_field) =
		chunk#write_string cf.cf_name

	(* Type instances *)

	method write_type_instance t =
		let write_function_arg (n,o,t) =
			chunk#write_string n;
			chunk#write_bool o;
			self#write_type_instance t;
		in
		match t with
		| TMono r ->
			begin match r.tm_type with
			| None ->
				chunk#write_byte 0
			| Some t ->
				chunk#write_byte 1;
				self#write_type_instance t
			end
		| TInst({cl_kind = KTypeParameter _} as c,[]) ->
			begin try
				let i = field_type_parameters#get (snd c.cl_path) in
				chunk#write_byte 5;
				chunk#write_uleb128 i
			with Not_found -> try
				let i = type_type_parameters#get (snd c.cl_path) in
				chunk#write_byte 6;
				chunk#write_uleb128 i
			with Not_found ->
				error ("Unbound type parameter " ^ (s_type_path c.cl_path))
			end
		| TInst(c,[]) ->
			chunk#write_byte 10;
			self#write_class_ref c;
		| TEnum(en,[]) ->
			chunk#write_byte 11;
			self#write_enum_ref en;
		| TType(td,[]) ->
			chunk#write_byte 12;
			self#write_typedef_ref td;
		| TAbstract(a,[]) ->
			chunk#write_byte 13;
			self#write_abstract_ref a;
		| TInst(c,tl) ->
			chunk#write_byte 14;
			self#write_class_ref c;
			self#write_types tl
		| TEnum(en,tl) ->
			chunk#write_byte 15;
			self#write_enum_ref en;
			self#write_types tl
		| TType(td,tl) ->
			chunk#write_byte 16;
			self#write_typedef_ref td;
			self#write_types tl
		| TAbstract(a,tl) ->
			chunk#write_byte 17;
			self#write_abstract_ref a;
			self#write_types tl
		(* | TFun([],t) when ExtType.is_void (follow t) ->
			chunk#write_byte 30;
		| TFun(args,t) when ExtType.is_void (follow t) ->
			chunk#write_byte 31;
			chunk#write_list args write_function_arg; *)
		| TFun(args,t) ->
			chunk#write_byte 32;
			chunk#write_list args write_function_arg;
			self#write_type_instance t;
		| TLazy r ->
			self#write_type_instance (lazy_type r);
		| TDynamic None ->
			chunk#write_byte 40
		| TDynamic (Some t) ->
			chunk#write_byte 41;
			self#write_type_instance t;
		| TAnon an when PMap.is_empty an.a_fields ->
			chunk#write_byte 50;
		| TAnon an ->
			let pfm = Option.get (anon_id#identify true t) in
			chunk#write_byte 51;
			chunk#write_uleb128 (anons#get_or_add pfm.pfm_path an)
			(* begin match !(an.a_status) with
			| Closed -> chunk#write_byte 50
			| Const -> chunk#write_byte 51
			| Extend _ -> chunk#write_byte 52
			| Statics _ -> chunk#write_byte 53
			| EnumStatics _ -> chunk#write_byte 54
			| AbstractStatics _ -> chunk#write_byte 55
			end; *)
			(* let l = pmap_to_list an.a_fields in
			(* chunk#write_list l (fun (_,cf) -> self#write_class_field cf); *)
			begin match !(an.a_status) with
			| Extend tl -> self#write_types tl
			| Statics c -> self#write_class_ref c
			| EnumStatics en -> self#write_enum_ref en
			| AbstractStatics a -> self#write_abstract_ref a
			| Closed
			| Const ->
				()
			end; *)

	method write_types tl =
		chunk#write_list tl self#write_type_instance

	(* Fields *)

	method set_field_type_parameters (params : typed_type_param list) =
		field_type_parameters <- new pool;
		List.iter (fun ttp ->
			ignore(field_type_parameters#add ttp.ttp_name ttp);
		) params

	method write_type_parameter_forward ttp = match follow ttp.ttp_type with
		| TInst({cl_kind = KTypeParameter _} as c,_) ->
			chunk#write_string ttp.ttp_name;
			self#write_pos c.cl_name_pos
		| _ ->
			die "" __LOC__

	method write_type_parameter_data ttp = match follow ttp.ttp_type with
		| TInst({cl_kind = KTypeParameter tl1},tl2) ->
			self#write_types tl1;
			self#write_types tl2;
		| _ ->
			die "" __LOC__

	method write_field_kind = function
		| Method MethNormal -> chunk#write_byte 0;
		| Method MethInline -> chunk#write_byte 1;
		| Method MethDynamic -> chunk#write_byte 2;
		| Method MethMacro -> chunk#write_byte 3;
		(* normal read *)
		| Var {v_read = AccNormal; v_write = AccNormal } -> chunk#write_byte 10
		| Var {v_read = AccNormal; v_write = AccNo } -> chunk#write_byte 11
		| Var {v_read = AccNormal; v_write = AccNever } -> chunk#write_byte 12
		| Var {v_read = AccNormal; v_write = AccCtor } -> chunk#write_byte 13
		| Var {v_read = AccNormal; v_write = AccCall } -> chunk#write_byte 14
		(* inline read *)
		| Var {v_read = AccInline; v_write = AccNever } -> chunk#write_byte 20
		(* getter read *)
		| Var {v_read = AccCall; v_write = AccNormal } -> chunk#write_byte 30
		| Var {v_read = AccCall; v_write = AccNo } -> chunk#write_byte 31
		| Var {v_read = AccCall; v_write = AccNever } -> chunk#write_byte 32
		| Var {v_read = AccCall; v_write = AccCtor } -> chunk#write_byte 33
		| Var {v_read = AccCall; v_write = AccCall } -> chunk#write_byte 34
		(* weird/overlooked combinations *)
		| Var {v_read = r;v_write = w } ->
			chunk#write_byte 100;
			let f = function
				| AccNormal -> chunk#write_byte 0
				| AccNo -> chunk#write_byte 1
				| AccNever -> chunk#write_byte 2
				| AccCtor -> chunk#write_byte 3
				| AccCall -> chunk#write_byte 4
				| AccInline -> chunk#write_byte 5
				| AccRequire(s,so) ->
					chunk#write_byte 6;
					chunk#write_string s;
					chunk#write_option so chunk#write_string
			in
			f r;
			f w;

	method write_class_field cf =
		self#set_field_type_parameters cf.cf_params;
		chunk#write_string cf.cf_name;
		chunk#write_list cf.cf_params self#write_type_parameter_forward;
		chunk#write_list cf.cf_params self#write_type_parameter_data;
		self#write_type_instance cf.cf_type;
		chunk#write_i32 cf.cf_flags;
		self#write_pos cf.cf_pos;
		self#write_pos cf.cf_name_pos;
		chunk#write_option cf.cf_doc self#write_documentation;
		self#write_metadata cf.cf_meta;
		self#write_field_kind cf.cf_kind;
		chunk#write_list cf.cf_overloads self#write_class_field;

	(* Module types *)

	method select_type (path : path) =
		type_type_parameters <- type_param_lut#extract path

	method write_common_module_type (infos : tinfos) : unit =
		chunk#write_bool infos.mt_private;
		chunk#write_option infos.mt_doc self#write_documentation;
		self#write_metadata infos.mt_meta;
		chunk#write_list infos.mt_params self#write_type_parameter_forward;
		chunk#write_list infos.mt_params self#write_type_parameter_data;
		chunk#write_list infos.mt_using (fun (c,p) ->
			self#write_class_ref c;
			self#write_pos p;
		);

	method write_class_kind = function
		| KNormal ->
			chunk#write_byte 0
		| KTypeParameter tl ->
			chunk#write_byte 1;
			self#write_types tl;
		| KExpr e ->
			chunk#write_byte 2;
			(* TODO *)
		| KGeneric ->
			chunk#write_byte 3;
		| KGenericInstance(c,tl) ->
			chunk#write_byte 4;
			self#write_class_ref c;
			self#write_types tl
		| KMacroType ->
			chunk#write_byte 5;
		| KGenericBuild l ->
			chunk#write_byte 6;
			(* TODO *)
		| KAbstractImpl a ->
			chunk#write_byte 7;
			self#write_abstract_ref a;
		| KModuleFields md ->
			chunk#write_byte 8;
			(* TODO *)

	method write_class (c : tclass) =
		begin match c.cl_kind with
		| KAbstractImpl a ->
			self#select_type a.a_path
		| _ ->
			self#select_type c.cl_path;
		end;
		self#write_common_module_type (Obj.magic c);
		self#write_class_kind c.cl_kind;
		chunk#write_u32 (Int32.of_int c.cl_flags);
		chunk#write_option c.cl_super (fun (c,tl) ->
			self#write_class_ref c;
			self#write_types tl
		);
		chunk#write_list c.cl_implements (fun (c,tl) ->
			self#write_class_ref c;
			self#write_types tl
		);
		chunk#write_option c.cl_dynamic self#write_type_instance;
		chunk#write_option c.cl_array_access self#write_type_instance;

	method write_abstract (a : tabstract) =
		begin try
			self#select_type a.a_path
		with Not_found ->
			print_endline ("Could not select abstract " ^ (s_type_path a.a_path));
		end;
		self#write_common_module_type (Obj.magic a);
		(* ops *)
		(* unops *)
		chunk#write_option a.a_impl self#write_class_ref;
		let c = match a.a_impl with
			| None ->
				null_class
			| Some c ->
				c
		in
		self#write_type_instance a.a_this;
		chunk#write_list a.a_from self#write_type_instance;
		chunk#write_list a.a_from_field (fun (t,cf) ->
			self#set_field_type_parameters cf.cf_params;
			self#write_type_instance t;
			self#write_field_ref (ClassStatic c) cf;
		);
		chunk#write_list a.a_to self#write_type_instance;
		chunk#write_list a.a_to_field (fun (t,cf) ->
			self#set_field_type_parameters cf.cf_params;
			self#write_type_instance t;
			self#write_field_ref (ClassStatic c) cf;
		);
		chunk#write_list a.a_array (self#write_field_ref (ClassStatic c));
		chunk#write_option a.a_read (self#write_field_ref (ClassStatic c));
		chunk#write_option a.a_write (self#write_field_ref (ClassStatic c));
		chunk#write_option a.a_call (self#write_field_ref (ClassStatic c));
		chunk#write_bool a.a_enum

	(* Module *)

	method forward_declare_type (mt : module_type) =
		let i = match mt with
		| TClassDecl c ->
			ignore(classes#add c.cl_path c);
			ignore(own_classes#add c.cl_path c);
			0
		| TEnumDecl _ ->
			1
		| TTypeDecl _ ->
			2
		| TAbstractDecl a ->
			ignore(abstracts#add a.a_path a);
			ignore(own_abstracts#add a.a_path a);
			3
		in
		let infos = t_infos mt in
		chunk#write_byte i;
		self#write_path infos.mt_path;
		self#write_pos infos.mt_pos;
		self#write_pos infos.mt_name_pos;
		let params = new pool in
		type_type_parameters <- params;
		ignore(type_param_lut#add infos.mt_path params);
		List.iter (fun ttp ->
			ignore(type_type_parameters#add ttp.ttp_name ttp);
		) infos.mt_params;

	method write_module (m : module_def) =
		self#start_chunk HHDR;
		self#write_path m.m_path;
		chunk#write_string (Path.UniqueKey.lazy_path m.m_extra.m_file);

		self#start_chunk TYPF;
		chunk#write_list m.m_types self#forward_declare_type;

		begin match own_classes#to_list with
		| [] ->
			()
		| own_classes ->
			self#start_chunk CLSD;
			chunk#write_list own_classes self#write_class;
			self#start_chunk CFLD;
			chunk#write_list own_classes (fun c ->
				begin match c.cl_kind with
				| KAbstractImpl a ->
					self#select_type a.a_path
				| _ ->
					self#select_type c.cl_path;
				end;
				chunk#write_option c.cl_constructor self#write_class_field;
				chunk#write_list c.cl_ordered_fields self#write_class_field;
				chunk#write_list c.cl_ordered_statics self#write_class_field;
			)
		end;
		begin match own_abstracts#to_list with
		| [] ->
			()
		| own_abstracts ->
			self#start_chunk ABSD;
			chunk#write_list own_abstracts self#write_abstract;
		end;
		begin match classes#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk CLSR;
			chunk#write_list l (fun c ->
				let m = c.cl_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd c.cl_path)
			)
		end;
		begin match abstracts#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ABSR;
			chunk#write_list l (fun a ->
				let m = a.a_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd a.a_path)
			)
		end;
		self#start_chunk HEND;

	(* Export *)

	method export : 'a . 'a IO.output -> unit = fun ch ->
		cp#export ch;
		if not docs#is_empty then
			docs#export ch;
		let l = DynArray.to_list chunks in
		let l = List.sort (fun chunk1 chunk2 ->
			(Obj.magic chunk1#kind) - (Obj.magic chunk2#kind)
		) l in
		List.iter (fun (chunk : chunk) ->
			chunk#export ch
		) l
end

(*
class hxb_constant_pool_writer = object(self)
	val lut = Hashtbl.create 0
	val pool = DynArray.create ()

	method get_index (s : string) =
		try
			Hashtbl.find lut s
		with Not_found ->
			let index = DynArray.length pool in
			Hashtbl.add lut s index;
			DynArray.add pool s;
			index

	method export : 'a . 'a IO.output -> unit = fun ch ->
		IO.write_real_i32 ch (Int32.of_int (DynArray.length pool));
		DynArray.iter (fun s ->
			let b = Bytes.of_string s in
			IO.write_real_i32 ch (Int32.of_int (Bytes.length b));
			IO.nwrite ch b;
		) pool;
end

let pmap_to_list map = PMap.foldi (fun k x l -> (k,x) :: l) map []
let hashtbl_to_list h = Hashtbl.fold (fun k x l -> (k,x) :: l) h []

let rec binop_index op = match op with
	| OpAdd -> 0
	| OpMult -> 1
	| OpDiv -> 2
	| OpSub -> 3
	| OpAssign -> 4
	| OpEq -> 5
	| OpNotEq -> 6
	| OpGt -> 7
	| OpGte -> 8
	| OpLt -> 9
	| OpLte -> 10
	| OpAnd -> 11
	| OpOr -> 12
	| OpXor -> 13
	| OpBoolAnd -> 14
	| OpBoolOr -> 15
	| OpShl -> 16
	| OpShr -> 17
	| OpUShr -> 18
	| OpMod -> 19
	| OpInterval -> 20
	| OpArrow -> 21
	| OpIn -> 22
	| OpAssignOp op -> 30 + binop_index op

let unop_index op flag = match op,flag with
	| Increment,Prefix -> 0
	| Decrement,Prefix -> 1
	| Not,Prefix -> 2
	| Neg,Prefix -> 3
	| NegBits,Prefix -> 4
	| Increment,Postfix -> 5
	| Decrement,Postfix -> 6
	| Not,Postfix -> 7
	| Neg,Postfix -> 8
	| NegBits,Postfix -> 9

class ['a] hxb_writer (ch : 'a IO.output) (cp : hxb_constant_pool_writer) = object(self)

	(* basic *)

	method write_byte b =
		IO.write_byte ch b;

	method write_bool b =
		chunk#write_byte (if b then 1 else 0)

	method write_ui16 i =
		IO.write_ui16 ch i;

	method write_i16 i =
		IO.write_i16 ch i;

	method write_i32 i =
		IO.write_real_i32 ch (Int32.of_int i);

	method write_float f =
		IO.write_double ch f

	method write_string s =
		self#write_i32 (cp#get_index s);

	method write_bytes b =
		self#write_i32 (Bytes.length b);
		IO.nwrite ch b;

	method write_list8 : 'b . 'b list -> ('b -> unit) -> unit = fun l f ->
		chunk#write_byte (List.length l);
		List.iter f l;

	method write_list16 : 'b . 'b list -> ('b -> unit) -> unit = fun l f ->
		self#write_ui16 (List.length l);
		List.iter f l;

	method write_option : 'b . 'b option -> ('b -> unit) -> unit = fun v f -> match v with
		| None -> chunk#write_byte 0
		| Some v ->
			chunk#write_byte 1;
			f v

	method write_path (path : path) =
		self#write_list8 (fst path) chunk#write_string;
		chunk#write_string (snd path);

	method write_documentation (doc : doc_block) =
		chunk#write_option doc.doc_own chunk#write_string;
		self#write_list8 doc.doc_inherited chunk#write_string

	(* basic compounds *)

	method write_pos p =
		chunk#write_string p.pfile;
		self#write_i32 p.pmin;
		self#write_i32 p.pmax;

	method write_metadata_entry ((meta,el,p) : metadata_entry) =
		chunk#write_string (Meta.to_string meta);
		(* TODO: el -_- *)
		self#write_pos p

	method write_metadata ml =
		self#write_list16 ml self#write_metadata_entry

	method write_type_params params =
		self#write_list16 params (fun (s,t) ->
			chunk#write_string s;
			match follow t with
			| TInst({cl_kind = KTypeParameter tl},_) ->
				self#write_types tl;
			| _ ->
				assert false
		)

	(* refs *)

	method write_class_ref c =
		self#write_path c.cl_path

	method write_enum_ref en =
		self#write_path en.e_path

	method write_typedef_ref td =
		self#write_path td.t_path

	method write_abstract_ref a =
		self#write_path a.a_path

	method write_field_ref cf =
		chunk#write_string cf.cf_name

	method write_enum_field_ref ef =
		chunk#write_string ef.ef_name

	(* type instance *)

	method write_type_instance t =
		let write_function_arg (n,o,t) =
			chunk#write_string n;
			chunk#write_bool o;
			self#write_type_instance t;
		in
		match t with
		| TMono r ->
			begin match r.tm_type with
			| None -> chunk#write_byte 0
			| Some t ->
				chunk#write_byte 1;
				self#write_type_instance t
			end
		| TInst(c,[]) ->
			chunk#write_byte 10;
			self#write_class_ref c;
		| TEnum(en,[]) ->
			chunk#write_byte 11;
			self#write_enum_ref en;
		| TType(td,[]) ->
			chunk#write_byte 12;
			self#write_typedef_ref td;
		| TAbstract(a,[]) ->
			chunk#write_byte 13;
			self#write_abstract_ref a;
		| TInst(c,tl) ->
			chunk#write_byte 14;
			self#write_class_ref c;
			self#write_types tl
		| TEnum(en,tl) ->
			chunk#write_byte 15;
			self#write_enum_ref en;
			self#write_types tl
		| TType(td,tl) ->
			chunk#write_byte 16;
			self#write_typedef_ref td;
			self#write_types tl
		| TAbstract(a,tl) ->
			chunk#write_byte 17;
			self#write_abstract_ref a;
			self#write_types tl
		| TFun([],t) when ExtType.is_void (follow t) ->
			chunk#write_byte 30;
		| TFun(args,t) when ExtType.is_void (follow t) ->
			chunk#write_byte 31;
			self#write_list16 args write_function_arg;
		| TFun(args,t) ->
			chunk#write_byte 32;
			self#write_list16 args write_function_arg;
		| TLazy r ->
			self#write_type_instance (lazy_type r);
		| TDynamic t ->
			if t == t_dynamic then chunk#write_byte 40
			else begin
				chunk#write_byte 41;
				self#write_type_instance t;
			end
		| TAnon an ->
			begin match !(an.a_status) with
			| Closed -> chunk#write_byte 50
			| Const -> chunk#write_byte 51
			| Extend _ -> chunk#write_byte 52
			| Statics _ -> chunk#write_byte 53
			| EnumStatics _ -> chunk#write_byte 54
			| AbstractStatics _ -> chunk#write_byte 55
			end;
			let l = pmap_to_list an.a_fields in
			self#write_list16 l (fun (_,cf) -> self#write_class_field cf);
			begin match !(an.a_status) with
			| Extend tl -> self#write_types tl
			| Statics c -> self#write_class_ref c
			| EnumStatics en -> self#write_enum_ref en
			| AbstractStatics a -> self#write_abstract_ref a
			| Closed
			| Const ->
				()
			end;

	method write_types tl =
		self#write_list16 tl self#write_type_instance

	(* texpr *)

	method write_var_kind vk =
		let b = match vk with
			| VUser TVOLocalVariable -> 0
			| VUser TVOArgument -> 1
			| VUser TVOForVariable -> 2
			| VUser TVOPatternVariable -> 3
			| VUser TVOCatchVariable -> 4
			| VUser TVOLocalFunction -> 5
			| VGenerated -> 6
			| VInlined -> 7
			| VInlinedConstructorVariable -> 8
			| VExtractorVariable -> 9
		in
		chunk#write_byte b

	method write_var v =
		self#write_i32 v.v_id;
		chunk#write_string v.v_name;
		self#write_type_instance v.v_type;
		self#write_var_kind v.v_kind;
		(* chunk#write_bool v.v_capture;
		chunk#write_bool v.v_final;
		chunk#write_option v.v_extra (fun (tl,eo) ->
			self#write_type_params tl;
			chunk#write_option eo self#write_texpr;
		); *)
		self#write_metadata v.v_meta;
		self#write_pos v.v_pos;

	method write_texpr (e : texpr) =
		self#write_pos e.epos;
		let curmin = ref e.epos.pmin in
		let curmax = ref e.epos.pmax in
		let check_diff p =
			let dmin = p.pmin - !curmin in
			let dmax = p.pmax - !curmax in
			self#write_i16 dmin;
			self#write_i16 dmax;
			curmin := p.pmin;
			curmax := p.pmax;
		in
		let rec loop e =
			self#write_type_instance e.etype;
			check_diff e.epos;
			match e.eexpr with
			(* values 0-19 *)
			| TConst ct ->
				begin match ct with
				| TNull ->
					chunk#write_byte 0;
				| TThis ->
					chunk#write_byte 1;
				| TSuper ->
					chunk#write_byte 2;
				| TBool false ->
					chunk#write_byte 3;
				| TBool true ->
					chunk#write_byte 4;
				| TInt i32 ->
					chunk#write_byte 5;
					IO.write_real_i32 ch i32;
				| TFloat f ->
					chunk#write_byte 6;
					chunk#write_string f;
				| TString s ->
					chunk#write_byte 7;
					chunk#write_string s
				end
			(* vars 20-29 *)
			| TLocal v ->
				chunk#write_byte 20;
				self#write_i32 v.v_id;
			| TVar(v,None) ->
				chunk#write_byte 21;
				self#write_var v;
			| TVar(v,Some e1) ->
				chunk#write_byte 22;
				self#write_var v;
				loop e1;
			(* blocks 30-49 *)
			| TBlock [] ->
				chunk#write_byte 30;
			| TBlock el ->
				let l = List.length el in
				begin match l with
				| 1 -> chunk#write_byte 31;
				| 2 -> chunk#write_byte 32;
				| 3 -> chunk#write_byte 33;
				| 4 -> chunk#write_byte 34;
				| 5 -> chunk#write_byte 35;
				| _ ->
					if l <= 0xFF then begin
						chunk#write_byte 36;
						chunk#write_byte l;
					end else if l < 0xFFFF then begin
						chunk#write_byte 37;
						self#write_ui16 l;
					end else begin
						chunk#write_byte 38;
						self#write_i32 l;
					end;
				end;
				List.iter loop el
			(* function 50-59 *)
			| TFunction tf ->
				chunk#write_byte 50;
				self#write_list16 tf.tf_args (fun (v,eo) ->
					self#write_var v;
					chunk#write_option eo loop
				);
				self#write_type_instance tf.tf_type;
				loop tf.tf_expr;
			(* texpr compounds 60-79 *)
			| TArray(e1,e2) ->
				chunk#write_byte 60;
				loop e1;
				loop e2;
			| TParenthesis e1 ->
				chunk#write_byte 61;
				loop e1;
			| TArrayDecl el ->
				chunk#write_byte 62;
				loop_el el;
			| TObjectDecl fl ->
				chunk#write_byte 63;
				self#write_list16 fl (fun ((name,p,qs),e) ->
					chunk#write_string name;
					self#write_pos p;
					begin match qs with
					| NoQuotes -> chunk#write_byte 0;
					| DoubleQuotes -> chunk#write_byte 1;
					end;
					loop e
				);
			| TCall(e1,el) ->
				chunk#write_byte 64;
				loop e1;
				loop_el el;
			| TMeta(m,e1) ->
				chunk#write_byte 65;
				self#write_metadata_entry m;
				loop e1;
			(* branching 80-89 *)
			| TIf(e1,e2,None) ->
				chunk#write_byte 80;
				loop e1;
				loop e2;
			| TIf(e1,e2,Some e3) ->
				chunk#write_byte 81;
				loop e1;
				loop e2;
				loop e3;
			| TSwitch(e1,cases,def) ->
				chunk#write_byte 82;
				loop e1;
				self#write_list16 cases (fun (el,e) ->
					loop_el el;
					loop e;
				);
				chunk#write_option def loop;
			| TTry(e1,catches) ->
				chunk#write_byte 83;
				loop e1;
				self#write_list16 catches  (fun (v,e) ->
					self#write_var v;
					loop e
				);
			| TWhile(e1,e2,flag) ->
				chunk#write_byte (if flag = NormalWhile then 84 else 85);
				loop e1;
				loop e2;
			| TFor(v,e1,e2) ->
				chunk#write_byte 86;
				self#write_var v;
				loop e1;
				loop e2;
			(* control flow 90-99 *)
			| TReturn None ->
				chunk#write_byte 90;
			| TReturn (Some e1) ->
				chunk#write_byte 91;
				loop e1;
			| TContinue ->
				chunk#write_byte 92;
			| TBreak ->
				chunk#write_byte 93;
			| TThrow e1 ->
				chunk#write_byte 94;
				loop e1;
			(* access 100-119 *)
			| TEnumIndex e1 ->
				chunk#write_byte 100;
				loop e1;
			| TEnumParameter(e1,ef,i) ->
				chunk#write_byte 101;
				loop e1;
				self#write_enum_field_ref ef;
				self#write_i32 i;
			| TField(e1,FInstance(c,tl,cf)) ->
				chunk#write_byte 102;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref cf;
			| TField(e1,FStatic(c,cf)) ->
				chunk#write_byte 103;
				loop e1;
				self#write_class_ref c;
				self#write_field_ref cf;
			| TField(e1,FAnon cf) ->
				chunk#write_byte 104;
				loop e1;
				self#write_field_ref cf;
			| TField(e1,FClosure(Some(c,tl),cf)) ->
				chunk#write_byte 105;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref cf;
			| TField(e1,FClosure(None,cf)) ->
				chunk#write_byte 106;
				loop e1;
				self#write_field_ref cf;
			| TField(e1,FEnum(en,ef)) ->
				chunk#write_byte 107;
				loop e1;
				self#write_enum_ref en;
				self#write_enum_field_ref ef;
			| TField(e1,FDynamic s) ->
				chunk#write_byte 108;
				loop e1;
				chunk#write_string s;
			(* module types 120-139 *)
			| TTypeExpr (TClassDecl c) ->
				chunk#write_byte 120;
				self#write_class_ref c;
			| TTypeExpr (TEnumDecl en) ->
				chunk#write_byte 121;
				self#write_enum_ref en;
			| TTypeExpr (TAbstractDecl a) ->
				chunk#write_byte 122;
				self#write_abstract_ref a
			| TTypeExpr (TTypeDecl td) ->
				chunk#write_byte 123;
				self#write_typedef_ref td
			| TCast(e1,None) ->
				chunk#write_byte 124;
				loop e1;
			| TCast(e1,Some md) ->
				chunk#write_byte 125;
				loop e1;
				self#write_path (t_infos md).mt_path
			| TNew(c,tl,el) ->
				chunk#write_byte 126;
				self#write_class_ref c;
				self#write_types tl;
				loop_el el;
			(* unops 140-159 *)
			| TUnop(op,flag,e1) ->
				chunk#write_byte (140 + unop_index op flag);
				loop e1;
			(* binops 160-219 *)
			| TBinop(op,e1,e2) ->
				chunk#write_byte (160 + binop_index op);
				loop e1;
				loop e2;
			(* rest 250-254 *)
			| TIdent s ->
				chunk#write_byte 250;
				chunk#write_string s;
		and loop_el el =
			self#write_ui16 (List.length el);
			List.iter loop el
		in
		loop e

	(* field *)

	method write_field_kind = function
		| Method MethNormal -> chunk#write_byte 0;
		| Method MethInline -> chunk#write_byte 1;
		| Method MethDynamic -> chunk#write_byte 2;
		| Method MethMacro -> chunk#write_byte 3;
		(* normal read *)
		| Var {v_read = AccNormal; v_write = AccNormal } -> chunk#write_byte 10
		| Var {v_read = AccNormal; v_write = AccNo } -> chunk#write_byte 11
		| Var {v_read = AccNormal; v_write = AccNever } -> chunk#write_byte 12
		| Var {v_read = AccNormal; v_write = AccCtor } -> chunk#write_byte 13
		| Var {v_read = AccNormal; v_write = AccCall } -> chunk#write_byte 14
		(* inline read *)
		| Var {v_read = AccInline; v_write = AccNever } -> chunk#write_byte 20
		(* getter read *)
		| Var {v_read = AccCall; v_write = AccNormal } -> chunk#write_byte 30
		| Var {v_read = AccCall; v_write = AccNo } -> chunk#write_byte 31
		| Var {v_read = AccCall; v_write = AccNever } -> chunk#write_byte 32
		| Var {v_read = AccCall; v_write = AccCtor } -> chunk#write_byte 33
		| Var {v_read = AccCall; v_write = AccCall } -> chunk#write_byte 34
		(* weird/overlooked combinations *)
		| Var {v_read = r;v_write = w } ->
			chunk#write_byte 100;
			let f = function
				| AccNormal -> chunk#write_byte 0
				| AccNo -> chunk#write_byte 1
				| AccNever -> chunk#write_byte 2
				| AccCtor -> chunk#write_byte 3
				| AccCall -> chunk#write_byte 4
				| AccInline -> chunk#write_byte 5
				| AccRequire(s,so) ->
					chunk#write_byte 6;
					chunk#write_string s;
					chunk#write_option so chunk#write_string
			in
			f r;
			f w;

	method write_class_field cf =
		self#write_field_ref cf;
		self#write_i32 cf.cf_flags;
		self#write_type_instance cf.cf_type;
		self#write_pos cf.cf_pos;
		self#write_pos cf.cf_name_pos;
		chunk#write_option cf.cf_doc self#write_documentation;
		self#write_metadata cf.cf_meta;
		self#write_type_params cf.cf_params;
		self#write_field_kind cf.cf_kind;
		chunk#write_option cf.cf_expr self#write_texpr;
		(* TODO: expr_unoptimized *)
		self#write_list16 cf.cf_overloads self#write_class_field;

	method write_enum_field ef =
		self#write_enum_field_ref ef;
		self#write_type_instance ef.ef_type;
		self#write_pos ef.ef_pos;
		self#write_pos ef.ef_name_pos;
		chunk#write_option ef.ef_doc self#write_documentation;
		self#write_i32 ef.ef_index;
		self#write_type_params ef.ef_params;
		self#write_metadata ef.ef_meta;

	(* module *)

	method write_class_kind = function
		| KNormal ->
			chunk#write_byte 0
		| KTypeParameter tl ->
			chunk#write_byte 1;
			self#write_types tl;
		| KExpr e ->
			chunk#write_byte 2;
			(* TODO *)
		| KGeneric ->
			chunk#write_byte 3;
		| KGenericInstance(c,tl) ->
			chunk#write_byte 4;
			self#write_class_ref c;
			self#write_types tl
		| KMacroType ->
			chunk#write_byte 5;
		| KGenericBuild l ->
			chunk#write_byte 6;
			(* TODO *)
		| KAbstractImpl a ->
			chunk#write_byte 7;
			self#write_abstract_ref a;
		| KModuleFields md ->
			chunk#write_byte 8;
			(* TODO *)

	method write_module_type mt =
		let infos = t_infos mt in
		self#write_path infos.mt_path;
		self#write_pos infos.mt_pos;
		self#write_pos infos.mt_name_pos;
		chunk#write_bool infos.mt_private;
		chunk#write_option infos.mt_doc self#write_documentation;
		self#write_metadata infos.mt_meta;
		self#write_type_params infos.mt_params;
		self#write_list8 infos.mt_using (fun (c,p) ->
			self#write_class_ref c;
			self#write_pos p;
		);
		match mt with
		| TClassDecl c ->
			chunk#write_byte 0;
			self#write_class_kind c.cl_kind;
			(* TODO *)
			(* chunk#write_bool c.cl_extern;
			chunk#write_bool c.cl_final;
			chunk#write_bool c.cl_interface; *)
			let write_relation (cr,tl) =
				self#write_class_ref cr;
				self#write_types tl;
			in
			chunk#write_option c.cl_super write_relation;
			self#write_list16 c.cl_implements write_relation;
			self#write_list16 c.cl_ordered_statics self#write_class_field;
			self#write_list16 c.cl_ordered_fields self#write_class_field;
			chunk#write_option c.cl_dynamic self#write_type_instance;
			chunk#write_option c.cl_array_access self#write_type_instance;
			chunk#write_option c.cl_constructor self#write_class_field;
			chunk#write_option c.cl_init self#write_texpr;
		| TEnumDecl en ->
			chunk#write_byte 1;
			self#write_module_type (TTypeDecl en.e_type);
			chunk#write_bool en.e_extern;
			self#write_list16 en.e_names (fun s ->
				let ef = PMap.find s en.e_constrs in
				self#write_enum_field ef;
			);
		| TTypeDecl td ->
			chunk#write_byte 2;
			self#write_type_instance td.t_type;
		| TAbstractDecl a ->
			chunk#write_byte 3;
			self#write_list16 a.a_ops (fun (op,cf) ->
				chunk#write_byte (binop_index op);
				self#write_field_ref cf
			);
			self#write_list16 a.a_unops (fun (op,flag,cf) ->
				chunk#write_byte (unop_index op flag);
				self#write_field_ref cf;
			);
			chunk#write_option a.a_impl self#write_class_ref;
			self#write_type_instance a.a_this;
			self#write_types a.a_from;
			self#write_list16 a.a_from_field (fun (t,cf) ->
				self#write_type_instance t;
				self#write_field_ref cf
			);
			self#write_types a.a_to;
			self#write_list16 a.a_to_field (fun (t,cf) ->
				self#write_type_instance t;
				self#write_field_ref cf
			);
			self#write_list16 a.a_array self#write_field_ref;
			chunk#write_option a.a_read self#write_field_ref;
			chunk#write_option a.a_write self#write_field_ref;

	method write_module m =
		self#write_i32 m.m_id;
		self#write_path m.m_path;
		self#write_list16 m.m_types self#write_module_type;
		let extra = m.m_extra in
		chunk#write_string (Path.UniqueKey.lazy_path extra.m_file);
		chunk#write_string (Digest.to_hex extra.m_sign);
		self#write_list16 extra.m_display.m_inline_calls (fun (p1,p2) ->
			self#write_pos p1;
			self#write_pos p2;
		);
		(* TODO *)
		(* self#write_list16 extra.m_display.m_type_hints (fun (p,t) ->
			self#write_pos p;
			self#write_type_instance t;
		); *)
		self#write_list8 extra.m_check_policy (fun pol -> chunk#write_byte (Obj.magic pol)); (* TODO: don't be lazy *)
		self#write_float extra.m_time;
		(* chunk#write_option extra.m_dirty (fun m -> self#write_path m.m_path); *) (* TODO *)
		self#write_i32 extra.m_added;
		self#write_i32 extra.m_mark;
		self#write_list16 (pmap_to_list extra.m_deps) (fun (i,m) ->
			self#write_i32 i;
			self#write_path m.m_path;
		);
		self#write_i32 extra.m_processed;
		chunk#write_byte (Obj.magic extra.m_kind); (* TODO: don't be lazy *)
		self#write_list16 (pmap_to_list extra.m_binded_res) (fun (s1,s2) ->
			chunk#write_string s1;
			chunk#write_bytes (Bytes.unsafe_of_string s2);
		);
		self#write_list16 extra.m_if_feature (fun (s,(c,cf,b)) ->
			chunk#write_string s;
			self#write_class_ref c;
			self#write_field_ref cf;
			chunk#write_bool b;
		);
		self#write_list16 (hashtbl_to_list extra.m_features) (fun (s,b) ->
			chunk#write_string s;
			chunk#write_bool b;
		);
end *)