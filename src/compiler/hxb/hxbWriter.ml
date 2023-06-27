open Globals
open Ast
open Type
open HxbData
open Tanon_identification

(* Debug utils *)
let no_color = false
let c_reset = if no_color then "" else "\x1b[0m"
let c_bold = if no_color then "" else "\x1b[1m"
let c_dim = if no_color then "" else "\x1b[2m"
let todo = "\x1b[33m[TODO]" ^ c_reset
let todo_error = "\x1b[41m[TODO] error:" ^ c_reset

type field_source =
	| ClassStatic of tclass
	| ClassMember of tclass
	| CLassConstructor of tclass

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
	| OpNullCoal -> 23
	| OpAssignOp op -> 30 + binop_index op

let unop_index op flag = match op,flag with
	| Increment,Prefix -> 0
	| Decrement,Prefix -> 1
	| Not,Prefix -> 2
	| Neg,Prefix -> 3
	| NegBits,Prefix -> 4
	| Spread,Prefix -> 5
	| Increment,Postfix -> 6
	| Decrement,Postfix -> 7
	| Not,Postfix -> 8
	| Neg,Postfix -> 9
	| NegBits,Postfix -> 10
	| Spread,Postfix -> 11

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

	method has (key : 'key) =
		Hashtbl.mem lut key

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

	method !export : 'a . 'a IO.output -> unit = fun chex ->
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
	val anons = new pool
	val anon_fields = new pool

	val own_classes = new pool
	val own_abstracts = new pool
	val own_enums = new pool
	val own_typedefs = new pool

	val type_param_lut = new pool
	val mutable ttp_key = ([],"")
	val mutable type_type_parameters = new pool
	val mutable field_type_parameters = new pool

	(* Chunks *)

	method start_chunk (kind : chunk_kind) =
		(* Printf.eprintf "Writing chunk %s\n" (string_of_chunk_kind kind); *)
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
		self#write_pos p;
		chunk#write_list el self#write_expr;

	method write_metadata ml =
		chunk#write_list ml self#write_metadata_entry

	(* References *)

	method write_class_ref (c : tclass) =
		let i = classes#get_or_add c.cl_path c in
		(* Printf.eprintf "  Write class ref %d for %s\n" i (snd c.cl_path); *)
		chunk#write_uleb128 i

	method write_enum_ref (en : tenum) =
		let i = enums#get_or_add en.e_path en in
		(* Printf.eprintf "  Write enum ref %d for %s\n" i (snd en.e_path); *)
		chunk#write_uleb128 i

	method write_typedef_ref (td : tdef) =
		let i = typedefs#get_or_add td.t_path td in
		(* Printf.eprintf "  Write typedef ref %d for %s\n" i (s_type_path td.t_path); *)
		chunk#write_uleb128 i

	method write_abstract_ref (a : tabstract) =
		let i = abstracts#get_or_add a.a_path a in
		(* Printf.eprintf "  Write abstract ref %d for %s\n" i (snd a.a_path); *)
		chunk#write_uleb128 i

	method write_anon_ref (an : tanon) =
		let pfm = Option.get (anon_id#identify true (TAnon an)) in
		let i = anons#get_or_add pfm.pfm_path (an,ttp_key) in
		(* Printf.eprintf "  Write anon ref %d for %s\n" i (s_type_path pfm.pfm_path); *)
		chunk#write_uleb128 i

	method write_field_ref (source : field_source) (cf : tclass_field) =
		chunk#write_string cf.cf_name

	method write_enum_field_ref ef =
		chunk#write_string ef.ef_name

	(* Type instances *)

	method write_type_instance t =
		let write_function_arg (n,o,t) =
			chunk#write_string n;
			chunk#write_bool o;
			self#write_type_instance t;
		in
		match t with
		(* TODO: we might need to properly restore monomorphs... *)
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
			begin match td.t_type with
				| TAnon an ->
					begin match !(an.a_status) with
						| Statics c ->
							chunk#write_byte 0;
							self#write_class_ref c;
						| EnumStatics en ->
							chunk#write_byte 1;
							self#write_enum_ref en;
						| AbstractStatics a ->
							chunk#write_byte 2;
							self#write_abstract_ref a;
						| _ ->
							chunk#write_byte 3;
							self#write_typedef_ref td;
					end
				| _ ->
					chunk#write_byte 3;
					self#write_typedef_ref td;
			end;
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
			(* Printf.eprintf "  TType %d for %s\n" 16 (s_type_path td.t_path); *)
			self#write_typedef_ref td;
			self#write_types tl
		| TAbstract(a,tl) ->
			chunk#write_byte 17;
			self#write_abstract_ref a;
			self#write_types tl
		(* TODO what to do with void special case? *)
		(* | TFun([],t) when ExtType.is_void (follow t) -> *)
		(* 	chunk#write_byte 30; *)
		(* | TFun(args,t) when ExtType.is_void (follow t) -> *)
		(* 	chunk#write_byte 31; *)
		(* 	chunk#write_list args write_function_arg; *)
		| TFun(args,t) ->
			chunk#write_byte 32;
			chunk#write_list args write_function_arg;
			self#write_type_instance t;
		| TLazy r ->
			chunk#write_byte 33;
			self#write_type_instance (lazy_type r);
		| TDynamic None ->
			chunk#write_byte 40
		| TDynamic (Some t) ->
			chunk#write_byte 41;
			self#write_type_instance t;
		| TAnon an when PMap.is_empty an.a_fields ->
			chunk#write_byte 50;
			chunk#write_bool true
		| TAnon an ->
			chunk#write_byte 51;
			self#write_anon_ref an;

	method write_types tl =
		chunk#write_list tl self#write_type_instance

	(* expr *)

	method write_object_field_key (n,p,qs) =
		chunk#write_string n;
		self#write_pos p;
		begin match qs with
			| NoQuotes -> chunk#write_byte 0
			| DoubleQuotes -> chunk#write_byte 1
		end

	method write_type_path tp =
		chunk#write_list tp.tpackage chunk#write_string;
		chunk#write_string tp.tname;
		chunk#write_list tp.tparams self#write_type_param_or_const;
		chunk#write_option tp.tsub chunk#write_string

	method write_placed_type_path (tp,p) =
		self#write_type_path tp;
		self#write_pos p

	method write_type_param_or_const = function
		| TPType th ->
			chunk#write_byte 0;
			self#write_type_hint th
		| TPExpr e ->
			chunk#write_byte 1;
			self#write_expr e

	method write_complex_type = function
		| CTPath tp ->
			chunk#write_byte 0;
			self#write_type_path tp
		| CTFunction(thl,th) ->
			chunk#write_byte 1;
			chunk#write_list thl self#write_type_hint;
			self#write_type_hint th
		| CTAnonymous cffl ->
			chunk#write_byte 2;
			chunk#write_list cffl self#write_cfield;
		| CTParent th ->
			chunk#write_byte 3;
			self#write_type_hint th
		| CTExtend(ptp,cffl) ->
			chunk#write_byte 4;
			chunk#write_list ptp self#write_placed_type_path;
			chunk#write_list cffl self#write_cfield;
		| CTOptional th ->
			chunk#write_byte 5;
			self#write_type_hint th
		| CTNamed(pn,th) ->
			chunk#write_byte 6;
			self#write_placed_name pn;
			self#write_type_hint th
		| CTIntersection(thl) ->
			chunk#write_byte 6;
			chunk#write_list thl self#write_type_hint;

	method write_type_hint (ct,p) =
		self#write_complex_type ct;
		self#write_pos p

	method write_type_param tp =
		self#write_placed_name tp.tp_name;
		chunk#write_list tp.tp_params self#write_type_param;
		chunk#write_option tp.tp_constraints self#write_type_hint;
		chunk#write_option tp.tp_default self#write_type_hint;
		chunk#write_list tp.tp_meta self#write_metadata_entry;

	method write_func_arg (pn,b,meta,tho,eo) =
		self#write_placed_name pn;
		chunk#write_bool b;
		self#write_metadata meta;
		chunk#write_option tho self#write_type_hint;
		chunk#write_option eo self#write_expr;

	method write_func f =
		chunk#write_list f.f_params self#write_type_param;
		chunk#write_list f.f_args self#write_func_arg;
		chunk#write_option f.f_type self#write_type_hint;
		chunk#write_option f.f_expr self#write_expr

	method write_placed_name (s,p) =
		chunk#write_string s;
		self#write_pos p

	method write_access ac =
		let i = match ac with
		| APublic -> 0
		| APrivate -> 1
		| AStatic -> 2
		| AOverride -> 3
		| ADynamic -> 4
		| AInline -> 5
		| AMacro -> 6
		| AFinal -> 7
		| AExtern -> 8
		| AAbstract -> 9
		| AOverload -> 10
		| AEnum -> 11
		in
		chunk#write_byte i;


	method write_placed_access (ac,p) =
		self#write_access ac;
		self#write_pos p;

	method write_cfield_kind = function
		| FVar(tho,eo) ->
			chunk#write_byte 0;
			chunk#write_option tho self#write_type_hint;
			chunk#write_option eo self#write_expr;
		| FFun f ->
			chunk#write_byte 1;
			self#write_func f;
		| FProp(pn1,pn2,tho,eo) ->
			chunk#write_byte 2;
			self#write_placed_name pn1;
			self#write_placed_name pn2;
			chunk#write_option tho self#write_type_hint;
			chunk#write_option eo self#write_expr;

	method write_cfield cff =
		self#write_placed_name cff.cff_name;
		chunk#write_option cff.cff_doc self#write_documentation;
		self#write_pos cff.cff_pos;
		self#write_metadata cff.cff_meta;
		chunk#write_list cff.cff_access self#write_placed_access;
		self#write_cfield_kind cff.cff_kind;

	method write_expr (e,p) =
		self#write_pos p;
		match e with
		| EConst (Int (s, suffix)) ->
			chunk#write_byte 0;
			chunk#write_string s;
			chunk#write_option suffix chunk#write_string;
		| EConst (Float (s, suffix)) ->
			chunk#write_byte 1;
			chunk#write_string s;
			chunk#write_option suffix chunk#write_string;
		| EConst (String (s,qs)) ->
			chunk#write_byte 2;
			chunk#write_string s;
			begin match qs with
			| SDoubleQuotes -> chunk#write_byte 0;
			| SSingleQuotes -> chunk#write_byte 1;
			end
		| EConst (Ident s) ->
			chunk#write_byte 3;
			chunk#write_string s;
		| EConst (Regexp(s1,s2)) ->
			chunk#write_byte 4;
			chunk#write_string s1;
			chunk#write_string s2;
		| EArray(e1,e2) ->
			chunk#write_byte 5;
			self#write_expr e1;
			self#write_expr e2;
		| EBinop(op,e1,e2) ->
			chunk#write_byte 6;
			chunk#write_byte (binop_index op);
			self#write_expr e1;
			self#write_expr e2;
		| EField(e1,s,kind) ->
			chunk#write_byte 7;
			self#write_expr e1;
			chunk#write_string s;
			begin match kind with
			| EFNormal -> chunk#write_byte 0;
			| EFSafe -> chunk#write_byte 1;
			end
		| EParenthesis e1 ->
			chunk#write_byte 8;
			self#write_expr e1
		| EObjectDecl fl ->
			chunk#write_byte 9;
			let write_field (k,e1) =
				self#write_object_field_key k;
				self#write_expr e1
			in
			chunk#write_list fl write_field;
		| EArrayDecl el ->
			chunk#write_byte 10;
			chunk#write_list el self#write_expr;
		| ECall(e1,el) ->
			chunk#write_byte 11;
			self#write_expr e1;
			chunk#write_list el self#write_expr
		| ENew(ptp,el) ->
			chunk#write_byte 12;
			self#write_placed_type_path ptp;
			chunk#write_list el self#write_expr;
		| EUnop(op,flag,e1) ->
			chunk#write_byte 13;
			chunk#write_byte (unop_index op flag);
			self#write_expr e1;
		| EVars vl ->
			chunk#write_byte 14;
			let write_var v =
				self#write_placed_name v.ev_name;
				chunk#write_bool v.ev_final;
				chunk#write_bool v.ev_static;
				chunk#write_option v.ev_type self#write_type_hint;
				chunk#write_option v.ev_expr self#write_expr;
				self#write_metadata v.ev_meta;
			in
			chunk#write_list vl write_var
		| EFunction(fk,f) ->
			chunk#write_byte 15;
			begin match fk with
			| FKAnonymous -> chunk#write_byte 0;
			| FKNamed (pn,inline) ->
				chunk#write_byte 1;
				self#write_placed_name pn;
				chunk#write_bool inline;
			| FKArrow -> chunk#write_byte 2;
			end;
			self#write_func f;
		| EBlock el ->
			chunk#write_byte 16;
			chunk#write_list el self#write_expr
		| EFor(e1,e2) ->
			chunk#write_byte 17;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,None) ->
			chunk#write_byte 18;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,Some e3) ->
			chunk#write_byte 19;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| EWhile(e1,e2,NormalWhile) ->
			chunk#write_byte 20;
			self#write_expr e1;
			self#write_expr e2;
		| EWhile(e1,e2,DoWhile) ->
			chunk#write_byte 21;
			self#write_expr e1;
			self#write_expr e2;
		| ESwitch(e1,cases,def) ->
			chunk#write_byte 22;
			self#write_expr e1;
			let write_case (el,eg,eo,p) =
				chunk#write_list el self#write_expr;
				chunk#write_option eg self#write_expr;
				chunk#write_option eo self#write_expr;
				self#write_pos p;
			in
			chunk#write_list cases write_case;
			let write_default (eo,p) =
				chunk#write_option eo self#write_expr;
				self#write_pos p
			in
			chunk#write_option def write_default;
		| ETry(e1,catches) ->
			chunk#write_byte 23;
			self#write_expr e1;
			let write_catch (pn,th,e,p) =
				self#write_placed_name pn;
				chunk#write_option th self#write_type_hint;
				self#write_expr e;
				self#write_pos p;
			in
			chunk#write_list catches write_catch;
		| EReturn None ->
			chunk#write_byte 24;
		| EReturn (Some e1) ->
			chunk#write_byte 25;
			self#write_expr e1;
		| EBreak ->
			chunk#write_byte 26;
		| EContinue ->
			chunk#write_byte 27;
		| EUntyped e1 ->
			chunk#write_byte 28;
			self#write_expr e1;
		| EThrow e1 ->
			chunk#write_byte 29;
			self#write_expr e1;
		| ECast(e1,None) ->
			chunk#write_byte 30;
			self#write_expr e1;
		| ECast(e1,Some th) ->
			chunk#write_byte 31;
			self#write_expr e1;
			self#write_type_hint th;
		| EIs(e1,th) ->
			chunk#write_byte 32;
			self#write_expr e1;
			self#write_type_hint th;
		| EDisplay(e1,dk) ->
			chunk#write_byte 33;
			self#write_expr e1;
			begin match dk with
			| DKCall -> chunk#write_byte 0;
			| DKDot -> chunk#write_byte 1;
			| DKStructure -> chunk#write_byte 2;
			| DKMarked -> chunk#write_byte 3;
			| DKPattern b ->
				chunk#write_byte 4;
				chunk#write_bool b;
			end
		| ETernary(e1,e2,e3) ->
			chunk#write_byte 34;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| ECheckType(e1,th) ->
			chunk#write_byte 35;
			self#write_expr e1;
			self#write_type_hint th;
		| EMeta(m,e1) ->
			chunk#write_byte 36;
			self#write_metadata_entry m;
			self#write_expr e1

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
			| VAbstractThis -> 10
		in
		chunk#write_byte b

	method write_var v =
		chunk#write_i32 v.v_id;
		chunk#write_string v.v_name;
		self#write_type_instance v.v_type;
		self#write_var_kind v.v_kind;
		chunk#write_option v.v_extra (fun ve ->
			(* TODO *)
			(* chunk#write_list ve.v_params self#write_typed_type_param; *)
			chunk#write_option ve.v_expr self#write_texpr;
		);
		chunk#write_i32 v.v_flags;
		self#write_metadata v.v_meta;
		self#write_pos v.v_pos;

	method write_texpr (e : texpr) =
		let rec loop e =
			self#write_type_instance e.etype;
			self#write_pos e.epos;

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
					chunk#write_u32 i32;
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
				chunk#write_i32 v.v_id;
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
						chunk#write_ui16 l;
					end else begin
						chunk#write_byte 38;
						chunk#write_i32 l;
					end;
				end;
				List.iter loop el
			(* function 50-59 *)
			| TFunction tf ->
				chunk#write_byte 50;
				chunk#write_list tf.tf_args (fun (v,eo) ->
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
				chunk#write_list fl (fun ((name,p,qs),e) ->
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
			| TSwitch s ->
				chunk#write_byte 82;
				loop s.switch_subject;
				chunk#write_list s.switch_cases (fun c ->
					loop_el c.case_patterns;
					loop c.case_expr;
				);
				chunk#write_option s.switch_default loop;
			| TTry(e1,catches) ->
				chunk#write_byte 83;
				loop e1;
				chunk#write_list catches  (fun (v,e) ->
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
			| TEnumParameter(e1,({ ef_type = TEnum(en,_) | TFun(_, TEnum(en,_)) } as ef),i) ->
				chunk#write_byte 101;
				loop e1;
				self#write_enum_ref en;
				self#write_enum_field_ref ef;
				chunk#write_i32 i;
			| TEnumParameter(e1,({ ef_type = eft}),i) ->
				Printf.eprintf "en = %s\n" (s_type_kind eft);
				assert false
			| TField(e1,FInstance(c,tl,cf)) ->
				chunk#write_byte 102;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref (ClassMember c) cf; (* TODO check source *)
			| TField(e1,FStatic(c,cf)) ->
				chunk#write_byte 103;
				loop e1;
				self#write_class_ref c;
				self#write_field_ref (ClassMember c) cf; (* TODO check source *)
			| TField(e1,FAnon cf) ->
				chunk#write_byte 104;
				loop e1;
				chunk#write_uleb128 (anon_fields#get_or_add cf cf);
			| TField(e1,FClosure(Some(c,tl),cf)) ->
				chunk#write_byte 105;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref (ClassMember c) cf; (* TODO check source *)
			| TField(e1,FClosure(None,cf)) ->
				chunk#write_byte 106;
				loop e1;
				chunk#write_uleb128 (anon_fields#get_or_add cf cf);
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
				let infos = t_infos md in
				let m = infos.mt_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd infos.mt_path);
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
			chunk#write_ui16 (List.length el);
			List.iter loop el
		in
		loop e

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

	method write_class_field ?(with_pos = false) cf =
		self#set_field_type_parameters cf.cf_params;
		(* Printf.eprintf " Write class field %s\n" cf.cf_name; *)
		chunk#write_string cf.cf_name;
		chunk#write_list cf.cf_params self#write_type_parameter_forward;
		chunk#write_list cf.cf_params self#write_type_parameter_data;
		self#write_type_instance cf.cf_type;
		chunk#write_i32 cf.cf_flags;
		if with_pos then begin
			self#write_pos cf.cf_pos;
			self#write_pos cf.cf_name_pos;
		end;
		chunk#write_option cf.cf_doc self#write_documentation;
		self#write_metadata cf.cf_meta;
		self#write_field_kind cf.cf_kind;
		chunk#write_option cf.cf_expr self#write_texpr;
		chunk#write_option cf.cf_expr_unoptimized self#write_texpr;
		chunk#write_list cf.cf_overloads (self#write_class_field ~with_pos:true);

	(* Module types *)

	method select_type (path : path) =
		(* Printf.eprintf "Select type %s\n" (s_type_path path); *)
		ttp_key <- path;
		type_type_parameters <- type_param_lut#extract path

	method write_common_module_type (infos : tinfos) : unit =
		(* self#write_path infos.mt_path; *)
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
			self#write_expr e;
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
			chunk#write_list l self#write_cfield;
		| KAbstractImpl a ->
			chunk#write_byte 7;
			self#write_abstract_ref a;
		| KModuleFields md ->
			chunk#write_byte 8;
			(* TODO *)
			Printf.eprintf "  %s KModuleFields\n" todo;

	method write_class (c : tclass) =
		begin match c.cl_kind with
		| KAbstractImpl a ->
			(* Printf.eprintf "Write abstract impl %s with %d type params\n" (snd c.cl_path) (List.length a.a_params); *)
			self#select_type a.a_path
		| _ ->
			self#select_type c.cl_path;
		end;
		(* Printf.eprintf "Write class %s with %d type params\n" (snd c.cl_path) (List.length c.cl_params); *)
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
			chunk#write_string cf.cf_name;
			self#set_field_type_parameters cf.cf_params;
			chunk#write_list cf.cf_params self#write_type_parameter_forward;
			chunk#write_list cf.cf_params self#write_type_parameter_data;
			self#write_type_instance t;
			self#write_field_ref (ClassStatic c) cf;
		);
		chunk#write_list a.a_to self#write_type_instance;
		chunk#write_list a.a_to_field (fun (t,cf) ->
			chunk#write_string cf.cf_name;
			self#set_field_type_parameters cf.cf_params;
			chunk#write_list cf.cf_params self#write_type_parameter_forward;
			chunk#write_list cf.cf_params self#write_type_parameter_data;
			self#write_type_instance t;
			self#write_field_ref (ClassStatic c) cf;
		);
		chunk#write_list a.a_array (self#write_field_ref (ClassStatic c));
		chunk#write_option a.a_read (self#write_field_ref (ClassStatic c));
		chunk#write_option a.a_write (self#write_field_ref (ClassStatic c));
		chunk#write_option a.a_call (self#write_field_ref (ClassStatic c));
		chunk#write_bool a.a_enum

	method write_enum (e : tenum) =
		(* Printf.eprintf "Write enum %s\n" (snd e.e_path); *)
		self#select_type e.e_path;
		self#write_common_module_type (Obj.magic e);
		chunk#write_bool e.e_extern;
		chunk#write_list e.e_names chunk#write_string;

	method write_typedef (td : tdef) =
		(* Printf.eprintf "Write typedef %s %s >>\n" (s_type_path td.t_path) (s_type_kind td.t_type); *)
		self#select_type td.t_path;
		self#write_common_module_type (Obj.magic td);
		self#write_type_instance td.t_type;

	method write_anon (m : module_def) ((an : tanon), (ttp_key : path)) =
		chunk#write_string (snd ttp_key);
		self#select_type ttp_key;

		let write_fields () =
			chunk#write_list (PMap.foldi (fun s f acc -> (s,f) :: acc) an.a_fields []) (fun (_,cf) ->
				self#write_class_field ~with_pos:true cf;
			)
		in

		begin match !(an.a_status) with
		| Closed ->
			chunk#write_byte 0;
			write_fields ()
		| Const ->
			chunk#write_byte 1;
			write_fields ()
		| Extend tl ->
			chunk#write_byte 2;
			self#write_types tl;
			write_fields ()
		| Statics c ->
			chunk#write_byte 3;
			self#write_class_ref c;
		| EnumStatics en ->
			chunk#write_byte 4;
			write_fields ()
		| AbstractStatics a ->
			chunk#write_byte 5;
			self#write_abstract_ref a;
			write_fields ()
		end

	(* Module *)

	method forward_declare_type (mt : module_type) =
		let name = ref "" in
		let i = match mt with
		| TClassDecl c ->
			ignore(classes#add c.cl_path c);
			ignore(own_classes#add c.cl_path c);
			name := snd c.cl_path;
			0
		| TEnumDecl e ->
			ignore(enums#get_or_add e.e_path e);
			ignore(own_enums#add e.e_path e);
			name := snd e.e_path;
			1
		| TTypeDecl t ->
			ignore(typedefs#get_or_add t.t_path t);
			ignore(own_typedefs#add t.t_path t);
			name := snd t.t_path;
			2
		| TAbstractDecl a ->
			ignore(abstracts#add a.a_path a);
			ignore(own_abstracts#add a.a_path a);
			name := snd a.a_path;
			3
		in

		let infos = t_infos mt in
		(* Printf.eprintf "Forward declare type %s\n" (s_type_path infos.mt_path); *)
		chunk#write_byte i;
		(* self#write_path infos.mt_path; *)
		self#write_full_path (fst infos.mt_path) (snd infos.mt_path) !name;
		self#write_pos infos.mt_pos;
		self#write_pos infos.mt_name_pos;
		let params = new pool in
		type_type_parameters <- params;
		ignore(type_param_lut#add infos.mt_path params);
		List.iter (fun ttp ->
			ignore(type_type_parameters#add ttp.ttp_name ttp);
		) infos.mt_params;

		(* Forward declare fields *)
		match mt with
		| TClassDecl c ->
			(* Write minimal data to be able to create refs *)
			let write_field cf =
				chunk#write_string cf.cf_name;
				self#write_pos cf.cf_pos;
				self#write_pos cf.cf_name_pos
			in
			chunk#write_option c.cl_constructor write_field;
			chunk#write_list c.cl_ordered_fields write_field;
			chunk#write_list c.cl_ordered_statics write_field;
		| TEnumDecl e ->
				chunk#write_list (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
					(* Printf.eprintf "  forward declare enum field %s.%s\n" (s_type_path e.e_path) s; *)
					chunk#write_string s;
					self#write_pos ef.ef_pos;
					self#write_pos ef.ef_name_pos;
					chunk#write_byte ef.ef_index
				);
		| TAbstractDecl a ->
			(* TODO ? *)
			()
		| TTypeDecl t ->
			(* TODO ? *)
			()

	method write_module (m : module_def) =
		self#start_chunk HHDR;
		self#write_path m.m_path;
		chunk#write_string (Path.UniqueKey.lazy_path m.m_extra.m_file);

		self#start_chunk TYPF;
		chunk#write_list m.m_types self#forward_declare_type;

		(* Printf.eprintf "Write module %s with %d own classes, %d own abstracts, %d own enums, %d own typedefs\n" *)
		(* 	(snd m.m_path) (List.length own_classes#to_list) (List.length own_abstracts#to_list) (List.length own_enums#to_list) (List.length own_typedefs#to_list); *)

		begin match own_abstracts#to_list with
		| [] ->
			()
		| own_abstracts ->
			self#start_chunk ABSD;
			chunk#write_list own_abstracts self#write_abstract;
		end;
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
		begin match own_enums#to_list with
		| [] ->
			()
		| own_enums ->
			self#start_chunk ENMD;
			chunk#write_list own_enums self#write_enum;
			self#start_chunk EFLD;
			chunk#write_list own_enums (fun e ->
				chunk#write_list (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
					(* Printf.eprintf "  Write enum field %s.%s\n" (s_type_path e.e_path) s; *)
					chunk#write_string s;
					self#set_field_type_parameters ef.ef_params;
					chunk#write_list ef.ef_params self#write_type_parameter_forward;
					chunk#write_list ef.ef_params self#write_type_parameter_data;
					self#write_type_instance ef.ef_type;
					chunk#write_option ef.ef_doc self#write_documentation;
					self#write_metadata ef.ef_meta;
				);
			)
		end;
		begin match own_typedefs#to_list with
		| [] ->
			()
		| own_typedefs ->
			self#start_chunk TPDD;
			chunk#write_list own_typedefs self#write_typedef;
		end;

		begin match anons#to_list with
		| [] ->
			()
		| anons ->
			self#start_chunk ANNR;
			chunk#write_uleb128 (List.length anons);
			self#start_chunk ANND;
			chunk#write_list anons (fun an -> self#write_anon m an);
		end;

		let anon_fields = anon_fields#to_list in
		begin match anon_fields with
		| [] ->
			()
		| l ->
			self#start_chunk ANFR;
			chunk#write_list l (fun cf ->
				(* Printf.eprintf "Write anon field %s\n" cf.cf_name; *)
				chunk#write_string cf.cf_name;
				self#write_pos cf.cf_pos;
				self#write_pos cf.cf_name_pos;
			);
			self#start_chunk ANFD;
			chunk#write_list l (fun cf ->
				self#write_class_field cf;
			);
		end;

		begin match classes#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk CLSR;
			chunk#write_list l (fun c ->
				let m = c.cl_module in
				(* Printf.eprintf "  [cls] Write full path %s\n" (ExtString.String.join "." ((fst m.m_path) @ [(snd m.m_path); (snd c.cl_path)])); *)
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
				(* Printf.eprintf "  [abs] Write full path %s\n" (ExtString.String.join "." ((fst m.m_path) @ [(snd m.m_path); (snd a.a_path)])); *)
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd a.a_path)
			)
		end;
		begin match enums#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ENMR;
			chunk#write_list l (fun en ->
				let m = en.e_module in
				(* Printf.eprintf "  [enm] Write full path %s\n" (ExtString.String.join "." ((fst m.m_path) @ [(snd m.m_path); (snd en.e_path)])); *)
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd en.e_path)
			)
		end;
		begin match typedefs#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk TPDR;
			chunk#write_list l (fun td ->
				let m = td.t_module in
				(* Printf.eprintf "  [tpdr] Write full path %s\n" (ExtString.String.join "." ((fst m.m_path) @ [(snd m.m_path); (snd td.t_path)])); *)
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd td.t_path)
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
