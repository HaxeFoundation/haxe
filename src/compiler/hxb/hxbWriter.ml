open Globals
open Ast
open Type
open HxbData
open Tanon_identification
open HxbShared

(* Debug utils *)
let no_color = false
let c_reset = if no_color then "" else "\x1b[0m"
let c_bold = if no_color then "" else "\x1b[1m"
let c_dim = if no_color then "" else "\x1b[2m"
let todo = "\x1b[33m[TODO]" ^ c_reset
let todo_error = "\x1b[31m[TODO] error:" ^ c_reset

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

let debug_msg msg =
	prerr_endline msg

let print_stacktrace () =
	let stack = Printexc.get_callstack 10 in
	let lines = Printf.sprintf "%s\n" (Printexc.raw_backtrace_to_string stack) in
	match (ExtString.String.split_on_char '\n' lines) with
		| (_ :: (_ :: lines)) -> debug_msg (Printf.sprintf "%s" (ExtString.String.join "\n" lines))
		| _ -> die "" __LOC__

let print_types source tl =
	debug_msg (Printf.sprintf "Types from %s:" source);
	List.iter (fun t -> debug_msg (Printf.sprintf "  %s" (s_type_kind t))) tl

let print_params source ttp =
	debug_msg (Printf.sprintf "Params from %s:" source);
	List.iter (fun t -> debug_msg (Printf.sprintf "  %s" t.ttp_name)) ttp

type hxb_writer_stats = {
	type_instance_kind_writes : int array;
	type_instance_immediate : int ref;
	type_instance_ring_hits : int ref;
	type_instance_cache_hits : int ref;
	type_instance_cache_misses : int ref;
	pos_writes_full : int ref;
	pos_writes_min : int ref;
	pos_writes_max : int ref;
	pos_writes_minmax : int ref;
}

let create_hxb_writer_stats () = {
	type_instance_kind_writes = Array.make 255 0;
	type_instance_immediate = ref 0;
	type_instance_ring_hits = ref 0;
	type_instance_cache_hits = ref 0;
	type_instance_cache_misses = ref 0;
	pos_writes_full = ref 0;
	pos_writes_min = ref 0;
	pos_writes_max = ref 0;
	pos_writes_minmax = ref 0;
}

let dump_stats name stats =
	let _,kind_writes = Array.fold_left (fun (index,acc) writes ->
		(index + 1,if writes = 0 then acc else (index,writes) :: acc)
	) (0,[]) stats.type_instance_kind_writes in
	let kind_writes = List.sort (fun (_,writes1) (_,writes2) -> compare writes2 writes1) kind_writes in
	let kind_writes = List.map (fun (index,writes) -> Printf.sprintf "    %3i: %9i" index writes) kind_writes in
	print_endline (Printf.sprintf "hxb_writer stats for %s" name);
	print_endline "  type instance kind writes:";
	List.iter print_endline kind_writes;
	print_endline "  type instance writes:";
	print_endline (Printf.sprintf "     immediate: %9i" !(stats.type_instance_immediate));
	print_endline (Printf.sprintf "     ring hits: %9i" !(stats.type_instance_ring_hits));
	print_endline (Printf.sprintf "    cache hits: %9i" !(stats.type_instance_cache_hits));
	print_endline (Printf.sprintf "    cache miss: %9i" !(stats.type_instance_cache_misses));
	print_endline "  pos writes:";
	print_endline (Printf.sprintf "      full: %9i\n       min: %9i\n       max: %9i\n    minmax: %9i" !(stats.pos_writes_full) !(stats.pos_writes_min) !(stats.pos_writes_max) !(stats.pos_writes_minmax))

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

	method advance dummy =
		DynArray.add items dummy

	method to_list =
		DynArray.to_list items

	method items = items
end

class ['key,'value] identity_pool = object(self)
	val items = DynArray.create ()

	method add (key : 'key) (value : 'value) =
		let index = DynArray.length items in
		DynArray.add items (key,value);
		index

	method get (key : 'key) =
		DynArray.index_of (fun (key',_) -> key == key') items

	method to_list =
		DynArray.to_list items

	method items = items
end

class io_chunk
	(name : string) =
object(self)
	val ch = IO.output_bytes()

	method write_u8 v =
		IO.write_byte ch v

	method write_u32 v =
		IO.write_real_i32 ch v

	method write_f64 v =
		IO.write_double ch v

	method write_bytes b =
		IO.nwrite ch b;

	method write_ui16 i =
		IO.write_ui16 ch i;

	method get_bytes =
		IO.close_out ch
end

class abstract_chunk (name : string) = object(self)
	inherit io_chunk name

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

	method write_bytes_length_prefixed b =
		self#write_uleb128 (Bytes.length b);
		self#write_bytes b

	method write_bool b =
		self#write_u8 (if b then 1 else 0)

	method write_i32 i =
		self#write_u32 (Int32.of_int i);

	method export : 'a . 'a IO.output -> unit = fun chex ->
		let bytes = self#get_bytes in
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
			self#write_bytes_length_prefixed b;
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
			self#write_u8 0
		| Some v ->
			self#write_u8 1;
			f v

	method export_data (chunk : chunk) =
		let bytes = self#get_bytes in
		chunk#write_bytes bytes

	method kind =
		kind
end

class pos_writer
	(chunk : chunk)
	(stats : hxb_writer_stats)
	(p_initial : pos)
	(write_equal : bool)
= object(self)

	val mutable p_file = p_initial.pfile
	val mutable p_min = p_initial.pmin
	val mutable p_max = p_initial.pmax

	method private do_write_pos (p : pos) =
		incr stats.pos_writes_full;
		chunk#write_string p.pfile;
		chunk#write_leb128 p.pmin;
		chunk#write_leb128 p.pmax;

	method write_pos (offset : int) (p : pos) =
		if p.pfile <> p_file then begin
			(* File changed, write full pos *)
			chunk#write_u8 (4 + offset);
			self#do_write_pos p;
			p_file <- p.pfile;
			p_min <- p.pmin;
			p_max <- p.pmax;
		end else if p.pmin <> p_min then begin
			if p.pmax <> p_max then begin
				(* pmin and pmax changed *)
				incr stats.pos_writes_minmax;
				chunk#write_u8 (3 + offset);
				chunk#write_leb128 p.pmin;
				chunk#write_leb128 p.pmax;
				p_min <- p.pmin;
				p_max <- p.pmax;
			end else begin
				(* pmin changed *)
				incr stats.pos_writes_min;
				chunk#write_u8 (1 + offset);
				chunk#write_leb128 p.pmin;
				p_min <- p.pmin;
			end
		end else if p.pmax <> p_max then begin
			(* pmax changed *)
			incr stats.pos_writes_max;
			chunk#write_u8 (2 + offset);
			chunk#write_leb128 p.pmax;
			p_max <- p.pmax;
		end else if write_equal then
			chunk#write_u8 offset;

	initializer
		self#do_write_pos p_initial
end

let ghetto_bottom_type = TInst({(null_class) with cl_path = ([],"Bottom")},[])

class t_rings (length : int) = object(self)
	val ring_inst = Ring.create length (ghetto_bottom_type,0)
	val ring_enum = Ring.create length (ghetto_bottom_type,0)
	val ring_type = Ring.create length (ghetto_bottom_type,0)
	val ring_abstract = Ring.create length (ghetto_bottom_type,0)
	val ring_fun = Ring.create length (ghetto_bottom_type,0)
	val ring_anon = Ring.create length (ghetto_bottom_type,0)
	val ring_dynamic = Ring.create 1 (ghetto_bottom_type,0)
	val ring_mono = Ring.create 1 (ghetto_bottom_type,0)

	method ring_inst = ring_inst
	method ring_enum = ring_enum
	method ring_type = ring_type
	method ring_abstract = ring_abstract
	method ring_fun = ring_fun
	method ring_anon = ring_anon
	method ring_dynamic = ring_dynamic

	method fast_eq_check type_param_check a b =
		if a == b then
			true
		else match a , b with
		| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
			List.for_all2 (fun (n1,o1,t1) (n2,o2,t2) ->
				n1 = n2 &&
				o1 = o2 &&
				type_param_check t1 t2
			) l1 l2 && type_param_check r1 r2
		| TType (t1,l1), TType (t2,l2) ->
			t1 == t2 && List.for_all2 type_param_check l1 l2
		| TEnum (e1,l1), TEnum (e2,l2) ->
			e1 == e2 && List.for_all2 type_param_check l1 l2
		| TInst (c1,l1), TInst (c2,l2) ->
			c1 == c2 && List.for_all2 type_param_check l1 l2
		| TAbstract (a1,l1), TAbstract (a2,l2) ->
			a1 == a2 && List.for_all2 type_param_check l1 l2
		| TAnon an1,TAnon an2 ->
			begin match !(an1.a_status),!(an2.a_status) with
				| ClassStatics c, ClassStatics c2 -> c == c2
				| EnumStatics e, EnumStatics e2 -> e == e2
				| AbstractStatics a, AbstractStatics a2 -> a == a2
				| _ -> false
			end
		| _ , _ ->
			false

	method find (ring : (Type.t * int) Ring.t) (t : Type.t) =
		let rec fast_eq a b = self#fast_eq_check fast_eq a b in
		let _,index = Ring.find ring (fun (t',_) -> fast_eq t t') in
		index
end

let dummy_rings = new t_rings 0

type field_writer_context = {
	t_pool : (bytes,bytes) pool;
	t_rings : t_rings;
	pos_writer : pos_writer;
	vars : (int,tvar) pool;
}

let create_field_writer_context pos_writer = {
	t_pool = new pool;
	t_rings = new t_rings 5;
	pos_writer = pos_writer;
	vars = new pool;
}

class hxb_writer
	(display_source_at : Globals.pos -> unit)
	(anon_id : Type.t Tanon_identification.tanon_identification)
	(stats : hxb_writer_stats)
= object(self)

	val mutable current_module = null_module
	val chunks = DynArray.create ()
	val cp = new string_pool STRI
	val docs = new string_pool DOCS

	val mutable chunk = Obj.magic ()

	val classes = new pool
	val enums = new pool
	val typedefs = new pool
	val abstracts = new pool
	val anons = new pool
	val anon_fields = new identity_pool
	val tmonos = new identity_pool

	val own_classes = new pool
	val own_abstracts = new pool
	val own_enums = new pool
	val own_typedefs = new pool

	val type_param_lut = new pool
	val class_fields = new identity_pool
	val enum_fields = new pool
	val mutable type_type_parameters = new pool
	val mutable field_type_parameters = new identity_pool
	val mutable local_type_parameters = new identity_pool

	val instance_overload_cache = Hashtbl.create 0

	val mutable field_stack = []

	method in_nested_scope = match field_stack with
		| [] -> assert false
		| [_] -> false
		| _ -> true

	(* Chunks *)

	method start_chunk (kind : chunk_kind) =
		let new_chunk = new chunk kind cp in
		DynArray.add chunks new_chunk;
		chunk <- new_chunk

	method start_temporary_chunk : 'a . (chunk -> 'a) -> 'a =
		let new_chunk = new chunk HEND (* TODO: something else? *) cp in
		let old_chunk = chunk in
		chunk <- new_chunk;
		(fun f ->
			chunk <- old_chunk;
			f new_chunk
		)

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


	(* expr *)

	method write_object_field_key (n,p,qs) =
		chunk#write_string n;
		self#write_pos p;
		begin match qs with
			| NoQuotes -> chunk#write_u8 0
			| DoubleQuotes -> chunk#write_u8 1
		end

	method write_type_path tp =
		chunk#write_list tp.tpackage chunk#write_string;
		chunk#write_string tp.tname;
		chunk#write_list tp.tparams self#write_type_param_or_const;
		chunk#write_option tp.tsub chunk#write_string

	method write_placed_type_path ptp =
		self#write_type_path ptp.path;
		self#write_pos ptp.pos_full;
		self#write_pos ptp.pos_path

	method write_type_param_or_const = function
		| TPType th ->
			chunk#write_u8 0;
			self#write_type_hint th
		| TPExpr e ->
			chunk#write_u8 1;
			self#write_expr e

	method write_complex_type = function
		| CTPath tp ->
			chunk#write_u8 0;
			self#write_placed_type_path tp
		| CTFunction(thl,th) ->
			chunk#write_u8 1;
			chunk#write_list thl self#write_type_hint;
			self#write_type_hint th
		| CTAnonymous cffl ->
			chunk#write_u8 2;
			chunk#write_list cffl self#write_cfield;
		| CTParent th ->
			chunk#write_u8 3;
			self#write_type_hint th
		| CTExtend(ptp,cffl) ->
			chunk#write_u8 4;
			chunk#write_list ptp self#write_placed_type_path;
			chunk#write_list cffl self#write_cfield;
		| CTOptional th ->
			chunk#write_u8 5;
			self#write_type_hint th
		| CTNamed(pn,th) ->
			chunk#write_u8 6;
			self#write_placed_name pn;
			self#write_type_hint th
		| CTIntersection(thl) ->
			chunk#write_u8 7;
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
		chunk#write_u8 i;

	method write_placed_access (ac,p) =
		self#write_access ac;
		self#write_pos p;

	method write_cfield_kind = function
		| FVar(tho,eo) ->
			chunk#write_u8 0;
			chunk#write_option tho self#write_type_hint;
			chunk#write_option eo self#write_expr;
		| FFun f ->
			chunk#write_u8 1;
			self#write_func f;
		| FProp(pn1,pn2,tho,eo) ->
			chunk#write_u8 2;
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
			chunk#write_u8 0;
			chunk#write_string s;
			chunk#write_option suffix chunk#write_string;
		| EConst (Float (s, suffix)) ->
			chunk#write_u8 1;
			chunk#write_string s;
			chunk#write_option suffix chunk#write_string;
		| EConst (String (s,qs)) ->
			chunk#write_u8 2;
			chunk#write_string s;
			begin match qs with
			| SDoubleQuotes -> chunk#write_u8 0;
			| SSingleQuotes -> chunk#write_u8 1;
			end
		| EConst (Ident s) ->
			chunk#write_u8 3;
			chunk#write_string s;
		| EConst (Regexp(s1,s2)) ->
			chunk#write_u8 4;
			chunk#write_string s1;
			chunk#write_string s2;
		| EArray(e1,e2) ->
			chunk#write_u8 5;
			self#write_expr e1;
			self#write_expr e2;
		| EBinop(op,e1,e2) ->
			chunk#write_u8 6;
			chunk#write_u8 (binop_index op);
			self#write_expr e1;
			self#write_expr e2;
		| EField(e1,s,kind) ->
			chunk#write_u8 7;
			self#write_expr e1;
			chunk#write_string s;
			begin match kind with
			| EFNormal -> chunk#write_u8 0;
			| EFSafe -> chunk#write_u8 1;
			end
		| EParenthesis e1 ->
			chunk#write_u8 8;
			self#write_expr e1
		| EObjectDecl fl ->
			chunk#write_u8 9;
			let write_field (k,e1) =
				self#write_object_field_key k;
				self#write_expr e1
			in
			chunk#write_list fl write_field;
		| EArrayDecl el ->
			chunk#write_u8 10;
			chunk#write_list el self#write_expr;
		| ECall(e1,el) ->
			chunk#write_u8 11;
			self#write_expr e1;
			chunk#write_list el self#write_expr
		| ENew(ptp,el) ->
			chunk#write_u8 12;
			self#write_placed_type_path ptp;
			chunk#write_list el self#write_expr;
		| EUnop(op,flag,e1) ->
			chunk#write_u8 13;
			chunk#write_u8 (unop_index op flag);
			self#write_expr e1;
		| EVars vl ->
			chunk#write_u8 14;
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
			chunk#write_u8 15;
			begin match fk with
			| FKAnonymous -> chunk#write_u8 0;
			| FKNamed (pn,inline) ->
				chunk#write_u8 1;
				self#write_placed_name pn;
				chunk#write_bool inline;
			| FKArrow -> chunk#write_u8 2;
			end;
			self#write_func f;
		| EBlock el ->
			chunk#write_u8 16;
			chunk#write_list el self#write_expr
		| EFor(e1,e2) ->
			chunk#write_u8 17;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,None) ->
			chunk#write_u8 18;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,Some e3) ->
			chunk#write_u8 19;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| EWhile(e1,e2,NormalWhile) ->
			chunk#write_u8 20;
			self#write_expr e1;
			self#write_expr e2;
		| EWhile(e1,e2,DoWhile) ->
			chunk#write_u8 21;
			self#write_expr e1;
			self#write_expr e2;
		| ESwitch(e1,cases,def) ->
			chunk#write_u8 22;
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
			chunk#write_u8 23;
			self#write_expr e1;
			let write_catch (pn,th,e,p) =
				self#write_placed_name pn;
				chunk#write_option th self#write_type_hint;
				self#write_expr e;
				self#write_pos p;
			in
			chunk#write_list catches write_catch;
		| EReturn None ->
			chunk#write_u8 24;
		| EReturn (Some e1) ->
			chunk#write_u8 25;
			self#write_expr e1;
		| EBreak ->
			chunk#write_u8 26;
		| EContinue ->
			chunk#write_u8 27;
		| EUntyped e1 ->
			chunk#write_u8 28;
			self#write_expr e1;
		| EThrow e1 ->
			chunk#write_u8 29;
			self#write_expr e1;
		| ECast(e1,None) ->
			chunk#write_u8 30;
			self#write_expr e1;
		| ECast(e1,Some th) ->
			chunk#write_u8 31;
			self#write_expr e1;
			self#write_type_hint th;
		| EIs(e1,th) ->
			chunk#write_u8 32;
			self#write_expr e1;
			self#write_type_hint th;
		| EDisplay(e1,dk) ->
			chunk#write_u8 33;
			self#write_expr e1;
			begin match dk with
			| DKCall -> chunk#write_u8 0;
			| DKDot -> chunk#write_u8 1;
			| DKStructure -> chunk#write_u8 2;
			| DKMarked -> chunk#write_u8 3;
			| DKPattern b ->
				chunk#write_u8 4;
				chunk#write_bool b;
			end
		| ETernary(e1,e2,e3) ->
			chunk#write_u8 34;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| ECheckType(e1,th) ->
			chunk#write_u8 35;
			self#write_expr e1;
			self#write_type_hint th;
		| EMeta(m,e1) ->
			chunk#write_u8 36;
			self#write_metadata_entry m;
			self#write_expr e1

	(* References *)

	method write_class_ref (c : tclass) =
		let i = classes#get_or_add c.cl_path c in
		chunk#write_uleb128 i

	method write_enum_ref (en : tenum) =
		let i = enums#get_or_add en.e_path en in
		chunk#write_uleb128 i

	method write_typedef_ref (td : tdef) =
		let i = typedefs#get_or_add td.t_path td in
		chunk#write_uleb128 i

	method write_abstract_ref (a : tabstract) =
		let i = abstracts#get_or_add a.a_path a in
		chunk#write_uleb128 i

	method write_anon_ref (an : tanon) (ttp : type_params) =
		let pfm = Option.get (anon_id#identify_anon ~strict:true an) in
		try
			let index = anons#get pfm.pfm_path in
			chunk#write_u8 0;
			chunk#write_uleb128 index
		with Not_found ->
			let index = anons#add pfm.pfm_path an in
			chunk#write_u8 1;
			chunk#write_uleb128 index;
			self#write_anon an ttp

	method write_tmono_ref (mono : tmono) =
		let index = try tmonos#get mono with Not_found -> tmonos#add mono () in
		chunk#write_uleb128 index;

	method write_field_ref (c : tclass) (kind : class_field_ref_kind)  (cf : tclass_field) =
		let index = try
			class_fields#get cf
		with Not_found ->
			let cf_base = find_field c cf.cf_name kind in
			let depth,cf =
				let rec loop depth cfl = match cfl with
					| cf' :: cfl ->
						if cf' == cf then
							depth,cf
						else
							loop (depth + 1) cfl
					| [] ->
						print_endline (Printf.sprintf "Could not resolve %s overload for %s on %s" (s_class_field_ref_kind kind) cf.cf_name (s_type_path c.cl_path));
						0,cf
				in
				let cfl = match kind with
					| CfrStatic | CfrConstructor ->
						(cf_base :: cf_base.cf_overloads)
					| CfrMember ->
						let key = (c.cl_path,cf_base.cf_name) in
						try
							Hashtbl.find instance_overload_cache key
						with Not_found ->
							let l = get_instance_overloads c cf_base.cf_name in
							Hashtbl.add instance_overload_cache key l;
							l
				in
				loop 0 cfl
			in
			class_fields#add cf (c,kind,depth)
		in
		chunk#write_uleb128 index

	method write_enum_field_ref (en : tenum) (ef : tenum_field) =
		let key = (en.e_path,ef.ef_name) in
		try
			chunk#write_uleb128 (enum_fields#get key)
		with Not_found ->
			ignore(enums#get_or_add en.e_path en);
			chunk#write_uleb128 (enum_fields#add key (en,ef))

	method write_anon_field_ref cf =
		try
			let index = anon_fields#get cf in
			chunk#write_u8 0;
			chunk#write_uleb128 index
		with Not_found ->
			let index = anon_fields#add cf () in
			chunk#write_u8 1;
			chunk#write_uleb128 index;
			let close = self#open_field_scope cf.cf_params in
			self#write_class_field_data cf;
			close()

	(* Type instances *)

	val warn_strings = Hashtbl.create 0

	method write_type_parameter_ref (ttp : typed_type_param) =
		begin try
			begin match ttp.ttp_host with
			| TPHMethod | TPHEnumConstructor | TPHAnonField | TPHConstructor ->
				let i = field_type_parameters#get ttp in
				chunk#write_u8 5;
				chunk#write_uleb128 i;
			| TPHType ->
				let i = type_type_parameters#get ttp.ttp_name in
				chunk#write_u8 6;
				chunk#write_uleb128 i
			| TPHLocal ->
				let index = local_type_parameters#get ttp in
				chunk#write_u8 7;
				chunk#write_uleb128 index;
		end with Not_found ->
			let msg = Printf.sprintf "[%s] %s Unbound type parameter %s" (s_type_path current_module.m_path) todo_error (s_type_path ttp.ttp_class.cl_path) in
			if not (Hashtbl.mem warn_strings msg) then begin
				Hashtbl.add warn_strings msg ();
				prerr_endline msg;
			end;
			(* TODO: handle unbound type parameters? *)
			chunk#write_u8 40; (* TDynamic None *)
		end

	method write_type_instance_byte i =
		stats.type_instance_kind_writes.(i) <- stats.type_instance_kind_writes.(i) + 1;
		chunk#write_u8 i

	method write_type_instance_simple (rings : t_rings) (t : Type.t) =
		match t with
		| TAbstract ({a_path = ([],"Int")},[]) ->
			self#write_type_instance_byte 100;
			None
		| TAbstract ({a_path = ([],"Float")},[]) ->
			self#write_type_instance_byte 101;
			None
		| TAbstract ({a_path = ([],"Bool")},[]) ->
			self#write_type_instance_byte 102;
			None
		| TInst ({cl_path = ([],"String")},[]) ->
			self#write_type_instance_byte 103;
			None
		| TMono r ->
			Monomorph.close r;
			begin match r.tm_type with
			| None ->
				self#write_type_instance_byte 1;
				self#write_tmono_ref r;
				None
			| Some t ->
				(* Don't write bound monomorphs, write underlying type directly *)
				self#write_type_instance_simple rings t
			end
		| TLazy f ->
			self#write_type_instance_simple rings (lazy_type f)
		| TInst({cl_kind = KTypeParameter ttp},[]) ->
			self#write_type_parameter_ref ttp;
			None
		| TInst({cl_kind = KExpr _},_) ->
			Some (t,rings#ring_inst)
		| TInst(c,[]) ->
			self#write_type_instance_byte 10;
			self#write_class_ref c;
			None
		| TEnum(en,[]) ->
			self#write_type_instance_byte 11;
			self#write_enum_ref en;
			None
		| TType(td,[]) ->
			let default () =
				self#write_type_instance_byte 12;
				self#write_typedef_ref td;
			in
			begin match td.t_type with
			| TAnon an ->
				begin match !(an.a_status) with
					| ClassStatics c ->
						self#write_type_instance_byte 13;
						self#write_class_ref c
					| EnumStatics en ->
						self#write_type_instance_byte 14;
						self#write_enum_ref en;
					| AbstractStatics a ->
						self#write_type_instance_byte 15;
						self#write_abstract_ref a
					| _ ->
						default()
				end
			| _ ->
				default()
			end;
			None
		| TAbstract(a,[]) ->
			self#write_type_instance_byte 16;
			self#write_abstract_ref a;
			None
		| TDynamic None ->
			self#write_type_instance_byte 40;
			None
		| TFun([],t) when ExtType.is_void (follow_lazy_and_mono t) ->
			self#write_type_instance_byte 30;
			None
		| TInst _ ->
			Some (t,rings#ring_inst)
		| TEnum _ ->
			Some (t,rings#ring_enum)
		| TType _ ->
			Some (t,rings#ring_type)
		| TAbstract _ ->
			Some (t,rings#ring_abstract)
		| TFun _ ->
			Some (t,rings#ring_fun)
		| TAnon _ ->
			Some (t,rings#ring_anon)
		| TDynamic _ ->
			Some (t,rings#ring_dynamic)

	method write_type_instance_not_simple t =
		let write_function_arg (n,o,t) =
			chunk#write_string n;
			chunk#write_bool o;
			self#write_type_instance t;
		in
		match t with
		| TMono _ | TLazy _ | TDynamic None ->
			die "" __LOC__
		| TInst({cl_kind = KExpr e},[]) ->
			self#write_type_instance_byte 8;
			self#write_expr e;
		| TInst(c,tl) ->
			self#write_type_instance_byte 17;
			self#write_class_ref c;
			self#write_types tl
		| TEnum(en,tl) ->
			self#write_type_instance_byte 18;
			self#write_enum_ref en;
			self#write_types tl
		| TType(td,tl) ->
			self#write_type_instance_byte 19;
			self#write_typedef_ref td;
			self#write_types tl
		| TAbstract(a,tl) ->
			self#write_type_instance_byte 20;
			self#write_abstract_ref a;
			self#write_types tl
		| TFun(args,t) when ExtType.is_void (follow_lazy_and_mono t) ->
			self#write_type_instance_byte 31;
			chunk#write_list args write_function_arg;
		| TFun(args,t) ->
			self#write_type_instance_byte 32;
			chunk#write_list args write_function_arg;
			self#write_type_instance t;
		| TDynamic (Some t) ->
			self#write_type_instance_byte 41;
			self#write_type_instance t;
		| TAnon an when PMap.is_empty an.a_fields ->
			self#write_type_instance_byte 50;
		| TAnon an ->
			self#write_type_instance_byte 51;
			self#write_anon_ref an []

	method write_type_instance (t: Type.t) =
		match self#write_type_instance_simple dummy_rings t with
			| None ->
				()
			| Some(t,_) ->
				self#write_type_instance_not_simple t

	method write_types tl =
		chunk#write_list tl self#write_type_instance

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
		chunk#write_u8 b

	method write_var fctx v =
		chunk#write_i32 v.v_id;
		chunk#write_string v.v_name;
		self#write_var_kind v.v_kind;
		chunk#write_i32 v.v_flags;
		self#write_metadata v.v_meta;
		self#write_pos v.v_pos

	method write_texpr_type_instance (fctx : field_writer_context) (t: Type.t) =
		let restore = self#start_temporary_chunk in
		let r = self#write_type_instance_simple fctx.t_rings t in
		let index = match r with
		| None ->
			let t_bytes = restore (fun new_chunk -> new_chunk#get_bytes) in
			incr stats.type_instance_immediate;
			fctx.t_pool#get_or_add t_bytes t_bytes
		| Some(t,ring) ->
			ignore(restore (fun new_chunk -> new_chunk#get_bytes));
			try
				let index = fctx.t_rings#find ring t in
				incr stats.type_instance_ring_hits;
				index
			with Not_found ->
				let restore = self#start_temporary_chunk in
				self#write_type_instance_not_simple t;
				let t_bytes = restore (fun new_chunk ->
					new_chunk#get_bytes
				) in
				let index = try
					let index = fctx.t_pool#get t_bytes in
					incr stats.type_instance_cache_hits;
					index
				with Not_found ->
					incr stats.type_instance_cache_misses;
					fctx.t_pool#add t_bytes t_bytes
				in
				Ring.push ring (t,index);
				index
		in
		chunk#write_uleb128 index

	method write_texpr (fctx : field_writer_context) (e : texpr) =
		let declare_var v =
			chunk#write_uleb128 (fctx.vars#add v.v_id v);
			chunk#write_option v.v_extra (fun ve ->
				chunk#write_list ve.v_params (fun ttp ->
					let index = local_type_parameters#add ttp () in
					chunk#write_uleb128 index
				);
				chunk#write_option ve.v_expr (self#write_texpr fctx);
			);
			self#write_type_instance v.v_type;
		in
		let rec loop e =

			self#write_texpr_type_instance fctx e.etype;
			fctx.pos_writer#write_pos 240 e.epos;

			match e.eexpr with
			(* values 0-19 *)
			| TConst ct ->
				begin match ct with
				| TNull ->
					chunk#write_u8 0;
				| TThis ->
					chunk#write_u8 1;
				| TSuper ->
					chunk#write_u8 2;
				| TBool false ->
					chunk#write_u8 3;
				| TBool true ->
					chunk#write_u8 4;
				| TInt i32 ->
					chunk#write_u8 5;
					chunk#write_u32 i32;
				| TFloat f ->
					chunk#write_u8 6;
					chunk#write_string f;
				| TString s ->
					chunk#write_u8 7;
					chunk#write_string s
				end
			(* vars 20-29 *)
			| TLocal v ->
				chunk#write_u8 20;
				chunk#write_uleb128 (fctx.vars#get v.v_id)
			| TVar(v,None) ->
				chunk#write_u8 21;
				declare_var v;
			| TVar(v,Some e1) ->
				chunk#write_u8 22;
				declare_var v;
				loop e1;
			(* blocks 30-49 *)
			| TBlock [] ->
				chunk#write_u8 30;
			| TBlock el ->
				let l = List.length el in
				begin match l with
				| 1 -> chunk#write_u8 31;
				| 2 -> chunk#write_u8 32;
				| 3 -> chunk#write_u8 33;
				| 4 -> chunk#write_u8 34;
				| 5 -> chunk#write_u8 35;
				| _ ->
					if l <= 0xFF then begin
						chunk#write_u8 36;
						chunk#write_u8 l;
					end else if l < 0xFFFF then begin
						chunk#write_u8 37;
						chunk#write_ui16 l;
					end else begin
						chunk#write_u8 38;
						chunk#write_i32 l;
					end;
				end;
				List.iter loop el
			(* function 50-59 *)
			| TFunction tf ->
				chunk#write_u8 50;
				chunk#write_list tf.tf_args (fun (v,eo) ->
					declare_var v;
					chunk#write_option eo loop;
				);
				self#write_type_instance tf.tf_type;
				loop tf.tf_expr;
			(* texpr compounds 60-79 *)
			| TArray(e1,e2) ->
				chunk#write_u8 60;
				loop e1;
				loop e2;
			| TParenthesis e1 ->
				chunk#write_u8 61;
				loop e1;
			| TArrayDecl el ->
				chunk#write_u8 62;
				loop_el el;
			| TObjectDecl fl ->
				chunk#write_u8 63;
				chunk#write_list fl (fun ((name,p,qs),e) ->
					chunk#write_string name;
					self#write_pos p;
					begin match qs with
					| NoQuotes -> chunk#write_u8 0;
					| DoubleQuotes -> chunk#write_u8 1;
					end;
					loop e
				);
			| TCall(e1,el) ->
				chunk#write_u8 64;
				loop e1;
				loop_el el;
			| TMeta(m,e1) ->
				chunk#write_u8 65;
				self#write_metadata_entry m;
				loop e1;
			(* branching 80-89 *)
			| TIf(e1,e2,None) ->
				chunk#write_u8 80;
				loop e1;
				loop e2;
			| TIf(e1,e2,Some e3) ->
				chunk#write_u8 81;
				loop e1;
				loop e2;
				loop e3;
			| TSwitch s ->
				chunk#write_u8 82;
				loop s.switch_subject;
				chunk#write_list s.switch_cases (fun c ->
					loop_el c.case_patterns;
					loop c.case_expr;
				);
				chunk#write_option s.switch_default loop;
			| TTry(e1,catches) ->
				chunk#write_u8 83;
				loop e1;
				chunk#write_list catches  (fun (v,e) ->
					declare_var v;
					loop e
				);
			| TWhile(e1,e2,flag) ->
				chunk#write_u8 (if flag = NormalWhile then 84 else 85);
				loop e1;
				loop e2;
			| TFor(v,e1,e2) ->
				chunk#write_u8 86;
				declare_var v;
				loop e1;
				loop e2;
			(* control flow 90-99 *)
			| TReturn None ->
				chunk#write_u8 90;
			| TReturn (Some e1) ->
				chunk#write_u8 91;
				loop e1;
			| TContinue ->
				chunk#write_u8 92;
			| TBreak ->
				chunk#write_u8 93;
			| TThrow e1 ->
				chunk#write_u8 94;
				loop e1;
			(* access 100-119 *)
			| TEnumIndex e1 ->
				chunk#write_u8 100;
				loop e1;
			| TEnumParameter(e1,ef,i) ->
				chunk#write_u8 101;
				loop e1;
				let en = match follow ef.ef_type with
					| TFun(_,tr) ->
						begin match follow tr with
							| TEnum(en,_) -> en
							| _ -> die "" __LOC__
						end
					| _ ->
						die "" __LOC__
				in
				self#write_enum_field_ref en ef;
				chunk#write_i32 i;
			| TField(e1,FInstance(c,tl,cf)) ->
				chunk#write_u8 102;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf;
			| TField(e1,FStatic(c,cf)) ->
				chunk#write_u8 103;
				loop e1;
				self#write_class_ref c;
				self#write_field_ref c CfrStatic cf;
			| TField(e1,FAnon cf) ->
				chunk#write_u8 104;
				loop e1;
				self#write_anon_field_ref cf
			| TField(e1,FClosure(Some(c,tl),cf)) ->
				chunk#write_u8 105;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf
			| TField(e1,FClosure(None,cf)) ->
				chunk#write_u8 106;
				loop e1;
				self#write_anon_field_ref cf
			| TField(e1,FEnum(en,ef)) ->
				chunk#write_u8 107;
				loop e1;
				self#write_enum_ref en;
				self#write_enum_field_ref en ef;
			| TField(e1,FDynamic s) ->
				chunk#write_u8 108;
				loop e1;
				chunk#write_string s;
			(* module types 120-139 *)
			| TTypeExpr (TClassDecl ({cl_kind = KTypeParameter ttp})) ->
				chunk#write_u8 128;
				self#write_type_parameter_ref ttp
			| TTypeExpr (TClassDecl c) ->
				chunk#write_u8 120;
				self#write_class_ref c;
			| TTypeExpr (TEnumDecl en) ->
				chunk#write_u8 121;
				self#write_enum_ref en;
			| TTypeExpr (TAbstractDecl a) ->
				chunk#write_u8 122;
				self#write_abstract_ref a
			| TTypeExpr (TTypeDecl td) ->
				chunk#write_u8 123;
				self#write_typedef_ref td
			| TCast(e1,None) ->
				chunk#write_u8 124;
				loop e1;
			| TCast(e1,Some md) ->
				chunk#write_u8 125;
				loop e1;
				let infos = t_infos md in
				let m = infos.mt_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd infos.mt_path);
			| TNew(({cl_kind = KTypeParameter ttp}),tl,el) ->
				chunk#write_u8 127;
				self#write_type_parameter_ref ttp;
				self#write_types tl;
				loop_el el;
			| TNew(c,tl,el) ->
				chunk#write_u8 126;
				self#write_class_ref c;
				self#write_types tl;
				loop_el el;
			(* unops 140-159 *)
			| TUnop(op,flag,e1) ->
				chunk#write_u8 (140 + unop_index op flag);
				loop e1;
			(* binops 160-219 *)
			| TBinop(op,e1,e2) ->
				chunk#write_u8 (160 + binop_index op);
				loop e1;
				loop e2;
			(* pos 241-244 *)
			(* rest 250-254 *)
			| TIdent s ->
				chunk#write_u8 250;
				chunk#write_string s;
		and loop_el el =
			chunk#write_list el loop
		in
		loop e

	(* Fields *)

	method write_type_parameter_forward ttp =
		self#write_path ttp.ttp_class.cl_path;
		self#write_pos ttp.ttp_class.cl_name_pos

	method write_type_parameter_data ttp =
		let c = ttp.ttp_class in
		self#write_metadata c.cl_meta;
		self#write_types (get_constraints ttp);
		chunk#write_option ttp.ttp_default self#write_type_instance

	method write_field_kind = function
		| Method MethNormal -> chunk#write_u8 0;
		| Method MethInline -> chunk#write_u8 1;
		| Method MethDynamic -> chunk#write_u8 2;
		| Method MethMacro -> chunk#write_u8 3;
		(* normal read *)
		| Var {v_read = AccNormal; v_write = AccNormal } -> chunk#write_u8 10
		| Var {v_read = AccNormal; v_write = AccNo } -> chunk#write_u8 11
		| Var {v_read = AccNormal; v_write = AccNever } -> chunk#write_u8 12
		| Var {v_read = AccNormal; v_write = AccCtor } -> chunk#write_u8 13
		| Var {v_read = AccNormal; v_write = AccCall } -> chunk#write_u8 14
		(* inline read *)
		| Var {v_read = AccInline; v_write = AccNever } -> chunk#write_u8 20
		(* getter read *)
		| Var {v_read = AccCall; v_write = AccNormal } -> chunk#write_u8 30
		| Var {v_read = AccCall; v_write = AccNo } -> chunk#write_u8 31
		| Var {v_read = AccCall; v_write = AccNever } -> chunk#write_u8 32
		| Var {v_read = AccCall; v_write = AccCtor } -> chunk#write_u8 33
		| Var {v_read = AccCall; v_write = AccCall } -> chunk#write_u8 34
		(* weird/overlooked combinations *)
		| Var {v_read = r;v_write = w } ->
			chunk#write_u8 100;
			let f = function
				| AccNormal -> chunk#write_u8 0
				| AccNo -> chunk#write_u8 1
				| AccNever -> chunk#write_u8 2
				| AccCtor -> chunk#write_u8 3
				| AccCall -> chunk#write_u8 4
				| AccInline -> chunk#write_u8 5
				| AccRequire(s,so) ->
					chunk#write_u8 6;
					chunk#write_string s;
					chunk#write_option so chunk#write_string
			in
			f r;
			f w;

	method open_field_scope (params : type_params) =
		field_stack <- () :: field_stack;
		let nested = self#in_nested_scope in
		let old_field_params = field_type_parameters in
		let old_local_params = local_type_parameters in
		if not nested then begin
			local_type_parameters <- new identity_pool;
			field_type_parameters <- new identity_pool;
		end;
		List.iter (fun ttp ->
			ignore(field_type_parameters#add ttp ());
		) params;
		(fun () ->
			field_type_parameters <- old_field_params;
			local_type_parameters <- old_local_params;
			field_stack <- List.tl field_stack
		)

	method write_class_field_forward cf =
		chunk#write_string cf.cf_name;
		self#write_pos cf.cf_pos;
		self#write_pos cf.cf_name_pos;
		chunk#write_list cf.cf_overloads (fun cf ->
			self#write_class_field_forward cf;
		);

	method start_texpr (p: pos) =
		let restore = self#start_temporary_chunk in
		let fctx = create_field_writer_context (new pos_writer chunk stats p false) in
		fctx,(fun () ->
			restore(fun new_chunk ->
				let items = fctx.t_pool#items in
				chunk#write_uleb128 (DynArray.length items);
				DynArray.iter (fun bytes ->
					chunk#write_bytes bytes
				) items;

				let items = fctx.vars#items in
				chunk#write_uleb128 (DynArray.length items);
				DynArray.iter (fun v ->
					self#write_var fctx v;
				) items;
				new_chunk#export_data chunk
			)
		)

	method commit_field_type_parameters (params : type_params) =
		chunk#write_uleb128 (List.length params);
		if not self#in_nested_scope then begin
			let ftp = List.map fst field_type_parameters#to_list in
			chunk#write_list ftp self#write_type_parameter_forward;
			chunk#write_list ftp self#write_type_parameter_data;
		end

	method write_class_field_data cf =
		let restore = self#start_temporary_chunk in
		(try self#write_type_instance cf.cf_type with e -> begin
			prerr_endline (Printf.sprintf "%s while writing type instance for field %s" todo_error cf.cf_name);
			raise e
		end);
		chunk#write_i32 cf.cf_flags;
		chunk#write_option cf.cf_doc self#write_documentation;
		self#write_metadata cf.cf_meta;
		self#write_field_kind cf.cf_kind;
		begin match cf.cf_expr with
			| None ->
				chunk#write_u8 0
			| Some e ->
				chunk#write_u8 1;
				let fctx,close = self#start_texpr e.epos in
				self#write_texpr fctx e;
				chunk#write_option cf.cf_expr_unoptimized (self#write_texpr fctx);
				close();
		end;

		restore (fun new_chunk ->
			self#commit_field_type_parameters cf.cf_params;
			if not self#in_nested_scope then begin
				let ltp = List.map fst local_type_parameters#to_list in
				chunk#write_list ltp self#write_type_parameter_forward;
				chunk#write_list ltp self#write_type_parameter_data;
			end;
			new_chunk#export_data chunk
		)

	method write_class_field_and_overloads_data (cf : tclass_field) =
		let write cf =
			let close = self#open_field_scope cf.cf_params in
			self#write_class_field_data cf;
			close();
		in
		write cf;
		chunk#write_list cf.cf_overloads write

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
			chunk#write_u8 0
		| KTypeParameter ttp ->
			die "TODO" __LOC__
		| KExpr e ->
			chunk#write_u8 2;
			self#write_expr e;
		| KGeneric ->
			chunk#write_u8 3;
		| KGenericInstance(c,tl) ->
			chunk#write_u8 4;
			self#write_class_ref c;
			self#write_types tl
		| KMacroType ->
			chunk#write_u8 5;
		| KGenericBuild l ->
			chunk#write_u8 6;
			chunk#write_list l self#write_cfield;
		| KAbstractImpl a ->
			chunk#write_u8 7;
			self#write_abstract_ref a;
		| KModuleFields md ->
			chunk#write_u8 8;

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
			prerr_endline ("Could not select abstract " ^ (s_type_path a.a_path));
		end;
		self#write_common_module_type (Obj.magic a);
		chunk#write_option a.a_impl self#write_class_ref;
		if Meta.has Meta.CoreType a.a_meta then
			chunk#write_u8 0
		else begin
			chunk#write_u8 1;
			self#write_type_instance a.a_this;
		end;
		chunk#write_list a.a_from self#write_type_instance;
		chunk#write_list a.a_to self#write_type_instance;
		chunk#write_bool a.a_enum

	method write_abstract_fields (a : tabstract) =
		let c = match a.a_impl with
			| None ->
				null_class
			| Some c ->
				c
		in

		chunk#write_list a.a_array (self#write_field_ref c CfrStatic);
		chunk#write_option a.a_read (self#write_field_ref c CfrStatic );
		chunk#write_option a.a_write (self#write_field_ref c CfrStatic);
		chunk#write_option a.a_call (self#write_field_ref c CfrStatic);

		chunk#write_list a.a_ops (fun (op, cf) ->
			chunk#write_u8 (binop_index op);
			self#write_field_ref c CfrStatic cf
		);

		chunk#write_list a.a_unops (fun (op, flag, cf) ->
			chunk#write_u8 (unop_index op flag);
			self#write_field_ref c CfrStatic cf
		);

		chunk#write_list a.a_from_field (fun (t,cf) ->
			self#write_field_ref c CfrStatic cf;
		);

		chunk#write_list a.a_to_field (fun (t,cf) ->
			self#write_field_ref c CfrStatic cf;
		);

	method write_enum (e : tenum) =
		self#select_type e.e_path;
		self#write_common_module_type (Obj.magic e);
		chunk#write_bool e.e_extern;
		chunk#write_list e.e_names chunk#write_string;

	method write_typedef (td : tdef) =
		self#select_type td.t_path;
		self#write_common_module_type (Obj.magic td);
		self#write_type_instance td.t_type;

	method write_anon (an : tanon) (ttp : type_params) =
		let write_fields () =
			chunk#write_list (PMap.foldi (fun s f acc -> (s,f) :: acc) an.a_fields []) (fun (_,cf) ->
				self#write_anon_field_ref cf
			)
		in

		begin match !(an.a_status) with
		| Closed ->
			chunk#write_u8 0;
			write_fields ()
		| Const ->
			chunk#write_u8 1;
			write_fields ()
		| Extend tl ->
			chunk#write_u8 2;
			self#write_types tl;
			write_fields ()
		| ClassStatics _ ->
			assert false
		| EnumStatics _ ->
			assert false
		| AbstractStatics _ ->
			assert false
		end;

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
		chunk#write_u8 i;
		self#write_full_path (fst infos.mt_path) (snd infos.mt_path) !name;
		self#write_pos infos.mt_pos;
		self#write_pos infos.mt_name_pos;
		let params = new pool in
		type_type_parameters <- params;
		ignore(type_param_lut#add infos.mt_path params);
		List.iter (fun ttp ->
			ignore(type_type_parameters#add ttp.ttp_name ttp)
		) infos.mt_params;

		(* Forward declare fields *)
		match mt with
		| TClassDecl c ->
			chunk#write_option c.cl_constructor self#write_class_field_forward;
			chunk#write_list c.cl_ordered_fields self#write_class_field_forward;
			chunk#write_list c.cl_ordered_statics self#write_class_field_forward;
		| TEnumDecl e ->
			chunk#write_list (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
				chunk#write_string s;
				self#write_pos ef.ef_pos;
				self#write_pos ef.ef_name_pos;
				chunk#write_u8 ef.ef_index
			);
		| TAbstractDecl a ->
			(* TODO ? *)
			()
		| TTypeDecl t ->
			(* TODO ? *)
			()

	method write_module (m : module_def) =
		current_module <- m;

		self#start_chunk TYPF;
		chunk#write_list m.m_types self#forward_declare_type;

		begin match own_abstracts#to_list with
		| [] ->
			()
		| own_abstracts ->
			self#start_chunk ABSD;
			chunk#write_list own_abstracts self#write_abstract;
			self#start_chunk AFLD;
			chunk#write_list own_abstracts self#write_abstract_fields;
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

				let write_field cf =
					self#write_class_field_and_overloads_data cf;
				in

				chunk#write_option c.cl_constructor write_field;
				chunk#write_list c.cl_ordered_fields write_field;
				chunk#write_list c.cl_ordered_statics write_field;
				chunk#write_option c.cl_init (fun e ->
					let fctx,close = self#start_texpr e.epos in
					self#write_texpr fctx e;
					close()
				);
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
					self#select_type e.e_path;
					let close = self#open_field_scope ef.ef_params in
					chunk#write_string s;
					let restore = self#start_temporary_chunk in
					self#write_type_instance ef.ef_type;
					let t_bytes = restore (fun new_chunk -> new_chunk#get_bytes) in
					self#commit_field_type_parameters ef.ef_params;
					chunk#write_bytes t_bytes;
					chunk#write_option ef.ef_doc self#write_documentation;
					self#write_metadata ef.ef_meta;
					close();
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

		begin match classes#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk CLSR;
			chunk#write_list l (fun c ->
				let m = c.cl_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd c.cl_path);
			)
		end;
		begin match abstracts#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ABSR;
			chunk#write_list l (fun a ->
				let m = a.a_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd a.a_path);
			)
		end;
		begin match enums#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ENMR;
			chunk#write_list l (fun en ->
				let m = en.e_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd en.e_path);
			)
		end;
		begin match typedefs#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk TPDR;
			chunk#write_list l (fun td ->
				let m = td.t_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd td.t_path);
			)
		end;

		self#start_chunk CFLR;
		let items = class_fields#items in
		chunk#write_uleb128 (DynArray.length items);
		DynArray.iter (fun (cf,(c,kind,depth)) ->
			self#write_class_ref c;
			begin match kind with
			| CfrStatic ->
				chunk#write_u8 0;
				chunk#write_string cf.cf_name
			| CfrMember ->
				chunk#write_u8 1;
				chunk#write_string cf.cf_name
			| CfrConstructor ->
				chunk#write_u8 2;
			end;
			chunk#write_uleb128 depth
		) items;

		self#start_chunk ENFR;
		let items = enum_fields#items in
		chunk#write_uleb128 (DynArray.length items);
		DynArray.iter (fun (en,ef) ->
			self#write_enum_ref en;
			chunk#write_string ef.ef_name;
		) items;

		self#start_chunk ANFR;
		let items = anon_fields#items in
		chunk#write_uleb128 (DynArray.length items);
		DynArray.iter (fun (cf,_) ->
			chunk#write_string cf.cf_name;
			self#write_pos cf.cf_pos;
			self#write_pos cf.cf_name_pos;
		) items;

		self#start_chunk HHDR;
		self#write_path m.m_path;
		chunk#write_string (Path.UniqueKey.lazy_path m.m_extra.m_file);
		chunk#write_uleb128 (DynArray.length anons#items);
		chunk#write_uleb128 (DynArray.length tmonos#items);
		self#start_chunk HEND;

	(* Export *)

	method export : 'a . 'a IO.output -> unit = fun ch ->
		IO.nwrite_string ch "hxb";
		IO.write_byte ch hxb_version;
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
