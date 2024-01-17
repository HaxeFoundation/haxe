open Globals
open Ast
open Type
open HxbData
open Tanon_identification

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

type hxb_writer_stats = {
	type_instance_kind_writes : int array;
	texpr_writes : int array;
	type_instance_immediate : int ref;
	type_instance_cache_hits : int ref;
	type_instance_cache_misses : int ref;
	pos_writes_full : int ref;
	pos_writes_min : int ref;
	pos_writes_max : int ref;
	pos_writes_minmax : int ref;
	pos_writes_eq : int ref;
	chunk_sizes : (string,int ref * int ref) Hashtbl.t;
}

let create_hxb_writer_stats () = {
	type_instance_kind_writes = Array.make 255 0;
	texpr_writes = Array.make 255 0;
	type_instance_immediate = ref 0;
	type_instance_cache_hits = ref 0;
	type_instance_cache_misses = ref 0;
	pos_writes_full = ref 0;
	pos_writes_min = ref 0;
	pos_writes_max = ref 0;
	pos_writes_minmax = ref 0;
	pos_writes_eq = ref 0;
	chunk_sizes = Hashtbl.create 0;
}

let dump_stats name stats =
	let sort_and_filter_array a =
		let _,kind_writes = Array.fold_left (fun (index,acc) writes ->
			(index + 1,if writes = 0 then acc else (index,writes) :: acc)
		) (0,[]) a in
		let kind_writes = List.sort (fun (_,writes1) (_,writes2) -> compare writes2 writes1) kind_writes in
		List.map (fun (index,writes) -> Printf.sprintf "    %3i: %9i" index writes) kind_writes
	in
	let t_kind_writes = sort_and_filter_array stats.type_instance_kind_writes in
	print_endline (Printf.sprintf "hxb_writer stats for %s" name);
	print_endline "  type instance kind writes:";
	List.iter print_endline t_kind_writes;
	let texpr_writes = sort_and_filter_array stats.texpr_writes in
	print_endline "  texpr writes:";
	List.iter print_endline texpr_writes;

	print_endline "  type instance writes:";
	print_endline (Printf.sprintf "     immediate: %9i" !(stats.type_instance_immediate));
	print_endline (Printf.sprintf "    cache hits: %9i" !(stats.type_instance_cache_hits));
	print_endline (Printf.sprintf "    cache miss: %9i" !(stats.type_instance_cache_misses));
	print_endline "  pos writes:";
	print_endline (Printf.sprintf "      full: %9i\n       min: %9i\n       max: %9i\n    minmax: %9i\n     equal: %9i" !(stats.pos_writes_full) !(stats.pos_writes_min) !(stats.pos_writes_max) !(stats.pos_writes_minmax) !(stats.pos_writes_eq));
	(* let chunk_sizes = Hashtbl.fold (fun name (imin,imax) acc -> (name,!imin,!imax) :: acc) stats.chunk_sizes [] in
	let chunk_sizes = List.sort (fun (_,imin1,imax1) (_,imin2,imax2) -> compare imax1 imax2) chunk_sizes in
	print_endline "chunk sizes:";
	List.iter (fun (name,imin,imax) ->
		print_endline (Printf.sprintf "    %s: %i - %i" name imin imax)
	) chunk_sizes *)

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

	method get_or_add (key : 'key) (value : 'value) =
		try
			self#get key
		with Not_found ->
			self#add key value

	method to_list =
		DynArray.to_list items

	method items = items

	method length = DynArray.length items
end

class ['hkey,'key,'value] hashed_identity_pool = object(self)
	val lut = Hashtbl.create 0
	val items = DynArray.create ()

	method add (hkey : 'hkey) (key : 'key) (value : 'value) =
		let index = DynArray.length items in
		DynArray.add items (key,value);
		Hashtbl.add lut hkey (key,index);
		index

	method get (hkey : 'hkey) (key : 'key) =
		let l = Hashtbl.find_all lut hkey in
		List.assq key l

	method items = items
end

module SimnBuffer = struct
	type t = {
		buffer_size : int;
		mutable buffer : bytes;
		mutable buffers : bytes Queue.t;
		mutable offset : int;
	}

	let create buffer_size = {
		buffer = Bytes.create buffer_size;
		buffers = Queue.create ();
		offset = 0;
		buffer_size = buffer_size;
	}

	let promote_buffer sb =
		Queue.add sb.buffer sb.buffers;
		sb.buffer <- Bytes.create sb.buffer_size;
		sb.offset <- 0

	let add_u8 sb i =
		if sb.offset = sb.buffer_size then begin
			(* Current buffer is full, promote it. *)
			promote_buffer sb;
			Bytes.unsafe_set sb.buffer 0 i;
			sb.offset <- 1;
		end else begin
			(* There's room, put it in. *)
			Bytes.unsafe_set sb.buffer sb.offset i;
			sb.offset <- sb.offset + 1
		end

	let add_bytes sb bytes =
		let rec loop offset left =
			let space = sb.buffer_size - sb.offset in
			if left > space then begin
				(* We need more than we have. Blit as much as we can, promote buffer, recurse. *)
				Bytes.unsafe_blit bytes offset sb.buffer sb.offset space;
				promote_buffer sb;
				loop (offset + space) (left - space)
			end else begin
				(* It fits, blit it. *)
				Bytes.unsafe_blit bytes offset sb.buffer sb.offset left;
				sb.offset <- sb.offset + left;
			end
		in
		loop 0 (Bytes.length bytes)

	let contents sb =
		let size = sb.offset + sb.buffer_size * Queue.length sb.buffers in
		let out = Bytes.create size in
		let offset = ref 0 in
		(* We know that all sb.buffers are of sb.buffer_size length, so blit them together. *)
		Queue.iter (fun bytes ->
			Bytes.unsafe_blit bytes 0 out !offset sb.buffer_size;
			offset := !offset + sb.buffer_size;
		) sb.buffers;
		(* Append our current buffer until sb.offset *)
		Bytes.unsafe_blit sb.buffer 0 out !offset sb.offset;
		out
end

module Chunk = struct
	type t = {
		kind : chunk_kind;
		cp : (string,string) pool;
		ch : SimnBuffer.t;
	}

	let create kind cp initial_size = {
		kind;
		cp;
		ch = SimnBuffer.create initial_size;
	}

	let write_u8 io v =
		SimnBuffer.add_u8 io.ch (Char.unsafe_chr v)

	let write_i32 io v =
		let base = Int32.to_int v in
		let big = Int32.to_int (Int32.shift_right_logical v 24) in
		write_u8 io base;
		write_u8 io (base lsr 8);
		write_u8 io (base lsr 16);
		write_u8 io big

	let write_i64 io v =
		write_i32 io (Int64.to_int32 v);
		write_i32 io (Int64.to_int32 (Int64.shift_right_logical v 32))

	let write_f64 io v =
		write_i64 io (Int64.bits_of_float v)

	let write_bytes io b =
		SimnBuffer.add_bytes io.ch b

	let write_ui16 io i =
		write_u8 io i;
		write_u8 io (i lsr 8)

	let get_bytes io =
		SimnBuffer.contents io.ch

	let rec write_uleb128 io v =
		let b = v land 0x7F in
		let rest = v lsr 7 in
		if rest = 0 then
			write_u8 io b
		else begin
			write_u8 io (b lor 0x80);
			write_uleb128 io rest
		end

	let rec write_leb128 io v =
		let b = v land 0x7F in
		let rest = v asr 7 in
		if (rest = 0 && (b land 0x40 = 0)) || (rest = -1 && (b land 0x40 = 0x40)) then
			write_u8 io b
		else begin
			write_u8 io (b lor 0x80);
			write_leb128 io rest
		end

	let write_bytes_length_prefixed io b =
		write_uleb128 io (Bytes.length b);
		write_bytes io b

	let write_bool io b =
		write_u8 io (if b then 1 else 0)

	let export : 'a . hxb_writer_stats -> t -> 'a IO.output -> unit = fun stats io chex ->
		let bytes = get_bytes io in
		let length = Bytes.length bytes in
		write_chunk_prefix io.kind length chex;
		(* begin try
			let (imin,imax) = Hashtbl.find stats.chunk_sizes io.name in
			if length < !imin then imin := length;
			if length > !imax then imax := length
		with Not_found ->
			Hashtbl.add stats.chunk_sizes io.name (ref length,ref length);
		end; *)
		IO.nwrite chex bytes

	let write_string chunk s =
		write_uleb128 chunk (chunk.cp#get_or_add s s)

	let write_list : 'b . t -> 'b list -> ('b -> unit) -> unit = fun chunk l f ->
		write_uleb128 chunk (List.length l);
		List.iter f l

	let write_option : 'b . t -> 'b option -> ('b -> unit) -> unit = fun chunk v f -> match v with
	| None ->
		write_u8 chunk 0
	| Some v ->
		write_u8 chunk 1;
		f v

	let export_data chunk_from chunk_to =
		let bytes = get_bytes chunk_from in
		write_bytes chunk_to bytes
end

class pos_writer
	(chunk_initial : Chunk.t)
	(stats : hxb_writer_stats)
	(p_initial : pos)
= object(self)

	val mutable p_file = p_initial.pfile
	val mutable p_min = p_initial.pmin
	val mutable p_max = p_initial.pmax

	method private do_write_pos (chunk : Chunk.t) (p : pos) =
		(* incr stats.pos_writes_full; *)
		Chunk.write_string chunk p.pfile;
		Chunk.write_leb128 chunk p.pmin;
		Chunk.write_leb128 chunk p.pmax;

	method write_pos (chunk : Chunk.t) (write_equal : bool) (offset : int) (p : pos) =
		if p.pfile != p_file then begin
			(* File changed, write full pos *)
			Chunk.write_u8 chunk (4 + offset);
			self#do_write_pos chunk p;
			p_file <- p.pfile;
			p_min <- p.pmin;
			p_max <- p.pmax;
		end else if p.pmin <> p_min then begin
			if p.pmax <> p_max then begin
				(* pmin and pmax changed *)
				(* incr stats.pos_writes_minmax; *)
				Chunk.write_u8 chunk (3 + offset);
				Chunk.write_leb128 chunk p.pmin;
				Chunk.write_leb128 chunk p.pmax;
				p_min <- p.pmin;
				p_max <- p.pmax;
			end else begin
				(* pmin changed *)
				(* incr stats.pos_writes_min; *)
				Chunk.write_u8 chunk (1 + offset);
				Chunk.write_leb128 chunk p.pmin;
				p_min <- p.pmin;
			end
		end else if p.pmax <> p_max then begin
			(* pmax changed *)
			(* incr stats.pos_writes_max; *)
			Chunk.write_u8 chunk (2 + offset);
			Chunk.write_leb128 chunk p.pmax;
			p_max <- p.pmax;
		end else begin
			(* incr stats.pos_writes_eq; *)
			if write_equal then
				Chunk.write_u8 chunk offset;
		end

	initializer
		self#do_write_pos chunk_initial p_initial
end

type field_writer_context = {
	t_pool : (bytes,bytes) pool;
	pos_writer : pos_writer;
	mutable texpr_this : texpr option;
	vars : (int,tvar) pool;
}

let create_field_writer_context pos_writer = {
	t_pool = new pool;
	pos_writer = pos_writer;
	texpr_this = None;
	vars = new pool;
}

class hxb_writer
	(display_source_at : Globals.pos -> unit)
	(warn : Warning.warning -> string -> Globals.pos -> unit)
	(anon_id : Type.t Tanon_identification.tanon_identification)
	(stats : hxb_writer_stats)
= object(self)

	val mutable current_module = null_module
	val chunks = DynArray.create ()
	val cp = new pool
	val docs = new pool

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
	val class_fields = new hashed_identity_pool
	val enum_fields = new pool
	val mutable type_type_parameters = new pool
	val mutable field_type_parameters = new identity_pool
	val mutable local_type_parameters = new identity_pool

	val instance_overload_cache = Hashtbl.create 0

	val mutable field_stack = []

	method in_nested_scope = match field_stack with
		| [] -> false (* can happen for cl_init and in EXD *)
		| [_] -> false
		| _ -> true

	(* Chunks *)

	method start_chunk (kind : chunk_kind) =
		let initial_size = match kind with
			| EOT | EOF | EOM -> 0
			| MDF -> 16
			| MTF | CLR | END | ABD | ENR | ABR | TDR | EFR | CFR | AFD -> 64
			| AFR | CLD | TDD | EFD -> 128
			| STR | DOC -> 256
			| CFD | EXD -> 512
		in
		let new_chunk = Chunk.create kind cp initial_size in
		DynArray.add chunks new_chunk;
		chunk <- new_chunk

	method start_temporary_chunk : 'a . int -> (Chunk.t -> 'a) -> 'a = fun initial_size ->
		let new_chunk = Chunk.create EOM (* TODO: something else? *) cp initial_size in
		let old_chunk = chunk in
		chunk <- new_chunk;
		(fun f ->
			chunk <- old_chunk;
			f new_chunk
		)

	(* Basic compounds *)

	method write_path (path : path) =
		Chunk.write_list chunk (fst path) (Chunk.write_string chunk);
		Chunk.write_string chunk (snd path);

	method write_full_path (pack : string list) (mname : string) (tname : string) =
		Chunk.write_list chunk pack (Chunk.write_string chunk);
		Chunk.write_string chunk mname;
		Chunk.write_string chunk tname;

	method write_documentation (doc : doc_block) =
		Chunk.write_option chunk doc.doc_own (fun s ->
			Chunk.write_uleb128 chunk (docs#get_or_add s s)
		);
		Chunk.write_list chunk doc.doc_inherited (fun s ->
			Chunk.write_uleb128 chunk (docs#get_or_add s s)
		);

	method write_pos (p : pos) =
		Chunk.write_string chunk p.pfile;
		Chunk.write_leb128 chunk p.pmin;
		Chunk.write_leb128 chunk p.pmax;

	method write_metadata_entry ((meta,el,p) : metadata_entry) =
		Chunk.write_string chunk (Meta.to_string meta);
		self#write_pos p;
		Chunk.write_list chunk el self#write_expr;

	method write_metadata ml =
		Chunk.write_list chunk ml self#write_metadata_entry


	(* expr *)

	method write_object_field_key (n,p,qs) =
		Chunk.write_string chunk n;
		self#write_pos p;
		begin match qs with
			| NoQuotes -> Chunk.write_u8 chunk 0
			| DoubleQuotes -> Chunk.write_u8 chunk 1
		end

	method write_type_path tp =
		Chunk.write_list chunk tp.tpackage (Chunk.write_string chunk);
		Chunk.write_string chunk tp.tname;
		Chunk.write_list chunk tp.tparams self#write_type_param_or_const;
		Chunk.write_option chunk tp.tsub (Chunk.write_string chunk)

	method write_placed_type_path ptp =
		self#write_type_path ptp.path;
		self#write_pos ptp.pos_full;
		self#write_pos ptp.pos_path

	method write_type_param_or_const = function
		| TPType th ->
			Chunk.write_u8 chunk 0;
			self#write_type_hint th
		| TPExpr e ->
			Chunk.write_u8 chunk 1;
			self#write_expr e

	method write_complex_type = function
		| CTPath tp ->
			Chunk.write_u8 chunk 0;
			self#write_placed_type_path tp
		| CTFunction(thl,th) ->
			Chunk.write_u8 chunk 1;
			Chunk.write_list chunk thl self#write_type_hint;
			self#write_type_hint th
		| CTAnonymous cffl ->
			Chunk.write_u8 chunk 2;
			Chunk.write_list chunk cffl self#write_cfield;
		| CTParent th ->
			Chunk.write_u8 chunk 3;
			self#write_type_hint th
		| CTExtend(ptp,cffl) ->
			Chunk.write_u8 chunk 4;
			Chunk.write_list chunk ptp self#write_placed_type_path;
			Chunk.write_list chunk cffl self#write_cfield;
		| CTOptional th ->
			Chunk.write_u8 chunk 5;
			self#write_type_hint th
		| CTNamed(pn,th) ->
			Chunk.write_u8 chunk 6;
			self#write_placed_name pn;
			self#write_type_hint th
		| CTIntersection(thl) ->
			Chunk.write_u8 chunk 7;
			Chunk.write_list chunk thl self#write_type_hint;

	method write_type_hint (ct,p) =
		self#write_complex_type ct;
		self#write_pos p

	method write_type_param tp =
		self#write_placed_name tp.tp_name;
		Chunk.write_list chunk tp.tp_params self#write_type_param;
		Chunk.write_option chunk tp.tp_constraints self#write_type_hint;
		Chunk.write_option chunk tp.tp_default self#write_type_hint;
		Chunk.write_list chunk tp.tp_meta self#write_metadata_entry;

	method write_func_arg (pn,b,meta,tho,eo) =
		self#write_placed_name pn;
		Chunk.write_bool chunk b;
		self#write_metadata meta;
		Chunk.write_option chunk tho self#write_type_hint;
		Chunk.write_option chunk eo self#write_expr;

	method write_func f =
		Chunk.write_list chunk f.f_params self#write_type_param;
		Chunk.write_list chunk f.f_args self#write_func_arg;
		Chunk.write_option chunk f.f_type self#write_type_hint;
		Chunk.write_option chunk f.f_expr self#write_expr

	method write_placed_name (s,p) =
		Chunk.write_string chunk s;
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
		Chunk.write_u8 chunk i;

	method write_placed_access (ac,p) =
		self#write_access ac;
		self#write_pos p;

	method write_cfield_kind = function
		| FVar(tho,eo) ->
			Chunk.write_u8 chunk 0;
			Chunk.write_option chunk tho self#write_type_hint;
			Chunk.write_option chunk eo self#write_expr;
		| FFun f ->
			Chunk.write_u8 chunk 1;
			self#write_func f;
		| FProp(pn1,pn2,tho,eo) ->
			Chunk.write_u8 chunk 2;
			self#write_placed_name pn1;
			self#write_placed_name pn2;
			Chunk.write_option chunk tho self#write_type_hint;
			Chunk.write_option chunk eo self#write_expr;

	method write_cfield cff =
		self#write_placed_name cff.cff_name;
		Chunk.write_option chunk cff.cff_doc self#write_documentation;
		self#write_pos cff.cff_pos;
		self#write_metadata cff.cff_meta;
		Chunk.write_list chunk cff.cff_access self#write_placed_access;
		self#write_cfield_kind cff.cff_kind;

	method write_expr (e,p) =
		self#write_pos p;
		match e with
		| EConst (Int (s, suffix)) ->
			Chunk.write_u8 chunk 0;
			Chunk.write_string chunk s;
			Chunk.write_option chunk suffix (Chunk.write_string chunk);
		| EConst (Float (s, suffix)) ->
			Chunk.write_u8 chunk 1;
			Chunk.write_string chunk s;
			Chunk.write_option chunk suffix (Chunk.write_string chunk);
		| EConst (String (s,qs)) ->
			Chunk.write_u8 chunk 2;
			Chunk.write_string chunk s;
			begin match qs with
			| SDoubleQuotes -> Chunk.write_u8 chunk 0;
			| SSingleQuotes -> Chunk.write_u8 chunk 1;
			end
		| EConst (Ident s) ->
			Chunk.write_u8 chunk 3;
			Chunk.write_string chunk s;
		| EConst (Regexp(s1,s2)) ->
			Chunk.write_u8 chunk 4;
			Chunk.write_string chunk s1;
			Chunk.write_string chunk s2;
		| EArray(e1,e2) ->
			Chunk.write_u8 chunk 5;
			self#write_expr e1;
			self#write_expr e2;
		| EBinop(op,e1,e2) ->
			Chunk.write_u8 chunk 6;
			Chunk.write_u8 chunk (binop_index op);
			self#write_expr e1;
			self#write_expr e2;
		| EField(e1,s,kind) ->
			Chunk.write_u8 chunk 7;
			self#write_expr e1;
			Chunk.write_string chunk s;
			begin match kind with
			| EFNormal -> Chunk.write_u8 chunk 0;
			| EFSafe -> Chunk.write_u8 chunk 1;
			end
		| EParenthesis e1 ->
			Chunk.write_u8 chunk 8;
			self#write_expr e1
		| EObjectDecl fl ->
			Chunk.write_u8 chunk 9;
			let write_field (k,e1) =
				self#write_object_field_key k;
				self#write_expr e1
			in
			Chunk.write_list chunk fl write_field;
		| EArrayDecl el ->
			Chunk.write_u8 chunk 10;
			Chunk.write_list chunk el self#write_expr;
		| ECall(e1,el) ->
			Chunk.write_u8 chunk 11;
			self#write_expr e1;
			Chunk.write_list chunk el self#write_expr
		| ENew(ptp,el) ->
			Chunk.write_u8 chunk 12;
			self#write_placed_type_path ptp;
			Chunk.write_list chunk el self#write_expr;
		| EUnop(op,flag,e1) ->
			Chunk.write_u8 chunk 13;
			Chunk.write_u8 chunk (unop_index op flag);
			self#write_expr e1;
		| EVars vl ->
			Chunk.write_u8 chunk 14;
			let write_var v =
				self#write_placed_name v.ev_name;
				Chunk.write_bool chunk v.ev_final;
				Chunk.write_bool chunk v.ev_static;
				Chunk.write_option chunk v.ev_type self#write_type_hint;
				Chunk.write_option chunk v.ev_expr self#write_expr;
				self#write_metadata v.ev_meta;
			in
			Chunk.write_list chunk vl write_var
		| EFunction(fk,f) ->
			Chunk.write_u8 chunk 15;
			begin match fk with
			| FKAnonymous -> Chunk.write_u8 chunk 0;
			| FKNamed (pn,inline) ->
				Chunk.write_u8 chunk 1;
				self#write_placed_name pn;
				Chunk.write_bool chunk inline;
			| FKArrow -> Chunk.write_u8 chunk 2;
			end;
			self#write_func f;
		| EBlock el ->
			Chunk.write_u8 chunk 16;
			Chunk.write_list chunk el self#write_expr
		| EFor(e1,e2) ->
			Chunk.write_u8 chunk 17;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,None) ->
			Chunk.write_u8 chunk 18;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,Some e3) ->
			Chunk.write_u8 chunk 19;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| EWhile(e1,e2,NormalWhile) ->
			Chunk.write_u8 chunk 20;
			self#write_expr e1;
			self#write_expr e2;
		| EWhile(e1,e2,DoWhile) ->
			Chunk.write_u8 chunk 21;
			self#write_expr e1;
			self#write_expr e2;
		| ESwitch(e1,cases,def) ->
			Chunk.write_u8 chunk 22;
			self#write_expr e1;
			let write_case (el,eg,eo,p) =
				Chunk.write_list chunk el self#write_expr;
				Chunk.write_option chunk eg self#write_expr;
				Chunk.write_option chunk eo self#write_expr;
				self#write_pos p;
			in
			Chunk.write_list chunk cases write_case;
			let write_default (eo,p) =
				Chunk.write_option chunk eo self#write_expr;
				self#write_pos p
			in
			Chunk.write_option chunk def write_default;
		| ETry(e1,catches) ->
			Chunk.write_u8 chunk 23;
			self#write_expr e1;
			let write_catch (pn,th,e,p) =
				self#write_placed_name pn;
				Chunk.write_option chunk th self#write_type_hint;
				self#write_expr e;
				self#write_pos p;
			in
			Chunk.write_list chunk catches write_catch;
		| EReturn None ->
			Chunk.write_u8 chunk 24;
		| EReturn (Some e1) ->
			Chunk.write_u8 chunk 25;
			self#write_expr e1;
		| EBreak ->
			Chunk.write_u8 chunk 26;
		| EContinue ->
			Chunk.write_u8 chunk 27;
		| EUntyped e1 ->
			Chunk.write_u8 chunk 28;
			self#write_expr e1;
		| EThrow e1 ->
			Chunk.write_u8 chunk 29;
			self#write_expr e1;
		| ECast(e1,None) ->
			Chunk.write_u8 chunk 30;
			self#write_expr e1;
		| ECast(e1,Some th) ->
			Chunk.write_u8 chunk 31;
			self#write_expr e1;
			self#write_type_hint th;
		| EIs(e1,th) ->
			Chunk.write_u8 chunk 32;
			self#write_expr e1;
			self#write_type_hint th;
		| EDisplay(e1,dk) ->
			Chunk.write_u8 chunk 33;
			self#write_expr e1;
			begin match dk with
			| DKCall -> Chunk.write_u8 chunk 0;
			| DKDot -> Chunk.write_u8 chunk 1;
			| DKStructure -> Chunk.write_u8 chunk 2;
			| DKMarked -> Chunk.write_u8 chunk 3;
			| DKPattern b ->
				Chunk.write_u8 chunk 4;
				Chunk.write_bool chunk b;
			end
		| ETernary(e1,e2,e3) ->
			Chunk.write_u8 chunk 34;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| ECheckType(e1,th) ->
			Chunk.write_u8 chunk 35;
			self#write_expr e1;
			self#write_type_hint th;
		| EMeta(m,e1) ->
			Chunk.write_u8 chunk 36;
			self#write_metadata_entry m;
			self#write_expr e1

	(* References *)

	method write_class_ref (c : tclass) =
		let i = classes#get_or_add c.cl_path c in
		Chunk.write_uleb128 chunk i

	method write_enum_ref (en : tenum) =
		let i = enums#get_or_add en.e_path en in
		Chunk.write_uleb128 chunk i

	method write_typedef_ref (td : tdef) =
		let i = typedefs#get_or_add td.t_path td in
		Chunk.write_uleb128 chunk i

	method write_abstract_ref (a : tabstract) =
		let i = abstracts#get_or_add a.a_path a in
		Chunk.write_uleb128 chunk i

	method write_anon_ref (an : tanon) (ttp : type_params) =
		let pfm = Option.get (anon_id#identify_anon ~strict:true an) in
		try
			let index = anons#get pfm.pfm_path in
			Chunk.write_u8 chunk 0;
			Chunk.write_uleb128 chunk index
		with Not_found ->
			let index = anons#add pfm.pfm_path an in
			Chunk.write_u8 chunk 1;
			Chunk.write_uleb128 chunk index;
			self#write_anon an ttp

	method write_tmono_ref (mono : tmono) =
		let index = try tmonos#get mono with Not_found -> tmonos#add mono () in
		Chunk.write_uleb128 chunk index;

	method write_field_ref (c : tclass) (kind : class_field_ref_kind)  (cf : tclass_field) =
		let index = try
			class_fields#get cf.cf_name cf
		with Not_found ->
			let find_overload c cf_base =
				let rec loop depth cfl = match cfl with
					| cf' :: cfl ->
						if cf' == cf then
							Some(c,depth)
						else
							loop (depth + 1) cfl
					| [] ->
						None
				in
				let cfl = cf_base :: cf_base.cf_overloads in
				loop 0 cfl
			in
			let find_overload c =
				try
					find_overload c (find_field c cf.cf_name kind)
				with Not_found ->
					None
			in
			let r = match kind with
				| CfrStatic | CfrConstructor ->
					find_overload c;
				| CfrMember ->
					(* For member overloads we need to find the correct class, which is a mess. *)
					let rec loop c = match find_overload c with
						| Some _ as r ->
							r
						| None ->
							if has_class_flag c CInterface then
								let rec loopi l = match l with
									| [] ->
										None
									| (c,_) :: l ->
										match loop c with
										| Some _ as r ->
											r
										| None ->
											loopi l
								in
								loopi c.cl_implements
							else match c.cl_super with
								| Some(c,_) ->
									loop c
								| None ->
									None
					in
					loop c;
			in
			let c,depth = match r with
				| None ->
					print_endline (Printf.sprintf "Could not resolve %s overload for %s on %s" (s_class_field_ref_kind kind) cf.cf_name (s_type_path c.cl_path));
					c,0
				| Some(c,depth) ->
					c,depth
			in
			class_fields#add cf.cf_name cf (c,kind,depth)
		in
		Chunk.write_uleb128 chunk index

	method write_enum_field_ref (en : tenum) (ef : tenum_field) =
		let key = (en.e_path,ef.ef_name) in
		try
			Chunk.write_uleb128 chunk (enum_fields#get key)
		with Not_found ->
			ignore(enums#get_or_add en.e_path en);
			Chunk.write_uleb128 chunk (enum_fields#add key (en,ef))

	method write_anon_field_ref cf =
		try
			let index = anon_fields#get cf in
			Chunk.write_u8 chunk 0;
			Chunk.write_uleb128 chunk index
		with Not_found ->
			let index = anon_fields#add cf () in
			Chunk.write_u8 chunk 1;
			Chunk.write_uleb128 chunk index;
			ignore(self#write_class_field_and_overloads_data true cf)

	(* Type instances *)

	val unbound_ttp = new identity_pool

	method write_type_parameter_ref (ttp : typed_type_param) =
		begin try
			begin match ttp.ttp_host with
			| TPHType ->
				let i = type_type_parameters#get ttp.ttp_name in
				Chunk.write_u8 chunk 1;
				Chunk.write_uleb128 chunk i
			| TPHMethod | TPHEnumConstructor | TPHAnonField | TPHConstructor ->
				let i = field_type_parameters#get ttp in
				Chunk.write_u8 chunk 2;
				Chunk.write_uleb128 chunk i;
			| TPHLocal ->
				let index = local_type_parameters#get ttp in
				Chunk.write_u8 chunk 3;
				Chunk.write_uleb128 chunk index;
		end with Not_found ->
			(try ignore(unbound_ttp#get ttp) with Not_found -> begin
				ignore(unbound_ttp#add ttp ());
				let p = { null_pos with pfile = (Path.UniqueKey.lazy_path current_module.m_extra.m_file) } in
				let msg = Printf.sprintf "Unbound type parameter %s" (s_type_path ttp.ttp_class.cl_path) in
				warn WUnboundTypeParameter msg p
			end);
			Chunk.write_u8 chunk 4; (* TDynamic None *)
		end

	method write_type_instance_byte i =
		(* stats.type_instance_kind_writes.(i) <- stats.type_instance_kind_writes.(i) + 1; *)
		Chunk.write_u8 chunk i

	(*
		simple references:
		     0 - mono
		     1 -> type ttp
		     2 -> field ttp
		     3 -> local ttp
		     4 -> Dynamic

		special references:
		    10 - class statics
		    11 - enum statics
		    12 - abstract statics
		    13 - KExpr

		void functions:
		    20: () -> Void
		    21: (A) -> Void
		    22: (A, B) -> Void
		    23: (A, B, C) -> Void
		    24: (A, B, C) -> Void
		    29: (?) -> Void

		non-void functions:
		    30: () -> T
		    31: (A) -> T
		    32: (A, B) -> T
		    33: (A, B, C) -> T
		    34: (A, B, C, D) -> T
		    39: (?) -> T

		class:
		    40: C
		    41: C<A>
		    42: C<A, B>
		    49: C<?>

		enum:
		    50: E
		    51: E<A>
		    52: E<A, B>
		    59: E<?>

		typedef:
		    60: T
		    61: T<A>
		    62: T<A, B>
		    69: T<?>

		abstract:
		    70: A
		    71: A<A>
		    72: A<A, B>
		    79: A<?>

		anons:
		    80: {}
			81: any anon
			89: Dynamic<T>

		concrete types:
		   100: Void
		   101: Int
		   102: Float
		   103: Bool
		   104: String
	*)
	method write_type_instance_simple (t : Type.t) =
		match t with
		| TAbstract ({a_path = ([],"Void")},[]) ->
			Chunk.write_u8 chunk 100;
			None
		| TAbstract ({a_path = ([],"Int")},[]) ->
			Chunk.write_u8 chunk 101;
			None
		| TAbstract ({a_path = ([],"Float")},[]) ->
			Chunk.write_u8 chunk 102;
			None
		| TAbstract ({a_path = ([],"Bool")},[]) ->
			Chunk.write_u8 chunk 103;
			None
		| TInst ({cl_path = ([],"String")},[]) ->
			Chunk.write_u8 chunk 104;
			None
		| TMono r ->
			Monomorph.close r;
			begin match r.tm_type with
			| None ->
				Chunk.write_u8 chunk 0;
				self#write_tmono_ref r;
				None
			| Some t ->
				(* Don't write bound monomorphs, write underlying type directly *)
				self#write_type_instance_simple t
			end
		| TLazy f ->
			self#write_type_instance_simple (lazy_type f)
		| TInst({cl_kind = KTypeParameter ttp},[]) ->
			self#write_type_parameter_ref ttp;
			None
		| TInst({cl_kind = KExpr _},_) ->
			Some t
		| TInst(c,[]) ->
			Chunk.write_u8 chunk 40;
			self#write_class_ref c;
			None
		| TEnum(en,[]) ->
			Chunk.write_u8 chunk 50;
			self#write_enum_ref en;
			None
		| TType(td,[]) ->
			let default () =
				Chunk.write_u8 chunk 60;
				self#write_typedef_ref td;
			in
			begin match td.t_type with
			| TAnon an ->
				begin match !(an.a_status) with
					| ClassStatics c ->
						Chunk.write_u8 chunk 10;
						self#write_class_ref c
					| EnumStatics en ->
						Chunk.write_u8 chunk 11;
						self#write_enum_ref en;
					| AbstractStatics a ->
						Chunk.write_u8 chunk 12;
						self#write_abstract_ref a
					| _ ->
						default()
				end
			| _ ->
				default()
			end;
			None
		| TAbstract(a,[]) ->
			Chunk.write_u8 chunk 70;
			self#write_abstract_ref a;
			None
		| TDynamic None ->
			Chunk.write_u8 chunk 4;
			None
		| TFun([],t) when ExtType.is_void (follow_lazy_and_mono t) ->
			Chunk.write_u8 chunk 20;
			None
		| TInst _ ->
			Some t
		| TEnum _ ->
			Some t
		| TType _ ->
			Some t
		| TAbstract _ ->
			Some t
		| TFun _ ->
			Some t
		| TAnon _ ->
			Some t
		| TDynamic _ ->
			Some t

	method write_inlined_list : 'a . int -> int -> (int -> unit) -> (unit -> unit) -> ('a -> unit) -> 'a list -> unit
		= fun offset max f_byte f_first f_elt l ->
		let length = List.length l in
		if length > max then begin
			f_byte (offset + 9);
			f_first ();
			Chunk.write_list chunk l f_elt
		end else begin
			f_byte (offset + length);
			f_first();
			List.iter (fun elt ->
				f_elt elt
			) l
		end

	method write_type_instance_not_simple t =
		let write_function_arg (n,o,t) =
			Chunk.write_string chunk n;
			Chunk.write_bool chunk o;
			self#write_type_instance t;
		in
		let write_inlined_list offset max f_first f_elt l =
			self#write_inlined_list offset max (Chunk.write_u8 chunk) f_first f_elt l
		in
		match t with
		| TMono _ | TLazy _ | TDynamic None ->
			die "" __LOC__
		| TInst({cl_kind = KExpr e},[]) ->
			Chunk.write_u8 chunk 13;
			self#write_expr e;
		| TFun(args,t) when ExtType.is_void (follow_lazy_and_mono t) ->
			write_inlined_list 20 4 (fun () -> ()) write_function_arg args;
		| TFun(args,t) ->
			write_inlined_list 30 4 (fun () -> ()) write_function_arg args;
			self#write_type_instance t;
		| TInst(c,tl) ->
			write_inlined_list 40 2 (fun () -> self#write_class_ref c) self#write_type_instance tl;
		| TEnum(en,tl) ->
			write_inlined_list 50 2 (fun () -> self#write_enum_ref en) self#write_type_instance tl;
		| TType(td,tl) ->
			write_inlined_list 60 2 (fun () -> self#write_typedef_ref td) self#write_type_instance tl;
		| TAbstract(a,tl) ->
			write_inlined_list 70 2 (fun () -> self#write_abstract_ref a) self#write_type_instance tl;
		| TAnon an when PMap.is_empty an.a_fields ->
			Chunk.write_u8 chunk 80;
		| TAnon an ->
			Chunk.write_u8 chunk 81;
			self#write_anon_ref an []
		| TDynamic (Some t) ->
			Chunk.write_u8 chunk 89;
			self#write_type_instance t;

	method write_type_instance (t: Type.t) =
		match self#write_type_instance_simple t with
			| None ->
				()
			| Some t ->
				self#write_type_instance_not_simple t

	method write_types tl =
		Chunk.write_list chunk tl self#write_type_instance

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
		Chunk.write_u8 chunk b

	method write_var fctx v =
		Chunk.write_uleb128 chunk v.v_id;
		Chunk.write_string chunk v.v_name;
		self#write_var_kind v.v_kind;
		Chunk.write_uleb128 chunk v.v_flags;
		self#write_metadata v.v_meta;
		self#write_pos v.v_pos

	method write_texpr_type_instance (fctx : field_writer_context) (t: Type.t) =
		let restore = self#start_temporary_chunk 32 in
		let r = self#write_type_instance_simple t in
		let index = match r with
		| None ->
			let t_bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
			(* incr stats.type_instance_immediate; *)
			fctx.t_pool#get_or_add t_bytes t_bytes
		| Some t ->
			ignore(restore (fun new_chunk -> Chunk.get_bytes new_chunk));
			let restore = self#start_temporary_chunk 32 in
			self#write_type_instance_not_simple t;
			let t_bytes = restore (fun new_chunk ->
				Chunk.get_bytes new_chunk
			) in
			let index = try
				let index = fctx.t_pool#get t_bytes in
				(* incr stats.type_instance_cache_hits; *)
				index
			with Not_found ->
				(* incr stats.type_instance_cache_misses; *)
				fctx.t_pool#add t_bytes t_bytes
			in
			index
		in
		Chunk.write_uleb128 chunk index

	method write_texpr_byte (i : int) =
		(* stats.texpr_writes.(i) <- stats.texpr_writes.(i) + 1; *)
		Chunk.write_u8 chunk i

	method write_texpr (fctx : field_writer_context) (e : texpr) =
		let declare_var v =
			Chunk.write_uleb128 chunk (fctx.vars#add v.v_id v);
			Chunk.write_option chunk v.v_extra (fun ve ->
				Chunk.write_list chunk ve.v_params (fun ttp ->
					let index = local_type_parameters#add ttp () in
					Chunk.write_uleb128 chunk index
				);
				Chunk.write_option chunk ve.v_expr (self#write_texpr fctx);
			);
			self#write_type_instance v.v_type;
		in
		let rec loop e =

			self#write_texpr_type_instance fctx e.etype;
			fctx.pos_writer#write_pos chunk true 0 e.epos;

			match e.eexpr with
			(* values 0-19 *)
			| TConst ct ->
				begin match ct with
				| TNull ->
					Chunk.write_u8 chunk 0;
				| TThis ->
					fctx.texpr_this <- Some e;
					Chunk.write_u8 chunk 1;
				| TSuper ->
					Chunk.write_u8 chunk 2;
				| TBool false ->
					Chunk.write_u8 chunk 3;
				| TBool true ->
					Chunk.write_u8 chunk 4;
				| TInt i32 ->
					Chunk.write_u8 chunk 5;
					Chunk.write_i32 chunk i32;
				| TFloat f ->
					Chunk.write_u8 chunk 6;
					Chunk.write_string chunk f;
				| TString s ->
					Chunk.write_u8 chunk 7;
					Chunk.write_string chunk s
				end
			(* vars 20-29 *)
			| TLocal v ->
				Chunk.write_u8 chunk 20;
				Chunk.write_uleb128 chunk (fctx.vars#get v.v_id)
			| TVar(v,None) ->
				Chunk.write_u8 chunk 21;
				declare_var v;
			| TVar(v,Some e1) ->
				Chunk.write_u8 chunk 22;
				declare_var v;
				loop e1;
			(* blocks 30-49 *)
			| TBlock [] ->
				Chunk.write_u8 chunk 30;
			| TBlock el ->
				let restore = self#start_temporary_chunk 256 in
				let i = ref 0 in
				List.iter (fun e ->
					incr i;
					loop e;
				) el;
				let bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
				let l = !i in
				begin match l with
				| 1 -> Chunk.write_u8 chunk 31;
				| 2 -> Chunk.write_u8 chunk 32;
				| 3 -> Chunk.write_u8 chunk 33;
				| 4 -> Chunk.write_u8 chunk 34;
				| 5 -> Chunk.write_u8 chunk 35;
				| _ ->
					if l <= 0xFF then begin
						Chunk.write_u8 chunk 36;
						Chunk.write_u8 chunk l;
					end else begin
						Chunk.write_u8 chunk 39;
						Chunk.write_uleb128 chunk l;
					end;
				end;
				Chunk.write_bytes chunk bytes;
			(* function 50-59 *)
			| TFunction tf ->
				Chunk.write_u8 chunk 50;
				Chunk.write_list chunk tf.tf_args (fun (v,eo) ->
					declare_var v;
					Chunk.write_option chunk eo loop;
				);
				self#write_type_instance tf.tf_type;
				loop tf.tf_expr;
			(* texpr compounds 60-79 *)
			| TArray(e1,e2) ->
				Chunk.write_u8 chunk 60;
				loop e1;
				loop e2;
			| TParenthesis e1 ->
				Chunk.write_u8 chunk 61;
				loop e1;
			| TArrayDecl el ->
				Chunk.write_u8 chunk 62;
				loop_el el;
			| TObjectDecl fl ->
				Chunk.write_u8 chunk 63;
				Chunk.write_list chunk fl (fun ((name,p,qs),e) ->
					Chunk.write_string chunk name;
					self#write_pos p;
					begin match qs with
					| NoQuotes -> Chunk.write_u8 chunk 0;
					| DoubleQuotes -> Chunk.write_u8 chunk 1;
					end;
					loop e
				);
			| TCall(e1,el) ->
				self#write_inlined_list 70 4 (Chunk.write_u8 chunk) (fun () -> loop e1) loop el
			| TMeta(m,e1) ->
				Chunk.write_u8 chunk 65;
				self#write_metadata_entry m;
				loop e1;
			(* branching 80-89 *)
			| TIf(e1,e2,None) ->
				Chunk.write_u8 chunk 80;
				loop e1;
				loop e2;
			| TIf(e1,e2,Some e3) ->
				Chunk.write_u8 chunk 81;
				loop e1;
				loop e2;
				loop e3;
			| TSwitch s ->
				Chunk.write_u8 chunk 82;
				loop s.switch_subject;
				Chunk.write_list chunk s.switch_cases (fun c ->
					loop_el c.case_patterns;
					loop c.case_expr;
				);
				Chunk.write_option chunk s.switch_default loop;
			| TTry(e1,catches) ->
				Chunk.write_u8 chunk 83;
				loop e1;
				Chunk.write_list chunk catches  (fun (v,e) ->
					declare_var v;
					loop e
				);
			| TWhile(e1,e2,flag) ->
				Chunk.write_u8 chunk (if flag = NormalWhile then 84 else 85);
				loop e1;
				loop e2;
			| TFor(v,e1,e2) ->
				Chunk.write_u8 chunk 86;
				declare_var v;
				loop e1;
				loop e2;
			(* control flow 90-99 *)
			| TReturn None ->
				Chunk.write_u8 chunk 90;
			| TReturn (Some e1) ->
				Chunk.write_u8 chunk 91;
				loop e1;
			| TContinue ->
				Chunk.write_u8 chunk 92;
			| TBreak ->
				Chunk.write_u8 chunk 93;
			| TThrow e1 ->
				Chunk.write_u8 chunk 94;
				loop e1;
			(* access 100-119 *)
			| TEnumIndex e1 ->
				Chunk.write_u8 chunk 100;
				loop e1;
			| TEnumParameter(e1,ef,i) ->
				Chunk.write_u8 chunk 101;
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
				Chunk.write_uleb128 chunk i;
			| TField({eexpr = TConst TThis; epos = p1},FInstance(c,tl,cf)) when fctx.texpr_this <> None ->
				Chunk.write_u8 chunk 111;
				fctx.pos_writer#write_pos chunk true 0 p1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf;
			| TField(e1,FInstance(c,tl,cf)) ->
				Chunk.write_u8 chunk 102;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf;
			| TField({eexpr = TTypeExpr (TClassDecl c'); epos = p1},FStatic(c,cf)) when c == c' ->
				Chunk.write_u8 chunk 110;
				fctx.pos_writer#write_pos chunk true 0 p1;
				self#write_class_ref c;
				self#write_field_ref c CfrStatic cf;
			| TField(e1,FStatic(c,cf)) ->
				Chunk.write_u8 chunk 103;
				loop e1;
				self#write_class_ref c;
				self#write_field_ref c CfrStatic cf;
			| TField(e1,FAnon cf) ->
				Chunk.write_u8 chunk 104;
				loop e1;
				self#write_anon_field_ref cf
			| TField(e1,FClosure(Some(c,tl),cf)) ->
				Chunk.write_u8 chunk 105;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf
			| TField(e1,FClosure(None,cf)) ->
				Chunk.write_u8 chunk 106;
				loop e1;
				self#write_anon_field_ref cf
			| TField(e1,FEnum(en,ef)) ->
				Chunk.write_u8 chunk 107;
				loop e1;
				self#write_enum_ref en;
				self#write_enum_field_ref en ef;
			| TField(e1,FDynamic s) ->
				Chunk.write_u8 chunk 108;
				loop e1;
				Chunk.write_string chunk s;
			(* module types 120-139 *)
			| TTypeExpr (TClassDecl ({cl_kind = KTypeParameter ttp})) ->
				Chunk.write_u8 chunk 128;
				self#write_type_parameter_ref ttp
			| TTypeExpr (TClassDecl c) ->
				Chunk.write_u8 chunk 120;
				self#write_class_ref c;
			| TTypeExpr (TEnumDecl en) ->
				Chunk.write_u8 chunk 121;
				self#write_enum_ref en;
			| TTypeExpr (TAbstractDecl a) ->
				Chunk.write_u8 chunk 122;
				self#write_abstract_ref a
			| TTypeExpr (TTypeDecl td) ->
				Chunk.write_u8 chunk 123;
				self#write_typedef_ref td
			| TCast(e1,None) ->
				Chunk.write_u8 chunk 124;
				loop e1;
			| TCast(e1,Some md) ->
				Chunk.write_u8 chunk 125;
				loop e1;
				let infos = t_infos md in
				let m = infos.mt_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd infos.mt_path);
			| TNew(({cl_kind = KTypeParameter ttp}),tl,el) ->
				Chunk.write_u8 chunk 127;
				self#write_type_parameter_ref ttp;
				self#write_types tl;
				loop_el el;
			| TNew(c,tl,el) ->
				Chunk.write_u8 chunk 126;
				self#write_class_ref c;
				self#write_types tl;
				loop_el el;
			(* unops 140-159 *)
			| TUnop(op,flag,e1) ->
				Chunk.write_u8 chunk (140 + unop_index op flag);
				loop e1;
			(* binops 160-219 *)
			| TBinop(op,e1,e2) ->
				Chunk.write_u8 chunk (160 + binop_index op);
				loop e1;
				loop e2;
			(* rest 250-254 *)
			| TIdent s ->
				Chunk.write_u8 chunk 250;
				Chunk.write_string chunk s;
		and loop_el el =
			Chunk.write_list chunk el loop
		in
		loop e

	method write_type_parameters_forward (ttps : typed_type_param list) =
		let write_type_parameter_forward ttp =
			self#write_path ttp.ttp_class.cl_path;
			self#write_pos ttp.ttp_class.cl_name_pos;
			let i = match ttp.ttp_host with
				| TPHType -> 0
				| TPHConstructor -> 1
				| TPHMethod -> 2
				| TPHEnumConstructor -> 3
				| TPHAnonField -> 4
				| TPHLocal -> 5
			in
			Chunk.write_u8 chunk i
		in
		Chunk.write_list chunk ttps write_type_parameter_forward

	method write_type_parameters_data (ttps : typed_type_param list) =
		let write_type_parameter_data ttp =
			let c = ttp.ttp_class in
			self#write_metadata c.cl_meta;
			self#write_types (get_constraints ttp);
			Chunk.write_option chunk ttp.ttp_default self#write_type_instance
		in
		List.iter write_type_parameter_data ttps

	method write_type_parameters (ttps : typed_type_param list) =
		self#write_type_parameters_forward ttps;
		self#write_type_parameters_data ttps;

	(* Fields *)

	method write_field_kind = function
		| Method MethNormal -> Chunk.write_u8 chunk 0;
		| Method MethInline -> Chunk.write_u8 chunk 1;
		| Method MethDynamic -> Chunk.write_u8 chunk 2;
		| Method MethMacro -> Chunk.write_u8 chunk 3;
		(* normal read *)
		| Var {v_read = AccNormal; v_write = AccNormal } -> Chunk.write_u8 chunk 10
		| Var {v_read = AccNormal; v_write = AccNo } -> Chunk.write_u8 chunk 11
		| Var {v_read = AccNormal; v_write = AccNever } -> Chunk.write_u8 chunk 12
		| Var {v_read = AccNormal; v_write = AccCtor } -> Chunk.write_u8 chunk 13
		| Var {v_read = AccNormal; v_write = AccCall } -> Chunk.write_u8 chunk 14
		(* inline read *)
		| Var {v_read = AccInline; v_write = AccNever } -> Chunk.write_u8 chunk 20
		(* getter read *)
		| Var {v_read = AccCall; v_write = AccNormal } -> Chunk.write_u8 chunk 30
		| Var {v_read = AccCall; v_write = AccNo } -> Chunk.write_u8 chunk 31
		| Var {v_read = AccCall; v_write = AccNever } -> Chunk.write_u8 chunk 32
		| Var {v_read = AccCall; v_write = AccCtor } -> Chunk.write_u8 chunk 33
		| Var {v_read = AccCall; v_write = AccCall } -> Chunk.write_u8 chunk 34
		(* weird/overlooked combinations *)
		| Var {v_read = r;v_write = w } ->
			Chunk.write_u8 chunk 100;
			let f = function
				| AccNormal -> Chunk.write_u8 chunk 0
				| AccNo -> Chunk.write_u8 chunk 1
				| AccNever -> Chunk.write_u8 chunk 2
				| AccCtor -> Chunk.write_u8 chunk 3
				| AccCall -> Chunk.write_u8 chunk 4
				| AccInline -> Chunk.write_u8 chunk 5
				| AccRequire(s,so) ->
					Chunk.write_u8 chunk 6;
					Chunk.write_string chunk s;
					Chunk.write_option chunk so (Chunk.write_string chunk)
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
		Chunk.write_string chunk cf.cf_name;
		self#write_pos cf.cf_pos;
		self#write_pos cf.cf_name_pos;
		Chunk.write_list chunk cf.cf_overloads (fun cf ->
			self#write_class_field_forward cf;
		);

	method start_texpr (p: pos) =
		let restore = self#start_temporary_chunk 512 in
		let fctx = create_field_writer_context (new pos_writer chunk stats p) in
		fctx,(fun () ->
			restore(fun new_chunk ->
				let restore = self#start_temporary_chunk 512 in
				if self#in_nested_scope then
					Chunk.write_u8 chunk 0
				else begin
					Chunk.write_u8 chunk 1;
					let ltp = List.map fst local_type_parameters#to_list in
					self#write_type_parameters ltp
				end;
				let items = fctx.t_pool#items in
				Chunk.write_uleb128 chunk (DynArray.length items);
				DynArray.iter (fun bytes ->
					Chunk.write_bytes chunk bytes
				) items;

				let items = fctx.vars#items in
				Chunk.write_uleb128 chunk (DynArray.length items);
				DynArray.iter (fun v ->
					self#write_var fctx v;
				) items;
				Chunk.export_data new_chunk chunk;
				restore(fun new_chunk -> new_chunk)
			)
		)

	method commit_field_type_parameters (params : type_params) =
		Chunk.write_uleb128 chunk (List.length params);
		if self#in_nested_scope then
			Chunk.write_u8 chunk 0
		else begin
			Chunk.write_u8 chunk 1;
			let ftp = List.map fst field_type_parameters#to_list in
			self#write_type_parameters ftp
		end

	method write_class_field_data (write_expr_immediately : bool) (cf : tclass_field) =
		let restore = self#start_temporary_chunk 512 in
		self#write_type_instance cf.cf_type;
		Chunk.write_uleb128 chunk cf.cf_flags;
		Chunk.write_option chunk cf.cf_doc self#write_documentation;
		self#write_metadata cf.cf_meta;
		self#write_field_kind cf.cf_kind;
		let expr_chunk = match cf.cf_expr with
			| None ->
				Chunk.write_u8 chunk 0;
				None
			| Some e when not write_expr_immediately ->
				Chunk.write_u8 chunk 0;
				let fctx,close = self#start_texpr e.epos in
				self#write_texpr fctx e;
				Chunk.write_option chunk cf.cf_expr_unoptimized (self#write_texpr fctx);
				let expr_chunk = close() in
				Some expr_chunk
			| Some e ->
				Chunk.write_u8 chunk 1;
				let fctx,close = self#start_texpr e.epos in
				self#write_texpr fctx e;
				Chunk.write_option chunk cf.cf_expr_unoptimized (self#write_texpr fctx);
				let expr_chunk = close() in
				Chunk.export_data expr_chunk chunk;
				None
		in
		restore (fun new_chunk ->
			self#commit_field_type_parameters cf.cf_params;
			Chunk.export_data new_chunk chunk
		);
		expr_chunk

	method write_class_field_and_overloads_data (write_expr_immediately : bool) (cf : tclass_field) =
		let cfl = cf :: cf.cf_overloads in
		Chunk.write_uleb128 chunk (List.length cfl);
		ExtList.List.filter_map (fun cf ->
			let close = self#open_field_scope cf.cf_params in
			let expr_chunk = self#write_class_field_data write_expr_immediately cf in
			close();
			Option.map (fun expr_chunk -> (cf,expr_chunk)) expr_chunk
		) cfl

	(* Module types *)

	method select_type (path : path) =
		type_type_parameters <- type_param_lut#extract path

	method write_common_module_type (infos : tinfos) : unit =
		Chunk.write_bool chunk infos.mt_private;
		Chunk.write_option chunk infos.mt_doc self#write_documentation;
		self#write_metadata infos.mt_meta;
		self#write_type_parameters_data infos.mt_params;
		Chunk.write_list chunk infos.mt_using (fun (c,p) ->
			self#write_class_ref c;
			self#write_pos p;
		);

	method write_class_kind = function
		| KNormal ->
			Chunk.write_u8 chunk 0
		| KTypeParameter ttp ->
			die "TODO" __LOC__
		| KExpr e ->
			Chunk.write_u8 chunk 2;
			self#write_expr e;
		| KGeneric ->
			Chunk.write_u8 chunk 3;
		| KGenericInstance(c,tl) ->
			Chunk.write_u8 chunk 4;
			self#write_class_ref c;
			self#write_types tl
		| KMacroType ->
			Chunk.write_u8 chunk 5;
		| KGenericBuild l ->
			Chunk.write_u8 chunk 6;
			Chunk.write_list chunk l self#write_cfield;
		| KAbstractImpl a ->
			Chunk.write_u8 chunk 7;
			self#write_abstract_ref a;
		| KModuleFields md ->
			Chunk.write_u8 chunk 8;

	method write_class (c : tclass) =
		begin match c.cl_kind with
		| KAbstractImpl a ->
			self#select_type a.a_path
		| _ ->
			self#select_type c.cl_path;
		end;
		self#write_common_module_type (Obj.magic c);
		self#write_class_kind c.cl_kind;
		Chunk.write_option chunk c.cl_super (fun (c,tl) ->
			self#write_class_ref c;
			self#write_types tl
		);
		Chunk.write_list chunk c.cl_implements (fun (c,tl) ->
			self#write_class_ref c;
			self#write_types tl
		);
		Chunk.write_option chunk c.cl_dynamic self#write_type_instance;
		Chunk.write_option chunk c.cl_array_access self#write_type_instance;

	method write_abstract (a : tabstract) =
		begin try
			self#select_type a.a_path
		with Not_found ->
			prerr_endline ("Could not select abstract " ^ (s_type_path a.a_path));
		end;
		self#write_common_module_type (Obj.magic a);
		Chunk.write_option chunk a.a_impl self#write_class_ref;
		if Meta.has Meta.CoreType a.a_meta then
			Chunk.write_u8 chunk 0
		else begin
			Chunk.write_u8 chunk 1;
			self#write_type_instance a.a_this;
		end;
		Chunk.write_list chunk a.a_from self#write_type_instance;
		Chunk.write_list chunk a.a_to self#write_type_instance;
		Chunk.write_bool chunk a.a_enum

	method write_abstract_fields (a : tabstract) =
		let c = match a.a_impl with
			| None ->
				null_class
			| Some c ->
				c
		in

		Chunk.write_list chunk a.a_array (self#write_field_ref c CfrStatic);
		Chunk.write_option chunk a.a_read (self#write_field_ref c CfrStatic );
		Chunk.write_option chunk a.a_write (self#write_field_ref c CfrStatic);
		Chunk.write_option chunk a.a_call (self#write_field_ref c CfrStatic);

		Chunk.write_list chunk a.a_ops (fun (op, cf) ->
			Chunk.write_u8 chunk (binop_index op);
			self#write_field_ref c CfrStatic cf
		);

		Chunk.write_list chunk a.a_unops (fun (op, flag, cf) ->
			Chunk.write_u8 chunk (unop_index op flag);
			self#write_field_ref c CfrStatic cf
		);

		Chunk.write_list chunk a.a_from_field (fun (t,cf) ->
			self#write_field_ref c CfrStatic cf;
		);

		Chunk.write_list chunk a.a_to_field (fun (t,cf) ->
			self#write_field_ref c CfrStatic cf;
		);

	method write_enum (e : tenum) =
		self#select_type e.e_path;
		self#write_common_module_type (Obj.magic e);
		Chunk.write_bool chunk e.e_extern;
		Chunk.write_list chunk e.e_names (Chunk.write_string chunk);

	method write_typedef (td : tdef) =
		self#select_type td.t_path;
		self#write_common_module_type (Obj.magic td);
		self#write_type_instance td.t_type;

	method write_anon (an : tanon) (ttp : type_params) =
		let write_fields () =
			Chunk.write_list chunk (PMap.foldi (fun s f acc -> (s,f) :: acc) an.a_fields []) (fun (_,cf) ->
				self#write_anon_field_ref cf
			)
		in

		begin match !(an.a_status) with
		| Closed ->
			Chunk.write_u8 chunk 0;
			write_fields ()
		| Const ->
			Chunk.write_u8 chunk 1;
			write_fields ()
		| Extend tl ->
			Chunk.write_u8 chunk 2;
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
		Chunk.write_u8 chunk i;
		self#write_path (fst infos.mt_path, !name);
		self#write_pos infos.mt_pos;
		self#write_pos infos.mt_name_pos;
		self#write_type_parameters_forward infos.mt_params;
		let params = new pool in
		type_type_parameters <- params;
		ignore(type_param_lut#add infos.mt_path params);
		List.iter (fun ttp ->
			ignore(type_type_parameters#add ttp.ttp_name ttp)
		) infos.mt_params;

		(* Forward declare fields *)
		match mt with
		| TClassDecl c ->
			Chunk.write_uleb128 chunk c.cl_flags;
			Chunk.write_option chunk c.cl_constructor self#write_class_field_forward;
			Chunk.write_list chunk c.cl_ordered_fields self#write_class_field_forward;
			Chunk.write_list chunk c.cl_ordered_statics self#write_class_field_forward;
		| TEnumDecl e ->
			Chunk.write_list chunk (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
				Chunk.write_string chunk s;
				self#write_pos ef.ef_pos;
				self#write_pos ef.ef_name_pos;
				Chunk.write_u8 chunk ef.ef_index
			);
		| TAbstractDecl a ->
			()
		| TTypeDecl t ->
			()

	method write_module (m : module_def) =
		current_module <- m;

		self#start_chunk MTF;
		Chunk.write_list chunk m.m_types self#forward_declare_type;

		begin match own_abstracts#to_list with
		| [] ->
			()
		| own_abstracts ->
			self#start_chunk ABD;
			Chunk.write_list chunk own_abstracts self#write_abstract;
			self#start_chunk AFD;
			Chunk.write_list chunk own_abstracts self#write_abstract_fields;
		end;
		begin match own_classes#to_list with
		| [] ->
			()
		| own_classes ->
			self#start_chunk CLD;
			Chunk.write_list chunk own_classes self#write_class;
			self#start_chunk CFD;
			let expr_chunks = ref [] in
			Chunk.write_list chunk own_classes (fun c ->
				begin match c.cl_kind with
				| KAbstractImpl a ->
					self#select_type a.a_path
				| _ ->
					self#select_type c.cl_path;
				end;

				let c_expr_chunks = ref [] in
				let write_field ref_kind cf =
					let l = self#write_class_field_and_overloads_data false cf in
					List.iter (fun (cf,e) ->
						c_expr_chunks := (cf,ref_kind,e) :: !c_expr_chunks
					) l
				in

				Chunk.write_option chunk c.cl_constructor (write_field CfrConstructor);
				Chunk.write_list chunk c.cl_ordered_fields (write_field CfrMember);
				Chunk.write_list chunk c.cl_ordered_statics (write_field CfrStatic);
				Chunk.write_option chunk c.cl_init (fun e ->
					let fctx,close = self#start_texpr e.epos in
					self#write_texpr fctx e;
					let new_chunk = close() in
					Chunk.export_data new_chunk chunk
				);
				match !c_expr_chunks with
				| [] ->
					()
				| c_expr_chunks ->
					expr_chunks := (c,c_expr_chunks) :: !expr_chunks
			);
			match !expr_chunks with
			| [] ->
				()
			| expr_chunks ->
				self#start_chunk EXD;
				Chunk.write_list chunk expr_chunks (fun (c,l) ->
					self#write_class_ref c;
					Chunk.write_list chunk l (fun (cf,ref_kind,e) ->
						self#write_field_ref c ref_kind cf;
						Chunk.export_data e chunk
					)
				)
		end;
		begin match own_enums#to_list with
		| [] ->
			()
		| own_enums ->
			self#start_chunk END;
			Chunk.write_list chunk own_enums self#write_enum;
			self#start_chunk EFD;
			Chunk.write_list chunk own_enums (fun e ->
				Chunk.write_list chunk (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
					self#select_type e.e_path;
					let close = self#open_field_scope ef.ef_params in
					Chunk.write_string chunk s;
					let restore = self#start_temporary_chunk 32 in
					self#write_type_instance ef.ef_type;
					let t_bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
					self#commit_field_type_parameters ef.ef_params;
					Chunk.write_bytes chunk t_bytes;
					Chunk.write_option chunk ef.ef_doc self#write_documentation;
					self#write_metadata ef.ef_meta;
					close();
				);
			)
		end;
		begin match own_typedefs#to_list with
		| [] ->
			()
		| own_typedefs ->
			self#start_chunk TDD;
			Chunk.write_list chunk own_typedefs self#write_typedef;
		end;

		begin match classes#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk CLR;
			Chunk.write_list chunk l (fun c ->
				let m = c.cl_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd c.cl_path);
			)
		end;
		begin match abstracts#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ABR;
			Chunk.write_list chunk l (fun a ->
				let m = a.a_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd a.a_path);
			)
		end;
		begin match enums#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ENR;
			Chunk.write_list chunk l (fun en ->
				let m = en.e_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd en.e_path);
			)
		end;
		begin match typedefs#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk TDR;
			Chunk.write_list chunk l (fun td ->
				let m = td.t_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd td.t_path);
			)
		end;

		let items = class_fields#items in
		if DynArray.length items > 0 then begin
			self#start_chunk CFR;
			Chunk.write_uleb128 chunk (DynArray.length items);
			DynArray.iter (fun (cf,(c,kind,depth)) ->
				self#write_class_ref c;
				begin match kind with
				| CfrStatic ->
					Chunk.write_u8 chunk 0;
					Chunk.write_string chunk cf.cf_name
				| CfrMember ->
					Chunk.write_u8 chunk 1;
					Chunk.write_string chunk cf.cf_name
				| CfrConstructor ->
					Chunk.write_u8 chunk 2;
				end;
				Chunk.write_uleb128 chunk depth
			) items;
		end;

		let items = enum_fields#items in
		if DynArray.length items > 0 then begin
			self#start_chunk EFR;
			Chunk.write_uleb128 chunk (DynArray.length items);
			DynArray.iter (fun (en,ef) ->
				self#write_enum_ref en;
				Chunk.write_string chunk ef.ef_name;
			) items;
		end;

		let items = anon_fields#items in
		if DynArray.length items > 0 then begin
			self#start_chunk AFR;
			Chunk.write_uleb128 chunk (DynArray.length items);
			DynArray.iter (fun (cf,_) ->
				self#write_class_field_forward cf
			) items;
		end;

		self#start_chunk MDF;
		self#write_path m.m_path;
		Chunk.write_string chunk (Path.UniqueKey.lazy_path m.m_extra.m_file);
		Chunk.write_uleb128 chunk (DynArray.length anons#items);
		Chunk.write_uleb128 chunk (DynArray.length tmonos#items);
		self#start_chunk EOT;
		self#start_chunk EOF;
		self#start_chunk EOM;

		let finalize_string_pool kind (pool : (string,string) pool) =
			self#start_chunk kind;
			Chunk.write_uleb128 chunk (DynArray.length pool#items);
			DynArray.iter (fun s ->
				let b = Bytes.unsafe_of_string s in
				Chunk.write_bytes_length_prefixed chunk b;
			) pool#items
		in
		finalize_string_pool STR cp;
		if not docs#is_empty then
			finalize_string_pool DOC docs

	(* Export *)

	method get_sorted_chunks =
		let l = DynArray.to_list chunks in
		let l = List.sort (fun chunk1 chunk2 ->
			(Obj.magic chunk1.Chunk.kind - (Obj.magic chunk2.kind))
		) l in
		l

	method get_chunks =
		List.map (fun chunk ->
			(chunk.Chunk.kind,Chunk.get_bytes chunk)
		) (self#get_sorted_chunks)

	method export : 'a . 'a IO.output -> unit = fun ch ->
		write_header ch;
		let l = self#get_sorted_chunks in
		List.iter (fun io ->
			Chunk.export stats io ch
		) l
end
