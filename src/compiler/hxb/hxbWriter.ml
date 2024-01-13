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
	texpr_writes : int array;
	type_instance_immediate : int ref;
	type_instance_ring_hits : int ref;
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
	type_instance_ring_hits = ref 0;
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
	print_endline (Printf.sprintf "     ring hits: %9i" !(stats.type_instance_ring_hits));
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

	method to_list =
		DynArray.to_list items

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

module IOChunk = struct
	type t = {
		kind : chunk_kind;
		ch : SimnBuffer.t;
	}

	let create kind initial_size =
		{
			kind = kind;
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
end

class string_pool (kind : chunk_kind) = object(self)

	val pool = new pool

	method get (s : string) =
		pool#get_or_add s s

	method is_empty =
		pool#is_empty

	method finalize =
		let io = IOChunk.create kind 512 in
		IOChunk.write_uleb128 io (DynArray.length pool#items);
		DynArray.iter (fun s ->
			let b = Bytes.unsafe_of_string s in
			IOChunk.write_bytes_length_prefixed io b;
		) pool#items;
		io
end

module Chunk = struct
	type t = {
		kind : chunk_kind;
		cp : string_pool;
		io : IOChunk.t;
	}

	let create kind cp initial_size = {
		kind;
		cp;
		io = IOChunk.create kind initial_size;
	}

	let write_string chunk s =
		IOChunk.write_uleb128 chunk.io (chunk.cp#get s)

	let write_list : 'b . t -> 'b list -> ('b -> unit) -> unit = fun chunk l f ->
		IOChunk.write_uleb128 chunk.io (List.length l);
		List.iter f l

	let write_option : 'b . t -> 'b option -> ('b -> unit) -> unit = fun chunk v f -> match v with
	| None ->
		IOChunk.write_u8 chunk.io 0
	| Some v ->
		IOChunk.write_u8 chunk.io 1;
		f v

	let export_data chunk_from chunk_to =
		let bytes = IOChunk.get_bytes chunk_from.io in
		IOChunk.write_bytes chunk_to.io bytes
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
		incr stats.pos_writes_full;
		Chunk.write_string chunk p.pfile;
		IOChunk.write_leb128 chunk.io p.pmin;
		IOChunk.write_leb128 chunk.io p.pmax;

	method write_pos (chunk : Chunk.t) (write_equal : bool) (offset : int) (p : pos) =
		if p.pfile != p_file then begin
			(* File changed, write full pos *)
			IOChunk.write_u8 chunk.io (4 + offset);
			self#do_write_pos chunk p;
			p_file <- p.pfile;
			p_min <- p.pmin;
			p_max <- p.pmax;
		end else if p.pmin <> p_min then begin
			if p.pmax <> p_max then begin
				(* pmin and pmax changed *)
				incr stats.pos_writes_minmax;
				IOChunk.write_u8 chunk.io (3 + offset);
				IOChunk.write_leb128 chunk.io p.pmin;
				IOChunk.write_leb128 chunk.io p.pmax;
				p_min <- p.pmin;
				p_max <- p.pmax;
			end else begin
				(* pmin changed *)
				incr stats.pos_writes_min;
				IOChunk.write_u8 chunk.io (1 + offset);
				IOChunk.write_leb128 chunk.io p.pmin;
				p_min <- p.pmin;
			end
		end else if p.pmax <> p_max then begin
			(* pmax changed *)
			incr stats.pos_writes_max;
			IOChunk.write_u8 chunk.io (2 + offset);
			IOChunk.write_leb128 chunk.io p.pmax;
			p_max <- p.pmax;
		end else begin
			incr stats.pos_writes_eq;
			if write_equal then
				IOChunk.write_u8 chunk.io offset;
		end

	initializer
		self#do_write_pos chunk_initial p_initial
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
	mutable texpr_this : texpr option;
	vars : (int,tvar) pool;
}

let create_field_writer_context pos_writer = {
	t_pool = new pool;
	t_rings = new t_rings 5;
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
		let initial_size = match kind with
			| HEND -> 0
			| HHDR -> 16
			| TYPF | CLSR | ENMD | ABSD | ENMR | ABSR | TPDR | ENFR | CFLR | AFLD -> 64
			| ANFR | CLSD | TPDD | EFLD -> 128
			| STRI | DOCS -> 256
			| CFLD -> 512
		in
		let new_chunk = Chunk.create kind cp initial_size in
		DynArray.add chunks new_chunk.io;
		chunk <- new_chunk

	method start_temporary_chunk : 'a . int -> (Chunk.t -> 'a) -> 'a = fun initial_size ->
		let new_chunk = Chunk.create HEND (* TODO: something else? *) cp initial_size in
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
			IOChunk.write_uleb128 chunk.io (docs#get s)
		);
		Chunk.write_list chunk doc.doc_inherited (fun s ->
			IOChunk.write_uleb128 chunk.io (docs#get s)
		);

	method write_pos (p : pos) =
		Chunk.write_string chunk p.pfile;
		IOChunk.write_leb128 chunk.io p.pmin;
		IOChunk.write_leb128 chunk.io p.pmax;

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
			| NoQuotes -> IOChunk.write_u8 chunk.io 0
			| DoubleQuotes -> IOChunk.write_u8 chunk.io 1
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
			IOChunk.write_u8 chunk.io 0;
			self#write_type_hint th
		| TPExpr e ->
			IOChunk.write_u8 chunk.io 1;
			self#write_expr e

	method write_complex_type = function
		| CTPath tp ->
			IOChunk.write_u8 chunk.io 0;
			self#write_placed_type_path tp
		| CTFunction(thl,th) ->
			IOChunk.write_u8 chunk.io 1;
			Chunk.write_list chunk thl self#write_type_hint;
			self#write_type_hint th
		| CTAnonymous cffl ->
			IOChunk.write_u8 chunk.io 2;
			Chunk.write_list chunk cffl self#write_cfield;
		| CTParent th ->
			IOChunk.write_u8 chunk.io 3;
			self#write_type_hint th
		| CTExtend(ptp,cffl) ->
			IOChunk.write_u8 chunk.io 4;
			Chunk.write_list chunk ptp self#write_placed_type_path;
			Chunk.write_list chunk cffl self#write_cfield;
		| CTOptional th ->
			IOChunk.write_u8 chunk.io 5;
			self#write_type_hint th
		| CTNamed(pn,th) ->
			IOChunk.write_u8 chunk.io 6;
			self#write_placed_name pn;
			self#write_type_hint th
		| CTIntersection(thl) ->
			IOChunk.write_u8 chunk.io 7;
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
		IOChunk.write_bool chunk.io b;
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
		IOChunk.write_u8 chunk.io i;

	method write_placed_access (ac,p) =
		self#write_access ac;
		self#write_pos p;

	method write_cfield_kind = function
		| FVar(tho,eo) ->
			IOChunk.write_u8 chunk.io 0;
			Chunk.write_option chunk tho self#write_type_hint;
			Chunk.write_option chunk eo self#write_expr;
		| FFun f ->
			IOChunk.write_u8 chunk.io 1;
			self#write_func f;
		| FProp(pn1,pn2,tho,eo) ->
			IOChunk.write_u8 chunk.io 2;
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
			IOChunk.write_u8 chunk.io 0;
			Chunk.write_string chunk s;
			Chunk.write_option chunk suffix (Chunk.write_string chunk);
		| EConst (Float (s, suffix)) ->
			IOChunk.write_u8 chunk.io 1;
			Chunk.write_string chunk s;
			Chunk.write_option chunk suffix (Chunk.write_string chunk);
		| EConst (String (s,qs)) ->
			IOChunk.write_u8 chunk.io 2;
			Chunk.write_string chunk s;
			begin match qs with
			| SDoubleQuotes -> IOChunk.write_u8 chunk.io 0;
			| SSingleQuotes -> IOChunk.write_u8 chunk.io 1;
			end
		| EConst (Ident s) ->
			IOChunk.write_u8 chunk.io 3;
			Chunk.write_string chunk s;
		| EConst (Regexp(s1,s2)) ->
			IOChunk.write_u8 chunk.io 4;
			Chunk.write_string chunk s1;
			Chunk.write_string chunk s2;
		| EArray(e1,e2) ->
			IOChunk.write_u8 chunk.io 5;
			self#write_expr e1;
			self#write_expr e2;
		| EBinop(op,e1,e2) ->
			IOChunk.write_u8 chunk.io 6;
			IOChunk.write_u8 chunk.io (binop_index op);
			self#write_expr e1;
			self#write_expr e2;
		| EField(e1,s,kind) ->
			IOChunk.write_u8 chunk.io 7;
			self#write_expr e1;
			Chunk.write_string chunk s;
			begin match kind with
			| EFNormal -> IOChunk.write_u8 chunk.io 0;
			| EFSafe -> IOChunk.write_u8 chunk.io 1;
			end
		| EParenthesis e1 ->
			IOChunk.write_u8 chunk.io 8;
			self#write_expr e1
		| EObjectDecl fl ->
			IOChunk.write_u8 chunk.io 9;
			let write_field (k,e1) =
				self#write_object_field_key k;
				self#write_expr e1
			in
			Chunk.write_list chunk fl write_field;
		| EArrayDecl el ->
			IOChunk.write_u8 chunk.io 10;
			Chunk.write_list chunk el self#write_expr;
		| ECall(e1,el) ->
			IOChunk.write_u8 chunk.io 11;
			self#write_expr e1;
			Chunk.write_list chunk el self#write_expr
		| ENew(ptp,el) ->
			IOChunk.write_u8 chunk.io 12;
			self#write_placed_type_path ptp;
			Chunk.write_list chunk el self#write_expr;
		| EUnop(op,flag,e1) ->
			IOChunk.write_u8 chunk.io 13;
			IOChunk.write_u8 chunk.io (unop_index op flag);
			self#write_expr e1;
		| EVars vl ->
			IOChunk.write_u8 chunk.io 14;
			let write_var v =
				self#write_placed_name v.ev_name;
				IOChunk.write_bool chunk.io v.ev_final;
				IOChunk.write_bool chunk.io v.ev_static;
				Chunk.write_option chunk v.ev_type self#write_type_hint;
				Chunk.write_option chunk v.ev_expr self#write_expr;
				self#write_metadata v.ev_meta;
			in
			Chunk.write_list chunk vl write_var
		| EFunction(fk,f) ->
			IOChunk.write_u8 chunk.io 15;
			begin match fk with
			| FKAnonymous -> IOChunk.write_u8 chunk.io 0;
			| FKNamed (pn,inline) ->
				IOChunk.write_u8 chunk.io 1;
				self#write_placed_name pn;
				IOChunk.write_bool chunk.io inline;
			| FKArrow -> IOChunk.write_u8 chunk.io 2;
			end;
			self#write_func f;
		| EBlock el ->
			IOChunk.write_u8 chunk.io 16;
			Chunk.write_list chunk el self#write_expr
		| EFor(e1,e2) ->
			IOChunk.write_u8 chunk.io 17;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,None) ->
			IOChunk.write_u8 chunk.io 18;
			self#write_expr e1;
			self#write_expr e2;
		| EIf(e1,e2,Some e3) ->
			IOChunk.write_u8 chunk.io 19;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| EWhile(e1,e2,NormalWhile) ->
			IOChunk.write_u8 chunk.io 20;
			self#write_expr e1;
			self#write_expr e2;
		| EWhile(e1,e2,DoWhile) ->
			IOChunk.write_u8 chunk.io 21;
			self#write_expr e1;
			self#write_expr e2;
		| ESwitch(e1,cases,def) ->
			IOChunk.write_u8 chunk.io 22;
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
			IOChunk.write_u8 chunk.io 23;
			self#write_expr e1;
			let write_catch (pn,th,e,p) =
				self#write_placed_name pn;
				Chunk.write_option chunk th self#write_type_hint;
				self#write_expr e;
				self#write_pos p;
			in
			Chunk.write_list chunk catches write_catch;
		| EReturn None ->
			IOChunk.write_u8 chunk.io 24;
		| EReturn (Some e1) ->
			IOChunk.write_u8 chunk.io 25;
			self#write_expr e1;
		| EBreak ->
			IOChunk.write_u8 chunk.io 26;
		| EContinue ->
			IOChunk.write_u8 chunk.io 27;
		| EUntyped e1 ->
			IOChunk.write_u8 chunk.io 28;
			self#write_expr e1;
		| EThrow e1 ->
			IOChunk.write_u8 chunk.io 29;
			self#write_expr e1;
		| ECast(e1,None) ->
			IOChunk.write_u8 chunk.io 30;
			self#write_expr e1;
		| ECast(e1,Some th) ->
			IOChunk.write_u8 chunk.io 31;
			self#write_expr e1;
			self#write_type_hint th;
		| EIs(e1,th) ->
			IOChunk.write_u8 chunk.io 32;
			self#write_expr e1;
			self#write_type_hint th;
		| EDisplay(e1,dk) ->
			IOChunk.write_u8 chunk.io 33;
			self#write_expr e1;
			begin match dk with
			| DKCall -> IOChunk.write_u8 chunk.io 0;
			| DKDot -> IOChunk.write_u8 chunk.io 1;
			| DKStructure -> IOChunk.write_u8 chunk.io 2;
			| DKMarked -> IOChunk.write_u8 chunk.io 3;
			| DKPattern b ->
				IOChunk.write_u8 chunk.io 4;
				IOChunk.write_bool chunk.io b;
			end
		| ETernary(e1,e2,e3) ->
			IOChunk.write_u8 chunk.io 34;
			self#write_expr e1;
			self#write_expr e2;
			self#write_expr e3;
		| ECheckType(e1,th) ->
			IOChunk.write_u8 chunk.io 35;
			self#write_expr e1;
			self#write_type_hint th;
		| EMeta(m,e1) ->
			IOChunk.write_u8 chunk.io 36;
			self#write_metadata_entry m;
			self#write_expr e1

	(* References *)

	method write_class_ref (c : tclass) =
		let i = classes#get_or_add c.cl_path c in
		IOChunk.write_uleb128 chunk.io i

	method write_enum_ref (en : tenum) =
		let i = enums#get_or_add en.e_path en in
		IOChunk.write_uleb128 chunk.io i

	method write_typedef_ref (td : tdef) =
		let i = typedefs#get_or_add td.t_path td in
		IOChunk.write_uleb128 chunk.io i

	method write_abstract_ref (a : tabstract) =
		let i = abstracts#get_or_add a.a_path a in
		IOChunk.write_uleb128 chunk.io i

	method write_anon_ref (an : tanon) (ttp : type_params) =
		let pfm = Option.get (anon_id#identify_anon ~strict:true an) in
		try
			let index = anons#get pfm.pfm_path in
			IOChunk.write_u8 chunk.io 0;
			IOChunk.write_uleb128 chunk.io index
		with Not_found ->
			let index = anons#add pfm.pfm_path an in
			IOChunk.write_u8 chunk.io 1;
			IOChunk.write_uleb128 chunk.io index;
			self#write_anon an ttp

	method write_tmono_ref (mono : tmono) =
		let index = try tmonos#get mono with Not_found -> tmonos#add mono () in
		IOChunk.write_uleb128 chunk.io index;

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
		IOChunk.write_uleb128 chunk.io index

	method write_enum_field_ref (en : tenum) (ef : tenum_field) =
		let key = (en.e_path,ef.ef_name) in
		try
			IOChunk.write_uleb128 chunk.io (enum_fields#get key)
		with Not_found ->
			ignore(enums#get_or_add en.e_path en);
			IOChunk.write_uleb128 chunk.io (enum_fields#add key (en,ef))

	method write_anon_field_ref cf =
		try
			let index = anon_fields#get cf in
			IOChunk.write_u8 chunk.io 0;
			IOChunk.write_uleb128 chunk.io index
		with Not_found ->
			let index = anon_fields#add cf () in
			IOChunk.write_u8 chunk.io 1;
			IOChunk.write_uleb128 chunk.io index;
			self#write_class_field_and_overloads_data cf;

	(* Type instances *)

	val unbound_ttp = new identity_pool

	method write_type_parameter_ref (ttp : typed_type_param) =
		begin try
			begin match ttp.ttp_host with
			| TPHType ->
				let i = type_type_parameters#get ttp.ttp_name in
				IOChunk.write_u8 chunk.io 1;
				IOChunk.write_uleb128 chunk.io i
			| TPHMethod | TPHEnumConstructor | TPHAnonField | TPHConstructor ->
				let i = field_type_parameters#get ttp in
				IOChunk.write_u8 chunk.io 2;
				IOChunk.write_uleb128 chunk.io i;
			| TPHLocal ->
				let index = local_type_parameters#get ttp in
				IOChunk.write_u8 chunk.io 3;
				IOChunk.write_uleb128 chunk.io index;
		end with Not_found ->
			(try ignore(unbound_ttp#get ttp) with Not_found -> begin
				ignore(unbound_ttp#add ttp ());
				let p = { null_pos with pfile = (Path.UniqueKey.lazy_path current_module.m_extra.m_file) } in
				let msg = Printf.sprintf "Unbound type parameter %s" (s_type_path ttp.ttp_class.cl_path) in
				(* if p = null_pos then trace_call_stack ~n:20 (); *)
				warn WUnboundTypeParameter msg p
			end);
			(* TODO: handle unbound type parameters? *)
			IOChunk.write_u8 chunk.io 4; (* TDynamic None *)
		end

	method write_type_instance_byte i =
		stats.type_instance_kind_writes.(i) <- stats.type_instance_kind_writes.(i) + 1;
		IOChunk.write_u8 chunk.io i

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
	method write_type_instance_simple (rings : t_rings) (t : Type.t) =
		match t with
		| TAbstract ({a_path = ([],"Void")},[]) ->
			self#write_type_instance_byte 100;
			None
		| TAbstract ({a_path = ([],"Int")},[]) ->
			self#write_type_instance_byte 101;
			None
		| TAbstract ({a_path = ([],"Float")},[]) ->
			self#write_type_instance_byte 102;
			None
		| TAbstract ({a_path = ([],"Bool")},[]) ->
			self#write_type_instance_byte 103;
			None
		| TInst ({cl_path = ([],"String")},[]) ->
			self#write_type_instance_byte 104;
			None
		| TMono r ->
			Monomorph.close r;
			begin match r.tm_type with
			| None ->
				self#write_type_instance_byte 0;
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
			self#write_type_instance_byte 40;
			self#write_class_ref c;
			None
		| TEnum(en,[]) ->
			self#write_type_instance_byte 50;
			self#write_enum_ref en;
			None
		| TType(td,[]) ->
			let default () =
				self#write_type_instance_byte 60;
				self#write_typedef_ref td;
			in
			begin match td.t_type with
			| TAnon an ->
				begin match !(an.a_status) with
					| ClassStatics c ->
						self#write_type_instance_byte 10;
						self#write_class_ref c
					| EnumStatics en ->
						self#write_type_instance_byte 11;
						self#write_enum_ref en;
					| AbstractStatics a ->
						self#write_type_instance_byte 12;
						self#write_abstract_ref a
					| _ ->
						default()
				end
			| _ ->
				default()
			end;
			None
		| TAbstract(a,[]) ->
			self#write_type_instance_byte 70;
			self#write_abstract_ref a;
			None
		| TDynamic None ->
			self#write_type_instance_byte 4;
			None
		| TFun([],t) when ExtType.is_void (follow_lazy_and_mono t) ->
			self#write_type_instance_byte 20;
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
			IOChunk.write_bool chunk.io o;
			self#write_type_instance t;
		in
		let write_inlined_list offset max f_first f_elt l =
			self#write_inlined_list offset max self#write_type_instance_byte f_first f_elt l
		in
		match t with
		| TMono _ | TLazy _ | TDynamic None ->
			die "" __LOC__
		| TInst({cl_kind = KExpr e},[]) ->
			self#write_type_instance_byte 13;
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
			self#write_type_instance_byte 80;
		| TAnon an ->
			self#write_type_instance_byte 81;
			self#write_anon_ref an []
		| TDynamic (Some t) ->
			self#write_type_instance_byte 89;
			self#write_type_instance t;

	method write_type_instance (t: Type.t) =
		match self#write_type_instance_simple dummy_rings t with
			| None ->
				()
			| Some(t,_) ->
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
		IOChunk.write_u8 chunk.io b

	method write_var fctx v =
		IOChunk.write_uleb128 chunk.io v.v_id;
		Chunk.write_string chunk v.v_name;
		self#write_var_kind v.v_kind;
		IOChunk.write_uleb128 chunk.io v.v_flags;
		self#write_metadata v.v_meta;
		self#write_pos v.v_pos

	method write_texpr_type_instance (fctx : field_writer_context) (t: Type.t) =
		let restore = self#start_temporary_chunk 32 in
		let r = self#write_type_instance_simple fctx.t_rings t in
		let index = match r with
		| None ->
			let t_bytes = restore (fun new_chunk -> IOChunk.get_bytes new_chunk.io) in
			incr stats.type_instance_immediate;
			fctx.t_pool#get_or_add t_bytes t_bytes
		| Some(t,ring) ->
			ignore(restore (fun new_chunk -> IOChunk.get_bytes new_chunk.io));
			try
				let index = fctx.t_rings#find ring t in
				incr stats.type_instance_ring_hits;
				index
			with Not_found ->
				let restore = self#start_temporary_chunk 32 in
				self#write_type_instance_not_simple t;
				let t_bytes = restore (fun new_chunk ->
					IOChunk.get_bytes new_chunk.io
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
		IOChunk.write_uleb128 chunk.io index

	method write_texpr_byte (i : int) =
		stats.texpr_writes.(i) <- stats.texpr_writes.(i) + 1;
		IOChunk.write_u8 chunk.io i

	method write_texpr (fctx : field_writer_context) (e : texpr) =
		let declare_var v =
			IOChunk.write_uleb128 chunk.io (fctx.vars#add v.v_id v);
			Chunk.write_option chunk v.v_extra (fun ve ->
				Chunk.write_list chunk ve.v_params (fun ttp ->
					let index = local_type_parameters#add ttp () in
					IOChunk.write_uleb128 chunk.io index
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
					self#write_texpr_byte 0;
				| TThis ->
					fctx.texpr_this <- Some e;
					self#write_texpr_byte 1;
				| TSuper ->
					self#write_texpr_byte 2;
				| TBool false ->
					self#write_texpr_byte 3;
				| TBool true ->
					self#write_texpr_byte 4;
				| TInt i32 ->
					self#write_texpr_byte 5;
					IOChunk.write_i32 chunk.io i32;
				| TFloat f ->
					self#write_texpr_byte 6;
					Chunk.write_string chunk f;
				| TString s ->
					self#write_texpr_byte 7;
					Chunk.write_string chunk s
				end
			(* vars 20-29 *)
			| TLocal v ->
				self#write_texpr_byte 20;
				IOChunk.write_uleb128 chunk.io (fctx.vars#get v.v_id)
			| TVar(v,None) ->
				self#write_texpr_byte 21;
				declare_var v;
			| TVar(v,Some e1) ->
				self#write_texpr_byte 22;
				declare_var v;
				loop e1;
			(* blocks 30-49 *)
			| TBlock [] ->
				self#write_texpr_byte 30;
			| TBlock el ->
				let restore = self#start_temporary_chunk 256 in
				let i = ref 0 in
				List.iter (fun e ->
					incr i;
					loop e;
				) el;
				let bytes = restore (fun new_chunk -> IOChunk.get_bytes new_chunk.io) in
				let l = !i in
				begin match l with
				| 1 -> self#write_texpr_byte 31;
				| 2 -> self#write_texpr_byte 32;
				| 3 -> self#write_texpr_byte 33;
				| 4 -> self#write_texpr_byte 34;
				| 5 -> self#write_texpr_byte 35;
				| _ ->
					if l <= 0xFF then begin
						self#write_texpr_byte 36;
						IOChunk.write_u8 chunk.io l;
					end else begin
						self#write_texpr_byte 39;
						IOChunk.write_uleb128 chunk.io l;
					end;
				end;
				IOChunk.write_bytes chunk.io bytes;
			(* function 50-59 *)
			| TFunction tf ->
				self#write_texpr_byte 50;
				Chunk.write_list chunk tf.tf_args (fun (v,eo) ->
					declare_var v;
					Chunk.write_option chunk eo loop;
				);
				self#write_type_instance tf.tf_type;
				loop tf.tf_expr;
			(* texpr compounds 60-79 *)
			| TArray(e1,e2) ->
				self#write_texpr_byte 60;
				loop e1;
				loop e2;
			| TParenthesis e1 ->
				self#write_texpr_byte 61;
				loop e1;
			| TArrayDecl el ->
				self#write_texpr_byte 62;
				loop_el el;
			| TObjectDecl fl ->
				self#write_texpr_byte 63;
				Chunk.write_list chunk fl (fun ((name,p,qs),e) ->
					Chunk.write_string chunk name;
					self#write_pos p;
					begin match qs with
					| NoQuotes -> IOChunk.write_u8 chunk.io 0;
					| DoubleQuotes -> IOChunk.write_u8 chunk.io 1;
					end;
					loop e
				);
			| TCall(e1,el) ->
				self#write_inlined_list 70 4 self#write_texpr_byte (fun () -> loop e1) loop el
			| TMeta(m,e1) ->
				self#write_texpr_byte 65;
				self#write_metadata_entry m;
				loop e1;
			(* branching 80-89 *)
			| TIf(e1,e2,None) ->
				self#write_texpr_byte 80;
				loop e1;
				loop e2;
			| TIf(e1,e2,Some e3) ->
				self#write_texpr_byte 81;
				loop e1;
				loop e2;
				loop e3;
			| TSwitch s ->
				self#write_texpr_byte 82;
				loop s.switch_subject;
				Chunk.write_list chunk s.switch_cases (fun c ->
					loop_el c.case_patterns;
					loop c.case_expr;
				);
				Chunk.write_option chunk s.switch_default loop;
			| TTry(e1,catches) ->
				self#write_texpr_byte 83;
				loop e1;
				Chunk.write_list chunk catches  (fun (v,e) ->
					declare_var v;
					loop e
				);
			| TWhile(e1,e2,flag) ->
				self#write_texpr_byte (if flag = NormalWhile then 84 else 85);
				loop e1;
				loop e2;
			| TFor(v,e1,e2) ->
				self#write_texpr_byte 86;
				declare_var v;
				loop e1;
				loop e2;
			(* control flow 90-99 *)
			| TReturn None ->
				self#write_texpr_byte 90;
			| TReturn (Some e1) ->
				self#write_texpr_byte 91;
				loop e1;
			| TContinue ->
				self#write_texpr_byte 92;
			| TBreak ->
				self#write_texpr_byte 93;
			| TThrow e1 ->
				self#write_texpr_byte 94;
				loop e1;
			(* access 100-119 *)
			| TEnumIndex e1 ->
				self#write_texpr_byte 100;
				loop e1;
			| TEnumParameter(e1,ef,i) ->
				self#write_texpr_byte 101;
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
				IOChunk.write_uleb128 chunk.io i;
			| TField({eexpr = TConst TThis; epos = p1},FInstance(c,tl,cf)) when fctx.texpr_this <> None ->
				self#write_texpr_byte 111;
				fctx.pos_writer#write_pos chunk true 0 p1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf;
			| TField(e1,FInstance(c,tl,cf)) ->
				self#write_texpr_byte 102;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf;
			| TField({eexpr = TTypeExpr (TClassDecl c'); epos = p1},FStatic(c,cf)) when c == c' ->
				self#write_texpr_byte 110;
				fctx.pos_writer#write_pos chunk true 0 p1;
				self#write_class_ref c;
				self#write_field_ref c CfrStatic cf;
			| TField(e1,FStatic(c,cf)) ->
				self#write_texpr_byte 103;
				loop e1;
				self#write_class_ref c;
				self#write_field_ref c CfrStatic cf;
			| TField(e1,FAnon cf) ->
				self#write_texpr_byte 104;
				loop e1;
				self#write_anon_field_ref cf
			| TField(e1,FClosure(Some(c,tl),cf)) ->
				self#write_texpr_byte 105;
				loop e1;
				self#write_class_ref c;
				self#write_types tl;
				self#write_field_ref c CfrMember cf
			| TField(e1,FClosure(None,cf)) ->
				self#write_texpr_byte 106;
				loop e1;
				self#write_anon_field_ref cf
			| TField(e1,FEnum(en,ef)) ->
				self#write_texpr_byte 107;
				loop e1;
				self#write_enum_ref en;
				self#write_enum_field_ref en ef;
			| TField(e1,FDynamic s) ->
				self#write_texpr_byte 108;
				loop e1;
				Chunk.write_string chunk s;
			(* module types 120-139 *)
			| TTypeExpr (TClassDecl ({cl_kind = KTypeParameter ttp})) ->
				self#write_texpr_byte 128;
				self#write_type_parameter_ref ttp
			| TTypeExpr (TClassDecl c) ->
				self#write_texpr_byte 120;
				self#write_class_ref c;
			| TTypeExpr (TEnumDecl en) ->
				self#write_texpr_byte 121;
				self#write_enum_ref en;
			| TTypeExpr (TAbstractDecl a) ->
				self#write_texpr_byte 122;
				self#write_abstract_ref a
			| TTypeExpr (TTypeDecl td) ->
				self#write_texpr_byte 123;
				self#write_typedef_ref td
			| TCast(e1,None) ->
				self#write_texpr_byte 124;
				loop e1;
			| TCast(e1,Some md) ->
				self#write_texpr_byte 125;
				loop e1;
				let infos = t_infos md in
				let m = infos.mt_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd infos.mt_path);
			| TNew(({cl_kind = KTypeParameter ttp}),tl,el) ->
				self#write_texpr_byte 127;
				self#write_type_parameter_ref ttp;
				self#write_types tl;
				loop_el el;
			| TNew(c,tl,el) ->
				self#write_texpr_byte 126;
				self#write_class_ref c;
				self#write_types tl;
				loop_el el;
			(* unops 140-159 *)
			| TUnop(op,flag,e1) ->
				self#write_texpr_byte (140 + unop_index op flag);
				loop e1;
			(* binops 160-219 *)
			| TBinop(op,e1,e2) ->
				self#write_texpr_byte (160 + binop_index op);
				loop e1;
				loop e2;
			(* rest 250-254 *)
			| TIdent s ->
				self#write_texpr_byte 250;
				Chunk.write_string chunk s;
		and loop_el el =
			Chunk.write_list chunk el loop
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
		Chunk.write_option chunk ttp.ttp_default self#write_type_instance

	method write_field_kind = function
		| Method MethNormal -> IOChunk.write_u8 chunk.io 0;
		| Method MethInline -> IOChunk.write_u8 chunk.io 1;
		| Method MethDynamic -> IOChunk.write_u8 chunk.io 2;
		| Method MethMacro -> IOChunk.write_u8 chunk.io 3;
		(* normal read *)
		| Var {v_read = AccNormal; v_write = AccNormal } -> IOChunk.write_u8 chunk.io 10
		| Var {v_read = AccNormal; v_write = AccNo } -> IOChunk.write_u8 chunk.io 11
		| Var {v_read = AccNormal; v_write = AccNever } -> IOChunk.write_u8 chunk.io 12
		| Var {v_read = AccNormal; v_write = AccCtor } -> IOChunk.write_u8 chunk.io 13
		| Var {v_read = AccNormal; v_write = AccCall } -> IOChunk.write_u8 chunk.io 14
		(* inline read *)
		| Var {v_read = AccInline; v_write = AccNever } -> IOChunk.write_u8 chunk.io 20
		(* getter read *)
		| Var {v_read = AccCall; v_write = AccNormal } -> IOChunk.write_u8 chunk.io 30
		| Var {v_read = AccCall; v_write = AccNo } -> IOChunk.write_u8 chunk.io 31
		| Var {v_read = AccCall; v_write = AccNever } -> IOChunk.write_u8 chunk.io 32
		| Var {v_read = AccCall; v_write = AccCtor } -> IOChunk.write_u8 chunk.io 33
		| Var {v_read = AccCall; v_write = AccCall } -> IOChunk.write_u8 chunk.io 34
		(* weird/overlooked combinations *)
		| Var {v_read = r;v_write = w } ->
			IOChunk.write_u8 chunk.io 100;
			let f = function
				| AccNormal -> IOChunk.write_u8 chunk.io 0
				| AccNo -> IOChunk.write_u8 chunk.io 1
				| AccNever -> IOChunk.write_u8 chunk.io 2
				| AccCtor -> IOChunk.write_u8 chunk.io 3
				| AccCall -> IOChunk.write_u8 chunk.io 4
				| AccInline -> IOChunk.write_u8 chunk.io 5
				| AccRequire(s,so) ->
					IOChunk.write_u8 chunk.io 6;
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
				let items = fctx.t_pool#items in
				IOChunk.write_uleb128 chunk.io (DynArray.length items);
				DynArray.iter (fun bytes ->
					IOChunk.write_bytes chunk.io bytes
				) items;

				let items = fctx.vars#items in
				IOChunk.write_uleb128 chunk.io (DynArray.length items);
				DynArray.iter (fun v ->
					self#write_var fctx v;
				) items;
				Chunk.export_data new_chunk chunk
			)
		)

	method commit_field_type_parameters (params : type_params) =
		IOChunk.write_uleb128 chunk.io (List.length params);
		if not self#in_nested_scope then begin
			let ftp = List.map fst field_type_parameters#to_list in
			Chunk.write_list chunk ftp self#write_type_parameter_forward;
			Chunk.write_list chunk ftp self#write_type_parameter_data;
		end

	method write_class_field_data cf =
		let restore = self#start_temporary_chunk 512 in
		(try self#write_type_instance cf.cf_type with e -> begin
			prerr_endline (Printf.sprintf "%s while writing type instance for field %s" todo_error cf.cf_name);
			raise e
		end);
		IOChunk.write_uleb128 chunk.io cf.cf_flags;
		Chunk.write_option chunk cf.cf_doc self#write_documentation;
		self#write_metadata cf.cf_meta;
		self#write_field_kind cf.cf_kind;
		begin match cf.cf_expr with
			| None ->
				IOChunk.write_u8 chunk.io 0
			| Some e ->
				IOChunk.write_u8 chunk.io 1;
				let fctx,close = self#start_texpr e.epos in
				self#write_texpr fctx e;
				Chunk.write_option chunk cf.cf_expr_unoptimized (self#write_texpr fctx);
				close();
		end;

		restore (fun new_chunk ->
			self#commit_field_type_parameters cf.cf_params;
			if not self#in_nested_scope then begin
				let ltp = List.map fst local_type_parameters#to_list in
				Chunk.write_list chunk ltp self#write_type_parameter_forward;
				Chunk.write_list chunk ltp self#write_type_parameter_data;
			end;
			Chunk.export_data new_chunk chunk
		)

	method write_class_field_and_overloads_data (cf : tclass_field) =
		let cfl = cf :: cf.cf_overloads in
		IOChunk.write_uleb128 chunk.io (List.length cfl);
		List.iter (fun cf ->
			let close = self#open_field_scope cf.cf_params in
			self#write_class_field_data cf;
			close();
		) cfl

	(* Module types *)

	method select_type (path : path) =
		type_type_parameters <- type_param_lut#extract path

	method write_common_module_type (infos : tinfos) : unit =
		IOChunk.write_bool chunk.io infos.mt_private;
		Chunk.write_option chunk infos.mt_doc self#write_documentation;
		self#write_metadata infos.mt_meta;
		Chunk.write_list chunk infos.mt_params self#write_type_parameter_forward;
		Chunk.write_list chunk infos.mt_params self#write_type_parameter_data;
		Chunk.write_list chunk infos.mt_using (fun (c,p) ->
			self#write_class_ref c;
			self#write_pos p;
		);

	method write_class_kind = function
		| KNormal ->
			IOChunk.write_u8 chunk.io 0
		| KTypeParameter ttp ->
			die "TODO" __LOC__
		| KExpr e ->
			IOChunk.write_u8 chunk.io 2;
			self#write_expr e;
		| KGeneric ->
			IOChunk.write_u8 chunk.io 3;
		| KGenericInstance(c,tl) ->
			IOChunk.write_u8 chunk.io 4;
			self#write_class_ref c;
			self#write_types tl
		| KMacroType ->
			IOChunk.write_u8 chunk.io 5;
		| KGenericBuild l ->
			IOChunk.write_u8 chunk.io 6;
			Chunk.write_list chunk l self#write_cfield;
		| KAbstractImpl a ->
			IOChunk.write_u8 chunk.io 7;
			self#write_abstract_ref a;
		| KModuleFields md ->
			IOChunk.write_u8 chunk.io 8;

	method write_class (c : tclass) =
		begin match c.cl_kind with
		| KAbstractImpl a ->
			self#select_type a.a_path
		| _ ->
			self#select_type c.cl_path;
		end;
		self#write_common_module_type (Obj.magic c);
		self#write_class_kind c.cl_kind;
		IOChunk.write_uleb128 chunk.io c.cl_flags;
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
			IOChunk.write_u8 chunk.io 0
		else begin
			IOChunk.write_u8 chunk.io 1;
			self#write_type_instance a.a_this;
		end;
		Chunk.write_list chunk a.a_from self#write_type_instance;
		Chunk.write_list chunk a.a_to self#write_type_instance;
		IOChunk.write_bool chunk.io a.a_enum

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
			IOChunk.write_u8 chunk.io (binop_index op);
			self#write_field_ref c CfrStatic cf
		);

		Chunk.write_list chunk a.a_unops (fun (op, flag, cf) ->
			IOChunk.write_u8 chunk.io (unop_index op flag);
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
		IOChunk.write_bool chunk.io e.e_extern;
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
			IOChunk.write_u8 chunk.io 0;
			write_fields ()
		| Const ->
			IOChunk.write_u8 chunk.io 1;
			write_fields ()
		| Extend tl ->
			IOChunk.write_u8 chunk.io 2;
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
		IOChunk.write_u8 chunk.io i;
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
			Chunk.write_option chunk c.cl_constructor self#write_class_field_forward;
			Chunk.write_list chunk c.cl_ordered_fields self#write_class_field_forward;
			Chunk.write_list chunk c.cl_ordered_statics self#write_class_field_forward;
		| TEnumDecl e ->
			Chunk.write_list chunk (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
				Chunk.write_string chunk s;
				self#write_pos ef.ef_pos;
				self#write_pos ef.ef_name_pos;
				IOChunk.write_u8 chunk.io ef.ef_index
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
		Chunk.write_list chunk m.m_types self#forward_declare_type;

		begin match own_abstracts#to_list with
		| [] ->
			()
		| own_abstracts ->
			self#start_chunk ABSD;
			Chunk.write_list chunk own_abstracts self#write_abstract;
			self#start_chunk AFLD;
			Chunk.write_list chunk own_abstracts self#write_abstract_fields;
		end;
		begin match own_classes#to_list with
		| [] ->
			()
		| own_classes ->
			self#start_chunk CLSD;
			Chunk.write_list chunk own_classes self#write_class;
			self#start_chunk CFLD;
			Chunk.write_list chunk own_classes (fun c ->
				begin match c.cl_kind with
				| KAbstractImpl a ->
					self#select_type a.a_path
				| _ ->
					self#select_type c.cl_path;
				end;

				let write_field cf =
					self#write_class_field_and_overloads_data cf;
				in

				Chunk.write_option chunk c.cl_constructor write_field;
				Chunk.write_list chunk c.cl_ordered_fields write_field;
				Chunk.write_list chunk c.cl_ordered_statics write_field;
				Chunk.write_option chunk c.cl_init (fun e ->
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
			Chunk.write_list chunk own_enums self#write_enum;
			self#start_chunk EFLD;
			Chunk.write_list chunk own_enums (fun e ->
				Chunk.write_list chunk (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
					self#select_type e.e_path;
					let close = self#open_field_scope ef.ef_params in
					Chunk.write_string chunk s;
					let restore = self#start_temporary_chunk 32 in
					self#write_type_instance ef.ef_type;
					let t_bytes = restore (fun new_chunk -> IOChunk.get_bytes new_chunk.io) in
					self#commit_field_type_parameters ef.ef_params;
					IOChunk.write_bytes chunk.io t_bytes;
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
			self#start_chunk TPDD;
			Chunk.write_list chunk own_typedefs self#write_typedef;
		end;

		begin match classes#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk CLSR;
			Chunk.write_list chunk l (fun c ->
				let m = c.cl_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd c.cl_path);
			)
		end;
		begin match abstracts#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ABSR;
			Chunk.write_list chunk l (fun a ->
				let m = a.a_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd a.a_path);
			)
		end;
		begin match enums#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk ENMR;
			Chunk.write_list chunk l (fun en ->
				let m = en.e_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd en.e_path);
			)
		end;
		begin match typedefs#to_list with
		| [] ->
			()
		| l ->
			self#start_chunk TPDR;
			Chunk.write_list chunk l (fun td ->
				let m = td.t_module in
				self#write_full_path (fst m.m_path) (snd m.m_path) (snd td.t_path);
			)
		end;

		self#start_chunk CFLR;
		let items = class_fields#items in
		IOChunk.write_uleb128 chunk.io (DynArray.length items);
		DynArray.iter (fun (cf,(c,kind,depth)) ->
			self#write_class_ref c;
			begin match kind with
			| CfrStatic ->
				IOChunk.write_u8 chunk.io 0;
				Chunk.write_string chunk cf.cf_name
			| CfrMember ->
				IOChunk.write_u8 chunk.io 1;
				Chunk.write_string chunk cf.cf_name
			| CfrConstructor ->
				IOChunk.write_u8 chunk.io 2;
			end;
			IOChunk.write_uleb128 chunk.io depth
		) items;

		self#start_chunk ENFR;
		let items = enum_fields#items in
		IOChunk.write_uleb128 chunk.io (DynArray.length items);
		DynArray.iter (fun (en,ef) ->
			self#write_enum_ref en;
			Chunk.write_string chunk ef.ef_name;
		) items;

		self#start_chunk ANFR;
		let items = anon_fields#items in
		IOChunk.write_uleb128 chunk.io (DynArray.length items);
		DynArray.iter (fun (cf,_) ->
			self#write_class_field_forward cf
		) items;

		self#start_chunk HHDR;
		self#write_path m.m_path;
		Chunk.write_string chunk (Path.UniqueKey.lazy_path m.m_extra.m_file);
		IOChunk.write_uleb128 chunk.io (DynArray.length anons#items);
		IOChunk.write_uleb128 chunk.io (DynArray.length tmonos#items);
		self#start_chunk HEND;
		DynArray.add chunks cp#finalize;
		if not docs#is_empty then
			DynArray.add chunks docs#finalize

	(* Export *)

	method get_sorted_chunks =
		let l = DynArray.to_list chunks in
		let l = List.sort (fun chunk1 chunk2 ->
			(Obj.magic chunk1.IOChunk.kind - (Obj.magic chunk2.kind))
		) l in
		l

	method get_chunks =
		List.map (fun chunk ->
			(chunk.IOChunk.kind,IOChunk.get_bytes chunk)
		) (self#get_sorted_chunks)

	method export : 'a . 'a IO.output -> unit = fun ch ->
		write_header ch;
		let l = self#get_sorted_chunks in
		List.iter (fun io ->
			IOChunk.export stats io ch
		) l
end
