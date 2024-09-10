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

module StringHashtbl = Hashtbl.Make(struct
	type t = string

	let equal =
		String.equal

	let hash s =
		(* What's the best here? *)
		Hashtbl.hash s
end)

module Pool = struct
	type ('key,'value) t = {
		lut : ('key,int) Hashtbl.t;
		items : 'value DynArray.t;
		mutable closed : bool;
	}

	let create () = {
		lut = Hashtbl.create 0;
		items = DynArray.create ();
		closed = false;
	}

	let add pool (key : 'key) (value : 'value) =
		assert (not pool.closed);
		let index = DynArray.length pool.items in
		DynArray.add pool.items value;
		Hashtbl.add pool.lut key index;
		index

	let get pool (key : 'key) =
		Hashtbl.find pool.lut key

	let extract pool (key : 'key) =
		DynArray.get pool.items (get pool key)

	let has pool (key : 'key) =
		Hashtbl.mem pool.lut key

	let get_or_add pool (key : 'key) (value : 'value) =
		try
			get pool key
		with Not_found ->
			add pool key value

	let is_empty pool =
		DynArray.length pool.items = 0

	let advance pool dummy =
		DynArray.add pool.items dummy

	let finalize pool =
		assert (not pool.closed);
		pool.closed <- true;
		pool.items
end

module IdentityPool = struct
	type ('key,'value) t = {
		items : ('key * 'value) DynArray.t;
		mutable closed : bool;
	}

	let create () = {
		items = DynArray.create ();
		closed = false;
	}

	let add pool (key : 'key) (value : 'value) =
		assert (not pool.closed);
		let index = DynArray.length pool.items in
		DynArray.add pool.items (key,value);
		index

	let get pool (key : 'key) =
		DynArray.index_of (fun (key',_) -> key == key') pool.items

	let get_or_add pool (key : 'key) (value : 'value) =
		try
			get pool key
		with Not_found ->
			add pool key value

	let to_list pool =
		DynArray.to_list pool.items

	let finalize pool =
		assert (not pool.closed);
		pool.closed <- true;
		pool.items

	let length pool = DynArray.length pool.items
end

module HashedIdentityPool = struct
	type ('hkey,'key,'value) t = {
		lut : ('hkey,('key * int)) Hashtbl.t;
		items : ('key * 'value) DynArray.t;
		mutable closed : bool;
	}

	let create () = {
		lut = Hashtbl.create 16;
		items = DynArray.create ();
		closed = false;
	}

	let add pool (hkey : 'hkey) (key : 'key) (value : 'value) =
		assert (not pool.closed);
		let index = DynArray.length pool.items in
		DynArray.add pool.items (key,value);
		Hashtbl.add pool.lut hkey (key,index);
		index

	let get pool (hkey : 'hkey) (key : 'key) =
		let l = Hashtbl.find_all pool.lut hkey in
		List.assq key l

	let finalize pool =
		assert (not pool.closed);
		pool.closed <- true;
		pool.items
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

	let reset sb =
		sb.buffer <- Bytes.create sb.buffer_size;
		sb.buffers <- Queue.create ();
		sb.offset <- 0

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
		cp : StringPool.t;
		ch : SimnBuffer.t;
	}

	let create kind cp initial_size = {
		kind;
		cp;
		ch = SimnBuffer.create initial_size;
	}

	let reset chunk =
		SimnBuffer.reset chunk.ch

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

	let export : 'a . t -> 'a IO.output -> unit = fun io chex ->
		let bytes = get_bytes io in
		let length = Bytes.length bytes in
		write_chunk_prefix io.kind length chex;
		IO.nwrite chex bytes

	let write_string chunk s =
		write_uleb128 chunk (StringPool.get_or_add chunk.cp s)

	let write_list : 'b . t -> 'b list -> ('b -> unit) -> unit = fun chunk l f ->
		write_uleb128 chunk (List.length l);
		List.iter f l

	let write_dynarray chunk d f =
		write_uleb128 chunk (DynArray.length d);
		DynArray.iter f d

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

module PosWriter = struct
	type t = {
		mutable p_file : string;
		mutable p_min : int;
		mutable p_max : int;
	}

	let do_write_pos (chunk : Chunk.t) (p : pos) =
		Chunk.write_string chunk p.pfile;
		Chunk.write_leb128 chunk p.pmin;
		Chunk.write_leb128 chunk p.pmax

	let create chunk p =
		do_write_pos chunk p;
	{
		p_file = p.pfile;
		p_min = p.pmin;
		p_max = p.pmax;
	}

	let write_pos pw (chunk : Chunk.t) (write_equal : bool) (offset : int) (p : pos) =
		if p.pfile != pw.p_file then begin
			(* File changed, write full pos *)
			Chunk.write_u8 chunk (4 + offset);
			do_write_pos chunk p;
			pw.p_file <- p.pfile;
			pw.p_min <- p.pmin;
			pw.p_max <- p.pmax;
		end else if p.pmin <> pw.p_min then begin
			if p.pmax <> pw.p_max then begin
				(* pmin and pmax changed *)
				Chunk.write_u8 chunk (3 + offset);
				Chunk.write_leb128 chunk p.pmin;
				Chunk.write_leb128 chunk p.pmax;
				pw.p_min <- p.pmin;
				pw.p_max <- p.pmax;
			end else begin
				(* pmin changed *)
				Chunk.write_u8 chunk (1 + offset);
				Chunk.write_leb128 chunk p.pmin;
				pw.p_min <- p.pmin;
			end
		end else if p.pmax <> pw.p_max then begin
			(* pmax changed *)
			Chunk.write_u8 chunk (2 + offset);
			Chunk.write_leb128 chunk p.pmax;
			pw.p_max <- p.pmax;
		end else begin
			if write_equal then
				Chunk.write_u8 chunk offset;
		end
end

type field_writer_context = {
	t_pool : StringPool.t;
	pos_writer : PosWriter.t;
	mutable texpr_this : texpr option;
	vars : (tvar * int) DynArray.t;
}

let create_field_writer_context pos_writer = {
	t_pool = StringPool.create ();
	pos_writer = pos_writer;
	texpr_this = None;
	vars = DynArray.create ();
}

type hxb_writer = {
	config : HxbWriterConfig.writer_target_config;
	warn : Warning.warning -> string -> Globals.pos -> unit;
	anon_id : Type.t Tanon_identification.tanon_identification;
	mutable current_module : module_def;
	chunks : Chunk.t DynArray.t;
	has_own_string_pool : bool;
	cp : StringPool.t;
	docs : StringPool.t;
	mutable chunk : Chunk.t;

	mutable minimal : bool;
	mutable deps : module_def list;

	classes : (path,tclass) Pool.t;
	enums : (path,tenum) Pool.t;
	typedefs : (path,tdef) Pool.t;
	abstracts : (path,tabstract) Pool.t;
	anons : (path,bytes option) Pool.t;
	anon_fields : (string,tclass_field,bytes option) HashedIdentityPool.t;
	tmonos : (tmono,unit) IdentityPool.t;

	own_classes : (path,tclass) Pool.t;
	own_enums : (path,tenum) Pool.t;
	own_typedefs : (path,tdef) Pool.t;
	own_abstracts : (path,tabstract) Pool.t;
	type_param_lut : (path,(string,typed_type_param) Pool.t) Pool.t;
	class_fields : (string,tclass_field,(tclass * class_field_ref_kind * int)) HashedIdentityPool.t;
	enum_fields : ((path * string),(tenum * tenum_field)) Pool.t;
	mutable type_type_parameters : (string,typed_type_param) Pool.t;
	mutable field_type_parameters : (typed_type_param,unit) IdentityPool.t;
	mutable local_type_parameters : (typed_type_param,unit) IdentityPool.t;
	mutable field_stack : unit list;
	mutable wrote_local_type_param : bool;
	mutable needs_local_context : bool;
	unbound_ttp : (typed_type_param,unit) IdentityPool.t;
	t_instance_chunk : Chunk.t;
}

module HxbWriter = struct
	let get_backtrace () = Printexc.get_raw_backtrace ()
	let get_callstack () = Printexc.get_callstack 200

	let failwith writer msg backtrace =
		let msg =
			(Printf.sprintf "Compiler failure while writing hxb chunk %s of %s: %s\n" (string_of_chunk_kind writer.chunk.kind) (s_type_path writer.current_module.m_path) (msg))
			^ "Please submit an issue at https://github.com/HaxeFoundation/haxe/issues/new\n"
			^ "Attach the following information:"
		in
		let backtrace = Printexc.raw_backtrace_to_string backtrace in
		let s = Printf.sprintf "%s\nHaxe: %s\n%s" msg s_version_full backtrace in
		failwith s

	let in_nested_scope writer = match writer.field_stack with
		| [] -> false (* can happen for cl_init and in EXD *)
		| [_] -> false
		| _ -> true

	(* Chunks *)

	let start_chunk writer (kind : chunk_kind) =
		let initial_size = match kind with
			| EOT | EOF | EOM -> 0
			| MDF -> 16
			| MTF | IMP | CLR | END | ABD | ENR | ABR | TDR | EFR | CFR | AFD -> 64
			| OFR | OFD | OBD | CLD | TDD | EFD -> 128
			| STR | DOC -> 256
			| CFD | EXD -> 512
		in
		let new_chunk = Chunk.create kind writer.cp initial_size in
		DynArray.add writer.chunks new_chunk;
		writer.chunk <- new_chunk

	let start_temporary_chunk : 'a . hxb_writer -> int -> (Chunk.t -> 'a) -> 'a = fun writer initial_size ->
		let new_chunk = Chunk.create EOM (* TODO: something else? *) writer.cp initial_size in
		let old_chunk = writer.chunk in
		writer.chunk <- new_chunk;
		(fun f ->
			writer.chunk <- old_chunk;
			f new_chunk
		)

	let write_inlined_list : 'a . hxb_writer -> int -> int -> (int -> unit) -> (unit -> unit) -> ('a -> unit) -> 'a list -> unit
		= fun writer offset max f_byte f_first f_elt l ->
		let length = List.length l in
		if length > max then begin
			f_byte (offset + 9);
			f_first ();
			Chunk.write_list writer.chunk l f_elt
		end else begin
			f_byte (offset + length);
			f_first();
			List.iter (fun elt ->
				f_elt elt
			) l
		end

	(* Basic compounds *)

	let write_path writer (path : path) =
		Chunk.write_list writer.chunk (fst path) (Chunk.write_string writer.chunk);
		Chunk.write_string writer.chunk (snd path)

	let write_full_path writer (pack : string list) (mname : string) (tname : string) =
		Chunk.write_list writer.chunk pack (Chunk.write_string writer.chunk);
		if mname = "" || tname = "" then
			failwith writer (Printf.sprintf "write_full_path: pack = %s, mname = %s, tname = %s" (String.concat "." pack) mname tname) (get_callstack ());
		Chunk.write_string writer.chunk mname;
		Chunk.write_string writer.chunk tname

	let maybe_write_documentation writer (doc : doc_block option) =
		match doc with
		| Some doc when writer.config.generate_docs ->
			Chunk.write_u8 writer.chunk 1;
			Chunk.write_option writer.chunk doc.doc_own (fun s ->
				Chunk.write_uleb128 writer.chunk (StringPool.get_or_add writer.docs s)
			);
			Chunk.write_list writer.chunk doc.doc_inherited (fun s ->
				Chunk.write_uleb128 writer.chunk (StringPool.get_or_add writer.docs s)
			)
		| _ ->
			Chunk.write_u8 writer.chunk 0

	let write_pos writer (p : pos) =
		Chunk.write_string writer.chunk p.pfile;
		Chunk.write_leb128 writer.chunk p.pmin;
		Chunk.write_leb128 writer.chunk p.pmax

	let write_pos_pair writer (p1 : pos) (p2 : pos) =
		(* Write second position offset relative to first position's pmin, which is often within 1 byte range. *)
		Chunk.write_string writer.chunk p1.pfile;
		Chunk.write_leb128 writer.chunk p1.pmin;
		Chunk.write_leb128 writer.chunk p1.pmax;
		Chunk.write_leb128 writer.chunk (p2.pmin - p1.pmin);
		Chunk.write_leb128 writer.chunk (p2.pmax - p1.pmin)

	let rec write_metadata_entry writer ((meta,el,p) : metadata_entry) =
		Chunk.write_string writer.chunk (Meta.to_string meta);
		write_pos writer p;
		Chunk.write_list writer.chunk el (write_expr writer)

	and write_metadata writer ml =
		Chunk.write_list writer.chunk ml (write_metadata_entry writer)

	(* expr *)

	and write_object_field_key writer (n,p,qs) =
		Chunk.write_string writer.chunk n;
		write_pos writer p;
		begin match qs with
			| NoQuotes -> Chunk.write_u8 writer.chunk 0
			| DoubleQuotes -> Chunk.write_u8 writer.chunk 1
		end

	and write_type_path writer tp =
		Chunk.write_list writer.chunk tp.tpackage (Chunk.write_string writer.chunk);
		Chunk.write_string writer.chunk tp.tname;
		Chunk.write_list writer.chunk tp.tparams (write_type_param_or_const writer);
		Chunk.write_option writer.chunk tp.tsub (Chunk.write_string writer.chunk)

	and write_placed_type_path writer ptp =
		write_type_path writer ptp.path;
		write_pos_pair writer ptp.pos_full ptp.pos_path

	and write_type_param_or_const writer = function
		| TPType th ->
			Chunk.write_u8 writer.chunk 0;
			write_type_hint writer th
		| TPExpr e ->
			Chunk.write_u8 writer.chunk 1;
			write_expr writer e

	and write_complex_type writer = function
		| CTPath tp ->
			Chunk.write_u8 writer.chunk 0;
			write_placed_type_path writer tp
		| CTFunction(thl,th) ->
			Chunk.write_u8 writer.chunk 1;
			Chunk.write_list writer.chunk thl (write_type_hint writer);
			write_type_hint writer th
		| CTAnonymous cffl ->
			Chunk.write_u8 writer.chunk 2;
			Chunk.write_list writer.chunk cffl (write_cfield writer);
		| CTParent th ->
			Chunk.write_u8 writer.chunk 3;
			write_type_hint writer th
		| CTExtend(ptp,cffl) ->
			Chunk.write_u8 writer.chunk 4;
			Chunk.write_list writer.chunk ptp (write_placed_type_path writer);
			Chunk.write_list writer.chunk cffl (write_cfield writer);
		| CTOptional th ->
			Chunk.write_u8 writer.chunk 5;
			write_type_hint writer th
		| CTNamed(pn,th) ->
			Chunk.write_u8 writer.chunk 6;
			write_placed_name writer pn;
			write_type_hint writer th
		| CTIntersection(thl) ->
			Chunk.write_u8 writer.chunk 7;
			Chunk.write_list writer.chunk thl (write_type_hint writer)

	and write_type_hint writer (ct,p) =
		write_complex_type writer ct;
		write_pos writer p

	and write_type_param writer tp =
		write_placed_name writer tp.tp_name;
		Chunk.write_list writer.chunk tp.tp_params (write_type_param writer);
		Chunk.write_option writer.chunk tp.tp_constraints (write_type_hint writer);
		Chunk.write_option writer.chunk tp.tp_default (write_type_hint writer);
		Chunk.write_list writer.chunk tp.tp_meta (write_metadata_entry writer)

	and write_func_arg writer (pn,b,meta,tho,eo) =
		write_placed_name writer pn;
		Chunk.write_bool writer.chunk b;
		write_metadata writer meta;
		Chunk.write_option writer.chunk tho (write_type_hint writer);
		Chunk.write_option writer.chunk eo (write_expr writer);

	and write_func writer f =
		Chunk.write_list writer.chunk f.f_params (write_type_param writer);
		Chunk.write_list writer.chunk f.f_args (write_func_arg writer);
		Chunk.write_option writer.chunk f.f_type (write_type_hint writer);
		Chunk.write_option writer.chunk f.f_expr (write_expr writer)

	and write_placed_name writer (s,p) =
		Chunk.write_string writer.chunk s;
		write_pos writer p

	and write_access writer ac =
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
		Chunk.write_u8 writer.chunk i

	and write_placed_access writer (ac,p) =
		write_access writer ac;
		write_pos writer p

	and write_cfield_kind writer = function
		| FVar(tho,eo) ->
			Chunk.write_u8 writer.chunk 0;
			Chunk.write_option writer.chunk tho (write_type_hint writer);
			Chunk.write_option writer.chunk eo (write_expr writer);
		| FFun f ->
			Chunk.write_u8 writer.chunk 1;
			write_func writer f;
		| FProp(pn1,pn2,tho,eo) ->
			Chunk.write_u8 writer.chunk 2;
			write_placed_name writer pn1;
			write_placed_name writer pn2;
			Chunk.write_option writer.chunk tho (write_type_hint writer);
			Chunk.write_option writer.chunk eo (write_expr writer)

	and write_cfield writer cff =
		write_placed_name writer cff.cff_name;
		maybe_write_documentation writer cff.cff_doc;
		write_pos writer cff.cff_pos;
		write_metadata writer cff.cff_meta;
		Chunk.write_list writer.chunk cff.cff_access (write_placed_access writer);
		write_cfield_kind writer cff.cff_kind

	and write_expr writer (e,p) =
		write_pos writer p;
		match e with
		| EConst (Int (s, suffix)) ->
			Chunk.write_u8 writer.chunk 0;
			Chunk.write_string writer.chunk s;
			Chunk.write_option writer.chunk suffix (Chunk.write_string writer.chunk);
		| EConst (Float (s, suffix)) ->
			Chunk.write_u8 writer.chunk 1;
			Chunk.write_string writer.chunk s;
			Chunk.write_option writer.chunk suffix (Chunk.write_string writer.chunk);
		| EConst (String (s,qs)) ->
			Chunk.write_u8 writer.chunk 2;
			Chunk.write_string writer.chunk s;
			begin match qs with
			| SDoubleQuotes -> Chunk.write_u8 writer.chunk 0;
			| SSingleQuotes -> Chunk.write_u8 writer.chunk 1;
			end
		| EConst (Ident s) ->
			Chunk.write_u8 writer.chunk 3;
			Chunk.write_string writer.chunk s;
		| EConst (Regexp(s1,s2)) ->
			Chunk.write_u8 writer.chunk 4;
			Chunk.write_string writer.chunk s1;
			Chunk.write_string writer.chunk s2;
		| EArray(e1,e2) ->
			Chunk.write_u8 writer.chunk 5;
			write_expr writer e1;
			write_expr writer e2;
		| EBinop(op,e1,e2) ->
			Chunk.write_u8 writer.chunk 6;
			Chunk.write_u8 writer.chunk (binop_index op);
			write_expr writer e1;
			write_expr writer e2;
		| EField(e1,s,kind) ->
			Chunk.write_u8 writer.chunk 7;
			write_expr writer e1;
			Chunk.write_string writer.chunk s;
			begin match kind with
			| EFNormal -> Chunk.write_u8 writer.chunk 0;
			| EFSafe -> Chunk.write_u8 writer.chunk 1;
			end
		| EParenthesis e1 ->
			Chunk.write_u8 writer.chunk 8;
			write_expr writer e1;
		| EObjectDecl fl ->
			Chunk.write_u8 writer.chunk 9;
			let write_field (k,e1) =
				write_object_field_key writer k;
				write_expr writer e1
			in
			Chunk.write_list writer.chunk fl write_field;
		| EArrayDecl el ->
			Chunk.write_u8 writer.chunk 10;
			Chunk.write_list writer.chunk el (write_expr writer);
		| ECall(e1,el) ->
			Chunk.write_u8 writer.chunk 11;
			write_expr writer e1;
			Chunk.write_list writer.chunk el (write_expr writer)
		| ENew(ptp,el) ->
			Chunk.write_u8 writer.chunk 12;
			write_placed_type_path writer ptp;
			Chunk.write_list writer.chunk el (write_expr writer);
		| EUnop(op,flag,e1) ->
			Chunk.write_u8 writer.chunk 13;
			Chunk.write_u8 writer.chunk (unop_index op flag);
			write_expr writer e1;
		| EVars vl ->
			Chunk.write_u8 writer.chunk 14;
			let write_var v =
				write_placed_name writer v.ev_name;
				Chunk.write_bool writer.chunk v.ev_final;
				Chunk.write_bool writer.chunk v.ev_static;
				Chunk.write_option writer.chunk v.ev_type (write_type_hint writer);
				Chunk.write_option writer.chunk v.ev_expr (write_expr writer);
				write_metadata writer v.ev_meta;
			in
			Chunk.write_list writer.chunk vl write_var
		| EFunction(fk,f) ->
			Chunk.write_u8 writer.chunk 15;
			begin match fk with
			| FKAnonymous -> Chunk.write_u8 writer.chunk 0;
			| FKNamed (pn,inline) ->
				Chunk.write_u8 writer.chunk 1;
				write_placed_name writer pn;
				Chunk.write_bool writer.chunk inline;
			| FKArrow -> Chunk.write_u8 writer.chunk 2;
			end;
			write_func writer f;
		| EBlock el ->
			Chunk.write_u8 writer.chunk 16;
			Chunk.write_list writer.chunk el (write_expr writer)
		| EFor(e1,e2) ->
			Chunk.write_u8 writer.chunk 17;
			write_expr writer e1;
			write_expr writer e2;
		| EIf(e1,e2,None) ->
			Chunk.write_u8 writer.chunk 18;
			write_expr writer e1;
			write_expr writer e2;
		| EIf(e1,e2,Some e3) ->
			Chunk.write_u8 writer.chunk 19;
			write_expr writer e1;
			write_expr writer e2;
			write_expr writer e3;
		| EWhile(e1,e2,NormalWhile) ->
			Chunk.write_u8 writer.chunk 20;
			write_expr writer e1;
			write_expr writer e2;
		| EWhile(e1,e2,DoWhile) ->
			Chunk.write_u8 writer.chunk 21;
			write_expr writer e1;
			write_expr writer e2;
		| ESwitch(e1,cases,def) ->
			Chunk.write_u8 writer.chunk 22;
			write_expr writer e1;
			let write_case (el,eg,eo,p) =
				Chunk.write_list writer.chunk el (write_expr writer);
				Chunk.write_option writer.chunk eg (write_expr writer);
				Chunk.write_option writer.chunk eo (write_expr writer);
				write_pos writer p;
			in
			Chunk.write_list writer.chunk cases write_case;
			let write_default (eo,p) =
				Chunk.write_option writer.chunk eo (write_expr writer);
				write_pos writer p
			in
			Chunk.write_option writer.chunk def write_default;
		| ETry(e1,catches) ->
			Chunk.write_u8 writer.chunk 23;
			write_expr writer e1;
			let write_catch (pn,th,e,p) =
				write_placed_name writer pn;
				Chunk.write_option writer.chunk th (write_type_hint writer);
				write_expr writer e;
				write_pos writer p;
			in
			Chunk.write_list writer.chunk catches write_catch;
		| EReturn None ->
			Chunk.write_u8 writer.chunk 24;
		| EReturn (Some e1) ->
			Chunk.write_u8 writer.chunk 25;
			write_expr writer e1;
		| EBreak ->
			Chunk.write_u8 writer.chunk 26;
		| EContinue ->
			Chunk.write_u8 writer.chunk 27;
		| EUntyped e1 ->
			Chunk.write_u8 writer.chunk 28;
			write_expr writer e1;
		| EThrow e1 ->
			Chunk.write_u8 writer.chunk 29;
			write_expr writer e1;
		| ECast(e1,None) ->
			Chunk.write_u8 writer.chunk 30;
			write_expr writer e1;
		| ECast(e1,Some th) ->
			Chunk.write_u8 writer.chunk 31;
			write_expr writer e1;
			write_type_hint writer th;
		| EIs(e1,th) ->
			Chunk.write_u8 writer.chunk 32;
			write_expr writer e1;
			write_type_hint writer th;
		| EDisplay(e1,dk) ->
			Chunk.write_u8 writer.chunk 33;
			write_expr writer e1;
			begin match dk with
			| DKCall -> Chunk.write_u8 writer.chunk 0;
			| DKDot -> Chunk.write_u8 writer.chunk 1;
			| DKStructure -> Chunk.write_u8 writer.chunk 2;
			| DKMarked -> Chunk.write_u8 writer.chunk 3;
			| DKPattern b ->
				Chunk.write_u8 writer.chunk 4;
				Chunk.write_bool writer.chunk b;
			end
		| ETernary(e1,e2,e3) ->
			Chunk.write_u8 writer.chunk 34;
			write_expr writer e1;
			write_expr writer e2;
			write_expr writer e3;
		| ECheckType(e1,th) ->
			Chunk.write_u8 writer.chunk 35;
			write_expr writer e1;
			write_type_hint writer th;
		| EMeta(m,e1) ->
			Chunk.write_u8 writer.chunk 36;
			write_metadata_entry writer m;
			write_expr writer e1

	(* References *)

	let maybe_add_sig_dep writer m =
		if writer.minimal && m.m_path <> writer.current_module.m_path && not (List.exists (fun m' -> m'.m_path = m.m_path) writer.deps) then
			writer.deps <- m :: writer.deps

	let write_class_ref writer (c : tclass) =
		let i = Pool.get_or_add writer.classes c.cl_path c in
		maybe_add_sig_dep writer c.cl_module;
		Chunk.write_uleb128 writer.chunk i

	let write_enum_ref writer (en : tenum) =
		let i = Pool.get_or_add writer.enums en.e_path en in
		maybe_add_sig_dep writer en.e_module;
		Chunk.write_uleb128 writer.chunk i

	let write_typedef_ref writer (td : tdef) =
		let i = Pool.get_or_add writer.typedefs td.t_path td in
		maybe_add_sig_dep writer td.t_module;
		Chunk.write_uleb128 writer.chunk i

	let write_abstract_ref writer (a : tabstract) =
		let i = Pool.get_or_add writer.abstracts a.a_path a in
		maybe_add_sig_dep writer a.a_module;
		Chunk.write_uleb128 writer.chunk i

	let write_tmono_ref writer (mono : tmono) =
		let index = IdentityPool.get_or_add writer.tmonos mono () in
		Chunk.write_uleb128 writer.chunk index

	let write_field_ref writer (c : tclass) (kind : class_field_ref_kind)  (cf : tclass_field) =
		let index = try
			HashedIdentityPool.get writer.class_fields cf.cf_name cf
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
				| CfrInit ->
					Some(c,0)
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
			HashedIdentityPool.add writer.class_fields cf.cf_name cf (c,kind,depth)
		in
		Chunk.write_uleb128 writer.chunk index

	let write_enum_field_ref writer (en : tenum) (ef : tenum_field) =
		let key = (en.e_path,ef.ef_name) in
		try
			Chunk.write_uleb128 writer.chunk (Pool.get writer.enum_fields key)
		with Not_found ->
			ignore(Pool.get_or_add writer.enums en.e_path en);
			Chunk.write_uleb128 writer.chunk (Pool.add writer.enum_fields key (en,ef))

	let write_var_kind writer vk =
		let b,sl = match vk with
			| VUser TVOLocalVariable -> 0, []
			| VUser TVOArgument -> 1, []
			| VUser TVOForVariable -> 2, []
			| VUser TVOPatternVariable -> 3, []
			| VUser TVOCatchVariable -> 4, []
			| VUser TVOLocalFunction -> 5, []
			| VGenerated -> 6, []
			| VInlined -> 7, []
			| VInlinedConstructorVariable sl -> 8, sl
			| VExtractorVariable -> 9, []
			| VAbstractThis -> 10, []
		in begin
			Chunk.write_u8 writer.chunk b;
			if (b == 8) then Chunk.write_list writer.chunk sl (Chunk.write_string writer.chunk);
		end

	let write_var writer fctx v =
		Chunk.write_uleb128 writer.chunk v.v_id;
		Chunk.write_string writer.chunk v.v_name;
		write_var_kind writer v.v_kind;
		Chunk.write_uleb128 writer.chunk v.v_flags;
		write_metadata writer v.v_meta;
		write_pos writer v.v_pos

	let rec write_anon writer (an : tanon) =
		let write_fields () =
			let restore = start_temporary_chunk writer 256 in
			let i = ref 0 in
			PMap.iter (fun _ cf ->
				write_anon_field_ref writer cf;
				incr i;
			) an.a_fields;
			let bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
			Chunk.write_uleb128 writer.chunk !i;
			Chunk.write_bytes writer.chunk bytes;
		in
		begin match !(an.a_status) with
		| Closed ->
			Chunk.write_u8 writer.chunk 0;
			write_fields ()
		| Const ->
			Chunk.write_u8 writer.chunk 1;
			write_fields ()
		| Extend tl ->
			Chunk.write_u8 writer.chunk 2;
			write_types writer tl;
			write_fields ()
		| ClassStatics _ ->
			assert false
		| EnumStatics _ ->
			assert false
		| AbstractStatics _ ->
			assert false
		end

	and write_anon_ref writer (an : tanon) =
		let pfm = Option.get (writer.anon_id#identify_anon ~strict:true an) in
		try
			let index = Pool.get writer.anons pfm.pfm_path in
			Chunk.write_u8 writer.chunk 0;
			Chunk.write_uleb128 writer.chunk index
		with Not_found ->
			let restore = start_temporary_chunk writer 256 in
			writer.needs_local_context <- false;
			write_anon writer an;
			let bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
			if writer.needs_local_context then begin
				let index = Pool.add writer.anons pfm.pfm_path None in
				Chunk.write_u8 writer.chunk 1;
				Chunk.write_uleb128 writer.chunk index;
				Chunk.write_bytes writer.chunk bytes
			end else begin
				let index = Pool.add writer.anons pfm.pfm_path (Some bytes) in
				Chunk.write_u8 writer.chunk 0;
				Chunk.write_uleb128 writer.chunk index;
			end

	and write_anon_field_ref writer cf =
		try
			let index = HashedIdentityPool.get writer.anon_fields cf.cf_name cf in
			Chunk.write_u8 writer.chunk 0;
			Chunk.write_uleb128 writer.chunk index
		with Not_found ->
			let restore = start_temporary_chunk writer 256 in
			let old = writer.wrote_local_type_param in
			writer.wrote_local_type_param <- false;
			ignore(write_class_field_and_overloads_data writer true cf);
			let bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
			if writer.needs_local_context || writer.wrote_local_type_param then begin
				(* If we access something from the method scope, we have to write the anon field immediately.
				   This should be fine because in such cases the field cannot be referenced elsewhere. *)
				let index = HashedIdentityPool.add writer.anon_fields cf.cf_name cf None in
				writer.needs_local_context <- true;
				Chunk.write_u8 writer.chunk 1;
				Chunk.write_uleb128 writer.chunk index;
				Chunk.write_bytes writer.chunk bytes
			end else begin
				let index = HashedIdentityPool.add writer.anon_fields cf.cf_name cf (Some bytes) in
				Chunk.write_u8 writer.chunk 0;
				Chunk.write_uleb128 writer.chunk index;
			end;
			writer.wrote_local_type_param <- old

	(* Type instances *)

	and write_type_parameter_ref writer (ttp : typed_type_param) =
		begin try
			begin match ttp.ttp_host with
			| TPHType ->
				let i = Pool.get writer.type_type_parameters ttp.ttp_name in
				(* TODO: this isn't correct, but if we don't do this we'll have to communicate the current class *)
				writer.wrote_local_type_param <- true;
				Chunk.write_u8 writer.chunk 1;
				Chunk.write_uleb128 writer.chunk i
			| TPHMethod | TPHEnumConstructor | TPHAnonField | TPHConstructor ->
				let i = IdentityPool.get writer.field_type_parameters ttp in
				writer.wrote_local_type_param <- true;
				Chunk.write_u8 writer.chunk 2;
				Chunk.write_uleb128 writer.chunk i;
			| TPHLocal ->
				let index = IdentityPool.get writer.local_type_parameters ttp in
				writer.wrote_local_type_param <- true;
				Chunk.write_u8 writer.chunk 3;
				Chunk.write_uleb128 writer.chunk index;
			| TPHUnbound ->
				raise Not_found
		end with Not_found ->
			(try ignore(IdentityPool.get writer.unbound_ttp ttp) with Not_found -> begin
				ignore(IdentityPool.add writer.unbound_ttp ttp ());
				let p = file_pos (Path.UniqueKey.lazy_path writer.current_module.m_extra.m_file) in
				let msg = Printf.sprintf "Unbound type parameter %s" (s_type_path ttp.ttp_class.cl_path) in
				writer.warn WUnboundTypeParameter msg p
			end);
			writer.wrote_local_type_param <- true;
			Chunk.write_u8 writer.chunk 5;
			write_path writer ttp.ttp_class.cl_path;
		end

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
	and write_type_instance writer t =
		let write_function_arg (n,o,t) =
			Chunk.write_string writer.chunk n;
			Chunk.write_bool writer.chunk o;
			write_type_instance writer t;
		in
		let write_inlined_list offset max f_first f_elt l =
			write_inlined_list writer offset max (Chunk.write_u8 writer.chunk) f_first f_elt l
		in
		match t with
			| TAbstract ({a_path = ([],"Void")},[]) ->
				Chunk.write_u8 writer.chunk 100;
			| TAbstract ({a_path = ([],"Int")},[]) ->
				Chunk.write_u8 writer.chunk 101;
			| TAbstract ({a_path = ([],"Float")},[]) ->
				Chunk.write_u8 writer.chunk 102;
			| TAbstract ({a_path = ([],"Bool")},[]) ->
				Chunk.write_u8 writer.chunk 103;
			| TInst ({cl_path = ([],"String")},[]) ->
				Chunk.write_u8 writer.chunk 104;
			| TMono r ->
				Monomorph.close r;
				begin match r.tm_type with
				| None ->
					Chunk.write_u8 writer.chunk 0;
					write_tmono_ref writer r;
				| Some t ->
					(* Don't write bound monomorphs, write underlying type directly *)
					write_type_instance writer t
				end
			| TLazy f ->
				write_type_instance writer (lazy_type f)
			| TInst({cl_kind = KTypeParameter ttp},[]) ->
				write_type_parameter_ref writer ttp;
			| TInst({cl_kind = KExpr e},[]) ->
				Chunk.write_u8 writer.chunk 13;
				write_expr writer e;
			| TInst(c,[]) ->
				Chunk.write_u8 writer.chunk 40;
				write_class_ref writer c;
			| TEnum(en,[]) ->
				Chunk.write_u8 writer.chunk 50;
				write_enum_ref writer en;
			| TType(td,[]) ->
				let default () =
					Chunk.write_u8 writer.chunk 60;
					write_typedef_ref writer td;
				in
				begin match td.t_type with
				| TAnon an ->
					begin match !(an.a_status) with
						| ClassStatics c ->
							Chunk.write_u8 writer.chunk 10;
							write_class_ref writer c
						| EnumStatics en ->
							Chunk.write_u8 writer.chunk 11;
							write_enum_ref writer en;
						| AbstractStatics a ->
							Chunk.write_u8 writer.chunk 12;
							write_abstract_ref writer a
						| _ ->
							default()
					end
				| _ ->
					default()
				end;
			| TAbstract(a,[]) ->
				Chunk.write_u8 writer.chunk 70;
				write_abstract_ref writer a;
			| TDynamic None ->
				Chunk.write_u8 writer.chunk 4;
			| TFun([],t) when ExtType.is_void (follow_lazy_and_mono t) ->
				Chunk.write_u8 writer.chunk 20;
			| TFun(args,t) when ExtType.is_void (follow_lazy_and_mono t) ->
				write_inlined_list 20 4 (fun () -> ()) write_function_arg args;
			| TFun(args,t) ->
				write_inlined_list 30 4 (fun () -> ()) write_function_arg args;
				write_type_instance writer t;
			| TInst(c,tl) ->
				write_inlined_list 40 2 (fun () -> write_class_ref writer c) (write_type_instance writer) tl;
			| TEnum(en,tl) ->
				write_inlined_list 50 2 (fun () -> write_enum_ref writer en) (write_type_instance writer) tl;
			| TType(td,tl) ->
				write_inlined_list 60 2 (fun () -> write_typedef_ref writer td) (write_type_instance writer) tl;
			| TAbstract(a,tl) ->
				write_inlined_list 70 2 (fun () -> write_abstract_ref writer a) (write_type_instance writer) tl;
			| TAnon an when PMap.is_empty an.a_fields ->
				Chunk.write_u8 writer.chunk 80;
			| TAnon an ->
				Chunk.write_u8 writer.chunk 81;
				write_anon_ref writer an
			| TDynamic (Some t) ->
				Chunk.write_u8 writer.chunk 89;
				write_type_instance writer t

	and write_types writer tl =
		Chunk.write_list writer.chunk tl (write_type_instance writer)

	(* texpr *)

	and write_texpr_type_instance writer (fctx : field_writer_context) (t: Type.t) =
		let old_chunk = writer.chunk in
		writer.chunk <- writer.t_instance_chunk;
		Chunk.reset writer.chunk;
		write_type_instance writer t;
		let t_bytes = Chunk.get_bytes writer.chunk in
		writer.chunk <- old_chunk;
		let index = StringPool.get_or_add fctx.t_pool (Bytes.unsafe_to_string t_bytes) in
		Chunk.write_uleb128 writer.chunk index

	and write_texpr writer (fctx : field_writer_context) (e : texpr) =
		let declare_var v =
			let index = if has_var_flag v VHxb then begin
				(* Duplicate var declaration! Can happen when writing both cf_expr and cf_expr_unoptimized,
				   although it arguably shouldn't. In this case we don't add the var again and instead write
				   out the existing ID.*)
				   v.v_id
			end else begin
				let index = DynArray.length fctx.vars in
				DynArray.add fctx.vars (v,v.v_id);
				(* Store local index in v_id so we find it easily for all the TLocal expressions.
				   This is set back by the var writer in start_texpr. *)
				v.v_id <- index;
				add_var_flag v VHxb;
				index;
			end in
			Chunk.write_uleb128 writer.chunk index;
			Chunk.write_option writer.chunk v.v_extra (fun ve ->
				Chunk.write_list writer.chunk ve.v_params (fun ttp ->
					let index = IdentityPool.add writer.local_type_parameters ttp () in
					Chunk.write_uleb128 writer.chunk index
				);
				Chunk.write_option writer.chunk ve.v_expr (write_texpr writer fctx);
			);
			write_type_instance writer v.v_type;
		in
		let rec loop e =
			let write_type = match e.eexpr with
			(* values 0-19 *)
			| TConst ct ->
				begin match ct with
				| TNull ->
					Chunk.write_u8 writer.chunk 0;
					true
				| TThis ->
					fctx.texpr_this <- Some e;
					Chunk.write_u8 writer.chunk 1;
					false;
				| TSuper ->
					Chunk.write_u8 writer.chunk 2;
					true;
				| TBool false when (ExtType.is_bool (follow_lazy_and_mono e.etype)) ->
					Chunk.write_u8 writer.chunk 3;
					false;
				| TBool true when (ExtType.is_bool (follow_lazy_and_mono e.etype)) ->
					Chunk.write_u8 writer.chunk 4;
					false;
				| TInt i32 when (ExtType.is_int (follow_lazy_and_mono e.etype)) ->
					Chunk.write_u8 writer.chunk 5;
					Chunk.write_i32 writer.chunk i32;
					false;
				| TFloat f when (ExtType.is_float (follow_lazy_and_mono e.etype)) ->
					Chunk.write_u8 writer.chunk 6;
					Chunk.write_string writer.chunk f;
					false;
				| TString s when (ExtType.is_string (follow_lazy_and_mono e.etype)) ->
					Chunk.write_u8 writer.chunk 7;
					Chunk.write_string writer.chunk s;
					false
				| TBool false ->
					Chunk.write_u8 writer.chunk 13;
					true;
				| TBool true ->
					Chunk.write_u8 writer.chunk 14;
					true;
				| TInt i32 ->
					Chunk.write_u8 writer.chunk 15;
					Chunk.write_i32 writer.chunk i32;
					true;
				| TFloat f ->
					Chunk.write_u8 writer.chunk 16;
					Chunk.write_string writer.chunk f;
					true;
				| TString s ->
					Chunk.write_u8 writer.chunk 17;
					Chunk.write_string writer.chunk s;
					true;
				end
			(* vars 20-29 *)
			| TLocal v ->
				Chunk.write_u8 writer.chunk 20;
				Chunk.write_uleb128 writer.chunk v.v_id;
				true; (* I think there are cases where v_type != etype *)
			| TVar(v,None) ->
				Chunk.write_u8 writer.chunk 21;
				declare_var v;
				false;
			| TVar(v,Some e1) ->
				Chunk.write_u8 writer.chunk 22;
				declare_var v;
				loop e1;
				false;
			(* blocks 30-49 *)
			| TBlock [] ->
				Chunk.write_u8 writer.chunk 30;
				true;
			| TBlock el ->
				let restore = start_temporary_chunk writer 256 in
				let i = ref 0 in
				List.iter (fun e ->
					incr i;
					loop e;
				) el;
				let bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
				let l = !i in
				begin match l with
				| 1 -> Chunk.write_u8 writer.chunk 31;
				| 2 -> Chunk.write_u8 writer.chunk 32;
				| 3 -> Chunk.write_u8 writer.chunk 33;
				| 4 -> Chunk.write_u8 writer.chunk 34;
				| 5 -> Chunk.write_u8 writer.chunk 35;
				| _ ->
					if l <= 0xFF then begin
						Chunk.write_u8 writer.chunk 36;
						Chunk.write_u8 writer.chunk l;
					end else begin
						Chunk.write_u8 writer.chunk 39;
						Chunk.write_uleb128 writer.chunk l;
					end;
				end;
				Chunk.write_bytes writer.chunk bytes;
				true;
			(* function 50-59 *)
			| TFunction tf ->
				Chunk.write_u8 writer.chunk 50;
				Chunk.write_list writer.chunk tf.tf_args (fun (v,eo) ->
					declare_var v;
					Chunk.write_option writer.chunk eo loop;
				);
				write_type_instance writer tf.tf_type;
				loop tf.tf_expr;
				true;
			(* texpr compounds 60-79 *)
			| TArray(e1,e2) ->
				Chunk.write_u8 writer.chunk 60;
				loop e1;
				loop e2;
				true;
			| TParenthesis e1 ->
				Chunk.write_u8 writer.chunk 61;
				loop e1;
				false; (* surely this is always the nested type *)
			| TArrayDecl el ->
				Chunk.write_u8 writer.chunk 62;
				loop_el el;
				true;
			| TObjectDecl fl ->
				Chunk.write_u8 writer.chunk 63;
				Chunk.write_list writer.chunk fl (fun ((name,p,qs),e) ->
					Chunk.write_string writer.chunk name;
					write_pos writer p;
					begin match qs with
					| NoQuotes -> Chunk.write_u8 writer.chunk 0;
					| DoubleQuotes -> Chunk.write_u8 writer.chunk 1;
					end;
					loop e
				);
				true;
			| TCall(e1,el) ->
				write_inlined_list writer 70 4 (Chunk.write_u8 writer.chunk) (fun () -> loop e1) loop el;
				true;
			| TMeta(m,e1) ->
				Chunk.write_u8 writer.chunk 65;
				write_metadata_entry writer m;
				loop e1;
				true;
			(* branching 80-89 *)
			| TIf(e1,e2,None) ->
				Chunk.write_u8 writer.chunk 80;
				loop e1;
				loop e2;
				false;
			| TIf(e1,e2,Some e3) ->
				Chunk.write_u8 writer.chunk 81;
				loop e1;
				loop e2;
				loop e3;
				true;
			| TSwitch s ->
				Chunk.write_u8 writer.chunk 82;
				loop s.switch_subject;
				Chunk.write_list writer.chunk s.switch_cases (fun c ->
					loop_el c.case_patterns;
					loop c.case_expr;
				);
				Chunk.write_option writer.chunk s.switch_default loop;
				true;
			| TTry(e1,catches) ->
				Chunk.write_u8 writer.chunk 83;
				loop e1;
				Chunk.write_list writer.chunk catches  (fun (v,e) ->
					declare_var v;
					loop e
				);
				true;
			| TWhile(e1,e2,flag) ->
				Chunk.write_u8 writer.chunk (if flag = NormalWhile then 84 else 85);
				loop e1;
				loop e2;
				false;
			| TFor(v,e1,e2) ->
				Chunk.write_u8 writer.chunk 86;
				declare_var v;
				loop e1;
				loop e2;
				false;
			(* control flow 90-99 *)
			| TReturn None ->
				Chunk.write_u8 writer.chunk 90;
				true;
			| TReturn (Some e1) ->
				Chunk.write_u8 writer.chunk 91;
				loop e1;
				true;
			| TContinue ->
				Chunk.write_u8 writer.chunk 92;
				true;
			| TBreak ->
				Chunk.write_u8 writer.chunk 93;
				true;
			| TThrow e1 ->
				Chunk.write_u8 writer.chunk 94;
				loop e1;
				true;
			(* access 100-119 *)
			| TEnumIndex e1 ->
				Chunk.write_u8 writer.chunk 100;
				loop e1;
				false;
			| TEnumParameter(e1,ef,i) ->
				Chunk.write_u8 writer.chunk 101;
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
				write_enum_field_ref writer en ef;
				Chunk.write_uleb128 writer.chunk i;
				true;
			| TField({eexpr = TConst TThis; epos = p1},FInstance(c,tl,cf)) when fctx.texpr_this <> None ->
				Chunk.write_u8 writer.chunk 111;
				PosWriter.write_pos fctx.pos_writer writer.chunk true 0 p1;
				write_class_ref writer c;
				write_types writer tl;
				write_field_ref writer c CfrMember cf;
				true;
			| TField(e1,FInstance(c,tl,cf)) ->
				Chunk.write_u8 writer.chunk 102;
				loop e1;
				write_class_ref writer c;
				write_types writer tl;
				write_field_ref writer c CfrMember cf;
				true;
			| TField({eexpr = TTypeExpr (TClassDecl c'); epos = p1},FStatic(c,cf)) when c == c' ->
				Chunk.write_u8 writer.chunk 110;
				PosWriter.write_pos fctx.pos_writer writer.chunk true 0 p1;
				write_class_ref writer c;
				write_field_ref writer c CfrStatic cf;
				true;
			| TField(e1,FStatic(c,cf)) ->
				Chunk.write_u8 writer.chunk 103;
				loop e1;
				write_class_ref writer c;
				write_field_ref writer c CfrStatic cf;
				true;
			| TField(e1,FAnon cf) ->
				Chunk.write_u8 writer.chunk 104;
				loop e1;
				write_anon_field_ref writer cf;
				true;
			| TField(e1,FClosure(Some(c,tl),cf)) ->
				Chunk.write_u8 writer.chunk 105;
				loop e1;
				write_class_ref writer c;
				write_types writer tl;
				write_field_ref writer c CfrMember cf;
				true;
			| TField(e1,FClosure(None,cf)) ->
				Chunk.write_u8 writer.chunk 106;
				loop e1;
				write_anon_field_ref writer cf;
				true;
			| TField(e1,FEnum(en,ef)) ->
				Chunk.write_u8 writer.chunk 107;
				loop e1;
				write_enum_ref writer en;
				write_enum_field_ref writer en ef;
				true;
			| TField(e1,FDynamic s) ->
				Chunk.write_u8 writer.chunk 108;
				loop e1;
				Chunk.write_string writer.chunk s;
				true;
			(* module types 120-139 *)
			| TTypeExpr (TClassDecl ({cl_kind = KTypeParameter ttp})) ->
				Chunk.write_u8 writer.chunk 128;
				write_type_parameter_ref writer ttp;
				true;
			| TTypeExpr (TClassDecl c) ->
				Chunk.write_u8 writer.chunk 120;
				write_class_ref writer c;
				false;
			| TTypeExpr (TEnumDecl en) ->
				Chunk.write_u8 writer.chunk 121;
				write_enum_ref writer en;
				false;
			| TTypeExpr (TAbstractDecl a) ->
				Chunk.write_u8 writer.chunk 122;
				write_abstract_ref writer a;
				true;
			| TTypeExpr (TTypeDecl td) ->
				Chunk.write_u8 writer.chunk 123;
				write_typedef_ref writer td;
				true;
			| TCast(e1,None) ->
				Chunk.write_u8 writer.chunk 124;
				loop e1;
				true;
			| TCast(e1,Some md) ->
				Chunk.write_u8 writer.chunk 125;
				loop e1;
				let infos = t_infos md in
				let m = infos.mt_module in
				write_full_path writer (fst m.m_path) (snd m.m_path) (snd infos.mt_path);
				true;
			| TNew(({cl_kind = KTypeParameter ttp}),tl,el) ->
				Chunk.write_u8 writer.chunk 127;
				write_type_parameter_ref writer ttp;
				write_types writer tl;
				loop_el el;
				true;
			| TNew(c,tl,el) ->
				Chunk.write_u8 writer.chunk 126;
				write_class_ref writer c;
				write_types writer tl;
				loop_el el;
				true;
			(* unops 140-159 *)
			| TUnop(op,flag,e1) ->
				Chunk.write_u8 writer.chunk (140 + unop_index op flag);
				loop e1;
				true;
			(* binops 160-219 *)
			| TBinop(op,e1,e2) ->
				Chunk.write_u8 writer.chunk (160 + binop_index op);
				loop e1;
				loop e2;
				true;
			(* rest 250-254 *)
			| TIdent s ->
				Chunk.write_u8 writer.chunk 250;
				Chunk.write_string writer.chunk s;
				true;
			in
			if write_type then
				write_texpr_type_instance writer fctx e.etype;
			PosWriter.write_pos fctx.pos_writer writer.chunk true 0 e.epos;

		and loop_el el =
			Chunk.write_list writer.chunk el loop
		in
		loop e

	and write_type_parameters_forward writer (ttps : typed_type_param list) =
		let write_type_parameter_forward ttp =
			write_path writer ttp.ttp_class.cl_path;
			write_pos writer ttp.ttp_class.cl_name_pos;
			let i = match ttp.ttp_host with
				| TPHType -> 0
				| TPHConstructor -> 1
				| TPHMethod -> 2
				| TPHEnumConstructor -> 3
				| TPHAnonField -> 4
				| TPHLocal -> 5
				| TPHUnbound -> 6
			in
			Chunk.write_u8 writer.chunk i
		in
		Chunk.write_list writer.chunk ttps write_type_parameter_forward

	and write_type_parameters_data writer (ttps : typed_type_param list) =
		let write_type_parameter_data ttp =
			let c = ttp.ttp_class in
			write_metadata writer c.cl_meta;
			write_types writer (get_constraints ttp);
			Chunk.write_option writer.chunk ttp.ttp_default (write_type_instance writer)
		in
		List.iter write_type_parameter_data ttps

	and write_type_parameters writer (ttps : typed_type_param list) =
		write_type_parameters_forward writer ttps;
		write_type_parameters_data writer ttps;

	(* Fields *)

	and write_field_kind writer = function
		| Method MethNormal -> Chunk.write_u8 writer.chunk 0;
		| Method MethInline -> Chunk.write_u8 writer.chunk 1;
		| Method MethDynamic -> Chunk.write_u8 writer.chunk 2;
		| Method MethMacro -> Chunk.write_u8 writer.chunk 3;
		(* normal read *)
		| Var {v_read = AccNormal; v_write = AccNormal } -> Chunk.write_u8 writer.chunk 10
		| Var {v_read = AccNormal; v_write = AccNo } -> Chunk.write_u8 writer.chunk 11
		| Var {v_read = AccNormal; v_write = AccNever } -> Chunk.write_u8 writer.chunk 12
		| Var {v_read = AccNormal; v_write = AccCtor } -> Chunk.write_u8 writer.chunk 13
		| Var {v_read = AccNormal; v_write = AccCall } -> Chunk.write_u8 writer.chunk 14
		(* inline read *)
		| Var {v_read = AccInline; v_write = AccNever } -> Chunk.write_u8 writer.chunk 20
		(* getter read *)
		| Var {v_read = AccCall; v_write = AccNormal } -> Chunk.write_u8 writer.chunk 30
		| Var {v_read = AccCall; v_write = AccNo } -> Chunk.write_u8 writer.chunk 31
		| Var {v_read = AccCall; v_write = AccNever } -> Chunk.write_u8 writer.chunk 32
		| Var {v_read = AccCall; v_write = AccCtor } -> Chunk.write_u8 writer.chunk 33
		| Var {v_read = AccCall; v_write = AccCall } -> Chunk.write_u8 writer.chunk 34
		(* weird/overlooked combinations *)
		| Var {v_read = r;v_write = w } ->
			Chunk.write_u8 writer.chunk 100;
			let f = function
				| AccNormal -> Chunk.write_u8 writer.chunk 0
				| AccNo -> Chunk.write_u8 writer.chunk 1
				| AccNever -> Chunk.write_u8 writer.chunk 2
				| AccCtor -> Chunk.write_u8 writer.chunk 3
				| AccCall -> Chunk.write_u8 writer.chunk 4
				| AccInline -> Chunk.write_u8 writer.chunk 5
				| AccRequire(s,so) ->
					Chunk.write_u8 writer.chunk 6;
					Chunk.write_string writer.chunk s;
					Chunk.write_option writer.chunk so (Chunk.write_string writer.chunk)
			in
			f r;
			f w

	and open_field_scope writer (params : type_params) =
		writer.field_stack <- () :: writer.field_stack;
		let nested = in_nested_scope writer in
		let old_field_params = writer.field_type_parameters in
		let old_local_params = writer.local_type_parameters in
		if not nested then begin
			writer.local_type_parameters <- IdentityPool.create ();
			writer.field_type_parameters <- IdentityPool.create ();
		end;
		List.iter (fun ttp ->
			ignore(IdentityPool.add writer.field_type_parameters ttp ());
		) params;
		(fun () ->
			writer.field_type_parameters <- old_field_params;
			writer.local_type_parameters <- old_local_params;
			writer.field_stack <- List.tl writer.field_stack
		)

	and write_class_field_forward writer cf =
		Chunk.write_string writer.chunk cf.cf_name;
		write_pos_pair writer cf.cf_pos cf.cf_name_pos;
		Chunk.write_list writer.chunk cf.cf_overloads (fun cf ->
			write_class_field_forward writer cf;
		);

	and start_texpr writer (p: pos) =
		let restore = start_temporary_chunk writer 512 in
		let fctx = create_field_writer_context (PosWriter.create writer.chunk p) in
		fctx,(fun () ->
			restore(fun new_chunk ->
				let restore = start_temporary_chunk writer 512 in
				if in_nested_scope writer then
					Chunk.write_u8 writer.chunk 0
				else begin
					Chunk.write_u8 writer.chunk 1;
					let ltp = List.map fst (IdentityPool.to_list writer.local_type_parameters) in
					write_type_parameters writer ltp
				end;
				Chunk.write_option writer.chunk fctx.texpr_this (fun e -> write_type_instance writer e.etype);
				let a = StringPool.finalize fctx.t_pool in
				Chunk.write_uleb128 writer.chunk a.length;
				StringDynArray.iter a (fun bytes ->
					Chunk.write_bytes writer.chunk (Bytes.unsafe_of_string bytes)
				);
				Chunk.write_uleb128 writer.chunk (DynArray.length fctx.vars);
				DynArray.iter (fun (v,v_id) ->
					v.v_id <- v_id;
					remove_var_flag v VHxb;
					write_var writer fctx v;
				) fctx.vars;
				restore(fun newer_chunk -> newer_chunk,new_chunk)
			)
		)

	and commit_field_type_parameters writer (params : type_params) =
		Chunk.write_uleb128 writer.chunk (List.length params);
		if in_nested_scope writer then
			Chunk.write_u8 writer.chunk 0
		else begin
			Chunk.write_u8 writer.chunk 1;
			let ftp = List.map fst (IdentityPool.to_list writer.field_type_parameters) in
			write_type_parameters writer ftp
		end

	and write_class_field_data writer (write_expr_immediately : bool) (cf : tclass_field) =
		let restore = start_temporary_chunk writer 512 in
		write_type_instance writer cf.cf_type;
		Chunk.write_uleb128 writer.chunk cf.cf_flags;
		maybe_write_documentation writer cf.cf_doc;
		write_metadata writer cf.cf_meta;
		write_field_kind writer cf.cf_kind;
		let expr_chunk = match cf.cf_expr with
			| _ when writer.minimal ->
				Chunk.write_u8 writer.chunk 0;
				None
			| None ->
				Chunk.write_u8 writer.chunk 0;
				None
			| Some e when not write_expr_immediately ->
				Chunk.write_u8 writer.chunk 2;
				let fctx,close = start_texpr writer e.epos in
				write_texpr writer fctx e;
				Chunk.write_option writer.chunk cf.cf_expr_unoptimized (write_texpr writer fctx);
				let expr_chunk = close() in
				Some expr_chunk
			| Some e ->
				Chunk.write_u8 writer.chunk 1;
				let fctx,close = start_texpr writer e.epos in
				write_texpr writer fctx e;
				Chunk.write_option writer.chunk cf.cf_expr_unoptimized (write_texpr writer fctx);
				let expr_pre_chunk,expr_chunk = close() in
				Chunk.export_data expr_pre_chunk writer.chunk;
				Chunk.export_data expr_chunk writer.chunk;
				None
		in
		restore (fun new_chunk ->
			commit_field_type_parameters writer cf.cf_params;
			Chunk.export_data new_chunk writer.chunk
		);
		expr_chunk

	and write_class_field_and_overloads_data writer (write_expr_immediately : bool) (cf : tclass_field) =
		let cfl = cf :: cf.cf_overloads in
		Chunk.write_uleb128 writer.chunk (List.length cfl);
		ExtList.List.filter_map (fun cf ->
			let close = open_field_scope writer cf.cf_params in
			let expr_chunk = write_class_field_data writer write_expr_immediately cf in
			close();
			Option.map (fun expr_chunk -> (cf,expr_chunk)) expr_chunk
		) cfl

	(* Module types *)

	let select_type writer (path : path) =
		writer.type_type_parameters <- Pool.extract writer.type_param_lut path

	let write_common_module_type writer (infos : tinfos) : unit =
		Chunk.write_bool writer.chunk infos.mt_private;
		maybe_write_documentation writer infos.mt_doc;
		write_metadata writer infos.mt_meta;
		write_type_parameters_data writer infos.mt_params;
		Chunk.write_list writer.chunk infos.mt_using (fun (c,p) ->
			write_class_ref writer c;
			write_pos writer p;
		)

	let write_class_kind writer = function
		| KNormal ->
			Chunk.write_u8 writer.chunk 0
		| KTypeParameter ttp ->
			die "" __LOC__
		| KExpr e ->
			Chunk.write_u8 writer.chunk 2;
			write_expr writer e;
		| KGeneric ->
			Chunk.write_u8 writer.chunk 3;
		| KGenericInstance(c,tl) ->
			Chunk.write_u8 writer.chunk 4;
			write_class_ref writer c;
			write_types writer tl
		| KMacroType ->
			Chunk.write_u8 writer.chunk 5;
		| KGenericBuild l ->
			Chunk.write_u8 writer.chunk 6;
			Chunk.write_list writer.chunk l (write_cfield writer);
		| KAbstractImpl a ->
			Chunk.write_u8 writer.chunk 7;
			write_abstract_ref writer a;
		| KModuleFields md ->
			Chunk.write_u8 writer.chunk 8

	let write_class writer (c : tclass) =
		begin match c.cl_kind with
		| KAbstractImpl a ->
			select_type writer a.a_path
		| _ ->
			select_type writer c.cl_path;
		end;
		write_common_module_type writer (Obj.magic c);
		write_class_kind writer c.cl_kind;
		Chunk.write_option writer.chunk c.cl_super (fun (c,tl) ->
			write_class_ref writer c;
			write_types writer tl
		);
		Chunk.write_list writer.chunk c.cl_implements (fun (c,tl) ->
			write_class_ref writer c;
			write_types writer tl
		);
		Chunk.write_option writer.chunk c.cl_dynamic (write_type_instance writer);
		Chunk.write_option writer.chunk c.cl_array_access (write_type_instance writer)

	let write_abstract writer (a : tabstract) =
		begin try
			select_type writer a.a_path
		with Not_found ->
			prerr_endline ("Could not select abstract " ^ (s_type_path a.a_path));
		end;
		write_common_module_type writer (Obj.magic a);
		Chunk.write_option writer.chunk a.a_impl (write_class_ref writer);
		if Meta.has Meta.CoreType a.a_meta then
			Chunk.write_u8 writer.chunk 0
		else begin
			Chunk.write_u8 writer.chunk 1;
			write_type_instance writer a.a_this;
		end;
		Chunk.write_list writer.chunk a.a_from (write_type_instance writer);
		Chunk.write_list writer.chunk a.a_to (write_type_instance writer);
		Chunk.write_bool writer.chunk a.a_extern;
		Chunk.write_bool writer.chunk a.a_enum

	let write_abstract_fields writer (a : tabstract) =
		let c = match a.a_impl with
			| None ->
				null_class
			| Some c ->
				c
		in

		Chunk.write_list writer.chunk a.a_array (write_field_ref writer c CfrStatic);
		Chunk.write_option writer.chunk a.a_read (write_field_ref writer c CfrStatic );
		Chunk.write_option writer.chunk a.a_write (write_field_ref writer c CfrStatic);
		Chunk.write_option writer.chunk a.a_call (write_field_ref writer c CfrStatic);

		Chunk.write_list writer.chunk a.a_ops (fun (op, cf) ->
			Chunk.write_u8 writer.chunk (binop_index op);
			write_field_ref writer c CfrStatic cf
		);

		Chunk.write_list writer.chunk a.a_unops (fun (op, flag, cf) ->
			Chunk.write_u8 writer.chunk (unop_index op flag);
			write_field_ref writer c CfrStatic cf
		);

		Chunk.write_list writer.chunk a.a_from_field (fun (t,cf) ->
			write_field_ref writer c CfrStatic cf;
		);

		Chunk.write_list writer.chunk a.a_to_field (fun (t,cf) ->
			write_field_ref writer c CfrStatic cf;
		)

	let write_enum writer (e : tenum) =
		select_type writer e.e_path;
		write_common_module_type writer (Obj.magic e);
		Chunk.write_uleb128 writer.chunk e.e_flags;
		Chunk.write_list writer.chunk e.e_names (Chunk.write_string writer.chunk)

	let write_typedef writer (td : tdef) =
		select_type writer td.t_path;
		write_common_module_type writer (Obj.magic td);
		write_type_instance writer td.t_type

	(* Module *)

	let forward_declare_type writer (mt : module_type) =
		let name = ref "" in
		let i = match mt with
		| TClassDecl c ->
			ignore(Pool.add writer.classes c.cl_path c);
			ignore(Pool.add writer.own_classes c.cl_path c);
			name := snd c.cl_path;
			0
		| TEnumDecl e ->
			ignore(Pool.add writer.enums e.e_path e);
			ignore(Pool.add writer.own_enums e.e_path e);
			name := snd e.e_path;
			1
		| TTypeDecl t ->
			ignore(Pool.add writer.typedefs t.t_path t);
			ignore(Pool.add writer.own_typedefs t.t_path t);
			name := snd t.t_path;
			2
		| TAbstractDecl a ->
			ignore(Pool.add writer.abstracts a.a_path a);
			ignore(Pool.add writer.own_abstracts a.a_path a);
			name := snd a.a_path;
			3
		in

		let infos = t_infos mt in
		Chunk.write_u8 writer.chunk i;
		write_path writer (fst infos.mt_path, !name);
		write_pos_pair writer infos.mt_pos infos.mt_name_pos;
		write_type_parameters_forward writer infos.mt_params;
		let params = Pool.create () in
		writer.type_type_parameters <- params;
		ignore(Pool.add writer.type_param_lut infos.mt_path params);
		List.iter (fun ttp ->
			ignore(Pool.add writer.type_type_parameters ttp.ttp_name ttp)
		) infos.mt_params;

		(* Forward declare fields *)
		match mt with
		| TClassDecl c ->
			Chunk.write_uleb128 writer.chunk c.cl_flags;
			Chunk.write_option writer.chunk c.cl_constructor (write_class_field_forward writer);
			Chunk.write_option writer.chunk c.cl_init (write_class_field_forward writer);

			(* Write in reverse order so reader can read tail-recursively without List.rev *)
			let write_fields cfl =
				let i = ref 0 in
				let rec loop cfl = match cfl with
					| [] ->
						()
					| cf :: cfl ->
						loop cfl;
						write_class_field_forward writer cf;
						incr i;
				in
				let restore = start_temporary_chunk writer 256 in
				loop cfl;
				let bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
				!i,bytes
			in
			let num_fields,field_bytes = write_fields c.cl_ordered_fields in
			let num_statics,static_bytes = write_fields c.cl_ordered_statics in
			Chunk.write_uleb128 writer.chunk num_fields;
			Chunk.write_uleb128 writer.chunk num_statics;
			Chunk.write_bytes writer.chunk field_bytes;
			Chunk.write_bytes writer.chunk static_bytes;

		| TEnumDecl e ->
			Chunk.write_list writer.chunk (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
				Chunk.write_string writer.chunk s;
				write_pos_pair writer ef.ef_pos ef.ef_name_pos;
				Chunk.write_uleb128 writer.chunk ef.ef_index
			);
		| TAbstractDecl a ->
			()
		| TTypeDecl t ->
			()

	let write_string_pool writer kind a =
		start_chunk writer kind;
		Chunk.write_uleb128 writer.chunk a.StringDynArray.length;
		StringDynArray.iter a (fun s ->
			let b = Bytes.unsafe_of_string s in
			Chunk.write_bytes_length_prefixed writer.chunk b;
		)

	let write_module writer (m : module_def) =
		writer.current_module <- m;

		start_chunk writer MTF;
		Chunk.write_list writer.chunk m.m_types (forward_declare_type writer);

		let items = Pool.finalize writer.own_abstracts in
		if DynArray.length items > 0 then begin
			start_chunk writer ABD;
			Chunk.write_dynarray writer.chunk items (write_abstract writer);
			start_chunk writer AFD;
			Chunk.write_dynarray writer.chunk items (write_abstract_fields writer);
		end;
		let items = Pool.finalize writer.own_classes in
		if DynArray.length items > 0 then begin
			start_chunk writer CLD;
			Chunk.write_dynarray writer.chunk items (write_class writer);
			start_chunk writer CFD;
			let expr_chunks = DynArray.create () in
			Chunk.write_dynarray writer.chunk items (fun c ->
				begin match c.cl_kind with
				| KAbstractImpl a ->
					select_type writer a.a_path
				| _ ->
					select_type writer c.cl_path;
				end;

				let c_expr_chunks = DynArray.create () in
				let write_field ref_kind cf =
					let l = write_class_field_and_overloads_data writer false cf in
					List.iter (fun (cf,e) ->
						DynArray.add c_expr_chunks (cf,ref_kind,e);
					) l
				in

				Chunk.write_option writer.chunk c.cl_constructor (write_field CfrConstructor);
				Chunk.write_option writer.chunk c.cl_init (write_field CfrInit);
				Chunk.write_list writer.chunk c.cl_ordered_fields (write_field CfrMember);
				Chunk.write_list writer.chunk c.cl_ordered_statics (write_field CfrStatic);
				if DynArray.length c_expr_chunks > 0 then
					DynArray.add expr_chunks (c,c_expr_chunks)
			);
			if DynArray.length expr_chunks > 0 then begin
				start_chunk writer EXD;
				Chunk.write_dynarray writer.chunk expr_chunks (fun (c,l) ->
					write_class_ref writer c;
					Chunk.write_dynarray writer.chunk l (fun (cf,ref_kind,(e_pre,e)) ->
						write_field_ref writer c ref_kind cf;
						let bytes_pre = Chunk.get_bytes e_pre in
						let bytes_e = Chunk.get_bytes e in
						Chunk.write_uleb128 writer.chunk (Bytes.length bytes_pre + Bytes.length bytes_e);
						Chunk.write_bytes writer.chunk bytes_pre;
						Chunk.write_bytes writer.chunk bytes_e;
					)
				)
			end
		end;
		let items = Pool.finalize writer.own_enums in
		if DynArray.length items > 0 then begin
			start_chunk writer END;
			Chunk.write_dynarray writer.chunk items (write_enum writer);
			start_chunk writer EFD;
			Chunk.write_dynarray writer.chunk items (fun e ->
				Chunk.write_list writer.chunk (PMap.foldi (fun s f acc -> (s,f) :: acc) e.e_constrs []) (fun (s,ef) ->
					select_type writer e.e_path;
					let close = open_field_scope writer ef.ef_params in
					Chunk.write_string writer.chunk s;
					let restore = start_temporary_chunk writer 32 in
					write_type_instance writer ef.ef_type;
					let t_bytes = restore (fun new_chunk -> Chunk.get_bytes new_chunk) in
					commit_field_type_parameters writer ef.ef_params;
					Chunk.write_bytes writer.chunk t_bytes;
					maybe_write_documentation writer ef.ef_doc;
					write_metadata writer ef.ef_meta;
					close();
				);
			)
		end;
		let items = Pool.finalize writer.own_typedefs in
		if DynArray.length items > 0 then begin
			start_chunk writer TDD;
			Chunk.write_dynarray writer.chunk items (write_typedef writer);
		end;

		let items = HashedIdentityPool.finalize writer.class_fields in
		if DynArray.length items > 0 then begin
			start_chunk writer CFR;
			Chunk.write_uleb128 writer.chunk (DynArray.length items);
			DynArray.iter (fun (cf,(c,kind,depth)) ->
				write_class_ref writer c;
				begin match kind with
				| CfrStatic ->
					Chunk.write_u8 writer.chunk 0;
					Chunk.write_string writer.chunk cf.cf_name
				| CfrMember ->
					Chunk.write_u8 writer.chunk 1;
					Chunk.write_string writer.chunk cf.cf_name
				| CfrConstructor ->
					Chunk.write_u8 writer.chunk 2;
				| CfrInit ->
					Chunk.write_u8 writer.chunk 3;
				end;
				Chunk.write_uleb128 writer.chunk depth
			) items;
		end;

		let items = Pool.finalize writer.enum_fields in
		if DynArray.length items > 0 then begin
			start_chunk writer EFR;
			Chunk.write_uleb128 writer.chunk (DynArray.length items);
			DynArray.iter (fun (en,ef) ->
				write_enum_ref writer en;
				Chunk.write_string writer.chunk ef.ef_name;
			) items;
		end;

		let items = HashedIdentityPool.finalize writer.anon_fields in
		if DynArray.length items > 0 then begin
			start_chunk writer OFR;
			Chunk.write_uleb128 writer.chunk (DynArray.length items);
			DynArray.iter (fun (cf,_) ->
				write_class_field_forward writer cf
			) items;

			let anon_fields_with_expr = DynArray.create () in
			DynArray.iteri (fun i (_,bytes) -> match bytes with
				| None ->
					()
				| Some bytes ->
					DynArray.add anon_fields_with_expr (i,bytes)
			) items;
			if DynArray.length anon_fields_with_expr > 0 then begin
				start_chunk writer OFD;
				Chunk.write_uleb128 writer.chunk (DynArray.length anon_fields_with_expr);
				DynArray.iter (fun (index,bytes) ->
					Chunk.write_uleb128 writer.chunk index;
					Chunk.write_bytes writer.chunk bytes
				) anon_fields_with_expr
			end;

		end;

		let items = Pool.finalize writer.classes in
		if DynArray.length items > 0 then begin
			start_chunk writer CLR;
			Chunk.write_dynarray writer.chunk items (fun c ->
				let m = c.cl_module in
				write_full_path writer (fst m.m_path) (snd m.m_path) (snd c.cl_path);
			)
		end;
		let items = Pool.finalize writer.abstracts in
		if DynArray.length items > 0 then begin
			start_chunk writer ABR;
			Chunk.write_dynarray writer.chunk items (fun a ->
				let m = a.a_module in
				write_full_path writer (fst m.m_path) (snd m.m_path) (snd a.a_path);
			)
		end;
		let items = Pool.finalize writer.enums in
		if DynArray.length items > 0 then begin
			start_chunk writer ENR;
			Chunk.write_dynarray writer.chunk items (fun en ->
				let m = en.e_module in
				write_full_path writer (fst m.m_path) (snd m.m_path) (snd en.e_path);
			)
		end;
		let items = Pool.finalize writer.typedefs in
		if DynArray.length items > 0 then begin
			start_chunk writer TDR;
			Chunk.write_dynarray writer.chunk items (fun td ->
				let m = td.t_module in
				write_full_path writer (fst m.m_path) (snd m.m_path) (snd td.t_path);
			)
		end;

		start_chunk writer MDF;
		write_path writer m.m_path;
		Chunk.write_string writer.chunk (Path.UniqueKey.lazy_path m.m_extra.m_file);
		let anons = Pool.finalize writer.anons in
		Chunk.write_uleb128 writer.chunk (DynArray.length anons);
		Chunk.write_uleb128 writer.chunk (DynArray.length (IdentityPool.finalize writer.tmonos));

		let anons_without_context = DynArray.create () in
		DynArray.iteri (fun i bytes -> match bytes with
			| None ->
				()
			| Some bytes ->
				DynArray.add anons_without_context (i,bytes)
		) anons;
		if DynArray.length anons_without_context > 0 then begin
			start_chunk writer OBD;
			Chunk.write_uleb128 writer.chunk (DynArray.length anons_without_context);
			DynArray.iter (fun (i,bytes) ->
				Chunk.write_uleb128 writer.chunk i;
				Chunk.write_bytes writer.chunk bytes
			) anons_without_context
		end;

		begin
			let imports = DynArray.create () in
			PMap.iter (fun _ mdep ->
				match mdep.md_kind, mdep.md_origin with
				| (MCode | MExtern), MDepFromImport when mdep.md_sign = m.m_extra.m_sign ->
					DynArray.add imports mdep.md_path;
				| _ ->
					()
			) m.m_extra.m_deps;

			if DynArray.length imports > 0 then begin
				start_chunk writer IMP;
				Chunk.write_uleb128 writer.chunk (DynArray.length imports);
				DynArray.iter (fun path ->
					write_path writer path
				) imports
			end;
		end;

		start_chunk writer EOT;
		start_chunk writer EOF;
		start_chunk writer EOM;

		if writer.has_own_string_pool then begin
			let a = StringPool.finalize writer.cp in
			write_string_pool writer STR a
		end;
		begin
			let a = StringPool.finalize writer.docs in
			if a.length > 0 then
				write_string_pool writer DOC a
		end

	let get_sorted_chunks writer =
		let l = DynArray.to_list writer.chunks in
		let l = List.sort (fun chunk1 chunk2 ->
			(Obj.magic chunk1.Chunk.kind - (Obj.magic chunk2.kind))
		) l in
		l
end

let get_dependencies writer =
	let deps = ref PMap.empty in

	List.iter (fun mdep ->
		let dep = {md_sign = mdep.m_extra.m_sign; md_path = mdep.m_path; md_kind = mdep.m_extra.m_kind; md_origin = MDepFromTyping} in
		deps := PMap.add mdep.m_id dep !deps;
	) writer.deps;

	!deps

let create config string_pool warn anon_id =
	let cp,has_own_string_pool = match string_pool with
		| None ->
			StringPool.create(),true
		| Some pool ->
			pool,false
	in
	{
		config;
		warn;
		anon_id;
		current_module = null_module;
		chunks = DynArray.create ();
		cp = cp;
		has_own_string_pool;
		deps = [];
		minimal = false;
		docs = StringPool.create ();
		chunk = Obj.magic ();
		classes = Pool.create ();
		enums = Pool.create ();
		typedefs = Pool.create ();
		abstracts = Pool.create ();
		anons = Pool.create ();
		anon_fields = HashedIdentityPool.create ();
		tmonos = IdentityPool.create ();
		own_classes = Pool.create ();
		own_abstracts = Pool.create ();
		own_enums = Pool.create ();
		own_typedefs = Pool.create ();
		type_param_lut = Pool.create ();
		class_fields = HashedIdentityPool.create ();
		enum_fields = Pool.create ();
		type_type_parameters = Pool.create ();
		field_type_parameters = IdentityPool.create ();
		local_type_parameters = IdentityPool.create ();
		field_stack = [];
		wrote_local_type_param = false;
		needs_local_context = false;
		unbound_ttp = IdentityPool.create ();
		t_instance_chunk = Chunk.create EOM cp 32;
	}

let write_module writer m =
	HxbWriter.write_module writer m

let get_chunks writer =
	List.map (fun chunk ->
		(chunk.Chunk.kind,Chunk.get_bytes chunk)
	) (HxbWriter.get_sorted_chunks writer)

let export : 'a . hxb_writer -> 'a IO.output -> unit = fun writer ch ->
	write_header ch;
	let l = HxbWriter.get_sorted_chunks writer in
	List.iter (fun io ->
		Chunk.export io ch
	) l
