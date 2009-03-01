(*
 *  This file is part of SwfLib
 *  Copyright (c)2004-2006 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open As3

let parse_idents = true
let parse_namespaces = true && parse_idents
let parse_ns_sets = true && parse_namespaces
let parse_names = true && parse_ns_sets
let parse_mtypes = true && parse_names
let parse_metadata = true && parse_mtypes
let parse_classes = true && parse_metadata
let parse_statics = true && parse_classes
let parse_inits = true && parse_statics
let parse_functions = true && parse_inits
let parse_bytecode = true && parse_functions

let magic_index (i : int) : 'a index =
	Obj.magic i

let magic_index_nz (i : int) : 'a index_nz =
	Obj.magic i

let index (t : 'a array) (i : int) : 'a index =
	if i <= 0 || i - 1 >= Array.length t then assert false;
	magic_index i

let index_opt t i =
	if i = 0 then
		None
	else
		Some (index t i)

let index_nz (t : 'a array) (i : int) : 'a index_nz =
	if i < 0 || i >= Array.length t then assert false;
	Obj.magic i

let index_int (i : 'a index) =
	(Obj.magic i : int)

let index_nz_int (i : 'a index_nz) =
	(Obj.magic i : int)

let iget (t : 'a array) (i : 'a index) : 'a =
	t.(index_int i - 1)

let no_nz (i : 'a index_nz) : 'a index =
	Obj.magic ((Obj.magic i) + 1)

(* ************************************************************************ *)
(* LENGTH *)

let as3_empty_index ctx =
	let empty_index = ref 0 in
	try
		Array.iteri (fun i x -> if x = "" then begin empty_index := (i + 1); raise Exit; end) ctx.as3_idents;
		if parse_idents then assert false;
		magic_index 0
	with Exit ->
		index ctx.as3_idents (!empty_index)

let as3_int_length i =
	if Int32.compare (Int32.shift_right_logical i 28) 0l > 0 then
		5
	else if Int32.compare (Int32.shift_right i 21) 0l > 0 then
		4
	else if Int32.compare (Int32.shift_right i 14) 0l > 0 then
		3
	else if Int32.compare (Int32.shift_right i 7) 0l > 0 then
		2
	else
		1

let as3_uint_length i =
	as3_int_length i

let sum f l =
	List.fold_left (fun acc n -> acc + f n) 0 l

let int_length i =
	as3_int_length (Int32.of_int i)

let idx_length i =
	int_length (index_int i)

let idx_length_nz i =
	int_length (index_nz_int i)

let idx_opt_length = function
	| None -> int_length 0
	| Some i -> idx_length i

let as3_ident_length s =
	let n = String.length s in
	n + int_length n

let as3_namespace_length ei = function
	| A3NStaticProtected o
	| A3NPrivate o ->
		1 + (match o with None -> int_length 0 | Some n -> idx_length n)
	| A3NPublic o
	| A3NInternal o ->
		1 + idx_length (match o with None -> ei | Some n -> n)
	| A3NExplicit n
	| A3NNamespace n
	| A3NProtected n ->
		1 + idx_length n

let as3_ns_set_length l =
	int_length (List.length l) + sum idx_length l

let rec as3_name_length t =
	1 +
	match t with
	| A3MMultiName (id,r) ->
		idx_opt_length id + idx_length r
	| A3MName (id,r) ->
		idx_length r + idx_length id
	| A3MRuntimeName i ->
		idx_length i
	| A3MRuntimeNameLate ->
		0
	| A3MMultiNameLate idx ->
		idx_length idx
	| A3MAttrib n ->
		as3_name_length n - 1
	| A3MParams (id,pl) ->
		idx_length id + 1 + (sum idx_length pl)

let as3_value_length extra = function
	| A3VNone -> if extra then 2 else 1
	| A3VNull | A3VBool _ -> 2
	| A3VString s -> 1 + idx_length s
	| A3VInt s -> 1 + idx_length s
	| A3VUInt s -> 1 + idx_length s
	| A3VFloat s -> 1 + idx_length s
	| A3VNamespace (_,s) -> 1 + idx_length s

let as3_method_type_length m =
	1 +
	idx_opt_length m.mt3_ret +
	sum idx_opt_length m.mt3_args +
	idx_opt_length m.mt3_debug_name +
	1 +
	(match m.mt3_dparams with None -> 0 | Some l -> 1 + sum (as3_value_length true) l) +
	(match m.mt3_pnames with None -> 0 | Some l -> sum idx_opt_length l)

let list_length f l =
	match Array.length l with
	| 0 -> int_length 0
	| n ->
		Array.fold_left (fun acc x -> acc + f x) (int_length (n + 1)) l

let list2_length f l =
	Array.fold_left (fun acc x -> acc + f x) (int_length (Array.length l)) l

let as3_field_length f =
	idx_length f.f3_name +
	1 +
	int_length f.f3_slot +
	(match f.f3_kind with
	| A3FMethod m ->
		idx_length_nz m.m3_type
	| A3FClass c ->
		idx_length_nz c
	| A3FFunction id ->
		idx_length_nz id
	| A3FVar v ->
		idx_opt_length v.v3_type + as3_value_length false v.v3_value) +
	match f.f3_metas with
	| None -> 0
	| Some l -> list2_length idx_length_nz l

let as3_class_length c =
	idx_length c.cl3_name +
	idx_opt_length c.cl3_super +
	1 +
	(match c.cl3_namespace with None -> 0 | Some r -> idx_length r) +
	list2_length idx_length c.cl3_implements +
	idx_length_nz c.cl3_construct +
	list2_length as3_field_length c.cl3_fields

let as3_static_length s =
	idx_length_nz s.st3_method +
	list2_length as3_field_length s.st3_fields

let as3_metadata_length m =
	idx_length m.meta3_name +
	list2_length (fun (i1,i2) -> idx_opt_length i1 + idx_length i2) m.meta3_data

let as3_try_catch_length t =
	int_length t.tc3_start +
	int_length t.tc3_end +
	int_length t.tc3_handle +
	idx_opt_length t.tc3_type +
	idx_opt_length t.tc3_name

let as3_function_length f =
	let clen = Array.fold_left (fun acc op -> acc + As3code.length op) 0 f.fun3_code in
	idx_length_nz f.fun3_id +
	int_length f.fun3_stack_size +
	int_length f.fun3_nregs +
	int_length f.fun3_init_scope +
	int_length f.fun3_max_scope +
	int_length clen +
	clen +
	list2_length as3_try_catch_length f.fun3_trys +
	list2_length as3_field_length f.fun3_locals

let as3_length ctx =
	let ei = as3_empty_index ctx in
	String.length ctx.as3_unknown +
	4 +
	list_length as3_int_length ctx.as3_ints +
	list_length as3_uint_length ctx.as3_uints +
	list_length (fun _ -> 8) ctx.as3_floats
	+ if parse_idents then list_length as3_ident_length ctx.as3_idents
	+ if parse_namespaces then list_length (as3_namespace_length ei) ctx.as3_namespaces
	+ if parse_ns_sets then list_length as3_ns_set_length ctx.as3_nsets
	+ if parse_names then list_length as3_name_length ctx.as3_names
	+ if parse_mtypes then list2_length as3_method_type_length ctx.as3_method_types
	+ if parse_metadata then list2_length as3_metadata_length ctx.as3_metadatas
	+ if parse_classes then list2_length as3_class_length ctx.as3_classes
	+ if parse_statics then Array.fold_left (fun acc x -> acc + as3_static_length x) 0 ctx.as3_statics
	+ if parse_inits then list2_length as3_static_length ctx.as3_inits
	+ if parse_functions then list2_length as3_function_length ctx.as3_functions
	  else 0 else 0 else 0 else 0 else 0 else 0 else 0 else 0 else 0 else 0

(* ************************************************************************ *)
(* PARSING *)

let read_as3_int ch =
	let a = IO.read_byte ch in
	if a < 128 then
		Int32.of_int a
	else
	let a = a land 127 in
	let b = IO.read_byte ch in
	if b < 128 then
		Int32.of_int ((b lsl 7) lor a)
	else
	let b = b land 127 in
	let c = IO.read_byte ch in
	if c < 128 then
		Int32.of_int ((c lsl 14) lor (b lsl 7) lor a)
	else
	let c = c land 127 in
	let d = IO.read_byte ch in
	if d < 128 then
		Int32.of_int ((d lsl 21) lor (c lsl 14) lor (b lsl 7) lor a)
	else
	let d = d land 127 in
	let e = IO.read_byte ch in
	if e > 15 then assert false;
	let small = Int32.of_int ((d lsl 21) lor (c lsl 14) lor (b lsl 7) lor a) in
	let big = Int32.shift_left (Int32.of_int e) 28 in
	Int32.logor big small

let read_as3_uint ch =
	read_as3_int ch

let read_int ch =
	Int32.to_int (read_as3_int ch)

let read_ident ch =
	IO.nread ch (read_int ch)

let read_namespace idents ch =
	let k = IO.read_byte ch in
	let p = index_opt idents (read_int ch) in
	match k with
	| 0x05 ->
		A3NPrivate p
	| 0x08 ->
		(match p with
		| None -> assert false
		| Some idx -> A3NNamespace idx)
	| 0x16 ->
		(match p with
		| None -> assert false
		| Some p when iget idents p = "" -> A3NPublic None
		| _ -> A3NPublic p)
	| 0x17 ->
		(match p with
		| None -> assert false
		| Some p when iget idents p = "" -> A3NInternal None
		| _ -> A3NInternal p)
	| 0x18 ->
		(match p with
		| None -> assert false
		| Some idx -> A3NProtected idx)
	| 0x19 ->
		(match p with
		| None -> assert false
		| Some idx -> A3NExplicit idx)
	| 0x1A ->
		A3NStaticProtected p
	| _ ->
		assert false

let read_ns_set namespaces ch =
	let rec loop n =
		if n = 0 then
			[]
		else
			let r = index namespaces (read_int ch) in
			r :: loop (n - 1)
	in
	loop (IO.read_byte ch)

let rec read_name ctx ?k ch =
	let k = (match k with None -> IO.read_byte ch | Some k -> k) in
	match k with
	| 0x07 ->
		let ns = index ctx.as3_namespaces (read_int ch) in
		let id = index ctx.as3_idents (read_int ch) in
		(* both ns and id can be 0 <=> '*' *)
		A3MName (id,ns)
	| 0x09 ->
		let id = index_opt ctx.as3_idents (read_int ch) in
		let ns = index ctx.as3_nsets (read_int ch) in
		A3MMultiName (id,ns)
	| 0x0D ->
		A3MAttrib (read_name ctx ~k:0x07 ch)
	| 0x0E ->
		A3MAttrib (read_name ctx ~k:0x09 ch)
	| 0x0F ->
		let id = index ctx.as3_idents (read_int ch) in
		A3MRuntimeName id
	| 0x10 ->
		A3MAttrib (read_name ctx ~k:0x0F ch)
	| 0x11 ->
		A3MRuntimeNameLate
	| 0x12 ->
		A3MAttrib (read_name ctx ~k:0x11 ch)
	| 0x1B ->
		let ns = index ctx.as3_nsets (read_int ch) in
		A3MMultiNameLate ns
	| 0x1C ->
		A3MAttrib (read_name ctx ~k:0x1B ch)
	| 0x1D ->
		let rec loop n =
			if n = 0 then
				[]
			else
				let name = magic_index (read_int ch) in
				name :: loop (n - 1)
		in
		let id = magic_index (read_int ch) in
		A3MParams (id,loop (IO.read_byte ch))
	| n ->
		prerr_endline (string_of_int n);
		assert false

let read_value ctx ch extra =
	let idx = read_int ch in
	if idx = 0 then begin
		if extra && IO.read_byte ch <> 0 then assert false;
		A3VNone
	end else match IO.read_byte ch with
	| 0x01 ->
		A3VString (index ctx.as3_idents idx)
	| 0x03 ->
		A3VInt (index ctx.as3_ints idx)
	| 0x04 ->
		A3VUInt (index ctx.as3_uints idx)
	| 0x06 ->
		A3VFloat (index ctx.as3_floats idx)
	| 0x08 | 0x16 | 0x17 | 0x18 | 0x19 | 0x1A | 0x05 as n->
		A3VNamespace (n,index ctx.as3_namespaces idx)
	| 0x0A ->
		if idx <> 0x0A then assert false;
		A3VBool false
	| 0x0B ->
		if idx <> 0x0B then assert false;
		A3VBool true
	| 0x0C ->
		if idx <> 0x0C then assert false;
		A3VNull
	| _ ->
		assert false

let read_method_type ctx ch =
	let nargs = IO.read_byte ch in
	let tret = index_opt ctx.as3_names (read_int ch) in
	let targs = Array.to_list (Array.init nargs (fun _ -> index_opt ctx.as3_names (read_int ch))) in
	let dname = index_opt ctx.as3_idents (read_int ch) in
	let flags = IO.read_byte ch in
	let dparams = (if flags land 0x08 <> 0 then
		Some (Array.to_list (Array.init (IO.read_byte ch) (fun _ -> read_value ctx ch true)))
	else
		None
	) in
	let pnames = (if flags land 0x80 <> 0 then
		Some (Array.to_list (Array.init nargs (fun _ -> index_opt ctx.as3_idents (read_int ch))))
	else
		None
	) in
	{
		mt3_ret = tret;
		mt3_args = targs;
		mt3_var_args = flags land 0x04 <> 0;
		mt3_native = flags land 0x20 <> 0;
		mt3_new_block = flags land 0x02 <> 0;
		mt3_debug_name = dname;
		mt3_dparams = dparams;
		mt3_pnames = pnames;
		mt3_arguments_defined = flags land 0x01 <> 0;
		mt3_uses_dxns = flags land 0x40 <> 0;
		mt3_unused_flag = flags land 0x10 <> 0;
	}

let read_list ch f =
	match read_int ch with
	| 0 -> [||]
	| n -> Array.init (n - 1) (fun _ -> f ch)

let read_list2 ch f =
	Array.init (read_int ch) (fun _ -> f ch)

let read_field ctx ch =
	let name = index ctx.as3_names (read_int ch) in
	let kind = IO.read_byte ch in
	let has_meta = kind land 0x40 <> 0 in
	let slot = read_int ch in
	let kind = (match kind land 0xF with
		| 0x00 | 0x06 ->
			let t = index_opt ctx.as3_names (read_int ch) in
			let value = read_value ctx ch false in
			A3FVar {
				v3_type = t;
				v3_value = value;
				v3_const = kind = 0x06;
			}
		| 0x02
		| 0x03
		| 0x01 ->
			let meth = index_nz ctx.as3_method_types (read_int ch) in
			let final = kind land 0x10 <> 0 in
			let override = kind land 0x20 <> 0 in
			A3FMethod {
				m3_type = meth;
				m3_final = final;
				m3_override = override;
				m3_kind = (match kind land 0xF with 0x01 -> MK3Normal | 0x02 -> MK3Getter | 0x03 -> MK3Setter | _ -> assert false);
			}
		| 0x04 ->
			let c = index_nz ctx.as3_classes (read_int ch) in
			A3FClass c
		| 0x05 ->
			let f = index_nz ctx.as3_method_types (read_int ch) in
			A3FFunction f
		| _ ->
			assert false
	) in
	let metas = (if has_meta then
		Some (read_list2 ch (fun _ -> index_nz ctx.as3_metadatas (read_int ch)))
	else
		None
	) in
	{
		f3_name = name;
		f3_slot = slot;
		f3_kind = kind;
		f3_metas = metas;
	}

let read_class ctx ch =
	let name = index ctx.as3_names (read_int ch) in
	let csuper = index_opt ctx.as3_names (read_int ch) in
	let flags = IO.read_byte ch in
	let namespace =
		if flags land 8 <> 0 then
			let r = index ctx.as3_namespaces (read_int ch) in
			Some r
		else
			None
	in
	let impls = read_list2 ch (fun _ -> index ctx.as3_names (read_int ch)) in
	let construct = index_nz ctx.as3_method_types (read_int ch) in
	let fields = read_list2 ch (read_field ctx) in
	{
		cl3_name = name;
		cl3_super = csuper;
		cl3_sealed = (flags land 1) <> 0;
		cl3_final = (flags land 2) <> 0;
		cl3_interface = (flags land 4) <> 0;
		cl3_namespace = namespace;
		cl3_implements = impls;
		cl3_construct = construct;
		cl3_fields = fields;
	}

let read_static ctx ch =
	let meth = index_nz ctx.as3_method_types (read_int ch) in
	let fields = read_list2 ch (read_field ctx) in
	{
		st3_method = meth;
		st3_fields = fields;
	}

let read_metadata ctx ch =
	let name = index ctx.as3_idents (read_int ch) in
	let data = read_list2 ch (fun _ -> index_opt ctx.as3_idents (read_int ch)) in
	let data = Array.map (fun i1 -> i1 , index ctx.as3_idents (read_int ch)) data in
	{
		meta3_name = name;
		meta3_data = data;
	}

let read_try_catch ctx ch =
	let start = read_int ch in
	let pend = read_int ch in
	let handle = read_int ch in
	let t = index_opt ctx.as3_names (read_int ch) in
	let name = index_opt ctx.as3_names (read_int ch) in
	{
		tc3_start = start;
		tc3_end = pend;
		tc3_handle = handle;
		tc3_type = t;
		tc3_name = name;
	}

let read_function ctx ch =
	let id = index_nz ctx.as3_method_types (read_int ch) in
	let ss = read_int ch in
	let nregs = read_int ch in
	let init_scope = read_int ch in
	let max_scope = read_int ch in
	let size = read_int ch in
	let code = if parse_bytecode then As3code.parse ch size else Array.init size (fun _ -> A3Unk (IO.read ch)) in
	let trys = read_list2 ch (read_try_catch ctx) in
	let local_funs = read_list2 ch (read_field ctx) in
	{
		fun3_id = id;
		fun3_stack_size = ss;
		fun3_nregs = nregs;
		fun3_init_scope = init_scope;
		fun3_max_scope = max_scope;
		fun3_code = code;
		fun3_trys = trys;
		fun3_locals = local_funs;
	}

let header_magic = 0x002E0010

let parse ch len =
	let data = IO.nread ch len in
	let ch = IO.input_string data in
	if IO.read_i32 ch <> header_magic then assert false;
	let ints = read_list ch read_as3_int in
	let uints = read_list ch read_as3_uint in
	let floats = read_list ch IO.read_double in
	let idents = (if parse_idents then read_list ch read_ident else [||]) in
	let namespaces = (if parse_namespaces then read_list ch (read_namespace idents) else [||]) in
	let nsets = (if parse_ns_sets then read_list ch (read_ns_set namespaces) else [||]) in
	let ctx = {
		as3_ints = ints;
		as3_uints = uints;
		as3_floats = floats;
		as3_idents = idents;
		as3_namespaces = namespaces;
		as3_nsets = nsets;
		as3_names = [||];
		as3_method_types = [||];
		as3_metadatas = [||];
		as3_classes = [||];
		as3_statics = [||];
		as3_inits = [||];
		as3_functions = [||];
		as3_unknown = "";
	} in
	if parse_names then ctx.as3_names <- read_list ch (read_name ctx);
	if parse_mtypes then ctx.as3_method_types <- read_list2 ch (read_method_type ctx);
	if parse_metadata then ctx.as3_metadatas <- read_list2 ch (read_metadata ctx);
	if parse_classes then ctx.as3_classes <- read_list2 ch (read_class ctx);
	if parse_statics then ctx.as3_statics <- Array.map (fun _ -> read_static ctx ch) ctx.as3_classes;
	if parse_inits then ctx.as3_inits <- read_list2 ch (read_static ctx);
	if parse_functions then ctx.as3_functions <- read_list2 ch (read_function ctx);
	ctx.as3_unknown <- IO.read_all ch;
	if parse_functions && String.length ctx.as3_unknown <> 0 then assert false;
	let len2 = as3_length ctx in
	if len2 <> len then begin Printf.printf "%d != %d" len len2; assert false; end;
	ctx

(* ************************************************************************ *)
(* WRITING *)

let write_as3_int ch i =
	let e = Int32.to_int (Int32.shift_right_logical i 28) in
	let d = Int32.to_int (Int32.shift_right i 21) land 0x7F in
	let c = Int32.to_int (Int32.shift_right i 14) land 0x7F in
	let b = Int32.to_int (Int32.shift_right i 7) land 0x7F in
	let a = Int32.to_int (Int32.logand i 0x7Fl) in
	if b <> 0 || c <> 0 || d <> 0 || e <> 0 then begin
		IO.write_byte ch (a lor 0x80);
		if c <> 0 || d <> 0 || e <> 0 then begin
			IO.write_byte ch (b lor 0x80);
			if d <> 0 || e <> 0 then begin
				IO.write_byte ch (c lor 0x80);
				if e <> 0 then begin
					IO.write_byte ch (d lor 0x80);
					IO.write_byte ch e;
				end else
					IO.write_byte ch d;
			end else
				IO.write_byte ch c;
		end else
			IO.write_byte ch b;
	end else
		IO.write_byte ch a

let write_as3_uint = write_as3_int

let write_int ch i =
	write_as3_int ch (Int32.of_int i)

let write_index ch n =
	write_int ch (index_int n)

let write_index_nz ch n =
	write_int ch (index_nz_int n)

let write_index_opt ch = function
	| None -> write_int ch 0
	| Some n -> write_index ch n

let write_as3_ident ch id =
	write_int ch (String.length id);
	IO.nwrite ch id

let write_namespace empty_index ch = function
	| A3NPrivate n ->
		IO.write_byte ch 0x05;
		(match n with
		| None -> write_int ch 0
		| Some n -> write_index ch n);
	| A3NPublic n ->
		IO.write_byte ch 0x16;
		(match n with
		| None -> write_index ch empty_index
		| Some n -> write_index ch n);
	| A3NInternal n ->
		IO.write_byte ch 0x17;
		(match n with
		| None -> write_index ch empty_index
		| Some n -> write_index ch n);
	| A3NProtected n ->
		IO.write_byte ch 0x18;
		write_index ch n
	| A3NNamespace n ->
		IO.write_byte ch 0x08;
		write_index ch n
	| A3NExplicit n ->
		IO.write_byte ch 0x19;
		write_index ch n
	| A3NStaticProtected n ->
		IO.write_byte ch 0x1A;
		(match n with
		| None -> write_int ch 0
		| Some n -> write_index ch n)

let write_rights ch l =
	IO.write_byte ch (List.length l);
	List.iter (write_index ch) l

let rec write_name ch ?k x =
	let b n = match k with None -> n | Some v -> v in
	match x with
	| A3MMultiName (id,r) ->
		IO.write_byte ch (b 0x09);
		write_index_opt ch id;
		write_index ch r;
	| A3MName (id,r) ->
		IO.write_byte ch (b 0x07);
		write_index ch r;
		write_index ch id
	| A3MRuntimeName i ->
		IO.write_byte ch (b 0x0F);
		write_index ch i
	| A3MRuntimeNameLate ->
		IO.write_byte ch (b 0x11);
	| A3MMultiNameLate id ->
		IO.write_byte ch (b 0x1B);
		write_index ch id
	| A3MAttrib n ->
		write_name ch ~k:(match n with
			| A3MName _ -> 0x0D
			| A3MMultiName _ -> 0x0E
			| A3MRuntimeName _ -> 0x10
			| A3MRuntimeNameLate -> 0x12
			| A3MMultiNameLate _ -> 0x1C
			| A3MAttrib _ | A3MParams _ -> assert false
		) n
	| A3MParams (id,pl) ->
		IO.write_byte ch (b 0x1D);
		write_index ch id;
		IO.write_byte ch (List.length pl);
		List.iter (write_index ch) pl

let write_value ch extra v =
	match v with
	| A3VNone ->
		IO.write_byte ch 0x00;
		if extra then IO.write_byte ch 0x00;
	| A3VNull ->
		IO.write_byte ch 0x0C;
		IO.write_byte ch 0x0C;
	| A3VBool b ->
		IO.write_byte ch (if b then 0x0B else 0x0A);
		IO.write_byte ch (if b then 0x0B else 0x0A);
	| A3VString s ->
		write_index ch s;
		IO.write_byte ch 0x01;
	| A3VInt s ->
		write_index ch s;
		IO.write_byte ch 0x03;
	| A3VUInt s ->
		write_index ch s;
		IO.write_byte ch 0x04;
	| A3VFloat s ->
		write_index ch s;
		IO.write_byte ch 0x06
	| A3VNamespace (n,s) ->
		write_index ch s;
		IO.write_byte ch n

let write_method_type ch m =
	let nargs = List.length m.mt3_args in
	IO.write_byte ch nargs;
	write_index_opt ch m.mt3_ret;
	List.iter (write_index_opt ch) m.mt3_args;
	write_index_opt ch m.mt3_debug_name;
	let flags =
		(if m.mt3_arguments_defined then 0x01 else 0) lor
		(if m.mt3_new_block then 0x02 else 0) lor
		(if m.mt3_var_args then 0x04 else 0) lor
		(if m.mt3_dparams <> None then 0x08 else 0) lor
		(if m.mt3_unused_flag then 0x10 else 0) lor
		(if m.mt3_native then 0x20 else 0) lor
		(if m.mt3_uses_dxns then 0x40 else 0) lor
		(if m.mt3_pnames <> None then 0x80 else 0)
	in
	IO.write_byte ch flags;
	(match m.mt3_dparams with
	| None -> ()
	| Some l ->
		IO.write_byte ch (List.length l);
		List.iter (write_value ch true) l);
	match m.mt3_pnames with
	| None -> ()
	| Some l ->
		if List.length l <> nargs then assert false;
		List.iter (write_index_opt ch) l

let write_list ch f l =
	match Array.length l with
	| 0 -> IO.write_byte ch 0
	| n ->
		write_int ch (n + 1);
		Array.iter (f ch) l

let write_list2 ch f l =
	write_int ch (Array.length l);
	Array.iter (f ch) l

let write_field ch f =
	write_index ch f.f3_name;
	let flags = (if f.f3_metas <> None then 0x40 else 0) in
	(match f.f3_kind with
	| A3FMethod m ->
		let base = (match m.m3_kind with MK3Normal -> 0x01 | MK3Getter -> 0x02 | MK3Setter -> 0x03) in
		let flags = flags lor (if m.m3_final then 0x10 else 0) lor (if m.m3_override then 0x20 else 0) in
		IO.write_byte ch (base lor flags);
		write_int ch f.f3_slot;
		write_index_nz ch m.m3_type;
	| A3FClass c ->
		IO.write_byte ch (0x04 lor flags);
		write_int ch f.f3_slot;
		write_index_nz ch c
	| A3FFunction i ->
		IO.write_byte ch (0x05 lor flags);
		write_int ch f.f3_slot;
		write_index_nz ch i
	| A3FVar v ->
		IO.write_byte ch (flags lor (if v.v3_const then 0x06 else 0x00));
		write_int ch f.f3_slot;
		write_index_opt ch v.v3_type;
		write_value ch false v.v3_value);
	match f.f3_metas with
	| None -> ()
	| Some l ->
		write_list2 ch write_index_nz l

let write_class ch c =
	write_index ch c.cl3_name;
	write_index_opt ch c.cl3_super;
	let flags =
		(if c.cl3_sealed then 1 else 0) lor
		(if c.cl3_final then 2 else 0) lor
		(if c.cl3_interface then 4 else 0) lor
		(if c.cl3_namespace <> None then 8 else 0)
	in
	IO.write_byte ch flags;
	(match c.cl3_namespace with
	| None -> ()
	| Some r -> write_index ch r);
	write_list2 ch write_index c.cl3_implements;
	write_index_nz ch c.cl3_construct;
	write_list2 ch write_field c.cl3_fields

let write_static ch s =
	write_index_nz ch s.st3_method;
	write_list2 ch write_field s.st3_fields

let write_metadata ch m =
	write_index ch m.meta3_name;
	write_list2 ch (fun _ (i1,_) -> write_index_opt ch i1) m.meta3_data;
	Array.iter (fun (_,i2) -> write_index ch i2) m.meta3_data

let write_try_catch ch t =
	write_int ch t.tc3_start;
	write_int ch t.tc3_end;
	write_int ch t.tc3_handle;
	write_index_opt ch t.tc3_type;
	write_index_opt ch t.tc3_name

let write_function ch f =
	write_index_nz ch f.fun3_id;
	write_int ch f.fun3_stack_size;
	write_int ch f.fun3_nregs;
	write_int ch f.fun3_init_scope;
	write_int ch f.fun3_max_scope;
	let clen = Array.fold_left (fun acc op -> acc + As3code.length op) 0 f.fun3_code in
	write_int ch clen;
	Array.iter (As3code.write ch) f.fun3_code;
	write_list2 ch write_try_catch f.fun3_trys;
	write_list2 ch write_field f.fun3_locals

let write ch1 ctx =
	let ch = IO.output_string() in
	let empty_index = as3_empty_index ctx in
	IO.write_i32 ch header_magic;
	write_list ch write_as3_int ctx.as3_ints;
	write_list ch write_as3_uint ctx.as3_uints;
	write_list ch IO.write_double ctx.as3_floats;
	if parse_idents then write_list ch write_as3_ident ctx.as3_idents;
	if parse_namespaces then write_list ch (write_namespace empty_index) ctx.as3_namespaces;
	if parse_ns_sets then write_list ch write_rights ctx.as3_nsets;
	if parse_names then write_list ch (write_name ?k:None) ctx.as3_names;
	if parse_mtypes then write_list2 ch write_method_type ctx.as3_method_types;
	if parse_metadata then write_list2 ch write_metadata ctx.as3_metadatas;
	if parse_classes then write_list2 ch write_class ctx.as3_classes;
	if parse_statics then Array.iter (write_static ch) ctx.as3_statics;
	if parse_inits then write_list2 ch write_static ctx.as3_inits;
	if parse_functions then write_list2 ch write_function ctx.as3_functions;
	IO.nwrite ch ctx.as3_unknown;
	let str = IO.close_out ch in
	IO.nwrite ch1 str

(* ************************************************************************ *)
(* DUMP *)

let dump_code_size = ref true

let ident_str ctx i =
	iget ctx.as3_idents i

let namespace_str ctx i =
	match iget ctx.as3_namespaces i with
	| A3NPrivate None -> "private"
	| A3NPrivate (Some n) -> "private:" ^ ident_str ctx n
	| A3NPublic None -> "public"
	| A3NPublic (Some n) -> "public:" ^ ident_str ctx n
	| A3NInternal None -> "internal"
	| A3NInternal (Some n) -> "internal:" ^ ident_str ctx n
	| A3NProtected n -> "protected:" ^ ident_str ctx n
	| A3NExplicit n -> "explicit:" ^ ident_str ctx n
	| A3NStaticProtected None -> "static_protected"
	| A3NStaticProtected (Some n) -> "static_protectec:" ^ ident_str ctx n
	| A3NNamespace n -> "namespace:" ^ ident_str ctx n

let ns_set_str ctx i =
	let l = iget ctx.as3_nsets i in
	String.concat " " (List.map (fun r -> namespace_str ctx r) l)

let rec name_str ctx kind t =
	let rec loop = function
		| A3MName (id,r) -> Printf.sprintf "%s %s%s" (namespace_str ctx r) kind (ident_str ctx id)
		| A3MMultiName (id,r) -> Printf.sprintf "[%s %s%s]" (ns_set_str ctx r) kind (match id with None -> "NO" | Some i -> ident_str ctx i)
		| A3MRuntimeName id -> Printf.sprintf "'%s'" (ident_str ctx id)
		| A3MRuntimeNameLate -> "RTLATE"
		| A3MMultiNameLate id -> Printf.sprintf "late:(%s)" (ns_set_str ctx id)
		| A3MAttrib n -> "attrib " ^ loop n
		| A3MParams (id,pl) -> Printf.sprintf "%s<%s>" (name_str ctx kind id) (String.concat "," (List.map (name_str ctx kind) pl))
	in
	loop (iget ctx.as3_names t)

let value_str ctx v =
	match v with
	| A3VNone -> "<none>"
	| A3VNull -> "null"
	| A3VString s -> "\"" ^ ident_str ctx s ^ "\""
	| A3VBool b -> if b then "true" else "false"
	| A3VInt s -> Printf.sprintf "%ld" (iget ctx.as3_ints s)
	| A3VUInt s -> Printf.sprintf "%ld" (iget ctx.as3_uints s)
	| A3VFloat s -> Printf.sprintf "%f" (iget ctx.as3_floats s)
	| A3VNamespace (_,s) -> "ns::" ^ namespace_str ctx s

let metadata_str ctx i =
	let m = iget ctx.as3_metadatas i in
	let data = List.map (fun (i1,i2) -> Printf.sprintf "%s=\"%s\"" (match i1 with None -> "NO" | Some i -> ident_str ctx i) (ident_str ctx i2)) (Array.to_list m.meta3_data) in
	Printf.sprintf "%s(%s)" (ident_str ctx m.meta3_name) (String.concat ", " data)

let method_str ?(infos=false) ctx m =
	let m = iget ctx.as3_method_types m in
	let pcount = ref 0 in
	Printf.sprintf "%s(%s%s)%s"
	(if m.mt3_native then " native " else "")
	(String.concat ", " (List.map (fun a ->
		let id = (match m.mt3_pnames with
			| None -> "p" ^ string_of_int !pcount
			| Some l -> 
				match List.nth l !pcount with
				| None -> "p" ^ string_of_int !pcount
				| Some i -> ident_str ctx i
		) in
		let p = (match a with None -> id | Some t -> name_str ctx (id ^ " : ") t) in

		let p = (match m.mt3_dparams with
		| None -> p
		| Some l ->
			let vargs = List.length m.mt3_args - List.length l in
			if !pcount >= vargs then
				let v = List.nth l (!pcount - vargs) in
				p  ^ " = " ^ value_str ctx v
			else
				p
		) in
		incr pcount;
		p
	) m.mt3_args))
	(if m.mt3_var_args then " ..." else "")
	(match m.mt3_ret with None -> "" | Some t -> " : " ^ name_str ctx "" t)
	^ (if infos then begin
		let name = (match m.mt3_debug_name with None -> "" | Some idx -> Printf.sprintf " '%s'" (ident_str ctx idx))  in
		Printf.sprintf "%s blk:%b args:%b dxns:%b%s" name m.mt3_new_block m.mt3_arguments_defined m.mt3_uses_dxns (if m.mt3_unused_flag then " SPECIAL-FLAG" else "")
	end else "")

let dump_field ctx ch stat f =
(*	(match f.f3_metas with
	| None -> ()
	| Some l -> Array.iter (fun i -> IO.printf ch "    [%s]\n" (metadata_str ctx (no_nz i))) l);
*)	IO.printf ch "    ";
	if stat then IO.printf ch "static ";
	(match f.f3_kind with
	| A3FVar v ->
		IO.printf ch "%s" (name_str ctx (if v.v3_const then "const " else "var ") f.f3_name);
		(match v.v3_type with
		| None -> ()
		| Some id -> IO.printf ch " : %s" (name_str ctx "" id));
		if v.v3_value <> A3VNone then IO.printf ch " = %s" (value_str ctx v.v3_value);
	| A3FClass c ->
		let c = iget ctx.as3_classes (no_nz c) in
		IO.printf ch "%s = %s" (name_str ctx "CLASS " c.cl3_name) (name_str ctx "class " f.f3_name);
	| A3FFunction id ->
		IO.printf ch "%s = %s" (method_str ~infos:false ctx (no_nz id)) (name_str ctx "method " f.f3_name);
	| A3FMethod m ->
		if m.m3_final then IO.printf ch "final ";
		if m.m3_override then IO.printf ch "override ";
		let k = "function " ^ (match m.m3_kind with
			| MK3Normal -> ""
			| MK3Getter -> "get "
			| MK3Setter -> "set "
		) in
		IO.printf ch "%s%s #%d" (name_str ctx k f.f3_name) (method_str ctx (no_nz m.m3_type)) (index_nz_int m.m3_type);
	);
	if f.f3_slot <> 0 then IO.printf ch " = [SLOT:%d]" f.f3_slot;
	IO.printf ch ";\n"

let dump_class ctx ch idx c =
	let st = if parse_statics then ctx.as3_statics.(idx) else { st3_method = magic_index_nz (-1); st3_fields = [||] } in
	if not c.cl3_sealed then IO.printf ch "dynamic ";
	if c.cl3_final then IO.printf ch "final ";
	(match c.cl3_namespace with
	| None -> ()
	| Some r -> IO.printf ch "%s " (namespace_str ctx r));
	let kind = (if c.cl3_interface then "interface " else "class ") in
	IO.printf ch "%s " (name_str ctx kind c.cl3_name);
	(match c.cl3_super with
	| None -> ()
	| Some s -> IO.printf ch "extends %s " (name_str ctx "" s));
	(match Array.to_list c.cl3_implements with
	| [] -> ()
	| l ->
		IO.printf ch "implements %s " (String.concat ", " (List.map (fun i -> name_str ctx "" i) l)));
	IO.printf ch "{\n";
	Array.iter (dump_field ctx ch false) c.cl3_fields;
	Array.iter (dump_field ctx ch true) st.st3_fields;
	IO.printf ch "} constructor#%d statics#%d\n\n" (index_nz_int c.cl3_construct) (index_nz_int st.st3_method)

let dump_init ctx ch idx s =
	IO.printf ch "init #%d {\n" (index_nz_int s.st3_method);
	Array.iter (dump_field ctx ch false) s.st3_fields;
	IO.printf ch "}\n\n"

let dump_try_catch ctx ch t =
	IO.printf ch "    try %d %d %d (%s) (%s)\n"
		t.tc3_start t.tc3_end t.tc3_handle
		(match t.tc3_type with None -> "*" | Some idx -> name_str ctx "" idx)
		(match t.tc3_name with None -> "NO" | Some idx -> name_str ctx "" idx)

let dump_function ctx ch idx f =
	IO.printf ch "function #%d %s\n" (index_nz_int f.fun3_id) (method_str ~infos:true ctx (no_nz f.fun3_id));
	IO.printf ch "    stack:%d nregs:%d scope:%d-%d\n" f.fun3_stack_size f.fun3_nregs f.fun3_init_scope f.fun3_max_scope;
	Array.iter (dump_field ctx ch false) f.fun3_locals;
	Array.iter (dump_try_catch ctx ch) f.fun3_trys;
	let pos = ref 0 in
	Array.iter (fun op ->
		IO.printf ch "%4d    %s\n" !pos (As3code.dump ctx op);
		if !dump_code_size then pos := !pos + As3code.length op else incr pos;
	) f.fun3_code;
	IO.printf ch "\n"

let dump_ident ctx ch idx _ =
	IO.printf ch "I%d = %s\n" (idx + 1) (ident_str ctx (index ctx.as3_idents (idx + 1)))

let dump_namespace ctx ch idx _ =
	IO.printf ch "N%d = %s\n" (idx + 1) (namespace_str ctx (index ctx.as3_namespaces (idx + 1)))

let dump_ns_set ctx ch idx _ =
	IO.printf ch "S%d = %s\n" (idx + 1) (ns_set_str ctx (index ctx.as3_nsets (idx + 1)))

let dump_name ctx ch idx _ =
	IO.printf ch "T%d = %s\n" (idx + 1) (name_str ctx "" (index ctx.as3_names (idx + 1)))

let dump_method_type ctx ch idx _ =
	IO.printf ch "M%d = %s\n" (idx + 1) (method_str ~infos:true ctx (index ctx.as3_method_types (idx + 1)))

let dump_metadata ctx ch idx _ =
	IO.printf ch "D%d = %s\n" (idx + 1) (metadata_str ctx (index ctx.as3_metadatas (idx + 1)))

let dump_int ctx ch idx i =
	IO.printf ch "INT %d = 0x%lX\n" (idx + 1) i

let dump_float ctx ch idx f =
	IO.printf ch "FLOAT %d = %f\n" (idx + 1) f

let dump ch ctx id =
	(match id with
	| None -> IO.printf ch "\n---------------- AS3 -------------------------\n\n";
	| Some (id,f) -> IO.printf ch "\n---------------- AS3 %s [%d] -----------------\n\n" f id);
(*	Array.iteri (dump_int ctx ch) ctx.as3_ints;
	Array.iteri (dump_float ctx ch) ctx.as3_floats;
	Array.iteri (dump_ident ctx ch) ctx.as3_idents;
	IO.printf ch "\n";
	Array.iteri (dump_namespace ctx ch) ctx.as3_namespaces;
	IO.printf ch "\n";
	Array.iteri (dump_ns_set ctx ch) ctx.as3_nsets;
	IO.printf ch "\n";
	Array.iteri (dump_name ctx ch) ctx.as3_names;
	IO.printf ch "\n"; *)
(*	Array.iteri (dump_metadata ctx ch) ctx.as3_metadatas; *)
	Array.iteri (dump_class ctx ch) ctx.as3_classes;
	Array.iteri (dump_init ctx ch) ctx.as3_inits;
	Array.iteri (dump_function ctx ch) ctx.as3_functions;
	IO.printf ch "\n"

;;
As3code.f_int_length := int_length;
As3code.f_int_read := read_int;
As3code.f_int_write := write_int;
