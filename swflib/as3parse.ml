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

let parse_base_rights = true
let parse_rights = true && parse_base_rights
let parse_types = true && parse_rights
let parse_mtypes = true && parse_types
let parse_classes = true && parse_mtypes
let parse_statics = true && parse_classes	

let index (t : 'a array) (i : int) : 'a index =
	if i <= 0 || i - 1 >= Array.length t then assert false;
	Obj.magic i

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
		assert false
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

let as3_base_right_length ei = function
	| A3RPrivate -> 1 + idx_length ei
	| A3RPublic o
	| A3RInternal o ->
		1 + idx_length (match o with None -> ei | Some n -> n)
	| A3RProtected n ->
		1 + idx_length n

let as3_rights_length l =
	int_length (List.length l) + sum idx_length_nz l

let as3_type_length t =
	1 + 
	match t with
	| A3TClassInterface (id,r) ->
		idx_length id + idx_length_nz r
	| A3TMethodVar (id,r) ->
		idx_length r + idx_length id

let as3_method_type_length m =
	1 +
	idx_opt_length m.mt3_ret +
	sum idx_opt_length m.mt3_args +
	2

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
	match f.f3_kind with
	| A3FMethod id ->
		idx_length id
	| A3FVar (id,v) ->
		idx_length id +
		match v with
		| None -> 1
		| Some _ -> assert false

let as3_class_length c =
	idx_length c.cl3_name +
	idx_length c.cl3_super +
	1 +
	(match c.cl3_rights with None -> 0 | Some r -> idx_length r) +
	list2_length idx_length c.cl3_implements +
	int_length c.cl3_slot +
	list2_length as3_field_length c.cl3_fields

let as3_static_length s =
	int_length s.st3_slot +
	list2_length as3_field_length s.st3_fields

let as3_length ctx =
	let ei = as3_empty_index ctx in
	4 +
	String.length ctx.as3_frame + 1 +
	4 +
	list_length as3_int_length ctx.as3_ints +
	1 +
	list_length (fun _ -> 8) ctx.as3_floats +
	list_length as3_ident_length ctx.as3_idents +
	(if parse_base_rights then list_length (as3_base_right_length ei) ctx.as3_base_rights
	+ if parse_rights then list_length as3_rights_length ctx.as3_rights
	+ if parse_types then list_length as3_type_length ctx.as3_types
	+ if parse_mtypes then
		list2_length as3_method_type_length ctx.as3_method_types +
		1
	+ if parse_classes then list2_length as3_class_length ctx.as3_classes
	+ if parse_statics then Array.fold_left (fun acc x -> acc + as3_static_length x) 0 ctx.as3_statics
	else 0 else 0 else 0 else 0 else 0 else 0) +
	String.length ctx.as3_unknown

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

let read_int ch =
	Int32.to_int (read_as3_int ch)

let read_ident ch =
	IO.nread ch (read_int ch)

let read_base_right idents ch =
	let k = IO.read_byte ch in
	let p = index_opt idents (read_int ch) in
	match k with
	| 0x05 ->
		if p <> None then assert false;
		A3RPrivate
	| 0x16 ->
		(match p with
		| None -> assert false
		| Some p when iget idents p = "" -> A3RPublic None
		| _ -> A3RPublic p)
	| 0x17 ->
		(match p with
		| None -> assert false
		| Some p when iget idents p = "" -> A3RInternal None
		| _ -> A3RInternal p)
	| 0x18 ->
		(match p with
		| None -> assert false
		| Some idx -> A3RProtected idx)
	| _ ->
		assert false

let read_rights base_rights ch =
	let rec loop n =
		if n = 0 then
			[]
		else
			let r = index_nz base_rights (read_int ch) in			
			r :: loop (n - 1)
	in
	loop (IO.read_byte ch)

let read_type ctx ch =
	match IO.read_byte ch with
	| 0x09 ->
		let id = index ctx.as3_idents (read_int ch) in
		let rights = index_nz ctx.as3_rights (read_int ch) in		
		A3TClassInterface (id,rights)
	| 0x07 ->
		let rights = index ctx.as3_base_rights (read_int ch) in
		let id = index ctx.as3_idents (read_int ch) in
		A3TMethodVar (id,rights)
	| _ ->
		assert false

let read_method_type ctx ch =
	let nargs = IO.read_byte ch in
	let tret = index_opt ctx.as3_types (read_int ch) in	
	let targs = Array.to_list (Array.init nargs (fun _ ->
		index_opt ctx.as3_types (read_int ch)
	)) in
	let unk1 = IO.read ch in
	let unk2 = IO.read ch in
	{
		mt3_ret = tret;
		mt3_args = targs;
		mt3_unk1 = unk1;
		mt3_unk2 = unk2;
	}

let read_list ch f =	
	match read_int ch with
	| 0 -> [||]
	| n -> Array.init (n - 1) (fun _ -> f ch)

let read_list2 ch f =
	Array.init (read_int ch) (fun _ -> f ch)

let read_field ctx ch =	
	let name = index ctx.as3_types (read_int ch) in
	let is_fun = IO.read_byte ch in
	let slot = read_int ch in
	let kind = (match is_fun with
		| 0 ->
			let t = index ctx.as3_types (read_int ch) in			
			let value = (match IO.read_byte ch with
				| 0 -> None
				| _ -> assert false
			) in
			A3FVar (t,value)
		| 1 ->
			let meth = index ctx.as3_method_types (read_int ch) in
			A3FMethod meth
		| _ -> assert false
	) in
	{
		f3_name = name;
		f3_slot = slot;
		f3_kind = kind;
	}

let read_class ctx ch =
	let name = index ctx.as3_types (read_int ch) in	
	let csuper = index ctx.as3_idents (read_int ch) in	
	let flags = IO.read_byte ch in
	let rights =
		if flags land 8 <> 0 then 
			let r = index ctx.as3_base_rights (read_int ch) in			
			Some r
		else
			None
	in
	let impls = read_list2 ch (fun _ -> index ctx.as3_types (read_int ch)) in
	let slot = read_int ch in
	let fields = read_list2 ch (read_field ctx) in
	{
		cl3_name = name;
		cl3_super = csuper;
		cl3_sealed = (flags land 1) <> 0;
		cl3_final = (flags land 2) <> 0;
		cl3_interface = (flags land 4) <> 0;
		cl3_rights = rights;
		cl3_implements = impls;
		cl3_slot = slot;
		cl3_fields = fields;
	}

let read_static ctx ch =
	let slot = read_int ch in
	let fields = read_list2 ch (read_field ctx) in
	{
		st3_slot = slot;
		st3_fields = fields;
	}

let header_1 = 0x00000001
let header_2 = 0x002E0010

let parse ch len =
	let data = IO.nread ch len in
	let ch = IO.input_string data in
	if IO.read_i32 ch <> header_1 then assert false;
	let frame = IO.read_string ch in
	if IO.read_i32 ch <> header_2 then assert false;
	let ints = read_list ch read_as3_int in
	if IO.read_byte ch <> 0 then assert false;
	let floats = read_list ch IO.read_double in
	let idents = read_list ch read_ident in
	let base_rights = (if parse_base_rights then read_list ch (read_base_right idents) else [||]) in
	let rights = (if parse_rights then read_list ch (read_rights base_rights) else [||]) in
	let ctx = {
		as3_frame = frame;
		as3_ints = ints;
		as3_floats = floats;
		as3_idents = idents;
		as3_base_rights = base_rights;
		as3_rights = rights;
		as3_types = [||];
		as3_method_types = [||];
		as3_classes = [||];
		as3_statics = [||];
		as3_unknown = "";
		as3_original_data = data;
	} in
	if parse_types then ctx.as3_types <- read_list ch (read_type ctx);
	if parse_mtypes then begin
		ctx.as3_method_types <- read_list2 ch (read_method_type ctx);
		if IO.read_byte ch <> 0 then assert false;
	end;
	if parse_classes then ctx.as3_classes <- read_list2 ch (read_class ctx);
	if parse_statics then ctx.as3_statics <- Array.map (fun _ -> read_static ctx ch) ctx.as3_classes;	
	ctx.as3_unknown <- IO.read_all ch;
	if as3_length ctx <> len then assert false;
	ctx

(* ************************************************************************ *)
(* WRITING *)

let write_as3_int ch i =
	let e = Int32.to_int (Int32.shift_right_logical i 28) in
	let d = Int32.to_int (Int32.shift_right i 21) land 0x7F in
	let c = Int32.to_int (Int32.shift_right i 14) land 0x7F in
	let b = Int32.to_int (Int32.shift_right i 7) land 0x7F in
	let a = Int32.to_int (Int32.logand i 0x7Fl) in
	if b <> 0 then begin
		IO.write_byte ch (a lor 0x80);
		if c <> 0 then begin
			IO.write_byte ch (b lor 0x80);
			if d <> 0 then begin
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

let write_base_right empty_index ch = function
	| A3RPrivate ->
		IO.write_byte ch 0x05;		
		write_int ch 0;
	| A3RPublic n ->
		IO.write_byte ch 0x16;
		(match n with
		| None -> write_index ch empty_index
		| Some n -> write_index ch n);
	| A3RInternal n ->
		IO.write_byte ch 0x17;
		(match n with
		| None -> write_index ch empty_index
		| Some n -> write_index ch n);
	| A3RProtected n ->
		IO.write_byte ch 0x18;
		write_index ch n

let write_rights ch l =
	IO.write_byte ch (List.length l);
	List.iter (write_index_nz ch) l

let write_type ch = function
	| A3TClassInterface (id,r) ->
		IO.write_byte ch 0x09;
		write_index ch id;
		write_index_nz ch r;
	| A3TMethodVar (id,r) ->
		IO.write_byte ch 0x07;
		write_index ch r;
		write_index ch id

let write_method_type ch m =
	write_int ch (List.length m.mt3_args);
	write_index_opt ch m.mt3_ret;
	List.iter (write_index_opt ch) m.mt3_args;
	IO.write ch m.mt3_unk1;
	IO.write ch m.mt3_unk2

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
	match f.f3_kind with
	| A3FMethod id ->
		IO.write_byte ch 1;
		write_int ch f.f3_slot;
		write_index ch id;		
	| A3FVar (id,v) ->
		IO.write_byte ch 0;
		write_int ch f.f3_slot;
		write_index ch id;
		match v with
		| None -> IO.write_byte ch 0
		| _ -> assert false

let write_class ch c =
	write_index ch c.cl3_name;
	write_index ch c.cl3_super;
	let flags = 
		(if c.cl3_sealed then 1 else 0) lor
		(if c.cl3_final then 2 else 0) lor
		(if c.cl3_interface then 4 else 0) lor
		(if c.cl3_rights <> None then 8 else 0)
	in
	IO.write_byte ch flags;
	(match c.cl3_rights with
	| None -> ()
	| Some r -> write_index ch r);
	write_list2 ch write_index c.cl3_implements;
	write_int ch c.cl3_slot;
	write_list2 ch write_field c.cl3_fields

let write_static ch s =
	write_int ch s.st3_slot;
	write_list2 ch write_field s.st3_fields

let write ch1 ctx =
	let ch = IO.output_string() in
	let empty_index = as3_empty_index ctx in
	IO.write_i32 ch header_1;
	IO.write_string ch ctx.as3_frame;
	IO.write_i32 ch header_2;
	write_list ch write_as3_int ctx.as3_ints;
	IO.write_byte ch 0;
	write_list ch IO.write_double ctx.as3_floats;
	write_list ch write_as3_ident ctx.as3_idents;
	if parse_base_rights then write_list ch (write_base_right empty_index) ctx.as3_base_rights;
	if parse_rights then write_list ch write_rights ctx.as3_rights;
	if parse_types then write_list ch write_type ctx.as3_types;
	if parse_mtypes then begin
		write_list2 ch write_method_type ctx.as3_method_types;
		IO.write_byte ch 0;
	end;
	if parse_classes then write_list2 ch write_class ctx.as3_classes;
	if parse_statics then Array.iter (write_static ch) ctx.as3_statics;
	IO.nwrite ch ctx.as3_unknown;
	let str = IO.close_out ch in
	if str <> ctx.as3_original_data then assert false;
	IO.nwrite ch1 str

(* ************************************************************************ *)
(* DUMP *)

let ident_str ctx i =
	iget ctx.as3_idents i

let base_right_str ctx i =
	match iget ctx.as3_base_rights i with
	| A3RPrivate -> "private"
	| A3RPublic None -> "public"
	| A3RPublic (Some n) -> "public:" ^ ident_str ctx n
	| A3RInternal None -> "internal"
	| A3RInternal (Some n) -> "internal:" ^ ident_str ctx n
	| A3RProtected n -> "protected:" ^ ident_str ctx n

let rights_str ctx i =
	let l = iget ctx.as3_rights i in
	String.concat " " (List.map (fun r -> base_right_str ctx (no_nz r)) l)

let type_str ctx kind t =
	match iget ctx.as3_types t with
	| A3TClassInterface (id,r) -> Printf.sprintf "%s %s%s" (rights_str ctx (no_nz r)) kind (ident_str ctx id)
	| A3TMethodVar (id,r) -> Printf.sprintf "%s %s%s" (base_right_str ctx r) kind (ident_str ctx id)

let method_str ctx m =
	let m = iget ctx.as3_method_types m in
	let p = ref 0 in
	Printf.sprintf "(%s)%s" (String.concat ", " (List.map (fun a ->
		incr p;
		let id = "p" ^ string_of_int !p in
		(match a with None -> id | Some t -> type_str ctx (id ^ " : ") t)
	) m.mt3_args)) (match m.mt3_ret with None -> "" | Some t -> type_str ctx ": " t)

let dump_field ctx ch stat f =
	IO.printf ch "    ";
	if stat then IO.printf ch "static ";	
	match f.f3_kind with
	| A3FVar (id, v) ->		
		IO.printf ch "%s : %s" (type_str ctx "var " f.f3_name) (type_str ctx "" id);
		(match v with
		| None -> ()
		| Some v -> assert false);
		IO.printf ch ";\n"
	| A3FMethod id ->
		IO.printf ch "%s%s;\n" (type_str ctx "function " f.f3_name) (method_str ctx id)

let dump_class ctx ch idx c =
	let st = ctx.as3_statics.(idx) in
	if not c.cl3_sealed then IO.printf ch "dynamic ";
	if c.cl3_final then IO.printf ch "final ";	
	(match c.cl3_rights with
	| None -> ()
	| Some r -> IO.printf ch "%s " (base_right_str ctx r));
	let kind = (if c.cl3_interface then "interface " else "class ") in
	IO.printf ch "%s extends %s {\n" (type_str ctx kind c.cl3_name) (ident_str ctx c.cl3_super);
	Array.iter (dump_field ctx ch false) c.cl3_fields;
	Array.iter (dump_field ctx ch true) st.st3_fields;
	IO.printf ch "} [SLOT:%d] [STATIC:%d]\n" c.cl3_slot st.st3_slot

let dump ch ctx =
	IO.printf ch "--- AS3 %s ----\n" ctx.as3_frame;
	Array.iteri (dump_class ctx ch) ctx.as3_classes;	
	IO.printf ch "\n"
