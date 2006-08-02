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
open IO
open As3

let s = Printf.sprintf

let f_int_length : (int -> int) ref = ref (fun _ -> assert false)
let f_int_read : (IO.input -> int) ref = ref (fun _ -> assert false)
let f_int_write : (unit IO.output -> int -> unit) ref = ref (fun _ _ -> assert false)

let int_length i = (!f_int_length) i  
let read_int ch = (!f_int_read) ch
let write_int (ch : 'a IO.output) i = (!f_int_write) (Obj.magic ch) i

let write_signed_byte = write_byte

let ops , ops_ids = 
	let h = Hashtbl.create 0 in
	let h2 = Hashtbl.create 0 in
	List.iter (fun (o,b) -> Hashtbl.add h b o; Hashtbl.add h2 o b)
	[
		A3Incr, 0x91;
		A3Decr, 0x93;
		A3Not, 0x96;
		A3BitNot, 0x97;
		A3OAdd, 0xA0;
		A3OSub, 0xA1;
		A3OMul, 0xA2;
		A3ODiv, 0xA3;
		A3OMod, 0xA4;
		A3OShl, 0xA5;
		A3OShr, 0xA6;
		A3OUShr, 0xA7;
		A3OAnd, 0xA8;
		A3OOr, 0xA9;
		A3OXor, 0xAA;
		A3OEq, 0xAB;
		A3OPhysEq, 0xAC;
		A3OLt, 0xAD;
		A3OLte, 0xAE;
		A3OGt, 0xAF;
		A3OGte, 0xB0;
		A3Is, 0x87;
		A3As, 0xB3;
	];
	h , h2

let length = function
	| A3SmallInt _ 
	-> 2
	| A3Int f
	| A3String f
	| A3IntRef f
	| A3Float f
	| A3DebugUnk f
	| A3DebugFile f
	| A3DebugLine f ->
		1 + int_length f
	| A3Op _
	| A3Undefined
	| A3Null
	| A3True
	| A3False
	| A3RetVoid
	| A3Ret
	| A3Pop
	| A3ToObject
	| A3ToInt
	| A3Unk _ -> 1
	| A3DebugStack _ -> 5
	| A3Stack n | A3SetStack n -> if n >= 1 && n <= 3 then 1 else 2	

let opcode ch =
	let op = (try Some (read_byte ch) with IO.No_more_input -> None) in
	match op with
	| None -> None
	| Some op -> Some (
		match op with
		| 0x20 -> A3Null
		| 0x21 -> A3Undefined
		| 0x24 -> A3SmallInt (read_signed_byte ch)
		| 0x25 -> A3Int (read_int ch)
		| 0x26 -> A3True
		| 0x27 -> A3False
		| 0x29 -> A3Pop
		| 0x2C -> A3String (read_int ch)
		| 0x2D -> A3IntRef (read_int ch)
		| 0x2F -> A3Float (read_int ch)
		| 0x47 -> A3RetVoid
		| 0x48 -> A3Ret
		| 0x62 -> A3Stack (read_byte ch)
		| 0x63 -> A3SetStack (read_byte ch)
		| 0x73 -> A3ToInt
		| 0xD0 -> A3DebugUnk (read_int ch)
		| 0xD1 -> A3Stack 1
		| 0xD2 -> A3Stack 2
		| 0xD3 -> A3Stack 3
		| 0xD5 -> A3SetStack 1
		| 0xD6 -> A3SetStack 2
		| 0xD7 -> A3SetStack 3
		| 0x82 -> A3ToObject
		| 0xEF ->
			let a = read_byte ch in			
			let b = read_byte ch in
			let c = read_byte ch in
			let d = read_byte ch in
			A3DebugStack (a,b,c,d)
		| 0xF0 -> A3DebugLine (read_int ch)
		| 0xF1 -> A3DebugFile (read_int ch)
		| _ ->
			try
				A3Op (Hashtbl.find ops op)
			with Not_found ->
				A3Unk (char_of_int op)
	)

let parse ch len =
	let data = nread ch len in
	let ch = input_string data in
	let rec loop acc =
		match opcode ch with
		| None -> List.rev acc
		| Some o -> loop (o :: acc)
	in
	loop []

let write ch = function
	| A3Null ->
		write_byte ch 0x20
	| A3Undefined ->
		write_byte ch 0x21
	| A3SmallInt b ->
		write_byte ch 0x24;
		write_signed_byte ch b
	| A3Int i ->
		write_byte ch 0x25;
		write_int ch i
	| A3True ->
		write_byte ch 0x26
	| A3False ->
		write_byte ch 0x27
	| A3Pop ->
		write_byte ch 0x29
	| A3String s ->
		write_byte ch 0x2C;
		write_int ch s
	| A3IntRef i ->
		write_byte ch 0x2D;
		write_int ch i
	| A3Float f ->
		write_byte ch 0x2F;
		write_int ch f
	| A3RetVoid ->
		write_byte ch 0x47
	| A3Ret ->
		write_byte ch 0x48
	| A3Stack n ->
		(match n with
		| 1 -> write_byte ch 0xD1;
		| 2 -> write_byte ch 0xD2;
		| 3 -> write_byte ch 0xD3;
		| _ -> 
			write_byte ch 0x62;
			write_byte ch n)
	| A3SetStack n ->		
		(match n with
		| 1 -> write_byte ch 0xD5;
		| 2 -> write_byte ch 0xD6;
		| 3 -> write_byte ch 0xD7;
		| _ -> 
			write_byte ch 0x63;
			write_byte ch n)
	| A3ToInt ->
		write_byte ch 0x73
	| A3DebugUnk f ->
		write_byte ch 0xD0;
		write_int ch f;
	| A3ToObject ->
		write_byte ch 0x82
	| A3DebugStack (a,b,c,d) ->
		write_byte ch 0xEF;
		write_byte ch a;
		write_byte ch b;
		write_byte ch c;
		write_byte ch d;
	| A3DebugLine f ->
		write_byte ch 0xF0;
		write_int ch f;
	| A3DebugFile f ->
		write_byte ch 0xF1;
		write_int ch f;
	| A3Op op ->
		write_byte ch (try Hashtbl.find ops_ids op with Not_found -> assert false)
	| A3Unk x ->
		write ch x

let dump_op = function
	| A3Incr -> "incr"
	| A3Decr -> "decr"
	| A3Not -> "not"
	| A3BitNot -> "bitnot"
	| A3OAdd -> "add"
	| A3OSub -> "sub"
	| A3OMul -> "mul"
	| A3ODiv -> "div"
	| A3OMod -> "mod"
	| A3OShl -> "shl"
	| A3OShr -> "shr"
	| A3OUShr -> "ushr"
	| A3OAnd -> "and"
	| A3OOr -> "or"
	| A3OXor -> "xor"
	| A3OEq -> "eq"
	| A3OPhysEq -> "physeq"
	| A3OLt -> "lt"
	| A3OLte -> "lte"
	| A3OGt -> "gt"
	| A3OGte -> "gte"
	| A3As -> "as"
	| A3Is -> "is"

let dump = function
	| A3Null -> s "null"
	| A3Undefined -> s "undefined"
	| A3SmallInt b -> s "smallint %d" b
	| A3Int n -> s "int %d" n
	| A3True -> s "true"
	| A3False -> s "false"
	| A3Pop -> "pop"
	| A3IntRef n -> s "int ref %d" n
	| A3Float f -> s "float %d" f
	| A3String n -> s "string %d" n
	| A3RetVoid -> s "ret void"
	| A3Ret -> "ret"
	| A3Stack n -> s "get %d" n
	| A3SetStack n -> s "set %d" n
	| A3ToInt -> "toint"
	| A3DebugUnk x -> s ".unk %d" x
	| A3ToObject -> "toobj"
	| A3DebugStack (a,b,c,d) -> s ".stack %d %d %d %d" a b c d
	| A3DebugLine l -> s ".line %d" l
	| A3DebugFile f -> s ".file %d" f
	| A3Op o -> dump_op o
	| A3Unk x -> s "??? 0x%X" (int_of_char x)
