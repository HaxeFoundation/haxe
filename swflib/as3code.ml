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

let iget (t : 'a array) (i : 'a index) : 'a =
	t.(Obj.magic i - 1)

let write_signed_byte = write_byte

let max_i24 = 1 lsl 23 - 1

let read_i24 ch =
	let a = read_byte ch in
	let b = read_byte ch in
	let c = read_byte ch in
	let n = a lor (b lsl 8) lor (c lsl 16) in
	if c land 128 <> 0 then
		n - (1 lsl 24)
	else
		n

let rec write_i24 ch n =
	if n < -max_i24 || n > max_i24 then assert false;
	let n = (if n land (1 lsl 23) <> 0 then n + (1 lsl 24) else n) in
	write_byte ch n;
	write_byte ch (n lsr 8);
	write_byte ch (n lsr 16)

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
	| A3Object _
	| A3StackReset _
	-> 2
	| A3Array f
	| A3Int f
	| A3String f
	| A3IntRef f
	| A3Float f
	| A3DebugFile f
	| A3Set f
	| A3Get f
	| A3Delete f
	| A3SetInf f
	| A3GetInf f
	| A3SetProp f
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
	| A3Dup
	| A3ToObject
	| A3ToInt
	| A3ToUInt
	| A3ToNumber
	| A3ToBool
	| A3This
	| A3Throw
	| A3Nop
	| A3Typeof
	| A3InstanceOf
	| A3Context
	| A3ForIn
	| A3ForEach
	| A3Unk _ -> 1
	| A3DebugStack _ -> 5
	| A3Stack n | A3SetStack n -> if n >= 1 && n <= 3 then 1 else 2
	| A3SuperCall (f,_) | A3Call (f,_) | A3New (f,_) -> 2 + int_length f
	| A3Jump _ -> 4
	| A3Next _ -> 3

let jump ch kind =
	A3Jump (kind,read_i24 ch)

let opcode ch =
	let op = (try Some (read_byte ch) with IO.No_more_input -> None) in
	match op with
	| None -> None
	| Some op -> Some (
		match op with
		| 0x03 -> A3Throw
		| 0x08 -> A3StackReset (read_byte ch)
		| 0x09 -> A3Nop
		| 0x0C -> jump ch J3NotLt
		| 0x0D -> jump ch J3NotLte
		| 0x0E -> jump ch J3NotGt
		| 0x0F -> jump ch J3NotGte
		| 0x10 -> jump ch J3Always
		| 0x11 -> jump ch J3True
		| 0x12 -> jump ch J3False
		| 0x13 -> jump ch J3Eq
		| 0x14 -> jump ch J3Neq
		| 0x19 -> jump ch J3PhysEq
		| 0x1A -> jump ch J3PhysNeq
		| 0x1E -> A3ForIn
		| 0x20 -> A3Null
		| 0x21 -> A3Undefined
		| 0x23 -> A3ForEach
		| 0x24 -> A3SmallInt (read_signed_byte ch)
		| 0x25 -> A3Int (read_int ch)
		| 0x26 -> A3True
		| 0x27 -> A3False
		| 0x29 -> A3Pop
		| 0x2A -> A3Dup
		| 0x2C -> A3String (read_int ch)
		| 0x2D -> A3IntRef (read_int ch)
		| 0x2F -> A3Float (read_int ch)
		| 0x30 -> A3Context
		| 0x32 -> 
			let st1 = read_byte ch in
			let st2 = read_byte ch in
			A3Next (st1,st2)
		| 0x45 ->
			let id = read_int ch in
			let nargs = read_byte ch in
			A3SuperCall (id,nargs)
		| 0x46 ->
			let id = read_int ch in
			let nargs = read_byte ch in
			A3Call (id,nargs)
		| 0x47 -> A3RetVoid
		| 0x48 -> A3Ret
		| 0x4A ->
			let id = read_int ch in
			let nargs = read_byte ch in
			A3New (id,nargs)
		| 0x55 -> A3Object (read_byte ch)
		| 0x56 -> A3Array (read_int ch)
		| 0x5D -> A3GetInf (read_int ch)
		| 0x5E -> A3SetInf (read_int ch)
		| 0x61 -> A3SetProp (read_int ch)
		| 0x62 -> A3Stack (read_byte ch)
		| 0x63 -> A3SetStack (read_byte ch)
		| 0x66 -> A3Get (read_int ch)
		| 0x68 -> A3Set (read_int ch)
		| 0x6A -> A3Delete (read_int ch)
		| 0x73 -> A3ToInt
		| 0x74 -> A3ToUInt
		| 0x75 -> A3ToNumber
		| 0x76 -> A3ToBool
		| 0x82 -> A3ToObject
		| 0x95 -> A3Typeof
		| 0xB1 -> A3InstanceOf
		| 0xD0 -> A3This
		| 0xD1 -> A3Stack 1
		| 0xD2 -> A3Stack 2
		| 0xD3 -> A3Stack 3
		| 0xD5 -> A3SetStack 1
		| 0xD6 -> A3SetStack 2
		| 0xD7 -> A3SetStack 3
		| 0xEF ->
			let a = read_byte ch in
			let b = read_byte ch in
			let c = read_byte ch in
			let line = read_byte ch in
			A3DebugStack (a,b,c,line)
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
	| A3Throw ->
		write_byte ch 0x03
	| A3StackReset n ->
		write_byte ch 0x08;
		write_byte ch n
	| A3Nop ->
		write_byte ch 0x09
	| A3Jump (k,n) ->
		write_byte ch (match k with
			| J3NotLt -> 0x0C
			| J3NotLte -> 0x0D
			| J3NotGt -> 0x0E
			| J3NotGte -> 0x0F
			| J3Always -> 0x10
			| J3True -> 0x11
			| J3False -> 0x12
			| J3Eq -> 0x13
			| J3Neq -> 0x14
			| J3PhysEq -> 0x19
			| J3PhysNeq -> 0x1A
		);
		write_i24 ch n
	| A3ForIn ->
		write_byte ch 0x1E
	| A3Null ->
		write_byte ch 0x20
	| A3Undefined ->
		write_byte ch 0x21
	| A3ForEach ->
		write_byte ch 0x23
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
	| A3Dup ->
		write_byte ch 0x2A
	| A3String s ->
		write_byte ch 0x2C;
		write_int ch s
	| A3IntRef i ->
		write_byte ch 0x2D;
		write_int ch i
	| A3Float f ->
		write_byte ch 0x2F;
		write_int ch f
	| A3Context ->
		write_byte ch 0x30
	| A3Next (st1,st2) ->
		write_byte ch 0x32;
		write_byte ch st1;
		write_byte ch st2
	| A3SuperCall (f,n) ->
		write_byte ch 0x45;
		write_int ch f;
		write_byte ch n
	| A3Call (f,n) ->
		write_byte ch 0x46;
		write_int ch f;
		write_byte ch n
	| A3RetVoid ->
		write_byte ch 0x47
	| A3Ret ->
		write_byte ch 0x48
	| A3New (f,n) ->
		write_byte ch 0x4A;
		write_int ch f;
		write_byte ch n
	| A3Object n ->
		write_byte ch 0x55;
		write_byte ch n
	| A3Array n ->
		write_byte ch 0x56;
		write_byte ch n
	| A3GetInf f ->
		write_byte ch 0x5D;
		write_int ch f
	| A3SetInf f ->
		write_byte ch 0x5E;
		write_int ch f
	| A3SetProp f ->
		write_byte ch 0x61;
		write_int ch f
	| A3Delete f ->
		write_byte ch 0x6A;
		write_int ch f
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
	| A3Get f ->
		write_byte ch 0x66;
		write_int ch f
	| A3Set f ->
		write_byte ch 0x68;
		write_int ch f
	| A3ToInt ->
		write_byte ch 0x73
	| A3ToUInt ->
		write_byte ch 0x74
	| A3ToNumber ->
		write_byte ch 0x75
	| A3ToBool ->
		write_byte ch 0x76
	| A3ToObject ->
		write_byte ch 0x82
	| A3Typeof ->
		write_byte ch 0x95
	| A3InstanceOf ->
		write_byte ch 0xB1
	| A3This ->
		write_byte ch 0xD0
	| A3DebugStack (a,b,c,line) ->
		write_byte ch 0xEF;
		write_byte ch a;
		write_byte ch b;
		write_byte ch c;
		write_byte ch line;
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


let dump_jump = function
	| J3NotLt -> "-nlt"
	| J3NotLte -> "-nlte"
	| J3NotGt -> "-ngt"
	| J3NotGte -> "-ngte"
	| J3Always -> ""
	| J3True -> "-if"
	| J3False -> "-ifnot"
	| J3Eq -> "-eq"
	| J3Neq -> "-neq"
	| J3PhysEq -> "-peq"
	| J3PhysNeq -> "-pneq"

let dump ctx op =
	let ident n = ctx.as3_idents.(n - 1) in
	let field n =
		let t = ctx.as3_types.(n - 1) in
		match t with
		| A3TClassInterface (ident,_) -> "dynamic:" ^ iget ctx.as3_idents ident
		| A3TMethodVar (ident,_) -> iget ctx.as3_idents ident
		| _ -> "???"
	in
	match op with
	| A3Throw -> "throw"
	| A3StackReset n -> s "reset %d" n
	| A3Nop -> "nop"
	| A3Jump (k,n) -> s "jump%s %d" (dump_jump k) n
	| A3ForIn -> "forin"
	| A3Null -> "null"
	| A3Undefined -> "undefined"
	| A3ForEach -> "foreach"
	| A3SmallInt b -> s "int %d" b
	| A3Int n -> s "int %d" n
	| A3True -> "true"
	| A3False -> "false"
	| A3Pop -> "pop"
	| A3Dup -> "dup"
	| A3IntRef n -> s "int [%ld]" ctx.as3_ints.(n - 1)
	| A3Float n -> s "float [%f]" ctx.as3_floats.(n - 1)
	| A3Context -> "ctx"
	| A3Next (st1,st2) -> s "next %d %d" st1 st2
	| A3String n -> s "string [%s]" (ident n)
	| A3SuperCall (f,n) -> s "supercall %s (%d)" (field f) n
	| A3Call (f,n) -> s "call %s (%d)" (field f) n
	| A3RetVoid -> "ret void"
	| A3Ret -> "ret"
	| A3New (f,n) -> s "new %s (%d)" (field f) n
	| A3Object n -> s "obj %d" n
	| A3Array n -> s "array %d" n
	| A3GetInf f -> s "iget %s" (field f)
	| A3SetInf f -> s "iset %s" (field f)
	| A3SetProp f -> s "setp %s" (field f)
	| A3Stack n -> s "stack %d" n
	| A3SetStack n -> s "setstack %d" n
	| A3Get f -> s "get %s" (field f)
	| A3Set f -> s "set %s" (field f)
	| A3Delete f -> s "delete %s" (field f)
	| A3ToInt -> "to_int"
	| A3ToUInt -> "to_uint"
	| A3ToNumber -> "to_number"
	| A3ToBool -> "to_bool"
	| A3ToObject -> "to_obj"
	| A3Typeof -> "typeof"
	| A3InstanceOf -> "instanceof"
	| A3This -> "this"
	| A3DebugStack (a,b,c,line) -> s ".stack %d %d %d line:%d" a b c line
	| A3DebugLine l -> s ".line %d" l
	| A3DebugFile f -> s ".file %s" (ident f)
	| A3Op o -> dump_op o
	| A3Unk x -> s "??? 0x%X" (int_of_char x)
