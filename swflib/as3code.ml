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

let int_index (x : 'a index) : int = Obj.magic x
let index_int (x : int) : 'a index = Obj.magic x

let read_index ch = index_int (read_int ch)
let write_index ch i = write_int ch (int_index i)

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
		A3As, 0x87;
		A3Neg, 0x90;
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
		A3Is, 0xB3;
		A3In, 0xB4;
		A3IIncr, 0xC0;
		A3IDecr, 0xC1;
	];
	h , h2

let length = function	
	| A3StackCall _
	| A3StackNew _
	| A3SmallInt _
	| A3Object _
	| A3RegReset _
	| A3SuperConstr _
	| A3GetSlot _
	| A3SetSlot _
	| A3Catch _
	| A3IncrReg _
	-> 2
	| A3Array f
	| A3Int f
	| A3ClassDef f
	| A3DebugLine f ->
		1 + int_length f
	| A3String f
	| A3DebugFile f ->
		1 + int_length (int_index f)
	| A3IntRef f ->
		1 + int_length (int_index f)
	| A3Float f ->
		1 + int_length (int_index f)
	| A3Function f ->
		1 + int_length (int_index f)	
	| A3Set f
	| A3Get f
	| A3Delete f
	| A3SetInf f
	| A3GetInf f
	| A3GetProp f
	| A3SetProp f
	| A3Cast f
	| A3GetSuper f
	| A3XmlOp1 f
	| A3SetSuper f ->
		1 + int_length (int_index f)
	| A3Op _
	| A3Undefined
	| A3Null
	| A3True
	| A3False
	| A3NaN
	| A3RetVoid
	| A3Ret
	| A3Pop
	| A3Dup
	| A3CatchDone
	| A3ToObject
	| A3ToInt
	| A3ToUInt
	| A3ToNumber
	| A3ToBool
	| A3ToString
	| A3This
	| A3Throw
	| A3Nop
	| A3Typeof
	| A3InstanceOf
	| A3Scope
	| A3ForIn
	| A3NewBlock
	| A3ForEach
	| A3PopScope
	| A3XmlOp3
	| A3XmlOp2
	| A3Unk _ -> 1
	| A3DebugReg _ -> 5
	| A3GetScope (n,b) -> if n = 0 && b then 1 else 2
	| A3Reg n | A3SetReg n -> if n >= 1 && n <= 3 then 1 else 2
	| A3SuperCall (f,_) | A3Call (f,_) | A3New (f,_) | A3CallUnknown (f,_) | A3SuperCallUnknown(f,_) -> 2 + int_length (int_index f)
	| A3Jump _ -> 4
	| A3Next _ -> 3
	| A3Switch (_,cases,_) -> 
		let ncases = List.length cases in
		1 + 3 + int_length ncases + 3 * (ncases + 1)

let jump ch kind =
	A3Jump (kind,read_i24 ch)

let opcode ch =
	let op = (try Some (read_byte ch) with IO.No_more_input -> None) in
	match op with
	| None -> None
	| Some op -> Some (
		match op with
		| 0x03 -> A3Throw
		| 0x04 -> A3GetSuper (read_index ch)
		| 0x05 -> A3SetSuper (read_index ch)
		| 0x08 -> A3RegReset (read_byte ch)
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
		| 0x15 -> jump ch J3Lt
		| 0x16 -> jump ch J3Lte
		| 0x17 -> jump ch J3Gt
		| 0x18 -> jump ch J3Gte
		| 0x19 -> jump ch J3PhysEq
		| 0x1A -> jump ch J3PhysNeq
		| 0x1B -> 
			let def = read_i24 ch in
			let rec loop n =
				if n = 0 then
					[]
				else
					let j = read_i24 ch in
					j :: loop (n - 1)
			in
			let cases = loop (read_int ch) in
			let def2 = read_i24 ch in
			A3Switch (def,cases,def2)
		| 0x1D -> A3PopScope
		| 0x1C -> A3XmlOp3
		| 0x1E -> A3ForIn
		| 0x20 -> A3Null
		| 0x21 -> A3Undefined
		| 0x23 -> A3ForEach
		| 0x24 -> A3SmallInt (read_signed_byte ch)
		| 0x25 -> A3Int (read_int ch)
		| 0x26 -> A3True
		| 0x27 -> A3False
		| 0x28 -> A3NaN
		| 0x29 -> A3Pop
		| 0x2A -> A3Dup
		| 0x2B -> A3CatchDone
		| 0x2C -> A3String (read_index ch)
		| 0x2D -> A3IntRef (read_index ch)
		| 0x2F -> A3Float (read_index ch)
		| 0x30 -> A3Scope
		| 0x32 -> 
			let r1 = read_byte ch in
			let r2 = read_byte ch in
			A3Next (r1,r2)
		| 0x40 -> A3Function (read_index ch)
		| 0x41 -> A3StackCall (read_byte ch)
		| 0x42 -> A3StackNew (read_byte ch)
		| 0x45 ->
			let id = read_index ch in
			let nargs = read_byte ch in
			A3SuperCall (id,nargs)
		| 0x46 ->
			let id = read_index ch in
			let nargs = read_byte ch in
			A3Call (id,nargs)
		| 0x47 -> A3RetVoid
		| 0x48 -> A3Ret
		| 0x49 -> A3SuperConstr (read_byte ch)
		| 0x4A ->
			let id = read_index ch in
			let nargs = read_byte ch in
			A3New (id,nargs)
		| 0x4E ->
			let id = read_index ch in
			let nargs = read_byte ch in
			A3SuperCallUnknown (id,nargs)
		| 0x4F ->
			let id = read_index ch in
			let nargs = read_byte ch in
			A3CallUnknown (id,nargs)
		| 0x55 -> A3Object (read_byte ch)
		| 0x56 -> A3Array (read_int ch)
		| 0x57 -> A3NewBlock
		| 0x58 -> A3ClassDef (read_int ch)
		| 0x59 -> A3XmlOp1 (read_index ch)
		| 0x5A -> A3Catch (read_byte ch)
		| 0x5D -> A3GetInf (read_index ch)
		| 0x5E -> A3SetInf (read_index ch)
		| 0x60 -> A3GetProp (read_index ch)
		| 0x61 -> A3SetProp (read_index ch)
		| 0x62 -> A3Reg (read_byte ch)
		| 0x63 -> A3SetReg (read_byte ch)
		| 0x64 -> A3GetScope (0,true)
		| 0x65 -> A3GetScope (read_byte ch,false)
		| 0x66 -> A3Get (read_index ch)
		| 0x68 -> A3Set (read_index ch)
		| 0x6A -> A3Delete (read_index ch)
		| 0x6C -> A3GetSlot (read_byte ch)
		| 0x6D -> A3SetSlot (read_byte ch)
		| 0x73 -> A3ToInt
		| 0x74 -> A3ToUInt
		| 0x75 -> A3ToNumber
		| 0x76 -> A3ToBool
		| 0x78 -> A3XmlOp2
		| 0x80 -> A3Cast (read_index ch)
		| 0x82 -> A3ToObject
		| 0x85 -> A3ToString
		| 0x95 -> A3Typeof
		| 0xB1 -> A3InstanceOf
		| 0xC2 -> A3IncrReg (read_byte ch)
		| 0xD0 -> A3This
		| 0xD1 -> A3Reg 1
		| 0xD2 -> A3Reg 2
		| 0xD3 -> A3Reg 3
		| 0xD5 -> A3SetReg 1
		| 0xD6 -> A3SetReg 2
		| 0xD7 -> A3SetReg 3
		| 0xEF ->
			let a = read_byte ch in
			let b = read_byte ch in
			let c = read_byte ch in
			let line = read_byte ch in
			A3DebugReg (a,b,c,line)
		| 0xF0 -> A3DebugLine (read_int ch)
		| 0xF1 -> A3DebugFile (read_index ch)
		| _ ->
			try
				A3Op (Hashtbl.find ops op)
			with Not_found ->
				Printf.printf "Unknown opcode 0x%.2X\n" op;
				A3Unk (char_of_int op)
	)

let parse ch len =
	let data = nread ch len in
	let ch = input_string data in
	let rec loop acc =
		match (try opcode ch with _ -> match acc with A3Unk '\xff' :: _ -> None | _ -> Some (A3Unk '\xff')) with
		| None -> List.rev acc
		| Some o -> loop (o :: acc)
	in
	loop []

let write ch = function
	| A3Throw ->
		write_byte ch 0x03
	| A3GetSuper f ->
		write_byte ch 0x04;
		write_index ch f
	| A3SetSuper f ->
		write_byte ch 0x05;
		write_index ch f
	| A3RegReset n ->
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
			| J3Lt -> 0x15
			| J3Lte -> 0x16
			| J3Gt -> 0x17
			| J3Gte -> 0x18
			| J3PhysEq -> 0x19
			| J3PhysNeq -> 0x1A
		);
		write_i24 ch n
	| A3Switch (def,cases,def2) ->
		write_byte ch 0x1B;
		write_i24 ch def;
		write_int ch (List.length cases);
		List.iter (write_i24 ch) cases;
		write_i24 ch def2
	| A3XmlOp3 ->
		write_byte ch 0x1C
	| A3PopScope ->
		write_byte ch 0x1D
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
	| A3NaN ->
		write_byte ch 0x28
	| A3Pop ->
		write_byte ch 0x29
	| A3Dup ->
		write_byte ch 0x2A
	| A3CatchDone ->
		write_byte ch 0x2B
	| A3String s ->
		write_byte ch 0x2C;
		write_index ch s
	| A3IntRef i ->
		write_byte ch 0x2D;
		write_index ch i
	| A3Float f ->
		write_byte ch 0x2F;
		write_index ch f
	| A3Scope ->
		write_byte ch 0x30
	| A3Next (r1,r2) ->
		write_byte ch 0x32;
		write_byte ch r1;
		write_byte ch r2
	| A3Function f ->
		write_byte ch 0x40;
		write_index ch f
	| A3StackCall n ->
		write_byte ch 0x41;
		write_byte ch n
	| A3StackNew n ->
		write_byte ch 0x42;
		write_byte ch n
	| A3SuperCall (f,n) ->
		write_byte ch 0x45;
		write_index ch f;
		write_byte ch n
	| A3Call (f,n) ->
		write_byte ch 0x46;
		write_index ch f;
		write_byte ch n
	| A3RetVoid ->
		write_byte ch 0x47
	| A3Ret ->
		write_byte ch 0x48
	| A3SuperConstr n ->
		write_byte ch 0x49;
		write_byte ch n
	| A3New (f,n) ->
		write_byte ch 0x4A;
		write_index ch f;
		write_byte ch n
	| A3SuperCallUnknown (f,n) ->
		write_byte ch 0x4E;
		write_index ch f;
		write_byte ch n
	| A3CallUnknown (f,n) ->
		write_byte ch 0x4F;
		write_index ch f;
		write_byte ch n
	| A3Object n ->
		write_byte ch 0x55;
		write_byte ch n
	| A3Array n ->
		write_byte ch 0x56;
		write_int ch n
	| A3NewBlock ->
		write_byte ch 0x57
	| A3ClassDef f ->
		write_byte ch 0x58;
		write_int ch f
	| A3XmlOp1 f ->
		write_byte ch 0x59;
		write_index ch f
	| A3Catch n ->
		write_byte ch 0x5A;
		write_byte ch n
	| A3GetInf f ->
		write_byte ch 0x5D;
		write_index ch f
	| A3SetInf f ->
		write_byte ch 0x5E;
		write_index ch f
	| A3GetProp f ->
		write_byte ch 0x60;
		write_index ch f
	| A3SetProp f ->
		write_byte ch 0x61;
		write_index ch f
	| A3Reg n ->
		(match n with
		| 1 -> write_byte ch 0xD1;
		| 2 -> write_byte ch 0xD2;
		| 3 -> write_byte ch 0xD3;
		| _ ->
			write_byte ch 0x62;
			write_byte ch n)
	| A3SetReg n ->
		(match n with
		| 1 -> write_byte ch 0xD5;
		| 2 -> write_byte ch 0xD6;
		| 3 -> write_byte ch 0xD7;
		| _ ->
			write_byte ch 0x63;
			write_byte ch n)
	| A3GetScope (0,true) ->
		write_byte ch 0x64
	| A3GetScope (n,_) ->		
		write_byte ch 0x65;
		write_byte ch n
	| A3Get f ->
		write_byte ch 0x66;
		write_index ch f
	| A3Set f ->
		write_byte ch 0x68;
		write_index ch f
	| A3Delete f ->
		write_byte ch 0x6A;
		write_index ch f
	| A3GetSlot n ->
		write_byte ch 0x6C;
		write_byte ch n
	| A3SetSlot n ->
		write_byte ch 0x6D;
		write_byte ch n
	| A3ToInt ->
		write_byte ch 0x73
	| A3ToUInt ->
		write_byte ch 0x74
	| A3ToNumber ->
		write_byte ch 0x75
	| A3ToBool ->
		write_byte ch 0x76
	| A3XmlOp2 ->
		write_byte ch 0x78
	| A3Cast f ->
		write_byte ch 0x80;
		write_index ch f
	| A3ToObject ->
		write_byte ch 0x82
	| A3ToString ->
		write_byte ch 0x85
	| A3Typeof ->
		write_byte ch 0x95
	| A3InstanceOf ->
		write_byte ch 0xB1
	| A3IncrReg r ->
		write_byte ch 0xC2;
		write_byte ch r
	| A3This ->
		write_byte ch 0xD0
	| A3DebugReg (a,b,c,line) ->
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
		write_index ch f;
	| A3Op op ->
		write_byte ch (try Hashtbl.find ops_ids op with Not_found -> assert false)
	| A3Unk x ->
		write ch x

let dump_op = function
	| A3As -> "as"
	| A3Neg -> "neg"
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
	| A3Is -> "is"
	| A3In -> "in"
	| A3IIncr -> "iincr"
	| A3IDecr -> "idecr"

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
	| J3Lt -> "-lt"
	| J3Lte -> "-lte"
	| J3Gt -> "-gt"
	| J3Gte -> "-gte"
	| J3PhysEq -> "-peq"
	| J3PhysNeq -> "-pneq"

let dump ctx op =
	let ident n = ctx.as3_idents.(int_index n - 1) in
	let field n =
		let t = ctx.as3_types.(int_index n - 1) in
		match t with
		| A3TClassInterface (Some ident,_) -> "[" ^ iget ctx.as3_idents ident ^ "]"
		| A3TMethodVar (ident,_) -> iget ctx.as3_idents ident
		| A3TArrayAccess idx -> "~array"
		| _ -> "???"
	in
	match op with
	| A3Throw -> "throw"
	| A3GetSuper f -> s "super.%s" (field f)
	| A3SetSuper f -> s "set super.%s" (field f)
	| A3RegReset n -> s "reset %d" n
	| A3Nop -> "nop"
	| A3Jump (k,n) -> s "jump%s %d" (dump_jump k) n
	| A3Switch (def,cases,def2) -> s "switch %d [%s] %d" def (String.concat "," (List.map (s "%d") cases)) def2
	| A3XmlOp3 -> "xml3"
	| A3PopScope -> "popscope"
	| A3ForIn -> "forin"
	| A3Null -> "null"
	| A3Undefined -> "undefined"
	| A3ForEach -> "foreach"
	| A3SmallInt b -> s "int %d" b
	| A3Int n -> s "int %d" n
	| A3True -> "true"
	| A3False -> "false"
	| A3NaN -> "nan"
	| A3Pop -> "pop"
	| A3Dup -> "dup"
	| A3CatchDone -> "catch-done?"
	| A3String n -> s "string [%s]" (ident n)
	| A3IntRef n -> s "int [%ld]" ctx.as3_ints.(int_index n - 1)
	| A3Float n -> s "float [%f]" ctx.as3_floats.(int_index n - 1)
	| A3Scope -> "scope"
	| A3Next (r1,r2) -> s "next %d %d" r1 r2
	| A3Function f -> s "function #%d" (int_index f)
	| A3StackCall n -> s "stackcall (%d)" n
	| A3StackNew n -> s "stacknew (%d)" n
	| A3SuperCall (f,n) -> s "supercall %s (%d)" (field f) n
	| A3Call (f,n) -> s "call %s (%d)" (field f) n
	| A3RetVoid -> "ret void"
	| A3Ret -> "ret"
	| A3SuperConstr n -> s "superconstr %d" n
	| A3New (f,n) -> s "new %s (%d)" (field f) n
	| A3SuperCallUnknown (f,n) -> s "?supercall %s (%d)" (field f) n
	| A3CallUnknown (f,n) -> s "?call %s (%d)" (field f) n
	| A3Object n -> s "obj %d" n
	| A3Array n -> s "array %d" n
	| A3NewBlock -> "newblock"
	| A3ClassDef n -> s "classdef %d" n
	| A3XmlOp1 f -> s "xml1 %s" (field f)
	| A3Catch n -> s "catch %d" n
	| A3GetInf f -> s "iget %s" (field f)
	| A3SetInf f -> s "iset %s" (field f)
	| A3GetProp f -> s "getp %s" (field f)
	| A3SetProp f -> s "setp %s" (field f)
	| A3Reg n -> s "reg %d" n
	| A3SetReg n -> s "setreg %d" n
	| A3GetScope (n,_) -> s "getscope %d" n
	| A3Get f -> s "get %s" (field f)
	| A3Set f -> s "set %s" (field f)
	| A3Delete f -> s "delete %s" (field f)
	| A3GetSlot n -> s "getslot %d" n
	| A3SetSlot n -> s "setslot %d" n
	| A3ToInt -> "to_int"
	| A3ToUInt -> "to_uint"
	| A3ToNumber -> "to_number"
	| A3ToBool -> "to_bool"
	| A3XmlOp2 -> "xml2"
	| A3Cast f -> s "cast %s" (field f)
	| A3ToObject -> "to_obj"
	| A3ToString -> "to_str"
	| A3Typeof -> "typeof"
	| A3InstanceOf -> "instanceof"
	| A3IncrReg r -> s "incr-reg %d" r
	| A3This -> "this"
	| A3DebugReg (a,b,c,line) -> s ".reg %d %d %d line:%d" a b c line
	| A3DebugLine l -> s ".line %d" l
	| A3DebugFile f -> s ".file %s" (ident f)
	| A3Op o -> dump_op o
	| A3Unk x -> s "??? 0x%X" (int_of_char x)
