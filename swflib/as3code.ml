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
let int_index_nz (x : 'a index_nz) : int = Obj.magic x
let index_nz_int (x : int) : 'a index_nz = Obj.magic x

let read_index ch = index_int (read_int ch)
let write_index ch i = write_int ch (int_index i)

let read_index_nz ch = index_nz_int (read_int ch)
let write_index_nz ch i = write_int ch (int_index_nz i)

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
		A3OAs, 0x87;
		A3ONeg, 0x90;
		A3OIncr, 0x91;
		(* 0x92 : REGINCR *)
		A3ODecr, 0x93;
		(* 0x94 : REGDECR *)
		(* 0x95 : TYPEOF *)
		A3ONot, 0x96;
		A3OBitNot, 0x97;
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
		A3OIs, 0xB3;
		A3OIn, 0xB4;
		A3OIIncr, 0xC0;
		A3OIDecr, 0xC1;
		A3OINeg, 0xC4;
		A3OIAdd, 0xC5;
		A3OISub, 0xC6;
		A3OIMul, 0xC7;
		A3OMemGet8, 0x35;
		A3OMemGet16, 0x36;
		A3OMemGet32, 0x37;
		A3OMemGetFloat, 0x38;
		A3OMemGetDouble, 0x39;
		A3OMemSet8, 0x3A;
		A3OMemSet16, 0x3B;
		A3OMemSet32, 0x3C;
		A3OMemSetFloat, 0x3D;
		A3OMemSetDouble, 0x3E;
		A3OSign1, 0x50;
		A3OSign8, 0x51;
		A3OSign16, 0x52;
	];
	h , h2

let length = function
	| A3SmallInt _ -> 2
	| A3Construct n
	| A3Object n
	| A3RegKill n
	| A3Catch n
	| A3IncrReg n
	| A3DecrReg n
	| A3IncrIReg n
	| A3DecrIReg n
	| A3Array n
	| A3Int n
	| A3CallStack n
	| A3ConstructSuper n
	| A3BreakPointLine n
	| A3ApplyType n
	| A3DebugLine n ->
		1 + int_length n
	| A3GetSlot s
	| A3SetSlot s ->
		1 + int_length s
	| A3ClassDef n ->
		1 + int_length (int_index_nz n)
	| A3DxNs f
	| A3String f
	| A3DebugFile f ->
		1 + int_length (int_index f)
	| A3IntRef f ->
		1 + int_length (int_index f)
	| A3UIntRef f ->
		1 + int_length (int_index f)
	| A3Float f ->
		1 + int_length (int_index f)
	| A3Function f ->
		1 + int_length (int_index_nz f)
	| A3Namespace f ->
		1 + int_length (int_index f)
	| A3GetProp f
	| A3InitProp f
	| A3DeleteProp f
	| A3FindPropStrict f
	| A3FindProp f
	| A3FindDefinition f
	| A3GetLex f
	| A3SetProp f
	| A3Cast f
	| A3GetSuper f
	| A3GetDescendants f
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
	| A3Swap
	| A3AsAny
	| A3ToString
	| A3ToXml
	| A3ToXmlAttr
	| A3ToInt
	| A3ToUInt
	| A3ToNumber
	| A3ToBool
	| A3ToObject
	| A3AsString
	| A3AsObject
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
	| A3CheckIsXml
	| A3Label
	| A3BreakPoint
	| A3PushWith
	| A3HasNext
	| A3SetThis
	| A3Timestamp
	| A3DxNsLate
	| A3Unk _ -> 1
	| A3AsType n | A3IsType n ->
		1 + int_length (int_index n)
	| A3DebugReg (name,reg,line) -> 1 + 1 + int_length (int_index name) + int_length (reg - 1) + int_length line
	| A3GetGlobalScope -> 1
	| A3GetScope n -> 1 + int_length n
	| A3Reg n | A3SetReg n -> if n >= 1 && n <= 3 then 1 else (1 + int_length n)
	| A3CallSuper (f,n) | A3CallProperty (f,n) | A3ConstructProperty (f,n) | A3CallPropLex (f,n) | A3CallPropVoid (f,n) | A3CallSuperVoid (f,n) ->
		1 + int_length n + int_length (int_index f)
	| A3CallMethod (f,n) ->
		1 + int_length n + int_length f
	| A3CallStatic (f,n) ->
		1 + int_length n + int_length (int_index f)
	| A3Jump _ -> 4
	| A3Next (a,b) -> 1 + int_length a + int_length b
	| A3Switch (_,cases) ->
		let ncases = List.length cases in
		1 + 3 + int_length (ncases - 1) + 3 * ncases

let jump ch kind =
	A3Jump (kind,read_i24 ch)

let opcode ch =
	let op = (try read_byte ch with IO.No_more_input -> raise Exit) in
	match op with
	| 0x01 -> A3BreakPoint
	| 0x02 -> A3Nop
	| 0x03 -> A3Throw
	| 0x04 -> A3GetSuper (read_index ch)
	| 0x05 -> A3SetSuper (read_index ch)
	| 0x06 -> A3DxNs (read_index ch)
	| 0x07 -> A3DxNsLate
	| 0x08 -> A3RegKill (read_int ch)
	| 0x09 -> A3Label
	(* 0x0A -> NONE *)
	(* 0x0B -> NONE *)
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
		let cases = loop (read_int ch + 1) in
		A3Switch (def,cases)
	| 0x1C -> A3PushWith
	| 0x1D -> A3PopScope
	| 0x1E -> A3ForIn
	| 0x1F -> A3HasNext
	| 0x20 -> A3Null
	| 0x21 -> A3Undefined
	(* 0x22 -> NONE *)
	| 0x23 -> A3ForEach
	| 0x24 -> A3SmallInt (read_signed_byte ch)
	| 0x25 -> A3Int (read_int ch)
	| 0x26 -> A3True
	| 0x27 -> A3False
	| 0x28 -> A3NaN
	| 0x29 -> A3Pop
	| 0x2A -> A3Dup
	| 0x2B -> A3Swap
	| 0x2C -> A3String (read_index ch)
	| 0x2D -> A3IntRef (read_index ch)
	| 0x2E -> A3UIntRef (read_index ch)
	| 0x2F -> A3Float (read_index ch)
	| 0x30 -> A3Scope
	| 0x31 -> A3Namespace (read_index ch)
	| 0x32 ->
		let r1 = read_int ch in
		let r2 = read_int ch in
		A3Next (r1,r2)
	(* 0x33 - 0x3F -> NONE *)
	| 0x40 -> A3Function (read_index_nz ch)
	| 0x41 -> A3CallStack (read_int ch)
	| 0x42 -> A3Construct (read_int ch)
	| 0x43 ->
		let id = read_int ch in
		let nargs = read_int ch in
		A3CallMethod (id,nargs)
	| 0x44 ->
		let id = read_index ch in
		let nargs = read_int ch in
		A3CallStatic (id,nargs)
	| 0x45 ->
		let id = read_index ch in
		let nargs = read_int ch in
		A3CallSuper (id,nargs)
	| 0x46 ->
		let id = read_index ch in
		let nargs = read_int ch in
		A3CallProperty (id,nargs)
	| 0x47 -> A3RetVoid
	| 0x48 -> A3Ret
	| 0x49 -> A3ConstructSuper (read_int ch)
	| 0x4A ->
		let id = read_index ch in
		let nargs = read_int ch in
		A3ConstructProperty (id,nargs)
	(* 0x4B -> NONE *)
	| 0x4C ->
		let id = read_index ch in
		let nargs = read_int ch in
		A3CallPropLex (id,nargs)
	(* 0x4D -> NONE *)
	| 0x4E ->
		let id = read_index ch in
		let nargs = read_int ch in
		A3CallSuperVoid (id,nargs)
	| 0x4F ->
		let id = read_index ch in
		let nargs = read_int ch in
		A3CallPropVoid (id,nargs)
	(* 0x50 - 0x52 -> NONE *)
	| 0x53 -> A3ApplyType (read_int ch)
	(* 0x54 -> NONE *)
	| 0x55 -> A3Object (read_int ch)
	| 0x56 -> A3Array (read_int ch)
	| 0x57 -> A3NewBlock
	| 0x58 -> A3ClassDef (read_index_nz ch)
	| 0x59 -> A3GetDescendants (read_index ch)
	| 0x5A -> A3Catch (read_int ch)
	(* 0x5B -> NONE *)
	(* 0x5C -> NONE *)
	| 0x5D -> A3FindPropStrict (read_index ch)
	| 0x5E -> A3FindProp (read_index ch)
	| 0x5F -> A3FindDefinition (read_index ch)
	| 0x60 -> A3GetLex (read_index ch)
	| 0x61 -> A3SetProp (read_index ch)
	| 0x62 -> A3Reg (read_int ch)
	| 0x63 -> A3SetReg (read_int ch)
	| 0x64 -> A3GetGlobalScope
	| 0x65 -> A3GetScope (IO.read_byte ch)
	| 0x66 -> A3GetProp (read_index ch)
	(* 0x67 -> NONE *)
	| 0x68 -> A3InitProp (read_index ch)
	(* 0x69 -> NONE *)
	| 0x6A -> A3DeleteProp (read_index ch)
	(* 0x6B -> NONE *)
	| 0x6C -> A3GetSlot (read_int ch)
	| 0x6D -> A3SetSlot (read_int ch)
	(* 0x6E -> DEPRECATED getglobalslot *)
	(* 0x6F -> DEPRECATED setglobalslot *)
	| 0x70 -> A3ToString
	| 0x71 -> A3ToXml
	| 0x72 -> A3ToXmlAttr
	| 0x73 -> A3ToInt
	| 0x74 -> A3ToUInt
	| 0x75 -> A3ToNumber
	| 0x76 -> A3ToBool
	| 0x77 -> A3ToObject
	| 0x78 -> A3CheckIsXml
	(* 0x79 -> NONE *)
	| 0x80 -> A3Cast (read_index ch)
	(* 0x81 -> DEPRECATED asbool *)
	| 0x82 -> A3AsAny
	(* 0x83 -> DEPRECATED asint *)
	(* 0x84 -> DEPRECATED asnumber *)
	| 0x85 -> A3AsString
	| 0x86 -> A3AsType (read_index ch)
	(* 0x87 -> OP *)
	(* 0x88 -> DEPRECATED asuint *)
	| 0x89 -> A3AsObject
	(* 0x8A - 0x8F -> NONE *)
	(* 0x90 - 0x91 -> OP *)
	| 0x92 -> A3IncrReg (read_int ch)
	(* 0x93 -> OP *)
	| 0x94 -> A3DecrReg (read_int ch)
	| 0x95 -> A3Typeof
	(* 0x96 -> OP *)
	(* 0x97 -> OP *)
	(* 0x98 - 0x9F -> NONE *)
	(* 0xA0 - 0xB0 -> OP *)
	| 0xB1 -> A3InstanceOf
	| 0xB2 -> A3IsType (read_index ch)
	(* 0xB3 -> OP *)
	(* 0xB4 -> OP *)
	(* 0xB5 - 0xBF -> NONE *)
	(* 0xC0 -> OP *)
	(* 0xC1 -> OP *)
	| 0xC2 -> A3IncrIReg (read_int ch)
	| 0xC3 -> A3DecrIReg (read_int ch)
	(* 0xC4 - 0xC7 -> OP *)
	(* 0xC8 - 0xCF -> NONE *)
	| 0xD0 -> A3This
	| 0xD1 -> A3Reg 1
	| 0xD2 -> A3Reg 2
	| 0xD3 -> A3Reg 3
	| 0xD4 -> A3SetThis
	| 0xD5 -> A3SetReg 1
	| 0xD6 -> A3SetReg 2
	| 0xD7 -> A3SetReg 3
	(* 0xD8 - 0xEE -> NONE *)
	| 0xEF ->
		if IO.read_byte ch <> 1 then assert false;
		let name = read_index ch in
		let reg = read_int ch + 1 in
		let line = read_int ch in
		A3DebugReg (name,reg,line)
	| 0xF0 -> A3DebugLine (read_int ch)
	| 0xF1 -> A3DebugFile (read_index ch)
	| 0xF2 -> A3BreakPointLine (read_int ch)
	| 0xF3 -> A3Timestamp
	(* 0xF4 - 0xFF -> NONE *)
	| _ ->
		try
			A3Op (Hashtbl.find ops op)
		with Not_found ->
			Printf.printf "Unknown opcode 0x%.2X\n" op;
			A3Unk (char_of_int op)

let parse ch len =
	let data = nread ch len in
	let ch = input_string data in
	let a = DynArray.create() in
	let rec loop() =
		DynArray.add a (opcode ch);
		loop();
	in
	(try loop() with Exit -> ());
	DynArray.to_array a

let write ch = function
	| A3BreakPoint ->
		write_byte ch 0x01
	| A3Nop ->
		write_byte ch 0x02
	| A3Throw ->
		write_byte ch 0x03
	| A3GetSuper f ->
		write_byte ch 0x04;
		write_index ch f
	| A3SetSuper f ->
		write_byte ch 0x05;
		write_index ch f
	| A3DxNs i ->
		write_byte ch 0x06;
		write_index ch i
	| A3DxNsLate ->
		write_byte ch 0x07
	| A3RegKill n ->
		write_byte ch 0x08;
		write_int ch n
	| A3Label ->
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
	| A3Switch (def,cases) ->
		write_byte ch 0x1B;
		write_i24 ch def;
		write_int ch (List.length cases - 1);
		List.iter (write_i24 ch) cases
	| A3PushWith ->
		write_byte ch 0x1C
	| A3PopScope ->
		write_byte ch 0x1D
	| A3ForIn ->
		write_byte ch 0x1E
	| A3HasNext ->
		write_byte ch 0x1F
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
	| A3Swap ->
		write_byte ch 0x2B
	| A3String s ->
		write_byte ch 0x2C;
		write_index ch s
	| A3IntRef i ->
		write_byte ch 0x2D;
		write_index ch i
	| A3UIntRef i ->
		write_byte ch 0x2E;
		write_index ch i
	| A3Float f ->
		write_byte ch 0x2F;
		write_index ch f
	| A3Scope ->
		write_byte ch 0x30
	| A3Namespace f ->
		write_byte ch 0x31;
		write_index ch f
	| A3Next (r1,r2) ->
		write_byte ch 0x32;
		write_int ch r1;
		write_int ch r2
	| A3Function f ->
		write_byte ch 0x40;
		write_index_nz ch f
	| A3CallStack n ->
		write_byte ch 0x41;
		write_int ch n
	| A3Construct n ->
		write_byte ch 0x42;
		write_int ch n
	| A3CallMethod (f,n) ->
		write_byte ch 0x43;
		write_int ch f;
		write_int ch n
	| A3CallStatic (f,n) ->
		write_byte ch 0x44;
		write_index ch f;
		write_int ch n
	| A3CallSuper (f,n) ->
		write_byte ch 0x45;
		write_index ch f;
		write_int ch n
	| A3CallProperty (f,n) ->
		write_byte ch 0x46;
		write_index ch f;
		write_int ch n
	| A3RetVoid ->
		write_byte ch 0x47
	| A3Ret ->
		write_byte ch 0x48
	| A3ConstructSuper n ->
		write_byte ch 0x49;
		write_int ch n
	| A3ConstructProperty (f,n) ->
		write_byte ch 0x4A;
		write_index ch f;
		write_int ch n
	| A3CallPropLex (f,n) ->
		write_byte ch 0x4C;
		write_index ch f;
		write_int ch n
	| A3CallSuperVoid (f,n) ->
		write_byte ch 0x4E;
		write_index ch f;
		write_int ch n
	| A3CallPropVoid (f,n) ->
		write_byte ch 0x4F;
		write_index ch f;
		write_int ch n
	| A3ApplyType n ->
		write_byte ch 0x53;
		write_int ch n
	| A3Object n ->
		write_byte ch 0x55;
		write_int ch n
	| A3Array n ->
		write_byte ch 0x56;
		write_int ch n
	| A3NewBlock ->
		write_byte ch 0x57
	| A3ClassDef f ->
		write_byte ch 0x58;
		write_index_nz ch f
	| A3GetDescendants f ->
		write_byte ch 0x59;
		write_index ch f
	| A3Catch n ->
		write_byte ch 0x5A;
		write_int ch n
	| A3FindPropStrict f ->
		write_byte ch 0x5D;
		write_index ch f
	| A3FindProp f ->
		write_byte ch 0x5E;
		write_index ch f
	| A3FindDefinition f ->
		write_byte ch 0x5F;
		write_index ch f
	| A3GetLex f ->
		write_byte ch 0x60;
		write_index ch f
	| A3SetProp f ->
		write_byte ch 0x61;
		write_index ch f
	| A3Reg n ->
		if n >= 0 && n < 4 then
			write_byte ch (0xD0 + n)
		else begin
			write_byte ch 0x62;
			write_int ch n
		end
	| A3SetReg n ->
		if n >= 0 && n < 4 then
			write_byte ch (0xD4 + n)
		else begin
			write_byte ch 0x63;
			write_int ch n
		end
	| A3GetGlobalScope ->
		write_byte ch 0x64
	| A3GetScope n ->
		write_byte ch 0x65;
		write_byte ch n
	| A3GetProp f ->
		write_byte ch 0x66;
		write_index ch f
	| A3InitProp f ->
		write_byte ch 0x68;
		write_index ch f
	| A3DeleteProp f ->
		write_byte ch 0x6A;
		write_index ch f
	| A3GetSlot n ->
		write_byte ch 0x6C;
		write_int ch n
	| A3SetSlot n ->
		write_byte ch 0x6D;
		write_int ch n
	| A3ToString ->
		write_byte ch 0x70
	| A3ToXml ->
		write_byte ch 0x71
	| A3ToXmlAttr ->
		write_byte ch 0x72
	| A3ToInt ->
		write_byte ch 0x73
	| A3ToUInt ->
		write_byte ch 0x74
	| A3ToNumber ->
		write_byte ch 0x75
	| A3ToBool ->
		write_byte ch 0x76
	| A3ToObject ->
		write_byte ch 0x77
	| A3CheckIsXml ->
		write_byte ch 0x78
	| A3Cast f ->
		write_byte ch 0x80;
		write_index ch f
	| A3AsAny ->
		write_byte ch 0x82
	| A3AsString ->
		write_byte ch 0x85
	| A3AsType n ->
		write_byte ch 0x86;
		write_index ch n
	| A3AsObject ->
		write_byte ch 0x89
	| A3IncrReg r ->
		write_byte ch 0x92;
		write_int ch r
	| A3DecrReg r ->
		write_byte ch 0x94;
		write_int ch r
	| A3Typeof ->
		write_byte ch 0x95
	| A3InstanceOf ->
		write_byte ch 0xB1
	| A3IsType n ->
		write_byte ch 0xB2;
		write_index ch n
	| A3IncrIReg r ->
		write_byte ch 0xC2;
		write_int ch r
	| A3DecrIReg r ->
		write_byte ch 0xC3;
		write_int ch r
	| A3This ->
		write_byte ch 0xD0
	| A3SetThis ->
		write_byte ch 0xD4
	| A3DebugReg (name,reg,line) ->
		write_byte ch 0xEF;
		write_byte ch 0x01;
		write_index ch name;
		write_int ch (reg - 1);
		write_int ch line;
	| A3DebugLine f ->
		write_byte ch 0xF0;
		write_int ch f;
	| A3DebugFile f ->
		write_byte ch 0xF1;
		write_index ch f;
	| A3BreakPointLine l ->
		write_byte ch 0xF2;
		write_int ch l
	| A3Timestamp ->
		write_byte ch 0xF3
	| A3Op op ->
		write_byte ch (try Hashtbl.find ops_ids op with Not_found -> assert false)
	| A3Unk x ->
		write ch x

let dump_op = function
	| A3OAs -> "as"
	| A3ONeg -> "neg"
	| A3OIncr -> "incr"
	| A3ODecr -> "decr"
	| A3ONot -> "not"
	| A3OBitNot -> "bitnot"
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
	| A3OIs -> "is"
	| A3OIn -> "in"
	| A3OIIncr -> "iincr"
	| A3OIDecr -> "idecr"
	| A3OINeg -> "ineg"
	| A3OIAdd -> "iadd"
	| A3OISub -> "isub"
	| A3OIMul -> "imul"
	| A3OMemSet8 -> "mset8"
	| A3OMemSet16 -> "set16"
	| A3OMemSet32 -> "mset32"
	| A3OMemSetFloat -> "msetfloat"
	| A3OMemSetDouble -> "msetdouble"
	| A3OMemGet8 -> "mget8"
	| A3OMemGet16 -> "mget16"
	| A3OMemGet32 -> "mget32"
	| A3OMemGetFloat -> "mgetfloat"
	| A3OMemGetDouble -> "mgetdouble"
	| A3OSign1 -> "sign1"
	| A3OSign8 -> "sign8"
	| A3OSign16 -> "sign16"

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
	let rec field n =
		let t = ctx.as3_names.(int_index n - 1) in
		match t with
		| A3MMultiName (Some ident,_) -> "[" ^ iget ctx.as3_idents ident ^ "]"
		| A3MName (ident,_) -> iget ctx.as3_idents ident
		| A3MMultiNameLate idx -> "~array"
		| A3MParams (t,params) -> field t ^ "<" ^ String.concat "." (List.map field params) ^ ">"
		| _ -> "???"
	in
	match op with
	| A3BreakPoint -> "bkpt"
	| A3Nop -> "nop"
	| A3Throw -> "throw"
	| A3GetSuper f -> s "getsuper %s" (field f)
	| A3SetSuper f -> s "setsuper %s" (field f)
	| A3DxNs i -> s "dxns %s" (ident i)
	| A3DxNsLate -> "dxnslate"
	| A3RegKill n -> s "kill %d" n
	| A3Label -> "label"
	| A3Jump (k,n) -> s "jump%s %d" (dump_jump k) n
	| A3Switch (def,cases) -> s "switch %d [%s]" def (String.concat "," (List.map (s "%d") cases))
	| A3PushWith -> "pushwith"
	| A3PopScope -> "popscope"
	| A3ForIn -> "forin"
	| A3HasNext -> "hasnext"
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
	| A3Swap -> "swap"
	| A3String n -> s "string [%s]" (ident n)
	| A3IntRef n -> s "int [%ld]" ctx.as3_ints.(int_index n - 1)
	| A3UIntRef n -> s "uint [%ld]" ctx.as3_uints.(int_index n - 1)
	| A3Float n -> s "float [%f]" ctx.as3_floats.(int_index n - 1)
	| A3Scope -> "scope"
	| A3Namespace f -> s "namespace [%d]" (int_index f)
	| A3Next (r1,r2) -> s "next %d %d" r1 r2
	| A3Function f -> s "function #%d" (int_index_nz f)
	| A3CallStack n -> s "callstack (%d)" n
	| A3Construct n -> s "construct (%d)" n
	| A3CallMethod (f,n) -> s "callmethod %d (%d)" f n
	| A3CallStatic (f,n) -> s "callstatic %d (%d)" (int_index f) n
	| A3CallSuper (f,n) -> s "callsuper %s (%d)" (field f) n
	| A3CallProperty (f,n) -> s "callprop %s (%d)" (field f) n
	| A3RetVoid -> "retvoid"
	| A3Ret -> "ret"
	| A3ConstructSuper n -> s "constructsuper %d" n
	| A3ConstructProperty (f,n) -> s "constructprop %s (%d)" (field f) n
	| A3CallPropLex (f,n) -> s "callproplex %s (%d)" (field f) n
	| A3CallSuperVoid (f,n) -> s "callsupervoid %s (%d)" (field f) n
	| A3CallPropVoid (f,n) -> s "callpropvoid %s (%d)" (field f) n
	| A3ApplyType n -> s "applytype %d" n
	| A3Object n -> s "object %d" n
	| A3Array n -> s "array %d" n
	| A3NewBlock -> "newblock"
	| A3ClassDef n -> s "classdef %d" (int_index_nz n)
	| A3GetDescendants f -> s "getdescendants %s" (field f)
	| A3Catch n -> s "catch %d" n
	| A3FindPropStrict f -> s "findpropstrict %s" (field f)
	| A3FindProp f -> s "findprop %s" (field f)
	| A3FindDefinition f -> s "finddefinition %s" (field f)
	| A3GetLex f -> s "getlex %s" (field f)
	| A3SetProp f -> s "setprop %s" (field f)
	| A3Reg n -> s "reg %d" n
	| A3SetReg n -> s "setreg %d" n
	| A3GetGlobalScope -> "getglobalscope"
	| A3GetScope n -> s "getscope %d" n
	| A3GetProp f -> s "getprop %s" (field f)
	| A3InitProp f -> s "initprop %s" (field f)
	| A3DeleteProp f -> s "deleteprop %s" (field f)
	| A3GetSlot n -> s "getslot %d" n
	| A3SetSlot n -> s "setslot %d" n
	| A3ToString -> "tostring"
	| A3ToXml -> "toxml"
	| A3ToXmlAttr -> "toxmlattr"
	| A3ToInt -> "toint"
	| A3ToUInt -> "touint"
	| A3ToNumber -> "tonumber"
	| A3ToBool -> "tobool"
	| A3ToObject -> "toobject"
	| A3CheckIsXml -> "checkisxml"
	| A3Cast f -> s "cast %s" (field f)
	| A3AsAny -> "asany"
	| A3AsString -> "asstring"
	| A3AsType f -> s "astype %s" (field f)
	| A3AsObject -> "asobject"
	| A3IncrReg r -> s "incrreg %d" r
	| A3DecrReg r -> s "decrreg %d" r
	| A3Typeof -> "typeof"
	| A3InstanceOf -> "instanceof"
	| A3IsType f -> s "istype %s" (field f)
	| A3IncrIReg r -> s "incrireg %d" r
	| A3DecrIReg r -> s "decrireg %d" r
	| A3This -> "this"
	| A3SetThis -> "setthis"
	| A3DebugReg (name,reg,line) -> s ".reg %d:%s line:%d" reg (ident name) line
	| A3DebugLine l -> s ".line %d" l
	| A3DebugFile f -> s ".file %s" (ident f)
	| A3BreakPointLine l -> s ".bkptline %d" l
	| A3Timestamp -> ".time"
	| A3Op o -> dump_op o
	| A3Unk x -> s "??? 0x%X" (int_of_char x)
