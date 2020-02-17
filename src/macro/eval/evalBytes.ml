(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

let read_byte this i = int_of_char (Bytes.get this i)

let read_ui16 this i =
	let ch1 = read_byte this i in
	let ch2 = read_byte this (i + 1) in
	ch1 lor (ch2 lsl 8)

let read_i32 this i =
	let ch1 = read_byte this i in
	let ch2 = read_byte this (i + 1) in
	let ch3 = read_byte this (i + 2) in
	let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let big = Int32.shift_left (Int32.of_int (read_byte this (i + 3))) 24 in
	Int32.logor base big

let read_i64 this i =
	let ch1 = read_byte this i in
	let ch2 = read_byte this (i + 1) in
	let ch3 = read_byte this (i + 2) in
	let ch4 = read_byte this (i + 3) in
	let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
	let big = Int64.of_int32 (read_i32 this (i + 4)) in
	Int64.logor (Int64.shift_left big 32) small

let write_byte this i v =
	Bytes.set this i (Char.unsafe_chr v)

let write_ui16 this i v =
	write_byte this i v;
	write_byte this (i + 1) (v lsr 8)

let write_i32 this i v =
	let base = Int32.to_int v in
	let big = Int32.to_int (Int32.shift_right_logical v 24) in
	write_byte this i base;
	write_byte this (i + 1) (base lsr 8);
	write_byte this (i + 2) (base lsr 16);
	write_byte this (i + 3) big

let write_i64 this i v =
	write_i32 this i (Int64.to_int32 v);
	write_i32 this (i + 4) (Int64.to_int32 (Int64.shift_right_logical v 32))


module Unicase = struct
	let _E = EvalBytes_E_L79._E
	let _L1 = EvalBytes_E_L79._L1
	let _L3 = EvalBytes_E_L79._L3
	let _L4 = EvalBytes_E_L79._L4
	let _L5 = EvalBytes_E_L79._L5
	let _L6 = EvalBytes_E_L79._L6
	let _L7 = EvalBytes_E_L79._L7
	let _L8 = EvalBytes_E_L79._L8
	let _L9 = EvalBytes_E_L79._L9
	let _L13 = EvalBytes_E_L79._L13
	let _L14 = EvalBytes_E_L79._L14
	let _L15 = EvalBytes_E_L79._L15
	let _L16 = EvalBytes_E_L79._L16
	let _L17 = EvalBytes_E_L79._L17
	let _L18 = EvalBytes_E_L79._L18
	let _L19 = EvalBytes_E_L79._L19
	let _L20 = EvalBytes_E_L79._L20
	let _L21 = EvalBytes_E_L79._L21
	let _L66 = EvalBytes_E_L79._L66
	let _L67 = EvalBytes_E_L79._L67
	let _L78 = EvalBytes_E_L79._L78
	let _L79 = EvalBytes_E_L79._L79
	let _L120 = EvalBytes_L1020._L120
	let _L121 = EvalBytes_L1020._L121
	let _L122 = EvalBytes_L1020._L122
	let _L123 = EvalBytes_L1020._L123
	let _L124 = EvalBytes_L1020._L124
	let _L125 = EvalBytes_L1020._L125
	let _L126 = EvalBytes_L1020._L126
	let _L127 = EvalBytes_L1020._L127
	let _L132 = EvalBytes_L1020._L132
	let _L133 = EvalBytes_L1020._L133
	let _L134 = EvalBytes_L1020._L134
	let _L146 = EvalBytes_L1020._L146
	let _L147 = EvalBytes_L1020._L147
	let _L176 = EvalBytes_L1020._L176
	let _L177 = EvalBytes_L1020._L177
	let _L178 = EvalBytes_L1020._L178
	let _L179 = EvalBytes_L1020._L179
	let _L665 = EvalBytes_L1020._L665
	let _L666 = EvalBytes_L1020._L666
	let _L668 = EvalBytes_L1020._L668
	let _L669 = EvalBytes_L1020._L669
	let _L670 = EvalBytes_L1020._L670
	let _L1020 = EvalBytes_L1020._L1020
	let _U1 = EvalBytes_U79._U1
	let _U2 = EvalBytes_U79._U2
	let _U3 = EvalBytes_U79._U3
	let _U4 = EvalBytes_U79._U4
	let _U5 = EvalBytes_U79._U5
	let _U6 = EvalBytes_U79._U6
	let _U7 = EvalBytes_U79._U7
	let _U8 = EvalBytes_U79._U8
	let _U9 = EvalBytes_U79._U9
	let _U10 = EvalBytes_U79._U10
	let _U13 = EvalBytes_U79._U13
	let _U14 = EvalBytes_U79._U14
	let _U15 = EvalBytes_U79._U15
	let _U16 = EvalBytes_U79._U16
	let _U17 = EvalBytes_U79._U17
	let _U18 = EvalBytes_U79._U18
	let _U19 = EvalBytes_U79._U19
	let _U20 = EvalBytes_U79._U20
	let _U21 = EvalBytes_U79._U21
	let _U22 = EvalBytes_U79._U22
	let _U79 = EvalBytes_U79._U79
	let _U117 = EvalBytes_U1021._U117
	let _U120 = EvalBytes_U1021._U120
	let _U121 = EvalBytes_U1021._U121
	let _U122 = EvalBytes_U1021._U122
	let _U123 = EvalBytes_U1021._U123
	let _U124 = EvalBytes_U1021._U124
	let _U125 = EvalBytes_U1021._U125
	let _U126 = EvalBytes_U1021._U126
	let _U127 = EvalBytes_U1021._U127
	let _U133 = EvalBytes_U1021._U133
	let _U134 = EvalBytes_U1021._U134
	let _U147 = EvalBytes_U1021._U147
	let _U176 = EvalBytes_U1021._U176
	let _U177 = EvalBytes_U1021._U177
	let _U178 = EvalBytes_U1021._U178
	let _U179 = EvalBytes_U1021._U179
	let _U180 = EvalBytes_U1021._U180
	let _U665 = EvalBytes_U1021._U665
	let _U666 = EvalBytes_U1021._U666
	let _U668 = EvalBytes_U1021._U668
	let _U669 = EvalBytes_U1021._U669
	let _U670 = EvalBytes_U1021._U670
	let _U685 = EvalBytes_U1021._U685
	let _U686 = EvalBytes_U1021._U686
	let _U1021 = EvalBytes_U1021._U1021

	let _LOWER = EvalBytesLower._LOWER
	let _UPPER = EvalBytesUpper._UPPER
end