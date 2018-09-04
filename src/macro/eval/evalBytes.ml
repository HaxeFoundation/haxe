(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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