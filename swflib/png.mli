(*
 *  PNG File Format Library
 *  Copyright (c)2005 Nicolas Cannasse
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

type grey_bits =
	| GBits1
	| GBits2
	| GBits4
	| GBits8
	| GBits16

type grey_alpha_bits =
	| GABits8
	| GABits16

type true_bits =
	| TBits8
	| TBits16

type index_bits =
	| IBits1
	| IBits2
	| IBits4
	| IBits8

type alpha =
	| NoAlpha
	| HaveAlpha

type color =
	| ClGreyScale of grey_bits
	| ClGreyAlpha of grey_alpha_bits
	| ClTrueColor of true_bits * alpha
	| ClIndexed of index_bits

type header = {
	width : int;
	height : int;
	color : color;
	interlace : bool;
}

type chunk_id = string

type chunk = 
	| CEnd
	| CHeader of header
	| CData of string
	| CPalette of string
	| CUnknown of chunk_id * string

type png = {
	header : header;
	data : string;
	palette : string option;
	chunks : chunk list;
}

type error_msg =
	| Invalid_header
	| Invalid_file
	| Truncated_file
	| Invalid_CRC
	| Invalid_colors
	| Unsupported_colors
	| Invalid_datasize
	| Invalid_filter of int

exception Error of error_msg

val error_msg : error_msg -> string

val is_critical : chunk_id -> bool
val is_public : chunk_id -> bool
val is_reseverd : chunk_id -> bool
val is_safe_to_copy : chunk_id -> bool

val color_bits : color -> int
val parse : IO.input -> png
val filter : png -> ?keep_invisible:bool -> ?default_alpha:char -> string -> string

