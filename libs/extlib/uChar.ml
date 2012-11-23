(* 
 * UChar - Unicode (ISO-UCS) characters
 * Copyright (C) 2002, 2003 Yamagata Yoriyuki
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type t = int

exception Out_of_range

external unsafe_chr_of_uint : int -> t = "%identity"
external uint_code : t -> int = "%identity"

let char_of c = 
  if c >= 0 && c < 0x100 then Char.chr c else raise Out_of_range

let of_char = Char.code

let code c = if c >= 0 then c else raise Out_of_range

let chr n =
  if n >= 0 && n lsr 31 = 0 then n else invalid_arg "UChar.chr"

let chr_of_uint n = if n lsr 31 = 0 then n else invalid_arg "UChar.uint_chr"
  
let eq (u1 : t) (u2 : t) = u1 = u2
let compare u1 u2 =
  let sgn = (u1 lsr 16) - (u2 lsr 16) in
  if sgn = 0 then (u1 land 0xFFFF) -  (u2 land 0xFFFF) else sgn

type uchar = t

let int_of_uchar u = uint_code u
let uchar_of_int n = chr_of_uint n
