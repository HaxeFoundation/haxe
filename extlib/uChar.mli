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

(** Unicode (ISO-UCS) characters.

   This module implements Unicode (actually ISO-UCS) characters.  All
   31-bit code points are allowed.
*)

(** Unicode characters. All 31-bit code points are allowed.*) 
type t

exception Out_of_range

(** [char_of u] returns the Latin-1 representation of [u].
   If [u] can not be represented by Latin-1, raises Out_of_range *)
val char_of : t -> char

(** [of_char c] returns the Unicode character of the Latin-1 character [c] *)
val of_char : char -> t

(** [code u] returns the Unicode code number of [u].
   If the value can not be represented by a positive integer,
   raise Out_of_range *)
val code : t -> int

(** [code n] returns the Unicode character with the code number [n]. 
   If n >= 2^32 or n < 0, raises [invalid_arg] *)
val chr : int -> t

(** [uint_code u] returns the Unicode code number of [u].
   The returned int is unsigned, that is, on 32-bit platforms,
   the sign bit is used for storing the 31-th bit of the code number. *)
external uint_code : t -> int = "%identity"

(** [chr_of_uint n] returns the Unicode character of the code number [n].
   [n] is interpreted as unsigned, that is, on 32-bit platforms,
   the sign bit is treated as the 31-th bit of the code number.
   If n exceeds 31-bit values, then raise [Invalid_arg]. *)
val chr_of_uint : int -> t

(** Unsafe version of {!UChar.chr_of_uint}.
   No check of its argument is performed. *)
external unsafe_chr_of_uint : int -> t = "%identity"

(** Equality by code point comparison *)
val eq : t -> t -> bool

(** [compare u1 u2] returns, 
   a value > 0 if [u1] has a larger Unicode code number than [u2], 
   0 if [u1] and [u2] are the same Unicode character,
   a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)
val compare : t -> t -> int

(** Aliases of [type t] *)
type uchar = t

(** Alias of [uint_code] *)
val int_of_uchar : uchar -> int

(** Alias of [chr_of_uint] *)
val uchar_of_int : int -> uchar
