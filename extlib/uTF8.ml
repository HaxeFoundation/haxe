(* 
 * UTF-8 - UTF-8 encoded Unicode string
 * Copyright 2002, 2003 (C) Yamagata Yoriyuki. 
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

open UChar

type t = string
type index = int
  
let look s i =
  let n' =
    let n = Char.code s.[i] in
    if n < 0x80 then n else
    if n <= 0xdf then
      (n - 0xc0) lsl 6 lor (0x7f land (Char.code s.[i + 1]))
    else if n <= 0xef then
      let n' = n - 0xe0 in
      let m0 = Char.code s.[i + 2] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)
    else if n <= 0xf7 then
      let n' = n - 0xf0 in
      let m0 = Char.code s.[i + 3] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)     
    else if n <= 0xfb then
      let n' = n - 0xf8 in
      let m0 = Char.code s.[i + 4] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 3)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)     
    else if n <= 0xfd then
      let n' = n - 0xfc in
      let m0 = Char.code s.[i + 5] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 3)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 4)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)
    else invalid_arg "UTF8.look"
  in
  Obj.magic n'

let rec search_head s i =
  if i >= String.length s then i else
  let n = Char.code (String.unsafe_get s i) in
  if n < 0x80 || n >= 0xc2 then i else
  search_head s (i + 1)

let next s i = 
  let n = Char.code s.[i] in
  if n < 0x80 then i + 1 else
  if n < 0xc0 then search_head s (i + 1) else
  if n <= 0xdf then i + 2
  else if n <= 0xef then i + 3
  else if n <= 0xf7 then i + 4
  else if n <= 0xfb then i + 5
  else if n <= 0xfd then i + 6
  else invalid_arg "UTF8.next"

let rec search_head_backward s i =
  if i < 0 then -1 else
  let n = Char.code s.[i] in
  if n < 0x80 || n >= 0xc2 then i else
  search_head_backward s (i - 1)

let prev s i = search_head_backward s (i - 1)

let move s i n =
  if n >= 0 then
    let rec loop i n = if n <= 0 then i else loop (next s i) (n - 1) in
    loop i n
  else
    let rec loop i n = if n >= 0 then i else loop (prev s i) (n + 1) in
    loop i n

let rec nth_aux s i n =
  if n = 0 then i else
  nth_aux s (next s i) (n - 1)

let nth s n = nth_aux s 0 n

let last s = search_head_backward s (String.length s - 1)

let out_of_range s i = i < 0 || i >= String.length s

let compare_index _ i j = i - j

let get s n = look s (nth s n)

let add_uchar buf u =
  let masq = 0b111111 in
  let k = int_of_uchar u in
  if k < 0 || k >= 0x4000000 then begin
    Buffer.add_char buf (Char.chr (0xfc + (k lsr 30)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 24) land masq))); 
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 18) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end else if k <= 0x7f then
    Buffer.add_char buf (Char.unsafe_chr k)
  else if k <= 0x7ff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xc0 lor (k lsr 6)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)))
  end else if k <= 0xffff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xe0 lor (k lsr 12)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end else if k <= 0x1fffff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xf0 + (k lsr 18)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end else begin
    Buffer.add_char buf (Char.unsafe_chr (0xf8 + (k lsr 24)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 18) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end 

let init len f =
  let buf = Buffer.create len in
  for c = 0 to len - 1 do add_uchar buf (f c) done;
  Buffer.contents buf

let rec length_aux s c i =
  if i >= String.length s then c else
  let n = Char.code (String.unsafe_get s i) in
  let k =
    if n < 0x80 then 1 else
    if n < 0xc0 then invalid_arg "UTF8.length" else
    if n < 0xe0 then 2 else
    if n < 0xf0 then 3 else
    if n < 0xf8 then 4 else
    if n < 0xfc then 5 else
    if n < 0xfe then 6 else
    invalid_arg "UTF8.length" in
  length_aux s (c + 1) (i + k)

let length s = length_aux s 0 0

let rec iter_aux proc s i =
  if i >= String.length s then () else
  let u = look s i in
  proc u;
  iter_aux proc s (next s i)

let iter proc s = iter_aux proc s 0

let compare s1 s2 = Pervasives.compare s1 s2

exception Malformed_code

let validate s =
  let rec trail c i a =
    if c = 0 then a else
    if i >= String.length s then raise Malformed_code else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 || n >= 0xc0 then raise Malformed_code else
    trail (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let rec main i =
    if i >= String.length s then () else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 then main (i + 1) else
    if n < 0xc2 then raise Malformed_code else
    if n <= 0xdf then 
      if trail 1 (i + 1) (n - 0xc0) < 0x80 then raise Malformed_code else 
      main (i + 2)
    else if n <= 0xef then 
      if trail 2 (i + 1) (n - 0xe0) < 0x800 then raise Malformed_code else 
      main (i + 3)
    else if n <= 0xf7 then 
      if trail 3 (i + 1) (n - 0xf0) < 0x10000 then raise Malformed_code else
      main (i + 4)
    else if n <= 0xfb then 
      if trail 4 (i + 1) (n - 0xf8) < 0x200000 then raise Malformed_code else
      main (i + 5)
    else if n <= 0xfd then 
      let n = trail 5 (i + 1) (n - 0xfc) in
      if n lsr 16 < 0x400 then raise Malformed_code else
      main (i + 6)
    else raise Malformed_code in
  main 0

module Buf = 
  struct
    include Buffer
    type buf = t
    let add_char = add_uchar
  end
