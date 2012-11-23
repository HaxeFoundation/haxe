(*
 * Bitset - Efficient bit sets
 * Copyright (C) 2003 Nicolas Cannasse
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA	02111-1307	USA
 *)

type intern

let bcreate : int -> intern = Obj.magic String.create
external fast_get : intern -> int -> int = "%string_unsafe_get"
external fast_set : intern -> int -> int -> unit = "%string_unsafe_set"
external fast_bool : int -> bool = "%identity"
let fast_blit : intern -> int -> intern -> int -> int -> unit = Obj.magic String.blit
let fast_fill : intern -> int -> int -> int -> unit = Obj.magic String.fill
let fast_length : intern -> int= Obj.magic String.length

let bget s ndx =
  assert (ndx >= 0 && ndx < fast_length s);
  fast_get s ndx

let bset s ndx v =
  assert (ndx >= 0 && ndx < fast_length s);
  fast_set s ndx v

let bblit src srcoff dst dstoff len = 
  assert (srcoff >= 0 && dstoff >= 0 && len >= 0);
  fast_blit src srcoff dst dstoff len

let bfill dst start len c = 
  assert (start >= 0 && len >= 0);
  fast_fill dst start len c

exception Negative_index of string

type t = {
	mutable data : intern;
	mutable len : int;
}

let error fname = raise (Negative_index fname)

let empty() =
	{
		data = bcreate 0;
		len = 0;
	}

let int_size = 7 (* value used to round up index *)
let log_int_size = 3 (* number of shifts *)

let create n =
	if n < 0 then error "create";
	let size = (n+int_size) lsr log_int_size in
	let b = bcreate size in
	bfill b 0 size 0;
	{
		data = b;
		len = size;
	}

let copy t =
	let b = bcreate t.len in
	bblit t.data 0 b 0 t.len;
	{
		data = b;
		len = t.len
	}

let clone = copy

let set t x =
	if x < 0 then error "set";
	let pos = x lsr log_int_size and delta = x land int_size in
	let size = t.len in
	if pos >= size then begin
		let b = bcreate (pos+1) in
		bblit t.data 0 b 0 size;
		bfill b size (pos - size + 1) 0;
		t.len <- pos + 1;
		t.data <- b;
	end;
	bset t.data pos ((bget t.data pos) lor (1 lsl delta))

let unset t x =
	if x < 0 then error "unset";
	let pos = x lsr log_int_size and delta = x land int_size in
	if pos < t.len then
		bset t.data pos ((bget t.data pos) land (0xFF lxor (1 lsl delta)))

let toggle t x =
	if x < 0 then error "toggle";
	let pos = x lsr log_int_size and delta = x land int_size in
	let size = t.len in
	if pos >= size then begin
		let b = bcreate (pos+1) in
		bblit t.data 0 b 0 size;
		bfill b size (pos - size + 1) 0;
		t.len <- pos + 1;
		t.data <- b;
	end;
	bset t.data pos ((bget t.data pos) lxor (1 lsl delta))

let put t = function
	| true -> set t
	| false -> unset t

let is_set t x =
  if x < 0 then error "is_set";
  let pos = x lsr log_int_size and delta = x land int_size in
  let size = t.len in
  if pos < size then
	fast_bool (((bget t.data pos) lsr delta) land 1)
  else
	false


exception Break_int of int

(* Find highest set element or raise Not_found *)
let find_msb t =
  (* Find highest set bit in a byte.  Does not work with zero. *)
  let byte_msb b = 
    assert (b <> 0);
    let rec loop n = 
      if b land (1 lsl n) = 0 then
        loop (n-1)
      else n in
    loop 7 in
  let n = t.len - 1
  and buf = t.data in
  try 
    for i = n downto 0 do
      let byte = bget buf i in
      if byte <> 0 then raise (Break_int ((i lsl log_int_size)+(byte_msb byte)))
    done;
    raise Not_found
  with 
    Break_int n -> n
  | _ -> raise Not_found

let compare t1 t2 =
  let some_msb b = try Some (find_msb b) with Not_found -> None in
  match (some_msb t1, some_msb t2) with
    (None, Some _) -> -1 (* 0-y -> -1 *)
  | (Some _, None) -> 1  (* x-0 ->  1 *)
  | (None, None) -> 0    (* 0-0 ->  0 *)
  | (Some a, Some b) ->  (* x-y *)
      if a < b then -1
      else if a > b then 1
      else
        begin
          (* MSBs differ, we need to scan arrays until we find a
             difference *)
          let ndx = a lsr log_int_size in 
          assert (ndx < t1.len && ndx < t2.len);
          try
            for i = ndx downto 0 do
              let b1 = bget t1.data i 
              and b2 = bget t2.data i in
              if b1 <> b2 then raise (Break_int (compare b1 b2))
            done;
            0
          with
            Break_int res -> res
        end

let equals t1 t2 =
	compare t1 t2 = 0

let partial_count t x =
	let rec nbits x =
		if x = 0 then
			0
		else if fast_bool (x land 1) then
			1 + (nbits (x lsr 1))
		else
			nbits (x lsr 1)
	in
	let size = t.len in
	let pos = x lsr log_int_size and delta = x land int_size in
	let rec loop n acc =
		if n = size then
			acc
		else
			let x = bget t.data n in
			loop (n+1) (acc + nbits x)
	in
	if pos >= size then
		0
	else
		loop (pos+1) (nbits ((bget t.data pos) lsr delta))

let count t =
	partial_count t 0

let enum t =
	let rec make n =
		let cur = ref n in
		let rec next() =
			let pos = !cur lsr log_int_size and delta = !cur land int_size in
			if pos >= t.len then raise Enum.No_more_elements;
			let x = bget t.data pos in
			let rec loop i =
	if i = 8 then
		next()
	else if x land (1 lsl i) = 0 then begin
		incr cur;
		loop (i+1)
	end else
		!cur
			in
			let b = loop delta in
			incr cur;
			b
		in
		Enum.make
			~next
			~count:(fun () -> partial_count t !cur)
			~clone:(fun () -> make !cur)
	in
	make 0

let raw_create size = 
  let b = bcreate size in
  bfill b 0 size 0;
  { data = b; len = size }

let inter a b =
  let max_size = max a.len b.len in
  let d = raw_create max_size in
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  (* Note: rest of the array is set to zero automatically *)
  for i = 0 to sl-1 do
    bset d.data i ((bget abuf i) land (bget bbuf i))
  done;
  d

(* Note: rest of the array is handled automatically correct, since we
   took a copy of the bigger set. *)
let union a b = 
  let d = if a.len > b.len then copy a else copy b in
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  for i = 0 to sl-1 do
    bset d.data i ((bget abuf i) lor (bget bbuf i))
  done;
  d

let diff a b = 
  let maxlen = max a.len b.len in
  let buf = bcreate maxlen in
  bblit a.data 0 buf 0 a.len;
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  for i = 0 to sl-1 do
    bset buf i ((bget abuf i) land (lnot (bget bbuf i)))
  done;
  { data = buf; len = maxlen }

let sym_diff a b = 
  let maxlen = max a.len b.len in
  let buf = bcreate maxlen in
  (* Copy larger (assumes missing bits are zero) *)
  bblit (if a.len > b.len then a.data else b.data) 0 buf 0 maxlen;
  let sl = min a.len b.len in
  let abuf = a.data
  and bbuf = b.data in
  for i = 0 to sl-1 do
    bset buf i ((bget abuf i) lxor (bget bbuf i))
  done;
  { data = buf; len = maxlen }

(* TODO the following set operations can be made faster if you do the
   set operation in-place instead of taking a copy.  But be careful
   when the sizes of the bitvector strings differ. *)
let intersect t t' =
  let d = inter t t' in
  t.data <- d.data;
  t.len <- d.len

let differentiate t t' =
  let d = diff t t' in
  t.data <- d.data;
  t.len <- d.len

let unite t t' =
  let d = union t t' in
  t.data <- d.data;
  t.len <- d.len

let differentiate_sym t t' =
  let d = sym_diff t t' in
  t.data <- d.data;
  t.len <- d.len
