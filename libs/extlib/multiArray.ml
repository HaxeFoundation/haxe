(*
 * MultiArray - Resizeable Big Ocaml arrays
 * Copyright (C) 2012 Nicolas Cannasse
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

type 'a intern

external ilen : 'a intern -> int = "%obj_size"
let idup (x : 'a intern) = if ilen x = 0 then x else (Obj.magic (Obj.dup (Obj.repr x)) : 'a intern)
let imake tag len = (Obj.magic (Obj.new_block tag len) : 'a intern)
external iget : 'a intern -> int -> 'a = "%obj_field"
external iset : 'a intern -> int -> 'a -> unit = "%obj_set_field"

type 'a t = {
	mutable arr : 'a intern intern;
	mutable len : int;
}

exception Invalid_arg of int * string * string

let invalid_arg n f p = raise (Invalid_arg (n,f,p))

let length d = d.len

(* create 1K chunks, which allows up to 4GB elements *)

let nbits = 10
let size = 1 lsl nbits
let mask = size - 1

let create() =
	{
		len = 0;
		arr = imake 0 0;
	}

let init len f =
	let count = (len + size - 1) lsr nbits in
	let d = {
		len = len;
		arr = imake 0 count;
	} in
	let max = count - 1 in
	for i = 0 to max do
		let arr = imake 0 size in
		iset d.arr i arr;
		for j = 0 to (if i = max then len land mask else size) - 1 do
			iset arr j (f ((i lsl nbits) + j))
		done;
	done;
	d

let make len e =
	let count = (len + size - 1) lsr nbits in
	let d = {
		len = len;
		arr = imake 0 count;
	} in
	let max = count - 1 in
	for i = 0 to max do
		let arr = imake 0 size in
		iset d.arr i arr;
		for j = 0 to (if i = max then len land mask else size) - 1 do
			iset arr j e
		done;
	done;
	d

let empty d =
	d.len = 0

let unsafe_get d idx =
	iget (iget d.arr (idx lsr nbits)) (idx land mask)

let unsafe_set d idx v =
	iset (iget d.arr (idx lsr nbits)) (idx land mask) v

let get d idx =
	if idx < 0 || idx >= d.len then invalid_arg idx "get" "index";
	iget (iget d.arr (idx lsr nbits)) (idx land mask)

let set d idx v =
	if idx < 0 || idx >= d.len then invalid_arg idx "set" "index";
	iset (iget d.arr (idx lsr nbits)) (idx land mask) v

let add d v =
	let asize = ilen d.arr in
	if d.len >= asize lsl nbits then begin
		let narr = imake 0 (asize + 1) in
		for i = 0 to asize-1 do
			iset narr i (iget d.arr i);
		done;
		iset narr asize (imake 0 size);
		d.arr <- narr;
	end;
	iset (iget d.arr (d.len lsr nbits)) (d.len land mask) v;
	d.len <- d.len + 1

let clear d =
	d.len <- 0;
	d.arr <- imake 0 0

let reset_pos d =
	d.len <- 0

let of_array src =
	let c = create() in
	Array.iteri (fun i v -> add c v) src;
	c

let of_list src =
	let c = create() in
	List.iter (add c) src;
	c
	
let iter f d =
 	let max = ilen d.arr - 1 in
	for i = 0 to max do
		let arr = iget d.arr i in
		for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
			f (iget arr j)
		done;
	done

let iteri f d =
	let max = ilen d.arr - 1 in
	for i = 0 to max do
		let arr = iget d.arr i in
		for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
			f ((i lsl nbits) + j) (iget arr j)
		done;
	done

let map f d =
	let max = ilen d.arr - 1 in
	let d2 = {
		len = d.len;
		arr = imake 0 (max + 1);
	} in
	for i = 0 to max do
		let arr = iget d.arr i in
		let narr = imake 0 size in
		iset d2.arr i narr;
		for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
			iset narr j (f (iget arr j))
		done;
	done;
	d2

let mapi f d =
	let max = ilen d.arr - 1 in
	let d2 = {
		len = d.len;
		arr = imake 0 (max + 1);
	} in
	for i = 0 to max do
		let arr = iget d.arr i in
		let narr = imake 0 size in
		iset d2.arr i narr;
		for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
			iset narr j (f ((i lsl nbits) + j) (iget arr j))
		done;
	done;
	d2

let fold_left f acc d =
	let acc = ref acc in
	let max = ilen d.arr - 1 in
	for i = 0 to max do
		let arr = iget d.arr i in
		for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
			acc := f !acc (iget arr j)
		done;
	done;
	!acc

