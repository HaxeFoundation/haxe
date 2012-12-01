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
	mutable darr : 'a intern option;
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
		darr = Some (imake 0 0);
	}

let init len f =
	if len > Sys.max_array_length then begin
		let count = (len + size - 1) lsr nbits in
		let d = {
			len = len;
			arr = imake 0 count;
			darr = None;
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
	end else begin
		let arr = imake 0 len in
		for i = 0 to len - 1 do
			iset arr i (f i)
		done;
		{
			len = len;
			arr = imake 0 0;
			darr = Some arr;
		}		
	end

let make len e =
	if len > Sys.max_array_length then begin
		let count = (len + size - 1) lsr nbits in
		let d = {
			len = len;
			arr = imake 0 count;
			darr = None;
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
	end else begin
		let arr = imake 0 len in
		for i = 0 to len - 1 do
			iset arr i e
		done;
		{
			len = len;
			arr = imake 0 0;
			darr = Some arr;
		}
	end

let empty d =
	d.len = 0

let get d idx =
	if idx < 0 || idx >= d.len then invalid_arg idx "get" "index";
	match d.darr with
	| None -> iget (iget d.arr (idx lsr nbits)) (idx land mask)
	| Some arr -> iget arr idx

let set d idx v =
	if idx < 0 || idx >= d.len then invalid_arg idx "set" "index";
	match d.darr with
	| None -> iset (iget d.arr (idx lsr nbits)) (idx land mask) v
	| Some arr -> iset arr idx v

let rec add d v =
	(match d.darr with
	| None ->
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
	| Some arr ->
		if d.len < ilen arr then begin
			(* set *)
			iset arr d.len v;			
		end else if d.len lsl 1 >= Sys.max_array_length then begin
			(* promote *)
			let count = (d.len + size) lsr nbits in
			d.darr <- None;
			d.arr <- imake 0 count;
			let max = count - 1 in
			for i = 0 to max do
				let arr2 = imake 0 size in
				iset d.arr i arr2;
				for j = 0 to (if i = max then d.len land mask else size) - 1 do
					iset arr2 j (iget arr ((i lsl nbits) + j))
				done;
			done;
			iset (iget d.arr (d.len lsr nbits)) (d.len land mask) v;
		end else begin
			(* resize *)
			let arr2 = imake 0 (if d.len = 0 then 1 else d.len lsl 1) in
			for i = 0 to d.len - 1 do
				iset arr2 i (iget arr i)
			done;
			iset arr2 d.len v;
			d.darr <- Some arr2;
		end);
	d.len <- d.len + 1

let clear d =
	d.len <- 0;
	d.arr <- imake 0 0;
	d.darr <- Some (imake 0 0)

let of_array src =
	let c = create() in
	Array.iteri (fun i v -> add c v) src;
	c

let of_list src =
	let c = create() in
	List.iter (add c) src;
	c
	
let iter f d = match d.darr with
	| None ->
	 	let max = ilen d.arr - 1 in
		for i = 0 to max do
			let arr = iget d.arr i in
			for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
				f (iget arr j)
			done;
		done
	| Some arr ->
		for i = 0 to d.len - 1 do
			f (iget arr i)
		done

let iteri f d = match d.darr with
	| None ->
		let max = ilen d.arr - 1 in
		for i = 0 to max do
			let arr = iget d.arr i in
			for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
				f ((i lsl nbits) + j) (iget arr j)
			done;
		done
	| Some arr ->
		for i = 0 to d.len - 1 do
			f i (iget arr i)
		done

let map f d = match d.darr with
	| None ->
		let max = ilen d.arr - 1 in
		let d2 = {
			len = d.len;
			arr = imake 0 (max + 1);
			darr = None;
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
	| Some arr ->
		let arr2 = imake 0 d.len in
		for i = 0 to d.len - 1 do
			iset arr2 i (f (iget arr i))
		done;
		{
			len = d.len;
			arr = imake 0 0;
			darr = Some (arr2);
		}

let mapi f d = match d.darr with
	| None ->
		let max = ilen d.arr - 1 in
		let d2 = {
			len = d.len;
			arr = imake 0 (max + 1);
			darr = None;
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
	| Some arr ->
		let arr2 = imake 0 d.len in
		for i = 0 to d.len - 1 do
			iset arr2 i (f i (iget arr i))
		done;
		{
			len = d.len;
			arr = imake 0 0;
			darr = Some (arr2);
		}

let fold_left f acc d = match d.darr with
	| None ->
		let acc = ref acc in
		let max = ilen d.arr - 1 in
		for i = 0 to max do
			let arr = iget d.arr i in
			for j = 0 to (if i = max then (d.len land mask) else size) - 1 do
				acc := f !acc (iget arr j)
			done;
		done;
		!acc
	| Some arr ->
		let acc = ref acc in
		for i = 0 to d.len - 1 do
			acc := f !acc (iget arr i)
		done;
		!acc