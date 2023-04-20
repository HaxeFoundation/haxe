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

open EvalValue

let create values = {
	avalues = values;
	alength = Array.length values;
}

let array_join a f sep =
	let l = Array.map f a in
	let l = Array.to_list l in
	EvalString.join sep l

let to_list a = Array.to_list (Array.sub a.avalues 0 a.alength)

let make len =
	try Array.make len vnull with _ -> EvalContext.error_message "Array allocation is too large"

let set_length a l =
	a.alength <- l;
	if a.alength > Array.length a.avalues then begin
		let values' = make (a.alength * 2) in
		Array.blit a.avalues 0 values' 0 (Array.length a.avalues);
		a.avalues <- values'
	end

let unsafe_get a i = a.avalues.(i)
let unsafe_set a i v = a.avalues.(i) <- v

let concat a a2 =
	let values' = make (a.alength + a2.alength) in
	Array.blit a.avalues 0 values' 0 a.alength;
	let values2 = (Obj.magic a2.avalues) in
	Array.blit values2 0 values' a.alength a2.alength;
	create values'

let copy a =
	create (Array.sub a.avalues 0 a.alength)

let filter a f =
	create (ExtArray.Array.filter f (Array.sub a.avalues 0 a.alength))

let get a i =
	if i < 0 || i >= a.alength then vnull
	else Array.unsafe_get a.avalues i

let rec indexOf a equals x fromIndex =
	if fromIndex >= a.alength then -1
	else if equals x (Array.get a.avalues fromIndex) then fromIndex
	else indexOf a equals x (fromIndex + 1)

let insert a pos x =
	if a.alength + 1 >= Array.length a.avalues then begin
		let values' = make (Array.length a.avalues * 2 + 5) in
		Array.blit a.avalues 0 values' 0 a.alength;
		a.avalues <- values'
	end;
	Array.blit a.avalues pos a.avalues (pos + 1) (a.alength - pos);
	Array.set a.avalues pos x;
	a.alength <- a.alength + 1

let iterator a =
	let i = ref 0 in
	let a = Array.sub a.avalues 0 a.alength in
	let length = Array.length a in
	(fun () ->
		!i < length
	),
	(fun () ->
		if !i >= length then
			vnull
		else begin
			let v = a.(!i) in
			incr i;
			v
		end
	)

let join a f sep =
	array_join (Array.sub a.avalues 0 a.alength) f sep

let lastIndexOf a equals x fromIndex =
	let rec loop i =
		if i < 0 then -1
		else if equals x (Array.get a.avalues i) then i
		else loop (i - 1)
	in
	if a.alength = 0 then -1 else loop fromIndex

let map a f =
	create (Array.map f (Array.sub a.avalues 0 a.alength))

let pop a =
	if a.alength = 0 then
		vnull
	else begin
		let v = get a (a.alength - 1) in
		unsafe_set a (a.alength - 1) vnull;
		a.alength <- a.alength - 1;
		v
	end

let push a v =
	if a.alength + 1 >= Array.length a.avalues then begin
		let values' = make (Array.length a.avalues * 2 + 5) in
		Array.blit a.avalues 0 values' 0 a.alength;
		Array.set values' a.alength v;
		a.avalues <- values'
	end else begin
		Array.set a.avalues a.alength v;
	end;
	a.alength <- a.alength + 1;
	a.alength

let remove a equals x =
	let i = indexOf a equals x 0 in
	if i < 0 then
		false
	else begin
		Array.blit a.avalues (i + 1) a.avalues i (a.alength - i - 1);
		a.alength <- a.alength - 1;
		true
	end

let contains a equals x =
	let i = indexOf a equals x 0 in
	i >= 0

let reverse a =
	a.avalues <- ExtArray.Array.rev (Array.sub a.avalues 0 a.alength)

let set a i v =
	if i >= a.alength then begin
		if i >= Array.length a.avalues then begin
			let values' = make (max (i + 5) (Array.length a.avalues * 2 + 5)) in
			Array.blit a.avalues 0 values' 0 a.alength;
			a.avalues <- values';
		end;
		a.alength <- i + 1;
	end;
	Array.unsafe_set a.avalues i v

let shift a =
	if a.alength = 0 then
		vnull
	else begin
		let v = get a 0 in
		a.alength <- a.alength - 1;
		Array.blit a.avalues 1 a.avalues 0 a.alength;
		v
	end

let slice a pos end' =
	if pos > a.alength || pos >= end' then
		create [||]
	else
		create (Array.sub a.avalues pos (end' - pos))

let sort a f =
	a.avalues <- Array.sub a.avalues 0 a.alength;
	Array.sort f a.avalues

let splice a pos len end' =
	let values' = Array.init len (fun i -> Array.get a.avalues (pos + i)) in
	Array.blit a.avalues (pos + len) a.avalues pos (a.alength - end');
	a.alength <- a.alength - len;
	create values'

let unshift a v =
	if a.alength + 1 >= Array.length a.avalues then begin
		let values' = make (Array.length a.avalues * 2 + 5) in
		Array.blit a.avalues 0 values' 1 a.alength;
		a.avalues <- values'
	end else begin
		Array.blit a.avalues 0 a.avalues 1 a.alength;
	end;
	Array.set a.avalues 0 v;
	a.alength <- a.alength + 1

let resize a l =
	if a.alength < l then begin
		set a (l - 1) vnull;
		()
	end else if a.alength > l then begin
		ignore(splice a l (a.alength - l) a.alength);
		()
	end else ()
