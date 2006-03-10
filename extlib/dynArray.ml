(*
 * DynArray - Resizeable Ocaml arrays
 * Copyright (C) 2003 Brian Hurt
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type resizer_t = currslots:int -> oldlength:int -> newlength:int -> int

type 'a intern

external ilen : 'a intern -> int = "%obj_size"
let idup (x : 'a intern) = if ilen x = 0 then x else (Obj.magic (Obj.dup (Obj.repr x)) : 'a intern)
let imake tag len = (Obj.magic (Obj.new_block tag len) : 'a intern)
external iget : 'a intern -> int -> 'a = "%obj_field"
external iset : 'a intern -> int -> 'a -> unit = "%obj_set_field"

type 'a t = {
	mutable arr : 'a intern;
	mutable len : int;
	mutable resize: resizer_t;
}

exception Invalid_arg of int * string * string

let invalid_arg n f p = raise (Invalid_arg (n,f,p))

let length d = d.len

let exponential_resizer ~currslots ~oldlength ~newlength =
	let rec doubler x = if x >= newlength then x else doubler (x * 2) in
	let rec halfer x = if x / 2 < newlength then x else halfer (x / 2) in
	if newlength = 1 then
		1
	else if currslots = 0 then
		doubler 1
	else if currslots < newlength then
		doubler currslots
	else
		halfer currslots

let step_resizer step =
	if step <= 0 then invalid_arg step "step_resizer" "step";
	(fun ~currslots ~oldlength ~newlength ->
		if currslots < newlength || newlength < (currslots - step)
		then
		   (newlength + step - (newlength mod step))
		else
			currslots)

let conservative_exponential_resizer ~currslots ~oldlength ~newlength =
	let rec doubler x = if x >= newlength then x else doubler (x * 2) in
	let rec halfer x = if x / 2 < newlength then x else halfer (x / 2) in
	if currslots < newlength then begin
		if newlength = 1 then
			1
		else if currslots = 0 then
			doubler 1
		else
			doubler currslots
	end else if oldlength < newlength then
		halfer currslots
	else
		currslots

let default_resizer = conservative_exponential_resizer

let changelen (d : 'a t) newlen =
	let oldsize = ilen d.arr in
	let r = d.resize
			~currslots:oldsize
			~oldlength:d.len
			~newlength:newlen
	in
	(* We require the size to be at least large enough to hold the number
	 * of elements we know we need!
	 *)
	let newsize = if r < newlen then newlen else r in
	if newsize <> oldsize then begin
		let newarr = imake 0 newsize in
		let cpylen = (if newlen < d.len then newlen else d.len) in
		for i = 0 to cpylen - 1 do
			iset newarr i (iget d.arr i);
		done;
		d.arr <- newarr;
	end;
	d.len <- newlen

let compact d =
	if d.len <> ilen d.arr then begin
		let newarr = imake 0 d.len in
		for i = 0 to d.len - 1 do
			iset newarr i (iget d.arr i)
		done;
		d.arr <- newarr;
	end

let create() =
	{
		resize = default_resizer;
		len = 0;
		arr = imake 0 0;
	}

let make initsize =
	if initsize < 0 then invalid_arg initsize "make" "size";
	{
		resize = default_resizer;
		len = 0;
		arr = imake 0 initsize;
	}

let init initlen f =
	if initlen < 0 then invalid_arg initlen "init" "len";
	let arr = imake 0 initlen in
	for i = 0 to initlen-1 do
		iset arr i (f i)
	done;
	{
		resize = default_resizer;
		len = initlen;
		arr = arr;
	}

let set_resizer d resizer =
	d.resize <- resizer

let get_resizer d =
	d.resize

let empty d =
	d.len = 0

let get d idx =
	if idx < 0 || idx >= d.len then invalid_arg idx "get" "index";
	iget d.arr idx

let last d =
	if d.len = 0 then invalid_arg 0 "last" "<array len is 0>";
	iget d.arr (d.len - 1)

let set d idx v =
	if idx < 0 || idx >= d.len then 	invalid_arg idx "set" "index";
	iset d.arr idx v

let insert d idx v =
	if idx < 0 || idx > d.len then invalid_arg idx "insert" "index";
	if d.len = ilen d.arr then changelen d (d.len + 1) else d.len <- d.len + 1;
	if idx < d.len - 1 then begin
		for i = d.len - 1 downto idx do
			iset d.arr (i+1) (iget d.arr i)
		done;
	end;
	iset d.arr idx v

let add d v =
	if d.len = ilen d.arr then changelen d (d.len + 1) else d.len <- d.len + 1;
	iset d.arr (d.len - 1) v

let delete d idx =
	if idx < 0 || idx >= d.len then invalid_arg idx "delete" "index";
	let oldsize = ilen d.arr in
	(* we don't call changelen because we want to blit *)
	let r = d.resize
		~currslots:oldsize
		~oldlength:d.len
		~newlength:(d.len - 1)
	in
	let newsize = (if r < d.len - 1 then d.len - 1 else r) in
	if oldsize <> newsize then begin
		let newarr = imake 0 newsize in
		for i = 0 to idx - 1 do
			iset newarr i (iget d.arr i);
		done;
		for i = idx to d.len - 2 do
			iset newarr i (iget d.arr (i+1));
		done;
		d.arr <- newarr;
	end else begin
		for i = idx to d.len - 2 do
			iset d.arr i (iget d.arr (i+1));
		done;
		iset d.arr (d.len - 1) (Obj.magic 0)
	end;
	d.len <- d.len - 1


let delete_range d idx len =
	if len < 0 then invalid_arg len "delete_range" "length";
	if idx < 0 || idx + len > d.len then invalid_arg idx "delete_range" "index";
	let oldsize = ilen d.arr in
	(* we don't call changelen because we want to blit *)
	let r = d.resize
		~currslots:oldsize
		~oldlength:d.len
		~newlength:(d.len - len)
	in
	let newsize = (if r < d.len - len then d.len - len else r) in
	if oldsize <> newsize then begin
		let newarr = imake 0 newsize in
		for i = 0 to idx - 1 do
			iset newarr i (iget d.arr i);
		done;
		for i = idx to d.len - len - 1 do
			iset newarr i (iget d.arr (i+len));
		done;
		d.arr <- newarr;
	end else begin
		for i = idx to d.len - len - 1 do
			iset d.arr i (iget d.arr (i+len));
		done;
		for i = d.len - len to d.len - 1 do
			iset d.arr i (Obj.magic 0)
		done;
	end;
	d.len <- d.len - len

let clear d =
	d.len <- 0;
	d.arr <- imake 0 0

let delete_last d =
	if d.len <= 0 then invalid_arg 0 "delete_last" "<array len is 0>";
	(* erase for GC, in case changelen don't resize our array *)
	iset d.arr (d.len - 1) (Obj.magic 0);
	changelen d (d.len - 1)

let rec blit src srcidx dst dstidx len =
	if len < 0 then invalid_arg len "blit" "len";
	if srcidx < 0 || srcidx + len > src.len then invalid_arg srcidx "blit" "source index";
	if dstidx < 0 || dstidx > dst.len then invalid_arg dstidx "blit" "dest index";
	let newlen = dstidx + len in
	if newlen > ilen dst.arr then begin
		(* this case could be inlined so we don't blit on just-copied elements *)
		changelen dst newlen
	end else begin
		if newlen > dst.len then dst.len <- newlen;
	end;
	(* same array ! we need to copy in reverse order *)
	if src.arr == dst.arr && dstidx > srcidx then
		for i = len - 1 downto 0 do
			iset dst.arr (dstidx+i) (iget src.arr (srcidx+i));
		done
	else
		for i = 0 to len - 1 do
			iset dst.arr (dstidx+i) (iget src.arr (srcidx+i));
		done

let append src dst =
	blit src 0 dst dst.len src.len

let to_list d =
	let rec loop idx accum =
		if idx < 0 then accum else loop (idx - 1) (iget d.arr idx :: accum)
	in
	loop (d.len - 1) []

let to_array d =
	if d.len = 0 then begin
		(* since the empty array is an atom, we don't care if float or not *)
		[||]
	end else begin
		let arr = Array.make d.len (iget d.arr 0) in
		for i = 1 to d.len - 1 do
			Array.unsafe_set arr i (iget d.arr i)
		done;
		arr;
	end

let of_list lst =
	let size = List.length lst in
	let arr = imake 0 size in
	let rec loop idx =  function
		| h :: t -> iset arr idx h; loop (idx + 1) t
		| [] -> ()
	in
	loop 0 lst;
	{
		resize = default_resizer;
		len = size;
		arr = arr;
	}

let of_array src =
	let size = Array.length src in
	let is_float = Obj.tag (Obj.repr src) = Obj.double_array_tag in
	let arr = (if is_float then begin
			let arr = imake 0 size in
			for i = 0 to size - 1 do
				iset arr i (Array.unsafe_get src i);
			done;
			arr
		end else
			(* copy the fields *)
			idup (Obj.magic src : 'a intern))
	in
	{
		resize = default_resizer;
		len = size;
		arr = arr;
	}

let copy src =
	{
		resize = src.resize;
		len = src.len;
		arr = idup src.arr;
	}

let sub src start len =
	if len < 0 then invalid_arg len "sub" "len";
	if start < 0 || start + len > src.len then invalid_arg start "sub" "start";
	let arr = imake 0 len in
	for i = 0 to len - 1 do
		iset arr i (iget src.arr (i+start));
	done;
	{
		resize = src.resize;
		len = len;
		arr = arr;
	}

let iter f d =
	for i = 0 to d.len - 1 do
		f (iget d.arr i)
	done

let iteri f d =
	for i = 0 to d.len - 1 do
		f i (iget d.arr i)
	done

let filter f d =
	let l = d.len in
	let a = imake 0 l in
	let a2 = d.arr in
	let p = ref 0 in
	for i = 0 to l - 1 do
		let x = iget a2 i in
		if f x then begin
			iset a !p x;
			incr p;
		end;
	done;
	d.len <- !p;
	d.arr <- a

let index_of f d =
	let rec loop i =
		if i >= d.len then
			raise Not_found
		else
			if f (iget d.arr i) then
				i
			else
				loop (i+1)
	in
	loop 0

let map f src =
	let arr = imake 0 src.len in
	for i = 0 to src.len - 1 do
		iset arr i (f (iget src.arr i))
	done;
	{
		resize = src.resize;
		len = src.len;
		arr = arr;
	}

let mapi f src =
	let arr = imake 0 src.len in
	for i = 0 to src.len - 1 do
		iset arr i (f i (iget src.arr i))
	done;
	{
		resize = src.resize;
		len = src.len;
		arr = arr;
	}

let fold_left f x a =
	let rec loop idx x =
		if idx >= a.len then x else loop (idx + 1) (f x (iget a.arr idx))
	in
	loop 0 x

let fold_right f a x =
	let rec loop idx x =
		if idx < 0 then x
		else loop (idx - 1) (f (iget a.arr idx) x)
	in
	loop (a.len - 1) x

let enum d =
	let rec make start =
		let idxref = ref 0 in
		let next () =
			if !idxref >= d.len then
				raise Enum.No_more_elements
			else
				let retval = iget d.arr !idxref in
				incr idxref;
				retval
		and count () =
			if !idxref >= d.len then 0
			else d.len - !idxref
		and clone () =
			make !idxref
		in
		Enum.make ~next:next ~count:count ~clone:clone
	in
	make 0

let of_enum e =
	if Enum.fast_count e then begin
		let c = Enum.count e in
		let arr = imake 0 c in
		Enum.iteri (fun i x -> iset arr i x) e;
		{
			resize = default_resizer;
			len = c;
			arr = arr;
		}
	end else
		let d = make 0 in
		Enum.iter (add d) e;
		d

let unsafe_get a n =
	iget a.arr n

let unsafe_set a n x =
	iset a.arr n x
