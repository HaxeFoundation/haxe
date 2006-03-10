(* 
 * Enum - Enumeration over abstract collection of elements.
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

type 'a t = {
	mutable count : unit -> int;
	mutable next : unit -> 'a;
	mutable clone : unit -> 'a t;
	mutable fast : bool;
}

(* raised by 'next' functions, should NOT go outside the API *)
exception No_more_elements

let _dummy () = assert false

let make ~next ~count ~clone =
	{
		count = count;
		next = next;
		clone = clone;
		fast = true;
	}

let rec init n f =
	if n < 0 then invalid_arg "Enum.init";
	let count = ref n in
	{
		count = (fun () -> !count);
		next = (fun () ->
			match !count with
			| 0 -> raise No_more_elements
			| _ ->
				decr count;
				f (n - 1 - !count));
		clone = (fun () -> init !count f);
		fast = true;
	}			

let rec empty () =
	{
		count = (fun () -> 0);
		next = (fun () -> raise No_more_elements);
		clone = (fun () -> empty());
		fast = true;
	}

type 'a _mut_list = {
	hd : 'a;
	mutable tl : 'a _mut_list;
}

let force t =
	let rec clone enum count =
		let enum = ref !enum
		and	count = ref !count in
		{
			count = (fun () -> !count);
			next = (fun () ->
				match !enum with
				| [] -> raise No_more_elements
				| h :: t -> decr count; enum := t; h);
			clone = (fun () ->
				let enum = ref !enum
				and count = ref !count in
				clone enum count);
			fast = true;
		}
	in
	let count = ref 0 in
	let _empty = Obj.magic [] in
	let rec loop dst =
		let x = { hd = t.next(); tl = _empty } in
		incr count;
		dst.tl <- x;
		loop x
	in
	let enum = ref _empty  in 
	(try
		enum := { hd = t.next(); tl = _empty };
		incr count;
		loop !enum;
	with No_more_elements -> ());
	let tc = clone (Obj.magic enum) count in
	t.clone <- tc.clone;
	t.next <- tc.next;
	t.count <- tc.count;
	t.fast <- true

let from f =
	let e = {
		next = f;
		count = _dummy;
		clone = _dummy;
		fast = false;
	} in
	e.count <- (fun () -> force e; e.count());
	e.clone <- (fun () -> force e; e.clone());
	e

let from2 next clone =
	let e = {
		next = next;
		count = _dummy;
		clone = clone;
		fast = false;
	} in
	e.count <- (fun () -> force e; e.count());
	e

let get t =
	try
		Some (t.next())
	with
		No_more_elements -> None

let push t e =
	let rec make t =
		let fnext = t.next in
		let fcount = t.count in
		let fclone = t.clone in
		let next_called = ref false in
		t.next <- (fun () ->
			next_called := true;
			t.next <- fnext;
			t.count <- fcount;
			t.clone <- fclone;
			e);
		t.count <- (fun () ->
			let n = fcount() in
			if !next_called then n else n+1);
		t.clone <- (fun () ->
			let tc = fclone() in
			if not !next_called then make tc;
			tc);
	in
	make t

let peek t =
	match get t with
	| None -> None
	| Some x ->
		push t x;
		Some x

let junk t =
	try
		ignore(t.next())
	with
		No_more_elements -> ()

let is_empty t =
	if t.fast then
		t.count() = 0
	else
		peek t = None

let count t =
	t.count()

let fast_count t =
	t.fast

let clone t =
	t.clone()

let iter f t =
	let rec loop () =
		f (t.next());
		loop();
	in
	try
		loop();
	with
		No_more_elements -> ()

let iteri f t =
	let rec loop idx =
		f idx (t.next());
		loop (idx+1);
	in
	try
		loop 0;
	with
		No_more_elements -> ()

let iter2 f t u =
	let push_t = ref None in
	let rec loop () =
		push_t := None;
		let e = t.next() in
		push_t := Some e;
		f e (u.next());
		loop ()
	in
	try
		loop ()
	with
		No_more_elements ->
			match !push_t with
			| None -> ()
			| Some e ->
				push t e

let iter2i f t u =
	let push_t = ref None in
	let rec loop idx =
		push_t := None;
		let e = t.next() in
		push_t := Some e;
		f idx e (u.next());
		loop (idx + 1)
	in
	try
		loop 0
	with
		No_more_elements ->
			match !push_t with
			| None -> ()
			| Some e -> push t e

let fold f init t =
	let acc = ref init in
	let rec loop() =
		acc := f (t.next()) !acc;
		loop()
	in
	try
		loop()
	with
		No_more_elements -> !acc

let foldi f init t =
	let acc = ref init in
	let rec loop idx =
		acc := f idx (t.next()) !acc;
		loop (idx + 1)
	in
	try
		loop 0
	with
		No_more_elements -> !acc

let fold2 f init t u =
	let acc = ref init in
	let push_t = ref None in
	let rec loop() =
		push_t := None;
		let e = t.next() in
		push_t := Some e;
		acc := f e (u.next()) !acc;
		loop()
	in
	try
		loop()
	with
		No_more_elements ->
			match !push_t with
			| None -> !acc
			| Some e ->
				push t e;
				!acc

let fold2i f init t u =
	let acc = ref init in
	let push_t = ref None in
	let rec loop idx =
		push_t := None;
		let e = t.next() in
		push_t := Some e;
		acc := f idx e (u.next()) !acc;
		loop (idx + 1)
	in
	try
		loop 0
	with
		No_more_elements ->
			match !push_t with
			| None -> !acc
			| Some e ->
				push t e;
				!acc

let find f t =
	let rec loop () =
		let x = t.next() in
		if f x then x else loop()
	in
	try
		loop()
	with
		No_more_elements -> raise Not_found

let rec map f t =
	{
		count = t.count;
		next = (fun () -> f (t.next()));
		clone = (fun () -> map f (t.clone()));
		fast = t.fast;
	}

let rec mapi f t =
	let idx = ref (-1) in
	{
		count = t.count;
		next = (fun () -> incr idx; f !idx (t.next()));
		clone = (fun () -> mapi f (t.clone()));
		fast = t.fast;
	}

let rec filter f t =
	let rec next() =
		let x = t.next() in
		if f x then x else next()
	in
	from2 next (fun () -> filter f (t.clone()))

let rec filter_map f t =
    let rec next () =
        match f (t.next()) with
        | None -> next()
        | Some x -> x
    in
	from2 next (fun () -> filter_map f (t.clone()))

let rec append ta tb = 
	let t = {
		count = (fun () -> ta.count() + tb.count());
		next = _dummy;
		clone = (fun () -> append (ta.clone()) (tb.clone()));
		fast = ta.fast && tb.fast;
	} in
	t.next <- (fun () ->
		try
			ta.next()
		with
			No_more_elements ->
				(* add one indirection because tb can mute *)
				t.next <- (fun () -> tb.next());
				t.count <- (fun () -> tb.count());
				t.clone <- (fun () -> tb.clone());
				t.fast <- tb.fast;
				t.next()
	);
	t

let rec concat t =
	let concat_ref = ref _dummy in
	let rec concat_next() =
		let tn = t.next() in
		concat_ref := (fun () ->
			try
				tn.next()
			with
				No_more_elements ->
					concat_next());
		!concat_ref ()
	in
	concat_ref := concat_next;
	from2 (fun () -> !concat_ref ()) (fun () -> concat (t.clone()))
