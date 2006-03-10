(*
 * ExtList - additional and modified functions for lists.
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

module List = struct

exception Empty_list
exception Invalid_index of int
exception Different_list_size of string

include List

(* Thanks to Jacques Garrigue for suggesting the following structure *)
type 'a mut_list =  {
	hd: 'a; 
	mutable tl: 'a list
}
external inj : 'a mut_list -> 'a list = "%identity"


let dummy_node () = { hd = Obj.magic (); tl = [] }

let hd = function
	| [] -> raise Empty_list
	| h :: t -> h

let tl = function
	| [] -> raise Empty_list
	| h :: t -> t

let nth l index =
	if index < 0 then raise (Invalid_index index);
	let rec loop n = function
		| [] -> raise (Invalid_index index);
		| h :: t -> 
			if n = 0 then h else loop (n - 1) t
	in
	loop index l

let append l1 l2 =
	match l1 with
	| [] -> l2
	| h :: t ->
		let rec loop dst = function
		| [] ->
			dst.tl <- l2
		| h :: t ->
			let cell = { hd = h; tl = [] } in
			dst.tl <- inj cell;
			loop cell t
		in
		let r = { hd = h; tl = [] } in
		loop r t;
		inj r

let rec flatten l =
	let rec inner dst = function
		| [] -> dst
		| h :: t ->
			let r = { hd = h; tl = [] } in
			dst.tl <- inj r;
			inner r t
	in
	let rec outer dst = function
		| [] -> ()
		| h :: t -> outer (inner dst h) t
	in
	let r = dummy_node () in
	outer r l;
	r.tl

let concat = flatten

let map f = function
	| [] -> []
	| h :: t ->
		let rec loop dst = function
		| [] -> ()
		| h :: t ->
			let r = { hd = f h; tl = [] } in
			dst.tl <- inj r;
			loop r t
		in
		let r = { hd = f h; tl = [] } in
		loop r t;
		inj r

let rec drop n = function
	| _ :: l when n > 0 -> drop (n-1) l
	| l -> l

let take n l =
	let rec loop n dst = function
		| h :: t when n > 0 ->
			let r = { hd = h; tl = [] } in
			dst.tl <- inj r;
			loop (n-1) r t
		| _ ->
			()
	in
	let dummy = dummy_node() in
	loop n dummy l;
	dummy.tl

(* takewhile and dropwhile by Richard W.M. Jones. *)
let rec takewhile f = function
  | [] -> []
  | x :: xs when f x -> x :: takewhile f xs
  | _ -> []

let rec dropwhile f = function
  | [] -> []
  | x :: xs when f x -> dropwhile f xs
  | xs -> xs


let rec unique ?(cmp = ( = )) l =
	let rec loop dst = function
		| [] -> ()
		| h :: t ->
			match exists (cmp h) t with
			| true -> loop dst t
			| false ->
				let r = { hd =  h; tl = [] }  in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node() in
	loop dummy l;
	dummy.tl

let filter_map f l =
	let rec loop dst = function
		| [] -> ()
		| h :: t ->
			match f h with
			| None -> loop dst t
			| Some x ->
				let r = { hd = x; tl = [] }  in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node() in
	loop dummy l;
	dummy.tl
	
let fold_right_max = 1000

let fold_right f l init =
	let rec tail_loop acc = function
		| [] -> acc
		| h :: t -> tail_loop (f h acc) t
	in
	let rec loop n = function
		| [] -> init
		| h :: t ->
			if n < fold_right_max then
				f h (loop (n+1) t)
			else
				f h (tail_loop init (rev t))
	in
	loop 0 l

let map2 f l1 l2 =
	let rec loop dst src1 src2 =
		match src1, src2 with
			| [], [] -> ()
			| h1 :: t1, h2 :: t2 ->
				let r = { hd = f h1 h2; tl = [] } in
				dst.tl <- inj r;
				loop r t1 t2
			| _ -> raise (Different_list_size "map2")
	in
	let dummy = dummy_node () in
	loop dummy l1 l2;
	dummy.tl

let rec iter2 f l1 l2 =
	match l1, l2 with
	| [], [] -> ()
	| h1 :: t1, h2 :: t2 -> f h1 h2; iter2 f t1 t2
	| _ -> raise (Different_list_size "iter2")

let rec fold_left2 f accum l1 l2 =
	match l1, l2 with
	| [], [] -> accum
	| h1 :: t1, h2 :: t2 -> fold_left2 f (f accum h1 h2) t1 t2
	| _ -> raise (Different_list_size "fold_left2")

let fold_right2 f l1 l2 init =
	let rec tail_loop acc l1 l2 =
		match l1, l2 with
		| [] , [] -> acc
		| h1 :: t1 , h2 :: t2 -> tail_loop (f h1 h2 acc) t1 t2
		| _ -> raise (Different_list_size "fold_right2")
	in
	let rec loop n l1 l2 =
		match l1, l2 with
		| [], [] -> init
		| h1 :: t1, h2 :: t2 ->
			if n < fold_right_max then
				f h1 h2 (loop (n+1) t1 t2)
			else
				f h1 h2 (tail_loop init (rev t1) (rev t2))
		| _ -> raise (Different_list_size "fold_right2")
	in
	loop 0 l1 l2

let for_all2 p l1 l2 =
	let rec loop l1 l2 =
		match l1, l2 with
		| [], [] -> true
		| h1 :: t1, h2 :: t2 -> if p h1 h2 then loop t1 t2 else false
		| _ -> raise (Different_list_size "for_all2")
	in
	loop l1 l2

let exists2 p l1 l2 =
	let rec loop l1 l2 =
		match l1, l2 with
			| [], [] -> false
			| h1 :: t1, h2 :: t2 -> if p h1 h2 then true else loop t1 t2
			| _ -> raise (Different_list_size "exists2")
	in
	loop l1 l2

let remove_assoc x lst = 
	let rec loop dst = function
		| [] -> ()
		| (a, _ as pair) :: t ->
			if a = x then
				dst.tl <- t
			else
				let r = { hd = pair; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node () in
	loop dummy lst;
	dummy.tl

let remove_assq x lst = 
	let rec loop dst = function
		| [] -> ()
		| (a, _ as pair) :: t ->
			if a == x then
				dst.tl <- t
			else
				let r = { hd =  pair; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node() in
	loop dummy lst;
	dummy.tl

let rfind p l = find p (rev l)

let find_all p l = 
	let rec findnext dst = function
		| [] -> ()
		| h :: t -> 
			if p h then
				let r = { hd = h; tl = [] } in
				dst.tl <- inj r;
				findnext r t
			else
				findnext dst t
	in
	let dummy = dummy_node () in
	findnext dummy l;
	dummy.tl

let rec findi p l =
	let rec loop n = function
		| [] -> raise Not_found
		| h :: t ->
			if p n h then (n,h) else loop (n+1) t
	in
	loop 0 l

let filter = find_all

let partition p lst = 
	let rec loop yesdst nodst = function
		| [] -> ()
		| h :: t ->
			let r = { hd = h; tl = [] } in
			if p h then
				begin
					yesdst.tl <- inj r;
					loop r nodst t
				end
			else
				begin
					nodst.tl <- inj r;
					loop yesdst r t
				end
	in
	let yesdummy = dummy_node()
	and nodummy = dummy_node()
	in
	loop yesdummy nodummy lst;
	yesdummy.tl, nodummy.tl

let split lst =
	let rec loop adst bdst = function
		| [] -> ()
		| (a, b) :: t -> 
			let x = { hd = a; tl = [] } 
			and y = { hd = b; tl = [] } in
			adst.tl <- inj x;
			bdst.tl <- inj y;
			loop x y t
	in
	let adummy = dummy_node ()
	and bdummy = dummy_node ()
	in
	loop adummy bdummy lst;
	adummy.tl, bdummy.tl

let combine l1 l2 =
	let rec loop dst l1 l2 =
		match l1, l2 with
		| [], [] -> ()
		| h1 :: t1, h2 :: t2 -> 
			let r = { hd = h1, h2; tl = [] } in
			dst.tl <- inj r;
			loop r t1 t2
		| _, _ -> raise (Different_list_size "combine")
	in
	let dummy = dummy_node () in
	loop dummy l1 l2;
	dummy.tl

let sort ?(cmp=compare) = List.sort cmp

let rec init size f =
	if size = 0 then [] 
	else if size < 0 then invalid_arg "ExtList.init"
	else
		let rec loop dst n =
			if n < size then
				let r = { hd = f n; tl = [] } in
				dst.tl <- inj r;
				loop r (n+1)
		in
		let r = { hd = f 0; tl = [] } in
		loop r 1;
		inj r

(* make by Richard W.M. Jones. *)
let make i x =
  if i < 0 then invalid_arg "ExtList.List.make";
  let rec make' x = function
    | 0 -> []
    | i -> x :: make' x (i-1)
  in
  make' x i

let mapi f = function
	| [] -> []
	| h :: t ->
		let rec loop dst n = function
			| [] -> ()
			| h :: t -> 
				let r = { hd = f n h; tl = [] } in
				dst.tl <- inj r;
				loop r (n+1) t
		in	
		let r = { hd = f 0 h; tl = [] } in
		loop r 1 t;
		inj r

let iteri f l = 
	let rec loop n = function
		| [] -> ()
		| h :: t ->
			f n h;
			loop (n+1) t
	in
	loop 0 l

let first = hd

let rec last = function
	| [] -> raise Empty_list
	| h :: [] -> h
	| _ :: t -> last t

let split_nth index = function
	| [] -> if index = 0 then [],[] else raise (Invalid_index index)
	| (h :: t as l) ->
		if index = 0 then [],l
		else if index < 0 then raise (Invalid_index index)
		else
			let rec loop n dst l =
				if n = 0 then l else
				match l with
				| [] -> raise (Invalid_index index)
				| h :: t ->
					let r = { hd =  h; tl = [] } in
					dst.tl <- inj r;
					loop (n-1) r t 
			in
			let r = { hd = h; tl = [] } in
			inj r, loop (index-1) r t

let find_exc f e l =
	try
		find f l
	with
		Not_found -> raise e

let remove l x =
	let rec loop dst = function
		| [] -> raise Not_found
		| h :: t ->
			if x = h then 
				dst.tl <- t
			else
				let r = { hd = h; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node () in
	loop dummy l;
	dummy.tl

let rec remove_if f lst =
	let rec loop dst = function
		| [] -> ()
		| x :: l ->
			if f x then
				dst.tl <- l
			else
				let r = { hd = x; tl = [] } in
				dst.tl <- inj r;
				loop r l
	in
	let dummy = dummy_node () in
	loop dummy lst;
	dummy.tl

let rec remove_all l x =
	let rec loop dst = function
		| [] -> ()
		| h :: t ->
			if x = h then
				loop dst t
			else
				let r = { hd = h; tl = [] } in
				dst.tl <- inj r;
				loop r t
	in
	let dummy = dummy_node () in
	loop dummy l;
	dummy.tl

let enum l =
	let rec make lr count =
		Enum.make
			~next:(fun () ->
				match !lr with
				| [] -> raise Enum.No_more_elements
				| h :: t ->
					decr count;
					lr := t;
					h
			)
			~count:(fun () ->
				if !count < 0 then count := length !lr;
				!count
			)
			~clone:(fun () ->
				make (ref !lr) (ref !count)
			)
	in
	make (ref l) (ref (-1))

let of_enum e =
	let h = dummy_node() in
	let _ = Enum.fold (fun x acc ->
		let r = { hd = x; tl = [] }  in
		acc.tl <- inj r;
		r) h e in
	h.tl

end

let ( @ ) = List.append
