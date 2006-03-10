(*
 * ExtList - additional and modified functions for lists.
 * Copyright (C) 2005 Richard W.M. Jones (rich @ annexia.org)
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

module Array = struct

include Array

let rev_in_place xs =
  let n = length xs in
  let j = ref (n-1) in
  for i = 0 to n/2-1 do
    let c = xs.(i) in
    xs.(i) <- xs.(!j);
    xs.(!j) <- c;
    decr j
  done

let rev xs =
  let ys = Array.copy xs in
  rev_in_place ys;
  ys

let for_all p xs =
  let n = length xs in
  let rec loop i =
    if i = n then true
    else if p xs.(i) then loop (succ i)
    else false
  in
  loop 0

let exists p xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if p xs.(i) then true
    else loop (succ i)
  in
  loop 0

let mem a xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if a = xs.(i) then true
    else loop (succ i)
  in
  loop 0

let memq a xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if a == xs.(i) then true
    else loop (succ i)
  in
  loop 0

let findi p xs =
  let n = length xs in
  let rec loop i =
    if i = n then raise Not_found
    else if p xs.(i) then i
    else loop (succ i)
  in
  loop 0

let find p xs = xs.(findi p xs)

(* Use of BitSet suggested by Brian Hurt. *)
let filter p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BitSet.create n in
  for i = 0 to n-1 do
    if p xs.(i) then BitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BitSet.count bs in
  let j = ref 0 in
  let xs' = init n'
    (fun _ ->
       (* Find the next set bit in the BitSet. *)
       while not (BitSet.is_set bs !j) do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  xs'

let find_all = filter

let partition p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in which final array. *)
  let bs = BitSet.create n in
  for i = 0 to n-1 do
    if p xs.(i) then BitSet.set bs i
  done;
  (* Allocate the final arrays and copy elements into them. *)
  let n1 = BitSet.count bs in
  let n2 = n - n1 in
  let j = ref 0 in
  let xs1 = init n1
    (fun _ ->
       (* Find the next set bit in the BitSet. *)
       while not (BitSet.is_set bs !j) do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  let j = ref 0 in
  let xs2 = init n2
    (fun _ ->
       (* Find the next clear bit in the BitSet. *)
       while BitSet.is_set bs !j do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  xs1, xs2

let enum xs =
  let rec make start xs =
    let n = length xs in
    Enum.make
      ~next:(fun () ->
	       if !start < n then (
		 let r = xs.(!start) in
		 incr start;
		 r
	       ) else
		 raise Enum.No_more_elements)
      ~count:(fun () ->
		n - !start)
      ~clone:(fun () ->
		let xs' = Array.sub xs !start (n - !start) in
		make (ref 0) xs')
  in
  make (ref 0) xs

let of_enum e =
  let n = Enum.count e in
  (* This assumes, reasonably, that init traverses the array in order. *)
  Array.init n
    (fun i ->
       match Enum.get e with
       | Some x -> x
       | None -> assert false)

end
