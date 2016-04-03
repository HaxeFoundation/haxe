(*
 * SMap - Fast String maps
 * Copyright (C) 2016 Nicolas Cannasse
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)

type 'a t =
  | Empty
  | Node of 'a t * string * 'a * 'a t * int

let height = function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0

type bucket =
	| Eof
	| Entry of string * int * bucket

let size = 256
let string_h = Array.make size Eof
let string_a = DynArray.create()

let string_id s = s
let id_to_string x = x

let make l k v r = Node (l, k, v, r, max (height l) (height r) + 1)

let bal l k v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lv, lr, _) ->
        if height ll >= height lr then make ll lk lv (make lr k v r)
        else
          (match lr with
          | Node (lrl, lrk, lrv, lrr, _) ->
              make (make ll lk lv lrl) lrk lrv (make lrr k v r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rv, rr, _) ->
        if height rr >= height rl then make (make l k v rl) rk rv rr
        else
          (match rl with
          | Node (rll, rlk, rlv, rlr, _) ->
              make (make l k v rll) rlk rlv (make rlr rk rv rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, v, r, max hl hr + 1)

let rec min_binding = function
  | Node (Empty, k, v, _, _) -> k, v
  | Node (l, _, _, _, _) -> min_binding l
  | Empty -> raise Not_found

let rec remove_min_binding = function
  | Node (Empty, _, _, r, _) -> r
  | Node (l, k, v, r, _) -> bal (remove_min_binding l) k v r
  | Empty -> invalid_arg "PMap.remove_min_binding"

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k, v = min_binding t2 in
      bal t1 k v (remove_min_binding t2)

let empty = Empty

let is_empty x = x = Empty

let add x d map =
  let x = string_id x in
  let rec loop = function
    | Node (l, k, v, r, h) ->
        if x = k then Node (l, x, d, r, h)
        else if x < k then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> Node (Empty, x, d, Empty, 1) in
  loop map

let find x map =
  let x = string_id x in
  let rec loop = function
    | Node (l, k, v, r, _) ->
        if x < k then loop l
        else if x > k then loop r
        else v
    | Empty -> raise Not_found in
  loop map

let remove x map =
  let x = string_id x in
  let rec loop = function
    | Node (l, k, v, r, _) ->
        if x = k then merge l r else
        if x < k then bal (loop l) k v r else bal l k v (loop r)
    | Empty -> Empty in
  loop map

let mem x map =
  let x = string_id x in
  let rec loop = function
    | Node (l, k, v, r, _) ->
        x = k || loop (if x < k then l else r)
    | Empty -> false in
  loop map

let exists = mem

let iter f map =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, v, r, _) -> loop l; f (id_to_string k) v; loop r in
  loop map

let map f map =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) ->
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f v, r, h) in
  loop map

let mapi f map =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) ->
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f (id_to_string k) v, r, h) in
  loop map

let fold f map acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, v, r, _) ->
	  loop (f v (loop acc l)) r in
  loop acc map

let foldi f map acc =
  let rec loop acc = function
    | Empty -> acc
	| Node (l, k, v, r, _) ->
       loop (f (id_to_string k) v (loop acc l)) r in
  loop acc map

