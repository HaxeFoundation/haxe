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

(* Source: http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance#OCaml *)
let levenshtein a b =
	let x = Array.init (String.length a) (fun i -> a.[i]) in
	let y = Array.init (String.length b) (fun i -> b.[i]) in
	let minimum (x:int) y z =
		let m' (a:int) b = if a < b then a else b in
		m' (m' x y) z
	in
	let init_matrix n m =
		let init_col = Array.init m in
			Array.init n (function
			| 0 -> init_col (function j -> j)
			| i -> init_col (function 0 -> i | _ -> 0)
		)
	in
	match Array.length x, Array.length y with
		| 0, n -> n
		| m, 0 -> m
		| m, n ->
			let matrix = init_matrix (m + 1) (n + 1) in
			for i = 1 to m do
				let s = matrix.(i) and t = matrix.(i - 1) in
				for j = 1 to n do
					let cost = abs (compare x.(i - 1) y.(j - 1)) in
					s.(j) <- minimum (t.(j) + 1) (s.(j - 1) + 1) (t.(j - 1) + cost)
				done
			done;
			matrix.(m).(n)

let filter_similar f cl =
	let rec loop sl = match sl with
		| (x,i) :: sl when f x i -> x :: loop sl
		| _ -> []
	in
	loop cl

let get_similar s sl =
	if sl = [] then [] else
	let cl = List.map (fun s2 -> s2,levenshtein s s2) sl in
	let cl = List.sort (fun (_,c1) (_,c2) -> compare c1 c2) cl in
	let cl = filter_similar (fun s2 i -> i <= (min (String.length s) (String.length s2)) / 3) cl in
	cl

let string_error_raise s sl msg =
	if sl = [] then msg else
	let cl = get_similar s sl in
	match cl with
		| [] -> raise Not_found
		| [s] -> Printf.sprintf "%s (Suggestion: %s)" msg s
		| sl -> Printf.sprintf "%s (Suggestions: %s)" msg (String.concat ", " sl)

let string_error s sl msg =
	try string_error_raise s sl msg
	with Not_found -> msg