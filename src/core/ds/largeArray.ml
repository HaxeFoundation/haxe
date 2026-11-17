(* Arrays of any size that never empty the minor heap: they are created with an
   immediate value, then filled. The elements must not be floats. *)

let make_non_float n : 'a array = Array.make n (Obj.magic 0)

let of_dynarray d =
	let n = DynArray.length d in
	if n = 0 then [||] else begin
		let a = make_non_float n in
		for i = 0 to n - 1 do Array.unsafe_set a i (DynArray.unsafe_get d i) done;
		a
	end

let init n f =
	if n = 0 then [||] else begin
		let a = make_non_float n in
		for i = 0 to n - 1 do Array.unsafe_set a i (f i) done;
		a
	end

let map f a =
	init (Array.length a) (fun i -> f (Array.unsafe_get a i))

let of_list l =
	match l with
	| [] -> [||]
	| _ ->
		let a = make_non_float (List.length l) in
		List.iteri (fun i v -> Array.unsafe_set a i v) l;
		a
