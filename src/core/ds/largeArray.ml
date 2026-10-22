(* Arrays of any size that never empty the minor heap.

   [Array.make n v] of an array larger than [Max_young_wosize] (256 words)
   allocates it in the major heap and, when [v] is a block of the minor heap,
   first empties the minor heap so that the array does not point into it. That
   is a stop-the-world minor collection of all the domains, and [Array.init],
   [Array.map], [Array.of_list] and [DynArray.to_array] all go through it with
   their first element, usually just allocated. These versions create the
   array with an immediate value and set the elements after (each with the
   write barrier). The elements must not be floats: a [float array] would not
   be flat. *)

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
