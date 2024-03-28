module StringHashtbl = Hashtbl.Make(struct
	type t = string

	let equal =
		String.equal

	let hash s =
		(* What's the best here? *)
		Hashtbl.hash s
end)

type t = {
	lut : int StringHashtbl.t;
	items : StringDynArray.t;
	mutable closed : bool;
}

let create () = {
	lut = StringHashtbl.create 16;
	items = StringDynArray.create 16;
	closed = false;
}

let add sp s =
	assert (not sp.closed);
	let index = StringDynArray.length sp.items in
	StringHashtbl.add sp.lut s index;
	StringDynArray.add sp.items s;
	index

let get sp s =
	StringHashtbl.find sp.lut s

let get_or_add sp s =
	try
		get sp s
	with Not_found ->
		add sp s

let finalize sp =
	assert (not sp.closed);
	sp.closed <- true;
	sp.items