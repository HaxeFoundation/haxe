type t = {
	mutable arr : string array;
	mutable length : int;
}

let create length = {
	arr = Array.make length "";
	length = 0;
}

let length d =
	d.length

let add d s =
	let length = Array.length d.arr in
	if d.length = length then begin
		let new_arr = Array.make (length * 2) "" in
		Array.blit d.arr 0 new_arr 0 length;
		d.arr <- new_arr;
	end;
	d.arr.(d.length) <- s;
	d.length <- d.length + 1

let iter d f =
	for i = 0 to d.length - 1 do
		f (Array.unsafe_get d.arr i)
	done