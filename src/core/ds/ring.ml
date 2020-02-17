type 'a t = {
	values : 'a array;
	mutable index : int;
	mutable num_filled : int;
}

let create len x = {
	values = Array.make len x;
	index = 0;
	num_filled = 0;
}

let push r x =
	r.values.(r.index) <- x;
	r.num_filled <- r.num_filled + 1;
	if r.index = Array.length r.values - 1 then begin
		r.index <- 0;
	end else
		r.index <- r.index + 1

let iter r f =
	let len = Array.length r.values in
	for i = 0 to len - 1 do
		let off = r.index + i in
		let off = if off >= len then off - len else off in
		f r.values.(off)
	done

let fold r acc f =
	let len = Array.length r.values in
	let rec loop i acc =
		if i = len then
			acc
		else begin
			let off = r.index + i in
			let off = if off >= len then off - len else off in
			loop (i + 1) (f acc r.values.(off))
		end
	in
	loop 0 acc

let is_filled r =
	r.num_filled >= Array.length r.values

let reset_filled r =
	r.num_filled <- 0
