open Globals

let byte_offset s chars =
	let len = String.length s in
	let rec loop i n =
		if n = chars || i >= len then i
		else
			let i = i + 1 in
			let rec skip i = if i < len && (Char.code s.[i]) land 0xC0 = 0x80 then skip (i + 1) else i in
			loop (skip i) (n + 1)
	in
	loop 0 0

let byte_range s p =
	let bmin = byte_offset s p.pmin in
	bmin,bmin + byte_offset (String.sub s bmin (String.length s - bmin)) (p.pmax - p.pmin)

let read p =
	try
		if p.pmin < 0 || p.pmin >= p.pmax then None
		else begin
			let s = Std.input_file ~bin:true p.pfile in
			let bmin,bmax = byte_range s p in
			if bmax > String.length s then None else Some (String.sub s bmin (bmax - bmin))
		end
	with _ ->
		None
