
let inflate i =
	let available = ref (IO.available i) in
	let buf = Buffer.create 0 in
	let refill str =
		let len = String.length str in
		if !available >= len then begin
			let data = IO.nread i len in
			String.blit data 0 str 0 len;
			available := !available - len;
			len
		end else begin
			let size = !available in
			let data = IO.nread i size in
			String.blit data 0 str 0 size;
			available := 0;
			size
		end
	in
	let flush str n =
		Buffer.add_substring buf str 0 n
	in
	Zlib.uncompress ~header:true refill flush;	
	IO.input_string (Buffer.contents buf)

let deflate o =
	let buf = Buffer.create 0 in
	let flush() =
		let data = Buffer.contents buf in
		let pos = ref 0 in
		let available = ref (String.length data) in
		let refill str =
			let len = String.length str in
			if !available >= len then begin
				String.blit data !pos str 0 len;
				available := !available - len;
				pos := !pos + len;
				len
			end else begin
				let size = !available in
				String.blit data !pos str 0 size;
				available := 0;
				pos := !pos + size;
				size
			end
		in
		let flush str n =
			if n = String.length str then
				IO.nwrite o str
			else
				IO.nwrite o (String.sub str 0 n)
		in
		Zlib.compress ~level:9 ~header:true refill flush;
		IO.flush o;
		Buffer.reset buf;
	in
	IO.create_out (Buffer.add_char buf) (Buffer.add_string buf) (fun () -> Buffer.length buf) flush flush

;;
Swf.__inflate := inflate;
Swf.__deflate := deflate;