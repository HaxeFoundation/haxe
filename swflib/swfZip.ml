
let inflate i =
	let buf = Buffer.create 0 in
	let refill str =
		try
			let len = String.length str in
			let data = IO.nread i len in
			let len = String.length data in
			String.blit data 0 str 0 len;
			len
		with
			IO.No_more_input -> 0
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
	IO.create_out 
		~write:(Buffer.add_char buf)
		~output:(fun s p l -> Buffer.add_substring buf s p l; l)
		~flush
		~close:(fun () -> flush(); IO.close_out o)

;;
Swf.__inflate := inflate;
Swf.__deflate := deflate;