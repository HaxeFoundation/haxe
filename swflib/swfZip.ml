
let inflate i =
	let str = IO.read_all i in
	Printf.printf "SIZE = %d\n" (String.length str);
	IO.input_string (Extc.unzip str)

let deflate o =
	let buf = Buffer.create 0 in
	let flush() =
		let data = Buffer.contents buf in
		IO.nwrite o (Extc.zip data);
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