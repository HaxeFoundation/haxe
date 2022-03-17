
let mk_length_prefixed_communication allow_nonblock chin chout =
	let sin = Unix.descr_of_in_channel chin in
	let chin = IO.input_channel chin in
	let chout = IO.output_channel chout in

	let bout = Buffer.create 0 in

	let block () = Unix.clear_nonblock sin in
	let unblock () = Unix.set_nonblock sin in

	let read_nonblock _ =
        let len = IO.read_i32 chin in
        Some (IO.really_nread_string chin len)
	in
	let read = if allow_nonblock then fun do_block ->
		if do_block then begin
			block();
			read_nonblock true;
		end else begin
			let c0 =
				unblock();
				try
					Some (IO.read_byte chin)
				with
				| Sys_blocked_io
				(* TODO: We're supposed to catch Sys_blocked_io only, but that doesn't work on my PC... *)
				| Sys_error _ ->
					None
			in
			begin match c0 with
			| Some c0 ->
				block(); (* We got something, make sure we block until we're done. *)
				let c1 = IO.read_byte chin in
				let c2 = IO.read_byte chin in
				let c3 = IO.read_byte chin in
				let len = c3 lsl 24 + c2 lsl 16 + c1 lsl 8 + c0 in
				Some (IO.really_nread_string chin len)
			| None ->
				None
			end
		end
	else read_nonblock in

	let write = Buffer.add_string bout in

	let close = fun() ->
		IO.write_i32 chout (Buffer.length bout);
		IO.nwrite_string chout (Buffer.contents bout);
		IO.flush chout
	in

	fun () ->
		Buffer.clear bout;
		allow_nonblock, read, write, close

let ssend sock str =
	let rec loop pos len =
		if len = 0 then
			()
		else
			let s = Unix.send sock str pos len [] in
			loop (pos + s) (len - s)
	in
	loop 0 (Bytes.length str)

(* The accept-function to wait for a stdio connection. *)
let init_wait_stdio() =
	set_binary_mode_in stdin true;
	set_binary_mode_out stderr true;
	mk_length_prefixed_communication false stdin stderr

(* The connect function to connect to [host] at [port] and send arguments [args]. *)
let do_connect host port args =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port));
	let rec display_stdin args =
		match args with
		| [] -> ""
		| "-D" :: ("display_stdin" | "display-stdin") :: _ ->
			let accept = init_wait_stdio() in
			let _, read, _, _ = accept() in
			Option.default "" (read true)
		| _ :: args ->
			display_stdin args
	in
	let args = ("--cwd " ^ Unix.getcwd()) :: args in
	let s = (String.concat "" (List.map (fun a -> a ^ "\n") args)) ^ (display_stdin args) in
	ssend sock (Bytes.of_string (s ^ "\000"));
	let has_error = ref false in
	let rec print line =
		match (if line = "" then '\x00' else line.[0]) with
		| '\x01' ->
			print_string (String.concat "\n" (List.tl (ExtString.String.nsplit line "\x01")));
			flush stdout
		| '\x02' ->
			has_error := true;
		| _ ->
			prerr_endline line;
	in
	let buf = Buffer.create 0 in
	let process() =
		let lines = ExtString.String.nsplit (Buffer.contents buf) "\n" in
		(* the last line ends with \n *)
		let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
		List.iter print lines;
	in
	let tmp = Bytes.create 1024 in
	let rec loop() =
		let b = Unix.recv sock tmp 0 1024 [] in
		Buffer.add_subbytes buf tmp 0 b;
		if b > 0 then begin
			if Bytes.get tmp (b - 1) = '\n' then begin
				process();
				Buffer.reset buf;
			end;
			loop();
		end
	in
	loop();
	process();
	if !has_error then exit 1
