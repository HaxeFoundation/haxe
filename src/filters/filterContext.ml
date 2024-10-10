let with_timer detail_times debug label identifier f =
	let label = match detail_times,identifier with
		| 0,_ -> ["filters"]
		| 1,_ -> "filters" :: label :: []
		| _,Some identifier -> "filters" :: label :: identifier :: []
		| _ -> ["filters"]
	in
	if debug then print_endline (Printf.sprintf "Running filter: %s" (String.concat "." label));
	let timer = Timer.timer label in
	Std.finally timer f ()