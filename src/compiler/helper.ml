exception HelpMessage of string

let is_debug_run = try Sys.getenv "HAXEDEBUG" = "1" with _ -> false

let start_time = ref (Timer.get_time())

let prompt = ref false

let expand_env ?(h=None) path  =
	let r = Str.regexp "%\\([A-Za-z0-9_]+\\)%" in
	Str.global_substitute r (fun s ->
		let key = Str.matched_group 1 s in
		try
			Sys.getenv key
		with Not_found -> try
			match h with
			| None -> raise Not_found
			| Some h -> Hashtbl.find h key
		with Not_found ->
			"%" ^ key ^ "%"
	) path

let unquote v =
	let len = String.length v in
	if len > 0 then
		match v.[0], v.[len - 1] with
			| '"', '"'
			| '\'', '\'' -> String.sub v 1 (len - 2)
			| _ -> v
	else v

let parse_hxml_data data =
	let lines = Str.split (Str.regexp "[\r\n]+") data in
	List.concat (List.map (fun l ->
		let l = unquote (ExtString.String.strip l) in
		if l = "" || l.[0] = '#' then
			[]
		else if l.[0] = '-' then
			try
				let a, b = ExtString.String.split l " " in
				[unquote a; unquote (ExtString.String.strip b)]
			with
				_ -> [l]
		else
			[l]
	) lines)

let parse_hxml file =
	let ch = IO.input_channel (try open_in_bin file with _ -> raise Not_found) in
	let data = IO.read_all ch in
	IO.close_in ch;
	parse_hxml_data data

let parse_host_port hp =
	let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
	let port = try int_of_string port with _ -> raise (Arg.Bad "Invalid port") in
	host, port