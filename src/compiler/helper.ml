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
