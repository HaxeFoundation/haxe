open StringHelper

let get_path_parts f =
	(*
		this function is quite weird: it tries to determine whether the given
		argument is a .hx file path with slashes or a dotted module path and
		based on that it returns path "parts", which are basically a list of
		either folders or packages (which are folders too) appended by the module name

		TODO: i started doubting my sanity while writing this comment, let's somehow
		refactor this stuff so it doesn't mix up file and module paths and doesn't introduce
		the weird "path part" entity.
	*)
	let l = String.length f in
	if l > 3 && (String.sub f (l-3) 3) = ".hx" then
		let f = String.sub f 0 (l-3) in (* strip the .hx *)
		ExtString.String.nsplit (String.concat "/" (ExtString.String.nsplit f "\\")) "/" (* TODO: wouldn't it be faster to Str.split here? *)
	else
		ExtString.String.nsplit f "."

let parse_path f =
	let cl = get_path_parts f in
	let error msg =
		let msg = "Could not process argument " ^ f ^ "\n" ^ msg in
		failwith msg
	in
	let invalid_char x =
		for i = 1 to String.length x - 1 do
			match x.[i] with
			| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '.' -> ()
			| c -> error ("invalid character: " ^ (String.make 1 c))
		done
	in
	let rec loop = function
		| [] ->
			error "empty part"
		| [x] ->
			invalid_char x;
			[],x
		| x :: l ->
			if String.length x = 0 then
				error "empty part"
			else if (x.[0] < 'a' || x.[0] > 'z') && x.[0] <> '_' then
				error "Package name must start with a lower case character";
			invalid_char x;
			let path,name = loop l in
			x :: path,name
	in
	loop cl

let parse_type_path s =
	let pack,name = parse_path s in
	check_uppercase name;
	pack,name

let path_regex = Str.regexp "[/\\]+"
let normalize_path path =
	let rec normalize acc m =
		match m with
		| [] ->
			List.rev acc
		| Str.Text "." :: Str.Delim _ :: tl when acc = [] ->
			normalize [] tl
		| Str.Text ".." :: Str.Delim _ :: tl ->
			(match acc with
			| [] -> raise Exit
			| _ :: acc -> normalize acc tl)
		| Str.Text t :: Str.Delim _ :: tl ->
			normalize (t :: acc) tl
		| Str.Delim _ :: tl ->
			normalize ("" :: acc) tl
		| Str.Text t :: [] ->
			List.rev (t :: acc)
		| Str.Text _ :: Str.Text  _ :: _ ->
			assert false
	in
	String.concat "/" (normalize [] (Str.full_split path_regex path))

let path_sep = if Globals.is_windows then "\\" else "/"

(** Returns absolute path. Doesn't fix path case on Windows. *)
let get_full_path f = try Extc.get_full_path f with _ -> f

(** Returns absolute path (on Windows ensures proper case with drive letter upper-cased)
    Use for returning positions from IDE support functions *)
let get_real_path =
	if Globals.is_windows then
		(fun p -> try Extc.get_real_path p with _ -> p)
	else
		get_full_path

(** Returns absolute path guaranteed to be the same for different letter case.
    Use where equality comparison is required, lowercases the path on Windows *)
let unique_full_path =
	if Globals.is_windows then
		(fun f -> String.lowercase (get_full_path f))
	else
		get_full_path

let add_trailing_slash p =
	let l = String.length p in
	if l = 0 then
		"./"
	else match p.[l-1] with
		| '\\' | '/' -> p
		| _ -> p ^ "/"

let rec remove_trailing_slash p =
	let l = String.length p in
	if l = 0 then
		"./"
	else match p.[l-1] with
		| '\\' | '/' -> remove_trailing_slash (String.sub p 0 (l - 1))
		| _ -> p

let flat_path (p,s) =
	(* Replace _ with _$ in paths to prevent name collisions. *)
	let escape str = String.concat "_$" (ExtString.String.nsplit str "_") in

	match p with
	| [] -> escape s
	| _ -> String.concat "_" (List.map escape p) ^ "_" ^ (escape s)

open Globals

let find_directories target recursive paths =
	let target_dirs = List.map platform_name platforms in
	let rec loop acc dir =
		try
			let entries = Sys.readdir dir in
			Array.fold_left (fun acc file ->
				match file with
					| "." | ".." ->
						acc
					| _ when Sys.is_directory (dir ^ file) && file.[0] >= 'a' && file.[0] <= 'z' ->
						if List.mem file target_dirs && file <> target then
							acc
						else begin
							let full = (dir ^ file) in
							if recursive then loop (full :: acc) (full ^ "/")
							else full :: acc
						end
					| _ ->
						acc
			) acc entries;
		with Sys_error _ ->
			acc
	in
	List.fold_left (fun acc dir -> loop acc dir) [] paths

let make_valid_filename s =
	let r = Str.regexp "[^A-Za-z0-9_\\-\\.,]" in
	Str.global_substitute r (fun s -> "_") s

let module_name_of_file file =
	match List.rev (Str.split path_regex (get_real_path file)) with
	| s :: _ ->
		let s = match List.rev (ExtString.String.nsplit s ".") with
		| [s] -> s
		| _ :: sl -> String.concat "." (List.rev sl)
		| [] -> ""
		in
		s
	| [] ->
		assert false

let rec create_file bin ext acc = function
	| [] -> assert false
	| d :: [] ->
		let d = make_valid_filename d in
		let maxlen = 200 - String.length ext in
		let d = if String.length d > maxlen then String.sub d 0 maxlen else d in
		let ch = (if bin then open_out_bin else open_out) (String.concat "/" (List.rev (d :: acc)) ^ ext) in
		ch
	| d :: l ->
		let dir = String.concat "/" (List.rev (d :: acc)) in
		if not (Sys.file_exists (remove_trailing_slash dir)) then Unix.mkdir dir 0o755;
		create_file bin ext (d :: acc) l

let rec mkdir_recursive base dir_list =
	match dir_list with
	| [] -> ()
	| dir :: remaining ->
		let path = match base with
				   | "" ->  dir
				   | "/" -> "/" ^ dir
				   | _ -> base ^ "/" ^ dir
		in
		let path_len = String.length path in
		let path =
			if path_len > 0 && (path.[path_len - 1] = '/' || path.[path_len - 1] == '\\') then
				String.sub path 0 (path_len - 1)
			else
				path
		in
		if not ( (path = "") || ( (path_len = 2) && ((String.sub path 1 1) = ":") ) ) then
			if not (Sys.file_exists path) then
				Unix.mkdir path 0o755;
		mkdir_recursive (if (path = "") then "/" else path) remaining

let mkdir_from_path path =
	let parts = Str.split_delim (Str.regexp "[\\/]+") path in
	match parts with
		| [] -> (* path was "" *) ()
		| _ ->
			let dir_list = List.rev (List.tl (List.rev parts)) in
			mkdir_recursive "" dir_list

let full_dot_path pack mname tname =
	if tname = mname then (pack,mname) else (pack @ [mname],tname)