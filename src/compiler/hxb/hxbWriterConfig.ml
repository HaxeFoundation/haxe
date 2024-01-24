open Globals
open Json
open Json.Reader

type writer_target_config = {
	mutable generate : bool;
	mutable exclude : string list list;
	mutable include' : string list list;
	mutable hxb_version : int;
}

type t = {
	mutable archive_path : string;
	target_config : writer_target_config;
	macro_config : writer_target_config;
}

let create_target_config () = {
	generate = true;
	exclude = [];
	include'= [];
	hxb_version = HxbData.hxb_version;
}

let create () = {
	archive_path = "";
	target_config = create_target_config ();
	macro_config = create_target_config ()
}

let error s =
	Error.raise_typing_error s null_pos

let process_json config target_name json =
	let read_string = function
		| JString s -> s
		| json -> error (Printf.sprintf "Invalid JSON where string was expected: %s" (string_of_json json))
	in
	let read_int = function
		| JInt i -> i
		| json -> error (Printf.sprintf "Invalid JSON where integer was expected: %s" (string_of_json json))
	in
	let read_bool = function
		| JBool b -> b
		| json -> error (Printf.sprintf "Invalid JSON where bool was expected: %s" (string_of_json json))
	in
	let read_array_or_null f json = match json with
		| JNull ->
			[]
		| JArray jl ->
			List.map f jl
		| _ ->
			error (Printf.sprintf "Invalid JSON where array was expected: %s" (string_of_json json))
	in
	let read_object_or_null f json = match json with
		| JNull ->
			()
		| JObject fl ->
			f fl
		| _ ->
			error (Printf.sprintf "Invalid JSON where object was expected: %s" (string_of_json json))
	in
	let read_target_config config fl =
		List.iter (fun (s,json) -> match s with
			| "generate" ->
				config.generate <- read_bool json;
			| "exclude" ->
				config.exclude <- read_array_or_null (fun json -> ExtString.String.nsplit (read_string json) ".") json
			| "include" ->
				config.include' <- read_array_or_null (fun json -> ExtString.String.nsplit (read_string json) ".") json
			| "hxbVersion" ->
				config.hxb_version <- read_int json
			| s ->
				error (Printf.sprintf "Unknown key for target config: %s" s)
		) fl;
	in
	let read_writer_config fl =
		List.iter (fun (s,json) ->
			match s with
			| "archivePath" ->
				let path = read_string json in
				let path = Str.global_replace (Str.regexp "\\$target") target_name path in
				config.archive_path <- path;
			| "targetConfig" ->
				read_object_or_null (read_target_config config.target_config) json
			| "macroConfig" ->
				read_object_or_null (read_target_config config.macro_config) json
			| s ->
				error (Printf.sprintf "Unknown key for writer config: %s" s)
		) fl;
	in
	read_object_or_null read_writer_config json

let parse config target_name input =
	let lexbuf = Sedlexing.Utf8.from_string input in
	let json = read_json lexbuf in
	process_json config target_name json

let process_argument target_name file =
	let config = create () in
	begin match Path.file_extension file with
		| "json" ->
			let file = try
				open_in file
			with exc ->
				error (Printf.sprintf "Could not open file %s: %s" file (Printexc.to_string exc))
			in
			let data = Std.input_all file in
			close_in file;
			parse config target_name data;
		| _ ->
			config.archive_path <- file;
	end;
	Some config