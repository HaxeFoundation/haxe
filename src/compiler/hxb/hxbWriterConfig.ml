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

module WriterConfigReader (API : DataReaderApi.DataReaderApi) = struct
	let read_target_config config fl =
		List.iter (fun (s,data) -> match s with
			| "generate" ->
				config.generate <- API.read_bool data;
			| "exclude" ->
				API.read_optional data (fun data ->
					let l = API.read_array data in
					config.exclude <- List.map (fun data -> ExtString.String.nsplit (API.read_string data) ".") l
				)
			| "include" ->
				API.read_optional data (fun data ->
					let l = API.read_array data in
					config.include'<- List.map (fun data -> ExtString.String.nsplit (API.read_string data) ".") l
				)
			| "hxbVersion" ->
				config.hxb_version <- API.read_int data
			| s ->
				error (Printf.sprintf "Unknown key for target config: %s" s)
		) fl

	let read_writer_config config target_name data =
		let read data =
			let fl = API.read_object data in
			List.iter (fun (s,data) ->
				match s with
				| "archivePath" ->
					let path = API.read_string data in
					let path = Str.global_replace (Str.regexp "\\$target") target_name path in
					config.archive_path <- path;
				| "targetConfig" ->
					API.read_optional data (fun data -> read_target_config config.target_config (API.read_object data))
				| "macroConfig" ->
					API.read_optional data (fun data -> read_target_config config.macro_config (API.read_object data))
				| s ->
					error (Printf.sprintf "Unknown key for writer config: %s" s)
			) fl
		in
		API.read_optional data read
end

module WriterConfigReaderJson = WriterConfigReader(JsonDataApi.JsonReaderApi)

module WriterConfigWriter (API : DataWriterApi.DataWriterApi) = struct
	let write_target_config config =
		API.write_object [
			"generate",API.write_bool config.generate;
			"exclude",API.write_array (List.map (fun sl -> API.write_string (String.concat "." sl)) config.exclude);
			"include",API.write_array (List.map (fun sl -> API.write_string (String.concat "." sl)) config.include');
			"hxbVersion",API.write_int config.hxb_version;
		]

	let write_writer_config config =
		API.write_object [
			"archivePath",API.write_string config.archive_path;
			"targetConfig",write_target_config config.target_config;
			"macroConfig",write_target_config config.macro_config;
		]
end

let process_json config target_name json =
	WriterConfigReaderJson.read_writer_config config target_name json

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