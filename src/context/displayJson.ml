open Globals
open Json.Reader
open JsonRpc
open Json
open Common
open Common.DisplayMode

type haxe_json_error =
	| MissingParam of string
	| BadParamType of string * string

let raise_haxe_json_error id = function
	| MissingParam s -> raise_custom id 1 ("Missing param: " ^ s)
	| BadParamType(name,expected) -> raise_custom id 2 ("Unexpected value for param " ^ name ^ ", expected " ^ expected)

let parse_input com input =
	let string_of_json json =
		let b = Buffer.create 0 in
		Json.write_json (Buffer.add_string b) json;
		Buffer.contents b;
	in
	let fail json =
		prerr_endline (string_of_json json);
		exit 1;
	in
	let process () =
		let id,name,params = JsonRpc.parse_request input in
		let params = match params with
			| Some (JObject fl) -> fl
			| Some json -> raise_invalid_params json
			| None -> raise_invalid_params JNull
		in
		let get_param name =
			try List.assoc name params with Not_found -> raise_haxe_json_error id (MissingParam name)
		in
		let get_string_param name = match get_param name with
			| JString s -> s
			| _ -> raise_haxe_json_error id (BadParamType(name,"String"))
		in
		let get_int_param name = match get_param name with
			| JInt i -> i
			| _ -> raise_haxe_json_error id (BadParamType(name,"Int"))
		in
		let enable_display mode =
			com.display <- DisplayMode.create mode;
			Common.display_default := mode;
			Common.define_value com Define.Display "1";
			Parser.use_doc := true;
		in
		let read_display_file () =
			let file = get_string_param "file" in
			let pos = get_int_param "offset" in
			Parser.resume_display := {
				pfile = Path.unique_full_path file;
				pmin = pos;
				pmax = pos;
			}
		in
		begin match name with
			| "definition" ->
				Common.define com Define.NoCOpt;
				read_display_file();
				enable_display DMPosition;
			| _ -> raise_method_not_found id name
		end;
		let f_result json =
			string_of_json (JsonRpc.result id json)
		in
		let f_error jl =
			fail (JsonRpc.error id 0 ~data:(Some (JArray jl)) "Compiler error")
		in
		com.json_out <- Some(f_result,f_error)
	in
	JsonRpc.handle_jsonrpc_error process fail;
	()