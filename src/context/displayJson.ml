open Globals
open Json.Reader
open JsonRpc
open Json
open Common
open DisplayTypes.DisplayMode

type haxe_json_error =
	| MissingParam of string
	| BadParamType of string * string

let raise_haxe_json_error id = function
	| MissingParam s -> raise_custom id 1 ("Missing param: " ^ s)
	| BadParamType(name,expected) -> raise_custom id 2 ("Unexpected value for param " ^ name ^ ", expected " ^ expected)

let get_capabilities () =
	JObject [
		"definitionProvider",JBool true;
		"hoverProvider",JBool true;
		"completionProvider",JBool true;
	]

let parse_input com input =
	let fail json =
		prerr_endline (string_of_json json);
		exit 1;
	in
	let process () =
		let id,name,params = JsonRpc.parse_request input in
		let f_result json =
			(string_of_json (JsonRpc.result id json));
		in
		let f_error jl =
			fail (JsonRpc.error id 0 ~data:(Some (JArray jl)) "Compiler error")
		in
		let params = match params with
			| Some (JObject fl) -> fl
			| Some json -> raise_invalid_params json
			| None -> []
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
		let get_bool_param name = match get_param name with
			| JBool b -> b
			| _ -> raise_haxe_json_error id (BadParamType(name,"Bool"))
		in
		(*
		let opt_param name f =
			if not (List.mem_assoc name params) then None
			else Some (f name)
		in *)
		let enable_display mode =
			com.display <- create mode;
			Common.display_default := mode;
			Common.define_value com Define.Display "1";
			Parser.use_doc := true;
		in
		let read_display_file was_auto_triggered =
			let file = get_string_param "file" in
			let pos = get_int_param "offset" in
			Parser.was_auto_triggered := was_auto_triggered;
			Parser.resume_display := {
				pfile = Path.unique_full_path file;
				pmin = pos;
				pmax = pos;
			}
		in
		begin match name with
			| "initialize" ->
				raise (DisplayOutput.Completion (f_result (JObject [
					"capabilities",get_capabilities()
				])))
			| "textDocument/completion" ->
				read_display_file (get_bool_param "wasAutoTriggered");
				enable_display DMDefault;
			| "textDocument/definition" ->
				Common.define com Define.NoCOpt;
				read_display_file false;
				enable_display DMDefinition;
			| "textDocument/hover" ->
				Common.define com Define.NoCOpt;
				read_display_file false;
				enable_display DMHover;
			| _ -> raise_method_not_found id name
		end;
		com.json_out <- Some(f_result,f_error)
	in
	JsonRpc.handle_jsonrpc_error process fail;
	()