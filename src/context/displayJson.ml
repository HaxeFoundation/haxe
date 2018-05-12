open Globals
open Json.Reader
open JsonRpc
open Json
open Common
open DisplayTypes.DisplayMode
open Timer
open Genjson

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
		"packageProvider",JBool true;
	]

(* Generate the JSON of our times. *)
let json_of_times root =
	let rec loop node =
		if node.time > 0.0009 then begin
			let children = ExtList.List.filter_map loop node.children in
			let fl = [
				"name",jstring node.name;
				"path",jstring node.path;
				"info",jstring node.info;
				"time",jfloat node.time;
				"calls",jint node.num_calls;
				"percentTotal",jfloat (node.time *. 100. /. root.time);
				"percentParent",jfloat (if node == root then 0. else node.time *. 100. /. node.parent.time);
			] in
			let fl = match children with
				| [] -> fl
				| _ -> ("children",jarray children) :: fl
			in
			Some (jobject fl)
		end else
			None
	in
	loop root

let parse_input com input report_times =
	let fail json =
		prerr_endline (string_of_json json);
		exit 1;
	in
	let process () =
		let id,name,params = JsonRpc.parse_request input in
		let f_result json =
			let fl = [
				"result",json;
			] in
			let fl = if !report_times then begin
				let _,_,root = Timer.build_times_tree () in
				begin match json_of_times root with
				| None -> fl
				| Some jo -> ("timers",jo) :: fl
				end
			end else fl in
			let jo = jobject fl in
			(string_of_json (JsonRpc.result id jo));
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
		let read_display_file was_auto_triggered requires_offset is_completion =
			let file = get_string_param "file" in
			let pos = if requires_offset then get_int_param "offset" else (-1) in
			Parser.was_auto_triggered := was_auto_triggered;
			let pos = if pos <> (-1) && not is_completion then pos + 1 else pos in
			Parser.resume_display := {
				pfile = Path.unique_full_path file;
				pmin = pos;
				pmax = pos;
			}
		in
		Parser.is_completion := false;
		begin match name with
			| "initialize" ->
				raise (DisplayOutput.Completion (f_result (JObject [
					"capabilities",get_capabilities()
				])))
			| "textDocument/completion" ->
				read_display_file (get_bool_param "wasAutoTriggered") true true;
				Parser.is_completion := true;
				enable_display DMDefault;
			| "textDocument/definition" ->
				Common.define com Define.NoCOpt;
				read_display_file false true false;
				enable_display DMDefinition;
			| "textDocument/hover" ->
				Common.define com Define.NoCOpt;
				read_display_file false true false;
				enable_display DMHover;
			| "textDocument/package" ->
				read_display_file false false false;
				enable_display DMPackage;
			| _ -> raise_method_not_found id name
		end;
		com.json_out <- Some(f_result,f_error)
	in
	JsonRpc.handle_jsonrpc_error process fail;
	()
