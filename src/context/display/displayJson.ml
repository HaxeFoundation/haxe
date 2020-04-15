open Globals
open Json.Reader
open JsonRpc
open Jsonrpc_handler
open Json
open Common
open DisplayTypes.DisplayMode
open Timer
open Genjson
open Type

(* Generate the JSON of our times. *)
let json_of_times root =
	let rec loop node =
		if node == root || node.time > 0.0009 then begin
			let children = ExtList.List.filter_map loop node.children in
			let fl = [
				"name",jstring node.name;
				"path",jstring node.path;
				"info",jstring node.info;
				"time",jfloat node.time;
				"calls",jint node.num_calls;
				"percentTotal",jfloat (if root.time = 0. then 0. else (node.time *. 100. /. root.time));
				"percentParent",jfloat (if node == root || node.parent.time = 0. then 0. else node.time *. 100. /. node.parent.time);
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

let supports_resolve = ref false

let create_json_context jsonrpc may_resolve =
	Genjson.create_context ~jsonrpc:jsonrpc (if may_resolve && !supports_resolve then GMMinimum else GMFull)

let send_string j =
	raise (DisplayOutput.Completion j)

let send_json json =
	send_string (string_of_json json)

class display_handler (jsonrpc : jsonrpc_handler) com (cs : CompilationServer.t) = object(self)
	val cs = cs;

	method get_cs = cs

	method enable_display mode =
		com.display <- create mode;
		Parser.display_mode := mode;
		Common.define_value com Define.Display "1"

	method set_display_file was_auto_triggered requires_offset =
		let file = jsonrpc#get_opt_param (fun () ->
			let file = jsonrpc#get_string_param "file" in
			Path.unique_full_path file
		) DisplayOutput.file_input_marker in
		let pos = if requires_offset then jsonrpc#get_int_param "offset" else (-1) in
		TypeloadParse.current_stdin := jsonrpc#get_opt_param (fun () ->
			let s = jsonrpc#get_string_param "contents" in
			Common.define com Define.DisplayStdin; (* TODO: awkward *)
			Some s
		) None;
		Parser.was_auto_triggered := was_auto_triggered;
		DisplayPosition.display_position#set {
			pfile = file;
			pmin = pos;
			pmax = pos;
		}
end

type handler_context = {
	com : Common.context;
	jsonrpc : jsonrpc_handler;
	display : display_handler;
	send_result : Json.t -> unit;
	send_error : 'a . Json.t list -> 'a;
}

let handler =
	let open CompilationServer in
	let h = Hashtbl.create 0 in
	let l = [
		"initialize", (fun hctx ->
			supports_resolve := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_bool_param "supportsResolve") false;
			DisplayException.max_completion_items := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_int_param "maxCompletionItems") 0;
			let exclude = hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_array_param "exclude") [] in
			DisplayToplevel.exclude := List.map (fun e -> match e with JString s -> s | _ -> assert false) exclude;
			let methods = Hashtbl.fold (fun k _ acc -> (jstring k) :: acc) h [] in
			hctx.send_result (JObject [
				"methods",jarray methods;
				"haxeVersion",jobject [
					"major",jint version_major;
					"minor",jint version_minor;
					"patch",jint version_revision;
					"pre",(match version_pre with None -> jnull | Some pre -> jstring pre);
					"build",(match Version.version_extra with None -> jnull | Some(_,build) -> jstring build);
				];
				"protocolVersion",jobject [
					"major",jint 0;
					"minor",jint 4;
					"patch",jint 0;
				]
			])
		);
		"display/completionItem/resolve", (fun hctx ->
			let i = hctx.jsonrpc#get_int_param "index" in
			begin try
				let item = (!DisplayException.last_completion_result).(i) in
				let ctx = Genjson.create_context GMFull in
				hctx.send_result (jobject ["item",CompletionItem.to_json ctx None item])
			with Invalid_argument _ ->
				hctx.send_error [jstring (Printf.sprintf "Invalid index: %i" i)]
			end
		);
		"display/completion", (fun hctx ->
			hctx.display#set_display_file (hctx.jsonrpc#get_bool_param "wasAutoTriggered") true;
			hctx.display#enable_display DMDefault;
		);
		"display/definition", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMDefinition;
		);
		"display/implementation", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMImplementation);
		);
		"display/typeDefinition", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMTypeDefinition;
		);
		"display/references", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMUsage (false,false,false));
		);
		"display/references/base", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMUsage (false,true,true));
		);
		"display/references/descendants", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMUsage (false,true,false));
		);
		"display/hover", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMHover;
		);
		"display/package", (fun hctx ->
			hctx.display#set_display_file false false;
			hctx.display#enable_display DMPackage;
		);
		"display/signatureHelp", (fun hctx ->
			hctx.display#set_display_file (hctx.jsonrpc#get_bool_param "wasAutoTriggered") true;
			hctx.display#enable_display DMSignature
		);
		"server/readClassPaths", (fun hctx ->
			hctx.com.callbacks#add_after_init_macros (fun () ->
				let cc = hctx.display#get_cs#get_context (Define.get_signature hctx.com.defines) in
				cc#set_initialized true;
				DisplayToplevel.read_class_paths hctx.com ["init"];
				let files = hctx.display#get_cs#get_files in
				hctx.send_result (jobject [
					"files", jint (List.length files)
				]);
			)
		);
		"server/contexts", (fun hctx ->
			let l = List.map (fun cc -> cc#get_json) hctx.display#get_cs#get_contexts in
			hctx.send_result (jarray l)
		);
		"server/modules", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let cc = hctx.display#get_cs#get_context sign in
			let l = Hashtbl.fold (fun _ m acc ->
				if m.m_extra.m_kind <> MFake then jstring (s_type_path m.m_path) :: acc else acc
			) cc#get_modules [] in
			hctx.send_result (jarray l)
		);
		"server/module", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "path") in
			let cc = hctx.display#get_cs#get_context sign in
			let m = try
				cc#find_module path
			with Not_found ->
				hctx.send_error [jstring "No such module"]
			in
			hctx.send_result (generate_module () m)
		);
		"server/moduleCreated", (fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let file = Path.unique_full_path file in
			let cs = hctx.display#get_cs in
			List.iter (fun cc ->
				Hashtbl.replace cc#get_removed_files file ()
			) cs#get_contexts;
			hctx.send_result (jstring file);
		);
		"server/files", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let cc = hctx.display#get_cs#get_context sign in
			let files = Hashtbl.fold (fun file cfile acc -> (file,cfile) :: acc) cc#get_files [] in
			let files = List.sort (fun (file1,_) (file2,_) -> compare file1 file2) files in
			let files = List.map (fun (file,cfile) ->
				jobject [
					"file",jstring file;
					"time",jfloat cfile.c_time;
					"pack",jstring (String.concat "." cfile.c_package);
					"moduleName",jopt jstring cfile.c_module_name;
				]
			) files in
			hctx.send_result (jarray files)
		);
		"server/invalidate", (fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let file = Path.unique_full_path file in
			let cs = hctx.display#get_cs in
			cs#taint_modules file;
			cs#remove_files file;
			hctx.send_result jnull
		);
		"server/configure", (fun hctx ->
			let l = ref (List.map (fun (name,value) ->
				let value = hctx.jsonrpc#get_bool "value" value in
				try
					ServerMessage.set_by_name name value;
					jstring (Printf.sprintf "Printing %s %s" name (if value then "enabled" else "disabled"))
				with Not_found ->
					hctx.send_error [jstring ("Invalid print parame name: " ^ name)]
			) (hctx.jsonrpc#get_opt_param (fun () -> (hctx.jsonrpc#get_object_param "print")) [])) in
			hctx.jsonrpc#get_opt_param (fun () ->
				let b = hctx.jsonrpc#get_bool_param "noModuleChecks" in
				ServerConfig.do_not_check_modules := b;
				l := jstring ("Module checks " ^ (if b then "disabled" else "enabled")) :: !l;
				()
			) ();
			hctx.jsonrpc#get_opt_param (fun () ->
				let b = hctx.jsonrpc#get_bool_param "legacyCompletion" in
				ServerConfig.legacy_completion := b;
				l := jstring ("Legacy completion " ^ (if b then "enabled" else "disabled")) :: !l;
				()
			) ();
			hctx.send_result (jarray !l)
		);
		"server/memory",(fun hctx ->
			let j = Memory.get_memory_json hctx.display#get_cs MCache in
			hctx.send_result j
		);
		"server/memory/context",(fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let j = Memory.get_memory_json hctx.display#get_cs (MContext sign) in
			hctx.send_result j
		);
		"server/memory/module",(fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "path") in
			let j = Memory.get_memory_json hctx.display#get_cs (MModule(sign,path)) in
			hctx.send_result j
		);
		(* TODO: wait till gama complains about the naming, then change it to something else *)
		"typer/compiledTypes", (fun hctx ->
			hctx.com.callbacks#add_after_filters (fun () ->
				let ctx = create_context GMFull in
				let l = List.map (generate_module_type ctx) hctx.com.types in
				hctx.send_result (jarray l)
			);
		);
	] in
	List.iter (fun (s,f) -> Hashtbl.add h s f) l;
	h

let parse_input com input report_times =
	let input =
		JsonRpc.handle_jsonrpc_error (fun () -> JsonRpc.parse_request input) send_json
	in
	let jsonrpc = new jsonrpc_handler input in

	let send_result json =
		let fl = [
			"result",json;
			"timestamp",jfloat (Unix.gettimeofday ());
		] in
		let fl = if !report_times then begin
			close_times();
			let _,_,root = Timer.build_times_tree () in
			begin match json_of_times root with
			| None -> fl
			| Some jo -> ("timers",jo) :: fl
			end
		end else fl in
		let fl = if DynArray.length com.pass_debug_messages > 0 then
			("passMessages",jarray (List.map jstring (DynArray.to_list com.pass_debug_messages))) :: fl
		else
			fl
		in
		let jo = jobject fl in
		send_json (JsonRpc.result jsonrpc#get_id  jo)
	in

	let send_error jl =
		send_json (JsonRpc.error jsonrpc#get_id 0 ~data:(Some (JArray jl)) "Compiler error")
	in

	com.json_out <- Some({
		send_result = send_result;
		send_error = send_error;
		jsonrpc = jsonrpc
	});

	let cs = match CompilationServer.get() with
		| Some cs -> cs
		| None -> send_error [jstring "compilation server not running for some reason"];
	in

	let display = new display_handler jsonrpc com cs in

	let hctx = {
		com = com;
		jsonrpc = jsonrpc;
		display = display;
		send_result = send_result;
		send_error = send_error;
	} in

	JsonRpc.handle_jsonrpc_error (fun () ->
		let method_name = jsonrpc#get_method_name in
		let f = try
			Hashtbl.find handler method_name
		with Not_found ->
			raise_method_not_found jsonrpc#get_id method_name
		in
		f hctx
	) send_json
