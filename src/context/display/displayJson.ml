open Globals
open Json.Reader
open JsonRpc
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

let supports_resolve = ref false

let create_json_context may_resolve =
	Genjson.create_context (if may_resolve && !supports_resolve then GMMinimum else GMFull)

let send_string j =
	raise (DisplayOutput.Completion j)

let send_json json =
	send_string (string_of_json json)

type haxe_json_error =
	| MissingField of string * string
	| BadType of string * string

class jsonrpc_handler report_times (id,name,params) = object(self)
	val id = id
	val method_name : string = name
	val params = match params with
		| Some (JObject fl) -> fl
		| Some json -> raise_invalid_params json
		| None -> []

	method get_id = id
	method get_method_name = method_name

	method raise_haxe_json_error : 'a . haxe_json_error -> 'a = function
		| MissingField(name,on) -> raise_custom id 1 (Printf.sprintf "Missing param \"%s\" on \"%s\"" name on)
		| BadType(desc,expected) -> raise_custom id 2 (Printf.sprintf "Unexpected value for \"%s\", expected %s" desc expected)

	method get_field desc fl name : Json.t =
		try
			List.assoc name fl
		with Not_found ->
			self#raise_haxe_json_error (MissingField(name,desc))

	method get_string desc j = match j with
		| JString s -> s
		| _ -> self#raise_haxe_json_error (BadType(desc,"String"))

	method get_int desc j = match j with
		| JInt i -> i
		| _ -> self#raise_haxe_json_error (BadType(desc,"String"))

	method get_bool desc j = match j with
		| JBool b -> b
		| _ -> self#raise_haxe_json_error (BadType(desc,"Bool"))

	method get_array desc j : Json.t list = match j with
		| JArray a -> a
		| _ -> self#raise_haxe_json_error (BadType(desc,"Array"))

	method get_object desc j = match j with
		| JObject o -> o
		| _ -> self#raise_haxe_json_error (BadType(desc,"Object"))

	method get_string_field desc name fl =
		self#get_string desc (self#get_field desc fl name)

	method get_int_field desc name fl =
		self#get_int desc (self#get_field desc fl name)

	method get_bool_field desc name fl =
		self#get_bool desc (self#get_field desc fl name)

	method get_array_field desc name fl =
		self#get_array desc (self#get_field desc fl name)

	method get_object_field desc name fl =
		self#get_object desc (self#get_field desc fl name)

	method get_string_param name =
		self#get_string_field "params" name params

	method get_int_param name =
		self#get_int_field "params" name params

	method get_bool_param name =
		self#get_bool_field "params" name params

	method get_array_param name =
		self#get_array_field "params" name params

	method get_object_param name =
		self#get_object_field "params" name params

	method get_opt_param : 'a . (unit -> 'a) -> 'a -> 'a = fun f def ->
		try f() with JsonRpc_error _ -> def

	method send_result json : unit =
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
		let jo = jobject fl in
		send_json (JsonRpc.result id jo);

	method send_error : 'a . Json.t list -> 'a  = fun jl ->
		send_json (JsonRpc.error id 0 ~data:(Some (JArray jl)) "Compiler error")
end

class display_handler (jsonrpc : jsonrpc_handler) com cs = object(self)
	val cs = cs;
	val mutable debug_context_sign = None

	method get_cs = cs

	method set_debug_context_sign sign =
		debug_context_sign <- sign

	method get_sign = match debug_context_sign with
		| None -> Define.get_signature com.defines
		| Some sign -> sign

	method enable_display mode =
		com.display <- create mode;
		Parser.display_mode := mode;
		Common.define_value com Define.Display "1";
		Parser.use_doc := true;

	method set_display_file was_auto_triggered requires_offset =
		let file = jsonrpc#get_string_param "file" in
		let file = Path.unique_full_path file in
		let pos = if requires_offset then jsonrpc#get_int_param "offset" else (-1) in
		TypeloadParse.current_stdin := jsonrpc#get_opt_param (fun () ->
			let s = jsonrpc#get_string_param "contents" in
			Common.define com Define.DisplayStdin; (* TODO: awkward *)
			(* Remove our current display file from the cache so the server doesn't pick it up *)
			CompilationServer.remove_files cs file;
			Some s
		) None;
		Parser.was_auto_triggered := was_auto_triggered;
		DisplayPosition.display_position := {
			pfile = file;
			pmin = pos;
			pmax = pos;
		}
end

type handler_context = {
	com : Common.context;
	jsonrpc : jsonrpc_handler;
	display : display_handler;
}

let handler =
	let open CompilationServer in
	let h = Hashtbl.create 0 in
	let l = [
		"initialize", (fun hctx ->
			supports_resolve := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_bool_param "supportsResolve") false;
			let methods = Hashtbl.fold (fun k _ acc -> (jstring k) :: acc) h [] in
			hctx.jsonrpc#send_result (JObject [
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
					"minor",jint 1;
					"patch",jint 0;
				]
			])
		);
		"display/completionItem/resolve", (fun hctx ->
			let i = hctx.jsonrpc#get_int_param "index" in
			begin try
				let item = (!DisplayException.last_completion_result).(i) in
				let ctx = Genjson.create_context GMFull in
				hctx.jsonrpc#send_result (jobject ["item",CompletionItem.to_json ctx item])
			with Invalid_argument _ ->
				hctx.jsonrpc#send_error [jstring (Printf.sprintf "Invalid index: %i" i)]
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
		"display/references", (fun hctx ->
			Common.define hctx.com Define.NoCOpt;
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMUsage false);
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
			hctx.com.callbacks.after_init_macros <- (fun () ->
				CompilationServer.set_initialized hctx.display#get_cs;
				DisplayToplevel.read_class_paths hctx.com ["init"];
				hctx.jsonrpc#send_result (jstring "class paths read");
			) :: hctx.com.callbacks.after_init_macros;
		);
		"server/contexts", (fun hctx ->
			let l = List.map (fun (sign,index) -> jobject [
				"index",jstring index;
				"signature",jstring (Digest.to_hex sign);
			]) (CompilationServer.get_signs hctx.display#get_cs) in
			hctx.jsonrpc#send_result (jarray l)
		);
		"server/select", (fun hctx ->
			let i = hctx.jsonrpc#get_int_param "index" in
			let (sign,_) = try
				CompilationServer.get_sign_by_index hctx.display#get_cs (string_of_int i)
			with Not_found ->
				hctx.jsonrpc#send_error [jstring "No such context"]
			in
			hctx.display#set_debug_context_sign (Some sign);
			hctx.jsonrpc#send_result (jstring (Printf.sprintf "Context %i selected" i))
		 );
		 "server/modules", (fun hctx ->
			let sign = hctx.display#get_sign in
			let l = Hashtbl.fold (fun (_,sign') m acc ->
				if sign = sign' && m.m_extra.m_kind <> MFake then jstring (s_type_path m.m_path) :: acc else acc
			) hctx.display#get_cs.cache.c_modules [] in
			hctx.jsonrpc#send_result (jarray l)
		 );
		"server/module", (fun hctx ->
			let sign = hctx.display#get_sign in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "path") in
			let m = try
				CompilationServer.find_module hctx.display#get_cs (path,sign)
			with Not_found ->
				hctx.jsonrpc#send_error [jstring "No such module"]
			in
			hctx.jsonrpc#send_result (generate_module () m)
		);
		"server/files", (fun hctx ->
			let files = CompilationServer.get_file_list hctx.display#get_cs hctx.com in
			let files = List.map (fun (file,cfile) ->
				jobject [
					"file",jstring file;
					"time",jfloat cfile.c_time;
					"package",jstring (String.concat "." cfile.c_package);
					"moduleName",jopt jstring cfile.c_module_name;
				]
			) files in
			hctx.jsonrpc#send_result (jarray files)
		);
		"server/invalidate", (fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let file = Path.unique_full_path file in
			CompilationServer.taint_modules hctx.display#get_cs file;
			hctx.jsonrpc#send_result jnull
		);
		"server/configure", (fun hctx ->
			let l = ref (List.map (fun (name,value) ->
				let value = hctx.jsonrpc#get_bool "value" value in
				try
					ServerMessage.set_by_name name value;
					jstring (Printf.sprintf "Printing %s %s" name (if value then "enabled" else "disabled"))
				with Not_found ->
					hctx.jsonrpc#send_error [jstring ("Invalid print parame name: " ^ name)]
			) (hctx.jsonrpc#get_opt_param (fun () -> (hctx.jsonrpc#get_object_param "print")) [])) in
			hctx.jsonrpc#get_opt_param (fun () ->
				let b = hctx.jsonrpc#get_bool_param "noModuleChecks" in
				ServerConfig.do_not_check_modules := b;
				l := jstring ("Module checks " ^ (if b then "disabled" else "enabled")) :: !l;
				()
			) ();
			hctx.jsonrpc#send_result (jarray !l)
		);
	] in
	List.iter (fun (s,f) -> Hashtbl.add h s f) l;
	h

let parse_input com input report_times =
	let input =
		JsonRpc.handle_jsonrpc_error (fun () -> JsonRpc.parse_request input) send_json
	in
	let jsonrpc = new jsonrpc_handler report_times input in
	com.json_out <- Some(jsonrpc#send_result,jsonrpc#send_error);

	let cs = match CompilationServer.get() with
		| Some cs -> cs
		| None -> jsonrpc#send_error [jstring "compilation server not running for some reason"];
	in

	let display = new display_handler jsonrpc com cs in

	let hctx = {
		com = com;
		jsonrpc = jsonrpc;
		display = display;
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
