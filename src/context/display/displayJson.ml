open Globals
open JsonRpc
open Jsonrpc_handler
open Json
open Common
open DisplayTypes.DisplayMode
open Timer
open Genjson
open Type
open DisplayProcessingGlobals

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

let create_json_context  may_resolve =
	Genjson.create_context (if may_resolve && !supports_resolve then GMMinimum else GMFull)

let send_string io j =
	CompilerIo.write_result io j

let send_json io json =
	send_string io (string_of_json json)

exception JsonCompleted

(* send_string_raise writes the string to io and then raises JsonCompleted.
   JsonCompleted is a control-flow exception signaling that a JSON-RPC response has been
   sent and no further output should be produced. It is caught by catch_completion_and_exit
   which calls finalize and exits cleanly.
   Raising is necessary for call-sites that send their response from within compilation
   (display output handlers, deferred callbacks, flush_context), where we need to abort
   further processing once the response is sent. *)
let send_string_raise io j =
	send_string io j;
	raise JsonCompleted

let send_json_raise io json =
	send_string_raise io (string_of_json json)

class display_handler (jsonrpc : jsonrpc_handler) com (cs : CompilationCache.t) = object(self)
	val cs = cs;

	method get_cs = cs

	method enable_display ?(skip_define=false) mode =
		com.display <- create mode;
		if not skip_define then Common.define_value com Define.Display "1"

	method set_display_file was_auto_triggered requires_offset =
		let file = jsonrpc#get_opt_param (fun () ->
			let file = jsonrpc#get_string_param "file" in
			Path.get_full_path file
		) file_input_marker in
		let contents = jsonrpc#get_opt_param (fun () ->
			let s = jsonrpc#get_string_param "contents" in
			Some s
		) None in

		let pos = if requires_offset then jsonrpc#get_int_param "offset" else (-1) in
		com.part_scope.parser_state.was_auto_triggered <- was_auto_triggered;

		if file <> file_input_marker then begin
			let file_unique = com.part_scope.file_keys#get file in

			DisplayPosition.display_position#set {
				pfile = file;
				pmin = pos;
				pmax = pos;
			};

			com.file_contents <- [file_unique, contents];
		end else begin
			let file_contents = jsonrpc#get_opt_param (fun () ->
				jsonrpc#get_opt_param (fun () -> jsonrpc#get_array_param "fileContents") []
			) [] in

			let file_contents = List.map (fun fc -> match fc with
				| JObject fl ->
					let file = jsonrpc#get_string_field "fileContents" "file" fl in
					let file = Path.get_full_path file in
					let file_unique = com.part_scope.file_keys#get file in
					let contents = jsonrpc#get_opt_param (fun () ->
						let s = jsonrpc#get_string_field "fileContents" "contents" fl in
						Some s
					) None in
					(file_unique, contents)
				| _ -> invalid_arg "fileContents"
			) file_contents in

			let files = (List.map (fun (k, _) -> k) file_contents) in
			com.file_contents <- file_contents;

			match files with
			| [] -> DisplayPosition.display_position#set { pfile = file; pmin = pos; pmax = pos; };
			| _ -> DisplayPosition.display_position#set_files files;
		end
end

class hxb_reader_api_com
	~(typing_mode : HxbData.typing_mode)
	(com : Common.context)
	(cc : CompilationCache.context_cache)
= object(self)
	method make_module (path : path) (file : string) =
		let mc = cc#get_hxb_module path in
		{
			m_id = mc.mc_id;
			m_path = path;
			m_types = [];
			m_statics = None;
			(* Creating a new m_extra because if we keep the same reference, display requests *)
			(* can alter it with bad data (for example adding dependencies that are not cached) *)
			m_extra = { mc.mc_extra with m_deps = mc.mc_extra.m_deps; m_display_deps = None }
		}

	method add_module (m : module_def) =
		com.module_lut#add m.m_path m;

	method resolve_type (pack : string list) (mname : string) (tname : string) (_:HxbData.typing_mode) =
		let path = (pack,mname) in
		let m = self#find_module path in
		List.find (fun t -> snd (t_path t) = tname) m.m_types

	method resolve_module (path : path) (_:HxbData.typing_mode) =
		self#find_module path

	method find_module (m_path : path) =
		try
			com.module_lut#find m_path
		with Not_found -> try
			cc#find_module m_path
		with Not_found ->
			let mc = cc#get_hxb_module m_path in
			let reader = new HxbReader.hxb_reader mc.mc_path com.hxb_reader_stats (if Common.defined com Define.HxbTimes then Some com.timer_ctx else None) in
			fst (reader#read_chunks_until (self :> HxbReaderApi.hxb_reader_api) mc.mc_chunks (if typing_mode = FullTyping then EOM else MTF) typing_mode)

	method basic_types =
		com.basic

	method get_var_id (i : int) =
		i

	method read_expression_eagerly (cf : tclass_field) =
		false

	method make_lazy_type t f =
		TLazy (make_unforced_lazy t f "com-api")
end

let find_module ~(typing_mode : HxbData.typing_mode) com cc path =
	(new hxb_reader_api_com ~typing_mode com cc)#find_module path

type handler_context = {
	com : Common.context;
	jsonrpc : jsonrpc_handler;
	display : display_handler;
}

type api_deferrence =
	| AfterInitMacros
	| AfterFilters

type api_response =
	| NoResponse
	| Result of Json.t
	| Error of Json.t
	| Deferred of api_deferrence * (unit -> api_response)

let defer deferrence f =
	Deferred (deferrence, fun () -> f ())

exception Api_error of Json.t

let api_error j =
	raise (Api_error j)

let handler =
	let open CompilationCache in
	let h = Hashtbl.create 0 in
	let l = [
		"initialize", (fun hctx ->
			supports_resolve := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_bool_param "supportsResolve") false;
			ServerConfig.max_completion_items := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_int_param "maxCompletionItems") 0;
			let exclude = hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_array_param "exclude") [] in
			DisplayToplevel.exclude := List.map (fun e -> match e with JString s -> s | _ -> die "" __LOC__) exclude;
			let methods = Hashtbl.fold (fun k _ acc -> (jstring k) :: acc) h [] in
			let version = hctx.com.sctx.version in
			Result (JObject [
				"methods",jarray methods;
				"haxeVersion",jobject [
					"major",jint version.major;
					"minor",jint version.minor;
					"patch",jint version.revision;
					"pre",(match version.pre with None -> jnull | Some pre -> jstring pre);
					"build",(match version.extra with None -> jnull | Some(_,build) -> jstring build);
				];
				"protocolVersion",jobject [
					"major",jint 0;
					"minor",jint 5;
					"patch",jint 0;
				]
			])
		);
		"display/completionItem/resolve", (fun hctx ->
			let i = hctx.jsonrpc#get_int_param "index" in
			begin try
				let item = (!DisplayException.last_completion_result).(i) in
				let ctx = Genjson.create_context GMFull in
				Result (jobject ["item",CompletionItem.to_json ctx None item])
			with Invalid_argument _ ->
				Error (jstring (Printf.sprintf "Invalid index: %i" i))
			end
		);
		"display/completion", (fun hctx ->
			hctx.display#set_display_file (hctx.jsonrpc#get_bool_param "wasAutoTriggered") true;
			hctx.display#enable_display DMDefault;
			NoResponse
		);
		"display/definition", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMDefinition;
			NoResponse
		);
		"display/diagnostics", (fun hctx ->
			hctx.display#set_display_file false false;
			hctx.display#enable_display ~skip_define:true DMNone;
			hctx.com.display <- { hctx.com.display with dms_display_file_policy = DFPAlso; dms_per_file = true; dms_populate_cache = true };
			hctx.com.part_scope.report_mode <- RMDiagnostics (List.map (fun (f,_) -> f) hctx.com.file_contents);
			NoResponse
		);
		"display/statistics", (fun hctx ->
			hctx.display#set_display_file false false;
			hctx.display#enable_display ~skip_define:true DMNone;
			hctx.com.display <- { hctx.com.display with dms_display_file_policy = DFPAlso; dms_per_file = true; dms_populate_cache = true };
			hctx.com.part_scope.report_mode <- RMStatistics;
			NoResponse
		);
		"display/implementation", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMImplementation);
			NoResponse
		);
		"display/typeDefinition", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMTypeDefinition;
			NoResponse
		);
		"display/references", (fun hctx ->
			hctx.display#set_display_file false true;
			begin match hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_string_param "kind") "normal" with
			| "withBaseAndDescendants" ->
				hctx.display#enable_display (DMUsage (false,true,true));
			| "withDescendants" ->
				hctx.display#enable_display (DMUsage (false,true,false));
			| _ ->
				hctx.display#enable_display (DMUsage (false,false,false));
			end;
			NoResponse
		);
		"display/hover", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMHover;
			NoResponse
		);
		"display/package", (fun hctx ->
			hctx.display#set_display_file false false;
			hctx.display#enable_display DMPackage;
			NoResponse
		);
		"display/documentSymbols", (fun hctx ->
			hctx.display#set_display_file false false;
			hctx.display#enable_display (DMModuleSymbols None);
			NoResponse
		);
		"display/workspaceSymbols", (fun hctx ->
			let filter = hctx.jsonrpc#get_string_param "filter" in
			hctx.display#enable_display (DMModuleSymbols (Some filter));
			NoResponse
		);
		"display/signatureHelp", (fun hctx ->
			hctx.display#set_display_file (hctx.jsonrpc#get_bool_param "wasAutoTriggered") true;
			hctx.display#enable_display DMSignature;
			NoResponse
		);
		"display/metadata", (fun hctx ->
			let include_compiler_meta = hctx.jsonrpc#get_bool_param "compiler" in
			let include_user_meta = hctx.jsonrpc#get_bool_param "user" in

			defer AfterInitMacros (fun () ->
				let all = Meta.get_meta_list hctx.com.user_metas in
				let all = List.filter (fun (_, (data:Meta.meta_infos)) ->
					match data.m_origin with
					| Compiler when include_compiler_meta -> true
					| UserDefined _ when include_user_meta -> true
					| _ -> false
				) all in

				Result (jarray (List.map (fun (t, (data:Meta.meta_infos)) ->
					let fields = [
						"name", jstring t;
						"doc", jstring data.m_doc;
						"parameters", jarray (List.map jstring data.m_params);
						"platforms", jarray (List.map (fun p -> jstring (platform_name p)) data.m_platforms);
						"targets", jarray (List.map (fun u -> jstring (Meta.print_meta_usage u)) data.m_used_on);
						"internal", jbool data.m_internal;
						"origin", jstring (match data.m_origin with
							| Compiler -> "haxe compiler"
							| UserDefined None -> "user-defined"
							| UserDefined (Some o) -> o
						);
						"links", jarray (List.map jstring data.m_links)
					] in

					(jobject fields)
				) all))
			)
		);
		"display/defines", (fun hctx ->
			let include_compiler_defines = hctx.jsonrpc#get_bool_param "compiler" in
			let include_user_defines = hctx.jsonrpc#get_bool_param "user" in

			defer AfterInitMacros (fun () ->
				let all = Define.get_define_list hctx.com.user_defines in
				let all = List.filter (fun (_, (data:Define.define_infos)) ->
					match data.d_origin with
					| Compiler when include_compiler_defines -> true
					| UserDefined _ when include_user_defines -> true
					| _ -> false
				) all in

				Result (jarray (List.map (fun (t, (data:Define.define_infos)) ->
					let fields = [
						"name", jstring t;
						"doc", jstring data.d_doc;
						"parameters", jarray (List.map jstring data.d_params);
						"platforms", jarray (List.map (fun p -> jstring (platform_name p)) data.d_platforms);
						"origin", jstring (match data.d_origin with
							| Compiler -> "haxe compiler"
							| UserDefined None -> "user-defined"
							| UserDefined (Some o) -> o
						);
						"deprecated", jopt jstring data.d_deprecated;
						"links", jarray (List.map jstring data.d_links);
						"reserved", jopt jbool data.d_reserved
					] in

					(jobject fields)
				) all))
			)
		);
		"server/resetCache", (fun hctx ->
			hctx.com.cs#clear;
			supports_resolve := false;
			DisplayException.reset();
			ServerConfig.reset();
			Result (jobject [
				"success", jbool true
			]);
		);
		"server/resetState", (fun hctx ->
			hctx.com.sctx.persistent_cwd <- None;
			hctx.com.cs#soft_clear;
			supports_resolve := false;
			DisplayException.reset();
			ServerConfig.reset();
			Result (jobject [
				"success", jbool true
			]);
		);
		"server/setCwd", (fun hctx ->
			let dir = hctx.jsonrpc#get_string_param "dir" in
			let dir = Path.get_full_path dir in
			if not (Sys.file_exists dir && Sys.is_directory dir) then
				raise (Api_error (jstring ("Invalid directory: " ^ dir)));
			hctx.com.sctx.persistent_cwd <- Some dir;
			Result jnull
		);
		"server/gcCompact", (fun hctx ->
			let t0 = Extc.time() in
			let stats_before = Gc.stat() in
			Gc.compact();
			let stats = Gc.quick_stat() in
			Result (jobject [
				"time", jfloat (Extc.time() -. t0);
				"before", jint (stats_before.Gc.heap_words * Sys.word_size / 8);
				"after", jint (stats.Gc.heap_words * Sys.word_size / 8);
			]);
		);
		"server/readClassPaths", (fun hctx ->
			let wait = hctx.jsonrpc#has_params && hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_bool_param "wait") false in
			defer AfterInitMacros (fun () ->
				let cc = hctx.display#get_cs#get_context (Define.get_signature hctx.com.defines) in
				cc#set_initialized true;
				DisplayToplevel.read_class_paths hctx.com ["init"];
				if wait then
					hctx.display#get_cs#run_tasks true (fun task -> match task#get_id with
						| "explore" :: _ -> true
						| _ -> false
					);
				let files = hctx.display#get_cs#get_files in
				Result (jobject [
					"files", jint (List.length files)
				]);
			)
		);
		"server/contexts", (fun hctx ->
			let l = List.map (fun cc -> cc#get_json) hctx.display#get_cs#get_contexts in
			let l = List.filter (fun json -> json <> JNull) l in
			Result (jarray l)
		);
		"server/modules", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let cc = hctx.display#get_cs#get_context sign in
			let open HxbData in
			let l = Hashtbl.fold (fun _ m acc ->
				if m.mc_extra.m_kind <> MFake then jstring (s_type_path m.mc_path) :: acc else acc
			) cc#get_hxb [] in
			Result (jarray l)
		);
		"server/module", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "path") in
			let cs = hctx.display#get_cs in
			let cc = cs#get_context sign in
			let typing_mode:HxbData.typing_mode = if Define.defined hctx.com.defines Define.DisableHxbOptimizations then FullTyping else AllowPartialTyping in
			try
				let m = find_module ~typing_mode hctx.com cc path in
				Result (generate_module (cc#get_hxb) (find_module ~typing_mode hctx.com cc) m)
			with Not_found ->
				Error (jstring "No such module")
		);
		"server/type", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "modulePath") in
			let typeName = hctx.jsonrpc#get_string_param "typeName" in
			let cc = hctx.display#get_cs#get_context sign in
			try
				let m = find_module ~typing_mode:FullTyping hctx.com cc path in
				let rec loop mtl = match mtl with
					| [] ->
						Error (jstring "No such type")
					| mt :: mtl ->
						begin match mt with
						| TClassDecl c -> c.cl_restore()
						| _ -> ()
						end;
						let infos = t_infos mt in
						if snd infos.mt_path = typeName then begin
							let ctx = Genjson.create_context GMMinimum in
							Result (Genjson.generate_module_type ctx mt)
						end else
							loop mtl
				in
				loop m.m_types
			with Not_found ->
				Error (jstring "No such module")
		);
		"server/typeContexts", (fun hctx ->
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "modulePath") in
			let typeName = hctx.jsonrpc#get_string_param "typeName" in
			let contexts = hctx.display#get_cs#get_contexts in

			Result (jarray (List.fold_left (fun acc cc ->
				match cc#find_module_opt path with
				| None -> acc
				| Some(m) ->
					let rec loop mtl = match mtl with
						| [] ->
							acc
						| mt :: mtl ->
							begin match mt with
							| TClassDecl c -> c.cl_restore()
							| _ -> ()
							end;
							if snd (t_infos mt).mt_path = typeName then
								cc#get_json :: acc
							else
								loop mtl
					in
					loop m.m_types
			) [] contexts))
		);
		"server/moduleCreated", (fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let file = Path.get_full_path file in
			let key = hctx.com.part_scope.file_keys#get file in
			let cs = hctx.display#get_cs in
			List.iter (fun cc ->
				Hashtbl.replace cc#get_removed_files key (ClassPaths.create_resolved_file file hctx.com.empty_class_path)
			) cs#get_contexts;
			Result (jstring file);
		);
		"server/files", (fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let cc = hctx.display#get_cs#get_context sign in
			let files = Hashtbl.fold (fun file cfile acc -> (file,cfile) :: acc) cc#get_files [] in
			let files = List.sort (fun (file1,_) (file2,_) -> compare file1 file2) files in
			let files = List.map (fun (fkey,cfile) ->
				jobject [
					"file",jstring cfile.c_file_path.file;
					"time",jfloat cfile.c_time;
					"pack",jstring (String.concat "." cfile.c_package);
					"moduleName",jopt jstring cfile.c_module_name;
				]
			) files in
			Result (jarray files)
		);
		"server/invalidate", (fun hctx ->
			let file = hctx.jsonrpc#get_string_param "file" in
			let fkey = hctx.com.part_scope.file_keys#get file in
			let cs = hctx.display#get_cs in
			cs#taint_modules fkey ServerInvalidate;
			cs#remove_files fkey;
			Result jnull
		);
		"server/configure", (fun hctx ->
			let l = ref (List.map (fun (name,value) ->
				let value = hctx.jsonrpc#get_bool "value" value in
				try
					ServerMessage.set_by_name name value;
					jstring (Printf.sprintf "Printing %s %s" name (if value then "enabled" else "disabled"))
				with Not_found ->
					raise (api_error (jstring ("Invalid print parame name: " ^ name)))
			) (hctx.jsonrpc#get_opt_param (fun () -> (hctx.jsonrpc#get_object_param "print")) [])) in
			hctx.jsonrpc#get_opt_param (fun () ->
				let b = hctx.jsonrpc#get_bool_param "noModuleChecks" in
				ServerConfig.do_not_check_modules := b;
				l := jstring ("Module checks " ^ (if b then "disabled" else "enabled")) :: !l;
				()
			) ();
			Result (jarray !l)
		);
		"server/memory",(fun hctx ->
			let j = DisplayMemory.get_memory_json hctx.display#get_cs MCache in
			Result j
		);
		"server/memory/context",(fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let j = DisplayMemory.get_memory_json hctx.display#get_cs (MContext sign) in
			Result j
		);
		"server/memory/module",(fun hctx ->
			let sign = Digest.from_hex (hctx.jsonrpc#get_string_param "signature") in
			let path = Path.parse_path (hctx.jsonrpc#get_string_param "path") in
			let j = DisplayMemory.get_memory_json hctx.display#get_cs (MModule(sign,path)) in
			Result j
		);
		"typer/compiledTypes", (fun hctx ->
			defer AfterFilters (fun () ->
				let ctx = create_context GMFull in
				let l = List.map (generate_module_type ctx) hctx.com.types in
				Result (jarray l)
			)
		);
	] in
	List.iter (fun (s,f) -> Hashtbl.add h s f) l;
	h

let run_on_com jsonrpc f com =
	let catch_api_error f =
		try f() with Api_error json -> Error json
	in
	let result_handler = com.request_scope.result_handler in
	let display = new display_handler jsonrpc com com.cs in

	let hctx = {
		com = com;
		jsonrpc = jsonrpc;
		display = display;
	} in
	let maybe_send_response = function
		| NoResponse ->
			NeedsTyping
		| Result json ->
			result_handler.send_result json;
			Completed
		| Error json ->
			result_handler.send_error [json];
			Completed
		| Deferred(deferrence,f) ->
			let send_response = function
				| NoResponse ->
					()
				| Result json ->
					result_handler.send_result_raise json;
				| Error json ->
					result_handler.send_error_raise [json];
				| Deferred _ ->
					die "" __LOC__
			in
			let f () = send_response (catch_api_error f) in
			begin match deferrence with
			| AfterInitMacros ->
				com.callbacks#add_after_init_macros f
			| AfterFilters ->
				com.callbacks#add_after_filters f
			end;
			NeedsTyping
	in
	maybe_send_response (catch_api_error (fun () -> f hctx))

let create_json_result_handler timer_ctx io jsonrpc =
	let send_result send json =
		flush stdout;
		flush stderr;
		let fl = [
			"result",json;
			"timestamp",jfloat (Unix.gettimeofday ());
		] in
		let fl = if timer_ctx.measure_times = Yes then begin
			let _,_,root = Timer.build_times_tree timer_ctx in
			begin match json_of_times root with
			| None -> fl
			| Some jo -> ("timers",jo) :: fl
			end
		end else fl in
		let jo = jobject fl in
		send (JsonRpc.result jsonrpc#get_id jo)
	in

	let send_error send jl =
		send (JsonRpc.error jsonrpc#get_id 0 ~data:(Some (JArray jl)) "Compiler error")
	in

	let send_result_raise = send_result (send_json_raise io) in
	let send_result_noraise = send_result (send_json io) in
	let send_error_raise = send_error (send_json_raise io) in
	let send_error_noraise = send_error (send_json io) in

	let method_name = jsonrpc#get_method_name in
	let f = try
		Hashtbl.find handler method_name
	with Not_found ->
		raise_method_not_found jsonrpc#get_id method_name
	in

	{
		send_result = send_result_noraise;
		send_result_raise = send_result_raise;
		send_error = send_error_noraise;
		send_error_raise = send_error_raise;
		flush_messages = (fun has_error com ->
			(* flush_messages in the JSON-RPC handler is always an error state:
			   a successful completion raises JsonCompleted before flush_context is
			   called, so reaching here means no meaningful result was sent. *)
			if has_error then begin
				let errors = List.map (fun cm ->
					Json.JObject [
						"severity",Json.JInt (Message.MessageSeverity.to_int (Message.cm_severity cm));
						"location",Genjson.generate_pos_as_location cm.cm_pos;
						"message",Json.JString cm.cm_message;
					]
				) (List.rev com.part_scope.messages) in
				send_error_raise errors;
			end else
				(* No completion point was found — send an appropriate no-result
				   response so the client is not left waiting. *)
				DisplayException.send_no_result_raise com
		);
		set_com = run_on_com jsonrpc f;
	}