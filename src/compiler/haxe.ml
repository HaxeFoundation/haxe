(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

(*
	Conventions:
	- e: expression (typed or untyped)
	- c: class
	- en: enum
	- td: typedef (tdef)
	- a: abstract
	- an: anon
	- tf: tfunc
	- cf: class_field
	- ef: enum_field
	- t: type (t)
	- ct: complex_type
	- v: local variable (tvar)
	- m: module (module_def)
	- mt: module_type
	- p: pos

	"param" refers to type parameters
	"arg" refers to function arguments
	leading s_ means function returns string
	trailing l means list (but we also use natural plurals such as "metas")
	semantic suffixes may be used freely (e.g. e1, e_if, e')
*)

open Printf
open Common
open DisplayTypes.DisplayMode
open DisplayTypes.CompletionResultKind
open DisplayException
open Type
open Server
open Globals
open Filename

exception Abort
exception HelpMessage of string

let executable_path() =
	Extc.executable_path()

let message ctx msg =
	ctx.messages <- msg :: ctx.messages

let deprecated = []

let limit_string s offset =
	let rest = 80 - offset in
	let words = ExtString.String.nsplit s " " in
	let rec loop i words = match words with
		| word :: words ->
			if String.length word + i + 1 > rest then (Printf.sprintf "\n%*s" offset "") :: word :: loop (String.length word) words
			else (if i = 0 then "" else " ") :: word :: loop (i + 1 + String.length word) words
		| [] ->
			[]
	in
	String.concat "" (loop 0 words)

let error ctx msg p =
	let msg = try List.assoc msg deprecated with Not_found -> msg in
	message ctx (CMError(msg,p));
	ctx.has_error <- true

let reserved_flags = [
	"true";"false";"null";"cross";"js";"lua";"neko";"flash";"php";"cpp";"cs";"java";"python";
	"swc";"macro";"sys";"static";"utf16";"haxe";"haxe_ver"
	]

let reserved_flag_namespaces = ["target"]

let delete_file f = try Sys.remove f with _ -> ()

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

let add_libs com libs =
	let global_repo = List.exists (fun a -> a = "--haxelib-global") com.args in
	let call_haxelib() =
		let t = Timer.timer ["haxelib"] in
		let cmd = "haxelib" ^ (if global_repo then " --global" else "") ^ " path " ^ String.concat " " libs in
		let pin, pout, perr = Unix.open_process_full cmd (Unix.environment()) in
		let lines = Std.input_list pin in
		let err = Std.input_list perr in
		let ret = Unix.close_process_full (pin,pout,perr) in
		if ret <> Unix.WEXITED 0 then failwith (match lines, err with
			| [], [] -> "Failed to call haxelib (command not found ?)"
			| [], [s] when ExtString.String.ends_with (ExtString.String.strip s) "Module not found: path" -> "The haxelib command has been strip'ed, please install it again"
			| _ -> String.concat "\n" (lines@err));
		t();
		lines
	in
	match libs with
	| [] -> []
	| _ ->
		let lines = match CompilationServer.get() with
			| Some cs ->
				(try
					(* if we are compiling, really call haxelib since library path might have changed *)
					if not com.display.dms_display then raise Not_found;
					cs#find_haxelib libs
				with Not_found ->
					let lines = call_haxelib() in
					cs#cache_haxelib libs lines;
					lines)
			| _ -> call_haxelib()
		in
		let extra_args = ref [] in
		let lines = List.fold_left (fun acc l ->
			let l = ExtString.String.strip l in
			if l = "" then acc else
			if l.[0] <> '-' then l :: acc else
			match (try ExtString.String.split l " " with _ -> l, "") with
			| ("-L",dir) ->
				com.neko_libs <- String.sub l 3 (String.length l - 3) :: com.neko_libs;
				acc
			| param, value ->
				extra_args := param :: !extra_args;
				if value <> "" then extra_args := value :: !extra_args;
				acc
		) [] lines in
		com.class_path <- lines @ com.class_path;
		List.rev !extra_args

let run_command ctx cmd =
	let h = Hashtbl.create 0 in
	Hashtbl.add h "__file__" ctx.com.file;
	Hashtbl.add h "__platform__" (platform_name ctx.com.platform);
	let t = Timer.timer ["command"] in
	let cmd = expand_env ~h:(Some h) cmd in
	let len = String.length cmd in
	if len > 3 && String.sub cmd 0 3 = "cd " then begin
		Sys.chdir (String.sub cmd 3 (len - 3));
		0
	end else
	let binary_string s =
		if not Globals.is_windows then s else String.concat "\n" (Str.split (Str.regexp "\r\n") s)
	in
	let pout, pin, perr = Unix.open_process_full cmd (Unix.environment()) in
	let bout = Buffer.create 0 in
	let berr = Buffer.create 0 in
	let read_content channel buf =
		Buffer.add_string buf (IO.read_all (IO.input_channel channel));
	in
	let tout = Thread.create (fun() -> read_content pout bout) () in
	read_content perr berr;
	Thread.join tout;
	let result = (match Unix.close_process_full (pout,pin,perr) with Unix.WEXITED c | Unix.WSIGNALED c | Unix.WSTOPPED c -> c) in
	let serr = binary_string (Buffer.contents berr) in
	let sout = binary_string (Buffer.contents bout) in
	if serr <> "" then ctx.messages <- CMError((if serr.[String.length serr - 1] = '\n' then String.sub serr 0 (String.length serr - 1) else serr),null_pos) :: ctx.messages;
	if sout <> "" then ctx.com.print (sout ^ "\n");
	t();
	result

module Initialize = struct
	let set_platform com pf file =
		if com.platform <> Cross then failwith "Multiple targets";
		Common.init_platform com pf;
		com.file <- file;
		if (pf = Flash) && file_extension file = "swc" then Common.define com Define.Swc

	let initialize_target ctx com classes =
		let add_std dir =
			com.class_path <- List.filter (fun s -> not (List.mem s com.std_path)) com.class_path @ List.map (fun p -> p ^ dir ^ "/_std/") com.std_path @ com.std_path
		in
		match com.platform with
			| Cross ->
				(* no platform selected *)
				set_platform com Cross "";
				"?"
			| Flash ->
				let rec loop = function
					| [] -> ()
					| (v,_) :: _ when v > com.flash_version -> ()
					| (v,def) :: l ->
						Common.raw_define com ("flash" ^ def);
						loop l
				in
				loop Common.flash_versions;
				Common.raw_define com "flash";
				com.package_rules <- PMap.remove "flash" com.package_rules;
				add_std "flash";
				"swf"
			| Neko ->
				add_std "neko";
				"n"
			| Js ->
				let es_version =
					try
						int_of_string (Common.defined_value com Define.JsEs)
					with
					| Not_found ->
						(Common.define_value com Define.JsEs "5"; 5)
					| _ ->
						0
				in

				if es_version < 3 || es_version = 4 then (* we don't support ancient and there's no 4th *)
					failwith "Invalid -D js-es value";

				if es_version >= 5 then Common.raw_define com "js-es5"; (* backward-compatibility *)

				add_std "js";
				"js"
			| Lua ->
				add_std "lua";
				"lua"
			| Php ->
				add_std "php";
				"php"
			| Cpp ->
				Common.define_value com Define.HxcppApiLevel "400";
				add_std "cpp";
				if Common.defined com Define.Cppia then
					classes := (Path.parse_path "cpp.cppia.HostClasses" ) :: !classes;
				"cpp"
			| Cs ->
				let old_flush = ctx.flush in
				ctx.flush <- (fun () ->
					com.native_libs.net_libs <- [];
					old_flush()
				);
				Dotnet.before_generate com;
				add_std "cs"; "cs"
			| Java ->
				let old_flush = ctx.flush in
				ctx.flush <- (fun () ->
					List.iter (fun java_lib -> java_lib#close) com.native_libs.java_libs;
					com.native_libs.java_libs <- [];
					old_flush()
				);
				Java.before_generate com;
				if defined com Define.Jvm then begin
					add_std "jvm";
					com.package_rules <- PMap.remove "jvm" com.package_rules;
				end;
				add_std "java";
				"java"
			| Python ->
				add_std "python";
				if not (Common.defined com Define.PythonVersion) then
					Common.define_value com Define.PythonVersion "3.3";
				"python"
			| Hl ->
				add_std "hl";
				if not (Common.defined com Define.HlVer) then Define.define_value com.defines Define.HlVer (try Std.input_file (Common.find_file com "hl/hl_version") with Not_found -> assert false);
				"hl"
			| Eval ->
				add_std "eval";
				"eval"
end

let generate tctx ext interp swf_header =
	let com = tctx.Typecore.com in
	(* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. *)
	if file_extension com.file = ext then delete_file com.file;
	if com.platform = Flash || com.platform = Cpp || com.platform = Hl then List.iter (Codegen.fix_overrides com) com.types;
	if Common.defined com Define.Dump then begin
		Codegen.Dump.dump_types com;
		Option.may Codegen.Dump.dump_types (com.get_macros())
	end;
	if Common.defined com Define.DumpDependencies then begin
		Codegen.Dump.dump_dependencies com;
		if not tctx.Typecore.in_macro then match tctx.Typecore.g.Typecore.macros with
			| None -> ()
			| Some(_,ctx) -> Codegen.Dump.dump_dependencies ~target_override:(Some "macro") ctx.Typecore.com
	end;
	begin match com.platform with
		| Neko | Hl | Eval when interp -> ()
		| Cpp when Common.defined com Define.Cppia -> ()
		| Cpp | Cs | Java | Php -> Path.mkdir_from_path (com.file ^ "/.")
		| _ -> Path.mkdir_from_path com.file
	end;
	if interp then
		Std.finally (Timer.timer ["interp"]) MacroContext.interpret tctx
	else if com.platform = Cross then
		()
	else begin
		let generate,name = match com.platform with
		| Flash ->
			Genswf.generate swf_header,"swf"
		| Neko ->
			Genneko.generate,"neko"
		| Js ->
			Genjs.generate,"js"
		| Lua ->
			Genlua.generate,"lua"
		| Php ->
			Genphp7.generate,"php"
		| Cpp ->
			Gencpp.generate,"cpp"
		| Cs ->
			Gencs.generate,"cs"
		| Java ->
			if Common.defined com Jvm then
				Genjvm.generate,"java"
			else
				Genjava.generate,"java"
		| Python ->
			Genpy.generate,"python"
		| Hl ->
			Genhl.generate,"hl"
		| Eval ->
			(fun _ -> MacroContext.interpret tctx),"eval"
		| Cross ->
			assert false
		in
		Common.log com ("Generating " ^ name ^ ": " ^ com.file);
		let t = Timer.timer ["generate";name] in
		generate com;
		t()
	end

let get_std_class_paths () =
	try
		let p = Sys.getenv "HAXE_STD_PATH" in
		let rec loop = function
			| drive :: path :: l ->
				if String.length drive = 1 && ((drive.[0] >= 'a' && drive.[0] <= 'z') || (drive.[0] >= 'A' && drive.[0] <= 'Z')) then
					(drive ^ ":" ^ path) :: loop l
				else
					drive :: loop (path :: l)
			| l ->
				l
		in
		let parts = Str.split_delim (Str.regexp "[;:]") p in
		"" :: List.map Path.add_trailing_slash (loop parts)
	with Not_found ->
		let base_path = Path.get_real_path (try executable_path() with _ -> "./") in
		if Sys.os_type = "Unix" then
			let prefix_path = Filename.dirname base_path in
			let lib_path = Filename.concat prefix_path "lib" in
			let share_path = Filename.concat prefix_path "share" in
			[
				"";
				Path.add_trailing_slash (Filename.concat lib_path "haxe/std");
				Path.add_trailing_slash (Filename.concat lib_path "haxe/extraLibs");
				Path.add_trailing_slash (Filename.concat share_path "haxe/std");
				Path.add_trailing_slash (Filename.concat share_path "haxe/extraLibs");
				Path.add_trailing_slash (Filename.concat base_path "std");
				Path.add_trailing_slash (Filename.concat base_path "extraLibs")
			]
		else
			[
				"";
				Path.add_trailing_slash (Filename.concat base_path "std");
				Path.add_trailing_slash (Filename.concat base_path "extraLibs")
			]

let setup_common_context ctx com =
	Common.define_value com Define.HaxeVer (Printf.sprintf "%.3f" (float_of_int Globals.version /. 1000.));
	Common.raw_define com "haxe3";
	Common.raw_define com "haxe4";
	Common.define_value com Define.Haxe (s_version false);
	Common.define_value com Define.Dce "std";
	com.info <- (fun msg p -> message ctx (CMInfo(msg,p)));
	com.warning <- (fun msg p -> message ctx (CMWarning(msg,p)));
	com.error <- error ctx;
	let filter_messages = (fun keep_errors predicate -> (List.filter (fun msg ->
		(match msg with
		| CMError(_,_) -> keep_errors;
		| CMInfo(_,_) | CMWarning(_,_) -> predicate msg;)
	) (List.rev ctx.messages))) in
	com.get_messages <- (fun () -> (List.map (fun msg ->
		(match msg with
		| CMError(_,_) -> assert false;
		| CMInfo(_,_) | CMWarning(_,_) -> msg;)
	) (filter_messages false (fun _ -> true))));
	com.filter_messages <- (fun predicate -> (ctx.messages <- (List.rev (filter_messages true predicate))));
	if CompilationServer.runs() then com.run_command <- run_command ctx;
	com.class_path <- get_std_class_paths ();
	com.std_path <- List.filter (fun p -> ExtString.String.ends_with p "std/" || ExtString.String.ends_with p "std\\") com.class_path

let process_args arg_spec =
	(* Takes a list of arg specs including some custom info, and generates a
	list in the format Arg.parse_argv wants. Handles multiple official or
	deprecated names for the same arg; deprecated versions will display a
	warning. *)
	List.flatten(List.map (fun (cat, ok, dep, spec, hint, doc) ->
		(* official argument names *)
		(List.map (fun (arg) -> (arg, spec, doc)) ok) @
		(* deprecated argument names *)
		(* let dep_msg arg = (Printf.sprintf "WARNING: %s is deprecated" arg) ^ (if List.length ok > 0 then (Printf.sprintf ". Use %s instead" (String.concat "/" ok)) else "") in *)
		(* For now, these warnings are a noop. Can replace this function to
		enable error output: *)
		(* let dep_fun = prerr_endline (dep_msg arg) in *)
		let dep_fun arg spec = () in
		let dep_spec arg spec = match spec with
			| Arg.String f -> Arg.String (fun x -> dep_fun arg spec; f x)
			| Arg.Unit f -> Arg.Unit (fun x -> dep_fun arg spec; f x)
			| Arg.Bool f -> Arg.Bool (fun x -> dep_fun arg spec; f x)
			| _ -> spec in
		(List.map (fun (arg) -> (arg, dep_spec arg spec, doc)) dep)
	) arg_spec)

let usage_string ?(print_cat=true) arg_spec usage =
	let make_label = fun names hint -> Printf.sprintf "%s %s" (String.concat ", " names) hint in
	let args = (List.filter (fun (cat, ok, dep, spec, hint, doc) -> (List.length ok) > 0) arg_spec) in
	let cat_order = ["Target";"Compilation";"Optimization";"Debug";"Batch";"Services";"Compilation Server";"Target-specific";"Miscellaneous"] in
	let cats = List.filter (fun x -> List.mem x (List.map (fun (cat, _, _, _, _, _) -> cat) args)) cat_order in
	let max_length = List.fold_left max 0 (List.map String.length (List.map (fun (_, ok, _, _, hint, _) -> make_label ok hint) args)) in
	usage ^ (String.concat "\n" (List.flatten (List.map (fun cat -> (if print_cat then ["\n"^cat^":"] else []) @ (List.map (fun (cat, ok, dep, spec, hint, doc) ->
		let label = make_label ok hint in
		Printf.sprintf "  %s%s  %s" label (String.make (max_length - (String.length label)) ' ') doc
	) (List.filter (fun (cat', _, _, _, _, _) -> (if List.mem cat' cat_order then cat' else "Miscellaneous") = cat) args))) cats)))

let process_display_configuration ctx =
	let com = ctx.com in
	if com.display.dms_kind <> DMNone then begin
		com.warning <-
			if com.display.dms_error_policy = EPCollect then
				(fun s p -> add_diagnostics_message com s p DKCompilerError DisplayTypes.DiagnosticsSeverity.Warning)
			else
				(fun msg p -> message ctx (CMWarning(msg,p)));
		com.error <- error ctx;
	end;
	Lexer.old_format := Common.defined com Define.OldErrorFormat;
	if !Lexer.old_format && !Parser.in_display then begin
		let p = DisplayPosition.display_position#get in
		(* convert byte position to utf8 position *)
		try
			let content = Std.input_file ~bin:true (Path.get_real_path p.pfile) in
			let pos = UTF8.length (String.sub content 0 p.pmin) in
			DisplayPosition.display_position#set { p with pmin = pos; pmax = pos }
		with _ ->
			() (* ignore *)
	end

let run_or_diagnose com f arg =
	let handle_diagnostics msg p kind =
		add_diagnostics_message com msg p kind DisplayTypes.DiagnosticsSeverity.Error;
		Diagnostics.run com;
	in
	match com.display.dms_kind with
	| DMDiagnostics _ ->
		begin try
			f arg
		with
		| Error.Error(msg,p) ->
			handle_diagnostics (Error.error_msg msg) p DisplayTypes.DiagnosticsKind.DKCompilerError
		| Parser.Error(msg,p) ->
			handle_diagnostics (Parser.error_msg msg) p DisplayTypes.DiagnosticsKind.DKParserError
		| Lexer.Error(msg,p) ->
			handle_diagnostics (Lexer.error_msg msg) p DisplayTypes.DiagnosticsKind.DKParserError
		end
	| _ ->
		f arg

let create_typer_context ctx native_libs =
	let com = ctx.com in
	ctx.setup();
	Common.log com ("Classpath: " ^ (String.concat ";" com.class_path));
	Common.log com ("Defines: " ^ (String.concat ";" (PMap.foldi (fun k v acc -> (match v with "1" -> k | _ -> k ^ "=" ^ v) :: acc) com.defines.Define.values [])));
	Typecore.type_expr_ref := (fun ?(mode=MGet) ctx e with_type -> Typer.type_expr ~mode ctx e with_type);
	List.iter (fun f -> f ()) (List.rev com.callbacks#get_before_typer_create);
	(* Native lib pass 1: Register *)
	let fl = List.map (fun (file,extern) -> NativeLibraryHandler.add_native_lib com file extern) (List.rev native_libs) in
	(* Native lib pass 2: Initialize *)
	List.iter (fun f -> f()) fl;
	Typer.create com

(** Creates the typer context and types [classes] into it. *)
let do_type tctx config_macros classes =
	let com = tctx.Typecore.com in
	let t = Timer.timer ["typing"] in
	Option.may (fun cs -> CommonCache.maybe_add_context_sign cs com "before_init_macros") (CompilationServer.get ());
	com.stage <- CInitMacrosStart;
	List.iter (MacroContext.call_init_macro tctx) (List.rev config_macros);
	com.stage <- CInitMacrosDone;
	CommonCache.lock_signature com "after_init_macros";
	List.iter (fun f -> f ()) (List.rev com.callbacks#get_after_init_macros);
	run_or_diagnose com (fun () ->
		if com.display.dms_kind <> DMNone then Option.may (DisplayTexpr.check_display_file tctx) (CompilationServer.get ());
		List.iter (fun cpath -> ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath null_pos)) (List.rev classes);
		Finalization.finalize tctx;
	) ();
	com.stage <- CTypingDone;
	(* If we are trying to find references, let's syntax-explore everything we know to check for the
		identifier we are interested in. We then type only those modules that contain the identifier. *)
	begin match !CompilationServer.instance,com.display.dms_kind with
		| Some cs,(DMUsage _ | DMImplementation) -> FindReferences.find_possible_references tctx cs;
		| _ -> ()
	end;
	t()

let load_display_module_in_macro tctx display_file_dot_path clear = match display_file_dot_path with
	| Some cpath ->
		let p = null_pos in
		begin try
			let open Typecore in
			let _, mctx = MacroContext.get_macro_context tctx p in
			(* Tricky stuff: We want to remove the module from our lookups and load it again in
				display mode. This covers some cases like --macro typing it in non-display mode (issue #7017). *)
			if clear then begin
				begin try
					let m = Hashtbl.find mctx.g.modules cpath in
					Hashtbl.remove mctx.g.modules cpath;
					Hashtbl.remove mctx.g.types_module cpath;
					List.iter (fun mt ->
						let ti = t_infos mt in
						Hashtbl.remove mctx.g.modules ti.mt_path;
						Hashtbl.remove mctx.g.types_module ti.mt_path;
					) m.m_types
				with Not_found ->
					()
				end;
			end;
			let _ = MacroContext.load_macro_module tctx cpath true p in
			Finalization.finalize mctx;
			Some mctx
		with DisplayException _ | Parser.TypePath _ as exc ->
			raise exc
		| _ ->
			None
		end
	| None ->
		None

let handle_display ctx tctx display_file_dot_path =
	let com = ctx.com in
	if not ctx.com.display.dms_display && ctx.has_error then raise Abort;
	begin match ctx.com.display.dms_kind,!Parser.delayed_syntax_completion with
		| DMDefault,Some(kind,subj) -> DisplayOutput.handle_syntax_completion com kind subj
		| _ -> ()
	end;
	if ctx.com.display.dms_exit_during_typing then begin
		if ctx.has_next || ctx.has_error then raise Abort;
		(* If we didn't find a completion point, load the display file in macro mode. *)
		ignore(load_display_module_in_macro tctx display_file_dot_path true);
		let no_completion_point_found = "No completion point was found" in
		match com.json_out with
		| Some _ -> (match ctx.com.display.dms_kind with
			| DMDefault -> raise (DisplayException(DisplayFields None))
			| DMSignature -> raise (DisplayException(DisplaySignatures None))
			| DMHover -> raise (DisplayException(DisplayHover None))
			| DMDefinition | DMTypeDefinition -> raise_positions []
			| _ -> failwith no_completion_point_found)
		| None ->
			failwith no_completion_point_found;
	end

let filter ctx tctx display_file_dot_path =
	let com = ctx.com in
	com.stage <- CFilteringStart;
	let t = Timer.timer ["filters"] in
	let main, types, modules = run_or_diagnose com Finalization.generate tctx in
	com.main <- main;
	com.types <- types;
	com.modules <- modules;
	(* Special case for diagnostics: We don't want to load the display file in macro mode because there's a chance it might not be
		macro-compatible. This means that we might some macro-specific diagnostics, but I don't see what we could do about that. *)
	let should_load_in_macro = match ctx.com.display.dms_kind with
		(* Special case for the special case: If the display file has a block which becomes active if `macro` is defined, we can safely
		   type the module in macro context. (#8682). *)
		| DMDiagnostics _ -> com.display_information.display_module_has_macro_defines
		| _ -> true
	in
	if ctx.com.display.dms_force_macro_typing && should_load_in_macro then begin
		match load_display_module_in_macro  tctx display_file_dot_path false with
		| None -> ()
		| Some mctx ->
			(* We don't need a full macro flush here because we're not going to run any macros. *)
			let _, types, modules = Finalization.generate mctx in
			mctx.Typecore.com.types <- types;
			mctx.Typecore.com.Common.modules <- modules
	end;
	DisplayOutput.process_global_display_mode com tctx;
	DeprecationCheck.run com;
	Filters.run com tctx main;
	t()

let check_auxiliary_output com xml_out json_out =
	begin match xml_out with
		| None -> ()
		| Some "hx" ->
			Genhxold.generate com
		| Some file ->
			Common.log com ("Generating xml: " ^ file);
			Path.mkdir_from_path file;
			Genxml.generate com file
	end;
	begin match json_out with
		| None -> ()
		| Some file ->
			Common.log com ("Generating json : " ^ file);
			Path.mkdir_from_path file;
			Genjson.generate com.types file
	end

let parse_host_port hp =
	let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
	let port = try int_of_string port with _ -> raise (Arg.Bad "Invalid port") in
	host, port

let rec process_params create pl =
	let each_params = ref [] in
	let rec loop acc = function
		| [] ->
			let ctx = create (!each_params @ (List.rev acc)) in
			init ctx;
			ctx.flush()
		| "--next" :: l when acc = [] -> (* skip empty --next *)
			loop [] l
		| "--next" :: l ->
			let ctx = create (!each_params @ (List.rev acc)) in
			ctx.has_next <- true;
			init ctx;
			ctx.flush();
			loop [] l
		| "--each" :: l ->
			each_params := List.rev acc;
			loop [] l
		| "--cwd" :: dir :: l | "-C" :: dir :: l ->
			(* we need to change it immediately since it will affect hxml loading *)
			(try Unix.chdir dir with _ -> raise (Arg.Bad ("Invalid directory: " ^ dir)));
			(* Push the --cwd arg so the arg processor know we did something. *)
			loop (dir :: "--cwd" :: acc) l
		| "--connect" :: hp :: l ->
			(match CompilationServer.get() with
			| None ->
				let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
				do_connect host (try int_of_string port with _ -> raise (Arg.Bad "Invalid port")) ((List.rev acc) @ l)
			| Some _ ->
				(* already connected : skip *)
				loop acc l)
		| "--run" :: cl :: args ->
			let acc = cl :: "-x" :: acc in
			let ctx = create (!each_params @ (List.rev acc)) in
			ctx.com.sys_args <- args;
			init ctx;
			ctx.flush()
		| arg :: l ->
			match List.rev (ExtString.String.nsplit arg ".") with
			| "hxml" :: _ when (match acc with "-cmd" :: _ | "--cmd" :: _ -> false | _ -> true) ->
				let acc, l = (try acc, parse_hxml arg @ l with Not_found -> (arg ^ " (file not found)") :: acc, l) in
				loop acc l
			| _ -> loop (arg :: acc) l
	in
	(* put --display in front if it was last parameter *)
	let pl = (match List.rev pl with
		| file :: "--display" :: pl when file <> "memory" -> "--display" :: file :: List.rev pl
		| _ -> pl
	) in
	loop [] pl

and init ctx =
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2020 Haxe Foundation\nUsage: haxe%s <target> [options] [hxml files...]\n"
		(s_version true) (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let com = ctx.com in
	let classes = ref [([],"Std")] in
try
	set_binary_mode_out stdout true;
	set_binary_mode_out stderr true;
	let xml_out = ref None in
	let json_out = ref None in
	let swf_header = ref None in
	let cmds = ref [] in
	let config_macros = ref [] in
	let cp_libs = ref [] in
	let added_libs = Hashtbl.create 0 in
	let no_output = ref false in
	let did_something = ref false in
	let force_typing = ref false in
	let pre_compilation = ref [] in
	let interp = ref false in
	let swf_version = ref false in
	let native_libs = ref [] in
	let add_native_lib file extern = native_libs := (file,extern) :: !native_libs in
	setup_common_context ctx com;
	let define f = Arg.Unit (fun () -> Common.define com f) in
	let process_ref = ref (fun args -> ()) in
	let process_libs() =
		let libs = List.filter (fun l -> not (Hashtbl.mem added_libs l)) (List.rev !cp_libs) in
		cp_libs := [];
		List.iter (fun l -> Hashtbl.add added_libs l ()) libs;
		(* immediately process the arguments to insert them at the place -lib was defined *)
		match add_libs com libs with
		| [] -> ()
		| args -> (!process_ref) args
	in
	(* category, official names, deprecated names, arg spec, usage hint, doc *)
	let basic_args_spec = [
		("Target",["--js"],["-js"],Arg.String (Initialize.set_platform com Js),"<file>","compile code to JavaScript file");
		("Target",["--lua"],["-lua"],Arg.String (Initialize.set_platform com Lua),"<file>","compile code to Lua file");
		("Target",["--swf"],["-swf"],Arg.String (Initialize.set_platform com Flash),"<file>","compile code to Flash SWF file");
		("Target",["--neko"],["-neko"],Arg.String (Initialize.set_platform com Neko),"<file>","compile code to Neko Binary");
		("Target",["--php"],["-php"],Arg.String (fun dir ->
			classes := (["php"],"Boot") :: !classes;
			Initialize.set_platform com Php dir;
		),"<directory>","generate PHP code into target directory");
		("Target",["--cpp"],["-cpp"],Arg.String (fun dir ->
			Initialize.set_platform com Cpp dir;
		),"<directory>","generate C++ code into target directory");
		("Target",["--cppia"],["-cppia"],Arg.String (fun file ->
			Common.define com Define.Cppia;
			Initialize.set_platform com Cpp file;
		),"<file>","generate Cppia code into target file");
		("Target",["--cs"],["-cs"],Arg.String (fun dir ->
			cp_libs := "hxcs" :: !cp_libs;
			Initialize.set_platform com Cs dir;
		),"<directory>","generate C# code into target directory");
		("Target",["--java"],["-java"],Arg.String (fun dir ->
			cp_libs := "hxjava" :: !cp_libs;
			Initialize.set_platform com Java dir;
		),"<directory>","generate Java code into target directory");
		("Target",["--python"],["-python"],Arg.String (fun dir ->
			Initialize.set_platform com Python dir;
		),"<file>","generate Python code as target file");
		("Target",["--hl"],["-hl"],Arg.String (fun file ->
			Initialize.set_platform com Hl file;
		),"<file>","compile HL code as target file");
		("Target",[],["-x"], Arg.String (fun cl ->
			let cpath = Path.parse_type_path cl in
			(match com.main_class with
				| Some c -> if cpath <> c then raise (Arg.Bad "Multiple --main classes specified")
				| None -> com.main_class <- Some cpath);
			classes := cpath :: !classes;
			Common.define com Define.Interp;
			Initialize.set_platform com (!Globals.macro_platform) "";
			interp := true;
		),"<class>","interpret the program using internal macro system");
		("Target",["--interp"],[], Arg.Unit (fun() ->
			Common.define com Define.Interp;
			Initialize.set_platform com (!Globals.macro_platform) "";
			interp := true;
		),"","interpret the program using internal macro system");

		("Compilation",["-p";"--class-path"],["-cp"],Arg.String (fun path ->
			process_libs();
			com.class_path <- Path.add_trailing_slash path :: com.class_path
		),"<path>","add a directory to find source files");
		("Compilation",["-m";"--main"],["-main"],Arg.String (fun cl ->
			if com.main_class <> None then raise (Arg.Bad "Multiple --main classes specified");
			let cpath = Path.parse_type_path cl in
			com.main_class <- Some cpath;
			classes := cpath :: !classes
		),"<class>","select startup class");
		("Compilation",["-L";"--library"],["-lib"],Arg.String (fun l ->
			cp_libs := l :: !cp_libs;
			Common.raw_define com l;
		),"<name[:ver]>","use a haxelib library");
		("Compilation",["-D";"--define"],[],Arg.String (fun var ->
			let flag = try fst (ExtString.String.split var "=") with _ -> var in
			let raise_reserved description =
				raise (Arg.Bad (description ^ " and cannot be defined from the command line"))
			in
			if List.mem flag reserved_flags then raise_reserved (Printf.sprintf "`%s` is a reserved compiler flag" flag);
			List.iter (fun ns ->
				if ExtString.String.starts_with flag (ns ^ ".") then raise_reserved (Printf.sprintf "`%s` uses the reserved compiler flag namespace `%s.*`" flag ns)
			) reserved_flag_namespaces;
			Common.raw_define com var;
		),"<var[=value]>","define a conditional compilation flag");
		("Debug",["-v";"--verbose"],[],Arg.Unit (fun () ->
			com.verbose <- true
		),"","turn on verbose mode");
		("Debug",["--debug"],["-debug"], Arg.Unit (fun() ->
			Common.define com Define.Debug;
			com.debug <- true;
		),"","add debug information to the compiled code");
		("Miscellaneous",["--version"],["-version"],Arg.Unit (fun() ->
			com.info (s_version true) null_pos;
			did_something := true;
		),"","print version and exit");
		("Miscellaneous", ["-h";"--help"], ["-help"], Arg.Unit (fun () ->
			raise (Arg.Help "")
		),"","show extended help information");
		("Miscellaneous",["--help-defines"],[], Arg.Unit (fun() ->
			let all,max_length = Define.get_documentation_list() in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			List.iter (fun msg -> ctx.com.print (msg ^ "\n")) all;
			did_something := true
		),"","print help for all compiler specific defines");
		("Miscellaneous",["--help-metas"],[], Arg.Unit (fun() ->
			let all,max_length = Meta.get_documentation_list() in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			List.iter (fun msg -> ctx.com.print (msg ^ "\n")) all;
			did_something := true
		),"","print help for all compiler metadatas");
		("Misc",["--run"],[], Arg.Unit (fun() -> assert false), "<module> [args...]","compile and execute a Haxe module with command line arguments");
	] in
	let adv_args_spec = [
		("Optimization",["--dce"],["-dce"],Arg.String (fun mode ->
			(match mode with
			| "std" | "full" | "no" -> ()
			| _ -> raise (Arg.Bad "Invalid DCE mode, expected std | full | no"));
			Common.define_value com Define.Dce mode
		),"[std|full|no]","set the dead code elimination mode (default std)");
		("Target-specific",["--swf-version"],["-swf-version"],Arg.Float (fun v ->
			if not !swf_version || com.flash_version < v then com.flash_version <- v;
			swf_version := true;
		),"<version>","change the SWF version");
		(* FIXME: replace with -D define *)
		("Target-specific",["--swf-header"],["-swf-header"],Arg.String (fun h ->
			try
				swf_header := Some (match ExtString.String.nsplit h ":" with
				| [width; height; fps] ->
					(int_of_string width,int_of_string height,float_of_string fps,0xFFFFFF)
				| [width; height; fps; color] ->
					let color = if ExtString.String.starts_with color "0x" then color else "0x" ^ color in
					(int_of_string width, int_of_string height, float_of_string fps, int_of_string color)
				| _ -> raise Exit)
			with
				_ -> raise (Arg.Bad "Invalid SWF header format, expected width:height:fps[:color]")
		),"<header>","define SWF header (width:height:fps:color)");
		("Target-specific",["--flash-strict"],[], define Define.FlashStrict, "","more type strict flash API");
		("Target-specific",["--swf-lib"],["-swf-lib"],Arg.String (fun file ->
			process_libs(); (* linked swf order matters, and lib might reference swf as well *)
			add_native_lib file false;
		),"<file>","add the SWF library to the compiled SWF");
		(* FIXME: replace with -D define *)
		("Target-specific",["--swf-lib-extern"],["-swf-lib-extern"],Arg.String (fun file ->
			add_native_lib file true;
		),"<file>","use the SWF library for type checking");
		("Target-specific",["--java-lib"],["-java-lib"],Arg.String (fun file ->
			add_native_lib file false;
		),"<file>","add an external JAR or class directory library");
		("Target-specific",["--net-lib"],["-net-lib"],Arg.String (fun file ->
			add_native_lib file false;
		),"<file>[@std]","add an external .NET DLL file");
		("Target-specific",["--net-std"],["-net-std"],Arg.String (fun file ->
			Dotnet.add_net_std com file
		),"<file>","add a root std .NET DLL search path");
		(* FIXME: replace with -D define *)
		("Target-specific",["--c-arg"],["-c-arg"],Arg.String (fun arg ->
			com.c_args <- arg :: com.c_args
		),"<arg>","pass option <arg> to the native Java/C# compiler");
		("Compilation",["-r";"--resource"],["-resource"],Arg.String (fun res ->
			let file, name = (match ExtString.String.nsplit res "@" with
				| [file; name] -> file, name
				| [file] -> file, file
				| _ -> raise (Arg.Bad "Invalid Resource format, expected file@name")
			) in
			let file = (try Common.find_file com file with Not_found -> file) in
			let data = (try
				let s = Std.input_file ~bin:true file in
				if String.length s > 12000000 then raise Exit;
				s;
			with
				| Sys_error _ -> failwith ("Resource file not found: " ^ file)
				| _ -> failwith ("Resource '" ^ file ^ "' excess the maximum size of 12MB")
			) in
			if Hashtbl.mem com.resources name then failwith ("Duplicate resource name " ^ name);
			Hashtbl.add com.resources name data
		),"<file>[@name]","add a named resource file");
		("Debug",["--prompt"],["-prompt"], Arg.Unit (fun() -> prompt := true),"","prompt on error");
		("Compilation",["--cmd"],["-cmd"], Arg.String (fun cmd ->
			cmds := DisplayOutput.unquote cmd :: !cmds
		),"<command>","run the specified command after successful compilation");
		(* FIXME: replace with -D define *)
		("Optimization",["--no-traces"],[], define Define.NoTraces, "","don't compile trace calls in the program");
		("Batch",["--next"],[], Arg.Unit (fun() -> assert false), "","separate several haxe compilations");
		("Batch",["--each"],[], Arg.Unit (fun() -> assert false), "","append preceding parameters to all Haxe compilations separated by --next");
		("Services",["--display"],[], Arg.String (fun input ->
			let input = String.trim input in
			if String.length input > 0 && (input.[0] = '[' || input.[0] = '{') then begin
				did_something := true;
				force_typing := true;
				DisplayJson.parse_input com input Timer.measure_times
			end else
				DisplayOutput.handle_display_argument com input pre_compilation did_something;
		),"","display code tips");
		("Services",["--xml"],["-xml"],Arg.String (fun file ->
			xml_out := Some file
		),"<file>","generate XML types description");
		("Services",["--json"],[],Arg.String (fun file ->
			json_out := Some file
		),"<file>","generate JSON types description");
		("Optimization",["--no-output"],[], Arg.Unit (fun() -> no_output := true),"","compiles but does not generate any file");
		("Debug",["--times"],[], Arg.Unit (fun() -> Timer.measure_times := true),"","measure compilation times");
		("Optimization",["--no-inline"],[], define Define.NoInline, "","disable inlining");
		("Optimization",["--no-opt"],[], Arg.Unit (fun() ->
			com.foptimize <- false;
			Common.define com Define.NoOpt;
		), "","disable code optimizations");
		("Compilation",["--remap"],[], Arg.String (fun s ->
			let pack, target = (try ExtString.String.split s ":" with _ -> raise (Arg.Bad "Invalid remap format, expected source:target")) in
			com.package_rules <- PMap.add pack (Remap target) com.package_rules;
		),"<package:target>","remap a package to another one");
		("Compilation",["--macro"],[], Arg.String (fun e ->
			force_typing := true;
			config_macros := e :: !config_macros
		),"<macro>","call the given macro before typing anything else");
		("Compilation Server",["--server-listen"],["--wait"], Arg.String (fun hp ->
			let accept = match hp with
				| "stdio" ->
					Server.init_wait_stdio()
				| _ ->
					let host, port = parse_host_port hp in
					init_wait_socket host port
			in
			wait_loop process_params com.verbose accept
		),"[[host:]port]|stdio]","wait on the given port (or use standard i/o) for commands to run");
		("Compilation Server",["--server-connect"],[], Arg.String (fun hp ->
			let host, port = parse_host_port hp in
			let accept = Server.init_wait_connect host port in
			wait_loop process_params com.verbose accept
		),"[host:]port]","connect to the given port and wait for commands to run");
		("Compilation Server",["--connect"],[],Arg.String (fun _ ->
			assert false
		),"<[host:]port>","connect on the given port and run commands there");
		("Compilation",["-C";"--cwd"],[], Arg.String (fun dir ->
			(* This is handled by process_params, but passed through so we know we did something. *)
			did_something := true;
		),"<dir>","set current working directory");
		("Compilation",["--haxelib-global"],[], Arg.Unit (fun () -> ()),"","pass --global argument to haxelib");
	] in
	let args_callback cl =
		begin try
			let path,name = Path.parse_path cl in
			if StringHelper.starts_uppercase_identifier name then
				classes := (path,name) :: !classes
			else begin
				force_typing := true;
				config_macros := (Printf.sprintf "include('%s', true, null, null, true)" cl) :: !config_macros;
			end
		with Failure _ when ctx.com.display.dms_display ->
			()
		end
	in
	let all_args = (basic_args_spec @ adv_args_spec) in
	let all_args_spec = process_args all_args in
	let process args =
		let current = ref 0 in
		(try
			let rec loop acc args = match args with
				| "--display" :: arg :: args ->
					loop (arg :: "--display" :: acc) args
				| arg :: args ->
					loop (expand_env arg :: acc) args
				| [] ->
					List.rev acc
			in
			let args = loop [] args in
			Arg.parse_argv ~current (Array.of_list ("" :: args)) all_args_spec args_callback "";
		with
		| Arg.Help _ ->
			raise (HelpMessage (usage_string all_args usage))
		| Arg.Bad msg ->
			let first_line = List.nth (Str.split (Str.regexp "\n") msg) 0 in
			let new_msg = (Printf.sprintf "%s" first_line) in
			let r = Str.regexp "unknown option [`']?\\([-A-Za-z]+\\)[`']?" in
			try
				ignore(Str.search_forward r msg 0);
				let s = Str.matched_group 1 msg in
				let sl = List.map (fun (s,_,_) -> s) all_args_spec in
				let sl = StringError.get_similar s sl in
				begin match sl with
				| [] -> raise Not_found
				| _ ->
					let spec = List.filter (fun (_,sl',sl'',_,_,_) ->
						List.exists (fun s -> List.mem s sl) (sl' @ sl'')
					) all_args in
					let new_msg = (Printf.sprintf "%s\nDid you mean:\n%s" first_line (usage_string ~print_cat:false spec "")) in
					raise (Arg.Bad new_msg)
				end;
			with Not_found ->
				raise (Arg.Bad new_msg));
		if com.platform = Globals.Cpp && not (Define.defined com.defines DisableUnicodeStrings) && not (Define.defined com.defines HxcppSmartStings) then begin
			Define.define com.defines HxcppSmartStings;
		end;
		if Define.raw_defined com.defines "gen_hx_classes" then begin
			(* TODO: this is something we're gonna remove once we have something nicer for generating flash externs *)
			force_typing := true;
			pre_compilation := (fun() ->
				let process_lib lib =
					if not (lib#has_flag NativeLibraries.FlagIsStd) then
						List.iter (fun path -> if path <> (["java";"lang"],"String") then classes := path :: !classes) lib#list_modules
				in
				List.iter process_lib com.native_libs.net_libs;
				List.iter process_lib com.native_libs.swf_libs;
				List.iter process_lib com.native_libs.java_libs;
			) :: !pre_compilation;
			xml_out := Some "hx"
		end;
	in
	process_ref := process;
	(* Handle CLI arguments *)
	process ctx.com.args;
	(* Process haxelibs *)
	process_libs();
	(* Set up display configuration *)
	process_display_configuration ctx;
	let display_file_dot_path = DisplayOutput.process_display_file com classes in
	(* Initialize target: This allows access to the appropriate std packages and sets the -D defines. *)
	let ext = Initialize.initialize_target ctx com classes in
	(* if we are at the last compilation step, allow all packages accesses - in case of macros or opening another project file *)
	if com.display.dms_display then begin match com.display.dms_kind with
		| DMDefault | DMUsage _ -> ()
		| _ -> if not ctx.has_next then com.package_rules <- PMap.foldi (fun p r acc -> match r with Forbidden -> acc | _ -> PMap.add p r acc) com.package_rules PMap.empty;
	end;
	com.config <- get_config com; (* make sure to adapt all flags changes defined after platform *)
	let t = Timer.timer ["init"] in
	List.iter (fun f -> f()) (List.rev (!pre_compilation));
	t();
	com.stage <- CInitialized;
	if !classes = [([],"Std")] && not !force_typing then begin
		if !cmds = [] && not !did_something then raise (HelpMessage (usage_string basic_args_spec usage));
	end else begin
		(* Actual compilation starts here *)
		let tctx = create_typer_context ctx !native_libs in
		com.stage <- CTyperCreated;
		let display_file_dot_path = match display_file_dot_path with
			| DPKMacro path ->
				ignore(load_display_module_in_macro tctx (Some path) true);
				Some path
			| DPKNormal path ->
				Some path
			| DPKNone ->
				None
			| DPKDirect file ->
				DisplayOutput.load_display_file_standalone tctx file;
				None
			| DPKInput input ->
				DisplayOutput.load_display_content_standalone tctx input;
				None
		in
		begin try
			do_type tctx !config_macros !classes;
		with TypeloadParse.DisplayInMacroBlock ->
			ignore(load_display_module_in_macro tctx display_file_dot_path true);
		end;
		handle_display ctx tctx display_file_dot_path;
		filter ctx tctx display_file_dot_path;
		if ctx.has_error then raise Abort;
		check_auxiliary_output com !xml_out !json_out;
		com.stage <- CGenerationStart;
		if not !no_output then generate tctx ext !interp !swf_header;
		com.stage <- CGenerationDone;
	end;
	Sys.catch_break false;
	List.iter (fun f -> f()) (List.rev com.callbacks#get_after_generation);
	if not !no_output then begin
		List.iter (fun c ->
			let r = run_command ctx c in
			if r <> 0 then failwith ("Command failed with error " ^ string_of_int r)
		) (List.rev !cmds)
	end
with
	| Abort ->
		()
	| Error.Fatal_error (m,p) ->
		error ctx m p
	| Common.Abort (m,p) ->
		error ctx m p
	| Lexer.Error (m,p) ->
		error ctx (Lexer.error_msg m) p
	| Parser.Error (m,p) ->
		error ctx (Parser.error_msg m) p
	| Typecore.Forbid_package ((pack,m,p),pl,pf)  ->
		if !Parser.display_mode <> DMNone && ctx.has_next then begin
			ctx.has_error <- false;
			ctx.messages <- [];
		end else begin
			error ctx (Printf.sprintf "You cannot access the %s package while %s (for %s)" pack (if pf = "macro" then "in a macro" else "targeting " ^ pf) (s_type_path m) ) p;
			List.iter (error ctx "    referenced here") (List.rev pl);
		end
	| Error.Error (m,p) ->
		error ctx (Error.error_msg m) p
	| Generic.Generic_Exception(m,p) ->
		error ctx m p
	| Arg.Bad msg ->
		error ctx ("Error: " ^ msg) null_pos
	| Failure msg when not is_debug_run ->
		error ctx ("Error: " ^ msg) null_pos
	| HelpMessage msg ->
		com.info msg null_pos
	| DisplayException(DisplayHover _ | DisplayPositions _ | DisplayFields _ | DisplayPackage _  | DisplaySignatures _ as de) when ctx.com.json_out <> None ->
		begin
			DisplayPosition.display_position#reset;
			match ctx.com.json_out with
			| Some api ->
				let ctx = DisplayJson.create_json_context api.jsonrpc (match de with DisplayFields _ -> true | _ -> false) in
				api.send_result (DisplayException.to_json ctx de)
			| _ -> assert false
		end
	(* | Parser.TypePath (_,_,_,p) when ctx.com.json_out <> None ->
		begin match com.json_out with
		| Some (f,_) ->
			let tctx = Typer.create ctx.com in
			let fields = DisplayToplevel.collect tctx true Typecore.NoValue in
			let jctx = Genjson.create_context Genjson.GMMinimum in
			f (DisplayException.fields_to_json jctx fields CRImport (Some (Parser.cut_pos_at_display p)) false)
		| _ -> assert false
		end *)
	| DisplayException(DisplayPackage pack) ->
		DisplayPosition.display_position#reset;
		raise (DisplayOutput.Completion (String.concat "." pack))
	| DisplayException(DisplayFields Some r) ->
		DisplayPosition.display_position#reset;
		let fields = if !Timer.measure_times then begin
			Timer.close_times();
			(List.map (fun (name,value) ->
				CompletionItem.make_ci_timer ("@TIME " ^ name) value
			) (DisplayOutput.get_timer_fields !start_time)) @ r.fitems
		end else
			r.fitems
		in
		let s = match r.fkind with
			| CRToplevel _
			| CRTypeHint
			| CRExtends
			| CRImplements
			| CRStructExtension _
			| CRImport
			| CRUsing
			| CRNew
			| CRPattern _
			| CRTypeRelation
			| CRTypeDecl ->
				DisplayOutput.print_toplevel fields
			| CRField _
			| CRStructureField
			| CRMetadata
			| CROverride ->
				DisplayOutput.print_fields fields
		in
		raise (DisplayOutput.Completion s)
	| DisplayException(DisplayHover Some ({hitem = {CompletionItem.ci_type = Some (t,_)}} as hover)) ->
		DisplayPosition.display_position#reset;
		let doc = CompletionItem.get_documentation hover.hitem in
		raise (DisplayOutput.Completion (DisplayOutput.print_type t hover.hpos doc))
	| DisplayException(DisplaySignatures Some (signatures,_,display_arg,_)) ->
		DisplayPosition.display_position#reset;
		if ctx.com.display.dms_kind = DMSignature then
			raise (DisplayOutput.Completion (DisplayOutput.print_signature signatures display_arg))
		else
			raise (DisplayOutput.Completion (DisplayOutput.print_signatures signatures))
	| DisplayException(DisplayPositions pl) ->
		DisplayPosition.display_position#reset;
		raise (DisplayOutput.Completion (DisplayOutput.print_positions pl))
	| Parser.TypePath (p,c,is_import,pos) ->
		let fields =
			try begin match c with
				| None ->
					DisplayPath.TypePathHandler.complete_type_path com p
				| Some (c,cur_package) ->
					let ctx = Typer.create com in
					DisplayPath.TypePathHandler.complete_type_path_inner ctx p c cur_package is_import
			end with Common.Abort(msg,p) ->
				error ctx msg p;
				None
		in
		begin match ctx.com.json_out,fields with
		| None,None ->
			()
		| None,Some fields ->
			raise (DisplayOutput.Completion (DisplayOutput.print_fields fields))
		| Some api,None when is_legacy_completion com ->
			api.send_result JNull
		| Some api,fields ->
			let fields = Option.default [] fields in
			let ctx = DisplayJson.create_json_context api.jsonrpc false in
			let path = match List.rev p with
				| name :: pack -> List.rev pack,name
				| [] -> [],""
			in
			let kind = CRField ((CompletionItem.make_ci_module path,pos,None,None)) in
			api.send_result (DisplayException.fields_to_json ctx fields kind (DisplayTypes.make_subject None pos));
		end
	| Parser.SyntaxCompletion(kind,subj) ->
		DisplayOutput.handle_syntax_completion com kind subj;
		error ctx ("Error: No completion point was found") null_pos
	| DisplayException(DisplayDiagnostics dctx) ->
		let s = Json.string_of_json (DiagnosticsPrinter.json_of_diagnostics dctx) in
		DisplayPosition.display_position#reset;
		raise (DisplayOutput.Completion s)
	| DisplayException(ModuleSymbols s | Statistics s | Metadata s) ->
		DisplayPosition.display_position#reset;
		raise (DisplayOutput.Completion s)
	| EvalExceptions.Sys_exit i | Hlinterp.Sys_exit i ->
		ctx.flush();
		if !Timer.measure_times then Timer.report_times prerr_endline;
		exit i
	| DisplayOutput.Completion _ as exc ->
		raise exc
	| Out_of_memory as exc ->
		raise exc
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" || CompilationServer.runs() with _ -> true) && not is_debug_run ->
		error ctx (Printexc.to_string e) null_pos

;;
let other = Timer.timer ["other"] in
Sys.catch_break true;
MacroContext.setup();

let args = List.tl (Array.to_list Sys.argv) in
(try
	let server = Sys.getenv "HAXE_COMPILATION_SERVER" in
	let host, port = (try ExtString.String.split server ":" with _ -> "127.0.0.1", server) in
	do_connect host (try int_of_string port with _ -> failwith "Invalid HAXE_COMPILATION_SERVER port") args
with Not_found -> try
	process_params create_context args
with DisplayOutput.Completion c ->
	prerr_endline c;
	exit 0
| Arg.Bad msg ->
	prerr_endline ("Error: " ^ msg);
	exit 1
);
other();
if !Timer.measure_times then Timer.report_times prerr_endline
