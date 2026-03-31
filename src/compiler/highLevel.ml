open Common
open ParsedArg

let add_libs timer_ctx (libs,cwd) args cs has_display =
	let global_repo = List.exists (fun a -> a = "--haxelib-global") args in
	let fail msg =
		raise (Arg.Bad msg)
	in
	let call_haxelib() =
		let cmd = "haxelib" ^ (if global_repo then " --global" else " --cwd " ^ cwd) ^ " path " ^ String.concat " " libs in
		let pin, pout, perr = Unix.open_process_full cmd (Unix.environment()) in
		let lines = Std.input_list pin in
		let err = Std.input_list perr in
		let ret = Unix.close_process_full (pin,pout,perr) in
		if ret <> Unix.WEXITED 0 then fail (match lines, err with
			| [], [] -> "Failed to call haxelib (command not found ?)"
			| [], [s] when ExtString.String.ends_with (ExtString.String.strip s) "Module not found: path" -> "The haxelib command has been strip'ed, please install it again"
			| _ -> String.concat "\n" (lines@err));
		lines
	in
	let call_haxelib () =
		Timer.time timer_ctx ["haxelib"] call_haxelib ()
	in
	match libs with
	| [] ->
		[]
	| _ ->
		let lines =
			try
				(* if we are compiling, really call haxelib since library path might have changed *)
				if not has_display then raise Not_found;
				cs#find_haxelib (libs, if global_repo then None else Some cwd)
			with Not_found -> try
				let lines = call_haxelib() in
				cs#cache_haxelib (libs, if global_repo then None else Some cwd) lines;
				lines
			with Unix.Unix_error(code,msg,arg) ->
				fail ((Printf.sprintf "%s (%s)" (Unix.error_message code) arg))
		in
		let lines = List.fold_left (fun acc l ->
			let l = ExtString.String.strip l in
			if l = "" then
				acc
			else if l.[0] <> '-' then
				"-libcp" :: l :: acc
			else match (try ExtString.String.split l " " with _ -> l, "") with
			| ("-L",dir) ->
				"--neko-lib-path" :: (String.sub l 3 (String.length l - 3)) :: acc
			| param, value ->
				let acc = if value <> "" then value :: acc else acc in
				let acc = param :: acc in
				acc
		) [] (List.rev lines) in
		lines

let create_context_from_part (sctx : ServerCompilationContext.t) (request_scope : request_scope) has_display has_next (part : Args.part_args) =
	sctx.compilation_step <- sctx.compilation_step + 1;
	let com = Compiler.create_context sctx request_scope part.runtime_args has_next in
	(* Expand Expand markers by calling haxelib, caching the result in the marker state *)
	let expand_part_libs has_global (part_args : parsed_arg list) =
		let expand_one arg = match arg with
			| Expand ex ->
				(match ex.state with
				| AlreadyExpanded expanded ->
					expanded
				| NotYetExpanded (AddLibs libs) ->
					let raw_lines = add_libs request_scope.timer_ctx libs (if has_global then ["--haxelib-global"] else []) sctx.cs has_display in
					let expanded = Args.parse_args raw_lines in
					ex.state <- AlreadyExpanded expanded;
					expanded)
			| arg -> [arg]
		in
		List.concat_map expand_one part_args
	in
	let has_global = List.exists (fun a -> a = HaxelibGlobal) part.Args.args in
	let expanded_args = expand_part_libs has_global part.Args.args in
	com.parsed_args <- expanded_args;
	com

let entry sctx request_scope (request_args : Args.request_args) =
	let curdir = Unix.getcwd () in
	let has_display = request_args.display_arg <> None in
	let rec loop = function
		| [] ->
			0
		| part :: rest ->
			let com = create_context_from_part sctx request_scope has_display (rest <> []) part in
			let code = Compiler.compile_ctx sctx com in
			Unix.chdir curdir;
			if code = 0 && rest <> [] && not has_display then
				loop rest
			else
				code
	in
	let code = loop request_args.parts in
	Unix.chdir curdir;
	code