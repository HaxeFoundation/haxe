open Globals
open CompilationContext
open TType
open Tanon_identification

let check_auxiliary_output com actx =
	begin match actx.xml_out with
		| None -> ()
		| Some "hx" ->
			Genhxold.generate com
		| Some file ->
			Common.log com ("Generating xml: " ^ file);
			Path.mkdir_from_path file;
			Genxml.generate com file
	end;
	begin match actx.json_out with
		| None -> ()
		| Some file ->
			Common.log com ("Generating json : " ^ file);
			Path.mkdir_from_path file;
			Genjson.generate com.types file
	end

let create_writer com config string_pool =
	let anon_identification = new tanon_identification in
	let warn w s p = com.Common.warning w com.warning_options s p in
	let writer = HxbWriter.create config string_pool warn anon_identification in
	writer,(fun () ->
		let out = IO.output_string () in
		HxbWriter.export writer out;
		IO.close_out out
	)

let export_hxb from_cache com config string_pool cc platform zip m =
	let open HxbData in
	match m.m_extra.m_kind with
		| MCode | MMacro | MFake | MExtern -> begin
			(* Printf.eprintf "Export module %s\n" (s_type_path m.m_path); *)
			let l = platform :: (fst m.m_path @ [snd m.m_path]) in
			let path = (String.concat "/" l) ^ ".hxb" in

			if from_cache then begin
				let hxb_cache = try cc#get_hxb_module m.m_path with Not_found -> raise Abort in
				let out = IO.output_string () in
				write_header out;
				List.iter (fun (kind,data) ->
					write_chunk_prefix kind (Bytes.length data) out;
					IO.nwrite out data
				) hxb_cache.mc_chunks;
				let data = IO.close_out out in
				zip#add_entry data path;
			end else begin
				let writer,close = create_writer com config string_pool in
				HxbWriter.write_module writer m;
				let bytes = close () in
				zip#add_entry bytes path;
			end
		end
	| _ ->
		()

let check_hxb_output ctx config =
	let open HxbWriterConfig in
	let com = ctx.com in
	let write_string_pool config zip name pool =
		let writer,close = create_writer com config (Some pool) in
		let a = StringPool.finalize writer.cp in
		HxbWriter.HxbWriter.write_string_pool writer STR a;
		let bytes = close () in
		zip#add_entry bytes name;
	in
	let match_path_list l sl_path =
		List.exists (fun sl -> Ast.match_path true sl_path sl) l
	in
	let try_write from_cache =
		let path = config.HxbWriterConfig.archive_path in
		let path = Str.global_replace (Str.regexp "\\$target") (platform_name ctx.com.platform) path in
		let t = Timer.timer ["generate";"hxb"] in
		Path.mkdir_from_path path;
		let zip = new Zip_output.zip_output path 6 in
		let export com config string_pool =
			let cc = CommonCache.get_cache com in
			let target = Common.platform_name_macro com in

			List.iter (fun m ->
				let t = Timer.timer ["generate";"hxb";s_type_path m.m_path] in
				let sl_path = fst m.m_path @ [snd m.m_path] in
				if not (match_path_list config.exclude sl_path) || match_path_list config.include' sl_path then
					Std.finally t (export_hxb from_cache com config string_pool cc target zip) m
			) com.modules;
		in
		Std.finally (fun () ->
			zip#close;
			t()
		) (fun () ->
			let string_pool = if config.share_string_pool then Some (StringPool.create ()) else None in
			if config.target_config.generate then begin
				export com config.target_config string_pool;
			end;

			if config.macro_config.generate then begin
				match com.get_macros() with
					| Some mcom ->
						let use_separate_pool = config.share_string_pool && from_cache in
						let string_pool = if use_separate_pool then Some (StringPool.create ()) else string_pool in
						export mcom config.macro_config string_pool;
						if use_separate_pool then write_string_pool config.macro_config zip "StringPool.macro.hxb" (Option.get string_pool)
					| _ ->
						()
			end;

			if config.share_string_pool then
				write_string_pool config.target_config zip "StringPool.hxb" (Option.get string_pool);
		) ()
	in
	try
		(* This Abort case shouldn't happen, unless some modules are not stored in hxb cache (which should not be the case currently) *)
		if ctx.comm.is_server then try try_write true with Abort -> try_write false
		else try_write false
	with Sys_error s ->
		CompilationContext.error ctx (Printf.sprintf "Could not write to %s: %s" config.archive_path s) null_pos

let parse_swf_header ctx h = match ExtString.String.nsplit h ":" with
		| [width; height; fps] ->
			Some (int_of_string width,int_of_string height,float_of_string fps,0xFFFFFF)
		| [width; height; fps; color] ->
			let color = if ExtString.String.starts_with color "0x" then color else "0x" ^ color in
			Some (int_of_string width, int_of_string height, float_of_string fps, int_of_string color)
		| _ ->
			error ctx "Invalid SWF header format, expected width:height:fps[:color]" null_pos;
			None

let delete_file f = try Sys.remove f with _ -> ()

let maybe_generate_dump ctx tctx =
	let com = tctx.Typecore.com in
	if Common.defined com Define.Dump then begin
		Codegen.Dump.dump_types com;
		Option.may Codegen.Dump.dump_types (com.get_macros())
	end;
	if Common.defined com Define.DumpDependencies then begin
		Codegen.Dump.dump_dependencies com;
		if not com.is_macro_context then match tctx.Typecore.g.Typecore.macros with
			| None -> ()
			| Some(_,ctx) -> Codegen.Dump.dump_dependencies ~target_override:(Some "macro") ctx.Typecore.com
	end

let generate ctx tctx ext actx =
	let com = tctx.Typecore.com in
	(* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. *)
	if Path.file_extension com.file = ext then delete_file com.file;
	if com.platform = Flash || com.platform = Cpp || com.platform = Hl then List.iter (Codegen.fix_overrides com) com.types;
	begin match com.platform with
		| Neko | Hl | Eval when actx.interp -> ()
		| Cpp when Common.defined com Define.Cppia -> ()
		| Cpp | Php -> Path.mkdir_from_path (com.file ^ "/.")
		| _ -> Path.mkdir_from_path com.file
	end;
	if actx.interp then begin
		let timer = Timer.timer ["interp"] in
		let old = tctx.com.args in
		tctx.com.args <- ctx.runtime_args;
		let restore () =
			tctx.com.args <- old;
			timer ()
		in
		Std.finally restore MacroContext.interpret tctx
	end else begin
		let generate,name = match com.platform with
		| Flash ->
			let header = try
				parse_swf_header ctx (Common.defined_value com Define.SwfHeader)
			with Not_found ->
				None
			in
			Genswf.generate header,"swf"
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
		| Jvm ->
			Genjvm.generate actx.jvm_flag,"jvm"
		| Python ->
			Genpy.generate,"python"
		| Hl ->
			Genhl.generate,"hl"
		| Eval ->
			(fun _ -> MacroContext.interpret tctx),"eval"
		| Cross
		| CustomTarget _ ->
			(fun _ -> ()),""
		in
		if name = "" then ()
		else begin
			Common.log com ("Generating " ^ name ^ ": " ^ com.file);
			let t = Timer.timer ["generate";name] in
			generate com;
			t()
		end
	end
