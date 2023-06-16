open Globals
open CompilationContext
open TType
open Tanon_identification

let test_hxb com m =
	if m.m_extra.m_kind = MCode then begin
		let ch = IO.output_bytes() in
		let anon_identification = new tanon_identification ([],"") in
		let writer = new HxbWriter.hxb_writer anon_identification (* cp *) in
		writer#write_module m;
		let bytes_module = IO.close_out ch in
		let ch = IO.output_bytes() in
		writer#export ch;
		let bytes_cp = IO.close_out ch in
		let path = m.m_path in
		(* TODO use actual hxb output path *)
		let l = ((Common.dump_path com) :: "hxb" :: (Common.platform_name_macro com) :: fst path @ [snd path]) in
		let ch_file = Path.create_file true ".hxb" [] l in
		output_bytes ch_file bytes_cp;
		output_bytes ch_file bytes_module;
		close_out ch_file
	end

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
	end;
	begin match actx.hxb_out with
		| None -> ()
		| Some file ->
				Common.log com ("Generating hxb : " ^ file);
				Path.mkdir_from_path file;
				let t = Timer.timer ["generate";"hxb"] in
				(* HxbWriter.write com file; *)
				List.iter (test_hxb com) com.modules;
				t();
	end

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

let generate ctx tctx ext actx =
	let com = tctx.Typecore.com in
	(* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. *)
	if Path.file_extension com.file = ext then delete_file com.file;
	if com.platform = Flash || com.platform = Cpp || com.platform = Hl then List.iter (Codegen.fix_overrides com) com.types;
	if Common.defined com Define.Dump then begin
		Codegen.Dump.dump_types com;
		Option.may Codegen.Dump.dump_types (com.get_macros())
	end;
	if Common.defined com Define.DumpDependencies then begin
		Codegen.Dump.dump_dependencies com;
		if not com.is_macro_context then match tctx.Typecore.g.Typecore.macros with
			| None -> ()
			| Some(_,ctx) -> Codegen.Dump.dump_dependencies ~target_override:(Some "macro") ctx.Typecore.com
	end;
	begin match com.platform with
		| Neko | Hl | Eval when actx.interp -> ()
		| Cpp when Common.defined com Define.Cppia -> ()
		| Cpp | Cs | Php -> Path.mkdir_from_path (com.file ^ "/.")
		| Java when not actx.jvm_flag -> Path.mkdir_from_path (com.file ^ "/.")
		| _ -> Path.mkdir_from_path com.file
	end;
	if actx.interp then
		Std.finally (Timer.timer ["interp"]) MacroContext.interpret tctx
	else begin
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
		| Cs ->
			Gencs.generate,"cs"
		| Java ->
			if Common.defined com Jvm then
				Genjvm.generate actx.jvm_flag,"java"
			else
				Genjava.generate,"java"
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
