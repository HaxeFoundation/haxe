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

let export_hxb com root m =
	match m.m_extra.m_kind with
		| MCode | MMacro | MFake -> begin
			(* Printf.eprintf "Export module %s\n" (s_type_path m.m_path); *)
			let anon_identification = new tanon_identification in
			let writer = new HxbWriter.hxb_writer (MessageReporting.display_source_at com) anon_identification in
			writer#write_module m;
			let l = (root :: fst m.m_path @ [snd m.m_path]) in
			let ch = Path.create_file true ".hxb" [] l in
			writer#export (IO.output_channel ch);
			close_out ch
		end
	| _ -> ()

let check_hxb_output com actx =
	begin match actx.hxb_out with
		| None -> ()
		| Some path ->
			(* TODO move somewhere else *)
			let clean_files path =
				let rec iter_files pack dir path = try
					let file = Unix.readdir dir in

					if file <> "." && file <> ".." then begin
						let filepath = path ^ "/" ^ file in
						if (Unix.stat filepath).st_kind = S_DIR then
							let pack = pack @ [file] in
							iter_files (pack) (Unix.opendir filepath) filepath;
							try Unix.rmdir filepath with Unix.Unix_error (ENOTEMPTY,_,_) -> ();
						else
							Sys.remove filepath
					end;

					iter_files pack dir path
				with | End_of_file | Unix.Unix_error _ ->
					Unix.closedir dir
				in
				iter_files [] (Unix.opendir path) path
			in

			let export com =
				let path = Path.add_trailing_slash (path ^ Path.path_sep ^ (Common.platform_name_macro com)) in
				Common.log com ("Generating hxb to " ^ path);
				Printf.eprintf "Generating hxb to %s\n" path;
				Path.mkdir_from_path path;
				clean_files path;
				let t = Timer.timer ["generate";"hxb"] in
				Printf.eprintf "%d modules, %d types\n" (List.length com.modules) (List.length com.types);
				List.iter (export_hxb com path) com.modules;
				t();
			in

			export com;
			Option.may export (com.get_macros());
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
