open TTFData

exception Abort

let gen_hxswfml_debug fontname =
	let xml = "<?xml version=\"1.0\" ?>
	<swf>
		<FileAttributes/>
		<Custom tagId=\"75\" file=\"" ^ fontname ^ ".dat\" comment=\"DefineFont3\"/>
		<SymbolClass id=\"1\" class=\"TestFont\" base=\"flash.text.Font\"/>
		<DefineABC file=\"Main.swf\" isBoot=\"true\"/>
		<ShowFrame/>
	</swf>"
	in
	Std.output_file (fontname ^ ".fxml") xml;
	if Sys.command "haxe -main Main -swf main.swf" <> 0 then failwith "Error while executing haxe";
	if Sys.command ("hxswfml xml2swf \"" ^ fontname ^ ".fxml\" \"" ^ fontname ^ ".swf\" -no-strict") <> 0 then failwith "Error while executing hxswfml";
	Unix.unlink (fontname ^ ".fxml");
	Unix.unlink "main.swf"

let normalize_path p =
	let l = String.length p in
	if l = 0 then
		"./"
	else begin
		let p = String.concat "/" (ExtString.String.nsplit p "\\") in
		match p.[l-1] with
		| '/' -> p
		| _ -> p ^ "/"
	end

let mk_dir_rec dir =
	let dir = normalize_path dir in
	let parts = ExtString.String.nsplit dir "/" in
	let rec create acc = function
		| [] -> ()
		| "" :: [] -> ()
		| d :: l ->
			let dir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
			create (d :: acc) l
	in
	create [] parts

let exit msg =
	prerr_endline msg;
	raise Abort

let process args =
	let fonts = ref [] in
	let range_str = ref "" in
	let targets = ref [] in
	let debug_hxswfml = ref false in
	let args_callback s = fonts := s :: !fonts in
	let usage = Printf.sprintf
		"Ttf <font paths> (-swf|-canvas)"
	in
	let basic_args = [
		("-range",Arg.String (fun str ->
			range_str := str;
		),"<str> : specifies the character range");
		("-swf",Arg.String (fun dir ->
			mk_dir_rec dir;
 			let f ttf range_str =
 				let config = {
 					ttfc_range_str = range_str;
 					ttfc_font_name = None;
					ttfc_font_weight = TFWRegular;
					ttfc_font_posture = TFPNormal;
 				} in
				let f2 = TTFSwfWriter.to_swf ttf config in
				let ch = IO.output_channel (open_out_bin (dir ^ "/" ^ ttf.ttf_font_name ^ ".dat")) in
				let b = IO.output_bits ch in
				IO.write_i16 ch 1;
				TTFSwfWriter.write_font2 ch b f2;
				IO.close_out ch;
				if !debug_hxswfml then begin
					if not (Sys.file_exists "Main.hx") then failwith "Could not find Main.hx required for -hxswfml-debug";
					let main = Std.input_file "Main.hx" in
					let old = Sys.getcwd () in
					Sys.chdir dir;
					Std.output_file ~filename:"Main.hx" ~text:main;
					gen_hxswfml_debug ttf.ttf_font_name;
					Unix.unlink "Main.hx";
					Sys.chdir old;
				end
			in
			targets := f :: !targets;
		),"<dir> : generate swf tag data to <dir>");
		("-canvas", Arg.String (fun dir ->
			mk_dir_rec dir;
 			let f ttf range_str =
 				let glyphs = TTFCanvasWriter.to_canvas ttf range_str in
				let ch = IO.output_channel (open_out_bin (dir ^ "/" ^ ttf.ttf_font_name ^ ".js")) in
				TTFCanvasWriter.write_font ch ttf glyphs;
				IO.close_out ch;
			in
			targets := f :: !targets;
		),"<dir> : generate canvas draw commands to <dir>");
		("-json", Arg.String (fun dir ->
			mk_dir_rec dir;
 			let f ttf range_str =
 				let glyphs = TTFJsonWriter.to_json ttf range_str in
				let ch = IO.output_channel (open_out_bin (dir ^ "/" ^ ttf.ttf_font_name ^ ".js")) in
				TTFJsonWriter.write_font ch ttf glyphs;
				IO.close_out ch;
			in
			targets := f :: !targets;
		),"<dir> : generate json-encoded glyph information to <dir>");
		("-hxswfml-debug", Arg.Unit (fun () ->
			debug_hxswfml := true;
		),": generate debug swf with hxswfml")
	] in
	if Array.length Sys.argv = 1 then
		Arg.usage basic_args usage
	else begin
		Arg.parse basic_args args_callback usage;
		match !fonts,!targets with
		| [],_ ->
			prerr_endline "Missing font argument";
			Arg.usage basic_args usage
		| _,[] ->
			prerr_endline "No targets specified (-swf|-canvas|-json)";
			Arg.usage basic_args usage
		| fonts,targets ->
			List.iter (fun font ->
				let ch = try open_in_bin font with _ -> exit ("No such file: " ^ font) in
				let ttf = TTFParser.parse ch in
				List.iter (fun target ->
					target ttf !range_str
				) targets;
				close_in ch;
			) fonts;
	end
;;
try
	process Sys.argv;
with Abort ->
	()
