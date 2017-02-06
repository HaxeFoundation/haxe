
open Globals
open Ast
open Lexer
open Common

(**
	Characters used for base64 VLQ encoding
*)
let chars = [|
	'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
	'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
	'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
	'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
|]

(**
	Encode an integer in range 0...63 (including 63)
*)
let encode_digit digit = Array.unsafe_get chars digit

(**
	Move the sign bit to the least significant bit.
	E.g.:
		1 becomes 2 (10 binary), -1 becomes 3 (11 binary)
		2 becomes 4 (100 binary), -2 becomes 5 (101 binary)
*)
let to_vlq number =
	if number < 0 then
		((-number) lsl 1) + 1
	else
		number lsl 1

(**
	Sourcemap generator for a signle `generated_file`
*)
class sourcemap_file generated_file =
	object (self)
		(** Output buffer for generated sourcemap *)
		val buffer = Rbuffer.create 1024
		(** Source Haxe files referenced by this sourcemap *)
		val files = DynArray.create()
		(** Positions of source Haxe files in `files` list *)
		val files_indexes = Hashtbl.create 100
		(** Index of a source file referenced in previous `map` call *)
		val mutable last_src_file = 0
		(** Zero-based index of a source line referenced in previous `map` call *)
		val mutable last_src_line = 0
		(** Zero-based index of a source column in a line referenced in previous `map` call *)
		val mutable last_src_col = 0
		(** Zero based index of a column in `generated_file` where last written string ended *)
		val mutable current_out_col = 0
		(** `current_out_col` value as it was when previous call to `map` was performed *)
		val mutable last_out_col = 0
		(** Indicates whether comma should be written to output buffer on next `map` call *)
		val mutable print_comma = false
	(**
		Map specified haxe position.
		This method should be called right before an expression in `pos` is writtend to generated file.
	*)
	method map pos =
		let src_file = self#get_file_index pos
		and src_line, src_col = match (Lexer.find_pos pos) with (line, col) -> (line + 1, col) in
		if print_comma then begin
			print_comma <- true;
			Rbuffer.add_char buffer ','
		end;
		self#write_base64_vlq (current_out_col - last_out_col);
		self#write_base64_vlq (src_file - last_src_file);
		self#write_base64_vlq (src_line - last_src_line);
		self#write_base64_vlq (src_col - last_src_col);
		last_src_file <- src_file;
		last_src_line <- src_line;
		last_src_col <- src_col;
		last_out_col <- current_out_col
	(**
		Should be called every time something is written to `generated_file`
	*)
	method string_written str =
		let length = String.length str in
		let rec handle_next_new_line previous_index =
			let next_index = try (String.index_from str (previous_index + 1) '\n') with Not_found -> -1 in
			if next_index < 0 then
				if previous_index >= 0 then
					begin
						print_comma <- false;
						current_out_col <- length - previous_index
					end
				else
					current_out_col <- current_out_col + length
			else begin
				Rbuffer.add_char buffer ';';
				handle_next_new_line next_index
			end
		in
		handle_next_new_line (-1);
		()
	(**
		Force new line.
	*)
	method new_line =
		print_comma <- false;
		current_out_col <- 0
	(**
		Write generated map to disk.
		If `file_name` is not provided then `generated_file` will be used with additional `.map` extension.
		E.g if `generated_file` is `path/to/file.js`, then sourcemap will be written to `path/to/file.js.map`.
		This function does not try to create missing directories.
	*)
	method generate ?file_name com =
		let file_name = match file_name with Some f -> f | None -> generated_file ^ ".map" in
		let channel = open_out file_name in
		let sources = DynArray.to_list files in
		let to_url file =
			ExtString.String.map (fun c -> if c == '\\' then '/' else c) (Path.get_full_path file)
		in
		output_string channel "{\n";
		output_string channel "\"version\":3,\n";
		output_string channel ("\"file\":\"" ^ (String.concat "\\\\" (ExtString.String.nsplit generated_file "\\")) ^ "\",\n");
		output_string channel ("\"sourceRoot\":\"file:///\",\n");
		output_string channel ("\"sources\":[" ^
			(String.concat "," (List.map (fun s -> "\"" ^ to_url s ^ "\"") sources)) ^
			"],\n");
		if Common.defined com Define.SourceMapContent then begin
			output_string channel ("\"sourcesContent\":[" ^
				(String.concat "," (List.map (fun s -> try "\"" ^ Ast.s_escape (Std.input_file ~bin:true s) ^ "\"" with _ -> "null") sources)) ^
				"],\n");
		end;
		output_string channel "\"names\":[],\n";
		output_string channel "\"mappings\":\"";
		Rbuffer.output_buffer channel buffer;
		output_string channel "\"\n";
		output_string channel "}";
		close_out channel
	(**
		Should be called when new line is inserted in generated file
	*)
	(*method new_line =
		last_out_col <- 0;
		line_is_empty <- true;
		Rbuffer.add_char buffer ';'*)
	(**
		Get source Haxe file position in a list of files referenced by this sourcemap
	*)
	method private get_file_index pos =
		try
			Hashtbl.find files_indexes pos.pfile
		with Not_found ->
			let index = (DynArray.length files) in
			Hashtbl.add files_indexes pos.pfile index;
			DynArray.add files pos.pfile;
			index
	(**
		Apply base64 VLQ encoding to `number` and write it to output buffer
	*)
	method private write_base64_vlq number =
		let rec loop vlq =
			let shift = 5 in
			let base = 1 lsl shift in
			let mask = base - 1 in
			let continuation_bit = base in
			let digit = vlq land mask in
			let next = vlq asr shift in
			Rbuffer.add_char buffer (encode_digit (
				if next > 0 then digit lor continuation_bit else digit));
			if next > 0 then loop next else ()
		in
		loop (to_vlq number)
end

(**
	This class can be used to manage sourcemaps for multiple generated files.
*)
class sourcemaps =
	object (self)
	(** Dictionary of all sourcemaps generators for each generated file. *)
	val generators = Hashtbl.create 1
	(**
		Get an object to generate sourcemap for `generated_file`.
		`generated_file` is not a file containing sourcemap, but a target language file.
	*)
	method for_file (generated_file:string) : sourcemap_file =
		try
			Hashtbl.find generators generated_file
		with Not_found ->
			let file = new sourcemap_file generated_file in
			Hashtbl.replace generators generated_file file;
			file
	(**
		Generate sourcemaps for all generated files
	*)
	method generate (com:Common.context) =
		Hashtbl.iter (fun _ (sm:sourcemap_file) -> sm#generate com) generators
end