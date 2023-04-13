open Extlib_leftovers
open Globals
open Common
open CompilationContext

let resolve_source file l1 p1 l2 p2 =
	let ch = open_in_bin file in
	let curline = ref 1 in
	let lines = ref [] in
	let rec loop p line =
		let inc i line =
			if (!curline >= l1) && (!curline <= l2) then lines := (!curline, line) :: !lines;
			curline := !curline + 1;
			(i, "")
		in

		let input_char_or_done ch line =
			try input_char ch with End_of_file -> begin
				ignore(inc 0 line);
				raise End_of_file
			end
		in

		try
			let read_char line = match input_char_or_done ch line with
				| '\n' -> inc 1 line
				| '\r' ->
					ignore(input_char_or_done ch line);
					inc 2 line
				| c -> begin
					let line = ref (line ^ (String.make 1 c)) in
					let rec skip n =
						if n > 0 then begin
							let c = input_char_or_done ch !line in
							line := !line ^ (String.make 1 c);
							skip (n - 1)
						end
					in

					let code = int_of_char c in
					if code < 0xC0 then ()
					else if code < 0xE0 then skip 1
					else if code < 0xF0 then skip 2
					else skip 3;

					(1, !line)
				end
			in

			let (delta, line) = read_char line in
			loop (p + delta) line
		with End_of_file ->
			close_in ch;
	in

	loop 0 "";
	List.rev !lines

let resolve_file ctx f =
		let ext = Common.extension f in
		let second_ext = Common.extension (Common.remove_extension f) in
		let platform_ext = "." ^ (platform_name_macro ctx) in
		if platform_ext = second_ext then
			(Common.remove_extension (Common.remove_extension f)) ^ ext
		else
			f

let error_printer file line = Printf.sprintf "%s:%d:" file line

type error_context = {
	mutable last_positions : pos IntMap.t;
	mutable max_lines : int IntMap.t;
	mutable gutter : int IntMap.t;
	mutable previous : (pos * MessageSeverity.t * int) option;
}

let create_error_context () = {
	last_positions = IntMap.empty;
	max_lines = IntMap.empty;
	gutter = IntMap.empty;
	previous = None;
}

let compiler_pretty_message_string com ectx cm =
	match cm.cm_message with
	(* Filter some messages that don't add much when using this message renderer *)
	| "End of overload failure reasons" -> None
	| _ -> begin
		ectx.last_positions <- (IntMap.add cm.cm_depth cm.cm_pos ectx.last_positions);
		let is_null_pos = cm.cm_pos = null_pos || cm.cm_pos.pmin = -1 in
		let is_unknown_file f = f = "" || f = "?" in

		(* Extract informations from position *)
		let l1, p1, l2, p2, epos, lines =
			if is_null_pos then begin
				let epos = if is_unknown_file cm.cm_pos.pfile then "(unknown position)" else cm.cm_pos.pfile in
				(-1, -1, -1, -1, epos, [])
			end else try begin
				let f = resolve_file com cm.cm_pos.pfile in
				let f = Common.find_file com f in
				let l1, p1, l2, p2 = Lexer.get_pos_coords cm.cm_pos in
				let lines = resolve_source f l1 p1 l2 p2 in
				let epos = Lexer.get_error_pos error_printer cm.cm_pos in
				(l1, p1, l2, p2, epos, lines)
			end with Not_found ->
				(1, 1, 1, 1, cm.cm_pos.pfile, [])
			in

		(* If 4 lines or less, display all; if more, crop the middle *)
		let lines = match lines with
			| _ :: (_ :: (_ :: (_ :: []))) -> lines
			| hd :: (_ :: (_ :: (_ :: l))) ->
				let _,line = hd in
				let indent = ref 0 in
				let found = ref false in

				while (not !found) && (!indent < (String.length line - 1)) do
					found := not (Lexer.is_whitespace (String.unsafe_get line !indent));
					indent := !indent + 1
				done;

				[hd; (0, (String.make (!indent+1) ' ') ^ "[...]"); List.hd (List.rev l)]
			| _ -> lines
		in

		let parent_pos =
			if cm.cm_depth = 0 then null_pos
			else (try IntMap.find (cm.cm_depth-1) ectx.last_positions with Not_found -> null_pos)
		in

		let prev_pos,prev_sev,prev_nl = match ectx.previous with
			| None -> (None, None, 0)
			| Some (p, sev, depth) -> (Some p, Some sev, depth)
		in

		let sev_changed = prev_sev = None || Some cm.cm_severity <> prev_sev in
		let pos_changed = (prev_pos = None || cm.cm_pos <> Option.get prev_pos || (cm.cm_depth <> prev_nl && cm.cm_depth <> prev_nl + 1)) && (parent_pos = null_pos || cm.cm_pos <> parent_pos) in
		let file_changed = prev_pos = None || (pos_changed && match (cm.cm_pos.pfile, (Option.get prev_pos).pfile) with
			| (f1, f2) when (is_unknown_file f1) && (is_unknown_file f2) -> false
			| (f1, f2) -> f1 <> f2
		) in

		let display_heading = cm.cm_depth = 0 || sev_changed || file_changed in
		let has_source = match lines with | [] -> false | _ -> true in
		let display_source = has_source && (cm.cm_depth = 0 || sev_changed || pos_changed) in
		let display_pos_marker = (not is_null_pos) && has_source && (cm.cm_depth = 0 || sev_changed || pos_changed) in

		let gutter_len = (try String.length (Printf.sprintf "%d" (IntMap.find cm.cm_depth ectx.max_lines)) with Not_found -> 0) + 2 in

		let no_color = Define.defined com.defines Define.NoColor in
		let c_reset = if no_color then "" else "\x1b[0m" in
		let c_bold = if no_color then "" else "\x1b[1m" in
		let c_dim = if no_color then "" else "\x1b[2m" in

		let (c_sev, c_sev_bg) = if no_color then ("", "") else match cm.cm_severity with
			| MessageSeverity.Warning -> ("\x1b[33m", "\x1b[30;43m")
			| Information | Hint -> ("\x1b[34m", "\x1b[30;44m")
			| Error -> ("\x1b[31m", "\x1b[30;41m")
		in

		let sev_label = if cm.cm_depth > 0 then " -> " else Printf.sprintf
			(if no_color then "[%s]" else " %s ")
			(match cm.cm_severity with
				| MessageSeverity.Warning -> "WARNING"
				| Information -> "INFO"
				| Hint -> "HINT"
				| Error -> "ERROR"
			) in

		let out = ref "" in

		if display_heading then
			out := Printf.sprintf "%s%s%s\n\n"
				(* Severity heading *)
				(c_sev_bg ^ sev_label ^ c_reset ^ " ")
				(* Macro context indicator *)
				(if cm.cm_from_macro then c_sev ^ "(macro) " ^ c_reset else "")
				(* File + line pointer *)
				epos;

		(* Error source *)
		if display_source then out := List.fold_left (fun out (l, line) ->
			let nb_len = String.length (string_of_int l) in

			(* Replace tabs with 1 space to avoid column misalignments *)
			let line = String.concat " " (ExtString.String.nsplit line "\t") in
			let len = String.length line in

			out ^ Printf.sprintf "%s%s | %s\n"
				(* left-padded line number *)
				(String.make (gutter_len-nb_len-1) ' ')
				(if l = 0 then "-" else Printf.sprintf "%d" l)
				(* Source code at that line *)
				(
					if l = 0 then
						c_dim ^ line ^ c_reset
					else if l1 = l2 then
						(if p1 > 1 then c_dim ^ (String.sub line 0 (p1-1)) else "")
						^ c_reset ^ c_bold ^ (String.sub line (p1-1) (p2-p1))
						^ c_reset ^ c_dim ^ (String.sub line (p2-1) (len - p2 + 1))
						^ c_reset
					else begin
						(if (l = l1) then
							(if p1 > 1 then c_dim ^ (String.sub line 0 (p1-1)) else "")
							^ c_reset ^ c_bold ^ (String.sub line (p1-1) (len-p1+1))
							^ c_reset
						else if (l = l2) then
							(if p2 > 1 then c_bold ^ (String.sub line 0 (p2-1)) else "")
							^ c_reset ^ c_dim ^ (String.sub line (p2-1) (len-p2+1))
							^ c_reset
						else c_bold ^ line ^ c_reset)
					end
				)
		) !out lines;

		(* Error position marker *)
		if display_pos_marker then
			out := Printf.sprintf "%s%s|%s\n"
				!out
				(String.make gutter_len ' ')
				(if l1 = l2 then String.make p1 ' ' ^ c_sev ^ String.make (if p1 = p2 then 1 else p2-p1) '^' ^ c_reset else "");

		(* Error message *)
		out := List.fold_left (fun out str -> Printf.sprintf "%s%s| %s\n"
			out
			(String.make gutter_len ' ')
			(* Remove "... " prefix *)
			(if (ExtString.String.starts_with str "... ") then String.sub str 4 ((String.length str) - 4) else str)
		) !out (ExtString.String.nsplit cm.cm_message "\n");

		ectx.previous <- Some ((if is_null_pos then null_pos else cm.cm_pos), cm.cm_severity, cm.cm_depth);
		ectx.gutter <- (IntMap.add cm.cm_depth gutter_len ectx.gutter);

		(* Indent sub errors *)
		let rec indent ?(acc=0) depth =
			if depth = 0 then acc
			else indent ~acc:(acc + try IntMap.find (depth-1) ectx.gutter with Not_found -> 3) (depth-1)
		in

		Some (
			if cm.cm_depth > 0 then String.concat "\n" (List.map (fun str -> match str with
				| "" -> ""
				| _ -> (String.make (indent cm.cm_depth) ' ') ^ str
			) (ExtString.String.nsplit !out "\n"))
			else !out
		)
	end

let compiler_message_string cm =
	let str = match cm.cm_severity with
		| MessageSeverity.Warning -> "Warning : " ^ cm.cm_message
		| Information | Error | Hint -> cm.cm_message
	in

	if cm.cm_pos = null_pos then
		Some str
	else begin
		let epos = Lexer.get_error_pos error_printer cm.cm_pos in
		let str =
			let lines =
				match (ExtString.String.nsplit str "\n") with
				| first :: rest -> first :: List.map Error.compl_msg rest
				| l -> l
			in
			String.concat ("\n" ^ epos ^ " : ") lines
		in
		Some (Printf.sprintf "%s : %s" epos str)
	end

let compiler_indented_message_string cm =
	match cm.cm_message with
	(* Filter some messages that don't add much when using this message renderer *)
	| "End of overload failure reasons" -> None
	| _ ->
		let str = match cm.cm_severity with
			| MessageSeverity.Warning -> "Warning : " ^ cm.cm_message
			| Information -> "Info : " ^ cm.cm_message
			| Error | Hint -> cm.cm_message
		in

		if cm.cm_pos = null_pos then
			Some str
		else begin
			let epos = Lexer.get_error_pos error_printer cm.cm_pos in
			let lines =
				match (ExtString.String.nsplit str "\n") with
				| first :: rest -> (cm.cm_depth, first) :: List.map (fun msg -> (cm.cm_depth+1, msg)) rest
				| l -> [(cm.cm_depth, List.hd l)]
			in
			let rm_prefix str = if (ExtString.String.starts_with str "... ") then String.sub str 4 ((String.length str) - 4) else str in
			Some (String.concat "\n" (List.map (fun (depth, msg) -> (String.make (depth*2) ' ') ^ epos ^ " : " ^ (rm_prefix msg)) lines))
		end

let get_max_line max_lines messages =
	List.fold_left (fun max_lines cm ->
		let _,_,l2,_ = Lexer.get_pos_coords cm.cm_pos in
		let old = try IntMap.find cm.cm_depth max_lines with Not_found -> 0 in

		if l2 > old then IntMap.add cm.cm_depth l2 max_lines
		else max_lines
	) max_lines messages

exception ConfigError of string

let get_formatter com ectx def default =
	let format_mode = Define.defined_value_safe ~default com.defines def in
	match format_mode with
		| "pretty" -> compiler_pretty_message_string com ectx
		| "indent" -> compiler_indented_message_string
		| "classic" -> compiler_message_string
		| m -> begin
			let def = Define.get_define_key def in
			raise (ConfigError (Printf.sprintf "Invalid message reporting mode: \"%s\", expected classic | pretty | indent (for -D %s)." m def))
		end

let print_error (err : Error.error) =
	let ret = ref "" in
	Error.recurse_error (fun depth err ->
		ret := !ret ^ (Lexer.get_error_pos (Printf.sprintf "%s:%d: ") err.err_pos) ^ (Error.error_msg err.err_message) ^ "\n"
	) err;
	!ret

let format_messages com messages =
	let ectx = create_error_context () in
	ectx.max_lines <- get_max_line ectx.max_lines messages;
	let message_formatter = get_formatter com ectx Define.MessageReporting "classic" in
	let lines = List.rev (
		List.fold_left (fun lines cm -> match (message_formatter cm) with
			| None -> lines
			| Some str -> str :: lines
		) [] messages
	) in
	ExtLib.String.join "\n" lines

let display_messages ctx on_message = begin
	let ectx = create_error_context () in
	ectx.max_lines <- get_max_line ectx.max_lines ctx.messages;

	let get_formatter _ _ def default =
		try get_formatter ctx.com ectx def default
		with | ConfigError s ->
			error_msg ctx s null_pos;
			compiler_message_string
	in

	let message_formatter = get_formatter ctx.com ectx Define.MessageReporting "classic" in
	let log_formatter = get_formatter ctx.com ectx Define.MessagesLogFormat "indent" in

	let log_messages = ref (Define.defined ctx.com.defines Define.MessagesLogFile) in
	let log_message = ref None in
	let close_logs = ref None in

	if !log_messages then begin
		try begin
			let buf = Rbuffer.create 16000 in

			let file = Define.defined_value ctx.com.defines Define.MessagesLogFile in
			let chan =
				Path.mkdir_from_path file;
				open_out_bin file
			in

			log_message := (Some (fun msg ->
				match (log_formatter msg) with
					| None -> ()
					| Some str -> Rbuffer.add_string buf (str ^ "\n")));

			close_logs := (Some (fun () ->
				Rbuffer.output_buffer chan buf;
				Rbuffer.clear buf;
				close_out chan
			));
		end with
			| Failure e | Sys_error e -> begin
				let def = Define.get_define_key Define.MessagesLogFile in
				error_msg ctx (Printf.sprintf "Error opening log file: %s. Logging to file disabled (-D %s)" e def) null_pos;
				log_messages := false;
			end
	end;

	List.iter (fun cm ->
		if !log_messages then (Option.get !log_message) cm;

		match (message_formatter cm) with
			| None -> ()
			| Some str -> on_message cm.cm_severity str
	) (List.rev ctx.messages);

	if !log_messages then (Option.get !close_logs) ();
end

