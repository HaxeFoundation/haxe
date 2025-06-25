open Globals
open Common

let default_config defines =
	Parser.create_config defines false false DMNone false None

let file_parser_config com file =
	let open DisplayPosition in
	let in_display = display_position#get <> null_pos in
	let in_display_file = in_display && display_position#is_in_file (Path.UniqueKey.create file) in
	Parser.create_config com.defines in_display in_display_file com.display.dms_kind com.parser_state.was_auto_triggered (Some com.parser_state.special_identifier_files)
