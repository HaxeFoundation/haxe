open Globals

let encloses_position p_target p =
	(Pos.min p_target) <> -1 && (Pos.max p_target) <> -1 && (Pos.min p) <= (Pos.min p_target) && (Pos.max p) >= (Pos.max p_target)

let encloses_position_gt p_target p =
	(Pos.min p_target) <> -1 && (Pos.max p_target) <> -1 && (Pos.min p) <= (Pos.min p_target) && (Pos.max p) > (Pos.max p_target)

class display_position_container =
	object (self)
		(** Current display position *)
		val mutable pos = null_pos
		val mutable file_key = None
		val mutable file_keys = []
		(**
			Display position value which was set with the latest `display_position#set p` call.
			Kept even after `display_position#reset` call.
		*)
		val mutable last_pos = null_pos
		(**
			Set current display position
		*)
		method set p =
			pos <- p;
			last_pos <- p;
			file_key <- None;
			file_keys <- if (Pos.file p) = DisplayProcessingGlobals.file_input_marker then [] else [Path.UniqueKey.create (Pos.file p)]

		method set_files files =
			file_keys <- files

		method get_files =
			file_keys

		(**
			Get current display position
		*)
		method get =
			pos
		(**
			Get current display position
		*)
		method get_file_key =
			match file_key with
			| None ->
				let key = Path.UniqueKey.create (Pos.file pos) in
				file_key <- Some key;
				key
			| Some key -> key
		(**
			Clears current display position.
		*)
		method reset =
			pos <- null_pos;
			file_key <- None;
			file_keys <- []
		(**
			Check if `p` contains current display position
		*)
		method enclosed_in p =
			encloses_position pos p
		(**
			Check if a file with `file_key` contains current display position
		*)
		method is_in_file file_key =
			((Pos.file pos) <> "?" && self#get_file_key = file_key) || self#has_file file_key

		(**
			This is a hack; currently used by Diagnostics.collect_diagnostics when sending multiple files
			to run diagnostics on via json rpc
		*)
		method has_file file_key =
			List.mem file_key file_keys

		(**
			Cut `p` at the position of the latest `display_position#set pos` call.
		*)
		method cut p =
			(Pos.with_max p ((Pos.max last_pos)))
		(**
			Temporarily reset display position, run `fn` and then restore display position.
		*)
		method run_outside : 'a . (unit->'a) -> 'a = fun fn ->
			let display_pos = self#get in
			self#reset;
			Std.finally (fun () -> self#set display_pos) fn ()

		(**
			Creates a new position with the file of [p] and the min/max of the display position.
		 *)
		method with_pos p =
			(Pos.with_range p ((Pos.min last_pos)) ((Pos.max last_pos)))
	end

let display_position = new display_position_container