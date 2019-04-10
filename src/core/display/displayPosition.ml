open Globals

let encloses_position p_target p =
	p_target.pmin <> -1 && p_target.pmax <> -1 && p.pmin <= p_target.pmin && p.pmax >= p_target.pmax

let encloses_position_gt p_target p =
	p_target.pmin <> -1 && p_target.pmax <> -1 && p.pmin <= p_target.pmin && p.pmax > p_target.pmax

class display_position_container =
	object (self)
		(** Current display position *)
		val mutable pos = null_pos
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
			last_pos <- p
		(**
			Get current display position
		*)
		method get =
			pos
		(**
			Clears current display position.
		*)
		method reset =
			pos <- null_pos
		(**
			Check if `p` contains current display position
		*)
		method enclosed_in p =
			encloses_position pos p
		(**
			Check if `file` contains current display position
		*)
		method is_in_file file =
			file <> "?" && Path.unique_full_path file = pos.pfile
		(**
			Cut `p` at the position of the latest `display_position#set pos` call.
		*)
		method cut p =
			{ p with pmax = last_pos.pmax }
	end

let display_position = new display_position_container