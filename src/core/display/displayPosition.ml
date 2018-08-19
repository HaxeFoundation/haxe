open Globals

let display_position = ref null_pos

let is_display_file file =
	file <> "?" && Path.unique_full_path file = !display_position.pfile

let encloses_position p_target p =
	p.pmin <= p_target.pmin && p.pmax >= p_target.pmax

let encloses_position_gt p_target p =
	p.pmin <= p_target.pmin && p.pmax > p_target.pmax

let encloses_display_position p =
	encloses_position !display_position p