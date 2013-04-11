open TTFData
open TTFTools

let rec write_glyph ttf key glyf =
	key,TTFTools.build_glyph_paths ttf false glyf

let write_font ch ttf glyphs =
	let scale = 1024. /. (float_of_int ttf.ttf_head.hd_units_per_em) in
	IO.nwrite ch "{\n\t";
	IO.nwrite ch (String.concat ",\n\t" (List.map (fun (key,paths) ->
		(Printf.sprintf "\"g%i\":[" key)
		^ (String.concat "," (List.map (fun path ->
			match path.gp_type with
			| 0 -> Printf.sprintf "[0,%.2f,%.2f]" (path.gp_x *. scale) (path.gp_y *. scale *. (-1.))
			| 1 -> Printf.sprintf "[1,%.2f,%.2f]" (path.gp_x *. scale) (path.gp_y *. scale *. (-1.))
			| 2 -> Printf.sprintf "[2,%.2f,%.2f,%.2f,%.2f]" (path.gp_cx *. scale) (path.gp_cy *. scale *. (-1.)) (path.gp_x *. scale) (path.gp_y *. scale *. (-1.))
			| _ -> assert false
		) paths))
		^ "]";
	) glyphs));
	IO.nwrite ch "\n}"

let to_json ttf range_str =
	let lut = TTFTools.build_lut ttf range_str in
	let glyfs = Hashtbl.fold (fun k v acc -> (k,ttf.ttf_glyfs.(v)) :: acc) lut [] in
	let glyfs = List.stable_sort (fun a b -> compare (fst a) (fst b)) glyfs in
	List.map (fun (k,g) -> write_glyph ttf k g) glyfs