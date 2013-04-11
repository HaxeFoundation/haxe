open TTFData
open TTFTools

let rec write_glyph ttf key glyf =
	key,TTFTools.build_glyph_paths ttf false glyf

let write_font ch ttf glyphs =
	let scale = 1024. /. (float_of_int ttf.ttf_head.hd_units_per_em) in
	List.iter (fun (key,paths) ->
		IO.nwrite ch (Printf.sprintf "\tfunction key%i(ctx) {\n" key);
		IO.nwrite ch "\t\tctx.beginPath();\n";
		List.iter (fun path ->
			IO.nwrite ch (match path.gp_type with
			| 0 -> Printf.sprintf "\t\tctx.moveTo(%.2f,%.2f);\n" (path.gp_x *. scale) (path.gp_y *. scale *. (-1.))
			| 1 -> Printf.sprintf "\t\tctx.lineTo(%.2f,%.2f);\n" (path.gp_x *. scale) (path.gp_y *. scale *. (-1.))
			| 2 -> Printf.sprintf "\t\tctx.quadraticCurveTo(%.2f,%.2f,%.2f,%.2f);\n" (path.gp_cx *. scale) (path.gp_cy *. scale *. (-1.)) (path.gp_x *. scale) (path.gp_y *. scale *. (-1.))
			| _ -> assert false)
		) paths;
		IO.nwrite ch "\t\tctx.fill();\n";
		IO.nwrite ch "\t}\n";
	) glyphs;
	()

let to_canvas ttf range_str =
	let lut = TTFTools.build_lut ttf range_str in
	let glyfs = Hashtbl.fold (fun k v acc -> (k,ttf.ttf_glyfs.(v)) :: acc) lut [] in
	let glyfs = List.stable_sort (fun a b -> compare (fst a) (fst b)) glyfs in
	List.map (fun (k,g) -> write_glyph ttf k g) glyfs