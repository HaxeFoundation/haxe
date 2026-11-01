open SafeCom

let platform_name_macro com =
	if Define.defined com.defines Define.Macro then "macro"
	else Globals.platform_name com.platform

let get_dump_path com path name =
	com.dump_config.dump_path :: [platform_name_macro com] @ (fst path) @ [Printf.sprintf "%s.%s" (snd path) name]

let start_graph ?(graph_config=[]) base_path suffix =
	let ch = Path.create_file false suffix [] base_path in
	Printf.fprintf ch "digraph graphname {\n";
	List.iter (fun s -> Printf.fprintf ch "%s;\n" s) graph_config;
	ch,(fun () ->
		Printf.fprintf ch "}\n";
		close_out ch
	)