(*
	-D dump-fingerprints=<file>: the fingerprints of the typed entities of the
	compilation and of the parsed declarations, one per line and sorted
	(docs/incremental-compilation.md, phase 4). Comparing the files of two
	compilations tells which nodes changed.
*)
open Globals
open Type

let dump com file =
	(* the modules of the compilation (before finalization, which sets com.modules) *)
	let modules_list = List.sort (fun m1 m2 -> compare m1.m_path m2.m_path) (com.Common.module_lut#fold (fun _ m acc -> m :: acc) []) in
	let lines = ref [] in
	let add key node fp = lines := Printf.sprintf "%s\t%s\t%s" key node (TFingerprint.to_string fp) :: !lines in
	List.iter (fun m ->
		EntityKey.iter_module (fun key e ->
			let k = EntityKey.to_string key in
			match e with
			| EntityKey.EType mt ->
				add k "header" (TFingerprint.type_header mt);
				add k "members" (TFingerprint.type_members mt)
			| EntityKey.EField (c,cf) ->
				add k "sig" (TFingerprint.field_sig cf);
				if TFingerprint.has_inline_body c cf then add k "inline" (TFingerprint.field_inline_body cf);
				add k "body" (TFingerprint.field_body cf)
			| EntityKey.EAnonField (_,cf) ->
				add k "sig" (TFingerprint.field_sig cf)
			| EntityKey.EEnumField _ | EntityKey.ETypeParam _ ->
				()
		) m
	) modules_list;
	Hashtbl.iter (fun file l ->
		List.iter (fun (name,fp) -> add (file ^ ":" ^ name) "decls" fp) l
	) TypeloadParse.decl_fingerprints;
	(* the dependencies of the nodes of the modules of the compilation, with
	   what they always depend on: their declaration, the imports of their
	   module and the types it declares *)
	let modules = Hashtbl.create 0 in
	List.iter (fun m -> Hashtbl.replace modules m.m_id m) modules_list;
	let lookups = Hashtbl.create 0 in
	let dep c key node = lines := Printf.sprintf "%s\t%s\tdep\t%s\t%s" c.DepRecord.c_key c.c_node key node :: !lines in
	List.iter (fun c ->
		match Hashtbl.find_opt modules c.DepRecord.c_module.m_id with
		| Some m when m == c.c_module ->
			let file = Path.UniqueKey.lazy_path m.m_extra.m_file in
			dep c (file ^ ":" ^ c.c_decl) "decls";
			dep c (file ^ ":(imports)") "decls";
			dep c (s_type_path m.m_path) "module";
			dep c "(defines)" "signature";
			(* the messages reported while computing the node, positions relative to it *)
			add c.c_key (c.c_node ^ " messages") (TFingerprint.with_state ~origin:c.c_pos ~positions:true (fun h ->
				List.iter (fun (msg,severity,p) -> TFingerprint.add_string h msg; TFingerprint.add_int h severity; TFingerprint.add_pos h p) (List.rev c.c_messages)
			));
			(* line numbers in the body depend on where the field is *)
			begin match c.c_field with
			| Some cf when (match cf.cf_expr with Some e -> TFingerprint.uses_line_numbers e | None -> false) ->
				let key = file ^ ":" ^ c.c_decl in
				dep c key "position";
				add key "position" (TFingerprint.with_state ~positions:false (fun h -> TFingerprint.add_int h (Pos.min cf.cf_pos)))
			| _ ->
				()
			end;
			Hashtbl.iter (fun k () ->
				match String.split_on_char '\t' k with
				| [key;node] ->
					if node = "lookup" then Hashtbl.replace lookups key ();
					dep c key node
				| _ -> ()
			) c.c_deps
		| _ ->
			()
	) !DepRecord.consumers;
	(* the types declared by each module, and what each lookup finds *)
	let module_types m = TFingerprint.with_state ~positions:false (fun h ->
		List.iter (fun mt -> TFingerprint.add_path h (t_infos mt).mt_path) m.m_types
	) in
	List.iter (fun m -> add (s_type_path m.m_path) "module" (module_types m)) modules_list;
	add "(defines)" "signature" (TFingerprint.with_state ~positions:false (fun h -> TFingerprint.add_string h (Define.get_signature com.Common.defines)));
	Hashtbl.iter (fun key () ->
		let fp = match List.find_opt (fun m -> s_type_path m.m_path = key) modules_list with
			| Some m -> module_types m
			| None -> (0,0)
		in
		add key "lookup" fp
	) lookups;
	let ch = open_out_bin file in
	List.iter (fun l -> output_string ch l; output_char ch '\n') (List.sort compare !lines);
	close_out ch

let maybe_dump com =
	match Common.defined_value_safe com Define.DumpFingerprints with
	| "" -> ()
	| file -> Timer.time com.Common.timer_ctx ["fingerprints"] (dump com) file
