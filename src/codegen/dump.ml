open Globals
open Common
open DumpConfig
open Type

(*
	Make a dump of the full typed AST of all types
*)
let create_dumpfile acc l =
	let ch = Path.create_file false ".dump" acc l in
	let buf = Buffer.create 0 in
	buf, (fun () ->
		output_string ch (Buffer.contents buf);
		close_out ch)

let create_dumpfile_from_path com path =
	let buf,close = create_dumpfile [] (com.part_scope.dump_config.dump_path :: (string_of_dump_stage com.part_scope.dump_config.dump_stage) :: (platform_name_macro com) :: fst path @ [snd path]) in
	buf,close

let dump_types com pretty =
	let restore =
		if not pretty then
			let old = !TPrinting.MonomorphPrinting.show_mono_ids in
			TPrinting.MonomorphPrinting.show_mono_ids := com.part_scope.dump_config.dump_print_ids;
			fun () -> TPrinting.MonomorphPrinting.show_mono_ids := old
		else fun () -> ()
	in
	let s_type = s_type (Type.print_context()) in
	let s_expr,s_type_param = if not pretty then
		(Type.s_expr_ast com.part_scope.dump_config.dump_print_ids "\t"),(Printer.s_type_param "")
	else
		(Type.s_expr_pretty false "\t" true),(s_type_param s_type)
	in
	let params tl = match tl with
		| [] -> ""
		| l -> Printf.sprintf "<%s>" (String.concat ", " (List.map s_type_param l))
	in
	let f mt =
		let path = Type.t_path mt in
		let buf,close = create_dumpfile_from_path com path in
		let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
		let s_metas ml tabs =
			let args el =
				match el with
				| [] -> ""
				| el -> Printf.sprintf "(%s)" (String.concat ", " (List.map (fun e -> Ast.Printer.s_expr e) el)) in
			match ml with
			| [] -> ""
			| ml -> String.concat " " (List.map (fun me -> match me with (m,el,_) -> "@" ^ Meta.to_string m ^ args el) ml) ^ "\n" ^ tabs in
		(match mt with
		| Type.TClassDecl c ->
			let s_cf_expr f =
				match f.cf_expr with
				| None -> ""
				| Some e -> Printf.sprintf "%s" (s_expr s_type e) in
			let is_inline_var v : bool = v = Var { v_read = AccInline; v_write = AccNever } in
			let rec print_field stat f =
				print "\n\t%s%s%s%s%s %s%s"
					(s_metas f.cf_meta "\t")
					(if (has_class_field_flag f CfPublic && not ((has_class_flag c CExtern) || (has_class_flag c CInterface))) then "public " else "")
					(if stat then "static " else "")
					(match f.cf_kind with
						| Var v when (is_inline_var f.cf_kind) -> "inline "
						| Var v -> ""
						| Method m ->
							match m with
							| MethNormal -> ""
							| MethDynamic -> "dynamic "
							| MethInline -> "inline "
							| MethMacro -> "macro ")
					(match f.cf_kind with Var v -> "var" | Method m -> "function")
					(f.cf_name ^ match f.cf_kind with
						| Var { v_read = AccNormal; v_write = AccNormal } -> ""
						| Var v when (is_inline_var f.cf_kind) -> ""
						| Var v -> "(" ^ s_access true v.v_read ^ "," ^ s_access false v.v_write ^ ")"
						| _ -> "")
					(params f.cf_params);
				(match f.cf_kind with
					| Var v -> print ":%s%s;" (s_type f.cf_type)
						(match f.cf_expr with
						| None -> ""
						| Some e -> " = " ^ (s_cf_expr f));
					| Method m -> if ((has_class_flag c CExtern) || (has_class_flag c CInterface)) then (
						match f.cf_type with
						| TFun(al,t) -> print "(%s):%s;" (String.concat ", " (
							List.map (fun (n,o,t) -> n ^ ":" ^ (s_type t)) al))
							(s_type t)
						| _ -> ()
					) else print "%s" (s_cf_expr f));
				print "\n";
				List.iter (fun f -> print_field stat f) f.cf_overloads
			in
			print "%s%s%s%s %s%s" (s_metas c.cl_meta "") (if c.cl_private then "private " else "") (if (has_class_flag c CExtern) then "extern " else "") (if (has_class_flag c CInterface) then "interface" else "class") (s_type_path path) (params c.cl_params);
			(match c.cl_super with None -> () | Some (c,pl) -> print " extends %s" (s_type (TInst (c,pl))));
			List.iter (fun (c,pl) -> print " implements %s" (s_type (TInst (c,pl)))) c.cl_implements;
			(match c.cl_array_access with None -> () | Some t -> print " implements ArrayAccess<%s>" (s_type t));
			print " {\n";
			(match c.cl_constructor with
			| None -> ()
			| Some f -> print_field false f);
			List.iter (print_field false) c.cl_ordered_fields;
			List.iter (print_field true) c.cl_ordered_statics;
			(match TClass.get_cl_init c with
			| None -> ()
			| Some e ->
				print "\n\tstatic function __init__() ";
				print "%s" (s_expr s_type e);
				print "\n");
			print "}";
		| Type.TEnumDecl e ->
			print "%s%s%senum %s%s {\n" (s_metas e.e_meta "") (if e.e_private then "private " else "") (if has_enum_flag e EnExtern then "extern " else "") (s_type_path path) (params e.e_params);
			List.iter (fun n ->
				let f = PMap.find n e.e_constrs in
				print "\t%s%s;\n" f.ef_name (
					match f.ef_type with
					| TFun (al,t) -> Printf.sprintf "(%s)" (String.concat ", "
						(List.map (fun (n,o,t) -> (if o then "?" else "") ^ n ^ ":" ^ (s_type t)) al))
					| _ -> "")
			) e.e_names;
			print "}"
		| Type.TTypeDecl t ->
			print "%s%stypedef %s%s = %s" (s_metas t.t_meta "") (if t.t_private then "private " else "") (s_type_path path) (params t.t_params) (s_type t.t_type);
		| Type.TAbstractDecl a ->
			print "%s%sabstract %s%s%s%s {}" (s_metas a.a_meta "") (if a.a_private then "private " else "") (s_type_path path) (params a.a_params)
			(String.concat " " (List.map (fun t -> " from " ^ s_type t) a.a_from))
			(String.concat " " (List.map (fun t -> " to " ^ s_type t) a.a_to));
		);
		close()
	in
	Parallel.run_with_pool com.sctx.pool (fun pool ->
		Parallel.ParallelArray.iter pool f (Array.of_list com.types)
	);
	restore()

let dump_record com =
	let f mt =
		let buf,close = create_dumpfile_from_path com (t_path mt) in
		let s = match mt with
			| TClassDecl c -> Printer.s_tclass "" c
			| TEnumDecl en -> Printer.s_tenum "" en
			| TTypeDecl t -> Printer.s_tdef "" t
			| TAbstractDecl a -> Printer.s_tabstract "" a
		in
		Buffer.add_string buf s;
		close()
	in
	Parallel.run_with_pool com.sctx.pool (fun pool ->
		Parallel.ParallelArray.iter pool f (Array.of_list com.types)
	)

let dump_position com =
	let f mt =
		match mt with
			| TClassDecl c ->
				let buf,close = create_dumpfile_from_path com (t_path mt) in
				Printf.bprintf buf "%s\n" (s_type_path c.cl_path);
				let field cf =
					Printf.bprintf buf "\t%s\n" cf.cf_name;
					begin match cf.cf_expr with
					| None -> ()
					| Some e ->
						Printf.bprintf buf "%s\n" (Texpr.dump_with_pos "\t" e);
					end
				in
				Option.may field c.cl_constructor;
				List.iter field c.cl_ordered_statics;
				List.iter field c.cl_ordered_fields;
				close();
			| _ ->
				()
	in
	Parallel.run_with_pool com.sctx.pool (fun pool ->
		Parallel.ParallelArray.iter pool f (Array.of_list com.types)
	)

let dump_types com =
	match com.part_scope.dump_config.dump_mode with
		| NoDump -> ()
		| Pretty -> dump_types com true
		| Record -> dump_record com
		| Position -> dump_position com
		| Ast -> dump_types com false

let dump_dependencies ?(target_override=None) com =
	let target_name = match target_override with
		| None -> platform_name_macro com
		| Some s -> s
	in
	let dump_dependencies_path = [com.part_scope.dump_config.dump_path;target_name;"dependencies"] in
	let buf,close = create_dumpfile [] dump_dependencies_path in
	let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
	let dep = Hashtbl.create 0 in
	(* Tally how the field-level dependency lattice partitions edges, to gauge how much
	   the skeleton/field tiers win over the conservative MDFull default. *)
	let n_skeleton = ref 0 and n_fields = ref 0 and n_full = ref 0 in
	let s_dep_fields = function
		| MDSkeleton -> incr n_skeleton; "skeleton"
		| MDFields fds -> incr n_fields; "fields: " ^ String.concat "," (List.map (fun fd -> fd.fd_field) fds)
		| MDFull -> incr n_full; "full"
	in
	List.iter (fun m ->
		print "%s:\n" (Path.UniqueKey.lazy_path m.m_extra.m_file);
		PMap.iter (fun _ mdep ->
			let com,ctx = match mdep.md_kind with
				| MMacro when not com.is_macro_context ->
					Option.get (com.get_macros()), "[macro] "
				| _ -> com, ""
			in
			let mdep_extra =
				try (com.module_lut#find mdep.md_path).m_extra
				with Not_found -> (com.cs#get_context mdep.md_sign)#find_module_extra mdep.md_path
			in
			let file = Path.UniqueKey.lazy_path mdep_extra.m_file in
			print "\t%s%s [%s]\n" ctx file (s_dep_fields mdep.md_fields);
			let l = try Hashtbl.find dep file with Not_found -> [] in
			Hashtbl.replace dep file (m :: l)
		) m.m_extra.m_deps;
	) com.Common.modules;
	ignore (n_skeleton,n_fields,n_full);
	(* Field-level dependency edge coverage: how many recorded edges are attributed to a specific
	   source field / target field vs still module-level (None). This is the new field-granular
	   dependency store (m_field_deps); m_deps above is its module-level projection. *)
	let n_edges = ref 0 and n_src = ref 0 and n_tgt = ref 0 and n_both = ref 0 in
	List.iter (fun m ->
		List.iter (fun e ->
			incr n_edges;
			let has_src = e.dep_src <> None and has_tgt = e.dep_tgt <> None in
			if has_src then incr n_src;
			if has_tgt then incr n_tgt;
			if has_src && has_tgt then incr n_both
		) m.m_extra.m_field_deps
	) com.Common.modules;
	let pct n = if !n_edges = 0 then 0. else 100. *. float_of_int n /. float_of_int !n_edges in
	print "\n# field-dep edges: %d total | with source field %d (%.1f%%) | with target field %d (%.1f%%) | field->field %d (%.1f%%)\n"
		!n_edges !n_src (pct !n_src) !n_tgt (pct !n_tgt) !n_both (pct !n_both);
	close();
	let dump_dependants_path = [com.part_scope.dump_config.dump_path;target_name;"dependants"] in
	let buf,close = create_dumpfile [] dump_dependants_path in
	let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
	Hashtbl.iter (fun n ml ->
		print "%s:\n" n;
		List.iter (fun m ->
			print "\t%s\n" (Path.UniqueKey.lazy_path m.m_extra.m_file);
		) ml;
	) dep;
	close()

(* Soundness check for the field-granular dependency edges (m_field_deps), using the trusted
   module-level m_deps as ground truth. For a field-driven consumer to be sound, every module
   dependency must be represented by at least one edge; a missing edge means a dependency the
   consumer would not know about. Also reports how many module deps carry field-level detail
   (an edge with a concrete target field) vs only module-level edges. *)
let verify_field_deps com =
	let n_deps = ref 0 and n_missing = ref 0 and n_fieldlevel = ref 0 and n_sourced = ref 0 in
	let missing = Hashtbl.create 0 in
	List.iter (fun m ->
		let edge_mods = Hashtbl.create 0 in
		let edge_field_mods = Hashtbl.create 0 in
		let edge_src_mods = Hashtbl.create 0 in
		List.iter (fun e ->
			Hashtbl.replace edge_mods e.dep_tgt_path ();
			if e.dep_tgt <> None then Hashtbl.replace edge_field_mods e.dep_tgt_path ();
			if e.dep_src <> None then Hashtbl.replace edge_src_mods e.dep_tgt_path ()
		) m.m_extra.m_field_deps;
		PMap.iter (fun _ mdep ->
			incr n_deps;
			if not (Hashtbl.mem edge_mods mdep.md_path) then begin
				incr n_missing;
				Hashtbl.replace missing (s_type_path m.m_path ^ " -> " ^ s_type_path mdep.md_path) ()
			end else begin
				if Hashtbl.mem edge_field_mods mdep.md_path then incr n_fieldlevel;
				if Hashtbl.mem edge_src_mods mdep.md_path then incr n_sourced
			end
		) m.m_extra.m_deps
	) com.Common.modules;
	let pct n = if !n_deps = 0 then 0. else 100. *. float_of_int n /. float_of_int !n_deps in
	print_endline (Printf.sprintf "[verify-field-deps] module deps: %d | with field-level edge %d (%.1f%%) | module-only %d (%.1f%%) | MISSING edge %d (%.1f%%, soundness bug)"
		!n_deps !n_fieldlevel (pct !n_fieldlevel) (!n_deps - !n_fieldlevel - !n_missing) (pct (!n_deps - !n_fieldlevel - !n_missing)) !n_missing (pct !n_missing));
	print_endline (Printf.sprintf "[verify-field-deps] with source field %d (%.1f%%)" !n_sourced (pct !n_sourced));
	if !n_missing > 0 then begin
		print_endline "[verify-field-deps] dependencies with NO edge (m_deps entry not in m_field_deps):";
		Hashtbl.iter (fun k () -> print_endline ("  " ^ k)) missing
	end;
	flush stdout

let maybe_generate_dump com stage =
	if com.Common.part_scope.dump_config.dump_mode <> NoDump && com.part_scope.dump_config.dump_stage = stage then begin
		Timer.time com.timer_ctx ["generate";"dump"] (fun () ->
			dump_types com;
			Option.may dump_types (com.get_macros());
		) ();
	end