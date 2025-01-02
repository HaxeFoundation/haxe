open Globals
open Common
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
	let buf,close = create_dumpfile [] ((dump_path com) :: (platform_name_macro com) :: fst path @ [snd path]) in
	buf,close

let dump_types com pretty =
	let s_type = s_type (Type.print_context()) in
	let s_expr,s_type_param = if not pretty then
		(Type.s_expr_ast (not (Common.defined com Define.DumpIgnoreVarIds)) "\t"),(Printer.s_type_param "")
	else
		(Type.s_expr_pretty false "\t" true),(s_type_param s_type)
	in
	let params tl = match tl with
		| [] -> ""
		| l -> Printf.sprintf "<%s>" (String.concat ", " (List.map s_type_param l))
	in
	List.iter (fun mt ->
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
		close();
	) com.types

let dump_record com =
	List.iter (fun mt ->
		let buf,close = create_dumpfile_from_path com (t_path mt) in
		let s = match mt with
			| TClassDecl c -> Printer.s_tclass "" c
			| TEnumDecl en -> Printer.s_tenum "" en
			| TTypeDecl t -> Printer.s_tdef "" t
			| TAbstractDecl a -> Printer.s_tabstract "" a
		in
		Buffer.add_string buf s;
		close();
	) com.types

let dump_position com =
	List.iter (fun mt ->
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
	) com.types

let dump_types com =
	match Common.defined_value_safe com Define.Dump with
		| "pretty" -> dump_types com true
		| "record" -> dump_record com
		| "position" -> dump_position com
		| _ -> dump_types com false

let dump_dependencies ?(target_override=None) com =
	let target_name = match target_override with
		| None -> platform_name_macro com
		| Some s -> s
	in
	let dump_dependencies_path = [dump_path com;target_name;"dependencies"] in
	let buf,close = create_dumpfile [] dump_dependencies_path in
	let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
	let dep = Hashtbl.create 0 in
	List.iter (fun m ->
		print "%s:\n" (Path.UniqueKey.lazy_path m.m_extra.m_file);
		PMap.iter (fun _ mdep ->
			let (ctx,m2) = match mdep.md_kind with
				| MMacro when not com.is_macro_context ->
					("[macro] ", (Option.get (com.get_macros())).module_lut#find mdep.md_path)
				| _ ->
					("", com.module_lut#find mdep.md_path)
			in
			let file = Path.UniqueKey.lazy_path m2.m_extra.m_file in
			print "\t%s%s\n" ctx file;
			let l = try Hashtbl.find dep file with Not_found -> [] in
			Hashtbl.replace dep file (m :: l)
		) m.m_extra.m_deps;
	) com.Common.modules;
	close();
	let dump_dependants_path = [dump_path com;target_name;"dependants"] in
	let buf,close = create_dumpfile [] dump_dependants_path in
	let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
	Hashtbl.iter (fun n ml ->
		print "%s:\n" n;
		List.iter (fun m ->
			print "\t%s\n" (Path.UniqueKey.lazy_path m.m_extra.m_file);
		) ml;
	) dep;
	close()