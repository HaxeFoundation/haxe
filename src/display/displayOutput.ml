open Ast
open Common
open Common.DisplayMode
open Type
open Display
open Typecore
open Error

(* Old XML stuff *)

let htmlescape s =
	let s = String.concat "&amp;" (ExtString.String.nsplit s "&") in
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	let s = String.concat "&quot;" (ExtString.String.nsplit s "\"") in
	s

let get_timer_fields start_time =
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
	let fields = [("@TOTAL", FKTimer (Printf.sprintf "%.3fs" (get_time() -. start_time)), "")] in
	if !tot > 0. then
		Hashtbl.fold (fun _ t acc ->
			("@TIME " ^ t.name, FKTimer (Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot)), "") :: acc
		) Common.htimers fields
	else
		fields

let print_keywords () =
	let b = Buffer.create 0 in
	Buffer.add_string b "<list>\n";
	Hashtbl.iter (fun k _ ->
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\"></i>\n" k)
	) Lexer.keywords;
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let print_fields fields =
	let b = Buffer.create 0 in
	Buffer.add_string b "<list>\n";
	List.iter (fun (n,k,d) ->
		let s_kind, t = match k with
			| FKVar t -> "var", s_type (print_context()) t
			| FKMethod t -> "method", s_type (print_context()) t
			| FKType t -> "type", s_type (print_context()) t
			| FKPackage -> "package", ""
			| FKModule -> "type", ""
			| FKMetadata -> "metadata", ""
			| FKTimer s -> "timer", s
		in
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\" k=\"%s\"><t>%s</t><d>%s</d></i>\n" n s_kind (htmlescape t) (htmlescape d))
	) (List.sort (fun (a,ak,_) (b,bk,_) -> compare (display_field_kind_index ak,a) (display_field_kind_index bk,b)) fields);
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let maybe_print_doc d =
	Option.map_default (fun s -> Printf.sprintf " d=\"%s\"" (htmlescape s)) "" d

let print_toplevel il =
	let b = Buffer.create 0 in
	Buffer.add_string b "<il>\n";
	let s_type t = htmlescape (s_type (print_context()) t) in
	let s_doc d = maybe_print_doc d in
	List.iter (fun id -> match id with
		| IdentifierType.ITLocal v ->
			Buffer.add_string b (Printf.sprintf "<i k=\"local\" t=\"%s\">%s</i>\n" (s_type v.v_type) v.v_name);
		| IdentifierType.ITMember(c,cf) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"member\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| IdentifierType.ITStatic(c,cf) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"static\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| IdentifierType.ITEnum(en,ef) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"enum\" t=\"%s\"%s>%s</i>\n" (s_type ef.ef_type) (s_doc ef.ef_doc) ef.ef_name);
		| IdentifierType.ITEnumAbstract(a,cf) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"enumabstract\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| IdentifierType.ITGlobal(mt,s,t) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"global\" p=\"%s\" t=\"%s\">%s</i>\n" (s_type_path (t_infos mt).mt_path) (s_type t) s);
		| IdentifierType.ITType(mt) ->
			let infos = t_infos mt in
			Buffer.add_string b (Printf.sprintf "<i k=\"type\" p=\"%s\"%s>%s</i>\n" (s_type_path infos.mt_path) (s_doc infos.mt_doc) (snd infos.mt_path));
		| IdentifierType.ITPackage s ->
			Buffer.add_string b (Printf.sprintf "<i k=\"package\">%s</i>\n" s)
	) il;
	Buffer.add_string b "</il>";
	Buffer.contents b

let print_type t p doc =
	let b = Buffer.create 0 in
	if p = null_pos then
		Buffer.add_string b "<type"
	else begin
		let error_printer file line = Printf.sprintf "%s:%d:" (Path.unique_full_path file) line in
		let epos = Lexer.get_error_pos error_printer p in
		Buffer.add_string b ("<type p=\"" ^ (htmlescape epos) ^ "\"")
	end;
	Buffer.add_string b (maybe_print_doc doc);
	Buffer.add_string b ">\n";
	Buffer.add_string b (htmlescape (s_type (print_context()) t));
	Buffer.add_string b "\n</type>\n";
	Buffer.contents b

let print_signatures tl =
	let b = Buffer.create 0 in
	List.iter (fun (t,doc) ->
		Buffer.add_string b "<type";
		Option.may (fun s -> Buffer.add_string b (Printf.sprintf " d=\"%s\"" (htmlescape s))) doc;
		Buffer.add_string b ">\n";
		Buffer.add_string b (htmlescape (s_type (print_context()) (follow t)));
		Buffer.add_string b "\n</type>\n";
	) tl;
	Buffer.contents b

let print_positions pl =
	let b = Buffer.create 0 in
	let error_printer file line = Printf.sprintf "%s:%d:" (Path.get_real_path file) line in
	Buffer.add_string b "<list>\n";
	List.iter (fun p ->
		let epos = Lexer.get_error_pos error_printer p in
		Buffer.add_string b "<pos>";
		Buffer.add_string b epos;
		Buffer.add_string b "</pos>\n";
	) pl;
	Buffer.add_string b "</list>";
	Buffer.contents b


let display_memory com =
	let verbose = com.verbose in
	let print = print_endline in
	let fmt_size sz =
		if sz < 1024 then
			string_of_int sz ^ " B"
		else if sz < 1024*1024 then
			string_of_int (sz asr 10) ^ " KB"
		else
			Printf.sprintf "%.1f MB" ((float_of_int sz) /. (1024.*.1024.))
	in
	let size v =
		fmt_size (mem_size v)
	in
	Gc.full_major();
	Gc.compact();
	let mem = Gc.stat() in
	print ("Total Allocated Memory " ^ fmt_size (mem.Gc.heap_words * (Sys.word_size asr 8)));
	print ("Free Memory " ^ fmt_size (mem.Gc.free_words * (Sys.word_size asr 8)));
	(match !global_cache with
	| None ->
		print "No cache found";
	| Some c ->
		print ("Total cache size " ^ size c);
		print ("  haxelib " ^ size c.c_haxelib);
		print ("  parsed ast " ^ size c.c_files ^ " (" ^ string_of_int (Hashtbl.length c.c_files) ^ " files stored)");
		print ("  typed modules " ^ size c.c_modules ^ " (" ^ string_of_int (Hashtbl.length c.c_modules) ^ " modules stored)");
		let rec scan_module_deps m h =
			if Hashtbl.mem h m.m_id then
				()
			else begin
				Hashtbl.add h m.m_id m;
				PMap.iter (fun _ m -> scan_module_deps m h) m.m_extra.m_deps
			end
		in
		let all_modules = Hashtbl.fold (fun _ m acc -> PMap.add m.m_id m acc) c.c_modules PMap.empty in
		let modules = Hashtbl.fold (fun (path,key) m acc ->
			let mdeps = Hashtbl.create 0 in
			scan_module_deps m mdeps;
			let deps = ref [] in
			let out = ref all_modules in
			Hashtbl.iter (fun _ md ->
				out := PMap.remove md.m_id !out;
				if m == md then () else begin
				deps := Obj.repr md :: !deps;
				List.iter (fun t ->
					match t with
					| TClassDecl c ->
						deps := Obj.repr c :: !deps;
						List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_statics;
						List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_fields;
					| TEnumDecl e ->
						deps := Obj.repr e :: !deps;
						List.iter (fun n -> deps := Obj.repr (PMap.find n e.e_constrs) :: !deps) e.e_names;
					| TTypeDecl t -> deps := Obj.repr t :: !deps;
					| TAbstractDecl a -> deps := Obj.repr a :: !deps;
				) md.m_types;
				end
			) mdeps;
			let chk = Obj.repr Common.memory_marker :: PMap.fold (fun m acc -> Obj.repr m :: acc) !out [] in
			let inf = Objsize.objsize m !deps chk in
			(m,Objsize.size_with_headers inf, (inf.Objsize.reached,!deps,!out)) :: acc
		) c.c_modules [] in
		let cur_key = ref "" and tcount = ref 0 and mcount = ref 0 in
		List.iter (fun (m,size,(reached,deps,out)) ->
			let key = m.m_extra.m_sign in
			if key <> !cur_key then begin
				print (Printf.sprintf ("    --- CONFIG %s ----------------------------") (Digest.to_hex key));
				cur_key := key;
			end;
			let sign md =
				if md.m_extra.m_sign = key then "" else "(" ^ (try Digest.to_hex md.m_extra.m_sign with _ -> "???" ^ md.m_extra.m_sign) ^ ")"
			in
			print (Printf.sprintf "    %s : %s" (Ast.s_type_path m.m_path) (fmt_size size));
			(if reached then try
				incr mcount;
				let lcount = ref 0 in
				let leak l =
					incr lcount;
					incr tcount;
					print (Printf.sprintf "      LEAK %s" l);
					if !lcount >= 3 && !tcount >= 100 && not verbose then begin
						print (Printf.sprintf "      ...");
						raise Exit;
					end;
				in
				if (Objsize.objsize m deps [Obj.repr Common.memory_marker]).Objsize.reached then leak "common";
				PMap.iter (fun _ md ->
					if (Objsize.objsize m deps [Obj.repr md]).Objsize.reached then leak (Ast.s_type_path md.m_path ^ sign md);
				) out;
			with Exit ->
				());
			if verbose then begin
				print (Printf.sprintf "      %d total deps" (List.length deps));
				PMap.iter (fun _ md ->
					print (Printf.sprintf "      dep %s%s" (Ast.s_type_path md.m_path) (sign md));
				) m.m_extra.m_deps;
			end;
			flush stdout
		) (List.sort (fun (m1,s1,_) (m2,s2,_) ->
			let k1 = m1.m_extra.m_sign and k2 = m2.m_extra.m_sign in
			if k1 = k2 then s1 - s2 else if k1 > k2 then 1 else -1
		) modules);
		if !mcount > 0 then print ("*** " ^ string_of_int !mcount ^ " modules have leaks !");
		print "Cache dump complete")

module TypePathHandler = struct
	let unique l =
		let rec _unique = function
			| [] -> []
			| x1 :: x2 :: l when x1 = x2 -> _unique (x2 :: l)
			| x :: l -> x :: _unique l
		in
		_unique (List.sort compare l)

	let rec read_type_path com p =
		let classes = ref [] in
		let packages = ref [] in
		let p = (match p with
			| x :: l ->
				(try
					match PMap.find x com.package_rules with
					| Remap s -> s :: l
					| _ -> p
				with
					Not_found -> p)
			| _ -> p
		) in
		List.iter (fun path ->
			let dir = path ^ String.concat "/" p in
			let r = (try Sys.readdir dir with _ -> [||]) in
			Array.iter (fun f ->
				if (try (Unix.stat (dir ^ "/" ^ f)).Unix.st_kind = Unix.S_DIR with _ -> false) then begin
					if f.[0] >= 'a' && f.[0] <= 'z' then begin
						if p = ["."] then
							match read_type_path com [f] with
							| [] , [] -> ()
							| _ ->
								try
									match PMap.find f com.package_rules with
									| Forbidden -> ()
									| Remap f -> packages := f :: !packages
								with Not_found ->
									packages := f :: !packages
						else
							packages := f :: !packages
					end;
				end else if file_extension f = "hx" then begin
					let c = Filename.chop_extension f in
					if String.length c < 2 || String.sub c (String.length c - 2) 2 <> "__" then classes := c :: !classes;
				end;
			) r;
		) com.class_path;
		List.iter (fun (_,_,extract) ->
			Hashtbl.iter (fun (path,name) _ ->
				if path = p then classes := name :: !classes else
				let rec loop p1 p2 =
					match p1, p2 with
					| [], _ -> ()
					| x :: _, [] -> packages := x :: !packages
					| a :: p1, b :: p2 -> if a = b then loop p1 p2
				in
				loop path p
			) (extract());
		) com.swf_libs;
		List.iter (fun (path,std,close,all_files,lookup) ->
			List.iter (fun (path, name) ->
				if path = p then classes := name :: !classes else
				let rec loop p1 p2 =
					match p1, p2 with
					| [], _ -> ()
					| x :: _, [] -> packages := x :: !packages
					| a :: p1, b :: p2 -> if a = b then loop p1 p2
				in
				loop path p
			) (all_files())
		) com.java_libs;
		List.iter (fun (path,std,all_files,lookup) ->
			List.iter (fun (path, name) ->
				if path = p then classes := name :: !classes else
				let rec loop p1 p2 =
					match p1, p2 with
					| [], _ -> ()
					| x :: _, [] -> packages := x :: !packages
					| a :: p1, b :: p2 -> if a = b then loop p1 p2
				in
			loop path p
			) (all_files())
		) com.net_libs;
		unique !packages, unique !classes

	(** raise field completion listing packages and modules in a given package *)
	let complete_type_path com p =
		let packs, modules = read_type_path com p in
		if packs = [] && modules = [] then
			(error ("No classes found in " ^ String.concat "." p) Ast.null_pos)
		else
			let packs = List.map (fun n -> n,Display.FKPackage,"") packs in
			let modules = List.map (fun n -> n,Display.FKModule,"") modules in
			Some (packs @ modules)

	(** raise field completion listing module sub-types and static fields *)
	let complete_type_path_inner com p c cur_package is_import =
		try
			let sl_pack,s_module = match List.rev p with
				| s :: sl when s.[0] >= 'A' && s.[0] <= 'Z' -> List.rev sl,s
				| _ -> p,c
			in
			let ctx = Typer.create com in
			let rec lookup p =
				try
					Typeload.load_module ctx (p,s_module) Ast.null_pos
				with e ->
					if cur_package then
						match List.rev p with
						| [] -> raise e
						| _ :: p -> lookup (List.rev p)
					else
						raise e
			in
			let m = lookup sl_pack in
			let statics = ref None in
			let public_types = List.filter (fun t ->
				let tinfos = t_infos t in
				let is_module_type = snd tinfos.mt_path = c in
				if is_import && is_module_type then begin match t with
					| TClassDecl c ->
						ignore(c.cl_build());
						statics := Some c.cl_ordered_statics
					| _ -> ()
				end;
				not tinfos.mt_private
			) m.m_types in
			let types =
				if c <> s_module then
					[]
				else
					List.map (fun t ->
						let infos = t_infos t in
						(snd infos.mt_path), Display.FKModule, (Option.default "" infos.mt_doc)
					) public_types
			in
			let make_field_doc cf =
				cf.cf_name,
				(match cf.cf_kind with Method _ -> Display.FKMethod cf.cf_type | Var _ -> Display.FKVar cf.cf_type),
				(match cf.cf_doc with Some s -> s | None -> "")
			in
			let fields = match !statics with
				| None -> types
				| Some cfl -> types @ (List.map make_field_doc (List.filter (fun cf -> cf.cf_public) cfl))
			in
			Some fields
		with _ ->
			error ("Could not load module " ^ (Ast.s_type_path (p,c))) Ast.null_pos
end

(* New JSON stuff *)

open Json

(** return a range JSON structure for given position
    positions are 0-based and the result object looks like this:
    {
        start: {line: 0, character: 0},
        end: {line: 3, character: 42},
    }
*)
let pos_to_json_range p =
	if p.pmin = -1 then
		JNull
	else
		let l1, p1, l2, p2 = Lexer.get_pos_coords p in
		let to_json l c = JObject [("line", JInt (l - 1)); ("character", JInt c)] in
		JObject [
			("start", to_json l1 p1);
			("end", to_json l2 p2);
		]

let print_signature tl display_arg =
	let st = s_type (print_context()) in
	let s_arg (n,o,t) = Printf.sprintf "%s%s:%s" (if o then "?" else "") n (st t) in
	let s_fun args ret = Printf.sprintf "(%s):%s" (String.concat ", " (List.map s_arg args)) (st ret) in
	let siginf = List.map (fun (t,doc) ->
		let label = match follow t with TFun(args,ret) -> s_fun args ret | _ -> st t in
		let parameters = match follow t with
			| TFun(args,_) ->
				List.map (fun arg ->
					let label = s_arg arg in
					JObject [
						"label",JString label
					]
				) args
			| _ -> []
		in
		let js = [
			"label",JString label;
			"parameters",JArray parameters;
		] in
		JObject (match doc with None -> js | Some s -> ("documentation",JString s) :: js)
	) tl in
	let jo = JObject [
		"signatures",JArray siginf;
		"activeParameter",JInt display_arg;
		"activeSignature",JInt 0;
	] in
	let b = Buffer.create 0 in
	write_json (Buffer.add_string b) jo;
	Buffer.contents b

module StatisticsPrinter = struct
	open Statistics

	let relation_to_string = function
		| Implemented -> "implementers"
		| Extended -> "subclasses"
		| Overridden -> "overrides"
		| Referenced -> "references"

	let symbol_to_string = function
		| SKClass _ -> "class type"
		| SKInterface _ -> "interface type"
		| SKEnum _ -> "enum type"
		| SKField _ -> "class field"
		| SKEnumField _ -> "enum field"
		| SKVariable _ -> "variable"

	let print_statistics (kinds,relations) =
		let files = Hashtbl.create 0 in
		Hashtbl.iter (fun p rl ->
			let file = Path.get_real_path p.pfile in
			try
				Hashtbl.replace files file ((p,rl) :: Hashtbl.find files file)
			with Not_found ->
				Hashtbl.add files file [p,rl]
		) relations;
		let ja = Hashtbl.fold (fun file relations acc ->
			let l = List.map (fun (p,rl) ->
				let h = Hashtbl.create 0 in
				List.iter (fun (r,p) ->
					let s = relation_to_string r in
					let jo = JObject [
						"range",pos_to_json_range p;
						"file",JString (Path.get_real_path p.pfile);
					] in
					try Hashtbl.replace h s (jo :: Hashtbl.find h s)
					with Not_found -> Hashtbl.add h s [jo]
				) rl;
				let l = Hashtbl.fold (fun s js acc -> (s,JArray js) :: acc) h [] in
				let l = ("range",pos_to_json_range p) :: l in
				let l = try ("kind",JString (symbol_to_string (Hashtbl.find kinds p))) :: l with Not_found -> l in
				JObject l
			) relations in
			(JObject [
				"file",JString file;
				"statistics",JArray l
			]) :: acc
		) files [] in
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) (JArray ja);
		Buffer.contents b
end

module DiagnosticsPrinter = struct
	open Diagnostics
	open Diagnostics.DiagnosticsKind
	open DisplayTypes

	type t = DiagnosticsKind.t * pos

	module UnresolvedIdentifierSuggestion = struct
		type t =
			| UISImport
			| UISTypo

		let to_int = function
			| UISImport -> 0
			| UISTypo -> 1
	end

	let print_diagnostics ctx global =
		let com = ctx.com in
		let diag = Hashtbl.create 0 in
		let add dk p sev args =
			let file = Path.get_real_path p.pfile in
			let diag = try
				Hashtbl.find diag file
			with Not_found ->
				let d = DynArray.create() in
				Hashtbl.add diag file d;
				d
			in
			DynArray.add diag (dk,p,sev,args)
		in
		let add dk p sev args =
			if global || is_display_file p.pfile then add dk p sev args
		in
		let find_type i =
			let types = ref [] in
			Hashtbl.iter (fun _ m ->
				List.iter (fun mt ->
					let s_full_type_path (p,s) n = s_type_path (p,s) ^ if (s <> n) then "." ^ n else "" in
					let tinfos = t_infos mt in
					if snd tinfos.mt_path = i then
						types := JObject [
							"kind",JInt (UnresolvedIdentifierSuggestion.to_int UnresolvedIdentifierSuggestion.UISImport);
							"name",JString (s_full_type_path m.m_path i)
						] :: !types
				) m.m_types;
			) ctx.g.modules;
			!types
		in
		List.iter (fun (s,p,suggestions) ->
			let suggestions = List.map (fun (s,_) ->
				JObject [
					"kind",JInt (UnresolvedIdentifierSuggestion.to_int UnresolvedIdentifierSuggestion.UISTypo);
					"name",JString s
				]
			) suggestions in
			add DKUnresolvedIdentifier p DiagnosticsSeverity.Error (JArray (suggestions @ (find_type s)));
		) com.display_information.unresolved_identifiers;
		PMap.iter (fun p (r,_) ->
			if not !r then add DKUnusedImport p DiagnosticsSeverity.Warning (JArray [])
		) com.shared.shared_display_information.import_positions;
		List.iter (fun (s,p,sev) ->
			add DKCompilerError p sev (JString s)
		) com.shared.shared_display_information.diagnostics_messages;
		List.iter (fun (s,p,prange) ->
			add DKRemovableCode p DiagnosticsSeverity.Warning (JObject ["description",JString s;"range",if prange = null_pos then JNull else pos_to_json_range prange])
		) com.shared.shared_display_information.removable_code;
		let jl = Hashtbl.fold (fun file diag acc ->
			let jl = DynArray.fold_left (fun acc (dk,p,sev,jargs) ->
				(JObject [
					"kind",JInt (to_int dk);
					"severity",JInt (DiagnosticsSeverity.to_int sev);
					"range",pos_to_json_range p;
					"args",jargs
				]) :: acc
			) [] diag in
			(JObject [
				"file",JString file;
				"diagnostics",JArray jl
			]) :: acc
		) diag [] in
		let js = JArray jl in
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) js;
		Buffer.contents b
end

module ModuleSymbolsPrinter = struct
	open DisplayTypes.SymbolKind
	open DisplayTypes.SymbolInformation

	let print_module_symbols com symbols filter =
		let regex = Option.map Str.regexp_case_fold filter in
		let reported = Hashtbl.create 0 in
		let add si =
			if Hashtbl.mem reported si.pos then false
			else begin
				let b = match regex with
					| None -> true
					| Some regex -> (try ignore(Str.search_forward regex si.name 0); true with Not_found -> false)
				in
				Hashtbl.replace reported si.pos true;
				b
			end
		in
		let ja = List.fold_left (fun acc (file,l) ->
			let jl = ExtList.List.filter_map (fun si ->
				if not (add si) then
					None
				else begin
					let l =
						("name",JString si.name) ::
						("kind",JInt (to_int si.kind)) ::
						("range", pos_to_json_range si.pos) ::
						(match si.container_name with None -> [] | Some s -> ["containerName",JString s])
					in
					Some (JObject l)
				end
			) (DynArray.to_list l) in
			if jl = [] then
				acc
			else
				(JObject [
					"file",JString file;
					"symbols",JArray jl
				]) :: acc
		) [] symbols in
		let js = JArray ja in
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) js;
		Buffer.contents b
end

(* Mode processing *)

exception Completion of string

let unquote v =
	let len = String.length v in
	if len > 0 && v.[0] = '"' && v.[len - 1] = '"' then String.sub v 1 (len - 2) else v

let handle_display_argument com file_pos pre_compilation did_something =
	match file_pos with
	| "classes" ->
		pre_compilation := (fun() -> raise (Parser.TypePath (["."],None,true))) :: !pre_compilation;
	| "keywords" ->
		raise (Completion (print_keywords ()))
	| "memory" ->
		did_something := true;
		(try display_memory com with e -> prerr_endline (Printexc.get_backtrace ()));
	| "diagnostics" ->
		Common.define com Define.NoCOpt;
		com.display <- DisplayMode.create (DMDiagnostics true);
		Common.display_default := DMDiagnostics true;
	| _ ->
		let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format: " ^ file_pos) in
		let file = unquote file in
		let pos, smode = try ExtString.String.split pos "@" with _ -> pos,"" in
		let mode = match smode with
			| "position" ->
				Common.define com Define.NoCOpt;
				DMPosition
			| "usage" ->
				Common.define com Define.NoCOpt;
				DMUsage false
			(*| "rename" ->
				Common.define com Define.NoCOpt;
				DMUsage true*)
			| "package" ->
				DMPackage
			| "type" ->
				Common.define com Define.NoCOpt;
				DMType
			| "toplevel" ->
				Common.define com Define.NoCOpt;
				DMToplevel
			| "module-symbols" ->
				Common.define com Define.NoCOpt;
				DMModuleSymbols None;
			| "diagnostics" ->
				Common.define com Define.NoCOpt;
				DMDiagnostics false;
			| "statistics" ->
				Common.define com Define.NoCOpt;
				DMStatistics
			| "signature" ->
				DMSignature
			| "" ->
				DMField
			| _ ->
				let smode,arg = try ExtString.String.split smode "@" with _ -> pos,"" in
				match smode with
					| "resolve" ->
						DMResolve arg
					| "workspace-symbols" ->
						Common.define com Define.NoCOpt;
						DMModuleSymbols (Some arg)
					| _ ->
						DMField
		in
		let pos = try int_of_string pos with _ -> failwith ("Invalid format: "  ^ pos) in
		com.display <- DisplayMode.create mode;
		Common.display_default := mode;
		Common.define_value com Define.Display (if smode <> "" then smode else "1");
		Parser.use_doc := true;
		Parser.resume_display := {
			Ast.pfile = Path.unique_full_path file;
			Ast.pmin = pos;
			Ast.pmax = pos;
		}

let process_display_file com classes =
	let get_module_path_from_file_path com spath =
		let rec loop = function
			| [] -> None
			| cp :: l ->
				let cp = (if cp = "" then "./" else cp) in
				let c = Path.add_trailing_slash (Path.get_real_path cp) in
				let clen = String.length c in
				if clen < String.length spath && String.sub spath 0 clen = c then begin
					let path = String.sub spath clen (String.length spath - clen) in
					(try
						let path = Path.parse_type_path path in
						(match loop l with
						| Some x as r when String.length (Ast.s_type_path x) < String.length (Ast.s_type_path path) -> r
						| _ -> Some path)
					with _ -> loop l)
				end else
					loop l
		in
		loop com.class_path
	in
	match com.display.dms_display_file_policy with
		| DFPNo ->
			()
		| dfp ->
			if dfp = DFPOnly then begin
				classes := [];
				com.main_class <- None;
			end;
			let real = Path.get_real_path (!Parser.resume_display).Ast.pfile in
			(match get_module_path_from_file_path com real with
			| Some path ->
				if com.display.dms_kind = DMPackage then raise (DisplayPackage (fst path));
				classes := path :: !classes
			| None ->
				if not (Sys.file_exists real) then failwith "Display file does not exist";
				(match List.rev (ExtString.String.nsplit real Path.path_sep) with
				| file :: _ when file.[0] >= 'a' && file.[1] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
				| _ -> ());
				failwith "Display file was not found in class path"
			);
			Common.log com ("Display file : " ^ real);
			Common.log com ("Classes found : ["  ^ (String.concat "," (List.map Ast.s_type_path !classes)) ^ "]")

let process_global_display_mode com tctx = match com.display.dms_kind with
	| DMUsage with_definition ->
		let symbols,relations = Statistics.collect_statistics tctx in
		let rec loop acc relations = match relations with
			| (Statistics.Referenced,p) :: relations -> loop (p :: acc) relations
			| _ :: relations -> loop acc relations
			| [] -> acc
		in
		let usages = Hashtbl.fold (fun p sym acc ->
			if Statistics.is_usage_symbol sym then begin
				let acc = if with_definition then p :: acc else acc in
				(try loop acc (Hashtbl.find relations p)
				with Not_found -> acc)
			end else
				acc
		) symbols [] in
		let usages = List.sort (fun p1 p2 ->
			let c = compare p1.pfile p2.pfile in
			if c <> 0 then c else compare p1.pmin p2.pmin
		) usages in
		raise (DisplayPosition usages)
	| DMDiagnostics global ->
		Diagnostics.prepare com global;
		raise (Diagnostics (DiagnosticsPrinter.print_diagnostics tctx global))
	| DMStatistics ->
		let stats = Statistics.collect_statistics tctx in
		raise (Statistics (StatisticsPrinter.print_statistics stats))
	| DMModuleSymbols filter ->
		let symbols = com.shared.shared_display_information.document_symbols in
		let symbols = match !global_cache with
			| None -> symbols
			| Some cache ->
				let rec loop acc com =
					let com_sign = get_signature com in
					let acc = Hashtbl.fold (fun (file,sign) (_,data) acc ->
						if (filter <> None || is_display_file file) && com_sign = sign then
							(file,DocumentSymbols.collect_module_symbols data) :: acc
						else
							acc
					) cache.c_files acc in
					match com.get_macros() with None -> acc | Some com -> loop acc com
				in
				loop symbols com
		in
		raise (ModuleSymbols(ModuleSymbolsPrinter.print_module_symbols com symbols filter))
	| _ -> ()