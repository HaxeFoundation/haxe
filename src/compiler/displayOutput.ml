open Globals
open Ast
open Common
open Filename
open CompilationServer
open Timer
open DisplayTypes.DisplayMode
open DisplayTypes.CompletionResultKind
open CompletionItem
open CompletionClassField
open CompletionEnumField
open ClassFieldOrigin
open DisplayException
open Type
open Display
open DisplayTypes
open CompletionModuleType
open Typecore
open Genjson

(* Old XML stuff *)

let htmlescape s =
	let s = String.concat "&amp;" (ExtString.String.nsplit s "&") in
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	let s = String.concat "&quot;" (ExtString.String.nsplit s "\"") in
	s

let get_timer_fields start_time =
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Timer.htimers;
	let fields = [("@TOTAL", Printf.sprintf "%.3fs" (get_time() -. start_time))] in
	if !tot > 0. then
		Hashtbl.fold (fun _ t acc ->
			((String.concat "." t.id),(Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot))) :: acc
		) Timer.htimers fields
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
	let convert k = match k.ci_kind with
		| ITClassField({field = cf}) | ITEnumAbstractField(_,{field = cf}) ->
			let kind = match cf.cf_kind with
				| Method _ -> "method"
				| Var _ -> "var"
			in
			kind,cf.cf_name,s_type (print_context()) cf.cf_type,cf.cf_doc
		| ITEnumField ef ->
			let ef = ef.efield in
			let kind = match follow ef.ef_type with
				| TFun _ -> "method"
				| _ -> "var"
			in
			kind,ef.ef_name,s_type (print_context()) ef.ef_type,ef.ef_doc
		| ITType(cm,_) ->
			let path = CompletionItem.CompletionModuleType.get_path cm in
			"type",snd path,s_type_path path,None
		| ITPackage(path,_) -> "package",snd path,"",None
		| ITModule path -> "type",snd path,"",None
		| ITMetadata  meta ->
			let s,(doc,_) = Meta.get_info meta in
			"metadata","@" ^ s,"",Some doc
		| ITTimer(name,value) -> "timer",name,"",Some value
		| ITLiteral s ->
			let t = match k.ci_type with None -> t_dynamic | Some (t,_) -> t in
			"literal",s,s_type (print_context()) t,None
		| ITLocal v -> "local",v.v_name,s_type (print_context()) v.v_type,None
		| ITKeyword kwd -> "keyword",Ast.s_keyword kwd,"",None
		| ITExpression _ | ITAnonymous _ | ITTypeParameter _ -> assert false
	in
	let fields = List.sort (fun k1 k2 -> compare (legacy_sort k1) (legacy_sort k2)) fields in
	let fields = List.map convert fields in
	List.iter (fun(k,n,t,d) ->
		let d = match d with None -> "" | Some d -> d in
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\" k=\"%s\"><t>%s</t><d>%s</d></i>\n" n k (htmlescape t) (htmlescape d))
	) fields;
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let maybe_print_doc d =
	Option.map_default (fun s -> Printf.sprintf " d=\"%s\"" (htmlescape s)) "" d

let print_toplevel il =
	let b = Buffer.create 0 in
	Buffer.add_string b "<il>\n";
	let s_type t = htmlescape (s_type (print_context()) t) in
	let s_doc d = maybe_print_doc d in
	let identifiers = Hashtbl.create 0 in
	let check_ident s =
		if Hashtbl.mem identifiers s then false
		else begin
			Hashtbl.add identifiers s true;
			true
		end
	in
	List.iter (fun id -> match id.ci_kind with
		| ITLocal v ->
			if check_ident v.v_name then Buffer.add_string b (Printf.sprintf "<i k=\"local\" t=\"%s\">%s</i>\n" (s_type v.v_type) v.v_name);
		| ITClassField({field = cf;scope = CFSMember}) ->
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"member\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITClassField({field = cf;scope = (CFSStatic | CFSConstructor)}) ->
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"static\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITEnumField ef ->
			let ef = ef.efield in
			if check_ident ef.ef_name then Buffer.add_string b (Printf.sprintf "<i k=\"enum\" t=\"%s\"%s>%s</i>\n" (s_type ef.ef_type) (s_doc ef.ef_doc) ef.ef_name);
		| ITEnumAbstractField(a,cf) ->
			let cf = cf.field in
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"enumabstract\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITType(cm,_) ->
			let path = CompletionItem.CompletionModuleType.get_path cm in
			Buffer.add_string b (Printf.sprintf "<i k=\"type\" p=\"%s\"%s>%s</i>\n" (s_type_path path) ("") cm.name);
		| ITPackage(path,_) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"package\">%s</i>\n" (snd path))
		| ITLiteral s ->
			Buffer.add_string b (Printf.sprintf "<i k=\"literal\">%s</i>\n" s)
		| ITTimer(s,_) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"timer\">%s</i>\n" s)
		| ITTypeParameter c ->
			Buffer.add_string b (Printf.sprintf "<i k=\"type\" p=\"%s\"%s>%s</i>\n" (s_type_path c.cl_path) ("") (snd c.cl_path));
		| ITMetadata _ | ITModule _ | ITKeyword _ | ITAnonymous _ | ITExpression _ ->
			(* compat: don't add *)
			()
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
	List.iter (fun (((args,ret),_),doc) ->
		Buffer.add_string b "<type";
		Option.may (fun s -> Buffer.add_string b (Printf.sprintf " d=\"%s\"" (htmlescape s))) doc;
		Buffer.add_string b ">\n";
		Buffer.add_string b (htmlescape (s_type (print_context()) (TFun(args,ret))));
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

module Memory = struct
	open CompilationServer

	let update_module_type_deps deps md =
		deps := Obj.repr md :: !deps;
		List.iter (fun t ->
			match t with
			| TClassDecl c ->
				deps := Obj.repr c :: !deps;
				c.cl_descendants <- []; (* prevent false positive *)
				List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_statics;
				List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_fields;
			| TEnumDecl e ->
				deps := Obj.repr e :: !deps;
				List.iter (fun n -> deps := Obj.repr (PMap.find n e.e_constrs) :: !deps) e.e_names;
			| TTypeDecl t -> deps := Obj.repr t :: !deps;
			| TAbstractDecl a -> deps := Obj.repr a :: !deps;
		) md.m_types

	let rec scan_module_deps m h =
		if Hashtbl.mem h m.m_id then
			()
		else begin
			Hashtbl.add h m.m_id m;
			PMap.iter (fun _ m -> scan_module_deps m h) m.m_extra.m_deps
		end

	let get_out out =
		Obj.repr Common.memory_marker :: PMap.fold (fun m acc -> Obj.repr m :: acc) out []

	let collect_memory_stats cs =
		let all_modules = Hashtbl.fold (fun _ m acc -> PMap.add m.m_id m acc) cs.c_modules PMap.empty in
		let modules = Hashtbl.fold (fun (path,key) m acc ->
			let mdeps = Hashtbl.create 0 in
			scan_module_deps m mdeps;
			let deps = ref [Obj.repr null_module] in
			let out = ref all_modules in
			Hashtbl.iter (fun _ md ->
				out := PMap.remove md.m_id !out;
				if m == md then
					()
				else
					update_module_type_deps deps md;
			) mdeps;
			let chk = get_out !out in
			let inf = Objsize.objsize m !deps chk in
			(m,Objsize.size_with_headers inf, (inf.Objsize.reached,!deps,!out)) :: acc
		) cs.c_modules [] in
		modules

	let fmt_size sz =
		if sz < 1024 then
			string_of_int sz ^ " B"
		else if sz < 1024*1024 then
			string_of_int (sz asr 10) ^ " KB"
		else
			Printf.sprintf "%.1f MB" ((float_of_int sz) /. (1024.*.1024.))

	let size v =
		fmt_size (mem_size v)

	let get_memory_json cs =
		Gc.full_major();
		Gc.compact();
		let contexts = Hashtbl.create 0 in
		let add_context sign =
			let ctx = (sign,ref [],ref 0) in
			Hashtbl.add contexts sign ctx;
			ctx
		in
		let get_context sign =
			try
				Hashtbl.find contexts sign
			with Not_found ->
				add_context sign
		in
		let modules = collect_memory_stats cs.cache in
		List.iter (fun (m,size,(reached,deps,_)) ->
			let (_,l,mem) = get_context m.m_extra.m_sign in
			let deps = ref deps in
			update_module_type_deps deps m;
			let deps = !deps in
			let types = List.map (fun md ->
				let fields,inf = match md with
					| TClassDecl c ->
						let field acc cf =
							let repr = Obj.repr cf in
							let deps = List.filter (fun repr' -> repr' != repr) deps in
							let size = Objsize.size_with_headers (Objsize.objsize cf deps []) in
							(cf.cf_name,size) :: acc
						in
						let fields = List.fold_left field [] c.cl_ordered_fields in
						let fields = List.fold_left field fields c.cl_ordered_statics in
						let fields = List.sort (fun (_,size1) (_,size2) -> compare size2 size1) fields in
						let fields = List.map (fun (name,size) ->
							jobject [
								"path",jstring name;
								"size",jint size;
							]
						) fields in
						let repr = Obj.repr c in
						let deps = List.filter (fun repr' -> repr' != repr) deps in
						fields,Objsize.objsize c deps []
					| TEnumDecl en ->
						let repr = Obj.repr en in
						let deps = List.filter (fun repr' -> repr' != repr) deps in
						[],Objsize.objsize en deps []
					| TTypeDecl td ->
						let repr = Obj.repr td in
						let deps = List.filter (fun repr' -> repr' != repr) deps in
						[],Objsize.objsize td deps []
					| TAbstractDecl a ->
						let repr = Obj.repr a in
						let deps = List.filter (fun repr' -> repr' != repr) deps in
						[],Objsize.objsize a deps []
				in
				md,Objsize.size_with_headers inf,fields
			) m.m_types in
			let types = List.sort (fun (_,size1,_) (_,size2,_) -> compare size2 size1) types in
			let ja = List.map (fun (md,size,fields) ->
				jobject [
					"path",jstring (s_type_path (t_infos md).mt_path);
					"size",jint size;
					"fields",jarray fields;
				]
			) types in
			l := (m,size,jarray ja) :: !l;
			mem := !mem + size;
		) modules;
		let ja = Hashtbl.fold (fun key (sign,modules,size) l ->
			let modules = List.sort (fun (_,size1,_) (_,size2,_)  -> compare size2 size1) !modules in
			let modules = List.map (fun (m,size,jmt) ->
				jobject [
					"path",jstring (s_type_path m.m_path);
					"size",jint size;
					"types",jmt;
				]
			) modules in
			let j = try (List.assoc sign cs.signs).cs_json with Not_found -> jnull in
			let jo = jobject [
				"context",j;
				"size",jint !size;
				"modules",jarray modules;
			] in
			jo :: l
		) contexts [] in
		jobject [
			"contexts",jarray ja;
			"memory",jobject [
				"totalCache",jint (mem_size cs.cache);
				"haxelibCache",jint (mem_size cs.cache.c_haxelib);
				"parserCache",jint (mem_size cs.cache.c_files);
				"moduleCache",jint (mem_size cs.cache.c_modules);
			]
		]

	let display_memory com =
		let verbose = com.verbose in
		let print = print_endline in
		Gc.full_major();
		Gc.compact();
		let mem = Gc.stat() in
		print ("Total Allocated Memory " ^ fmt_size (mem.Gc.heap_words * (Sys.word_size asr 8)));
		print ("Free Memory " ^ fmt_size (mem.Gc.free_words * (Sys.word_size asr 8)));
		(match get() with
		| None ->
			print "No cache found";
		| Some {cache = c} ->
			print ("Total cache size " ^ size c);
			print ("  haxelib " ^ size c.c_haxelib);
			print ("  parsed ast " ^ size c.c_files ^ " (" ^ string_of_int (Hashtbl.length c.c_files) ^ " files stored)");
			print ("  typed modules " ^ size c.c_modules ^ " (" ^ string_of_int (Hashtbl.length c.c_modules) ^ " modules stored)");
			let modules = collect_memory_stats c in
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
				print (Printf.sprintf "    %s : %s" (s_type_path m.m_path) (fmt_size size));
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
						if (Objsize.objsize m deps [Obj.repr md]).Objsize.reached then leak (s_type_path md.m_path ^ sign md);
					) out;
				with Exit ->
					());
				if verbose then begin
					print (Printf.sprintf "      %d total deps" (List.length deps));
					PMap.iter (fun _ md ->
						print (Printf.sprintf "      dep %s%s" (s_type_path md.m_path) (sign md));
					) m.m_extra.m_deps;
				end;
				flush stdout
			) (List.sort (fun (m1,s1,_) (m2,s2,_) ->
				let k1 = m1.m_extra.m_sign and k2 = m2.m_extra.m_sign in
				if k1 = k2 then s1 - s2 else if k1 > k2 then 1 else -1
			) modules);
			if !mcount > 0 then print ("*** " ^ string_of_int !mcount ^ " modules have leaks !");
			print "Cache dump complete")
end

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
					| Directory d -> d :: l
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
									| Directory _ -> raise Not_found
								with Not_found ->
									packages := f :: !packages
						else
							packages := f :: !packages
					end;
				end else if file_extension f = "hx" && f <> "import.hx" then begin
					let c = Filename.chop_extension f in
					try
						ignore(String.index c '.')
					with Not_found ->
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
			(abort ("No modules found in " ^ String.concat "." p) null_pos)
		else
			let packs = List.map (fun n -> make_ci_package (p,n) []) packs in
			let modules = List.map (fun n -> make_ci_module (p,n)) modules in
			Some (packs @ modules)

	(** raise field completion listing module sub-types and static fields *)
	let complete_type_path_inner com p c cur_package is_import =
		try
			let sl_pack,s_module = match List.rev p with
				| s :: sl when s.[0] >= 'A' && s.[0] <= 'Z' -> List.rev sl,s
				| _ -> p,c
			in
			let ctx = Typer.create com in
			(* This is a bit wacky: We want to reset the display position so that revisiting the display file
			   does not raise another TypePath exception. However, we still want to have it treated like the
			   display file, so we just set the position to 0 (#6558). *)
			DisplayPosition.display_position#set {DisplayPosition.display_position#get with pmin = 0; pmax = 0};
			let rec lookup p =
				try
					TypeloadModule.load_module ctx (p,s_module) null_pos
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
			let enum_statics = ref None in
			let public_types = List.filter (fun t ->
				let tinfos = t_infos t in
				let is_module_type = snd tinfos.mt_path = c in
				if is_import && is_module_type then begin match t with
					| TClassDecl c | TAbstractDecl {a_impl = Some c} ->
						ignore(c.cl_build());
						statics := Some c
					| TEnumDecl en ->
						enum_statics := Some en
					| _ -> ()
				end;
				not tinfos.mt_private
			) m.m_types in
			let types =
				if c <> s_module then
					[]
				else
					List.map (fun mt ->
						make_ci_type (CompletionItem.CompletionModuleType.of_module_type mt) ImportStatus.Imported None
					) public_types
			in
			let class_origin c = match c.cl_kind with
				| KAbstractImpl a -> Self (TAbstractDecl a)
				| _ -> Self (TClassDecl c)
			in
			let tpair t =
				(t,DisplayEmitter.completion_type_of_type ctx t)
			in
			let make_field_doc c cf =
				make_ci_class_field (CompletionClassField.make cf CFSStatic (class_origin c) true) (tpair cf.cf_type)
			in
			let fields = match !statics with
				| None -> types
				| Some c -> types @ (List.map (make_field_doc c) (List.filter (fun cf -> has_class_field_flag cf CfPublic) c.cl_ordered_statics))
			in
			let fields = match !enum_statics with
				| None -> fields
				| Some en -> PMap.fold (fun ef acc ->
					make_ci_enum_field (CompletionEnumField.make ef (Self (TEnumDecl en)) true) (tpair ef.ef_type) :: acc
				) en.e_constrs fields
			in
			Some fields
		with _ ->
			abort ("Could not load module " ^ (s_type_path (p,c))) null_pos
end

(* New JSON stuff *)

open Json

let print_signature tl display_arg =
	let st = s_type (print_context()) in
	let s_arg (n,o,t) = Printf.sprintf "%s%s:%s" (if o then "?" else "") n (st t) in
	let s_fun args ret = Printf.sprintf "(%s):%s" (String.concat ", " (List.map s_arg args)) (st ret) in
	let siginf = List.map (fun (((args,ret),_),doc) ->
		let label = s_fun args ret in
		let parameters =
			List.map (fun arg ->
					let label = s_arg arg in
					JObject [
						"label",JString label
					]
			) args
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
	string_of_json jo

(* Mode processing *)

exception Completion of string

let unquote v =
	let len = String.length v in
	if len > 0 && v.[0] = '"' && v.[len - 1] = '"' then String.sub v 1 (len - 2) else v

let handle_display_argument com file_pos pre_compilation did_something =
	match file_pos with
	| "classes" ->
		pre_compilation := (fun() -> raise (Parser.TypePath (["."],None,true,null_pos))) :: !pre_compilation;
	| "keywords" ->
		raise (Completion (print_keywords ()))
	| "memory" ->
		did_something := true;
		(try Memory.display_memory com with e -> prerr_endline (Printexc.get_backtrace ()));
	| "diagnostics" ->
		Common.define com Define.NoCOpt;
		com.display <- DisplayMode.create (DMDiagnostics true);
		Parser.display_mode := DMDiagnostics true;
	| _ ->
		let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format: " ^ file_pos) in
		let file = unquote file in
		let pos, smode = try ExtString.String.split pos "@" with _ -> pos,"" in
		let mode = match smode with
			| "position" ->
				Common.define com Define.NoCOpt;
				DMDefinition
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
				DMHover
			| "toplevel" ->
				DMDefault
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
				Common.define com Define.NoCOpt;
				DMSignature
			| "" ->
				DMDefault
			| _ ->
				let smode,arg = try ExtString.String.split smode "@" with _ -> pos,"" in
				match smode with
					| "resolve" ->
						DMResolve arg
					| "workspace-symbols" ->
						Common.define com Define.NoCOpt;
						DMModuleSymbols (Some arg)
					| _ ->
						DMDefault
		in
		let pos = try int_of_string pos with _ -> failwith ("Invalid format: "  ^ pos) in
		com.display <- DisplayMode.create mode;
		Parser.display_mode := mode;
		if not com.display.dms_full_typing then Common.define_value com Define.Display (if smode <> "" then smode else "1");
		DisplayPosition.display_position#set {
			pfile = Path.unique_full_path file;
			pmin = pos;
			pmax = pos;
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
						let path = Path.parse_path path in
						(match loop l with
						| Some x as r when String.length (s_type_path x) < String.length (s_type_path path) -> r
						| _ -> Some path)
					with _ -> loop l)
				end else
					loop l
		in
		loop com.class_path
	in
	match com.display.dms_display_file_policy with
		| DFPNo ->
			None
		| dfp ->
			if dfp = DFPOnly then begin
				classes := [];
				com.main_class <- None;
			end;
			let real = Path.get_real_path (DisplayPosition.display_position#get).pfile in
			let path = match get_module_path_from_file_path com real with
			| Some path ->
				if com.display.dms_kind = DMPackage then raise_package (fst path);
				classes := path :: !classes;
				Some path
			| None ->
				if not (Sys.file_exists real) then failwith "Display file does not exist";
				(match List.rev (ExtString.String.nsplit real Path.path_sep) with
				| file :: _ when file.[0] >= 'a' && file.[0] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
				| _ -> ());
				failwith "Display file was not found in class path"
			in
			Common.log com ("Display file : " ^ real);
			Common.log com ("Classes found : ["  ^ (String.concat "," (List.map s_type_path !classes)) ^ "]");
			path

let process_global_display_mode com tctx = match com.display.dms_kind with
	| DMUsage with_definition ->
		FindReferences.find_references tctx com with_definition
	| DMDiagnostics global ->
		Diagnostics.run com global
	| DMStatistics ->
		let stats = Statistics.collect_statistics tctx (SFFile (DisplayPosition.display_position#get).pfile) in
		raise_statistics (Statistics.Printer.print_statistics stats)
	| DMModuleSymbols (Some "") -> ()
	| DMModuleSymbols filter ->
		let symbols = match CompilationServer.get() with
			| None -> []
			| Some cs ->
				let l = CompilationServer.get_context_files cs ((Define.get_signature com.defines) :: (match com.get_macros() with None -> [] | Some com -> [Define.get_signature com.defines])) in
				List.fold_left (fun acc (file,cfile) ->
					if (filter <> None || DisplayPosition.display_position#is_in_file file) then
						(file,DocumentSymbols.collect_module_symbols (filter = None) (cfile.c_package,cfile.c_decls)) :: acc
					else
						acc
				) [] l
		in
		raise_module_symbols (DocumentSymbols.Printer.print_module_symbols com symbols filter)
	| _ -> ()

let find_doc t =
	let doc = match follow t with
		| TAnon an ->
			begin match !(an.a_status) with
				| Statics c -> c.cl_doc
				| EnumStatics en -> en.e_doc
				| AbstractStatics a -> a.a_doc
				| _ -> None
			end
		| _ ->
			None
	in
	doc

let handle_syntax_completion com kind p =
	let open Parser in
	let l,kind = match kind with
		| SCClassRelation ->
			[Extends;Implements],CRTypeRelation
		| SCInterfaceRelation ->
			[Extends],CRTypeRelation
		| SCComment ->
			[],CRTypeRelation
		| SCTypeDecl mode ->
			let in_import_hx = Filename.basename p.pfile = "import.hx" in
			let l = if in_import_hx then [] else [Private;Extern;Class;Interface;Enum;Abstract;Typedef;Final] in
			let l = match mode with
				| TCBeforePackage -> Package :: Import :: Using :: l
				| TCAfterImport -> Import :: Using :: l
				| TCAfterType -> l
			in
			l,CRTypeDecl
		| SCAfterTypeFlag flags ->
			let l = [Class;Interface] in
			let l = if List.mem DPrivate flags then l else Private :: l in
			let l = if List.mem DExtern flags then l else Extern :: l in
			let l = if List.mem DFinal flags then l else
				Final :: Enum :: Abstract :: Typedef :: l
			in
			l,CRTypeDecl
	in
	match l with
	| [] ->
		()
	| _ ->
		let l = List.map make_ci_keyword l in
		match com.json_out with
		| None ->
			let b = Buffer.create 0 in
			Buffer.add_string b "<il>\n";
			List.iter (fun item -> match item.ci_kind with
				| ITKeyword kwd -> Buffer.add_string b (Printf.sprintf "<i k=\"keyword\">%s</i>" (s_keyword kwd));
				| _ -> assert false
			) l;
			Buffer.add_string b "</il>";
			let s = Buffer.contents b in
			raise (Completion s)
		| Some(f,_) ->
			let ctx = Genjson.create_context GMFull in
			f(fields_to_json ctx l kind None)