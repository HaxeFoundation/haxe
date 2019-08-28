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
		| ITExpression _ | ITAnonymous _ | ITTypeParameter _ | ITDefine _ -> assert false
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
		| ITMetadata _ | ITModule _ | ITKeyword _ | ITAnonymous _ | ITExpression _ | ITDefine _ ->
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

	type memory_request =
		| MCache
		| MContext of string
		| MModule of string * path

	let clear_descendants md =
		List.iter (function
			| TClassDecl c ->
				c.cl_descendants <- []
			| _ ->
				()
		) md.m_types

	let update_module_type_deps deps md =
		let deps = ref (Obj.repr md :: deps) in
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
		) md.m_types;
		!deps

	let rec scan_module_deps m h =
		if Hashtbl.mem h m.m_id then
			()
		else begin
			Hashtbl.add h m.m_id m;
			PMap.iter (fun _ m -> scan_module_deps m h) m.m_extra.m_deps
		end

	let module_sign key md =
		if md.m_extra.m_sign = key then "" else "(" ^ (try Digest.to_hex md.m_extra.m_sign with _ -> "???" ^ md.m_extra.m_sign) ^ ")"

	let collect_leaks m deps out =
		let leaks = ref [] in
		let leak s =
			leaks := s :: !leaks
		in
		if (Objsize.objsize m deps [Obj.repr Common.memory_marker]).Objsize.reached then leak "common";
		PMap.iter (fun _ md ->
			if (Objsize.objsize m deps [Obj.repr md]).Objsize.reached then leak (s_type_path md.m_path ^ module_sign m.m_extra.m_sign md);
		) out;
		!leaks

	let get_out out =
		Obj.repr Common.memory_marker :: PMap.fold (fun m acc -> Obj.repr m :: acc) out []

	let get_module_memory cs all_modules m =
		let mdeps = Hashtbl.create 0 in
		scan_module_deps m mdeps;
		let deps = ref [Obj.repr null_module] in
		let out = ref all_modules in
		let deps = Hashtbl.fold (fun _ md deps ->
			out := PMap.remove md.m_id !out;
			if m == md then
				deps
			else
				update_module_type_deps deps md;
		) mdeps !deps in
		clear_descendants m;
		let out = !out in
		let chk = get_out out in
		let inf = Objsize.objsize m deps chk in
		let leaks = if inf.reached then collect_leaks m deps out else [] in
		(Objsize.size_with_headers inf,(inf.reached,deps,out,leaks))

	let fmt_size sz =
		if sz < 1024 then
			string_of_int sz ^ " B"
		else if sz < 1024*1024 then
			string_of_int (sz asr 10) ^ " KB"
		else
			Printf.sprintf "%.1f MB" ((float_of_int sz) /. (1024.*.1024.))

	let size v =
		fmt_size (mem_size v)

	let get_memory_json (cs : CompilationServer.t) mreq =
		begin match mreq with
		| MCache ->
			Gc.compact();
			let stat = Gc.quick_stat() in
			let size = (float_of_int stat.Gc.heap_words) *. (float_of_int (Sys.word_size / 8)) in
			let cache_mem = cs#get_pointers in
			let contexts = cs#get_contexts in
			let j_contexts = List.map (fun cc -> jobject [
				"context",cc#get_json;
				"size",jint (mem_size cc);
			]) contexts in
			jobject [
				"contexts",jarray j_contexts;
				"memory",jobject [
					"totalCache",jint (mem_size cs);
					"contextCache",jint (mem_size cache_mem.(0));
					"haxelibCache",jint (mem_size cache_mem.(1));
					"directoryCache",jint (mem_size cache_mem.(2));
					"nativeLibCache",jint (mem_size cache_mem.(3));
					"additionalSizes",jarray [
						jobject ["name",jstring "macro interpreter";"size",jint (mem_size (MacroContext.macro_interp_cache))];
						jobject ["name",jstring "last completion result";"size",jint (mem_size (DisplayException.last_completion_result))];
						jobject ["name",jstring "Lexer file cache";"size",jint (mem_size (Lexer.all_files))];
						jobject ["name",jstring "GC heap words";"size",jint (int_of_float size)];
					];
				]
			]
		| MContext sign ->
			let cc = cs#get_context sign in
			let all_modules = List.fold_left (fun acc m -> PMap.add m.m_id m acc) PMap.empty cs#get_modules in
			let l = Hashtbl.fold (fun _ m acc ->
				(m,(get_module_memory cs all_modules m)) :: acc
			) cc#get_modules [] in
			let l = List.sort (fun (_,(size1,_)) (_,(size2,_)) -> compare size2 size1) l in
			let leaks = ref [] in
			let l = List.map (fun (m,(size,(reached,_,_,mleaks))) ->
				if reached then leaks := (m,mleaks) :: !leaks;
				jobject [
					"path",jstring (s_type_path m.m_path);
					"size",jint size;
				]
			) l in
			let leaks = match !leaks with
				| [] -> jnull
				| leaks ->
					let jleaks = List.map (fun (m,leaks) ->
						let jleaks = List.map (fun s -> jobject ["path",jstring s]) leaks in
						jobject [
							"path",jstring (s_type_path m.m_path);
							"leaks",jarray jleaks;
						]
					) leaks in
					jarray jleaks
			in
			let cache_mem = cc#get_pointers in
			jobject [
				"leaks",leaks;
				"syntaxCache",jobject [
					"size",jint (mem_size cache_mem.(0));
				];
				"moduleCache",jobject [
					"size",jint (mem_size cache_mem.(1));
					"list",jarray l;
				];
			]
		| MModule(sign,path) ->
			let cc = cs#get_context sign in
			let m = cc#find_module path in
			let all_modules = List.fold_left (fun acc m -> PMap.add m.m_id m acc) PMap.empty cs#get_modules in
			let _,(_,deps,out,_) = get_module_memory cs all_modules m in
			let deps = update_module_type_deps deps m in
			let out = get_out out in
			let types = List.map (fun md ->
				let fields,inf = match md with
					| TClassDecl c ->
						let own_deps = ref deps in
						let field acc cf =
							let repr = Obj.repr cf in
							own_deps := List.filter (fun repr' -> repr != repr') !own_deps;
							let deps = List.filter (fun repr' -> repr' != repr) deps in
							let size = Objsize.size_with_headers (Objsize.objsize cf deps out) in
							(cf,size) :: acc
						in
						let fields = List.fold_left field [] c.cl_ordered_fields in
						let fields = List.fold_left field fields c.cl_ordered_statics in
						let fields = List.sort (fun (_,size1) (_,size2) -> compare size2 size1) fields in
						let fields = List.map (fun (cf,size) ->
							jobject [
								"name",jstring cf.cf_name;
								"size",jint size;
								"pos",generate_pos_as_location cf.cf_name_pos;
							]
						) fields in
						let repr = Obj.repr c in
						let deps = List.filter (fun repr' -> repr' != repr) !own_deps in
						fields,Objsize.objsize c deps out
					| TEnumDecl en ->
						let repr = Obj.repr en in
						let deps = List.filter (fun repr' -> repr' != repr) deps in
						[],Objsize.objsize en deps out
					| TTypeDecl td ->
						let repr = Obj.repr td in
						let deps = List.filter (fun repr' -> repr' != repr) deps in
						[],Objsize.objsize td deps out
					| TAbstractDecl a ->
						let repr = Obj.repr a in
						let deps = List.filter (fun repr' -> repr' != repr) deps in
						[],Objsize.objsize a deps out
				in
				let size = Objsize.size_with_headers inf in
				let jo = jobject [
					"name",jstring (s_type_path (t_infos md).mt_path);
					"size",jint size;
					"pos",generate_pos_as_location (t_infos md).mt_name_pos;
					"fields",jarray fields;
				] in
				size,jo
			) m.m_types in
			let types = List.sort (fun (size1,_) (size2,_) -> compare size2 size1) types in
			let types = List.map snd types in
			jobject [
				"moduleExtra",jint (Objsize.size_with_headers (Objsize.objsize m.m_extra deps out));
				"types",jarray types;
			]
		end

	let display_memory com =
		()
		(* let verbose = com.verbose in
		let print = print_endline in
		Gc.full_major();
		Gc.compact();
		let mem = Gc.stat() in
		print ("Total Allocated Memory " ^ fmt_size (mem.Gc.heap_words * (Sys.word_size asr 8)));
		print ("Free Memory " ^ fmt_size (mem.Gc.free_words * (Sys.word_size asr 8)));
		(match get() with
		| None ->
			print "No cache found";
		| Some c ->
			print ("Total cache size " ^ size c);
			print ("  haxelib " ^ size c.c_haxelib);
			(* print ("  parsed ast " ^ size c.c_files ^ " (" ^ string_of_int (Hashtbl.length c.c_files) ^ " files stored)"); *)
			(* print ("  typed modules " ^ size c.c_modules ^ " (" ^ string_of_int (Hashtbl.length c.c_modules) ^ " modules stored)"); *)
			let modules = collect_memory_stats c in
			let cur_key = ref "" and tcount = ref 0 and mcount = ref 0 in
			List.iter (fun (m,size,(reached,deps,out,leaks)) ->
				let key = m.m_extra.m_sign in
				if key <> !cur_key then begin
					print (Printf.sprintf ("    --- CONFIG %s ----------------------------") (Digest.to_hex key));
					cur_key := key;
				end;
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
					List.iter leak leaks;
				with Exit ->
					());
				if verbose then begin
					print (Printf.sprintf "      %d total deps" (List.length deps));
					PMap.iter (fun _ md ->
						print (Printf.sprintf "      dep %s%s" (s_type_path md.m_path) (module_sign key md));
					) m.m_extra.m_deps;
				end;
				flush stdout
			) (List.sort (fun (m1,s1,_) (m2,s2,_) ->
				let k1 = m1.m_extra.m_sign and k2 = m2.m_extra.m_sign in
				if k1 = k2 then s1 - s2 else if k1 > k2 then 1 else -1
			) modules);
			if !mcount > 0 then print ("*** " ^ string_of_int !mcount ^ " modules have leaks !");
			print "Cache dump complete") *)
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
	if len > 0 then
		match v.[0], v.[len - 1] with
			| '"', '"'
			| '\'', '\'' -> String.sub v 1 (len - 2)
			| _ -> v
	else v

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

type display_path_kind =
	| DPKNormal of path
	| DPKMacro of path
	| DPKNone

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
			DPKNone
		| dfp ->
			if dfp = DFPOnly then begin
				classes := [];
				com.main_class <- None;
			end;
			let real = Path.get_real_path (DisplayPosition.display_position#get).pfile in
			let path = match get_module_path_from_file_path com real with
			| Some path ->
				if com.display.dms_kind = DMPackage then raise_package (fst path);
				let path = match ExtString.String.nsplit (snd path) "." with
					| [name;"macro"] ->
						(* If we have a .macro.hx path, don't add the file to classes because the compiler won't find it.
						   This can happen if we're completing in such a file. *)
						DPKMacro (fst path,name)
					| [name] ->
						classes := path :: !classes;
						DPKNormal path
					| _ ->
						assert false
				in
				path
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

let promote_type_hints tctx =
	let rec explore_type_hint (md,p,t) =
		match t with
		| TMono r -> (match !r with None -> () | Some t -> explore_type_hint (md,p,t))
		| TLazy f -> explore_type_hint (md,p,lazy_type f)
		| TInst(({cl_name_pos = pn;cl_path = (_,name)}),_)
		| TEnum(({e_name_pos = pn;e_path = (_,name)}),_)
		| TType(({t_name_pos = pn;t_path = (_,name)}),_)
		| TAbstract(({a_name_pos = pn;a_path = (_,name)}),_) ->
			md.m_type_hints <- (p,pn) :: md.m_type_hints;
		| TDynamic _ -> ()
		| TFun _ | TAnon _ -> ()
	in
	List.iter explore_type_hint tctx.g.type_hints

let process_global_display_mode com tctx =
	promote_type_hints tctx;
	match com.display.dms_kind with
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
				let l = cs#get_context_files ((Define.get_signature com.defines) :: (match com.get_macros() with None -> [] | Some com -> [Define.get_signature com.defines])) in
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

let handle_syntax_completion com kind subj =
	let open Parser in
	let l,kind = match kind with
		| SCClassRelation ->
			[Extends;Implements],CRTypeRelation
		| SCInterfaceRelation ->
			[Extends],CRTypeRelation
		| SCComment ->
			[],CRTypeRelation
		| SCTypeDecl mode ->
			let in_import_hx = Filename.basename subj.s_insert_pos.pfile = "import.hx" in
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
		| Some api ->
			let ctx = Genjson.create_context ~jsonrpc:api.jsonrpc GMFull in
			api.send_result(fields_to_json ctx l kind subj)