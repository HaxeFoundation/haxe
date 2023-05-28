open Globals
open Common
open Type
open Genjson

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
	if (Objsize.objsize m deps [Obj.repr Typecore.memory_marker]).Objsize.reached then leak "typecore";
	PMap.iter (fun _ md ->
		if (Objsize.objsize m deps [Obj.repr md]).Objsize.reached then leak (s_type_path md.m_path ^ module_sign m.m_extra.m_sign md);
	) out;
	!leaks

let get_out out =
	Obj.repr Common.memory_marker :: Obj.repr Typecore.memory_marker :: PMap.fold (fun m acc -> Obj.repr m :: acc) out []

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

let fmt_word f =
	fmt_size (int_of_float f * (Sys.word_size / 8))

let size v =
	fmt_size (mem_size v)

let get_memory_json (cs : CompilationCache.t) mreq =
	begin match mreq with
	| MCache ->
		let old_gc = Gc.get() in
		Gc.set { old_gc with
			Gc.max_overhead = 0;
			Gc.space_overhead = 0
		};
		Gc.compact();
		Gc.set old_gc;
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
					jobject ["name",jstring "macro stdlib";"size",jint (mem_size (EvalContext.GlobalState.stdlib))];
					jobject ["name",jstring "macro macro_lib";"size",jint (mem_size (EvalContext.GlobalState.macro_lib))];
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
				"hasTypes",jbool (match m.m_extra.m_kind with MCode | MMacro -> true | _ -> false);
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
	let verbose = com.verbose in
	let print = print_endline in
	Gc.full_major();
	Gc.compact();
	let mem = Gc.stat() in
	print ("Total Allocated Memory " ^ fmt_size (mem.Gc.heap_words * (Sys.word_size asr 8)));
	print ("Free Memory " ^ fmt_size (mem.Gc.free_words * (Sys.word_size asr 8)));
	let c = com.cs in
	print ("Total cache size " ^ size c);
	(* print ("  haxelib " ^ size c.c_haxelib); *)
	(* print ("  parsed ast " ^ size c.c_files ^ " (" ^ string_of_int (Hashtbl.length c.c_files) ^ " files stored)"); *)
	(* print ("  typed modules " ^ size c.c_modules ^ " (" ^ string_of_int (Hashtbl.length c.c_modules) ^ " modules stored)"); *)
	let module_list = c#get_modules in
	let all_modules = List.fold_left (fun acc m -> PMap.add m.m_id m acc) PMap.empty module_list in
	let modules = List.fold_left (fun acc m ->
		let (size,r) = get_module_memory c all_modules m in
		(m,size,r) :: acc
	) [] module_list in
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
	print "Cache dump complete"
