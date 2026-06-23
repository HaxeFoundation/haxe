open Globals
open Common
open Type

class lib_build_task cs file ftime lib = object(self)
	inherit CompilationCache.server_task ["build_lib";lib#get_name] 60

	method private execute =
		(* Created lookup and eagerly read each known type. *)
		lib#load;
		let h = Hashtbl.create 0 in
		List.iter (fun path ->
			if not (Hashtbl.mem h path) then begin
				let p = file_pos (file ^ " @ " ^ Globals.s_type_path path) in
				try begin match lib#build path p with
				| Some r -> Hashtbl.add h path r
				| None -> ()
				end with _ ->
					()
			end
		) lib#list_modules;
		lib#close;
		(* Save and set up lookup. *)
		cs#add_native_lib file h ftime;
end

let handle_native_lib com lib =
	com.native_libs.all_libs <- lib#get_file_path :: com.native_libs.all_libs;
	let build path =
		(* The first build has to load, afterwards we install a direct lib#build call. *)
		lib#load;
		com.load_extern_type <- List.map (fun (name,f) ->
			name,if name = lib#get_file_path then lib#build else f
		) com.load_extern_type;
		lib#build path;
	in
	com.load_extern_type <- com.load_extern_type @ [lib#get_file_path,build];
	if not (Define.raw_defined com.defines "haxe.noNativeLibsCache") then begin
		let cs = com.cs in
		let init () =
			let file = lib#get_file_path in
			let ftime = file_time file in
			begin match cs#get_native_lib file with
			| Some lib when ftime <= lib.c_nl_mtime ->
				(* Cached lib is good, set up lookup into cached files. *)
				Some lib.c_nl_files;
			| _ ->
				(* Cached lib is outdated or doesn't exist yet, register build task. *)
				cs#add_task (new lib_build_task cs file ftime lib);
				None
			end;
		in
		(fun () ->
			let lut = init() in
			match lut with
			| Some lut ->
				let build path p =
					try Some (Hashtbl.find lut path)
					with Not_found -> None
				in
				com.load_extern_type <- List.map (fun (name,f) ->
					name,if name = lib#get_file_path then build else f
				) com.load_extern_type
			| None ->
				()
		)
	end else
		(* Offline mode, just read library as usual. *)
		(fun () -> lib#load)

(* context *)

let get_cache com = match com.Common.cache with
	| None ->
		let sign = Define.get_signature com.defines in
		com.cs#get_context sign
	| Some cache ->
		cache

let get_cache_sign com = match com.Common.cache with
	| None -> Define.get_signature com.defines
	| Some cache -> cache#get_sign

(* Diagnostic for `-D hxb.detect_unexpected_mutations`: compares the freshly re-serialized chunks of a
   module the unchanged-skip heuristic would skip against the cached ones, returning a description of any
   *unexpected* differences. EXD (expression data) diffs are ignored: re-reading reassigns var/anon ids,
   so EXD round-trips are not byte-identical even when nothing changed. This means genuine expression-body
   mutations to a reused module would not be detected, but those don't happen (cached modules aren't re-typed). *)
let unexpected_chunk_diffs (cached : HxbData.cached_chunks) (fresh : HxbData.cached_chunks) =
	if List.length cached <> List.length fresh then
		Some (Printf.sprintf "chunk count %d vs %d" (List.length cached) (List.length fresh))
	else
		let diffs = List.filter_map (fun ((ka,ba),(kb,bb)) ->
			if ka = HxbData.EXD then None
			else if ka <> kb then Some (Printf.sprintf "%s<>%s(kind)" (HxbData.string_of_chunk_kind ka) (HxbData.string_of_chunk_kind kb))
			else if ba <> bb then Some (Printf.sprintf "%s(%d/%d)" (HxbData.string_of_chunk_kind ka) (Bytes.length ba) (Bytes.length bb))
			else None
		) (List.combine cached fresh) in
		match diffs with [] -> None | _ -> Some (String.concat "," diffs)

let rec cache_context cs com =
	let cc = get_cache com in
	let sign = Define.get_signature com.defines in

	let detect_mutations = Define.defined com.defines HxbDetectUnexpectedMutations in
	(* `-D hxb.measure_signatures`: behaviour-neutral. For each re-typed module, diff its new header
	   signature against the cached one to gauge how many re-types were header-unchanged (body-only),
	   i.e. how many dependents were invalidated needlessly. Measurement only; no sparing yet. *)
	let measure_sigs = Define.raw_defined com.defines "hxb.measure_signatures" in
	let sig_unchanged = ref 0 and sig_changed = ref 0 and sig_no_baseline = ref 0 in
	let parallels = DynArray.create () in
	(* Modules the unchanged-skip heuristic would skip, to be re-serialized and verified (in parallel,
	   like the real write path) only when `-D hxb.detect_unexpected_mutations` is set. *)
	let detect_parallels = DynArray.create () in
	let cache_module m =
		if Define.defined com.defines DisableHxbCache then
			(* If we have a signature mismatch, look-up cache for module. Physical equality check is fine as a heuristic. *)
			let cc = if m.m_extra.m_sign = sign then cc else cs#get_context m.m_extra.m_sign in
			cc#cache_module_in_memory m.m_path m;
		else begin
			(* If we have a signature mismatch, look-up cache for module. Physical equality check is fine as a heuristic. *)
			let cc = if m.m_extra.m_sign = sign then cc else cs#get_context m.m_extra.m_sign in
			let make_writer warn =
				let anon_identification = new Tanon_identification.tanon_identification in
				let config = match com.hxb_writer_config with
					| None ->
						HxbWriterConfig.create_target_config ()
					| Some config ->
						if com.is_macro_context then config.macro_config else config.target_config
				in
				cc#cache_hxb_module config warn anon_identification m
			in
			let warn w s p = com.warning w com.warning_options s p in
			(* A module that wasn't (re)typed this round (its m_processed is from an earlier compilation
			   step) serializes to chunks identical to what's already cached, so we can skip writing it
			   entirely as long as a good binary cache entry for it already exists. This avoids
			   re-serializing the whole module graph on incremental compiles and diagnostics. *)
			let unchanged =
				m.m_extra.m_processed <> 0
				&& m.m_extra.m_processed < com.part_scope.compilation_step
				&& not m.m_extra.m_cache_dirty
				&& cc#has_good_hxb_module m.m_path m.m_id
			in
			if unchanged then begin
				if detect_mutations then begin
					(* Suppress writer warnings here: this module is being re-serialized only to verify the
					   skip, so its writer warnings are not "real" and would just be noise. *)
					match make_writer (fun _ _ _ -> ()) with
					| None ->
						()
					| Some f ->
						DynArray.add detect_parallels (cc,m,f)
				end
			end else begin
				(* Re-typed module: compute its header signature now (pre-DCE — full public surface,
				   cf_expr_unoptimized present) so it is carried in mc_extra for the next round. *)
				ModuleSignature.compute_and_store m;
				if measure_sigs then begin
					(* The cache still holds the previous round's entry (not yet replaced), so its
					   mc_extra carries the old signature. *)
					let old_sig = try (cc#get_hxb_module m.m_path).HxbData.mc_extra.m_sig with Not_found -> None in
					(match old_sig, m.m_extra.m_sig with
					| Some old, Some nw -> if ModuleSignature.diff old nw = [] then incr sig_unchanged else incr sig_changed
					| _ -> incr sig_no_baseline)
				end;
				match make_writer warn with
				| None ->
					()
				| Some f ->
					DynArray.add parallels (cc,m,f)
			end
		end
	in
	List.iter cache_module com.modules;
	let a = Parallel.run_with_pool com.sctx.pool (fun pool ->
		Parallel.ParallelArray.map pool (fun (cc,m,f) ->
			let chunks = f() in
			(cc,m,chunks)
		) (DynArray.to_array parallels) (cc,null_module,[])
	) in
	Array.iter (fun (cc,m,chunks) ->
		cc#add_binary_cache m chunks
	) a;
	if detect_mutations && DynArray.length detect_parallels > 0 then
		(* Re-serialize the skipped modules in parallel (like the write path), diff against the cache,
		   then emit warnings sequentially (com.warning is not thread-safe). Timed so its cost shows up. *)
		Timer.time com.timer_ctx ["server";"cache context";"detect mutations"] (fun () ->
			let results = Parallel.run_with_pool com.sctx.pool (fun pool ->
				Parallel.ParallelArray.map pool (fun (cc,m,f) ->
					let fresh = f () in
					let cached = (cc#get_hxb_module m.m_path).HxbData.mc_chunks in
					(m,unexpected_chunk_diffs cached fresh)
				) (DynArray.to_array detect_parallels) (null_module,None)
			) in
			Array.iter (fun (m,diff) -> match diff with
				| Some desc ->
					let p = file_pos (Path.UniqueKey.lazy_path m.m_extra.m_file) in
					com.warning WHxbUnexpectedMutation com.warning_options
						(Printf.sprintf "Module %s was mutated without being re-typed; its cached hxb form would be stale (%s)" (s_type_path m.m_path) desc) p
				| None ->
					()
			) results
		) ();
	if measure_sigs && (!sig_unchanged + !sig_changed + !sig_no_baseline) > 0 then begin
		let retyped = !sig_unchanged + !sig_changed in
		let pct = if retyped > 0 then 100. *. float !sig_unchanged /. float retyped else 0. in
		Printf.printf "[measure-signatures] re-typed with baseline: %d | header unchanged %d (%.1f%%, dependents needlessly invalidated) | header changed %d | no baseline %d\n%!"
			retyped !sig_unchanged pct !sig_changed !sig_no_baseline
	end;
	let written = ref (Array.length a) in
	begin match com.get_macros() with
		| None -> ()
		| Some macro_com ->
			cc#add_child (get_cache_sign macro_com);
			written := !written + cache_context cs macro_com
	end;
	if Define.defined com.defines HxbStats then
		HxbReader.dump_stats (platform_name com.platform) com.hxb_reader_stats;
	!written

let maybe_add_context_sign cs com desc =
	let sign = Define.get_signature com.defines in
	ignore(cs#add_info sign desc com.platform com.class_paths com.defines)

let lock_signature com name =
	let cs = com.cs in
	maybe_add_context_sign cs com name;
	com.cache <- Some (get_cache com)

let maybe_cache_context com =
	if com.display.dms_full_typing && com.display.dms_populate_cache then begin
		let written = Timer.time com.timer_ctx ["server";"cache context"] (cache_context com.cs) com in
		ServerMessage.cached_modules com "" (List.length com.modules) written;
	end
