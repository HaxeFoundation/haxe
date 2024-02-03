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
				let p = { pfile = file ^ " @ " ^ Globals.s_type_path path; pmin = 0; pmax = 0; } in
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

let rec cache_context cs com =
	let cc = get_cache com in
	let sign = Define.get_signature com.defines in
	let anon_identification = new Tanon_identification.tanon_identification in
	let config = match com.hxb_writer_config with
		| None ->
			HxbWriterConfig.create_target_config ()
		| Some config ->
			if com.is_macro_context then config.macro_config else config.target_config
	in
	let cache_module m =
		(* If we have a signature mismatch, look-up cache for module. Physical equality check is fine as a heueristic. *)
		let cc = if m.m_extra.m_sign = sign then cc else cs#get_context m.m_extra.m_sign in
		let warn w s p = com.warning w com.warning_options s p in
		cc#cache_module config warn anon_identification com.hxb_writer_stats m.m_path m;
	in
	List.iter cache_module com.modules;
	begin match com.get_macros() with
		| None -> ()
		| Some com -> cache_context cs com
	end;
	if Define.raw_defined com.defines "hxb.stats" then begin
		HxbReader.dump_stats (platform_name com.platform) com.hxb_reader_stats;
		HxbWriter.dump_stats (platform_name com.platform) com.hxb_writer_stats
	end

let maybe_add_context_sign cs com desc =
	let sign = Define.get_signature com.defines in
	ignore(cs#add_info sign desc com.platform com.class_paths com.defines)

let lock_signature com name =
	let cs = com.cs in
	maybe_add_context_sign cs com name;
	com.cache <- Some (get_cache com)
