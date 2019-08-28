open Globals
open Common
open CompilationServer
open Type

let handle_native_lib com lib =
	com.native_libs.all_libs <- lib#get_file_path :: com.native_libs.all_libs;
	com.load_extern_type <- com.load_extern_type @ [lib#get_file_path,lib#build];
	match get() with
	| Some cs when not (Define.raw_defined com.defines "haxe.noNativeLibsCache") ->
		let init () =
			let file = lib#get_file_path in
			let key = file in
			let ftime = file_time file in
			begin match cs#get_native_lib key with
			| Some lib when ftime <= lib.c_nl_mtime ->
				(* Cached lib is good, set up lookup into cached files. *)
				lib.c_nl_files;
			| _ ->
				(* Cached lib is outdated or doesn't exist yet, read library. *)
				lib#load;
				(* Created lookup and eagerly read each known type. *)
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
				(* Save and set up lookup. *)
				cs#add_native_lib key h ftime;
				h;
			end;
		in
		(fun () ->
			let lut = init() in
			let build path p =
				try Some (Hashtbl.find lut path)
				with Not_found -> None
			in
			com.load_extern_type <- List.map (fun (name,f) ->
				name,if name = lib#get_file_path then build else f
			) com.load_extern_type
		)
	| _ ->
		(* Offline mode, just read library as usual. *)
		(fun () -> lib#load)

(* context *)

let get_cache cs com = match com.Common.cache with
	| None ->
		let sign = Define.get_signature com.defines in
		cs#get_context sign
	| Some cache ->
		cache

let rec cache_context cs com =
	let cc = get_cache cs com in
	let sign = Define.get_signature com.defines in
	let cache_module m =
		(* If we have a signature mismatch, look-up cache for module. Physical equality check is fine as a heueristic. *)
		let cc = if m.m_extra.m_sign == sign then cc else cs#get_context m.m_extra.m_sign in
		cc#cache_module m.m_path m;
	in
	List.iter cache_module com.modules;
	match com.get_macros() with
	| None -> ()
	| Some com -> cache_context cs com

let maybe_add_context_sign cs com desc =
	let sign = Define.get_signature com.defines in
	ignore(cs#add_info sign desc com.platform com.class_path com.defines)

let lock_signature com name = match CompilationServer.get() with
	| Some cs ->
		maybe_add_context_sign cs com name;
		com.cache <- Some (get_cache cs com)
	| None ->
		()