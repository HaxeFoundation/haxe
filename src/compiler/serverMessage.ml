open Globals
open Common
open CompilationCache
open Type
open Json

type server_message_options = {
	mutable print_added_directory : bool;
	mutable print_found_directories : bool;
	mutable print_changed_directories : bool;
	mutable print_module_path_changed : bool;
	mutable print_not_cached : bool;
	mutable print_parsed : bool;
	mutable print_removed_directory : bool;
	mutable print_reusing : bool;
	mutable print_skipping_dep : bool;
	mutable print_unchanged_content : bool;
	mutable print_cached_modules : bool;
	mutable print_class_paths_changed : bool;
	mutable print_arguments : bool;
	mutable print_completion : bool;
	mutable print_defines : bool;
	mutable print_signature : bool;
	mutable print_display_position : bool;
	mutable print_stats : bool;
	mutable print_message : bool;
	mutable print_socket_message : bool;
	mutable print_uncaught_error : bool;
	mutable print_new_context : bool;
}

let config = {
	print_added_directory = false;
	print_found_directories = false;
	print_changed_directories = false;
	print_module_path_changed = false;
	print_not_cached = false;
	print_parsed = false;
	print_removed_directory = false;
	print_reusing = false;
	print_skipping_dep = false;
	print_unchanged_content = false;
	print_cached_modules = false;
	print_class_paths_changed = false;
	print_arguments = false;
	print_completion = false;
	print_defines = false;
	print_signature = false;
	print_display_position = false;
	print_stats = false;
	print_message = false;
	print_socket_message = false;
	print_uncaught_error = false;
	print_new_context = false;
}

let sign_string com =
	let sign = Define.get_signature com.defines in
	let cs = com.cs in
	let	sign_id = (cs#get_context sign)#get_index in
	Printf.sprintf "%2i,%3s: " sign_id (short_platform_name com.platform)

let added_directory com tabs dir =
	if config.print_added_directory then print_endline (Printf.sprintf "%sadded directory %s" (sign_string com) dir)

let found_directories com tabs dirs =
	if config.print_found_directories then print_endline (Printf.sprintf "%sfound %i directories" (sign_string com) (List.length dirs))

let changed_directories com tabs dirs =
	if config.print_changed_directories then print_endline (Printf.sprintf "%schanged directories: [%s]" (sign_string com) (String.concat ", " (List.map (fun dir -> "\"" ^ dir.c_path ^ "\"") dirs)))

let module_path_changed com tabs (m,time,file) =
	if config.print_module_path_changed then print_endline (Printf.sprintf "%smodule path might have changed: %s\n\twas: %2.0f %s\n\tnow: %2.0f %s"
		(sign_string com) (s_type_path m.m_path) m.m_extra.m_time (Path.UniqueKey.lazy_path m.m_extra.m_file) time file)

let not_cached com tabs m =
	if config.print_not_cached then print_endline (Printf.sprintf "%s%s not cached (%s)" (sign_string com) (s_type_path m.m_path) "modified")

let parsed com tabs (ffile,info) =
	if config.print_parsed then print_endline (Printf.sprintf "%sparsed %s (%s)" (sign_string com) ffile info)

let removed_directory com tabs dir =
	if config.print_removed_directory then print_endline (Printf.sprintf "%sremoved directory %s" (sign_string com) dir)

let reusing com tabs m =
	if config.print_reusing then print_endline (Printf.sprintf "%s%sreusing %s" (sign_string com) tabs (s_type_path m.m_path))

let skipping_dep com tabs (m,reason) =
	if config.print_skipping_dep then print_endline (Printf.sprintf "%sskipping %s (%s)" (sign_string com) (s_type_path m.m_path) reason)

let unchanged_content com tabs file =
	if config.print_unchanged_content then print_endline (Printf.sprintf "%s%s changed time not but content, reusing" (sign_string com) file)

let cached_modules com tabs i =
	if config.print_cached_modules then print_endline (Printf.sprintf "%sCached %i modules" (sign_string com) i)

let class_paths_changed com tabs =
	if config.print_class_paths_changed then print_endline (Printf.sprintf "%sclass paths changed, resetting directories" (sign_string com))

let arguments data =
	if config.print_arguments then print_endline (("Processing Arguments [" ^ String.concat "," data ^ "]"))

let completion str =
	if config.print_completion then print_endline ("Completion Response =\n" ^ str)

let defines com tabs =
	if config.print_defines then begin
		let buffer = Buffer.create 64 in
		Buffer.add_string buffer "Defines ";
		PMap.iter (Printf.bprintf buffer "%s=%s,") com.defines.values;
		Buffer.truncate buffer (Buffer.length buffer - 1);
		print_endline (Buffer.contents buffer)
	end

let signature com tabs sign =
	if config.print_signature then print_endline ("Using signature " ^ Digest.to_hex sign)

let display_position com tabs p =
	if config.print_display_position then print_endline ("Display position: " ^ (Printer.s_pos p))

let stats stats time =
	if config.print_stats then begin
		print_endline (Printf.sprintf "Stats = %d files, %d classes, %d methods, %d macros" !(stats.s_files_parsed) !(stats.s_classes_built) !(stats.s_methods_typed) !(stats.s_macros_called));
		print_endline (Printf.sprintf "Time spent : %.3fs" time)
	end

let message s =
	if config.print_message then print_endline ("> " ^ s)

let gc_stats time stats_before did_compact space_overhead =
	if config.print_stats then begin
		let stats = Gc.quick_stat() in
		print_endline (Printf.sprintf "GC %s done in %.2fs with space_overhead = %i\n\tbefore: %s\n\tafter: %s"
			(if did_compact then "compaction" else "collection")
			time
			space_overhead
			(Memory.fmt_word (float_of_int stats_before.Gc.heap_words))
			(Memory.fmt_word (float_of_int stats.heap_words))
		)
	end

let socket_message s =
	if config.print_socket_message then print_endline s

let uncaught_error s =
	if config.print_uncaught_error then print_endline ("Uncaught Error : " ^ s)

let enable_all () =
	config.print_added_directory <- true;
	config.print_found_directories <- true;
	config.print_changed_directories <- true;
	config.print_module_path_changed <- true;
	config.print_not_cached <- true;
	config.print_parsed <- true;
	config.print_removed_directory <- true;
	config.print_reusing <- true;
	config.print_skipping_dep <- true;
	config.print_unchanged_content <- true;
	config.print_cached_modules <- true;
	config.print_arguments <- true;
	config.print_completion <- true;
	config.print_defines <- true;
	config.print_signature <- true;
	config.print_display_position <- true;
	config.print_stats <- true;
	config.print_message <- true;
	config.print_socket_message <- true;
	config.print_uncaught_error <- true;
	config.print_new_context <- true

let set_by_name name value = match name with
	| "addedDirectory" -> config.print_added_directory <- value
	| "foundDirectories" -> config.print_found_directories <- value;
	| "changedDirectories" -> config.print_changed_directories <- value;
	| "modulePathChanged" -> config.print_module_path_changed <- value;
	| "notCached" -> config.print_not_cached <- value;
	| "parsed" -> config.print_parsed <- value;
	| "removedDirectory" -> config.print_removed_directory <- value;
	| "reusing" -> config.print_reusing <- value;
	| "skippingDep" -> config.print_skipping_dep <- value;
	| "unchangedContent" -> config.print_unchanged_content <- value;
	| "cachedModules" -> config.print_cached_modules <- value;
	| "arguments" -> config.print_arguments <- value;
	| "completion" -> config.print_completion <- value;
	| "defines" -> config.print_defines <- value;
	| "signature" -> config.print_signature <- value;
	| "displayPosition" -> config.print_display_position <- value;
	| "stats" -> config.print_stats <- value;
	| "message" -> config.print_message <- value;
	| "socketMessage" -> config.print_socket_message <- value;
	| "uncaughtError" -> config.print_uncaught_error <- value;
	| "newContext" -> config.print_new_context <- value;
	| _ -> raise Not_found
