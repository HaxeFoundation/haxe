open Globals
open Type

(**
	Check if all items of the `needle` list exist in the same order in the beginning of the `haystack` list.
*)
let rec list_starts_with_list (haystack:string list) (needle:string list) =
	match haystack, needle with
		| _, [] -> true
		| [], _ -> false
		| current_haystack :: rest_haystack, current_needle :: rest_needle ->
			current_haystack = current_needle && list_starts_with_list rest_haystack rest_needle


class null_safety =
	object (self)
		(** Packages/classes added via `haxe.macro.Compiler.nulLSafety(dotPath)` *)
		val mutable paths = []
		(**
			Add a package or a class to the safe area.
		*)
		method add_path (dot_path:string) =
			let path = String.split_on_char '.' dot_path in
			if not (self#is_safe_path path) then
				paths <- path :: paths
		(**
			Run null safety checks
		*)
		method run (typer:Typecore.typer) (types:module_type list) =
			match paths with
				| [] -> ()
				| _ ->
					let timer = Timer.timer ["filters"; "null safety"] in
					print_endline "Null Safety!";
					print_endline (String.concat "; " (List.map (String.concat ".") paths));
					timer()
		(**
			Check if a type with the specified `type_path` is located in a "safe" package or module.
		*)
		method is_in_safety type_path =
			let path = fst type_path @ [snd type_path] in
			self#is_safe_path path
		(**
			Check if null safety is applied to types in this `path`
		*)
		method is_safe_path (path:string list) =
			List.exists (fun p -> list_starts_with_list p path) paths
	end
;;
