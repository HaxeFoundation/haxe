open Globals
open Common

type target =
	| Scopes of ClassPath.class_path_scope list
	| Path of string

let parse_targets value =
	let entries = List.filter (fun s -> s <> "") (List.map String.trim (ExtString.String.nsplit value ",")) in
	List.map (function
		| "1" | "user" -> Scopes [ClassPath.User]
		| "lib" -> Scopes [ClassPath.Lib]
		| "std" -> Scopes [ClassPath.Std;ClassPath.StdTarget]
		| "all" -> Scopes [ClassPath.User;ClassPath.Lib;ClassPath.Std;ClassPath.StdTarget]
		| path ->
			let full = Path.get_full_path path in
			Path (if Sys.file_exists full && Sys.is_directory full then Path.add_trailing_slash full else full)
	) entries

let scope_of com file =
	let owner = ref None in
	com.class_paths#iter (fun cp ->
		let dir = Path.add_trailing_slash (Path.get_full_path cp#path) in
		if ExtString.String.starts_with file dir then match !owner with
			| Some (d,_) when String.length d >= String.length dir -> ()
			| _ -> owner := Some (dir,cp#scope)
	);
	Option.map snd !owner

let is_target com targets file =
	let file = Path.get_full_path file in
	let scope = scope_of com file in
	List.exists (function
		| Scopes scopes -> (match scope with Some scope -> List.mem scope scopes | None -> false)
		| Path path -> file = path || ExtString.String.starts_with file path
	) targets

let apply_to_file file fixes =
	let content = Std.input_file ~bin:true file in
	let sorted = List.sort (fun a b ->
		let c = compare a.fix_range.pmin b.fix_range.pmin in
		if c <> 0 then c else compare b.fix_range.pmax a.fix_range.pmax
	) fixes in
	let applied,skipped,_ = List.fold_left (fun (applied,skipped,last) f ->
		let bmin,bmax = SourceText.byte_range content f.fix_range in
		if bmin < last || bmax > String.length content then (applied,skipped + 1,last)
		else ((bmin,bmax,f.fix_text) :: applied,skipped,bmax)
	) ([],0,0) sorted in
	let b = Buffer.create (String.length content) in
	let last = List.fold_left (fun pos (bmin,bmax,text) ->
		Buffer.add_substring b content pos (bmin - pos);
		Buffer.add_string b text;
		bmax
	) 0 (List.rev applied) in
	Buffer.add_substring b content last (String.length content - last);
	let ch = open_out_bin file in
	output_string ch (Buffer.contents b);
	close_out ch;
	List.length applied,skipped

let run com =
	let targets = parse_targets (Common.defined_value_safe com Define.ApplyFixes) in
	let by_file = Hashtbl.create 0 in
	List.iter (fun f ->
		let file = f.fix_range.pfile in
		if is_target com targets file then
			Hashtbl.replace by_file file (f :: (try Hashtbl.find by_file file with Not_found -> []))
	) com.part_scope.fixes;
	let files = Hashtbl.fold (fun file fixes acc -> (file,fixes) :: acc) by_file [] in
	let files = List.sort compare files in
	let total,skipped = List.fold_left (fun (total,skipped) (file,fixes) ->
		let n,s = apply_to_file file fixes in
		if n > 0 then com.info (Printf.sprintf "Applied %d fix%s" n (if n = 1 then "" else "es")) { pfile = file; pmin = 0; pmax = 0 };
		(total + n,skipped + s)
	) (0,0) files in
	if skipped > 0 then
		com.info (Printf.sprintf "%d overlapping fix%s skipped, compile again to apply" skipped (if skipped = 1 then " was" else "es were")) null_pos;
	ignore total
