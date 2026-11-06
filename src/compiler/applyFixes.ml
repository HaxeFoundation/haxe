open Globals
open Common

let split_list value =
	List.filter (fun s -> s <> "") (List.map String.trim (ExtString.String.nsplit value ","))

let parse_targets value =
	List.concat_map (function
		| "1" -> [SourceScope.User]
		| "all" -> [SourceScope.User;SourceScope.Lib None;SourceScope.Std]
		| s -> try [SourceScope.parse s] with SourceScope.Unknown_kind -> [SourceScope.Path s]
	) (split_list value)

let is_target com targets file =
	List.exists (SourceScope.matcher com.class_paths file) targets

let edits_of content fixes =
	let sorted = List.sort (fun a b ->
		let c = compare a.fix_range.pmin b.fix_range.pmin in
		if c <> 0 then c else compare b.fix_range.pmax a.fix_range.pmax
	) fixes in
	let applied,skipped,_ = List.fold_left (fun (applied,skipped,last) f ->
		let bmin,bmax = SourceText.byte_range content f.fix_range in
		if bmin < last || bmax > String.length content then (applied,skipped + 1,last)
		else ((bmin,bmax,(List.hd f.fix_choices).fc_text) :: applied,skipped,bmax)
	) ([],0,0) sorted in
	List.rev applied,skipped

let identifier_offsets content name =
	let is_ident c = match c with 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false in
	let len = String.length content and n = String.length name in
	let rec loop acc i =
		if i + n > len then List.rev acc
		else if content.[i] = name.[0] && String.sub content i n = name && (i = 0 || not (is_ident content.[i - 1])) && (i + n = len || not (is_ident content.[i + n])) then loop (i :: acc) (i + n)
		else loop acc (i + 1)
	in
	loop [] 0

(* A guarded group applies only if its anchor (the fix without fallback) is there and its
   names only appear in the file where fixes replace them; otherwise its fixes use their fallbacks. *)
let resolve_guards content fixes =
	let groups = Hashtbl.create 0 in
	List.iter (fun f -> match f.fix_guard with
		| Some g -> Hashtbl.replace groups g.fg_group (f :: (try Hashtbl.find groups g.fg_group with Not_found -> []))
		| None -> ()
	) fixes;
	let ranges = List.map (fun f -> SourceText.byte_range content f.fix_range) fixes in
	let holds = Hashtbl.create 0 in
	let check group =
		let members = Hashtbl.find groups group in
		let guard f = Option.get f.fix_guard in
		List.exists (fun f -> (guard f).fg_fallback = None) members &&

		List.for_all (fun name ->
			List.for_all (fun b -> List.exists (fun (bmin,bmax) -> b >= bmin && b < bmax) ranges) (identifier_offsets content name)
		) (guard (List.hd members)).fg_names
	in
	let holds group = try Hashtbl.find holds group with Not_found -> let b = check group in Hashtbl.add holds group b; b in
	List.filter_map (fun f -> match f.fix_guard with
		| None -> Some f
		| Some g when holds g.fg_group -> Some f
		| Some { fg_fallback = Some text } -> Some { f with fix_choices = [choice text] }
		| Some { fg_fallback = None } -> None
	) fixes

let apply_edits content offset edits =
	let b = Buffer.create (String.length content) in
	let last = List.fold_left (fun pos (bmin,bmax,text) ->
		Buffer.add_substring b content pos (bmin - offset - pos);
		Buffer.add_string b text;
		bmax - offset
	) 0 edits in
	Buffer.add_substring b content last (String.length content - last);
	Buffer.contents b

let context_lines = 3

let unified_diff file content edits =
	let starts =
		let l = ref [0] in
		String.iteri (fun i c -> if c = '\n' && i + 1 < String.length content then l := (i + 1) :: !l) content;
		Array.of_list (List.rev !l)
	in
	let line_count = Array.length starts in
	let no_final_newline = content = "" || content.[String.length content - 1] <> '\n' in
	let no_newline_marker i = if no_final_newline && i = line_count - 1 then "\\ No newline at end of file\n" else "" in
	let line_end i = if i + 1 < line_count then starts.(i + 1) - 1 else String.length content - (if content <> "" && content.[String.length content - 1] = '\n' then 1 else 0) in
	let line_text i = String.sub content starts.(i) (line_end i - starts.(i)) in
	let line_of b =
		let rec search lo hi =
			if lo >= hi then lo
			else let mid = (lo + hi + 1) / 2 in if starts.(mid) <= b then search mid hi else search lo (mid - 1)
		in
		search 0 (line_count - 1)
	in
	let blocks = List.fold_left (fun acc ((bmin,bmax,_) as edit) ->
		let first = line_of bmin and last = line_of (max bmin (bmax - 1)) in
		match acc with
		| (bfirst,blast,edits) :: acc when first <= blast + 1 -> (bfirst,max blast last,edits @ [edit]) :: acc
		| _ -> (first,last,[edit]) :: acc
	) [] edits in
	let blocks = List.rev_map (fun (first,last,edits) ->
		let old_text = String.sub content starts.(first) (line_end last - starts.(first)) in
		(first,last,String.split_on_char '\n' (apply_edits old_text starts.(first) edits))
	) blocks in
	let rec group acc current blocks = match current,blocks with
		| [],b :: blocks -> group acc [b] blocks
		| ((_,last,_) :: _),(((first,_,_) as b) :: blocks) when first - last - 1 <= 2 * context_lines -> group acc (b :: current) blocks
		| _,b :: blocks -> group (List.rev current :: acc) [b] blocks
		| [],[] -> List.rev acc
		| _,[] -> List.rev (List.rev current :: acc)
	in
	let b = Buffer.create 0 in
	Buffer.add_string b (Printf.sprintf "--- %s\n+++ %s\n" file file);
	ignore (List.fold_left (fun delta hunk ->
		let (hfirst,_,_) = List.hd hunk and (_,hlast,_) = List.hd (List.rev hunk) in
		let from = max 0 (hfirst - context_lines) and upto = min (line_count - 1) (hlast + context_lines) in
		let lines = Buffer.create 0 in
		let rec emit i blocks new_count =
			if i > upto then new_count
			else match blocks with
				| (first,last,new_lines) :: blocks when first = i ->
					for j = first to last do Buffer.add_string lines ("-" ^ line_text j ^ "\n" ^ no_newline_marker j) done;
					List.iter (fun l -> Buffer.add_string lines ("+" ^ l ^ "\n")) new_lines;
					Buffer.add_string lines (no_newline_marker last);
					emit (last + 1) blocks (new_count + List.length new_lines)
				| _ ->
					Buffer.add_string lines (" " ^ line_text i ^ "\n" ^ no_newline_marker i);
					emit (i + 1) blocks (new_count + 1)
		in
		let new_count = emit from hunk 0 in
		let old_count = upto - from + 1 in
		Buffer.add_string b (Printf.sprintf "@@ -%d,%d +%d,%d @@\n" (from + 1) old_count (from + 1 + delta) new_count);
		Buffer.add_buffer b lines;
		delta + new_count - old_count
	) 0 (group [] [] blocks));
	Buffer.contents b

let is_clean file =
	let dir = Filename.dirname file and base = Filename.basename file in
	try
		let (cin,cout,cerr) as ch = Unix.open_process_args_full "git" [|"git";"-C";dir;"status";"--porcelain";"--ignored";"--";base|] (Unix.environment ()) in
		close_out cout;
		let output = try input_line cin with End_of_file -> "" in
		ignore (Std.input_list cerr);
		match Unix.close_process_full ch with
		| Unix.WEXITED 0 -> output = ""
		| _ -> false
	with _ ->
		false

let parse_version s =
	let s = try String.sub s 0 (String.index s '-') with Not_found -> s in
	List.map (fun n -> try int_of_string n with Failure _ -> 0) (String.split_on_char '.' s)

let rec compare_versions v1 v2 = match v1,v2 with
	| [],[] -> 0
	| [],v2 -> compare_versions [0] v2
	| v1,[] -> compare_versions v1 [0]
	| n1 :: v1,n2 :: v2 -> if n1 <> n2 then compare n1 n2 else compare_versions v1 v2

let run com config =
	let targets = parse_targets config.fix_scope in
	let dry = config.fix_diff in
	let compat = Option.map parse_version config.fix_compat in
	let too_recent = ref [] in
	let unsafe = config.fix_unsafe in
	let skipped_unsafe = ref 0 in
	let since_of c = match c.fc_since with Some since -> parse_version since | None -> [] in
	let choose f =
		let newest_first = List.stable_sort (fun c1 c2 -> compare_versions (since_of c2) (since_of c1)) f.fix_choices in
		let compatible = List.filter (fun c -> match compat,c.fc_since with
			| Some compat,Some since -> compare_versions (parse_version since) compat <= 0
			| _ -> true
		) newest_first in
		match compatible,List.rev newest_first with
		| [],{fc_since = Some since} :: _ ->
			too_recent := since :: !too_recent;
			None
		| [],_ ->
			None
		| _ ->
			match List.find_opt (fun c -> c.fc_safe || unsafe) compatible with
			| Some c -> Some { f with fix_choices = [c] }
			| None -> incr skipped_unsafe; None
	in
	let selected f = match config.fix_only,f.fix_warning with
		| [],_ | _,None -> true
		| l,Some w -> List.exists (Warning.matches w) l
	in
	let by_file = Hashtbl.create 0 in
	List.iter (fun f ->
		let file = f.fix_range.pfile in
		if is_target com targets file && List.for_all (is_target com targets) f.fix_requires && selected f then
			Option.may (fun f -> Hashtbl.replace by_file file (f :: (try Hashtbl.find by_file file with Not_found -> []))) (choose f)
	) com.part_scope.fixes;
	let files = Hashtbl.fold (fun file fixes acc -> (file,fixes) :: acc) by_file [] in
	let files = List.sort compare files in
	let changed,files = List.partition (fun (file,fixes) ->
		let time = Common.file_time file in
		List.exists (fun f -> f.fix_time <> time) fixes
	) files in
	let changed = List.fold_left (fun n (_,fixes) -> n + List.length fixes) 0 changed in
	let dirty = if dry || config.fix_allow_dirty then [] else List.filter (fun (file,_) -> not (is_clean file)) files in
	if dirty <> [] then begin
		let names = String.concat ", " (List.map fst dirty) in
		com.error (Printf.sprintf "Not applying fixes: %s %s uncommitted changes or %s not in a git repository. Commit them first, use --fix-allow-dirty, or review the fixes with --fix-diff" names (if List.length dirty = 1 then "has" else "have") (if List.length dirty = 1 then "is" else "are")) null_pos;
		0
	end else
	let total,skipped = List.fold_left (fun (total,skipped) (file,fixes) ->
		let content = Std.input_file ~bin:true file in
		let edits,s = edits_of content (resolve_guards content fixes) in
		let n = List.length edits in
		if n > 0 then begin
			if dry then
				CompilerIo.write_out com.request_scope.io (unified_diff file content edits)
			else begin
				let ch = open_out_bin file in
				output_string ch (apply_edits content 0 edits);
				close_out ch;
				let key = com.part_scope.file_keys#get file in
				com.cs#taint_modules key ServerInvalidateFiles;
				com.cs#remove_files key;
				com.info (Printf.sprintf "Applied %d fix%s" n (if n = 1 then "" else "es")) { pfile = file; pmin = 0; pmax = 0 }
			end
		end;
		(total + n,skipped + s)
	) (0,0) files in
	let note msg = if dry then com.warning WInfo [] msg null_pos else com.info msg null_pos in
	let plural n one many = if n = 1 then one else many in
	begin match List.sort_uniq compare !too_recent with
		| [] ->
			()
		| versions ->
			let n = List.length !too_recent in
			let msg = Printf.sprintf "%d fix%s skipped: %s Haxe %s (--fix-compat %s)" n (if n = 1 then " was" else "es were")
				(if n = 1 then "it needs" else "they need") (String.concat ", " versions) (Option.default "" config.fix_compat) in
			note msg
	end;
	if !skipped_unsafe > 0 then
		note (Printf.sprintf "%d unsafe fix%s skipped, review %s with --fix-diff --fix-unsafe and apply %s with --fix-unsafe" !skipped_unsafe (plural !skipped_unsafe " was" "es were") (plural !skipped_unsafe "it" "them") (plural !skipped_unsafe "it" "them"));
	if changed > 0 then
		note (Printf.sprintf "%d fix%s skipped: %s changed during the compilation" changed (plural changed " was" "es were") (plural changed "its file" "their files"));
	if skipped > 0 then
		note (Printf.sprintf "%d overlapping fix%s skipped, compile again to apply" skipped (plural skipped " was" "es were"));
	if dry then 0 else total

let max_passes com = match com.fix_config with
	| Some config -> config.fix_passes
	| None -> 1
