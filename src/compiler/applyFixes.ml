open Globals
open Common

let full_dir path =
	Path.add_trailing_slash (Path.get_full_path (if path = "" then "." else path))

(* Only the files of the -cp class paths, not the ones of the libraries or the std *)
let is_user_file com file =
	let file = Path.get_full_path file in
	let owner = List.fold_left (fun acc cp ->
		let dir = full_dir cp in
		if ExtString.String.starts_with file dir then match acc with
			| Some best when String.length best >= String.length dir -> acc
			| _ -> Some dir
		else acc
	) None com.class_path in
	match owner with
	| None ->
		false
	| Some dir ->
		let excluded = List.map full_dir (com.lib_class_path @ com.std_path) in
		not (List.exists (fun d -> ExtString.String.starts_with dir d) excluded)

let byte_offset s chars =
	let len = String.length s in
	let rec loop i n =
		if n = chars || i >= len then i
		else
			let i = i + 1 in
			let rec skip i = if i < len && (Char.code s.[i]) land 0xC0 = 0x80 then skip (i + 1) else i in
			loop (skip i) (n + 1)
	in
	loop 0 0

let byte_range s p =
	let bmin = byte_offset s p.pmin in
	bmin,bmin + byte_offset (String.sub s bmin (String.length s - bmin)) (p.pmax - p.pmin)

(* Overlapping fixes keep the outermost one; fixes whose text changed are dropped *)
let edits_of content fixes =
	let sorted = List.sort_uniq (fun a b ->
		let c = compare a.fix_pos.pmin b.fix_pos.pmin in
		if c <> 0 then c else compare b.fix_pos.pmax a.fix_pos.pmax
	) fixes in
	let applied,skipped,_ = List.fold_left (fun (applied,skipped,last) f ->
		let bmin,bmax = byte_range content f.fix_pos in
		if bmax > String.length content || String.sub content bmin (bmax - bmin) <> f.fix_old then (applied,skipped,last)
		else if bmin < last then (applied,skipped + 1,last)
		else ((bmin,bmax,f.fix_text) :: applied,skipped,bmax)
	) ([],0,0) sorted in
	List.rev applied,skipped

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

let plural n one many = if n = 1 then one else many

let run com write_out config =
	let by_file = Hashtbl.create 0 in
	List.iter (fun f ->
		let file = f.fix_pos.pfile in
		if is_user_file com file then Hashtbl.replace by_file file (f :: (try Hashtbl.find by_file file with Not_found -> []))
	) config.fixes;
	let files = List.sort compare (Hashtbl.fold (fun file fixes acc -> (file,fixes) :: acc) by_file []) in
	let dirty = if config.fix_diff || config.fix_allow_dirty then [] else List.filter (fun (file,_) -> not (is_clean file)) files in
	if dirty <> [] then
		com.error (Printf.sprintf "Not applying fixes: %s %s uncommitted changes or %s not in a git repository. Commit them first, use --fix-allow-dirty, or review the fixes with --fix-diff"
			(String.concat ", " (List.map fst dirty)) (plural (List.length dirty) "has" "have") (plural (List.length dirty) "is" "are")) null_pos
	else begin
		let skipped = List.fold_left (fun skipped (file,fixes) ->
			let content = Std.input_file ~bin:true file in
			let edits,s = edits_of content fixes in
			let n = List.length edits in
			if n > 0 then begin
				if config.fix_diff then
					write_out (unified_diff file content edits)
				else begin
					let ch = open_out_bin file in
					output_string ch (apply_edits content 0 edits);
					close_out ch;
					com.info (Printf.sprintf "Applied %d fix%s" n (plural n "" "es")) { pfile = file; pmin = 0; pmax = 0 }
				end
			end;
			skipped + s
		) 0 files in
		if skipped > 0 then
			com.info (Printf.sprintf "%d overlapping fix%s skipped, compile again to apply" skipped (plural skipped " was" "es were")) null_pos
	end
