type replay_kind =
	| InitCompilation of string list
	| CompileContext of string list

type replay_entry = {
	replay_time : float;
	replay_kind : replay_kind;
}

type replay = {
	replay_out : out_channel;
	mutable replay_has_entry : bool;
}

let cleanup () =
	(* Cleanup old logs so we don't get mysqled *)
	let num_keep = 5 in
	let dir = "dump/replay" in
	if Sys.file_exists dir then begin
		let entries = Sys.readdir dir in
		let l = Array.fold_left (fun acc file ->
			let path = dir ^ "/" ^ file in
			(path :: acc)
		) [] entries in
		let l = List.sort compare l in
		let l = List.rev l in
		let rec loop i l = match l with
			| [] ->
				()
			| x :: l ->
				if i >= num_keep then Sys.remove x;
				loop (i + 1) l
		in
		loop 0 l
	end

let create () =
	(* We shouldn't have to call that here if we can find a way to make sure each server run
	   calls close upon exiting. It's not obvious how to do that in all cases though... *)
	cleanup();
	let open Unix in
	let t = localtime (Unix.time()) in
	let file_name = Printf.sprintf "%.4d%.2d%.2d_%.2d-%.2d-%.2d.replay" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec in
	let ch = Path.create_file false ".log" [] ["dump";"replay";file_name] in
	output_string ch "[";
	{
		replay_out = ch;
		replay_has_entry = false;
	}

let close replay =
	output_string replay.replay_out "\n]";
	close_out replay.replay_out;
	cleanup()

let create_entry time kind = {
	replay_time = time;
	replay_kind = kind;
}

let create_entry_now kind =
	create_entry (Sys.time()) kind

open Json

let entry_to_json entry =
	let kind_to_json kind =
		let name,args = match kind with
			| InitCompilation args ->
				"InitCompilation",List.map (fun s -> JString s) args
			| CompileContext args ->
				"CompileContext",List.map (fun s -> JString s) args
		in
		[
			"kind",JString name;
			"args",JArray args;
		]
	in
	let l = kind_to_json entry.replay_kind in
	let l = ("time",JFloat entry.replay_time) :: l in
	JObject l

let add_entry replay entry =
	if not replay.replay_has_entry then begin
		replay.replay_has_entry <- true;
	end else
		output_string replay.replay_out ",";
	output_string replay.replay_out "\n\t";
	output_string replay.replay_out (string_of_json (entry_to_json entry));
	flush replay.replay_out
