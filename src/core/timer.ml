(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

module Timer : sig
	type t
	type timer_node = {
		name : string;
		path : string;
		parent : timer_node;
		info : string;
		mutable time : float;
		mutable num_calls : int;
		mutable children : timer_node list;
	}

	val get_time : unit -> float
	val close : float -> t -> unit
	val timer : string list -> t
	val close_times : unit -> unit
	val report_times : (string -> unit) -> unit
	(* Iterate closed timers only. Arguments: timer id and total duration *)
	val iter_times : (string list -> float -> unit) -> unit
	(* Iterate closed timers only. Arguments: timer id and total duration *)
	val fold_times : (string list -> float -> 'a -> 'a) -> 'a -> 'a
	val clear_times : unit -> unit
	val build_times_tree : unit -> (int * int * timer_node)
end =
struct
	type t = {
		id : string list;
		mutable start : float;
		mutable pause : float;
	}

	type timer_node = {
		name : string;
		path : string;
		parent : timer_node;
		info : string;
		mutable time : float;
		mutable num_calls : int;
		mutable children : timer_node list;
	}

	type result = {
		id : string list;
		mutable total : float;
		mutable count : int;
	}

	let no_pause_yet = -1.
	let is_paused t = t.pause >= 0.

	let get_time = Extc.time

	let open_timers = ref []
	let closed_timers = ref []
	let results = Hashtbl.create 0

	let close end_time t =
		let dt = end_time -. t.start in
		closed_timers := (t.id, if dt < 0. then 0. else dt) :: !closed_timers;
		let rec loop() =
			match !open_timers with
			| [] -> failwith ("Timer " ^ (String.concat "." t.id) ^ " closed while not active")
			| tt :: l ->
				if t == tt then begin
					match l with
					| previous :: rest when is_paused previous ->
						let now = get_time() in
						let new_start = now -. (previous.pause -. previous.start) in
						previous.start <- if new_start > now then now else new_start;
						previous.pause <- no_pause_yet
					| _ -> ()
				end;
				open_timers := l;
				if t != tt then loop()
		in
		loop()
(*
		(* because of rounding errors while adding small times, we need to make sure that we don't have start > end_time *)
		List.iter
			(fun ct ->
				ct.start <- List.map (fun t ->
					let s = t +. dt in
					if s > end_time then end_time else s
				) ct.start
			)
			!open_timers *)

	let timer id =
		let now = get_time() in
		let t = { id = id; start = now; pause = no_pause_yet; } in
		(match !open_timers with
		| [] -> ()
		| current :: rest ->
			if not (is_paused current) then
				current.pause <- now
		);
		open_timers := t :: !open_timers;
		t

	let close_times() =
		let end_time = get_time() in
		List.iter (close end_time) !open_timers

	let consume_closed_timers() =
		match !closed_timers with
		| [] -> ()
		| timers ->
			List.iter
				(fun (id, duration) ->
					let result =
						try
							Hashtbl.find results id
						with Not_found ->
							let result = { id = id; total = 0.; count = 0; } in
							Hashtbl.add results id result;
							result
					in
					result.total <- result.total +. duration;
					result.count <- result.count + 1
				)
				timers;
			closed_timers := []

	let iter_times callback =
		consume_closed_timers();
		Hashtbl.iter (fun _ result -> callback result.id result.total) results

	let fold_times callback initial =
		consume_closed_timers();
		Hashtbl.fold (fun _ result acc -> callback result.id result.total acc) results initial

	let clear_times() = Hashtbl.clear results

	(* Printing *)

	let build_times_tree () =
		consume_closed_timers();
		let nodes = Hashtbl.create 0 in
		let rec root = {
			name = "";
			path = "";
			parent = root;
			info = "";
			time = 0.;
			num_calls = 0;
			children = [];
		} in
		Hashtbl.iter (fun _ timer ->
			let rec loop parent sl = match sl with
				| [] -> assert false
				| s :: sl ->
					let path = (match parent.path with "" -> "" | _ -> parent.path ^ ".") ^ s in
					let node = try
						let node = Hashtbl.find nodes path in
						node.num_calls <- node.num_calls + timer.count;
						node.time <- node.time +. timer.total;
						node
					with Not_found ->
						let name,info = try
							let i = String.rindex s '.' in
							String.sub s (i + 1) (String.length s - i - 1),String.sub s 0 i
						with Not_found ->
							s,""
						in
						let node = {
							name = name;
							path = path;
							parent = parent;
							info = info;
							time = timer.total;
							num_calls = timer.count;
							children = [];
						} in
						Hashtbl.add nodes path node;
						node
					in
					begin match sl with
						| [] -> ()
						| _ ->
							let child = loop node sl in
							if not (List.memq child node.children) then
								node.children <- child :: node.children;
					end;
					node
			in
			let node = loop root timer.id in
			if not (List.memq node root.children) then
				root.children <- node :: root.children
		) results;
		let max_name = ref 0 in
		let max_calls = ref 0 in
		let rec loop depth node =
			let l = (String.length node.name) + 2 * depth in
			List.iter (fun child ->
				if depth = 0 then begin
					node.num_calls <- node.num_calls + child.num_calls;
					node.time <- node.time +. child.time;
				end;
				loop (depth + 1) child;
			) node.children;
			node.children <- List.sort (fun node1 node2 -> compare node2.time node1.time) node.children;
			if node.num_calls > !max_calls then max_calls := node.num_calls;
			if node.time > 0.0009 && l > !max_name then max_name := l;
		in
		loop 0 root;
		!max_name,!max_calls,root

	let report_times print =
		let max_name,max_calls,root = build_times_tree () in
		let max_calls = String.length (string_of_int max_calls) in
		print (Printf.sprintf "%-*s | %7s |   %% |  p%% | %*s | info" max_name "name" "time(s)" max_calls "#");
		let sep = String.make (max_name + max_calls + 27) '-' in
		print sep;
		let print_time name node =
			if node.time > 0.0009 then
				print (Printf.sprintf "%-*s | %7.3f | %3.0f | %3.0f | %*i | %s" max_name name node.time (node.time *. 100. /. root.time) (node.time *. 100. /. node.parent.time) max_calls node.num_calls node.info)
		in
		let rec loop depth node =
			let name = (String.make (depth * 2) ' ') ^ node.name in
			print_time name node;
			List.iter (loop (depth + 1)) node.children
		in
		List.iter (loop 0) root.children;
		print sep;
		print_time "total" root
end

type timer_node = Timer.timer_node

let get_time = Timer.get_time
let timer = Timer.timer
let close t = Timer.close (Timer.get_time()) t
let close_times = Timer.close_times
let report_times = Timer.report_times
let iter_times = Timer.iter_times
let fold_times = Timer.fold_times
let clear_times = Timer.clear_times
let build_times_tree = Timer.build_times_tree

let timer_fn id =
	let t = Timer.timer id in
	(fun () -> close t)