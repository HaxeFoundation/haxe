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

let get_time = Extc.time

type timer_node = {
	name : string;
	path : string;
	parent : timer_node;
	info : string;
	mutable time : float;
	mutable num_calls : int;
	mutable children : timer_node list;
}

module ActiveTimers : sig
	type t
	val timer : string list -> t
	val close : float -> t -> unit
	val close_times : unit -> unit
	val clear_times : unit -> unit
	(* Iterate closed timers only. Arguments: timer id and total duration *)
	val iter_times : (string list -> float -> unit) -> unit
	(* Iterate closed timers only. Arguments: timer id and total duration *)
	val fold_times : (string list -> float -> 'a -> 'a) -> 'a -> 'a
	val build_times_tree : unit -> (int * int * timer_node)
end = struct
	type t = {
		id : string list;
		mutable start : float;
		mutable duration : float;
		mutable previous : t;
	}

	type result = {
		r_id : string list;
		mutable r_total : float;
		mutable r_count : int;
	}

	let rec nil = {
		id = [];
		start = get_time();
		duration = 0.;
		previous = nil;
	}

	let current = ref nil
	let closed = ref nil
	(* Calculated summary of closed timers *)
	let results = Hashtbl.create 0

	let stamp time tmr =
		tmr.duration <- tmr.duration +. (time -. tmr.start);
		tmr.start <- time

	let timer id =
		let now = get_time() in
		let tmr = {
			id = id;
			start = now;
			duration = 0.;
			previous = !current;
		} in
		stamp now !current;
		current := tmr;
		tmr

	let close time tmr =
		stamp time tmr;
		current := tmr.previous;
		!current.start <- time;
		tmr.previous <- !closed;
		closed := tmr

	let close_times() =
		let now = get_time() in
		let rec close_next() =
			if !current == nil then ()
			else begin
				close now !current;
				close_next()
			end
		in
		close_next()

	let clear_times() =
		Hashtbl.clear results;
		current := nil;
		closed := nil

	let consume_closed_timers() =
		let rec loop tmr =
			if tmr == nil then ()
			else begin
				let result =
					try
						Hashtbl.find results tmr.id
					with Not_found ->
						let result = { r_id = tmr.id; r_total = 0.; r_count = 0; } in
						Hashtbl.add results tmr.id result;
						result
				in
				result.r_total <- result.r_total +. tmr.duration;
				result.r_count <- result.r_count + 1;
				loop tmr.previous
			end
		in
		loop !closed;
		closed := nil

	let iter_times callback =
		consume_closed_timers();
		Hashtbl.iter (fun _ result -> callback result.r_id result.r_total) results

	let fold_times callback initial =
		consume_closed_timers();
		Hashtbl.fold (fun _ result acc -> callback result.r_id result.r_total acc) results initial

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
		Hashtbl.iter (fun _ result ->
			let rec loop parent sl = match sl with
				| [] -> assert false
				| s :: sl ->
					let path = (match parent.path with "" -> "" | _ -> parent.path ^ ".") ^ s in
					let node = try
						let node = Hashtbl.find nodes path in
						node.num_calls <- node.num_calls + result.r_count;
						node.time <- node.time +. result.r_total;
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
							time = result.r_total;
							num_calls = result.r_count;
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
			let node = loop root result.r_id in
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
end

let timer = ActiveTimers.timer
let close tmr = ActiveTimers.close (get_time()) tmr
let close_times = ActiveTimers.close_times
let clear_times = ActiveTimers.clear_times
let iter_times = ActiveTimers.iter_times
let fold_times = ActiveTimers.fold_times
let build_times_tree = ActiveTimers.build_times_tree

let timer_fn id =
	let tmr = timer id in
	(fun() -> close tmr)

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