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

type timer_infos = {
	id : string list;
	mutable start : float list;
	mutable pauses : float list;
	mutable total : float;
	mutable calls : int;
}

let get_time = Extc.time
let htimers = Hashtbl.create 0

let new_timer id =
	let now = get_time() in
	let key = String.concat "." id in
	try
		let t = Hashtbl.find htimers key in
		t.start <- now :: t.start;
		t.pauses <- 0. :: t.pauses;
		t.calls <- t.calls + 1;
		t
	with Not_found ->
		let t = { id = id; start = [now]; pauses = [0.]; total = 0.; calls = 1; } in
		Hashtbl.add htimers key t;
		t

let curtime = ref []

let rec close now t =
	match !curtime with
	| [] ->
		failwith ("Timer " ^ (String.concat "." t.id) ^ " closed while not active")
	| tt :: rest ->
		if t == tt then begin
			let start = List.hd t.start in
			let pauses = List.hd t.pauses in
			let dt = now -. start in
			t.total <- t.total +. dt -. pauses;
			t.start <- List.tl t.start;
			t.pauses <- List.tl t.pauses;
			curtime := rest;
			match !curtime with
			| [] -> ()
			| current :: _ ->
				let pauses = dt +. List.hd current.pauses in
				current.pauses <- pauses :: List.tl current.pauses
				(* current.total <- *)
		end else
			close now tt
	(* because of rounding errors while adding small times, we need to make sure that we don't have start > now *)
	(* List.iter (fun ct -> ct.start <- List.map (fun t -> let s = t +. dt in if s > now then now else s) ct.start) !curtime *)

let timer id =
	let t = new_timer id in
	curtime := t :: !curtime;
	(function() -> close (get_time()) t)

let rec close_times() =
	let now = get_time() in
	match !curtime with
	| [] -> ()
	| t :: _ -> close now t; close_times()

let close = close (get_time())

(* Printing *)

type timer_node = {
	name : string;
	path : string;
	parent : timer_node;
	info : string;
	mutable time : float;
	mutable num_calls : int;
	mutable children : timer_node list;
}

let build_times_tree () =
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
					node.num_calls <- node.num_calls + timer.calls;
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
						num_calls = timer.calls;
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
	) htimers;
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