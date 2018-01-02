(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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
	mutable total : float;
	mutable calls : int;
}

let get_time = Extc.time
let htimers = Hashtbl.create 0

let new_timer id =
	let key = String.concat "." id in
	try
		let t = Hashtbl.find htimers key in
		t.start <- get_time() :: t.start;
		t.calls <- t.calls + 1;
		t
	with Not_found ->
		let t = { id = id; start = [get_time()]; total = 0.; calls = 1; } in
		Hashtbl.add htimers key t;
		t

let curtime = ref []

let close t =
	let start = (match t.start with
		| [] -> assert false
		| s :: l -> t.start <- l; s
	) in
	let now = get_time() in
	let dt = now -. start in
	t.total <- t.total +. dt;
	let rec loop() =
		match !curtime with
		| [] -> failwith ("Timer " ^ (String.concat "." t.id) ^ " closed while not active")
		| tt :: l -> curtime := l; if t != tt then loop()
	in
	loop();
	(* because of rounding errors while adding small times, we need to make sure that we don't have start > now *)
	List.iter (fun ct -> ct.start <- List.map (fun t -> let s = t +. dt in if s > now then now else s) ct.start) !curtime

let timer id =
	let t = new_timer id in
	curtime := t :: !curtime;
	(function() -> close t)

let rec close_times() =
	match !curtime with
	| [] -> ()
	| t :: _ -> close t; close_times()

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

let report_times print =
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
	let max_calls = String.length (string_of_int !max_calls) in
	print (Printf.sprintf "%-*s | %7s |   %% |  p%% | %*s | info" !max_name "name" "time(s)" max_calls "#");
	let sep = String.make (!max_name + max_calls + 27) '-' in
	print sep;
	let print_time name node =
		if node.time > 0.0009 then
			print (Printf.sprintf "%-*s | %7.3f | %3.0f | %3.0f | %*i | %s" !max_name name node.time (node.time *. 100. /. root.time) (node.time *. 100. /. node.parent.time) max_calls node.num_calls node.info)
	in
	let rec loop depth node =
		let name = (String.make (depth * 2) ' ') ^ node.name in
		print_time name node;
		List.iter (loop (depth + 1)) node.children
	in
	List.iter (loop 0) root.children;
	print sep;
	print_time "total" root