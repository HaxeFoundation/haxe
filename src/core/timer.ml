(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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

;;