(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let verbose = ref false

let class_path = ref ([] : string list)

let defines = ref (PMap.add "true" () PMap.empty)

let defined v = PMap.mem v (!defines)
let define v = defines := PMap.add v () (!defines)

let get_full_path = ref (fun (s:string) -> s)

let find_file f =
	let rec loop = function
		| [] -> raise Not_found
		| p :: l ->
			let file = p ^ f in
			if Sys.file_exists file then
				file
			else
				loop l
	in
	loop !class_path

type timer_infos = {
	name : string;
	mutable start : float;
	mutable total : float;
}

let times = ref true

let get_time = Unix.gettimeofday
let htimers = Hashtbl.create 0

let new_timer name = 
	try
		let t = Hashtbl.find htimers name in
		t.start <- get_time();
		t
	with Not_found ->
		let t = { name = name; start = get_time(); total = 0.; } in
		Hashtbl.add htimers name t;
		t

let curtime = ref None

let timer name =
	if not !times then
		(function() -> ())
	else
	let t = new_timer name in
	let old = !curtime in	
	curtime := Some t;
	(function() ->
		let dt = get_time() -. t.start in
		t.total <- t.total +. dt;		
		curtime := old;
		match !curtime with
		| None -> ()
		| Some ct -> ct.start <- ct.start +. dt
	)
