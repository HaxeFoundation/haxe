(*
 *  Haxe Compiler
 *  Copyright (c)2005-2008 Nicolas Cannasse
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
open Type

type package_rule =
	| Forbidden
	| Directory of string
	| Remap of string

type platform =
	| Cross
	| Flash
	| Js
	| Neko
	| Flash9
	| Php

type pos = Ast.pos

type context_type_api = {
	(* basic types *)
	mutable tvoid : t;
	mutable tint : t;
	mutable tfloat : t;
	mutable tbool : t;
	mutable tnull : t -> t;
	mutable tstring : t;
	mutable tarray : t -> t;
	(* api *)
	mutable load_module : path -> pos -> module_def;
	mutable build_instance : module_type -> pos -> ((string * t) list * path * (t list -> t));
	mutable on_generate : module_type -> unit;
	mutable get_type_module : module_type -> module_def;
}

type context = {
	(* config *)
	mutable debug : bool;
	mutable verbose : bool;
	mutable platform : platform;
	mutable class_path : string list;
	mutable main_class : Type.path option; 
	mutable defines : (string,unit) PMap.t;
	mutable package_rules : (string,package_rule) PMap.t;
	mutable error : string -> pos -> unit;
	mutable warning : string -> pos -> unit;
	(* output *)
	mutable file : string;
	mutable flash_version : int;
	mutable types : Type.module_type list;
	mutable resources : (string,string) Hashtbl.t;
	mutable php_front : string option;
	(* typing *)
	mutable type_api : context_type_api;
}

exception Abort of string * Ast.pos

let create() =
	let m = Type.mk_mono() in
	{
		debug = false;
		verbose = false;
		platform = Cross;
		class_path = [];
		main_class = None;
		defines = PMap.add "true" () PMap.empty;
		package_rules = PMap.empty;
		file = "";
		types = [];
		flash_version = 8;
		resources = Hashtbl.create 0;
		php_front = None;
		warning = (fun _ _ -> assert false);
		error = (fun _ _ -> assert false);
		type_api = {
			tvoid = m;
			tint = m;
			tfloat = m;
			tbool = m;
			tnull = (fun _ -> assert false);
			tstring = m;
			tarray = (fun _ -> assert false);
			load_module = (fun _ _ -> assert false);
			build_instance = (fun _ _ -> assert false);
			on_generate = (fun _ -> ());
			get_type_module = (fun _ -> assert false);
		};
	}

let defined ctx v = PMap.mem v ctx.defines

let define ctx v =
	ctx.defines <- PMap.add v () ctx.defines;
	let v = String.concat "_" (ExtString.String.nsplit v "-") in
	ctx.defines <- PMap.add v () ctx.defines

let error msg p = raise (Abort (msg,p))

let platform ctx p = ctx.platform = p

let find_file ctx f =
	let rec loop = function
		| [] -> raise Not_found
		| p :: l ->
			let file = p ^ f in
			if Sys.file_exists file then
				file
			else
				loop l
	in
	loop ctx.class_path

let get_full_path = Extc.get_full_path

(* ------------------------- TIMERS ----------------------------- *)

type timer_infos = {
	name : string;
	mutable start : float;
	mutable total : float;
}

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
