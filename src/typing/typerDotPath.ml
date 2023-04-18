(*
	The Haxe Compiler
	Copyright (C) 2005-2020 Haxe Foundation

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
open Globals
open Type
open Typecore
open TType
open TyperBase
open Calls
open Fields
open TFunctions
open Error

let mk_dot_path_part s p : dot_path_part =
	let case = if is_lower_ident s p then PLowercase else PUppercase in
	{
		name = s;
		case = case;
		pos = p
	}

let s_dot_path parts =
	String.concat "." (List.map (fun (s,_,_) -> s) parts)

(** resolve given path against module fields or raise Not_found *)
let resolve_module_field ctx m path p mode with_type =
	match path, m.m_statics with
	| [], _ | _, None ->
		raise Not_found
	| {name = name; pos = p} :: path_rest, Some c ->
		let f = PMap.find name c.cl_statics in (* raises Not_found *)
		let e = type_module_type ctx (TClassDecl c) None p in
		field_access ctx mode f (FHStatic c) e p, path_rest

let resolve_module_type ctx m name p =
	let t = Typeload.find_type_in_module m name in (* raises Not_found *)
	mk_module_type_access ctx t p

let resolve_in_module ctx m path p mode with_type =
	try
		(* first, try to find module-level static access *)
		resolve_module_field ctx m path p mode with_type
	with Not_found ->
		(* if there was no module fields, resolve  *)
		let mname = snd m.m_path in
		match path with
		| {name = sname; case = PUppercase; pos = sp} :: path_rest ->
			begin
			try
				resolve_module_type ctx m sname sp, path_rest
			with Not_found ->
				resolve_module_type ctx m mname p, path
			end
		| _ ->
			resolve_module_type ctx m mname p, path

(** resolve given qualified module pack+name (and possibly next path part) or raise Not_found *)
let resolve_qualified ctx pack name next_path p mode with_type =
	try
		let m = Typeload.load_module ctx (pack,name) p in
		resolve_in_module ctx m next_path p mode with_type
	with Error { err_message = Module_not_found mpath } when mpath = (pack,name) ->
		(* might be an instance of https://github.com/HaxeFoundation/haxe/issues/9150
		   so let's also check (pack,name) of a TYPE in the current module context ¯\_(ツ)_/¯ *)
		let t = Typeload.find_type_in_current_module_context ctx pack name in (* raises Not_found *)
		mk_module_type_access ctx t p, next_path

(** resolve the given unqualified name (and possibly next path part) or raise Not_found *)
let resolve_unqualified ctx name next_path p mode with_type =
	try
		(* if there's a type with this name in current module context - try resolving against it *)
		let t = Typeload.find_type_in_current_module_context ctx [] name in (* raises Not_found *)

		begin
			(*
				if there's further field access, it might be a this-package module access rather than static field access,
				so we try resolving a field first and fall back to find_in_unqualified_modules
			*)
			match next_path with
			| {name = field; pos = pfield} :: next_path ->
				let e = type_module_type ctx t None p in
				let access = type_field (TypeFieldConfig.create true) ctx e field pfield mode with_type in
				access, next_path
			| _ ->
				mk_module_type_access ctx t p, next_path
		end
	with Not_found ->
		(* otherwise run the unqualified module resolution mechanism and look into the modules  *)
		let f m ~resume = resolve_in_module ctx m next_path p mode with_type in
		Typeload.find_in_unqualified_modules ctx name p f ~resume:true (* raise Not_found *)

(** given a list of dot path parts, resolve it into access getter or raise Not_found *)
let resolve_dot_path ctx (path_parts : dot_path_part list) mode with_type =
	let rec loop pack_acc path =
		match path with
		| {case = PLowercase} as x :: path ->
			(* part starts with lowercase - it's a package part, add it the accumulator and proceed *)
			loop (x :: pack_acc) path

		| {name = name; case = PUppercase; pos = p} :: path ->
			(* If this is the last part we want to use the actual mode. *)
			let mode,with_type = match path with
				| [] | [_] -> mode,with_type
				| _ -> MGet,WithType.value
			in
			(* part starts with uppercase - it's a module name - try resolving *)
			let accessor, path_rest =
				if pack_acc <> [] then
					let pack = List.rev_map (fun part -> part.name) pack_acc in
					resolve_qualified ctx pack name path p mode with_type
				else
					resolve_unqualified ctx name path p mode with_type
			in
			(* if we get here (that is, Not_found is not raised) - we have something to resolve against *)
			field_chain ctx path_rest accessor

		| [] ->
			(* if we get to here, it means that there was no uppercase part, so it's not a qualified dot-path *)
			raise Not_found
	in
	loop [] path_parts
