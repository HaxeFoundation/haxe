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
open TyperBase
open Calls
open TFunctions
open Error

type dot_path_part_case =
	| PUppercase
	| PLowercase

type dot_path_part = (string * dot_path_part_case * pos)

let mk_dot_path_part s p : dot_path_part =
	let case = if is_lower_ident s p then PLowercase else PUppercase in
	(s,case,p)

(* given a list of dot path parts, try to resolve it into access getter, raises Not_found on failure *)
let resolve_dot_path ctx (path_parts : dot_path_part list) =
	(*
		we rely on the fact that packages start with a lowercase letter, while modules and types start with uppercase letters,
		so we processes path parts, accumulating lowercase parts in `pack_acc`, until we encounter an upper-case part,
		which can mean either a module access or module's primary type access, so we try to figure out the type and
		resolve the rest of the field access chain against it.
	*)
	let rec loop pack_acc path =
		match path with
		| (_,PLowercase,_) as x :: path ->
			(* part starts with lowercase - it's a package part, add it the accumulator and proceed *)
			loop (x :: pack_acc) path

		| (name,PUppercase,p) :: path ->
			(* part starts with uppercase - it either points to a module or its main type *)

			let pack = List.rev_map (fun (x,_,_) -> x) pack_acc in

			(* default behaviour: try loading module's primary type (with the same name as module)
				and resolve the rest of the field chain against its statics (or return the type itself
				if the rest of chain is empty) *)
			let def () =
				try
					let e = type_type ctx (pack,name) p in
					field_chain ctx path (fun _ -> AKExpr e)
				with
					Error (Module_not_found m,_) when m = (pack,name) ->
						(* not a module path after all *)
						raise Not_found
			in

			(match path with
			| (sname,PUppercase,p) :: path ->
				(* next part starts with uppercase, meaning it can be either a module sub-type access
					or static field access for the primary module type, so we have to do some guessing here

					In this block, `name` is the first first-uppercase part (possibly a module name),
					and `sname` is the second first-uppsercase part (possibly a subtype name). *)

				(* get static field by `sname` from a given type `t`, if `resume` is true - raise Not_found *)
				let get_static resume t =
					field_chain ctx ~resume ((sname,PUppercase,p) :: path) (fun _ -> AKExpr (type_module_type ctx t None p))
				in

				(* try accessing subtype or main class static field by `sname` in given module with path `m` *)
				let check_module m =
					try
						let md = TypeloadModule.load_module ctx m p in
						(* first look for existing subtype *)
						(try
							let t = List.find (fun t -> not (t_infos t).mt_private && t_path t = (fst m,sname)) md.m_types in
							Some (field_chain ctx path (fun _ -> AKExpr (type_module_type ctx t None p)))
						with Not_found -> try
						(* then look for main type statics *)
							if fst m = [] then raise Not_found; (* ensure that we use def() to resolve local types first *)
							let t = List.find (fun t -> not (t_infos t).mt_private && t_path t = m) md.m_types in
							Some (get_static false t)
						with Not_found ->
							None)
					with Error (Module_not_found m2,_) when m = m2 ->
						None
				in

				(match pack with
				| [] ->
					(* no package was specified - Unqualified access *)
					(try
						(* first try getting a type by `name` in current module types and current imports
							and try accessing its static field by `sname` *)
						let path_match t = snd (t_infos t).mt_path = name in
						let t =
							try
								List.find path_match ctx.m.curmod.m_types (* types in this modules *)
							with Not_found ->
								let t,p = List.find (fun (t,_) -> path_match t) ctx.m.module_types in (* imported types *)
								ImportHandling.mark_import_position ctx p;
								t
						in
						get_static true t
					with Not_found ->
						(* if the static field (or the type) wasn't not found, look for a subtype instead - #1916
							look for subtypes/main-class-statics in modules of current package and its parent packages *)
						let rec loop pack =
							match check_module (pack,name) with
							| Some r -> r
							| None ->
								match List.rev pack with
								| [] -> def()
								| _ :: l -> loop (List.rev l)
						in
						loop (fst ctx.m.curmod.m_path))
				| _ ->
					(* if package was specified - Qualified access *)
					(match check_module (pack,name) with
					| Some r -> r
					| None -> def ()));
			| _ ->
				(* no more parts or next part starts with lowercase - it's surely not a type name, so do the default thing *)
				def())

		| [] ->
			(* if we get to here, it means that there was no uppercase parts, so it's not a qualified dot-path *)
			raise Not_found

	in
	loop [] path_parts
