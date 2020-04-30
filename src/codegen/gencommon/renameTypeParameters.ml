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
open Type

(* ******************************************* *)
(* Rename Type Parameters *)
(* ******************************************* *)
(*
	This module should run after everything is already applied,
	it will look for possible type parameter name clashing and change the classes names to a
*)
let run types =
	let i = ref 0 in
	let found_types = ref PMap.empty in
	let check_type name on_changed =
		let rec loop name =
			incr i;
			let changed_name = (name ^ (string_of_int !i)) in
			if PMap.mem changed_name !found_types then loop name else changed_name
		in
		if PMap.mem name !found_types then begin
			let new_name = loop name in
			found_types := PMap.add new_name true !found_types;
			on_changed new_name
		end else found_types := PMap.add name true !found_types
	in

	let get_cls t =
		match follow t with
		| TInst(cl,_) -> cl
		| _ -> Globals.die "" __LOC__
	in

	let iter_types (nt,t) =
		let cls = get_cls t in
		let orig = cls.cl_path in
		check_type (snd orig) (fun name -> cls.cl_path <- (fst orig, name))
	in

	let save_params save params =
		List.fold_left (fun save (_,t) ->
			let cls = get_cls t in
			(cls.cl_path,t) :: save) save params
	in

	List.iter (function
		| TClassDecl cl ->
			i := 0;

			let save = [] in

			found_types := PMap.empty;
			let save = save_params save cl.cl_params in
			List.iter iter_types cl.cl_params;
			let cur_found_types = !found_types in
			let save = ref save in
			List.iter (fun cf ->
				found_types := cur_found_types;
				save := save_params !save cf.cf_params;
				List.iter iter_types cf.cf_params
			) (cl.cl_ordered_fields @ cl.cl_ordered_statics);

			if !save <> [] then begin
				let save = !save in
				let res = cl.cl_restore in
				cl.cl_restore <- (fun () ->
					res();
					List.iter (fun (path,t) ->
						let cls = get_cls t in
						cls.cl_path <- path) save
				);
			end

		| TEnumDecl ( ({ e_params = hd :: tl }) ) ->
			i := 0;
			found_types := PMap.empty;
			List.iter iter_types (hd :: tl)

		| TAbstractDecl { a_params = hd :: tl } ->
			i := 0;
			found_types := PMap.empty;
			List.iter iter_types (hd :: tl)

		| _ -> ()
	) types
