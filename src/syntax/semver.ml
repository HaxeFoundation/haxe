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

(**
	A single version within semver string (e.g major or minor or patch)
*)
type version =
	| SVNum of int
	| SVStr of string	(* `alpha`, `pre`, `rc` etc. *)

let s_version = function
	| SVNum n -> "SVNum " ^ (string_of_int n)
	| SVStr s -> "SVStr " ^ s

let to_string ((major,minor,patch),pre) =
	let str v =
		match v with
			| SVNum n -> string_of_int n
			| SVStr s -> s
	in
	(String.concat "." (List.map str [major;minor;patch]))
	^ match pre with
		| None -> ""
		| Some pre -> "-" ^ (String.concat "." (List.map str pre))


(**
	Parse SemVer string
*)
let parse_version s =
	let error () =
		raise (Invalid_argument ("Invalid version string \"" ^ s ^ "\". Should follow SemVer."))
	in
	let parse dotted_str =
		List.map
			(fun s ->
				try SVNum (int_of_string s)
				with _ -> SVStr s
			)
			(ExtString.String.nsplit dotted_str ".")
	in
	let parse_release dotted_str =
		match parse dotted_str with
			| [SVNum _ as major; SVNum _ as minor; SVNum _ as patch] -> (major, minor, patch)
			| _ -> error()
	in
	let index = try Some (String.index s '-') with Not_found -> None in
	match index with
		(* 1.2.3 *)
		| None -> (parse_release s), None
		(* 1.2.3- *)
		| Some index when index + 1 = String.length s -> error()
		(* 1.2.3-alpha.1+23 *)
		| Some index ->
			let release = parse_release (String.sub s 0 index)
			and pre =
				let pre_str = String.sub s (index + 1) (String.length s - (index + 1)) in
				(* remove build meta *)
				let pre_str =
					try String.sub pre_str 0 (String.index pre_str '+')
					with Not_found -> pre_str
				in
				parse pre_str
			in
			release, Some pre

(**
	@see https://semver.org/#spec-item-11
*)
let compare_version a b =
	let compare_v v1 v2 =
		match v1,v2 with
			| SVNum n1, SVNum n2 -> compare n1 n2
			| SVStr s1, SVStr s2 -> compare s1 s2
			| SVStr _, SVNum _ -> 1
			| SVNum _, SVStr _ -> -1
	in
	let rec compare_lists version_list1 version_list2 =
		match version_list1, version_list2 with
			| [], [] -> 0
			| [], _ -> -1
			| _, [] -> 1
			| v1 :: rest1, v2 :: rest2 ->
				let diff = compare_v v1 v2 in
				if diff <> 0 then diff
				else compare_lists rest1 rest2
	in
	match a, b with
		| ((major1,minor1,patch1), pre1), ((major2,minor2,patch2), pre2) ->
			let diff = compare_lists [major1;minor1;patch1] [major2;minor2;patch2] in
			if diff <> 0 then
				diff
			else
				match pre1, pre2 with
				| None, None -> 0
				| None, _ -> 1
				| _, None -> -1
				| Some pre1, Some pre2 -> compare_lists pre1 pre2