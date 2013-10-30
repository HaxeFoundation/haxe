(*
 *  This file is part of ilLib
 *  Copyright (c)2004-2013 Haxe Foundation
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

open PeData;;
open PeReader;;
open IlMeta;;
open IO;;

(* encoding helpers *)

let int_of_type_def_vis = function
	(* visibility flags - mask 0x7 *)
	| VPrivate -> 0x0 (* 0x0 *)
	| VPublic -> 0x1 (* 0x1 *)
	| VNestedPublic -> 0x2 (* 0x2 *)
	| VNestedPrivate -> 0x3 (* 0x3 *)
	| VNestedFamily -> 0x4 (* 0x4 *)
	| VNestedAssembly -> 0x5 (* 0x5 *)
	| VNestedFamAndAssem -> 0x6 (* 0x6 *)
	| VNestedFamOrAssem -> 0x7 (* 0x7 *)

let int_of_type_def_layout = function
	(* layout flags - mask 0x18 *)
	| LAuto -> 0x0 (* 0x0 *)
	| LSequential -> 0x8 (* 0x8 *)
	| LExplicit -> 0x10 (* 0x10 *)

let int_of_type_def_semantics props = List.fold_left (fun acc prop ->
		(match prop with
		(* semantics flags - mask 0x5A0 *)
		| SInterface -> 0x20 (* 0x20 *)
		| SAbstract -> 0x80 (* 0x80 *)
		| SSealed -> 0x100 (* 0x100 *)
		| SSpecialName -> 0x400 (* 0x400 *)
		) lor acc
	) 0 props

let int_of_type_def_impl props = List.fold_left (fun acc prop ->
		(match prop with
		(* type implementation flags - mask 0x103000 *)
		| IImport -> 0x1000 (* 0x1000 *)
		| ISerializable -> 0x2000 (* 0x2000 *)
		| IBeforeFieldInit -> 0x00100000 (* 0x00100000 *)
		) lor acc
	) 0 props

let int_of_type_def_string = function
	(* string formatting flags - mask 0x00030000 *)
	| SAnsi -> 0x0 (* 0x0 *)
	| SUnicode -> 0x00010000 (* 0x00010000 *)
	| SAutoChar -> 0x00020000 (* 0x00020000 *)

let int_of_type_def_flags f =
	int_of_type_def_vis f.tdf_vis
		logor
	int_of_type_def_layout f.tdf_layout
		logor
	int_of_type_def_semantics f.tdf_semantics
		logor
	int_of_type_def_impl f.tdf_impl
		logor
	int_of_type_def_string f.tdf_string
