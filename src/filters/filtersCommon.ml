(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*)
open Type
open Common
open Typecore

let remove_generic_base t = match t with
	| TClassDecl c when FilterContext.is_removable_class c ->
		add_class_flag c CExtern;
	| _ ->
		()

(**
	Check if `field` is overridden in subclasses
*)
let is_overridden cls field =
	let rec loop_inheritance c =
		(PMap.mem field.cf_name c.cl_fields)
		|| List.exists (fun d -> loop_inheritance d) c.cl_descendants;
	in
	List.exists (fun d -> loop_inheritance d) cls.cl_descendants

let is_cached com t =
	let m = (t_infos t).mt_module.m_extra in
	m.m_processed <> 0 && m.m_processed < com.compilation_step

let apply_filters_once ctx scom filters t =
	let detail_times = (try int_of_string (Common.defined_value_safe ctx.com ~default:"0" Define.FilterTimes) with _ -> 0) in
	if not (is_cached ctx.com t) then SafeCom.run_expression_filters_safe scom detail_times filters t