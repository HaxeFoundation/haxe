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
open Globals
open Type
open Typecore

let rec is_removable_class c =
	match c.cl_kind with
	| KGeneric ->
		(Meta.has Meta.Remove c.cl_meta ||
		(match c.cl_super with
			| Some (c,_) -> is_removable_class c
			| _ -> false) ||
		List.exists (fun tp -> match follow tp.ttp_type with
			| TInst(c,_) ->
				has_ctor_constraint c || Meta.has Meta.Const c.cl_meta
			| _ ->
				false
		) c.cl_params)
	| KTypeParameter _ ->
		(* this shouldn't happen, have to investigate (see #4092) *)
		true
	| _ ->
		false

(**
	Check if `field` is overridden in subclasses
*)
let is_overridden cls field =
	let rec loop_inheritance c =
		(PMap.mem field.cf_name c.cl_fields)
		|| List.exists (fun d -> loop_inheritance d) c.cl_descendants;
	in
	List.exists (fun d -> loop_inheritance d) cls.cl_descendants

let run_expression_filters ?(ignore_processed_status=false) ctx detail_times filters t =
	let com = ctx.com in
	let run identifier e =
		List.fold_left (fun e (filter_name,f) ->
			FilterContext.with_timer detail_times filter_name identifier (fun () -> f e)
		) e filters
	in
	match t with
	| TClassDecl c when is_removable_class c -> ()
	| TClassDecl c ->
		ctx.curclass <- c;
		ctx.m <- TypeloadModule.make_curmod ctx c.cl_module;
		let rec process_field f =
			if ignore_processed_status || not (has_class_field_flag f CfPostProcessed) then begin
				ctx.curfield <- f;
				(match f.cf_expr with
				| Some e when not (is_removable_field com f) ->
					let identifier = Printf.sprintf "%s.%s" (s_type_path c.cl_path) f.cf_name in
					f.cf_expr <- Some (rec_stack_loop AbstractCast.cast_stack f (run (Some identifier)) e);
				| _ -> ());
			end;
			List.iter process_field f.cf_overloads
		in
		List.iter process_field c.cl_ordered_fields;
		List.iter process_field c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> process_field f);
		(match c.cl_init with
		| None -> ()
		| Some e ->
			let identifier = Printf.sprintf "%s.__init__" (s_type_path c.cl_path) in
			c.cl_init <- Some (run (Some identifier) e));
	| TEnumDecl _ -> ()
	| TTypeDecl _ -> ()
	| TAbstractDecl _ -> ()
