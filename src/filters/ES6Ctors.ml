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
open Common
open Globals
open Type
open Texpr.Builder

(* name of the method to which the constructor is extracted *)
let ctor_method_name = "_hx_constructor"

(* name of the static bool flag to skip constructor body execution *)
let skip_ctor_flag_name = "_hx_skip_constructor"

(* replace super(a,b,c) with super._hx_constructor(a,b,c) *)
let rec replace_super_call e =
	match e.eexpr with
	| TCall ({ eexpr = TConst TSuper } as e_super, args) ->
		let e_super_hxctor = { e_super with eexpr = TField (e_super, FDynamic ctor_method_name) } in
		{ e with eexpr = TCall (e_super_hxctor, args) }
	| _ ->
		map_expr replace_super_call e

exception Accessed_this of texpr

(* return whether given expression has `this` access before calling `super` *)
let has_this_before_super e =
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper }, args) ->
			List.iter loop args;
			raise Exit
		| TConst TThis ->
			raise (Accessed_this e)
		| _ ->
			Type.iter loop e
	in
	try
		(loop e; None)
	with
		| Exit -> None
		| Accessed_this e -> Some e

let get_num_args cf =
	match follow cf.cf_type with
	| TFun (args, _) -> List.length args
	| _ -> die()

(*
	the filter works in two passes:
	- mark classes whether they need to support constructor skipping and/or they skip parent's constructor
	- change the constructors of marked classes (extract body into method and/or add skipping super calls)
*)
let rewrite_ctors com =
	(* we mark classes that need changing by storing them in these two maps *)
	let needs_ctor_skipping, does_ctor_skipping, inject_super =
		let l = List.length com.types in
		Hashtbl.create l, Hashtbl.create l, Hashtbl.create l
	in

	(*
		we're using a reference to the root of the inheritance chain so we can easily
		generate RootClass._hx_skip_constructor expressions
	*)
	let mark_does_ctor_skipping cl cl_super p_this_access =
		let rec mark_needs_ctor_skipping cl =
			(* for non haxe-generated extern classes we can't generate any valid code, so just fail *)
			if cl.cl_extern && not (Meta.has Meta.HxGen cl.cl_meta) then begin
				abort "Must call `super()` constructor before accessing `this` in classes derived from an extern class with constructor" p_this_access;
			end;
			try
				Hashtbl.find needs_ctor_skipping cl.cl_path
			with Not_found ->
				let root =
					match cl.cl_super with
					| Some ({ cl_constructor = Some ctor_super } as cl_super,_) ->
						let root = mark_needs_ctor_skipping cl_super in
						Option.may (fun ctor ->
							(* if parent's constructor receives less arguments than needed for this - we need to override the constructor *)
							if get_num_args ctor > get_num_args ctor_super then
								Hashtbl.add does_ctor_skipping cl.cl_path root;
						) cl.cl_constructor;
						root
					| _ ->
						cl
				in
				Hashtbl.add needs_ctor_skipping cl.cl_path root;
				root
		in
		let root_cl = mark_needs_ctor_skipping cl_super in
		Hashtbl.add does_ctor_skipping cl.cl_path root_cl;
	in

	let e_empty_super_call = (* super() *)
		let e_super = mk (TConst TSuper) t_dynamic null_pos in
		mk (TCall (e_super,[])) com.basic.tvoid null_pos
	in

	let activated = ref false in
	let mark t =
		match t with
		| TClassDecl ({ cl_constructor = Some { cf_expr = Some { eexpr = TFunction tf } }; cl_super = Some (cl_super,_) } as cl) ->
			if Type.has_constructor cl_super then begin
				(* if parent class has a constructor, check for `this` accesses before calling `super()` *)
				let this_before_super = has_this_before_super tf.tf_expr in
				Option.may (fun e_this_access ->
					activated := true;
					mark_does_ctor_skipping cl cl_super e_this_access.epos
				) this_before_super
			end else begin
				(* if there was no ctor in the parent class, we still gotta call `super` *)
				Hashtbl.add inject_super cl.cl_path cl;
			end
		| _ -> ()
	in
	List.iter mark com.types;

	if !activated then begin
		(* just some helper common exprs *)
		let e_false = (make_bool com.basic false null_pos) in
		let e_true = (make_bool com.basic true null_pos) in
		let e_hx_ctor = (* this._hx_constructor *)
			let ethis = mk (TConst TThis) t_dynamic null_pos  in
			mk (TField (ethis, FDynamic ctor_method_name)) t_dynamic null_pos
		in

		let change t =
			match t with
			| TClassDecl ({ cl_constructor = Some ({ cf_expr = Some ({ eexpr = TFunction tf_ctor } as ctor_expr) } as cf_ctor) } as cl) ->
				let does_ctor_skipping = try Some (Hashtbl.find does_ctor_skipping cl.cl_path) with Not_found -> None in

				let add_hx_ctor_method () =
					let cf_fun_ctor = mk_field ctor_method_name cf_ctor.cf_type cf_ctor.cf_pos null_pos in
					cf_fun_ctor.cf_expr <- Some (replace_super_call ctor_expr);
					cf_fun_ctor.cf_kind <- Method MethNormal;
					cl.cl_ordered_fields <- cf_fun_ctor :: cl.cl_ordered_fields;
					cl.cl_fields <- PMap.add cf_fun_ctor.cf_name cf_fun_ctor cl.cl_fields;
				in

				let make_hx_ctor_call e_skip_flag = (* this._hx_constructor(a,b,c) *)
					let hxctor_call_args = List.map (fun (v,_) -> make_local v null_pos) tf_ctor.tf_args in
					let hx_ctor_call = mk (TCall (e_hx_ctor, hxctor_call_args)) com.basic.tvoid null_pos in
					if does_ctor_skipping <> None then
						mk (TBlock [
							mk (TBinop (OpAssign, e_skip_flag, e_true)) com.basic.tbool null_pos;
							e_empty_super_call;
							mk (TBinop (OpAssign, e_skip_flag, e_false)) com.basic.tbool null_pos;
							hx_ctor_call
						]) com.basic.tvoid null_pos
					else
						hx_ctor_call
				in

				let make_skip_flag root_cl = (* TopClass._hx_skip_constructor *)
					let e_top = mk (TTypeExpr (TClassDecl root_cl)) t_dynamic null_pos in
					mk (TField (e_top, FDynamic skip_ctor_flag_name)) com.basic.tbool null_pos
				in

				(match (try Some (Hashtbl.find needs_ctor_skipping cl.cl_path) with Not_found -> None) with
				| Some root ->
					add_hx_ctor_method ();

					if does_ctor_skipping = None && cl != root then
						(* for intermediate classes that support skipping but don't do skipping themselves, we can just remove the constructor altogether,
						because the skipping logic is implemented in the parent constructor, and the actual constructor body is moved into _hx_constructor *)
						cf_ctor.cf_expr <- None
					else begin
						let e_skip =
							let e_return = (mk (TReturn None) t_dynamic null_pos) in
							if cl.cl_super = None || (Hashtbl.mem inject_super cl.cl_path)  then
								(* just `return` *)
								e_return
							else
								(* `{ super(); return; }` *)
								mk (TBlock [
									e_empty_super_call;
									e_return;
								]) com.basic.tvoid null_pos
						in

						let e_skip_flag = make_skip_flag root in

						let e_ctor_replaced = { tf_ctor.tf_expr with
							eexpr = TBlock [
								mk (TIf (mk_parent e_skip_flag, e_skip, None)) com.basic.tvoid null_pos;
								make_hx_ctor_call e_skip_flag
							]
						} in

						cf_ctor.cf_expr <- Some { ctor_expr with eexpr = TFunction { tf_ctor with tf_expr = e_ctor_replaced } };
					end;

					if cl == root then begin
						let cf_skip_ctor = mk_field skip_ctor_flag_name com.basic.tbool null_pos null_pos in
						cf_skip_ctor.cf_expr <- Some e_false;
						cl.cl_ordered_statics <- cf_skip_ctor :: cl.cl_ordered_statics;
						cl.cl_statics <- PMap.add cf_skip_ctor.cf_name cf_skip_ctor cl.cl_statics;
					end
				| None ->
					(match does_ctor_skipping with
					| Some root ->

						add_hx_ctor_method ();

						let e_skip_flag = make_skip_flag root in

						let e_ctor_replaced = { tf_ctor.tf_expr with
							eexpr = TBlock [
								make_hx_ctor_call e_skip_flag
							]
						} in
						cf_ctor.cf_expr <- Some { ctor_expr with eexpr = TFunction { tf_ctor with tf_expr = e_ctor_replaced } };

					| None -> ())
				)
			| _ ->
				()
		in
		List.iter change com.types
	end;

	Hashtbl.iter (fun _ cl ->
		match cl with
		| { cl_constructor = Some ({ cf_expr = Some ({ eexpr = TFunction tf } as e_ctor) } as cf_ctor); cl_super = Some (cl_super,_) } ->
			cl.cl_constructor <- Some { cf_ctor with cf_expr = Some { e_ctor with eexpr = TFunction { tf with tf_expr = { tf.tf_expr with eexpr = TBlock [e_empty_super_call; tf.tf_expr] } } } };
		| _ ->
			die()
	) inject_super;
