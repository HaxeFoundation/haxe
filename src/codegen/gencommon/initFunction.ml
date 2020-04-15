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
open Type
open Codegen
open Texpr.Builder
open Gencommon

(*
	This module will take proper care of the init function, by taking off all expressions from static vars and putting them
	in order in the init function.

	It will also initialize dynamic functions, both by putting them in the constructor and in the init function

	depends on:
		(syntax) must run before ExprStatement module
		(ok) must run before OverloadingConstructor module so the constructor can be in the correct place
		(syntax) must run before FunctionToClass module
*)

let ensure_simple_expr com e =
	let rec iter e =
		match e.eexpr with
		| TConst _ | TLocal _ | TArray _ | TBinop _
		| TField _ | TTypeExpr _ | TParenthesis _ | TCast _ | TMeta _
		| TCall _ | TNew _ | TUnop _ | TIdent _ ->
			Type.iter iter e
		| _ ->
			print_endline (debug_expr e);
			com.error "Expression is too complex for a readonly variable initialization" e.epos
	in
	iter e

let handle_override_dynfun acc e this field =
	let v = mk_temp ("super_" ^ field) e.etype in
	v.v_capture <- true;

	let add_expr = ref None in

	let rec loop e =
		match e.eexpr with
		| TField ({ eexpr = TConst TSuper }, f) ->
			let n = field_name f in
			if n <> field then Globals.die();
			if Option.is_none !add_expr then
				add_expr := Some { e with eexpr = TVar(v, Some this) };
			mk_local v e.epos
		| TConst TSuper -> Globals.die()
		| _ -> Type.map_expr loop e
	in
	let e = loop e in

	match !add_expr with
	| None -> e :: acc
	| Some add_expr -> add_expr :: e :: acc

let handle_class com cl =
	let init = match cl.cl_init with
		| None -> []
		| Some i -> [i]
	in
	let init = List.fold_left (fun acc cf ->
		match cf.cf_kind with
			| Var v when Meta.has Meta.ReadOnly cf.cf_meta ->
					if v.v_write <> AccNever && not (Meta.has Meta.CoreApi cl.cl_meta) then com.warning "@:readOnly variable declared without `never` setter modifier" cf.cf_pos;
					(match cf.cf_expr with
					| None -> com.warning "Uninitialized readonly variable" cf.cf_pos
					| Some e -> ensure_simple_expr com e);
					acc
			| Var _
			| Method MethDynamic when Type.is_physical_field cf ->
				(match cf.cf_expr with
				| Some e ->
					(match cf.cf_params with
					| [] ->
						let var = mk (TField (make_static_this cl cf.cf_pos, FStatic(cl,cf))) cf.cf_type cf.cf_pos in
						let ret = binop Ast.OpAssign var e cf.cf_type cf.cf_pos in
						cf.cf_expr <- None;
						ret :: acc
					| _ ->
						let params = List.map (fun _ -> t_dynamic) cf.cf_params in
						let fn = apply_params cf.cf_params params in
						let var = mk (TField (make_static_this cl cf.cf_pos, FStatic(cl,cf))) (fn cf.cf_type) cf.cf_pos in
						let rec change_expr e =
							Type.map_expr_type change_expr fn (fun v -> v.v_type <- fn v.v_type; v) e
						in
						let ret = binop Ast.OpAssign var (change_expr e) (fn cf.cf_type) cf.cf_pos in
						cf.cf_expr <- None;
						ret :: acc)
				| None -> acc)
			| _ -> acc
	) init cl.cl_ordered_statics in
	let init = List.rev init in
	(match init with
	| [] -> cl.cl_init <- None
	| _ -> cl.cl_init <- Some (mk (TBlock init) com.basic.tvoid cl.cl_pos));

	(* FIXME: find a way to tell OverloadingConstructor to execute this code even with empty constructors *)
	let vars, funs = List.fold_left (fun (acc_vars,acc_funs) cf ->
		match cf.cf_kind with
		| Var v when Meta.has Meta.ReadOnly cf.cf_meta ->
				if v.v_write <> AccNever && not (Meta.has Meta.CoreApi cl.cl_meta) then com.warning "@:readOnly variable declared without `never` setter modifier" cf.cf_pos;
				Option.may (ensure_simple_expr com) cf.cf_expr;
				(acc_vars,acc_funs)
		| Var _
		| Method MethDynamic ->
			let is_var = match cf.cf_kind with Var _ -> true | _ -> false in
			(match cf.cf_expr, cf.cf_params with
			| Some e, [] ->
				let var = mk (TField ((mk (TConst TThis) (TInst (cl, List.map snd cl.cl_params)) cf.cf_pos), FInstance(cl, List.map snd cl.cl_params, cf))) cf.cf_type cf.cf_pos in
				let ret = binop Ast.OpAssign var e cf.cf_type cf.cf_pos in
				cf.cf_expr <- None;
				let is_override = List.memq cf cl.cl_overrides in

				if is_override then begin
					cl.cl_ordered_fields <- List.filter (fun f -> f.cf_name <> cf.cf_name) cl.cl_ordered_fields;
					cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
					acc_vars, handle_override_dynfun acc_funs ret var cf.cf_name
				end else if is_var then
					ret :: acc_vars, acc_funs
				else
					acc_vars, ret :: acc_funs
			| Some e, _ ->
				let params = List.map (fun _ -> t_dynamic) cf.cf_params in
				let fn = apply_params cf.cf_params params in
				let var = mk (TField ((mk (TConst TThis) (TInst (cl, List.map snd cl.cl_params)) cf.cf_pos), FInstance(cl, List.map snd cl.cl_params, cf))) cf.cf_type cf.cf_pos in
				let rec change_expr e =
					Type.map_expr_type (change_expr) fn (fun v -> v.v_type <- fn v.v_type; v) e
				in

				let ret = binop Ast.OpAssign var (change_expr e) (fn cf.cf_type) cf.cf_pos in
				cf.cf_expr <- None;
				let is_override = List.memq cf cl.cl_overrides in

				if is_override then begin
					cl.cl_ordered_fields <- List.filter (fun f -> f.cf_name <> cf.cf_name) cl.cl_ordered_fields;
					cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
					acc_vars, handle_override_dynfun acc_funs ret var cf.cf_name
				end else if is_var then
					ret :: acc_vars, acc_funs
				else
					acc_vars, ret :: acc_funs
			| None, _ -> acc_vars,acc_funs)
		| _ -> acc_vars,acc_funs
	) ([],[]) cl.cl_ordered_fields
	in
	(* let vars = List.rev vars in *)
	(* let funs = List.rev funs in *)
	(* see if there is any *)
	(match vars, funs with
	| [], [] -> ()
	| _ ->
		(* if there is, we need to find the constructor *)
		let ctors =
			match cl.cl_constructor with
			| Some ctor ->
				ctor
			| None ->
				try
					let sctor, sup, stl = OverloadingConstructor.prev_ctor cl (List.map snd cl.cl_params) in
					let ctor = OverloadingConstructor.clone_ctors com sctor sup stl cl in
					cl.cl_constructor <- Some ctor;
					ctor
				with Not_found ->
					let ctor = mk_class_field "new" (TFun([], com.basic.tvoid)) false cl.cl_pos (Method MethNormal) [] in
					ctor.cf_expr <- Some
					{
						eexpr = TFunction {
							tf_args = [];
							tf_type = com.basic.tvoid;
							tf_expr = { eexpr = TBlock[]; etype = com.basic.tvoid; epos = cl.cl_pos };
						};
						etype = ctor.cf_type;
						epos = ctor.cf_pos;
					};
					cl.cl_constructor <- Some ctor;
					ctor
		in
		let process ctor =
			let func =
				match ctor.cf_expr with
				| Some ({ eexpr = TFunction tf } as e) ->
					let rec add_fn e =
						match e.eexpr with
						| TBlock(hd :: tl) ->
							(match hd.eexpr with
							| TCall ({ eexpr = TConst TSuper }, _) ->
								let tl_block = { e with eexpr = TBlock(tl) } in
								if not (OverloadingConstructor.descends_from_native_or_skipctor cl) then
									{ e with eexpr = TBlock (vars @ (hd :: (funs @ [tl_block]))) }
								else
									{ e with eexpr = TBlock (hd :: (vars @ funs @ [tl_block])) }
							| TBlock _ ->
								let tl_block = { e with eexpr = TBlock(tl) } in
								{ e with eexpr = TBlock ((add_fn hd) :: [tl_block]) }
							| _ ->
								{ e with eexpr = TBlock (vars @ funs @ [{ e with eexpr = TBlock(hd :: tl) }]) })
						| _ ->
							Type.concat { e with eexpr = TBlock (vars @ funs) } { e with eexpr = TBlock([e]) }
					in
					let tf_expr = add_fn (mk_block tf.tf_expr) in
					{ e with eexpr = TFunction { tf with tf_expr = tf_expr } }
				| _ ->
					Globals.die()
			in
			ctor.cf_expr <- Some func
		in
		List.iter process (ctors :: ctors.cf_overloads)
	)

let mod_filter com md =
	match md with
	| TClassDecl cl when not cl.cl_extern ->
		handle_class com cl
	| _ -> ()

let name = "init_funcs"
let priority = solve_deps name [DBefore OverloadingConstructor.priority]

let configure gen =
	let run = (fun md -> mod_filter gen.gcon md; md) in
	gen.gmodule_filters#add name (PCustom priority) run
