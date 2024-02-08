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
open Texpr.Builder

(*
	This Module Filter will go through all defined functions in all modules and change them
	so they set all default arguments to be of a Nullable type, and adds the unroll from nullable to
	the not-nullable type in the beginning of the function.
*)

let gen_check basic t nullable_var const pos =
	let needs_cast t1 t2 =
		let is_null t = match t with TAbstract ({a_path = ([],"Null")}, _) -> true | _ -> false in
		(is_null t1) <> (is_null t2)
	in

	let const_t = const.etype in
	let const = if needs_cast t const_t then mk_cast const t pos else const in

	let arg = make_local nullable_var pos in
	let arg = if needs_cast t nullable_var.v_type then mk_cast arg t pos else arg in

	let check = binop Ast.OpEq (make_local nullable_var pos) (null nullable_var.v_type pos) basic.tbool pos in
	mk (TIf (check, const, Some arg)) t pos

let add_opt com block pos (var,opt) =
	match opt with
	| None | Some {eexpr = TConst TNull} ->
		(var,opt)
	| Some ({eexpr = TConst (TString str)} as e) ->
		block := Texpr.set_default com.basic var e pos :: !block;
		(var, opt)
	| Some const ->
		let basic = com.basic in
		let nullable_var = alloc_var var.v_kind var.v_name (basic.tnull var.v_type) pos in
		(* var v = (temp_var == null) ? const : cast temp_var; *)
		let evar = mk (TVar(var, Some(gen_check basic var.v_type nullable_var const pos))) basic.tvoid pos in
		block := evar :: !block;
		(nullable_var, opt)

let rec change_func com cl cf =
	List.iter (change_func com cl) cf.cf_overloads;

	match cf.cf_kind, follow cf.cf_type with
	| _ when has_class_field_flag cf CfPostProcessed ->
		()
	| Var _, _ | Method MethDynamic, _ ->
		()
	| _, TFun(args, ret) ->
		let is_ctor = cf.cf_name = "new" in
		let basic = com.basic in

		let found = ref false in

		let args = ref (List.map (fun (n,opt,t) ->
			(n,opt, if opt then (found := true; basic.tnull t) else t)
		) args) in

		(match !found, cf.cf_expr with
		| true, Some ({ eexpr = TFunction tf } as texpr) ->
			let block = ref [] in
			let tf_args = List.map (add_opt com block tf.tf_expr.epos) tf.tf_args in
			let arg_assoc = List.map2 (fun (v,o) (v2,_) -> v,(v2,o) ) tf.tf_args tf_args in
			let rec extract_super e = match e.eexpr with
				| TBlock (({ eexpr = TCall ({ eexpr = TConst TSuper }, _) } as e2) :: tl) ->
					e2, tl
				| TBlock (hd :: tl) ->
					let e2, tl2 = extract_super hd in
					e2, tl2 @ tl
				| _ ->
					raise Not_found
			in
			let block =
				try
					if not is_ctor then raise Not_found;

					(* issue #2570 *)
					(* check if the class really needs the super as the first statement -
					just to make sure we don't inadvertently break any existing code *)
					let rec check cl =
						if not (Meta.has Meta.HxGen cl.cl_meta) then
							()
						else match cl.cl_super with
							| None ->
								raise Not_found
							| Some (cl, _) ->
								check cl
					in
					check cl;

					let super, tl = extract_super tf.tf_expr in
					(match super.eexpr with
					| TCall ({ eexpr = TConst TSuper } as e1, args) ->
						(* any super argument will be replaced by an inlined version of the check *)
						let found = ref false in
						let rec replace_args e =
							match e.eexpr with
							| TLocal v ->
								(try
									let v2,o = List.assq v arg_assoc in
									let o = match o with
									| None -> raise Not_found
									| Some o -> o
									in
									found := true;
									gen_check com.basic v.v_type v2 o e.epos
								with Not_found -> e)
							| _ ->
								Type.map_expr replace_args e
						in
						let args = List.map replace_args args in
						{ tf.tf_expr with eexpr = TBlock ((if !found then { super with eexpr = TCall (e1, args) } else super) :: !block @ tl) }
					| _ -> Globals.die "" __LOC__)
				with Not_found ->
					Type.concat { tf.tf_expr with eexpr = TBlock !block; etype = basic.tvoid } tf.tf_expr
			in

			args := List.map (fun (v,s) -> (v.v_name, (s <> None), v.v_type)) tf_args;

			let cf_type = TFun (!args, ret) in
			cf.cf_expr <- Some { texpr with
				eexpr = TFunction { tf with
					tf_args = tf_args;
					tf_expr = block
				};
				etype = cf_type
			};
			cf.cf_type <- cf_type

		| _ -> ());
		(if !found then cf.cf_type <- TFun(!args, ret))
	| _, _ -> Globals.die "" __LOC__

let run com md =
	match md with
	| TClassDecl cl ->
		let apply = change_func com cl in
		List.iter apply cl.cl_ordered_fields;
		List.iter apply cl.cl_ordered_statics;
		Option.may apply cl.cl_constructor;
	| _ -> ()
