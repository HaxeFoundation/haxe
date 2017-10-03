(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open Ast
open Type
open Common
open Codegen.ExprBuilder

module Utils = struct
	let class_of_module_type mt = match mt with
		| TClassDecl c -> c
		| _ -> failwith ("Not a class: " ^ (s_type_path (t_infos mt).mt_path))

	let find_type com path =
		try
			List.find (fun mt -> match mt with
				| TAbstractDecl _ -> false
				| _ -> (t_infos mt).mt_path = path
			) com.types
		with Not_found ->
			abort (Printf.sprintf "Could not find type %s\n" (s_type_path path)) null_pos

	let mk_static_field c cf p =
			let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
			let ethis = mk (TTypeExpr (TClassDecl c)) ta p in
			let t = monomorphs cf.cf_params cf.cf_type in
			mk (TField (ethis,(FStatic (c,cf)))) t p

	let mk_static_call c cf el p =
		let ef = mk_static_field c cf p in
		let tr = match follow ef.etype with
			| TFun(args,tr) -> tr
			| _ -> assert false
		in
		mk (TCall(ef,el)) tr p

	let resolve_static_field c n =
		try
			PMap.find n c.cl_statics
		with Not_found ->
			failwith (Printf.sprintf "Class %s has no field %s" (s_type_path c.cl_path) n)

	let mk_static_field_2 c n p =
		mk_static_field c (resolve_static_field c n) p

	let mk_static_call_2 c n el p =
		mk_static_call c (resolve_static_field c n) el p
end

module KeywordHandler = struct
	let kwds =
		let h = Hashtbl.create 0 in
		List.iter (fun s -> Hashtbl.add h s ()) [
			"and"; "as"; "assert"; "break"; "class"; "continue"; "def"; "del"; "elif"; "else"; "except"; "exec"; "finally"; "for";
			"from"; "global"; "if"; "import"; "in"; "is"; "lambda"; "not"; "or"; "pass"; " raise"; "return"; "try"; "while";
			"with"; "yield"; "None"; "True"; "False";
		];
		h

	let kwds2 =
		let h = Hashtbl.create 0 in
		List.iter (fun s -> Hashtbl.add h s ()) [
			"len"; "int"; "float"; "list"; "bool"; "str"; "isinstance"; "print"; "min"; "max";
			"hasattr"; "getattr"; "setattr"; "delattr"; "callable"; "type"; "ord"; "chr"; "iter"; "map"; "filter";
			"tuple"; "dict"; "set"; "bytes"; "bytearray"; "self";
		];
		h

	let handle_keywords s =
		let l = String.length s in
		if Hashtbl.mem kwds s then
			"_hx_" ^ s
		(*
			handle special __ underscore behaviour (creates private fields for objects) for fields but only if the field doesn't
			end with at least one underscores like __iter__ because these are special fields
		*)
		else if l > 2 && String.sub s 0 2 = "__" && String.sub s (l - 1) 1 <> "_" then
			"_hx_" ^ s
		else s

	let check_var_declaration v =
		if not (Meta.has Meta.This v.v_meta) then
			if Hashtbl.mem kwds2 v.v_name then v.v_name <- "_hx_" ^ v.v_name
end

module Transformer = struct
	type adjusted_expr = {
		a_expr : texpr;
		a_blocks : texpr list;
		a_next_id : unit -> string;
		a_is_value : bool;
	}

	let como = ref None
	let t_bool = ref t_dynamic
	let t_void = ref t_dynamic
	let t_string = ref t_dynamic
	let t_int = ref t_dynamic
	let c_reflect = ref (fun () -> null_class)

	let init com =
		como := Some com;
		t_bool := com.basic.tbool;
		t_void := com.basic.tvoid;
		t_string := com.basic.tstring;
		t_int := com.basic.tint;
		c_reflect := fun () -> Utils.class_of_module_type (Utils.find_type com ([],"Reflect"))

	and debug_expr e =
		let s_type = Type.s_type (print_context()) in
		let s = Type.s_expr_pretty false "    " false s_type e in
		Printf.printf "%s\n" s

	and debug_expr_with_type e =
		let s_type = Type.s_type (print_context()) in
		let es = Type.s_expr_pretty false "    " false s_type e in
		let t = s_type e.etype in
		Printf.printf "%s : %s\n" es t

	and debug_type t =
		let s_type = Type.s_type (print_context()) in
		let t = s_type t in
		Printf.printf "%s\n" t

	let new_counter () =
		let n = ref (-1) in
		(fun () ->
			incr n;
			Printf.sprintf "_hx_local_%i" !n
		)

	let to_expr ae =
		match ae.a_blocks with
			| [] ->
				ae.a_expr
			| el ->
				match ae.a_expr.eexpr with
					| TBlock el2 ->
						{ ae.a_expr with eexpr = TBlock (el @ el2) }
					| _ ->
						{ ae.a_expr with eexpr = TBlock (el @ [ae.a_expr])}

	let lift_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) e =
		let next_id = match next_id with
			| None ->
				new_counter()
			| Some f ->
				f
		in
		{
			a_expr = e;
			a_blocks = blocks;
			a_next_id = next_id;
			a_is_value = is_value
		}

	let lift_expr1 is_value next_id blocks e =
		lift_expr ~is_value:is_value ~next_id:(Some next_id) ~blocks:blocks e

	let to_tvar ?(capture = false) n t p =
		alloc_var n t p
		(* { v_name = n; v_type = t; v_id = 0; v_capture = capture; v_extra = None; v_meta = [] } *)

	let create_non_local n pos =
		let s = "nonlocal " ^ (KeywordHandler.handle_keywords n) in
		(* TODO: this is a hack... *)
		let id = mk (TLocal (to_tvar "python_Syntax._pythonCode" t_dynamic pos) ) !t_void pos in
		let id2 = mk (TLocal( to_tvar s t_dynamic pos)) !t_void pos in
		mk (TCall(id, [id2])) t_dynamic pos

	let to_tlocal_expr ?(capture = false) n t p =
		mk (TLocal (to_tvar ~capture:capture n t p)) t p

	let check_unification e t = match follow e.etype,follow t with
		| TAnon an1, TAnon an2 ->
			PMap.iter (fun s cf ->
				if not (PMap.mem s an1.a_fields) then an1.a_fields <- PMap.add s cf an1.a_fields
			) an2.a_fields;
			e
		| _ ->
			e

	let dynamic_field_read e s t =
		let e = Utils.mk_static_call_2 ((!c_reflect)()) "field" [e;mk (TConst (TString s)) !t_string e.epos] e.epos in
		{ e with etype = t }

	let dynamic_field_write e1 s e2 =
		Utils.mk_static_call_2 ((!c_reflect)()) "setField" [e1;mk (TConst (TString s)) !t_string e1.epos;e2] e1.epos

	let dynamic_field_read_write next_id e1 s op e2 t =
		let id = next_id() in
		let temp_var = to_tvar id e1.etype e1.epos in
		let temp_var_def = mk (TVar(temp_var,Some e1)) e1.etype e1.epos in
		let temp_local = mk (TLocal temp_var) e1.etype e1.epos in
		let e_field = dynamic_field_read temp_local s t in
		let e_op = mk (TBinop(op,e_field,e2)) e_field.etype e_field.epos in
		let e_set_field = dynamic_field_write temp_local s e_op in
		mk (TBlock [
			temp_var_def;
			e_set_field;
		]) e_set_field.etype e_set_field.epos

	let add_non_locals_to_func e = match e.eexpr with
		| TFunction tf ->
			let cur = ref PMap.empty in
			let save () =
				let prev = !cur in
				(fun () ->
					cur := prev
				)
			in
			let declare v =
				cur := PMap.add v.v_id v !cur;
			in
			List.iter (fun (v,_) -> declare v) tf.tf_args;
			let non_locals = Hashtbl.create 0 in
			let rec it e = match e.eexpr with
				| TVar(v,e1) ->
					begin match e1 with
						| Some e ->
							maybe_continue e
						| None ->
							()
					end;
					declare v;
				| TTry(e1,catches) ->
					it e1;
					List.iter (fun (v,e) ->
						let restore = save() in
						declare v;
						it e;
						restore()
					) catches;
				| TBinop( (OpAssign | OpAssignOp(_)), { eexpr = TLocal v }, e2) ->
					if not (PMap.mem v.v_id !cur) then
						Hashtbl.add non_locals v.v_id v;
					maybe_continue e2;
				| TFunction _ ->
					()
				| _ ->
					Type.iter it e
			and maybe_continue e = match e.eexpr with
				| TFunction _ ->
					()
				| _ ->
					it e
			in
			it tf.tf_expr;
			let el = Hashtbl.fold (fun k v acc ->
				(create_non_local v.v_name e.epos) :: acc
			) non_locals [] in
			let el = tf.tf_expr :: el in
			let tf = { tf with tf_expr = { tf.tf_expr with eexpr = TBlock(List.rev el)}} in
			{e with eexpr = TFunction tf}
		| _ ->
			assert false

	let rec transform_function tf ae is_value =
		let p = tf.tf_expr.epos in
		let assigns = List.fold_left (fun acc (v,value) ->
			KeywordHandler.check_var_declaration v;
			match value with
				| None | Some TNull ->
					acc
				| Some ct ->
					let a_local = mk (TLocal v) v.v_type p in
					let a_null = mk (TConst TNull) v.v_type p in
					let a_cmp = mk (TBinop(OpEq,a_local,a_null)) !t_bool p in
					let a_value = mk (TConst(ct)) v.v_type p in
					let a_assign = mk (TBinop(OpAssign,a_local,a_value)) v.v_type p in
					let a_if = mk (TIf(a_cmp,a_assign,None)) !t_void p in
					a_if :: acc
			) [] tf.tf_args
		in
		let body = match assigns with
			| [] ->
				tf.tf_expr
			| _ ->
				let eb = mk (TBlock (List.rev assigns)) t_dynamic p in
				Type.concat eb tf.tf_expr
		in
		let e1 = to_expr (transform_expr ~next_id:(Some ae.a_next_id) body) in
		let fn = mk (TFunction({
			tf_expr = e1;
			tf_args = tf.tf_args;
			tf_type = tf.tf_type;
		})) ae.a_expr.etype p in
		let fn = add_non_locals_to_func fn in
		if is_value then begin
			let new_name = ae.a_next_id() in
			let new_var = alloc_var new_name tf.tf_type p in
			let new_local = mk (TLocal new_var) fn.etype p in
			let def = mk (TVar(new_var,Some fn)) fn.etype p in
			lift_expr1 false ae.a_next_id [def] new_local
		end else
			lift_expr fn

	and transform_var_expr ae eo v =
		KeywordHandler.check_var_declaration v;
		let b,new_expr = match eo with
			| None ->
				[],None
			| Some e1 ->
				let f = transform_expr1 true ae.a_next_id [] e1 in
				let b = f.a_blocks in
				b,Some(f.a_expr)
		in
		let e = mk (TVar(v,new_expr)) ae.a_expr.etype ae.a_expr.epos in
		lift_expr ~next_id:(Some ae.a_next_id) ~blocks:b e

	and transform_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) (e : texpr) : adjusted_expr =
		transform1 (lift_expr ~is_value ~next_id ~blocks e)

	and transform_expr1 is_value next_id blocks e =
		transform_expr ~is_value ~next_id:(Some next_id) ~blocks e

	and transform_exprs_to_block el tb is_value p next_id =
		match el with
			| [e] ->
				transform_expr ~is_value ~next_id:(Some next_id) e
			| _ ->
				let size = List.length el in
				let res = DynArray.create () in
				ExtList.List.iteri (fun i e ->
					(* this removes len(x) calls which are reproduced by the inlined return
					   of Array.push even if the value is not used *)
					let is_removable_statement e = (not is_value || i < size-1) &&
						match e.eexpr with
						| TField(_, FInstance({cl_path = [],"list"},_,{ cf_name = "length" })) -> true
						| _ -> false
					in
					if not (is_removable_statement e) then
						let ae = transform_expr ~is_value ~next_id:(Some next_id) e in
						List.iter (DynArray.add res) ae.a_blocks;
						DynArray.add res ae.a_expr
					else
						()
				) el;
				lift_expr (mk (TBlock (DynArray.to_list res)) tb p)

	and transform_switch ae is_value e1 cases edef =
		let case_functions = ref [] in
		let case_to_if (el,e) eelse =
			let val_reversed = List.rev el in
			let mk_eq e = mk (TBinop(OpEq,e1,e)) !t_bool (punion e1.epos e.epos) in
			let cond = match val_reversed with
				| [] ->
					assert false
				| [e] ->
					mk_eq e
				| e :: el ->
					List.fold_left (fun eelse e -> mk (TBinop(OpBoolOr,eelse,mk_eq e)) !t_bool (punion eelse.epos e.epos)) (mk_eq e) el
			in
			let eif = if is_value then begin
				let name = ae.a_next_id() in
				let func = exprs_to_func [e] name ae in
				case_functions := !case_functions @ func.a_blocks;
				let call = func.a_expr in
				mk (TIf(cond,call,eelse)) ae.a_expr.etype ae.a_expr.epos
			end else
				mk (TIf(cond,e,eelse)) ae.a_expr.etype e.epos
			in
			eif
		in
		let rev_cases = List.rev cases in
		let edef = Some (match edef with
			| None ->
				mk (TBlock []) ae.a_expr.etype ae.a_expr.epos
			| Some e ->
				e)
		in
		let res = match rev_cases,edef with
			| [],Some edef ->
				edef
			| [],None ->
				(* I don't think that can happen? *)
				assert false
			| [case],_ ->
				case_to_if case edef
			| case :: cases,_ ->
				List.fold_left (fun acc case -> case_to_if case (Some acc)) (case_to_if case edef) cases
		in
		let res = if is_value then
			mk (TBlock ((List.rev (res :: !case_functions)))) res.etype res.epos
		else
			res
		in
		forward_transform res ae

	and transform_string_switch ae is_value e1 cases edef =
		let length_map = Hashtbl.create 0 in
		List.iter (fun (el,e) ->
			List.iter (fun es ->
				match es.eexpr with
				| TConst (TString s) ->
					let l = UTF8.length s in
					let sl = try
						Hashtbl.find length_map l
					with Not_found ->
						let sl = ref [] in
						Hashtbl.replace length_map l sl;
						sl
					in
					sl := ([es],e) :: !sl;
				| _ ->
					()
			) el
		) cases;
		if Hashtbl.length length_map < 2 then
			transform_switch ae is_value e1 cases edef
		else
			let mk_eq e1 e2 = mk (TBinop(OpEq,e1,e2)) !t_bool (punion e1.epos e2.epos) in
			let mk_or e1 e2 = mk (TBinop(OpOr,e1,e2)) !t_bool (punion e1.epos e2.epos) in
			let mk_if (el,e) eo =
				let eif = List.fold_left (fun eacc e -> mk_or eacc (mk_eq e1 e)) (mk_eq e1 (List.hd el)) (List.tl el) in
				mk (TIf(Codegen.mk_parent eif,e,eo)) e.etype e.epos
			in
			let cases = Hashtbl.fold (fun i el acc ->
				let eint = mk (TConst (TInt (Int32.of_int i))) !t_int e1.epos in
				let fs = match List.fold_left (fun eacc ec -> Some (mk_if ec eacc)) edef !el with Some e -> e | None -> assert false in
				([eint],fs) :: acc
			) length_map [] in
			let c_string = match !t_string with TInst(c,_) -> c | _ -> assert false in
			let cf_length = PMap.find "length" c_string.cl_fields in
			let ef = mk (TField(e1,FInstance(c_string,[],cf_length))) !t_int e1.epos in
			let res_var = alloc_var (ae.a_next_id()) ef.etype ef.epos in
			let res_local = {ef with eexpr = TLocal res_var} in
			let var_expr = {ef with eexpr = TVar(res_var,Some ef)} in
			let e = mk (TBlock [
				var_expr;
				mk (TSwitch(res_local,cases,edef)) ae.a_expr.etype e1.epos
			]) ae.a_expr.etype e1.epos in
			forward_transform e ae

	and transform_op_assign_op ae e1 op one is_value post =
		let e1_ = transform_expr e1 ~is_value:true ~next_id:(Some ae.a_next_id) in
		let handle_as_local temp_local =
			let ex = ae.a_expr in
			let res_var = alloc_var (ae.a_next_id()) ex.etype ex.epos in
			let res_local = {ex with eexpr = TLocal res_var} in
			let plus = {ex with eexpr = TBinop(op,temp_local,one)} in
			let var_expr = {ex with eexpr = TVar(res_var,Some temp_local)} in
			let assign_expr = {ex with eexpr = TBinop(OpAssign,e1_.a_expr,plus)} in
			let blocks = if post then
				[var_expr;assign_expr;res_local]
			else
				[assign_expr;temp_local]
			in
			(* TODO: block is ignored in the else case? *)
			let block = e1_.a_blocks @ blocks in
			if is_value then begin
				let f = exprs_to_func block (ae.a_next_id()) ae in
				lift_expr f.a_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:f.a_blocks
			end else begin
				let block = e1_.a_blocks @ [assign_expr] in
				transform_exprs_to_block block ex.etype false ex.epos ae.a_next_id
			end
		in
		match e1_.a_expr.eexpr with
			| TArray({eexpr = TLocal _},{eexpr = TLocal _})
			| TField({eexpr = TLocal _},_)
			| TLocal _ ->
				handle_as_local e1_.a_expr
			| TArray(e1,e2) ->
				let id = ae.a_next_id() in
				let temp_var_l = alloc_var id e1.etype e1.epos in
				let temp_local_l = {e1 with eexpr = TLocal temp_var_l} in
				let temp_var_l = {e1 with eexpr = TVar(temp_var_l,Some e1)} in

				let id = ae.a_next_id() in
				let temp_var_r = alloc_var id e2.etype e2.epos in
				let temp_local_r = {e2 with eexpr = TLocal temp_var_r} in
				let temp_var_r = {e2 with eexpr = TVar(temp_var_r,Some e2)} in

				let id = ae.a_next_id() in
				let temp_var = alloc_var id e1_.a_expr.etype e1_.a_expr.epos in
				let temp_local = {e1_.a_expr with eexpr = TLocal temp_var} in
				let temp_var_expr = {e1_.a_expr with eexpr = TArray(temp_local_l,temp_local_r)} in
				let temp_var = {e1_.a_expr with eexpr = TVar(temp_var,Some temp_var_expr)} in

				let plus = {ae.a_expr with eexpr = TBinop(op,temp_local,one)} in
				let assign_expr = {ae.a_expr with eexpr = TBinop(OpAssign,temp_var_expr,plus)} in
				let block = e1_.a_blocks @ [temp_var_l;temp_var_r;temp_var;assign_expr;if post then temp_local else temp_var_expr] in
				if is_value then begin
					let f = exprs_to_func block (ae.a_next_id()) ae in
					lift_expr f.a_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:f.a_blocks
				end else
					transform_exprs_to_block block ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
			| TField(e1,fa) ->
				let temp_var_l = alloc_var (ae.a_next_id()) e1.etype e1.epos in
				let temp_local_l = {e1 with eexpr = TLocal temp_var_l} in
				let temp_var_l = {e1 with eexpr = TVar(temp_var_l,Some e1)} in

				let temp_var = alloc_var (ae.a_next_id()) e1_.a_expr.etype e1_.a_expr.epos in
				let temp_local = {e1_.a_expr with eexpr = TLocal temp_var} in
				let temp_var_expr = {e1_.a_expr with eexpr = TField(temp_local_l,fa)} in
				let temp_var = {e1_.a_expr with eexpr = TVar(temp_var,Some temp_var_expr)} in

				let plus = {ae.a_expr with eexpr = TBinop(op,temp_local,one)} in
				let assign_expr = {ae.a_expr with eexpr = TBinop(OpAssign,temp_var_expr,plus)} in
				let block = e1_.a_blocks @ [temp_var_l;temp_var;assign_expr;if post then temp_local else temp_var_expr] in
				if is_value then begin
					let f = exprs_to_func block (ae.a_next_id()) ae in
					lift_expr f.a_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:f.a_blocks
				end else
					transform_exprs_to_block block ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
			| _ ->
				debug_expr e1_.a_expr;
				assert false

	and var_to_treturn_expr ?(capture = false) n t p =
		let x = mk (TLocal (to_tvar ~capture:capture n t p)) t p in
		mk (TReturn (Some x)) t p

	and exprs_to_func exprs name base =
		let convert_return_expr (expr:texpr) =
			match expr.eexpr with
			| TWhile(_,_,_) ->
				let ret = { expr with eexpr = TReturn (None) } in
				[expr; ret]
			| TFunction(f) ->
				let ret = var_to_treturn_expr name f.tf_type f.tf_expr.epos in
				[expr;ret]
			| TBinop(OpAssign, l, r) ->
				let r = { l with eexpr = TReturn(Some l) } in
				[expr; r]
			| x ->
				let ret_expr = { expr with eexpr = TReturn( Some(expr) )} in
				[ret_expr]
		in
		let def =
			(let ex = match exprs with
			| [] -> assert false
			| [x] ->
				(let exs = convert_return_expr x in
				match exs with
				| [] -> assert false
				| [x] -> x
				| x ->
					match List.rev x with
					| x::xs ->
						mk (TBlock exs) x.etype base.a_expr.epos
					| _ -> assert false)

			| x ->
				match List.rev x with
				| x::xs ->
					(let ret = x in
					let tail = List.rev xs in
					let block = tail @ (convert_return_expr ret) in
					match List.rev block with
					| x::_ ->
						mk (TBlock block) x.etype base.a_expr.epos
					| _ -> assert false)
				| _ -> assert false
			in
			let f1 = { tf_args = []; tf_type = TFun([],ex.etype); tf_expr = ex} in
			let fexpr = mk (TFunction f1) ex.etype ex.epos in
			let fvar = to_tvar name fexpr.etype fexpr.epos in
			let f = add_non_locals_to_func fexpr in
			let assign = { ex with eexpr = TVar(fvar, Some(f))} in
			let call_expr = (mk (TLocal fvar) fexpr.etype ex.epos ) in
			let substitute = mk (TCall(call_expr, [])) ex.etype ex.epos in
			lift_expr ~blocks:[assign] substitute)
		in
		match exprs with
		| [{ eexpr = TFunction({ tf_args = []} as f) } as x] ->
			let l = to_tlocal_expr name f.tf_type f.tf_expr.epos in
			let substitute = mk (TCall(l, [])) f.tf_type f.tf_expr.epos in
			lift_expr ~blocks:[x] substitute
		| _ -> def

	and transform_call is_value e params ae =
		let trans is_value blocks e = transform_expr1 is_value ae.a_next_id blocks e in
		let trans1 e params =
			let e = trans true [] e in
			let blocks = e.a_blocks @ (List.flatten (List.map (fun (p) -> p.a_blocks) params)) in
			let params = List.map (fun (p) -> p.a_expr) params in
			let e = { ae.a_expr with eexpr = TCall(e.a_expr, params) } in
			lift_expr ~blocks:blocks e
		in
		match e, params with
		(* the foreach block should not be handled as a value *)
		| ({ eexpr = TField(_, FStatic({cl_path = ["python";],"Syntax"},{ cf_name = "_foreach" }))} as e, [e1;e2;e3]) ->
			trans1 e [trans true [] e1; trans true [] e2; trans false [] e3]
		| (e, params) ->
			trans1 e (List.map (trans true []) params)


	and transform1 ae : adjusted_expr =

		let trans is_value blocks e = transform_expr1 is_value ae.a_next_id blocks e in
		let lift is_value blocks e = lift_expr1 is_value ae.a_next_id blocks e in
		let a_expr = ae.a_expr in
		match ae.a_is_value,ae.a_expr.eexpr with
		| (is_value,TBlock [x]) ->
			trans is_value [] x
		| (false,TBlock []) ->
			lift_expr a_expr
		| (true,TBlock []) ->
			lift_expr (mk (TConst TNull) ae.a_expr.etype ae.a_expr.epos)
		| (false,TBlock el) ->
			transform_exprs_to_block el ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
		| (true,TBlock el) ->
			let name = ae.a_next_id() in
			let block,tr = match List.rev el with
				| e :: el ->
					List.rev ((mk (TReturn (Some e)) t_dynamic e.epos) :: el),e.etype
				| [] ->
					assert false
			in
			let my_block = transform_exprs_to_block block tr false ae.a_expr.epos ae.a_next_id in
			let fn = mk (TFunction {
				tf_args = [];
				tf_type = tr;
				tf_expr = my_block.a_expr;
			}) ae.a_expr.etype ae.a_expr.epos in
			let t_var = alloc_var name ae.a_expr.etype ae.a_expr.epos in
			let f = add_non_locals_to_func fn in
			let fn_assign = mk (TVar (t_var,Some f)) ae.a_expr.etype ae.a_expr.epos in
			let ev = mk (TLocal t_var) ae.a_expr.etype ae.a_expr.epos in
			let substitute = mk (TCall(ev,[])) ae.a_expr.etype ae.a_expr.epos in
			lift_expr ~blocks:[fn_assign] substitute
		| (is_value,TFunction(f)) ->
			transform_function f ae is_value
		| (_,TVar(v,None)) ->

			transform_var_expr ae None v

 		| (false, TVar(v,Some({ eexpr = TUnop((Increment | Decrement as unop),post_fix,({eexpr = TLocal _ | TField({eexpr = TConst TThis},_)} as ve))} as e1))) ->
			let one = {e1 with eexpr = TConst (TInt (Int32.of_int 1))} in
			let op = if unop = Increment then OpAdd else OpSub in
			let inc = {e1 with eexpr = TBinop(op,ve,one)} in
			let inc_assign = {e1 with eexpr = TBinop(OpAssign,ve,inc)} in
			let var_assign = {e1 with eexpr = TVar(v,Some ve)} in
			if post_fix = Postfix then
				lift true [var_assign] inc_assign
			else
				lift true [inc_assign] var_assign
		| (_,TVar(v,eo)) ->
			transform_var_expr ae eo v
		| (_,TFor(v,e1,e2)) ->
			let a1 = trans true [] e1 in
			let a2 = to_expr (trans false [] e2) in

			let name = (ae.a_next_id ()) in
			let t_var = alloc_var name e1.etype e1.epos in

			let ev = make_local t_var e1.epos in
			let ehasnext = mk (TField(ev,quick_field e1.etype "hasNext")) (tfun [] (!t_bool) ) e1.epos in
			let ehasnext = mk (TCall(ehasnext,[])) ehasnext.etype ehasnext.epos in

			let enext = mk (TField(ev,quick_field e1.etype "next")) (tfun [] v.v_type) e1.epos in
			let enext = mk (TCall(enext,[])) v.v_type e1.epos in

			let var_assign = mk (TVar (v,Some enext)) v.v_type a_expr.epos in

			let ebody = Type.concat var_assign (a2) in

			let var_decl = mk (TVar (t_var,Some a1.a_expr)) (!t_void) e1.epos in
			let twhile = mk (TWhile((mk (TParenthesis ehasnext) ehasnext.etype ehasnext.epos),ebody,NormalWhile)) (!t_void) e1.epos in

			let blocks = a1.a_blocks @ [var_decl] in

			lift_expr ~blocks: blocks twhile
		| (_,TReturn None) ->
			ae
		| (_,TReturn (Some ({eexpr = TFunction f} as ef))) ->
			let n = ae.a_next_id() in
			let e1 = to_expr (trans false [] f.tf_expr) in
			let f = mk (TFunction {
				tf_args = f.tf_args;
				tf_type = f.tf_type;
				tf_expr = e1;
			}) ef.etype ef.epos in
			let f1 = add_non_locals_to_func f in
			let var_n = alloc_var n ef.etype ef.epos in
			let f1_assign = mk (TVar(var_n,Some f1)) !t_void f1.epos in
			let var_local = mk (TLocal var_n) ef.etype f1.epos in
			let er = mk (TReturn (Some var_local)) t_dynamic  ae.a_expr.epos in
			lift true [f1_assign] er

		| (_,TReturn Some(x)) ->
			let x1 = trans true [] x in
			(match x1.a_blocks with
				| [] ->
					lift true [] { ae.a_expr with eexpr = TReturn(Some x1.a_expr) }
				| blocks ->
					let f = exprs_to_func (blocks @ [x1.a_expr]) (ae.a_next_id()) ae in
					lift true f.a_blocks {a_expr with eexpr = TReturn (Some f.a_expr)})
		| (_, TParenthesis(e1)) ->
			let e1 = trans true [] e1 in
			let p = { ae.a_expr with eexpr = TParenthesis(e1.a_expr)} in
			lift true e1.a_blocks p
		| (_, TEnumParameter(e1,ef,i)) ->
			let e1 = trans true [] e1 in
			let p = { ae.a_expr with eexpr = TEnumParameter(e1.a_expr,ef,i)} in
			lift true e1.a_blocks p
		| (_, TEnumIndex e1) ->
			let e1 = trans true [] e1 in
			let p = { ae.a_expr with eexpr = TEnumIndex e1.a_expr } in
			lift true e1.a_blocks p
		| (true, TIf(econd, eif, eelse)) ->
			(let econd1 = trans true [] econd in
			let eif1 = trans true [] eif in
			let eelse1 = match eelse with
				| Some x -> Some(trans true [] x)
				| None -> None
			in
			let blocks = [] in
			let eif2, blocks =
				match eif1.a_blocks with
				| [] -> eif1.a_expr, blocks
				| x ->
					let regular =
						let fname = eif1.a_next_id () in
						let f = exprs_to_func (List.append eif1.a_blocks [eif1.a_expr]) fname ae in
						f.a_expr, List.append blocks f.a_blocks
					in
					match eif1.a_blocks with
					| [{ eexpr = TVar(_, Some({ eexpr = TFunction(_)}))} as b] ->
						eif1.a_expr, List.append blocks [b]
					| _ -> regular
			in
			let eelse2, blocks =
				match eelse1 with
				| None -> None, blocks
				| Some({ a_blocks = []} as x) -> Some(x.a_expr), blocks
				| Some({ a_blocks = b} as eelse1) ->
					let regular =
						let fname = eelse1.a_next_id () in
						let f = exprs_to_func (List.append eelse1.a_blocks [eelse1.a_expr]) fname ae in
						Some(f.a_expr), List.append blocks f.a_blocks
					in
					match b with
					| [{ eexpr = TVar(_, Some({ eexpr = TFunction(f)}))} as b] ->
						Some(eelse1.a_expr), List.append blocks [b]
					| _ -> regular
			in
			let blocks = List.append econd1.a_blocks blocks in
			let new_if = { ae.a_expr with eexpr = TIf(econd1.a_expr, eif2, eelse2) } in
			match blocks with
			| [] ->
				let meta = Meta.Custom(":ternaryIf"), [], ae.a_expr.epos in
				let ternary = { ae.a_expr with eexpr = TMeta(meta, new_if) } in
				lift_expr ~blocks:blocks ternary
			| b ->
				let f = exprs_to_func (List.append blocks [new_if]) (ae.a_next_id ()) ae in
				lift_expr ~blocks:f.a_blocks f.a_expr)
		| (false, TIf(econd, eif, eelse)) ->
			let econd = trans true [] econd in
			let eif = to_expr (trans false [] eif) in
			let eelse = match eelse with
			| Some(x) -> Some(to_expr (trans false [] x))
			| None -> None
			in
			let new_if = { ae.a_expr with eexpr = TIf(econd.a_expr, eif, eelse) } in
			lift false econd.a_blocks new_if
		| (false, TWhile(econd, e1, NormalWhile)) ->
			let econd1 = trans true [] econd in
			let e11 = to_expr (trans false [] e1) in
			let new_while = mk (TWhile(econd1.a_expr,e11,NormalWhile)) a_expr.etype a_expr.epos in
			lift false econd1.a_blocks new_while
		| (true, TWhile(econd, ebody, NormalWhile)) ->
			let econd = trans true [] econd in
			let ebody = to_expr (trans false [] ebody) in
			let ewhile = { ae.a_expr with eexpr = TWhile(econd.a_expr, ebody, NormalWhile) } in
			let eval = { ae.a_expr with eexpr = TConst(TNull) } in
			let f = exprs_to_func (List.append econd.a_blocks [ewhile; eval]) (ae.a_next_id ()) ae in
			lift true f.a_blocks f.a_expr
		| (false, TWhile(econd, ebody, DoWhile)) ->
			let not_expr = { econd with eexpr = TUnop(Not, Prefix, econd) } in
			let break_expr = mk TBreak !t_void econd.epos in
			let if_expr = mk (TIf(not_expr, break_expr, None)) (!t_void) econd.epos in
			let new_e = match ebody.eexpr with
				| TBlock(exprs) -> { econd with eexpr = TBlock( List.append exprs [if_expr]) }
				| _ -> { econd with eexpr = TBlock( List.append [ebody] [if_expr]) }
			in
			let true_expr = mk (TConst(TBool(true))) econd.etype ae.a_expr.epos in
			let new_expr = { ae.a_expr with eexpr = TWhile( true_expr, new_e, NormalWhile) } in
			forward_transform new_expr ae

		| (is_value, TSwitch(e, cases, edef)) ->
			begin match follow e.etype with
				| TInst({cl_path = [],"str"},_) ->
					transform_string_switch ae is_value e cases edef
				| _ ->
					transform_switch ae is_value e cases edef
			end
		(* anon field access on optional params *)
		| (is_value, TField(e,FAnon cf)) when Meta.has Meta.Optional cf.cf_meta ->
			let e = dynamic_field_read e cf.cf_name ae.a_expr.etype in
			transform_expr ~is_value:is_value e
		| (is_value, TBinop(OpAssign,{eexpr = TField(e1,FAnon cf)},e2)) when Meta.has Meta.Optional cf.cf_meta ->
			let e = dynamic_field_write e1 cf.cf_name e2 in
			transform_expr ~is_value:is_value e
		| (is_value, TBinop(OpAssignOp op,{eexpr = TField(e1,FAnon cf); etype = t},e2)) when Meta.has Meta.Optional cf.cf_meta ->
			let e = dynamic_field_read_write ae.a_next_id e1 cf.cf_name op e2 t in
			transform_expr ~is_value:is_value e
		(* TODO we need to deal with Increment, Decrement too!

		| (_, TUnop( (Increment | Decrement) as unop, op,{eexpr = TField(e1,FAnon cf)})) when Meta.has Meta.Optional cf.cf_meta  ->
			let  = dynamic_field_read e cf.cf_name in

			let e = dynamic_field_read_write_unop ae.a_next_id e1 cf.cf_name unop op in
			Printf.printf "dyn read write\n";
			transform_expr e
		*)
		(*
			anon field access with non optional members like iterator, length, split must be handled too, we need to Reflect on them too when it's a runtime method
		*)
		| (is_value, TUnop( (Increment | Decrement) as unop, op, e)) ->
			let one = { ae.a_expr with eexpr = TConst(TInt(Int32.of_int(1)))} in
			let is_postfix = match op with
			| Postfix -> true
			| Prefix -> false in
			let op = match unop with
			| Increment -> OpAdd
			| Decrement -> OpSub
			| _ -> assert false in
			transform_op_assign_op ae e op one is_value is_postfix
		| (_, TUnop(op, Prefix, e)) ->
			let e1 = trans true [] e in
			let r = { a_expr with eexpr = TUnop(op, Prefix, e1.a_expr) } in
			lift_expr ~blocks:e1.a_blocks r

		| (is_value, TField(e,FDynamic s)) ->
			let e = dynamic_field_read e s ae.a_expr.etype in
			transform_expr ~is_value:is_value e
		| (is_value, TBinop(OpAssign,{eexpr = TField(e1,FDynamic s)},e2)) ->
			let e = dynamic_field_write e1 s e2 in
			transform_expr ~is_value:is_value e
		| (is_value, TBinop(OpAssignOp op,{eexpr = TField(e1,FDynamic s); etype = t},e2)) ->
			let e = dynamic_field_read_write ae.a_next_id e1 s op e2 t in
			transform_expr ~is_value:is_value e
		| (is_value, TBinop(OpAssign, left, right))->
			(let left = trans true [] left in
			let right = trans true [] right in
			let r = { a_expr with eexpr = TBinop(OpAssign, left.a_expr, right.a_expr)} in
			if is_value then
				(let blocks = List.concat [left.a_blocks; right.a_blocks; [r]] in
				let f = exprs_to_func blocks (ae.a_next_id ()) ae in
				lift true f.a_blocks f.a_expr)
			else
				lift false (List.append left.a_blocks right.a_blocks) r)
		| (is_value, TBinop(OpAssignOp(x), left, right)) ->
			let right = trans true [] right in
			let v = right.a_expr in
			let res = transform_op_assign_op ae left x v is_value false in
			lift true (List.append right.a_blocks res.a_blocks) res.a_expr
		| (_, TBinop(op, left, right))->
			(let left = trans true [] left in
			let right = trans true [] right in
			let r = { a_expr with eexpr = TBinop(op, left.a_expr, right.a_expr)} in
			lift false (List.append left.a_blocks right.a_blocks) r)

		| (true, TThrow(x)) ->
			let block = TBlock([a_expr; { a_expr with eexpr = TConst(TNull) }]) in
			let r = { a_expr with eexpr = block } in
			forward_transform r ae
		| (false, TThrow(x)) ->
			let x = trans true [] x in
			let r = { a_expr with eexpr = TThrow(x.a_expr)} in
			lift false x.a_blocks r
		| (_, TNew(c, tp, params)) ->
			let params = List.map (trans true []) params in
			let blocks = List.flatten (List.map (fun (p) -> p.a_blocks) params) in
			let params = List.map (fun (p) -> p.a_expr) params in
			let e = { a_expr with eexpr = TNew(c, tp, params) } in
			lift false blocks e
		| (is_value, TCall(e,params)) ->
			transform_call is_value e params ae
		| (_, TArray(e1, e2)) ->
			let e1 = trans true [] e1 in
			let e2 = trans true [] e2 in
			let r = { a_expr with eexpr = TArray(e1.a_expr, e2.a_expr)} in
			let blocks = List.append e1.a_blocks e2.a_blocks in
			lift_expr ~blocks:blocks r
		| (false, TTry(etry, catches)) ->
			let etry = trans false [] etry in
			let catches = List.map (fun(v,e) -> KeywordHandler.check_var_declaration v; v, trans false [] e) catches in
			let blocks = List.flatten (List.map (fun (_,e) -> e.a_blocks) catches) in
			let catches = List.map (fun(v,e) -> v, e.a_expr) catches in
			let r = { a_expr with eexpr = TTry(etry.a_expr, catches)} in
			let blocks = List.append etry.a_blocks blocks in
			lift false blocks r
		| (true, TTry(etry, catches)) ->

			let id = ae.a_next_id () in
			let temp_var = to_tvar id a_expr.etype a_expr.epos in
			let temp_var_def = { a_expr with eexpr = TVar(temp_var, None) } in
			let temp_local = { a_expr with eexpr = TLocal(temp_var)} in
			let mk_temp_assign right = { a_expr with eexpr = TBinop(OpAssign, temp_local, right)} in
			let etry = mk_temp_assign etry in
			let catches = List.map (fun (v,e)-> KeywordHandler.check_var_declaration v; v, mk_temp_assign e) catches in
			let new_try = { a_expr with eexpr = TTry(etry, catches)} in
			let block = [temp_var_def; new_try; temp_local] in
			let new_block = { a_expr with eexpr = TBlock(block)} in
			forward_transform new_block ae
		| (_, TObjectDecl(fields)) ->
			let fields = List.map (fun (name,ex) -> name, trans true [] ex) fields in
			let blocks = List.flatten (List.map (fun (_,ex) -> ex.a_blocks) fields) in
			let fields = List.map (fun (name,ex) -> name, ex.a_expr) fields in
			let r = { a_expr with eexpr = (TObjectDecl(fields) )} in
			lift_expr ~blocks r
		| (_, TArrayDecl(values)) ->
			let values = List.map (trans true []) values in
			let blocks = List.flatten (List.map (fun (v) -> v.a_blocks) values) in
			let exprs = List.map (fun (v) -> v.a_expr) values in
			let r = { a_expr with eexpr = TArrayDecl exprs } in
			lift_expr ~blocks:blocks r
		| (is_value, TCast(e1,Some mt)) ->
			let e = Codegen.default_cast ~vtmp:(ae.a_next_id()) (match !como with Some com -> com | None -> assert false) e1 mt ae.a_expr.etype ae.a_expr.epos in
			transform_expr ~is_value:is_value e
		| (is_value, TCast(e,None)) ->
			let e = trans is_value [] e in
			let r = { a_expr with eexpr = TCast(e.a_expr, None)} in
			lift_expr ~blocks:e.a_blocks r
		| (_, TField(e,f)) ->
			let e = trans true [] e in
			let r = { a_expr with eexpr = TField(e.a_expr, f) } in
			lift_expr ~blocks:e.a_blocks r
		| (is_value, TMeta(m, e)) ->
			let e = trans is_value [] e in
			let r = { a_expr with eexpr = TMeta(m, e.a_expr); etype = e.a_expr.etype } in
			lift_expr ~blocks:e.a_blocks r
		| ( _, TLocal _ ) -> lift_expr a_expr

		| ( _, TConst _ ) -> lift_expr a_expr
		| ( _, TTypeExpr _ ) -> lift_expr a_expr
		| ( _, TUnop _ ) -> assert false
		| ( true, TWhile(econd, ebody, DoWhile) ) ->
			let new_expr = trans false [] a_expr in
			let f = exprs_to_func (new_expr.a_blocks @ [new_expr.a_expr]) (ae.a_next_id()) ae in
			lift_expr ~is_value:true ~blocks:f.a_blocks f.a_expr

		| ( _, TBreak ) | ( _, TContinue ) | ( _, TIdent _) ->
			lift_expr a_expr

	and transform e =
		to_expr (transform1 (lift_expr e))

	and forward_transform e base =
		transform1 (lift_expr1 base.a_is_value base.a_next_id base.a_blocks e)

	let transform_to_value e =
		to_expr (transform1 (lift_expr e ~is_value:true))

end

module Printer = struct

	type print_context = {
		pc_indent : string;
		pc_next_anon_func : unit -> string;
		pc_debug : bool;
		pc_com : Common.context;
	}

	let has_feature pctx = Common.has_feature pctx.pc_com

	let add_feature pctx = Common.add_feature pctx.pc_com

	let create_context =
		let n = ref (-1) in
		(fun indent com debug -> {
				pc_indent = indent;
				pc_next_anon_func = (fun () -> incr n; Printf.sprintf "anon_%i" !n);
				pc_debug = debug;
				pc_com = com;
			}
		)

	let tabs = ref ""

	let opt o f s = match o with
		| None -> ""
		| Some v -> s ^ (f v)

	(* TODO: both of these are crazy *)

	let is_type p t =
		(fun r ->
			let x = t_infos r in
			(String.concat "." (fst x.mt_path)) = p && (snd x.mt_path) = t
		)

	let is_type1 p s =
		(fun t -> match follow t with
			| TInst(c,_) -> (is_type p s)(TClassDecl c)
			| TAbstract(a,_) -> (is_type p s)(TAbstractDecl a)
			| TEnum(en,_) -> (is_type p s)(TEnumDecl en)
			| _ -> false
		)

	let is_underlying_string t = match follow t with
		| TAbstract(a,tl) -> (is_type1 "" "str")(Abstract.get_underlying_type a tl)
		| _ -> false
	let is_underlying_array t = match follow t with
		| TAbstract(a,tl) -> (is_type1 "" "list")(Abstract.get_underlying_type a tl)
		| _ -> false

	let rec is_anon_or_dynamic t = match follow t with
		| TAbstract(a,tl) ->
			is_anon_or_dynamic (Abstract.get_underlying_type a tl)
		| TAnon _ | TDynamic _ -> true
		| _ -> false

	let handle_keywords s =
		KeywordHandler.handle_keywords s

	let print_unop = function
		| Increment | Decrement -> assert false
		| Not -> "not "
		| Neg -> "-";
		| NegBits -> "~"

	let print_binop = function
		| OpAdd -> "+"
		| OpSub -> "-"
		| OpMult -> "*"
		| OpDiv -> "/"
		| OpAssign -> "="
		| OpEq -> "=="
		| OpNotEq -> "!="
		| OpGt -> ">"
		| OpGte -> ">="
		| OpLt -> "<"
		| OpLte -> "<="
		| OpAnd -> "&"
		| OpOr -> "|"
		| OpXor -> "^"
		| OpBoolAnd -> "and"
		| OpBoolOr -> "or"
		| OpShl -> "<<"
		| OpShr -> ">>"
		| OpUShr -> ">>"
		| OpMod -> "%"
		| OpInterval | OpArrow | OpIn | OpAssignOp _ -> assert false

	let print_string s =
		Printf.sprintf "\"%s\"" (Ast.s_escape s)

	let print_constant = function
		| TThis -> "self"
		| TNull -> "None"
		| TBool(true) -> "True"
		| TBool(false) -> "False"
		| TString(s) -> print_string s
		| TInt(i) -> Int32.to_string i
		| TFloat s -> s
		| TSuper -> "super"

	let print_base_type tp =
		try
			begin match Meta.get Meta.Native tp.mt_meta with
				| _,[EConst(String (s,_)),_],_ -> s
				| _ -> raise Not_found
			end
		with Not_found ->
			let pack,name = tp.mt_path in
			(String.concat "_" pack) ^ (if pack = [] then name else "_" ^ name)

	let print_module_type mt = print_base_type (t_infos mt)

	let print_metadata (name,_,_) =
		Printf.sprintf "@%s" name

	let rec remove_outer_parens e = match e.eexpr with
		| TParenthesis(e) -> remove_outer_parens e
		| TMeta((Meta.Custom ":ternaryIf",_,_),_) -> e
		| TMeta(_,e) -> remove_outer_parens e
		| _ -> e

	let print_args args p =
		let had_value = ref false in
		let had_var_args = ref false in
		let had_kw_args = ref false in
		let sl = List.map (fun (v,cto) ->
			let check_err () = if !had_var_args || !had_kw_args then abort "Arguments after KwArgs/VarArgs are not allowed" p in
			let name = handle_keywords v.v_name in
			match follow v.v_type with
				| TAbstract({a_path = ["python"],"KwArgs"},_) ->
					if !had_kw_args then abort "Arguments after KwArgs are not allowed" p;
					had_kw_args := true;
					"**" ^ name
				| TAbstract({a_path = ["python"],"VarArgs"},_) ->
					check_err ();
					had_var_args := true;
					"*" ^ name
				| _ ->
					check_err ();
					name ^ match cto with
						| None when !had_value -> " = None"
						| None -> ""
						| Some ct ->
							had_value := true;
							Printf.sprintf " = %s" (print_constant ct)
		) args in
		String.concat "," sl

	let rec print_op_assign_right pctx e =
		match e.eexpr with
			| TIf({eexpr = TParenthesis econd},eif,Some eelse)
			| TIf(econd,eif,Some eelse) ->
				Printf.sprintf "%s if %s else %s" (print_expr pctx eif) (print_expr pctx econd) (print_expr pctx eelse)
			| _ ->
				print_expr pctx (remove_outer_parens e)

	and print_var pctx v eo =
		match eo with
			| Some ({eexpr = TFunction tf} as e) ->
				print_function pctx tf (Some v.v_name) e.epos
			| _ ->
				let s_init = match eo with
					| None -> "None"
					| Some e -> print_op_assign_right pctx e
				in
				Printf.sprintf "%s = %s" (handle_keywords v.v_name) s_init

	and print_function pctx tf name p =
		let s_name = match name with
			| None -> pctx.pc_next_anon_func()
			| Some s -> handle_keywords s
		in
		let s_args = print_args tf.tf_args p in
		let s_expr = print_expr {pctx with pc_indent = "    " ^ pctx.pc_indent} tf.tf_expr in
		Printf.sprintf "def %s(%s):\n%s    %s" s_name s_args pctx.pc_indent s_expr

	and print_tarray_list pctx e1 e2 =
		let s1 = (print_expr pctx e1) in
		let s2 = (print_expr pctx e2) in
		let default = Printf.sprintf "python_internal_ArrayImpl._get(%s, %s)" s1 s2 in

		let handle_index =
			match e2.eexpr with
			| TConst TInt index ->
				if Int32.to_int index >= 0 then
					Printf.sprintf "(%s[%s] if %s < len(%s) else None)" s1 s2 s2 s1
				else
					"None"
			| TLocal _ ->
				Printf.sprintf "(%s[%s] if %s >= 0 and %s < len(%s) else None)" s1 s2 s2 s2 s1
			| _ ->
				default
		in
		match e1.eexpr with
		| TLocal _ -> handle_index
		| TField ({eexpr=(TConst TThis | TLocal _)},_) -> handle_index
		| _ -> default

	and is_safe_string pctx x =
		let follow_parens e = match e.eexpr with
			| TParenthesis e -> e
			| _ -> e
		in
		match (follow_parens x).eexpr with
		| TBinop(OpAdd, e1, e2) -> is_safe_string pctx e1 && is_safe_string pctx e2
		| TCall (e1,_) ->
			let id = print_expr pctx (follow_parens e1) in
			(match id with
			| "Std.string" -> true
			| _ -> false)
		| TConst (TString s) -> true
		| _ -> false

	and print_expr pctx e =
		let indent = pctx.pc_indent in
		let print_expr_indented e = print_expr {pctx with pc_indent = "    " ^ pctx.pc_indent} e in
		match e.eexpr with
			| TConst ct ->
				print_constant ct
			| TTypeExpr mt ->
				print_module_type mt
			| (TLocal v | TParenthesis({ eexpr = (TLocal v) })) ->
				handle_keywords v.v_name
			| TEnumParameter(e1,_,index) ->
				Printf.sprintf "%s.params[%i]" (print_expr pctx e1) index
			| TEnumIndex e1 ->
				Printf.sprintf "%s.index" (print_expr pctx e1)
			| TArray(e1,e2) when (is_type1 "" "list")(e1.etype) || is_underlying_array e1.etype ->
				print_tarray_list pctx e1 e2
			| TArray({etype = t} as e1,e2) when is_anon_or_dynamic t ->
				Printf.sprintf "HxOverrides.arrayGet(%s, %s)" (print_expr pctx e1) (print_expr pctx e2)
			| TArray(e1,e2) ->
				Printf.sprintf "%s[%s]" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpAssign, {eexpr = TArray(e1,e2)}, e3) when (is_type1 "" "list")(e1.etype) || is_underlying_array e1.etype ->
				Printf.sprintf "python_internal_ArrayImpl._set(%s, %s, %s)" (print_expr pctx e1) (print_expr pctx e2) (print_expr pctx e3)
			| TBinop(OpAssign,{eexpr = TArray({etype = t} as e1,e2)},e3) when is_anon_or_dynamic t ->
				Printf.sprintf "HxOverrides.arraySet(%s,%s,%s)" (print_expr pctx e1) (print_expr pctx e2) (print_expr pctx e3)
			| TBinop(OpAssign,{eexpr = TArray(e1,e2)},e3) ->
				Printf.sprintf "%s[%s] = %s" (print_expr pctx e1) (print_expr pctx e2) (print_expr pctx (remove_outer_parens e3) )
			| TBinop(OpAssign,{eexpr = TField(ef1,fa)},e2) ->
				Printf.sprintf "%s = %s" (print_field pctx ef1 fa true) (print_op_assign_right pctx e2)
			| TBinop(OpAssign,e1,e2) ->
				Printf.sprintf "%s = %s" (print_expr pctx e1) (print_expr pctx (remove_outer_parens e2))
			| TBinop(op,e1,({eexpr = TBinop(_,_,_)} as e2)) ->
				print_expr pctx { e with eexpr = TBinop(op, e1, { e2 with eexpr = TParenthesis(e2) })}
			| TBinop(OpEq,{eexpr = TCall({eexpr = TIdent "__typeof__"},[e1])},e2) ->
				begin match e2.eexpr with
					| TConst(TString s) ->
						begin match s with
							| "string" -> Printf.sprintf "Std._hx_is(%s, str)" (print_expr pctx e1)
							| "boolean" -> Printf.sprintf "Std._hx_is(%s, bool)" (print_expr pctx e1)
							| "number" -> Printf.sprintf "Std._hx_is(%s, float)" (print_expr pctx e1)
							| _ -> assert false
						end
					| _ ->
						assert false
				end
			| TBinop(OpEq,e1,({eexpr = TConst TNull} as e2)) ->
				Printf.sprintf "(%s is %s)" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpNotEq,e1,({eexpr = TConst TNull} as e2)) ->
				Printf.sprintf "(%s is not %s)" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpEq|OpNotEq as op,e1, e2) ->
				let ops = match op with
					| OpEq -> "is", "==", "HxOverrides.eq"
					| OpNotEq -> "is not", "!=", "not HxOverrides.eq"
					| _ -> assert false
				in
				let third (_,_,x) = x in
				let fst (x,_,_) = x in
				let snd (_,x,_) = x in
				let is_list_or_anon x = begin match x with
					| TInst({cl_path = [],("list")},_) -> true
					| TAnon _ -> true
					| _ -> false
				end in
				let is_const_byte x =
					match x.eexpr with
					| TConst TInt x ->
						let x = Int32.to_int x in
						x >= 0 && x <= 256
					| _ -> false
				in
				(match follow e1.etype, follow e2.etype with
				| TAbstract({a_path = [],("Int")}, _),TAbstract({a_path = [],("Int")}, _) when is_const_byte e2 || is_const_byte e1 ->
					Printf.sprintf "(%s %s %s)" (print_expr pctx e1) (snd ops) (print_expr pctx e2)
					(* the following optimization causes a problem with polygonal unit tests
					   see: https://github.com/HaxeFoundation/haxe/issues/2952
					*)
					(* Printf.sprintf "(%s %s %s)" (print_expr pctx e1) (fst ops) (print_expr pctx e2) *)
				| TInst({cl_path = [],("list")},_), _ ->
					Printf.sprintf "(%s %s %s)" (print_expr pctx e1) (fst ops) (print_expr pctx e2)
				| x, _ when is_underlying_array x ->
					Printf.sprintf "(%s %s %s)" (print_expr pctx e1) (fst ops) (print_expr pctx e2)
				| TDynamic _, TDynamic _ ->
					Printf.sprintf "%s(%s,%s)" (third ops) (print_expr pctx e1) (print_expr pctx e2)
				| TDynamic _, x when is_list_or_anon x ->
					Printf.sprintf "%s(%s,%s)" (third ops) (print_expr pctx e1) (print_expr pctx e2)
				| x, TDynamic _ when is_list_or_anon x ->
					Printf.sprintf "%s(%s,%s)" (third ops) (print_expr pctx e1) (print_expr pctx e2)
				| _,_ -> Printf.sprintf "(%s %s %s)" (print_expr pctx e1) (snd ops) (print_expr pctx e2))
			| TBinop(OpMod,e1,e2) when (is_type1 "" "Int")(e1.etype) && (is_type1 "" "Int")(e2.etype) ->
				(match e1.eexpr with
				| TConst(TInt(x)) when (Int32.to_int x) >= 0 ->
					(* constant optimization *)
					Printf.sprintf "%s %% %s" (print_expr pctx e1) (print_expr pctx e2)
				| _ ->
					Printf.sprintf "HxOverrides.mod(%s, %s)" (print_expr pctx e1) (print_expr pctx e2))
			| TBinop(OpMod,e1,e2) ->
				Printf.sprintf "HxOverrides.modf(%s, %s)" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpUShr,e1,e2) ->
				Printf.sprintf "HxOverrides.rshift(%s, %s)" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpAdd,e1,e2) when (is_type1 "" "str")(e.etype) || is_underlying_string e.etype ->
				let rec safe_string ex =
					match ex.eexpr, ex.etype with
						| e, _ when is_safe_string pctx ex -> print_expr pctx ex
						| TBinop(OpAdd, e1, e2), x when (is_type1 "" "str")(x) -> Printf.sprintf "(%s + %s)" (safe_string e1) (safe_string e2)
						| (TLocal(_)),x when (is_type1 "" "str")(x) ->
							(*
								we could add this pattern too, but is it sideeffect free??
								| TField({ eexpr = TLocal(_)},_)
							*)
							let s = (print_expr pctx ex) in
							Printf.sprintf "(\"null\" if %s is None else %s)" s s
						| _,x when (is_type1 "" "str")(x) -> Printf.sprintf "HxOverrides.stringOrNull(%s)" (print_expr pctx ex)
						| _,_ ->
							if has_feature pctx "Std.string" then
								Printf.sprintf "Std.string(%s)" (print_expr pctx ex)
							else
								Printf.sprintf "str(%s)" (print_expr pctx ex)
				in
				let e1_str = safe_string e1 in
				let e2_str = safe_string e2 in
				Printf.sprintf "(%s + %s)" e1_str e2_str
			| TBinop(OpAdd,e1,e2) when (match follow e.etype with TDynamic _ -> true | _ -> false) ->
				Printf.sprintf "python_Boot._add_dynamic(%s,%s)" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(op,e1,e2) ->
				Printf.sprintf "(%s %s %s)" (print_expr pctx e1) (print_binop op) (print_expr pctx e2)
			| TField(e1,fa) ->
				print_field pctx e1 fa false
			| TParenthesis e1 ->
				Printf.sprintf "(%s)" (print_expr pctx e1)
			| TObjectDecl fl ->
				let fl2 = ref fl in
				begin match follow e.etype with
					| TAnon an ->
						PMap.iter (fun s cf ->
							if not (Expr.field_mem_assoc s fl) then fl2 := ((s,null_pos,NoQuotes),null cf.cf_type cf.cf_pos) :: !fl2
						) an.a_fields
					| _ ->
						()
				end;
				Printf.sprintf "_hx_AnonObject(%s)" (print_exprs_named pctx ", " !fl2)
			| TArrayDecl el ->
				Printf.sprintf "[%s]" (print_exprs pctx ", " el)
			| TCall(e1,el) ->
				print_call pctx e1 el e
			| TNew(c,_,el) ->
				let id = print_base_type (t_infos (TClassDecl c)) in
				Printf.sprintf "%s(%s)" id (print_call_args pctx e el)
			| TUnop(Not,Prefix,e1) ->
				Printf.sprintf "(%s%s)" (print_unop Not) (print_expr pctx e1)
			| TUnop(op,Prefix,e1) ->
				Printf.sprintf "%s%s" (print_unop op) (print_expr pctx e1)
			| TFunction tf ->
				print_function pctx tf None e.epos
			| TVar (v,eo) ->
				print_var pctx v eo
			| TBlock [] ->
				Printf.sprintf "pass"
			| TBlock [{ eexpr = TBlock _} as b] ->
				print_expr pctx b
			| TBlock el ->
				let old = !tabs in
				tabs := pctx.pc_indent;
				let s = print_block_exprs pctx ("\n" ^ !tabs) pctx.pc_debug el in
				tabs := old;
				Printf.sprintf "%s" s
			| TIf(econd,eif,(Some {eexpr = TIf _} as eelse)) ->
				print_if_else pctx econd eif eelse true
			| TIf(econd,eif,eelse) ->
				print_if_else pctx econd eif eelse false
			| TWhile(econd,e1,NormalWhile) ->
				Printf.sprintf "while %s:\n%s    %s" (print_expr pctx (remove_outer_parens econd)) indent (print_expr_indented e1)
			| TWhile(econd,e1,DoWhile) ->
				abort "Currently not supported" e.epos
			| TTry(e1,catches) ->
				print_try pctx e1 catches
			| TReturn eo ->
				Printf.sprintf "return%s" (opt eo (print_op_assign_right pctx) " ")
			| TBreak ->
				"break"
			| TContinue ->
				"continue"
			| TThrow e1 ->
				let rec is_native_exception t =
					match Abstract.follow_with_abstracts t with
					| TInst ({ cl_path = [],"BaseException" }, _) ->
						true
					| TInst ({ cl_super = Some csup }, _) ->
						is_native_exception (TInst(fst csup, snd csup))
					| _ ->
						false
				in
				if is_native_exception e1.etype then
					Printf.sprintf "raise %s" (print_expr pctx e1)
				else
					Printf.sprintf "raise _HxException(%s)" (print_expr pctx e1)
			| TCast(e1,None) ->
				print_expr pctx e1
			| TMeta((Meta.Custom ":ternaryIf",_,_),{eexpr = TIf(econd,eif,Some eelse)}) ->
				Printf.sprintf "(%s if %s else %s)" (print_expr pctx eif) (print_expr pctx econd) (print_expr pctx eelse)
			| TMeta(_,e1) ->
				print_expr pctx e1
			| TIdent s ->
				s
			| TSwitch _ | TCast(_, Some _) | TFor _ | TUnop(_,Postfix,_) ->
				assert false

	and print_if_else pctx econd eif eelse as_elif =
		let econd1 = match econd.eexpr with
			| TParenthesis e -> e
			| _ -> econd
		in
		let if_str = print_expr {pctx with pc_indent = "    " ^ pctx.pc_indent} eif in
		let indent = pctx.pc_indent in
		let else_str = if as_elif then
			opt eelse (print_expr pctx) "el"
		else
			opt eelse (print_expr {pctx with pc_indent = "    " ^ pctx.pc_indent}) (Printf.sprintf "else:\n%s    " indent)
		in
		let else_str = if else_str = "" then "" else "\n" ^ indent ^ else_str in
		Printf.sprintf "if %s:\n%s    %s%s" (print_expr pctx (remove_outer_parens econd1)) indent if_str else_str

	and print_field pctx e1 fa is_assign =
		let obj = match e1.eexpr with
			| TConst TSuper -> "super()"
			| _ -> print_expr pctx e1
		in
		let name = field_name fa in
		let is_extern = (match fa with
		| FInstance(c,_,_) -> c.cl_extern
		| FStatic(c,_) -> c.cl_extern
		| _ -> false)
		in
		let do_default () =
			Printf.sprintf "%s.%s" obj (if is_extern then name else (handle_keywords name))
		in
		let call_override s =
			match s with
			| "iterator" | "toUpperCase" | "toLowerCase" | "pop" | "shift" | "join" | "push" | "map" | "filter" -> true
			| _ -> false
		in
		match fa with
			(* we need to get rid of these cases in the transformer, how is this handled in js *)
			| FInstance(c,_,{cf_name = "length"}) when (is_type "" "list")(TClassDecl c) ->
				Printf.sprintf "len(%s)" (print_expr pctx e1)
			| FInstance(c,_,{cf_name = "length"}) when (is_type "" "str")(TClassDecl c) ->
				Printf.sprintf "len(%s)" (print_expr pctx e1)
			| FStatic(c,{cf_name = "fromCharCode"}) when (is_type "" "str")(TClassDecl c) ->
				Printf.sprintf "HxString.fromCharCode"
			| FStatic({cl_path = ["python";"internal"],"UBuiltins"},{cf_name = s}) ->
				s
			| FClosure (Some(c,cf),_) when ((is_type "" "list")(TClassDecl c)) ->
				Printf.sprintf "python_Boot.createClosure(%s, python_internal_ArrayImpl.%s)" obj name
			| FClosure (Some(c,cf),_) when ((is_type "" "str")(TClassDecl c)) ->
				Printf.sprintf "python_Boot.createClosure(%s, HxString.%s)" obj name
			| FInstance (c,_,cf) when ((is_type "" "list")(TClassDecl c)) ->
				Printf.sprintf "python_Boot.createClosure(%s, python_internal_ArrayImpl.%s)" obj name
			| FInstance (c,_,cf) when ((is_type "" "str")(TClassDecl c)) ->
				Printf.sprintf "python_Boot.createClosure(%s, HxString.%s)" obj name
			| FInstance _ | FStatic _ ->
				do_default ()
			| FAnon cf when is_assign && call_override(name) ->
				begin match follow cf.cf_type with
					| TFun([],_) ->
						Printf.sprintf "_hx_partial(HxOverrides.%s, %s)" name obj
					| _ ->
						do_default()
				end
			| _ ->
				do_default()

	and print_try pctx e1 catches =
		let has_catch_all = List.exists (fun (v,_) -> match v.v_type with
			| TDynamic _ -> true
			| _ -> false
		) catches in
		let has_only_catch_all = has_catch_all && begin match catches with
			| [_] -> true
			| _ -> false
		end in
		let print_catch pctx i (v,e) =
			let is_empty_expr = begin match e.eexpr with
				| TBlock [] -> true
				| _ -> false
			end in
			let indent = pctx.pc_indent in
			(* Don't generate assignment to catch variable when catch expression is an empty block *)
			let assign = if is_empty_expr then "" else Printf.sprintf "%s = _hx_e1\n%s" v.v_name indent in
			let handle_base_type bt =
				let t = print_base_type bt in
				let print_type_check t_str =
					Printf.sprintf "if isinstance(_hx_e1, %s):\n%s    %s    %s" t_str indent assign (print_expr {pctx with pc_indent = "    " ^ pctx.pc_indent} e)
				in
				let res = match t with
				| "str" -> print_type_check "str"
				| "Bool" -> print_type_check "bool"
				| "Int" -> print_type_check "int"
				| "Float" -> print_type_check "float"
				| t -> print_type_check t
				in
				if i > 0 then
					indent ^ "el" ^ res
				else
					res
			in
			match follow v.v_type with
				| TDynamic _ ->
					begin if has_only_catch_all then
						Printf.sprintf "%s%s" assign (print_expr pctx e)
					else
						(* Dynamic is always the last block *)
						Printf.sprintf "%selse:\n    %s%s    %s" indent indent assign (print_expr {pctx with pc_indent = "    " ^ pctx.pc_indent} e)
					end
				| TInst(c,_) ->
					handle_base_type (t_infos (TClassDecl c))
				| TEnum(en,_) ->
					handle_base_type (t_infos (TEnumDecl en))
				| TAbstract(a,_) ->
					handle_base_type (t_infos (TAbstractDecl a))
				| _ ->
					assert false
		in
		let indent = pctx.pc_indent in
		let print_expr_indented e = print_expr {pctx with pc_indent = "    " ^ pctx.pc_indent} e in
		let try_str = Printf.sprintf "try:\n%s    %s\n%s" indent (print_expr_indented e1) indent in
		let except = if has_feature pctx "has_throw" then
			Printf.sprintf "except Exception as _hx_e:\n%s    _hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e\n%s    " indent indent
		else
			Printf.sprintf "except Exception as _hx_e:\n%s    _hx_e1 = _hx_e\n%s    " indent indent
		in
		let catch_str = String.concat (Printf.sprintf "\n") (ExtList.List.mapi (fun i catch -> print_catch {pctx with pc_indent = "    " ^ pctx.pc_indent} i catch) catches) in
		let except_end = if not has_catch_all then Printf.sprintf "\n%s    else:\n%s        raise _hx_e" indent indent else "" in
		Printf.sprintf "%s%s%s%s" try_str except catch_str except_end

	and print_call2 pctx e1 el =
		let id = print_expr pctx e1 in
		match id,el with
			| "__define_feature__",[_;e] ->
				print_expr pctx e
			| "super",_ ->
				let s_el = (print_call_args pctx e1 el) in
				Printf.sprintf "super().__init__(%s)" s_el
			| ("python_Syntax._pythonCode"),[({ eexpr = TConst (TString code) } as ecode); {eexpr = TArrayDecl tl}] ->
				let buf = Buffer.create 0 in
				let interpolate () =
					Codegen.interpolate_code pctx.pc_com code tl (Buffer.add_string buf) (fun e -> Buffer.add_string buf (print_expr pctx e)) ecode.epos
				in
				let old = pctx.pc_com.error in
				pctx.pc_com.error <- abort;
				Std.finally (fun() -> pctx.pc_com.error <- old) interpolate ();
				Buffer.contents buf
			| ("python_Syntax._pythonCode"), [e] ->
				print_expr pctx e
			| "python_Syntax._callNamedUntyped",el ->
				let res,fields = match List.rev el with
					| {eexpr = TObjectDecl fields} :: el ->
						List.rev el,fields
					| _ ->
						assert false
				in
				begin match res with
					| e1 :: [] ->
						Printf.sprintf "%s(%s)" (print_expr pctx e1) (print_params_named pctx ", " fields)
					| e1 :: el ->
						Printf.sprintf "%s(%s, %s)" (print_expr pctx e1) (print_exprs pctx ", " el) (print_params_named pctx ", " fields)
					| [] ->
						Printf.sprintf "%s(%s)" (print_expr pctx e1) (print_params_named pctx ", " fields)
				end
			| "python_Syntax.varArgs",[e1] ->
				"*" ^ (print_expr pctx e1)
			| "python_Syntax.call" ,e1 :: [{eexpr = TArrayDecl el}]->
				Printf.sprintf "%s(%s)" (print_expr pctx e1) (print_exprs pctx ", " el)
			| "python_Syntax.field",[e1;{eexpr = TConst(TString id)}] ->
				Printf.sprintf "%s.%s" (print_expr pctx e1) id
			| "python_Syntax._tuple", [{eexpr = TArrayDecl el}] ->
				(match el with
				| [e] ->
					Printf.sprintf "(%s,)" (print_expr pctx e)
				| _ ->
					Printf.sprintf "(%s)" (print_exprs pctx ", " el))
			| "python_Syntax._arrayAccess", e1 :: {eexpr = TArrayDecl el} :: etrail ->
				let trailing_colon = match etrail with
					| [{eexpr = TConst(TBool(true))}] -> true
					| _ -> false
				in
				Printf.sprintf "%s[%s%s]" (print_expr pctx e1) (print_exprs pctx ":" el) (if trailing_colon then ":" else "")
			| "python_Syntax.isIn",[e1;e2] ->
				Printf.sprintf "(%s in %s)" (print_expr pctx e1) (print_expr pctx e2)
			| "python_Syntax.delete",[e1] ->
				Printf.sprintf "del %s" (print_expr pctx e1)
			| "python_Syntax.binop",[e0;{eexpr = TConst(TString id)};e2] ->
				Printf.sprintf "(%s %s %s)" (print_expr pctx e0) id (print_expr pctx e2)
			| "python_Syntax.assign",[e0;e1] ->
				Printf.sprintf "%s = %s" (print_expr pctx e0) (print_expr pctx e1)
			| "python_Syntax.arraySet",[e1;e2;e3] ->
				Printf.sprintf "%s[%s] = %s" (print_expr pctx e1) (print_expr pctx e2) (print_expr pctx e3)
			| "python_Syntax._newInstance", e1 :: [{eexpr = TArrayDecl el}] ->
				Printf.sprintf "%s(%s)" (print_expr pctx e1) (print_exprs pctx ", " el)
			| "python_Syntax.opPow", [e1;e2] ->
				Printf.sprintf "(%s ** %s)" (print_expr pctx e1) (print_expr pctx e2)
 			| "python_Syntax._foreach",[e1;e2;e3] ->
				let pctx = {pctx with pc_indent = "    " ^ pctx.pc_indent} in
				let i = pctx.pc_indent in
				Printf.sprintf "for %s in %s:\n%s%s" (print_expr pctx e1) (print_expr pctx e2) i (print_expr pctx e3)
			| _,el ->
				Printf.sprintf "%s(%s)" id (print_call_args pctx e1 el)

	and print_call pctx e1 el call_expr =
		let get_native_fields t = match follow t with
			| TAnon(a) ->
				let fold f cf acc =
					if Meta.has Meta.Native cf.cf_meta then begin
						let _, args, mp = Meta.get Meta.Native cf.cf_meta in
						match args with
						| [( EConst(String (s,_)),_)] -> PMap.add f s acc
						| _ -> acc
					end else acc
				in
				let mapping = PMap.foldi fold a.a_fields PMap.empty in
				mapping
			| _ -> PMap.empty
		in
		let native_fields_str native_fields =
			let fold_dict k v acc =
				let prefix = if acc = "" then "" else "," in
				Printf.sprintf "%s%s\"%s\":\"%s\"" acc prefix (handle_keywords k) v
			in
			PMap.foldi fold_dict native_fields ""
		in
		match e1.eexpr, el with
			| TIdent "`trace", [e;infos] ->
				if has_feature pctx "haxe.Log.trace" then begin
					"haxe_Log.trace(" ^ (print_expr pctx e) ^ "," ^ (print_expr pctx infos) ^ ")"
				end else if is_safe_string pctx e then
					"print(" ^ (print_expr pctx e) ^ ")"
				else
					"print(str(" ^ (print_expr pctx e) ^ "))"
			| TField(e1,((FAnon {cf_name = (("join" | "push" | "map" | "filter") as s)}) | FDynamic (("join" | "push" | "map" | "filter") as s))), [x] ->
				Printf.sprintf "HxOverrides.%s(%s, %s)" s (print_expr pctx e1) (print_expr pctx x)
			| TField(e1,((FAnon {cf_name = (("iterator" | "toUpperCase" | "toLowerCase" | "pop" | "shift") as s)}) | FDynamic (("iterator" | "toUpperCase" | "toLowerCase" | "pop" | "shift") as s))), [] ->
				Printf.sprintf "HxOverrides.%s(%s)" s (print_expr pctx e1)
			| TField(_, (FStatic({cl_path = ["python"; "_KwArgs"], "KwArgs_Impl_"},{ cf_name="fromT" }))), [e2]  ->
				let t = match follow call_expr.etype with
				| TAbstract(_, [t]) -> t
				| _ -> assert false
				in
				let native_fields = get_native_fields t in
				if PMap.is_empty native_fields then
					print_call2 pctx e1 el
				else
					let s1 = native_fields_str native_fields in
					Printf.sprintf "python__KwArgs_KwArgs_Impl_.fromT(HxOverrides.mapKwArgs(%s, {%s}))" (print_expr pctx e2) s1
			| TField(_, (FStatic({cl_path = ["python"; "_KwArgs"], "KwArgs_Impl_"},{ cf_name="toDictHelper" }))), [e2; et]  ->
				let native_fields = get_native_fields et.etype in
				if PMap.is_empty native_fields then
					print_call2 pctx e1 el
				else
					let s1 = native_fields_str native_fields in
					Printf.sprintf "python__KwArgs_KwArgs_Impl_.toDictHelper(HxOverrides.reverseMapKwArgs(%s, {%s}), None)" (print_expr pctx e2) s1
			| _,_ ->
				print_call2 pctx e1 el

	and print_call_args pctx e1 el =
		let print_arg pctx i x =
			let e = match x.eexpr, follow x.etype with
				| TConst TNull, TAbstract({a_path = ["python"],"KwArgs"},_) -> "{}"
				| TConst TNull, TAbstract({a_path = ["python"],"VarArgs"},_) -> "[]"
				| _ -> (print_expr pctx x)
			in
			let prefix = match e1.eexpr, follow x.etype with
				(* the should not apply for the instance methods of the abstract itself *)
				| TField(_, FStatic({cl_path = ["python"; "_KwArgs"],"KwArgs_Impl_"},f)), _ when i == 0 && Meta.has Meta.Impl f.cf_meta -> ""
				| TField(_, FStatic({cl_path = ["python"; "_VarArgs"],"VarArgs_Impl_"},f)), _ when i == 0 && Meta.has Meta.Impl f.cf_meta -> ""
				| _, TAbstract({a_path = ["python"],"KwArgs"},_) -> "**"
				| _, TAbstract({a_path = ["python"],"VarArgs"},_) -> "*"
				| _, _ -> ""
			in
			prefix ^ e
		in
		String.concat "," (ExtList.List.mapi (print_arg pctx) el)

	and print_exprs pctx sep el =
		String.concat sep (List.map (print_expr pctx) el)

	and last_debug_comment = ref ("")

	and print_block_exprs pctx sep print_debug_comment el =
		if print_debug_comment then begin
			let el = List.fold_left (fun acc e ->
				let line = Lexer.get_error_line e.epos in
				let debug_line = (Printf.sprintf "# %s:%i" e.epos.pfile line) in
				let res = if (!last_debug_comment) <> debug_line then
					(print_expr pctx e) :: debug_line :: acc
				else
					(print_expr pctx e) :: acc
				in
				last_debug_comment := debug_line;
				res
			) [] el in
			String.concat sep (List.rev el)
		end else
			print_exprs pctx sep el

	and print_exprs_named pctx sep fl =
		let args = String.concat sep (List.map (fun ((s,_,_),e) -> Printf.sprintf "'%s': %s" (Ast.s_escape (handle_keywords s)) (print_expr pctx e)) fl) in
		Printf.sprintf "{%s}" args
	and print_params_named pctx sep fl =
		let args = String.concat sep (List.map (fun ((s,_,_),e) -> Printf.sprintf "%s= %s" (handle_keywords s) (print_expr pctx e)) fl) in
		Printf.sprintf "%s" args
	let handle_keywords s =
		KeywordHandler.handle_keywords s
end

module Generator = struct
	type context = {
		com : Common.context;
		buf : Buffer.t;
		packages : (string,int) Hashtbl.t;
		mutable static_inits : (unit -> unit) list;
		mutable class_inits : (unit -> unit) list;
		mutable indent_count : int;
		transform_time : float;
		print_time : float;
	}

	let has_feature ctx = Common.has_feature ctx.com
	let add_feature ctx = Common.add_feature ctx.com

	type class_field_infos = {
		cfd_fields : string list;
		cfd_props : string list;
		cfd_methods : string list;
	}

	type import_type =
		| IModule of string
		| IObject of string * string

	let mk_context com = {
		com = com;
		buf = Buffer.create 16000;
		packages = Hashtbl.create 0;
		static_inits = [];
		class_inits = [];
		indent_count = 0;
		transform_time = 0.;
		print_time = 0.;
	}

	(* Transformer interface *)

	let transform_expr e =
		(* let e = Codegen.UnificationCallback.run Transformer.check_unification e in *)
		Transformer.transform e

	let transform_to_value e =
		(* let e = Codegen.UnificationCallback.run Transformer.check_unification e in *)
		Transformer.transform_to_value e

	(* Printer interface *)

	let get_path mt =
		Printer.print_base_type mt

	let tfunc_str f pctx name p =
		Printer.print_function pctx f name p

	let texpr_str e pctx =
		Printer.print_expr pctx e

	let handle_keywords s =
		Printer.handle_keywords s

	(* Helper *)

	let get_full_name mt =
		(* TODO: haxe source is crazy *)
		s_type_path mt.mt_path

	let collect_class_field_data cfl =
		let fields = DynArray.create () in
		let props = DynArray.create () in
		let methods = DynArray.create () in
		List.iter (fun cf ->
			match cf.cf_kind with
				| Var({v_read = AccResolve}) ->
					()
				| Var _ when not (is_physical_field cf) ->
					()
				| Var({v_read = AccCall}) ->
					if Meta.has Meta.IsVar cf.cf_meta then
						DynArray.add fields cf.cf_name
					else
						DynArray.add props cf.cf_name
				| Var _ ->
					DynArray.add fields cf.cf_name
				| _ ->
					DynArray.add methods cf.cf_name
		) cfl;
		{
			cfd_fields = DynArray.to_list fields;
			cfd_props = DynArray.to_list props;
			cfd_methods = DynArray.to_list methods;
		}

	let collect_class_statics_data cfl =
		let fields = DynArray.create () in
		List.iter (fun cf ->
			if is_physical_field cf then
				DynArray.add fields cf.cf_name
		) cfl;
		DynArray.to_list fields

	let filter_py_metas metas =
		List.filter (fun (n,_,_) -> match n with Meta.Custom ":python" -> true | _ -> false) metas

	let get_members_with_init_expr c =
		List.filter (fun cf -> match cf.cf_kind with
			| Var _ when not (is_physical_field cf) -> false
			| Var _ when cf.cf_expr = None -> true
			| _ -> false
		) c.cl_ordered_fields

	(* Printing *)

	let spr ctx s =
		Buffer.add_string ctx.buf s

	let spr_line ctx s =
		Buffer.add_string ctx.buf s;
		Buffer.add_string ctx.buf "\n"

	let print ctx =
		Printf.kprintf (fun s -> begin
			Buffer.add_string ctx.buf s
		end)

	let newline ctx =
		if not (Buffer.length ctx.buf = 0) then spr ctx "\n"


	(* Generating functions *)

	let gen_py_metas ctx metas indent =
		List.iter (fun (n,el,_) ->
			match el with
				| [EConst(String (s,_)),_] ->
					print ctx "%s@%s\n" indent s
				| _ ->
					assert false
		) metas

	let gen_expr ctx e field indent =
		let pctx = Printer.create_context ("    " ^ indent) ctx.com ctx.com.debug in
		let e = match e.eexpr with
			| TFunction(f) ->
				{e with eexpr = TBlock [e]}
			| _ ->
				e
		in
		let expr2 = transform_to_value e in
		let name = "_hx_init_" ^ (String.concat "_" (ExtString.String.nsplit field ".")) in
		let maybe_split_expr expr2 = match expr2.eexpr with
			| TBlock es when es <> [] && field <> "" ->
				begin match List.rev es with
					| e_last :: el ->
						let new_last = {e_last with eexpr = TReturn (Some e_last)} in
						let new_block = {expr2 with eexpr = TBlock (List.rev (new_last :: el))} in
						let v_name = alloc_var name (tfun [] e_last.etype) e_last.epos in
						let f_name = mk (TLocal v_name) v_name.v_type e_last.epos in
						let call_f = mk (TCall(f_name,[])) e_last.etype e_last.epos in
						Some new_block,call_f
					| _ ->
						assert false
				end
			| _ ->
				None,expr2
		in
		let r = maybe_split_expr expr2 in
		match r with
			| Some e1,e2 ->
				let expr_string_1 = texpr_str e1 pctx in
				let expr_string_2 = texpr_str e2 pctx in
				print ctx "%sdef %s():\n    %s" indent name expr_string_1;
				newline ctx;
				print ctx "%s%s = %s" indent field expr_string_2;
			| None,e2 ->
				let expr_string_2 = texpr_str e2 pctx in
				if field = "" then
					spr ctx expr_string_2
				else
					print ctx "%s%s = %s" indent field expr_string_2

	let gen_func_expr ctx e c name metas add_self indent stat p =
		let pctx = Printer.create_context indent ctx.com ctx.com.debug in
		let e = match e.eexpr with
			| TFunction(f) ->
				let args = if add_self then
					let v = alloc_var "self" t_dynamic p in
					v.v_meta <- (Meta.This,[],p) :: v.v_meta;
					(v,None) :: f.tf_args
				else
					f.tf_args
				in
				{e with eexpr = TFunction {f with tf_args = args}}
			| _ ->
				e
		in
		if stat then begin
			newline ctx;
			spr ctx indent;
			spr ctx "@staticmethod\n"
		end;
		let expr1 = transform_expr e in
		let expr_string = match expr1.eexpr with
			| TFunction f ->
				tfunc_str f pctx (Some name) p
			| _ ->
				Printf.sprintf "%s = %s" name (texpr_str expr1 pctx)
		in
		gen_py_metas ctx metas indent;
		spr ctx indent;
		spr ctx expr_string

	let gen_class_constructor ctx c cf =
		let member_inits = get_members_with_init_expr c in
		let py_metas = filter_py_metas cf.cf_meta in
		begin match cf.cf_expr with
			| Some ({eexpr = TFunction f} as ef) ->
				let ethis = mk (TConst TThis) (TInst(c,List.map snd c.cl_params)) cf.cf_pos in
				let assigned_fields = ref [] in
				(* Collect all fields that are assigned to but panic out as soon as `this`,
				   `super`, `return` or `throw` appears (regardless of control flow). *)
				let collect_assignments e =
					let rec loop e = match e.eexpr with
						| TBinop(OpAssign,{eexpr = TField({eexpr = TConst TThis}, FInstance(_,_,cf))},e2) ->
							loop e2;
							assigned_fields := cf :: !assigned_fields
						| TConst (TSuper | TThis) | TThrow _ | TReturn _ ->
							raise Exit
						(* TODO: We could do some branch intersection stunts to make this more accurate. *)
						| TIf(e1,_,_) | TSwitch(e1,_,_) | TWhile(e1,_,_) ->
							loop e1
						| _ ->
							Type.iter loop e
					in
					try loop e with Exit -> ()
				in
				collect_assignments f.tf_expr;
				let member_data = List.fold_left (fun acc cf ->
					if not (List.memq cf !assigned_fields) then begin
						let ef = mk (TField(ethis,FInstance(c,[],cf))) cf.cf_type cf.cf_pos in (* TODO *)
						let e = mk (TBinop(OpAssign,ef,null ef.etype ef.epos)) ef.etype ef.epos in
						e :: acc
					end else
						acc
				) [] member_inits in
				let e = concat (mk (TBlock member_data) ctx.com.basic.tvoid cf.cf_pos) f.tf_expr in
				let ef = {ef with eexpr = TFunction {f with tf_expr = e}} in
				cf.cf_expr <- Some ef;

				newline ctx;
				newline ctx;
				gen_func_expr ctx ef c "__init__" py_metas true "    " false cf.cf_pos
			| _ ->
				assert false
		end

	let gen_class_field ctx c p cf =
		let field = handle_keywords cf.cf_name in
		begin match cf.cf_expr with
			| None ->
				()(* print ctx "    # var %s" field *)
			| Some e ->
				newline ctx;
				newline ctx;
				begin match cf.cf_kind with
					| Method _ ->
						let py_metas = filter_py_metas cf.cf_meta in
						gen_func_expr ctx e c field py_metas true "    " false cf.cf_pos;

					| _ ->
						gen_expr ctx e (Printf.sprintf "# var %s" field) "    ";
				end
		end

	let gen_class_empty_constructor ctx p cfl =
		if has_feature ctx "Type.createEmptyInstance" then begin
			newline ctx;
			newline ctx;
			print ctx "    @staticmethod\n    def _hx_empty_init(_hx_o):";
			let found_fields = ref false in
			List.iter (fun cf -> match cf.cf_kind with
					| Var ({v_read = AccResolve | AccCall}) ->
						()
					| Var _ ->
						found_fields := true;
						newline ctx;
						print ctx "        _hx_o.%s = None" (handle_keywords cf.cf_name)
					| _ ->
						()
			) cfl;
			if not !found_fields then
				spr ctx "        pass"
		end else begin
			newline ctx
		end

	let gen_class_statics ctx c p =
		let methods, other = List.partition (fun cf ->
			match cf.cf_kind with
			| Method _ -> (match cf.cf_expr with Some _ -> true | _ -> false)
			| _ -> false
		) c.cl_ordered_statics in

		(* generate non methods *)
		let has_empty_static_vars = ref false in
		List.iter (fun cf ->
			let p = get_path (t_infos (TClassDecl c)) in
			let field = handle_keywords cf.cf_name in
			match cf.cf_expr with
			| None ->
				has_empty_static_vars := true;
				newline ctx;
				print ctx "    %s = None" field
			| Some e ->
				(let f = fun () ->
					newline ctx;
					gen_expr ctx e (Printf.sprintf "%s.%s" p field) "";
				in
				ctx.static_inits <- f :: ctx.static_inits)
		) other;

		(* generate static methods *)
		let has_static_methods = ref false in
		List.iter (fun cf ->
			has_static_methods := true;
			let field = handle_keywords cf.cf_name in
			let py_metas = filter_py_metas cf.cf_meta in
			let e = match cf.cf_expr with Some e -> e | _ -> assert false in
			newline ctx;
			gen_func_expr ctx e c field py_metas false "    " true cf.cf_pos;
		) methods;

		!has_static_methods || !has_empty_static_vars

	let gen_class_init ctx c =
		match c.cl_init with
			| None ->
				()
			| Some e ->
				let is_math = c.cl_path = ([], "Math") in
				let math_feature = has_feature ctx "Math" in
				let f = if is_math && not math_feature then
					fun () -> ()
				else fun () ->
					let e = transform_expr e in
					newline ctx;
					spr ctx (texpr_str e (Printer.create_context "" ctx.com ctx.com.debug));
				in
				ctx.class_inits <- f :: ctx.class_inits

	let gen_class ctx c =
		if not c.cl_extern then begin
			let is_nativegen = Meta.has Meta.NativeGen c.cl_meta in
			let mt = (t_infos (TClassDecl c)) in
			let p = get_path mt in
			let p_name = get_full_name mt in
			let x = collect_class_field_data c.cl_ordered_fields in
			let p_super = match c.cl_super with
				| None ->
					None
				| Some (csup,_) ->
					Some (get_path (t_infos (TClassDecl csup)))
			in
			let p_interfaces = List.map (fun (c,tl) ->
				get_path (t_infos (TClassDecl c))
			) c.cl_implements in

			newline ctx;
			newline ctx;
			newline ctx;
			print ctx "class %s" p;
			(match p_super with Some p -> print ctx "(%s)" p | _ -> ());
			spr ctx ":";

			let use_pass = ref true in

			if not is_nativegen then begin
				if has_feature ctx "python._hx_class_name" then begin
					use_pass := false;
					print ctx "\n    _hx_class_name = \"%s\"" p_name
				end;

				let print_field names field quote =
					if has_feature ctx ("python." ^ field) then try
						let q s = if quote then "\"" ^ s ^ "\"" else s in
						let s = match names with
							| [] when (match c.cl_super with Some _ -> false | _ -> true) ->
								(* always overwrite parent's class fields *)
								raise Exit
							| _ ->
								"[" ^ (String.concat ", " (List.map q names)) ^ "]"
						in
						use_pass := false;
						print ctx "\n    %s = %s" field s
					with Exit -> ()
				in

				(try (
					let real_fields =
						List.filter (fun f -> match f.cf_kind with
							| Method MethDynamic -> raise Exit (* if a class has dynamic method, we can't use __slots__ because python will complain *)
							| Var _ -> is_physical_field f
							| _ -> false
						) c.cl_ordered_fields
					in
					let field_names = List.map (fun f -> handle_keywords f.cf_name) real_fields in
					let field_names = match c.cl_dynamic with Some _ -> "__dict__" :: field_names | None -> field_names in
					use_pass := false;
					print ctx "\n    __slots__ = (";
					(match field_names with
					| [] -> ()
					| [name] -> print ctx "\"%s\"," name
					| names -> print ctx "\"%s\"" (String.concat "\", \"" names));
					print ctx ")";
				) with Exit -> ());

				print_field x.cfd_fields "_hx_fields" true;
				print_field x.cfd_methods "_hx_methods" true;
				(* TODO: It seems strange to have a separation for member fields but a plain _hx_statics for static ones *)
				print_field (collect_class_statics_data c.cl_ordered_statics) "_hx_statics" true;
				print_field (p_interfaces) "_hx_interfaces" false;

				if has_feature ctx "python._hx_super" then (match p_super with
					| None -> ()
					| Some ps ->
						use_pass := false;
						print ctx "\n    _hx_super = %s\n" ps
				);

			end;

			begin match c.cl_constructor with
				| Some cf -> gen_class_constructor ctx c cf;
				| None -> ()
			end;
			List.iter (fun cf -> gen_class_field ctx c p cf) c.cl_ordered_fields;

			let has_inner_static = gen_class_statics ctx c p in

			let has_empty_constructor = match ((Meta.has Meta.NativeGen c.cl_meta) || c.cl_interface), c.cl_ordered_fields with
				| true,_
				| _, [] ->
					false
				| _ ->
					gen_class_empty_constructor ctx p c.cl_ordered_fields;
					has_feature ctx "Type.createEmptyInstance"
			in

			let use_pass = !use_pass && (not has_inner_static) && (not has_empty_constructor) && match x.cfd_methods with
				| [] -> c.cl_constructor = None
				| _ -> c.cl_interface
			in
			if use_pass then spr ctx "\n    pass";

			if not is_nativegen then begin
				if has_feature ctx "python._hx_class" then print ctx "\n%s._hx_class = %s" p p;
				if has_feature ctx "python._hx_classes" then print ctx "\n_hx_classes[\"%s\"] = %s" p_name p;
			end
		end;
		gen_class_init ctx c

	let gen_enum_metadata ctx en p =
		let meta = Codegen.build_metadata ctx.com (TEnumDecl en) in
		match meta with
			| None ->
				()
			| Some e ->
				newline ctx;
				print ctx "%s.__meta__ = " p;
				gen_expr ctx e "" ""

	let gen_enum ctx en =
		let mt = (t_infos (TEnumDecl en)) in
		let p = get_path mt in
		let p_name = get_full_name mt in

		let enum_constructs = PMap.foldi (fun k ef acc -> ef :: acc) en.e_constrs [] in
		let enum_constructs = List.sort (fun a b -> if a.ef_index < b.ef_index then -1 else if a.ef_index > b.ef_index then 1 else 0) enum_constructs in

		newline ctx;
		newline ctx;
		print ctx "class %s(Enum):" p;
		print ctx "\n    __slots__ = ()";

		if has_feature ctx "python._hx_class_name" then begin
			print ctx "\n    _hx_class_name = \"%s\"" p_name
		end;
		if has_feature ctx "python._hx_constructs" then begin
			let fix = match enum_constructs with [] -> "" | _ -> "\"" in
			let enum_constructs_str = fix ^ (String.concat ("\", \"") (List.map (fun ef -> ef.ef_name) enum_constructs)) ^ fix in
			print ctx "\n    _hx_constructs = [%s]" enum_constructs_str;
		end;

		let const_constructors,param_constructors = List.partition (fun ef ->
			match follow ef.ef_type with
			| TFun(_,_) -> false
			| _ -> true
		) enum_constructs in

		List.iter (fun ef ->
			match follow ef.ef_type with
			| TFun(args, _) ->
				let print_args args =
					let had_optional = ref false in
					let sl = List.map (fun (n,o,_) ->
						let name = handle_keywords n in
						let arg_value = if !had_optional then
							"= None"
						else if o then begin
							had_optional := true;
							" = None"
						end else
							""
						in
						Printf.sprintf "%s%s" name arg_value
					) args in
					String.concat "," sl
				in
				let f = handle_keywords ef.ef_name in
				let param_str = print_args args in
				let args_str = String.concat "," (List.map (fun (n,_,_) -> handle_keywords n) args) in
				newline ctx;
				newline ctx;
				print ctx "    @staticmethod\n    def %s(%s):\n" f param_str;
				print ctx "        return %s(\"%s\", %i, [%s])" p ef.ef_name ef.ef_index args_str;
			| _ -> assert false
		) param_constructors;

		List.iter (fun ef ->
			(* TODO: haxe source has api.quoteString for ef.ef_name *)
			let f = handle_keywords ef.ef_name in
			newline ctx;
			print ctx "%s.%s = %s(\"%s\", %i, list())" p f p ef.ef_name ef.ef_index
		) const_constructors;

		if has_feature ctx "python._hx_class" then print ctx "\n%s._hx_class = %s" p p;
		if has_feature ctx "python._hx_classes" then print ctx "\n_hx_classes[\"%s\"] = %s" p_name p;

		gen_enum_metadata ctx en p

	let gen_abstract ctx a =
		newline ctx;
		newline ctx;
		newline ctx;
		let mt = (t_infos (TAbstractDecl a)) in
		let p = get_path mt in
		print ctx "class %s: pass" p

	let gen_type ctx mt = match mt with
		| TClassDecl c -> gen_class ctx c
		| TEnumDecl en when not en.e_extern -> gen_enum ctx en
		| TAbstractDecl {a_path = [],"UInt"} -> ()
		| TAbstractDecl {a_path = [],"Enum"} -> ()
		| TAbstractDecl {a_path = [],"EnumValue"} when not (has_feature ctx "has_enum") -> ()
		| TAbstractDecl {a_path = [],"Void"} -> ()
		| TAbstractDecl {a_path = [],"Int"} when not (has_feature ctx "Int.*") -> ()
		| TAbstractDecl {a_path = [],"Float"} when not (has_feature ctx "Float.*") -> ()
		| TAbstractDecl {a_path = [],"Class"} when not (has_feature ctx "Class.*") -> ()
		| TAbstractDecl {a_path = [],"Dynamic"} when not (has_feature ctx "Dynamic.*") -> ()
		| TAbstractDecl {a_path = [],"Bool"} when not (has_feature ctx "Bool.*") -> ()

		| TAbstractDecl a when Meta.has Meta.RuntimeValue a.a_meta -> gen_abstract ctx a
		| _ -> ()

	(* Generator parts *)

	let gen_resources ctx =
		if Hashtbl.length ctx.com.resources > 0 then begin
			let slash_index = try (String.rindex ctx.com.file '/')+1 with Not_found -> 0 in
			let len = String.length ctx.com.file - slash_index in
			let file_name = String.sub ctx.com.file slash_index len in
			newline ctx;
			newline ctx;
			newline ctx;
			spr ctx "def _hx_resources__():";
			spr ctx "\n    import inspect";
			spr ctx "\n    import sys";
			spr ctx "\n    if not hasattr(sys.modules[__name__], '__file__'):";
			print ctx "\n        _file = '%s'" file_name;
			spr ctx "\n    else:";
			spr ctx "\n        _file = __file__";

			spr ctx "\n    return {";
			let first = ref true in
			Hashtbl.iter (fun k v ->
				let prefix = if !first then begin
					first := false;
					"";
				end else
					","
				in
				let k_enc = Codegen.escape_res_name k false in
				print ctx "%s\"%s\": open('%%s.%%s'%%(_file,'%s'),'rb').read()" prefix (Ast.s_escape k) k_enc;

				let f = open_out_bin (ctx.com.file ^ "." ^ k_enc) in
				output_string f v;
				close_out f
			) ctx.com.resources;
			spr ctx "}"
		end

	let gen_imports ctx =
		let import path meta =
			if Meta.has Meta.PythonImport meta && is_directly_used ctx.com meta then begin
				let _, args, mp = Meta.get Meta.PythonImport meta in

				let class_name = match path with
					| [],name -> name
					| path,name -> (ExtString.String.join "_" path) ^ "_" ^ name
				in

				let import_type,ignore_error = match args with
					| [(EConst(String(module_name,_)), _)]
					| [(EConst(String(module_name,_)), _); (EBinop(OpAssign, (EConst(Ident("ignoreError")),_), (EConst(Ident("false")),_)),_)] ->
						IModule module_name, false

					| [(EConst(String(module_name,_)), _); (EBinop(OpAssign, (EConst(Ident("ignoreError")),_), (EConst(Ident("true")),_)),_)] ->
						IModule module_name,true

					| [(EConst(String(module_name,_)), _); (EConst(String(object_name,_)), _)]
					| [(EConst(String(module_name,_)), _); (EConst(String(object_name,_)), _); (EBinop(OpAssign, (EConst(Ident("ignoreError")),_), (EConst(Ident("false")),_)),_)] ->
						IObject (module_name,object_name), false

					| [(EConst(String(module_name,_)), _); (EConst(String(object_name,_)), _); (EBinop(OpAssign, (EConst(Ident("ignoreError")),_), (EConst(Ident("true")),_)),_)] ->
						IObject (module_name,object_name), true
					| _ ->
						abort "Unsupported @:pythonImport format" mp
				in

				let import = match import_type with
					| IModule module_name ->
						(* importing whole module *)
						if module_name = class_name then
							"import " ^ module_name
						else
							"import " ^ module_name ^ " as " ^ class_name

					| IObject (module_name,object_name) ->
						if String.contains object_name '.' then
							(* importing nested class *)
							"import " ^ module_name ^ " as _hx_temp_import; " ^ class_name ^ " = _hx_temp_import." ^ object_name ^ "; del _hx_temp_import"
						else
							(* importing a class from a module *)
							if object_name = class_name then
								"from " ^ module_name ^ " import " ^ object_name
							else
								"from " ^ module_name ^ " import " ^ object_name ^ " as " ^ class_name
				in
				newline ctx;
				if ignore_error then begin
					spr ctx "try:\n    ";
					spr_line ctx import;
					spr ctx "except:\n    pass"
				end else
					spr ctx import
			end
		in
		List.iter (fun mt ->
			match mt with
			| TClassDecl c when c.cl_extern -> import c.cl_path c.cl_meta
			| TEnumDecl e when e.e_extern -> import e.e_path e.e_meta
			| _ -> ()
		) ctx.com.types

	let gen_types ctx =
		let used_paths = Hashtbl.create 0 in
		let find_type path =
			Hashtbl.add used_paths path true;
			Utils.find_type ctx.com path
		in
		let need_anon_for_trace = (has_feature ctx "has_anon_trace") && (has_feature ctx "haxe.Log.trace") in
		if (has_feature ctx "has_anon") || (has_feature ctx "_hx_AnonObject") || (has_feature ctx "has_metadata") || need_anon_for_trace then begin
			let with_body = (has_feature ctx "has_anon") || (has_feature ctx "has_metadata") || need_anon_for_trace in
			newline ctx;
			newline ctx;
			newline ctx;
			spr ctx "class _hx_AnonObject:\n";
			if with_body then begin
				spr ctx "    def __init__(self, fields):\n";
				spr ctx "        self.__dict__ = fields"
			end else
				spr ctx "    pass";
			Hashtbl.add used_paths ([],"_hx_AnonObject") true;
		end;
		if has_feature ctx "python._hx_classes" then begin
			newline ctx;
			newline ctx;
			newline ctx;
			spr ctx "_hx_classes = {}";
		end;
		if has_feature ctx "Boot.*" then
			gen_type ctx (find_type (["python"],"Boot"));
		if has_feature ctx "has_enum" || has_feature ctx "Enum.*" then
			gen_type ctx (find_type ([],"Enum"));
		if has_feature ctx "HxOverrides.*" then
			gen_type ctx (find_type ([],"HxOverrides"));
		List.iter (fun mt ->
			if not (Hashtbl.mem used_paths (t_infos mt).mt_path) then
				gen_type ctx mt
		) ctx.com.types

	let gen_static_inits ctx =
		newline ctx;
		List.iter (fun f -> f()) (List.rev ctx.static_inits)

	let gen_class_inits ctx =
		newline ctx;
		List.iter (fun f -> f()) (List.rev ctx.class_inits)

	let gen_main ctx =
		match ctx.com.main with
			| None ->
				()
			| Some e ->
				newline ctx;
				newline ctx;
				match e.eexpr with
				| TBlock el ->
					List.iter (fun e -> gen_expr ctx e "" ""; newline ctx) el
				| _ ->
					gen_expr ctx e "" ""

	(* Entry point *)

	let run com =
		Transformer.init com;
		let ctx = mk_context com in
		Codegen.map_source_header com (fun s -> print ctx "# %s\n# coding: utf-8\n" s);
		if has_feature ctx "closure_Array" || has_feature ctx "closure_String" then
			spr ctx "from functools import partial as _hx_partial";
		gen_imports ctx;
		gen_resources ctx;
		gen_types ctx;
		gen_class_inits ctx;
		gen_static_inits ctx;
		gen_main ctx;

		mkdir_from_path com.file;
		let ch = open_out_bin com.file in
		output_string ch (Buffer.contents ctx.buf);
		close_out ch
end

let generate com =
	Generator.run com
