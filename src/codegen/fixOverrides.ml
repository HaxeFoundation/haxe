open Globals
open Common
open Type

(* -------------------------------------------------------------------------- *)
(* FIX OVERRIDES *)

(*
	on some platforms which doesn't support type parameters, we must have the
	exact same type for overridden/implemented function as the original one
*)

let rec find_field com c f =
	try
		(match c.cl_super with
		| None ->
			raise Not_found
		| Some ( {cl_path = (["cpp"],"FastIterator")}, _ ) ->
			raise Not_found (* This is a strongly typed 'extern' and the usual rules don't apply *)
		| Some (c,_) ->
			find_field com c f)
	with Not_found -> try
		if com.platform = Cpp || com.platform = Hl then (* uses delegation for interfaces *)
			raise Not_found;
		let rec loop = function
			| [] ->
				raise Not_found
			| (c,_) :: l ->
				try
					find_field com c f
				with
					Not_found -> loop l
		in
		loop c.cl_implements
	with Not_found ->
		let f = PMap.find f.cf_name c.cl_fields in
		(match f.cf_kind with Var { v_read = AccRequire _ } -> raise Not_found | _ -> ());
		f

let fix_override com c f fd =
	let f2 = (try Some (find_field com c f) with Not_found -> None) in
	match f2,fd with
		| Some (f2), Some(fd) ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> die "" __LOC__) in
			let changed_args = ref [] in
			let prefix = "_tmp_" in
			let nargs = List.map2 (fun ((v,ct) as cur) (_,_,t2) ->
				try
					type_eq EqStrict (monomorphs c.cl_params (monomorphs f.cf_params v.v_type)) t2;
					(* Flash generates type parameters with a single constraint as that constraint type, so we
					   have to detect this case and change the variable (issue #2712). *)
					begin match follow v.v_type with
						| TInst({cl_kind = KTypeParameter ttp} as cp,_) when com.platform = Flash ->
							begin match get_constraints ttp with
							| [tc] ->
								if List.exists (fun tp -> tp.ttp_name = (snd cp.cl_path)) c.cl_params then raise (Unify_error [])
							| _ ->
								()
							end
						| _ ->
							()
					end;
					cur
				with Unify_error _ ->
					let v2 = alloc_var VGenerated (prefix ^ v.v_name) t2 v.v_pos in
					changed_args := (v,v2) :: !changed_args;
					v2,ct
			) fd.tf_args targs in
			let fd2 = {
				tf_args = nargs;
				tf_type = tret;
				tf_expr = (match List.rev !changed_args with
					| [] -> fd.tf_expr
					| args ->
						let e = fd.tf_expr in
						let el = (match e.eexpr with TBlock el -> el | _ -> [e]) in
						let p = (match el with [] -> e.epos | e :: _ -> e.epos) in
						let el_v = List.map (fun (v,v2) ->
							mk (TVar (v,Some (mk (TCast (mk (TLocal v2) v2.v_type p,None)) v.v_type p))) com.basic.tvoid p
						) args in
						{ e with eexpr = TBlock (el_v @ el) }
				);
			} in
			let targs = List.map (fun(v,c) -> (v.v_name, Option.is_some c, v.v_type)) nargs in
			let fde = (match f.cf_expr with None -> die "" __LOC__ | Some e -> e) in
			f.cf_expr <- Some { fde with eexpr = TFunction fd2 };
			f.cf_type <- TFun(targs,tret);
		| Some(f2), None when (has_class_flag c CInterface) ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> die "" __LOC__) in
			f.cf_type <- TFun(targs,tret)
		| _ ->
			()

let fix_overrides com t =
	match t with
	| TClassDecl c ->
		(* overrides can be removed from interfaces *)
		if (has_class_flag c CInterface) then
			c.cl_ordered_fields <- List.filter (fun f ->
				try
					if find_field com c f == f then raise Not_found;
					c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
					false;
				with Not_found ->
					true
			) c.cl_ordered_fields;
		List.iter (fun f ->
			match f.cf_expr, f.cf_kind with
			| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
				fix_override com c f (Some fd)
			| None, Method (MethNormal | MethInline) when (has_class_flag c CInterface) ->
				fix_override com c f None
			| _ ->
				()
		) c.cl_ordered_fields
	| _ ->
		()

(*
	PHP does not allow abstract classes extending other abstract classes to override any fields, so these duplicates
	must be removed from the child interface
*)
let fix_abstract_inheritance com t =
	match t with
	| TClassDecl c when (has_class_flag c CInterface) ->
		c.cl_ordered_fields <- List.filter (fun f ->
			let b = try (find_field com c f) == f
			with Not_found -> false in
			if not b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
			b;
		) c.cl_ordered_fields
	| _ -> ()