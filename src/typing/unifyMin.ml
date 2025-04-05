open Globals
open Type
open Error

exception NoValue

let rec unify_min_raise basic el =
	match el with
	| [] ->
		raise NoValue
	| [e] ->
		e.etype
	| _ ->
		let rec chk_null e = is_null e.etype || is_explicit_null e.etype ||
			match e.eexpr with
			| TConst TNull -> true
			| TBlock el ->
				(match List.rev el with
				| [] -> false
				| e :: _ -> chk_null e)
			| TParenthesis e | TMeta(_,e) -> chk_null e
			| _ -> false
		in
		(* First pass: Try normal unification and find out if null is involved. *)
		let rec loop t = function
			| [] ->
				let t = match t with
					| Some t ->
						t
					| None ->
						(* The only way to get here is if all types were Dynamic. We know there's more than 0
						   because in that case we raise NoValue above. *)
						t_dynamic
				in
				(false,t)
			| e :: el when (follow e.etype == t_dynamic) ->
				loop t el
			| e :: el ->
				begin match t with
					| None ->
						loop (Some (if chk_null e then basic.tnull e.etype else e.etype)) el
					| Some t ->
						let t = if chk_null e then basic.tnull t else t in
						try
							Type.unify e.etype t;
							loop (Some t) el
						with Unify_error _ -> try
							Type.unify t e.etype;
							loop (Some (if is_null t then basic.tnull e.etype else e.etype)) el
						with Unify_error _ ->
							true, t
				end
		in
		let has_error, t = loop None el in
		if not has_error then
			t
		else try
			(* specific case for const anon : we don't want to hide fields but restrict their common type *)
			let fcount = ref (-1) in
			let field_count a =
				PMap.fold (fun _ acc -> acc + 1) a.a_fields 0
			in
			let expr f = match f.cf_expr with None -> mk (TBlock []) f.cf_type f.cf_pos | Some e -> e in
			let fields = List.fold_left (fun acc e ->
				match follow e.etype with
				| TAnon a when !(a.a_status) = Const ->
					if !fcount = -1 then begin
						fcount := field_count a;
						PMap.map (fun f -> [expr f]) a.a_fields
					end else begin
						if !fcount <> field_count a then raise Not_found;
						PMap.mapi (fun n el -> expr (PMap.find n a.a_fields) :: el) acc
					end
				| _ ->
					raise Not_found
			) PMap.empty el in
			let fields = PMap.foldi (fun n el acc ->
				let t = try unify_min_raise basic el with Unify_error _ -> raise Not_found in
				PMap.add n (mk_field n t (List.hd el).epos null_pos) acc
			) fields PMap.empty in
			mk_anon ~fields (ref Closed)
		with Not_found -> try
			(* specific case for TFun, see #9579 *)
			let e0,el = match el with
				| e0 :: el -> e0,el
				| _ -> raise Exit
			in
			let args,tr0 = match follow e0.etype with
				| TFun(tl,tr) ->
					Array.of_list tl,tr
				| _ ->
					raise Exit
			in
			let arity = Array.length args in
			let rets = List.map (fun e -> match follow e.etype with
				| TFun(tl,tr) ->
					let ta = Array.of_list tl in
					if Array.length ta <> arity then raise Exit;
					for i = 0 to arity - 1 do
						let (_,_,tcur) = args.(i) in
						let (_,_,tnew) as argnew = ta.(i) in
						if Type.does_unify tnew tcur then
							args.(i) <- argnew
						else if not (Type.does_unify tcur tnew) then
							raise Exit
					done;
					tr
				| _ ->
					raise Exit
			) el in
			let common_types = UnifyMinT.collect_base_types tr0 in
			let tr = match UnifyMinT.unify_min' (default_unification_context()) common_types rets with
			| UnifyMinOk t ->
				t
			| UnifyMinError(l,index) ->
				raise Exit
			in
			TFun(Array.to_list args,tr)
		with Exit ->
			(* Second pass: Get all base types (interfaces, super classes and their interfaces) of most general type.
			   Then for each additional type filter all types that do not unify. *)
			let common_types = UnifyMinT.collect_base_types t in
			let dyn_types = List.fold_left (fun acc t ->
				let rec loop c =
					Meta.has Meta.UnifyMinDynamic c.cl_meta || (match c.cl_super with None -> false | Some (c,_) -> loop c)
				in
				match t with
				| TInst (c,params) when params <> [] && loop c ->
					TInst (c,List.map (fun _ -> t_dynamic) params) :: acc
				| _ -> acc
			) [] common_types in
			let common_types = (match List.rev dyn_types with [] -> common_types | l -> common_types @ l) in
			let el = List.tl el in
			let tl = List.map (fun e -> e.etype) el in
			begin match UnifyMinT.unify_min' (default_unification_context()) common_types tl with
			| UnifyMinOk t ->
				t
			| UnifyMinError(l,index) ->
				raise_typing_error_ext (make_error (Unify l) (List.nth el index).epos)
			end