open TType
open TUnification
open TFunctions
open TOther
open FieldCallCandidate

(* Try the functional-interface conversion path: if [t_param] is a SAM class
   and [t_arg] is a function type, unify the function against the SAM method's
   signature (mirroring what AbstractCast does at cast time). Returns true if
   compatible. Used during overload candidate filtering so SAM-parameter
   overloads aren't discarded before the cast layer runs — but unlike a blind
   accept, this still rejects lambdas whose shape doesn't match the SAM. *)
let try_functional_interface_match t_param t_arg =
	match follow t_param, follow t_arg with
	| TInst(c,tl), TFun _ when has_class_flag c CFunctionalInterface ->
		begin match TClass.get_singular_interface_field c.cl_ordered_fields with
		| None -> false
		| Some cf ->
			let map = apply_params c.cl_params tl in
			let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
			let expected = map (apply_params cf.cf_params monos cf.cf_type) in
			try Type.unify t_arg expected; true
			with Unify_error _ -> false
		end
	| _ -> false

let unify_cf map_type c cf el =
	let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
	match follow (apply_params cf.cf_params monos (map_type cf.cf_type)) with
		| TFun(tl'',ret) as tf ->
			let rec loop2 acc el tl = match el,tl with
				| e :: el,(_,o,t) :: tl ->
					begin try
						(try Type.unify e.etype t
						 with Unify_error _ when try_functional_interface_match t e.etype -> ());
						loop2 (e :: acc) el tl
					with _ ->
						if Type.ExtType.is_rest (follow t) then
							match follow t,tl with
							| TAbstract({a_path=["haxe"],"Rest"},[t]),[] ->
								begin try
									let el = List.map (fun e -> unify t e.etype; e) el in
									let fcc = make_field_call_candidate ((List.rev acc) @ el) ret monos tf cf (c,cf,monos) in
									Some fcc
								with _ ->
									None
								end
							| _ ->
								Globals.die "" __LOC__
						else
							None
					end
				| [],[] ->
					let fcc = make_field_call_candidate (List.rev acc) ret monos tf cf (c,cf,monos) in
					Some fcc
				| _ ->
					None
			in
			loop2 [] el tl''
		| t ->
			None

let find_overload map_type c cf el =
	ExtList.List.filter_map (fun cf -> unify_cf map_type c cf el) (cf :: cf.cf_overloads)

let filter_overloads candidates =
	match Overloads.Resolution.reduce_compatible candidates with
	| [fcc] -> Some(fcc.fc_data)
	| [] -> None
	| ((fcc) :: _) (* as resolved *) ->
		(* let st = s_type (print_context()) in
		print_endline (Printf.sprintf "Ambiguous overload for %s(%s)" name (String.concat ", " (List.map (fun e -> st e.etype) el)));
		List.iter (fun (_,t,(c,cf)) ->
			print_endline (Printf.sprintf "\tCandidate: %s.%s(%s)" (s_type_path c.cl_path) cf.cf_name (st t));
		) resolved; *)
		Some(fcc.fc_data)

let resolve_instance_overload is_ctor map_type c name el =
	let candidates = ref [] in
	let has_function t1 fcc2 =
		begin match follow t1,fcc2.fc_type with
		| TFun(tl1,_),TFun(tl2,_) -> type_iseq (TFun(tl1,t_dynamic)) (TFun(tl2,t_dynamic))
		| _ -> false
		end
	in
	let rec loop map_type c =
		begin try
			let cf = if is_ctor then
				(match c.cl_constructor with Some cf -> cf | None -> raise Not_found)
			else
				PMap.find name c.cl_fields
			in
			begin match find_overload map_type c cf el with
			| [] -> raise Not_found
			| l ->
				List.iter (fun fcc ->
					if not (List.exists (has_function fcc.fc_type) !candidates) then candidates := fcc :: !candidates
				) l
			end;
			if has_class_field_flag cf CfOverload || cf.cf_overloads <> [] then raise Not_found
		with Not_found ->
			if (has_class_flag c CInterface) then
				List.iter (fun (c,tl) -> loop (apply_params c.cl_params (List.map map_type tl)) c) c.cl_implements
			else match c.cl_super with
			| None -> ()
			| Some(c,tl) -> loop (apply_params c.cl_params (List.map map_type tl)) c
		end;
	in
	loop map_type c;
	filter_overloads (List.rev !candidates)

let maybe_resolve_instance_overload is_ctor map_type c cf el =
	if has_class_field_flag cf CfOverload || cf.cf_overloads <> [] then
		resolve_instance_overload is_ctor map_type c cf.cf_name el
	else match unify_cf map_type c cf el with
		| Some fcc -> Some (fcc.fc_data)
		| None -> Some(c,cf,extract_param_types cf.cf_params)

let maybe_resolve_constructor_overload c tl el =
	let cf,c,tl = get_constructor_class c tl in
	maybe_resolve_instance_overload true (apply_params c.cl_params tl) c cf el

(*
	After inlining or generic substitution, [Type.map_expr_type] has re-resolved
	TField nodes by name through [quick_field], which collapses an overloaded
	method to its first declared overload while leaving the field's applied type
	untouched. This restores the correct overload on such call nodes (other nodes
	are returned unchanged - hence [maybe_]).

	The matching overload is re-selected structurally from the (already mapped)
	argument types and just the field reference is restored, which is what code
	generation relies on. This needs no typer: the arguments are already typed,
	so resolution only unifies their types against each candidate.
*)
let maybe_reapply_overload_call e =
	match e.eexpr with
		| TCall({eexpr = TField(e1,fa)} as ef,el) ->
			let rebuild cf' =
				let fa = match fa with
					| FInstance(c,tl,_) -> FInstance(c,tl,cf')
					| FStatic(c,_) -> FStatic(c,cf')
					| _ -> fa
				in
				{e with eexpr = TCall({ef with eexpr = TField(e1,fa)},el)}
			in
			begin match fa with
			| FStatic(c,cf) when has_class_field_flag cf CfOverload ->
				begin match filter_overloads (find_overload (fun t -> t) c cf el) with
				| Some(_,cf',_) -> rebuild cf'
				| None -> e
				end
			| FInstance(c,tl,cf) when has_class_field_flag cf CfOverload ->
				let map_type = apply_params c.cl_params tl in
				begin match resolve_instance_overload false map_type c cf.cf_name el with
				| Some(_,cf',_) -> rebuild cf'
				| None -> e
				end
			| _ ->
				e
			end
		| _ ->
			e
