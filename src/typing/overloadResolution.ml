open Typecore
open TType
open TUnification
open TFunctions

let unify_cf map_type c cf el =
	let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
	match follow (apply_params cf.cf_params monos (map_type cf.cf_type)) with
		| TFun(tl'',ret) as tf ->
			let rec loop2 acc el tl = match el,tl with
				| e :: el,(_,o,t) :: tl ->
					begin try
						Type.unify e.etype t;
						loop2 ((e,o) :: acc) el tl
					with _ ->
						match t,tl with
						| TAbstract({a_path=["haxe";"extern"],"Rest"},[t]),[] ->
							begin try
								let el = List.map (fun e -> unify t e.etype; e,o) el in
								let fcc = make_field_call_candidate ((List.rev acc) @ el) ret monos tf cf (c,cf,monos) in
								Some fcc
							with _ ->
								None
							end
						| _ ->
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
	let matches = ref [] in
	let rec loop cfl = match cfl with
		| cf :: cfl ->
			begin match unify_cf map_type c cf el with
			| Some r -> matches := r :: !matches;
			| None -> ()
			end;
			loop cfl
		| [] ->
			List.rev !matches
	in
	loop (cf :: cf.cf_overloads)

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
				List.iter (fun (c,tl) -> loop (fun t -> apply_params c.cl_params (List.map map_type tl) t) c) c.cl_implements
			else match c.cl_super with
			| None -> ()
			| Some(c,tl) -> loop (fun t -> apply_params c.cl_params (List.map map_type tl) t) c
		end;
	in
	loop map_type c;
	filter_overloads (List.rev !candidates)

let maybe_resolve_instance_overload is_ctor map_type c cf el =
	if has_class_field_flag cf CfOverload || cf.cf_overloads <> [] then
		resolve_instance_overload is_ctor map_type c cf.cf_name el
	else match unify_cf map_type c cf el with
		| Some fcc -> Some (fcc.fc_data)
		| None -> Some(c,cf,List.map snd cf.cf_params)