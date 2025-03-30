let with_timer detail_times label identifier f =
	let label = match detail_times,identifier with
		| 0,_ -> ["filters"]
		| 1,_ -> "filters" :: label :: []
		| _,Some identifier -> "filters" :: label :: identifier :: []
		| _ -> ["filters"]
	in
	let timer = Timer.timer label in
	Std.finally timer f ()

open Type

let rec is_removable_class c =
	match c.cl_kind with
	| KGeneric ->
		(Meta.has Meta.Remove c.cl_meta ||
		(match c.cl_super with
			| Some (c,_) -> is_removable_class c
			| _ -> false) ||
		List.exists (fun tp ->
			has_ctor_constraint tp.ttp_class || Meta.has Meta.Const tp.ttp_class.cl_meta
		) c.cl_params)
	| KTypeParameter _ ->
		(* this shouldn't happen, have to investigate (see #4092) *)
		true
	| _ ->
		false

let is_removable_field is_macro_context f =
	not (has_class_field_flag f CfOverride) && (
		has_class_field_flag f CfExtern || has_class_field_flag f CfGeneric
		|| (match f.cf_kind with
			| Var {v_read = AccRequire (s,_)} -> true
			| Method MethMacro -> not is_macro_context
			| _ -> false)
	)