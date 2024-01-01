open Type

let get_instance_overloads c cf =
	let acc = DynArray.create () in
	let rec loop c cf =
		ignore(c.cl_build());
		List.iter (DynArray.add acc) (cf :: cf.cf_overloads);
		let maybe_recurse c =
			try
				loop c (PMap.find cf.cf_name c.cl_fields)
			with Not_found ->
				()
		in
		if has_class_flag c CInterface then
			List.iter (fun (c,_) ->
				maybe_recurse c
			) c.cl_implements
		else match c.cl_super with
			| Some(c,_) ->
				maybe_recurse c
			| None ->
				()
	in
	loop c cf;
	DynArray.to_list acc