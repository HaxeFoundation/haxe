open Type

let get_instance_overloads c name =
	let acc = DynArray.create () in
	let rec loop c =
		ignore(c.cl_build());
		begin try
			let cf = PMap.find name c.cl_fields in
			List.iter (DynArray.add acc) (cf :: cf.cf_overloads);
		with Not_found ->
			()
		end;
		if has_class_flag c CInterface then
			List.iter (fun (c,_) ->
				loop c
			) c.cl_implements
		else match c.cl_super with
			| Some(c,_) ->
				loop c
			| None ->
				()
	in
	loop c;
	DynArray.to_list acc