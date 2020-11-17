class javadoc (doc : string) = object(self)
	method get_param_info (name : string) =
		(* TODO: Parse this properly *)
		let s = "@param " ^ name ^ " \\(.*\\)" in
		let reg = Str.regexp s in
		try
			ignore(Str.search_forward reg doc 0);
			Some (Str.matched_group 1 doc)
		with Not_found ->
			None
end