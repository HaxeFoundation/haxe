open Meta
open Type
open Error

let has_direct_to ab pl b =
	List.exists (unify_to ab pl ~allow_transitive_cast:false b) ab.a_to

let has_direct_from ab pl a b =
	List.exists (unify_from ab pl a ~allow_transitive_cast:false b) ab.a_from

let find_field_to ab pl b =
	List.find (unify_to_field ab pl b) ab.a_to_field

let find_field_from ab pl a b =
	List.find (unify_from_field ab pl a b) ab.a_from_field

let find_to_from f ab_left tl_left ab_right tl_right tleft tright =
	if has_direct_to ab_right tl_right tleft || has_direct_from ab_left tl_left tright tleft then
		raise Not_found
	else
		try f ab_right tl_right (fun () -> find_field_to ab_right tl_right tleft)
		with Not_found -> f ab_left tl_left (fun () -> find_field_from ab_left tl_left tright tleft)

let find_to ab pl b =
	if follow b == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_to_field
	else if has_direct_to ab pl b then
		raise Not_found (* legacy compatibility *)
	else
		find_field_to ab pl b

let find_from ab pl a b =
	if follow a == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_from_field
	else if has_direct_from ab pl a b then
		raise Not_found (* legacy compatibility *)
	else
		find_field_from ab pl a b

let underlying_type_stack = ref []

let rec get_underlying_type a pl =
	let maybe_recurse t =
		underlying_type_stack := (TAbstract(a,pl)) :: !underlying_type_stack;
		let rec loop t = match t with
			| TMono r ->
				(match !r with
				| Some t -> loop t
				| _ -> t)
			| TLazy f ->
				loop (lazy_type f)
			| TAbstract({a_path=([],"Null")} as a,[t1]) ->
				TAbstract(a,[loop t1])
			| TType (t,tl) ->
				loop (apply_params t.t_params tl t.t_type)
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				if List.exists (fast_eq t) !underlying_type_stack then begin
					let pctx = print_context() in
					let s = String.concat " -> " (List.map (fun t -> s_type pctx t) (List.rev (t :: !underlying_type_stack))) in
					underlying_type_stack := [];
					error ("Abstract chain detected: " ^ s) a.a_pos
				end;
				get_underlying_type a tl
			| _ ->
				t
		in
		let t = loop t in
		underlying_type_stack := List.tl !underlying_type_stack;
		t
	in
	try
		if not (Meta.has Meta.MultiType a.a_meta) then raise Not_found;
		let m = mk_mono() in
		let _ = find_to a pl m in
		maybe_recurse (follow m)
	with Not_found ->
		if Meta.has Meta.CoreType a.a_meta then
			t_dynamic
		else
			maybe_recurse (apply_params a.a_params pl a.a_this)

let rec follow_with_abstracts t = match follow t with
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		follow_with_abstracts (get_underlying_type a tl)
	| t ->
		t