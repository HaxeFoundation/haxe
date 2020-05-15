open Meta
open TType
open TFunctions
open TPrinting
open TUnification
open Error

let build_abstract a = match a.a_impl with
	| Some c -> ignore(c.cl_build())
	| None -> ()

let has_direct_to uctx ab pl b =
	List.exists (unify_to uctx ab pl ~allow_transitive_cast:false b) ab.a_to

let has_direct_from uctx ab pl a b =
	List.exists (unify_from uctx ab pl a ~allow_transitive_cast:false b) ab.a_from

let find_field_to uctx ab pl b =
	build_abstract ab;
	List.find (unify_to_field uctx ab pl b) ab.a_to_field

let find_field_from uctx ab pl a b =
	build_abstract ab;
	List.find (unify_from_field uctx ab pl a b) ab.a_from_field

let find_to_from uctx f ab_left tl_left ab_right tl_right tleft tright =
	build_abstract ab_left;
	build_abstract ab_right;
	if has_direct_to uctx ab_right tl_right tleft || has_direct_from uctx ab_left tl_left tright tleft then
		raise Not_found
	else
		try f ab_right tl_right (fun () -> find_field_to uctx ab_right tl_right tleft)
		with Not_found -> f ab_left tl_left (fun () -> find_field_from uctx ab_left tl_left tright tleft)

let find_to uctx ab pl b =
	build_abstract ab;
	if follow b == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_to_field
	else if has_direct_to uctx ab pl b then
		raise Not_found (* legacy compatibility *)
	else
		find_field_to uctx ab pl b

let find_from uctx ab pl a b =
	build_abstract ab;
	if follow a == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_from_field
	else if has_direct_from uctx ab pl a b then
		raise Not_found (* legacy compatibility *)
	else
		find_field_from uctx ab pl a b

let underlying_type_stack = new_rec_stack()

let rec get_underlying_type ?(return_first=false) a pl =
	let maybe_recurse t =
		let rec loop t = match t with
			| TMono r ->
				(match r.tm_type with
				| Some t -> loop t
				| _ -> t)
			| TLazy f ->
				loop (lazy_type f)
			| TAbstract({a_path=([],"Null")} as a,[t1]) ->
				TAbstract(a,[loop t1])
			| TType (t,tl) ->
				loop (apply_params t.t_params tl t.t_type)
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				if rec_stack_exists (fast_eq t) underlying_type_stack then begin
					let pctx = print_context() in
					let s = String.concat " -> " (List.map (fun t -> s_type pctx t) (List.rev (t :: underlying_type_stack.rec_stack))) in
					error ("Abstract chain detected: " ^ s) a.a_pos
				end;
				get_underlying_type a tl
			| _ ->
				t
		in
		(*
			Even if only the first underlying type was requested
			keep traversing to detect mutually recursive abstracts
		*)
		let result = rec_stack_loop underlying_type_stack (TAbstract(a,pl)) loop t in
		if return_first then t
		else result
	in
	try
		if not (Meta.has Meta.MultiType a.a_meta) then raise Not_found;
		(* TODO:
			Look into replacing `mk_mono` & `find_to` with `build_abstract a` & `TAbstract(a, pl)`.
			`find_to` is probably needed for `@:multiType`
		*)
		let m = mk_mono() in
		let _ = find_to default_unification_context a pl m in
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

let rec follow_with_abstracts_without_null t = match follow_without_null t with
	| TAbstract({a_path = [],"Null"},_) ->
		t
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		follow_with_abstracts_without_null (get_underlying_type a tl)
	| t ->
		t