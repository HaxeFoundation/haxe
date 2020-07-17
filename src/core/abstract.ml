open Meta
open TType
open TFunctions
open TPrinting
open TUnification
open Error

let build_abstract a = match a.a_impl with
	| Some c -> ignore(c.cl_build())
	| None -> ()

let has_direct_from uctx a b ab tl =
	unifies_from {uctx with allow_transitive_cast = false} a b ab tl

let has_direct_to uctx a b ab tl =
	unifies_to {uctx with allow_transitive_cast = false} a b ab tl

let find_field_from uctx a b ab tl =
	List.find (unifies_from_field uctx a b ab tl) ab.a_from_field

let find_field_to uctx a b ab tl =
	List.find (unifies_to_field uctx a b ab tl) ab.a_to_field

let find_to_from uctx a b a1 tl1 a2 tl2 =
	build_abstract a1;
	build_abstract a2;
	if has_direct_to uctx a b a1 tl1 || has_direct_from uctx a b a2 tl2 then
		raise Not_found
	else try
		(a1,tl1,find_field_to uctx a b a1 tl1)
	with Not_found ->
		(a2,tl2,find_field_from uctx a b a2 tl2)

let find_from uctx a ab tl =
	let b = TAbstract(ab,tl) in
	build_abstract ab;
	if follow a == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_from_field
	else if has_direct_from uctx a b ab tl then
		raise Not_found (* legacy compatibility *)
	else
		find_field_from uctx a b ab tl

let find_to uctx b ab tl =
	let a = TAbstract(ab,tl) in
	build_abstract ab;
	if follow b == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_to_field
	else if has_direct_to uctx a b ab tl then
		raise Not_found (* legacy compatibility *)
	else
		find_field_to uctx a b ab tl

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
		let _ = find_to default_unification_context m a pl in
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
