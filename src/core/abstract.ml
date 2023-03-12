open Globals
open Ast
open Meta
open TType
open TFunctions
open TPrinting
open TUnification
open Error

let build_abstract a = match a.a_impl with
	| Some c -> ignore(c.cl_build())
	| None -> ()

let find_cast_field uctx find =
	let found = try
		find {uctx with allow_transitive_cast = false}
	with Not_found ->
		find uctx
	in
	match found with
		| Some value -> value
		| None -> raise Not_found

let find_field_from uctx a b ab tl =
	List.find (unifies_from_field uctx a b ab tl) ab.a_from_field

let find_field_to uctx a b ab tl =
	List.find (unifies_to_field uctx a b ab tl) ab.a_to_field

let find_to_from uctx a b a1 tl1 a2 tl2 =
	build_abstract a1;
	build_abstract a2;
	find_cast_field uctx (fun uctx ->
		if unifies_abstracts uctx a b a1 tl1 a2 tl2 then
			None
		else try
			Some((a1,tl1,(find_field_to uctx a b a1 tl1)))
		with Not_found ->
			Some((a2,tl2,(find_field_from uctx a b a2 tl2)))
	)

let find_from uctx a ab tl =
	build_abstract ab;
	if follow a == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_from_field
	else
		let b = TAbstract(ab,tl) in
		find_cast_field uctx (fun uctx ->
			if unifies_from uctx a b ab tl then
				None
			else
				Some(find_field_from uctx a b ab tl)
		)

let find_to uctx b ab tl =
	build_abstract ab;
	if follow b == t_dynamic then
		List.find (fun (t,_) -> follow t == t_dynamic) ab.a_to_field
	else
		let a = TAbstract(ab,tl) in
		find_cast_field uctx (fun uctx ->
			if unifies_to uctx a b ab tl then
				None
			else
				Some(find_field_to uctx a b ab tl)
		)

let underlying_type_stack = new_rec_stack()

(**
	Returns type parameters and the list of types, which should be known at compile time
	to be able to choose multitype specialization.
*)
let rec find_multitype_params a pl =
	match Meta.get Meta.MultiType a.a_meta with
	| _,[],_ -> pl,pl
	| _,el,_ ->
		let relevant = Hashtbl.create 0 in
		List.iter (fun e ->
			let rec loop f e = match fst e with
				| EConst(Ident s) ->
					Hashtbl.replace relevant s f
				| EMeta((Meta.Custom ":followWithAbstracts",_,_),e1) ->
					loop follow_with_abstracts e1;
				| _ ->
					str_typing_error "Type parameter expected" (pos e)
			in
			loop (fun t -> t) e
		) el;
		let definitive_types = ref [] in
		let tl = List.map2 (fun tp t ->
			try
				let t = (Hashtbl.find relevant tp.ttp_name) t in
				definitive_types := t :: !definitive_types;
				t
			with Not_found ->
				if not (has_mono t) then t
				else t_dynamic
		) a.a_params pl in
		tl,!definitive_types

and find_multitype_specialization_type a pl =
	let uctx = default_unification_context in
	let m = mk_mono() in
	let tl,definitive_types = find_multitype_params a pl in
	ignore(find_to uctx m a tl);
	if List.exists (fun t -> has_mono t) definitive_types then raise Not_found;
	follow m

and get_underlying_type ?(return_first=false) a pl =
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
				loop (apply_typedef t tl)
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				if rec_stack_exists (fast_eq t) underlying_type_stack then begin
					let pctx = print_context() in
					let s = String.concat " -> " (List.map (fun t -> s_type pctx t) (List.rev (t :: underlying_type_stack.rec_stack))) in
					str_typing_error ("Abstract chain detected: " ^ s) a.a_pos
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
		find_multitype_specialization_type a pl
	with Not_found ->
		if Meta.has Meta.CoreType a.a_meta then
			t_dynamic
		else
			maybe_recurse (apply_params a.a_params pl a.a_this)

and follow_with_abstracts t = match follow t with
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		follow_with_abstracts (get_underlying_type a tl)
	| t ->
		t

let rec follow_with_forward_ctor ?(build=false) t = match follow t with
	| TAbstract(a,tl) as t ->
		if build then build_abstract a;
		if Meta.has Meta.ForwardNew a.a_meta && not (match a.a_impl with
			| Some c -> PMap.mem "_new" c.cl_statics
			| None -> false
		) then
			follow_with_forward_ctor (get_underlying_type ~return_first:true a tl)
		else
			t
	| t ->
		t

let rec follow_with_abstracts_without_null t = match follow_without_null t with
	| TAbstract({a_path = [],"Null"},_) ->
		t
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		follow_with_abstracts_without_null (get_underlying_type a tl)
	| t ->
		t
