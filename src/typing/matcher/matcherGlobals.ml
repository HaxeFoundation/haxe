open Type

exception Internal_match_failure

let s_type t = s_type (print_context()) t
let s_expr_pretty e = s_expr_pretty false "" false s_type e

let make_offset_list left right middle other =
	(ExtList.List.make left other) @ [middle] @ (ExtList.List.make right other)

(* Like is_explicit_null, but follows abstract underlying types. *)
let rec is_explicit_null_or_abstract_over_that = function
	| TMono r ->
		(match r.tm_type with None -> false | Some t -> is_explicit_null_or_abstract_over_that t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		true
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		is_explicit_null_or_abstract_over_that (Abstract.get_underlying_type a tl)
	| TLazy f ->
		is_explicit_null_or_abstract_over_that (lazy_type f)
	| TType (t,tl) ->
		is_explicit_null_or_abstract_over_that (apply_typedef t tl)
	| _ ->
		false