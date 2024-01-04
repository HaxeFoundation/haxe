open Type

exception Internal_match_failure

let s_type t = s_type (print_context()) t
let s_expr_pretty e = s_expr_pretty false "" false s_type e

let make_offset_list left right middle other =
	(ExtList.List.make left other) @ [middle] @ (ExtList.List.make right other)
