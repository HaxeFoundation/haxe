open Globals
open Type

type bind_status =
	| BindUsed
	| BindUnused
	| BindDeferred

type bind = {
	b_var : tvar;
	b_pos : pos;
	b_expr : texpr;
	mutable b_status : bind_status;
}
