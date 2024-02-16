open Globals
open Type

type coro_block = {
	cb_id : int;
	cb_el : texpr DynArray.t;
	cb_typepos : (Type.t * pos) option;
	cb_catch : coro_block option;
	mutable cb_next : coro_next;
}

and coro_next_kind =
	| NextUnknown
	| NextSub of coro_block * coro_block
	| NextReturnVoid
	| NextReturn of texpr
	| NextThrow of texpr
	| NextIfThen of texpr * coro_block * coro_block
	| NextIfThenElse of texpr * coro_block * coro_block * coro_block
	| NextSwitch of coro_switch * coro_block
	| NextWhile of texpr * coro_block * coro_block
	| NextTry of coro_block * coro_catch * coro_block
	| NextSuspend of coro_suspend * coro_block
	(* graph connections from here on, careful with traversal *)
	| NextBreak of coro_block
	| NextContinue of coro_block
	| NextFallThrough of coro_block
	| NextGoto of coro_block

and coro_switch = {
	cs_subject : texpr;
	cs_cases : (texpr list * coro_block) list;
	cs_default : coro_block option;
	cs_exhaustive : bool;
}

and coro_catch = {
	cc_cb : coro_block;
	cc_catches : (tvar * coro_block) list;
}

and coro_suspend = {
	cs_fun : texpr;
	cs_args : texpr list;
	cs_pos : pos;
}

and coro_next = {
	next_kind : coro_next_kind;
	next_type : Type.t;
	next_pos : pos;
}

type coro_ctx = {
	com : Common.context;
	coro_debug : bool;
	mutable vthis : tvar option;
	mutable next_block_id : int;
	mutable cb_unreachable : coro_block;
	mutable current_catch : coro_block option;
}
