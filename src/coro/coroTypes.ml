open Globals
open Type

type suspend_expr =
	| SusBlock
	| SusResult

type coro_block = {
	mutable cb_id : int;
	cb_el : texpr DynArray.t;
	cb_typepos : (Type.t * pos) option;
	mutable cb_catch : coro_block option;
	mutable cb_next : coro_next;
	mutable cb_flags : int;
	mutable cb_stack_value : texpr option;
}

and coro_block_next = coro_block option

and coro_next =
	| NextUnknown
	| NextReturnVoid
	| NextReturn of texpr
	| NextThrow of texpr
	| NextIfThen of texpr * coro_block * coro_block
	| NextIfThenElse of texpr * coro_block * coro_block * coro_block_next
	| NextSwitch of coro_switch * coro_block_next
	| NextWhile of texpr * coro_block * coro_block_next
	| NextTry of coro_block * coro_catch * coro_block_next
	| NextSuspend of coro_suspend * coro_block_next
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
	cs_result : suspend_expr;
}

type coro_ctx = {
	builder : CoroElsewhere.texpr_builder;
	typer : Typecore.typer;
	coro_debug : bool;
	nothrow : bool;
	mutable vthis : tvar option;
	mutable next_block_id : int;
	mutable current_catch : coro_block option;
	mutable has_catch : bool;
}

type cb_flag =
	| CbGenerated
	| CbSuspendState
	| CbResumeState

type coro_scope = {
	scope_var : tvar;
	restricted_suspension : bool;
}