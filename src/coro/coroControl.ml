open Globals
open Type
open Texpr

type coro_control =
	| CoroPending
	| CoroReturned
	| CoroThrown

let mk_int basic i = Texpr.Builder.make_int basic i null_pos

let mk_control basic (c : coro_control) = mk_int basic (Obj.magic c)

let make_control_switch basic e_subject e_pending e_returned e_thrown p =
	let cases = [{
		case_patterns = [mk_control basic CoroPending];
		case_expr = e_pending;
	}; {
		case_patterns = [mk_control basic CoroReturned];
		case_expr = e_returned;
	}; {
		case_patterns = [mk_control basic CoroThrown];
		case_expr = e_thrown;
	}] in
	let switch = {
		switch_subject = e_subject;
		switch_cases = cases;
		switch_default = None;
		switch_exhaustive = true;
	} in
	mk (TSwitch switch) basic.tvoid p