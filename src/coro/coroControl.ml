open Globals
open Type
open Texpr

type coro_control =
	| CoroPending
	| CoroReturned
	| CoroThrown

let mk_int basic i = Texpr.Builder.make_int basic i null_pos

let mk_control basic (c : coro_control) = mk_int basic (Obj.magic c)

let make_custom_control_switch basic e_subject cases p =
	let cases = List.map (fun (l,e) -> {
		case_patterns = List.map (mk_control basic) l;
		case_expr = e;
	}) cases in
	let switch = {
		switch_subject = e_subject;
		switch_cases = cases;
		switch_default = None;
		switch_exhaustive = true;
	} in
	mk (TSwitch switch) basic.tvoid p

let make_control_switch basic e_subject e_pending e_returned e_thrown p =
	let cases = [
		[CoroPending],e_pending;
		[CoroReturned],e_returned;
		[CoroThrown],e_thrown;
	] in
	make_custom_control_switch basic e_subject cases p