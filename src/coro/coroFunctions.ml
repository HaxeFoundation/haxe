open Globals
open Type
open CoroTypes

let make_block ctx typepos =
	let id = ctx.next_block_id in
	ctx.next_block_id <- ctx.next_block_id + 1;
	{
		cb_id = id;
		cb_el = DynArray.create ();
		cb_typepos = typepos;
		cb_next = NextUnknown;
		cb_catch = ctx.current_catch;
		cb_flags = 0;
	}

let add_block_flag cb (flag : cb_flag) =
	cb.cb_flags <- set_flag cb.cb_flags (Obj.magic flag)

let has_block_flag cb (flag : cb_flag) =
	has_flag cb.cb_flags (Obj.magic flag)

let coro_iter f cb =
	Option.may f cb.cb_catch;
	match cb.cb_next with
	| NextSub(cb_sub,cb_next) ->
		f cb_sub;
		f cb_next
	| NextIfThen(_,cb_then,cb_next) ->
		f cb_then;
		f cb_next;
	| NextIfThenElse(_,cb_then,cb_else,cb_next) ->
		f cb_then;
		f cb_else;
		f cb_next;
	| NextSwitch(switch,cb_next) ->
		List.iter (fun (_,cb) -> f cb) switch.cs_cases;
		Option.may f switch.cs_default;
		f cb_next;
	| NextWhile(e,cb_body,cb_next) ->
		f cb_body;
		f cb_next;
	| NextTry(cb_try,catch,cb_next) ->
		f cb_try;
		f catch.cc_cb;
		List.iter (fun (_,cb) -> f cb) catch.cc_catches;
		f cb_next;
	| NextSuspend(call,cb_next) ->
		f cb_next
	| NextBreak cb_next | NextContinue cb_next | NextFallThrough cb_next | NextGoto cb_next ->
		f cb_next;
	| NextUnknown | NextReturnVoid | NextReturn _ | NextThrow _ ->
		()

let coro_next_map f cb =
	Option.may (fun cb_catch -> cb.cb_catch <- Some (f cb_catch)) cb.cb_catch;
	match cb.cb_next with
	| NextSub(cb_sub,cb_next) ->
		let cb_sub = f cb_sub in
		let cb_next = f cb_next in
		cb.cb_next <- NextSub(cb_sub,cb_next);
	| NextIfThen(e,cb_then,cb_next) ->
		let cb_then = f cb_then in
		let cb_next = f cb_next in
		cb.cb_next <- NextIfThen(e,cb_then,cb_next);
	| NextIfThenElse(e,cb_then,cb_else,cb_next) ->
		let cb_then = f cb_then in
		let cb_else = f cb_else in
		let cb_next = f cb_next in
		cb.cb_next <- NextIfThenElse(e,cb_then,cb_else,cb_next);
	| NextSwitch(switch,cb_next) ->
		let cases = List.map (fun (el,cb) -> (el,f cb)) switch.cs_cases in
		let def = Option.map f switch.cs_default in
		let switch = {
			switch with cs_cases = cases; cs_default = def
		} in
		let cb_next = f cb_next in
		cb.cb_next <- NextSwitch(switch,cb_next);
	| NextWhile(e,cb_body,cb_next) ->
		let cb_body = f cb_body in
		let cb_next = f cb_next in
		cb.cb_next <- NextWhile(e,cb_body,cb_next);
	| NextTry(cb_try,catch,cb_next) ->
		let cb_try = f cb_try in
		let cc_cb = f catch.cc_cb in
		let catches = List.map (fun (v,cb) -> (v,f cb)) catch.cc_catches in
		let catch = {
			cc_cb;
			cc_catches = catches
		} in
		let cb_next = f cb_next in
		cb.cb_next <- NextTry(cb_try,catch,cb_next);
	| NextSuspend(call,cb_next) ->
		let cb_next = f cb_next in
		cb.cb_next <- NextSuspend(call,cb_next);
	| NextBreak cb_next ->
		cb.cb_next <- NextBreak (f cb_next);
	| NextContinue cb_next ->
		cb.cb_next <- NextContinue (f cb_next);
	| NextGoto cb_next ->
		cb.cb_next <- NextContinue (f cb_next);
	| NextFallThrough cb_next ->
		cb.cb_next <- NextFallThrough (f cb_next);
	| NextReturnVoid | NextReturn _ | NextThrow _ | NextUnknown ->
		()