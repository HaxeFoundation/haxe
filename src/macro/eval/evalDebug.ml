open Gc
open Globals
open Ast
open Type
open EvalJitContext
open EvalContext
open EvalValue
open EvalExceptions
open EvalPrinting
open EvalHash
open EvalEncode
open EvalMisc
open EvalDebugMisc
open MacroApi

let is_caught ctx v =
	try
		Hashtbl.iter (fun path _ -> if is v path then raise Exit) ctx.debug.caught_types;
		false
	with Exit ->
		true

(* Checks debug state and calls what's needed. *)
let rec run_loop ctx wait run env : value =
	let check_breakpoint () =
		if ctx.debug.breakpoint.bpstate = BPHit && env.env_debug.line <> ctx.debug.breakpoint.bpline then ctx.debug.breakpoint.bpstate <- BPEnabled
	in
	match ctx.debug.debug_state with
		| DbgRunning ->
			check_breakpoint();
			run env
		| DbgContinue ->
			check_breakpoint();
			run env
		| DbgNext(env',p) ->
			let b = DisplayPosition.encloses_position (env.env_debug.expr.epos) p in
			let rec is_on_stack env =
				match env.env_parent with
				| Some env -> env == env' || is_on_stack env
				| None -> false
			in
			if is_on_stack env || b then
				run env
			else begin
				ctx.debug.debug_state <- DbgWaiting;
				run_loop ctx wait run env
			end;
		| DbgFinish env' ->
			if env' != env then
				run env
			else begin
				ctx.debug.debug_state <- DbgWaiting;
				run_loop ctx wait run env
			end
		| DbgWaiting | DbgStart ->
			wait ctx run env

let debug_loop jit conn e f =
	let ctx = jit.ctx in
	let scopes = jit.scopes in
	let line,col1,_,_ = Lexer.get_pos_coords e.epos in
	let column_matches breakpoint = match breakpoint.bpcolumn with
		| BPAny -> true
		| BPColumn i -> i = col1
	in
	let condition_holds env breakpoint = match breakpoint.bpcondition with
		| None -> true
		| Some e -> match expr_to_value_safe ctx env e with
			| VTrue -> true
			| _ -> false
	in
	let debugger_catches v = match ctx.debug.exception_mode with
		| CatchAll -> true
		| CatchUncaught -> not (is_caught ctx v)
		| CatchNone -> false
	in
	(* Checks if we hit a breakpoint, runs the code if not. *)
	let rec run_check_breakpoint env =
		try
			let h = Hashtbl.find ctx.debug.breakpoints env.env_info.pfile in
			let breakpoint = Hashtbl.find h env.env_debug.line in
			begin match breakpoint.bpstate with
				| BPEnabled when column_matches breakpoint && condition_holds env breakpoint ->
					breakpoint.bpstate <- BPHit;
					ctx.debug.breakpoint <- breakpoint;
					conn.bp_stop ctx env;
					ctx.debug.debug_state <- DbgWaiting;
					run_loop ctx conn.wait run_check_breakpoint env
				| _ ->
					raise Not_found
			end
		with Not_found -> try
			f env
		with
		| RunTimeException(v,_,_) when debugger_catches v && ctx.debug.caught_exception != v ->
			ctx.debug.caught_exception <- v;
			conn.exc_stop ctx v e.epos;
			ctx.debug.debug_state <- DbgWaiting;
			run_loop ctx conn.wait run_check_breakpoint env
		| BreakHere ->
			conn.bp_stop ctx env;
			ctx.debug.debug_state <- DbgWaiting;
			run_loop ctx conn.wait run_check_breakpoint env
		(* | Return _ | Break | Continue | Sys_exit _ | RunTimeException _ as exc ->
			raise exc
		| exc ->
			throw (EvalString.vstring (EvalString.create_ascii (Printexc.to_string exc))) e.epos; *)
	in
	(* Sets the environmental debug data, then executes the debug loop. *)
	let run_set env =
		env.env_debug.scopes <- scopes;
		env.env_debug.line <- line;
		env.env_debug.expr <- e;
		run_loop ctx conn.wait run_check_breakpoint env;
	in
	run_set