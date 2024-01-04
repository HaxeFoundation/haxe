open Type
open EvalJitContext
open EvalContext
open EvalValue
open EvalExceptions
open EvalDebugMisc

let is_caught eval v =
	try
		Hashtbl.iter (fun path _ -> if is v path then raise Exit) eval.caught_types;
		false
	with Exit ->
		true

(* Checks debug state and calls what's needed. *)
let rec run_loop run env : value =
	let eval = env.env_eval in
	let check_breakpoint () =
		if eval.breakpoint.bpstate = BPHit && env.env_debug.line <> eval.breakpoint.bpline then eval.breakpoint.bpstate <- BPEnabled
	in
	match eval.debug_state with
		| DbgRunning ->
			check_breakpoint();
			run env
		| DbgNext(env',p) ->
			let b = DisplayPosition.encloses_position (env.env_debug.debug_pos) p in
			let rec is_on_stack env =
				match env.env_parent with
				| Some env -> env == env' || is_on_stack env
				| None -> false
			in
			if is_on_stack env || b then
				run env
			else begin
				eval.debug_state <- DbgWaiting;
				run_loop run env
			end;
		| DbgFinish env' ->
			if env' != env then
				run env
			else begin
				eval.debug_state <- DbgWaiting;
				run_loop run env
			end
		| DbgStep ->
			eval.debug_state <- DbgWaiting;
			run env
		| DbgWaiting ->
			ignore(Event.sync(Event.receive eval.debug_channel));
			run_loop run env

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
		| Some e -> match safe_call env.env_eval (expr_to_value_safe ctx env) e with
			| VTrue -> true
			| _ -> false
	in
	let debugger_catches eval v = match ctx.debug.exception_mode with
		| CatchAll -> true
		| CatchUncaught -> not (is_caught eval v)
		| CatchNone -> false
	in
	(* Checks if we hit a breakpoint, runs the code if not. *)
	let rec run_check_breakpoint env =
		let eval = env.env_eval in
		try
			let h = Hashtbl.find ctx.debug.breakpoints env.env_info.pfile_unique in
			let breakpoint = Hashtbl.find h env.env_debug.line in
			begin match breakpoint.bpstate with
				| BPEnabled when column_matches breakpoint && condition_holds env breakpoint ->
					breakpoint.bpstate <- BPHit;
					eval.breakpoint <- breakpoint; (* TODO: per-thread... *)
					conn.bp_stop ctx.debug;
					eval.debug_state <- DbgWaiting;
					run_loop run_check_breakpoint env
				| _ ->
					raise Not_found
			end
		with Not_found -> try
			f env
		with
		| RunTimeException(v,_,_) when debugger_catches env.env_eval v && eval.caught_exception != v ->
			eval.caught_exception <- v;
			conn.exc_stop ctx.debug v e.epos;
			eval.debug_state <- DbgWaiting;
			run_loop run_check_breakpoint env
		| BreakHere ->
			conn.bp_stop ctx.debug;
			eval.debug_state <- DbgWaiting;
			run_loop (fun _ -> vnull) env
		| Return v as exc ->
			eval.last_return <- Some v;
			raise exc
		(* | Return _ | Break | Continue | Sys_exit _ | RunTimeException _ as exc ->
			raise exc
		| exc ->
			throw (EvalString.vstring (EvalString.create_ascii (Printexc.to_string exc))) e.epos; *)
	in
	(* Sets the environmental debug data, then executes the debug loop. *)
	let run_set env =
		env.env_debug.scopes <- scopes;
		env.env_debug.line <- line;
		env.env_debug.debug_pos <- e.epos;
		env.env_debug.debug_expr <- s_expr_pretty e;
		run_loop run_check_breakpoint env;
	in
	run_set
