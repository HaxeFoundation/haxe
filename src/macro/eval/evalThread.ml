open Globals
open EvalContext
open EvalDebugMisc
open EvalExceptions
open EvalValue

module Deque = struct
	let create _ = {
		dvalues = [];
		dmutex = Mutex.create();
		dcond = Condition.create();
	}

	let add this i =
		Mutex.lock this.dmutex;
		this.dvalues <- this.dvalues @ [i];
		Condition.signal this.dcond;
		Mutex.unlock this.dmutex

	let pop this blocking =
		Mutex.lock this.dmutex;
		if blocking then begin
			while this.dvalues = [] do
				Condition.wait this.dcond this.dmutex
			done
		end;
		let result = match this.dvalues with
			| v :: vl ->
				this.dvalues <- vl;
				Some v
			| [] ->
				None
		in
		Mutex.unlock this.dmutex;
		result

	let push this i =
		Mutex.lock this.dmutex;
		this.dvalues <- i :: this.dvalues;
		Condition.signal this.dcond;
		Mutex.unlock this.dmutex
end

let create_eval thread = {
	env = None;
	thread = thread;
	exception_stack = [];
	debug_channel = Event.new_channel ();
	debug_state = DbgRunning;
	breakpoint = make_breakpoint 0 0 BPDisabled BPAny None;
	caught_types = IntHashtbl.create 0;
	last_return = None;
	caught_exception = vnull;
}

let run ctx f thread =
	let id = thread.tid in
	let maybe_send_thread_event reason = match ctx.debug.debug_socket with
		| Some socket ->
			socket.connection.send_thread_event id reason
		| None ->
			()
	in
	let new_eval = create_eval thread in
	ThreadSafeHashtbl.add ctx.evals id new_eval;
	Domain.DLS.set ctx.eval new_eval;
	let close () =
		ThreadSafeHashtbl.remove ctx.evals id;
		maybe_send_thread_event "exited";
	in
	try
		maybe_send_thread_event "started";
		ignore(f ());
		close();
	with
	| RunTimeException(v,stack,p) ->
		let msg = get_exc_error_message ctx v stack p in
		prerr_endline msg;
		close();
	| EvalTypes.Sys_exit i ->
		close();
		exit i;
	| exc ->
		close();
		raise exc

let spawn ctx f =
	let id = Atomic.fetch_and_add ctx.next_thread_id 1 + 1 in
	let thread = {
		tid = id;
		tthread = Obj.magic ();
		tstorage = IntMap.empty;
		tevents = vnull;
		tdeque = Deque.create id;
	} in
	thread.tthread <- Domain.spawn (fun () -> run ctx f thread);
	thread

(**
	Just executes `f` if called from a Haxe thread.
	Otherwise creates Haxe thread data structures, runs `f` and then cleans up
	created data.
*)
(* let run ctx f =
	let id = Thread.id (Thread.self()) in
	if ThreadSafeHashtbl.mem ctx.evals id then
		ignore(f())
	else begin
		let thread = {
			tthread = Domain.self();
			tstorage = IntMap.empty;
			tevents = vnull;
			tdeque = Deque.create();
		} in
		run ctx f thread
	end *)