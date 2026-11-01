open Globals
open EvalContext
open EvalDebugMisc
open EvalExceptions
open EvalValue

module Deque = struct
	let create () = {
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

let create_eval tthread =
	let eval = {
		env = None;
		thread = tthread;
		exception_stack = [];
		debug_channel = Event.new_channel ();
		debug_state = DbgRunning;
		breakpoint = make_breakpoint 0 0 BPDisabled BPAny None;
		caught_types = IntHashtbl.create 0;
		last_return = None;
		caught_exception = vnull;
		eval_storage = TlsStorage.create 0;
	} in
	eval

let run ctx tthread f =
	let new_eval = create_eval tthread in
	let id = tthread.thread_id in
	let maybe_send_thread_event reason = match ctx.debug.debug_socket with
		| Some socket ->
			socket.connection.send_thread_event id reason
		| None ->
			()
	in
	ThreadSafeHashtbl.add ctx.evals id new_eval;
	Thread_local_storage.set ctx.eval new_eval;
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

let create_thread_info next_thread_id mode =
	let id = Atomic.fetch_and_add next_thread_id 1 + 1 in
	let tthread = {
		thread_id = id;
		thread_mode = mode;
		thread_deque = Deque.create ();
	} in
	tthread

let spawn_domain ctx f =
	let tthread = create_thread_info ctx.next_thread_id (Obj.magic ()) in
	let start_sem = Semaphore.Binary.make false in
	let thread = Domain (Domain.spawn (fun () ->
		Semaphore.Binary.acquire start_sem;
		run ctx tthread f
	)) in
	tthread.thread_mode <- thread;
	Semaphore.Binary.release start_sem;
	tthread

let spawn_thread ctx f =
	let tthread = create_thread_info ctx.next_thread_id (Obj.magic ()) in
	let start_sem = Semaphore.Binary.make false in
	let thread = Thread (Thread.create (fun () ->
		Semaphore.Binary.acquire start_sem;
		run ctx tthread f
	) ()) in
	tthread.thread_mode <- thread;
	Semaphore.Binary.release start_sem;
	tthread