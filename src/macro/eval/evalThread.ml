open Globals
open EvalContext
open EvalDebugMisc
open EvalExceptions
open EvalValue

module Deque = struct
	let create id = {
		dvalues = [];
		dmutex = DomainMutex.create();
	}

	let add this domain_id i =
		DomainMutex.lock this.dmutex domain_id;
		this.dvalues <- this.dvalues @ [i];
		DomainMutex.unlock this.dmutex

	let pop this domain_id blocking =
		if not blocking then begin
			DomainMutex.lock this.dmutex domain_id;
			match this.dvalues with
			| v :: vl ->
				this.dvalues <- vl;
				DomainMutex.unlock this.dmutex;
				Some v
			| [] ->
				DomainMutex.unlock this.dmutex;
				None
		end else begin
			(* Optimistic first attempt with immediate lock. *)
			DomainMutex.lock this.dmutex domain_id;
			begin match this.dvalues with
			| v :: vl ->
				this.dvalues <- vl;
				DomainMutex.unlock this.dmutex;
				Some v
			| [] ->
				DomainMutex.unlock this.dmutex;
				(* First attempt failed, let's be pessimistic now to avoid locks. *)
				let rec loop () =
					Domain.cpu_relax ();
					match this.dvalues with
					| v :: vl ->
						(* Only lock if there's a chance to have a value. This avoids high amounts of unneeded locking. *)
						DomainMutex.lock this.dmutex domain_id;
						(* We have to check again because the value could be gone by now. *)
						begin match this.dvalues with
						| v :: vl ->
							this.dvalues <- vl;
							DomainMutex.unlock this.dmutex;
							Some v
						| [] ->
							DomainMutex.unlock this.dmutex;
							loop()
						end
					| [] ->
						loop()
				in
				loop()
			end
		end

	let push this domain_id i =
		DomainMutex.lock this.dmutex domain_id;
		this.dvalues <- i :: this.dvalues;
		DomainMutex.unlock this.dmutex
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