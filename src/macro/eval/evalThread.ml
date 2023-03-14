open Globals
open EvalContext
open EvalDebugMisc
open EvalExceptions
open EvalValue

module Deque = struct
	let create () = {
		dvalues = [];
		dmutex = Mutex.create();
	}

	let add this i =
		Mutex.lock this.dmutex;
		this.dvalues <- this.dvalues @ [i];
		Mutex.unlock this.dmutex

	let pop this blocking =
		if not blocking then begin
			Mutex.lock this.dmutex;
			match this.dvalues with
			| v :: vl ->
				this.dvalues <- vl;
				Mutex.unlock this.dmutex;
				Some v
			| [] ->
				Mutex.unlock this.dmutex;
				None
		end else begin
			(* Optimistic first attempt with immediate lock. *)
			Mutex.lock this.dmutex;
			begin match this.dvalues with
			| v :: vl ->
				this.dvalues <- vl;
				Mutex.unlock this.dmutex;
				Some v
			| [] ->
				Mutex.unlock this.dmutex;
				(* First attempt failed, let's be pessimistic now to avoid locks. *)
				let rec loop () =
					Thread.yield();
					match this.dvalues with
					| v :: vl ->
						(* Only lock if there's a chance to have a value. This avoids high amounts of unneeded locking. *)
						Mutex.lock this.dmutex;
						(* We have to check again because the value could be gone by now. *)
						begin match this.dvalues with
						| v :: vl ->
							this.dvalues <- vl;
							Mutex.unlock this.dmutex;
							Some v
						| [] ->
							Mutex.unlock this.dmutex;
							loop()
						end
					| [] ->
						loop()
				in
				loop()
			end
		end

	let push this i =
		Mutex.lock this.dmutex;
		this.dvalues <- i :: this.dvalues;
		Mutex.unlock this.dmutex
end

let create_eval thread = {
	env = None;
	thread = thread;
	debug_channel = Event.new_channel ();
	debug_state = DbgRunning;
	breakpoint = make_breakpoint 0 0 BPDisabled BPAny None;
	caught_types = Hashtbl.create 0;
	last_return = None;
	caught_exception = vnull;
}

let run ctx f thread =
	let id = Thread.id (Thread.self()) in
	let maybe_send_thread_event reason = match ctx.debug.debug_socket with
		| Some socket ->
			socket.connection.send_thread_event id reason
		| None ->
			()
	in
	let new_eval = create_eval thread in
	ctx.evals <- IntMap.add id new_eval ctx.evals;
	let close () =
		ctx.evals <- IntMap.remove id ctx.evals;
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
	| Sys_exit i ->
		close();
		exit i;
	| exc ->
		close();
		raise exc

let spawn ctx f =
	let thread = {
		tthread = Obj.magic ();
		tstorage = IntMap.empty;
		tevents = vnull;
		tdeque = Deque.create();
	} in
	thread.tthread <- Thread.create (run ctx f) thread;
	thread

(**
	Just executes `f` if called from a Haxe thread.
	Otherwise creates Haxe thread data structures, runs `f` and then cleans up
	created data.
*)
let run ctx f =
	let id = Thread.id (Thread.self()) in
	if IntMap.mem id ctx.evals then
		ignore(f())
	else begin
		let thread = {
			tthread = Thread.self();
			tstorage = IntMap.empty;
			tevents = vnull;
			tdeque = Deque.create();
		} in
		run ctx f thread
	end