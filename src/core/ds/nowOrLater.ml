type t = {
	now : Mutex.t;
	later: (unit -> unit) Queue.t;
	later_mutex : Mutex.t;
}

let create () = {
	now = Mutex.create ();
	later = Queue.create ();
	later_mutex = Mutex.create ();
}

let try_now nol f =
	if Mutex.try_lock nol.now then begin
		f();
		Mutex.unlock nol.now
	end else begin
		Mutex.protect nol.later_mutex (fun () -> Queue.push f nol.later)
	end

let handle_later nol =
	let rec loop () = match Queue.take_opt nol.later with
		| Some f ->
			f ();
			loop ()
		| None ->
			()
	in
	loop ()