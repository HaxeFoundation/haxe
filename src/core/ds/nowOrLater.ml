type t = {
	now : Mutex.t;
	later: (unit -> unit) Lockfree.Single_consumer_queue.t;
}

let create () = {
	now = Mutex.create ();
	later = Lockfree.Single_consumer_queue.create ();
}

let try_now nol f =
	if Mutex.try_lock nol.now then begin
		f();
		Mutex.unlock nol.now
	end else
		Lockfree.Single_consumer_queue.push nol.later f

let handle_later nol =
	let rec loop () = match Lockfree.Single_consumer_queue.pop nol.later with
		| Some f ->
			f ();
			loop ()
		| None ->
			()
	in
	loop ()