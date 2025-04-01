type t = {
	now : Mutex.t;
	later: (unit -> unit) Mpsc_queue.t;
}

let create () = {
	now = Mutex.create ();
	later = Mpsc_queue.create ();
}

let try_now nol f =
	if Mutex.try_lock nol.now then begin
		f();
		Mutex.unlock nol.now
	end else
		Mpsc_queue.push nol.later f

let handle_later nol =
	let rec loop () = match Mpsc_queue.pop nol.later with
		| Some f ->
			f ();
			loop ()
		| None ->
			()
	in
	loop ()