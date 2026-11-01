module Mpsc = Saturn.Single_consumer_queue

type t = {
	now : Mutex.t;
	later: (unit -> unit) Mpsc.t;
}

let create () = {
	now = Mutex.create ();
	later = Mpsc.create ();
}

let try_now nol f =
	if Mutex.try_lock nol.now then begin
		f();
		Mutex.unlock nol.now
	end else begin
		Mpsc.push nol.later f
	end

let handle_later nol =
	let rec loop () = match Mpsc.pop_opt nol.later with
		| Some f ->
			f ();
			loop ()
		| None ->
			()
	in
	loop ()