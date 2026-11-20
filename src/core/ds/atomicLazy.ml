type 'a atomic_state =
	| Unset of (unit -> 'a)
	| Computing
	| Set of 'a

type 'a t = {
	state : 'a atomic_state Atomic.t;
}

let from_fun f =
	{ state = Atomic.make (Unset f) }

let from_val v =
	{ state = Atomic.make (Set v) }

let is_val lazy_val = match Atomic.get lazy_val.state with
	| Set _ -> true
	| _ -> false

let rec force lazy_val =
	let wait_loop () =
		let rec loop backoff = match Atomic.get lazy_val.state with
			| Set v ->
				v
			| Computing ->
				Thread.yield();
				loop (Backoff.once backoff)
			| Unset _ ->
				(* the computation raised an exception *)
				force lazy_val
		in
		loop (Backoff.create ())
	in
	match Atomic.get lazy_val.state with
	 | Set v ->
		v
	| Computing ->
		wait_loop ()
	| Unset f as r ->
		if Atomic.compare_and_set lazy_val.state r Computing then begin
			let v = try
				f()
			with exc ->
				let bt = Printexc.get_raw_backtrace () in
				Atomic.set lazy_val.state r;
				Printexc.raise_with_backtrace exc bt
			in
			Atomic.set lazy_val.state (Set v);
			v
		end else
			wait_loop ()

