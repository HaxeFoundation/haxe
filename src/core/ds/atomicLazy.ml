type 'a atomic_state =
	| Unset of (unit -> 'a)
	| Computing
	| Set of 'a

type 'a t = {
	state : 'a atomic_state Atomic.t;
}

let from_fun f =
	{ state = Atomic.make (Unset f) }

let force lazy_val =
	let wait_loop () =
		let rec loop backoff = match Atomic.get lazy_val.state with
			| Set v ->
				v
			| Computing ->
				loop (Backoff.once backoff)
			| Unset _ ->
				assert false
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
			let v = f() in
			Atomic.set lazy_val.state (Set v);
			v
		end else
			wait_loop ()

