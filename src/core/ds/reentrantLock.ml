(* A lock that the domain holding it can take again. *)
type t = {
	m : Mutex.t;
	c : Condition.t;
	mutable owner : Domain.id option;
	mutable count : int;
	(* the time domains spent waiting to take the lock, and how many times *)
	mutable waited : float;
	mutable waits : int;
}

let create () = { m = Mutex.create (); c = Condition.create (); owner = None; count = 0; waited = 0.; waits = 0 }

let lock l =
	let me = Domain.self () in
	Mutex.lock l.m;
	if l.owner = Some me then
		l.count <- l.count + 1
	else begin
		if l.owner <> None then begin
			let t = Unix.gettimeofday () in
			while l.owner <> None do Condition.wait l.c l.m done;
			l.waited <- l.waited +. (Unix.gettimeofday () -. t);
			l.waits <- l.waits + 1
		end;
		l.owner <- Some me;
		l.count <- 1
	end;
	Mutex.unlock l.m

let unlock l =
	Mutex.lock l.m;
	l.count <- l.count - 1;
	if l.count = 0 then begin
		l.owner <- None;
		Condition.broadcast l.c
	end;
	Mutex.unlock l.m

let protect l f =
	lock l;
	match f () with
	| v -> unlock l; v
	| exception e -> unlock l; raise e

(* Wakes up the domains waiting in [wait_until]. *)
let notify l =
	Mutex.lock l.m;
	Condition.broadcast l.c;
	Mutex.unlock l.m

(* Waits until [cond ()] holds, releasing the lock meanwhile if the current
   domain holds it, and taking it back as it was. [cond] is checked while no
   domain holds the lock, and must not take it. *)
let wait_until l cond =
	let me = Domain.self () in
	Mutex.lock l.m;
	let saved = if l.owner = Some me then l.count else 0 in
	if saved > 0 then begin
		l.owner <- None;
		l.count <- 0;
		Condition.broadcast l.c
	end;
	let rec loop () =
		while l.owner <> None do Condition.wait l.c l.m done;
		if cond () then begin
			if saved > 0 then begin
				l.owner <- Some me;
				l.count <- saved
			end
		end else begin
			Condition.wait l.c l.m;
			loop ()
		end
	in
	loop ();
	Mutex.unlock l.m

(* Releases the lock if the current domain holds it, giving what [reacquire]
   needs to take it back as it was. *)
let release_all l =
	let me = Domain.self () in
	Mutex.lock l.m;
	let n = if l.owner = Some me then l.count else 0 in
	if n > 0 then begin
		l.owner <- None;
		l.count <- 0;
		Condition.broadcast l.c
	end;
	Mutex.unlock l.m;
	n

let reacquire l n =
	if n > 0 then begin
		let me = Domain.self () in
		Mutex.lock l.m;
		while l.owner <> None do Condition.wait l.c l.m done;
		l.owner <- Some me;
		l.count <- n;
		Mutex.unlock l.m
	end
