(* A lock that the domain holding it can take again. *)
type t = {
	m : Mutex.t;
	c : Condition.t;
	mutable owner : Domain.id option;
	mutable count : int;
	(* the time domains spent waiting to take the lock, and how many times *)
	mutable waited : float;
	mutable waits : int;
	(* what the current owner took the lock for, and since when; the time the
	   lock was held, by what it was taken for *)
	mutable site : string;
	mutable since : float;
	held : (string, float ref) Hashtbl.t;
}

let create () = { m = Mutex.create (); c = Condition.create (); owner = None; count = 0; waited = 0.; waits = 0; site = ""; since = 0.; held = Hashtbl.create 0 }

(* with [l.m] held *)
let take l me site =
	l.owner <- Some me;
	l.site <- site;
	l.since <- Unix.gettimeofday ()

(* with [l.m] held, the current domain giving the lock up *)
let account l =
	let d = Unix.gettimeofday () -. l.since in
	match Hashtbl.find_opt l.held l.site with
	| Some r -> r := !r +. d
	| None -> Hashtbl.add l.held l.site (ref d)

let lock ?(site="other") l =
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
		take l me site;
		l.count <- 1
	end;
	Mutex.unlock l.m

let unlock l =
	Mutex.lock l.m;
	l.count <- l.count - 1;
	if l.count = 0 then begin
		account l;
		l.owner <- None;
		Condition.broadcast l.c
	end;
	Mutex.unlock l.m

let protect ?site l f =
	lock ?site l;
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
	let site = l.site in
	if saved > 0 then begin
		account l;
		l.owner <- None;
		l.count <- 0;
		Condition.broadcast l.c
	end;
	let rec loop () =
		while l.owner <> None do Condition.wait l.c l.m done;
		if cond () then begin
			if saved > 0 then begin
				take l me site;
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
	let site = l.site in
	if n > 0 then begin
		account l;
		l.owner <- None;
		l.count <- 0;
		Condition.broadcast l.c
	end;
	Mutex.unlock l.m;
	(n,site)

let reacquire l (n,site) =
	if n > 0 then begin
		let me = Domain.self () in
		Mutex.lock l.m;
		if l.owner <> None then begin
			let t = Unix.gettimeofday () in
			while l.owner <> None do Condition.wait l.c l.m done;
			l.waited <- l.waited +. (Unix.gettimeofday () -. t);
			l.waits <- l.waits + 1
		end;
		take l me site;
		l.count <- n;
		Mutex.unlock l.m
	end

(* The time the lock was held, by what it was taken for. *)
let held l =
	Mutex.protect l.m (fun () -> Hashtbl.fold (fun site r acc -> (site,!r) :: acc) l.held [])
