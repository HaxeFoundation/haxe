(*
	Claims of the domains typing in parallel (docs/incremental-compilation.md,
	phase 3.5): a lazy type, a field body or the classes of a module are computed
	by the domain that claimed them, the others wait for it. A domain does not
	wait when that would close a cycle of waiting domains: as on one domain, it
	then sees what is being computed.
*)
let mutex = Mutex.create ()
let done_ = Condition.create ()

(* What a domain waits for, in the order in which waits are let through to
   break a cycle of waiting domains: going on without the claim of the macros
   is what a nested macro call does on one domain; going on without a value
   (a lazy type, a field body, a constructor) sees its placeholder, as a cycle
   on one domain does; a class being built is never read partially, unless the
   cycle has nothing else. *)
type wait_kind =
	| WaitMacros
	| WaitValue
	| WaitClass

let rank = function
	| WaitMacros -> 0
	| WaitValue -> 1
	| WaitClass -> 2

let kind_names = [|"macros";"value";"class"|]

(* the domains waiting, with the domain they wait for, what they wait for and,
   for a class, when its build started, the condition they wait for, and
   whether letting them through is what one domain does (see [wait]) *)
let waiting : (Domain.id * (Domain.id * wait_kind * int * (unit -> bool) * (unit -> bool))) list ref = ref []

(* the time domains spent waiting, and how many times, by kind *)
let waited = Array.make 3 0.
let waits = Array.make 3 0
(* the cycles broken, by kind of the wait let through *)
let breaks = Array.make 3 0

(* the waiting domains let through to break a cycle *)
let let_through : Domain.id list ref = ref []

(* Waits, with [mutex] held, until [cond ()] holds, or until let through to
   break a cycle of waiting domains (see [wait_kind]). Tells whether [cond ()]
   holds. [safe ()]: whether going on without [cond ()] reads what a single
   domain reads in the same cycle (the final type of a field whose body is
   being typed): such a wait is let through first among those of its kind. *)
let wait ?(kind=WaitValue) ?(seq=0) ?(safe=(fun () -> false)) ~owner cond =
	let me = Domain.self () in
	let rec chain d acc =
		if d = me then Some acc
		(* a cycle without the current domain: its domains break it *)
		else if List.mem_assoc d acc then None
		else match List.assoc_opt d !waiting with
			(* a domain whose condition holds, or let through, is about to go on *)
			| Some (_,_,_,cond',_) when cond' () || List.mem d !let_through -> None
			| Some (d',k,n,_,s) -> chain d' ((d,(k,n,s ())) :: acc)
			| None -> None
	in
	let through () = List.mem me !let_through in
	let block () =
		waiting := (me,(owner,kind,seq,cond,safe)) :: !waiting;
		if not (cond () || through ()) then begin
			let t = Unix.gettimeofday () in
			while not (cond () || through ()) do Condition.wait done_ mutex done;
			let k = rank kind in
			waited.(k) <- waited.(k) +. (Unix.gettimeofday () -. t);
			waits.(k) <- waits.(k) + 1
		end;
		waiting := List.remove_assoc me !waiting;
		let_through := List.filter (fun d -> d <> me) !let_through
	in
	match chain owner [] with
	| None ->
		block ();
		cond ()
	| Some cycle ->
		(* the domain of the cycle to let through: the current one when it is as good;
		   a safe wait first; among classes, the one whose build started first, as
		   the outermost build of a cycle on one domain is the one read partially *)
		let better (k,n,s) (k',n',s') =
			rank k' < rank k || (rank k' = rank k && ((s' && not s) || (s' = s && n' < n)))
		in
		let best = List.fold_left (fun (d,kn) (d',kn') -> if better kn kn' then (d',kn') else (d,kn)) (me,(kind,seq,safe ())) cycle in
		let k = (match snd best with (k,_,_) -> rank k) in
		breaks.(k) <- breaks.(k) + 1;
		if fst best = me then
			false
		else begin
			let_through := fst best :: !let_through;
			Condition.broadcast done_;
			block ();
			cond ()
		end

(* Waits outside of the typing lock, which the owner may need. *)
let wait_unlocked ~owner cond =
	TypingLock.without (fun () -> Mutex.protect mutex (fun () -> ignore(wait ~kind:WaitValue ~owner cond)))

(* The macros: one domain runs macros at a time, even while it waits for
   another domain (it releases the typing lock then, not this claim). A domain
   let through to break a cycle takes the claim over until it is done with it,
   so that the macros still run on one domain at a time: the domains holding
   the claim, the current one first, how many times they claimed it and when
   they first did. *)
type macro_owner = {
	mo_domain : Domain.id;
	mutable mo_count : int;
	mo_seq : int;
	(* what the claim was taken for, and when *)
	mo_reason : string;
	mo_since : float;
}

(* the time the claim of the macros was held, by what it was taken for, and
   how many times (with [mutex]) *)
let claim_held : (string, (float * int)) Hashtbl.t = Hashtbl.create 0

let add_held tbl reason d =
	let (t,n) = try Hashtbl.find tbl reason with Not_found -> (0.,0) in
	Hashtbl.replace tbl reason (t +. d,n + 1)

let macro_owners : macro_owner list ref = ref []
let claim_seq = ref 0

let held () = Mutex.protect mutex (fun () -> Hashtbl.fold (fun r v acc -> (r,v) :: acc) claim_held [])

(* Takes the claim of the macros (see [macro_owner]), giving what to release. *)
let take_macro_claim ?(reason="macro") () =
	let me = Domain.self () in
	let take () =
		incr claim_seq;
		let o = { mo_domain = me; mo_count = 1; mo_seq = !claim_seq; mo_reason = reason; mo_since = Unix.gettimeofday () } in
		macro_owners := o :: !macro_owners;
		(* the domains waiting for the claim wait for its owner to change: a
		   domain let through to break a cycle that takes it over must wake
		   them, or the one whose condition now holds sleeps on while the cycle
		   search of the others takes it for running *)
		Condition.broadcast done_;
		o
	in
	let rec claim () =
		match !macro_owners with
		| [] ->
			take ()
		| o :: _ when o.mo_domain = me ->
			o.mo_count <- o.mo_count + 1;
			o
		| o :: _ ->
			let changed () = match !macro_owners with o' :: _ -> o' != o | [] -> true in
			if wait ~kind:WaitMacros ~seq:o.mo_seq ~owner:o.mo_domain changed then claim ()
			else take ()
	in
	(* the typing lock is only released to wait *)
	let o = Mutex.protect mutex (fun () -> match !macro_owners with
		| [] -> Some (take ())
		| o :: _ when o.mo_domain = me -> o.mo_count <- o.mo_count + 1; Some o
		| _ -> None
	) in
	match o with
	| Some o -> o
	| None -> TypingLock.without (fun () -> Mutex.protect mutex claim)

let release_macro_claim o =
	Mutex.protect mutex (fun () ->
		o.mo_count <- o.mo_count - 1;
		if o.mo_count = 0 then begin
			add_held claim_held o.mo_reason (Unix.gettimeofday () -. o.mo_since);
			macro_owners := List.filter (fun o' -> o' != o) !macro_owners;
			Condition.broadcast done_
		end
	)

let with_macro_claim ?reason f =
	if not !TypingLock.active then f () else begin
		let o = take_macro_claim ?reason () in
		match f () with
		| v -> release_macro_claim o; v
		| exception e -> release_macro_claim o; raise e
	end

(* The tasks of a parallel batch, started in their order. Their effects do not
   wait for the tasks before them: the output does not depend on the order in
   which they happen (-D typer-shuffle checks it on one domain). *)
let task_count = ref 0
let task_next = ref 0

let start_tasks n =
	task_count := n;
	task_next := 0

(* the next task to start, which the current domain runs *)
let next_task () =
	Mutex.protect mutex (fun () ->
		let i = !task_next in
		if i >= !task_count then None
		else begin
			incr task_next;
			Some i
		end
	)

(* A macro can run without the claim of the macros while it touches nothing
   that the macros share (the state of the interpreter, what a macro changes
   in the compilation): it takes the claim at its first such access
   ([end_speculation]) and keeps it until it is done. *)
let speculating : bool Domain.DLS.key = Domain.DLS.new_key (fun () -> false)
(* the claim taken by the speculation of the current domain *)
let speculation_claim : macro_owner option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)

let is_speculating () = Domain.DLS.get speculating

(* how many macros started speculating, and how many of them were done
   without taking the claim of the macros *)
let speculations = Atomic.make 0
let speculations_done = Atomic.make 0

(* Ends the speculation of the current domain: takes the claim of the macros.
   [reason]: what the macro accesses, reported by --times. *)
let end_speculation ?(reason=(fun () -> "other")) () =
	if Domain.DLS.get speculating then begin
		Domain.DLS.set speculating false;
		if !TypingLock.active then Domain.DLS.set speculation_claim (Some (take_macro_claim ~reason:("speculation ended: " ^ reason ()) ()))
	end

(* Runs [f] speculating if [b] (see [speculating]), within the speculation
   of the current domain if any. *)
let speculate b f =
	if not b || Domain.DLS.get speculating then f () else begin
		let old_claim = Domain.DLS.get speculation_claim in
		Atomic.incr speculations;
		Domain.DLS.set speculating true;
		Domain.DLS.set speculation_claim None;
		Std.finally (fun () ->
			(match Domain.DLS.get speculation_claim with
			| Some o -> release_macro_claim o
			| None -> Atomic.incr speculations_done);
			Domain.DLS.set speculating false;
			Domain.DLS.set speculation_claim old_claim
		) f ()
	end
