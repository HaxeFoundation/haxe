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
	| WaitOrder
	| WaitMacros
	| WaitValue
	| WaitClass

let rank = function
	| WaitOrder -> -1
	| WaitMacros -> 0
	| WaitValue -> 1
	| WaitClass -> 2

(* the domains waiting, with the domain they wait for, what they wait for and,
   for a class, when its build started *)
let waiting : (Domain.id * (Domain.id * wait_kind * int)) list ref = ref []

(* the waiting domains let through to break a cycle *)
let let_through : Domain.id list ref = ref []

(* Waits, with [mutex] held, until [cond ()] holds, or until let through to
   break a cycle of waiting domains (see [wait_kind]). Tells whether [cond ()]
   holds. *)
let wait ?(kind=WaitValue) ?(seq=0) ~owner cond =
	let me = Domain.self () in
	let rec chain d acc =
		if d = me then Some acc
		(* a cycle without the current domain: its domains break it *)
		else if List.mem_assoc d acc then None
		else match List.assoc_opt d !waiting with
			| Some (d',k,n) -> chain d' ((d,(k,n)) :: acc)
			| None -> None
	in
	let through () = List.mem me !let_through in
	let block () =
		waiting := (me,(owner,kind,seq)) :: !waiting;
		while not (cond () || through ()) do Condition.wait done_ mutex done;
		waiting := List.remove_assoc me !waiting;
		let_through := List.filter (fun d -> d <> me) !let_through
	in
	match chain owner [] with
	| None ->
		block ();
		cond ()
	| Some cycle ->
		(* the domain of the cycle to let through: the current one when it is as good;
		   among classes, the one whose build started first, as the outermost build
		   of a cycle on one domain is the one read partially *)
		let better (k,n) (k',n') = rank k' < rank k || (rank k' = rank k && n' < n) in
		let best = List.fold_left (fun (d,kn) (d',kn') -> if better kn kn' then (d',kn') else (d,kn)) (me,(kind,seq)) cycle in
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
}

let macro_owners : macro_owner list ref = ref []
let claim_seq = ref 0

let with_macro_claim f =
	if not !TypingLock.active then f () else begin
		let me = Domain.self () in
		let take () =
			incr claim_seq;
			let o = { mo_domain = me; mo_count = 1; mo_seq = !claim_seq } in
			macro_owners := o :: !macro_owners;
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
		let release o =
			Mutex.protect mutex (fun () ->
				o.mo_count <- o.mo_count - 1;
				if o.mo_count = 0 then begin
					macro_owners := List.filter (fun o' -> o' != o) !macro_owners;
					Condition.broadcast done_
				end
			)
		in
		(* the typing lock is only released to wait *)
		let o = Mutex.protect mutex (fun () -> match !macro_owners with
			| [] -> Some (take ())
			| o :: _ when o.mo_domain = me -> o.mo_count <- o.mo_count + 1; Some o
			| _ -> None
		) in
		let o = match o with
			| Some o -> o
			| None -> TypingLock.without (fun () -> Mutex.protect mutex claim)
		in
		match f () with
		| v -> release o; v
		| exception e -> release o; raise e
	end

(* The order of the tasks of a parallel batch: the tasks are started in their
   order, and the effects that one domain has in the order of the tasks (the
   builds of classes, the macros, the loading of modules) wait for the tasks
   before them to be done, so that they happen in the order they have on one
   domain. Going on out of order is what breaks a cycle of waiting domains
   first. *)
let task_runners : Domain.id array ref = ref [||]
let task_done : bool array ref = ref [||]
(* the first task not done *)
let task_prefix = ref 0
let task_next = ref 0
let current_task : int Domain.DLS.key = Domain.DLS.new_key (fun () -> -1)

let start_tasks n =
	task_runners := Array.make n (Domain.self ());
	task_done := Array.make n false;
	task_prefix := 0;
	task_next := 0

(* the next task to start, which the current domain runs *)
let next_task () =
	Mutex.protect mutex (fun () ->
		let i = !task_next in
		if i >= Array.length !task_done then None
		else begin
			incr task_next;
			!task_runners.(i) <- Domain.self ();
			Some i
		end
	)

let run_task i f =
	Domain.DLS.set current_task i;
	let finish () =
		Domain.DLS.set current_task (-1);
		Mutex.protect mutex (fun () ->
			!task_done.(i) <- true;
			let n = Array.length !task_done in
			while !task_prefix < n && !task_done.(!task_prefix) do incr task_prefix done;
			Condition.broadcast done_
		)
	in
	match f () with
	| v -> finish (); v
	| exception e -> finish (); raise e

let in_order () =
	let i = Domain.DLS.get current_task in
	if i > 0 && !TypingLock.active && Mutex.protect mutex (fun () -> !task_prefix < i) then
		TypingLock.without (fun () -> Mutex.protect mutex (fun () ->
			let rec loop () =
				let p = !task_prefix in
				if p < i then begin
					ignore (wait ~kind:WaitOrder ~seq:p ~owner:!task_runners.(p) (fun () -> !task_prefix <> p));
					(* else: let through to break a cycle *)
					if !task_prefix <> p then loop ()
				end
			in
			loop ()
		))
