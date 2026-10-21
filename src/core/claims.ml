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

let kind_names = [|"order";"macros";"value";"class"|]

(* the domains waiting, with the domain they wait for, what they wait for and,
   for a class, when its build started, and the condition they wait for *)
let waiting : (Domain.id * (Domain.id * wait_kind * int * (unit -> bool))) list ref = ref []

(* the time domains spent waiting, and how many times, by kind *)
let waited = Array.make 4 0.
let waits = Array.make 4 0
(* the cycles broken, by kind of the wait let through *)
let breaks = Array.make 4 0

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
			(* a domain whose condition holds, or let through, is about to go on *)
			| Some (_,_,_,cond') when cond' () || List.mem d !let_through -> None
			| Some (d',k,n,_) -> chain d' ((d,(k,n)) :: acc)
			| None -> None
	in
	let through () = List.mem me !let_through in
	let block () =
		waiting := (me,(owner,kind,seq,cond)) :: !waiting;
		if not (cond () || through ()) then begin
			let t = Unix.gettimeofday () in
			while not (cond () || through ()) do Condition.wait done_ mutex done;
			let k = rank kind + 1 in
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
		   among classes, the one whose build started first, as the outermost build
		   of a cycle on one domain is the one read partially *)
		let better (k,n) (k',n') = rank k' < rank k || (rank k' = rank k && n' < n) in
		let best = List.fold_left (fun (d,kn) (d',kn') -> if better kn kn' then (d',kn') else (d,kn)) (me,(kind,seq)) cycle in
		let k = rank (fst (snd best)) + 1 in
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

(* what waits for the order of the tasks, the time it waited and how many times *)
type order_reason =
	| OrderBuild
	| OrderMacro
	| OrderModule

let order_index = function
	| OrderBuild -> 0
	| OrderMacro -> 1
	| OrderModule -> 2

let order_reasons = [|"build";"macro";"module"|]
let order_waited = Array.make 3 0.
let order_waits = Array.make 3 0

(* A macro of a task can run before the tasks before it are done, while it
   touches nothing that the macros share (the state of the interpreter, what a
   macro changes in the compilation): it then waits for the order of the tasks
   at its first such access ([in_order]), releasing the claim of the macros
   meanwhile, and goes on in order. *)
let speculating : bool Domain.DLS.key = Domain.DLS.new_key (fun () -> false)

let is_speculating () = Domain.DLS.get speculating

(* Called when a speculating domain waits for the order of the tasks, which
   lets other macros run: gives what puts its state back (the macro API). *)
let speculation_resume = ref (fun () -> (fun () -> ()))

(* Whether the current domain would have to wait for the order of the tasks. *)
let ahead_of_order () =
	let i = Domain.DLS.get current_task in
	i > 0 && !TypingLock.active && Mutex.protect mutex (fun () -> !task_prefix < i)

(* how many macros started speculating, and how many of them were done
   without waiting for the order of the tasks *)
let speculations = Atomic.make 0
let speculations_done = Atomic.make 0

(* Runs [f] speculating if [b] (see [speculating]). *)
let speculate b f =
	let old = Domain.DLS.get speculating in
	if b then begin
		Atomic.incr speculations;
		Domain.DLS.set speculating true
	end;
	Std.finally (fun () ->
		(* once in order, the macros around are in order as well *)
		if Domain.DLS.get speculating then begin
			if b && not old then Atomic.incr speculations_done;
			Domain.DLS.set speculating old
		end
	) f ()

(* Releases the claim of the macros of the current domain, with [mutex] held:
   the record to take back. *)
let suspend_macro_claim () =
	let me = Domain.self () in
	match List.find_opt (fun o -> o.mo_domain = me) !macro_owners with
	| None ->
		None
	| Some o ->
		macro_owners := List.filter (fun o' -> o' != o) !macro_owners;
		Condition.broadcast done_;
		Some o

(* Takes back the claim of the macros, with [mutex] held. *)
let rec resume_macro_claim o =
	match !macro_owners with
	| [] ->
		macro_owners := [o]
	| o' :: _ ->
		let changed () = match !macro_owners with o'' :: _ -> o'' != o' | [] -> true in
		if wait ~kind:WaitMacros ~seq:o'.mo_seq ~owner:o'.mo_domain changed then resume_macro_claim o
		else macro_owners := o :: !macro_owners

(* Waits, with [mutex] held, until the tasks before task [i] are done (or until
   let through to break a cycle). *)
let wait_for_tasks_before i =
	let rec loop () =
		let p = !task_prefix in
		if p < i then begin
			ignore (wait ~kind:WaitOrder ~seq:p ~owner:!task_runners.(p) (fun () -> !task_prefix <> p));
			(* else: let through to break a cycle *)
			if !task_prefix <> p then loop ()
		end
	in
	loop ()

let in_order reason =
	let why = order_index reason in
	let i = Domain.DLS.get current_task in
	let spec = Domain.DLS.get speculating in
	if spec then Domain.DLS.set speculating false;
	if i > 0 && !TypingLock.active && Mutex.protect mutex (fun () -> !task_prefix < i) then begin
		(* a speculating macro lets the macros run meanwhile, then gets its state back *)
		let resume = if spec then !speculation_resume () else (fun () -> ()) in
		Std.finally resume TypingLock.without (fun () -> Mutex.protect mutex (fun () ->
			let suspended = if spec then suspend_macro_claim () else None in
			let t = Unix.gettimeofday () in
			Std.finally (fun () ->
				order_waited.(why) <- order_waited.(why) +. (Unix.gettimeofday () -. t);
				order_waits.(why) <- order_waits.(why) + 1;
				match suspended with Some o -> resume_macro_claim o | None -> ()
			) wait_for_tasks_before i
		))
	end
