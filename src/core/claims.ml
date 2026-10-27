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

(* A batch of tasks of the macro context (the typing of the modules a macro
   load brings): its domain lends it the claim of the
   macros it holds (see [lent]), and the domains waiting for that claim run its
   tasks instead of sleeping ([help], set by the batch). *)
let in_batch : bool Domain.DLS.key = Domain.DLS.new_key (fun () -> false)
let no_help () = false
let help : (unit -> bool) ref = ref no_help

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
			TypingTrace.event (fun () -> Printf.sprintf "wait %s for %d" kind_names.(rank kind) (owner :> int));
			let t = Unix.gettimeofday () in
			while not (cond () || through ()) do
				(* waiting for the claim of the macros lent to a batch: run its tasks *)
				(* only when a claim is lent: while it helps, a broadcast is lost and a
				   domain starting to wait does not see a cycle through it *)
				let helped = kind = WaitMacros && not (Domain.DLS.get in_batch) && !help != no_help && begin
					(* not waiting while it runs a task: a domain waiting for it
					   does not close a cycle through its wait *)
					let entry = List.assoc me !waiting in
					waiting := List.remove_assoc me !waiting;
					Mutex.unlock mutex;
					let r = !help () in
					Mutex.lock mutex;
					waiting := (me,entry) :: !waiting;
					r
				end in
				(* [mutex] was released to help: a broadcast sent meanwhile is lost,
				   the condition is checked again before sleeping *)
				if not (helped || cond () || through ()) then Condition.wait done_ mutex
			done;
			let k = rank kind in
			TypingTrace.event (fun () -> "waited " ^ kind_names.(k));
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
	(* whether the claim is taken to change what the macros share *)
	mo_effects : bool;
}

(* how many times a claim taken to change what the macros share was released,
   by any domain and by the current one (see [speculate]) *)
let effects_epoch = Atomic.make 0
let own_effects : int Domain.DLS.key = Domain.DLS.new_key (fun () -> 0)

(* the time the claim of the macros was held, by what it was taken for, and
   how many times (with [mutex]) *)
let claim_held : (string, (float * int)) Hashtbl.t = Hashtbl.create 0

let add_held tbl reason d =
	let (t,n) = try Hashtbl.find tbl reason with Not_found -> (0.,0) in
	Hashtbl.replace tbl reason (t +. d,n + 1)

let macro_owners : macro_owner list ref = ref []
let claim_seq = ref 0

(* the claim lent to the tasks of a batch (see [in_batch]): a task takes it
   over (one at a time) when it is on top, the domain that lent it included,
   so that the macros still run on one domain at a time *)
let lent : macro_owner option ref = ref None

let is_lent o = Domain.DLS.get in_batch && (match !lent with Some l -> l == o | None -> false)

let held () = Mutex.protect mutex (fun () -> Hashtbl.fold (fun r v acc -> (r,v) :: acc) claim_held [])

(* Takes the claim of the macros (see [macro_owner]), giving what to release. *)
let take_macro_claim ?(reason="macro") ?(effects=true) () =
	let me = Domain.self () in
	let take () =
		incr claim_seq;
		let o = { mo_domain = me; mo_count = 1; mo_seq = !claim_seq; mo_reason = reason; mo_since = Unix.gettimeofday (); mo_effects = effects } in
		TypingTrace.event (fun () -> "claim " ^ reason);
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
		| o :: _ when is_lent o ->
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
		| o :: _ when is_lent o -> Some (take ())
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
			if o.mo_effects then begin
				Atomic.incr effects_epoch;
				Domain.DLS.set own_effects (Domain.DLS.get own_effects + 1)
			end;
			TypingTrace.event (fun () -> "release " ^ o.mo_reason);
			macro_owners := List.filter (fun o' -> o' != o) !macro_owners;
			Condition.broadcast done_
		end
	)

let with_macro_claim ?reason ?effects f =
	if not !TypingLock.active then f () else begin
		let o = take_macro_claim ?reason ?effects () in
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

(* Frozen persistent state (docs/incremental-compilation.md, §3.5, step 13):
   the @:persistent static variables of the macros are frozen at the start of
   a parallel batch, when no macro runs (see [EvalValue.freeze_persistent]). A
   speculating macro reading a frozen variable goes on speculating, as a
   reader of its group (what the variable holds). A macro changing a frozen
   group (writing the variable, or changing an object it holds) ends its own
   speculation first, then thaws the group, which invalidates the speculating
   readers of the group: such a macro has no effect yet, it is stopped at its
   next access to what the macros share (or when it is done) and runs again
   with the claim of the macros. A writer never waits for the readers. *)
exception Invalidated

type reader = {
	r_invalid : bool Atomic.t;
	(* the groups read, by their root *)
	mutable r_groups : int list;
}

(* the speculating domains that read frozen groups (with [mutex]) *)
let readers : (Domain.id * reader) list ref = ref []
(* the reader of the speculation of the current domain *)
let current_reader : reader option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)
(* how deep the current macro is nested in the macro that started the
   speculation: only that one can read a frozen group, the effects of the
   nested ones (the classes built by a macro typing something...) stay *)
let speculation_depth : int Domain.DLS.key = Domain.DLS.new_key (fun () -> 0)
(* whether the speculation of the current domain was invalidated (once it is
   not a reader any more) *)
let invalidated : bool Domain.DLS.key = Domain.DLS.new_key (fun () -> false)
(* the effects (see [effects_epoch]) when the speculation of the current
   domain started: one that ends after other domains changed what the macros
   share may have read what they changed (a type not defined yet...) *)
let speculation_epoch : (int * int) Domain.DLS.key = Domain.DLS.new_key (fun () -> (0,0))

(* How many calls of the macro API the current domain is in: what such a call
   runs (the typing of a module, the initializers of the classes a macro load
   brings, the macros they run) is not the code of the macro that made it,
   which cannot be stopped from there. *)
let api_depth : int Domain.DLS.key = Domain.DLS.new_key (fun () -> 0)
(* the API depth of the code of the macro that started the speculation *)
let speculation_api : int Domain.DLS.key = Domain.DLS.new_key (fun () -> 0)

let in_api f =
	let d = Domain.DLS.get api_depth in
	Domain.DLS.set api_depth (d + 1);
	Std.finally (fun () -> Domain.DLS.set api_depth d) f ()

(* The macros nested in the macro that started the speculation of the current
   domain (run by the typing of an API call), innermost first. One that starts
   while the domain speculates may read what another domain changes before
   the claim is taken: it is checked at the first access to what the macros
   share made by its own code, or when it ends if a macro nested in it took
   the claim, and runs again (alone, with the claim) if another domain changed
   what the macros share since it started. Its caller is checked the same way
   once it is done (the macro that started the speculation by [invalidated]):
   no macro is stopped through the typing that runs it. *)
type frame = {
	(* the effects when it started (see [speculation_epoch]) *)
	fr_epoch : int * int;
	(* the API depth of its code *)
	fr_api : int;
	(* whether it was checked, or started with the claim *)
	mutable fr_checked : bool;
}

exception Rerun of frame

let frames : frame list Domain.DLS.key = Domain.DLS.new_key (fun () -> [])

(* Whether other domains changed what the macros share since [epoch]. *)
let effects_since (e0,o0) =
	Atomic.get effects_epoch - e0 > Domain.DLS.get own_effects - o0

(* how many frozen variables were read speculating, and how many macros ran
   again *)
let frozen_reads = Atomic.make 0
let reruns = Atomic.make 0

(* Stops the current macro if its speculation was invalidated. A macro
   nested in it is not stopped: what stops it would go through the typing
   that runs it, which is not undone; the macro that started the speculation
   runs again once it is done. *)
let check_valid () =
	if Domain.DLS.get speculation_depth = 0 && Domain.DLS.get api_depth = Domain.DLS.get speculation_api then match Domain.DLS.get current_reader with
	| Some r when Atomic.get r.r_invalid -> raise Invalidated
	| _ -> if Domain.DLS.get invalidated then raise Invalidated

(* Whether the current domain runs a speculation (or the macro that started it
   ended it). *)
let in_speculation () = Domain.DLS.get speculating || Domain.DLS.get speculation_claim <> None

(* Tells whether the speculating macro of the current domain may read group
   [g] without the claim of the macros ([frozen ()]: whether the group is
   frozen, checked with [mutex], which thawing it holds). *)
let read_frozen g frozen =
	Domain.DLS.get speculating && Domain.DLS.get speculation_depth = 0 && !TypingLock.active &&
	Mutex.protect mutex (fun () ->
		frozen () && begin
			let r = match Domain.DLS.get current_reader with
				| Some r ->
					r
				| None ->
					let r = { r_invalid = Atomic.make false; r_groups = [] } in
					Domain.DLS.set current_reader (Some r);
					readers := (Domain.self (),r) :: !readers;
					r
			in
			if not (List.mem g r.r_groups) then r.r_groups <- g :: r.r_groups;
			Atomic.incr frozen_reads;
			true
		end
	)

(* Thaws group [g] ([thaw ()] does it, if it is still frozen), invalidating
   its other readers. *)
let thaw g frozen thaw =
	Mutex.protect mutex (fun () ->
		if frozen () then begin
			thaw ();
			let me = Domain.self () in
			List.iter (fun (d,r) -> if d <> me && List.mem g r.r_groups then Atomic.set r.r_invalid true) !readers
		end
	)

(* The current domain is not a reader any more (keeping whether it was
   invalidated). *)
let stop_reading () =
	match Domain.DLS.get current_reader with
	| None ->
		()
	| Some r ->
		Mutex.protect mutex (fun () ->
			readers := List.filter (fun (_,r') -> r' != r) !readers;
			if Atomic.get r.r_invalid then Domain.DLS.set invalidated true
		);
		Domain.DLS.set current_reader None

(* Ends the speculation of the current domain: takes the claim of the macros.
   [reason]: what the macro accesses, reported by --times. Stops the macro if
   its speculation was invalidated (it runs again, see [speculate]). *)
let end_speculation ?(reason=(fun () -> "other")) () =
	if Domain.DLS.get speculating then begin
		Domain.DLS.set speculating false;
		if !TypingLock.active then begin
			Domain.DLS.set speculation_claim (Some (take_macro_claim ~reason:("speculation ended: " ^ reason ()) ()));
			(* the groups it read cannot change while it holds the claim *)
			stop_reading ();
			if effects_since (Domain.DLS.get speculation_epoch) then Domain.DLS.set invalidated true;
			check_valid ()
		end
	end else if Domain.DLS.get speculation_claim <> None then
		(* ended by a nested macro: the macro that started it is stopped
		   before it changes anything *)
		check_valid ();
	(* the innermost nested macro, if its own code accesses it *)
	if not (Domain.DLS.get speculating) then match Domain.DLS.get frames with
		| fr :: _ when not fr.fr_checked && Domain.DLS.get api_depth = fr.fr_api ->
			fr.fr_checked <- true;
			if effects_since fr.fr_epoch then raise (Rerun fr)
		| _ ->
			()

(* Runs [f] speculating if [b] (see [speculating]), within the speculation
   of the current domain if any. A speculation invalidated by a writer of a
   frozen group it read runs [f] again with the claim of the macros. *)
let speculate b f =
	if in_speculation () then begin
		(* nested in the macro that started the speculation (see [frame]) *)
		let depth = Domain.DLS.get speculation_depth and old_frames = Domain.DLS.get frames in
		let fr = {
			fr_epoch = (Atomic.get effects_epoch,Domain.DLS.get own_effects);
			fr_api = Domain.DLS.get api_depth;
			fr_checked = not (Domain.DLS.get speculating);
		} in
		Domain.DLS.set speculation_depth (depth + 1);
		Domain.DLS.set frames (fr :: old_frames);
		let rerun () =
			Atomic.incr reruns;
			TypingTrace.event (fun () -> "nested macro run again");
			f ()
		in
		let run () = match f () with
			| v ->
				(* a macro nested in it took the claim: checked once it is done *)
				if fr.fr_checked || Domain.DLS.get speculating then v
				else begin
					fr.fr_checked <- true;
					if effects_since fr.fr_epoch then rerun () else v
				end
			| exception (Rerun fr') when fr' == fr ->
				rerun ()
		in
		Std.finally (fun () ->
			Domain.DLS.set speculation_depth depth;
			Domain.DLS.set frames old_frames
		) run ()
	end else if not b then
		f ()
	else begin
		let old_claim = Domain.DLS.get speculation_claim in
		Atomic.incr speculations;
		Domain.DLS.set speculating true;
		Domain.DLS.set speculation_claim None;
		Domain.DLS.set invalidated false;
		Domain.DLS.set speculation_epoch (Atomic.get effects_epoch,Domain.DLS.get own_effects);
		let old_api = Domain.DLS.get speculation_api in
		Domain.DLS.set speculation_api (Domain.DLS.get api_depth);
		let finish () =
			Domain.DLS.set speculation_api old_api;
			stop_reading ();
			let invalid = Domain.DLS.get invalidated in
			Domain.DLS.set invalidated false;
			(match Domain.DLS.get speculation_claim with
			| Some o -> release_macro_claim o
			| None -> if not invalid then Atomic.incr speculations_done);
			Domain.DLS.set speculating false;
			Domain.DLS.set speculation_claim old_claim;
			invalid
		in
		let rerun () =
			Atomic.incr reruns;
			with_macro_claim ~reason:"macro run again" f
		in
		match f () with
		| v -> if finish () then rerun () else v
		| exception e -> if finish () then rerun () else raise e
	end

(* The claim of the macros the current domain holds, lent to the tasks of a
   batch while [f] runs them ([run_task] runs the next one, if any): the
   domains waiting for it run tasks too. The speculation state of a domain
   running a task is its own (a task types what the domain lending the claim
   would type). *)
let lend_claim run_task f =
	let o = Mutex.protect mutex (fun () -> match !macro_owners with
		| o :: _ when o.mo_domain = Domain.self () && !lent = None -> Some o
		| _ -> None
	) in
	match o with
	| None ->
		(* nothing to lend (no claim, or one lent already): the tasks run
		   on the domains of the batch only *)
		f ()
	| Some o ->
		Mutex.protect mutex (fun () ->
			lent := Some o;
			help := run_task;
			Condition.broadcast done_
		);
		Std.finally (fun () -> Mutex.protect mutex (fun () ->
			lent := None;
			help := no_help;
			Condition.broadcast done_
		)) f ()

(* Runs [f] as a task of a batch, with the speculation state of a domain
   that holds no claim, restoring the current one afterwards. *)
let as_batch_task f =
	let s = (Domain.DLS.get speculating,Domain.DLS.get speculation_claim,Domain.DLS.get current_reader,
		Domain.DLS.get speculation_depth,Domain.DLS.get invalidated,Domain.DLS.get speculation_epoch,Domain.DLS.get in_batch) in
	let s' = (Domain.DLS.get frames,Domain.DLS.get api_depth,Domain.DLS.get speculation_api) in
	Domain.DLS.set frames [];
	Domain.DLS.set api_depth 0;
	Domain.DLS.set speculation_api 0;
	Domain.DLS.set speculating false;
	Domain.DLS.set speculation_claim None;
	Domain.DLS.set current_reader None;
	Domain.DLS.set speculation_depth 0;
	Domain.DLS.set invalidated false;
	Domain.DLS.set in_batch true;
	let restore () =
		let (a,b,c,d,e,f,g) = s in
		Domain.DLS.set speculating a;
		Domain.DLS.set speculation_claim b;
		Domain.DLS.set current_reader c;
		Domain.DLS.set speculation_depth d;
		Domain.DLS.set invalidated e;
		Domain.DLS.set speculation_epoch f;
		Domain.DLS.set in_batch g;
		let (h,i,j) = s' in
		Domain.DLS.set frames h;
		Domain.DLS.set api_depth i;
		Domain.DLS.set speculation_api j
	in
	Std.finally restore f ()

(* Wakes the domains waiting with [mutex] (a task of a batch is done). *)
let notify_done () = Mutex.protect mutex (fun () -> Condition.broadcast done_)

(* Waits until [cond ()] holds, with [mutex]. *)
let wait_done cond = Mutex.protect mutex (fun () -> while not (cond ()) do Condition.wait done_ mutex done)
