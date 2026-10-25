let enable = ref true

let is_enabled () = !enable

module ParallelArray = struct
	let iter pool f a =
		match pool with
		| None ->
			Array.iter f a
		| Some pool ->
			let f' idx = f a.(idx) in
			Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length a - 1) ~body:f'

	let map pool f a x =
		match pool with
		| None ->
			Array.map f a
		| Some pool ->
			let length = Array.length a in
			let a_out = Array.make length x in
			let f' idx =
				Array.unsafe_set a_out idx (f (Array.unsafe_get a idx))
			in
			Domainslib.Task.parallel_for pool ~start:0 ~finish:(length - 1) ~body:f';
			a_out
end

(* The minor heap of the worker domains of a pool, in MB: allocation-heavy work
   on several domains (typing, filters, the analyzer) otherwise spends its time
   in the minor collections, which stop every domain. A new domain gets the
   default size whatever the domain that creates it uses, so each worker sets
   its own. 16MB: as fast as 32MB on SpaceCraft, and each domain of the pool
   (one per hardware thread) pays it in memory (about 0.5GB less on 22 threads).
   HAXE_WORKER_MINOR_MB overrides it (0 keeps the default). *)
let worker_minor_heap_mb () =
	match Sys.getenv_opt "HAXE_WORKER_MINOR_MB" with
	| None | Some "" -> 16
	| Some s -> (try int_of_string s with _ -> 16)

let set_worker_minor_heap pool =
	let mb = worker_minor_heap_mb () in
	if mb > 0 then begin
		let words = mb * 1024 * 1024 / (Sys.word_size / 8) in
		let n = Domainslib.Task.get_num_domains pool in
		let arrived = Atomic.make 0 in
		(* every domain of the pool takes one of the [n] tasks: each waits for the
		   others (for a while) so that no domain takes two *)
		Domainslib.Task.run pool (fun () ->
			Domainslib.Task.parallel_for pool ~chunk_size:1 ~start:0 ~finish:(n - 1) ~body:(fun _ ->
				if (Gc.get ()).Gc.minor_heap_size < words then
					Gc.set { (Gc.get ()) with Gc.minor_heap_size = words };
				Atomic.incr arrived;
				let t = Unix.gettimeofday () in
				while Atomic.get arrived < n && Unix.gettimeofday () -. t < 0.5 do Domain.cpu_relax () done
			)
		)
	end

(* Runs [f] on each domain of the pool (at most once on each). *)
let on_each_domain pool f =
	match pool with
	| None ->
		f ()
	| Some pool ->
		let n = Domainslib.Task.get_num_domains pool in
		Domainslib.Task.parallel_for pool ~chunk_size:1 ~start:0 ~finish:(n - 1) ~body:(fun _ -> f ())

module ParallelSeq = struct
	let iter pool f seq =
		ParallelArray.iter pool f (Array.of_seq seq)
end

(* A pool that can be acquired on demand and released when idle.
   Unlike AtomicLazy, supports teardown and re-creation so that
   domain workers don't stay alive during phases that don't need them
   (e.g. eval interpretation). *)
module ManagedPool = struct
	type t = {
		setup : unit -> Domainslib.Task.pool;
		mutable pool : Domainslib.Task.pool option;
		mutex : Mutex.t;
	}

	let create setup = {
		setup;
		pool = None;
		mutex = Mutex.create ();
	}

	let acquire mp =
		Mutex.protect mp.mutex (fun () ->
			match mp.pool with
			| Some p -> p
			| None ->
				let p = mp.setup () in
				mp.pool <- Some p;
				p
		)

	let release mp =
		Mutex.protect mp.mutex (fun () ->
			match mp.pool with
			| Some p ->
				Domainslib.Task.teardown_pool p;
				mp.pool <- None
			| None -> ()
		)

	let is_active mp =
		Mutex.protect mp.mutex (fun () ->
			mp.pool <> None
		)
end

let run_in_new_pool timer_ctx f =
	if not !enable then
		f None
	else
		let pool = Timer.time timer_ctx ["domainslib";"setup"] (Domainslib.Task.setup_pool ~num_domains:(Domain.recommended_domain_count() - 1)) () in
		Std.finally (fun () -> Timer.time timer_ctx ["domainslib";"teardown"] Domainslib.Task.teardown_pool pool) (Domainslib.Task.run pool) (fun () -> f (Some pool))

let run_with_pool mp f =
	if not !enable then
		f None
	else
		let pool = ManagedPool.acquire mp in
		Domainslib.Task.run pool (fun () -> f (Some pool))
