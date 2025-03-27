let run_parallel_for num_domains ?(chunk_size=0) length f =
	let pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) () in
	Domainslib.Task.run pool (fun _ -> Domainslib.Task.parallel_for pool ~chunk_size ~start:0 ~finish:(length-1) ~body:f);
	Domainslib.Task.teardown_pool pool

let run_parallel_on_array pool a f =
	let f' idx = f a.(idx) in
	let old = Atomic.exchange Timer.in_parallel true in
	Domainslib.Task.run pool (fun _ -> Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length a - 1) ~body:f');
	Atomic.set Timer.in_parallel old

let run_parallel_on_seq pool seq f =
	run_parallel_on_array pool (Array.of_seq seq) f

let run_in_new_pool f =
	let pool = Domainslib.Task.setup_pool ~num_domains:(Domain.recommended_domain_count()) () in
	Std.finally (fun () -> Domainslib.Task.teardown_pool pool) f pool
