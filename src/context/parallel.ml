let run_parallel_for num_domains ?(chunk_size=0) length f =
	let pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) () in
	Domainslib.Task.run pool (fun _ -> Domainslib.Task.parallel_for pool ~chunk_size ~start:0 ~finish:(length-1) ~body:f);
	Domainslib.Task.teardown_pool pool

module ParallelArray = struct
	let iter pool f a =
		let f' idx = f a.(idx) in
		Domainslib.Task.run pool (fun _ -> Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length a - 1) ~body:f')
end

module ParallelSeq = struct
	let iter pool f seq =
		ParallelArray.iter pool f (Array.of_seq seq)
end

let run_in_new_pool timer_ctx f =
	let pool = Timer.time timer_ctx ["domainslib";"setup"] (Domainslib.Task.setup_pool ~num_domains:(Domain.recommended_domain_count() - 1)) () in
	Std.finally (fun () -> Timer.time timer_ctx ["domainslib";"teardown"] Domainslib.Task.teardown_pool pool) f pool
