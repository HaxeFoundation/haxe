
let run_parallel_for num_domains ?(chunk_size=0) length f =
	let pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) () in
	Domainslib.Task.run pool (fun _ -> Domainslib.Task.parallel_for pool ~chunk_size ~start:0 ~finish:(length-1) ~body:f);
	Domainslib.Task.teardown_pool pool;
