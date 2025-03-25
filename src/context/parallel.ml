type context = {
	pool : Domainslib.Task.pool;
}

let create_context num_domains =
	let ctx = {
		pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) ();
	} in
	Some ctx

let release_context ctx =
	(match ctx with
	| None ->
		()
	| Some ctx ->
		Domainslib.Task.teardown_pool ctx.pool;
	);
	None

let run_parallel_for ctx ?(chunk_size=0) length f =
	(match ctx with
	| None ->
		for i = 0 to length-1 do f i done
	| Some ctx ->
		Domainslib.Task.run ctx.pool (fun _ -> Domainslib.Task.parallel_for ctx.pool ~chunk_size ~start:0 ~finish:(length-1) ~body:f);
	)
