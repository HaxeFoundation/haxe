let enable = ref true

let run_parallel_for num_domains ?(chunk_size=0) length f =
	if not !enable then begin
		for i = 0 to length - 1 do
			f i
		done
	end else
		let pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) () in
		Domainslib.Task.run pool (fun _ -> Domainslib.Task.parallel_for pool ~chunk_size ~start:0 ~finish:(length-1) ~body:f);
		Domainslib.Task.teardown_pool pool

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

module ParallelSeq = struct
	let iter pool f seq =
		ParallelArray.iter pool f (Array.of_seq seq)
end

let run_in_new_pool timer_ctx f =
	if not !enable then
		f None
	else
		let pool = Timer.time timer_ctx ["domainslib";"setup"] (Domainslib.Task.setup_pool ~num_domains:(Domain.recommended_domain_count() - 1)) () in
		Std.finally (fun () -> Timer.time timer_ctx ["domainslib";"teardown"] Domainslib.Task.teardown_pool pool) (Domainslib.Task.run pool) (fun () -> f (Some pool))
