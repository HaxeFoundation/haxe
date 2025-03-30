let with_timer timer_ctx level label identifier f =
	let id = Timer.determine_id level ["filters"] [label] identifier in
	Timer.time timer_ctx id f ()