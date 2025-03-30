let with_timer timer_ctx level label identifier f =
	let id = BetterTimer.determine_id level ["filters"] [label] identifier in
	BetterTimer.time timer_ctx id f ()