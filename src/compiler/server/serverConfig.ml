let do_not_check_modules = ref false

let max_completion_items = ref 0

(* Maximum age in seconds for unused context caches before they are removed.
   10 minutes is long enough to survive bursts of display requests with
   varying defines, while still cleaning up contexts that are truly abandoned. *)
let default_stale_context_max_age = 600
let stale_context_max_age_seconds = ref default_stale_context_max_age

let reset () =
	do_not_check_modules := false;
	max_completion_items := 0;
	stale_context_max_age_seconds := default_stale_context_max_age
