let do_not_check_modules = ref false

let max_completion_items = ref 0

(* Maximum age in seconds for unused context caches before they are removed.
   10 minutes is long enough to survive bursts of display requests with
   varying defines, while still cleaning up contexts that are truly abandoned. *)
let stale_context_max_age_seconds = ref 600

let reset () =
	do_not_check_modules := false;
	max_completion_items := 0;
	stale_context_max_age_seconds := 600
