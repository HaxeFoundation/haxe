let do_not_check_modules = ref false
let populate_cache_from_display = ref true
let legacy_completion = ref false

let max_completion_items = ref 0

let reset () =
	do_not_check_modules := false;
	populate_cache_from_display := true;
	legacy_completion := false;
	max_completion_items := 0