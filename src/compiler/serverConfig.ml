let do_not_check_modules = ref false
let legacy_completion = ref false

let max_completion_items = ref 0

let reset () =
	do_not_check_modules := false;
	legacy_completion := false;
	max_completion_items := 0
