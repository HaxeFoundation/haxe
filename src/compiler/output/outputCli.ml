open CompilerOutput

(** CLI output handler for non-server compilation.

    Routes output to stdout/stderr directly:
    - [OTimerData]: written to stderr
    - Other kinds: not yet handled (will be added during migration) *)

let create () : output_handler = fun kind ->
	match kind with
	| OTimerData s -> prerr_string s
	| _ -> ()
