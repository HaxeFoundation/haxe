(*
	The lock of the typing operations that change state shared by the domains
	typing field bodies in parallel (docs/incremental-compilation.md, phase 3.5):
	loading modules, building types, calling macros, creating generic
	instances, reporting messages... It is only taken while bodies are typed in
	parallel ([active]); typing on one domain does not pay for it.
*)

let active = ref false

let lock = ReentrantLock.create ()

(* [site]: what the lock is taken for, reported by --times *)
let protect ?site f = if !active then ReentrantLock.protect ?site lock f else f ()

let notify () = if !active then ReentrantLock.notify lock

let wait_until cond = if !active then ReentrantLock.wait_until lock cond

(* Runs [f] without the lock, taking it back as it was afterwards. *)
let without f =
	if !active then begin
		let n = ReentrantLock.release_all lock in
		match f () with
		| v -> ReentrantLock.reacquire lock n; v
		| exception e -> ReentrantLock.reacquire lock n; raise e
	end else
		f ()

(* A lock for the shared tables that typing only adds to (messages, features,
   caches, flags): taken for short operations that never take a lock again, so
   that they do not wait for the typing lock. *)
let leaf = Mutex.create ()

let protect_leaf f = if !active then Mutex.protect leaf f else f ()
