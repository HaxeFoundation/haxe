(*
	The lock of the typing operations that change state shared by the domains
	typing field bodies in parallel (docs/incremental-compilation.md, phase 3.5):
	loading modules, building types, calling macros, creating generic
	instances, reporting messages... It is only taken while bodies are typed in
	parallel ([active]); typing on one domain does not pay for it.
*)

let active = ref false

let lock = ReentrantLock.create ()

let protect f = if !active then ReentrantLock.protect lock f else f ()

let notify () = if !active then ReentrantLock.notify lock

let wait_until cond = if !active then ReentrantLock.wait_until lock cond
