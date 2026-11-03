(* This module shadows Stdlib.Lazy to prevent accidental use of OCaml's non-thread-safe
   lazy values. Use AtomicLazy instead. *)

type 'a t = 'a AtomicLazy.t

let force = AtomicLazy.force
let from_fun = AtomicLazy.from_fun
let from_val = AtomicLazy.from_val
let is_val = AtomicLazy.is_val
