(* The native tier of eval is not available with this build (see
   docs/macro-jit-native.md): functions are compiled to closures. *)

let jit_tfunction = EvalJit.jit_tfunction
