(* Cost of compiling a small unit in a loop (backend, encoder, loader), to
   find the fixed costs per unit. Usage: bench_compile.exe [n] *)

open Jit
open Ocaml_backend
open Cmm

let dbg = Debuginfo.none

let program sym =
  let v = Backend_var.create_local "n" in
  let e = Backend_var.create_local "env" in
  let wp = Backend_var.With_provenance.create in
  [Cfunction {
      fun_name = sym "f";
      fun_args = [wp v, typ_val; wp e, typ_val];
      fun_body = Cifthenelse (Cop (Ccmpi Clt, [Cvar v; Cconst_int (5, dbg)], dbg), dbg,
                              Cvar v, dbg,
                              Cop (Caddi, [Cvar v; Cconst_int (2, dbg)], dbg), dbg);
      fun_codegen_options = [];
      fun_poll = Lambda.Default_poll;
      fun_dbg = dbg;
    }]

(* A function shaped like the ones of EvalNative: [calls] calls of OCaml
   closures loaded from a table, on values read from the environment *)
let calls_program calls sym =
  let env = Backend_var.create_local "env" and clos = Backend_var.create_local "clos" in
  let wp = Backend_var.With_provenance.create in
  let load e i = Cmm_helpers.get_field_gen Asttypes.Mutable e i dbg in
  let table = Cconst_symbol (sym "consts", dbg) in
  let body = List.init calls (fun i ->
      Cmm_helpers.generic_apply Asttypes.Mutable (load table i)
        [load (load (Cvar env) 4) (i mod 8)] dbg) in
  let body = List.fold_left (fun acc c -> Csequence (acc, c)) (Cconst_int (1, dbg)) body in
  [Cdata ([Cint (Cmm_helpers.black_block_header 0 calls); Cdefine_symbol (sym "consts")]
          @ List.init calls (fun _ -> Cint 1n));
   Cfunction {
     fun_name = sym "f";
     fun_args = [wp env, typ_val; wp clos, typ_val];
     fun_body = body;
     fun_codegen_options = (if Sys.getenv_opt "NO_CSE" <> None then [No_CSE] else []);
     fun_poll = Lambda.Default_poll;
     fun_dbg = dbg;
   }]

let measure name n program =
  Gc.full_major ();
  let w0 = Gc.minor_words () and t0 = Unix.gettimeofday () in
  for _ = 1 to n do ignore (Jit_backend.compile program) done;
  let t1 = Unix.gettimeofday () and w1 = Gc.minor_words () in
  Printf.printf "%-10s backend: %7.1f us, %6.0f KB allocated per unit\n" name
    ((t1 -. t0) *. 1e6 /. float n) ((w1 -. w0) *. 8. /. 1024. /. float n)

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 1000 in
  if Sys.getenv_opt "COLORING" <> None then begin
    Jit_backend.init (); Clflags.use_linscan := false
  end;
  if Sys.getenv_opt "PROFILE" <> None then begin
    Clflags.profile_columns := [`Time; `Alloc];
    measure "50 calls" n (calls_program 50);
    Profile.print Format.std_formatter !Clflags.profile_columns;
    exit 0
  end;
  measure "tiny" n program;
  List.iter (fun k -> measure (Printf.sprintf "%d calls" k) n (calls_program k)) [5; 20; 50; 100]
