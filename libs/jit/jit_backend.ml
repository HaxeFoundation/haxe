(* Drives the OCaml native backend to compile Cmm phrases in process.

   The backend keeps global state (Compilenv, Reg, Emitaux, X86_proc, the
   counters of Cmm labels and variables): one compilation at a time, under
   [lock]. *)

open Ocaml_backend

let lock = Mutex.create ()

(* Whether this domain is compiling: a compilation started while building
   the phrases of another one (on the same domain) cannot take the lock. *)
let compiling = Domain.DLS.new_key (fun () -> false)

exception Busy

(* Time spent in each stage, printed at exit when HAXE_JIT_STATS is set *)
let stats = Hashtbl.create 8
let stats_enabled = Sys.getenv_opt "HAXE_JIT_STATS" <> None

let timed name f =
  if not stats_enabled then f ()
  else begin
    let t0 = Unix.gettimeofday () and w0 = Gc.minor_words () in
    Fun.protect f ~finally:(fun () ->
        let t, n, w = try Hashtbl.find stats name with Not_found -> 0., 0, 0. in
        Hashtbl.replace stats name
          (t +. Unix.gettimeofday () -. t0, n + 1, w +. Gc.minor_words () -. w0))
  end

(* Backend time of each unit *)
let unit_log = ref []

let () =
  if stats_enabled then
    at_exit (fun () ->
        let l = List.sort compare !unit_log in
        let n = List.length l in
        let total = List.fold_left (fun acc (t, _) -> acc +. t) 0. l in
        if n > 0 then begin
          let pct p = fst (List.nth l (min (n - 1) (n * p / 100))) *. 1e6 in
          Printf.eprintf "jit units: %d, median %.0f us, p90 %.0f us, p99 %.0f us, max %.0f us\n"
            n (pct 50) (pct 90) (pct 99) (fst (List.nth l (n - 1)) *. 1e6);
          let top = List.filteri (fun i _ -> i >= n - 20) l in
          Printf.eprintf "jit share of the 1%% slowest units: %.0f%%\n"
            (100. *. List.fold_left (fun acc (t, _) -> acc +. t) 0.
               (List.filteri (fun i _ -> i >= n - n / 100) l) /. total);
          ignore top
        end;
        Hashtbl.iter (fun name (t, n, w) ->
            Printf.eprintf "jit %-12s %4d calls %8.3f s %8.1f MB allocated\n" name n t (w *. 8. /. 1e6))
          stats)

let initialized = ref false

let init () =
  if not !initialized then begin
    initialized := true;
    Clflags.native_code := true;
    Clflags.dlcode := true;
    Clflags.pic_code := true;
    Clflags.use_linscan := true;
    (* no text output: the program is captured by the internal assembler *)
    Emitaux.create_asm_file := false
  end

let unit_counter = ref 0

(* A fresh unit name *)
let new_unit_name () =
  incr unit_counter;
  Printf.sprintf "HxJit%d" !unit_counter

type compiled = {
  unit_name : string;
  program : X86_ast.asm_program;
  symbol : string -> string;   (* symbol of a name of the unit *)
}

(* Compiles the phrases built by [gen] in a new unit, with the application
   functions ([caml_applyN]) they need. [gen] receives the function giving
   the symbol of a name in the unit, and runs under the lock: it can use the
   global state of the backend (labels, variables, [Cmm_helpers]).
   Raises [Busy] if this domain is already compiling. *)
let compile ?(ppf_dump = Format.err_formatter) (gen : (string -> string) -> Cmm.phrase list)
  : compiled =
  if Domain.DLS.get compiling then raise Busy;
  Mutex.protect lock (fun () ->
      Domain.DLS.set compiling true;
      Fun.protect ~finally:(fun () -> Domain.DLS.set compiling false) (fun () ->
          init ();
          let unit_name = new_unit_name () in
          Compilenv.reset unit_name;
          let symbol name = Compilenv.make_symbol (Some name) in
          let phrases = timed "translate" (fun () -> gen symbol) in
          let phrases =
            phrases @ Cmm_helpers.generic_functions true [Compilenv.current_unit_infos ()]
          in
          let captured = ref None in
          let t0 = Unix.gettimeofday () in
          Fun.protect ~finally:(fun () ->
              if stats_enabled then begin
                let t = Unix.gettimeofday () -. t0 in
                unit_log := (t, unit_name) :: !unit_log;
                (* the Cmm of the slow units, to see why *)
                match Sys.getenv_opt "HAXE_JIT_DUMP_SLOW" with
                | Some dir when t > 0.005 ->
                  Out_channel.with_open_text (Filename.concat dir (Printf.sprintf "%06d_%s.cmm" (int_of_float (t *. 1e6)) unit_name))
                    (fun oc ->
                       let ppf = Format.formatter_of_out_channel oc in
                       List.iter (Format.fprintf ppf "%a@." Printcmm.phrase) phrases)
                | _ -> ()
              end) @@ fun () ->
          timed "backend" (fun () ->
              X86_proc.with_internal_assembler
                (fun program -> captured := Some program; fun _ -> ())
                (fun () ->
                   Emit.begin_assembly ();
                   List.iter (Asmgen.compile_phrase ~ppf_dump) phrases;
                   Emit.end_assembly ()));
          match !captured with
          | Some program -> { unit_name; program; symbol }
          | None -> failwith "Jit_backend.compile: no program"))

(* Assembles and loads a compiled unit, and registers it with the runtime *)
let load ?gc_roots c =
  let a = timed "assemble" (fun () -> X86_encode.assemble c.program) in
  if stats_enabled then begin
    let t, n, w = try Hashtbl.find stats "code bytes" with Not_found -> 0., 0, 0. in
    Hashtbl.replace stats "code bytes" (t, n + 1, w +. float (Bytes.length a.text.s_bytes) /. 8.)
  end;
  let l = timed "link" (fun () -> Jit_link.load a) in
  let sym name = X86_proc.string_of_symbol "" (c.symbol name) in
  timed "register" (fun () ->
      Jit_link.register_unit l
        ?gc_roots:(Option.map sym gc_roots)
        ~frametable:(sym "frametable") ~code_begin:(sym "code_begin") ~code_end:(sym "code_end"));
  a, l

(* The OCaml value at the symbol of [name] in a loaded unit: a block emitted
   statically (closure, table of constants) *)
let value c (l : Jit_link.loaded) name =
  Jit_link.value_at (l.lookup (X86_proc.string_of_symbol "" (c.symbol name)))
