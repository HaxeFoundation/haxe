(* Drives the OCaml native backend to compile Cmm phrases in process.

   The backend keeps global state (Compilenv, Reg, Emitaux, X86_proc): one
   compilation at a time, under [lock]. *)

open Ocaml_backend

let lock = Mutex.create ()

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

(* Compiles the phrases built by [gen] in a new unit. [gen] receives the
   function giving the symbol of a name in the unit. *)
let compile ?(ppf_dump = Format.err_formatter) (gen : (string -> string) -> Cmm.phrase list)
  : compiled =
  Mutex.protect lock (fun () ->
      init ();
      let unit_name = new_unit_name () in
      Compilenv.reset unit_name;
      let symbol name = Compilenv.make_symbol (Some name) in
      let phrases = gen symbol in
      let captured = ref None in
      X86_proc.with_internal_assembler
        (fun program -> captured := Some program; fun _ -> ())
        (fun () ->
           Emit.begin_assembly ();
           List.iter (Asmgen.compile_phrase ~ppf_dump) phrases;
           Emit.end_assembly ());
      match !captured with
      | Some program -> { unit_name; program; symbol }
      | None -> failwith "Jit_backend.compile: no program")

(* Assembles and loads a compiled unit, and registers it with the runtime *)
let load ?gc_roots c =
  let a = X86_encode.assemble c.program in
  let l = Jit_link.load a in
  let sym name = X86_proc.string_of_symbol "" (c.symbol name) in
  Jit_link.register_unit l
    ?gc_roots:(Option.map sym gc_roots)
    ~frametable:(sym "frametable") ~code_begin:(sym "code_begin") ~code_end:(sym "code_end");
  a, l
