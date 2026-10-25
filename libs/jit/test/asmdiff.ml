(* Differential test of [X86_encode] against GNU as.

   An ocamlopt driver whose assembler step, for every unit it compiles,
   - prints the program as GNU as text and assembles it with the system
     assembler: the object file is the one the compilation goes on with;
   - assembles the same program with [X86_encode] and writes the result next
     to the object file ([<obj>.jit]), for [asmdiff.py] to compare.

   Usage: asmdiff.exe <ocamlopt arguments> *)

open Jit

let dump_jit file (a : X86_encode.assembled) =
  let oc = open_out_bin file in
  let hex b = String.concat "" (List.init (Bytes.length b) (fun i ->
      Printf.sprintf "%02x" (Char.code (Bytes.get b i)))) in
  Printf.fprintf oc "TEXT %s\nDATA %s\n" (hex a.text.s_bytes) (hex a.data.s_bytes);
  Hashtbl.iter (fun s (k, off) ->
      Printf.fprintf oc "SYM %s %s %d\n" s (if k = X86_encode.Text then "T" else "D") off)
    a.symbols;
  let relocs k (sec : X86_encode.section) =
    List.iter (fun (r : X86_encode.reloc) ->
        let size = match r.r_kind with Abs n -> n | _ -> 4 in
        Printf.fprintf oc "RELOC %s %d %d\n" k r.r_offset size)
      sec.s_relocs
  in
  relocs "T" a.text; relocs "D" a.data;
  List.iter (fun (off, len) -> Printf.fprintf oc "PAD %d %d\n" off len) a.padding;
  close_out oc

let () =
  X86_proc.register_internal_assembler (fun program outfile ->
      let s = Filename.remove_extension outfile ^ ".ref.s" in
      let oc = open_out s in
      X86_gas.generate_asm oc program;
      close_out oc;
      let cmd = Printf.sprintf "as -o %s %s" (Filename.quote outfile) (Filename.quote s) in
      if Sys.command cmd <> 0 then failwith ("assembler failed: " ^ cmd);
      Sys.remove s;
      let jit = outfile ^ ".jit" in
      (* the program of compiler-libs, for the vendored backend of the JIT:
         same type definitions, from the same sources *)
      let program : Ocaml_backend.X86_ast.asm_program = Obj.magic program in
      let t0 = Unix.gettimeofday () in
      match X86_encode.assemble program with
      | a ->
        let t = Unix.gettimeofday () -. t0 in
        dump_jit jit a;
        let oc = open_out_gen [Open_append] 0o644 jit in
        Printf.fprintf oc "TIME %f\n" t;
        close_out oc
      | exception X86_encode.Unsupported msg ->
        let oc = open_out jit in
        Printf.fprintf oc "UNSUPPORTED %s\n" msg;
        close_out oc);
  Emitaux.binary_backend_available := true;
  exit (Optmaindriver.main Sys.argv Format.err_formatter)
