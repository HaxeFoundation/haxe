(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common functions for emitting assembly code *)

type error =
  | Stack_frame_too_large of int

exception Error of error

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_int n = output_string !output_channel (Int.to_string n)

let emit_char c = output_char !output_channel c

let emit_nativeint n = output_string !output_channel (Nativeint.to_string n)

let emit_printf fmt =
  Printf.fprintf !output_channel fmt

let emit_int32 n = emit_printf "0x%lx" n

let emit_symbol s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        output_char !output_channel c
    | _ ->
      if c = Compilenv.symbol_separator then
        output_char !output_channel c
      else
        Printf.fprintf !output_channel "$%02x" (Char.code c)
  done

let emit_string_literal s =
  let last_was_escape = ref false in
  emit_string "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.fprintf !output_channel "\\%o" (Char.code c)
      else output_char !output_channel c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      output_char !output_channel c;
      last_was_escape := false
    end else begin
      Printf.fprintf !output_channel "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  emit_string "\""

let emit_string_directive directive s =
  let l = String.length s in
  if l = 0 then ()
  else if l < 80 then begin
    emit_string directive;
    emit_string_literal s;
    emit_char '\n'
  end else begin
    let i = ref 0 in
    while !i < l do
      let n = Int.min (l - !i) 80 in
      emit_string directive;
      emit_string_literal (String.sub s !i n);
      emit_char '\n';
      i := !i + n
    done
  end

let emit_bytes_directive directive s =
   let pos = ref 0 in
   for i = 0 to String.length s - 1 do
     if !pos = 0
     then emit_string directive
     else emit_char ',';
     emit_int(Char.code s.[i]);
     incr pos;
     if !pos >= 16 then begin emit_char '\n'; pos := 0 end
   done;
   if !pos > 0 then emit_char '\n'

let emit_float64_directive directive x =
  emit_printf "\t%s\t0x%Lx\n" directive x

let emit_float64_split_directive directive x =
  let lo = Int64.logand x 0xFFFF_FFFFL
  and hi = Int64.shift_right_logical x 32 in
  emit_printf "\t%s\t0x%Lx, 0x%Lx\n"
    directive
    (if Arch.big_endian then hi else lo)
    (if Arch.big_endian then lo else hi)

let emit_float32_directive directive x =
  emit_printf "\t%s\t0x%lx\n" directive x

(* Record live pointers at call points *)

type frame_debuginfo =
  | Dbg_alloc of Debuginfo.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list;           (* Offsets/regs of live addresses *)
    fd_debuginfo: frame_debuginfo }     (* Location, if any *)

let frame_descriptors = ref([] : frame_descr list)

let record_frame_descr ~label ~frame_size ~live_offset debuginfo =
  frame_descriptors :=
    { fd_lbl = label;
      fd_frame_size = frame_size;
      fd_live_offset = List.sort_uniq (-) live_offset;
      fd_debuginfo = debuginfo } :: !frame_descriptors

type emit_frame_actions =
  { efa_code_label: int -> unit;
    efa_data_label: int -> unit;
    efa_8: int -> unit;
    efa_16: int -> unit;
    efa_32: int32 -> unit;
    efa_word: int -> unit;
    efa_align: int -> unit;
    efa_label_rel: int -> int32 -> unit;
    efa_def_label: int -> unit;
    efa_string: string -> unit }

let emit_frames a =
  let filenames = Hashtbl.create 7 in
  let label_filename name =
    try
      Hashtbl.find filenames name
    with Not_found ->
      let lbl = Cmm.new_label () in
      Hashtbl.add filenames name lbl;
      lbl
  in
  let defnames = Hashtbl.create 7 in
  let label_defname filename defname loc =
    try
      snd (Hashtbl.find defnames (filename, defname, loc))
    with Not_found ->
      let file_lbl = label_filename filename in
      let def_lbl = Cmm.new_label () in
      Hashtbl.add defnames (filename, defname, loc) (file_lbl, def_lbl);
      def_lbl
  in
  let module Label_table =
    Hashtbl.Make (struct
      type t = bool * Debuginfo.t

      let equal ((rs1 : bool), dbg1) (rs2, dbg2) =
        rs1 = rs2 && Debuginfo.compare dbg1 dbg2 = 0

      let hash (rs, dbg) =
        Hashtbl.hash (rs, Debuginfo.hash dbg)
    end)
  in
  let debuginfos = Label_table.create 7 in
  let label_debuginfos rs dbg =
    let rdbg = List.rev dbg in
    let key = (rs, rdbg) in
    try Label_table.find debuginfos key
    with Not_found ->
      let lbl = Cmm.new_label () in
      Label_table.add debuginfos key lbl;
      lbl
  in
  let efa_16_checked n =
    assert (n >= 0);
    if n < 0x1_0000
    then a.efa_16 n
    else raise (Error(Stack_frame_too_large n))
  in
  let emit_frame fd =
    assert (fd.fd_frame_size land 3 = 0);
    let flags =
      match fd.fd_debuginfo with
      | Dbg_other d | Dbg_raise d ->
        if Debuginfo.is_none d then 0 else 1
      | Dbg_alloc dbgs ->
        if !Clflags.debug &&
           List.exists (fun d ->
             not (Debuginfo.is_none d.Debuginfo.alloc_dbg)) dbgs
        then 3 else 2
    in
    a.efa_code_label fd.fd_lbl;
    efa_16_checked (fd.fd_frame_size + flags);
    efa_16_checked (List.length fd.fd_live_offset);
    List.iter efa_16_checked fd.fd_live_offset;
    begin match fd.fd_debuginfo with
    | _ when flags = 0 ->
      ()
    | Dbg_other dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos false dbg) Int32.zero
    | Dbg_raise dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos true dbg) Int32.zero
    | Dbg_alloc dbg ->
      assert (List.length dbg < 256);
      a.efa_8 (List.length dbg);
      List.iter (fun Debuginfo.{alloc_words;_} ->
        (* Possible allocations range between 2 and 257 *)
        assert (2 <= alloc_words &&
                alloc_words - 1 <= Config.max_young_wosize &&
                Config.max_young_wosize <= 256);
        a.efa_8 (alloc_words - 2)) dbg;
      if flags = 3 then begin
        a.efa_align 4;
        List.iter (fun Debuginfo.{alloc_dbg; _} ->
          if Debuginfo.is_none alloc_dbg then
            a.efa_32 Int32.zero
          else
            a.efa_label_rel (label_debuginfos false alloc_dbg) Int32.zero) dbg
      end
    end;
    a.efa_align Arch.size_addr
  in
  let emit_filename name lbl =
    a.efa_def_label lbl;
    a.efa_string name
  in
  let emit_defname (_filename, defname, loc) (file_lbl, lbl) =
    let emit_loc (start_chr, end_chr, end_offset) =
      a.efa_16 start_chr;
      a.efa_16 end_chr;
      a.efa_32 (Int32.of_int end_offset)
    in
    (* These must be 32-bit aligned, both because they contain a
       32-bit value, and because emit_debuginfo assumes the low 2 bits
       of their addresses are 0. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    a.efa_label_rel file_lbl 0l;
    (* Include the additional 64-bits of location information which didn't pack
       in the main 64-bit word *)
    Option.iter emit_loc loc;
    a.efa_string defname
  in
  let fully_pack_info fd_raise d has_next =
    (* See format in caml_debuginfo_location in runtime/backtrace-nat.c *)
    let open Debuginfo in
    let kind = if fd_raise then 1 else 0
    and has_next = if has_next then 1 else 0
    and char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
    let char_end_offset = d.dinfo_end_bol - d.dinfo_start_bol in
    Int64.(add (shift_left (of_int d.dinfo_line) 51)
             (add (shift_left (of_int (d.dinfo_end_line - d.dinfo_line)) 48)
                (add (shift_left (of_int d.dinfo_char_start) 42)
                   (add (shift_left (of_int char_end) 35)
                      (add (shift_left (of_int char_end_offset) 26)
                         (add (shift_left (of_int kind) 1)
                            (of_int has_next)))))))
  in
  let partially_pack_info fd_raise d has_next =
    (* Partially packed debuginfo:
       1lllllllllmmmmmmmmddddddddddddkn
         1           - d points to a name_and_loc_info struct
         l (19 bits) - start line number
         m (18 bits) - offset of end line number from start
         d (24 bits) - memory offset to name_and_loc_info struct
         k (1 bit)   - fd_raise flag
         n (1 bit)   - has_next flag *)
    let open Debuginfo in
    let start_line = Int.min 0x7FFFF d.dinfo_line
    and end_line = Int.min 0x3FFFF (d.dinfo_end_line - d.dinfo_line)
    and kind = if fd_raise then 1 else 0
    and has_next = if has_next then 1 else 0 in
    Int64.(add (shift_left Int64.one 63)
             (add (shift_left (of_int start_line) 44)
                (add (shift_left (of_int end_line) 26)
                   (add (shift_left (of_int kind) 1)
                      (of_int has_next)))))
  in
  let emit_debuginfo (rs, rdbg) lbl =
    (* Due to inlined functions, a single debuginfo may have multiple locations.
       These are represented sequentially in memory (innermost frame first),
       with the low bit of the packed debuginfo being 0 on the last entry. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    let rec emit rs d rest =
      let open Debuginfo in
      let defname = Scoped_location.string_of_scopes d.dinfo_scopes in
      let char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
      let is_fully_packable =
        d.dinfo_line <= 0xFFF
        && d.dinfo_end_line - d.dinfo_line <= 0x7
        && d.dinfo_char_start <= 0x3F
        && char_end <= 0x7F
        && d.dinfo_end_bol - d.dinfo_start_bol <= 0x1FF
      in
      let info =
        if is_fully_packable then
          fully_pack_info rs d (rest <> [])
        else
          partially_pack_info rs d (rest <> [])
      in
      let loc =
        if is_fully_packable then
          None
        else
          Some (Int.min 0xFFFF d.dinfo_char_start,   (* start_chr *)
                Int.min 0xFFFF char_end,             (* end_chr *)
                Int.min 0x3FFFFFFF d.dinfo_char_end) (* end_offset *)
      in
      a.efa_label_rel
        (label_defname d.dinfo_file defname loc)
        (Int64.to_int32 info);
      a.efa_32 (Int64.to_int32 (Int64.shift_right info 32));
      match rest with
      | [] -> ()
      | d :: rest -> emit false d rest in
    match rdbg with
    | [] -> assert false
    | d :: rest -> emit rs d rest in
  a.efa_word (List.length !frame_descriptors);
  List.iter emit_frame !frame_descriptors;
  Label_table.iter emit_debuginfo debuginfos;
  Hashtbl.iter emit_filename filenames;
  Hashtbl.iter emit_defname defnames;
  a.efa_align Arch.size_addr;
  frame_descriptors := []

(* Detection of functions that can be duplicated between a DLL and
   the main program (PR#4690) *)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

let is_generic_function name =
  List.exists
    (fun p -> isprefix p name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

(* CFI directives *)

let is_cfi_enabled () =
  Config.asm_cfi_supported

let cfi_startproc () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_startproc\n"

let cfi_endproc () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_endproc\n"

let cfi_remember_state () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_remember_state\n"

let cfi_restore_state () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_restore_state\n"

let cfi_adjust_cfa_offset n =
  if is_cfi_enabled () then
  begin
    emit_string "\t.cfi_adjust_cfa_offset\t"; emit_int n; emit_string "\n";
  end

let cfi_def_cfa_offset n =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_def_cfa_offset\t"; emit_int n; emit_string "\n";
  end

let cfi_offset ~reg ~offset =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_offset ";
    emit_int reg;
    emit_string ", ";
    emit_int offset;
    emit_string "\n"
  end

let cfi_def_cfa_register ~reg =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_def_cfa_register ";
    emit_int reg;
    emit_string "\n"
  end

(* Emit debug information *)

(* This assoc list is expected to be very short *)
let file_pos_nums =
  (ref [] : (string * int) list ref)

(* Number of files *)
let file_pos_num_cnt = ref 1

(* Reset debug state at beginning of asm file *)
let reset_debug_info () =
  file_pos_nums := [];
  file_pos_num_cnt := 1

(* We only display .file if the file has not been seen before. We
   display .loc for every instruction. *)
let emit_debug_info_gen dbg file_emitter loc_emitter =
  if is_cfi_enabled () &&
    (!Clflags.debug || Config.with_frame_pointers) then begin
    match List.rev dbg with
    | [] -> ()
    | { Debuginfo.dinfo_line = line;
        dinfo_char_start = col;
        dinfo_file = file_name; } :: _ ->
      if line > 0 then begin (* PR#6243 *)
        let file_num =
          try List.assoc file_name !file_pos_nums
          with Not_found ->
            let file_num = !file_pos_num_cnt in
            incr file_pos_num_cnt;
            file_emitter ~file_num ~file_name;
            file_pos_nums := (file_name,file_num) :: !file_pos_nums;
            file_num in
        loc_emitter ~file_num ~line ~col;
      end
  end

let emit_debug_info dbg =
  emit_debug_info_gen dbg (fun ~file_num ~file_name ->
      emit_string "\t.file\t";
      emit_int file_num; emit_char '\t';
      emit_string_literal file_name; emit_char '\n';
    )
    (fun ~file_num ~line ~col:_ ->
       emit_string "\t.loc\t";
       emit_int file_num; emit_char '\t';
       emit_int line; emit_char '\n')

let reset () =
  reset_debug_info ();
  frame_descriptors := []

let binary_backend_available = ref false
let create_asm_file = ref true

let report_error_doc ppf = function
  | Stack_frame_too_large n ->
      Format_doc.fprintf ppf "stack frame too large (%d bytes)" n

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc

let mk_env f : Emitenv.per_function_env =
  {
    f;
    stack_offset = 0;
    call_gc_sites = [];
    bound_error_sites = [];
    bound_error_call = None;
    call_gc_label = 0;
    jumptables_lbl = None;
    jumptables = [];
    float_literals = [];
    int_literals = [];
  }

let emit_named_text_section func_name prefix_char =
  if !Clflags.function_sections then begin
    emit_string "\t.section .text.caml.";
    emit_symbol func_name;
    emit_char ',';
    emit_string_literal "ax";
    emit_char ',';
    emit_char prefix_char;
    emit_string "progbits\n";
  end
  else
    emit_string "\t.text\n"
