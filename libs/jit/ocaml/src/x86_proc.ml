(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open X86_ast

type system =
  | S_macosx
  | S_gnu
  | S_cygwin
  | S_solaris
  | S_beos
  | S_win64
  | S_linux
  | S_mingw64
  | S_freebsd
  | S_netbsd
  | S_openbsd

  | S_unknown


let system = match Config.system with
  | "macosx" -> S_macosx
  | "gnu" -> S_gnu
  | "cygwin" -> S_cygwin
  | "solaris" -> S_solaris
  | "beos" -> S_beos
  | "win64" -> S_win64
  | "linux" -> S_linux
  | "mingw64" -> S_mingw64
  | "freebsd" -> S_freebsd
  | "netbsd" -> S_netbsd
  | "openbsd" -> S_openbsd

  | _ -> S_unknown

let windows =
  match system with
  | S_mingw64 | S_cygwin | S_win64 -> true
  | _ -> false

let string_of_string_literal s =
  let b = Buffer.create (String.length s + 2) in
  let last_was_escape = ref false in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.bprintf b "\\%o" (Char.code c)
      else Buffer.add_char b c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      Buffer.add_char b c;
      last_was_escape := false
    end else begin
      Printf.bprintf b "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  Buffer.contents b

let string_of_symbol prefix s =
  let is_special_char = function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> false
    | c -> c <> Compilenv.symbol_separator
  in
  let spec = String.exists is_special_char s in
  if not spec then if prefix = "" then s else prefix ^ s
  else
    let b = Buffer.create (String.length s + 10) in
    Buffer.add_string b prefix;
    String.iter
      (fun c ->
       (* FIXME: using $ to prefix escaped characters can make names
          ambiguous if the symbol separator is also set to $; a different
          escape prefix should be used in this case, if this ever causes
          problems in the real world. *)
       if is_special_char c then
         Printf.bprintf b "$%02x" (Char.code c)
       else
         Buffer.add_char b c
      )
      s;
    Buffer.contents b

let buf_bytes_directive b directive s =
  let pos = ref 0 in
  for i = 0 to String.length s - 1 do
    if !pos = 0
    then begin
      if i > 0 then Buffer.add_char b '\n';
      Buffer.add_char b '\t';
      Buffer.add_string b directive;
      Buffer.add_char b '\t';
    end
    else Buffer.add_char b ',';
    Printf.bprintf b "%d" (Char.code s.[i]);
    incr pos;
    if !pos >= 16 then begin pos := 0 end
  done

let string_of_reg64 = function
  | RAX -> "rax"
  | RBX -> "rbx"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | RBP -> "rbp"
  | RSP -> "rsp"
  | R8  -> "r8"
  | R9  -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let string_of_reg8l = function
  | RAX -> "al"
  | RBX -> "bl"
  | RCX -> "cl"
  | RDX -> "dl"
  | RSP -> "spl"
  | RBP -> "bpl"
  | RSI -> "sil"
  | RDI -> "dil"
  | R8  -> "r8b"
  | R9  -> "r9b"
  | R10 -> "r10b"
  | R11 -> "r11b"
  | R12 -> "r12b"
  | R13 -> "r13b"
  | R14 -> "r14b"
  | R15 -> "r15b"

let string_of_reg8h = function
  | AH -> "ah"
  | BH -> "bh"
  | CH -> "ch"
  | DH -> "dh"

let string_of_reg16 = function
  | RAX -> "ax"
  | RBX -> "bx"
  | RCX -> "cx"
  | RDX -> "dx"
  | RSP -> "sp"
  | RBP -> "bp"
  | RSI -> "si"
  | RDI -> "di"
  | R8  -> "r8w"
  | R9  -> "r9w"
  | R10 -> "r10w"
  | R11 -> "r11w"
  | R12 -> "r12w"
  | R13 -> "r13w"
  | R14 -> "r14w"
  | R15 -> "r15w"

let string_of_reg32 = function
  | RAX -> "eax"
  | RBX -> "ebx"
  | RCX -> "ecx"
  | RDX -> "edx"
  | RSP -> "esp"
  | RBP -> "ebp"
  | RSI -> "esi"
  | RDI -> "edi"
  | R8  -> "r8d"
  | R9  -> "r9d"
  | R10 -> "r10d"
  | R11 -> "r11d"
  | R12 -> "r12d"
  | R13 -> "r13d"
  | R14 -> "r14d"
  | R15 -> "r15d"

let string_of_registerf = function
  | XMM n -> Printf.sprintf "xmm%d" n
  | TOS -> Printf.sprintf "tos"
  | ST n -> Printf.sprintf "st(%d)" n

let string_of_condition = function
  | E -> "e"
  | AE -> "ae"
  | A -> "a"
  | GE -> "ge"
  | G -> "g"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | L -> "l"
  | LE -> "le"
  | NP -> "np"
  | P -> "p"
  | NS -> "ns"
  | S -> "s"
  | NO -> "no"
  | O -> "o"

let string_of_float_condition = function
  | EQf -> "eq"
  | LTf -> "lt"
  | LEf -> "le"
  | UNORDf -> "unord"
  | NEQf -> "neq"
  | NLTf -> "nlt"
  | NLEf -> "nle"
  | ORDf -> "ord"

let string_of_rounding = function
  | RoundDown -> "roundsd.down"
  | RoundUp -> "roundsd.up"
  | RoundTruncate -> "roundsd.trunc"
  | RoundNearest -> "roundsd.near"

let internal_assembler = ref None
let register_internal_assembler f = internal_assembler := Some f
let with_internal_assembler assemble k =
  Misc.protect_refs [ R (internal_assembler, Some assemble) ] k

(* Which asm conventions to use *)
let masm =
  match system with
  | S_win64 -> true
  | _ -> false

let use_plt =
  match system with
  | S_macosx | S_mingw64 | S_cygwin | S_win64 -> false
  | _ -> !Clflags.dlcode

(* Shall we use an external assembler command ?
   If [binary_content] contains some data, we can directly
   save it. Otherwise, we have to ask an external command.
*)
let binary_content = ref None

let compile infile outfile =
  if masm then
    Ccomp.command (Config.asm ^
                   Filename.quote outfile ^ " " ^ Filename.quote infile ^
                   (if !Clflags.verbose then "" else ">NUL"))
  else
    Ccomp.command (Config.asm ^ " " ^
                   (String.concat " " (Misc.debug_prefix_map_flags ())) ^
                   " -o " ^ Filename.quote outfile ^ " " ^
                   Filename.quote infile)

let assemble_file infile outfile =
  match !binary_content with
  | None -> compile infile outfile
  | Some content -> content outfile; binary_content := None; 0

let asm_code = ref []

let directive dir = asm_code := dir :: !asm_code
let emit ins = directive (Ins ins)

let reset_asm_code () = asm_code := []

let generate_code asm =
  let instrs = List.rev !asm_code in
  begin match asm with
  | Some f -> f instrs
  | None -> ()
  end;
  begin match !internal_assembler with
  | Some f -> binary_content := Some (f instrs)
  | None -> binary_content := None
  end
