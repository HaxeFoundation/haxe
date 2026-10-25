(* Binary encoder for the amd64 output of the OCaml backend.

   The OCaml native backend produces, through
   [X86_proc.register_internal_assembler], the program it would print for the
   system assembler, as a list of [X86_ast] lines. This module turns it into
   machine code and data, leaving the relocations to the linker ([Jit_link]).

   Only the instructions and directives that [asmcomp/amd64/emit.mlp] emits are
   supported; anything else raises [Unsupported], and the caller keeps the
   function interpreted. The encodings are the ones GNU as picks, so that the
   output can be compared with the system assembler in tests (alignment
   padding excepted). *)

open Ocaml_backend

open X86_ast

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

(* Output *)

type reloc_kind =
  | Abs of int   (* absolute value on 1, 2, 4 or 8 bytes ([ConstThis] is the
                    address of the field) *)
  | Pcrel32      (* 32-bit, relative to the end of the instruction *)
  | Plt32        (* call or jump to a symbol, possibly through a PLT stub;
                    relative to the end of the instruction *)
  | Gotpcrel32   (* 32-bit, address of the GOT entry of the symbol, relative to
                    the end of the instruction *)

type reloc = {
  r_offset : int;     (* offset of the field in the section *)
  r_end : int;        (* offset of the end of the instruction (PC-relative) *)
  r_kind : reloc_kind;
  r_expr : constant;
}

type section_kind = Text | Data

type section = {
  s_bytes : Bytes.t;
  s_relocs : reloc list;
}

type assembled = {
  text : section;
  data : section;
  (* symbols and labels defined by the program *)
  symbols : (string, section_kind * int) Hashtbl.t;
  (* text ranges of alignment padding (for comparisons with GNU as) *)
  padding : (int * int) list;
}

(* Registers and conditions *)

let reg_num = function
  | RAX -> 0 | RCX -> 1 | RDX -> 2 | RBX -> 3
  | RSP -> 4 | RBP -> 5 | RSI -> 6 | RDI -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

let cond_code = function
  | O -> 0x0 | NO -> 0x1 | B -> 0x2 | AE -> 0x3
  | E -> 0x4 | NE -> 0x5 | BE -> 0x6 | A -> 0x7
  | S -> 0x8 | NS -> 0x9 | P -> 0xa | NP -> 0xb
  | L -> 0xc | GE -> 0xd | LE -> 0xe | G -> 0xf

let float_cond_code = function
  | EQf -> 0 | LTf -> 1 | LEf -> 2 | UNORDf -> 3
  | NEQf -> 4 | NLTf -> 5 | NLEf -> 6 | ORDf -> 7

let rounding_code = function
  | RoundNearest -> 0 | RoundDown -> 1 | RoundUp -> 2 | RoundTruncate -> 3

let is_imm8 n = n >= -128L && n <= 127L
let is_imm32 n = n >= -0x8000_0000L && n <= 0x7FFF_FFFFL

let strip_suffix s suffix =
  let ls = String.length s and lx = String.length suffix in
  if ls > lx && String.sub s (ls - lx) lx = suffix then Some (String.sub s 0 (ls - lx))
  else None

(* Instruction buffer: the bytes of one instruction and its relocations,
   relative to the start of the instruction *)

type ibuf = {
  b : Buffer.t;
  mutable irelocs : (int * reloc_kind * constant) list;
}

let ib_create () = { b = Buffer.create 16; irelocs = [] }
let byte ib n = Buffer.add_char ib.b (Char.unsafe_chr (n land 0xff))
let le ib n size =
  for i = 0 to size - 1 do
    byte ib (Int64.to_int (Int64.shift_right_logical n (8 * i)))
  done
let add_reloc ib kind expr size =
  ib.irelocs <- (Buffer.length ib.b, kind, expr) :: ib.irelocs;
  for _ = 1 to size do byte ib 0 done

(* Operands *)

type size = S8 | S16 | S32 | S64

let size_of_typ = function
  | BYTE -> Some S8 | WORD -> Some S16 | DWORD | REAL4 -> Some S32
  | QWORD | REAL8 -> Some S64 | NONE | OWORD | NEAR | PROC -> None

let size_of_arg = function
  | Reg8L _ | Reg8H _ -> Some S8
  | Reg16 _ -> Some S16
  | Reg32 _ -> Some S32
  | Reg64 _ -> Some S64
  | Mem { typ; _ } | Mem64_RIP (typ, _, _) -> size_of_typ typ
  | Imm _ | Sym _ | Regf _ -> None

(* An r/m operand *)
type rm =
  | RmReg of int * bool       (* register number; true for spl, bpl, sil, dil,
                                 which need a REX prefix *)
  | RmBase of int * int       (* disp(%base) *)
  | RmSib of int option * int * int * int
                              (* disp(%base, %index, scale) *)
  | RmRip of string * int     (* sym+disp(%rip) *)

let rm_of_arg = function
  | Reg64 r | Reg32 r | Reg16 r -> RmReg (reg_num r, false)
  | Reg8L r ->
    let n = reg_num r in
    RmReg (n, n >= 4 && n < 8)
  | Reg8H r -> RmReg ((match r with AH -> 4 | CH -> 5 | DH -> 6 | BH -> 7), false)
  | Regf (XMM n) -> RmReg (n, false)
  | Mem { arch = X86; _ } -> unsupported "32-bit addressing"
  | Mem { sym = Some _; _ } -> unsupported "absolute symbol addressing"
  | Mem { scale = 0; _ } -> unsupported "absolute addressing"
  | Mem { base = None; scale = 1; idx; displ; _ } -> RmBase (reg_num idx, displ)
  | Mem { base; scale; idx; displ; _ } ->
    RmSib (Option.map reg_num base, reg_num idx, scale, displ)
  | Mem64_RIP (_, s, d) -> RmRip (s, d)
  | Imm _ -> unsupported "immediate as r/m"
  | Sym s -> unsupported "symbol %s as r/m" s
  | Regf _ -> unsupported "x87 register"

let is_reg = function Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ -> true | _ -> false
let reg_of = function
  | Reg8L r | Reg16 r | Reg32 r | Reg64 r -> reg_num r
  | Reg8H r -> (match r with AH -> 4 | CH -> 5 | DH -> 6 | BH -> 7)
  | Regf (XMM n) -> n
  | _ -> unsupported "expected a register"

(* Byte registers spl, bpl, sil and dil need a REX prefix *)
let needs_rex8 = function
  | Reg8L r -> let n = reg_num r in n >= 4 && n < 8
  | _ -> false

(* Prefixes, REX, opcode, ModRM, SIB and displacement of an instruction whose
   ModRM register field is [reg] (a register or an opcode extension). *)
let modrm ib ?(prefix = []) ?(w = false) ?(rex = false) opcode reg rm =
  List.iter (byte ib) prefix;
  let r = reg land 8 <> 0 in
  let x, b, rex8 =
    match rm with
    | RmReg (n, needs) -> false, n land 8 <> 0, needs
    | RmBase (base, _) -> false, base land 8 <> 0, false
    | RmSib (base, idx, _, _) ->
      idx land 8 <> 0, (match base with Some n -> n land 8 <> 0 | None -> false), false
    | RmRip _ -> false, false, false
  in
  if w || r || x || b || rex8 || rex then
    byte ib (0x40 lor (if w then 8 else 0) lor (if r then 4 else 0)
             lor (if x then 2 else 0) lor (if b then 1 else 0));
  List.iter (byte ib) opcode;
  let reg3 = (reg land 7) lsl 3 in
  let disp_mode base3 disp =
    if disp = 0 && base3 <> 5 then 0
    else if disp >= -128 && disp <= 127 then 1
    else 2
  in
  let disp mode d =
    if mode = 1 then byte ib d
    else if mode = 2 then le ib (Int64.of_int d) 4
  in
  match rm with
  | RmReg (n, _) -> byte ib (0xC0 lor reg3 lor (n land 7))
  | RmRip (s, d) ->
    byte ib (0x05 lor reg3);
    (match strip_suffix s "@GOTPCREL" with
     | Some s ->
       if d <> 0 then unsupported "GOTPCREL with an offset";
       add_reloc ib Gotpcrel32 (ConstLabel s) 4
     | None ->
       let e = if d = 0 then ConstLabel s
         else ConstAdd (ConstLabel s, Const (Int64.of_int d)) in
       add_reloc ib Pcrel32 e 4)
  | RmBase (base, d) ->
    let m = disp_mode (base land 7) d in
    byte ib ((m lsl 6) lor reg3 lor (base land 7));
    if base land 7 = 4 then byte ib 0x24;
    disp m d
  | RmSib (base, idx, scale, d) ->
    if idx = 4 then unsupported "%%rsp as an index";
    let ss = match scale with
      | 1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3
      | _ -> unsupported "scale %d" scale
    in
    match base with
    | None ->
      byte ib (0x04 lor reg3);
      byte ib ((ss lsl 6) lor ((idx land 7) lsl 3) lor 5);
      le ib (Int64.of_int d) 4
    | Some base ->
      let m = disp_mode (base land 7) d in
      byte ib ((m lsl 6) lor reg3 lor 4);
      byte ib ((ss lsl 6) lor ((idx land 7) lsl 3) lor (base land 7));
      disp m d

(* Size of an integer instruction: from its register operand if any,
   otherwise from the type of its memory operand. *)
let op_size args =
  let rec find = function
    | [] -> unsupported "operand size"
    | a :: l -> (match size_of_arg a with Some s -> s | None -> find l)
  in
  (* registers first: a memory operand may be untyped *)
  match List.find_opt is_reg args with
  | Some r -> (match size_of_arg r with Some s -> s | None -> find args)
  | None -> find args

let size_prefix = function S16 -> [0x66] | _ -> []
let size_w = function S64 -> true | _ -> false

let imm_le ib n = function
  | S8 -> le ib n 1
  | S16 -> le ib n 2
  | S32 | S64 -> le ib n 4

(* ADD, OR, AND, SUB, XOR, CMP: [ext] is the opcode extension *)
let alu ib ext src dst =
  match src, dst with
  | Imm n, _ ->
    let s = op_size [dst] in
    let rm = rm_of_arg dst in
    if s = S8 then begin
      if (match dst with Reg8L RAX -> true | _ -> false) then begin
        byte ib (ext * 8 + 4); le ib n 1
      end else begin
        modrm ib [0x80] ext rm; le ib n 1
      end
    end else if is_imm8 n then begin
      modrm ib ~prefix:(size_prefix s) ~w:(size_w s) [0x83] ext rm; le ib n 1
    end else begin
      if not (is_imm32 n) then unsupported "64-bit immediate";
      (match rm with
       | RmReg (0, _) ->
         List.iter (byte ib) (size_prefix s);
         if size_w s then byte ib 0x48;
         byte ib (ext * 8 + 5)
       | _ -> modrm ib ~prefix:(size_prefix s) ~w:(size_w s) [0x81] ext rm);
      imm_le ib n s
    end
  | _, _ when is_reg src ->
    let s = op_size [src] in
    let op = ext * 8 + (if s = S8 then 0 else 1) in
    modrm ib ~prefix:(size_prefix s) ~w:(size_w s) ~rex:(needs_rex8 src)
      [op] (reg_of src) (rm_of_arg dst)
  | _, _ when is_reg dst ->
    let s = op_size [dst] in
    let op = ext * 8 + (if s = S8 then 2 else 3) in
    modrm ib ~prefix:(size_prefix s) ~w:(size_w s) ~rex:(needs_rex8 dst)
      [op] (reg_of dst) (rm_of_arg src)
  | _ -> unsupported "ALU operands"

(* Unary group 3 / group 5 style: [op8] [op] /ext *)
let unary ib op8 op ext arg =
  let s = op_size [arg] in
  modrm ib ~prefix:(size_prefix s) ~w:(size_w s)
    [if s = S8 then op8 else op] ext (rm_of_arg arg)

let shift ib ext count dst =
  let s = op_size [dst] in
  let rm = rm_of_arg dst in
  let pre = size_prefix s and w = size_w s in
  match count with
  | Imm 1L -> modrm ib ~prefix:pre ~w [if s = S8 then 0xD0 else 0xD1] ext rm
  | Imm n -> modrm ib ~prefix:pre ~w [if s = S8 then 0xC0 else 0xC1] ext rm; le ib n 1
  | Reg8L RCX -> modrm ib ~prefix:pre ~w [if s = S8 then 0xD2 else 0xD3] ext rm
  | _ -> unsupported "shift count"

(* SSE instruction: [prefix] 0F [op] /r, [dst] being the register *)
let sse ib ?(w = false) prefix op src dst =
  modrm ib ~prefix ~w (0x0F :: op) (reg_of dst) (rm_of_arg src)

let mov ib src dst =
  match src, dst with
  | Imm n, Reg64 r when not (is_imm32 n) ->
    (* movabsq *)
    let n' = reg_num r in
    byte ib (0x48 lor (if n' land 8 <> 0 then 1 else 0));
    byte ib (0xB8 + (n' land 7));
    le ib n 8
  | Imm n, Reg32 r ->
    let n' = reg_num r in
    if n' land 8 <> 0 then byte ib 0x41;
    byte ib (0xB8 + (n' land 7));
    le ib n 4
  | Imm n, _ ->
    let s = op_size [dst] in
    modrm ib ~prefix:(size_prefix s) ~w:(size_w s) [if s = S8 then 0xC6 else 0xC7] 0
      (rm_of_arg dst);
    imm_le ib n s
  | Sym sym, Reg64 r ->
    (* movabsq $sym, %r *)
    let n' = reg_num r in
    byte ib (0x48 lor (if n' land 8 <> 0 then 1 else 0));
    byte ib (0xB8 + (n' land 7));
    add_reloc ib (Abs 8) (ConstLabel sym) 8
  | _, _ when is_reg src ->
    let s = op_size [src] in
    modrm ib ~prefix:(size_prefix s) ~w:(size_w s) ~rex:(needs_rex8 src)
      [if s = S8 then 0x88 else 0x89] (reg_of src) (rm_of_arg dst)
  | _, _ when is_reg dst ->
    let s = op_size [dst] in
    modrm ib ~prefix:(size_prefix s) ~w:(size_w s) ~rex:(needs_rex8 dst)
      [if s = S8 then 0x8A else 0x8B] (reg_of dst) (rm_of_arg src)
  | _ -> unsupported "mov operands"

let push_pop ib is_push arg =
  match arg with
  | Reg64 r ->
    let n = reg_num r in
    if n land 8 <> 0 then byte ib 0x41;
    byte ib ((if is_push then 0x50 else 0x58) + (n land 7))
  | Imm n when is_push ->
    if is_imm8 n then (byte ib 0x6A; le ib n 1) else (byte ib 0x68; le ib n 4)
  | Mem _ | Mem64_RIP _ ->
    if is_push then modrm ib [0xFF] 6 (rm_of_arg arg)
    else modrm ib [0x8F] 0 (rm_of_arg arg)
  | _ -> unsupported "push/pop operand"

(* Instructions other than jumps and calls to labels *)
let encode ib = function
  | ADD (s, d) -> alu ib 0 s d
  | OR (s, d) -> alu ib 1 s d
  | AND (s, d) -> alu ib 4 s d
  | SUB (s, d) -> alu ib 5 s d
  | XOR (s, d) -> alu ib 6 s d
  | CMP (s, d) -> alu ib 7 s d
  | TEST (s, d) ->
    (match s with
     | Imm n ->
       let sz = op_size [d] in
       (match rm_of_arg d with
        | RmReg (0, false) ->
          List.iter (byte ib) (size_prefix sz);
          if size_w sz then byte ib 0x48;
          byte ib (if sz = S8 then 0xA8 else 0xA9)
        | rm ->
          modrm ib ~prefix:(size_prefix sz) ~w:(size_w sz)
            [if sz = S8 then 0xF6 else 0xF7] 0 rm);
       imm_le ib n sz
     | _ ->
       let reg, other = if is_reg s then s, d else d, s in
       let sz = op_size [reg] in
       modrm ib ~prefix:(size_prefix sz) ~w:(size_w sz)
         ~rex:(needs_rex8 reg || needs_rex8 other)
         [if sz = S8 then 0x84 else 0x85] (reg_of reg) (rm_of_arg other))
  | MOV (s, d) -> mov ib s d
  | MOVZX (s, d) | MOVSX (s, d) as i ->
    let ss = op_size [s] and ds = op_size [d] in
    let base = match i with MOVZX _ -> 0xB6 | _ -> 0xBE in
    let op = if ss = S8 then base else base + 1 in
    modrm ib ~prefix:(size_prefix ds) ~w:(size_w ds) ~rex:(needs_rex8 s)
      [0x0F; op] (reg_of d) (rm_of_arg s)
  | MOVSXD (s, d) -> modrm ib ~w:true [0x63] (reg_of d) (rm_of_arg s)
  | LEA (s, d) ->
    let ds = op_size [d] in
    modrm ib ~prefix:(size_prefix ds) ~w:(size_w ds) [0x8D] (reg_of d) (rm_of_arg s)
  | PUSH a -> push_pop ib true a
  | POP a -> push_pop ib false a
  | CALL (Reg64 _ | Mem _ | Mem64_RIP _ as a) -> modrm ib [0xFF] 2 (rm_of_arg a)
  | JMP (Reg64 _ | Mem _ | Mem64_RIP _ as a) -> modrm ib [0xFF] 4 (rm_of_arg a)
  | RET -> byte ib 0xC3
  | NOP -> byte ib 0x90
  | HLT -> byte ib 0xF4
  | LEAVE -> byte ib 0xC9
  | CQO -> byte ib 0x48; byte ib 0x99
  | CDQ -> byte ib 0x99
  | INC a -> unary ib 0xFE 0xFF 0 a
  | DEC a -> unary ib 0xFE 0xFF 1 a
  | NEG a -> unary ib 0xF6 0xF7 3 a
  | IDIV a -> unary ib 0xF6 0xF7 7 a
  | IMUL (a, None) -> unary ib 0xF6 0xF7 5 a
  | IMUL (Imm n, Some d) ->
    let s = op_size [d] in
    if is_imm8 n then begin
      modrm ib ~prefix:(size_prefix s) ~w:(size_w s) [0x6B] (reg_of d) (rm_of_arg d);
      le ib n 1
    end else begin
      modrm ib ~prefix:(size_prefix s) ~w:(size_w s) [0x69] (reg_of d) (rm_of_arg d);
      imm_le ib n s
    end
  | IMUL (src, Some d) ->
    let s = op_size [d] in
    modrm ib ~prefix:(size_prefix s) ~w:(size_w s) [0x0F; 0xAF] (reg_of d) (rm_of_arg src)
  | SAL (c, d) -> shift ib 4 c d
  | SHR (c, d) -> shift ib 5 c d
  | SAR (c, d) -> shift ib 7 c d
  | SET (c, d) ->
    modrm ib ~rex:(needs_rex8 d) [0x0F; 0x90 + cond_code c] 0 (rm_of_arg d)
  | CMOV (c, s, d) ->
    let sz = op_size [d] in
    modrm ib ~prefix:(size_prefix sz) ~w:(size_w sz) [0x0F; 0x40 + cond_code c]
      (reg_of d) (rm_of_arg s)
  | XCHG (a, b) ->
    let sz = op_size [a; b] in
    (match a, b with
     | (Reg64 RAX | Reg32 RAX), (Reg64 r | Reg32 r)
     | (Reg64 r | Reg32 r), (Reg64 RAX | Reg32 RAX) when sz <> S16 ->
       let n = reg_num r in
       if size_w sz || n land 8 <> 0 then
         byte ib (0x40 lor (if size_w sz then 8 else 0) lor (if n land 8 <> 0 then 1 else 0));
       byte ib (0x90 + (n land 7))
     | _ ->
       let reg, other = if is_reg a then a, b else b, a in
       modrm ib ~prefix:(size_prefix sz) ~w:(size_w sz)
         [if sz = S8 then 0x86 else 0x87] (reg_of reg) (rm_of_arg other))
  | BSWAP (Reg64 r | Reg32 r as a) ->
    let n = reg_num r in
    let w = (match a with Reg64 _ -> true | _ -> false) in
    if w || n land 8 <> 0 then
      byte ib (0x40 lor (if w then 8 else 0) lor (if n land 8 <> 0 then 1 else 0));
    byte ib 0x0F; byte ib (0xC8 + (n land 7))
  | ADDSD (s, d) -> sse ib [0xF2] [0x58] s d
  | SUBSD (s, d) -> sse ib [0xF2] [0x5C] s d
  | MULSD (s, d) -> sse ib [0xF2] [0x59] s d
  | DIVSD (s, d) -> sse ib [0xF2] [0x5E] s d
  | SQRTSD (s, d) -> sse ib [0xF2] [0x51] s d
  | ANDPD (s, d) -> sse ib [0x66] [0x54] s d
  | XORPD (s, d) -> sse ib [0x66] [0x57] s d
  | COMISD (s, d) -> sse ib [0x66] [0x2F] s d
  | UCOMISD (s, d) -> sse ib [0x66] [0x2E] s d
  | CMPSD (c, s, d) -> sse ib [0xF2] [0xC2] s d; byte ib (float_cond_code c)
  | CVTSD2SS (s, d) -> sse ib [0xF2] [0x5A] s d
  | CVTSS2SD (s, d) -> sse ib [0xF3] [0x5A] s d
  | CVTSI2SD (s, d) -> sse ib ~w:(op_size [s] = S64) [0xF2] [0x2A] s d
  | CVTTSD2SI (s, d) -> sse ib ~w:(op_size [d] = S64) [0xF2] [0x2C] s d
  | CVTSD2SI (s, d) -> sse ib ~w:(op_size [d] = S64) [0xF2] [0x2D] s d
  | ROUNDSD (r, s, d) -> sse ib [0x66] [0x3A; 0x0B] s d; byte ib (rounding_code r)
  | MOVSD (s, d) | MOVSS (s, d) | MOVAPD (s, d) | MOVLPD (s, d) as i ->
    let prefix, load, store = match i with
      | MOVSD _ -> [0xF2], 0x10, 0x11
      | MOVSS _ -> [0xF3], 0x10, 0x11
      | MOVAPD _ -> [0x66], 0x28, 0x29
      | _ -> [0x66], 0x12, 0x13
    in
    (match d with
     | Regf (XMM _) -> sse ib prefix [load] s d
     | _ -> modrm ib ~prefix [0x0F; store] (reg_of s) (rm_of_arg d))
  | MOVD (s, d) ->
    (match d with
     | Regf (XMM _) -> sse ib ~w:(op_size [s] = S64) [0x66] [0x6E] s d
     | _ -> modrm ib ~prefix:[0x66] ~w:(op_size [d] = S64) [0x0F; 0x7E]
              (reg_of s) (rm_of_arg d))
  | (CALL _ | JMP _ | J _) -> unsupported "jump operand"
  | BSWAP _ -> unsupported "bswap operand"
  | FABS | FADD _ | FADDP _ | FCHS | FCOMP _ | FCOMPP | FCOS | FDIV _ | FDIVP _
  | FDIVR _ | FDIVRP _ | FILD _ | FISTP _ | FLD _ | FLD1 | FLDCW _ | FLDLG2
  | FLDLN2 | FLDZ | FMUL _ | FMULP _ | FNSTCW _ | FNSTSW _ | FPATAN | FPTAN
  | FSIN | FSQRT | FSTP _ | FSUB _ | FSUBP _ | FSUBR _ | FSUBRP _ | FXCH _
  | FYL2X -> unsupported "x87 instruction"

(* Assembly *)

(* An item of a section, before layout *)
type item =
  | Code of string * (int * reloc_kind * constant) list   (* bytes, relocs *)
  | Branch of int option * string * bool ref
    (* condition (None: jmp), target label, long form *)
  | Call of string                                         (* call sym *)
  | Label of string
  | Align of int
  | Datum of int * constant                                (* size, value *)

let rec const_is_constant = function
  | Const _ -> true
  | ConstThis | ConstLabel _ -> false
  | ConstAdd (a, b) | ConstSub (a, b) -> const_is_constant a && const_is_constant b

let rec const_value = function
  | Const n -> n
  | ConstAdd (a, b) -> Int64.add (const_value a) (const_value b)
  | ConstSub (a, b) -> Int64.sub (const_value a) (const_value b)
  | ConstThis | ConstLabel _ -> assert false

let branch_size cond long =
  match cond, long with
  | _, false -> 2
  | None, true -> 5
  | Some _, true -> 6

let item_size pos = function
  | Code (s, _) -> String.length s
  | Branch (c, _, long) -> branch_size c !long
  | Call _ -> 5
  | Label _ -> 0
  | Align n -> (n - pos mod n) mod n
  | Datum (n, _) -> n

(* Multi-byte NOPs, as recommended by the Intel manual *)
let nops = [|
  "";
  "\x90";
  "\x66\x90";
  "\x0f\x1f\x00";
  "\x0f\x1f\x40\x00";
  "\x0f\x1f\x44\x00\x00";
  "\x66\x0f\x1f\x44\x00\x00";
  "\x0f\x1f\x80\x00\x00\x00\x00";
  "\x0f\x1f\x84\x00\x00\x00\x00\x00";
|]

let fill_nops buf n =
  let n = ref n in
  while !n > 0 do
    let k = min !n 8 in
    Buffer.add_string buf nops.(k);
    n := !n - k
  done

let section_of_name = function
  | [] -> unsupported "empty section name"
  | "__TEXT" :: ("__literal8" | "__literal16") :: _ -> Some Data
  | name :: _ ->
    let starts p = String.length name >= String.length p
                   && String.sub name 0 (String.length p) = p in
    if starts ".text" || name = "__TEXT" then Some Text
    else if starts ".data" || starts ".rodata" || name = "__DATA" then Some Data
    else if starts ".note" || starts ".debug" then None
    else unsupported "section %s" name

let assemble (program : asm_program) : assembled =
  (* items of each section, in reverse order; [None]: ignored section *)
  (* text sections are merged; data sections are kept apart, in their order
     of appearance, as the system linker would place them *)
  let text = ref [] in
  let data = Hashtbl.create 4 and data_order = ref [] in
  let current = ref (Some Text) and current_data = ref [] and current_key = ref "" in
  let add item =
    match !current with
    | Some Text -> text := item :: !text
    | Some Data -> current_data := item :: !current_data
    | None -> ()
  in
  let switch_section name =
    (match !current with
     | Some Data -> Hashtbl.replace data !current_key !current_data
     | _ -> ());
    current := section_of_name name;
    match !current with
    | Some Data ->
      let key = String.concat "," name in
      if not (Hashtbl.mem data key) then data_order := key :: !data_order;
      current_key := key;
      current_data := Option.value (Hashtbl.find_opt data key) ~default:[]
    | _ -> ()
  in
  let ins i =
    match i with
    | J (c, Sym l) -> add (Branch (Some (cond_code c), l, ref false))
    | JMP (Sym l) ->
      (match strip_suffix l "@PLT" with
       | Some l -> add (Code ("\xe9\000\000\000\000", [1, Plt32, ConstLabel l]))
       | None -> add (Branch (None, l, ref false)))
    | CALL (Sym l) -> add (Call l)
    | i ->
      let ib = ib_create () in
      encode ib i;
      add (Code (Buffer.contents ib.b, List.rev ib.irelocs))
  in
  List.iter (function
      | Ins i -> ins i
      | Section (name, _, _) -> switch_section name
      | NewLabel (l, _) -> add (Label l)
      | Align (_, n) -> add (Align n)
      | Byte c -> add (Datum (1, c))
      | Word c -> add (Datum (2, c))
      | Long c -> add (Datum (4, c))
      | Quad c -> add (Datum (8, c))
      | Bytes s -> add (Code (s, []))
      | Space n -> add (Code (String.make n '\000', []))
      | Global _ | Comment _ | Type _ | Size _ | File _ | Loc _
      | Cfi_adjust_cfa_offset _ | Cfi_endproc | Cfi_startproc
      | Cfi_remember_state | Cfi_restore_state | Cfi_def_cfa_register _
      | Cfi_def_cfa_offset _ | Private_extern _ | Indirect_symbol _
      | External _ | Mode386 | Model _ -> ()
      | Set (name, _) -> unsupported "set %s" name)
    program;
  switch_section [".text"];
  let symbols = Hashtbl.create 64 in
  (* Layout of a section, with branch relaxation: branches start short and
     become long when their target is out of range, until nothing changes. *)
  let layout symbols kind items =
    let items = Array.of_list (List.rev items) in
    let offsets = Array.make (Array.length items + 1) 0 in
    let compute () =
      let pos = ref 0 in
      Array.iteri (fun i it ->
          offsets.(i) <- !pos;
          (match it with Label l -> Hashtbl.replace symbols l (kind, !pos) | _ -> ());
          pos := !pos + item_size !pos it)
        items;
      offsets.(Array.length items) <- !pos
    in
    let changed = ref true in
    while !changed do
      changed := false;
      compute ();
      Array.iteri (fun i it ->
          match it with
          | Branch (c, l, long) when not !long ->
            (match Hashtbl.find_opt symbols l with
             | Some (k, target) when k = kind ->
               let d = target - (offsets.(i) + branch_size c false) in
               if d < -128 || d > 127 then (long := true; changed := true)
             | _ -> long := true; changed := true)
          | _ -> ())
        items;
    done;
    let buf = Buffer.create offsets.(Array.length items) in
    let relocs = ref [] and padding = ref [] in
    Array.iteri (fun i it ->
        let pos = offsets.(i) in
        assert (Buffer.length buf = pos);
        match it with
        | Code (s, rs) ->
          Buffer.add_string buf s;
          let len = String.length s in
          List.iter (fun (off, k, e) ->
              relocs := { r_offset = pos + off; r_end = pos + len; r_kind = k; r_expr = e }
                        :: !relocs)
            rs
        | Branch (c, l, long) ->
          let local = match Hashtbl.find_opt symbols l with
            | Some (k, t) when k = kind -> Some t
            | _ -> None
          in
          if not !long then begin
            let d = Option.get local - (pos + 2) in
            Buffer.add_char buf (Char.chr (match c with None -> 0xEB | Some c -> 0x70 + c));
            Buffer.add_char buf (Char.unsafe_chr (d land 0xff))
          end else begin
            (match c with
             | None -> Buffer.add_char buf '\xe9'
             | Some c -> Buffer.add_char buf '\x0f'; Buffer.add_char buf (Char.chr (0x80 + c)));
            let size = branch_size c true in
            (match local with
             | Some t ->
               let d = Int64.of_int (t - (pos + size)) in
               for k = 0 to 3 do
                 Buffer.add_char buf
                   (Char.unsafe_chr (Int64.to_int (Int64.shift_right d (8 * k)) land 0xff))
               done
             | None ->
               relocs := { r_offset = pos + size - 4; r_end = pos + size;
                           r_kind = Plt32; r_expr = ConstLabel l } :: !relocs;
               Buffer.add_string buf "\000\000\000\000")
          end
        | Call l ->
          Buffer.add_char buf '\xe8';
          let target = match strip_suffix l "@PLT" with Some s -> s | None -> l in
          relocs := { r_offset = pos + 1; r_end = pos + 5; r_kind = Plt32;
                      r_expr = ConstLabel target } :: !relocs;
          Buffer.add_string buf "\000\000\000\000"
        | Label _ -> ()
        | Align n ->
          let pad = (n - pos mod n) mod n in
          if kind = Text then begin
            fill_nops buf pad;
            if pad > 0 then padding := (pos, pad) :: !padding
          end else Buffer.add_string buf (String.make pad '\000')
        | Datum (n, c) ->
          if const_is_constant c then begin
            let v = const_value c in
            for k = 0 to n - 1 do
              Buffer.add_char buf
                (Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical v (8 * k)) land 0xff))
            done
          end else begin
            relocs := { r_offset = pos; r_end = pos + n; r_kind = Abs n; r_expr = c } :: !relocs;
            Buffer.add_string buf (String.make n '\000')
          end)
      items;
    { s_bytes = Buffer.to_bytes buf; s_relocs = List.rev !relocs }, List.rev !padding
  in
  let text, padding = layout symbols Text !text in
  (* data sections, each aligned on 16 bytes *)
  let buf = Buffer.create 256 and relocs = ref [] in
  List.iter (fun key ->
      let syms = Hashtbl.create 16 in
      let sec, _ = layout syms Data (Hashtbl.find data key) in
      let base = (Buffer.length buf + 15) / 16 * 16 in
      Buffer.add_string buf (String.make (base - Buffer.length buf) '\000');
      Buffer.add_bytes buf sec.s_bytes;
      Hashtbl.iter (fun l (k, off) -> Hashtbl.replace symbols l (k, base + off)) syms;
      relocs := List.rev_append
          (List.map (fun r -> { r with r_offset = base + r.r_offset; r_end = base + r.r_end })
             sec.s_relocs) !relocs)
    (List.rev !data_order);
  let data = { s_bytes = Buffer.to_bytes buf; s_relocs = List.rev !relocs } in
  { text; data; symbols; padding }
