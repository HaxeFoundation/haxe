(* In-memory linker and loader for the units assembled by [X86_encode].

   Layout of a loaded unit, in one allocation:

     [text][PLT]  page aligned, read and execute
     [data][GOT]  read and write

   Calls and jumps to symbols outside the unit go through a PLT stub, and
   references marked @GOTPCREL through a GOT entry holding the absolute
   address: the unit can be anywhere in the address space, whatever the
   distance to the runtime. *)

open Ocaml_backend

open X86_encode

exception Link_error of string

let link_error fmt = Printf.ksprintf (fun s -> raise (Link_error s)) fmt

external runtime_symbol : string -> nativeint = "jit_runtime_symbol"
external alloc : int -> nativeint = "jit_alloc"
external free : nativeint -> int -> unit = "jit_free"
external blit : Bytes.t -> nativeint -> unit = "jit_blit"
external make_executable : nativeint -> int -> unit = "jit_make_executable"
external register : nativeint -> nativeint -> nativeint -> nativeint -> unit
  = "jit_register"
external value_at : nativeint -> Obj.t = "jit_value_at"

type loaded = {
  base : nativeint;
  size : int;
  lookup : string -> nativeint;   (* address of a symbol of the unit *)
}

let page_size = 4096
let round n a = (n + a - 1) / a * a

let plt_entry_size = 8   (* jmp *disp32(%rip), padded *)

(* External symbols: those of the runtime, then those of [extra] *)
let resolve_external extra s =
  match extra s with
  | Some a -> a
  | None ->
    try runtime_symbol s with Not_found -> link_error "undefined symbol %s" s

let set_le b off v n =
  for k = 0 to n - 1 do
    Bytes.set b (off + k)
      (Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical v (8 * k)) land 0xff))
  done

let load ?(extra = fun _ -> None) (a : assembled) : loaded =
  let text_size = Bytes.length a.text.s_bytes in
  (* GOT and PLT entries *)
  let got = Hashtbl.create 16 and plt = Hashtbl.create 16 in
  let is_local s = Hashtbl.mem a.symbols s in
  let add tbl s = if not (Hashtbl.mem tbl s) then Hashtbl.add tbl s (Hashtbl.length tbl) in
  let scan sec =
    List.iter (fun r ->
        match r.r_kind, r.r_expr with
        | Gotpcrel32, X86_ast.ConstLabel s -> add got s
        | Plt32, X86_ast.ConstLabel s -> if not (is_local s) then (add plt s; add got s)
        | (Gotpcrel32 | Plt32), _ -> link_error "unexpected relocation expression"
        | _ -> ())
      sec.s_relocs
  in
  scan a.text; scan a.data;
  let plt_offset = round text_size 16 in
  let exec_size = round (plt_offset + plt_entry_size * Hashtbl.length plt) page_size in
  let data_offset = exec_size in
  let data_size = Bytes.length a.data.s_bytes in
  let got_offset = data_offset + round data_size 8 in
  let size = round (got_offset + 8 * Hashtbl.length got) page_size in
  let base = alloc size in
  let addr off = Nativeint.add base (Nativeint.of_int off) in
  let symbol s =
    match Hashtbl.find_opt a.symbols s with
    | Some (Text, off) -> addr off
    | Some (Data, off) -> addr (data_offset + off)
    | None -> resolve_external extra s
  in
  let image = Bytes.make size '\000' in
  Bytes.blit a.text.s_bytes 0 image 0 text_size;
  Bytes.blit a.data.s_bytes 0 image data_offset data_size;
  (* GOT entries and PLT stubs *)
  Hashtbl.iter (fun s i ->
      set_le image (got_offset + 8 * i) (Int64.of_nativeint (symbol s)) 8)
    got;
  Hashtbl.iter (fun s i ->
      let off = plt_offset + plt_entry_size * i in
      let got_entry = got_offset + 8 * Hashtbl.find got s in
      Bytes.set image off '\xff';
      Bytes.set image (off + 1) '\x25';
      set_le image (off + 2) (Int64.of_int (got_entry - (off + 6))) 4;
      Bytes.set image (off + 6) '\x0f';   (* ud2 *)
      Bytes.set image (off + 7) '\x0b')
    plt;
  (* Relocations *)
  let apply sec_offset sec =
    List.iter (fun r ->
        let field = sec_offset + r.r_offset in
        let rec eval = function
          | X86_ast.Const n -> n
          | ConstThis -> Int64.of_nativeint (addr field)
          | ConstLabel s -> Int64.of_nativeint (symbol s)
          | ConstAdd (x, y) -> Int64.add (eval x) (eval y)
          | ConstSub (x, y) -> Int64.sub (eval x) (eval y)
        in
        let pc_end = Int64.of_nativeint (addr (sec_offset + r.r_end)) in
        let rel32 target =
          let d = Int64.sub target pc_end in
          if d < -0x8000_0000L || d > 0x7FFF_FFFFL then
            link_error "32-bit displacement out of range";
          set_le image field d 4
        in
        match r.r_kind with
        | Abs n ->
          let v = eval r.r_expr in
          if n = 4 && (v < -0x8000_0000L || v > 0xFFFF_FFFFL) then
            link_error "32-bit value out of range";
          set_le image field v n
        | Pcrel32 -> rel32 (eval r.r_expr)
        | Plt32 ->
          (match r.r_expr with
           | ConstLabel s when is_local s -> rel32 (eval r.r_expr)
           | ConstLabel s ->
             rel32 (Int64.of_nativeint (addr (plt_offset + plt_entry_size * Hashtbl.find plt s)))
           | _ -> assert false)
        | Gotpcrel32 ->
          (match r.r_expr with
           | ConstLabel s ->
             rel32 (Int64.of_nativeint (addr (got_offset + 8 * Hashtbl.find got s)))
           | _ -> assert false))
      sec.s_relocs
  in
  apply 0 a.text;
  apply data_offset a.data;
  blit image base;
  make_executable base exec_size;
  let lookup s =
    match Hashtbl.find_opt a.symbols s with
    | Some _ -> symbol s
    | None -> raise Not_found
  in
  { base; size; lookup }

(* Registers the unit [unit_symbol] with the runtime: frame table, code
   fragment and, if [gc_roots] names a symbol, its GC roots. *)
let register_unit ?gc_roots l ~frametable ~code_begin ~code_end =
  let roots = match gc_roots with Some s -> l.lookup s | None -> 0n in
  register (l.lookup frametable) roots (l.lookup code_begin) (l.lookup code_end)

let unload l = free l.base l.size
