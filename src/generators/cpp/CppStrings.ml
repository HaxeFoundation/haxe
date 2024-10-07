open Extlib_leftovers
open Globals

let gen_hash32 seed str =
  let h = ref (Int32.of_int seed) in
  let cycle = Int32.of_int 223 in
  for i = 0 to String.length str - 1 do
    h :=
      Int32.add (Int32.mul !h cycle)
        (Int32.of_int (int_of_char (String.unsafe_get str i)))
  done;
  !h

let hash64 s = String.sub (Digest.to_hex (Digest.string s)) 0 16
let gen_hash seed str = Printf.sprintf "0x%08lx" (gen_hash32 seed str)
let gen_hash_small seed str = Printf.sprintf "%08lx" (gen_hash32 seed str)

let gen_qstring_hash str =
  let h = gen_hash32 0 str in
  Printf.sprintf "%02lx,%02lx,%02lx,%02lx"
    (Int32.shift_right_logical (Int32.shift_left h 24) 24)
    (Int32.shift_right_logical (Int32.shift_left h 16) 24)
    (Int32.shift_right_logical (Int32.shift_left h 8) 24)
    (Int32.shift_right_logical h 24)

let gen_wqstring_hash str =
  let h = gen_hash32 0 str in
  Printf.sprintf "%04lx,%04lx"
    (Int32.shift_right_logical (Int32.shift_left h 16) 16)
    (Int32.shift_right_logical h 16)

let special_to_hex s =
  let l = String.length s in
  let b = Buffer.create 0 in
  for i = 0 to l - 1 do
    match Char.code (String.unsafe_get s i) with
    | c when c > 127 || c < 32 ->
        Buffer.add_string b (Printf.sprintf "\\x%02x\"\"" c)
    | c -> Buffer.add_char b (Char.chr c)
  done;
  Buffer.contents b

let strq ctx s =
  let has_utf8_chars s =
    let result = ref false in
    for i = 0 to String.length s - 1 do
      result := !result || Char.code (String.unsafe_get s i) > 127
    done;
    !result
  in

  let gen_str macro gen s =
    let rec split s plus =
      let escaped = StringHelper.s_escape ~hex:false s in
      let hexed = special_to_hex escaped in
      if String.length hexed <= 16000 then
        plus ^ " HX_CSTRING(\"" ^ hexed ^ "\")"
      else
        let len = String.length s in
        let half = len lsr 1 in
        split (String.sub s 0 half) plus
        ^ split (String.sub s half (len - half)) "+"
    in
    let escaped = StringHelper.s_escape ~hex:false s in
    let hexed = special_to_hex escaped in
    if String.length hexed <= 16000 then
      macro ^ "(\"" ^ hexed ^ "\"," ^ gen s ^ ")"
    else "(" ^ split s "" ^ ")"
  in

  if Common.defined ctx Define.HxcppSmartStings && has_utf8_chars s then (
    let b = Buffer.create 0 in

    let add ichar =
      match ichar with
      | 92 (* \ *) -> Buffer.add_string b "\\\\"
      | 39 (* ' *) -> Buffer.add_string b "\\\'"
      | 34 -> Buffer.add_string b "\\\""
      | 13 (* \r *) -> Buffer.add_string b "\\r"
      | 10 (* \n *) -> Buffer.add_string b "\\n"
      | 9 (* \t *) -> Buffer.add_string b "\\t"
      | c when c < 32 || (c >= 127 && c <= 0xFFFF) ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" c)
      | c when c > 0xFFFF -> Buffer.add_string b (Printf.sprintf "\\U%08x" c)
      | c -> Buffer.add_char b (Char.chr c)
    in
    UTF8.iter (fun c -> add (UCharExt.code c)) s;
    "HX_W(u\"" ^ Buffer.contents b ^ "\"," ^ gen_wqstring_hash s ^ ")")
  else gen_str "HX_" gen_qstring_hash s

let escape_command s =
  let b = Buffer.create 0 in
  String.iter
    (fun ch ->
      if ch == '"' || ch == '\\' then Buffer.add_string b "\\";
      Buffer.add_char b ch)
    s;
  Buffer.contents b

let const_char_star s =
  let escaped = StringHelper.s_escape ~hex:false s in
  "\"" ^ special_to_hex escaped ^ "\""

let make_path_absolute path pos =
   try
      if (String.sub path 0 2) = "./" then begin
         let base = if (Filename.is_relative pos.pfile) then
            Filename.concat (Sys.getcwd()) pos.pfile
         else
            pos.pfile
         in
         Path.normalize_path (Filename.concat (Filename.dirname base) (String.sub path 2 ((String.length path) -2)))
      end else
         path
   with Invalid_argument _ -> path

let path_of_string path =
  ["@verbatim"], path

let get_all_meta_string_path meta_list key =
  let extract_path pos expr =
    match expr with
    | Ast.EConst (Ast.String (name, _)), _ -> make_path_absolute name pos
    | _ -> ""
  in
  let extract_meta meta =
    match meta with
    | k, exprs, pos when k = key -> Some (extract_path pos (List.hd exprs))
    | _ -> None
  in
  ExtList.List.filter_map extract_meta meta_list