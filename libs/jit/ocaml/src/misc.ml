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

(* Errors *)

exception Fatal_error

let fatal_errorf fmt =
  Format.kfprintf
    (fun _ -> raise Fatal_error)
    Format.err_formatter
    ("@?>> Fatal error: " ^^ fmt ^^ "@.")

let fatal_error msg = fatal_errorf "%s" msg

(* Exceptions *)

let try_finally ?(always=(fun () -> ())) ?(exceptionally=(fun () -> ())) work =
  match work () with
    | result ->
      begin match always () with
        | () -> result
        | exception always_exn ->
          let always_bt = Printexc.get_raw_backtrace () in
          exceptionally ();
          Printexc.raise_with_backtrace always_exn always_bt
      end
    | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      begin match always () with
        | () ->
          exceptionally ();
          Printexc.raise_with_backtrace work_exn work_bt
        | exception always_exn ->
          let always_bt = Printexc.get_raw_backtrace () in
          exceptionally ();
          Printexc.raise_with_backtrace always_exn always_bt
      end

let reraise_preserving_backtrace e f =
  let bt = Printexc.get_raw_backtrace () in
  f ();
  Printexc.raise_with_backtrace e bt

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter (fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map (fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    Fun.protect ~finally:(fun () -> set_refs backup) f

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rev_map_end f l1 l2 =
  let rec rmap_f accu = function
    | [] -> accu
    | hd::tl -> rmap_f (f hd :: accu) tl
  in
  rmap_f l2 l1

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

module Stdlib = struct
  module List = struct
    type 'a t = 'a list

    let rec compare cmp l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | [], _::_ -> -1
      | _::_, [] -> 1
      | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c <> 0 then c
        else compare cmp t1 t2

    let rec equal eq l1 l2 =
      match l1, l2 with
      | ([], []) -> true
      | (hd1 :: tl1, hd2 :: tl2) -> eq hd1 hd2 && equal eq tl1 tl2
      | (_, _) -> false

    let map2_prefix f l1 l2 =
      let rec aux acc l1 l2 =
        match l1, l2 with
        | [], _ -> (List.rev acc, l2)
        | _ :: _, [] -> raise (Invalid_argument "map2_prefix")
        | h1::t1, h2::t2 ->
          let h = f h1 h2 in
          aux (h :: acc) t1 t2
      in
      aux [] l1 l2

    let rec iteri2 i f l1 l2 =
      match (l1, l2) with
        ([], []) -> ()
      | (a1::l1, a2::l2) -> f i a1 a2; iteri2 (i + 1) f l1 l2
      | (_, _) -> raise (Invalid_argument "iteri2")

    let iteri2 f l1 l2 = iteri2 0 f l1 l2

    let some_if_all_elements_are_some l =
      let rec aux acc l =
        match l with
        | [] -> Some (List.rev acc)
        | None :: _ -> None
        | Some h :: t -> aux (h :: acc) t
      in
      aux [] l

    let split_at n l =
      let rec aux n acc l =
        if n = 0
        then List.rev acc, l
        else
          match l with
          | [] -> raise (Invalid_argument "split_at")
          | t::q -> aux (n-1) (t::acc) q
      in
      aux n [] l

    let chunks_of n l =
      if n <= 0 then raise (Invalid_argument "chunks_of");
      (* Invariant: List.length l = remaining *)
      let rec aux n acc l ~remaining =
        match remaining with
        | 0 -> List.rev acc
        | _ when remaining <= n -> List.rev (l :: acc)
        | _ ->
          let chunk, rest = split_at n l in
          aux n (chunk :: acc) rest ~remaining:(remaining - n)
      in
      aux n [] l ~remaining:(List.length l)

    let rec is_prefix ~equal t ~of_ =
      match t, of_ with
      | [], [] -> true
      | _::_, [] -> false
      | [], _::_ -> true
      | x1::t, x2::of_ -> equal x1 x2 && is_prefix ~equal t ~of_

    type 'a longest_common_prefix_result = {
      longest_common_prefix : 'a list;
      first_without_longest_common_prefix : 'a list;
      second_without_longest_common_prefix : 'a list;
    }

    let find_and_chop_longest_common_prefix ~equal ~first ~second =
      let rec find_prefix ~longest_common_prefix_rev l1 l2 =
        match l1, l2 with
        | elt1 :: l1, elt2 :: l2 when equal elt1 elt2 ->
          let longest_common_prefix_rev = elt1 :: longest_common_prefix_rev in
          find_prefix ~longest_common_prefix_rev l1 l2
        | l1, l2 ->
          { longest_common_prefix = List.rev longest_common_prefix_rev;
            first_without_longest_common_prefix = l1;
            second_without_longest_common_prefix = l2;
          }
      in
      find_prefix ~longest_common_prefix_rev:[] first second
  end

  module Option = struct
    type 'a t = 'a option

    let print print_contents ppf t =
      match t with
      | None -> Format.pp_print_string ppf "None"
      | Some contents ->
        Format.fprintf ppf "@[(Some@ %a)@]" print_contents contents
  end

  module Array = struct
    let exists2 p a1 a2 =
      let n = Array.length a1 in
      if Array.length a2 <> n then invalid_arg "Misc.Stdlib.Array.exists2";
      let rec loop i =
        if i = n then false
        else if p (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) then true
        else loop (succ i) in
      loop 0

    let for_alli p a =
      let n = Array.length a in
      let rec loop i =
        if i = n then true
        else if p i (Array.unsafe_get a i) then loop (succ i)
        else false in
      loop 0

    let all_somes a =
      try
        Some (Array.map (function None -> raise_notrace Exit | Some x -> x) a)
      with
      | Exit -> None
  end

  module String = struct
    include String
    module Set = Set.Make(String)
    module Map = Map.Make(String)
    module Tbl = Hashtbl.Make(struct
      include String
      let hash = Hashtbl.hash
    end)

    let for_all f t =
      let len = String.length t in
      let rec loop i =
        i = len || (f t.[i] && loop (i + 1))
      in
      loop 0

    let print ppf t =
      Format.pp_print_string ppf t
  end

  external compare : 'a -> 'a -> int = "%compare"
end

(** {1 Minimal support for Unicode characters in identifiers} *)

module Utf8_lexeme = struct

  type t = string

  (* Non-ASCII letters that are allowed in identifiers (currently: Latin-9) *)

  type case = Upper of Uchar.t | Lower of Uchar.t
  let known_chars : (Uchar.t, case) Hashtbl.t = Hashtbl.create 32

  let _ =
    List.iter
      (fun (upper, lower) ->
        let upper = Uchar.of_int upper and lower = Uchar.of_int lower in
        Hashtbl.add known_chars upper (Upper lower);
        Hashtbl.add known_chars lower (Lower upper))
  [
    (0xc0, 0xe0); (* À, à *)    (0xc1, 0xe1); (* Á, á *)
    (0xc2, 0xe2); (* Â, â *)    (0xc3, 0xe3); (* Ã, ã *)
    (0xc4, 0xe4); (* Ä, ä *)    (0xc5, 0xe5); (* Å, å *)
    (0xc6, 0xe6); (* Æ, æ *)    (0xc7, 0xe7); (* Ç, ç *)
    (0xc8, 0xe8); (* È, è *)    (0xc9, 0xe9); (* É, é *)
    (0xca, 0xea); (* Ê, ê *)    (0xcb, 0xeb); (* Ë, ë *)
    (0xcc, 0xec); (* Ì, ì *)    (0xcd, 0xed); (* Í, í *)
    (0xce, 0xee); (* Î, î *)    (0xcf, 0xef); (* Ï, ï *)
    (0xd0, 0xf0); (* Ð, ð *)    (0xd1, 0xf1); (* Ñ, ñ *)
    (0xd2, 0xf2); (* Ò, ò *)    (0xd3, 0xf3); (* Ó, ó *)
    (0xd4, 0xf4); (* Ô, ô *)    (0xd5, 0xf5); (* Õ, õ *)
    (0xd6, 0xf6); (* Ö, ö *)    (0xd8, 0xf8); (* Ø, ø *)
    (0xd9, 0xf9); (* Ù, ù *)    (0xda, 0xfa); (* Ú, ú *)
    (0xdb, 0xfb); (* Û, û *)    (0xdc, 0xfc); (* Ü, ü *)
    (0xdd, 0xfd); (* Ý, ý *)    (0xde, 0xfe); (* Þ, þ *)
    (0x160, 0x161); (* Š, š *)  (0x17d, 0x17e); (* Ž, ž *)
    (0x152, 0x153); (* Œ, œ *)  (0x178, 0xff); (* Ÿ, ÿ *)
    (0x1e9e, 0xdf); (* ẞ, ß *)
  ]

  (* NFD to NFC conversion table for the letters above *)

  let known_pairs : (Uchar.t * Uchar.t, Uchar.t) Hashtbl.t = Hashtbl.create 32

  let _ =
    List.iter
      (fun (c1, n2, n) ->
        Hashtbl.add known_pairs
          (Uchar.of_char c1, Uchar.of_int n2) (Uchar.of_int n))
  [
    ('A', 0x300, 0xc0); (* À *)    ('A', 0x301, 0xc1); (* Á *)
    ('A', 0x302, 0xc2); (* Â *)    ('A', 0x303, 0xc3); (* Ã *)
    ('A', 0x308, 0xc4); (* Ä *)    ('A', 0x30a, 0xc5); (* Å *)
    ('C', 0x327, 0xc7); (* Ç *)    ('E', 0x300, 0xc8); (* È *)
    ('E', 0x301, 0xc9); (* É *)    ('E', 0x302, 0xca); (* Ê *)
    ('E', 0x308, 0xcb); (* Ë *)    ('I', 0x300, 0xcc); (* Ì *)
    ('I', 0x301, 0xcd); (* Í *)    ('I', 0x302, 0xce); (* Î *)
    ('I', 0x308, 0xcf); (* Ï *)    ('N', 0x303, 0xd1); (* Ñ *)
    ('O', 0x300, 0xd2); (* Ò *)    ('O', 0x301, 0xd3); (* Ó *)
    ('O', 0x302, 0xd4); (* Ô *)    ('O', 0x303, 0xd5); (* Õ *)
    ('O', 0x308, 0xd6); (* Ö *)
    ('U', 0x300, 0xd9); (* Ù *)    ('U', 0x301, 0xda); (* Ú *)
    ('U', 0x302, 0xdb); (* Û *)    ('U', 0x308, 0xdc); (* Ü *)
    ('Y', 0x301, 0xdd); (* Ý *)    ('Y', 0x308, 0x178);  (* Ÿ *)
    ('S', 0x30c, 0x160); (* Š *)   ('Z', 0x30c, 0x17d); (* Ž *)
    ('a', 0x300, 0xe0); (* à *)    ('a', 0x301, 0xe1); (* á *)
    ('a', 0x302, 0xe2); (* â *)    ('a', 0x303, 0xe3); (* ã *)
    ('a', 0x308, 0xe4); (* ä *)    ('a', 0x30a, 0xe5); (* å *)
    ('c', 0x327, 0xe7); (* ç *)    ('e', 0x300, 0xe8); (* è *)
    ('e', 0x301, 0xe9); (* é *)    ('e', 0x302, 0xea); (* ê *)
    ('e', 0x308, 0xeb); (* ë *)    ('i', 0x300, 0xec); (* ì *)
    ('i', 0x301, 0xed); (* í *)    ('i', 0x302, 0xee); (* î *)
    ('i', 0x308, 0xef); (* ï *)    ('n', 0x303, 0xf1); (* ñ *)
    ('o', 0x300, 0xf2); (* ò *)    ('o', 0x301, 0xf3); (* ó *)
    ('o', 0x302, 0xf4); (* ô *)    ('o', 0x303, 0xf5); (* õ *)
    ('o', 0x308, 0xf6); (* ö *)
    ('u', 0x300, 0xf9); (* ù *)    ('u', 0x301, 0xfa); (* ú *)
    ('u', 0x302, 0xfb); (* û *)    ('u', 0x308, 0xfc); (* ü *)
    ('y', 0x301, 0xfd); (* ý *)    ('y', 0x308, 0xff); (* ÿ *)
    ('s', 0x30c, 0x161); (* š *)   ('z', 0x30c, 0x17e); (* ž *)
  ]

  let normalize_generic ~keep_ascii transform s =
    let rec norm check buf prev i =
      if i >= String.length s then begin
        Buffer.add_utf_8_uchar buf (transform prev)
      end else begin
        let d = String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        check d u;
        let i' = i + Uchar.utf_decode_length d in
        match Hashtbl.find_opt known_pairs (prev, u) with
        | Some u' ->
            norm check buf u' i'
        | None ->
            Buffer.add_utf_8_uchar buf (transform prev);
            norm check buf u i'
      end in
    let ascii_limit = 128 in
    if s = ""
    || keep_ascii && String.for_all (fun x -> Char.code x < ascii_limit) s
    then Ok s
    else
      let buf = Buffer.create (String.length s) in
      let valid = ref true in
      let check d u =
        valid := !valid && Uchar.utf_decode_is_valid d && u <> Uchar.rep
      in
      let d = String.get_utf_8_uchar s 0 in
      let u = Uchar.utf_decode_uchar d in
      check d u;
      norm check buf u (Uchar.utf_decode_length d);
      let contents = Buffer.contents buf in
      if !valid then
        Ok contents
      else
        Error contents

  let normalize s =
    normalize_generic ~keep_ascii:true (fun u -> u) s

  (* Capitalization *)

  let uchar_is_uppercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then c >= 65 && c <= 90 else
      match Hashtbl.find_opt known_chars u with
      | Some(Upper _) -> true
      | _ -> false

  let uchar_lowercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then
      if c >= 65 && c <= 90 then Uchar.of_int (c + 32) else u
    else
      match Hashtbl.find_opt known_chars u with
      | Some(Upper u') -> u'
      | _ -> u

  let uchar_uppercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then
      if c >= 97 && c <= 122 then Uchar.of_int (c - 32) else u
    else
      match Hashtbl.find_opt known_chars u with
      | Some(Lower u') -> u'
      | _ -> u

  let capitalize s =
    let first = ref true in
    normalize_generic ~keep_ascii:false
      (fun u -> if !first then (first := false; uchar_uppercase u) else u)
      s

  let uncapitalize s =
    let first = ref true in
    normalize_generic ~keep_ascii:false
      (fun u -> if !first then (first := false; uchar_lowercase u) else u)
      s

  let is_capitalized s =
    s <> "" &&
    uchar_is_uppercase (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))

  (* Characters allowed in identifiers after normalization is applied.
     Currently:
       - ASCII letters, underscore
       - Latin-9 letters, represented in NFC
       - ASCII digits, single quote (but not as first character)
       - dot if [with_dot] = true
  *)
  let uchar_valid_in_identifier ~with_dot u =
    let c = Uchar.to_int u in
    if c < 0x80 then
         c >= 97 (* a *) && c <= 122 (* z *)
      || c >= 65 (* A *) && c <= 90 (* Z *)
      || c >= 48 (* 0 *) && c <= 57 (* 9 *)
      || c = 95 (* underscore *)
      || c = 39 (* single quote *)
      || (with_dot && c = 46) (* dot *)
    else
      Hashtbl.mem known_chars u

  let uchar_not_identifier_start u =
    let c = Uchar.to_int u in
       c >= 48 (* 0 *) && c <= 57 (* 9 *)
    || c = 39  (* single quote *)

  (* Check whether a normalized string is a valid OCaml identifier. *)

  type validation_result =
    | Valid
    | Invalid_character of Uchar.t   (** Character not allowed *)
    | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

  let validate_identifier ?(with_dot=false) s =
    let rec check i =
      if i >= String.length s then Valid else begin
        let d = String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        let i' = i + Uchar.utf_decode_length d in
        if not (uchar_valid_in_identifier ~with_dot u) then
          Invalid_character u
        else if i = 0 && uchar_not_identifier_start u then
          Invalid_beginning u
        else
          check i'
      end
    in check 0

  let is_valid_identifier s =
    validate_identifier s = Valid

  let starts_like_a_valid_identifier s =
    s <> "" &&
    (let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0) in
     uchar_valid_in_identifier ~with_dot:false u
     && not (uchar_not_identifier_start u))

  let is_lowercase s =
    let rec is_lowercase_at len s n =
      if n >= len then true
      else
        let d = String.get_utf_8_uchar s n in
        let u = Uchar.utf_decode_uchar d in
        (uchar_valid_in_identifier ~with_dot:false  u)
        && not (uchar_is_uppercase u)
        && is_lowercase_at len s (n+Uchar.utf_decode_length d)
    in
    is_lowercase_at (String.length s) s 0
end

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = simplify (Filename.concat dir name) in
      if Sys.file_exists fullname then fullname else try_dir rem
  in try_dir path

let normalized_unit_filename = Utf8_lexeme.uncapitalize

let find_in_path_normalized path name =
  match normalized_unit_filename name with
  | Error _ -> raise Not_found
  | Ok uname ->
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
    if Sys.is_regular_file filename
    then Sys.remove filename
  with Sys_error _msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

let path_separator =
  match Sys.os_type with
  | "Win32" -> ';'
  | _ -> ':'

let split_path_contents ?(sep = path_separator) = function
  | "" -> []
  | s -> String.split_on_char sep s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (Int.min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let (temp_filename, oc) =
    Filename.open_temp_file
       ~mode ~perms:0o666 ~temp_dir:(Filename.dirname filename)
       (Filename.basename filename) ".tmp" in
    (* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res ->
      close_out oc;
      begin try
        Sys.rename temp_filename filename; res
      with exn ->
        remove_file temp_filename; raise exn
      end
  | exception exn ->
      close_out oc; remove_file temp_filename; raise exn

let protect_writing_to_file ~filename ~f =
  let outchan = open_out_bin filename in
  try_finally ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file filename)
    (fun () -> f outchan)

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && (a * b) / b <> a))

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size - 1 && min_int asr k <= a && a <= max_int asr k

let letter_of_int n =
  let letter = String.make 1 (Char.chr (Char.code 'a' + n mod 26)) in
  let num = n / 26 in
  if num = 0 then letter
  else letter ^ Int.to_string num

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

(* [find_first_mono p] assumes that there exists a natural number
   N such that [p] is false on [0; N[ and true on [N; max_int], and
   returns this N. (See misc.mli for the detailed specification.) *)
let find_first_mono =
  let rec find p ~low ~jump ~high =
    (* Invariants:
       [low, jump, high] are non-negative with [low < high],
       [p low = false],
       [p high = true]. *)
    if low + 1 = high then high
    (* ensure that [low + jump] is in ]low; high[ *)
    else if jump < 1 then find p ~low ~jump:1 ~high
    else if jump >= high - low then find p ~low ~jump:((high - low) / 2) ~high
    else if p (low + jump) then
      (* We jumped too high: continue with a smaller jump and lower limit *)
      find p ~low:low ~jump:(jump / 2) ~high:(low + jump)
    else
      (* we jumped too low:
         continue from [low + jump] with a larger jump *)
      let next_jump = max jump (2 * jump) (* avoid overflows *) in
      find p ~low:(low + jump) ~jump:next_jump ~high
  in
  fun p ->
    if p 0 then 0
    else find p ~low:0 ~jump:1 ~high:max_int

(* String operations *)

let split_null_terminated s =
  let[@tail_mod_cons] rec discard_last_sep = function
    | [] | [""] -> []
    | x :: xs -> x :: discard_last_sep xs
  in
  discard_last_sep (String.split_on_char '\000' s)

let concat_null_terminated = function
  | [] -> ""
  | l -> String.concat "\000" (l @ [""])

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str curr (next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str curr (String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat after (search [] 0)

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let set_or_ignore f opt x =
  match f x with
  | None -> ()
  | Some y -> opt := Some y

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)

let ordinal_suffix n =
  let teen = (n mod 100)/10 = 1 in
  match n mod 10 with
  | 1 when not teen -> "st"
  | 2 when not teen -> "nd"
  | 3 when not teen -> "rd"
  | _ -> "th"

(* Color support handling *)
module Color = struct
  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto | Always | Never

  let default_setting = Auto
  let enabled = ref true

end

(* Terminal styling handling *)
module Style = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"


  type Format.stag += Style of style list

  type tag_style ={
    ansi: style list;
    text_open:string;
    text_close:string
  }

  type styles = {
    error: tag_style;
    warning: tag_style;
    loc: tag_style;
    hint: tag_style;
    inline_code: tag_style;
  }

  let no_markup stl = { ansi = stl; text_close = ""; text_open = "" }

  let default_styles = {
      warning = no_markup [Bold; FG Magenta];
      error = no_markup [Bold; FG Red];
      loc = no_markup [Bold];
      hint = no_markup [Bold; FG Blue];
      inline_code= { ansi=[Bold]; text_open = {|"|}; text_close = {|"|} }
    }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | Format.String_tag "error" ->  (!cur_styles).error
    | Format.String_tag "warning" ->(!cur_styles).warning
    | Format.String_tag "loc" -> (!cur_styles).loc
    | Format.String_tag "hint" -> (!cur_styles).hint
    | Format.String_tag "inline_code" -> (!cur_styles).inline_code
    | Style s -> no_markup s
    | _ -> raise Not_found


  let as_inline_code printer ppf x =
    let open Format_doc in
    pp_open_stag ppf (Format.String_tag "inline_code");
    printer ppf x;
    pp_close_stag ppf ()

  let inline_code ppf s = as_inline_code Format_doc.pp_print_string ppf s

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l style.ansi else style.text_open
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l [Reset] else style.text_close
    with Not_found -> or_else s

  (* add tag handling to formatter [ppf] *)
  let set_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' = {functions with
      mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
      mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    ()

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    let enable_color = function
      | Color.Auto -> Color.should_enable_color ()
      | Color.Always -> true
      | Color.Never -> false
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_tag_handling formatter_l;
        Color.enabled := (match o with
          | Some s -> enable_color s
          | None -> enable_color Color.default_setting)
      );
      ()
end

let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    Int.min (Int.max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = Int.max 1 (i - cutoff - 1) to Int.min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          Int.min (1 + Int.min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else Int.min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end

let spellcheck env name =
  let cutoff =
    match String.length name with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target acc head =
    match edit_distance target head cutoff with
      | None -> acc
      | Some dist ->
         let (best_choice, best_dist) = acc in
         if dist < best_dist then ([head], dist)
         else if dist = best_dist then (head :: best_choice, dist)
         else acc
  in
  let env = List.sort_uniq (fun s1 s2 -> String.compare s2 s1) env in
  fst (List.fold_left (compare name) ([], max_int) env)


let did_you_mean ppf get_choices =
  let open Format_doc in
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
    let rest, last = split_last choices in
     fprintf ppf "@\n@[@{<hint>Hint@}: Did you mean %a%s%a?@]"
       (pp_print_list ~pp_sep:comma Style.inline_code) rest
       (if rest = [] then "" else " or ")
       Style.inline_code last

module Error_style = struct
  type setting =
    | Contextual
    | Short

  let default_setting = Contextual
end

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

let delete_eol_spaces src =
  let len_src = String.length src in
  let dst = Bytes.create len_src in
  let rec loop i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces 1 (i_src + 1) i_dst
      | c ->
        Bytes.set dst i_dst c;
        loop (i_src + 1) (i_dst + 1)
  and loop_spaces spaces i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces (spaces + 1) (i_src + 1) i_dst
      | '\n' ->
        Bytes.set dst i_dst '\n';
        loop (i_src + 1) (i_dst + 1)
      | _ ->
        for n = 0 to spaces do
          Bytes.set dst (i_dst + n) src.[i_src - spaces + n]
        done;
        loop (i_src + 1) (i_dst + spaces + 1)
  in
  let stop = loop 0 0 in
  Bytes.sub_string dst 0 stop

(* showing configuration and configuration variables *)
let show_config_and_exit () =
  Config.print_config stdout;
  exit 0

let show_config_variable_and_exit x =
  match Config.config_var x with
  | Some v ->
      (* we intentionally don't print a newline to avoid Windows \r
         issues: bash only strips the trailing \n when using a command
         substitution $(ocamlc -config-var foo), so a trailing \r would
         remain if printing a newline under Windows and scripts would
         have to use $(ocamlc -config-var foo | tr -d '\r')
         for portability. Ugh. *)
      print_string v;
      exit 0
  | None ->
      exit 2

let get_build_path_prefix_map =
  let init = ref false in
  let map_cache = ref None in
  fun () ->
    if not !init then begin
      init := true;
      match Sys.getenv "BUILD_PATH_PREFIX_MAP" with
      | exception Not_found -> ()
      | encoded_map ->
        match Build_path_prefix_map.decode_map encoded_map with
          | Error err ->
              fatal_errorf
                "Invalid value for the environment variable \
                 BUILD_PATH_PREFIX_MAP: %s" err
          | Ok map -> map_cache := Some map
    end;
    !map_cache

let debug_prefix_map_flags () =
  if not Config.as_has_debug_prefix_map then
    []
  else begin
    match get_build_path_prefix_map () with
    | None -> []
    | Some map ->
      List.fold_right
        (fun map_elem acc ->
           match map_elem with
           | None -> acc
           | Some { Build_path_prefix_map.target; source; } ->
             (Printf.sprintf "--debug-prefix-map %s=%s"
                (Filename.quote source)
                (Filename.quote target)) :: acc)
        map
        []
  end

let print_see_manual ppf manual_section =
  let open Format_doc in
  fprintf ppf "(see manual section %a)"
    (pp_print_list ~pp_sep:(fun f () -> pp_print_char f '.') pp_print_int)
    manual_section

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg


type filepath = string
type modname = string
type crcs = (modname * Digest.t option) list

type alerts = string Stdlib.String.Map.t

module Magic_number = struct
  type native_obj_config = {
    flambda : bool;
  }
  let native_obj_config = {
    flambda = Config.flambda;
  }

  type version = int

  type kind =
    | Exec
    | Cmi | Cmo | Cma
    | Cmx of native_obj_config | Cmxa of native_obj_config
    | Cmxs
    | Cmt
    | Ast_impl | Ast_intf

  (* please keep up-to-date, this is used for sanity checking *)
  let all_native_obj_configs = [
      {flambda = true};
      {flambda = false};
    ]
  let all_kinds = [
    Exec;
    Cmi; Cmo; Cma;
  ]
  @ List.map (fun conf -> Cmx conf) all_native_obj_configs
  @ List.map (fun conf -> Cmxa conf) all_native_obj_configs
  @ [
    Cmt;
    Ast_impl; Ast_intf;
  ]

  type raw = string
  type info = {
    kind: kind;
    version: version;
  }

  type raw_kind = string

  let parse_kind : raw_kind -> kind option = function
    | "Caml1999X" -> Some Exec
    | "Caml1999I" -> Some Cmi
    | "Caml1999O" -> Some Cmo
    | "Caml1999A" -> Some Cma
    | "Caml1999y" -> Some (Cmx {flambda = true})
    | "Caml1999Y" -> Some (Cmx {flambda = false})
    | "Caml1999z" -> Some (Cmxa {flambda = true})
    | "Caml1999Z" -> Some (Cmxa {flambda = false})

    (* Caml2007D and Caml2012T were used instead of the common Caml1999 prefix
       between the introduction of those magic numbers and October 2017
       (8ba70ff194b66c0a50ffb97d41fe9c4bdf9362d6).

       We accept them here, but will always produce/show kind prefixes
       that follow the current convention, Caml1999{D,T}. *)
    | "Caml2007D" | "Caml1999D" -> Some Cmxs
    | "Caml2012T" | "Caml1999T" -> Some Cmt

    | "Caml1999M" -> Some Ast_impl
    | "Caml1999N" -> Some Ast_intf
    | _ -> None

  (* note: over time the magic kind number has changed for certain kinds;
     this function returns them as they are produced by the current compiler,
     but [parse_kind] accepts older formats as well. *)
  let raw_kind : kind -> raw = function
    | Exec -> "Caml1999X"
    | Cmi -> "Caml1999I"
    | Cmo -> "Caml1999O"
    | Cma -> "Caml1999A"
    | Cmx config ->
       if config.flambda
       then "Caml1999y"
       else "Caml1999Y"
    | Cmxa config ->
       if config.flambda
       then "Caml1999z"
       else "Caml1999Z"
    | Cmxs -> "Caml1999D"
    | Cmt -> "Caml1999T"
    | Ast_impl -> "Caml1999M"
    | Ast_intf -> "Caml1999N"

  let string_of_kind : kind -> string = function
    | Exec -> "exec"
    | Cmi -> "cmi"
    | Cmo -> "cmo"
    | Cma -> "cma"
    | Cmx _ -> "cmx"
    | Cmxa _ -> "cmxa"
    | Cmxs -> "cmxs"
    | Cmt -> "cmt"
    | Ast_impl -> "ast_impl"
    | Ast_intf -> "ast_intf"

  let human_description_of_native_obj_config : native_obj_config -> string =
    fun[@warning "+9"] {flambda} ->
      if flambda then "flambda" else "non flambda"

  let human_name_of_kind : kind -> string = function
    | Exec -> "executable"
    | Cmi -> "compiled interface file"
    | Cmo -> "bytecode object file"
    | Cma -> "bytecode library"
    | Cmx config ->
       Printf.sprintf "native compilation unit description (%s)"
         (human_description_of_native_obj_config config)
    | Cmxa config ->
       Printf.sprintf "static native library (%s)"
         (human_description_of_native_obj_config config)
    | Cmxs -> "dynamic native library"
    | Cmt -> "compiled typedtree file"
    | Ast_impl -> "serialized implementation AST"
    | Ast_intf -> "serialized interface AST"

  let kind_length = 9
  let version_length = 3
  let magic_length =
    kind_length + version_length

  type parse_error =
    | Truncated of string
    | Not_a_magic_number of string

  let explain_parse_error kind_opt error =
       Printf.sprintf
         "We expected a valid %s, but the file %s."
         (Option.fold ~none:"object file" ~some:human_name_of_kind kind_opt)
         (match error with
            | Truncated "" -> "is empty"
            | Truncated _ -> "is truncated"
            | Not_a_magic_number _ -> "has a different format")

  let parse s : (info, parse_error) result =
    if String.length s = magic_length then begin
      let raw_kind = String.sub s 0 kind_length in
      let raw_version = String.sub s kind_length version_length in
      match parse_kind raw_kind with
      | None -> Error (Not_a_magic_number s)
      | Some kind ->
          begin match int_of_string raw_version with
          | exception _ -> Error (Truncated s)
          | version -> Ok { kind; version }
          end
    end
    else begin
      (* a header is "truncated" if it starts like a valid magic number,
         that is if its longest segment of length at most [kind_length]
         is a prefix of [raw_kind kind] for some kind [kind] *)
      let sub_length = Int.min kind_length (String.length s) in
      let starts_as kind =
        String.sub s 0 sub_length = String.sub (raw_kind kind) 0 sub_length
      in
      if List.exists starts_as all_kinds then Error (Truncated s)
      else Error (Not_a_magic_number s)
    end

  let read_info ic =
    let header = Buffer.create magic_length in
    begin
      try Buffer.add_channel header ic magic_length
      with End_of_file -> ()
    end;
    parse (Buffer.contents header)

  let raw { kind; version; } =
    Printf.sprintf "%s%03d" (raw_kind kind) version

  let current_raw kind =
    let open Config in
    match[@warning "+9"] kind with
      | Exec -> exec_magic_number
      | Cmi -> cmi_magic_number
      | Cmo -> cmo_magic_number
      | Cma -> cma_magic_number
      | Cmx config ->
         (* the 'if' guarantees that in the common case
            we return the "trusted" value from Config. *)
         let reference = cmx_magic_number in
         if config = native_obj_config then reference
         else
           (* otherwise we stitch together the magic number
              for a different configuration by concatenating
              the right magic kind at this configuration
              and the rest of the current raw number for our configuration. *)
           let raw_kind = raw_kind kind in
           let len = String.length raw_kind in
           raw_kind ^ String.sub reference len (String.length reference - len)
      | Cmxa config ->
         let reference = cmxa_magic_number in
         if config = native_obj_config then reference
         else
           let raw_kind = raw_kind kind in
           let len = String.length raw_kind in
           raw_kind ^ String.sub reference len (String.length reference - len)
      | Cmxs -> cmxs_magic_number
      | Cmt -> cmt_magic_number
      | Ast_intf -> ast_intf_magic_number
      | Ast_impl -> ast_impl_magic_number

  (* it would seem more direct to define current_version with the
     correct numbers and current_raw on top of it, but for now we
     consider the Config.foo values to be ground truth, and don't want
     to trust the present module instead. *)
  let current_version kind =
    let raw = current_raw kind in
    try int_of_string (String.sub raw kind_length version_length)
    with _ -> assert false

  type 'a unexpected = { expected : 'a; actual : 'a }
  type unexpected_error =
    | Kind of kind unexpected
    | Version of kind * version unexpected

  let explain_unexpected_error = function
    | Kind { actual; expected } ->
        Printf.sprintf "We expected a %s (%s) but got a %s (%s) instead."
          (human_name_of_kind expected) (string_of_kind expected)
          (human_name_of_kind actual) (string_of_kind actual)
    | Version (kind, { actual; expected }) ->
        Printf.sprintf "This seems to be a %s (%s) for %s version of OCaml."
          (human_name_of_kind kind) (string_of_kind kind)
          (if actual < expected then "an older" else "a newer")

  let check_current expected_kind { kind; version } : _ result =
    if kind <> expected_kind then begin
      let actual, expected = kind, expected_kind in
      Error (Kind { actual; expected })
    end else begin
      let actual, expected = version, current_version kind in
      if actual <> expected
      then Error (Version (kind, { actual; expected }))
      else Ok ()
    end

  type error =
    | Parse_error of parse_error
    | Unexpected_error of unexpected_error

  let read_current_info ~expected_kind ic =
    match read_info ic with
      | Error err -> Error (Parse_error err)
      | Ok info ->
         let kind = Option.value ~default:info.kind expected_kind in
         match check_current kind info with
           | Error err -> Error (Unexpected_error err)
           | Ok () -> Ok info
end
