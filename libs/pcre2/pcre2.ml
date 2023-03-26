(*
   PCRE2-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* Public exceptions and their registration with the C runtime *)

type error =
  | Partial
  | BadPattern of string * int
  | BadUTF
  | BadUTFOffset
  | MatchLimit
  | DepthLimit
  | WorkspaceSize
  | InternalError of string

exception Error of error
exception Backtrack
exception Regexp_or of string * error

(* Puts exceptions into global C-variables for fast retrieval *)
external pcre2_ocaml_init : unit -> unit = "pcre2_ocaml_init"

(* Registers exceptions with the C runtime and caches polymorphic variants *)
let () =
  Callback.register_exception "Pcre2.Error" (Error (InternalError ""));
  Callback.register_exception "Pcre2.Backtrack" Backtrack;
  pcre2_ocaml_init ()


(* Compilation and runtime flags and their conversion functions *)

type icflag = int64
type irflag = int64

(* Compilation flags *)

type cflag =
  [
  | `ALLOW_EMPTY_CLASS
  | `ALT_BSUX
  | `ALT_CIRCUMFLEX
  | `ALT_VERBNAMES
  | `ANCHORED
  | `AUTO_CALLOUT
  | `CASELESS
  | `DOLLAR_ENDONLY
  | `DOTALL
  | `DUPNAMES
  | `ENDANCHORED
  | `EXTENDED
  | `EXTENDED_MORE
  | `FIRSTLINE
  | `LITERAL
  | `MATCH_INVALID_UTF
  | `MATCH_UNSET_BACKREF
  | `MULTILINE
  | `NEVER_BACKSLASH_C
  | `NEVER_UCP
  | `NEVER_UTF
  | `NO_AUTO_CAPTURE
  | `NO_AUTO_POSSESS
  | `NO_DOTSTAR_ANCHOR
  | `NO_START_OPTIMIZE
  | `NO_UTF_CHECK
  | `UCP
  | `UNGREEDY
  | `USE_OFFSET_LIMIT
  | `UTF
  ]

let int_of_cflag = function
  | `ALLOW_EMPTY_CLASS -> 0x00000001L
  | `ALT_BSUX -> 0x00000002L
  | `AUTO_CALLOUT -> 0x00000004L
  | `CASELESS -> 0x00000008L
  | `DOLLAR_ENDONLY -> 0x00000010L
  | `DOTALL -> 0x00000020L
  | `DUPNAMES -> 0x00000040L
  | `EXTENDED -> 0x00000080L
  | `FIRSTLINE -> 0x00000100L
  | `MATCH_UNSET_BACKREF -> 0x00000200L
  | `MULTILINE -> 0x00000400L
  | `NEVER_UCP -> 0x00000800L
  | `NEVER_UTF -> 0x00001000L
  | `NO_AUTO_CAPTURE -> 0x00002000L
  | `NO_AUTO_POSSESS -> 0x00004000L
  | `NO_DOTSTAR_ANCHOR -> 0x00008000L
  | `NO_START_OPTIMIZE -> 0x00010000L
  | `UCP -> 0x00020000L
  | `UNGREEDY -> 0x00040000L
  | `UTF -> 0x00080000L
  | `NEVER_BACKSLASH_C -> 0x00100000L
  | `ALT_CIRCUMFLEX -> 0x00200000L
  | `ALT_VERBNAMES -> 0x00400000L
  | `USE_OFFSET_LIMIT -> 0x00800000L
  | `EXTENDED_MORE -> 0x01000000L
  | `LITERAL -> 0x02000000L
  | `MATCH_INVALID_UTF -> 0x04000000L
  | `ENDANCHORED -> 0x20000000L
  | `NO_UTF_CHECK -> 0x40000000L
  | `ANCHORED -> 0x80000000L


let coll_icflag icflag flag = Int64.logor (int_of_cflag flag) icflag
let cflags flags = List.fold_left coll_icflag 0L flags

let cflag_of_int = function
  | 0x00000001L -> `ALLOW_EMPTY_CLASS
  | 0x00000002L -> `ALT_BSUX
  | 0x00000004L -> `AUTO_CALLOUT
  | 0x00000008L -> `CASELESS
  | 0x00000010L -> `DOLLAR_ENDONLY
  | 0x00000020L -> `DOTALL
  | 0x00000040L -> `DUPNAMES
  | 0x00000080L -> `EXTENDED
  | 0x00000100L -> `FIRSTLINE
  | 0x00000200L -> `MATCH_UNSET_BACKREF
  | 0x00000400L -> `MULTILINE
  | 0x00000800L -> `NEVER_UCP
  | 0x00001000L -> `NEVER_UTF
  | 0x00002000L -> `NO_AUTO_CAPTURE
  | 0x00004000L -> `NO_AUTO_POSSESS
  | 0x00008000L -> `NO_DOTSTAR_ANCHOR
  | 0x00010000L -> `NO_START_OPTIMIZE
  | 0x00020000L -> `UCP
  | 0x00040000L -> `UNGREEDY
  | 0x00080000L -> `UTF
  | 0x00100000L -> `NEVER_BACKSLASH_C
  | 0x00200000L -> `ALT_CIRCUMFLEX
  | 0x00400000L -> `ALT_VERBNAMES
  | 0x00800000L -> `USE_OFFSET_LIMIT
  | 0x01000000L -> `EXTENDED_MORE
  | 0x02000000L -> `LITERAL
  | 0x04000000L -> `MATCH_INVALID_UTF
  | 0x20000000L -> `ENDANCHORED
  | 0x40000000L -> `NO_UTF_CHECK
  | 0x80000000L -> `ANCHORED
  | _ -> failwith "Pcre2.cflag_list: unknown compilation flag"

let all_cflags =
  [
    0x00000001L; 0x00000002L; 0x00000004L; 0x00000008L;
    0x00000010L; 0x00000020L; 0x00000040L; 0x00000080L;
    0x00000100L; 0x00000200L; 0x00000400L; 0x00000800L;
    0x00001000L; 0x00002000L; 0x00004000L; 0x00008000L;
    0x00010000L; 0x00020000L; 0x00040000L; 0x00080000L;
    0x00100000L; 0x00200000L; 0x00400000L; 0x00800000L;
    0x01000000L; 0x02000000L; 0x04000000L;
    0x20000000L; 0x40000000L; 0x80000000L;
  ]

let cflag_list icflags =
  let coll flag_list flag =
    if Int64.equal (Int64.logand icflags flag) 0L then flag_list
    else cflag_of_int flag :: flag_list in
  List.fold_left coll [] all_cflags


(* Runtime flags *)

type rflag =
  [
  | `ANCHORED
  | `COPY_MATCHED_SUBJECT
  | `DFA_RESTART
  | `DFA_SHORTEST
  | `ENDANCHORED
  | `NOTBOL
  | `NOTEOL
  | `NOTEMPTY
  | `NOTEMPTY_ATSTART
  | `NO_JIT
  | `NO_UTF_CHECK
  | `PARTIAL_HARD
  | `PARTIAL_SOFT
  ]

let int_of_rflag = function
  | `NOTBOL -> 0x00000001L
  | `NOTEOL -> 0x00000002L
  | `NOTEMPTY ->  0x00000004L
  | `NOTEMPTY_ATSTART -> 0x00000008L
  | `PARTIAL_SOFT -> 0x00000010L
  | `PARTIAL_HARD -> 0x00000020L
  | `DFA_RESTART -> 0x00000040L
  | `DFA_SHORTEST -> 0x00000080L
  | `NO_JIT -> 0x00002000L
  | `COPY_MATCHED_SUBJECT -> 0x00004000L
  | `ENDANCHORED -> 0x20000000L
  | `NO_UTF_CHECK -> 0x40000000L
  | `ANCHORED -> 0x80000000L

let coll_irflag irflag flag = Int64.logor (int_of_rflag flag) irflag
let rflags flags = List.fold_left coll_irflag 0L flags

let rflag_of_int = function
  | 0x00000001L -> `NOTBOL
  | 0x00000002L -> `NOTEOL
  | 0x00000004L -> `NOTEMPTY
  | 0x00000008L -> `NOTEMPTY_ATSTART
  | 0x00000010L -> `PARTIAL_SOFT
  | 0x00000020L -> `PARTIAL_HARD
  | 0x00000040L -> `DFA_RESTART
  | 0x00000080L -> `DFA_SHORTEST
  | 0x00002000L -> `NO_JIT
  | 0x00004000L -> `COPY_MATCHED_SUBJECT
  | 0x20000000L -> `ENDANCHORED
  | 0x40000000L -> `NO_UTF_CHECK
  | 0x80000000L -> `ANCHORED
  | _ -> failwith "Pcre2.rflag_list: unknown runtime flag"

let all_rflags =
  [
    0x00000001L; 0x00000002L; 0x00000004L; 0x00000008L;
    0x00000010L; 0x00000020L; 0x00000040L; 0x00000080L;
    0x00002000L; 0x00004000L;
    0x20000000L; 0x40000000L; 0x80000000L;
  ]

let rflag_list irflags =
  let coll flag_list flag =
    if Int64.equal (Int64.logand irflags flag) 0L then flag_list
    else rflag_of_int flag :: flag_list in
  List.fold_left coll [] all_rflags


(* Information on the PCRE2-configuration (build-time options) *)

external pcre2_version : unit -> string = "pcre2_version_stub"

external pcre2_config_unicode : unit -> bool
  = "pcre2_config_unicode_stub" [@@noalloc]

external pcre2_config_newline : unit -> char
  = "pcre2_config_newline_stub" [@@noalloc]

external pcre2_config_link_size : unit -> (int [@untagged])
  = "pcre2_config_link_size_stub_bc" "pcre2_config_link_size_stub" [@@noalloc]

external pcre2_config_match_limit : unit -> (int [@untagged])
  = "pcre2_config_match_limit_stub_bc" "pcre2_config_match_limit_stub"
  [@@noalloc]

external pcre2_config_depth_limit : unit -> (int [@untagged])
  = "pcre2_config_depth_limit_stub_bc" "pcre2_config_depth_limit_stub"
  [@@noalloc]

external pcre2_config_stackrecurse :
  unit -> bool = "pcre2_config_stackrecurse_stub" [@@noalloc]

let version = pcre2_version ()
let config_unicode = pcre2_config_unicode ()
let config_newline = pcre2_config_newline ()
let config_link_size = pcre2_config_link_size ()
let config_match_limit = pcre2_config_match_limit ()
let config_depth_limit = pcre2_config_depth_limit ()
let config_stackrecurse = pcre2_config_stackrecurse ()


(* Information on patterns *)

type firstcodeunit_info =
  [ `Char of char
  | `Start_only
  | `ANCHORED ]

type regexp

external options : regexp -> (icflag [@unboxed])
  = "pcre2_argoptions_stub_bc" "pcre2_argoptions_stub"

external size : regexp -> (int [@untagged])
  = "pcre2_size_stub_bc" "pcre2_size_stub"

external capturecount : regexp -> (int [@untagged])
  = "pcre2_capturecount_stub_bc" "pcre2_capturecount_stub"

external backrefmax : regexp -> (int [@untagged])
  = "pcre2_backrefmax_stub_bc" "pcre2_backrefmax_stub"

external namecount : regexp -> (int [@untagged])
  = "pcre2_namecount_stub_bc" "pcre2_namecount_stub"

external nameentrysize : regexp -> (int [@untagged])
  = "pcre2_nameentrysize_stub_bc" "pcre2_nameentrysize_stub"

external names : regexp -> string array = "pcre2_names_stub"
external firstcodeunit : regexp -> firstcodeunit_info = "pcre2_firstcodeunit_stub"
external lastcodeunit : regexp -> char option = "pcre2_lastcodeunit_stub"

(* Compilation of patterns *)

type chtables

external maketables : unit -> chtables = "pcre2_maketables_stub"

external compile : (icflag [@unboxed]) -> chtables option -> string -> regexp
  = "pcre2_compile_stub_bc" "pcre2_compile_stub"

(* external get_match_limit : regexp -> int option = "pcre2_get_match_limit_stub" *)

(* Internal use only! *)
external set_imp_match_limit : regexp -> (int [@untagged]) -> regexp
  = "pcre2_set_imp_match_limit_stub_bc" "pcre2_set_imp_match_limit_stub"
  [@@noalloc]

(* external get_depth_limit :
  regexp -> int option = "pcre2_get_depth_limit_stub" *)

(* Internal use only! *)
external set_imp_depth_limit : regexp -> (int [@untagged]) -> regexp
  = "pcre2_set_imp_depth_limit_stub_bc" "pcre2_set_imp_depth_limit_stub"
  [@@noalloc]

(* TODO implement jit using new pcre2_jit_compile api *)
let regexp
      (* ?(jit_compile = false) *)
      ?limit ?depth_limit
      ?(iflags = 0L) ?flags ?chtables pat =
  let rex =
    match flags with
    | Some flag_list -> compile (cflags flag_list) chtables pat
    | _ -> compile iflags chtables pat
  in
  let rex =
    match limit with
    | None -> rex
    | Some lim -> set_imp_match_limit rex lim
  in
  match depth_limit with
  | None -> rex
  | Some lim -> set_imp_depth_limit rex lim

let regexp_or
      (* ?jit_compile *) ?limit ?depth_limit ?(iflags = 0L) ?flags ?chtables pats =
  let check pat =
    try ignore (regexp ~iflags ?flags ?chtables pat)
    with Error error -> raise (Regexp_or (pat, error))
  in
  List.iter check pats;
  let big_pat =
    let cnv pat = "(?:" ^ pat ^ ")" in
    String.concat "|" (List.rev (List.rev_map cnv pats))
  in
  regexp (* ?jit_compile *) ?limit ?depth_limit ~iflags ?flags ?chtables big_pat

let bytes_unsafe_blit_string str str_ofs bts bts_ofs len =
  let str_bts = Bytes.unsafe_of_string str in
  Bytes.unsafe_blit str_bts str_ofs bts bts_ofs len

let string_unsafe_sub str ofs len =
  let res = Bytes.create len in
  bytes_unsafe_blit_string str ofs res 0 len;
  Bytes.unsafe_to_string res

let quote s =
  let len = String.length s in
  let buf = Bytes.create (len lsl 1) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | '\\' | '^' | '$' | '.' | '[' | '|'
    | '('  | ')' | '?' | '*' | '+' | '{' as c ->
      Bytes.unsafe_set buf !pos '\\';
      incr pos;
      Bytes.unsafe_set buf !pos c;
      incr pos
    | c -> Bytes.unsafe_set buf !pos c; incr pos
  done;
  string_unsafe_sub (Bytes.unsafe_to_string buf) 0 !pos


(* Matching of patterns and subpattern extraction *)

(* Default regular expression when none is provided by the user *)
let def_rex = regexp (* ~jit_compile:true *) "\\s+"

type substrings = string * int array

type callout_data =
  {
    callout_number : int;
    substrings : substrings;
    start_match : int;
    current_position : int;
    capture_top : int;
    capture_last : int;
    pattern_position : int;
    next_item_length : int;
  }

type callout = callout_data -> unit

let get_subject (subj, _) = subj

let num_of_subs (_, ovector) = Array.length ovector / 3

let get_offset_start ovector str_num =
  if str_num < 0 || str_num >= Array.length ovector / 3 then
    invalid_arg "Pcre2.get_offset_start: illegal offset";
  let offset = str_num lsl 1 in
  offset, Array.unsafe_get ovector offset

let get_substring_aux (subj, ovector) offset start =
  if start < 0 then raise Not_found
  else
    string_unsafe_sub subj start (Array.unsafe_get ovector (offset + 1) - start)

let get_substring (_, ovector as substrings) str_num =
  let offset, start = get_offset_start ovector str_num in
  get_substring_aux substrings offset start

let get_substring_ofs (_subj, ovector) str_num =
  let offset, start = get_offset_start ovector str_num in
  if start < 0 then raise Not_found
  else start, Array.unsafe_get ovector (offset + 1)

let unsafe_get_substring (_, ovector as substrings) str_num =
  let offset = str_num lsl 1 in
  try get_substring_aux substrings offset (Array.unsafe_get ovector offset)
  with Not_found -> ""

let get_substrings ?(full_match = true) (_, ovector as substrings) =
  if full_match then
    Array.init (Array.length ovector / 3) (unsafe_get_substring substrings)
  else
    let len = (Array.length ovector / 3) - 1 in
    Array.init len (fun n -> unsafe_get_substring substrings (n + 1))

let unsafe_get_opt_substring (_, ovector as substrings) str_num =
  let offset = str_num lsl 1 in
  try
    let start = Array.unsafe_get ovector offset in
    let str = get_substring_aux substrings offset start in
    Some str
  with Not_found -> None

let get_opt_substrings ?(full_match = true) (_, ovector as substrings) =
  if full_match then
    Array.init (Array.length ovector / 3) (unsafe_get_opt_substring substrings)
  else
    let len = (Array.length ovector / 3) - 1 in
    Array.init len (fun n -> unsafe_get_opt_substring substrings (n + 1))

external get_stringnumber : regexp -> string -> (int [@untagged])
  =
  "pcre2_substring_number_from_name_stub_bc"
  "pcre2_substring_number_from_name_stub"

let get_named_substring rex name substrings =
  get_substring substrings (get_stringnumber rex name)

let get_named_substring_ofs rex name substrings =
  get_substring_ofs substrings (get_stringnumber rex name)

external unsafe_pcre2_match :
  (irflag [@unboxed]) ->
  regexp ->
  pos : (int [@untagged]) ->
  subj_start : (int [@untagged]) ->
  subj : string ->
  int array ->
  callout option ->
  unit = "pcre2_match_stub_bc" "pcre2_match_stub"

let make_ovector rex =
  let subgroups1 = capturecount rex + 1 in
  let subgroups2 = subgroups1 lsl 1 in
  subgroups2, Array.make (subgroups1 + subgroups2) 0

external unsafe_pcre2_dfa_match :
  (irflag [@unboxed]) ->
  regexp ->
  pos : (int [@untagged]) ->
  subj_start : (int [@untagged]) ->
  subj : string ->
  int array ->
  callout option ->
  workspace : int array ->
  unit = "pcre2_dfa_match_stub_bc" "pcre2_match_stub0"

let pcre2_dfa_match ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat ?(pos = 0)
                  ?callout ?(workspace = Array.make 20 0) subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let _, ovector = make_ovector rex in
  unsafe_pcre2_dfa_match
    iflags rex ~pos ~subj_start:0 ~subj ovector callout ~workspace;
  ovector

let pcre2_match ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat ?(pos = 0)
              ?callout subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let _, ovector = make_ovector rex in
  unsafe_pcre2_match iflags rex ~pos ~subj_start:0 ~subj ovector callout;
  ovector

let exec ?iflags ?flags ?rex ?pat ?pos ?callout subj =
  subj, pcre2_match ?iflags ?flags ?rex ?pat ?pos ?callout subj

let next_match ?iflags ?flags ?rex ?pat ?(pos = 0) ?callout (subj, ovector) =
  let pos = Array.unsafe_get ovector 1 + pos in
  let subj_len = String.length subj in
  if pos < 0 || pos > subj_len then
    invalid_arg "Pcre2.next_match: illegal offset";
  subj, pcre2_match ?iflags ?flags ?rex ?pat ~pos ?callout subj

let rec copy_lst ar n = function
  | [] -> ar
  | h :: t -> Array.unsafe_set ar n h; copy_lst ar (n - 1) t

let exec_all ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat ?pos ?callout subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let (_, ovector as sstrs) = exec ~iflags ~rex ?pos ?callout subj in
  let null_flags = Int64.logor iflags 0x00000004L in (* `NOTEMPTY *)
  let subj_len = String.length subj in
  let rec loop pos (subj, ovector as sstrs) n lst =
    let maybe_ovector =
      try
        let first = Array.unsafe_get ovector 0 in
        if first = pos && Array.unsafe_get ovector 1 = pos then
          if pos = subj_len then None
          else Some (pcre2_match ~iflags:null_flags ~rex ~pos ?callout subj)
        else Some (pcre2_match ~iflags ~rex ~pos ?callout subj)
      with Not_found -> None in
    match maybe_ovector with
    | Some ovector ->
        let new_pos = Array.unsafe_get ovector 1 in
        loop new_pos (subj, ovector) (n + 1) (sstrs :: lst)
    | None -> copy_lst (Array.make (n + 1) sstrs) (n - 1) lst in
  loop (Array.unsafe_get ovector 1) sstrs 0 []

let extract ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj =
  get_substrings ?full_match (exec ?iflags ?flags ?rex ?pat ?pos ?callout subj)

let extract_opt ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj =
  get_opt_substrings
    ?full_match (exec ?iflags ?flags ?rex ?pat ?pos ?callout subj)

let extract_all ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj =
  let many_sstrs = exec_all ?iflags ?flags ?rex ?pat ?pos ?callout subj in
  Array.map (get_substrings ?full_match) many_sstrs

let extract_all_opt ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj =
  let many_sstrs = exec_all ?iflags ?flags ?rex ?pat ?pos ?callout subj in
  Array.map (get_opt_substrings ?full_match) many_sstrs

let pmatch ?iflags ?flags ?rex ?pat ?pos ?callout subj =
  try ignore (pcre2_match ?iflags ?flags ?rex ?pat ?pos ?callout subj); true
  with Not_found -> false


(* String substitution *)

(* Elements of a substitution pattern *)
type subst =
  | SubstString of int * int (* Denotes a substring in the substitution *)
  | Backref of int           (* nth backreference ($0 is program name!) *)
  | Match                    (* The whole matched string *)
  | PreMatch                 (* The string before the match *)
  | PostMatch                (* The string after the match *)
  | LastParenMatch           (* The last matched group *)

(* Information on substitution patterns *)
type substitution = string     (* The substitution string *)
                  * int        (* Highest group number of backreferences *)
                  * bool       (* Makes use of "LastParenMatch" *)
                  * subst list (* The list of substitution elements *)

(* Only used internally in "subst" *)
exception FoundAt of int

let zero = Char.code '0'

let subst str =
  let max_br = ref 0 in
  let with_lp = ref false in
  let lix = String.length str - 1 in
  let rec loop acc n =
    if lix < n then acc
    else
      try
        for i = n to lix do
          if String.unsafe_get str i = '$' then raise (FoundAt i)
        done;
        SubstString (n, lix - n + 1) :: acc
      with FoundAt i ->
        if i = lix then SubstString (n, lix - n + 1) :: acc
        else
          let i1 = i + 1 in
          let acc = if n = i then acc else SubstString (n, i - n) :: acc in
          match String.unsafe_get str i1 with
          | '0'..'9' as c ->
              let subpat_nr = ref (Char.code c - zero) in
              (try
                for j = i1 + 1 to lix do
                  let c = String.unsafe_get str j in
                  if c >= '0' && c <= '9' then
                    subpat_nr := 10 * !subpat_nr + Char.code c - zero
                  else raise (FoundAt j)
                done;
                max_br := max !subpat_nr !max_br;
                Backref !subpat_nr :: acc
              with FoundAt j ->
                max_br := max !subpat_nr !max_br;
                loop (Backref !subpat_nr :: acc) j)
          | '!'  -> loop acc (i1 + 1)
          | '$'  -> loop (SubstString (i1, 1) :: acc) (i1 + 1)
          | '&'  -> loop (Match :: acc) (i1 + 1)
          | '`'  -> loop (PreMatch :: acc) (i1 + 1)
          | '\'' -> loop (PostMatch :: acc) (i1 + 1)
          | '+'  ->
              with_lp := true;
              loop (LastParenMatch :: acc) (i1 + 1)
          | _    -> loop acc i1 in
  let subst_lst = loop [] 0 in
  str, !max_br, !with_lp, subst_lst

let def_subst = subst ""

(* Calculates a list of tuples (str, offset, len) which contain
   substrings to be copied on substitutions. Internal use only! *)
let calc_trans_lst subgroups2 ovector subj templ subst_lst =
  let prefix_len = Array.unsafe_get ovector 0 in
  let last = Array.unsafe_get ovector 1 in
  let coll (res_len, trans_lst as accu) =
    let return_lst (_str, _ix, len as el) =
      if len = 0 then accu else res_len + len, el :: trans_lst in
    function
    | SubstString (ix, len) -> return_lst (templ, ix, len)
    | Backref 0 ->
        let prog_name = Sys.argv.(0) in
        return_lst (prog_name, 0, String.length prog_name)
    | Backref n ->
        let offset = n lsl 1 in
        let start = Array.unsafe_get ovector offset in
        let len = Array.unsafe_get ovector (offset + 1) - start in
        return_lst (subj, start, len)
    | Match -> return_lst (subj, prefix_len, last - prefix_len)
    | PreMatch -> return_lst (subj, 0, prefix_len)
    | PostMatch -> return_lst (subj, last, String.length subj - last)
    | LastParenMatch ->
        let subgroups2_2 = subgroups2 - 2 in
        let pos = ref subgroups2_2 in
        let ix = ref (Array.unsafe_get ovector subgroups2_2) in
        while !ix < 0 do
          let pos_2 = !pos - 2 in
          pos := pos_2;
          ix := Array.unsafe_get ovector pos_2
        done;
        return_lst (subj, !ix, Array.unsafe_get ovector (!pos + 1) - !ix) in
  List.fold_left coll (0, []) subst_lst

let replace ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat
            ?(pos = 0) ?(itempl = def_subst) ?templ ?callout subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let templ, max_br, with_lp, subst_lst =
    match templ with
    | Some str -> subst str
    | _ -> itempl in
  let subj_len = String.length subj in
  if pos < 0 || pos > subj_len then invalid_arg "Pcre2.replace: illegal offset";
  let subgroups2, ovector = make_ovector rex in
  let nsubs = (subgroups2 lsr 1) - 1 in
  if max_br > nsubs then
    failwith "Pcre2.replace: backreference denotes nonexistent subpattern";
  if with_lp && nsubs = 0 then failwith "Pcre2.replace: no backreferences";
  let rec loop full_len trans_lsts cur_pos =
    if
      cur_pos > subj_len ||
      try
        unsafe_pcre2_match
          iflags rex ~pos:cur_pos ~subj_start:0 ~subj
          ovector callout;
        false
      with Not_found -> true
    then
      let postfix_len = max (subj_len - cur_pos) 0 in
      let left = pos + full_len in
      let res = Bytes.create (left + postfix_len) in
      bytes_unsafe_blit_string subj 0 res 0 pos;
      bytes_unsafe_blit_string subj cur_pos res left postfix_len;
      let inner_coll ofs (templ, ix, len) =
        bytes_unsafe_blit_string templ ix res ofs len; ofs + len in
      let coll ofs (res_len, trans_lst) =
        let new_ofs = ofs - res_len in
        let _ = List.fold_left inner_coll new_ofs trans_lst in
        new_ofs in
      let _ = List.fold_left coll left trans_lsts in
      Bytes.unsafe_to_string res
    else
      let first = Array.unsafe_get ovector 0 in
      let len = first - cur_pos in
      let res_len, _ as trans_lst_el =
        calc_trans_lst subgroups2 ovector subj templ subst_lst in
      let trans_lsts =
        if len > 0 then
          trans_lst_el :: (len, [(subj, cur_pos, len)]) :: trans_lsts
        else trans_lst_el :: trans_lsts in
      let full_len = full_len + len + res_len in
      let next = first + 1 in
      let last = Array.unsafe_get ovector 1 in
      if last < next then
        if first < subj_len then
          let new_trans_lsts = (1, [(subj, cur_pos + len, 1)]) :: trans_lsts in
          loop (full_len + 1) new_trans_lsts next
        else loop full_len trans_lsts next
      else loop full_len trans_lsts last in
  loop 0 [] pos

let qreplace ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat
             ?(pos = 0) ?(templ = "") ?callout subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let subj_len = String.length subj in
  if pos < 0 || pos > subj_len then invalid_arg "Pcre2.qreplace: illegal offset";
  let templ_len = String.length templ in
  let _, ovector = make_ovector rex in
  let rec loop full_len subst_lst cur_pos =
    if
      cur_pos > subj_len ||
      try
        unsafe_pcre2_match
          iflags rex ~pos:cur_pos ~subj_start:0 ~subj ovector callout;
        false
      with Not_found -> true
    then
      let postfix_len = max (subj_len - cur_pos) 0 in
      let left = pos + full_len in
      let res = Bytes.create (left + postfix_len) in
      bytes_unsafe_blit_string subj 0 res 0 pos;
      bytes_unsafe_blit_string subj cur_pos res left postfix_len;
      let coll ofs = function
        | Some (substr, ix, len) ->
            let new_ofs = ofs - len in
            bytes_unsafe_blit_string substr ix res new_ofs len;
            new_ofs
        | None ->
            let new_ofs = ofs - templ_len in
            bytes_unsafe_blit_string templ 0 res new_ofs templ_len;
            new_ofs in
      let _ = List.fold_left coll left subst_lst in
      Bytes.unsafe_to_string res
    else
      let first = Array.unsafe_get ovector 0 in
      let len = first - cur_pos in
      let subst_lst =
        if len > 0 then None :: Some (subj, cur_pos, len) :: subst_lst
        else None :: subst_lst in
      let last = Array.unsafe_get ovector 1 in
      let full_len = full_len + len + templ_len in
      let next = first + 1 in
      if last < next then
        if first < subj_len then
          loop (full_len + 1) (Some (subj, cur_pos + len, 1) :: subst_lst) next
        else loop full_len subst_lst next
      else loop full_len subst_lst last in
  loop 0 [] pos

let substitute_substrings ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat
                          ?(pos = 0) ?callout ~subst subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let subj_len = String.length subj in
  if pos < 0 || pos > subj_len then invalid_arg "Pcre2.substitute: illegal offset";
  let _, ovector = make_ovector rex in
  let rec loop full_len subst_lst cur_pos =
    if
      cur_pos > subj_len ||
      try
        unsafe_pcre2_match
          iflags rex ~pos:cur_pos ~subj_start:0 ~subj ovector callout;
        false
      with Not_found -> true
    then
      let postfix_len = max (subj_len - cur_pos) 0 in
      let left = pos + full_len in
      let res = Bytes.create (left + postfix_len) in
      bytes_unsafe_blit_string subj 0 res 0 pos;
      bytes_unsafe_blit_string subj cur_pos res left postfix_len;
      let coll ofs (templ, ix, len) =
        let new_ofs = ofs - len in
        bytes_unsafe_blit_string templ ix res new_ofs len;
        new_ofs in
      let _ = List.fold_left coll left subst_lst in
      Bytes.unsafe_to_string res
    else
      let first = Array.unsafe_get ovector 0 in
      let len = first - cur_pos in
      let templ = subst (subj, ovector) in
      let templ_len = String.length templ in
      let subst_lst =
        if len > 0 then
          (templ, 0, templ_len) :: (subj, cur_pos, len) :: subst_lst
        else (templ, 0, templ_len) :: subst_lst in
      let last = Array.unsafe_get ovector 1 in
      let full_len = full_len + len + templ_len in
      let next = first + 1 in
      if last < next then
        if first < subj_len then
          loop (full_len + 1) ((subj, cur_pos + len, 1) :: subst_lst) next
        else loop full_len subst_lst next
      else loop full_len subst_lst last in
  loop 0 [] pos

let substitute ?iflags ?flags ?rex ?pat ?pos ?callout ~subst:str_subst subj =
  let subst (subj, ovector) =
    let first = Array.unsafe_get ovector 0 in
    let last = Array.unsafe_get ovector 1 in
    str_subst (string_unsafe_sub subj first (last - first)) in
  substitute_substrings ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj

let replace_first ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat ?(pos = 0)
                  ?(itempl = def_subst) ?templ ?callout subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let templ, max_br, with_lp, subst_lst =
    match templ with
    | Some str -> subst str
    | _ -> itempl in
  let subgroups2, ovector = make_ovector rex in
  let nsubs = (subgroups2 lsr 1) - 1 in
  if max_br > nsubs then
    failwith "Pcre2.replace_first: backreference denotes nonexistent subpattern";
  if with_lp && nsubs = 0 then failwith "Pcre2.replace_first: no backreferences";
  try
    unsafe_pcre2_match iflags rex ~pos ~subj_start:0 ~subj ovector callout;
    let res_len, trans_lst =
      calc_trans_lst subgroups2 ovector subj templ subst_lst in
    let first = Array.unsafe_get ovector 0 in
    let last = Array.unsafe_get ovector 1 in
    let rest = String.length subj - last in
    let res = Bytes.create (first + res_len + rest) in
    bytes_unsafe_blit_string subj 0 res 0 first;
    let coll ofs (templ, ix, len) =
      bytes_unsafe_blit_string templ ix res ofs len; ofs + len in
    let ofs = List.fold_left coll first trans_lst in
    bytes_unsafe_blit_string subj last res ofs rest;
    Bytes.unsafe_to_string res
  with Not_found -> subj

let qreplace_first ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat
                   ?(pos = 0) ?(templ = "") ?callout subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let _, ovector = make_ovector rex in
  try
    unsafe_pcre2_match iflags rex ~pos ~subj_start:0 ~subj ovector callout;
    let first = Array.unsafe_get ovector 0 in
    let last = Array.unsafe_get ovector 1 in
    let len = String.length templ in
    let rest = String.length subj - last in
    let postfix_start = first + len in
    let res = Bytes.create (postfix_start + rest) in
    bytes_unsafe_blit_string subj 0 res 0 first;
    bytes_unsafe_blit_string templ 0 res first len;
    bytes_unsafe_blit_string subj last res postfix_start rest;
    Bytes.unsafe_to_string res
  with Not_found -> subj

let substitute_substrings_first ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat
                                ?(pos = 0) ?callout ~subst subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let _, ovector = make_ovector rex in
  try
    unsafe_pcre2_match iflags rex ~pos ~subj_start:0 ~subj ovector callout;
    let subj_len = String.length subj in
    let prefix_len = Array.unsafe_get ovector 0 in
    let last = Array.unsafe_get ovector 1 in
    let templ = subst (subj, ovector) in
    let postfix_len = subj_len - last in
    let templ_len = String.length templ in
    let postfix_start = prefix_len + templ_len in
    let res = Bytes.create (postfix_start + postfix_len) in
    bytes_unsafe_blit_string subj 0 res 0 prefix_len;
    bytes_unsafe_blit_string templ 0 res prefix_len templ_len;
    bytes_unsafe_blit_string subj last res postfix_start postfix_len;
    Bytes.unsafe_to_string res
  with Not_found -> subj

let substitute_first ?iflags ?flags ?rex ?pat ?pos
                     ?callout ~subst:str_subst subj =
  let subst (subj, ovector) =
    let first = Array.unsafe_get ovector 0 in
    let last = Array.unsafe_get ovector 1 in
    str_subst (string_unsafe_sub subj first (last - first)) in
  substitute_substrings_first
    ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj


(* Splitting *)

let internal_psplit flags rex max pos callout subj =
  let subj_len = String.length subj in
  if subj_len = 0 then []
  else if max = 1 then [subj]
  else
    let subgroups2, ovector = make_ovector rex in

    (* Adds contents of subgroups to the string accumulator *)
    let handle_subgroups strs =
      let strs = ref strs in
      let i = ref 2 in
      while !i < subgroups2 do
        let first = Array.unsafe_get ovector !i in
        incr i;
        let last = Array.unsafe_get ovector !i in
        let str =
          if first < 0 then ""
          else string_unsafe_sub subj first (last - first) in
        strs := str :: !strs; incr i
      done;
      !strs in

    (* Performs the recursive split *)
    let rec loop strs cnt pos prematch =
      let len = subj_len - pos in
      if len < 0 then strs
      else
        (* Checks termination due to max restriction *)
        if cnt = 0 then
          if prematch &&
            try
              unsafe_pcre2_match
                flags rex ~pos ~subj_start:pos ~subj ovector callout;
              true
            with Not_found -> false
          then
            let last = Array.unsafe_get ovector 1 in
            let strs = handle_subgroups strs in
            string_unsafe_sub subj last (subj_len - last) :: strs
          else string_unsafe_sub subj pos len :: strs

        (* Calculates next accumulator state for splitting *)
        else
          if
            try
              unsafe_pcre2_match
                flags rex ~pos ~subj_start:pos ~subj ovector callout;
              false
            with Not_found -> true
          then string_unsafe_sub subj pos len :: strs
          else
            let first = Array.unsafe_get ovector 0 in
            let last = Array.unsafe_get ovector 1 in
            if first = pos then
              if last = pos then
                let strs = if prematch then handle_subgroups strs else strs in
                if len = 0 then "" :: strs
                else if
                  try
                    unsafe_pcre2_match
                      (* `ANCHORED | `NOTEMPTY *)
                      (Int64.logor flags 0x80000004L) rex ~pos ~subj_start:pos ~subj
                      ovector callout;
                    true
                  with Not_found -> false
                then
                  let new_strs = handle_subgroups ("" :: strs) in
                  loop new_strs (cnt - 1) (Array.unsafe_get ovector 1) false
                else
                  let new_strs = string_unsafe_sub subj pos 1 :: strs in
                  loop new_strs (cnt - 1) (pos + 1) true
              else
                if prematch then loop (handle_subgroups strs) cnt last false
                else loop (handle_subgroups ("" :: strs)) (cnt - 1) last false
            else
              let new_strs = string_unsafe_sub subj pos (first - pos) :: strs in
              loop (handle_subgroups new_strs) (cnt - 1) last false in
    loop [] (max - 1) pos false

let rec strip_all_empty = function "" :: t -> strip_all_empty t | l -> l

external isspace : char -> bool = "pcre2_isspace_stub" [@@noalloc]

let rec find_no_space ix len str =
  if ix = len || not (isspace (String.unsafe_get str ix)) then ix
  else find_no_space (ix + 1) len str

let split ?(iflags = 0L) ?flags ?rex ?pat ?(pos = 0) ?(max = 0) ?callout subj =
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let res =
    match pat, rex with
    | Some str, _ -> internal_psplit iflags (regexp str) max pos callout subj
    | _, Some rex -> internal_psplit iflags rex max pos callout subj
    | _ ->
        (* special case for Perl-splitting semantics *)
        let len = String.length subj in
        if pos > len || pos < 0 then failwith "Pcre2.split: illegal offset";
        let new_pos = find_no_space pos len subj in
        internal_psplit iflags def_rex max new_pos callout subj in
  List.rev (if max = 0 then strip_all_empty res else res)

let asplit ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj =
  Array.of_list (split ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj)


(* Full splitting *)

type split_result = Text of string
                  | Delim of string
                  | Group of int * string
                  | NoGroup

let rec strip_all_empty_full = function
  | Delim _ :: rest -> strip_all_empty_full rest
  | l -> l

let full_split ?(iflags = 0L) ?flags ?(rex = def_rex) ?pat
               ?(pos = 0) ?(max = 0) ?callout subj =
  let rex = match pat with Some str -> regexp str | _ -> rex in
  let iflags = match flags with Some flags -> rflags flags | _ -> iflags in
  let subj_len = String.length subj in
  if subj_len = 0 then []
  else if max = 1 then [Text (subj)]
  else
    let subgroups2, ovector = make_ovector rex in

    (* Adds contents of subgroups to the string accumulator *)
    let handle_subgroups strs =
      let strs = ref strs in
      let i = ref 2 in
      while !i < subgroups2 do
        let group_nr = !i lsr 1 in
        let first = Array.unsafe_get ovector !i in
        incr i;
        let last = Array.unsafe_get ovector !i in
        let str =
          if first < 0 then NoGroup
          else
            let group_str = string_unsafe_sub subj first (last - first) in
            Group (group_nr, group_str) in
        strs := str :: !strs; incr i
      done;
      !strs in

    (* Performs the recursive split *)
    let rec loop strs cnt pos prematch =
      let len = subj_len - pos in
      if len < 0 then strs
      else
        (* Checks termination due to max restriction *)
        if cnt = 0 then
          if prematch &&
            try
              unsafe_pcre2_match
                iflags rex ~pos ~subj_start:pos ~subj ovector callout;
               true
            with Not_found -> false
          then
            let first = Array.unsafe_get ovector 0 in
            let last = Array.unsafe_get ovector 1 in
            let delim = Delim (string_unsafe_sub subj first (last - first)) in
            Text (string_unsafe_sub subj last (subj_len - last))
              :: handle_subgroups (delim :: strs)
          else
            if len = 0 then strs
            else Text (string_unsafe_sub subj pos len) :: strs

        (* Calculates next accumulator state for splitting *)
        else
          if
            try
              unsafe_pcre2_match
                iflags rex ~pos ~subj_start:pos ~subj ovector callout;
              false
            with Not_found -> true
          then
            if len = 0 then strs
            else Text (string_unsafe_sub subj pos len) :: strs
          else
            let first = Array.unsafe_get ovector 0 in
            let last = Array.unsafe_get ovector 1 in
            if first = pos then
              if last = pos then
                if len = 0 then handle_subgroups (Delim "" :: strs)
                else
                  let empty_groups = handle_subgroups [] in
                  if
                    try
                      unsafe_pcre2_match
                        (* `ANCHORED | `NOTEMPTY *)
                        (Int64.logor iflags 0x80000004L) rex ~pos ~subj_start:pos ~subj
                        ovector callout;
                      true
                    with Not_found -> false
                  then
                    let first = Array.unsafe_get ovector 0 in
                    let last = Array.unsafe_get ovector 1 in
                    let delim =
                      Delim (string_unsafe_sub subj first (last - first)) in
                    let new_strs =
                      handle_subgroups (
                        delim :: (if prematch then strs
                                  else empty_groups @ (Delim "" :: strs))) in
                    loop new_strs (cnt - 1) last false
                  else
                    let new_strs =
                      Text (string_unsafe_sub subj pos 1)
                        :: empty_groups @ Delim "" :: strs in
                    loop new_strs (cnt - 1) (pos + 1) true
              else
                  let delim =
                    Delim (string_unsafe_sub subj first (last - first)) in
                  loop (handle_subgroups (delim :: strs)) cnt last false
            else
              let delim = Delim (string_unsafe_sub subj first (last - first)) in
              let pre_strs =
                Text (string_unsafe_sub subj pos (first - pos)) :: strs in
              loop
                (handle_subgroups (delim :: pre_strs)) (cnt - 1) last false in
    let res = loop [] (max - 1) pos true in
    List.rev (if max = 0 then strip_all_empty_full res else res)


(* Additional convenience functions useful in combination with this library *)

let foreach_line ?(ic = stdin) f =
  try while true do f (input_line ic) done with End_of_file -> ()

let foreach_file filenames f =
  let do_with_file filename =
    let file = open_in filename in
    try f filename file; close_in file
    with exn -> close_in file; raise exn in
  List.iter do_with_file filenames
