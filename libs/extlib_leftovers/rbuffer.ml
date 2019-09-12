(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Ropes-based implementation of Buffer *)

type rope =
  | Str of string
  | App of rope * rope * int (* total length *)

let rope_empty = Str ""

let rope_length = function
  | Str s -> String.length s
  | App (_, _, n) -> n

let rec rope_nth i = function
  | Str s ->
      String.unsafe_get s i
  | App (l, r, _) ->
      let ll = rope_length l in
      if i < ll then rope_nth i l else rope_nth (i - ll) r

type t = {
  mutable rope : rope;     (* the left part is a rope *)
  mutable buffer : bytes; (* the right part is a buffer... *)
  mutable position : int;  (* ...with [position] bytes used *)
}

let create n =
  let n = if n < 1 then 1 else n in
  let n = if n > Sys.max_string_length then Sys.max_string_length else n in
  let s = Bytes.create n in
  { rope = rope_empty; buffer = s; position = 0; }

let reset b =
  b.rope <- rope_empty;
  b.position <- 0

let clear = reset

let length b =
  rope_length b.rope + b.position

(* [blit s i r] blits the contents of rope [r] in string [s] at index [i] *)
let rec blit_rope s i = function
  | Str str ->
      String.blit str 0 s i (String.length str)
  | App (l, r, _) ->
      let ll = rope_length l in
      blit_rope s i l;
      blit_rope s (i + ll) r

(* rename contents to unsafe_contents to avoid accidental usage *)
let unsafe_contents b =
  let r = rope_length b.rope in
  let n = b.position in
  let len = r + n in
  if len > Sys.max_string_length then invalid_arg "Rbuffer.contents";
  let s = Bytes.create len in
  blit_rope s 0 b.rope;
  Bytes.blit b.buffer 0 s r n;
  Bytes.unsafe_to_string s

(* [blit_subrope s i ofs len] blits the subrope [r[ofs..ofs+len-1]] in string
   [s] at index [i] *)
let rec blit_subrope s i ofs len = function
  | Str str ->
      assert (ofs >= 0 && ofs + len <= String.length str);
      String.blit str ofs s i len
  | App (l, r, _) ->
      let ll = rope_length l in
      if ofs + len <= ll then
	blit_subrope s i ofs len l
      else if ofs >= ll then
	blit_subrope s i (ofs - ll) len r
      else begin
	let lenl = ll - ofs in
	blit_subrope s i ofs lenl l;
	blit_subrope s (i + lenl) 0 (len - lenl) r
      end

let sub b ofs len =
  let r = rope_length b.rope in
  if len > Sys.max_string_length ||
     ofs < 0 || len < 0 || ofs > r + b.position - len
  then invalid_arg "Buffer.sub";
  let s = Bytes.create len in
  if ofs + len <= r then
    blit_subrope s 0 ofs len b.rope
  else if ofs >= r then
    Bytes.blit b.buffer (ofs - r) s 0 len
  else begin
    blit_subrope s 0 ofs (r - ofs) b.rope;
    Bytes.blit b.buffer 0 s (r - ofs) (ofs + len - r)
  end;
  Bytes.unsafe_to_string s

let nth b i =
  let r = rope_length b.rope in
  if i < 0 || i >= r + b.position then invalid_arg "Buffer.nth";
  if i < r then rope_nth i b.rope else Bytes.unsafe_get b.buffer (i - r)

(* moves the data in [b.buffer], if any, to the rope; ensures [b.position=0] *)
let move_buffer_to_rope b =
  let pos = b.position in
  if pos > 0 then begin
    let n = Bytes.length b.buffer in
    if pos = n then begin
      (* whole buffer goes to the rope; faster to allocate a new buffer *)
      b.rope <- App (b.rope, Str (Bytes.unsafe_to_string b.buffer), rope_length b.rope + pos);
      b.buffer <- Bytes.create n
    end else begin
      (* part of the buffer goes to the rope; easier to copy it *)
      b.rope <- App (b.rope, Str (Bytes.sub_string b.buffer 0 pos),
		     rope_length b.rope + pos)
    end;
    b.position <- 0
  end

let add_char b c =
  if b.position = Bytes.length b.buffer then move_buffer_to_rope b;
  let pos = b.position in
  Bytes.set b.buffer pos c;
  b.position <- pos + 1

(* allocates space for [len] bytes and returns the corresponding place
   (as a string and an offset within that string) *)
let alloc b len =
  let n = Bytes.length b.buffer in
  let pos = b.position in
  let len' = pos + len in
  if len' <= n then begin
    (* fits in the buffer *)
    b.position <- len';
    b.buffer, pos
  end else if len' <= Sys.max_string_length then begin
    (* buffer and len fit in a new string, allocated in the rope *)
    let str = Bytes.create len' in
    Bytes.blit b.buffer 0 str 0 pos;
    b.rope <- App (b.rope, Str (Bytes.unsafe_to_string str), rope_length b.rope + len');
    b.position <- 0;
    str, pos
  end else begin
    (* buffer and len require two strings, allocated in the rope *)
    let str = Bytes.create len in
    b.rope <- App (b.rope,
		   App (Str (Bytes.sub_string b.buffer 0 pos), Str (Bytes.unsafe_to_string str), len'),
		   rope_length b.rope + len');
    b.position <- 0;
    str, 0
  end

let safe_add_substring b s offset len =
  let str, pos = alloc b len in
  String.blit s offset str pos len

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Buffer.add_substring";
  safe_add_substring b s offset len

let add_string b s =
  safe_add_substring b s 0 (String.length s)

let add_buffer b b2 =
  if b.position > 0 then move_buffer_to_rope b;
  (* now we have b.position = 0 *)
  b.rope <- App (b.rope, b2.rope, rope_length b.rope + rope_length b2.rope);
  add_substring b (Bytes.unsafe_to_string b2.buffer) 0 b2.position

let rec add_channel b ic len =
  if len <= Sys.max_string_length then begin
    let str, pos = alloc b len in
    really_input ic str pos len
  end else begin
    let str, pos = alloc b Sys.max_string_length in
    really_input ic str pos Sys.max_string_length;
    add_channel b ic (len - Sys.max_string_length)
  end

let output_buffer oc b =
  let rec loop wl = match wl with
    | Str s :: wl ->
      output oc (Bytes.of_string s) 0 (String.length s);
      loop wl
    | App( l, r, _) :: wl ->
      loop (l :: r :: wl)
    | [] ->
      ()
  in
  loop [b.rope];
  output oc b.buffer 0 b.position

open Format

let print fmt b =
  let rec loop wl = match wl with
    | Str s :: wl ->
      pp_print_string fmt s;
      loop wl
    | App( l, r, _) :: wl ->
      loop (l :: r :: wl)
    | [] ->
      ()
  in
  loop [b.rope];
  pp_print_string fmt (Bytes.sub_string b.buffer 0 b.position)
