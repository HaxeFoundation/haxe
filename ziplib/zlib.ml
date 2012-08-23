(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                  adapted to Extc lib by Caue Waneck                 *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License, with     *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id: zlib.ml,v 1.4 2008/12/07 09:23:08 xleroy Exp $ *)

open Extc;;

let buffer_size = 1024

let polynom = 0xedb88320l

let crc_table = Array.init 256 (fun n ->
  let crc = ref (Int32.of_int n) in
  for j = 0 to 7 do
    crc := if Int32.to_int (Int32.logand (!crc) 1l) <> 0 then
      Int32.logxor (Int32.shift_right_logical (!crc) 1) polynom
    else
      Int32.shift_right_logical (!crc) 1;
  done;
  !crc) 

let max_wbits = 15

let compress ?(level = 6) ?(header = true) refill flush =
  let inbuf = String.create buffer_size
  and outbuf = String.create buffer_size in
  let zs = Extc.zlib_deflate_init2 level (if header then max_wbits else -max_wbits) in
  let rec compr inpos inavail =
    if inavail = 0 then begin
      let incount = refill inbuf in
      if incount = 0 then compr_finish() else compr 0 incount
    end else begin
      let res = Extc.zlib_deflate zs ~src:inbuf ~spos:inpos ~slen:inavail ~dst:outbuf ~dpos:0 ~dlen:buffer_size Z_NO_FLUSH in
      let used_in, used_out = res.z_read, res.z_wrote in
      flush outbuf used_out;
      compr (inpos + used_in) (inavail - used_in)
    end
  and compr_finish () =
    let ret = Extc.zlib_deflate zs ~src:inbuf ~spos:0 ~slen:0 ~dst:outbuf ~dpos:0 ~dlen:buffer_size Z_FINISH in
    let (finished, _, used_out) = ret.z_finish, ret.z_read, ret.z_wrote in
    flush outbuf used_out;
    if not finished then compr_finish()
  in
    compr 0 0;
    Extc.zlib_deflate_end zs

let compress_direct  ?(level = 6) ?(header = true) flush =
  let outbuf = String.create buffer_size in
  let zs = Extc.zlib_deflate_init2 level (if header then max_wbits else -max_wbits) in
  let rec compr inbuf inpos inavail =
    if inavail = 0 then ()
    else begin
      let res = Extc.zlib_deflate zs ~src:inbuf ~spos:inpos ~slen:inavail ~dst:outbuf ~dpos:0 ~dlen:buffer_size Z_NO_FLUSH in
      let used_in, used_out = res.z_read, res.z_wrote in
      flush outbuf used_out;
      compr inbuf (inpos + used_in) (inavail - used_in)
    end
  and compr_finish () =
    let ret = Extc.zlib_deflate zs ~src:"" ~spos:0 ~slen:0 ~dst:outbuf ~dpos:0 ~dlen:buffer_size Z_FINISH in
    let (finished, _, used_out) = ret.z_finish, (), ret.z_wrote in
    flush outbuf used_out;
    if not finished then compr_finish()
  in
  compr, compr_finish

let uncompress ?(header = true) refill flush =
  let inbuf = String.create buffer_size
  and outbuf = String.create buffer_size in
  let zs = Extc.zlib_inflate_init2 (if header then max_wbits else -max_wbits) in
  let rec uncompr inpos inavail =
    if inavail = 0 then begin
      let incount = refill inbuf in
      if incount = 0 then uncompr_finish true else uncompr 0 incount
    end else begin
      let ret = Extc.zlib_inflate zs ~src:inbuf ~spos: inpos ~slen:inavail ~dst:outbuf ~dpos:0 ~dlen:buffer_size Z_SYNC_FLUSH in
      let (finished, used_in, used_out) = ret.z_finish, ret.z_read, ret.z_wrote in
      flush outbuf used_out;
      if not finished then uncompr (inpos + used_in) (inavail - used_in)
    end
  and uncompr_finish first_finish =
    (* Gotcha: if there is no header, inflate requires an extra "dummy" byte
       after the compressed stream in order to complete decompression
       and return finished = true. *)
    let dummy_byte = if first_finish && not header then 1 else 0 in
    let ret = Extc.zlib_inflate zs ~src:inbuf ~spos:0 ~slen:dummy_byte ~dst:outbuf ~dpos:0 ~dlen:buffer_size Z_SYNC_FLUSH in
    let (finished, _, used_out) = ret.z_finish, ret.z_read, ret.z_wrote in
    flush outbuf used_out;
    if not finished then uncompr_finish false
  in
    uncompr 0 0;
    Extc.zlib_inflate_end zs

let update_crc crc buf pos len =
  let c = ref (Int32.lognot crc) in
  for i = pos to (len + pos - 1) do
    let b = Int32.of_int (int_of_char (String.get buf i)) in
    c := Int32.logxor (Array.get crc_table (Int32.to_int (Int32.logand (Int32.logxor !c b) 0xFFl))) (Int32.shift_right_logical !c 8);
  done;
  let ret = Int32.lognot !c in
  ret