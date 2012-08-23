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

(* $Id: zlib.mli,v 1.2 2008/12/07 09:23:08 xleroy Exp $ *)

val compress:
  ?level: int -> ?header: bool -> 
  (string -> int) -> (string -> int -> unit) -> unit

val compress_direct:
  ?level: int -> ?header: bool -> (string -> int -> unit) ->
  (string -> int -> int -> unit) * (unit -> unit)

val uncompress:
  ?header: bool -> (string -> int) -> (string -> int -> unit) -> unit

val update_crc: 
  int32 -> string -> int -> int -> int32