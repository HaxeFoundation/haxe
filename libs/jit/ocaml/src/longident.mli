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

(** Long identifiers, used in parsetree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

  To print a longident, see {!Pprintast.longident}, using
    {!Format.asprintf} to convert to a string.

*)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten: t -> string list
val unflatten: string list -> t option
(** For a non-empty list [l], [unflatten l] is [Some lid] where [lid] is
    the long identifier created by concatenating the elements of [l]
    with [Ldot].
    [unflatten []] is [None].
*)

val last: t -> string
val parse: string -> t
[@@deprecated "this function may misparse its input,\n\
use \"Parse.longident\" or \"Longident.unflatten\""]
(**

   This function is broken on identifiers that are not just "Word.Word.word";
   for example, it returns incorrect results on infix operators
   and extended module paths.

   If you want to generate long identifiers that are a list of
   dot-separated identifiers, the function {!unflatten} is safer and faster.
   {!unflatten} is available since OCaml 4.06.0.

   If you want to parse any identifier correctly, use the long-identifiers
   functions from the {!Parse} module, in particular {!Parse.longident}.
   They are available since OCaml 4.11, and also provide proper
   input-location support.

*)
