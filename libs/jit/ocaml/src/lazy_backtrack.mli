(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a,'b) t

type log

val force : ('a -> 'b) -> ('a,'b) t -> 'b
val create : 'a -> ('a,'b) t
val get_arg : ('a,'b) t -> 'a option
val get_contents : ('a,'b) t -> ('a,'b) Either.t
val create_forced : 'b -> ('a, 'b) t
val create_failed : exn -> ('a, 'b) t

(* [force_logged log f t] is equivalent to [force f t] but if [f]
   returns [Error _] then [t] is recorded in [log]. [backtrack log]
   will then reset all the recorded [t]s back to their original
   state. *)
val log : unit -> log
val force_logged :
  log -> ('a -> ('b, 'c) result) -> ('a,('b, 'c) result) t -> ('b, 'c) result
val backtrack : log -> unit
