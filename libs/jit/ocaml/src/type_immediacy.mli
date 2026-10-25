(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Immediacy status of a type *)

type t =
  | Unknown
  (** We don't know anything *)
  | Always
  (** We know for sure that values of this type are always immediate *)
  | Always_on_64bits
  (** We know for sure that values of this type are always immediate
      on 64 bit platforms. For other platforms, we know nothing. *)

module Violation : sig
  type t =
    | Not_always_immediate
    | Not_always_immediate_on_64bits
end

(** [coerce t ~as_] returns [Ok ()] iff [t] can be seen as type
    immediacy [as_]. For instance, [Always] can be seen as
    [Always_on_64bits] but the opposite is not true. Return [Error _]
    if the coercion is not possible. *)
val coerce : t -> as_:t -> (unit, Violation.t) result

(** Return the immediateness of a type as indicated by the user via
    attributes *)
val of_attributes : Parsetree.attributes -> t
