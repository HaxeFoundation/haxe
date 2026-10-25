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

type t =
  | Unknown
  | Always
  | Always_on_64bits

module Violation = struct
  type t =
    | Not_always_immediate
    | Not_always_immediate_on_64bits
end

let coerce t ~as_ =
  match t, as_ with
  | _, Unknown
  | Always, Always
  | (Always | Always_on_64bits), Always_on_64bits -> Ok ()
  | (Unknown | Always_on_64bits), Always ->
      Error Violation.Not_always_immediate
  | Unknown, Always_on_64bits ->
      Error Violation.Not_always_immediate_on_64bits

let of_attributes attrs =
  match
    Builtin_attributes.immediate attrs,
    Builtin_attributes.immediate64 attrs
  with
  | true, _ -> Always
  | false, true -> Always_on_64bits
  | false, false -> Unknown
