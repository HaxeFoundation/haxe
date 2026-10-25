(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Frederic Bour, Tarides                          *)
(*                         Thomas Refis, Tarides                          *)
(*                                                                        *)
(*   Copyright 2020 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module provides some facilities for creating references (and hash
    tables) which can easily be snapshotted and restored to an arbitrary
    version.

    It is used throughout the frontend (read: typechecker), to register all
    (well, hopefully) the global state. Thus making it easy for tools like
    Merlin to go back and forth typechecking different files. *)

(** {1 Creators} *)

val s_ref : 'a -> 'a ref
(** Similar to {!val:Stdlib.ref}, except the allocated reference is registered
    into the store. *)

val s_table : ('a -> 'b) -> 'a -> 'b ref
(** Used to register hash tables. Those also need to be placed into refs to be
    easily swapped out, but one can't just "snapshot" the initial value to
    create fresh instances, so instead an initializer is required.

    Use it like this:
    {[
      let my_table = s_table Hashtbl.create 42
    ]}
*)

(** {1 State management}

    Note: all the following functions are currently unused inside the compiler
    codebase. Merlin is their only user at the moment. *)

type store

val fresh : unit -> store
(** Returns a fresh instance of the store.

    The first time this function is called, it snapshots the value of all the
    registered references, later calls to [fresh] will return instances
    initialized to those values. *)

val with_store : store -> (unit -> 'a) -> 'a
(** [with_store s f] resets all the registered references to the value they have
    in [s] for the run of [f].
    If [f] updates any of the registered refs, [s] is updated to remember those
    changes. *)

val reset : unit -> unit
(** Resets all the references to the initial snapshot (i.e. to the same values
    that new instances start with). *)

val is_bound : unit -> bool
(** Returns [true] when a store is active (i.e. when called from the callback
    passed to {!with_store}), [false] otherwise. *)
