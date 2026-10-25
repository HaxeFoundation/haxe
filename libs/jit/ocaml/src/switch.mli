(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Luc Maranget, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*
  This module transforms generic switches in combinations
  of if tests and switches.
*)

(* For detecting action sharing, object style *)

(* Store for actions in object style:
  act_store : store an action, returns index in table
              In case an action with equal key exists, returns index
              of the stored action. Otherwise add entry in table.
  act_store_shared : This stored action will always be shared.
  act_get   : retrieve table
  act_get_shared : retrieve table, with sharing explicit
*)

type 'a shared = Shared of 'a | Single of 'a

type ('a, 'ctx) t_store =
    {act_get : unit -> 'a array ;
     act_get_shared : unit -> 'a shared array ;
     act_store : 'ctx -> 'a -> int ;
     act_store_shared : 'ctx -> 'a -> int ; }

module type Stored = sig
  type t
  type key
  val compare_key : key -> key -> int
  val make_key : t -> key option
end

module type CtxStored = sig
  include Stored
  type context
  val make_key : context -> t -> key option
end

module CtxStore(A:CtxStored) :
    sig
      val mk_store : unit -> (A.t, A.context) t_store
    end

module Store(A:Stored) :
    sig
      val mk_store : unit -> (A.t, unit) t_store
    end

(* Arguments to the Make functor *)
module type S =
  sig
    (* type of basic tests *)
    type primitive
    (* basic tests themselves *)
    val eqint : primitive
    val neint : primitive
    val leint : primitive
    val ltint : primitive
    val geint : primitive
    val gtint : primitive

    (* type of source locations *)
    type loc
    (* type of switch scrutinees *)
    type arg
    (* type of tests on scrutinees *)
    type test
    (* type of actions *)
    type act

    (* Various constructors, for making a binder,
        adding one integer, etc. *)

    (* [bind arg cont] should bind the expression arg to a variable,
       then call [cont] on that variable, and return the term made of
       the binding and the result of the call. *)
    val bind : arg -> (arg -> act) -> act
    (* [make_const n] generates a term for the integer constant [n] *)
    val make_const : int -> arg
    (* [make_offset arg n] generates a term for adding the constant
       integer [n] to the term [arg] *)
    val make_offset : arg -> int -> arg
    (* [make_prim p args] generates a test using the primitive operation [p]
       applied to arguments [args] *)
    val make_prim : primitive -> arg list -> test
    (* [make_isout h arg] generates a test that holds when [arg] is out of
       the interval [0, h] *)
    val make_isout : arg -> arg -> test
    (* [make_isin h arg] generates a test that holds when [arg] is in
       the interval [0, h] *)
    val make_isin : arg -> arg -> test
    (* [make_is_nonzero arg] generates a test that holds when [arg] is any
       value except 0 *)
    val make_is_nonzero : arg -> test
    (* [arg_as_test arg] casts [arg], known to be either 0 or 1,
       to a boolean test *)
    val arg_as_test : arg -> test
    (* [make_if cond ifso ifnot] generates a conditional branch *)
    val make_if : test -> act -> act -> act
   (* construct an actual switch :
      make_switch arg cases acts
      NB:  cases is in the value form *)
    val make_switch : loc -> arg -> int array -> act array -> act

   (* Build last minute sharing of action stuff *)
   val make_catch : act -> int * (act -> act)
   val make_exit : int -> act
  end


(*
  Make.zyva arg low high cases actions where
    - arg is the argument of the switch.
    - low, high are the interval limits.
    - cases is a list of sub-interval and action indices
    - actions is an array of actions.

  All these arguments specify a switch construct and zyva
  returns an action that performs the switch.
*)
module Make :
  functor (Arg : S) ->
    sig
(* Standard entry point, sharing is tracked *)
      val zyva :
          Arg.loc ->
          (int * int) ->
           Arg.arg ->
           (int * int * int) array ->
           (Arg.act, _) t_store ->
           Arg.act

(* Output test sequence, sharing tracked *)
     val test_sequence :
           Arg.arg ->
           (int * int * int) array ->
           (Arg.act, _) t_store ->
           Arg.act
    end
