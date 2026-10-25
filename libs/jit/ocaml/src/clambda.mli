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

(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

open Asttypes
open Lambda

type function_label = string

type ustructured_constant =
  | Uconst_float of float
  | Uconst_int32 of int32
  | Uconst_int64 of int64
  | Uconst_nativeint of nativeint
  | Uconst_block of int * uconstant list
  | Uconst_float_array of float list
  | Uconst_string of string
  | Uconst_closure of ufunction list * string * uconstant list

and uconstant =
  | Uconst_ref of string * ustructured_constant option
  | Uconst_int of int

and uphantom_defining_expr =
  | Uphantom_const of uconstant
  (** The phantom-let-bound variable is a constant. *)
  | Uphantom_var of Backend_var.t
  (** The phantom-let-bound variable is an alias for another variable. *)
  | Uphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  (** The phantom-let-bound-variable's value is defined by adding the given
      number of words to the pointer contained in the given identifier. *)
  | Uphantom_read_field of { var : Backend_var.t; field : int; }
  (** The phantom-let-bound-variable's value is found by adding the given
      number of words to the pointer contained in the given identifier, then
      dereferencing. *)
  | Uphantom_read_symbol_field of { sym : string; field : int; }
  (** As for [Uphantom_read_var_field], but with the pointer specified by
      a symbol. *)
  | Uphantom_block of { tag : int; fields : Backend_var.t list; }
  (** The phantom-let-bound variable points at a block with the given
      structure. *)

and ulambda =
    Uvar of Backend_var.t
  | Uconst of uconstant
  | Udirect_apply of function_label * ulambda list * Debuginfo.t
  | Ugeneric_apply of ulambda * ulambda list * Debuginfo.t
  | Uclosure of ufunction list * ulambda list
  | Uoffset of ulambda * int
  | Ulet of mutable_flag * value_kind * Backend_var.With_provenance.t
      * ulambda * ulambda
  | Uphantom_let of Backend_var.With_provenance.t
      * uphantom_defining_expr option * ulambda
  | Uprim of Clambda_primitives.primitive * ulambda list * Debuginfo.t
  | Uswitch of ulambda * ulambda_switch * Debuginfo.t
  | Ustringswitch of ulambda * (string * ulambda) list * ulambda option
  | Ustaticfail of int * ulambda list
  | Ucatch of
      int *
      (Backend_var.With_provenance.t * value_kind) list *
      ulambda *
      ulambda
  | Utrywith of ulambda * Backend_var.With_provenance.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Backend_var.With_provenance.t * ulambda * ulambda
      * direction_flag * ulambda
  | Uassign of Backend_var.t * ulambda
  | Usend of meth_kind * ulambda * ulambda * ulambda list * Debuginfo.t
  | Uunreachable

and ufunction = {
  label  : function_label;
  arity  : int;
  params : (Backend_var.With_provenance.t * value_kind) list;
  return : value_kind;
  body   : ulambda;
  dbg    : Debuginfo.t;
  env    : Backend_var.t option;
  poll   : poll_attribute;
}

and ulambda_switch =
  { us_index_consts: int array;
    us_actions_consts: ulambda array;
    us_index_blocks: int array;
    us_actions_blocks: ulambda array}

(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (Backend_var.With_provenance.t list * ulambda) option;
    mutable fun_float_const_prop: bool; (* Can propagate FP consts *)
    fun_poll: poll_attribute;           (* Behaviour for polls *)
  }

(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_const of uconstant
  | Value_global_field of string * int

(* Comparison functions for constants *)

val compare_structured_constants:
        ustructured_constant -> ustructured_constant -> int
val compare_constants:
        uconstant -> uconstant -> int

type usymbol_provenance = {
  original_idents : Ident.t list;
  module_path : Path.t;
}

type uconstant_block_field =
  | Uconst_field_ref of string
  | Uconst_field_int of int

type preallocated_block = {
  symbol : string;
  exported : bool;
  tag : int;
  fields : uconstant_block_field option list;
  provenance : usymbol_provenance option;
}

type preallocated_constant = {
  symbol : string;
  exported : bool;
  definition : ustructured_constant;
  provenance : usymbol_provenance option;
}

type with_constants =
  ulambda * preallocated_block list * preallocated_constant list
