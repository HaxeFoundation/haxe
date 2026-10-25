(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Fu Yong Quah, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = private string

val apply_arg : t
val apply_funct : t
val block_symbol : t
val block_symbol_get : t
val block_symbol_get_field : t
val closure : t
val cond : t
val cond_sequor : t
val const_block : t
val const_bool : t
val const_boxed_int : t
val const_char : t
val const_false : t
val const_float : t
val const_int : t
val const_one : t
val const_ptr : t
val const_ptr_one : t
val const_ptr_zero : t
val const_sequand : t
val const_string : t
val const_true : t
val const_zero : t
val denominator : t
val division_by_zero : t
val dummy : t
val dup_func : t
val dup_set_of_closures : t
val const_float_array : t
val fake_effect_symbol : t
val for_from : t
val for_to : t
val from_closure : t
val full_apply : t
val get_symbol_field : t
val const_immstring : t
val const_int32 : t
val const_int64 : t
val ignore : t
val is_zero : t
val lifted_let_rec_block : t
val meth : t
val module_as_block : t
val const_nativeint : t
val new_value : t
val numerator : t
val obj : t
val offsetted : t
val partial_fun : t
val pgetglobal : t
val pointer : t
val predef_exn : t
val project_closure : t
val raise : t
val raise_arg : t
val read_mutable : t
val remove_unused_arguments : t
val result : t
val send_arg : t
val sequence : t
val set_of_closures : t
val staticraise_arg : t
val simplify_fv : t
val string_switch : t
val switch : t
val symbol : t
val symbol_field : t
val symbol_field_block : t
val the_dead_constant : t
val toplevel_substitution_named : t
val unbox_free_vars_of_closures : t
val unit : t
val zero : t

val of_primitive : Lambda.primitive -> t

val of_primitive_arg : Lambda.primitive -> t

val anon_fn_with_loc : Lambda.scoped_location -> t
