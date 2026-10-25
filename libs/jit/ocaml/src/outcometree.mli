(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Outcometree]: results displayed by the toplevel *)

(* These types represent messages that the toplevel displays as normal
   results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] *)

(** An [out_name] is a string representation of an identifier which can be
    rewritten on the fly to avoid name collisions *)
type out_name = { mutable printed_name: string }

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_ident of out_name

type out_string =
  | Ostr_string
  | Ostr_bytes

type out_attribute =
  { oattr_name: string }

type out_value =
  | Oval_array of out_value list
  | Oval_char of char
  | Oval_constr of out_ident * out_value list
  | Oval_ellipsis
  | Oval_float of float
  | Oval_int of int
  | Oval_int32 of int32
  | Oval_int64 of int64
  | Oval_nativeint of nativeint
  | Oval_list of out_value list
  | Oval_printer of (Format_doc.formatter -> unit)
  | Oval_record of (out_ident * out_value) list
  | Oval_string of string * int * out_string (* string, size-to-print, kind *)
  | Oval_stuff of string
  | Oval_tuple of out_value list
  | Oval_variant of string * out_value option
  | Oval_lazy of out_value

type out_type_param = {
    ot_non_gen: bool;
    ot_name: string;
    ot_variance: Asttypes.variance * Asttypes.injectivity
}

type out_type =
  | Otyp_abstract
  | Otyp_open
  | Otyp_alias of {non_gen:bool; aliased:out_type; alias:string}
  | Otyp_arrow of Asttypes.arg_label * out_type * out_type
  | Otyp_class of out_ident * out_type list
  | Otyp_constr of out_ident * out_type list
  | Otyp_manifest of out_type * out_type
  | Otyp_object of { fields: (string * out_type) list; open_row:bool}
  | Otyp_record of out_label list
  | Otyp_stuff of string
  | Otyp_sum of out_constructor list
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string
  | Otyp_variant of out_variant * bool * (string list) option
  | Otyp_poly of string list * out_type
  | Otyp_module of out_ident * (string * out_type) list
  | Otyp_attribute of out_type * out_attribute

and out_label = {
  olab_name: string;
  olab_mut: Asttypes.mutable_flag;
  olab_type: out_type;
}

and out_constructor = {
  ocstr_name: string;
  ocstr_args: out_type list;
  ocstr_return_type: out_type option;
}

and out_variant =
  | Ovar_fields of (string * bool * out_type list) list
  | Ovar_typ of out_type

type out_class_type =
  | Octy_constr of out_ident * out_type list
  | Octy_arrow of Asttypes.arg_label * out_type * out_class_type
  | Octy_signature of out_type option * out_class_sig_item list
and out_class_sig_item =
  | Ocsg_constraint of out_type * out_type
  | Ocsg_method of string * bool * bool * out_type
  | Ocsg_value of string * bool * bool * out_type

type out_module_type =
  | Omty_abstract
  | Omty_functor of (string option * out_module_type) option * out_module_type
  | Omty_ident of out_ident
  | Omty_signature of out_sig_item list
  | Omty_alias of out_ident
and out_sig_item =
  | Osig_class of
      bool * string * out_type_param list * out_class_type *
        out_rec_status
  | Osig_class_type of
      bool * string * out_type_param list * out_class_type *
        out_rec_status
  | Osig_typext of out_extension_constructor * out_ext_status
  | Osig_modtype of string * out_module_type
  | Osig_module of string * out_module_type * out_rec_status
  | Osig_type of out_type_decl * out_rec_status
  | Osig_value of out_val_decl
  | Osig_ellipsis
and out_type_decl =
  { otype_name: string;
    otype_params: out_type_param list;
    otype_type: out_type;
    otype_private: Asttypes.private_flag;
    otype_immediate: Type_immediacy.t;
    otype_unboxed: bool;
    otype_cstrs: (out_type * out_type) list }
and out_extension_constructor =
  { oext_name: string;
    oext_type_name: string;
    oext_type_params: string list;
    oext_args: out_type list;
    oext_ret_type: out_type option;
    oext_private: Asttypes.private_flag }
and out_type_extension =
  { otyext_name: string;
    otyext_params: string list;
    otyext_constructors: out_constructor list;
    otyext_private: Asttypes.private_flag }
and out_val_decl =
  { oval_name: string;
    oval_type: out_type;
    oval_prims: string list;
    oval_attributes: out_attribute list }
and out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next
and out_ext_status =
  | Oext_first
  | Oext_next
  | Oext_exception

type out_phrase =
  | Ophr_eval of out_value * out_type
  | Ophr_signature of (out_sig_item * out_value option) list
  | Ophr_exception of (exn * out_value)
