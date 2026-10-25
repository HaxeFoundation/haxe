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

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

type t = string

let apply_arg = "apply_arg"
let apply_funct = "apply_funct"
let block_symbol = "block_symbol"
let block_symbol_get = "block_symbol_get"
let block_symbol_get_field = "block_symbol_get_field"
let closure = "closure"
let cond = "cond"
let cond_sequor = "cond_sequor"
let const_block = "const_block"
let const_bool = "const_bool"
let const_boxed_int = "const_boxed_int"
let const_char = "const_char"
let const_false = "const_false"
let const_float = "const_float"
let const_int = "const_int"
let const_one = "const_one"
let const_ptr = "const_ptr"
let const_ptr_one = "const_ptr_one"
let const_ptr_zero = "const_ptr_zero"
let const_sequand = "const_sequand"
let const_string = "const_string"
let const_true = "const_true"
let const_zero = "const_zero"
let denominator = "denominator"
let division_by_zero = "division_by_zero"
let dummy = "dummy"
let dup_func = "dup_func"
let dup_set_of_closures = "dup_set_of_closures"
let const_float_array = "const_float_array"
let fake_effect_symbol = "fake_effect_symbol"
let for_from = "for_from"
let for_to = "for_to"
let from_closure = "from_closure"
let full_apply = "full_apply"
let get_symbol_field = "get_symbol_field"
let const_immstring = "const_immstring"
let const_int32 = "const_int32"
let const_int64 = "const_int64"
let ignore = "ignore"
let is_zero = "is_zero"
let lifted_let_rec_block = "lifted_let_rec_block"
let meth = "meth"
let module_as_block = "module_as_block"
let const_nativeint = "const_nativeint"
let new_value = "new_value"
let numerator = "numerator"
let obj = "obj"
let offsetted = "offsetted"
let pabsfloat = "Pabsfloat"
let paddbint = "Paddbint"
let paddfloat = "Paddfloat"
let paddint = "Paddint"
let pandbint = "Pandbint"
let pandint = "Pandint"
let parraylength = "Parraylength"
let parrayrefs = "Parrayrefs"
let parrayrefu = "Parrayrefu"
let parraysets = "Parraysets"
let parraysetu = "Parraysetu"
let pasrbint = "Pasrbint"
let pasrint = "Pasrint"
let pbbswap = "Pbbswap"
let pbigarraydim = "Pbigarraydim"
let pbigarrayref = "Pbigarrayref"
let pbigarrayset = "Pbigarrayset"
let pbigstring_load_16 = "Pbigstring_load_16"
let pbigstring_load_32 = "Pbigstring_load_32"
let pbigstring_load_64 = "Pbigstring_load_64"
let pbigstring_set_16 = "Pbigstring_set_16"
let pbigstring_set_32 = "Pbigstring_set_32"
let pbigstring_set_64 = "Pbigstring_set_64"
let pbintcomp = "Pbintcomp"
let pbintofint = "Pbintofint"
let pbswap16 = "Pbswap16"
let pbytes_of_string = "Pbytes_of_string"
let pbytes_load_16 = "Pbytes_load_16"
let pbytes_load_32 = "Pbytes_load_32"
let pbytes_load_64 = "Pbytes_load_64"
let pbytes_set_16 = "Pbytes_set_16"
let pbytes_set_32 = "Pbytes_set_32"
let pbytes_set_64 = "Pbytes_set_64"
let pbytes_to_string = "Pbytes_to_string"
let pbyteslength = "Pbyteslength"
let pbytesrefs = "Pbytesrefs"
let pbytesrefu = "Pbytesrefu"
let pbytessets = "Pbytessets"
let pbytessetu = "Pbytessetu"
let pccall = "Pccall"
let pctconst = "Pctconst"
let pcvtbint = "Pcvtbint"
let pdivbint = "Pdivbint"
let pdivfloat = "Pdivfloat"
let pdivint = "Pdivint"
let pduparray = "Pduparray"
let pduprecord = "Pduprecord"
let pfield = "Pfield"
let pfield_computed = "Pfield_computed"
let pfloatcomp = "Pfloatcomp"
let pfloatfield = "Pfloatfield"
let pfloatofint = "Pfloatofint"
let pgetglobal = "Pgetglobal"
let pignore = "Pignore"
let pint_as_pointer = "Pint_as_pointer"
let pintcomp = "Pintcomp"
let pcompare_ints = "Pcompare_ints"
let pcompare_floats = "Pcompare_floats"
let pcompare_bints = "Pcompare_bints"
let pintofbint = "Pintofbint"
let pintoffloat = "Pintoffloat"
let pisint = "Pisint"
let pisout = "Pisout"
let plslbint = "Plslbint"
let plslint = "Plslint"
let plsrbint = "Plsrbint"
let plsrint = "Plsrint"
let pmakearray = "Pmakearray"
let pmakeblock = "Pmakeblock"
let pmodbint = "Pmodbint"
let pmodint = "Pmodint"
let pmulbint = "Pmulbint"
let pmulfloat = "Pmulfloat"
let pmulint = "Pmulint"
let pnegbint = "Pnegbint"
let pnegfloat = "Pnegfloat"
let pnegint = "Pnegint"
let pnot = "Pnot"
let poffsetint = "Poffsetint"
let poffsetref = "Poffsetref"
let pointer = "pointer"
let popaque = "Popaque"
let porbint = "Porbint"
let porint = "Porint"
let praise = "Praise"
let predef_exn = "predef_exn"
let project_closure = "project_closure"
let psequand = "Psequand"
let psequor = "Psequor"
let psetfield = "Psetfield"
let psetfield_computed = "Psetfield_computed"
let psetfloatfield = "Psetfloatfield"
let psetglobal = "Psetglobal"
let pstring_load_16 = "Pstring_load_16"
let pstring_load_32 = "Pstring_load_32"
let pstring_load_64 = "Pstring_load_64"
let pstringlength = "Pstringlength"
let pstringrefs = "Pstringrefs"
let pstringrefu = "Pstringrefu"
let psubbint = "Psubbint"
let psubfloat = "Psubfloat"
let psubint = "Psubint"
let pxorbint = "Pxorbint"
let pxorint = "Pxorint"
let patomic_cas = "Patomic_cas"
let patomic_exchange = "Patomic_exchange"
let patomic_fetch_add = "Patomic_fetch_add"
let patomic_load = "Patomic_load"
let prunstack = "Prunstack"
let pperform = "Pperform"
let presume = "Presume"
let preperform = "Preperform"
let pdls_get = "Pdls_get"
let ppoll = "Ppoll"

let pabsfloat_arg = "Pabsfloat_arg"
let paddbint_arg = "Paddbint_arg"
let paddfloat_arg = "Paddfloat_arg"
let paddint_arg = "Paddint_arg"
let pandbint_arg = "Pandbint_arg"
let pandint_arg = "Pandint_arg"
let parraylength_arg = "Parraylength_arg"
let parrayrefs_arg = "Parrayrefs_arg"
let parrayrefu_arg = "Parrayrefu_arg"
let parraysets_arg = "Parraysets_arg"
let parraysetu_arg = "Parraysetu_arg"
let partial_fun = "partial_fun"
let pasrbint_arg = "Pasrbint_arg"
let pasrint_arg = "Pasrint_arg"
let pbbswap_arg = "Pbbswap_arg"
let pbigarraydim_arg = "Pbigarraydim_arg"
let pbigarrayref_arg = "Pbigarrayref_arg"
let pbigarrayset_arg = "Pbigarrayset_arg"
let pbigstring_load_16_arg = "Pbigstring_load_16_arg"
let pbigstring_load_32_arg = "Pbigstring_load_32_arg"
let pbigstring_load_64_arg = "Pbigstring_load_64_arg"
let pbigstring_set_16_arg = "Pbigstring_set_16_arg"
let pbigstring_set_32_arg = "Pbigstring_set_32_arg"
let pbigstring_set_64_arg = "Pbigstring_set_64_arg"
let pbintcomp_arg = "Pbintcomp_arg"
let pbintofint_arg = "Pbintofint_arg"
let pbswap16_arg = "Pbswap16_arg"
let pbytes_of_string_arg = "Pbytes_of_string_arg"
let pbytes_to_string_arg = "Pbytes_to_string_arg"
let pbyteslength_arg = "Pbyteslength_arg"
let pbytesrefs_arg = "Pbytesrefs_arg"
let pbytesrefu_arg = "Pbytesrefu_arg"
let pbytessets_arg = "Pbytessets_arg"
let pbytessetu_arg = "Pbytessetu_arg"
let pccall_arg = "Pccall_arg"
let pctconst_arg = "Pctconst_arg"
let pcvtbint_arg = "Pcvtbint_arg"
let pdivbint_arg = "Pdivbint_arg"
let pdivfloat_arg = "Pdivfloat_arg"
let pdivint_arg = "Pdivint_arg"
let pduparray_arg = "Pduparray_arg"
let pduprecord_arg = "Pduprecord_arg"
let pfield_arg = "Pfield_arg"
let pfield_computed_arg = "Pfield_computed_arg"
let pfloatcomp_arg = "Pfloatcomp_arg"
let pfloatfield_arg = "Pfloatfield_arg"
let pfloatofint_arg = "Pfloatofint_arg"
let pgetglobal_arg = "Pgetglobal_arg"
let pignore_arg = "Pignore_arg"
let pint_as_pointer_arg = "Pint_as_pointer_arg"
let pintcomp_arg = "Pintcomp_arg"
let pcompare_ints_arg = "Pcompare_ints_arg"
let pcompare_floats_arg = "Pcompare_floats_arg"
let pcompare_bints_arg = "Pcompare_bints_arg"
let pintofbint_arg = "Pintofbint_arg"
let pintoffloat_arg = "Pintoffloat_arg"
let pisint_arg = "Pisint_arg"
let pisout_arg = "Pisout_arg"
let plslbint_arg = "Plslbint_arg"
let plslint_arg = "Plslint_arg"
let plsrbint_arg = "Plsrbint_arg"
let plsrint_arg = "Plsrint_arg"
let pmakearray_arg = "Pmakearray_arg"
let pmakeblock_arg = "Pmakeblock_arg"
let pmodbint_arg = "Pmodbint_arg"
let pmodint_arg = "Pmodint_arg"
let pmulbint_arg = "Pmulbint_arg"
let pmulfloat_arg = "Pmulfloat_arg"
let pmulint_arg = "Pmulint_arg"
let pnegbint_arg = "Pnegbint_arg"
let pnegfloat_arg = "Pnegfloat_arg"
let pnegint_arg = "Pnegint_arg"
let pnot_arg = "Pnot_arg"
let poffsetint_arg = "Poffsetint_arg"
let poffsetref_arg = "Poffsetref_arg"
let popaque_arg = "Popaque_arg"
let porbint_arg = "Porbint_arg"
let porint_arg = "Porint_arg"
let praise_arg = "Praise_arg"
let psequand_arg = "Psequand_arg"
let psequor_arg = "Psequor_arg"
let psetfield_arg = "Psetfield_arg"
let psetfield_computed_arg = "Psetfield_computed_arg"
let psetfloatfield_arg = "Psetfloatfield_arg"
let psetglobal_arg = "Psetglobal_arg"
let pstring_load_16_arg = "Pstring_load_16_arg"
let pstring_load_32_arg = "Pstring_load_32_arg"
let pstring_load_64_arg = "Pstring_load_64_arg"
let pbytes_load_16_arg = "Pbytes_load_16_arg"
let pbytes_load_32_arg = "Pbytes_load_32_arg"
let pbytes_load_64_arg = "Pbytes_load_64_arg"
let pbytes_set_16_arg = "Pbytes_set_16_arg"
let pbytes_set_32_arg = "Pbytes_set_32_arg"
let pbytes_set_64_arg = "Pbytes_set_64_arg"
let pstringlength_arg = "Pstringlength_arg"
let pstringrefs_arg = "Pstringrefs_arg"
let pstringrefu_arg = "Pstringrefu_arg"
let psubbint_arg = "Psubbint_arg"
let psubfloat_arg = "Psubfloat_arg"
let psubint_arg = "Psubint_arg"
let pxorbint_arg = "Pxorbint_arg"
let pxorint_arg = "Pxorint_arg"
let patomic_cas_arg = "Patomic_cas_arg"
let patomic_exchange_arg = "Patomic_exchange_arg"
let patomic_fetch_add_arg = "Patomic_fetch_add_arg"
let patomic_load_arg = "Patomic_load_arg"
let prunstack_arg = "Prunstack_arg"
let pperform_arg = "Pperform_arg"
let presume_arg = "Presume_arg"
let preperform_arg = "Preperform_arg"
let pdls_get_arg = "Pdls_get_arg"
let ppoll_arg = "Ppoll_arg"

let raise = "raise"
let raise_arg = "raise_arg"
let read_mutable = "read_mutable"
let remove_unused_arguments = "remove_unused_arguments"
let result = "result"
let send_arg = "send_arg"
let sequence = "sequence"
let set_of_closures = "set_of_closures"
let simplify_fv = "simplify_fv"
let staticraise_arg = "staticraise_arg"
let string_switch = "string_switch"
let switch = "switch"
let symbol = "symbol"
let symbol_field = "symbol_field"
let symbol_field_block = "symbol_field_block"
let the_dead_constant = "the_dead_constant"
let toplevel_substitution_named = "toplevel_substitution_named"
let unbox_free_vars_of_closures = "unbox_free_vars_of_closures"
let unit = "unit"
let zero = "zero"

let anon_fn_with_loc (sloc: Lambda.scoped_location) =
  let loc = Debuginfo.Scoped_location.to_location sloc in
  let (file, line, startchar) = Location.get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in
  let pp_chars ppf =
    if startchar >= 0 then Format.fprintf ppf ",%i--%i" startchar endchar in
  if loc.Location.loc_ghost then "anon_fn"
  else
    Format.asprintf "anon_fn[%s:%i%t]"
      (Filename.basename file) line pp_chars

let of_primitive : Lambda.primitive -> string = function
  | Pbytes_of_string -> pbytes_of_string
  | Pbytes_to_string -> pbytes_to_string
  | Pignore -> pignore
  | Pgetglobal _ -> pgetglobal
  | Psetglobal _ -> psetglobal
  | Pmakeblock _ -> pmakeblock
  | Pfield _ -> pfield
  | Pfield_computed -> pfield_computed
  | Psetfield _ -> psetfield
  | Psetfield_computed _ -> psetfield_computed
  | Pfloatfield _ -> pfloatfield
  | Psetfloatfield _ -> psetfloatfield
  | Pduprecord _ -> pduprecord
  | Pccall _ -> pccall
  | Praise _ -> praise
  | Psequand -> psequand
  | Psequor -> psequor
  | Pnot -> pnot
  | Pnegint -> pnegint
  | Paddint -> paddint
  | Psubint -> psubint
  | Pmulint -> pmulint
  | Pdivint _ -> pdivint
  | Pmodint _ -> pmodint
  | Pandint -> pandint
  | Porint -> porint
  | Pxorint -> pxorint
  | Plslint -> plslint
  | Plsrint -> plsrint
  | Pasrint -> pasrint
  | Pintcomp _ -> pintcomp
  | Pcompare_ints -> pcompare_ints
  | Pcompare_floats -> pcompare_floats
  | Pcompare_bints _ -> pcompare_bints
  | Poffsetint _ -> poffsetint
  | Poffsetref _ -> poffsetref
  | Pintoffloat -> pintoffloat
  | Pfloatofint -> pfloatofint
  | Pnegfloat -> pnegfloat
  | Pabsfloat -> pabsfloat
  | Paddfloat -> paddfloat
  | Psubfloat -> psubfloat
  | Pmulfloat -> pmulfloat
  | Pdivfloat -> pdivfloat
  | Pfloatcomp _ -> pfloatcomp
  | Pstringlength -> pstringlength
  | Pstringrefu -> pstringrefu
  | Pstringrefs -> pstringrefs
  | Pbyteslength -> pbyteslength
  | Pbytesrefu -> pbytesrefu
  | Pbytessetu -> pbytessetu
  | Pbytesrefs -> pbytesrefs
  | Pbytessets -> pbytessets
  | Parraylength _ -> parraylength
  | Pmakearray _ -> pmakearray
  | Pduparray _ -> pduparray
  | Parrayrefu _ -> parrayrefu
  | Parraysetu _ -> parraysetu
  | Parrayrefs _ -> parrayrefs
  | Parraysets _ -> parraysets
  | Pctconst _ -> pctconst
  | Pisint -> pisint
  | Pisout -> pisout
  | Pbintofint _ -> pbintofint
  | Pintofbint _ -> pintofbint
  | Pcvtbint _ -> pcvtbint
  | Pnegbint _ -> pnegbint
  | Paddbint _ -> paddbint
  | Psubbint _ -> psubbint
  | Pmulbint _ -> pmulbint
  | Pdivbint _ -> pdivbint
  | Pmodbint _ -> pmodbint
  | Pandbint _ -> pandbint
  | Porbint _ -> porbint
  | Pxorbint _ -> pxorbint
  | Plslbint _ -> plslbint
  | Plsrbint _ -> plsrbint
  | Pasrbint _ -> pasrbint
  | Pbintcomp _ -> pbintcomp
  | Pbigarrayref _ -> pbigarrayref
  | Pbigarrayset _ -> pbigarrayset
  | Pbigarraydim _ -> pbigarraydim
  | Pstring_load_16 _ -> pstring_load_16
  | Pstring_load_32 _ -> pstring_load_32
  | Pstring_load_64 _ -> pstring_load_64
  | Pbytes_load_16 _ -> pbytes_load_16
  | Pbytes_load_32 _ -> pbytes_load_32
  | Pbytes_load_64 _ -> pbytes_load_64
  | Pbytes_set_16 _ -> pbytes_set_16
  | Pbytes_set_32 _ -> pbytes_set_32
  | Pbytes_set_64 _ -> pbytes_set_64
  | Pbigstring_load_16 _ -> pbigstring_load_16
  | Pbigstring_load_32 _ -> pbigstring_load_32
  | Pbigstring_load_64 _ -> pbigstring_load_64
  | Pbigstring_set_16 _ -> pbigstring_set_16
  | Pbigstring_set_32 _ -> pbigstring_set_32
  | Pbigstring_set_64 _ -> pbigstring_set_64
  | Pbswap16 -> pbswap16
  | Pbbswap _ -> pbbswap
  | Pint_as_pointer -> pint_as_pointer
  | Popaque -> popaque
  | Patomic_cas -> patomic_cas
  | Patomic_exchange -> patomic_exchange
  | Patomic_fetch_add -> patomic_fetch_add
  | Patomic_load _ -> patomic_load
  | Prunstack -> prunstack
  | Pperform -> pperform
  | Presume -> presume
  | Preperform -> preperform
  | Pdls_get -> pdls_get
  | Ppoll -> ppoll

let of_primitive_arg : Lambda.primitive -> string = function
  | Pbytes_of_string -> pbytes_of_string_arg
  | Pbytes_to_string -> pbytes_to_string_arg
  | Pignore -> pignore_arg
  | Pgetglobal _ -> pgetglobal_arg
  | Psetglobal _ -> psetglobal_arg
  | Pmakeblock _ -> pmakeblock_arg
  | Pfield _ -> pfield_arg
  | Pfield_computed -> pfield_computed_arg
  | Psetfield _ -> psetfield_arg
  | Psetfield_computed _ -> psetfield_computed_arg
  | Pfloatfield _ -> pfloatfield_arg
  | Psetfloatfield _ -> psetfloatfield_arg
  | Pduprecord _ -> pduprecord_arg
  | Pccall _ -> pccall_arg
  | Praise _ -> praise_arg
  | Psequand -> psequand_arg
  | Psequor -> psequor_arg
  | Pnot -> pnot_arg
  | Pnegint -> pnegint_arg
  | Paddint -> paddint_arg
  | Psubint -> psubint_arg
  | Pmulint -> pmulint_arg
  | Pdivint _ -> pdivint_arg
  | Pmodint _ -> pmodint_arg
  | Pandint -> pandint_arg
  | Porint -> porint_arg
  | Pxorint -> pxorint_arg
  | Plslint -> plslint_arg
  | Plsrint -> plsrint_arg
  | Pasrint -> pasrint_arg
  | Pintcomp _ -> pintcomp_arg
  | Pcompare_ints -> pcompare_ints_arg
  | Pcompare_floats -> pcompare_floats_arg
  | Pcompare_bints _ -> pcompare_bints_arg
  | Poffsetint _ -> poffsetint_arg
  | Poffsetref _ -> poffsetref_arg
  | Pintoffloat -> pintoffloat_arg
  | Pfloatofint -> pfloatofint_arg
  | Pnegfloat -> pnegfloat_arg
  | Pabsfloat -> pabsfloat_arg
  | Paddfloat -> paddfloat_arg
  | Psubfloat -> psubfloat_arg
  | Pmulfloat -> pmulfloat_arg
  | Pdivfloat -> pdivfloat_arg
  | Pfloatcomp _ -> pfloatcomp_arg
  | Pstringlength -> pstringlength_arg
  | Pstringrefu -> pstringrefu_arg
  | Pstringrefs -> pstringrefs_arg
  | Pbyteslength -> pbyteslength_arg
  | Pbytesrefu -> pbytesrefu_arg
  | Pbytessetu -> pbytessetu_arg
  | Pbytesrefs -> pbytesrefs_arg
  | Pbytessets -> pbytessets_arg
  | Parraylength _ -> parraylength_arg
  | Pmakearray _ -> pmakearray_arg
  | Pduparray _ -> pduparray_arg
  | Parrayrefu _ -> parrayrefu_arg
  | Parraysetu _ -> parraysetu_arg
  | Parrayrefs _ -> parrayrefs_arg
  | Parraysets _ -> parraysets_arg
  | Pctconst _ -> pctconst_arg
  | Pisint -> pisint_arg
  | Pisout -> pisout_arg
  | Pbintofint _ -> pbintofint_arg
  | Pintofbint _ -> pintofbint_arg
  | Pcvtbint _ -> pcvtbint_arg
  | Pnegbint _ -> pnegbint_arg
  | Paddbint _ -> paddbint_arg
  | Psubbint _ -> psubbint_arg
  | Pmulbint _ -> pmulbint_arg
  | Pdivbint _ -> pdivbint_arg
  | Pmodbint _ -> pmodbint_arg
  | Pandbint _ -> pandbint_arg
  | Porbint _ -> porbint_arg
  | Pxorbint _ -> pxorbint_arg
  | Plslbint _ -> plslbint_arg
  | Plsrbint _ -> plsrbint_arg
  | Pasrbint _ -> pasrbint_arg
  | Pbintcomp _ -> pbintcomp_arg
  | Pbigarrayref _ -> pbigarrayref_arg
  | Pbigarrayset _ -> pbigarrayset_arg
  | Pbigarraydim _ -> pbigarraydim_arg
  | Pstring_load_16 _ -> pstring_load_16_arg
  | Pstring_load_32 _ -> pstring_load_32_arg
  | Pstring_load_64 _ -> pstring_load_64_arg
  | Pbytes_load_16 _ -> pbytes_load_16_arg
  | Pbytes_load_32 _ -> pbytes_load_32_arg
  | Pbytes_load_64 _ -> pbytes_load_64_arg
  | Pbytes_set_16 _ -> pbytes_set_16_arg
  | Pbytes_set_32 _ -> pbytes_set_32_arg
  | Pbytes_set_64 _ -> pbytes_set_64_arg
  | Pbigstring_load_16 _ -> pbigstring_load_16_arg
  | Pbigstring_load_32 _ -> pbigstring_load_32_arg
  | Pbigstring_load_64 _ -> pbigstring_load_64_arg
  | Pbigstring_set_16 _ -> pbigstring_set_16_arg
  | Pbigstring_set_32 _ -> pbigstring_set_32_arg
  | Pbigstring_set_64 _ -> pbigstring_set_64_arg
  | Pbswap16 -> pbswap16_arg
  | Pbbswap _ -> pbbswap_arg
  | Pint_as_pointer -> pint_as_pointer_arg
  | Popaque -> popaque_arg
  | Patomic_cas -> patomic_cas_arg
  | Patomic_exchange -> patomic_exchange_arg
  | Patomic_fetch_add -> patomic_fetch_add_arg
  | Patomic_load _ -> patomic_load_arg
  | Prunstack -> prunstack_arg
  | Pperform -> pperform_arg
  | Presume -> presume_arg
  | Preperform -> preperform_arg
  | Pdls_get -> pdls_get_arg
  | Ppoll -> ppoll_arg
