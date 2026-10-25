(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

let for_primitive (prim : Clambda_primitives.primitive) =
  match prim with
  | Pmakeblock _
  | Pmakearray (_, Mutable) -> Only_generative_effects, No_coeffects
  | Pmakearray (_, Immutable) -> No_effects, No_coeffects
  | Pduparray (_, Immutable) ->
      No_effects, No_coeffects  (* Pduparray (_, Immutable) is allowed only on
                                   immutable arrays. *)
  | Pduparray (_, Mutable) | Pduprecord _ ->
      Only_generative_effects, Has_coeffects
  | Pccall { prim_name =
               ( "caml_format_float" | "caml_format_int" | "caml_int32_format"
               | "caml_nativeint_format" | "caml_int64_format" ) } ->
      No_effects, No_coeffects
  | Pccall _ -> Arbitrary_effects, Has_coeffects
  | Praise _ -> Arbitrary_effects, No_coeffects
  | Prunstack | Pperform | Presume | Preperform ->
      Arbitrary_effects, Has_coeffects
  | Pnot
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp _ -> No_effects, No_coeffects
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
    -> No_effects, No_coeffects
  | Pdivbint { is_safe = Unsafe }
  | Pmodbint { is_safe = Unsafe }
  | Pdivint Unsafe
  | Pmodint Unsafe ->
      No_effects, No_coeffects  (* Will not raise [Division_by_zero]. *)
  | Pdivbint { is_safe = Safe }
  | Pmodbint { is_safe = Safe }
  | Pdivint Safe
  | Pmodint Safe ->
      Arbitrary_effects, No_coeffects
  | Poffsetint _ -> No_effects, No_coeffects
  | Poffsetref _ -> Arbitrary_effects, Has_coeffects
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pabsfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pfloatcomp _ -> No_effects, No_coeffects
  | Pstringlength | Pbyteslength
  | Parraylength _ -> No_effects, No_coeffects
  | Pisint
  | Pisout
  | Pbintofint _
  | Pintofbint _
  | Pcvtbint _
  | Pnegbint _
  | Paddbint _
  | Psubbint _
  | Pmulbint _
  | Pandbint _
  | Porbint _
  | Pxorbint _
  | Plslbint _
  | Plsrbint _
  | Pasrbint _
  | Pbintcomp _ -> No_effects, No_coeffects
  | Pbigarraydim _ ->
      No_effects, Has_coeffects  (* Some people resize bigarrays in place. *)
  | Pread_symbol _
  | Pfield _
  | Pfield_computed
  | Pfloatfield _
  | Parrayrefu _
  | Pstringrefu
  | Pbytesrefu
  | Pstring_load (_, Unsafe)
  | Pbytes_load (_, Unsafe)
  | Pbigarrayref (true, _, _, _)
  | Pbigstring_load (_, Unsafe) ->
      No_effects, Has_coeffects
  | Parrayrefs _
  | Pstringrefs
  | Pbytesrefs
  | Pstring_load (_, Safe)
  | Pbytes_load (_, Safe)
  | Pbigarrayref (false, _, _, _)
  | Pbigstring_load (_, Safe) ->
      (* May trigger a bounds check exception. *)
      Arbitrary_effects, Has_coeffects
  | Psetfield _
  | Psetfield_computed _
  | Psetfloatfield _
  | Patomic_load _
  | Patomic_exchange
  | Patomic_cas
  | Patomic_fetch_add
  | Parraysetu _
  | Parraysets _
  | Pbytessetu
  | Pbytessets
  | Pbytes_set _
  | Pbigarrayset _
  | Pbigstring_set _ ->
      (* Whether or not some of these are "unsafe" is irrelevant; they always
         have an effect. *)
      Arbitrary_effects, No_coeffects
  | Pbswap16
  | Pbbswap _ -> No_effects, No_coeffects
  | Pint_as_pointer -> No_effects, No_coeffects
  | Popaque | Ppoll -> Arbitrary_effects, Has_coeffects
  | Psequand
  | Psequor ->
      (* Removed by [Closure_conversion] in the flambda pipeline. *)
      No_effects, No_coeffects
  | Pdls_get ->
      (* only read *)
      No_effects, No_coeffects

type return_type =
  | Float
  | Other

let return_type_of_primitive (prim:Clambda_primitives.primitive) =
  match prim with
  | Pfloatofint
  | Pnegfloat
  | Pabsfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pfloatfield _
  | Parrayrefu Pfloatarray
  | Parrayrefs Pfloatarray ->
      Float
  | _ ->
      Other
