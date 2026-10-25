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


open Format
open Asttypes

let boxed_integer_name = function
  | Lambda.Pnativeint -> "nativeint"
  | Lambda.Pint32 -> "int32"
  | Lambda.Pint64 -> "int64"

let boxed_integer_mark name = function
  | Lambda.Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Lambda.Pint32 -> Printf.sprintf "Int32.%s" name
  | Lambda.Pint64 -> Printf.sprintf "Int64.%s" name

let print_boxed_integer name ppf bi =
  fprintf ppf "%s" (boxed_integer_mark name bi)

let array_kind array_kind =
  let open Lambda in
  match array_kind with
  | Pgenarray -> "gen"
  | Paddrarray -> "addr"
  | Pintarray -> "int"
  | Pfloatarray -> "float"

let access_size size =
  let open Clambda_primitives in
  match size with
  | Sixteen -> "16"
  | Thirty_two -> "32"
  | Sixty_four -> "64"

let access_safety safety =
  let open Lambda in
  match safety with
  | Safe -> ""
  | Unsafe -> "unsafe_"

let primitive ppf (prim:Clambda_primitives.primitive) =
  let open Lambda in
  let open Clambda_primitives in
  match prim with
  | Pread_symbol sym ->
      fprintf ppf "read_symbol %s" sym
  | Pmakeblock(tag, Immutable, shape) ->
      fprintf ppf "makeblock %i%a" tag Printlambda.block_shape shape
  | Pmakeblock(tag, Mutable, shape) ->
      fprintf ppf "makemutable %i%a" tag Printlambda.block_shape shape
  | Pfield(n, ptr, mut) ->
      let instr =
        match ptr, mut with
        | Immediate, _ -> "field_int "
        | Pointer, Mutable -> "field_mut "
        | Pointer, Immutable -> "field_imm "
      in
      fprintf ppf "%s%i" instr n
  | Pfield_computed -> fprintf ppf "field_computed"
  | Psetfield(n, ptr, init) ->
      let instr =
        match ptr with
        | Pointer -> "ptr"
        | Immediate -> "imm"
      in
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
      in
      fprintf ppf "setfield_%s%s %i" instr init n
  | Psetfield_computed (ptr, init) ->
      let instr =
        match ptr with
        | Pointer -> "ptr"
        | Immediate -> "imm"
      in
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
      in
      fprintf ppf "setfield_%s%s_computed" instr init
  | Pfloatfield n -> fprintf ppf "floatfield %i" n
  | Psetfloatfield (n, init) ->
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
      in
      fprintf ppf "setfloatfield%s %i" init n
  | Pduprecord (rep, size) ->
      fprintf ppf "duprecord %a %i" Printlambda.record_rep rep size
  | Prunstack -> fprintf ppf "runstack"
  | Pperform -> fprintf ppf "perform"
  | Presume -> fprintf ppf "resume"
  | Preperform -> fprintf ppf "reperform"
  | Pccall p -> fprintf ppf "%s" p.Primitive.prim_name
  | Praise k -> fprintf ppf "%s" (Lambda.raise_kind k)
  | Psequand -> fprintf ppf "&&"
  | Psequor -> fprintf ppf "||"
  | Pnot -> fprintf ppf "not"
  | Pnegint -> fprintf ppf "~"
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint Safe -> fprintf ppf "/"
  | Pdivint Unsafe -> fprintf ppf "/u"
  | Pmodint Safe -> fprintf ppf "mod"
  | Pmodint Unsafe -> fprintf ppf "mod_unsafe"
  | Pandint -> fprintf ppf "and"
  | Porint -> fprintf ppf "or"
  | Pxorint -> fprintf ppf "xor"
  | Plslint -> fprintf ppf "lsl"
  | Plsrint -> fprintf ppf "lsr"
  | Pasrint -> fprintf ppf "asr"
  | Pintcomp(cmp) -> Printlambda.integer_comparison ppf cmp
  | Pcompare_ints -> fprintf ppf "compare_ints"
  | Pcompare_floats -> fprintf ppf "compare_floats"
  | Pcompare_bints bi -> fprintf ppf "compare_bints %s" (boxed_integer_name bi)
  | Poffsetint n -> fprintf ppf "%i+" n
  | Poffsetref n -> fprintf ppf "+:=%i"n
  | Pintoffloat -> fprintf ppf "int_of_float"
  | Pfloatofint -> fprintf ppf "float_of_int"
  | Pnegfloat -> fprintf ppf "~."
  | Pabsfloat -> fprintf ppf "abs."
  | Paddfloat -> fprintf ppf "+."
  | Psubfloat -> fprintf ppf "-."
  | Pmulfloat -> fprintf ppf "*."
  | Pdivfloat -> fprintf ppf "/."
  | Pfloatcomp(cmp) -> Printlambda.float_comparison ppf cmp
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pbyteslength -> fprintf ppf "bytes.length"
  | Pbytesrefu -> fprintf ppf "bytes.unsafe_get"
  | Pbytessetu -> fprintf ppf "bytes.unsafe_set"
  | Pbytesrefs -> fprintf ppf "bytes.get"
  | Pbytessets -> fprintf ppf "bytes.set"

  | Parraylength k -> fprintf ppf "array.length[%s]" (array_kind k)
  | Pmakearray (k, Mutable) -> fprintf ppf "makearray[%s]" (array_kind k)
  | Pmakearray (k, Immutable) -> fprintf ppf "makearray_imm[%s]" (array_kind k)
  | Pduparray (k, Mutable) -> fprintf ppf "duparray[%s]" (array_kind k)
  | Pduparray (k, Immutable) -> fprintf ppf "duparray_imm[%s]" (array_kind k)
  | Parrayrefu k -> fprintf ppf "array.unsafe_get[%s]" (array_kind k)
  | Parraysetu k -> fprintf ppf "array.unsafe_set[%s]" (array_kind k)
  | Parrayrefs k -> fprintf ppf "array.get[%s]" (array_kind k)
  | Parraysets k -> fprintf ppf "array.set[%s]" (array_kind k)
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbintofint bi -> print_boxed_integer "of_int" ppf bi
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi
  | Pcvtbint (bi1, bi2) ->
      fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)
  | Pnegbint bi -> print_boxed_integer "neg" ppf bi
  | Paddbint bi -> print_boxed_integer "add" ppf bi
  | Psubbint bi -> print_boxed_integer "sub" ppf bi
  | Pmulbint bi -> print_boxed_integer "mul" ppf bi
  | Pdivbint { size = bi; is_safe = Safe } ->
      print_boxed_integer "div" ppf bi
  | Pdivbint { size = bi; is_safe = Unsafe } ->
      print_boxed_integer "div_unsafe" ppf bi
  | Pmodbint { size = bi; is_safe = Safe } ->
      print_boxed_integer "mod" ppf bi
  | Pmodbint { size = bi; is_safe = Unsafe } ->
      print_boxed_integer "mod_unsafe" ppf bi
  | Pandbint bi -> print_boxed_integer "and" ppf bi
  | Porbint bi -> print_boxed_integer "or" ppf bi
  | Pxorbint bi -> print_boxed_integer "xor" ppf bi
  | Plslbint bi -> print_boxed_integer "lsl" ppf bi
  | Plsrbint bi -> print_boxed_integer "lsr" ppf bi
  | Pasrbint bi -> print_boxed_integer "asr" ppf bi
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi
  | Pbintcomp(bi, Cne) -> print_boxed_integer "!=" ppf bi
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi
  | Pbigarrayref(unsafe, _n, kind, layout) ->
      Printlambda.print_bigarray "get" unsafe kind ppf layout
  | Pbigarrayset(unsafe, _n, kind, layout) ->
      Printlambda.print_bigarray "set" unsafe kind ppf layout
  | Pbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n
  | Pstring_load(size, safety) ->
      fprintf ppf "string.%sget%s" (access_safety safety) (access_size size)
  | Pbytes_load(size, safety) ->
      fprintf ppf "bytes.%sget%s" (access_safety safety) (access_size size)
  | Pbytes_set(size, safety) ->
      fprintf ppf "bytes.%sset%s" (access_safety safety) (access_size size)
  | Pbigstring_load(size, safety) ->
      fprintf ppf "bigarray.array1.%sget%s"
        (access_safety safety) (access_size size)
  | Pbigstring_set(size, safety) ->
      fprintf ppf "bigarray.array1.%sset%s"
        (access_safety safety) (access_size size)
  | Pbswap16 -> fprintf ppf "bswap16"
  | Pbbswap(bi) -> print_boxed_integer "bswap" ppf bi
  | Pint_as_pointer -> fprintf ppf "int_as_pointer"
  | Patomic_load {immediate_or_pointer} ->
      (match immediate_or_pointer with
        | Immediate -> fprintf ppf "atomic_load_imm"
        | Pointer -> fprintf ppf "atomic_load_ptr")
  | Patomic_exchange -> fprintf ppf "atomic_exchange"
  | Patomic_cas -> fprintf ppf "atomic_cas"
  | Patomic_fetch_add -> fprintf ppf "atomic_fetch_add"
  | Popaque -> fprintf ppf "opaque"
  | Pdls_get -> fprintf ppf "dls_get"
  | Ppoll -> fprintf ppf "poll"
