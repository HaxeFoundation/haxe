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
open Primitive
open Types
open Lambda


let rec struct_const ppf = function
  | Const_base(Const_int n) -> fprintf ppf "%i" n
  | Const_base(Const_char c) -> fprintf ppf "%C" c
  | Const_base(Const_string (s, _, _)) -> fprintf ppf "%S" s
  | Const_immstring s -> fprintf ppf "#%S" s
  | Const_base(Const_float f) -> fprintf ppf "%s" f
  | Const_base(Const_int32 n) -> fprintf ppf "%lil" n
  | Const_base(Const_int64 n) -> fprintf ppf "%LiL" n
  | Const_base(Const_nativeint n) -> fprintf ppf "%nin" n
  | Const_block(tag, []) ->
      fprintf ppf "[%i]" tag
  | Const_block(tag, sc1::scl) ->
      let sconsts ppf scl =
        List.iter (fun sc -> fprintf ppf "@ %a" struct_const sc) scl in
      fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_float_array [] ->
      fprintf ppf "[| |]"
  | Const_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl

let array_kind = function
  | Pgenarray -> "gen"
  | Paddrarray -> "addr"
  | Pintarray -> "int"
  | Pfloatarray -> "float"

let boxed_integer_name = function
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let value_kind ppf = function
  | Pgenval -> ()
  | Pintval -> fprintf ppf "[int]"
  | Pfloatval -> fprintf ppf "[float]"
  | Pboxedintval bi -> fprintf ppf "[%s]" (boxed_integer_name bi)

let return_kind ppf = function
  | Pgenval -> ()
  | Pintval -> fprintf ppf ": int@ "
  | Pfloatval -> fprintf ppf ": float@ "
  | Pboxedintval bi -> fprintf ppf ": %s@ " (boxed_integer_name bi)

let field_kind = function
  | Pgenval -> "*"
  | Pintval -> "int"
  | Pfloatval -> "float"
  | Pboxedintval bi -> boxed_integer_name bi

let print_boxed_integer_conversion ppf bi1 bi2 =
  fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)

let boxed_integer_mark name = function
  | Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Pint32 -> Printf.sprintf "Int32.%s" name
  | Pint64 -> Printf.sprintf "Int64.%s" name

let print_boxed_integer name ppf bi =
  fprintf ppf "%s" (boxed_integer_mark name bi)

let print_bigarray name unsafe kind ppf layout =
  fprintf ppf "Bigarray.%s[%s,%s]"
    (if unsafe then "unsafe_"^ name else name)
    (match kind with
     | Pbigarray_unknown -> "generic"
     | Pbigarray_float16 -> "float16"
     | Pbigarray_float32 -> "float32"
     | Pbigarray_float64 -> "float64"
     | Pbigarray_sint8 -> "sint8"
     | Pbigarray_uint8 -> "uint8"
     | Pbigarray_sint16 -> "sint16"
     | Pbigarray_uint16 -> "uint16"
     | Pbigarray_int32 -> "int32"
     | Pbigarray_int64 -> "int64"
     | Pbigarray_caml_int -> "camlint"
     | Pbigarray_native_int -> "nativeint"
     | Pbigarray_complex32 -> "complex32"
     | Pbigarray_complex64 -> "complex64")
    (match layout with
    |  Pbigarray_unknown_layout -> "unknown"
     | Pbigarray_c_layout -> "C"
     | Pbigarray_fortran_layout -> "Fortran")

let record_rep ppf r =
  match r with
  | Record_regular -> fprintf ppf "regular"
  | Record_inlined i -> fprintf ppf "inlined(%i)" i
  | Record_unboxed false -> fprintf ppf "unboxed"
  | Record_unboxed true -> fprintf ppf "inlined(unboxed)"
  | Record_float -> fprintf ppf "float"
  | Record_extension path -> fprintf ppf "ext(%a)" Printtyp.path path

let block_shape ppf shape = match shape with
  | None | Some [] -> ()
  | Some l when List.for_all ((=) Pgenval) l -> ()
  | Some [elt] ->
      Format.fprintf ppf " (%s)" (field_kind elt)
  | Some (h :: t) ->
      Format.fprintf ppf " (%s" (field_kind h);
      List.iter (fun elt ->
          Format.fprintf ppf ",%s" (field_kind elt))
        t;
      Format.fprintf ppf ")"

let integer_comparison ppf = function
  | Ceq -> fprintf ppf "=="
  | Cne -> fprintf ppf "!="
  | Clt -> fprintf ppf "<"
  | Cle -> fprintf ppf "<="
  | Cgt -> fprintf ppf ">"
  | Cge -> fprintf ppf ">="

let float_comparison ppf = function
  | CFeq -> fprintf ppf "==."
  | CFneq -> fprintf ppf "!=."
  | CFlt -> fprintf ppf "<."
  | CFnlt -> fprintf ppf "!<."
  | CFle -> fprintf ppf "<=."
  | CFnle -> fprintf ppf "!<=."
  | CFgt -> fprintf ppf ">."
  | CFngt -> fprintf ppf "!>."
  | CFge -> fprintf ppf ">=."
  | CFnge -> fprintf ppf "!>=."

let primitive ppf = function
  | Pbytes_to_string -> fprintf ppf "bytes_to_string"
  | Pbytes_of_string -> fprintf ppf "bytes_of_string"
  | Pignore -> fprintf ppf "ignore"
  | Pgetglobal id -> fprintf ppf "global %a" Ident.print id
  | Psetglobal id -> fprintf ppf "setglobal %a" Ident.print id
  | Pmakeblock(tag, Immutable, shape) ->
      fprintf ppf "makeblock %i%a" tag block_shape shape
  | Pmakeblock(tag, Mutable, shape) ->
      fprintf ppf "makemutable %i%a" tag block_shape shape
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
  | Pduprecord (rep, size) -> fprintf ppf "duprecord %a %i" record_rep rep size
  | Prunstack -> fprintf ppf "runstack"
  | Pperform -> fprintf ppf "perform"
  | Presume -> fprintf ppf "resume"
  | Preperform -> fprintf ppf "reperform"
  | Pccall p -> fprintf ppf "%s" p.prim_name
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
  | Pintcomp(cmp) -> integer_comparison ppf cmp
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
  | Pfloatcomp(cmp) -> float_comparison ppf cmp
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
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Int_size -> "int_size"
       | Max_wosize -> "max_wosize"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin"
       | Backend_type -> "backend_type" in
     fprintf ppf "sys.constant_%s" const_name
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbintofint bi -> print_boxed_integer "of_int" ppf bi
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi
  | Pcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2
  | Pnegbint bi -> print_boxed_integer "neg" ppf bi
  | Paddbint bi -> print_boxed_integer "add" ppf bi
  | Psubbint bi -> print_boxed_integer "sub" ppf bi
  | Pmulbint bi -> print_boxed_integer "mul" ppf bi
  | Pdivbint { size; is_safe = Safe } ->
      print_boxed_integer "div" ppf size
  | Pdivbint { size; is_safe = Unsafe } ->
      print_boxed_integer "div_unsafe" ppf size
  | Pmodbint { size; is_safe = Safe } ->
      print_boxed_integer "mod" ppf size
  | Pmodbint { size; is_safe = Unsafe } ->
      print_boxed_integer "mod_unsafe" ppf size
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
      print_bigarray "get" unsafe kind ppf layout
  | Pbigarrayset(unsafe, _n, kind, layout) ->
      print_bigarray "set" unsafe kind ppf layout
  | Pbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n
  | Pstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get16"
     else fprintf ppf "string.get16"
  | Pstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get32"
     else fprintf ppf "string.get32"
  | Pstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get64"
     else fprintf ppf "string.get64"
  | Pbytes_load_16(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_get16"
     else fprintf ppf "bytes.get16"
  | Pbytes_load_32(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_get32"
     else fprintf ppf "bytes.get32"
  | Pbytes_load_64(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_get64"
     else fprintf ppf "bytes.get64"
  | Pbytes_set_16(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_set16"
     else fprintf ppf "bytes.set16"
  | Pbytes_set_32(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_set32"
     else fprintf ppf "bytes.set32"
  | Pbytes_set_64(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_set64"
     else fprintf ppf "bytes.set64"
  | Pbigstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get16"
     else fprintf ppf "bigarray.array1.get16"
  | Pbigstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get32"
     else fprintf ppf "bigarray.array1.get32"
  | Pbigstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get64"
     else fprintf ppf "bigarray.array1.get64"
  | Pbigstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set16"
     else fprintf ppf "bigarray.array1.set16"
  | Pbigstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set32"
     else fprintf ppf "bigarray.array1.set32"
  | Pbigstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set64"
     else fprintf ppf "bigarray.array1.set64"
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

let name_of_primitive = function
  | Pbytes_of_string -> "Pbytes_of_string"
  | Pbytes_to_string -> "Pbytes_to_string"
  | Pignore -> "Pignore"
  | Pgetglobal _ -> "Pgetglobal"
  | Psetglobal _ -> "Psetglobal"
  | Pmakeblock _ -> "Pmakeblock"
  | Pfield _ -> "Pfield"
  | Pfield_computed -> "Pfield_computed"
  | Psetfield _ -> "Psetfield"
  | Psetfield_computed _ -> "Psetfield_computed"
  | Pfloatfield _ -> "Pfloatfield"
  | Psetfloatfield _ -> "Psetfloatfield"
  | Pduprecord _ -> "Pduprecord"
  | Pccall _ -> "Pccall"
  | Praise _ -> "Praise"
  | Psequand -> "Psequand"
  | Psequor -> "Psequor"
  | Pnot -> "Pnot"
  | Pnegint -> "Pnegint"
  | Paddint -> "Paddint"
  | Psubint -> "Psubint"
  | Pmulint -> "Pmulint"
  | Pdivint _ -> "Pdivint"
  | Pmodint _ -> "Pmodint"
  | Pandint -> "Pandint"
  | Porint -> "Porint"
  | Pxorint -> "Pxorint"
  | Plslint -> "Plslint"
  | Plsrint -> "Plsrint"
  | Pasrint -> "Pasrint"
  | Pintcomp _ -> "Pintcomp"
  | Pcompare_ints -> "Pcompare_ints"
  | Pcompare_floats -> "Pcompare_floats"
  | Pcompare_bints _ -> "Pcompare"
  | Poffsetint _ -> "Poffsetint"
  | Poffsetref _ -> "Poffsetref"
  | Pintoffloat -> "Pintoffloat"
  | Pfloatofint -> "Pfloatofint"
  | Pnegfloat -> "Pnegfloat"
  | Pabsfloat -> "Pabsfloat"
  | Paddfloat -> "Paddfloat"
  | Psubfloat -> "Psubfloat"
  | Pmulfloat -> "Pmulfloat"
  | Pdivfloat -> "Pdivfloat"
  | Pfloatcomp _ -> "Pfloatcomp"
  | Pstringlength -> "Pstringlength"
  | Pstringrefu -> "Pstringrefu"
  | Pstringrefs -> "Pstringrefs"
  | Pbyteslength -> "Pbyteslength"
  | Pbytesrefu -> "Pbytesrefu"
  | Pbytessetu -> "Pbytessetu"
  | Pbytesrefs -> "Pbytesrefs"
  | Pbytessets -> "Pbytessets"
  | Parraylength _ -> "Parraylength"
  | Pmakearray _ -> "Pmakearray"
  | Pduparray _ -> "Pduparray"
  | Parrayrefu _ -> "Parrayrefu"
  | Parraysetu _ -> "Parraysetu"
  | Parrayrefs _ -> "Parrayrefs"
  | Parraysets _ -> "Parraysets"
  | Pctconst _ -> "Pctconst"
  | Pisint -> "Pisint"
  | Pisout -> "Pisout"
  | Pbintofint _ -> "Pbintofint"
  | Pintofbint _ -> "Pintofbint"
  | Pcvtbint _ -> "Pcvtbint"
  | Pnegbint _ -> "Pnegbint"
  | Paddbint _ -> "Paddbint"
  | Psubbint _ -> "Psubbint"
  | Pmulbint _ -> "Pmulbint"
  | Pdivbint _ -> "Pdivbint"
  | Pmodbint _ -> "Pmodbint"
  | Pandbint _ -> "Pandbint"
  | Porbint _ -> "Porbint"
  | Pxorbint _ -> "Pxorbint"
  | Plslbint _ -> "Plslbint"
  | Plsrbint _ -> "Plsrbint"
  | Pasrbint _ -> "Pasrbint"
  | Pbintcomp _ -> "Pbintcomp"
  | Pbigarrayref _ -> "Pbigarrayref"
  | Pbigarrayset _ -> "Pbigarrayset"
  | Pbigarraydim _ -> "Pbigarraydim"
  | Pstring_load_16 _ -> "Pstring_load_16"
  | Pstring_load_32 _ -> "Pstring_load_32"
  | Pstring_load_64 _ -> "Pstring_load_64"
  | Pbytes_load_16 _ -> "Pbytes_load_16"
  | Pbytes_load_32 _ -> "Pbytes_load_32"
  | Pbytes_load_64 _ -> "Pbytes_load_64"
  | Pbytes_set_16 _ -> "Pbytes_set_16"
  | Pbytes_set_32 _ -> "Pbytes_set_32"
  | Pbytes_set_64 _ -> "Pbytes_set_64"
  | Pbigstring_load_16 _ -> "Pbigstring_load_16"
  | Pbigstring_load_32 _ -> "Pbigstring_load_32"
  | Pbigstring_load_64 _ -> "Pbigstring_load_64"
  | Pbigstring_set_16 _ -> "Pbigstring_set_16"
  | Pbigstring_set_32 _ -> "Pbigstring_set_32"
  | Pbigstring_set_64 _ -> "Pbigstring_set_64"
  | Pbswap16 -> "Pbswap16"
  | Pbbswap _ -> "Pbbswap"
  | Pint_as_pointer -> "Pint_as_pointer"
  | Patomic_load {immediate_or_pointer} ->
      (match immediate_or_pointer with
        | Immediate -> "atomic_load_imm"
        | Pointer -> "atomic_load_ptr")
  | Patomic_exchange -> "Patomic_exchange"
  | Patomic_cas -> "Patomic_cas"
  | Patomic_fetch_add -> "Patomic_fetch_add"
  | Popaque -> "Popaque"
  | Prunstack -> "Prunstack"
  | Presume -> "Presume"
  | Pperform -> "Pperform"
  | Preperform -> "Preperform"
  | Pdls_get -> "Pdls_get"
  | Ppoll -> "Ppoll"

let function_attribute ppf t =
  if t.is_a_functor then
    fprintf ppf "is_a_functor@ ";
  if t.stub then
    fprintf ppf "stub@ ";
  begin match t.inline with
  | Default_inline -> ()
  | Always_inline -> fprintf ppf "always_inline@ "
  | Hint_inline -> fprintf ppf "hint_inline@ "
  | Never_inline -> fprintf ppf "never_inline@ "
  | Unroll i -> fprintf ppf "unroll(%i)@ " i
  end;
  begin match t.specialise with
  | Default_specialise -> ()
  | Always_specialise -> fprintf ppf "always_specialise@ "
  | Never_specialise -> fprintf ppf "never_specialise@ "
  end;
  begin match t.local with
  | Default_local -> ()
  | Always_local -> fprintf ppf "always_local@ "
  | Never_local -> fprintf ppf "never_local@ "
  end;
  if t.tmc_candidate then
    fprintf ppf "tail_mod_cons@ ";
  begin match t.poll with
  | Default_poll -> ()
  | Error_poll -> fprintf ppf "error_poll@ "
  end

let apply_tailcall_attribute ppf = function
  | Default_tailcall -> ()
  | Tailcall_expectation true ->
    fprintf ppf " tailcall"
  | Tailcall_expectation false ->
    fprintf ppf " tailcall(false)"

let apply_inlined_attribute ppf = function
  | Default_inline -> ()
  | Always_inline -> fprintf ppf " always_inline"
  | Never_inline -> fprintf ppf " never_inline"
  | Hint_inline -> fprintf ppf " hint_inline"
  | Unroll i -> fprintf ppf " never_inline(%i)" i

let apply_specialised_attribute ppf = function
  | Default_specialise -> ()
  | Always_specialise -> fprintf ppf " always_specialise"
  | Never_specialise -> fprintf ppf " never_specialise"

let rec lam ppf = function
  | Lvar id ->
      Ident.print ppf id
  | Lmutvar id ->
      fprintf ppf "*%a" Ident.print id
  | Lconst cst ->
      struct_const ppf cst
  | Lapply ap ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a%a%a%a)@]" lam ap.ap_func lams ap.ap_args
        apply_tailcall_attribute ap.ap_tailcall
        apply_inlined_attribute ap.ap_inlined
        apply_specialised_attribute ap.ap_specialised
  | Lfunction lfun ->
      lfunction ppf lfun
  | Llet(_, k, id, arg, body)
  | Lmutlet(k, id, arg, body) as l ->
      let let_kind = begin function
        | Llet(str,_,_,_,_) ->
           begin match str with
             Alias -> "a" | Strict -> "" | StrictOpt -> "o"
           end
        | Lmutlet _ -> "mut"
        | _ -> assert false
        end
      in
      let rec letbody = function
        | Llet(_, k, id, arg, body)
        | Lmutlet(k, id, arg, body) as l ->
           fprintf ppf "@ @[<2>%a =%s%a@ %a@]"
             Ident.print id (let_kind l) value_kind k lam arg;
           letbody body
        | expr -> expr in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a =%s%a@ %a@]"
        Ident.print id (let_kind l) value_kind k lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Lletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun { id; def } ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lfunction def)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Lprim(prim, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | Lswitch(larg, sw, _loc) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
         (fun (n, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
         sw.sw_consts;
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.sw_blocks ;
        begin match sw.sw_failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
       "@[<1>(%s %a@ @[<v 0>%a@])@]"
       (match sw.sw_failaction with None -> "switch*" | _ -> "switch")
       lam larg switch sw
  | Lstringswitch(arg, cases, default, _) ->
      let switch ppf cases =
        let spc = ref false in
        List.iter
         (fun (s, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
          cases;
        begin match default with
        | Some default ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam default
        | None -> ()
        end in
      fprintf ppf
       "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" lam arg switch cases
  | Lstaticraise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Lstaticcatch(lbody, (i, vars), lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
        lam lbody i
        (fun ppf vars ->
           List.iter
             (fun (x, k) -> fprintf ppf " %a%a" Ident.print x value_kind k)
             vars
        )
        vars
        lam lhandler
  | Ltrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Lifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Lsequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Lwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Lfor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Lassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Lsend (k, met, obj, largs, _) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Self then "self" else if k = Cached then "cache" else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs
  | Levent(expr, ev) ->
      let kind =
       match ev.lev_kind with
       | Lev_before -> "before"
       | Lev_after _  -> "after"
       | Lev_function -> "funct-body"
       | Lev_pseudo -> "pseudo"
      in
      (* -dno-locations also hides the placement of debug events;
         this is good for the readability of the resulting output (usually
         the end-user goal when using -dno-locations), as it strongly
         reduces the nesting level of subterms. *)
      if not !Clflags.locations then lam ppf expr
      else begin match ev.lev_loc with
      | Loc_unknown ->
        fprintf ppf "@[<2>(%s <unknown location>@ %a)@]" kind lam expr
      | Loc_known {scopes; loc} ->
        fprintf ppf "@[<2>(%s %s %s(%i)%s:%i-%i@ %a)@]" kind
                (Debuginfo.Scoped_location.string_of_scopes scopes)
                loc.Location.loc_start.Lexing.pos_fname
                loc.Location.loc_start.Lexing.pos_lnum
                (if loc.Location.loc_ghost then "<ghost>" else "")
                loc.Location.loc_start.Lexing.pos_cnum
                loc.Location.loc_end.Lexing.pos_cnum
                lam expr
      end
  | Lifused(id, expr) ->
      fprintf ppf "@[<2>(ifused@ %a@ %a)@]" Ident.print id lam expr

and sequence ppf = function
  | Lsequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | l ->
      lam ppf l

and lfunction ppf {kind; params; return; body; attr} =
  let pr_params ppf params =
    match kind with
    | Curried ->
        List.iter (fun (param, k) ->
            fprintf ppf "@ %a%a" Ident.print param value_kind k) params
    | Tupled ->
        fprintf ppf " (";
        let first = ref true in
        List.iter
          (fun (param, k) ->
             if !first then first := false else fprintf ppf ",@ ";
             Ident.print ppf param;
             value_kind ppf k)
          params;
            fprintf ppf ")" in
  fprintf ppf "@[<2>(function%a@ %a%a%a)@]" pr_params params
    function_attribute attr return_kind return lam body


let structured_constant = struct_const

let lambda = lam

let program ppf { code } = lambda ppf code
