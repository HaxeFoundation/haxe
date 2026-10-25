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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Types
open Btype

(* Simplified version of Ctype.free_vars *)
let free_vars ?(param=false) ty =
  let ret = ref TypeSet.empty in
  with_type_mark begin fun mark ->
    let rec loop ty =
      if try_mark_node mark ty then
        match get_desc ty with
        | Tvar _ ->
            ret := TypeSet.add ty !ret
        | Tvariant row ->
            iter_row loop row;
            if not (static_row row) then begin
              match get_desc (row_more row) with
              | Tvar _ when param -> ret := TypeSet.add ty !ret
              | _ -> loop (row_more row)
            end
                (* XXX: What about Tobject ? *)
        | _ ->
            iter_type_expr loop ty
    in
    loop ty
  end;
  !ret

let newgenconstr path tyl = newgenty (Tconstr (path, tyl, ref Mnil))

let constructor_existentials cd_args cd_res =
  let tyl =
    match cd_args with
    | Cstr_tuple l -> l
    | Cstr_record l -> List.map (fun l -> l.ld_type) l
  in
  let existentials =
    match cd_res with
    | None -> []
    | Some type_ret ->
        let arg_vars_set = free_vars (newgenty (Ttuple tyl)) in
        let res_vars = free_vars type_ret in
        TypeSet.elements (TypeSet.diff arg_vars_set res_vars)
  in
  (tyl, existentials)

let constructor_args ~current_unit priv cd_args cd_res path rep =
  let tyl, existentials = constructor_existentials cd_args cd_res in
  match cd_args with
  | Cstr_tuple l -> existentials, l, None
  | Cstr_record lbls ->
      let arg_vars_set = free_vars ~param:true (newgenty (Ttuple tyl)) in
      let type_params = TypeSet.elements arg_vars_set in
      let arity = List.length type_params in
      let tdecl =
        {
          type_params;
          type_arity = arity;
          type_kind = Type_record (lbls, rep);
          type_private = priv;
          type_manifest = None;
          type_variance = Variance.unknown_signature ~injective:true ~arity;
          type_separability = Types.Separability.default_signature ~arity;
          type_is_newtype = false;
          type_expansion_scope = Btype.lowest_level;
          type_loc = Location.none;
          type_attributes = [];
          type_immediate = Unknown;
          type_unboxed_default = false;
          type_uid = Uid.mk ~current_unit;
        }
      in
      existentials,
      [ newgenconstr path type_params ],
      Some tdecl

let constructor_descrs ~current_unit ty_path decl cstrs rep =
  let ty_res = newgenconstr ty_path decl.type_params in
  let num_consts = ref 0 and num_nonconsts = ref 0 in
  List.iter
    (fun {cd_args; _} ->
      if cd_args = Cstr_tuple [] then incr num_consts else incr num_nonconsts)
    cstrs;
  let rec describe_constructors idx_const idx_nonconst = function
      [] -> []
    | {cd_id; cd_args; cd_res; cd_loc; cd_attributes; cd_uid} :: rem ->
        let ty_res =
          match cd_res with
          | Some ty_res' -> ty_res'
          | None -> ty_res
        in
        let (tag, descr_rem) =
          match cd_args, rep with
          | _, Variant_unboxed ->
            assert (rem = []);
            (Cstr_unboxed, [])
          | Cstr_tuple [], Variant_regular ->
             (Cstr_constant idx_const,
              describe_constructors (idx_const+1) idx_nonconst rem)
          | _, Variant_regular  ->
             (Cstr_block idx_nonconst,
              describe_constructors idx_const (idx_nonconst+1) rem) in
        let cstr_name = Ident.name cd_id in
        let existentials, cstr_args, cstr_inlined =
          let representation =
            match rep with
            | Variant_unboxed -> Record_unboxed true
            | Variant_regular -> Record_inlined idx_nonconst
          in
          constructor_args ~current_unit decl.type_private cd_args cd_res
            Path.(Pextra_ty (ty_path, Pcstr_ty cstr_name)) representation
        in
        let cstr =
          { cstr_name;
            cstr_res = ty_res;
            cstr_existentials = existentials;
            cstr_args;
            cstr_arity = List.length cstr_args;
            cstr_tag = tag;
            cstr_consts = !num_consts;
            cstr_nonconsts = !num_nonconsts;
            cstr_private = decl.type_private;
            cstr_generalized = cd_res <> None;
            cstr_loc = cd_loc;
            cstr_attributes = cd_attributes;
            cstr_inlined;
            cstr_uid = cd_uid;
          } in
        (cd_id, cstr) :: descr_rem in
  describe_constructors 0 0 cstrs

let extension_descr ~current_unit path_ext ext =
  let ty_res =
    match ext.ext_ret_type with
        Some type_ret -> type_ret
      | None -> newgenconstr ext.ext_type_path ext.ext_type_params
  in
  let existentials, cstr_args, cstr_inlined =
    constructor_args ~current_unit ext.ext_private ext.ext_args ext.ext_ret_type
      Path.(Pextra_ty (path_ext, Pext_ty)) (Record_extension path_ext)
  in
    { cstr_name = Path.last path_ext;
      cstr_res = ty_res;
      cstr_existentials = existentials;
      cstr_args;
      cstr_arity = List.length cstr_args;
      cstr_tag = Cstr_extension(path_ext, cstr_args = []);
      cstr_consts = -1;
      cstr_nonconsts = -1;
      cstr_private = ext.ext_private;
      cstr_generalized = ext.ext_ret_type <> None;
      cstr_loc = ext.ext_loc;
      cstr_attributes = ext.ext_attributes;
      cstr_inlined;
      cstr_uid = ext.ext_uid;
    }

let none =
  create_expr (Ttuple []) ~level:(-1) ~scope:Btype.generic_level ~id:(-1)
    (* Clearly ill-formed type *)

let dummy_label =
  { lbl_name = ""; lbl_res = none; lbl_arg = none; lbl_mut = Immutable;
    lbl_pos = (-1); lbl_all = [||]; lbl_repres = Record_regular;
    lbl_private = Public;
    lbl_loc = Location.none;
    lbl_attributes = [];
    lbl_uid = Uid.internal_not_actually_unique;
  }

let label_descrs ty_res lbls repres priv =
  let all_labels = Array.make (List.length lbls) dummy_label in
  let rec describe_labels num = function
      [] -> []
    | l :: rest ->
        let lbl =
          { lbl_name = Ident.name l.ld_id;
            lbl_res = ty_res;
            lbl_arg = l.ld_type;
            lbl_mut = l.ld_mutable;
            lbl_pos = num;
            lbl_all = all_labels;
            lbl_repres = repres;
            lbl_private = priv;
            lbl_loc = l.ld_loc;
            lbl_attributes = l.ld_attributes;
            lbl_uid = l.ld_uid;
          } in
        all_labels.(num) <- lbl;
        (l.ld_id, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls

exception Constr_not_found

let rec find_constr tag num_const num_nonconst = function
    [] ->
      raise Constr_not_found
  | {cd_args = Cstr_tuple []; _} as c  :: rem ->
      if tag = Cstr_constant num_const
      then c
      else find_constr tag (num_const + 1) num_nonconst rem
  | c :: rem ->
      if tag = Cstr_block num_nonconst || tag = Cstr_unboxed
      then c
      else find_constr tag num_const (num_nonconst + 1) rem

let find_constr_by_tag tag cstrlist =
  find_constr tag 0 0 cstrlist

let constructors_of_type ~current_unit ty_path decl =
  match decl.type_kind with
  | Type_variant (cstrs,rep) ->
     constructor_descrs ~current_unit ty_path decl cstrs rep
  | Type_record _ | Type_abstract _ | Type_open -> []

let labels_of_type ty_path decl =
  match decl.type_kind with
  | Type_record(labels, rep) ->
      label_descrs (newgenconstr ty_path decl.type_params)
        labels rep decl.type_private
  | Type_variant _ | Type_abstract _ | Type_open -> []
