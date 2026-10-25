(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Basic operations on core types *)

open Asttypes
open Types

open Local_store

(**** Sets, maps and hashtables of types ****)

let wrap_repr f ty = f (Transient_expr.repr ty)
let wrap_type_expr f tty = f (Transient_expr.type_expr tty)

module TransientTypeSet = Set.Make(TransientTypeOps)
module TypeSet = struct
  include TransientTypeSet
  let add = wrap_repr add
  let mem = wrap_repr mem
  let singleton = wrap_repr singleton
  let exists p = TransientTypeSet.exists (wrap_type_expr p)
  let elements set =
    List.map Transient_expr.type_expr (TransientTypeSet.elements set)
end
module TransientTypeMap = Map.Make(TransientTypeOps)
module TypeMap = struct
  include TransientTypeMap
  let add ty = wrap_repr add ty
  let find ty = wrap_repr find ty
  let singleton ty = wrap_repr singleton ty
  let fold f = TransientTypeMap.fold (wrap_type_expr f)
end
module TypeHash = struct
  include TransientTypeHash
  let mem hash = wrap_repr (mem hash)
  let add hash = wrap_repr (add hash)
  let remove hash = wrap_repr (remove hash)
  let find hash = wrap_repr (find hash)
  let find_opt hash = wrap_repr (find_opt hash)
  let iter f = TransientTypeHash.iter (wrap_type_expr f)
end
module TransientTypePairs =
  Hashtbl.Make (struct
    type t = transient_expr * transient_expr
    let equal (t1, t1') (t2, t2') = (t1 == t2) && (t1' == t2')
    let hash (t, t') = t.id + 93 * t'.id
 end)
module TypePairs = struct
  module H = TransientTypePairs
  open Transient_expr

  type t = {
    set : unit H.t;
    mutable elems : (transient_expr * transient_expr) list;
    (* elems preserves the (reversed) insertion order of elements *)
  }

  let create n =
    { elems = []; set = H.create n }

  let clear t =
    t.elems <- [];
    H.clear t.set

  let repr2 (t1, t2) = (repr t1, repr t2)

  let add t p =
    let p = repr2 p in
    if H.mem t.set p then () else begin
      H.add t.set p ();
      t.elems <- p :: t.elems
    end

  let mem t p = H.mem t.set (repr2 p)

  let iter f t =
    (* iterate in insertion order, not Hashtbl.iter order *)
    List.rev t.elems
    |> List.iter (fun (t1,t2) ->
        f (type_expr t1, type_expr t2))
end

(**** Type level management ****)

let generic_level = Ident.highest_scope
let lowest_level = Ident.lowest_scope

(**** leveled type pool ****)
(* This defines a stack of pools of type nodes indexed by the level
   we will try to generalize them in [Ctype.with_local_level_gen].
   [pool_of_level] returns the pool in which types at level [level]
   should be kept, which is the topmost pool whose level is lower or
   equal to [level].
   [Ctype.with_local_level_gen] shall call [with_new_pool] to create
   a new pool at a given level. On return it shall process all nodes
   that were added to the pool.
   Remark: the only function adding to a pool is [add_to_pool], and
   the only function returning the contents of a pool is [with_new_pool],
   so that the initial pool can be added to, but never read from. *)

type pool = {level: int; mutable pool: transient_expr list; next: pool}
(* To avoid an indirection we choose to add a dummy level at the end of
   the list. It will never be accessed, as [pool_of_level] is always called
   with [level >= 0]. *)
let rec dummy = {level = max_int; pool = []; next = dummy}
let pool_stack = s_table (fun () -> {level = 0; pool = []; next = dummy}) ()

(* Lookup in the stack is linear, but the depth is the number of nested
   generalization points (e.g. lhs of let-definitions), which in ML is known
   to be generally low. In most cases we are allocating in the topmost pool.
   In [Ctype.with_local_gen], we move non-generalizable type nodes from the
   topmost pool to one deeper in the stack, so that for each type node the
   accumulated depth of lookups over its life is bounded by the depth of
   the stack when it was allocated.
   In case this linear search turns out to be costly, we could switch to
   binary search, exploiting the fact that the levels of pools in the stack
   are expected to grow. *)
let rec pool_of_level level pool =
  if level >= pool.level then pool else pool_of_level level pool.next

(* Create a new pool at given level, and use it locally. *)
let with_new_pool ~level f =
  let pool = {level; pool = []; next = !pool_stack} in
  let r =
    Misc.protect_refs [ R(pool_stack, pool) ] f
  in
  (r, pool.pool)

let add_to_pool ~level ty =
  if level >= generic_level || level <= lowest_level then () else
  let pool = pool_of_level level !pool_stack in
  pool.pool <- ty :: pool.pool

(**** Some type creators ****)

let newty3 ~level ~scope desc =
  let ty = proto_newty3 ~level ~scope desc in
  add_to_pool ~level ty;
  Transient_expr.type_expr ty

let newty2 ~level desc =
  newty3 ~level ~scope:Ident.lowest_scope desc

let newgenty desc      = newty2 ~level:generic_level desc
let newgenvar ?name () = newgenty (Tvar name)
let newgenstub ~scope  = newty3 ~level:generic_level ~scope (Tvar None)

(**** Check some types ****)

let is_Tvar ty = match get_desc ty with Tvar _ -> true | _ -> false
let is_Tunivar ty = match get_desc ty with Tunivar _ -> true | _ -> false
let is_Tconstr ty = match get_desc ty with Tconstr _ -> true | _ -> false
let is_poly_Tpoly ty =
  match get_desc ty with Tpoly (_, _ :: _) -> true | _ -> false
let type_kind_is_abstract decl =
  match decl.type_kind with Type_abstract _ -> true | _ -> false
let type_origin decl =
  match decl.type_kind with
  | Type_abstract origin -> origin
  | Type_variant _ | Type_record _ | Type_open -> Definition
let label_is_poly lbl = is_poly_Tpoly lbl.lbl_arg

let dummy_method = "*dummy method*"

(**** Representative of a type ****)

let merge_fixed_explanation fixed1 fixed2 =
  match fixed1, fixed2 with
  | Some Univar _ as x, _ | _, (Some Univar _ as x) -> x
  | Some Fixed_private as x, _ | _, (Some Fixed_private as x) -> x
  | Some Reified _ as x, _ | _, (Some Reified _ as x) -> x
  | Some Rigid as x, _ | _, (Some Rigid as x) -> x
  | None, None -> None


let fixed_explanation row =
  match row_fixed row with
  | Some _ as x -> x
  | None ->
      let ty = row_more row in
      match get_desc ty with
      | Tvar _ | Tnil -> None
      | Tunivar _ -> Some (Univar ty)
      | Tconstr (p,_,_) -> Some (Reified p)
      | _ -> assert false

let is_fixed row = match row_fixed row with
  | None -> false
  | Some _ -> true

let has_fixed_explanation row = fixed_explanation row <> None

let static_row row =
  row_closed row &&
  List.for_all
    (fun (_,f) -> match row_field_repr f with Reither _ -> false | _ -> true)
    (row_fields row)

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let proxy ty =
  match get_desc ty with
  | Tvariant row when not (static_row row) ->
      row_more row
  | Tobject (ty, _) ->
      let rec proxy_obj ty =
        match get_desc ty with
          Tfield (_, _, _, ty) -> proxy_obj ty
        | Tvar _ | Tunivar _ | Tconstr _ -> ty
        | Tnil -> ty
        | _ -> assert false
      in proxy_obj ty
  | _ -> ty

(**** Utilities for fixed row private types ****)

let row_of_type t =
  match get_desc t with
    Tobject(t,_) ->
      let rec get_row t =
        match get_desc t with
          Tfield(_,_,_,t) -> get_row t
        | _ -> t
      in get_row t
  | Tvariant row ->
      row_more row
  | _ ->
      t

let has_constr_row t =
  not (is_Tconstr t) && is_Tconstr (row_of_type t)

let is_row_name s =
  let l = String.length s in
  (* PR#10661: when l=4 and s is "#row", this is not a row name
     but the valid #-type name of a class named "row". *)
  l > 4 && String.sub s (l-4) 4 = "#row"

let is_constr_row ~allow_ident t =
  match get_desc t with
    Tconstr (Path.Pident id, _, _) when allow_ident ->
      is_row_name (Ident.name id)
  | Tconstr (Path.Pdot (_, s), _, _) -> is_row_name s
  | _ -> false

(* TODO: where should this really be *)
(* Set row_name in Env, cf. GPR#1204/1329 *)
let set_static_row_name decl path =
  match decl.type_manifest with
    None -> ()
  | Some ty ->
      match get_desc ty with
        Tvariant row when static_row row ->
          let row =
            set_row_name row (Some (path, decl.type_params)) in
          set_type_desc ty (Tvariant row)
      | _ -> ()

                  (**********************************)
                  (*  Utilities for type traversal  *)
                  (**********************************)

let fold_row f init row =
  let result =
    List.fold_left
      (fun init (_, fi) ->
         match row_field_repr fi with
         | Rpresent(Some ty) -> f init ty
         | Reither(_, tl, _) -> List.fold_left f init tl
         | _ -> init)
      init
      (row_fields row)
  in
  match get_desc (row_more row) with
  | Tvar _ | Tunivar _ | Tsubst _ | Tconstr _ | Tnil ->
    begin match
      Option.map (fun (_,l) -> List.fold_left f result l) (row_name row)
    with
    | None -> result
    | Some result -> result
    end
  | _ -> assert false

let iter_row f row =
  fold_row (fun () v -> f v) () row

let fold_type_expr f init ty =
  match get_desc ty with
    Tvar _              -> init
  | Tarrow (_, ty1, ty2, _) ->
      let result = f init ty1 in
      f result ty2
  | Ttuple l            -> List.fold_left f init l
  | Tconstr (_, l, _)   -> List.fold_left f init l
  | Tobject(ty, {contents = Some (_, p)}) ->
      let result = f init ty in
      List.fold_left f result p
  | Tobject (ty, _)     -> f init ty
  | Tvariant row        ->
      let result = fold_row f init row in
      f result (row_more row)
  | Tfield (_, _, ty1, ty2) ->
      let result = f init ty1 in
      f result ty2
  | Tnil                -> init
  | Tlink _
  | Tsubst _            -> assert false
  | Tunivar _           -> init
  | Tpoly (ty, tyl)     ->
    let result = f init ty in
    List.fold_left f result tyl
  | Tpackage (_, fl)  ->
    List.fold_left (fun result (_n, ty) -> f result ty) init fl

let iter_type_expr f ty =
  fold_type_expr (fun () v -> f v) () ty

let rec iter_abbrev f = function
    Mnil                   -> ()
  | Mcons(_, _, ty, ty', rem) -> f ty; f ty'; iter_abbrev f rem
  | Mlink rem              -> iter_abbrev f !rem

let iter_type_expr_cstr_args f = function
  | Cstr_tuple tl -> List.iter f tl
  | Cstr_record lbls -> List.iter (fun d -> f d.ld_type) lbls

let map_type_expr_cstr_args f = function
  | Cstr_tuple tl -> Cstr_tuple (List.map f tl)
  | Cstr_record lbls ->
      Cstr_record (List.map (fun d -> {d with ld_type=f d.ld_type}) lbls)

let iter_type_expr_kind f = function
  | Type_abstract _ -> ()
  | Type_variant (cstrs, _) ->
      List.iter
        (fun cd ->
           iter_type_expr_cstr_args f cd.cd_args;
           Option.iter f cd.cd_res
        )
        cstrs
  | Type_record(lbls, _) ->
      List.iter (fun d -> f d.ld_type) lbls
  | Type_open ->
      ()

                  (**********************************)
                  (*     Utilities for marking      *)
                  (**********************************)

let rec mark_type mark ty =
  if try_mark_node mark ty then iter_type_expr (mark_type mark) ty

let mark_type_params mark ty =
  iter_type_expr (mark_type mark) ty

                  (**********************************)
                  (*  (Object-oriented) iterator    *)
                  (**********************************)

type 'a type_iterators =
  { it_signature: 'a type_iterators -> signature -> unit;
    it_signature_item: 'a type_iterators -> signature_item -> unit;
    it_value_description: 'a type_iterators -> value_description -> unit;
    it_type_declaration: 'a type_iterators -> type_declaration -> unit;
    it_extension_constructor:
        'a type_iterators -> extension_constructor -> unit;
    it_module_declaration: 'a type_iterators -> module_declaration -> unit;
    it_modtype_declaration: 'a type_iterators -> modtype_declaration -> unit;
    it_class_declaration: 'a type_iterators -> class_declaration -> unit;
    it_class_type_declaration:
        'a type_iterators -> class_type_declaration -> unit;
    it_functor_param: 'a type_iterators -> functor_parameter -> unit;
    it_module_type: 'a type_iterators -> module_type -> unit;
    it_class_type: 'a type_iterators -> class_type -> unit;
    it_type_kind: 'a type_iterators -> type_decl_kind -> unit;
    it_do_type_expr: 'a type_iterators -> 'a;
    it_type_expr: 'a type_iterators -> type_expr -> unit;
    it_path: Path.t -> unit; }

type type_iterators_full = (type_expr -> unit) type_iterators
type type_iterators_without_type_expr = (unit -> unit) type_iterators

let type_iterators_without_type_expr =
  let it_signature it =
    List.iter (it.it_signature_item it)
  and it_signature_item it = function
      Sig_value (_, vd, _)          -> it.it_value_description it vd
    | Sig_type (_, td, _, _)        -> it.it_type_declaration it td
    | Sig_typext (_, td, _, _)      -> it.it_extension_constructor it td
    | Sig_module (_, _, md, _, _)   -> it.it_module_declaration it md
    | Sig_modtype (_, mtd, _)       -> it.it_modtype_declaration it mtd
    | Sig_class (_, cd, _, _)       -> it.it_class_declaration it cd
    | Sig_class_type (_, ctd, _, _) -> it.it_class_type_declaration it ctd
  and it_value_description it vd =
    it.it_type_expr it vd.val_type
  and it_type_declaration it td =
    List.iter (it.it_type_expr it) td.type_params;
    Option.iter (it.it_type_expr it) td.type_manifest;
    it.it_type_kind it td.type_kind
  and it_extension_constructor it td =
    it.it_path td.ext_type_path;
    List.iter (it.it_type_expr it) td.ext_type_params;
    iter_type_expr_cstr_args (it.it_type_expr it) td.ext_args;
    Option.iter (it.it_type_expr it) td.ext_ret_type
  and it_module_declaration it md =
    it.it_module_type it md.md_type
  and it_modtype_declaration it mtd =
    Option.iter (it.it_module_type it) mtd.mtd_type
  and it_class_declaration it cd =
    List.iter (it.it_type_expr it) cd.cty_params;
    it.it_class_type it cd.cty_type;
    Option.iter (it.it_type_expr it) cd.cty_new;
    it.it_path cd.cty_path
  and it_class_type_declaration it ctd =
    List.iter (it.it_type_expr it) ctd.clty_params;
    it.it_class_type it ctd.clty_type;
    it.it_path ctd.clty_path
  and it_functor_param it = function
    | Unit -> ()
    | Named (_, mt) -> it.it_module_type it mt
  and it_module_type it = function
      Mty_ident p
    | Mty_alias p -> it.it_path p
    | Mty_signature sg -> it.it_signature it sg
    | Mty_functor (p, mt) ->
        it.it_functor_param it p;
        it.it_module_type it mt
  and it_class_type it = function
      Cty_constr (p, tyl, cty) ->
        it.it_path p;
        List.iter (it.it_type_expr it) tyl;
        it.it_class_type it cty
    | Cty_signature cs ->
        it.it_type_expr it cs.csig_self;
        it.it_type_expr it cs.csig_self_row;
        Vars.iter (fun _ (_,_,ty) -> it.it_type_expr it ty) cs.csig_vars;
        Meths.iter (fun _ (_,_,ty) -> it.it_type_expr it ty) cs.csig_meths
    | Cty_arrow  (_, ty, cty) ->
        it.it_type_expr it ty;
        it.it_class_type it cty
  and it_type_kind it kind =
    iter_type_expr_kind (it.it_type_expr it) kind
  and it_path _p = ()
  in
  { it_path; it_type_expr = (fun _ _ -> ()); it_do_type_expr = (fun _ _ -> ());
    it_type_kind; it_class_type; it_functor_param; it_module_type;
    it_signature; it_class_type_declaration; it_class_declaration;
    it_modtype_declaration; it_module_declaration; it_extension_constructor;
    it_type_declaration; it_value_description; it_signature_item; }

let type_iterators mark =
  let it_type_expr it ty =
    if try_mark_node mark ty then it.it_do_type_expr it ty
  and it_do_type_expr it ty =
    iter_type_expr (it.it_type_expr it) ty;
    match get_desc ty with
      Tconstr (p, _, _)
    | Tobject (_, {contents=Some (p, _)})
    | Tpackage (p, _) ->
        it.it_path p
    | Tvariant row ->
        Option.iter (fun (p,_) -> it.it_path p) (row_name row)
    | _ -> ()
  in
  {type_iterators_without_type_expr with it_type_expr; it_do_type_expr}

                  (**********************************)
                  (*  Utilities for copying         *)
                  (**********************************)

let copy_row f fixed row keep more =
  let Row {fields = orig_fields; fixed = orig_fixed; closed; name = orig_name} =
    row_repr row in
  let fields = List.map
      (fun (l, fi) -> l,
        match row_field_repr fi with
        | Rpresent oty -> rf_present (Option.map f oty)
        | Reither(c, tl, m) ->
            let use_ext_of = if keep then Some fi else None in
            let m = if is_fixed row then fixed else m in
            let tl = List.map f tl in
            rf_either tl ?use_ext_of ~no_arg:c ~matched:m
        | Rabsent -> rf_absent)
      orig_fields in
  let name =
    match orig_name with
    | None -> None
    | Some (path, tl) -> Some (path, List.map f tl) in
  let fixed = if fixed then orig_fixed else None in
  create_row ~fields ~more ~fixed ~closed ~name

let copy_commu c = if is_commu_ok c then commu_ok else commu_var ()

let rec copy_type_desc ?(keep_names=false) f = function
    Tvar _ as ty        -> if keep_names then ty else Tvar None
  | Tarrow (p, ty1, ty2, c)-> Tarrow (p, f ty1, f ty2, copy_commu c)
  | Ttuple l            -> Ttuple (List.map f l)
  | Tconstr (p, l, _)   -> Tconstr (p, List.map f l, ref Mnil)
  | Tobject(ty, {contents = Some (p, tl)})
                        -> Tobject (f ty, ref (Some(p, List.map f tl)))
  | Tobject (ty, _)     -> Tobject (f ty, ref None)
  | Tvariant _          -> assert false (* too ambiguous *)
  | Tfield (p, k, ty1, ty2) ->
      Tfield (p, field_kind_internal_repr k, f ty1, f ty2)
      (* the kind is kept shared, with indirections removed for performance *)
  | Tnil                -> Tnil
  | Tlink ty            -> copy_type_desc f (get_desc ty)
  | Tsubst _            -> assert false
  | Tunivar _ as ty     -> ty (* always keep the name *)
  | Tpoly (ty, tyl)     ->
      let tyl = List.map f tyl in
      Tpoly (f ty, tyl)
  | Tpackage (p, fl)  -> Tpackage (p, List.map (fun (n, ty) -> (n, f ty)) fl)

(* TODO: rename to [module Copy_scope] *)
module For_copy : sig
  type copy_scope

  val redirect_desc: copy_scope -> type_expr -> type_desc -> unit

  val with_scope: (copy_scope -> 'a) -> 'a
end = struct
  type copy_scope = {
    mutable saved_desc : (transient_expr * type_desc) list;
    (* Save association of generic nodes with their description. *)
  }

  let redirect_desc copy_scope ty desc =
    let ty = Transient_expr.repr ty in
    copy_scope.saved_desc <- (ty, ty.desc) :: copy_scope.saved_desc;
    Transient_expr.set_desc ty desc

  (* Restore type descriptions. *)
  let cleanup { saved_desc; _ } =
    List.iter (fun (ty, desc) -> Transient_expr.set_desc ty desc) saved_desc

  let with_scope f =
    let scope = { saved_desc = [] } in
    Fun.protect ~finally:(fun () -> cleanup scope) (fun () -> f scope)

end

                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

(* Search whether the expansion has been memorized. *)

let lte_public p1 p2 =  (* Private <= Public *)
  match p1, p2 with
  | Private, _ | _, Public -> true
  | Public, Private -> false

let rec find_expans priv p1 = function
    Mnil -> None
  | Mcons (priv', p2, _ty0, ty, _)
    when lte_public priv priv' && Path.same p1 p2 -> Some ty
  | Mcons (_, _, _, _, rem)   -> find_expans priv p1 rem
  | Mlink {contents = rem} -> find_expans priv p1 rem

(* debug: check for cycles in abbreviation. only works with -principal
let rec check_expans visited ty =
  let ty = repr ty in
  assert (not (List.memq ty visited));
  match ty.desc with
    Tconstr (path, args, abbrev) ->
      begin match find_expans path !abbrev with
        Some ty' -> check_expans (ty :: visited) ty'
      | None -> ()
      end
  | _ -> ()
*)

let memo = s_ref []
        (* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem priv path v v' =
        (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (priv, path, v, v', !mem);
  (* check_expans [] v; *)
  memo := mem :: !memo

let rec forget_abbrev_rec mem path =
  match mem with
    Mnil ->
      mem
  | Mcons (_, path', _, _, rem) when Path.same path path' ->
      rem
  | Mcons (priv, path', v, v', rem) ->
      Mcons (priv, path', v, v', forget_abbrev_rec rem path)
  | Mlink mem' ->
      mem' := forget_abbrev_rec !mem' path;
      raise Exit

let forget_abbrev mem path =
  try mem := forget_abbrev_rec !mem path with Exit -> ()

(* debug: check for invalid abbreviations
let rec check_abbrev_rec = function
    Mnil -> true
  | Mcons (_, ty1, ty2, rem) ->
      repr ty1 != repr ty2
  | Mlink mem' ->
      check_abbrev_rec !mem'

let check_memorized_abbrevs () =
  List.for_all (fun mem -> check_abbrev_rec !mem) !memo
*)

(* Re-export backtrack *)

let snapshot = snapshot
let backtrack = backtrack ~cleanup_abbrev

                  (**********************************)
                  (*  Utilities for labels          *)
                  (**********************************)

let is_optional = function Optional _ -> true | _ -> false

let label_name = function
    Nolabel -> ""
  | Labelled s
  | Optional s -> s

let prefixed_label_name = function
    Nolabel -> ""
  | Labelled s -> "~" ^ s
  | Optional s -> "?" ^ s

let rec extract_label_aux hd l = function
  | [] -> None
  | (l',t as p) :: ls ->
      if label_name l' = l then
        Some (l', t, hd <> [], List.rev_append hd ls)
      else
        extract_label_aux (p::hd) l ls

let extract_label l ls = extract_label_aux [] l ls

                              (*******************************)
                              (*  Operations on class types  *)
                              (*******************************)

let rec signature_of_class_type =
  function
    Cty_constr (_, _, cty) -> signature_of_class_type cty
  | Cty_signature sign     -> sign
  | Cty_arrow (_, _, cty)   -> signature_of_class_type cty

let rec class_body cty =
  match cty with
    Cty_constr _ ->
      cty (* Only class bodies can be abbreviated *)
  | Cty_signature _ ->
      cty
  | Cty_arrow (_, _, cty) ->
      class_body cty

(* Fully expand the head of a class type *)
let rec scrape_class_type =
  function
    Cty_constr (_, _, cty) -> scrape_class_type cty
  | cty                     -> cty

let rec class_type_arity =
  function
    Cty_constr (_, _, cty) ->  class_type_arity cty
  | Cty_signature _        ->  0
  | Cty_arrow (_, _, cty)    ->  1 + class_type_arity cty

let rec abbreviate_class_type path params cty =
  match cty with
    Cty_constr (_, _, _) | Cty_signature _ ->
      Cty_constr (path, params, cty)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, ty, abbreviate_class_type path params cty)

let self_type cty =
  (signature_of_class_type cty).csig_self

let self_type_row cty =
  (signature_of_class_type cty).csig_self_row

(* Return the methods of a class signature *)
let methods sign =
  Meths.fold
    (fun name _ l -> name :: l)
    sign.csig_meths []

(* Return the virtual methods of a class signature *)
let virtual_methods sign =
  Meths.fold
    (fun name (_priv, vr, _ty) l ->
       match vr with
       | Virtual -> name :: l
       | Concrete -> l)
    sign.csig_meths []

(* Return the concrete methods of a class signature *)
let concrete_methods sign =
  Meths.fold
    (fun name (_priv, vr, _ty) s ->
       match vr with
       | Virtual -> s
       | Concrete -> MethSet.add name s)
    sign.csig_meths MethSet.empty

(* Return the public methods of a class signature *)
let public_methods sign =
  Meths.fold
    (fun name (priv, _vr, _ty) l ->
       match priv with
       | Mprivate _ -> l
       | Mpublic -> name :: l)
    sign.csig_meths []

(* Return the instance variables of a class signature *)
let instance_vars sign =
  Vars.fold
    (fun name _ l -> name :: l)
    sign.csig_vars []

(* Return the virtual instance variables of a class signature *)
let virtual_instance_vars sign =
  Vars.fold
    (fun name (_mut, vr, _ty) l ->
       match vr with
       | Virtual -> name :: l
       | Concrete -> l)
    sign.csig_vars []

(* Return the concrete instance variables of a class signature *)
let concrete_instance_vars sign =
  Vars.fold
    (fun name (_mut, vr, _ty) s ->
       match vr with
       | Virtual -> s
       | Concrete -> VarSet.add name s)
    sign.csig_vars VarSet.empty

let method_type label sign =
  match Meths.find label sign.csig_meths with
  | (_, _, ty) -> ty
  | exception Not_found -> assert false

let instance_variable_type label sign =
  match Vars.find label sign.csig_vars with
  | (_, _, ty) -> ty
  | exception Not_found -> assert false


                  (**********)
                  (*  Misc  *)
                  (**********)

(**** Type information getter ****)

let cstr_type_path cstr =
  match get_desc cstr.cstr_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false
