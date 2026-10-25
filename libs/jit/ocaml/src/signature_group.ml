(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, Inria Paris                        *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Fold on a signature by syntactic group of items *)

(** Classes and class types generate ghosts signature items, we group them
    together before printing *)
type sig_item =
  {
    src: Types.signature_item;
    post_ghosts: Types.signature_item list
    (** ghost classes types are post-declared *);
  }
let flatten x = x.src :: x.post_ghosts

type core_rec_group =
  | Not_rec of sig_item
  | Rec_group of sig_item list

let rec_items = function
  | Not_rec x -> [x]
  | Rec_group x -> x

(** Private row types are manifested as a sequence of definitions
    preceding a recursive group, we collect them and separate them from the
    syntactic recursive group. *)
type rec_group =
  { pre_ghosts: Types.signature_item list; group:core_rec_group }

let next_group = function
  | [] -> None
  | src :: q ->
      let ghosts, q =
        match src with
        | Types.Sig_class _ ->
            (* a class declaration for [c] is followed by the ghost
               declarations of class type [c], and type [c] *)
            begin match q with
            | ct::t::q -> [ct;t], q
            | _ -> assert false
            end
        | Types.Sig_class_type _  ->
            (* a class type declaration for [ct] is followed by the ghost
               declaration of type [ct] *)
           begin match q with
            | t::q -> [t], q
            | _ -> assert false
           end
        | Types.(Sig_module _ | Sig_value _ | Sig_type _ | Sig_typext _
                | Sig_modtype _) ->
            [],q
      in
      Some({src; post_ghosts=ghosts}, q)

let recursive_sigitem = function
  | Types.Sig_type(ident, _, rs, _)
  | Types.Sig_class(ident,_,rs,_)
  | Types.Sig_class_type (ident,_,rs,_)
  | Types.Sig_module(ident, _, _, rs, _) -> Some (ident,rs)
  | Types.(Sig_value _ | Sig_modtype _ | Sig_typext _ )  -> None

let next x =
  let cons_group pre group q =
    let group = Rec_group (List.rev group) in
    Some({ pre_ghosts=List.rev pre; group },q)
  in
  let rec not_in_group pre l = match next_group l with
    | None ->
        assert (pre=[]);
        None
    | Some(elt, q)  ->
        match recursive_sigitem elt.src with
        | Some (id, _) when Btype.is_row_name (Ident.name id) ->
            not_in_group (elt.src::pre) q
        | None | Some (_, Types.Trec_not) ->
            let sgroup = { pre_ghosts=List.rev pre; group=Not_rec elt } in
            Some (sgroup,q)
        | Some (id, Types.(Trec_first | Trec_next) )  ->
            in_group ~pre ~ids:[id] ~group:[elt] q
  and in_group ~pre ~ids ~group rem = match next_group rem with
    | None -> cons_group pre group []
    | Some (elt,next) ->
        match recursive_sigitem elt.src with
        | Some (id, Types.Trec_next) ->
            in_group ~pre ~ids:(id::ids) ~group:(elt::group) next
        | None | Some (_, Types.(Trec_not|Trec_first)) ->
            cons_group pre group rem
  in
  not_in_group [] x

let seq l = Seq.unfold next l
let iter f l = Seq.iter f (seq l)
let fold f acc l = Seq.fold_left f acc (seq l)

let update_rec_next rs rem =
  match rs with
  | Types.Trec_next -> rem
  | Types.(Trec_first | Trec_not) ->
      match rem with
      | Types.Sig_type (id, decl, Trec_next, priv) :: rem ->
          Types.Sig_type (id, decl, rs, priv) :: rem
      | Types.Sig_module (id, pres, mty, Trec_next, priv) :: rem ->
          Types.Sig_module (id, pres, mty, rs, priv) :: rem
      | _ -> rem

type in_place_patch = {
  ghosts: Types.signature;
  replace_by: Types.signature_item option;
}


let replace_in_place f sg =
  let rec next_group f before signature =
    match next signature with
    | None -> None
    | Some(item,sg) ->
        core_group f ~before ~ghosts:item.pre_ghosts ~before_group:[]
          (rec_items item.group) ~sg
  and core_group f ~before ~ghosts ~before_group current ~sg =
    let commit ghosts = before_group @ List.rev_append ghosts before in
    match current with
    | [] -> next_group f (commit ghosts) sg
    | a :: q ->
        match f ~ghosts a.src with
        | Some (info, {ghosts; replace_by}) ->
            let after = List.concat_map flatten q @ sg in
            let after = match recursive_sigitem a.src, replace_by with
              | None, _ | _, Some _ -> after
              | Some (_,rs), None -> update_rec_next rs after
            in
            let before = match replace_by with
              | None -> commit ghosts
              | Some x -> x :: commit ghosts
            in
            let sg = List.rev_append before after in
            Some(info, sg)
        | None ->
            let before_group =
              List.rev_append a.post_ghosts (a.src :: before_group)
            in
            core_group f ~before ~ghosts ~before_group q ~sg
  in
  next_group f [] sg
