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

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

let no_effects_prim (prim : Clambda_primitives.primitive) =
  match Semantics_of_primitives.for_primitive prim with
  | (No_effects | Only_generative_effects), (No_coeffects | Has_coeffects) ->
    true
  | _ -> false

let rec no_effects (flam : Flambda.t) =
  match flam with
  | Var _ -> true
  | Let { defining_expr; body; _ } ->
    no_effects_named defining_expr && no_effects body
  | Let_mutable { body } -> no_effects body
  | If_then_else (_, ifso, ifnot) -> no_effects ifso && no_effects ifnot
  | Switch (_, sw) ->
    let aux (_, flam) = no_effects flam in
    List.for_all aux sw.blocks
      && List.for_all aux sw.consts
      && Option.fold ~some:no_effects ~none:true sw.failaction
  | String_switch (_, sw, def) ->
    List.for_all (fun (_, lam) -> no_effects lam) sw
      && Option.fold ~some:no_effects ~none:true def
  | Static_catch (_, _, body, _) | Try_with (body, _, _) ->
    (* If there is a [raise] in [body], the whole [Try_with] may have an
       effect, so there is no need to test the handler. *)
    no_effects body
  | While _ | For _ | Apply _ | Send _ | Assign _ | Static_raise _ -> false
  | Proved_unreachable -> true

and no_effects_named (named : Flambda.named) =
  match named with
  | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
  | Read_symbol_field _
  | Set_of_closures _ | Project_closure _ | Project_var _
  | Move_within_set_of_closures _ -> true
  | Prim (prim, _, _) -> no_effects_prim prim
  | Expr flam -> no_effects flam
