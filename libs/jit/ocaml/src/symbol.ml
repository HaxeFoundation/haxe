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


type t =
  | Linkage of
      { compilation_unit : Compilation_unit.t;
        label : Linkage_name.t;
        hash : int; }
  | Variable of
      { compilation_unit : Compilation_unit.t;
        variable : Variable.t; }

let label t =
  match t with
  | Linkage { label; _ } -> label
  | Variable { variable; _ } ->
      (* Use the variable's compilation unit for the label, since the
         symbol's compilation unit might be a pack *)
      let compilation_unit = Variable.get_compilation_unit variable in
      let unit_linkage_name =
        Linkage_name.to_string
          (Compilation_unit.get_linkage_name compilation_unit)
      in
      let label = unit_linkage_name ^ "__" ^ Variable.unique_name variable in
      Linkage_name.create label

include Identifiable.Make (struct

  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else begin
      match t1, t2 with
      | Linkage _, Variable _ -> 1
      | Variable _, Linkage _ -> -1
      | Linkage l1, Linkage l2 ->
        let c = compare l1.hash l2.hash in
        if c <> 0 then c else begin
          (* Linkage names are unique across a whole project, so just comparing
             those is sufficient. *)
          Linkage_name.compare l1.label l2.label
        end
      | Variable v1, Variable v2 ->
        Variable.compare v1.variable v2.variable
    end

  let equal x y =
    if x == y then true
    else compare x y = 0

  let output chan t =
    Linkage_name.output chan (label t)

  let hash t =
    match t with
    | Linkage { hash; _ } -> hash
    | Variable { variable } -> Variable.hash variable

  let print ppf t =
    Linkage_name.print ppf (label t)

end)

let of_global_linkage compilation_unit label =
  let hash = Linkage_name.hash label in
  Linkage { compilation_unit; hash; label }

let of_variable variable =
  let compilation_unit = Variable.get_compilation_unit variable in
  Variable { variable; compilation_unit }

let import_for_pack ~pack:compilation_unit symbol =
  match symbol with
  | Linkage l -> Linkage { l with compilation_unit }
  | Variable v -> Variable { v with compilation_unit }

let compilation_unit t =
  match t with
  | Linkage { compilation_unit; _ } -> compilation_unit
  | Variable { compilation_unit; _ } -> compilation_unit

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no symbol>"
  | Some t -> print ppf t

let compare_lists l1 l2 =
  Misc.Stdlib.List.compare compare l1 l2
