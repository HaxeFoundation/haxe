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

(* Compute a spanning tree representation of types *)

open Misc
open Ctype
open Longident
open Path
open Asttypes
open Types
open Btype
open Outcometree

module String = Misc.Stdlib.String
module Sig_component_kind = Shape.Sig_component_kind
module Style = Misc.Style

(* Print a long identifier *)

module Fmt = Format_doc
open Format_doc

(* Print an identifier avoiding name collisions *)

module Out_name = struct
  let create x = { printed_name = x }
  let print x = x.printed_name
end

(** Some identifiers may require hiding when printing *)
type bound_ident = { hide:bool; ident:Ident.t }

(* printing environment for path shortening and naming *)
let printing_env = ref Env.empty

(* When printing, it is important to only observe the
   current printing environment, without reading any new
   cmi present on the file system *)
let in_printing_env f = Env.without_cmis f !printing_env

 type namespace = Sig_component_kind.t =
    | Value
    | Type
    | Constructor
    | Label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type


module Namespace = struct

  let id = function
    | Type -> 0
    | Module -> 1
    | Module_type -> 2
    | Class -> 3
    | Class_type -> 4
    | Extension_constructor | Value | Constructor | Label -> 5
     (* we do not handle those component *)

  let size = 1 + id Value


  let pp ppf x =
    Fmt.pp_print_string ppf (Shape.Sig_component_kind.to_string x)

  (** The two functions below should never access the filesystem,
      and thus use {!in_printing_env} rather than directly
      accessing the printing environment *)
  let lookup =
    let to_lookup f lid = fst @@ in_printing_env (f (Lident lid)) in
    function
    | Some Type -> to_lookup Env.find_type_by_name
    | Some Module -> to_lookup Env.find_module_by_name
    | Some Module_type -> to_lookup Env.find_modtype_by_name
    | Some Class -> to_lookup Env.find_class_by_name
    | Some Class_type -> to_lookup Env.find_cltype_by_name
    | None | Some(Value|Extension_constructor|Constructor|Label) ->
         fun _ -> raise Not_found

  let location namespace id =
    let path = Path.Pident id in
    try Some (
        match namespace with
        | Some Type -> (in_printing_env @@ Env.find_type path).type_loc
        | Some Module -> (in_printing_env @@ Env.find_module path).md_loc
        | Some Module_type -> (in_printing_env @@ Env.find_modtype path).mtd_loc
        | Some Class -> (in_printing_env @@ Env.find_class path).cty_loc
        | Some Class_type -> (in_printing_env @@ Env.find_cltype path).clty_loc
        | Some (Extension_constructor|Value|Constructor|Label) | None ->
            Location.none
      ) with Not_found -> None

  let best_class_namespace = function
    | Papply _ | Pdot _ -> Some Module
    | Pextra_ty _ -> assert false (* Only in type path *)
    | Pident c ->
        match location (Some Class) c with
        | Some _ -> Some Class
        | None -> Some Class_type

end

(** {2 Ident conflicts printing}

  Ident conflicts arise when multiple {!Ident.t}s are attributed the same name.
  The following module stores the global conflict references and provides the
  printing functions for explaining the source of the conflicts.
*)
module Ident_conflicts = struct
  module M = String.Map
  type explanation =
    { kind: namespace; name:string; root_name:string; location:Location.t}
  let explanations = ref M.empty

  let add namespace name id =
    match Namespace.location (Some namespace) id with
    | None -> ()
    | Some location ->
        let explanation =
          { kind = namespace; location; name; root_name=Ident.name id}
        in
        explanations := M.add name explanation !explanations

  let collect_explanation namespace id ~name =
    let root_name = Ident.name id in
    (* if [name] is of the form "root_name/%d", we register both
      [id] and the identifier in scope for [root_name].
     *)
    if root_name <> name && not (M.mem name !explanations) then
      begin
        add namespace name id;
        if not (M.mem root_name !explanations) then
          (* lookup the identifier in scope with name [root_name] and
             add it too
           *)
          match Namespace.lookup (Some namespace) root_name with
          | Pident root_id -> add namespace root_name root_id
          | exception Not_found | _ -> ()
      end

  let pp_explanation ppf r=
    Fmt.fprintf ppf "@[<v 2>%a:@,Definition of %s %a@]"
      Location.Doc.loc r.location (Sig_component_kind.to_string r.kind)
      Style.inline_code r.name

  let print_located_explanations ppf l =
    Fmt.fprintf ppf "@[<v>%a@]"
      (Fmt.pp_print_list pp_explanation) l

  let reset () = explanations := M.empty
  let list_explanations () =
    let c = !explanations in
    reset ();
    c |> M.bindings |> List.map snd |> List.sort Stdlib.compare


  let print_toplevel_hint ppf l =
    let conj ppf () = Fmt.fprintf ppf " and@ " in
    let pp_namespace_plural ppf n = Fmt.fprintf ppf "%as" Namespace.pp n in
    let root_names = List.map (fun r -> r.kind, r.root_name) l in
    let unique_root_names = List.sort_uniq Stdlib.compare root_names in
    let submsgs = Array.make Namespace.size [] in
    let () = List.iter (fun (n,_ as x) ->
        submsgs.(Namespace.id n) <- x :: submsgs.(Namespace.id n)
      )  unique_root_names in
    let pp_submsg ppf names =
      match names with
      | [] -> ()
      | [namespace, a] ->
          Fmt.fprintf ppf
        "@,\
         @[<2>@{<hint>Hint@}: The %a %a has been defined multiple times@ \
         in@ this@ toplevel@ session.@ \
         Some toplevel values still refer to@ old@ versions@ of@ this@ %a.\
         @ Did you try to redefine them?@]"
        Namespace.pp namespace
        Style.inline_code a Namespace.pp namespace
      | (namespace, _) :: _ :: _ ->
        Fmt.fprintf ppf
        "@,\
         @[<2>@{<hint>Hint@}: The %a %a have been defined multiple times@ \
         in@ this@ toplevel@ session.@ \
         Some toplevel values still refer to@ old@ versions@ of@ those@ %a.\
         @ Did you try to redefine them?@]"
        pp_namespace_plural namespace
        Fmt.(pp_print_list ~pp_sep:conj Style.inline_code)
        (List.map snd names)
        pp_namespace_plural namespace in
    Array.iter (pp_submsg ppf) submsgs

  let err_msg () =
    let ltop, l =
      (* isolate toplevel locations, since they are too imprecise *)
      let from_toplevel a =
        a.location.Location.loc_start.Lexing.pos_fname = "//toplevel//" in
      List.partition from_toplevel (list_explanations ())
    in
    match l, ltop with
    | [], [] -> None
    | _  ->
        Some
          (Fmt.doc_printf "%a%a"
             print_located_explanations l
             print_toplevel_hint ltop
          )
  let err_print ppf = Option.iter Fmt.(fprintf ppf "@,%a" pp_doc) (err_msg ())

  let exists () = M.cardinal !explanations >0
end

module Ident_names = struct

module M = String.Map
module S = String.Set

let enabled = ref true
let enable b = enabled := b

(* Names bound in recursive definitions should be considered as bound
   in the environment when printing identifiers but not when trying
   to find shortest path.
   For instance, if we define
   [{
   module Avoid__me = struct
     type t = A
   end
   type t = X
   type u = [` A of t * t ]
   module M = struct
     type t = A of [ u | `B ]
     type r = Avoid__me.t
   end
  }]
  It is is important that in the definition of [t] that the outer type [t] is
  printed as [t/2] reserving the name [t] to the type being defined in the
  current recursive definition.
     Contrarily, in the definition of [r], one should not shorten the
  path [Avoid__me.t] to [r] until the end of the definition of [r].
  The [bound_in_recursion] bridges the gap between those two slightly different
  notions of printing environment.
*)
let bound_in_recursion = ref M.empty

(* When dealing with functor arguments, identity becomes fuzzy because the same
   syntactic argument may be represented by different identifiers during the
   error processing, we are thus disabling disambiguation on the argument name
*)
let fuzzy = ref S.empty
let with_fuzzy id f =
  protect_refs [ R(fuzzy, S.add (Ident.name id) !fuzzy) ] f
let fuzzy_id namespace id = namespace = Module && S.mem (Ident.name id) !fuzzy

let with_hidden ids f =
  let update m id = M.add (Ident.name id.ident) id.ident m in
  let updated = List.fold_left update !bound_in_recursion ids in
  protect_refs [ R(bound_in_recursion, updated )] f

let human_id id index =
  (* The identifier with index [k] is the (k+1)-th most recent identifier in
     the printing environment. We print them as [name/(k+1)] except for [k=0]
     which is printed as [name] rather than [name/1].
  *)
  if index = 0 then
    Ident.name id
  else
    let ordinal = index + 1 in
    String.concat "/" [Ident.name id; string_of_int ordinal]

let indexed_name namespace id =
  let find namespace id env = match namespace with
    | Type -> Env.find_type_index id env
    | Module -> Env.find_module_index id env
    | Module_type -> Env.find_modtype_index id env
    | Class -> Env.find_class_index id env
    | Class_type-> Env.find_cltype_index id env
    | Value | Extension_constructor | Constructor | Label -> None
  in
  let index =
    match M.find_opt (Ident.name id) !bound_in_recursion with
    | Some rec_bound_id ->
        (* the identifier name appears in the current group of recursive
           definition *)
        if Ident.same rec_bound_id id then
          Some 0
        else
          (* the current recursive definition shadows one more time the
            previously existing identifier with the same name *)
          Option.map succ (in_printing_env (find namespace id))
    | None ->
        in_printing_env (find namespace id)
  in
  let index =
    (* If [index] is [None] at this point, it might indicate that
       the identifier id is not defined in the environment, while there
       are other identifiers in scope that share the same name.
       Currently, this kind of partially incoherent environment happens
       within functor error messages where the left and right hand side
       have a different views of the environment at the source level.
       Printing the source-level by using a default index of `0`
       seems like a reasonable compromise in this situation however.*)
    Option.value index ~default:0
  in
  human_id id index

let ident_name namespace id =
  match namespace, !enabled with
  | None, _ | _, false -> Out_name.create (Ident.name id)
  | Some namespace, true ->
      if fuzzy_id namespace id then Out_name.create (Ident.name id)
      else
        let name = indexed_name namespace id in
        Ident_conflicts.collect_explanation namespace id ~name;
        Out_name.create name
end
let ident_name = Ident_names.ident_name

(* Print a path *)

let ident_stdlib = Ident.create_persistent "Stdlib"

let non_shadowed_stdlib namespace = function
  | Pdot(Pident id, s) as path ->
      Ident.same id ident_stdlib &&
      (match Namespace.lookup namespace s with
       | path' -> Path.same path path'
       | exception Not_found -> true)
  | _ -> false

let find_double_underscore s =
  let len = String.length s in
  let rec loop i =
    if i + 1 >= len then
      None
    else if s.[i] = '_' && s.[i + 1] = '_' then
      Some i
    else
      loop (i + 1)
  in
  loop 0

let rec module_path_is_an_alias_of env path ~alias_of =
  match Env.find_module path env with
  | { md_type = Mty_alias path'; _ } ->
    Path.same path' alias_of ||
    module_path_is_an_alias_of env path' ~alias_of
  | _ -> false
  | exception Not_found -> false

(* Simple heuristic to print Foo__bar.* as Foo.Bar.* when Foo.Bar is an alias
   for Foo__bar. This pattern is used by the stdlib. *)
let rec rewrite_double_underscore_paths env p =
  match p with
  | Pdot (p, s) ->
    Pdot (rewrite_double_underscore_paths env p, s)
  | Papply (a, b) ->
    Papply (rewrite_double_underscore_paths env a,
            rewrite_double_underscore_paths env b)
  | Pextra_ty (p, extra) ->
    Pextra_ty (rewrite_double_underscore_paths env p, extra)
  | Pident id ->
    let name = Ident.name id in
    match find_double_underscore name with
    | None -> p
    | Some i ->
      let better_lid =
        Ldot
          (Lident (String.sub name 0 i),
           Unit_info.modulize
             (String.sub name (i + 2) (String.length name - i - 2)))
      in
      match Env.find_module_by_name better_lid env with
      | exception Not_found -> p
      | p', _ ->
          if module_path_is_an_alias_of env p' ~alias_of:p then
            p'
          else
          p

let rewrite_double_underscore_paths env p =
  if env == Env.empty then
    p
  else
    rewrite_double_underscore_paths env p

let rec tree_of_path ?(disambiguation=true) namespace p =
  let tree_of_path namespace p = tree_of_path ~disambiguation namespace p in
  let namespace = if disambiguation then namespace else None in
  match p with
  | Pident id ->
      Oide_ident (ident_name namespace id)
  | Pdot(_, s) as path when non_shadowed_stdlib namespace path ->
      Oide_ident (Out_name.create s)
  | Pdot(p, s) ->
      Oide_dot (tree_of_path (Some Module) p, s)
  | Papply(p1, p2) ->
      let t1 = tree_of_path (Some Module) p1 in
      let t2 = tree_of_path (Some Module) p2 in
      Oide_apply (t1, t2)
  | Pextra_ty (p, extra) -> begin
      (* inline record types are syntactically prevented from escaping their
         binding scope, and are never shown to users. *)
      match extra with
        Pcstr_ty s ->
          Oide_dot (tree_of_path (Some Type) p, s)
      | Pext_ty ->
          tree_of_path None p
    end

let tree_of_path ?disambiguation namespace p =
  tree_of_path ?disambiguation namespace
    (rewrite_double_underscore_paths !printing_env p)


(* Print a recursive annotation *)

let tree_of_rec = function
  | Trec_not -> Orec_not
  | Trec_first -> Orec_first
  | Trec_next -> Orec_next

(* Normalize paths *)

type param_subst = Id | Nth of int | Map of int list

let is_nth = function
    Nth _ -> true
  | _ -> false

let compose l1 = function
  | Id -> Map l1
  | Map l2 -> Map (List.map (List.nth l1) l2)
  | Nth n  -> Nth (List.nth l1 n)

let apply_subst s1 tyl =
  if tyl = [] then []
  (* cf. PR#7543: Typemod.type_package doesn't respect type constructor arity *)
  else
    match s1 with
      Nth n1 -> [List.nth tyl n1]
    | Map l1 -> List.map (List.nth tyl) l1
    | Id -> tyl

type best_path = Paths of Path.t list | Best of Path.t

(** Short-paths cache: the five mutable variables below implement a one-slot
    cache for short-paths
 *)
let printing_old = ref Env.empty
let printing_pers = ref String.Set.empty
(** {!printing_old} and  {!printing_pers} are the keys of the one-slot cache *)

let printing_depth = ref 0
let printing_cont = ref ([] : Env.iter_cont list)
let printing_map = ref Path.Map.empty
(**
   - {!printing_map} is the main value stored in the cache.
   Note that it is evaluated lazily and its value is updated during printing.
   - {!printing_dep} is the current exploration depth of the environment,
   it is used to determine whenever the {!printing_map} should be evaluated
   further before completing a request.
   - {!printing_cont} is the list of continuations needed to evaluate
   the {!printing_map} one level further (see also {!Env.run_iter_cont})
*)

let rec index l x =
  match l with
    [] -> raise Not_found
  | a :: l -> if eq_type x a then 0 else 1 + index l x

let rec uniq = function
    [] -> true
  | a :: l -> not (List.memq (a : int) l) && uniq l

let rec normalize_type_path ?(cache=false) env p =
  try
    let (params, ty, _) = Env.find_type_expansion p env in
    match get_desc ty with
      Tconstr (p1, tyl, _) ->
        if List.length params = List.length tyl
        && List.for_all2 eq_type params tyl
        then normalize_type_path ~cache env p1
        else if cache || List.length params <= List.length tyl
             || not (uniq (List.map get_id tyl)) then (p, Id)
        else
          let l1 = List.map (index params) tyl in
          let (p2, s2) = normalize_type_path ~cache env p1 in
          (p2, compose l1 s2)
    | _ ->
        (p, Nth (index params ty))
  with
    Not_found ->
      (Env.normalize_type_path None env p, Id)

let penalty s =
  if s <> "" && s.[0] = '_' then
    10
  else
    match find_double_underscore s with
    | None -> 1
    | Some _ -> 10

let rec path_size = function
    Pident id ->
      penalty (Ident.name id), -Ident.scope id
  | Pdot (p, _) | Pextra_ty (p, Pcstr_ty _) ->
      let (l, b) = path_size p in (1+l, b)
  | Papply (p1, p2) ->
      let (l, b) = path_size p1 in
      (l + fst (path_size p2), b)
  | Pextra_ty (p, _) -> path_size p

let same_printing_env env =
  let used_pers = Env.used_persistent () in
  Env.same_types !printing_old env && String.Set.equal !printing_pers used_pers

let set_printing_env env =
  printing_env := env;
  if !Clflags.real_paths ||
     !printing_env == Env.empty ||
     same_printing_env env then
    ()
  else begin
    (* printf "Reset printing_map@."; *)
    printing_old := env;
    printing_pers := Env.used_persistent ();
    printing_map := Path.Map.empty;
    printing_depth := 0;
    (* printf "Recompute printing_map.@."; *)
    let cont =
      Env.iter_types
        (fun p (p', _decl) ->
          let (p1, s1) = normalize_type_path env p' ~cache:true in
          (* Format.eprintf "%a -> %a = %a@." path p path p' path p1 *)
          if s1 = Id then
          try
            let r = Path.Map.find p1 !printing_map in
            match !r with
              Paths l -> r := Paths (p :: l)
            | Best p' -> r := Paths [p; p'] (* assert false *)
          with Not_found ->
            printing_map := Path.Map.add p1 (ref (Paths [p])) !printing_map)
        env in
    printing_cont := [cont];
  end

let wrap_printing_env env f =
  set_printing_env env;
  try_finally f ~always:(fun () -> set_printing_env Env.empty)

let wrap_printing_env ~error env f =
  if error then Env.without_cmis (wrap_printing_env env) f
  else wrap_printing_env env f

let rec lid_of_path = function
    Path.Pident id ->
      Longident.Lident (Ident.name id)
  | Path.Pdot (p1, s) | Path.Pextra_ty (p1, Pcstr_ty s)  ->
      Longident.Ldot (lid_of_path p1, s)
  | Path.Papply (p1, p2) ->
      Longident.Lapply (lid_of_path p1, lid_of_path p2)
  | Path.Pextra_ty (p, Pext_ty) -> lid_of_path p

let is_unambiguous path env =
  let l = Env.find_shadowed_types path env in
  List.exists (Path.same path) l || (* concrete paths are ok *)
  match l with
    [] -> true
  | p :: rem ->
      (* allow also coherent paths:  *)
      let normalize p = fst (normalize_type_path ~cache:true env p) in
      let p' = normalize p in
      List.for_all (fun p -> Path.same (normalize p) p') rem ||
      (* also allow repeatedly defining and opening (for toplevel) *)
      let id = lid_of_path p in
      List.for_all (fun p -> lid_of_path p = id) rem &&
      Path.same p (fst (Env.find_type_by_name id env))

let rec get_best_path r =
  match !r with
    Best p' -> p'
  | Paths [] -> raise Not_found
  | Paths l ->
      r := Paths [];
      List.iter
        (fun p ->
          (* Format.eprintf "evaluating %a@." path p; *)
          match !r with
            Best p' when path_size p >= path_size p' -> ()
          | _ -> if is_unambiguous p !printing_env then r := Best p)
              (* else Format.eprintf "%a ignored as ambiguous@." path p *)
        l;
      get_best_path r

let best_type_path p =
  if !printing_env == Env.empty
  then (p, Id)
  else if !Clflags.real_paths
  then (p, Id)
  else
    let (p', s) = normalize_type_path !printing_env p in
    let get_path () = get_best_path (Path.Map.find  p' !printing_map) in
    while !printing_cont <> [] &&
      try fst (path_size (get_path ())) > !printing_depth with Not_found -> true
    do
      printing_cont := List.map snd (Env.run_iter_cont !printing_cont);
      incr printing_depth;
    done;
    let p'' = try get_path () with Not_found -> p' in
    (* Format.eprintf "%a = %a -> %a@." path p path p' path p''; *)
    (p'', s)

(* When building a tree for a best type path, we should not disambiguate
   identifiers whenever the short-path algorithm detected a better path than
   the original one.*)
let tree_of_best_type_path p p' =
  if Path.same p p' then tree_of_path (Some Type) p'
  else tree_of_path ~disambiguation:false None p'

(* Print a type expression *)

let proxy ty = Transient_expr.repr (proxy ty)

(* When printing a type scheme, we print weak names.  When printing a plain
   type, we do not.  This type controls that behavior *)
type type_or_scheme = Type | Type_scheme

let is_non_gen mode ty =
  match mode with
  | Type_scheme -> is_Tvar ty && get_level ty <> generic_level
  | Type        -> false

let nameable_row row =
  row_name row <> None &&
  List.for_all
    (fun (_, f) ->
       match row_field_repr f with
       | Reither(c, l, _) ->
           row_closed row && if c then l = [] else List.length l = 1
       | _ -> true)
    (row_fields row)

(* This specialized version of [Btype.iter_type_expr] normalizes and
   short-circuits the traversal of the [type_expr], so that it covers only the
   subterms that would be printed by the type printer. *)
let printer_iter_type_expr f ty =
  match get_desc ty with
  | Tconstr(p, tyl, _) ->
      let (_p', s) = best_type_path p in
      List.iter f (apply_subst s tyl)
  | Tvariant row -> begin
      match row_name row with
      | Some(_p, tyl) when nameable_row row ->
          List.iter f tyl
      | _ ->
          iter_row f row
    end
  | Tobject (fi, nm) -> begin
      match !nm with
      | None ->
          let fields, _ = flatten_fields fi in
          List.iter
            (fun (_, kind, ty) ->
               if field_kind_repr kind = Fpublic then
                 f ty)
            fields
      | Some (_, l) ->
          List.iter f (List.tl l)
    end
  | Tfield(_, kind, ty1, ty2) ->
      if field_kind_repr kind = Fpublic then
        f ty1;
      f ty2
  | _ ->
      Btype.iter_type_expr f ty

let quoted_ident ppf x =
  Style.as_inline_code !Oprint.out_ident ppf x

module Internal_names : sig

  val reset : unit -> unit

  val add : Path.t -> unit

  val print_explanations : Env.t -> Fmt.formatter -> unit

end = struct

  let names = ref Ident.Set.empty

  let reset () =
    names := Ident.Set.empty

  let add p =
    match p with
    | Pident id ->
        let name = Ident.name id in
        if String.length name > 0 && name.[0] = '$' then begin
          names := Ident.Set.add id !names
        end
    | Pdot _ | Papply _ | Pextra_ty _ -> ()

  let print_explanations env ppf =
    let constrs =
      Ident.Set.fold
        (fun id acc ->
          let p = Pident id in
          match Env.find_type p env with
          | exception Not_found -> acc
          | decl ->
              match type_origin decl with
              | Existential constr ->
                  let prev = String.Map.find_opt constr acc in
                  let prev = Option.value ~default:[] prev in
                  String.Map.add constr (tree_of_path None p :: prev) acc
              | Definition | Rec_check_regularity -> acc)
        !names String.Map.empty
    in
    String.Map.iter
      (fun constr out_idents ->
        match out_idents with
        | [] -> ()
        | [out_ident] ->
            fprintf ppf
              "@ @[<2>@{<hint>Hint@}:@ %a@ is an existential type@ \
               bound by the constructor@ %a.@]"
              quoted_ident out_ident
              Style.inline_code constr
        | out_ident :: out_idents ->
            fprintf ppf
              "@ @[<2>@{<hint>Hint@}:@ %a@ and %a@ are existential types@ \
               bound by the constructor@ %a.@]"
              (Fmt.pp_print_list
                 ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
                 quoted_ident)
              (List.rev out_idents)
              quoted_ident out_ident
              Style.inline_code constr)
      constrs

end

module Variable_names : sig
  val reset_names : unit -> unit

  val add_subst : (type_expr * type_expr) list -> unit

  val new_name : unit -> string
  val new_var_name : non_gen:bool -> type_expr -> unit -> string

  val name_of_type : (unit -> string) -> transient_expr -> string
  val check_name_of_type : non_gen:bool -> transient_expr -> unit


  val reserve: type_expr -> unit

  val remove_names : transient_expr list -> unit

  val with_local_names : (unit -> 'a) -> 'a

  (* Refresh the weak variable map in the toplevel; for [print_items], which is
     itself for the toplevel *)
  val refresh_weak : unit -> unit
end = struct
  (* We map from types to names, but not directly; we also store a substitution,
     which maps from types to types.  The lookup process is
     "type -> apply substitution -> find name".  The substitution is presumed to
     be one-shot. *)
  let names = ref ([] : (transient_expr * string) list)
  let name_subst = ref ([] : (transient_expr * transient_expr) list)
  let name_counter = ref 0
  let named_vars = ref ([] : string list)
  let visited_for_named_vars = ref ([] : transient_expr list)

  let weak_counter = ref 1
  let weak_var_map = ref TypeMap.empty
  let named_weak_vars = ref String.Set.empty

  let reset_names () =
    names := [];
    name_subst := [];
    name_counter := 0;
    named_vars := [];
    visited_for_named_vars := []

  let add_named_var tty =
    match tty.desc with
      Tvar (Some name) | Tunivar (Some name) ->
        if List.mem name !named_vars then () else
        named_vars := name :: !named_vars
    | _ -> ()

  let rec add_named_vars ty =
    let tty = Transient_expr.repr ty in
    let px = proxy ty in
    if not (List.memq px !visited_for_named_vars) then begin
      visited_for_named_vars := px :: !visited_for_named_vars;
      match tty.desc with
      | Tvar _ | Tunivar _ ->
          add_named_var tty
      | _ ->
          printer_iter_type_expr add_named_vars ty
    end

  let substitute ty =
    match List.assq ty !name_subst with
    | ty' -> ty'
    | exception Not_found -> ty

  let add_subst subst =
    name_subst :=
      List.map (fun (t1,t2) -> Transient_expr.repr t1, Transient_expr.repr t2)
        subst
      @ !name_subst

  let name_is_already_used name =
    List.mem name !named_vars
    || List.exists (fun (_, name') -> name = name') !names
    || String.Set.mem name !named_weak_vars

  let rec new_name () =
    let name = Misc.letter_of_int !name_counter in
    incr name_counter;
    if name_is_already_used name then new_name () else name

  let rec new_weak_name ty () =
    let name = "weak" ^ Int.to_string !weak_counter in
    incr weak_counter;
    if name_is_already_used name then new_weak_name ty ()
    else begin
        named_weak_vars := String.Set.add name !named_weak_vars;
        weak_var_map := TypeMap.add ty name !weak_var_map;
        name
      end

  let new_var_name ~non_gen ty () =
    if non_gen then new_weak_name ty ()
    else new_name ()

  let name_of_type name_generator t =
    (* We've already been through repr at this stage, so t is our representative
       of the union-find class. *)
    let t = substitute t in
    try List.assq t !names with Not_found ->
      try TransientTypeMap.find t !weak_var_map with Not_found ->
      let name =
        match t.desc with
          Tvar (Some name) | Tunivar (Some name) ->
            (* Some part of the type we've already printed has assigned another
             * unification variable to that name. We want to keep the name, so
             * try adding a number until we find a name that's not taken. *)
            let available name =
              List.for_all
                (fun (_, name') -> name <> name')
                !names
            in
            if available name then name
            else
              let suffixed i = name ^ Int.to_string i in
              let i = Misc.find_first_mono (fun i -> available (suffixed i)) in
              suffixed i
        | _ ->
            (* No name available, create a new one *)
            name_generator ()
      in
      (* Exception for type declarations *)
      if name <> "_" then names := (t, name) :: !names;
      name

  let check_name_of_type ~non_gen px =
    let name_gen = new_var_name ~non_gen (Transient_expr.type_expr px) in
    ignore(name_of_type name_gen px)

  let remove_names tyl =
    let tyl = List.map substitute tyl in
    names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !names

  let with_local_names f =
    let old_names = !names in
    let old_subst = !name_subst in
    names      := [];
    name_subst := [];
    try_finally
      ~always:(fun () ->
        names      := old_names;
        name_subst := old_subst)
      f

  let refresh_weak () =
    let refresh t name (m,s) =
      if is_non_gen Type_scheme t then
        begin
          TypeMap.add t name m,
          String.Set.add name s
        end
      else m, s in
    let m, s =
      TypeMap.fold refresh !weak_var_map (TypeMap.empty ,String.Set.empty) in
    named_weak_vars := s;
    weak_var_map := m

  let reserve ty =
    normalize_type ty;
    add_named_vars ty
end

module Aliases = struct
  let visited_objects = ref ([] : transient_expr list)
  let aliased = ref ([] : transient_expr list)
  let delayed = ref ([] : transient_expr list)
  let printed_aliases = ref ([] : transient_expr list)

(* [printed_aliases] is a subset of [aliased] that records only those aliased
   types that have actually been printed; this allows us to avoid naming loops
   that the user will never see. *)

  let is_delayed t = List.memq t !delayed

  let remove_delay t =
    if is_delayed t then
      delayed := List.filter ((!=) t) !delayed

  let add_delayed t =
    if not (is_delayed t) then delayed := t :: !delayed

  let is_aliased_proxy px = List.memq px !aliased
  let is_printed_proxy px = List.memq px !printed_aliases

  let add_proxy px =
    if not (is_aliased_proxy px) then
      aliased := px :: !aliased

  let add ty = add_proxy (proxy ty)

  let add_printed_proxy ~non_gen px =
    Variable_names.check_name_of_type ~non_gen px;
    printed_aliases := px :: !printed_aliases

  let mark_as_printed px =
     if is_aliased_proxy px then (add_printed_proxy ~non_gen:false) px

  let add_printed ty = add_printed_proxy (proxy ty)

  let aliasable ty =
    match get_desc ty with
      Tvar _ | Tunivar _ | Tpoly _ -> false
    | Tconstr (p, _, _) ->
        not (is_nth (snd (best_type_path p)))
    | _ -> true

  let should_visit_object ty =
    match get_desc ty with
    | Tvariant row -> not (static_row row)
    | Tobject _ -> opened_object ty
    | _ -> false

  let rec mark_loops_rec visited ty =
    let px = proxy ty in
    if List.memq px visited && aliasable ty then add_proxy px else
      let tty = Transient_expr.repr ty in
      let visited = px :: visited in
      match tty.desc with
      | Tvariant _ | Tobject _ ->
          if List.memq px !visited_objects then add_proxy px else begin
            if should_visit_object ty then
              visited_objects := px :: !visited_objects;
            printer_iter_type_expr (mark_loops_rec visited) ty
          end
      | Tpoly(ty, tyl) ->
          List.iter add tyl;
          mark_loops_rec visited ty
      | _ ->
          printer_iter_type_expr (mark_loops_rec visited) ty

  let mark_loops ty =
    mark_loops_rec [] ty

  let reset () =
    visited_objects := []; aliased := []; delayed := []; printed_aliases := []

end

let prepare_type ty =
  Variable_names.reserve ty;
  Aliases.mark_loops ty


let reset_except_conflicts () =
  Variable_names.reset_names (); Aliases.reset (); Internal_names.reset ()

let reset () =
  Ident_conflicts.reset ();
  reset_except_conflicts ()

let prepare_for_printing tyl =
  reset_except_conflicts ();
  List.iter prepare_type tyl

let add_type_to_preparation = prepare_type

(* Disabled in classic mode when printing an unification error *)
let print_labels = ref true
let with_labels b f = Misc.protect_refs [R (print_labels,b)] f

let alias_nongen_row mode px ty =
    match get_desc ty with
    | Tvariant _ | Tobject _ ->
        if is_non_gen mode (Transient_expr.type_expr px) then
          Aliases.add_proxy px
    | _ -> ()

let rec tree_of_typexp mode ty =
  let px = proxy ty in
  if Aliases.is_printed_proxy px && not (Aliases.is_delayed px) then
   let non_gen = is_non_gen mode (Transient_expr.type_expr px) in
   let name = Variable_names.(name_of_type (new_var_name ~non_gen ty)) px in
   Otyp_var (non_gen, name) else

  let pr_typ () =
    let tty = Transient_expr.repr ty in
    match tty.desc with
    | Tvar _ ->
        let non_gen = is_non_gen mode ty in
        let name_gen = Variable_names.new_var_name ~non_gen ty in
        Otyp_var (non_gen, Variable_names.name_of_type name_gen tty)
    | Tarrow(l, ty1, ty2, _) ->
        let lab =
          if !print_labels || is_optional l then l else Nolabel
        in
        let t1 =
          if is_optional l then
            match get_desc ty1 with
            | Tconstr(path, [ty], _)
              when Path.same path Predef.path_option ->
                tree_of_typexp mode ty
            | _ -> Otyp_stuff "<hidden>"
          else tree_of_typexp mode ty1 in
        Otyp_arrow (lab, t1, tree_of_typexp mode ty2)
    | Ttuple tyl ->
        Otyp_tuple (tree_of_typlist mode tyl)
    | Tconstr(p, tyl, _abbrev) ->
        let p', s = best_type_path p in
        let tyl' = apply_subst s tyl in
        if is_nth s && not (tyl'=[])
        then tree_of_typexp mode (List.hd tyl')
        else begin
          Internal_names.add p';
          Otyp_constr (tree_of_best_type_path p p', tree_of_typlist mode tyl')
        end
    | Tvariant row ->
        let Row {fields; name; closed; _} = row_repr row in
        let fields =
          if closed then
            List.filter (fun (_, f) -> row_field_repr f <> Rabsent)
              fields
          else fields in
        let present =
          List.filter
            (fun (_, f) ->
               match row_field_repr f with
               | Rpresent _ -> true
               | _ -> false)
            fields in
        let all_present = List.length present = List.length fields in
        begin match name with
        | Some(p, tyl) when nameable_row row ->
            let (p', s) = best_type_path p in
            let id = tree_of_best_type_path p p' in
            let args = tree_of_typlist mode (apply_subst s tyl) in
            let out_variant =
              if is_nth s then List.hd args else Otyp_constr (id, args) in
            if closed && all_present then
              out_variant
            else
              let tags =
                if all_present then None else Some (List.map fst present) in
              Otyp_variant (Ovar_typ out_variant, closed, tags)
        | _ ->
            let fields = List.map (tree_of_row_field mode) fields in
            let tags =
              if all_present then None else Some (List.map fst present) in
            Otyp_variant (Ovar_fields fields, closed, tags)
        end
    | Tobject (fi, nm) ->
        tree_of_typobject mode fi !nm
    | Tnil | Tfield _ ->
        tree_of_typobject mode ty None
    | Tsubst _ ->
        (* This case should only happen when debugging the compiler *)
        Otyp_stuff "<Tsubst>"
    | Tlink _ ->
        fatal_error "Out_type.tree_of_typexp"
    | Tpoly (ty, []) ->
        tree_of_typexp mode ty
    | Tpoly (ty, tyl) ->
        (*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in *)
        if tyl = [] then tree_of_typexp mode ty else begin
          let tyl = List.map Transient_expr.repr tyl in
          let old_delayed = !Aliases.delayed in
          (* Make the names delayed, so that the real type is
             printed once when used as proxy *)
          List.iter Aliases.add_delayed tyl;
          let tl = List.map Variable_names.(name_of_type new_name) tyl in
          let tr = Otyp_poly (tl, tree_of_typexp mode ty) in
          (* Forget names when we leave scope *)
          Variable_names.remove_names tyl;
          Aliases.delayed := old_delayed; tr
        end
    | Tunivar _ ->
        Otyp_var (false, Variable_names.(name_of_type new_name) tty)
    | Tpackage (p, fl) ->
        let fl =
          List.map
            (fun (li, ty) -> (
              String.concat "." (Longident.flatten li),
              tree_of_typexp mode ty
            )) fl in
        Otyp_module (tree_of_path (Some Module_type) p, fl)
  in
  Aliases.remove_delay px;
  alias_nongen_row mode px ty;
  if Aliases.(is_aliased_proxy px && aliasable ty) then begin
    let non_gen = is_non_gen mode (Transient_expr.type_expr px) in
    Aliases.add_printed_proxy ~non_gen px;
    (* add_printed_alias chose a name, thus the name generator
       doesn't matter.*)
    let alias = Variable_names.(name_of_type (new_var_name ~non_gen ty)) px in
    Otyp_alias {non_gen;  aliased = pr_typ (); alias } end
  else pr_typ ()

and tree_of_row_field mode (l, f) =
  match row_field_repr f with
  | Rpresent None | Reither(true, [], _) -> (l, false, [])
  | Rpresent(Some ty) -> (l, false, [tree_of_typexp mode ty])
  | Reither(c, tyl, _) ->
      if c (* contradiction: constant constructor with an argument *)
      then (l, true, tree_of_typlist mode tyl)
      else (l, false, tree_of_typlist mode tyl)
  | Rabsent -> (l, false, [] (* actually, an error *))

and tree_of_typlist mode tyl =
  List.map (tree_of_typexp mode) tyl

and tree_of_typobject mode fi nm =
  begin match nm with
  | None ->
      let pr_fields fi =
        let (fields, rest) = flatten_fields fi in
        let present_fields =
          List.fold_right
            (fun (n, k, t) l ->
               match field_kind_repr k with
               | Fpublic -> (n, t) :: l
               | _ -> l)
            fields [] in
        let sorted_fields =
          List.sort
            (fun (n, _) (n', _) -> String.compare n n') present_fields in
        tree_of_typfields mode rest sorted_fields in
      let (fields, open_row) = pr_fields fi in
      Otyp_object {fields; open_row}
  | Some (p, _ty :: tyl) ->
      let args = tree_of_typlist mode tyl in
      let (p', s) = best_type_path p in
      assert (s = Id);
      Otyp_class (tree_of_best_type_path p p', args)
  | _ ->
      fatal_error "Out_type.tree_of_typobject"
  end

and tree_of_typfields mode rest = function
  | [] ->
      let open_row =
        match get_desc rest with
        | Tvar _ | Tunivar _ | Tconstr _-> true
        | Tnil -> false
        | _ -> fatal_error "typfields (1)"
      in
      ([], open_row)
  | (s, t) :: l ->
      let field = (s, tree_of_typexp mode t) in
      let (fields, rest) = tree_of_typfields mode rest l in
      (field :: fields, rest)

let typexp mode ppf ty =
  !Oprint.out_type ppf (tree_of_typexp mode ty)

let prepared_type_expr ppf ty = typexp Type ppf ty

(* "Half-prepared" type expression: [ty] should have had its names reserved, but
   should not have had its loops marked. *)
let type_expr_with_reserved_names ppf ty =
  Aliases.reset ();
  Aliases.mark_loops ty;
  prepared_type_expr ppf ty


let prepared_type_scheme ppf ty = typexp Type_scheme ppf ty

(* Print one type declaration *)

let tree_of_constraints params =
  List.fold_right
    (fun ty list ->
       let ty' = unalias ty in
       if proxy ty != proxy ty' then
         let tr = tree_of_typexp Type_scheme ty in
         (tr, tree_of_typexp Type_scheme ty') :: list
       else list)
    params []

let filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        if List.exists (eq_type ty) tyl
        then newty2 ~level:generic_level (Ttuple [ty]) :: tyl
        else ty :: tyl)
      (* Two parameters might be identical due to a constraint but we need to
         print them differently in order to make the output syntactically valid.
         We use [Ttuple [ty]] because it is printed as [ty]. *)
      (* Replacing fold_left by fold_right does not work! *)
      [] tyl
  in List.rev params

let prepare_type_constructor_arguments = function
  | Cstr_tuple l -> List.iter prepare_type l
  | Cstr_record l -> List.iter (fun l -> prepare_type l.ld_type) l

let tree_of_label l =
  {
    olab_name = Ident.name l.ld_id;
    olab_mut = l.ld_mutable;
    olab_type = tree_of_typexp Type l.ld_type;
  }

let tree_of_constructor_arguments = function
  | Cstr_tuple l -> tree_of_typlist Type l
  | Cstr_record l -> [ Otyp_record (List.map tree_of_label l) ]

let tree_of_single_constructor cd =
  let name = Ident.name cd.cd_id in
  let ret = Option.map (tree_of_typexp Type) cd.cd_res in
  let args = tree_of_constructor_arguments cd.cd_args in
  {
      ocstr_name = name;
      ocstr_args = args;
      ocstr_return_type = ret;
  }

(* When printing GADT constructor, we need to forget the naming decision we took
  for the type parameters and constraints. Indeed, in
  {[
  type 'a t = X: 'a -> 'b t
   ]}
  It is fine to print both the type parameter ['a] and the existentially
  quantified ['a] in the definition of the constructor X as ['a]
 *)
let tree_of_constructor_in_decl cd =
  match cd.cd_res with
  | None -> tree_of_single_constructor cd
  | Some _ ->
      Variable_names.with_local_names (fun () -> tree_of_single_constructor cd)

let prepare_decl id decl =
  let params = filter_params decl.type_params in
  begin match decl.type_manifest with
  | Some ty ->
      let vars = free_variables ty in
      List.iter
        (fun ty ->
          if get_desc ty = Tvar (Some "_") && List.exists (eq_type ty) vars
          then set_type_desc ty (Tvar None))
        params
  | None -> ()
  end;
  List.iter Aliases.add params;
  List.iter prepare_type params;
  List.iter (Aliases.add_printed ~non_gen:false) params;
  let ty_manifest =
    match decl.type_manifest with
    | None -> None
    | Some ty ->
        let ty =
          (* Special hack to hide variant name *)
          match get_desc ty with
            Tvariant row ->
              begin match row_name row with
                Some (Pident id', _) when Ident.same id id' ->
                  newgenty (Tvariant (set_row_name row None))
              | _ -> ty
              end
          | _ -> ty
        in
        prepare_type ty;
        Some ty
  in
  begin match decl.type_kind with
  | Type_abstract _ -> ()
  | Type_variant (cstrs, _rep) ->
      List.iter
        (fun c ->
           prepare_type_constructor_arguments c.cd_args;
           Option.iter prepare_type c.cd_res)
        cstrs
  | Type_record(l, _rep) ->
      List.iter (fun l -> prepare_type l.ld_type) l
  | Type_open -> ()
  end;
  ty_manifest, params

let tree_of_type_decl id decl =
  let ty_manifest, params = prepare_decl id decl in
  let type_param ot_variance =
    function
    | Otyp_var (ot_non_gen, ot_name) -> {ot_non_gen; ot_name; ot_variance}
    | _ -> {ot_non_gen=false; ot_name="?"; ot_variance}
  in
  let type_defined decl =
    let abstr =
      match decl.type_kind with
        Type_abstract _ ->
          decl.type_manifest = None || decl.type_private = Private
      | Type_record _ ->
          decl.type_private = Private
      | Type_variant (tll, _rep) ->
          decl.type_private = Private ||
          List.exists (fun cd -> cd.cd_res <> None) tll
      | Type_open ->
          decl.type_manifest = None
    in
    let vari =
      List.map2
        (fun ty v ->
          let is_var = is_Tvar ty in
          if abstr || not is_var then
            let inj =
              type_kind_is_abstract decl && Variance.mem Inj v &&
              match decl.type_manifest with
              | None -> true
              | Some ty -> (* only abstract or private row types *)
                  decl.type_private = Private &&
                  Btype.is_constr_row ~allow_ident:true (Btype.row_of_type ty)
            and (co, cn) = Variance.get_upper v in
            (if not cn then Covariant else
             if not co then Contravariant else NoVariance),
            (if inj then Injective else NoInjectivity)
          else (NoVariance, NoInjectivity))
        decl.type_params decl.type_variance
    in
    (Ident.name id,
     List.map2 (fun ty cocn -> type_param cocn (tree_of_typexp Type ty))
       params vari)
  in
  let tree_of_manifest ty1 =
    match ty_manifest with
    | None -> ty1
    | Some ty -> Otyp_manifest (tree_of_typexp Type ty, ty1)
  in
  let (name, args) = type_defined decl in
  let constraints = tree_of_constraints params in
  let ty, priv, unboxed =
    match decl.type_kind with
    | Type_abstract _ ->
        begin match ty_manifest with
        | None -> (Otyp_abstract, Public, false)
        | Some ty ->
            tree_of_typexp Type ty, decl.type_private, false
        end
    | Type_variant (cstrs, rep) ->
        tree_of_manifest
          (Otyp_sum (List.map tree_of_constructor_in_decl cstrs)),
        decl.type_private,
        (rep = Variant_unboxed)
    | Type_record(lbls, rep) ->
        tree_of_manifest (Otyp_record (List.map tree_of_label lbls)),
        decl.type_private,
        (match rep with Record_unboxed _ -> true | _ -> false)
    | Type_open ->
        tree_of_manifest Otyp_open,
        decl.type_private,
        false
  in
    { otype_name = name;
      otype_params = args;
      otype_type = ty;
      otype_private = priv;
      otype_immediate = Type_immediacy.of_attributes decl.type_attributes;
      otype_unboxed = unboxed;
      otype_cstrs = constraints }

let add_type_decl_to_preparation id decl =
   ignore @@ prepare_decl id decl

let tree_of_prepared_type_decl id decl =
  tree_of_type_decl id decl

let tree_of_type_decl id decl =
  reset_except_conflicts();
  tree_of_type_decl id decl

let add_constructor_to_preparation c =
  prepare_type_constructor_arguments c.cd_args;
  Option.iter prepare_type c.cd_res

let prepared_constructor ppf c =
  !Oprint.out_constr ppf (tree_of_single_constructor c)


let tree_of_type_declaration id decl rs =
  Osig_type (tree_of_type_decl id decl, tree_of_rec rs)

let tree_of_prepared_type_declaration id decl rs =
  Osig_type (tree_of_prepared_type_decl id decl, tree_of_rec rs)

let add_type_declaration_to_preparation id decl =
  add_type_decl_to_preparation id decl

let prepared_type_declaration id ppf decl =
  !Oprint.out_sig_item ppf
    (tree_of_prepared_type_declaration id decl Trec_first)


(* When printing extension constructor, it is important to ensure that
after printing the constructor, we are still in the scope of the constructor.
For GADT constructor, this can be done by printing the type parameters inside
their own isolated scope. This ensures that in
{[
   type 'b t += A: 'b -> 'b any t
]}
the type parameter `'b` is not bound when printing the type variable `'b` from
the constructor definition from the type parameter.

Contrarily, for non-gadt constructor, we must keep the same scope for
the type parameters and the constructor because a type constraint may
have changed the name of the type parameter:
{[
type -'a t = .. constraint <x:'a. 'a t -> 'a> = 'a
(* the universal 'a is here to steal the name 'a from the type parameter *)
type 'a t = X of 'a
]} *)
let add_extension_constructor_to_preparation ext =
  let ty_params = filter_params ext.ext_type_params in
  List.iter Aliases.add ty_params;
  List.iter prepare_type ty_params;
  prepare_type_constructor_arguments ext.ext_args;
  Option.iter prepare_type ext.ext_ret_type

let extension_constructor_args_and_ret_type_subtree ext_args ext_ret_type =
  let ret = Option.map (tree_of_typexp Type) ext_ret_type in
  let args = tree_of_constructor_arguments ext_args in
  (args, ret)

let prepared_tree_of_extension_constructor
   id ext es
  =
  let ty_name = Path.name ext.ext_type_path in
  let ty_params = filter_params ext.ext_type_params in
  let type_param =
    function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let param_scope f =
    match ext.ext_ret_type with
    | None ->
        (* normal constructor: same scope for parameters and the constructor *)
        f ()
    | Some _ ->
        (* gadt constructor: isolated scope for the type parameters *)
        Variable_names.with_local_names f
  in
  let ty_params =
    param_scope
      (fun () ->
         List.iter (Aliases.add_printed ~non_gen:false) ty_params;
         List.map (fun ty -> type_param (tree_of_typexp Type ty)) ty_params
      )
  in
  let name = Ident.name id in
  let args, ret =
    extension_constructor_args_and_ret_type_subtree
      ext.ext_args
      ext.ext_ret_type
  in
  let ext =
    { oext_name = name;
      oext_type_name = ty_name;
      oext_type_params = ty_params;
      oext_args = args;
      oext_ret_type = ret;
      oext_private = ext.ext_private }
  in
  let es =
    match es with
        Text_first -> Oext_first
      | Text_next -> Oext_next
      | Text_exception -> Oext_exception
  in
    Osig_typext (ext, es)

let tree_of_extension_constructor id ext es =
  reset_except_conflicts ();
  add_extension_constructor_to_preparation ext;
  prepared_tree_of_extension_constructor id ext es

let prepared_extension_constructor id ppf ext =
  !Oprint.out_sig_item ppf
    (prepared_tree_of_extension_constructor id ext Text_first)

(* Print a value declaration *)

let tree_of_value_description id decl =
  (* Format.eprintf "@[%a@]@." raw_type_expr decl.val_type; *)
  let id = Ident.name id in
  let () = prepare_for_printing [decl.val_type] in
  let ty = tree_of_typexp Type_scheme decl.val_type in
  let vd =
    { oval_name = id;
      oval_type = ty;
      oval_prims = [];
      oval_attributes = [] }
  in
  let vd =
    match decl.val_kind with
    | Val_prim p -> Primitive.print p vd
    | _ -> vd
  in
  Osig_value vd

(* Print a class type *)

let method_type priv ty =
  match priv, get_desc ty with
  | Mpublic, Tpoly(ty, tyl) -> (ty, tyl)
  | _ , _ -> (ty, [])

let prepare_method _lab (priv, _virt, ty) =
  let ty, _ = method_type priv ty in
  prepare_type ty

let tree_of_method mode (lab, priv, virt, ty) =
  let (ty, tyl) = method_type priv ty in
  let tty = tree_of_typexp mode ty in
  Variable_names.remove_names (List.map Transient_expr.repr tyl);
  let priv = priv <> Mpublic in
  let virt = virt = Virtual in
  Ocsg_method (lab, priv, virt, tty)

let rec prepare_class_type params = function
  | Cty_constr (_p, tyl, cty) ->
      let row = Btype.self_type_row cty in
      if List.memq (proxy row) !Aliases.visited_objects
      || not (List.for_all is_Tvar params)
      || List.exists (deep_occur row) tyl
      then prepare_class_type params cty
      else List.iter prepare_type tyl
  | Cty_signature sign ->
      (* Self may have a name *)
      let px = proxy sign.csig_self_row in
      if List.memq px !Aliases.visited_objects then Aliases.add_proxy px
      else Aliases.(visited_objects := px :: !visited_objects);
      Vars.iter (fun _ (_, _, ty) -> prepare_type ty) sign.csig_vars;
      Meths.iter prepare_method sign.csig_meths
  | Cty_arrow (_, ty, cty) ->
      prepare_type ty;
      prepare_class_type params cty

let rec tree_of_class_type mode params =
  function
  | Cty_constr (p', tyl, cty) ->
      let row = Btype.self_type_row cty in
      if List.memq (proxy row) !Aliases.visited_objects
      || not (List.for_all is_Tvar params)
      then
        tree_of_class_type mode params cty
      else
        let namespace = Namespace.best_class_namespace p' in
        Octy_constr (tree_of_path namespace p', tree_of_typlist Type_scheme tyl)
  | Cty_signature sign ->
      let px = proxy sign.csig_self_row in
      let self_ty =
        if Aliases.is_aliased_proxy px then
          Some
            (Otyp_var (false, Variable_names.(name_of_type new_name) px))
        else None
      in
      let csil = [] in
      let csil =
        List.fold_left
          (fun csil (ty1, ty2) -> Ocsg_constraint (ty1, ty2) :: csil)
          csil (tree_of_constraints params)
      in
      let all_vars =
        Vars.fold (fun l (m, v, t) all -> (l, m, v, t) :: all) sign.csig_vars []
      in
      (* Consequence of PR#3607: order of Map.fold has changed! *)
      let all_vars = List.rev all_vars in
      let csil =
        List.fold_left
          (fun csil (l, m, v, t) ->
            Ocsg_value (l, m = Mutable, v = Virtual, tree_of_typexp mode t)
            :: csil)
          csil all_vars
      in
      let all_meths =
        Meths.fold
          (fun l (p, v, t) all -> (l, p, v, t) :: all)
          sign.csig_meths []
      in
      let all_meths = List.rev all_meths in
      let csil =
        List.fold_left
          (fun csil meth -> tree_of_method mode meth :: csil)
          csil all_meths
      in
      Octy_signature (self_ty, List.rev csil)
  | Cty_arrow (l, ty, cty) ->
      let lab =
        if !print_labels || is_optional l then l else Nolabel
      in
      let tr =
       if is_optional l then
         match get_desc ty with
         | Tconstr(path, [ty], _) when Path.same path Predef.path_option ->
             tree_of_typexp mode ty
         | _ -> Otyp_stuff "<hidden>"
       else tree_of_typexp mode ty in
      Octy_arrow (lab, tr, tree_of_class_type mode params cty)


let tree_of_class_param param variance =
  let ot_variance =
    if is_Tvar param then Asttypes.(NoVariance, NoInjectivity) else variance in
  match tree_of_typexp Type_scheme param with
    Otyp_var (ot_non_gen, ot_name) -> {ot_non_gen; ot_name; ot_variance}
  | _ -> {ot_non_gen=false; ot_name="?"; ot_variance}

let class_variance =
  let open Variance in let open Asttypes in
  List.map (fun v ->
    (if not (mem May_pos v) then Contravariant else
     if not (mem May_neg v) then Covariant else NoVariance),
    NoInjectivity)

let tree_of_class_declaration id cl rs =
  let params = filter_params cl.cty_params in

  reset_except_conflicts ();
  List.iter Aliases.add params;
  prepare_class_type params cl.cty_type;
  let px = proxy (Btype.self_type_row cl.cty_type) in
  List.iter prepare_type params;

  List.iter (Aliases.add_printed ~non_gen:false) params;
  if Aliases.is_aliased_proxy px then
    Aliases.add_printed_proxy ~non_gen:false px;

  let vir_flag = cl.cty_new = None in
  Osig_class
    (vir_flag, Ident.name id,
     List.map2 tree_of_class_param params (class_variance cl.cty_variance),
     tree_of_class_type Type_scheme params cl.cty_type,
     tree_of_rec rs)

let tree_of_cltype_declaration id cl rs =
  let params = cl.clty_params in

  reset_except_conflicts ();
  List.iter Aliases.add params;
  prepare_class_type params cl.clty_type;
  let px = proxy (Btype.self_type_row cl.clty_type) in
  List.iter prepare_type params;

  List.iter (Aliases.add_printed ~non_gen:false) params;
  Aliases.mark_as_printed px;

  let sign = Btype.signature_of_class_type cl.clty_type in
  let has_virtual_vars =
    Vars.fold (fun _ (_,vr,_) b -> vr = Virtual || b)
      sign.csig_vars false
  in
  let has_virtual_meths =
    Meths.fold (fun _ (_,vr,_) b -> vr = Virtual || b)
      sign.csig_meths false
  in
  Osig_class_type
    (has_virtual_vars || has_virtual_meths, Ident.name id,
     List.map2 tree_of_class_param params (class_variance cl.clty_variance),
     tree_of_class_type Type_scheme params cl.clty_type,
     tree_of_rec rs)

(* Print a module type *)

let wrap_env fenv ftree arg =
  (* We save the current value of the short-path cache *)
  (* From keys *)
  let env = !printing_env in
  let old_pers = !printing_pers in
  (* to data *)
  let old_map = !printing_map in
  let old_depth = !printing_depth in
  let old_cont = !printing_cont in
  set_printing_env (fenv env);
  let tree = ftree arg in
  if !Clflags.real_paths
     || same_printing_env env then ()
   (* our cached key is still live in the cache, and we want to keep all
      progress made on the computation of the [printing_map] *)
  else begin
    (* we restore the snapshotted cache before calling set_printing_env *)
    printing_old := env;
    printing_pers := old_pers;
    printing_depth := old_depth;
    printing_cont := old_cont;
    printing_map := old_map
  end;
  set_printing_env env;
  tree

let dummy =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract Definition;
    type_private = Public;
    type_manifest = None;
    type_variance = [];
    type_separability = [];
    type_is_newtype = false;
    type_expansion_scope = Btype.lowest_level;
    type_loc = Location.none;
    type_attributes = [];
    type_immediate = Unknown;
    type_unboxed_default = false;
    type_uid = Uid.internal_not_actually_unique;
  }

(** we hide items being defined from short-path to avoid shortening
    [type t = Path.To.t] into [type t = t].
*)

let ident_sigitem = function
  | Types.Sig_type(ident,_,_,_) ->  {hide=true;ident}
  | Types.Sig_class(ident,_,_,_)
  | Types.Sig_class_type (ident,_,_,_)
  | Types.Sig_module(ident,_, _,_,_)
  | Types.Sig_value (ident,_,_)
  | Types.Sig_modtype (ident,_,_)
  | Types.Sig_typext (ident,_,_,_)   ->  {hide=false; ident }

let hide ids env =
  let hide_id id env =
    (* Global idents cannot be renamed *)
    if id.hide && not (Ident.global id.ident) then
      Env.add_type ~check:false (Ident.rename id.ident) dummy env
    else env
  in
  List.fold_right hide_id ids env

let with_hidden_items ids f =
  let with_hidden_in_printing_env ids f =
    wrap_env (hide ids) (Ident_names.with_hidden ids) f
  in
  if not !Clflags.real_paths then
    with_hidden_in_printing_env ids f
  else
    Ident_names.with_hidden ids f


let add_sigitem env x =
  Env.add_signature (Signature_group.flatten x) env

let rec tree_of_modtype ?(ellipsis=false) = function
  | Mty_ident p ->
      Omty_ident (tree_of_path (Some Module_type) p)
  | Mty_signature sg ->
      Omty_signature (if ellipsis then [Osig_ellipsis]
                      else tree_of_signature sg)
  | Mty_functor(param, ty_res) ->
      let param, env =
        tree_of_functor_parameter param
      in
      let res = wrap_env env (tree_of_modtype ~ellipsis) ty_res in
      Omty_functor (param, res)
  | Mty_alias p ->
      Omty_alias (tree_of_path (Some Module) p)

and tree_of_functor_parameter = function
  | Unit ->
      None, fun k -> k
  | Named (param, ty_arg) ->
      let name, env =
        match param with
        | None -> None, fun env -> env
        | Some id ->
            Some (Ident.name id),
            Env.add_module ~arg:true id Mp_present ty_arg
      in
      Some (name, tree_of_modtype ~ellipsis:false ty_arg), env

and tree_of_signature sg =
  wrap_env (fun env -> env)(fun sg ->
      let tree_groups = tree_of_signature_rec !printing_env sg in
      List.concat_map (fun (_env,l) -> List.map snd l) tree_groups
    ) sg

and tree_of_signature_rec env' sg =
  let structured = List.of_seq (Signature_group.seq sg) in
  let collect_trees_of_rec_group group =
    let env = !printing_env in
    let env', group_trees =
       trees_of_recursive_sigitem_group env group
    in
    set_printing_env env';
    (env, group_trees) in
  set_printing_env env';
  List.map collect_trees_of_rec_group structured

and trees_of_recursive_sigitem_group env
    (syntactic_group: Signature_group.rec_group) =
  let display (x:Signature_group.sig_item) = x.src, tree_of_sigitem x.src in
  let env = Env.add_signature syntactic_group.pre_ghosts env in
  match syntactic_group.group with
  | Not_rec x -> add_sigitem env x, [display x]
  | Rec_group items ->
      let ids = List.map (fun x -> ident_sigitem x.Signature_group.src) items in
      List.fold_left add_sigitem env items,
      with_hidden_items ids (fun () -> List.map display items)

and tree_of_sigitem = function
  | Sig_value(id, decl, _) ->
      tree_of_value_description id decl
  | Sig_type(id, decl, rs, _) ->
      tree_of_type_declaration id decl rs
  | Sig_typext(id, ext, es, _) ->
      tree_of_extension_constructor id ext es
  | Sig_module(id, _, md, rs, _) ->
      let ellipsis =
        List.exists (function
          | Parsetree.{attr_name = {txt="..."}; attr_payload = PStr []} -> true
          | _ -> false)
          md.md_attributes in
      tree_of_module id md.md_type rs ~ellipsis
  | Sig_modtype(id, decl, _) ->
      tree_of_modtype_declaration id decl
  | Sig_class(id, decl, rs, _) ->
      tree_of_class_declaration id decl rs
  | Sig_class_type(id, decl, rs, _) ->
      tree_of_cltype_declaration id decl rs

and tree_of_modtype_declaration id decl =
  let mty =
    match decl.mtd_type with
    | None -> Omty_abstract
    | Some mty -> tree_of_modtype mty
  in
  Osig_modtype (Ident.name id, mty)

and tree_of_module id ?ellipsis mty rs =
  Osig_module (Ident.name id, tree_of_modtype ?ellipsis mty, tree_of_rec rs)

(* For the toplevel: merge with tree_of_signature? *)
let print_items showval env x =
  Variable_names.refresh_weak();
  Ident_conflicts.reset ();
  let extend_val env (sigitem,outcome) = outcome, showval env sigitem in
  let post_process (env,l) = List.map (extend_val env) l in
  List.concat_map post_process @@ tree_of_signature_rec env x

let same_path t t' =
  let open Types in
  eq_type t t' ||
  match get_desc t, get_desc t' with
    Tconstr(p,tl,_), Tconstr(p',tl',_) ->
      let (p1, s1) = best_type_path p and (p2, s2)  = best_type_path p' in
      begin match s1, s2 with
        Nth n1, Nth n2 when n1 = n2 -> true
      | (Id | Map _), (Id | Map _) when Path.same p1 p2 ->
          let tl = apply_subst s1 tl and tl' = apply_subst s2 tl' in
          List.length tl = List.length tl' &&
          List.for_all2 eq_type tl tl'
      | _ -> false
      end
  | _ ->
      false

type 'a diff = Same of 'a | Diff of 'a * 'a

let trees_of_type_expansion mode Errortrace.{ty = t; expanded = t'} =
  Aliases.reset ();
  Aliases.mark_loops t;
  if same_path t t'
  then begin Aliases.add_delayed (proxy t); Same (tree_of_typexp mode t) end
  else begin
    Aliases.mark_loops t';
    let t' = if proxy t == proxy t' then unalias t' else t' in
    (* beware order matter due to side effect,
       e.g. when printing object types *)
    let first = tree_of_typexp mode t in
    let second = tree_of_typexp mode t' in
    if first = second then Same first
    else Diff(first,second)
  end

let pp_type ppf t =
  Style.as_inline_code !Oprint.out_type ppf t

let pp_type_expansion ppf = function
  | Same t -> pp_type ppf t
  | Diff(t,t') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
        pp_type t
        pp_type t'

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  let open Types in
  match get_desc t with
  | Tvariant row ->
      let Row {fields; more; name; fixed; closed} = row_repr row in
      if name = None then t else
      Btype.newty2 ~level:(get_level t)
        (Tvariant
           (create_row ~fields ~fixed ~closed ~name:None
              ~more:(Ctype.newvar2 (get_level more))))
  | _ -> t

let prepare_expansion Errortrace.{ty; expanded} =
  let expanded = hide_variant_name expanded in
  Variable_names.reserve ty;
  if not (same_path ty expanded) then Variable_names.reserve expanded;
  Errortrace.{ty; expanded}


(* Adapt functions to exposed interface *)
let namespaced_tree_of_path n = tree_of_path (Some n)
let tree_of_path ?disambiguation p = tree_of_path ?disambiguation None p
let tree_of_modtype = tree_of_modtype ~ellipsis:false
let tree_of_type_declaration ident td rs =
  with_hidden_items [{hide=true; ident}]
    (fun () -> tree_of_type_declaration ident td rs)

let tree_of_class_type kind cty = tree_of_class_type kind [] cty
let prepare_class_type cty = prepare_class_type [] cty

let tree_of_type_path p =
  let (p', s) = best_type_path p in
  let p'' = if (s = Id) then p' else p in
  tree_of_best_type_path p p''
