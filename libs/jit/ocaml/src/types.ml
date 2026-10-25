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

(* Representation of types and declarations *)

open Asttypes

(* Type expressions for the core language *)

type transient_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: scope_field;
    id: int }

and scope_field = int
  (* bit field: 27 bits for scope (Ident.highest_scope = 100_000_000)
     and at least 4 marks *)

and type_expr = transient_expr

and type_desc =
    Tvar of string option
  | Tarrow of arg_label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * (Longident.t * type_expr) list

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_closed: bool;
      row_fixed: fixed_explanation option;
      row_name: (Path.t * type_expr list) option }
and fixed_explanation =
  | Univar of type_expr | Fixed_private | Reified of Path.t | Rigid
and row_field = [`some] row_field_gen
and row_field_cell = [`some | `none] row_field_gen ref
and _ row_field_gen =
    RFpresent : type_expr option -> [> `some] row_field_gen
  | RFeither :
      { no_arg: bool;
        arg_type: type_expr list;
        matched: bool;
        ext: row_field_cell} -> [> `some] row_field_gen
  | RFabsent : [> `some] row_field_gen
  | RFnone : [> `none] row_field_gen

and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and any = [`some | `none | `var]
and field_kind = [`some|`var] field_kind_gen
and _ field_kind_gen =
    FKvar : {mutable field_kind: any field_kind_gen} -> [> `var] field_kind_gen
  | FKprivate : [> `none] field_kind_gen  (* private method; only under FKvar *)
  | FKpublic  : [> `some] field_kind_gen  (* public method *)
  | FKabsent  : [> `some] field_kind_gen  (* hidden private method *)

and commutable = [`some|`var] commutable_gen
and _ commutable_gen =
    Cok      : [> `some] commutable_gen
  | Cunknown : [> `none] commutable_gen
  | Cvar : {mutable commu: any commutable_gen} -> [> `var] commutable_gen

module TransientTypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

module TransientTypeHash = Hashtbl.Make(TransientTypeOps)

(* *)

module Uid = Shape.Uid

(* Maps of methods and instance variables *)

module MethSet = Misc.Stdlib.String.Set
module VarSet = Misc.Stdlib.String.Set

module Meths = Misc.Stdlib.String.Map
module Vars = Misc.Stdlib.String.Map


(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
    val_uid: Uid.t;
  }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of
      class_signature * self_meths * Ident.t Vars.t * string
                                        (* Self *)
  | Val_anc of class_signature * Ident.t Meths.t * string
                                        (* Ancestor *)

and self_meths =
  | Self_concrete of Ident.t Meths.t
  | Self_virtual of Ident.t Meths.t ref

and class_signature =
  { csig_self: type_expr;
    mutable csig_self_row: type_expr;
    mutable csig_vars: (mutable_flag * virtual_flag * type_expr) Vars.t;
    mutable csig_meths: (method_privacy * virtual_flag * type_expr) Meths.t; }

and method_privacy =
  | Mpublic
  | Mprivate of field_kind

(* Variance *)
(* Variance forms a product lattice of the following partial orders:
     0 <= may_pos <= pos
     0 <= may_weak <= may_neg <= neg
     0 <= inj
   Additionally, the following implications are valid
     pos => inj
     neg => inj
   Examples:
     type 'a t        : may_pos + may_neg + may_weak
     type 'a t = 'a   : pos
     type 'a t = 'a -> unit : neg
     type 'a t = ('a -> unit) -> unit : pos + may_weak
     type 'a t = A of (('a -> unit) -> unit) : pos
     type +'a p = ..  : may_pos + inj
     type +!'a t      : may_pos + inj
     type -!'a t      : may_neg + inj
     type 'a t = A    : inj
 *)

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2 + 4
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16 + 8 + 1
    | Neg -> 32 + 8 + 4 + 2
    | Inv -> 63
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = (v1 land v2 = v1)
  let eq (v1 : t) v2 = (v1 = v2)
  let set x v = union v (single x)
  let set_if b x v = if b then set x v else v
  let mem x = subset (single x)
  let null = 0
  let unknown = 7
  let full = single Inv
  let covariant = single Pos
  let contravariant = single Neg
  let swap f1 f2 v v' =
    set_if (mem f2 v) f1 (set_if (mem f1 v) f2 v')
  let conjugate v =
    let v' = inter v (union (single Inj) (single May_weak)) in
    swap Pos Neg v (swap May_pos May_neg v v')
  let compose v1 v2 =
    if mem Inv v1 && mem Inj v2 then full else
    let mp =
      mem May_pos v1 && mem May_pos v2 || mem May_neg v1 && mem May_neg v2
    and mn =
      mem May_pos v1 && mem May_neg v2 || mem May_neg v1 && mem May_pos v2
    and mw = mem May_weak v1 && v2 <> null || v1 <> null && mem May_weak v2
    and inj = mem Inj v1 && mem Inj v2
    and pos = mem Pos v1 && mem Pos v2 || mem Neg v1 && mem Neg v2
    and neg = mem Pos v1 && mem Neg v2 || mem Neg v1 && mem Pos v2 in
    List.fold_left (fun v (b,f) -> set_if b f v) null
      [mp, May_pos; mn, May_neg; mw, May_weak; inj, Inj; pos, Pos; neg, Neg]
  let strengthen v =
    if mem May_neg v then v else v land (full - single May_weak)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inj v)
  let unknown_signature ~injective ~arity =
    let v = if injective then set Inj unknown else unknown in
    Misc.replicate_list v arity
end

module Separability = struct
  type t = Ind | Sep | Deepsep
  type signature = t list
  let eq (m1 : t) m2 = (m1 = m2)
  let rank = function
    | Ind -> 0
    | Sep -> 1
    | Deepsep -> 2
  let compare m1 m2 = compare (rank m1) (rank m2)
  let max m1 m2 = if rank m1 >= rank m2 then m1 else m2

  let print ppf = function
    | Ind -> Format.fprintf ppf "Ind"
    | Sep -> Format.fprintf ppf "Sep"
    | Deepsep -> Format.fprintf ppf "Deepsep"

  let print_signature ppf modes =
    let pp_sep ppf () = Format.fprintf ppf ",@," in
    Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list ~pp_sep print) modes

  let default_signature ~arity =
    let default_mode = if Config.flat_float_array then Deepsep else Ind in
    Misc.replicate_list default_mode arity
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_immediate: Type_immediacy.t;
    type_unboxed_default: bool;
    type_uid: Uid.t;
 }

and type_decl_kind = (label_declaration, constructor_declaration) type_kind

and ('lbl, 'cstr) type_kind =
    Type_abstract of type_origin
  | Type_record of 'lbl list * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open

and type_origin =
    Definition
  | Rec_check_regularity
  | Existential of string

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of Path.t          (* Inlined record under extension *)

and variant_representation =
    Variant_regular          (* Constant or boxed constructors *)
  | Variant_unboxed          (* One unboxed single-field constructor *)

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutable_flag;
    ld_type: type_expr;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
    ld_uid: Uid.t;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
    cd_uid: Uid.t;
  }

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

type extension_constructor =
  { ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_uid: Uid.t;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

(* Type expressions for the class language *)

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
    cty_uid: Uid.t;
 }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_hash_type: type_declaration;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
    clty_uid: Uid.t;
  }

(* Type expressions for the module language *)

type visibility =
  | Exported
  | Hidden

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

and module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

and module_declaration =
  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

and modtype_declaration =
  {
    mtd_type: module_type option;  (* Note: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }

and rec_status =
    Trec_not                   (* first in a nonrecursive group *)
  | Trec_first                 (* first in a recursive group *)
  | Trec_next                  (* not first in a recursive/nonrecursive group *)

and ext_status =
    Text_first                     (* first constructor of an extension *)
  | Text_next                      (* not first constructor of an extension *)
  | Text_exception                 (* an exception *)


(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

let equal_tag t1 t2 =
  match (t1, t2) with
  | Cstr_constant i1, Cstr_constant i2 -> i2 = i1
  | Cstr_block i1, Cstr_block i2 -> i2 = i1
  | Cstr_unboxed, Cstr_unboxed -> true
  | Cstr_extension (path1, b1), Cstr_extension (path2, b2) ->
      Path.same path1 path2 && b1 = b2
  | (Cstr_constant _|Cstr_block _|Cstr_unboxed|Cstr_extension _), _ -> false

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Cstr_extension _,Cstr_extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

let item_visibility = function
  | Sig_value (_, _, vis)
  | Sig_type (_, _, _, vis)
  | Sig_typext (_, _, _, vis)
  | Sig_module (_, _, _, _, vis)
  | Sig_modtype (_, _, vis)
  | Sig_class (_, _, _, vis)
  | Sig_class_type (_, _, _, vis) -> vis

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
   }

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg}, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_typext(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, Mp_present, _, _, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_class(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

let signature_item_id = function
  | Sig_value (id, _, _)
  | Sig_type (id, _, _, _)
  | Sig_typext (id, _, _, _)
  | Sig_module (id, _, _, _, _)
  | Sig_modtype (id, _, _)
  | Sig_class (id, _, _, _)
  | Sig_class_type (id, _, _, _)
    -> id

(**** Definitions for backtracking ****)

type change =
    Ctype of type_expr * type_desc
  | Ccompress of type_expr * type_desc * type_desc
  | Clevel of type_expr * int
  | Cscope of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Crow of [`none|`some] row_field_gen ref
  | Ckind of [`var] field_kind_gen
  | Ccommu of [`var] commutable_gen
  | Cuniv of type_expr option ref * type_expr option

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

let trail = Local_store.s_table ref Unchanged

let log_change ch =
  let r' = ref Unchanged in
  !trail := Change (ch, r');
  trail := r'

(* constructor and accessors for [field_kind] *)

type field_kind_view =
    Fprivate
  | Fpublic
  | Fabsent

let rec field_kind_internal_repr : field_kind -> field_kind = function
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as fk} ->
      field_kind_internal_repr fk
  | kind -> kind

let field_kind_repr fk =
  match field_kind_internal_repr fk with
  | FKvar _ -> Fprivate
  | FKpublic -> Fpublic
  | FKabsent -> Fabsent

let field_public = FKpublic
let field_absent = FKabsent
let field_private () = FKvar {field_kind=FKprivate}

(* Constructor and accessors for [commutable] *)

let rec is_commu_ok : type a. a commutable_gen -> bool = function
  | Cvar {commu} -> is_commu_ok commu
  | Cunknown -> false
  | Cok -> true

let commu_ok = Cok
let commu_var () = Cvar {commu=Cunknown}

(**** Representative of a type ****)

let rec repr_link (t : type_expr) d : type_expr -> type_expr =
 function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' ->
     log_change (Ccompress (t, t.desc, d));
     t.desc <- d;
     t'

let repr_link1 t = function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' -> t'

let repr t =
  match t.desc with
   Tlink t' ->
     repr_link1 t t'
 | Tfield (_, k, _, t') when field_kind_internal_repr k = FKabsent ->
     repr_link1 t t'
 | _ -> t

(* scope_field and marks *)

let scope_mask = (1 lsl 27) - 1
let marks_mask = (-1) lxor scope_mask
let () = assert (Ident.highest_scope land marks_mask = 0)

type type_mark =
  | Mark of {mark: int; mutable marked: type_expr list}
  | Hash of {visited: unit TransientTypeHash.t}
let type_marks =
  (* All the bits in marks_mask *)
  List.init (Sys.int_size - 27) (fun x -> 1 lsl (x + 27))
let available_marks = Local_store.s_ref type_marks
let with_type_mark f =
  match !available_marks with
  | mark :: rem as old ->
      available_marks := rem;
      let mk = Mark {mark; marked = []} in
      Misc.try_finally (fun () -> f mk) ~always: begin fun () ->
        available_marks := old;
        match mk with
        | Mark {marked} ->
            (* unmark marked type nodes *)
            List.iter
              (fun ty -> ty.scope <- ty.scope land ((-1) lxor mark))
              marked
        | Hash _ -> ()
      end
  | [] ->
      (* When marks are exhausted, fall back to using a hash table *)
      f (Hash {visited = TransientTypeHash.create 1})

(* getters for type_expr *)

let get_desc t = (repr t).desc
let get_level t = (repr t).level
let get_scope t = (repr t).scope land scope_mask
let get_id t = (repr t).id
let not_marked_node mark t =
  match mark with
  | Mark {mark} -> (repr t).scope land mark = 0
  | Hash {visited} -> not (TransientTypeHash.mem visited (repr t))

(* transient type_expr *)

module Transient_expr = struct
  let create desc ~level ~scope ~id = {desc; level; scope; id}
  let set_desc ty d = ty.desc <- d
  let set_stub_desc ty d = assert (ty.desc = Tvar None); ty.desc <- d
  let set_level ty lv = ty.level <- lv
  let get_scope ty = ty.scope land scope_mask
  let get_marks ty = ty.scope lsr 27
  let set_scope ty sc =
    if (sc land marks_mask <> 0) then
      invalid_arg "Types.Transient_expr.set_scope";
    ty.scope <- (ty.scope land marks_mask) lor sc
  let try_mark_node mark ty =
    match mark with
    | Mark ({mark} as mk) ->
        (ty.scope land mark = 0) && (* mark type node when not marked *)
        (ty.scope <- ty.scope lor mark; mk.marked <- ty :: mk.marked; true)
    | Hash {visited} ->
        not (TransientTypeHash.mem visited ty) &&
        (TransientTypeHash.add visited ty (); true)
  let coerce ty = ty
  let repr = repr
  let type_expr ty = ty
end

(* setting marks *)
let try_mark_node mark t = Transient_expr.try_mark_node mark (repr t)

(* Comparison for [type_expr]; cannot be used for functors *)

let eq_type t1 t2 = t1 == t2 || repr t1 == repr t2
let compare_type t1 t2 = compare (get_id t1) (get_id t2)

(* Constructor and accessors for [row_desc] *)

let create_row ~fields ~more ~closed ~fixed ~name =
    { row_fields=fields; row_more=more;
      row_closed=closed; row_fixed=fixed; row_name=name }

(* [row_fields] subsumes the original [row_repr] *)
let rec row_fields row =
  match get_desc row.row_more with
  | Tvariant row' ->
      row.row_fields @ row_fields row'
  | _ ->
      row.row_fields

let rec row_repr_no_fields row =
  match get_desc row.row_more with
  | Tvariant row' -> row_repr_no_fields row'
  | _ -> row

let row_more row = (row_repr_no_fields row).row_more
let row_closed row = (row_repr_no_fields row).row_closed
let row_fixed row = (row_repr_no_fields row).row_fixed
let row_name row = (row_repr_no_fields row).row_name

let rec get_row_field tag row =
  let rec find = function
    | (tag',f) :: fields ->
        if tag = tag' then f else find fields
    | [] ->
        match get_desc row.row_more with
        | Tvariant row' -> get_row_field tag row'
        | _ -> RFabsent
  in find row.row_fields

let set_row_name row row_name =
  let row_fields = row_fields row in
  let row = row_repr_no_fields row in
  {row with row_fields; row_name}

type row_desc_repr =
    Row of { fields: (label * row_field) list;
             more:type_expr;
             closed:bool;
             fixed:fixed_explanation option;
             name:(Path.t * type_expr list) option }

let row_repr row =
  let fields = row_fields row in
  let row = row_repr_no_fields row in
  Row { fields;
        more = row.row_more;
        closed = row.row_closed;
        fixed = row.row_fixed;
        name = row.row_name }

type row_field_view =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

let rec row_field_repr_aux tl : row_field -> row_field = function
  | RFeither ({ext = {contents = RFnone}} as r) ->
      RFeither {r with arg_type = tl@r.arg_type}
  | RFeither {arg_type;
              ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_repr_aux (tl@arg_type) rf
  | RFpresent (Some _) when tl <> [] ->
      RFpresent (Some (List.hd tl))
  | RFpresent _ as rf -> rf
  | RFabsent -> RFabsent

let row_field_repr fi =
  match row_field_repr_aux [] fi with
  | RFeither {no_arg; arg_type; matched} -> Reither (no_arg, arg_type, matched)
  | RFpresent t -> Rpresent t
  | RFabsent -> Rabsent

let rec row_field_ext (fi : row_field) =
  match fi with
  | RFeither {ext = {contents = RFnone} as ext} -> ext
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_ext rf
  | _ -> Misc.fatal_error "Types.row_field_ext "

let rf_present oty = RFpresent oty
let rf_absent = RFabsent
let rf_either ?use_ext_of ~no_arg arg_type ~matched =
  let ext =
    match use_ext_of with
      Some rf -> row_field_ext rf
    | None -> ref RFnone
  in
  RFeither {no_arg; arg_type; matched; ext}

let rf_either_of = function
  | None ->
      RFeither {no_arg=true; arg_type=[]; matched=false; ext=ref RFnone}
  | Some ty ->
      RFeither {no_arg=false; arg_type=[ty]; matched=false; ext=ref RFnone}

let eq_row_field_ext rf1 rf2 =
  row_field_ext rf1 == row_field_ext rf2

let changed_row_field_exts l f =
  let exts = List.map row_field_ext l in
  f ();
  List.exists (fun r -> !r <> RFnone) exts

let match_row_field ~present ~absent ~either (f : row_field) =
  match f with
  | RFabsent -> absent ()
  | RFpresent t -> present t
  | RFeither {no_arg; arg_type; matched; ext} ->
      let e : row_field option =
        match !ext with
        | RFnone -> None
        | RFeither _ | RFpresent _ | RFabsent as e -> Some e
      in
      either no_arg arg_type matched (ext,e)

(**** Some type creators ****)

let new_id = Local_store.s_ref (-1)

let create_expr = Transient_expr.create

let proto_newty3 ~level ~scope desc  =
  incr new_id;
  create_expr desc ~level ~scope ~id:!new_id

                  (**********************************)
                  (*  Utilities for backtracking    *)
                  (**********************************)

let undo_change = function
    Ctype  (ty, desc) -> Transient_expr.set_desc ty desc
  | Ccompress (ty, desc, _) -> Transient_expr.set_desc ty desc
  | Clevel (ty, level) -> Transient_expr.set_level ty level
  | Cscope (ty, scope) -> Transient_expr.set_scope ty scope
  | Cname  (r, v)    -> r := v
  | Crow   r         -> r := RFnone
  | Ckind  (FKvar r) -> r.field_kind <- FKprivate
  | Ccommu (Cvar r)  -> r.commu <- Cunknown
  | Cuniv  (r, v)    -> r := v

type snapshot = changes ref * int
let last_snapshot = Local_store.s_ref 0

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  let ty = repr ty in
  let ty' = repr ty' in
  if ty == ty' then () else begin
  log_type ty;
  let desc = ty.desc in
  Transient_expr.set_desc ty (Tlink ty');
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    Tvar name, Tvar name' ->
      begin match name, name' with
      | Some _, None -> log_type ty'; Transient_expr.set_desc ty' (Tvar name)
      | None, Some _ -> ()
      | Some _, Some _ ->
          if ty.level < ty'.level then
            (log_type ty'; Transient_expr.set_desc ty' (Tvar name))
      | None, None   -> ()
      end
  | _ -> ()
  end
  (* ; assert (check_memorized_abbrevs ()) *)
  (*  ; check_expans [] ty' *)
(* TODO: consider eliminating set_type_desc, replacing it with link types *)
let set_type_desc ty td =
  let ty = repr ty in
  if td != ty.desc then begin
    log_type ty;
    Transient_expr.set_desc ty td
  end
(* TODO: separate set_level into two specific functions: *)
(*  set_lower_level and set_generic_level *)
let set_level ty level =
  let ty = repr ty in
  if level <> ty.level then begin
    if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
    Transient_expr.set_level ty level
  end

(* TODO: introduce a guard and rename it to set_higher_scope? *)
let set_scope ty scope =
  let ty = repr ty in
  let prev_scope = ty.scope land marks_mask in
  if scope <> prev_scope then begin
    if ty.id <= !last_snapshot then log_change (Cscope (ty, prev_scope));
    Transient_expr.set_scope ty scope
  end

let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v

let rec link_row_field_ext ~(inside : row_field) (v : row_field) =
  match inside with
  | RFeither {ext = {contents = RFnone} as e} ->
      let RFeither _ | RFpresent _ | RFabsent as v = v in
      log_change (Crow e); e := v
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      link_row_field_ext ~inside:rf v
  | _ -> invalid_arg "Types.link_row_field_ext"

let rec link_kind ~(inside : field_kind) (k : field_kind) =
  match inside with
  | FKvar ({field_kind = FKprivate} as rk) as inside ->
      (* prevent a loop by normalizing k and comparing it with inside *)
      let FKvar _ | FKpublic | FKabsent as k = field_kind_internal_repr k in
      if k != inside then begin
        log_change (Ckind inside);
        rk.field_kind <- k
      end
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as inside} ->
      link_kind ~inside k
  | _ -> invalid_arg "Types.link_kind"

let rec commu_repr : commutable -> commutable = function
  | Cvar {commu = Cvar _ | Cok as commu} -> commu_repr commu
  | c -> c

let rec link_commu ~(inside : commutable) (c : commutable) =
  match inside with
  | Cvar ({commu = Cunknown} as rc) as inside ->
      (* prevent a loop by normalizing c and comparing it with inside *)
      let Cvar _ | Cok as c = commu_repr c in
      if c != inside then begin
        log_change (Ccommu inside);
        rc.commu <- c
      end
  | Cvar {commu = Cvar _ | Cok as inside} ->
      link_commu ~inside c
  | _ -> invalid_arg "Types.link_commu"

let set_commu_ok c = link_commu ~inside:c Cok

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  (!trail, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack ~cleanup_abbrev (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Types.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      trail := changes

let undo_first_change_after (changes, _) =
  match !changes with
  | Change (ch, _) ->
      undo_change ch
  | _ -> ()

let rec rev_compress_log log r =
  match !r with
    Unchanged | Invalid ->
      log
  | Change (Ccompress _, next) ->
      rev_compress_log (r::log) next
  | Change (_, next) ->
      rev_compress_log log next

let undo_compress (changes, _old) =
  match !changes with
    Unchanged
  | Invalid -> ()
  | Change _ ->
      let log = rev_compress_log [] changes in
      List.iter
        (fun r -> match !r with
          Change (Ccompress (ty, desc, d), next) when ty.desc == d ->
            Transient_expr.set_desc ty desc; r := !next
        | _ -> ())
        log
