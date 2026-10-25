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

(* Operations on core types *)

open Asttypes
open Types

exception Unify    of Errortrace.unification_error
exception Equality of Errortrace.equality_error
exception Moregen  of Errortrace.moregen_error
exception Subtype  of Errortrace.Subtype.error

exception Escape of type_expr Errortrace.escape

exception Tags of label * label
exception Cannot_expand
exception Cannot_apply
exception Matches_failure of Env.t * Errortrace.unification_error
  (* Raised from [matches], hence the odd name *)
exception Incompatible
  (* Raised from [mcomp] *)

(* All the following wrapper functions revert to the original level,
   even in case of exception. *)
val with_local_level_generalize:
    ?before_generalize:('a -> unit) -> (unit -> 'a) -> 'a
val with_local_level_generalize_if:
        bool -> ?before_generalize:('a -> unit) -> (unit -> 'a) -> 'a
val with_local_level_generalize_structure: (unit -> 'a) -> 'a
val with_local_level_generalize_structure_if: bool -> (unit -> 'a) -> 'a
val with_local_level_generalize_structure_if_principal: (unit -> 'a) -> 'a
val with_local_level_generalize_for_class: (unit -> 'a) -> 'a

val with_local_level: ?post:('a -> unit) -> (unit -> 'a) -> 'a
        (* [with_local_level (fun () -> cmd) ~post] evaluates [cmd] at a
           raised level.
           If given, [post] is applied to the result, at the original level.
           It is expected to contain only level related post-processing. *)
val with_local_level_if: bool -> (unit -> 'a) -> post:('a -> unit) -> 'a
        (* Same as [with_local_level], but only raise the level conditionally.
           [post] also is only called if the level is raised. *)
val with_local_level_iter: (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
        (* Variant of [with_local_level], where [post] is iterated on the
           returned list. *)
val with_local_level_iter_if:
    bool -> (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
        (* Conditional variant of [with_local_level_iter] *)
val with_level: level: int -> (unit -> 'a) -> 'a
        (* [with_level ~level (fun () -> cmd)] evaluates [cmd] with
           [current_level] set to [level] *)
val with_level_if: bool -> level: int -> (unit -> 'a) -> 'a
        (* Conditional variant of [with_level] *)
val with_local_level_if_principal: (unit -> 'a) -> post:('a -> unit) -> 'a
val with_local_level_iter_if_principal:
    (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
        (* Applications of [with_local_level_if] and [with_local_level_iter_if]
           to [!Clflags.principal] *)

val with_local_level_for_class: ?post:('a -> unit) -> (unit -> 'a) -> 'a
        (* Variant of [with_local_level], where the current level is raised but
           the nongen level is not touched *)
val with_raised_nongen_level: (unit -> 'a) -> 'a
        (* Variant of [with_local_level],
           raises the nongen level to the current level *)

val reset_global_level: unit -> unit
        (* Reset the global level before typing an expression *)
val increase_global_level: unit -> int
val restore_global_level: int -> unit
        (* This pair of functions is only used in Typetexp *)

val create_scope : unit -> int

val newty: type_desc -> type_expr
val new_scoped_ty: int -> type_desc -> type_expr
val newvar: ?name:string -> unit -> type_expr
val newvar2: ?name:string -> int -> type_expr
        (* Return a fresh variable *)
val new_global_var: ?name:string -> unit -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
val newobj: type_expr -> type_expr
val newconstr: Path.t -> type_expr list -> type_expr
val none: type_expr
        (* A dummy type expression *)

val object_fields: type_expr -> type_expr
val flatten_fields:
        type_expr -> (string * field_kind * type_expr) list * type_expr
(** Transform a field type into a list of pairs label-type.
    The fields are sorted.

    Beware of the interaction with GADTs:

    Due to the introduction of object indexes for GADTs, the row variable of
    an object may now be an expansible type abbreviation.
    A first consequence is that [flatten_fields] will not completely flatten
    the object, since the type abbreviation will not be expanded
    ([flatten_fields] does not receive the current environment).
    Another consequence is that various functions may be called with the
    expansion of this type abbreviation, which is a Tfield, e.g. during
    printing.

    Concrete problems have been fixed, but new bugs may appear in the
    future. (Test cases were added to typing-gadts/test.ml)
*)

val associate_fields:
        (string * field_kind * type_expr) list ->
        (string * field_kind * type_expr) list ->
        (string * field_kind * type_expr * field_kind * type_expr) list *
        (string * field_kind * type_expr) list *
        (string * field_kind * type_expr) list
val opened_object: type_expr -> bool
val set_object_name:
        Ident.t -> type_expr list -> type_expr -> unit
val remove_object_name: type_expr -> unit
val find_cltype_for_path: Env.t -> Path.t -> type_declaration * type_expr

val sort_row_fields: (label * row_field) list -> (label * row_field) list
val merge_row_fields:
        (label * row_field) list -> (label * row_field) list ->
        (label * row_field) list * (label * row_field) list *
        (label * row_field * row_field) list
val filter_row_fields:
        bool -> (label * row_field) list -> (label * row_field) list

val lower_contravariant: Env.t -> type_expr -> unit
        (* Lower level of type variables inside contravariant branches;
           to be used before generalize for expansive expressions *)
val lower_variables_only: Env.t -> int -> type_expr -> unit
        (* Lower all variables to the given level *)
val enforce_current_level: Env.t -> type_expr -> unit
        (* Lower whole type to !current_level *)
val generalize_class_signature_spine: class_signature -> unit
       (* Special function to generalize methods during inference *)
val limited_generalize: type_expr -> inside:type_expr -> unit
        (* Only generalize some part of the type
           Make the remaining of the type non-generalizable *)
val limited_generalize_class_type: type_expr -> inside:class_type -> unit
        (* Same, but for class types *)

val duplicate_type: type_expr -> type_expr
        (* Returns a copy with non-variable nodes at generic level *)
val fully_generic: type_expr -> bool

val check_scope_escape : Env.t -> int -> type_expr -> unit
        (* [check_scope_escape env lvl ty] ensures that [ty] could be raised
           to the level [lvl] without any scope escape.
           Raises [Escape] otherwise *)

val instance: ?partial:bool -> type_expr -> type_expr
        (* Take an instance of a type scheme *)
        (* partial=None  -> normal
           partial=false -> newvar() for non generic subterms
           partial=true  -> newty2 ty.level Tvar for non generic subterms *)
val generic_instance: type_expr -> type_expr
        (* Same as instance, but new nodes at generic_level *)
val instance_list: type_expr list -> type_expr list
        (* Take an instance of a list of type schemes *)
val new_local_type:
        ?loc:Location.t ->
        ?manifest_and_scope:(type_expr * int) ->
        type_origin -> type_declaration

module Pattern_env : sig
  type t = private
    { mutable env : Env.t;
      equations_scope : int;
      (* scope for local type declarations *)
      allow_recursive_equations : bool;
      (* true iff checking counter examples *)
    }
  val make: Env.t -> equations_scope:int -> allow_recursive_equations:bool -> t
  val copy: ?equations_scope:int -> t -> t
  val set_env: t -> Env.t -> unit
end

type existential_treatment =
  | Keep_existentials_flexible
  | Make_existentials_abstract of Pattern_env.t

val instance_constructor: existential_treatment ->
        constructor_description -> type_expr list * type_expr * type_expr list
        (* Same, for a constructor. Also returns existentials. *)
val instance_parameterized_type:
        ?keep_names:bool ->
        type_expr list -> type_expr -> type_expr list * type_expr
val instance_declaration: type_declaration -> type_declaration
val generic_instance_declaration: type_declaration -> type_declaration
        (* Same as instance_declaration, but new nodes at generic_level *)
val instance_class:
        type_expr list -> class_type -> type_expr list * class_type

val instance_poly:
        ?keep_names:bool -> fixed:bool ->
        type_expr list -> type_expr -> type_expr list * type_expr
        (* Take an instance of a type scheme containing free univars *)
val polyfy: Env.t -> type_expr -> type_expr list -> type_expr * bool
val instance_label:
        fixed:bool ->
        label_description -> type_expr list * type_expr * type_expr
        (* Same, for a label *)
val apply:
        ?use_current_level:bool ->
        Env.t -> type_expr list -> type_expr -> type_expr list -> type_expr
        (* [apply [p1...pN] t [a1...aN]] applies the type function
           [fun p1 ... pN -> t] to the arguments [a1...aN] and returns the
           resulting instance of [t].
           New nodes default to generic level except if [use_current_level] is
           set to true.
           Exception [Cannot_apply] is raised in case of failure. *)

val try_expand_once_opt: Env.t -> type_expr -> type_expr
val try_expand_safe_opt: Env.t -> type_expr -> type_expr

val expand_head_once: Env.t -> type_expr -> type_expr
val expand_head: Env.t -> type_expr -> type_expr
val expand_head_opt: Env.t -> type_expr -> type_expr
(** The compiler's own version of [expand_head] necessary for type-based
    optimisations. *)

(** Expansion of types for error traces; lives here instead of in [Errortrace]
    because the expansion machinery lives here. *)

(** Create an [Errortrace.Diff] by expanding the two types *)
val expanded_diff :
  Env.t ->
  got:type_expr -> expected:type_expr ->
  (Errortrace.expanded_type, 'variant) Errortrace.elt

(** Create an [Errortrace.Diff] by *duplicating* the two types, so that each
    one's expansion is identical to itself.  Despite the name, does create
    [Errortrace.expanded_type]s. *)
val unexpanded_diff :
  got:type_expr -> expected:type_expr ->
  (Errortrace.expanded_type, 'variant) Errortrace.elt

val full_expand: may_forget_scope:bool -> Env.t -> type_expr -> type_expr

type typedecl_extraction_result =
  | Typedecl of Path.t * Path.t * type_declaration
    (* The original path of the types, and the first concrete
       type declaration found expanding it. *)
  | Has_no_typedecl
  | May_have_typedecl

val extract_concrete_typedecl:
        Env.t -> type_expr -> typedecl_extraction_result

val get_new_abstract_name : Env.t -> string -> string

val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
val unify_gadt:
        Pattern_env.t -> type_expr -> type_expr -> Btype.TypePairs.t
        (* [unify_gadt penv ty1 ty2] unifies [ty1] and [ty2] in
           [Pattern] mode, possible adding local constraints to the
           environment in [penv]. Raises [Unify] if not possible.
           Returns the pairs of types that have been equated.
           Type variables in [ty1] are assumed to be non-leaking (safely
           reifiable), moreover if [penv.allow_recursive_equations = true]
           the same assumption is made for [ty2]. *)
val unify_var: Env.t -> type_expr -> type_expr -> unit
        (* Same as [unify], but allow free univars when first type
           is a variable. *)
val filter_arrow: Env.t -> type_expr -> arg_label -> type_expr * type_expr
        (* A special case of unification with [l:'a -> 'b].  Raises
           [Filter_arrow_failed] instead of [Unify]. *)
val filter_method: Env.t -> string -> type_expr -> type_expr
        (* A special case of unification (with {m : 'a; 'b}).  Raises
           [Filter_method_failed] instead of [Unify]. *)
val occur_in: Env.t -> type_expr -> type_expr -> bool
val deep_occur: type_expr -> type_expr -> bool
val moregeneral: Env.t -> bool -> type_expr -> type_expr -> unit
        (* Check if the first type scheme is more general than the second. *)
val is_moregeneral: Env.t -> bool -> type_expr -> type_expr -> bool
val rigidify: type_expr -> type_expr list
        (* "Rigidify" a type and return its type variable *)
val all_distinct_vars: Env.t -> type_expr list -> bool
        (* Check those types are all distinct type variables *)
val matches: expand_error_trace:bool -> Env.t -> type_expr -> type_expr -> unit
        (* Same as [moregeneral false], implemented using the two above
           functions and backtracking. Ignore levels. The [expand_error_trace]
           flag controls whether the error raised performs expansion; this
           should almost always be [true]. *)
val does_match: Env.t -> type_expr -> type_expr -> bool
        (* Same as [matches], but returns a [bool] *)

val reify_univars : Env.t -> Types.type_expr -> Types.type_expr
        (* Replaces all the variables of a type by a univar. *)

(* Exceptions for special cases of unify *)

type filter_arrow_failure =
  | Unification_error of Errortrace.unification_error
  | Label_mismatch of
      { got           : arg_label
      ; expected      : arg_label
      ; expected_type : type_expr
      }
  | Not_a_function

exception Filter_arrow_failed of filter_arrow_failure

type filter_method_failure =
  | Unification_error of Errortrace.unification_error
  | Not_a_method
  | Not_an_object of type_expr

exception Filter_method_failed of filter_method_failure

type class_match_failure =
    CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of int * Env.t * Errortrace.equality_error
  | CM_Class_type_mismatch of Env.t * class_type * class_type
  | CM_Parameter_mismatch of int * Env.t * Errortrace.moregen_error
  | CM_Val_type_mismatch of string * Env.t * Errortrace.comparison_error
  | CM_Meth_type_mismatch of string * Env.t * Errortrace.comparison_error
  | CM_Non_mutable_value of string
  | CM_Non_concrete_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string * string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string

val match_class_types:
    ?trace:bool -> Env.t -> class_type -> class_type -> class_match_failure list
        (* Check if the first class type is more general than the second. *)
val equal: Env.t -> bool -> type_expr list -> type_expr list -> unit
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)
val is_equal : Env.t -> bool -> type_expr list -> type_expr list -> bool
val equal_private :
        Env.t -> type_expr list -> type_expr ->
        type_expr list -> type_expr -> unit
(* [equal_private env t1 params1 t2 params2] checks that [t1::params1]
   equals [t2::params2] but it is allowed to expand [t1] if it is a
   private abbreviations. *)

val match_class_declarations:
        Env.t -> type_expr list -> class_type -> type_expr list ->
        class_type -> class_match_failure list
        (* Check if the first class type is more general than the second. *)

val enlarge_type: Env.t -> type_expr -> type_expr * bool
        (* Make a type larger, flag is true if some pruning had to be done *)
val subtype: Env.t -> type_expr -> type_expr -> unit -> unit
        (* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
           It accumulates the constraints the type variables must
           enforce and returns a function that enforces this
           constraints. *)

(* Operations on class signatures *)

val new_class_signature : unit -> class_signature
val add_dummy_method : Env.t -> scope:int -> class_signature -> unit

type add_method_failure =
  | Unexpected_method
  | Type_mismatch of Errortrace.unification_error

exception Add_method_failed of add_method_failure

val add_method : Env.t ->
  label -> private_flag -> virtual_flag -> type_expr -> class_signature -> unit

type add_instance_variable_failure =
  | Mutability_mismatch of mutable_flag
  | Type_mismatch of Errortrace.unification_error

exception Add_instance_variable_failed of add_instance_variable_failure

val add_instance_variable : strict:bool -> Env.t ->
  label -> mutable_flag -> virtual_flag -> type_expr -> class_signature -> unit

type inherit_class_signature_failure =
  | Self_type_mismatch of Errortrace.unification_error
  | Method of label * add_method_failure
  | Instance_variable of label * add_instance_variable_failure

exception Inherit_class_signature_failed of inherit_class_signature_failure

val inherit_class_signature : strict:bool -> Env.t ->
  class_signature -> class_signature -> unit

val update_class_signature :
  Env.t -> class_signature -> label list * label list

val hide_private_methods : Env.t -> class_signature -> unit

val close_class_signature : Env.t -> class_signature -> bool

exception Nondep_cannot_erase of Ident.t

val nondep_type: Env.t -> Ident.t list -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to any of the given identifiers.
           Raise [Nondep_cannot_erase id] if no such type exists because [id],
           in particular, could not be erased. *)
val nondep_type_decl:
        Env.t -> Ident.t list -> bool -> type_declaration -> type_declaration
        (* Same for type declarations. *)
val nondep_extension_constructor:
        Env.t -> Ident.t list -> extension_constructor ->
        extension_constructor
          (* Same for extension constructor *)
val nondep_class_declaration:
        Env.t -> Ident.t list -> class_declaration -> class_declaration
        (* Same for class declarations. *)
val nondep_cltype_declaration:
  Env.t -> Ident.t list -> class_type_declaration -> class_type_declaration
        (* Same for class type declarations. *)
(*val correct_abbrev: Env.t -> Path.t -> type_expr list -> type_expr -> unit*)
val is_contractive: Env.t -> Path.t -> bool
val normalize_type: type_expr -> unit

val nongen_vars_in_schema: Env.t -> type_expr -> Btype.TypeSet.t option
        (* Return any non-generic variables in the type scheme *)

val nongen_vars_in_class_declaration:class_declaration -> Btype.TypeSet.t option
        (* Return any non-generic variables in the class type.
           Uses the empty environment.  *)

type variable_kind = Row_variable | Type_variable
type closed_class_failure = {
  free_variable: type_expr * variable_kind;
  meth: string;
  meth_ty: type_expr;
}

val free_variables: ?env:Env.t -> type_expr -> type_expr list
        (* If env present, then check for incomplete definitions too *)
val closed_type_expr: ?env:Env.t -> type_expr -> bool
val closed_type_decl: type_declaration -> type_expr option
val closed_extension_constructor: extension_constructor -> type_expr option
val closed_class:
        type_expr list -> class_signature ->
        closed_class_failure option
        (* Check whether all type variables are bound *)

val unalias: type_expr -> type_expr

val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)

val collapse_conj_params: Env.t -> type_expr list -> unit
        (* Collapse conjunctive types in class parameters *)

val get_current_level: unit -> int
val wrap_trace_gadt_instances: ?force:bool -> Env.t -> ('a -> 'b) -> 'a -> 'b

val immediacy : Env.t -> type_expr -> Type_immediacy.t

(* Stubs *)
val package_subtype :
    (Env.t -> Path.t -> (Longident.t * type_expr) list ->
      Path.t -> (Longident.t * type_expr) list ->
     (unit,Errortrace.first_class_module) Result.t) ref

(* Raises [Incompatible] *)
val mcomp : Env.t -> type_expr -> type_expr -> unit
