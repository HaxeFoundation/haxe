(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Support for the builtin attributes:

    - ocaml.alert
    - ocaml.boxed
    - ocaml.deprecated
    - ocaml.deprecated_mutable
    - ocaml.explicit_arity
    - ocaml.immediate
    - ocaml.immediate64
    - ocaml.inline
    - ocaml.inlined
    - ocaml.noalloc
    - ocaml.poll
    - ocaml.ppwarning
    - ocaml.specialise
    - ocaml.specialised
    - ocaml.tailcall
    - ocaml.tail_mod_cons
    - ocaml.unboxed
    - ocaml.untagged
    - ocaml.unrolled
    - ocaml.warnerror
    - ocaml.warning
    - ocaml.warn_on_literal_pattern

    {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

(** {2 Attribute tracking for warning 53} *)

(** [register_attr] must be called on the locations of all attributes that
    should be tracked for the purpose of misplaced attribute warnings.  In
    particular, it should be called on all attributes that are present in the
    source program except those that are contained in the payload of another
    attribute (because these may be left behind by a ppx and intentionally
    ignored by the compiler).

    The [current_phase] argument indicates when this function is being called
    - either when an attribute is created in the parser or when we see an
    attribute while running the check in the [Ast_invariants] module.  This is
    used to ensure that we track only attributes from the final version of the
    parse tree: we skip adding attributes seen at parse time if we can see that
    a ppx will be run later, because the [Ast_invariants] check is always run on
    the result of a ppx.

    Note that the [Ast_invariants] check is also run on parse trees created from
    marshalled ast files if no ppx is being used, ensuring we don't miss
    attributes in that case.
*)
type current_phase = Parser | Invariant_check
val register_attr : current_phase -> string Location.loc -> unit

(** Marks the attributes hiding in the payload of another attribute used, for
    the purposes of misplaced attribute warnings (see comment on
    [current_phase] above).  In the parser, it's simplest to add these to
    the table and remove them later, rather than threading through state
    tracking whether we're in an attribute payload. *)
val mark_payload_attrs_used : Parsetree.payload -> unit

(** Issue misplaced attribute warnings for all attributes created with
    [mk_internal] but not yet marked used. Does nothing if compilation
    is stopped before lambda due to command-line flags. *)
val warn_unused : unit -> unit

(** {3 Warning 53 helpers for environment attributes}

    Some attributes, like deprecation markers, do not affect the compilation of
    the definition on which they appear, but rather result in warnings on future
    uses of that definition.  This is implemented by moving the raw attributes
    into the environment, where they will be noticed on future accesses.

    To make misplaced attribute warnings work appropriately for these
    attributes, we mark them "used" when they are moved into the environment.
    This is done with the helper functions in this section.
*)

(** Marks the attribute used for the purposes of misplaced attribute warnings if
    it is an alert.  Call this when moving things allowed to have alert
    attributes into the environment. *)
val mark_alert_used : Parsetree.attribute -> unit

(** The same as [List.iter mark_alert_used]. *)
val mark_alerts_used : Parsetree.attributes -> unit

(** Marks "warn_on_literal_pattern" attributes used for the purposes of
    misplaced attribute warnings.  Call this when moving constructors into the
    environment. *)
val mark_warn_on_literal_pattern_used : Parsetree.attributes -> unit

(** Marks "deprecated_mutable" attributes used for the purposes of misplaced
    attribute warnings.  Call this when moving labels of mutable fields into the
    environment. *)
val mark_deprecated_mutable_used : Parsetree.attributes -> unit

(** {2 Helpers for alert and warning attributes} *)

val check_alerts: Location.t -> Parsetree.attributes -> string -> unit
val check_alerts_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit
val alerts_of_attrs: Parsetree.attributes -> Misc.alerts
val alerts_of_sig: mark:bool -> Parsetree.signature -> Misc.alerts
val alerts_of_str: mark:bool -> Parsetree.structure -> Misc.alerts

val check_deprecated_mutable:
    Location.t -> Parsetree.attributes -> string -> unit
val check_deprecated_mutable_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit

val error_of_extension: Parsetree.extension -> Location.error

val warning_attribute: ?ppwarning:bool -> Parsetree.attribute -> unit
  (** Apply warning settings from the specified attribute.
      "ocaml.warning"/"ocaml.warnerror" (and variants without the prefix) are
      processed and marked used for warning 53.  Other attributes are ignored.

      Also implement ocaml.ppwarning (unless ~ppwarning:false is
      passed).
  *)

val warning_scope:
  ?ppwarning:bool ->
  Parsetree.attributes -> (unit -> 'a) -> 'a
  (** Execute a function in a new scope for warning settings.  This
      means that the effect of any call to [warning_attribute] during
      the execution of this function will be discarded after
      execution.

      The function also takes a list of attributes which are processed
      with [warning_attribute] in the fresh scope before the function
      is executed.
  *)

(** {2 Helpers for searching for particular attributes} *)

(** [has_attribute name attrs] is true if an attribute with name [name] or
    ["ocaml." ^ name] is present in [attrs].  It marks that attribute used for
    the purposes of misplaced attribute warnings. *)
val has_attribute : string -> Parsetree.attributes -> bool

(** [select_attributes actions attrs] finds the elements of [attrs] that appear
    in [actions] and either returns them or just marks them used, according to
    the corresponding [attr_action].

    Each element [(nm, action)] of the [actions] list is an attribute along with
    an [attr_action] specifying what to do with that attribute.  The action is
    used to accommodate different compiler configurations.  If an attribute is
    used only in some compiler configurations, it's important that we still look
    for it and mark it used when compiling with other configurations.
    Otherwise, we would issue spurious misplaced attribute warnings. *)
type attr_action = Mark_used_only | Return
val select_attributes :
  (string * attr_action) list -> Parsetree.attributes -> Parsetree.attributes

(** [attr_equals_builtin attr s] is true if the name of the attribute is [s] or
    ["ocaml." ^ s].  This is useful for manually inspecting attribute names, but
    note that doing so will not result in marking the attribute used for the
    purpose of warning 53, so it is usually preferable to use [has_attribute]
    or [select_attributes]. *)
val attr_equals_builtin : Parsetree.attribute -> string -> bool

val warn_on_literal_pattern: Parsetree.attributes -> bool
val explicit_arity: Parsetree.attributes -> bool

val immediate: Parsetree.attributes -> bool
val immediate64: Parsetree.attributes -> bool

val has_unboxed: Parsetree.attributes -> bool
val has_boxed: Parsetree.attributes -> bool
