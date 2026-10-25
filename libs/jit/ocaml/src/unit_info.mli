(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module centralize the handling of compilation files and their metadata.

  Maybe more importantly, this module provides functions for deriving module
  names from strings or filenames.
*)

(** {1:modname_from_strings Module name convention and computation} *)

type intf_or_impl = Intf | Impl
type modname = string
type filename = string
type file_prefix = string

type error = Invalid_encoding of filename
exception Error of error

(** [modulize s] capitalizes the first letter of [s]. *)
val modulize: string -> modname

(** [normalize s] uncapitalizes the first letter of [s]. *)
val normalize: string -> string

(** [lax_modname_from_source filename] is [modulize stem] where [stem] is the
    basename of the filename [filename] stripped from all its extensions.
    For instance, [modname_from_source "/pa.th/x.ml.pp"] is ["X"]. *)
val lax_modname_from_source: filename -> modname

(** Same as {!lax_modname_from_source} but raises an {!error.Invalid_encoding}
    error on filename with invalid utf8 encoding. *)
val strict_modname_from_source: filename -> modname

(** {2:module_name_validation Module name validation function}*)

(** [is_unit_name name] is true only if [name] can be used as a
    valid module name. *)
val is_unit_name : modname -> bool


(** {1:unit_info Metadata for compilation unit} *)

type t
(**  Metadata for a compilation unit:
    - the module name associated to the unit
    - the filename prefix (dirname + basename with all extensions stripped)
      for compilation artifacts
    - the input source file
    For instance, when calling [ocamlopt dir/x.mli -o target/y.cmi],
    - the input source file is [dir/x.mli]
    - the module name is [Y]
    - the prefix is [target/y]
*)

(** [source_file u] is the source file of [u]. *)
val source_file: t -> filename

(** [prefix u] is the filename prefix of the unit. *)
val prefix: t -> file_prefix

(** [modname u] or [artifact_modname a] is the module name of the unit
    or compilation artifact.*)
val modname: t -> modname

(** [kind u] is the kind (interface or implementation) of the unit. *)
val kind: t -> intf_or_impl

(** [check_unit_name u] prints a warning if the derived module name [modname u]
    should not be used as a module name as specified
    by {!is_unit_name}[ ~strict:true]. *)
val check_unit_name : t -> unit

(** [make ~check ~source_file kind prefix] associates both the
    [source_file] and the module name {!modname_from_source}[ target_prefix] to
    the prefix filesystem path [prefix].

   If [check_modname=true], this function emits a warning if the derived module
   name is not valid according to {!check_unit_name}.
*)
val make:
    ?check_modname:bool -> source_file:filename ->
    intf_or_impl -> file_prefix -> t

(** {1:artifact_function Build artifacts }*)
module Artifact: sig
  type t
(**  Metadata for a single compilation artifact:
    - the module name associated to the artifact
    - the filesystem path
    - the input source file if it exists
*)

   (** [source_file a] is the source file of [a] if it exists. *)
   val source_file: t -> filename option

  (** [prefix a] is the filename prefix of the compilation artifact. *)
   val prefix: t ->  file_prefix

   (** [filename u] is the filesystem path for a compilation artifact. *)
   val filename: t -> filename

   (** [modname a] is the module name of the compilation artifact.*)
   val modname: t -> modname

   (** [from_filename filename] reconstructs the module name
       [modname_from_source filename] associated to the artifact [filename]. *)
   val from_filename: filename -> t

end

(** {1:info_build_artifacts Derived build artifact metadata} *)

(** Those functions derive a specific [artifact] metadata from an [unit]
    metadata.*)
val cmi: t -> Artifact.t
val cmo: t -> Artifact.t
val cmx: t -> Artifact.t
val obj: t -> Artifact.t
val cmt: t -> Artifact.t
val cmti: t -> Artifact.t
val annot: t -> Artifact.t

(** The functions below change the type of an artifact by updating the
    extension of its filename.
    Those functions purposefully do not cover all artifact kinds because we want
    to track which artifacts are assumed to be bundled together. *)
val companion_obj: Artifact.t -> Artifact.t
val companion_cmt: Artifact.t -> Artifact.t

val companion_cmi: Artifact.t -> Artifact.t
(** Beware that [companion_cmi a] strips all extensions from the
 filename of [a] before adding the [".cmi"] suffix contrarily to
 the other functions which only remove the rightmost extension.
 In other words, the companion cmi of a file [something.d.cmo] is
 [something.cmi] and not [something.d.cmi].
*)

(** {1:ml_mli_cmi_interaction Mli and cmi derived from implementation files } *)

(** The compilation of module implementation changes in presence of mli and cmi
    files, the function belows help to handle this. *)

(** [mli_from_source u] is the interface source filename associated to the unit
    [u]. The actual suffix depends on {!Config.interface_suffix}.
*)
val mli_from_source: t -> filename

(** [mli_from_artifact t] is the name of the interface source file derived from
    the artifact [t]. This variant is necessary when handling artifacts derived
    from an unknown source files (e.g. packed modules). *)
val mli_from_artifact: Artifact.t -> filename

(** Check if the artifact is a cmi *)
val is_cmi: Artifact.t -> bool

(** [find_normalized_cmi u] finds in the load_path a file matching the module
    name [modname u].
    @raise Not_found if no such cmi exists *)
val find_normalized_cmi: t -> Artifact.t
