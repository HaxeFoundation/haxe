(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy, projet Gallium, INRIA Rocquencourt                     *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

module Consistbl : module type of struct
  include Consistbl.Make (Misc.Stdlib.String)
end

type error =
  | Illegal_renaming of modname * modname * filepath
  | Inconsistent_import of modname * filepath * filepath
  | Need_recursive_types of modname

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer

module Persistent_signature : sig
  type t =
    { filename : string; (** Name of the file containing the signature. *)
      cmi : Cmi_format.cmi_infos;
      visibility : Load_path.visibility
    }

  (** Function used to load a persistent signature. The default is to look for
      the .cmi file in the load path. This function can be overridden to load
      it from memory, for instance to build a self-contained toplevel. *)
  val load : (allow_hidden:bool -> unit_name:string -> t option) ref
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Lazy_backtrack.log

type 'a t

val empty : unit -> 'a t

val clear : 'a t -> unit
val clear_missing : 'a t -> unit

val fold : 'a t -> (modname -> 'a -> 'b -> 'b) -> 'b -> 'b

val read : 'a t -> (Persistent_signature.t -> 'a) -> Unit_info.Artifact.t -> 'a
val find : allow_hidden:bool -> 'a t -> (Persistent_signature.t -> 'a)
  -> modname -> 'a

val find_in_cache : 'a t -> modname -> 'a option

val check : allow_hidden:bool -> 'a t -> (Persistent_signature.t -> 'a)
  -> loc:Location.t -> modname -> unit

(* [looked_up penv md] checks if one has already tried
   to read the signature for [md] in the environment
   [penv] (it may have failed) *)
val looked_up : 'a t -> modname -> bool

(* [is_imported penv md] checks if [md] has been successfully
   imported in the environment [penv] *)
val is_imported : 'a t -> modname -> bool

(* [is_imported_opaque penv md] checks if [md] has been imported
   in [penv] as an opaque module *)
val is_imported_opaque : 'a t -> modname -> bool

(* [register_import_as_opaque penv md] registers [md] in [penv] as an
   opaque module *)
val register_import_as_opaque : 'a t -> modname -> unit

val make_cmi : 'a t -> modname -> Types.signature -> alerts
  -> Cmi_format.cmi_infos

val save_cmi : 'a t -> Persistent_signature.t -> 'a -> unit

val can_load_cmis : 'a t -> can_load_cmis
val set_can_load_cmis : 'a t -> can_load_cmis -> unit
val without_cmis : 'a t -> ('b -> 'c) -> 'b -> 'c
(* [without_cmis penv f arg] applies [f] to [arg], but does not
    allow [penv] to openi cmis during its execution *)

(* may raise Consistbl.Inconsistency *)
val import_crcs : 'a t -> source:filepath -> crcs -> unit

(* Return the set of compilation units imported, with their CRC *)
val imports : 'a t -> crcs

(* Return the CRC of the interface of the given compilation unit *)
val crc_of_unit: 'a t -> (Persistent_signature.t -> 'a) -> modname -> Digest.t

(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref
