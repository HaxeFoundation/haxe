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

(** Source code locations (ranges of positions), used in parsetree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Format

type t = Warnings.loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

(** Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val is_none : t -> bool
(** True for [Location.none], false any other location *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val rhs_interval: int -> int -> t

val get_pos_info: Lexing.position -> string * int * int
(** file, line, char *)

type 'a loc = {
  txt : 'a;
  loc : t;
}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc


(** {1 Input info} *)

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

(* This is used for reporting errors coming from the toplevel.

   When running a toplevel session (i.e. when [!input_name] is "//toplevel//"),
   [!input_phrase_buffer] should be [Some buf] where [buf] contains the last
   toplevel phrase. *)
val input_phrase_buffer: Buffer.t option ref


(** {1 Toplevel-specific functions} *)

val echo_eof: unit -> unit
val reset: unit -> unit


(** {1 Rewriting path } *)

val rewrite_absolute_path: string -> string
(** [rewrite_absolute_path path] rewrites [path] to honor the
    BUILD_PATH_PREFIX_MAP variable
    if it is set. It does not check whether [path] is absolute or not.
    The result is as follows:
    - If BUILD_PATH_PREFIX_MAP is not set, just return [path].
    - otherwise, rewrite using the mapping (and if there are no
      matching prefixes that will just return [path]).

    See
    {{: https://reproducible-builds.org/specs/build-path-prefix-map/ }
    the BUILD_PATH_PREFIX_MAP spec}
    *)

val rewrite_find_first_existing: string -> string option
(** [rewrite_find_first_existing path] uses a BUILD_PATH_PREFIX_MAP mapping
    and tries to find a source in mapping
    that maps to a result that exists in the file system.
    There are the following return values:
    - [None], means either
      {ul {- BUILD_PATH_PREFIX_MAP is not set and [path] does not exists, or}
          {- no source prefixes of [path] in the mapping were found,}}
    - [Some target], means [target] exists and either
      {ul {- BUILD_PATH_PREFIX_MAP is not set and [target] = [path], or}
          {- [target] is the first file (in priority
             order) that [path] mapped to that exists in the file system.}}
    - [Not_found] raised, means some source prefixes in the map
      were found that matched [path], but none of them existed
      in the file system. The caller should catch this and issue
      an appropriate error message.

    See
    {{: https://reproducible-builds.org/specs/build-path-prefix-map/ }
    the BUILD_PATH_PREFIX_MAP spec}
    *)

val rewrite_find_all_existing_dirs: string -> string list
(** [rewrite_find_all_existing_dirs dir] accumulates a list of existing
    directories, [dirs], that are the result of mapping a potentially
    abstract directory, [dir], over all the mapping pairs in the
    BUILD_PATH_PREFIX_MAP environment variable, if any. The list [dirs]
    will be in priority order (head as highest priority).

    The possible results are:
    - [[]], means either
      {ul {- BUILD_PATH_PREFIX_MAP is not set and [dir] is not an existing
      directory, or}
          {- if set, then there were no matching prefixes of [dir].}}
    - [Some dirs], means dirs are the directories found. Either
      {ul {- BUILD_PATH_PREFIX_MAP is not set and [dirs = [dir]], or}
          {- it was set and [dirs] are the mapped existing directories.}}
    - Not_found raised, means some source prefixes in the map
      were found that matched [dir], but none of mapping results
      were existing directories (possibly due to misconfiguration).
      The caller should catch this and issue an appropriate error
      message.

    See
    {{: https://reproducible-builds.org/specs/build-path-prefix-map/ }
    the BUILD_PATH_PREFIX_MAP spec}
    *)

val absolute_path: string -> string
 (** [absolute_path path] first makes an absolute path, [s] from [path],
     prepending the current working directory if [path] was relative.
     Then [s] is rewritten using [rewrite_absolute_path].
     Finally the result is normalized by eliminating instances of
     ['.'] or ['..']. *)

(** {1 Printing locations} *)

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)

val print_filename: formatter -> string -> unit
val print_loc: formatter -> t -> unit
val print_locs: formatter -> t list -> unit
val separate_new_message: formatter -> unit

module Doc: sig
  val separate_new_message: unit Format_doc.printer
  val filename: string Format_doc.printer
  val quoted_filename: string Format_doc.printer
  val loc: t Format_doc.printer
  val locs: t list Format_doc.printer
end

(** {1 Toplevel-specific location highlighting} *)

val highlight_terminfo:
  Lexing.lexbuf -> formatter -> t list -> unit


(** {1 Reporting errors and warnings} *)

(** {2 The type of reports and report printers} *)

type msg = Format_doc.t loc

val msg: ?loc:t -> ('a, Format_doc.formatter, unit, msg) format4 -> 'a

type report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

type report = {
  kind : report_kind;
  main : msg;
  sub : msg list;
  footnote: Format_doc.t option
}

type report_printer = {
  (* The entry point *)
  pp : report_printer ->
    Format.formatter -> report -> unit;

  pp_report_kind : report_printer -> report ->
    Format.formatter -> report_kind -> unit;
  pp_main_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_main_txt : report_printer -> report ->
    Format.formatter -> Format_doc.t -> unit;
  pp_submsgs : report_printer -> report ->
    Format.formatter -> msg list -> unit;
  pp_submsg : report_printer -> report ->
    Format.formatter -> msg -> unit;
  pp_submsg_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_submsg_txt : report_printer -> report ->
    Format.formatter -> Format_doc.t -> unit;
}
(** A printer for [report]s, defined using open-recursion.
    The goal is to make it easy to define new printers by re-using code from
    existing ones.
*)

(** {2 Report printers used in the compiler} *)

val batch_mode_printer: report_printer

val terminfo_toplevel_printer: Lexing.lexbuf -> report_printer

val best_toplevel_printer: unit -> report_printer
(** Detects the terminal capabilities and selects an adequate printer *)

(** {2 Printing a [report]} *)

val print_report: formatter -> report -> unit
(** Display an error or warning report. *)

val report_printer: (unit -> report_printer) ref
(** Hook for redefining the printer of reports.

    The hook is a [unit -> report_printer] and not simply a [report_printer]:
    this is useful so that it can detect the type of the output (a file, a
    terminal, ...) and select a printer accordingly. *)

val default_report_printer: unit -> report_printer
(** Original report printer for use in hooks. *)


(** {1 Reporting warnings} *)

(** {2 Converting a [Warnings.t] into a [report]} *)

val report_warning: t -> Warnings.t -> report option
(** [report_warning loc w] produces a report for the given warning [w], or
   [None] if the warning is not to be printed. *)

val warning_reporter: (t -> Warnings.t -> report option) ref
(** Hook for intercepting warnings. *)

val default_warning_reporter: t -> Warnings.t -> report option
(** Original warning reporter for use in hooks. *)

(** {2 Printing warnings} *)

val formatter_for_warnings : formatter ref

val print_warning: t -> formatter -> Warnings.t -> unit
(** Prints a warning. This is simply the composition of [report_warning] and
   [print_report]. *)

val prerr_warning: t -> Warnings.t -> unit
(** Same as [print_warning], but uses [!formatter_for_warnings] as output
   formatter. *)

(** {1 Reporting alerts} *)

(** {2 Converting an [Alert.t] into a [report]} *)

val report_alert: t -> Warnings.alert -> report option
(** [report_alert loc w] produces a report for the given alert [w], or
   [None] if the alert is not to be printed. *)

val alert_reporter: (t -> Warnings.alert -> report option) ref
(** Hook for intercepting alerts. *)

val default_alert_reporter: t -> Warnings.alert -> report option
(** Original alert reporter for use in hooks. *)

(** {2 Printing alerts} *)

val print_alert: t -> formatter -> Warnings.alert -> unit
(** Prints an alert. This is simply the composition of [report_alert] and
   [print_report]. *)

val prerr_alert: t -> Warnings.alert -> unit
(** Same as [print_alert], but uses [!formatter_for_warnings] as output
   formatter. *)

val deprecated: ?def:t -> ?use:t -> t -> string -> unit
(** Prints a deprecation alert. *)

val alert: ?def:t -> ?use:t -> kind:string -> t -> string -> unit
(** Prints an arbitrary alert. *)

val auto_include_alert: string -> unit
(** Prints an alert that -I +lib has been automatically added to the load
    path *)

val deprecated_script_alert: string -> unit
(** [deprecated_script_alert command] prints an alert that [command foo] has
    been deprecated in favour of [command ./foo] *)

(** {1 Reporting errors} *)

type error = report
(** An [error] is a [report] which [report_kind] must be [Report_error]. *)

type delayed_msg = unit -> Format_doc.t option

val error: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg -> string -> error

val errorf: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg ->
  ('a, Format_doc.formatter, unit, error) format4 -> 'a

val error_of_printer: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg ->
  (Format_doc.formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (Format_doc.formatter -> 'a -> unit) -> 'a -> error


(** {1 Automatically reporting errors for raised exceptions} *)

val register_error_of_exn: (exn -> error option) -> unit
(** Each compiler module which defines a custom type of exception
    which can surface as a user-visible error should register
    a "printer" for this exception using [register_error_of_exn].
    The result of the printer is an [error] value containing
    a location, a message, and optionally sub-messages (each of them
    being located as well). *)

val error_of_exn: exn -> [ `Ok of error | `Already_displayed ] option

exception Error of error
(** Raising [Error e] signals an error [e]; the exception will be caught and the
   error will be printed. *)

exception Already_displayed_error
(** Raising [Already_displayed_error] signals an error which has already been
   printed. The exception will be caught, but nothing will be printed *)

val raise_errorf: ?loc:t -> ?sub:msg list -> ?footnote:delayed_msg ->
  ('a, Format_doc.formatter, unit, 'b) format4 -> 'a

val report_exception: formatter -> exn -> unit
(** Reraise the exception if it is unknown. *)
