(*
 * optParse - Functions for parsing command line arguments.
 * Copyright (C) 2004 Bardur Arantsson
 *
 * Heavily influenced by the optparse.py module from the Python
 * standard library, but with lots of adaptation to the 'Ocaml Way'
 *
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Modules for GNU [getopt(3)]-style command line parsing. *)


(** This module contains the basic functions and types for defining
  new option types and accessing the values of options. *)
module Opt :
  sig

    (** {6 Exceptions} *)

    exception No_value
    (** [No_value] gets raised by {!OptParse.Opt.get} when an option
      value is not available. *)

    exception Option_error of string * string
    (** This exception signals that an option value is invalid. The
      first string contains the option string ('-x' or '--long-name')
      and the second string contains an error message.

      This exception is only used when implementing custom option types
      and can never "escape" the scope of a {!OptParse.OptParser.parse}.
      The user should therefore not attempt to catch it.  *)

    exception Option_help
    (** When an option wants to display a usage message, this exception
      may be raised.  It can never "escape" the scope of a
      {!OptParse.OptParser.parse} call and the user should therefore not
      attempt to catch it. *)


    (** {6 Types} *)

    type 'a t = {
      option_set : string -> string list -> unit;
      option_set_value : 'a -> unit;
      option_get : unit -> 'a option;
      option_metavars : string list;
      option_defhelp : string option
    }
    (** Option type.

      [option_set] is a closure which converts and records the value of
      an option so that it can be retrieved with a later call to the
      [option_get] closure. It is called with the option name which was
      given on the command line and a list of strings, each representing
      one of the argument values given on the command line. It may raise
      [Option_error] if the value is invalid (for whatever reason).

      [option_set_value] is a closure which sets the value of an option
      to a particular value.

      [option_get] is a closure which retrieves the recorded value
      of the option. If the option value has not been set from the
      command line, the default value is used.  If there is no default
      value, then [None] should be returned.

      [option_metavars] is a list of "meta-variables" (arguments)
      which this option accepts. This is mainly for display purposes,
      but the length of this list determines how many arguments the
      option parser accepts for this option (currently only lists of
      length 0 or 1 are supported).

      [option_defhelp] is the default help string (if any).  It is
      used for displaying help messages whenever the user does {b
      not} specify a help string manually when adding this
      option. Using a non-None value here only makes sense for
      completely generic options like {!OptParse.StdOpt.help_option}.

    *)


    (** {6 Option value retrieval} *)

    val get : 'a t -> 'a
    (** Get the value of an option.

      @return the value of the option. If the option has not been
      encountered while parsing the command line, the default value is
      returned.

      @raise No_value if no default values has been given
      and the option value has not been set from the command line.

    *)

    val set : 'a t -> 'a -> unit
    (** Set the value of an option. *)

    val opt : 'a t -> 'a option
    (** Get the value of an option as an optional value.

      @return [Some x] if the option has value [x] (either by default or
      from the command line). If the option doesn't have a value [None]
      is returned. *)

    val is_set : 'a t -> bool
    (** Find out if the option has a value (either by default or
      from the command line).

      @return [True] iff the option has a value.
    *)



    (** {6 Option creation} *)

    val value_option :
      string -> 'a option -> (string -> 'a) -> (exn -> string -> string) ->
        'a t
    (** Make an option which takes a single argument.

      [value_option metavar default coerce errfmt] returns an option
      which takes a single argument from the command line and calls
      [coerce] to coerce it to the proper type. If [coerce] raises an
      exception, [exn], then [errfmt exn argval] is called to generate
      an error message for display. [metavar] is the name of the
      metavariable of the option.

      [default] is the default value of the option. If [None], the the
      option has no default value.

      @return the newly created option.

    *)

    val callback_option :
      string -> (string -> 'a) -> (exn -> string -> string) -> ('a -> unit) ->
        unit t
    (** Make a callback option which takes a single argument.

      [callback_option metavar coerce errfmt f] returns an option which
      takes a single argument from the command line and calls [coerce]
      to coerce it to the proper type. If [coerce] raises an exception
      [errfmt exn argval] is called to format an error message for
      display. If [coerce] succeeds, the callback function [f] is called
      with the coerced value. Finally, [metavar] is the name of the
      metavariable of the option.

      @return the newly created option.
    *)


  end


(** This module contains various standard options. *)
module StdOpt :
  sig

    (** {6 Flag options} *)

    val store_const : ?default: 'a -> 'a -> 'a Opt.t
    (** [store_const ?default const] returns a flag option which
      stores the constant value [const] when the option is
      encountered on the command line. *)

    val store_true : unit -> bool Opt.t
    (** [store_true ()] returns an option which is set to true when
      it is encountered on the command line. The default value is
      false. *)

    val store_false : unit -> bool Opt.t
    (** [store_false ()] returns an option which is set to false when
      it is encountered on the command line. The default value is
      true. *)

    val count_option : ?dest: int ref -> ?increment: int -> unit -> int Opt.t
    (** Create a counting option which increments its value each time the
      option is encountered on the command line.

      @param increment Increment to add to the option value each
      time the option is encountered.

      @param dest Reference to the option value. Useful for making
      options like '--quiet' and '--verbose' sharing a single value.

      @return the newly created option.
    *)

    val incr_option : ?dest: int ref -> unit -> int Opt.t
    (** Exactly identical to [count_option ~dest:dest ~increment:1 ()]. *)

    val decr_option : ?dest: int ref -> unit -> int Opt.t
    (** Exactly identical to [count_option ~dest:dest ~increment:(-1) ()]. *)


    (** {6 Value options} *)

    val int_option : ?default: int -> ?metavar: string -> unit -> int Opt.t
    (** [int_option ?default ?metavar ()] returns an option which takes
      a single integer argument. If [~default] is given it is the
      default value returned when the option has not been encountered
      on the command line. *)

    val float_option :
      ?default: float -> ?metavar: string -> unit -> float Opt.t
    (** See {!OptParse.StdOpt.int_option}. *)

    val str_option :
      ?default: string -> ?metavar: string -> unit -> string Opt.t
    (** See {!OptParse.StdOpt.int_option}. *)


    (** {6 Callback options} *)

    val int_callback : ?metavar: string -> (int -> unit) -> unit Opt.t
    (** [int_callback ?metavar f] returns an option which takes a single
      integer argument and calls [f] with that argument when encountered
      on the command line. *)

    val float_callback : ?metavar: string -> (float -> unit) -> unit Opt.t
    (** See {!OptParse.StdOpt.int_callback}. *)

    val str_callback : ?metavar: string -> (string -> unit) -> unit Opt.t
    (** See {!OptParse.StdOpt.int_callback}. *)


    (** {6 Special options} *)

    val help_option : unit -> 'a Opt.t
    (** [help_option ()] returns the standard help option which
      displays a usage message and exits the program when encountered
      on the command line. *)

    val version_option : (unit -> string) -> 'a Opt.t
    (** [version_option f] returns the standard version option which
      displays the string returned by [f ()] (and nothing else) on
      standard output and exits. *)

  end


(** This module contains the types and functions for implementing
  custom usage message formatters. *)
module Formatter :
  sig
    type t = {
      indent : unit -> unit; (** Increase the indentation level. *)
      dedent : unit -> unit; (** Decrease the indentation level. *)
      format_usage : string -> string; (** Format usage string into style of this formatter. *)
      format_heading : string -> string; (** Format heading into style of this formatter. *)
      format_description : string -> string; (** Format description into style of this formatter. *)
      format_option :
        char list * string list -> string list -> string option -> string (** Format option into style of this formatter (see explanation below). *)
    }

    (** This is the type of a formatter. The [format_option] has
      signature [format_option (snames,lnames) metavars help], where
      [snames] is a list of the short option names, [lnames] is a
      list of the long option names, [metavars] is a list of the
      metavars the option takes as arguments, and [help] is the help
      string supplied by the user.  *)


    (** {6 Standard formatters} *)


    val indented_formatter :
      ?level: int ref -> ?indent: int ref -> ?indent_increment: int ->
        ?max_help_position: int -> ?width: int -> ?short_first: bool ->
        unit -> t
    (** Create an "indented" formatter with the given options.

      @param width Total with of the usage messages printed.

      @param max_help_position Maximum starting column for the help
      messages relating to each option.

      @param short_first List all the short option names first?

      @param indent_increment Number of columns to indent by when
      more indentation is required.

      @param indent Reference to the current indentation amount. Its
      value reflects changes in indentation level.

      @param level Reference to the current indentation level. Its
      value reflects changes in indentation level.  *)

    val titled_formatter : ?level: int ref -> ?indent: int ref ->
      ?indent_increment: int -> ?max_help_position: int ->
      ?width: int -> ?short_first: bool -> unit -> t
    (** Creates a titled formatter which is quite similar to the
      indented formatter. See
      {!OptParse.Formatter.indented_formatter} for a description of
      the options. *)


    (** {6 Low-level formatting} *)


    val wrap : ?initial_indent: int -> ?subsequent_indent: int ->
      string -> int -> string list
    (** [wrap text width] reflows the given text paragraph into lines
      of width at most [width] (lines may exceed this if the are
      single words that exceed this limit).

      @param initial_indent Indentation of the first line.

      @param subsequent_indent Indentation of the following lines.

      @return a list of lines making up the reformatted paragraph. *)

    val fill : ?initial_indent: int -> ?subsequent_indent: int ->
      string -> int -> string
    (** See {!OptParse.Formatter.wrap}.

      @return a string containing the reformatted paragraph. *)

  end



(** This module contains the option parser itself.

  It provides functions to create, populate and use option parsers to
  parse command line arguments. *)
module OptParser :
  sig

    (** {6 Exceptions} *)


    exception Option_conflict of string
    (** [Option_conflic name] is raised by {!OptParse.OptParser.add}
      when two different options are added with identical
      names. Usually this doesn't need to be caught since this error
      is usually easily fixed permanently by removing/renaming the
      conflicting option names. *)


    (** {6 Types} *)


    type t
    (** The type of an option parser. *)

    type group
    (** The type of an option group. *)


    (** {6 Option parser creation} *)

    val make : ?usage: string -> ?description: string -> ?version: string ->
      ?suppress_usage: bool -> ?suppress_help: bool -> ?prog: string ->
      ?formatter: Formatter.t -> unit -> t
    (** Creates a new option parser with the given options.

      @param usage Usage message. The default is a reasonable usage
      message for most programs. Any occurrence of the substring
      ["%prog"] in [usage] is replaced with the name of the program
      (see [prog]).

      @param prog Program name. The default is the base name of the
      executable.

      @param suppress_usage Suppress the usage message if set.

      @param suppress_help Suppress the 'help' option which is
      otherwise added by default.

      @param version Version string. If set, a '--version' option is
      automatically added. When encountered on the command line it
      causes [version] to be printed to the standard output and the
      program to exit.

      @param description: description of the main purpose of the
      program.

      @return the new option parser.

    *)


    val add : t -> ?group: group -> ?help: string -> ?hide: bool ->
        ?short_name: char -> ?short_names: char list -> ?long_name: string ->
        ?long_names: string list -> 'a Opt.t -> unit
    (** Add an option to the option parser.

      @raise Option_conflict if the short name(s) or long name(s)
      have alread been used for some other option.

      @param help Short help message describing the option (for the usage message).

      @param hide If true, hide the option from the usage
      message. This can be used to implement "secret" options which
      are not shown, but work just the same as regular options in all
      other respects.

      @param short_name is the name for the short form of the option
      (e.g. ['x'] means that the option is invoked with [-x] on the
      command line).

      @param short_names is a list of names for the short form of the
      option (see [short_name]).

      @param long_name is the name for the long form of the option
      (e.g. ["xyzzy"] means that the option is invoked with [--xyzzy]
      on the command line).

      @param long_names is a list of names for the long form of the
      option (see [long_name]).
    *)


    val add_group : t -> ?parent: group -> ?description: string ->
      string -> group
    (** Add a group to the option parser.

      @param parent is the parent group (if any).

      @param description is a description of the group.

      @return the new group.

    *)

    (** {6 Output and error handling} *)

    val error : t -> ?chn: out_channel -> ?status: int -> string -> unit
    (** Display an error message and exit the program. The error
      message is printed to the channel [chn] (default is
      [Pervasives.stderr]) and the program exits with exit status
      [status] (default is 1). *)

    val usage : t -> ?chn: out_channel -> unit -> unit
    (** Display the usage message to the channel [chn] (default is
      [Pervasives.stdout]) and return. *)


    (** {6 Option parsing} *)

    val parse : t -> ?first: int -> ?last: int -> string array -> string list
    (** Parse arguments as if the arguments [args.(first)],
      [args.(first+1)], ..., [args.(last)] had been given on the
      command line. By default [first] is 0 and [last] is the index
      of the last element of the array. *)

    val parse_argv : t -> string list
    (** Parse all the arguments in [Sys.argv]. *)

  end
