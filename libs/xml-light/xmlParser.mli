(*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library has the special exception on linking described in file
 * README.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 *)

(** Xml Light Parser

 While basic parsing functions can be used in the {!Xml} module, this module
 is providing a way to create, configure and run an Xml parser.

*)

(** Abstract type for an Xml parser. *)
type t

(** Several kind of resources can contain Xml documents. *)
type source = 
	| SFile of string
	| SChannel of in_channel
	| SString of string
	| SLexbuf of Lexing.lexbuf

(** This function returns a new parser with default options. *)
val make : unit -> t

(** This function enable or disable automatic DTD proving with the parser. 
 Note that Xml documents having no reference to a DTD are never proved
 when parsed (but you can prove them later using the {!Dtd} module
 {i (by default, prove is true)}. *)
val prove : t -> bool -> unit

(** When parsing an Xml document from a file using the {!Xml.parse_file}
 function, the DTD file if declared by the Xml document has to be in the
 same directory as the xml file. When using other parsing functions, 
 such as on a string or on a channel, the parser will raise everytime
 {!Xml.File_not_found} if a DTD file is needed and prove enabled. To enable
 the DTD loading of the file, the user have to configure the Xml parser
 with a [resolve] function which is taking as argument the DTD filename and
 is returning a checked DTD. The user can then implement any kind of DTD
 loading strategy, and can use the {!Dtd} module functions to parse and check
 the DTD file {i (by default, the resolve function is raising}
 {!Xml.File_not_found}). *)
val resolve : t -> (string -> Dtd.checked) -> unit

(** When a Xml document is parsed, the parser will check that the end of the
 document is reached, so for example parsing ["<A/><B/>"] will fail instead
 of returning only the A element. You can turn off this check by setting
 [check_eof] to [false] {i (by default, check_eof is true)}. *)
val check_eof : t -> bool -> unit

(** Once the parser is configurated, you can run the parser on a any kind
 of xml document source to parse its contents into an Xml data structure. *)
val parse :  t -> source -> Xml.xml

(** When several PCData elements are separed by a \n (or \r\n), you can
 either split the PCData in two distincts PCData or merge them with \n
 as seperator into one PCData. The default behavior is to concat the
 PCData, but this can be changed for a given parser with this flag. *)
val concat_pcdata : t -> bool -> unit

(**/**)

(* internal usage only... *)
val _raises : (Xml.error_msg -> Lexing.lexbuf -> exn) -> (string -> exn) -> (Dtd.parse_error_msg -> Lexing.lexbuf -> exn) -> unit