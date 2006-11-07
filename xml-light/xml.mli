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

(** Xml Light
 
  Xml Light is a minimal Xml parser & printer for OCaml.
  It provide few functions to parse a basic Xml document into
  an OCaml data structure and to print back the data structures
  to an Xml document.

  Xml Light has also support for {b DTD} (Document Type Definition).

  {i (c)Copyright 2002-2003 Nicolas Cannasse}
*)

(** {6 Xml Data Structure} *)

(** An Xml node is either
	[Element (tag-name, attributes, children)] or [PCData text] *)
type xml = 
	| Element of (string * (string * string) list * xml list)
	| PCData of string

(** {6 Xml Parsing} *)

(** For easily parsing an Xml data source into an xml data structure,
	you can use theses functions. But if you want advanced parsing usage,
	please look at the {!XmlParser} module.
	All the parsing functions can raise some exceptions, see the
	{{:#exc}Exceptions} section for more informations. *)

(** Parse the named file into an Xml data structure. *)
val parse_file : string -> xml

(** Read the content of the in_channel and parse it into an Xml data
 structure. *)
val parse_in : in_channel -> xml

(** Parse the string containing an Xml document into an Xml data
 structure. *)
val parse_string : string -> xml

(** {6:exc Xml Exceptions} *)

(** Several exceptions can be raised when parsing an Xml document : {ul
	{li {!Xml.Error} is raised when an xml parsing error occurs. the
		{!Xml.error_msg} tells you which error occured during parsing
		and the {!Xml.error_pos} can be used to retreive the document
		location where the error occured at.}
	{li {!Xml.File_not_found} is raised when and error occured while
		opening a file with the {!Xml.parse_file} function or when a
		DTD file declared by the Xml document is not found {i (see the
		{!XmlParser} module for more informations on how to handle the
		DTD file loading)}.}
	}
	If the Xml document is containing a DTD, then some other exceptions
	can be raised, see the module {!Dtd} for more informations.
 *)

type error_pos

type error_msg =
	| UnterminatedComment
	| UnterminatedString
	| UnterminatedEntity
	| IdentExpected
	| CloseExpected
	| NodeExpected
	| AttributeNameExpected
	| AttributeValueExpected
	| EndOfTagExpected of string
	| EOFExpected

type error = error_msg * error_pos

exception Error of error

exception File_not_found of string

(** Get a full error message from an Xml error. *)
val error : error -> string

(** Get the Xml error message as a string. *)
val error_msg : error_msg -> string 

(** Get the line the error occured at. *)
val line : error_pos -> int

(** Get the relative character range (in current line) the error occured at.*)
val range : error_pos -> int * int

(** Get the absolute character range the error occured at. *)
val abs_range : error_pos -> int * int

(** {6 Xml Functions} *)

exception Not_element of xml
exception Not_pcdata of xml
exception No_attribute of string

(** [tag xdata] returns the tag value of the xml node.
 Raise {!Xml.Not_element} if the xml is not an element *)
val tag : xml -> string

(** [pcdata xdata] returns the PCData value of the xml node.
 Raise {!Xml.Not_pcdata} if the xml is not a PCData *)
val pcdata : xml -> string

(** [attribs xdata] returns the attribute list of the xml node.
 First string if the attribute name, second string is attribute value.
 Raise {!Xml.Not_element} if the xml is not an element *)
val attribs : xml -> (string * string) list 

(** [attrib xdata "href"] returns the value of the ["href"]
 attribute of the xml node (attribute matching is case-insensitive).
 Raise {!Xml.No_attribute} if the attribute does not exists in the node's
 attribute list 
 Raise {!Xml.Not_element} if the xml is not an element *)
val attrib : xml -> string -> string

(** [children xdata] returns the children list of the xml node
 Raise {!Xml.Not_element} if the xml is not an element *)
val children : xml -> xml list

(*** [enum xdata] returns the children enumeration of the xml node
 Raise {!Xml.Not_element} if the xml is not an element *)
(* val enum : xml -> xml Enum.t *)

(** [iter f xdata] calls f on all children of the xml node.
 Raise {!Xml.Not_element} if the xml is not an element *)
val iter : (xml -> unit) -> xml -> unit

(** [map f xdata] is equivalent to [List.map f (Xml.children xdata)]
 Raise {!Xml.Not_element} if the xml is not an element *)
val map : (xml -> 'a) -> xml -> 'a list

(** [fold f init xdata] is equivalent to
 [List.fold_left f init (Xml.children xdata)]
 Raise {!Xml.Not_element} if the xml is not an element *)
val fold : ('a -> xml -> 'a) -> 'a -> xml -> 'a

(** {6 Xml Printing} *)

(** Print the xml data structure into a compact xml string (without
 any user-readable formating ). *)
val to_string : xml -> string

(** Print the xml data structure into an user-readable string with
 tabs and lines break between different nodes. *)
val to_string_fmt : xml -> string
