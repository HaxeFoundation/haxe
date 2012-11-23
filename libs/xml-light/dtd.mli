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

(** Xml Light DTD

	This module provide several functions to create, check, and use DTD
	to prove Xml documents : {ul
	{li using the DTD types, you can directly create your own DTD structure}
	{li the {!Dtd.check} function can then be used to check that all DTD
		states have been declared, that no attributes are declared twice,
		and so on.}
	{li the {!Dtd.prove} function can be used to check an {!Xml} data
		structure with a checked DTD. The function will return the
		expanded Xml document or raise an exception if the DTD proving
		fails.}
	}

	{i Note about ENTITIES:}
	
	While parsing Xml, PCDATA is always parsed and
	the Xml entities &amp; &gt; &lt; &apos; &quot; are replaced by their
	corresponding ASCII characters. For Xml attributes, theses can be
	put between either double or simple quotes, and the backslash character
	can be used to escape inner quotes. There is no support for CDATA Xml
	nodes or PCDATA attributes declarations in DTD, and no support for
	user-defined entities using the ENTITY DTD element.
*)

(** {6 The DTD Types} *)

type dtd_child =
	| DTDTag of string
	| DTDPCData
	| DTDOptional of dtd_child
	| DTDZeroOrMore of dtd_child
	| DTDOneOrMore of dtd_child
	| DTDChoice of dtd_child list
	| DTDChildren of dtd_child list

type dtd_element_type =
	| DTDEmpty
	| DTDAny
	| DTDChild of dtd_child

type dtd_attr_default =
	| DTDDefault of string
	| DTDRequired
	| DTDImplied
	| DTDFixed of string

type dtd_attr_type =
	| DTDCData
	| DTDNMToken
	| DTDEnum of string list
	| DTDID
	| DTDIDRef

type dtd_item =
	| DTDAttribute of string * string * dtd_attr_type * dtd_attr_default
	| DTDElement of string * dtd_element_type

type dtd = dtd_item list

type checked

(** {6 The DTD Functions} *)

(** Parse the named file into a Dtd data structure. Raise
	{!Xml.File_not_found} if an error occured while opening the file. 
	Raise {!Dtd.Parse_error} if parsing failed. *)
val parse_file : string -> dtd

(** Read the content of the in_channel and parse it into a Dtd data
 structure. Raise {!Dtd.Parse_error} if parsing failed. *)
val parse_in : in_channel -> dtd

(** Parse the string containing a Dtd document into a Dtd data
 structure. Raise {!Dtd.Parse_error} if parsing failed. *)
val parse_string : string -> dtd

(** Check the Dtd data structure declaration and return a checked
 DTD. Raise {!Dtd.Check_error} if the DTD checking failed. *)
val check : dtd -> checked

(** Prove an Xml document using a checked DTD and an entry point.
 The entry point is the first excepted tag of the Xml document,
 the returned Xml document has the same structure has the original
 one, excepted that non declared optional attributes have been set
 to their default value specified in the DTD.
 Raise {!Dtd.Check_error} [ElementNotDeclared] if the entry point
 is not found, raise {!Dtd.Prove_error} if the Xml document failed
 to be proved with the DTD. *)
val prove : checked -> string -> Xml.xml -> Xml.xml

(** Print a DTD element into a string. You can easily get a DTD
 document from a DTD data structure using for example
 [String.concat "\n" (List.map Dtd.to_string) my_dtd] *)
val to_string : dtd_item -> string

(** {6 The DTD Exceptions} *)

(** There is three types of DTD excecptions : {ul
	{li {!Dtd.Parse_error} is raised when an error occured while
	parsing a DTD document into a DTD data structure.}
	{li {!Dtd.Check_error} is raised when an error occured while
	checking a DTD data structure for completeness, or when the
	prove entry point is not found when calling {!Dtd.prove}.}
	{li {!Dtd.Prove_error} is raised when an error occured while
	proving an Xml document.}
	}

	Several string conversion functions are provided to enable you
	to report errors to the user.
*)

type parse_error_msg =
	| InvalidDTDDecl
	| InvalidDTDElement
	| InvalidDTDAttribute
	| InvalidDTDTag
	| DTDItemExpected

type check_error =
	| ElementDefinedTwice of string
	| AttributeDefinedTwice of string * string
	| ElementEmptyContructor of string
	| ElementReferenced of string * string
	| ElementNotDeclared of string
	| WrongImplicitValueForID of string * string

type prove_error =
	| UnexpectedPCData
	| UnexpectedTag of string
	| UnexpectedAttribute of string
	| InvalidAttributeValue of string
	| RequiredAttribute of string
	| ChildExpected of string
	| EmptyExpected
	| DuplicateID of string
	| MissingID of string

type parse_error = parse_error_msg * Xml.error_pos

exception Parse_error of parse_error
exception Check_error of check_error
exception Prove_error of prove_error

val parse_error : parse_error -> string
val check_error : check_error -> string
val prove_error : prove_error -> string

(**/**)

(* internal usage only... *)
val _raises : (string -> exn) -> unit
