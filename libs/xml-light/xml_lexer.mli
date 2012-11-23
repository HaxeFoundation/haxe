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

type error =
	| EUnterminatedComment
	| EUnterminatedString
	| EIdentExpected
	| ECloseExpected
	| ENodeExpected
	| EAttributeNameExpected
	| EAttributeValueExpected
	| EUnterminatedEntity

type dtd_error =
	| EInvalidDTDDecl
	| EInvalidDTDTag
	| EDTDItemExpected
	| EInvalidDTDElement
	| EInvalidDTDAttribute

exception Error of error
exception DTDError of dtd_error

type dtd_decl =
	| DTDFile of string
	| DTDData of Dtd.dtd

type token =
	| Tag of string * (string * string) list * bool
	| PCData of string
	| Endtag of string
	| DocType of (string * dtd_decl)
	| Eof

type pos = int * int * int * int

val init : Lexing.lexbuf -> unit 
val close : Lexing.lexbuf -> unit
val token : Lexing.lexbuf -> token
val dtd : Lexing.lexbuf -> Dtd.dtd
val pos : Lexing.lexbuf -> pos
val restore : pos -> unit