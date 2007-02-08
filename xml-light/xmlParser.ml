(*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 * Copyright (C) 2003 Jacques Garrigue
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

open Printf
open Dtd
open Xml

type t = {
	mutable prove : bool;
	mutable check_eof : bool;
	mutable concat_pcdata : bool;
	mutable resolve : (string -> checked);
}

type source = 
	| SFile of string
	| SChannel of in_channel
	| SString of string
	| SLexbuf of Lexing.lexbuf

type state = {
	source : Lexing.lexbuf;
	stack : Xml_lexer.token Stack.t;
	xparser : t;
}

exception Internal_error of Xml.error_msg
exception NoMoreData

let xml_error = ref (fun _ -> assert false)
let dtd_error = ref (fun _ -> assert false)
let file_not_found = ref (fun _ -> assert false)

let _raises e f d =
	xml_error := e;
	file_not_found := f;
	dtd_error := d

let make () =
	{
		prove = true;
		check_eof = true;
		concat_pcdata = true;
		resolve = (fun file -> raise (!file_not_found file))
	}

let prove p v = p.prove <- v
let resolve p f = p.resolve <- f
let check_eof p v = p.check_eof <- v
let concat_pcdata p v = p.concat_pcdata <- v

let pop s =
	try
		Stack.pop s.stack
	with
		Stack.Empty ->
			Xml_lexer.token s.source

let push t s =
	Stack.push t s.stack

let rec read_node s =
	match pop s with
	| Xml_lexer.PCData s -> PCData s
	| Xml_lexer.Tag (tag, attr, true) -> Element (tag, attr, [])
	| Xml_lexer.Tag (tag, attr, false) -> Element (tag, attr, read_elems ~tag s)
	| t ->
		push t s;
		raise NoMoreData
and
	read_elems ?tag s =
		let elems = ref [] in
		(try
			while true do
				match s.xparser.concat_pcdata , read_node s , !elems with
				| true , PCData c , (PCData c2) :: q ->
					elems := PCData (sprintf "%s\n%s" c2 c) :: q
				| _ , x , l ->
					elems := x :: l
			done
		with
			NoMoreData -> ());
		match pop s with
		| Xml_lexer.Endtag s when Some s = tag -> List.rev !elems
		| Xml_lexer.Eof when tag = None -> List.rev !elems
		| t ->
			match tag with
			| None -> raise (Internal_error EOFExpected)
			| Some s -> raise (Internal_error (EndOfTagExpected s))

let read_xml s =
	match s.xparser.prove, pop s with
	| true, Xml_lexer.DocType (root, Xml_lexer.DTDFile file) ->
		let pos = Xml_lexer.pos s.source in
		let dtd = s.xparser.resolve file in
		Xml_lexer.restore pos;
		let x = read_node s in
		Dtd.prove dtd root x
	| true, Xml_lexer.DocType (root, Xml_lexer.DTDData dtd) ->
		let dtd = Dtd.check dtd in
		let x = read_node s in
		Dtd.prove dtd root x
	| false, Xml_lexer.DocType _ ->
		read_node s
	| _, t ->
		push t s;
		read_node s

let convert = function
	| Xml_lexer.EUnterminatedComment -> UnterminatedComment
	| Xml_lexer.EUnterminatedString -> UnterminatedString
	| Xml_lexer.EIdentExpected -> IdentExpected
	| Xml_lexer.ECloseExpected -> CloseExpected
	| Xml_lexer.ENodeExpected -> NodeExpected
	| Xml_lexer.EAttributeNameExpected -> AttributeNameExpected
	| Xml_lexer.EAttributeValueExpected -> AttributeValueExpected
	| Xml_lexer.EUnterminatedEntity -> 	UnterminatedEntity

let dtd_convert = function
	| Xml_lexer.EInvalidDTDDecl -> InvalidDTDDecl
	| Xml_lexer.EInvalidDTDTag -> InvalidDTDTag
	| Xml_lexer.EDTDItemExpected -> DTDItemExpected
	| Xml_lexer.EInvalidDTDElement -> InvalidDTDElement
	| Xml_lexer.EInvalidDTDAttribute -> InvalidDTDAttribute

let do_parse xparser source =
	try
		Xml_lexer.init source;
		let s = { source = source; xparser = xparser; stack = Stack.create(); } in
		let tk = pop s in
		(* skip UTF8 BOM *)
		if tk <> Xml_lexer.PCData "\239\187\191" then push tk s;
		let x = read_xml s in
		if xparser.check_eof && pop s <> Xml_lexer.Eof then raise (Internal_error EOFExpected);
		Xml_lexer.close source;
		x
	with
		| NoMoreData ->
			Xml_lexer.close source;
			raise (!xml_error NodeExpected source)
		| Internal_error e ->
			Xml_lexer.close source;
			raise (!xml_error e source)
		| Xml_lexer.Error e ->
			Xml_lexer.close source;
			raise (!xml_error (convert e) source)
		| Xml_lexer.DTDError e ->
			Xml_lexer.close source;
			raise (!dtd_error (dtd_convert e) source)

let parse p = function
	| SChannel ch -> do_parse p (Lexing.from_channel ch)
	| SString str -> do_parse p (Lexing.from_string str)
	| SLexbuf lex -> do_parse p lex
	| SFile fname ->
		let ch = (try open_in fname with Sys_error _ -> raise (!file_not_found fname)) in
		try
			let x = do_parse p (Lexing.from_channel ch) in
			close_in ch;
			x
		with
			e ->
				close_in ch;
				raise e
