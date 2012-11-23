{(*
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

open Lexing
open Xml_parser
open Dtd

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

type pos = int * int * int * int

type dtd_decl =
	| DTDFile of string
	| DTDData of dtd

type dtd_item_type =
	| TElement
	| TAttribute

type token =
	| Tag of string * (string * string) list * bool
	| PCData of string
	| Endtag of string
	| DocType of (string * dtd_decl)
	| Eof

let last_pos = ref 0
and current_line = ref 0
and current_line_start = ref 0

let tmp = Buffer.create 200

let idents = Hashtbl.create 0

let _ = begin
	Hashtbl.add idents "gt;" ">";
	Hashtbl.add idents "lt;" "<";
	Hashtbl.add idents "amp;" "&";
	Hashtbl.add idents "apos;" "'";
	Hashtbl.add idents "quot;" "\"";
end

let init lexbuf =
	current_line := 1;
	current_line_start := lexeme_start lexbuf;
	last_pos := !current_line_start

let close lexbuf =
	Buffer.reset tmp

let pos lexbuf =
	!current_line ,	!current_line_start ,
	!last_pos ,
	lexeme_start lexbuf

let restore (cl,cls,lp,_) =
	current_line := cl;
	current_line_start := cls;
	last_pos := lp

let newline lexbuf =
	incr current_line;
	last_pos := lexeme_end lexbuf;
	current_line_start := !last_pos

let error lexbuf e =
	last_pos := lexeme_start lexbuf;
	raise (Error e)

let dtd_error lexbuf e =
	last_pos := lexeme_start lexbuf;
	raise (DTDError e)
}

let newline = ['\n']
let break = ['\r']
let space = [' ' '\t']
let identchar =  ['A'-'Z' 'a'-'z' '_' '0'-'9' ':' '-']
let entitychar = ['A'-'Z' 'a'-'z']
let pcchar = [^ '\r' '\n' '<' '>' '&']
let cdata_start = ['c''C']['d''D']['a''A']['t''T']['a''A']

rule token = parse
	| newline
		{
			newline lexbuf;
			token lexbuf
		}
	| (space | break) +
		{
			last_pos := lexeme_end lexbuf;
			token lexbuf
		}
	| "<!DOCTYPE"
		{
			last_pos := lexeme_start lexbuf;
			ignore_spaces lexbuf;
			let root = ident_name lexbuf in
			ignore_spaces lexbuf;
			let data = dtd_data lexbuf in
			DocType (root, data)
		}
	| "<![" cdata_start '['
		{
			last_pos := lexeme_start lexbuf;
			Buffer.reset tmp;
			PCData (cdata lexbuf)
		}		
	| "<!--"
		{
			last_pos := lexeme_start lexbuf;
			comment lexbuf;
			token lexbuf
		}
	| "<?"
		{
			last_pos := lexeme_start lexbuf;
			header lexbuf;
			token lexbuf;
		}
	| '<' space* '/' space*
		{
			last_pos := lexeme_start lexbuf;
			let tag = ident_name lexbuf in
			ignore_spaces lexbuf;
			close_tag lexbuf;
			Endtag tag
		}
	| '<' space*
		{
			last_pos := lexeme_start lexbuf;
			let tag = ident_name lexbuf in
			ignore_spaces lexbuf;
			let attribs, closed = attributes lexbuf in
			Tag(tag, attribs, closed)
		}
	| "&#"
		{
			last_pos := lexeme_start lexbuf;
			Buffer.reset tmp;
			Buffer.add_string tmp (lexeme lexbuf);
			PCData (pcdata lexbuf)
		}
	| '&'
		{
			last_pos := lexeme_start lexbuf;
			Buffer.reset tmp;
			Buffer.add_string tmp (entity lexbuf);
			PCData (pcdata lexbuf)
		}
	| space* pcchar+
		{
			last_pos := lexeme_start lexbuf;
			Buffer.reset tmp;
			Buffer.add_string tmp (lexeme lexbuf);
			PCData (pcdata lexbuf)
		}
	| eof { Eof }
	| _
		{ error lexbuf ENodeExpected }

and ignore_spaces = parse
	| newline
		{
			newline lexbuf;
			ignore_spaces lexbuf
		}
	| (space | break) +
		{ ignore_spaces lexbuf }
	| ""
		{ () }

and comment = parse
	| newline
		{
			newline lexbuf;
			comment lexbuf
		}
	| "-->"
		{ () }
	| eof
		{ raise (Error EUnterminatedComment) }
	| _
		{ comment lexbuf }

and header = parse
	| newline
		{
			newline lexbuf;
			header lexbuf
		}
	| "?>"
		{ () }
	| eof
		{ error lexbuf ECloseExpected }
	| _
		{ header lexbuf }		

and cdata = parse
	| [^ ']' '\n']+
		{
			Buffer.add_string tmp (lexeme lexbuf);
			cdata lexbuf
		}
	| newline 
		{
			newline lexbuf;
			Buffer.add_string tmp (lexeme lexbuf);
			cdata lexbuf
		}
	| "]]>"
		{ Buffer.contents tmp }
	| ']'
		{
			Buffer.add_string tmp (lexeme lexbuf);
			cdata lexbuf
		}
	| eof
		{ error lexbuf ECloseExpected }

and pcdata = parse
	| pcchar+
		{
			Buffer.add_string tmp (lexeme lexbuf);
			pcdata lexbuf
		}
	| "&#"
		{
			Buffer.add_string tmp (lexeme lexbuf);
			pcdata lexbuf;
		}
	| '&'
		{
			Buffer.add_string tmp (entity lexbuf);
			pcdata lexbuf
		}
	| ""
		{ Buffer.contents tmp }

and entity = parse
	| entitychar+ ';'
		{
			let ident = lexeme lexbuf in
			try
				Hashtbl.find idents (String.lowercase ident)
			with
				Not_found -> "&" ^ ident
		}
	| _ | eof
		{ raise (Error EUnterminatedEntity) }

and ident_name = parse
	| identchar+
		{ lexeme lexbuf }
	| _ | eof
		{ error lexbuf EIdentExpected }

and close_tag = parse
	| '>'
		{ () }
	| _ | eof
		{ error lexbuf ECloseExpected }

and attributes = parse
	| '>'
		{ [], false }
	| "/>"
		{ [], true }
	| "" (* do not read a char ! *)
		{
			let key = attribute lexbuf in
			let data = attribute_data lexbuf in
			ignore_spaces lexbuf;
			let others, closed = attributes lexbuf in
			(key, data) :: others, closed
		}

and attribute = parse
	| identchar+
		{ lexeme lexbuf }
	| _ | eof
		{ error lexbuf EAttributeNameExpected }

and attribute_data = parse
	| space* '=' space* '"'
		{
			Buffer.reset tmp;
			last_pos := lexeme_end lexbuf;
			dq_string lexbuf
		}
	| space* '=' space* '\''
		{
			Buffer.reset tmp;
			last_pos := lexeme_end lexbuf;
			q_string lexbuf
		}
	| _ | eof
		{ error lexbuf EAttributeValueExpected }

and dq_string = parse
	| '"'
		{ Buffer.contents tmp }
	| '\\' [ '"' '\\' ]
		{
			Buffer.add_char tmp (lexeme_char lexbuf 1);
			dq_string lexbuf
		}
	| eof
		{ raise (Error EUnterminatedString) }
	| _
		{ 
			Buffer.add_char tmp (lexeme_char lexbuf 0);
			dq_string lexbuf
		}

and q_string = parse
	| '\''
		{ Buffer.contents tmp }
	| '\\' [ '\'' '\\' ]
		{
			Buffer.add_char tmp (lexeme_char lexbuf 1);
			q_string lexbuf
		}
	| eof
		{ raise (Error EUnterminatedString) }
	| _
		{ 
			Buffer.add_char tmp (lexeme_char lexbuf 0);
			q_string lexbuf
		}

and dtd_data = parse
	| "PUBLIC"
		{
			ignore_spaces lexbuf;
			(* skipping Public ID *)
			let _ = dtd_file lexbuf in
			let file = dtd_file lexbuf in
			dtd_end_decl lexbuf;
			DTDFile file
		}
	| "SYSTEM"
		{
			ignore_spaces lexbuf;
			let file = dtd_file lexbuf in
			dtd_end_decl lexbuf;
			DTDFile file
		}
	| '['
		{
			ignore_spaces lexbuf;
			let data = dtd_intern lexbuf in
			dtd_end_decl lexbuf;
			DTDData data
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDDecl }

and dtd_file = parse
	| '"'
		{
			Buffer.reset tmp;
			let s = dq_string lexbuf in
			ignore_spaces lexbuf;
			s
		}
	| '\''
		{
			Buffer.reset tmp;
			let s = q_string lexbuf in
			ignore_spaces lexbuf;
			s
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDDecl }

and dtd_intern = parse
	| ']'
		{ 
			ignore_spaces lexbuf;
			[]
		}
	| ""
		{
			let l = dtd_item lexbuf in
			l @ (dtd_intern lexbuf)
		}

and dtd = parse
	| eof
		{ [] }
	| newline
		{
			newline lexbuf;
			dtd lexbuf
		}
	| (space | break)+
		{ dtd lexbuf }
	| ""
		{
			let l = dtd_item lexbuf in
			l @ (dtd lexbuf)
		}

and dtd_end_decl = parse
	| '>'
		{ ignore_spaces lexbuf }
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDDecl }

and dtd_item = parse
	| "<!--"
		{
			comment lexbuf;
			[];
		}
	| "<!"
		{
			ignore_spaces lexbuf;
			let t = dtd_item_type lexbuf in
			let name = (try ident_name lexbuf with Error EIdentExpected -> raise (DTDError EInvalidDTDDecl)) in
			ignore_spaces lexbuf;
			match t with
			| TElement -> [ DTDElement (name , (dtd_element_type lexbuf)) ]
			| TAttribute -> List.map (fun (attrname,atype,adef) -> DTDAttribute (name, attrname, atype, adef)) (dtd_attributes lexbuf)
		}
	| _ | eof
		{  dtd_error lexbuf EDTDItemExpected }

and dtd_attributes = parse
	| '>'
		{
			ignore_spaces lexbuf;
			[]
		}
	| ""
		{
			let attrname = (try ident_name lexbuf with Error EIdentExpected -> raise (DTDError EInvalidDTDAttribute)) in
			ignore_spaces lexbuf;
			let atype = dtd_attr_type lexbuf in
			let adef = dtd_attr_default lexbuf in
			let a = (attrname, atype, adef) in
			a :: (dtd_attributes lexbuf)
		}

and dtd_item_type = parse
	| "ELEMENT"
		{
			ignore_spaces lexbuf;
			TElement
		}
	| "ATTLIST"
		{
			ignore_spaces lexbuf;
			TAttribute
		} 
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDTag }

and dtd_element_type = parse
	| "ANY"
		{ 
			ignore_spaces lexbuf;
			dtd_end_element lexbuf;
			DTDAny
		}
	| "EMPTY"
		{ 
			ignore_spaces lexbuf;
			dtd_end_element lexbuf;
			DTDEmpty
		}
	| '('
		{
			try
				let item = Xml_parser.dtd_element dtd_element_token lexbuf in
				ignore_spaces lexbuf;
				DTDChild item
			with
				Parsing.Parse_error -> dtd_error lexbuf EInvalidDTDElement
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDElement }

and dtd_end_element = parse
	| '>' 
		{ ignore_spaces lexbuf }
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDElement }

and dtd_end_attribute = parse
	| '>' 
		{ ignore_spaces lexbuf }
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDAttribute }

and dtd_element_token = parse
	| newline
		{
			newline lexbuf;
			dtd_element_token lexbuf
		}
	| (space | break) +
		{ dtd_element_token lexbuf }
	| '('
		{ OPEN }
	| ')'
		{ CLOSE }
	| ','
		{ NEXT }
	| '>'
		{ END }
	| '|'
		{ OR }
	| "#PCDATA"
		{ PCDATA }
	| '*'
		{ STAR }
	| '+'
		{ PLUS }
	| '?'
		{ QUESTION }
	| identchar+
		{ IDENT (lexeme lexbuf) }
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDElement }

and dtd_attr_type = parse
	| "CDATA"
		{
			ignore_spaces lexbuf;
			DTDCData
		}
	| "NMTOKEN"
		{
			ignore_spaces lexbuf;
			DTDNMToken
		}
	| "ID"
		{
			ignore_spaces lexbuf;
		 	DTDID
		}
	| "IDREF"
		{
			ignore_spaces lexbuf;
			DTDIDRef
		}
	| '('
		{
			ignore_spaces lexbuf;
			DTDEnum (dtd_attr_enum lexbuf)
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDAttribute }
	
and dtd_attr_enum = parse
	| identchar+
		{
			let v = lexeme lexbuf in
			ignore_spaces lexbuf;
			v :: (dtd_attr_enum_next lexbuf)
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDAttribute }

and dtd_attr_enum_next = parse
	| ')'
		{
			ignore_spaces lexbuf;
			[]
		}
	| '|'
		{
			ignore_spaces lexbuf;
			dtd_attr_enum lexbuf
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDAttribute }

and dtd_attr_default = parse
	| '"'
		{
			Buffer.reset tmp;
			let v = (try dq_string lexbuf with Error EUnterminatedString -> raise (DTDError EInvalidDTDAttribute)) in
			ignore_spaces lexbuf;
			DTDDefault v
		}
	| '\''
		{
			Buffer.reset tmp;
			let v = (try q_string lexbuf with Error EUnterminatedString -> raise (DTDError EInvalidDTDAttribute)) in
			ignore_spaces lexbuf;
			DTDDefault v
		}
	| "#REQUIRED"
		{
			ignore_spaces lexbuf;
			DTDRequired
		}
	| "#IMPLIED"
		{
			ignore_spaces lexbuf;
			DTDImplied
		}
	| "#FIXED"
		{
			ignore_spaces lexbuf;
			DTDFixed (dtd_attr_string lexbuf)
		}
	| "#DEFAULT"
		{
			ignore_spaces lexbuf;
			DTDDefault (dtd_attr_string lexbuf)
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDAttribute }

and dtd_attr_string = parse
	| '"'
		{
			Buffer.reset tmp;
			let v = (try dq_string lexbuf with Error EUnterminatedString -> raise (DTDError EInvalidDTDAttribute)) in
			ignore_spaces lexbuf;
			v
		}
	| '\''
		{
			Buffer.reset tmp;
			let v = (try q_string lexbuf with Error EUnterminatedString -> raise (DTDError EInvalidDTDAttribute)) in
			ignore_spaces lexbuf;
			v
		}
	| _ | eof
		{ dtd_error lexbuf EInvalidDTDAttribute }
