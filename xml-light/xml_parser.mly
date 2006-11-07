%{(*
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
%}
%token NEXT OR
%token <string>IDENT
%token PCDATA
%token STAR QUESTION PLUS
%token OPEN CLOSE
%token END

%right STAR QUESTION PLUS

%start dtd_element
%type <Dtd.dtd_child> dtd_element
%%

dtd_element:
	| dtd_full_seq END
		{ $1 }
;
dtd_full_seq:
	| dtd_seq CLOSE dtd_op
		{ $3 $1 }
	| dtd_seq CLOSE
		{ $1 }
;
dtd_seq:
	| dtd_item NEXT dtd_children
		{ Dtd.DTDChildren ($1 :: $3) }
	| dtd_item OR dtd_choice
		{ Dtd.DTDChoice ($1 :: $3) }
	| dtd_item
		{ $1 }
;
dtd_children:
	| dtd_item NEXT dtd_children
		{ $1 :: $3 }
	| dtd_item
		{ [$1] }
;
dtd_choice:
	| dtd_item OR dtd_choice
		{ $1 :: $3 }
	| dtd_item
		{ [$1] }
;
dtd_item:
	| OPEN dtd_full_seq
		{ $2 }
	| dtd_member
		{ $1 }
;
dtd_member:
	| IDENT dtd_op
		{ $2 (Dtd.DTDTag $1) }
	| PCDATA dtd_op
		{ $2 Dtd.DTDPCData }
	| IDENT
		{ Dtd.DTDTag $1 }
	| PCDATA
		{ Dtd.DTDPCData }
;
dtd_op:
	| dtd_op_item dtd_op
		{ (fun x -> $2 ($1 x)) }
	| dtd_op_item 
		{ $1 }
;
dtd_op_item:
	| STAR
		{ (fun x -> Dtd.DTDZeroOrMore x) }
	| QUESTION
		{ (fun x -> Dtd.DTDOptional x) }
	| PLUS
		{ (fun x -> Dtd.DTDOneOrMore x) }
;