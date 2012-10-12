(*
 *  Neko NXML for OCaml
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Nast

type xml =
	| Node of string * (string * string) list * xml list
	| CData of string

let node name att childs = Node(name,att,childs)

let rec to_xml_rec p2 ast =
	let e , p = ast in
	let name = ref "" in
	let aval = ref None in
	let childs = ref [] in
	(match e with
	| EConst c ->
		(match c with
		| True
		| False
		| Null
		| This
		| Builtin _
		| Ident _ ->
			name := "v";
			aval := Some (s_constant c)
		| Int i ->
			name := "i";
			aval := Some (string_of_int i);
		| Float s ->
			name := "f";
			aval := Some s;
		| String s ->
			name := "s";
			aval := Some s;
		| Int32 i ->
			name := "i";
			aval := Some (Int32.to_string i);
		)
	| EBlock el ->
		name := "b";
		childs := List.map (to_xml_rec p) el; 
	| EParenthesis e ->
		name := "p";
		childs := [to_xml_rec p e];
	| EField (e,f) ->
		name := "g";
		aval := Some f;
		childs := [to_xml_rec p e];
	| ECall (e,el) ->
		name := "c";
		childs := to_xml_rec p e :: List.map (to_xml_rec p) el;
	| EArray (a,b) ->
		name := "a";
		childs := [to_xml_rec p a; to_xml_rec p b]; 
	| EVars vl ->
		name := "var";
		childs := List.map (fun(v,e) ->
			node "v" [("v",v)] (match e with None -> [] | Some e -> [to_xml_rec p e])
		) vl;
	| EWhile (econd,e,NormalWhile) ->
		name := "while";
		childs := [to_xml_rec p econd; to_xml_rec p e];
	| EWhile (econd,e,DoWhile) ->
		name := "do";
		childs := [to_xml_rec p e; to_xml_rec p econd];
	| EIf (cond,e,eelse) ->
		name := "if";
		childs := to_xml_rec p cond :: to_xml_rec p e :: (match eelse with None -> [] | Some e -> [to_xml_rec p e])
	| ETry (e1,v,e2) ->
		name := "try";
		aval := Some v;
		childs := [to_xml_rec p e1; to_xml_rec p e2];
	| EFunction (args,e) ->
		name := "function";
		aval := Some (String.concat ":" args);
		childs := [to_xml_rec p e];
	| EBinop (op,e1,e2) ->
		name := "o";
		aval := Some op;
		childs := [to_xml_rec p e1; to_xml_rec p e2];
	| EReturn e ->
		name := "return";
		childs := (match e with None -> [] | Some e -> [to_xml_rec p e]);
	| EBreak e ->
		name := "break";
		childs := (match e with None -> [] | Some e -> [to_xml_rec p e]);
	| EContinue ->
		name := "continue";
	| ENext (e1,e2) ->
		name := "next";
		childs := [to_xml_rec p e1; to_xml_rec p e2];
	| EObject fl ->
		name := "object";
		childs := List.map (fun(v,e) -> node "v" [("v",v)] [to_xml_rec p e]) fl;
	| ELabel v ->
		name := "label";
		aval := Some v;
	| ESwitch (e,cases,def) ->
		name := "switch";
		let cases = List.map (fun(e1,e2) -> node "case" [] [to_xml_rec p e1; to_xml_rec p e2]) cases in
		childs := to_xml_rec p e :: (match def with None -> cases | Some e -> node "default" [] [to_xml_rec p e] :: cases );
	| ENeko s ->
		name := "neko";
		childs := [CData s];
	);
	let pos = (if p.psource <> p2.psource then
		[("p",p.psource ^ ":" ^ string_of_int p.pline)]
	else if p.pline <> p2.pline then
		[("p",string_of_int p.pline)]
	else
		[]
	) in
	let aval = (match !aval with None -> [] | Some v -> [("v",v)]) in
	node !name (List.append pos aval) !childs

let to_xml ast =
	to_xml_rec null_pos ast

let rec write_fmt_rec tabs ch x =
	match x with
	| CData s ->
		IO.printf ch "%s<![CDATA[%s]]>" tabs s
	| Node (name,att,childs) ->
		IO.printf ch "%s<%s%s" tabs name (String.concat "" (List.map (fun(a,v) -> " " ^ a ^ "=\"" ^ escape v ^ "\"") att));
		match childs with
		| [] -> IO.nwrite ch "/>"
		| l ->
			IO.nwrite ch ">\n";
			List.iter (fun(x) -> write_fmt_rec (tabs ^ " ") ch x; IO.write ch '\n') l;
			IO.printf ch "%s</%s>" tabs name

let write_fmt ch x =
	write_fmt_rec "" ch (node "nxml" [] [x])

let rec write_rec ch x =
	match x with
	| CData s ->
		IO.printf ch "<![CDATA[%s]]>" s
	| Node (name,att,childs) ->
		IO.printf ch "<%s%s" name (String.concat "" (List.map (fun(a,v) -> " " ^ a ^ "=\"" ^ escape v ^ "\"") att));
		match childs with
		| [] -> IO.nwrite ch "/>"
		| l ->
			IO.nwrite ch ">";
			List.iter (fun(x) -> write_rec ch x) l;
			IO.printf ch "</%s>" name

let write ch x =
	write_rec ch (node "nxml" [] [x])
