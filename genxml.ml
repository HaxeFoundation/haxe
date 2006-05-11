(*
 *  Haxe Compiler
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
open Ast
open Type

type xml = 
	| Node of string * (string * string) list * xml list
	| PCData of string

let tag name = Node (name,[],[])
let xml name att = Node (name,att,[])
let node name att childs = Node (name,att,childs)
let pcdata s = PCData s

let pmap f m = 
	PMap.fold (fun x acc -> f x :: acc) m []

let gen_path (p,n) =
	("path",String.concat "." (p @ [n]))

let gen_doc s = node "haxe:doc" [] [pcdata s]

let gen_doc_opt d =
	match d with 
	| None -> []
	| Some s -> [gen_doc s]

let rec gen_type t =
	match t with
	| TMono m -> (match !m with None -> tag "unknown" | Some t -> gen_type t)
	| TEnum (e,params) -> node "e" [gen_path e.e_path] (List.map gen_type params)
	| TInst (c,params) -> node "c" [gen_path c.cl_path] (List.map gen_type params)
	| TFun (args,r) -> node "f" ["a",String.concat ":" (List.map fst args)] (List.map gen_type (List.map snd args @ [r]))
	| TAnon (fields,_) -> node "a" [] (pmap (fun f -> node f.cf_name [] [gen_type f.cf_type]) fields)
	| TDynamic t2 -> node "d" [] (if t == t2 then [] else [gen_type t2])
	| TLazy f -> gen_type (!f())

let gen_constr e =
	let doc = gen_doc_opt e.ef_doc in
	let args, t = (match follow e.ef_type with 
		| TFun (args,_) ->
			["a",String.concat ":" (List.map fst args)] ,
			List.map (fun (_,t) -> gen_type t) args @ doc
		| _ -> 
			[] , doc
	) in
	node e.ef_name args t 

let gen_field att f =
	let att = (match f.cf_expr with None -> att | Some e -> ("line",string_of_int (Lexer.get_error_line e.epos)) :: att) in
	node f.cf_name (if f.cf_public then ("public","1") :: att else att) (gen_type f.cf_type :: gen_doc_opt f.cf_doc)

let gen_type_params priv path params pos =
	let priv = (if priv then [("private","1")] else []) in
	gen_path path :: ("params", String.concat ":" (List.map fst params)) :: ("file",pos.pfile) :: priv

let gen_type t =
	match t with
	| TClassDecl c -> 
		let stats = pmap (gen_field ["static","1"]) c.cl_statics in
		let fields = pmap (gen_field []) c.cl_fields in
		let constr = (match c.cl_constructor with None -> [] | Some f -> [gen_field [] f]) in
		let doc = gen_doc_opt c.cl_doc in
		node "class" (gen_type_params c.cl_private c.cl_path c.cl_types c.cl_pos) (stats @ fields @ constr @ doc)
	| TEnumDecl e ->
		let doc = gen_doc_opt e.e_doc in
		node "enum" (gen_type_params e.e_private e.e_path e.e_types e.e_pos) (pmap gen_constr e.e_constrs @ doc)
	| TSignatureDecl s ->
		let doc = gen_doc_opt s.s_doc in
		let fields = pmap (gen_field []) s.s_fields in
		node "signature" (gen_type_params false s.s_path [] s.s_pos) (fields @ doc)

let att_str att = 
	String.concat "" (List.map (fun (a,v) -> Printf.sprintf " %s=\"%s\"" a v) att)

let rec write_xml ch tabs x =
	match x with
	| Node (name,att,[]) -> 
		IO.printf ch "%s<%s%s/>" tabs name (att_str att)
	| Node (name,att,[PCData s]) -> 
		IO.printf ch "%s<%s%s>%s</%s>" tabs name (att_str att) s name
	| Node (name,att,childs) ->
		IO.printf ch "%s<%s%s>\n" tabs name (att_str att);
		List.iter (fun x ->
			write_xml ch (tabs ^ "\t") x;
			IO.printf ch "\n";
		) childs;
		IO.printf ch "%s</%s>" tabs name
	| PCData s ->
		assert false

let generate file types =
	let x = node "haxe" [] (List.map gen_type types) in
	let ch = IO.output_channel (open_out file) in
	write_xml ch "" x;
	IO.close_out ch
