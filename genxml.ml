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
open Common

type xml =
	| Node of string * (string * string) list * xml list
	| PCData of string
	| CData of string

let tag name = Node (name,[],[])
let xml name att = Node (name,att,[])
let node name att childs = Node (name,att,childs)
let pcdata s = PCData s
let cdata s = CData s

let pmap f m =
	PMap.fold (fun x acc -> f x :: acc) m []

let gen_path (p,n) priv =
	("path",String.concat "." (p @ [n]))

let gen_doc s =
	let f = if String.contains s '<' || String.contains s '>' || String.contains s '&' then	cdata else pcdata in
	node "haxe_doc" [] [f s]

let gen_doc_opt d =
	match d with
	| None -> []
	| Some s -> [gen_doc s]

let gen_arg_name (name,opt,_) =
	(if opt then "?" else "") ^ name

let rec follow_param t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow_param t
		| _ -> t)
	| TType ({ t_path = [],"Null" } as t,tl) ->
		follow_param (apply_params t.t_types tl t.t_type)
	| _ ->
		t

let rec gen_type t =
	match t with
	| TMono m -> (match !m with None -> tag "unknown" | Some t -> gen_type t)
	| TEnum (e,params) -> node "e" [gen_path e.e_path e.e_private] (List.map gen_type params)
	| TInst (c,params) -> node "c" [gen_path c.cl_path c.cl_private] (List.map gen_type params)
	| TType (t,params) -> node "t" [gen_path t.t_path t.t_private] (List.map gen_type params)
	| TFun (args,r) -> node "f" ["a",String.concat ":" (List.map gen_arg_name args)] (List.map gen_type (List.map (fun (_,opt,t) -> if opt then follow_param t else t) args @ [r]))
	| TAnon a -> node "a" [] (pmap (fun f -> node f.cf_name [] [gen_type f.cf_type]) a.a_fields)
	| TDynamic t2 -> node "d" [] (if t == t2 then [] else [gen_type t2])
	| TLazy f -> gen_type (!f())

let gen_constr e =
	let doc = gen_doc_opt e.ef_doc in
	let args, t = (match follow e.ef_type with
		| TFun (args,_) ->
			["a",String.concat ":" (List.map gen_arg_name args)] ,
			List.map (fun (_,opt,t) -> gen_type (if opt then follow_param t else t)) args @ doc
		| _ ->
			[] , doc
	) in
	node e.ef_name args t

let gen_field att f =
	let add_get_set acc name att =
		match acc with
		| NormalAccess | ResolveAccess -> att
		| NoAccess | NeverAccess -> (name, "null") :: att
		| MethodAccess m -> (name, if m = name ^ "_" ^ f.cf_name then "dynamic" else m) :: att
		| MethodCantAccess -> att
		| InlineAccess -> assert false
	in
	let att = (match f.cf_expr with None -> att | Some e -> ("line",string_of_int (Lexer.get_error_line e.epos)) :: att) in
	let att = (match f.cf_get with
		| InlineAccess -> att
		| g -> add_get_set f.cf_get "get" (add_get_set f.cf_set "set" att)
	) in
	let att = (match f.cf_params with [] -> att | l -> ("params", String.concat ":" (List.map (fun (n,_) -> n) l)) :: att) in
	node f.cf_name (if f.cf_public then ("public","1") :: att else att) (gen_type f.cf_type :: gen_doc_opt f.cf_doc)

let gen_type_params priv path params pos m =
	let mpriv = (if priv then [("private","1")] else []) in
	let mpath = (if m.mpath <> path then [("module",snd (gen_path m.mpath false))] else []) in
	gen_path path priv :: ("params", String.concat ":" (List.map fst params)) :: ("file",if pos == null_pos then "" else pos.pfile) :: (mpriv @ mpath)

let gen_class_path name (c,pl) =
	node name [("path",s_type_path c.cl_path)] (List.map gen_type pl)

let rec exists f c =
	try
		let f2 = PMap.find f.cf_name c.cl_fields in
		not (type_iseq f.cf_type f2.cf_type)
	with
		Not_found ->
			match c.cl_super with
			| None -> true
			| Some (csup,_) -> exists f csup

let gen_type_decl com t =
	let m = com.type_api.get_type_module t in
	match t with
	| TClassDecl c ->
		let stats = List.map (gen_field ["static","1"]) c.cl_ordered_statics in
		let fields = (match c.cl_super with
			| None -> c.cl_ordered_fields
			| Some (csup,_) -> List.filter (fun f -> exists f csup) c.cl_ordered_fields
		) in
		let fields = List.map (gen_field []) fields in
		let constr = (match c.cl_constructor with None -> [] | Some f -> [gen_field [] f]) in
		let impl = List.map (gen_class_path "implements") c.cl_implements in
		let tree = (match c.cl_super with
			| None -> impl
			| Some x -> gen_class_path "extends" x :: impl
		) in
		let doc = gen_doc_opt c.cl_doc in
		let ext = (if c.cl_extern then [("extern","1")] else []) in
		let interf = (if c.cl_interface then [("interface","1")] else []) in
		let dynamic = (match c.cl_dynamic with
			| None -> []
			| Some t -> [node "haxe_dynamic" [] [gen_type t]]
		) in
		node "class" (gen_type_params c.cl_private c.cl_path c.cl_types c.cl_pos m @ ext @ interf) (tree @ stats @ fields @ constr @ doc @ dynamic)
	| TEnumDecl e ->
		let doc = gen_doc_opt e.e_doc in
		node "enum" (gen_type_params e.e_private e.e_path e.e_types e.e_pos m) (pmap gen_constr e.e_constrs @ doc)
	| TTypeDecl t ->
		let doc = gen_doc_opt t.t_doc in
		let tt = gen_type t.t_type in
		node "typedef" (gen_type_params t.t_private t.t_path t.t_types t.t_pos m) (tt :: doc)

let att_str att =
	String.concat "" (List.map (fun (a,v) -> Printf.sprintf " %s=\"%s\"" a v) att)

let rec write_xml ch tabs x =
	match x with
	| Node (name,att,[]) ->
		IO.printf ch "%s<%s%s/>" tabs name (att_str att)
	| Node (name,att,[x]) ->
		IO.printf ch "%s<%s%s>" tabs name (att_str att);
		write_xml ch "" x;
		IO.printf ch "</%s>" name;
	| Node (name,att,childs) ->
		IO.printf ch "%s<%s%s>\n" tabs name (att_str att);
		List.iter (fun x ->
			write_xml ch (tabs ^ "\t") x;
			IO.printf ch "\n";
		) childs;
		IO.printf ch "%s</%s>" tabs name
	| PCData s ->
		IO.printf ch "%s" s
	| CData s ->
		IO.printf ch "<![CDATA[%s]]>" s

let generate com file =
	let t = Common.timer "construct xml" in
	let x = node "haxe" [] (List.map (gen_type_decl com) com.types) in
	t();
	let t = Common.timer "write xml" in
	let ch = IO.output_channel (open_out_bin file) in
	write_xml ch "" x;
	IO.close_out ch;
	t()

let gen_type_string ctx t =
	let x = gen_type_decl ctx t in
	let ch = IO.output_string() in
	write_xml ch "" x;
	IO.close_out ch

