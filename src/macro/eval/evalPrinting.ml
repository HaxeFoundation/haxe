(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Globals
open Type
open EvalValue
open EvalContext
open EvalField
open EvalHash
open EvalString

let rempty = create_ascii ""
let rbropen = create_ascii "{"
let rbrclose = create_ascii "}"
let rbkopen = create_ascii "["
let rbkclose = create_ascii "]"
let rpopen = create_ascii "("
let rpclose = create_ascii ")"
let rcolon = create_ascii ":"
let rgt = create_ascii ">"
let rstop = create_ascii "<...>"
let rnull = create_ascii "null"
let rcomma = create_ascii ","
let rtrue = create_ascii "true"
let rfalse = create_ascii "false"
let rfun = create_ascii "#fun"
let rclosure = create_ascii "#closure"

let s_date d =
	let open Unix in
	let t = localtime d in
	create_ascii (Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec)

let s_hash key = create_ascii (EvalHash.rev_hash key)

let rec s_object depth o =
	let fields = object_fields o in
	let s,_ = List.fold_left (fun (s,sep) (key,value) ->
		let s = concat s sep in
		let s = concat s (s_hash key) in
		let s = concat s rcolon in
		let s = concat s (s_value depth value) in
		(s,rcomma)
	) (rempty,rbropen) fields in
	concat s rbrclose

and s_array depth va =
	join rempty [
		rbkopen;
		EvalArray.join va (s_value depth) rcomma;
		rbkclose;
	]

and s_vector depth vv =
	join rempty [
		rbkopen;
		EvalArray.join (EvalArray.create vv) (s_value depth) rcomma;
		rbkclose;
	]

and s_enum_ctor_name ve =
	try
		begin match (get_static_prototype_raise (get_ctx()) ve.epath).pkind with
			| PEnum names -> (try fst (List.nth names ve.eindex) with _ -> "#unknown")
			| _ -> raise Not_found
		end
	with Not_found -> "#unknown"

and s_enum_value depth ve =
	let name = s_enum_ctor_name ve in
	match ve.eargs with
	| [||] -> create_ascii name
	| vl ->
		join rempty [
			create_ascii name;
			rpopen;
			join rcomma (Array.to_list (Array.map (s_value (depth + 1)) vl));
			rpclose;
		]

and s_proto_kind proto = match proto.pkind with
	| PClass _ -> join rempty [create_ascii "Class<"; s_hash proto.ppath; rgt]
	| PEnum _ -> join rempty [create_ascii "Enum<"; s_hash proto.ppath; rgt]
	| PInstance | PObject -> assert false

and s_value depth v =
	let call_to_string () =
		let vf = field_raise v EvalHash.key_toString in
		s_value (depth + 1) (call_value_on v vf [])
	in
	if depth > 5 then rstop
	else match v with
	| VNull -> rnull
	| VInt32 i32 -> create_ascii(Int32.to_string i32)
	| VTrue -> rtrue
	| VFalse -> rfalse
	| VFloat f ->
		let s = Numeric.float_repres f in
		let len = String.length s in
		create_ascii (if String.unsafe_get s (len - 1) = '.' then String.sub s 0 (len - 1) else s)
	| VFunction (f,_) -> rfun
	| VFieldClosure _ -> rclosure
	| VEnumValue ve -> s_enum_value depth ve
	| VString s -> s
	| VArray va -> s_array (depth + 1) va
	| VVector vv -> s_vector (depth + 1) vv
	| VInstance {ikind=IDate d} -> s_date d
	| VInstance {ikind=IPos p} -> create_ascii ("#pos(" ^ Lexer.get_error_pos (Printf.sprintf "%s:%d:") p ^ ")") (* STODO: not ascii? *)
	| VInstance i -> (try call_to_string () with Not_found -> s_hash i.iproto.ppath)
	| VObject o -> (try call_to_string () with Not_found -> s_object (depth + 1) o)
	| VLazy f -> s_value depth (!f())
	| VPrototype proto ->
		try
			call_to_string()
		with Not_found ->
			s_proto_kind proto

and call_value_on vthis v vl =
	match v with
	| VFunction(f,b) ->
		let vl = if not b then vthis :: vl else vl in
		call_function f vl
	| VFieldClosure(v1,f) -> call_function f (v1 :: vl)
	| _ -> exc_string ("Cannot call " ^ (value_string v))

and value_string v =
	let s = s_value 0 v in
	EvalString.get s