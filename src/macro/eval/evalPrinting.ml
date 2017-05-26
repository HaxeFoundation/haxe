(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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

open Rope

let rnull = of_string "null"
let rcomma = of_char ','
let rtrue = of_string "true"
let rfalse = of_string "false"
let rfun = of_string "#fun"
let rclosure = of_string "#closure"

let s_date d =
	let open Unix in
	let t = localtime d in
	of_string (Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec)

let rec s_object depth o =
	let fields = object_fields o in
	let fields = List.map (fun (key,value) -> (concat empty [EvalHash.rev_hash key; of_string ": "; s_value depth value])) fields in
	concat empty [
		of_char '{';
		concat rcomma fields;
		of_char '}'
	]

and s_array depth va =
	concat empty [
		of_char '[';
		EvalArray.join va (s_value 0) rcomma;
		of_char ']';
	]

and s_vector depth vv =
	concat empty [
		of_char '[';
		EvalArray.join (EvalArray.create vv) (s_value 0) rcomma;
		of_char ']';
	]

and s_enum_ctor_name ve =
	try
		begin match (get_static_prototype_raise (get_ctx()) ve.epath).pkind with
			| PEnum names -> (try List.nth names ve.eindex with _ -> "#unknown")
			| _ -> raise Not_found
		end
	with Not_found -> "#unknown"

and s_enum_value depth ve =
	let name = s_enum_ctor_name ve in
	match ve.eargs with
	| [||] -> of_string name
	| vl ->
		concat empty [
			of_string name;
			of_char '(';
			concat rcomma (Array.to_list (Array.map (s_value (depth + 1)) vl));
			of_char ')'
		]

and s_proto_kind proto = match proto.pkind with
	| PClass _ -> concat empty [of_string "Class<"; rev_hash proto.ppath; of_char '>']
	| PEnum _ -> concat empty [of_string "Enum<"; rev_hash proto.ppath; of_char '>']
	| PInstance | PObject -> assert false

and s_value depth v =
	let call_to_string () =
		let vf = field_raise v EvalHash.key_toString in
		s_value (depth + 1) (call_value_on v vf [])
	in
	if depth > 5 then of_string "<...>"
	else match v with
	| VNull -> rnull
	| VInt32 i32 -> of_string (Int32.to_string i32)
	| VTrue -> rtrue
	| VFalse -> rfalse
	| VFloat f ->
		let s = Common.float_repres f in
		let len = String.length s in
		of_string (if String.unsafe_get s (len - 1) = '.' then String.sub s 0 (len - 1) else s)
	| VFunction (f,_) ->
		let s = match num_args f with
			| -1 -> ""
			| i -> string_of_int i
		in
		concat2 rfun (Rope.of_string (s))
	| VFieldClosure _ -> rclosure
	| VEnumValue ve -> s_enum_value depth ve
	| VString(s,_) -> s
	| VArray va -> s_array (depth + 1) va
	| VVector vv -> s_vector (depth + 1) vv
	| VInstance {ikind=IDate d} -> s_date d
	| VInstance {ikind=IPos p} -> of_string ("#pos(" ^ Lexer.get_error_pos (Printf.sprintf "%s:%d:") p ^ ")")
	| VInstance i -> (try call_to_string () with Not_found -> rev_hash i.iproto.ppath)
	| VObject o -> (try call_to_string () with Not_found -> s_object (depth + 1) o)
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

and value_string v = Rope.to_string (s_value 0 v)