(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

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
open Extlib_leftovers
open Globals
open EvalValue
open EvalContext
open EvalField
open EvalHash
open EvalString

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
let rhandle = create_ascii "#handle"

let s_date d =
	let open Unix in
	let t = localtime d in
	create_ascii (Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec)

let s_hash key = create_ascii (EvalHash.rev_hash key)

let rec indent buf s n =
	match n with
	| 0 -> ()
	| _ -> begin
		Buffer.add_string buf s;
		indent buf s (n - 1)
	end

let rec s_object depth indent_level o =
	let fields = object_fields o in
	let buf = Buffer.create 0 in
	let inner_indent_level = indent_level + 1 in

	Buffer.add_string buf "{";
	(match (get_ctx()).print_indentation with
		| None -> ()
		| Some s -> begin
			Buffer.add_string buf "\n";
			indent buf s inner_indent_level
		end);

	List.iteri (fun i (k,v) ->
		if i > 0 then begin
			match (get_ctx()).print_indentation with
				| None -> Buffer.add_string buf ", "
				| Some s -> begin
					Buffer.add_string buf ",\n";
					indent buf s inner_indent_level
				end;
		end;

		Buffer.add_string buf (rev_hash k);
		Buffer.add_string buf ": ";
		Buffer.add_string buf (s_value ~indent_level:inner_indent_level depth v).sstring;
	) fields;

	(match (get_ctx()).print_indentation with
		| None -> ()
		| Some s -> begin
			Buffer.add_string buf "\n";
			indent buf s indent_level
		end);

	Buffer.add_string buf "}";
	let s = Buffer.contents buf in
	create_with_length s (try UTF8.length s with _ -> String.length s)

and s_array depth indent_level va =
	join empty_string [
		rbkopen;
		EvalArray.join va (s_value ~indent_level depth) rcomma;
		rbkclose;
	]

and s_vector depth indent_level vv =
	join empty_string [
		rbkopen;
		EvalArray.join (EvalArray.create vv) (s_value ~indent_level depth) rcomma;
		rbkclose;
	]

and s_enum_ctor_name ve =
	try
		begin match (get_static_prototype_raise (get_ctx()) ve.epath).pkind with
			| PEnum names -> (try fst (List.nth names ve.eindex) with _ -> "#unknown")
			| _ -> raise Not_found
		end
	with Not_found -> "#unknown"

and s_enum_value depth indent_level ve =
	let name = s_enum_ctor_name ve in
	match ve.eargs with
	| [||] -> create_ascii name
	| vl ->
		join empty_string [
			create_ascii name;
			rpopen;
			join rcomma (Array.to_list (Array.map (s_value ~indent_level (depth + 1)) vl));
			rpclose;
		]

and s_proto_kind proto = match proto.pkind with
	| PClass _ -> join empty_string [create_ascii "Class<"; s_hash proto.ppath; rgt]
	| PEnum _ -> join empty_string [create_ascii "Enum<"; s_hash proto.ppath; rgt]
	| PInstance | PObject -> die "" __LOC__

and s_value ?(indent_level=0) depth v =
	let call_to_string () =
		let vf = field_raise v EvalHash.key_toString in
		s_value ~indent_level (depth + 1) (call_value_on v vf [])
	in
	if depth > (get_ctx()).max_print_depth then rstop
	else match v with
	| VNull -> rnull
	| VInt32 i32 -> create_ascii(Int32.to_string i32)
	| VInt64 i -> create_ascii(Signed.Int64.to_string i)
	| VUInt64 u -> create_ascii(Unsigned.UInt64.to_string u)
	| VTrue -> rtrue
	| VFalse -> rfalse
	| VFloat f ->
		let s = Numeric.float_repres f in
		let len = String.length s in
		create_ascii (if String.unsafe_get s (len - 1) = '.' then String.sub s 0 (len - 1) else s)
	| VFunction (f,_) -> rfun
	| VFieldClosure _ -> rclosure
	| VHandle _ -> rhandle
	| VEnumValue ve -> s_enum_value depth indent_level ve
	| VString s -> s
	| VNativeString s -> create_unknown_vstring s
	| VArray va -> s_array (depth + 1) indent_level va
	| VVector vv -> s_vector (depth + 1) indent_level vv
	| VInstance {ikind=IDate d} -> s_date d
	| VInstance {ikind=IPos p} -> create_ascii ("#pos(" ^ Lexer.get_error_pos (Printf.sprintf "%s:%d:") p ^ ")") (* STODO: not ascii? *)
	| VInstance {ikind=IRegex r} -> r.r_rex_string
	| VInstance i -> (try call_to_string () with Not_found -> s_hash i.iproto.ppath)
	| VObject o -> (try call_to_string () with Not_found -> s_object (depth + 1) indent_level o)
	| VLazy f -> s_value ~indent_level depth (!f())
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
	(s_value 0 v).sstring
