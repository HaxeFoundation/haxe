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
open Ast
open Type
open EvalValue
open EvalContext
open EvalEncode
open EvalDecode
open EvalExceptions
open EvalPrinting
open EvalHash

let throw_string s p =
	throw (encode_string s) p

let invalid_binop op v1 v2 p =
	throw_string (Printf.sprintf "Invalid operation: %s %s %s" (value_string v1) (s_binop op) (value_string v2)) p

(* Calls *)

let call_value v vl =
	match v with
	| VFunction(f,_) ->	 call_function f vl
	| VFieldClosure(v1,f) -> call_function f (v1 :: vl)
	| VInstance {ikind = ILazyType(_,get)} -> get()
	| _ -> exc_string ("Cannot call " ^ (value_string v))

(* Field setters *)

let set_proto_field proto name v2 =
	proto.pfields.(get_proto_field_index_raise proto name) <- v2

let set_instance_field vi name v2 =
	vi.ifields.(get_instance_field_index_raise vi.iproto name) <- v2

let set_object_field o name v2 =
	try
		o.ofields.(get_instance_field_index_raise o.oproto name) <- v2;
		o.oremoved <- IntMap.remove name o.oremoved;
	with Not_found ->
		o.oextra <- IntMap.add name v2 o.oextra

let set_bytes_length_field v1 v2 =
	match v1 with
	| VInstance ({ikind=IBytes b} as vi) ->
		let i = decode_int v2 in
		let b' = Bytes.create i in
		Bytes.blit b 0 b' 0 (if i > Bytes.length b then Bytes.length b else i);
		vi.ikind <- IBytes b'
	| _ -> unexpected_value v1 "bytes"

let set_field v1 name v2 = match vresolve v1 with
	| VObject o -> set_object_field o name v2
	| VPrototype proto -> set_proto_field proto name v2
	| VArray va ->
		(* Vector.new does this *)
		if name = key_length then begin
			EvalArray.set_length va (decode_int v2);
		end else
			unexpected_value v1 "object"
	| VInstance {ikind = IBytes _} -> set_bytes_length_field v1 v2
	| VInstance vi -> set_instance_field vi name v2
	| _ -> unexpected_value v1 "object"

(* Equality/compare *)

let fcmp (a:float) b = if a = b then CEq else if a < b then CInf else if a > b then CSup else CUndef

let icmp (a:int32) b = let l = Int32.compare a b in if l = 0 then CEq else if l < 0 then CInf else CSup

let rec compare a b =
	match a, b with
	| VNull,VNull -> CEq
	| VInt32 a,VInt32 b -> icmp a b
	| VFloat a,VFloat b -> fcmp a b
	| VFloat a,VInt32 b -> fcmp a (Int32.to_float b)
	| VInt32 a,VFloat b -> fcmp (Int32.to_float a) b
	| VTrue,VTrue | VFalse,VFalse -> CEq
	| VFalse,VTrue -> CInf
	| VTrue,VFalse -> CSup
	| VString(_,s1),VString(_,s2) ->
		let r = String.compare (Lazy.force s1) (Lazy.force s2) in
		if r = 0 then CEq else if r < 0 then CInf else CSup
	| VFunction(a,_), VFunction(b,_) -> if a == b then CEq else CUndef
	| VArray va1,VArray va2 -> if va1 == va2 then CEq else CUndef
	| VVector vv1,VVector vv2 -> if vv1 == vv2 then CEq else CUndef
	| VObject a,VObject b -> if a == b then CEq else CUndef
	| VInstance a,VInstance b -> if a == b then CEq else CUndef
	| VPrototype a,VPrototype b -> if a == b then CEq else CUndef
	| VEnumValue a,VEnumValue b ->
		if a == b then CEq
		else if a.eindex < b.eindex then CInf
		else if a.eindex > b.eindex then CSup
		else if a.epath <> b.epath then CUndef
		else if Array.length a.eargs = 0 && Array.length b.eargs = 0 then CEq
		else CUndef
	| VFieldClosure(v1,f1),VFieldClosure(v2,f2) ->
		if f1 != f2 then CUndef
		else compare v1 v2
	| VLazy f1,_ ->
		compare (!f1()) b
	| _,VLazy f2 ->
		compare a (!f2())
	| _ -> CUndef

let rec arrays_equal cmp a1 a2 =
	if Array.length a1 <> Array.length a2 then
		false
	else begin
		let rec loop i =
			if i = Array.length a1 then true
			else if not (cmp a1.(i) a2.(i)) then false
			else loop (i + 1)
		in
		loop 0
	end

and equals_structurally a b =
	match a,b with
	| VInt32 a,VInt32 b -> Int32.compare a b = 0
	| VFloat a,VFloat b -> a = b
	| VFloat a,VInt32 b -> a = (Int32.to_float b)
	| VInt32 a,VFloat b -> (Int32.to_float a) = b
	| VString(_,s1),VString(_,s2) -> Lazy.force s1 = Lazy.force s2
	| VArray a,VArray b -> a == b || arrays_equal equals_structurally a.avalues b.avalues
	| VVector a,VVector b -> a == b || arrays_equal equals_structurally a b
	| VObject a,VObject b -> a == b || arrays_equal equals_structurally a.ofields b.ofields && IntMap.equal equals_structurally a.oextra b.oextra
	| VEnumValue a,VEnumValue b -> a == b || a.eindex = b.eindex && arrays_equal equals_structurally a.eargs b.eargs && a.epath = b.epath
	| VPrototype proto1,VPrototype proto2 -> proto1.ppath = proto2.ppath
	| VLazy f1,_ -> equals_structurally (!f1()) b
	| _,VLazy f2 -> equals_structurally a (!f2())
	| _ -> a == b

let is_true v = match v with
	| VTrue -> true
	| _ -> false

let op_add v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.add i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (f1 +. f2)
	| VInt32 i,VFloat f | VFloat f,VInt32 i -> vfloat ((Int32.to_float i) +. f)
	| VString(s1,_),VString(s2,_) -> encode_rope (Rope.concat2 s1 s2)
	| VString(s1,_),v2 -> encode_rope (Rope.concat2 s1 (s_value 0 v2))
	| v1,VString(s2,_) -> encode_rope (Rope.concat2 (s_value 0 v1) s2)
	| v1,v2 -> encode_rope (Rope.concat2 (s_value 0 v1) (s_value 0 v2))

let op_mult p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.mul i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (f1 *. f2)
	| VInt32 i,VFloat f | VFloat f,VInt32 i -> vfloat ((Int32.to_float i) *. f)
	| _ -> invalid_binop OpMult v1 v2 p

let op_div p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vfloat ((Int32.to_float i1) /. (Int32.to_float i2))
	| VFloat f1,VFloat f2 -> vfloat (f1 /. f2)
	| VInt32 i1,VFloat f2 -> vfloat ((Int32.to_float i1) /. f2)
	| VFloat f1,VInt32 i2 -> vfloat (f1 /. (Int32.to_float i2))
	| _ -> invalid_binop OpDiv v1 v2 p

let op_sub p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.sub i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (f1 -. f2)
	| VInt32 i1,VFloat f2 -> vfloat ((Int32.to_float i1) -. f2)
	| VFloat f1,VInt32 i2 -> vfloat (f1 -. (Int32.to_float i2))
	| _ -> invalid_binop OpSub v1 v2 p

let op_eq v1 v2 = vbool (equals v1 v2)

let op_not_eq v1 v2 = vbool (not (equals v1 v2))

let op_gt v1 v2 = vbool (compare v1 v2 = CSup)

let op_gte v1 v2 = vbool (match compare v1 v2 with CSup | CEq -> true | _ -> false)

let op_lt v1 v2 = vbool (compare v1 v2 = CInf)

let op_lte v1 v2 = vbool (match compare v1 v2 with CInf | CEq -> true | _ -> false)

let op_and p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.logand i1 i2)
	| _ -> invalid_binop OpAnd v1 v2 p

let op_or p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.logor i1 i2)
	| _ -> invalid_binop OpOr v1 v2 p

let op_xor p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.logxor i1 i2)
	| _ -> invalid_binop OpXor v1 v2 p

let op_shl p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.shift_left i1 (Int32.to_int i2))
	| _ -> invalid_binop OpShl v1 v2 p

let op_shr p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.shift_right i1 (Int32.to_int i2))
	| _ -> invalid_binop OpShr v1 v2 p

let op_ushr p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.shift_right_logical i1 (Int32.to_int i2))
	| _ -> invalid_binop OpUShr v1 v2 p

let op_mod p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.rem i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (mod_float f1 f2)
	| VInt32 i1,VFloat f2 -> vfloat (mod_float (Int32.to_float i1) f2)
	| VFloat f1,VInt32 i2 -> vfloat (mod_float f1 (Int32.to_float i2))
	| _ -> invalid_binop OpMod v1 v2 p

let get_binop_fun op p = match op with
	| OpAdd -> op_add
	| OpMult -> op_mult p
	| OpDiv -> op_div p
	| OpSub -> op_sub p
	| OpEq -> op_eq
	| OpNotEq -> op_not_eq
	| OpGt -> op_gt
	| OpGte -> op_gte
	| OpLt -> op_lt
	| OpLte -> op_lte
	| OpAnd -> op_and p
	| OpOr -> op_or p
	| OpXor -> op_xor p
	| OpShl -> op_shl p
	| OpShr -> op_shr p
	| OpUShr -> op_ushr p
	| OpMod -> op_mod p
	| OpAssign | OpBoolAnd | OpBoolOr | OpAssignOp _ | OpInterval | OpArrow | OpIn -> assert false
