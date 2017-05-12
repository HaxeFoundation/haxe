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
open EvalEncode
open EvalDecode
open EvalExceptions
open EvalPrinting
open EvalHash

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

let set_field v1 name v2 = match v1 with
	| VObject o -> set_object_field o name v2
	| VPrototype proto -> set_proto_field proto name v2
	| VArray va ->
		(* Vector.new does this *)
		if name = key_length then begin
			EvalArray.set_length va (decode_int v2);
		end else
			unexpected_value v1 "object"
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
	| VObject a,VObject b -> if a == b then CEq else CUndef
	| VInstance a,VInstance b -> if a == b then CEq else CUndef
	| VPrototype a,VPrototype b -> if a == b then CEq else CUndef
	| VEnumValue a,VEnumValue b ->
		if a == b then CEq
		else if a.eindex < b.eindex then CInf
		else if a.eindex > b.eindex then CSup
		else if Array.length a.eargs = 0 && Array.length b.eargs = 0 then CEq
		else CUndef
	| VFieldClosure(v1,f1),VFieldClosure(v2,f2) ->
		if f1 != f2 then CUndef
		else compare v1 v2
	| _ -> CUndef

let rec equals a b = match a,b with
	| VNull,VNull -> true
	| VInt32 a,VInt32 b -> a = b
	| VFloat a,VFloat b -> a = b
	| VFloat a,VInt32 b -> a = (Int32.to_float b)
	| VInt32 a,VFloat b -> (Int32.to_float a) = b
	| VTrue,VTrue | VFalse,VFalse -> true
	| VString(r1,s1),VString(r2,s2) -> r1 == r2 || Lazy.force s1 = Lazy.force s2
	| VArray a,VArray b -> a == b
	| VFunction(a,_),VFunction(b,_) -> a == b
	| VObject a,VObject b -> a == b
	| VInstance a,VInstance b -> a == b
	| VPrototype a,VPrototype b -> a == b
	| VEnumValue a,VEnumValue b -> a == b || a.eindex = b.eindex && Array.length a.eargs = 0 && Array.length b.eargs = 0
	| VFieldClosure(v1,f1),VFieldClosure(v2,f2) -> f1 == f2 && equals v1 v2
	| _ -> false

let rec arrays_equal a1 a2 =
	if Array.length a1 <> Array.length a2 then
		false
	else begin
		let rec loop i =
			if i = Array.length a1 then true
			else if not (equals_structurally a1.(i) a2.(i)) then false
			else loop (i + 1)
		in
		loop 0
	end

and equals_structurally a b =
	match a,b with
	| VNull,VNull -> true
	| VInt32 a,VInt32 b -> Int32.compare a b = 0
	| VFloat a,VFloat b -> a = b
	| VFloat a,VInt32 b -> a = (Int32.to_float b)
	| VInt32 a,VFloat b -> (Int32.to_float a) = b
	| VTrue,VTrue | VFalse,VFalse -> true
	| VString(_,s1),VString(_,s2) -> Lazy.force s1 = Lazy.force s2
	| VFunction(a,_),VFunction(b,_) -> a == b
	| VArray a,VArray b -> a == b || arrays_equal a.avalues b.avalues
	| VObject a,VObject b -> a == b || arrays_equal a.ofields b.ofields && IntMap.equal equals_structurally a.oextra b.oextra
	| VInstance a,VInstance b -> a == b
	| VPrototype a,VPrototype b -> a == b
	| VEnumValue a,VEnumValue b -> a == b || a.eindex = b.eindex && arrays_equal a.eargs b.eargs
	| VFieldClosure(v1,f1),VFieldClosure(v2,f2) -> f1 == f2 && equals v1 v2
	| _ -> false

let is v path =
	path = key_Dynamic || match v with
	| VInt32 _ -> path = key_Int || path = key_Float
	| VFloat f -> path = key_Float || (path = key_Int && f = (float_of_int (int_of_float f)) && f <= 2147483647. && f >= -2147483648.)
	| VTrue | VFalse -> path = key_Bool
	| VPrototype {pkind = PClass _} -> path = key_Class
	| VPrototype {pkind = PEnum _} -> path = key_Enum
	| VEnumValue ve -> path = key_EnumValue || path = ve.epath
	| VString _ -> path = key_String
	| VArray _ -> path = key_Array
	| VInstance vi ->
		let has_interface path' =
			try begin match (get_static_prototype_raise (get_ctx()) path').pkind with
				| PClass interfaces -> List.mem path interfaces
				| _ -> false
			end with Not_found ->
				false
		in
		let rec loop proto =
			if path = proto.ppath || has_interface proto.ppath then true
			else begin match proto.pparent with
				| Some proto -> loop proto
				| None -> false
			end
		in
		loop vi.iproto
	| _ -> false

let is_true v = match v with
	| VTrue -> true
	| _ -> false