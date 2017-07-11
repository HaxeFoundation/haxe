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
open EvalHash

type cmp =
	| CEq
	| CSup
	| CInf
	| CUndef

type vstring = Rope.t * string Lazy.t

module StringHashtbl = Hashtbl.Make(struct
	type t = vstring
	let equal (r1,s1) (r2,s2) = r1 == r2 || Lazy.force s1 = Lazy.force s2
	let hash (_,s) = Hashtbl.hash (Lazy.force s)
end)

module IntHashtbl = Hashtbl.Make(struct type t = int let equal = (=) let hash = Hashtbl.hash end)

type vregex = {
	r : Pcre.regexp;
	r_global : bool;
	mutable r_string : string;
	mutable r_groups : Pcre.substrings array;
}

type vzlib = {
	z : Extc.zstream;
	mutable z_flush : Extc.zflush;
}

type vprototype_kind =
	| PClass of int list
	| PEnum of string list
	| PInstance
	| PObject

type value =
	| VNull
	| VTrue
	| VFalse
	| VInt32 of int32
	| VFloat of float
	| VEnumValue of venum_value
	| VObject of vobject
	| VInstance of vinstance
	| VString of vstring
	| VArray of varray
	| VVector of vvector
	| VPrototype of vprototype
	| VFunction of vfunc * bool
	| VFieldClosure of value * vfunc

and vfunc =
	| Fun0 of (unit -> value)
	| Fun1 of (value -> value)
	| Fun2 of (value -> value -> value)
	| Fun3 of (value -> value -> value -> value)
	| Fun4 of (value -> value -> value -> value -> value)
	| Fun5 of (value -> value -> value -> value -> value -> value)
	| FunN of (value list -> value)

and vobject = {
	(* The fields of the object known when it is created. *)
	ofields : value array;
	(* The prototype of the object. *)
	oproto : vprototype;
	(* Extra fields that were added after the object was created. *)
	mutable oextra : value IntMap.t;
	(* Map of fields (in ofields) that were deleted via Reflect.deleteField *)
	mutable oremoved : bool IntMap.t;
}

and vprototype = {
	(* The path of the prototype. Using rev_hash_s on this gives the original dot path. *)
	ppath : int;
	(* The fields of the prototype itself (static fields). *)
	pfields : value array;
	(* Map from hashed name to field offset (in pfields). *)
	pnames : int IntMap.t;
	(* The fields of instances of this prototype (non-static fields). *)
	pinstance_fields : value array;
	(* Map from hashed name to field offset (in pinstance_fields). *)
	pinstance_names : int IntMap.t;
	(*
		The parent prototype in case of inheritance. Static inheritance is reflected here
		as well because that information is requires for Type.getSuperClass.
	*)
	pparent : vprototype option;
	(* The [vprototype_kind]. *)
	pkind : vprototype_kind;
	(* The value of this prototype, i.e. VPrototype self. *)
	mutable pvalue : value;
}

and vinstance_kind =
	| IBytes of bytes
	| IRegex of vregex
	| IDate of float
	| IStringMap of value StringHashtbl.t
	| IIntMap of value IntHashtbl.t
	| IObjectMap of (value,value) Hashtbl.t
	| IOutput of Buffer.t (* BytesBuffer *)
	| IBuffer of Rope.Buffer.t (* StringBuf *)
	| IPos of pos
	| IUtf8 of UTF8.Buf.buf
	| IProcess of Process.process
	| IInChannel of in_channel * bool ref (* FileInput *)
	| IOutChannel of out_channel (* FileOutput *)
	| ISocket of Unix.file_descr
	| IThread of Thread.t
	| IZip of vzlib (* Compress/Uncompress *)
	| ITypeDecl of Type.module_type
	| ILazyType of ((unit -> Type.t) ref) * (unit -> value)
	| IRef of Obj.t
	| INormal

and vinstance = {
	(* The fields of this instance. *)
	ifields : value array;
	(*
		The prototype of this instance. Field offsets for ifields can be found using
		iproto.pinstance_names.
	*)
	iproto : vprototype;
	(* The [vinstance_kind]. *)
	mutable ikind : vinstance_kind;
}

and varray = {
	mutable avalues : value array;
	mutable alength : int
}

and vvector = value array

and venum_value = {
	eindex : int;
	eargs : value array;
	epath : int;
	enpos : pos option;
}

module ValueHashtbl = Hashtbl.Make(struct
	type t = value

	let equal a b = match a,b with
		| VObject o1,VObject o2 -> o1 == o2
		| VInstance vi1,VInstance vi2 -> vi1 == vi2
		| VPrototype p1,VPrototype p2 -> p1 == p2
		| VFunction(f1,_),VFunction(f2,_) -> f1 == f2
		| VFieldClosure(v1,f1),VFieldClosure(v2,f2) -> v1 == v2 && f1 == f2
		| _ -> false

	let hash = Hashtbl.hash
end)

let vnull = VNull
let vtrue = VTrue
let vfalse = VFalse
let vbool b = if b then VTrue else VFalse
let vprototype proto = VPrototype proto
let vinstance i = VInstance i
let vfunction f = VFunction(f,false)
let vstatic_function f = VFunction(f,true)
let vfield_closure v f = VFieldClosure(v,f)
let vobject o = VObject o
let vint i = VInt32 (Int32.of_int i)
let vint32 i = VInt32 i
let vfloat f = VFloat f
let venum_value e = VEnumValue e

let s_expr_pretty e = (Type.s_expr_pretty false "" false (Type.s_type (Type.print_context())) e)

let num_args = function
	| Fun0 _ -> 0
	| Fun1 _ -> 1
	| Fun2 _ -> 2
	| Fun3 _ -> 3
	| Fun4 _ -> 4
	| Fun5 _ -> 5
	| FunN _ -> -1