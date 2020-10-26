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
open EvalHash

type cmp =
	| CEq
	| CSup
	| CInf
	| CUndef

type vstring = {
	(* The bytes representation of the string. This is only evaluated if we
	   need it for something like random access. *)
	sstring : UTF8.t;
	(* The length of the string. *)
	slength : int;
	(* The current (character * byte) offsets. *)
	mutable soffsets : (int ref * int ref) list;
}

type vstring_buffer = {
	        bbuffer : Buffer.t;
	mutable blength : int;
}

let vstring_equal s1 s2 =
	s1 == s2 || s1.sstring = s2.sstring

module StringHashtbl = struct
	type 'value t = (vstring * 'value) StringMap.t ref

	let add this key v = this := StringMap.add key.sstring (key,v) !this
	let copy this = ref !this
	let create () = ref StringMap.empty
	let find this key = StringMap.find key.sstring !this
	let fold f this acc = StringMap.fold f !this acc
	let is_empty this = StringMap.is_empty !this
	let iter f this = StringMap.iter f !this
	let mem this key = StringMap.mem key.sstring !this
	let remove this key = this := StringMap.remove key.sstring !this
	let clear this = this := StringMap.empty
end

module IntHashtbl = struct
	type 'value t = (int, 'value) Hashtbl.t

	let add this key v = Hashtbl.replace this key v
	let copy this = Hashtbl.copy this
	let create () = Hashtbl.create 0
	let find this key = Hashtbl.find this key
	let fold f this acc = Hashtbl.fold f this acc
	let is_empty this = Hashtbl.length this = 0
	let iter f this = Hashtbl.iter f this
	let mem this key = Hashtbl.mem this key
	let remove this key = Hashtbl.remove this key
	let clear this = Hashtbl.clear this
end

type vregex = {
	r : Pcre.regexp;
	r_rex_string : vstring;
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
	| PEnum of (string * int list) list
	| PInstance
	| PObject

type vhandle =
	| HLoop of Luv.Loop.t
	| HIdle of Luv.Idle.t
	| HTimer of Luv.Timer.t
	| HAsync of Luv.Async.t
	| HBuffer of Luv.Buffer.t
	| HSockAddr of Luv.Sockaddr.t
	| HTcp of Luv.TCP.t
	| HUdp of Luv.UDP.t
	| HPipe of Luv.Pipe.t
	| HTty of Luv.TTY.t
	| HFile of Luv.File.t
	| HSignal of Luv.Signal.t
	| HProcess of Luv.Process.t
	| HRedirection of Luv.Process.redirection
	| HAddrRequest of Luv.DNS.Addr_info.Request.t
	| HNameRequest of Luv.DNS.Name_info.Request.t
	| HFileRequest of Luv.File.Request.t
	| HRandomRequest of Luv.Random.Request.t
	| HThreadPoolRequest of Luv.Thread_pool.Request.t
	| HFileModeNumeric of Luv.File.Mode.numeric

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
	| VLazy of (unit -> value) ref
	| VNativeString of string
	| VHandle of vhandle
	| VInt64 of Signed.Int64.t
	| VUInt64 of Unsigned.UInt64.t

and vfunc = value list -> value

and vobject = {
	(* The fields of the object known when it is created. *)
	mutable ofields : value array;
	(* The prototype of the object. *)
	mutable oproto : vprototype;
}

and vprototype = {
	(* The path of the prototype. Using rev_hash on this gives the original dot path. *)
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
	| IBuffer of vstring_buffer(* StringBuf *)
	| IPos of pos
	| IUtf8 of UTF8.Buf.buf
	| IProcess of Process.process
	| IInChannel of in_channel * bool ref (* FileInput *)
	| IOutChannel of out_channel (* FileOutput *)
	| ISocket of Unix.file_descr
	| IThread of vthread
	| IMutex of vmutex
	| ILock of vlock
	| ITls of int
	| IDeque of vdeque
	| IZip of vzlib (* Compress/Uncompress *)
	| ITypeDecl of Type.module_type
	| ILazyType of (Type.tlazy ref) * (unit -> value)
	| IRef of Obj.t
	(* SSL *)
	| IMbedtlsConfig of Mbedtls.mbedtls_ssl_config
	| IMbedtlsCtrDrbg of Mbedtls.mbedtls_ctr_drbg_context
	| IMbedtlsEntropy of Mbedtls.mbedtls_entropy_context
	| IMbedtlsPkContext of Mbedtls.mbedtls_pk_context
	| IMbedtlsSsl of Mbedtls.mbedtls_ssl_context
	| IMbedtlsX509Crt of Mbedtls.mbedtls_x509_crt
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

and vthread = {
	mutable tthread : Thread.t;
	tdeque : vdeque;
	mutable tevents : value;
	mutable tstorage : value IntMap.t;
}

and vdeque = {
	mutable dvalues : value list;
	dmutex : Mutex.t;
}

and vmutex = {
	mmutex : Mutex.t;
	mutable mowner : int option; (* thread ID *)
}

and vlock = {
	ldeque : vdeque;
}

let same_handle h1 h2 =
	match h1, h2 with
	| HLoop h1, HLoop h2 -> h1 == h2
	| HIdle h1, HIdle h2 -> h1 == h2
	| HTimer h1, HTimer h2 -> h1 == h2
	| HAsync h1, HAsync h2 -> h1 == h2
	| HBuffer h1, HBuffer h2 -> h1 == h2
	| HSockAddr h1, HSockAddr h2 -> h1 == h2
	| HTcp h1, HTcp h2 -> h1 == h2
	| HPipe h1, HPipe h2 -> h1 == h2
	| HTty h1, HTty h2 -> h1 == h2
	| HFile h1, HFile h2 -> h1 == h2
	| HUdp h1, HUdp h2 -> h1 == h2
	| HSignal h1, HSignal h2 -> h1 == h2
	| HProcess h1, HProcess h2 -> h1 == h2
	| HRedirection h1, HRedirection h2 -> h1 == h2
	| HFileRequest h1, HFileRequest h2 -> h1 == h2
	| HNameRequest h1, HNameRequest h2 -> h1 == h2
	| HAddrRequest h1, HAddrRequest h2 -> h1 == h2
	| HRandomRequest h1, HRandomRequest h2 -> h1 == h2
	| HThreadPoolRequest h1, HThreadPoolRequest h2 -> h1 == h2
	| HFileModeNumeric h1, HFileModeNumeric h2 -> h1 == h2
	| HBuffer _,_ | HAsync _,_ | HTimer _, _ | HLoop _, _ | HIdle _, _ | HSockAddr _, _
	| HTcp _, _ | HPipe _, _ | HTty _, _ | HFile _, _ | HUdp _, _ | HSignal _, _
	| HProcess _, _ | HRedirection _, _| HFileRequest _, _ | HAddrRequest _, _
	| HNameRequest _, _ | HRandomRequest _, _ | HThreadPoolRequest _, _
	| HFileModeNumeric _, _ -> false

let rec equals a b = match a,b with
	| VTrue,VTrue
	| VFalse,VFalse
	| VNull,VNull -> true
	| VInt32 a,VInt32 b -> a = b
	| VFloat a,VFloat b -> a = b
	| VFloat a,VInt32 b -> a = (Int32.to_float b)
	| VInt32 a,VFloat b -> (Int32.to_float a) = b
	| VEnumValue a,VEnumValue b -> a == b || a.eindex = b.eindex && Array.length a.eargs = 0 && Array.length b.eargs = 0 && a.epath = b.epath
	| VObject vo1,VObject vo2 -> vo1 == vo2
	| VInstance vi1,VInstance vi2 -> vi1 == vi2
	| VString s1,VString s2 -> vstring_equal s1 s2
	| VArray va1,VArray va2 -> va1 == va2
	| VVector vv1,VVector vv2 -> vv1 == vv2
	| VFunction(vf1,_),VFunction(vf2,_) -> vf1 == vf2
	| VPrototype proto1,VPrototype proto2 -> proto1.ppath = proto2.ppath
	| VNativeString s1,VNativeString s2 -> s1 = s2
	| VHandle h1,VHandle h2 -> same_handle h1 h2
	| VLazy f1,_ -> equals (!f1()) b
	| _,VLazy f2 -> equals a (!f2())
	| _ -> a == b

module ValueHashtbl = Hashtbl.Make(struct
	type t = value

	let equal = equals

	let hash _ = 0
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
let vnative_string s = VNativeString s

let s_expr_pretty e = (Type.s_expr_pretty false "" false (Type.s_type (Type.print_context())) e)

let rec vresolve v = match v with
	| VLazy f -> vresolve (!f())
	| _ -> v