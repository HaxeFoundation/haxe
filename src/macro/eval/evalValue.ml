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

module DomainMutex = struct
	type t = {
		dmutex : Mutex.t;
		downer : int Atomic.t;  (* owner domain_id, or -1 if unlocked *)
		mutable ddepth : int;   (* reentrant depth, only accessed by owner *)
	}

	let create () = {
		dmutex = Mutex.create();
		downer = Atomic.make (-1);
		ddepth = 0;
	}

	let lock mutex domain_id =
		if Atomic.get mutex.downer = domain_id then
			mutex.ddepth <- mutex.ddepth + 1
		else begin
			Mutex.lock mutex.dmutex;
			Atomic.set mutex.downer domain_id;
			mutex.ddepth <- 1
		end

	let try_lock mutex domain_id =
		if Atomic.get mutex.downer = domain_id then begin
			mutex.ddepth <- mutex.ddepth + 1;
			true
		end else if Mutex.try_lock mutex.dmutex then begin
			Atomic.set mutex.downer domain_id;
			mutex.ddepth <- 1;
			true
		end else
			false

	let unlock mutex =
		if mutex.ddepth > 1 then
			mutex.ddepth <- mutex.ddepth - 1
		else begin
			Atomic.set mutex.downer (-1);
			Mutex.unlock mutex.dmutex
		end
end

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

module RuntimeStringHashtbl = struct
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
	let size this = StringMap.cardinal !this
end

module RuntimeIntHashtbl = struct
	type 'value t = 'value IntHashtbl.t

	let add this key v = IntHashtbl.replace this key v
	let copy this = IntHashtbl.copy this
	let create () = IntHashtbl.create 0
	let find this key = IntHashtbl.find this key
	let fold f this acc = IntHashtbl.fold f this acc
	let is_empty this = IntHashtbl.length this = 0
	let iter f this = IntHashtbl.iter f this
	let mem this key = IntHashtbl.mem this key
	let remove this key = IntHashtbl.remove this key
	let clear this = IntHashtbl.clear this
	let size this = IntHashtbl.length this
end

module RuntimeInt64Hashtbl = struct
	type 'value t = 'value Int64Hashtbl.t

	let add this key v = Int64Hashtbl.replace this key v
	let copy this = Int64Hashtbl.copy this
	let create () = Int64Hashtbl.create 0
	let find this key = Int64Hashtbl.find this key
	let fold f this acc = Int64Hashtbl.fold f this acc
	let is_empty this = Int64Hashtbl.length this = 0
	let iter f this = Int64Hashtbl.iter f this
	let mem this key = Int64Hashtbl.mem this key
	let remove this key = Int64Hashtbl.remove this key
	let clear this = Int64Hashtbl.clear this
	let size this = Int64Hashtbl.length this
end

type vregex = {
	r : Pcre2.regexp;
	r_rex_string : vstring;
	r_global : bool;
	mutable r_string : string;
	mutable r_groups : Pcre2.substrings array;
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
	| HDir of Luv.File.Dir.t
	| HSignal of Luv.Signal.t
	| HProcess of Luv.Process.t
	| HRedirection of Luv.Process.redirection
	| HAddrRequest of Luv.DNS.Addr_info.Request.t
	| HNameRequest of Luv.DNS.Name_info.Request.t
	| HFileRequest of Luv.File.Request.t
	| HRandomRequest of Luv.Random.Request.t
	| HThreadPoolRequest of Luv.Thread_pool.Request.t
	| HFileModeNumeric of Luv.File.Mode.numeric
	| HFsEvent of Luv.FS_event.t
	| HThread of Luv.Thread.t
	| HOnce of Luv.Once.t
	| HMutex of Luv.Mutex.t
	| HRwLock of Luv.Rwlock.t
	| HSemaphore of Luv.Semaphore.t
	| HCondition of Luv.Condition.t
	| HBarrier of Luv.Barrier.t
	| HFsPoll of Luv.FS_poll.t
	| HPrepare of Luv.Prepare.t
	| HCheck of Luv.Check.t

type value =
	| VNull
	| VTrue
	| VFalse
	(* An Int: a signed 32-bit value, unboxed (see [wrap32]). *)
	| VInt of int
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
	| VLazy of value AtomicLazy.t
	| VNativeString of string
	| VHandle of vhandle
	| VInt64 of Signed.Int64.t
	| VUInt64 of Unsigned.UInt64.t

and vfunc = value list -> value

and vobject = {
	(* The fields of the object known when it is created. *)
	mutable ofields : value array;
	(* The prototype of the object. *)
	mutable oproto : vobject_proto;
}

and vobject_proto =
	| OProto of vprototype
	| ODictionary of value IntMap.t

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
	(* Static isolation (docs/incremental-compilation.md, phase 7): the
	   initializers of the static variables (variables without one are set to
	   null). *)
	mutable pinit : (vprototype -> unit) list;
	(* the slots initialized again by [pinit] (not @:persistent) *)
	mutable preset : int array;
	(* During an expansion, the values of the [preset] slots are those of the
	   domain running it (macros can run on several domains): by domain slot
	   (see [domain_slot]), the values and the expansion they were initialized
	   for. [presettable] tells which slots they are. *)
	mutable presettable : bool array;
	(* the @:persistent slots, shared by the expansions *)
	mutable pshared : bool array;
	mutable pdomain : value array array;
	mutable pdomain_epoch : int array;
}

and vinstance_kind =
	| IBytes of bytes
	| IRegex of vregex
	| IDate of float
	| IStringMap of value RuntimeStringHashtbl.t
	| IIntMap of value RuntimeIntHashtbl.t
	| IInt64Map of value RuntimeInt64Hashtbl.t
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
	| IMutex of DomainMutex.t
	| ISemaphore of Semaphore.Counting.t
	| ICondition of vcondition
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
	| IAtomicBool of bool Atomic.t
	| IAtomicInt of int Atomic.t
	| IAtomicObject of value Atomic.t

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
	mutable enpos : pos option;
}

and vthread_mode =
	| Thread of Thread.t
	| Domain of unit Domain.t
	| LuvThread of Luv.Thread.t

and vthread = {
	thread_id : int;
	mutable thread_mode : vthread_mode;
	thread_deque : vdeque;
}

and vdeque = {
	mutable dvalues : value list;
	dmutex : Mutex.t;
	dcond : Condition.t;
}

and vlock = {
	lmutex : Mutex.t;
	lcond : Condition.t;
	mutable lcount : int;
}

and vcondition = {
	cond : Condition.t;
	cmutex : Mutex.t;
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
	| HDir h1, HDir h2 -> h1 == h2
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
	| HFsEvent h1, HFsEvent h2 -> h1 == h2
	| HThread h1, HThread h2 -> Luv.Thread.equal h1 h2
	| HOnce h1, HOnce h2 -> h1 == h2
	| HMutex h1, HMutex h2 -> h1 == h2
	| HRwLock h1, HRwLock h2 -> h1 == h2
	| HSemaphore h1, HSemaphore h2 -> h1 == h2
	| HCondition h1, HCondition h2 -> h1 == h2
	| HBarrier h1, HBarrier h2 -> h1 == h2
	| HFsPoll h1, HFsPoll h2 -> h1 == h2
	| HPrepare h1, HPrepare h2 -> h1 == h2
	| HCheck h1, HCheck h2 -> h1 == h2
	| HBuffer _,_ | HAsync _,_ | HTimer _, _ | HLoop _, _ | HIdle _, _ | HSockAddr _, _
	| HTcp _, _ | HPipe _, _ | HTty _, _ | HFile _, _ | HUdp _, _ | HSignal _, _
	| HProcess _, _ | HRedirection _, _| HFileRequest _, _ | HAddrRequest _, _
	| HNameRequest _, _ | HRandomRequest _, _ | HThreadPoolRequest _, _
	| HFileModeNumeric _, _ | HDir _, _ | HFsEvent _, _ | HThread _, _ | HOnce _, _
	| HMutex _, _ | HRwLock _, _ | HSemaphore _, _ | HCondition _, _ | HBarrier _, _
	| HFsPoll _, _ | HPrepare _, _ | HCheck _, _ -> false

let rec equals a b = match a,b with
	| VTrue,VTrue
	| VFalse,VFalse
	| VNull,VNull -> true
	| VInt a,VInt b -> a = b
	| VFloat a,VFloat b -> a = b
	| VFloat a,VInt b -> a = (float_of_int b)
	| VInt a,VFloat b -> (float_of_int a) = b
	| VEnumValue a,VEnumValue b -> a == b || a.eindex = b.eindex && Array.length a.eargs = 0 && Array.length b.eargs = 0 && a.epath = b.epath
	| VObject vo1,VObject vo2 -> vo1 == vo2
	| VInstance vi1,VInstance vi2 -> vi1 == vi2
	| VString s1,VString s2 -> vstring_equal s1 s2
	| VArray va1,VArray va2 -> va1 == va2
	| VVector vv1,VVector vv2 -> vv1 == vv2
	| VFunction(vf1,_),VFunction(vf2,_) -> vf1 == vf2
	| VPrototype proto1,VPrototype proto2 -> proto1.ppath = proto2.ppath
	| VFieldClosure(v1,f1),VFieldClosure(v2,f2) -> f1 == f2 && equals v1 v2
	| VNativeString s1,VNativeString s2 -> s1 = s2
	| VHandle h1,VHandle h2 -> same_handle h1 h2
	| VLazy f1,_ -> equals (AtomicLazy.force f1) b
	| _,VLazy f2 -> equals a (AtomicLazy.force f2)
	| _ -> a == b

module ValueHashtbl = Hashtbl.Make(struct
	type t = value

	let equal = equals

	let hash _ = 0
end)

(* Called before a value shared by the macros (a static variable, an effect of
   the macro API or of the standard library) is read or changed: a macro
   running out of the order of the tasks typing in parallel waits for it there
   (see [Claims.speculating]). *)
let on_shared_state : ((unit -> string) -> unit) ref = ref (fun _ -> ())

(* The name of the static variable at slot [i] of [proto], for reports. *)
let static_name proto i =
	let name = IntMap.fold (fun n j acc -> if j = i then EvalHash.rev_hash n else acc) proto.pnames "?" in
	EvalHash.rev_hash proto.ppath ^ "." ^ name

(* Static isolation (docs/incremental-compilation.md, phase 7, item 4): the
   static variables of the macro context start from their initial value in
   every expansion, and an expansion's writes are discarded when it ends (so
   that a nested expansion leaves its caller's state as it was). Statics are
   initialized again lazily, on the first access in an expansion. *)
let epoch_counter = Atomic.make 0
let current_epoch : int DomainLocal.t = DomainLocal.make (fun () -> 0)
(* the prototypes initialized again by the current expansion, with the previous
   values of their [preset] slots, and the signatures of their initial values
   when the writes of the expansion are checked *)
let restore_log : (vprototype * int * value array * int * string array option) list DomainLocal.t = DomainLocal.make (fun () -> [])

(* An initialization macro configures the macros that run after it: what it
   writes to a static that is reset is lost, which [lost_write] reports (the
   static should be @:persistent). [init_macro_call] marks the next expansion
   as an initialization macro, whose writes are checked; nested expansions are
   not. *)
let init_macro_call : bool DomainLocal.t = DomainLocal.make (fun () -> false)
let check_writes : bool DomainLocal.t = DomainLocal.make (fun () -> false)
let value_signature : (value -> string) ref = ref (fun _ -> "")
let lost_write : (vprototype -> int -> unit) ref = ref (fun _ _ -> ())

(* The slot of the current domain in [pdomain] (slots are reused once their
   domain exits). *)
let max_domain_slots = 256
let slot_mutex = Mutex.create ()
let next_slot = ref 0
let free_slots = ref []
let domain_slot : int Domain.DLS.key = Domain.DLS.new_key (fun () ->
	let s = Mutex.protect slot_mutex (fun () -> match !free_slots with
		| s :: l -> free_slots := l; s
		| [] -> let s = !next_slot in incr next_slot; s
	) in
	if s >= max_domain_slots then failwith "Too many domains running macros";
	Domain.at_exit (fun () -> Mutex.protect slot_mutex (fun () -> free_slots := s :: !free_slots));
	s
)

(* Gives [proto] the storage of its isolated static variables. *)
let set_isolated_statics proto slots shared =
	proto.pshared <- Array.make (Array.length proto.pfields) false;
	Array.iter (fun i -> proto.pshared.(i) <- true) shared;
	proto.preset <- slots;
	proto.presettable <- Array.make (Array.length proto.pfields) false;
	Array.iter (fun i -> proto.presettable.(i) <- true) slots;
	proto.pdomain <- Array.make max_domain_slots [||];
	proto.pdomain_epoch <- Array.make max_domain_slots (-1)

(* The values of the isolated static variables of [proto] for the running
   expansion, initialized on first access. *)
let rec isolated_values proto e =
	let s = Domain.DLS.get domain_slot in
	if Array.unsafe_get proto.pdomain_epoch s <> e then begin
		let old = proto.pdomain.(s) and old_epoch = proto.pdomain_epoch.(s) in
		proto.pdomain.(s) <- Array.copy proto.pfields;
		proto.pdomain_epoch.(s) <- e;
		List.iter (fun f -> f proto) proto.pinit;
		let sigs = if DomainLocal.get check_writes then Some (Array.map (fun i -> !value_signature proto.pdomain.(s).(i)) proto.preset) else None in
		DomainLocal.set restore_log ((proto,s,old,old_epoch,sigs) :: DomainLocal.get restore_log)
	end;
	Array.unsafe_get proto.pdomain s

and fresh_statics proto =
	if proto.pinit <> [] then begin
		let e = DomainLocal.get current_epoch in
		if e <> 0 then ignore (isolated_values proto e)
	end;
	match proto.pparent with
	| Some p -> fresh_statics p
	| None -> ()

(* Whether the static variable at slot [i] of [proto] is shared by the
   expansions (@:persistent; not the isolated ones, the methods and the
   natives of the standard library): reading or changing it is an access to
   what the macros share (see [on_shared_state]). *)
let is_shared_static proto i =
	Array.length proto.pshared > 0 && Array.unsafe_get proto.pshared i

(* The same, by name. *)
let is_shared_static_name proto name =
	match IntMap.find_opt name proto.pnames with
	| Some i -> is_shared_static proto i
	| None -> true

(* The static variable at slot [i] of [proto]: during an expansion, an
   isolated one is the running expansion's. *)
let get_static proto i =
	if Array.length proto.presettable > 0 && Array.unsafe_get proto.presettable i then begin
		let e = DomainLocal.get current_epoch in
		if e <> 0 then Array.unsafe_get (isolated_values proto e) i else proto.pfields.(i)
	end else
		proto.pfields.(i)

let set_static proto i v =
	if Array.length proto.presettable > 0 && Array.unsafe_get proto.presettable i then begin
		let e = DomainLocal.get current_epoch in
		if e <> 0 then Array.unsafe_set (isolated_values proto e) i v else proto.pfields.(i) <- v
	end else
		proto.pfields.(i) <- v

(* Runs [f] as an expansion of its own. *)
let with_expansion f =
	let old_epoch = DomainLocal.get current_epoch and old_log = DomainLocal.get restore_log in
	let old_check = DomainLocal.get check_writes in
	DomainLocal.set check_writes (DomainLocal.get init_macro_call);
	DomainLocal.set init_macro_call false;
	DomainLocal.set current_epoch (Atomic.fetch_and_add epoch_counter 1 + 1);
	DomainLocal.set restore_log [];
	Std.finally (fun () ->
		List.iter (fun (proto,s,old,epoch,sigs) ->
			Option.may (fun sigs ->
				Array.iteri (fun k i -> if !value_signature proto.pdomain.(s).(i) <> sigs.(k) then !lost_write proto i) proto.preset
			) sigs;
			(* the values of the caller (the @:persistent slots are not isolated) *)
			proto.pdomain.(s) <- old;
			proto.pdomain_epoch.(s) <- epoch
		) (DomainLocal.get restore_log);
		DomainLocal.set current_epoch old_epoch;
		DomainLocal.set check_writes old_check;
		DomainLocal.set restore_log old_log
	) f ()

let vnull = VNull

(* An array of [n] nulls: small ones are allocated inline (Array.make is a C
   call). *)
let make_null_array n = match n with
	| 0 -> [||]
	| 1 -> [|VNull|]
	| 2 -> [|VNull;VNull|]
	| 3 -> [|VNull;VNull;VNull|]
	| 4 -> [|VNull;VNull;VNull;VNull|]
	| 5 -> [|VNull;VNull;VNull;VNull;VNull|]
	| 6 -> [|VNull;VNull;VNull;VNull;VNull;VNull|]
	| 7 -> [|VNull;VNull;VNull;VNull;VNull;VNull;VNull|]
	| 8 -> [|VNull;VNull;VNull;VNull;VNull;VNull;VNull;VNull|]
	| n -> Array.make n VNull
let vtrue = VTrue
let vfalse = VFalse
let vbool b = if b then VTrue else VFalse
let vprototype proto = VPrototype proto
let vinstance i = VInstance i
let vfunction f = VFunction(f,false)
let vstatic_function f = VFunction(f,true)
let vfield_closure v f = VFieldClosure(v,f)
let vobject o = VObject o
(* [i] wrapped to a signed 32-bit value, as an Int holds it *)
let int32_shift = Sys.int_size - 32
let wrap32 i = (i lsl int32_shift) asr int32_shift [@@inline]

let vint i = VInt (wrap32 i) [@@inline]
let vint32 i = VInt (Int32.to_int i)
let vint64 i = VInt64 i
let vfloat f = VFloat f
let venum_value e = VEnumValue e
let vnative_string s = VNativeString s

let s_expr_pretty e = (Type.s_expr_pretty false "" false (Type.s_type (Type.print_context())) e)

let rec vresolve_lazy f = match AtomicLazy.force f with
	| VLazy f -> vresolve_lazy f
	| v -> v

let vresolve v = match v with
	| VLazy f -> vresolve_lazy f
	| _ -> v
[@@inline]

let associate_enum_value_pos ve p = match ve with
	| VEnumValue ve ->
		ve.enpos <- Some p
	| _ ->
		()