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
	(* the frozen group of the object, 0 if none (see [freeze_persistent]) *)
	mutable omark : int;
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
	(* Static isolation (docs/incremental-compilation.md, phase 7): the slots
	   of the static variables that every expansion sees as the base left them
	   (not @:persistent, see [get_static]). *)
	mutable preset : int array;
	(* During an expansion, the values of the [preset] slots are those of the
	   copy of the base of the domain running it (macros can run on several
	   domains): by domain slot (see [domain_slot]), the values, the generation
	   of the base they were copied from and the last expansion that journaled
	   them. [presettable] tells which slots they are. *)
	mutable presettable : bool array;
	(* the @:persistent slots, shared by the expansions *)
	mutable pshared : bool array;
	mutable pdomain : value array array;
	mutable pdomain_gen : int array;
	mutable pdomain_epoch : int array;
	(* the [preset] slots holding what writes cannot be journaled for (a
	   vector, bytes...): copied again from the base by every expansion *)
	mutable pvolatile : int array;
	(* while the initializers of the prototype run: its slots are its base *)
	mutable pinitializing : bool;
	(* what records that an expansion reads its isolated variables: it depends
	   on their initializers (see [DepRecord.exec_recorder]), and the last
	   expansion that did, by domain slot *)
	mutable pexec : (int -> unit) list;
	mutable pdomain_seen : int array;
	(* the frozen group of each @:persistent slot, 0 if it is not frozen, -1 if
	   what it holds cannot be frozen (see [freeze_persistent]) *)
	mutable pfrozen : int array;
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
	(* the frozen group of the instance, 0 if none (see [freeze_persistent]) *)
	mutable imark : int;
}

and varray = {
	mutable avalues : value array;
	mutable alength : int;
	(* the frozen group of the array, 0 if none (see [freeze_persistent]) *)
	mutable amark : int;
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

(* Static isolation (docs/incremental-compilation.md, phase 7, item 4): every
   expansion starts from the same state of the static variables of the macro
   context, their base: what their initializers and the initialization macros
   left in [pfields]. A domain running macros works on a deep copy of the base,
   made on first access once per generation of the base, and the changes an
   expansion makes to that copy (to its slots, or in place to the objects
   copied from the base) are journaled and undone when it ends: the next
   expansion on the domain finds the base. A nested expansion starts from the
   base too: the changes of its callers are set aside while it runs (the
   entries of the journal swap the state of what they journaled). The base only changes while no copy of it is made: by the
   initialization macros, by the reset of a new compilation, and in the slots
   of the prototypes whose initializers run. The @:persistent variables are
   not copied: they are shared (see [freeze_persistent]). *)
let epoch_counter = Atomic.make 0
let current_epoch : int DomainLocal.t = DomainLocal.make (fun () -> 0)
(* the changes of the current expansion to the copy of the base of its domain,
   the last one first: an entry swaps the state of what it journaled with the
   state it saved, which undoes the change (and does it again) *)
let journal : (unit -> unit) list DomainLocal.t = DomainLocal.make (fun () -> [])

let journal_undo f = DomainLocal.set journal (f :: DomainLocal.get journal)

(* While an initialization macro runs (with what it runs): it changes the
   base, which configures the macros that run after it. *)
let base_mode : bool DomainLocal.t = DomainLocal.make (fun () -> false)

(* Whether the initialization macros of the compilation are done: the
   initializers of the prototypes loaded since then only give the base of
   their own variables (see [initialize_statics]). *)
let init_phase_done = ref false

(* The generation of the base: the copies of an older one are made again. *)
let base_generation = Atomic.make 0

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

(* The marks of the arrays, objects and instances ([amark], [omark], [imark]):
   0 for none; a frozen group of @:persistent state (see [freeze_persistent])
   in ]0,base_mark[; an object of the base, with its identity (see
   [seal_statics]), from [base_mark]; below 0, an object of the copy of the
   base of a domain slot, with the last expansion that journaled it. *)
let base_mark = 1 lsl 48
let is_group_mark m = m > 0 && m < base_mark
let copy_mark epoch s = -(epoch * max_domain_slots + s) - 1
let copy_epoch m = (-m - 1) / max_domain_slots
let copy_slot m = (-m - 1) mod max_domain_slots

(* Gives [proto] the storage of its isolated static variables. *)
let set_isolated_statics proto slots shared =
	proto.pshared <- Array.make (Array.length proto.pfields) false;
	Array.iter (fun i -> proto.pshared.(i) <- true) shared;
	proto.preset <- slots;
	proto.presettable <- Array.make (Array.length proto.pfields) false;
	Array.iter (fun i -> proto.presettable.(i) <- true) slots;
	proto.pdomain <- Array.make max_domain_slots [||];
	proto.pdomain_gen <- Array.make max_domain_slots (-1);
	proto.pdomain_epoch <- Array.make max_domain_slots 0;
	proto.pdomain_seen <- Array.make max_domain_slots 0

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

(* Frozen persistent state (see [Claims.Invalidated]): at the start of a
   batch of typing tasks, the values of the @:persistent static variables are
   frozen, each variable with what it holds (its arrays, objects, instances,
   maps) forming a group, marked on them. A speculating macro reads a frozen
   variable without the claim of the macros; writing the variable or changing
   one of its objects thaws the group (until the next batch). A variable
   holding something whose changes are not seen (a function, which may have
   captured objects, a vector, bytes, a buffer...), or an object of the copy of
   the base of a domain, is not frozen. The variables loaded by a macro are
   frozen once it is loaded. Freezing only marks what no speculating macro can
   read (the variables not frozen), with the claim of the macros or while no
   macro runs; thawing is done with [Claims.mutex]. *)
let group_parent = ref (Array.make 64 0)
let group_frozen = ref (Array.make 64 false)
let group_count = ref 1

let new_group () =
	let g = !group_count in
	if g >= Array.length !group_parent then Mutex.protect Claims.mutex (fun () ->
		let grow a v = let a' = Array.make (2 * Array.length a) v in Array.blit a 0 a' 0 (Array.length a); a' in
		group_parent := grow !group_parent 0;
		group_frozen := grow !group_frozen false
	);
	incr group_count;
	(!group_parent).(g) <- g;
	(!group_frozen).(g) <- false;
	g

let rec group_root g =
	let p = (!group_parent).(g) in
	if p = g then g else group_root p

let is_frozen g = is_group_mark g && (!group_frozen).(group_root g)

exception Unfreezable

(* Marks what [v] holds with group [g]; the frozen groups it shares objects
   with are added to [unions]. *)
let freeze_value g unions v =
	let rec loop v = match v with
		| VNull | VTrue | VFalse | VInt _ | VFloat _ | VString _ | VNativeString _ | VInt64 _ | VUInt64 _ | VPrototype _ ->
			()
		| VEnumValue ev ->
			Array.iter loop ev.eargs
		| VArray a ->
			visit a.amark (fun () -> a.amark <- g) (fun () -> for i = 0 to a.alength - 1 do loop a.avalues.(i) done)
		| VObject o ->
			visit o.omark (fun () -> o.omark <- g) (fun () ->
				Array.iter loop o.ofields;
				match o.oproto with
				| ODictionary d -> IntMap.iter (fun _ v -> loop v) d
				| OProto _ -> ()
			)
		| VInstance vi ->
			let fields () = Array.iter loop vi.ifields in
			let instance f = visit vi.imark (fun () -> vi.imark <- g) (fun () -> fields (); f ()) in
			begin match vi.ikind with
			| INormal -> instance (fun () -> ())
			| IStringMap h -> instance (fun () -> RuntimeStringHashtbl.iter (fun _ (_,v) -> loop v) h)
			| IIntMap h -> instance (fun () -> RuntimeIntHashtbl.iter (fun _ v -> loop v) h)
			| IInt64Map h -> instance (fun () -> RuntimeInt64Hashtbl.iter (fun _ v -> loop v) h)
			| IObjectMap h -> instance (fun () -> Hashtbl.iter (fun k v -> loop k; loop v) h)
			| IPos _ | IDate _ when Array.length vi.ifields = 0 -> ()
			| _ -> raise Unfreezable
			end
		| VVector _ | VFunction _ | VFieldClosure _ | VLazy _ | VHandle _ ->
			raise Unfreezable
	and visit mark set descend =
		if mark = g then ()
		(* an object of the copy of the base of a domain, which changes it *)
		else if mark < 0 then raise Unfreezable
		else if is_frozen mark then unions := mark :: !unions
		else begin set (); descend () end
	in
	loop v

(* Freezes the @:persistent variables of [protos] (an iterator) that are not. *)
let freeze_persistent protos =
	protos (fun proto ->
		if Array.length proto.pshared > 0 then begin
			if Array.length proto.pfrozen = 0 then proto.pfrozen <- Array.make (Array.length proto.pshared) 0;
			Array.iteri (fun i shared ->
				if shared && proto.pfrozen.(i) >= 0 && not (is_frozen proto.pfrozen.(i)) then begin
					let g = new_group () in
					let unions = ref [] in
					match freeze_value g unions proto.pfields.(i) with
					| () ->
						(!group_frozen).(g) <- true;
						List.iter (fun g' -> let r = group_root g' in if r <> g then (!group_parent).(r) <- g) !unions;
						proto.pfrozen.(i) <- g
					| exception Unfreezable ->
						(* what is marked with [g] is not frozen; the variable is
						   not walked again until it is written *)
						proto.pfrozen.(i) <- -1
				end
			) proto.pshared
		end
	)

(* Before changing something of group [g]: the speculation of the current
   domain ends, then the group is thawed if it is frozen. A group already
   thawed is still shared: a speculating macro that read it while it was
   frozen (invalidated since) must not change it without the claim. *)
let frozen_write g =
	!on_shared_state (fun () -> "a frozen value changed");
	let r = group_root g in
	if (!group_frozen).(r) then
		Claims.thaw r (fun () -> (!group_frozen).(r)) (fun () -> (!group_frozen).(r) <- false)

(* The first change of an object of the copy of the base of a domain in an
   expansion journals what it holds. *)
let journal_array a =
	let e = DomainLocal.get current_epoch in
	if e <> 0 then begin
		let m = a.amark in
		let values = ref (Array.sub a.avalues 0 a.alength) and length = ref a.alength and mark = ref m in
		journal_undo (fun () ->
			let values' = a.avalues and length' = a.alength and mark' = a.amark in
			a.avalues <- !values; a.alength <- !length; a.amark <- !mark;
			values := values'; length := length'; mark := mark'
		);
		a.amark <- copy_mark e (copy_slot m)
	end

let journal_object o =
	let e = DomainLocal.get current_epoch in
	if e <> 0 then begin
		let m = o.omark in
		let fields = ref (Array.copy o.ofields) and proto = ref o.oproto and mark = ref m in
		journal_undo (fun () ->
			let fields' = o.ofields and proto' = o.oproto and mark' = o.omark in
			o.ofields <- !fields; o.oproto <- !proto; o.omark <- !mark;
			fields := fields'; proto := proto'; mark := mark'
		);
		o.omark <- copy_mark e (copy_slot m)
	end

(* Swaps the content of hash table [h] with the one of [saved]. *)
let swap_table copy reset iter replace h saved =
	let current = copy h in
	reset h;
	iter (replace h) !saved;
	saved := current

let journal_instance vi =
	let e = DomainLocal.get current_epoch in
	if e <> 0 then begin
		let m = vi.imark in
		let fields = ref (Array.copy vi.ifields) and mark = ref m in
		(* a map is swapped in place: a copy made by [Reflect.copy] shares it *)
		let swap_kind = match vi.ikind with
			| IStringMap h ->
				let saved = ref !h in
				(fun () -> let current = !h in h := !saved; saved := current)
			| IIntMap h ->
				let saved = ref (IntHashtbl.copy h) in
				(fun () -> swap_table IntHashtbl.copy IntHashtbl.reset IntHashtbl.iter IntHashtbl.replace h saved)
			| IInt64Map h ->
				let saved = ref (Int64Hashtbl.copy h) in
				(fun () -> swap_table Int64Hashtbl.copy Int64Hashtbl.reset Int64Hashtbl.iter Int64Hashtbl.replace h saved)
			| IObjectMap h ->
				let h : value ValueHashtbl.t = Obj.magic h in
				let saved = ref (ValueHashtbl.copy h) in
				(fun () -> swap_table ValueHashtbl.copy ValueHashtbl.reset ValueHashtbl.iter ValueHashtbl.replace h saved)
			| _ ->
				(fun () -> ())
		in
		journal_undo (fun () ->
			let fields' = Array.copy vi.ifields and mark' = vi.imark in
			Array.blit !fields 0 vi.ifields 0 (Array.length vi.ifields);
			swap_kind ();
			vi.imark <- !mark;
			fields := fields'; mark := mark'
		);
		vi.imark <- copy_mark e (copy_slot m)
	end

(* Before changing [a], [o] or [vi]: a frozen group is thawed, an object of the
   copy of the base journaled (an object of the base is changed by the
   initialization macros only). *)
let array_barrier a =
	let m = a.amark in
	if m <> 0 then begin
		if m < 0 then begin if copy_epoch m <> DomainLocal.get current_epoch then journal_array a end
		else if m < base_mark then frozen_write m
	end

let object_barrier o =
	let m = o.omark in
	if m <> 0 then begin
		if m < 0 then begin if copy_epoch m <> DomainLocal.get current_epoch then journal_object o end
		else if m < base_mark then frozen_write m
	end

let instance_barrier vi =
	let m = vi.imark in
	if m <> 0 then begin
		if m < 0 then begin if copy_epoch m <> DomainLocal.get current_epoch then journal_instance vi end
		else if m < base_mark then frozen_write m
	end

let array_same a1 a2 =
	let rec loop i = i < 0 || (a1.(i) == a2.(i) && loop (i - 1)) in
	Array.length a1 = Array.length a2 && loop (Array.length a1 - 1)

(* The kind of an instance copied, [f] copying the values it holds. *)
let copy_kind f kind = match kind with
	| IStringMap h ->
		IStringMap (ref (StringMap.map (fun (k,v) -> (k,f v)) !h))
	| IIntMap h ->
		let h' = IntHashtbl.create (IntHashtbl.length h) in
		IntHashtbl.iter (fun k v -> IntHashtbl.replace h' k (f v)) h;
		IIntMap h'
	| IInt64Map h ->
		let h' = Int64Hashtbl.create (Int64Hashtbl.length h) in
		Int64Hashtbl.iter (fun k v -> Int64Hashtbl.replace h' k (f v)) h;
		IInt64Map h'
	| IObjectMap h ->
		let h : value ValueHashtbl.t = Obj.magic h in
		let h' = ValueHashtbl.create (ValueHashtbl.length h) in
		ValueHashtbl.iter (fun k v -> ValueHashtbl.replace h' (f k) (f v)) h;
		IObjectMap (Obj.magic h')
	| IRegex r ->
		(* the state of its last match *)
		IRegex { r with r_string = r.r_string }
	| IBytes b ->
		IBytes (Bytes.copy b)
	| IBuffer b ->
		let b' = Buffer.create (Buffer.length b.bbuffer) in
		Buffer.add_buffer b' b.bbuffer;
		IBuffer { bbuffer = b'; blength = b.blength }
	| IOutput b ->
		let b' = Buffer.create (Buffer.length b) in
		Buffer.add_buffer b' b;
		IOutput b'
	| IAtomicBool a -> IAtomicBool (Atomic.make (Atomic.get a))
	| IAtomicInt a -> IAtomicInt (Atomic.make (Atomic.get a))
	| IAtomicObject a -> IAtomicObject (Atomic.make (f (Atomic.get a)))
	| _ -> kind

(* Whether the changes of an instance of this kind are journaled (copied by
   [copy_kind] otherwise, or shared: a mutex, a thread, a socket...). *)
let journaled_kind = function
	| INormal | IStringMap _ | IIntMap _ | IInt64Map _ | IObjectMap _ | IRegex _ -> true
	| _ -> false

let copied_kind = function
	| IBytes _ | IBuffer _ | IOutput _ | IAtomicBool _ | IAtomicInt _ | IAtomicObject _ -> true
	| k -> journaled_kind k

(* [v] with the objects of the copy of the base of a domain it holds (which
   escaped to the base, or to a @:persistent variable) copied: that domain
   changes them, and undoes it. Only these objects are walked. *)
let export v =
	let copied = ref [] in
	let rec loop v = match v with
		| VArray a when a.amark < 0 ->
			memo (Obj.repr a) (fun () ->
				let a' = { avalues = [||]; alength = 0; amark = 0 } in
				VArray a',(fun () -> a'.avalues <- Array.map loop (Array.sub a.avalues 0 a.alength); a'.alength <- a.alength)
			)
		| VObject o when o.omark < 0 ->
			memo (Obj.repr o) (fun () ->
				let o' = { ofields = [||]; oproto = o.oproto; omark = 0 } in
				VObject o',(fun () ->
					o'.ofields <- Array.map loop o.ofields;
					match o.oproto with
					| ODictionary d -> o'.oproto <- ODictionary (IntMap.map loop d)
					| OProto _ -> ()
				)
			)
		| VInstance vi when vi.imark < 0 ->
			memo (Obj.repr vi) (fun () ->
				let vi' = { ifields = Array.copy vi.ifields; iproto = vi.iproto; ikind = vi.ikind; imark = 0 } in
				VInstance vi',(fun () ->
					Array.iteri (fun i v -> vi'.ifields.(i) <- loop v) vi.ifields;
					vi'.ikind <- copy_kind loop vi.ikind
				)
			)
		| VEnumValue ev when Array.length ev.eargs > 0 ->
			let args = Array.map loop ev.eargs in
			if array_same args ev.eargs then v else VEnumValue { ev with eargs = args }
		| VFieldClosure(v1,f) ->
			let v1' = loop v1 in
			if v1' == v1 then v else VFieldClosure(v1',f)
		| _ ->
			v
	and memo k create = match List.assq_opt k !copied with
		| Some v ->
			v
		| None ->
			let v,fill = create () in
			copied := (k,v) :: !copied;
			fill ();
			v
	in
	loop v

(* The identity of the next object of the base (see [base_mark]). *)
let next_base_id = ref base_mark

(* Seals the base of the isolated variables of [protos] (an iterator): gives
   their objects an identity, by which the copies of the domains are made (see
   [copy_base]), replaces the objects of a copy that escaped to it (see
   [export]), and finds the variables that every expansion copies again (see
   [pvolatile]). Done while no copy of what it walks is made: [full] walks
   everything (the base may have changed in place: the copies are made again,
   see [reseal_statics]), otherwise what has no identity yet (the variables of
   prototypes just initialized). *)
let seal_statics ~full protos =
	let start = !next_base_id in
	(* the objects of the walk holding what is copied again *)
	let volatile_ids = Hashtbl.create 0 in
	let rec seal v = match v with
		| VArray a ->
			if a.amark < 0 then seal (export v)
			else visit a.amark v (fun id -> a.amark <- id) (fun () ->
				let volatile = ref false in
				for i = 0 to a.alength - 1 do
					let v = a.avalues.(i) in
					let v',b = seal v in
					if v' != v then a.avalues.(i) <- v';
					if b then volatile := true
				done;
				!volatile
			)
		| VObject o ->
			if o.omark < 0 then seal (export v)
			else visit o.omark v (fun id -> o.omark <- id) (fun () ->
				let volatile = seal_array o.ofields in
				match o.oproto with
				| ODictionary d ->
					let volatile = ref volatile in
					o.oproto <- ODictionary (IntMap.map (fun v -> let v,b = seal v in if b then volatile := true; v) d);
					!volatile
				| OProto _ ->
					volatile
			)
		| VInstance vi when copied_kind vi.ikind ->
			if vi.imark < 0 then seal (export v)
			else visit vi.imark v (fun id -> vi.imark <- id) (fun () ->
				let volatile = ref (seal_array vi.ifields) in
				let seal_value v = let v,b = seal v in if b then volatile := true; v in
				begin match vi.ikind with
				| IStringMap h ->
					h := StringMap.map (fun (k,v) -> (k,seal_value v)) !h
				| IIntMap h ->
					let l = IntHashtbl.fold (fun k v acc -> (k,v) :: acc) h [] in
					List.iter (fun (k,v) -> let v' = seal_value v in if v' != v then IntHashtbl.replace h k v') l
				| IInt64Map h ->
					let l = Int64Hashtbl.fold (fun k v acc -> (k,v) :: acc) h [] in
					List.iter (fun (k,v) -> let v' = seal_value v in if v' != v then Int64Hashtbl.replace h k v') l
				| IObjectMap h ->
					let h : value ValueHashtbl.t = Obj.magic h in
					let l = ValueHashtbl.fold (fun k v acc -> (k,v) :: acc) h [] in
					List.iter (fun (k,v) ->
						let k' = seal_value k and v' = seal_value v in
						if k' != k then begin ValueHashtbl.remove h k; ValueHashtbl.replace h k' v' end
						else if v' != v then ValueHashtbl.replace h k v'
					) l
				| IAtomicObject a ->
					Atomic.set a (seal_value (Atomic.get a))
				| _ ->
					()
				end;
				!volatile || not (journaled_kind vi.ikind)
			)
		| VVector vv ->
			ignore (seal_array vv);
			v,true
		| VEnumValue ev ->
			v,seal_array ev.eargs
		| VFieldClosure(v1,f) ->
			let v1',b = seal v1 in
			(if v1' == v1 then v else VFieldClosure(v1',f)),b
		| _ ->
			v,false
	and seal_array a =
		let volatile = ref false in
		Array.iteri (fun i v ->
			let v',b = seal v in
			if v' != v then a.(i) <- v';
			if b then volatile := true
		) a;
		!volatile
	and visit m v set descend =
		(* a frozen group: shared by the expansions *)
		if is_group_mark m then v,false
		(* visited by this walk (being walked: not volatile so far) *)
		else if m >= start then v,Hashtbl.mem volatile_ids m
		else if m >= base_mark && not full then v,false
		else begin
			let id = !next_base_id in
			incr next_base_id;
			set id;
			let b = descend () in
			if b then Hashtbl.replace volatile_ids id ();
			v,b
		end
	in
	protos (fun proto ->
		if Array.length proto.preset > 0 then begin
			let volatile = ref [] in
			Array.iter (fun i ->
				let v,b = seal proto.pfields.(i) in
				proto.pfields.(i) <- v;
				if b then volatile := i :: !volatile
			) proto.preset;
			proto.pvolatile <- Array.of_list (List.rev !volatile)
		end
	)

(* The copy of [v] (of the base) for domain slot [s]: an object of the base is
   copied once by [memo] (by its identity), what the expansions share (a
   frozen group, a function, a type...) is not. *)
let rec copy_base s memo v = match v with
	| VArray a ->
		copy_object memo a.amark v (fun () ->
			let a' = { avalues = [||]; alength = a.alength; amark = copy_mark 0 s } in
			VArray a',(fun () -> a'.avalues <- Array.map (copy_base s memo) (Array.sub a.avalues 0 a.alength))
		)
	| VObject o ->
		copy_object memo o.omark v (fun () ->
			let o' = { ofields = [||]; oproto = o.oproto; omark = copy_mark 0 s } in
			VObject o',(fun () ->
				o'.ofields <- Array.map (copy_base s memo) o.ofields;
				match o.oproto with
				| ODictionary d -> o'.oproto <- ODictionary (IntMap.map (copy_base s memo) d)
				| OProto _ -> ()
			)
		)
	| VInstance vi when copied_kind vi.ikind ->
		copy_object memo vi.imark v (fun () ->
			let vi' = { ifields = Array.copy vi.ifields; iproto = vi.iproto; ikind = vi.ikind; imark = copy_mark 0 s } in
			VInstance vi',(fun () ->
				Array.iteri (fun i v -> vi'.ifields.(i) <- copy_base s memo v) vi.ifields;
				vi'.ikind <- copy_kind (copy_base s memo) vi.ikind
			)
		)
	| VVector vv ->
		VVector (Array.map (copy_base s memo) vv)
	| VEnumValue ev when Array.length ev.eargs > 0 ->
		let args = Array.map (copy_base s memo) ev.eargs in
		if array_same args ev.eargs then v else VEnumValue { ev with eargs = args }
	| VFieldClosure(v1,f) ->
		let v1' = copy_base s memo v1 in
		if v1' == v1 then v else VFieldClosure(v1',f)
	| _ ->
		v

and copy_object memo m v create =
	if m >= base_mark then begin match IntHashtbl.find_opt memo m with
		| Some v' ->
			v'
		| None ->
			let v',fill = create () in
			IntHashtbl.add memo m v';
			fill ();
			v'
	end else
		(* a frozen group, shared *)
		v

(* The copies of the objects of the base made for each domain slot, by
   identity, with their generation (only the domain of a slot uses it). *)
let slot_memos = Array.make max_domain_slots (-1,IntHashtbl.create 0)

let slot_memo s gen =
	let (gen',memo) = slot_memos.(s) in
	if gen' = gen then memo
	else begin
		let memo = IntHashtbl.create 0 in
		slot_memos.(s) <- (gen,memo);
		memo
	end

(* Journals the copy of the variables of [proto] of domain slot [s] for
   expansion [e]. *)
let journal_slots proto s e =
	let values = ref (Array.copy proto.pdomain.(s)) and epoch = ref proto.pdomain_epoch.(s) in
	journal_undo (fun () ->
		let values' = proto.pdomain.(s) and epoch' = proto.pdomain_epoch.(s) in
		proto.pdomain.(s) <- !values; proto.pdomain_epoch.(s) <- !epoch;
		values := values'; epoch := epoch'
	);
	proto.pdomain_epoch.(s) <- e

(* The values of the isolated variables of [proto] for expansion [e] (not 0)
   on the current domain: its copy of the base, made on first access. *)
let domain_values proto e =
	let s = Domain.DLS.get domain_slot in
	let gen = Atomic.get base_generation in
	if Array.unsafe_get proto.pdomain_gen s <> gen then begin
		let memo = slot_memo s gen in
		let values = Array.copy proto.pfields in
		Array.iter (fun i -> values.(i) <- copy_base s memo proto.pfields.(i)) proto.preset;
		proto.pdomain.(s) <- values;
		proto.pdomain_gen.(s) <- gen;
		proto.pdomain_epoch.(s) <- 0
	end;
	if proto.pexec <> [] && Array.unsafe_get proto.pdomain_seen s <> e then begin
		proto.pdomain_seen.(s) <- e;
		List.iter (fun r -> r e) proto.pexec
	end;
	if Array.length proto.pvolatile > 0 && Array.unsafe_get proto.pdomain_epoch s <> e then begin
		journal_slots proto s e;
		let memo = IntHashtbl.create 0 in
		let values = proto.pdomain.(s) in
		Array.iter (fun i -> values.(i) <- copy_base s memo proto.pfields.(i)) proto.pvolatile
	end;
	Array.unsafe_get proto.pdomain s

(* Whether the isolated variables of [proto] are read and written in the base
   (outside of an expansion, while an initialization macro runs, or while the
   initializers of [proto] run). *)
let base_access proto e =
	e = 0 || proto.pinitializing || DomainLocal.get base_mode

(* The static variable at slot [i] of [proto]: during an expansion, an
   isolated one is the copy of the domain running it. *)
let get_static proto i =
	if Array.length proto.presettable > 0 && Array.unsafe_get proto.presettable i then begin
		let e = DomainLocal.get current_epoch in
		if base_access proto e then proto.pfields.(i) else Array.unsafe_get (domain_values proto e) i
	end else
		proto.pfields.(i)

let set_static proto i v =
	if Array.length proto.presettable > 0 && Array.unsafe_get proto.presettable i then begin
		let e = DomainLocal.get current_epoch in
		if base_access proto e then proto.pfields.(i) <- v
		else begin
			let values = domain_values proto e in
			let s = Domain.DLS.get domain_slot in
			if Array.unsafe_get proto.pdomain_epoch s <> e then journal_slots proto s e;
			Array.unsafe_set values i v
		end
	end else if is_shared_static proto i then
		proto.pfields.(i) <- export v
	else
		proto.pfields.(i) <- v

let frozen_group proto i = if i < Array.length proto.pfrozen then max 0 proto.pfrozen.(i) else 0

(* Before reading the static variable at shared slot [i] of [proto]. *)
let shared_static_read proto i =
	let g = frozen_group proto i in
	if not (g <> 0 && let r = group_root g in Claims.read_frozen r (fun () -> (!group_frozen).(r))) then
		!on_shared_state (fun () -> static_name proto i)

(* Before writing (or reading and writing) it. *)
let shared_static_write proto i =
	!on_shared_state (fun () -> static_name proto i);
	let g = frozen_group proto i in
	if g <> 0 then frozen_write g;
	if i < Array.length proto.pfrozen then proto.pfrozen.(i) <- 0

(* The same, by name. *)
let shared_static_index proto name =
	match IntMap.find_opt name proto.pnames with
	| Some i -> i
	| None -> -1

let shared_static_name_read proto name =
	let i = shared_static_index proto name in
	if i >= 0 then shared_static_read proto i else !on_shared_state (fun () -> static_name proto i)

let shared_static_name_write proto name =
	let i = shared_static_index proto name in
	if i >= 0 then shared_static_write proto i else !on_shared_state (fun () -> static_name proto i)

(* Runs [f] as an expansion of its own, from the base: the changes of the
   expansion running it (if any) are set aside, its own changes undone when
   it ends. *)
let with_expansion f =
	let old_epoch = DomainLocal.get current_epoch and caller = DomainLocal.get journal in
	List.iter (fun swap -> swap ()) caller;
	DomainLocal.set current_epoch (Atomic.fetch_and_add epoch_counter 1 + 1);
	DomainLocal.set journal [];
	Std.finally (fun () ->
		List.iter (fun swap -> swap ()) (DomainLocal.get journal);
		List.iter (fun swap -> swap ()) (List.rev caller);
		DomainLocal.set current_epoch old_epoch;
		DomainLocal.set journal caller
	) f ()

(* Runs [f] changing the base (see [base_mode]). *)
let with_base_mode f =
	let old = DomainLocal.get base_mode in
	DomainLocal.set base_mode true;
	Std.finally (fun () -> DomainLocal.set base_mode old) f ()

(* Seals the whole base again once it may have changed in place (see
   [seal_statics]): the copies of the domains are made again. *)
let reseal_statics protos =
	Atomic.incr base_generation;
	seal_statics ~full:true protos

(* Runs [f], the initializers of the static variables of the new prototypes
   [protos], which give their base. Once the initialization macros are done,
   copies of the base are made: [f] runs as an expansion, whose changes to the
   variables of the other prototypes (a registry...) are undone. *)
let initialize_statics protos f =
	if !init_phase_done && not (DomainLocal.get base_mode) then begin
		List.iter (fun proto -> proto.pinitializing <- true) protos;
		Std.finally (fun () -> List.iter (fun proto -> proto.pinitializing <- false) protos) with_expansion f
	end else
		f ();
	seal_statics ~full:false (fun g -> List.iter g protos)

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