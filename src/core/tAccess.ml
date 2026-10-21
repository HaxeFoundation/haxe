(*
	Accessors of the contents of types, grouped by the query that computes them
	(docs/incremental-compilation.md, phase 3.2). Reading through an accessor
	demands the query: the header or the members of a type are computed if they
	are not yet. Typing code reads the contents of other types through them;
	the query implementations (the typeload modules) and the code running
	after typing (filters, generators) read the fields directly.
*)
open TType

(* The @:using metadata of a module type, resolved when the type's usings are
   first read (or at the end of the structure stage, whichever comes first). *)
let pending_usings : (Globals.path,(tinfos * (unit -> unit)) list) Hashtbl.t = Hashtbl.create 0

let set_pending_usings (ti : tinfos) f =
	let l = try Hashtbl.find pending_usings ti.mt_path with Not_found -> [] in
	Hashtbl.replace pending_usings ti.mt_path ((ti,f) :: l)

let force_usings (ti : tinfos) = if Hashtbl.length pending_usings > 0 then TypingLock.protect @@ fun () ->
	match Hashtbl.find_opt pending_usings ti.mt_path with
	| None ->
		()
	| Some l ->
		match List.partition (fun (ti',_) -> ti' == ti) l with
		| [],_ ->
			()
		| fl,rest ->
			if rest = [] then Hashtbl.remove pending_usings ti.mt_path else Hashtbl.replace pending_usings ti.mt_path rest;
			List.iter (fun (_,f) -> f ()) fl

(* The final states of the queries: once a closure is one of these, reading the
   contents needs no lock (phase 3.5). *)
let built () = Built
let header_done () = ()
let module_complete () = ()

(* Members of a module type read from a .hxb file are read with the rest of
   its module. The closures are read under the lock: another domain may change
   them meanwhile. *)
let complete_module m =
	if m.m_extra.m_complete != module_complete then
		TypingLock.protect (fun () -> m.m_extra.m_complete ())

(* While typing on several domains, classes are built under the typing lock:
   building them in parallel made build cycles split between domains, whose
   outcome depends on timing (phase 3.5). A build can still release the lock
   while it waits for another domain: a domain that then needs the class waits
   for the build to end instead of reading it partially. *)
(* the classes being built, with their builder and when the build started: in
   a cycle of builds split between domains, the build that started first is
   read partially, as the outermost build of a cycle on one domain is *)
let builders : (tclass * (Domain.id * int)) list ref = ref []
let build_seq = ref 0

let builder_of c =
	let rec loop = function
		| [] -> None
		| (c',d) :: l -> if c' == c then Some d else loop l
	in
	loop !builders

let build_started c =
	if !TypingLock.active then
		Mutex.protect Claims.mutex (fun () ->
			incr build_seq;
			builders := (c,(Domain.self (),!build_seq)) :: !builders
		)

let build_done c =
	if !TypingLock.active || !builders <> [] then
		Mutex.protect Claims.mutex (fun () ->
			builders := List.filter (fun (c',_) -> c' != c) !builders;
			Condition.broadcast Claims.done_
		)

let with_class_claim c f =
	if not !TypingLock.active then
		f ()
	else begin
		let me = Domain.self () in
		let rec go () =
			let r = TypingLock.protect (fun () ->
				match Mutex.protect Claims.mutex (fun () -> builder_of c) with
				| Some (d,seq) when d <> me -> `Wait (d,seq)
				| _ -> `Done (f ())
			) in
			match r with
			| `Done v ->
				v
			| `Wait (owner,seq) ->
				let built = TypingLock.without (fun () -> Mutex.protect Claims.mutex (fun () ->
					Claims.wait ~kind:Claims.WaitClass ~seq ~owner (fun () -> builder_of c = None)
				)) in
				if built then go () else TypingLock.protect f
		in
		(* a build may run macros: it starts with the claim of the macros, so
		   that a macro running on another domain gets the class built itself
		   instead of finding it started (the order of the macros then is the
		   one of a single domain) *)
		Claims.in_order ();
		Claims.with_macro_claim go
	end

module Header = struct
	(* The header of a class: type parameters, super class, interfaces,
	   declared metadata. *)
	let demand c =
		if c.cl_build_header != header_done then
			with_class_claim c (fun () -> c.cl_build_header ())

	let super c = demand c; c.cl_super
	let implements c = demand c; c.cl_implements
end

(* Classes whose module is being created: their build is not set up yet, and
   reading their members gives what is there (nothing). *)
let not_ready : tclass list ref = ref []

(* Builds class [c] (its [members] query) and gives its build state. *)
let build c =
	if c.cl_build == built then Built
	else with_class_claim c (fun () -> c.cl_build ())

module Members = struct
	(* The members of a class, after its build macros, connected. *)
	let demand c =
		if c.cl_build != built then
			with_class_claim c (fun () -> if not (List.memq c !not_ready) then ignore (c.cl_build ()))

	let fields c = demand c; c.cl_fields
	let ordered_fields c = demand c; c.cl_ordered_fields
	let statics c = demand c; c.cl_statics
	let ordered_statics c = demand c; c.cl_ordered_statics
	let constructor c = demand c; c.cl_constructor

	let find_field c name = PMap.find name (fields c)
	let find_static c name = PMap.find name (statics c)

	(* The constructors of an enum. *)
	let enum_constructors e = complete_module e.e_module; e.e_constrs
	let enum_names e = complete_module e.e_module; e.e_names
	let find_enum_constructor e name = PMap.find name (enum_constructors e)

	(* The members of an abstract: its implementation class, casts and operators. *)
	let abstract a =
		complete_module a.a_module;
		(match a.a_impl with Some c -> demand c | None -> ());
		a
end
