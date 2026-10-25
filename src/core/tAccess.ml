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

let force_usings (ti : tinfos) = if Hashtbl.length pending_usings > 0 then TypingLock.protect ~site:"usings" @@ fun () ->
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
		TypingLock.protect ~site:"complete module" (fun () -> m.m_extra.m_complete ())

(* While typing on several domains, a class is built (and its header computed)
   by the domain that claims it, without the typing lock (phase 3.5): a domain
   that needs a class another domain is building waits for the build to end
   instead of reading it partially. *)
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
			if builder_of c = None then begin
				incr build_seq;
				builders := (c,(Domain.self (),!build_seq)) :: !builders
			end
		)

let build_done c =
	if !TypingLock.active || !builders <> [] then
		Mutex.protect Claims.mutex (fun () ->
			builders := List.filter (fun (c',_) -> c' != c) !builders;
			Condition.broadcast Claims.done_
		)

(* Runs [f] (a build or the header of class [c]) without the typing lock: the
   domain that claims [c] runs it, the others wait for it to end. *)
let with_unlocked_claim ~is_done c f =
	let me = Domain.self () in
	let rec go () =
		let r = Mutex.protect Claims.mutex (fun () ->
			match builder_of c with
			| Some (d,seq) when d <> me ->
				`Wait (d,seq)
			| Some _ ->
				`Run
			| None when is_done () ->
				`Run
			| None ->
				incr build_seq;
				builders := (c,(me,!build_seq)) :: !builders;
				`Claimed
		) in
		match r with
		| `Run ->
			f ()
		| `Claimed ->
			Std.finally (fun () -> build_done c) f ()
		| `Wait (owner,seq) ->
			let built = TypingLock.without (fun () -> Mutex.protect Claims.mutex (fun () ->
				Claims.wait ~kind:Claims.WaitClass ~seq ~owner (fun () -> builder_of c = None)
			)) in
			if built then go () else f ()
	in
	go ()

module Header = struct
	(* The header of a class: type parameters, super class, interfaces,
	   declared metadata. *)
	let demand c =
		if !DepRecord.enabled then DepRecord.record_type c.cl_path "header";
		if c.cl_build_header != header_done then begin
			if !TypingLock.active then
				with_unlocked_claim ~is_done:(fun () -> c.cl_build_header == header_done) c (fun () -> c.cl_build_header ())
			else
				c.cl_build_header ()
		end

	let super c = demand c; c.cl_super
	let implements c = demand c; c.cl_implements
	(* set with the interfaces: [ArrayAccess<T>] *)
	let array_access c = demand c; c.cl_array_access
end

(* Classes whose module is being created: their build is not set up yet, and
   reading their members gives what is there (nothing). *)
let not_ready : tclass list ref = ref []

let with_unlocked_build c f = with_unlocked_claim ~is_done:(fun () -> c.cl_build == built) c f

let with_build_claim c f =
	if not !TypingLock.active then f ()
	else with_unlocked_build c f

let build_state c =
	if c.cl_build == built then Built
	else with_build_claim c (fun () -> c.cl_build ())

(* Builds class [c] (its [members] query) and gives its build state: the
   caller reads its members. *)
let build c =
	if !DepRecord.enabled then DepRecord.record_type c.cl_path "members";
	build_state c

(* Builds class [c] for a caller that reads its members through [Members] only
   (a field found or not by name, all of them), which records what it reads:
   building reads the header (an instance of the class in a type). *)
let build_header c =
	if !DepRecord.enabled then DepRecord.record_type c.cl_path "header";
	build_state c

module Members = struct
	(* The members of a class, after its build macros, connected. *)
	let demand c =
		if !DepRecord.enabled then DepRecord.record_type c.cl_path "members";
		if c.cl_build != built then
			with_build_claim c (fun () -> if not (List.memq c !not_ready) then ignore (c.cl_build ()))

	(* all the fields of a class: their signatures are read too (the
	   fingerprint of the members does not include them) *)
	let all_sigs c l =
		if !DepRecord.enabled then List.iter (fun cf -> DepRecord.record_field c cf "sig") l

	let fields c = demand c; all_sigs c c.cl_ordered_fields; c.cl_fields
	let ordered_fields c = demand c; all_sigs c c.cl_ordered_fields; c.cl_ordered_fields
	let statics c = demand c; all_sigs c c.cl_ordered_statics; c.cl_statics
	let ordered_statics c = demand c; all_sigs c c.cl_ordered_statics; c.cl_ordered_statics
	(* the constructor reads its signature, or that the class has none (one
	   added would be found) *)
	let constructor c =
		if c.cl_build != built then
			with_build_claim c (fun () -> if not (List.memq c !not_ready) then ignore (c.cl_build ()));
		if !DepRecord.enabled then (match c.cl_constructor with
			| Some cf -> DepRecord.record_field c cf "sig"
			| None -> DepRecord.record_absent c.cl_path "new" false);
		c.cl_constructor

	(* a field found by name: its signature is read *)
	let found c cf =
		if !DepRecord.enabled then DepRecord.record_field c cf "sig";
		cf

	(* a field found by name reads its signature only; a field not found reads
	   that the class has no field of that name (one added would be found) *)
	let find_in c map static name =
		if c.cl_build != built then
			with_build_claim c (fun () -> if not (List.memq c !not_ready) then ignore (c.cl_build ()));
		match PMap.find name (map c) with
		| cf -> found c cf
		| exception Not_found -> if !DepRecord.enabled then DepRecord.record_absent c.cl_path name static; raise Not_found

	let find_field c name = find_in c (fun c -> c.cl_fields) false name
	let find_static c name = find_in c (fun c -> c.cl_statics) true name

	(* The constructors of an enum. *)
	let enum_constructors e = if !DepRecord.enabled then DepRecord.record_type e.e_path "members"; complete_module e.e_module; e.e_constrs
	let enum_names e = if !DepRecord.enabled then DepRecord.record_type e.e_path "members"; complete_module e.e_module; e.e_names
	let find_enum_constructor e name = PMap.find name (enum_constructors e)

	(* The members of an abstract: its implementation class, casts and operators. *)
	let abstract a =
		if !DepRecord.enabled then DepRecord.record_type a.a_path "members";
		complete_module a.a_module;
		(match a.a_impl with Some c -> demand c | None -> ());
		a
end
