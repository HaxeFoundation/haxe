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

module Header = struct
	(* The header of a class: type parameters, super class, interfaces,
	   declared metadata. *)
	let demand c =
		if c.cl_build_header != header_done then
			TypingLock.protect (fun () -> c.cl_build_header ())

	let super c = demand c; c.cl_super
	let implements c = demand c; c.cl_implements
end

(* Classes whose module is being created: their build is not set up yet, and
   reading their members gives what is there (nothing). *)
let not_ready : tclass list ref = ref []

(* Builds class [c] (its [members] query) and gives its build state. *)
let build c =
	if c.cl_build == built then Built
	else TypingLock.protect (fun () -> c.cl_build ())

module Members = struct
	(* The members of a class, after its build macros, connected. *)
	let demand c =
		if c.cl_build != built then
			TypingLock.protect (fun () -> if not (List.memq c !not_ready) then ignore (c.cl_build ()))

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
