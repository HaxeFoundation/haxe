(*
	Accessors of the contents of types, grouped by the query that computes them
	(docs/incremental-compilation.md, phase 3.2). Reading through an accessor
	demands the query: the header or the members of a type are computed if they
	are not yet. Typing code reads the contents of other types through them;
	the query implementations (the typeload modules) and the code running
	after typing (filters, generators) read the fields directly.
*)
open TType

(* Members of a module type read from a .hxb file are read with the rest of
   its module. *)
let complete_module m = m.m_extra.m_complete ()

module Header = struct
	(* The header of a class: type parameters, super class, interfaces,
	   declared metadata. *)
	let demand c = c.cl_build_header ()

	let super c = demand c; c.cl_super
	let implements c = demand c; c.cl_implements
end

(* Classes whose module is being created: their build is not set up yet, and
   reading their members gives what is there (nothing). *)
let not_ready : tclass list ref = ref []

module Members = struct
	(* The members of a class, after its build macros, connected. *)
	let demand c = if not (List.memq c !not_ready) then ignore (c.cl_build ())

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
