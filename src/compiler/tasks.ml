
open Type
open CompilationCache

class class_maintenance_task (cs : CompilationCache.t) (c : tclass) = object(self)
	inherit server_task ["module maintenance"] 70

	method private execute =
		let rec field cf =
			(* Unset cf_expr. This holds the optimized version for generators, which we don't need to persist. If
				we compile again, the semi-optimized expression will be restored by calling cl_restore(). *)
			cf.cf_expr <- None;
			List.iter field cf.cf_overloads
		in
		(* What we're doing here at the moment is free, so we can just do it in one task. If this ever gets more expensive,
			we should spawn a task per-field. *)
		List.iter field c.cl_ordered_fields;
		List.iter field c.cl_ordered_statics;
		Option.may field c.cl_constructor;
end

class module_maintenance_task (cs : CompilationCache.t) (m : module_def) = object(self)
	inherit server_task ["module maintenance"] 80

	method private execute =
		List.iter (fun mt -> match mt with
			| TClassDecl c ->
				cs#add_task (new class_maintenance_task cs c)
			| _ ->
				()
		) m.m_types
end

class server_exploration_task (cs : CompilationCache.t) = object(self)
	inherit server_task ["server explore"] 90

	method private execute =
		cs#iter_modules (fun m -> cs#add_task (new module_maintenance_task cs m))
end
