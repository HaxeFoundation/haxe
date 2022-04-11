
open Type
open CompilationCache

class gc_task (max_working_memory : float) (heap_size : float) = object(self)
	inherit server_task ["gc"] 100

	method private execute =
		let t0 = Timer.get_time() in
		let stats = Gc.stat() in
		let live_words = float_of_int stats.live_words in
		(* Maximum heap size needed for the last X compilations = sum of what's live + max working memory. *)
		let needed_max = live_words +. max_working_memory in
		(* Additional heap percentage needed = what's live / max of what was live. *)
		let percent_needed = (1. -. live_words /. needed_max) in
		(* Effective cache size percentage = what's live / heap size. *)
		let percent_used = live_words /. heap_size in
		(* Set allowed space_overhead to the maximum of what we needed during the last X compilations. *)
		let new_space_overhead = int_of_float ((percent_needed +. 0.05) *. 100.) in
		let old_gc = Gc.get() in
		Gc.set { old_gc with Gc.space_overhead = new_space_overhead; };
		(* Compact if less than 80% of our heap words consist of the cache and there's less than 50% overhead. *)
		let do_compact = percent_used < 0.8 && percent_needed < 0.5 in
		begin if do_compact then
			Gc.compact()
		else
			Gc.full_major();
		end;
		Gc.set old_gc;
		ServerMessage.gc_stats (Timer.get_time() -. t0) stats do_compact new_space_overhead
end

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