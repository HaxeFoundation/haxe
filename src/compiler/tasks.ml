
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

(* Idle-time incremental major GC.

   The worker domain drains one server_task per loop turn and re-checks the request queue *before* each
   turn (server.ml), so a task is the unit of preemption. We exploit that: each run does exactly ONE
   bounded [Gc.major_slice] and re-enqueues itself while a full major cycle has not completed since the
   drain started. An incoming request therefore preempts the collection at slice granularity (worst-case
   added latency = one slice), and a half-finished major cycle is safe to leave and resume — there is
   nothing to abort. Priority 100 keeps it behind the maintenance tasks (70/80/90), which drop cf_expr
   and thus produce the garbage this then reclaims.

   Never call Gc.full_major/Gc.major/Gc.compact here: each is a single uninterruptible call that would
   block the worker (and any queued request) for its whole duration — the exact stall we are avoiding. *)
type gc_drain_state = {
	gcd_slice_words : int;      (* words passed to Gc.major_slice (latency knob) *)
	gcd_target : int;           (* major_collections value that ends the drain *)
	gcd_started : float;
	gcd_heap0 : int;            (* heap_words when the drain started *)
	gcd_top0 : int;             (* top_heap_words when the drain started *)
	mutable gcd_slices : int;
	mutable gcd_slice_time : float;
}

let gc_words_to_mb w =
	float_of_int w *. float_of_int (Sys.word_size / 8) /. 1048576.

class gc_slice_task (cs : CompilationCache.t) (st : gc_drain_state) = object(self)
	inherit server_task ["gc"] 100

	method private execute =
		(* quick_stat is O(1) — never Gc.stat here, it forces a full major. *)
		let qs = Gc.quick_stat () in
		if qs.Gc.major_collections >= st.gcd_target then begin
			let total = Extc.time () -. st.gcd_started in
			ServerMessage.gc_task (Printf.sprintf
				"idle collection done: %d slice(s), %.1fms slicing / %.1fms wall, %d cycle(s), heap %.1fMB (peak %.1fMB)"
				st.gcd_slices (st.gcd_slice_time *. 1000.) (total *. 1000.)
				(qs.Gc.major_collections - (st.gcd_target - 1))
				(gc_words_to_mb qs.Gc.heap_words) (gc_words_to_mb qs.Gc.top_heap_words))
		end else begin
			let t0 = Extc.time () in
			ignore (Gc.major_slice st.gcd_slice_words);
			let dt = Extc.time () -. t0 in
			st.gcd_slices <- st.gcd_slices + 1;
			st.gcd_slice_time <- st.gcd_slice_time +. dt;
			cs#add_task (new gc_slice_task cs st)
		end
end

(* Kick off one idle collection: drive slices until one full major cycle completes. *)
let schedule_gc_drain (cs : CompilationCache.t) (slice_words : int) =
	let qs = Gc.quick_stat () in
	let st = {
		gcd_slice_words = slice_words;
		gcd_target = qs.Gc.major_collections + 1;
		gcd_started = Extc.time ();
		gcd_heap0 = qs.Gc.heap_words;
		gcd_top0 = qs.Gc.top_heap_words;
		gcd_slices = 0;
		gcd_slice_time = 0.;
	} in
	ServerMessage.gc_task (Printf.sprintf "idle collection start: heap %.1fMB (peak %.1fMB), slice %d words"
		(gc_words_to_mb st.gcd_heap0) (gc_words_to_mb st.gcd_top0) slice_words);
	cs#add_task (new gc_slice_task cs st)
