open Globals
open Common
open Timer
open CompilationCache

type t = {
	(* If true, prints some debug information *)
	verbose : bool;
	(* The list of changed directories per-signature *)
	changed_directories : (Digest.t,cached_directory list) Hashtbl.t;
	(* A reference to the compilation server instance *)
	cs : CompilationCache.t;
	(* A list of class paths per-signature *)
	class_paths : (Digest.t,string list) Hashtbl.t;
	(* Increased for each compilation *)
	mutable compilation_step : int;
	(* A list of delays which are run after compilation *)
	mutable delays : (unit -> unit) list;
	(* True if it's an actual compilation, false if it's a display operation *)
	mutable was_compilation : bool;
	(* True if the macro context has been set up *)
	mutable macro_context_setup : bool;
}

let create verbose = {
	verbose = verbose;
	cs = new CompilationCache.cache;
	class_paths = Hashtbl.create 0;
	changed_directories = Hashtbl.create 0;
	compilation_step = 0;
	delays = [];
	was_compilation = false;
	macro_context_setup = false;
}

let add_delay sctx f =
	sctx.delays <- f :: sctx.delays

let run_delays sctx =
	let fl = sctx.delays in
	sctx.delays <- [];
	List.iter (fun f -> f()) fl

(* Resets the state for a new compilation *)
let reset sctx =
	Hashtbl.clear sctx.changed_directories;
	sctx.was_compilation <- false;
	Parser.reset_state();
	return_partial_type := false;
	measure_times := false;
	Hashtbl.clear DeprecationCheck.warned_positions;
	close_times();
	stats.s_files_parsed := 0;
	stats.s_classes_built := 0;
	stats.s_methods_typed := 0;
	stats.s_macros_called := 0;
	Hashtbl.clear Timer.htimers;
	Helper.start_time := get_time()

let maybe_cache_context sctx com =
	if com.display.dms_full_typing then begin
		CommonCache.cache_context sctx.cs com;
		ServerMessage.cached_modules com "" (List.length com.modules);
	end

let ensure_macro_setup sctx =
	if not sctx.macro_context_setup then begin
		sctx.macro_context_setup <- true;
		MacroContext.setup();
	end

let cleanup () = match !MacroContext.macro_interp_cache with
	| Some interp -> EvalContext.GlobalState.cleanup interp
	| None -> ()