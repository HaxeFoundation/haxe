open Globals
open CompilationCache


type t = {
	version : Globals.compiler_version;
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
	(* Stdin content for the current display request *)
	mutable current_stdin : string option;
	(* Persistent working directory set via server/setCwd, applied before each request *)
	mutable persistent_cwd : string option;
	(* The server's domain pool. *)
	pool : Parallel.ManagedPool.t;
}

let create_version () =
	{
		version = version;
		major = version_major;
		minor = version_minor;
		revision = version_revision;
		pre = version_pre;
		extra = Version.version_extra;
	}

let create verbose =
	let pool = Parallel.ManagedPool.create (fun () -> Domainslib.Task.setup_pool ~num_domains:(Domain.recommended_domain_count() - 1) ()) in
	{
		version = create_version ();
		verbose;
		cs = new CompilationCache.cache;
		class_paths = Hashtbl.create 0;
		changed_directories = Hashtbl.create 0;
		compilation_step = 0;
		delays = [];
		was_compilation = false;
		macro_context_setup = false;
		current_stdin = None;
		persistent_cwd = None;
		pool;
	}

let dispose sctx =
	Parallel.ManagedPool.release sctx.pool

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
	Parser.reset_state()