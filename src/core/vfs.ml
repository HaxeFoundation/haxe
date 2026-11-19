(*
	File system view of the compilation server: file information and directory
	listings kept across compilations, invalidated by file system events, so
	that a compilation without changes does not query the file system again.

	Disabled by default: every function then goes to the file system. The
	server enables it with HAXE_VFS=watch.
*)

type kind =
	| File
	| Dir
	| Missing

type info = {
	kind : kind;
	mtime : float;
	size : int;
}

type mode =
	| Off
	| Watched

let mode = ref Off

let mutex = Mutex.create ()

(* Entries are keyed by absolute paths, lowercase with forward slashes on
   Windows, so that the names given by file system events find them. *)
let infos : (string,info) Hashtbl.t = Hashtbl.create 0
let listings : (string,string array option) Hashtbl.t = Hashtbl.create 0
(* The keys of the entries of each directory, to invalidate a directory
   without going through all the entries. *)
let children : (string,(string,unit) Hashtbl.t) Hashtbl.t = Hashtbl.create 0

(* Contents of unsaved editor buffers, which take precedence over the files.
   Their modification time is negative and decreases on every change. *)
let overlays : (string,string * string * float) Hashtbl.t = Hashtbl.create 0
let overlay_clock = ref 0.

let syscalls = Atomic.make 0
let last_compilation_syscalls = Atomic.make 0

let trace_syscalls = Sys.getenv_opt "HAXE_VFS_TRACE" <> None

let count_syscall what path =
	if trace_syscalls then prerr_endline ("vfs: " ^ what ^ " " ^ path);
	Atomic.incr syscalls

let full_path path =
	let path = Path.get_full_path path in
	let len = String.length path in
	if len > 1 && (path.[len - 1] = '/' || path.[len - 1] = '\\') && not (len = 3 && path.[1] = ':') then String.sub path 0 (len - 1) else path

let key_of_full path =
	if Globals.is_windows then String.lowercase_ascii (String.map (fun c -> if c = '\\' then '/' else c) path) else path

(* The roots are "/" and "c:/". *)
let parent_key key =
	let len = String.length key in
	match String.rindex_opt key '/' with
	| Some i when i = len - 1 -> None
	| Some 0 -> Some "/"
	| Some 2 when key.[1] = ':' -> Some (String.sub key 0 3)
	| Some i -> Some (String.sub key 0 i)
	| None -> None

let child_key key name =
	if key.[String.length key - 1] = '/' then key ^ name else key ^ "/" ^ name

let add_child_unlocked key =
	match parent_key key with
	| Some parent ->
		let set = match Hashtbl.find_opt children parent with
			| Some set -> set
			| None -> let set = Hashtbl.create 8 in Hashtbl.replace children parent set; set
		in
		Hashtbl.replace set key ()
	| None ->
		()

let rec invalidate_contents_unlocked key =
	match Hashtbl.find_opt children key with
	| Some set ->
		Hashtbl.remove children key;
		Hashtbl.iter (fun child () ->
			Hashtbl.remove infos child;
			Hashtbl.remove listings child;
			invalidate_contents_unlocked child
		) set
	| None ->
		()

(* Drops [key], everything below it, and the information and listing of its
   parent directory. *)
let invalidate_unlocked key =
	invalidate_contents_unlocked key;
	Hashtbl.remove infos key;
	Hashtbl.remove listings key;
	match parent_key key with
	| Some parent ->
		Hashtbl.remove infos parent;
		Hashtbl.remove listings parent
	| None ->
		()

let invalidate path =
	if !mode = Watched then begin
		let key = key_of_full (full_path path) in
		Mutex.protect mutex (fun () -> invalidate_unlocked key)
	end

let () = Path.on_directory_created := invalidate

(* The working directory seen by macros (Sys.setCwd, Sys.getCwd, their file
   accesses and processes): the compiler keeps the one of the compilation. *)
let macro_cwd : string option Atomic.t = Atomic.make None
let cwd_mutex = Mutex.create ()

let getcwd () =
	match Atomic.get macro_cwd with
	| Some dir -> dir
	| None -> Unix.getcwd ()

(* [path] for the file accesses of macros. *)
let resolve path =
	match Atomic.get macro_cwd with
	| Some dir when Filename.is_relative path -> Filename.concat dir path
	| _ -> path

let setcwd dir =
	let dir = resolve dir in
	if not (Sys.file_exists dir && Sys.is_directory dir) then raise (Unix.Unix_error (Unix.ENOENT,"chdir",dir));
	Atomic.set macro_cwd (Some (full_path dir))

let reset_cwd () =
	Atomic.set macro_cwd None

(* Runs [f], which starts a process, in the working directory of macros. *)
let with_cwd f =
	match Atomic.get macro_cwd with
	| None ->
		f ()
	| Some dir ->
		Mutex.protect cwd_mutex (fun () ->
			let old = Unix.getcwd () in
			Unix.chdir dir;
			Std.finally (fun () -> Unix.chdir old) f ()
		)

(* libuv is not thread-safe: watches are started and events processed at the
   beginning of a compilation only, the directories to watch are queued
   meanwhile. *)
module Watcher = struct
	(* FSEvents delivers events with a latency: the directories are checked
	   again on every compilation on macOS, as unwatchable ones. *)
	let is_macos = Sys.os_type = "Unix" && Sys.file_exists "/System/Library/CoreServices/SystemVersion.plist"

	let loop = lazy (if is_macos then None else match Luv.Loop.init () with
		| Ok loop -> Some loop
		| Error _ -> None
	)
	let watched : (string,Luv.FS_event.t) Hashtbl.t = Hashtbl.create 0
	let unwatchable : (string,unit) Hashtbl.t = Hashtbl.create 0
	let pending : (string,string) Hashtbl.t = Hashtbl.create 0

	let request_unlocked key path =
		if not (Hashtbl.mem watched key) && not (Hashtbl.mem unwatchable key) then
			Hashtbl.replace pending key path

	let on_event key result =
		Mutex.protect mutex (fun () ->
			match result with
			| Ok (Some name,events) when not (List.mem `RENAME events) ->
				(* A modification: the listing of the directory is the same. *)
				Hashtbl.remove infos (child_key key (key_of_full name))
			| Ok (Some name,_) ->
				invalidate_unlocked (child_key key (key_of_full name))
			| Ok (None,_) ->
				invalidate_unlocked key
			| Error _ ->
				Hashtbl.replace unwatchable key ();
				invalidate_unlocked key
		)

	(* Starts the pending watches, then processes the events received since the
	   previous compilation. The entries of a directory read before its watch
	   started are dropped, as a change may have been missed in between. *)
	let update () =
		let dirs = Mutex.protect mutex (fun () ->
			let dirs = Hashtbl.fold (fun key path acc -> (key,path) :: acc) pending [] in
			Hashtbl.reset pending;
			dirs
		) in
		match Stdlib.Lazy.force loop with
		| None ->
			Mutex.protect mutex (fun () -> List.iter (fun (key,_) -> Hashtbl.replace unwatchable key ()) dirs)
		| Some loop ->
			List.iter (fun (key,path) ->
				match Luv.FS_event.init ~loop () with
				| Ok handle ->
					Luv.FS_event.start handle path (on_event key);
					Mutex.protect mutex (fun () ->
						Hashtbl.replace watched key handle;
						invalidate_contents_unlocked key;
						Hashtbl.remove listings key
					)
				| Error _ ->
					Mutex.protect mutex (fun () ->
						Hashtbl.replace unwatchable key ();
						invalidate_unlocked key
					)
			) dirs;
			ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ());
			ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ())

	let revalidate_unwatchable () =
		Mutex.protect mutex (fun () ->
			Hashtbl.iter (fun key () -> invalidate_unlocked key) unwatchable
		)
end

let begin_compilation () =
	Atomic.set syscalls 0;
	if !mode = Watched then begin
		Watcher.update ();
		Watcher.revalidate_unwatchable ()
	end

let end_compilation () =
	Atomic.set last_compilation_syscalls (Atomic.get syscalls)

let stat_uncached path =
	count_syscall "stat" path;
	try
		let st = Unix.stat path in
		let kind = match st.Unix.st_kind with Unix.S_DIR -> Dir | _ -> File in
		{ kind; mtime = Extc.filetime path; size = st.Unix.st_size }
	with Unix.Unix_error _ ->
		{ kind = Missing; mtime = 0.; size = 0 }

let find_overlay key =
	if Hashtbl.length overlays = 0 then None else Mutex.protect mutex (fun () -> Hashtbl.find_opt overlays key)

let rec stat_full path key =
	match find_overlay key with
	| Some (_,content,mtime) ->
		{ kind = File; mtime; size = String.length content }
	| None when !mode = Off ->
		stat_uncached path
	| None ->
		match Mutex.protect mutex (fun () -> Hashtbl.find_opt infos key) with
		| Some info ->
			info
		| None ->
			let info = stat_uncached path in
			(* The watch of the parent directory covers the changes of this entry. *)
			let parent = match parent_key key with
				| Some parent_key ->
					let parent = Filename.dirname path in
					if (stat_full parent parent_key).kind = Dir then Some (parent_key,parent) else None
				| None ->
					None
			in
			Mutex.protect mutex (fun () ->
				Option.may (fun (parent_key,parent) -> Watcher.request_unlocked parent_key parent) parent;
				Hashtbl.replace infos key info;
				add_child_unlocked key
			);
			info

let stat path =
	let path = full_path path in
	stat_full path (key_of_full path)

let exists path =
	(stat path).kind <> Missing

let is_directory path =
	(stat path).kind = Dir

let file_exists path =
	(stat path).kind = File

(* Modification time of [path], or 0 if it does not exist. *)
let mtime path =
	if !mode = Off && Hashtbl.length overlays = 0 then Extc.filetime path else (stat path).mtime

let readdir_uncached path key =
	count_syscall "readdir" path;
	try
		let entries = Sys.readdir path in
		let extra = if Hashtbl.length overlays = 0 then [] else Mutex.protect mutex (fun () ->
			Hashtbl.fold (fun file (name,_,_) acc ->
				match parent_key file with
				| Some parent when parent = key ->
					let lname = key_of_full name in
					if Array.exists (fun e -> key_of_full e = lname) entries then acc else name :: acc
				| _ ->
					acc
			) overlays []
		) in
		Some (if extra = [] then entries else Array.append entries (Array.of_list extra))
	with Sys_error _ ->
		None

(* Listing of directory [path], or None if it is not a directory. *)
let readdir path =
	let path = full_path path in
	let key = key_of_full path in
	if !mode = Off then
		readdir_uncached path key
	else match Mutex.protect mutex (fun () -> Hashtbl.find_opt listings key) with
		| Some listing ->
			listing
		| None ->
			let listing = readdir_uncached path key in
			Mutex.protect mutex (fun () ->
				if listing <> None then Watcher.request_unlocked key path;
				Hashtbl.replace listings key listing;
				add_child_unlocked key
			);
			listing

let set_overlay path content =
	let path = full_path path in
	let key = key_of_full path in
	Mutex.protect mutex (fun () ->
		overlay_clock := !overlay_clock -. 1.;
		Hashtbl.replace overlays key (Filename.basename path,content,!overlay_clock);
		invalidate_unlocked key
	)

let remove_overlay path =
	let key = key_of_full (full_path path) in
	Mutex.protect mutex (fun () ->
		Hashtbl.remove overlays key;
		invalidate_unlocked key
	)

(* Content of [path]: its overlay if it has one, the file otherwise. *)
let read_file path =
	match find_overlay (key_of_full (full_path path)) with
	| Some (_,content,_) ->
		content
	| None ->
		count_syscall "read" path;
		Std.input_file ~bin:true path
