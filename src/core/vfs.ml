(*
	Virtual file system: an in-memory view of the parts of the file system the
	compiler uses (file and directory information, directory listings).

	In the compilation server, entries are kept across compilations and
	invalidated by file system events, so that resolving a path that is
	already known does not make any system call. See
	docs/incremental-compilation.md, phase 6.

	Entries are keyed by absolute, normalized paths. The current directory is
	tracked by the VFS itself (see `chdir`) instead of being queried for every
	relative path.
*)

type kind =
	| File
	| Dir
	| Missing

type info = {
	kind : kind;
	(* Modification time with the full precision of the file system. *)
	mtime : float;
	size : int;
}

type mode =
	(* Entries are dropped at the start of every compilation. *)
	| Uncached
	(* Entries are kept for the lifetime of the process (command line compiler). *)
	| Process
	(* Entries are kept across compilations and invalidated by file system
	   events (compilation server). *)
	| Watched

let mode = ref Process

let mutex = Mutex.create ()
let infos : (string,info) Hashtbl.t = Hashtbl.create 0
let listings : (string,string array option) Hashtbl.t = Hashtbl.create 0

(* Overlays: contents sent by an editor for unsaved buffers. They take
   precedence over the file system until they are removed. Each overlay has its
   own modification time (negative, decreasing on every change), so that a
   new overlay content is seen as a change. *)
let overlays : (string,string * float) Hashtbl.t = Hashtbl.create 0
let overlay_clock = ref 0.

(* Number of file system calls made by the VFS since the last `reset_stats`. *)
let syscalls = Atomic.make 0
let trace_syscalls = Sys.getenv_opt "HAXE_VFS_TRACE" <> None
let count_syscall ?(what="") () =
	if trace_syscalls then prerr_endline ("vfs: " ^ what);
	Atomic.incr syscalls
(* Number of calls of the last finished compilation (see `end_compilation`). *)
let last_compilation_syscalls = Atomic.make 0

let reset_stats () = Atomic.set syscalls 0

let cwd = Atomic.make None

let getcwd () =
	match Atomic.get cwd with
	| Some dir ->
		dir
	| None ->
		count_syscall ~what:"getcwd" ();
		let dir = Unix.getcwd () in
		Atomic.set cwd (Some dir);
		dir

(* Changes the current directory. All directory changes made by the compiler
   go through this function, so that the current directory is not queried
   again. *)
let chdir dir =
	let target = Path.get_full_path dir in
	Atomic.set cwd None;
	Unix.chdir dir;
	Atomic.set cwd (Some target)

(* For directory changes made outside of `chdir`. *)
let forget_cwd () =
	Atomic.set cwd None

let () = Path.getcwd := getcwd

let full_path path =
	let path = Path.get_full_path path in
	let len = String.length path in
	if len > 1 && (path.[len - 1] = '/' || path.[len - 1] = '\\') then String.sub path 0 (len - 1) else path

let is_in path dir =
	let ld = String.length dir in
	String.length path > ld && String.sub path 0 ld = dir && (path.[ld] = '/' || path.[ld] = '\\')

(* Drops the entries below directory [path] and its listing. *)
let invalidate_contents_unlocked path =
	let below = Hashtbl.fold (fun k _ acc -> if is_in k path then k :: acc else acc) infos [] in
	List.iter (Hashtbl.remove infos) below;
	let below = Hashtbl.fold (fun k _ acc -> if is_in k path then k :: acc else acc) listings [] in
	List.iter (Hashtbl.remove listings) below;
	Hashtbl.remove listings path

(* Drops the entries of [path] and of everything below it, and the listing of
   its parent directory. *)
let invalidate_unlocked path =
	invalidate_contents_unlocked path;
	Hashtbl.remove infos path;
	Hashtbl.remove listings path;
	let parent = Filename.dirname path in
	Hashtbl.remove infos parent;
	Hashtbl.remove listings parent

let invalidate path =
	let path = full_path path in
	Mutex.protect mutex (fun () -> invalidate_unlocked path)

let () = Path.on_directory_created := invalidate

let clear () =
	Mutex.protect mutex (fun () ->
		Hashtbl.reset infos;
		Hashtbl.reset listings
	)

(* File system watching. libuv is not thread-safe, so the watcher loop is only
   used from `begin_compilation`: directories to watch are queued meanwhile. *)
module Watcher = struct
	let loop_state = ref None
	let get_loop () = match !loop_state with
		| Some l -> l
		| None ->
			let l = match Luv.Loop.init () with
				| Ok loop -> Some loop
				| Error _ -> None
			in
			loop_state := Some l;
			l
	let watched : (string,Luv.FS_event.t) Hashtbl.t = Hashtbl.create 0
	(* Directories that cannot be watched (watch limit, unsupported file
	   system): their entries are revalidated on every compilation. *)
	let unwatchable : (string,unit) Hashtbl.t = Hashtbl.create 0
	let pending : (string,unit) Hashtbl.t = Hashtbl.create 0

	(* Called with the VFS mutex held. *)
	let request dir =
		if !mode = Watched && not (Hashtbl.mem watched dir) && not (Hashtbl.mem unwatchable dir) then
			Hashtbl.replace pending dir ()

	let on_event dir result =
		Mutex.protect mutex (fun () ->
			match result with
			| Ok (Some name,_) ->
				invalidate_unlocked (Filename.concat dir name)
			| Ok (None,_) ->
				invalidate_unlocked dir
			| Error _ ->
				Hashtbl.replace unwatchable dir ();
				invalidate_unlocked dir
		)

	(* Starts the pending watches, then processes the events received since the
	   previous compilation. Entries read before their directory was watched are
	   dropped once the watch is active, as a change may have been missed in
	   between. *)
	let update () =
		match get_loop () with
		| None ->
			Mutex.protect mutex (fun () ->
				Hashtbl.iter (fun dir _ -> Hashtbl.replace unwatchable dir ()) pending;
				Hashtbl.reset pending
			)
		| Some loop ->
			let dirs = Mutex.protect mutex (fun () ->
				let dirs = Hashtbl.fold (fun dir _ acc -> dir :: acc) pending [] in
				Hashtbl.reset pending;
				dirs
			) in
			List.iter (fun dir ->
				match Luv.FS_event.init ~loop () with
				| Ok handle ->
					Luv.FS_event.start handle dir (on_event dir);
					(* The directory itself is covered by the watch on its parent. *)
					Mutex.protect mutex (fun () ->
						Hashtbl.replace watched dir handle;
						invalidate_contents_unlocked dir
					)
				| Error _ ->
					Mutex.protect mutex (fun () ->
						Hashtbl.replace unwatchable dir ();
						invalidate_unlocked dir
					)
			) dirs;
			ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ());
			ignore (Luv.Loop.run ~loop ~mode:`NOWAIT ())

	let revalidate_unwatchable () =
		Mutex.protect mutex (fun () ->
			Hashtbl.iter (fun dir _ -> invalidate_unlocked dir) unwatchable
		)
end

(* Prepares the VFS for a compilation: applies the changes that happened since
   the previous one. The compilation then works on a stable view. *)
let begin_compilation () =
	if trace_syscalls then prerr_endline "vfs: --- begin compilation";
	match !mode with
	| Uncached ->
		clear ()
	| Process ->
		()
	| Watched ->
		Watcher.update ();
		Watcher.revalidate_unwatchable ()

let end_compilation () =
	Atomic.set last_compilation_syscalls (Atomic.get syscalls)

let stat_uncached path =
	count_syscall ~what:("stat " ^ path) ();
	try
		let st = Unix.stat path in
		{
			kind = (match st.Unix.st_kind with Unix.S_DIR -> Dir | _ -> File);
			mtime = st.Unix.st_mtime;
			size = st.Unix.st_size;
		}
	with Unix.Unix_error _ ->
		{ kind = Missing; mtime = 0.; size = 0 }

let rec stat_full path =
	match Mutex.protect mutex (fun () -> Hashtbl.find_opt overlays path) with
	| Some (content,mtime) ->
		{ kind = File; mtime; size = String.length content }
	| None ->
	match Mutex.protect mutex (fun () -> Hashtbl.find_opt infos path) with
	| Some info ->
		info
	| None ->
		let info = stat_uncached path in
		let parent = Filename.dirname path in
		(* Watching the parent directory covers changes to this entry. When the
		   parent does not exist, its own parent is watched, and so on. *)
		if parent <> path then begin
			let parent_info = stat_full parent in
			if parent_info.kind = Dir then Mutex.protect mutex (fun () -> Watcher.request parent)
		end;
		Mutex.protect mutex (fun () -> Hashtbl.replace infos path info);
		info

let stat path =
	stat_full (full_path path)

let exists path =
	(stat path).kind <> Missing

let is_directory path =
	(stat path).kind = Dir

let file_exists path =
	(stat path).kind = File

(* Modification time of [path], or 0 if it does not exist. *)
let mtime path =
	(stat path).mtime

(* Sorted listing of directory [path], or None if it is not a directory. *)
let readdir path =
	let path = full_path path in
	match Mutex.protect mutex (fun () -> Hashtbl.find_opt listings path) with
	| Some listing ->
		listing
	| None ->
		let listing =
			if (stat_full path).kind = Dir then begin
				count_syscall ~what:("readdir " ^ path) ();
				try
					let entries = Sys.readdir path in
					(* Overlays of files that do not exist on disk yet. *)
					let extra = Mutex.protect mutex (fun () ->
						Hashtbl.fold (fun file _ acc ->
							let name = Filename.basename file in
							if Filename.dirname file = path && not (Array.mem name entries) then name :: acc else acc
						) overlays []
					) in
					let entries = Array.append entries (Array.of_list extra) in
					Array.sort compare entries;
					Some entries
				with Sys_error _ ->
					None
			end else
				None
		in
		Mutex.protect mutex (fun () ->
			if listing <> None then Watcher.request path;
			Hashtbl.replace listings path listing
		);
		listing

(* Files written through `write_file`: digest of the content and file
   information right after the write. *)
let written : (string,Digest.t * info) Hashtbl.t = Hashtbl.create 0

(* Writes [content] to [path], unless the file already has this content. The
   check uses the digest of the previous write when the file has not changed
   since, and reads the file otherwise. Keeps the VFS up to date, so that the
   file is immediately visible to the next lookup. Returns true if the file was
   written. *)
let write_file path content =
	let path = full_path path in
	let digest = Digest.string content in
	let current = stat_full path in
	let unchanged = match Mutex.protect mutex (fun () -> Hashtbl.find_opt written path) with
		| Some (d,info) ->
			d = digest && info = current
		| None when current.kind = File && current.size = String.length content ->
			count_syscall ~what:("read " ^ path) ();
			(try Std.input_file ~bin:true path = content with Sys_error _ -> false)
		| None ->
			false
	in
	if unchanged then
		false
	else begin
		count_syscall ~what:("write " ^ path) ();
		let ch = open_out_bin path in
		Std.finally (fun () -> close_out ch) (output_string ch) content;
		Mutex.protect mutex (fun () -> invalidate_unlocked path);
		let info = stat_full path in
		Mutex.protect mutex (fun () -> Hashtbl.replace written path (digest,info));
		true
	end

let set_overlay path content =
	let path = full_path path in
	Mutex.protect mutex (fun () ->
		overlay_clock := !overlay_clock -. 1.;
		Hashtbl.replace overlays path (content,!overlay_clock);
		invalidate_unlocked path
	)

let remove_overlay path =
	let path = full_path path in
	Mutex.protect mutex (fun () ->
		Hashtbl.remove overlays path;
		invalidate_unlocked path
	)

(* Content of [path]: its overlay if it has one, the file otherwise. *)
let read_file path =
	let full = full_path path in
	match Mutex.protect mutex (fun () -> Hashtbl.find_opt overlays full) with
	| Some (content,_) ->
		content
	| None ->
		count_syscall ~what:("read " ^ full) ();
		Std.input_file ~bin:true path
