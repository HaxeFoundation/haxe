(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Local_store

module STbl = Misc.Stdlib.String.Tbl

(* Mapping from basenames to full filenames *)
type registry = string STbl.t

let visible_files : registry ref = s_table STbl.create 42
let visible_files_uncap : registry ref = s_table STbl.create 42

let hidden_files : registry ref = s_table STbl.create 42
let hidden_files_uncap : registry ref = s_table STbl.create 42

module Dir = struct
  type t = {
    path : string;
    files : string list;
    hidden : bool;
  }

  let path t = t.path
  let files t = t.files
  let hidden t = t.hidden

  let find t fn =
    if List.mem fn t.files then
      Some (Filename.concat t.path fn)
    else
      None

  let find_normalized t fn =
    let fn = Misc.normalized_unit_filename fn in
    let search base =
      if Misc.normalized_unit_filename base = fn then
        Some (Filename.concat t.path base)
      else
        None
    in
    List.find_map search t.files

  (* For backward compatibility reason, simulate the behavior of
     [Misc.find_in_path]: silently ignore directories that don't exist
     + treat [""] as the current directory. *)
  let readdir_compat dir =
    try
      Sys.readdir (if dir = "" then Filename.current_dir_name else dir)
    with Sys_error _ ->
      [||]

  let create ~hidden path =
    { path; files = Array.to_list (readdir_compat path); hidden }
end

type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string

let visible_dirs = s_ref []
let hidden_dirs = s_ref []
let no_auto_include _ _ = raise Not_found
let auto_include_callback = ref no_auto_include

let reset () =
  assert (not Config.merlin || Local_store.is_bound ());
  STbl.clear !hidden_files;
  STbl.clear !hidden_files_uncap;
  STbl.clear !visible_files;
  STbl.clear !visible_files_uncap;
  hidden_dirs := [];
  visible_dirs := [];
  auto_include_callback := no_auto_include

let get_visible () = List.rev !visible_dirs

let get_path_list () =
  Misc.rev_map_end Dir.path !visible_dirs (List.rev_map Dir.path !hidden_dirs)

type paths =
  { visible : string list;
    hidden : string list }

let get_paths () =
  { visible = List.rev_map Dir.path !visible_dirs;
    hidden = List.rev_map Dir.path !hidden_dirs }

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

(* Optimized version of [add] below, for use in [init] and [remove_dir]: since
   we are starting from an empty cache, we can avoid checking whether a unit
   name already exists in the cache simply by adding entries in reverse
   order. *)
let prepend_add dir =
  List.iter (fun base ->
      Result.iter (fun filename ->
          let fn = Filename.concat dir.Dir.path base in
          if dir.Dir.hidden then begin
            STbl.replace !hidden_files base fn;
            STbl.replace !hidden_files_uncap filename fn
          end else begin
            STbl.replace !visible_files base fn;
            STbl.replace !visible_files_uncap filename fn
          end)
        (Misc.normalized_unit_filename base)
    ) dir.Dir.files

let init ~auto_include ~visible ~hidden =
  reset ();
  visible_dirs := List.rev_map (Dir.create ~hidden:false) visible;
  hidden_dirs := List.rev_map (Dir.create ~hidden:true) hidden;
  List.iter prepend_add !hidden_dirs;
  List.iter prepend_add !visible_dirs;
  auto_include_callback := auto_include

let remove_dir dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let visible = List.filter (fun d -> Dir.path d <> dir) !visible_dirs in
  let hidden = List.filter (fun d -> Dir.path d <> dir) !hidden_dirs in
  if    List.compare_lengths visible !visible_dirs <> 0
     || List.compare_lengths hidden !hidden_dirs <> 0 then begin
    reset ();
    visible_dirs := visible;
    hidden_dirs := hidden;
    List.iter prepend_add hidden;
    List.iter prepend_add visible
  end

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present, in order to enforce
   left-to-right precedence. *)
let add (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  let update base fn visible_files hidden_files =
    if dir.hidden && not (STbl.mem !hidden_files base) then
      STbl.replace !hidden_files base fn
    else if not (STbl.mem !visible_files base) then
      STbl.replace !visible_files base fn
  in
  List.iter
    (fun base ->
       Result.iter (fun ubase ->
           let fn = Filename.concat dir.Dir.path base in
           update base fn visible_files hidden_files;
           update ubase fn visible_files_uncap hidden_files_uncap
         )
         (Misc.normalized_unit_filename base)
    )
    dir.files;
  if dir.hidden then
    hidden_dirs := dir :: !hidden_dirs
  else
    visible_dirs := dir :: !visible_dirs

let append_dir = add

let add_dir ~hidden dir = add (Dir.create ~hidden dir)

(* Add the directory at the start of load path - so basenames are
   unconditionally added. *)
let prepend_dir (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  prepend_add dir;
  if dir.hidden then
    hidden_dirs := !hidden_dirs @ [dir]
  else
    visible_dirs := !visible_dirs @ [dir]

let is_basename fn = Filename.basename fn = fn

let auto_include_libs libs alert find_in_dir fn =
  let scan (lib, lazy dir) =
    let file = find_in_dir dir fn in
    let alert_and_add_dir _ =
      alert lib;
      append_dir dir
    in
    Option.iter alert_and_add_dir file;
    file
  in
  match List.find_map scan libs with
  | Some base -> base
  | None -> raise Not_found

let auto_include_otherlibs =
  (* Ensure directories are only ever scanned once *)
  let expand = Misc.expand_directory Config.standard_library in
  let otherlibs =
    let read_lib lib = lazy (Dir.create ~hidden:false (expand ("+" ^ lib))) in
    List.map (fun lib -> (lib, read_lib lib)) ["dynlink"; "str"; "unix"] in
  auto_include_libs otherlibs

type visibility = Visible | Hidden

let find_file_in_cache fn visible_files hidden_files =
  try (STbl.find !visible_files fn, Visible) with
  | Not_found -> (STbl.find !hidden_files fn, Hidden)

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  try
    if is_basename fn && not !Sys.interactive then
      fst (find_file_in_cache fn visible_files hidden_files)
    else
      Misc.find_in_path (get_path_list ()) fn
  with Not_found ->
    !auto_include_callback Dir.find fn

let find_normalized_with_visibility fn =
  assert (not Config.merlin || Local_store.is_bound ());
  match Misc.normalized_unit_filename fn with
  | Error _ -> raise Not_found
  | Ok fn_uncap ->
  try
    if is_basename fn && not !Sys.interactive then
      find_file_in_cache fn_uncap
        visible_files_uncap hidden_files_uncap
    else
      try
        (Misc.find_in_path_normalized (get_visible_path_list ()) fn, Visible)
      with
      | Not_found ->
        (Misc.find_in_path_normalized (get_hidden_path_list ()) fn, Hidden)
  with Not_found ->
    (!auto_include_callback Dir.find_normalized fn_uncap, Visible)

let find_normalized fn = fst (find_normalized_with_visibility fn)
