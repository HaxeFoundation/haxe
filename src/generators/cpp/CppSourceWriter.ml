open Extlib_leftovers
open Ast
open Type
open Error
open Common
open Globals
open CppStrings
open CppAstTools
open CppTypeUtils

(*
  Code for generating source files.
  It manages creating diretories, indents, blocks and only modifying files
  when the content changes.
*)

let get_include_prefix common_ctx with_slash =
  try
    Common.defined_value common_ctx Define.IncludePrefix ^ if with_slash then "/" else ""
  with Not_found -> ""

let should_prefix_include = function
  | x when is_internal_class x -> false
  | [], "hxMath" -> true
  | _ -> false

let verbatim_include file =
  match String.sub file 0 1 with
  | "@" -> "@import " ^ String.sub file 1 (String.length file - 1) ^ ";\n"
  | _ -> "#include \"" ^ file ^ "\"\n"

let guarded_include file =
  let guard_name = "INCLUDED_" ^ hash64 file in
  "#ifndef " ^ guard_name ^ "\n" ^ "#define " ^ guard_name ^ "\n" ^ verbatim_include file ^ "#endif\n"

let source_file_extension common_ctx =
  (* no need to -D file_extension if -D objc is defined *)
  if Common.defined common_ctx Define.Objc then ".mm"
  else
    try
      "." ^ Common.defined_value common_ctx Define.FileExtension
    with Not_found -> ".cpp"

class source_writer common_ctx write_header_func write_func close_func =
  object (this)
    val indent_str = "\t"
    val mutable indent = ""
    val mutable indents = []
    val mutable just_finished_block = false
    val mutable headerLines = Hashtbl.create 0

    method close =
      close_func ();
      ()

    method write x =
      write_func x;
      just_finished_block <- false

    method write_h x =
      write_header_func x;
      ()

    method write_h_unique x =
      if not (Hashtbl.mem headerLines x) then (
        Hashtbl.add headerLines x ();
        this#write_h x)

    method indent_one = this#write indent_str

    method push_indent =
      indents <- indent_str :: indents;
      indent <- String.concat "" indents

    method pop_indent =
      match indents with
      | h :: tail ->
          indents <- tail;
          indent <- String.concat "" indents
      | [] -> indent <- "/*?*/"

    method write_i x = this#write (indent ^ x)
    method get_indent = indent

    method begin_block =
      this#write "{\n";
      this#push_indent

    method end_block =
      this#pop_indent;
      this#write_i "}\n";
      just_finished_block <- true

    method end_block_line =
      this#pop_indent;
      this#write_i "}";
      just_finished_block <- true

    method terminate_line =
      this#write (if just_finished_block then "" else ";\n")

    method add_big_closures =
      this#write_h_unique "#include <hx/MacrosJumbo.h>\n"

    method add_include class_path =
      match class_path with
      | [ "@verbatim" ], file -> this#write_h_unique (guarded_include file)
      | _ ->
          let prefix =
            if should_prefix_include class_path then ""
            else get_include_prefix common_ctx true
          in
          this#write_h
            ("#ifndef INCLUDED_" ^ join_class_path class_path "_" ^ "\n");
          this#write_h
            ("#include <" ^ prefix ^ join_class_path class_path "/" ^ ".h>\n");
          this#write_h "#endif\n"
  end

let read_whole_file chan = Std.input_all chan

(*
  The cached_source_writer will not write to the file if it has not changed,
  thus allowing the makefile dependencies to work correctly
*)
let cached_source_writer common_ctx filename =
  let header = Buffer.create 0 in
  let add_header str = Buffer.add_string header str in
  let buffer = Buffer.create 0 in
  let add_buf str = Buffer.add_string buffer str in
  let close () =
    Buffer.add_buffer header buffer;
    let contents = Buffer.contents header in
    let same =
      try
        let in_file = open_in filename in
        let old_contents = read_whole_file in_file in
        close_in in_file;
        contents = old_contents
      with _ -> false
    in
    if not same then (
      let out_file = open_out filename in
      output_string out_file contents;
      close_out out_file)
  in
  new source_writer common_ctx add_header add_buf close

let new_source_file common_ctx base_dir sub_dir extension class_path =
  let include_prefix = get_include_prefix common_ctx true in
  let full_dir =
    if sub_dir = "include" && include_prefix <> "" then (
      let dir =
        match fst class_path with
        | [] -> base_dir ^ "/include/" ^ get_include_prefix common_ctx false
        | path ->
            base_dir ^ "/include/" ^ include_prefix ^ String.concat "/" path
      in
      Path.mkdir_recursive base_dir
        ([ "include"; include_prefix ] @ fst class_path);
      dir)
    else (
      Path.mkdir_recursive base_dir (sub_dir :: fst class_path);
      base_dir ^ "/" ^ sub_dir ^ "/" ^ String.concat "/" (fst class_path))
  in
  let file =
    cached_source_writer common_ctx (full_dir ^ "/" ^ snd class_path ^ extension)
  in
  Codegen.map_source_header common_ctx (fun s ->
      file#write_h (Printf.sprintf "// %s\n" s));
  file

let new_cpp_file common_ctx base_dir =
  new_source_file common_ctx base_dir "src" (source_file_extension common_ctx)

let new_header_file common_ctx base_dir =
  new_source_file common_ctx base_dir "include" ".h"

let new_placed_cpp_file common_ctx class_path =
  let base_dir = common_ctx.file in

  if (Common.defined common_ctx Define.Vcproj ) then begin
    Path.mkdir_recursive base_dir ("src"::[]);
    cached_source_writer common_ctx
      ( base_dir ^ "/src/" ^ ( String.concat "-" (fst class_path) ) ^ "-" ^
      (snd class_path) ^ (source_file_extension common_ctx) )
  end else
    new_cpp_file common_ctx common_ctx.file class_path