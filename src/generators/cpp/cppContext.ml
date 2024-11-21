open Extlib_leftovers
open Ast
open Type
open Error
open Common
open Globals
open CppAstTools

(* CPP code generation context *)
(*
  ctx_debug_level
    0 = no debug
    1 = function + line debug via macros, which can be activated at cpp compile-time
    2 = include macros for HXCPP_DEBUGGER
    3 = annotate source with additional info about AST and types
    4 = console output at haxe compile-time

   normal = 1
*)
type context = {
  ctx_common : Common.context;
  mutable ctx_debug_level : int;
  (* cached as required *)
  mutable ctx_file_info : (string, string) PMap.t ref;
  ctx_type_ids : (string, Int32.t) Hashtbl.t;
  (* Per file *)
  ctx_output : string -> unit;
  ctx_writer : CppSourceWriter.source_writer;
  ctx_file_id : int ref;
  ctx_is_header : bool;
  ctx_interface_slot : (string, int) Hashtbl.t ref;
  ctx_interface_slot_count : int ref;
  (* This is for returning from the child nodes of TSwitch && TTry *)
  mutable ctx_real_this_ptr : bool;
  mutable ctx_class_member_types : (string, string) Hashtbl.t;
}

let new_context common_ctx debug file_info member_types =
  let null_file =
    new CppSourceWriter.source_writer common_ctx ignore ignore (fun () -> ())
  in
  let has_def def = Common.defined_value_safe common_ctx def <> "" in
  let result =
    {
      ctx_common = common_ctx;
      ctx_writer = null_file;
      ctx_file_id = ref (-1);
      ctx_type_ids = Hashtbl.create 0;
      ctx_is_header = false;
      ctx_output = null_file#write;
      ctx_interface_slot = ref (Hashtbl.create 0);
      ctx_interface_slot_count = ref 2;
      ctx_debug_level =
        (if has_def Define.AnnotateSource then 3
         else if has_def Define.HxcppDebugger then 2
         else debug);
      ctx_real_this_ptr = true;
      ctx_class_member_types = member_types;
      ctx_file_info = file_info;
    }
  in
  result

let file_context ctx writer debug header =
  {
    ctx with
    ctx_writer = writer;
    ctx_output = writer#write;
    ctx_is_header = header;
    ctx_file_id = ref (-1);
  }

(* todo - is this how it's done? *)

let pmap_keys pmap =
  let key_list = ref [] in
  PMap.iter (fun key _ -> key_list :=  key :: !key_list ) pmap;
  !key_list

let pmap_values pmap =
  let value_list = ref [] in
  PMap.iter (fun _ value -> value_list :=  value :: !value_list ) pmap;
  !value_list

(* The Hashtbl structure seems a little odd - but here is a helper function *)
let hash_iterate hash visitor =
  let result = ref [] in
  Hashtbl.iter (fun key value -> result :=  (visitor key value) :: !result ) hash;
  !result

let hash_keys hash =
  let key_list = ref [] in
  Hashtbl.iter (fun key value -> key_list :=  key :: !key_list ) hash;
  !key_list

let is_gc_element ctx member_type =
  Common.defined ctx.ctx_common Define.HxcppGcGenerational && (is_object_element member_type)

let strip_file ctx file = match Common.defined ctx Common.Define.AbsolutePath with
  | true -> Path.get_full_path file
  | false -> ctx.class_paths#relative_path file