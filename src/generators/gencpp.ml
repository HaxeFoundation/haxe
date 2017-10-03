(*
   The Haxe Compiler
   Copyright (C) 2005-2017  Haxe Foundation

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Ast
open Type
open Common
open Globals

(*
   Generators do not care about non-core-type abstracts, so let us follow them
   away by default.
*)
let follow = Abstract.follow_with_abstracts

(*
   Code for generating source files.
   It manages creating diretories, indents, blocks and only modifying files
   when the content changes.
*)

(*
   A class_path is made from a package (array of strings) and a class name.
   Join these together, inclding a separator.  eg, "/" for includes : pack1/pack2/Name or "::"
   for namespace "pack1::pack2::Name"
*)
let join_class_path path separator =
   let result = match fst path, snd path with
   | [], s -> s
   | el, s -> String.concat separator el ^ separator ^ s in
   if (String.contains result '+') then begin
      let idx = String.index result '+' in
      (String.sub result 0 idx) ^ (String.sub result (idx+1) ((String.length result) - idx -1 ) )
   end else
      result;;

let class_text path =
   join_class_path path "::"
;;

(* The internal classes are implemented by the core hxcpp system, so the cpp
   classes should not be generated *)
let is_internal_class = function
   |  ([],"Int") | ([],"Void") |  ([],"String") | ([], "Null") | ([], "Float")
   |  ([],"Array") | ([], "Class") | ([], "Enum") | ([], "Bool")
   |  ([], "Dynamic") | ([], "ArrayAccess") | (["cpp"], "FastIterator")
   |  (["cpp"],"Pointer") | (["cpp"],"ConstPointer")
   |  (["cpp"],"RawPointer") | (["cpp"],"RawConstPointer")
   |  (["cpp"],"Function") -> true
   |  (["cpp"],"VirtualArray") -> true
   |  ([],"Math") -> true
   |  (["cpp"],"Int8") | (["cpp"],"UInt8") | (["cpp"],"Char")
   |  (["cpp"],"Int16") | (["cpp"],"UInt16")
   |  (["cpp"],"Int32") | (["cpp"],"UInt32")
   |  (["cpp"],"Int64") | (["cpp"],"UInt64")
   |  (["cpp"],"Float32") | (["cpp"],"Float64") | ([],"Single") -> true
   | _ -> false;;

let get_include_prefix common_ctx with_slash =
   try
     (Common.defined_value common_ctx Define.IncludePrefix) ^ (if with_slash then "/" else "")
   with
     Not_found -> ""
;;


let should_prefix_include = function
   | x when is_internal_class x -> false
   |  ([],"hxMath") -> true
   | _ -> false;;


let verbatim_include file =
   if (String.sub file 0 1)="@" then
      ("@import " ^ (String.sub file 1 ((String.length file) - 1 )) ^ ";\n")
   else
      ("#include \"" ^ file ^ "\"\n")
;;

let hash64 s =
   String.sub (Digest.to_hex (Digest.string s)) 0 16
;;


let guarded_include file =
   let guard_name = "INCLUDED_" ^ (hash64 file) in
   "#ifndef " ^ guard_name ^ "\n" ^
   "#define " ^ guard_name ^ "\n" ^
   (verbatim_include file) ^
   "#endif\n";



class source_writer common_ctx write_header_func write_func close_func =
   object(this)
   val indent_str = "\t"
   val mutable indent = ""
   val mutable indents = []
   val mutable just_finished_block = false
   val mutable headerLines  = Hashtbl.create 0
   method close = close_func(); ()
   method write x = write_func x; just_finished_block <- false
   method write_h x = write_header_func x; ()
   method write_h_unique x = if not (Hashtbl.mem headerLines x) then begin
      Hashtbl.add headerLines x ();
      this#write_h x;
      end
   method indent_one = this#write indent_str

   method push_indent = indents <- indent_str::indents; indent <- String.concat "" indents
   method pop_indent = match indents with
                     | h::tail -> indents <- tail; indent <- String.concat "" indents
                     | [] -> indent <- "/*?*/";
   method write_i x = this#write (indent ^ x)
   method get_indent = indent
   method begin_block = this#write ("{\n"); this#push_indent
   method end_block = this#pop_indent; this#write_i "}\n"; just_finished_block <- true
   method end_block_line = this#pop_indent; this#write_i "}"; just_finished_block <- true
   method terminate_line = this#write (if just_finished_block then "" else ";\n")
   method add_big_closures = this#write_h_unique "#include <hx/MacrosJumbo.h>\n";

   method add_include class_path =
      ( match class_path with
         | (["@verbatim"],file) -> this#write_h_unique (guarded_include file)
         | _ ->
            let prefix = if should_prefix_include class_path then "" else get_include_prefix common_ctx true in
            this#write_h ("#ifndef INCLUDED_" ^ (join_class_path class_path "_") ^ "\n");
            this#write_h ("#include <" ^ prefix ^ (join_class_path class_path "/") ^ ".h>\n");
            this#write_h ("#endif\n")
      )
end;;

let read_whole_file chan =
   Std.input_all chan;;

(* The cached_source_writer will not write to the file if it has not changed,
   thus allowing the makefile dependencies to work correctly *)
let cached_source_writer common_ctx filename =
   let header = Buffer.create 0 in
   let add_header str = Buffer.add_string header str in
   let buffer = Buffer.create 0 in
   let add_buf str = Buffer.add_string buffer str in
   let close = fun() ->
      Buffer.add_buffer header buffer;
      let contents = Buffer.contents header in
      let same =
         try
            let in_file = open_in filename in
            let old_contents = read_whole_file in_file in
            close_in in_file;
            contents=old_contents
         with _ ->
            false
      in
      if not same then begin
         let out_file = open_out filename in
         output_string out_file contents;
         close_out out_file;
      end;
   in
   new source_writer common_ctx (add_header) (add_buf) (close)
;;

let make_class_directories = Common.mkdir_recursive;;

let make_base_directory dir =
   make_class_directories "" ( ( Str.split_delim (Str.regexp "[\\/]+") dir ) );;

let new_source_file common_ctx base_dir sub_dir extension class_path =
   let include_prefix = get_include_prefix common_ctx true in
   let full_dir =
      if (sub_dir="include") && (include_prefix<>"") then begin
         let dir = match fst class_path with
            | [] -> base_dir ^ "/include/" ^ (get_include_prefix common_ctx false)
            | path -> base_dir ^ "/include/" ^ include_prefix ^ ( String.concat "/" path )
         in
         make_class_directories base_dir (["include";include_prefix]@(fst class_path));
         dir
      end else begin
         make_class_directories base_dir ( sub_dir :: (fst class_path));
         base_dir ^ "/" ^ sub_dir ^ "/" ^ ( String.concat "/" (fst class_path) )
      end
   in
   let file = cached_source_writer common_ctx (full_dir ^ "/" ^ ((snd class_path) ^ extension)) in
   Codegen.map_source_header common_ctx (fun s -> file#write_h (Printf.sprintf "// %s\n" s));
   file



let source_file_extension common_ctx =
   (* no need to -D file_extension if -D objc is defined *)
   if Common.defined common_ctx Define.Objc then
      ".mm"
   else try
      "." ^ (Common.defined_value common_ctx Define.FileExtension)
   with
     Not_found -> ".cpp"
;;


let new_cpp_file common_ctx base_dir = new_source_file common_ctx base_dir "src" (source_file_extension common_ctx);;

let new_header_file common_ctx base_dir =
   new_source_file common_ctx base_dir "include" ".h";;



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
type context =
{
   ctx_common : Common.context;

   mutable ctx_debug_level : int;
   (* cached as required *)
   mutable ctx_file_info : (string,string) PMap.t ref;

   ctx_type_ids : (string,Int32.t) Hashtbl.t;

   (* Per file *)
   ctx_output : string -> unit;
   ctx_writer : source_writer;
   ctx_file_id : int ref;
   ctx_is_header : bool;

   ctx_interface_slot : (string,int) Hashtbl.t ref;
   ctx_interface_slot_count : int ref;
   (* This is for returning from the child nodes of TSwitch && TTry *)
   mutable ctx_real_this_ptr : bool;
   mutable ctx_class_member_types : (string,string) Hashtbl.t;
}

let new_context common_ctx debug file_info member_types =
let null_file = new source_writer common_ctx ignore ignore (fun () -> () ) in
let has_def def = Common.defined_value_safe common_ctx def <>""  in
let result =
{
   ctx_common = common_ctx;
   ctx_writer = null_file;
   ctx_file_id = ref (-1);
   ctx_type_ids = Hashtbl.create 0;
   ctx_is_header = false;
   ctx_output = (null_file#write);
   ctx_interface_slot = ref (Hashtbl.create 0);
   ctx_interface_slot_count = ref 2;
   ctx_debug_level = if has_def Define.AnnotateSource then 3 else
                     if has_def Define.HxcppDebugger then 2 else
                        debug;
   ctx_real_this_ptr = true;
   ctx_class_member_types =  member_types;
   ctx_file_info = file_info;
} in
result


let file_context ctx writer debug header =
   { ctx with
      ctx_writer = writer;
      ctx_output = (writer#write);
      ctx_is_header = header;
      ctx_file_id = ref (-1);
   }
;;


(* The internal header files are also defined in the hx/Object.h file, so you do
   #include them separately.  However, Math classes has its
   own header file (under the hxcpp tree) so these should be included *)
let include_class_header = function
   | ([],"@Main") -> false
   | ([],"Math") -> true
   | path -> not ( is_internal_class path )


let is_cpp_class = function
   | ("cpp"::_ , _)  -> true
   | ( [] , "EReg" )  -> true
   | ( ["haxe"] , "Log" )  -> true
   | _ -> false;;

let is_block exp = match exp.eexpr with | TBlock _ -> true | _ -> false ;;

(* todo - is this how it's done? *)
let hash_keys hash =
   let key_list = ref [] in
   Hashtbl.iter (fun key value -> key_list :=  key :: !key_list ) hash;
   !key_list;;

let pmap_keys pmap =
   let key_list = ref [] in
   PMap.iter (fun key _ -> key_list :=  key :: !key_list ) pmap;
   !key_list;;

let pmap_values pmap =
   let value_list = ref [] in
   PMap.iter (fun _ value -> value_list :=  value :: !value_list ) pmap;
   !value_list;;



(* The Hashtbl structure seems a little odd - but here is a helper function *)
let hash_iterate hash visitor =
   let result = ref [] in
   Hashtbl.iter (fun key value -> result :=  (visitor key value) :: !result ) hash;
   !result





let is_internal_member member =
   member = "toString" || (
      (String.length member > 1) && (String.sub member 0 2 = "__") &&
         (match member with
         | "__ArgCount" | "__ArrayImplRef" | "__CStr" | "__Compare" | "__Create"
         | "__CreateEmpty" | "__FieldRef" | "__FindArgCount"
         | "__GetFieldMap" | "__GetHandle" | "__GetItem"
         | "__GetScriptCallable" | "__GetScriptVTable"
         | "__Param" | "__Remove" | "__SGetClass"
         | "__Set" | "__SetItem" | "__TArrayImplRef"
         | "__ToDouble" | "__ToInt" | "__ToInterface" | "__ToObject"
         | "__Visit" | "__WCStr" | "__a" | "__blit" | "__boot"
         | "__boot_all" | "__compare" | "__concat" | "__construct" | "__copy"
         | "__filter" | "__get_args" | "__hx_dump_stack" | "__hx_field_iter" | "__hxt_gc_new"
         | "__indexOf" | "__insert" | "__instanceof" | "__int" | "__iterator"
         | "__join" | "__lastIndexOf" | "__loadprim" | "__mClass" | "__mDynamicFields"
         | "__map" | "__memcmp" | "__new" | "__pop" | "__prime"
         | "__push" | "__qsort" | "__unshift" | "__unsafeStringReference" | "__time_stamp"
         | "__superString" | "__splice" | "__shift" | "__slice" | "__sort"
         | "__s_id" | "__run" | "__root" | "__register" | "__remove"
         | "__removeAt" | "__reverse" | "__zero"
         | "__Field" | "__IField" | "__Run" | "__Is" | "__GetClass" | "__GetType" | "__ToString"
         | "__s" | "__GetPtr" | "__SetField" | "__length" | "__IsArray" | "__SetThis" | "__Internal"
         | "__EnumParams" | "__Index" | "__Tag" | "__GetFields" | "__HasField"
         | "__get" | "__set" | "__unsafe_get" | "__unsafe_set" | "__global__"
         | "__SetSize" | "__trace" | "__GetRealObject" | "__SetSizeExact" | "__cpp__"
         | "__URLEncode" | "__URLDecode" | "__IsEnum"
               -> true
         | _ -> (String.length member > 4) && (String.sub member 0 4 = "__hx") ) );;

let is_known_member member =
   match member with
   | "__meta__" | "__rtti" | "_Compare"
         -> true
   | _ -> false;;

(* Convert function names that can't be written in c++ ... *)
let keyword_remap name =
   if (is_internal_member name) || (is_known_member name) then
      name
   else if (String.length name > 1) && (String.sub name 0 2 = "__") then
      "_hx_" ^ name
   else match name with
   | "int" | "Int" | "Bool" | "super"
   | "auto" | "char" | "const" | "delete" | "double" | "Float" | "enum"
   | "extern" | "float" | "friend" | "goto" | "long" | "operator" | "protected"
   | "register" | "short" | "signed" | "sizeof" | "template" | "typedef"
   | "union" | "unsigned" | "void" | "volatile" | "or" | "and" | "xor" | "or_eq" | "not"
   | "and_eq" | "xor_eq" | "typeof" | "stdin" | "stdout" | "stderr" | "system"
   | "BIG_ENDIAN" | "LITTLE_ENDIAN" | "assert" | "NULL" | "wchar_t" | "EOF"
   | "bool" | "const_cast" | "dynamic_cast" | "explicit" | "export" | "mutable" | "namespace"
   | "reinterpret_cast" | "static_cast" | "typeid" | "typename" | "virtual"
   | "_Complex" | "INFINITY" | "NAN"
   | "INT_MIN" | "INT_MAX" | "INT8_MIN" | "INT8_MAX" | "UINT8_MAX" | "INT16_MIN"
   | "INT16_MAX" | "UINT16_MAX" | "INT32_MIN" | "INT32_MAX" | "UINT32_MAX"
   | "asm" | "near" | "far"
   | "HX_" | "HXLINE" | "HXDLIN"
   | "NO" | "YES"
   | "abstract" | "decltype" | "finally" | "nullptr" | "static_assert"
   | "struct" -> "_hx_" ^ name
   | x -> x
;;

let remap_class_path class_path =
   let path_remap with_keywords name =
      let len = String.length name in
      if (len > 3) && (String.sub name 0 3 = " ::") then
         String.sub name 3 (len-3)
      else if (len > 2) && (String.sub name 0 2 = "::") then
         String.sub name 2 (len-2)
      else if with_keywords then
         keyword_remap name
      else
         name
   in
   (List.map (path_remap true) (fst class_path)) , path_remap false (snd class_path)
;;

let join_class_path_remap path separator =
   match join_class_path (remap_class_path path) separator with
   | "Class" -> "hx::Class"
   | x -> x
;;

let get_meta_string meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String (name,_)),_],_) :: _  when k=key-> name
      | _ :: l -> loop l
      in
   loop meta
;;



let get_meta_string_path meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String (name,_)),_], pos) :: _  when k=key->
           (try
           if (String.sub name 0 2) = "./" then begin
              let base = if (Filename.is_relative pos.pfile) then
                 Filename.concat (Sys.getcwd()) pos.pfile
              else
                 pos.pfile
              in
              Path.normalize_path (Filename.concat (Filename.dirname base) (String.sub name 2 ((String.length name) -2)  ))
           end else
              name
           with Invalid_argument _ -> name)
      | _ :: l -> loop l
      in
   loop meta
;;


let get_meta_string_full_filename meta key =
   let rec loop = function
      | [] -> ""
      | (k,_, pos) :: _  when k=key->
           if (Filename.is_relative pos.pfile) then
              Path.normalize_path (Filename.concat (Sys.getcwd()) pos.pfile)
           else
              pos.pfile
      | _ :: l -> loop l
      in
   loop meta
;;

let get_meta_string_full_dirname meta key =
   let name = get_meta_string_full_filename meta key in
   try
      Path.normalize_path (Filename.dirname name)
   with Invalid_argument _ -> ""
;;


let get_field_access_meta field_access key =
match field_access with
   | FInstance(_,_,class_field)
   | FStatic(_,class_field) -> get_meta_string class_field.cf_meta key
   | _ -> ""
;;

let format_code code =
	String.concat "\n" (ExtString.String.nsplit code "\r\n")

let get_code meta key =
   let code = get_meta_string meta key in
   let magic_var = "${GENCPP_SOURCE_DIRECTORY}"  in
   let code = if ExtString.String.exists code magic_var then begin
         let source_directory = get_meta_string_full_dirname meta key in
         let _,code = ExtString.String.replace code magic_var source_directory in
         code
      end else
         code
      in
   if (code<>"") then format_code code ^ "\n" else code
;;

let has_meta_key meta key =
   List.exists (fun m -> match m with | (k,_,_) when k=key-> true | _ -> false ) meta
;;

let type_has_meta_key haxe_type key =
   match follow haxe_type with
   | TInst (klass,_) -> has_meta_key klass.cl_meta key
   | TType (type_def,_) -> has_meta_key type_def.t_meta key
   | TEnum (enum_def,_) -> has_meta_key enum_def.e_meta key
   | _ -> false
;;


(*
let dump_meta meta =
   List.iter (fun m -> match m with | (k,_,_) -> print_endline ((fst (Meta.to_string k)) ^ "=" ^ (get_meta_string meta k) ) | _ -> () ) meta;;
*)

let get_class_code class_def key = match class_def.cl_kind with
    | KAbstractImpl abstract_def ->
       let value = (get_code abstract_def.a_meta key) in
       value
    | _ -> get_code class_def.cl_meta key
;;


(* Add include to source code *)
let add_include writer class_path =
   writer#add_include class_path;;

let list_num l = string_of_int (List.length l);;


(* This gets the class include order correct.  In the header files, we forward declare
   the class types so the header file does not have any undefined variables.
   In the cpp files, we include all the required header files, providing the actual
   types for everything.  This way there is no problem with circular class references.
*)
let gen_forward_decl writer class_path isNative =
   begin
      let output = writer#write in
      match class_path with
      | (["@verbatim"],file) ->
          writer#write (guarded_include file)
      | _ ->
         let name = fst (remap_class_path class_path) in
         output ((if isNative then "HX_DECLARE_NATIVE" else "HX_DECLARE_CLASS") ^ list_num name  ^ "(");
         List.iter (fun package_part -> output (package_part ^ ",") ) name;
         output ( (snd class_path) ^ ")\n")
end;;

let real_interfaces =
List.filter (function (t,pl) ->
   match t, pl with
   | { cl_path = ["cpp";"rtti"],_ },[] -> false
   | _ -> true
);;


let is_var_field field =
   match field.cf_kind with
   | Var _ -> true
   | Method MethDynamic -> true
   | _ -> false
;;

let rec has_rtti_interface c interface =
   List.exists (function (t,pl) ->
      (snd t.cl_path) = interface && (match fst t.cl_path with | ["cpp";"rtti"] -> true | _ -> false )
   ) c.cl_implements ||
      (match c.cl_super with None -> false | Some (c,_) -> has_rtti_interface c interface);;

let has_field_integer_lookup class_def =
   has_rtti_interface class_def "FieldIntegerLookup";;

let has_field_integer_numeric_lookup class_def =
   has_rtti_interface class_def "FieldNumericIntegerLookup";;

(* Output required code to place contents in required namespace *)
let gen_open_namespace output class_path =
      List.iter (fun namespace -> output ("namespace " ^ namespace ^ "{\n")) (List.map keyword_remap (fst class_path));;

let gen_close_namespace output class_path =
      List.iter
         (fun namespace -> output ( "}" ^ " // end namespace " ^ namespace ^"\n"))
         (fst class_path);;

(* The basic types can have default values and are passesby value *)
let is_numeric = function
   | "Int" | "Bool" | "Float" |  "unsigned char" -> true
   | "::cpp::UInt8" | "::cpp::Int8" | "::cpp::Char"
   | "::cpp::UInt16" | "::cpp::Int16"
   | "::cpp::UInt32" | "::cpp::Int32"
   | "::cpp::UInt64" | "::cpp::Int64"
   | "::cpp::Float32" | "::cpp::Float64"
   | "int" | "bool" | "double" | "float" | "Single" -> true
   | _ -> false


let rec remove_parens expression =
      match expression.eexpr with
      | TParenthesis e -> remove_parens e
      | TMeta(_,e) -> remove_parens e
      | _ -> expression
;;


let rec remove_parens_cast expression =
   match expression.eexpr with
   | TParenthesis e -> remove_parens_cast e
   | TMeta(_,e) -> remove_parens_cast e
   | TCast ( e,None) -> remove_parens_cast e
   | _ -> expression
;;

let is_interface_type t =
   match follow t with
   | TInst (klass,params) -> klass.cl_interface
   | _ -> false
;;


let is_cpp_function_instance haxe_type =
   match follow haxe_type with
   | TInst (klass,params) ->
      (match klass.cl_path with
      | ["cpp"] , "Function" -> true
      | _ -> false )
   | _ -> false
   ;;


let is_objc_class klass =
   klass.cl_extern && Meta.has Meta.Objc klass.cl_meta
;;

let rec is_objc_type t =
   match t with
   | TInst(cl,_) -> cl.cl_extern && Meta.has Meta.Objc cl.cl_meta
   | TType(td,_) -> (Meta.has Meta.Objc td.t_meta)
   | TAbstract (a,_) -> (Meta.has Meta.Objc a.a_meta)
   | TMono r -> (match !r with | Some t -> is_objc_type t | _ -> false)
   | TLazy f -> is_objc_type (lazy_type f)
   | _ -> false
;;


let is_lvalue var =
   match (remove_parens var).eexpr with
   | TLocal _ -> true
   | TField (_,FStatic(_,field) ) | TField (_,FInstance(_,_,field) ) -> is_var_field field
   | _ -> false
;;



let is_pointer haxe_type includeRaw =
   match follow haxe_type with
   | TInst (klass,params) ->
      (match klass.cl_path with
      | ["cpp"] , "Pointer"
      | ["cpp"] , "ConstPointer"
      | ["cpp"] , "Function" -> true
      | ["cpp"] , "RawPointer" when includeRaw -> true
      | ["cpp"] , "RawConstPointer" when includeRaw -> true
      | _ -> false )
   | TType (type_def,params) ->
      (match type_def.t_path with
      | ["cpp"] , "Pointer"
      | ["cpp"] , "ConstPointer"
      | ["cpp"] , "Function" -> true
      | ["cpp"] , "RawPointer" when includeRaw -> true
      | ["cpp"] , "RawConstPointer" when includeRaw -> true
      | _ -> false )
   | _ -> false
   ;;

let is_dynamic_type_param class_kind =
   match class_kind with
   | KTypeParameter _ -> true
   | _ -> false
;;


let is_native_gen_class class_def =
   (has_meta_key class_def.cl_meta Meta.NativeGen) ||
      (match class_def.cl_kind with
       | KAbstractImpl abstract_def -> (has_meta_key abstract_def.a_meta Meta.NativeGen)
       | _ -> false );
;;

let is_native_gen_module = function
   | TClassDecl class_def -> is_native_gen_class class_def
   | _ -> false
;;



(*  Get a string to represent a type.
   The "suffix" will be nothing or "_obj", depending if we want the name of the
   pointer class or the pointee (_obj class *)
let rec class_string klass suffix params remap =
   let type_string = type_string_remap remap in
   let join_class_path_remap = if remap then join_class_path_remap else join_class_path in
   (match klass.cl_path with
   (* Array class *)
   |  ([],"Array") when is_dynamic_array_param (List.hd params) ->
           "cpp::ArrayBase" ^ suffix
           (*"cpp::VirtualArray" ^ suffix*)
   |  ([],"Array") -> (snd klass.cl_path) ^ suffix ^ "< " ^ (String.concat ","
               (List.map array_element_type params) ) ^ " >"
   (* FastIterator class *)
   |  (["cpp"],"FastIterator") -> "::cpp::FastIterator" ^ suffix ^ "< " ^ (String.concat ","
               (List.map type_string  params) ) ^ " >"
   |  (["cpp"],"Pointer")
   |  (["cpp"],"ConstPointer") ->
        "::cpp::Pointer< " ^ (String.concat "," (List.map type_string params) ) ^ " >"
   |  (["cpp"],"RawPointer") ->
        " " ^ (String.concat "," (List.map type_string params) ) ^ " * "
   |  (["cpp"],"RawConstPointer") ->
        " const " ^ (String.concat "," (List.map type_string params) ) ^ " * "
   |  (["cpp"],"Function") ->
        "::cpp::Function< " ^ (cpp_function_signature_params params) ^ " >"
   | _ when is_dynamic_type_param klass.cl_kind -> "Dynamic"
   |  ([],"#Int") -> "/* # */int"
   |  (["cpp"],"UInt8") -> "unsigned char"
   |  ([],"Class") -> "hx::Class"
   |  ([],"EnumValue") -> "Dynamic"
   |  ([],"Null") -> (match params with
         | [t] ->
            (match follow t with
            | TAbstract ({ a_path = [],"Int" },_)
            | TAbstract ({ a_path = [],"Float" },_)
            | TAbstract ({ a_path = [],"Bool" },_) -> "Dynamic"
            | TAbstract ({ a_path = ["cpp"],"UInt8" },_) -> "Dynamic"
            | t when type_has_meta_key t Meta.NotNull -> "Dynamic"
            | _ -> "/*NULL*/" ^ (type_string t) )
         | _ -> assert false);
   (* Objective-C class *)
   | path when is_objc_type (TInst(klass,[])) ->
      let str = join_class_path_remap klass.cl_path "::" in
      if suffix = "_obj" then
         str
      else if klass.cl_interface then
         "id < " ^ str ^ ">"
      else
         str ^ " *"
   (* Native interface - use pointer *)
   | _ when klass.cl_interface && is_native_gen_class klass ->
            (join_class_path_remap klass.cl_path "::") ^ " *"
   (* Normal class *)
   | path when klass.cl_extern && (not (is_internal_class path) )->
            (join_class_path_remap klass.cl_path "::") ^ suffix
   | _ ->
      let globalNamespace = if (get_meta_string klass.cl_meta Meta.Native)<>"" then "" else "::" in
      globalNamespace ^ (join_class_path_remap klass.cl_path "::") ^ suffix
   )
and type_string_suff suffix haxe_type remap =
   let type_string = type_string_remap remap in
   let join_class_path_remap = if remap then join_class_path_remap else join_class_path in
   (match haxe_type with
   | TMono r -> (match !r with None -> "Dynamic" ^ suffix | Some t -> type_string_suff suffix t remap)
   | TAbstract ({ a_path = ([],"Void") },[]) -> "Void"
   | TAbstract ({ a_path = ([],"Bool") },[]) -> "bool"
   | TAbstract ({ a_path = ([],"Float") },[]) -> "Float"
   | TAbstract ({ a_path = ([],"Int") },[]) -> "int"
   | TAbstract ({ a_path = (["cpp"],"UInt8") },[]) -> "unsigned char"
   | TAbstract( { a_path = ([], "EnumValue") }, _  ) -> "Dynamic"
   | TAbstract ({ a_path = ([],"Null") }, [t]) ->
		(match follow t with
		| TAbstract ({ a_path = [],"Int" },_)
		| TAbstract ({ a_path = [],"Float" },_)
		| TAbstract ({ a_path = [],"Bool" },_) -> "Dynamic" ^ suffix
		| t when type_has_meta_key t Meta.NotNull -> "Dynamic" ^ suffix
		| _ -> type_string_suff suffix t remap)
   | TEnum (enum,params) ->  "::" ^ (join_class_path_remap enum.e_path "::") ^ suffix
   | TInst (klass,params) ->  (class_string klass suffix params remap)
   | TType (type_def,params) ->
      (match type_def.t_path with
      | [] , "Array" ->
         (match params with
         | [t] when (type_string (follow t) ) = "Dynamic" -> "Dynamic"
         | [t] -> "Array< " ^ (type_string (follow t) ) ^ " >"
         | _ -> assert false)
      | ["cpp"] , "FastIterator" ->
         (match params with
         | [t] -> "::cpp::FastIterator< " ^ (type_string (follow t) ) ^ " >"
         | _ -> assert false)
      | ["cpp"] , "Pointer"
      | ["cpp"] , "ConstPointer" ->
         (match params with
         | [t] -> "::cpp::Pointer< " ^ (type_string (follow t) ) ^ " >"
         | _ -> assert false)
      | ["cpp"] , "RawPointer" ->
         (match params with
         | [t] -> " " ^ (type_string (follow t) ) ^ " *"
         | _ -> assert false)
      | ["cpp"] , "RawConstPointer" ->
         (match params with
         | [t] -> "const " ^ (type_string (follow t) ) ^ " *"
         | _ -> assert false)
      | ["cpp"] , "Function" ->
         "::cpp::Function< " ^ (cpp_function_signature_params params ) ^ " >"
      | _ ->  type_string_suff suffix (apply_params type_def.t_params params type_def.t_type) remap
      )
   | TFun (args,haxe_type) -> "Dynamic" ^ suffix
   | TAnon a -> "Dynamic"
      (*
      (match !(a.a_status) with
      | Statics c -> type_string_suff suffix (TInst (c,List.map snd c.cl_params))
      | EnumStatics e -> type_string_suff suffix (TEnum (e,List.map snd e.e_params))
      | _ -> "Dynamic"  ^ suffix )
      *)
   | TDynamic haxe_type -> "Dynamic" ^ suffix
   | TLazy func -> type_string_suff suffix (lazy_type func) remap
   | TAbstract (abs,pl) when abs.a_impl <> None ->
      type_string_suff suffix (Abstract.get_underlying_type abs pl) remap
   | TAbstract (abs,pl) ->
      "::" ^ (join_class_path_remap abs.a_path "::") ^ suffix
   )

and type_string_remap remap haxe_type =
   type_string_suff "" haxe_type remap

and type_string haxe_type =
   type_string_suff "" haxe_type true

and array_element_type haxe_type =
   match type_string haxe_type with
   | x when cant_be_null haxe_type -> x
   | x when is_interface_type (follow haxe_type) -> x
   | "::String" -> "::String"
   | _ -> "::Dynamic"

and is_dynamic_array_param haxe_type =
   if (type_string (follow haxe_type)) = "Dynamic" then true
   else (match follow haxe_type with
   | TInst (klass,params) ->
         (match klass.cl_path with
         | ([],"Array") | ([],"Class") | (["cpp"],"FastIterator")
         | (["cpp"],"RawPointer") |(["cpp"],"ConstRawPointer")
         | (["cpp"],"Pointer") |(["cpp"],"ConstPointer")|(["cpp"],"Function") -> false
         | _ -> (match klass.cl_kind with KTypeParameter _ -> true | _ -> false)
         )
   | _ -> false
   )
and cpp_function_signature tfun abi =
   match follow tfun with
   | TFun(args,ret) -> (type_string ret) ^ " " ^ abi ^ "(" ^ (gen_tfun_interface_arg_list args) ^ ")"
   | _ -> "void *"

and cpp_function_signature_params params = match params with
   | [t; abi] -> (match follow abi with
       | TInst (klass,_) -> cpp_function_signature t (get_meta_string klass.cl_meta Meta.Abi)
       | _ -> print_endline (type_string abi);
           assert false )
   | _ ->
      print_endline ("Params:" ^ (String.concat "," (List.map type_string params) ));
      assert false;

and gen_interface_arg_type_name name opt typ =
   let type_str = (type_string typ) in
   (* type_str may have already converted Null<X> to Dynamic because of NotNull tag ... *)
   (if (opt && (cant_be_null typ) && type_str<>"Dynamic" ) then
      "hx::Null< " ^ type_str ^ " > "
   else
      type_str ) ^ " " ^ (keyword_remap name)

and gen_tfun_interface_arg_list args =
   String.concat "," (List.map (fun (name,opt,typ) -> gen_interface_arg_type_name name opt typ) args)
and cant_be_null haxe_type =
   is_numeric (type_string haxe_type) || (type_has_meta_key haxe_type Meta.NotNull )
;;



let is_array haxe_type =
   match follow haxe_type with
   | TInst (klass,params) ->
      (match klass.cl_path with
      | [] , "Array" -> not (is_dynamic_array_param (List.hd params))
      | _ -> false )
   | TType (type_def,params) ->
      (match type_def.t_path with
      | [] , "Array" ->  not (is_dynamic_array_param (List.hd params))
      | _ -> false )
   | _ -> false
   ;;

let is_array_or_dyn_array haxe_type =
   match follow haxe_type with
   | TInst (klass,params) ->
      (match klass.cl_path with | [] , "Array" -> true | _ -> false )
   | TType (type_def,params) ->
      (match type_def.t_path with | [] , "Array" -> true | _ -> false )
   | _ -> false
   ;;



let is_array_implementer haxe_type =
   match follow haxe_type with
   | TInst (klass,params) ->
      (match klass.cl_array_access with
      | Some _ -> true
      | _ -> false )
   | _ -> false
   ;;



let is_static_access obj =
   match (remove_parens obj).eexpr with
   | TTypeExpr _ -> true
   | _ -> false
;;

let is_native_with_space func =
   match (remove_parens func).eexpr with
   | TField(obj,field) when is_static_access obj ->
      String.contains (get_field_access_meta field Meta.Native) ' '
   | _ -> false
;;


let is_native_pointer expr =
   let t = type_string expr.etype in
   let l = String.length t in
   l>1 && (String.sub t (l-1) 1) = "*"
;;


let rec is_cpp_function_member func =
   match (remove_parens func).eexpr with
   | TField(obj,field) when is_cpp_function_instance obj.etype -> true
   | TCall(obj,_) -> is_cpp_function_member obj
   | _ -> false
;;




(* Get the type and output it to the stream *)
(*
let gen_type ctx haxe_type =
   ctx.ctx_output (type_string haxe_type)
;;

let member_type ctx field_object member =
   let name = (if (is_array field_object.etype) then "::Array"
            else (type_string field_object.etype)) ^ "." ^ member in
   try ( Hashtbl.find ctx.ctx_class_member_types name )
   with Not_found -> "?";;

*)
let is_interface obj = is_interface_type obj.etype;;

let should_implement_field x = is_physical_field x;;

let is_extern_class class_def =
   class_def.cl_extern || (has_meta_key class_def.cl_meta Meta.Extern) ||
      (match class_def.cl_kind with
       | KAbstractImpl abstract_def -> (has_meta_key abstract_def.a_meta Meta.Extern)
       | _ -> false );
;;

let is_scalar_abstract abstract_def =
   Meta.has Meta.Scalar abstract_def.a_meta && Meta.has Meta.CoreType abstract_def.a_meta
;;


let real_non_native_interfaces =
List.filter (function (t,pl) ->
   match t, pl with
   | { cl_path = ["cpp";"rtti"],_ },[] -> false
   | _ -> not (is_native_gen_class t)
);;



let is_extern_class_instance obj =
   match follow obj.etype with
   | TInst (klass,params) -> klass.cl_extern
   | _ -> false
;;


let rec is_dynamic_accessor name acc field class_def =
 ( ( acc ^ "_" ^ field.cf_name) = name ) &&
   ( not (List.exists (fun f -> f.cf_name=name) class_def.cl_ordered_fields) )
   && (match class_def.cl_super with None -> true | Some (parent,_) -> is_dynamic_accessor name acc field parent )
;;


(* Check to see if we are the first object in the parent tree to implement a dynamic interface *)
let implement_dynamic_here class_def =
   let implements_dynamic c = match c.cl_dynamic with None -> false | _ -> true  in
   let rec super_implements_dynamic c = match c.cl_super with
      | None -> false
      | Some (csup, _) -> if (implements_dynamic csup) then true else
            super_implements_dynamic csup;
   in
   ( (implements_dynamic class_def) && (not (super_implements_dynamic class_def) ) );;


let gen_hash32 seed str =
   let h = ref (Int32.of_int seed) in
   let cycle = Int32.of_int 223 in
   for i = 0 to String.length str - 1 do
      h := Int32.add (Int32.mul !h cycle) (Int32.of_int (int_of_char (String.unsafe_get str i)));
   done;
   !h
;;

let gen_hash seed str =
   Printf.sprintf "0x%08lx" (gen_hash32 seed str)
;;

let gen_hash_small seed str =
   Printf.sprintf "%08lx" (gen_hash32 seed str)
;;

let gen_string_hash str =
   let h = gen_hash32 0 str in
   Printf.sprintf "\"\\x%02lx\",\"\\x%02lx\",\"\\x%02lx\",\"\\x%02lx\""
       (Int32.shift_right_logical (Int32.shift_left h 24) 24)
       (Int32.shift_right_logical (Int32.shift_left h 16) 24)
       (Int32.shift_right_logical (Int32.shift_left h 8) 24)
       (Int32.shift_right_logical h 24)
;;

let gen_qstring_hash str =
   let h = gen_hash32 0 str in
   Printf.sprintf "%02lx,%02lx,%02lx,%02lx"
       (Int32.shift_right_logical (Int32.shift_left h 24) 24)
       (Int32.shift_right_logical (Int32.shift_left h 16) 24)
       (Int32.shift_right_logical (Int32.shift_left h 8) 24)
       (Int32.shift_right_logical h 24)
;;





(* Make string printable for c++ code *)
(* Here we know there are no utf8 characters, so use the L"" notation to avoid conversion *)
let escape_stringw s l =
   let b = Buffer.create 0 in
   Buffer.add_char b 'L';
   Buffer.add_char b '"';
   let skip = ref 0 in
   for i = 0 to String.length s - 1 do
      if (!skip>0) then begin
         skip := !skip -1;
         l := !l-1;
      end else
      match Char.code (String.unsafe_get s i) with
      | c when (c>127) ->
         let encoded =  ((c land 0x3F) lsl 6) lor ( Char.code ((String.unsafe_get s (i+1))) land 0x7F) in
         skip := 1;
         Buffer.add_string b (Printf.sprintf "\\x%X\"L\"" encoded)
      | c when (c < 32) -> Buffer.add_string b (Printf.sprintf "\\x%X\"L\"" c)
      | c -> Buffer.add_char b (Char.chr c)
   done;
   Buffer.add_char b '"';
   Buffer.contents b;;

let special_to_hex s =
   let l = String.length s in
   let b = Buffer.create 0 in
   for i = 0 to l - 1 do
      match Char.code (String.unsafe_get s i) with
      | c when (c>127) || (c<32) ->
         Buffer.add_string b (Printf.sprintf "\\x%02x\"\"" c)
      | c -> Buffer.add_char b (Char.chr c)
   done;
   Buffer.contents b;;

let escape_extern s =
   let l = String.length s in
   let b = Buffer.create 0 in
   for i = 0 to l - 1 do
      match Char.code (String.unsafe_get s i) with
      | c when (c>127) || (c<32) || (c=34) || (c=92) ->
         Buffer.add_string b (Printf.sprintf "\\x%02x" c)
      | c -> Buffer.add_char b (Char.chr c)
   done;
   Buffer.contents b;;



let has_utf8_chars s =
   let result = ref false in
   for i = 0 to String.length s - 1 do
      result := !result || ( Char.code (String.unsafe_get s i) > 127 )
   done;
   !result;;

let escape_command s =
   let b = Buffer.create 0 in
   String.iter (fun ch -> if (ch=='"' || ch=='\\' ) then Buffer.add_string b "\\";  Buffer.add_char b ch ) s;
   Buffer.contents b;;

let gen_str macro gen s =
   let rec split s plus =
      let escaped = Ast.s_escape ~hex:false s in
      let hexed = (special_to_hex escaped) in
      if (String.length hexed <= 16000 ) then
         plus ^ " HX_CSTRING(\"" ^ hexed ^ "\")"
      else begin
         let len = String.length s in
         let half = len lsr 1 in
         (split (String.sub s 0 half) plus ) ^ (split (String.sub s half (len-half)) "+" )
      end
   in
   let escaped = Ast.s_escape ~hex:false s in
   let hexed = (special_to_hex escaped) in
   if (String.length hexed <= 16000 ) then
      macro ^ "(\"" ^ hexed ^ "\"," ^ (gen s) ^ ")"
   else
      "(" ^ (split s "" ) ^ ")"
;;

let str s = gen_str "HX_HCSTRING" gen_string_hash s;;
let strq s = gen_str "HX_" gen_qstring_hash s;;



let const_char_star s =
   let escaped = Ast.s_escape ~hex:false s in
   "\"" ^ special_to_hex escaped ^ "\"";
;;




(* Convert an array to a comma separated list of values *)
let array_arg_list inList =
   let i = ref (0-1) in
   String.concat "," (List.map (fun _ -> incr i; "inArgs[" ^ (string_of_int !i) ^ "]"  ) inList)


(* See if there is a haxe break statement that will be swollowed by c++ break *)
exception BreakFound;;

(* Decide is we should look the field up by name *)
let dynamic_internal = function | "__Is" -> true | _ -> false


let rec is_null expr =
   match expr.eexpr with
   | TConst TNull -> true
   | TParenthesis expr | TMeta (_,expr) -> is_null expr
   | TCast (e,None) -> is_null e
   | _ -> false
;;


let is_virtual_array expr = (type_string expr.etype="cpp::VirtualArray") ;;

let is_real_function field =
   match field.cf_kind with
   | Method MethNormal | Method MethInline-> true
   | _ -> false
;;


let is_this expression =
   match (remove_parens expression).eexpr with
   | TConst TThis -> true
   | _ -> false
;;

let is_super expression =
   match (remove_parens expression).eexpr with
   | TConst TSuper -> true
   | _ -> false
;;


let rec is_dynamic_in_cpp ctx expr =
   let expr_type = type_string ( match follow expr.etype with TFun (args,ret) -> ret | _ -> expr.etype) in
   if ( expr_type="Dynamic" || expr_type="cpp::ArrayBase") then
      true
   else begin
      let result = (
      match expr.eexpr with
      | TEnumParameter( obj, _, index ) -> true (* TODO? *)
      | TField( obj, field ) ->
            (is_dynamic_member_lookup_in_cpp ctx obj field) ||
            (is_dynamic_member_return_in_cpp ctx obj field)
      | TArray (obj,index) -> (is_dynamic_in_cpp ctx obj || is_virtual_array obj)
      | TTypeExpr _ -> false
      | TCall(func,args) ->
         let is_IaCall =
            (match (remove_parens_cast func).eexpr with
            | TField ( { eexpr = TIdent "__global__" }, field ) -> false
            | TField (obj,FStatic (class_def,field) ) when is_real_function field -> false
            | TField (obj,FInstance (_,_,field) ) when (is_this obj) && (is_real_function field) -> false
            | TField (obj,FInstance (_,_,field) ) when is_super obj -> false
            | TField (obj,FInstance (_,_,field) ) when field.cf_name = "_hx_getIndex" -> false
            | TField (obj,FInstance (_,_,field) ) when field.cf_name = "__Index" || (not (is_dynamic_in_cppia ctx obj) && is_real_function field) -> false
            | TField (obj,FDynamic (name) )  when (is_internal_member name || (type_string obj.etype = "::String" && name="cca") ) -> false
            | TConst TSuper -> false
            | TField (_,FEnum (enum,field)) -> false
            | _ -> true
            ) in
         if is_IaCall then
            true
         else
            (match follow func.etype with
            | TFun (args,ret) -> is_dynamic_in_cpp ctx func
            | _ -> true
         );
      | TParenthesis(expr) | TMeta(_,expr) -> is_dynamic_in_cpp ctx expr
      | TCast (e,None) -> (type_string expr.etype) = "Dynamic"
      | TIdent "__global__" -> false
      | TConst TNull -> true
      | _ -> false (* others ? *) )
      in
      result
   end

and is_dynamic_member_lookup_in_cpp ctx field_object field =
   let member = field_name field in
   if (is_internal_member member) then false else
   if (is_native_pointer field_object) then false else
   if (is_pointer field_object.etype true) then false else
   if (match field_object.eexpr with | TTypeExpr _ -> true | _ -> false) then false else
   if (is_dynamic_in_cpp ctx field_object) then true else
   if (is_array field_object.etype) then false else (
   let tstr = type_string field_object.etype in
   match tstr with
      (* Internal classes have no dynamic members *)
      | "::String" | "Null" | "::hx::Class" | "::Enum" | "::Math" | "::ArrayAccess" -> false
      | "Dynamic" -> true
      | name ->
          let full_name = name ^ "." ^ member in
          if Hashtbl.mem ctx.ctx_class_member_types full_name  then
               false
            else
               not (is_extern_class_instance field_object)
   )
and is_dynamic_member_return_in_cpp ctx field_object field =
   let member = field_name field in
   if (is_array field_object.etype) then false else
   if (is_pointer field_object.etype true) then false else
   if (is_internal_member member) then false else
   match field_object.eexpr with
   | TTypeExpr t ->
         let full_name = "::" ^ (join_class_path_remap (t_path t) "::" ) ^ "." ^ member in
         ( try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in
             mem_type="Dynamic" || mem_type="cpp::ArrayBase" || mem_type="cpp::VirtualArray" )
         with Not_found -> true )
   | _ ->
      let tstr = type_string field_object.etype in
      (match tstr with
         (* Internal classes have no dynamic members *)
         | "::String" | "Null" | "::hx::Class" | "::Enum" | "::Math" | "::ArrayAccess" -> false
         | "Dynamic" | "cpp::ArrayBase" | "cpp::VirtualArray" -> true
         | name ->
               let full_name = name ^ "." ^ member in
               try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in
                  mem_type="Dynamic" || mem_type="cpp::ArrayBase" || mem_type="cpp::VirtualArray" )
               with Not_found -> true )
and is_dynamic_in_cppia ctx expr =
   match expr.eexpr with
   | TCast(_,None) -> true
   | _ -> is_dynamic_in_cpp ctx expr
;;

let cast_if_required ctx expr to_type =
   if (is_dynamic_in_cpp ctx expr) then
      ctx.ctx_output (".Cast< " ^ to_type ^ " >()" )
;;


let is_matching_interface_type t0 t1 =
    (match (follow t0),(follow t1) with
    | TInst (k0,_), TInst(k1,_) -> k0==k1
    | _ -> false
    )
;;



let default_value_string = function
   | TInt i -> Printf.sprintf "%ld" i
   | TFloat float_as_string -> "((Float)" ^ float_as_string ^ ")"
   | TString s -> str s
   | TBool b -> (if b then "true" else "false")
   | TNull -> "null()"
   | _ -> "/* Hmmm */"
;;



let get_nth_type field index =
   match follow field.ef_type with
      | TFun (args,_) ->
         let rec nth l index = match l with
         | [] -> raise Not_found
         | (_,_,t)::rest ->
             if index = 0 then t
             else nth rest (index-1)
         in
         nth args index
      | _ -> raise Not_found
;;



let has_default_values args =
   List.exists ( fun (_,o) -> match o with
            | Some TNull -> false
            | Some _ -> true
            | _ -> false ) args ;;

exception PathFound of string;;


let strip_file ctx file = (match Common.defined ctx Common.Define.AbsolutePath with
   | true -> file
   | false -> let flen = String.length file in
   (* Not quite right - should probably test is file exists *)
   try
      List.iter (fun path ->
         let plen = String.length path in
         if (flen>plen && path=(String.sub file 0 plen ))
            then raise (PathFound (String.sub file plen (flen-plen)) ) )
         (ctx.class_path @ ctx.std_path);
      file;
   with PathFound tail ->
      tail)
;;

let hx_stack_push ctx output clazz func_name pos gc_stack =
   if ctx.ctx_debug_level > 0 then begin
      let stripped_file = strip_file ctx.ctx_common pos.pfile in
      let esc_file = (Ast.s_escape stripped_file) in
      ctx.ctx_file_info := PMap.add stripped_file pos.pfile !(ctx.ctx_file_info);
      let full_name = clazz ^ "." ^ func_name ^ (
        if (clazz="*") then
          (" (" ^ esc_file ^ ":" ^ (string_of_int (Lexer.get_error_line pos) ) ^ ")")
        else "") in

      let hash_class_func = gen_hash 0 (clazz^"."^func_name) in
      let hash_file = gen_hash 0 stripped_file in

      let lineName  = (string_of_int (Lexer.get_error_line pos) ) in
      incr ctx.ctx_file_id;
      let classId = hash64 (clazz ^ "." ^ stripped_file) in
      let varName = "_hx_pos_" ^ classId ^ "_" ^ lineName ^ "_" ^func_name in
      let decl = ( varName ^ ",\"" ^ clazz ^ "\",\"" ^ func_name ^ "\"," ^ hash_class_func ^ ",\"" ^
              full_name ^ "\",\"" ^ esc_file ^ "\"," ^ lineName ^  "," ^ hash_file ) in
      if ctx.ctx_is_header then
         ctx.ctx_writer#write_h_unique ("HX_DECLARE_STACK_FRAME" ^ "(" ^ varName ^ ")\n")
      else
         ctx.ctx_writer#write_h_unique ( (if func_name="new" then "HX_DEFINE_STACK_FRAME" else "HX_LOCAL_STACK_FRAME") ^ "(" ^ decl ^ ")\n");
      output ( (if gc_stack then "HX_GC_STACKFRAME" else "HX_STACKFRAME") ^ "(&" ^ varName ^ ")\n");
   end else if gc_stack then
      output ("HX_JUST_GC_STACKFRAME\n")
;;



(* { *)

type tcpp =
   | TCppDynamic
   | TCppUnchanged
   | TCppObject
   | TCppObjectPtr
   | TCppVoid
   | TCppNull
   | TCppEnum of tenum
   | TCppScalar of string
   | TCppString
   | TCppFastIterator of tcpp
   | TCppPointer of string * tcpp
   | TCppRawPointer of string * tcpp
   | TCppFunction of tcpp list * tcpp * string
   | TCppObjCBlock of tcpp list * tcpp
   | TCppRest of tcpp
   | TCppReference of tcpp
   | TCppStruct of tcpp
   | TCppStar of tcpp * bool
   | TCppVoidStar
   | TCppVarArg
   | TCppAutoCast
   | TCppDynamicArray
   | TCppObjectArray of tcpp
   | TCppWrapped of tcpp
   | TCppScalarArray of tcpp
   | TCppObjC of tclass
   | TCppNativePointer of tclass
   | TCppVariant
   | TCppCode of tcpp
   | TCppInst of tclass
   | TCppInterface of tclass
   | TCppProtocol of tclass
   | TCppClass
   | TCppGlobal


and tcppexpr = {
   cppexpr : tcpp_expr_expr;
   cpptype : tcpp;
   cpppos : pos;
}


and tcpp_closure = {
   close_type : tcpp;
   close_args : (tvar * tconstant option) list;
   close_expr : tcppexpr;
   close_id : int;
   close_undeclared : (string,tvar) Hashtbl.t;
   close_this : tcppthis option;
}


and tcppcrementop =
   | CppIncrement
   | CppDecrement

and tcppunop =
   | CppNeg
   | CppNegBits
   | CppNot

and tcppthis =
   | ThisReal
   | ThisFake
   | ThisDynamic

and tcppvarloc =
   | VarLocal of tvar
   | VarClosure of tvar
   | VarThis of tclass_field * tcpp
   | VarInstance of tcppexpr * tclass_field * string * string
   | VarInterface of tcppexpr * tclass_field
   | VarStatic of tclass * bool * tclass_field
   | VarInternal of tcppexpr * string * string

and tcppinst =
   | InstPtr
   | InstObjC
   | InstStruct

and tcppfuncloc =
   | FuncThis of tclass_field * tcpp
   | FuncInstance of tcppexpr * tcppinst * tclass_field
   | FuncStatic of tclass * bool * tclass_field
   | FuncTemplate of tclass * tclass_field * path * bool
   | FuncInterface of tcppexpr * tclass * tclass_field
   | FuncEnumConstruct of tenum * tenum_field
   | FuncSuperConstruct of tcpp
   | FuncSuper of tcppthis * tcpp * tclass_field
   | FuncNew of tcpp
   | FuncExpression of tcppexpr
   | FuncInternal of tcppexpr * string * string
   | FuncExtern of string * bool
   | FuncFromStaticFunction

and tcpparrayloc =
   | ArrayTyped of tcppexpr * tcppexpr * tcpp
   | ArrayPointer of tcppexpr * tcppexpr
   | ArrayRawPointer of tcppexpr * tcppexpr
   | ArrayObject of tcppexpr * tcppexpr * tcpp
   | ArrayVirtual of tcppexpr * tcppexpr
   | ArrayImplements of tclass * tcppexpr * tcppexpr
   | ArrayDynamic of tcppexpr * tcppexpr

and tcpplvalue =
   | CppVarRef of tcppvarloc
   | CppArrayRef of tcpparrayloc
   | CppDynamicRef of tcppexpr * string
   | CppExternRef of string * bool


and tcpp_expr_expr =
   | CppInt of int32
   | CppFloat of string
   | CppString of string
   | CppBool of bool
   | CppNull
   | CppNullAccess
   | CppNil
   | CppThis of tcppthis
   | CppSuper of tcppthis
   | CppCode of string * tcppexpr list
   | CppClosure of tcpp_closure
   | CppVar of tcppvarloc
   | CppExtern of string * bool
   | CppDynamicField of tcppexpr * string
   | CppFunction of tcppfuncloc * tcpp
   | CppEnumIndex of tcppexpr
   | CppEnumField of tenum * tenum_field
   | CppCall of tcppfuncloc * tcppexpr list
   | CppFunctionAddress of tclass * tclass_field
   | CppAddressOf of tcppexpr
   | CppDereference of tcppexpr
   | CppArray of tcpparrayloc
   | CppCrement of  tcppcrementop * Ast.unop_flag * tcpplvalue
   | CppSet of tcpplvalue * tcppexpr
   | CppModify of Ast.binop * tcpplvalue * tcppexpr
   | CppBinop of Ast.binop * tcppexpr * tcppexpr
   | CppCompare of string * tcppexpr * tcppexpr * Ast.binop
   | CppNullCompare of string * tcppexpr
   | CppObjectDecl of (string * tcppexpr) list * bool
   | CppPosition of string * int32 * string * string
   | CppArrayDecl of tcppexpr list
   | CppUnop of tcppunop * tcppexpr
   | CppVarDecl of tvar * tcppexpr option
   | CppBlock of tcppexpr list * tcpp_closure list * bool
   | CppFor of tvar * tcppexpr * tcppexpr
   | CppIf of tcppexpr * tcppexpr * tcppexpr option
   | CppWhile of tcppexpr * tcppexpr * Ast.while_flag * int
   | CppIntSwitch of tcppexpr * (Int32.t list * tcppexpr) list * tcppexpr option
   | CppSwitch of tcppexpr * tcpp * (tcppexpr list * tcppexpr) list * tcppexpr option * int
   | CppTry of tcppexpr * (tvar * tcppexpr) list
   | CppBreak
   | CppContinue
   | CppClassOf of path * bool
   | CppGoto of int
   | CppReturn of tcppexpr option
   | CppThrow of tcppexpr
   | CppEnumParameter of tcppexpr * tenum_field * int
   | CppTCast of tcppexpr * tcpp
   | CppCast of tcppexpr * tcpp
   | CppCastStatic of tcppexpr * tcpp
   | CppCastScalar of tcppexpr * string
   | CppCastVariant of tcppexpr
   | CppCastObjC of tcppexpr * tclass
   | CppCastObjCBlock of tcppexpr * tcpp list * tcpp
   | CppCastProtocol of tcppexpr * tclass
   | CppCastNative of tcppexpr

let rec s_tcpp = function
   | CppInt _  -> "CppInt"
   | CppFloat _ -> "CppFloat"
   | CppString _ -> "CppString"
   | CppBool _ -> "CppBool"
   | CppNull -> "CppNull"
   | CppNil -> "CppNil"
   | CppThis _ -> "CppThis"
   | CppSuper _ -> "CppSuper"
   | CppCode _ -> "CppCode"
   | CppClosure _ -> "CppClosure"
   | CppVar VarLocal(_) -> "CppVarLocal"
   | CppVar VarClosure(_) -> "CppVarClosure"
   | CppVar VarThis(_) -> "CppVarThis"
   | CppVar VarInstance(expr,field,clazz,op) -> "CppVarInstance(" ^ clazz ^ "::" ^ op ^ field.cf_name ^ ")"
   | CppVar VarInterface(_) -> "CppVarInterface"
   | CppVar VarStatic(_,true,_) -> "CppObjcVarStatic"
   | CppVar VarStatic(_) -> "CppVarStatic"
   | CppVar VarInternal(_) -> "CppVarInternal"
   | CppDynamicField _ -> "CppDynamicField"
   | CppExtern _ -> "CppExtern"
   | CppFunction _ -> "CppFunction"
   | CppEnumIndex _ -> "CppEnumIndex"
   | CppEnumField  _ -> "CppEnumField"
   | CppNullAccess -> "CppNullAccess"

   | CppCall (FuncThis _,_)  -> "CppCallThis"
   | CppCall (FuncInstance (obj,inst,field),_) ->
       (match inst with InstObjC -> "CppCallObjCInstance(" | InstPtr-> "CppCallInstance(" | _ -> "CppCallStruct(") ^
          tcpp_to_string obj.cpptype ^ "," ^ field.cf_name ^ ")"
   | CppCall (FuncInterface  _,_) -> "CppCallInterface"
   | CppCall (FuncStatic  (_,objC,_),_) -> if objC then "CppCallStaticObjC" else "CppCallStatic"
   | CppCall (FuncTemplate  _,_) -> "CppCallTemplate"
   | CppCall (FuncEnumConstruct _,_) -> "CppCallEnumConstruct"
   | CppCall (FuncSuperConstruct _,_) -> "CppCallSuperConstruct"
   | CppCall (FuncSuper _,_) -> "CppCallSuper"
   | CppCall (FuncNew _,_) -> "CppCallNew"
   | CppCall (FuncExpression _,_) -> "CppCallExpression"
   | CppCall (FuncInternal _,_) -> "CppCallInternal"
   | CppCall (FuncExtern _,_) -> "CppCallExtern"
   | CppCall (FuncFromStaticFunction,_) -> "CppCallFromStaticFunction"
   | CppAddressOf _  -> "CppAddressOf"
   | CppDereference _  -> "CppDereference"
   | CppFunctionAddress  _ -> "CppFunctionAddress"
   | CppArray  _ -> "CppArray"
   | CppCrement  _ -> "CppCrement"
   | CppSet  _ -> "CppSet"
   | CppModify  _ -> "CppModify"
   | CppBinop  _ -> "CppBinop"
   | CppCompare  _ -> "CppCompare"
   | CppNullCompare  _ -> "CppNullCompare"
   | CppObjectDecl  _ -> "CppObjectDecl"
   | CppPosition  _ -> "CppPosition"
   | CppArrayDecl  _ -> "CppArrayDecl"
   | CppUnop  _ -> "CppUnop"
   | CppVarDecl  _ -> "CppVarDecl"
   | CppBlock  _ -> "CppBlock"
   | CppFor  _ -> "CppFor"
   | CppIf  _ -> "CppIf"
   | CppWhile _ -> "CppWhile"
   | CppIntSwitch  _ -> "CppIntSwitch"
   | CppSwitch  _ -> "CppSwitch"
   | CppTry _ -> "CppTry"
   | CppBreak -> "CppBreak"
   | CppContinue -> "CppContinue"
   | CppClassOf _ -> "CppClassOf"
   | CppGoto _ -> "CppGoto"
   | CppReturn _ -> "CppReturn"
   | CppThrow _ -> "CppThrow"
   | CppEnumParameter _ -> "CppEnumParameter"
   | CppTCast _ -> "CppTCast"
   | CppCast _ -> "CppCast"
   | CppCastStatic _ -> "CppCastStatic"
   | CppCastScalar _ -> "CppCastScalar"
   | CppCastVariant _ -> "CppCastVariant"
   | CppCastObjC _ -> "CppCastObjC"
   | CppCastObjCBlock _ -> "CppCastObjCBlock"
   | CppCastProtocol _ -> "CppCastProtocol"
   | CppCastNative _ -> "CppCastNative"

and tcpp_to_string_suffix suffix tcpp = match tcpp with
   | TCppDynamic -> " ::Dynamic"
   | TCppUnchanged -> " ::Dynamic/*Unchanged*/"
   | TCppObject -> " ::Dynamic"
   | TCppObjectPtr -> " ::hx::Object *"
   | TCppReference t -> (tcpp_to_string t) ^" &"
   | TCppStruct t -> "cpp::Struct< " ^ (tcpp_to_string t) ^" >"
   | TCppStar(t,const) -> (if const then "const " else "" ) ^ (tcpp_to_string t) ^" *"
   | TCppVoid -> "void"
   | TCppVoidStar -> "void *"
   | TCppRest _ -> "vaarg_list"
   | TCppVarArg -> "vararg"
   | TCppAutoCast -> "::cpp::AutoCast"
   | TCppVariant -> "::cpp::Variant"
   | TCppEnum(enum) -> " ::" ^ (join_class_path_remap enum.e_path "::") ^ suffix
   | TCppScalar(scalar) -> scalar
   | TCppString -> "::String"
   | TCppFastIterator it -> "::cpp::FastIterator" ^ suffix ^ "< " ^ (tcpp_to_string it) ^ " >";
   | TCppPointer(ptrType,valueType) -> "::cpp::" ^ ptrType ^ "< " ^ (tcpp_to_string valueType) ^ " >"
   | TCppRawPointer(constName,valueType) -> constName ^ (tcpp_to_string valueType) ^ "*"
   | TCppFunction(argTypes,retType,abi) ->
        let args = (String.concat "," (List.map tcpp_to_string argTypes)) in
        "::cpp::Function< " ^ (tcpp_to_string retType) ^ " " ^ abi ^ " (" ^ args ^ ") >"
   | TCppObjCBlock(argTypes,retType) ->
        (tcpp_objc_block_struct argTypes retType) ^ "::t"
   | TCppDynamicArray -> "::cpp::VirtualArray" ^ suffix
   | TCppObjectArray _ -> "::Array" ^ suffix ^ "< ::Dynamic>"
   | TCppWrapped _ -> " ::Dynamic"
   | TCppScalarArray(value) -> "::Array" ^ suffix ^ "< " ^ (tcpp_to_string value) ^ " >"
   | TCppObjC klass ->
      let path = join_class_path_remap klass.cl_path "::" in
      if klass.cl_interface then
         "id < " ^ path ^ ">"
      else
         path ^ " *"
   | TCppProtocol interface ->
      let path = get_meta_string interface.cl_meta Meta.ObjcProtocol in
      let path = if path<>"" then path else join_class_path_remap interface.cl_path "::" in
      "id < " ^ path ^ ">"
   | TCppNativePointer klass ->
       let name = (join_class_path_remap klass.cl_path "::") in
       if suffix="_obj" then
          name
       else
          "hx::Native< " ^ name ^ "* >";
   | TCppInst klass ->
        (cpp_class_path_of klass) ^ suffix
   | TCppInterface klass when suffix="_obj" ->
        (cpp_class_path_of klass) ^ suffix
   | TCppInterface _ -> "::Dynamic"
   | TCppClass -> "hx::Class" ^ suffix;
   | TCppGlobal -> "::Dynamic";
   | TCppNull -> " ::Dynamic";
   | TCppCode _ -> "Code"

and tcpp_objc_block_struct argTypes retType =
   let args = (String.concat "," (List.map tcpp_to_string argTypes)) in
   let ret = tcpp_to_string retType in
   let suffix = (string_of_int (List.length argTypes)) in
      if (ret="void") then begin
         if (List.length argTypes) = 0 then
            "hx::TObjcBlockVoidVoid"
         else
            "hx::TObjcBlockVoidArgs" ^ suffix ^ "< " ^ args ^ " >"
      end else begin
         if (List.length argTypes) = 0 then
            "hx::TObjcBlockRetVoid< " ^ ret ^ " >"
         else
            "hx::TObjcBlockRetArgs" ^ suffix ^ "< " ^ ret ^ "," ^ args ^ " >"
      end

and tcpp_to_string tcpp =
    tcpp_to_string_suffix "" tcpp

and cpp_class_path_of klass =
      let globalNamespace = if (get_meta_string klass.cl_meta Meta.Native)<>"" then " " else " ::" in
      globalNamespace ^ (join_class_path_remap klass.cl_path "::")
;;


let cpp_const_type cval = match cval with
   | TInt i -> CppInt(i) , TCppScalar("int")
   | TBool b -> CppBool(b) , TCppScalar("bool")
   | TFloat f -> CppFloat(f) , TCppScalar("Float")
   | TString s -> CppString(s) , TCppString
   | _ -> (* TNull, TThis & TSuper should already be handled *)
      CppNull, TCppNull
;;


let is_cpp_scalar cpp_type =
   match cpp_type with
   | TCppScalar(_) -> true
   | _ -> false
;;


let is_cpp_array_implementer cppType =
   match cppType with
   | TCppInst (klass)
   | TCppInterface (klass) ->
      (match klass.cl_array_access with
      | Some _ -> true
      | _ -> false )
   | _ -> false
;;

let rec const_int_of expr =
   match expr.eexpr with
   | TConst TInt x -> x
   | TConst TBool x -> Int32.of_int (if x then 1 else 0)
   | TParenthesis e -> const_int_of e
   | _ -> raise Not_found
;;

let rec const_float_of expr =
   match expr.eexpr with
   | TConst TInt x -> Printf.sprintf "%ld" x
   | TConst TFloat x -> x
   | TConst TBool x -> if x then "1" else "0"
   | TParenthesis e -> const_float_of e
   | _ -> raise Not_found
;;


let rec const_string_of expr =
   match expr.eexpr with
   | TConst TString x -> x
   | TParenthesis e -> const_string_of e
   | _ -> raise Not_found
;;


let rec cpp_is_struct_access t =
   match t with
   | TCppFunction _ -> true
   | TCppStruct _-> false
   | TCppInst (class_def) -> (has_meta_key class_def.cl_meta Meta.StructAccess)
   | TCppReference (r) -> cpp_is_struct_access r
   | _ -> false
;;


let cpp_is_dynamic_type = function
   | TCppDynamic | TCppObject | TCppVariant | TCppWrapped _ | TCppGlobal | TCppNull
   | TCppInterface _
      -> true
   | _ -> false
;;


let rec cpp_type_of ctx haxe_type =
   (match haxe_type with
   | TMono r -> (match !r with None -> TCppDynamic | Some t -> cpp_type_of ctx t)

   | TEnum (enum,params) ->  TCppEnum(enum)

   | TInst ({ cl_path=([],"Array"); cl_kind = KTypeParameter _},_)
      -> TCppObject

   | TInst ({ cl_kind = KTypeParameter _},_)
      -> TCppDynamic

   | TInst (klass,params) ->
      cpp_instance_type ctx klass params

   | TAbstract (abs,pl) when not (Meta.has Meta.CoreType abs.a_meta) ->
       cpp_type_from_path ctx abs.a_path pl (fun () ->
            cpp_type_of ctx (Abstract.get_underlying_type abs pl) )

   | TAbstract (a,params) ->
       cpp_type_from_path ctx a.a_path params (fun () ->
            if is_scalar_abstract a then begin
               let native =  get_meta_string a.a_meta Meta.Native in
               TCppScalar(if native="" then join_class_path a.a_path "::" else native)
            end else
               TCppDynamic)

   | TType (type_def,params) ->
       cpp_type_from_path ctx type_def.t_path params (fun () ->
          cpp_type_of ctx (apply_params type_def.t_params params type_def.t_type) )

   | TFun _ -> TCppObject
   | TAnon _ -> TCppObject
   | TDynamic _ -> TCppDynamic
   | TLazy func -> cpp_type_of ctx (lazy_type func)
   )
   and  cpp_type_from_path ctx path params default =
      match path,params with
      | ([],"Void"),_ -> TCppVoid
      | ([],"void"),_ -> TCppVoid (* for old code with @:void *)
      | ([],"Bool"),_ -> TCppScalar("bool")
      | ([],"Float"),_ -> TCppScalar("Float")
      | ([],"Int"),_ -> TCppScalar("int")
      | ([], "EnumValue"),_ -> TCppObject
      | ([], "Class"),_ -> TCppClass
      | ([], "Enum"),_  -> TCppClass
      | ([], "Single"),_ -> TCppScalar("float")
      | (["cpp"], "Char"),_ -> TCppScalar("char")
      | (["cpp"], "Object"),_ -> TCppObjectPtr
      | (["cpp"], "Float32"),_ -> TCppScalar("float")
      | (["cpp"], "Float64"),_ -> TCppScalar("double")
      | (["cpp"], "Int8"),_ -> TCppScalar("signed char")
      | (["cpp"], "Int16"),_ -> TCppScalar("short")
      | (["cpp"], "Int32"),_ -> TCppScalar("int")
      | (["cpp"], "Int64"),_ -> TCppScalar("::cpp::Int64")
      | (["cpp"], "UInt8"),_ -> TCppScalar("unsigned char")
      | (["cpp"], "UInt16"),_ -> TCppScalar("unsigned short")
      | (["cpp"], "UInt32"),_ -> TCppScalar("unsigned int")
      | (["cpp"], "UInt64"),_ -> TCppScalar("::cpp::UInt64")
      | (["cpp"], "VarArg"),_ -> TCppVarArg
      | (["cpp"], "AutoCast"),_ -> TCppAutoCast

      | ([],"String"), [] ->
         TCppString

      (* Things with type parameters hxcpp knows about ... *)
      | (["cpp"],"FastIterator"), [p] ->
            TCppFastIterator(cpp_type_of ctx p)
      | (["cpp"],"Pointer"), [p] ->
            TCppPointer("Pointer", cpp_type_of ctx p)
      | (["cpp"],"ConstPointer"), [p] ->
            TCppPointer("ConstPointer", cpp_type_of ctx p)
      | (["cpp"],"RawPointer"), [p] ->
            TCppRawPointer("", cpp_type_of ctx p)
      | (["cpp"],"RawConstPointer"), [p] ->
            TCppRawPointer("const ", cpp_type_of ctx p)
      | (["cpp"],"Function"), [function_type; abi] ->
            cpp_function_type_of ctx function_type abi;
      | (["cpp"],"Callable"), [function_type]
      | (["cpp"],"CallableData"), [function_type] ->
            cpp_function_type_of_string ctx function_type "";
      | (("cpp"::["objc"]),"ObjcBlock"), [function_type] ->
            let args,ret = (cpp_function_type_of_args_ret ctx function_type) in
            TCppObjCBlock(args,ret)
      | (["haxe";"extern"], "Rest"),[rest] ->
            TCppRest(cpp_type_of ctx rest)
      | (("cpp"::["objc"]),"Protocol"), [interface_type] ->
            (match follow interface_type with
            | TInst (klass,[]) when klass.cl_interface ->
                TCppProtocol(klass)
            (* TODO - get the line number here *)
            | _ -> print_endline "cpp.objc.Protocol must refer to an interface";
                   assert false;
            )
      | (["cpp"],"Reference"), [param] ->
            TCppReference(cpp_type_of ctx param)
      | (["cpp"],"Struct"), [param] ->
            TCppStruct(cpp_type_of ctx param)
      | (["cpp"],"Star"), [param] ->
            TCppStar(cpp_type_of ctx param,false)
      | (["cpp"],"ConstStar"), [param] ->
            TCppStar(cpp_type_of ctx param,true)

      | ([],"Array"), [p] ->
         let arrayOf = cpp_type_of ctx p in
         (match arrayOf with
            | TCppVoid (* ? *)
            | TCppDynamic ->
              TCppDynamicArray

            | TCppObject
            | TCppObjectPtr
            | TCppReference _
            | TCppStruct _
            | TCppStar _
            | TCppEnum _
            | TCppInst _
            | TCppInterface _
            | TCppProtocol _
            | TCppClass
            | TCppDynamicArray
            | TCppObjectArray _
            | TCppScalarArray _
               -> TCppObjectArray(arrayOf)
            | _ ->
              TCppScalarArray(arrayOf)
         )

      | ([],"Null"), [p] ->
            cpp_type_of_null ctx p

      | _ -> default ()

   and cpp_type_of_null ctx p =
     let baseType = cpp_type_of ctx p in
     if (type_has_meta_key p Meta.NotNull) || (is_cpp_scalar baseType) then
        TCppObject
     else
        baseType

   (* Optional types are Dynamic if they norally could not be null *)
   and cpp_fun_arg_type_of ctx tvar opt =
      match opt with
      | Some _ -> cpp_type_of_null ctx tvar.t_type
      | _ -> cpp_type_of ctx tvar.t_type

   and cpp_tfun_arg_type_of ctx opt t =
      if opt then cpp_type_of_null ctx t else cpp_type_of ctx t

   and cpp_function_type_of ctx function_type abi =
      let abi = (match follow abi with
                 | TInst (klass1,_) -> get_meta_string klass1.cl_meta Meta.Abi
                 | _ -> assert false )
      in
      cpp_function_type_of_string ctx function_type abi
   and cpp_function_type_of_string ctx function_type abi_string =
      let args,ret = cpp_function_type_of_args_ret ctx function_type in
      TCppFunction(args, ret, abi_string)

   and cpp_function_type_of_args_ret ctx function_type =
      match follow function_type with
      | TFun(args,ret) ->
          (* Optional types are Dynamic if they norally could not be null *)
          let  cpp_arg_type_of = fun(_,optional,haxe_type) ->
             if optional then
                cpp_type_of_null ctx haxe_type
             else
                cpp_type_of ctx haxe_type
          in
          List.map cpp_arg_type_of args, cpp_type_of ctx ret
      | _ ->  (* ? *)
          [TCppVoid], TCppVoid

   and cpp_instance_type ctx klass params =
      cpp_type_from_path ctx klass.cl_path params (fun () ->
         if is_objc_class klass then
            TCppObjC(klass)
         else if klass.cl_interface && is_native_gen_class klass then
            TCppNativePointer(klass)
         else if klass.cl_interface then
            TCppInterface(klass)
         else if klass.cl_extern && (not (is_internal_class klass.cl_path) ) then
            TCppInst(klass)
         else
            TCppInst(klass)
       )
;;


let cpp_return_type ctx haxe_type =
  match haxe_type with
  | TFun (_,ret) -> cpp_type_of ctx ret
  | _ -> TCppDynamic
;;


let cpp_member_return_type ctx member =
   cpp_return_type ctx member.cf_type
;;

let is_cpp_objc_type cpptype = match cpptype with
   | TCppObjC(_) -> true;
   | _ -> false
;;


let cpp_enum_path_of enum =
   (*
   let rename = get_meta_string enum.e_meta Meta.Native in
   if rename <> "" then
      rename
   else
   *)
   let globalNamespace = if (get_meta_string enum.e_meta Meta.Native)<>"" then "" else "::" in
   globalNamespace ^ (join_class_path_remap enum.e_path "::")
;;



(*
let rec cpp_object_name = function
   | TCppString -> "::String"
   | TCppDynamicArray -> "::cpp::VirtualArray_obj"
   | TCppObjectArray _ -> "::Array_obj< ::Dynamic>"
   | TCppScalarArray(value) -> "::Array_obj< " ^ (tcpp_to_string value) ^ " >"
   | TCppObjC klass ->  (cpp_class_path_of klass) ^ "_obj"
   | TCppInst klass -> (cpp_class_path_of klass) ^ "_obj"
   | TCppClass -> "hx::Class_obj";
   | TCppDynamic -> "Dynamic"
   | TCppVoid -> "void"
   | TCppVoidStar -> "void *"
   | TCppEnum(enum) -> "::hx::EnumBase"
   | TCppScalar(scalar) -> scalar
   | TCppFastIterator it -> "::cpp::FastIterator< " ^ (tcpp_to_string it) ^ " >";
   | TCppPointer(ptrType,valueType) -> "::cpp::" ^ ptrType ^ "< " ^ (tcpp_to_string valueType) ^ " >"
   | TCppRawPointer(constName,valueType) -> constName ^ (tcpp_to_string valueType) ^ "*"
   | TCppFunction(argTypes,retType,abi) ->
        let args = (String.concat "," (List.map tcpp_to_string argTypes)) in
        "::cpp::Function< " ^ abi ^ " " ^ (tcpp_to_string retType) ^ "(" ^ args ^ ") >"
   | TCppWrapped _ -> "Dynamic"
   | TCppNativePointer klass -> (cpp_class_path_of klass) ^ " *"
   | TCppGlobal -> "";
   | TCppNull -> "Dynamic";
   | TCppCode -> "/* code */"
;;
*)

let cpp_class_name klass =
   (*
   let rename = get_meta_string klass.cl_meta Meta.Native in
   if rename <> "" then
      rename ^ "_obj"
   else
   *)
   let globalNamespace = if (get_meta_string klass.cl_meta Meta.Native)<>"" then "" else "::" in
   let path = globalNamespace ^ (join_class_path_remap klass.cl_path "::") in
   if path="::String" then path else path ^ "_obj"
;;


let cpp_variant_type_of t = match t with
   | TCppDynamic
   | TCppUnchanged
   | TCppObject
   | TCppObjectPtr
   | TCppReference _
   | TCppStruct _
   | TCppStar _
   | TCppVoid
   | TCppFastIterator _
   | TCppDynamicArray
   | TCppObjectArray _
   | TCppScalarArray _
   | TCppWrapped _
   | TCppObjC _
   | TCppObjCBlock _
   | TCppRest _
   | TCppInst _
   | TCppInterface _
   | TCppProtocol _
   | TCppCode _
   | TCppClass
   | TCppGlobal
   | TCppNull
   | TCppEnum _ -> TCppDynamic
   | TCppString -> TCppString
   | TCppFunction _
   | TCppNativePointer _
   | TCppPointer _
   | TCppRawPointer _
   | TCppAutoCast
   | TCppVarArg
   | TCppVoidStar -> TCppVoidStar
   | TCppScalar "Int"
   | TCppScalar "bool"
   | TCppScalar "Float"  -> t
   | TCppScalar "double"
   | TCppScalar "float" -> TCppScalar("Float")
   | TCppScalar _  -> TCppScalar("int")
   | TCppVariant -> TCppVariant
;;

let cpp_cast_variant_type_of t = match t with
   | TCppObjectArray _
   | TCppScalarArray _
   | TCppDynamicArray
   | TCppClass
   | TCppEnum _
   | TCppInst _ -> t
   | _ -> cpp_variant_type_of t;
;;

let cpp_base_type_of t =
   match cpp_variant_type_of t with
   | TCppDynamic -> "Object"
   | TCppString -> "String"
   | TCppVoidStar -> "Pointer"
   | TCppScalar "int"  -> "Int"
   | TCppScalar "bool"  -> "Bool"
   | TCppScalar x  -> x
   | _  -> "Object"
;;

let ctx_type_string ctx haxe_type =
      tcpp_to_string (cpp_type_of ctx haxe_type)
;;


let ctx_cant_be_null ctx haxe_type =
   match cpp_type_of ctx haxe_type with
   | TCppScalar _ -> true
   | _  -> false

let is_complex_compare =  function
   | TCppScalar _ -> false
   | TCppString  -> false
   | _ -> true
;;



let ctx_arg_type_name ctx name default_val arg_type prefix =
   let remap_name = keyword_remap name in
   let type_str = (ctx_type_string ctx arg_type) in
   match default_val with
   | Some TNull  -> (type_str,remap_name)
   | Some constant when (ctx_cant_be_null ctx arg_type) -> ("hx::Null< " ^ type_str ^ " > ",prefix ^ remap_name)
   | Some constant  -> (type_str,prefix ^ remap_name)
   | _ -> (type_str,remap_name);;



(* Generate prototype text, including allowing default values to be null *)
let ctx_arg ctx name default_val arg_type prefix =
   let pair = ctx_arg_type_name ctx name default_val arg_type prefix in
   (fst pair) ^ " " ^ (snd pair);;


(* Generate prototype text, including allowing default values to be null *)
let ctx_arg_name ctx name default_val arg_type prefix =
   let pair = ctx_arg_type_name ctx name default_val arg_type prefix in
   (snd pair);;


let ctx_arg_list ctx arg_list prefix =
   String.concat "," (List.map (fun (v,o) -> (ctx_arg ctx v.v_name o v.v_type prefix) ) arg_list)

let ctx_arg_list_name ctx arg_list prefix =
   String.concat "," (List.map (fun (v,o) -> (ctx_arg_name ctx v.v_name o v.v_type prefix) ) arg_list)

let cpp_arg_names args =
   String.concat "," (List.map (fun (name,_,_) -> keyword_remap name) args)
;;

let rec ctx_tfun_arg_list ctx include_names arg_list =
   let oType o arg_type =
      let type_str = (ctx_type_string ctx arg_type) in
      (* type_str may have already converted Null<X> to Dynamic because of NotNull tag ... *)
      if o && (ctx_cant_be_null ctx arg_type) && type_str<>"Dynamic" then
         "hx::Null< " ^ type_str ^ " > "
      else
         type_str
   in
   match arg_list with
   | [] -> ""
   | [(name,o,arg_type)] -> (oType o arg_type) ^ (if include_names then " " ^ (keyword_remap name) else "")
   | (name,o,arg_type) :: remaining  ->
      (oType o arg_type) ^ (if include_names then " " ^ (keyword_remap name) else "") ^  "," ^ (ctx_tfun_arg_list ctx include_names remaining)

let cpp_var_type_of ctx var =
   tcpp_to_string (cpp_type_of ctx var.v_type)
;;


let cpp_macro_var_type_of ctx var =
   let t = tcpp_to_string (cpp_type_of ctx var.v_type) in
   if String.contains t ',' then
      Str.global_replace (Str.regexp ",") " HX_COMMA " t
   else
     t
;;




let ctx_function_signature ctx include_names tfun abi =
   match follow tfun with
   | TFun(args,ret) -> (ctx_type_string ctx ret) ^ " " ^ abi ^ "(" ^ (ctx_tfun_arg_list ctx include_names args) ^ ")"
   | _ -> "void *"



let cpp_var_name_of var =
   let rename = get_meta_string var.v_meta Meta.Native in
   if rename <> "" then
      rename
   else
      keyword_remap var.v_name
;;

let cpp_var_debug_name_of v =
   let rec loop meta = match meta with
      | (Meta.RealPath,[EConst (String (s,_)),_],_) :: _ -> s
      | _ :: meta -> loop meta
      | [] -> v.v_name
   in
   loop v.v_meta
;;


let cpp_no_debug_synbol ctx var =
   (ctx.ctx_debug_level<=1) || (has_meta_key var.v_meta Meta.CompilerGenerated) ||
      match cpp_type_of ctx var.v_type with
      | TCppStar _ | TCppReference _ -> true
      | TCppInst (class_def) when (has_meta_key class_def.cl_meta Meta.StructAccess) -> true
      | TCppInst (class_def) when (has_meta_key class_def.cl_meta Meta.Unreflective) -> true
      | _->
         let name = cpp_var_debug_name_of var in
         (String.length name) >4 && (String.sub name 0 4) = "_hx_"
;;

let cpp_debug_name_of var =
   keyword_remap var.v_name
;;

let cpp_debug_var_visible ctx var =
   not (cpp_no_debug_synbol ctx (fst var))
;;


let only_stack_access ctx haxe_type =
   let tcpp = cpp_type_of ctx haxe_type in
   match tcpp with
   | TCppInst(klass) -> has_meta_key klass.cl_meta Meta.StackOnly
   | _ -> false;
;;

let cpp_is_real_array obj =
   match obj.cpptype with
   | TCppScalarArray _
   | TCppObjectArray _ -> true
   | _ -> false
;;


let is_array_splice_call obj member =
   match obj.cpptype, member.cf_name with
   | TCppScalarArray _, "splice"
   | TCppObjectArray _, "splice" -> true
   | _,_ -> false
;;

let is_map_get_call obj member =
   member.cf_name="get" &&
   (match obj.cpptype  with
   | TCppInst({cl_path=(["haxe";"ds"],"IntMap")}) -> true
   | TCppInst({cl_path=(["haxe";"ds"],"StringMap")}) -> true
   | TCppInst({cl_path=(["haxe";"ds"],"ObjectMap")}) -> true
   | _ -> false
   )
;;

let is_map_set_call obj member =
   member.cf_name="set" &&
   (match obj.cpptype  with
   | TCppInst({cl_path=(["haxe";"ds"],"IntMap")}) -> true
   | TCppInst({cl_path=(["haxe";"ds"],"StringMap")}) -> true
   | TCppInst({cl_path=(["haxe";"ds"],"ObjectMap")}) -> true
   | _ -> false
   )
;;



let is_array_concat_call obj member =
   match obj.cpptype, member.cf_name with
   | TCppScalarArray _, "concat"
   | TCppObjectArray _, "concat" -> true
   | _,_ -> false
;;

let cpp_can_static_cast funcType inferredType =
   match funcType with
   | TCppReference(_) | TCppStar(_) | TCppStruct(_) -> false
   | _ ->
      (match inferredType with
      | TCppInst _
      | TCppClass
      | TCppEnum _
         -> (tcpp_to_string funcType) <> (tcpp_to_string inferredType)
      | _ -> false
   )
;;

let cpp_member_name_of member =
   let rename = get_meta_string member.cf_meta Meta.Native in
   if rename <> "" then
      rename
   else
      keyword_remap member.cf_name
;;

let cpp_is_templated_call ctx member =
   has_meta_key member.cf_meta Meta.TemplatedCall
;;

let cpp_is_static_extension ctx member =
   has_meta_key member.cf_meta Meta.NativeStaticExtension
;;


let cpp_template_param path native =
   let path = "::" ^ (join_class_path_remap (path) "::" ) in
   if (native) then
      path
   else match path with
   | "::Array" -> "hx::ArrayBase"
   | "::Int" -> "int"
   | "::Bool" -> "bool"
   | x -> x
;;


let cpp_append_block block expr =
   match block.cppexpr with
   | CppBlock(expr_list, closures, gc_stack) ->
       { block with cppexpr = CppBlock( expr_list @ [expr], closures, gc_stack) }
   | _ -> abort "Internal error appending expression" block.cpppos
;;



let cpp_enum_name_of field =
   let rename = get_meta_string field.ef_meta Meta.Native in
   if rename <> "" then
      rename
   else
      keyword_remap field.ef_name
;;

let is_gc_element ctx member_type =
  Common.defined ctx.ctx_common Define.HxcppGcGenerational &&
  match member_type with
   | TCppDynamic
   | TCppObject
   | TCppObjectPtr
   | TCppEnum _
   | TCppString
   | TCppFunction _
   | TCppDynamicArray
   | TCppObjectArray _
   | TCppWrapped _
   | TCppScalarArray _
   | TCppInst _
   | TCppInterface _
   | TCppClass
       -> true
   | _ -> false

;;



let retype_expression ctx request_type function_args function_type expression_tree forInjection =
   let rev_closures = ref [] in
   let closureId = ref 0 in
   let declarations = ref (Hashtbl.create 0) in
   let undeclared = ref (Hashtbl.create 0) in
   let uses_this = ref None in
   let gc_stack = ref false in
   let injection = ref forInjection in
   let this_real = ref (if ctx.ctx_real_this_ptr then ThisReal else ThisDynamic) in
   let file_id = ctx.ctx_file_id in
   let function_return_type = ref (cpp_type_of ctx function_type) in
   let loop_stack = ref [] in
   let forCppia = Common.defined ctx.ctx_common Define.Cppia in
   let alloc_file_id () =
      incr file_id;
      !file_id
   in
   let begin_loop () =
      loop_stack := (alloc_file_id (),ref false) :: !loop_stack;
      (fun () -> match !loop_stack with
         | (label_id,used) :: tl ->
            loop_stack := tl;
            if !used then label_id else -1
         | [] ->
            abort "Invalid inernal loop handling" expression_tree.epos
      )
   in

   (* '__trace' is at the top-level *)
   Hashtbl.add !declarations "__trace" ();
   List.iter (fun arg -> Hashtbl.add !declarations arg.v_name () ) function_args;

   let rec to_lvalue value =
      match value.cppexpr with
      | CppVar( VarClosure(var) as varloc) when is_gc_element ctx (cpp_type_of ctx var.v_type) ->
           CppVarRef(varloc), true
      | CppVar( VarThis(member,_) as varloc) when is_gc_element ctx (cpp_type_of ctx member.cf_type) ->
           CppVarRef(varloc), true
      | CppVar( VarInstance(obj,member,_,"->") as varloc) when is_gc_element ctx (cpp_type_of ctx member.cf_type) ->
           CppVarRef(varloc), true
      | CppVar varloc -> CppVarRef(varloc), false

      | CppArray arrayloc ->
         CppArrayRef(arrayloc), (match arrayloc with
         | ArrayObject(arrayObj, index, _) when (is_gc_element ctx TCppDynamic) -> true
         | ArrayTyped(arrayObj, index, t) when (is_gc_element ctx t) -> true
         | _ -> false)

      | CppDynamicField(expr, name) -> CppDynamicRef(expr,name), false
      | CppTCast(cppExpr,_)
      | CppCast(cppExpr,_)
      | CppCastStatic(cppExpr,_)
      | CppCastObjC(cppExpr,_)
      | CppCastObjCBlock(cppExpr,_,_)
      | CppCastScalar(cppExpr,_) -> to_lvalue cppExpr
      | CppCastVariant(cppExpr) -> to_lvalue cppExpr
      | CppExtern(name,isGlobal) -> CppExternRef(name,isGlobal), false

      | _ -> abort ("Could not convert expression to l-value (" ^ s_tcpp value.cppexpr ^ ")") value.cpppos
   in

   let rec retype return_type expr =
      let cpp_type_of t = cpp_type_of ctx t in
      let mk_cppexpr newExpr newType = { cppexpr = newExpr; cpptype = newType; cpppos = expr.epos } in
      let retype_function_args args arg_types =
         let rec map_pair args types result=
            match args, types with
            | args, [TCppRest(rest)] -> (List.rev (List.map (retype rest) args) ) @ result
            | [], [] -> result
            | a::arest, t::trest -> map_pair arest trest ((retype t a) :: result )
            | _, [] -> abort ("Too many args") expr.epos
            | [], _ -> abort ("Too many types") expr.epos
         in
         List.rev (map_pair args arg_types [])
      in

      let retypedExpr, retypedType =
         match expr.eexpr with
         | TEnumParameter( enumObj, enumField, enumIndex  ) ->
            let retypedObj = retype TCppDynamic enumObj in
            CppEnumParameter( retypedObj, enumField, enumIndex ), cpp_cast_variant_type_of (cpp_type_of (get_nth_type enumField enumIndex))

         | TEnumIndex enumObj ->
            let retypedObj = retype TCppDynamic enumObj in
            CppEnumIndex retypedObj, TCppScalar "int"

         | TConst TThis ->
            uses_this := Some !this_real;
            CppThis(!this_real), if !this_real=ThisDynamic then TCppDynamic else cpp_type_of expr.etype

         | TConst TSuper ->
            uses_this := Some !this_real;
            CppSuper(!this_real), if !this_real=ThisDynamic then TCppDynamic else cpp_type_of expr.etype

         | TConst TNull when is_objc_type expr.etype ->
            CppNil, TCppNull

         | TConst x ->
            cpp_const_type x

         | TIdent "__global__" ->
            (* functions/vars will appear to be members of the virtual global object *)
            CppClassOf(([],""),false), TCppGlobal

         | TLocal tvar ->
            let name = tvar.v_name in
            if (Hashtbl.mem !declarations name) then begin
               (*print_endline ("Using existing tvar " ^ tvar.v_name);*)
               CppVar(VarLocal(tvar)), cpp_type_of tvar.v_type
            end else begin
               (*print_endline ("Missing tvar " ^ tvar.v_name);*)
               Hashtbl.replace !undeclared name tvar;
               if tvar.v_capture then
                  CppVar(VarClosure(tvar)), cpp_type_of tvar.v_type
               else
                  CppExtern(name,false), cpp_type_of tvar.v_type
            end

         | TIdent name ->
             CppExtern(name,false), return_type

         | TBreak ->
            if forCppia then
               CppBreak, TCppVoid
            else begin match !loop_stack with
               | [] ->
                  CppBreak, TCppVoid
               | (label_id,used) :: _ ->
                  used := true;
                  (CppGoto label_id),TCppVoid
            end

         | TContinue ->
            CppContinue, TCppVoid

         | TThrow e1 ->
            CppThrow(retype TCppDynamic e1), TCppVoid

         | TMeta( (Meta.Fixed,_,_),e) ->
            let cppType = retype return_type e in
            (match cppType.cppexpr with
            | CppObjectDecl(def,false) -> CppObjectDecl(def,true), cppType.cpptype
            | _ -> cppType.cppexpr, cppType.cpptype
            )

         | TMeta(_,e)
         | TParenthesis e ->
            let cppType = retype return_type e in
            cppType.cppexpr, cppType.cpptype

         | TField( obj, field ) ->
            (match field with
            | FInstance (clazz,params,member)
            | FClosure (Some (clazz,params),member) ->
               let funcReturn = cpp_member_return_type ctx member in
               let clazzType = cpp_instance_type ctx clazz params in
               let retypedObj = retype clazzType obj in
               let exprType = cpp_type_of member.cf_type in
               let is_objc = is_cpp_objc_type retypedObj.cpptype in

               if retypedObj.cpptype=TCppNull then
                  CppNullAccess, TCppDynamic
               else if retypedObj.cpptype=TCppDynamic && not clazz.cl_interface then begin
                  if is_internal_member member.cf_name then
                    CppFunction( FuncInstance(retypedObj,InstPtr,member), funcReturn ), exprType
                  else
                     CppDynamicField(retypedObj, member.cf_name), TCppVariant
               end else if cpp_is_struct_access retypedObj.cpptype then begin

                  match retypedObj.cppexpr with
                  | CppThis ThisReal ->
                      CppVar(VarThis(member, retypedObj.cpptype)), exprType
                  | _ -> if (is_var_field member) then
                         CppVar( VarInstance(retypedObj,member,tcpp_to_string clazzType, ".") ), exprType
                     else
                         CppFunction( FuncInstance(retypedObj,InstStruct,member), funcReturn ), exprType

               end else if is_var_field member then begin

                  let exprType = match retypedObj.cpptype, exprType with
                       | TCppPointer(_,t), TCppDynamic
                       | TCppRawPointer(_,t), TCppDynamic (* the 'type parameter' will show up as Dynamic *)
                          -> t
                       | _ -> exprType
                  in

                  match retypedObj.cppexpr with
                  | CppThis ThisReal ->
                     CppVar(VarThis(member, retypedObj.cpptype) ), exprType
                  | _ ->
                     (match retypedObj.cpptype, member.cf_name with
                     (* Special variable remapping ... *)
                     | TCppDynamicArray, "length" when (  not forCppia )->
                        CppCall(FuncInternal(retypedObj,"get_length","->"),[]), exprType

                     | TCppInterface _,_
                     | TCppDynamic,_ ->
                        CppDynamicField(retypedObj, member.cf_name), TCppVariant
                     | TCppObjC _,_ ->
                        CppVar(VarInstance(retypedObj,member,tcpp_to_string clazzType, ".") ), exprType

                     | _ ->
                        let operator = if cpp_is_struct_access retypedObj.cpptype || retypedObj.cpptype=TCppString then "." else "->" in
                        CppVar(VarInstance(retypedObj,member,tcpp_to_string clazzType, operator) ), exprType
                     )
               end else if (clazz.cl_interface && not is_objc (* Use instance call for objc interfaces *)) then
                  CppFunction( FuncInterface(retypedObj,clazz,member), funcReturn ), exprType
               else begin
                  let isArrayObj = match retypedObj.cpptype with
                     | TCppDynamicArray
                     | TCppObjectArray _
                     | TCppScalarArray _
                         -> true
                     | _ -> false in
                 (* Special array return values *)
                 let funcReturn =
                    if isArrayObj then match member.cf_name with
                       | "map" -> TCppDynamicArray
                       | "splice"
                       | "slice"
                       | "concat"
                       | "copy"
                       |  "filter" -> retypedObj.cpptype
                       | _ -> funcReturn
                    else match retypedObj.cpptype, funcReturn with
                       | TCppPointer(_,t), TCppDynamic
                       | TCppRawPointer(_,t), TCppDynamic (* the 'type parameter' will show up as Dynamic *)
                          -> t
                       | _ -> funcReturn
                 in
                 (match retypedObj.cppexpr with
                 | CppThis ThisReal ->
                    CppFunction( FuncThis(member, retypedObj.cpptype), funcReturn ), exprType
                 | CppSuper this ->
                    CppFunction( FuncSuper(this, retypedObj.cpptype,member), funcReturn ), exprType
                 | _ ->
                    CppFunction( FuncInstance(retypedObj,(if is_objc then InstObjC else InstPtr),member), funcReturn ), exprType
                 )
               end

            | FStatic ( _, ({cf_name="nativeFromStaticFunction"} as member) ) ->
               let funcReturn = cpp_member_return_type ctx member in
               let exprType = cpp_type_of member.cf_type in
               CppFunction( FuncFromStaticFunction, funcReturn ), exprType

            | FStatic (clazz,member) ->
               let funcReturn = cpp_member_return_type ctx member in
               let exprType = cpp_type_of member.cf_type in
               let objC = is_objc_class clazz in
               if is_var_field member then
                  CppVar(VarStatic(clazz, objC, member)), exprType
               else
                  CppFunction( FuncStatic(clazz,objC,member), funcReturn ), exprType
            | FClosure (None,field)
            | FAnon field ->
               let obj = retype TCppDynamic obj in
               let fieldName = field.cf_name in
               if obj.cpptype=TCppGlobal then
                  CppExtern(fieldName,true), cpp_type_of expr.etype
               else if obj.cpptype=TCppNull then
                  CppNullAccess, TCppDynamic
               else if is_internal_member fieldName then begin
                  let cppType = cpp_return_type ctx expr.etype in
                  if obj.cpptype=TCppString then
                     CppFunction( FuncInternal(obj,fieldName,"."), cppType), cppType
                  else
                     CppFunction( FuncInternal(obj,fieldName,"->"), cppType), cppType
               end else
                  CppDynamicField(obj, field.cf_name), TCppVariant

            | FDynamic fieldName ->
               let obj = retype TCppDynamic obj in
               if obj.cpptype=TCppNull then
                  CppNullAccess, TCppDynamic
               else if fieldName="cca" && obj.cpptype=TCppString then
                  CppFunction( FuncInternal(obj,"cca","."), TCppScalar("int")), TCppDynamic
               else if fieldName="__s" && obj.cpptype=TCppString then
                  CppVar( VarInternal(obj,".","__s")), TCppPointer("ConstPointer", TCppScalar("char"))
               else if fieldName="__Index" then
                  CppEnumIndex(obj), TCppScalar("int")
               else if is_internal_member fieldName || cpp_is_real_array obj then begin
                  let cppType = cpp_return_type ctx expr.etype in
                  if obj.cpptype=TCppString then
                     CppFunction( FuncInternal(obj,fieldName,"."), cppType), cppType
                  else
                     CppFunction( FuncInternal(obj,fieldName,"->"), cppType), cppType
               end else if (obj.cpptype=TCppGlobal) then
                  CppExtern(fieldName,true), cpp_type_of expr.etype
               else if (obj.cpptype=TCppClass) then begin
                  match obj.cppexpr with
                  | CppClassOf(path,_) ->
                     CppExtern ( (join_class_path_remap path "::" ) ^ "_obj::" ^ fieldName, true ), cpp_type_of expr.etype
                  | _ ->
                     CppVar( VarInternal(obj,"->",fieldName)), cpp_type_of expr.etype
               end else
                  CppDynamicField(obj, fieldName), TCppVariant

            | FEnum (enum, enum_field) ->
                  CppEnumField(enum, enum_field), TCppEnum(enum)
            )

         | TCall( {eexpr = TIdent "__cpp__"}, arg_list ) ->
            let  cppExpr = match arg_list with
            | [{ eexpr = TConst (TString code) }] -> CppCode(code, [])
            | ({ eexpr = TConst (TString code) }) :: remaining ->
                  let retypedArgs = List.map (fun arg -> retype (TCppCode(cpp_type_of arg.etype)) arg) remaining in
                  CppCode(code, retypedArgs)
            | _ -> abort "__cpp__'s first argument must be a string" expr.epos;
            in
            cppExpr, TCppCode(cpp_type_of expr.etype)

         | TCall( func, args ) ->
            let retypedFunc = retype TCppUnchanged func in
            (match retypedFunc.cpptype with
            | TCppNull ->
               CppNullAccess, TCppDynamic
            | TCppFunction(argTypes,retType,_) ->
               let retypedArgs = retype_function_args args argTypes in
               CppCall( FuncExpression(retypedFunc) ,retypedArgs), retType
            |  TCppObjCBlock(argTypes,retType) ->
               let retypedArgs = retype_function_args args argTypes in
               CppCall( FuncExpression(retypedFunc) ,retypedArgs), retType

            | _ ->
               let cppType = cpp_type_of expr.etype in
               (match retypedFunc.cppexpr with
               | CppFunction(FuncFromStaticFunction ,returnType) ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  ( match retypedArgs with
                  | [ {cppexpr=CppFunction( FuncStatic(clazz,false,member), funcReturn)} ] ->
                     CppFunctionAddress(clazz,member), funcReturn
                  | _ -> abort "cpp.Function.fromStaticFunction must be called on static function" expr.epos;
                  )
               | CppEnumIndex(_) ->
                  (* Not actually a TCall...*)
                  retypedFunc.cppexpr, retypedFunc.cpptype

               | CppFunction( FuncInstance(obj, InstPtr, member), _ ) when not forCppia && return_type=TCppVoid && is_array_splice_call obj member ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  CppCall( FuncInstance(obj, InstPtr, {member with cf_name="removeRange"}), retypedArgs), TCppVoid

               | CppFunction( FuncInstance(obj, InstPtr, member), _ ) when is_array_concat_call obj member ->
                  let retypedArgs = List.map (retype obj.cpptype) args in
                  CppCall( FuncInstance(obj, InstPtr, member), retypedArgs), return_type

               | CppFunction( FuncStatic(obj, false, member), _ ) when member.cf_name = "hx::AddressOf" ->
                    let arg = retype TCppUnchanged (List.hd args) in
                    CppAddressOf(arg), TCppRawPointer("", arg.cpptype)

               | CppFunction( FuncStatic(obj, false, member), _ ) when member.cf_name = "_hx_create_array_length" ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  (* gc_stack - not needed yet *)
                  (match return_type with
                  | TCppObjectArray _
                  | TCppScalarArray _ -> CppCall( FuncNew(return_type), retypedArgs), return_type
                  | _ -> CppCall( FuncNew(TCppDynamicArray), retypedArgs), return_type
                  )

               | CppFunction( FuncStatic(obj, false, member), returnType ) when cpp_is_templated_call ctx member ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  (match retypedArgs with
                  | {cppexpr = CppClassOf(path,native) }::rest ->
                      CppCall( FuncTemplate(obj,member,path,native), rest), returnType
                  | _ -> abort "First parameter of template function must be a Class" retypedFunc.cpppos
                  )

               | CppFunction( FuncInstance(obj, InstPtr, member), _ ) when is_map_get_call obj member ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  let fname, cppType = match return_type with
                  | TCppVoid | TCppScalar("bool")  -> (if forCppia then "getBool" else "get_bool"), return_type
                  | TCppScalar("int")  -> (if forCppia then "getInt" else "get_int"), return_type
                  | TCppScalar("Float")  -> (if forCppia then "getFloat" else "get_float"), return_type
                  | TCppString  -> (if forCppia then "getString" else "get_string"), return_type
                  | _ -> "get", TCppDynamic
                  in
                  let func = FuncInstance(obj, InstPtr, {member with cf_name=fname}) in
                  (*
                  if  cpp_can_static_cast cppType return_type then begin
                     let call = mk_cppexpr (CppCall(func,retypedArgs)) cppType in
                     CppCastStatic(call, cppType), cppType
                  end else
                  *)
                     CppCall( func, retypedArgs), cppType


               | CppFunction( FuncInstance(obj, InstPtr, member), _ ) when forCppia && is_map_set_call obj member ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  let fname = match retypedArgs with
                  | [_;{cpptype=TCppScalar("bool")}]  -> "setBool"
                  | [_;{cpptype=TCppScalar("int")}]  -> "setInt"
                  | [_;{cpptype=TCppScalar("Float")}]  -> "setFloat"
                  | [_;{cpptype=TCppString}]  -> "setString"
                  | _ -> "set"
                  in
                  let func = FuncInstance(obj, InstPtr, {member with cf_name=fname}) in
                  CppCall( func, retypedArgs), cppType


               | CppFunction( FuncInstance(obj,InstPtr,member) as func, returnType ) when cpp_can_static_cast returnType cppType ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  let call = mk_cppexpr (CppCall(func,retypedArgs)) returnType in
                  CppCastStatic(call, cppType), cppType
                  (*
                  let error_printer file line = Printf.sprintf "%s:%d:" file line in
                  let epos = Lexer.get_error_pos error_printer expr.epos in
                  print_endline ( "fixed override " ^ member.cf_name ^ " @ " ^  epos ^ " " ^ (tcpp_to_string returnType) ^ "->" ^ (ctx_type_string ctx expr.etype) );
                  CppCall(func,retypedArgs), returnType
                  *)

               (* Other functions ... *)
               | CppFunction( FuncInstance(_,_,{cf_type=TFun(arg_types,_)} ) as func, returnType )
               | CppFunction( FuncStatic(_,_,{cf_type=TFun(arg_types,_)} ) as func, returnType )
               | CppFunction( FuncThis({cf_type=TFun(arg_types,_)},_ ) as func, returnType ) ->
                  let arg_types = List.map (fun (_,opt,t) -> cpp_tfun_arg_type_of ctx opt t) arg_types in
                  (* retype args specifically (not just CppDynamic) *)
                  let retypedArgs = retype_function_args args arg_types in
                  CppCall(func,retypedArgs), returnType

               | CppFunction(func,returnType) ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  CppCall(func,retypedArgs), returnType

               | CppEnumField(enum, field) ->
                  (* TODO - proper re-typing *)
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  CppCall( FuncEnumConstruct(enum,field),retypedArgs), cppType

               | CppSuper(_) ->
                  (* TODO - proper re-typing *)
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  CppCall( FuncSuperConstruct(retypedFunc.cpptype) ,retypedArgs), TCppVoid

               | CppDynamicField(expr,name) ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  (* Special function calls *)
                  (match expr.cpptype, name with
                  | TCppGlobal, _  ->
                     let retypedArgs = List.map (retype TCppUnchanged ) args in
                     CppCall( FuncExtern(name,true),retypedArgs), cppType

                  | TCppString, _  ->
                     CppCall( FuncInternal(expr,name,"."),retypedArgs), cppType

                  | _, "__Tag"  ->
                     CppCall( FuncInternal(expr,"_hx_getTag","->"),retypedArgs), cppType

                  | _, name when is_internal_member name ->
                     CppCall( FuncInternal(expr,name,"->"),retypedArgs), cppType

                  | _ -> (* not special *)
                     CppCall( FuncExpression(retypedFunc), retypedArgs), TCppDynamic
                  )

               |  CppExtern(name,isGlobal) ->
                  let retypedArgs = List.map (retype TCppUnchanged ) args in
                  CppCall( FuncExtern(name,isGlobal) ,retypedArgs), cppType

               | _ ->
                  let retypedArgs = List.map (retype TCppDynamic ) args in
                  CppCall( FuncExpression(retypedFunc), retypedArgs), TCppDynamic
               )
            )

         | TNew (class_def,params,args) ->
            let rec find_constructor c = (match c.cl_constructor, c.cl_super with
            | (Some constructor), _  -> constructor.cf_type
            | _ , Some (super,_)  -> find_constructor super
            | _ -> abort "TNew without constructor " expr.epos
            ) in
            let constructor_type = find_constructor class_def in
            let arg_types, _ = cpp_function_type_of_args_ret ctx constructor_type in
            let retypedArgs = retype_function_args args arg_types in
            let created_type = cpp_type_of expr.etype in
            gc_stack := !gc_stack || (match created_type with | TCppInst(_) -> true | _ -> false );
            CppCall( FuncNew(created_type), retypedArgs), created_type

         | TFunction func ->
            let old_this_real = !this_real in
            this_real := ThisFake;
            (* TODO - this_dynamic ? *)
            let old_undeclared = Hashtbl.copy !undeclared in
            let old_declarations = Hashtbl.copy !declarations in
            let old_uses_this = !uses_this in
            let old_gc_stack = !gc_stack in
            let old_return_type = !function_return_type in
            let ret =cpp_type_of func.tf_type in
            function_return_type := ret;
            uses_this := None;
            undeclared := Hashtbl.create 0;
            declarations := Hashtbl.create 0;
            List.iter ( fun (tvar,_) ->
               Hashtbl.add !declarations tvar.v_name () ) func.tf_args;
            let cppExpr = retype TCppVoid (mk_block func.tf_expr) in
            let result = { close_expr=cppExpr;
                           close_id= !closureId;
                           close_undeclared= !undeclared;
                           close_type= ret;
                           close_args= func.tf_args;
                           close_this= !uses_this;
                         } in
            incr closureId;
            declarations := old_declarations;
            undeclared := old_undeclared;
            Hashtbl.iter (fun name tvar ->
               if not (Hashtbl.mem !declarations name) then
                  Hashtbl.replace !undeclared name tvar;
            ) result.close_undeclared;
            function_return_type := old_return_type;
            this_real := old_this_real;
            uses_this := if !uses_this != None then Some old_this_real else old_uses_this;
            gc_stack := old_gc_stack;
            rev_closures := result:: !rev_closures;
            CppClosure(result), TCppDynamic

         | TArray (e1,e2) ->
            let retypedObj = retype TCppDynamic e1 in
            let retypedIdx = retype (TCppScalar("int")) e2 in
            let arrayExpr, elemType = (match retypedObj.cpptype with
              | TCppScalarArray scalar ->
                 CppArray( ArrayTyped(retypedObj,retypedIdx,scalar) ), scalar
              | TCppPointer (_,elem) ->
                 CppArray( ArrayPointer(retypedObj, retypedIdx) ), elem
              | TCppRawPointer (_,elem) ->
                 CppArray( ArrayRawPointer(retypedObj, retypedIdx) ), elem
              | TCppObjectArray TCppDynamic ->
                 CppArray( ArrayObject(retypedObj,retypedIdx,TCppDynamic) ), TCppDynamic
              | TCppObjectArray elem ->
                 CppArray( ArrayObject(retypedObj,retypedIdx,elem) ), elem
              | TCppInst({cl_array_access = Some _ } as klass) ->
                 CppArray( ArrayImplements(klass, retypedObj,retypedIdx) ), cpp_type_of expr.etype
              | TCppDynamicArray ->
                 CppArray( ArrayVirtual(retypedObj, retypedIdx) ), TCppDynamic
              | _ ->
                 CppArray( ArrayDynamic(retypedObj, retypedIdx) ), TCppDynamic
            ) in
            let returnType = cpp_type_of expr.etype in
            if cpp_can_static_cast elemType returnType then
               CppCastStatic(mk_cppexpr arrayExpr returnType, returnType), returnType
            else
               arrayExpr, elemType

         | TTypeExpr module_type ->
            let path = t_path module_type in
            CppClassOf(path, is_native_gen_module module_type), TCppClass

         | TBinop (op,left,right) ->
            let binOpType = match op with
            | OpDiv -> TCppScalar("Float")
            | OpBoolAnd | OpBoolOr -> TCppScalar("bool")
            | OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr -> TCppScalar("int")
            | OpAssign -> (retype TCppUnchanged left).cpptype
            | _ -> TCppUnchanged
            in
            let e1 = retype binOpType left in
            let e2 = retype binOpType right in

            let complex = (is_complex_compare e1.cpptype) || (is_complex_compare e2.cpptype) in
            let e1_null = e1.cpptype=TCppNull in
            let e2_null = e2.cpptype=TCppNull in
            let reference = match op with
               | OpAssign ->
                  let lvalue, gc = to_lvalue e1 in
                  if gc then gc_stack := true;
                  CppSet(lvalue, e2)
               | OpAssignOp op ->
                  let lvalue, gc = to_lvalue e1 in
                  if gc then gc_stack := true;
                  CppModify(op, lvalue, e2)
               | OpEq when    e1_null && e2_null-> CppBool(true)
               | OpGte when   e1_null && e2_null-> CppBool(true)
               | OpLte when   e1_null && e2_null-> CppBool(true)
               | OpNotEq when e1_null && e2_null-> CppBool(false)
               | _ when   e1_null && e2_null-> CppBool(false)

               | OpEq when    e1_null -> CppNullCompare("IsNull", e2)
               | OpGte when   e1_null -> CppNullCompare("IsNull", e2)
               | OpLte when   e1_null -> CppNullCompare("IsNull", e2)
               | OpNotEq when e1_null -> CppNullCompare("IsNotNull", e2)

               | OpEq when    e2_null -> CppNullCompare("IsNull", e1)
               | OpGte when   e2_null -> CppNullCompare("IsNull", e1)
               | OpLte when   e2_null -> CppNullCompare("IsNull", e1)
               | OpNotEq when e2_null -> CppNullCompare("IsNotNull", e1)

               | OpEq when complex -> CppCompare("IsEq", e1, e2, op)
               | OpNotEq when complex -> CppCompare("IsNotEq", e1, e2, op)
               | OpGte when complex -> CppCompare("IsGreaterEq", e1, e2, op)
               | OpLte when complex -> CppCompare("IsLessEq", e1, e2, op)
               | OpGt when complex -> CppCompare("IsGreater", e1, e2, op)
               | OpLt  when complex -> CppCompare("IsLess", e1, e2, op)

               | _ -> CppBinop(op,e1,e2)
            in
            reference, cpp_type_of expr.etype

         | TUnop (op,pre,e1) ->
            let targetType = match op with
            | Not -> TCppScalar("bool")
            | NegBits -> TCppScalar("int")
            | _ -> cpp_type_of e1.etype
            in

            let e1 = retype targetType e1 in
            let reference = match op with
               | Increment ->
                   let lvalue, gc = to_lvalue e1 in
                   if gc then gc_stack := true;
                   CppCrement( CppIncrement, pre, lvalue)
               | Decrement ->
                   let lvalue, gc = to_lvalue e1 in
                   if gc then gc_stack := true;
                   CppCrement( CppDecrement, pre, lvalue)
               | Neg -> CppUnop(CppNeg,e1)
               | Not -> CppUnop(CppNot,e1)
               | NegBits -> CppUnop(CppNegBits,e1)
            in reference, cpp_type_of expr.etype

         | TFor (v,init,block) ->
            let old_declarations = Hashtbl.copy !declarations in
            Hashtbl.add !declarations v.v_name ();
            let init = retype (cpp_type_of v.v_type) init in
            let block = retype TCppVoid (mk_block block) in
            declarations := old_declarations;
            CppFor(v,init,block), TCppVoid

         | TWhile (e1,e2,flag) ->
            let condition = retype (TCppScalar("bool")) e1 in
            let close = begin_loop() in
            let block = retype TCppVoid (mk_block e2) in
            CppWhile(condition, block, flag, close()), TCppVoid

         | TArrayDecl el ->
            let retypedEls = List.map (retype TCppDynamic) el in
            CppArrayDecl(retypedEls), cpp_type_of expr.etype

         | TBlock expr_list ->
            let inject = !injection in
            injection := false;
            if (return_type<>TCppVoid) && not forCppia then
               print_endline ("Value from a block not handled " ^
               (expr.epos.pfile ) ^ " " ^  (string_of_int (Lexer.get_error_line expr.epos) ));

            let old_declarations = Hashtbl.copy !declarations in
            let old_closures = !rev_closures in
            rev_closures := [];
            let local_closures = ref [] in
            let remaining = ref (List.length expr_list) in
            let cppExprs = List.map ( fun expr ->
                  let targetType = if inject && (!remaining=1) then cpp_type_of expr.etype else TCppVoid in
                  decr remaining;
                  let result = retype targetType expr in
                  local_closures := !rev_closures @ !local_closures;
                  rev_closures := [];
                  result
               ) expr_list in
            declarations := old_declarations;
            rev_closures := old_closures;

            CppBlock(cppExprs, List.rev !local_closures, !gc_stack ), TCppVoid

         | TObjectDecl (
            (("fileName",_,_) , { eexpr = (TConst (TString file)) }) ::
               (("lineNumber",_,_) , { eexpr = (TConst (TInt line)) }) ::
                  (("className",_,_) , { eexpr = (TConst (TString class_name)) }) ::
                     (("methodName",_,_), { eexpr = (TConst (TString meth)) }) :: [] ) ->
              CppPosition(file,line,class_name,meth), TCppDynamic

         | TObjectDecl el ->
            let retypedEls = List.map ( fun((v,_,_),e) -> v, retype TCppDynamic e) el in
            (match return_type with
            | TCppVoid -> CppObjectDecl(retypedEls,false), TCppVoid
            | _ -> CppObjectDecl(retypedEls,false), TCppDynamic
            )

         | TVar (v,eo) ->
            let varType = cpp_type_of v.v_type in
            let init = match eo with None -> None | Some e -> Some (retype varType e) in
            Hashtbl.add !declarations v.v_name ();
            CppVarDecl(v, init), varType

         | TIf (ec,e1,e2) ->
            let ec = retype (TCppScalar("bool")) ec in
            let blockify =  if return_type!=TCppVoid then fun e -> e else mk_block in
            let e1 = retype return_type (blockify e1) in
            let e2 = match e2 with None->None | Some e -> Some (retype return_type (blockify e))
            in
            CppIf(ec, e1, e2), if return_type=TCppVoid then TCppVoid else cpp_type_of expr.etype

          (* Switch internal return - wrap whole thing in block  *)
         | TSwitch (condition,cases,def) ->
            if return_type<>TCppVoid then
               abort "Value from a switch not handled" expr.epos;

            let conditionType = cpp_type_of condition.etype in
            let condition = retype conditionType condition in
            let cppDef = match def with None -> None | Some e -> Some (retype TCppVoid (mk_block e)) in
            if forCppia then begin
               let cases = List.map (fun (el,e2) ->
                  let cppBlock = retype TCppVoid (mk_block e2) in
                  (List.map (retype conditionType) el), cppBlock ) cases in
               CppSwitch(condition, conditionType, cases, cppDef, -1), TCppVoid
            end else (try
               (match conditionType with TCppScalar("int") | TCppScalar("bool") -> () | _ -> raise Not_found );
               let cases = List.map (fun (el,e2) ->
                  (List.map const_int_of el), (retype TCppVoid (mk_block e2)) ) cases in
               CppIntSwitch(condition, cases, cppDef), TCppVoid
            with Not_found ->
               let label = alloc_file_id () in
               (* do something better maybe ... *)
               let cases = List.map (fun (el,e2) ->
                  let cppBlock = retype TCppVoid (mk_block e2) in
                  let gotoExpr = { cppexpr = CppGoto(label); cpptype = TCppVoid; cpppos = e2.epos } in
                  let cppBlock = cpp_append_block cppBlock  gotoExpr in
                  (List.map (retype conditionType) el), cppBlock ) cases in
               CppSwitch(condition, conditionType, cases, cppDef, label), TCppVoid
            )

         | TTry (try_block,catches) ->
            (* TTry internal return - wrap whole thing in block ? *)
            if return_type<>TCppVoid then
               abort "Value from a try-block not handled" expr.epos;
            let cppBlock = retype TCppVoid try_block in
            let cppCatches = List.map (fun (tvar,catch_block) ->
                let old_declarations = Hashtbl.copy !declarations in
                Hashtbl.add !declarations tvar.v_name ();
                let cppCatchBlock = retype TCppVoid catch_block in
                declarations := old_declarations;
                tvar, cppCatchBlock;
            ) catches in
            CppTry(cppBlock, cppCatches), TCppVoid

         | TReturn eo ->
            CppReturn(match eo with None -> None | Some e -> Some (retype (!function_return_type) e)), TCppVoid

         | TCast (base,None) -> (* Use auto-cast rules *)
            let return_type = cpp_type_of expr.etype in
            let baseCpp = retype (return_type) base in
            let baseStr = (tcpp_to_string baseCpp.cpptype) in
            let returnStr = (tcpp_to_string return_type) in
            if baseStr=returnStr then
               baseCpp.cppexpr, baseCpp.cpptype (* nothing to do *)
            else (match return_type with
               | TCppObjC(k) -> CppCastObjC(baseCpp,k), return_type
               | TCppPointer(_,_)
               | TCppInst(_) -> CppCast(baseCpp,return_type), return_type
               | TCppString -> CppCastScalar(baseCpp,"::String"), return_type
               | TCppCode(t) when baseStr <> (tcpp_to_string t)  ->
                     CppCast(baseCpp, t),  t
               | TCppNativePointer(klass) -> CppCastNative(baseCpp), return_type
               | TCppObjCBlock(args,ret) -> CppCastObjCBlock(baseCpp,args,ret), return_type
               | TCppProtocol(p) -> CppCastProtocol(baseCpp,p), return_type
               | TCppDynamic when baseCpp.cpptype=TCppClass ->  CppCast(baseCpp,TCppDynamic), TCppDynamic
               | _ -> baseCpp.cppexpr, baseCpp.cpptype (* use autocasting rules *)
            )

         | TCast (base,Some t) ->
            let baseCpp = retype (cpp_type_of base.etype) base in
            let baseStr = (tcpp_to_string baseCpp.cpptype) in
            let return_type = if return_type=TCppUnchanged then cpp_type_of expr.etype else return_type in
            let returnStr = (tcpp_to_string return_type) in

            if baseStr=returnStr then
               baseCpp.cppexpr, baseCpp.cpptype (* nothing to do *)
            else (match return_type with
            | TCppNativePointer(klass) -> CppCastNative(baseCpp), return_type
            | TCppVoid -> baseCpp.cppexpr, TCppVoid
            | TCppInterface _ ->
                  baseCpp.cppexpr, return_type
            | TCppDynamic ->
                  baseCpp.cppexpr, baseCpp.cpptype
            | _ ->
               CppTCast(baseCpp, return_type), return_type
            )
      in
      let cppExpr = mk_cppexpr retypedExpr retypedType in

      (* Autocast rules... *)
      if return_type=TCppVoid then
         mk_cppexpr retypedExpr TCppVoid
      else if return_type=TCppVarArg then begin
         match cpp_variant_type_of cppExpr.cpptype with
         | TCppVoidStar
         | TCppScalar _ -> cppExpr
         | TCppString ->  mk_cppexpr (CppVar(VarInternal(cppExpr,".","__s"))) (TCppPointer("ConstPointer", TCppScalar("char")))
         | TCppDynamic ->  mk_cppexpr (CppCastNative(cppExpr)) TCppVoidStar
         | _ -> let toDynamic = mk_cppexpr (CppCast(cppExpr, TCppDynamic)) TCppDynamic in
                mk_cppexpr (CppCastNative(toDynamic)) TCppVoidStar
      end else if (cppExpr.cpptype=TCppVariant || cppExpr.cpptype=TCppDynamic || cppExpr.cpptype==TCppObject) then begin
         match return_type with
         | TCppUnchanged -> cppExpr
         | TCppInst(t) when (has_meta_key t.cl_meta Meta.StructAccess) ->
             let structType = TCppStruct( TCppInst(t) ) in
             let structCast =  mk_cppexpr (CppCast(cppExpr,structType)) structType in
             mk_cppexpr (CppCast(structCast,(TCppInst t))) (TCppInst t)

         | TCppObjectArray _
         | TCppScalarArray _
         | TCppNativePointer _
         | TCppDynamicArray
         | TCppObjectPtr
         | TCppVarArg
         | TCppInst _
             -> mk_cppexpr (CppCast(cppExpr,return_type)) return_type

         | TCppObjC k
             -> mk_cppexpr (CppCastObjC(cppExpr,k)) return_type

         | TCppObjCBlock(ret,args)
             -> mk_cppexpr (CppCastObjCBlock(cppExpr,ret,args)) return_type

         | TCppScalar(scalar)
             -> mk_cppexpr (CppCastScalar(cppExpr,scalar)) return_type

         | TCppString
             -> mk_cppexpr (CppCastScalar(cppExpr,"::String")) return_type

         | TCppInterface _ when cppExpr.cpptype=TCppVariant
              -> mk_cppexpr (CppCastVariant(cppExpr)) return_type

         | TCppDynamic when cppExpr.cpptype=TCppVariant
              -> mk_cppexpr (CppCastVariant(cppExpr)) return_type

         | TCppStar(t,const) ->
             let ptrType = TCppPointer((if const then "ConstPointer" else "Pointer"),t) in
             let ptrCast =  mk_cppexpr (CppCast(cppExpr,ptrType)) ptrType in
             mk_cppexpr (CppCast(ptrCast,TCppStar(t,const))) (TCppStar(t,const))

         | _ -> cppExpr
      end else match cppExpr.cpptype, return_type with
         | _, TCppUnchanged -> cppExpr
         (*
            Using the 'typedef hack', where we use typedef X<T> = T, allows the
            haxe compiler to use these types interchangeably. We then work
            out the correct way to convert between them when one is expected, but another provided.

            TCppFunction: these do not really interact with the haxe function type, T
            Since they are implemented with cpp::Function, conversion to/from Dynamic should happen automatically
               CallableData<T> = T;
               FunctionData<T,ABI> = T;

            TCppObjCBlock can move in and out of Dyanmic
               ObjcBlock<T> = T;

             TCppProtocol can move in and out of Dyanmic, via delegate creation
               Protocol<T /*:interface*/ > = T;

            Explicitly wrapped type - already interacts well with Dynamic and T
              Struct<T> = T;

            TCppStar, TCppStruct, TCppReference - for interacting with native code
              Star<T> = T;
              ConstStar<T> = T;
              Reference<T> = T;
              T may be an extern class, with @:structAccess - in which case
                 Dynamic interaction must be handled explicitly
            These types, plus Dynamic can be used interchangeably by haxe
            Derived/inherited types may also be mixed in
         *)
         | TCppAutoCast, _
         | TCppObjC(_), TCppDynamic
         | TCppObjCBlock(_), TCppDynamic
              -> mk_cppexpr (CppCast(cppExpr,return_type)) return_type

         (* Infer type from right-hand-side for pointer or reference to Dynamic *)
         | TCppReference(TCppDynamic), TCppReference(_) -> cppExpr
         | TCppReference(TCppDynamic),  t ->
             mk_cppexpr retypedExpr (TCppReference(t))
         | TCppStar(TCppDynamic,_), TCppStar(_,_) -> cppExpr
         | TCppStar(TCppDynamic,const),  t ->
             mk_cppexpr retypedExpr (TCppStar(t,const))

         | TCppStar(t,const),  TCppDynamic ->
             let ptrType = TCppPointer((if const then "ConstPointer" else "Pointer"),t) in
             let ptrCast =  mk_cppexpr (CppCast(cppExpr,ptrType)) ptrType in
             mk_cppexpr (CppCast(ptrCast,TCppDynamic)) TCppDynamic


         | TCppStar(t,const), TCppInst _
         | TCppStar(t,const), TCppStruct _ ->
             mk_cppexpr (CppDereference(cppExpr)) return_type

         | TCppInst _, TCppStar(p,const)
         | TCppStruct _, TCppStar(p,const) ->
             mk_cppexpr (CppAddressOf(cppExpr)) return_type

         | TCppObjectPtr, TCppObjectPtr -> cppExpr
         | TCppObjectPtr, _ ->
                mk_cppexpr (CppCast(cppExpr,TCppDynamic)) TCppDynamic

         | TCppProtocol _, TCppProtocol _ -> cppExpr
         | t, TCppProtocol protocol ->
              mk_cppexpr (CppCastProtocol(cppExpr,protocol)) return_type

         | TCppInst(t), TCppDynamic when (has_meta_key t.cl_meta Meta.StructAccess) ->
             let structType = TCppStruct( TCppInst(t) ) in
             let structCast =  mk_cppexpr (CppCast(cppExpr,structType)) structType in
             mk_cppexpr (CppCast(structCast,TCppDynamic)) TCppDynamic

         | _, TCppObjectPtr ->
             mk_cppexpr (CppCast(cppExpr,TCppObjectPtr)) TCppObjectPtr

         | TCppDynamicArray, TCppScalarArray _
         | TCppDynamicArray, TCppObjectArray _
         | TCppScalarArray _, TCppDynamicArray
         | TCppObjectArray _, TCppDynamicArray when forCppia ->
             mk_cppexpr (CppCast(cppExpr,return_type)) return_type
         | TCppScalar(from), TCppScalar(too) when from<>too ->
             mk_cppexpr (CppCastScalar(cppExpr,too)) return_type

         | _ -> cppExpr
   in
   retype request_type expression_tree
;;

type tinject = {
   inj_prologue : bool -> unit;
   inj_setvar : string;
   inj_tail : string;
}

let mk_injection prologue set_var tail =
   Some { inj_prologue=prologue; inj_setvar=set_var; inj_tail=tail }
;;


let cpp_arg_type_name ctx tvar default_val prefix =
   let remap_name = (cpp_var_name_of tvar) in
   let type_str = (cpp_var_type_of ctx tvar) in
   match default_val with
   | Some TNull  -> (tcpp_to_string (cpp_type_of_null ctx tvar.v_type)),remap_name
   | Some constant -> (tcpp_to_string (cpp_type_of_null ctx tvar.v_type)),prefix ^ remap_name
   | _ -> type_str,remap_name
;;


let cpp_gen_default_values ctx args prefix =
   List.iter ( fun (tvar,o) ->
      match o with
      | Some TNull -> ()
      | Some const ->
         let name = cpp_var_name_of tvar in
         ctx.ctx_output ((cpp_var_type_of ctx tvar) ^ " " ^ name ^ " = " ^ prefix ^ name ^ ".Default(" ^
            (default_value_string const) ^ ");\n")
      | _ -> ()
   ) args;
;;

let is_constant_zero expr =
  match expr.cppexpr with
  | CppFloat x when (float_of_string x) = 0.0 -> true
  | CppInt i when i = Int32.of_int 0 -> true
  | _ -> false
;;

let cpp_is_const_scalar_array arrayType expressions =
   List.length expressions>0 && (match arrayType with
   | TCppScalarArray _ ->
        List.for_all (fun expr -> match expr.cppexpr with
            | CppInt _ | CppFloat _ | CppString _ | CppBool _ -> true
            | _ -> false
         ) expressions
   | _ -> false)
;;



(* Generate prototype text, including allowing default values to be null *)
let cpp_arg_string ctx tvar default_val prefix =
   let t,n = cpp_arg_type_name ctx tvar default_val prefix in
   t ^ " " ^ n
;;

let cpp_arg_list ctx args prefix =
    String.concat "," (List.map (fun (v,o) -> (cpp_arg_string ctx v o prefix) ) args)
;;


let ctx_default_values ctx args prefix =
    cpp_gen_default_values ctx args prefix
;;


let gen_type ctx haxe_type =
   ctx.ctx_output (ctx_type_string ctx haxe_type)
;;


let gen_cpp_ast_expression_tree ctx class_name func_name function_args function_type injection tree =
   let writer = ctx.ctx_writer in
   let out = ctx.ctx_output in
   let lastLine = ref (-1) in
   let tempId = ref 0 in

   let spacer = if (ctx.ctx_debug_level>0) then "            \t" else "" in
   let output_i value = out spacer; writer#write_i value in

   let output_p expr value =
       if (ctx.ctx_debug_level>0) then begin
          let line = Lexer.get_error_line expr.cpppos in
          let lineName = Printf.sprintf "%4d" line in
          let macro = if (line != !lastLine) then "HXLINE" else "HXDLIN" in
          out (macro ^ "(" ^ lineName ^ ")\t" );
          lastLine := line;
       end;
       writer#write_i value
   in

   let forInjection = match injection with Some inject -> inject.inj_setvar<>"" | _ -> false in

   let cppTree =  retype_expression ctx TCppVoid function_args function_type tree forInjection in
   let label_name i = Printf.sprintf "_hx_goto_%i" i in
   let class_hash = gen_hash_small 0 class_name in
   (*let genGc = Common.defined ctx.ctx_common Define.HxcppGcGenerational in*)

   let rec gen_with_injection injection expr =
      (match expr.cppexpr with
      | CppBlock(exprs,closures,gc_stack) ->
         writer#begin_block;
         List.iter gen_closure closures;
         (match injection with Some inject -> inject.inj_prologue gc_stack | _ -> () );
         let remaining = ref (List.length exprs) in
         lastLine := Lexer.get_error_line tree.epos;
         List.iter (fun e ->
            output_p e "";
            if (!remaining=1) then
               (match injection with Some inject -> out inject.inj_setvar | _ -> () );
            gen e;
            decr remaining;
            writer#terminate_line;
         ) exprs;
         (match injection with Some inject -> out inject.inj_tail | _ -> () );
         out spacer;
         writer#end_block;

      | CppInt i -> out (Printf.sprintf (if i> Int32.of_int(-1000000000) && i< Int32.of_int(1000000000) then "%ld" else "(int)%ld") i)
      | CppFloat float_as_string -> out ("((Float)" ^ float_as_string ^")")
      | CppString s -> out (strq s)
      | CppBool b -> out (if b then "true" else "false")
      | CppNull -> out "null()"
      | CppNil -> out "nil"

      | CppThis ThisReal -> out "hx::ObjectPtr<OBJ_>(this)"
      | CppThis _ -> out "__this"

      | CppSuper thiscall ->
            out ("hx::ObjectPtr<super>(" ^ (if thiscall=ThisReal then "this" else "__this.mPtr") ^ ")")

      | CppBreak -> out "break"
      | CppContinue -> out "continue"
      | CppGoto label -> out ("goto " ^ (label_name label));

      | CppVarDecl(var,init) ->
         let name =  cpp_var_name_of var in
         if cpp_no_debug_synbol ctx var then
            out ( (cpp_var_type_of ctx var) ^ " " ^ name )
         else begin
            let dbgName =  cpp_var_debug_name_of var in
            let macro = if init=None then "HX_VAR" else "HX_VARI" in
            let varType = cpp_macro_var_type_of ctx var in
            if name<>dbgName then
               out ( macro ^ "_NAME( " ^ varType ^ "," ^ name ^ ",\"" ^ dbgName ^ "\")" )
            else
               out ( macro ^ "( " ^ varType  ^ "," ^ name ^ ")");
         end;
         (match init with Some init -> out " = "; gen init | _ -> () );

      | CppEnumIndex(obj) ->
         gen obj;
         if cpp_is_dynamic_type obj.cpptype then
            out ".StaticCast< ::hx::EnumBase >()";
         out "->_hx_getIndex()"

      | CppNullAccess -> out ("hx::Throw(" ^ strq "Null access" ^ ")")
      | CppFunction(func,_) ->
         (match func with
         | FuncThis(field,_) ->
              out ("this->" ^ (cpp_member_name_of field) ^ "_dyn()");
         | FuncInstance(expr,inst,field) ->
              gen expr; out ((if expr.cpptype=TCppString || inst=InstStruct then "." else "->") ^ (cpp_member_name_of field) ^ "_dyn()");
         | FuncInterface(expr,_,field) ->
              gen expr;
              out ("->__Field(" ^ strq field.cf_name ^ ", hx::paccDynamic)")
         | FuncStatic(clazz,_,field) ->
              let rename = get_meta_string field.cf_meta Meta.Native in
              if rename<>"" then
                 out rename
              else
                 (out (cpp_class_name clazz); out ("::" ^ (cpp_member_name_of field) ^ "_dyn()"))
         | FuncExpression(expr) ->
              gen expr;
         | FuncExtern(name, isGlobal) ->
              if isGlobal then out " ::";
              out name;
         | FuncInternal(expr,name,_) ->
              gen expr; out ("->__Field(" ^ (strq name) ^ ",hx::paccDynamic)")
         | FuncSuper _ | FuncSuperConstruct _ -> abort "Can't create super closure" expr.cpppos
         | FuncNew _ -> abort "Can't create new closure" expr.cpppos
         | FuncEnumConstruct _ -> abort "Enum constructor outside of CppCall" expr.cpppos
         | FuncFromStaticFunction -> abort "Can't create cpp.Function.fromStaticFunction closure" expr.cpppos
         | FuncTemplate _ -> abort "Can't create template function closure" expr.cpppos
         );
      | CppCall( FuncInterface(expr,clazz,field), args) when not (is_native_gen_class clazz)->
         out ( cpp_class_name clazz ^ "::" ^ cpp_member_name_of field ^ "(");
         gen expr;
         List.iter (fun arg -> out ","; gen arg ) args;
         out ")";

      | CppCall(FuncStatic(_,true,field) as func, arg_list)
      | CppCall(FuncInstance(_,InstObjC,field) as func, arg_list) ->
         out "[ ";
         (match func with
         | FuncStatic(cl,_,_) -> out (join_class_path_remap cl.cl_path "::")
         | FuncInstance(expr,_,_) -> gen expr
         | _ ->() );

         let names = ExtString.String.nsplit field.cf_name ":" in
         let field_name, arg_names = match names with
           | name :: args -> name, args
           | _ -> assert false (* per nsplit specs, this should never happen *)
         in
         out (" " ^ field_name);
         (try match arg_list, arg_names with
         | [], _ -> ()
         | [single_arg], _ -> out ": "; gen single_arg
         | first_arg :: args, arg_names ->
             out ": ";
             gen first_arg;
             List.iter2 (fun arg arg_name ->
               out (" " ^ arg_name ^ ": ");
               gen arg) args arg_names
         with | Invalid_argument _ -> (* not all arguments names are known *)
           abort (
             "The function called here with name " ^ (String.concat ":" names) ^
             " does not contain the right amount of arguments' names as required" ^
             " by the objective-c calling / naming convention:" ^
             " expected " ^ (string_of_int (List.length arg_list)) ^
             " and found " ^ (string_of_int (List.length arg_names)))
           expr.cpppos);
         out " ]"

      | CppCall(FuncNew( TCppInst klass), args) ->
         out ((cpp_class_path_of klass) ^ "_obj::__alloc( HX_CTX ");
         List.iter (fun arg -> out ","; gen arg ) args;
         out (")")

      | CppCall(func, args) ->
         let closeCall = ref "" in
         let argsRef = ref args in
         (match func with
         | FuncThis(field,_) ->
              out ("this->" ^ (cpp_member_name_of field) );
         | FuncInstance(expr,inst,field) ->
              let operator = if (expr.cpptype = TCppString || inst=InstStruct) then "." else "->" in
              gen expr; out (operator ^ (cpp_member_name_of field) );
         | FuncInterface(expr,_,field) ->
              gen expr; out ("->" ^ (cpp_member_name_of field) );
         | FuncStatic(clazz,false,field) when cpp_is_static_extension ctx field ->
            (match args with
            | fst :: remaining ->
               argsRef := remaining;
               gen fst; out ("->" ^ (cpp_member_name_of field) );
            | _ -> abort "Native static extensions must have at least 1 argument" expr.cpppos
            );

         | FuncStatic(clazz,_,field) ->
              let rename = get_meta_string field.cf_meta Meta.Native in
              if rename<>"" then begin
                 (* This is the case if you use @:native('new foo').  c++ wil group the space undesirably *)
                 if String.contains rename ' ' then begin
                    out "(";
                    closeCall := ")"
                 end;
                 out rename
              end else
                 (out (cpp_class_name clazz); out ("::" ^ (cpp_member_name_of field) ))

         | FuncTemplate(clazz,field,tpath,native) ->
              let rename = get_meta_string field.cf_meta Meta.Native in
              if rename<>"" then begin
                 (* This is the case if you use @:native('new foo').  c++ wil group the space undesirably *)
                 if String.contains rename ' ' then begin
                    out "(";
                    closeCall := ")"
                 end;
                 out rename
              end else
                 (out (cpp_class_name clazz); out ("::" ^ (cpp_member_name_of field) ));
              out ("< " ^ (cpp_template_param tpath native) ^ "  >")

         | FuncFromStaticFunction ->
              abort "Unexpected FuncFromStaticFunction" expr.cpppos
         | FuncEnumConstruct(enum,field) ->
            out ((string_of_path enum.e_path) ^ "::" ^ (cpp_enum_name_of field));

         | FuncSuperConstruct _ -> out ((if not ctx.ctx_real_this_ptr then "__this->" else "") ^  "super::__construct")

         | FuncSuper(this,_,field) ->
              out ( (if this==ThisReal then "this->" else "__->") ^ "super::" ^ (cpp_member_name_of field) )

         | FuncNew(newType) ->
            let objName = match newType with
            | TCppString -> "::String"
            | TCppDynamicArray -> "::cpp::VirtualArray_obj::__new"
            | TCppObjectArray _ -> "::Array_obj< ::Dynamic>::__new"
            | TCppScalarArray(value) -> "::Array_obj< " ^ (tcpp_to_string value) ^ " >::__new"
            | TCppObjC klass ->  (cpp_class_path_of klass) ^ "_obj::__new"
            | TCppNativePointer klass -> "new " ^ (cpp_class_path_of klass);
            | TCppInst klass -> (cpp_class_path_of klass) ^ "_obj::__new"
            | TCppClass -> "hx::Class_obj::__new";
            | TCppFunction _ -> tcpp_to_string newType
            | _ -> abort ("Unknown 'new' target " ^ (tcpp_to_string newType)) expr.cpppos
            in
            out objName

         | FuncInternal(func,name,join) ->
            gen func; out (join ^ name);

         | FuncExtern(name, isGlobal) ->
              if isGlobal then out " ::";
              out name;
         | FuncExpression(expr)  ->
              gen expr;
         );
         let sep = ref "" in
         out "(";
         List.iter (fun arg ->
            out !sep; sep := ",";
            gen arg;
            ) !argsRef;
         out (")" ^ !closeCall);
      | CppAddressOf(e) ->
         out ("&("); gen e; out ")";
      | CppDereference(e) ->
         out ("(*("); gen e; out "))";
      | CppFunctionAddress(klass, member) ->
         let signature = ctx_function_signature ctx false member.cf_type "" in
         let name = cpp_member_name_of member in
         (*let void_cast = has_meta_key field.cf_meta Meta.Void in*)
         out ("::cpp::Function< " ^ signature ^">(hx::AnyCast(");
         out ("&::" ^(join_class_path_remap klass.cl_path "::")^ "_obj::" ^ name );
         out " ))"

      | CppExtern(name,isGlobal) ->
         if isGlobal then out " ::";
         out name;

      | CppDynamicField(obj,name) ->
         gen obj;
         out ("->__Field(" ^ (strq name)  ^ ",hx::paccDynamic)");

      | CppArray(arrayLoc) -> (match arrayLoc with
         | ArrayTyped(arrayObj,index,_) ->
            gen arrayObj; out "->__get("; gen index; out ")"

         | ArrayPointer(arrayObj,index) ->
            gen arrayObj; out ".ptr["; gen index; out "]"

         | ArrayRawPointer(arrayObj,index) ->
            gen arrayObj; out "["; gen index; out "]"

         | ArrayObject(arrayObj,index,elem) ->
            let close = if cpp_is_dynamic_type elem then
                  ""
               else if elem=TCppDynamicArray then begin
                  out (tcpp_to_string elem ^ "( "); ")"
               end else
                  ".StaticCast< " ^ tcpp_to_string elem ^ " >()"
            in
            gen arrayObj; out "->__get("; gen index; out (")" ^ close);

         | ArrayVirtual(arrayObj,index) ->
            gen arrayObj; out "->__get("; gen index; out ")";

         | ArrayDynamic(arrayObj,index) ->
            gen arrayObj; out "->__GetItem("; gen index; out ")"

         | ArrayImplements(_,arrayObj,index) ->
            gen arrayObj; out "->__get("; gen index; out ")";
         )


      | CppSet(lvalue,rvalue) ->
         let close = if expr.cpptype=TCppVoid then "" else (out "("; ")" ) in
         (match lvalue with
         | CppVarRef( VarClosure(var)) when is_gc_element ctx (cpp_type_of ctx var.v_type) ->
              out ("this->_hx_set_" ^ (cpp_var_name_of var) ^ "(HX_CTX, "); gen rvalue; out ")"

         | CppVarRef( VarThis(member,_)) when is_gc_element ctx (cpp_type_of ctx member.cf_type) ->
              out ("this->_hx_set_" ^ (cpp_member_name_of member) ^ "(HX_CTX, "); gen rvalue; out ")"

         | CppVarRef( VarInstance(obj,member,_,"->")) when is_gc_element ctx (cpp_type_of ctx member.cf_type) ->
              gen obj; out ("->_hx_set_" ^ (cpp_member_name_of member) ^ "(HX_CTX, "); gen rvalue; out ")"
         | CppVarRef( VarInternal(obj,operator,member) ) ->
              gen obj; out (operator ^ member)

         | CppVarRef varLoc ->
              gen_val_loc varLoc true; out " = "; gen rvalue;


         | CppArrayRef arrayLoc -> (match arrayLoc with
            | ArrayObject(arrayObj, index, _) when (is_gc_element ctx TCppDynamic) ->
               gen arrayObj; out "->setCtx( HX_CTX, "; gen index; out ","; gen rvalue; out ")"
            | ArrayTyped(arrayObj, index, t) when (is_gc_element ctx t) ->
               gen arrayObj; out "->setCtx( HX_CTX, "; gen index; out ","; gen rvalue; out ")"
            | ArrayObject(arrayObj, index, _)
            | ArrayTyped(arrayObj, index, _)
            | ArrayRawPointer(arrayObj, index) ->
               gen arrayObj; out "["; gen index; out "] = "; gen rvalue
            | ArrayPointer(arrayObj, index) ->
               gen arrayObj; out ".ptr["; gen index; out "] = "; gen rvalue
            | ArrayVirtual(arrayObj, index)  ->
               gen arrayObj; out "->set("; gen index; out ","; gen rvalue; out ")"

            | ArrayDynamic(arrayObj, index) ->
               gen arrayObj; out "->__SetItem("; gen index; out ","; gen rvalue; out ")"

            | ArrayImplements(_,arrayObj,index) ->
               gen arrayObj; out "->__set("; gen index; out ","; gen rvalue; out ")"
            )
         | CppDynamicRef(expr,name) ->
            gen expr; out ("->__SetField(" ^ (strq name) ^ ","); gen rvalue; out ",hx::paccDynamic)"
         | CppExternRef(name, isGlobal) -> if isGlobal then out " ::"; out (name ^ " = ");
         );
         out close;

      | CppCrement(incFlag,preFlag, lvalue) ->
         let op = if incFlag==CppIncrement then "++" else "--" in
         if (preFlag==Prefix) then out op;
         gen_lvalue lvalue;
         if (preFlag==Postfix) then out op

      | CppModify(op,lvalue,rvalue) ->
         out (string_of_op_eq op expr.cpppos);
         out "("; gen_lvalue lvalue; out ","; gen rvalue; out ")"

      | CppPosition(name,line,clazz,func) ->
         out ("hx::SourceInfo(" ^ strq name ^ "," ^ string_of_int(Int32.to_int line) ^ "," ^ strq clazz ^ "," ^ strq func ^ ")")

      | CppClassOf (path,native) ->
         let path = "::" ^ (join_class_path_remap (path) "::" ) in
         let path = match path with
         | "::Int" -> "int"
         | "::Bool" -> "bool"
         | x -> x in
         if (native) then
            out "null()"
         else if (path="::Array") then
            out "hx::ArrayBase::__mClass"
         else
            out ("hx::ClassOf< " ^ path ^ " >()")

      | CppVar(loc) ->
         gen_val_loc loc false;


      | CppClosure closure ->
          out (" ::Dynamic(new _hx_Closure_" ^ (string_of_int(closure.close_id)) ^ "(");
          let separator = ref "" in
          (match closure.close_this with
          | Some this ->
             out (if this=ThisReal then "this" else "__this");
             separator := ",";
          | _ -> () );

          Hashtbl.iter (fun name value ->
             out !separator; separator := ",";
             out (keyword_remap name)
         )  closure.close_undeclared;
         out "))";

      | CppObjectDecl (values,isStruct) ->
         let length = List.length values in
         let lengthStr = string_of_int length in
         if (expr.cpptype!=TCppVoid) then out " ::Dynamic(";
         if (isStruct) && length>0 && length<=5 then begin
            out ("hx::AnonStruct" ^ lengthStr ^"_obj< " ^
               (String.concat "," (List.map (fun (_,value) ->  tcpp_to_string value.cpptype) values) ) ^
               " >::Create(" );
            let sep = ref "" in
            List.iter (fun (name,value) -> out (!sep ^ (strq name) ^ "," ); sep:=","; gen value ) values;
            out ")";
         end else begin
            out ("hx::Anon_obj::Create(" ^ lengthStr ^")");
            let sorted = List.sort (fun  (_,_,h0) (_,_,h1) -> Int32.compare h0 h1 )
                (List.map (fun (name,value) -> name,value,(gen_hash32 0 name ) ) values) in
            writer#push_indent;
            ExtList.List.iteri (fun idx (name,value,_) ->
               out ("\n" ^ spacer); writer#write_i ("->setFixed(" ^ (string_of_int idx) ^ "," ^ (strq name) ^ ","); gen value; out ")";
            ) sorted;
         end;
         if (expr.cpptype!=TCppVoid) then out ")";
         writer#pop_indent;

      | CppArrayDecl(exprList) when cpp_is_const_scalar_array expr.cpptype exprList ->
         let arrayType = match expr.cpptype with TCppScalarArray(value) -> value | _ -> assert  false in
         let typeName = tcpp_to_string arrayType in
         incr ctx.ctx_file_id;

         let id = "_hx_array_data_" ^ class_hash ^ "_" ^ string_of_int( !(ctx.ctx_file_id) ) in

         let out_top = ctx.ctx_writer#write_h in
         out_top ("static const " ^ typeName ^ " " ^ id ^ "[] = {\n\t");
         List.iter (fun expr -> match expr.cppexpr with
            | CppInt i -> out_top (Printf.sprintf "(%s)%ld," typeName i)
            | CppFloat f -> out_top ( f ^ "," )
            | CppString s -> out_top ( (strq s) ^ "," )
            | CppBool b -> out_top (if b then "1," else "0,")
            | _ -> assert false
         ) exprList;
         out_top ("\n};\n");
         out ("::Array_obj< " ^ typeName ^ " >::fromData( " ^ id ^ "," ^ list_num exprList ^ ")");

      | CppArrayDecl(exprList) ->
         let count = List.length exprList in
         let countStr = string_of_int count in
         let arrayType,close = match expr.cpptype with
            | TCppObjectArray _ -> "::Array_obj< ::Dynamic>",""
            | TCppScalarArray(value) -> "::Array_obj< " ^ (tcpp_to_string value) ^ " >",""
            | TCppDynamicArray -> "::cpp::VirtualArray_obj",""
            | _ -> " ::Dynamic( ::cpp::VirtualArray_obj",")"
         in
         out (arrayType ^ "::__new(" ^ countStr ^ ")" );
         ExtList.List.iteri ( fun idx elem -> out ("->init(" ^ (string_of_int idx) ^ ",");
                     gen elem; out ")" ) exprList;
         out close;


      | CppBinop( Ast.OpUShr, left, right) ->
         out "hx::UShr("; gen left; out ","; gen right; out ")";

      | CppBinop( Ast.OpMod, left, right) ->
         if is_constant_zero right then begin
            out "hx::Mod("; gen left; out ",(double)( "; gen right; out " ))";
         end else begin
            out "hx::Mod("; gen left; out ","; gen right; out ")";
         end

      | CppBinop( Ast.OpDiv, left, right) when is_constant_zero right ->
         out "hx::DivByZero("; gen left; out ")";

      | CppBinop(op, left, right) ->
         let op = string_of_op op expr.cpppos in
          out "(";
          gen left;
          out (" " ^ op ^ " ");
          gen right;
          out ")";
      | CppCompare(opName, left, right, _) ->
          out ("hx::" ^ opName ^ "( ");
          gen left;
          out (",");
          gen right;
          out (" )");
      | CppNullCompare(op, left) ->
          out ("hx::" ^ op ^ "( "); gen left; out (" )");

      | CppThrow(value) ->
         out "HX_STACK_DO_THROW("; gen value; out ")";

      | CppReturn None -> out "return";
      | CppReturn Some value -> out "return "; gen value;

      | CppEnumField(enum,field) ->
         out ((string_of_path enum.e_path) ^ "::" ^ (cpp_enum_name_of field) ^ "_dyn()" );

      | CppEnumParameter(obj,field,index) ->
         let valueType = cpp_type_of ctx (get_nth_type field index) in
         let baseType = cpp_base_type_of valueType in
         gen obj;
         if cpp_is_dynamic_type obj.cpptype then
            out ".StaticCast< ::hx::EnumBase >()";
         out ( "->_hx_get" ^ baseType ^ "(" ^ (string_of_int index) ^ ")");
         (match valueType with
         | TCppObjectArray _
         | TCppScalarArray _
         | TCppDynamicArray
         | TCppClass
         | TCppEnum _
         | TCppInst _ -> out (".StaticCast< " ^ (tcpp_to_string valueType ) ^ " >()")
         | _ ->()
         )

      | CppIntSwitch(condition, cases, defVal) ->
         out "switch((int)("; gen condition; out "))";
         writer#begin_block;
         List.iter (fun (values,expr) ->
            out spacer; writer#write_i "";
            List.iter (fun value -> out ("case (int)" ^ (Printf.sprintf "%ld" value) ^ ": " ) ) values;
            gen expr;
            out spacer; writer#write_i "break;\n";
         ) cases;
         (match defVal with
         | Some expr -> output_i "default:"; gen expr; | _ -> ()  );
         out spacer;
         writer#end_block;
      | CppSwitch(condition, conditionType, cases, optional_default, label) ->
         let tmp_name = "_hx_switch_" ^ (string_of_int !tempId) in
         incr tempId;
         out ( (tcpp_to_string conditionType) ^ " " ^ tmp_name ^ " = " );
         gen condition;
         out ";\n";
         List.iter (fun (cases,expression) ->
            output_i "if ( ";
            let or_str = ref "" in
            List.iter (fun value ->
               out (!or_str ^ " (" ^ tmp_name ^ "=="); gen value; out ")";
               or_str := " || ";
               ) cases;
            out (" )");
            gen expression;
            ) cases;
         (match optional_default with | None -> ()
         | Some default ->
            output_i "/* default */";
            gen default;
         );
         output_i ((label_name label) ^ ":")

      | CppUnop(unop,value) ->
           out (match unop with
           | CppNot -> "!"
           | CppNeg -> "-"
           | CppNegBits -> "~"
           );
           out "(";  gen value; out ")"

      | CppWhile(condition, block, while_flag, loop_id) ->
          (match while_flag with
          | NormalWhile ->
              out "while("; gen condition; out (")");
              lastLine := -1;
              gen block;
          | DoWhile ->
              out ("do ");
              lastLine := -1;
              gen block;
              out "while("; gen condition; out ")"
          );
          if loop_id > -1 then output_i ((label_name loop_id) ^ ":");

      | CppIf (condition,block,None) ->
          out "if ("; gen condition; out (") ");
          gen block;

      | CppIf (condition,block,Some elze) when expr.cpptype = TCppVoid ->
          out "if ("; gen condition; out (") ");
          gen block;
          output_i ("else ");
          gen elze;

      | CppIf (condition,block,Some elze) ->
          gen condition; out " ? "; gen block; out " : "; gen elze;

      | CppFor (tvar, init, loop) ->
         let varType = cpp_var_type_of ctx tvar in
         out ("for(::cpp::FastIterator_obj< " ^  varType ^
               " > *__it = ::cpp::CreateFastIterator< "^ varType ^ " >(");
         gen init;
         out (");  __it->hasNext(); )");
         let prologue = fun _ ->
            output_i ( varType ^ " " ^ (cpp_var_name_of tvar) ^ " = __it->next();\n" );
         in
         gen_with_injection (mk_injection prologue "" "") loop;


      | CppTry(block,catches) ->
          let prologue = function _ ->
             ExtList.List.iteri (fun idx (v,_) ->
                output_i ("HX_STACK_CATCHABLE(" ^ cpp_macro_var_type_of ctx v  ^ ", " ^ string_of_int idx ^ ");\n")
             ) catches
          in
          out ("try ");
          gen_with_injection (mk_injection prologue "" "" ) block;
          if (List.length catches > 0 ) then begin
             output_i "catch( ::Dynamic _hx_e)";
             writer#begin_block;

             let seen_dynamic = ref false in
             let else_str = ref "" in
             List.iter (fun (v,catch) ->
                let type_name = cpp_var_type_of ctx v in
                if (type_name="Dynamic") then begin
                   seen_dynamic := true;
                   output_i !else_str;
                end else
                   output_i (!else_str ^ "if (_hx_e.IsClass< " ^ type_name ^ " >() )");
                let prologue = function _ ->
                   output_i "HX_STACK_BEGIN_CATCH\n";
                   output_i (type_name ^ " " ^ (cpp_var_name_of v) ^ " = _hx_e;\n");
                in
                gen_with_injection (mk_injection prologue "" "") catch;
                else_str := "else ";
                ) catches;

             if (not !seen_dynamic) then begin
                output_i "else {\n";
                output_i "\tHX_STACK_DO_THROW(_hx_e);\n";
                output_i "}\n";
             end;
             out spacer;
             writer#end_block;
          end

      | CppCode(value, exprs) ->
         Codegen.interpolate_code ctx.ctx_common (format_code value) exprs out (fun e -> gen e) expr.cpppos
      | CppTCast(expr,cppType) ->
         let toType = tcpp_to_string cppType in
         if toType="Dynamic" then
            (out " ::Dynamic("; gen expr; out ")")
         else
            (out ("hx::TCast< " ^ toType ^ " >::cast("); gen expr; out ")")

      | CppCastStatic(expr,toType) ->
         let close = match expr.cpptype with
         | TCppDynamic -> ""
         | _ -> out "Dynamic( "; ")"
         in
         gen expr; out (close ^ ".StaticCast< " ^ tcpp_to_string toType ^" >()")

      | CppCast(expr,toType) ->
         (match expr.cppexpr, expr.cpptype with
         | CppCall( FuncInternal _, _), _ ->
            gen expr; out (".StaticCast< " ^ tcpp_to_string toType ^" >()")
         | _, TCppObjC(_)
         | _, TCppObjCBlock(_)  ->
            out ("( ("^ tcpp_to_string toType ^")((id) ( "); gen expr; out (") ))")
         | _,_ ->
            (match toType with
               | TCppObjectPtr -> out ("hx::DynamicPtr("); gen expr; out (")")
               | t -> out ("( ("^ tcpp_to_string t ^")("); gen expr; out (") )")
            )
         )

      | CppCastScalar(expr,scalar) ->
         out ("( ("^scalar^")("); gen expr; out (") )");

      | CppCastVariant(expr) ->
         out " ::Dynamic("; gen expr; out ")";

      | CppCastObjC(expr,klass) ->
         let path = join_class_path_remap klass.cl_path "::"  in
         let toType = if klass.cl_interface then "id < " ^ path ^ ">" else path ^ " *" in
         out ("( (" ^ toType ^ ") (id) ("); gen expr; out ") )"

      | CppCastObjCBlock(expr,args,ret) ->
         out (tcpp_objc_block_struct args ret ^ "::create( ");
         gen expr;
         out ")"

      | CppCastProtocol(expr,klass) ->
         out ( (join_class_path_remap klass.cl_path "::" ) ^ "_obj::_hx_toProtocol( ");
         gen expr;
         out ")"

      | CppCastNative(expr) ->
         out "("; gen expr; out ").mPtr"
      );
      if (ctx.ctx_debug_level >= 3) then
         out ("/* " ^ (s_tcpp expr.cppexpr) ^ ":" ^ tcpp_to_string expr.cpptype ^ " */")

   and gen expr =
      gen_with_injection None expr

   and gen_lvalue lvalue =
      match lvalue with
      | CppVarRef varLoc ->
           gen_val_loc varLoc true
      | CppArrayRef arrayLoc -> (match arrayLoc with
         | ArrayObject(arrayObj, index, _) ->
            out "hx::IndexRef("; gen arrayObj; out ".mPtr,"; gen index; out ")";
         | ArrayTyped(arrayObj, index, _) ->
            gen arrayObj; out "["; gen index; out "]";
         | ArrayPointer(arrayObj, index) ->
            gen arrayObj; out ".ptr["; gen index; out "]";
         | ArrayRawPointer(arrayObj, index) ->
            gen arrayObj; out "["; gen index; out "]";
         | ArrayVirtual(arrayObj, index)
         | ArrayDynamic(arrayObj, index) ->
            out "hx::IndexRef("; gen arrayObj; out ".mPtr,"; gen index; out ")";
         | ArrayImplements(_,arrayObj,index) ->
            out "hx::__ArrayImplRef("; gen arrayObj; out ","; gen index; out ")";
         )
      | CppExternRef(name,isGlobal) -> if isGlobal then out " ::"; out name
      | CppDynamicRef(expr,name) ->
         let objPtr = match expr.cpptype with
         |  TCppVariant -> "getObject()"
         | _ -> ".mPtr"
         in
         out "hx::FieldRef(("; gen expr ; out (")" ^ objPtr ^ "," ^ strq name ^ ")")

   and gen_val_loc loc lvalue =
      match loc with
      | VarClosure(var) ->
          out (cpp_var_name_of var)
      | VarLocal(local) -> out (cpp_var_name_of local)
      | VarStatic(clazz,objc,member) ->
          let rename = get_meta_string member.cf_meta Meta.Native in
          if rename <> "" then
             out rename
          else if objc then
             (out ( (join_class_path_remap clazz.cl_path "::") ); out ("." ^ (cpp_member_name_of member)))
          else
             (out (cpp_class_name clazz ); out ("::" ^ (cpp_member_name_of member)))
      | VarThis(member,_) ->
         out ("this->" ^ (cpp_member_name_of member))
      | VarInstance(obj,member,_,operator) ->
         gen obj; out (operator ^ (cpp_member_name_of member))
      | VarInternal(obj,operator,member) ->
         gen obj; out (operator ^ member)
      | VarInterface(obj,member) ->
         gen obj; out ("->" ^ (cpp_member_name_of member) ^ "_get()" )

   and string_of_op_eq op pos = match op with
      | OpAdd -> "hx::AddEq"
      | OpMult -> "hx::MultEq"
      | OpDiv -> "hx::DivEq"
      | OpSub -> "hx::SubEq"
      | OpAnd -> "hx::AndEq"
      | OpOr  -> "hx::OrEq"
      | OpXor  -> "hx::XorEq"
      | OpShl  -> "hx::ShlEq"
      | OpShr  -> "hx::ShrEq"
      | OpUShr  -> "hx::UShrEq"
      | OpMod  -> "hx::ModEq"
      | _ -> abort "Bad assign op" pos
   and string_of_op op pos = match op with
      | OpAdd -> "+"
      | OpMult -> "*"
      | OpDiv -> "/"
      | OpSub -> "-"
      | OpEq -> "=="
      | OpNotEq -> "!="
      | OpGt -> ">"
      | OpGte -> ">="
      | OpLt -> "<"
      | OpLte -> "<="
      | OpAnd -> "&"
      | OpOr -> "|"
      | OpXor -> "^"
      | OpBoolAnd -> "&&"
      | OpBoolOr -> "||"
      | OpShl -> "<<"
      | OpShr -> ">>"
      | OpUShr -> "<<<"
      | OpMod -> "%"
      | OpInterval -> "..."
      | OpArrow -> "->"
      | OpIn -> " in "
      | OpAssign | OpAssignOp _ -> abort "Unprocessed OpAssign" pos
   and string_of_path path =
      "::" ^ (join_class_path_remap path "::") ^ "_obj"

   and gen_closure closure =
      let argc = Hashtbl.length closure.close_undeclared in
      let size = string_of_int argc in
      if argc >= 62 then (* Limited by c++ macro size of 128 args *)
         abort "Too many capture variables" closure.close_expr.cpppos;
      if argc >= 20 || (List.length closure.close_args) >= 20 then
         writer#add_big_closures;
      let argsCount = list_num closure.close_args in
      output_i ("HX_BEGIN_LOCAL_FUNC_S" ^ size ^ "(");
      out (if closure.close_this != None then "hx::LocalThisFunc," else "hx::LocalFunc,");
      out ("_hx_Closure_" ^ (string_of_int closure.close_id) );
      Hashtbl.iter (fun name var ->
         out ("," ^ (cpp_macro_var_type_of ctx var) ^ "," ^ (keyword_remap name));
      ) closure.close_undeclared;
      out (") HXARGC(" ^ argsCount ^")\n");

      let func_type = tcpp_to_string closure.close_type in
      output_i (func_type ^ " _hx_run(" ^ (cpp_arg_list ctx closure.close_args "__o_") ^ ")");

      let prologue = function gc_stack ->
          cpp_gen_default_values ctx closure.close_args "__o_";
          hx_stack_push ctx output_i class_name func_name closure.close_expr.cpppos gc_stack;
          if (ctx.ctx_debug_level>=2) then begin
             if (closure.close_this != None) then
                output_i ("HX_STACK_THIS(__this.mPtr)\n");
             List.iter (fun (v,_) -> output_i ("HX_STACK_ARG(" ^ (cpp_var_name_of v) ^ ",\"" ^ (cpp_debug_name_of v) ^"\")\n") )
                (List.filter (cpp_debug_var_visible ctx) closure.close_args);

             let line = Lexer.get_error_line closure.close_expr.cpppos in
             let lineName = Printf.sprintf "%4d" line in
             out ("HXLINE(" ^ lineName ^ ")\n" );
          end
      in

      gen_with_injection (mk_injection prologue "" "") closure.close_expr;

      let return = match closure.close_type with TCppVoid -> "(void)" | _ -> "return" in

      output_i ("HX_END_LOCAL_FUNC" ^ argsCount ^ "(" ^ return ^ ")\n\n");
   in


   (*out "\t";*)

   gen_with_injection injection cppTree;

;;

(* } *)


let gen_cpp_function_body ctx clazz is_static func_name function_def head_code tail_code no_debug =
   let output = ctx.ctx_output in
   let dot_name = join_class_path clazz.cl_path "." in
   if no_debug then ctx.ctx_debug_level <- 0;
   let prologue = function gc_stack ->
      let spacer = if no_debug then "\t" else "            \t" in
      let output_i = fun s -> output (spacer ^ s) in
      ctx_default_values ctx function_def.tf_args "__o_";
      hx_stack_push ctx output_i dot_name func_name function_def.tf_expr.epos gc_stack;
      if ctx.ctx_debug_level >= 2 then begin
         if (not is_static)
            then output_i ("HX_STACK_THIS(" ^ (if ctx.ctx_real_this_ptr then "this" else "__this") ^")\n");
         List.iter (fun (v,_) -> if not (cpp_no_debug_synbol ctx v) then
              output_i ("HX_STACK_ARG(" ^ (cpp_var_name_of v) ^ ",\"" ^ v.v_name ^"\")\n") ) function_def.tf_args;

         let line = Lexer.get_error_line function_def.tf_expr.epos in
         let lineName = Printf.sprintf "%4d" line in
         output ("HXLINE(" ^ lineName ^ ")\n" );
      end;
      if (head_code<>"") then
         output_i (head_code ^ "\n");
   in
   let args = List.map fst function_def.tf_args in

   let injection = mk_injection prologue "" tail_code in
   gen_cpp_ast_expression_tree ctx dot_name func_name args function_def.tf_type injection (mk_block function_def.tf_expr);
;;

let gen_cpp_init ctx dot_name func_name var_name expr =
   let output = ctx.ctx_output in
   let prologue = function gc_stack ->
      let spacer = if ctx.ctx_debug_level > 0 then "            \t" else "\t" in
      let output_i = fun s -> output (spacer ^ s) in
         hx_stack_push ctx output_i dot_name func_name expr.epos gc_stack;
   in
   let injection = mk_injection prologue var_name "" in
   gen_cpp_ast_expression_tree ctx dot_name func_name [] t_dynamic injection (mk_block expr);
;;


(*
let is_dynamic_haxe_method f =
   match follow f.cf_type with
   | TFun _ when f.cf_expr = None -> true
   | _ ->
      (match f.cf_expr with
      | Some { eexpr = TFunction fd } when f.cf_set = MethodAccess true -> true
      | Some { eexpr = TFunction fd } when f.cf_set = NormalAccess -> true
      | _ -> false);;
*)

let is_dynamic_haxe_method f =
      (match f.cf_expr, f.cf_kind with
      | Some { eexpr = TFunction _ }, (Var _ | Method MethDynamic) -> true
      | _ -> false);;


let is_data_member field =
   match field.cf_kind with
   | Var _ | Method MethDynamic -> true
   | _ -> false;;


let is_override class_def field =
   List.exists (fun f -> f.cf_name = field) class_def.cl_overrides
;;

let current_virtual_functions clazz =
  List.rev (List.fold_left (fun result elem -> match follow elem.cf_type, elem.cf_kind  with
    | _, Method MethDynamic -> result
    | TFun (args,return_type), Method _  when not (is_override clazz elem.cf_name ) -> (elem,args,return_type) :: result
    | _,_ -> result ) [] clazz.cl_ordered_fields)
;;

let all_virtual_functions clazz =
  let rec all_virtual_functions clazz =
   (match clazz.cl_super with
   | Some def -> all_virtual_functions (fst def)
   | _ -> [] ) @ current_virtual_functions clazz
   in
   all_virtual_functions clazz
;;


let rec unreflective_type t =
    match follow t with
       | TInst (klass,_) ->  Meta.has Meta.Unreflective klass.cl_meta
       | TFun (args,ret) ->
           List.fold_left (fun result (_,_,t) -> result || (unreflective_type t)) (unreflective_type ret) args;
       | _ -> false
;;

let reflective class_def field = not (
    (Meta.has Meta.NativeGen class_def.cl_meta) ||
    (Meta.has Meta.Unreflective class_def.cl_meta) ||
    (Meta.has Meta.Unreflective field.cf_meta) ||
    unreflective_type field.cf_type
   )
;;





let field_arg_count field =
   match follow field.cf_type, field.cf_kind  with
      | _, Method MethDynamic -> -1
      | TFun (args,return_type), Method _  -> List.length args
      | _,_ -> -1
;;

let native_field_name_remap is_static field =
   let remap_name = keyword_remap field.cf_name in
   if not is_static then
      remap_name
   else begin
      let nativeImpl = get_meta_string field.cf_meta Meta.Native in
      if nativeImpl<>"" then begin
         let r = Str.regexp "^[a-zA-Z_0-9]+$" in
            if Str.string_match r remap_name 0 then
               "_hx_" ^ remap_name
            else
               "_hx_f" ^ (gen_hash 0 remap_name)
      end else
         remap_name
   end
;;


let gen_field ctx class_def class_name ptr_name dot_name is_static is_interface field =
   let output = ctx.ctx_output in
   ctx.ctx_real_this_ptr <- not is_static;
   let remap_name = keyword_remap field.cf_name in
   let decl = get_meta_string field.cf_meta Meta.Decl in
   let has_decl = decl <> "" in
   if (is_interface) then begin
      (* Just the dynamic glue  - not even that ... *)
      ()
   end else (match  field.cf_expr with
   (* Function field *)
   | Some { eexpr = TFunction function_def } ->
      let return_type_str = (ctx_type_string ctx function_def.tf_type) in
      let nargs = string_of_int (List.length function_def.tf_args) in
      let return_type = (cpp_type_of ctx function_def.tf_type ) in
      let is_void = return_type = TCppVoid in
      let ret = if is_void  then "(void)" else "return " in

      let needsWrapper t = match t with
         | TCppStar _ -> true
         | TCppInst(t) -> has_meta_key t.cl_meta Meta.StructAccess
         | _ -> false
      in
      let orig_debug = ctx.ctx_debug_level in
      let no_debug = has_meta_key field.cf_meta Meta.NoDebug in

      if (not (is_dynamic_haxe_method field)) then begin
         (* The actual function definition *)
         let nativeImpl = get_meta_string field.cf_meta Meta.Native in
         let remap_name = native_field_name_remap is_static field in
         output (if is_void then "void" else return_type_str );
         output (" " ^ class_name ^ "::" ^ remap_name ^ "(" );
         output (ctx_arg_list ctx function_def.tf_args "__o_");
         output ")";
         ctx.ctx_real_this_ptr <- true;
         let code = (get_code field.cf_meta Meta.FunctionCode) in
         let tail_code = (get_code field.cf_meta Meta.FunctionTailCode) in

         if nativeImpl<>"" && is_static then begin
            output " {\n";
            output ("\t" ^ ret ^ "::" ^ nativeImpl ^ "(" ^ (ctx_arg_list_name ctx function_def.tf_args "__o_") ^ ");\n");
            output "}\n\n";
         end else
            gen_cpp_function_body ctx class_def is_static field.cf_name function_def code tail_code no_debug;

         output "\n\n";
         let nonVirtual = has_meta_key field.cf_meta Meta.NonVirtual in
         let doDynamic =  (nonVirtual || not (is_override class_def field.cf_name ) ) && (reflective class_def field ) in
         (* generate dynamic version too ... *)
         if ( doDynamic ) then begin
            let tcpp_args = List.map (fun (v,_) -> cpp_type_of ctx v.v_type  ) function_def.tf_args in
            let wrap = (needsWrapper return_type) || (List.exists needsWrapper tcpp_args) in
            if wrap then begin
               let wrapName = "_hx_wrap" ^ class_name ^ "_" ^ remap_name in
               output ("static ::Dynamic " ^ wrapName ^ "( "  );
               let sep = ref " " in
               if not is_static then begin
                  output "hx::Object *obj";
                  sep := ",";
               end;
               ExtList.List.iteri (fun i _ -> output (!sep ^ "const Dynamic &a" ^ (string_of_int i)) ; sep:=",")  tcpp_args;
               output ( ") {\n\t");
               if not is_void then begin
                  match return_type with
                    | TCppStar _ ->
                       output "return (cpp::Pointer<const void *>) "
                    | TCppInst(t) when has_meta_key t.cl_meta Meta.StructAccess ->
                       output ("return (cpp::Struct< " ^ (tcpp_to_string return_type) ^ " >) ");
                    | _ -> output "return ";
               end;

               if is_static then
                  output (class_name ^ "::" ^ remap_name ^ "(")
               else
                  output ("reinterpret_cast< " ^ class_name ^ " *>(obj)->" ^ remap_name ^ "(");

               sep := "";
               ExtList.List.iteri (fun i arg ->
                     output !sep; sep := ",";
                     (match arg with
                       | TCppStar (t,const) ->
                          output ("(cpp::" ^ (if const then "Const" else "") ^"Pointer<" ^ (tcpp_to_string t)^" >) ")
                       | TCppInst(t) when has_meta_key t.cl_meta Meta.StructAccess ->
                          output ("(cpp::Struct< " ^ (tcpp_to_string return_type) ^ " >) ");
                       | _ -> () );
                     output ("a" ^ (string_of_int i));
                  )  tcpp_args;

               output ");\n";

               if is_void then output "\treturn null();\n";
               output "}\n";
               let nName = string_of_int (List.length tcpp_args) in
               output ("::Dynamic " ^ class_name ^ "::" ^ remap_name ^ "_dyn() {\n\treturn ");
               if is_static then
                  output ("hx::CreateStaticFunction" ^ nName ^ "(\"" ^ remap_name ^ "\"," ^ wrapName ^ ");")
               else
                  output ("hx::CreateMemberFunction" ^ nName ^ "(\"" ^ remap_name ^ "\",this," ^ wrapName ^ ");");
               output "}\n";
            end else begin
               if (is_static) then output "STATIC_";
               output ("HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ "," ^ remap_name ^ "," ^ ret ^ ")\n\n");
            end
         end;

      end else begin
         ctx.ctx_real_this_ptr <- false;
         let func_name = "__default_" ^ (remap_name) in
         output ("HX_BEGIN_DEFAULT_FUNC(" ^ func_name ^ "," ^ class_name ^ ")\n");
         output return_type_str;
         output (" _hx_run(" ^ (ctx_arg_list ctx function_def.tf_args "__o_") ^ ")");
         gen_cpp_function_body ctx class_def is_static func_name function_def "" "" no_debug;

         output ("HX_END_LOCAL_FUNC" ^ nargs ^ "(" ^ ret ^ ")\n");
         output ("HX_END_DEFAULT_FUNC\n\n");

         if (is_static) then
            output ( "::Dynamic " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
      end;
      ctx.ctx_debug_level <- orig_debug

   (* Data field *)
   | _ when has_decl ->
      if is_static then begin
         output ( class_name ^ "::" ^ remap_name ^ "_decl ");
         output ( " " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
      end
   | _ ->
      if is_static && is_physical_field field then begin
         gen_type ctx field.cf_type;
         output ( " " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
      end
   )
   ;;





let gen_field_init ctx class_def field =
   let dot_name = join_class_path class_def.cl_path "." in
   let output = ctx.ctx_output in
   let remap_name = keyword_remap field.cf_name in
   match  field.cf_expr with
   (* Function field *)
   | Some { eexpr = TFunction function_def } ->
      if (is_dynamic_haxe_method field) then begin
         let func_name = "__default_" ^ (remap_name) in
         output ( "\t" ^ remap_name ^ " = new " ^ func_name ^ ";\n\n" );
      end

   (* Data field *)
   | Some expr ->
      let var_name = ( match remap_name with
                  | "__meta__" -> "__mClass->__meta__"
                  | "__rtti" -> "__mClass->__rtti__"
                  | _ -> remap_name ) in

      gen_cpp_init ctx dot_name "boot" (var_name ^ " = ") expr
   | _ ->  ()
;;


let cpp_interface_impl_name ctx interface =
   "_hx_" ^ (join_class_path interface.cl_path "_" )
;;


let cpp_class_hash interface =
   gen_hash 0 (join_class_path interface.cl_path "::" )
;;



let has_field_init field =
   match field.cf_expr with
   (* Function field *)
   | Some { eexpr = TFunction function_def } -> is_dynamic_haxe_method field
   (* Data field *)
   | Some _ -> true
   | _ -> false
;;


let gen_member_def ctx class_def is_static is_interface field =
   let output = ctx.ctx_output in
   let remap_name = keyword_remap field.cf_name in
   let nativeGen = has_meta_key class_def.cl_meta Meta.NativeGen in

   if (is_interface) then begin
      match follow field.cf_type, field.cf_kind with
      | _, Method MethDynamic  -> ()
      | TFun (args,return_type), Method _  ->
         let gen_args = ctx_tfun_arg_list ctx true in
         if is_static || nativeGen then begin
            output ( (if (not is_static) then "		virtual " else "		" ) ^ (ctx_type_string ctx return_type) );
            output (" " ^ remap_name ^ "( " );
            output (gen_args args);
            output (if (not is_static) then ")=0;\n" else ");\n");
            if (reflective class_def field) then begin
               if (Common.defined ctx.ctx_common Define.DynamicInterfaceClosures) then
                  output ("		inline ::Dynamic " ^ remap_name ^ "_dyn() { return __Field( " ^ (str field.cf_name) ^ ", hx::paccDynamic); }\n" )
               else
                  output ("		virtual ::Dynamic " ^ remap_name ^ "_dyn()=0;\n" );
            end
         end else begin
            let argList = gen_args args in
            let returnType = ctx_type_string ctx return_type in
            let returnStr = if returnType = "void" then "" else "return " in
            let commaArgList = if argList="" then argList else "," ^ argList in
            let cast = "static_cast< ::" ^ join_class_path_remap class_def.cl_path "::" ^ "_obj *>" in
            output ("		" ^ returnType ^ " (hx::Object :: *_hx_" ^ remap_name ^ ")(" ^ argList ^ "); \n");
            output ("		static inline " ^ returnType ^ " " ^ remap_name ^ "( ::Dynamic _hx_" ^ commaArgList ^ ") {\n");
            output ("			" ^ returnStr ^ "(_hx_.mPtr->*( " ^ cast ^ "(_hx_.mPtr->_hx_getInterface(" ^ (cpp_class_hash class_def) ^ ")))->_hx_" ^ remap_name ^ ")(" ^ cpp_arg_names args ^ ");\n		}\n" );
         end
      | _  ->  ( )
   end else begin
      let decl = get_meta_string field.cf_meta Meta.Decl in
      let has_decl = decl <> "" in
      if (has_decl) then
         output ( "      typedef " ^ decl ^ ";\n" );
      output (if is_static then "\t\tstatic " else "\t\t");
      (match  field.cf_expr with
      | Some { eexpr = TFunction function_def } ->
         let nonVirtual = has_meta_key field.cf_meta Meta.NonVirtual in
         let doDynamic =  (nonVirtual || not (is_override class_def field.cf_name ) ) && (reflective class_def field ) in
         if ( is_dynamic_haxe_method field ) then begin
            if ( doDynamic ) then begin
               output ("::Dynamic " ^ remap_name ^ ";\n");
               if (not is_static) && (is_gc_element ctx TCppDynamic) then
                  output ("\t\tinline ::Dynamic _hx_set_" ^ remap_name ^ "(hx::StackContext *_hx_ctx,::Dynamic _hx_v) { HX_OBJ_WB(this,_hx_v.mPtr) return " ^ remap_name ^ "=_hx_v; }\n");
               output (if is_static then "\t\tstatic " else "\t\t");
               output ("inline ::Dynamic &" ^ remap_name ^ "_dyn() " ^ "{return " ^ remap_name^ "; }\n")
            end
         end else begin
            let return_type = (ctx_type_string ctx function_def.tf_type) in
            if ( not is_static && not nonVirtual ) then begin
               let scriptable = Common.defined ctx.ctx_common Define.Scriptable in
               if (not (is_internal_member field.cf_name) && not scriptable ) then begin
                  let key = (join_class_path class_def.cl_path ".") ^ "." ^ field.cf_name in
                  try output (Hashtbl.find ctx.ctx_class_member_types key) with Not_found -> ()
               end else
                  output "virtual ";
            end;
            output (if return_type="Void" then "void" else return_type );

            let remap_name = native_field_name_remap is_static field in
            output (" " ^ remap_name ^ "(" );
            output (ctx_arg_list ctx function_def.tf_args "" );
            output ");\n";
            if ( doDynamic ) then begin
               output (if is_static then "\t\tstatic " else "\t\t");
               output ("::Dynamic " ^ remap_name ^ "_dyn();\n" )
            end;
         end;
         output "\n";
      | _ when has_decl ->
         output ( remap_name ^ "_decl " ^ remap_name ^ ";\n" );
         (* Variable access *)
      | _ ->
         (* Variable access *)
         let tcpp = cpp_type_of ctx field.cf_type in
         let tcppStr = tcpp_to_string tcpp in
         if not is_static && only_stack_access ctx field.cf_type then
            abort ("Variables of type " ^ tcppStr ^ " may not be used as members") field.cf_pos;

         output (tcppStr ^ " " ^ remap_name ^ ";\n" );
         if not is_static && (is_gc_element ctx tcpp) then begin
            let getPtr = match tcpp with | TCppString -> ".__s" | _ -> ".mPtr" in
            output ("\t\tinline " ^ tcppStr ^ " _hx_set_" ^ remap_name ^ "(hx::StackContext *_hx_ctx," ^ tcppStr ^ " _hx_v) { HX_OBJ_WB(this,_hx_v" ^ getPtr ^ ") return " ^ remap_name ^ "=_hx_v; }\n");
         end;

         (* Add a "dyn" function for variable to unify variable/function access *)
         (match follow field.cf_type with
         | _ when nativeGen  -> ()
         | TFun (_,_) ->
            output (if is_static then "\t\tstatic " else "\t\t");
            output ("Dynamic " ^ remap_name ^ "_dyn() { return " ^ remap_name ^ ";}\n" )
         | _ ->  (match field.cf_kind with
            | Var { v_read = AccCall } when (not is_static) && (is_dynamic_accessor ("get_" ^ field.cf_name) "get" field class_def) ->
               output ("\t\tDynamic get_" ^ field.cf_name ^ ";\n" )
            | _ -> ()
            );
            (match field.cf_kind with
            | Var { v_write = AccCall } when (not is_static) &&  (is_dynamic_accessor ("set_" ^ field.cf_name) "set" field class_def) ->
               output ("\t\tDynamic set_" ^ field.cf_name ^ ";\n" )
            | _ -> ()
            )
         )
      );
      end
   ;;

let path_of_string path =
   ["@verbatim"], path
;;


(*
   Get a list of all classes referred to by the class/enum definition
   These are used for "#include"ing the appropriate header files,
   or for building the dependencies in the Build.xml file
*)
let find_referenced_types_flags ctx obj field_name super_deps constructor_deps header_only for_depends include_super_args =
   let types = ref PMap.empty in
   let rec add_type_flag isNative in_path =
      if ( not (PMap.mem in_path !types)) then begin
         types := (PMap.add in_path isNative !types);
         try
            List.iter (add_type_flag isNative) (Hashtbl.find super_deps in_path);
         with Not_found -> ()
      end
   and add_type in_path =
      add_type_flag false in_path
   in
   let add_extern_type decl =
      let tinfo = t_infos decl in
      let include_file = get_meta_string_path tinfo.mt_meta (if for_depends then Meta.Depend else Meta.Include) in
      if (include_file<>"") then
         add_type ( path_of_string include_file )
      else if (not for_depends) && (has_meta_key tinfo.mt_meta Meta.Include) then
         add_type tinfo.mt_path
   in

   let add_extern_class klass =
      add_extern_type (TClassDecl klass)
   in
   let add_native_gen_class klass =
      let include_file = get_meta_string_path klass.cl_meta (if for_depends then Meta.Depend else Meta.Include) in
      if (include_file<>"") then
         add_type ( path_of_string include_file )
      else if for_depends then
         add_type klass.cl_path
      else begin
         let path = klass.cl_path in
         if not klass.cl_interface then
            (* Always include native struct headers directly ... *)
            add_type ( path_of_string ( (join_class_path path "/") ^ ".h") )
         else begin
            add_type_flag true klass.cl_path
         end
      end
   in
   let visited = ref [] in
   let rec visit_type in_type =
      if not (List.exists (fun t2 -> Type.fast_eq in_type t2) !visited) then begin
         visited := in_type :: !visited;
         begin match follow in_type with
         | TMono r -> (match !r with None -> () | Some t -> visit_type t)
         | TEnum (enum,params) -> add_type enum.e_path
         (* If a class has a template parameter, then we treat it as dynamic - except
            for the Array, Class, FastIterator or Pointer classes, for which we do a fully typed object *)
         | TInst (klass,params) ->
            (match klass.cl_path with
            | ([],"Array") | ([],"Class") | (["cpp"],"FastIterator")
            | (["cpp"],"Pointer") | (["cpp"],"ConstPointer") | (["cpp"],"Function")
            | (["cpp"],"RawPointer") | (["cpp"],"RawConstPointer") -> List.iter visit_type params
            | _ when is_native_gen_class klass -> add_native_gen_class klass
            | _ when is_extern_class klass -> add_extern_class klass
            | _ -> (match klass.cl_kind with KTypeParameter _ -> () | _ -> add_type klass.cl_path);
            )
         | TAbstract (a,params) when is_scalar_abstract a ->
            add_extern_type (TAbstractDecl a)
         | TFun (args,haxe_type) -> visit_type haxe_type;
            List.iter (fun (_,_,t) -> visit_type t; ) args;
         | _ -> ()
         end;
         visited := List.tl !visited;
      end
   in
   let rec visit_params expression =
      begin
      let rec visit_expression = fun expression ->
         (* Expand out TTypeExpr (ie, the name of a class, as used for static access etc ... *)
         (match expression.eexpr with
            | TTypeExpr type_def -> ( match type_def with
               | TClassDecl class_def when is_native_gen_class class_def -> add_native_gen_class class_def
               | TClassDecl class_def when is_extern_class class_def -> add_extern_class class_def
               | _ -> add_type (t_path type_def)
               )

            (* Must visit the types, Type.iter will visit the expressions ... *)
            | TTry (e,catches) ->
               List.iter (fun (v,_) -> visit_type v.v_type) catches
            (* Must visit type too, Type.iter will visit the expressions ... *)
            | TNew  (klass,params,_) -> begin
               visit_type (TInst (klass,params));
               try
               let construct_type = Hashtbl.find constructor_deps klass.cl_path in
                  visit_type construct_type.cf_type
               with Not_found -> ();
               end
            (* Must visit type too, Type.iter will visit the expressions ... *)
            | TVar (v,_) ->
               visit_type v.v_type
            (* Must visit enum type too, Type.iter will visit the expressions ... *)
            | TEnumParameter (_,ef,_) -> visit_type (follow ef.ef_type)
            (* Must visit args too, Type.iter will visit the expressions ... *)
            | TFunction func_def ->
               List.iter (fun (v,_) -> visit_type v.v_type) func_def.tf_args;

            | TField( obj, field ) ->
               (match field with
               | FInstance (clazz,params,_)
               | FClosure (Some (clazz,params),_) ->
                   visit_type (TInst (clazz,params))
               | _ -> ()
               )
            | TConst TSuper ->
               (match follow expression.etype with
               | TInst (klass,params) ->
                  (try let construct_type = Hashtbl.find constructor_deps klass.cl_path in
                     visit_type construct_type.cf_type
                  with Not_found -> () )
               | _ -> print_endline ("TSuper : Odd etype ?" ^ ( (ctx_type_string ctx expression.etype)) )
               )
            | _ -> ()
         );
         Type.iter visit_expression expression;
         visit_type (follow expression.etype)
      in
      visit_expression expression
      end
   in
   let visit_field field =
      (* Add the type of the expression ... *)
      visit_type field.cf_type;
      if (not header_only) then
         (match field.cf_expr with
         | Some expression -> visit_params expression | _ -> ());
   in
   let visit_class class_def =
      let fields = List.append class_def.cl_ordered_fields class_def.cl_ordered_statics in
      let fields_and_constructor = List.append fields
         (match class_def.cl_constructor with | Some expr -> [expr] | _ -> [] ) in
      let fields_and_constructor =
         if field_name="*" then
            fields_and_constructor
         else
            List.filter (fun f -> f.cf_name=field_name) fields_and_constructor in
      List.iter visit_field fields_and_constructor;
      if (include_super_args) then
         List.iter visit_field (List.map (fun (a,_,_) -> a ) (all_virtual_functions class_def ));

      (* Add super & interfaces *)
      if is_native_gen_class class_def then
         add_native_gen_class class_def
      else
         add_type class_def.cl_path;
   in
   let visit_enum enum_def =
      add_type enum_def.e_path;
      PMap.iter (fun _ constructor ->
         (match constructor.ef_type with
         | TFun (args,_) ->
            List.iter (fun (_,_,t) -> visit_type t; ) args;
         | _ -> () );
         ) enum_def.e_constrs;
      if (not header_only) then begin
         let meta = Codegen.build_metadata ctx.ctx_common (TEnumDecl enum_def) in
         match meta with Some expr -> visit_params expr | _ -> ();
      end;
   in
   let inc_cmp i1 i2 =
      String.compare (join_class_path i1 ".") (join_class_path i2 ".")
   in

   (* Body of main function *)
   (match obj with
   | TClassDecl class_def -> visit_class class_def;
      (match class_def.cl_init with Some expression -> visit_params expression | _ -> ())
   | TEnumDecl enum_def -> visit_enum enum_def
   | TTypeDecl _ | TAbstractDecl _ -> (* These are expanded *) ());

   let deps = List.sort inc_cmp (List.filter (fun path -> (include_class_header path) ) (pmap_keys !types)) in
   let flags = List.map (fun dep -> PMap.find dep !types) deps in
   deps, flags
;;

let find_referenced_types ctx obj super_deps constructor_deps header_only for_depends include_super_args =
  let deps,_ = find_referenced_types_flags ctx obj "*" super_deps constructor_deps header_only for_depends include_super_args in
  deps
;;


let generate_main_header output_main =
   output_main "#include <hxcpp.h>\n\n";
   output_main "#include <stdio.h>\n\n";
   output_main "extern \"C\" void __hxcpp_main();\n\n";
   output_main "extern \"C\" void __hxcpp_lib_main();\n\n"
;;

let generate_main_footer1 output_main =
   output_main "void __hxcpp_main() {\n";;

let generate_main_footer2 output_main =
   output_main "	}\n\n";
   output_main "void __hxcpp_lib_main() {\n";
   output_main "	HX_TOP_OF_STACK\n";
   output_main "	hx::Boot();\n";
   output_main "	__boot_all();\n";
   output_main "	__hxcpp_main();\n";
   output_main "	}\n"
;;


let generate_main ctx super_deps class_def =
   let common_ctx = ctx.ctx_common in
   (* main routine should be a single static function *)
   let main_expression =
      (match class_def.cl_ordered_statics with
      | [{ cf_expr = Some expression }] -> expression;
      | _ -> assert false ) in
   ignore(find_referenced_types ctx (TClassDecl class_def) super_deps (Hashtbl.create 0) false false false);
   let depend_referenced = find_referenced_types ctx (TClassDecl class_def) super_deps (Hashtbl.create 0) false true false in
   let generate_startup filename is_main =
      (*make_class_directories base_dir ( "src" :: []);*)
      let cpp_file = new_cpp_file common_ctx common_ctx.file ([],filename) in
      let output_main = (cpp_file#write) in

      generate_main_header (cpp_file#write_h);

      List.iter ( add_include cpp_file ) depend_referenced;
      output_main "\n\n";

      if is_main then output_main "\n#include <hx/HxcppMain.h>\n\n";

      generate_main_footer1 output_main;

      let ctx = file_context ctx cpp_file 1 false in
      gen_cpp_init ctx "hxcpp" "__hxcpp_main" "" main_expression;


      generate_main_footer2 output_main;
      cpp_file#close;
   in
   generate_startup "__main__" true;
   generate_startup "__lib__" false
   ;;

let generate_dummy_main common_ctx =
   let generate_startup filename is_main =
      let main_file = new_cpp_file common_ctx common_ctx.file ([],filename) in
      let output_main = (main_file#write) in
      generate_main_header (main_file#write_h);
      if is_main then output_main "\n#include <hx/HxcppMain.h>\n\n";
      generate_main_footer1 output_main;
      generate_main_footer2 output_main;
      main_file#close;
   in
   generate_startup "__main__" true;
   generate_startup "__lib__" false
   ;;

let generate_boot ctx boot_enums boot_classes nonboot_classes init_classes =
   let common_ctx = ctx.ctx_common in
   (* Write boot class too ... *)
   let base_dir = common_ctx.file in
   let boot_file = new_cpp_file common_ctx base_dir ([],"__boot__") in
   let output_boot = (boot_file#write) in
   boot_file#write_h "#include <hxcpp.h>\n\n";

   List.iter ( fun class_path -> boot_file#add_include class_path )
      (boot_enums @ boot_classes @ nonboot_classes);

   let newScriptable = (Common.defined common_ctx Define.Scriptable) in
   if newScriptable then begin
      output_boot "#include <hx/Scriptable.h>\n";
      let funcs = hash_iterate !(ctx.ctx_interface_slot) (fun name id -> (name,id) ) in
      let sorted = List.sort (fun (_,id1) (_,id2) -> id1-id2 ) funcs in
      output_boot "static const char *scriptableInterfaceFuncs[] = {\n\t0,\n\t0,\n";
      List.iter (fun (name,id) -> output_boot ("\t\"" ^ name ^ "\", //" ^ (string_of_int (-id) ) ^ "\n")) sorted;
      output_boot "};\n";
   end;


   output_boot "\nvoid __files__boot();\n";
   output_boot "\nvoid __boot_all()\n{\n";
   output_boot "__files__boot();\n";
   output_boot "hx::RegisterResources( hx::GetResources() );\n";
   if newScriptable then
      output_boot ("hx::ScriptableRegisterNameSlots(scriptableInterfaceFuncs," ^ (string_of_int !(ctx.ctx_interface_slot_count) ) ^ ");\n");

   List.iter ( fun class_path ->
      output_boot ("::" ^ ( join_class_path_remap class_path "::" ) ^ "_obj::__register();\n") )
         (boot_enums @ boot_classes @ nonboot_classes);

   let dump_boot =
      List.iter ( fun class_path ->
         output_boot ("::" ^ ( join_class_path_remap class_path "::" ) ^ "_obj::__boot();\n") ) in

   dump_boot boot_enums;

   List.iter ( fun class_path ->
      output_boot ("::" ^ ( join_class_path_remap class_path "::" ) ^ "_obj::__init__();\n") ) (List.rev init_classes);

   dump_boot (List.filter  (fun path -> is_cpp_class path )  (List.rev boot_classes));
   dump_boot (List.filter  (fun path -> not (is_cpp_class path) )  (List.rev boot_classes));

   output_boot "}\n\n";
   boot_file#close;;


let generate_files common_ctx file_info =
   (* Write __files__ class too ... *)
   let base_dir = common_ctx.file in
   let files_file = new_cpp_file common_ctx base_dir ([],"__files__") in
   let output_files = (files_file#write) in
   let types = common_ctx.types in
   files_file#write_h "#include <hxcpp.h>\n\n";
   output_files "namespace hx {\n";
   output_files "const char *__hxcpp_all_files[] = {\n";
   output_files "#ifdef HXCPP_DEBUGGER\n";
   List.iter ( fun file -> output_files ((const_char_star file)^",\n" ) )
      ( List.sort String.compare ( pmap_keys !file_info) );
   output_files "#endif\n";
   output_files " 0 };\n";
   output_files "\n";

   output_files "const char *__hxcpp_all_files_fullpath[] = {\n";
   output_files "#ifdef HXCPP_DEBUGGER\n";
   List.iter ( fun file -> output_files ((const_char_star (
      Path.get_full_path (try Common.find_file common_ctx file with Not_found -> file)
      ))^",\n" ) )
      ( List.sort String.compare ( pmap_keys !file_info) );
   output_files "#endif\n";
   output_files " 0 };\n";
   output_files "\n";


   output_files "const char *__hxcpp_all_classes[] = {\n";
   output_files "#ifdef HXCPP_DEBUGGER\n";
   List.iter ( fun object_def ->
   (match object_def with
      | TClassDecl class_def when is_extern_class class_def -> ( )
      | TClassDecl class_def when class_def.cl_interface -> ( )
      | TClassDecl class_def ->
         output_files ((const_char_star (join_class_path class_def.cl_path "." )) ^ ",\n")
      | _ -> ( )
      )
   ) types;
   output_files "#endif\n";
   output_files " 0 };\n";

   output_files "} // namespace hx\n";
   output_files "void __files__boot() { __hxcpp_set_debugger_info(hx::__hxcpp_all_classes, hx::__hxcpp_all_files_fullpath); }\n";

   files_file#close;;


let begin_header_file output_h def_string nativeGen =
   output_h ("#ifndef INCLUDED_" ^ def_string ^ "\n");
   output_h ("#define INCLUDED_" ^ def_string ^ "\n\n");
   output_h "#ifndef HXCPP_H\n";
   if nativeGen then begin
      output_h "#ifdef HXCPP_API_LEVEL\n";
      output_h "#include <hxcpp.h>\n";
      output_h "#else\n";
      output_h "#include <hx/Native.h>\n";
      output_h "#endif\n"
   end else begin
      output_h "#include <hxcpp.h>\n"
   end;
   output_h "#endif\n\n";;

let end_header_file output_h def_string =
   output_h ("\n#endif /* INCLUDED_" ^ def_string ^ " */ \n");;

let new_placed_cpp_file common_ctx class_path =
   let base_dir = common_ctx.file in

   if (Common.defined common_ctx Define.Vcproj ) then begin
      make_class_directories base_dir ("src"::[]);
      cached_source_writer common_ctx
         ( base_dir ^ "/src/" ^ ( String.concat "-" (fst class_path) ) ^ "-" ^
         (snd class_path) ^ (source_file_extension common_ctx) )
   end else
      new_cpp_file common_ctx common_ctx.file class_path;;



let generate_enum_files baseCtx enum_def super_deps meta =
   let common_ctx = baseCtx.ctx_common in
   let class_path = enum_def.e_path in
   let just_class_name =  (snd class_path) in
   let class_name =  just_class_name ^ "_obj" in
   let remap_class_name =  ("::" ^ (join_class_path_remap class_path "::") )  in
   (*let cpp_file = new_cpp_file common_ctx.file class_path in*)
   let cpp_file = new_placed_cpp_file common_ctx class_path in
   let output_cpp = (cpp_file#write) in
   let debug = if (has_meta_key enum_def.e_meta Meta.NoDebug) || ( Common.defined  common_ctx Define.NoDebug)
      then 0 else 1 in

   let ctx = file_context baseCtx cpp_file debug false in

   if (debug>1) then
      print_endline ("Found enum definition:" ^ (join_class_path  class_path "::" ));

   cpp_file#write_h "#include <hxcpp.h>\n\n";

   let referenced,flags = find_referenced_types_flags ctx (TEnumDecl enum_def) "*" super_deps (Hashtbl.create 0) false false false in
   List.iter (add_include cpp_file) referenced;

   gen_open_namespace output_cpp class_path;
   output_cpp "\n";

   PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      match constructor.ef_type with
      | TFun (args,_) ->
         output_cpp (remap_class_name ^ " " ^ class_name ^ "::" ^ name ^ "(" ^
            (ctx_tfun_arg_list ctx true args) ^")\n");

         output_cpp ("{\n\treturn hx::CreateEnum< " ^ class_name ^ " >(" ^ (strq name) ^ "," ^
            (string_of_int constructor.ef_index) ^ "," ^ (string_of_int (List.length args)) ^  ")" );
          ExtList.List.iteri (fun i (arg,_,_) -> output_cpp ("->_hx_init(" ^ (string_of_int i) ^ "," ^ (keyword_remap arg) ^ ")")) args;
         output_cpp ";\n}\n\n"
      | _ ->
         output_cpp ( remap_class_name ^ " " ^ class_name ^ "::" ^ name ^ ";\n\n" )
   ) enum_def.e_constrs;


   let constructor_arg_count constructor =
      (match constructor.ef_type with | TFun(args,_) -> List.length args | _ -> 0 )
   in

   output_cpp ("bool " ^ class_name ^ "::__GetStatic(const ::String &inName, ::Dynamic &outValue, hx::PropertyAccess inCallProp)\n{\n");
   PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let dyn = if constructor_arg_count constructor > 0 then "_dyn()" else "" in
      output_cpp ("\tif (inName==" ^ strq name ^ ") { outValue = " ^ class_name ^ "::" ^ keyword_remap name ^ dyn ^ "; return true; }\n" );
   ) enum_def.e_constrs;
   output_cpp ("\treturn super::__GetStatic(inName, outValue, inCallProp);\n}\n\n");

   output_cpp ("HX_DEFINE_CREATE_ENUM(" ^ class_name ^ ")\n\n");

   output_cpp ("int " ^ class_name ^ "::__FindIndex(::String inName)\n{\n");
   PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let idx = string_of_int constructor.ef_index in
      output_cpp ("\tif (inName==" ^ (strq name) ^ ") return " ^ idx ^ ";\n") ) enum_def.e_constrs;
   output_cpp ("\treturn super::__FindIndex(inName);\n");
   output_cpp ("}\n\n");

   (* Dynamic versions of constructors *)
   let dump_dynamic_constructor _ constr =
      let count = constructor_arg_count constr in
      if (count>0) then begin
         let nargs = string_of_int count in
         output_cpp ("STATIC_HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ "," ^
               (keyword_remap constr.ef_name) ^ ",return)\n\n");
      end
   in
   PMap.iter dump_dynamic_constructor enum_def.e_constrs;


   output_cpp ("int " ^ class_name ^ "::__FindArgCount(::String inName)\n{\n");
   PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let count = string_of_int (constructor_arg_count constructor) in
      output_cpp ("\tif (inName==" ^ (strq name) ^ ") return " ^ count ^ ";\n") ) enum_def.e_constrs;
      output_cpp ("\treturn super::__FindArgCount(inName);\n");
      output_cpp ("}\n\n");

   (* Dynamic "Get" Field function - string version *)
   output_cpp ("hx::Val " ^ class_name ^ "::__Field(const ::String &inName,hx::PropertyAccess inCallProp)\n{\n");
   let dump_constructor_test _ constr =
      output_cpp ("\tif (inName==" ^ (strq constr.ef_name) ^ ") return " ^
                  (keyword_remap constr.ef_name) );
      if ( (constructor_arg_count constr) > 0 ) then output_cpp "_dyn()";
      output_cpp (";\n")
   in
   PMap.iter dump_constructor_test enum_def.e_constrs;
   output_cpp ("\treturn super::__Field(inName,inCallProp);\n}\n\n");

   output_cpp ("static ::String " ^ class_name ^ "_sStaticFields[] = {\n");
   let sorted =
      List.sort (fun f1 f2 -> (PMap.find f1 enum_def.e_constrs ).ef_index -
               (PMap.find f2 enum_def.e_constrs ).ef_index )
         (pmap_keys enum_def.e_constrs) in

    List.iter (fun name -> output_cpp ("\t" ^ (strq name) ^ ",\n") ) sorted;

   output_cpp "\t::String(null())\n};\n\n";

   (* ENUM - Mark static as used by GC - they are const now, so no marking*)
   (* ENUM - Visit static as used by GC - none *)

   output_cpp ("hx::Class " ^ class_name ^ "::__mClass;\n\n");

   output_cpp ("Dynamic __Create_" ^ class_name ^ "() { return new " ^ class_name ^ "; }\n\n");

   output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
   let text_name = str (join_class_path class_path ".") in
   output_cpp ("\nhx::Static(__mClass) = hx::_hx_RegisterClass(" ^ text_name ^
               ", hx::TCanCast< " ^ class_name ^ " >," ^ class_name ^ "_sStaticFields,0,\n");
   output_cpp ("\t&__Create_" ^ class_name ^ ", &__Create,\n");
   output_cpp ("\t&super::__SGetClass(), &Create" ^ class_name ^ ", 0\n");
   output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n    , 0\n#endif\n");
   output_cpp ("#ifdef HXCPP_SCRIPTABLE\n    , 0\n#endif\n");
      output_cpp (");\n");
   output_cpp ("\t__mClass->mGetStaticField = &" ^ class_name ^"::__GetStatic;\n");
   output_cpp "}\n\n";

   output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
   (match meta with
      | Some expr ->
         let ctx = file_context ctx cpp_file 1 false in
         gen_cpp_init ctx class_name "boot" "__mClass->__meta__ = " expr
      | _ -> () );
   PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      match constructor.ef_type with
      | TFun (_,_) -> ()
      | _ ->
         output_cpp ( (keyword_remap name) ^ " = hx::CreateConstEnum< " ^ class_name ^ " >(" ^ (str name) ^  "," ^
            (string_of_int constructor.ef_index) ^ ");\n" )
   ) enum_def.e_constrs;
   output_cpp ("}\n\n");




   output_cpp "\n";
   gen_close_namespace output_cpp class_path;
   cpp_file#close;

   let h_file = new_header_file common_ctx common_ctx.file class_path in
   let super = "hx::EnumBase_obj" in
   let output_h = (h_file#write) in
   let def_string = join_class_path class_path "_"  in

   let ctx = file_context baseCtx h_file debug true in

   begin_header_file (h_file#write_h) def_string false;

   List.iter2 (fun r f -> gen_forward_decl h_file r f) referenced flags;

   gen_open_namespace output_h class_path;

   output_h "\n\n";
   output_h ("class " ^ class_name ^ " : public " ^ super ^ "\n");
   output_h ("{\n\ttypedef " ^ super ^ " super;\n");
   output_h ("\t\ttypedef " ^ class_name ^ " OBJ_;\n");
   output_h "\n\tpublic:\n";
   output_h ("\t\t" ^ class_name ^ "() {};\n");
   output_h ("\t\tHX_DO_ENUM_RTTI;\n");
   output_h ("\t\tstatic void __boot();\n");
   output_h ("\t\tstatic void __register();\n");
   output_h ("\t\tstatic bool __GetStatic(const ::String &inName, Dynamic &outValue, hx::PropertyAccess inCallProp);\n");
   output_h ("\t\t::String GetEnumName( ) const { return " ^ (str (join_class_path class_path "."))  ^ "; }\n" );
   output_h ("\t\t::String __ToString() const { return " ^ (str (just_class_name ^ ".") )^ " + _hx_tag; }\n\n");


   PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      output_h ( "\t\tstatic " ^  remap_class_name ^ " " ^ name );
      match constructor.ef_type with
      | TFun (args,_) ->
         output_h ( "(" ^ (ctx_tfun_arg_list ctx true args) ^");\n");
         output_h ( "\t\tstatic ::Dynamic " ^ name ^ "_dyn();\n");
      | _ ->
         output_h ";\n";
         output_h ( "\t\tstatic inline " ^  remap_class_name ^ " " ^ name ^
                  "_dyn() { return " ^name ^ "; }\n" );
   ) enum_def.e_constrs;

   output_h "};\n\n";

   gen_close_namespace output_h class_path;

   end_header_file output_h def_string;
   h_file#close
;;

let generate_enum_deps ctx enum_def super_deps =
   find_referenced_types ctx (TEnumDecl enum_def) super_deps (Hashtbl.create 0) false true false
;;

let list_iteri func in_list =
   let idx = ref 0 in
   List.iter (fun elem -> func !idx elem; idx := !idx + 1 ) in_list
;;

let has_new_gc_references ctx class_def =
   match class_def.cl_dynamic with
   | Some _ -> true
   | _ -> (
      let is_gc_reference field =
      (should_implement_field field) && (is_data_member field) && not (ctx_cant_be_null ctx field.cf_type)
      in
      List.exists is_gc_reference class_def.cl_ordered_fields
      )
;;


let rec has_gc_references ctx class_def =
   ( match class_def.cl_super with
   | Some def when has_gc_references ctx (fst def) -> true
   | _ -> false )
   || has_new_gc_references ctx class_def
;;

let rec find_next_super_iteration ctx class_def =
   match class_def.cl_super with
   | Some  (klass,params) when has_new_gc_references ctx klass ->
        tcpp_to_string_suffix "_obj" (cpp_instance_type ctx klass params)
   | Some  (klass,_) -> find_next_super_iteration ctx klass
   | _ -> "";
;;

let has_init_field class_def =
   match class_def.cl_init with
   | Some _ -> true
   | _ -> false;;


let is_abstract_impl class_def = match class_def.cl_kind with
   | KAbstractImpl _ -> true
   | _ -> false
;;

let variable_field field =
   (match field.cf_expr with
   | Some { eexpr = TFunction function_def } -> is_dynamic_haxe_method field
   | _ -> true)
;;

let is_readable class_def field =
   (match field.cf_kind with
   | Var { v_read = AccNever } when not (is_physical_field field) -> false
   | Var { v_read = AccInline } -> false
   | Var _ when is_abstract_impl class_def -> false
   | _ -> true)
;;

let is_writable class_def field =
   (match field.cf_kind with
   | Var { v_write = AccNever } when not (is_physical_field field) -> false
   | Var { v_read = AccInline } -> false
   | Var _ when is_abstract_impl class_def -> false
   | _ -> true)
;;


let statics_except_meta class_def = (List.filter (fun static -> static.cf_name <> "__meta__" && static.cf_name <> "__rtti") class_def.cl_ordered_statics);;

let has_set_member_field class_def =
   implement_dynamic_here class_def || (
      let reflect_fields = List.filter (reflective class_def) (class_def.cl_ordered_fields) in
      let reflect_writable = List.filter (is_writable class_def) reflect_fields in
      List.exists variable_field reflect_writable
   )
;;


let has_set_static_field class_def =
      let reflect_fields = List.filter (reflective class_def) (statics_except_meta class_def) in
      let reflect_writable = List.filter (is_writable class_def) reflect_fields in
      List.exists variable_field reflect_writable
;;


let has_get_fields class_def =
   implement_dynamic_here class_def || (
      let is_data_field field = (match follow field.cf_type with | TFun _ -> false | _ -> true) in
      List.exists is_data_field class_def.cl_ordered_fields
   )
;;

let has_get_member_field class_def =
   implement_dynamic_here class_def || (
      let reflect_fields = List.filter (reflective class_def) (class_def.cl_ordered_fields) in
      List.exists (is_readable class_def) reflect_fields
   )
;;


let has_get_static_field class_def =
      let reflect_fields = List.filter (reflective class_def) (statics_except_meta class_def) in
      List.exists (is_readable class_def) reflect_fields
;;



let has_boot_field class_def =
   match class_def.cl_init with
   | None -> List.exists has_field_init (List.filter should_implement_field class_def.cl_ordered_statics)
   | _ -> true
;;

let cpp_tfun_signature ctx include_names args return_type =
  let argList = ctx_tfun_arg_list ctx include_names args in
  let returnType = ctx_type_string ctx return_type in
  ("( " ^ returnType ^ " (hx::Object::*)(" ^ argList ^ "))")
;;

exception FieldFound of tclass_field;;

let find_class_implementation ctx class_def name interface =
   let rec find def =
      List.iter (fun f -> if f.cf_name=name then raise (FieldFound f) ) def.cl_ordered_fields;
      match def.cl_super with
      | Some (def,_) -> find def
      | _ -> ()
   in
   try
     find class_def;
     abort ("Could not find implementation of " ^ name ^ " in " ^
        (join_class_path class_def.cl_path ".") ^ " required by " ^ (join_class_path interface.cl_path ".")) class_def.cl_pos
   with FieldFound field ->
      match follow field.cf_type, field.cf_kind  with
      | _, Method MethDynamic -> ""
      | TFun (args,return_type), Method _ ->
         cpp_tfun_signature ctx false args return_type
      | _,_ -> ""
;;


let is_macro meta =
   Meta.has Meta.Macro meta
;;


let cpp_get_interface_slot ctx name =
   try Hashtbl.find !(ctx.ctx_interface_slot) name
   with Not_found -> begin
      let result = !(ctx.ctx_interface_slot_count) in
      Hashtbl.replace !(ctx.ctx_interface_slot) name result;
      ctx.ctx_interface_slot_count := !(ctx.ctx_interface_slot_count) + 1;
      result
   end
;;

let access_str a = match a with
   | AccNormal -> "AccNormal"
   | AccNo -> "AccNo"
   | AccNever -> "AccNever"
   | AccResolve -> "AccResolve"
   | AccCall -> "AccCall"
   | AccInline -> "AccInline"
   | AccRequire(_,_) -> "AccRequire"
   | AccCtor -> "AccCtor";;


let script_type t optional = if optional then begin
   match type_string t with
   | "::String" -> "String"
   | _ -> "Object"
   end else match type_string t with
   | "bool" -> "Int"
   | "int" -> "Int"
   | "Float" -> "Float"
   | "::String" -> "String"
   | "Null" -> "Void"
   | "Void" -> "Void"
   | _ -> "Object"
;;

let script_signature t optional = match script_type t optional with
   | "Bool" -> "b"
   | "Int" -> "i"
   | "Float" -> "f"
   | "String" -> "s"
   | "Void" -> "v"
   | "void" -> "v"
   | _ -> "o"
;;

let script_size_type t optional = match script_type t optional with
   | "Object" -> "void *"
   | "Int" -> "int"
   | "Bool" -> "bool"
   | x -> x
;;



let constructor_arg_var_list class_def ctx =
   match class_def.cl_constructor with
   | Some definition ->
            (match definition.cf_expr with
               | Some { eexpr = TFunction function_def } ->
                  List.map (fun (v,o) -> (v.v_name, ctx_arg_type_name ctx v.v_name o v.v_type "__o_"))
                        function_def.tf_args;
               | _ ->
                  (match follow definition.cf_type with
                     | TFun (args,_) -> List.map (fun (a,_,t) -> (a, (ctx_type_string ctx t, a)) )  args
                     | _ -> [])
            )
   | _ -> []
;;

let can_inline_constructor ctx class_def super_deps constructor_deps =
   match class_def.cl_constructor with
   | Some { cf_expr= Some super_func } ->
       let is_simple = ref true in
       let rec check_simple e =
          (match e.eexpr with
          | TReturn _ -> is_simple := false
          | TArrayDecl e when List.length e > 0 -> is_simple := false
          | _ -> ()
          );
          if !is_simple then Type.iter check_simple e
       in
       check_simple super_func;
       !is_simple && (
          let rec known_classes class_def so_far = match class_def.cl_super with
          | Some super -> known_classes (fst super) ((fst super).cl_path ::  so_far)
          | _ -> so_far in
          let allowed = known_classes class_def [class_def.cl_path] in
          (* Check to see if all the types required by the constructor are already in the header *)
          (* This is quite restrictive, since most classes are forward-declared *)
          let deps,_ = find_referenced_types_flags ctx (TClassDecl class_def) "new" super_deps constructor_deps false false true in
          List.for_all (fun dep ->  List.mem dep allowed ) deps
       )
   | _ -> true
;;

let has_dynamic_member_functions class_def =
List.fold_left (fun result field ->
   match field.cf_expr with
   | Some { eexpr = TFunction function_def } when is_dynamic_haxe_method field -> true
   | _ -> result ) false class_def.cl_ordered_fields
;;


let generate_protocol_delegate ctx class_def output =
   let protocol = get_meta_string class_def.cl_meta Meta.ObjcProtocol in
   let full_class_name =  ("::" ^ (join_class_path_remap class_def.cl_path "::") ) ^ "_obj"  in
   let name = "_hx_" ^ protocol ^ "_delegate" in
   output ("@interface " ^ name ^ " : NSObject<" ^ protocol ^ "> {\n");
   output ("\t::hx::Object *haxeObj;\n");
   output ("}\n");
   output ("@end\n\n");
   output ("@implementation " ^ name ^ "\n");
   output ("- (id)initWithImplementation:( ::hx::Object *)inInplemnetation {\n");
   output ("  if (self = [super init]) {\n");
   output ("     self->haxeObj = inInplemnetation;\n");
   output ("     GCAddRoot(&self->haxeObj);\n");
   output ("  }\n");
   output ("  return self;\n");
   output ("}\n");
   output ("- (void)dealloc {\n");
   output ("   GCRemoveRoot(&self->haxeObj);\n");
   output ("   #ifndef OBJC_ARC\n");
   output ("   [super dealloc];\n");
   output ("   #endif\n");
   output ("}\n\n");

   let dump_delegate field =
      match field.cf_type with
      |  TFun(args,ret) ->
         let retStr = ctx_type_string ctx ret in
         let nativeName = get_meta_string field.cf_meta Meta.ObjcProtocol in
         let fieldName,argNames = if nativeName<>"" then begin
            let parts = ExtString.String.nsplit nativeName ":" in
            List.hd parts, parts
         end else
            field.cf_name, List.map (fun (n,_,_) -> n ) args
         in
         output ("- (" ^ retStr ^ ") " ^ fieldName );

         let first = ref true in
         (try
            List.iter2 (fun (name,_,argType) signature_name ->
                if !first then
                  output (" :(" ^ (ctx_type_string ctx argType) ^ ")" ^ name )
                else
                  output (" " ^ signature_name ^ ":(" ^ (ctx_type_string ctx argType) ^ ")" ^ name );
                first := false;
                ) args argNames;
         with Invalid_argument _ -> begin
           abort (
              let argString  = String.concat "," (List.map (fun (name,_,_) -> name) args) in
             "Invalid arg count in delegate in " ^ field.cf_name ^ " '" ^ field.cf_name ^ "," ^
             (argString) ^ "' != '" ^ (String.concat "," argNames) ^ "'" ) field.cf_pos
         end);
         output (" {\n");
         output ("\thx::NativeAttach _hx_attach;\n");
         output ( (if retStr="void" then "\t" else "\treturn ") ^ full_class_name ^ "::" ^ (keyword_remap field.cf_name) ^ "(haxeObj");
         List.iter (fun (name,_,_) -> output ("," ^ name)) args;
         output (");\n}\n\n");
      | _ -> ()
   in
   List.iter dump_delegate class_def.cl_ordered_fields;

   output ("@end\n\n");
;;


(*
  Generate class header and cpp files

*)


let generate_class_files baseCtx super_deps constructor_deps class_def inScriptable =

   (* Shorcuts *)
   let common_ctx = baseCtx.ctx_common in
   let class_path = class_def.cl_path in
   let nativeGen = has_meta_key class_def.cl_meta Meta.NativeGen in
   let class_name = (snd class_path) ^ (if nativeGen then "" else "_obj") in
   let dot_name = join_class_path class_path "." in
   let smart_class_name =  (snd class_path)  in
   let class_name_text = join_class_path class_path "." in
   let gcName = const_char_star class_name_text in
   let ptr_name = "hx::ObjectPtr< " ^ class_name ^ " >" in
   let debug = if (has_meta_key class_def.cl_meta Meta.NoDebug) || ( Common.defined  baseCtx.ctx_common Define.NoDebug)
      then 0 else 1 in
   let scriptable = inScriptable && not class_def.cl_private in

   let classId = try Hashtbl.find baseCtx.ctx_type_ids (class_text class_def.cl_path) with Not_found -> Int32.of_int 0 in
   let classIdTxt = Printf.sprintf "0x%08lx" classId in

   (* Config *)
   let implement_dynamic = implement_dynamic_here class_def in
   let override_iteration = (not nativeGen) && (has_new_gc_references baseCtx class_def) in
   let dynamic_interface_closures =  (Common.defined baseCtx.ctx_common Define.DynamicInterfaceClosures) in

   (* All interfaces (and sub-interfaces) implemented *)
   let implemented_hash = Hashtbl.create 0 in
   let native_implemented = Hashtbl.create 0 in
   List.iter (fun imp ->
      let rec descend_interface interface =
         let intf_def = (fst interface) in
         let interface_name = cpp_interface_impl_name baseCtx intf_def in
         let hash = if is_native_gen_class intf_def then native_implemented else implemented_hash in
         if ( not (Hashtbl.mem hash interface_name) ) then begin
            Hashtbl.replace hash interface_name intf_def;
            List.iter descend_interface intf_def.cl_implements;
         end;
         match intf_def.cl_super with
         | Some (interface,params) -> descend_interface (interface,params)
         | _ -> ()
      in descend_interface imp
   ) (real_interfaces class_def.cl_implements);
   let implemented = hash_keys implemented_hash in
   let implementsNative = (Hashtbl.length native_implemented) > 0 in

   let dynamic_functions = List.fold_left (fun result field ->
      match field.cf_expr with
      | Some { eexpr = TFunction function_def } when is_dynamic_haxe_method field ->
            (keyword_remap field.cf_name) :: result
      | _ -> result ) [] class_def.cl_ordered_fields
   in


   (* Field groups *)
   let statics_except_meta = statics_except_meta class_def in
   let implemented_fields = List.filter should_implement_field statics_except_meta in
   let implemented_instance_fields = List.filter should_implement_field class_def.cl_ordered_fields in

   let reflect_member_fields = List.filter (reflective class_def) class_def.cl_ordered_fields in
   let reflect_member_readable = List.filter (is_readable class_def) reflect_member_fields in
   let reflect_member_writable = List.filter (is_writable class_def) reflect_member_fields in
   let reflect_write_member_variables = List.filter variable_field reflect_member_writable in

   let reflect_static_fields = List.filter (reflective class_def) (statics_except_meta) in
   let reflect_static_readable = List.filter (is_readable class_def) reflect_static_fields in
   let reflect_static_writable = List.filter (is_writable class_def) reflect_static_fields in
   let reflect_write_static_variables = List.filter variable_field reflect_static_writable in

   let reflective_members = List.filter (reflective class_def) implemented_instance_fields in

   (* native interface glue *)
   let neededInterfaceFunctions = if not implementsNative then []
      else begin
         let have = Hashtbl.create 0 in
         List.iter (fun field -> Hashtbl.replace have field.cf_name () ) implemented_instance_fields;
         let want = ref [] in
         Hashtbl.iter (fun _ intf_def ->
            List.iter (fun field ->
               if not (Hashtbl.mem have field.cf_name) then begin
                  Hashtbl.replace have field.cf_name ();
                  want := field :: !want;
               end
               ) intf_def.cl_ordered_fields;
          ) native_implemented;
         !want;
      end
   in

   let not_toString = fun (field,args,_) -> field.cf_name<>"toString" || class_def.cl_interface in
   let functions = List.filter not_toString (all_virtual_functions class_def) in

   (* Constructor definition *)
   let cargs = (constructor_arg_var_list class_def baseCtx) in
   let constructor_type_var_list = List.map snd cargs in
   let constructor_var_list = List.map snd constructor_type_var_list in
   let constructor_type_args = String.concat ","
            (List.map (fun (t,a) -> t ^ " " ^ a) constructor_type_var_list) in
   let constructor_args = String.concat "," constructor_var_list in

   let isContainer = if (has_gc_references common_ctx class_def) then "true" else "false" in

   let outputConstructor ctx out isHeader =
      let classScope = if isHeader then "" else class_name ^ "::" in
      out (ptr_name ^ " " ^ classScope ^ "__new(" ^constructor_type_args ^") {\n");
      out ("\t" ^ ptr_name ^ " __this = new " ^ class_name ^ "();\n");
         out ("\t__this->__construct(" ^ constructor_args ^ ");\n");
      out ("\treturn __this;\n");
      out ("}\n\n");

      out ((if isHeader then "static " else "") ^ ptr_name ^ " " ^ classScope ^ "__alloc(hx::Ctx *_hx_ctx" ^ (if constructor_type_args="" then "" else "," ^constructor_type_args)  ^") {\n");
      out ("\t" ^ class_name ^ " *__this = (" ^ class_name ^ "*)(hx::Ctx::alloc(_hx_ctx, sizeof(" ^ class_name ^ "), " ^ isContainer ^", " ^ gcName ^ "));\n");
      out ("\t*(void **)__this = " ^ class_name ^ "::_hx_vtable;\n");
      let rec dump_dynamic class_def =
         if has_dynamic_member_functions class_def then
            out ("\t" ^ (join_class_path_remap class_def.cl_path "::") ^ "_obj::__alloc_dynamic_functions(_hx_ctx,__this);\n")
         else match class_def.cl_super with
         | Some super -> dump_dynamic (fst super)
         | _ -> ()
      in
      dump_dynamic class_def;

      if isHeader then begin
        match class_def.cl_constructor with
         | Some ( { cf_expr = Some ( { eexpr = TFunction(function_def) } ) } as definition ) ->
            let old_debug = ctx.ctx_debug_level in
            if has_meta_key definition.cf_meta Meta.NoDebug then
               ctx.ctx_debug_level <- 0;
            ctx.ctx_real_this_ptr <- false;
            gen_cpp_function_body ctx class_def false "new" function_def "" "" (has_meta_key definition.cf_meta Meta.NoDebug);
            out "\n";

            ctx.ctx_debug_level <- old_debug;
         | _ -> ()
      end else
         out ("\t__this->__construct(" ^ constructor_args ^ ");\n");

      out ("\treturn __this;\n");
      out ("}\n\n");
   in

   (* State *)
   let header_glue = ref [] in


   let cpp_file = new_placed_cpp_file baseCtx.ctx_common class_path in
   let cpp_ctx = file_context baseCtx cpp_file debug false in

   let inlineContructor = can_inline_constructor cpp_ctx class_def super_deps constructor_deps in

 (*
   Generate cpp code
 *)
 let generate_class_cpp () =
   (*let cpp_file = new_cpp_file common_ctx.file class_path in*)
   let ctx = cpp_ctx in
   let output_cpp = (cpp_file#write) in

   let class_super_name = (match class_def.cl_super with
      | Some (klass, params) -> (tcpp_to_string_suffix "_obj" (cpp_instance_type ctx klass params) )
      | _ -> "") in
   if (debug>1) then print_endline ("Found class definition:" ^ (join_class_path class_def.cl_path "::"));


   cpp_file#write_h "#include <hxcpp.h>\n\n";

   let all_referenced = find_referenced_types ctx (TClassDecl class_def) super_deps constructor_deps false false scriptable in
   List.iter ( add_include cpp_file  ) all_referenced;


   if (scriptable) then
      cpp_file#write_h "#include <hx/Scriptable.h>\n";

   cpp_file#write_h "\n";

   output_cpp ( get_class_code class_def Meta.CppFileCode );
   let inc = get_meta_string_path class_def.cl_meta Meta.CppInclude in
   if (inc<>"") then
      output_cpp ("#include \"" ^ inc ^ "\"\n");

   gen_open_namespace output_cpp class_path;
   output_cpp "\n";

   output_cpp ( get_class_code class_def Meta.CppNamespaceCode );

   if (not class_def.cl_interface) && not nativeGen then begin
      output_cpp ("void " ^ class_name ^ "::__construct(" ^ constructor_type_args ^ ")");
      (match class_def.cl_constructor with
         | Some ( { cf_expr = Some ( { eexpr = TFunction(function_def) } ) } as definition ) ->
            let old_debug = ctx.ctx_debug_level in
            if has_meta_key definition.cf_meta Meta.NoDebug then
               ctx.ctx_debug_level <- 0;

            gen_cpp_function_body ctx class_def false "new" function_def "" "" (has_meta_key definition.cf_meta Meta.NoDebug);
            output_cpp "\n";

            ctx.ctx_debug_level <- old_debug;
         | _ ->  output_cpp " { }\n\n"
      );

      (* Destructor goes in the cpp file so we can "see" the full definition of the member vars *)
      output_cpp ("Dynamic " ^ class_name ^ "::__CreateEmpty() { return new " ^ class_name ^ "; }\n\n");
      output_cpp ("void *" ^ class_name ^ "::_hx_vtable = 0;\n\n");

      output_cpp ("Dynamic " ^ class_name ^ "::__Create(hx::DynamicArray inArgs)\n");
      output_cpp ("{\n\t" ^ ptr_name ^ " _hx_result = new " ^ class_name ^ "();\n");
      output_cpp ("\t_hx_result->__construct(" ^ (array_arg_list constructor_var_list) ^ ");\n");
      output_cpp ("\treturn _hx_result;\n}\n\n");

      let rec addParent cls others = match cls.cl_super with
      | Some (super,_) -> ( try (
         let parentId = Hashtbl.find ctx.ctx_type_ids (class_text super.cl_path)  in
         addParent super (parentId :: others);
         ) with Not_found -> others )
      | _ -> others
      in
      let implemented_classes = addParent class_def [classId ; (Int32.of_int 1)] in
      let implemented_classes = List.sort compare implemented_classes in

      output_cpp ("bool " ^ class_name ^ "::_hx_isInstanceOf(int inClassId) {\n");
      let txt cId = Printf.sprintf "0x%08lx" cId in
      let rec dump_classes indent classes = match classes with
         | [] -> ()
         | [c] -> output_cpp (indent ^ "return inClassId==(int)" ^ (txt c) ^ ";\n" )
         | [c;c1] -> output_cpp (indent ^ "return inClassId==(int)" ^ (txt c) ^ " || inClassId==(int)" ^ (txt c1) ^ ";\n" )
         | _ ->
            let len = List.length classes in
            let mid = List.nth classes (len / 2) in
            let low,high = List.partition (fun e -> e<=mid) classes in
            output_cpp (indent ^ "if (inClassId<=(int)" ^ (txt mid) ^ ") {\n");
            dump_classes (indent ^ "\t") low;
            output_cpp (indent ^ "} else {\n");
            dump_classes (indent ^ "\t") high;
            output_cpp (indent ^ "}\n");
      in
      dump_classes "\t" implemented_classes;
      output_cpp ("}\n\n");

      if ( List.length implemented) > 0 then begin
            let alreadyGlued = Hashtbl.create 0 in
            let cname = "_hx_" ^ (join_class_path class_def.cl_path "_") in
            let implname = (cpp_class_name class_def) in
            let cpp_glue = ref [] in
            List.iter (fun interface_name ->
               (try let interface = Hashtbl.find implemented_hash interface_name in
                   output_cpp ("static " ^ cpp_class_name interface ^ " " ^ cname ^ "_" ^ interface_name ^ "= {\n" );
                   let rec gen_interface_funcs interface =
                      let gen_field field = (match follow field.cf_type, field.cf_kind  with
                      | _, Method MethDynamic -> ()
                      | TFun (args,return_type), Method _ ->
                         let cast = cpp_tfun_signature ctx false args return_type in
                         let class_implementation = find_class_implementation ctx class_def field.cf_name interface in
                         let realName= cpp_member_name_of field in
                         let castKey = realName ^ "::" ^ cast in
                         (* C++ can't work out which function it needs to take the addrss of
                           when the implementation is overloaded - currently the map-set functions.
                           Change the castKey to force a glue function in this case (could double-cast the pointer, but it is ugly)
                         *)
                         let castKey = if interface_name="_hx_haxe_IMap" && realName="set" then castKey ^ "*" else castKey in
                         let implementationKey = realName ^ "::" ^ class_implementation in
                         if castKey <> implementationKey && not (Hashtbl.mem alreadyGlued castKey) then begin
                            Hashtbl.replace alreadyGlued castKey ();
                            let glue =  Printf.sprintf "%s_%08lx" field.cf_name (gen_hash32 0 cast) in
                            let argList = ctx_tfun_arg_list ctx true args in
                            let returnType = ctx_type_string ctx return_type in
                            let returnStr = if returnType="void" then "" else "return " in
                            let cppCode = returnType ^ " " ^ class_name ^ "::" ^ glue ^ "(" ^ argList ^ ") {\n" ^
                               "\t\t\t" ^ returnStr ^ realName ^ "(" ^ cpp_arg_names args ^ ");\n}\n" in
                            let headerCode = "\t\t" ^ returnType ^ " " ^ glue ^ "(" ^ argList ^ ");\n" in
                            header_glue := headerCode :: !header_glue;
                            cpp_glue := cppCode :: !cpp_glue;
                            output_cpp ("	" ^ cast ^ "&" ^ implname ^ "::" ^ glue ^ ",\n");
                         end else
                            output_cpp ("	" ^ cast ^ "&" ^ implname ^ "::" ^ realName ^ ",\n");
                      | _ -> () )
                      in
                      (match interface.cl_super with
                      | Some super -> gen_interface_funcs (fst super)
                      | _ -> ());
                      List.iter gen_field interface.cl_ordered_fields;
                      in
                   gen_interface_funcs interface;
                   output_cpp "};\n\n";
               with Not_found -> () )
               ) implemented;

            output_cpp (String.concat "\n" !cpp_glue);

            output_cpp ("void *" ^ class_name ^ "::_hx_getInterface(int inHash) {\n");
            output_cpp "\tswitch(inHash) {\n";
            List.iter (fun interface_name ->
               try let interface = Hashtbl.find implemented_hash interface_name in
                  output_cpp ("\t\tcase (int)" ^ (cpp_class_hash interface) ^ ": return &" ^ cname ^ "_" ^ interface_name ^ ";\n")
               with Not_found -> ()
               ) implemented;

            output_cpp "\t}\n";

            if class_super_name="" then begin
               output_cpp ("\t#ifdef HXCPP_SCRIPTABLE\n");
               output_cpp ("\treturn super::_hx_getInterface(inHash);\n");
               output_cpp ("\t#else\n");
               output_cpp ("\treturn 0;\n");
               output_cpp ("\t#endif\n")
            end else
               output_cpp ("\treturn super::_hx_getInterface(inHash);\n");
            output_cpp ("}\n\n");
      end;
   end;

   (match class_def.cl_init with
   | Some expression ->
      let ctx = file_context baseCtx cpp_file debug false in
      output_cpp ("void " ^ class_name^ "::__init__()");
      gen_cpp_init ctx (cpp_class_name class_def) "__init__" "" (mk_block expression);
      output_cpp "\n\n";
   | _ -> ());


   let dump_field_name = (fun field -> output_cpp ("\t" ^  (str field.cf_name) ^ ",\n")) in

   List.iter
      (gen_field ctx class_def class_name smart_class_name dot_name false class_def.cl_interface)
      class_def.cl_ordered_fields;
   List.iter
      (gen_field ctx class_def class_name smart_class_name dot_name true class_def.cl_interface) statics_except_meta;
   output_cpp "\n";

   if (List.length dynamic_functions > 0) then begin
      output_cpp ("void " ^ class_name ^ "::__alloc_dynamic_functions(hx::Ctx *_hx_ctx," ^ class_name ^ " *_hx_obj) {\n");
      List.iter (fun name ->
             output_cpp ("\tif (!_hx_obj->" ^ name ^".mPtr) _hx_obj->" ^ name ^ " = new __default_" ^ name ^ "(_hx_obj);\n")
         ) dynamic_functions;
      (match class_def.cl_super with
      | Some super ->
          let rec find_super class_def =
             if has_dynamic_member_functions class_def then begin
                let super_name = (join_class_path_remap class_def.cl_path "::" ) ^ "_obj" in
                output_cpp ("\t" ^ super_name ^ "::__alloc_dynamic_functions(_hx_ctx,_hx_obj);\n")
             end else
                match class_def.cl_super with
                | Some super -> find_super (fst super)
                | _ -> ()
          in
          find_super (fst super);
      | _ -> ()
      );
      output_cpp ("}\n");
   end;

   if (not class_def.cl_interface) && not nativeGen && not inlineContructor then
      outputConstructor ctx output_cpp false;


   (* Initialise non-static variables *)
   if ( (not class_def.cl_interface) && (not nativeGen) ) then begin
      output_cpp (class_name ^ "::" ^ class_name ^  "()\n{\n");
      List.iter (fun name ->
             output_cpp ("\t" ^ name ^ " = new __default_" ^ name ^ "(this);\n")
         ) dynamic_functions;
      output_cpp "}\n\n";


      let dump_field_iterator macro field =
         if (is_data_member field) then begin
            let remap_name = keyword_remap field.cf_name in
            output_cpp ("\t" ^ macro ^ "(" ^ remap_name ^ ",\"" ^ field.cf_name^ "\");\n");

               (match field.cf_kind with Var { v_read = AccCall } when (is_dynamic_accessor ("get_" ^ field.cf_name) "get" field class_def) ->
                  let name = "get_" ^ field.cf_name in
                  output_cpp ("\t" ^ macro ^ "(" ^ name ^ "," ^ "\"" ^ name ^ "\");\n" ) | _ -> ());
               (match field.cf_kind with Var { v_write = AccCall } when  (is_dynamic_accessor ("set_" ^ field.cf_name) "set" field class_def) ->
                  let name = "set_" ^ field.cf_name in
                  output_cpp ("\t" ^ macro ^ "(" ^ name ^ "," ^ "\"" ^ name ^ "\");\n" ) | _ -> ());
            end
      in


      if (override_iteration) then begin
         let super_needs_iteration = find_next_super_iteration ctx class_def in
         (* MARK function - explicitly mark all child pointers *)
         output_cpp ("void " ^ class_name ^ "::__Mark(HX_MARK_PARAMS)\n{\n");
         output_cpp ("\tHX_MARK_BEGIN_CLASS(" ^ smart_class_name ^ ");\n");
         if (implement_dynamic) then
            output_cpp "\tHX_MARK_DYNAMIC;\n";
         List.iter (dump_field_iterator "HX_MARK_MEMBER_NAME") implemented_instance_fields;
         (match super_needs_iteration with
         | "" -> ()
         | super -> output_cpp ("\t" ^ super^"::__Mark(HX_MARK_ARG);\n" ) );
         output_cpp "\tHX_MARK_END_CLASS();\n";
         output_cpp "}\n\n";

         (* Visit function - explicitly visit all child pointers *)
         output_cpp ("void " ^ class_name ^ "::__Visit(HX_VISIT_PARAMS)\n{\n");
         if (implement_dynamic) then
            output_cpp "\tHX_VISIT_DYNAMIC;\n";
         List.iter (dump_field_iterator "HX_VISIT_MEMBER_NAME") implemented_instance_fields;
         (match super_needs_iteration with
         | "" -> ()
         | super -> output_cpp ("\t" ^ super ^ "::__Visit(HX_VISIT_ARG);\n") );
         output_cpp "}\n\n";
      end;


      let dump_quick_field_test fields =
         if ( (List.length fields) > 0) then begin
            let len = function (_,l,_) -> l in
            let sfields = List.sort (fun f1 f2 -> (len f1)-(len f2)) fields in
            let len_case = ref (-1) in
            output_cpp "\tswitch(inName.length) {\n";
            List.iter (fun (field,l,result) ->
               if (l <> !len_case) then begin
                  if (!len_case>=0) then output_cpp "\t\tbreak;\n";
                  output_cpp ("\tcase " ^ (string_of_int l) ^ ":\n");
                  len_case := l;
               end;
               output_cpp ("\t\tif (HX_FIELD_EQ(inName,\"" ^  (Ast.s_escape field)  ^ "\") ) { " ^ result ^ " }\n");
            ) sfields;
            output_cpp "\t}\n";
         end;
      in

      let checkPropCall field = if ( (has_meta_key class_def.cl_meta Meta.NativeProperty) ||
                                     (has_meta_key field.cf_meta Meta.NativeProperty) ||
                                     (Common.defined common_ctx Define.ForceNativeProperty) )
         then
            "inCallProp != hx::paccNever"
         else
            "inCallProp == hx::paccAlways"
      in

      let toCommon t f value =
         t ^ "( " ^ ( match cpp_type_of ctx f.cf_type with
           | TCppInst(t) as inst when (has_meta_key t.cl_meta Meta.StructAccess)
              -> "cpp::Struct< " ^ (tcpp_to_string inst) ^ " >( " ^ value ^ " )"
           | TCppStar(t,_) -> "cpp::Pointer<void *>( " ^ value ^ " )"
           | _ -> value
         ) ^  " )"
      in
      let toVal f value = toCommon "hx::Val" f value in
      let toDynamic f value = toCommon "" f value in


      if (has_get_member_field class_def) then begin
         (* Dynamic "Get" Field function - string version *)
         output_cpp ("hx::Val " ^ class_name ^ "::__Field(const ::String &inName,hx::PropertyAccess inCallProp)\n{\n");
         let get_field_dat = List.map (fun f ->
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_read = AccCall } when not (is_physical_field f) ->
                     "if (" ^ (checkPropCall f) ^ ") return " ^ (toVal f ((keyword_remap ("get_" ^ f.cf_name)) ^ "()" ) ) ^ ";"
               | Var { v_read = AccCall } -> "return " ^ (toVal f ((checkPropCall f) ^ " ? " ^ (keyword_remap ("get_" ^ f.cf_name)) ^ "() : " ^
                     ((keyword_remap f.cf_name) ^ (if (variable_field f) then "" else "_dyn()")) ) ) ^ ";"
               | _ -> "return " ^ (toVal f (((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()"))) ^ ";"
            ) ) )
         in
         dump_quick_field_test (get_field_dat reflect_member_readable);
         if (implement_dynamic) then
            output_cpp "\tHX_CHECK_DYNAMIC_GET_FIELD(inName);\n";
         output_cpp ("\treturn super::__Field(inName,inCallProp);\n}\n\n");

      end;

      if (has_get_static_field class_def) then begin
         output_cpp ("bool " ^ class_name ^ "::__GetStatic(const ::String &inName, Dynamic &outValue, hx::PropertyAccess inCallProp)\n{\n");
         let get_field_dat = List.map (fun f ->
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_read = AccCall } when not (is_physical_field f) ->
                     "if (" ^ (checkPropCall f) ^ ") { outValue = " ^ (toDynamic f (keyword_remap ("get_" ^ f.cf_name) ^ "()")) ^ "; return true; }"
               | Var { v_read = AccCall } -> "outValue = " ^ (toDynamic f ((checkPropCall f) ^ " ? " ^ (keyword_remap ("get_" ^ f.cf_name)) ^ "() : " ^
                     ((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()"))) ^ "; return true;";
               | _ when variable_field f -> "outValue = " ^ (toDynamic f (keyword_remap f.cf_name)) ^ "; return true;"
               | _ -> "outValue = " ^ ((native_field_name_remap true f) ^ "_dyn(); return true;")
               )
            ) )
         in
         dump_quick_field_test (get_field_dat reflect_static_readable);
         output_cpp ("\treturn false;\n}\n\n");
      end;

      let castable f =
         match cpp_type_of ctx f.cf_type with
           | TCppInst(t) as inst when (has_meta_key t.cl_meta Meta.StructAccess)
              -> "cpp::Struct< " ^ (tcpp_to_string inst) ^ " > "
           | TCppStar(t,_) -> "cpp::Pointer< " ^ ( tcpp_to_string t ) ^ " >"
           | _ -> ctx_type_string ctx f.cf_type
      in

      (* Dynamic "Set" Field function *)
      if (has_set_member_field class_def) then begin

         output_cpp ("hx::Val " ^ class_name ^ "::__SetField(const ::String &inName,const hx::Val &inValue,hx::PropertyAccess inCallProp)\n{\n");

         let set_field_dat = List.map (fun f ->
            let default_action = if is_gc_element ctx (cpp_type_of ctx f.cf_type) then
                  "_hx_set_" ^ (keyword_remap f.cf_name) ^ "(HX_CTX_GET,inValue.Cast< " ^ (castable f) ^ " >());" ^ " return inValue;"
               else
                  (keyword_remap f.cf_name) ^ "=inValue.Cast< " ^ (castable f) ^ " >();" ^ " return inValue;"
            in
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_write = AccCall } ->
                  let inVal =  "(inValue.Cast< " ^ (castable f) ^ " >())" in
                  let setter = keyword_remap ("set_" ^ f.cf_name) in
                  "if (" ^ (checkPropCall f) ^ ") return " ^ (toVal f (setter ^inVal) ) ^ ";" ^
                      ( if not (is_physical_field f) then "" else default_action )
               | _ -> default_action
               )
            )
         ) in

         dump_quick_field_test (set_field_dat reflect_write_member_variables);
         if (implement_dynamic) then begin
            output_cpp ("\ttry { return super::__SetField(inName,inValue,inCallProp); }\n");
            output_cpp ("\tcatch(Dynamic e) { HX_DYNAMIC_SET_FIELD(inName,inValue); }\n");
            output_cpp "\treturn inValue;\n}\n\n";
         end else
            output_cpp ("\treturn super::__SetField(inName,inValue,inCallProp);\n}\n\n");
      end;

      if (has_set_static_field class_def) then begin

         output_cpp ("bool " ^ class_name ^ "::__SetStatic(const ::String &inName,Dynamic &ioValue,hx::PropertyAccess inCallProp)\n{\n");

         let set_field_dat = List.map (fun f ->
            let default_action =
               (keyword_remap f.cf_name) ^ "=ioValue.Cast< " ^ (castable f) ^ " >(); return true;" in
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_write = AccCall } ->
                  let inVal = "(ioValue.Cast< " ^ (castable f) ^ " >())" in
                  let setter = keyword_remap ("set_" ^ f.cf_name) in
                  "if (" ^ (checkPropCall f) ^ ")  ioValue = " ^ (toDynamic f (setter ^ inVal) ) ^ ";"
                      ^ ( if not (is_physical_field f) then "" else " else " ^ default_action )
               | _ -> default_action
               )
            )
         ) in

         dump_quick_field_test (set_field_dat reflect_write_static_variables);
         output_cpp ("\treturn false;\n}\n\n");
      end;




      (* For getting a list of data members (eg, for serialization) *)
      if (has_get_fields class_def) then begin
         let append_field =
            (fun field -> output_cpp ("\toutFields->push(" ^( str field.cf_name )^ ");\n")) in
         let is_data_field field = (match follow field.cf_type with | TFun _ -> false | _ -> true) in

         output_cpp ("void " ^ class_name ^ "::__GetFields(Array< ::String> &outFields)\n{\n");
         List.iter append_field (List.filter is_data_field class_def.cl_ordered_fields);
         if (implement_dynamic) then
            output_cpp "\tHX_APPEND_DYNAMIC_FIELDS(outFields);\n";
         output_cpp "\tsuper::__GetFields(outFields);\n";
         output_cpp "};\n\n";
      end;

      let storage field = match type_string field.cf_type with
         | "bool" -> "hx::fsBool"
         | "int" -> "hx::fsInt"
         | "Float" -> "hx::fsFloat"
         | "::String" -> "hx::fsString"
         | str -> "hx::fsObject" ^ " /*" ^ str ^ "*/ "
         in
      let dump_member_storage = (fun field ->
         output_cpp ("\t{" ^ (storage field) ^ ",(int)offsetof(" ^ class_name ^"," ^ (keyword_remap field.cf_name) ^")," ^
            (str field.cf_name) ^ "},\n")
         )
      in
      let dump_static_storage = (fun field ->
         output_cpp ("\t{" ^ (storage field) ^ ",(void *) &" ^ class_name ^"::" ^ (keyword_remap field.cf_name) ^"," ^
            (str field.cf_name) ^ "},\n")
         )
      in

      output_cpp "#if HXCPP_SCRIPTABLE\n";

      let stored_fields = List.filter is_data_member implemented_instance_fields in
      if ( (List.length stored_fields) > 0) then begin
         output_cpp ("static hx::StorageInfo " ^ class_name ^ "_sMemberStorageInfo[] = {\n");
         List.iter dump_member_storage stored_fields;
         output_cpp "\t{ hx::fsUnknown, 0, null()}\n};\n";
      end else
         output_cpp ("static hx::StorageInfo *" ^ class_name ^ "_sMemberStorageInfo = 0;\n");

      let stored_statics = List.filter is_data_member implemented_fields in
      if ( (List.length stored_statics) > 0) then begin
         output_cpp ("static hx::StaticInfo " ^ class_name ^ "_sStaticStorageInfo[] = {\n");
         List.iter dump_static_storage stored_statics;
         output_cpp "\t{ hx::fsUnknown, 0, null()}\n};\n";
      end else
         output_cpp ("static hx::StaticInfo *" ^ class_name ^ "_sStaticStorageInfo = 0;\n");

      output_cpp "#endif\n\n";
   end; (* cl_interface *)

   let sMemberFields = if List.length reflective_members>0 then begin
      let memberFields = class_name ^ "_sMemberFields" in
      output_cpp ("static ::String " ^ memberFields ^ "[] = {\n");
      List.iter dump_field_name  reflective_members;
      output_cpp "\t::String(null()) };\n\n";
      memberFields
   end else
      "0 /* sMemberFields */";
   in

   let hasMarkFunc = (not nativeGen) && (List.exists is_data_member implemented_fields) in

   if (hasMarkFunc) then begin
      (* Mark static variables as used *)
      output_cpp ("static void " ^ class_name ^ "_sMarkStatics(HX_MARK_PARAMS) {\n");
      List.iter (fun field ->
         if (is_data_member field) then
            output_cpp ("\tHX_MARK_MEMBER_NAME(" ^ class_name ^ "::" ^ (keyword_remap field.cf_name) ^ ",\"" ^  field.cf_name ^ "\");\n") )
         implemented_fields;
      output_cpp "};\n\n";

      (* Visit static variables *)
      output_cpp "#ifdef HXCPP_VISIT_ALLOCS\n";
      output_cpp ("static void " ^ class_name ^ "_sVisitStatics(HX_VISIT_PARAMS) {\n");
      List.iter (fun field ->
         if (is_data_member field) then
            output_cpp ("\tHX_VISIT_MEMBER_NAME(" ^ class_name ^ "::" ^ (keyword_remap field.cf_name) ^ ",\"" ^  field.cf_name ^ "\");\n") )
         implemented_fields;
      output_cpp "};\n\n";
      output_cpp "#endif\n\n";
   end;

   let generate_script_function isStatic field scriptName callName =
      match follow field.cf_type  with
      | TFun (args,return_type) when not (is_data_member field) ->
         output_cpp ("\nstatic void CPPIA_CALL " ^ scriptName ^ "(hx::CppiaCtx *ctx) {\n");
         let ret = script_signature return_type false in
         if (ret<>"v") then output_cpp ("ctx->return" ^ (script_type return_type false) ^ "(");
         if class_def.cl_interface then begin
            output_cpp (class_name ^ "::" ^ callName ^ "(ctx->getThis()" ^ (if (List.length args) > 0 then "," else ""));
         end else if isStatic then
            output_cpp (class_name ^ "::" ^ callName ^ "(")
         else
            output_cpp ("((" ^  class_name ^ "*)ctx->getThis())->" ^  callName ^ "(");

         let (signature,_,_) = List.fold_left (fun (signature,sep,size) (_,opt,t) ->
            output_cpp (sep ^ "ctx->get" ^ (script_type t opt) ^ "(" ^ size ^ ")");
            (signature ^ (script_signature t opt ), ",", (size^"+sizeof(" ^ (script_size_type t opt) ^ ")") ) ) (ret,"","sizeof(void*)")  args
         in

         output_cpp ")";
         if (ret<>"v") then output_cpp (")");
         output_cpp (";\n}\n");
         signature;
      | _ -> ""
   in


   let newInteface = class_def.cl_interface in

   if (scriptable && not nativeGen) then begin
      let delegate = "this->" in
      let dump_script_field idx (field,f_args,return_t) =
         let args = ctx_tfun_arg_list ctx true f_args in
         let names = List.map (fun (n,_,_) -> keyword_remap n) f_args in
         let return_type = ctx_type_string ctx return_t in
         let ret = if (return_type="Void" || return_type="void") then " " else "return " in
         let name = keyword_remap field.cf_name in
         let vtable =  "__scriptVTable[" ^ (string_of_int (idx+1) ) ^ "] " in
         let args_varray = (List.fold_left (fun l n -> l ^ ".Add(" ^ n ^ ")") "Array<Dynamic>()" names) in

         output_cpp ("	" ^ return_type ^ " " ^ name ^ "( " ^ args ^ " ) {\n");
         if newInteface then begin
            output_cpp ("\t\thx::CppiaCtx *__ctx = hx::CppiaCtx::getCurrent();\n" );
            output_cpp ("\t\thx::AutoStack __as(__ctx);\n" );
            output_cpp ("\t\t__ctx->pushObject(this);\n" );
            List.iter (fun (name,opt, t ) ->
               output_cpp ("\t\t__ctx->push" ^ (script_type t opt) ^ "(" ^ (keyword_remap name) ^ ");\n" );
            ) f_args;
            let interfaceSlot = string_of_int( -(cpp_get_interface_slot ctx name) ) in
            output_cpp ("\t\t" ^ ret ^ "__ctx->run" ^ (script_type return_t false) ^ "(__GetScriptVTable()[" ^ interfaceSlot ^ "]);\n" );
            output_cpp "\t}\n";
         end else begin
            output_cpp ("\tif (" ^ vtable ^ ") {\n" );
            output_cpp ("\t\thx::CppiaCtx *__ctx = hx::CppiaCtx::getCurrent();\n" );
            output_cpp ("\t\thx::AutoStack __as(__ctx);\n" );
            output_cpp ("\t\t__ctx->pushObject(" ^ (if class_def.cl_interface then "mDelegate.mPtr" else "this" ) ^");\n" );
            List.iter (fun (name,opt, t ) ->
               output_cpp ("\t\t__ctx->push" ^ (script_type t opt) ^ "(" ^ (keyword_remap name) ^ ");\n" );
            ) f_args;
            output_cpp ("\t\t" ^ ret ^ "__ctx->run" ^ (script_type return_t false) ^ "(" ^ vtable ^ ");\n" );
            output_cpp ("\t}  else " ^ ret );


            if (class_def.cl_interface) then begin
               output_cpp (" " ^ delegate ^ "__Field(HX_CSTRING(\"" ^ field.cf_name ^ "\"), hx::paccNever)");
               if (List.length names <= 5) then
                  output_cpp ("->__run(" ^ (String.concat "," names) ^ ");")
               else
                  output_cpp ("->__Run(" ^ args_varray ^ ");");
            end else
               output_cpp (class_name ^ "::" ^ name ^ "(" ^ (String.concat "," names)^ ");");
            if (return_type<>"void") then
               output_cpp "return null();";
            output_cpp "}\n";
            if (class_def.cl_interface) && not dynamic_interface_closures then begin
               output_cpp ("	Dynamic " ^ name ^ "_dyn() { return mDelegate->__Field(HX_CSTRING(\"" ^ field.cf_name ^ "\"), hx::paccNever); }\n\n");

            end
         end
      in

      let new_sctipt_functions = if newInteface then
            all_virtual_functions class_def
         else
            current_virtual_functions class_def
      in
      let sctipt_name = class_name ^ "__scriptable" in

      if newInteface then begin
         output_cpp ("class " ^ sctipt_name ^ " : public hx::Object {\n" );
         output_cpp "public:\n";
      end else begin
         output_cpp ("class " ^ sctipt_name ^ " : public " ^ class_name ^ " {\n" );
         output_cpp ("   typedef "^sctipt_name ^" __ME;\n");
         output_cpp ("   typedef "^class_name ^" super;\n");
         let has_funky_toString = List.exists (fun f -> f.cf_name="toString") class_def.cl_ordered_statics  ||
                                 List.exists (fun f -> f.cf_name="toString" && field_arg_count f <> 0) class_def.cl_ordered_fields in
         let super_string = if has_funky_toString then class_name ^ "::super" else class_name in
         output_cpp ("   typedef "^ super_string ^" __superString;\n");
         if (class_def.cl_interface) then
            output_cpp ("   HX_DEFINE_SCRIPTABLE_INTERFACE\n")
         else begin
            output_cpp ("   HX_DEFINE_SCRIPTABLE(HX_ARR_LIST" ^ (string_of_int (List.length constructor_var_list) ) ^ ")\n");
            if (not implement_dynamic) then
               output_cpp "\tHX_DEFINE_SCRIPTABLE_DYNAMIC;\n";
         end;
      end;

      list_iteri dump_script_field functions;
      output_cpp ("};\n\n");

      let sigs = Hashtbl.create 0 in

      let static_functions = (List.filter (fun f-> not (is_data_member f) ) reflect_static_fields) in
      let all_script_functions = (List.map (fun (f,_,_)->f)  new_sctipt_functions) @ static_functions in

      if (List.length all_script_functions) > 0 then begin
         List.iter (fun (f,_,_) ->
            let s = generate_script_function false f ("__s_" ^f.cf_name) (keyword_remap f.cf_name) in
            Hashtbl.add sigs f.cf_name s
         ) new_sctipt_functions;

         let dump_script_static f =
            let s = generate_script_function true f ("__s_" ^f.cf_name) (keyword_remap f.cf_name) in
            Hashtbl.add sigs f.cf_name s
         in
         List.iter dump_script_static class_def.cl_ordered_statics;

         output_cpp "static hx::ScriptNamedFunction __scriptableFunctions[] = {\n";
         let dump_func f isStaticFlag =
            let s = try Hashtbl.find sigs f.cf_name with Not_found -> "v" in
            output_cpp ("  hx::ScriptNamedFunction(\"" ^ f.cf_name ^ "\",__s_" ^ f.cf_name ^ ",\"" ^ s ^ "\", " ^ isStaticFlag ^ " ),\n" )
         in
         List.iter (fun (f,_,_) -> dump_func f "false") new_sctipt_functions;
         List.iter (fun f -> dump_func f "true") static_functions;
         output_cpp "  hx::ScriptNamedFunction(0,0,0) };\n";
      end else
         output_cpp "static hx::ScriptNamedFunction *__scriptableFunctions = 0;\n";

      if newInteface then begin
         output_cpp ("\n\n" ^ class_name ^ " " ^ class_name ^ "_scriptable = {\n");
         List.iter (fun (f,args,return_type) ->
            let cast = cpp_tfun_signature ctx true args return_type in
            output_cpp ("\t" ^ cast ^ "&" ^ sctipt_name ^ "::" ^ (keyword_remap f.cf_name) ^ ",\n")
         ) new_sctipt_functions;
         output_cpp ("};\n");
      end;

   end;


   let class_name_text = join_class_path class_path "." in

   (* Initialise static in boot function ... *)
   if (not class_def.cl_interface && not nativeGen) then begin
      (* Remap the specialised "extern" classes back to the generic names *)
      output_cpp ("hx::Class " ^ class_name ^ "::__mClass;\n\n");
      if (scriptable) then begin
         (match class_def.cl_constructor with
            | Some field  ->
                  let signature = generate_script_function false field "__script_construct_func" "__construct" in
                  output_cpp ("hx::ScriptFunction " ^ class_name ^ "::__script_construct(__script_construct_func,\"" ^ signature ^ "\");\n");
            | _ ->
                  output_cpp ("hx::ScriptFunction " ^ class_name ^ "::__script_construct(0,0);\n");
         );
      end;

      let reflective_statics = List.filter (reflective class_def) implemented_fields in
      let sStaticFields = if List.length reflective_statics > 0 then begin
         output_cpp ("static ::String " ^ class_name ^ "_sStaticFields[] = {\n");
         List.iter dump_field_name  reflective_statics;
         output_cpp "\t::String(null())\n};\n\n";
         class_name ^ "_sStaticFields";
      end else
        "0 /* sStaticFields */"
      in

      output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
      output_cpp ("\t" ^ class_name ^ " _hx_dummy;\n");
      output_cpp ("\t" ^ class_name ^ "::_hx_vtable = *(void **)&_hx_dummy;\n");
      output_cpp ("\thx::Static(__mClass) = new hx::Class_obj();\n");
      output_cpp ("\t__mClass->mName = " ^  (str class_name_text)  ^ ";\n");
      output_cpp ("\t__mClass->mSuper = &super::__SGetClass();\n");
      output_cpp ("\t__mClass->mConstructEmpty = &__CreateEmpty;\n");
      output_cpp ("\t__mClass->mConstructArgs = &__Create;\n");
      output_cpp ("\t__mClass->mGetStaticField = &" ^ (
         if (has_get_static_field class_def) then class_name ^ "::__GetStatic;\n" else "hx::Class_obj::GetNoStaticField;\n" ));
      output_cpp ("\t__mClass->mSetStaticField = &" ^ (
         if (has_set_static_field class_def) then class_name ^ "::__SetStatic;\n" else "hx::Class_obj::SetNoStaticField;\n" ));
      if hasMarkFunc then
         output_cpp ("\t__mClass->mMarkFunc = " ^ class_name ^ "_sMarkStatics;\n");
      output_cpp ("\t__mClass->mStatics = hx::Class_obj::dupFunctions(" ^ sStaticFields ^ ");\n");
      output_cpp ("\t__mClass->mMembers = hx::Class_obj::dupFunctions(" ^ sMemberFields ^ ");\n");
      output_cpp ("\t__mClass->mCanCast = hx::TCanCast< " ^ class_name ^ " >;\n");
      if hasMarkFunc then
         output_cpp ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = " ^ class_name ^ "_sVisitStatics;\n#endif\n");
      output_cpp ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mMemberStorageInfo = " ^ class_name ^ "_sMemberStorageInfo;\n#endif\n");
      output_cpp ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mStaticStorageInfo = " ^ class_name ^ "_sStaticStorageInfo;\n#endif\n");
      output_cpp ("\thx::_hx_RegisterClass(__mClass->mName, __mClass);\n");
      if (scriptable) then
         output_cpp ("  HX_SCRIPTABLE_REGISTER_CLASS(\""^class_name_text^"\"," ^ class_name ^ ");\n");
      Hashtbl.iter (fun _ intf_def ->
          output_cpp ("\tHX_REGISTER_VTABLE_OFFSET( " ^ class_name ^ "," ^ (join_class_path_remap intf_def.cl_path "::")^ ");\n");
          ) native_implemented;
      output_cpp ("}\n\n");
   end else if not nativeGen then begin
      output_cpp ("hx::Class " ^ class_name ^ "::__mClass;\n\n");

      output_cpp ("void " ^ class_name ^ "::__register()\n{\n");

      output_cpp ("\thx::Static(__mClass) = new hx::Class_obj();\n");
      output_cpp ("\t__mClass->mName = " ^  (str class_name_text)  ^ ";\n");
      output_cpp ("\t__mClass->mSuper = &super::__SGetClass();\n");
      if hasMarkFunc then
         output_cpp ("\t__mClass->mMarkFunc = " ^ class_name ^ "_sMarkStatics;\n");
      output_cpp ("\t__mClass->mMembers = hx::Class_obj::dupFunctions(" ^ sMemberFields ^ ");\n");
      output_cpp ("\t__mClass->mCanCast = hx::TIsInterface< (int)" ^ (cpp_class_hash class_def)  ^ " >;\n");
      if hasMarkFunc then
         output_cpp ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = " ^ class_name ^ "_sVisitStatics;\n#endif\n");
      output_cpp ("\thx::_hx_RegisterClass(__mClass->mName, __mClass);\n");
      if (scriptable) then
         output_cpp ("  HX_SCRIPTABLE_REGISTER_INTERFACE(\""^class_name_text^"\"," ^ class_name ^ ");\n");
      output_cpp ("}\n\n");
   end;

   if (has_boot_field class_def) then begin
      output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");

      List.iter (gen_field_init ctx class_def ) (List.filter should_implement_field class_def.cl_ordered_statics);

      output_cpp ("}\n\n");
   end;


   gen_close_namespace output_cpp class_path;

   if class_def.cl_interface && has_meta_key class_def.cl_meta Meta.ObjcProtocol then begin
      let full_class_name =  ("::" ^ (join_class_path_remap class_path "::") ) ^ "_obj"  in
      let protocol = get_meta_string class_def.cl_meta Meta.ObjcProtocol in
      generate_protocol_delegate ctx class_def output_cpp;
      output_cpp ("id<" ^ protocol ^ "> " ^  full_class_name ^ "::_hx_toProtocol(Dynamic inImplementation) {\n");
      output_cpp ("\treturn [ [_hx_" ^ protocol ^ "_delegate alloc] initWithImplementation:inImplementation.mPtr];\n");
      output_cpp ("}\n\n");
   end;


   cpp_file#close;
 in
 (*
    Header code
 *)
 let generate_class_header () =
   let common_ctx = baseCtx.ctx_common in
   let class_path = class_def.cl_path in
   let nativeGen = has_meta_key class_def.cl_meta Meta.NativeGen in
   let class_name = (snd class_path) ^ (if nativeGen then "" else "_obj") in
   let smart_class_name =  (snd class_path)  in
   let scriptable = inScriptable && not class_def.cl_private in
   (*let cpp_file = new_cpp_file common_ctx.file class_path in*)
   let debug = if (has_meta_key class_def.cl_meta Meta.NoDebug) || ( Common.defined  baseCtx.ctx_common Define.NoDebug)
      then 0 else 1 in

   let h_file = new_header_file common_ctx common_ctx.file class_path in
   let ctx = file_context baseCtx h_file debug true in


   let parent,super = match class_def.cl_super with
      | Some (klass,params) ->
            let name = (tcpp_to_string_suffix "_obj" (cpp_instance_type ctx klass params) ) in
            (if class_def.cl_interface && nativeGen then "virtual " else "" ) ^ name, name
      | None when nativeGen && class_def.cl_interface  -> "virtual hx::NativeInterface", "hx::NativeInterface"
      | None when class_def.cl_interface -> "", "hx::Object"
      | None when nativeGen -> "", ""
      | None -> "hx::Object", "hx::Object"
      in
   let output_h = (h_file#write) in
   let def_string = join_class_path class_path "_"  in


   begin_header_file (h_file#write_h) def_string nativeGen;

   (* Include the real header file for the super class *)
   (match class_def.cl_super with
   | Some super ->
      let super_path = (fst super).cl_path in
      h_file#add_include super_path
   | _ -> () );

   (* And any interfaces ... *)
   List.iter (fun imp->
      let interface = fst imp in
      let include_file = get_meta_string_path interface.cl_meta Meta.Include in
      h_file#add_include (if include_file="" then interface.cl_path else path_of_string include_file) )
      (real_interfaces class_def.cl_implements);

   (* Only need to forward-declare classes that are mentioned in the header file
      (ie, not the implementation)  *)
   let header_referenced,header_flags = find_referenced_types_flags ctx (TClassDecl class_def) "*" super_deps (Hashtbl.create 0) true false scriptable in
   List.iter2 ( fun r f -> gen_forward_decl h_file r f ) header_referenced header_flags;
   output_h "\n";

   output_h ( get_class_code class_def Meta.HeaderCode );
   let inc = get_meta_string_path class_def.cl_meta Meta.HeaderInclude in
   if (inc<>"") then
      output_h ("#include \"" ^ inc ^ "\"\n");

   gen_open_namespace output_h class_path;
   output_h "\n\n";
   output_h ( get_class_code class_def Meta.HeaderNamespaceCode );

   let extern_class =  Common.defined common_ctx Define.DllExport in
   let attribs = "HXCPP_" ^ (if extern_class then "EXTERN_" else "") ^ "CLASS_ATTRIBUTES" in

   let dump_native_interfaces () =
      List.iter ( fun(c,params) ->
         output_h (" , public virtual " ^ (join_class_path c.cl_path "::") )
      ) (List.filter  (fun (t,_) -> is_native_gen_class t) class_def.cl_implements);
   in

   if (class_def.cl_interface && not nativeGen) then begin
      output_h ("class " ^ attribs ^ " " ^ class_name ^ " {\n");
      output_h "\tpublic:\n";
      output_h ("\t\ttypedef " ^ super ^ " super;\n");
   end else if (super="") then begin
      output_h ("class " ^ attribs ^ " " ^ class_name);
      dump_native_interfaces();
      output_h "\n{\n\tpublic:\n";
   end else begin
      output_h ("class " ^ attribs ^ " " ^ class_name ^ " : public " ^ parent );
      dump_native_interfaces();
      output_h "\n{\n\tpublic:\n";
      if not nativeGen then begin
         output_h ("\t\ttypedef " ^ super ^ " super;\n");
         output_h ("\t\ttypedef " ^ class_name ^ " OBJ_;\n");
      end
   end;



   if (not class_def.cl_interface && not nativeGen) then begin
      output_h ("\t\t" ^ class_name ^  "();\n");
      output_h "\n\tpublic:\n";
      output_h ("\t\tenum { _hx_ClassId = " ^ classIdTxt ^ " };\n\n");
      output_h ("\t\tvoid __construct(" ^ constructor_type_args ^ ");\n");
      output_h ("\t\tinline void *operator new(size_t inSize, bool inContainer=" ^ isContainer ^",const char *inName=" ^ gcName ^ ")\n" );
      output_h ("\t\t\t{ return hx::Object::operator new(inSize,inContainer,inName); }\n" );
      output_h ("\t\tinline void *operator new(size_t inSize, int extra)\n" );
      output_h ("\t\t\t{ return hx::Object::operator new(inSize+extra," ^ isContainer ^ "," ^ gcName ^ "); }\n" );
      if inlineContructor then begin
         output_h "\n";
         outputConstructor ctx (fun str -> output_h ("\t\t" ^ str) ) true
      end else begin
         output_h ("\t\tstatic " ^ptr_name^ " __new(" ^constructor_type_args ^");\n");
         output_h ("\t\tstatic " ^ptr_name^ " __alloc(hx::Ctx *_hx_ctx" ^ (if constructor_type_args="" then "" else "," ^constructor_type_args)  ^");\n");
      end;
      output_h ("\t\tstatic void * _hx_vtable;\n");
      output_h ("\t\tstatic Dynamic __CreateEmpty();\n");
      output_h ("\t\tstatic Dynamic __Create(hx::DynamicArray inArgs);\n");
      if (List.length dynamic_functions > 0) then
         output_h ("\t\tstatic void __alloc_dynamic_functions(hx::Ctx *_hx_alloc," ^ class_name ^ " *_hx_obj);\n");
      if (scriptable) then
         output_h ("\t\tstatic hx::ScriptFunction __script_construct;\n");
      output_h ("\t\t//~" ^ class_name ^ "();\n\n");
      output_h ("\t\tHX_DO_RTTI_ALL;\n");
      if (has_get_member_field class_def) then
         output_h ("\t\thx::Val __Field(const ::String &inString, hx::PropertyAccess inCallProp);\n");
      if (has_get_static_field class_def) then
         output_h ("\t\tstatic bool __GetStatic(const ::String &inString, Dynamic &outValue, hx::PropertyAccess inCallProp);\n");
      if (has_set_member_field class_def) then
         output_h ("\t\thx::Val __SetField(const ::String &inString,const hx::Val &inValue, hx::PropertyAccess inCallProp);\n");
      if (has_set_static_field class_def) then
         output_h ("\t\tstatic bool __SetStatic(const ::String &inString, Dynamic &ioValue, hx::PropertyAccess inCallProp);\n");
      if (has_get_fields class_def) then
         output_h ("\t\tvoid __GetFields(Array< ::String> &outFields);\n");

      if (implement_dynamic) then
         output_h ("\t\tHX_DECLARE_IMPLEMENT_DYNAMIC;\n");
      output_h ("\t\tstatic void __register();\n");
      if (override_iteration) then begin
         output_h ("\t\tvoid __Mark(HX_MARK_PARAMS);\n");
         output_h ("\t\tvoid __Visit(HX_VISIT_PARAMS);\n");
      end;

      if (implementsNative) then begin
         output_h ("\n\t\tHX_NATIVE_IMPLEMENTATION\n");
         List.iter (fun field ->
            match follow field.cf_type, field.cf_kind with
            | _, Method MethDynamic  -> ()
            | TFun (args,return_type), _  ->
                let retVal = ctx_type_string ctx return_type in
                let ret = if retVal="void" then "" else "return " in
                let name = keyword_remap field.cf_name in
                let argNames = List.map (fun (name,_,_) -> keyword_remap name ) args in
                output_h ( "\t\t" ^ retVal ^" " ^ name ^ "( " ^ ctx_tfun_arg_list ctx true args ^ ") {\n");
                output_h ( "\t\t\t" ^ ret ^ "super::" ^ name ^ "( " ^ (String.concat "," argNames) ^ ");\n\t\t}\n");
            | _ -> ()
            ) neededInterfaceFunctions;
         output_h ("\n");
      end;

      output_h ("\t\tbool _hx_isInstanceOf(int inClassId);\n");
      if ( (List.length implemented) > 0 ) then begin
         output_h "\t\tvoid *_hx_getInterface(int inHash);\n";
         output_h (String.concat "\n" !header_glue);
      end;


      if (has_init_field class_def) then
         output_h "\t\tstatic void __init__();\n\n";
      output_h ("\t\t::String __ToString() const { return " ^ (str smart_class_name) ^ "; }\n\n");
   end else if not nativeGen then begin
      output_h ("\t\tHX_DO_INTERFACE_RTTI;\n\n");
   end else begin
      (* native interface *) ( )
   end;

   if (has_boot_field class_def) then
      output_h ("\t\tstatic void __boot();\n");


   (match class_def.cl_array_access with
   | Some t -> output_h ("\t\ttypedef " ^ (type_string t) ^ " __array_access;\n")
   | _ -> ());


   List.iter (gen_member_def ctx class_def true class_def.cl_interface) (List.filter should_implement_field class_def.cl_ordered_statics);

   if class_def.cl_interface then begin
      List.iter (fun (field,_,_) -> gen_member_def ctx class_def false true field) functions;
   end else begin
      List.iter (gen_member_def ctx class_def false false) (List.filter should_implement_field class_def.cl_ordered_fields);
   end;

   if class_def.cl_interface && has_meta_key class_def.cl_meta Meta.ObjcProtocol then begin
      let protocol = get_meta_string class_def.cl_meta Meta.ObjcProtocol in
      output_h ("\t\tstatic id<" ^ protocol ^ "> _hx_toProtocol(Dynamic inImplementation);\n");
   end;


   output_h ( get_class_code class_def Meta.HeaderClassCode );
   output_h "};\n\n";

   gen_close_namespace output_h class_path;

   end_header_file output_h def_string;
   h_file#close;

  in

  (* create header and cpp files *)
  if not (nativeGen && class_def.cl_interface) then
     generate_class_cpp ();
  generate_class_header ()
;;

let generate_class_deps ctx class_def super_deps constructor_deps =
   find_referenced_types ctx (TClassDecl class_def) super_deps constructor_deps false true false
;;




let write_resources common_ctx =

   let idx = ref 0 in

   Hashtbl.iter (fun _ data ->
      let id = "__res_" ^ (string_of_int !idx) in
      let resource_file = new_cpp_file common_ctx common_ctx.file (["resources"],id) in
      resource_file#write "namespace hx {\n";
      resource_file#write_i ("unsigned char " ^ id ^ "[] = {\n");
      resource_file#write_i "0xff, 0xff, 0xff, 0xff,\n";
      for i = 0 to String.length data - 1 do
      let code = Char.code (String.unsafe_get data i) in
         resource_file#write  (Printf.sprintf "%d," code);
         if ( (i mod 10) = 9) then resource_file#write "\n";
      done;
      resource_file#write ("0x00 };\n");
      incr idx;
      resource_file#write ("}\n");
      resource_file#close;
   ) common_ctx.resources;


   let resource_file = new_cpp_file common_ctx common_ctx.file ([],"__resources__") in
   resource_file#write_h "#include <hxcpp.h>\n\n";
   resource_file#write "namespace hx {\n";

   idx := 0;
   Hashtbl.iter (fun _ data ->
      let id = "__res_" ^ (string_of_int !idx) in
      resource_file#write_i ("extern unsigned char " ^ id ^ "[];\n");
      incr idx;
   ) common_ctx.resources;

   resource_file#write "}\n\n";

   idx := 0;
   resource_file#write "hx::Resource __Resources[] = ";
   resource_file#begin_block;
   Hashtbl.iter (fun name data ->
      let id = "__res_" ^ (string_of_int !idx) in
      resource_file#write_i
         ("{ " ^ (str name) ^ "," ^ (string_of_int (String.length data)) ^ "," ^
            "hx::" ^ id ^ " + 4 },\n");
      incr idx;
   ) common_ctx.resources;

   resource_file#write_i "{ ::String(null()),0,0 }\n";
   resource_file#end_block_line;
   resource_file#write ";\n\n";
   resource_file#write "namespace hx { Resource *GetResources() { return __Resources; } }\n";
   resource_file#close;;



let write_build_data common_ctx filename classes main_deps boot_deps build_extra extern_src exe_name =
   let buildfile = open_out filename in
   let include_prefix = get_include_prefix common_ctx true in
   let add_class_to_buildfile class_path deps fileXml =
      let cpp = (join_class_path class_path "/") ^ (source_file_extension common_ctx) in
      output_string buildfile ( "  <file name=\"src/" ^ cpp ^ "\" ");
      if fileXml<>"" then output_string buildfile fileXml;
      output_string buildfile (">\n" );
      let project_deps = List.filter (fun path -> not (is_internal_class path) ) deps in
      List.iter (fun path-> output_string buildfile ("   <depend name=\"" ^
      ( match path with
         | (["@verbatim"],file) -> file
         | _ -> "include/" ^ include_prefix ^ (join_class_path path "/") ^ ".h" )
      ^ "\"/>\n") ) project_deps;
      output_string buildfile ( "  </file>\n" )
   in
   let add_classdef_to_buildfile (class_path, deps, object_def )  =
      let fileXml = match object_def with
         | TClassDecl class_def -> get_meta_string class_def.cl_meta Meta.FileXml
         | TEnumDecl enum_def -> get_meta_string enum_def.e_meta Meta.FileXml
         | _ -> ""
      in
      add_class_to_buildfile class_path deps fileXml
   in

   output_string buildfile "<xml>\n";
   output_string buildfile ("<set name=\"HXCPP_API_LEVEL\" value=\"" ^
            (Common.defined_value common_ctx Define.HxcppApiLevel) ^ "\" />\n");
   output_string buildfile "<files id=\"haxe\">\n";
   output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
   List.iter add_classdef_to_buildfile classes;
   add_class_to_buildfile ( [] , "__boot__")  boot_deps "";
   add_class_to_buildfile ( [] , "__files__")  [] "if='HXCPP_DEBUGGER'";
   output_string buildfile ("   <file name=\"${HXCPP}/src/hx/NoFiles.cpp\" unless=\"HXCPP_DEBUGGER\" />\n");
   add_class_to_buildfile ( [] , "__resources__")  [] "";
   output_string buildfile "</files>\n";
   output_string buildfile "<files id=\"__lib__\">\n";
   output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
   add_class_to_buildfile ( [] , "__lib__") main_deps "";
   output_string buildfile "</files>\n";
   output_string buildfile "<files id=\"__main__\">\n";
   output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
   add_class_to_buildfile  ( [] , "__main__") main_deps "";
   output_string buildfile "</files>\n";
   output_string buildfile "<files id=\"__resources__\">\n";
   let idx = ref 0 in
   let ext = source_file_extension common_ctx in
   Hashtbl.iter (fun _ data ->
      let id = "__res_" ^ (string_of_int !idx) in
      output_string buildfile ("<file name=\"src/resources/" ^ id ^ ext ^ "\" tags=\"optim-none\" />\n");
      incr idx;
   ) common_ctx.resources;
   output_string buildfile "</files>\n";
   output_string buildfile "<files id=\"__externs__\">\n";
   List.iter (fun src -> output_string buildfile ("<file name=\"" ^src^ "\" />\n") ) extern_src;
   output_string buildfile "</files>\n";
   output_string buildfile ("<set name=\"HAXE_OUTPUT\" value=\"" ^ exe_name ^ "\" />\n");
   output_string buildfile "<include name=\"${HXCPP}/build-tool/BuildCommon.xml\"/>\n";
   output_string buildfile build_extra;
   output_string buildfile "</xml>\n";
   close_out buildfile;;

let write_build_options common_ctx filename defines =
   let writer = cached_source_writer common_ctx filename in
   PMap.iter ( fun name value -> match name with
      | "true" | "sys" | "dce" | "cpp" | "debug" -> ()
      | _ ->  writer#write (name ^ "="^(escape_command value)^ "\n" ) ) defines;
   let cmd = Unix.open_process_in "haxelib path hxcpp" in
   writer#write ("hxcpp=" ^ (Pervasives.input_line cmd));
   Pervasives.ignore (Unix.close_process_in cmd);
   writer#close;;

let create_member_types common_ctx =
   let result = Hashtbl.create 0 in
      List.iter (fun object_def ->
         (match object_def with
         | TClassDecl class_def when not class_def.cl_interface ->
            let rec add_override to_super =
               let class_name = (join_class_path to_super.cl_path ".") in
               List.iter (fun member -> Hashtbl.add result (class_name ^ "." ^ member.cf_name) "virtual " ) class_def.cl_ordered_fields;
               match to_super.cl_super with
               | Some super -> add_override (fst super)
               | _ -> ()
             in
             (match  class_def.cl_super with Some super -> add_override (fst super) | _->())
         | _ -> ()
         ) ) common_ctx.types;
   result;;

(* Builds inheritance tree, so header files can include parents defs.  *)
let create_super_dependencies common_ctx =
   let result = Hashtbl.create 0 in
   List.iter (fun object_def ->
      (match object_def with
      | TClassDecl class_def when not class_def.cl_extern ->
         let deps = ref [] in
         (match class_def.cl_super with Some super ->
            if not (fst super).cl_extern then
               deps := ((fst super).cl_path) :: !deps
         | _ ->() );
         List.iter (fun imp -> if not (fst imp).cl_extern then deps := (fst imp).cl_path :: !deps) (real_non_native_interfaces class_def.cl_implements);
         Hashtbl.add result class_def.cl_path !deps;
      | TEnumDecl enum_def when not enum_def.e_extern ->
         Hashtbl.add result enum_def.e_path [];
      | _ -> () );
      ) common_ctx.types;
   result;;

let create_constructor_dependencies common_ctx =
   let result = Hashtbl.create 0 in
   List.iter (fun object_def ->
      (match object_def with
      | TClassDecl class_def when not class_def.cl_extern ->
         (match class_def.cl_constructor with
         | Some func_def -> Hashtbl.add result class_def.cl_path func_def
         | _ -> () )
      | _ -> () );
      ) common_ctx.types;
   result;;


let is_assign_op op =
   match op with
   | OpAssign
   | OpAssignOp _ -> true
   | _ -> false
;;





(*
  ------------------ CPPIA ----------------------------------------
*)






let rec script_type_string haxe_type =
   match haxe_type with
   | TAbstract ({ a_path = ([],"Null") },[t]) ->
      (match follow t with
      | TAbstract ({ a_path = [],"Int" },_)
      | TAbstract ({ a_path = [],"Float" },_)
      | TAbstract ({ a_path = [],"Bool" },_) -> "Dynamic"
      | _ -> script_type_string t)
   | TInst ({cl_path=[],"Null"},[t]) ->
      (match follow t with
      | TAbstract ({ a_path = [],"Int" },_)
      | TAbstract ({ a_path = [],"Float" },_)
      | TAbstract ({ a_path = [],"Bool" },_) -> "Dynamic"
      | _ -> script_type_string t )
   | _ ->
      match follow haxe_type with
      | TType ({t_path = [],"Array"},params) -> "Array"
      | TInst ({cl_path=[],"Array"},params) ->
         (match params with
         | [t] ->
            (match type_string_suff "" t false with
            | "int" -> "Array.int"
            | "Float" -> "Array.Float"
            | "bool" -> "Array.bool"
            | "::String" -> "Array.String"
            | "unsigned char" -> "Array.unsigned char"
            | "::cpp::UInt8" -> "Array.unsigned char"
            | "Dynamic" -> "Array.Any"
            | _ -> "Array.Object"
            )
         | _ -> "Array.Object"
         )
     | TAbstract (abs,pl) when abs.a_impl <> None ->
         script_type_string  (Abstract.get_underlying_type abs pl);
     | _ ->
         type_string_suff "" haxe_type false
;;


let rec script_cpptype_string cppType = match cppType with
   | TCppDynamic
   | TCppUnchanged
   | TCppWrapped _
   | TCppObject -> "Dynamic"
   | TCppObjectPtr -> ".*.hx.Object*"
   | TCppReference t -> ".ref." ^ (script_cpptype_string t)
   | TCppStruct t -> ".struct." ^ (script_cpptype_string t)
   | TCppStar(t,_) -> "*." ^ (script_cpptype_string t)
   | TCppVoid -> "void"
   | TCppVoidStar -> "*.void"
   | TCppRest _ -> "vaarg_list"
   | TCppVarArg -> "vararg"
   | TCppAutoCast -> ".cpp.AutoCast"
   | TCppVariant -> ".cpp.Variant"
   | TCppEnum(enum) -> (join_class_path enum.e_path ".")
   | TCppScalar(scalar) -> scalar
   | TCppString -> "String"
   | TCppFastIterator it -> "cpp.FastIterator." ^ (script_cpptype_string it)
   | TCppPointer(_,valueType) -> "cpp.Pointer." ^ (script_cpptype_string valueType)
   | TCppRawPointer(_,valueType) -> "cpp.RawPointer." ^ (script_cpptype_string valueType)
   | TCppFunction _ -> "cpp.Function"
   | TCppObjCBlock _ -> "cpp.ObjCBlock"
   | TCppDynamicArray -> "Array.Any"
   | TCppObjectArray _ -> "Array.Object"
   | TCppScalarArray(value) -> "Array." ^ (script_cpptype_string value)
   | TCppObjC _ -> "cpp.ObjC"
   | TCppProtocol _ -> "cpp.ObjC.Protocol"
   | TCppNativePointer klass -> "cpp.Pointer." ^ (join_class_path klass.cl_path ".")
   | TCppInterface klass -> (join_class_path klass.cl_path ".")
   | TCppInst klass -> (join_class_path klass.cl_path ".")
   | TCppClass -> "Class"
   | TCppGlobal -> "?global";
   | TCppNull -> "null";
   | TCppCode _ -> "Dynamic"
;;


type array_of =
   | ArrayInterface of int
   | ArrayData of string
   | ArrayObject
   | ArrayAny
   | ArrayNone
;;

let is_template_type t =
   false
;;

type cppia_op =
	| IaFunction
	| IaVar
	| IaToInterface
	| IaToDynArray
	| IaToDataArray
	| IaToInterfaceArray
	| IaFun
	| IaCast
	| IaTCast
	| IaBlock
	| IaBreak
	| IaContinue
	| IaIsNull
	| IaNotNull
	| IaSet
	| IaCall
	| IaCallGlobal
	| IaCallStatic
	| IaCallMember
	| IaCallSuper
	| IaCallThis
	| IaCallSuperNew
	| IaCreateEnum
	| IaADef
	| IaIf
	| IaIfElse
	| IaFStatic
	| IaFName
	| IaFThisInst
	| IaFLink
	| IaFThisName
	| IaFEnum
	| IaThrow
	| IaArrayI
	| IaPlusPlus
	| IaPlusPlusPost
	| IaMinusMinus
	| IaMinusMinusPost
	| IaNeg
	| IaBitNot
	| IaLogicNot
	| IaTVars
	| IaVarDecl
	| IaVarDeclI
	| IaNew
	| IaReturn
	| IaRetVal
	| IaPosInfo
	| IaObjDef
	| IaClassOf
	| IaWhile
	| IaFor
	| IaEnumI
	| IaSwitch
	| IaTry
	| IaImplDynamic
   | IaConstInt
   | IaConstFloat
   | IaConstString
   | IaConstFalse
   | IaConstTrue
   | IaConstNull
   | IaConsThis
   | IaConstSuper
   | IaCastInt
   | IaCastBool
   | IaInterface
   | IaClass
   | IaAccessNormal
   | IaAccessNot
   | IaAccessResolve
   | IaAccessCall
   | IaEnum
   | IaInline
   | IaMain
   | IaNoMain
   | IaResources
   | IaReso
   | IaNoCast
   | IaAccessCallNative

	| IaBinOp of Ast.binop
;;

let cppia_op_info = function
	| IaFunction -> ("FUNCTION", 1)
	| IaVar      -> ("VAR", 2)
	| IaToInterface -> ("TOINTERFACE", 3)
	| IaToDynArray -> ("TODYNARRAY", 4)
	| IaToDataArray -> ("TODATAARRAY", 5)
	| IaToInterfaceArray -> ("TOINTERFACEARRAY", 6)
	| IaFun -> ("FUN", 7)
	| IaCast -> ("CAST", 8)
	| IaBlock -> ("BLOCK", 9)
	| IaBreak -> ("BREAK", 10)
	| IaContinue -> ("CONTINUE", 11)
	| IaIsNull -> ("ISNULL", 12)
	| IaNotNull -> ("NOTNULL", 13)
	| IaSet -> ("SET", 14)
	| IaCall -> ("CALL", 15)
	| IaCallGlobal -> ("CALLGLOBAL", 16)
	| IaCallStatic -> ("CALLSTATIC", 17)
	| IaCallMember -> ("CALLMEMBER", 18)
	| IaCallSuper -> ("CALLSUPER", 19)
	| IaCallThis -> ("CALLTHIS", 20)
	| IaCallSuperNew -> ("CALLSUPERNEW", 21)
	| IaCreateEnum -> ("CREATEENUM", 22)
	| IaADef -> ("ADEF", 23)
	| IaIf -> ("IF", 24)
	| IaIfElse -> ("IFELSE", 25)
	| IaFName -> ("FNAME", 27)
	| IaFStatic -> ("FSTATIC", 28)
	| IaFThisInst -> ("FTHISINST", 29)
	| IaFLink -> ("FLINK", 30)
	| IaFThisName -> ("FTHISNAME", 31)
	| IaFEnum -> ("FENUM", 32)
	| IaThrow -> ("THROW", 33)
	| IaArrayI -> ("ARRAYI", 34)
	| IaPlusPlus -> ("++", 35)
	| IaPlusPlusPost -> ("+++", 36)
	| IaMinusMinus -> ("--", 37)
	| IaMinusMinusPost -> ("---", 38)
	| IaNeg -> ("NEG", 39)
	| IaBitNot -> ("~", 40)
	| IaLogicNot -> ("!", 41)
	| IaTVars -> ("TVARS", 42)
	| IaVarDecl -> ("VARDECL", 43)
	| IaVarDeclI -> ("VARDECLI", 44)
	| IaNew -> ("NEW", 45)
	| IaReturn -> ("RETURN", 46)
	| IaRetVal -> ("RETVAL", 47)
	| IaPosInfo -> ("POSINFO", 48)
	| IaObjDef -> ("OBJDEF", 49)
	| IaClassOf -> ("CLASSOF", 50)
	| IaWhile -> ("WHILE", 51)
	| IaFor -> ("FOR", 52)
	| IaEnumI -> ("ENUMI", 53)
	| IaSwitch -> ("SWITCH", 54)
	| IaTry -> ("TRY", 55)
	| IaImplDynamic -> ("IMPLDYNAMIC", 56)
   | IaConstInt -> ("i", 57)
   | IaConstFloat -> ("f", 58)
   | IaConstString -> ("s", 59)
   | IaConstFalse -> ("false", 60)
   | IaConstTrue -> ("true", 61)
   | IaConstNull -> ("NULL", 62)
   | IaConsThis -> ("THIS", 63)
   | IaConstSuper -> ("SUPER", 64)
   | IaCastInt -> ("CASTINT", 65)
   | IaCastBool -> ("CASTBOOL", 66)
   | IaInterface -> ("INTERFACE", 67)
   | IaClass -> ("CLASS", 68)
   | IaAccessNormal -> ("N", 69)
   | IaAccessNot  -> ("n", 70)
   | IaAccessResolve  -> ("R", 71)
   | IaAccessCall -> ("C", 72)
   | IaEnum -> ("ENUM", 73)
   | IaInline -> ("INLINE", 74)
   | IaMain -> ("MAIN", 75)
   | IaNoMain -> ("NOMAIN", 76)
   | IaResources -> ("RESOURCES", 77)
   | IaReso -> ("RESO", 78)
	| IaNoCast -> ("NOCAST", 79)
   | IaAccessCallNative -> ("V", 80)

	| IaBinOp OpAdd -> ("+", 101)
	| IaBinOp OpMult -> ("*", 102)
	| IaBinOp OpDiv -> ("/", 103)
	| IaBinOp OpSub -> ("-", 104)
	| IaBinOp OpAssign -> ("=", 105)
	| IaBinOp OpEq -> ("==", 106)
	| IaBinOp OpNotEq -> ("!=", 107)
	| IaBinOp OpGte -> (">=", 108)
	| IaBinOp OpLte -> ("<=", 109)
	| IaBinOp OpGt -> (">", 110)
	| IaBinOp OpLt -> ("<", 111)
	| IaBinOp OpAnd -> ("&", 112)
	| IaBinOp OpOr -> ("|", 113)
	| IaBinOp OpXor -> ("^", 114)
	| IaBinOp OpBoolAnd -> ("&&", 115)
	| IaBinOp OpBoolOr -> ("||", 116)
	| IaBinOp OpShr -> (">>", 117)
	| IaBinOp OpUShr -> (">>>", 118)
	| IaBinOp OpShl -> ("<<", 119)
	| IaBinOp OpMod -> ("%", 120)
	| IaBinOp OpInterval -> ("...", 121)
	| IaBinOp OpArrow -> ("=>", 122)
	| IaBinOp OpIn -> (" in ", 123)
	| IaBinOp OpAssignOp OpAdd -> ("+=", 201)
	| IaBinOp OpAssignOp OpMult -> ("*=", 202)
	| IaBinOp OpAssignOp OpDiv -> ("/=", 203)
	| IaBinOp OpAssignOp OpSub -> ("-=", 204)


	| IaBinOp OpAssignOp OpAnd -> ("&=", 212)
	| IaBinOp OpAssignOp OpOr -> ("|=", 213)
	| IaBinOp OpAssignOp OpXor -> ("^=", 214)
	| IaBinOp OpAssignOp OpBoolAnd -> ("&&=", 215)
	| IaBinOp OpAssignOp OpBoolOr -> ("||=", 216)
	| IaBinOp OpAssignOp OpShr -> (">>=", 217)
	| IaBinOp OpAssignOp OpUShr -> (">>>=", 218)
	| IaBinOp OpAssignOp OpShl -> ("<<=", 219)
	| IaBinOp OpAssignOp OpMod -> ("%=", 220)

	| IaBinOp OpAssignOp OpIn
	| IaBinOp OpAssignOp OpInterval
	| IaBinOp OpAssignOp OpAssign
	| IaBinOp OpAssignOp OpEq
	| IaBinOp OpAssignOp OpNotEq
	| IaBinOp OpAssignOp OpGte
	| IaBinOp OpAssignOp OpLte
	| IaBinOp OpAssignOp OpGt
	| IaBinOp OpAssignOp OpLt
	| IaBinOp OpAssignOp OpAssignOp _
	| IaBinOp OpAssignOp OpArrow -> assert false
	| IaTCast -> ("TCAST", 221)
;;



class script_writer ctx filename asciiOut =
   object(this)
   val debug = asciiOut
   val doComment = asciiOut && (Common.defined ctx.ctx_common Define.AnnotateSource)
   val indent_str = if asciiOut then "\t" else ""
   val mutable indent = ""
   val mutable indents = []
   val mutable just_finished_block = false
   val mutable classCount = 0
   val mutable return_type = TMono(ref None)
   val buffer = Buffer.create 0
   val identTable = Hashtbl.create 0
   val fileTable = Hashtbl.create 0
   val identBuffer = Buffer.create 0
   val cppiaAst = not (Common.defined ctx.ctx_common Define.NoCppiaAst)

   method stringId name =
      try ( Hashtbl.find identTable name )
      with Not_found -> begin
         let size = Hashtbl.length identTable in
         Hashtbl.add identTable name size;
         Buffer.add_string identBuffer ((string_of_int (String.length name)) ^ " " ^ name ^ "\n");
         size;
      end
   method incClasses = classCount <- classCount +1

   method stringText name = (string_of_int (this#stringId name)) ^ " "
   val typeTable = Hashtbl.create 0
   val typeBuffer = Buffer.create 0
   method typeId name =
      let name = if name="::hx::Class" then "::Class" else name in
      try ( Hashtbl.find typeTable name )
      with Not_found -> begin
         let size = Hashtbl.length typeTable in
         Hashtbl.add typeTable name size;
         Buffer.add_string typeBuffer ((string_of_int (String.length name)) ^ " " ^ name ^ "\n");
         size;
      end
   method write str = if asciiOut then
          Buffer.add_string buffer str
      else begin
         let push i = Buffer.add_char buffer (Char.chr i) in
         let pushI32 i = push (Int32.to_int (Int32.logand i (Int32.of_int 255))) in
         List.iter (fun i ->
            if ((Int32.compare i Int32.zero) >= 0) && ((Int32.compare i (Int32.of_int 254)) < 0) then
               pushI32 i
            else if ((Int32.compare i Int32.zero) >= 0) && ((Int32.compare i (Int32.of_int 65536)) < 0) then begin
               push 254;
               pushI32 i;
               pushI32 (Int32.shift_right i 8);
            end else begin
               push 255;
               pushI32 i;
               pushI32 (Int32.shift_right i 8);
               pushI32 (Int32.shift_right i 16);
               pushI32 (Int32.shift_right i 24);
            end
         ) (List.map Int32.of_string (Str.split (Str.regexp "[\n\t ]+") str) );
      end;
      just_finished_block <- false
   method comment text = if doComment then this#write ("# " ^ text ^ "\n")
   method commentOf text = if doComment then " # " ^ text else ""
   method typeTextString typeName = (string_of_int (this#typeId typeName)) ^ " "
   method typeText typeT =
      let tname = if cppiaAst then script_cpptype_string (cpp_type_of ctx typeT) else script_type_string typeT in
      (string_of_int (this#typeId tname)) ^ " "
   method astType cppType = (string_of_int (this#typeId (script_cpptype_string cppType))) ^ " "
   method writeType typeT = this#write (this#typeText typeT)
   method toCppType etype = (string_of_int (this#typeId (script_cpptype_string (cpp_type_of ctx etype) ))) ^ " "
   method boolText value = if value then "1" else "0"
   method writeBool value = this#write (if value then "1 " else "0 ")
   method staticText value = if value then "1" else "0"
   method writeData str = Buffer.add_string buffer str;
   method wint ival = this#write ((string_of_int ival)^" ")
   method ident name = this#wint (this#stringId name)
   method cppInstText clazz = match clazz.cl_path with
      | ([],"Array") -> this#typeTextString "Array"
      | x -> this#typeTextString (join_class_path x ".")
   method instText clazz = match clazz.cl_path with
      | ([],"Array") -> string_of_int (this#typeId "Array< ::Dynamic >") ^ " "
      | _ -> this#typeText (TInst(clazz,[]))
   method instName clazz = this#write (if cppiaAst then (this#cppInstText clazz) else (this#instText clazz))
   method enumText e = this#typeText (TEnum(e,[]))
   method close =
      let out_file = open_out_bin filename in
      output_string out_file (if asciiOut then "CPPIA\n" else "CPPIB\n");
      let idents =  Buffer.contents identBuffer in
      output_string out_file ((string_of_int (Hashtbl.length identTable)) ^ "\n");
      output_string out_file idents;
      let types =  Buffer.contents typeBuffer in
      output_string out_file ((string_of_int (Hashtbl.length typeTable)) ^ "\n");
      output_string out_file types;
      output_string out_file ( (string_of_int classCount) ^ "\n" );
      let contents = Buffer.contents buffer in
      output_string out_file contents;
      close_out out_file
   method fileId file =
      try ( Hashtbl.find fileTable file )
      with Not_found -> begin
         let stripped_file = strip_file ctx.ctx_common file in
         let result = this#stringId stripped_file in
         Hashtbl.add fileTable file result;
         result;
      end
   method constText c = match c with
   | TInt i -> (this#op IaConstInt) ^ (Printf.sprintf "%ld " i)
   | TFloat f -> (this#op IaConstFloat) ^ (this#stringText f)
   | TString s -> (this#op IaConstString) ^ (this#stringText s)
   | TBool true -> (this#op IaConstTrue)
   | TBool false -> (this#op IaConstFalse)
   | TNull -> (this#op IaConstNull)
   | TThis ->  (this#op IaConsThis)
   | TSuper ->  (this#op IaConstSuper)

   method get_array_type t =
      match follow t with
      | TInst ({cl_path=[],"Array"},[param]) ->
            let typeName = type_string_suff "" param false in
            (match typeName with
            | "::String"  -> ArrayData "String"
            | "int" | "Float" | "bool" | "String" | "unsigned char" | "::cpp::UInt8" ->
               ArrayData typeName
            | "cpp::ArrayBase" | "cpp::VirtualArray" | "Dynamic" -> ArrayAny
            | _ when is_interface_type param -> ArrayInterface (this#typeId (script_type_string param))
            | _ -> ArrayObject
            )
      | TAbstract (abs,pl) when abs.a_impl <> None ->
            this#get_array_type  (Abstract.get_underlying_type abs pl);
      | _ -> ArrayNone;

   method pushReturn inType =
      let oldReturnType = return_type in
      return_type <- inType;
      fun () -> return_type <- oldReturnType;
   method fileText file = string_of_int (this#fileId file)
   method indent_one = this#write indent_str
   method push_indent = indents <- indent_str::indents; indent <- String.concat "" indents
   method pop_indent = match indents with
                     | h::tail -> indents <- tail; indent <- String.concat "" indents
                     | [] -> indent <- "/*?*/";
   method write_i x = this#write (indent ^ x)
   method get_indent = indent
   method begin_expr = this#push_indent
   method end_expr = if not just_finished_block then this#write "\n"; this#pop_indent; just_finished_block <- true
   method op x = match cppia_op_info x with
      | (name,index) -> (if debug then name else string_of_int index) ^ " "
   method writeOp o = this#write (this#op o)
   method writeOpLine o = this#write ((this#op o) ^ "\n")
   method voidFunc isStatic isDynamic funcName fieldExpression =
      this#comment funcName;
      this#write ( (this#op IaFunction) ^ (this#staticText isStatic) ^ " " ^(this#boolText isDynamic) ^ " " ^(this#stringText funcName) ^ " ");
      this#write ((this#typeTextString "Void") ^ "0\n");
         this#gen_expression fieldExpression
   method func isStatic isDynamic funcName ret args isInterface fieldExpression =
      this#comment funcName;
      this#write ( (this#op IaFunction) ^ (this#staticText isStatic) ^ " " ^(this#boolText isDynamic) ^ " " ^(this#stringText funcName) ^ " ");
      this#write ((this#typeText ret) ^ (string_of_int (List.length args)) ^ " ");
      List.iter (fun (name,opt,typ) -> this#write ( (this#stringText name) ^ (this#boolText opt) ^ " " ^ (this#typeText typ) ^ " " )) args;
      this#write "\n";
      if (not isInterface) then begin
         match fieldExpression with
         | Some ({ eexpr = TFunction function_def } as e) ->
            if cppiaAst  then begin
               let args = List.map fst function_def.tf_args in
               let cppExpr = retype_expression ctx TCppVoid args function_def.tf_type function_def.tf_expr false in
               this#begin_expr;
               this#writePos function_def.tf_expr;
               this#write ( (this#op IaFun) ^ (this#typeText function_def.tf_type) ^ (string_of_int (List.length args)) ^ "\n" );
               List.iter (fun(arg,init) ->
                  this#write (indent ^ indent_str );
                  this#writeVar arg;
                  match init with
                  | Some const -> this#write ("1 " ^ (this#constText const) ^ "\n")
                  | _ -> this#write "0\n";
               ) function_def.tf_args;
               this#gen_expression_tree cppExpr;
               this#end_expr;
            end else
               this#gen_expression e
         | _ -> print_endline ("Missing function body for " ^ funcName );
      end
   method var readAcc writeAcc isExtern isStatic name varType varExpr =
      this#write ( (this#op IaVar) ^ (this#staticText isStatic) ^ " " ^ (this#op readAcc) ^ (this#op writeAcc) ^
         (this#boolText isExtern) ^ " " ^ (this#stringText name)^ (this#typeText varType) ^
         (match varExpr with Some _ -> "1" | _ -> "0" )  ^
         (if doComment then (" # " ^ name ^ "\n") else "\n") );
      match varExpr with
      | Some expression ->
            if cppiaAst then begin
               let varType = cpp_type_of ctx expression.etype in
               let cppExpr = retype_expression ctx varType [] t_dynamic expression false in
               this#gen_expression_tree cppExpr
            end else
               this#gen_expression expression
      | _ -> ()
   method implDynamic = this#writeOpLine IaImplDynamic;
   method writeVar v =
      this#ident v.v_name;
      this#wint v.v_id;
      this#writeBool v.v_capture;
      this#writeType v.v_type;
   method writeList prefix len = this#write (prefix ^" "  ^ (string_of_int (len)) ^ "\n");
   method wpos p = if debug then
      this#write ( (this#fileText p.pfile) ^ "\t" ^ (string_of_int (Lexer.get_error_line p) ) ^ indent);
   method writePos expr = this#wpos expr.epos
   method writeCppPos expr = this#wpos expr.cpppos
   method checkCast toType expr forceCast fromGenExpression=
   let write_cast text =
      if (not fromGenExpression) then
         this#writePos expr;
      this#write (text ^"\n" );
      this#begin_expr;
      this#gen_expression expr;
      this#end_expr;
      true;
   in
   let was_cast =
      if (is_interface_type toType) then begin
         if (is_dynamic_in_cppia ctx expr) then begin
            write_cast ( (this#op IaToInterface) ^ (this#typeText toType) ^ " " ^ (this#typeTextString "Dynamic") )
         end else if (not (is_matching_interface_type toType expr.etype)) then begin
            write_cast ( (this#op IaToInterface) ^ (this#typeText toType) ^ " " ^ (this#typeText expr.etype) )
         end else
            false
      end else begin
        let get_array_expr_type expr =
            if is_dynamic_in_cppia ctx expr then
               ArrayNone
            else
               this#get_array_type expr.etype
            in
         match (this#get_array_type toType), (get_array_expr_type expr) with
         | ArrayAny, _ -> false
         | ArrayObject, ArrayData _ -> write_cast (this#op IaToDynArray)
         | ArrayObject, ArrayObject -> false
         | ArrayObject, ArrayNone
         | ArrayObject, ArrayAny -> write_cast ((this#op IaToDataArray) ^ (this#typeTextString ("Array.Object")))
         | ArrayData t, ArrayNone
         | ArrayData t, ArrayObject
         | ArrayData t, ArrayAny -> write_cast ((this#op IaToDataArray)  ^ (this#typeTextString ("Array." ^ t)))
         | ArrayInterface t, ArrayNone
         | ArrayInterface t, ArrayAny -> write_cast ((this#op IaToInterfaceArray) ^ (string_of_int t))
         | _,_ -> (* a0,a1 ->
               let arrayString a =
                  match a with
                  | ArrayNone -> "ArrayNone"
                  | ArrayAny -> "ArrayAny"
                  | ArrayObject -> "ArrayObject"
                  | ArrayData _ -> "ArrayData"
                  | ArrayInterface _ -> "ArrayInterface"
            in
            this#write ("NOCAST " ^ (arrayString a0) ^ "=" ^ (arrayString a1));  *)
            false
      end
   in

   if (not was_cast) then begin
      if (forceCast) then begin
         let op =match (type_string expr.etype) with
         | "int" -> IaCastInt
         | "bool" -> IaCastBool
         | _ when is_interface_type toType -> IaNoCast
         | _ -> IaCast
         in
         this#writeOpLine op;
      end;
      this#gen_expression expr;
   end



   method gen_expression expr = (* { *)
   let expression = remove_parens expr in
   this#begin_expr;
   (*this#write ( (this#fileText expression.epos.pfile) ^ "\t" ^ (string_of_int (Lexer.get_error_line expression.epos) ) ^ indent);*)
   this#writePos expression;
   (match expression.eexpr with
   | TFunction function_def -> this#write ( (this#op IaFun) ^ (this#typeText function_def.tf_type) ^ (string_of_int (List.length function_def.tf_args)) ^ "\n" );
         List.iter (fun(arg,init) ->
            this#write (indent ^ indent_str );
            this#writeVar arg;
            match init with
            | Some const -> this#write ("1 " ^ (this#constText const) ^ "\n")
            | _ -> this#write "0\n";
         ) function_def.tf_args;
         let pop = this#pushReturn function_def.tf_type in
         this#gen_expression function_def.tf_expr;
         pop ();
   | TBlock expr_list -> this#writeList (this#op IaBlock) (List.length expr_list);
         List.iter this#gen_expression expr_list;
   | TConst const -> this#write (this#constText const)
   | TBreak -> this#writeOp IaBreak
   | TContinue -> this#writeOp IaContinue

   | TBinop (op,e1,e2) when op=OpAssign ->
      this#writeOpLine IaSet;
      this#gen_expression e1;
      this#checkCast e1.etype e2 false false;
   | TBinop (OpEq ,e1, { eexpr = TConst TNull } ) -> this#writeOpLine IaIsNull;
      this#gen_expression e1;
   | TBinop (OpNotEq ,e1, { eexpr = TConst TNull }) -> this#writeOpLine IaNotNull;
      this#gen_expression e1;
   | TBinop (OpEq , { eexpr = TConst TNull }, e1) -> this#writeOpLine IaIsNull;
      this#gen_expression e1;
   | TBinop (OpNotEq, { eexpr = TConst TNull }, e1) -> this#writeOpLine IaNotNull;
      this#gen_expression e1;
   | TBinop (op,e1,e2) -> this#writeOpLine (IaBinOp op);
      this#gen_expression e1;
      this#gen_expression e2;
   | TThrow e -> this#writeOpLine IaThrow;
      this#gen_expression e;
   | TArrayDecl expr_list ->
      this#write ( (this#op IaADef) ^ (this#typeText expression.etype) ^ " " ^(string_of_int (List.length expr_list))^"\n");
      List.iter this#gen_expression expr_list;
   | TIf (e,e1,e2) ->
      (match e2 with
      | None ->
         this#writeOpLine IaIf;
         this#gen_expression e;
         this#gen_expression e1;
      | Some elze ->
         this#writeOpLine IaIfElse;
         this#gen_expression e;
         this#gen_expression e1;
         this#gen_expression elze; )
   | TCall (func, arg_list) ->
      let argN = (string_of_int (List.length arg_list)) ^ " " in
      let gen_call () =
         (match (remove_parens_cast func).eexpr with
         | TField ( { eexpr = TIdent "__global__" }, field ) ->
                  this#write ( (this#op IaCallGlobal) ^ (this#stringText (field_name field)) ^ argN ^ (this#commentOf (field_name field)) ^ "\n");
         | TField (obj,FStatic (class_def,field) ) when is_real_function field ->
                  this#write ( (this#op IaCallStatic) ^ (this#instText class_def) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ (this#commentOf ( join_class_path class_def.cl_path "." ^ "." ^ field.cf_name) ) ^ "\n");
         | TField (obj,FInstance (_,_,field) ) when (is_this obj) && (is_real_function field) ->
                  this#write ( (this#op IaCallThis) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ (this#commentOf field.cf_name) ^ "\n");
         | TField (obj,FInstance (_,_,field) ) when is_super obj ->
                  this#write ( (this#op IaCallSuper) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ (this#commentOf field.cf_name) ^ "\n");
         (* Cppia does not have a "GetEnumIndex" op code - must use IaCallMember hx::EnumBase.__Index *)
         | TField (obj,FInstance (_,_,field) ) when field.cf_name = "_hx_getIndex" && (script_type_string obj.etype)="hx::EnumBase" ->
                  this#write ( (this#op IaCallMember) ^ (this#typeTextString "hx::EnumBase") ^ " " ^ (this#stringText "__Index") ^
                     argN ^ (this#commentOf ("Enum index") ) ^ "\n");
                  this#gen_expression obj;
         | TField (obj,FInstance (_,_,field) ) when field.cf_name = "__Index" || (not (is_dynamic_in_cppia ctx obj) && is_real_function field) ->
                  this#write ( (this#op IaCallMember) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ (this#commentOf field.cf_name) ^ "\n");
                  this#gen_expression obj;
         | TField (obj,FDynamic (name) )  when (is_internal_member name || (type_string obj.etype = "::String" && name="cca") ) ->
                  this#write ( (this#op IaCallMember) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText name) ^
                     argN ^ (this#commentOf name) ^ "\n");
                  this#gen_expression obj;
         | TConst TSuper -> this#write ((this#op IaCallSuperNew) ^ (this#typeText func.etype) ^ " " ^ argN ^ "\n");
         | TField (_,FEnum (enum,field)) -> this#write ((this#op IaCreateEnum) ^ (this#enumText enum) ^ " " ^ (this#stringText field.ef_name) ^ argN ^
                   (this#commentOf field.ef_name) ^ "\n");
         | _ -> this#write ( (this#op IaCall) ^ argN ^ "\n");
                  this#gen_expression func;
         );
         let matched_args = match func.etype with
            | TFun (args,_) ->
               ( try (
                  List.iter2 (fun (_,_,protoT) arg -> this#checkCast protoT arg false false)  args arg_list;
                  true; )
               with Invalid_argument _ -> (*print_endline "Bad count?";*) false )
            | _ -> false
         in
         if not matched_args then
            List.iter this#gen_expression arg_list;
      in
      (match (remove_parens_cast func).eexpr with
         | TField(obj,field) when is_array_or_dyn_array obj.etype && (field_name field)="map" ->
            (match this#get_array_type expression.etype with
            | ArrayData t ->
                this#write ( (this#op IaToDataArray) ^ (this#typeTextString ("Array." ^ t)) ^ "\n");
                this#begin_expr;
                this#writePos func;
                gen_call();
                this#end_expr;
            | ArrayInterface t ->
                this#write ( (this#op IaToInterfaceArray) ^ (string_of_int t) ^ "\n");
                this#begin_expr;
                this#writePos func;
                gen_call();
                this#end_expr;
            | _ -> gen_call();
            )
         | _ -> gen_call();
      );
   | TField (obj, acc) ->
      let objType = if is_dynamic_in_cppia ctx obj then "Dynamic" else script_type_string obj.etype in
      let typeText = if is_dynamic_in_cppia ctx obj then this#typeTextString "Dynamic" else this#typeText obj.etype in
      (match acc with
      | FDynamic name -> this#write ( (this#op IaFName) ^ typeText ^ " " ^ (this#stringText name) ^  (this#commentOf name) ^ "\n");
            this#gen_expression obj;
      | FStatic (class_def,field) -> this#write ( (this#op IaFStatic)  ^ (this#instText class_def) ^ " " ^
           (this#stringText field.cf_name) ^ (this#commentOf field.cf_name) );
      | FInstance (_,_,field) when is_this obj -> this#write ( (this#op IaFThisInst) ^ typeText ^ " " ^ (this#stringText field.cf_name)
                ^ (this#commentOf field.cf_name) );
      | FInstance (_,_,field) -> this#write ( (this#op IaFLink) ^ typeText ^ " " ^ (this#stringText field.cf_name) ^ (this#commentOf ( objType ^ "." ^ field.cf_name)) ^ "\n");
            this#gen_expression obj;

      | FClosure (_,field) when is_this obj -> this#write ( (this#op IaFThisName) ^typeText ^ " " ^  (this#stringText field.cf_name) ^ "\n")
      | FAnon (field) when is_this obj -> this#write ( (this#op IaFThisName) ^typeText ^ " " ^  (this#stringText field.cf_name) ^ (this#commentOf field.cf_name) ^ "\n")

      | FClosure (_,field)
      | FAnon (field) -> this#write ( (this#op IaFName) ^typeText ^ " " ^  (this#stringText field.cf_name) ^ (this#commentOf field.cf_name) ^ "\n");
            this#gen_expression obj;

      | FEnum (enum,field) -> this#write ( (this#op IaFEnum)  ^ (this#enumText enum) ^ " " ^ (this#stringText field.ef_name) ^ (this#commentOf field.ef_name)  );
      )
   | TArray (e1, e2) -> this#write ((this#op IaArrayI) ^ (this#typeText e1.etype) ^ "\n");
      this#gen_expression e1;
      this#gen_expression e2;
   | TUnop (op, flag, e) ->
      this#writeOpLine (match op,flag with
      | Increment, Prefix -> IaPlusPlus
      | Increment, _ -> IaPlusPlusPost
      | Decrement, Prefix -> IaMinusMinus
      | Decrement, _ -> IaMinusMinusPost
      | Not, _ -> IaLogicNot
      | Neg, _ -> IaNeg
      | NegBits, _ -> IaBitNot );
      this#gen_expression e;
   (* TODO - lval op-assign local/member/array *)
   | TLocal var -> this#write ((this#op IaVar) ^ (string_of_int var.v_id) ^ (this#commentOf var.v_name) );

   | TVar (tvar,optional_init) ->
         this#write ( (this#op IaTVars) ^ (string_of_int (1)) ^ (this#commentOf (tvar.v_name ^ ":" ^ (script_type_string tvar.v_type)) ) ^ "\n");
            this#write ("\t\t" ^ indent);
            (match optional_init with
            | None -> this#writeOp IaVarDecl;
                     this#writeVar tvar;
            | Some init ->this#writeOp IaVarDeclI;
                     let init = remove_parens init in
                     this#writeVar tvar;
                     this#write (" " ^ (this#typeText init.etype));
                     this#write "\n";
                     this#checkCast tvar.v_type init false false);
   | TNew (clazz,params,arg_list) ->
      this#write ((this#op IaNew) ^ (this#typeText (TInst(clazz,params))) ^ (string_of_int (List.length arg_list)) ^ "\n");
      let rec matched_args clazz = match clazz.cl_constructor, clazz.cl_super with
         | None, Some super -> matched_args (fst super)
         | None, _ -> false
         | Some ctr, _ ->
            (match ctr.cf_type with
            | TFun(args,_) ->
               ( try (
                  List.iter2 (fun (_,_,protoT) arg -> this#checkCast protoT arg false false)  args arg_list;
                  true; )
                 with Invalid_argument _ -> (*print_endline "Bad count?";*) false )
            | _ -> false
            )
      in
      if not (matched_args clazz) then
         List.iter this#gen_expression arg_list;

   | TReturn optval -> (match optval with
         | None -> this#writeOpLine IaReturn;
         | Some value -> this#write ( (this#op IaRetVal) ^ (this#typeText value.etype) ^ "\n");
              this#checkCast return_type value false false;
         )
   | TObjectDecl (
      (("fileName",_,_) , { eexpr = (TConst (TString file)) }) ::
         (("lineNumber",_,_) , { eexpr = (TConst (TInt line)) }) ::
            (("className",_,_) , { eexpr = (TConst (TString class_name)) }) ::
               (("methodName",_,_), { eexpr = (TConst (TString meth)) }) :: [] ) ->
            this#write ( (this#op IaPosInfo) ^ (this#stringText file) ^ (Printf.sprintf "%ld" line) ^ " " ^
                        (this#stringText class_name) ^ " " ^  (this#stringText meth))

   | TObjectDecl values ->this#write ( (this#op IaObjDef) ^ (string_of_int (List.length values)));
         this#write " ";
         List.iter (fun ((name,_,_),_) -> this#write (this#stringText name)  ) values;
         this#write "\n";
         List.iter (fun (_,e) -> this#gen_expression e ) values;
   | TTypeExpr type_expr ->
         let klass = "::" ^ (join_class_path (t_path type_expr) "::" ) in
         this#write ((this#op IaClassOf) ^ (string_of_int (this#typeId klass)))
   | TWhile (e1,e2,flag) -> this#write ( (this#op IaWhile) ^ (if flag=NormalWhile then "1" else "0" ) ^ "\n");
         this#gen_expression e1;
         this#gen_expression e2;
   | TFor (tvar,init,loop) -> this#writeOp IaFor;
         this#writeVar tvar;
         this#write "\n";
         this#gen_expression init;
         this#gen_expression loop;
   | TEnumParameter (expr,ef,i) ->
         let enum = match follow ef.ef_type with
            | TEnum(en,_) | TFun(_,TEnum(en,_)) -> en
            | _ -> assert false
         in
         this#write ( (this#op IaEnumI) ^ (this#typeText (TEnum(enum,[])) ) ^ (string_of_int i) ^ "\n");
         this#gen_expression expr;
   | TEnumIndex expr ->
         this#write ( (this#op IaCallMember) ^ (this#typeTextString "hx::EnumBase") ^ " " ^ (this#stringText "__Index") ^ "0" ^ (this#commentOf ("Enum index") ) ^ "\n");
         this#gen_expression expr;
   | TSwitch (condition,cases,optional_default)  ->
         this#write ( (this#op IaSwitch) ^ (string_of_int (List.length cases)) ^ " " ^
                           (match optional_default with None -> "0" | Some _ -> "1") ^ "\n");
         this#gen_expression condition;
         List.iter (fun (cases_list,expression) ->
            this#writeList ("\t\t\t"^indent) (List.length cases_list);
            List.iter (fun value -> this#gen_expression value ) cases_list;
            this#gen_expression expression;
         ) cases;
         (match optional_default with None -> () | Some expr -> this#gen_expression expr);
   | TTry (e,catches)  ->
         this#writeList (this#op IaTry) (List.length catches);
         this#gen_expression e;
         List.iter ( fun (tvar,catch_expr) ->
            this#write ("\t\t\t"^indent);
            this#writeVar tvar;
            this#write "\n";
            this#gen_expression catch_expr;
         ) catches;
   | TCast (cast,Some (TClassDecl t)) ->
         this#write ((this#op IaTCast) ^ (this#typeText (TInst(t,[])) ) ^ "\n");
         this#gen_expression cast;
   | TCast (cast,_) -> this#checkCast expression.etype cast true true;
   | TParenthesis _ -> abort "Unexpected parens" expression.epos
   | TMeta(_,_) -> abort "Unexpected meta" expression.epos
   | TIdent _ -> abort "Unexpected ident" expression.epos
   );
   this#end_expr;
   (* } *)
   method gen_expression_tree expression_tree = (* { *)
      let rec gen_expression expression =
      begin
         this#begin_expr;
         this#writeCppPos expression;
         let rec match_expr expression = match expression.cppexpr with
         | CppBlock(exprs,closures,_) ->
            this#writeList (this#op IaBlock) (List.length exprs);
            List.iter gen_expression exprs;

         | CppVarDecl(var,init) ->
            let name =  cpp_var_name_of var in
            this#write ( (this#op IaTVars) ^ (string_of_int (1)) ^ (this#commentOf (name ^ ":" ^ (script_type_string var.v_type)) ) ^ "\n");
            this#write ("\t\t" ^ indent);
            (match init with
            | None -> this#writeOp IaVarDecl; this#writeVar var;
            | Some init ->this#writeOp IaVarDeclI;
                this#writeVar var;
                this#write (" " ^ (this#astType init.cpptype));
                this#write "\n";
                gen_expression init;
            )
         | CppInt i -> this#write ((this#op IaConstInt) ^ (Printf.sprintf "%ld " i))
         | CppFloat float_as_string -> this#write ((this#op IaConstFloat) ^ (this#stringText float_as_string))
         | CppString s -> this#write ((this#op IaConstString) ^ (this#stringText s))
         | CppBool false -> this#writeOp IaConstFalse
         | CppBool true -> this#writeOp IaConstTrue
         | CppNull -> this#writeOp IaConstNull
         | CppNil -> abort "Nil not supported in cppia" expression.cpppos
         | CppThis _ -> this#writeOp IaConsThis
         | CppSuper _ -> this#writeOp IaConstSuper
         | CppBreak -> this#writeOp IaBreak
         | CppContinue -> this#writeOp IaContinue
         | CppGoto label -> abort "Goto not supported in cppia" expression.cpppos
         | CppReturn None -> this#writeOpLine IaReturn;

         | CppReturn Some value ->
             this#write ( (this#op IaRetVal) ^ (this#astType value.cpptype) ^ "\n");
             gen_expression value;

         | CppWhile(condition, block, while_flag, _) ->
             this#write ( (this#op IaWhile) ^ (if while_flag=NormalWhile then "1" else "0" ) ^ "\n");
             gen_expression condition;
             gen_expression block;

         | CppIf (condition,block,None) ->
            this#writeOpLine IaIf;
            gen_expression condition;
            gen_expression block;

         | CppIf (condition,block,Some elze) ->
            this#writeOpLine IaIfElse;
            gen_expression condition;
            gen_expression block;
            gen_expression elze;

         | CppBinop(op, left, right) ->
            this#writeOpLine (IaBinOp op);
            gen_expression left;
            gen_expression right;

         | CppVar var -> gen_var_loc var

         | CppExtern (name,_) -> abort ("Unexpected global '"^ name ^"' in cppia") expression.cpppos

         | CppSet(lvalue,rvalue) ->
            this#writeOpLine IaSet;
            gen_lvalue lvalue expression.cpppos;
            gen_expression rvalue;

         | CppCall(func, args) ->
            let argN = (string_of_int (List.length args)) ^ " " in
            (match func with
            | FuncThis(field, inst) ->
               let name = field.cf_name in
               this#write ( (this#op IaCallThis) ^ (this#astType inst) ^ " " ^ (this#stringText name) ^
                     argN ^ (this#commentOf name) ^ "\n");
            | FuncInstance(expr,_,field)
            | FuncInterface(expr,_,field) ->
               this#write ( (this#op IaCallMember) ^ (this#astType expr.cpptype) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ (this#commentOf field.cf_name) ^ "\n");
               gen_expression expr;
            | FuncStatic(class_def,_,field) ->
               this#write ( (this#op IaCallStatic) ^ (this#cppInstText class_def) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ (this#commentOf ( join_class_path class_def.cl_path "." ^ "." ^ field.cf_name) ) ^ "\n");
            | FuncTemplate _ -> abort "Templated function call not supported in cppia" expression.cpppos
            | FuncFromStaticFunction -> abort "Unexpected FuncFromStaticFunction" expression.cpppos
            | FuncEnumConstruct(enum,field) ->
               this#write ((this#op IaCreateEnum) ^ (this#enumText enum) ^ " " ^ (this#stringText field.ef_name) ^ argN ^
                   (this#commentOf field.ef_name) ^ "\n");
            | FuncSuperConstruct childType ->
               this#write ((this#op IaCallSuperNew) ^ (this#astType childType) ^ " " ^ argN ^ "\n");
            | FuncSuper(_,objType,field) ->
               this#write ( (this#op IaCallSuper) ^ (this#astType objType) ^ " " ^ (this#stringText field.cf_name) ^
                  argN ^ (this#commentOf field.cf_name) ^ "\n");
            | FuncExtern(name,_) ->
               this#write ( (this#op IaCallGlobal) ^ (this#stringText name) ^ argN ^ (this#commentOf name) ^ "\n");

            | FuncNew(newType) ->
               this#write ((this#op IaNew) ^ (this#astType newType) ^ argN ^ "\n");

            | FuncInternal(obj,"cca",".") when obj.cpptype=TCppString ->
               this#write ( (this#op IaCallMember) ^ (this#astType obj.cpptype) ^ " " ^ (this#stringText "cca") ^
                     argN ^ (this#commentOf "cca") ^ "\n");
               gen_expression obj;
            | FuncInternal(obj,name,join) ->
               (* abort ("Internal function call '" ^ name ^ "' not supported in cppia") expression.cpppos; *)
               this#write ( (this#op IaCallMember) ^ (this#astType obj.cpptype) ^ " " ^ (this#stringText name) ^
                     argN ^ (this#commentOf name) ^ "\n");
               gen_expression obj;
            | FuncExpression(expr)  ->
               this#write ( (this#op IaCall) ^ argN ^ "\n");
               gen_expression expr;
            );
            List.iter gen_expression args;

         | CppFunction(func,_) ->
            (match func with
            | FuncThis(field, inst) ->
               this#write ( (this#op IaFThisName) ^ (this#astType inst) ^ " " ^ (this#stringText field.cf_name) ^
                  (this#commentOf ((script_cpptype_string inst) ^ "." ^ field.cf_name)) );
            | FuncInternal(expr,name,_) ->
               this#write ( (this#op IaFLink) ^ (this#astType expr.cpptype) ^ " " ^ (this#stringText name) ^
                 (this#commentOf ( "Internal " ^ (script_cpptype_string expr.cpptype) ^ "." ^ name)) ^ "\n");
               gen_expression expr;

            | FuncInstance(expr,_,field)
            | FuncInterface(expr,_,field) ->
               this#write ( (this#op IaFName) ^ (this#astType expr.cpptype) ^ " " ^ (this#stringText field.cf_name) ^
                 (this#commentOf ( (script_cpptype_string expr.cpptype) ^ "." ^ field.cf_name)) ^ "\n");
               gen_expression expr;

            | FuncStatic(class_def,_,field) ->
               this#write ( (this#op IaFStatic)  ^ (this#cppInstText class_def) ^ " " ^ (this#stringText field.cf_name) ^ (this#commentOf field.cf_name) );
            | FuncExpression(expr) -> match_expr expr;
            | FuncExtern(name,_) ->abort ("Can't create extern " ^ name ^ " closure") expression.cpppos
            | FuncSuper _ | FuncSuperConstruct _ -> abort "Can't create super closure" expression.cpppos
            | FuncNew _ -> abort "Can't create new closure" expression.cpppos
            | FuncEnumConstruct _ -> abort "Enum constructor outside of CppCall" expression.cpppos
            | FuncFromStaticFunction -> abort "Can't create cpp.Function.fromStaticFunction closure" expression.cpppos
            | FuncTemplate _ -> abort "Can't create template function closure" expression.cpppos
            )

         | CppPosition(file,line,class_name,meth) ->
            this#write ( (this#op IaPosInfo) ^ (this#stringText file) ^ (Printf.sprintf "%ld" line) ^ " " ^
                        (this#stringText class_name) ^ " " ^  (this#stringText meth))

         | CppNullCompare("IsNull", e) ->
            this#writeOpLine IaIsNull;
            gen_expression e;

         | CppNullCompare(_, e) ->
            this#writeOpLine IaNotNull;
            gen_expression e;

         | CppCompare(_, left, right, op) ->
            this#writeOpLine (IaBinOp op);
            gen_expression left;
            gen_expression right;

         | CppArray(arrayLoc) -> gen_array arrayLoc expression.cpppos

         | CppArrayDecl(exprList) ->
            this#write ( (this#op IaADef) ^ (this#astType expression.cpptype) ^ " " ^(string_of_int (List.length exprList))^"\n");
            List.iter gen_expression exprList;

         | CppEnumField(enum,field) ->
            this#write ( (this#op IaFEnum)  ^ (this#enumText enum) ^ " " ^ (this#stringText field.ef_name) ^ (this#commentOf field.ef_name) );

         | CppEnumIndex(obj) ->
            (* Cppia does not have a "GetEnumIndex" op code - must use IaCallMember hx::EnumBase.__Index *)
            this#write ( (this#op IaCallMember) ^ (this#typeTextString "hx::EnumBase") ^ " " ^ (this#stringText "__Index") ^
                     "0" ^ (this#commentOf ("Enum index") ) ^ "\n");
            gen_expression obj;

         | CppDynamicField(obj,name) ->
            this#write ( (this#op IaFName) ^ (this#typeTextString "Dynamic") ^ " " ^ (this#stringText name) ^  (this#commentOf name) ^ "\n");
            gen_expression obj;

         | CppClassOf (path,native) ->
            let klass =  (join_class_path path "." ) in
            this#write ((this#op IaClassOf) ^ (this#typeTextString klass) ^  (this#commentOf klass) );

         | CppEnumParameter(obj,field,index) ->
            this#write ( (this#op IaEnumI) ^ (this#typeTextString "Dynamic") ^ (string_of_int index) ^ "\n");
            gen_expression obj;

         | CppClosure closure ->
            this#write ( (this#op IaFun) ^ (this#astType closure.close_type) ^ (string_of_int (List.length closure.close_args)) ^ "\n" );
            List.iter (fun(arg,init) ->
               this#write (indent ^ indent_str );
               this#writeVar arg;
               match init with
               | Some const -> this#write ("1 " ^ (this#constText const) ^ "\n")
               | _ -> this#write "0\n";
            ) closure.close_args;
            gen_expression closure.close_expr;

         | CppObjectDecl (values,isStruct) ->this#write ( (this#op IaObjDef) ^ (string_of_int (List.length values)));
            this#write " ";
            List.iter (fun (name,_) -> this#write (this#stringText name)  ) values;
            this#write "\n";
            List.iter (fun (_,e) -> gen_expression e ) values;

         | CppCrement(incFlag,preFlag,lvalue) ->
            let op = match incFlag, preFlag with
            | CppIncrement, Prefix -> IaPlusPlus
            | CppIncrement, Postfix -> IaPlusPlusPost
            | CppDecrement, Prefix -> IaMinusMinus
            | CppDecrement, Postfix -> IaMinusMinusPost
            in
            this#writeOpLine op;
            gen_lvalue lvalue expression.cpppos;

         | CppModify(op,lvalue,rvalue) ->
            this#writeOpLine (IaBinOp (OpAssignOp op));
            gen_lvalue lvalue expression.cpppos;
            gen_expression rvalue;

         | CppUnop(op,expr) ->
            let op = match op with
            | CppNot -> IaLogicNot
            | CppNeg -> IaNeg
            | CppNegBits -> IaBitNot
            in
            this#writeOpLine op;
            gen_expression expr;

         | CppThrow(value) -> this#writeOpLine IaThrow;
            gen_expression value;

         | CppTry(block,catches) ->
            this#writeList (this#op IaTry) (List.length catches);
            gen_expression block;
            List.iter ( fun (tvar,catch_expr) ->
               this#write ("\t\t\t"^indent);
               this#writeVar tvar;
               this#write "\n";
               gen_expression catch_expr;
            ) catches;

         | CppIntSwitch _ -> abort "CppIntSwitch not supported in cppia" expression.cpppos;
         | CppSwitch(condition,_, cases, optional_default, _) ->
            this#write ( (this#op IaSwitch) ^ (string_of_int (List.length cases)) ^ " " ^
                              (match optional_default with None -> "0" | Some _ -> "1") ^ "\n");
            gen_expression condition;
            List.iter (fun (cases_list,expression) ->
               this#writeList ("\t\t\t"^indent) (List.length cases_list);
               List.iter (fun value -> gen_expression value ) cases_list;
               gen_expression expression;
            ) cases;
            (match optional_default with None -> () | Some expr -> gen_expression expr);


         | CppTCast(expr,toType) ->
            this#write ((this#op IaTCast) ^ (this#astType toType) ^ "\n");
            gen_expression expr;

         | CppCast(expr,toType) ->
            (match toType with
            | TCppDynamicArray ->
               this#write ((this#op IaToDynArray) ^ "\n");
               gen_expression expr;
            | TCppObjectArray(_) ->
               this#write ((this#op IaToDataArray) ^ (this#typeTextString ("Array.Object")) ^ "\n");
               gen_expression expr;
            | TCppScalarArray(t) ->
               this#write ((this#op IaToDataArray)  ^ (this#typeTextString ("Array." ^ (script_cpptype_string t))) ^ "\n");
               gen_expression expr;
            | _ -> match_expr expr
            )

         | CppCastScalar(expr,"bool") ->
             this#writeOpLine IaCastBool;
             gen_expression expr;

         | CppCastScalar(expr,"int") ->
             this#writeOpLine IaCastInt;
             gen_expression expr;

         | CppCastScalar(expr,_) -> match_expr expr
         | CppCastVariant(expr) -> match_expr expr
         | CppCastStatic(expr,_) -> match_expr expr
         | CppNullAccess ->
            this#writeOpLine IaThrow;
            this#begin_expr;
            this#writeCppPos expression;
            this#write ((this#op IaConstString) ^ (this#stringText "Null access"));
            this#end_expr;

         | CppCode _
         | CppFunctionAddress _
         | CppDereference _
         | CppAddressOf _
         | CppFor _
         | CppCastObjC _
         | CppCastObjCBlock _
         | CppCastProtocol _
         | CppCastNative _ -> abort ("Unsupported operation in cppia :" ^ (s_tcpp expression.cppexpr) ) expression.cpppos

         (*| x -> print_endline ("Unknown cppexpr " ^ (s_tcpp x) );*)
         in
         match_expr expression;
         this#end_expr;
      end and gen_array arrayLoc pos =
         match arrayLoc with
            | ArrayObject(arrayObj, index, _)
            | ArrayTyped(arrayObj, index, _) ->
               this#write ((this#op IaArrayI) ^ (this#astType arrayObj.cpptype) ^ "\n");
               gen_expression arrayObj;
               gen_expression index;
            | ArrayPointer(_, _)
            | ArrayRawPointer(_,_) -> abort "Unvalid array access in cppia" pos
            | ArrayVirtual(arrayObj, index)
            | ArrayImplements(_,arrayObj,index)
            | ArrayDynamic(arrayObj, index) ->
               this#write ((this#op IaArrayI) ^ (this#astType arrayObj.cpptype) ^ "\n");
               gen_expression arrayObj;
               gen_expression index;
      and gen_lvalue lvalue pos =
         this#begin_expr;
         this#wpos pos;
         (match lvalue with
         | CppVarRef varLoc -> gen_var_loc varLoc
         | CppArrayRef arrayLoc -> gen_array arrayLoc pos
         | CppExternRef(name,_) -> abort ("Unsupported extern '" ^ name ^ "' in cppia") pos;
         | CppDynamicRef(expr,name) ->
            let typeText = this#typeTextString "Dynamic" in
            this#write ( (this#op IaFName) ^ typeText ^ " " ^ (this#stringText name) ^  (this#commentOf name) ^ "\n");
            gen_expression expr;
         );
         this#end_expr;

      and gen_var_loc loc =
         match loc with
         | VarClosure(var)
         | VarLocal(var) ->
            this#write ((this#op IaVar) ^ (string_of_int var.v_id) ^ (this#commentOf var.v_name) )
         | VarStatic(class_def,_,field) ->
            this#write ( (this#op IaFStatic)  ^ (this#cppInstText class_def) ^ " " ^ (this#stringText field.cf_name) ^ (this#commentOf field.cf_name) );
         | VarThis(field, thisType) ->
            this#write ( (this#op IaFThisInst) ^ (this#astType thisType) ^ " " ^ (this#stringText field.cf_name) ^ (this#commentOf field.cf_name) );
         | VarInstance(obj,field,_,_)
         | VarInterface(obj,field) ->
            let objType = script_cpptype_string obj.cpptype in
            this#write ( (this#op IaFLink) ^ (this#astType obj.cpptype) ^ " " ^ (this#stringText field.cf_name) ^ (this#commentOf ( objType ^ "." ^ field.cf_name)) ^ "\n");
            gen_expression obj;
         | VarInternal(obj,_,name) ->
            let objType = script_cpptype_string obj.cpptype in
            this#write ( (this#op IaFLink) ^ (this#astType obj.cpptype) ^ " " ^ (this#stringText name) ^ (this#commentOf ( objType ^ "." ^ name)) ^ "\n");
            gen_expression obj;
      (*
      and get_array_type elem =
         this#stringText (script_cpptype_string elem.cpptype);
      *)
      in
      gen_expression expression_tree
end;;




let generate_script_class common_ctx script class_def =
   script#incClasses;
   let classText = (join_class_path class_def.cl_path ".") in
   script#comment ("Class " ^ classText);
   script#writeOp (if class_def.cl_interface then IaInterface else IaClass );
   script#instName class_def;
   (match class_def.cl_super with
      | None -> script#ident ""
      | Some (c,_) -> script#instName c);
   script#wint (List.length class_def.cl_implements);
   List.iter (fun(c,_) -> script#instName c) class_def.cl_implements;
   script#write "\n";
   (* Looks like some map impl classes have their bodies discarded - not sure best way to filter *)
   let non_dodgy_function field =
      class_def.cl_interface ||
      match field.cf_kind, field.cf_expr with
      | Var _, _ -> true
      | Method MethDynamic, _ -> true
      | Method _, Some _ -> true
      | _ -> false
   in
   let ordered_statics = List.filter non_dodgy_function class_def.cl_ordered_statics in
   let ordered_fields = List.filter non_dodgy_function class_def.cl_ordered_fields in
   script#write ((string_of_int ( (List.length ordered_fields) +
                                 (List.length ordered_statics) +
                                 (match class_def.cl_constructor with Some _ -> 1 | _ -> 0 ) +
                                 (if (implement_dynamic_here class_def) then 1 else 0) +
                                 (match class_def.cl_init with Some _ -> 1 | _ -> 0 ) ) )
                                 ^ "\n");

   let generate_field isStatic field =
      match field.cf_kind, follow field.cf_type with
      | Var { v_read = AccInline; v_write = AccNever },_ ->
         script#writeOpLine IaInline;
      | Var v,_ ->
         let mode_code mode = match mode with
         | AccNormal | AccCtor -> IaAccessNormal
         | AccNo -> IaAccessNot
         | AccNever -> IaAccessNot
         | AccResolve -> IaAccessResolve
         | AccCall -> if ( (has_meta_key class_def.cl_meta Meta.NativeProperty) ||
                           (has_meta_key field.cf_meta Meta.NativeProperty) ||
                           (Common.defined common_ctx Define.ForceNativeProperty) )
                         then IaAccessCallNative else IaAccessCall;
         | AccInline -> IaAccessNormal
         | AccRequire (_,_) -> IaAccessNormal
         in
         let isExtern = not (is_physical_field field) in
         script#var (mode_code v.v_read) (mode_code v.v_write) isExtern isStatic field.cf_name field.cf_type field.cf_expr
      | Method MethDynamic, TFun(args,ret) ->
         script#func isStatic true field.cf_name ret args class_def.cl_interface field.cf_expr
      | Method _, TFun(args,ret) when field.cf_name="new" ->
         script#func true false "new" (TInst(class_def,[])) args false field.cf_expr
      | Method _, TFun (args,ret) ->
         script#func isStatic false field.cf_name ret args class_def.cl_interface field.cf_expr
      | Method _, _ -> print_endline ("Unknown method type " ^ (join_class_path class_def.cl_path "." )
                     ^ "." ^field.cf_name )
   in
   (match class_def.cl_constructor with
      | Some field  -> generate_field true field
      | _ -> () );
   (match class_def.cl_init with
      | Some expression  -> script#voidFunc true false "__init__" expression
      | _ -> () );

   List.iter (generate_field false) ordered_fields;
   List.iter (generate_field true) ordered_statics;
   if (implement_dynamic_here class_def) then
      script#implDynamic;
   script#write "\n";
;;

let generate_script_enum common_ctx script enum_def meta =
   script#incClasses;
   let sorted_items = List.sort (fun f1 f2 -> (f1.ef_index - f2.ef_index ) ) (pmap_values enum_def.e_constrs) in
   script#writeList ((script#op IaEnum) ^ (script#enumText enum_def)) (List.length sorted_items);

   List.iter (fun constructor ->
      let name = script#stringText constructor.ef_name in
      match constructor.ef_type with
      | TFun (args,_) ->
         script#write ( name ^ " " ^ (string_of_int (List.length args)) );
         List.iter (fun (arg,_,t) -> script#write ( " " ^ (script#stringText arg) ^ " " ^ (script#typeText t) ) ) args;
         script#write "\n";
      | _ -> script#write ( name ^ " 0\n" )
   ) sorted_items;

   match meta with
   | Some expr -> script#write "1\n";
      script#gen_expression expr
   | _ -> script#write "0\n";
   script#write "\n"
;;


let generate_cppia ctx =
   let common_ctx = ctx.ctx_common in
   let debug = ctx.ctx_debug_level in
   let script = new script_writer ctx common_ctx.file common_ctx.debug in
   ignore (script#stringId "");
   ignore (script#typeId "");

   List.iter (fun object_def ->
      (match object_def with
      | TClassDecl class_def when class_def.cl_extern  ->
         () (*if (gen_externs) then gen_extern_class common_ctx class_def;*)
      | TClassDecl class_def ->
         let is_internal = is_internal_class class_def.cl_path in
         if (is_internal || (is_macro class_def.cl_meta)) then
            ( if (debug>=4) then print_endline (" internal class " ^ (join_class_path class_def.cl_path ".") ))
         else begin
            generate_script_class common_ctx script class_def
         end
      | TEnumDecl enum_def when enum_def.e_extern -> ()
      | TEnumDecl enum_def ->
         let is_internal = is_internal_class enum_def.e_path in
         if (is_internal) then
            (if (debug>=4) then print_endline (" internal enum " ^ (join_class_path enum_def.e_path ".") ))
         else begin
            let meta = Codegen.build_metadata common_ctx object_def in
            if (enum_def.e_extern) then
               (if (debug>=4) then print_endline ("external enum " ^  (join_class_path enum_def.e_path ".") ));
            generate_script_enum common_ctx script enum_def meta
         end
      | TTypeDecl _ | TAbstractDecl _ -> (* already done *) ()
      );
   ) common_ctx.types;

   (match common_ctx.main with
   | None -> script#writeOpLine IaNoMain;
   | Some e -> script#writeOpLine IaMain;
         script#gen_expression e
   );

   script#write ( (script#op IaResources) ^ (string_of_int (Hashtbl.length common_ctx.resources)) ^ "\n");
   Hashtbl.iter (fun name data ->
      script#write ((script#op IaReso) ^ (script#stringText name) ^  (string_of_int (String.length data)) ^ "\n");
   ) common_ctx.resources;
   Hashtbl.iter (fun _ data -> script#writeData data) common_ctx.resources;

   script#close
;;


(*
 The common_ctx contains the haxe AST in the "types" field and the resources
*)
let generate_source ctx =
   let common_ctx = ctx.ctx_common in
   let debug = ctx.ctx_debug_level in
   make_base_directory common_ctx.file;
   let exe_classes = ref [] in
   let boot_classes = ref [] in
   let boot_enums = ref [] in
   let nonboot_classes = ref [] in
   let init_classes = ref [] in
   let super_deps = create_super_dependencies common_ctx in
   let constructor_deps = create_constructor_dependencies common_ctx in
   let main_deps = ref [] in
   let extern_src = ref [] in
   let jobs = ref [] in
   let build_xml = ref "" in
   let scriptable = (Common.defined common_ctx Define.Scriptable) in
   let existingIds = Hashtbl.create 0 in

   List.iter (fun object_def ->
      (* check if any @:objc class is referenced while '-D objc' is not defined
         This will guard all code changes to this flag *)
      (if not (Common.defined common_ctx Define.Objc) then match object_def with
         | TClassDecl class_def when Meta.has Meta.Objc class_def.cl_meta ->
            abort "In order to compile '@:objc' classes, please define '-D objc'" class_def.cl_pos
         | _ -> ());
      (match object_def with
      | TClassDecl class_def when is_extern_class class_def ->
         build_xml := !build_xml ^ (get_class_code class_def Meta.BuildXml);
         let source = get_meta_string_path class_def.cl_meta Meta.SourceFile in
         if (source<>"") then
            extern_src := source :: !extern_src;
      | TClassDecl class_def ->
         let name =  class_text class_def.cl_path in
         let is_internal = is_internal_class class_def.cl_path in
         if (is_internal || (is_macro class_def.cl_meta)) then
            ( if (debug>=4) then print_endline (" internal class " ^ name ))
         else begin
            let rec makeId class_name seed =
               let id = gen_hash32 seed class_name in
               (* reserve first 100 ids for runtime *)
               if id < Int32.of_int 100 || Hashtbl.mem existingIds id then
                  makeId class_name (seed+100)
               else begin
                  Hashtbl.add existingIds id true;
                  Hashtbl.add ctx.ctx_type_ids class_name id;
               end in
            makeId name 0;

            build_xml := !build_xml ^ (get_class_code class_def Meta.BuildXml);
            if (has_init_field class_def) then
               init_classes := class_def.cl_path ::  !init_classes;
            if (has_boot_field class_def) then
               boot_classes := class_def.cl_path ::  !boot_classes
            else if not (has_meta_key class_def.cl_meta Meta.NativeGen) then
               nonboot_classes := class_def.cl_path ::  !nonboot_classes;
            jobs := (fun () -> generate_class_files ctx super_deps constructor_deps class_def scriptable ) :: !jobs;
            let deps = generate_class_deps ctx class_def super_deps constructor_deps in
            if not (class_def.cl_interface && (is_native_gen_class class_def)) then
               exe_classes := (class_def.cl_path, deps, object_def)  ::  !exe_classes;
         end
      | TEnumDecl enum_def when enum_def.e_extern -> ()
      | TEnumDecl enum_def ->
         let name =  class_text enum_def.e_path in
         let is_internal = is_internal_class enum_def.e_path in
         if (is_internal) then
            (if (debug>1) then print_endline (" internal enum " ^ name ))
         else begin
            let meta = Codegen.build_metadata common_ctx object_def in
            if (enum_def.e_extern) then
               (if (debug>1) then print_endline ("external enum " ^ name ));
            boot_enums := enum_def.e_path :: !boot_enums;
            jobs := (fun () -> generate_enum_files ctx enum_def super_deps meta ) :: !jobs;
            let deps = generate_enum_deps ctx enum_def super_deps in
            exe_classes := (enum_def.e_path, deps, object_def) :: !exe_classes;
         end
      | TTypeDecl _ | TAbstractDecl _ -> (* already done *) ()
      );
   ) common_ctx.types;

   List.iter (fun job -> job () ) !jobs;


   (match common_ctx.main with
   | None -> generate_dummy_main common_ctx
   | Some e ->
      let main_field = { cf_name = "__main__"; cf_type = t_dynamic; cf_expr = Some e; cf_expr_unoptimized = None; cf_pos = e.epos; cf_name_pos = null_pos; cf_public = true; cf_meta = []; cf_overloads = []; cf_doc = None; cf_kind = Var { v_read = AccNormal; v_write = AccNormal; }; cf_params = [] } in
      let class_def = { null_class with cl_path = ([],"@Main"); cl_ordered_statics = [main_field] } in
      main_deps := find_referenced_types ctx (TClassDecl class_def) super_deps constructor_deps false true false;
      generate_main ctx super_deps class_def
   );

   generate_boot ctx !boot_enums !boot_classes !nonboot_classes !init_classes;

   generate_files common_ctx ctx.ctx_file_info;

   write_resources common_ctx;

   (* Output class info if requested *)
   if (scriptable || (Common.defined common_ctx Define.DllExport) ) then begin
      let filename =
         try
            let value = Common.defined_value common_ctx Define.DllExport in
            if value="1" then raise Not_found;
            value
         with Not_found -> "export_classes.info"
      in
      if (filename <> "") then begin
         let escape s =
            let b = Buffer.create 0 in
            for i = 0 to String.length s - 1 do
               let c = String.unsafe_get s i in
               match c with
               | '\\' -> Buffer.add_char b c; Buffer.add_char b c;
               | ' ' -> Buffer.add_char b '\\'; Buffer.add_char b 's';
               | '\n' -> Buffer.add_char b '\\'; Buffer.add_char b 'n';
               | _ -> Buffer.add_char b c;
            done;
            Buffer.contents b;
         in

         let exeClasses = open_out filename in
         let out = output_string exeClasses in
         let outline str = output_string exeClasses (str ^ "\n") in
         let spath path = (join_class_path path ".") in
         List.iter (fun (name,_,def) ->
            match def with
            | TClassDecl class_def ->
                outline ((if class_def.cl_interface then "interface " else "class ") ^ (spath name) );
            | TEnumDecl enum_def ->
                out ("enum " ^ (spath name) ^ "\n");
            | _ -> ()
            ) !exe_classes;

         (* Output file info too *)
         List.iter ( fun file ->
               let full_path = Path.get_full_path (try Common.find_file common_ctx file with Not_found -> file) in
               if file <> "?" then
                  out ("file " ^ (escape file) ^ " " ^ (escape full_path) ^"\n") )
            ( List.sort String.compare ( pmap_keys !(ctx.ctx_file_info) ) );
         close_out exeClasses;
     end;
   end;

   let output_name = match  common_ctx.main_class with
   | Some path -> (snd path)
   | _ -> "output" in

   write_build_data common_ctx (common_ctx.file ^ "/Build.xml") !exe_classes !main_deps (!boot_enums@ !boot_classes) !build_xml !extern_src output_name;
   let cmd_defines = ref "" in
   PMap.iter ( fun name value -> match name with
      | "true" | "sys" | "dce" | "cpp" | "debug" -> ()
      | _ -> cmd_defines := !cmd_defines ^ " -D" ^ name ^ "=\"" ^ (escape_command value) ^ "\"" ) common_ctx.defines.Define.values;
   write_build_options common_ctx (common_ctx.file ^ "/Options.txt") common_ctx.defines.Define.values;
   if ( not (Common.defined common_ctx Define.NoCompilation) ) then begin
      let t = Common.timer ["generate";"cpp";"native compilation"] in
      let old_dir = Sys.getcwd() in
      Sys.chdir common_ctx.file;
      let cmd = ref "haxelib run hxcpp Build.xml haxe" in
      if (common_ctx.debug) then cmd := !cmd ^ " -Ddebug";
      cmd := !cmd ^ !cmd_defines;
      cmd := List.fold_left (fun cmd path -> cmd ^ " -I\"" ^ (escape_command path) ^ "\"" ) !cmd common_ctx.class_path;
      common_ctx.print (!cmd ^ "\n");
      if common_ctx.run_command !cmd <> 0 then failwith "Build failed";
      Sys.chdir old_dir;
      t()
   end
   ;;

let generate common_ctx =
   let debug_level = if (Common.defined common_ctx Define.NoDebug) then 0 else 1 in
   if (Common.defined common_ctx Define.Cppia) then begin
      let ctx = new_context common_ctx debug_level (ref PMap.empty) (Hashtbl.create 0)  in
      generate_cppia ctx
   end else begin
      let ctx = new_context common_ctx debug_level (ref PMap.empty) (create_member_types common_ctx) in
      generate_source ctx
   end
;;


