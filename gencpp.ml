(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast
open Type
open Common

let unsupported p = error "This expression cannot be generated to Cpp" p

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


(* The internal classes are implemented by the core hxcpp system, so the cpp
   classes should not be generated *)
let is_internal_class = function
   |  ([],"Int") | ([],"Void") |  ([],"String") | ([], "Null") | ([], "Float")
   |  ([],"Array") | ([], "Class") | ([], "Enum") | ([], "Bool")
   |  ([], "Dynamic") | ([], "ArrayAccess") | (["cpp"], "FastIterator")
   |  (["cpp"],"Pointer") | (["cpp"],"ConstPointer")
   |  (["cpp"],"RawPointer") | (["cpp"],"RawConstPointer")
   |  (["cpp"],"Function") -> true
   |  ([],"Math") | (["haxe";"io"], "Unsigned_char__") -> true
   |  (["cpp"],"Int8") | (["cpp"],"UInt8") | (["cpp"],"Char")
   |  (["cpp"],"Int16") | (["cpp"],"UInt16")
   |  (["cpp"],"Int32") | (["cpp"],"UInt32")
   |  (["cpp"],"Int64") | (["cpp"],"UInt64")
   |  (["cpp"],"Float32") | (["cpp"],"Float64") -> true
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

class source_writer common_ctx write_func close_func =
   object(this)
   val indent_str = "\t"
   val mutable indent = ""
   val mutable indents = []
   val mutable just_finished_block = false
   method close = close_func(); ()
   method write x = write_func x; just_finished_block <- false
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


   method add_include class_path =
      ( match class_path with
         | (["@verbatim"],file) -> this#write ("#include \"" ^ file ^ "\"\n");
         | _ ->
            let prefix = if should_prefix_include class_path then "" else get_include_prefix common_ctx true in
            this#write ("#ifndef INCLUDED_" ^ (join_class_path class_path "_") ^ "\n");
            this#write ("#include <" ^ prefix ^ (join_class_path class_path "/") ^ ".h>\n");
            this#write ("#endif\n")
      )
end;;

let file_source_writer common_ctx filename =
   let out_file = open_out filename in
   new source_writer common_ctx (output_string out_file) (fun ()-> close_out out_file);;


let read_whole_file chan =
   Std.input_all chan;;

(* The cached_source_writer will not write to the file if it has not changed,
   thus allowing the makefile dependencies to work correctly *)
let cached_source_writer common_ctx filename =
   try
      let in_file = open_in filename in
      let old_contents = read_whole_file in_file in
      close_in in_file;
      let buffer = Buffer.create 0 in
      let add_buf str = Buffer.add_string buffer str in
      let close = fun () ->
         let contents = Buffer.contents buffer in
         if (not (contents=old_contents) ) then begin
            let out_file = open_out filename in
            output_string out_file contents;
            close_out out_file;
         end;
      in
      new source_writer common_ctx (add_buf) (close);
   with _ ->
      file_source_writer common_ctx filename;;

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
         make_base_directory dir;
         dir
      end else begin
         make_class_directories base_dir ( sub_dir :: (fst class_path));
         base_dir ^ "/" ^ sub_dir ^ "/" ^ ( String.concat "/" (fst class_path) )
      end
   in
   cached_source_writer common_ctx (full_dir ^ "/" ^ ((snd class_path) ^ extension));;



let source_file_extension common_ctx =
   try
     "." ^ (Common.defined_value common_ctx Define.FileExtension)
   with
     Not_found -> ".cpp"
;;


let new_cpp_file common_ctx base_dir = new_source_file common_ctx base_dir "src" (source_file_extension common_ctx);;

let new_header_file common_ctx base_dir =
   new_source_file common_ctx base_dir "include" ".h";;



(* CPP code generation context *)

type context =
{
   mutable ctx_common : Common.context;
   mutable ctx_output : string -> unit;
   mutable ctx_dbgout : string -> unit;
   mutable ctx_writer : source_writer;
   mutable ctx_calling : bool;
   mutable ctx_assigning : bool;
   mutable ctx_return_from_block : bool;
   mutable ctx_tcall_expand_args : bool;
   (* This is for returning from the child nodes of TMatch, TSwitch && TTry *)
   mutable ctx_return_from_internal_node : bool;
   mutable ctx_debug_level : int;
   mutable ctx_real_this_ptr : bool;
   mutable ctx_real_void : bool;
   mutable ctx_dynamic_this_ptr : bool;
   mutable ctx_dump_src_pos : unit -> unit;
   mutable ctx_static_id_curr : int;
   mutable ctx_static_id_used : int;
   mutable ctx_static_id_depth : int;
   mutable ctx_switch_id : int;
   mutable ctx_class_name : string;
   mutable ctx_class_super_name : string;
   mutable ctx_local_function_args : (string,string) Hashtbl.t;
   mutable ctx_local_return_block_args : (string,string) Hashtbl.t;
   mutable ctx_class_member_types : (string,string) Hashtbl.t;
   mutable ctx_file_info : (string,string) PMap.t ref;
   mutable ctx_for_extern : bool;
}

let new_context common_ctx writer debug file_info =
{
   ctx_common = common_ctx;
   ctx_writer = writer;
   ctx_output = (writer#write);
   ctx_dbgout = if debug>1 then (writer#write) else (fun _ -> ());
   ctx_calling = false;
   ctx_assigning = false;
   ctx_debug_level = debug;
   ctx_dump_src_pos = (fun() -> ());
   ctx_return_from_block = false;
   ctx_tcall_expand_args = false;
   ctx_return_from_internal_node = false;
   ctx_real_this_ptr = true;
   ctx_real_void = false;
   ctx_dynamic_this_ptr = false;
   ctx_static_id_curr = 0;
   ctx_static_id_used = 0;
   ctx_static_id_depth = 0;
   ctx_switch_id = 0;
   ctx_class_name = "";
   ctx_class_super_name = "";
   ctx_local_function_args = Hashtbl.create 0;
   ctx_local_return_block_args = Hashtbl.create 0;
   ctx_class_member_types =  Hashtbl.create 0;
   ctx_file_info = file_info;
   ctx_for_extern = false;
}

let new_extern_context common_ctx writer debug file_info =
   let ctx = new_context common_ctx writer debug file_info in
   ctx.ctx_for_extern <- true;
   ctx
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

let is_scalar typename = match typename with
   | "int" | "unsigned int" | "signed int"
   | "char" | "unsigned char"
   | "short" | "unsigned short"
   | "float" | "double"
   | "bool" -> true
   | _ -> false
;;

let is_block exp = match exp.eexpr with | TBlock _ -> true | _ -> false ;;

let to_block expression =
   if is_block expression then expression else (mk_block expression);;

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

(* Convert function names that can't be written in c++ ... *)
let keyword_remap name =
   match name with
   | "int"
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
   | "struct" -> "_" ^ name
   | "asm" -> "_asm_"
   | x -> x
;;

let remap_class_path class_path =
   (List.map keyword_remap (fst class_path)) , (snd class_path)
;;

let join_class_path_remap path separator =
   match join_class_path (remap_class_path path) separator with
   | "Class" -> "hx::Class"
   | x -> x
;;

let get_meta_string meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String name),_],_) :: _  when k=key-> name
      | _ :: l -> loop l
      in
   loop meta
;;



let get_meta_string_path meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String name),_], pos) :: _  when k=key->
           (try
           if (String.sub name 0 2) = "./" then begin
              let base = if (Filename.is_relative pos.pfile) then
                 Filename.concat (Sys.getcwd()) pos.pfile
              else
                 pos.pfile
              in
              Gencommon.normalize (Filename.concat (Filename.dirname base) (String.sub name 2 ((String.length name) -2)  ))
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
              Gencommon.normalize (Filename.concat (Sys.getcwd()) pos.pfile)
           else
              pos.pfile
      | _ :: l -> loop l
      in
   loop meta
;;

let get_meta_string_full_dirname meta key =
   let name = get_meta_string_full_filename meta key in
   try
      Gencommon.normalize (Filename.dirname name)
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
   List.iter (fun m -> match m with | (k,_,_) -> print_endline ((fst (MetaInfo.to_string k)) ^ "=" ^ (get_meta_string meta k) ) | _ -> () ) meta;;
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


(* This gets the class include order correct.  In the header files, we forward declare
   the class types so the header file does not have any undefined variables.
   In the cpp files, we include all the required header files, providing the actual
   types for everything.  This way there is no problem with circular class references.
*)
let gen_forward_decl writer class_path =
   begin
      let output = writer#write in
      match class_path with
      | (["@verbatim"],file) -> writer#write ("#include <" ^ file ^ ">\n");
      | _ ->
         let name = fst (remap_class_path class_path) in
         output ("HX_DECLARE_CLASS" ^ (string_of_int (List.length name ) ) ^ "(");
         List.iter (fun package_part -> output (package_part ^ ",") ) name;
         output ( (snd class_path) ^ ")\n")
end;;

let real_interfaces =
List.filter (function (t,pl) ->
   match t, pl with
   | { cl_path = ["cpp";"rtti"],_ },[] -> false
   | _ -> true
);;

let rec is_function_expr expr =
   match expr.eexpr with
   | TParenthesis expr | TMeta(_,expr) -> is_function_expr expr
   | TFunction _ -> true
   | _ -> false;;

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
   | "Int" | "Bool" | "Float" |  "::haxe::io::Unsigned_char__" | "unsigned char" -> true
   | "::cpp::UInt8" | "::cpp::Int8" | "::cpp::Char"
   | "::cpp::UInt16" | "::cpp::Int16"
   | "::cpp::UInt32" | "::cpp::Int32"
   | "::cpp::UInt64" | "::cpp::Int64"
   | "::cpp::Float32" | "::cpp::Float64"
   | "int" | "bool" | "double" | "float" -> true
   | _ -> false


let rec remove_parens expression =
      match expression.eexpr with
      | TParenthesis e -> remove_parens e
      | TMeta(_,e) -> remove_parens e
      | _ -> expression
;;


(*
let rec remove_parens_cast expression =
   match expression.eexpr with
   | TParenthesis e -> remove_parens_cast e
   | TMeta(_,e) -> remove_parens_cast e
   | TCast ( e,None) -> remove_parens_cast e
   | _ -> expression
;;
*)
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


let is_cpp_function_class haxe_type =
   match follow haxe_type with
   | TType (klass,params) ->
      (match klass.t_path with
      | ["cpp"] , "Function" -> true
      | _ -> false )
   | _ -> false
   ;;

let is_fromStaticFunction_call func =
   match (remove_parens func).eexpr with
   | TField (_,FStatic ({cl_path=["cpp"],"Function"},{cf_name="fromStaticFunction"} ) ) -> true
   | _ -> false
;;

let is_addressOf_call func =
   match (remove_parens func).eexpr with
   | TField (_,FStatic ({cl_path=["cpp"],"Pointer"},{cf_name="addressOf"} ) ) -> true
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

(*  Get a string to represent a type.
   The "suffix" will be nothing or "_obj", depending if we want the name of the
   pointer class or the pointee (_obj class *)
let rec class_string klass suffix params remap =
   let type_string = type_string_remap remap in
   let join_class_path_remap = if remap then join_class_path_remap else join_class_path in
   (match klass.cl_path with
   (* Array class *)
   |  ([],"Array") when is_dynamic_array_param (List.hd params) ->
           "cpp::ArrayBase" ^ suffix (* "Dynamic" *)
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
   |  (["haxe";"io"],"Unsigned_char__") -> "unsigned char"
   |  ([],"Class") -> "hx::Class"
   |  ([],"EnumValue") -> "Dynamic"
   |  ([],"Null") -> (match params with
         | [t] ->
            (match follow t with
            | TAbstract ({ a_path = [],"Int" },_)
            | TAbstract ({ a_path = [],"Float" },_)
            | TAbstract ({ a_path = [],"Bool" },_)
            | TInst ({ cl_path = [],"Int" },_)
            | TInst ({ cl_path = [],"Float" },_)
            | TEnum ({ e_path = [],"Bool" },_) -> "Dynamic"
            | t when type_has_meta_key t Meta.NotNull -> "Dynamic"
            | _ -> "/*NULL*/" ^ (type_string t) )
         | _ -> assert false);
   (* Normal class *)
   | path when klass.cl_extern && (not (is_internal_class path) )->
            (join_class_path_remap klass.cl_path "::") ^ suffix
   | _ -> "::" ^ (join_class_path_remap klass.cl_path "::") ^ suffix
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
   | TAbstract( { a_path = ([], "EnumValue") }, _  ) -> "Dynamic"
   | TEnum (enum,params) ->  "::" ^ (join_class_path_remap enum.e_path "::") ^ suffix
   | TInst (klass,params) ->  (class_string klass suffix params remap)
   | TType (type_def,params) ->
      (match type_def.t_path with
      | [] , "Null" ->
         (match params with
         | [t] ->
            (match follow t with
            | TAbstract ({ a_path = [],"Int" },_)
            | TAbstract ({ a_path = [],"Float" },_)
            | TAbstract ({ a_path = [],"Bool" },_)
            | TInst ({ cl_path = [],"Int" },_)
            | TInst ({ cl_path = [],"Float" },_)
            | TEnum ({ e_path = [],"Bool" },_) -> "Dynamic" ^ suffix
            | t when type_has_meta_key t Meta.NotNull -> "Dynamic" ^ suffix
            | _ -> type_string_suff suffix t remap)
         | _ -> assert false);
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
   | TLazy func -> type_string_suff suffix ((!func)()) remap
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
   | TFun(args,ret) -> (type_string ret) ^ " " ^ abi ^ "( " ^ (gen_tfun_interface_arg_list args) ^ ")"
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
      type_str )


   ^ " " ^ (keyword_remap name)
and gen_tfun_interface_arg_list args =
   String.concat "," (List.map (fun (name,opt,typ) -> gen_interface_arg_type_name name opt typ) args)
and cant_be_null haxe_type =
   is_numeric (type_string haxe_type) || (type_has_meta_key haxe_type Meta.NotNull )
;;

let is_object type_string =
   not (is_numeric type_string || type_string="::String");
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


let is_numeric_field field =
   match field.cf_kind with
   | Var _ -> is_numeric (type_string field.cf_type)
   | _ -> false;
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


let rec is_cpp_function_member func =
   match (remove_parens func).eexpr with
   | TField(obj,field) when is_cpp_function_instance obj.etype -> true
   | TCall(obj,_) -> is_cpp_function_member obj
   | _ -> false
;;




(* Get the type and output it to the stream *)
let gen_type ctx haxe_type =
   ctx.ctx_output (type_string haxe_type)
;;

let member_type ctx field_object member =
   let name = (if (is_array field_object.etype) then "::Array"
            else (type_string field_object.etype)) ^ "." ^ member in
   try ( Hashtbl.find ctx.ctx_class_member_types name )
   with Not_found -> "?";;

let is_interface obj = is_interface_type obj.etype;;

let should_implement_field x = not (is_extern_field x);;

let is_function_member expression =
   match (follow expression.etype) with | TFun (_,_) -> true | _ -> false;;

let is_internal_member member =
   match member with
   | "__Field" | "__IField" | "__Run" | "__Is" | "__GetClass" | "__GetType" | "__ToString"
   | "__s" | "__GetPtr" | "__SetField" | "__length" | "__IsArray" | "__SetThis" | "__Internal"
   | "__EnumParams" | "__Index" | "__Tag" | "__GetFields" | "toString" | "__HasField"
   | "__GetRealObject"
         -> true
   | _ -> false;;


let is_extern_class class_def =
   class_def.cl_extern || (has_meta_key class_def.cl_meta Meta.Extern) ||
      (match class_def.cl_kind with
       | KAbstractImpl abstract_def -> (has_meta_key abstract_def.a_meta Meta.Extern)
       | _ -> false );
;;


let is_native_gen_class class_def =
   (has_meta_key class_def.cl_meta Meta.NativeGen) ||
      (match class_def.cl_kind with
       | KAbstractImpl abstract_def -> (has_meta_key abstract_def.a_meta Meta.NativeGen)
       | _ -> false );
;;


let is_extern_class_instance obj =
   match follow obj.etype with
   | TInst (klass,params) -> klass.cl_extern
   | _ -> false
;;


let is_struct_access t =
   match follow t with
   | TInst (class_def,_) -> (has_meta_key class_def.cl_meta Meta.StructAccess)
   | _ -> false
;;



let rec is_dynamic_accessor name acc field class_def =
 ( ( acc ^ "_" ^ field.cf_name) = name ) &&
   ( not (List.exists (fun f -> f.cf_name=name) class_def.cl_ordered_fields) )
   && (match class_def.cl_super with None -> true | Some (parent,_) -> is_dynamic_accessor name acc field parent )
;;


let gen_arg_type_name name default_val arg_type prefix =
   let remap_name = keyword_remap name in
   let type_str = (type_string arg_type) in
   match default_val with
   | Some TNull  -> (type_str,remap_name)
   | Some constant when (cant_be_null arg_type) -> ("hx::Null< " ^ type_str ^ " > ",prefix ^ remap_name)
   | Some constant  -> (type_str,prefix ^ remap_name)
   | _ -> (type_str,remap_name);;

(* Generate prototype text, including allowing default values to be null *)
let gen_arg name default_val arg_type prefix =
   let pair = gen_arg_type_name name default_val arg_type prefix in
   (fst pair) ^ " " ^ (snd pair);;

let rec gen_arg_list arg_list prefix =
   String.concat "," (List.map (fun (v,o) -> (gen_arg v.v_name o v.v_type prefix) ) arg_list)


let rec gen_tfun_arg_list arg_list =
   match arg_list with
   | [] -> ""
   | [(name,o,arg_type)] -> gen_arg name None arg_type ""
   | (name,o,arg_type) :: remaining  ->
      (gen_arg name None arg_type "") ^ "," ^ (gen_tfun_arg_list remaining)

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

let gen_string_hash str =
   let h = gen_hash32 0 str in
   Printf.sprintf "\"\\x%02lx\",\"\\x%02lx\",\"\\x%02lx\",\"\\x%02lx\""
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

let str s =
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
      "HX_HCSTRING(\"" ^ hexed ^ "\"," ^ (gen_string_hash s) ^ ")"
   else
      "(" ^ (split s "" ) ^ ")"
;;

let const_char_star s =
   let escaped = Ast.s_escape ~hex:false s in
   "\"" ^ special_to_hex escaped ^ "\"";
;;



(* When we are in a "real" object, we refer to ourselves as "this", but
   if we are in a local class that is used to generate return values,
   we use the fake "__this" pointer.
   If we are in an "Anon" object, then the "this" refers to the anon object (eg List iterator) *)
let clear_real_this_ptr ctx dynamic_this =
   let old_flag = ctx.ctx_real_this_ptr in
   let old_dynamic = ctx.ctx_dynamic_this_ptr in
   let old_void = ctx.ctx_real_void in
   ctx.ctx_real_this_ptr <- false;
   ctx.ctx_dynamic_this_ptr <- dynamic_this;
   fun () -> (
      ctx.ctx_real_this_ptr <- old_flag;
      ctx.ctx_dynamic_this_ptr <- old_dynamic;
      ctx.ctx_real_void <- old_void;
      )
;;


(* Generate temp variable names *)
let next_anon_function_name ctx =
   ctx.ctx_static_id_curr <- ctx.ctx_static_id_curr + 1;
   "_Function_" ^ (string_of_int ctx.ctx_static_id_depth) ^"_"^ (string_of_int ctx.ctx_static_id_curr);;

let use_anon_function_name ctx =
   ctx.ctx_static_id_used <- ctx.ctx_static_id_used + 1;
   "_Function_" ^ (string_of_int ctx.ctx_static_id_depth) ^"_"^ (string_of_int ctx.ctx_static_id_used);;

let push_anon_names ctx =
   let old_used = ctx.ctx_static_id_used in
   let old_curr = ctx.ctx_static_id_curr in
   let old_depth = ctx.ctx_static_id_depth in
   ctx.ctx_static_id_used <- 0;
   ctx.ctx_static_id_curr <- 0;
   ctx.ctx_static_id_depth <- ctx.ctx_static_id_depth + 1;
   ( function () -> (
      ctx.ctx_static_id_used <- old_used;
      ctx.ctx_static_id_curr <- old_curr;
      ctx.ctx_static_id_depth <- old_depth; ) )
;;

let get_switch_var ctx =
   ctx.ctx_switch_id <- ctx.ctx_switch_id + 1;
   "_switch_" ^ (string_of_int ctx.ctx_switch_id)


(* If you put on the "-debug" flag, you get extra comments in the source code *)
let debug_expression expression type_too =
   "/* " ^ Type.s_expr_kind expression ^ (if (type_too) then " = " ^ (type_string expression.etype) else "") ^ " */";;

(* This is like the Type.iter, but also keeps the "retval" flag up to date *)
let rec iter_retval f retval e =
   match e.eexpr with
   | TConst _
   | TLocal _
   | TBreak
   | TContinue
   | TTypeExpr _ ->
      ()
   | TArray (e1,e2)
   | TBinop (_,e1,e2) ->
      f true e1;
      f true e2;
   | TWhile (e1,e2,_) ->
      f true e1;
      f false e2;
   | TFor (_,e1,e2) ->
      f true e1;
      f false e2;
   | TThrow e
   | TField (e,_)
   | TEnumParameter (e,_,_)
   | TUnop (_,_,e) ->
      f true e
   | TParenthesis e | TMeta(_,e) ->
      f retval e
   | TBlock expr_list when retval ->
      let rec return_last = function
         | [] -> ()
         | expr :: [] -> f true expr
         | expr :: exprs -> f false expr; return_last exprs in
      return_last expr_list
   | TArrayDecl el
   | TNew (_,_,el) ->
      List.iter (f true ) el
   | TBlock el ->
      List.iter (f false ) el
   | TObjectDecl fl ->
      List.iter (fun (_,e) -> f true e) fl
   | TCall (e,el) ->
      f true e;
      List.iter (f true) el
   | TVar (_,eo) ->
      (match eo with None -> () | Some e -> f true e)
   | TFunction fu ->
      f false fu.tf_expr
   | TIf (e,e1,e2) ->
      f true e;
      f retval e1;
      (match e2 with None -> () | Some e -> f retval e)
   | TSwitch (e,cases,def) ->
      f true e;
      List.iter (fun (el,e2) -> List.iter (f true) el; f retval e2) cases;
      (match def with None -> () | Some e -> f retval e)
(*    | TMatch (e,_,cases,def) ->
      f true e;
      List.iter (fun (_,_,e) -> f false e) cases;
      (match def with None -> () | Some e -> f false e) *)
   | TTry (e,catches) ->
      f retval e;
      List.iter (fun (_,e) -> f false e) catches
   | TReturn eo ->
      (match eo with None -> () | Some e -> f true e)
   | TCast (e,None) ->
      f retval e
   | TCast (e,_) ->
      f true e
;;


(* Convert an array to a comma separated list of values *)
let array_arg_list inList =
   let i = ref (0-1) in
   String.concat "," (List.map (fun _ -> incr i; "inArgs[" ^ (string_of_int !i) ^ "]"  ) inList)

let list_num l = string_of_int (List.length l);;


let only_int_cases cases =
   match cases with
   | [] -> false
   | _ ->
   not (List.exists (fun (cases,expression) ->
         List.exists (fun case -> match case.eexpr with TConst (TInt _) -> false | _ -> true ) cases
            ) cases );;

(* See if there is a haxe break statement that will be swollowed by c++ break *)
exception BreakFound;;

let contains_break expression =
   try (
   let rec check_all expression =
      Type.iter (fun expr -> match expr.eexpr with
         | TBreak -> raise BreakFound
         | TFor _
         | TFunction _
         | TWhile (_,_,_) -> ()
         | _ -> check_all expr;
         ) expression in
   check_all expression;
   false;
   ) with BreakFound -> true;;


(* Decide is we should look the field up by name *)
let dynamic_internal = function | "__Is" -> true | _ -> false


let rec is_null expr =
   match expr.eexpr with
   | TConst TNull -> true
   | TParenthesis expr | TMeta (_,expr) -> is_null expr
   | TCast (e,None) -> is_null e
   | _ -> false
;;


let find_undeclared_variables_ctx ctx undeclared declarations this_suffix allow_this expression =
   let output = ctx.ctx_output in
   let rec find_undeclared_variables undeclared declarations this_suffix allow_this expression =
      match expression.eexpr with
      | TVar (tvar,optional_init) ->
            Hashtbl.add declarations (keyword_remap tvar.v_name) ();
            if (ctx.ctx_debug_level>1) then
               output ("/* found var " ^ tvar.v_name ^ "*/ ");
            (match optional_init with
            | Some expression -> find_undeclared_variables undeclared declarations this_suffix allow_this expression
            | _ -> ())
      | TFunction func -> List.iter ( fun (tvar, opt_val) ->
            if (ctx.ctx_debug_level>1) then
               output ("/* found arg " ^ tvar.v_name ^ " = " ^ (type_string tvar.v_type) ^ " */ ");
            Hashtbl.add declarations (keyword_remap tvar.v_name) () ) func.tf_args;
            find_undeclared_variables undeclared declarations this_suffix false func.tf_expr
      | TTry (try_block,catches) ->
         find_undeclared_variables undeclared declarations this_suffix allow_this try_block;
         List.iter (fun (tvar,catch_expt) ->
            let old_decs = Hashtbl.copy declarations in
            Hashtbl.add declarations (keyword_remap tvar.v_name) ();
            find_undeclared_variables undeclared declarations this_suffix allow_this catch_expt;
            Hashtbl.clear declarations;
            Hashtbl.iter ( Hashtbl.add declarations ) old_decs
            ) catches;
      | TLocal tvar ->
         let name = keyword_remap tvar.v_name in
         if  not (Hashtbl.mem declarations name) then
            Hashtbl.replace undeclared name (type_string expression.etype)
(*       | TMatch (condition, enum, cases, default) ->
         find_undeclared_variables undeclared declarations this_suffix allow_this condition;
         List.iter (fun (case_ids,params,expression) ->
            let old_decs = Hashtbl.copy declarations in
            (match params with
            | None -> ()
            | Some l -> List.iter (fun (opt_var) ->
               match opt_var with | Some v -> Hashtbl.add declarations (keyword_remap v.v_name) () | _ -> ()  )
               l  );
            find_undeclared_variables undeclared declarations this_suffix allow_this expression;
            Hashtbl.clear declarations;
            Hashtbl.iter ( Hashtbl.add declarations ) old_decs
            ) cases;
         (match default with | None -> ()
         | Some expr ->
            find_undeclared_variables undeclared declarations this_suffix allow_this expr;
         ); *)
      | TFor (tvar, init, loop) ->
         let old_decs = Hashtbl.copy declarations in
         Hashtbl.add declarations (keyword_remap tvar.v_name) ();
         find_undeclared_variables undeclared declarations this_suffix allow_this init;
         find_undeclared_variables undeclared declarations this_suffix allow_this loop;
         Hashtbl.clear declarations;
         Hashtbl.iter ( Hashtbl.add declarations ) old_decs
      | TConst TSuper
      | TConst TThis ->
         if  ((not (Hashtbl.mem declarations "this")) && allow_this) then
            Hashtbl.replace undeclared "this" (type_string_suff this_suffix expression.etype true)
      | TBlock expr_list ->
         let old_decs = Hashtbl.copy declarations in
         List.iter (find_undeclared_variables undeclared declarations this_suffix allow_this ) expr_list;
         (* what is the best way for this ? *)
         Hashtbl.clear declarations;
         Hashtbl.iter ( Hashtbl.add declarations ) old_decs
      | _ -> Type.iter (find_undeclared_variables undeclared declarations this_suffix allow_this) expression
   in
   find_undeclared_variables undeclared declarations this_suffix allow_this expression
;;


let rec is_dynamic_in_cpp ctx expr =
   let expr_type = type_string ( match follow expr.etype with TFun (args,ret) -> ret | _ -> expr.etype) in
   ctx.ctx_dbgout ( "/* idic: " ^ expr_type ^ " */" );
   if ( expr_type="Dynamic" || expr_type="cpp::ArrayBase") then
      true
   else begin
      let result = (
      match expr.eexpr with
       | TEnumParameter( obj, _, index ) ->
         true (* TODO? *)
      | TField( obj, field ) ->
         let name = field_name field in
         ctx.ctx_dbgout ("/* ?tfield "^name^" */");
            if (is_dynamic_member_lookup_in_cpp ctx obj field) then
            (
               ctx.ctx_dbgout "/* tf=dynobj */";
               true
            )
            else if (is_dynamic_member_return_in_cpp ctx obj field)  then
            (
               ctx.ctx_dbgout "/* tf=dynret */";
               true
            )
            else
            (
               ctx.ctx_dbgout "/* tf=notdyn */";
               false
            )
      | TConst TThis when ((not ctx.ctx_real_this_ptr) && ctx.ctx_dynamic_this_ptr) ->
            ctx.ctx_dbgout ("/* dthis */"); true
      | TArray (obj,index) -> let dyn = is_dynamic_in_cpp ctx obj in
            ctx.ctx_dbgout ("/* aidr:" ^ (if dyn then "Dyn" else "Not") ^ " */");
            dyn;
      | TTypeExpr _ -> false
      | TCall(func,args) ->
               (match follow func.etype with
               | TFun (args,ret) -> ctx.ctx_dbgout ("/* ret = "^ (type_string ret) ^" */");
                  is_dynamic_in_cpp ctx func
               | _ -> ctx.ctx_dbgout "/* not TFun */";  true
         );
      | TParenthesis(expr) | TMeta(_,expr) -> is_dynamic_in_cpp ctx expr
      | TCast (e,None) -> (type_string expr.etype) = "Dynamic"
      | TLocal { v_name = "__global__" } -> false
      | TConst TNull -> true
      | _ -> ctx.ctx_dbgout "/* other */";  false (* others ? *) )
      in
      ctx.ctx_dbgout (if result then "/* Y */" else "/* N */" );
      result
   end

and is_dynamic_member_lookup_in_cpp ctx field_object field =
   let member = field_name field in
   ctx.ctx_dbgout ("/*mem."^member^".*/");
   if (is_internal_member member) then false else
   if (is_pointer field_object.etype true) then false else
   if (match field_object.eexpr with | TTypeExpr _ -> ctx.ctx_dbgout "/*!TTypeExpr*/"; true | _ -> false) then false else
   if (is_dynamic_in_cpp ctx field_object) then true else
   if (is_array field_object.etype) then false else (
   let tstr = type_string field_object.etype in
   ctx.ctx_dbgout ("/* ts:"^tstr^"*/");
   match tstr with
      (* Internal classes have no dynamic members *)
      | "::String" | "Null" | "::hx::Class" | "::Enum" | "::Math" | "::ArrayAccess" -> ctx.ctx_dbgout ("/* ok:" ^ (type_string field_object.etype)  ^ " */"); false
      | "Dynamic" -> true
      | name ->
            let full_name = name ^ "." ^ member in
            ctx.ctx_dbgout ("/* t:" ^ full_name ^ " */");
            try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in
               ctx.ctx_dbgout ("/* =" ^ mem_type ^ "*/");
               false )
            with Not_found -> not (is_extern_class_instance field_object)
   )
and is_dynamic_member_return_in_cpp ctx field_object field =
   let member = field_name field in
   if (is_array field_object.etype) then false else
   if (is_pointer field_object.etype true) then false else
   if (is_internal_member member) then false else
   match field_object.eexpr with
   | TTypeExpr t ->
         let full_name = "::" ^ (join_class_path (t_path t) "::" ) ^ "." ^ member in
         ctx.ctx_dbgout ("/*static:"^ full_name^"*/");
         ( try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in mem_type="Dynamic"||mem_type="cpp::ArrayBase" )
         with Not_found -> true )
   | _ ->
      let tstr = type_string field_object.etype in
      (match tstr with
         (* Internal classes have no dynamic members *)
         | "::String" | "Null" | "::hx::Class" | "::Enum" | "::Math" | "::ArrayAccess" -> false
         | "Dynamic" | "cpp::ArrayBase" -> ctx.ctx_dbgout "/*D*/"; true
         | name ->
               let full_name = name ^ "." ^ member in
               ctx.ctx_dbgout ("/*R:"^full_name^"*/");
               try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in mem_type="Dynamic"||mem_type="cpp::ArrayBase" )
               with Not_found -> true )
;;

let cast_if_required ctx expr to_type =
   let expr_type = (type_string expr.etype) in
   ctx.ctx_dbgout ( "/* cir: " ^ expr_type ^ " */" );
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

let generate_default_values ctx args prefix =
   List.iter ( fun (v,o) -> let type_str = type_string v.v_type in
   let name = (keyword_remap v.v_name) in
   match o with
   | Some TNull -> ()
   | Some const ->
      ctx.ctx_output (type_str ^ " " ^ name ^ " = " ^ prefix ^ name ^ ".Default(" ^
         (default_value_string const) ^ ");\n")
   | _ -> () ) args;;

let return_type_string t =
   match t with
   |  TFun (_,ret) -> type_string ret
   | _ -> ""
;;


let get_return_type field =
   match follow field.cf_type with
      | TFun (_,return_type) -> return_type
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

let hx_stack_push ctx output clazz func_name pos =
   if ctx.ctx_debug_level > 0 then begin
      let stripped_file = strip_file ctx.ctx_common pos.pfile in
      let esc_file = (Ast.s_escape stripped_file) in
      ctx.ctx_file_info := PMap.add stripped_file pos.pfile !(ctx.ctx_file_info);
      if (ctx.ctx_debug_level>0) then begin
         let full_name = clazz ^ "." ^ func_name ^ (
           if (clazz="*") then
             (" (" ^ esc_file ^ ":" ^ (string_of_int (Lexer.get_error_line pos) ) ^ ")")
           else "") in
         let hash_class_func = gen_hash 0 (clazz^"."^func_name) in
         let hash_file = gen_hash 0 stripped_file in
         output ("HX_STACK_FRAME(\"" ^ clazz ^ "\",\"" ^ func_name ^ "\"," ^ hash_class_func ^ ",\"" ^
               full_name ^ "\",\"" ^ esc_file ^ "\"," ^
               (string_of_int (Lexer.get_error_line pos) ) ^  "," ^ hash_file ^ ")\n")
      end
   end
;;


(*
   This is the big one.
   Once you get inside a function, all code is generated (recursively) as a "expression".
   "retval" is tracked to determine whether the value on an expression is actually used.
   eg, if the result of a block (ie, the last expression in the list) is used, then
   we have to do some funky stuff to generate a local function.
   Some things that change less often are stored in the context and are extracted
   at the top for simplicity.
*)

let gen_expression_tree ctx retval expression_tree set_var tail_code =
 let writer = ctx.ctx_writer in
 let output_i = writer#write_i in
 let output = ctx.ctx_output in

 let rec define_local_function_ctx func_name func_def =
   let remap_this = function | "this" -> "__this" | other -> other in
   let rec define_local_function func_name func_def =
      let declarations = Hashtbl.create 0 in
      let undeclared = Hashtbl.create 0 in
      (* '__global__', '__cpp__' are always defined *)
      Hashtbl.add declarations "__global__" ();
      Hashtbl.add declarations "__cpp__" ();
      Hashtbl.add declarations "__trace" ();
      (* Add args as defined variables *)
      List.iter ( fun (arg_var, opt_val) ->
         if (ctx.ctx_debug_level>1) then
            output ("/* found arg " ^ arg_var.v_name ^ " = " ^ (type_string arg_var.v_type) ^" */ ");
         Hashtbl.add declarations (keyword_remap arg_var.v_name) () ) func_def.tf_args;
      find_undeclared_variables_ctx ctx undeclared declarations "" true func_def.tf_expr;

      let has_this = Hashtbl.mem undeclared "this" in
      if (has_this) then Hashtbl.remove undeclared "this";
      let typed_vars = hash_iterate undeclared (fun key value -> value ^ "," ^ (keyword_remap key) ) in
      let func_name_sep = func_name ^ (if List.length typed_vars > 0 then "," else "") in
      output_i ("HX_BEGIN_LOCAL_FUNC_S" ^ (list_num typed_vars) ^ "(" ^
         (if has_this then "hx::LocalThisFunc," else "hx::LocalFunc,") ^ func_name_sep ^
                  (String.concat "," typed_vars) ^ ")\n" );
      output_i ("int __ArgCount() const { return " ^ (string_of_int (List.length func_def.tf_args)) ^"; }\n");

      (* actual function, called "run" *)
      let args_and_types = List.map
            (fun (v,_) -> (type_string v.v_type) ^ " " ^ (keyword_remap v.v_name) ) func_def.tf_args in
      let block = is_block func_def.tf_expr in
      let func_type = type_string func_def.tf_type in
      output_i (func_type ^ " run(" ^ (gen_arg_list func_def.tf_args "__o_") ^ ")");

      let close_defaults =
         if (has_default_values func_def.tf_args) then begin
            writer#begin_block;
            output_i "";
            generate_default_values ctx func_def.tf_args "__o_";
            output_i "";
            true;
         end
      else
         false in


      let pop_real_this_ptr = clear_real_this_ptr ctx true in

      writer#begin_block;
      if (ctx.ctx_debug_level>0) then begin
         hx_stack_push ctx output_i "*" func_name func_def.tf_expr.epos;
         if (has_this && ctx.ctx_debug_level>0) then
            output_i ("HX_STACK_THIS(__this.mPtr)\n");
            List.iter (fun (v,_) -> output_i ("HX_STACK_ARG(" ^ (keyword_remap v.v_name) ^ ",\"" ^ v.v_name ^"\")\n") )
            func_def.tf_args;
      end;

      if (block) then begin
         output_i "";
         gen_expression false func_def.tf_expr;
         output_i "return null();\n";
      end else begin
         (* Save old values, and equalize for new input ... *)
         let pop_names = push_anon_names ctx in

         find_local_functions_and_return_blocks_ctx false func_def.tf_expr;

         (match func_def.tf_expr.eexpr with
         | TReturn (Some return_expression) when (func_type<>"Void") ->
            output_i "return ";
            gen_expression true return_expression;
         | TReturn (Some return_expression) ->
            output_i "";
            gen_expression false return_expression;
         | _ ->
            output_i "";
            gen_expression false (to_block func_def.tf_expr);
         );
         output ";\n";
         output_i "return null();\n";
         pop_names();
      end;
      writer#end_block;

      if close_defaults then writer#end_block;
      pop_real_this_ptr();

      let return = if (type_string func_def.tf_type ) = "Void" then "(void)" else "return" in
      output_i ("HX_END_LOCAL_FUNC" ^ (list_num args_and_types) ^ "(" ^ return ^ ")\n\n");

      Hashtbl.replace ctx.ctx_local_function_args func_name
         (if (ctx.ctx_real_this_ptr) then
            String.concat "," (hash_keys undeclared)
         else
            String.concat "," (List.map remap_this (hash_keys undeclared)) )
   in
   define_local_function func_name func_def

 and find_local_functions_and_return_blocks_ctx retval expression =
   let rec find_local_functions_and_return_blocks retval expression =
      match expression.eexpr with
      | TBlock _ ->
         if (retval) then begin
            define_local_return_block_ctx expression (next_anon_function_name ctx) true;
         end  (* else we are done *)
      | TTry (_, _)
      | TSwitch (_, _, _) when retval ->
            define_local_return_block_ctx expression (next_anon_function_name ctx) true;
      | TObjectDecl ( ("fileName" , { eexpr = (TConst (TString file)) }) ::
         ("lineNumber" , { eexpr = (TConst (TInt line)) }) ::
            ("className" , { eexpr = (TConst (TString class_name)) }) ::
               ("methodName", { eexpr = (TConst (TString meth)) }) :: [] ) -> ()
      | TObjectDecl decl_list ->
            let name = next_anon_function_name ctx in
            define_local_return_block_ctx expression name true;
      | TFunction func ->
         let func_name = next_anon_function_name ctx in
         output "\n";
         define_local_function_ctx func_name func
      | TField (obj,_) | TEnumParameter (obj,_,_) when (is_null obj) -> ( )
      | TArray (obj,_) when (is_null obj) -> ( )
      | TIf ( _ , _ , _ ) when retval -> (* ? operator style *)
         iter_retval find_local_functions_and_return_blocks retval expression
      | TSwitch (_, _, _) when retval -> ( )
      (* | TMatch ( cond , _, _, _) *)
      | TWhile ( cond , _, _ )
      | TIf ( cond , _, _ )
      | TSwitch ( cond , _, _) -> iter_retval find_local_functions_and_return_blocks true cond
      | _ -> iter_retval find_local_functions_and_return_blocks retval expression
   in find_local_functions_and_return_blocks retval expression

 and define_local_return_block_ctx expression name retval =
   let check_this = function | "this" when not ctx.ctx_real_this_ptr -> "__this" | x -> x in
   let rec define_local_return_block expression  =
      let declarations = Hashtbl.create 0 in
      let undeclared = Hashtbl.create 0 in
      (* '__global__' is always defined *)
      Hashtbl.add declarations "__global__" ();
      Hashtbl.add declarations "__cpp__" ();
      Hashtbl.add declarations "__trace" ();
      find_undeclared_variables_ctx ctx undeclared declarations "_obj" true expression;

      let vars = (hash_keys undeclared) in
      let args = String.concat "," (List.map check_this (hash_keys undeclared)) in
      Hashtbl.replace ctx.ctx_local_return_block_args name args;
      output_i ("struct " ^ name);
      writer#begin_block;
      let ret_type = if (not retval) then "Void" else
         match expression.eexpr with
         | TObjectDecl _ -> "Dynamic"
         | _ -> type_string expression.etype in
      (* TODO - analyse usage *)
      let pass_by_value name = (String.length name >=5 ) && (String.sub name 0 5 = "_this") in
      output_i ("inline static " ^ ret_type ^ " Block( ");
      output (String.concat "," (
         (List.map
            (fun var ->
               let var_type = Hashtbl.find undeclared var in
               (* Args passed into inline-block should be references, so they can be changed.
                  Fake 'this' pointers can't be changed, so needn't be references *)
               match var with
               | "this" -> "hx::ObjectPtr< " ^ var_type ^ " > __this"
               | name when (pass_by_value name)  -> var_type ^ " " ^ name
               | name -> var_type ^ " &" ^name
            ) vars) ) );
      output (")");
      let return_data = ret_type <> "Void" in
      writer#begin_block;
      hx_stack_push ctx output_i "*" "closure" expression.epos;
      output_i "";

      let pop_real_this_ptr = clear_real_this_ptr ctx false in
      (match expression.eexpr with
      | TObjectDecl decl_list ->
         writer#begin_block;
         output_i "hx::Anon __result = hx::Anon_obj::Create();\n";
         let pop_names = push_anon_names ctx in
         List.iter (function (name,value) ->
            find_local_functions_and_return_blocks_ctx true value;
            output_i ( "__result->Add(" ^ (str name) ^ " , ");
            gen_expression true value;
            output (if is_function_expr value then ",true" else ",false" );
            output (");\n");
         ) decl_list;
         pop_names();
         output_i "return __result;\n";
         writer#end_block;
      | TBlock _ ->
         ctx.ctx_return_from_block <- return_data;
         ctx.ctx_return_from_internal_node <- false;
         gen_expression false expression;
      | TCall(func,args) ->
         writer#begin_block;
         let pop_names = push_anon_names ctx in
         find_local_functions_and_return_blocks_ctx true func;
         List.iter (find_local_functions_and_return_blocks_ctx true) args;
         ctx.ctx_tcall_expand_args <- true;
         gen_expression return_data expression;
         output ";\n";
         pop_names();
         writer#end_block;
      | _ ->
         ctx.ctx_return_from_block <- false;
         ctx.ctx_return_from_internal_node <- return_data;
         gen_expression false (to_block expression);
      );
      output_i "return null();\n";
      writer#end_block;
      pop_real_this_ptr();
      writer#end_block_line;
      output ";\n";
   in
   define_local_return_block expression


 and gen_expression retval expression =
   let calling = ctx.ctx_calling in
   ctx.ctx_calling <- false;
   let assigning = ctx.ctx_assigning in
   ctx.ctx_assigning <- false;
   let return_from_block = ctx.ctx_return_from_block in
   ctx.ctx_return_from_block <- false;
   let tcall_expand_args = ctx.ctx_tcall_expand_args in
   ctx.ctx_tcall_expand_args <- false;
   let return_from_internal_node = ctx.ctx_return_from_internal_node in
   ctx.ctx_return_from_internal_node <- false;
   let dump_src_pos = ctx.ctx_dump_src_pos in
   ctx.ctx_dump_src_pos <- (fun() -> ());

   (* Annotate source code with debug - can get a bit verbose.  Mainly for debugging code gen,
      rather than the run time *)
   if (ctx.ctx_debug_level>1) then begin
      (*if calling then output "/* Call */";*)
      (*if ctx.ctx_real_this_ptr then output "/* this */" else output "/* FAKE __this */";*)
      output (debug_expression expression (ctx.ctx_debug_level>1) );
   end;

   (* Write comma separated list of variables - useful for function args. *)
   let rec gen_expression_list expressions =
      (match expressions with
      | [] -> ()
      | [single] -> gen_expression true single
      | first :: remaining ->
         gen_expression true first;
         output ",";
         gen_expression_list remaining
      ) in

   let rec gen_bin_op_string expr1 op expr2 =
      let cast = (match op with
         | ">>" | "<<" | "&" | "|" | "^"  -> "int("
         | "&&" | "||" -> "bool("
         | "/" -> "Float("
         | _ -> "") in
      if (op <> "=") then output "(";
      if ( cast <> "") then output cast;
      gen_expression true expr1;
      if ( cast <> "") then output ")";

      output (" " ^ op ^ " ");

      if ( cast <> "") then output cast;
      gen_expression true expr2;
      if ( cast <> "") then output ")";
      if (op <> "=") then output ")";
   in
   let rec is_const_string_term expr =
      match expr.eexpr with
      | TConst( TString _ ) -> true
      | TBinop (OpAdd,e1,e2) -> (is_const_string_term e1) && (is_const_string_term e2 )
      | _ -> false
   in
   let rec combine_string_terms expr =
      match expr.eexpr with
      | TConst( TString s ) -> s
      | TBinop (OpAdd,e1,e2) -> (combine_string_terms e1) ^ (combine_string_terms e2 )
      | _ -> ""
   in
   let rec gen_bin_op op expr1 expr2 =
      match op with
      | Ast.OpAdd when (is_const_string_term expr1) && (is_const_string_term expr2) ->
         output (str ((combine_string_terms expr1) ^ (combine_string_terms expr2)) )
      | Ast.OpAssign -> ctx.ctx_assigning <- true;
                        gen_bin_op_string expr1 "=" expr2
      | Ast.OpUShr ->
         output "hx::UShr(";
         gen_expression true expr1;
         output ",";
         gen_expression true expr2;
         output ")";
      | Ast.OpMod ->
         output "hx::Mod(";
         gen_expression true expr1;
         output ",";
         gen_expression true expr2;
         output ")";

      | Ast.OpAssignOp bin_op ->
         output (match bin_op with
            | Ast.OpAdd -> "hx::AddEq("
            | Ast.OpMult -> "hx::MultEq("
            | Ast.OpDiv -> "hx::DivEq("
            | Ast.OpSub -> "hx::SubEq("
            | Ast.OpAnd -> "hx::AndEq("
            | Ast.OpOr  -> "hx::OrEq("
            | Ast.OpXor  -> "hx::XorEq("
            | Ast.OpShl  -> "hx::ShlEq("
            | Ast.OpShr  -> "hx::ShrEq("
            | Ast.OpUShr  -> "hx::UShrEq("
            | Ast.OpMod  -> "hx::ModEq("
            | _ -> error "Unknown OpAssignOp" expression.epos );
         ctx.ctx_assigning <- true;
         gen_expression true expr1;
         output ",";
         gen_expression true expr2;
         output ")"
      | Ast.OpNotEq -> gen_bin_op_string expr1 "!=" expr2
      | Ast.OpEq -> gen_bin_op_string expr1 "==" expr2
      | _ ->  gen_bin_op_string expr1 (Ast.s_binop op) expr2
      in

   let gen_array_cast cast_name real_type call =
   output (cast_name ^ "< " ^ real_type ^ " >" ^ call)
   in
   let rec check_array_element_cast array_type cast_name call =
      match follow array_type with
      | TInst (klass,[element]) ->
         ( match type_string element with
         | _ when is_struct_access element -> ()
         | x when cant_be_null element -> ()
         | _ when is_interface_type element -> ()
         | "::String" | "Dynamic" -> ()
         | real_type -> gen_array_cast cast_name real_type call
         )
      | TAbstract (abs,pl) when abs.a_impl <> None ->
         check_array_element_cast (Abstract.get_underlying_type abs pl) cast_name call
      | _ -> ()
   in
   let rec check_array_cast array_type =
      match follow array_type with
      | x when is_interface_type x -> ()
      | TInst (klass,[element]) ->
         let name = type_string element in
         if ( is_object name && not (is_interface_type element) ) then
            gen_array_cast ".StaticCast" "Array<Dynamic>" "()"
         else
            gen_array_cast ".StaticCast" (type_string array_type) "()"
      | TAbstract (abs,pl) when abs.a_impl <> None ->
         check_array_cast (Abstract.get_underlying_type abs pl)
      | _ -> ()
   in

   let rec gen_tfield field_object field =
      let member = (field_name field) in
      let remap_name = keyword_remap member in
      let already_dynamic = ref false in
      (match field_object.eexpr with
      (* static access ... *)
      | TTypeExpr type_def ->
         (match get_field_access_meta field Meta.Native with
         | "" ->
            let class_name = "::" ^ (join_class_path_remap (t_path type_def) "::" ) in
            if (class_name="::String") then
               output ("::String::" ^ remap_name)
            else
               output (class_name ^ "_obj::" ^ remap_name);
         | native -> output native
         )
      (* Special internal access *)
      | TLocal { v_name = "__global__" } ->
         output ("::" ^ member )
      | TConst TSuper -> output (if ctx.ctx_real_this_ptr then "this" else "__this");
                  output ("->super::" ^ remap_name)
      | TConst TThis when ctx.ctx_real_this_ptr -> output ( "this->" ^ remap_name )
      | TConst TNull -> output "null()"
      | _ ->
         gen_expression true field_object;
         ctx.ctx_dbgout "/* TField */";
         (* toString is the only internal member that can be set... *)
         let settingInternal = assigning && member="toString" in
         let isString = (type_string field_object.etype)="::String" in
         if (is_struct_access field_object.etype) then
            output ( "." ^ member )
         else if (is_internal_member member && not settingInternal) then begin
            output ( (if isString then "." else "->") ^ member );
         end else if (settingInternal || is_dynamic_member_lookup_in_cpp ctx field_object field) then begin
            if assigning then
               output ( "->__FieldRef(" ^ (str member) ^ ")" )
            else
               output ( "->__Field(" ^ (str member) ^ ", hx::paccDynamic )" );
            already_dynamic := true;
         end else begin
            if (isString) then
               output ( "." ^ remap_name )
            else begin
               cast_if_required ctx field_object (type_string field_object.etype);
               let remap_name = if (type_string field_object.etype)="cpp::ArrayBase" then
                   match remap_name with
                   | "length" -> remap_name
                   | _ -> "__" ^ remap_name
               else
                  remap_name
               in
               output ( "->" ^ remap_name );
               if (calling && (is_array field_object.etype) && remap_name="iterator" ) then
                  check_array_element_cast field_object.etype "Fast" "";

               already_dynamic := (match field with
                  | FInstance(_,_,var) when is_var_field var -> true
                  | _ -> false);
            end;
         end;
      );
      if ( (not !already_dynamic) && (not calling) && (not assigning) && (is_function_member expression) ) then
         output "_dyn()";
   in
   let gen_local_block_call () =
      let func_name = use_anon_function_name ctx in (
         try
         output ( func_name ^ "::Block(" ^
            (Hashtbl.find ctx.ctx_local_return_block_args func_name) ^ ")" )
         with Not_found ->
         (*error ("Block function " ^ func_name ^ " not found" ) expression.epos;*)
         output ("/* Block function " ^ func_name ^ " not found */" );
         )
   in

   match expression.eexpr with
   | TConst TNull when not retval ->
      output "Dynamic()";
   | TCall (func, arg_list) when (match func.eexpr with
         | TLocal { v_name = "__cpp__" } -> true
         | _ -> false) ->
      ( match arg_list with
      | [{ eexpr = TConst (TString code) }] -> output (format_code code);
      | ({ eexpr = TConst (TString code) } as ecode) :: tl ->
         Codegen.interpolate_code ctx.ctx_common (format_code code) tl output (gen_expression true) ecode.epos
      | _ -> error "__cpp__'s first argument must be a string" func.epos;
      )
   | TCall (func, arg_list) when tcall_expand_args->
      let arg_string = ref "" in
      let idx = ref 0 in
      List.iter (fun arg ->
         let a_name = "__a" ^ string_of_int(!idx) in
         arg_string := !arg_string ^ (if !arg_string<>"" then "," else "") ^ a_name;
         idx := !idx + 1;
         output_i ( (type_string arg.etype) ^ " " ^ a_name ^ " = ");
         gen_expression true arg;
         output ";\n";
      ) arg_list;
      output_i (if retval then "return " else "");
      ctx.ctx_calling <- true;
      gen_expression true func;
      output ("(" ^ !arg_string ^ ");\n");
   | TCall (func, arg_list) when is_fromStaticFunction_call func ->
      (match arg_list with
         | [ {eexpr = TField( _, FStatic(klass,field)) } ] ->
            let signature = cpp_function_signature field.cf_type "" in
            let name = keyword_remap field.cf_name in
            let void_cast = has_meta_key field.cf_meta Meta.Void in
            output ("::cpp::Function< " ^ signature ^">(");
            if (void_cast) then output "hx::AnyCast(";
            output ("&::" ^(join_class_path klass.cl_path "::")^ "_obj::" ^ name );
            if (void_cast) then output ")";
            output (" )");
         | _ -> error "fromStaticFunction must take a static function" expression.epos;
      )

   | TCall (func, [arg]) when is_addressOf_call func && not (is_lvalue arg) ->
      error "addressOf must take a local or member variable" expression.epos;

   | TCall (func, arg_list) ->
      let rec is_variable e = match e.eexpr with
      | TField _ | TEnumParameter _ -> false
      | TLocal { v_name = "__global__" } -> false
      | TParenthesis p | TMeta(_,p) -> is_variable p
      | _ -> true
      in
      let expr_type = type_string expression.etype in
      let rec is_fixed_override e = (not (is_scalar expr_type)) && match e.eexpr with
      | TField(obj,FInstance(_,_,field) ) ->
         let cpp_type = member_type ctx obj field.cf_name in
         (not (is_scalar cpp_type)) && (
            let fixed = (cpp_type<>"?") && (expr_type<>"Dynamic") && (cpp_type<>"Dynamic") &&
               (cpp_type<>expr_type) && (expr_type<>"Void") && (cpp_type<>"cpp::ArrayBase") in
            if (fixed && (ctx.ctx_debug_level>1) ) then begin
               output ("/* " ^ (cpp_type) ^ " != " ^ expr_type ^ " -> cast */");
            end;
            fixed
         )
      | TParenthesis p | TMeta(_,p) -> is_fixed_override p
      | _ -> false
      in
      let check_extern_pointer_cast e = match (remove_parens e).eexpr with
      | TField (_,FInstance(class_def,_,_) )
      | TField (_,FStatic(class_def,_) )
         when class_def.cl_extern ->
         (try
            let return_type = expression.etype in
            (is_pointer return_type false) &&
               ( output ( (type_string return_type) ^ "(" ); true; )
         with Not_found -> false )
      | _ -> false
      in
      let is_super = (match func.eexpr with | TConst TSuper -> true | _ -> false ) in
      if (ctx.ctx_debug_level>1) then output ("/* TCALL ret=" ^ expr_type ^ "*/");
      let cast_result =  (not is_super) && (is_fixed_override func) in
      if (cast_result) then output ("hx::TCast< " ^ expr_type ^ " >::cast(");
      let cast_result = cast_result || check_extern_pointer_cast func in

      (* If a static function has @:native('new abc')
          c++ new has lower precedence than in haxe so ( ) must be used *)
      let paren_result =
        if is_native_with_space func then
           ( output "("; true )
        else
           false
      in
      ctx.ctx_calling <- true;
      gen_expression true func;

      output "(";
      gen_expression_list arg_list;
      output ")";
      if paren_result then
         output ")";

      if (cast_result) then output (")");
      if ( (is_variable func) && (not (is_cpp_function_member func) ) &&
           (expr_type<>"Dynamic" && expr_type<>"cpp::ArrayBase" ) && (not is_super)  ) then
         ctx.ctx_output (".Cast< " ^ expr_type ^ " >()" );

      let rec cast_array_output func =
         match func.eexpr with
            | TField(obj,field) when is_array obj.etype ->
               (match field_name field with
                  | "pop" | "shift" | "__unsafe_get" | "__unsafe_set" -> check_array_element_cast obj.etype ".StaticCast" "()"
                  | "map" -> check_array_cast expression.etype
                  | _ -> ()
               )
            | TParenthesis p | TMeta(_,p) -> cast_array_output p
            | _ -> ()
      in
      cast_array_output func;

   | TBlock expr_list ->
      if (retval) then
         gen_local_block_call()
      else begin
         writer#begin_block;
         dump_src_pos();
         (* Save old values, and equalize for new input ... *)
         let pop_names = push_anon_names ctx in
         let remaining = ref (List.length expr_list) in
         List.iter (fun expression ->
            let want_value = (return_from_block && !remaining = 1) in
            find_local_functions_and_return_blocks_ctx want_value expression;
            if (ctx.ctx_debug_level>0) then
               output_i ("HX_STACK_LINE(" ^ (string_of_int (Lexer.get_error_line expression.epos)) ^ ")\n" );
            output_i "";
            ctx.ctx_return_from_internal_node <- return_from_internal_node;
            if (want_value) then output "return ";
            gen_expression want_value expression;
            decr remaining;
            writer#terminate_line
            ) expr_list;
         writer#end_block;
         pop_names()
      end
   | TTypeExpr type_expr ->
      let klass = "::" ^ (join_class_path_remap (t_path type_expr) "::" ) in
      let klass1 = if klass="::Array" then "Array<int>" else klass in
      output ("hx::ClassOf< " ^ klass1 ^ " >()")
   | TReturn _ when retval ->
      unsupported expression.epos
   | TReturn optional_expr ->
      output "";
      ( match optional_expr with
      | Some return_expression when ( (type_string expression.etype)="Void") ->
         output "return null(";
         gen_expression true return_expression;
         output ")";
      | Some return_expression ->
         output "return ";
         gen_expression true return_expression
      | _ -> output (if ctx.ctx_real_void then "return" else "return null()")
      )

   | TConst const ->
      (match const with
      | TInt i when  ctx.ctx_for_extern -> output (Printf.sprintf "%ld" i)
      | TInt i -> output (Printf.sprintf "(int)%ld"  i)
      | TFloat float_as_string -> output ("((Float)" ^ float_as_string ^")")
      | TString s when ctx.ctx_for_extern -> output ("\"" ^ (escape_extern s) ^ "\"")
      | TString s -> output (str s)
      | TBool b -> output (if b then "true" else "false")
      (*| TNull -> output ("((" ^ (type_string expression.etype) ^ ")null())")*)
      | TNull -> output (if ctx.ctx_for_extern then "null" else "null()")
      | TThis -> output (if ctx.ctx_real_this_ptr then "hx::ObjectPtr<OBJ_>(this)" else "__this")
      | TSuper when calling ->
         output (if ctx.ctx_real_this_ptr then
               "super::__construct"
            else
               ("__this->" ^ ctx.ctx_class_super_name ^ "::__construct") )
      | TSuper -> output ("hx::ObjectPtr<super>(" ^ (if ctx.ctx_real_this_ptr then "this" else "__this.mPtr") ^ ")")
      )


   | TLocal v -> output (keyword_remap v.v_name);
   | TArray (array_expr,_) when (is_null array_expr) -> output "Dynamic()"
   | TArray (array_expr,index) ->
      let dynamic =  is_dynamic_in_cpp ctx array_expr || (type_string array_expr.etype) = "cpp::ArrayBase" in
      if ( assigning && (not dynamic) ) then begin
         if (is_array_implementer array_expr.etype) then begin
            output "hx::__ArrayImplRef(";
            gen_expression true array_expr;
            output ",";
            gen_expression true index;
            output ")";
         end else begin
            gen_expression true array_expr;
            output "[";
            gen_expression true index;
            output "]";
         end
      end else if (assigning) then begin
         (* output (" /*" ^ (type_string array_expr.etype) ^ " */ "); *)
         output "hx::IndexRef((";
         gen_expression true array_expr;
         output ").mPtr,";
         gen_expression true index;
         output ")";
      end else if ( dynamic ) then begin
         gen_expression true array_expr;
         output "->__GetItem(";
         gen_expression true index;
         output ")";
      end else begin
         gen_expression true array_expr;
         output "->__get(";
         gen_expression true index;
         output ")";
         if not (is_pointer array_expr.etype true) then
            check_array_element_cast array_expr.etype ".StaticCast" "()";
      end
   (* Get precidence matching haxe ? *)
   | TBinop (op,expr1,expr2) -> gen_bin_op op expr1 expr2
   | TField (expr,_) | TEnumParameter (expr,_,_) when (is_null expr) ->
         output "hx::Throw(HX_CSTRING(\"Invalid field access on null object\"))"
   | TEnumParameter (expr,ef,i) ->
      let enum = match follow ef.ef_type with
         | TEnum(en,_) | TFun(_,TEnum(en,_)) -> en
         | _ -> assert false
      in
      output (  "(::" ^ (join_class_path_remap enum.e_path "::") ^ "(");
      gen_expression true expr;
      output ( "))->__Param(" ^ (string_of_int i) ^ ")")
   | TField (field_object,field) ->
      gen_tfield field_object field

   | TParenthesis expr when not retval ->
         gen_expression retval expr;
   | TParenthesis expr -> output "("; gen_expression retval expr; output ")"
   | TMeta (_,expr) -> gen_expression retval expr;
   | TObjectDecl (
      ("fileName" , { eexpr = (TConst (TString file)) }) ::
         ("lineNumber" , { eexpr = (TConst (TInt line)) }) ::
            ("className" , { eexpr = (TConst (TString class_name)) }) ::
               ("methodName", { eexpr = (TConst (TString meth)) }) :: [] ) ->
      output ("hx::SourceInfo(" ^ (str file) ^ "," ^ (Printf.sprintf "%ld" line) ^ "," ^
         (str class_name) ^ "," ^ (str meth) ^ ")" )
   | TObjectDecl decl_list -> gen_local_block_call()
   | TArrayDecl decl_list ->
      (* gen_type output expression.etype; *)
      let tstr = (type_string_suff "_obj" expression.etype true) in
      if tstr="Dynamic" then
         output "Dynamic( Array_obj<Dynamic>::__new()"
      else
         output ( (type_string_suff "_obj" expression.etype true) ^ "::__new()");
      List.iter ( fun elem -> output ".Add(";
                     gen_expression true elem;
                     output ")" ) decl_list;
      if tstr="Dynamic" then output ")";
   | TNew (klass,params,expressions) ->
      let is_param_array = match klass.cl_path with
      | ([],"Array") when is_dynamic_array_param (List.hd params) -> true | _ -> false
      in
      if is_param_array then
            output "Dynamic( Array_obj<Dynamic>::__new() )"
      else begin
         if (klass.cl_path = ([],"String")) then
            output "::String("
         else
            output ( ( class_string klass "_obj" params true) ^ "::__new(" );
         gen_expression_list expressions;
         output ")"
      end
   | TUnop (Ast.NegBits,Ast.Prefix,expr) ->
      output "~(int)(";
      gen_expression true expr;
      output ")"
   | TUnop (op,Ast.Prefix,expr) ->
      ctx.ctx_assigning <- (match op with Ast.Increment | Ast.Decrement -> true | _ ->false);
      output (Ast.s_unop op);
      output "(";
      gen_expression true expr;
      output ")"
   | TUnop (op,Ast.Postfix,expr) ->
      ctx.ctx_assigning <- true;
      output "(";
      gen_expression true expr;
      output ")";
      output (Ast.s_unop op)
   | TFunction func ->
      let func_name = use_anon_function_name ctx in
      (
      try
         output ( " Dynamic(new " ^ func_name ^ "(" ^
            (Hashtbl.find ctx.ctx_local_function_args func_name) ^ "))" )
      with Not_found ->
         (*error ("function " ^ func_name ^ " not found.") expression.epos; *)
         output ("function " ^ func_name ^ " not found.");
      )

   | TVar (tvar,optional_init) ->
      let count = ref 1 in (* TODO: this section can be simplified *)
      if (retval && !count==1) then
         (match optional_init with
         | None -> output "null()"
         | Some expression -> gen_expression true expression )
      else begin
      let type_name = (type_string tvar.v_type) in
         output (if type_name="Void" then "Dynamic" else type_name );
         let name = (keyword_remap tvar.v_name) in
         output (" " ^ name );
         (match optional_init with
         | None -> ()
         | Some expression -> output " = "; gen_expression true expression);
         count := !count -1;
      if (ctx.ctx_debug_level>0) then
            output (";\t\tHX_STACK_VAR(" ^name ^",\""^ tvar.v_name ^"\")");
         if (!count > 0) then begin output ";\n"; output_i "" end
      end
   | TFor (tvar, init, loop) ->
      output ("for(::cpp::FastIterator_obj< " ^  (type_string tvar.v_type) ^
            " > *__it = ::cpp::CreateFastIterator< "^(type_string tvar.v_type) ^ " >(");
      gen_expression true init;
      output (");  __it->hasNext(); )");
      ctx.ctx_writer#begin_block;
      output_i ( (type_string tvar.v_type) ^ " " ^ (keyword_remap tvar.v_name) ^ " = __it->next();\n" );
      output_i "";
      gen_expression false loop;
      output ";\n";
      ctx.ctx_writer#end_block;
   | TIf (condition, if_expr, optional_else_expr)  ->
      (match optional_else_expr with
      | Some else_expr ->
         if (retval) then begin
            output "(  (";
            gen_expression true condition;
            output ") ? ";
            let type_str = match (type_string expression.etype) with
            | "Void" -> "Dynamic"
            | other -> other
            in
            output (type_str ^ "(");
            gen_expression true if_expr;
            output ") : ";

            output (type_str ^ "(");
            gen_expression true else_expr;
            output ") )";
         end else begin
            output "if (";
            gen_expression true condition;
            output ")";
            gen_expression false (to_block if_expr);
            output_i "else";
            gen_expression false (to_block else_expr);
         end
      | _ -> output "if (";
         gen_expression true condition;
         output ")";
         gen_expression false (to_block if_expr);
      )
   | TWhile (condition, repeat, Ast.NormalWhile ) ->
         output  "while(";
         gen_expression true condition;
         output ")";
         gen_expression false (to_block repeat)
   | TWhile (condition, repeat, Ast.DoWhile ) ->
         output "do";
         gen_expression false (to_block repeat);
         output "while(";
         gen_expression true condition;
         output ")"

   (* These have already been defined in find_local_return_blocks ... *)
   | TTry (_,_)
   | TSwitch (_,_,_) when (retval && (not return_from_internal_node) ) ->
      gen_local_block_call()
   | TSwitch (condition,cases,optional_default)  ->
      let switch_on_int_constants = (only_int_cases cases) && (not (contains_break expression)) in
      if (switch_on_int_constants) then begin
         output "switch( (int)";
         gen_expression true condition;
         output ")";
         ctx.ctx_writer#begin_block;
         List.iter (fun (cases_list,expression) ->
            output_i "";
            List.iter (fun value -> output "case ";
                        gen_expression true value;
                        output ": " ) cases_list;
            ctx.ctx_return_from_block <- return_from_internal_node;
            gen_expression false (to_block expression);
            output_i ";break;\n";
            ) cases;
         (match optional_default with | None -> ()
         | Some default ->
            output_i "default: ";
            ctx.ctx_return_from_block <- return_from_internal_node;
            gen_expression false (to_block default);
         );
         ctx.ctx_writer#end_block;
      end else begin
         let tmp_name = get_switch_var ctx in
         output ( (type_string condition.etype) ^ " " ^ tmp_name ^ " = " );
         gen_expression true condition;
         output ";\n";
         let else_str = ref "" in
         if (List.length cases > 0) then
            List.iter (fun (cases,expression) ->
               output_i ( !else_str ^ "if ( ");
               else_str := "else ";
               let or_str = ref "" in
               List.iter (fun value ->
                  output (!or_str ^ " ( " ^ tmp_name ^ "==");
                  gen_expression true value;
                  output ")";
                  or_str := " || ";
                  ) cases;
               output (")");
               ctx.ctx_return_from_block <- return_from_internal_node;
               gen_expression false (to_block expression);
               ) cases;
         (match optional_default with | None -> ()
         | Some default ->
            output_i ( !else_str ^ " ");
            ctx.ctx_return_from_block <- return_from_internal_node;
            gen_expression false (to_block default);
            output ";\n";
         );
      end
   | TTry (expression, catch_list) ->
      output "try\n";
      output_i "{\n";
      let counter = ref 0 in
      List.iter (fun (v, e) ->
         let type_name = type_string v.v_type in
            output_i ("HX_STACK_CATCHABLE(" ^ type_name ^ ", " ^ string_of_int !counter ^ ");\n");
               counter := !counter + 1;)
            catch_list;
      output_i("");
      (* Move this "inside" the try call ... *)
      ctx.ctx_return_from_block <-return_from_internal_node;
      gen_expression false (to_block expression);
      output_i "}\n";
      if (List.length catch_list > 0 ) then begin
         output_i "catch(Dynamic __e)";
         ctx.ctx_writer#begin_block;
         let seen_dynamic = ref false in
         let else_str = ref "" in
         List.iter (fun (v,expression) ->
            let type_name = type_string v.v_type in
            if (type_name="Dynamic") then begin
               seen_dynamic := true;
               output_i !else_str;
            end else
               output_i (!else_str ^ "if (__e.IsClass< " ^ type_name ^ " >() )");
            ctx.ctx_writer#begin_block;
            output_i "HX_STACK_BEGIN_CATCH\n";
            output_i (type_name ^ " " ^ v.v_name ^ " = __e;");
            (* Move this "inside" the catch call too ... *)
            ctx.ctx_return_from_block <-return_from_internal_node;
            gen_expression false (to_block expression);
            ctx.ctx_writer#end_block;
            else_str := "else ";
            ) catch_list;
         if (not !seen_dynamic) then begin
            output_i "else {\n";
               output_i "    HX_STACK_DO_THROW(__e);\n";
               output_i "}\n";
         end;
         ctx.ctx_writer#end_block;
      end;
   | TBreak -> output "break"
   | TContinue -> output "continue"
   | TThrow expression ->
         output "HX_STACK_DO_THROW(";
         gen_expression true expression;
         output ")";
   | TCast (cast,None) when (not retval) || (type_string expression.etype) = "Void" ->
      gen_expression retval cast;
   | TCast (cast,None) ->
      let ret_type = type_string expression.etype in
      let from_type = if is_dynamic_in_cpp ctx cast then "Dynamic" else type_string cast.etype in
      if (from_type = ret_type) then begin
         gen_expression true cast
      end else begin
         output ("((" ^ ret_type ^ ")(");
         gen_expression true cast;
         output "))";
      end;
   | TCast (e1,Some t) ->
      let class_name = (join_class_path_remap (t_path t) "::" ) in
      if (class_name="Array") then
         output ("hx::TCastToArray(" )
      else
         output ("hx::TCast< ::" ^ class_name ^ " >::cast(" );
      gen_expression true e1;
      output ")";
 in

 if (set_var<>"") then begin
    find_local_functions_and_return_blocks_ctx true expression_tree;
    output set_var;
 end;
 gen_expression retval expression_tree;
 output tail_code
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
   match field.cf_expr with
   | Some { eexpr = TFunction function_def } -> is_dynamic_haxe_method field
   | _ -> true;;


let is_override class_def field =
   List.exists (fun f -> f.cf_name = field) class_def.cl_overrides
;;

let rec all_virtual_functions clazz =
   (List.fold_left (fun result elem -> match follow elem.cf_type, elem.cf_kind  with
      | _, Method MethDynamic -> result
      | TFun (args,return_type), Method _  when not (is_override clazz elem.cf_name ) -> (elem,args,return_type) :: result
      | _,_ -> result ) [] clazz.cl_ordered_fields)
   @ (match clazz.cl_super with
   | Some def -> all_virtual_functions (fst def)
   | _ -> [] )
;;

let reflective class_def field = not (
    (Meta.has Meta.NativeGen class_def.cl_meta) ||
    (Meta.has Meta.Unreflective class_def.cl_meta) ||
    (Meta.has Meta.Unreflective field.cf_meta) ||
    (match field.cf_type with
       | TInst (klass,_) ->  Meta.has Meta.Unreflective klass.cl_meta
       | _ -> false
    )
)
;;





let field_arg_count field =
   match follow field.cf_type, field.cf_kind  with
      | _, Method MethDynamic -> -1
      | TFun (args,return_type), Method _  -> List.length args
      | _,_ -> -1
;;


            (* external mem  Dynamic & *)

let gen_field ctx class_def class_name ptr_name dot_name is_static is_interface field =
   let output = ctx.ctx_output in
   ctx.ctx_real_this_ptr <- not is_static;
   let remap_name = keyword_remap field.cf_name in
   let decl = get_meta_string field.cf_meta Meta.Decl in
   let has_decl = decl <> "" in
   let nativeGen = has_meta_key class_def.cl_meta Meta.NativeGen in
   if (is_interface) then begin
      (* Just the dynamic glue  - not even that ... *)
      ()
   end else (match  field.cf_expr with
   (* Function field *)
   | Some { eexpr = TFunction function_def } ->
      let return_type = (type_string function_def.tf_type) in
      let nargs = string_of_int (List.length function_def.tf_args) in
      let is_void = (type_string function_def.tf_type ) = "Void" in
      let ret = if is_void  then "(void)" else "return " in
      let output_i = ctx.ctx_writer#write_i in
      let orig_debug = ctx.ctx_debug_level in
      let dump_src = if ((Meta.has Meta.NoStack field.cf_meta)||(Meta.has Meta.NoDebug field.cf_meta) || orig_debug<1 || nativeGen) then begin
         ctx.ctx_debug_level <- 0;
         (fun()->())
      end else begin
         (fun() ->
         hx_stack_push ctx output_i dot_name field.cf_name function_def.tf_expr.epos;
         if (not is_static) then output_i ("HX_STACK_THIS(this)\n");
         List.iter (fun (v,_) -> output_i ("HX_STACK_ARG(" ^ (keyword_remap v.v_name) ^ ",\"" ^ v.v_name ^"\")\n") )
            function_def.tf_args )
      end in

      if (not (is_dynamic_haxe_method field)) then begin
         (* The actual function definition *)
         let real_void = is_void  && (has_meta_key field.cf_meta Meta.Void) in
         let fake_void = is_void  && not real_void in
         output (if real_void then "void" else return_type );
         output (" " ^ class_name ^ "::" ^ remap_name ^ "( " );
         output (gen_arg_list function_def.tf_args "__o_");
         output ")";
         ctx.ctx_real_this_ptr <- true;
         ctx.ctx_real_void <- real_void;
         ctx.ctx_dynamic_this_ptr <- false;
         let code = (get_code field.cf_meta Meta.FunctionCode) in
         let tail_code = (get_code field.cf_meta Meta.FunctionTailCode) in
         if (has_default_values function_def.tf_args) then begin
            ctx.ctx_writer#begin_block;
            generate_default_values ctx function_def.tf_args "__o_";
            dump_src();
            output code;
            gen_expression_tree ctx false function_def.tf_expr "" tail_code;
            if (fake_void) then output "return null();\n";
            ctx.ctx_writer#end_block;
         end else begin
            let add_block = is_void || (code <> "") || (tail_code <> "") in
            if (add_block) then ctx.ctx_writer#begin_block;
            ctx.ctx_dump_src_pos <- dump_src;
            output code;
            gen_expression_tree ctx false (to_block function_def.tf_expr) "" tail_code;
            if (add_block) then begin
               if (fake_void) then output "return null();\n";
               ctx.ctx_writer#end_block;
            end;
         end;

         output "\n\n";
         let nonVirtual = has_meta_key field.cf_meta Meta.NonVirtual in
         let doDynamic =  (nonVirtual || not (is_override class_def field.cf_name ) ) && (reflective class_def field ) in
         (* generate dynamic version too ... *)
         if ( doDynamic ) then begin
            if (is_static) then output "STATIC_";
            output ("HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ "," ^
                     remap_name ^ "," ^ ret ^ ")\n\n");
         end;

      end else begin
         ctx.ctx_real_this_ptr <- false;
         ctx.ctx_dynamic_this_ptr <- false;
         let func_name = "__default_" ^ (remap_name) in
         output ("HX_BEGIN_DEFAULT_FUNC(" ^ func_name ^ "," ^ class_name ^ ")\n");
         output return_type;
         output (" run(" ^ (gen_arg_list function_def.tf_args "__o_") ^ ")");
         ctx.ctx_dump_src_pos <- dump_src;
         if (is_void) then begin
            ctx.ctx_writer#begin_block;
            generate_default_values ctx function_def.tf_args "__o_";
            gen_expression_tree ctx false function_def.tf_expr "" "";
            output "return null();\n";
            ctx.ctx_writer#end_block;
         end else if (has_default_values function_def.tf_args) then begin
            ctx.ctx_writer#begin_block;
            generate_default_values ctx function_def.tf_args "__o_";
            gen_expression_tree ctx false function_def.tf_expr "" "";
            ctx.ctx_writer#end_block;
         end else
            gen_expression_tree ctx false (to_block function_def.tf_expr) "" "";

         output ("HX_END_LOCAL_FUNC" ^ nargs ^ "(" ^ ret ^ ")\n");
         output ("HX_END_DEFAULT_FUNC\n\n");

         if (is_static) then
            output ( "Dynamic " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
      end;
      ctx.ctx_debug_level <- orig_debug

   (* Data field *)
   | _ when has_decl ->
      if is_static then begin
         output ( class_name ^ "::" ^ remap_name ^ "_decl ");
         output ( " " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
      end
   | _ ->
      if is_static && (not (is_extern_field field)) then begin
         gen_type ctx field.cf_type;
         output ( " " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
      end
   )
   ;;





let gen_field_init ctx field =
   let output = ctx.ctx_output in
   let remap_name = keyword_remap field.cf_name in
   (match  field.cf_expr with
   (* Function field *)
   | Some { eexpr = TFunction function_def } ->

      if (is_dynamic_haxe_method field) then begin
         let func_name = "__default_" ^ (remap_name) in
         output ( "\t" ^ remap_name ^ " = new " ^ func_name ^ ";\n\n" );
      end

   (* Data field *)
   | _ -> (match field.cf_expr with
      | Some expr ->
         let var_name = ( match remap_name with
                  | "__meta__" -> "\t__mClass->__meta__="
                  | "__rtti" -> "\t__mClass->__rtti__="
                  | _ -> "\t" ^ remap_name ^ "= ") in
         gen_expression_tree ctx true expr var_name ";\n";
      | _ -> ( )
      );
   )
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
         output ( (if (not is_static) then "		virtual " else "		" ) ^ type_string return_type);
         output (" " ^ remap_name ^ "( " );
         output (gen_tfun_interface_arg_list args);
         output (if (not is_static) then ")=0;\n" else ");\n");
         if reflective class_def field then begin
            output ("virtual Dynamic " ^ remap_name ^ "_dyn()=0;\n" );
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
            output ("Dynamic " ^ remap_name ^ ";\n");
            output (if is_static then "\t\tstatic " else "\t\t");
            output ("inline Dynamic &" ^ remap_name ^ "_dyn() " ^ "{return " ^ remap_name^ "; }\n")
         end
      end else begin
         let return_type = (type_string function_def.tf_type) in

         if ( not is_static && not nonVirtual ) then output "virtual ";
         output (if return_type="Void" && (has_meta_key field.cf_meta Meta.Void) then "void" else return_type );

         output (" " ^ remap_name ^ "( " );
         output (gen_arg_list function_def.tf_args "" );
         output ");\n";
         if ( doDynamic ) then begin
            output (if is_static then "\t\tstatic " else "\t\t");
            output ("Dynamic " ^ remap_name ^ "_dyn();\n" )
         end;
      end;
      output "\n";
   | _ when has_decl ->
      output ( remap_name ^ "_decl " ^ remap_name ^ ";\n" );
      (* Variable access *)
   | _ ->
      (* Variable access *)
      gen_type ctx field.cf_type;
      output (" " ^ remap_name ^ ";\n" );

      (* Add a "dyn" function for variable to unify variable/function access *)
      (match follow field.cf_type with
      | _ when nativeGen  -> ()
      | TFun (_,_) ->
         output (if is_static then "\t\tstatic " else "\t\t");
         gen_type ctx field.cf_type;
         output (" &" ^ remap_name ^ "_dyn() { return " ^ remap_name ^ ";}\n" )
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
let find_referenced_types ctx obj super_deps constructor_deps header_only for_depends include_super_args =
   let types = ref PMap.empty in
   let rec add_type in_path =
      if ( not (PMap.mem in_path !types)) then begin
         types := (PMap.add in_path () !types);
         try
            List.iter add_type (Hashtbl.find super_deps in_path);
         with Not_found -> ()
      end
   in
   let add_extern_class klass =
      let include_file = get_meta_string_path klass.cl_meta (if for_depends then Meta.Depend else Meta.Include) in
      if (include_file<>"") then
         add_type ( path_of_string include_file )
      else if (not for_depends) && (has_meta_key klass.cl_meta Meta.Include) then
         add_type klass.cl_path
   in
   let add_native_gen_class klass =
      let include_file = get_meta_string_path klass.cl_meta (if for_depends then Meta.Depend else Meta.Include) in
      if (include_file<>"") then
         add_type ( path_of_string include_file )
      else if for_depends then
         add_type klass.cl_path
      else
         add_type ( path_of_string ( (join_class_path klass.cl_path "/") ^ ".h") )
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
            (* Must visit the enum param types, Type.iter will visit the rest ... *)
(*             | TMatch (_,enum,cases,_) ->
               add_type (fst enum).e_path;
               List.iter (fun (case_ids,params,expression) ->
                  (match params with
                  | None -> ()
                  | Some l -> List.iter (function None -> () | Some v -> visit_type v.v_type) l  ) ) cases; *)
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
            | TConst TSuper ->
               (match follow expression.etype with
               | TInst (klass,params) ->
                  (try let construct_type = Hashtbl.find constructor_deps klass.cl_path in
                     visit_type construct_type.cf_type
                  with Not_found -> () )
               | _ -> print_endline ("TSuper : Odd etype ?" ^ ( (type_string expression.etype)) )
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
         let meta = Codegen.build_metadata ctx (TEnumDecl enum_def) in
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

   List.sort inc_cmp (List.filter (fun path -> (include_class_header path) ) (pmap_keys !types))
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


let generate_main common_ctx member_types super_deps class_def file_info =
   (* main routine should be a single static function *)
   let main_expression =
      (match class_def.cl_ordered_statics with
      | [{ cf_expr = Some expression }] -> expression;
      | _ -> assert false ) in
   ignore(find_referenced_types common_ctx (TClassDecl class_def) super_deps (Hashtbl.create 0) false false false);
   let depend_referenced = find_referenced_types common_ctx (TClassDecl class_def) super_deps (Hashtbl.create 0) false true false in
   let generate_startup filename is_main =
      (*make_class_directories base_dir ( "src" :: []);*)
      let cpp_file = new_cpp_file common_ctx common_ctx.file ([],filename) in
      let output_main = (cpp_file#write) in

      generate_main_header output_main;

      List.iter ( add_include cpp_file ) depend_referenced;
      output_main "\n\n";

      if is_main then output_main "\n#include <hx/HxcppMain.h>\n\n";

      generate_main_footer1 output_main;
      gen_expression_tree (new_context common_ctx cpp_file 1 file_info) false main_expression "" ";\n";
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
      generate_main_header output_main;
      if is_main then output_main "\n#include <hx/HxcppMain.h>\n\n";
      generate_main_footer1 output_main;
      generate_main_footer2 output_main;
      main_file#close;
   in
   generate_startup "__main__" true;
   generate_startup "__lib__" false
   ;;

let generate_boot common_ctx boot_enums boot_classes nonboot_classes init_classes =
   (* Write boot class too ... *)
   let base_dir = common_ctx.file in
   let boot_file = new_cpp_file common_ctx base_dir ([],"__boot__") in
   let output_boot = (boot_file#write) in
   output_boot "#include <hxcpp.h>\n\n";
   List.iter ( fun class_path -> boot_file#add_include class_path )
      (boot_enums @ boot_classes @ nonboot_classes);

   output_boot "\nvoid __files__boot();\n";
   output_boot "\nvoid __boot_all()\n{\n";
   output_boot "__files__boot();\n";
   output_boot "hx::RegisterResources( hx::GetResources() );\n";
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
   output_files "#include <hxcpp.h>\n\n";
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
      Common.get_full_path (try Common.find_file common_ctx file with Not_found -> file)
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


let begin_header_file output_h def_string =
   output_h ("#ifndef INCLUDED_" ^ def_string ^ "\n");
   output_h ("#define INCLUDED_" ^ def_string ^ "\n\n");
   output_h "#ifndef HXCPP_H\n";
   output_h "#include <hxcpp.h>\n";
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



let generate_enum_files common_ctx enum_def super_deps meta file_info =
   let class_path = enum_def.e_path in
   let just_class_name =  (snd class_path) in
   let class_name =  just_class_name ^ "_obj" in
   let remap_class_name =  ("::" ^ (join_class_path_remap class_path "::") )  in
   (*let cpp_file = new_cpp_file common_ctx.file class_path in*)
   let cpp_file = new_placed_cpp_file common_ctx class_path in
   let output_cpp = (cpp_file#write) in
   let debug = if (has_meta_key enum_def.e_meta Meta.NoDebug) || ( Common.defined  common_ctx Define.NoDebug)
      then 0 else 1 in

   let ctx = new_context common_ctx cpp_file debug file_info in

   if (debug>1) then
      print_endline ("Found enum definition:" ^ (join_class_path  class_path "::" ));

   output_cpp "#include <hxcpp.h>\n\n";

   let referenced = find_referenced_types common_ctx (TEnumDecl enum_def) super_deps (Hashtbl.create 0) false false false in
   List.iter (add_include cpp_file) referenced;

   gen_open_namespace output_cpp class_path;
   output_cpp "\n";

   PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      match constructor.ef_type with
      | TFun (args,_) ->
         output_cpp (remap_class_name ^ "  " ^ class_name ^ "::" ^ name ^ "(" ^
            (gen_tfun_arg_list args) ^")\n");
         output_cpp ("\t{ return hx::CreateEnum< " ^ class_name ^ " >(" ^ (str name) ^ "," ^
            (string_of_int constructor.ef_index) ^ ",hx::DynamicArray(0," ^
            (string_of_int (List.length args)) ^  ")" );
         List.iter (fun (arg,_,_) -> output_cpp (".Add(" ^ (keyword_remap arg) ^ ")")) args;
         output_cpp "); }\n\n"

      | _ ->
         output_cpp ( remap_class_name ^ " " ^ class_name ^ "::" ^ name ^ ";\n\n" )
   ) enum_def.e_constrs;




   output_cpp ("HX_DEFINE_CREATE_ENUM(" ^ class_name ^ ")\n\n");
   output_cpp ("int " ^ class_name ^ "::__FindIndex(::String inName)\n{\n");
   PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let idx = string_of_int constructor.ef_index in
      output_cpp ("\tif (inName==" ^ (str name) ^ ") return " ^ idx ^ ";\n") ) enum_def.e_constrs;
   output_cpp ("\treturn super::__FindIndex(inName);\n");
   output_cpp ("}\n\n");

   let constructor_arg_count constructor =
      (match constructor.ef_type with | TFun(args,_) -> List.length args | _ -> 0 )
   in


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
      output_cpp ("\tif (inName==" ^ (str name) ^ ") return " ^ count ^ ";\n") ) enum_def.e_constrs;
      output_cpp ("\treturn super::__FindArgCount(inName);\n");
      output_cpp ("}\n\n");

   (* Dynamic "Get" Field function - string version *)
   output_cpp ("Dynamic " ^ class_name ^ "::__Field(const ::String &inName,hx::PropertyAccess inCallProp)\n{\n");
   let dump_constructor_test _ constr =
      output_cpp ("\tif (inName==" ^ (str constr.ef_name) ^ ") return " ^
                  (keyword_remap constr.ef_name) );
      if ( (constructor_arg_count constr) > 0 ) then output_cpp "_dyn()";
      output_cpp (";\n")
   in
   PMap.iter dump_constructor_test enum_def.e_constrs;
   output_cpp ("\treturn super::__Field(inName,inCallProp);\n}\n\n");

   output_cpp "static ::String sStaticFields[] = {\n";
   let sorted =
      List.sort (fun f1 f2 -> (PMap.find f1 enum_def.e_constrs ).ef_index -
               (PMap.find f2 enum_def.e_constrs ).ef_index )
         (pmap_keys enum_def.e_constrs) in

    List.iter (fun name -> output_cpp ("\t" ^ (str name) ^ ",\n") ) sorted;

   output_cpp "\t::String(null()) };\n\n";

   (* ENUM - Mark static as used by GC *)
   output_cpp "static void sMarkStatics(HX_MARK_PARAMS) {\n";
   PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      match constructor.ef_type with
      | TFun (_,_) -> ()
      | _ -> output_cpp ("\tHX_MARK_MEMBER_NAME(" ^ class_name ^ "::" ^ name ^ ",\"" ^ name ^ "\");\n") )
   enum_def.e_constrs;
   output_cpp "};\n\n";

   (* ENUM - Visit static as used by GC *)
   output_cpp "#ifdef HXCPP_VISIT_ALLOCS\n";
   output_cpp "static void sVisitStatic(HX_VISIT_PARAMS) {\n";
   output_cpp ("\tHX_VISIT_MEMBER_NAME(" ^ class_name ^ "::__mClass,\"__mClass\");\n");
   PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      match constructor.ef_type with
      | TFun (_,_) -> ()
      | _ -> output_cpp ("\tHX_VISIT_MEMBER_NAME(" ^ class_name ^ "::" ^ name ^ ",\"" ^ name ^ "\");\n") )
   enum_def.e_constrs;
   output_cpp "};\n";
   output_cpp "#endif\n\n";

   output_cpp "static ::String sMemberFields[] = { ::String(null()) };\n";

   output_cpp ("hx::Class " ^ class_name ^ "::__mClass;\n\n");

   output_cpp ("Dynamic __Create_" ^ class_name ^ "() { return new " ^ class_name ^ "; }\n\n");

   output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
   let text_name = str (join_class_path class_path ".") in
   output_cpp ("\nhx::Static(__mClass) = hx::RegisterClass(" ^ text_name ^
               ", hx::TCanCast< " ^ class_name ^ " >,sStaticFields,sMemberFields,\n");
   output_cpp ("\t&__Create_" ^ class_name ^ ", &__Create,\n");
   output_cpp ("\t&super::__SGetClass(), &Create" ^ class_name ^ ", sMarkStatics\n");
   output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n    , sVisitStatic\n#endif\n");
   output_cpp ("#ifdef HXCPP_SCRIPTABLE\n    , 0\n#endif\n");
   output_cpp (");\n}\n\n");

   output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
   (match meta with
      | Some expr ->
         let ctx = new_context common_ctx cpp_file 1 file_info in
         gen_expression_tree ctx true expr  "__mClass->__meta__ = " ";\n";
      | _ -> () );
   PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      match constructor.ef_type with
      | TFun (_,_) -> ()
      | _ ->
         output_cpp ( "hx::Static(" ^ (keyword_remap name) ^ ") = hx::CreateEnum< " ^ class_name ^ " >(" ^ (str name) ^  "," ^
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
   ctx.ctx_output <- output_h;

   begin_header_file output_h def_string;

   List.iter (gen_forward_decl h_file ) referenced;

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
      output_h ("\t\t::String GetEnumName( ) const { return " ^
                           (str (join_class_path class_path "."))  ^ "; }\n" );
   output_h ("\t\t::String __ToString() const { return " ^
                           (str (just_class_name ^ ".") )^ " + tag; }\n\n");


   PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      output_h ( "\t\tstatic " ^  remap_class_name ^ " " ^ name );
      match constructor.ef_type with
      | TFun (args,_) ->
         output_h ( "(" ^ (gen_tfun_arg_list args) ^");\n");
         output_h ( "\t\tstatic Dynamic " ^ name ^ "_dyn();\n");
      | _ ->
         output_h ";\n";
         output_h ( "\t\tstatic inline " ^  remap_class_name ^ " " ^ name ^
                  "_dyn() { return " ^name ^ "; }\n" );
   ) enum_def.e_constrs;

   output_h "};\n\n";

   gen_close_namespace output_h class_path;

   end_header_file output_h def_string;
   h_file#close;
   let depend_referenced = find_referenced_types common_ctx (TEnumDecl enum_def) super_deps (Hashtbl.create 0) false true false in
   depend_referenced;;


let list_iteri func in_list =
   let idx = ref 0 in
   List.iter (fun elem -> func !idx elem; idx := !idx + 1 ) in_list
;;

let has_new_gc_references class_def =
   match class_def.cl_dynamic with
   | Some _ -> true
   | _ -> (
      let is_gc_reference field =
      (should_implement_field field) && (is_data_member field) &&
         match type_string field.cf_type with
            | "bool" | "int" | "Float" -> false
            | _ -> true
      in
      List.exists is_gc_reference class_def.cl_ordered_fields
      )
;;


let rec has_gc_references class_def =
   ( match class_def.cl_super with
   | Some def when has_gc_references (fst def) -> true
   | _ -> false )
   || has_new_gc_references class_def
;;

let rec find_next_super_iteration class_def =
   match class_def.cl_super with
   | Some  (klass,params) when has_new_gc_references klass -> class_string klass "_obj" params true
   | Some  (klass,_) -> find_next_super_iteration klass
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
   | Var { v_read = AccNever } when (is_extern_field field) -> false
   | Var { v_read = AccInline } -> false
   | Var _ when is_abstract_impl class_def -> false
   | _ -> true)
;;

let is_writable class_def field =
   (match field.cf_kind with
   | Var { v_write = AccNever } when (is_extern_field field) -> false
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
   List.exists has_field_init (List.filter should_implement_field class_def.cl_ordered_statics);
;;


let is_macro meta =
   Meta.has Meta.Macro meta
;;


let access_str a = match a with
   | AccNormal -> "AccNormal"
   | AccNo -> "AccNo"
   | AccNever -> "AccNever"
   | AccResolve -> "AccResolve"
   | AccCall -> "AccCall"
   | AccInline -> "AccInline"
   | AccRequire(_,_) -> "AccRequire" ;;

let generate_class_files common_ctx member_types super_deps constructor_deps class_def file_info inScriptable =
   let class_path = class_def.cl_path in
   let nativeGen = has_meta_key class_def.cl_meta Meta.NativeGen in
   let class_name = (snd class_path) ^ (if nativeGen then "" else "_obj") in
   let dot_name = join_class_path class_path "." in
   let smart_class_name =  (snd class_path)  in
   (*let cpp_file = new_cpp_file common_ctx.file class_path in*)
   let cpp_file = new_placed_cpp_file common_ctx class_path in
   let output_cpp = (cpp_file#write) in
   let debug = if (has_meta_key class_def.cl_meta Meta.NoDebug) || ( Common.defined  common_ctx Define.NoDebug)
      then 0 else 1 in
   let scriptable = inScriptable && not class_def.cl_private in
   let ctx = new_context common_ctx cpp_file debug file_info in


   ctx.ctx_class_name <- "::" ^ (join_class_path class_def.cl_path "::");
   ctx.ctx_class_super_name <- (match class_def.cl_super with
      | Some (klass, params) -> class_string klass "_obj" params true
      | _ -> "");
   ctx.ctx_class_member_types <- member_types;
   if (debug>1) then print_endline ("Found class definition:" ^ ctx.ctx_class_name);

   let ptr_name = "hx::ObjectPtr< " ^ class_name ^ " >" in
   let constructor_arg_var_list =
      match class_def.cl_constructor with
      | Some definition ->
               (match definition.cf_expr with
                  | Some { eexpr = TFunction function_def } ->
                     List.map (fun (v,o) -> (v.v_name, gen_arg_type_name v.v_name o v.v_type "__o_"))
                           function_def.tf_args;
                  | _ ->
                     (match follow definition.cf_type with
                        | TFun (args,_) -> List.map (fun (a,_,t) -> (a, (type_string t, a)) )  args
                        | _ -> [])
               )
      | _ -> [] in
   let constructor_type_var_list =
      List.map snd constructor_arg_var_list in
   let constructor_var_list = List.map snd constructor_type_var_list in
   let constructor_type_args = String.concat ","
            (List.map (fun (t,a) -> t ^ " " ^ a) constructor_type_var_list) in
   let constructor_args = String.concat "," constructor_var_list in

   let implement_dynamic = implement_dynamic_here class_def in

   output_cpp "#include <hxcpp.h>\n\n";

   let force_field = scriptable && (has_get_member_field class_def) in
   let field_integer_dynamic = force_field || (has_field_integer_lookup class_def) in
   let field_integer_numeric = force_field || (has_field_integer_numeric_lookup class_def) in

   let all_referenced = find_referenced_types ctx.ctx_common (TClassDecl class_def) super_deps constructor_deps false false scriptable in
   List.iter ( add_include cpp_file  ) all_referenced;


   (* All interfaces (and sub-interfaces) implemented *)
   let implemented_hash = Hashtbl.create 0 in
   List.iter (fun imp ->
      let rec descend_interface interface =
         let imp_path = (fst interface).cl_path in
         let interface_name = "::" ^ (join_class_path_remap imp_path "::" ) in
         if ( not (Hashtbl.mem implemented_hash interface_name) ) then begin
            Hashtbl.add implemented_hash interface_name ();
            List.iter descend_interface (fst interface).cl_implements;
         end;
         match (fst interface).cl_super with
         | Some (interface,params) -> descend_interface (interface,params)
         | _ -> ()
      in descend_interface imp
   ) (real_interfaces class_def.cl_implements);
   let implemented = hash_keys implemented_hash in

   if (scriptable) then
      output_cpp "#include <hx/Scriptable.h>\n";

   output_cpp ( get_class_code class_def Meta.CppFileCode );
   let inc = get_meta_string_path class_def.cl_meta Meta.CppInclude in
   if (inc<>"") then
      output_cpp ("#include \"" ^ inc ^ "\"\n");

   gen_open_namespace output_cpp class_path;
   output_cpp "\n";

   output_cpp ( get_class_code class_def Meta.CppNamespaceCode );

   if (not class_def.cl_interface) && not nativeGen then begin
      output_cpp ("Void " ^ class_name ^ "::__construct(" ^ constructor_type_args ^ ")\n{\n");
      (match class_def.cl_constructor with
         | Some definition ->
               (match  definition.cf_expr with
               | Some { eexpr = TFunction function_def } ->
                  if has_meta_key definition.cf_meta Meta.NoDebug then ctx.ctx_debug_level <- 0;
                  if ctx.ctx_debug_level >0 then begin
                     hx_stack_push ctx output_cpp dot_name "new" function_def.tf_expr.epos;
                     output_cpp "HX_STACK_THIS(this)\n";
                     List.iter (fun (a,(t,o)) -> output_cpp ("HX_STACK_ARG(" ^ (keyword_remap o) ^ ",\"" ^ a ^"\")\n") ) constructor_arg_var_list;
                  end;

                  if (has_default_values function_def.tf_args) then begin
                     generate_default_values ctx function_def.tf_args "__o_";
                  end;
                  gen_expression_tree ctx false (to_block function_def.tf_expr) "" ";\n";
                  ctx.ctx_debug_level <- debug;
               | _ -> ()
               )
         | _ -> ());
         output_cpp "\treturn null();\n";
         output_cpp "}\n\n";

      (* Destructor goes in the cpp file so we can "see" the full definition of the member vars *)
      output_cpp ( "//" ^ class_name ^ "::~" ^ class_name ^ "() { }\n\n");
      output_cpp ("Dynamic " ^ class_name ^ "::__CreateEmpty() { return  new " ^ class_name ^ "; }\n");

      output_cpp (ptr_name ^ " " ^ class_name ^ "::__new(" ^constructor_type_args ^")\n");

      let create_result () =
         output_cpp ("{  " ^ ptr_name ^ " _result_ = new " ^ class_name ^ "();\n");
         in
      create_result ();
      output_cpp ("\t_result_->__construct(" ^ constructor_args ^ ");\n");
      output_cpp ("\treturn _result_;}\n\n");

      output_cpp ("Dynamic " ^ class_name ^ "::__Create(hx::DynamicArray inArgs)\n");
      create_result ();
      output_cpp ("\t_result_->__construct(" ^ (array_arg_list constructor_var_list) ^ ");\n");
      output_cpp ("\treturn _result_;}\n\n");
      if ( (List.length implemented) > 0 ) then begin
         output_cpp ("hx::Object *" ^ class_name ^ "::__ToInterface(const hx::type_info &inType) {\n");
         List.iter (fun interface_name ->
            output_cpp ("\tif (inType==typeid( " ^ interface_name ^ "_obj)) " ^
               "return operator " ^ interface_name ^ "_obj *();\n");
            ) implemented;
         output_cpp ("\treturn super::__ToInterface(inType);\n}\n\n");


         List.iter (fun interface_name ->
            output_cpp (class_name ^ "::operator " ^ interface_name ^ "_obj *()\n\t" ^
               "{ return new " ^ interface_name ^ "_delegate_< " ^ class_name ^" >(this); }\n" );
         ) implemented;
      end;

   end;

   (match class_def.cl_init with
   | Some expression ->
      output_cpp ("void " ^ class_name^ "::__init__() {\n");
      hx_stack_push ctx output_cpp dot_name "__init__" expression.epos;
      gen_expression_tree (new_context common_ctx cpp_file debug file_info) false (to_block expression) "" "";
      output_cpp "}\n\n";
   | _ -> ());

   let statics_except_meta = statics_except_meta class_def in
   let implemented_fields = List.filter should_implement_field statics_except_meta in
   let dump_field_name = (fun field -> output_cpp ("\t" ^  (str field.cf_name) ^ ",\n")) in
   let implemented_instance_fields = List.filter should_implement_field class_def.cl_ordered_fields in

   List.iter
      (gen_field ctx class_def class_name smart_class_name dot_name false class_def.cl_interface)
      class_def.cl_ordered_fields;
   List.iter
      (gen_field ctx class_def class_name smart_class_name dot_name true class_def.cl_interface) statics_except_meta;
   output_cpp "\n";

   let override_iteration = (not nativeGen) && (has_new_gc_references class_def) in

   (* Initialise non-static variables *)
   if ( (not class_def.cl_interface) && (not nativeGen) ) then begin
      output_cpp (class_name ^ "::" ^ class_name ^  "()\n{\n");
      if (implement_dynamic) then
         output_cpp "\tHX_INIT_IMPLEMENT_DYNAMIC;\n";
      List.iter
         (fun field -> let remap_name = keyword_remap field.cf_name in
            match field.cf_expr with
            | Some { eexpr = TFunction function_def } ->
                  if (is_dynamic_haxe_method field) then
                     output_cpp ("\t" ^ remap_name ^ " = new __default_" ^ remap_name ^ "(this);\n")
            | _ -> ()
         )
         class_def.cl_ordered_fields;
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
         let super_needs_iteration = find_next_super_iteration class_def in
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


      let reflect_member_fields = List.filter (reflective class_def) class_def.cl_ordered_fields in
      let reflect_member_readable = List.filter (is_readable class_def) reflect_member_fields in
      let reflect_member_writable = List.filter (is_writable class_def) reflect_member_fields in
      let reflect_write_member_variables = List.filter variable_field reflect_member_writable in

      let reflect_static_fields = List.filter (reflective class_def) (statics_except_meta) in
      let reflect_static_readable = List.filter (is_readable class_def) reflect_static_fields in
      let reflect_static_writable = List.filter (is_writable class_def) reflect_static_fields in
      let reflect_write_static_variables = List.filter variable_field reflect_static_writable in

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



      if (has_get_member_field class_def) then begin
         (* Dynamic "Get" Field function - string version *)
         output_cpp ("Dynamic " ^ class_name ^ "::__Field(const ::String &inName,hx::PropertyAccess inCallProp)\n{\n");
         let get_field_dat = List.map (fun f ->
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_read = AccCall } when is_extern_field f -> "if (" ^ (checkPropCall f) ^ ") return " ^(keyword_remap ("get_" ^ f.cf_name)) ^ "()"
               | Var { v_read = AccCall } -> "return " ^ (checkPropCall f) ^ " ? " ^ (keyword_remap ("get_" ^ f.cf_name)) ^ "() : " ^
                     ((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()")
               | _ -> "return " ^ ((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()")
               ) ^ ";"
            ) )
         in
         dump_quick_field_test (get_field_dat reflect_member_readable);
         if (implement_dynamic) then
            output_cpp "\tHX_CHECK_DYNAMIC_GET_FIELD(inName);\n";
         output_cpp ("\treturn super::__Field(inName,inCallProp);\n}\n\n");

         (* Dynamic "Get" Field function - int version *)
         if ( field_integer_numeric || field_integer_dynamic) then begin
            let dump_static_ids = (fun field ->
               let remap_name = keyword_remap field.cf_name in
               output_cpp ("static int __id_" ^ remap_name ^ " = __hxcpp_field_to_id(\"" ^
                              (field.cf_name) ^ "\");\n");
               ) in
            List.iter dump_static_ids reflect_member_readable;
            output_cpp "\n\n";


            let output_ifield return_type function_name all_fields =
            output_cpp (return_type ^" " ^ class_name ^ "::" ^ function_name ^ "(int inFieldID)\n{\n");
            let dump_field_test = (fun f ->
               let remap_name = keyword_remap f.cf_name in
               output_cpp ("\tif (inFieldID==__id_" ^ remap_name ^ ") return "  ^
                  ( if (return_type="Float") then "hx::ToDouble( " else "" ) ^
                  (match f.cf_kind with
                  | Var { v_read = AccCall } -> (keyword_remap ("get_" ^ f.cf_name)) ^ "()"
                  | _ -> (remap_name ^ if ( variable_field f) then "" else "_dyn()")
                  ) ^ ( if (return_type="Float") then " ) " else "" ) ^ ";\n");
               ) in
            List.iter dump_field_test (List.filter (fun f -> all_fields || (is_numeric_field f)) reflect_member_readable);
            if (implement_dynamic) then
               output_cpp "\tHX_CHECK_DYNAMIC_GET_INT_FIELD(inFieldID);\n";
            output_cpp ("\treturn super::" ^ function_name ^ "(inFieldID);\n}\n\n");
            in

            if (field_integer_dynamic) then output_ifield "Dynamic" "__IField" true;
            if (field_integer_numeric) then output_ifield "double" "__INumField" false;
         end;
      end;

      if (has_get_static_field class_def) then begin
         output_cpp ("bool " ^ class_name ^ "::__GetStatic(const ::String &inName, Dynamic &outValue, hx::PropertyAccess inCallProp)\n{\n");
         let get_field_dat = List.map (fun f ->
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_read = AccCall } when is_extern_field f -> "if (" ^ (checkPropCall f) ^ ") { outValue = " ^(keyword_remap ("get_" ^ f.cf_name)) ^ "(); return true; }"
               | Var { v_read = AccCall } -> "outValue = " ^ (checkPropCall f) ^ " ? " ^ (keyword_remap ("get_" ^ f.cf_name)) ^ "() : " ^
                     ((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()") ^ "; return true;";
               | _ -> "outValue = " ^ ((keyword_remap f.cf_name) ^ (if (variable_field f) then "" else "_dyn()") ^ "; return true; ")
               )
            ) )
         in
         dump_quick_field_test (get_field_dat reflect_static_readable);
         output_cpp ("\treturn false;\n}\n\n");
      end;

      (* Dynamic "Set" Field function *)
      if (has_set_member_field class_def) then begin

         output_cpp ("Dynamic " ^ class_name ^ "::__SetField(const ::String &inName,const Dynamic &inValue,hx::PropertyAccess inCallProp)\n{\n");

         let set_field_dat = List.map (fun f ->
            let default_action =
               (keyword_remap f.cf_name) ^ "=inValue.Cast< " ^ (type_string f.cf_type) ^ " >();" ^
                  " return inValue;" in
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_write = AccCall } -> "if (" ^ (checkPropCall f) ^ ") return " ^ (keyword_remap ("set_" ^ f.cf_name)) ^ "(inValue);"
                  ^ ( if is_extern_field f then "" else default_action )
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
               (keyword_remap f.cf_name) ^ "=ioValue.Cast< " ^ (type_string f.cf_type) ^ " >(); return true;" in
            (f.cf_name, String.length f.cf_name,
               (match f.cf_kind with
               | Var { v_write = AccCall } -> "if (" ^ (checkPropCall f) ^ ")  ioValue = " ^ (keyword_remap ("set_" ^ f.cf_name)) ^ "(ioValue);"
                  ^ ( if is_extern_field f then "" else " else " ^ default_action )
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
         output_cpp "static hx::StorageInfo sMemberStorageInfo[] = {\n";
         List.iter dump_member_storage stored_fields;
         output_cpp "\t{ hx::fsUnknown, 0, null()}\n};\n";
      end else
         output_cpp "static hx::StorageInfo *sMemberStorageInfo = 0;\n";

      let stored_statics = List.filter is_data_member implemented_fields in
      if ( (List.length stored_statics) > 0) then begin
         output_cpp "static hx::StaticInfo sStaticStorageInfo[] = {\n";
         List.iter dump_static_storage stored_statics;
         output_cpp "\t{ hx::fsUnknown, 0, null()}\n};\n";
      end else
         output_cpp "static hx::StaticInfo *sStaticStorageInfo = 0;\n";

      output_cpp "#endif\n\n";
   end; (* cl_interface *)

   let reflective_members = List.filter (reflective class_def) implemented_instance_fields in
   let sMemberFields = if List.length reflective_members>0 then begin
      output_cpp "static ::String sMemberFields[] = {\n";
      List.iter dump_field_name  reflective_members;
      output_cpp "\t::String(null()) };\n\n";
      "sMemberFields"
   end else
      "0 /* sMemberFields */";
   in

   if (not nativeGen) then begin
      (* Mark static variables as used *)
      output_cpp "static void sMarkStatics(HX_MARK_PARAMS) {\n";
      output_cpp ("\tHX_MARK_MEMBER_NAME(" ^ class_name ^ "::__mClass,\"__mClass\");\n");
      List.iter (fun field ->
         if (is_data_member field) then
            output_cpp ("\tHX_MARK_MEMBER_NAME(" ^ class_name ^ "::" ^ (keyword_remap field.cf_name) ^ ",\"" ^  field.cf_name ^ "\");\n") )
         implemented_fields;
      output_cpp "};\n\n";

      (* Visit static variables *)
      output_cpp "#ifdef HXCPP_VISIT_ALLOCS\n";
      output_cpp "static void sVisitStatics(HX_VISIT_PARAMS) {\n";
      output_cpp ("\tHX_VISIT_MEMBER_NAME(" ^ class_name ^ "::__mClass,\"__mClass\");\n");
      List.iter (fun field ->
         if (is_data_member field) then
            output_cpp ("\tHX_VISIT_MEMBER_NAME(" ^ class_name ^ "::" ^ (keyword_remap field.cf_name) ^ ",\"" ^  field.cf_name ^ "\");\n") )
         implemented_fields;
      output_cpp "};\n\n";
      output_cpp "#endif\n\n";
   end;

   let script_type t optional = if optional then "Object" else
   match type_string t with
   | "bool" -> "Int"
   | "int" -> "Int"
   | "Float" -> "Float"
   | "::String" -> "String"
   | "Null" -> "Void"
   | "Void" -> "Void"
   | _ -> "Object"
   in
   let script_signature t optional = match script_type t optional with
   | "Bool" -> "b"
   | "Int" -> "i"
   | "Float" -> "f"
   | "String" -> "s"
   | "Void" -> "v"
   | _ -> "o"
   in
   let script_size_type t optional = match script_type t optional with
   | "Object" -> "void *"
   | x -> x
   in

   let generate_script_function isStatic field scriptName callName =
      match follow field.cf_type  with
      | TFun (args,return_type) ->
         output_cpp ("\nstatic void " ^ scriptName ^ "(hx::CppiaCtx *ctx) {\n");
         let ret = script_signature return_type false in
         if (ret<>"v") then output_cpp ("ctx->return" ^ (script_type return_type false) ^ "(");
         if isStatic then
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


   if (scriptable && not nativeGen) then begin
      let dump_script_field idx (field,f_args,return_t) =
      let args = if (class_def.cl_interface) then
            gen_tfun_interface_arg_list f_args
         else
            gen_tfun_arg_list f_args in
      let names = List.map (fun (n,_,_) -> keyword_remap n) f_args in
      let return_type = type_string return_t in
      let ret = if (return_type="Void") then " " else "return " in
      let name = keyword_remap field.cf_name in
      let vtable =  "__scriptVTable[" ^ (string_of_int (idx+1) ) ^ "] " in
      let args_varray = (List.fold_left (fun l n -> l ^ ".Add(" ^ n ^ ")") "Array<Dynamic>()" names) in
      output_cpp ("	" ^ return_type ^ " " ^ name ^ "( " ^ args ^ " ) { ");
      output_cpp ("\n\tif (" ^ vtable ^ ") {\n" );
      output_cpp ("\t\thx::CppiaCtx *__ctx = hx::CppiaCtx::getCurrent();\n" );
      output_cpp ("\t\thx::AutoStack __as(__ctx);\n" );
      output_cpp ("\t\t__ctx->pushObject(" ^ (if class_def.cl_interface then "mDelegate.mPtr" else "this" ) ^");\n" );
      List.iter (fun (name,opt, t ) ->
         output_cpp ("\t\t__ctx->push" ^ (script_type t opt) ^ "(" ^ (keyword_remap name) ^ ");\n" );
      ) f_args;
      output_cpp ("\t\t" ^ ret ^ "__ctx->run" ^ (script_type return_t false) ^ "(" ^ vtable ^ ");\n" );
      output_cpp ("\t}  else " ^ ret );

      if (class_def.cl_interface) then begin
         output_cpp (" mDelegate->__Field(HX_CSTRING(\"" ^ field.cf_name ^ "\"), hx::paccNever)");
         if (List.length names <= 5) then
            output_cpp ("->__run(" ^ (String.concat "," names) ^ ");")
         else
            output_cpp ("->__Run(" ^ args_varray ^ ");");
      end else
         output_cpp (class_name ^ "::" ^ name ^ "(" ^ (String.concat "," names)^ ");");
      output_cpp ("return null(); }\n");
      if (class_def.cl_interface) then begin
      output_cpp ("	Dynamic " ^ name ^ "_dyn() { return mDelegate->__Field(HX_CSTRING(\"" ^ field.cf_name ^ "\"), hx::paccNever); }\n\n");

      end
      in

      let not_toString = fun (field,args,_) -> field.cf_name<>"toString" || class_def.cl_interface in
      let functions = List.filter not_toString (all_virtual_functions class_def) in
      let new_sctipt_functions = List.filter (fun (f,_,_) -> not (is_override class_def f.cf_name) ) functions in
      let sctipt_name = class_name ^ "__scriptable" in
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

      list_iteri dump_script_field functions;
      output_cpp ("};\n\n");

      if (List.length new_sctipt_functions) > 0 then begin
         let sigs = Hashtbl.create 0 in
         List.iter (fun (f,_,_) ->
            let s = generate_script_function false f ("__s_" ^f.cf_name) (keyword_remap f.cf_name) in
            Hashtbl.add sigs f.cf_name s
         ) new_sctipt_functions;

         output_cpp "static hx::ScriptNamedFunction __scriptableFunctions[] = {\n";
         List.iter (fun (f,_,_) ->
            let s = try Hashtbl.find sigs f.cf_name with Not_found -> "v" in
            output_cpp ("  hx::ScriptNamedFunction(\"" ^ f.cf_name ^ "\",__s_" ^ f.cf_name ^ ",\"" ^ s ^ "\"),\n" ) ) new_sctipt_functions;
         output_cpp "  hx::ScriptNamedFunction(0,0,0) };\n";
      end else
         output_cpp "static hx::ScriptNamedFunction *__scriptableFunctions = 0;\n";
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
         output_cpp "static ::String sStaticFields[] = {\n";
         List.iter dump_field_name  reflective_statics;
         output_cpp "\t::String(null()) };\n\n";
         "sStaticFields";
      end else
        "0 /* sStaticFields */"
      in

      output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
      output_cpp ("\thx::Static(__mClass) = new hx::Class_obj();\n");
      output_cpp ("\t__mClass->mName = " ^  (str class_name_text)  ^ ";\n");
      output_cpp ("\t__mClass->mSuper = &super::__SGetClass();\n");
      output_cpp ("\t__mClass->mConstructEmpty = &__CreateEmpty;\n");
      output_cpp ("\t__mClass->mConstructArgs = &__Create;\n");
      output_cpp ("\t__mClass->mGetStaticField = &" ^ (
         if (has_get_static_field class_def) then class_name ^ "::__GetStatic;\n" else "hx::Class_obj::GetNoStaticField;\n" ));
      output_cpp ("\t__mClass->mSetStaticField = &" ^ (
         if (has_set_static_field class_def) then class_name ^ "::__SetStatic;\n" else "hx::Class_obj::SetNoStaticField;\n" ));
      output_cpp ("\t__mClass->mMarkFunc = sMarkStatics;\n");
      output_cpp ("\t__mClass->mStatics = hx::Class_obj::dupFunctions(" ^ sStaticFields ^ ");\n");
      output_cpp ("\t__mClass->mMembers = hx::Class_obj::dupFunctions(" ^ sMemberFields ^ ");\n");
      output_cpp ("\t__mClass->mCanCast = hx::TCanCast< " ^ class_name ^ " >;\n");
      output_cpp ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = sVisitStatics;\n#endif\n");
      output_cpp ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mMemberStorageInfo = sMemberStorageInfo;\n#endif\n");
      output_cpp ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mStaticStorageInfo = sStaticStorageInfo;\n#endif\n");
      output_cpp ("\thx::RegisterClass(__mClass->mName, __mClass);\n");
      if (scriptable) then
         output_cpp ("  HX_SCRIPTABLE_REGISTER_CLASS(\""^class_name_text^"\"," ^ class_name ^ ");\n");
      output_cpp ("}\n\n");

   end else if not nativeGen then begin
      output_cpp ("hx::Class " ^ class_name ^ "::__mClass;\n\n");

      output_cpp ("void " ^ class_name ^ "::__register()\n{\n");

      output_cpp ("\thx::Static(__mClass) = new hx::Class_obj();\n");
      output_cpp ("\t__mClass->mName = " ^  (str class_name_text)  ^ ";\n");
      output_cpp ("\t__mClass->mSuper = &super::__SGetClass();\n");
      output_cpp ("\t__mClass->mMarkFunc = sMarkStatics;\n");
      (*output_cpp ("\t__mClass->mStatics = hx::Class_obj::dupFunctions(" ^ sStaticFields ^ ");\n");*)
      output_cpp ("\t__mClass->mMembers = hx::Class_obj::dupFunctions(" ^ sMemberFields ^ ");\n");
      output_cpp ("\t__mClass->mCanCast = hx::TCanCast< " ^ class_name ^ " >;\n");
      output_cpp ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = sVisitStatics;\n#endif\n");
      output_cpp ("\thx::RegisterClass(__mClass->mName, __mClass);\n");
      if (scriptable) then
         output_cpp ("  HX_SCRIPTABLE_REGISTER_INTERFACE(\""^class_name_text^"\"," ^ class_name ^ ");\n");
      output_cpp ("}\n\n");
   end;

   if (has_boot_field class_def) then begin
      output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
      List.iter (gen_field_init ctx ) (List.filter should_implement_field class_def.cl_ordered_statics);
      output_cpp ("}\n\n");
   end;


   gen_close_namespace output_cpp class_path;

   cpp_file#close;


   let h_file = new_header_file common_ctx common_ctx.file class_path in
   let super = match class_def.cl_super with
      | Some (klass,params) -> (class_string klass "_obj" params true)
      | _ when nativeGen -> ""
      | _ -> if (class_def.cl_interface) then "hx::Interface" else "hx::Object"
      in
   let output_h = (h_file#write) in
   let def_string = join_class_path class_path "_"  in
   ctx.ctx_output <- output_h;

   begin_header_file output_h def_string;

   (* Include the real header file for the super class *)
   (match class_def.cl_super with
   | Some super ->
      let super_path = (fst super).cl_path in
      h_file#add_include super_path
   | _ -> () );

   (* And any interfaces ... *)
   List.iter (fun imp-> h_file#add_include (fst imp).cl_path)
      (real_interfaces class_def.cl_implements);

   (* Only need to foreward-declare classes that are mentioned in the header file
      (ie, not the implementation)  *)
   let referenced = find_referenced_types ctx.ctx_common (TClassDecl class_def) super_deps (Hashtbl.create 0) true false scriptable in
   List.iter ( gen_forward_decl h_file ) referenced;

   output_h ( get_class_code class_def Meta.HeaderCode );
   let inc = get_meta_string_path class_def.cl_meta Meta.HeaderInclude in
   if (inc<>"") then
      output_h ("#include \"" ^ inc ^ "\"\n");

   gen_open_namespace output_h class_path;
   output_h "\n\n";
   output_h ( get_class_code class_def Meta.HeaderNamespaceCode );

   let extern_class =  Common.defined common_ctx Define.DllExport in
   let attribs = "HXCPP_" ^ (if extern_class then "EXTERN_" else "") ^ "CLASS_ATTRIBUTES " in

   if (super="") then begin
      output_h ("class " ^ attribs ^ " " ^ class_name);
      output_h "{\n\tpublic:\n";
   end else begin
      output_h ("class " ^ attribs ^ " " ^ class_name ^ " : public " ^ super );
      output_h "{\n\tpublic:\n";
      output_h ("\t\ttypedef " ^ super ^ " super;\n");
      output_h ("\t\ttypedef " ^ class_name ^ " OBJ_;\n");
   end;

   if (not class_def.cl_interface && not nativeGen) then begin
      output_h ("\t\t" ^ class_name ^  "();\n");
      output_h ("\t\tVoid __construct(" ^ constructor_type_args ^ ");\n");
      output_h "\n\tpublic:\n";
      let new_arg = if (has_gc_references class_def) then "true" else "false" in
      output_h ("\t\tinline void *operator new( size_t inSize, bool inContainer=" ^ new_arg
         ^",const char *inName=" ^ (const_char_star class_name_text )^ ")\n" );
      output_h ("\t\t\t{ return hx::Object::operator new(inSize,inContainer,inName); }\n" );
      output_h ("\t\tstatic " ^ptr_name^ " __new(" ^constructor_type_args ^");\n");
      output_h ("\t\tstatic Dynamic __CreateEmpty();\n");
      output_h ("\t\tstatic Dynamic __Create(hx::DynamicArray inArgs);\n");
      if (scriptable) then
         output_h ("\t\tstatic hx::ScriptFunction __script_construct;\n");
      output_h ("\t\t//~" ^ class_name ^ "();\n\n");
      output_h ("\t\tHX_DO_RTTI_ALL;\n");
      if (has_get_member_field class_def) then
         output_h ("\t\tDynamic __Field(const ::String &inString, hx::PropertyAccess inCallProp);\n");
      if (has_get_static_field class_def) then
         output_h ("\t\tstatic bool __GetStatic(const ::String &inString, Dynamic &outValue, hx::PropertyAccess inCallProp);\n");
      if (has_set_member_field class_def) then
         output_h ("\t\tDynamic __SetField(const ::String &inString,const Dynamic &inValue, hx::PropertyAccess inCallProp);\n");
      if (has_set_static_field class_def) then
         output_h ("\t\tstatic bool __SetStatic(const ::String &inString, Dynamic &ioValue, hx::PropertyAccess inCallProp);\n");
      if (has_get_fields class_def) then
         output_h ("\t\tvoid __GetFields(Array< ::String> &outFields);\n");

      if (field_integer_dynamic) then output_h "\t\tDynamic __IField(int inFieldID);\n";
      if (field_integer_numeric) then output_h "\t\tdouble __INumField(int inFieldID);\n";
      if (implement_dynamic) then
         output_h ("\t\tHX_DECLARE_IMPLEMENT_DYNAMIC;\n");
      output_h ("\t\tstatic void __register();\n");
      if (override_iteration) then begin
         output_h ("\t\tvoid __Mark(HX_MARK_PARAMS);\n");
         output_h ("\t\tvoid __Visit(HX_VISIT_PARAMS);\n");
      end;

      if ( (List.length implemented) > 0 ) then begin
         output_h "\t\thx::Object *__ToInterface(const hx::type_info &inType);\n";

         List.iter (fun interface_name ->
            output_h ("\t\toperator " ^ interface_name ^ "_obj *();\n")
         ) implemented;
      end;

      if (has_init_field class_def) then
         output_h "\t\tstatic void __init__();\n\n";
      output_h ("\t\t::String __ToString() const { return " ^ (str smart_class_name) ^ "; }\n\n");
   end else if not nativeGen then begin
      output_h ("\t\tHX_DO_INTERFACE_RTTI;\n");
   end;
   if (has_boot_field class_def) then
      output_h ("\t\tstatic void __boot();\n");


   (match class_def.cl_array_access with
   | Some t -> output_h ("\t\ttypedef " ^ (type_string t) ^ " __array_access;\n")
   | _ -> ());


   List.iter (gen_member_def ctx class_def true class_def.cl_interface) (List.filter should_implement_field class_def.cl_ordered_statics);

   if class_def.cl_interface then begin
      let dumped = ref PMap.empty in
      let rec dump_def interface =
         List.iter (fun field -> try ignore (PMap.find field.cf_name !dumped) with Not_found ->
         begin
            dumped := PMap.add field.cf_name true !dumped;
            gen_member_def ctx interface false true field
         end
         ) interface.cl_ordered_fields;
         List.iter (fun impl -> dump_def (fst impl)) (real_interfaces interface.cl_implements);
      in
      (* Dump this class, not its super, but also its implements *)
      dump_def class_def;
      List.iter (fun impl -> dump_def (fst impl)) (real_interfaces class_def.cl_implements);
   end else begin
      List.iter (gen_member_def ctx class_def false false) (List.filter should_implement_field class_def.cl_ordered_fields);
   end;


   output_h ( get_class_code class_def Meta.HeaderClassCode );
   output_h "};\n\n";

   if (class_def.cl_interface && not nativeGen) then begin
      output_h ("\n\n");
      output_h ("template<typename IMPL>\n");
      output_h ("class " ^ smart_class_name ^ "_delegate_ : public " ^ class_name^"\n");
      output_h "{\n\tprotected:\n";
      output_h ("\t\tIMPL *mDelegate;\n");
      output_h "\tpublic:\n";
      output_h ("\t\t" ^ smart_class_name ^ "_delegate_(IMPL *inDelegate) : mDelegate(inDelegate) {}\n");
      output_h ("\t\thx::Object *__GetRealObject() { return mDelegate; }\n");
      output_h ("\t\tvoid __Visit(HX_VISIT_PARAMS) { HX_VISIT_OBJECT(mDelegate); }\n");

      let dumped = ref PMap.empty in
      let rec dump_delegate interface =
         List.iter (fun field -> try ignore (PMap.find field.cf_name !dumped) with Not_found ->
         begin
            dumped := PMap.add field.cf_name true !dumped;
            match follow field.cf_type, field.cf_kind  with
            | _, Method MethDynamic -> ()
            | TFun (args,return_type), Method _ ->
               let remap_name = keyword_remap field.cf_name in
               output_h ( "		"  ^ (type_string return_type) ^ " " ^ remap_name ^ "( " );
               output_h (gen_tfun_interface_arg_list args);
               output_h (") { return mDelegate->" ^ remap_name^ "(");
               output_h (String.concat "," (List.map (fun (name,opt,typ) -> (keyword_remap name)) args));
               output_h ");}\n";
               if reflective interface field then
                  output_h ("		Dynamic " ^ remap_name ^ "_dyn() { return mDelegate->" ^ remap_name ^ "_dyn();}\n");
            | _ -> ()
         end
         ) interface.cl_ordered_fields;

         match interface.cl_super with | Some super -> dump_delegate (fst super) | _ -> ();
         List.iter (fun impl -> dump_delegate (fst impl)) (real_interfaces interface.cl_implements);
      in
      dump_delegate class_def;
      List.iter (fun impl -> dump_delegate (fst impl)) (real_interfaces class_def.cl_implements);
      output_h "};\n\n";
   end;


   gen_close_namespace output_h class_path;

   end_header_file output_h def_string;
   h_file#close;
   let depend_referenced = find_referenced_types ctx.ctx_common (TClassDecl class_def) super_deps constructor_deps false true false in
   depend_referenced;;


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
   resource_file#write "#include <hxcpp.h>\n\n";
   resource_file#write "namespace hx { \n\n";

   idx := 0;
   Hashtbl.iter (fun _ data ->
      let id = "__res_" ^ (string_of_int !idx) in
      resource_file#write_i ("extern unsigned char " ^ id ^ "[];\n");
      incr idx;
   ) common_ctx.resources;

   resource_file#write "}\n\n";

   idx := 0;
   resource_file#write "hx::Resource __Resources[] =";
   resource_file#begin_block;
   Hashtbl.iter (fun name data ->
      let id = "__res_" ^ (string_of_int !idx) in
      resource_file#write_i
         ("{ " ^ (str name) ^ "," ^ (string_of_int (String.length data)) ^ "," ^
            "hx::" ^ id ^ " + 4 },\n");
      incr idx;
   ) common_ctx.resources;

   resource_file#write_i "{::String(null()),0,0}";
   resource_file#end_block_line;
   resource_file#write ";\n\n";
   resource_file#write "namespace hx { Resource *GetResources() { return __Resources; } } \n\n";
   resource_file#close;;



let write_build_data common_ctx filename classes main_deps boot_deps build_extra extern_src exe_name =
   let buildfile = open_out filename in
   let include_prefix = get_include_prefix common_ctx true in
   let add_class_to_buildfile class_path deps  =
      let cpp = (join_class_path class_path "/") ^ (source_file_extension common_ctx) in
      output_string buildfile ( "  <file name=\"src/" ^ cpp ^ "\">\n" );
      let project_deps = List.filter (fun path -> not (is_internal_class path) ) deps in
      List.iter (fun path-> output_string buildfile ("   <depend name=\"" ^
      ( match path with
         | (["@verbatim"],file) -> file
         | _ -> "include/" ^ include_prefix ^ (join_class_path path "/") ^ ".h" )
      ^ "\"/>\n") ) project_deps;
      output_string buildfile ( "  </file>\n" )
   in
   let add_classdef_to_buildfile (class_path, deps, _)  = add_class_to_buildfile class_path deps in

   output_string buildfile "<xml>\n";
   output_string buildfile ("<set name=\"HXCPP_API_LEVEL\" value=\"" ^
            (Common.defined_value common_ctx Define.HxcppApiLevel) ^ "\" />\n");
   output_string buildfile "<files id=\"haxe\">\n";
   output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
   List.iter add_classdef_to_buildfile classes;
   add_class_to_buildfile ( [] , "__boot__")  boot_deps;
   add_class_to_buildfile ( [] , "__files__")  [];
   add_class_to_buildfile ( [] , "__resources__")  [];
   output_string buildfile "</files>\n";
   output_string buildfile "<files id=\"__lib__\">\n";
   output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
   add_class_to_buildfile ( [] , "__lib__") main_deps;
   output_string buildfile "</files>\n";
   output_string buildfile "<files id=\"__main__\">\n";
   output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
   add_class_to_buildfile  ( [] , "__main__") main_deps;
   output_string buildfile "</files>\n";
   output_string buildfile "<files id=\"__resources__\">\n";
   let idx = ref 0 in
   Hashtbl.iter (fun _ data ->
      let id = "__res_" ^ (string_of_int !idx) in
      output_string buildfile ("<file name=\"src/resources/" ^ id ^ ".cpp\" />\n");
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
   let add_member class_name interface member =
      match follow member.cf_type, member.cf_kind with
      | _, Var _ when interface -> ()
      | _, Method MethDynamic when interface -> ()
      | TFun (_,ret), _ ->
         (*print_endline (class_name ^ "." ^ member.cf_name ^ "=" ^  (type_string ret) );*)
         Hashtbl.add result (class_name ^ "." ^ member.cf_name) (type_string ret)
      | _,_ when not interface ->
         Hashtbl.add result (class_name ^ "." ^ member.cf_name) (type_string member.cf_type)
      | _ -> ()
      in
   List.iter (fun object_def ->
      (match object_def with
      | TClassDecl class_def ->
         let class_name = "::" ^ (join_class_path class_def.cl_path "::") in
         let rec add_all_fields class_def =
            if class_def.cl_interface then
               List.iter (fun impl -> add_all_fields (fst impl) ) class_def.cl_implements;
            (match  class_def.cl_super with Some super -> add_all_fields (fst super) | _->(););
            List.iter (add_member class_name class_def.cl_interface) class_def.cl_ordered_fields;
            List.iter (add_member class_name class_def.cl_interface) class_def.cl_ordered_statics
         in
         add_all_fields class_def
      | _ -> ( )
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
         List.iter (fun imp -> if not (fst imp).cl_extern then deps := (fst imp).cl_path :: !deps) (real_interfaces class_def.cl_implements);
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

(*

  Exports can now be done with macros and a class list

let rec s_type t =
   let result =
   match t with
   | TMono r -> (match !r with | None -> "Dynamic" | Some t -> s_type t)
   | TEnum (e,tl) -> Ast.s_type_path e.e_path ^ s_type_params tl
   | TInst (c,tl) -> Ast.s_type_path c.cl_path ^ s_type_params tl
   | TType (t,tl) -> Ast.s_type_path t.t_path ^ s_type_params tl
   | TAbstract (abs,pl) when abs.a_impl <> None ->
      s_type (Abstract.get_underlying_type abs pl);
   | TAbstract (a,tl) -> Ast.s_type_path a.a_path ^ s_type_params tl
   | TFun ([],t) -> "Void -> " ^ s_fun t false
   | TFun (l,t) ->
      String.concat " -> " (List.map (fun (s,b,t) ->
         (if b then "?" else "") ^ (""(*if s = "" then "" else s ^ " : "*)) ^ s_fun t true
      ) l) ^ " -> " ^ s_fun t false
   | TAnon a ->
   let fl = PMap.fold (fun f acc -> ((if Meta.has Meta.Optional f.cf_meta then " ?" else " ") ^ f.cf_name ^ " : " ^ s_type f.cf_type) :: acc) a.a_fields [] in
      "{" ^ (if not (is_closed a) then "+" else "") ^  String.concat "," fl ^ " }"
   | TDynamic t2 -> "Dynamic" ^ s_type_params (if t == t2 then [] else [t2])
   | TLazy f -> s_type (!f())
   in
   if result="Array<haxe.io.Unsigned_char__>" then "haxe.io.BytesData" else result

and s_fun t void =
   match follow t with
   | TFun _ -> "(" ^ s_type t ^ ")"
   | TAbstract ({ a_path = ([],"Void") },[]) when void -> "(" ^ s_type t ^ ")"
   | TMono r -> (match !r with | None -> s_type t | Some t -> s_fun t void)
   | TLazy f -> s_fun (!f()) void
   | _ ->  (s_type t)

and s_type_params = function
   | [] -> ""
   | l -> "< " ^ String.concat ", " (List.map s_type  l) ^ " >"

;;


let gen_extern_class common_ctx class_def file_info =
   let file = new_source_file common_ctx common_ctx.file  "extern" ".hx" class_def.cl_path in
   let path = class_def.cl_path in

   let rec remove_all_prefix class_def field t =
      let path = class_def.cl_path in
      let filterPath = fst path @ [snd path] in
      let rec remove_prefix t = match t with
         | TInst ({cl_path=[f],suffix } as cval ,tl) when f=field ->
               TInst ( { cval with cl_path = ([],suffix) }, List.map remove_prefix tl)
         | TInst ({cl_path=cpath,suffix } as cval ,tl) when cpath=filterPath ->
               TInst ( { cval with cl_path = ([],suffix) }, List.map remove_prefix tl)
         | TInst (cval,tl) -> TInst ( cval, List.map remove_prefix tl)
         (*| TInst ({cl_path=prefix} as cval ,tl) ->
            TInst ( { cval with cl_path = ([],snd cval.cl_path) }, List.map (remove_prefix field) tl)*)
         | t -> Type.map remove_prefix t
      in
      let t = remove_prefix t in
      let superred = (match class_def.cl_super with
         | Some (super,_) -> remove_all_prefix super field t
         | _ -> t )
      in
      List.fold_left ( fun t (impl,_) -> remove_all_prefix impl field t ) superred class_def.cl_implements;
      (*
      remove_prefix t
      *)
   in


   let params = function [] -> "" | l ->  "< " ^ (String.concat "," (List.map (fun (n,t) -> n) l) ^ " >")  in
   let output = file#write in

   let print_field stat f =
      let s_type t = s_type (remove_all_prefix class_def f.cf_name t) in
      let args  = function  TFun (args,_) ->
         String.concat "," (List.map (fun (name,opt,t) -> (if opt then "?" else "") ^ name ^":"^ (s_type t)) args) | _ -> "" in
      let ret  = function  TFun (_,ret) -> s_type ret | _ -> "Dynamic" in
      let override = if (is_override class_def f.cf_name ) then "override " else "" in

      output ("\t" ^ (if stat then "static " else "") ^ (if f.cf_public then "public " else "") );
      let s_access mode op name = match mode with
         | AccNormal -> "default"
         | AccNo -> "null"
         | AccNever -> "never"
         | AccResolve -> "resolve"
         | AccCall -> op ^ "_" ^ name
         | AccInline -> "default"
         | AccRequire (n,_) -> "require " ^ n
      in
      (match f.cf_kind, f.cf_name with
      | Var { v_read = AccInline; v_write = AccNever },_ ->
         (match f.cf_expr with Some expr ->
            output ("inline var " ^ f.cf_name ^ ":" ^ (s_type f.cf_type) ^ "=" );
            let ctx = (new_extern_context common_ctx file 1 file_info) in
            gen_expression ctx true expr;
         | _ -> ()  )
      | Var { v_read = AccNormal; v_write = AccNormal },_ -> output ("var " ^ f.cf_name ^ ":" ^ (s_type f.cf_type))
      | Var v,_ -> output ("var " ^ f.cf_name ^ "(" ^ (s_access v.v_read "get" f.cf_name) ^ "," ^ (s_access v.v_write "set" f.cf_name) ^ "):" ^ (s_type f.cf_type))
      | Method _, "new" -> output ("function new(" ^ (args f.cf_type) ^ "):Void")
      | Method MethDynamic, _  -> output ("dynamic function " ^ f.cf_name ^ (params f.cf_params) ^ "(" ^ (args f.cf_type) ^ "):" ^ (ret f.cf_type) )
      | Method _, _  -> output (override ^ "function " ^ f.cf_name ^ (params f.cf_params) ^ "(" ^ (args f.cf_type) ^ "):" ^ (ret f.cf_type) )
      );
      output ";\n\n";
   in

   let s_type t = s_type (remove_all_prefix class_def "*" t) in
   let c = class_def in
   output ( "package " ^ (String.concat "." (fst path)) ^ ";\n" );
   output ( "@:include extern " ^ (if c.cl_private then "private " else "") ^ (if c.cl_interface then "interface" else "class")
            ^ " " ^ (snd path) ^ (params c.cl_params) );
   (match c.cl_super with None -> () | Some (c,pl) -> output (" extends " ^  (s_type (TInst (c,pl)))));
   List.iter (fun (c,pl) -> output ( " implements " ^ (s_type (TInst (c,pl))))) (real_interfaces c.cl_implements);
   (match c.cl_dynamic with None -> () | Some t -> output (" implements Dynamic< " ^ (s_type t) ^ " >"));
   (match c.cl_array_access with None -> () | Some t -> output (" implements ArrayAccess< " ^ (s_type t) ^ " >"));
   output "{\n";
   (match c.cl_constructor with
   | None -> ()
   | Some f -> print_field false f);
   let is_public f = f.cf_public in
   List.iter (print_field false) (List.filter is_public c.cl_ordered_fields);
   List.iter (print_field true) (List.filter is_public c.cl_ordered_statics);
   output "}";
   output "\n";
   file#close
;;




let gen_extern_enum common_ctx enum_def file_info =
   let path = enum_def.e_path in
   let file = new_source_file common_ctx common_ctx.file  "extern" ".hx" path in
   let output = file#write in

   let params = function [] -> "" | l ->  "< " ^ (String.concat "," (List.map (fun (n,t) -> n) l) ^ " >")  in
   output ( "package " ^ (String.concat "." (fst path)) ^ ";\n" );
   output ( "@:include extern " ^ (if enum_def.e_private then "private " else "")
            ^ " enum " ^ (snd path) ^ (params enum_def.e_params) );
   output " {\n";
   let sorted_items = List.sort (fun f1 f2 -> (f1.ef_index - f2.ef_index ) ) (pmap_values enum_def.e_constrs) in
   List.iter (fun constructor ->
      let name = keyword_remap constructor.ef_name in
      match constructor.ef_type with
      | TFun (args,_) ->
         output ( name ^ "(" );
         output ( String.concat "," (List.map (fun (arg,_,t) -> arg ^ ":" ^ (s_type t) ) args) );
         output ");\n\n";
      | _ -> output ( name ^ ";\n\n" )
   ) sorted_items;

   output "}\n";
   file#close
;;
*)

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


let is_assign_op op =
   match op with
   | OpAssign
   | OpAssignOp _ -> true
   | _ -> false
;;

let rec script_type_string haxe_type =
   match haxe_type with
   | TType ({ t_path = ([],"Null") },[t]) ->
      (match follow t with
      | TAbstract ({ a_path = [],"Int" },_)
      | TAbstract ({ a_path = [],"Float" },_)
      | TAbstract ({ a_path = [],"Bool" },_)
      | TInst ({ cl_path = [],"Int" },_)
      | TInst ({ cl_path = [],"Float" },_)
      | TEnum ({ e_path = [],"Bool" },_) -> "Dynamic"
      | _ -> script_type_string t)
   | TInst ({cl_path=[],"Null"},[t]) ->
      (match follow t with
      | TAbstract ({ a_path = [],"Int" },_)
      | TAbstract ({ a_path = [],"Float" },_)
      | TAbstract ({ a_path = [],"Bool" },_)
      | TInst ({ cl_path = [],"Int" },_)
      | TInst ({ cl_path = [],"Float" },_)
      | TEnum ({ e_path = [],"Bool" },_) -> "Dynamic"
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

let rec is_dynamic_in_cppia ctx expr =
   match expr.eexpr with
   | TCast(_,None) -> true
   | _ -> is_dynamic_in_cpp ctx expr
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
;;

class script_writer common_ctx ctx filename asciiOut =
   object(this)
   val debug = asciiOut
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
   method typeTextString typeName = (string_of_int (this#typeId typeName)) ^ " "
   method typeText typeT = (string_of_int (this#typeId (script_type_string typeT))) ^ " "
   method writeType typeT = this#write (this#typeText typeT)
   method boolText value = if value then "1" else "0"
   method writeBool value = this#write (if value then "1 " else "0 ")
   method staticText value = if value then "1" else "0"
   method writeData str = Buffer.add_string buffer str;
   method wint ival = this#write ((string_of_int ival)^" ")
   method ident name = this#wint (this#stringId name)
   method instText clazz = match clazz.cl_path with
      | ([],"Array") -> string_of_int (this#typeId "Array< ::Dynamic >") ^ " "
      | _ -> this#typeText (TInst(clazz,[]))
   method instName clazz = this#write (this#instText clazz)
   method enumText e = this#typeText (TEnum(e,[]))
   method enumName e = this#write (this#enumText e)
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
         let stripped_file = strip_file common_ctx file in
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
            | "int" | "Float" | "bool" | "String" | "unsigned char" ->
               ArrayData typeName
            | "cpp::ArrayBase" | "Dynamic" -> ArrayAny
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
      this#write ( (this#op IaFunction) ^ (this#staticText isStatic) ^ " " ^(this#boolText isDynamic) ^ " " ^(this#stringText funcName) ^ " ");
      this#write ((this#typeTextString "Void") ^ "0\n");
         this#gen_expression fieldExpression
   method func isStatic isDynamic funcName ret args isInterface fieldExpression =
      this#write ( (this#op IaFunction) ^ (this#staticText isStatic) ^ " " ^(this#boolText isDynamic) ^ " " ^(this#stringText funcName) ^ " ");
      this#write ((this#typeText ret) ^ (string_of_int (List.length args)) ^ " ");
      List.iter (fun (name,opt,typ) -> this#write ( (this#stringText name) ^ (this#boolText opt) ^ " " ^ (this#typeText typ) ^ " " )) args;
      this#write "\n";
      if (not isInterface) then begin
         match fieldExpression with
         | Some ({ eexpr = TFunction function_def } as e) -> this#gen_expression e
         | _ -> print_endline ("Missing function body for " ^ funcName );
      end
   method var readAcc writeAcc isExtern isStatic name varType varExpr =
      this#write ( (this#op IaVar) ^ (this#staticText isStatic) ^ " " ^ (this#op readAcc) ^ (this#op writeAcc) ^
         (this#boolText isExtern) ^ " " ^ (this#stringText name)^ (this#typeText varType) ^
         (match varExpr with Some _ -> "1\n" | _ -> "0\n" ) );
      match varExpr with
      | Some expression -> this#gen_expression expression
      | _ -> ()
   method implDynamic = this#writeOpLine IaImplDynamic;
   method writeVar v =
      this#ident v.v_name;
      this#wint v.v_id;
      this#writeBool v.v_capture;
      this#writeType v.v_type;
   method writeList prefix len = this#write (prefix ^" "  ^ (string_of_int (len)) ^ "\n");
   method writePos expr = if debug then
      this#write ( (this#fileText expr.epos.pfile) ^ "\t" ^ (string_of_int (Lexer.get_error_line expr.epos) ) ^ indent);
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
   method gen_expression expr =
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
      let is_real_function field =
         match field.cf_kind with
         | Method MethNormal | Method MethInline-> true
         | _ -> false;
      in
      let gen_call () =
         (match (remove_parens func).eexpr with
         | TField ( { eexpr = TLocal  { v_name = "__global__" }}, field ) ->
                  this#write ( (this#op IaCallGlobal) ^ (this#stringText (field_name field)) ^ argN ^ "\n");
         | TField (obj,FStatic (class_def,field) ) when is_real_function field ->
                  this#write ( (this#op IaCallStatic) ^ (this#instText class_def) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ "\n");
         | TField (obj,FInstance (_,_,field) ) when (is_this obj) && (is_real_function field) ->
                  this#write ( (this#op IaCallThis) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ "\n");
         | TField (obj,FInstance (_,_,field) ) when is_super obj ->
                  this#write ( (this#op IaCallSuper) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ "\n");
         | TField (obj,FInstance (_,_,field) ) when is_real_function field ->
                  this#write ( (this#op IaCallMember) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText field.cf_name) ^
                     argN ^ "\n");
                  this#gen_expression obj;
         | TField (obj,FDynamic (name) )  when (is_internal_member name || (type_string obj.etype = "::String" && name="cca") ) ->
                  this#write ( (this#op IaCallMember) ^ (this#typeText obj.etype) ^ " " ^ (this#stringText name) ^
                     argN ^ "\n");
                  this#gen_expression obj;
         | TConst TSuper -> this#write ((this#op IaCallSuperNew) ^ (this#typeText func.etype) ^ " " ^ argN ^ "\n");
         | TField (_,FEnum (enum,field)) -> this#write ((this#op IaCreateEnum) ^ (this#enumText enum) ^ " " ^ (this#stringText field.ef_name) ^ argN ^ "\n");
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
      (match (remove_parens func).eexpr with
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
      let typeText = this#typeText obj.etype in
      (match acc with
      | FDynamic name -> this#write ( (this#op IaFName) ^ typeText ^ " " ^ (this#stringText name) ^ "\n");
            this#gen_expression obj;
      | FStatic (class_def,field) -> this#write ( (this#op IaFStatic)  ^ (this#instText class_def) ^ " " ^ (this#stringText field.cf_name) );
      | FInstance (_,_,field) when is_this obj -> this#write ( (this#op IaFThisInst) ^ typeText ^ " " ^ (this#stringText field.cf_name) );
      | FInstance (_,_,field) -> this#write ( (this#op IaFLink) ^ typeText ^ " " ^ (this#stringText field.cf_name) ^ "\n");
            this#gen_expression obj;

      | FClosure (_,field) when is_this obj -> this#write ( (this#op IaFThisName) ^typeText ^ " " ^  (this#stringText field.cf_name) ^ "\n")
      | FAnon (field) when is_this obj -> this#write ( (this#op IaFThisName) ^typeText ^ " " ^  (this#stringText field.cf_name) ^ "\n")

      | FClosure (_,field)
      | FAnon (field) -> this#write ( (this#op IaFName) ^typeText ^ " " ^  (this#stringText field.cf_name) ^ "\n");
            this#gen_expression obj;

      | FEnum (enum,field) -> this#write ( (this#op IaFEnum)  ^ (this#enumText enum) ^ " " ^ (this#stringText field.ef_name) );
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
   | TLocal var -> this#write ((this#op IaVar) ^ (string_of_int var.v_id) );

   | TVar (tvar,optional_init) ->
         this#write ( (this#op IaTVars) ^ (string_of_int (1)) ^ "\n");
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
      ("fileName" , { eexpr = (TConst (TString file)) }) ::
         ("lineNumber" , { eexpr = (TConst (TInt line)) }) ::
            ("className" , { eexpr = (TConst (TString class_name)) }) ::
               ("methodName", { eexpr = (TConst (TString meth)) }) :: [] ) ->
            this#write ( (this#op IaPosInfo) ^ (this#stringText file) ^ (Printf.sprintf "%ld" line) ^ " " ^
                        (this#stringText class_name) ^ " " ^  (this#stringText meth))

   | TObjectDecl values ->this#write ( (this#op IaObjDef) ^ (string_of_int (List.length values)));
         this#write " ";
         List.iter (fun (name,_) -> this#write (this#stringText name)  ) values;
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
   | TCast (cast,None) -> this#checkCast expression.etype cast true true;
   | TCast (cast,Some _) -> this#checkCast expression.etype cast true true;
   | TParenthesis _ -> error "Unexpected parens" expression.epos
   | TMeta(_,_) -> error "Unexpected meta" expression.epos
   );
   this#end_expr;
end;;

let generate_script_class common_ctx script class_def =
   script#incClasses;
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
         | AccNormal -> IaAccessNormal
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
         let isExtern = is_extern_field field in
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


let generate_cppia common_ctx =
   let debug = 1 in
   let null_file = new source_writer common_ctx ignore (fun () -> () ) in
   let ctx = new_context common_ctx null_file debug (ref PMap.empty) in
   ctx.ctx_class_member_types <- create_member_types common_ctx;
   let script = new script_writer common_ctx ctx common_ctx.file common_ctx.debug in
   ignore (script#stringId "");
   ignore (script#typeId "");

      List.iter (fun object_def ->
      (match object_def with
      | TClassDecl class_def when class_def.cl_extern  ->
         () (*if (gen_externs) then gen_extern_class common_ctx class_def;*)
      | TClassDecl class_def ->
         let is_internal = is_internal_class class_def.cl_path in
         if (is_internal || (is_macro class_def.cl_meta)) then
            ( if (debug>1) then print_endline (" internal class " ^ (join_class_path class_def.cl_path ".") ))
         else begin
            ctx.ctx_class_name <- "::" ^ (join_class_path class_def.cl_path "::");
            generate_script_class common_ctx script class_def
         end
      | TEnumDecl enum_def when enum_def.e_extern -> ()
      | TEnumDecl enum_def ->
         let is_internal = is_internal_class enum_def.e_path in
         if (is_internal) then
            (if (debug>1) then print_endline (" internal enum " ^ (join_class_path enum_def.e_path ".") ))
         else begin
            let meta = Codegen.build_metadata common_ctx object_def in
            if (enum_def.e_extern) then
               (if (debug>1) then print_endline ("external enum " ^  (join_class_path enum_def.e_path ".") ));
            ctx.ctx_class_name <- "*";
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
let generate_source common_ctx =
   make_base_directory common_ctx.file;

   let debug = 1 in
   let exe_classes = ref [] in
   let boot_classes = ref [] in
   let boot_enums = ref [] in
   let nonboot_classes = ref [] in
   let init_classes = ref [] in
   let file_info = ref PMap.empty in
   let class_text path = join_class_path path "::" in
   let member_types = create_member_types common_ctx in
   let super_deps = create_super_dependencies common_ctx in
   let constructor_deps = create_constructor_dependencies common_ctx in
   let main_deps = ref [] in
   let extern_src = ref [] in
   let build_xml = ref "" in
   let scriptable = (Common.defined common_ctx Define.Scriptable) in

   List.iter (fun object_def ->
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
            ( if (debug>1) then print_endline (" internal class " ^ name ))
         else begin
            build_xml := !build_xml ^ (get_class_code class_def Meta.BuildXml);
            if (has_init_field class_def) then
               init_classes := class_def.cl_path ::  !init_classes;
            if (has_boot_field class_def) then
               boot_classes := class_def.cl_path ::  !boot_classes
            else if not (has_meta_key class_def.cl_meta Meta.NativeGen) then
               nonboot_classes := class_def.cl_path ::  !nonboot_classes;
            let deps = generate_class_files common_ctx
               member_types super_deps constructor_deps class_def file_info scriptable in
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
            let deps = generate_enum_files common_ctx enum_def super_deps meta file_info in
            exe_classes := (enum_def.e_path, deps, object_def) :: !exe_classes;
         end
      | TTypeDecl _ | TAbstractDecl _ -> (* already done *) ()
      );
   ) common_ctx.types;


   (match common_ctx.main with
   | None -> generate_dummy_main common_ctx
   | Some e ->
      let main_field = { cf_name = "__main__"; cf_type = t_dynamic; cf_expr = Some e; cf_pos = e.epos; cf_public = true; cf_meta = []; cf_overloads = []; cf_doc = None; cf_kind = Var { v_read = AccNormal; v_write = AccNormal; }; cf_params = [] } in
      let class_def = { null_class with cl_path = ([],"@Main"); cl_ordered_statics = [main_field] } in
      main_deps := find_referenced_types common_ctx (TClassDecl class_def) super_deps constructor_deps false true false;
      generate_main common_ctx member_types super_deps class_def file_info
   );

   generate_boot common_ctx !boot_enums !boot_classes !nonboot_classes !init_classes;

   generate_files common_ctx file_info;

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
         let rec stype = function
            | TMono r -> (match !r with None -> "Dynamic" | Some t -> stype t)
            | TAbstract ({ a_path = ([],"Void") },[]) -> "void"
            | TAbstract ({ a_path = ([],"Bool") },[]) -> "bool"
            | TAbstract ({ a_path = ([],"Float") },[]) -> "float"
            | TAbstract ({ a_path = ([],"Int") },[]) -> "int"
            | TAbstract( { a_path = ([], "EnumValue") }, _  ) -> "Dynamic"
            | TEnum (enum,params) -> spath enum.e_path
            | TInst (klass,params) ->
               (match klass.cl_path, params with
               (* Array class *)
               (*|  ([],"Array") when is_dynamic_array_param (List.hd params) -> "Dynamic" *)
               | _,_ when is_dynamic_type_param klass.cl_kind -> "Dynamic"
               | ([],"Array"), [t] -> "Array<" ^ (stype t) ^ ">"
               | (["haxe";"io"],"Unsigned_char__"),_ -> "uint8"
               | ([],"EnumValue"),_ -> "Dynamic"
               | ([],"Null"),[t] when cant_be_null t -> "Null<" ^ (stype t) ^ ">"
               | ([],"Null"),[t] -> (stype t)
               | _ -> spath klass.cl_path
               )
            | TType (type_def,params) ->
               (match type_def.t_path, params with
               | ([],"Null"),[t] when cant_be_null t -> "Null<" ^ (stype t) ^ ">"
               | ([],"Array"), [t] -> "Array< " ^ (stype (follow t) ) ^ " >"
               | _,_ ->  stype (apply_params type_def.t_params params type_def.t_type)
               )
            | TLazy func -> stype ((!func)())
            | TAbstract (abs,pl) when abs.a_impl <> None ->
               stype (Abstract.get_underlying_type abs pl)
            | TAbstract (abs,_) -> spath abs.a_path
            | TFun (args,ret) -> "fun<" ^ (List.fold_left (fun s (_,opt,t) -> s ^ (if opt then "?" else "") ^ (stype t) ^ ",") "" args) ^ (stype ret) ^ ">"
            | _ -> "Dynamic"
            in
         List.iter (fun (name,_,def) ->
            match def with
            | TClassDecl class_def ->
                outline ((if class_def.cl_interface then "interface " else "class ") ^ (spath name) );
                (match class_def.cl_super with
                | Some (super,_) -> outline ("super " ^ (spath super.cl_path) )
                | _ -> () );
                List.iter ( fun(c,_) -> out ("implements " ^ (spath c.cl_path) ^ "\n") ) class_def.cl_implements;
                (match class_def.cl_dynamic with None -> () | Some t -> outline ("implementsdynamic " ^ (stype t)));
                (match class_def.cl_array_access with None -> () | Some t -> outline ("arrayaccess " ^ (stype t)));

                let args  = function
                   | TFun (args,_) ->
                       List.iter  (fun (name,opt,t) ->
                          outline ("arg " ^ name ^ (if opt then " ? " else " : ") ^ (stype t) )
                       ) args;
                   | _ -> () in
                let ret  = function  TFun (_,ret) -> stype ret | _ -> "Dynamic" in

                let print_field stat f =
                   let pub = if f.cf_public then "pub " else "priv " in
                   let stat = pub ^ ( if stat then "s " else "m " ) in
                   (match f.cf_kind, f.cf_name with
                   | Var { v_read = AccInline; v_write = AccNever },_ ->
                        outline ("inlinevar " ^ f.cf_name ^ " " ^ (stype f.cf_type) )
                   | Var { v_read = AccNormal; v_write = AccNormal },_ ->
                        outline ("var " ^ stat ^ f.cf_name ^ " " ^ (stype f.cf_type) )
                   | Var v,_ ->
                        let saccess = function | AccNormal -> "v" | AccNo -> "0" | AccNever -> "!"
                           | AccResolve -> "r" | AccCall -> "c" | AccInline -> "i" | AccRequire (_,_) -> "v" in
                        outline ("property " ^ stat ^ (saccess v.v_read) ^ " " ^ (saccess v.v_write)
                           ^ " " ^ f.cf_name ^ " " ^ (stype f.cf_type) )
                   | Method _, "new" ->
                        outline ("function " ^ stat ^ "new " ^ (ret f.cf_type) );
                        args f.cf_type
                   | Method MethDynamic, _  ->
                        outline ("dynamicfunction " ^ stat ^ f.cf_name ^ " " ^ (ret f.cf_type) );
                        args f.cf_type
                   | Method _, _  ->
                        outline ("function " ^ stat ^ f.cf_name ^ " " ^ (ret f.cf_type) );
                        args f.cf_type
                  ) in
                (match class_def.cl_constructor with | None -> () | Some f -> print_field false f);
                List.iter (print_field false) class_def.cl_ordered_fields;
                List.iter (print_field true) class_def.cl_ordered_statics;
            | TEnumDecl enum_def ->
                out ("enum " ^ (spath name) ^ "\n");
                let sorted_items = List.sort (fun f1 f2 -> (f1.ef_index - f2.ef_index ) ) (pmap_values enum_def.e_constrs) in
                List.iter (fun constructor ->
                   outline ("constructor " ^ constructor.ef_name);
                   match constructor.ef_type with
                   | TFun (args,_) -> List.iter (fun (arg,_,t) -> outline ("eparam " ^ arg ^ " " ^ (stype t) ) ) args;
                   | _ -> ()
                ) sorted_items;
            | _ -> ()
            ) !exe_classes;

         (* Output file info too *)
         List.iter ( fun file ->
               let full_path = Common.get_full_path (try Common.find_file common_ctx file with Not_found -> file) in
               out ("file " ^ (escape file) ^ " " ^ (escape full_path) ^"\n") )
            ( List.sort String.compare ( pmap_keys !file_info) );
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
      | _ -> cmd_defines := !cmd_defines ^ " -D" ^ name ^ "=\"" ^ (escape_command value) ^ "\"" ) common_ctx.defines;
   write_build_options common_ctx (common_ctx.file ^ "/Options.txt") common_ctx.defines;
   if ( not (Common.defined common_ctx Define.NoCompilation) ) then begin
      let old_dir = Sys.getcwd() in
      Sys.chdir common_ctx.file;
      let cmd = ref "haxelib run hxcpp Build.xml haxe" in
      if (common_ctx.debug) then cmd := !cmd ^ " -Ddebug";
      cmd := !cmd ^ !cmd_defines;
      cmd := List.fold_left (fun cmd path -> cmd ^ " -I\"" ^ (escape_command path) ^ "\"" ) !cmd common_ctx.class_path;
      print_endline !cmd;
      if common_ctx.run_command !cmd <> 0 then failwith "Build failed";
      Sys.chdir old_dir;
   end
   ;;

let generate common_ctx =
   if (Common.defined common_ctx Define.Cppia) then
      generate_cppia common_ctx
   else
      generate_source common_ctx
;;


