(*
   The Haxe Compiler
   Copyright (C) 2005-2019  Haxe Foundation

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
open Error
open Common
open Globals
open CppStrings
open CppExprUtils
open CppTypeUtils
open CppAst
open CppAstTools
open CppSourceWriter
open CppContext

let make_base_directory dir =
   Path.mkdir_recursive "" ( ( Str.split_delim (Str.regexp "[\\/]+") dir ) )

let get_meta_string meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String(name,_)),_],_) :: _  when k=key-> name
      | _ :: l -> loop l
      in
   loop meta

let get_meta_string_path meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String(name,_)),_], pos) :: _  when k=key->
           make_path_absolute name pos
      | _ :: l -> loop l
      in
   loop meta

let write_resources common_ctx =

   let idx = ref 0 in

   Hashtbl.iter (fun _ data ->
      let id = "__res_" ^ (string_of_int !idx) in
      let resource_file = new_cpp_file common_ctx common_ctx.file (["resources"],id) in
      resource_file#write "namespace hx {\n";
      resource_file#write_i ("unsigned char " ^ id ^ "[] = {\n");
      resource_file#write_i "0x80, 0x00, 0x00, 0x80,\n";
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
   resource_file#write "::hx::Resource __Resources[] = ";
   resource_file#begin_block;
   Hashtbl.iter (fun name data ->
      let id = "__res_" ^ (string_of_int !idx) in
      resource_file#write_i
         ("{ " ^ (strq common_ctx name) ^ "," ^ (string_of_int (String.length data)) ^ "," ^
            "::hx::" ^ id ^ " + 4 },\n");
      incr idx;
   ) common_ctx.resources;

   resource_file#write_i "{ ::String(null()),0,0 }\n";
   resource_file#end_block_line;
   resource_file#write ";\n\n";
   resource_file#write "namespace hx { Resource *GetResources() { return __Resources; } }\n";
   resource_file#close

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
   let api_string = (Common.defined_value common_ctx Define.HxcppApiLevel) in
   output_string buildfile ("<set name=\"HXCPP_API_LEVEL\" value=\"" ^ api_string ^ "\" />\n");
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
   if (Common.defined common_ctx Define.HxcppSmartStings) then
      output_string buildfile ("<error value=\"Hxcpp is out of date - please update\" unlessApi=\"" ^ api_string ^ "\" />\n");
   output_string buildfile "</xml>\n";
   close_out buildfile

let write_build_options common_ctx filename defines =
   let writer = cached_source_writer common_ctx filename in
   let write_define name value = writer#write (Printf.sprintf "%s=%s\n" name value) in
   PMap.iter ( fun name value -> match name with
      | "true" | "sys" | "dce" | "cpp" | "debug" -> ()
      | _ ->  write_define name (escape_command value)) defines;
   let pin,pid = Process_helper.open_process_args_in_pid "haxelib" [|"haxelib"; "path"; "hxcpp"|] in
   set_binary_mode_in pin false;
   write_define "hxcpp" (Stdlib.input_line pin);
   Stdlib.ignore (Process_helper.close_process_in_pid (pin,pid));
   writer#close

let create_member_types common_ctx =
   let result = Hashtbl.create 0 in
      List.iter (fun object_def ->
         (match object_def with
         | TClassDecl class_def when not (has_class_flag class_def CInterface) ->
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
   result

let is_assign_op op =
   match op with
   | OpAssign
   | OpAssignOp _ -> true
   | _ -> false

let generate_class_files ctx class_def =
   (* create header and cpp files *)
   let nativeGen = Meta.has Meta.NativeGen class_def.cl_meta in
   if not (nativeGen && (has_class_flag class_def CInterface)) then
      CppGenClassImplementation.generate ctx class_def;
   CppGenClassHeader.generate ctx class_def

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
   let super_deps = CppGen.create_super_dependencies common_ctx in
   let constructor_deps = CppGen.create_constructor_dependencies common_ctx in
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
         build_xml := !build_xml ^ (CppGen.get_class_code class_def Meta.BuildXml);
         let source = get_meta_string_path class_def.cl_meta Meta.SourceFile in
         if (source<>"") then
            extern_src := source :: !extern_src;
      | TClassDecl class_def ->
         let name =  class_text class_def.cl_path in
         let is_internal = is_internal_class class_def.cl_path in
         if (is_internal || (Meta.has Meta.Macro class_def.cl_meta)) then
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

            build_xml := !build_xml ^ (CppGen.get_class_code class_def Meta.BuildXml);
            if (has_init_field class_def) then
               init_classes := class_def.cl_path ::  !init_classes;
            if (has_boot_field class_def) then
               boot_classes := class_def.cl_path ::  !boot_classes
            else if not (Meta.has Meta.NativeGen class_def.cl_meta) then
               nonboot_classes := class_def.cl_path ::  !nonboot_classes;
            jobs := (fun () -> generate_class_files ctx class_def) :: !jobs;
            let deps = CppReferences.find_referenced_types ctx (TClassDecl class_def) super_deps constructor_deps false true scriptable in
            if not ((has_class_flag class_def CInterface) && (is_native_gen_class class_def)) then
               exe_classes := (class_def.cl_path, deps, object_def)  ::  !exe_classes;
         end
      | TEnumDecl enum_def when has_enum_flag enum_def EnExtern -> ()
      | TEnumDecl enum_def ->
         let name =  class_text enum_def.e_path in
         let is_internal = is_internal_class enum_def.e_path in
         if (is_internal) then
            (if (debug>1) then print_endline (" internal enum " ^ name ))
         else begin
            let rec makeId enum_name seed =
               let id = gen_hash32 seed enum_name in
               (* reserve first 100 ids for runtime *)
               if id < Int32.of_int 100 || Hashtbl.mem existingIds id then
                  makeId enum_name (seed+100)
               else begin
                  Hashtbl.add existingIds id true;
                  Hashtbl.add ctx.ctx_type_ids enum_name id;
               end in
            makeId name 0;

            if (has_enum_flag enum_def EnExtern) then
               (if (debug>1) then print_endline ("external enum " ^ name ));
            boot_enums := enum_def.e_path :: !boot_enums;
            jobs := (fun () -> CppGenEnum.generate ctx enum_def) :: !jobs;
            let deps = CppReferences.find_referenced_types ctx (TEnumDecl enum_def) super_deps (Hashtbl.create 0) false true false in
            exe_classes := (enum_def.e_path, deps, object_def) :: !exe_classes;
         end
      | TTypeDecl _ | TAbstractDecl _ -> (* already done *) ()
      );
   ) common_ctx.types;

   List.iter (fun job -> job () ) !jobs;


   (match common_ctx.main.main_expr with
   | None -> CppGen.generate_dummy_main common_ctx
   | Some e ->
      let main_field = { (mk_field "__main__" t_dynamic e.epos null_pos) with
         cf_expr = Some e;
	  } in
      let class_def = { null_class with cl_path = ([],"@Main"); cl_ordered_statics = [main_field] } in
      main_deps := CppReferences.find_referenced_types ctx (TClassDecl class_def) super_deps constructor_deps false true false;
      CppGen.generate_main ctx super_deps class_def
   );

   CppGen.generate_boot ctx !boot_enums !boot_classes !nonboot_classes !init_classes;

   CppGen.generate_files common_ctx ctx.ctx_file_info;

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
                outline ((if (has_class_flag class_def CInterface) then "interface " else "class ") ^ (spath name) );
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

   let output_name = match  common_ctx.main.main_class with
   | Some path -> (snd path)
   | _ -> "output" in

   write_build_data common_ctx (common_ctx.file ^ "/Build.xml") !exe_classes !main_deps (!boot_enums@ !boot_classes) !build_xml !extern_src output_name;
   write_build_options common_ctx (common_ctx.file ^ "/Options.txt") common_ctx.defines.Define.values;
   if ( not (Common.defined common_ctx Define.NoCompilation) ) then begin
      let t = Timer.timer ["generate";"cpp";"native compilation"] in
      let old_dir = Sys.getcwd() in
      Sys.chdir common_ctx.file;
      let cmd = ref ["run"; "hxcpp"; "Build.xml"; "haxe"] in
	  if (common_ctx.debug) then cmd := !cmd @ ["-Ddebug"];
      PMap.iter ( fun name value -> match name with
         | "true" | "sys" | "dce" | "cpp" | "debug" -> ();
         | _ -> cmd := !cmd @ [Printf.sprintf "-D%s=\"%s\"" name (escape_command value)];
      ) common_ctx.defines.values;
      common_ctx.class_paths#iter (fun path ->
		let path = path#path in
		cmd := !cmd @ [Printf.sprintf "-I%s" (escape_command path)]
	  );
      common_ctx.print ("haxelib " ^ (String.concat " " !cmd) ^ "\n");
      if common_ctx.run_command_args "haxelib" !cmd <> 0 then failwith "Build failed";
      Sys.chdir old_dir;
      t()
   end

let generate common_ctx =
   let debug_level = if (Common.defined common_ctx Define.NoDebug) then 0 else 1 in
   if (Common.defined common_ctx Define.Cppia) then begin
      let ctx = new_context common_ctx debug_level (ref PMap.empty) (Hashtbl.create 0)  in
      CppCppia.generate_cppia ctx
   end else begin
      let ctx = new_context common_ctx debug_level (ref PMap.empty) (create_member_types common_ctx) in
      generate_source ctx
   end