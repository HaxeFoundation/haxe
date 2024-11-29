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
open CppAst
open Gctx
open Type
open Error
open Globals
open CppStrings
open CppTypeUtils
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
   let api_string = (Gctx.defined_value common_ctx Define.HxcppApiLevel) in
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
   if (Gctx.defined common_ctx Define.HxcppSmartStings) then
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
   let folder acc object_def =
      match object_def with
      | TClassDecl class_def when not (has_class_flag class_def CInterface) ->
         let rec add_override acc to_super =
            let class_name = (join_class_path to_super.cl_path ".") in
            let folder acc member = StringMap.add (class_name ^ "." ^ member.cf_name) "virtual " acc in
            let acc = List.fold_left folder acc class_def.cl_ordered_fields in
            match to_super.cl_super with
            | Some (super, _) -> add_override acc super
            | _ -> acc
         in
         (match class_def.cl_super with Some (super, _) -> add_override acc super | _ -> acc)
      | _ -> acc
   in
   List.fold_left folder StringMap.empty common_ctx.types

(* Builds inheritance tree, so header files can include parents defs.  *)
let create_super_dependencies common_ctx =
   let real_non_native_interfaces =
     List.filter (function t, pl ->
      (match (t, pl) with
      | { cl_path = [ "cpp"; "rtti" ], _ }, [] -> false
      | _ -> not (is_native_gen_class t)))
   in
   let folder acc object_def =
     match object_def with
     | TClassDecl class_def when not (has_class_flag class_def CExtern) ->
         let initial = match class_def.cl_super with
         | Some (cls, _) when not (has_class_flag cls CExtern) ->
            [ cls.cl_path ]
         | _ ->
            [] in

         let deps =
            class_def.cl_implements
            |> real_non_native_interfaces
            |> List.fold_left
               (fun acc (cls, _) -> if has_class_flag cls CExtern then acc else cls.cl_path :: acc)
               initial in

         PathMap.add class_def.cl_path deps acc
     | TEnumDecl enum_def when not (has_enum_flag enum_def EnExtern) ->
         PathMap.add enum_def.e_path [] acc
     | _ ->
         acc
   in
   List.fold_left folder PathMap.empty common_ctx.types

let create_constructor_dependencies common_ctx =
   List.fold_left
      (fun acc object_def ->
         match object_def with
         | TClassDecl class_def when not (has_class_flag class_def CExtern) ->
            (match class_def.cl_constructor with
            | Some func -> PathMap.add class_def.cl_path func acc
            | None -> acc)
         | _ -> acc)
      PathMap.empty
      common_ctx.types

let is_assign_op op =
   match op with
   | OpAssign
   | OpAssignOp _ -> true
   | _ -> false

(*
 The common_ctx contains the haxe AST in the "types" field and the resources
*)

type gensrc_ctx = {
   extern_src : string list;
   build_xml : string;
   boot_classes : path list;
   init_classes : path list;
   nonboot_classes : path list;
   boot_enums : path list;
   exe_classes : (path * path list * module_type) list;
   decls : tcpp_decl list;
   ids : CppAst.ObjectIds.t;
   slots : CppAst.InterfaceSlots.t;
}

let generate_source ctx =
   let common_ctx = ctx.ctx_common in
   make_base_directory common_ctx.file;
   let main_deps = ref [] in
   let scriptable = (Gctx.defined common_ctx Define.Scriptable) in

   let initial = {
      extern_src = [];
      build_xml = "";
      boot_classes = [];
      init_classes = [];
      nonboot_classes = [];
      boot_enums = [];
      exe_classes = [];
      decls = [];
      ids = ObjectIds.empty;
      slots = InterfaceSlots.empty;
   } in

   let folder acc cur =
      (if not (Gctx.defined common_ctx Define.Objc) then
         match cur with
         | TClassDecl class_def when Meta.has Meta.Objc class_def.cl_meta ->
            abort "In order to compile '@:objc' classes, please define '-D objc'" class_def.cl_pos
         | _ -> ());

      match cur with
      | TClassDecl class_def when is_extern_class class_def ->
         let acc_build_xml  = acc.build_xml ^ (CppGen.get_class_code class_def Meta.BuildXml) in
         let acc_extern_src =
            match Ast.get_meta_string class_def.cl_meta Meta.SourceFile with
            | Some source -> make_path_absolute source class_def.cl_pos :: acc.extern_src
            | None -> acc.extern_src in

         { acc with build_xml = acc_build_xml; extern_src = acc_extern_src }

      | TClassDecl class_def when is_internal_class class_def.cl_path || Meta.has Meta.Macro class_def.cl_meta ->
         acc

      | TClassDecl class_def ->
         let native_gen       = Meta.has Meta.NativeGen class_def.cl_meta in
         let decl, slots, ids =
            match has_class_flag class_def CInterface with
            | true ->
               let (slots, iface) = CppRetyper.tcpp_interface_from_tclass ctx acc.slots class_def in
               if native_gen then (NativeInterface iface, slots, acc.ids) else (ManagedInterface iface, acc.slots, acc.ids)
            | false ->
               let (slots, ids, cls) = CppRetyper.tcpp_class_from_tclass ctx acc.ids acc.slots class_def [] in
               if native_gen then (NativeClass cls, slots, ids) else (ManagedClass cls, slots, ids) in

         let acc_decls           = decl :: acc.decls in
         let acc_build_xml       = acc.build_xml ^ (CppGen.get_class_code class_def Meta.BuildXml) in
         let acc_init_classes    = if has_init_field class_def then class_def.cl_path :: acc.init_classes else acc.init_classes in
         let acc_boot_classes    = if has_boot_field class_def then class_def.cl_path :: acc.boot_classes else acc.boot_classes in
         let acc_nonboot_classes = if Meta.has Meta.NativeGen class_def.cl_meta then acc.nonboot_classes else class_def.cl_path :: acc.nonboot_classes in
         let acc_exe_classes     =
            if (has_class_flag class_def CInterface) && (is_native_gen_class class_def) then
               acc.exe_classes
            else
               let deps = CppReferences.find_referenced_types ctx (TClassDecl class_def) ctx.ctx_super_deps ctx.ctx_constructor_deps false true scriptable in

               (class_def.cl_path, deps, cur) :: acc.exe_classes in

         { acc with
            build_xml = acc_build_xml;
            decls = acc_decls;
            init_classes = acc_init_classes;
            boot_classes = acc_boot_classes;
            nonboot_classes = acc_nonboot_classes;
            exe_classes = acc_exe_classes;
            ids = ids;
            slots = slots
         }

      | TEnumDecl enum_def when is_extern_enum enum_def || is_internal_class enum_def.e_path ->
         acc

      | TEnumDecl enum_def ->
         let deps             = CppReferences.find_referenced_types ctx (TEnumDecl enum_def) ctx.ctx_super_deps PathMap.empty false true false in
         let ids, enum        = CppRetyper.tcpp_enum_from_tenum ctx acc.ids enum_def in
         let acc_decls        = (Enum enum) :: acc.decls in
         let acc_boot_enums   = enum_def.e_path :: acc.boot_enums in
         let acc_exe_classes  = (enum_def.e_path, deps, cur) :: acc.exe_classes in
         
         { acc with decls = acc_decls; boot_enums = acc_boot_enums; exe_classes = acc_exe_classes; ids = ids }
      | _ ->
         acc
   in
   let srcctx = List.fold_left folder initial common_ctx.types in

   List.iter (fun tcpp_type ->
      match tcpp_type with
      | ManagedClass tcpp_class ->
         CppGenClassHeader.generate_managed_header ctx tcpp_class;
         CppGenClassImplementation.generate_managed_class ctx tcpp_class;
      | NativeClass tcpp_class ->
         CppGenClassHeader.generate_native_header ctx tcpp_class;
         CppGenClassImplementation.generate_native_class ctx tcpp_class;
      | ManagedInterface interface_def ->
         CppGenInterfaceHeader.generate_managed_interface ctx interface_def;
         CppGenInterfaceImplementation.generate_managed_interface ctx interface_def;
      | NativeInterface interface_def ->
         CppGenInterfaceHeader.generate_native_interface ctx interface_def
      | Enum tcpp_enum ->
         CppGenEnum.generate ctx tcpp_enum) srcctx.decls;

   (match common_ctx.main.main_expr with
   | None -> CppGen.generate_dummy_main common_ctx
   | Some e ->
      let main_field = { (mk_field "__main__" t_dynamic e.epos null_pos) with
         cf_expr = Some e;
	  } in
      let class_def = { null_class with cl_path = ([],"@Main"); cl_ordered_statics = [main_field] } in
      main_deps := CppReferences.find_referenced_types ctx (TClassDecl class_def) ctx.ctx_super_deps ctx.ctx_constructor_deps false true false;
      CppGen.generate_main ctx ctx.ctx_super_deps class_def
   );

   CppGen.generate_boot ctx srcctx.boot_enums srcctx.boot_classes srcctx.nonboot_classes srcctx.init_classes srcctx.slots;

   CppGen.generate_files common_ctx ctx.ctx_file_info;

   write_resources common_ctx;

   (* Output class info if requested *)
   if (scriptable || (Gctx.defined common_ctx Define.DllExport) ) then begin
      let filename =
         try
            let value = Gctx.defined_value common_ctx Define.DllExport in
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
            ) srcctx.exe_classes;

         (* Output file info too *)
         List.iter ( fun file ->
               let full_path = Path.get_full_path (try Gctx.find_file common_ctx file with Not_found -> file) in
               if file <> "?" then
                  out ("file " ^ (escape file) ^ " " ^ (escape full_path) ^"\n") )
            ( List.sort String.compare ( pmap_keys !(ctx.ctx_file_info) ) );
         close_out exeClasses;
     end;
   end;

   let output_name = match  common_ctx.main.main_class with
   | Some path -> (snd path)
   | _ -> "output" in

   write_build_data common_ctx (common_ctx.file ^ "/Build.xml") srcctx.exe_classes !main_deps (srcctx.boot_enums@ srcctx.boot_classes) srcctx.build_xml srcctx.extern_src output_name;
   write_build_options common_ctx (common_ctx.file ^ "/Options.txt") common_ctx.defines.Define.values;
   if ( not (Gctx.defined common_ctx Define.NoCompilation) ) then begin
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
   let debug_level = if (Gctx.defined common_ctx Define.NoDebug) then 0 else 1 in
   let super_deps = create_super_dependencies common_ctx in
   let constructor_deps = create_constructor_dependencies common_ctx in
   if (Gctx.defined common_ctx Define.Cppia) then begin
      let ctx = new_context common_ctx debug_level (ref PMap.empty) StringMap.empty super_deps constructor_deps in
      CppCppia.generate_cppia ctx
   end else begin   
      let ctx = new_context common_ctx debug_level (ref PMap.empty) (create_member_types common_ctx) super_deps constructor_deps in
      generate_source ctx
   end
