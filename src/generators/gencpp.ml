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
open CppHash
open CppExprUtils
open CppTypeUtils
open CppAst
open CppAstTools
open CppSourceWriter
open CppContext

(*
   Generators do not care about non-core-type abstracts, so let us follow them
   away by default.
*)
let follow = Abstract.follow_with_abstracts

let replace_float_separators s = Texpr.replace_separators s ""

let make_base_directory dir =
   Path.mkdir_recursive "" ( ( Str.split_delim (Str.regexp "[\\/]+") dir ) );;

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

let make_path_absolute path pos =
   try
      if (String.sub path 0 2) = "./" then begin
         let base = if (Filename.is_relative pos.pfile) then
            Filename.concat (Sys.getcwd()) pos.pfile
         else
            pos.pfile
         in
         Path.normalize_path (Filename.concat (Filename.dirname base) (String.sub path 2 ((String.length path) -2)))
      end else
         path
   with Invalid_argument _ -> path
;;

let get_meta_string meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String(name,_)),_],_) :: _  when k=key-> name
      | _ :: l -> loop l
      in
   loop meta
;;

let get_meta_string_path meta key =
   let rec loop = function
      | [] -> ""
      | (k,[Ast.EConst (Ast.String(name,_)),_], pos) :: _  when k=key->
           make_path_absolute name pos
      | _ :: l -> loop l
      in
   loop meta
;;

let get_all_meta_string_path meta_list key =
   let extract_path pos expr =
      match expr with
      | (Ast.EConst (Ast.String(name, _)), _) -> make_path_absolute name pos
      | _ -> "" in
   let extract_meta meta =
      match meta with
      | (k, exprs, pos) when k = key -> Some (extract_path pos (List.hd exprs))
      | _ -> None in
   ExtList.List.filter_map extract_meta meta_list
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

(* Output required code to place contents in required namespace *)
let gen_open_namespace output class_path =
      List.iter (fun namespace -> output ("namespace " ^ namespace ^ "{\n")) (List.map keyword_remap (fst class_path));;

let gen_close_namespace output class_path =
      List.iter
         (fun namespace -> output ( "}" ^ " // end namespace " ^ namespace ^"\n"))
         (fst class_path);;

let is_interface obj = is_interface_type obj.etype;;

let real_non_native_interfaces =
List.filter (function (t,pl) ->
   match t, pl with
   | { cl_path = ["cpp";"rtti"],_ },[] -> false
   | _ -> not (is_native_gen_class t)
);;

let rec is_dynamic_accessor name acc field class_def =
 ( ( acc ^ "_" ^ field.cf_name) = name ) &&
   ( not (List.exists (fun f -> f.cf_name=name) class_def.cl_ordered_fields) )
   && (match class_def.cl_super with None -> true | Some (parent,_) -> is_dynamic_accessor name acc field parent )
;;

(* Convert an array to a comma separated list of values *)
let array_arg_list inList =
   let i = ref (0-1) in
   String.concat "," (List.map (fun _ -> incr i; "inArgs[" ^ (string_of_int !i) ^ "]"  ) inList)


(* See if there is a haxe break statement that will be swollowed by c++ break *)
exception BreakFound;;

(* Decide is we should look the field up by name *)
let dynamic_internal = function | "__Is" -> true | _ -> false

exception PathFound of string;;

let with_debug ctx metadata run =
   let old_debug = ctx.ctx_debug_level in
   let no_debug = Meta.has Meta.NoDebug metadata in
   if no_debug then ctx.ctx_debug_level <- 0;
   run no_debug;
   ctx.ctx_debug_level <- old_debug;
;;

let hx_stack_push ctx output clazz func_name pos gc_stack =
   if ctx.ctx_debug_level > 0 then begin
      let stripped_file = strip_file ctx.ctx_common pos.pfile in
      let esc_file = (StringHelper.s_escape stripped_file) in
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

let cpp_type_of = CppRetyper.cpp_type_of
and cpp_type_of_null = CppRetyper.cpp_type_of_null
and cpp_instance_type = CppRetyper.cpp_instance_type
;;

let cpp_class_name klass =
   (*
   let rename = get_meta_string klass.cl_meta Meta.Native in
   if rename <> "" then
      rename ^ "_obj"
   else
   *)
   let globalNamespace = if (get_meta_string klass.cl_meta Meta.Native)<>"" then "" else "::" in
   let path = globalNamespace ^ (join_class_path_remap klass.cl_path "::") in
   if (is_native_class klass) || path="::String" then path else path ^ "_obj"
;;

let type_to_string haxe_type =
      tcpp_to_string (cpp_type_of haxe_type)

let type_cant_be_null haxe_type =
   match cpp_type_of haxe_type with
   | TCppScalar _ -> true
   | _  -> false

let type_arg_to_string name default_val arg_type prefix =
   let remap_name = keyword_remap name in
   let type_str = (type_to_string arg_type) in
   match default_val with
   | Some {eexpr = TConst TNull}  -> (type_str,remap_name)
   | Some constant when (type_cant_be_null arg_type) -> ("::hx::Null< " ^ type_str ^ " > ",prefix ^ remap_name)
   | Some constant  -> (type_str,prefix ^ remap_name)
   | _ -> (type_str,remap_name)

(* Generate prototype text, including allowing default values to be null *)
let print_arg name default_val arg_type prefix =
   let (n, t) = type_arg_to_string name default_val arg_type prefix in
   n ^ " " ^ t

(* Generate prototype text, including allowing default values to be null *)
let print_arg_name name default_val arg_type prefix =
   let (n, _) = type_arg_to_string name default_val arg_type prefix in
   n

let print_arg_list ctx arg_list prefix =
   String.concat "," (List.map (fun (v,o) -> (print_arg v.v_name o v.v_type prefix) ) arg_list)

let print_arg_list_name ctx arg_list prefix =
   String.concat "," (List.map (fun (v,o) -> (print_arg_name v.v_name o v.v_type prefix) ) arg_list)

let print_arg_names args =
   String.concat "," (List.map (fun (name,_,_) -> keyword_remap name) args)

let rec print_tfun_arg_list include_names arg_list =
   let oType o arg_type =
      let type_str = (type_to_string arg_type) in
      (* type_str may have already converted Null<X> to Dynamic because of NotNull tag ... *)
      if o && (type_cant_be_null arg_type) && type_str<>"Dynamic" then
         "::hx::Null< " ^ type_str ^ " > "
      else
         type_str
   in
   match arg_list with
   | [] -> ""
   | [(name,o,arg_type)] -> (oType o arg_type) ^ (if include_names then " " ^ (keyword_remap name) else "")
   | (name,o,arg_type) :: remaining  ->
      (oType o arg_type) ^ (if include_names then " " ^ (keyword_remap name) else "") ^  "," ^ (print_tfun_arg_list include_names remaining)

let cpp_var_type_of var =
   tcpp_to_string (cpp_type_of var.v_type)
;;

let cpp_macro_var_type_of var =
   let t = tcpp_to_string (cpp_type_of var.v_type) in
   if String.contains t ',' then
      Str.global_replace (Str.regexp ",") " HX_COMMA " t
   else
     t
;;

let function_signature include_names tfun abi =
   match follow tfun with
   | TFun(args,ret) -> (type_to_string ret) ^ " " ^ abi ^ "(" ^ (print_tfun_arg_list include_names args) ^ ")"
   | _ -> "void *"

let cpp_no_debug_synbol ctx var =
   (ctx.ctx_debug_level<=1) || (match var.v_kind with VUser _ -> false | _ -> true) ||
      match cpp_type_of var.v_type with
      | TCppStar _ | TCppReference _ -> true
      | TCppInst (class_def, _) when (Meta.has Meta.StructAccess class_def.cl_meta) -> true
      | TCppInst (class_def, _) when (Meta.has Meta.Unreflective class_def.cl_meta) -> true
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


let only_stack_access haxe_type =
   let tcpp = cpp_type_of haxe_type in
   match tcpp with
   | TCppInst(klass, _) -> Meta.has Meta.StackOnly klass.cl_meta
   | _ -> false;
;;

let cpp_member_name_of member =
   let rename = get_meta_string member.cf_meta Meta.Native in
   if rename <> "" then
      rename
   else
      keyword_remap member.cf_name
;;

let cpp_is_static_extension ctx member =
   Meta.has Meta.NativeStaticExtension member.cf_meta
;;


let cpp_template_param path native =
   let path = "::" ^ (join_class_path_remap (path) "::" ) in
   if (native) then
      path
   else match path with
   | "::Array" -> "::hx::ArrayBase"
   | "::Int" -> "int"
   | "::Bool" -> "bool"
   | x -> x
;;

let cpp_enum_name_of field =
   let rename = get_meta_string field.ef_meta Meta.Native in
   if rename <> "" then
      rename
   else
      keyword_remap field.ef_name
;;

type tinject = {
   inj_prologue : bool -> unit;
   inj_setvar : string;
   inj_tail : string;
}

let mk_injection prologue set_var tail =
   Some { inj_prologue=prologue; inj_setvar=set_var; inj_tail=tail }
;;


let tvar_arg_to_string tvar default_val prefix =
   let remap_name = (cpp_var_name_of tvar) in
   let type_str = (cpp_var_type_of tvar) in
   match default_val with
   | Some {eexpr = TConst TNull}  -> (tcpp_to_string (cpp_type_of_null tvar.v_type)),remap_name
   | Some constant -> (tcpp_to_string (cpp_type_of_null tvar.v_type)),prefix ^ remap_name
   | _ -> type_str,remap_name
;;



let string_of_path path =
      "::" ^ (join_class_path_remap path "::") ^ "_obj"
;;

let default_value_string ctx value =
match value.eexpr with
   | TConst (TInt i) -> Printf.sprintf "%ld" i
   | TConst (TFloat float_as_string) -> "((Float)" ^ (replace_float_separators float_as_string) ^ ")"
   | TConst (TString s) -> strq ctx s
   | TConst (TBool b) -> (if b then "true" else "false")
   | TConst TNull -> "null()"
   | TField (_, FEnum(enum,field) ) -> (string_of_path enum.e_path) ^ "::" ^ (cpp_enum_name_of field) ^ "_dyn()"
   | _ -> "/* Hmmm " ^ (s_expr_kind value) ^ " */"
;;



let cpp_gen_default_values ctx args prefix =
   List.iter ( fun (tvar,o) ->
      let vtype = cpp_type_of tvar.v_type in
      let not_null = (type_has_meta_key Meta.NotNull tvar.v_type) || (is_cpp_scalar vtype) in
      match o with
      | Some {eexpr = TConst TNull} -> ()
      | Some const ->
         let name = cpp_var_name_of tvar in
         let spacer = if (ctx.ctx_debug_level>0) then "            \t" else "" in
         let pname = prefix ^ name in
         ctx.ctx_output ( spacer ^ "\t" ^ (tcpp_to_string vtype) ^ " " ^ name ^ " = " ^ pname );
         ctx.ctx_output ( if not_null then
                ".Default(" ^ (default_value_string ctx.ctx_common const) ^ ");\n"
            else
                ";\n" ^ spacer ^ "\tif (::hx::IsNull(" ^ pname ^ ")) " ^  name ^  " = " ^ (default_value_string ctx.ctx_common const) ^ ";\n"
            );
      | _ -> ()
   ) args;
;;

let ctx_default_values ctx args prefix =
    cpp_gen_default_values ctx args prefix
;;

let cpp_class_hash interface =
   gen_hash 0 (join_class_path interface.cl_path "::" )
;;


let rec is_constant_zero expr =
  match expr.cppexpr with
  | CppFloat x when (float_of_string x) = 0.0 -> true
  | CppInt i when i = Int32.zero -> true
  | CppCastScalar(expr,_) -> is_constant_zero(expr)
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
let cpp_arg_string tvar default_val prefix =
   let t,n = tvar_arg_to_string tvar default_val prefix in
   t ^ " " ^ n
;;

let cpp_arg_list args prefix =
    String.concat "," (List.map (fun (v,o) -> (cpp_arg_string v o prefix) ) args)
;;


let gen_type ctx haxe_type =
   ctx.ctx_output (type_to_string haxe_type)
;;





let rec implements_native_interface class_def =
   List.exists (fun (intf_def,_) ->
        is_native_gen_class intf_def ||
           implements_native_interface intf_def
    ) class_def.cl_implements ||
        (match class_def.cl_super with
           | Some (i,_) -> implements_native_interface i
           | _ -> false )
;;

let can_quick_alloc klass =
   (not (is_native_class klass)) && (not (implements_native_interface klass))
;;


let gen_cpp_ast_expression_tree ctx class_name func_name function_args function_type injection tree =
   let writer = ctx.ctx_writer in
   let out = ctx.ctx_output in
   let lastLine = ref (-1) in
   let tempId = ref 0 in
   let strq = strq ctx.ctx_common in

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

   let cppTree =  CppRetyper.expression ctx TCppVoid function_args function_type tree forInjection in
   let label_name i = Printf.sprintf "_hx_goto_%i" i in
   let class_hash = gen_hash_small 0 class_name in
   (*let genGc = Common.defined ctx.ctx_common Define.HxcppGcGenerational in*)

   let rec gen_with_injection injection expr new_line =
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
         if new_line then writer#end_block else writer#end_block_line;

      | CppInt i -> out (Printf.sprintf (if i> Int32.of_int(-1000000000) && i< Int32.of_int(1000000000) then "%ld" else "(int)%ld") i)
      | CppFloat float_as_string -> out ("((Float)" ^ float_as_string ^")")
      | CppString s -> out (strq s)
      | CppBool b -> out (if b then "true" else "false")
      | CppNull -> out "null()"
      | CppNil -> out "nil"

      | CppThis ThisReal -> out "::hx::ObjectPtr<OBJ_>(this)"
      | CppThis _ -> out "__this"

      | CppSuper thiscall ->
            out ("::hx::ObjectPtr<super>(" ^ (if thiscall=ThisReal then "this" else "__this.mPtr") ^ ")")

      | CppBreak -> out "break"
      | CppContinue -> out "continue"
      | CppGoto label -> out ("goto " ^ (label_name label));

      | CppVarDecl(var,init) ->
         let name =  cpp_var_name_of var in
         if cpp_no_debug_synbol ctx var then
            out ( (cpp_var_type_of var) ^ " " ^ name )
         else begin
            let dbgName =  cpp_var_debug_name_of var in
            let macro = if init=None then "HX_VAR" else "HX_VARI" in
            let varType = cpp_macro_var_type_of var in
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

      | CppNullAccess -> out ("::hx::Throw(" ^ strq "Null access" ^ ")")
      | CppFunction(func,_) ->
         (match func with
         | FuncThis(field,_) ->
              out ("this->" ^ (cpp_member_name_of field) ^ "_dyn()");
         | FuncInstance(expr,inst,field) ->
              gen expr; out ((if expr.cpptype=TCppString || inst=InstStruct then "." else "->") ^ (cpp_member_name_of field) ^ "_dyn()");
         | FuncInterface(expr,_,field) ->
              gen expr;
              out ("->__Field(" ^ strq field.cf_name ^ ", ::hx::paccDynamic)")
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
              gen expr; out ("->__Field(" ^ (strq name) ^ ",::hx::paccDynamic)")
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
           | _ -> die "" __LOC__ (* per nsplit specs, this should never happen *)
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

      | CppCall(FuncNew( TCppInst (klass, p)), args) when can_quick_alloc klass ->
         out ((cpp_class_path_of klass p) ^ "_obj::__alloc( HX_CTX ");
         List.iter (fun arg -> out ","; gen arg ) args;
         out (")")

      | CppCall(func, args) ->
         let doCall = ref true in
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

         | FuncSuperConstruct(TCppInst (klass, _)) when is_native_class klass ->
            doCall := false;

         | FuncSuperConstruct _ ->
            out ((if not ctx.ctx_real_this_ptr then "__this->" else "") ^  "super::__construct")

         | FuncSuper(_,TCppInst(klass, p),field) when is_native_class klass ->
            out ((cpp_class_path_of klass p) ^ "::" ^ (cpp_member_name_of field));

         | FuncSuper(this,_,field) ->
            out ( (if this==ThisReal then "this->" else "__->") ^ "super::" ^ (cpp_member_name_of field) )

         | FuncNew(newType) ->
            let objName = match newType with
            | TCppString -> "::String"
            | TCppDynamicArray -> "::cpp::VirtualArray_obj::__new"
            | TCppObjectArray _ -> "::Array_obj< ::Dynamic>::__new"
            | TCppScalarArray(value) -> "::Array_obj< " ^ (tcpp_to_string value) ^ " >::__new"
            | TCppObjC klass ->  (cpp_class_path_of klass []) ^ "_obj::__new"
            | TCppNativePointer klass -> "new " ^ (cpp_class_path_of klass []);
            | TCppInst (klass, p) when is_native_class klass -> cpp_class_path_of klass p
            | TCppInst (klass, p) -> (cpp_class_path_of klass p) ^ "_obj::__new"
            | TCppClass -> "::hx::Class_obj::__new";
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
         if !doCall then begin
            let sep = ref "" in
            out "(";
            List.iter (fun arg ->
               out !sep; sep := ",";
               gen arg;
               ) !argsRef;
            out (")" ^ !closeCall);
         end
      | CppNewNative(e) ->
         out "new "; gen e;
      | CppAddressOf(e) ->
         out ("&("); gen e; out ")";
      | CppDereference(e) ->
         out ("(*("); gen e; out "))";
      | CppFunctionAddress(klass, member) ->
         let signature = function_signature false member.cf_type "" in
         let name = cpp_member_name_of member in
         (*let void_cast = has_meta_key field.cf_meta Meta.Void in*)
         out ("::cpp::Function< " ^ signature ^">(::hx::AnyCast(");
         out ("&::" ^(join_class_path_remap klass.cl_path "::")^ "_obj::" ^ name );
         out " ))"

      | CppExtern(name,isGlobal) ->
         if isGlobal then out " ::";
         out name;

      | CppDynamicField(obj,name) ->
         gen obj;
         out ("->__Field(" ^ (strq name)  ^ ",::hx::paccDynamic)");

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
         | CppVarRef( VarClosure(var)) when is_gc_element ctx (cpp_type_of var.v_type) ->
              out ("this->_hx_set_" ^ (cpp_var_name_of var) ^ "(HX_CTX, "); gen rvalue; out ")"

         | CppVarRef( VarThis(member,_)) when is_gc_element ctx (cpp_type_of member.cf_type) ->
              out ("this->_hx_set_" ^ (cpp_member_name_of member) ^ "(HX_CTX, "); gen rvalue; out ")"

         | CppVarRef( VarInstance(obj,member,_,"->")) when is_gc_element ctx (cpp_type_of member.cf_type) ->
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
            gen expr; out ("->__SetField(" ^ (strq name) ^ ","); gen rvalue; out ",::hx::paccDynamic)"
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
         out ("::hx::SourceInfo(" ^ strq name ^ "," ^ string_of_int(Int32.to_int line) ^ "," ^ strq clazz ^ "," ^ strq func ^ ")")

      | CppClassOf (path,native) ->
         let path = "::" ^ (join_class_path_remap (path) "::" ) in
         let path = match path with
         | "::Int" -> "int"
         | "::Bool" -> "bool"
         | x -> x in
         if (native) then
            out "null()"
         else if (path="::Array") then
            out "::hx::ArrayBase::__mClass"
         else
            out ("::hx::ClassOf< " ^ path ^ " >()")

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
            out ("::hx::AnonStruct" ^ lengthStr ^"_obj< " ^
               (String.concat "," (List.map (fun (_,value) ->  tcpp_to_string value.cpptype) values) ) ^
               " >::Create(" );
            let sep = ref "" in
            List.iter (fun (name,value) -> out (!sep ^ (strq name) ^ "," ); sep:=","; gen value ) values;
            out ")";
         end else begin
            out ("::hx::Anon_obj::Create(" ^ lengthStr ^")");
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
            | _ -> die "" __LOC__
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
         out "::hx::UShr("; gen left; out ","; gen right; out ")";

      | CppBinop( Ast.OpMod, left, right) ->
         if is_constant_zero right then begin
            out "::hx::Mod("; gen left; out ",(double)( "; gen right; out " ))";
         end else begin
            out "::hx::Mod("; gen left; out ","; gen right; out ")";
         end

      | CppBinop( Ast.OpDiv, left, right) when is_constant_zero right ->
         out "::hx::DivByZero("; gen left; out ")";

      | CppBinop(op, left, right) ->
         let op = string_of_op op expr.cpppos in
          out "(";
          gen left;
          out (" " ^ op ^ " ");
          gen right;
          out ")";
      | CppCompare(opName, left, right, _) ->
          out ("::hx::" ^ opName ^ "( ");
          gen left;
          out (",");
          gen right;
          out (" )");
      | CppNullCompare(op, left) ->
          out ("::hx::" ^ op ^ "( "); gen left; out (" )");

      | CppThrow(value) ->
         out "HX_STACK_DO_THROW("; gen value; out ")";

      | CppReturn None -> out "return";
      | CppReturn Some value -> out "return "; gen value;

      | CppEnumField(enum,field) ->
         out ((string_of_path enum.e_path) ^ "::" ^ (cpp_enum_name_of field) ^ "_dyn()" );

      | CppEnumParameter(obj,field,index) ->
         let valueType = cpp_type_of (get_nth_type field index) in
         let baseType = enum_getter_type valueType in
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
              gen_with_injection None block false;
              out " while("; gen condition; out ");\n"
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
         let varType = cpp_var_type_of tvar in
         out ("for(::cpp::FastIterator_obj< " ^  varType ^
               " > *__it = ::cpp::CreateFastIterator< "^ varType ^ " >(");
         gen init;
         out (");  __it->hasNext(); )");
         let prologue = fun _ ->
            output_i ( varType ^ " " ^ (cpp_var_name_of tvar) ^ " = __it->next();\n" );
         in
         gen_with_injection (mk_injection prologue "" "") loop true;


      | CppTry(block,catches) ->
          let prologue = function _ ->
             ExtList.List.iteri (fun idx (v,_) ->
                output_i ("HX_STACK_CATCHABLE(" ^ cpp_macro_var_type_of v  ^ ", " ^ string_of_int idx ^ ");\n")
             ) catches
          in
          out ("try ");
          gen_with_injection (mk_injection prologue "" "" ) block (List.length catches < 0);
          if (List.length catches > 0 ) then begin
             out " catch( ::Dynamic _hx_e) ";
             writer#begin_block;

             let seen_dynamic = ref false in
             let else_str = ref "" in
             List.iter (fun (v,catch) ->
                let type_name = cpp_var_type_of v in
                (match cpp_type_of v.v_type with
                | TCppInterface(klass) ->
                   let hash = (cpp_class_hash klass) in
                   output_i (!else_str ^ "if (::hx::TIsInterface< (int)" ^ hash  ^ " >(_hx_e.mPtr))")
                | TCppString ->
                   output_i (!else_str ^ "if (_hx_e.IsClass< ::String >() && _hx_e->toString()!=null() )");
                | _ ->
                   if (type_name="Dynamic") then begin
                      seen_dynamic := true;
                      output_i !else_str;
                   end else
                      output_i (!else_str ^ "if (_hx_e.IsClass< " ^ type_name ^ " >() )");
                );

                let prologue = function _ ->
                   output_i "HX_STACK_BEGIN_CATCH\n";
                   output_i (type_name ^ " " ^ (cpp_var_name_of v) ^ " = _hx_e;\n");
                in
                gen_with_injection (mk_injection prologue "" "") catch true;
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
         (match cppType with
         | TCppInterface(i) ->
             out " ::hx::interface_check(";
             gen expr;
             out ("," ^ (cpp_class_hash i) ^")")
         | _ -> begin
            let toType = tcpp_to_string cppType in
            if toType="Dynamic" then
               (out " ::Dynamic("; gen expr; out ")")
            else
               (out ("::hx::TCast< " ^ toType ^ " >::cast("); gen expr; out ")")
            end
         )

      | CppCastStatic(expr,toType) ->
         let close = match expr.cpptype with
         | TCppDynamic -> ""
         | _ -> out "Dynamic( "; ")"
         in
         gen expr; out (close ^ ".StaticCast< " ^ tcpp_to_string toType ^" >()")

      | CppCast(expr,toType) ->
         (match expr.cppexpr, expr.cpptype, toType with
         | CppCall( FuncInternal _, _), _, _ ->
            gen expr; out (".StaticCast< " ^ tcpp_to_string toType ^" >()")
         | _, TCppObjC(_), _
         | _, TCppObjCBlock(_), _  ->
            out ("( ("^ tcpp_to_string toType ^")((id) ( "); gen expr; out (") ))")
         | _,_,TCppObjectPtr -> out ("::hx::DynamicPtr("); gen expr; out (")")
         | _,TCppPointer(_,_), TCppStar(_,_)
         | _,TCppPointer(_,_), TCppRawPointer(_,_)
               -> out ("( ("^ tcpp_to_string toType ^")( ("); gen expr; out (").get_raw()) )")
         | _ -> out ("( ("^ tcpp_to_string toType ^")("); gen expr; out (") )")
         )

      | CppCastScalar(expr,scalar) ->
         out ("( ("^scalar^")("); gen expr; out (") )");

      | CppCastVariant(expr) ->
         out " ::Dynamic("; gen expr; out ")";

      | CppCastObjC(expr,klass) ->
         let path = join_class_path_remap klass.cl_path "::"  in
         let toType = if (has_class_flag klass CInterface) then "id < " ^ path ^ ">" else path ^ " *" in
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
      gen_with_injection None expr true

   and gen_lvalue lvalue =
      match lvalue with
      | CppVarRef varLoc ->
           gen_val_loc varLoc true
      | CppArrayRef arrayLoc -> (match arrayLoc with
         | ArrayObject(arrayObj, index, _) ->
            out "::hx::IndexRef("; gen arrayObj; out ".mPtr,"; gen index; out ")";
         | ArrayTyped(arrayObj, index, _) ->
            gen arrayObj; out "["; gen index; out "]";
         | ArrayPointer(arrayObj, index) ->
            gen arrayObj; out ".ptr["; gen index; out "]";
         | ArrayRawPointer(arrayObj, index) ->
            gen arrayObj; out "["; gen index; out "]";
         | ArrayVirtual(arrayObj, index)
         | ArrayDynamic(arrayObj, index) ->
            out "::hx::IndexRef("; gen arrayObj; out ".mPtr,"; gen index; out ")";
         | ArrayImplements(_,arrayObj,index) ->
            out "::hx::__ArrayImplRef("; gen arrayObj; out ","; gen index; out ")";
         )
      | CppExternRef(name,isGlobal) -> if isGlobal then out " ::"; out name
      | CppDynamicRef(expr,name) ->
         let objPtr = match expr.cpptype with
         |  TCppVariant -> "getObject()"
         | _ -> ".mPtr"
         in
         out "::hx::FieldRef(("; gen expr ; out (")" ^ objPtr ^ "," ^ strq name ^ ")")

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
      | OpAdd -> "::hx::AddEq"
      | OpMult -> "::hx::MultEq"
      | OpDiv -> "::hx::DivEq"
      | OpSub -> "::hx::SubEq"
      | OpAnd -> "::hx::AndEq"
      | OpOr  -> "::hx::OrEq"
      | OpXor  -> "::hx::XorEq"
      | OpShl  -> "::hx::ShlEq"
      | OpShr  -> "::hx::ShrEq"
      | OpUShr  -> "::hx::UShrEq"
      | OpMod  -> "::hx::ModEq"
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
      | OpUShr -> ">>>"
      | OpMod -> "%"
      | OpInterval -> "..."
      | OpArrow -> "->"
      | OpIn -> " in "
      | OpNullCoal -> "??"
      | OpAssign | OpAssignOp _ -> abort "Unprocessed OpAssign" pos

   and gen_closure closure =
      let argc = Hashtbl.length closure.close_undeclared in
      let size = string_of_int argc in
      if argc >= 62 then (* Limited by c++ macro size of 128 args *)
         abort "Too many capture variables" closure.close_expr.cpppos;
      if argc >= 20 || (List.length closure.close_args) >= 20 then
         writer#add_big_closures;
      let argsCount = list_num closure.close_args in
      output_i ("HX_BEGIN_LOCAL_FUNC_S" ^ size ^ "(");
      out (if closure.close_this != None then "::hx::LocalThisFunc," else "::hx::LocalFunc,");
      out ("_hx_Closure_" ^ (string_of_int closure.close_id) );
      Hashtbl.iter (fun name var ->
         out ("," ^ (cpp_macro_var_type_of var) ^ "," ^ (keyword_remap name));
      ) closure.close_undeclared;
      out (") HXARGC(" ^ argsCount ^")\n");

      let func_type = tcpp_to_string closure.close_type in
      output_i (func_type ^ " _hx_run(" ^ (cpp_arg_list closure.close_args "__o_") ^ ")");

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
      gen_with_injection (mk_injection prologue "" "") closure.close_expr true;

      let return = match closure.close_type with TCppVoid -> "(void)" | _ -> "return" in

      output_i ("HX_END_LOCAL_FUNC" ^ argsCount ^ "(" ^ return ^ ")\n\n");
   in


   (*out "\t";*)

   gen_with_injection injection cppTree true;

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


let is_override field =
   has_class_field_flag field CfOverride
;;

(*
   Functions are added in reverse order (oldest on right), then list is reversed because this is easier in ocaml
   The order is important because cppia looks up functions by index
*)


let current_virtual_functions_rev clazz base_functions =
   List.fold_left (fun result elem -> match follow elem.cf_type, elem.cf_kind  with
      | _, Method MethDynamic -> result
      | TFun (args,return_type), Method _  ->
          if (is_override elem ) then
            if List.exists (fun (e,a,r) -> e.cf_name=elem.cf_name ) result then
               result
            else
               (elem,args,return_type) :: result
          else
             (elem,args,return_type) :: result
      | _,_ -> result
    ) base_functions clazz.cl_ordered_fields
;;

let all_virtual_functions clazz =
  let rec all_virtual_functions_rec clazz =
   current_virtual_functions_rev clazz (match clazz.cl_super with
       | Some def -> all_virtual_functions_rec (fst def)
       | _ -> []
     )
   in
   List.rev (all_virtual_functions_rec clazz)
;;



(*
let current_virtual_functions clazz parents override_types =
  List.fold_left (fun result elem -> match follow elem.cf_type, elem.cf_kind  with
    | _, Method MethDynamic -> result
    | TFun (args,return_type), Method _ ->
        if override_types then
           (elem,args,return_type) :: (List.filter (fun (e,a,r) -> e.cf_name<>elem.cf_name) result)
        else if (is_override clazz elem.cf_name ) then
           result
        else
           (elem,args,return_type) :: result
    | _,_ -> result ) parents (List.rev clazz.cl_ordered_fields)
;;

let all_virtual_functions clazz override_types =
   let rec all_virtual_functions clazz =
      current_virtual_functions clazz (match clazz.cl_super with
         | Some def -> all_virtual_functions (fst def)
         | _ -> [] ) false
   in
   all_virtual_functions clazz
;;
*)


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
      let return_type_str = (type_to_string function_def.tf_type) in
      let nargs = string_of_int (List.length function_def.tf_args) in
      let return_type = (cpp_type_of function_def.tf_type ) in
      let is_void = return_type = TCppVoid in
      let ret = if is_void  then "(void)" else "return " in

      let needsWrapper t = match t with
         | TCppStar _ -> true
         | TCppInst(t, _) -> Meta.has Meta.StructAccess t.cl_meta
         | _ -> false
      in
      let orig_debug = ctx.ctx_debug_level in
      let no_debug = Meta.has Meta.NoDebug field.cf_meta in

      if (not (is_dynamic_haxe_method field)) then begin
         (* The actual function definition *)
         let nativeImpl = get_meta_string field.cf_meta Meta.Native in
         let remap_name = native_field_name_remap is_static field in
         output (if is_void then "void" else return_type_str );
         output (" " ^ class_name ^ "::" ^ remap_name ^ "(" );
         output (print_arg_list ctx function_def.tf_args "__o_");
         output ")";
         ctx.ctx_real_this_ptr <- true;
         let code = (get_code field.cf_meta Meta.FunctionCode) in
         let tail_code = (get_code field.cf_meta Meta.FunctionTailCode) in

         if nativeImpl<>"" && is_static then begin
            output " {\n";
            output ("\t" ^ ret ^ "::" ^ nativeImpl ^ "(" ^ (print_arg_list_name ctx function_def.tf_args "__o_") ^ ");\n");
            output "}\n\n";
         end else
            gen_cpp_function_body ctx class_def is_static field.cf_name function_def code tail_code no_debug;

         output "\n\n";
         let nonVirtual = Meta.has Meta.NonVirtual field.cf_meta in
         let doDynamic =  (nonVirtual || not (is_override field ) ) && (reflective class_def field ) in
         (* generate dynamic version too ... *)
         if ( doDynamic ) then begin
            let tcpp_args = List.map (fun (v,_) -> cpp_type_of v.v_type) function_def.tf_args in
            let wrap = (needsWrapper return_type) || (List.exists needsWrapper tcpp_args) in
            if wrap then begin
               let wrapName = "_hx_wrap" ^ class_name ^ "_" ^ remap_name in
               output ("static ::Dynamic " ^ wrapName ^ "( "  );
               let sep = ref " " in
               if not is_static then begin
                  output "::hx::Object *obj";
                  sep := ",";
               end;
               ExtList.List.iteri (fun i _ -> output (!sep ^ "const Dynamic &a" ^ (string_of_int i)) ; sep:=",")  tcpp_args;
               output ( ") {\n\t");
               if not is_void then begin
                  match return_type with
                    | TCppStar _ ->
                       output "return (cpp::Pointer<const void *>) "
                    | TCppInst(t, _) when Meta.has Meta.StructAccess t.cl_meta ->
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
                       | TCppInst(t, _) when Meta.has Meta.StructAccess t.cl_meta ->
                          output ("(cpp::Struct< " ^ (tcpp_to_string arg) ^ " >) ");
                       | _ -> () );
                     output ("a" ^ (string_of_int i));
                  )  tcpp_args;

               output ");\n";

               if is_void then output "\treturn null();\n";
               output "}\n";
               let nName = string_of_int (List.length tcpp_args) in
               output ("::Dynamic " ^ class_name ^ "::" ^ remap_name ^ "_dyn() {\n\treturn ");
               if is_static then
                  output ("::hx::CreateStaticFunction" ^ nName ^ "(\"" ^ remap_name ^ "\"," ^ wrapName ^ ");")
               else
                  output ("::hx::CreateMemberFunction" ^ nName ^ "(\"" ^ remap_name ^ "\",this," ^ wrapName ^ ");");
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
         output (" _hx_run(" ^ (print_arg_list ctx function_def.tf_args "__o_") ^ ")");
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
      end else if has_class_field_flag field CfAbstract then begin
         let tl,tr = match follow field.cf_type with
            | TFun(tl,tr) -> tl,tr
            | _ -> die "" __LOC__
         in
         let nargs = string_of_int (List.length tl) in
         let return_type = (cpp_type_of tr) in
         let is_void = return_type = TCppVoid in
         let ret = if is_void  then "(void)" else "return " in
         output ("HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ "," ^ remap_name ^ "," ^ ret ^ ")\n\n");
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
   let nativeGen = Meta.has Meta.NativeGen class_def.cl_meta in

   if (is_interface) then begin
      match follow field.cf_type, field.cf_kind with
      | _, Method MethDynamic  -> ()
      | TFun (args,return_type), Method _  ->
         let gen_args = print_tfun_arg_list true in
         if is_static || nativeGen then begin
            output ( (if (not is_static) then "		virtual " else "		" ) ^ (type_to_string return_type) );
            output (" " ^ remap_name ^ "( " );
            output (gen_args args);
            output (if (not is_static) then ")=0;\n" else ");\n");
            if (reflective class_def field) then begin
               if (Common.defined ctx.ctx_common Define.DynamicInterfaceClosures) then
                  output ("		inline ::Dynamic " ^ remap_name ^ "_dyn() { return __Field( " ^ (strq ctx.ctx_common field.cf_name) ^ ", ::hx::paccDynamic); }\n" )
               else
                  output ("		virtual ::Dynamic " ^ remap_name ^ "_dyn()=0;\n" );
            end
         end else begin
            let argList = gen_args args in
            let returnType = type_to_string return_type in
            let returnStr = if returnType = "void" then "" else "return " in
            let commaArgList = if argList="" then argList else "," ^ argList in
            let cast = "::hx::interface_cast< ::" ^ join_class_path_remap class_def.cl_path "::" ^ "_obj *>" in
            output ("		" ^ returnType ^ " (::hx::Object :: *_hx_" ^ remap_name ^ ")(" ^ argList ^ "); \n");
            output ("		static inline " ^ returnType ^ " " ^ remap_name ^ "( ::Dynamic _hx_" ^ commaArgList ^ ") {\n");
            output ("			#ifdef HXCPP_CHECK_POINTER\n");
            output ("			if (::hx::IsNull(_hx_)) ::hx::NullReference(\"Object\", false);\n");
            output ("			#ifdef HXCPP_GC_CHECK_POINTER\n");
            output ("				GCCheckPointer(_hx_.mPtr);\n");
            output ("			#endif\n");
            output ("			#endif\n");
            output ("			" ^ returnStr ^ "(_hx_.mPtr->*( " ^ cast ^ "(_hx_.mPtr->_hx_getInterface(" ^ (cpp_class_hash class_def) ^ ")))->_hx_" ^ remap_name ^ ")(" ^ print_arg_names args ^ ");\n		}\n" );
         end
      | _  ->  ( )
   end else begin
      let decl = get_meta_string field.cf_meta Meta.Decl in
      let has_decl = decl <> "" in
      let nonVirtual = Meta.has Meta.NonVirtual field.cf_meta in
      let doDynamic =  (nonVirtual || not (is_override field ) ) && (reflective class_def field ) in
      if (has_decl) then
         output ( "      typedef " ^ decl ^ ";\n" );
      output (if is_static then "\t\tstatic " else "\t\t");
      (match  field.cf_expr with
      | Some { eexpr = TFunction function_def } ->
         if ( is_dynamic_haxe_method field ) then begin
            if ( doDynamic ) then begin
               output ("::Dynamic " ^ remap_name ^ ";\n");
               if (not is_static) && (is_gc_element ctx TCppDynamic) then
                  output ("\t\tinline ::Dynamic _hx_set_" ^ remap_name ^ "(::hx::StackContext *_hx_ctx,::Dynamic _hx_v) { HX_OBJ_WB(this,_hx_v.mPtr) return " ^ remap_name ^ "=_hx_v; }\n");
               output (if is_static then "\t\tstatic " else "\t\t");
               output ("inline ::Dynamic &" ^ remap_name ^ "_dyn() " ^ "{return " ^ remap_name^ "; }\n")
            end
         end else begin
            let return_type = (type_to_string function_def.tf_type) in
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
            output (print_arg_list ctx function_def.tf_args "" );
            output ");\n";
            if ( doDynamic ) then begin
               output (if is_static then "\t\tstatic " else "\t\t");
               output ("::Dynamic " ^ remap_name ^ "_dyn();\n" )
            end;
         end;
         output "\n";
      | _ when has_class_field_flag field CfAbstract ->
         let ctx_arg_list ctx arg_list prefix =
            let get_default_value name =
               try
                  match Meta.get Meta.Value field.cf_meta with
                  | (_,[ (EObjectDecl decls, _) ],_) ->
                     Some ((List.find (fun ((n,_,_), _) -> n = name) decls) |> snd |> (type_constant_value ctx.ctx_common.basic));
                  | _ ->
                     None
               with Not_found ->
                  None
            in

            String.concat "," (List.map (fun (n,o,t) -> (print_arg n (get_default_value n) t prefix) ) arg_list)
         in
         let tl,tr = match follow field.cf_type with
            | TFun(tl,tr) -> tl,tr
            | _ -> die "" __LOC__
         in
         let return_type = (type_to_string tr) in
         let remap_name = native_field_name_remap is_static field in
         output "virtual ";
         output (if return_type="Void" then "void" else return_type );
         output (" " ^ remap_name ^ "(" );
         output (ctx_arg_list ctx tl "" );
         output (") " ^ (if return_type="void" then "{}" else "{ return 0; }" ) ^ "\n");
         if doDynamic then
            output ("		::Dynamic " ^ remap_name ^ "_dyn();\n" );
      | _ when has_decl ->
         output ( remap_name ^ "_decl " ^ remap_name ^ ";\n" );
         (* Variable access *)
      | _ ->
         (* Variable access *)
         let tcpp = cpp_type_of field.cf_type in
         let tcppStr = tcpp_to_string tcpp in
         if not is_static && only_stack_access field.cf_type then
            abort ("Variables of type " ^ tcppStr ^ " may not be used as members") field.cf_pos;

         output (tcppStr ^ " " ^ remap_name ^ ";\n" );
         if not is_static && (is_gc_element ctx tcpp) then begin
            let getPtr = match tcpp with | TCppString -> ".raw_ref()" | _ -> ".mPtr" in
            output ("\t\tinline " ^ tcppStr ^ " _hx_set_" ^ remap_name ^ "(::hx::StackContext *_hx_ctx," ^ tcppStr ^ " _hx_v) { HX_OBJ_WB(this,_hx_v" ^ getPtr ^ ") return " ^ remap_name ^ "=_hx_v; }\n");
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
   if for_depends then begin
      let include_files = get_all_meta_string_path (t_infos obj).mt_meta Meta.Depend in
      let include_adder = fun inc -> types := (PMap.add ( path_of_string inc ) true !types) in
      List.iter include_adder include_files;
   end;
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
      let include_files = get_all_meta_string_path tinfo.mt_meta (if for_depends then Meta.Depend else Meta.Include) in
      if List.length include_files > 0 then
         List.iter (fun inc -> add_type(path_of_string inc)) include_files
      else if (not for_depends) && (Meta.has Meta.Include tinfo.mt_meta) then
         add_type tinfo.mt_path
   in

   let add_extern_class klass =
      add_extern_type (TClassDecl klass)
   in
   let add_extern_enum enum =
      add_extern_type (TEnumDecl enum)
   in
   let add_native_gen_class klass =
      let include_files = get_all_meta_string_path klass.cl_meta (if for_depends then Meta.Depend else Meta.Include) in
      if List.length include_files > 0 then
         List.iter (fun inc -> add_type ( path_of_string inc )) include_files
      else if for_depends then
         add_type klass.cl_path
      else begin
         let path = klass.cl_path in
         if not (has_class_flag klass CInterface) then
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
         | TMono r -> (match r.tm_type with None -> () | Some t -> visit_type t)
         | TEnum (enum,_) ->
            (match is_extern_enum enum with
            | true -> add_extern_enum enum
            | false -> add_type enum.e_path)
         (* If a class has a template parameter, then we treat it as dynamic - except
            for the Array, Class, FastIterator or Pointer classes, for which we do a fully typed object *)
         | TInst (klass,params) ->
            (match klass.cl_path with
            | ([],"Array") | ([],"Class") | (["cpp"],"FastIterator")
            | (["cpp"],"Pointer") | (["cpp"],"ConstPointer") | (["cpp"],"Function")
            | (["cpp"],"RawPointer") | (["cpp"],"RawConstPointer") -> List.iter visit_type params
            | _ when is_native_gen_class klass -> add_native_gen_class klass
            | _ when is_extern_class klass ->
               add_extern_class klass;
               List.iter visit_type params;
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
   let visit_params expression =
      begin
      let rec visit_expression = fun expression ->
         (* Expand out TTypeExpr (ie, the name of a class, as used for static access etc ... *)
         (match expression.eexpr with
            | TTypeExpr type_def -> ( match type_def with
               | TClassDecl class_def when is_native_gen_class class_def -> add_native_gen_class class_def
               | TClassDecl class_def when is_extern_class class_def -> add_extern_class class_def
               | TEnumDecl enum_def when is_extern_enum enum_def -> add_extern_enum enum_def
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
               | _ -> print_endline ("TSuper : Odd etype ?" ^ ( (type_to_string expression.etype)) )
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
         let meta = Texpr.build_metadata ctx.ctx_common.basic (TEnumDecl enum_def) in
         match meta with Some expr -> visit_params expr | _ -> ();
      end;
   in
   let inc_cmp i1 i2 =
      String.compare (join_class_path i1 ".") (join_class_path i2 ".")
   in

   (* Body of main function *)
   (match obj with
   | TClassDecl class_def -> visit_class class_def;
      (match TClass.get_cl_init class_def with Some expression -> visit_params expression | _ -> ())
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
   output_main "	::hx::Boot();\n";
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
      | _ -> die "" __LOC__ ) in
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
   output_boot "::hx::RegisterResources( ::hx::GetResources() );\n";
   if newScriptable then
      output_boot ("::hx::ScriptableRegisterNameSlots(scriptableInterfaceFuncs," ^ (string_of_int !(ctx.ctx_interface_slot_count) ) ^ ");\n");

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
      | TClassDecl class_def when (has_class_flag class_def CInterface) -> ( )
      | TClassDecl class_def ->
         output_files ((const_char_star (join_class_path class_def.cl_path "." )) ^ ",\n")
      | _ -> ( )
      )
   ) types;
   output_files "#endif\n";
   output_files " 0 };\n";

   output_files "} // namespace hx\n";
   output_files "void __files__boot() { __hxcpp_set_debugger_info(::hx::__hxcpp_all_classes, ::hx::__hxcpp_all_files_fullpath); }\n";

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
      Path.mkdir_recursive base_dir ("src"::[]);
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
   let debug = if (Meta.has Meta.NoDebug enum_def.e_meta) || ( Common.defined  common_ctx Define.NoDebug)
      then 0 else 1 in

   let ctx = file_context baseCtx cpp_file debug false in
   let strq = strq ctx.ctx_common in

   let classId = try Hashtbl.find baseCtx.ctx_type_ids (class_text enum_def.e_path) with Not_found -> Int32.zero in
   let classIdTxt = Printf.sprintf "0x%08lx" classId in

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
            (print_tfun_arg_list true args) ^")\n");

         output_cpp ("{\n\treturn ::hx::CreateEnum< " ^ class_name ^ " >(" ^ (strq name) ^ "," ^
            (string_of_int constructor.ef_index) ^ "," ^ (string_of_int (List.length args)) ^  ")" );
          ExtList.List.iteri (fun i (arg,_,_) -> output_cpp ("->_hx_init(" ^ (string_of_int i) ^ "," ^ (keyword_remap arg) ^ ")")) args;
         output_cpp ";\n}\n\n"
      | _ ->
         output_cpp ( remap_class_name ^ " " ^ class_name ^ "::" ^ name ^ ";\n\n" )
   ) enum_def.e_constrs;


   let constructor_arg_count constructor =
      (match constructor.ef_type with | TFun(args,_) -> List.length args | _ -> 0 )
   in

   output_cpp ("bool " ^ class_name ^ "::__GetStatic(const ::String &inName, ::Dynamic &outValue, ::hx::PropertyAccess inCallProp)\n{\n");
   PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let dyn = if constructor_arg_count constructor > 0 then "_dyn()" else "" in
      output_cpp ("\tif (inName==" ^ strq name ^ ") { outValue = " ^ class_name ^ "::" ^ keyword_remap name ^ dyn ^ "; return true; }\n" );
   ) enum_def.e_constrs;
   output_cpp ("\treturn super::__GetStatic(inName, outValue, inCallProp);\n}\n\n");

   output_cpp ("HX_DEFINE_CREATE_ENUM(" ^ class_name ^ ")\n\n");

   output_cpp ("bool " ^ class_name ^ "::_hx_isInstanceOf(int inClassId) {\n");
   output_cpp ("\treturn inClassId == (int)0x00000001 || inClassId == ::hx::EnumBase_obj::_hx_ClassId || inClassId == _hx_ClassId;\n");
   output_cpp ("}\n");

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
   output_cpp ("::hx::Val " ^ class_name ^ "::__Field(const ::String &inName,::hx::PropertyAccess inCallProp)\n{\n");
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

   output_cpp ("::hx::Class " ^ class_name ^ "::__mClass;\n\n");

   output_cpp ("Dynamic __Create_" ^ class_name ^ "() { return new " ^ class_name ^ "; }\n\n");

   output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
   let text_name = strq (join_class_path class_path ".") in
   output_cpp ("\n::hx::Static(__mClass) = ::hx::_hx_RegisterClass(" ^ text_name ^
               ", ::hx::TCanCast< " ^ class_name ^ " >," ^ class_name ^ "_sStaticFields,0,\n");
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
         output_cpp ( (keyword_remap name) ^ " = ::hx::CreateConstEnum< " ^ class_name ^ " >(" ^ (strq name) ^  "," ^
            (string_of_int constructor.ef_index) ^ ");\n" )
   ) enum_def.e_constrs;
   output_cpp ("}\n\n");




   output_cpp "\n";
   gen_close_namespace output_cpp class_path;
   cpp_file#close;

   let h_file = new_header_file common_ctx common_ctx.file class_path in
   let super = "::hx::EnumBase_obj" in
   let output_h = (h_file#write) in
   let def_string = join_class_path class_path "_"  in

   begin_header_file (h_file#write_h) def_string false;

   List.iter2 (fun r f -> gen_forward_decl h_file r f) referenced flags;

   output_h ( get_code enum_def.e_meta Meta.HeaderCode );

   gen_open_namespace output_h class_path;

   output_h "\n\n";
   output_h ("class " ^ class_name ^ " : public " ^ super ^ "\n");
   output_h ("{\n\ttypedef " ^ super ^ " super;\n");
   output_h ("\t\ttypedef " ^ class_name ^ " OBJ_;\n");
   output_h "\n\tpublic:\n";
   output_h ("\t\tenum { _hx_ClassId = " ^ classIdTxt ^ " };\n\n");
   output_h ("\t\t" ^ class_name ^ "() {};\n");
   output_h ("\t\tHX_DO_ENUM_RTTI;\n");
   output_h ("\t\tstatic void __boot();\n");
   output_h ("\t\tstatic void __register();\n");
   output_h ("\t\tstatic bool __GetStatic(const ::String &inName, Dynamic &outValue, ::hx::PropertyAccess inCallProp);\n");
   output_h ("\t\t::String GetEnumName( ) const { return " ^ (strq (join_class_path class_path "."))  ^ "; }\n" );
   output_h ("\t\t::String __ToString() const { return " ^ (strq (just_class_name ^ ".") )^ " + _hx_tag; }\n");
   output_h ("\t\tbool _hx_isInstanceOf(int inClassId);\n\n");


   PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      output_h ( "\t\tstatic " ^  remap_class_name ^ " " ^ name );
      match constructor.ef_type with
      | TFun (args,_) ->
         output_h ( "(" ^ (print_tfun_arg_list true args) ^");\n");
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

let has_new_gc_references class_def =
   (
      let is_gc_reference field =
      (should_implement_field field) && (is_data_member field) && not (type_cant_be_null field.cf_type)
      in
      List.exists is_gc_reference class_def.cl_ordered_fields
   )
;;


let rec has_gc_references ctx class_def =
   ( match class_def.cl_super with
   | Some def when has_gc_references ctx (fst def) -> true
   | _ -> false )
   || has_new_gc_references class_def
;;

let rec find_next_super_iteration ctx class_def =
   match class_def.cl_super with
   | Some  (klass,params) when has_new_gc_references klass ->
        tcpp_to_string_suffix "_obj" (cpp_instance_type klass params)
   | Some  (klass,_) -> find_next_super_iteration ctx klass
   | _ -> "";
;;

let has_init_field class_def =
   match TClass.get_cl_init class_def with
   | Some _ -> true
   | _ -> false;;


let is_abstract_impl class_def = match class_def.cl_kind with
   | KAbstractImpl _ -> true
   | _ -> false
;;

let variable_field field =
   (match field.cf_expr with
   | Some { eexpr = TFunction function_def } -> is_dynamic_haxe_method field
   | None when has_class_field_flag field CfAbstract -> false
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
   (
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
   (
      let is_data_field field = (match follow field.cf_type with | TFun _ -> false | _ -> true) in
      List.exists is_data_field class_def.cl_ordered_fields
   )
;;

let has_get_member_field class_def =
   (
      let reflect_fields = List.filter (reflective class_def) (class_def.cl_ordered_fields) in
      List.exists (is_readable class_def) reflect_fields
   )
;;


let has_get_static_field class_def =
      let reflect_fields = List.filter (reflective class_def) (statics_except_meta class_def) in
      List.exists (is_readable class_def) reflect_fields
;;

let has_compare_field class_def =
    List.exists (fun f -> f.cf_name="__compare") class_def.cl_ordered_fields
;;


let has_boot_field class_def =
   match TClass.get_cl_init class_def with
   | None -> List.exists has_field_init (List.filter should_implement_field class_def.cl_ordered_statics)
   | _ -> true
;;

let cpp_tfun_signature ctx include_names args return_type =
  let argList = print_tfun_arg_list include_names args in
  let returnType = type_to_string return_type in
  ("( " ^ returnType ^ " (::hx::Object::*)(" ^ argList ^ "))")
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
   | "int" | "::cpp::Int32" -> "Int"
   | "Float" -> "Float"
   | "::String" -> "String"
   | "Null" -> "Void"
   | "Void" -> "Void"
   | "float" | "::cpp::Float32" | "::cpp::Float64" -> "Float"
   | "::cpp::Int64" | "::cpp::UInt64" -> "Object"
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
                  List.map (fun (v,o) -> (v.v_name, type_arg_to_string v.v_name o v.v_type "__o_"))
                        function_def.tf_args;
               | _ ->
                  (match follow definition.cf_type with
                     | TFun (args,_) -> List.map (fun (a,_,t) -> (a, (type_to_string t, a)) )  args
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
         let retStr = type_to_string ret in
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
                  output (" :(" ^ (type_to_string argType) ^ ")" ^ name )
                else
                  output (" " ^ signature_name ^ ":(" ^ (type_to_string argType) ^ ")" ^ name );
                first := false;
                ) args argNames;
         with Invalid_argument _ -> begin
           abort (
              let argString  = String.concat "," (List.map (fun (name,_,_) -> name) args) in
             "Invalid arg count in delegate in " ^ field.cf_name ^ " '" ^ field.cf_name ^ "," ^
             (argString) ^ "' != '" ^ (String.concat "," argNames) ^ "'" ) field.cf_pos
         end);
         output (" {\n");
         output ("\t::hx::NativeAttach _hx_attach;\n");
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
   let nativeGen = Meta.has Meta.NativeGen class_def.cl_meta in
   let class_name = (snd class_path) ^ (if nativeGen then "" else "_obj") in
   let dot_name = join_class_path class_path "." in
   let smart_class_name =  (snd class_path)  in
   let class_name_text = join_class_path class_path "." in
   let gcName = const_char_star class_name_text in
   let ptr_name = "::hx::ObjectPtr< " ^ class_name ^ " >" in
   let debug = if (Meta.has Meta.NoDebug class_def.cl_meta) || ( Common.defined baseCtx.ctx_common Define.NoDebug)
      then 0 else 1 in
   let scriptable = inScriptable && not class_def.cl_private in

   let classId = try Hashtbl.find baseCtx.ctx_type_ids (class_text class_def.cl_path) with Not_found -> Int32.zero in
   let classIdTxt = Printf.sprintf "0x%08lx" classId in

   (* Config *)
   let override_iteration = (not nativeGen) && (has_new_gc_references class_def) in
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

   let not_toString = fun (field,args,_) -> field.cf_name<>"toString" || (has_class_flag class_def CInterface) in
   let functions = List.filter not_toString (all_virtual_functions class_def) in

   (* Constructor definition *)
   let cargs = (constructor_arg_var_list class_def baseCtx) in
   let constructor_type_var_list = List.map snd cargs in
   let constructor_var_list = List.map snd constructor_type_var_list in
   let constructor_type_args = String.concat ","
            (List.map (fun (t,a) -> t ^ " " ^ a) constructor_type_var_list) in
   let constructor_args = String.concat "," constructor_var_list in

   let isContainer = if (has_gc_references common_ctx class_def) then "true" else "false" in

   let can_quick_alloc = can_quick_alloc class_def in

   let outputConstructor ctx out isHeader =
      let classScope = if isHeader then "" else class_name ^ "::" in
      let staticHead = if isHeader then "inline static " else "" in
      out (staticHead ^ ptr_name ^ " " ^ classScope ^ "__new(" ^constructor_type_args ^") {\n");
      out ("\t" ^ ptr_name ^ " __this = new " ^ class_name ^ "();\n");
         out ("\t__this->__construct(" ^ constructor_args ^ ");\n");
      out ("\treturn __this;\n");
      out ("}\n\n");

      if can_quick_alloc then begin
         out (staticHead ^ ptr_name ^ " " ^ classScope ^ "__alloc(::hx::Ctx *_hx_ctx" ^
            (if constructor_type_args="" then "" else "," ^constructor_type_args)  ^") {\n");
         out ("\t" ^ class_name ^ " *__this = (" ^ class_name ^ "*)(::hx::Ctx::alloc(_hx_ctx, sizeof(" ^ class_name ^ "), " ^ isContainer ^", " ^ gcName ^ "));\n");
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
               with_debug ctx definition.cf_meta (fun no_debug ->
                  ctx.ctx_real_this_ptr <- false;
                  gen_cpp_function_body ctx class_def false "new" function_def "" "" no_debug;
                  out "\n";
               )
            | _ -> ()
         end else
            out ("\t__this->__construct(" ^ constructor_args ^ ");\n");

         out ("\treturn __this;\n");
         out ("}\n\n");
      end;
   in

   let outputNativeConstructor ctx out isHeader =
      match class_def.cl_constructor with
      | Some ({ cf_expr = Some { eexpr = TFunction(function_def) } } as definition) ->
         if isHeader then begin
            out ("\t\t" ^ class_name ^ "(" ^ constructor_type_args ^ ");\n\n");
         end else begin
            with_debug ctx definition.cf_meta (fun no_debug ->
               ctx.ctx_real_this_ptr <- true;
               out (class_name ^ "::" ^ class_name ^ "(" ^ constructor_type_args ^ ")");

               (match class_def.cl_super with
               | Some (klass, _) ->
                  let rec find_super_args = function
                     | TCall ({ eexpr = TConst TSuper }, args) :: _ -> Some args
                     | (TParenthesis(e) | TMeta(_,e) | TCast(e,None)) :: rest -> find_super_args (e.eexpr :: rest)
                     | TBlock e :: rest -> find_super_args ((List.map (fun e -> e.eexpr) e) @ rest)
                     | _ :: rest -> find_super_args rest
                     | _ -> None
                  in
                  (match find_super_args [function_def.tf_expr.eexpr] with
                  | Some args ->
                     out ("\n:" ^ (cpp_class_path_of klass []) ^ "(");
                     let sep = ref "" in
                     List.iter (fun arg ->
                        out !sep; sep := ",";
                        gen_cpp_ast_expression_tree ctx "" "" [] t_dynamic None arg;
                     ) args;
                     out ")\n";
                  | _ -> ());
               | _ -> ());

               let head_code = get_code definition.cf_meta Meta.FunctionCode in
               let tail_code = get_code definition.cf_meta Meta.FunctionTailCode in
               gen_cpp_function_body ctx class_def false "new" function_def head_code tail_code no_debug;
            )
         end
      | _ -> ()
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
   let strq = strq ctx.ctx_common in

   let class_super_name = (match class_def.cl_super with
      | Some (klass, params) -> (tcpp_to_string_suffix "_obj" (cpp_instance_type klass params) )
      | _ -> "") in
   if (debug>1) then print_endline ("Found class definition:" ^ (join_class_path class_def.cl_path "::"));


   cpp_file#write_h "#include <hxcpp.h>\n\n";

   let all_referenced = find_referenced_types ctx (TClassDecl class_def) super_deps constructor_deps false false scriptable in
   List.iter ( add_include cpp_file  ) all_referenced;


   if (scriptable) then
      cpp_file#write_h "#include <hx/Scriptable.h>\n";

   cpp_file#write_h "\n";

   output_cpp ( get_class_code class_def Meta.CppFileCode );
   let includes = get_all_meta_string_path class_def.cl_meta Meta.CppInclude in
   let printer  = fun inc -> output_cpp ("#include \"" ^ inc ^ "\"\n") in
   List.iter printer includes;

   gen_open_namespace output_cpp class_path;
   output_cpp "\n";

   output_cpp ( get_class_code class_def Meta.CppNamespaceCode );

   if (not (has_class_flag class_def CInterface)) && not nativeGen then begin
      output_cpp ("void " ^ class_name ^ "::__construct(" ^ constructor_type_args ^ ")");
      (match class_def.cl_constructor with
         | Some ( { cf_expr = Some ( { eexpr = TFunction(function_def) } ) } as definition ) ->
            with_debug ctx definition.cf_meta (fun no_debug ->
               gen_cpp_function_body ctx class_def false "new" function_def "" "" no_debug;
               output_cpp "\n";
            )
         | _ ->  output_cpp " { }\n\n"
      );

      (* Destructor goes in the cpp file so we can "see" the full definition of the member vars *)
      if not (has_class_flag class_def CAbstract) then begin
         output_cpp ("Dynamic " ^ class_name ^ "::__CreateEmpty() { return new " ^ class_name ^ "; }\n\n");
         output_cpp ("void *" ^ class_name ^ "::_hx_vtable = 0;\n\n");

         output_cpp ("Dynamic " ^ class_name ^ "::__Create(::hx::DynamicArray inArgs)\n");
         output_cpp ("{\n\t" ^ ptr_name ^ " _hx_result = new " ^ class_name ^ "();\n");
         output_cpp ("\t_hx_result->__construct(" ^ (array_arg_list constructor_var_list) ^ ");\n");
         output_cpp ("\treturn _hx_result;\n}\n\n");
      end;
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
                         if castKey <> implementationKey then begin
                            let glue =  Printf.sprintf "%s_%08lx" field.cf_name (gen_hash32 0 cast) in
                            if not (Hashtbl.mem alreadyGlued castKey) then begin
                               Hashtbl.replace alreadyGlued castKey ();
                               let argList = print_tfun_arg_list true args in
                               let returnType = type_to_string return_type in
                               let returnStr = if returnType="void" then "" else "return " in
                               let cppCode = returnType ^ " " ^ class_name ^ "::" ^ glue ^ "(" ^ argList ^ ") {\n" ^
                                  "\t\t\t" ^ returnStr ^ realName ^ "(" ^ print_arg_names args ^ ");\n}\n" in
                               let headerCode = "\t\t" ^ returnType ^ " " ^ glue ^ "(" ^ argList ^ ");\n" in
                               header_glue := headerCode :: !header_glue;
                               cpp_glue := cppCode :: !cpp_glue;
                            end;
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

   (match TClass.get_cl_init class_def with
   | Some expression ->
      let ctx = file_context baseCtx cpp_file debug false in
      output_cpp ("void " ^ class_name^ "::__init__()");
      gen_cpp_init ctx (cpp_class_name class_def) "__init__" "" (mk_block expression);
      output_cpp "\n\n";
   | _ -> ());


   let dump_field_name = (fun field -> output_cpp ("\t" ^  (strq field.cf_name) ^ ",\n")) in

   List.iter
      (gen_field ctx class_def class_name smart_class_name dot_name false (has_class_flag class_def CInterface))
      class_def.cl_ordered_fields;
   List.iter
      (gen_field ctx class_def class_name smart_class_name dot_name true (has_class_flag class_def CInterface)) statics_except_meta;
   output_cpp "\n";

   if (List.length dynamic_functions > 0) then begin
      output_cpp ("void " ^ class_name ^ "::__alloc_dynamic_functions(::hx::Ctx *_hx_ctx," ^ class_name ^ " *_hx_obj) {\n");
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

   if (not (has_class_flag class_def CInterface)) && not nativeGen && not inlineContructor && not (has_class_flag class_def CAbstract) then
      outputConstructor ctx output_cpp false
   else if nativeGen then
      outputNativeConstructor ctx output_cpp false;


   (* Initialise non-static variables *)
   if ( (not (has_class_flag class_def CInterface)) && (not nativeGen) ) then begin
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
         List.iter (dump_field_iterator "HX_MARK_MEMBER_NAME") implemented_instance_fields;
         (match super_needs_iteration with
         | "" -> ()
         | super -> output_cpp ("\t" ^ super^"::__Mark(HX_MARK_ARG);\n" ) );
         output_cpp "\tHX_MARK_END_CLASS();\n";
         output_cpp "}\n\n";

         (* Visit function - explicitly visit all child pointers *)
         output_cpp ("void " ^ class_name ^ "::__Visit(HX_VISIT_PARAMS)\n{\n");
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
               output_cpp ("\t\tif (HX_FIELD_EQ(inName,\"" ^  (StringHelper.s_escape field)  ^ "\") ) { " ^ result ^ " }\n");
            ) sfields;
            output_cpp "\t}\n";
         end;
      in

      let checkPropCall field = if ( (Meta.has Meta.NativeProperty class_def.cl_meta) ||
                                     (Meta.has Meta.NativeProperty field.cf_meta) ||
                                     (Common.defined common_ctx Define.ForceNativeProperty) )
         then
            "inCallProp != ::hx::paccNever"
         else
            "inCallProp == ::hx::paccAlways"
      in

      let toCommon t f value =
         t ^ "( " ^ ( match cpp_type_of f.cf_type with
           | TCppInst(t, _) as inst when (Meta.has Meta.StructAccess t.cl_meta)
              -> "cpp::Struct< " ^ (tcpp_to_string inst) ^ " >( " ^ value ^ " )"
           | TCppStar(t,_) -> "cpp::Pointer<void *>( " ^ value ^ " )"
           | _ -> value
         ) ^  " )"
      in
      let toVal f value = toCommon "::hx::Val" f value in
      let toDynamic f value = toCommon "" f value in


      if (has_get_member_field class_def) then begin
         (* Dynamic "Get" Field function - string version *)
         output_cpp ("::hx::Val " ^ class_name ^ "::__Field(const ::String &inName,::hx::PropertyAccess inCallProp)\n{\n");
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
         output_cpp ("\treturn super::__Field(inName,inCallProp);\n}\n\n");

      end;

      if (has_get_static_field class_def) then begin
         output_cpp ("bool " ^ class_name ^ "::__GetStatic(const ::String &inName, Dynamic &outValue, ::hx::PropertyAccess inCallProp)\n{\n");
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
         match cpp_type_of f.cf_type with
           | TCppInst(t, _) as inst when (Meta.has Meta.StructAccess t.cl_meta)
              -> "cpp::Struct< " ^ (tcpp_to_string inst) ^ " > "
           | TCppStar(t,_) -> "cpp::Pointer< " ^ ( tcpp_to_string t ) ^ " >"
           | _ -> type_to_string f.cf_type
      in

      (* Dynamic "Set" Field function *)
      if (has_set_member_field class_def) then begin

         output_cpp ("::hx::Val " ^ class_name ^ "::__SetField(const ::String &inName,const ::hx::Val &inValue,::hx::PropertyAccess inCallProp)\n{\n");

         let set_field_dat = List.map (fun f ->
            let default_action = if is_gc_element ctx (cpp_type_of f.cf_type) then
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
         output_cpp ("\treturn super::__SetField(inName,inValue,inCallProp);\n}\n\n");
      end;

      if (has_set_static_field class_def) then begin

         output_cpp ("bool " ^ class_name ^ "::__SetStatic(const ::String &inName,Dynamic &ioValue,::hx::PropertyAccess inCallProp)\n{\n");

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
            (fun field -> output_cpp ("\toutFields->push(" ^( strq field.cf_name )^ ");\n")) in
         let is_data_field field = (match follow field.cf_type with | TFun _ -> false | _ -> true) in

         output_cpp ("void " ^ class_name ^ "::__GetFields(Array< ::String> &outFields)\n{\n");
         List.iter append_field (List.filter is_data_field class_def.cl_ordered_fields);
         output_cpp "\tsuper::__GetFields(outFields);\n";
         output_cpp "};\n\n";
      end;

      let storage field = match (cpp_type_of field.cf_type) with
         | TCppScalar("bool") -> "::hx::fsBool"
         | TCppScalar("int") -> "::hx::fsInt"
         | TCppScalar("Float") -> "::hx::fsFloat"
         | TCppString -> "::hx::fsString"
         | o when is_object_element o -> "::hx::fsObject" ^ " /* " ^ (tcpp_to_string o ) ^ " */ "
         | u -> "::hx::fsUnknown" ^ " /* " ^ (tcpp_to_string u) ^ " */ "
         in
      let dump_member_storage = (fun field ->
         output_cpp ("\t{" ^ (storage field) ^ ",(int)offsetof(" ^ class_name ^"," ^ (keyword_remap field.cf_name) ^")," ^
            (strq field.cf_name) ^ "},\n")
         )
      in
      let dump_static_storage = (fun field ->
         output_cpp ("\t{" ^ (storage field) ^ ",(void *) &" ^ class_name ^"::" ^ (keyword_remap field.cf_name) ^"," ^
            (strq field.cf_name) ^ "},\n")
         )
      in

      output_cpp "#ifdef HXCPP_SCRIPTABLE\n";

      let stored_fields = List.filter is_data_member implemented_instance_fields in
      if ( (List.length stored_fields) > 0) then begin
         output_cpp ("static ::hx::StorageInfo " ^ class_name ^ "_sMemberStorageInfo[] = {\n");
         List.iter dump_member_storage stored_fields;
         output_cpp "\t{ ::hx::fsUnknown, 0, null()}\n};\n";
      end else
         output_cpp ("static ::hx::StorageInfo *" ^ class_name ^ "_sMemberStorageInfo = 0;\n");

      let stored_statics = List.filter is_data_member implemented_fields in
      if ( (List.length stored_statics) > 0) then begin
         output_cpp ("static ::hx::StaticInfo " ^ class_name ^ "_sStaticStorageInfo[] = {\n");
         List.iter dump_static_storage stored_statics;
         output_cpp "\t{ ::hx::fsUnknown, 0, null()}\n};\n";
      end else
         output_cpp ("static ::hx::StaticInfo *" ^ class_name ^ "_sStaticStorageInfo = 0;\n");

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
         let isTemplated = not isStatic && not (has_class_flag class_def CInterface) in
         if isTemplated then output_cpp ("\ntemplate<bool _HX_SUPER=false>");
         output_cpp ("\nstatic void CPPIA_CALL " ^ scriptName ^ "(::hx::CppiaCtx *ctx) {\n");
         let ret = match cpp_type_of return_type with TCppScalar("bool") -> "b" | _ -> script_signature return_type false in
         if (ret<>"v") then output_cpp ("ctx->return" ^ (script_type return_type false) ^ "(");


         let dump_call cast =
            if (has_class_flag class_def CInterface) then begin
               output_cpp (class_name ^ "::" ^ callName ^ "(ctx->getThis()" ^ (if (List.length args) > 0 then "," else ""));
            end else if isStatic then
               output_cpp (class_name ^ "::" ^ callName ^ "(")
            else
               output_cpp ("((" ^  class_name ^ "*)ctx->getThis())->" ^ cast ^ callName ^ "(");

            let (signature,_,_) = List.fold_left (fun (signature,sep,size) (_,opt,t) ->
               output_cpp (sep ^ "ctx->get" ^ (script_type t opt) ^ "(" ^ size ^ ")");
               (signature ^ (script_signature t opt ), ",", (size^"+sizeof(" ^ (script_size_type t opt) ^ ")") ) ) (ret,"","sizeof(void*)")  args
            in
            output_cpp ")";
            signature
         in
         let signature =
            if isTemplated then begin
               output_cpp (" _HX_SUPER ? ");
               ignore( dump_call (class_name ^ "::") );
               output_cpp (" : ");
               dump_call ""
            end else
               dump_call "";
         in

         if (ret<>"v") then output_cpp (")");
         output_cpp (";\n}\n");
         signature;
      | _ -> ""
   in


   let newInteface = (has_class_flag class_def CInterface) in

   if (scriptable && not nativeGen) then begin
      let delegate = "this->" in
      let dump_script_field idx (field,f_args,return_t) =
         let args = print_tfun_arg_list true f_args in
         let names = List.map (fun (n,_,_) -> keyword_remap n) f_args in
         let return_type = type_to_string return_t in
         let ret = if (return_type="Void" || return_type="void") then " " else "return " in
         let name = keyword_remap field.cf_name in
         let vtable =  "__scriptVTable[" ^ (string_of_int (idx+1) ) ^ "] " in
         let args_varray = (List.fold_left (fun l n -> l ^ ".Add(" ^ n ^ ")") "Array<Dynamic>()" names) in

         output_cpp ("	" ^ return_type ^ " " ^ name ^ "( " ^ args ^ " ) {\n");
         if newInteface then begin
            output_cpp ("\t\t::hx::CppiaCtx *__ctx = ::hx::CppiaCtx::getCurrent();\n" );
            output_cpp ("\t\t::hx::AutoStack __as(__ctx);\n" );
            output_cpp ("\t\t__ctx->pushObject(this);\n" );
            List.iter (fun (name,opt, t ) ->
               output_cpp ("\t\t__ctx->push" ^ (script_type t opt) ^ "(" ^ (keyword_remap name) ^ ");\n" );
            ) f_args;
            let interfaceSlot = string_of_int( -(cpp_get_interface_slot ctx name) ) in
            output_cpp ("\t\t" ^ ret ^ "__ctx->run" ^ (script_type return_t false) ^ "(__GetScriptVTable()[" ^ interfaceSlot ^ "]);\n" );
            output_cpp "\t}\n";
         end else begin
            output_cpp ("\tif (" ^ vtable ^ ") {\n" );
            output_cpp ("\t\t::hx::CppiaCtx *__ctx = ::hx::CppiaCtx::getCurrent();\n" );
            output_cpp ("\t\t::hx::AutoStack __as(__ctx);\n" );
            output_cpp ("\t\t__ctx->pushObject(" ^ (if (has_class_flag class_def CInterface) then "mDelegate.mPtr" else "this" ) ^");\n" );
            List.iter (fun (name,opt, t ) ->
               output_cpp ("\t\t__ctx->push" ^ (script_type t opt) ^ "(" ^ (keyword_remap name) ^ ");\n" );
            ) f_args;
            output_cpp ("\t\t" ^ ret ^ "__ctx->run" ^ (script_type return_t false) ^ "(" ^ vtable ^ ");\n" );
            output_cpp ("\t}  else " ^ ret );


            if ((has_class_flag class_def CInterface)) then begin
               output_cpp (" " ^ delegate ^ "__Field(HX_CSTRING(\"" ^ field.cf_name ^ "\"), ::hx::paccNever)");
               if (List.length names <= 5) then
                  output_cpp ("->__run(" ^ (String.concat "," names) ^ ");")
               else
                  output_cpp ("->__Run(" ^ args_varray ^ ");");
            end else
               output_cpp (class_name ^ "::" ^ name ^ "(" ^ (String.concat "," names)^ ");");
            if (return_type<>"void") then
               output_cpp "return null();";
            output_cpp "}\n";
            if ((has_class_flag class_def CInterface)) && not dynamic_interface_closures then begin
               output_cpp ("	Dynamic " ^ name ^ "_dyn() { return mDelegate->__Field(HX_CSTRING(\"" ^ field.cf_name ^ "\"), ::hx::paccNever); }\n\n");

            end
         end
      in

      let new_sctipt_functions = if newInteface then
            all_virtual_functions class_def
         else
            List.rev (current_virtual_functions_rev class_def [])
      in
      let sctipt_name = class_name ^ "__scriptable" in

      if newInteface then begin
         output_cpp ("class " ^ sctipt_name ^ " : public ::hx::Object {\n" );
         output_cpp "public:\n";
      end else begin
         output_cpp ("class " ^ sctipt_name ^ " : public " ^ class_name ^ " {\n" );
         output_cpp ("   typedef "^sctipt_name ^" __ME;\n");
         output_cpp ("   typedef "^class_name ^" super;\n");
         let has_funky_toString = List.exists (fun f -> f.cf_name="toString") class_def.cl_ordered_statics  ||
                                 List.exists (fun f -> f.cf_name="toString" && field_arg_count f <> 0) class_def.cl_ordered_fields in
         let super_string = if has_funky_toString then class_name ^ "::super" else class_name in
         output_cpp ("   typedef "^ super_string ^" __superString;\n");
         if ((has_class_flag class_def CInterface)) then
            output_cpp ("   HX_DEFINE_SCRIPTABLE_INTERFACE\n")
         else begin
            output_cpp ("   HX_DEFINE_SCRIPTABLE(HX_ARR_LIST" ^ (string_of_int (List.length constructor_var_list) ) ^ ")\n");
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

         output_cpp "#ifndef HXCPP_CPPIA_SUPER_ARG\n";
         output_cpp "#define HXCPP_CPPIA_SUPER_ARG(x)\n";
         output_cpp "#endif\n";
         output_cpp "static ::hx::ScriptNamedFunction __scriptableFunctions[] = {\n";
         let dump_func f isStaticFlag =
            let s = try Hashtbl.find sigs f.cf_name with Not_found -> "v" in
            output_cpp ("  ::hx::ScriptNamedFunction(\"" ^ f.cf_name ^ "\",__s_" ^ f.cf_name ^ ",\"" ^ s ^ "\", " ^ isStaticFlag ^ " " );
            let superCall = if (isStaticFlag="true") || (has_class_flag class_def CInterface) then "0" else ("__s_" ^ f.cf_name ^ "<true>") in
            output_cpp ("HXCPP_CPPIA_SUPER_ARG(" ^ superCall ^")" );
            output_cpp (" ),\n" )
         in
         List.iter (fun (f,_,_) -> dump_func f "false") new_sctipt_functions;
         List.iter (fun f -> dump_func f "true") static_functions;
         output_cpp "  ::hx::ScriptNamedFunction(0,0,0 HXCPP_CPPIA_SUPER_ARG(0) ) };\n";
      end else
         output_cpp "static ::hx::ScriptNamedFunction *__scriptableFunctions = 0;\n";

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
   if (not (has_class_flag class_def CInterface) && not nativeGen) then begin
      (* Remap the specialised "extern" classes back to the generic names *)
      output_cpp ("::hx::Class " ^ class_name ^ "::__mClass;\n\n");
      if (scriptable) then begin
         (match class_def.cl_constructor with
            | Some field  ->
                  let signature = generate_script_function false field "__script_construct_func" "__construct" in
                  output_cpp ("::hx::ScriptFunction " ^ class_name ^ "::__script_construct(__script_construct_func,\"" ^ signature ^ "\");\n");
            | _ ->
                  output_cpp ("::hx::ScriptFunction " ^ class_name ^ "::__script_construct(0,0);\n");
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
      if (not (has_class_flag class_def CAbstract)) then begin
         output_cpp ("\t" ^ class_name ^ " _hx_dummy;\n");
         output_cpp ("\t" ^ class_name ^ "::_hx_vtable = *(void **)&_hx_dummy;\n");
      end;
      output_cpp ("\t::hx::Static(__mClass) = new ::hx::Class_obj();\n");
      output_cpp ("\t__mClass->mName = " ^  (strq class_name_text)  ^ ";\n");
      output_cpp ("\t__mClass->mSuper = &super::__SGetClass();\n");
      if (not (has_class_flag class_def CAbstract)) then begin
         output_cpp ("\t__mClass->mConstructEmpty = &__CreateEmpty;\n");
         output_cpp ("\t__mClass->mConstructArgs = &__Create;\n");
      end;
      output_cpp ("\t__mClass->mGetStaticField = &" ^ (
         if (has_get_static_field class_def) then class_name ^ "::__GetStatic;\n" else "::hx::Class_obj::GetNoStaticField;\n" ));
      output_cpp ("\t__mClass->mSetStaticField = &" ^ (
         if (has_set_static_field class_def) then class_name ^ "::__SetStatic;\n" else "::hx::Class_obj::SetNoStaticField;\n" ));
      if hasMarkFunc then
         output_cpp ("\t__mClass->mMarkFunc = " ^ class_name ^ "_sMarkStatics;\n");
      output_cpp ("\t__mClass->mStatics = ::hx::Class_obj::dupFunctions(" ^ sStaticFields ^ ");\n");
      output_cpp ("\t__mClass->mMembers = ::hx::Class_obj::dupFunctions(" ^ sMemberFields ^ ");\n");
      output_cpp ("\t__mClass->mCanCast = ::hx::TCanCast< " ^ class_name ^ " >;\n");
      if hasMarkFunc then
         output_cpp ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = " ^ class_name ^ "_sVisitStatics;\n#endif\n");
      output_cpp ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mMemberStorageInfo = " ^ class_name ^ "_sMemberStorageInfo;\n#endif\n");
      output_cpp ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mStaticStorageInfo = " ^ class_name ^ "_sStaticStorageInfo;\n#endif\n");
      output_cpp ("\t::hx::_hx_RegisterClass(__mClass->mName, __mClass);\n");
      if (scriptable) then
         output_cpp ("  HX_SCRIPTABLE_REGISTER_CLASS(\""^class_name_text^"\"," ^ class_name ^ ");\n");
      Hashtbl.iter (fun _ intf_def ->
          output_cpp ("\tHX_REGISTER_VTABLE_OFFSET( " ^ class_name ^ "," ^ (join_class_path_remap intf_def.cl_path "::")^ ");\n");
          ) native_implemented;
      output_cpp ("}\n\n");
   end else if not nativeGen then begin
      output_cpp ("::hx::Class " ^ class_name ^ "::__mClass;\n\n");

      output_cpp ("void " ^ class_name ^ "::__register()\n{\n");

      output_cpp ("\t::hx::Static(__mClass) = new ::hx::Class_obj();\n");
      output_cpp ("\t__mClass->mName = " ^  (strq class_name_text)  ^ ";\n");
      output_cpp ("\t__mClass->mSuper = &super::__SGetClass();\n");
      if hasMarkFunc then
         output_cpp ("\t__mClass->mMarkFunc = " ^ class_name ^ "_sMarkStatics;\n");
      output_cpp ("\t__mClass->mMembers = ::hx::Class_obj::dupFunctions(" ^ sMemberFields ^ ");\n");
      output_cpp ("\t__mClass->mCanCast = ::hx::TIsInterface< (int)" ^ (cpp_class_hash class_def)  ^ " >;\n");
      if hasMarkFunc then
         output_cpp ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = " ^ class_name ^ "_sVisitStatics;\n#endif\n");
      output_cpp ("\t::hx::_hx_RegisterClass(__mClass->mName, __mClass);\n");
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

   if (has_class_flag class_def CInterface) && Meta.has Meta.ObjcProtocol class_def.cl_meta then begin
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
   let nativeGen = Meta.has Meta.NativeGen class_def.cl_meta in
   let class_name = (snd class_path) ^ (if nativeGen then "" else "_obj") in
   let smart_class_name =  (snd class_path)  in
   let scriptable = inScriptable && not class_def.cl_private in
   (*let cpp_file = new_cpp_file common_ctx.file class_path in*)
   let debug = if (Meta.has Meta.NoDebug class_def.cl_meta) || ( Common.defined  baseCtx.ctx_common Define.NoDebug)
      then 0 else 1 in

   let h_file = new_header_file common_ctx common_ctx.file class_path in
   let ctx = file_context baseCtx h_file debug true in
   let strq = strq ctx.ctx_common in


   let parent,super = match class_def.cl_super with
      | Some (klass,params) ->
            let name = (tcpp_to_string_suffix "_obj" (cpp_instance_type klass params) ) in
            (if (has_class_flag class_def CInterface) && nativeGen then "virtual " else "" ) ^ name, name
      | None when nativeGen && (has_class_flag class_def CInterface)  -> "virtual ::hx::NativeInterface", "::hx::NativeInterface"
      | None when (has_class_flag class_def CInterface) -> "", "::hx::Object"
      | None when nativeGen -> "", ""
      | None -> "::hx::Object", "::hx::Object"
      in
   let output_h = (h_file#write) in
   let def_string = join_class_path class_path "_"  in


   begin_header_file (h_file#write_h) def_string nativeGen;

   (* Include the real header file for the super class *)
   (match class_def.cl_super with
   | Some super ->
      let klass = fst super in
      let include_files = get_all_meta_string_path klass.cl_meta Meta.Include in
      if List.length include_files > 0 then
         List.iter (fun inc -> h_file#add_include (path_of_string inc)) include_files
      else
         h_file#add_include klass.cl_path
   | _ -> () );

   (* And any interfaces ... *)
   List.iter (fun imp->
      let interface = fst imp in
      let include_files = get_all_meta_string_path interface.cl_meta Meta.Include in
      if List.length include_files > 0 then
         List.iter (fun inc -> h_file#add_include (path_of_string inc)) include_files
      else
         h_file#add_include interface.cl_path)
      (real_interfaces class_def.cl_implements);

   (* Only need to forward-declare classes that are mentioned in the header file
      (ie, not the implementation)  *)
   let header_referenced,header_flags = find_referenced_types_flags ctx (TClassDecl class_def) "*" super_deps (Hashtbl.create 0) true false scriptable in
   List.iter2 ( fun r f -> gen_forward_decl h_file r f ) header_referenced header_flags;
   output_h "\n";

   output_h ( get_class_code class_def Meta.HeaderCode );
   let includes = get_all_meta_string_path class_def.cl_meta Meta.HeaderInclude in
   let printer  = fun inc -> output_h ("#include \"" ^ inc ^ "\"\n") in
   List.iter printer includes;

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

   if ((has_class_flag class_def CInterface) && not nativeGen) then begin
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



   if (not (has_class_flag class_def CInterface) && not nativeGen) then begin
      output_h ("\t\t" ^ class_name ^  "();\n");
      output_h "\n\tpublic:\n";
      output_h ("\t\tenum { _hx_ClassId = " ^ classIdTxt ^ " };\n\n");
      output_h ("\t\tvoid __construct(" ^ constructor_type_args ^ ");\n");
      output_h ("\t\tinline void *operator new(size_t inSize, bool inContainer=" ^ isContainer ^",const char *inName=" ^ gcName ^ ")\n" );
      output_h ("\t\t\t{ return ::hx::Object::operator new(inSize,inContainer,inName); }\n" );
      output_h ("\t\tinline void *operator new(size_t inSize, int extra)\n" );
      output_h ("\t\t\t{ return ::hx::Object::operator new(inSize+extra," ^ isContainer ^ "," ^ gcName ^ "); }\n" );
      if has_class_flag class_def CAbstract then
         output_h "\n"
      else if inlineContructor then begin
         output_h "\n";
         outputConstructor ctx (fun str -> output_h ("\t\t" ^ str) ) true
      end else begin
         output_h ("\t\tstatic " ^ptr_name^ " __new(" ^constructor_type_args ^");\n");
         if can_quick_alloc then
             output_h ("\t\tstatic " ^ptr_name^ " __alloc(::hx::Ctx *_hx_ctx" ^
                 (if constructor_type_args="" then "" else "," ^constructor_type_args)  ^");\n");
      end;
      if (not (has_class_flag class_def CAbstract)) then begin
         output_h ("\t\tstatic void * _hx_vtable;\n");
         output_h ("\t\tstatic Dynamic __CreateEmpty();\n");
         output_h ("\t\tstatic Dynamic __Create(::hx::DynamicArray inArgs);\n");
      end;
      if (List.length dynamic_functions > 0) then
         output_h ("\t\tstatic void __alloc_dynamic_functions(::hx::Ctx *_hx_alloc," ^ class_name ^ " *_hx_obj);\n");
      if (scriptable) then
         output_h ("\t\tstatic ::hx::ScriptFunction __script_construct;\n");
      output_h ("\t\t//~" ^ class_name ^ "();\n\n");
      output_h ("\t\tHX_DO_RTTI_ALL;\n");
      if (has_get_member_field class_def) then
         output_h ("\t\t::hx::Val __Field(const ::String &inString, ::hx::PropertyAccess inCallProp);\n");
      if (has_get_static_field class_def) then
         output_h ("\t\tstatic bool __GetStatic(const ::String &inString, Dynamic &outValue, ::hx::PropertyAccess inCallProp);\n");
      if (has_set_member_field class_def) then
         output_h ("\t\t::hx::Val __SetField(const ::String &inString,const ::hx::Val &inValue, ::hx::PropertyAccess inCallProp);\n");
      if (has_set_static_field class_def) then
         output_h ("\t\tstatic bool __SetStatic(const ::String &inString, Dynamic &ioValue, ::hx::PropertyAccess inCallProp);\n");
      if (has_get_fields class_def) then
         output_h ("\t\tvoid __GetFields(Array< ::String> &outFields);\n");

      if (has_compare_field class_def) then
         output_h ("\t\tint __Compare(const ::hx::Object *inRHS) const { " ^
                           "return const_cast<" ^ class_name ^ " *>(this)->__compare(Dynamic((::hx::Object *)inRHS)); }\n");

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
                let retVal = type_to_string return_type in
                let ret = if retVal="void" then "" else "return " in
                let name = keyword_remap field.cf_name in
                let argNames = List.map (fun (name,_,_) -> keyword_remap name ) args in
                output_h ( "\t\t" ^ retVal ^" " ^ name ^ "( " ^ print_tfun_arg_list true args ^ ") {\n");
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
      output_h ("\t\t::String __ToString() const { return " ^ (strq smart_class_name) ^ "; }\n\n");
   end else if not nativeGen then begin
      output_h ("\t\tHX_DO_INTERFACE_RTTI;\n\n");
   end else begin
      outputNativeConstructor ctx output_h true;
      (* native interface *) ( )
   end;

   if (has_boot_field class_def) then
      output_h ("\t\tstatic void __boot();\n");


   (match class_def.cl_array_access with
   | Some t -> output_h ("\t\ttypedef " ^ (type_string t) ^ " __array_access;\n")
   | _ -> ());


   List.iter (gen_member_def ctx class_def true (has_class_flag class_def CInterface)) (List.filter should_implement_field class_def.cl_ordered_statics);

   if (has_class_flag class_def CInterface) then begin
      List.iter (fun (field,_,_) -> gen_member_def ctx class_def false true field) functions;
   end else begin
      List.iter (gen_member_def ctx class_def false false) (List.filter should_implement_field class_def.cl_ordered_fields);
   end;

   if (has_class_flag class_def CInterface) && Meta.has Meta.ObjcProtocol class_def.cl_meta then begin
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
  if not (nativeGen && (has_class_flag class_def CInterface)) then
     generate_class_cpp ();
  generate_class_header ()
;;

let generate_class_deps ctx class_def super_deps constructor_deps scriptable =
   find_referenced_types ctx (TClassDecl class_def) super_deps constructor_deps false true scriptable
;;




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
   close_out buildfile;;

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
   writer#close;;

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
   result;;

(* Builds inheritance tree, so header files can include parents defs.  *)
let create_super_dependencies common_ctx =
   let result = Hashtbl.create 0 in
   List.iter (fun object_def ->
      (match object_def with
      | TClassDecl class_def when not (has_class_flag class_def CExtern) ->
         let deps = ref [] in
         (match class_def.cl_super with Some super ->
            if not (has_class_flag (fst super) CExtern) then
               deps := ((fst super).cl_path) :: !deps
         | _ ->() );
         List.iter (fun imp -> if not (has_class_flag (fst imp) CExtern) then deps := (fst imp).cl_path :: !deps) (real_non_native_interfaces class_def.cl_implements);
         Hashtbl.add result class_def.cl_path !deps;
      | TEnumDecl enum_def when not (has_enum_flag enum_def EnExtern) ->
         Hashtbl.add result enum_def.e_path [];
      | _ -> () );
      ) common_ctx.types;
   result;;

let create_constructor_dependencies common_ctx =
   let result = Hashtbl.create 0 in
   List.iter (fun object_def ->
      (match object_def with
      | TClassDecl class_def when not (has_class_flag class_def CExtern) ->
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

            build_xml := !build_xml ^ (get_class_code class_def Meta.BuildXml);
            if (has_init_field class_def) then
               init_classes := class_def.cl_path ::  !init_classes;
            if (has_boot_field class_def) then
               boot_classes := class_def.cl_path ::  !boot_classes
            else if not (Meta.has Meta.NativeGen class_def.cl_meta) then
               nonboot_classes := class_def.cl_path ::  !nonboot_classes;
            jobs := (fun () -> generate_class_files ctx super_deps constructor_deps class_def scriptable ) :: !jobs;
            let deps = generate_class_deps ctx class_def super_deps constructor_deps scriptable in
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

            let meta = Texpr.build_metadata common_ctx.basic object_def in
            if (has_enum_flag enum_def EnExtern) then
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


   (match common_ctx.main.main_expr with
   | None -> generate_dummy_main common_ctx
   | Some e ->
      let main_field = { (mk_field "__main__" t_dynamic e.epos null_pos) with
         cf_expr = Some e;
	  } in
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
   ;;

let generate common_ctx =
   let debug_level = if (Common.defined common_ctx Define.NoDebug) then 0 else 1 in
   if (Common.defined common_ctx Define.Cppia) then begin
      let ctx = new_context common_ctx debug_level (ref PMap.empty) (Hashtbl.create 0)  in
      CppCppia.generate_cppia ctx
   end else begin
      let ctx = new_context common_ctx debug_level (ref PMap.empty) (create_member_types common_ctx) in
      generate_source ctx
   end
;;
