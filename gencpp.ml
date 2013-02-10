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


class source_writer write_func close_func=
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
		this#write ("#ifndef INCLUDED_" ^ (join_class_path class_path "_") ^ "\n");
		this#write ("#include <" ^ (join_class_path class_path "/") ^ ".h>\n");
		this#write ("#endif\n")
end;;

let file_source_writer filename =
	let out_file = open_out filename in
	new source_writer (output_string out_file) (fun ()-> close_out out_file);;


let read_whole_file chan =
	Std.input_all chan;;

(* The cached_source_writer will not write to the file if it has not changed,
	thus allowing the makefile dependencies to work correctly *)
let cached_source_writer filename =
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
		new source_writer (add_buf) (close);
	with _ ->
		file_source_writer filename;;

let rec make_class_directories base dir_list =
	( match dir_list with
	| [] -> ()
	| dir :: remaining ->
		let path = match base with
                   | "" ->  dir
                   | "/" -> "/" ^ dir
                   | _ -> base ^ "/" ^ dir  in
         if ( not ( (path="") ||
           ( ((String.length path)=2) && ((String.sub path 1 1)=":") ) ) ) then
		         if not (Sys.file_exists path) then
			          Unix.mkdir path 0o755;
		make_class_directories (if (path="") then "/" else path) remaining
	);;


let new_source_file base_dir sub_dir extension class_path =
	make_class_directories base_dir ( sub_dir :: (fst class_path));
	cached_source_writer
		( base_dir ^ "/" ^ sub_dir ^ "/" ^ ( String.concat "/" (fst class_path) ) ^ "/" ^
		(snd class_path) ^ extension);;


let new_cpp_file base_dir = new_source_file base_dir "src" ".cpp";;

let new_header_file base_dir = new_source_file base_dir "include" ".h";;

let make_base_directory file =
	make_class_directories "" ( ( Str.split_delim (Str.regexp "[\\/]+") file ) );


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
	mutable ctx_debug : bool;
	mutable ctx_debug_type : bool;
	mutable ctx_real_this_ptr : bool;
	mutable ctx_dynamic_this_ptr : bool;
	mutable ctx_dump_src_pos : unit -> unit;
	mutable ctx_dump_stack_line : bool;
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
}

let new_context common_ctx writer debug file_info =
	{
	ctx_common = common_ctx;
	ctx_writer = writer;
	ctx_output = (writer#write);
	ctx_dbgout = if debug then (writer#write) else (fun _ -> ());
	ctx_calling = false;
	ctx_assigning = false;
	ctx_debug = debug;
	ctx_debug_type = debug;
	ctx_dump_src_pos = (fun() -> ());
	ctx_dump_stack_line = true;
	ctx_return_from_block = false;
	ctx_tcall_expand_args = false;
	ctx_return_from_internal_node = false;
	ctx_real_this_ptr = true;
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
	}


(* The internal classes are implemented by the core hxcpp system, so the cpp
	 classes should not be generated *)
let is_internal_class = function
	|  ([],"Int") | ([],"Void") |  ([],"String") | ([], "Null") | ([], "Float")
	|  ([],"Array") | ([], "Class") | ([], "Enum") | ([], "Bool")
   |  ([], "Dynamic") | ([], "ArrayAccess") | (["cpp"], "FastIterator")-> true
	|  (["cpp"], "CppInt32__") | ([],"Math") | (["haxe";"io"], "Unsigned_char__") -> true
	| _ -> false


(* The internal header files are also defined in the hx/Object.h file, so you do
	 #include them separately.  However, the Int32 and Math classes do have their
	 own header files (these are under the hxcpp tree) so these should be included *)
let include_class_header = function
	| ([],"@Main") -> false
	| (["cpp"], "CppInt32__") | ([],"Math") -> true
	| path -> not ( is_internal_class path )


let is_cpp_class = function
	| ("cpp"::_ , _)  -> true
	| ( [] , "Xml" )  -> true
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
	PMap.iter (fun key value -> key_list :=  key :: !key_list ) pmap;
	!key_list;;



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
	| "and_eq" | "xor_eq" | "typeof" | "stdin" | "stdout" | "stderr"
	| "BIG_ENDIAN" | "LITTLE_ENDIAN" | "assert" | "NULL" | "wchar_t" | "EOF"
	| "bool" | "const_cast" | "dynamic_cast" | "explicit" | "export" | "mutable" | "namespace"
 	| "reinterpret_cast" | "static_cast" | "typeid" | "typename" | "virtual"
	| "struct" -> "_" ^ name
	| "asm" -> "_asm_"
	| x -> x

let get_meta_string meta key =
	let rec loop = function
		| [] -> ""
		| (k,[Ast.EConst (Ast.String name),_],_) :: _  when k=key-> name
		| _ :: l -> loop l
		in
	loop meta
;;

let has_meta_key meta key =
   List.exists (fun m -> match m with | (k,_,_) when k=key-> true | _ -> false ) meta
;;



let get_code meta key =
	let code = get_meta_string meta key in
	if (code<>"") then code ^ "\n" else code
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
	if ( class_path = (["cpp"],"CppInt32__")) then
		writer#add_include class_path
	else begin
		let output = writer#write in
		output ("HX_DECLARE_CLASS" ^ (string_of_int (List.length (fst class_path) ) ) ^ "(");
		List.iter (fun package_part -> output (package_part ^ ",") ) (fst class_path);
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
   | TParenthesis expr -> is_function_expr expr
   | TCast (e,None) -> is_function_expr e
   | TFunction _ -> true
   | _ -> false;;

let is_virtual_field field =
   match field.cf_kind with
   | Var _ -> false
	| Method MethDynamic -> false
	| _ -> true
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
		List.iter (fun namespace -> output ("namespace " ^ namespace ^ "{\n")) (fst class_path);;

let gen_close_namespace output class_path =
		List.iter
			(fun namespace -> output ( "}" ^ " // end namespace " ^ namespace ^"\n"))
			(fst class_path);;

(* The basic types can have default values and are passesby value *)
let cant_be_null = function
	| "Int" | "Bool" | "Float" |  "::haxe::io::Unsigned_char__" -> true
	| "int" | "bool" | "double" | "float" -> true
	| _ -> false

(*  Get a string to represent a type.
	 The "suffix" will be nothing or "_obj", depending if we want the name of the
	 pointer class or the pointee (_obj class *)
let rec class_string klass suffix params =
	(match klass.cl_path with
	(* Array class *)
	|  ([],"Array") when is_dynamic_array_param (List.hd params) -> "Dynamic"
	|  ([],"Array") -> (snd klass.cl_path) ^ suffix ^ "< " ^ (String.concat ","
					 (List.map type_string  params) ) ^ " >"
	(* FastIterator class *)
	|  (["cpp"],"FastIterator") -> "::cpp::FastIterator" ^ suffix ^ "< " ^ (String.concat ","
					 (List.map type_string  params) ) ^ " >"
	| _ when (match klass.cl_kind with KTypeParameter _ -> true | _ -> false) -> "Dynamic"
	|  ([],"#Int") -> "/* # */int"
	|  (["haxe";"io"],"Unsigned_char__") -> "unsigned char"
	|  ([],"Class") -> "::Class"
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
				| _ -> "/*NULL*/" ^ (type_string t) )
			| _ -> assert false);
	(* Normal class *)
	| path when klass.cl_extern && (not (is_internal_class path) )->
            (join_class_path klass.cl_path "::") ^ suffix
	| _ -> "::" ^ (join_class_path klass.cl_path "::") ^ suffix
	)
and type_string_suff suffix haxe_type =
	(match haxe_type with
	| TMono r -> (match !r with None -> "Dynamic" ^ suffix | Some t -> type_string_suff suffix t)
	| TAbstract ({ a_path = ([],"Void") },[]) -> "Void"
	| TAbstract ({ a_path = ([],"Bool") },[]) -> "bool"
	| TAbstract ({ a_path = ([],"Float") },[]) -> "Float"
	| TAbstract ({ a_path = ([],"Int") },[]) -> "int"
	| TAbstract( { a_path = ([], "EnumValue") }, _  ) -> "Dynamic"
	| TEnum ({ e_path = ([],"Void") },[]) -> "Void"
	| TEnum ({ e_path = ([],"Bool") },[]) -> "bool"
	| TInst ({ cl_path = ([],"Float") },[]) -> "Float"
	| TInst ({ cl_path = ([],"Int") },[]) -> "int"
	| TEnum (enum,params) ->  "::" ^ (join_class_path enum.e_path "::") ^ suffix
	| TInst (klass,params) ->  (class_string klass suffix params)
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
				| _ -> type_string_suff suffix t)
			| _ -> assert false);
		| [] , "Array" ->
			(match params with
			| [t] when (type_string (follow t)) = "Dynamic" -> "Dynamic"
			| [t] -> "Array< " ^ (type_string (follow t) ) ^ " >"
			| _ -> assert false)
		| ["cpp"] , "FastIterator" ->
			(match params with
			| [t] -> "::cpp::FastIterator< " ^ (type_string (follow t) ) ^ " >"
			| _ -> assert false)
		| _ ->  type_string_suff suffix (apply_params type_def.t_types params type_def.t_type)
		)
	| TFun (args,haxe_type) -> "Dynamic" ^ suffix
	| TAnon a -> "Dynamic"
      (*
		(match !(a.a_status) with
		| Statics c -> type_string_suff suffix (TInst (c,List.map snd c.cl_types))
		| EnumStatics e -> type_string_suff suffix (TEnum (e,List.map snd e.e_types))
		| _ -> "Dynamic"  ^ suffix )
      *)
	| TDynamic haxe_type -> "Dynamic" ^ suffix
	| TLazy func -> type_string_suff suffix ((!func)())
	| TAbstract (abs,pl) when abs.a_impl <> None ->
		type_string_suff suffix (Codegen.get_underlying_type abs pl)
	| TAbstract (abs,pl) ->
		"::" ^ (join_class_path abs.a_path "::") ^ suffix
	)
and type_string haxe_type =
	type_string_suff "" haxe_type

and is_dynamic_array_param haxe_type =
   if (type_string (follow haxe_type)) = "Dynamic" then true
	else (match follow haxe_type with
	| TInst (klass,params) ->
			(match klass.cl_path with
         | ([],"Array") | ([],"Class") | (["cpp"],"FastIterator") -> false
			| _ -> (match klass.cl_kind with KTypeParameter _ -> true | _ -> false)
			)
   | _ -> false
	)
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

let is_array_implementer haxe_type =
	match follow haxe_type with
	| TInst (klass,params) ->
		(match klass.cl_array_access with
		| Some _ -> true
		| _ -> false )
	| _ -> false
	;;



(* Get the type and output it to the stream *)
let gen_type ctx haxe_type =
	ctx.ctx_output (type_string haxe_type)
;;

(* Get the type and output it to the stream *)
let gen_type_suff ctx haxe_type suff =
	ctx.ctx_output (type_string_suff suff haxe_type);;

let member_type ctx field_object member =
	let name = (if (is_array field_object.etype) then "::Array"
              else (type_string field_object.etype)) ^ "." ^ member in
	try ( Hashtbl.find ctx.ctx_class_member_types name )
	with Not_found -> "?";;

let is_interface_type t =
	match follow t with
	| TInst (klass,params) -> klass.cl_interface
	| _ -> false
;;

let is_interface obj = is_interface_type obj.etype;;

let should_implement_field x = not (is_extern_field x);;

let is_function_member expression =
	match (follow expression.etype) with | TFun (_,_) -> true | _ -> false;;

let is_internal_member member =
   match member with
	| "__Field" | "__IField" | "__Run" | "__Is" | "__GetClass" | "__GetType" | "__ToString"
	| "__s" | "__GetPtr" | "__SetField" | "__length" | "__IsArray" | "__SetThis" | "__Internal"
	| "__EnumParams" | "__Index" | "__Tag" | "__GetFields" | "toString" | "__HasField"
			-> true
   | _ -> false;;


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
	| Some constant when (cant_be_null type_str) -> ("hx::Null< " ^ type_str ^ " > ",prefix ^ remap_name)
	| Some constant  -> (type_str,prefix ^ remap_name)
	| _ -> (type_str,remap_name);;

let gen_interface_arg_type_name name opt typ =
	let type_str = (type_string typ) in
   (if (opt && (cant_be_null type_str) ) then
      "hx::Null< " ^ type_str ^ " > "
   else
      type_str )
   ^ " " ^ (keyword_remap name)
;;

let gen_tfun_interface_arg_list args =
   String.concat "," (List.map (fun (name,opt,typ) -> gen_interface_arg_type_name name opt typ) args)
;;

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


let has_utf8_chars s =
	let result = ref false in
	for i = 0 to String.length s - 1 do
		result := !result || ( Char.code (String.unsafe_get s i) > 127 )
	done;
	!result;;

let escape_null s =
	let b = Buffer.create 0 in
   String.iter (fun ch -> if (ch=='\x00') then Buffer.add_string b "\\000" else Buffer.add_char b ch ) s;
   Buffer.contents b;;

let escape_command s =
	let b = Buffer.create 0 in
   String.iter (fun ch -> if (ch=='"' || ch=='\\' ) then Buffer.add_string b "\\";  Buffer.add_char b ch ) s;
   Buffer.contents b;;


let str s =
	let escaped = Ast.s_escape s in
      let null_escaped = escape_null escaped in
        if (has_utf8_chars escaped) then begin
		(* Output both wide and thin versions - let the compiler choose ... *)
		let l = ref (String.length escaped) in
		let q = escape_stringw (Ast.s_escape s) l in
		("HX_CSTRING2(" ^ q ^ "," ^ (string_of_int !l) ^ ",\"" ^ (special_to_hex null_escaped) ^ "\" )")
        end else
		(* The wide and thin versions are the same ...  *)
		("HX_CSTRING(\"" ^ (special_to_hex null_escaped) ^ "\")")
;;



(* When we are in a "real" object, we refer to ourselves as "this", but
	if we are in a local class that is used to generate return values,
	we use the fake "__this" pointer.
	If we are in an "Anon" object, then the "this" refers to the anon object (eg List iterator) *)
let clear_real_this_ptr ctx dynamic_this =
	let old_flag = ctx.ctx_real_this_ptr in
	let old_dynamic = ctx.ctx_dynamic_this_ptr in
	ctx.ctx_real_this_ptr <- false;
	ctx.ctx_dynamic_this_ptr <- dynamic_this;
	fun () -> ( ctx.ctx_real_this_ptr <- old_flag; ctx.ctx_dynamic_this_ptr <- old_dynamic; );;


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
	"/* " ^ Type.s_expr_kind expression ^ (if (type_too) then " = " ^ (type_string expression.etype) else "") ^	" */";;

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
	| TUnop (_,_,e) ->
		f true e
	| TParenthesis e ->
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
	| TVars vl ->
		List.iter (fun (_,e) -> match e with None -> () | Some e -> f true e) vl
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
	| TMatch (e,_,cases,def) ->
		f true e;
		List.iter (fun (_,_,e) -> f false e) cases;
		(match def with None -> () | Some e -> f false e)
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


(* Get a list of variables to extract from a enum tmatch *)
let tmatch_params_to_args params =
	(match params with
	| None | Some [] -> []
	| Some l ->
		let n = ref (-1) in
		List.fold_left
			(fun acc v -> incr n; match v with None -> acc | Some v -> (v.v_name,v.v_type,!n) :: acc) [] l)

let rec is_null expr =
   match expr.eexpr with
   | TConst TNull -> true
   | TParenthesis expr -> is_null expr
   | TCast (e,None) -> is_null e
   | _ -> false
;;


let find_undeclared_variables_ctx ctx undeclared declarations this_suffix allow_this expression =
	let output = ctx.ctx_output in
	let rec find_undeclared_variables undeclared declarations this_suffix allow_this expression =
		match expression.eexpr with
		| TVars var_list ->
			List.iter (fun (tvar, optional_init) ->
				Hashtbl.add declarations (keyword_remap tvar.v_name) ();
				if (ctx.ctx_debug) then
					output ("/* found var " ^ tvar.v_name ^ "*/ ");
				match optional_init with
				| Some expression -> find_undeclared_variables undeclared declarations this_suffix allow_this expression
				| _ -> ()
				) var_list
		| TFunction func -> List.iter ( fun (tvar, opt_val) ->
				if (ctx.ctx_debug) then
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
		| TMatch (condition, enum, cases, default) ->
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
			);
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
				Hashtbl.replace undeclared "this" (type_string_suff this_suffix expression.etype)
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
	if ( expr_type="Dynamic" ) then
		true
	else begin
		let result = (
		match expr.eexpr with
		| TField( obj, field ) ->
			let name = field_name field in
			ctx.ctx_dbgout ("/* ?tfield "^name^" */");
				if (is_dynamic_member_lookup_in_cpp ctx obj name) then
            (
               ctx.ctx_dbgout "/* tf=dynobj */";
               true
            )
            else if (is_dynamic_member_return_in_cpp ctx obj name)  then
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
		| TParenthesis(expr) -> is_dynamic_in_cpp ctx expr
      | TCast (e,None) -> is_dynamic_in_cpp ctx e
		| TLocal { v_name = "__global__" } -> false
		| TConst TNull -> true
		| _ -> ctx.ctx_dbgout "/* other */";  false (* others ? *) )
		in
		ctx.ctx_dbgout (if result then "/* Y */" else "/* N */" );
		result
	end

and is_dynamic_member_lookup_in_cpp ctx field_object member =
   ctx.ctx_dbgout ("/*mem."^member^".*/");
	if (is_internal_member member) then false else
	if (match field_object.eexpr with | TTypeExpr _ -> ctx.ctx_dbgout "/*!TTypeExpr*/"; true | _ -> false) then false else
	if (is_dynamic_in_cpp ctx field_object) then true else
	if (is_array field_object.etype) then false else (
	let tstr = type_string field_object.etype in
   ctx.ctx_dbgout ("/* ts:"^tstr^"*/");
	match tstr with
		(* Internal classes have no dynamic members *)
		| "::String" | "Null" | "::Class" | "::Enum" | "::Math" | "::ArrayAccess" -> ctx.ctx_dbgout ("/* ok:" ^ (type_string field_object.etype)  ^ " */"); false
		| "Dynamic" -> true
		| name ->
				let full_name = name ^ "." ^ member in
				ctx.ctx_dbgout ("/* t:" ^ full_name ^ " */");
				try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in
					ctx.ctx_dbgout ("/* =" ^ mem_type ^ "*/");
					false )
				with Not_found -> true
   )
and is_dynamic_member_return_in_cpp ctx field_object member =
	if (is_array field_object.etype) then member="map" else
	if (is_internal_member member) then false else
   match field_object.eexpr with
   | TTypeExpr t ->
         let full_name = "::" ^ (join_class_path (t_path t) "::" ) ^ "." ^ member in
		   ctx.ctx_dbgout ("/*static:"^ full_name^"*/");
			( try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in mem_type="Dynamic" )
			with Not_found -> true )
   | _ ->
	   let tstr = type_string field_object.etype in
	   (match tstr with
		   (* Internal classes have no dynamic members *)
		   | "::String" | "Null" | "::Class" | "::Enum" | "::Math" | "::ArrayAccess" -> false
		   | "Dynamic" -> ctx.ctx_dbgout "/*D*/"; true
		   | name ->
				   let full_name = name ^ "." ^ member in
		         ctx.ctx_dbgout ("/*R:"^full_name^"*/");
				   try ( let mem_type = (Hashtbl.find ctx.ctx_class_member_types full_name) in mem_type="Dynamic" )
				   with Not_found -> true )
;;

let cast_if_required ctx expr to_type =
	let expr_type = (type_string expr.etype) in
   ctx.ctx_dbgout ( "/* cir: " ^ expr_type ^ " */" );
   if (is_dynamic_in_cpp ctx expr) then
      ctx.ctx_output (".Cast< " ^ to_type ^ " >()" )
;;


let default_value_string = function
	| TInt i -> Printf.sprintf "%ld" i
	| TFloat float_as_string -> float_as_string
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

(*
let rec has_side_effects expr =
   match expr.eexpr with
     | TConst _ | TLocal _ | TFunction _ | TTypeExpr _ -> false
     | TUnop(Increment,_,_) | TUnop(Decrement,_,_) | TBinop(OpAssign,_,_) | TBinop(OpAssignOp _,_,_) -> true
     | TUnop(_,_,e) -> has_side_effects e
     | TArray(e1,e2) | TBinop(_,e1,e2) -> has_side_effects e1 || has_side_effects e2
     | TIf(cond,e1,Some e2) -> has_side_effects cond || has_side_effects e1 || has_side_effects e2
     | TField(e,_) | TParenthesis e -> has_side_effects e
     | TArrayDecl el -> List.exists has_side_effects el
     | TObjectDecl decls -> List.exists (fun (_,e) -> has_side_effects e) decls
     | TCast(e,_) -> has_side_effects e
     | _ -> true
;;

let rec can_be_affected expr =
   match expr.eexpr with
     | TConst _ | TFunction _ | TTypeExpr _ -> false
     | TLocal _ -> true
     | TUnop(Increment,_,_) | TUnop(Decrement,_,_) -> true
     | TUnop(_,_,e) -> can_be_affected e
     | TBinop(OpAssign,_,_) | TBinop(OpAssignOp _,_,_) -> true
     | TBinop(_,e1,e2) -> can_be_affected e1 || can_be_affected e2
     | TField(e,_) -> can_be_affected e
     | TParenthesis e -> can_be_affected e
     | TCast(e,_) -> can_be_affected e
     | TArrayDecl el -> List.exists can_be_affected el
     | TObjectDecl decls -> List.exists (fun (_,e) -> can_be_affected e) decls
     | _ -> true
;;


let call_has_side_effects func args =
   let effects = (if has_side_effects func then 1 else 0) + (List.length (List.filter has_side_effects args)) in
   let affected = (if can_be_affected func then 1 else 0) + (List.length (List.filter can_be_affected args)) in
   effects + affected > 22;
;;
  The above code may be overly pessimistic - will have to check performance

*)

  

let has_side_effects expr = false;;
let call_has_side_effects func args = false;;


let has_default_values args =
	List.exists ( fun (_,o) -> match o with
            | Some TNull -> false
            | Some _ -> true
            | _ -> false ) args ;;

exception PathFound of string;;

let hx_stack_push ctx output clazz func_name pos =
   let file = pos.pfile in
	let flen = String.length file in
	(* Not quite right - should probably test is file exists *)
   let stripped_file = try
		List.iter (fun path ->
			let plen = String.length path in
			if (flen>plen && path=(String.sub file 0 plen ))
				then raise (PathFound (String.sub file plen (flen-plen)) ) )
			 (ctx.ctx_common.class_path @ ctx.ctx_common.std_path);
		file;
	with PathFound tail -> tail in
   let qfile = "\"" ^ (Ast.s_escape stripped_file) ^ "\"" in
	ctx.ctx_file_info := PMap.add qfile qfile !(ctx.ctx_file_info);
	if (ctx.ctx_dump_stack_line) then
		output ("HX_STACK_PUSH(\"" ^ clazz ^ "::" ^ func_name ^ "\"," ^ qfile ^ ","
					^ (string_of_int (Lexer.get_error_line pos) ) ^ ");\n")
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


let rec define_local_function_ctx ctx func_name func_def =
	let writer = ctx.ctx_writer in
	let output_i = writer#write_i in
	let output = ctx.ctx_output in
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
			if (ctx.ctx_debug) then
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
      hx_stack_push ctx output_i "*" func_name func_def.tf_expr.epos;
		if (has_this && ctx.ctx_dump_stack_line) then
			output_i ("HX_STACK_THIS(__this.mPtr);\n");
		List.iter (fun (v,_) -> output_i ("HX_STACK_ARG(" ^ (keyword_remap v.v_name) ^ ",\"" ^ v.v_name ^"\");\n") )
            func_def.tf_args;

		if (block) then begin
			output_i "";
			gen_expression ctx false func_def.tf_expr;
			output_i "return null();\n";
		end else begin
			(* Save old values, and equalize for new input ... *)
			let pop_names = push_anon_names ctx in

			find_local_functions_and_return_blocks_ctx ctx false func_def.tf_expr;

			(match func_def.tf_expr.eexpr with
			| TReturn (Some return_expression) when (func_type<>"Void") ->
				output_i "return ";
				gen_expression ctx true return_expression;
			| TReturn (Some return_expression) ->
				output_i "";
				gen_expression ctx false return_expression;
			| _ ->
				output_i "";
				gen_expression ctx false (to_block func_def.tf_expr);
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

and find_local_functions_and_return_blocks_ctx ctx retval expression =
	let output = ctx.ctx_output in
	let rec find_local_functions_and_return_blocks retval expression =
		match expression.eexpr with
		| TBlock _ ->
			if (retval) then begin
				define_local_return_block_ctx ctx expression (next_anon_function_name ctx) true;
			end  (* else we are done *)
		| TMatch (_, _, _, _)
		| TTry (_, _)
		| TSwitch (_, _, _) when retval ->
				define_local_return_block_ctx ctx expression (next_anon_function_name ctx) true;
      | TObjectDecl ( ("fileName" , { eexpr = (TConst (TString file)) }) ::
         ("lineNumber" , { eexpr = (TConst (TInt line)) }) ::
            ("className" , { eexpr = (TConst (TString class_name)) }) ::
               ("methodName", { eexpr = (TConst (TString meth)) }) :: [] ) -> ()
		| TObjectDecl decl_list ->
				let name = next_anon_function_name ctx in
				define_local_return_block_ctx ctx expression name true;
		| TCall(func,args) when call_has_side_effects func args ->
				define_local_return_block_ctx ctx expression (next_anon_function_name ctx) retval
		(*| TCall (e,el) -> (* visit function object first, then args *)
			find_local_functions_and_return_blocks e;
			List.iter find_local_functions_and_return_blocks  el *)
		| TFunction func ->
			let func_name = next_anon_function_name ctx in
			output "\n";
			define_local_function_ctx ctx func_name func
		| TField (obj,_) when (is_null obj) -> ( )
		| TArray (obj,_) when (is_null obj) -> ( )
		| TIf ( _ , _ , _ ) when retval -> (* ? operator style *)
		   iter_retval find_local_functions_and_return_blocks retval expression
		| TMatch (_, _, _, _)
		| TSwitch (_, _, _) when retval -> ( )
		| TMatch ( cond , _, _, _)
		| TWhile ( cond , _, _ )
		| TIf ( cond , _, _ )
		| TSwitch ( cond , _, _) -> iter_retval find_local_functions_and_return_blocks true cond
		| _ -> iter_retval find_local_functions_and_return_blocks retval expression
	in find_local_functions_and_return_blocks retval expression

and define_local_return_block_ctx ctx expression name retval =
	let writer = ctx.ctx_writer in
	let output_i = writer#write_i in
	let output = ctx.ctx_output in
	let check_this = function | "this" when not ctx.ctx_real_this_ptr -> "__this" | x -> x in
	let reference = function | "this" -> " *__this" | "_this" -> " _this" | name -> " &" ^name in
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
		output_i ("inline static " ^ ret_type ^ " Block( ");
		output (String.concat "," ( (List.map (fun var ->
				(Hashtbl.find undeclared var) ^ (reference var)) ) vars));
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
				find_local_functions_and_return_blocks_ctx ctx true value;
				output_i ( "__result->Add(" ^ (str name) ^ " , ");
				gen_expression ctx true value;
				output (if is_function_expr value then ",true" else ",false" );
				output (");\n");
			) decl_list;
			pop_names();
			output_i "return __result;\n";
			writer#end_block;
		| TBlock _ ->
			ctx.ctx_return_from_block <- return_data;
			ctx.ctx_return_from_internal_node <- false;
			gen_expression ctx false expression;
		| TCall(func,args) ->
			writer#begin_block;
			let pop_names = push_anon_names ctx in
			find_local_functions_and_return_blocks_ctx ctx true func;
			List.iter (find_local_functions_and_return_blocks_ctx ctx true) args;
			ctx.ctx_tcall_expand_args <- true;
			gen_expression ctx return_data expression;
			output ";\n";
			pop_names();
			writer#end_block;
		| _ ->
			ctx.ctx_return_from_block <- false;
			ctx.ctx_return_from_internal_node <- return_data;
			gen_expression ctx false (to_block expression);
		);
		output_i "return null();\n";
		writer#end_block;
		pop_real_this_ptr();
		writer#end_block_line;
		output ";\n";
	in
	define_local_return_block expression


and gen_expression ctx retval expression =
	let output = ctx.ctx_output in
	let writer = ctx.ctx_writer in
	let output_i = writer#write_i in
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
	if (ctx.ctx_debug) then begin
		(*if calling then output "/* Call */";*)
		(*if ctx.ctx_real_this_ptr then output "/* this */" else output "/* FAKE __this */";*)
		output (debug_expression expression ctx.ctx_debug_type);
	end;

	(* Write comma separated list of variables - useful for function args. *)
	let rec gen_expression_list expressions =
		(match expressions with
		| [] -> ()
		| [single] -> gen_expression ctx true single
		| first :: remaining ->
			gen_expression ctx true first;
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
		gen_expression ctx true expr1;
		if ( cast <> "") then output ")";

		output (" " ^ op ^ " ");

		if ( cast <> "") then output cast;
		gen_expression ctx true expr2;
		if ( cast <> "") then output ")";
		if (op <> "=") then output ")";
	in
	let rec gen_bin_op op expr1 expr2 =
		match op with
		| Ast.OpAssign -> ctx.ctx_assigning <- true;
								gen_bin_op_string expr1 "=" expr2
		| Ast.OpUShr ->
			output "hx::UShr(";
			gen_expression ctx true expr1;
			output ",";
			gen_expression ctx true expr2;
			output ")";
		| Ast.OpMod ->
			output "hx::Mod(";
			gen_expression ctx true expr1;
			output ",";
			gen_expression ctx true expr2;
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
			gen_expression ctx true expr1;
			output ",";
			gen_expression ctx true expr2;
			output ")"
		| Ast.OpNotEq -> gen_bin_op_string expr1 "!=" expr2
		| Ast.OpEq -> gen_bin_op_string expr1 "==" expr2
		| _ ->  gen_bin_op_string expr1 (Ast.s_binop op) expr2
		in

	let rec gen_tfield field_object member =
		let remap_name = keyword_remap member in
		let already_dynamic = ref false in
		(match field_object.eexpr with
		(* static access ... *)
		| TTypeExpr type_def ->
			let class_name = "::" ^ (join_class_path (t_path type_def) "::" ) in
			if (class_name="::String") then
				output ("::String::" ^ remap_name)
			else
				output (class_name ^ "_obj::" ^ remap_name);
		(* Special internal access *)
		| TLocal { v_name = "__global__" } ->
			output ("::" ^ member )
		| TConst TSuper -> output (if ctx.ctx_real_this_ptr then "this" else "__this");
						output ("->super::" ^ remap_name)
		| TConst TThis when ctx.ctx_real_this_ptr -> output ( "this->" ^ remap_name )
		| TConst TNull -> output "null()"
		| _ ->
			gen_expression ctx true field_object;
         ctx.ctx_dbgout "/* TField */";
         if (is_internal_member member) then begin
				output ( "->" ^ member );
         end else if (is_dynamic_member_lookup_in_cpp ctx field_object member) then begin
            if assigning then
				    output ( "->__FieldRef(" ^ (str member) ^ ")" )
            else
				    output ( "->__Field(" ^ (str member) ^ ",true)" );
            already_dynamic := true;
         end else begin
            if ((type_string field_object.etype)="::String" ) then
				   output ( "." ^ remap_name )
            else begin
               cast_if_required ctx field_object (type_string field_object.etype);
				   output ( "->" ^ remap_name )
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

	(match expression.eexpr with
	| TConst TNull when not retval ->
		output "Dynamic()";
	| TCall (func, arg_list) when (match func.eexpr with
			| TLocal { v_name = "__cpp__" } -> true
			| _ -> false) ->
		( match arg_list with
		| [{ eexpr = TConst (TString code) }] -> output code;
		| _ -> error "__cpp__ accepts only one string as an argument" func.epos;
		)
	| TCall (func, arg_list) when tcall_expand_args->
      let use_temp_func = has_side_effects func in
      if (use_temp_func) then begin
         output_i "Dynamic __func = ";
         gen_expression ctx true func;
         output ";\n";
      end;
      let arg_string = ref "" in
      let idx = ref 0 in
      List.iter (fun arg ->
         let a_name = "__a" ^ string_of_int(!idx) in
         arg_string := !arg_string ^ (if !arg_string<>"" then "," else "") ^ a_name;
         idx := !idx + 1;
         output_i ( (type_string arg.etype) ^ " " ^ a_name ^ " = ");
         gen_expression ctx true arg;
         output ";\n";
      ) arg_list;
      output_i (if retval then "return " else "");
      if use_temp_func then
         output "__func"
      else begin
		   ctx.ctx_calling <- true;
         gen_expression ctx true func;
      end;
      output ("(" ^ !arg_string ^ ");\n");
	| TCall (func, arg_list) ->
		let rec is_variable e = match e.eexpr with
		| TField _ -> false
		| TLocal { v_name = "__global__" } -> false
		| TParenthesis p -> is_variable p
		| TCast (e,None) -> is_variable e
		| _ -> true
      in
		let expr_type = type_string expression.etype in
		let rec is_fixed_override e = (not (is_scalar expr_type)) && match e.eexpr with
		| TField(obj,FInstance(_,field) ) ->
         let cpp_type = member_type ctx obj field.cf_name in
         (not (is_scalar cpp_type)) && (
            let fixed = (cpp_type<>"?") && (expr_type<>"Dynamic") && (cpp_type<>"Dynamic") &&
               (cpp_type<>expr_type) && (expr_type<>"Void") in
            if (fixed && ctx.ctx_debug_type ) then begin
               output ("/* " ^ (cpp_type) ^ " != " ^ expr_type ^ " -> cast */");
               (* print_endline (cpp_type ^ " != " ^ expr_type ^ " -> cast"); *)
            end;
            fixed
          )
		| TParenthesis p -> is_fixed_override p
		| _ -> false
      in
      let is_super = (match func.eexpr with | TConst TSuper -> true | _ -> false ) in
		if (ctx.ctx_debug_type) then output ("/* TCALL ret=" ^ expr_type ^ "*/");
		let is_block_call = call_has_side_effects func arg_list in
      let cast_result =  (not is_super) && (is_fixed_override func) in
      if (cast_result) then output ("hx::TCast< " ^ expr_type ^ " >::cast(");
		if (is_block_call) then
         gen_local_block_call()
      else begin
		   ctx.ctx_calling <- true;
		   gen_expression ctx true func;

		   output "(";
		   gen_expression_list arg_list;
		   output ")";
      end;
      if (cast_result) then output (")");
      if ( (is_variable func) && (expr_type<>"Dynamic") && (not is_super) && (not is_block_call)) then
         ctx.ctx_output (".Cast< " ^ expr_type ^ " >()" );
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
				find_local_functions_and_return_blocks_ctx ctx want_value expression;
				if (ctx.ctx_dump_stack_line) then
				   output_i ("HX_STACK_LINE(" ^ (string_of_int (Lexer.get_error_line expression.epos)) ^ ")\n" );
				output_i "";
				ctx.ctx_return_from_internal_node <- return_from_internal_node;
				if (want_value) then output "return ";
				gen_expression ctx want_value expression;
				decr remaining;
				writer#terminate_line
				) expr_list;
			writer#end_block;
			pop_names()
		end
	| TTypeExpr type_expr ->
		let klass = "::" ^ (join_class_path (t_path type_expr) "::" ) in
		let klass1 = if klass="::Array" then "Array<int>" else klass in
		output ("hx::ClassOf< " ^ klass1 ^ " >()")
	| TReturn _ when retval ->
		unsupported expression.epos
	| TReturn optional_expr ->
		output "";
		( match optional_expr with
		| Some return_expression when ( (type_string expression.etype)="Void") ->
			output "return null(";
         gen_expression ctx true return_expression;
			output ")";
		| Some return_expression ->
			output "return ";
			gen_expression ctx true return_expression
		| _ -> output "return null()"
		)

	| TConst const ->
		(match const with
		| TInt i -> output (Printf.sprintf "(int)%ld" i)
		| TFloat float_as_string -> output float_as_string
		| TString s -> output (str s)
		| TBool b -> output (if b then "true" else "false")
		(*| TNull -> output ("((" ^ (type_string expression.etype) ^ ")null())")*)
		| TNull -> output "null()"
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
		let dynamic =  is_dynamic_in_cpp ctx array_expr in
		if ( assigning && (not dynamic) ) then begin
         if (is_array_implementer array_expr.etype) then begin
			   output "hx::__ArrayImplRef(";
			   gen_expression ctx true array_expr;
			   output ",";
			   gen_expression ctx true index;
			   output ")";
         end else begin
			   gen_expression ctx true array_expr;
			   output "[";
			   gen_expression ctx true index;
			   output "]";
         end
		end else if (assigning) then begin
			(* output (" /*" ^ (type_string array_expr.etype) ^ " */ "); *)
			output "hx::IndexRef((";
			gen_expression ctx true array_expr;
			output ").mPtr,";
			gen_expression ctx true index;
			output ")";
		end else if ( dynamic ) then begin
			gen_expression ctx true array_expr;
			output "->__GetItem(";
			gen_expression ctx true index;
			output ")";
		end else begin
			gen_expression ctx true array_expr;
			output "->__get(";
			gen_expression ctx true index;
			output ")";
		end
	(* Get precidence matching haxe ? *)
	| TBinop (op,expr1,expr2) -> gen_bin_op op expr1 expr2
	| TField (expr,name) when (is_null expr) -> output "Dynamic()"

	| TField (field_object,field) ->
		gen_tfield field_object (field_name field)

	| TParenthesis expr when not retval ->
			gen_expression ctx retval expr;
	| TParenthesis expr -> output "("; gen_expression ctx retval expr; output ")"
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
      let tstr = (type_string_suff "_obj" expression.etype) in
      if tstr="Dynamic" then
		   output "Dynamic( Array_obj<Dynamic>::__new()"
      else
		   output ( (type_string_suff "_obj" expression.etype) ^ "::__new()");
		List.iter ( fun elem -> output ".Add(";
							gen_expression ctx true elem;
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
			   output ( ( class_string klass "_obj" params) ^ "::__new(" );
		   gen_expression_list expressions;
		   output ")"
      end
	| TUnop (Ast.NegBits,Ast.Prefix,expr) ->
		output "~(int)(";
		gen_expression ctx true expr;
		output ")"
	| TUnop (op,Ast.Prefix,expr) ->
		ctx.ctx_assigning <- (match op with Ast.Increment | Ast.Decrement -> true | _ ->false);
		output (Ast.s_unop op);
		output "(";
		gen_expression ctx true expr;
		output ")"
	| TUnop (op,Ast.Postfix,expr) ->
		ctx.ctx_assigning <- true;
		output "(";
		gen_expression ctx true expr;
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

	| TVars var_list ->
		let count = ref (List.length var_list) in
		List.iter (fun (tvar, optional_init) ->
			if (retval && !count==1) then
				(match optional_init with
				| None -> output "null()"
				| Some expression -> gen_expression ctx true expression )
			else begin
            let type_name = (type_string tvar.v_type) in
				output (if type_name="Void" then "Dynamic" else type_name );
				let name = (keyword_remap tvar.v_name) in
				output (" " ^ name );
				(match optional_init with
				| None -> ()
				| Some expression -> output " = "; gen_expression ctx true expression);
				count := !count -1;
            if (ctx.ctx_dump_stack_line) then
				   output (";\t\tHX_STACK_VAR(" ^name ^",\""^ tvar.v_name ^"\")");
				if (!count > 0) then begin output ";\n"; output_i "" end
			end
		) var_list
	| TFor (tvar, init, loop) ->
		output ("for(::cpp::FastIterator_obj< " ^  (type_string tvar.v_type) ^
             " > *__it = ::cpp::CreateFastIterator< "^(type_string tvar.v_type) ^ " >(");
		gen_expression ctx true init;
		output (");  __it->hasNext(); )");
		ctx.ctx_writer#begin_block;
		output_i ( (type_string tvar.v_type) ^ " " ^ (keyword_remap tvar.v_name) ^ " = __it->next();\n" );
		output_i "";
		gen_expression ctx false loop;
		output ";\n";
		ctx.ctx_writer#end_block;
	| TIf (condition, if_expr, optional_else_expr)  ->
		(match optional_else_expr with
		| Some else_expr ->
			if (retval) then begin
            output "(  (";
				gen_expression ctx true condition;
				output ") ? ";
				let type_str = match (type_string expression.etype) with
				| "Void" -> "Dynamic"
				| other -> other
				in
				output (type_str ^ "(");
				gen_expression ctx true if_expr;
				output ") : ";

				output (type_str ^ "(");
				gen_expression ctx true else_expr;
				output ") )";
			end else begin
				output "if (";
				gen_expression ctx true condition;
				output ")";
				gen_expression ctx false (to_block if_expr);
				output_i "else";
				gen_expression ctx false (to_block else_expr);
			end
		| _ -> output "if (";
			gen_expression ctx true condition;
			output ")";
			gen_expression ctx false (to_block if_expr);
		)
	| TWhile (condition, repeat, Ast.NormalWhile ) ->
			output  "while(";
			gen_expression ctx true condition;
			output ")";
         gen_expression ctx false (to_block repeat)
	| TWhile (condition, repeat, Ast.DoWhile ) ->
			output "do";
			gen_expression ctx false (to_block repeat);
			output "while(";
			gen_expression ctx true condition;
			output ")"

	(* These have already been defined in find_local_return_blocks ... *)
	| TTry (_,_)
	| TSwitch (_,_,_)
	| TMatch (_, _, _, _) when (retval && (not return_from_internal_node) )->
      gen_local_block_call()
	| TSwitch (condition,cases,optional_default)  ->
		let switch_on_int_constants = (only_int_cases cases) && (not (contains_break expression)) in
		if (switch_on_int_constants) then begin
			output "switch( (int)";
			gen_expression ctx true condition;
			output ")";
			ctx.ctx_writer#begin_block;
			List.iter (fun (cases_list,expression) ->
				output_i "";
				List.iter (fun value -> output "case ";
								gen_expression ctx true value;
								output ": " ) cases_list;
				ctx.ctx_return_from_block <- return_from_internal_node;
				gen_expression ctx false (to_block expression);
				output_i ";break;\n";
				) cases;
			(match optional_default with | None -> ()
			| Some default ->
				output_i "default: ";
				ctx.ctx_return_from_block <- return_from_internal_node;
				gen_expression ctx false (to_block default);
			);
			ctx.ctx_writer#end_block;
		end else begin
			let tmp_name = get_switch_var ctx in
			output ( (type_string condition.etype) ^ " " ^ tmp_name ^ " = " );
			gen_expression ctx true condition;
			output ";\n";
			let else_str = ref "" in
			if (List.length cases > 0) then
				List.iter (fun (cases,expression) ->
					output_i ( !else_str ^ "if ( ");
					else_str := "else ";
					let or_str = ref "" in
					List.iter (fun value ->
						output (!or_str ^ " ( " ^ tmp_name ^ "==");
						gen_expression ctx true value;
						output ")";
						or_str := " || ";
						) cases;
					output (")");
					ctx.ctx_return_from_block <- return_from_internal_node;
					gen_expression ctx false (to_block expression);
					) cases;
			(match optional_default with | None -> ()
			| Some default ->
				output_i ( !else_str ^ " ");
				ctx.ctx_return_from_block <- return_from_internal_node;
				gen_expression ctx false (to_block default);
				output ";\n";
			);
		end
	| TMatch (condition, enum, cases, default) ->
		let tmp_var = get_switch_var ctx in
		writer#begin_block;
		output_i (  "::" ^ (join_class_path (fst enum).e_path "::") ^ " " ^ tmp_var ^ " = " );
		gen_expression ctx true condition;
		output ";\n";

		let use_if_statements = contains_break expression in
		let dump_condition = if (use_if_statements) then begin
			let tmp_name = get_switch_var ctx in
			output_i ( "int " ^ tmp_name ^ " = (" ^ tmp_var ^ ")->GetIndex();" );
			let elif = ref "if" in
			( fun case_ids -> output (!elif ^ " (" ); elif := "else if";
					output (String.concat "||"
					(List.map (fun id -> (string_of_int id) ^ "==" ^ tmp_name ) case_ids ) );
				output ") " )
		end else begin
			output_i ("switch((" ^ tmp_var ^ ")->GetIndex())");
			( fun case_ids ->
			List.iter (fun id -> output ("case " ^ (string_of_int id) ^ ": ") ) case_ids;
			)
		end in
		writer#begin_block;
		List.iter (fun (case_ids,params,expression) ->
			output_i "";
			dump_condition case_ids;
			let has_params = match params with | Some _ -> true | _ -> false in
			if (has_params) then begin
				writer#begin_block;
				List.iter (fun (name,vtype,id) -> output_i
				((type_string vtype) ^ " " ^ (keyword_remap name) ^
					" = " ^ tmp_var ^ "->__Param(" ^ (string_of_int id) ^ ");\n"))
						(tmatch_params_to_args params);
			end;
			ctx.ctx_return_from_block <- return_from_internal_node;
			gen_expression ctx false (to_block expression);
			if (has_params) then writer#end_block;
			if (not use_if_statements) then output_i ";break;\n";
		) cases;
		(match default with
		| None -> ()
		|  Some e ->
			if (use_if_statements) then
				output_i "else "
			else
				output_i "default: ";
			ctx.ctx_return_from_block <- return_from_internal_node;
			gen_expression ctx false (to_block e);
		);
		writer#end_block;
		writer#end_block;

	| TTry (expression, catch_list) ->
		output "try";
		(* Move this "inside" the try call ... *)
		ctx.ctx_return_from_block <-return_from_internal_node;
		gen_expression ctx false (to_block expression);
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
				output_i (type_name ^ " " ^ v.v_name ^ " = __e;");
				(* Move this "inside" the catch call too ... *)
				ctx.ctx_return_from_block <-return_from_internal_node;
				gen_expression ctx false (to_block expression);
				ctx.ctx_writer#end_block;
				else_str := "else ";
				) catch_list;
			if (not !seen_dynamic) then begin
				output_i "else throw(__e);\n";
			end;
			ctx.ctx_writer#end_block;
		end;
	| TBreak -> output "break"
	| TContinue -> output "continue"
	| TThrow expression -> output "hx::Throw (";
			gen_expression ctx true expression;
			output ")"
	| TCast (cast,None) ->
		let void_cast = retval && ((type_string expression.etype)="Void" ) in
      if (void_cast) then output "Void(";
		gen_expression ctx retval cast;
      if (void_cast) then output ")";
	| TCast (e1,Some t) ->
		let class_name = (join_class_path (t_path t) "::" ) in
		if (class_name="Array") then
			output ("hx::TCastToArray(" )
		else
			output ("hx::TCast< " ^ class_name ^ " >::cast(" );
		gen_expression ctx true e1;
		output ")";
	);;



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
   List.mem field class_def.cl_overrides
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


			   (* external mem  Dynamic & *)

let gen_field ctx class_def class_name ptr_name is_static is_interface field =
	let output = ctx.ctx_output in
	ctx.ctx_real_this_ptr <- not is_static;
	let remap_name = keyword_remap field.cf_name in
	let decl = get_meta_string field.cf_meta Meta.Decl in
	let has_decl = decl <> "" in
	if (is_interface) then begin
		(* Just the dynamic glue ... *)
		match follow field.cf_type, field.cf_kind  with
		| _, Method MethDynamic  -> ()
		| TFun (args,result), Method _  ->
			if (is_static) then output "STATIC_";
			let ret = if ((type_string result ) = "Void" ) then "" else "return " in
			output ("HX_DEFINE_DYNAMIC_FUNC" ^ (string_of_int (List.length args)) ^
				 "(" ^ class_name ^ "," ^ remap_name ^ "," ^ ret ^ ")\n\n");
		| _ -> ()
	end else (match  field.cf_expr with
	(* Function field *)
	| Some { eexpr = TFunction function_def } ->
		let return_type = (type_string function_def.tf_type) in
		let nargs = string_of_int (List.length function_def.tf_args) in
		let is_void = (type_string function_def.tf_type ) = "Void" in
		let ret = if is_void  then "(void)" else "return " in
		let output_i = ctx.ctx_writer#write_i in
		let dump_src = if (Meta.has Meta.NoStack field.cf_meta) then begin
			ctx.ctx_dump_stack_line <- false;
			(fun()->())
		end else begin
			ctx.ctx_dump_stack_line <- true;
			(fun() ->
         hx_stack_push ctx output_i ptr_name field.cf_name function_def.tf_expr.epos;
         if (not is_static) then output_i ("HX_STACK_THIS(this);\n");
			List.iter (fun (v,_) -> output_i ("HX_STACK_ARG(" ^ (keyword_remap v.v_name) ^ ",\"" ^ v.v_name ^"\");\n") )
            function_def.tf_args )
		end in

		if (not (is_dynamic_haxe_method field)) then begin
			(* The actual function definition *)
			output return_type;
			output (" " ^ class_name ^ "::" ^ remap_name ^ "( " );
			output (gen_arg_list function_def.tf_args "__o_");
			output ")";
			ctx.ctx_real_this_ptr <- true;
			ctx.ctx_dynamic_this_ptr <- false;
         let code = (get_code field.cf_meta Meta.FunctionCode) in
         let tail_code = (get_code field.cf_meta Meta.FunctionTailCode) in
			if (has_default_values function_def.tf_args) then begin
				ctx.ctx_writer#begin_block;
				generate_default_values ctx function_def.tf_args "__o_";
				dump_src();
            output code;
				gen_expression ctx false function_def.tf_expr;
            output tail_code;
				if (is_void) then output "return null();\n";
				ctx.ctx_writer#end_block;
			end else begin
				let add_block = is_void || (code <> "") || (tail_code <> "") in
				if (add_block) then ctx.ctx_writer#begin_block;
			   ctx.ctx_dump_src_pos <- dump_src;
				output code;
				gen_expression ctx false (to_block function_def.tf_expr);
				output tail_code;
				if (add_block) then begin
					if (is_void) then output "return null();\n";
					ctx.ctx_writer#end_block;
				end;
			end;

			output "\n\n";
			(* generate dynamic version too ... *)
         if ( not (is_override class_def field.cf_name ) ) then begin
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
			output (" run(" ^ (gen_arg_list function_def.tf_args "") ^ ")");
			ctx.ctx_dump_src_pos <- dump_src;
			if (is_void) then begin
				ctx.ctx_writer#begin_block;
				gen_expression ctx false function_def.tf_expr;
				output "return null();\n";
				ctx.ctx_writer#end_block;
			end else
				gen_expression ctx false (to_block function_def.tf_expr);

			output ("HX_END_LOCAL_FUNC" ^ nargs ^ "(" ^ ret ^ ")\n");
			output ("HX_END_DEFAULT_FUNC\n\n");

			if (is_static) then
				output ( "Dynamic " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
		end

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
			output ( "	" ^ remap_name ^ " = new " ^ func_name ^ ";\n\n" );
		end

	(* Data field *)
	| _ -> (match field.cf_expr with
		| Some expr ->
			find_local_functions_and_return_blocks_ctx ctx true expr;
			output ( match remap_name with "__meta__" -> "	__mClass->__meta__=" | _ -> "	" ^ remap_name ^ "= ");
			gen_expression ctx true expr;
			output ";\n"
		| _ -> ( )
		);
	)
	;;



let gen_member_def ctx class_def is_static is_interface field =
	let output = ctx.ctx_output in
	let remap_name = keyword_remap field.cf_name in

	if (is_interface) then begin
		match follow field.cf_type, field.cf_kind with
		| _, Method MethDynamic  -> ()
		| TFun (args,return_type), Method _  ->
			output ( (if (not is_static) then "virtual " else "" ) ^ type_string return_type);
			output (" " ^ remap_name ^ "( " );
			output (gen_tfun_interface_arg_list args);
			output (if (not is_static) then ")=0;\n" else ");\n");
			output (if is_static then "		static " else "		");
			output ("Dynamic " ^ remap_name ^ "_dyn();\n" );
		| _  ->  ( )
	end else begin
	let decl = get_meta_string field.cf_meta Meta.Decl in
	let has_decl = decl <> "" in
   if (has_decl) then
		output ( "      typedef " ^ decl ^ ";\n" );
	output (if is_static then "		static " else "		");
   (match  field.cf_expr with
	| Some { eexpr = TFunction function_def } ->
		if ( is_dynamic_haxe_method field ) then begin
         if ( not (is_override class_def field.cf_name ) ) then begin
			   output ("Dynamic " ^ remap_name ^ ";\n");
			   output (if is_static then "		static " else "		");
			   output ("inline Dynamic &" ^ remap_name ^ "_dyn() " ^ "{return " ^ remap_name^ "; }\n")
          end
		end else begin
			let return_type = (type_string function_def.tf_type) in
			if (not is_static) then output "virtual ";
			output return_type;
			output (" " ^ remap_name ^ "( " );
			output (gen_arg_list function_def.tf_args "" );
			output ");\n";
         if ( not (is_override class_def field.cf_name ) ) then begin
				output (if is_static then "		static " else "		");
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
		| TFun (_,_) ->
			output (if is_static then "		static " else "		");
			gen_type ctx field.cf_type;
			output (" &" ^ remap_name ^ "_dyn() { return " ^ remap_name ^ ";}\n" )
		| _ ->  (match field.cf_kind with
			| Var { v_read = AccCall name } when (not is_static) && (is_dynamic_accessor name "get" field class_def) ->
				output ("\t\tDynamic get_" ^ field.cf_name ^ ";\n" )
			| _ -> ()
			);
			(match field.cf_kind with
			| Var { v_write = AccCall name } when (not is_static) &&  (is_dynamic_accessor name "set" field class_def) ->
				output ("\t\tDynamic set_" ^ field.cf_name ^ ";\n" )
			| _ -> ()
			)
		)
	);
   end
	;;

let path_of_string verbatim path =
   if verbatim then ( ["@verbatim"], path ) else
   match List.rev (Str.split_delim (Str.regexp "/") path ) with
   | [] -> ([],"")
   | [single] -> ([],single)
   | head :: rest -> (List.rev rest, head)
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
      let include_file = get_meta_string klass.cl_meta (if for_depends then Meta.Depend else Meta.Include) in
      if (include_file<>"") then
         add_type ( path_of_string for_depends include_file )
      else if (not for_depends) && (has_meta_key klass.cl_meta Meta.Include) then
         add_type klass.cl_path
   in
	let rec visit_type in_type =
		match (follow in_type) with
		| TMono r -> (match !r with None -> () | Some t -> visit_type t)
		(*| TEnum ({ e_path = ([],"Void") },[]) -> ()
		| TEnum ({ e_path = ([],"Bool") },[]) -> () *)
		| TEnum (enum,params) -> add_type enum.e_path
		(* If a class has a template parameter, then we treat it as dynamic - except
			for the Array or Class class, for which we do a fully typed object *)
		| TInst (klass,params) ->
			(match klass.cl_path with
         | ([],"Array") | ([],"Class") | (["cpp"],"FastIterator") -> List.iter visit_type params
         | (["cpp"],"CppInt32__") -> add_type klass.cl_path;
         | _ when klass.cl_extern -> add_extern_class klass
			| _ -> (match klass.cl_kind with KTypeParameter _ -> () | _ -> add_type klass.cl_path);
			)
		| TFun (args,haxe_type) -> visit_type haxe_type;
				List.iter (fun (_,_,t) -> visit_type t; ) args;
		| TAbstract (abs,pl) when abs.a_impl <> None ->
			visit_type (Codegen.get_underlying_type abs pl)
		| _ -> ()
	in
	let rec visit_types expression =
		begin
		let rec visit_expression = fun expression ->
			(* Expand out TTypeExpr (ie, the name of a class, as used for static access etc ... *)
			(match expression.eexpr with
				| TTypeExpr type_def -> ( match type_def with
               | TClassDecl class_def when class_def.cl_extern -> add_extern_class class_def
	            | _ -> add_type (t_path type_def)
               )

				(* Must visit the types, Type.iter will visit the expressions ... *)
				| TTry (e,catches) ->
					List.iter (fun (v,_) -> visit_type v.v_type) catches
				(* Must visit the enum param types, Type.iter will visit the rest ... *)
				| TMatch (_,enum,cases,_) ->
					add_type (fst enum).e_path;
					List.iter (fun (case_ids,params,expression) ->
						(match params with
						| None -> ()
						| Some l -> List.iter (function None -> () | Some v -> visit_type v.v_type) l  ) ) cases;
				(* Must visit type too, Type.iter will visit the expressions ... *)
            | TNew  (klass,params,_) -> begin
               visit_type (TInst (klass,params));
               try
               let construct_type = Hashtbl.find constructor_deps klass.cl_path in
					   visit_type construct_type.cf_type
               with Not_found -> ();
               end
				(* Must visit type too, Type.iter will visit the expressions ... *)
				| TVars var_list ->
					List.iter (fun (v, _) -> visit_type v.v_type) var_list
				(* Must visit args too, Type.iter will visit the expressions ... *)
				| TFunction func_def ->
					List.iter (fun (v,_) -> visit_type v.v_type) func_def.tf_args;
				| TConst TSuper ->
                (match expression.etype with
	             | TInst (klass,params) ->
                   (try let construct_type = Hashtbl.find constructor_deps klass.cl_path in
					      visit_type construct_type.cf_type
                   with Not_found -> () )
                | _ -> print_endline ("TSuper : Odd etype?")
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
			| Some expression -> visit_types expression | _ -> ());
	in
	let visit_class class_def =
		let fields = List.append class_def.cl_ordered_fields class_def.cl_ordered_statics in
		let fields_and_constructor = List.append fields
			(match class_def.cl_constructor with | Some expr -> [expr] | _ -> [] ) in
		List.iter visit_field fields_and_constructor;
		if (include_super_args) then
         List.iter visit_field (List.map (fun (a,_,_) -> a ) (all_virtual_functions class_def ));

		(* Add super & interfaces *)
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
			match meta with Some expr -> visit_types expr | _ -> ();
		end;
	in
	let inc_cmp i1 i2 =
		String.compare (join_class_path i1 ".") (join_class_path i2 ".")
	in

	(* Body of main function *)
	(match obj with
	| TClassDecl class_def -> visit_class class_def;
		(match class_def.cl_init with Some expression -> visit_types expression | _ -> ())
	| TEnumDecl enum_def -> visit_enum enum_def
	| TTypeDecl _ | TAbstractDecl _ -> (* These are expanded *) ());

	List.sort inc_cmp (List.filter (fun path -> (include_class_header path) ) (pmap_keys !types))
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
		let cpp_file = new_cpp_file common_ctx.file ([],filename) in
		let output_main = (cpp_file#write) in

		output_main "#include <hxcpp.h>\n\n";
		output_main "#include <stdio.h>\n\n";

		List.iter ( add_include cpp_file ) depend_referenced;
		output_main "\n\n";

		output_main ( if is_main then "HX_BEGIN_MAIN\n\n" else "HX_BEGIN_LIB_MAIN\n\n" );
		gen_expression (new_context common_ctx cpp_file false file_info) false main_expression;
		output_main ";\n";
		output_main ( if is_main then "HX_END_MAIN\n\n" else "HX_END_LIB_MAIN\n\n" );
		cpp_file#close;
	in
	generate_startup "__main__" true;
	generate_startup "__lib__" false
   ;;

let generate_dummy_main common_ctx =
	let generate_startup filename is_main =
		let main_file = new_cpp_file common_ctx.file ([],filename) in
		let output_main = (main_file#write) in
		output_main "#include <hxcpp.h>\n\n";
		output_main "#include <stdio.h>\n\n";
		output_main ( if is_main then "HX_BEGIN_MAIN\n\n" else "HX_BEGIN_LIB_MAIN\n\n" );
		output_main ( if is_main then "HX_END_MAIN\n\n" else "HX_END_LIB_MAIN\n\n" );
		main_file#close;
	in
	generate_startup "__main__" true;
	generate_startup "__lib__" false
   ;;

let generate_boot common_ctx boot_classes init_classes =
	(* Write boot class too ... *)
	let base_dir = common_ctx.file in
	let boot_file = new_cpp_file base_dir ([],"__boot__") in
	let output_boot = (boot_file#write) in
	output_boot "#include <hxcpp.h>\n\n";
	List.iter ( fun class_path ->
		output_boot ("#include <" ^
			( join_class_path class_path "/" ) ^ ".h>\n")
			) boot_classes;

	output_boot "\nvoid __boot_all()\n{\n";
	output_boot "hx::RegisterResources( hx::GetResources() );\n";
	List.iter ( fun class_path ->
		output_boot ("::" ^ ( join_class_path class_path "::" ) ^ "_obj::__register();\n") ) boot_classes;
	List.iter ( fun class_path ->
		output_boot ("::" ^ ( join_class_path class_path "::" ) ^ "_obj::__init__();\n") ) (List.rev init_classes);
   let dump_boot =
	List.iter ( fun class_path ->
		output_boot ("::" ^ ( join_class_path class_path "::" ) ^ "_obj::__boot();\n") ) in
   dump_boot (List.filter  (fun path -> is_cpp_class path )  (List.rev boot_classes));
   dump_boot (List.filter  (fun path -> not (is_cpp_class path) )  (List.rev boot_classes));

	output_boot "}\n\n";
	boot_file#close;;


let generate_files common_ctx file_info =
	(* Write __files__ class too ... *)
	let base_dir = common_ctx.file in
	let files_file = new_cpp_file base_dir ([],"__files__") in
	let output_files = (files_file#write) in
	output_files "#include <hxcpp.h>\n\n";
	output_files "namespace hx {\n";
	output_files "const char *__hxcpp_all_files[] = {\n";
	output_files "#ifdef HXCPP_DEBUGGER\n";
	List.iter ( fun file -> output_files ("	" ^ file ^ ",\n" ) ) ( List.sort String.compare ( pmap_keys !file_info) );
	output_files "#endif\n";
	output_files " 0 };\n";
	output_files "const char *__hxcpp_class_path[] = {\n";
	output_files "#ifdef HXCPP_DEBUGGER\n";
	List.iter ( fun file -> output_files ("	\"" ^ file ^ "\",\n" ) ) (common_ctx.class_path @ common_ctx.std_path);
	output_files "#endif\n";
	output_files " 0 };\n";
	output_files "} // namespace hx\n";
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
		cached_source_writer
			( base_dir ^ "/src/" ^ ( String.concat "-" (fst class_path) ) ^ "-" ^
			(snd class_path) ^ ".cpp")
	end else
		new_cpp_file common_ctx.file class_path;;



let generate_enum_files common_ctx enum_def super_deps meta file_info =
	let class_path = enum_def.e_path in
	let just_class_name =  (snd class_path) in
	let class_name =  just_class_name ^ "_obj" in
	let smart_class_name =  ("::" ^ (join_class_path class_path "::") )  in
	(*let cpp_file = new_cpp_file common_ctx.file class_path in*)
	let cpp_file = new_placed_cpp_file common_ctx class_path in
	let output_cpp = (cpp_file#write) in
	let debug = false in
	let ctx = new_context common_ctx cpp_file debug file_info in

	if (debug) then
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
			output_cpp (smart_class_name ^ "  " ^ class_name ^ "::" ^ name ^ "(" ^
				(gen_tfun_arg_list args) ^")\n");
			output_cpp ("	{ return hx::CreateEnum< " ^ class_name ^ " >(" ^ (str name) ^ "," ^
				(string_of_int constructor.ef_index) ^ ",hx::DynamicArray(0," ^
				(string_of_int (List.length args)) ^  ")" );
			List.iter (fun (arg,_,_) -> output_cpp (".Add(" ^ (keyword_remap arg) ^ ")")) args;
			output_cpp "); }\n\n"

		| _ ->
			output_cpp ( smart_class_name ^ " " ^ class_name ^ "::" ^ name ^ ";\n\n" )
	) enum_def.e_constrs;




	output_cpp ("HX_DEFINE_CREATE_ENUM(" ^ class_name ^ ")\n\n");
	output_cpp ("int " ^ class_name ^ "::__FindIndex(::String inName)\n{\n");
	PMap.iter (fun _ constructor ->
		let name = constructor.ef_name in
		let idx = string_of_int constructor.ef_index in
		output_cpp ("	if (inName==" ^ (str name) ^ ") return " ^ idx ^ ";\n") ) enum_def.e_constrs;
	output_cpp ("	return super::__FindIndex(inName);\n");
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
		output_cpp ("	if (inName==" ^ (str name) ^ ") return " ^ count ^ ";\n") ) enum_def.e_constrs;
		output_cpp ("	return super::__FindArgCount(inName);\n");
		output_cpp ("}\n\n");

	(* Dynamic "Get" Field function - string version *)
	output_cpp ("Dynamic " ^ class_name ^ "::__Field(const ::String &inName,bool inCallProp)\n{\n");
	let dump_constructor_test _ constr =
		output_cpp ("	if (inName==" ^ (str constr.ef_name) ^ ") return " ^
                   (keyword_remap constr.ef_name) );
		if ( (constructor_arg_count constr) > 0 ) then output_cpp "_dyn()";
		output_cpp (";\n")
	in
	PMap.iter dump_constructor_test enum_def.e_constrs;
	output_cpp ("	return super::__Field(inName,inCallProp);\n}\n\n");

	output_cpp "static ::String sStaticFields[] = {\n";
	let sorted =
	   List.sort (fun f1 f2 -> (PMap.find f1 enum_def.e_constrs ).ef_index -
				   (PMap.find f2 enum_def.e_constrs ).ef_index )
			  (pmap_keys enum_def.e_constrs) in

 	List.iter (fun name -> output_cpp ("	" ^ (str name) ^ ",\n") ) sorted;

	output_cpp "	::String(null()) };\n\n";

	(* ENUM - Mark static as used by GC *)
	output_cpp "static void sMarkStatics(HX_MARK_PARAMS) {\n";
	PMap.iter (fun _ constructor ->
		let name = keyword_remap constructor.ef_name in
		match constructor.ef_type with
		| TFun (_,_) -> ()
		| _ -> output_cpp ("	HX_MARK_MEMBER_NAME(" ^ class_name ^ "::" ^ name ^ ",\"" ^ name ^ "\");\n") )
	enum_def.e_constrs;
	output_cpp "};\n\n";

	(* ENUM - Visit static as used by GC *)
	output_cpp "static void sVisitStatic(HX_VISIT_PARAMS) {\n";
	output_cpp ("	HX_VISIT_MEMBER_NAME(" ^ class_name ^ "::__mClass,\"__mClass\");\n");
	PMap.iter (fun _ constructor ->
		let name = keyword_remap constructor.ef_name in
		match constructor.ef_type with
		| TFun (_,_) -> ()
		| _ -> output_cpp ("	HX_VISIT_MEMBER_NAME(" ^ class_name ^ "::" ^ name ^ ",\"" ^ name ^ "\");\n") )
	enum_def.e_constrs;
	output_cpp "};\n\n";

	output_cpp "static ::String sMemberFields[] = { ::String(null()) };\n";

	output_cpp ("Class " ^ class_name ^ "::__mClass;\n\n");

	output_cpp ("Dynamic __Create_" ^ class_name ^ "() { return new " ^ class_name ^ "; }\n\n");

	output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
	let text_name = str (join_class_path class_path ".") in
	output_cpp ("\nhx::Static(__mClass) = hx::RegisterClass(" ^ text_name ^
					", hx::TCanCast< " ^ class_name ^ " >,sStaticFields,sMemberFields,\n");
	output_cpp ("	&__Create_" ^ class_name ^ ", &__Create,\n");
	output_cpp ("	&super::__SGetClass(), &Create" ^ class_name ^ ", sMarkStatics, sVisitStatic);\n");
	output_cpp ("}\n\n");

	output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
	(match meta with
		| Some expr ->
			let ctx = new_context common_ctx cpp_file false file_info in
			find_local_functions_and_return_blocks_ctx ctx true expr;
			output_cpp ("__mClass->__meta__ = ");
			gen_expression ctx true expr;
			output_cpp ";\n"
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

	let h_file = new_header_file common_ctx.file class_path in
	let super = "hx::EnumBase_obj" in
	let output_h = (h_file#write) in
	let def_string = join_class_path class_path "_"  in
	ctx.ctx_output <- output_h;

	begin_header_file output_h def_string;

	List.iter (gen_forward_decl h_file ) referenced;

	gen_open_namespace output_h class_path;

	output_h "\n\n";
	output_h ("class " ^ class_name ^ " : public " ^ super ^ "\n");
	output_h ("{\n	typedef " ^ super ^ " super;\n");
	output_h ("		typedef " ^ class_name ^ " OBJ_;\n");
	output_h "\n	public:\n";
	output_h ("		" ^ class_name ^ "() {};\n");
	output_h ("		HX_DO_ENUM_RTTI;\n");
	output_h ("		static void __boot();\n");
	output_h ("		static void __register();\n");
		output_h ("		::String GetEnumName( ) const { return " ^
									(str (join_class_path class_path "."))  ^ "; }\n" );
	output_h ("		::String __ToString() const { return " ^
									(str (just_class_name ^ ".") )^ " + tag; }\n\n");


	PMap.iter (fun _ constructor ->
		let name = keyword_remap constructor.ef_name in
		output_h ( "		static " ^  smart_class_name ^ " " ^ name );
		match constructor.ef_type with
		| TFun (args,_) ->
			output_h ( "(" ^ (gen_tfun_arg_list args) ^");\n");
			output_h ( "		static Dynamic " ^ name ^ "_dyn();\n");
		| _ ->
			output_h ";\n";
		   output_h ( "		static inline " ^  smart_class_name ^ " " ^ name ^
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


let has_init_field class_def =
	match class_def.cl_init with
	| Some _ -> true
	| _ -> false;;

let is_macro meta =
  Meta.has Meta.Macro meta
;;


let access_str a = match a with
	| AccNormal -> "AccNormal"
	| AccNo -> "AccNo"
	| AccNever -> "AccNever"
	| AccResolve -> "AccResolve"
	| AccCall(_) -> "AccCall"
	| AccInline -> "AccInline"
	| AccRequire(_,_) -> "AccRequire" ;;

let generate_class_files common_ctx member_types super_deps constructor_deps class_def file_info scriptable =
	let class_path = class_def.cl_path in
	let class_name = (snd class_def.cl_path) ^ "_obj" in
	let smart_class_name =  (snd class_def.cl_path)  in
	(*let cpp_file = new_cpp_file common_ctx.file class_path in*)
	let cpp_file = new_placed_cpp_file common_ctx class_path in
	let output_cpp = (cpp_file#write) in
	let debug = false in
	let ctx = new_context common_ctx cpp_file debug file_info in
	ctx.ctx_class_name <- "::" ^ (join_class_path class_path "::");
	ctx.ctx_class_super_name <- (match class_def.cl_super with
      | Some (klass, params) -> class_string klass "_obj" params
      | _ -> "");
	ctx.ctx_class_member_types <- member_types;
	if debug then print_endline ("Found class definition:" ^ ctx.ctx_class_name);

	let ptr_name = "hx::ObjectPtr< " ^ class_name ^ " >" in
	let constructor_type_var_list =
		match class_def.cl_constructor with
		| Some definition ->
					(match definition.cf_expr with
						| Some { eexpr = TFunction function_def } ->
							List.map (fun (v,o) -> gen_arg_type_name v.v_name o v.v_type "__o_")
									function_def.tf_args;
						| _ ->
							(match follow definition.cf_type with
								| TFun (args,_) -> List.map (fun (a,_,t) -> (type_string t,a) )  args
								| _ -> [])
					)
		| _ -> [] in
	let constructor_var_list = List.map snd constructor_type_var_list in
	let constructor_type_args = String.concat ","
				(List.map (fun (t,a) -> t ^ " " ^ a) constructor_type_var_list) in
	let constructor_args = String.concat "," constructor_var_list in

	let implement_dynamic = implement_dynamic_here class_def in

	output_cpp "#include <hxcpp.h>\n\n";

	let field_integer_dynamic = scriptable || (has_field_integer_lookup class_def) in
	let field_integer_numeric = scriptable || (has_field_integer_numeric_lookup class_def) in

	let all_referenced = find_referenced_types ctx.ctx_common (TClassDecl class_def) super_deps constructor_deps false false scriptable in
	List.iter ( add_include cpp_file  ) all_referenced;


	(* All interfaces (and sub-interfaces) implemented *)
	let implemented_hash = Hashtbl.create 0 in
	List.iter (fun imp ->
		let rec descend_interface interface =
			let imp_path = (fst interface).cl_path in
			let interface_name = "::" ^ (join_class_path imp_path "::" ) in
			if ( not (Hashtbl.mem implemented_hash interface_name) ) then begin
				Hashtbl.add implemented_hash interface_name ();
				List.iter descend_interface (fst interface).cl_implements;
			end
		in descend_interface imp
	) (real_interfaces class_def.cl_implements);
	let implemented = hash_keys implemented_hash in

   if (scriptable) then
      output_cpp "#include <hx/Scriptable.h>\n";

	output_cpp ( get_code class_def.cl_meta Meta.CppFileCode );

	gen_open_namespace output_cpp class_path;
	output_cpp "\n";

	output_cpp ( get_code class_def.cl_meta Meta.CppNamespaceCode );

	if (not class_def.cl_interface) then begin
		output_cpp ("Void " ^ class_name ^ "::__construct(" ^ constructor_type_args ^ ")\n{\n");
		(match class_def.cl_constructor with
			| Some definition ->
					(match  definition.cf_expr with
					| Some { eexpr = TFunction function_def } ->
      				hx_stack_push ctx output_cpp smart_class_name "new" function_def.tf_expr.epos;
						if (has_default_values function_def.tf_args) then begin
							generate_default_values ctx function_def.tf_args "__o_";
							gen_expression ctx false (to_block function_def.tf_expr);
							output_cpp ";\n";
						end else begin
							gen_expression ctx false (to_block function_def.tf_expr);
							output_cpp ";\n";
							(*gen_expression (new_context common_ctx cpp_file debug ) false function_def.tf_expr;*)
						end
					| _ -> ()
					)
			| _ -> ());
			output_cpp "	return null();\n";
			output_cpp "}\n\n";

		(* Destructor goes in the cpp file so we can "see" the full definition of the member vars *)
		output_cpp ( class_name ^ "::~" ^ class_name ^ "() { }\n\n");
		output_cpp ("Dynamic " ^ class_name ^ "::__CreateEmpty() { return  new " ^ class_name ^ "; }\n");

		output_cpp (ptr_name ^ " " ^ class_name ^ "::__new(" ^constructor_type_args ^")\n");

		let create_result () =
			output_cpp ("{  " ^ ptr_name ^ " result = new " ^ class_name ^ "();\n");
			in
		create_result ();
		output_cpp ("	result->__construct(" ^ constructor_args ^ ");\n");
		output_cpp ("	return result;}\n\n");

		output_cpp ("Dynamic " ^ class_name ^ "::__Create(hx::DynamicArray inArgs)\n");
		create_result ();
		output_cpp ("	result->__construct(" ^ (array_arg_list constructor_var_list) ^ ");\n");
		output_cpp ("	return result;}\n\n");
		if ( (List.length implemented) > 0 ) then begin
			output_cpp ("hx::Object *" ^ class_name ^ "::__ToInterface(const type_info &inType) {\n");
			List.iter (fun interface_name ->
				output_cpp ("	if (inType==typeid( " ^ interface_name ^ "_obj)) " ^
					"return operator " ^ interface_name ^ "_obj *();\n");
				) implemented;
			output_cpp ("	return super::__ToInterface(inType);\n}\n\n");
		end;

	end;

	(match class_def.cl_init with
	| Some expression ->
		output_cpp ("void " ^ class_name^ "::__init__() {\n");
      hx_stack_push ctx output_cpp smart_class_name "__init__" expression.epos;
		gen_expression (new_context common_ctx cpp_file debug file_info) false (to_block expression);
		output_cpp "}\n\n";
	| _ -> ());

	let statics_except_meta = (List.filter (fun static -> static.cf_name <> "__meta__") class_def.cl_ordered_statics) in
	let implemented_fields = List.filter should_implement_field statics_except_meta in

	List.iter
		(gen_field ctx class_def class_name smart_class_name false class_def.cl_interface)
		class_def.cl_ordered_fields;
	List.iter
		(gen_field ctx class_def class_name smart_class_name true class_def.cl_interface) statics_except_meta;
	output_cpp "\n";


	(* Initialise non-static variables *)
	if (not class_def.cl_interface) then begin
		output_cpp (class_name ^ "::" ^ class_name ^  "()\n{\n");
		if (implement_dynamic) then
			output_cpp "	HX_INIT_IMPLEMENT_DYNAMIC;\n";
		List.iter
			(fun field -> let remap_name = keyword_remap field.cf_name in
				match field.cf_expr with
				| Some { eexpr = TFunction function_def } ->
						if (is_dynamic_haxe_method field) then
							output_cpp ("	" ^ remap_name ^ " = new __default_" ^ remap_name ^ "(this);\n")
				| _ -> ()
			)
			class_def.cl_ordered_fields;
		output_cpp "}\n\n";


		let dump_field_iterator macro field =
			if (is_data_member field) then begin
				let remap_name = keyword_remap field.cf_name in
				output_cpp ("	" ^ macro ^ "(" ^ remap_name ^ ",\"" ^ field.cf_name^ "\");\n");

					(match field.cf_kind with Var { v_read = AccCall name } when (is_dynamic_accessor name "get" field class_def) ->
						output_cpp ("\t" ^ macro ^ "(" ^ name ^ "," ^ "\"" ^ name ^ "\");\n" ) | _ -> ());
					(match field.cf_kind with Var { v_write = AccCall name } when  (is_dynamic_accessor name "set" field class_def) ->
						output_cpp ("\t" ^ macro ^ "(" ^ name ^ "," ^ "\"" ^ name ^ "\");\n" ) | _ -> ());
				end
		in

      let implemented_instance_fields = List.filter should_implement_field class_def.cl_ordered_fields in

		(* MARK function - explicitly mark all child pointers *)
		output_cpp ("void " ^ class_name ^ "::__Mark(HX_MARK_PARAMS)\n{\n");
		output_cpp ("	HX_MARK_BEGIN_CLASS(" ^ smart_class_name ^ ");\n");
		if (implement_dynamic) then
			output_cpp "	HX_MARK_DYNAMIC;\n";
		List.iter (dump_field_iterator "HX_MARK_MEMBER_NAME") implemented_instance_fields;
		(match  class_def.cl_super with Some _ -> output_cpp "	super::__Mark(HX_MARK_ARG);\n" | _ -> () );
		output_cpp "	HX_MARK_END_CLASS();\n";
		output_cpp "}\n\n";

		(* Visit function - explicitly visit all child pointers *)
		output_cpp ("void " ^ class_name ^ "::__Visit(HX_VISIT_PARAMS)\n{\n");
		if (implement_dynamic) then
			output_cpp "	HX_VISIT_DYNAMIC;\n";
		List.iter (dump_field_iterator "HX_VISIT_MEMBER_NAME") implemented_instance_fields;
		(match  class_def.cl_super with Some _ -> output_cpp "	super::__Visit(HX_VISIT_ARG);\n" | _ -> () );
		output_cpp "}\n\n";


		let variable_field field =
			(match field.cf_expr with
			| Some { eexpr = TFunction function_def } -> is_dynamic_haxe_method field
			| _ -> true)
		in
      let is_readable field =
			(match field.cf_kind with | Var { v_read = AccNever } | Var { v_read = AccInline } -> false
			| _ -> true) in
      let is_writable field =
			(match field.cf_kind with | Var { v_write = AccNever } | Var { v_read = AccInline } -> false
			| _ -> true) in

      let reflective field = not (Meta.has Meta.Unreflective field.cf_meta) in
		let reflect_fields = List.filter reflective (statics_except_meta @ class_def.cl_ordered_fields) in
		let reflect_writable = List.filter is_writable reflect_fields in
		let reflect_readable = List.filter is_readable reflect_fields in
		let reflect_write_variables = List.filter variable_field reflect_writable in

		let dump_quick_field_test fields =
			if ( (List.length fields) > 0) then begin
				let len = function (_,l,_) -> l in
				let sfields = List.sort (fun f1 f2 -> (len f1)-(len f2)) fields in
				let len_case = ref (-1) in
				output_cpp "	switch(inName.length) {\n";
				List.iter (fun (field,l,result) ->
					if (l <> !len_case) then begin
						if (!len_case>=0) then output_cpp "		break;\n";
						output_cpp ("	case " ^ (string_of_int l) ^ ":\n");
						len_case := l;
					end;
					output_cpp ("		if (HX_FIELD_EQ(inName,\"" ^  (Ast.s_escape field)  ^ "\") ) { " ^ result ^ " }\n");
				) sfields;
				output_cpp "	}\n";
			end;
		in


		(* Dynamic "Get" Field function - string version *)
		output_cpp ("Dynamic " ^ class_name ^ "::__Field(const ::String &inName,bool inCallProp)\n{\n");
		let get_field_dat = List.map (fun f ->
			(f.cf_name, String.length f.cf_name, "return " ^
				(match f.cf_kind with
				| Var { v_read = AccCall prop } when is_extern_field f -> (keyword_remap prop) ^ "()"
				| Var { v_read = AccCall prop } -> "inCallProp ? " ^ (keyword_remap prop) ^ "() : " ^
				        ((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()")
				| _ -> ((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()")
				) ^ ";"
			) )
		in
		dump_quick_field_test (get_field_dat reflect_readable);
		if (implement_dynamic) then
			output_cpp "	HX_CHECK_DYNAMIC_GET_FIELD(inName);\n";
		output_cpp ("	return super::__Field(inName,inCallProp);\n}\n\n");

		(* Dynamic "Get" Field function - int version *)
		if ( field_integer_numeric || field_integer_dynamic) then begin
			let dump_static_ids = (fun field ->
				let remap_name = keyword_remap field.cf_name in
				output_cpp ("static int __id_" ^ remap_name ^ " = __hxcpp_field_to_id(\"" ^
								  	(field.cf_name) ^ "\");\n");
				) in
			List.iter dump_static_ids reflect_readable;
			output_cpp "\n\n";


			let output_ifield return_type function_name =
			output_cpp (return_type ^" " ^ class_name ^ "::" ^ function_name ^ "(int inFieldID)\n{\n");
			let dump_field_test = (fun f ->
				let remap_name = keyword_remap f.cf_name in
				output_cpp ("	if (inFieldID==__id_" ^ remap_name ^ ") return "  ^
					( if (return_type="Float") then "hx::ToDouble( " else "" ) ^
					(match f.cf_kind with
					| Var { v_read = AccCall prop } -> (keyword_remap prop) ^ "()"
					| _ -> ((keyword_remap f.cf_name) ^ if ( variable_field f) then "" else "_dyn()")
					) ^ ( if (return_type="Float") then " ) " else "" ) ^ ";\n");
				) in
			List.iter dump_field_test reflect_readable;
			if (implement_dynamic) then
				output_cpp "	HX_CHECK_DYNAMIC_GET_INT_FIELD(inFieldID);\n";
			output_cpp ("	return super::" ^ function_name ^ "(inFieldID);\n}\n\n");
			in

			if (field_integer_dynamic) then output_ifield "Dynamic" "__IField";
			if (field_integer_numeric) then output_ifield "Float" "__INumField";
		end;


		(* Dynamic "Set" Field function *)
		output_cpp ("Dynamic " ^ class_name ^ "::__SetField(const ::String &inName,const Dynamic &inValue,bool inCallProp)\n{\n");

		let set_field_dat = List.map (fun f ->
         let default_action =
            (keyword_remap f.cf_name) ^ "=inValue.Cast< " ^ (type_string f.cf_type) ^ " >();" ^
               " return inValue;" in
			(f.cf_name, String.length f.cf_name,
				(match f.cf_kind with
				| Var { v_write = AccCall prop } when is_extern_field f -> "return " ^ (keyword_remap prop) ^ "(inValue);"
				| Var { v_write = AccCall prop } -> "if (inCallProp) return " ^ (keyword_remap prop) ^ "(inValue);"
					 ^ default_action
            | _ -> default_action
				)
         )
		) in

		dump_quick_field_test (set_field_dat reflect_write_variables);
		if (implement_dynamic) then begin
			output_cpp ("	try { return super::__SetField(inName,inValue,inCallProp); }\n");
			output_cpp ("	catch(Dynamic e) { HX_DYNAMIC_SET_FIELD(inName,inValue); }\n");
			output_cpp "	return inValue;\n}\n\n";
		end else
			output_cpp ("	return super::__SetField(inName,inValue,inCallProp);\n}\n\n");

		(* For getting a list of data members (eg, for serialization) *)
		let append_field =
			(fun field -> output_cpp ("	outFields->push(" ^( str field.cf_name )^ ");\n")) in
		let is_data_field field = (match follow field.cf_type with | TFun _ -> false | _ -> true) in

		output_cpp ("void " ^ class_name ^ "::__GetFields(Array< ::String> &outFields)\n{\n");
		List.iter append_field (List.filter is_data_field class_def.cl_ordered_fields);
		if (implement_dynamic) then
			output_cpp "	HX_APPEND_DYNAMIC_FIELDS(outFields);\n";
		output_cpp "	super::__GetFields(outFields);\n";
		output_cpp "};\n\n";

		let dump_field_name = (fun field -> output_cpp ("	" ^  (str field.cf_name) ^ ",\n")) in
		output_cpp "static ::String sStaticFields[] = {\n";
		List.iter dump_field_name  implemented_fields;
		output_cpp "	String(null()) };\n\n";

		output_cpp "static ::String sMemberFields[] = {\n";
		List.iter dump_field_name  implemented_instance_fields;
		output_cpp "	String(null()) };\n\n";

     end; (* cl_interface *)

		(* Mark static variables as used *)
		output_cpp "static void sMarkStatics(HX_MARK_PARAMS) {\n";
		output_cpp ("	HX_MARK_MEMBER_NAME(" ^ class_name ^ "::__mClass,\"__mClass\");\n");
		List.iter (fun field ->
			if (is_data_member field) then
				output_cpp ("	HX_MARK_MEMBER_NAME(" ^ class_name ^ "::" ^ (keyword_remap field.cf_name) ^ ",\"" ^  field.cf_name ^ "\");\n") )
			implemented_fields;
		output_cpp "};\n\n";

		(* Visit static variables *)
		output_cpp "static void sVisitStatics(HX_VISIT_PARAMS) {\n";
		output_cpp ("	HX_VISIT_MEMBER_NAME(" ^ class_name ^ "::__mClass,\"__mClass\");\n");
		List.iter (fun field ->
			if (is_data_member field) then
				output_cpp ("	HX_VISIT_MEMBER_NAME(" ^ class_name ^ "::" ^ (keyword_remap field.cf_name) ^ ",\"" ^  field.cf_name ^ "\");\n") )
			implemented_fields;
		output_cpp "};\n\n";

   if (scriptable ) then begin
      let dump_script_field idx (field,f_args,return_t) =
        let args = if (class_def.cl_interface) then
              gen_tfun_interface_arg_list f_args
           else
              gen_tfun_arg_list f_args in
        let names = List.map (fun (n,_,_) -> keyword_remap n) f_args in
        let return_type = type_string return_t in
        let ret = if (return_type="Void") then " " else "return " in
        let name = keyword_remap field.cf_name in
        let vtable =  "__scriptVTable[" ^ (string_of_int idx) ^ "] " in
        let args_varray = (List.fold_left (fun l n -> l ^ ".Add(" ^ n ^ ")") "Array<Dynamic>()" names)  in
        let args_comma = List.fold_left (fun l n -> l ^ "," ^ n) "" names in
        output_cpp ("   " ^ return_type ^ " " ^ name ^ "( " ^ args ^ " ) { ");
        if (class_def.cl_interface) then begin
           output_cpp (" " ^ ret ^ "mDelegate->__Field(HX_CSTRING(\"" ^ field.cf_name ^ "\"),false)");
           if (List.length names <= 5) then
              output_cpp ("->__run(" ^ (String.concat "," names) ^ ")")
           else
              output_cpp ("->__Run(" ^ args_varray ^ ")");
           output_cpp ";return null(); }\n";
        end else begin
           output_cpp (" if (" ^ vtable ^ ") " ^ ret);
           if (List.length names <= 5) then
               output_cpp("hx::ScriptableCall" ^ (string_of_int (List.length names)) ^
                  "("^ vtable ^ ",this" ^ args_comma ^ ");")
           else
               output_cpp("hx::ScriptableCallMult("^ vtable ^ ",this," ^ args_varray^ "->Pointer());");
           output_cpp (" else " ^ ret ^ class_name ^ "::" ^ name ^ "(" ^ (String.concat "," names)^ "); return null(); }\n");
        end
      in
      let sctipt_name = class_name ^ "__scriptable" in
      output_cpp ("class " ^ sctipt_name ^ " : public " ^ class_name ^ " {\n" );
      output_cpp ("   typedef "^sctipt_name ^" __ME;\n");
      if (class_def.cl_interface) then
         output_cpp ("   HX_DEFINE_SCRIPTABLE_INTERFACE\n")
      else begin
         output_cpp ("   HX_DEFINE_SCRIPTABLE(HX_ARR_LIST" ^ (string_of_int (List.length constructor_var_list) ) ^ ")\n");
	      if (not implement_dynamic) then
			   output_cpp "	HX_DEFINE_SCRIPTABLE_DYNAMIC;\n";
      end;
      let functions = all_virtual_functions class_def in
	   list_iteri dump_script_field functions;
      output_cpp ("};\n\n");

      if (not class_def.cl_interface) then begin
         output_cpp "static String __scriptableFunctionNames[] = {\n";
         List.iter (fun (f,_,_) -> output_cpp ("  HX_CSTRING(\"" ^ f.cf_name ^ "\"),\n" ) ) functions;
         output_cpp "  String(null()) };\n";
      end;
   end;



	(* Initialise static in boot function ... *)
	if (not class_def.cl_interface) then begin
		(* Remap the specialised "extern" classes back to the generic names *)
		let class_name_text = match class_path with
			| path -> join_class_path path "." in

		output_cpp ("Class " ^ class_name ^ "::__mClass;\n\n");

		output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
		output_cpp ("	hx::Static(__mClass) = hx::RegisterClass(" ^ (str class_name_text)  ^
				", hx::TCanCast< " ^ class_name ^ "> ,sStaticFields,sMemberFields,\n");
		output_cpp ("	&__CreateEmpty, &__Create,\n");
		output_cpp ("	&super::__SGetClass(), 0, sMarkStatics, sVisitStatics);\n");
      if (scriptable) then
            output_cpp ("  HX_SCRIPTABLE_REGISTER_CLASS(\""^class_name_text^"\"," ^ class_name ^ ");\n");
		output_cpp ("}\n\n");

	end else begin
		let class_name_text = join_class_path class_path "." in

		output_cpp ("Class " ^ class_name ^ "::__mClass;\n\n");

		output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
		output_cpp ("	hx::Static(__mClass) = hx::RegisterClass(" ^ (str class_name_text)  ^
				", hx::TCanCast< " ^ class_name ^ "> ,0,0,\n");
		output_cpp ("	0, 0,\n");
		output_cpp ("	&super::__SGetClass(), 0, sMarkStatics, sVisitStatics);\n");
      if (scriptable) then
         output_cpp ("  HX_SCRIPTABLE_REGISTER_INTERFACE(\""^class_name_text^"\"," ^ class_name ^ ");\n");
		output_cpp ("}\n\n");
	end;

   output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
	List.iter (gen_field_init ctx ) (List.filter should_implement_field class_def.cl_ordered_statics);
	output_cpp ("}\n\n");


	gen_close_namespace output_cpp class_path;

	cpp_file#close;


	let h_file = new_header_file common_ctx.file class_path in
	let super = match class_def.cl_super with
		| Some (klass,params) -> (class_string klass "_obj" params)
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
		output_h ("#include <" ^ ( join_class_path super_path "/" ) ^ ".h>\n")
	| _ -> () );

	(* And any interfaces ... *)
	List.iter (fun imp->
		let imp_path = (fst imp).cl_path in
		output_h ("#include <" ^ ( join_class_path imp_path "/" ) ^ ".h>\n") )
		(real_interfaces class_def.cl_implements);

   (* Only need to foreward-declare classes that are mentioned in the header file
	   (ie, not the implementation)  *)
   let referenced = find_referenced_types ctx.ctx_common (TClassDecl class_def) super_deps (Hashtbl.create 0) true false scriptable in
	List.iter ( gen_forward_decl h_file ) referenced;

	output_h ( get_code class_def.cl_meta Meta.HeaderCode );

	gen_open_namespace output_h class_path;
	output_h "\n\n";
	output_h ( get_code class_def.cl_meta Meta.HeaderNamespaceCode );

   let extern_class =  Common.defined common_ctx Define.DllExport in
	let attribs = "HXCPP_" ^ (if extern_class then "EXTERN_" else "") ^ "CLASS_ATTRIBUTES " in

	output_h ("class " ^ attribs ^ " " ^ class_name ^ " : public " ^ super );
	output_h "{\n	public:\n";
	output_h ("		typedef " ^ super ^ " super;\n");
	output_h ("		typedef " ^ class_name ^ " OBJ_;\n");

	if (not class_def.cl_interface) then begin
		output_h ("		" ^ class_name ^  "();\n");
		output_h ("		Void __construct(" ^ constructor_type_args ^ ");\n");
		output_h "\n	public:\n";
		output_h ("		static " ^ptr_name^ " __new(" ^constructor_type_args ^");\n");
		output_h ("		static Dynamic __CreateEmpty();\n");
		output_h ("		static Dynamic __Create(hx::DynamicArray inArgs);\n");
		output_h ("		~" ^ class_name ^ "();\n\n");
		output_h ("		HX_DO_RTTI;\n");
		if (field_integer_dynamic) then output_h "		Dynamic __IField(int inFieldID);\n";
		if (field_integer_numeric) then output_h "		double __INumField(int inFieldID);\n";
		if (implement_dynamic) then
			output_h ("		HX_DECLARE_IMPLEMENT_DYNAMIC;\n");
		output_h ("		static void __boot();\n");
		output_h ("		static void __register();\n");
		output_h ("		void __Mark(HX_MARK_PARAMS);\n");
		output_h ("		void __Visit(HX_VISIT_PARAMS);\n");

		List.iter (fun interface_name ->
			output_h ("		inline operator " ^ interface_name ^ "_obj *()\n			" ^
					"{ return new " ^ interface_name ^ "_delegate_< " ^ class_name ^" >(this); }\n" );
		) implemented;

		if ( (List.length implemented) > 0 ) then
			output_h "		hx::Object *__ToInterface(const type_info &inType);\n";

		if (has_init_field class_def) then
			output_h "		static void __init__();\n\n";
		output_h ("		::String __ToString() const { return " ^ (str smart_class_name) ^ "; }\n\n");
	end else begin
		output_h ("		HX_DO_INTERFACE_RTTI;\n");
		output_h ("		static void __boot();\n");
   end;


	(match class_def.cl_array_access with
	| Some t -> output_h ("		typedef " ^ (type_string t) ^ " __array_access;\n")
	| _ -> ());


	let interface = class_def.cl_interface in
	List.iter (gen_member_def ctx class_def false interface) (List.filter should_implement_field class_def.cl_ordered_fields);
	List.iter (gen_member_def ctx class_def true interface)  (List.filter should_implement_field class_def.cl_ordered_statics);

	output_h ( get_code class_def.cl_meta Meta.HeaderClassCode );

	output_h "};\n\n";

	if (class_def.cl_interface) then begin
		output_h ("#define DELEGATE_" ^ (join_class_path  class_def.cl_path "_" ) ^ " \\\n");
		List.iter (fun field ->
		match follow field.cf_type, field.cf_kind  with
		| _, Method MethDynamic -> ()
		| TFun (args,return_type), Method _ ->
			(* TODO : virtual ? *)
			let remap_name = keyword_remap field.cf_name in
			output_h ( "virtual "  ^ (type_string return_type) ^ " " ^ remap_name ^ "( " );
			output_h (gen_tfun_interface_arg_list args);
			output_h (") { return mDelegate->" ^ remap_name^ "(");
			output_h (String.concat "," (List.map (fun (name,opt,typ) -> (keyword_remap name)) args));
			output_h ");}  \\\n";
			output_h ("virtual Dynamic " ^ remap_name ^ "_dyn() { return mDelegate->" ^
						remap_name ^ "_dyn();}  \\\n");
		| _ -> ()
		) class_def.cl_ordered_fields;
		output_h ("\n\n");
		output_h ("template<typename IMPL>\n");
		output_h ("class " ^ smart_class_name ^ "_delegate_ : public " ^ class_name^"\n");
		output_h "{\n	protected:\n";
		output_h ("		IMPL *mDelegate;\n");
		output_h "	public:\n";
		output_h ("		" ^ smart_class_name ^ "_delegate_(IMPL *inDelegate) : mDelegate(inDelegate) {}\n");
		output_h ("		hx::Object *__GetRealObject() { return mDelegate; }\n");
		output_h ("		void __Visit(HX_VISIT_PARAMS) { HX_VISIT_OBJECT(mDelegate); }\n");
		let rec dump_delegate interface =
			output_h ("		DELEGATE_" ^ (join_class_path  interface.cl_path "_" ) ^ "\n");
			match interface.cl_super with | Some super -> dump_delegate (fst super) | _ -> ();
		in
		dump_delegate class_def;
		output_h "};\n\n";
	end;


	gen_close_namespace output_h class_path;

	end_header_file output_h def_string;
	h_file#close;
	let depend_referenced = find_referenced_types ctx.ctx_common (TClassDecl class_def) super_deps constructor_deps false true false in
	depend_referenced;;

let write_resources common_ctx =
	let resource_file = new_cpp_file common_ctx.file ([],"__resources__") in
	resource_file#write "#include <hxcpp.h>\n\n";

	let idx = ref 0 in
	Hashtbl.iter (fun _ data ->
		resource_file#write_i ("static unsigned char __res_" ^ (string_of_int !idx) ^ "[] = {\n");
		for i = 0 to String.length data - 1 do
		let code = Char.code (String.unsafe_get data i) in
			resource_file#write  (Printf.sprintf "0x%.2x, " code);
			if ( (i mod 10) = 9) then resource_file#write "\n";
		done;
		resource_file#write ("};\n");
		incr idx;
	) common_ctx.resources;

	idx := 0;
	resource_file#write "hx::Resource __Resources[] =";
	resource_file#begin_block;
	Hashtbl.iter (fun name data ->
		resource_file#write_i
			("{ " ^ (str name) ^ "," ^ (string_of_int (String.length data)) ^ "," ^
				"__res_" ^ (string_of_int !idx) ^ " },\n");
		incr idx;
	) common_ctx.resources;

	resource_file#write_i "{String(null()),0,0}";
	resource_file#end_block_line;
	resource_file#write ";\n\n";
	resource_file#write "namespace hx { Resource *GetResources() { return __Resources; } } \n\n";
	resource_file#close;;



let write_build_data filename classes main_deps build_extra exe_name =
	let buildfile = open_out filename in
	let add_class_to_buildfile class_def =
		let class_path = fst class_def in
		let deps = snd class_def in
		let cpp = (join_class_path class_path "/") ^ ".cpp" in
		output_string buildfile ( "  <file name=\"src/" ^ cpp ^ "\">\n" );
		let project_deps = List.filter (fun path -> not (is_internal_class path) ) deps in
		List.iter (fun path-> output_string buildfile ("   <depend name=\"" ^
        ( match path with
         | (["@verbatim"],file) -> file
         | _ -> "include/" ^ (join_class_path path "/") ^ ".h" )
       ^ "\"/>\n") ) project_deps;
		output_string buildfile ( "  </file>\n" )
	in

	output_string buildfile "<xml>\n";
	output_string buildfile "<files id=\"haxe\">\n";
	output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
	List.iter add_class_to_buildfile classes;
	add_class_to_buildfile (  ( [] , "__boot__") , [] );
	add_class_to_buildfile (  ( [] , "__files__") , [] );
	add_class_to_buildfile (  ( [] , "__resources__") , [] );
	output_string buildfile "</files>\n";
	output_string buildfile "<files id=\"__lib__\">\n";
	output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
	add_class_to_buildfile (  ( [] , "__lib__") , main_deps );
	output_string buildfile "</files>\n";
	output_string buildfile "<files id=\"__main__\">\n";
	output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
	add_class_to_buildfile (  ( [] , "__main__") , main_deps );
	output_string buildfile "</files>\n";
	output_string buildfile ("<set name=\"HAXE_OUTPUT\" value=\"" ^ exe_name ^ "\" />\n");
	output_string buildfile "<include name=\"${HXCPP}/build-tool/BuildCommon.xml\"/>\n";
	output_string buildfile build_extra;
	output_string buildfile "</xml>\n";
	close_out buildfile;;

let write_build_options filename defines =
	let writer = cached_source_writer filename in
	writer#write ( defines ^ "\n");
	let cmd = Unix.open_process_in "haxelib path hxcpp" in
	writer#write (Pervasives.input_line cmd);
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
		| TClassDecl class_def ->
			let deps = ref [] in
			(match class_def.cl_super with Some super ->
				deps := ((fst super).cl_path) :: !deps
			| _ ->() );
			List.iter (fun imp -> deps := (fst imp).cl_path :: !deps) (real_interfaces class_def.cl_implements);
			Hashtbl.add result class_def.cl_path !deps;
		| TEnumDecl enum_def ->
			Hashtbl.add result enum_def.e_path [];
		| _ -> () );
		) common_ctx.types;
	result;;

let create_constructor_dependencies common_ctx =
	let result = Hashtbl.create 0 in
	List.iter (fun object_def ->
		(match object_def with
		| TClassDecl class_def ->
			(match class_def.cl_constructor with
           | Some func_def -> Hashtbl.add result class_def.cl_path func_def
           | _ -> () )
		| _ -> () );
		) common_ctx.types;
	result;;


let gen_extern_class common_ctx class_def =
   let file = new_source_file common_ctx.file  "extern" ".hx" class_def.cl_path in
   let path = class_def.cl_path in
   let rec remove_prefix  =  function
      | TInst ({cl_path=prefix} as cval ,tl) ->  TInst ( { cval with cl_path = ([],snd cval.cl_path) }, List.map remove_prefix tl)
      | t -> Type.map remove_prefix t
      in
   let s_type t = s_type (Type.print_context()) (remove_prefix t) in
   let output = file#write in
   let params = function [] -> "" | l ->  "<" ^ (String.concat "," (List.map (fun (n,t) -> n) l) ^ ">")  in
   let args  = function  TFun (args,_) ->
       String.concat "," (List.map (fun (name,opt,t) -> (if opt then "?" else "") ^ name ^":"^ (s_type t)) args) | _ -> "" in
   let ret  = function  TFun (_,ret) -> s_type ret | _ -> "Dynamic" in

   let print_field stat f =
		output ("\t" ^ (if stat then "static " else "") ^ (if f.cf_public then "public " else "") );
      (match f.cf_kind, f.cf_name with
	   | Var { v_read = AccNormal; v_write = AccNormal },_ -> output ("var " ^ f.cf_name ^ ":" ^ (s_type f.cf_type))
	   | Var v,_ -> output ("var " ^ f.cf_name ^ "(" ^ (s_access v.v_read) ^ "," ^ (s_access v.v_write) ^ "):" ^ (s_type f.cf_type))
	   | Method _, "new" -> output ("function new(" ^ (args f.cf_type) ^ "):Void")
	   | Method MethDynamic, _  -> output ("dynamic function " ^ f.cf_name ^ (params f.cf_params) ^ "(" ^ (args f.cf_type) ^ "):" ^ (ret f.cf_type) )
	   | Method _, _  -> output ("function " ^ f.cf_name ^ (params f.cf_params) ^ "(" ^ (args f.cf_type) ^ "):" ^ (ret f.cf_type) )
      );
		output ";\n\n";
	in
   let c = class_def in
	output ( "package " ^ (String.concat "." (fst path)) ^ ";\n" );
	output ( "@:include extern " ^ (if c.cl_private then "private " else "") ^ (if c.cl_interface then "interface" else "class")
              ^ " " ^ (snd path) ^ (params c.cl_types) );
	(match c.cl_super with None -> () | Some (c,pl) -> output (" extends " ^  (s_type (TInst (c,pl)))));
	List.iter (fun (c,pl) -> output ( " implements " ^ (s_type (TInst (c,pl))))) c.cl_implements;
	(match c.cl_dynamic with None -> () | Some t -> output (" implements Dynamic<" ^ (s_type t) ^ ">"));
	(match c.cl_array_access with None -> () | Some t -> output (" implements ArrayAccess<" ^ (s_type t) ^ ">"));
	output "{\n";
	(match c.cl_constructor with
	| None -> ()
	| Some f -> print_field false f);
	List.iter (print_field false) c.cl_ordered_fields;
	List.iter (print_field true) c.cl_ordered_statics;
	output "}";
   output "\n";
	file#close
;;

(* The common_ctx contains the haxe AST in the "types" field and the resources *)
let generate common_ctx =
	make_base_directory common_ctx.file;

	let debug = false in
	let exe_classes = ref [] in
	let boot_classes = ref [] in
	let init_classes = ref [] in
	let file_info = ref PMap.empty in
	let class_text path = join_class_path path "::" in
	let member_types = create_member_types common_ctx in
	let super_deps = create_super_dependencies common_ctx in
	let constructor_deps = create_constructor_dependencies common_ctx in
	let main_deps = ref [] in
	let build_xml = ref "" in
   let scriptable = (Common.defined common_ctx Define.Scriptable) in
   let gen_externs = scriptable || (Common.defined common_ctx Define.DllExport) in
   if (gen_externs) then begin
     make_base_directory (common_ctx.file ^ "/extern");
   end;

	List.iter (fun object_def ->
		(match object_def with
		| TClassDecl class_def when class_def.cl_extern ->
         if (gen_externs) then gen_extern_class common_ctx class_def;
		| TClassDecl class_def ->
			let name =  class_text class_def.cl_path in
         if (gen_externs) then gen_extern_class common_ctx class_def;
			let is_internal = is_internal_class class_def.cl_path in
			if (is_internal || (is_macro class_def.cl_meta) ) then
				( if debug then print_endline (" internal class " ^ name ))
			else begin
				build_xml := !build_xml ^ (get_code class_def.cl_meta Meta.BuildXml);
				boot_classes := class_def.cl_path ::  !boot_classes;
				if (has_init_field class_def) then
					init_classes := class_def.cl_path ::  !init_classes;
				let deps = generate_class_files common_ctx
               member_types super_deps constructor_deps class_def file_info scriptable in
				exe_classes := (class_def.cl_path, deps)  ::  !exe_classes;
			end
		| TEnumDecl enum_def ->
			let name =  class_text enum_def.e_path in
			let is_internal = is_internal_class enum_def.e_path in
			if (is_internal) then
				(if debug then print_endline (" internal enum " ^ name ))
			else begin
				let meta = Codegen.build_metadata common_ctx object_def in
				if (enum_def.e_extern) then
					(if debug then print_endline ("external enum " ^ name ));
				boot_classes := enum_def.e_path :: !boot_classes;
				let deps = generate_enum_files common_ctx enum_def super_deps meta file_info in
				exe_classes := (enum_def.e_path, deps) :: !exe_classes;
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

	generate_boot common_ctx !boot_classes !init_classes;

	generate_files common_ctx file_info;

	write_resources common_ctx;


	let output_name = match  common_ctx.main_class with
	| Some path -> (snd path)
	| _ -> "output" in

	write_build_data (common_ctx.file ^ "/Build.xml") !exe_classes !main_deps !build_xml output_name;
	let cmd_defines = ref "" in
	PMap.iter ( fun name value -> match name with
        | "true" | "sys" | "dce" | "cpp" | "debug" -> ()
        | _ -> cmd_defines := !cmd_defines ^ " -D" ^ name ^ "=\"" ^ (escape_command value) ^ "\"" ) common_ctx.defines;
	write_build_options (common_ctx.file ^ "/Options.txt") !cmd_defines;
	if ( not (Common.defined common_ctx Define.NoCompilation) ) then begin
		let old_dir = Sys.getcwd() in
		Sys.chdir common_ctx.file;
		let cmd = ref "haxelib run hxcpp Build.xml haxe" in
		if (common_ctx.debug) then cmd := !cmd ^ " -Ddebug";
      cmd := !cmd ^ !cmd_defines;
      print_endline !cmd;
		if common_ctx.run_command !cmd <> 0 then failwith "Build failed";
		Sys.chdir old_dir;
	end
	;;



