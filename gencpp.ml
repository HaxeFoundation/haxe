(*
 *  haXe/CPP Compiler
 *  Copyright (c)2008 Hugh Sanderson
 *  based on and including code by (c)2005-2008 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Type
open Common


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
		let path = base ^ "/" ^ dir  in
		if not (Sys.file_exists path) then
			Unix.mkdir path 0o755;
		make_class_directories path remaining
	);;


let new_source_file base_dir sub_dir extension class_path =
	make_class_directories base_dir ( sub_dir :: (fst class_path));
	cached_source_writer
		( base_dir ^ "/" ^ sub_dir ^ "/" ^ ( String.concat "/" (fst class_path) ) ^ "/" ^
		(snd class_path) ^ extension);;


let new_cpp_file base_dir = new_source_file base_dir "src" ".cpp";;

let new_header_file base_dir = new_source_file base_dir "include" ".h";;

let make_base_directory file =
	make_class_directories "." (file :: []);;


(* CPP code generation context *)

type context =
{
	mutable ctx_output : string -> unit;
	mutable ctx_writer : source_writer;
	mutable ctx_calling : bool;
	mutable ctx_assigning : bool;
	mutable ctx_return_from_block : bool;
	(* This is for returning from the child nodes of TMatch, TSwitch && TTry *)
	mutable ctx_return_from_internal_node : bool;
	mutable ctx_debug : bool;
	mutable ctx_debug_type : bool;
	mutable ctx_do_safe_point : bool;
	mutable ctx_real_this_ptr : bool;
	mutable ctx_dynamic_this_ptr : bool;
	mutable ctx_static_id_curr : int;
	mutable ctx_static_id_used : int;
	mutable ctx_static_id_depth : int;
	mutable ctx_switch_id : int;
	mutable ctx_class_name : string;
	mutable ctx_local_function_args : (string,string) Hashtbl.t;
	mutable ctx_local_return_block_args : (string,string) Hashtbl.t;
	mutable ctx_class_member_types : (string,string) Hashtbl.t;
}

let new_context writer debug = 
	{
	ctx_writer = writer;
	ctx_output = (writer#write);
	ctx_calling = false;
	ctx_assigning = false;
	ctx_debug = debug;
	ctx_debug_type = debug;
	ctx_return_from_block = false;
	ctx_return_from_internal_node = false;
	ctx_do_safe_point = false;
	ctx_real_this_ptr = true;
	ctx_dynamic_this_ptr = false;
	ctx_static_id_curr = 0;
	ctx_static_id_used = 0;
	ctx_static_id_depth = 0;
	ctx_switch_id = 0;
	ctx_class_name = "";
	ctx_local_function_args = Hashtbl.create 0;
	ctx_local_return_block_args = Hashtbl.create 0;
	ctx_class_member_types =  Hashtbl.create 0;
	}


(* The internal classes are implemented by the core hxcpp system, so the cpp
	 classes should not be generated *)
let is_internal_class = function
	|  ([],"Int") | ([],"Void") |  ([],"String") | ([], "Null") | ([], "Float")
	|  ([],"Array") | ([], "Class") | ([], "Enum") | ([], "Bool")
	|  ([], "Dynamic") | ([], "ArrayAccess") -> true
	|  (["cpp"], "CppInt32__") | ([],"Math") | (["haxe";"io"], "Unsigned_char__") -> true
	| _ -> false


(* The internal header files are also defined in the hx/Object.h file, so you do
	 #include them separately.  However, the Int32 and Math classes do have their
	 own header files (these are under the hxcpp tree) so these should be included *)
let is_internal_header = function
	| ([],"@Main") -> true
	| (["cpp"], "CppInt32__") | ([],"Math") -> false
	| path -> is_internal_class path



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
	| "int" -> "toInt"
	| "asm" | "auto" | "char" | "const" | "delete" | "double" | "enum"
	| "extern" | "float" | "friend" | "goto" | "long" | "operator" | "protected"
	| "register" | "short" | "signed" | "sizeof" | "template" | "typedef"
	| "union" | "unsigned" | "void" | "volatile" | "or" | "and" | "xor" | "or_eq"
	| "and_eq" | "xor_eq" | "typeof" | "stdin" | "stdout" | "stderr"
	| "BIG_ENDIAN" | "LITTLE_ENDIAN"
	| "assert" | "NULL"
	| "struct" -> "_" ^ name
	| x -> x

(*
 While #include "Math.h" sould be different from "#include <math.h>", and it may be possible
  to use include paths to get this right, I think it is easier just to chnage the name *)
let include_remap = function | ([],"Math") -> ([],"hxMath") | x -> x;;


(* Add include to source code *)
let add_include writer class_path =
	writer#add_include (include_remap class_path);;


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
let is_basic_type = function
	| "Int" | "Bool" | "Float" | "::String" | "::haxe::io::Unsigned_char__" -> true
	| "int" | "bool" | "double" -> true
	| _ -> false

(*  Get a string to represent a type.
	 The "suffix" will be nothing or "_obj", depending if we want the name of the
	 pointer class or the pointee (_obj class *)
let rec class_string klass suffix params =
	(match klass.cl_path with
	(* Array class *)
	|  ([],"Array") -> (snd klass.cl_path) ^ suffix ^ "< " ^ (String.concat ","
					 (List.map type_string  params) ) ^ " >"
	| _ when klass.cl_kind=KTypeParameter -> "Dynamic"
	|  ([],"#Int") -> "/* # */int"
	|  (["haxe";"io"],"Unsigned_char__") -> "unsigned char"
	|  ([],"Class") -> "::Class"
	|  ([],"Null") -> (match params with
			| [t] ->
				(match follow t with
				| TInst ({ cl_path = [],"Int" },_)
				| TInst ({ cl_path = [],"Float" },_)
				| TEnum ({ e_path = [],"Bool" },_) -> "Dynamic"
				| _ -> "/*NULL*/" ^ (type_string t) )
			| _ -> assert false); 
	(* Normal class *)
	| _ -> "::" ^ (join_class_path klass.cl_path "::") ^ suffix
	)
and type_string_suff suffix haxe_type =
	(match haxe_type with
	| TMono r -> (match !r with None -> "Dynamic" ^ suffix | Some t -> type_string_suff suffix t)
	| TEnum ({ e_path = ([],"Void") },[]) -> "Void"
	| TEnum ({ e_path = ([],"Bool") },[]) -> "bool"
	| TInst ({ cl_path = ([],"Float") },[]) -> "double"
	| TInst ({ cl_path = ([],"Int") },[]) -> "int"
	| TEnum (enum,params) ->  "::" ^ (join_class_path enum.e_path "::") ^ suffix
	| TInst (klass,params) ->  (class_string klass suffix params)
	| TType (type_def,params) ->
		(match type_def.t_path with
		| [] , "Null" ->
			(match params with
			| [t] ->
				(match follow t with
				| TInst ({ cl_path = [],"Int" },_)
				| TInst ({ cl_path = [],"Float" },_)
				| TEnum ({ e_path = [],"Bool" },_) -> "Dynamic" ^ suffix
				| _ -> type_string_suff suffix t)
			| _ -> assert false);
		| [] , "Array" ->
			(match params with
			| [t] -> "Array< " ^ (type_string (follow t) ) ^ " >"
			| _ -> assert false)
		| _ ->  type_string_suff suffix (apply_params type_def.t_types params type_def.t_type)
		)
	| TFun (args,haxe_type) -> "Dynamic" ^ suffix
	| TAnon anon -> "Dynamic" ^ suffix
	| TDynamic haxe_type -> "Dynamic" ^ suffix
	| TLazy func -> type_string_suff suffix ((!func)())
	)
and type_string haxe_type = 
	type_string_suff "" haxe_type;;

let is_array haxe_type =
	match follow haxe_type with
	| TInst (klass,params) -> 
		(match klass.cl_path with
		| [] , "Array" -> true
		| _ -> false )
	| TType (type_def,params) ->
		(match type_def.t_path with
		| [] , "Array" -> true
		| _ -> false )
	| _ -> false
	;;
 

let is_dynamic haxe_type = type_string haxe_type ="Dynamic";;


(* Get the type and output it to the stream *)
let gen_type ctx haxe_type =
	ctx.ctx_output (type_string haxe_type);;

(* Get the type and output it to the stream *)
let gen_type_suff ctx haxe_type suff =
	ctx.ctx_output (type_string_suff suff haxe_type);;

let member_type ctx field_object member =
	let name = (type_string field_object.etype) ^ "." ^ member in
	try ( Hashtbl.find ctx.ctx_class_member_types name )
	with Not_found -> "?";;

let is_interface obj =
	match obj.etype with
	| TInst (klass,params) -> klass.cl_interface
	| _ -> false;;

let is_function_member expression =
	match (follow expression.etype) with | TFun (_,_) -> true | _ -> false;;

(* Some fields of a dynamic object are internal and should be accessed directly,
	rather than through the abstract interface.  In haxe code, these will be written
	as "untyped" values.  *)
let dynamic_access ctx field_object member is_function =
	match member with
	| "__Field" | "__IField" | "__Run" | "__Is" | "__GetClass" | "__GetType" | "__ToString"
	| "__s" | "__GetPtr" | "__SetField" | "__length" | "__IsArray" | "__SetThis"
	| "__EnumParams" | "__Index" | "__Tag" | "__GetFields" | "toString" | "__HasField"
			-> false
	| _ ->
		let could_be_dynamic_interface haxe_type =
   		if (is_array haxe_type) then false else
				(match type_string haxe_type with
				| "::String" | "Null" | "::Class" | "::Enum" | "::Math" | "::ArrayAccess" -> false
				| _ -> true ) in
		if ( (could_be_dynamic_interface field_object.etype) &&
			  ((member_type ctx field_object member)="?") ) then true else
		if ( (is_interface field_object) && (not is_function) ) then true else
		match field_object.eexpr with
		| TConst TThis when ((not ctx.ctx_real_this_ptr) && ctx.ctx_dynamic_this_ptr) -> true
		| _ -> (match follow field_object.etype with
			| TMono mono -> true
			| TAnon anon -> true
			| TDynamic haxe_type -> true
			| other -> (type_string other ) = "Dynamic");;

let gen_arg_type_name name default_val arg_type prefix =
	let remap_name = keyword_remap name in
	let type_str = (type_string arg_type) in
	match default_val with
	| Some TNull when (type_str="::String") -> (type_str,remap_name)
	| Some constant when (is_basic_type type_str) -> ("Dynamic",prefix ^ remap_name)
	| _ -> (type_str,remap_name);;


(* Generate prototype text, including allowing default values to be null *)
let gen_arg name default_val arg_type prefix =
	let pair = gen_arg_type_name name default_val arg_type prefix in
	(fst pair) ^ " " ^ (snd pair);;

let rec gen_arg_list arg_list prefix =
  String.concat "," (List.map (fun (name,o,arg_type) -> (gen_arg name o arg_type prefix) ) arg_list)


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


let has_utf8_chars s = 
	let result = ref false in
	for i = 0 to String.length s - 1 do
		result := !result || ( Char.code (String.unsafe_get s i) > 127 )
	done;
	!result;;

let quote s l =
   l := String.length s;
   escape_stringw (Ast.s_escape s) l;;

let str s =
   let l = ref 0 in
	let q = quote s l in
   "HX_STRING(" ^ q ^ "," ^ (string_of_int !l) ^ ")";;

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
	"/* " ^
	(match expression.eexpr with
	| TConst _ -> "TConst"
	| TLocal _ -> "TLocal"
	| TEnumField _ -> "TEnumField"
	| TArray (_,_) -> "TArray"
	| TBinop (_,_,_) -> "TBinop"
	| TField (_,_) -> "TField"
	| TClosure _ -> "TClosure"
	| TTypeExpr _ -> "TTypeExpr"
	| TParenthesis _ -> "TParenthesis"
	| TObjectDecl _ -> "TObjectDecl"
	| TArrayDecl _ -> "TArrayDecl"
	| TCall (_,_) -> "TCall"
	| TNew (_,_,_) -> "TNew"
	| TUnop (_,_,_) -> "TUnop"
	| TFunction _ -> "TFunction"
	| TVars _ -> "TVars"
	| TBlock _ -> "TBlock"
	| TFor (_,_,_,_) -> "TFor"
	| TIf (_,_,_) -> "TIf"
	| TWhile (_,_,_) -> "TWhile"
	| TSwitch (_,_,_) -> "TSwitch"
	| TMatch (_,_,_,_) -> "TMatch"
	| TTry (_,_) -> "TTry"
	| TReturn _ -> "TReturn"
	| TBreak -> "TBreak"
	| TContinue -> "TContinue"
	| TThrow _ -> "TThrow" ) ^
	(if (type_too) then " = " ^ (type_string expression.etype) else "") ^
	" */";;



(* This is like the Type.iter, but also keeps the "retval" flag up to date *)
let rec iter_retval f retval e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TEnumField _
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
	| TFor (_,_,e1,e2) ->
		f true e1;
		f false e2;
	| TThrow e
	| TField (e,_)
	| TClosure (e,_)
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
		List.iter (fun (_,_,e) -> match e with None -> () | Some e -> f true e) vl
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
		List.iter (fun (_,_,e) -> f false e) catches
	| TReturn eo ->
		(match eo with None -> () | Some e -> f true e)
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
			| TFor (_,_,_,_)
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
			(fun acc (v,t) -> incr n; match v with None -> acc | Some v -> (v,t,!n) :: acc) [] l)

exception AlreadySafe;;
exception PossibleRecursion;;

let expression_needs_safe_point expression =
	try (
	let rec needs_safe expression always_executed  =
	   (* TODO - fill this out *)
		Type.iter (fun expr -> match expr.eexpr with
			| TNew (_,_,_) when always_executed -> raise AlreadySafe
			| TCall (_,_) -> raise PossibleRecursion
			| _ -> needs_safe expr false;
			) expression in
	needs_safe expression true;
	false;
	) with  AlreadySafe -> false
	      | PossibleRecursion -> true
;;



let find_undeclared_variables_ctx ctx undeclared declarations this_suffix allow_this expression =
	let output = ctx.ctx_output in
	let rec find_undeclared_variables undeclared declarations this_suffix allow_this expression =
		match expression.eexpr with
		| TVars var_list ->
			List.iter (fun (var_name, var_type, optional_init) ->
				Hashtbl.add declarations var_name ();
				if (ctx.ctx_debug) then
					output ("/* found var " ^ var_name ^ "*/ ");
				match optional_init with
				| Some expression -> find_undeclared_variables undeclared declarations this_suffix allow_this expression
				| _ -> ()
				) var_list
		| TFunction func -> List.iter ( fun (arg_name, opt_val, arg_type) ->
				if (ctx.ctx_debug) then
					output ("/* found arg " ^ arg_name ^ " = " ^ (type_string arg_type) ^ " */ ");
				Hashtbl.add declarations arg_name () ) func.tf_args;
				find_undeclared_variables undeclared declarations this_suffix false func.tf_expr
		| TTry (try_block,catches) ->
			find_undeclared_variables undeclared declarations this_suffix allow_this try_block;
			List.iter (fun (name,t,catch_expt) ->
				let old_decs = Hashtbl.copy declarations in
				Hashtbl.add declarations name ();
				find_undeclared_variables undeclared declarations this_suffix allow_this catch_expt;
				Hashtbl.clear declarations;
				Hashtbl.iter ( Hashtbl.add declarations ) old_decs
				) catches;
		| TLocal local_name ->
			if  not (Hashtbl.mem declarations local_name) then
				Hashtbl.replace undeclared local_name (type_string expression.etype)
		| TMatch (condition, enum, cases, default) ->
			Type.iter (find_undeclared_variables undeclared declarations this_suffix allow_this) condition;
			List.iter (fun (case_ids,params,expression) ->
				let old_decs = Hashtbl.copy declarations in
				(match params with
				| None -> ()
				| Some l -> List.iter (fun (opt_name,t) ->
					match opt_name with | Some name -> Hashtbl.add declarations name () | _ -> ()  )
					l  );
				Type.iter (find_undeclared_variables undeclared declarations this_suffix allow_this) expression;
				Hashtbl.clear declarations;
				Hashtbl.iter ( Hashtbl.add declarations ) old_decs
				) cases;
			(match default with | None -> ()
			| Some expr ->
				Type.iter (find_undeclared_variables undeclared declarations this_suffix allow_this) expr;
			);
		| TFor (var_name, var_type, init, loop) ->
			let old_decs = Hashtbl.copy declarations in
			Hashtbl.add declarations var_name ();
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
		(* Add args as defined variables *)
		List.iter ( fun (arg_name, opt_val, arg_type) ->
			if (ctx.ctx_debug) then
				output ("/* found arg " ^ arg_name ^ " = " ^ (type_string arg_type) ^" */ ");
			Hashtbl.add declarations arg_name () ) func_def.tf_args;
		find_undeclared_variables_ctx ctx undeclared declarations "" true func_def.tf_expr;

		let has_this = Hashtbl.mem undeclared "this" in
		if (has_this) then Hashtbl.remove undeclared "this";
		let typed_vars = hash_iterate undeclared (fun key value -> value ^ "," ^ key ) in
		let func_name_sep = func_name ^ (if List.length typed_vars > 0 then "," else "") in
		output_i ("HX_BEGIN_LOCAL_FUNC" ^ (list_num typed_vars) ^ "(" ^ func_name_sep ^
						(String.concat "," typed_vars) ^ ")\n" );

		(* actual function, called "run" *)
		let args_and_types = List.map
				(fun (name,_,arg_type) -> (type_string arg_type) ^ " " ^ name ) func_def.tf_args in
		let block = is_block func_def.tf_expr in
		let func_type = type_string func_def.tf_type in
		output_i (func_type ^ " run(" ^ (String.concat "," args_and_types) ^ ")");

		let pop_real_this_ptr = clear_real_this_ptr ctx true in

		let do_safe = expression_needs_safe_point func_def.tf_expr in
		if (block) then begin
			writer#begin_block;
			ctx.ctx_do_safe_point <- do_safe;
			gen_expression ctx false func_def.tf_expr;
			output_i "return null();\n";
			writer#end_block;
		end else begin
			writer#begin_block;
			if (do_safe) then output_i "__SAFE_POINT;\n";
			(* Save old values, and equalize for new input ... *)
			let pop_names = push_anon_names ctx in

			find_local_functions_ctx ctx func_def.tf_expr;
			find_local_return_blocks_ctx ctx false func_def.tf_expr;

			(match func_def.tf_expr.eexpr with
			| TReturn (Some return_expression) when (func_type<>"Void") ->
				output_i "return ";
				gen_expression ctx true return_expression;
			| TReturn (Some return_expression) ->
				output_i "";
				gen_expression ctx false return_expression;
			| _ ->
				output_i "";
				gen_expression ctx false func_def.tf_expr;
			);
			output ";\n";
			output_i "return null();\n";
			pop_names();
			writer#end_block;
		end;
		pop_real_this_ptr();

		if (has_this) then begin
			output_i "Dynamic __this;\n";
			output_i "void __SetThis(Dynamic inThis) { __this = inThis; }\n";
		end;

		let return = if (type_string func_def.tf_type ) = "Void" then "(void)" else "return" in
		output_i ("HX_END_LOCAL_FUNC" ^ (list_num args_and_types) ^ "(" ^ return ^ ")\n\n");

		Hashtbl.replace ctx.ctx_local_function_args func_name
			(if (ctx.ctx_real_this_ptr) then
				String.concat "," (hash_keys undeclared)
			else
				String.concat "," (List.map remap_this (hash_keys undeclared)) )
	in
	define_local_function func_name func_def

and find_local_functions_ctx ctx expression =
	let output = ctx.ctx_output in
	let rec find_local_functions expression =
		match expression.eexpr with
		| TBlock _
		| TObjectDecl _ -> ()  (* stop at block - since that block will define the function *)
		| TCall (e,el) -> (* visit the args first, then the function *)
			List.iter find_local_functions  el;
			find_local_functions e
		| TFunction func ->
			let func_name = next_anon_function_name ctx in
			output "\n";
			define_local_function_ctx ctx func_name func
		| TField ( { eexpr = (TConst TNull) }, _ ) -> ( )
		| _ -> Type.iter find_local_functions expression
	in find_local_functions expression

and find_local_return_blocks_ctx ctx retval expression =
	let rec find_local_return_blocks retval expression =
		match expression.eexpr with
		| TBlock _ ->
			if (retval) then begin
				define_local_return_block_ctx ctx expression (next_anon_function_name ctx);
			end  (* else we are done *)
		| TFunction func -> ()
		| TField ( { eexpr = (TConst TNull) }, _ ) -> ( )
		| TMatch (_, _, _, _)
		| TTry (_, _)
		| TSwitch (_, _, _) when retval ->
				define_local_return_block_ctx ctx expression (next_anon_function_name ctx)
		| TObjectDecl decl_list ->
				let name = next_anon_function_name ctx in
				(*
				List.iter (fun (name,expr) -> iter_retval find_local_return_blocks true expr) decl_list;
				*)
				define_local_return_block_ctx ctx expression name;
		| _ -> iter_retval find_local_return_blocks retval expression
	in
	find_local_return_blocks retval expression

and define_local_return_block_ctx ctx expression name =
	let writer = ctx.ctx_writer in
	let output_i = writer#write_i in
	let output = ctx.ctx_output in
	let check_this = function | "this" when not ctx.ctx_real_this_ptr -> "__this" | x -> x in
	let reference = function | "this" -> " *__this" | name -> " &" ^name in
	let rec define_local_return_block expression  =
		let declarations = Hashtbl.create 0 in
		let undeclared = Hashtbl.create 0 in
		find_undeclared_variables_ctx ctx undeclared declarations "_obj" true expression;

		let vars = (hash_keys undeclared) in
		let args = String.concat "," (List.map check_this (hash_keys undeclared)) in
		Hashtbl.replace ctx.ctx_local_return_block_args name args;
		output_i ("struct " ^ name);
		writer#begin_block;
		let ret_type = type_string expression.etype in
		output_i ("inline static " ^ ret_type ^ " Block( ");
		output (String.concat "," ( (List.map (fun var ->
				(Hashtbl.find undeclared var) ^ (reference var)) ) vars));
		output (")");

		let pop_real_this_ptr = clear_real_this_ptr ctx false in
		(match expression.eexpr with
		| TObjectDecl decl_list ->
			writer#begin_block;
			output_i "hx::Anon __result = hx::Anon_obj::Create();\n";
			let pop_names = push_anon_names ctx in
			List.iter (function (name,value) ->
				find_local_return_blocks_ctx ctx true value;
				find_local_functions_ctx ctx value;
				output_i ( "__result->Add(" ^ (str name) ^ " , ");
				gen_expression ctx true value;
				output (");\n");
			) decl_list;
			pop_names();
			output_i "return __result;\n";
			writer#end_block;
		| TBlock _ ->
			ctx.ctx_return_from_block <- true;
			ctx.ctx_return_from_internal_node <- false;
			output "/* DEF (ret block)(not intern) */";
			gen_expression ctx false expression;
		| _ ->
			ctx.ctx_return_from_block <- false;
			ctx.ctx_return_from_internal_node <- true;
			output "/* DEF (not block)(ret intern) */";
			gen_expression ctx false (to_block expression);
		);
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
	let return_from_internal_node = ctx.ctx_return_from_internal_node in
	ctx.ctx_return_from_internal_node <- false;
	let do_safe_point = ctx.ctx_do_safe_point in
	ctx.ctx_do_safe_point <- false;

	(* Annotate source code with debug - can get a bit verbose.  Mainly for debugging code gen,
		rather than the run time *)
	if (ctx.ctx_debug) then begin
		if calling then output "/* Call */";
		if ctx.ctx_real_this_ptr then output "/* REAL */" else output "/* FAKE __this */";
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
			| "/" -> "double("
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
	let gen_member_access field_object member is_function return_type =
		let remap_name = keyword_remap member in
		begin
		let check_dynamic_member_access member = begin
			(match (dynamic_access ctx field_object member is_function) with
			| true when (not (dynamic_internal member)) ->
					let access = (if assigning then "->__FieldRef" else "->__Field") in
					(* output ( "/* " ^ (type_string field_object.etype) ^ " */" ); *)
					output ( access ^ "(" ^ (str member) ^ ")" );
					if (not assigning) then begin
						let return = type_string return_type in
						if ( not (return="Dynamic") ) then
							output (".Cast< " ^ return ^ " >()");
					end
			| _ ->
			let member_name = remap_name ^
				( if ( (not calling) && is_function && (not assigning)) then "_dyn()" else "" ) in
			if ( (type_string field_object.etype)="::String") then
				output ( "." ^ member_name)
			else begin
				output ( "->" ^ member_name);
				if (not assigning) then begin
					let expr_type = type_string return_type in
					let mem_type = member_type ctx field_object member in
					if ( (mem_type="Dynamic") && expr_type<>"Dynamic") then
						output (".Cast< " ^ expr_type ^ " >()");
				end;
			end )
		end in

		match field_object.eexpr with
		(* static access ... *)
		| TTypeExpr type_def ->
			let class_name = "::" ^ (join_class_path (t_path type_def) "::" ) in
			if (class_name="::String") then
				output ("::String::" ^ remap_name)
			else
				output (class_name ^ "_obj::" ^ remap_name);
			if ( (not calling) && (not assigning) && is_function) then
				output "_dyn()"
		| TArray (e1,e2) ->
			gen_expression ctx true e1;
			output "[";
			gen_expression ctx true e2;
			output "]";
			check_dynamic_member_access member
		| TBlock block -> let func_name = use_anon_function_name ctx in
			( try output ( func_name ^ "::Block(" ^
				(Hashtbl.find ctx.ctx_local_return_block_args func_name) ^ ")" )
			with Not_found ->
			 (output ("/* Block function " ^ func_name ^ " not found */" ) ) );
			check_dynamic_member_access member
		| TParenthesis expr ->
			output "(";
			ctx.ctx_calling <- calling;
			gen_expression ctx  true expr;
			output ")";
			check_dynamic_member_access member
		| TNew (klass,params,expressions) ->
			output ( ( class_string klass "_obj" params) ^ "::__new(" );
			gen_expression_list expressions;
			output ")";
			output ( "->" ^ member )
		| TLocal name when name = "__global__" ->
			output ("::" ^ member )
		| TConst TSuper -> output (if ctx.ctx_real_this_ptr then "this" else "__this");
						output ("->super::" ^ remap_name)
		| TConst TNull -> output "null()"
		| _ -> 
			gen_expression ctx true  field_object;
			check_dynamic_member_access member
	end in

	(match expression.eexpr with
	| TConst TNull when not retval ->
		output "{}";
	| TCall (func, arg_list) when (match func.eexpr with | TConst TSuper -> true | _ -> false ) ->
		output "super::__construct(";
		gen_expression_list arg_list;
		output ")";
	| TCall (func, arg_list) ->
		ctx.ctx_calling <- true;
		gen_expression ctx true func;
		output "(";
		gen_expression_list arg_list;
		output ")";
		(* This is a horrible hack - may need to prevent the strong typing of
			the return value in the first place.
			Eg.  haxe thinks List<X> first() is of type X, but cpp thinks it is Dynamic.
		*)
		let expr_type = type_string expression.etype in
			if (not(expr_type="Void")) then
				(match func.eexpr with 
				| TField(expr,name) ->
					let mem_type = member_type ctx expr name in
						if ( (mem_type="Dynamic") && (not(expr_type="Dynamic") ) ) then
							 output (".Cast< " ^ expr_type ^ " >()");
				| _ -> () )
	| TBlock expr_list ->
		if (retval) then begin
			let func_name = use_anon_function_name ctx in
			(
			try
			output ( func_name ^ "::Block(" ^
				(Hashtbl.find ctx.ctx_local_return_block_args func_name) ^ ")" )
			with Not_found ->
			 (*error ("Block function " ^ func_name ^ " not found" ) expression.epos;*)
			 output ("/* Block function " ^ func_name ^ " not found */" );
			)
		end else begin
			writer#begin_block;
			if (do_safe_point) then output_i "__SAFE_POINT\n";
			(* Save old values, and equalize for new input ... *)
			let pop_names = push_anon_names ctx in
			let remaining = ref (List.length expr_list) in
			List.iter (fun expresion ->
				find_local_functions_ctx ctx expresion;
				if (return_from_block && !remaining = 1) then begin
					find_local_return_blocks_ctx ctx true expresion;
					output_i "";
					ctx.ctx_return_from_internal_node <- return_from_internal_node;
					output "return ";
					gen_expression ctx true expresion;
				end else begin
					find_local_return_blocks_ctx ctx false expresion;
					output_i "";
					ctx.ctx_return_from_internal_node <- return_from_internal_node;
					gen_expression ctx false expresion;
				end;
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
	| TReturn optional_expr ->
		output "";
		( match optional_expr with
		| Some expression ->
			output "return ";
			gen_expression ctx true expression
		| _ -> output "return null()"
		)

	| TConst const ->
		(match const with
		| TInt i -> output (Printf.sprintf "%ld" i)
		| TFloat float_as_string -> output float_as_string
		| TString s -> output (str s)
		| TBool b -> output (if b then "true" else "false")
		(*| TNull -> output ("((" ^ (type_string expression.etype) ^ ")null())")*)
		| TNull -> output "null()"
		| TThis -> output (if ctx.ctx_real_this_ptr then "this" else "__this")
		| TSuper -> output (if ctx.ctx_real_this_ptr then "((super *)this)" else "((super*)__this)")
		)


	| TLocal local_name -> output (keyword_remap local_name);
	| TEnumField (enum, name) ->
			output ("::" ^ (join_class_path enum.e_path "::") ^ "_obj::" ^ name)
	| TArray (array_expr,index) ->
		if ( (assigning && (is_array array_expr.etype)) || (is_dynamic array_expr.etype) ) then begin
			gen_expression ctx true array_expr;
			output "[";
			gen_expression ctx true index;
			output "]";
		end else if (assigning) then begin
			(* output (" /*" ^ (type_string array_expr.etype) ^ " */ "); *)
			output "hxIndexRefNew(";
			gen_expression ctx true array_expr;
			output ",";
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
	| TClosure (expr,name)
	| TField (expr,name) ->
		gen_member_access expr name (is_function_member expression) expression.etype
	| TParenthesis expr -> output "("; gen_expression ctx true expr; output ")"
	| TObjectDecl decl_list ->
		let func_name = use_anon_function_name ctx in
		(try output ( func_name ^ "::Block(" ^
					(Hashtbl.find ctx.ctx_local_return_block_args func_name) ^ ")" )
		with Not_found ->
			output ("/* TObjectDecl block " ^ func_name ^ " not found */" ); )
	| TArrayDecl decl_list ->
		(* gen_type output expression.etype; *)
		output ( (type_string_suff "_obj" expression.etype) ^ "::__new()");
		List.iter ( fun elem -> output ".Add(";
							gen_expression ctx true elem;
							output ")" ) decl_list;
	| TNew (klass,params,expressions) ->
		if (klass.cl_path = ([],"String")) then
			output "::String("
		else
			output ( ( class_string klass "_obj" params) ^ "::__new(" );
		gen_expression_list expressions;
		output ")"
	| TUnop (op,Ast.Prefix,expr) ->
		ctx.ctx_assigning <- true;
		output (Ast.s_unop op);
		gen_expression ctx true expr
	| TUnop (op,Ast.Postfix,expr) ->
		ctx.ctx_assigning <- true;
		gen_expression ctx true expr;
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
		List.iter (fun (var_name, var_type, optional_init) ->
			gen_type ctx var_type;
			output (" " ^ (keyword_remap var_name) );
			(match optional_init with
			| None -> ()
			| Some expression -> output " = "; gen_expression ctx true expression);
			count := !count -1;
			if (!count > 0) then begin output ";\n"; output_i "" end
		) var_list
	| TFor (var_name, var_type, init, loop) ->
		output ("for(Dynamic __it = ");
		gen_expression ctx true init;
		output (";  __it->__Field(" ^ (str "hasNext") ^ ")(); )");
		ctx.ctx_writer#begin_block;
		output ( (type_string var_type) ^ " " ^ (keyword_remap var_name) ^
			" = __it->__Field(" ^ (str "next") ^ ")();\n" );
		output_i "";
		gen_expression ctx false loop;
		output ";\n";
		output_i "__SAFE_POINT\n";
		ctx.ctx_writer#end_block;
	| TIf (condition, if_expr, optional_else_expr)  ->
		let output_if_expr expr terminate =
			(match expr.eexpr with
			| TBlock _ -> gen_expression ctx false expr
			| _ ->  output "\n";
				output_i "";
				writer#indent_one;
				gen_expression ctx false expr;
				if (terminate) then output ";\n"
			) in

		(match optional_else_expr with
		| Some else_expr -> 
			if (retval) then begin
				gen_expression ctx true condition;
				output " ? ";
				let type_str = match (type_string expression.etype) with
				| "Void" -> "Dynamic"
				| other -> other
				in
				if ( true (*(type_string if_expr.etype) <> type_str*) ) then begin
					output (type_str ^ "( ");
					gen_expression ctx true if_expr;
					output " )";
				end else
					gen_expression ctx true if_expr;

				output " : ";

				if ( true (*(type_string else_expr.etype) <> type_str*) ) then begin
					output (type_str ^ "( ");
					gen_expression ctx true else_expr;
					output " )";
				end else
					gen_expression ctx true else_expr;
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
			output_if_expr if_expr false
		)
	| TWhile (condition, repeat, Ast.NormalWhile ) ->
			output  "while(";
			gen_expression ctx true condition;
			output ")";
			ctx.ctx_do_safe_point <- true;
			gen_expression ctx false repeat
	| TWhile (condition, repeat, Ast.DoWhile ) ->
			output "do";
			gen_expression ctx false repeat;
			output "while(";
			gen_expression ctx true condition;
			output ")"

	(* These have already been defined in find_local_return_blocks ... *)
	| TTry (_,_)
	| TSwitch (_,_,_)
	| TMatch (_, _, _, _) when (retval && (not return_from_internal_node) )->
		let func_name = use_anon_function_name ctx in
		(try output ( func_name ^ "::Block(" ^
					(Hashtbl.find ctx.ctx_local_return_block_args func_name) ^ ")" )
		with Not_found ->
			output ("/* return block " ^ func_name ^ " not found */" ); )
				(*error ("return block " ^ func_name ^ " not found" ) expression.epos;*)

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
	output ( (type_string condition.etype) ^ " " ^ tmp_var ^ " = " );
	gen_expression ctx true condition;
	output ";\n";

	output_i ("switch((" ^ tmp_var ^ ")->GetIndex())");
	writer#begin_block;
	List.iter (fun (case_ids,params,expression) ->
		output_i "";
		List.iter (fun id -> output ("case " ^ (string_of_int id) ^ ": ") ) case_ids;
		let has_params = match params with | Some _ -> true | _ -> false in
		if (has_params) then begin
			writer#begin_block;
			List.iter (fun (name,vtype,id) -> output_i
			((type_string vtype) ^ " " ^ name ^
				" = " ^ tmp_var ^ "->__Param(" ^ (string_of_int id) ^ ");\n"))
					(tmatch_params_to_args params);
		end;
		ctx.ctx_return_from_block <- return_from_internal_node;
		gen_expression ctx false (to_block expression);
		if (has_params) then writer#end_block;
		output_i ";break;\n";
	) cases;
	(match default with
	| None -> ()
	|  Some e ->
		output_i "default: ";
		ctx.ctx_return_from_block <- return_from_internal_node;
		gen_expression ctx false (to_block e);
	);
	writer#end_block
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
			List.iter (fun (name,t,expression) ->
				let type_name = type_string t in
				if (type_name="Dynamic") then begin
					seen_dynamic := true;
					output_i !else_str;
				end else
					output_i (!else_str ^ "if (__e.IsClass< " ^ type_name ^ " >() )");
				ctx.ctx_writer#begin_block;
				output_i (type_name ^ " " ^ name ^ " = __e;");
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
	);;



(*
let is_dynamic_method f =
	match follow f.cf_type with
	| TFun _ when f.cf_expr = None -> true
	| _ ->
		(match f.cf_expr with
		| Some { eexpr = TFunction fd } when f.cf_set = MethodAccess true -> true
		| Some { eexpr = TFunction fd } when f.cf_set = NormalAccess -> true
		| _ -> false);;
*)

let is_dynamic_method f =
		(match f.cf_expr with
		| Some { eexpr = TFunction fd } when f.cf_set = MethodAccess true -> true
		| Some { eexpr = TFunction fd } when f.cf_set = NormalAccess -> true
		| _ -> false);;


let is_data_member field =
	match field.cf_expr with
	| Some { eexpr = TFunction function_def } -> is_dynamic_method field
	| _ -> true;;


let default_value_string = function
	| TInt i -> Printf.sprintf "%ld" i
	| TFloat float_as_string -> float_as_string
	| TString s -> str s
	| TBool b -> (if b then "true" else "false")
	| TNull -> "null()"
	| _ -> "/* Hmmm */"


let generate_default_values ctx args prefix =
  List.iter ( fun (name,o,arg_type) -> let type_str = type_string arg_type in
	match o with
	| Some TNull when (type_str = "::String") -> ()
	| Some const when (is_basic_type type_str) ->
		ctx.ctx_output (type_str ^ " " ^ name ^ " = " ^ prefix ^ name ^ ".Default(" ^ 
			(default_value_string const) ^ ");\n")
	| _ -> () ) args;;


let has_default_values args =
	List.exists ( fun (name,o,arg_type) -> let type_str = type_string arg_type in
	match o with
	| Some TNull when (type_str = "::String") -> false
	| Some const when (is_basic_type type_str) -> true
	| _ -> false ) args;;


let gen_field ctx class_name ptr_name is_static is_external is_interface field =
	let output = ctx.ctx_output in
	ctx.ctx_real_this_ptr <- not is_static;
	let remap_name = keyword_remap field.cf_name in
	if (is_external || is_interface) then begin
		(* Just the dynamic glue ... *)
		match follow field.cf_type with
		| TFun (args,result) ->
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

		if (not (is_dynamic_method field)) then begin
			(* The actual function definition *)
			output return_type;
			output (" " ^ class_name ^ "::" ^ remap_name ^ "( " );
			output (gen_arg_list function_def.tf_args "__o_");
			output ")";
			ctx.ctx_real_this_ptr <- true;
			ctx.ctx_dynamic_this_ptr <- false;
			ctx.ctx_do_safe_point <- expression_needs_safe_point function_def.tf_expr;
			if (has_default_values function_def.tf_args) then begin
				ctx.ctx_writer#begin_block;
				generate_default_values ctx function_def.tf_args "__o_";
				gen_expression ctx false function_def.tf_expr;
				if (is_void) then output "return null();\n";
				ctx.ctx_writer#end_block;
			end else begin
				if (is_void) then ctx.ctx_writer#begin_block;
				gen_expression ctx false (to_block function_def.tf_expr);
				if (is_void) then begin
					output "return null();\n";
					ctx.ctx_writer#end_block;
				end;
			end;

			output "\n\n";
			(* generate dynamic version too ... *)
			if (is_static) then output "STATIC_";
			output ("HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ "," ^
							 remap_name ^ "," ^ ret ^ ")\n\n");

		end else begin
			ctx.ctx_real_this_ptr <- false;
			ctx.ctx_dynamic_this_ptr <- false;
			let func_name = "__default_" ^ (remap_name) in
			output ("HX_BEGIN_DEFAULT_FUNC(" ^ func_name ^ "," ^ class_name ^ ")\n");
			output return_type;
			output (" run(" ^ (gen_arg_list function_def.tf_args "") ^ ")");
			if (is_void) then begin
				ctx.ctx_writer#begin_block;
				gen_expression ctx false function_def.tf_expr;
				output "return null();\n";
				ctx.ctx_writer#end_block;
			end else
				gen_expression ctx false function_def.tf_expr;

			output ("HX_END_LOCAL_FUNC" ^ nargs ^ "(" ^ ret ^ ")\n");
			output ("HX_END_DEFAULT_FUNC\n\n");

			if (is_static) then
				output ( "Dynamic " ^ class_name ^ "::" ^ remap_name ^ ";\n\n");
		end

	(* Data field *)
	| _ ->
		if is_static then begin
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

		if (is_dynamic_method field) then begin
			let func_name = "__default_" ^ (remap_name) in
			output ( "	hx::Static(" ^ remap_name ^ ") = new " ^ func_name ^ ";\n\n" );
		end

	(* Data field *)
	| _ -> (match field.cf_expr with
		| Some expr ->
			find_local_functions_ctx ctx expr;
			find_local_return_blocks_ctx ctx true expr;
			output ( "	hx::Static(" ^ remap_name ^ ") = ");
			gen_expression ctx true expr;
			output ";\n"
		| _ ->
			output ( "	hx::Static(" ^ remap_name ^ ");\n");
		);
	)
	;;



let gen_member_def ctx is_static is_extern is_interface field =
	let output = ctx.ctx_output in
	let remap_name = keyword_remap field.cf_name in

	output (if is_static then "		static " else "		");
	if (is_extern || is_interface) then begin
		match follow field.cf_type with
		| TFun (args,return_type) ->
			output ( (if (not is_static) then "virtual " else "" ) ^ type_string return_type);
			output (" " ^ remap_name ^ "( " );
			output (String.concat "," (List.map (fun (name,opt,typ) ->
				(type_string typ) ^ " " ^ name ^ (if opt then "=null()" else "")) args));
			output (if (not is_static) then ")=0;\n" else ");\n");
			(*if (not is_interface) then begin*)
				output (if is_static then "		static " else "		");
				output ("Dynamic " ^ remap_name ^ "_dyn();\n" );
			(*end else
				output ("		virtual Dynamic " ^ remap_name ^ "_dyn() = 0;\n\n" );*)
		| _ -> 
			if (is_interface) then begin
				(*
				output "virtual ";
				gen_type ctx field.cf_type;
				output (" & __get_" ^ remap_name ^ "()=0;\n" ) *)
				output "\n";
			end else begin
				gen_type ctx field.cf_type;
				output (" " ^ remap_name ^ ";\n" );
			end
	end else (match  field.cf_expr with
	| Some { eexpr = TFunction function_def } ->
		if ( is_dynamic_method field ) then begin
			output ("Dynamic " ^ remap_name ^ ";\n");
			output (if is_static then "		static " else "		");
			(* external mem  Dynamic & *)
			output ("inline Dynamic &" ^ remap_name ^ "_dyn() " ^ "{return " ^ remap_name^ "; }\n") 
		end else begin
			let return_type = (type_string function_def.tf_type) in
			if (not is_static) then output "virtual ";
			output return_type;
			output (" " ^ remap_name ^ "( " );
			output (gen_arg_list function_def.tf_args "" );
			output ");\n";
			output (if is_static then "		static " else "		");
			output ("Dynamic " ^ remap_name ^ "_dyn();\n" )
			end;
		output "\n";
	| _ ->
		(* Variable access *)
		gen_type ctx field.cf_type;
		output (" " ^ remap_name ^ "; /* REM */ \n" );
		(* Add a "dyn" function for variable to unify variable/function access *)
		(match follow field.cf_type with
		| TFun (_,_) ->
			output "	";
			gen_type ctx field.cf_type;
			output (" &" ^ remap_name ^ "_dyn() { return " ^ remap_name ^ ";}\n" )
		| _ -> ()
			(*
			(match field.cf_get with
			| CallAccess name when name = ("get_" ^ field.cf_name) -> output (" Dynamic get_" ^ field.cf_name ^ ";\n" )
			| _ -> ()
			);
			(match field.cf_set with
			| CallAccess name when name = ("set_" ^ field.cf_name) -> output (" Dynamic set_" ^ field.cf_name ^ ";\n" )
			| _ -> ()
			)
			*)
		)
	)
	;;



(*
  Get a list of all classes referred to by the class/enum definition
  These are used for "#include"ing the appropriate header files.
*)
let find_referenced_types obj super_deps header_only =
	let types = ref PMap.empty in
	(* When a class or function is templated on type T, variables of that type show
		up as being in a package "class-name.T" or "function-name.T"  in these cases
		we just use "Dynamic" - TODO: Use cl_kind *)
	let ignore_class_name = ref "?" in
	let ignore_function_name = ref "?" in
	let rec add_type in_path =
		let package = (String.concat "." (fst in_path)) in
		if ( not ((package=(!ignore_function_name)) || (package=(!ignore_class_name)) ||
						(package="Array") || (package="Class")) ) then
			if ( not (PMap.mem in_path !types)) then begin
				types := (PMap.add in_path () !types);
				try
					List.iter add_type (Hashtbl.find super_deps in_path);
				with Not_found -> ()
			end
	in
	let rec visit_type in_type =
		match (follow in_type) with
		| TMono r -> (match !r with None -> () | Some t -> visit_type t)
		(*| TEnum ({ e_path = ([],"Void") },[]) -> ()
		| TEnum ({ e_path = ([],"Bool") },[]) -> () *)
		| TEnum (enum,params) -> add_type enum.e_path
		(* If a class has a template parameter, then we treat it as dynamic - except
			for the Array or Class class, for which we do a fully typed object *)
		| TInst (klass,params) -> add_type klass.cl_path;
			(match klass.cl_path with
				| ([],"Array") | ([],"Class") -> List.iter visit_type params
			| _ -> () )
		| TFun (args,haxe_type) -> visit_type haxe_type;
				List.iter (fun (_,_,t) -> visit_type t; ) args;
		| _ -> ()
	in
	let rec visit_types expression =
		begin
		let rec visit_expression = fun expression ->
			(* Expand out TTypeExpr ... *)
			(match expression.eexpr with
				| TTypeExpr type_def -> add_type (t_path type_def)
				(* Must visit the types, Type.iter will visit the expressions ... *)
				| TTry (e,catches) ->
					List.iter (fun (_,catch_type,_) -> visit_type catch_type) catches
				(* Must visit the enum param types, Type.iter will visit the rest ... *)
				| TMatch (_,_,cases,_) ->
					List.iter (fun (case_ids,params,expression) ->
						(match params with
						| None -> ()
						| Some l -> List.iter (fun (v,t) -> visit_type t) l  ) ) cases;
				(* Must visit type too, Type.iter will visit the expressions ... *)
				| TNew  (klass,params,_) -> visit_type (TInst (klass,params))
				(* Must visit type too, Type.iter will visit the expressions ... *)
				| TVars var_list ->
					List.iter (fun (_, var_type, _) -> visit_type var_type ) var_list
				(* Must visit args too, Type.iter will visit the expressions ... *)
				| TFunction func_def ->
					List.iter (fun (_,_,arg_type) -> visit_type arg_type) func_def.tf_args;
				| _ -> ()
			);
			Type.iter visit_expression expression;
			visit_type (follow expression.etype)
		in
		visit_expression expression
		end
	in
	let visit_field field =
		ignore_function_name := field.cf_name;
		(* Add the type of the expression ... *)
		visit_type field.cf_type;
		if (not header_only) then
			(match field.cf_expr with
			| Some expression -> visit_types expression | _ -> ());
		ignore_function_name := "?"
	in
	let visit_class class_def =
		ignore_class_name := join_class_path class_def.cl_path ".";
		let fields = List.append class_def.cl_ordered_fields class_def.cl_ordered_statics in
		let fields_and_constructor = List.append fields
			(match class_def.cl_constructor with | Some expr -> [expr] | _ -> [] ) in
		List.iter visit_field fields_and_constructor;
		(* Add super & interfaces *)
		add_type class_def.cl_path;
		ignore_class_name := "?"
	in
	let visit_enum enum_def =
		ignore_class_name := join_class_path enum_def.e_path ".";
		add_type enum_def.e_path;
		PMap.iter (fun _ constructor ->
			(match constructor.ef_type with
			| TFun (args,_) ->
				List.iter (fun (_,_,t) -> visit_type t; ) args;
			| _ -> () );
			) enum_def.e_constrs;
		ignore_class_name := "?"
	in
	let inc_cmp i1 i2 =
		String.compare (join_class_path i1 ".") (join_class_path i2 ".")
	in

	(* Body of main function *)
	(match obj with
	| TClassDecl class_def -> visit_class class_def;
		(match class_def.cl_init with Some expression -> visit_types expression | _ -> ())
	| TEnumDecl enum_def -> visit_enum enum_def
	| TTypeDecl _ -> (* These are expanded *) ());
	List.sort inc_cmp (List.filter (fun path -> not (is_internal_header path) ) (pmap_keys !types))
	;;




let generate_main common_ctx member_types super_deps class_def boot_classes init_classes =
	let base_dir = common_ctx.file in
	(* main routine should be a single static function *)
	let main_expression = 
		(match class_def.cl_ordered_statics with
		| [{ cf_expr = Some expression }] -> expression;
		| _ -> assert false ) in
	let referenced = find_referenced_types (TClassDecl class_def) super_deps false in
	let generate_startup filename is_main =
		(*make_class_directories base_dir ( "src" :: []);*)
		let cpp_file = new_cpp_file common_ctx.file ([],filename) in
		let output_main = (cpp_file#write) in
		let ctx = new_context cpp_file false in
		ctx.ctx_class_name <- "?";
		ctx.ctx_class_member_types <- member_types;

		output_main "#include <hxcpp.h>\n\n";
		output_main "#include <stdio.h>\n\n";

		List.iter ( add_include cpp_file ) referenced;
		output_main "\n\n";

		output_main ( if is_main then "HX_BEGIN_MAIN\n\n" else "HX_BEGIN_LIB_MAIN\n\n" );
		gen_expression (new_context cpp_file false) false main_expression;
		output_main ";\n";
		output_main ( if is_main then "HX_END_MAIN\n\n" else "HX_END_LIB_MAIN\n\n" );
		cpp_file#close;
	in
	generate_startup "__main__" true;
	generate_startup "__lib__" false;

	(* Write boot class too ... *)
	let boot_file = new_cpp_file base_dir ([],"__boot__") in
	let output_boot = (boot_file#write) in
	output_boot "#include <hxcpp.h>\n\n";
	List.iter ( fun class_path ->
		output_boot ("#include <" ^
			( join_class_path (include_remap class_path) "/" ) ^ ".h>\n")
			) boot_classes;

	output_boot "\nvoid __boot_all()\n{\n";
	output_boot "hx::RegisterResources( hx::GetResources() );\n";
	List.iter ( fun class_path ->
		output_boot ("::" ^ ( join_class_path class_path "::" ) ^ "_obj::__register();\n") ) boot_classes;
	List.iter ( fun class_path ->
		output_boot ("::" ^ ( join_class_path class_path "::" ) ^ "_obj::__init__();\n") ) (List.rev init_classes);
	List.iter ( fun class_path ->
		output_boot ("::" ^ ( join_class_path class_path "::" ) ^ "_obj::__boot();\n") ) (List.rev boot_classes);
	output_boot "}\n\n";
	boot_file#close;;


let begin_header_file output_h def_string =
	output_h ("#ifndef INCLUDED_" ^ def_string ^ "\n");
	output_h ("#define INCLUDED_" ^ def_string ^ "\n\n");
	output_h "#include <hxcpp.h>\n\n";;

let end_header_file output_h def_string = 
	output_h ("\n#endif /* INCLUDED_" ^ def_string ^ " */ \n");;

let new_placed_cpp_file common_ctx class_path =
	let base_dir = common_ctx.file in
	if (Common.defined common_ctx "vcproj" ) then begin
		make_class_directories base_dir ("src"::[]);
		cached_source_writer
			( base_dir ^ "/src/" ^ ( String.concat "-" (fst class_path) ) ^ "-" ^
			(snd class_path) ^ ".cpp")
	end else
		new_cpp_file common_ctx.file class_path;;



let generate_enum_files common_ctx enum_def super_deps =
	let class_path = enum_def.e_path in
	let class_name = (snd class_path) ^ "_obj" in
	let smart_class_name =  (snd class_path)  in
	(*let cpp_file = new_cpp_file common_ctx.file class_path in*)
	let cpp_file = new_placed_cpp_file common_ctx class_path in
	let output_cpp = (cpp_file#write) in
	let debug = false in
	let ctx = new_context cpp_file debug in

	if (debug) then
		print_endline ("Found enum definition:" ^ (join_class_path  class_path "::" ));

	output_cpp "#include <hxcpp.h>\n\n";

	let referenced = find_referenced_types (TEnumDecl enum_def) super_deps false in
	List.iter (add_include cpp_file) referenced;

	gen_open_namespace output_cpp class_path;
	output_cpp "\n";

	PMap.iter (fun _ constructor ->
		let name = constructor.ef_name in
		match constructor.ef_type with
		| TFun (args,_) -> 
			output_cpp (smart_class_name ^ "  " ^ class_name ^ "::" ^ name ^ "(" ^
				(gen_tfun_arg_list args) ^")\n");
			output_cpp ("	{ return hx::CreateEnum< " ^ class_name ^ " >(" ^ (str name) ^ "," ^
				(string_of_int constructor.ef_index) ^ ",hx::DynamicArray(0," ^
				(string_of_int (List.length args)) ^  ")" );
			List.iter (fun (arg,_,_) -> output_cpp (".Add(" ^ arg ^ ")")) args;
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
						 constr.ef_name ^ ",return)\n\n");
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
	output_cpp ("Dynamic " ^ class_name ^ "::__Field(const ::String &inName)\n{\n");
	let dump_constructor_test _ constr =
		output_cpp ("	if (inName==" ^ (str constr.ef_name) ^ ") return " ^ constr.ef_name );
		if ( (constructor_arg_count constr) > 0 ) then output_cpp "_dyn()";
		output_cpp (";\n")
	in
	PMap.iter dump_constructor_test enum_def.e_constrs;
	output_cpp ("	return super::__Field(inName);\n}\n\n");


	output_cpp "static ::String sStaticFields[] = {\n";
	PMap.iter
		(fun _ constructor -> output_cpp ("	" ^ (str constructor.ef_name) ^ ",\n") )
		enum_def.e_constrs;
	output_cpp "	::String(null()) };\n\n";

	(* ENUM - MARK function - only used with internal GC *)
	output_cpp "static void sMarkStatics() {\n";
	PMap.iter (fun _ constructor ->
		let name = constructor.ef_name in
		match constructor.ef_type with
		| TFun (_,_) -> ()
		| _ -> output_cpp ("	hx::MarkMember(" ^ class_name ^ "::" ^ name ^ ");\n") )
	enum_def.e_constrs;
	output_cpp "};\n\n";


	output_cpp "static ::String sMemberFields[] = { ::String(null()) };\n";

	output_cpp ("Class " ^ class_name ^ "::__mClass;\n\n");

	output_cpp ("Dynamic __Create_" ^ class_name ^ "() { return new " ^ class_name ^ "; }\n\n");

	output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
	let text_name = str (join_class_path class_path ".") in
	output_cpp ("\nStatic(__mClass) = hx::RegisterClass(" ^ text_name ^
					", hx::TCanCast< " ^ class_name ^ " >,sStaticFields,sMemberFields,\n");
	output_cpp ("	&__Create_" ^ class_name ^ ", &__Create,\n");
	output_cpp ("	&super::__SGetClass(), &Create" ^ class_name ^ ", sMarkStatics);\n");
	output_cpp ("}\n\n");

	output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
	PMap.iter (fun _ constructor ->
		let name = constructor.ef_name in
		match constructor.ef_type with
		| TFun (_,_) -> ()
		| _ ->
			output_cpp ( "Static(" ^ name ^ ") = hx::CreateEnum< " ^ class_name ^ " >(" ^ (str name) ^  "," ^
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
									(str (smart_class_name ^ ".") )^ " + tag; }\n\n");

	PMap.iter (fun _ constructor ->
		let name = constructor.ef_name in
		output_h ( "		static " ^  smart_class_name ^ " " ^ name );
		match constructor.ef_type with
		| TFun (args,_) -> 
			output_h ( "(" ^ (gen_tfun_arg_list args) ^");\n");
			output_h ( "		static Dynamic " ^ name ^ "_dyn();\n");
		| _ ->
			output_h ";\n"
	) enum_def.e_constrs;

	output_h "};\n\n";

	gen_close_namespace output_h class_path;

	end_header_file output_h def_string;
	h_file#close;
	referenced;;

let has_init_field class_def =
	match class_def.cl_init with
	| Some _ -> true
	| _ -> false;;


let generate_class_files common_ctx member_types super_deps class_def =
	let is_extern = class_def.cl_extern in
	let class_path = class_def.cl_path in
	let class_name = (snd class_def.cl_path) ^ "_obj" in
	let smart_class_name =  (snd class_def.cl_path)  in
	(*let cpp_file = new_cpp_file common_ctx.file class_path in*)
	let cpp_file = new_placed_cpp_file common_ctx class_path in
	let output_cpp = (cpp_file#write) in
	let debug = false in
	let ctx = new_context cpp_file debug in
	ctx.ctx_class_name <- "::" ^ (join_class_path class_path "::");
	ctx.ctx_class_member_types <- member_types;
	if debug then print_endline ("Found class definition:" ^ ctx.ctx_class_name);

	let ptr_name = "hx::ObjectPtr< " ^ class_name ^ " >" in
	let constructor_type_var_list =
		match class_def.cl_constructor with
		| Some definition ->
				(match  definition.cf_type with
					| TFun (args,_) -> List.map (fun (a,_,t) -> (type_string t,a) )  args
					| _ ->  (match definition.cf_expr with
						| Some { eexpr = TFunction function_def } ->
							List.map (fun (name,o,arg_type) -> gen_arg_type_name name o arg_type "__o_")
									function_def.tf_args;
						| _ -> [] )
					)
		| _ -> [] in
	let constructor_var_list = List.map snd constructor_type_var_list in
	let constructor_type_args = String.concat ","
				(List.map (fun (t,a) -> t ^ " " ^ a) constructor_type_var_list) in
	let constructor_args = String.concat "," constructor_var_list in

	let implement_dynamic = implement_dynamic_here class_def in

	output_cpp "#include <hxcpp.h>\n\n";

	let field_integer_dynamic = has_field_integer_lookup class_def in
	let field_integer_numeric = has_field_integer_numeric_lookup class_def in

	let all_referenced = find_referenced_types (TClassDecl class_def) super_deps false in
	List.iter ( add_include cpp_file  ) all_referenced;

	(* All interfaces (and sub-interfaces) implemented *)
	let implemented_hash = Hashtbl.create 0 in
	List.iter (fun imp ->
		let rec descend_interface interface =
			let imp_path = (fst interface).cl_path in
			let interface_name = "::" ^ (join_class_path imp_path "::" ) in
			if ( not (Hashtbl.mem implemented_hash interface_name) ) then begin
				Hashtbl.add implemented_hash interface_name ();
				match (fst interface).cl_super with | Some super -> descend_interface super | _->();
			end
		in descend_interface imp
	) (real_interfaces class_def.cl_implements);
	let implemented = hash_keys implemented_hash in

	gen_open_namespace output_cpp class_path;
	output_cpp "\n";

	if (not class_def.cl_interface) then begin
		if (not is_extern) then begin
			output_cpp ("Void " ^ class_name ^ "::__construct(" ^ constructor_type_args ^ ")\n{\n");
			(match class_def.cl_constructor with
				| Some definition ->
						(match  definition.cf_expr with
						| Some { eexpr = TFunction function_def } ->
							if (has_default_values function_def.tf_args) then begin
								generate_default_values ctx function_def.tf_args "__o_";
								gen_expression ctx false function_def.tf_expr;
								output_cpp ";\n";
							end else begin
								gen_expression ctx false function_def.tf_expr;
								output_cpp ";\n";
								(*gen_expression (new_context cpp_file debug ) false function_def.tf_expr;*)
							end
						| _ -> ()
						)
				| _ -> ());
			output_cpp "	return null();\n";
			output_cpp "}\n\n";
		end;

		(* Destructor goes in the cpp file so we can "see" the full definition of the member vars *)
		output_cpp ( class_name ^ "::~" ^ class_name ^ "() { }\n\n");
		if (not is_extern) then
			output_cpp ("Dynamic " ^ class_name ^ "::__CreateEmpty() { return  new " ^ class_name ^ "; }\n");

		output_cpp (ptr_name ^ " " ^ class_name ^ "::__new(" ^constructor_type_args ^")\n");

		let create_result ext = 
			if (ext) then
				output_cpp ("{  " ^ ptr_name ^ " result = __CreateEmpty();\n")
			else
				output_cpp ("{  " ^ ptr_name ^ " result = new " ^ class_name ^ "();\n");
			in
		create_result is_extern;
		output_cpp ("	result->__construct(" ^ constructor_args ^ ");\n");
		output_cpp ("	return result;}\n\n");

		output_cpp ("Dynamic " ^ class_name ^ "::__Create(hx::DynamicArray inArgs)\n");
		create_result is_extern;
		output_cpp ("	result->__construct(" ^ (array_arg_list constructor_var_list) ^ ");\n");
		output_cpp ("	return result;}\n\n");

	end;

	(match class_def.cl_init with
	| Some expression -> 
		output_cpp ("void " ^ class_name^ "::__init__()");
		gen_expression (new_context cpp_file debug) false expression;
		output_cpp "\n\n";
	| _ -> ());


	if ( (List.length implemented) > 0 ) then begin
		output_cpp ("hx::Object *" ^ class_name ^ "::__ToInterface(const type_info &inType) {\n");
		List.iter (fun interface_name ->
			output_cpp ("	if (inType==typeid( " ^ interface_name ^ "_obj)) " ^
				"return operator " ^ interface_name ^ "_obj *();\n");
		) implemented;
		output_cpp ("	return super::__ToInterface(inType);\n}\n\n");
	end;

	List.iter
		(gen_field ctx class_name smart_class_name false is_extern class_def.cl_interface)
		class_def.cl_ordered_fields;
	List.iter
		(gen_field ctx class_name smart_class_name true is_extern class_def.cl_interface)
		class_def.cl_ordered_statics;
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
						if (is_dynamic_method field) then
							output_cpp ("	" ^ remap_name ^ " = new __default_" ^ remap_name ^ "(this);\n")
				| _ -> ()
			)
			class_def.cl_ordered_fields;
		output_cpp "}\n\n";

                (* MARK function - only used with internal GC *)
		output_cpp ("void " ^ class_name ^ "::__Mark()\n{\n");
		if (implement_dynamic) then
			output_cpp "	HX_MARK_DYNAMIC;\n";
		List.iter
			(fun field -> let remap_name = keyword_remap field.cf_name in
				if (is_data_member field) then
				   output_cpp ("	hx::MarkMember(" ^ remap_name ^ ");\n")
			)
			class_def.cl_ordered_fields;
		(match  class_def.cl_super with Some _ -> output_cpp "	super::__Mark();\n" | _ -> () );
		output_cpp "}\n\n";



		let variable_field field =
			(match field.cf_expr with
			| Some { eexpr = TFunction function_def } -> is_dynamic_method field
			| _ -> (not is_extern) ||
				(match follow field.cf_type with | TFun _ -> false | _ -> true) ) in

      let all_fields = class_def.cl_ordered_statics @ class_def.cl_ordered_fields in
		let all_variables = List.filter variable_field all_fields in

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
					let strl = ref 0 in
					let text = quote field strl in
					output_cpp ("		if (!memcmp(inName.__s," ^ text ^
					     ",sizeof(wchar_t)*" ^ (string_of_int !strl) ^ ") ) { " ^ result ^ " }\n");
				) sfields;
				output_cpp "	}\n";
			end;
		in


		(* Dynamic "Get" Field function - string version *)
		output_cpp ("Dynamic " ^ class_name ^ "::__Field(const ::String &inName)\n{\n");
		let get_field_dat = List.map (fun f ->
			(f.cf_name, String.length f.cf_name, "return " ^
				(match f.cf_get with
				| CallAccess prop -> (keyword_remap prop) ^ "()"
				| _ -> ((keyword_remap f.cf_name) ^ if (variable_field f) then "" else "_dyn()")
				) ^ ";"
			) )
		in
		dump_quick_field_test (get_field_dat all_fields);
		if (implement_dynamic) then
			output_cpp "	HX_CHECK_DYNAMIC_GET_FIELD(inName);\n";
		output_cpp ("	return super::__Field(inName);\n}\n\n");


		(* Dynamic "Get" Field function - int version *)
		if ( field_integer_numeric || field_integer_dynamic) then begin
			let dump_static_ids = (fun field ->
				let remap_name = keyword_remap field.cf_name in
				output_cpp ("static int __id_" ^ remap_name ^ " = __hxcpp_field_to_id(\"" ^
								  	(field.cf_name) ^ "\");\n");
				) in
			List.iter dump_static_ids all_fields;
			output_cpp "\n\n";


			let output_ifield return_type function_name =
			output_cpp (return_type ^" " ^ class_name ^ "::" ^ function_name ^ "(int inFieldID)\n{\n");
			let dump_field_test = (fun f ->
				let remap_name = keyword_remap f.cf_name in
				output_cpp ("	if (inFieldID==__id_" ^ remap_name ^ ") return "  ^
					( if (return_type="double") then "hx::ToDouble( " else "" ) ^
					(match f.cf_get with
					| CallAccess prop -> (keyword_remap prop) ^ "()"
					| _ -> ((keyword_remap f.cf_name) ^ if ( variable_field f) then "" else "_dyn()")
					) ^ ( if (return_type="double") then " ) " else "" ) ^ ";\n");
				) in
			List.iter dump_field_test all_fields;
			if (implement_dynamic) then
				output_cpp "	HX_CHECK_DYNAMIC_GET_INT_FIELD(inFieldID);\n";
			output_cpp ("	return super::" ^ function_name ^ "(inFieldID);\n}\n\n");
			in

			if (field_integer_dynamic) then output_ifield "Dynamic" "__IField";
			if (field_integer_numeric) then output_ifield "double" "__INumField";
		end;


		(* Dynamic "Set" Field function *)
		output_cpp ("Dynamic " ^ class_name ^ "::__SetField(const ::String &inName," ^
						"const Dynamic &inValue)\n{\n");

		let set_field_dat = List.map (fun f ->
			(f.cf_name, String.length f.cf_name,
				(match f.cf_set with
				| CallAccess prop -> "return " ^ (keyword_remap prop) ^ "(inValue);"
				| _ -> (keyword_remap f.cf_name) ^ "=inValue.Cast< " ^ (type_string f.cf_type) ^
				         " >(); return inValue;"
				)  )
		) in

		dump_quick_field_test (set_field_dat all_variables);
		if (implement_dynamic) then begin
			output_cpp ("	try { return super::__SetField(inName,inValue); }\n");
			output_cpp ("	catch(Dynamic e) { HX_DYNAMIC_SET_FIELD(inName,inValue); }\n");
			output_cpp "	return inValue;\n}\n\n";
		end else
			output_cpp ("	return super::__SetField(inName,inValue);\n}\n\n");

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
		List.iter dump_field_name  class_def.cl_ordered_statics;
		output_cpp "	String(null()) };\n\n";

		output_cpp "static ::String sMemberFields[] = {\n";
		List.iter dump_field_name  class_def.cl_ordered_fields;
		output_cpp "	String(null()) };\n\n";

		(* MARK function - only used with internal GC *)
		output_cpp "static void sMarkStatics() {\n";
		List.iter (fun field ->
			if (is_data_member field) then
				output_cpp ("	hx::MarkMember(" ^ class_name ^ "::" ^ (keyword_remap field.cf_name) ^ ");\n") )
			class_def.cl_ordered_statics;
		output_cpp "};\n\n";

	end;



	(* Initialise static in boot function ... *)
	if (not class_def.cl_interface) then begin
		(* Remap the specialised "extern" classes back to the generic names *)
		let class_name_text = match class_path with
			| ["cpp"], "CppDate__" -> "Date"
			| ["cpp"], "CppXml__" -> "Xml"
			| path -> join_class_path path "." in

		output_cpp ("Class " ^ class_name ^ "::__mClass;\n\n");

		output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
		output_cpp ("	Static(__mClass) = hx::RegisterClass(" ^ (str class_name_text)  ^
				", hx::TCanCast< " ^ class_name ^ "> ,sStaticFields,sMemberFields,\n");
		output_cpp ("	&__CreateEmpty, &__Create,\n");
		output_cpp ("	&super::__SGetClass(), 0, sMarkStatics);\n");
		output_cpp ("}\n\n");

		if (not is_extern) then begin
			output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
			List.iter (gen_field_init ctx ) class_def.cl_ordered_statics;
			output_cpp ("}\n\n");
		end;
	end;

	gen_close_namespace output_cpp class_path;

	if (is_extern) then begin
		output_cpp ("\n\n#include<extern/" ^ (join_class_path class_path  "/") ^ ".cpp>\n\n");
	end;
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
	let referenced = find_referenced_types (TClassDecl class_def) super_deps true in
	List.iter ( gen_forward_decl h_file ) referenced;

	gen_open_namespace output_h class_path;
	output_h "\n\n";

	output_h ("class " ^ class_name ^ " : public " ^ super );
	output_h "{\n	public:\n";
	output_h ("		typedef " ^ super ^ " super;\n");
	output_h ("		typedef " ^ class_name ^ " OBJ_;\n");

	if (not class_def.cl_interface) then begin
		output_h ("		" ^ class_name ^  "();\n");
		if (is_extern) then
			output_h ("		virtual Void __construct(" ^ constructor_type_args ^ ")=0;\n")
		else
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
		output_h ("		void __Mark();\n");

		List.iter (fun interface_name ->
			output_h ("		inline operator " ^ interface_name ^ "_obj *()\n			" ^
					"{ return new " ^ interface_name ^ "_delegate_< " ^ class_name ^" >(this); }\n" );
		) implemented;

		if ( (List.length implemented) > 0 ) then
			output_h "		hx::Object *__ToInterface(const type_info &inType);\n";

		if (has_init_field class_def) then
			output_h "		static void __init__();\n\n";
		output_h ("		::String __ToString() const { return " ^ (str smart_class_name) ^ "; }\n\n");
	end;


	(match class_def.cl_array_access with
	| Some t -> output_h ("		typedef " ^ (type_string t) ^ " __array_access;\n")
	| _ -> ());


	let interface = class_def.cl_interface in
	List.iter (gen_member_def ctx false is_extern interface) class_def.cl_ordered_fields;
	List.iter (gen_member_def ctx true is_extern interface)  class_def.cl_ordered_statics;

	output_h "};\n\n";

	if (class_def.cl_interface) then begin
		output_h ("#define DELEGATE_" ^ (join_class_path  class_def.cl_path "_" ) ^ " \\\n");
		List.iter (fun field ->
		match follow field.cf_type with
		| TFun (args,return_type) ->
			(* TODO : virtual ? *)
			let remap_name = keyword_remap field.cf_name in
			output_h ( "virtual "  ^ (type_string return_type) ^ " " ^ remap_name ^ "( " );
			output_h (String.concat "," (List.map (fun (name,opt,typ) ->
				(type_string typ) ^ " " ^ name ^ (if opt then "=null()" else "")) args));
			output_h (") { return mDelegate->" ^ remap_name^ "(");
			output_h (String.concat "," (List.map (fun (name,opt,typ) -> name) args));
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
	all_referenced;;


let gen_deps deps =
	let project_deps = List.filter (fun path -> not (is_internal_class path) ) deps in
	String.concat " " (List.map (fun class_path ->
		"include/" ^ (join_class_path class_path "/") ^ ".h") project_deps );;

let add_class_to_makefile makefile add_obj class_def =
	let class_path = fst class_def in
	let deps = snd class_def in
	let obj_file =  "obj/" ^ (join_class_path class_path "-") ^ "$(OBJ)" in
	let cpp = (join_class_path class_path "/") ^ ".cpp" in
	output_string makefile ( obj_file ^ " : src/" ^ cpp ^ " " ^ (gen_deps deps) ^ "\n");
	output_string makefile ("\t$(COMPILE) src/" ^ cpp ^ " $(OUT_FLAGS)$@\n\n");
	output_string makefile (add_obj ^ " " ^ obj_file ^ "\n\n" );;
	

let kind_string = function
	| KNormal -> "KNormal"
	| KTypeParameter -> "KTypeParameter"
	| KExtension _ -> "KExtension"
	| KConstant _ -> "KConstant"
	| KGeneric -> "KGeneric"
	| KGenericInstance _ -> "KGenericInstance";;


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


let add_class_to_buildfile buildfile class_def =
	let class_path = fst class_def in
	let deps = snd class_def in
	let cpp = (join_class_path class_path "/") ^ ".cpp" in
	output_string buildfile ( "  <file name=\"src/" ^ cpp ^ "\">\n" );

	let project_deps = List.filter (fun path -> not (is_internal_class path) ) deps in
	List.iter (fun path-> output_string buildfile ("   <depend name=\"" ^
		"include/" ^ (join_class_path path "/") ^ ".h\"/>\n") ) project_deps;

	output_string buildfile ( "  </file>\n" );;


let write_build_data filename classes main_deps exe_name =
	let buildfile = open_out filename in
	output_string buildfile "<xml>\n";
	output_string buildfile "<files id=\"haxe\">\n";
	output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
	List.iter (add_class_to_buildfile buildfile) classes;
	add_class_to_buildfile buildfile  (  ( [] , "__boot__") , [] );
	add_class_to_buildfile buildfile  (  ( [] , "__resources__") , [] );
	output_string buildfile "</files>\n";
	output_string buildfile "<files id=\"__lib__\">\n";
	output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
	add_class_to_buildfile buildfile  (  ( [] , "__lib__") , main_deps );
	output_string buildfile "</files>\n";
	output_string buildfile "<files id=\"__main__\">\n";
	output_string buildfile "<compilerflag value=\"-Iinclude\"/>\n";
	add_class_to_buildfile buildfile  (  ( [] , "__main__") , main_deps );
	output_string buildfile "</files>\n";
	output_string buildfile ("<set name=\"HAXE_OUTPUT\" value=\"" ^ exe_name ^ "\" />\n");
	output_string buildfile "<include name=\"${HXCPP}/build-tool/BuildCommon.xml\"/>\n";
	output_string buildfile "</xml>\n";
	close_out buildfile;;

let write_build_options filename options =
	let writer = cached_source_writer filename in
	PMap.iter ( fun name _ -> if (name <> "debug") then writer#write ( name ^ "\n") ) options;
	let cmd = Unix.open_process_in "haxelib path hxcpp" in
	writer#write (Pervasives.input_line cmd);
	Pervasives.ignore (Unix.close_process_in cmd);
	writer#close;;

let create_member_types common_ctx = 
	let result = Hashtbl.create 0 in
	let add_member class_name member =
		match follow member.cf_type with
		| TFun (_,ret) ->
			(* print_endline (((join_class_path class_path "::") ^ "." ^ member.cf_name) ^ "=" ^
						(type_string ret)); *)
			Hashtbl.add result (class_name ^ "." ^ member.cf_name) (type_string ret)
		| _ ->
			Hashtbl.add result (class_name ^ "." ^ member.cf_name) (type_string member.cf_type)
		in
	List.iter (fun object_def ->
		(match object_def with
		| TClassDecl class_def when (match class_def.cl_kind with | KGeneric -> false | _ ->true) ->
			let class_name = "::" ^ (join_class_path class_def.cl_path "::") in
			let rec add_all_fields class_def =
				(match  class_def.cl_super with Some super -> add_all_fields (fst super) | _->(););
				List.iter (add_member class_name) class_def.cl_ordered_fields;
				List.iter (add_member class_name) class_def.cl_ordered_statics
			in
			add_all_fields class_def
		| _ -> ()
		) ) common_ctx.types;
	result;;

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


(* The common_ctx contains the haxe AST in the "types" field and the resources *)
let generate common_ctx =
	make_base_directory common_ctx.file;

	let debug = false in
	let exe_classes = ref [] in
	let boot_classes = ref [] in
	let init_classes = ref [] in
	let class_text path = join_class_path path "::" in
	let member_types = create_member_types common_ctx in
	let super_deps = create_super_dependencies common_ctx in
	let main_deps = ref [] in

	List.iter (fun object_def ->
		(match object_def with
		| TClassDecl class_def when (match class_def.cl_kind with | KGeneric -> true | _ ->false) ->
			let name =  class_text class_def.cl_path in
			(if debug then print_endline ("	ignore generic class " ^ name))
		| TClassDecl class_def ->
			(match class_def.cl_path with
			| [], "@Main" ->
				main_deps := find_referenced_types (TClassDecl class_def) super_deps false;
				generate_main common_ctx member_types super_deps class_def !boot_classes !init_classes;
			| _ ->
				let name =  class_text class_def.cl_path in
				let is_internal = is_internal_class class_def.cl_path in
				if (is_internal) then
					( if debug then print_endline (" internal class " ^ name ))
				else begin
					if (not class_def.cl_interface) then
						boot_classes := class_def.cl_path ::  !boot_classes;
					if (has_init_field class_def) then
						init_classes := class_def.cl_path ::  !init_classes;
					let deps = generate_class_files common_ctx member_types super_deps class_def in
					exe_classes := (class_def.cl_path, deps)  ::  !exe_classes;
				end
			)
		| TEnumDecl enum_def ->
			let name =  class_text enum_def.e_path in
			let is_internal = is_internal_class enum_def.e_path in
			if (is_internal) then
				(if debug then print_endline (" internal enum " ^ name ))
			else begin
				if (enum_def.e_extern) then
					(if debug then print_endline ("external enum " ^ name ));
				boot_classes := enum_def.e_path :: !boot_classes;
				let deps = generate_enum_files common_ctx enum_def super_deps in
				exe_classes := (enum_def.e_path, deps) :: !exe_classes;
			end
		| TTypeDecl _ -> (* already done *) ()
		);
	) common_ctx.types;

	write_resources common_ctx;


	let output_name = match  common_ctx.main_class with
	| Some path -> (snd path)
	| _ -> "output" in

	write_build_data (common_ctx.file ^ "/Build.xml") !exe_classes !main_deps output_name;
	write_build_options (common_ctx.file ^ "/Options.txt") common_ctx.defines;
	if ( not (Common.defined common_ctx "no-compilation") ) then begin
		let old_dir = Sys.getcwd() in
		Sys.chdir common_ctx.file;
		let cmd = ref "haxelib run hxcpp Build.xml haxe" in
		if (common_ctx.debug) then cmd := !cmd ^ " -Ddebug";
		PMap.iter ( fun name _ -> cmd := !cmd ^ " -D" ^ name ^ "" ) common_ctx.defines;
		print_endline !cmd;
		if Sys.command !cmd <> 0 then failwith "Build failed";
		Sys.chdir old_dir;
	end
	;;



