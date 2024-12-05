open Globals
open Ast
open Type
open Common
open Error

let display_identifier_error com ?prepend_msg msg p =
	let prepend = match prepend_msg with Some s -> s ^ " " | _ -> "" in
	Common.display_error com (prepend ^ msg) p

let check_identifier_name ?prepend_msg com name kind p =
	if starts_with name '$' then
		display_identifier_error com ?prepend_msg ((StringHelper.capitalize kind) ^ " names starting with a dollar are not allowed: \"" ^ name ^ "\"") p
	else if not (Lexer.is_valid_identifier name) then
		display_identifier_error com ?prepend_msg ("\"" ^ (StringHelper.s_escape name) ^ "\" is not a valid " ^ kind ^ " name.") p

let check_field_name com name p =
	match name with
	| "new" -> () (* the only keyword allowed in field names *)
	| _ -> check_identifier_name com name "field" p

let check_uppercase_identifier_name ?prepend_msg com name kind p =
	if String.length name = 0 then
		display_identifier_error ?prepend_msg com ((StringHelper.capitalize kind) ^ " name must not be empty.") p
	else if Ast.is_lower_ident name then
		display_identifier_error ?prepend_msg com ((StringHelper.capitalize kind) ^ " name should start with an uppercase letter: \"" ^ name ^ "\"") p
	else
		check_identifier_name ?prepend_msg com name kind p

let check_module_path com (pack,name) p =
	let full_path = StringHelper.s_escape (if pack = [] then name else (String.concat "." pack) ^ "." ^ name) in
	check_uppercase_identifier_name ~prepend_msg:("Module \"" ^ full_path ^ "\" does not have a valid name.") com name "module" p;
	try
		List.iter (fun part -> Path.check_package_name part) pack;
	with Failure msg ->
		display_error_ext com (make_error
			~sub:[make_error (Custom msg) p]
			(Custom ("\"" ^ (StringHelper.s_escape (String.concat "." pack)) ^ "\" is not a valid package name:"))
			p
		)

let check_local_variable_name com name origin p =
	match name with
	| "this" -> () (* TODO: vars named `this` should technically be VGenerated, not VUser *)
	| _ ->
		let s_var_origin origin =
			match origin with
			| TVOLocalVariable -> "variable"
			| TVOArgument -> "function argument"
			| TVOForVariable -> "for variable"
			| TVOPatternVariable -> "pattern variable"
			| TVOCatchVariable -> "catch variable"
			| TVOLocalFunction -> "function"
		in
		check_identifier_name com name (s_var_origin origin) p