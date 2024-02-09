open Globals
open Ast
open Type
open Common
open Error

(** retrieve string from @:native metadata or raise Not_found *)
let get_native_name meta =
	let rec get_native meta = match meta with
		| [] -> raise Not_found
		| (Meta.Native,[v],p as meta) :: _ ->
			meta
		| _ :: meta ->
			get_native meta
	in
	let (_,e,mp) = get_native meta in
	match e with
	| [Ast.EConst (Ast.String(name,_)),p] ->
		name,p
	| [] ->
		raise Not_found
	| _ ->
		Error.raise_typing_error "String expected" mp

(* Rewrites class or enum paths if @:native metadata is set *)
let apply_native_paths t =
	let get_real_name meta name =
		let name',p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (name,SDoubleQuotes)), p], p), name'
	in
	let get_real_path meta path =
		let name,p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (s_type_path path,SDoubleQuotes)), p], p), parse_path name
	in
	try
		(match t with
		| TClassDecl c ->
			let did_change = ref false in
			let field cf = try
				let meta,name = get_real_name cf.cf_meta cf.cf_name in
				cf.cf_name <- name;
				cf.cf_meta <- meta :: cf.cf_meta;
				List.iter (fun cf -> cf.cf_name <- name) cf.cf_overloads;
				did_change := true
			with Not_found ->
				()
			in
			let fields cfs old_map =
				did_change := false;
				List.iter field cfs;
				if !did_change then
					List.fold_left (fun map f -> PMap.add f.cf_name f map) PMap.empty cfs
				else
					old_map
			in
			c.cl_fields <- fields c.cl_ordered_fields c.cl_fields;
			c.cl_statics <- fields c.cl_ordered_statics c.cl_statics;
			let meta,path = get_real_path c.cl_meta c.cl_path in
			c.cl_meta <- meta :: c.cl_meta;
			c.cl_path <- path;
		| TEnumDecl e ->
			let did_change = ref false in
			let field _ ef = try
				let meta,name = get_real_name ef.ef_meta ef.ef_name in
				ef.ef_name <- name;
				ef.ef_meta <- meta :: ef.ef_meta;
				did_change := true;
			with Not_found ->
				()
			in
			PMap.iter field e.e_constrs;
			if !did_change then begin
				let names = ref [] in
				e.e_constrs <- PMap.fold
					(fun ef map ->
						names := ef.ef_name :: !names;
						PMap.add ef.ef_name ef map
					)
					e.e_constrs PMap.empty;
				e.e_names <- !names;
			end;
			let meta,path = get_real_path e.e_meta e.e_path in
			e.e_meta <- meta :: e.e_meta;
			e.e_path <- path;
		| _ ->
			())
	with Not_found ->
		()


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