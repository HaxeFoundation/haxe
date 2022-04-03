open Json

exception Prebuild_error of string

let as_string = function
	| JString s -> Some s
	| _ -> None

let as_int = function
	| JInt i -> Some i
	| _ -> None

let as_params = function
	| JArray s -> Some (List.map (function
			| JString s -> s
			| _ -> raise (Prebuild_error "parameter description should be a string")
		) s)
	| _ -> None

let as_platforms = function
	| JArray s -> Some (List.map (function
			| JString "cross" -> "Cross"
			| JString "js" -> "Js"
			| JString "lua" -> "Lua"
			| JString "neko" -> "Neko"
			| JString "flash" -> "Flash"
			| JString "php" -> "Php"
			| JString "cpp" -> "Cpp"
			| JString "cs" -> "Cs"
			| JString "java" -> "Java"
			| JString "python" -> "Python"
			| JString "hl" -> "Hl"
			| JString "eval" -> "Eval"
			| _ -> raise (Prebuild_error "invalid platform")
		) s)
	| _ -> None

let as_targets = function
	| JArray s -> Some (List.map (function
			| JString "TClass" -> "TClass"
			| JString "TClassField" -> "TClassField"
			| JString "TAbstract" -> "TAbstract"
			| JString "TAbstractField" -> "TAbstractField"
			| JString "TEnum" -> "TEnum"
			| JString "TTypedef" -> "TTypedef"
			| JString "TAnyField" -> "TAnyField"
			| JString "TExpr" -> "TExpr"
			| JString "TTypeParameter" -> "TTypeParameter"
			| _ -> raise (Prebuild_error "invalid metadata target")
		) s)
	| _ -> None

let as_bool = function
	| JBool b -> Some b
	| _ -> None

let as_links = function
	| JArray s -> Some (List.map (function
			| JString s -> s
			| _ -> raise (Prebuild_error "link should be a string")
		) s)
	| _ -> None

let get_optional_field name map default fields =
	try
		let field = List.find (fun (n, _) -> n = name) fields in
		let value = map (snd field) in
		match value with
		| None -> raise (Prebuild_error ("field `" ^ name ^ "` has invalid data"))
		| Some v -> v
	with Not_found -> default

let get_field name map fields =
	let field = try List.find (fun (n, _) -> n = name) fields with Not_found -> raise (Prebuild_error ("no `" ^ name ^ "` field")) in
	let value = map (snd field) in
	match value with
	| None -> raise (Prebuild_error ("field `" ^ name ^ "` has invalid data"))
	| Some v -> v

let parse_define json =
	let fields = match json with
		| JObject fl -> fl
		| _ -> raise (Prebuild_error "not an object")
	in
	(* name *) get_field "name" as_string fields,
	(* define *) get_field "define" as_string fields,
	(* doc *) get_field "doc" as_string fields,
	(* params *) get_optional_field "params" as_params [] fields,
	(* platforms *) get_optional_field "platforms" as_platforms [] fields,
	(* links *) get_optional_field "links" as_links [] fields

let parse_meta json =
	let fields = match json with
		| JObject fl -> fl
		| _ -> raise (Prebuild_error "not an object")
	in
	(* name *) get_field "name" as_string fields,
	(* metadata *) get_field "metadata" as_string fields,
	(* doc *) get_field "doc" as_string fields,
	(* params *) get_optional_field "params" as_params [] fields,
	(* platforms *) get_optional_field "platforms" as_platforms [] fields,
	(* targets *) get_optional_field "targets" as_targets [] fields,
	(* internal *) get_optional_field "internal" as_bool false fields,
	(* links *) get_optional_field "links" as_links [] fields

let parse_warning json =
	let fields = match json with
		| JObject fl -> fl
		| _ -> raise (Prebuild_error "not an object")
	in
	(* name *) get_field "name" as_string fields,
	(* code *) get_field "code" as_int fields,
	(* doc *)  get_field "doc" as_string fields

let parse_file_array path map =
	let file = open_in path in
	let data = Std.input_all file in
	let open Json.Reader in
	let lexbuf = Sedlexing.Utf8.from_string data in
	let json = read_json lexbuf in
	match json with
	| JArray s -> List.map map s
	| _ -> raise (Prebuild_error "not an array")

let s_escape ?(hex=true) s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| c when int_of_char c < 32 && hex -> Buffer.add_string b (Printf.sprintf "\\x%.2X" (int_of_char c))
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let gen_platforms = function
	| [] -> []
	| platforms -> ["Platforms [" ^ (String.concat ";" platforms) ^ "]"]

let gen_params = List.map (function param -> "HasParam \"" ^ param ^ "\"" )

let gen_links = List.map (function link -> "Link \"" ^ link ^ "\"" )

let gen_define_type defines =
	String.concat "\n" (List.map (function (name, _, _, _, _, _) -> "\t| " ^ name) defines)

let gen_define_info defines =
	let define_str = List.map (function
		(name, define, doc, params, platforms, links) ->
			let platforms_str = gen_platforms platforms in
			let params_str = gen_params params in
			let links_str = gen_links links in
			let define = String.concat "_" (ExtString.String.nsplit define "-") in
			"\t| " ^ name ^ " -> \"" ^ define ^ "\",(" ^ (Printf.sprintf "%S" doc) ^ ",[" ^ (String.concat "; " (platforms_str @ params_str @ links_str)) ^ "])"
	) defines in
	String.concat "\n" define_str

let gen_meta_type metas =
	String.concat "\n" (List.map (function
		| ("InlineConstructorArgument", _, _, _, _, _, _, _) -> "\t| InlineConstructorArgument of int * int"
		| (name, _, _, _, _, _, _, _) -> "\t| " ^ name
	) metas)

let gen_meta_info metas =
	let meta_str = List.map (function
		(name, metadata, doc, params, platforms, targets, internal, links) ->
			let platforms_str = gen_platforms platforms in
			let params_str = gen_params params in
			let targets_str = (match targets with
				| [] -> []
				| targets -> ["UsedOn [" ^ (String.concat ";" targets) ^ "]"]
			) in
			let internal_str = if internal then ["UsedInternally"] else [] in
			let links_str = gen_links links in
			let name = (match name with
				(* this is a hacky, I know *)
				| "InlineConstructorArgument" -> "InlineConstructorArgument _"
				| _ -> name
			) in
			"\t| " ^ name ^ " -> \"" ^ metadata ^ "\",(" ^ (Printf.sprintf "%S" doc) ^ ",[" ^ (String.concat "; " (platforms_str @ params_str @ targets_str @ internal_str @ links_str)) ^ "])"
	) metas in
	String.concat "\n" meta_str

let gen_warning_type warnings =
	let warning_str = List.map (function
		(name,code,doc) ->
			Printf.sprintf "\t| %s" name
	) warnings in
	String.concat "\n" warning_str

let gen_warning_ids warnings =
	let seen = Hashtbl.create 0 in
	let warning_str = List.map (function
		(name,code,doc) ->
			try
				let prev = Hashtbl.find seen code in
				failwith (Printf.sprintf "Duplicate warning code %i: Used for %s and %s" code prev name)
			with Not_found ->
				Hashtbl.add seen code name;
				Printf.sprintf "\t| %s -> %i" name code
	) warnings in
	String.concat "\n" warning_str

let gen_warning_doc warnings =
	let warning_str = List.map (function
		(name,code,doc) ->
			Printf.sprintf "\t| %s -> \"%s\"" name (s_escape doc)
	) warnings in
	String.concat "\n" warning_str

let autogen_header = "(* This file is auto-generated using prebuild from files in src-json *)
(* Do not edit manually! *)
"

let define_header = autogen_header ^ "
open Globals

type define_parameter =
	| HasParam of string
	| Platforms of platform list
	| Link of string

"

let meta_header = autogen_header ^ "
open Globals

type meta_usage =
	| TClass
	| TClassField
	| TAbstract
	| TAbstractField
	| TEnum
	| TTypedef
	| TAnyField
	| TExpr
	| TTypeParameter
	| TVariable

type meta_parameter =
	| HasParam of string
	| Platforms of platform list
	| UsedOn of meta_usage list
	| UsedInternally
	| Link of string

"

;;

match Array.to_list (Sys.argv) with
	| [_; "define"; define_path]->
		let defines = parse_file_array define_path parse_define in
		Printf.printf "%s" define_header;
		Printf.printf "type strict_defined =\n";
		Printf.printf "%s" (gen_define_type defines);
		Printf.printf "\n\t| Last\n\n"; (* must be last *)
		Printf.printf "let infos = function\n";
		Printf.printf "%s" (gen_define_info defines);
		Printf.printf "\n\t| Last -> die \"\" __LOC__\n"
	| [_; "meta"; meta_path]->
		let metas = parse_file_array meta_path parse_meta in
		Printf.printf "%s" meta_header;
		Printf.printf "type strict_meta =\n";
		Printf.printf "%s" (gen_meta_type metas);
		Printf.printf "\n\t| Last\n\t| Dollar of string\n\t| Custom of string\n\n";
		Printf.printf "let get_info = function\n";
		Printf.printf "%s" (gen_meta_info metas);
		Printf.printf "\n\t| Last -> die \"\" __LOC__\n\t| Dollar s -> \"$\" ^ s,(\"\",[])\n\t| Custom s -> s,(\"\",[])\n"
	| [_; "warning"; warning_path]->
		let warnings = parse_file_array warning_path parse_warning in
		print_endline "type warning =";
		print_endline (gen_warning_type warnings);
		print_endline "";
		print_endline "let warning_id = function";
		print_endline (gen_warning_ids warnings);
		print_endline "";
		print_endline "let warning_doc = function";
		print_endline (gen_warning_doc warnings);
	| _ :: "libparams" :: params ->
		Printf.printf "(%s)" (String.concat " " (List.map (fun s -> Printf.sprintf "\"%s\"" s) params))
	| [_ ;"version";add_revision;branch;sha] ->
		begin match add_revision with
		| "0" | "" ->
			print_endline "let version_extra = None"
		| _ ->
			Printf.printf "let version_extra = Some (\"git build %s\",\"%s\")" branch sha
		end
	| args ->
		print_endline (String.concat ", " args)
