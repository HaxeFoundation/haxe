open Json

exception Prebuild_error of string

type parsed_warning = {
	w_name : string;
	w_doc : string;
	w_parent : string option;
	w_generic : bool;
}

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

let get_optional_field2 name map fields =
	try
		let field = List.find (fun (n, _) -> n = name) fields in
		let value = map (snd field) in
		match value with
		| None -> raise (Prebuild_error ("field `" ^ name ^ "` has invalid data"))
		| Some v -> Some v
	with Not_found ->
		None

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
	{
		w_name = get_field "name" as_string fields;
		w_doc = get_field "doc" as_string fields;
		w_parent = get_optional_field2 "parent" as_string fields;
		w_generic = get_optional_field "generic" as_bool false fields;
	}

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
		w ->
			Printf.sprintf "\t| %s" w.w_name
	) warnings in
	String.concat "\n" warning_str

let gen_warning_parse warnings =
	let warning_str = List.map (function
		w ->
			Printf.sprintf "\t| \"%s\" -> %s" w.w_name w.w_name
	) warnings in
	let warning_str = warning_str @ ["\t| _ -> raise Exit"] in
	String.concat "\n" warning_str


let gen_warning_obj warnings =
	let warning_str = List.map (fun w ->
		let w_parent = match w.w_parent with
			| None -> if w.w_name = "WAll" then "None" else "Some WAll"
			| Some w -> Printf.sprintf "Some %s" w
		in
		Printf.sprintf "\t| %s -> {w_name = \"%s\"; w_doc = \"%s\"; w_generic = %b; w_parent = %s}" w.w_name w.w_name (s_escape w.w_doc) w.w_generic w_parent
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

let parse_meta_usage = function
	| \"TClass\" -> TClass
	| \"TClassField\" -> TClassField
	| \"TAbstract\" -> TAbstract
	| \"TAbstractField\" -> TAbstractField
	| \"TEnum\" -> TEnum
	| \"TTypedef\" -> TTypedef
	| \"TAnyField\" -> TAnyField
	| \"TExpr\" -> TExpr
	| \"TTypeParameter\" -> TTypeParameter
	| \"TVariable\" -> TVariable
	| t -> raise (failwith (\"invalid metadata target \" ^ t))

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
		Printf.printf "\n\t| Last\n\t| Custom of string\n\n";
		Printf.printf "let infos = function\n";
		Printf.printf "%s" (gen_define_info defines);
		Printf.printf "\n\t| Last -> die \"\" __LOC__\n\t| Custom s -> s,(\"\",[])\n"
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
		print_endline "type warning_obj = {";
		print_endline "\tw_name : string;";
		print_endline "\tw_doc : string;";
		print_endline "\tw_generic : bool;";
		print_endline "\tw_parent : warning option;";
		print_endline "}";
		print_endline "";
		print_endline "let warning_obj = function";
		print_endline (gen_warning_obj warnings);
		print_endline "";
		print_endline "let from_string = function";
		print_endline (gen_warning_parse warnings);
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
