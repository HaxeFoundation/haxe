open Type

type with_type_source_information = {
	si_name : string;
	si_doc : string option;
}

type with_type_source =
	| FunctionArgument of with_type_source_information
	| StructureField of with_type_source_information
	| LocalVariable of with_type_source_information
	| ImplicitReturn

type t =
	| NoValue
	| Value of with_type_source option
	| WithType of Type.t * with_type_source option

let make_with_type_source_information name doc = {
	si_name = name;
	si_doc = doc;
}

let with_type t = WithType(t,None)
let of_implicit_return t = WithType(t,Some ImplicitReturn)
let with_argument t name = WithType(t,Some(FunctionArgument (make_with_type_source_information name None)))
let with_argument_and_doc t name doc = WithType(t,Some(FunctionArgument (make_with_type_source_information name (Some doc))))
let with_structure_field t name = WithType(t,Some(StructureField (make_with_type_source_information name None)))
let with_local_variable t name = WithType(t,Some(LocalVariable (make_with_type_source_information name None)))
let value = Value None
let named_argument name = Value (Some(FunctionArgument (make_with_type_source_information name None)))
let named_structure_field name = Value (Some(StructureField (make_with_type_source_information name None)))
let no_value = NoValue

let get_source_info_name = function
	| FunctionArgument si -> Some si.si_name
	| StructureField si -> Some si.si_name
	| LocalVariable si -> Some si.si_name
	| ImplicitReturn -> None

let string_of_with_type_source = function
	| FunctionArgument si ->
		Printf.sprintf "FunctionArgument(%s)" si.si_name
	| StructureField si ->
		Printf.sprintf "StructureField(%s)" si.si_name
	| LocalVariable si ->
		Printf.sprintf "LocalVariable(%s)" si.si_name
	| ImplicitReturn ->
		"ImplicitReturn"

let get_expected_name with_type = match with_type with
	| Value (Some si) | WithType(_,Some si) ->
		get_source_info_name si
	| _ ->
		None

let to_string = function
	| NoValue ->
		"NoValue"
	| Value None ->
		"Value(None)"
	| Value (Some wts) ->
		Printf.sprintf "Value(Some(%s))" (string_of_with_type_source wts)
	| WithType(t,wts) ->
		let s = match wts with
			| None ->
				"None"
			| Some wts ->
				Printf.sprintf "Some(%s)" (string_of_with_type_source wts)
		in
		Printf.sprintf "WithType(%s, %s)" (s_type (print_context()) t) s