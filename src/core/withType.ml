open Type

type expected_name_source =
	| FunctionArgument
	| StructureField

type expected_name = string * expected_name_source

type t =
	| NoValue
	| Value of expected_name option
	| WithType of Type.t * expected_name option

let with_type t = WithType(t,None)
let with_argument t name = WithType(t,Some(name,FunctionArgument))
let with_structure_field t name = WithType(t,Some(name,StructureField))
let value = Value None
let named_argument name = Value (Some(name,FunctionArgument))
let named_structure_field name = Value (Some(name,StructureField))
let no_value = NoValue

let to_string = function
	| NoValue -> "NoValue"
	| Value None -> "Value"
	| Value (Some(s,_)) -> "Value " ^ s
	| WithType(t,s) ->
		let name = match s with
			| None -> "None"
			| Some(s,_) -> s
		in
		Printf.sprintf "WithType(%s, %s)" (s_type (print_context()) t) name
