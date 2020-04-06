open Type

type with_type_source =
	| FunctionArgument of string
	| StructureField of string
	| ImplicitReturn

type t =
	| NoValue
	| Value of with_type_source option
	| WithType of Type.t * with_type_source option

let with_type t = WithType(t,None)
let of_implicit_return t = WithType(t,Some ImplicitReturn)
let with_argument t name = WithType(t,Some(FunctionArgument name))
let with_structure_field t name = WithType(t,Some(StructureField name))
let value = Value None
let named_argument name = Value (Some(FunctionArgument name))
let named_structure_field name = Value (Some(StructureField name))
let no_value = NoValue

let to_string = function
	| NoValue -> "NoValue"
	| Value (None | Some ImplicitReturn) -> "Value"
	| Value (Some(FunctionArgument s | StructureField s)) -> "Value " ^ s
	| WithType(t,s) ->
		let name = match s with
			| Some(FunctionArgument s | StructureField s) -> s
			| _ -> "None"
		in
		Printf.sprintf "WithType(%s, %s)" (s_type (print_context()) t) name
