open Json

let error s =
	(* TODO: should this raise something else? *)
	Error.raise_typing_error s Globals.null_pos

type data = Json.t

let read_optional json f = match json with
	| JNull ->
		()
	| _ ->
		f json

let read_object json = match json with
	| JObject fl ->
		fl
	| _ ->
		error (Printf.sprintf "Expected JObject, found %s" (string_of_json json))

let read_array json = match json with
	| JArray l ->
		l
	| _ ->
		error (Printf.sprintf "Expected JArray, found %s" (string_of_json json))

let read_string json = match json with
	| JString s ->
		s
	| _ ->
		error (Printf.sprintf "Expected JString, found %s" (string_of_json json))

let read_int json = match json with
	| JInt i ->
		i
	| _ ->
		error (Printf.sprintf "Expected JInt, found %s" (string_of_json json))

let read_bool json = match json with
	| JBool b ->
		b
	| _ ->
		error (Printf.sprintf "Expected JBool, found %s" (string_of_json json))

let data_to_string json =
	string_of_json json