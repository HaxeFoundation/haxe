open Json
open JsonRpc

type haxe_json_error =
	| MissingField of string * string
	| BadType of string * string

class jsonrpc_handler (id,name,params) = object(self)
	val id = id
	val method_name : string = name
	val params = match params with
		| Some json -> json
		| None -> JNull

	method get_id = id
	method get_method_name = method_name

	method raise_haxe_json_error : 'a . haxe_json_error -> 'a = function
		| MissingField(name,on) -> raise_custom id 1 (Printf.sprintf "Missing param \"%s\" on \"%s\"" name on)
		| BadType(desc,expected) -> raise_custom id 2 (Printf.sprintf "Unexpected value for \"%s\", expected %s" desc expected)

	method get_field desc fl name : Json.t =
		try
			List.assoc name fl
		with Not_found ->
			self#raise_haxe_json_error (MissingField(name,desc))

	method get_string desc j = match j with
		| JString s -> s
		| _ -> self#raise_haxe_json_error (BadType(desc,"String"))

	method get_int desc j = match j with
		| JInt i -> i
		| _ -> self#raise_haxe_json_error (BadType(desc,"String"))

	method get_bool desc j = match j with
		| JBool b -> b
		| _ -> self#raise_haxe_json_error (BadType(desc,"Bool"))

	method get_array desc j : Json.t list = match j with
		| JArray a -> a
		| _ -> self#raise_haxe_json_error (BadType(desc,"Array"))

	method get_object desc j = match j with
		| JObject o -> o
		| _ -> self#raise_haxe_json_error (BadType(desc,"Object"))

	method get_string_field desc name fl =
		self#get_string desc (self#get_field desc fl name)

	method get_int_field desc name fl =
		self#get_int desc (self#get_field desc fl name)

	method get_bool_field desc name fl =
		self#get_bool desc (self#get_field desc fl name)

	method get_array_field desc name fl =
		self#get_array desc (self#get_field desc fl name)

	method get_object_field desc name fl =
		self#get_object desc (self#get_field desc fl name)

	method private get_obj_params = match params with
		| JObject fl -> fl
		| _ -> invalid_arg "params"

	method get_string_param name =
		self#get_string_field "params" name (self#get_obj_params)

	method get_int_param name =
		self#get_int_field "params" name (self#get_obj_params)

	method get_bool_param name =
		self#get_bool_field "params" name (self#get_obj_params)

	method get_array_param name =
		self#get_array_field "params" name (self#get_obj_params)

	method get_object_param name =
		self#get_object_field "params" name (self#get_obj_params)

	method get_opt_param : 'a . (unit -> 'a) -> 'a -> 'a = fun f def ->
		try f() with JsonRpc_error _ -> def

	method get_params = params
end