module type DataWriterApi = sig
	type data

	val write_optional : data option -> data

	val write_object : (string * data) list -> data

	val write_array : data list -> data

	val write_string : string -> data

	val write_bool : bool -> data

	val write_int : int -> data
end