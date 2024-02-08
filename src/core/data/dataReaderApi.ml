module type DataReaderApi = sig
	type data

	val read_optional : data -> (data -> unit) -> unit

	val read_object : data -> (string * data) list

	val read_array : data -> data list

	val read_string : data -> string

	val read_bool : data -> bool

	val read_int : data -> int

	val data_to_string : data -> string
end