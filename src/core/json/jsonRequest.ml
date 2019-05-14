open Jsonrpc_handler

class json_request (rpc:jsonrpc_handler) = object (self)
	val mutable requested_meta_list = None
	(**
		The contents of `params.meta` field of the request json
	*)
	method get_requested_meta_list : string list option =
		match requested_meta_list with
			| Some result -> result
			| None ->
				let result =
					rpc#get_opt_param
						(fun () -> Some (List.map (rpc#get_string "Metadata name") (rpc#get_array_param "meta")))
						None
				in
				requested_meta_list <- Some result;
				result
end