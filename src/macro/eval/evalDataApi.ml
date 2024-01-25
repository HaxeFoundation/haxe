open EvalValue
open EvalContext
open EvalDecode

type data = value

let read_optional v f = match v with
	| VNull ->
		()
	| _ ->
		f v

let read_object v =
	List.map (fun (i,v) ->
		EvalHash.rev_hash i,v
	) (object_fields (decode_object v))

let read_array v =
	EvalArray.to_list (decode_varray v)

let read_string v =
	decode_string v

let read_int v =
	decode_int v

let read_bool v =
	decode_bool v

let data_to_string v =
	(EvalPrinting.s_value 0 v).sstring