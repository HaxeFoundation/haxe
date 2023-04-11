exception HxbFailure of string

type chunk_kind =
	| STRI (* string pool *)
	| DOCS (* doc pool *)
	| HHDR (* module header *)
	| TYPF (* forward types *)
	| CLSR (* class reference array *)
	| ABSR (* abstract reference array *)
	| CLSD (* class definition *)
	| CFLD (* class fields without expressions *)
	| ABSD (* abstract definition *)
	| HEND (* the end *)

let string_of_chunk_kind = function
	| STRI -> "STRI"
	| DOCS -> "DOCS"
	| HHDR -> "HHDR"
	| TYPF -> "TYPF"
	| CLSR -> "CLSR"
	| ABSR -> "ABSR"
	| CLSD -> "CLSD"
	| CFLD -> "CFLD"
	| ABSD -> "ABSD"
	| HEND -> "HEND"

let chunk_kind_of_string = function
	| "STRI" -> STRI
	| "DOCS" -> DOCS
	| "HHDR" -> HHDR
	| "TYPF" -> TYPF
	| "CLSR" -> CLSR
	| "ABSR" -> ABSR
	| "CLSD" -> CLSD
	| "CFLD" -> CFLD
	| "ABSD" -> ABSD
	| "HEND" -> HEND
	| name -> raise (HxbFailure ("Invalid chunk name: " ^ name))

let error (s : string) =
	raise (HxbFailure s)