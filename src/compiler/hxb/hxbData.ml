exception HxbFailure of string

type chunk_kind =
	| STRI (* string pool *)
	| DOCS (* doc pool *)
	| HHDR (* module header *)
	| ANNR (* anon reference array *)
	| TYPF (* forward types *)
	| CLSR (* class reference array *)
	| ABSR (* abstract reference array *)
	| TPDR (* typedef reference array *)
	| ENMR (* enum reference array *)
	| ANFR (* anon field reference array *)
	| CLSD (* class definition *)
	| ABSD (* abstract definition *)
	| CFLD (* class fields *)
	| TPDD (* typedef definition *)
	| ENMD (* enum definition *)
	| EFLD (* enum fields *)
	| ANND (* anon definition *)
	| ANFD (* anon fields *)
	| HEND (* the end *)

let string_of_chunk_kind = function
	| STRI -> "STRI"
	| DOCS -> "DOCS"
	| HHDR -> "HHDR"
	| TYPF -> "TYPF"
	| ANNR -> "ANNR"
	| CLSR -> "CLSR"
	| ABSR -> "ABSR"
	| ENMR -> "ENMR"
	| TPDR -> "TPDR"
	| ANFR -> "ANFR"
	| ANND -> "ANND"
	| CLSD -> "CLSD"
	| CFLD -> "CFLD"
	| ABSD -> "ABSD"
	| ENMD -> "ENMD"
	| EFLD -> "EFLD"
	| TPDD -> "TPDD"
	| ANFD -> "ANFD"
	| HEND -> "HEND"

let chunk_kind_of_string = function
	| "STRI" -> STRI
	| "DOCS" -> DOCS
	| "HHDR" -> HHDR
	| "TYPF" -> TYPF
	| "ANNR" -> ANNR
	| "CLSR" -> CLSR
	| "ABSR" -> ABSR
	| "ENMR" -> ENMR
	| "TPDR" -> TPDR
	| "ANFR" -> ANFR
	| "ANND" -> ANND
	| "CLSD" -> CLSD
	| "CFLD" -> CFLD
	| "ABSD" -> ABSD
	| "ENMD" -> ENMD
	| "EFLD" -> EFLD
	| "ANFD" -> ANFD
	| "TPDD" -> TPDD
	| "HEND" -> HEND
	| name -> raise (HxbFailure ("Invalid chunk name: " ^ name))

let error (s : string) =
	Printf.eprintf "[error] %s\n" s;
	raise (HxbFailure s)
