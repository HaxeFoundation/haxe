exception HxbFailure of string

(* TEMP: Wipe server cache to force loading from hxb *)
(* See ServerCompilationContext.after_compilation *)
(* Also see ServerTests.testDisplayModuleRecache test which needs updating if set to false *)
let always_wipe_cache = true

type chunk_kind =
	| STRI (* string pool *)
	| DOCS (* doc pool *)
	| HHDR (* module header *)
	| TYPF (* forward types *)
	| CLSR (* class reference array *)
	| ABSR (* abstract reference array *)
	| TPDR (* typedef reference array *)
	| ENMR (* enum reference array *)
	| CLSD (* class definition *)
	| ABSD (* abstract definition *)
	| CFLD (* class fields *)
	| AFLD (* abstract fields *)
	| TPDD (* typedef definition *)
	| ENMD (* enum definition *)
	| EFLD (* enum fields *)
	| HEND (* the end *)

let string_of_chunk_kind = function
	| STRI -> "STRI"
	| DOCS -> "DOCS"
	| HHDR -> "HHDR"
	| TYPF -> "TYPF"
	| CLSR -> "CLSR"
	| ABSR -> "ABSR"
	| TPDR -> "TPDR"
	| ENMR -> "ENMR"
	| CLSD -> "CLSD"
	| ABSD -> "ABSD"
	| CFLD -> "CFLD"
	| AFLD -> "AFLD"
	| TPDD -> "TPDD"
	| ENMD -> "ENMD"
	| EFLD -> "EFLD"
	| HEND -> "HEND"

let chunk_kind_of_string = function
	| "STRI" -> STRI
	| "DOCS" -> DOCS
	| "HHDR" -> HHDR
	| "TYPF" -> TYPF
	| "CLSR" -> CLSR
	| "ABSR" -> ABSR
	| "TPDR" -> TPDR
	| "ENMR" -> ENMR
	| "CLSD" -> CLSD
	| "ABSD" -> ABSD
	| "CFLD" -> CFLD
	| "AFLD" -> AFLD
	| "TPDD" -> TPDD
	| "ENMD" -> ENMD
	| "EFLD" -> EFLD
	| "HEND" -> HEND
	| name -> raise (HxbFailure ("Invalid chunk name: " ^ name))

let error (s : string) =
	Printf.eprintf "[error] %s\n" s;
	raise (HxbFailure s)

let hxb_version = 1