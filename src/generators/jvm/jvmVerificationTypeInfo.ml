open JvmGlobals
open JvmSignature

type t =
	| VTop
	| VInteger
	| VFloat
	| VDouble
	| VLong
	| VNull
	| VUninitializedThis
	| VObject of jvm_constant_pool_index
	| VUninitialized of int

let of_signature pool jsig = match jsig with
    | TByte | TChar | TBool | TShort | TInt -> VInteger
    | TFloat -> VFloat
    | TLong -> VLong
    | TDouble -> VDouble
    | TObject(path,_) -> VObject (pool#add_path path)
	| TMethod _ -> VObject (pool#add_path (["java";"lang";"invoke"],"MethodHandle"))
	| TArray _ -> VObject (pool#add_path ([],generate_signature false jsig))
	| TTypeParameter _ -> VObject (pool#add_path (["java";"lang"],"Object"))
	| TUninitialized (Some i) -> VUninitialized i
	| TUninitialized None -> VUninitializedThis
    | _ -> assert false

let to_string vtt = match vtt with
	| VTop -> "top"
	| VInteger -> "integer"
	| VFloat -> "float"
	| VLong -> "long"
	| VDouble -> "double"
	| VNull -> "null"
	| VUninitializedThis -> "uninitializedThis"
	| VObject i -> "object " ^ (string_of_int i)
	| VUninitialized i -> "uninitializedVariable " ^ (string_of_int i)