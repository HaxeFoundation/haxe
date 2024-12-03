open Type

let haxe_exception_type_path = (["haxe"],"Exception")
let value_exception_type_path = (["haxe"],"ValueException")

(**
	Check if `cls` is or extends (if `check_parent=true`) `haxe.Exception`
*)
	let rec is_haxe_exception_class ?(check_parent=true) cls =
		cls.cl_path = haxe_exception_type_path
		|| (check_parent && match cls.cl_super with
			| None -> false
			| Some (cls, _) -> is_haxe_exception_class ~check_parent cls
		)
	
	(**
		Check if `t` is or extends `haxe.Exception`
	*)
	let is_haxe_exception ?(check_parent=true) (t:Type.t) =
		match Abstract.follow_with_abstracts t with
			| TInst (cls, _) -> is_haxe_exception_class ~check_parent cls
			| _ -> false