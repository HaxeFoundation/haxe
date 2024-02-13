open Type

let haxe_exception_type_path = (["haxe"],"Exception")
let value_exception_type_path = (["haxe"],"ValueException")

let is_dynamic t =
	match Abstract.follow_with_abstracts t with
	| TAbstract({ a_path = [],"Dynamic" }, _) -> true
	| t -> t == t_dynamic

(**
	Check if expression represents an exception wrapped with `haxe.Exception.thrown`
*)
let is_wrapped_exception e =
	match e.eexpr with
	| TMeta ((Meta.WrappedException, _, _), _) -> true
	| _ -> false

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

(**
	Check if type path of `t` exists in `lst`
*)
let is_in_list t lst =
	match Abstract.follow_with_abstracts t with
	| TInst(cls,_) ->
		let rec check cls =
			List.mem cls.cl_path lst
			|| List.exists (fun (cls,_) -> check cls) cls.cl_implements
			|| Option.map_default (fun (cls,_) -> check cls) false cls.cl_super
		in
		(match follow t with
		| TInst (cls, _) -> check cls
		| _ -> false
		)
	| TAbstract({ a_path = path },_)
	| TEnum({ e_path = path },_) ->
		List.mem path lst
	| _ -> false