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

let is_dynamic t =
	match Abstract.follow_with_abstracts t with
	| TAbstract({ a_path = [],"Dynamic" }, _) -> true
	| t -> t == t_dynamic

let make_call scom eon el tret p =
	let default () =
		mk (TCall(eon,el)) tret p
	in
	match eon.eexpr with
	| TField(ef,(FStatic(cl,cf) | FInstance(cl,_,cf))) when SafeCom.needs_inline scom (Some cl) cf ->
		begin match cf.cf_expr with
		| Some {eexpr = TFunction tf} ->
			let config = Inline.inline_config (Some cl) cf el tret in
			Inline.type_inline (Inline.context_of_scom scom) cf tf ef el tret config p false
		| _ ->
			default ()
		end
	| _ ->
		default ()

let make_static_call scom c cf el tret p =
	let ef = Texpr.Builder.make_static_field c cf p in
	make_call scom ef el tret p
