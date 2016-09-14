open Ast
open Type

type call_error =
	| Not_enough_arguments of (string * bool * t) list
	| Too_many_arguments
	| Could_not_unify of error_msg
	| Cannot_skip_non_nullable of string

and error_msg =
	| Module_not_found of path
	| Type_not_found of path * string
	| Unify of unify_error list
	| Custom of string
	| Unknown_ident of string
	| Stack of error_msg * error_msg
	| Call_error of call_error
	| No_constructor of module_type

exception Fatal_error of string * pos
exception Error of error_msg * pos

let string_source t = match follow t with
	| TInst(c,_) -> List.map (fun cf -> cf.cf_name) c.cl_ordered_fields
	| TAnon a -> PMap.fold (fun cf acc -> cf.cf_name :: acc) a.a_fields []
	| TAbstract({a_impl = Some c},_) -> List.map (fun cf -> cf.cf_name) c.cl_ordered_statics
	| _ -> []

let short_type ctx t =
	let tstr = s_type ctx t in
	if String.length tstr > 150 then String.sub tstr 0 147 ^ "..." else tstr

let unify_error_msg ctx = function
	| Cannot_unify (t1,t2) ->
		s_type ctx t1 ^ " should be " ^ s_type ctx t2
	| Invalid_field_type s ->
		"Invalid type for field " ^ s ^ " :"
	| Has_no_field (t,n) ->
		StringError.string_error n (string_source t) (short_type ctx t ^ " has no field " ^ n)
	| Has_no_runtime_field (t,n) ->
		s_type ctx t ^ "." ^ n ^ " is not accessible at runtime"
	| Has_extra_field (t,n) ->
		short_type ctx t ^ " has extra field " ^ n
	| Invalid_kind (f,a,b) ->
		(match a, b with
		| Var va, Var vb ->
			let name, stra, strb = if va.v_read = vb.v_read then
				"setter", s_access false va.v_write, s_access false vb.v_write
			else if va.v_write = vb.v_write then
				"getter", s_access true va.v_read, s_access true vb.v_read
			else
				"access", "(" ^ s_access true va.v_read ^ "," ^ s_access false va.v_write ^ ")", "(" ^ s_access true vb.v_read ^ "," ^ s_access false vb.v_write ^ ")"
			in
			"Inconsistent " ^ name ^ " for field " ^ f ^ " : " ^ stra ^ " should be " ^ strb
		| _ ->
			"Field " ^ f ^ " is " ^ s_kind a ^ " but should be " ^ s_kind b)
	| Invalid_visibility n ->
		"The field " ^ n ^ " is not public"
	| Not_matching_optional n ->
		"Optional attribute of parameter " ^ n ^ " differs"
	| Cant_force_optional ->
		"Optional parameters can't be forced"
	| Invariant_parameter _ ->
		"Type parameters are invariant"
	| Constraint_failure name ->
		"Constraint check failure for " ^ name
	| Missing_overload (cf, t) ->
		cf.cf_name ^ " has no overload for " ^ s_type ctx t
	| Unify_custom msg ->
		msg

let rec error_msg = function
	| Module_not_found m -> "Type not found : " ^ s_type_path m
	| Type_not_found (m,t) -> "Module " ^ s_type_path m ^ " does not define type " ^ t
	| Unify l ->
		let ctx = print_context() in
		String.concat "\n" (List.map (unify_error_msg ctx) l)
	| Unknown_ident s -> "Unknown identifier : " ^ s
	| Custom s -> s
	| Stack (m1,m2) -> error_msg m1 ^ "\n" ^ error_msg m2
	| Call_error err -> s_call_error err
	| No_constructor mt -> (s_type_path (t_infos mt).mt_path ^ " does not have a constructor")

and s_call_error = function
	| Not_enough_arguments tl ->
		let pctx = print_context() in
		"Not enough arguments, expected " ^ (String.concat ", " (List.map (fun (n,_,t) -> n ^ ":" ^ (short_type pctx t)) tl))
	| Too_many_arguments -> "Too many arguments"
	| Could_not_unify err -> error_msg err
	| Cannot_skip_non_nullable s -> "Cannot skip non-nullable argument " ^ s

let error msg p = raise (Error (Custom msg,p))

let raise_error err p = raise (Error(err,p))