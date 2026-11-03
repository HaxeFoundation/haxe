open Globals
open Type

(**
	The capture policy tells which handling we make of captured locals
	(the locals which are referenced in local functions)

	See details/implementation in Codegen.captured_vars
*)
type capture_policy =
	(** do nothing, let the platform handle it *)
	| CPNone
	(** wrap all captured variables into a single-element array to allow modifications *)
	| CPWrapRef
	(** similar to wrap ref, but will only apply to the locals that are declared in loops *)
	| CPLoopVars

type exceptions_config = {
	(* Base types which may be thrown from Haxe code without wrapping. *)
	ec_native_throws : path list;
	(* Base types which may be caught from Haxe code without wrapping. *)
	ec_native_catches : path list;
	(*
		Hint exceptions filter to avoid wrapping for targets, which can throw/catch any type
		Ignored on targets with a specific native base type for exceptions.
	*)
	ec_avoid_wrapping : bool;
	(* Path of a native class or interface, which can be used for wildcard catches. *)
	ec_wildcard_catch : path;
	(*
		Path of a native base class or interface, which can be thrown.
		This type is used to cast `haxe.Exception.thrown(v)` calls to.
		For example `throw 123` is compiled to `throw (cast Exception.thrown(123):ec_base_throw)`
	*)
	ec_base_throw : path;
	(*
		Checks if throwing this expression is a special case for current target
		and should not be modified.
	*)
	ec_special_throw : texpr -> bool;
}

type var_scope =
	| FunctionScope
	| BlockScope

type var_scoping_flags =
	(**
		Variables are hoisted in their scope
	*)
	| VarHoisting
	(**
		It's not allowed to shadow existing variables in a scope.
	*)
	| NoShadowing
	(**
		It's not allowed to shadow a `catch` variable.
	*)
	| NoCatchVarShadowing
	(**
		Local vars cannot have the same name as the current top-level package or
		(if in the root package) current class name
	*)
	| ReserveCurrentTopLevelSymbol
	(**
		Local vars cannot have a name used for any top-level symbol
		(packages and classes in the root package)
	*)
	| ReserveAllTopLevelSymbols
	(**
		Reserve all type-paths converted to "flat path" with `Path.flat_path`
	*)
	| ReserveAllTypesFlat
	(**
		List of names cannot be taken by local vars
	*)
	| ReserveNames of string list
	(**
		Cases in a `switch` won't have blocks, but will share the same outer scope.
	*)
	| SwitchCasesNoBlocks

type var_scoping_config = {
	vs_flags : var_scoping_flags list;
	vs_scope : var_scope;
}

type platform_config = {
	(** has a static type system, with not-nullable basic types (Int/Float/Bool) *)
	pf_static : bool;
	(** has access to the "sys" package *)
	pf_sys : bool;
	(** captured variables handling (see before) *)
	pf_capture_policy : capture_policy;
	(** when calling a method with optional args, do we replace the missing args with "null" constants *)
	pf_pad_nulls : bool;
	(** add a final return to methods not having one already - prevent some compiler warnings *)
	pf_add_final_return : bool;
	(** does the platform natively support overloaded functions *)
	pf_overload : bool;
	(** can the platform use default values for non-nullable arguments *)
	pf_can_skip_non_nullable_argument : bool;
	(** type paths that are reserved on the platform *)
	pf_reserved_type_paths : path list;
	(** supports function == function **)
	pf_supports_function_equality : bool;
	(** uses utf16 encoding with ucs2 api **)
	pf_uses_utf16 : bool;
	(** target supports accessing `this` before calling `super(...)` **)
	pf_this_before_super : bool;
	(** target supports threads **)
	pf_supports_threads : bool;
	(** target supports Unicode **)
	pf_supports_unicode : bool;
	(** target supports rest arguments **)
	pf_supports_rest_args : bool;
	(** exceptions handling config **)
	pf_exceptions : exceptions_config;
	(** the scoping of local variables *)
	pf_scoping : var_scoping_config;
	(** target supports atomic operations via haxe.Atomic **)
	pf_supports_atomics : bool;
}