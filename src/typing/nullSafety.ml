open Globals
open Ast
open Type

type safety_message = {
	sm_msg : string;
	sm_pos : pos;
}

type safety_report = {
	mutable sr_errors : safety_message list;
}

let add_error report msg pos =
	let error = { sm_msg = ("Null safety: " ^ msg); sm_pos = pos; } in
	if not (List.mem error report.sr_errors) then
		report.sr_errors <- error :: report.sr_errors;

type scope_type =
	| STNormal
	| STLoop
	| STClosure
	(* A closure which gets executed along the "normal" program flow without being delayed or stored somewhere *)
	| STImmediateClosure

type safety_unify_error =
	| NullSafetyError

exception Safety_error of safety_unify_error

let safety_error () : unit = raise (Safety_error NullSafetyError)

type safety_mode =
	| SMOff
	| SMLoose
	| SMStrict
	| SMStrictThreaded

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue in compiler repo.
*)
let fail ?msg hxpos mlpos =
	let msg =
		(Lexer.get_error_pos (Printf.sprintf "%s:%d:") hxpos) ^ ": "
		^ "Null safety: " ^ (match msg with Some msg -> msg | _ -> "unexpected expression.") ^ "\n"
		^ "Submit an issue to https://github.com/HaxeFoundation/haxe/issues with expression example and following information:"
	in
	match mlpos with
		| (file, line, _, _) ->
			Printf.eprintf "%s\n" msg;
			Printf.eprintf "%s:%d\n" file line;
			die "" __LOC__

(**
	Returns human-readable string representation of specified type
*)
let str_type = s_type (print_context())

(**
	Returns human-readable representation of specified expression
*)
let str_expr = s_expr_pretty false "\t" true str_type

let is_string_type t =
	match t with
		| TInst ({ cl_path = ([], "String")}, _)
		| TAbstract ({ a_path = ([],"Null") },[TInst ({ cl_path = ([], "String")}, _)]) -> true
		| _ -> false

(**
	Check for explicit `Null<>` typing
*)
let rec is_nullable_type ?(dynamic_is_nullable=false) = function
	| TMono r ->
		(match r.tm_type with None -> false | Some t -> is_nullable_type t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		true
	| TAbstract ({ a_path = ([],"Any") },[]) ->
		false
	| TAbstract (a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		is_nullable_type (apply_params a.a_params tl a.a_this)
	| TLazy f ->
		is_nullable_type (lazy_type f)
	| TType (t,tl) ->
		is_nullable_type (apply_typedef t tl)
	| (TDynamic _) as t ->
		dynamic_is_nullable && t == t_dynamic
	| _ ->
		false
(*
(**
	Check if `callee` represents `trace`
*)
let is_trace_expr callee =
	match callee.eexpr with
	| TIdent "`trace" -> true
	| _ -> false *)


(**
	If `expr` is a TCast or TMeta, returns underlying expression (recursively bypassing nested casts).
	Otherwise returns `expr` as is.
	If `stay_safe` is true, then casts to non-nullable types won't be revealed and an expression will stay intact.
*)
let rec reveal_expr ?(stay_safe=true) expr =
	match expr.eexpr with
		| TCast (e, _) when not stay_safe || is_nullable_type expr.etype -> reveal_expr e
		| TMeta (_, e) -> reveal_expr e
		| _ -> expr

(**
	Try to get a human-readable representation of an `expr`
*)
let symbol_name expr =
	match (reveal_expr ~stay_safe:false expr).eexpr with
		| TField (_, access) -> field_name access
		| TIdent name -> name
		| TLocal { v_name = name } -> name
		| TNew _ -> "new"
		| _ -> ""

type safety_subject =
	(*
		Fields accessed through a static access are identified by the class path and the field name.
		E.g.
			`pack.MyClass.field` is `((["pack"], "MyClass"), ["field"])`
			`pack.MyClass.field.sub` is `((["pack"], "MyClass"), ["field"; "sub"])`
	*)
	| SFieldOfClass of (path * (string list))
	(*
		Fields accessed through a local variable are identified by the var id and the field name.
		E.g.
			`v.field` is `(v.v_id, ["field"])`
			`v.field.sub` is `(v.v_id, ["field"; "sub"])`
	*)
	| SFieldOfLocalVar of (int * (string list))
	(*
		Fields accessed through `this` are identified by their names.
		E.g.
			`this.field` is `["field"]`
			`this.field.sub` is `["field"; "sub"]`
	*)
	| SFieldOfThis of (string list)
	(*
		Local variables - by tvar.v_id
	*)
	| SLocalVar of int
	(*
		For expressions, which cannot be checked agains `null` to become safe
	*)
	| SNotSuitable

let rec get_subject mode expr =
	match (reveal_expr expr).eexpr with
		| TLocal v ->
			SLocalVar v.v_id
		| TField ({ eexpr = TTypeExpr _ }, FStatic (cls, field)) when (mode <> SMStrictThreaded) || (has_class_field_flag field CfFinal) ->
			SFieldOfClass (cls.cl_path, [field.cf_name])
		| TField ({ eexpr = TConst TThis }, (FInstance (_, _, field) | FAnon field)) when (mode <> SMStrictThreaded) || (has_class_field_flag field CfFinal) ->
			SFieldOfThis [field.cf_name]
		| TField ({ eexpr = TLocal v }, (FInstance (_, _, field) | FAnon field)) when (mode <> SMStrictThreaded) || (has_class_field_flag field CfFinal) ->
			SFieldOfLocalVar (v.v_id, [field.cf_name])
		| TField (e, (FInstance (_, _, field) | FAnon field)) when (mode <> SMStrictThreaded) ->
			(match get_subject mode e with
				| SFieldOfClass (path, fields) -> SFieldOfClass (path, field.cf_name :: fields)
				| SFieldOfThis fields -> SFieldOfThis (field.cf_name :: fields)
				| SFieldOfLocalVar (var_id, fields) -> SFieldOfLocalVar (var_id, field.cf_name :: fields)
				|_ -> SNotSuitable
			)
		|_ -> SNotSuitable

(**
	Check if provided expression is a subject to null safety.
	E.g. a call cannot be such a subject, because we cannot track null-state of the call result.
*)
let rec is_suitable mode expr =
	match (reveal_expr expr).eexpr with
		| TField ({ eexpr = TConst TThis }, FInstance _)
		| TField ({ eexpr = TLocal _ }, (FInstance _ | FAnon _))
		| TField ({ eexpr = TTypeExpr _ }, FStatic _)
		| TLocal _ -> true
		| TField (target, (FInstance _ | FStatic _ | FAnon _)) when mode <> SMStrictThreaded -> is_suitable mode target
		|_ -> false

(**
	Returns a list of metadata attached to `callee` arguments.
	E.g. for
	```
	function(@:meta1 a:Type1, b:Type2, @:meta2 c:Type3)
	```
	will return `[ [@:meta1], [], [@:meta2] ]`
*)
let get_arguments_meta callee expected_args_count =
	let rec empty_list n =
		if n <= 0 then []
		else [] :: (empty_list (n - 1))
	in
	match callee.eexpr with
		| TField (_, FAnon field)
		| TField (_, FClosure (_,field))
		| TField (_, FStatic (_, field))
		| TField (_, FInstance (_, _, field)) ->
			(try
				match get_meta Meta.HaxeArguments field.cf_meta with
				| _,[EFunction(_,{ f_args = args }),_],_ when expected_args_count = List.length args ->
					List.map (fun (_,_,m,_,_) -> m) args
				| _ ->
					raise Not_found
			with Not_found ->
				empty_list expected_args_count
			)
		| TFunction { tf_args = args } when expected_args_count = List.length args ->
			List.map (fun (v,_) -> v.v_meta) args
		| _ ->
			empty_list expected_args_count

class unificator =
	object(self)
		val stack = new_rec_stack()
		(**
			Check if it's possible to pass a value of type `a` to a place where a value of type `b` is expected.
			Raises `Safety_error` exception if it's not.
		*)
		method unify a b =
			if a == b then
				()
			else
				match a, b with
					(* if `b` is nullable, no more checks needed *)
					| _, TAbstract ({ a_path = ([],"Null") },[t]) ->
						()
					| TAbstract ({ a_path = ([],"Null") },[t]), _ when not (is_nullable_type b) ->
						safety_error()
					| TInst (_, a_params), TInst(_, b_params) when (List.length a_params) = (List.length b_params) ->
						List.iter2 self#unify a_params b_params
					| TAnon a_anon, TAnon b_anon ->
						self#unify_anon_to_anon a_anon b_anon
					| TInst (a_cls, a_params), TAnon b_anon ->
						self#unify_class_to_anon a_cls a_params b_anon
					| TFun a_signature, TFun b_signature ->
						self#unify_functions a_signature b_signature
					(* patterns below are used to reveal real type *)
					| TLazy f, _ ->
						self#unify (lazy_type f) b
					| _, TLazy f -> self#unify a (lazy_type f)
					| TMono t, _ ->
						(match t.tm_type with None -> () | Some t -> self#unify t b)
					| _, TMono t ->
						(match t.tm_type with None -> () | Some t -> self#unify a t)
					| TType (t,tl), _ ->
						self#unify_rec a b (fun() -> self#unify (apply_typedef t tl) b)
					| _, TType (t,tl) ->
						self#unify_rec a b (fun() -> self#unify a (apply_typedef t tl))
					| TAbstract (abstr,tl), _ when not (Meta.has Meta.CoreType abstr.a_meta) ->
						self#unify (apply_params abstr.a_params tl abstr.a_this) b
					| _, TAbstract (abstr,tl) when not (Meta.has Meta.CoreType abstr.a_meta) ->
						self#unify a (apply_params abstr.a_params tl abstr.a_this)
					| _ ->
						()

		method unify_rec (a:t) (b:t) (frun:unit->unit) =
			let checked =
				rec_stack_exists
					(fun(a2,b2) -> fast_eq a a2 && fast_eq b b2)
					stack
			in
			if not checked then begin
				try
					stack.rec_stack <- (a, b) :: stack.rec_stack;
					frun();
					stack.rec_stack <- List.tl stack.rec_stack
				with
					| e ->
						stack.rec_stack <- List.tl stack.rec_stack;
						raise e
			end

		method private unify_anon_to_anon (a:tanon) (b:tanon) =
			PMap.iter
				(fun name b_field ->
					let a_field =
						try Some (PMap.find name a.a_fields)
						with Not_found -> None
					in
					match a_field with
						| None -> ()
						| Some a_field -> self#unify a_field.cf_type b_field.cf_type
				)
				b.a_fields

		method private unify_class_to_anon (a:tclass) (a_params:tparams) (b:tanon) =
			PMap.iter
				(fun name b_field ->
					let a_field =
						try Some (PMap.find name a.cl_fields)
						with Not_found -> None
					in
					match a_field with
						| None -> ()
						| Some a_field ->
							let a_type = apply_params a.cl_params a_params a_field.cf_type in
							self#unify a_type b_field.cf_type
				)
				b.a_fields

		method private unify_functions (a_args, a_result) (b_args, b_result) =
			(* check return type *)
			(match b_result with
				| TAbstract ({ a_path = ([], "Void") }, []) -> ()
				| _ -> self#unify a_result b_result;
			);
			(* check arguments *)
			let rec traverse a_args b_args =
				match a_args, b_args with
					| [], _ | _, [] -> ()
					| (_, _, a_arg) :: a_rest, (_, _, b_arg) :: b_rest ->
						self#unify b_arg a_arg;
						traverse a_rest b_rest
			in
			traverse a_args b_args
	end

(**
	Check if `expr` is a `trace` (not a call, but identifier itself)
*)
let is_trace expr =
	match expr.eexpr with
	| TIdent "`trace" -> true
	| TField (_, FStatic ({ cl_path = (["haxe"], "Log") }, { cf_name = "trace" })) -> true
	| _ -> false

(**
	If `t` represents `Null<SomeType>` this function returns `SomeType`.
*)
let rec unfold_null t =
	match t with
		| TMono r -> (match r.tm_type with None -> t | Some t -> unfold_null t)
		| TAbstract ({ a_path = ([],"Null") }, [t]) -> unfold_null t
		| TAbstract (abstr,tl) when not (Meta.has Meta.CoreType abstr.a_meta) -> unfold_null (apply_params abstr.a_params tl abstr.a_this)
		| TLazy f -> unfold_null (lazy_type f)
		| TType (t,tl) -> unfold_null (apply_typedef t tl)
		| _ -> t

(**
	Shadow Type.error to avoid raising unification errors, which should not be raised from null-safety checks
*)
let safety_error () : unit = raise (Safety_error NullSafetyError)

let accessed_field_name access =
	match access with
		| FInstance (_, _, { cf_name = name }) -> name
		| FStatic (_, { cf_name = name }) -> name
		| FAnon { cf_name = name } -> name
		| FDynamic name -> name
		| FClosure (_, { cf_name = name }) -> name
		| FEnum (_, { ef_name = name }) -> name

(**
	Collect nullable local vars which are checked against `null`.
	Returns a tuple of (vars_checked_to_be_null * vars_checked_to_be_not_null) in case `condition` evaluates to `true`.
*)
let rec process_condition mode condition (is_nullable_expr:texpr->bool) callback =
	let nulls = ref []
	and not_nulls = ref [] in
	let add to_nulls expr =
		let expr = reveal_expr expr in
		if to_nulls then nulls := expr :: !nulls
		else not_nulls := expr :: !not_nulls
	in
	let rec traverse positive e =
		match e.eexpr with
			| TUnop (Not, Prefix, e) -> traverse (not positive) e
			| TBinop (OpEq, { eexpr = TConst TNull }, checked_expr) when is_suitable mode checked_expr ->
				add positive checked_expr
			| TBinop (OpEq, checked_expr, { eexpr = TConst TNull }) when is_suitable mode checked_expr ->
				add positive checked_expr
			| TBinop (OpNotEq, { eexpr = TConst TNull }, checked_expr) when is_suitable mode checked_expr ->
				add (not positive) checked_expr
			| TBinop (OpNotEq, checked_expr, { eexpr = TConst TNull }) when is_suitable mode checked_expr ->
				add (not positive) checked_expr
			| TBinop (OpEq, e, checked_expr) when is_suitable mode checked_expr && not (is_nullable_expr e) ->
				if positive then not_nulls := checked_expr :: !not_nulls
			| TBinop (OpEq, checked_expr, e) when is_suitable mode checked_expr && not (is_nullable_expr e) ->
				if positive then not_nulls := checked_expr :: !not_nulls
			| TBinop (OpBoolAnd, left_expr, right_expr) when positive ->
				traverse positive left_expr;
				traverse positive right_expr
			| TBinop (OpBoolAnd, left_expr, right_expr) when not positive ->
				List.iter
					(fun e ->
						let _, not_nulls = process_condition mode left_expr is_nullable_expr callback in
						List.iter (add true) not_nulls
					)
					[left_expr; right_expr]
			| TBinop (OpBoolOr, left_expr, right_expr) when not positive ->
				traverse positive left_expr;
				traverse positive right_expr
			| TBinop (OpBoolOr, left_expr, right_expr) when positive ->
				List.iter
					(fun e ->
						let nulls, _ = process_condition mode left_expr is_nullable_expr callback in
						List.iter (add true) nulls
					)
					[left_expr; right_expr]
			| TParenthesis e -> traverse positive e
			| _ -> callback e
	in
	traverse true condition;
	(!nulls, !not_nulls)

(**
	Check if metadata contains @:nullSafety(Off) meta
**)
let rec contains_unsafe_meta metadata =
	match metadata with
		| [] -> false
		| (Meta.NullSafety, [(EConst (Ident "Off"), _)], _) :: _  -> true
		| _ :: rest -> contains_unsafe_meta rest

(**
	Check if metadata contains @:nullSafety or @:nullSafety(true) meta
**)
let rec contains_safe_meta metadata =
	match metadata with
		| [] -> false
		| (Meta.NullSafety, [], _) :: _
		| (Meta.NullSafety, [(EConst (Ident ("Loose" | "Strict" | "StrictThreaded")), _)], _) :: _  -> true
		| _ :: rest -> contains_safe_meta rest

let safety_enabled meta =
	(contains_safe_meta meta) && not (contains_unsafe_meta meta)

let safety_mode (metadata:Ast.metadata) =
	let rec traverse mode meta =
		match mode, meta with
			| Some SMOff, _
			| _, [] -> mode
			| _, (Meta.NullSafety, [(EConst (Ident "Off"), _)], _) :: _ ->
				Some SMOff
			| None, (Meta.NullSafety, ([] | [(EConst (Ident "Loose"), _)]), _) :: rest ->
				traverse (Some SMLoose) rest
			| _, (Meta.NullSafety, [(EConst (Ident "Strict"), _)], _) :: rest ->
				traverse (Some SMStrict) rest
			| _, (Meta.NullSafety, [(EConst (Ident "StrictThreaded"), _)], _) :: rest ->
				traverse (Some SMStrictThreaded) rest
			| _, _ :: rest ->
				traverse mode rest
	in
	match traverse None metadata with
		| Some mode -> mode
		| None -> SMOff

let rec validate_safety_meta report (metadata:Ast.metadata) =
	match metadata with
		| [] -> ()
		| (Meta.NullSafety, args, pos) :: rest ->
			(match args with
				| ([] | [(EConst (Ident ("Off" | "Loose" | "Strict" | "StrictThreaded")), _)]) -> ()
				| _ -> add_error report "Invalid argument for @:nullSafety meta" pos
			);
			validate_safety_meta report rest
		| _ :: rest -> validate_safety_meta report rest

(**
	Check if specified `field` represents a `var` field which will exist at runtime.
*)
let should_be_initialized field =
	not (has_class_field_flag field CfExtern)
	&& match field.cf_kind with
		| Var { v_read = AccNormal | AccInline | AccNo } | Var { v_write = AccNormal | AccNo } -> true
		| Var _ -> Meta.has Meta.IsVar field.cf_meta
		| _ -> false

(**
	Check if all items of the `needle` list exist in the same order in the beginning of the `haystack` list.
*)
let rec list_starts_with_list (haystack:string list) (needle:string list) =
	match haystack, needle with
		| _, [] -> true
		| [], _ -> false
		| current_haystack :: rest_haystack, current_needle :: rest_needle ->
			current_haystack = current_needle && list_starts_with_list rest_haystack rest_needle

(**
	A class which is used to check if an anonymous function passed to a method will be executed
	before that method execution is finished.
*)
class immediate_execution =
	object(self)
		val cache = Hashtbl.create 500
		(**
			Get cached results of the previous checks for the specified `field`
		*)
		method private get_cache field =
			try
				Hashtbl.find cache field
			with
				| Not_found ->
					let field_cache = Hashtbl.create 5 in
					Hashtbl.add cache field field_cache;
					field_cache
		(**
			Check if a lambda passed to `arg_num`th argument of the `callee` function will be executed immediately without
			delaying it or storing it somewhere else.
		*)
		method check callee arg_num =
			match (reveal_expr callee).eexpr with
				| TField (_, FClosure (Some (cls, _), ({ cf_kind = Method (MethNormal | MethInline) } as field)))
				| TField (_, FStatic (cls, ({ cf_kind = Method (MethNormal | MethInline) } as field)))
				| TField (_, FInstance (cls, _, ({ cf_kind = Method (MethNormal | MethInline) } as field))) ->
					if PurityState.is_pure cls field then
						true
					else
						(match cls, field with
							(* known to be pure *)
							| { cl_path = ([], "Array") }, _ -> true
							(* try to analyze function code *)
							| _, ({ cf_expr = (Some { eexpr = TFunction fn }) } as field) when (has_class_field_flag field CfFinal) || not (FiltersCommon.is_overridden cls field) ->
								if arg_num < 0 || arg_num >= List.length fn.tf_args then
									false
								else begin
									let cache = self#get_cache field in
									if Hashtbl.mem cache arg_num then
										Hashtbl.find cache arg_num
									else begin
										Hashtbl.add cache arg_num true;
										let (arg_var, _) = List.nth fn.tf_args arg_num in
										let result = not (self#is_stored arg_var fn.tf_expr) in
										Hashtbl.replace cache arg_num result;
										result
									end
								end
							| _ ->
								false
						)
				| _ -> false
		(**
			Check if `fn_var` is passed somewhere else in `expr` (stored to a var/field, captured by a closure etc.)
		*)
		method private is_stored fn_var expr =
			match expr.eexpr with
				| TThrow { eexpr = TLocal v }
				| TReturn (Some { eexpr = TLocal v })
				| TCast ({ eexpr = TLocal v }, _)
				| TMeta (_, { eexpr = TLocal v })
				| TBinop (OpAssign, _, { eexpr = TLocal v }) when v.v_id = fn_var.v_id ->
					true
				| TFunction fn ->
					let rec captured e =
						match e.eexpr with
							| TLocal v -> v.v_id = fn_var.v_id
							| _ -> check_expr captured e
					in
					captured fn.tf_expr
				| TCall (callee, args) ->
					if self#is_stored fn_var callee then
						true
					else begin
						let arg_num = ref 0 in
						List.exists
							(fun arg ->
								let result =
									match arg.eexpr with
										| TLocal v when v.v_id = fn_var.v_id -> not (self#check callee !arg_num)
										| _ -> self#is_stored fn_var arg
								in
								incr arg_num;
								result
							)
							args
					end
				| _ -> check_expr (self#is_stored fn_var) expr
	end

(**
	Each loop or function should have its own safety scope.
*)
class safety_scope (mode:safety_mode) (scope_type:scope_type) (safe_locals:(safety_subject,texpr) Hashtbl.t) (never_safe:(safety_subject,texpr) Hashtbl.t) =
	object (self)
		(** Local vars declared in current scope *)
		val declarations = Hashtbl.create 100
		method get_safe_locals = safe_locals
		method get_never_safe = never_safe
		method get_type = scope_type
		(**
			Reset local vars safety to the specified state
		*)
		method reset_to (state:(safety_subject,texpr) Hashtbl.t) =
			Hashtbl.clear safe_locals;
			Hashtbl.iter (Hashtbl.add safe_locals) state
		(**
			Should be called for each local var declared
		*)
		method declare_var v =
			Hashtbl.add declarations v.v_id v
		(**
			Check if local var was declared in this scope
		*)
		method owns_var v =
			Hashtbl.mem declarations v.v_id
		(**
			Check if local variable declared in this scope is guaranteed to not have a `null` value.
		*)
		method is_safe (expr:texpr) =
			not (is_nullable_type expr.etype)
			|| match self#get_subject expr with
				| SNotSuitable ->
					false
				| subj ->
					not (Hashtbl.mem never_safe subj)
					&& Hashtbl.mem safe_locals subj
			(* not (Hashtbl.mem never_safe local_var.v_id)
			&& (
				Hashtbl.mem safe_locals local_var.v_id
				|| not (is_nullable_type local_var.v_type)
			) *)
		(**
			Add variable to the list of safe locals.
		*)
		method add_to_safety expr =
			match self#get_subject expr with
				| SNotSuitable -> ()
				| subj -> Hashtbl.replace safe_locals subj expr
		(**
			Remove variable from the list of safe locals.
		*)
		method remove_from_safety ?(forever=false) expr =
			match self#get_subject expr with
				| SNotSuitable -> ()
				| subj ->
					Hashtbl.remove safe_locals subj;
					if forever then
						Hashtbl.replace never_safe subj expr
		(**
			Remove locals, which don't exist in `sample`, from safety.
		*)
		method filter_safety (sample:(safety_subject,texpr) Hashtbl.t) =
			Hashtbl.iter
				(fun subj expr ->
					if not (Hashtbl.mem sample subj) then
						Hashtbl.remove safe_locals subj
				)
				(Hashtbl.copy safe_locals);
		(**
			Should be called upon assigning a value to `expr`.
			Removes subjects like `expr.subField` from safety.
		*)
		method reassigned (expr:texpr) =
			match self#get_subject expr with
				| SNotSuitable -> ()
				| subj ->
					(*
						If this is an assignment to a field, drop all safe field accesses first,
						because it could alter an object of those field accesses.
					*)
					(match subj with
						| SFieldOfClass _ | SFieldOfLocalVar _ | SFieldOfThis _ -> self#drop_safe_fields_in_strict_mode
						| _ -> ()
					);
					let add_to_remove safe_subj safe_fields fields to_remove =
						if list_starts_with_list (List.rev safe_fields) (List.rev fields) then
							safe_subj :: to_remove
						else
							to_remove
					in
					let remove_list =
						Hashtbl.fold
							(fun safe_subj safe_expr to_remove ->
								match safe_subj, subj with
									| SFieldOfLocalVar (safe_id, _), SLocalVar v_id when safe_id = v_id ->
										safe_subj :: to_remove
									| SFieldOfLocalVar (safe_id, safe_fields), SFieldOfLocalVar (v_id, fields) when safe_id = v_id ->
										add_to_remove safe_subj safe_fields fields to_remove
									| SFieldOfClass (safe_path, safe_fields), SFieldOfClass (path, fields) when safe_path = path ->
										add_to_remove safe_subj safe_fields fields to_remove
									| SFieldOfClass (safe_path, safe_fields), SFieldOfClass (path, fields) when safe_path = path ->
										add_to_remove safe_subj safe_fields fields to_remove
									| SFieldOfThis safe_fields, SFieldOfThis fields ->
										add_to_remove safe_subj safe_fields fields to_remove
									| _ -> to_remove
							)
							safe_locals []
					in
					List.iter (Hashtbl.remove safe_locals) remove_list
		(**
			Should be called upon a call.
			In Strict mode making a call removes all field accesses from safety.
		*)
		method call_made =
			self#drop_safe_fields_in_strict_mode
		(**
			Un-safe all field accesses if safety mode is one of strict modes
		*)
		method private drop_safe_fields_in_strict_mode =
			match mode with
			| SMOff | SMLoose -> ()
			| SMStrict | SMStrictThreaded ->
				let remove_list =
					Hashtbl.fold
						(fun subj expr to_remove ->
							match subj with
							| SFieldOfLocalVar _ | SFieldOfClass _ | SFieldOfThis _ -> subj :: to_remove
							| _ -> to_remove
						)
						safe_locals []
				in
				List.iter (Hashtbl.remove safe_locals) remove_list
		(**
			Wrapper for `get_subject` function
		*)
		method get_subject =
			get_subject mode
	end

(**
	Class to simplify collecting lists of local vars, fields and other symbols checked against `null`.
*)
class local_safety (mode:safety_mode) =
	object (self)
		val mutable scopes = [new safety_scope mode STNormal (Hashtbl.create 100) (Hashtbl.create 100)]
		(**
			Drop collected data
		*)
		method clear =
			scopes <- [new safety_scope mode STNormal (Hashtbl.create 100) (Hashtbl.create 100)]
		(**
			Get the latest created scope.
		*)
		method private get_current_scope =
			match scopes with
				| current :: _-> current
				| [] -> fail ~msg:"List of scopes should never end." null_pos __POS__
		(**
			Get a copy of hashtable, which stores currently safe locals
		*)
		method get_safe_locals_copy =
			Hashtbl.copy (self#get_current_scope#get_safe_locals)
		(**
			Remove locals, which don't exist in `sample`, from safety.
		*)
		method filter_safety sample =
			self#get_current_scope#filter_safety sample
		(**
			Should be called upon local function declaration.
		*)
		method function_declared (immediate_execution:bool) (fn:tfunc) =
			let scope =
				if immediate_execution || mode = SMLoose then
					new safety_scope mode STImmediateClosure self#get_current_scope#get_safe_locals self#get_current_scope#get_never_safe
				else
					new safety_scope mode STClosure (Hashtbl.create 100) (Hashtbl.create 100)
			in
			scopes <- scope :: scopes;
			List.iter (fun (v, _) -> scope#declare_var v) fn.tf_args
		(**
			Should be called upon standalone block declaration.
		*)
		method block_declared =
			let scope = new safety_scope mode STNormal self#get_current_scope#get_safe_locals self#get_current_scope#get_never_safe in
			scopes <- scope :: scopes
		(**
			Should be called upon entering a loop.
		*)
		method loop_declared e =
			let scope = new safety_scope mode STLoop self#get_current_scope#get_safe_locals self#get_current_scope#get_never_safe in
			(* let scope = new safety_scope mode STLoop (Hashtbl.create 100) (Hashtbl.create 100) in *)
			scopes <- scope :: scopes;
			match e.eexpr with
				| TFor (v, _, _) -> scope#declare_var v
				| TWhile _ -> ()
				| _ -> fail ~msg:"Expected TFor or TWhile." e.epos __POS__
		(**
			Should be called upon leaving local function declaration.
		*)
		method scope_closed =
			match scopes with
				| [] -> fail ~msg:"No scopes left." null_pos __POS__
				| [scope] -> fail ~msg:"Cannot close the last scope." null_pos __POS__
				| _ :: rest -> scopes <- rest
		(**
			Should be called for each local var declared
		*)
		method declare_var ?(is_safe=false) (v:tvar) =
			let scope = self#get_current_scope in
			scope#declare_var v;
			if is_safe then scope#add_to_safety { eexpr = TVar (v, None); etype = v.v_type; epos = v.v_pos }
		(**
			Check if local variable is guaranteed to not have a `null` value.
		*)
		method is_safe expr =
			if not (is_nullable_type expr.etype) then
				true
			else
				let captured () =
					match expr.eexpr with
						| TLocal local_var ->
							let rec traverse scopes =
								match scopes with
									| [] -> false
									| current :: rest ->
										if current#owns_var local_var then
											false
										else if current#get_type = STClosure then
											true
										else
											traverse rest
							in
							traverse scopes
						| _ -> false
				in
				(mode = SMLoose || not (captured())) && self#get_current_scope#is_safe expr
		(**
			This method should be called upon passing `while`.
			It collects locals which are checked against `null` and executes callbacks for expressions with proper statuses of locals.
		*)
		method process_while expr is_nullable_expr (condition_callback:texpr->unit) (body_callback:(unit->unit)->texpr->unit) =
			match expr.eexpr with
				| TWhile (condition, body, DoWhile) ->
					let original_safe_locals = self#get_safe_locals_copy in
					condition_callback condition;
					let (_, not_nulls) = process_condition mode condition is_nullable_expr (fun _ -> ()) in
					body_callback
						(fun () ->
							List.iter
								(fun not_null ->
									match get_subject mode not_null with
										| SNotSuitable -> ()
										| subj ->
											if Hashtbl.mem original_safe_locals subj then
												self#get_current_scope#add_to_safety not_null
								)
								not_nulls
						)
						body
				| TWhile (condition, body, NormalWhile) ->
					condition_callback condition;
					let (nulls, not_nulls) = process_condition mode condition is_nullable_expr (fun _ -> ()) in
					(** execute `body` with known not-null variables *)
					List.iter self#get_current_scope#add_to_safety not_nulls;
					body_callback
						(fun () -> List.iter self#get_current_scope#add_to_safety not_nulls)
						body;
					List.iter self#get_current_scope#remove_from_safety not_nulls;
				| _ -> fail ~msg:"Expected TWhile" expr.epos __POS__
		(**
			Should be called for bodies of loops (for, while)
		*)
		method process_loop_body (first_check:unit->unit) (intermediate_action:(unit->unit) option) (second_check:unit->unit) =
			let original_safe_locals = self#get_safe_locals_copy in
			(** The first check to find out which vars will become unsafe in a loop *)
			first_check();
			(* If local var became safe in a loop, then we need to remove it from safety to make it unsafe outside of a loop again *)
			self#filter_safety original_safe_locals;
			Option.may (fun action -> action()) intermediate_action;
			(** The second check with unsafe vars removed from safety *)
			second_check()
		(**
			This method should be called upon passing `try`.
		*)
		method process_try (try_block:texpr) (catches:(tvar * texpr) list) (check_expr:texpr->unit) =
			let original_safe_locals = self#get_safe_locals_copy in
			check_expr try_block;
			(* Remove locals which became safe inside of a try block from safety *)
			self#filter_safety original_safe_locals;
			let safe_after_try = self#get_safe_locals_copy
			and safe_after_catches = self#get_safe_locals_copy in
			List.iter
				(fun (_, catch_block) ->
					self#get_current_scope#reset_to safe_after_try;
					check_expr catch_block;
					Hashtbl.iter
						(fun var_id v ->
							if not (self#is_safe v) then
								Hashtbl.remove safe_after_catches var_id
						)
						(Hashtbl.copy safe_after_catches)
				)
				catches;
			self#get_current_scope#reset_to safe_after_catches
		(**
			This method should be called upon passing `if`.
			It collects locals which are checked against `null` and executes callbacks for expressions with proper statuses of locals.
		*)
		method process_if expr is_nullable_expr (condition_callback:texpr->unit) (body_callback:texpr->unit) =
			match expr.eexpr with
				| TIf (condition, if_body, else_body) ->
					condition_callback condition;
					let (nulls_in_if, not_nulls) =
						process_condition mode condition is_nullable_expr (fun _ -> ())
					in
					(* Don't touch expressions, which already was safe before this `if` *)
					let filter = List.filter (fun e -> not (self#is_safe e)) in
					let not_nulls = filter not_nulls in
					let not_condition =
						{ eexpr = TUnop (Not, Prefix, condition); etype = condition.etype; epos = condition.epos }
					in
					let (_, else_not_nulls) =
						process_condition mode not_condition is_nullable_expr (fun _ -> ())
					in
					let else_not_nulls = filter else_not_nulls in
					let initial_safe = self#get_safe_locals_copy in
					(** execute `if_body` with known not-null variables *)
					List.iter self#get_current_scope#add_to_safety not_nulls;
					body_callback if_body;
					let safe_after_if = self#get_safe_locals_copy in
					(* List.iter self#get_current_scope#remove_from_safety not_nulls; *)
					self#get_current_scope#reset_to initial_safe;
					(** execute `else_body` with known not-null variables *)
					let handle_dead_end body safe_vars =
						if DeadEnd.has_dead_end body then
							List.iter self#get_current_scope#add_to_safety safe_vars
					in
					(match else_body with
						| None ->
							(*
								`if` gets executed only when each of `nulls_in_if` is `null`.
								That means if they become safe in `if`, then they are safe after `if` too.
							*)
							List.iter (fun e ->
								let subj = self#get_current_scope#get_subject e in
								if Hashtbl.mem safe_after_if subj then
									self#get_current_scope#add_to_safety e;
							) nulls_in_if;
							(* These became unsafe in `if` *)
							Hashtbl.iter (fun subj e ->
								if not (Hashtbl.mem safe_after_if subj) then
									self#get_current_scope#remove_from_safety e;
							) initial_safe;
							(** If `if_body` terminates execution, then bypassing `if` means `else_not_nulls` are safe now *)
							handle_dead_end if_body else_not_nulls
						| Some else_body ->
							List.iter self#get_current_scope#add_to_safety else_not_nulls;
							body_callback else_body;
							let safe_after_else = self#get_safe_locals_copy in
							self#get_current_scope#reset_to initial_safe;
							(* something was safe before `if..else`, but became unsafe in `if` or in `else` *)
							Hashtbl.iter (fun subj e ->
								if not (Hashtbl.mem safe_after_if subj && Hashtbl.mem safe_after_else subj) then
									self#get_current_scope#remove_from_safety e;
								Hashtbl.remove safe_after_if subj;
								Hashtbl.remove safe_after_else subj;
							) initial_safe;
							(* something became safe in both `if` and `else` *)
							Hashtbl.iter (fun subj e ->
								if Hashtbl.mem safe_after_else subj then
									self#get_current_scope#add_to_safety e
							) safe_after_if;
							(** If `if_body` terminates execution, then bypassing `if` means `else_not_nulls` are safe now *)
							handle_dead_end if_body else_not_nulls;
							(** If `else_body` terminates execution, then bypassing `else` means `not_nulls` are safe now *)
							handle_dead_end else_body not_nulls
					);
				| _ -> fail ~msg:"Expected TIf" expr.epos __POS__
		(**
			Handle boolean AND outside of `if` condition.
		*)
		method process_and left_expr right_expr is_nullable_expr (callback:texpr->unit) =
			callback left_expr;
			let (_, not_nulls) = process_condition mode left_expr is_nullable_expr (fun e -> ()) in
			List.iter self#get_current_scope#add_to_safety not_nulls;
			callback right_expr;
			List.iter self#get_current_scope#remove_from_safety not_nulls
		(**
			Handle boolean OR outside of `if` condition.
		*)
		method process_or left_expr right_expr is_nullable_expr (callback:texpr->unit) =
			let (nulls, _) = process_condition mode left_expr is_nullable_expr callback in
			List.iter self#get_current_scope#add_to_safety nulls;
			callback right_expr;
			List.iter self#get_current_scope#remove_from_safety nulls
		(**
			Remove subject from the safety list if a nullable value is assigned or if an object with safe field is reassigned.
		*)
		method handle_assignment is_nullable_expr left_expr (right_expr:texpr) =
			if is_suitable mode left_expr then
				self#get_current_scope#reassigned left_expr;
				if is_nullable_expr right_expr then
					match left_expr.eexpr with
						| TLocal v ->
							let captured = ref false in
							let rec traverse (lst:safety_scope list) =
								match lst with
									| [] -> ()
									| current :: rest ->
										if current#owns_var v then
											current#remove_from_safety ~forever:!captured left_expr
										else begin
											captured := !captured || current#get_type = STClosure;
											current#remove_from_safety ~forever:!captured left_expr;
											traverse rest
										end
							in
							traverse scopes
						| _ -> ()
				else if is_nullable_type left_expr.etype then
					self#get_current_scope#add_to_safety left_expr
		method call_made =
			self#get_current_scope#call_made
	end

(**
	This class is used to recursively check typed expressions for null-safety
*)
class expr_checker mode immediate_execution report =
	object (self)
		val local_safety = new local_safety mode
		val mutable return_types = []
		val mutable in_closure = false
		(* if this flag is `true` then spotted errors and warnings will not be reported *)
		val mutable is_pretending = false
		(* val mutable cnt = 0 *)
		(**
			Get safety mode for this expression checker
		*)
		method get_mode = mode
		(**
			Register an error
		*)
		method error msg (positions:Globals.pos list) =
			if not is_pretending then begin
				let rec get_first_valid_pos positions =
					match positions with
						| [] -> null_pos
						| p :: rest ->
							if p <> null_pos then p
							else get_first_valid_pos rest
				in
				add_error report msg (get_first_valid_pos positions)
			end
		(**
			Check if `e` is nullable even if the type is reported not-nullable.
			Haxe type system lies sometimes.
		*)
		method private is_nullable_expr e =
			let e = reveal_expr e in
			match e.eexpr with
				| TConst TNull -> true
				| TConst _ -> false
				| TParenthesis e -> self#is_nullable_expr e
				| TMeta (m, _) when contains_unsafe_meta [m] -> false
				| TMeta (_, e) -> self#is_nullable_expr e
				| TThrow _ -> false
				| TReturn (Some e) -> self#is_nullable_expr e
				| TBinop ((OpAssign | OpAssignOp _), _, right) -> self#is_nullable_expr right
				| TBlock exprs ->
					local_safety#block_declared;
					let rec traverse exprs =
						match exprs with
							| [] -> false
							| [e] -> self#is_nullable_expr e
							| e :: exprs ->
								(match e.eexpr with
									| TVar (v,_) -> local_safety#declare_var v
									| _ -> ()
								);
								traverse exprs
					in
					let is_nullable = traverse exprs in
					local_safety#scope_closed;
					is_nullable
					(* (match exprs with
						| [] -> false
						| _ -> self#is_nullable_expr (List.hd (List.rev exprs))
					) *)
				| TIf _ ->
					let nullable = ref false in
					let check body = nullable := !nullable || self#is_nullable_expr body in
					local_safety#process_if e self#is_nullable_expr (fun _ -> ()) check;
					!nullable
				| _ ->
					is_nullable_type e.etype && not (local_safety#is_safe e)
		(**
			Check if `expr` can be passed to a place where `to_type` is expected.
			This method has side effect: it logs an error if `expr` has a type parameter incompatible with the type parameter of `to_type`.
			E.g.: `Array<Null<String>>` vs `Array<String>` returns `true`, but also adds a compilation error.
		*)
		method can_pass_expr expr to_type p =
			match expr.eexpr, to_type with
				| TLocal v, _ when contains_unsafe_meta v.v_meta -> true
				| TObjectDecl fields, TAnon to_type ->
					List.for_all
						(fun ((name, _, _), field_expr) ->
							try
								let field_to_type = PMap.find name to_type.a_fields in
								self#can_pass_expr field_expr field_to_type.cf_type p
							with Not_found -> false)
						fields
				| _, _ ->
					if self#is_nullable_expr expr && not (is_nullable_type ~dynamic_is_nullable:true to_type) then
						false
					else begin
						let expr_type = unfold_null expr.etype in
						try
							new unificator#unify expr_type to_type;
							true
						with
							| Safety_error err ->
								self#error ("Cannot unify " ^ (str_type expr_type) ^ " with " ^ (str_type to_type)) [p; expr.epos];
								(* returning `true` because error is already logged in the line above *)
								true
							| e ->
								fail ~msg:"Null safety unification failure" expr.epos __POS__
					end
		(**
			Should be called for the root expressions of a method or for then initialization expressions of fields.
		*)
		method check_root_expr e =
			self#check_expr e;
			local_safety#clear;
			return_types <- [];
			in_closure <- false
		(**
			Recursively checks an expression
		*)
		method private check_expr e =
			match e.eexpr with
				| TConst _ -> ()
				| TLocal _ -> ()
				| TArray (arr, idx) -> self#check_array_access arr idx e.epos
				| TBinop (op, left_expr, right_expr) -> self#check_binop op left_expr right_expr e.epos
				| TField (target, access) -> self#check_field target access e.epos
				| TTypeExpr _ -> ()
				| TParenthesis e -> self#check_expr e
				| TObjectDecl fields -> List.iter (fun (_, e) -> self#check_expr e) fields
				| TArrayDecl items -> self#check_array_decl items e.etype e.epos
				| TCall (callee, args) -> self#check_call callee args e.epos
				| TNew _ -> self#check_new e
				| TUnop (_, _, expr) -> self#check_unop expr e.epos
				| TFunction fn -> self#check_function fn
				| TVar (v, init_expr) -> self#check_var v init_expr e.epos
				| TBlock exprs -> self#check_block exprs e.epos
				| TFor _ -> self#check_for e
				| TIf _ -> self#check_if e
				| TWhile _ -> self#check_while e
				| TSwitch (target, cases, default) -> self#check_switch target cases default e.epos
				| TTry (try_block, catches) -> self#check_try try_block catches
				| TReturn (Some expr) -> self#check_return expr e.epos
				| TReturn None -> ()
				| TBreak -> ()
				| TContinue -> ()
				| TThrow expr -> self#check_throw expr e.epos
				| TCast (expr, _) -> self#check_cast expr e.etype e.epos
				| TMeta (m, _) when contains_unsafe_meta [m] -> ()
				| TMeta ((Meta.NullSafety, _, _) as m, e) -> validate_safety_meta report [m]; self#check_expr e
				| TMeta (_, e) -> self#check_expr e
				| TEnumIndex idx -> self#check_enum_index idx e.epos
				| TEnumParameter (e, _, _) -> self#check_expr e (** Checking enum value itself is not needed here because this expr always follows after TEnumIndex *)
				| TIdent _ -> ()
		(**
			Check expressions in a block
		*)
		method private check_block exprs p =
			local_safety#block_declared;
			let rec traverse exprs =
				match exprs with
					| [] -> ()
					| e :: rest ->
						self#check_expr e;
						traverse rest
			in
			traverse exprs;
			local_safety#scope_closed
		(**
			Don't allow to use nullable values as items in declaration of not-nullable arrays
		*)
		method private check_array_decl items arr_type p =
			(match Abstract.follow_with_abstracts arr_type with
				| TInst ({ cl_path = ([], "Array") }, [item_type]) ->
					List.iter
						(fun e ->
							if not (self#can_pass_expr e item_type e.epos) then
								self#error ("Cannot use nullable value of " ^ (str_type e.etype) ^ " as an item in Array<" ^ (str_type item_type) ^ ">") [e.epos; p]
						)
						items;
				| _ -> ()
			);
			List.iter self#check_expr items
		(**
			Deal with nullable enum values
		*)
		method private check_enum_index idx p =
			if self#is_nullable_expr idx then
				self#error "Cannot access nullable enum value." [idx.epos; p];
			self#check_expr idx
		(**
			Check try...catch
		*)
		method private check_try try_block catches =
			local_safety#process_try try_block catches self#check_expr
		(**
			Don't use nullable value as a condition in `while`
		*)
		method private check_while e =
			match e.eexpr with
				| TWhile _ ->
					let check_condition condition =
						if self#is_nullable_expr condition then
							self#error "Cannot use nullable value as a condition in \"while\"." [condition.epos; e.epos];
						self#check_expr condition
					in
					local_safety#loop_declared e;
					local_safety#process_while
						e
						self#is_nullable_expr
						check_condition
						(* self#check_loop_body; *)
						(fun handle_condition_effect body ->
							self#check_loop_body
								(Some handle_condition_effect)
								body
						);
					local_safety#scope_closed
				| _ -> fail ~msg:"Expected TWhile." e.epos __POS__
		(**
			Don't iterate on nullable values
		*)
		method private check_for e =
			match e.eexpr with
				| TFor (v, iterable, body) ->
					if self#is_nullable_expr iterable then
						self#error "Cannot iterate over nullable value." [iterable.epos; e.epos];
					self#check_expr iterable;
					local_safety#declare_var v;
					local_safety#loop_declared e;
					self#check_loop_body None body;
					local_safety#scope_closed
				| _ -> fail ~msg:"Expected TFor." e.epos __POS__
		(**
			Handle safety inside of loops
		*)
		method private check_loop_body (handle_condition_effect:(unit->unit) option) body =
			local_safety#process_loop_body
				(* Start pretending to ignore errors *)
				(fun () ->
					is_pretending <- true;
					self#check_expr body
				)
				handle_condition_effect
				(* Now we know, which vars will become unsafe in this loop. Stop pretending and check again *)
				(fun () ->
					is_pretending <- false;
					self#check_expr body;
				)
		(**
			Don't throw nullable values
		*)
		method private check_throw e p =
			if self#is_nullable_expr e then
				self#error "Cannot throw nullable value." [p; e.epos];
			self#check_expr e
		(**
			Don't cast nullable expressions to not-nullable types
		*)
		method private check_cast expr to_type p =
			self#check_expr expr;
			match to_type with
				(* untyped cast *)
				| TMono _ -> ()
				(* typed cast and type check *)
				| _ ->
					if not (self#can_pass_expr expr to_type p) then
						self#error "Cannot cast nullable value to not nullable type." [p; expr.epos]
		(**
			Check safety in a function
		*)
		method private check_function ?(immediate_execution=false) fn =
			local_safety#function_declared immediate_execution fn;
			return_types <- fn.tf_type :: return_types;
			if immediate_execution || mode = SMLoose then
				begin
					let original_safe_locals = local_safety#get_safe_locals_copy in
					(* Start pretending to ignore errors *)
					is_pretending <- true;
					self#check_expr fn.tf_expr;
					(* Now we know, which vars will become unsafe in this closure. Stop pretending and perform real check *)
					is_pretending <- false;
					local_safety#filter_safety original_safe_locals;
					self#check_expr fn.tf_expr
				end
			else
				self#check_expr fn.tf_expr;
			return_types <- List.tl return_types;
			local_safety#scope_closed
		(**
			Don't return nullable values as not-nullable return types.
		*)
		method private check_return e p =
			self#check_expr e;
			match return_types with
				| t :: _ when not (self#can_pass_expr e t p) ->
					self#error ("Cannot return nullable value of " ^ (str_type e.etype) ^ " as " ^ (str_type t)) [p; e.epos]
				| _ -> ()
		(**
			Check safety in `switch` expressions.
		*)
		method private check_switch target cases default p =
			if self#is_nullable_expr target then
				self#error "Cannot switch on nullable value." [target.epos; p];
			self#check_expr target;
			let rec traverse_cases cases =
				match cases with
					| [] -> ()
					| (_, body) :: rest ->
						self#check_expr body;
						traverse_cases rest
			in
			traverse_cases cases;
			match default with
				| None -> ()
				| Some e -> self#check_expr e
		(**
			Check safety in `if` expressions
		*)
		method private check_if expr =
			let check_condition e =
				if self#is_nullable_expr e then
					self#error "Cannot use nullable value as condition in \"if\"." [e.epos; expr.epos];
				self#check_expr e
			in
			local_safety#process_if expr self#is_nullable_expr check_condition self#check_expr
		(**
			Check array access on nullable values or using nullable indexes
		*)
		method private check_array_access arr idx p =
			if self#is_nullable_expr arr then
				self#error "Cannot perform array access on nullable value." [p; arr.epos];
			if self#is_nullable_expr idx then
				self#error "Cannot use nullable value as an index for array access." [p; idx.epos];
			self#check_expr arr;
			self#check_expr idx
		(**
			Don't perform unsafe binary operations
		*)
		method private check_binop op left_expr right_expr p =
			let check_both () =
				self#check_expr left_expr;
				self#check_expr right_expr
			in
			match op with
				| OpEq | OpNotEq -> check_both()
				| OpBoolAnd ->
					local_safety#process_and left_expr right_expr self#is_nullable_expr self#check_expr
				| OpBoolOr ->
					local_safety#process_or left_expr right_expr self#is_nullable_expr self#check_expr
				(* String concatenation is safe if one of operands is safe *)
				| OpAdd
				| OpAssignOp OpAdd when is_string_type left_expr.etype || is_string_type right_expr.etype  ->
					check_both();
					if is_nullable_type left_expr.etype && is_nullable_type right_expr.etype then
						self#error "Cannot concatenate two nullable values." [p; left_expr.epos; right_expr.epos]
				| OpAssign ->
					check_both();
					if not (self#can_pass_expr right_expr left_expr.etype p) then
						match left_expr.eexpr with
						| TLocal v when contains_unsafe_meta v.v_meta -> ()
						| _ ->
							self#error "Cannot assign nullable value here." [p; right_expr.epos; left_expr.epos]
					else
						local_safety#handle_assignment self#is_nullable_expr left_expr right_expr;
				| _->
					if self#is_nullable_expr left_expr || self#is_nullable_expr right_expr then
						self#error "Cannot perform binary operation on nullable value." [p; left_expr.epos; right_expr.epos];
					check_both()
		(**
			Don't perform unops on nullable values
		*)
		method private check_unop e p =
			if self#is_nullable_expr e then
				self#error "Cannot perform unary operation on nullable value." [p; e.epos];
			self#check_expr e
		(**
			Don't assign nullable value to not-nullable variable on var declaration
		*)
		method private check_var v init p =
			local_safety#declare_var v;
			match init with
				| None -> ()
				(* Local named functions like `function fn() {}`, which are generated as `var fn = null; fn = function(){}` *)
				| Some { eexpr = TConst TNull } when v.v_kind = VUser TVOLocalFunction -> ()
				(* `_this = null` is generated for local `inline function` *)
				(* | Some { eexpr = TConst TNull } when v.v_kind = VGenerated -> () *)
				| Some e ->
					let local = { eexpr = TLocal v; epos = v.v_pos; etype = v.v_type } in
					self#check_binop OpAssign local e p
		(**
			Make sure nobody tries to access a field on a nullable value
		*)
		method private check_field target access p =
			self#check_expr target;
			if self#is_nullable_expr target then
				self#error ("Cannot access \"" ^ accessed_field_name access ^ "\" of a nullable value.") [p; target.epos];
		(**
			Check constructor invocation: don't pass nullable values to not-nullable arguments
		*)
		method private check_new e_new =
			match e_new.eexpr with
				| TNew (cls, params, args) ->
					let ctor =
						try
							Some (get_constructor cls)
						with
							| Not_found -> None
					in
					(match ctor with
						| None ->
							List.iter self#check_expr args
						| Some cf ->
							let rec traverse t =
								match follow t with
									| TFun (types, _) -> self#check_args e_new args types
									| _ -> fail ~msg:"Unexpected constructor type." e_new.epos __POS__
							in
							let ctor_type = apply_params cls.cl_params params cf.cf_type in
							traverse ctor_type
					)
				| _ -> fail ~msg:"TNew expected" e_new.epos __POS__
		(**
			Check calls: don't call a nullable value, dont' pass nulable values to not-nullable arguments
		*)
		method private check_call callee args p =
			if self#is_nullable_expr callee then
				self#error "Cannot call a nullable value." [callee.epos; p];
			(match callee.eexpr with
				| TFunction fn | TParenthesis { eexpr = TFunction fn } ->
					self#check_function ~immediate_execution:true fn
				| _ ->
					self#check_expr callee
			);
			(match follow callee.etype with
				| TFun (types, _) ->
					if is_trace callee then
						let real_args =
							match List.rev args with
								| { eexpr = TObjectDecl fields } :: [first_arg] ->
									(try
										let arr =
											snd (List.find (fun ((name, _, _), _) -> name = "customParams") fields)
										in
										match arr.eexpr with
											| TArrayDecl rest_args -> first_arg :: rest_args
											| _ -> args
									with Not_found -> args
									)
								| _ -> args
						in
						List.iter self#check_expr real_args
					else begin
						self#check_args callee args types
					end
				| _ ->
					List.iter self#check_expr args
			);
			local_safety#call_made
		(**
			Check if specified expressions can be passed to a call which expects `types`.
		*)
		method private check_args callee args types =
			let rec traverse arg_num args types meta =
				match (args, types, meta) with
					| (arg :: args, (arg_name, optional, t) :: types, arg_meta :: meta) ->
						let unsafe_argument = contains_unsafe_meta arg_meta in
						if
							not optional && not unsafe_argument
							&& not (self#can_pass_expr arg t arg.epos)
						then begin
							let fn_str = match symbol_name callee with "" -> "" | name -> " of function \"" ^ name ^ "\""
							and arg_str = if arg_name = "" then "" else " \"" ^ arg_name ^ "\"" in
							self#error ("Cannot pass nullable value to not-nullable argument" ^ arg_str ^ fn_str ^ ".") [arg.epos; callee.epos]
						end;
						(match arg.eexpr with
							| TFunction fn ->
								self#check_function ~immediate_execution:(immediate_execution#check callee arg_num) fn
							| TCast(e,None) when unsafe_argument && fast_eq arg.etype t ->
								self#check_expr e
							| _ ->
								self#check_expr arg
						);
						traverse (arg_num + 1) args types meta;
					| _ -> ()
			in
			let meta = get_arguments_meta callee (List.length types) in
			traverse 0 args types meta
	end

class class_checker cls immediate_execution report =
	let cls_meta = cls.cl_meta @ (match cls.cl_kind with KAbstractImpl a -> a.a_meta | _ -> []) in
	object (self)
			val is_safe_class = (safety_enabled cls_meta)
			val mutable checker = new expr_checker SMLoose immediate_execution report
			val mutable mode = None
		(**
			Entry point for checking a class
		*)
		method check =
			validate_safety_meta report cls_meta;
			if is_safe_class && (not (has_class_flag cls CExtern)) && (not (has_class_flag cls CInterface)) then
				self#check_var_fields;
			let check_field is_static f =
				validate_safety_meta report f.cf_meta;
				match (safety_mode (cls_meta @ f.cf_meta)) with
					| SMOff -> ()
					| mode ->
						(match f.cf_expr with
							| None -> ()
							| Some expr ->
								(self#get_checker mode)#check_root_expr expr
						);
						self#check_accessors is_static f
			in
			if is_safe_class then
				Option.may ((self#get_checker (safety_mode cls_meta))#check_root_expr) cls.cl_init;
			Option.may (check_field false) cls.cl_constructor;
			List.iter (check_field false) cls.cl_ordered_fields;
			List.iter (check_field true) cls.cl_ordered_statics;
		(**
			Check if a getter/setter for non-nullable property return safe values.
			E.g.
			```
			var str(get,never):String;
			function get_str() return (null:Null<String>); //should fail null safety check
			```
		*)
		method private check_accessors is_static field =
			match field.cf_kind with
				| Var { v_read = read_access; v_write = write_access } when not (is_nullable_type field.cf_type) ->
					let fields = if is_static then cls.cl_statics else cls.cl_fields in
					let check_accessor prefix =
						let accessor =
							try Some (PMap.find (prefix ^ field.cf_name) fields)
							with Not_found -> None
						in
						match accessor with
							| None -> ()
							| Some accessor ->
								if self#is_in_safety accessor then
									match accessor.cf_expr with
										| Some ({ eexpr = TFunction fn } as accessor_expr) ->
											let fn = { fn with tf_type = field.cf_type } in
											(self#get_checker self#class_safety_mode)#check_root_expr { accessor_expr with eexpr = TFunction fn }
										| _ -> ()
					in
					if read_access = AccCall then check_accessor "get_";
					if write_access = AccCall then check_accessor "set_"
				| _ -> ()
		(**
			Get safety mode for the current class
		*)
		method private class_safety_mode =
			match mode with
				| Some mode -> mode
				| None ->
					let m = safety_mode cls_meta in
					mode <- Some m;
					m
		(**
			Get an instance of expression checker with safety mode set to `mode`
		*)
		method private get_checker mode =
			if checker#get_mode <> mode then
				checker <- new expr_checker mode immediate_execution report;
			checker
		(**
			Check if field should be checked by null safety
		*)
		method private is_in_safety field =
			(is_safe_class && not (contains_unsafe_meta field.cf_meta)) || safety_enabled field.cf_meta
		(**
			Check `var` fields are initialized properly
		*)
		method check_var_fields =
			let check_field is_static field =
				validate_safety_meta report field.cf_meta;
				if should_be_initialized field then
					if not (is_nullable_type field.cf_type) && self#is_in_safety field then
						match field.cf_expr with
							| None ->
								if is_static then
									checker#error
										("Field \"" ^ field.cf_name ^ "\" is not nullable thus should have an initial value.")
										[field.cf_pos]
							| Some e ->
								if not (checker#can_pass_expr e field.cf_type e.epos) then
									checker#error ("Cannot set nullable initial value for not-nullable field \"" ^ field.cf_name ^ "\".") [field.cf_pos]
			in
			List.iter (check_field false) cls.cl_ordered_fields;
			List.iter (check_field true) cls.cl_ordered_statics;
			self#check_fields_initialization_in_constructor ()
		(**
			Check instance fields without initial values are properly initialized in constructor
		*)
		method private check_fields_initialization_in_constructor () =
			let fields_to_initialize = Hashtbl.create 20
			(* Compiler-autogenerated local vars for transfering `this` to local functions *)
			and this_vars = Hashtbl.create 5 in
			List.iter
				(fun f ->
					if
						should_be_initialized f
						&& not (is_nullable_type f.cf_type)
						&& not (contains_unsafe_meta f.cf_meta)
					then
						match f.cf_expr with
							| Some _ -> ()
							| None -> Hashtbl.add fields_to_initialize f.cf_name f
				)
				cls.cl_ordered_fields;
			let rec check_unsafe_usage init_list safety_enabled e =
				if Hashtbl.length init_list > 0 then
					match e.eexpr with
						| TField ({ eexpr = TConst TThis }, FInstance (_, _, field)) ->
							if Hashtbl.mem init_list field.cf_name then
								checker#error ("Cannot use field " ^ field.cf_name ^ " until initialization.") [e.epos]
						| TField ({ eexpr = TConst TThis }, FClosure (_, field)) ->
							checker#error ("Cannot use method " ^ field.cf_name ^ " until all instance fields are initialized.") [e.epos];
						| TCall ({ eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, field)) }, args) ->
							checker#error ("Cannot call method " ^ field.cf_name ^ " until all instance fields are initialized.") [e.epos];
							List.iter (check_unsafe_usage init_list safety_enabled) args
						| TConst TThis when safety_enabled ->
							checker#error "Cannot use \"this\" until all instance fields are initialized." [e.epos]
						| TLocal v when safety_enabled && Hashtbl.mem this_vars v.v_id ->
							checker#error "Cannot use \"this\" until all instance fields are initialized." [e.epos]
						| TMeta ((Meta.NullSafety, [(EConst (Ident "Off"), _)], _), e) ->
							iter (check_unsafe_usage init_list false) e
						| TMeta ((Meta.NullSafety, _, _), e) ->
							iter (check_unsafe_usage init_list true) e
						| _ ->
							iter (check_unsafe_usage init_list safety_enabled) e
			in
			let rec traverse init_list e =
				(match e.eexpr with
					| TBinop (OpAssign, { eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, f)) }, right_expr) ->
						Hashtbl.remove init_list f.cf_name;
						ignore (traverse init_list right_expr)
					| TWhile (condition, body, DoWhile) ->
						check_unsafe_usage init_list true condition;
						ignore (traverse init_list body)
					| TBlock exprs ->
						List.iter (fun e -> ignore (traverse init_list e)) exprs
					| TIf (_, if_block, Some else_block) ->
						let if_init_list = traverse (Hashtbl.copy init_list) if_block
						and else_init_list = traverse (Hashtbl.copy init_list) else_block in
						Hashtbl.clear init_list;
						Hashtbl.iter (Hashtbl.replace init_list) if_init_list;
						Hashtbl.iter (Hashtbl.replace init_list) else_init_list
					(* var _gthis = this *)
					| TVar (v, Some { eexpr = TConst TThis }) ->
						Hashtbl.add this_vars v.v_id v
					| _ ->
						check_unsafe_usage init_list true e
				);
				init_list
			in
			(match cls.cl_constructor with
				| Some { cf_expr = Some { eexpr = TFunction { tf_expr = e } } } ->
					ignore (traverse fields_to_initialize e);
				| _ -> ()
			);
			Hashtbl.iter
				(fun name field ->
					checker#error
						("Field \"" ^ name ^ "\" is not nullable thus should have an initial value or should be initialized in constructor.")
						[field.cf_pos]
				)
				fields_to_initialize
	end

(**
	Run null safety checks.
*)
let run (com:Common.context) (types:module_type list) =
	let timer = Timer.timer ["null safety"] in
	let report = { sr_errors = [] } in
	let immediate_execution = new immediate_execution in
	let rec traverse module_type =
		match module_type with
			| TEnumDecl enm -> ()
			| TTypeDecl typedef -> ()
			| TAbstractDecl abstr -> ()
			| TClassDecl cls -> (new class_checker cls immediate_execution report)#check
	in
	List.iter traverse types;
	timer();
	match com.callbacks#get_null_safety_report with
		| [] ->
			List.iter (fun err -> com.error err.sm_msg err.sm_pos) (List.rev report.sr_errors)
		| callbacks ->
			let errors =
				List.map (fun err -> (err.sm_msg, err.sm_pos)) report.sr_errors
			in
			List.iter (fun fn -> fn errors) callbacks

;;
