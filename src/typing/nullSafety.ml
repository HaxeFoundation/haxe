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

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue in compiler repo.
*)
let fail ?msg hxpos mlpos =
	let msg =
		(Lexer.get_error_pos (Printf.sprintf "%s:%d:") hxpos) ^ ": "
		^ "Haxe-safety: " ^ (match msg with Some msg -> msg | _ -> "unexpected expression.") ^ "\n"
		^ "Submit an issue to https://github.com/RealyUniqueName/Safety/issues with expression example and following information:"
	in
	match mlpos with
		| (file, line, _, _) ->
			Printf.eprintf "%s\n" msg;
			Printf.eprintf "%s:%d\n" file line;
			assert false

(**
	Returns human-readable string representation of specified type
*)
let str_type t = s_type (print_context()) t

(**
	Check for explicit `Null<>` typing
*)
let rec is_nullable_type = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_nullable_type t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		true
	| TAbstract (a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
		is_nullable_type (apply_params a.a_params tl a.a_this)
	| TLazy f ->
		is_nullable_type (lazy_type f)
	| TType (t,tl) ->
		is_nullable_type (apply_params t.t_params tl t.t_type)
	| _ ->
		false

(**
	If `expr` is a TCast or TMeta, returns underlying expression (recursively bypassing nested casts).
	Otherwise returns `expr` as is.
*)
let rec reveal_expr expr =
	match expr.eexpr with
		| TCast (e, _) -> reveal_expr e
		| TMeta (_, e) -> reveal_expr e
		| _ -> expr

(**
	Try to get a human-readable representation of an `expr`
*)
let symbol_name expr =
	match (reveal_expr expr).eexpr with
		| TField (_, access) -> field_name access
		| TIdent name -> name
		| TLocal { v_name = name } -> name
		| TNew _ -> "new"
		| _ -> ""

(**
	Check if it's possible to pass a value of type `a` to a place where a value of type `b` is expected.
	Raises `Safety_error` exception if it's not.
*)
let rec unify a b =
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
				List.iter2 unify a_params b_params
			| TAnon a_anon, TAnon b_anon ->
				unify_anon_to_anon a_anon b_anon
			| TInst (a_cls, a_params), TAnon b_anon ->
				unify_class_to_anon a_cls a_params b_anon
			| TFun a_signature, TFun b_signature ->
				unify_functions a_signature b_signature
			(* patterns below are used to reveal real type *)
			| TLazy f, _ ->
				unify (lazy_type f) b
			| _, TLazy f -> unify a (lazy_type f)
			| TMono t, _ ->
				(match !t with None -> () | Some t -> unify t b)
			| _, TMono t ->
				(match !t with None -> () | Some t -> unify a t)
			| TType (t,tl), _ ->
				unify (apply_params t.t_params tl t.t_type) b
			| _, TType (t,tl) ->
				unify a (apply_params t.t_params tl t.t_type)
			| TAbstract (abstr,tl), _ when not (Meta.has Meta.CoreType abstr.a_meta) ->
				unify (apply_params abstr.a_params tl abstr.a_this) b
			| _, TAbstract (abstr,tl) when not (Meta.has Meta.CoreType abstr.a_meta) ->
				unify a (apply_params abstr.a_params tl abstr.a_this)
			| _ ->
				()

and unify_anon_to_anon (a:tanon) (b:tanon) =
	PMap.iter
		(fun name b_field ->
			let a_field =
				try Some (PMap.find name a.a_fields)
				with Not_found -> None
			in
			match a_field with
				| None -> ()
				| Some a_field -> unify a_field.cf_type b_field.cf_type
		)
		b.a_fields

and unify_class_to_anon (a:tclass) (a_params:tparams) (b:tanon) =
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
					unify a_type b_field.cf_type
		)
		b.a_fields

and unify_functions (a_args, a_result) (b_args, b_result) =
	(* check return type *)
	unify a_result b_result;
	(* check arguments *)
	List.iter2
		(fun (_, _, a_arg) (_, _, b_arg) -> unify b_arg a_arg)
		a_args
		b_args


(**
	Check if provided type is `Unsafe<T>`
*)
let is_special_type_unsafe t =
	match t with
		| TType ({ t_path = (["haxe"], "Unsafe") }, _) -> true
		| _ -> false

(**
	Checks if execution of provided expression is guaranteed to be terminated with `return`, `throw`, `break` or `continue`.
*)
let rec is_dead_end e =
	match e.eexpr with
		| TThrow _ -> true
		| TReturn _ -> true
		| TBreak -> true
		| TContinue -> true
		| TWhile (_, body, DoWhile) -> is_dead_end body
		| TIf (_, if_body, Some else_body) -> is_dead_end if_body && is_dead_end else_body
		| TBlock exprs -> List.exists is_dead_end exprs
		| TMeta (_, e) -> is_dead_end e
		| TCast (e, _) -> is_dead_end e
		| _ -> false

(**
	If `t` represents `Null<SomeType>` this function returns `SomeType`.
*)
let rec unfold_null t =
	match t with
		| TMono r -> (match !r with None -> t | Some t -> unfold_null t)
		| TAbstract ({ a_path = ([],"Null") }, [t]) -> unfold_null t
		| TLazy f -> unfold_null (lazy_type f)
		| TType (t,tl) -> unfold_null (apply_params t.t_params tl t.t_type)
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

let rec can_pass_type src dst =
	if is_nullable_type src && not (is_nullable_type dst) then
		false
	else
		(* TODO *)
		match dst with
			| TMono r -> (match !r with None -> true | Some t -> can_pass_type src t)
			| TEnum (_, params) -> true
			| TInst _ -> true
			| TType (t, tl) -> can_pass_type src (apply_params t.t_params tl t.t_type)
			| TFun _ -> true
			| TAnon _ -> true
			| TDynamic _ -> true
			| TLazy _ -> true
			| TAbstract ({ a_path = ([],"Null") }, [t]) -> true
			| TAbstract _ -> true

(**
	Collect nullable local vars which are checked against `null`.
	Returns a tuple of (vars_checked_to_be_null * vars_checked_to_be_not_null) in case `condition` evaluates to `true`.
*)
let process_condition condition (is_nullable_expr:texpr->bool) callback =
	let nulls = ref []
	and not_nulls = ref [] in
	let add to_nulls v =
		if to_nulls then nulls := v :: !nulls
		else not_nulls := v :: !not_nulls
	in
	let rec traverse positive e =
		match e.eexpr with
			| TUnop (Not, Prefix, e) -> traverse (not positive) e
			| TBinop (OpEq, { eexpr = TConst TNull }, { eexpr = TLocal v })
			| TBinop (OpEq, { eexpr = TLocal v }, { eexpr = TConst TNull }) ->
				add positive v
			| TBinop (OpNotEq, { eexpr = TConst TNull }, { eexpr = TLocal v })
			| TBinop (OpNotEq, { eexpr = TLocal v }, { eexpr = TConst TNull }) ->
				add (not positive) v
			| TBinop (OpEq, e, { eexpr = TLocal v }) when not (is_nullable_expr e) ->
				if positive then not_nulls := v :: !not_nulls
			| TBinop (OpEq, { eexpr = TLocal v }, e) when not (is_nullable_expr e) ->
				if positive then not_nulls := v :: !not_nulls
			| TBinop (OpBoolAnd, left_expr, right_expr) when positive ->
				traverse positive left_expr;
				traverse positive right_expr
			| TBinop (OpBoolOr, left_expr, right_expr) when not positive ->
				traverse positive left_expr;
				traverse positive right_expr
			| TParenthesis e -> traverse positive e
			| _ -> callback e
	in
	traverse true condition;
	(!nulls, !not_nulls)

(**
	Check if metadata contains @:nullSafety(false) meta
**)
let rec contains_unsafe_meta metadata =
	match metadata with
		| [] -> false
		| (Meta.NullSafety, [(EConst (Ident "false"), _)], _) :: _  -> true
		| _ :: rest -> contains_unsafe_meta rest

(**
	Check if metadata contains @:nullSafety or @:nullSafety(true) meta
**)
let rec contains_safe_meta metadata =
	match metadata with
		| [] -> false
		| (Meta.NullSafety, [], _) :: _
		| (Meta.NullSafety, [(EConst (Ident "true"), _)], _) :: _  -> true
		| _ :: rest -> contains_safe_meta rest

(**
	Check if `haystack` starts with `needle`
**)
let starts_with (haystack:string) (needle:string) :bool =
	(String.length haystack >= String.length needle)
	&& (needle = String.sub haystack 0 (String.length needle))

(**
	Check if specified `path` is mentioned in `--macro nullSafety(dotPath)`
*)
let need_check_path safe_paths type_path =
	let class_path =
		match type_path with
			| ([], name) -> name
			| (pack, name) -> (String.concat "." pack) ^ "." ^ name
	in
	let rec traverse paths =
		match paths with
			| [] -> false
			| current :: rest ->
				if current = "" || starts_with class_path current then
					true
				else
					traverse rest
	in
	traverse safe_paths

(**
	Check if specified `field` represents a `var` field which will exist at runtime.
*)
let should_be_initialized field =
	match field.cf_kind with
		| Var { v_read = AccNormal | AccInline | AccNo } | Var { v_write = AccNormal | AccNo } -> true
		| Var _ -> Meta.has Meta.IsVar field.cf_meta
		| _ -> false

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
							| _, ({ cf_expr = (Some { eexpr = TFunction fn }) } as field) ->
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
	Each loop or function should have its own scope.
*)
class safety_scope (scope_type:scope_type) (safe_locals:(int,tvar) Hashtbl.t) (never_safe:(int,tvar) Hashtbl.t) =
	object (self)
		(** Local vars declared in current scope *)
		val declarations = Hashtbl.create 100
		method get_safe_locals = safe_locals
		method get_never_safe = never_safe
		method get_type = scope_type
		(**
			Reset local vars safety to the specified state
		*)
		method reset_to (state:(int,tvar) Hashtbl.t) =
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
		method is_safe local_var =
			not (Hashtbl.mem never_safe local_var.v_id)
			&& (
				Hashtbl.mem safe_locals local_var.v_id
				|| not (is_nullable_type local_var.v_type)
			)
		(**
			Add variable to the list of safe locals.
		*)
		method add_to_safety v =
			Hashtbl.replace safe_locals v.v_id v
		(**
			Remove variable from the list of safe locals.
		*)
		method remove_from_safety ?(forever=false) v =
			Hashtbl.remove safe_locals v.v_id;
			if forever then
				Hashtbl.replace never_safe v.v_id v
		(**
			Remove locals, which don't exist in `sample`, from safety.
		*)
		method filter_safety (sample:(int,tvar) Hashtbl.t) =
			Hashtbl.iter
				(fun var_id v ->
					if not (Hashtbl.mem sample var_id) then
						self#remove_from_safety v
				)
				(Hashtbl.copy safe_locals);
	end

(**
	Class to simplify collecting lists of local vars checked against `null`.
*)
class local_vars =
	object (self)
		val mutable scopes = [new safety_scope STNormal (Hashtbl.create 100) (Hashtbl.create 100)]
		(**
			Drop collected data
		*)
		method clear =
			scopes <- [new safety_scope STNormal (Hashtbl.create 100) (Hashtbl.create 100)]
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
			Should be called upon local function declaration.
		*)
		method function_declared (immediate_execution:bool) (fn:tfunc) =
			let scope =
				if immediate_execution then
					new safety_scope STImmediateClosure self#get_current_scope#get_safe_locals self#get_current_scope#get_never_safe
				else
					new safety_scope STClosure (Hashtbl.create 100) (Hashtbl.create 100)
			in
			scopes <- scope :: scopes;
			List.iter (fun (v, _) -> scope#declare_var v) fn.tf_args
		(**
			Should be called upon entering a loop.
		*)
		method loop_declared e =
			let scope = new safety_scope STLoop self#get_current_scope#get_safe_locals self#get_current_scope#get_never_safe in
			(* let scope = new safety_scope STLoop (Hashtbl.create 100) (Hashtbl.create 100) in *)
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
			if is_safe then scope#add_to_safety v
		(**
			Check if local variable is guaranteed to not have a `null` value.
		*)
		method is_safe local_var =
			if not (is_nullable_type local_var.v_type) then
				true
			else
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
				let captured = traverse scopes in
				not captured && self#get_current_scope#is_safe local_var
		(**
			This method should be called upon passing `while`.
			It collects locals which are checked against `null` and executes callbacks for expressions with proper statuses of locals.
		*)
		method process_while expr is_nullable_expr (condition_callback:texpr->unit) (body_callback:texpr->unit) =
			match expr.eexpr with
				| TWhile (condition, body, DoWhile) ->
					condition_callback condition;
					body_callback body
				| TWhile (condition, body, NormalWhile) ->
					condition_callback condition;
					let (nulls, not_nulls) = process_condition condition is_nullable_expr (fun _ -> ()) in
					(** execute `body` with known not-null variables *)
					List.iter self#get_current_scope#add_to_safety not_nulls;
					body_callback body;
					List.iter self#get_current_scope#remove_from_safety not_nulls;
				| _ -> fail ~msg:"Expected TWhile" expr.epos __POS__
		(**
			Should be called for bodies of loops (for, while)
		*)
		method process_loop_body (first_check:unit->unit) (second_check:unit->unit) =
			let original_safe_locals = self#get_safe_locals_copy in
			(** The first check to find out which vars will become unsafe in a loop *)
			first_check();
			(* If local var became safe in a loop, then we need to remove it from safety to make it unsafe outside of a loop again *)
			self#get_current_scope#filter_safety original_safe_locals;
			(** The second check with unsafe vars removed from safety *)
			second_check()
		(**
			This method should be called upon passing `try`.
		*)
		method process_try (try_block:texpr) (catches:(tvar * texpr) list) (check_expr:texpr->unit) =
			let original_safe_locals = self#get_safe_locals_copy in
			check_expr try_block;
			(* Remove locals which became safe inside of a try block from safety *)
			self#get_current_scope#filter_safety original_safe_locals;
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
					let (nulls, not_nulls) =
						process_condition condition is_nullable_expr (fun _ -> ())
					in
					let not_condition =
						{ eexpr = TUnop (Not, Prefix, condition); etype = condition.etype; epos = condition.epos }
					in
					let (else_nulls, else_not_nulls) =
						process_condition not_condition is_nullable_expr (fun _ -> ())
					in
					(** execute `if_body` with known not-null variables *)
					List.iter self#get_current_scope#add_to_safety not_nulls;
					body_callback if_body;
					List.iter self#get_current_scope#remove_from_safety not_nulls;
					(** execute `else_body` with known not-null variables *)
					let handle_dead_end body safe_vars =
						if is_dead_end body then
							List.iter self#get_current_scope#add_to_safety safe_vars
					in
					(match else_body with
						| None ->
							(** If `if_body` terminates execution, then bypassing `if` means `else_not_nulls` are safe now *)
							handle_dead_end if_body else_not_nulls
						| Some else_body ->
							List.iter self#get_current_scope#add_to_safety else_not_nulls;
							body_callback else_body;
							List.iter self#get_current_scope#remove_from_safety else_not_nulls;
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
			let (_, not_nulls) = process_condition left_expr is_nullable_expr callback in
			List.iter self#get_current_scope#add_to_safety not_nulls;
			callback right_expr;
			List.iter self#get_current_scope#remove_from_safety not_nulls
		(**
			Handle boolean OR outside of `if` condition.
		*)
		method process_or left_expr right_expr is_nullable_expr (callback:texpr->unit) =
			let (nulls, _) = process_condition left_expr is_nullable_expr callback in
			List.iter self#get_current_scope#add_to_safety nulls;
			callback right_expr;
			List.iter self#get_current_scope#remove_from_safety nulls
		(**
			Remove local var from safety list if a nullable value is assigned to that var
		*)
		method handle_assignment is_nullable_expr left_expr (right_expr:texpr) =
			match (reveal_expr left_expr).eexpr with
				| TLocal v ->
					if is_nullable_expr right_expr then
						begin
							let captured = ref false in
							let rec traverse (lst:safety_scope list) =
								match lst with
									| [] -> ()
									| current :: rest ->
										if current#owns_var v then
											current#remove_from_safety ~forever:!captured v
										else begin
											captured := !captured || current#get_type = STClosure;
											current#remove_from_safety ~forever:!captured v;
											traverse rest
										end
							in
							traverse scopes
						end
					else if is_nullable_type v.v_type then
						self#get_current_scope#add_to_safety v
				| _ -> ()
	end

(**
	This class is used to recursively check typed expressions for null-safety
*)
class expr_checker immediate_execution report =
	object (self)
		val local_safety = new local_vars
		val mutable return_types = []
		val mutable in_closure = false
		(* if this flag is `true` then spotted errors and warnings will not be reported *)
		val mutable is_pretending = false
		(* val mutable cnt = 0 *)
		(**
			Register an error
		*)
		method error msg (p:Globals.pos) =
			if not is_pretending then
				report.sr_errors <- { sm_msg = ("Safety: " ^ msg); sm_pos = p; } :: report.sr_errors;
		(**
			Check if `e` is nullable even if the type is reported not-nullable.
			Haxe type system lies sometimes.
		*)
		method is_nullable_expr e =
			match e.eexpr with
				| TConst TNull -> true
				| TConst _ -> false
				(* Safety.unsafe() *)
				| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ([], "Safety")}, { cf_name = "unsafe" })) }, _) -> false
				| TParenthesis e -> self#is_nullable_expr e
				| TMeta (_, e) -> self#is_nullable_expr e
				| TLocal v -> not (local_safety#is_safe v)
				| TThrow _ -> false
				| TBlock exprs ->
					(match exprs with
						| [] -> false
						| _ -> self#is_nullable_expr (List.hd (List.rev exprs))
					)
				| TIf _ ->
					let nullable = ref false in
					let check body = nullable := !nullable || self#is_nullable_expr body in
					local_safety#process_if e self#is_nullable_expr (fun _ -> ()) check;
					!nullable
				| _ -> is_nullable_type e.etype
		(**
			Check if `expr` can be passed to a place where `to_type` is expected.
			This method has side effect: it logs an error if `expr` has a type parameter incompatible with the type parameter of `to_type`.
			E.g.: `Array<Null<String>>` vs `Array<String>` returns `true`, but also adds a compilation error.
		*)
		method can_pass_expr expr to_type p =
			if (is_special_type_unsafe expr.etype) || (is_special_type_unsafe to_type) then
				true
			else if self#is_nullable_expr expr && not (is_nullable_type to_type) then
				false
			else
				let expr_type = unfold_null expr.etype in
				try
					unify expr_type to_type;
					true
				with
					| Safety_error err ->
						self#error ("Cannot unify " ^ (str_type expr_type) ^ " with " ^ (str_type to_type)) p;
						(* returning `true` because error is already logged in the line above *)
						true
					(* returning `true` because real unification check is already performed by the compiler at this moment *)
					| _ -> true
				(* can_pass_type expr.etype to_type *)
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
				| TCall (callee, args) -> self#check_call callee args
				| TNew _ -> self#check_new e
				| TUnop (_, _, expr) -> self#check_unop expr e.epos
				| TFunction fn -> self#check_function fn
				| TVar (v, init_expr) -> self#check_var v init_expr e.epos
				| TBlock exprs -> self#check_block exprs e.epos
				| TFor _ -> self#check_for e
				| TIf _ -> self#check_if e
				| TWhile _ -> self#check_while e
				| TSwitch (target, cases, default) -> self#check_switch target cases default
				| TTry (try_block, catches) -> self#check_try try_block catches
				| TReturn (Some expr) -> self#check_return expr e.epos
				| TReturn None -> ()
				| TBreak -> ()
				| TContinue -> ()
				| TThrow expr -> self#check_throw expr e.epos
				| TCast (expr, _) -> self#check_cast expr e.etype e.epos
				| TMeta (_, e) -> self#check_expr e
				| TEnumIndex idx -> self#check_enum_index idx
				| TEnumParameter (e, _, _) -> self#check_expr e (** Checking enum value itself is not needed here because this expr always follows after TEnumIndex *)
				| TIdent _ -> ()
		(**
			Check expressions in a block
		*)
		method private check_block exprs p =
			match exprs with
				| [] -> ()
				(* Local named functions like `function fn() {}`, which are generated as `var fn = null; fn = function(){}` *)
				| { eexpr = TVar (v1, Some { eexpr = TConst TNull }) }
					:: ({ eexpr = TBinop (OpAssign, { eexpr = TLocal v2 }, { eexpr = TFunction _ }) } as e)
					:: rest
						when v1.v_id = v2.v_id && (match v1.v_type with TFun _ -> true | _ -> false) ->
					self#check_expr e;
					self#check_block rest p
				| e :: rest ->
					self#check_expr e;
					self#check_block rest p
		(**
			Don't allow to use nullable values as items in declaration of not-nullable arrays
		*)
		method private check_array_decl items arr_type p =
			(match Abstract.follow_with_abstracts arr_type with
				| TInst ({ cl_path = ([], "Array") }, [item_type]) ->
					List.iter
						(fun e ->
							if not (self#can_pass_expr e item_type e.epos) then
								self#error ("Cannot use nullable value of " ^ (str_type e.etype) ^ " as an item in Array<" ^ (str_type item_type) ^ ">") e.epos
						)
						items;
				| _ -> ()
			);
			List.iter self#check_expr items
		(**
			Deal with nullable enum values
		*)
		method private check_enum_index idx =
			if self#is_nullable_expr idx then
				self#error "Cannot access nullable enum value." idx.epos;
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
							self#error "Cannot use nullable value as a condition in \"while\"." condition.epos;
						self#check_expr condition
					in
					local_safety#loop_declared e;
					local_safety#process_while e self#is_nullable_expr check_condition self#check_loop_body;
					local_safety#scope_closed
				| _ -> fail ~msg:"Expected TWhile." e.epos __POS__
		(**
			Don't iterate on nullable values
		*)
		method private check_for e =
			match e.eexpr with
				| TFor (v, iterable, body) ->
					if self#is_nullable_expr iterable then
						self#error "Cannot iterate over nullable value." iterable.epos;
					self#check_expr iterable;
					local_safety#declare_var v;
					local_safety#loop_declared e;
					self#check_loop_body body;
					local_safety#scope_closed
				| _ -> fail ~msg:"Expected TFor." e.epos __POS__
		(**
			Handle safety inside of loops
		*)
		method private check_loop_body body =
			local_safety#process_loop_body
				(* Start pretending to ignore errors *)
				(fun () ->
					is_pretending <- true;
					self#check_expr body
				)
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
				self#error "Cannot throw nullable value." p;
			self#check_expr e
		(**
			Don't cast nullable expressions to not-nullable types
		*)
		method private check_cast expr to_type p =
			(* Don't check `(expr:Unsafe<T>)` *)
			if not (is_special_type_unsafe to_type) then begin
				self#check_expr expr;
				match to_type with
					(* untyped cast *)
					| TMono _ -> ()
					(* typed cast and type check *)
					| _ ->
						if not (self#can_pass_expr expr to_type p) then
							self#error "Cannot cast nullable value to not nullable type." p
			end
		(**
			Check safety in a function
		*)
		method private check_function ?(immediate_execution=false) fn =
			local_safety#function_declared immediate_execution fn;
			return_types <- fn.tf_type :: return_types;
			if immediate_execution then
				begin
					(* Start pretending to ignore errors *)
					is_pretending <- true;
					self#check_expr fn.tf_expr;
					(* Now we know, which vars will become unsafe in this closure. Stop pretending and perform real check *)
					is_pretending <- false;
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
					self#error ("Cannot return nullable value of " ^ (str_type e.etype) ^ " as " ^ (str_type t)) p
				| _ -> ()
		(**
			Check safety in `switch` expressions.
		*)
		method private check_switch target cases default =
			if self#is_nullable_expr target then
				self#error "Cannot switch on nullable value." target.epos;
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
		method private check_if e =
			let check_condition e =
				if self#is_nullable_expr e then
					self#error "Cannot use nullable value as condition in \"if\"." e.epos;
				self#check_expr e
			in
			local_safety#process_if e self#is_nullable_expr check_condition self#check_expr
		(**
			Check array access on nullable values or using nullable indexes
		*)
		method private check_array_access arr idx p =
			if self#is_nullable_expr arr then
				self#error "Cannot perform array access on nullable value." p;
			if self#is_nullable_expr idx then
				self#error "Cannot use nullable value as an index for array access." p;
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
				| OpAssign ->
					if not (self#can_pass_expr right_expr left_expr.etype p) then
						begin
							self#error "Cannot assign nullable value here." p;
							check_both()
						end
					else
						begin
							check_both();
							local_safety#handle_assignment self#is_nullable_expr left_expr right_expr;
						end
				| _->
					if self#is_nullable_expr left_expr || self#is_nullable_expr right_expr then
						self#error "Cannot perform binary operation on nullable value." p;
					check_both()
		(**
			Don't perform unops on nullable values
		*)
		method private check_unop e p =
			if self#is_nullable_expr e then
				self#error "Cannot perform unary operation on nullable value." p;
			self#check_expr e
		(**
			Don't assign nullable value to not-nullable variable on var declaration
		*)
		method private check_var v init p =
			local_safety#declare_var v;
			match init with
				| None -> ()
				| Some e ->
					let local = { eexpr = TLocal v; epos = v.v_pos; etype = v.v_type } in
					self#check_binop OpAssign local e p
					(* self#check_expr e;
					local_safety#handle_assignment self#is_nullable_expr local e;
					if not (self#can_pass_expr e v.v_type p) then
						self#error "Cannot assign nullable value to not-nullable variable." p; *)
		(**
			Make sure nobody tries to access a field on a nullable value
		*)
		method private check_field target access p =
			if self#is_nullable_expr target then
				self#error ("Cannot access \"" ^ accessed_field_name access ^ "\" of a nullable value.") p;
			self#check_expr target
		(**
			Check constructor invocation: don't pass nulable values to not-nullable arguments
		*)
		method private check_new e_new =
			match e_new.eexpr with
				| TNew (cls, params, args) ->
					let ctor =
						try
							Some (get_constructor (fun ctor -> apply_params cls.cl_params params ctor.cf_type) cls)
						with
							| Not_found -> None
					in
					(match ctor with
						| None ->
							List.iter self#check_expr args
						| Some (ctor_type, _) ->
							let rec traverse t =
								match follow t with
									| TFun (types, _) -> self#check_args e_new args types
									| _ -> fail ~msg:"Unexpected constructor type." e_new.epos __POS__
							in
							traverse ctor_type
					)
				| _ -> fail ~msg:"TNew expected" e_new.epos __POS__
		(**
			Check calls: don't call a nullable value, dont' pass nulable values to not-nullable arguments
		*)
		method private check_call callee args =
			if self#is_nullable_expr callee then
				self#error "Cannot call a nullable value." callee.epos;
			self#check_expr callee;
			match follow callee.etype with
				| TFun (types, _) ->
					self#check_args callee args types
				| _ ->
					List.iter self#check_expr args
		(**
			Check if specified expressions can be passed to a call which expects `types`.
		*)
		method private check_args ?(arg_num=0) callee args types =
			match (args, types) with
				| (arg :: args, (arg_name, optional, t) :: types) ->
					if not optional && not (self#can_pass_expr arg t arg.epos) then begin
						let fn_str = match symbol_name callee with "" -> "" | name -> " of function \"" ^ name ^ "\""
						and arg_str = if arg_name = "" then "" else " \"" ^ arg_name ^ "\"" in
						self#error ("Cannot pass nullable value to not-nullable argument" ^ arg_str ^ fn_str ^ ".") arg.epos
					end;
					(match arg.eexpr with
						| TFunction fn ->
							self#check_function ~immediate_execution:(immediate_execution#check callee arg_num) fn
						| _ ->
							self#check_expr arg
					);
					self#check_args ~arg_num:(arg_num + 1) callee args types;
				| _ -> ()
	end

class class_checker cls immediate_execution report  =
	object (self)
			val checker = new expr_checker immediate_execution report
		(**
			Entry point for checking a class
		*)
		method check =
			(* if snd cls.cl_path = "AllVarsInitializedInConstructor_thisShouldBeUsable" then
				Option.may (fun f -> Option.may (fun e -> print_endline (s_expr (fun t -> "") e)) f.cf_expr) cls.cl_constructor; *)
			if (not cls.cl_extern) && (not cls.cl_interface) then
				self#check_var_fields;
			let check_field f =
				if not (contains_unsafe_meta f.cf_meta) then begin
					(* if f.cf_name = "closure_immediatelyExecuted_shouldInheritSafety" then
						Option.may (fun e -> print_endline (s_expr (fun t -> "") e)) f.cf_expr; *)
					Option.may checker#check_root_expr f.cf_expr
				end
			in
			Option.may checker#check_root_expr cls.cl_init;
			Option.may check_field cls.cl_constructor;
			List.iter check_field cls.cl_ordered_fields;
			List.iter check_field cls.cl_ordered_statics;
		(**
			Check `var` fields are initialized properly
		*)
		method check_var_fields =
			let check_field is_static field =
				if should_be_initialized field then
					if not (is_nullable_type field.cf_type) && not (contains_unsafe_meta field.cf_meta) then
						match field.cf_expr with
							| None ->
								if is_static then
									checker#error
										("Field \"" ^ field.cf_name ^ "\" is not nullable thus should have an initial value.")
										field.cf_pos
							| Some e ->
								if not (checker#can_pass_expr e field.cf_type e.epos) then
									checker#error ("Cannot set nullable initial value for not-nullable field \"" ^ field.cf_name ^ "\".") field.cf_pos
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
			let rec check_unsafe_usage init_list e =
				if Hashtbl.length init_list > 0 then
					match e.eexpr with
						| TField ({ eexpr = TConst TThis }, FInstance (_, _, field)) ->
							if Hashtbl.mem init_list field.cf_name then
								checker#error ("Cannot use field " ^ field.cf_name ^ " until initialization.") e.epos
						| TField ({ eexpr = TConst TThis }, FClosure (_, field)) ->
							checker#error ("Cannot use method " ^ field.cf_name ^ " until all instance fields are initialized.") e.epos;
						| TCall ({ eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, field)) }, args) ->
							checker#error ("Cannot call method " ^ field.cf_name ^ " until all instance fields are initialized.") e.epos;
							List.iter (check_unsafe_usage init_list) args
						| TConst TThis ->
							checker#error "Cannot use \"this\" until all instance fields are initialized." e.epos
						| TLocal v when Hashtbl.mem this_vars v.v_id ->
							checker#error "Cannot use \"this\" until all instance fields are initialized." e.epos
						| _ ->
							iter (check_unsafe_usage init_list) e
			in
			let rec traverse init_list e =
				(match e.eexpr with
					| TBinop (OpAssign, { eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, f)) }, right_expr) ->
						Hashtbl.remove init_list f.cf_name;
						ignore (traverse init_list right_expr)
					| TWhile (condition, body, DoWhile) ->
						check_unsafe_usage init_list condition;
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
						check_unsafe_usage init_list e
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
						field.cf_pos
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
			| TClassDecl cls when (contains_safe_meta cls.cl_meta) && not (contains_unsafe_meta cls.cl_meta) ->
				(new class_checker cls immediate_execution report)#check
			| TClassDecl _ -> ()
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
