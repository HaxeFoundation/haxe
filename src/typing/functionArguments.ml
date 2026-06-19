open Globals
open Ast
open Type
open Typecore
open Error

let rec is_flash_native_basic t = match follow t with
	| TAbstract({a_path=([],("Int"|"Float"|"Bool"))},_) -> true
	| TAbstract({a_path=(["haxe"],("Int32"|"UInt32"))},_) -> true
	| TInst({cl_path=([],("Int"|"Float"))},_) -> true
	| TInst({cl_path=(["haxe"],"Int32")},_) -> true
	| TEnum({e_path=([],"Bool")},_) -> true
	| _ -> false

let type_function_arg com t e opt p =
	(* TODO https://github.com/HaxeFoundation/haxe/issues/8461 *)
	(* delay ctx PTypeField (fun() ->
		if ExtType.is_void (follow t) then
			error "Arguments of type Void are not allowed" p
	); *)
	if opt then
		let e = (match e with None -> Some (EConst (Ident "null"),null_pos) | _ -> e) in
		com.Common.basic.tnull t, e
	else
		let t = match e with Some (EConst (Ident "null"),null_pos) -> com.basic.tnull t | _ -> t in
		t, e

let type_function_arg_value ctx t c do_display =
	match c with
		| None -> None
		| Some e ->
			let p = pos e in
			let e = if do_display then Display.preprocess_expr ctx.com e else e in
			let e = type_expr ctx e (WithType.with_type t) in
			let e = AbstractCast.cast_or_unify ctx t e p in
			let e = Optimizer.reduce_expression (SafeCom.of_typer ctx) e in
			let run_analyzer e = !analyzer_run_on_expr_ref ctx.com (Printf.sprintf "%s.%s" (s_type_path ctx.c.curclass.cl_path) ctx.f.curfield.cf_name) e in
			if ctx.e.curfun = FunConstructor then begin
				let rec check_this e = match e.eexpr with
					| TConst TThis ->
						raise_typing_error "Cannot access this in a constructor's default argument value" e.epos
					| TLocal v when (match ctx.f.vthis with Some v2 -> v == v2 | None -> false) ->
						raise_typing_error "Cannot access this in a constructor's default argument value" e.epos
					| _ ->
						Type.iter check_this e
				in
				check_this e
			end;
			let references_arg e =
				let rec loop e = match e.eexpr with
					| TLocal v when (try PMap.find v.v_name ctx.f.locals == v with Not_found -> false) -> raise Exit
					| _ -> Type.iter loop e
				in
				try loop e; false with Exit -> true
			in
			let rec loop analyzered e = match e.eexpr with
				| TConst _ -> Some e
				| TLocal _ -> Some e
				| TField({eexpr = TTypeExpr _},FEnum _) -> Some e
				| TField({eexpr = TTypeExpr _},FStatic({cl_kind = KAbstractImpl a},cf)) when a.a_enum && has_class_field_flag cf CfEnum -> Some e
				| TCast(e,None) -> loop analyzered e
				| _ when not analyzered && not (references_arg e) -> loop true (run_analyzer e)
				| _ when ctx.com.platform = Flash && is_flash_native_basic t ->
					raise_typing_error ("Non-constant default argument values are not supported on the flash target for basic type " ^ s_type (print_context()) t) p
				| _ -> Some e
			in
			loop false e

class function_arguments
	(com : Common.context)
	(type_arg : int -> bool -> type_hint option -> pos -> Type.t)
	(is_extern : bool)
	(do_display : bool)
	(abstract_this : Type.t option)
	(syntax : (placed_name * bool * metadata * type_hint option * expr option) list)
=
	let with_default =
		let l = List.mapi (fun i ((name,pn),opt,_,t,eo) ->
			let t = type_arg i opt t pn in
			let t,eo = type_function_arg com t eo opt pn in
			(name,eo,t)
		) syntax in
		let l = match abstract_this with
			| None ->
				l
			| Some t ->
				("this",None,t) :: l
		in
		l
	in

	let check_coroutine_scope v =
		try
			let mt = t_infos (module_type_of_type v.v_type) in
			(match CoroScopeConfig.of_meta_list mt.mt_meta with
			| Some config ->
				add_var_flag v VCoroScope;
				if config.CoroScopeConfig.restricted_suspension then
					add_var_flag v VCoroRestrictedSuspension
			| None -> ())
		with Exit ->
			()
	in
object(self)

	val mutable type_repr = None
	val mutable expr_repr = None

	(* Returns the `(string * bool * Type.t) list` requires by `TFun` .*)
	method for_type = match type_repr with
		| Some l ->
			l
		| None ->
			let l = List.map (fun (n,eo,t) -> n,eo <> None,t) with_default in
			type_repr <- Some l;
			l

	method private check_rest (is_last : bool) (eo : expr option) (opt : bool) (t : Type.t) (pn : pos) =
		if ExtType.is_rest (follow t) then begin
			if opt then raise_typing_error "Rest argument cannot be optional" pn;
			begin match eo with None -> () | Some (_,p) -> raise_typing_error "Rest argument cannot have default value" p end;
			if not is_last then raise_typing_error "Rest should only be used for the last function argument" pn;
		end

	(* Returns the `(tvar * texpr option) list` for `tf_args`. Also checks the validity of argument names and whether or not
	   an argument should be displayed. *)
	method for_expr ctx = match expr_repr with
		| Some l ->
			l
		| None ->
			let make_local name kind t meta pn =
				let v = alloc_var kind name t pn in
				let meta = (StrictMeta.check_strict_meta ctx meta) @ meta in
				v.v_meta <- meta;
				v
			in
			let rec loop acc is_abstract_this syntax typed = match syntax,typed with
				| syntax,(name,_,t) :: typed when is_abstract_this ->
					let v = make_local name VAbstractThis t [] null_pos in
					v.v_meta <- (Meta.This,[],null_pos) :: v.v_meta;
					loop ((v,None) :: acc) false syntax typed
				| ((_,pn),opt,m,_,_) :: syntax,(name,eo,t) :: typed ->
					delay ctx.g PTypeField (fun() -> self#check_rest (typed = []) eo opt t pn);
					if not is_extern then begin
						Naming.check_local_variable_name ctx.com name TVOArgument pn;
						if name <> "_" && List.exists (fun (v,_) -> v.v_name = name) acc then
							raise_typing_error ("Duplicate argument name \"" ^ name ^ "\"") pn;
					end;
					let eo = type_function_arg_value ctx t eo do_display in
					let v = make_local name (VUser TVOArgument) t m pn in
					if do_display && DisplayPosition.display_position#enclosed_in pn then
						DisplayEmitter.display_variable ctx v pn;
					if acc = [] && TyperManager.is_coroutine_context ctx then check_coroutine_scope v;
					if name <> "_" then ctx.f.locals <- PMap.add v.v_name v ctx.f.locals;
					loop ((v,eo) :: acc) false syntax typed
				| [],[] ->
					List.rev acc
				| _ ->
					die "" __LOC__
			in
			let l = loop [] (abstract_this <> None) syntax with_default in
			expr_repr <- Some l;
			l

	(* Verifies the validity of any argument typed as `haxe.extern.Rest` and checks default values. *)
	method verify_extern ctx =
		let rec loop is_abstract_this syntax typed = match syntax,typed with
			| syntax,(name,_,t) :: typed when is_abstract_this ->
				loop false syntax typed
			| ((_,pn),opt,m,_,_) :: syntax,(name,eo,t) :: typed ->
				delay ctx.g PTypeField (fun() -> self#check_rest (typed = []) eo opt t pn);
				ignore(type_function_arg_value ctx t eo do_display);
				loop false syntax typed
			| [],[] ->
				()
			| _ ->
				die "" __LOC__
		in
		loop (abstract_this <> None) syntax with_default

	(* Brings arguments into context by adding them to `ctx.f.locals`. *)
	method bring_into_context ctx =
		List.iter (fun (v,_) ->
			ctx.f.locals <- PMap.add v.v_name v ctx.f.locals
		) (self#for_expr ctx)
end
