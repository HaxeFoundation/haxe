open Globals
open Ast
open Type
open Typecore
open Error

let type_opt ctx is_core_api p t =
	let c = ctx.curclass in
	match t with
	| None when (has_class_flag c CExtern) || (has_class_flag c CInterface) ->
		display_error ctx "Type required for extern classes and interfaces" p;
		t_dynamic
	| None when is_core_api ->
		display_error ctx "Type required for core api classes" p;
		t_dynamic
	| _ ->
		Typeload.load_type_hint ctx p t

let type_function_arg ctx t e opt p =
	(* TODO https://github.com/HaxeFoundation/haxe/issues/8461 *)
	(* delay ctx PTypeField (fun() ->
		if ExtType.is_void (follow t) then
			error "Arguments of type Void are not allowed" p
	); *)
	if opt then
		let e = (match e with None -> Some (EConst (Ident "null"),null_pos) | _ -> e) in
		ctx.t.tnull t, e
	else
		let t = match e with Some (EConst (Ident "null"),null_pos) -> ctx.t.tnull t | _ -> t in
		t, e

let type_function_arg_value ctx t c do_display =
	match c with
		| None -> None
		| Some e ->
			let p = pos e in
			let e = if do_display then Display.ExprPreprocessing.process_expr ctx.com e else e in
			let e = ctx.g.do_optimize ctx (type_expr ctx e (WithType.with_type t)) in
			unify ctx e.etype t p;
			let rec loop e = match e.eexpr with
				| TConst _ -> Some e
				| TField({eexpr = TTypeExpr _},FEnum _) -> Some e
				| TField({eexpr = TTypeExpr _},FStatic({cl_kind = KAbstractImpl a},cf)) when a.a_enum && has_class_field_flag cf CfEnum -> Some e
				| TCast(e,None) -> loop e
				| _ ->
					if ctx.com.display.dms_kind = DMNone || ctx.com.display.dms_inline && ctx.com.display.dms_error_policy = EPCollect then
						display_error ctx "Parameter default value should be constant" p;
					None
			in
			loop e

class function_arguments
	(ctx : typer)
	(type_arg : bool -> type_hint option -> pos -> Type.t)
	(is_extern : bool)
	(do_display : bool)
	(abstract_this : Type.t option)
	(syntax : (placed_name * bool * metadata * type_hint option * expr option) list)
=
	let with_default =
		let l = List.map (fun ((name,pn),opt,m,t,eo) ->
			let t = type_arg opt t pn in
			let t,eo = type_function_arg ctx t eo opt pn in
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
		match follow t with
			| TAbstract({a_path = ["haxe";"extern"],"Rest"},_) ->
				if not is_extern then error "Rest argument are only supported for extern methods" pn;
				if opt then error "Rest argument cannot be optional" pn;
				begin match eo with None -> () | Some (_,p) -> error "Rest argument cannot have default value" p end;
				if not is_last then error "Rest should only be used for the last function argument" pn;
			| _ ->
				()

	(* Returns the `(tvar * texpr option) list` for `tf_args`. Also checks the validity of argument names and whether or not
	   an argument should be displayed. *)
	method for_expr = match expr_repr with
		| Some l ->
			l
		| None ->
			let make_local name t meta pn =
				let v = alloc_var (VUser TVOArgument) name t pn in
				v.v_meta <- v.v_meta @ meta;
				v
			in
			let rec loop acc is_abstract_this syntax typed = match syntax,typed with
				| syntax,(name,_,t) :: typed when is_abstract_this ->
					let v = make_local name t [] null_pos in
					v.v_meta <- (Meta.This,[],null_pos) :: v.v_meta;
					loop ((v,None) :: acc) false syntax typed
				| ((_,pn),opt,m,_,_) :: syntax,(name,eo,t) :: typed ->
					delay ctx PTypeField (fun() -> self#check_rest (typed = []) eo opt t pn);
					if not is_extern then check_local_variable_name ctx name TVOArgument pn;
					let eo = type_function_arg_value ctx t eo do_display in
					let v = make_local name t m pn in
					if do_display && DisplayPosition.display_position#enclosed_in pn then
						DisplayEmitter.display_variable ctx v pn;
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
	method verify_extern =
		let rec loop is_abstract_this syntax typed = match syntax,typed with
			| syntax,(name,_,t) :: typed when is_abstract_this ->
				loop false syntax typed
			| ((_,pn),opt,m,_,_) :: syntax,(name,eo,t) :: typed ->
				delay ctx PTypeField (fun() -> self#check_rest (typed = []) eo opt t pn);
				ignore(type_function_arg_value ctx t eo do_display);
				loop false syntax typed
			| [],[] ->
				()
			| _ ->
				die "" __LOC__
		in
		loop (abstract_this <> None) syntax with_default

	(* Brings arguments into context by adding them to `ctx.locals`. *)
	method bring_into_context =
		List.iter (fun (v,_) ->
			ctx.locals <- PMap.add v.v_name v ctx.locals
		) self#for_expr
end