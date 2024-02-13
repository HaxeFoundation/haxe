open Globals
open Ast
open Type
open Common
open Typecore
open Error
open ExceptionTypes
open ExceptionFunctions

(**
	Check if `e` contains any throws or try..catches.
*)
let rec contains_throw_or_try e =
	match e.eexpr with
	| TThrow _ | TTry _ -> true
	| _ -> check_expr contains_throw_or_try e

(**
	Transform `throw` and `try..catch` expressions.
	`rename_locals` is required to deal with the names of temp vars.
*)
let filter tctx =
	let stub e = e in
	match tctx.com.platform with (* TODO: implement for all targets *)
	| Php | Js | Jvm | Python | Lua | Eval | Neko | Flash | Hl | Cpp ->
		let ctx = ExceptionMapper.create_exception_context tctx in
		let rec run e =
			match e.eexpr with
			| TThrow e1 ->
				{ e with eexpr = TThrow (ExceptionMapper.throw_native ctx (run e1) e.etype e.epos) }
			| TTry(e1,catches) ->
				let catches =
					let catches = List.map (fun (v,e) -> (v,run e)) catches in
					(ExceptionMapper.catch_native ctx catches e.etype e.epos)
				in
				{ e with eexpr = TTry(run e1,catches) }
			| _ ->
				map_expr run e
		in
		(fun e ->
			if contains_throw_or_try e then run e
			else stub e
		)
	| Cross | CustomTarget _ -> stub

(**
	Inserts `haxe.NativeStackTrace.saveStack(e)` in non-haxe.Exception catches.
*)
let insert_save_stacks tctx =
	if not (has_feature tctx.com "haxe.NativeStackTrace.exceptionStack") then
		(fun e -> e)
	else
		let native_stack_trace_cls =
			let tp = mk_type_path (["haxe"],"NativeStackTrace") in
			match Typeload.load_type_def tctx null_pos tp with
			| TClassDecl cls -> cls
			| TAbstractDecl { a_impl = Some cls } -> cls
			| _ -> raise_typing_error "haxe.NativeStackTrace is expected to be a class or an abstract" null_pos
		in
		let rec contains_insertion_points e =
			match e.eexpr with
			| TTry (e, catches) ->
				List.exists (fun (v, _) -> Meta.has Meta.NeedsExceptionStack v.v_meta) catches
				|| contains_insertion_points e
				|| List.exists (fun (_, e) -> contains_insertion_points e) catches
			| _ ->
				check_expr contains_insertion_points e
		in
		let save_exception_stack catch_var =
			(* GOTCHA: `has_feature` always returns `true` if executed before DCE filters *)
			if has_feature tctx.com "haxe.NativeStackTrace.exceptionStack" then
				let method_field =
					try PMap.find "saveStack" native_stack_trace_cls.cl_statics
					with Not_found -> raise_typing_error ("haxe.NativeStackTrace has no field saveStack") null_pos
				in
				let return_type =
					match follow method_field.cf_type with
					| TFun(_,t) -> t
					| _ -> raise_typing_error ("haxe.NativeStackTrace." ^ method_field.cf_name ^ " is not a function and cannot be called") null_pos
				in
				let catch_local = mk (TLocal catch_var) catch_var.v_type catch_var.v_pos in
				begin
					add_dependency tctx.c.curclass.cl_module native_stack_trace_cls.cl_module;
					make_static_call tctx native_stack_trace_cls method_field (fun t -> t) [catch_local] return_type catch_var.v_pos
				end
			else
				mk (TBlock[]) tctx.t.tvoid catch_var.v_pos
		in
		let rec run e =
			match e.eexpr with
			| TTry (e1, catches) ->
				let e1 = map_expr run e1 in
				let catches =
					List.map (fun ((v, body) as catch) ->
						if Meta.has Meta.NeedsExceptionStack v.v_meta then
							let exprs =
								match body.eexpr with
								| TBlock exprs ->
									save_exception_stack v :: exprs
								| _ ->
									[save_exception_stack v; body]
							in
							(v, { body with eexpr = TBlock exprs })
						else
							catch
					) catches
				in
				{ e with eexpr = TTry (e1, catches) }
			| _ ->
				map_expr run e
		in
		(fun e ->
			if contains_insertion_points e then run e
			else e
		)

(**
	Adds `this.__shiftStack()` calls to constructors of classes which extend `haxe.Exception`
*)
let patch_constructors tctx =
	let tp = make_ptp (mk_type_path haxe_exception_type_path) null_pos in
	match Typeload.load_instance tctx tp ParamSpawnMonos LoadNormal with
	(* Add only if `__shiftStack` method exists *)
	| TInst(cls,_) when PMap.mem "__shiftStack" cls.cl_fields ->
		(fun mt ->
			match mt with
			| TClassDecl cls when not (has_class_flag cls CExtern) && cls.cl_path <> haxe_exception_type_path && is_haxe_exception_class cls ->
				let shift_stack p =
					let t = type_of_module_type mt in
					let this = { eexpr = TConst(TThis); etype = t; epos = p } in
					let faccess =
						try quick_field t "__shiftStack"
						with Not_found -> raise_typing_error "haxe.Exception has no field __shiftStack" p
					in
					match faccess with
					| FInstance (_,_,cf) ->
						let efield = { eexpr = TField(this,faccess); etype = cf.cf_type; epos = p } in
						let rt =
							match follow cf.cf_type with
							| TFun(_,t) -> t
							| _ ->
								raise_typing_error "haxe.Exception.__shiftStack is not a function and cannot be called" cf.cf_name_pos
						in
						make_call tctx efield [] rt p
					| _ -> raise_typing_error "haxe.Exception.__shiftStack is expected to be an instance method" p
				in
				TypeloadFunction.add_constructor tctx cls true cls.cl_name_pos;
				Option.may (fun cf -> ignore(follow cf.cf_type)) cls.cl_constructor;
				(match cls.cl_constructor with
				| Some ({ cf_expr = Some e_ctor } as ctor) ->
					let rec add e =
						match e.eexpr with
						| TFunction _ -> e
						| TReturn _ -> mk (TBlock [shift_stack e.epos; e]) e.etype e.epos
						| _ -> map_expr add e
					in
					(ctor.cf_expr <- match e_ctor.eexpr with
						| TFunction fn ->
							Some { e_ctor with
								eexpr = TFunction { fn with
									tf_expr = mk (TBlock [add fn.tf_expr; shift_stack fn.tf_expr.epos]) tctx.t.tvoid fn.tf_expr.epos
								}
							}
						| _ -> die "" __LOC__
					)
				| None -> die "" __LOC__
				| _ -> ()
				)
			| _ -> ()
		)
	| _ -> (fun _ -> ())
