open Globals
open Ast
open Type
open Common
open Typecore
open TyperBase
open Fields
open Error

let haxe_exception_type_path = (["haxe"],"Exception")
let value_exception_type_path = (["haxe"],"ValueException")

type context = {
	typer : typer;
	basic : basic_types;
	config : exceptions_config;
	wildcard_catch_type : Type.t;
	base_throw_type : Type.t;
	throws_anything : bool;
	catches_anything : bool;
	haxe_exception_class : tclass;
	haxe_exception_type : Type.t;
	haxe_native_stack_trace : tclass;
	value_exception_type : Type.t;
	value_exception_class : tclass;
}

let is_dynamic t =
	match Abstract.follow_with_abstracts t with
	| TAbstract({ a_path = [],"Dynamic" }, _) -> true
	| t -> t == t_dynamic

(**
	Generate `haxe.Exception.method_name(args)`
*)
let haxe_exception_static_call ctx method_name args p =
	let method_field =
		try PMap.find method_name ctx.haxe_exception_class.cl_statics
		with Not_found -> typing_error ("haxe.Exception has no field " ^ method_name) p
	in
	let return_type =
		match follow method_field.cf_type with
		| TFun(_,t) -> t
		| _ -> typing_error ("haxe.Exception." ^ method_name ^ " is not a function and cannot be called") p
	in
	add_dependency ctx.typer.curclass.cl_module ctx.haxe_exception_class.cl_module;
	make_static_call ctx.typer ctx.haxe_exception_class method_field (fun t -> t) args return_type p

(**
	Generate `haxe_exception.method_name(args)`
*)
let haxe_exception_instance_call ctx haxe_exception method_name args p =
	match quick_field haxe_exception.etype method_name with
	| FInstance (_,_,cf) as faccess ->
		let efield = { eexpr = TField(haxe_exception,faccess); etype = cf.cf_type; epos = p } in
		let rt =
			match follow cf.cf_type with
			| TFun(_,t) -> t
			| _ ->
				typing_error ((s_type (print_context()) haxe_exception.etype) ^ "." ^ method_name ^ " is not a function and cannot be called") p
		in
		make_call ctx.typer efield args rt p
	| _ -> typing_error ((s_type (print_context()) haxe_exception.etype) ^ "." ^ method_name ^ " is expected to be an instance method") p

(**
	Generate `Std.isOfType(e, t)`
*)
let std_is ctx e t p =
	let t = follow t in
	let std_cls =
		match Typeload.load_type_raise ctx.typer ([],"Std") "Std" p with
		| TClassDecl cls -> cls
		| _ -> typing_error "Std is expected to be a class" p
	in
	let isOfType_field =
		try PMap.find "isOfType" std_cls.cl_statics
		with Not_found -> typing_error ("Std has no field isOfType") p
	in
	let return_type =
		match follow isOfType_field.cf_type with
		| TFun(_,t) -> t
		| _ -> typing_error ("Std.isOfType is not a function and cannot be called") p
	in
	let type_expr = { eexpr = TTypeExpr(module_type_of_type t); etype = t; epos = p } in
	make_static_call ctx.typer std_cls isOfType_field (fun t -> t) [e; type_expr] return_type p

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

(**
	Check if `t` can be thrown without wrapping.
*)
let rec is_native_throw ctx t =
	ctx.throws_anything || is_in_list t ctx.config.ec_native_throws

(**
	Check if `t` can be caught without wrapping.
*)
let rec is_native_catch ctx t =
	ctx.catches_anything || is_in_list t ctx.config.ec_native_catches

(**
	Check if `t` can be used for a Haxe-specific wildcard catch.
	E.g `catch(e:Dynamic)` or `catch(e:haxe.Exception)`
*)
let is_haxe_wildcard_catch ctx t =
	let t = Abstract.follow_with_abstracts t in
	t == t_dynamic || fast_eq ctx.haxe_exception_type t

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
	Check if `v` variable is used in `e` expression
*)
let rec is_var_used v e =
	match e.eexpr with
	| TLocal v2 -> v == v2
	| _ -> check_expr (is_var_used v) e

(**
	Check if `e` contains any throws or try..catches.
*)
let rec contains_throw_or_try e =
	match e.eexpr with
	| TThrow _ | TTry _ -> true
	| _ -> check_expr contains_throw_or_try e

(**
	Check if expression represents an exception wrapped with `haxe.Exception.thrown`
*)
let is_wrapped_exception e =
	match e.eexpr with
	| TMeta ((Meta.WrappedException, _, _), _) -> true
	| _ -> false

(**
	Returns `true` if `e` has to be wrapped with `haxe.Exception.thrown(e)`
	to be thrown.
*)
let requires_wrapped_throw ctx e =
	if ctx.throws_anything || is_wrapped_exception e || ctx.config.ec_special_throw e then
		false
	else
		(*
			Check if `e` is of `haxe.Exception` type directly (not a descendant),
			but not a `new haxe.Exception(...)` expression.
			In this case we delegate the decision to `haxe.Exception.thrown(e)`.
			Because it could happen to be a wrapper for a wildcard catch.
		*)
		let is_stored_haxe_exception() =
			is_haxe_exception ~check_parent:false e.etype
			&& match e.eexpr with
				| TNew(_,_,_) -> false
				| _ -> true
		in
		is_stored_haxe_exception()
		|| (not (is_native_throw ctx e.etype) && not (is_haxe_exception e.etype))

(**
	Generate a throw of a native exception.
*)
let throw_native ctx e_thrown t p =
	let e_native =
		if requires_wrapped_throw ctx e_thrown then
			let thrown = haxe_exception_static_call ctx "thrown" [e_thrown] p in
			let wrapped =
				if is_dynamic ctx.base_throw_type then thrown
				else mk_cast thrown ctx.base_throw_type p
			in
			mk (TMeta ((Meta.WrappedException,[],p),wrapped)) thrown.etype p
		else
			e_thrown
	in
	mk (TThrow e_native) t p

let set_needs_exception_stack v =
	if not (Meta.has Meta.NeedsExceptionStack v.v_meta) then
		v.v_meta <- (Meta.NeedsExceptionStack,[],null_pos) :: v.v_meta

class catch ctx catch_local catch_pos =
	object (self)
		val mutable hx_exception_var = None
		val mutable unwrapped_var = None

		method get_haxe_exception p =
			let v =
				match hx_exception_var with
				| None ->
					let v = alloc_var VGenerated "`" ctx.haxe_exception_type p in
					hx_exception_var <- Some v;
					v
				| Some v -> v
			in
			mk (TLocal v) v.v_type p

		method unwrap p =
			let v =
				match unwrapped_var with
				| None ->
					let v = alloc_var VGenerated "`" t_dynamic p in
					unwrapped_var <- Some v;
					(* unwrapped_local <- Some e; *)
					v
				| Some e -> e
			in
			mk (TLocal v) v.v_type p

		method declare_haxe_exception p =
			match hx_exception_var with
			| Some v ->
				let caught = haxe_exception_static_call ctx "caught" [catch_local] p in
				mk (TVar (v, Some caught)) ctx.basic.tvoid p
			| None ->
				mk (TBlock[]) ctx.basic.tvoid p

		method declare_unwrap p =
			match unwrapped_var with
			| Some v ->
				let unwrap = haxe_exception_instance_call ctx (self#get_haxe_exception p) "unwrap" [] p in
				mk (TVar (v, Some unwrap)) ctx.basic.tvoid p
			| None ->
				mk (TBlock[]) ctx.basic.tvoid p
	end

(**
	Transforms the set of catch-blocks into `if(Std.is(e, ExceptionType)`-fest.

	`t` - the type of `try...catch` expression under transformation.
*)
let catches_to_ifs ctx catches t p =
	match catches with
	| [] -> []
	| ((first_v, first_body) :: _) as rest ->
		let catch_var = alloc_var VGenerated "`" ctx.wildcard_catch_type first_v.v_pos in
		add_var_flag catch_var VCaught;
		let catch_local = mk (TLocal catch_var) catch_var.v_type catch_var.v_pos in
		let body =
			let catch = new catch ctx catch_local p in
			let rec transform = function
				| (v, body) :: rest ->
					let current_t = Abstract.follow_with_abstracts v.v_type in
					let var_used = is_var_used v body in
					(* catch(e:ExtendsHaxeError) *)
					if is_haxe_exception current_t then
						let condition =
							(* catch(e:haxe.Exception) is a wildcard catch *)
							if fast_eq ctx.haxe_exception_type current_t then
								mk (TConst (TBool true)) ctx.basic.tbool v.v_pos
							else
								std_is ctx (catch#get_haxe_exception v.v_pos) v.v_type v.v_pos
						in
						let body =
							if var_used then
								mk (TBlock [
									(* var v:ExceptionType = cast haxe_exception_local; *)
									mk (TVar (v, Some (mk_cast (catch#get_haxe_exception v.v_pos) v.v_type v.v_pos))) ctx.basic.tvoid v.v_pos;
									body
								]) body.etype body.epos
							else
								body
						in
						compose condition body rest
					(* catch(e:Dynamic) *)
					else if current_t == t_dynamic then
						begin
							set_needs_exception_stack catch_var;
							(* this is a wildcard catch *)
							let condition = mk (TConst (TBool true)) ctx.basic.tbool v.v_pos in
							let body =
								mk (TBlock [
									if var_used then
										(* `var v:Dynamic = catch_local;` or `var v:Dynamic = haxe_exception_local.unwrap();` *)
										let e =
											if ctx.catches_anything then catch_local
											else catch#unwrap v.v_pos
										in
										mk (TVar (v, Some e)) ctx.basic.tvoid v.v_pos
									else
										mk (TBlock[]) ctx.basic.tvoid v.v_pos;
									body
								]) body.etype body.epos
							in
							compose condition body rest
						end
					(* catch(e:NativeWildcardException) *)
					else if fast_eq ctx.wildcard_catch_type current_t then
						begin
							set_needs_exception_stack catch_var;
							(* this is a wildcard catch *)
							let condition = mk (TConst (TBool true)) ctx.basic.tbool v.v_pos in
							let body =
								mk (TBlock [
									(* var v:NativeWildcardException = catch_var; *)
									if var_used then
										mk (TVar (v, Some catch_local)) ctx.basic.tvoid v.v_pos
									else
										mk (TBlock[]) ctx.basic.tvoid v.v_pos;
									body
								]) body.etype body.epos
							in
							compose condition body rest
						end
					(* catch(e:AnythingElse) *)
					else begin
						set_needs_exception_stack catch_var;
						let condition =
							(* Std.isOfType(haxe_exception_local.unwrap(), ExceptionType) *)
							std_is ctx (catch#unwrap v.v_pos) v.v_type v.v_pos
						in
						let body =
							mk (TBlock [
								(* var v:ExceptionType = cast haxe_exception_local.unwrap() *)
								if var_used then
									mk (TVar (v, Some (mk_cast (catch#unwrap v.v_pos) v.v_type v.v_pos))) ctx.basic.tvoid v.v_pos
								else
									mk (TBlock[]) ctx.basic.tvoid v.v_pos;
								body
							]) body.etype body.epos
						in
						compose condition body rest
					end
				| [] -> mk (TThrow catch_local) t p
			and compose condition body rest_catches =
				let else_body =
					match rest_catches with
					| [] -> mk (TThrow catch_local) (mk_mono()) p
					| _ -> transform rest_catches
				in
				mk (TIf(condition, body, Some else_body)) t p
			in
			let transformed_catches = transform rest in
			(* haxe.Exception.caught(catch_var) *)
			let exprs = [
				(* var haxe_exception_local = haxe.Exception.caught(catch_var); *)
				catch#declare_haxe_exception catch_var.v_pos;
				(* var unwrapped_local = haxe_exception_local.unwrap(); *)
				catch#declare_unwrap catch_var.v_pos;
				transformed_catches
			] in
			mk (TBlock exprs) t p
		in (* let body =  *)
		[(catch_var,body)]

(**
	Transforms set of catches into
	```
	catch(e:ValueException) {
		if(Std.isOfType(e.value, Exception1)) {
			<...>
		} else if(Std.isOfType(e.value, Exception2)) {
			<...>
		} else <...>
	}
	```
*)
let catches_as_value_exception ctx non_value_exception_catches value_exception_catch t p =
	match non_value_exception_catches, value_exception_catch with
	| [], None ->
		die ~p "Nothing to transform into ValueException catch" __LOC__
	| [], Some catch ->
		catch
	| (first_v,_) :: _, _ ->
		let catch_var =
			match value_exception_catch with
			| Some (catch_var, _) ->
				catch_var
			| None ->
				let catch_var = alloc_var VGenerated "`" ctx.value_exception_type first_v.v_pos in
				add_var_flag catch_var VCaught;
				catch_var
		in
		let catch_local =
			mk (TLocal catch_var) catch_var.v_type catch_var.v_pos
		in
		(* catch_local.value *)
		let catch_local_value =
			let cf =
				try PMap.find "value" ctx.value_exception_class.cl_fields
				with Not_found -> die "haxe.ValueException is missing field \"value\"" __LOC__
			in
			mk (TField (catch_local, FInstance (ctx.value_exception_class,[],cf))) cf.cf_type catch_local.epos
		in
		let rec traverse catches final_else =
			match catches with
			| [] -> final_else
			| (v,body) :: rest ->
				set_needs_exception_stack catch_var;
				(* Std.isOfType(catch_local.value, ExceptionType) *)
				let condition = std_is ctx catch_local_value v.v_type v.v_pos in
				let body =
					mk (TBlock [
						(* var v:ExceptionType = cast catch_local.value *)
						if is_var_used v body then
							mk (TVar (v, Some (mk_cast catch_local_value v.v_type v.v_pos))) ctx.basic.tvoid v.v_pos
						else
							mk (TBlock[]) ctx.basic.tvoid v.v_pos;
						body
					]) body.etype body.epos
				in
				mk (TIf (condition,body,Some (traverse rest final_else))) t p
		in
		let final_else =
			Option.map_default
				(fun (_,body) -> body)
				(mk (TThrow catch_local) t_dynamic p)
				value_exception_catch
		in
		(catch_var, traverse non_value_exception_catches final_else)

(**
	Transform user-written `catches` to a set of catches, which would not require
	special handling in the target generator.

	For example:
	```
	} catch(e:SomeNativeError) {
		doStuff();
	} catch(e:String) {
		trace(e);
	}
	```
	is transformed into
	```
	} catch(e:SomeNativeError) {
		doStuff();
	} catch(etmp:WildCardNativeException) {
		var ehx:haxe.Exception = haxe.Exception.caught(etmp);
		if(Std.isOfType(ehx.unwrap(), String)) {
			var e:String = ehx.unwrap();
			trace(e);
		} else {
			throw etmp;
		}
	}
	```
*)
let catch_native ctx catches t p =
	let rec transform handle_as_value_exception value_exception_catch catches =
		match catches with
		| [] ->
			(match handle_as_value_exception, value_exception_catch with
			| [], None ->
				[]
			| [], Some catch ->
				catches_to_ifs ctx [catch] t p
			| _, _ ->
				[catches_as_value_exception ctx handle_as_value_exception None t p]
				@ Option.map_default (fun catch -> catches_to_ifs ctx [catch] t p) [] value_exception_catch
			)
		(* Haxe-specific wildcard catches should go to if-fest because they need additional handling *)
		| (v,_) :: _ when is_haxe_wildcard_catch ctx v.v_type ->
			(match handle_as_value_exception with
			| [] ->
				catches_to_ifs ctx catches t p
			| _ ->
				catches_as_value_exception ctx handle_as_value_exception None t p
				:: catches_to_ifs ctx catches t p
			)
		| (v,_) as current :: rest when ctx.catches_anything && fast_eq ctx.value_exception_type (Abstract.follow_with_abstracts v.v_type) ->
			catches_as_value_exception ctx handle_as_value_exception (Some current) t p
			:: transform [] (Some (Option.default current value_exception_catch)) rest
		(* Keep catches for native exceptions intact *)
		| (v,_) as current :: rest when (is_native_catch ctx v.v_type) ->
			let handle_as_value_exception =
				(*
					If current target can catch any type natively, then we also need
					to check if `new haxe.ValueException(value)` with the same type of
					`value` was thrown. That is, we should be able to catch `throw 'error'`
					and `throw new ValueException('error')` with a single `catch(e:String)`
					expression in user's code to be consistent with targets which can't
					catch arbitrary types.
				*)
				if ctx.catches_anything && not (is_haxe_exception v.v_type) then
					current :: handle_as_value_exception
				else
					handle_as_value_exception
			in
			current :: (transform handle_as_value_exception value_exception_catch rest)
		(* everything else goes to if-fest *)
		| catches ->
			catches_to_ifs ctx (handle_as_value_exception @ catches) t p
	in
	transform [] None catches

(**
	Transform `throw` and `try..catch` expressions.
	`rename_locals` is required to deal with the names of temp vars.
*)
let filter tctx =
	let stub e = e in
	match tctx.com.platform with (* TODO: implement for all targets *)
	| Php | Js | Java | Cs | Python | Lua | Eval | Neko | Flash | Hl | Cpp ->
		let config = tctx.com.config.pf_exceptions in
		let tp (pack,name) =
			match List.rev pack with
			| module_name :: pack_rev when not (Ast.is_lower_ident module_name) ->
				(mk_type_path ~sub:name (List.rev pack_rev,module_name), null_pos)
			| _ ->
				(mk_type_path (pack,name), null_pos)
		in
		let wildcard_catch_type =
			let t = Typeload.load_instance tctx (tp config.ec_wildcard_catch) true in
			if is_dynamic t then t_dynamic
			else t
		and base_throw_type =
			let t = Typeload.load_instance tctx (tp config.ec_base_throw) true in
			if is_dynamic t then t_dynamic
			else t
		and haxe_exception_type, haxe_exception_class =
			match Typeload.load_instance tctx (tp haxe_exception_type_path) true with
			| TInst(cls,_) as t -> t,cls
			| _ -> typing_error "haxe.Exception is expected to be a class" null_pos
		and value_exception_type, value_exception_class =
			match Typeload.load_instance tctx (tp value_exception_type_path) true with
			| TInst(cls,_) as t -> t,cls
			| _ -> typing_error "haxe.ValueException is expected to be a class" null_pos
		and haxe_native_stack_trace =
			match Typeload.load_instance tctx (tp (["haxe"],"NativeStackTrace")) true with
			| TInst(cls,_) -> cls
			| TAbstract({ a_impl = Some cls },_) -> cls
			| _ -> typing_error "haxe.NativeStackTrace is expected to be a class or an abstract" null_pos
		in
		let is_path_of_dynamic (pack,name) =
			name = "Dynamic" && (pack = [] || pack = ["StdTypes"])
		in
		let ctx = {
			typer = tctx;
			basic = tctx.t;
			config = config;
			wildcard_catch_type = wildcard_catch_type;
			base_throw_type = base_throw_type;
			throws_anything = is_path_of_dynamic config.ec_base_throw && config.ec_avoid_wrapping;
			catches_anything = is_path_of_dynamic config.ec_wildcard_catch && config.ec_avoid_wrapping;
			haxe_exception_class = haxe_exception_class;
			haxe_exception_type = haxe_exception_type;
			haxe_native_stack_trace = haxe_native_stack_trace;
			value_exception_type = value_exception_type;
			value_exception_class = value_exception_class;
		} in
		let rec run e =
			match e.eexpr with
			| TThrow e1 ->
				{ e with eexpr = TThrow (throw_native ctx (run e1) e.etype e.epos) }
			| TTry(e1,catches) ->
				let catches =
					let catches = List.map (fun (v,e) -> (v,run e)) catches in
					(catch_native ctx catches e.etype e.epos)
				in
				{ e with eexpr = TTry(run e1,catches) }
			| _ ->
				map_expr run e
		in
		(fun e ->
			if contains_throw_or_try e then run e
			else stub e
		)
	| Cross -> stub

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
			| _ -> typing_error "haxe.NativeStackTrace is expected to be a class or an abstract" null_pos
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
					with Not_found -> typing_error ("haxe.NativeStackTrace has no field saveStack") null_pos
				in
				let return_type =
					match follow method_field.cf_type with
					| TFun(_,t) -> t
					| _ -> typing_error ("haxe.NativeStackTrace." ^ method_field.cf_name ^ " is not a function and cannot be called") null_pos
				in
				let catch_local = mk (TLocal catch_var) catch_var.v_type catch_var.v_pos in
				begin
					add_dependency tctx.curclass.cl_module native_stack_trace_cls.cl_module;
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
	let tp = (mk_type_path haxe_exception_type_path, null_pos) in
	match Typeload.load_instance tctx tp true with
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
						with Not_found -> typing_error "haxe.Exception has no field __shiftStack" p
					in
					match faccess with
					| FInstance (_,_,cf) ->
						let efield = { eexpr = TField(this,faccess); etype = cf.cf_type; epos = p } in
						let rt =
							match follow cf.cf_type with
							| TFun(_,t) -> t
							| _ ->
								typing_error "haxe.Exception.__shiftStack is not a function and cannot be called" cf.cf_name_pos
						in
						make_call tctx efield [] rt p
					| _ -> typing_error "haxe.Exception.__shiftStack is expected to be an instance method" p
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
