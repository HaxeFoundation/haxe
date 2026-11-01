open Globals
open Type
open PlatformConfig
open Error
open ExceptionFunctions

type context = {
	scom : SafeCom.t;
	basic : basic_types;
	config : exceptions_config;
	wildcard_catch_type : Type.t;
	base_throw_type : Type.t;
	throws_anything : bool;
	catches_anything : bool;
	haxe_exception : (Type.t * tclass) AtomicLazy.t;
	haxe_native_stack_trace : tclass AtomicLazy.t;
	value_exception : (Type.t * tclass) AtomicLazy.t;
	is_of_type : (tclass * tclass_field * Type.t);
}

let haxe_exception_class ctx =
	let cls = snd (AtomicLazy.force ctx.haxe_exception) in
	assert (ctx.scom.curclass != null_class);
	add_dependency ctx.scom.curclass.cl_module cls.cl_module MDepFromTyping;
	cls

let haxe_exception_type ctx =
	let t,cls = AtomicLazy.force ctx.haxe_exception in
	assert (ctx.scom.curclass != null_class);
	add_dependency ctx.scom.curclass.cl_module cls.cl_module MDepFromTyping;
	t

let value_exception_class ctx =
	let cls = snd (AtomicLazy.force ctx.value_exception) in
	assert (ctx.scom.curclass != null_class);
	add_dependency ctx.scom.curclass.cl_module cls.cl_module MDepFromTyping;
	cls

let value_exception_type ctx =
	let t,cls = AtomicLazy.force ctx.value_exception in
	assert (ctx.scom.curclass != null_class);
	add_dependency ctx.scom.curclass.cl_module cls.cl_module MDepFromTyping;
	t

(**
	Generate `haxe.Exception.method_name(args)`
*)
let haxe_exception_static_call ctx method_name args p =
	let method_field =
		try PMap.find method_name (haxe_exception_class ctx).cl_statics
		with Not_found -> raise_typing_error ("haxe.Exception has no field " ^ method_name) p
	in
	let return_type =
		match follow method_field.cf_type with
		| TFun(_,t) -> t
		| _ -> raise_typing_error ("haxe.Exception." ^ method_name ^ " is not a function and cannot be called") p
	in
	make_static_call ctx.scom (haxe_exception_class ctx) method_field args return_type p

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
				raise_typing_error ((s_type (print_context()) haxe_exception.etype) ^ "." ^ method_name ^ " is not a function and cannot be called") p
		in
		make_call ctx.scom efield args rt p
	| _ -> raise_typing_error ((s_type (print_context()) haxe_exception.etype) ^ "." ^ method_name ^ " is expected to be an instance method") p

let add_meta_exception_type_check e =
	mk (TMeta((Meta.ExceptionTypeCheck,[],e.epos),e)) e.etype e.epos

(**
	Generate `Std.isOfType(e, t)`
*)
let std_is ctx e t p =
	let t = follow t in
	let type_expr = TyperBase.type_module_type_simple (module_type_of_type t) p in
	let (std_cls,isOfType_field,return_type) = ctx.is_of_type in
	let e = make_static_call ctx.scom std_cls isOfType_field [e; type_expr] return_type p in
	add_meta_exception_type_check e

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
let is_native_throw ctx t =
	ctx.throws_anything || is_in_list t ctx.config.ec_native_throws

(**
	Check if `t` can be caught without wrapping.
*)
let is_native_catch ctx t =
	ctx.catches_anything || is_in_list t ctx.config.ec_native_catches

(**
	Check if `t` can be used for a Haxe-specific wildcard catch.
	E.g `catch(e:Dynamic)` or `catch(e:haxe.Exception)`
*)
let is_haxe_wildcard_catch ctx t =
	let t = Abstract.follow_with_abstracts t in
	t == t_dynamic || fast_eq (haxe_exception_type ctx) t


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
			mk (TMeta ((Meta.WrappedException,[],p),wrapped)) wrapped.etype p
		else
			e_thrown
	in
	e_native

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
					let v = alloc_var VGenerated gen_local_prefix (haxe_exception_type ctx) p in
					hx_exception_var <- Some v;
					v
				| Some v -> v
			in
			mk (TLocal v) v.v_type p

		method unwrap p =
			let v =
				match unwrapped_var with
				| None ->
					let v = alloc_var VGenerated gen_local_prefix t_dynamic p in
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
		let catch_var = alloc_var VGenerated gen_local_prefix ctx.wildcard_catch_type first_v.v_pos in
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
							if fast_eq (haxe_exception_type ctx) current_t then
								add_meta_exception_type_check (mk (TConst (TBool true)) ctx.basic.tbool v.v_pos)
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
				let catch_var = alloc_var VGenerated gen_local_prefix (value_exception_type ctx) first_v.v_pos in
				add_var_flag catch_var VCaught;
				catch_var
		in
		let catch_local =
			mk (TLocal catch_var) catch_var.v_type catch_var.v_pos
		in
		(* catch_local.value *)
		let catch_local_value =
			let cf =
				try PMap.find "value" (value_exception_class ctx).cl_fields
				with Not_found -> die "haxe.ValueException is missing field \"value\"" __LOC__
			in
			mk (TField (catch_local, FInstance (value_exception_class ctx,[],cf))) cf.cf_type catch_local.epos
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
			(match handle_as_value_exception, value_exception_catch with
			| [], None ->
				catches_to_ifs ctx catches t p
			| [], Some catch ->
				catches_to_ifs ctx (catch :: catches) t p
			| _ ->
				catches_as_value_exception ctx handle_as_value_exception None t p
				:: catches_to_ifs ctx catches t p
			)
		| (v,_) as current :: rest when ctx.catches_anything && fast_eq (value_exception_type ctx) (Abstract.follow_with_abstracts v.v_type) ->
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
let filter ectx (scom:SafeCom.t) =
	let stub e = e in
	match ectx with
	| Some ctx ->
		let ctx = { ctx with scom } in
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
	| None ->
		stub
