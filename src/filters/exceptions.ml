open Globals
open Ast
open Type
open Common
open Typecore
open TyperBase
open Fields
open Error

let haxe_exception_type_path = (["haxe"],"Exception")

type context = {
	typer : typer;
	basic : basic_types;
	config : exceptions_config;
	wildcard_catch_type : Type.t;
	base_throw_type : Type.t;
	haxe_exception_class : tclass;
	haxe_exception_type : Type.t;
	haxe_call_stack_class : tclass;
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
		with Not_found -> error ("haxe.Exception has no field " ^ method_name) p
	in
	let return_type =
		match follow method_field.cf_type with
		| TFun(_,t) -> t
		| _ -> error ("haxe.Exception." ^ method_name ^ " is not a function and cannot be called") p
	in
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
				error ((s_type (print_context()) haxe_exception.etype) ^ "." ^ method_name ^ " is not a function and cannot be called") p
		in
		make_call ctx.typer efield args rt p
	| _ -> error ((s_type (print_context()) haxe_exception.etype) ^ "." ^ method_name ^ " is expected to be an instance method") p

(**
	Generate `Std.isOfType(e, t)`
*)
let std_is ctx e t p =
	let t = follow t in
	let std_cls =
		match Typeload.load_type_raise ctx.typer ([],"Std") "Std" p with
		| TClassDecl cls -> cls
		| _ -> error "Std is expected to be a class" p
	in
	let isOfType_field =
		try PMap.find "isOfType" std_cls.cl_statics
		with Not_found -> error ("Std has no field isOfType") p
	in
	let return_type =
		match follow isOfType_field.cf_type with
		| TFun(_,t) -> t
		| _ -> error ("Std.isOfType is not a function and cannot be called") p
	in
	let type_expr = { eexpr = TTypeExpr(module_type_of_type t); etype = t; epos = null_pos } in
	make_static_call ctx.typer std_cls isOfType_field (fun t -> t) [e; type_expr] return_type p

(**
	Generates `haxe.CallStack.saveExceptionStack(catch_local)` if needed
*)
let save_exception_stack ctx catch_local =
	(* TODO: this `has_feature` always returns `true` if executed before DCE filters *)
	if has_feature ctx.typer.com "haxe.CallStack.exceptionStack" then
		let method_field =
			try PMap.find "saveExceptionStack" ctx.haxe_call_stack_class.cl_statics
			with Not_found -> error ("haxe.Exception has no field saveExceptionStack") null_pos
		in
		let return_type =
			match follow method_field.cf_type with
			| TFun(_,t) -> t
			| _ -> error ("haxe.CallStack." ^ method_field.cf_name ^ " is not a function and cannot be called") null_pos
		in
		make_static_call ctx.typer ctx.haxe_call_stack_class method_field (fun t -> t) [catch_local] return_type null_pos
	else
		mk (TBlock[]) ctx.basic.tvoid null_pos

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
	is_in_list t ctx.config.ec_native_throws

(**
	Check if `t` can be caught without wrapping.
*)
let rec is_native_catch ctx t =
	is_in_list t ctx.config.ec_native_catches

(**
	Check if `t` is or extends `haxe.Exception`
*)
let rec is_haxe_exception ?(check_parent=true) (t:Type.t) =
	let rec check cls =
		cls.cl_path = haxe_exception_type_path
		|| (check_parent && match cls.cl_super with
			| None -> false
			| Some (cls, _) -> check cls
		)
	in
	match follow t with
		| TInst (cls, _) -> check cls
		| _ -> false

(**
	Generate a throw of a native exception.
*)
let throw_native ctx e_thrown t p =
	let e_native =
		(*
			Check if `e_thrown` is of `haxe.Exception` type directly (not a descendant),
			but not a `new haxe.Exception(...)` expression.
			In this case we delegate the decision to `haxe.Exception.thrown(e_thrown)`.
			Because it could happen to be a wrapper for a wildcard catch.
		*)
		let is_stored_haxe_exception() =
			is_haxe_exception ~check_parent:false e_thrown.etype
			&& match e_thrown.eexpr with
				| TNew(_,_,_) -> false
				| _ -> true
		in
		if
			(* already a native exception *)
			(is_native_throw ctx e_thrown.etype && not (is_stored_haxe_exception()))
			(* current target does not have native exceptions and `e_thrown` is `haxe.Exception` *)
			|| (is_dynamic ctx.base_throw_type && is_haxe_exception e_thrown.etype && not (is_stored_haxe_exception()))
		then
			e_thrown
		(* Wrap everything else with `haxe.Exception.thrown` call *)
		else
			let thrown = haxe_exception_static_call ctx "thrown" [e_thrown] p in
			if is_dynamic ctx.base_throw_type then thrown
			else mk_cast thrown ctx.base_throw_type p
	in
	mk (TThrow e_native) t p

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
and catch_native ctx catches t p =
	let rec transform = function
		(* Keep catches for native exceptions intact *)
		| (v,_) as current :: rest when (is_native_catch ctx v.v_type)
			(*
				In case haxe.Exception extends native exception on current target.
				We don't want it to be generated as a native catch.
			*)
			&& not (fast_eq ctx.haxe_exception_type (follow v.v_type)) ->
			current :: (transform rest)
		| [] -> []
		(* Everything else falls into `if(Std.is(e, ExceptionType)`-fest *)
		| rest ->
			let catch_var = gen_local ctx.typer ctx.wildcard_catch_type null_pos in
			let catch_local = mk (TLocal catch_var) catch_var.v_type null_pos in
			let body =
				let haxe_exception_var = gen_local ctx.typer ctx.haxe_exception_type null_pos in
				let haxe_exception_local = mk (TLocal haxe_exception_var) haxe_exception_var.v_type null_pos in
				let unwrapped_var = gen_local ctx.typer t_dynamic null_pos in
				let unwrapped_local = mk (TLocal unwrapped_var) unwrapped_var.v_type null_pos in
				let needs_unwrap = ref false in
				let unwrap() =
					needs_unwrap := true;
					unwrapped_local;
				in
				let rec transform = function
					(* catch(e:ExtendsHaxeError) *)
					| (v, body) :: rest when is_haxe_exception v.v_type ->
						let condition =
							(* catch(e:haxe.Exception) is a wildcard catch *)
							if fast_eq ctx.haxe_exception_type (follow v.v_type) then
								mk (TConst (TBool true)) ctx.basic.tbool v.v_pos
							else
								std_is ctx haxe_exception_local v.v_type v.v_pos
						in
						let body =
							mk (TBlock [
								(* var v:ExceptionType = cast haxe_exception_local; *)
								mk (TVar (v, Some (mk_cast haxe_exception_local v.v_type null_pos))) ctx.basic.tvoid null_pos;
								body
							]) body.etype body.epos
						in
						compose condition body rest
					(* catch(e:Dynamic) *)
					| (v, body) :: rest when (follow v.v_type) == t_dynamic ->
						(* catch(e:Dynamic) is a wildcard catch *)
						let condition = mk (TConst (TBool true)) ctx.basic.tbool v.v_pos in
						let body =
							mk (TBlock [
								save_exception_stack ctx catch_local;
								(* var v:Dynamic = haxe_exception_local.unwrap(); *)
								mk (TVar (v, Some (unwrap()))) ctx.basic.tvoid null_pos;
								body
							]) body.etype body.epos
						in
						compose condition body rest
					(* catch(e:AnythingElse) *)
					| (v, body) :: rest ->
						let condition =
							(* Std.isOfType(haxe_exception_local.unwrap(), ExceptionType) *)
							std_is ctx (unwrap()) v.v_type v.v_pos
						in
						let body =
							mk (TBlock [
								save_exception_stack ctx catch_local;
								(* var v:ExceptionType = cast haxe_exception_local.unwrap() *)
								mk (TVar (v, Some (mk_cast (unwrap()) v.v_type null_pos))) ctx.basic.tvoid null_pos;
								body
							]) body.etype body.epos
						in
						compose condition body rest
					| [] -> mk (TThrow catch_local) t p
				and compose condition body rest_catches =
					let else_body =
						match rest_catches with
						| [] -> mk (TThrow catch_local) (mk_mono()) p
						| _ -> transform rest_catches
					in
					mk (TIf(condition, body, Some else_body)) t p
				in
				(* haxe.Exception.caught(catch_var) *)
				let caught = haxe_exception_static_call ctx "caught" [catch_local] null_pos in
				let exprs = [
					(* var haxe_exception_local = haxe.Exception.caught(catch_var); *)
					(mk (TVar (haxe_exception_var, Some caught)) ctx.basic.tvoid null_pos);
					(* var unwrapped_local = haxe_exception_local.unwrap(); *)
					if !needs_unwrap then
						let unwrap = haxe_exception_instance_call ctx haxe_exception_local "unwrap" [] null_pos in
						mk (TVar (unwrapped_var, Some unwrap)) ctx.basic.tvoid null_pos
					else
						mk (TBlock[]) ctx.basic.tvoid null_pos;
					(* transform catches *)
					transform rest
				] in
				mk (TBlock exprs) t p
			in (* let body =  *)
			[(catch_var,body)]
	in
	transform catches

let filter tctx =
	match tctx.com.platform with (* TODO: implement for all targets *)
	| Php | Js | Java | Cs | Python | Lua | Eval | Neko | Flash | Hl ->
		let config = tctx.com.config.pf_exceptions in
		let tp (pack,name) =
			match List.rev pack with
			| module_name :: pack_rev when not (Ast.is_lower_ident module_name) ->
				({ tpackage = List.rev pack_rev; tname = module_name; tparams = []; tsub = Some name },null_pos)
			| _ ->
				({ tpackage = pack; tname = name; tparams = []; tsub = None },null_pos)
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
			| _ -> error "haxe.Exception is expected to be a class" null_pos
		and haxe_call_stack_class =
			match Typeload.load_instance tctx (tp (["haxe"],"CallStack")) true with
			| TInst(cls,_) -> cls
			| TAbstract({ a_impl = Some cls },_) -> cls
			| _ -> error "haxe.CallStack is expected to be a class or an abstract" null_pos
		in
		let ctx = {
			typer = tctx;
			basic = tctx.t;
			config = config;
			wildcard_catch_type = wildcard_catch_type;
			base_throw_type = base_throw_type;
			haxe_exception_class = haxe_exception_class;
			haxe_exception_type = haxe_exception_type;
			haxe_call_stack_class = haxe_call_stack_class;
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
		run
	| _ -> fun e -> e