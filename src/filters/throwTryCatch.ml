open Globals
open Ast
open Type
open Common
open Typecore
open TyperBase
open Fields
open Error

let haxe_error_type_path = (["haxe"],"Error")

type context = {
	typer : typer;
	basic : basic_types;
	native_exception_type : Type.t;
	haxe_error_class : tclass;
	haxe_error_type : Type.t;
	haxe_value_error_class : tclass;
	haxe_value_error_type : Type.t
}

(**
	Generate `haxe.Error.method_name(args)`
*)
let haxe_error_static_call ctx method_name args p =
	let method_field =
		try PMap.find method_name ctx.haxe_error_class.cl_statics
		with Not_found -> error ("haxe.Error has no field " ^ method_name) p
	in
	let return_type =
		match follow method_field.cf_type with
		| TFun(_,t) -> t
		| _ -> error ("haxe.Error." ^ method_name ^ " is not a function and cannot be called") p
	in
	make_static_call ctx.typer ctx.haxe_error_class method_field (fun t -> t) args return_type p

(**
	Generate `haxe_error.method_name(args)`
*)
let haxe_error_instance_call ctx haxe_error method_name args p =
	match quick_field haxe_error.etype method_name with
	| FInstance (_,_,cf) as faccess ->
		let efield = { eexpr = TField(haxe_error,faccess); etype = cf.cf_type; epos = p } in
		let rt =
			match follow cf.cf_type with
			| TFun(_,t) -> t
			| _ ->
				error ((s_type (print_context()) haxe_error.etype) ^ "." ^ method_name ^ " is not a function and cannot be called") p
		in
		make_call ctx.typer efield args rt p
	| _ -> error ((s_type (print_context()) haxe_error.etype) ^ "." ^ method_name ^ " is expected to be an instance method") p

(**
	Generate `Std.isOfType(e, t)`
*)
let std_is_call ctx e t p =
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
	Check if `t` can be used as native target exception type.
*)
let rec is_native_exception ctx t =
	match follow ctx.native_exception_type with
	| TInst({ cl_path = native_exception_path },_) ->
		let rec check cls =
			cls.cl_path = native_exception_path
			|| List.exists (fun (cls,_) -> check cls) cls.cl_implements
			|| Option.map_default (fun (cls,_) -> check cls) false cls.cl_super
		in
		(match follow t with
		| TInst (cls, _) -> check cls
		| _ -> false
		)
	| _ -> false

(**
	Check if `t` is or extends `haxe.Error`
*)
let rec is_haxe_error ?(check_parent=true) (t:Type.t) =
	let rec check cls =
		cls.cl_path = haxe_error_type_path
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
		(* already a native exception *)
		if is_native_exception ctx e_thrown.etype then
			e_thrown
		else begin
			(* Wrap any value into `haxe.Error` instance *)
			let haxe_error =
				(* already a `haxe.Error` instance *)
				if is_haxe_error e_thrown.etype then
					e_thrown
				(* throwing dynamic values: generate `haxe.Error.ofAny(e_thrown)` *)
				else if (follow e_thrown.etype) == t_dynamic then
					haxe_error_static_call ctx "ofAny" [e_thrown] p
				(* throwing other values: generate `new haxe.ValueError(e_thrown)` *)
				else
					mk (TNew(ctx.haxe_value_error_class,[],[e_thrown])) ctx.haxe_value_error_type p
			in
			(* generate `haxe_error.get_native()` *)
			haxe_error_instance_call ctx haxe_error "get_native" [] e_thrown.epos
		end
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
		var ehx:haxe.Error = haxe.Error.ofNative(etmp);
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
		| (v,_) as current :: rest when (is_native_exception ctx v.v_type)
			(* in case haxe.Error extends native exception on current target *)
			&& not (fast_eq ctx.haxe_error_type (follow v.v_type)) ->
			current :: (transform rest)
		| [] -> []
		(* Everything else falls into `if(Std.is(e, ExceptionType){`-fest *)
		| rest ->
			let catch_var = gen_local ctx.typer ctx.native_exception_type null_pos in
			let catch_local = mk (TLocal catch_var) catch_var.v_type null_pos in
			(* __saveExceptionStack__(catch_var) *)
			let capture_stack() =
				(* TODO: generate `haxe.CallStack.captureExceptionStack(capture_var)` *)
				if false && has_feature ctx.typer.com "haxe.CallStack.exceptionStack" then
					let fn = { eexpr = TIdent "__saveExceptionStack__"; etype = mk_mono(); epos = null_pos } in
					{ eexpr = TCall (fn, [catch_local]); etype = ctx.basic.tvoid; epos = null_pos }
				else
					mk (TBlock[]) ctx.basic.tvoid null_pos
			in
			let body =
				let haxe_error_var = gen_local ctx.typer ctx.haxe_error_type null_pos in
				let haxe_error_local = mk (TLocal haxe_error_var) haxe_error_var.v_type null_pos in
				let unwrap() =
					(* haxe_error_local.unwrap(); *)
					haxe_error_instance_call ctx haxe_error_local "unwrap" [] null_pos;
				in
				let rec transform = function
					(* catch(e:ExtendsHaxeError) *)
					| (v, body) :: rest when is_haxe_error v.v_type ->
						let condition =
							(* catch(e:haxe.Error) is a wildcard catch *)
							if fast_eq ctx.haxe_error_type (follow v.v_type) then
								mk (TConst (TBool true)) ctx.basic.tbool v.v_pos
							else
								std_is_call ctx haxe_error_local v.v_type v.v_pos
						in
						let body =
							mk (TBlock [
								(* var v:ExceptionType = cast haxe_error_local; *)
								mk (TVar (v, Some (mk_cast haxe_error_local v.v_type null_pos))) ctx.basic.tvoid null_pos;
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
								capture_stack();
								(* var v:Dynamic = haxe_error_local.unwrap(); *)
								mk (TVar (v, Some (unwrap()))) ctx.basic.tvoid null_pos;
								body
							]) body.etype body.epos
						in
						compose condition body rest
					(* catch(e:AnythingElse) *)
					| (v, body) :: rest ->
						let condition =
							(* Std.isOfType(haxe_error_local.unwrap(), ExceptionType) *)
							std_is_call ctx (unwrap()) v.v_type v.v_pos
						in
						let body =
							mk (TBlock [
								capture_stack();
								(* var v:ExceptionType = cast haxe_error_local.unwrap() *)
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
				(* haxe.Error.ofNative(catch_var) *)
				let ofNative_call = haxe_error_static_call ctx "ofNative" [catch_local] null_pos in
				let exprs = [
					(* var haxe_error_var = haxe.Error.ofNative(catch_var); *)
					(mk (TVar (haxe_error_var, Some ofNative_call)) ctx.basic.tvoid null_pos);
					transform rest
				] in
				mk (TBlock exprs) t p
			in (* let body =  *)
			[(catch_var,body)]
	in
	transform catches

let filter tctx =
	match tctx.com.platform with (* TODO: implements for all targets *)
	| Php ->
		let native_exception_type =
			match Typeload.load_type_raise tctx haxe_error_type_path "NativeException" null_pos with
			| TTypeDecl td -> TType(td,[])
			| _ -> error "haxe.Error.NativeExtension is expected to be a typedef" null_pos
		and haxe_error_class =
			match Typeload.load_type_raise tctx haxe_error_type_path "Error" null_pos with
			| TClassDecl cls -> cls
			| _ -> error "haxe.Error is expected to be a class" null_pos
		and haxe_value_error_class =
			match Typeload.load_type_raise tctx haxe_error_type_path "ValueError" null_pos with
			| TClassDecl cls -> cls
			| _ -> error "haxe.ValueError is expected to be a class" null_pos
		in
		let ctx = {
			typer = tctx;
			basic = tctx.t;
			native_exception_type = native_exception_type;
			haxe_error_class = haxe_error_class;
			haxe_error_type = TInst(haxe_error_class,[]);
			haxe_value_error_class = haxe_value_error_class;
			haxe_value_error_type = TInst(haxe_value_error_class,[]);
		} in
		let rec run e =
			match e.eexpr with
			| TThrow e1 ->
				{ e with eexpr = TThrow (throw_native ctx (run e1) e.etype e.epos) }
			| TTry(e1,catches) ->
				let catches =
					List.map
						(fun (v,e) -> (v,run e))
						(catch_native ctx catches e.etype e.epos)
				in
				{ e with eexpr = TTry(run e1,catches) }
			| _ ->
				map_expr run e
		in
		run
	| _ -> fun e -> e