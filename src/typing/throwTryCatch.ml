open Globals
open Ast
open Type
open Common
open Typecore
open TyperBase
open Fields
open Error

let haxe_error_type_path = (["haxe"],"Error")
let haxe_value_error_type_path = (["haxe"],"ValueError")

(**
	Get `Type.t` instance for `haxe.Error.NativeException`
*)
let native_exception_type ctx p =
	match Typeload.load_type_raise ctx haxe_error_type_path "NativeException" p with
	| TTypeDecl td -> TType(td,[])
	| _ -> error "haxe.Error.NativeExtension is expected to be a typedef" p

(**
	Get `tclass` instance for `haxe.Error`
*)
let haxe_error_class ctx p =
	match Typeload.load_type_raise ctx haxe_error_type_path "Error" p with
	| TClassDecl cls -> cls
	| _ -> error "haxe.Error is expected to be a class" p

(**
	Get `tclass` instance for `haxe.ValueError`
*)
let haxe_value_error_class ctx p =
	match Typeload.load_type_raise ctx haxe_value_error_type_path "ValueError" p with
	| TClassDecl cls -> cls
	| _ -> error "haxe.ValueError is expected to be a class" p

(**
	Generate `haxe.Error.method_name(args)`
*)
let haxe_error_static_call ctx method_name args p =
	let error_cls = haxe_error_class ctx p in
	let method_field =
		try PMap.find method_name error_cls.cl_statics
		with Not_found -> error ("haxe.Error has no field " ^ method_name) p
	in
	let return_type =
		match follow method_field.cf_type with
		| TFun(_,t) -> t
		| _ -> error ("haxe.Error." ^ method_name ^ " is not a function and cannot be called") p
	in
	make_static_call ctx error_cls method_field (fun t -> t) args return_type p

(**
	Generate `Std.isOfType(e, t)`
*)
let std_is_call ctx e t p =
	let std_cls =
		match Typeload.load_type_raise ctx ([],"Std") "Std" p with
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
	make_static_call ctx std_cls isOfType_field (fun t -> t) [e; type_expr] return_type p

(**
	Check if `t` can be used as native target exception type.
*)
let rec is_native_exception ctx t =
	let native_exception_path =
		match follow (native_exception_type ctx null_pos) with
		| TInst(cls,_) -> cls.cl_path
		| _ -> error "haxe.Error.NativeException must be an alias of a class or interface." null_pos
	in
	let rec check cls =
		cls.cl_path = native_exception_path
		|| List.exists (fun (cls,_) -> check cls) cls.cl_implements
		|| Option.map_default (fun (cls,_) -> check cls) false cls.cl_super
	in
	match follow t with
	| TInst (cls, _) -> check cls
	| _ -> false

(**
	Check if `t` is or extends `haxe.Error`
*)
let rec is_haxe_error ?(check_parent=true) (t:Type.t) =
	let rec check cls =
		cls.cl_path = (["haxe"], "Error")
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
let throw_native ctx e_thrown p =
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
					let error_cls = haxe_value_error_class ctx p in
					mk (TNew(error_cls,[],[e_thrown])) (TInst(error_cls,[])) p
			in
			(* generate `haxe_error.get_native()` *)
			match quick_field haxe_error.etype "get_native" with
			| FInstance (_,_,cf) as faccess ->
				let efield = { eexpr = TField(haxe_error,faccess); etype = cf.cf_type; epos = e_thrown.epos } in
				let rt =
					match follow cf.cf_type with
					| TFun(_,t) -> t
					| _ -> error ("haxe.Error.get_native is not a function and cannot be called") p
				in
				make_call ctx efield [] rt p
			| _ -> error ("haxe.Error.get_native is not an instance field") p
		end
	in
	mk (TThrow e_native) (mk_mono()) p

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
		if(Std.is(ehx, ValueError) && Std.is((cast ehx:ValueError).value, String)) {
			var e:String = ehx;
			trace(e);
		} else {
			throw etmp;
		}
	}
	```
*)
and catch_native ctx catches t p =
	let capture_stack catch_var =
		(* TODO *)
		mk (TBlock []) (mk_mono()) null_pos
	in
	let haxe_error_type = TInst(haxe_error_class ctx p,[]) in
	let rec transform = function
		(* Keep catches for native exceptions intact *)
		| (v,_) as current :: rest when (is_native_exception ctx v.v_type)
			(* in case haxe.Error extends native exception on current target *)
			&& not (fast_eq haxe_error_type (follow v.v_type)) ->
			current :: (transform rest)
		| [] -> []
		(* Everything else falls into `if(Std.is(e, ExceptionType){`-fest *)
		| rest ->
			let catch_var = gen_local ctx (native_exception_type ctx p) null_pos in
			let catch_local = mk (TLocal catch_var) catch_var.v_type null_pos in
			let body =
				let value_error_class = haxe_value_error_class ctx p in
				let value_error_type = TInst(value_error_class,[]) in
				let haxe_error_var = gen_local ctx haxe_error_type null_pos in
				let haxe_error_local = mk (TLocal haxe_error_var) haxe_error_var.v_type null_pos in
				(* (cast haxe_error_local:ValueError).value *)
				let get_value() =
					let cast_to_value_error = mk_cast haxe_error_local value_error_type null_pos in
					match quick_field value_error_type "value" with
					| FInstance (_,_,cf) as value_access ->
						mk (TField(cast_to_value_error,value_access)) cf.cf_type null_pos
					| _ -> error "haxe.ValueError.value is not an instance field" p
				(* Std.isOfType(haxe_error_local, ValueError) *)
				and is_value() =
					std_is_call ctx haxe_error_local value_error_type null_pos
				in
				let rec transform = function
					(* catch(e:ExtendsHaxeError) *)
					| (v, body) :: rest when is_haxe_error v.v_type ->
						let condition =
							(* catch(e:haxe.Error) is a wildcard catch *)
							if fast_eq haxe_error_type (follow v.v_type) then
								mk (TConst (TBool true)) ctx.t.tbool v.v_pos
							else
								std_is_call ctx haxe_error_local v.v_type v.v_pos
						in
						let body =
							mk (TBlock [
								(* var v:ExceptionType = cast haxe_error_local; *)
								mk (TVar (v, Some (mk_cast haxe_error_local v.v_type null_pos))) ctx.t.tvoid null_pos;
								body
							]) body.etype body.epos
						in
						let else_body =
							match rest with
							| [] -> mk (TThrow catch_local) (mk_mono()) p
							| _ -> transform rest
						in
						mk (TIf(condition, body, Some else_body)) t p
					(* catch(e:Dynamic) *)
					| (v, body) :: rest when (follow v.v_type) == t_dynamic ->
						(* catch(e:Dynamic) is a wildcard catch *)
						let condition = mk (TConst (TBool true)) ctx.t.tbool v.v_pos in
						let body =
							mk (TBlock [
								capture_stack();
								(*
									var v:Dynamic = if(Std.isOfType(haxe_error_local, ValueError))
										(cast haxe_error_local:ValueError).value
									else
										haxe_error_local;
								*)
								begin
									let eif =
										mk (TIf(is_value(), get_value(), Some haxe_error_local)) t_dynamic null_pos
									in
									mk (TVar (v, Some eif)) ctx.t.tvoid null_pos;
								end;
								body
							]) body.etype body.epos
						in
						let else_body =
							match rest with
							| [] -> mk (TThrow catch_local) (mk_mono()) p
							| _ -> transform rest
						in
						mk (TIf(condition, body, Some else_body)) t p
					(* catch(e:AnythingElse) *)
					| (v, body) :: rest ->
						let condition =
							(*
								Std.isOfType(haxe_error_local, ValueError)
								&& Std.isOfType((cast haxe_error_local:ValueError).value, ExceptionType)
							*)
							let check_value = std_is_call ctx (get_value()) v.v_type null_pos in
							mk (TBinop(OpBoolAnd, is_value(), check_value)) ctx.t.tbool v.v_pos
						in
						let body =
							mk (TBlock [
								(* var v:ExceptionType = cast (cast haxe_error_local:ValueError).value *)
								mk (TVar (v, Some (mk_cast (get_value()) v.v_type null_pos))) ctx.t.tvoid null_pos;
								body
							]) body.etype body.epos
						in
						let else_body =
							match rest with
							| [] -> mk (TThrow catch_local) (mk_mono()) p
							| _ -> transform rest
						in
						mk (TIf(condition, body, Some else_body)) t p
					| [] -> mk (TThrow catch_local) t p
				in
				(* haxe.Error.ofNative(catch_var) *)
				let ofNative_call = haxe_error_static_call ctx "ofNative" [catch_local] null_pos in
				let exprs = [
					(* var haxe_error_var = haxe.Error.ofNative(catch_var); *)
					(mk (TVar (haxe_error_var, Some ofNative_call)) ctx.t.tvoid null_pos);
					transform rest
				] in
				mk (TBlock exprs) t p
			in (* let body =  *)
			[(catch_var,body)]
	in
	transform catches
