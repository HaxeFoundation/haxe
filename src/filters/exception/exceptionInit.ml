open Globals
open Ast
open Common
open Error
open Exceptions
open Type
open Typecore
open ExceptionFunctions

let create_exception_context tctx =
	match tctx.com.platform with (* TODO: implement for all targets *)
	| Php | Js | Jvm | Python | Lua | Eval | Neko | Flash | Hl | Cpp ->
		let config = tctx.com.config.pf_exceptions in
		let tp (pack,name) =
			let tp = match List.rev pack with
			| module_name :: pack_rev when not (Ast.is_lower_ident module_name) ->
				mk_type_path ~sub:name (List.rev pack_rev,module_name)
			| _ ->
				mk_type_path (pack,name)
			in
			make_ptp tp null_pos
		in
		let wildcard_catch_type =
			let t = Typeload.load_instance tctx (tp config.ec_wildcard_catch) ParamSpawnMonos LoadNormal in
			if is_dynamic t then t_dynamic
			else t
		and base_throw_type =
			let t = Typeload.load_instance tctx (tp config.ec_base_throw) ParamSpawnMonos LoadNormal in
			if is_dynamic t then t_dynamic
			else t
		and haxe_exception = AtomicLazy.from_fun (fun () ->
			match Typeload.load_instance tctx (tp haxe_exception_type_path) ParamSpawnMonos LoadNormal with
			| TInst(cls,_) as t -> t,cls
			| _ -> raise_typing_error "haxe.Exception is expected to be a class" null_pos)
		and value_exception = AtomicLazy.from_fun (fun () ->
			match Typeload.load_instance tctx (tp value_exception_type_path) ParamSpawnMonos LoadNormal with
			| TInst(cls,_) as t -> t,cls
			| _ -> raise_typing_error "haxe.ValueException is expected to be a class" null_pos)
		and haxe_native_stack_trace = AtomicLazy.from_fun (fun () ->
			match Typeload.load_instance tctx (tp (["haxe"],"NativeStackTrace")) ParamSpawnMonos LoadNormal with
			| TInst(cls,_) -> cls
			| TAbstract({ a_impl = Some cls },_) -> cls
			| _ -> raise_typing_error "haxe.NativeStackTrace is expected to be a class or an abstract" null_pos)
		in
		let is_path_of_dynamic (pack,name) =
			name = "Dynamic" && (pack = [] || pack = ["StdTypes"])
		in
		let is_of_type =
			let std_cls = tctx.com.std in
			let isOfType_field =
				try PMap.find "isOfType" std_cls.cl_statics
				with Not_found -> raise_typing_error ("Std has no field isOfType") null_pos
			in
			let return_type =
				match follow isOfType_field.cf_type with
				| TFun(_,t) -> t
				| _ -> raise_typing_error ("Std.isOfType is not a function and cannot be called") null_pos
			in
			(std_cls,isOfType_field,return_type)
		in
		let ctx = {
			scom = SafeCom.of_typer tctx;
			basic = tctx.t;
			config = config;
			wildcard_catch_type = wildcard_catch_type;
			base_throw_type = base_throw_type;
			throws_anything = is_path_of_dynamic config.ec_base_throw && config.ec_avoid_wrapping;
			catches_anything = is_path_of_dynamic config.ec_wildcard_catch && config.ec_avoid_wrapping;
			haxe_exception = haxe_exception;
			haxe_native_stack_trace = haxe_native_stack_trace;
			value_exception = value_exception;
			is_of_type = is_of_type;
		} in
		Some ctx
	| Cross | CustomTarget _ ->
		None
