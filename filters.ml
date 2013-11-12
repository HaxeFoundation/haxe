open Ast
open Common
open Type

let run com tctx main =
	let filters = [
		Codegen.Abstract.handle_abstract_casts tctx;
		(match com.platform with Cpp -> Codegen.handle_side_effects com (Typecore.gen_local tctx) | _ -> fun e -> e);
		Codegen.promote_complex_rhs com;
		if com.foptimize then (fun e -> Optimizer.reduce_expression tctx (Optimizer.inline_constructors tctx e)) else Optimizer.sanitize tctx;
		Codegen.check_local_vars_init;
		Codegen.captured_vars com;
		Codegen.rename_local_vars com;
	] in
	List.iter (Codegen.post_process tctx filters) com.types;
	Codegen.post_process_end();
	List.iter (fun f -> f()) (List.rev com.filters);
	List.iter (Codegen.save_class_state tctx) com.types;
	List.iter (fun t ->
		Codegen.remove_generic_base tctx t;
		Codegen.remove_extern_fields tctx t
	) com.types;
	if com.display = DMUsage then
		Codegen.detect_usage com;
	Codegen.update_cache_dependencies com;
	let dce_mode = (try Common.defined_value com Define.Dce with _ -> "no") in
	if not (Common.defined com Define.As3 || dce_mode = "no" || Common.defined com Define.DocGen) then Dce.run com main (dce_mode = "full" && not (Common.defined com Define.Interp));
	(* always filter empty abstract implementation classes (issue #1885) *)
	List.iter (fun mt -> match mt with
		| TClassDecl({cl_kind = KAbstractImpl _} as c) when c.cl_ordered_statics = [] && c.cl_ordered_fields = [] -> c.cl_extern <- true
		| _ -> ()
	) com.types;
	let type_filters = [
		Codegen.check_private_path;
		Codegen.apply_native_paths;
		Codegen.add_rtti;
		(match com.platform with | Java | Cs -> (fun _ _ -> ()) | _ -> Codegen.add_field_inits);
		Codegen.add_meta_field;
		Codegen.check_remove_metadata;
		Codegen.check_void_field;
	] in
	List.iter (fun t -> List.iter (fun f -> f tctx t) type_filters) com.types;