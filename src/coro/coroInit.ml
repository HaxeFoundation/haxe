open Globals
open Type
open Typecore

let load_module_class ctx path =
	let m = ctx.g.do_load_module ctx path null_pos in
	ExtList.List.find_map_exn (function
	| TClassDecl({ cl_path = path' } as cl) when path = path' ->
		Some cl
	| _ ->
		None
	) m.m_types

let load_module_abstract ctx path =
	let m = ctx.g.do_load_module ctx path null_pos in
	ExtList.List.find_map_exn (function
	| TAbstractDecl({ a_path = path' } as a) when path = path' ->
		Some a
	| _ ->
		None
	) m.m_types

let make_continuation_api ctx =
	let base_continuation_class = load_module_class ctx (["haxe";"coro"], "BaseContinuation") in
	let immediate_suspension_result_class = load_module_class ctx (["haxe";"coro"],"ImmediateSuspensionResult") in
	let suspension_state = TAbstract(load_module_abstract ctx (["haxe";"coro"],"SuspensionState"),[]) in
	let suspension_result_class = Lazy.force ctx.t.tcoro.suspension_result_class in
	let cf_state      = PMap.find "state" suspension_result_class.cl_fields in
	let cf_result     = PMap.find "result" suspension_result_class.cl_fields in
	let cf_error      = PMap.find "error" suspension_result_class.cl_fields in
	let cf_completion = PMap.find "completion" base_continuation_class.cl_fields in
	let cf_context    = PMap.find "context" base_continuation_class.cl_fields in
	let cf_goto_label = PMap.find "gotoLabel" base_continuation_class.cl_fields in
	let cf_recursing  = PMap.find "recursing" base_continuation_class.cl_fields in
	let cf_suspended  = PMap.find "suspended" suspension_result_class.cl_statics in
	let immediate_result,immediate_error =
		let c = immediate_suspension_result_class in
		let cf_result = PMap.find "withResult" c.cl_statics in
		let cf_error = PMap.find "withError" c.cl_statics in
		(fun e ->
			CallUnification.make_static_call_better ctx c cf_result [e.etype] [e] (TInst(c,[e.etype])) e.epos
		), (fun e t ->
			CallUnification.make_static_call_better ctx c cf_error [] [e] (TInst(c,[t])) e.epos
		)
	in
	let api = ContTypes.create_continuation_api base_continuation_class immediate_suspension_result_class suspension_state suspension_result_class (Lazy.force ctx.t.tcoro.continuation) immediate_result immediate_error cf_suspended cf_state cf_result cf_error cf_completion cf_context cf_goto_label cf_recursing in
	ctx.g.continuation_api <- Some api;
	api