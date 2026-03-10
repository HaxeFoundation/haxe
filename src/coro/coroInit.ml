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
	let suspension_state = TAbstract(load_module_abstract ctx (["haxe";"coro"],"SuspensionState"),[]) in
	let suspension_result_class = AtomicLazy.force ctx.t.tcoro.suspension_result_class in
	let cf_state      = PMap.find "state" suspension_result_class.cl_fields in
	let cf_result     = PMap.find "result" suspension_result_class.cl_fields in
	let cf_error      = PMap.find "error" suspension_result_class.cl_fields in
	let cf_completion = PMap.find "completion" base_continuation_class.cl_fields in
	let cf_context    = PMap.find "context" base_continuation_class.cl_fields in
	let cf_goto_label = PMap.find "gotoLabel" base_continuation_class.cl_fields in
	let cf_suspended  = PMap.find "suspended" suspension_result_class.cl_statics in
	let api = ContTypes.create_continuation_api base_continuation_class suspension_state suspension_result_class (AtomicLazy.force ctx.t.tcoro.continuation) cf_suspended cf_state cf_result cf_error cf_completion cf_context cf_goto_label in
	ctx.g.continuation_api <- Some api;
	api