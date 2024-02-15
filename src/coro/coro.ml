open Globals
open Type
open CoroTypes
open CoroFunctions

let fun_to_coro ctx e tf =
	let p = e.epos in
	let v_result = alloc_var VGenerated "_hx_result" t_dynamic p in
	let v_error = alloc_var VGenerated "_hx_error" t_dynamic p in
	let cb_root = make_block (Some(e.etype,p)) in
	ignore(CoroFromTexpr.expr_to_coro ctx (v_result,v_error) cb_root tf.tf_expr);
	let ret_type = if ExtType.is_void (follow tf.tf_type) then t_dynamic else tf.tf_type in
	let vcontinuation = alloc_var VGenerated "_hx_continuation" (tfun [ret_type; t_dynamic] ctx.com.basic.tvoid) p in
	let tf_expr = CoroToTexpr.block_to_texpr_coroutine ctx cb_root vcontinuation v_result v_error e.epos in
	let tf_args = tf.tf_args @ [(vcontinuation,None)] in
	let tf_type = tfun [t_dynamic; t_dynamic] ctx.com.basic.tvoid in
	if ctx.coro_debug then begin
		print_endline ("BEFORE:\n" ^ (s_expr_debug e));
		CoroDebug.create_dotgraph (DotGraph.get_dump_path ctx.com ([],e.epos.pfile) (Printf.sprintf "pos_%i" e.epos.pmin)) cb_root
	end;
	let e = {e with eexpr = TFunction {tf_args; tf_expr; tf_type}} in
	if ctx.coro_debug then print_endline ("AFTER:\n" ^ (s_expr_debug e));
	e

let create_coro_context com meta = {
	com;
	coro_debug = Meta.has (Meta.Custom ":coroutine.debug") meta;
	vthis = None;
	cb_unreachable = make_block None;
}