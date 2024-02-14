
open CoroTypes
open Type

let create_dotgraph path cb =
	print_endline (String.concat "." path);
	let ch,close = DotGraph.start_graph path "coro" in
	let i = ref 0 in
	let pctx = print_context() in
	let st = s_type pctx in
	let se = s_expr_pretty true "" false st in
	let edges = DynArray.create () in
	let rec block cb =
		let cb_id = !i in
		let edge_block label cb_target =
			let target_id = block cb_target in
			DynArray.add edges (cb_id,target_id,label);
		in
		incr i;
		let s = String.concat "\n" (DynArray.to_list (DynArray.map se cb.cb_el)) in
		let snext = match cb.cb_next.next_kind with
			| NextUnknown ->
				None
			| NextSub(cb_sub,cb_next) ->
				edge_block "sub" cb_sub;
				edge_block "next" cb_next;
				None
			| NextBreak ->
				Some "break"
			| NextContinue ->
				Some "continue"
			| NextReturnVoid ->
				Some "return"
			| NextReturn e ->
				Some ("return " ^ se e)
			| NextThrow e ->
				Some ("throw " ^ se e)
			| NextIfThen(e,cb_then,cb_next) ->
				edge_block "then" cb_then;
				edge_block "next" cb_next;
				Some ("if " ^ se e)
			| NextIfThenElse(e,cb_then,cb_else,cb_next) ->
				edge_block "then" cb_then;
				edge_block "else" cb_else;
				edge_block "next" cb_next;
				Some ("if " ^ se e)
			| NextSwitch(switch,cb_next) ->
				List.iter (fun (el,cb_case) ->
					edge_block (String.concat " | " (List.map se el)) cb_case
				) switch.cs_cases;
				edge_block "next" cb_next;
				Option.may (fun cb_default -> edge_block "default" cb_default) switch.cs_default;
				Some ("switch " ^ se switch.cs_subject)
			| NextWhile(e,cb_body,cb_next) ->
				edge_block "body" cb_body;
				edge_block "next" cb_next;
				Some ("while " ^ se e)
			| NextTry(cb_try,catches,cb_next) ->
				edge_block "try" cb_try;
				List.iter (fun (v,cb_catch) ->
					edge_block (st v.v_type) cb_catch
				) catches;
				edge_block "next" cb_next;
				None
			| NextSuspend(suspend,cb_next) ->
				edge_block "next" cb_next;
				Some (Printf.sprintf "%s(%s)" (se suspend.cs_fun) (String.concat ", " (List.map se suspend.cs_args)))
		in
		let s = match snext with
			| None ->
				s
			| Some snext ->
				if s = "" then snext else s ^ "\n" ^ snext
		in
		Printf.fprintf ch "n%i [shape=box,label=\"%s\"];\n" cb_id (StringHelper.s_escape s);
		cb_id
	in
	ignore(block cb);
	DynArray.iter (fun (id_from,id_to,label) ->
		Printf.fprintf ch "n%i -> n%i[label=\"%s\"];\n" id_from id_to label;
	) edges;
	close();