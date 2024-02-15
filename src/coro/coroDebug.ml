
open CoroTypes
open Type

let create_dotgraph path cb =
	print_endline (String.concat "." path);
	let ch,close = DotGraph.start_graph path "coro" in
	let pctx = print_context() in
	let st = s_type pctx in
	let se = s_expr_pretty true "" false st in
	let edges = DynArray.create () in
	let rec block cb =
		let edge_block label cb_target =
			block cb_target;
			DynArray.add edges (cb.cb_id,cb_target.cb_id,label);
		in
		let s = String.concat "\n" (DynArray.to_list (DynArray.map se cb.cb_el)) in
		let snext = match cb.cb_next.next_kind with
			| NextUnknown ->
				None
			| NextSub(cb_sub,cb_next) ->
				edge_block "next" cb_next;
				edge_block "sub" cb_sub;
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
				edge_block "next" cb_next;
				edge_block "then" cb_then;
				Some ("if " ^ se e)
			| NextIfThenElse(e,cb_then,cb_else,cb_next) ->
				edge_block "next" cb_next;
				edge_block "then" cb_then;
				edge_block "else" cb_else;
				Some ("if " ^ se e)
			| NextSwitch(switch,cb_next) ->
				edge_block "next" cb_next;
				List.iter (fun (el,cb_case) ->
					edge_block (String.concat " | " (List.map se el)) cb_case
				) switch.cs_cases;
				Option.may (fun cb_default -> edge_block "default" cb_default) switch.cs_default;
				Some ("switch " ^ se switch.cs_subject)
			| NextWhile(e,cb_body,cb_next) ->
				edge_block "next" cb_next;
				edge_block "body" cb_body;
				Some ("while " ^ se e)
			| NextTry(cb_try,catches,cb_next) ->
				edge_block "next" cb_next;
				edge_block "try" cb_try;
				List.iter (fun (v,cb_catch) ->
					edge_block (st v.v_type) cb_catch
				) catches;
				None
			| NextSuspend(suspend,cb_next) ->
				edge_block "next" cb_next;
				Some (Printf.sprintf "%s(%s)" (se suspend.cs_fun) (String.concat ", " (List.map se suspend.cs_args)))
			| NextFallThrough cb_next ->
				DynArray.add edges (cb.cb_id,cb_next.cb_id,"fall-through");
				None
		in
		let s = match snext with
			| None ->
				s
			| Some snext ->
				if s = "" then snext else s ^ "\n" ^ snext
		in
		Printf.fprintf ch "n%i [shape=box,label=\"%s\"];\n" cb.cb_id (StringHelper.s_escape s);
	in
	ignore(block cb);
	DynArray.iter (fun (id_from,id_to,label) ->
		Printf.fprintf ch "n%i -> n%i[label=\"%s\"];\n" id_from id_to label;
	) edges;
	close();