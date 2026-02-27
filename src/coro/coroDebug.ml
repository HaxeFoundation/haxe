
open CoroTypes
open CoroFunctions
open Type

let create_dotgraph path cb =
	print_endline (String.concat "." path);
	let ch,close = DotGraph.start_graph path ".coro" in
	let pctx = print_context() in
	let st = s_type pctx in
	let se = s_expr_pretty true "" false st in
	let edges = DynArray.create () in
	let rec block cb =
		let edge_block label cb_target =
			block cb_target;
			DynArray.add edges (cb.cb_id,cb_target.cb_id,label,true);
		in
		let maybe_edge_block label = Option.may (edge_block label) in
		let s = String.concat "\n" (DynArray.to_list (DynArray.map se cb.cb_el)) in
		let flags = if has_block_flag cb CbResumeState then " resume" else if has_block_flag cb CbSuspendState then " suspend" else "" in
		let s = if s = "" then Printf.sprintf "(%i%s)" cb.cb_id flags else Printf.sprintf "(%i%s)\n%s" cb.cb_id flags s in
		let snext = match cb.cb_next with
			| NextUnknown ->
				None
			| NextBreak cb_break ->
				DynArray.add edges (cb.cb_id,cb_break.cb_id,"goto",false);
				Some "break"
			| NextContinue cb_continue ->
				DynArray.add edges (cb.cb_id,cb_continue.cb_id,"goto",false);
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
				maybe_edge_block "next" cb_next;
				edge_block "then" cb_then;
				edge_block "else" cb_else;
				Some ("if " ^ se e)
			| NextSwitch(switch,cb_next) ->
				maybe_edge_block "next" cb_next;
				List.iter (fun (el,cb_case) ->
					edge_block (String.concat " | " (List.map se el)) cb_case
				) switch.cs_cases;
				Option.may (fun cb_default -> edge_block "default" cb_default) switch.cs_default;
				Some ("switch " ^ se switch.cs_subject)
			| NextWhile(e,cb_body,cb_next) ->
				maybe_edge_block "next" cb_next;
				edge_block "body" cb_body;
				Some ("while " ^ se e)
			| NextTry(cb_try,catch,cb_next) ->
				maybe_edge_block "next" cb_next;
				edge_block "try" cb_try;
				DynArray.add edges (cb_try.cb_id,catch.cc_cb.cb_id,"catch",true);
				Printf.fprintf ch "n%i [shape=box,label=\"(%i)\"];\n" catch.cc_cb.cb_id catch.cc_cb.cb_id;
				List.iter (fun (v,cb_catch) ->
					block cb_catch;
					DynArray.add edges (catch.cc_cb.cb_id,cb_catch.cb_id,(st v.v_type),true);
				) catch.cc_catches;
				None
			| NextSuspend(suspend,cb_next) ->
				maybe_edge_block "next" cb_next;
				Some (Printf.sprintf "%s(%s)" (se suspend.cs_fun) (String.concat ", " (List.map se suspend.cs_args)))
			| NextFallThrough cb_next ->
				DynArray.add edges (cb.cb_id,cb_next.cb_id,"fall-through",false);
				None
			| NextGoto cb_next ->
				DynArray.add edges (cb.cb_id,cb_next.cb_id,"goto",false);
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
	DynArray.iter (fun (id_from,id_to,label,tree_edge) ->
		let style = if tree_edge then "style=\"solid\",color=\"black\""  else "style=\"dashed\", color=\"lightgray\"" in
		Printf.fprintf ch "n%i -> n%i[%s label=\"%s\"];\n" id_from id_to style (StringHelper.s_escape label);
	) edges;
	close();