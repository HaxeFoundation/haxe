open Ast
open Common
open DisplayTypes
open DisplayMode
open CompletionItem
open CompletionResultKind
open Type
open Typecore
open Globals
open Genjson

let reference_position = ref null_pos

let is_display_file file =
	file <> "?" && Path.unique_full_path file = (!Parser.resume_display).pfile

let encloses_position p_target p =
	p.pmin < p_target.pmin && p.pmax >= p_target.pmax

let is_display_position p =
	encloses_position !Parser.resume_display p

module ExprPreprocessing = struct
	let find_before_pos com is_completion e =
		let display_pos = ref (!Parser.resume_display) in
		let is_annotated p = encloses_position !display_pos p in
		let annotate e dk =
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			(EDisplay(e,dk),pos e)
		in
		let annotate_marked e = annotate e DKMarked in
		let mk_null p = annotate_marked ((EConst(Ident "null")),p) in
		let loop_el el =
			let pr = !Parser.resume_display in
			let rec loop el = match el with
				| [] -> [mk_null pr]
				| e :: el ->
					if (pos e).pmin >= pr.pmax then (mk_null pr) :: e :: el
					else e :: loop el
			in
			(* print_endline (Printf.sprintf "%i-%i: PR" pr.pmin pr.pmax);
			List.iter (fun e ->
				print_endline (Printf.sprintf "%i-%i: %s" (pos e).pmin (pos e).pmax (Ast.s_expr e));
			) el; *)
			match el with
			| [] -> [mk_null pr]
			| e :: el ->
				if (pos e).pmin >= pr.pmax then (mk_null pr) :: e :: el
				else loop (e :: el)
		in
		let loop e =
			(* print_endline (Printf.sprintf "%i-%i: %s" (pos e).pmin (pos e).pmax (Ast.s_expr e)); *)
			match fst e with
			| EVars vl ->
				if List.exists (fun ((_,p),_,_) -> is_annotated p) vl then
					annotate_marked e
				else
					e
			| EBlock [] when is_annotated (pos e) ->
				annotate e DKStructure
			| EBlock el when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				EBlock el,(pos e)
			| ECall(e1,el) when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				ECall(e1,el),(pos e)
			| ENew((tp,pp),el) when is_annotated (pos e) && is_completion ->
				if is_annotated pp || pp.pmax >= !Parser.resume_display.pmax then
					annotate_marked e
				else begin
					let el = loop_el el in
					ENew((tp,pp),el),(pos e)
				end
			| EArrayDecl el when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				EArrayDecl el,(pos e)
			| EDisplay _ ->
				raise Exit
			| EConst (String _) when (not (Lexer.is_fmt_string (pos e)) || !Parser.was_auto_triggered) && is_annotated (pos e) && is_completion ->
				(* TODO: check if this makes any sense *)
				raise Exit
			| _ ->
				if is_annotated (pos e) then
					annotate_marked e
				else
					e
		in
		let rec map e =
			loop (Ast.map_expr map e)
		in
		try map e with Exit -> e

	let find_display_call e =
		let found = ref false in
		let loop e = match fst e with
			| ECall(_,el) | ENew(_,el) when not !found && is_display_position (pos e) ->
				let call_arg_is_marked () =
					List.exists (fun (e,_) -> match e with EDisplay(_,DKMarked) -> true | _ -> false) el
				in
				if not !Parser.was_auto_triggered || call_arg_is_marked () then begin
					found := true;
					Parser.mk_display_expr e DKCall
				end else
					e
			| _ -> e
		in
		let rec map e = loop (Ast.map_expr map e) in
		map e


	let process_expr com e = match com.display.dms_kind with
		| DMDefinition | DMUsage _ | DMHover -> find_before_pos com false e
		| DMDefault -> find_before_pos com true e
		| DMSignature -> find_display_call e
		| _ -> e
end