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
open DisplayPosition

let reference_position = ref ("",null_pos,KVar)

module ExprPreprocessing = struct
	let find_before_pos com dm e =

		let display_pos = ref (!DisplayPosition.display_position) in
		let is_annotated,is_completion = match dm with
			| DMDefault -> (fun p -> encloses_position !display_pos p),true
			| DMHover -> (fun p -> encloses_position_gt !display_pos p),false
			| _ -> (fun p -> encloses_position !display_pos p),false
		in
		let annotate e dk =
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			(EDisplay(e,dk),pos e)
		in
		let annotate_marked e = annotate e DKMarked in
		let mk_null p = annotate_marked ((EConst(Ident "null")),p) in
		let loop_el el =
			let pr = !DisplayPosition.display_position in
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
		let in_pattern = ref false in
		let loop e =
			(* print_endline (Printf.sprintf "%i-%i: %s" (pos e).pmin (pos e).pmax (Ast.s_expr e)); *)
			match fst e with
			| EVars vl when is_annotated (pos e) && is_completion ->
				let rec loop2 acc mark vl = match vl with
					| ((s,pn),tho,eo) as v :: vl ->
						if mark then
							loop2 (v :: acc) mark vl
						else if is_annotated pn then
							(* If the name is the display position, mark the expression *)
							loop2 (v :: acc) true vl
						else begin match eo with
							| None ->
								(* If there is no expression, we don't have to do anything.
								   Should the display position be on the type-hint, it will
								   be picked up while loading the type. *)
								loop2 (v :: acc) mark vl
							| Some e ->
								(* Determine the area between the `|` in `var x| = | e`. This is not really
								   correct because we don't want completion on the left side of the `=`, but
								   we cannot determine that correctly without knowing its position.
								   Note: We know `e` itself isn't the display position because this entire
								   algorithm is bottom-up and it would be marked already if it was. *)
								let p0 = match tho with
									| Some (_,pt) -> pt
									| None -> pn
								in
								let p = {p0 with pmax = (pos e).pmin} in
								let e = if is_annotated p then annotate_marked e else e in
								loop2 (((s,pn),tho,(Some e)) :: acc) mark vl
						end
					| [] ->
						List.rev acc,mark
				in
				let vl,mark = loop2 [] false vl in
				let e = EVars (List.rev vl),pos e in
				if mark then annotate_marked e else e
			| EBinop((OpAssign | OpAssignOp _) as op,e1,e2) when is_annotated (pos e) && is_completion ->
				(* Special case for assign ops: If the expression is marked, but none of its operands are,
				   we are "probably" interested in the rhs. Like with EVars, this isn't accurate because we
				   could be on the left side of the `=`. I don't think there's a reason for requesting
				   completion there though. *)
				(EBinop(op,e1,annotate_marked e2)),(pos e)
			| EBinop(OpOr,e1,(EIf(_,(EConst(Ident "null"),_),None),p1)) when is_annotated (pos e) && is_completion && !in_pattern ->
				(* This HAS TO come from an attempted `case pattern | guard:` completion (issue #7068). *)
				let p = { p1 with pmin = (pos e1).pmax; pmax = p1.pmin } in
				EBinop(OpOr,e1,mk_null p),(pos e)
			| EIf(_,(EConst(Ident "null"),_),None) when is_completion && !in_pattern ->
				(* This is fine. *)
				mk_null (pos e)
			| EBlock [] when is_annotated (pos e) ->
				annotate e DKStructure
			| EBlock [EDisplay((EConst(Ident s),pn),DKMarked),_] when is_completion ->
				let e = EObjectDecl [(s,pn,NoQuotes),(EConst (Ident "null"),null_pos)],(pos e) in
				annotate e DKStructure
			| EBlock el when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				EBlock el,(pos e)
			| ECall(e1,el) when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				ECall(e1,el),(pos e)
			| ENew((tp,pp),el) when is_annotated (pos e) && is_completion ->
				if is_annotated pp || pp.pmax >= !DisplayPosition.display_position.pmax then
					annotate_marked e
				else begin
					let el = loop_el el in
					ENew((tp,pp),el),(pos e)
				end
			| EArrayDecl el when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				EArrayDecl el,(pos e)
			| EObjectDecl fl when is_annotated (pos e) && is_completion ->
				annotate e DKStructure
			| EDisplay _ ->
				raise Exit
			| EConst (String _) when (not (Lexer.is_fmt_string (pos e)) || !Parser.was_auto_triggered) && is_annotated (pos e) && is_completion ->
				(* TODO: check if this makes any sense *)
				raise Exit
			| EConst(Regexp _) when is_annotated (pos e) && is_completion ->
				raise Exit
			| _ ->
				if is_annotated (pos e) then
					annotate_marked e
				else
					e
		in
		let opt f o =
			match o with None -> None | Some v -> Some (f v)
		in
		let rec map e = match fst e with
			| ESwitch(e1,cases,def) when is_annotated (pos e) ->
				let e1 = loop e1 in
				let cases = List.map (fun (el,eg,e,p) ->
					let old = !in_pattern in
					in_pattern := true;
					let el = List.map map el in
					in_pattern := old;
					let eg = opt map eg in
					let e = opt map e in
					el,eg,e,p
				) cases in
				let def = opt (fun (eo,p) -> opt map eo,p) def in
				loop (ESwitch (e1, cases, def),(pos e))
			| _ ->
				loop (Ast.map_expr map e)
		in
		try map e with Exit -> e

	let find_display_call e =
		let found = ref false in
		let loop e = match fst e with
			| ECall(_,el) | ENew(_,el) when not !found && encloses_display_position (pos e) ->
				let call_arg_is_marked () =
					el = [] || List.exists (fun (e,_) -> match e with EDisplay(_,DKMarked) -> true | _ -> false) el
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
		| DMDefinition | DMTypeDefinition | DMUsage _ | DMHover | DMDefault -> find_before_pos com com.display.dms_kind e
		| DMSignature -> find_display_call e
		| _ -> e
end