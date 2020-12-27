open Ast
open Common
open DisplayTypes
open DisplayMode
open DisplayPosition
open CompletionItem
open CompletionResultKind
open Type
open Typecore
open Globals
open Genjson
open DisplayPosition
open ImportStatus

let merge_core_doc ctx mtype =
	display_position#run_outside (fun () -> Typecore.merge_core_doc ctx mtype)

let parse_module' com m p =
	display_position#run_outside (fun () -> TypeloadParse.parse_module' com m p)

let parse_module ctx m p =
	display_position#run_outside (fun () -> TypeloadParse.parse_module ctx m p)

module ReferencePosition = struct
	let reference_position = ref ("",null_pos,SKOther)
	let set (s,p,k) =
		let p =
			if p = null_pos then p
			else {p with pfile = Path.get_full_path p.pfile}
		in
		reference_position := (s,p,k)
	let get () = !reference_position
	let reset () = reference_position := ("",null_pos,SKOther)
end

module ExprPreprocessing = struct
	let find_before_pos dm e =
		let display_pos = ref (DisplayPosition.display_position#get) in
		let was_annotated = ref false in
		let is_annotated,is_completion = match dm with
			| DMDefault -> (fun p -> not !was_annotated && encloses_position !display_pos p),true
			| DMHover -> (fun p -> not !was_annotated && encloses_position_gt !display_pos p),false
			| _ -> (fun p -> not !was_annotated && encloses_position !display_pos p),false
		in
		let annotate e dk =
			was_annotated := true;
			(EDisplay(e,dk),pos e)
		in
		let annotate_marked e = annotate e DKMarked in
		let mk_null p = annotate_marked ((EConst(Ident "null")),p) in
		let loop_el el =
			let pr = DisplayPosition.display_position#with_pos (pos e) in
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
					| v :: vl ->
						if mark then
							loop2 (v :: acc) mark vl
						else if is_annotated (snd v.ev_name) then
							(* If the name is the display position, mark the expression *)
							loop2 (v :: acc) true vl
						else begin match v.ev_expr with
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
								let p0 = match v.ev_type with
									| Some (_,pt) -> pt
									| None -> snd v.ev_name
								in
								let p = {p0 with pmax = (pos e).pmin} in
								let e = if is_annotated p then annotate_marked e else e in
								loop2 ({ v with ev_expr = Some e } :: acc) mark vl
						end
					| [] ->
						List.rev acc,mark
				in
				let vl,mark = loop2 [] false vl in
				let e = EVars (List.rev vl),pos e in
				if !was_annotated then e else raise Exit
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
				if is_annotated pp || pp.pmax >= (DisplayPosition.display_position#get).pmax then
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
			| ESwitch(e1,cases,def) when is_annotated (pos e) ->
				(* We must be "between" two cases, or at the end of the last case.
				   Let's find the last case which has a position that is < the display
				   position and mark it. *)
				let did_mark = ref false in
				let mark_case ec p =
					did_mark := true;
					let ep = mk_null p in
					match ec with
					| Some ec ->
						let ec = match fst ec with
							| EBlock el -> (EBlock (el @ [ep]),p)
							| _ -> (EBlock [ec;ep],p)
						in
						Some ec
					| None ->
						Some (mk_null p)
				in
				let rec loop cases = match cases with
					| [el,eg,ec,p1] ->
						let ec = match def with
						| None when (pos e).pmax > !display_pos.pmin -> (* this is so we don't trigger if we're on the } *)
							mark_case ec p1 (* no default, must be the last case *)
						| Some (_,p2) when p1.pmax <= !display_pos.pmin && p2.pmin >= !display_pos.pmax ->
							mark_case ec p1 (* default is beyond display position, mark *)
						| _ ->
							ec (* default contains display position, don't mark *)
						in
						[el,eg,ec,p1]
					| (el1,eg1,ec1,p1) :: (el2,eg2,ec2,p2) :: cases ->
						if p1.pmax <= !display_pos.pmin && p2.pmin >= !display_pos.pmax then
							(el1,eg1,mark_case ec1 p1,p1) :: (el2,eg2,ec2,p2) :: cases
						else
							(el1,eg1,ec1,p1) :: loop ((el2,eg2,ec2,p2) :: cases)
					| [] ->
						[]
				in
				let cases = loop cases in
				let def = if !did_mark then
					def
				else match def with
					| Some(eo,p) when (pos e).pmax > !display_pos.pmin -> Some (mark_case eo p,p)
					| _ -> def
				in
				ESwitch(e1,cases,def),pos e
			| EDisplay _ ->
				raise Exit
			| EMeta((Meta.Markup,_,_),(EConst(String _),p)) when is_annotated p ->
				annotate_marked e
			| EConst (String (_,q)) when ((q <> SSingleQuotes) || !Parser.was_auto_triggered) && is_annotated (pos e) && is_completion ->
				(* TODO: check if this makes any sense *)
				raise Exit
			| EConst(Regexp _) when is_annotated (pos e) && is_completion ->
				raise Exit
			| EVars vl when is_annotated (pos e) ->
				(* We only want to mark EVars if we're on a var name. *)
				if List.exists (fun v -> is_annotated (snd v.ev_name)) vl then
					annotate_marked e
				else
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
				let e1 = map e1 in
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
		let handle_el e el =
			let call_arg_is_marked () =
				el = [] || List.exists (fun (e,_) -> match e with EDisplay(_,DKMarked) -> true | _ -> false) el
			in
			if not !Parser.was_auto_triggered || call_arg_is_marked () then begin
			found := true;
			Parser.mk_display_expr e DKCall
			end else
				e
		in
		let loop e = match fst e with
			| ECall(_,el) | ENew(_,el) when not !found && display_position#enclosed_in (pos e) ->
				handle_el e el
			| EArray(e1,e2) when not !found && display_position#enclosed_in (pos e2) ->
				handle_el e [e2]
			| EDisplay(_,DKCall) ->
				raise Exit
			| _ -> e
		in
		let rec map e = loop (Ast.map_expr map e) in
		try map e with Exit -> e


	let process_expr com e = match com.display.dms_kind with
		| DMDefinition | DMTypeDefinition | DMUsage _ | DMImplementation | DMHover | DMDefault -> find_before_pos com.display.dms_kind e
		| DMSignature -> find_display_call e
		| _ -> e
end

let get_expected_name with_type = match with_type with
	| WithType.Value (Some src) | WithType.WithType(_,Some src) ->
		(match src with
		| WithType.FunctionArgument si -> Some si.si_name
		| WithType.StructureField si -> Some si .si_name
		| WithType.ImplicitReturn -> None
		)
	| _ -> None

let sort_fields l with_type tk =
	let p = match tk with
		| TKExpr p | TKField p -> Some p
		| _ -> None
	in
	let expected_name = get_expected_name with_type in
	let l = List.map (fun ci ->
		let i = get_sort_index tk ci (Option.default Globals.null_pos p) expected_name in
		ci,i
	) l in
	let sort l =
		List.map fst (List.sort (fun (_,i1) (_,i2) -> compare i1 i2) l)
	in
	(* This isn't technically accurate, but I don't think it matters. *)
	let rec dynamify_type_params t = match follow t with
		| TInst({cl_kind = KTypeParameter _},_) -> mk_mono()
		| _ -> Type.map dynamify_type_params t
	in
	let l = match with_type with
		| WithType.WithType(t,_) when (match follow t with TMono _ -> false | _ -> true) ->
			let rec comp item = match item.ci_type with
				| None -> 9
				| Some (t',_) ->
				(* For enum constructors, we consider the return type of the constructor function
				   so it has the same priority as argument-less constructors. *)
				let t' = match item.ci_kind,follow t' with
					| ITEnumField _,TFun(_,r) -> r
					| _ -> t'
				in
				let t' = dynamify_type_params t' in
				if type_iseq t' t then 0 (* equal types - perfect *)
				else if t' == t_dynamic then 5 (* dynamic isn't good, but better than incompatible *)
				else try Type.unify t' t; 1 (* assignable - great *)
				with Unify_error _ -> match follow t' with
					| TFun(_,tr) ->
						if type_iseq tr t then 2 (* function returns our exact type - alright *)
						else (try Type.unify tr t; 3 (* function returns compatible type - okay *)
						with Unify_error _ -> 7) (* incompatible function - useless *)
					| _ ->
						6 (* incompatible type - probably useless *)
			in
			let l = List.map (fun (item,i1) ->
				let i2 = comp item in
				item,(i2,i1)
			) l in
			sort l
		| _ ->
			sort l
	in
	l

let get_import_status ctx path =
	try
		let mt' = ctx.g.do_load_type_def ctx null_pos (mk_type_path ([],snd path)) in
		if path <> (t_infos mt').mt_path then Shadowed else Imported
	with _ ->
		Unimported
