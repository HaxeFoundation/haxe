open Globals
open Ast
open Type
open Typecore
open Common
open Display
open DisplayTypes.DisplayMode
open DisplayTypes
open DisplayException

let add_removable_code ctx s p prange =
	ctx.removable_code <- (s,p,prange) :: ctx.removable_code

let error_in_diagnostics_run com p =
	let b = DiagnosticsPrinter.is_diagnostics_file com (com.file_keys#get p.pfile) in
	if b then com.has_error <- true;
	b

let find_unused_variables com e =
	let vars = Hashtbl.create 0 in
	let pmin_map = Hashtbl.create 0 in
	let rec loop e = match e.eexpr with
		| TVar({v_kind = VUser _} as v,eo) when v.v_name <> "_" ->
			Hashtbl.add pmin_map e.epos.pmin v;
			let p = match eo with
				| None -> e.epos
				| Some e1 ->
					loop e1;
					{ e.epos with pmax = e1.epos.pmin }
			in
			Hashtbl.replace vars v.v_id (v,p);
		| TLocal ({v_kind = VUser _} as v) ->
			Hashtbl.remove vars v.v_id;
		| _ ->
			Type.iter loop e
	in
	loop e;
	Hashtbl.iter (fun _ (v,p) ->
		let p = match (Hashtbl.find_all pmin_map p.pmin) with [_] -> p | _ -> null_pos in
		add_removable_code com "Unused variable" v.v_pos p
	) vars

let check_other_things com e =
	let had_effect = ref false in
	let no_effect p =
		add_diagnostics_message com (located "This code has no effect" p) DKCompilerMessage Warning;
	in
	let pointless_compound s p =
		add_diagnostics_message com (located (Printf.sprintf "This %s has no effect, but some of its sub-expressions do" s) p) DKCompilerMessage Warning;
	in
	let rec compound s el p =
		let old = !had_effect in
		had_effect := false;
		List.iter (loop true) el;
		if not !had_effect then no_effect p else pointless_compound s p;
		had_effect := old;
	and loop in_value e = match e.eexpr with
		| TBlock el ->
			let rec loop2 el = match el with
				| [] -> ()
				| [e] -> loop in_value e
				| e :: el -> loop false e; loop2 el
			in
			loop2 el
		| TMeta((Meta.Extern,_,_),_) ->
			(* This is so something like `[inlineFunc()]` is not reported. *)
			had_effect := true;
		| TConst _ | TLocal {v_kind = VUser _} | TTypeExpr _ | TFunction _ | TIdent _ when not in_value ->
			no_effect e.epos;
		| TConst _ | TLocal _ | TTypeExpr _ | TEnumParameter _ | TEnumIndex _ | TVar _ | TIdent _ ->
			()
		| TField (_, fa) when PurityState.is_explicitly_impure fa -> ()
		| TFunction tf ->
			loop false tf.tf_expr
		| TCall({eexpr = TField(e1,fa)},el) when not in_value && PurityState.is_pure_field_access fa -> compound "call" el e.epos
		| TNew _ | TCall _ | TBinop ((Ast.OpAssignOp _ | Ast.OpAssign),_,_) | TUnop ((Ast.Increment | Ast.Decrement),_,_)
		| TReturn _ | TBreak | TContinue | TThrow _ | TCast (_,Some _)
		| TIf _ | TTry _ | TSwitch _ | TWhile _ | TFor _ ->
			had_effect := true;
			Type.iter (loop true) e
		| TParenthesis e1 | TMeta(_,e1) ->
			loop in_value e1
		| TArray _ | TCast (_,None) | TBinop _ | TUnop _
		| TField _ | TArrayDecl _ | TObjectDecl _ when in_value ->
			Type.iter (loop true) e;
		| TArray(e1,e2) -> compound "array access" [e1;e2] e.epos
		| TCast(e1,None) -> compound "cast" [e1] e.epos
		| TBinop(op,e1,e2) -> compound (Printf.sprintf "'%s' operator" (s_binop op)) [e1;e2] e.epos
		| TUnop(op,_,e1) -> compound (Printf.sprintf "'%s' operator" (s_unop op)) [e1] e.epos
		| TField(e1,_) -> compound "field access" [e1] e.epos
		| TArrayDecl el -> compound "array declaration" el e.epos
		| TObjectDecl fl -> compound "object declaration" (List.map snd fl) e.epos
	in
	loop true e

let prepare_field dctx dectx com cf = match cf.cf_expr with
	| None -> ()
	| Some e ->
		find_unused_variables dctx e;
		check_other_things com e;
		DeprecationCheck.run_on_expr {dectx with field_meta = cf.cf_meta} e

let collect_diagnostics dctx com =
	let open CompilationCache in
	let dectx = DeprecationCheck.create_context com in
	List.iter (function
		| TClassDecl c when DiagnosticsPrinter.is_diagnostics_file com (com.file_keys#get c.cl_pos.pfile) ->
			let dectx = {dectx with class_meta = c.cl_meta} in
			List.iter (prepare_field dctx dectx com) c.cl_ordered_fields;
			List.iter (prepare_field dctx dectx com) c.cl_ordered_statics;
			(match c.cl_constructor with None -> () | Some cf -> prepare_field dctx dectx com cf);
		| _ ->
			()
	) com.types;
	let handle_dead_blocks com = match com.cache with
		| Some cc ->
			let display_defines = adapt_defines_to_display_context com.defines in
			let is_true defines e =
				ParserEntry.is_true (ParserEntry.eval defines e)
			in
			Hashtbl.iter (fun file_key cfile ->
				if DisplayPosition.display_position#is_in_file (com.file_keys#get cfile.c_file_path) then begin
					let dead_blocks = cfile.c_pdi.pd_dead_blocks in
					let dead_blocks = List.filter (fun (_,e) -> not (is_true display_defines e)) dead_blocks in
					try
						let dead_blocks2 = Hashtbl.find dctx.dead_blocks file_key in
						(* Intersect *)
						let dead_blocks2 = List.filter (fun (p,_) -> List.mem_assoc p dead_blocks) dead_blocks2 in
						Hashtbl.replace dctx.dead_blocks file_key dead_blocks2
					with Not_found ->
						Hashtbl.add dctx.dead_blocks file_key dead_blocks
				end
			) cc#get_files
		| None ->
			()
	in
	handle_dead_blocks com

let prepare com =
	let dctx = {
		removable_code = [];
		import_positions = PMap.empty;
		dead_blocks = Hashtbl.create 0;
		diagnostics_messages = [];
		unresolved_identifiers = [];
		missing_fields = PMap.empty;
	} in
	if not (List.exists (fun (_,_,_,sev,_) -> sev = MessageSeverity.Error) com.shared.shared_display_information.diagnostics_messages) then
		collect_diagnostics dctx com;
	let process_modules com =
		List.iter (fun m ->
			PMap.iter (fun p b ->
				if not (PMap.mem p dctx.import_positions) then
					dctx.import_positions <- PMap.add p b dctx.import_positions
				else if !b then begin
					let b' = PMap.find p dctx.import_positions in
					b' := true
				end
			) m.m_extra.m_display.m_import_positions;
		) com.modules;
		List.iter (function
			| MissingFields mf ->
				let p = mf.mf_pos in
				begin try
					let _,l = PMap.find p dctx.missing_fields in
					l := mf :: !l
				with Not_found ->
					dctx.missing_fields <- PMap.add p (mf.mf_on,ref [mf]) dctx.missing_fields
				end
		) com.display_information.module_diagnostics
	in
	process_modules com;
	begin match com.get_macros() with
	| None -> ()
	| Some com -> process_modules com
	end;
	(* We do this at the end because some of the prepare functions might add information to the common context. *)
	dctx.diagnostics_messages <- com.shared.shared_display_information.diagnostics_messages;
	dctx.unresolved_identifiers <- com.display_information.unresolved_identifiers;
	dctx

let secure_generated_code ctx e =
	(* This causes problems and sucks in general... need a different solution. But I forgot which problem this solved anyway. *)
	(* mk (TMeta((Meta.Extern,[],e.epos),e)) e.etype e.epos *)
	e

let print com =
	let dctx = prepare com in
	Json.string_of_json (DiagnosticsPrinter.json_of_diagnostics com dctx)

let run com =
	let dctx = prepare com in
	dctx