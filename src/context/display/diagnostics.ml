open Globals
open Ast
open Type
open Typecore
open Common
open Display
open DisplayTypes.DisplayMode

module DiagnosticsKind = struct
	type t =
		| DKUnusedImport
		| DKUnresolvedIdentifier
		| DKCompilerError
		| DKRemovableCode

	let to_int = function
		| DKUnusedImport -> 0
		| DKUnresolvedIdentifier -> 1
		| DKCompilerError -> 2
		| DKRemovableCode -> 3
end

type diagnostics_context = {
	com : Common.context;
	mutable removable_code : (string * pos * pos) list;
}

open DiagnosticsKind
open DisplayTypes

let add_removable_code ctx s p prange =
	ctx.removable_code <- (s,p,prange) :: ctx.removable_code

let find_unused_variables com e =
	let vars = Hashtbl.create 0 in
	let pmin_map = Hashtbl.create 0 in
	let rec loop e = match e.eexpr with
		| TVar(v,eo) when Meta.has Meta.UserVariable v.v_meta ->
			Hashtbl.add pmin_map e.epos.pmin v;
			let p = match eo with
				| None -> e.epos
				| Some e1 ->
					loop e1;
					{ e.epos with pmax = e1.epos.pmin }
			in
			Hashtbl.replace vars v.v_id (v,p);
		| TLocal v when Meta.has Meta.UserVariable v.v_meta ->
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
		add_diagnostics_message com "This code has no effect" p DiagnosticsSeverity.Warning;
	in
	let pointless_compound s p =
		add_diagnostics_message com (Printf.sprintf "This %s has no effect, but some of its sub-expressions do" s) p DiagnosticsSeverity.Warning;
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
		| TLocal v when not (Meta.has Meta.UserVariable v.v_meta) ->
			()
		| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ | TIdent _ when not in_value ->
			no_effect e.epos;
		| TConst _ | TLocal _ | TTypeExpr _ | TEnumParameter _ | TEnumIndex _ | TVar _ | TIdent _ ->
			()
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

let prepare_field dctx cf = match cf.cf_expr with
	| None -> ()
	| Some e ->
		find_unused_variables dctx e;
		check_other_things dctx.com e;
		DeprecationCheck.run_on_expr dctx.com e

let prepare com global =
	let dctx = {
		removable_code = [];
		com = com;
	} in
	List.iter (function
		| TClassDecl c when global || is_display_file c.cl_pos.pfile ->
			List.iter (prepare_field dctx) c.cl_ordered_fields;
			List.iter (prepare_field dctx) c.cl_ordered_statics;
			(match c.cl_constructor with None -> () | Some cf -> prepare_field dctx cf);
		| _ ->
			()
	) com.types;
	dctx

let is_diagnostics_run p = match (!Parser.display_mode) with
	| DMDiagnostics true -> true
	| DMDiagnostics false -> is_display_file p.pfile
	| _ -> false

let secure_generated_code ctx e =
	if is_diagnostics_run e.epos then mk (TMeta((Meta.Extern,[],e.epos),e)) e.etype e.epos else e

module Printer = struct
	open Json
	open DiagnosticsKind
	open DisplayTypes

	type t = DiagnosticsKind.t * pos

	module UnresolvedIdentifierSuggestion = struct
		type t =
			| UISImport
			| UISTypo

		let to_int = function
			| UISImport -> 0
			| UISTypo -> 1
	end

	open UnresolvedIdentifierSuggestion
	open CompletionItem
	open CompletionModuleType

	let print_diagnostics dctx ctx global =
		let com = dctx.com in
		let diag = Hashtbl.create 0 in
		let add dk p sev args =
			let file = Path.get_real_path p.pfile in
			let diag = try
				Hashtbl.find diag file
			with Not_found ->
				let d = Hashtbl.create 0 in
				Hashtbl.add diag file d;
				d
			in
			if not (Hashtbl.mem diag p) then
				Hashtbl.add diag p (dk,p,sev,args)
		in
		let add dk p sev args =
			if global || is_display_file p.pfile then add dk p sev args
		in
		List.iter (fun (s,p,suggestions) ->
			let suggestions = ExtList.List.filter_map (fun (s,item,r) ->
				match item.ci_kind with
				| ITType(t,_) when r = 0 ->
					let path = if t.module_name = t.name then (t.pack,t.name) else (t.pack @ [t.module_name],t.name) in
					Some (JObject [
						"kind",JInt (to_int UISImport);
						"name",JString (s_type_path path);
					])
				| _ when r = 0 ->
					(* TODO !!! *)
					None
				| _ ->
					Some (JObject [
						"kind",JInt (to_int UISTypo);
						"name",JString s;
					])
			) suggestions in
			add DKUnresolvedIdentifier p DiagnosticsSeverity.Error (JArray suggestions);
		) com.display_information.unresolved_identifiers;
		PMap.iter (fun p (r,_) ->
			if not !r then add DKUnusedImport p DiagnosticsSeverity.Warning (JArray [])
		) com.shared.shared_display_information.import_positions;
		List.iter (fun (s,p,sev) ->
			add DKCompilerError p sev (JString s)
		) com.shared.shared_display_information.diagnostics_messages;
		List.iter (fun (s,p,prange) ->
			add DKRemovableCode p DiagnosticsSeverity.Warning (JObject ["description",JString s;"range",if prange = null_pos then JNull else Genjson.generate_pos_as_range prange])
		) dctx.removable_code;
		let jl = Hashtbl.fold (fun file diag acc ->
			let jl = Hashtbl.fold (fun _ (dk,p,sev,jargs) acc ->
				(JObject [
					"kind",JInt (DiagnosticsKind.to_int dk);
					"severity",JInt (DiagnosticsSeverity.to_int sev);
					"range",Genjson.generate_pos_as_range p;
					"args",jargs
				]) :: acc
			) diag [] in
			(JObject [
				"file",JString file;
				"diagnostics",JArray jl
			]) :: acc
		) diag [] in
		let js = JArray jl in
		string_of_json js
end