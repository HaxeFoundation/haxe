open Ast
open Common
open Type
open Typecore

(* order of these variants affects output sorting *)
type display_field_kind =
	| FKVar
	| FKMethod
	| FKType
	| FKPackage

exception Diagnostics of string
exception ModuleSymbols of string
exception DisplaySignatures of (t * documentation) list
exception DisplayType of t * pos
exception DisplayPosition of Ast.pos list
exception DisplaySubExpression of Ast.expr
exception DisplayFields of (string * t * display_field_kind option * documentation) list
exception DisplayToplevel of IdentifierType.t list
exception DisplayPackage of string list

let is_display_file file =
	file <> "?" && Common.unique_full_path file = (!Parser.resume_display).pfile

let encloses_position p_target p =
	p.pmin <= p_target.pmin && p.pmax >= p_target.pmax

let is_display_position p =
	encloses_position !Parser.resume_display p

let find_enclosing com e =
	let display_pos = ref (!Parser.resume_display) in
	let mk_null p = (EDisplay(((EConst(Ident "null")),p),false),p) in
	let encloses_display_pos p =
		if encloses_position !display_pos p then begin
			let p = !display_pos in
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			Some p
		end else
			None
	in
	let rec loop e = match fst e with
		| EBlock el ->
			let p = pos e in
			(* We want to find the innermost block which contains the display position. *)
			let el = List.map loop el in
			let el = match encloses_display_pos p with
				| None ->
					el
				| Some p2 ->
					let b,el = List.fold_left (fun (b,el) e ->
						let p = pos e in
						if b || p.pmax <= p2.pmin then begin
							(b,e :: el)
						end else begin
							let e_d = (EDisplay(mk_null p,false)),p in
							(true,e :: e_d :: el)
						end
					) (false,[]) el in
					let el = if b then
						el
					else begin
						mk_null p :: el
					end in
					List.rev el
			in
			(EBlock el),(pos e)
		| _ ->
			Ast.map_expr loop e
	in
	loop e

let find_before_pos com e =
	let display_pos = ref (!Parser.resume_display) in
	let is_annotated p =
		if p.pmin <= !display_pos.pmin && p.pmax >= !display_pos.pmax then begin
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			true
		end else
			false
	in
	let rec loop e =
		if is_annotated (pos e) then
			(EDisplay(e,false),(pos e))
		else
			e
	in
	let rec map e =
		loop (Ast.map_expr map e)
	in
	map e

let display_type dm t p =
	try
		let mt = module_type_of_type t in
		begin match dm with
			| DMPosition -> raise (DisplayPosition [(t_infos mt).mt_pos]);
			| DMUsage ->
				let ti = t_infos mt in
				ti.mt_meta <- (Meta.Usage,[],ti.mt_pos) :: ti.mt_meta
			| DMType -> raise (DisplayType (t,p))
			| _ -> ()
		end
	with Exit ->
		()

let display_module_type dm mt =
	display_type dm (type_of_module_type mt)

let display_variable dm v p = match dm with
	| DMPosition -> raise (DisplayPosition [v.v_pos])
	| DMUsage -> v.v_meta <- (Meta.Usage,[],v.v_pos) :: v.v_meta;
	| DMType -> raise (DisplayType (v.v_type,p))
	| _ -> ()

let display_field dm cf p = match dm with
	| DMPosition -> raise (DisplayPosition [cf.cf_pos]);
	| DMUsage -> cf.cf_meta <- (Meta.Usage,[],cf.cf_pos) :: cf.cf_meta;
	| DMType -> raise (DisplayType (cf.cf_type,p))
	| _ -> ()

let display_enum_field dm ef p = match dm with
	| DMPosition -> raise (DisplayPosition [p]);
	| DMUsage -> ef.ef_meta <- (Meta.Usage,[],p) :: ef.ef_meta;
	| DMType -> raise (DisplayType (ef.ef_type,p))
	| _ -> ()

module SymbolKind = struct
	type t =
		| Class
		| Interface
		| Enum
		| Typedef
		| Abstract
		| Field
		| Property
		| Method
		| Constructor
		| Function
		| Variable

	let to_int = function
		| Class -> 1
		| Interface -> 2
		| Enum -> 3
		| Typedef -> 4
		| Abstract -> 5
		| Field -> 6
		| Property -> 7
		| Method -> 8
		| Constructor -> 9
		| Function -> 10
		| Variable -> 11
end

module SymbolInformation = struct
	type t = {
		name : string;
		kind : SymbolKind.t;
		pos : pos;
		container_name : string option;
	}

	let make name kind pos container_name = {
		name = name;
		kind = kind;
		pos = pos;
		container_name = container_name;
	}
end

open SymbolKind
open SymbolInformation
open Json

let pos_to_json_range p =
	if p.pmin = -1 then
		JNull
	else
		let l1, p1, l2, p2 = Lexer.get_pos_coords p in
		let to_json l c = JObject [("line", JInt (l - 1)); ("character", JInt c)] in
		JObject [
			("start", to_json l1 p1);
			("end", to_json l2 p2);
		]

let print_module_symbols (pack,decls) =
	let l = DynArray.create() in
	let add name kind location parent =
		let si = SymbolInformation.make name kind location (match parent with None -> None | Some si -> Some si.name) in
		DynArray.add l si;
		si
	in
(* 		let si_pack = match pack with
		| [] -> None
		| _ -> Some (add (String.concat "." pack) Package null_pos None) (* TODO: we don't have the position *)
	in *)
	let si_pack = None in (* TODO: no position, no point *)
	let rec expr si (e,p) =
		let add name kind location = add name kind location si in
		let add_ignore name kind location = ignore(add name kind location) in
		begin match e with
			(* TODO: disabled for now because it's too spammy *)
(* 			| EConst ct ->
			begin match ct with
				| Int i | Float i -> add_ignore i Number p
				| Ast.String s -> add_ignore s String p
				| Ident ("true" | "false" as s) -> add_ignore s Boolean p
				| Ident _ -> (* Hmm... *) ()
				| _ -> ()
			end *)
		| EVars vl ->
			List.iter (fun ((s,p),_,eo) ->
				add_ignore s Variable p;
				expr_opt si eo
			) vl
		| ETry(e1,catches) ->
			expr si e1;
			List.iter (fun ((s,p),_,e) ->
				add_ignore s Variable p;
				expr si e
			) catches;
		| EFunction(Some s,f) ->
			let si_function = add s Function p in
			func si_function f
		| EIn((EConst(Ident s),p),e2) ->
			add_ignore s Variable p;
			expr si e2;
		| _ ->
			iter_expr (expr si) (e,p)
		end
	and expr_opt si eo = match eo with
		| None -> ()
		| Some e -> expr si e
	and func si f =
		List.iter (fun ((s,p),_,_,_,eo) ->
			let si_arg = add s Variable p (Some si) in
			expr_opt (Some si_arg) eo
		) f.f_args;
		expr_opt (Some si) f.f_expr
	in
	let field si_type cff = match cff.cff_kind with
		| FVar(_,eo) ->
			let si_field = add (fst cff.cff_name) Field cff.cff_pos (Some si_type) in
			expr_opt (Some si_field) eo
		| FFun f ->
			let si_method = add (fst cff.cff_name) (if fst cff.cff_name = "new" then Constructor else Method) cff.cff_pos (Some si_type) in
			func si_method f
		| FProp(_,_,_,eo) ->
			let si_property = add (fst cff.cff_name) Property cff.cff_pos (Some si_type) in
			expr_opt (Some si_property) eo
	in
	List.iter (fun (td,p) -> match td with
		| EImport _ | EUsing _ ->
			() (* TODO: Can we do anything with these? *)
		| EClass d ->
			let si_type = add (fst d.d_name) (if List.mem HInterface d.d_flags then Interface else Class) p si_pack in
			List.iter (field si_type) d.d_data
		| EEnum d ->
			let si_type = add (fst d.d_name) Enum p si_pack in
			List.iter (fun ef ->
				ignore (add (fst ef.ec_name) Method ef.ec_pos (Some si_type))
			) d.d_data
		| ETypedef d ->
			let si_type = add (fst d.d_name) Typedef p si_pack in
			(match d.d_data with
			| CTAnonymous fields,_ ->
				List.iter (field si_type) fields
			| _ -> ())
		| EAbstract d ->
			let si_type = add (fst d.d_name) Abstract p si_pack in
			List.iter (field si_type) d.d_data
	) decls;
	let jl = List.map (fun si ->
		let l =
			("name",JString si.name) ::
			("kind",JInt (to_int si.kind)) ::
			("range", pos_to_json_range si.pos) ::
			(match si.container_name with None -> [] | Some s -> ["containerName",JString s])
		in
		JObject l
	) (DynArray.to_list l) in
	let js = JArray jl in
	let b = Buffer.create 0 in
	write_json (Buffer.add_string b) js;
	Buffer.contents b

type import_display_kind =
	| IDKPackage of string list
	| IDKModule of string list * string
	| IDKSubType of string list * string * string
	| IDKModuleField of string list * string * string
	| IDKSubTypeField of string list * string * string * string
	| IDK

type import_display = import_display_kind * pos

let convert_import_to_something_usable path =
	let rec loop pack m t = function
		| (s,p) :: l ->
			let is_lower = is_lower_ident s in
			let is_display_pos = encloses_position !Parser.resume_display p in
			begin match is_lower,m,t with
				| _,None,Some _ ->
					assert false (* impossible, I think *)
				| true,Some m,None ->
					if is_display_pos then (IDKModuleField(List.rev pack,m,s),p)
					else (IDK,p) (* assume that we're done *)
				| _,Some m,Some t ->
					if is_display_pos then (IDKSubTypeField(List.rev pack,m,t,s),p)
					else (IDK,p)
				| true,None,None ->
					if is_display_pos then (IDKPackage (List.rev (s :: pack)),p)
					else loop (s :: pack) m t l
				| false,Some sm,None ->
					if is_display_pos then (IDKSubType (List.rev pack,sm,s),p)
					else loop pack m (Some s) l
				| false,None,None ->
					if is_display_pos then (IDKModule (List.rev pack,s),p)
					else loop pack (Some s) None l
			end
		| [] ->
			(IDK,null_pos)
	in
	loop [] None None path

let process_expr com e = match com.display with
	| DMToplevel -> find_enclosing com e
	| DMPosition | DMUsage | DMType -> find_before_pos com e
	| _ -> e

let add_import_position com p =
	com.shared.shared_display_information.import_positions <- PMap.add p (ref false) com.shared.shared_display_information.import_positions

let mark_import_position com p =
	try
		let r = PMap.find p com.shared.shared_display_information.import_positions in
		r := true
	with Not_found ->
		()

module Diagnostics = struct
	module DiagnosticsKind = struct
		type t =
			| DKUnusedImport
			| DKUnresolvedIdentifier
			| DKCompilerError

		let to_int = function
			| DKUnusedImport -> 0
			| DKUnresolvedIdentifier -> 1
			| DKCompilerError -> 2
	end

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
	open DiagnosticsKind

	let find_unused_variables com cf =
		let vars = Hashtbl.create 0 in
		let rec loop e = match e.eexpr with
			| TVar(v,eo) when Meta.has Meta.UserVariable v.v_meta ->
				Hashtbl.replace vars v.v_id v;
				(match eo with None -> () | Some e -> loop e)
			| TLocal v when Meta.has Meta.UserVariable v.v_meta ->
				Hashtbl.remove vars v.v_id;
			| _ ->
				Type.iter loop e
		in
		(match cf.cf_expr with None -> () | Some e -> loop e);
		Hashtbl.iter (fun _ v ->
			add_diagnostics_message com "Unused variable" v.v_pos DiagnosticsSeverity.Warning
		) vars

	let prepare com =
		List.iter (function
			| TClassDecl c ->
				let field = find_unused_variables com in
				List.iter field c.cl_ordered_fields;
				List.iter field c.cl_ordered_statics;
				(match c.cl_constructor with None -> () | Some cf -> field cf);
			| _ ->
				()
		) com.types

	let print_diagnostics ctx =
		let com = ctx.com in
		let diag = DynArray.create() in
		let add dk p sev args =
			DynArray.add diag (dk,p,sev,args)
		in
		let find_type i =
			let types = ref [] in
			Hashtbl.iter (fun _ m ->
				List.iter (fun mt ->
					let tinfos = t_infos mt in
					if snd tinfos.mt_path = i then
						types := JObject [
							"kind",JInt (UnresolvedIdentifierSuggestion.to_int UISImport);
							"name",JString (s_type_path m.m_path)
						] :: !types
				) m.m_types;
			) ctx.g.modules;
			!types
		in
		List.iter (fun (s,p,suggestions) ->
			let suggestions = List.map (fun (s,_) ->
				JObject [
					"kind",JInt (UnresolvedIdentifierSuggestion.to_int UISTypo);
					"name",JString s
				]
			) suggestions in
			add DKUnresolvedIdentifier p DiagnosticsSeverity.Error (suggestions @ (find_type s));
		) com.display_information.unresolved_identifiers;
		PMap.iter (fun p r ->
			if not !r then add DKUnusedImport p DiagnosticsSeverity.Warning []
		) com.shared.shared_display_information.import_positions;
		List.iter (fun (s,p,sev) ->
			add DKCompilerError p sev [JString s]
		) com.shared.shared_display_information.diagnostics_messages;
		let jl = DynArray.fold_left (fun acc (dk,p,sev,args) ->
			(JObject [
				"kind",JInt (to_int dk);
				"severity",JInt (DiagnosticsSeverity.to_int sev);
				"range",pos_to_json_range p;
				"args",JArray args;
				"file",JString (get_real_path p.pfile)
			]) :: acc
		) [] diag in
		let js = JArray jl in
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) js;
		Buffer.contents b

	let is_diagnostics_run ctx = match ctx.com.display with
		| DMDiagnostics true -> true
		| DMDiagnostics false -> ctx.is_display_file
		| _ -> false
end

let maybe_mark_import_position ctx p =
	if Diagnostics.is_diagnostics_run ctx then mark_import_position ctx.com p