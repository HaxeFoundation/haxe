open Ast

open DisplayTypes.SymbolKind

let collect_module_symbols with_locals (pack,decls) =
	let l = DynArray.create() in
	let add name kind location parent =
		let si = DisplayTypes.SymbolInformation.make name kind location (if parent = "" then None else Some parent) in
		DynArray.add l si;
	in
	let rec expr parent (e,p) =
		let add name kind location = add name kind location parent in
		begin match e with
		| EVars vl ->
			List.iter (fun ((s,p),_,eo) ->
				add s Variable p;
				expr_opt parent eo
			) vl
		| ETry(e1,catches) ->
			expr parent e1;
			List.iter (fun ((s,p),_,e,_) ->
				add s Variable p;
				expr parent e
			) catches;
		| EFunction(Some s,f) ->
			add s Function p;
			func parent f
		| EBinop(OpIn,(EConst(Ident s),p),e2) ->
			add s Variable p;
			expr parent e2;
		| _ ->
			iter_expr (expr parent) (e,p)
		end
	and expr_opt parent eo = match eo with
		| None -> ()
		| Some e -> expr parent e
	and func parent f =
		List.iter (fun ((s,p),_,_,_,eo) ->
			add s Variable p parent;
			expr_opt parent eo
		) f.f_args;
		expr_opt parent f.f_expr
	in
	let field parent cff =
		let field_parent = parent ^ "." ^ (fst cff.cff_name) in
		match cff.cff_kind with
		| FVar(_,eo) ->
			add (fst cff.cff_name) Field cff.cff_pos parent;
			if with_locals then expr_opt field_parent eo
		| FFun f ->
			add (fst cff.cff_name) (if fst cff.cff_name = "new" then Constructor else Method) cff.cff_pos parent;
			if with_locals then func field_parent f
		| FProp(_,_,_,eo) ->
			add (fst cff.cff_name) Property cff.cff_pos parent;
			if with_locals then expr_opt field_parent eo
	in
	List.iter (fun (td,p) -> match td with
		| EImport _ | EUsing _ ->
			() (* TODO: Can we do anything with these? *)
		| EClass d ->
			add (fst d.d_name) (if List.mem HInterface d.d_flags then Interface else Class) p "";
			List.iter (field (fst d.d_name)) d.d_data
		| EEnum d ->
			add (fst d.d_name) Enum p "";
			List.iter (fun ef ->
				add (fst ef.ec_name) Method ef.ec_pos (fst d.d_name)
			) d.d_data
		| ETypedef d ->
			add (fst d.d_name) Typedef p "";
			(match d.d_data with
			| CTAnonymous fields,_ ->
				List.iter (field (fst d.d_name)) fields
			| _ -> ())
		| EAbstract d ->
			add (fst d.d_name) Abstract p "";
			List.iter (field (fst d.d_name)) d.d_data
	) decls;
	l

module Printer = struct
	open Json
	open DisplayTypes.SymbolKind
	open DisplayTypes.SymbolInformation

	let print_module_symbols com symbols filter =
		let regex = Option.map Str.regexp_case_fold filter in
		let reported = Hashtbl.create 0 in
		let add si =
			if Hashtbl.mem reported si.pos then false
			else begin
				let b = match regex with
					| None -> true
					| Some regex -> (try ignore(Str.search_forward regex si.name 0); true with Not_found -> false)
				in
				Hashtbl.replace reported si.pos true;
				b
			end
		in
		let ja = List.fold_left (fun acc (file,l) ->
			let jl = ExtList.List.filter_map (fun si ->
				if not (add si) then
					None
				else begin
					let l =
						("name",JString si.name) ::
						("kind",JInt (to_int si.kind)) ::
						("range", Genjson.generate_pos_as_range si.pos) ::
						(match si.container_name with None -> [] | Some s -> ["containerName",JString s])
					in
					Some (JObject l)
				end
			) (DynArray.to_list l) in
			if jl = [] then
				acc
			else
				(JObject [
					"file",JString file;
					"symbols",JArray jl
				]) :: acc
		) [] symbols in
		let js = JArray ja in
		string_of_json js
end