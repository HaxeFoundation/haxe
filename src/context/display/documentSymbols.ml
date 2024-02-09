open Ast
open Globals
open DisplayTypes.SymbolKind

let collect_module_symbols mname with_locals (pack,decls) =
	let l = DynArray.create() in
	let add name kind location parent deprecated =
		let si = DisplayTypes.SymbolInformation.make name kind location (if parent = "" then None else Some parent) deprecated in
		DynArray.add l si;
	in
	let rec expr parent (e,p) =
		let add name kind location = add name kind location parent in
		begin match e with
		| EVars vl ->
			List.iter (fun v ->
				add (fst v.ev_name) Variable (snd v.ev_name) false;
				expr_opt parent v.ev_expr
			) vl
		| ETry(e1,catches) ->
			expr parent e1;
			List.iter (fun ((s,p),_,e,_) ->
				add s Variable p false;
				expr parent e
			) catches;
		| EFunction(FKNamed ((s,_),_),f) ->
			add s Function p false;
			func parent f
		| EBinop(OpIn,(EConst(Ident s),p),e2) ->
			add s Variable p false;
			expr parent e2;
		| _ ->
			iter_expr (expr parent) (e,p)
		end
	and expr_opt parent eo = match eo with
		| None -> ()
		| Some e -> expr parent e
	and func parent f =
		List.iter (fun ((s,p),_,_,_,eo) ->
			add s Variable p parent false;
			expr_opt parent eo
		) f.f_args;
		expr_opt parent f.f_expr
	in
	let is_deprecated meta = Meta.has Meta.Deprecated meta in
	let field' parent parent_kind cff_name cff_kind cff_access cff_pos cff_meta =
		let field_parent = parent ^ "." ^ (fst cff_name) in
		let add_field kind = add (fst cff_name) kind cff_pos parent (is_deprecated cff_meta) in
		match cff_kind with
		| FVar(_,eo) ->
			add_field (
				if parent_kind = EnumAbstract && not (List.mem_assoc AStatic cff_access) then EnumMember
				else if (List.mem_assoc AInline cff_access) then Constant
				else Field
			);
			if with_locals then expr_opt field_parent eo
		| FFun f ->
			add_field (
				if fst cff_name = "new" then Constructor
				else if ((parent_kind = EnumAbstract || parent_kind = Abstract) && Meta.has_one_of [Meta.Op; Meta.ArrayAccess; Meta.Resolve] cff_meta) then Operator
				else Method
			);
			if with_locals then func field_parent f
		| FProp(_,_,_,eo) ->
			add_field Property;
			if with_locals then expr_opt field_parent eo
	in
	let field parent parent_kind cff =
		field' parent parent_kind cff.cff_name cff.cff_kind cff.cff_access cff.cff_pos cff.cff_meta
	in
	let type_decls = Hashtbl.create 0 in
	List.iter (fun (td,p) ->
		let get_decl_path d =
			let module_name = Path.module_name_of_file p.pfile in
			let type_name = fst d.d_name in
			let is_primary_type = type_name = module_name in
			let type_path = if is_primary_type then pack else pack @ [module_name] in
			type_path, type_name
		in
		let string_of_path l = String.concat "." l in
		let add_type d kind =
			let type_path, type_name = get_decl_path d in
			Hashtbl.add type_decls type_name ();
			add type_name kind p (string_of_path type_path) (is_deprecated d.d_meta);
			string_of_path (type_path @ [type_name])
		in
		match td with
		| EImport _ | EUsing _ ->
			()
		| EClass d ->
			let kind = if List.mem HInterface d.d_flags then Interface else Class in
			let parent = add_type d kind in
			List.iter (field parent kind) d.d_data
		| EEnum d ->
			let parent = add_type d Enum in
			List.iter (fun ef ->
				add (fst ef.ec_name) EnumMember ef.ec_pos parent (is_deprecated ef.ec_meta)
			) d.d_data
		| ETypedef d ->
			(match d.d_data with
			| CTAnonymous fields,_ ->
				let parent = add_type d Struct in
				List.iter (field parent Struct) fields
			| _ ->
				ignore(add_type d TypeAlias)
			)
		| EAbstract d ->
			let kind = if List.mem AbEnum d.d_flags then EnumAbstract else Abstract in
			let parent = add_type d kind in
			List.iter (field parent kind) d.d_data
		| EStatic d ->
			let path, name = get_decl_path d in
			let dotpath = string_of_path path in
			field' dotpath Class d.d_name d.d_data d.d_flags p d.d_meta
	) decls;
	begin match mname with
	| Some(file,mname) when not (Hashtbl.mem type_decls mname) ->
		add mname Module {pfile = file; pmin = 0; pmax = 0} (String.concat "." pack) false
	| _ ->
		()
	end;
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
					let l = if si.deprecated then ("isDeprecated",JBool true) :: l else l in
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