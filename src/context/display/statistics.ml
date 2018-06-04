open Globals
open Ast
open Type
open Common
open Typecore

open ImportHandling

type relation =
	| Implemented
	| Extended
	| Overridden
	| Referenced

type symbol =
	| SKClass of tclass
	| SKInterface of tclass
	| SKEnum of tenum
	| SKTypedef of tdef
	| SKAbstract of tabstract
	| SKField of tclass_field
	| SKEnumField of tenum_field
	| SKVariable of tvar

let collect_statistics ctx =
	let relations = Hashtbl.create 0 in
	let symbols = Hashtbl.create 0 in
	let handled_modules = Hashtbl.create 0 in
	let add_relation pos r =
		if pos <> null_pos then try
			let l = Hashtbl.find relations pos in
			if not (List.mem r l) then
				Hashtbl.replace relations pos (r :: l)
		with Not_found ->
			Hashtbl.add relations pos [r]
	in
	let declare kind p =
		if p <> null_pos then begin
			if not (Hashtbl.mem relations p) then Hashtbl.add relations p [];
			Hashtbl.replace symbols p kind;
		end
	in
	let collect_overrides c =
		List.iter (fun cf ->
			let rec loop c = match c.cl_super with
				| Some (c,_) ->
					begin try
						let cf' = PMap.find cf.cf_name c.cl_fields in
						add_relation cf'.cf_name_pos (Overridden,cf.cf_pos)
					with Not_found ->
						loop c
					end
				| _ ->
					()
			in
			loop c
		) c.cl_overrides
	in
	let rec find_real_constructor c = match c.cl_constructor,c.cl_super with
		(* The pos comparison might be a bit weak, not sure... *)
		| Some cf,_ when not (Meta.has Meta.CompilerGenerated cf.cf_meta) && c.cl_pos <> cf.cf_pos -> cf
		| _,Some(c,_) -> find_real_constructor c
		| _,None -> raise Not_found
	in
	let var_decl v = declare (SKVariable v) v.v_pos in
	let patch_string_pos p s = { p with pmin = p.pmax - String.length s } in
	let field_reference cf p =
		add_relation cf.cf_name_pos (Referenced,patch_string_pos p cf.cf_name)
	in
	let collect_references c e =
		let rec loop e = match e.eexpr with
			| TField(e1,fa) ->
				(* Check if the sub-expression is actually shorter than the whole one. This should
					detect cases where it was automatically generated. *)
				if e1.epos.pmin = e.epos.pmin && e1.epos.pmax <> e.epos.pmax then
					loop e1;
				begin match fa with
					| FStatic(_,cf) | FInstance(_,_,cf) | FClosure(_,cf) ->
						field_reference cf e.epos
					| FAnon cf ->
						declare  (SKField cf) cf.cf_name_pos;
						field_reference cf e.epos
					| FEnum(_,ef) ->
						add_relation ef.ef_name_pos (Referenced,patch_string_pos e.epos ef.ef_name)
					| FDynamic _ ->
						()
				end
			| TTypeExpr mt ->
				let tinfos = t_infos mt in
				add_relation tinfos.mt_name_pos (Referenced,patch_string_pos e.epos (snd tinfos.mt_path))
			| TNew(c,_,el) ->
				List.iter loop el;
				(try add_relation (find_real_constructor c).cf_name_pos (Referenced,e.epos) with Not_found -> ());
			| TCall({eexpr = TConst TSuper},el) ->
				List.iter loop el;
				begin match c.cl_super with
					| Some(c,_) -> (try add_relation (find_real_constructor c).cf_name_pos (Referenced,e.epos) with Not_found -> ())
					| None -> ()
				end
			| TVar(v,eo) ->
				Option.may loop eo;
				var_decl v;
			| TFor(v,e1,e2) ->
				var_decl v;
				loop e1;
				loop e2;
			| TFunction tf ->
				List.iter (fun (v,_) -> var_decl v) tf.tf_args;
				loop tf.tf_expr;
			| TLocal v when e.epos.pmax - e.epos.pmin = String.length v.v_name ->
				add_relation v.v_pos (Referenced,e.epos)
			| _ ->
				Type.iter loop e
		in
		loop e
	in
	let rec explore_type_hint (p,t) =
		match t with
		| TMono r -> (match !r with None -> () | Some t -> explore_type_hint (p,t))
		| TLazy f -> explore_type_hint (p,lazy_type f)
		| TInst(({cl_name_pos = pn;cl_path = (_,name)}),_)
		| TEnum(({e_name_pos = pn;e_path = (_,name)}),_)
		| TType(({t_name_pos = pn;t_path = (_,name)}),_)
		| TAbstract(({a_name_pos = pn;a_path = (_,name)}),_) ->
			add_relation pn (Referenced,p)
		| TDynamic _ -> ()
		| TFun _ | TAnon _ -> ()
	in
	let check_module m =
		if not (Hashtbl.mem handled_modules m.m_path) then begin
			Hashtbl.add handled_modules m.m_path true;
			List.iter (fun (p1,p2) ->
				add_relation p1 (Referenced,p2)
			) m.m_extra.m_display.m_inline_calls;
			List.iter explore_type_hint m.m_extra.m_display.m_type_hints
		end
	in
	let f = function
		| TClassDecl c ->
			check_module c.cl_module;
			declare (if c.cl_interface then (SKInterface c) else (SKClass c)) c.cl_name_pos;
			List.iter (fun (c',_) -> add_relation c'.cl_name_pos ((if c.cl_interface then Extended else Implemented),c.cl_name_pos)) c.cl_implements;
			begin match c.cl_super with
				| None -> ()
				| Some (c',_) -> add_relation c'.cl_name_pos (Extended,c.cl_name_pos);
			end;
			collect_overrides c;
			let field cf =
				if cf.cf_pos.pmin > c.cl_name_pos.pmin then declare (SKField cf) cf.cf_name_pos;
				let _ = follow cf.cf_type in
				match cf.cf_expr with None -> () | Some e -> collect_references c e
			in
			Option.may field c.cl_constructor;
			List.iter field c.cl_ordered_fields;
			List.iter field c.cl_ordered_statics;
		| TEnumDecl en ->
			check_module en.e_module;
			declare (SKEnum en) en.e_name_pos;
			PMap.iter (fun _ ef -> declare (SKEnumField ef) ef.ef_name_pos) en.e_constrs
		| TTypeDecl td ->
			check_module td.t_module;
			declare (SKTypedef td) td.t_name_pos
		| TAbstractDecl a ->
			check_module a.a_module;
			declare (SKAbstract a) a.a_name_pos
	in
	let rec loop com =
		List.iter f com.types;
		Option.may loop (com.get_macros())
	in
	loop ctx.com;
	let l = List.fold_left (fun acc (_,cfi,_,cfo) -> match cfo with
		| Some cf -> if List.mem_assoc cf.cf_name_pos acc then acc else (cf.cf_name_pos,cfi.cf_name_pos) :: acc
		| None -> acc
	) [] ctx.com.display_information.interface_field_implementations in
	List.iter (fun (p,p') -> add_relation p' (Implemented,p)) l;
	(* let deal_with_imports paths =
		let check_subtype m s p =
			try
				let mt = List.find (fun mt -> snd (t_infos mt).mt_path = s) m.m_types in
				add_relation (t_infos mt).mt_name_pos (Referenced,p);
				Some mt
			with Not_found ->
				None
		in
		let check_module path p =
			let m = ctx.g.do_load_module ctx path p in
			m
		in
		let check_field c s p =
			let cf = PMap.find s c.cl_statics in
			add_relation cf.cf_name_pos (Referenced,p)
		in
		let check_subtype_field m ssub psub sfield pfield = match check_subtype m ssub psub with
			| Some (TClassDecl c) -> check_field c sfield pfield
			| _ -> ()
		in
		PMap.iter (fun p (_,path) ->
			match ImportHandling.convert_import_to_something_usable { p with pmin = p.pmax - 1; pmax = p.pmax - 1 } path,List.rev path with
			| (IDKSubType(sl,s1,s2),_),(_,psubtype) :: (_,pmodule) :: _ ->
				let m = check_module (sl,s1) pmodule in
				(*ignore(check_subtype m s1 pmodule);*)
				ignore(check_subtype m s2 psubtype)
			| (IDKModuleField(sl,s1,s2),_),(_,pfield) :: (_,pmodule) :: _ ->
				let m = check_module (sl,s1) pmodule in
				check_subtype_field m s1 pmodule s2 pfield
			| (IDKSubTypeField(sl,s1,s2,s3),_),(_,pfield) :: (_,psubtype) :: (_,pmodule) :: _ ->
				let m = check_module (sl,s1) pmodule in
				check_subtype_field m s2 psubtype s3 pfield
			| (IDKModule(sl,s),_),(_,pmodule) :: _ ->
				let m = check_module (sl,s) pmodule in
				ignore(check_subtype m s pmodule);
			| _ ->
				()
		) paths
	in
	if false then deal_with_imports ctx.com.shared.shared_display_information.import_positions; *)
	symbols,relations

module Printer = struct
	open Json

	let relation_to_string = function
		| Implemented -> "implementers"
		| Extended -> "subclasses"
		| Overridden -> "overrides"
		| Referenced -> "references"

	let symbol_to_string = function
		| SKClass _ -> "class type"
		| SKInterface _ -> "interface type"
		| SKEnum _ -> "enum type"
		| SKTypedef _ -> "typedef"
		| SKAbstract _ -> "abstract"
		| SKField _ -> "class field"
		| SKEnumField _ -> "enum field"
		| SKVariable _ -> "variable"

	let print_statistics (kinds,relations) =
		let files = Hashtbl.create 0 in
		Hashtbl.iter (fun p rl ->
			let file = Path.get_real_path p.pfile in
			try
				Hashtbl.replace files file ((p,rl) :: Hashtbl.find files file)
			with Not_found ->
				Hashtbl.add files file [p,rl]
		) relations;
		let ja = Hashtbl.fold (fun file relations acc ->
			let l = List.map (fun (p,rl) ->
				let h = Hashtbl.create 0 in
				List.iter (fun (r,p) ->
					let s = relation_to_string r in
					let jo = JObject [
						"range",Genjson.generate_pos_as_range p;
						"file",JString (Path.get_real_path p.pfile);
					] in
					try Hashtbl.replace h s (jo :: Hashtbl.find h s)
					with Not_found -> Hashtbl.add h s [jo]
				) rl;
				let l = Hashtbl.fold (fun s js acc -> (s,JArray js) :: acc) h [] in
				let l = ("range",Genjson.generate_pos_as_range p) :: l in
				let l = try ("kind",JString (symbol_to_string (Hashtbl.find kinds p))) :: l with Not_found -> l in
				JObject l
			) relations in
			(JObject [
				"file",JString file;
				"statistics",JArray l
			]) :: acc
		) files [] in
		string_of_json (JArray ja)
end