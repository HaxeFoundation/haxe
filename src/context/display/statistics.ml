open Globals
open Ast
open Type
open Common
open Typecore
open DisplayTypes

open ImportHandling

type relation =
	| Implemented
	| Extended
	| Overridden
	| Referenced

type statistics_filter =
	| SFNone
	| SFPos of pos
	| SFFile of string

let collect_statistics ctx pfilter with_expressions =
	let relations = Hashtbl.create 0 in
	let symbols = Hashtbl.create 0 in
	let handled_modules = Hashtbl.create 0 in
	let path_key =
		let paths = Hashtbl.create 0 in
		(fun path ->
			try
				Hashtbl.find paths path
			with Not_found ->
				let unique = Path.UniqueKey.create path in
				Hashtbl.add paths path unique;
				unique
		)
	in
	let check_pos = match pfilter with
		| SFNone -> (fun p -> p <> null_pos)
		| SFPos p -> (fun p' -> p.pmin = p'.pmin && p.pmax = p'.pmax && path_key p.pfile = path_key p'.pfile)
		| SFFile s -> (fun p -> path_key p.pfile = path_key s)
	in
	let add_relation p r =
		if check_pos p then try
			let l = Hashtbl.find relations p in
			if not (List.mem r l) then
				Hashtbl.replace relations p (r :: l)
		with Not_found ->
			Hashtbl.add relations p [r]
	in
	let declare kind p =
		if check_pos p then begin
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
						add_relation cf'.cf_name_pos (Overridden,cf.cf_name_pos)
					with Not_found ->
						()
					end;
					loop c
				| _ ->
					()
			in
			loop c
		) c.cl_overrides
	in
	let collect_implementations c =
		List.iter (fun cf ->
			let rec loop c =
				begin try
					let cf' = PMap.find cf.cf_name c.cl_fields in
					add_relation cf.cf_name_pos (Implemented,cf'.cf_name_pos)
				with Not_found ->
					()
				end;
				List.iter loop c.cl_descendants
			in
			List.iter loop c.cl_descendants
		) c.cl_ordered_fields;
		let rec loop c' =
			add_relation c.cl_name_pos ((if c'.cl_interface then Extended else Implemented),c'.cl_name_pos);
			List.iter loop c'.cl_descendants
		in
		List.iter loop c.cl_descendants
	in
	let rec find_real_constructor c = match c.cl_constructor,c.cl_super with
		(* The pos comparison might be a bit weak, not sure... *)
		| Some cf,_ when not (Meta.has Meta.CompilerGenerated cf.cf_meta) && c.cl_pos <> cf.cf_pos -> cf
		| _,Some(c,_) -> find_real_constructor c
		| _,None -> raise Not_found
	in
	let var_decl v = declare (SKVariable v) v.v_pos in
	let patch_string_pos p s = { p with pmin = p.pmax - String.length s } in
	let related_fields = Hashtbl.create 0 in
	let field_reference co cf p =
		let p' = patch_string_pos p cf.cf_name in
		add_relation cf.cf_name_pos (Referenced,p');
		(* extend to related classes for instance fields *)
		if check_pos cf.cf_name_pos then match co with
		| Some c ->
			let id = (c.cl_path,cf.cf_name) in
			begin try
				let cfl = Hashtbl.find related_fields id in
				List.iter (fun cf -> add_relation cf.cf_name_pos (Referenced,p')) cfl
			with Not_found ->
				let cfl = ref [] in
				let check c =
					try
						let cf = PMap.find cf.cf_name c.cl_fields in
						add_relation cf.cf_name_pos (Referenced,p');
						cfl := cf :: !cfl
					with Not_found ->
						()
				in
				(* to children *)
				let rec loop c =
					List.iter (fun c ->
						check c;
						loop c;
					) c.cl_descendants
				in
				loop c;
				(* to parents *)
				let rec loop c =
					let f (c,_) =
						check c;
						loop c;
					in
					List.iter f c.cl_implements;
					Option.may f c.cl_super
				in
				loop c;
				Hashtbl.add related_fields id !cfl
			end
		| None ->
			()
	in
	let collect_references c e =
		let rec loop e = match e.eexpr with
			| TField(e1,fa) ->
				(* Check if the sub-expression is actually shorter than the whole one. This should
					detect cases where it was automatically generated. *)
				if e1.epos.pmin = e.epos.pmin && e1.epos.pmax <> e.epos.pmax then
					loop e1;
				begin match fa with
					| FStatic(_,cf) | FClosure(None,cf) ->
						field_reference None cf e.epos
					| FInstance(c,_,cf) | FClosure(Some(c,_),cf) ->
						field_reference (Some c) cf e.epos
					| FAnon cf ->
						declare  (SKField (cf,None)) cf.cf_name_pos;
						field_reference None cf e.epos
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
	let check_module m =
		if not (Hashtbl.mem handled_modules m.m_path) then begin
			Hashtbl.add handled_modules m.m_path true;
			List.iter (fun (p1,p2) ->
				add_relation p1 (Referenced,p2)
			) m.m_extra.m_display.m_inline_calls;
			List.iter (fun (p,pn) -> add_relation pn (Referenced,p)) m.m_extra.m_display.m_type_hints
		end
	in
	(* set up descendants *)
	let f = function
		| TClassDecl c ->
				List.iter (fun (iface,_) -> add_descendant iface c) c.cl_implements;
				begin match c.cl_super with
					| Some (csup,_) -> add_descendant csup c
					| None -> ()
				end;
		| _ ->
			()
	in
	let rec loop com =
		List.iter f com.types;
		Option.may loop (com.get_macros())
	in
	loop ctx.com;
	(* find things *)
	let f = function
		| TClassDecl c ->
			check_module c.cl_module;
			declare (if c.cl_interface then (SKInterface c) else (SKClass c)) c.cl_name_pos;
			begin match c.cl_super with
				| None -> ()
				| Some (c',_) ->
					let rec loop c' =
						add_relation c'.cl_name_pos (Extended,c.cl_name_pos);
						Option.may (fun (c',_) -> loop c') c'.cl_super
					in
					loop c'
			end;
			collect_overrides c;
			if c.cl_interface then
				collect_implementations c;
			let field cf =
				if cf.cf_pos.pmin > c.cl_name_pos.pmin then declare (SKField (cf,Some c.cl_path)) cf.cf_name_pos;
				if with_expressions then begin
					let _ = follow cf.cf_type in
					match cf.cf_expr with None -> () | Some e -> collect_references c e
				end
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
	(* TODO: Using syntax-exploration here is technically fine, but I worry about performance in real codebases. *)
	(* let find_symbols = Hashtbl.fold (fun _ kind acc ->
		let name = string_of_symbol kind in
		(name,kind) :: acc
	) symbols [] in
	let additional_modules = SyntaxExplorer.explore_uncached_modules ctx (CompilationServer.force()) find_symbols in
	List.iter (fun md ->
		List.iter f md.m_types
	) additional_modules; *)
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
		| SKConstructor _ -> "constructor"
		| SKEnumField _ -> "enum field"
		| SKVariable _ -> "variable"
		| SKOther -> "other"

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