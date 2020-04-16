open Globals
open Ast
open DisplayTypes
open Typecore

type reference_kind =
	| KVar
	| KIdent
	| KAnyField
	| KClassField
	| KEnumField
	| KModuleType
	| KConstructor

let find_in_syntax symbols (pack,decls) =
	(* Employ some heuristics: We know what kind of symbol we are looking for, so let's
	   filter where we can. *)
	let check kind' name' =
		List.iter (fun (name,kind) ->
			if name = name' then match kind',kind with
				| KIdent,_
				| KAnyField,(SKField _ | SKConstructor _ | SKEnumField _)
				| KClassField,SKField _
				| KEnumField,SKEnumField _
				| KModuleType,(SKClass _ | SKEnum _ | SKTypedef _ | SKAbstract _)
				| KConstructor,(SKConstructor _ | SKClass _) ->
					raise Exit
				| _ ->
					()
		) symbols
	in
	let rec type_path kind path =
		check KModuleType path.tname;
		Option.may (check KModuleType) path.tsub;
		List.iter (function
			| TPType th -> type_hint th
			| TPExpr e -> expr e
		) path.tparams
	and type_hint th = match fst th with
		| CTPath path -> type_path KModuleType path
		| CTParent th | CTOptional th | CTNamed(_,th) -> type_hint th
		| CTFunction(thl,th) ->
			List.iter type_hint thl;
			type_hint th;
		| CTAnonymous cffl ->
			List.iter field cffl
		| CTExtend(tl,cffl) ->
			List.iter (fun (path,_) -> type_path KModuleType path) tl;
			List.iter field cffl;
		| CTIntersection tl ->
			List.iter type_hint tl
	and type_param tp =
		List.iter type_param tp.tp_params;
		Option.may type_hint tp.tp_constraints
	and expr (e,p) =
		begin match e with
		| EConst(Ident s) ->
			check KIdent s
		| EField(e1,s) ->
			expr e1;
			check KAnyField s;
		| EVars vl ->
			List.iter (fun (_,_,tho,eo) ->
				Option.may type_hint tho;
				expr_opt eo
			) vl;
		| ECast(e1,tho) ->
			expr e1;
			Option.may type_hint tho;
		| ENew((path,_),el) ->
			type_path KConstructor path;
			List.iter expr el;
		| EFunction(_,f) ->
			func f
		| ETry(e1,catches) ->
			expr e1;
			List.iter (fun (_,th,e,_) ->
				Option.may type_hint th;
				expr e
			) catches;
		| ECheckType(e1,th) ->
			expr e1;
			type_hint th;
		| _ ->
			iter_expr expr (e,p)
		end
	and expr_opt eo = match eo with
		| None -> ()
		| Some e -> expr e
	and func f =
		List.iter (fun ((s,p),_,_,tho,eo) ->
			Option.may type_hint tho;
			expr_opt eo
		) f.f_args;
		List.iter type_param f.f_params;
		Option.may type_hint f.f_type;
		expr_opt f.f_expr
	and field cff =
		check KClassField (fst cff.cff_name);
		match cff.cff_kind with
		| FVar(tho,eo) ->
			Option.may type_hint tho;
			expr_opt eo
		| FFun f ->
			func f
		| FProp(_,_,tho,eo) ->
			Option.may type_hint tho;
			expr_opt eo
	in
	List.iter (fun (td,p) -> match td with
		| EImport(path,_) | EUsing path ->
			begin match fst (ImportHandling.convert_import_to_something_usable null_pos path) with
			| IDKModule(_,s) -> check KModuleType s
			| IDKSubType(_,s1,s2) ->
				check KModuleType s1;
				check KModuleType s2;
			| IDKSubTypeField(_,s1,s2,s3) ->
				check KModuleType s1;
				check KModuleType s2;
				check KAnyField s3;
			| IDKModuleField(_,s1,s2) ->
				check KModuleType s1;
				check KAnyField s2;
			| IDKPackage _ | IDK ->
				()
			end;
		| EClass d ->
			check KModuleType (fst d.d_name);
			List.iter (function
				| HExtends(path,_) | HImplements(path,_) -> type_path KModuleType path
				| _ -> ()
			) d.d_flags;
			List.iter type_param d.d_params;
			List.iter field d.d_data
		| EEnum d ->
			check KModuleType (fst d.d_name);
			List.iter (fun ef ->
				Option.may type_hint ef.ec_type;
				check KEnumField (fst ef.ec_name);
				List.iter type_param ef.ec_params;
			) d.d_data;
			List.iter type_param d.d_params;
		| ETypedef d ->
			check KModuleType (fst d.d_name);
			List.iter type_param d.d_params;
			type_hint d.d_data;
		| EAbstract d ->
			check KModuleType (fst d.d_name);
			List.iter field d.d_data;
			List.iter type_param d.d_params;
			List.iter (function
				| AbFrom th | AbTo th | AbOver th -> type_hint th
				| _ -> ()
			) d.d_flags;
	) decls

let explore_uncached_modules tctx cs symbols =
	DisplayToplevel.init_or_update_server cs tctx.com ["display";"references"];
	let cc = CommonCache.get_cache cs tctx.com in
	let files = cc#get_files in
	let modules = cc#get_modules in
	let t = Timer.timer ["display";"references";"candidates"] in
	let acc = Hashtbl.fold (fun file cfile acc ->
		let module_name = CompilationServer.get_module_name_of_cfile file cfile in
		if Hashtbl.mem modules (cfile.c_package,module_name) then
			acc
		else try
			find_in_syntax symbols (cfile.c_package,cfile.c_decls);
			acc
		with Exit ->
			begin try
				let m = tctx.g.do_load_module tctx (cfile.c_package,module_name) null_pos in
				(* We have to flush immediately so we catch exceptions from weird modules *)
				Typecore.flush_pass tctx Typecore.PFinal "final";
				m :: acc
			with _ ->
				acc
			end
	) files [] in
	t();
	acc