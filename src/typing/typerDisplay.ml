open Globals
open Ast
open Common.DisplayMode
open Common
open Type
open Typecore
open TyperBase
open Fields
open Calls
open Error

let rec handle_display ctx e_ast with_type =
	let old = ctx.in_display,ctx.in_call_args in
	ctx.in_display <- true;
	ctx.in_call_args <- false;
	let e = match e_ast,with_type with
	| (EConst (Ident "$type"),_),_ ->
		let mono = mk_mono() in
		raise (Display.DisplaySignatures ([((["expression",false,mono],mono),Some "Outputs type of argument as a warning and uses argument as value")],0))
	| (EConst (Ident "trace"),_),_ ->
		raise (Display.DisplaySignatures ([((["value",false,t_dynamic],ctx.com.basic.tvoid),Some "Print given arguments")],0))
	| (EConst (Ident "_"),p),WithType t ->
		mk (TConst TNull) t p (* This is "probably" a bind skip, let's just use the expected type *)
	| _ -> try
		type_expr ctx e_ast with_type
	with Error (Unknown_ident n,_) ->
		raise (Parser.TypePath ([n],None,false))
	| Error (Type_not_found (path,_),_) as err ->
		begin try
			raise (Display.DisplayFields (DisplayFields.get_submodule_fields ctx path))
		with Not_found ->
			raise err
		end
	in
	let p = e.epos in
	let e = match with_type with
		| WithType t -> (try AbstractCast.cast_or_unify_raise ctx t e e.epos with Error (Unify l,p) -> e)
		| _ -> e
	in
	ctx.in_display <- fst old;
	ctx.in_call_args <- snd old;
	display_expr ctx e_ast e with_type p

and handle_signature_display ctx e_ast with_type =
	ctx.in_display <- true;
	let p = pos e_ast in
	let handle_call tl el p0 =
		let rec follow_with_callable (t,doc) = match follow t with
			| TAbstract(a,tl) when Meta.has Meta.Callable a.a_meta -> follow_with_callable (Abstract.get_underlying_type a tl,doc)
			| TFun(args,ret) -> ((args,ret),doc)
			| _ -> error ("Not a callable type: " ^ (s_type (print_context()) t)) p
		in
		let tl = List.map follow_with_callable tl in
		let rec loop i p1 el = match el with
			| (e,p2) :: el ->
				if Display.is_display_position (punion p1 p2) then i else loop (i + 1) p2 el
			| [] ->
				i
		in
		let display_arg = loop 0 p0 el in
		(* If our display position exceeds the argument number we add a null expression in order to make
		unify_call_args error out. *)
		let el = if display_arg >= List.length el then el @ [EConst (Ident "null"),null_pos] else el in
		let rec loop acc tl = match tl with
			| (t,doc) :: tl ->
				let keep (args,r) =
					begin try
						let _ = unify_call_args' ctx el args r p false false in
						true
					with
					| Error(Call_error (Not_enough_arguments _),_) -> true
					| _ -> false
					end
				in
				loop (if keep t then (t,doc) :: acc else acc) tl
			| [] ->
				acc
		in
		let overloads = match loop [] tl with [] -> tl | tl -> tl in
		raise (Display.DisplaySignatures(overloads,display_arg))
	in
	let find_constructor_types t = match follow t with
		| TInst (c,tl) | TAbstract({a_impl = Some c},tl) ->
			let ct,cf = get_constructor ctx c tl p in
			let tl = (ct,cf.cf_doc) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc) cf.cf_overloads in
			tl
		| _ ->
			[]
	in
	match fst e_ast with
		| ECall(e1,el) ->
			let e1 = try
				type_expr ctx e1 Value
			with Error (Unknown_ident "trace",_) ->
				let e = expr_of_type_path (["haxe";"Log"],"trace") p in
				type_expr ctx e Value
			in
			let tl = match e1.eexpr with
				| TField(_,fa) ->
					begin match extract_field fa with
						| Some cf -> (e1.etype,cf.cf_doc) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc) cf.cf_overloads
						| None -> [e1.etype,None]
					end
				| TConst TSuper ->
					find_constructor_types e1.etype
				| _ -> [e1.etype,None]
			in
			handle_call tl el e1.epos
		| ENew(tpath,el) ->
			let t = Typeload.load_instance ctx tpath true p in
			handle_call (find_constructor_types t) el (pos tpath)
		| EObjectDecl fl ->
			let fail () = error "Unexpected type or something" p in
			begin match with_type with
			| WithType t ->
				begin match follow t with
				| TAnon an ->
					let fl = PMap.foldi (fun k cf acc ->
						if Expr.field_mem_assoc k fl then acc
						else ((k,false,cf.cf_type),cf.cf_name_pos) :: acc
					) an.a_fields [] in
					let fl = List.sort (fun (_,p1) (_,p2) -> compare p1 p2) fl in
					let fl = List.map fst fl in
					let fl = [(fl,t),None] in
					raise (Display.DisplaySignatures (fl,0))
				| _ -> fail()
				end
			| _ -> fail()
			end
		| _ -> error "Call expected" p

and display_expr ctx e_ast e with_type p =
	let get_super_constructor () = match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let _, f = get_constructor ctx c params p in
			f
	in
	match ctx.com.display.dms_kind with
	| DMResolve _ | DMPackage | DMSignature ->
		assert false
	| DMType ->
		let rec loop e = match e.eexpr with
			| TVar(v,_) -> v.v_type,None
			| TCall({eexpr = TConst TSuper; etype = t},_) -> t,None
			| TNew({cl_kind = KAbstractImpl a},tl,_) -> TType(abstract_module_type a tl,[]),None
			| TNew(c,tl,_) ->
				let t,_ = get_constructor ctx c tl p in
				t,None
			| TTypeExpr (TClassDecl {cl_kind = KAbstractImpl a}) -> TType(abstract_module_type a (List.map snd a.a_params),[]),None
			| TField(e1,FDynamic "bind") when (match follow e1.etype with TFun _ -> true | _ -> false) -> e1.etype,None
			| TReturn (Some e1) -> loop e1 (* No point in letting the internal Dynamic surface (issue #5655) *)
			| TField(_,(FStatic(c,cf) | FInstance(c,_,cf) | FClosure(Some(c,_),cf))) ->
				if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
				e.etype,cf.cf_doc
			| TField(_,FEnum(_,ef)) ->
				e.etype,ef.ef_doc
			| _ -> e.etype,None
		in
		let t,doc = loop e in
		raise (Display.DisplayType (t,p,doc))
	| DMUsage _ ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) ->
			ef.ef_meta <- (Meta.Usage,[],p) :: ef.ef_meta;
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) ->
			cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
		| TLocal v | TVar(v,_) ->
			v.v_meta <- (Meta.Usage,[],p) :: v.v_meta;
		| TTypeExpr mt ->
			let ti = t_infos mt in
			ti.mt_meta <- (Meta.Usage,[],p) :: ti.mt_meta;
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
			with Not_found ->
				()
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf = get_super_constructor() in
				cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
			with Not_found ->
				()
			end
		| TConst TSuper ->
			begin match ctx.curclass.cl_super with
				| None -> ()
				| Some (c,_) -> c.cl_meta <- (Meta.Usage,[],p) :: c.cl_meta;
			end
		| TCall(e1,_) ->
			loop e1
		| _ ->
			()
		in
		loop e;
		e
	| DMPosition ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) -> [ef.ef_pos]
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) -> [cf.cf_pos]
		| TLocal v | TVar(v,_) -> [v.v_pos]
		| TTypeExpr mt -> [(t_infos mt).mt_pos]
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				[cf.cf_pos]
			with Not_found ->
				[]
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf = get_super_constructor() in
				[cf.cf_pos]
			with Not_found ->
				[]
			end
		| TConst TSuper ->
			begin match ctx.curclass.cl_super with
				| None -> []
				| Some (c,_) -> [c.cl_pos]
			end
		| TCall(e1,_) ->
			loop e1
		| _ ->
			[]
		in
		let pl = loop e in
		raise (Display.DisplayPosition pl);
	| DMToplevel ->
		raise (Display.DisplayToplevel (DisplayToplevel.collect ctx false))
	| DMField | DMNone | DMModuleSymbols _ | DMDiagnostics _ | DMStatistics ->
		let fields = DisplayFields.collect ctx e_ast e with_type p in
		raise (Display.DisplayFields fields)