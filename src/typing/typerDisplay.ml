open Globals
open Ast
open DisplayTypes
open DisplayMode
open CompletionResultKind
open CompletionItem
open CompletionModuleKind
open CompletionModuleType
open ClassFieldOrigin
open DisplayException
open DisplayEmitter
open Display
open Common
open Type
open Typecore
open TyperBase
open Fields
open CallUnification
open Calls
open Error
open FieldAccess

let convert_function_signature ctx values (args,ret) = match CompletionType.from_type (get_import_status ctx) ~values (TFun(args,ret)) with
	| CompletionType.CTFunction ctf -> ((args,ret),ctf)
	| _ -> die "" __LOC__

let completion_item_of_expr ctx e =
	let retype e s t =
		try
			let e' = type_expr ctx (EConst(Ident s),null_pos) (WithType.with_type t) in
			Texpr.equal e e'
		with _ ->
			false
	in
	let tpair ?(values=PMap.empty) t =
		let ct = CompletionType.from_type (get_import_status ctx) ~values t in
		(t,ct)
	in
	let of_field e origin cf scope make_ci =
		let is_qualified = retype e cf.cf_name e.etype in
		make_ci (CompletionClassField.make cf scope origin is_qualified) (tpair ~values:(get_value_meta cf.cf_meta) e.etype)
	in
	let of_enum_field e origin ef =
		let is_qualified = retype e ef.ef_name e.etype in
		make_ci_enum_field (CompletionEnumField.make ef origin is_qualified) (tpair e.etype)
	in
	let itexpr e =
		let t = tpair e.etype in
		make_ci_expr e t
	in
	let was_probably_literal_abstract v t = match follow t with
		| TAbstract(a,tl) ->
			type_iseq (Abstract.get_underlying_type a tl) v.v_type
		| _ ->
			false
	in
	let rec loop e = match e.eexpr with
		| TLocal ({v_kind = VAbstractThis} as v) when was_probably_literal_abstract v e.etype ->
			(* `abstract` is typed as ((this : Underlying) : Abstract), so we detect it like that.
			   This could lead to false positives if somebody really tries, but even in that case
				the displayed type is not wrong. *)
			make_ci_literal "abstract" (tpair e.etype)
		| TLocal v | TVar(v,_) -> make_ci_local v (tpair ~values:(get_value_meta v.v_meta) v.v_type)
		| TField(e1,FStatic(c,cf)) ->
			let te,cf = DisplayToplevel.maybe_resolve_macro_field ctx e.etype c cf in
			Display.merge_core_doc ctx (TClassDecl c);
			let decl = decl_of_class c in
			let origin = match c.cl_kind,e1.eexpr with
				| KAbstractImpl _,_ when has_class_field_flag cf CfImpl -> Self decl
				| _,TMeta((Meta.StaticExtension,_,_),_) -> StaticExtension decl
				| _ -> Self decl
			in
			let make_ci = match c.cl_kind with
				| KAbstractImpl a when has_class_field_flag cf CfEnum -> make_ci_enum_abstract_field a
				| _ -> make_ci_class_field
			in
			of_field {e with etype = te} origin cf CFSStatic make_ci
		| TField(e1,(FInstance(c,_,cf) | FClosure(Some(c,_),cf))) ->
			let te,cf = DisplayToplevel.maybe_resolve_macro_field ctx e.etype c cf in
			Display.merge_core_doc ctx (TClassDecl c);
			let origin = match follow e1.etype with
			| TInst(c',_) when c != c' ->
				Parent (TClassDecl c)
			| _ ->
				Self (TClassDecl c)
			in
			of_field {e with etype = te} origin cf CFSMember make_ci_class_field
		| TField(_,FEnum(en,ef)) -> of_enum_field e (Self (TEnumDecl en)) ef
		| TField(e1,(FAnon cf | FClosure(None,cf))) ->
			begin match follow e1.etype with
				| TAnon an ->
					let origin = match e1.etype with
						| TType(td,_) -> Self (TTypeDecl td)
						| _ -> AnonymousStructure an
					in
					of_field e origin cf CFSMember make_ci_class_field
				| _ -> itexpr e
			end
		| TTypeExpr (TClassDecl {cl_kind = KAbstractImpl a}) ->
			Display.merge_core_doc ctx (TAbstractDecl a);
			let t = TType(abstract_module_type a (extract_param_types a.a_params),[]) in
			let t = tpair t in
			make_ci_type (CompletionModuleType.of_module_type (TAbstractDecl a)) ImportStatus.Imported (Some t)
		| TTypeExpr mt ->
			Display.merge_core_doc ctx mt;
			let t = tpair e.etype in
			make_ci_type (CompletionModuleType.of_module_type mt) ImportStatus.Imported (Some t) (* TODO *)
		| TConst (TThis | TSuper) -> itexpr e (* TODO *)
		| TConst(ct) -> make_ci_literal (s_const ct) (tpair e.etype)
		| TObjectDecl _ ->
			begin match follow e.etype with
				| TAnon an -> make_ci_anon an (tpair e.etype)
				| _ -> itexpr e
			end
		| TNew(c,tl,el) ->
			Display.merge_core_doc ctx (TClassDecl c);
			(* begin match fst e_ast with
			| EConst (Regexp (r,opt)) ->
				let present,absent = List.partition (String.contains opt) ['g';'i';'m';'s';'u'] in
				let doc flag desc = Printf.sprintf "* %s: %s" (String.make 1 flag) desc in
				let f c = match c with
					| 'g' -> doc c "global split and replace"
					| 'i' -> doc c "case insensitive matching"
					| 'm' -> doc c "multiline matching"
					| 's' -> doc c "dot also match newlines"
					| 'u' -> doc c "use UTF-8 matching"
					| _ -> die "" __LOC__
				in
				let present = List.map f present in
				let present = match present with [] -> [] | _ -> "\n\nActive flags:\n\n" :: present in
				let absent = List.map f absent in
				let absent = match absent with [] -> [] | _ -> "\n\nInactive flags:\n\n" :: absent in
				(TInst(c,tl)),Some ("Regular expression\n\n" ^ (String.concat "\n" (present @ absent)))
			| _ -> *)
				let fa = get_constructor_access c tl e.epos in
				let fcc = unify_field_call ctx fa el [] e.epos false in
				let cf = fcc.fc_field in
				let t = match follow (FieldAccess.get_map_function fa cf.cf_type) with
					| TFun(args,_) -> TFun(args,TInst(c,tl))
					| t -> t
				in
				make_ci_class_field (CompletionClassField.make cf CFSConstructor (Self (decl_of_class c)) true) (tpair ~values:(get_value_meta cf.cf_meta) t)
			(* end *)
		| TCall({eexpr = TConst TSuper; etype = t} as e1,_) ->
			itexpr e1 (* TODO *)
		| TCast(e1,_) -> loop {e1 with etype = e.etype}
		| TParenthesis e1 | TMeta(_,e1) -> loop e1
		| _ -> itexpr e
	in
	loop e

let get_expected_type ctx with_type =
	let t = match with_type with
		| WithType.WithType(t,_) -> Some t
		| _ -> None
	in
	match t with
		| None -> None
		| Some t ->
			let from_type = CompletionType.from_type (get_import_status ctx) in
			let t_followed = Type.map follow (follow t) in
			let compatible = match t_followed with
					| TInst(c,tl) when Meta.has Meta.StructInit c.cl_meta ->
						let fields = Some (get_struct_init_anon_fields c tl) in
						let ta = mk_anon ?fields (ref Closed) in
						[from_type ta]
					| _ ->
						[]
			in
			Some {
				expected_type = from_type t;
				expected_type_followed = from_type t_followed;
				compatible_types = compatible;
			}

let raise_toplevel ctx dk with_type (subject,psubject) =
	let expected_type = get_expected_type ctx with_type in
	DisplayToplevel.collect_and_raise ctx (match dk with DKPattern _ -> TKPattern psubject | _ -> TKExpr psubject) with_type (CRToplevel expected_type) (subject,psubject) psubject

let display_dollar_type ctx p make_type =
	let mono = spawn_monomorph ctx.e p in
	let doc = doc_from_string "Outputs type of argument as a warning and uses argument as value" in
	let arg = ["expression",false,mono] in
	begin match ctx.com.display.dms_kind with
	| DMSignature ->
		raise_signatures [(convert_function_signature ctx PMap.empty (arg,mono),doc)] 0 0 SKCall
	| DMHover ->
		let t = TFun(arg,mono) in
		raise_hover (make_ci_expr (mk (TIdent "trace") t p) (make_type t)) (Some (WithType.named_argument "expression")) p
	| DMDefinition | DMTypeDefinition ->
		raise_positions []
	| _ ->
		raise_typing_error "Unsupported method" p
	end

let rec handle_signature_display ctx e_ast with_type =
	ctx.f.in_display <- true;
	let p = pos e_ast in
	let handle_call tl el p0 =
		let rec follow_with_callable (t,doc,values) = match follow t with
			| TAbstract(a,tl) when Meta.has Meta.Callable a.a_meta -> follow_with_callable (Abstract.get_underlying_type a tl,doc,values)
			| TFun(args,ret) -> ((args,ret),doc,values)
			| _ -> raise_typing_error ("Not a callable type: " ^ (s_type (print_context()) t)) p
		in
		let tl = List.map follow_with_callable tl in
		let rec loop i acc el = match el with
			| e :: el ->
				begin match fst e with
				| EDisplay(e1,DKMarked) -> i,List.rev (e1 :: acc) @ el
				| _ -> loop (i + 1) (e :: acc) el
				end
			| [] ->
				0,List.rev acc
		in
		let display_arg,el = loop 0 [] el in
		(* If our display position exceeds the argument number we add a null expression in order to make
		unify_call_args error out. *)
		let el = if el <> [] && display_arg >= List.length el then el @ [EConst (Ident "null"),null_pos] else el in
		let rec loop acc tl = match tl with
			| (t,doc,values) :: tl ->
				let keep (args,r) =
					begin try
						let _ = unify_call_args ctx el args r p false false false in
						true
					with
					| Error { err_message = Call_error (Not_enough_arguments _) } -> true
					| _ -> false
					end
				in
				loop (if keep t then (t,doc,values) :: acc else acc) tl
			| [] ->
				acc
		in
		let overloads = match loop [] tl with [] -> tl | tl -> tl in
		let overloads = List.map (fun (t,doc,values) -> (convert_function_signature ctx values t,doc)) overloads in
		raise_signatures overloads 0 (* ? *) display_arg SKCall
	in
	let process_overloads stat co (map,cf,t) =
		let is_wacky_overload = not (has_class_field_flag cf CfOverload) in
		let can_access cf = match co with
			| Some c -> can_access ctx c cf stat
			| None -> true
		in
		let l = (t,cf) :: List.rev_map (fun cf -> map cf.cf_type,cf) cf.cf_overloads in
		let l = List.filter (fun (_,cf) -> can_access cf) l in
		let l = List.map (fun (t,cf') ->
			(* Ghetto overloads have their documentation on the main field. *)
			let doc = if is_wacky_overload then cf.cf_doc else cf'.cf_doc in
			(t,doc,get_value_meta cf.cf_meta)
		) l in
		l
	in
	let find_constructor_types t = match follow t with
		| TInst ({cl_kind = KTypeParameter ttp} as c,_) ->
			let rec loop tl = match tl with
				| [] -> raise_typing_error_ext (make_error (No_constructor (TClassDecl c)) p)
				| t :: tl -> match follow t with
					| TAbstract({a_path = ["haxe"],"Constructible"},[t]) -> t
					| _ -> loop tl
			in
			[loop (get_constraints ttp),None,PMap.empty]
		| TInst (c,tl) | TAbstract({a_impl = Some c},tl) ->
			Display.merge_core_doc ctx (TClassDecl c);
			let fa = get_constructor_access c tl p in
			let map = FieldAccess.get_map_function fa in
			let cf = fa.fa_field in
			process_overloads false (Some c) (map,cf,cf.cf_type)
		| _ ->
			[]
	in
	match fst e_ast with
		| ECall(e1,el) ->
			let def () =
				try
					acc_get ctx (!type_call_target_ref ctx e1 el with_type None)
				with
				| Error { err_message = Unknown_ident "trace" } ->
					let e = expr_of_type_path (["haxe";"Log"],"trace") p in
					type_expr ctx e WithType.value
				| Error { err_message = Unknown_ident "$type"; err_pos = p } ->
					display_dollar_type ctx p (fun t -> t,(CompletionType.from_type (get_import_status ctx) t))
			in
			let e1 = match e1 with
				| (EField (e,"bind",_),p) ->
					let e = type_expr ctx e WithType.value in
					(match follow e.etype with
						| TFun signature -> e
						| _ -> def ())
				| _ ->	def()
			in
			let tl = match e1.eexpr with
				| TField(_,fa) ->
					begin match fa with
						| FStatic(c,cf) ->
							let t,cf = DisplayToplevel.maybe_resolve_macro_field ctx e1.etype c cf in
							process_overloads true (Some c) ((fun t -> t),cf,t)
						| FInstance(c,tl,cf) | FClosure(Some(c,tl),cf) ->
							let t,cf = DisplayToplevel.maybe_resolve_macro_field ctx e1.etype c cf in
							process_overloads false (Some c) (TClass.get_map_function c tl,cf,t)
						| FAnon cf | FClosure(None,cf) ->
							process_overloads false None ((fun t -> t),cf,e1.etype)
						| _ ->
							[e1.etype,None,PMap.empty]
					end;
				| TConst TSuper ->
					find_constructor_types e1.etype
				| TLocal v ->
					[e1.etype,None,get_value_meta v.v_meta]
				| _ -> [e1.etype,None,PMap.empty]
			in
			handle_call tl el e1.epos
		| ENew(ptp,el) ->
			let t = Abstract.follow_with_forward_ctor (Typeload.load_instance ctx ptp ParamSpawnMonos LoadNormal) in
			handle_call (find_constructor_types t) el ptp.pos_full
		| EArray(e1,e2) ->
			let e1 = type_expr ctx e1 WithType.value in
			begin match follow e1.etype with
			| TInst({cl_path=([],"Array")},[t]) ->
				let res = convert_function_signature ctx PMap.empty (["index",false,ctx.t.tint],t) in
				raise_signatures [res,doc_from_string "The array index"] 0 0 SKCall
			| TAbstract(a,tl) ->
				(match a.a_impl with Some c -> ignore(c.cl_build()) | _ -> ());
				let sigs = ExtList.List.filter_map (fun cf -> match follow cf.cf_type with
					| TFun(_ :: args,r) ->
						if ExtType.is_void (follow r) && (match with_type with WithType.NoValue -> false | _ -> true) then
							None
						else begin
							let map = apply_params a.a_params tl in
							let tl = List.map (fun (n,o,t) -> n,o,map t) args in
							let r = map r in
							Some (convert_function_signature ctx PMap.empty (tl,r),cf.cf_doc)
						end
					| _ ->
						None
				) a.a_array in
				raise_signatures sigs 0 0 SKArrayAccess
			| _ ->
				raise_signatures [] 0 0 SKArrayAccess
			end
		| _ -> raise_typing_error "Call expected" p

and display_expr ctx e_ast e dk mode with_type p =
	let get_super_constructor () = match ctx.c.curclass.cl_super with
		| None -> raise_typing_error "Current class does not have a super" p
		| Some (c,params) ->
			let fa = get_constructor_access c params p in
			fa.fa_field,c
	in
	let maybe_expand_overload el_typed e e_on host cf = match mode with
		| MCall el when cf.cf_overloads <> [] ->
			let fa = FieldAccess.create e_on cf host false p in
			let fcc = unify_field_call ctx fa el_typed el p false in
			FieldAccess.get_field_expr {fa with fa_field = fcc.fc_field} FCall
		| _ ->
			e
	in
	let e,el_typed = match fst e_ast,e.eexpr with
		| _,TMeta((Meta.StaticExtension,[e_self],_),e1) ->
			e1,[type_stored_expr ctx e_self]
		| EField((_,_,EFSafe)),e1 ->
			(* For ?. we want to extract the then-expression of the TIf. *)
			let rec loop e1 = match e1.eexpr with
				| TIf({eexpr = TBinop(OpNotEq,_,{eexpr = TConst TNull})},e1,Some _) ->
					e1
				| TBlock el ->
					begin match List.rev el with
						| e :: _ -> loop e
						| _ -> e
					end
				| _ ->
					e
			in
			loop e,[]
		| _ ->
			e,[]
	in
	(* If we display on a TField node that points to an overloaded field, let's try to unify the field call
	   in order to resolve the correct overload (issue #7753). *)
	let e = match e.eexpr with
		| TField(e1,FStatic(c,cf)) -> maybe_expand_overload el_typed e e1 (FHStatic c) cf
		| TField(e1,(FInstance(c,tl,cf) | FClosure(Some(c,tl),cf))) -> maybe_expand_overload el_typed e e1 (FHInstance(c,tl)) cf
		| TField(e1,(FAnon cf | FClosure(None,cf))) -> maybe_expand_overload el_typed e e1 FHAnon cf
		| _ -> e
	in
	match ctx.com.display.dms_kind with
	| DMPackage ->
		die "" __LOC__
	| DMSignature ->
		handle_signature_display ctx e_ast with_type
	| DMHover ->
		let item = completion_item_of_expr ctx e in
		raise_hover item (Some with_type) e.epos
	| DMUsage _ | DMImplementation ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) ->
			Display.ReferencePosition.set (ef.ef_name,ef.ef_name_pos,SKEnumField ef);
		| TField(_,(FAnon cf | FClosure (None,cf))) ->
			Display.ReferencePosition.set (cf.cf_name,cf.cf_name_pos,SKField (cf,None));
		| TField(_,(FInstance (c,_,cf) | FStatic (c,cf) | FClosure (Some (c,_),cf))) ->
			Display.ReferencePosition.set (cf.cf_name,cf.cf_name_pos,SKField (cf,Some c));
		| TLocal v | TVar(v,_) ->
			Display.ReferencePosition.set (v.v_name,v.v_pos,SKVariable v);
		| TTypeExpr mt ->
			let ti = t_infos mt in
			Display.ReferencePosition.set (snd ti.mt_path,ti.mt_name_pos,symbol_of_module_type mt);
		| TNew(c,tl,_) ->
			begin try
				let fa = get_constructor_access c tl p in
				let cf = fa.fa_field in
				Display.ReferencePosition.set (snd c.cl_path,cf.cf_name_pos,SKConstructor cf);
			with Not_found ->
				()
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf,c = get_super_constructor() in
				Display.ReferencePosition.set (cf.cf_name,cf.cf_name_pos,SKField (cf,Some c));
			with Not_found ->
				()
			end
		| TConst TSuper ->
			begin match ctx.c.curclass.cl_super with
				| None -> ()
				| Some (c,_) -> Display.ReferencePosition.set (snd c.cl_path,c.cl_name_pos,SKClass c);
			end
		| TCall(e1,_) ->
			loop e1
		| TCast(e1,_) ->
			loop e1
		| _ ->
			()
		in
		loop e;
		e
	| DMDefinition ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) -> [ef.ef_name_pos]
		| TField(_,(FStatic (c,cf))) when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			cf.cf_name_pos :: (try [(PMap.find cf.cf_name c'.cl_statics).cf_name_pos] with Not_found -> [])
		| TField(_,(FInstance (c,tl,cf) | FClosure (Some(c,tl),cf))) when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			let l = try
				let _,_,cf = Type.class_field c' tl cf.cf_name in
				[cf.cf_name_pos]
			with Not_found ->
				[]
			in
			cf.cf_name_pos :: l
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) -> [cf.cf_name_pos]
		| TLocal v | TVar(v,_) -> [v.v_pos]
		| TTypeExpr (TClassDecl c) when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			[c.cl_name_pos;c'.cl_name_pos]
		| TTypeExpr mt -> [(t_infos mt).mt_name_pos]
		| TNew(c,tl,_) ->
			begin try
				let fa = get_constructor_access c tl p in
				let cf = fa.fa_field in
				if Meta.has Meta.CoreApi c.cl_meta then begin
					let c' = ctx.g.do_load_core_class ctx c in
					begin match c'.cl_constructor with
					| Some cf' -> [cf.cf_name_pos;cf'.cf_name_pos]
					| None -> [cf.cf_name_pos]
					end
				end else
					[cf.cf_name_pos]
			with Not_found ->
				[]
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf,_ = get_super_constructor() in
				[cf.cf_name_pos]
			with Not_found ->
				[]
			end
		| TConst TSuper ->
			begin match ctx.c.curclass.cl_super with
				| None -> []
				| Some (c,_) -> [c.cl_name_pos]
			end
		| TCall(e1,_) ->
			loop e1
		| TCast(e1,_) ->
			loop e1
		| _ ->
			[]
		in
		let pl = loop e in
		raise_positions pl
	| DMTypeDefinition ->
		raise_position_of_type ctx e.etype
	| DMDefault when not (!Parser.had_resume)->
		let display_fields e_ast e1 so =
			let l = match so with None -> 0 | Some s -> String.length s in
			let fields = DisplayFields.collect ctx e_ast e1 dk with_type p in
			let item = completion_item_of_expr ctx e1 in
			raise_fields fields (CRField(item,e1.epos,None,None)) (make_subject so ~start_pos:(Some (pos e_ast)) {e.epos with pmin = e.epos.pmax - l;})
		in
		begin match fst e_ast,e.eexpr with
			| EField(e1,s,_),TField(e2,_) ->
				display_fields e1 e2 (Some s)
			| EObjectDecl [(name,pn,_),(EConst (Ident "null"),pe)],_ when pe.pmin = -1 ->
				(* This is what the parser emits for #8651. Bit of a dodgy heuristic but should be fine. *)
				raise_toplevel ctx dk with_type (name,pn)
			| _ ->
				if dk = DKDot then display_fields e_ast e None
				else begin
					let name = try String.concat "." (string_list_of_expr_path_raise e_ast) with Exit -> "" in
					let name = if name = "null" then "" else name in
					let p = pos e_ast in
					let p = if name <> "" then p else (DisplayPosition.display_position#with_pos p) in
					raise_toplevel ctx dk with_type (name,p)
				end
		end
	| DMDefault | DMNone | DMModuleSymbols _ ->
		let fields = DisplayFields.collect ctx e_ast e dk with_type p in
		let item = completion_item_of_expr ctx e in
		let iterator = try
			let it = (ForLoop.IterationKind.of_texpr ~resume:true ctx e None e.epos) in
			match follow it.it_type with
				| TDynamic _ ->  None
				| t -> Some t
			with Error _ | Not_found ->
				None
		in
		let keyValueIterator =
			try begin
				let _,pt = ForLoop.IterationKind.check_iterator ~resume:true ctx "keyValueIterator" e e.epos in
				match follow pt with
					| TAnon a ->
						let key = PMap.find "key" a.a_fields in
						let value = PMap.find "value" a.a_fields in
						Some (key.cf_type,value.cf_type)
					| _ ->
						None
			end with Error _ | Not_found ->
				None
		in
		raise_fields fields (CRField(item,e.epos,iterator,keyValueIterator)) (make_subject None (DisplayPosition.display_position#with_pos p))

let handle_display ctx e_ast dk mode with_type =
	let old = ctx.f.in_display,ctx.f.in_call_args in
	ctx.f.in_display <- true;
	ctx.f.in_call_args <- false;
	let tpair t =
		let ct = CompletionType.from_type (get_import_status ctx) t in
		(t,ct)
	in
	let e = match e_ast,with_type with
	| (EConst (Ident "$type"),p),_ ->
		display_dollar_type ctx p tpair
	| (EConst (Ident "trace"),_),_ ->
		let doc = doc_from_string "Print given arguments" in
		let arg = ["value",false,t_dynamic] in
		let ret = ctx.com.basic.tvoid in
		let p = pos e_ast in
		begin match ctx.com.display.dms_kind with
		| DMSignature ->
			raise_signatures [(convert_function_signature ctx PMap.empty (arg,ret),doc)] 0 0 SKCall
		| DMHover ->
			let t = TFun(arg,ret) in
			raise_hover (make_ci_expr (mk (TIdent "trace") t p) (tpair t)) (Some (WithType.named_argument "value")) p
		| DMDefinition | DMTypeDefinition ->
			raise_positions []
		| _ ->
			raise_typing_error "Unsupported method" p
		end
	| (EConst (Ident "_"),p),WithType.WithType(t,_) ->
		mk (TConst TNull) t p (* This is "probably" a bind skip, let's just use the expected type *)
	| (_,p),_ -> try
		type_expr ~mode ctx e_ast with_type
	with Error { err_message = Unknown_ident n } when ctx.com.display.dms_kind = DMDefault ->
        if dk = DKDot && is_legacy_completion ctx.com then raise (Parser.TypePath ([n],None,false,p))
		else raise_toplevel ctx dk with_type (n,p)
	| Error ({ err_message = Type_not_found (path,_,_) | Module_not_found path } as err) when ctx.com.display.dms_kind = DMDefault ->
		if is_legacy_completion ctx.com then begin try
			raise_fields (DisplayFields.get_submodule_fields ctx path) (CRField((make_ci_module path),p,None,None)) (make_subject None (pos e_ast))
		with Not_found ->
			raise_error err
		end else
			raise_toplevel ctx dk with_type (s_type_path path,p)
	| DisplayException(DisplayFields ({fkind = CRTypeHint} as r)) when (match fst e_ast with ENew _ -> true | _ -> false) ->
		let timer = Timer.timer ["display";"toplevel";"filter ctors"] in
		let l = List.filter (fun item ->
			let is_private_to_current_module mt =
				(* Remove the _Module nonsense from the package *)
				let pack = List.rev (List.tl (List.rev mt.pack)) in
				(pack,mt.module_name) = ctx.m.curmod.m_path
			in
			match item.ci_kind with
			| ITType({kind = (Class | Abstract | TypeAlias)} as mt,_) when not mt.is_private || is_private_to_current_module mt ->
				begin match mt.has_constructor with
				| Yes -> true
				| YesButPrivate ->
					if (Meta.has Meta.PrivateAccess ctx.f.meta) then true
					else
						begin
							match ctx.c.curclass.cl_kind with
							| KAbstractImpl { a_path = (pack, name) } -> pack = mt.pack && name = mt.name
							| _ -> false
						end
						|| begin
							let path = (mt.pack,mt.name) in
							let rec loop c =
								if c.cl_path = path then true
								else match c.cl_super with
									| Some(c,_) -> loop c
									| None -> false
							in
							loop ctx.c.curclass
						end
				| No -> false
				| Maybe ->
					begin try
						let mt = ctx.g.do_load_type_def ctx null_pos {tpackage=mt.pack;tname=mt.module_name;tsub=Some mt.name;tparams=[]} in
						begin match resolve_typedef mt with
						| TClassDecl c -> has_constructor c
						| TAbstractDecl a -> (match Abstract.follow_with_forward_ctor ~build:true (TAbstract(a,extract_param_types a.a_params)) with
							| TInst(c,_) -> has_constructor c
							| TAbstract({a_impl = Some c},_) -> PMap.mem "_new" c.cl_statics
							| _ -> false)
						| _ -> false
						end
					with _ ->
						false
					end
				end
			| ITTypeParameter {cl_kind = KTypeParameter ttp} when get_constructible_constraint ctx (get_constraints ttp) null_pos <> None ->
				true
			| _ -> false
		) r.fitems in
		timer();
		raise_fields l CRNew r.fsubject
	in
	let e = match e_ast, e.eexpr with
		| _, TField(e1,FDynamic "bind") when (match follow e1.etype with TFun _ -> true | _ -> false) -> e1
		| (EField(_,"new",_),_), TFunction { tf_expr = { eexpr = TReturn (Some ({ eexpr = TNew _ } as e1))} } -> e1
		| _ -> e
	in
	let is_display_debug = Meta.has (Meta.Custom ":debug.display") ctx.f.curfield.cf_meta in
	if is_display_debug then begin
		print_endline (Printf.sprintf "expected type: %s" (WithType.to_string with_type));
		print_endline (Printf.sprintf "typed expr:\n%s" (s_expr_ast true "" (s_type (print_context())) e));
	end;
	let p = e.epos in
	begin match with_type with
		| WithType.WithType(t,_) ->
			(* We don't want to actually use the transformed expression which may have inserted implicit cast calls.
			   It only matters that unification takes place. *)
			(try ignore(AbstractCast.cast_or_unify_raise ctx t e e.epos) with Error { err_message = Unify _ } -> ());
		| _ ->
			()
	end;
	if is_display_debug then begin
		print_endline (Printf.sprintf "cast expr:\n%s" (s_expr_ast true "" (s_type (print_context())) e));
	end;
	ctx.f.in_display <- fst old;
	ctx.f.in_call_args <- snd old;
	let f () = display_expr ctx e_ast e dk mode with_type p in
	if ctx.f.in_overload_call_args then begin
		try
			f()
		with DisplayException de ->
			ctx.g.delayed_display <- Some de;
			e
	end else
		f()

let handle_structure_display ctx e fields origin =
	let p = pos e in
	let fields = PMap.foldi (fun _ cf acc -> cf :: acc) fields [] in
	let fields = List.sort (fun cf1 cf2 -> -compare cf1.cf_pos.pmin cf2.cf_pos.pmin) fields in
	let tpair ?(values=PMap.empty) t =
		let ct = CompletionType.from_type (get_import_status ctx) ~values t in
		(t,ct)
	in
	let make_field_item cf =
		make_ci_class_field (CompletionClassField.make cf CFSMember origin true) (tpair ~values:(get_value_meta cf.cf_meta) cf.cf_type)
	in
	match fst e with
	| EObjectDecl fl ->
		let fields = ref fields in
		let rec loop subj fl = match fl with
			| [] -> subj
			| ((n,p,_),e) :: fl ->
				let wt () =
					try
						let cf = List.find (fun { cf_name = name } -> name = n) !fields in
						WithType.with_type cf.cf_type
					with Not_found -> WithType.value
				in
				let subj = if DisplayPosition.display_position#enclosed_in p then
					Some(n,p)
				else begin
					if DisplayPosition.display_position#enclosed_in (pos e) then
						ignore(handle_display ctx e DKMarked MGet (wt()))
					else begin
						(* If we are between the : and the expression, we don't want to use the actual expression as a filter string (issue #10414) *)
						let p_between = { p with pmin = p.pmax + 1; pmax = (pos e).pmin - 1} in
						if DisplayPosition.display_position#enclosed_in p_between then begin
							let e = (EConst(Ident "null"),p_between) in
							ignore(handle_display ctx e DKMarked MGet (wt()))
						end;
					end;
					fields := List.filter (fun cf -> cf.cf_name <> n) !fields;
					subj
				end in
				loop subj fl
		in
		let subj = loop None fl in
		let name,pinsert = match subj with
			| None -> None,DisplayPosition.display_position#with_pos (pos e)
			| Some(name,p) -> Some name,p
		in
		let fields = List.map make_field_item !fields in
		raise_fields fields CRStructureField (make_subject name pinsert)
	| EBlock [] ->
		let fields = List.fold_left (fun acc cf ->
			(make_field_item cf) :: acc
		) [] fields in
		let pinsert = DisplayPosition.display_position#with_pos (pos e) in
		raise_fields fields CRStructureField (make_subject None pinsert)
	| _ ->
		raise_typing_error "Expected object expression" p

let handle_edisplay ctx e dk mode with_type =
	let handle_display ctx e dk with_type =
		handle_display ctx e dk mode with_type
	in
	match dk,ctx.com.display.dms_kind with
	| DKCall,(DMSignature | DMDefault) -> handle_signature_display ctx e with_type
	| DKStructure,DMDefault ->
		begin match with_type with
			| WithType.WithType(t,_) ->
				begin match follow t with
					| TAnon an ->
						let origin = match t with
							| TType(td,_) -> Self (TTypeDecl td)
							| _ -> AnonymousStructure an
						in
						handle_structure_display ctx e an.a_fields origin
					| TInst(c,tl) when Meta.has Meta.StructInit c.cl_meta ->
						let fields = get_struct_init_anon_fields c tl in
						handle_structure_display ctx e fields (Self (TClassDecl c))
					| _ -> handle_display ctx e dk with_type
				end
			| _ ->
				handle_display ctx e dk with_type
		end
	| DKPattern outermost,DMDefault ->
		begin try
			handle_display ctx e dk with_type
		with DisplayException(DisplayFields ({fkind = CRToplevel _} as r)) ->
			raise_fields r.fitems (CRPattern ((get_expected_type ctx with_type),outermost)) r.fsubject
		end
	| _ -> handle_display ctx e dk with_type
