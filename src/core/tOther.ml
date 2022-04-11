open Globals
open Ast
open TType
open TFunctions
open TPrinting

module TExprToExpr = struct
	let tpath p mp pl =
		if snd mp = snd p then
			CTPath (mk_type_path ~params:pl p)
		else
			CTPath (mk_type_path ~params:pl ~sub:(snd p) mp)

	let rec convert_type = function
		| TMono r ->
			(match r.tm_type with
			| None -> raise Exit
			| Some t -> convert_type t)
		| TInst ({cl_private = true; cl_path=_,name},tl)
		| TEnum ({e_private = true; e_path=_,name},tl)
		| TType ({t_private = true; t_path=_,name},tl)
		| TAbstract ({a_private = true; a_path=_,name},tl) ->
			CTPath (mk_type_path ~params:(List.map tparam tl) ([],name))
		| TEnum (e,pl) ->
			tpath e.e_path e.e_module.m_path (List.map tparam pl)
		| TInst({cl_kind = KExpr e} as c,pl) ->
			tpath ([],snd c.cl_path) ([],snd c.cl_path) (List.map tparam pl)
		| TInst({cl_kind = KTypeParameter _} as c,pl) ->
			tpath ([],snd c.cl_path) ([],snd c.cl_path) (List.map tparam pl)
		| TInst (c,pl) ->
			tpath c.cl_path c.cl_module.m_path (List.map tparam pl)
		| TType (t,pl) as tf ->
			(* recurse on type-type *)
			if (snd t.t_path).[0] = '#' then convert_type (follow tf) else tpath t.t_path t.t_module.m_path (List.map tparam pl)
		| TAbstract (a,pl) ->
			tpath a.a_path a.a_module.m_path (List.map tparam pl)
		| TFun (args,ret) ->
			CTFunction (List.map (fun (_,_,t) -> convert_type' t) args, (convert_type' ret))
		| TAnon a ->
			begin match !(a.a_status) with
			| Statics c -> tpath ([],"Class") ([],"Class") [TPType (tpath c.cl_path c.cl_path [],null_pos)]
			| EnumStatics e -> tpath ([],"Enum") ([],"Enum") [TPType (tpath e.e_path e.e_path [],null_pos)]
			| _ ->
				CTAnonymous (PMap.foldi (fun _ f acc ->
					{
						cff_name = f.cf_name,null_pos;
						cff_kind = FVar (mk_type_hint f.cf_type null_pos,None);
						cff_pos = f.cf_pos;
						cff_doc = f.cf_doc;
						cff_meta = f.cf_meta;
						cff_access = [];
					} :: acc
				) a.a_fields [])
			end
		| (TDynamic t2) as t ->
			tpath ([],"Dynamic") ([],"Dynamic") (if t == t_dynamic then [] else [tparam t2])
		| TLazy f ->
			convert_type (lazy_type f)

	and convert_type' t =
		convert_type t,null_pos

	and tparam = function
		| TInst ({cl_kind = KExpr e}, _) -> TPExpr e
		| t -> TPType (convert_type' t)

	and mk_type_hint t p =
		match follow t with
		| TMono _ -> None
		| _ -> (try Some (convert_type t,p) with Exit -> None)

	let rec convert_expr e =
		let full_type_path t =
			let mp,p = match t with
			| TClassDecl c -> c.cl_module.m_path,c.cl_path
			| TEnumDecl en -> en.e_module.m_path,en.e_path
			| TAbstractDecl a -> a.a_module.m_path,a.a_path
			| TTypeDecl t -> t.t_module.m_path,t.t_path
			in
			if snd mp = snd p then p else (fst mp) @ [snd mp],snd p
		in
		let mk_path = expr_of_type_path in
		let mk_ident = function
			| "`trace" -> Ident "trace"
			| n -> Ident n
		in
		let eopt = function None -> None | Some e -> Some (convert_expr e) in
		((match e.eexpr with
		| TConst c ->
			EConst (tconst_to_const c)
		| TLocal v -> EConst (mk_ident v.v_name)
		| TArray (e1,e2) -> EArray (convert_expr e1,convert_expr e2)
		| TBinop (op,e1,e2) -> EBinop (op, convert_expr e1, convert_expr e2)
		| TField (e,f) -> EField (convert_expr e, field_name f, EFNormal)
		| TTypeExpr t -> fst (mk_path (full_type_path t) e.epos)
		| TParenthesis e -> EParenthesis (convert_expr e)
		| TObjectDecl fl -> EObjectDecl (List.map (fun (k,e) -> k, convert_expr e) fl)
		| TArrayDecl el -> EArrayDecl (List.map convert_expr el)
		| TCall (e,el) -> ECall (convert_expr e,List.map convert_expr el)
		| TNew (c,pl,el) -> ENew ((match (try convert_type (TInst (c,pl)) with Exit -> convert_type (TInst (c,[]))) with CTPath p -> p,null_pos | _ -> die "" __LOC__),List.map convert_expr el)
		| TUnop (op,p,e) -> EUnop (op,p,convert_expr e)
		| TFunction f ->
			let arg (v,c) = (v.v_name,v.v_pos), false, v.v_meta, mk_type_hint v.v_type null_pos, (match c with None -> None | Some c -> Some (convert_expr c)) in
			EFunction (FKAnonymous,{ f_params = []; f_args = List.map arg f.tf_args; f_type = mk_type_hint f.tf_type null_pos; f_expr = Some (convert_expr f.tf_expr) })
		| TVar (v,eo) ->
			let final = has_var_flag v VFinal
			and t = mk_type_hint v.v_type v.v_pos
			and eo = eopt eo in
			EVars ([mk_evar ~final ?t ?eo ~meta:v.v_meta (v.v_name,v.v_pos)])
		| TBlock el -> EBlock (List.map convert_expr el)
		| TFor (v,it,e) ->
			let ein = (EBinop (OpIn,(EConst (Ident v.v_name),it.epos),convert_expr it),it.epos) in
			EFor (ein,convert_expr e)
		| TIf (e,e1,e2) -> EIf (convert_expr e,convert_expr e1,eopt e2)
		| TWhile (e1,e2,flag) -> EWhile (convert_expr e1, convert_expr e2, flag)
		| TSwitch (e,cases,def) ->
			let cases = List.map (fun (vl,e) ->
				List.map convert_expr vl,None,(match e.eexpr with TBlock [] -> None | _ -> Some (convert_expr e)),e.epos
			) cases in
			let def = match eopt def with None -> None | Some (EBlock [],_) -> Some (None,null_pos) | Some e -> Some (Some e,pos e) in
			ESwitch (convert_expr e,cases,def)
		| TEnumIndex _
		| TEnumParameter _ ->
			(* these are considered complex, so the AST is handled in TMeta(Meta.Ast) *)
			die "" __LOC__
		| TTry (e,catches) ->
			let e1 = convert_expr e in
			let catches = List.map (fun (v,e) ->
				let ct = try convert_type v.v_type,null_pos with Exit -> die "" __LOC__ in
				let e = convert_expr e in
				(v.v_name,v.v_pos),(Some ct),e,(pos e)
			) catches in
			ETry (e1,catches)
		| TReturn e -> EReturn (eopt e)
		| TBreak -> EBreak
		| TContinue -> EContinue
		| TThrow e -> EThrow (convert_expr e)
		| TCast (e,t) ->
			let t = (match t with
				| None -> None
				| Some t ->
					let t = (match t with TClassDecl c -> TInst (c,[]) | TEnumDecl e -> TEnum (e,[]) | TTypeDecl t -> TType (t,[]) | TAbstractDecl a -> TAbstract (a,[])) in
					Some (try convert_type t,null_pos with Exit -> die "" __LOC__)
			) in
			ECast (convert_expr e,t)
		| TMeta ((Meta.Ast,[e1,_],_),_) -> e1
		| TMeta (m,e) -> EMeta(m,convert_expr e)
		| TIdent s -> EConst (Ident s))
		,e.epos)

end

module ExtType = struct
	let is_mono = function
		| TMono { tm_type = None } -> true
		| _ -> false

	let is_void = function
		| TAbstract({a_path=[],"Void"},_) -> true
		| _ -> false

	let is_int t = match t with
		| TAbstract({a_path=[],"Int"},_) -> true
		| _ -> false

	let is_float t = match t with
		| TAbstract({a_path=[],"Float"},_) -> true
		| _ -> false

	let is_numeric t = match t with
		| TAbstract({a_path=[],"Float"},_) -> true
		| TAbstract({a_path=[],"Int"},_) -> true
		| _ -> false

	let is_string t = match t with
		| TInst({cl_path=[],"String"},_) -> true
		| _ -> false

	let is_bool t = match t with
		| TAbstract({a_path=[],"Bool"},_) -> true
		| _ -> false

	let is_rest t = match t with
		| TType({t_path=["haxe"; "extern"],"Rest"},_)
		| TAbstract({a_path=["haxe"],"Rest"},_) -> true
		| _ -> false

	let is_type_param t =
		match t with
		| TInst({ cl_kind = KTypeParameter _ }, _) -> true
		| _ -> false

	type semantics =
		| VariableSemantics
		| ReferenceSemantics
		| ValueSemantics

	let semantics_name = function
		| VariableSemantics -> "variable"
		| ReferenceSemantics -> "reference"
		| ValueSemantics -> "value"

	let has_semantics t sem =
		let name = semantics_name sem in
		let check meta =
			has_meta_option meta Meta.Semantics name
		in
		let rec loop t = match t with
			| TInst(c,_) -> check c.cl_meta
			| TEnum(en,_) -> check en.e_meta
			| TType(t,tl) -> check t.t_meta || (loop (apply_typedef t tl))
			| TAbstract(a,_) -> check a.a_meta
			| TLazy f -> loop (lazy_type f)
			| TMono r ->
				(match r.tm_type with
				| Some t -> loop t
				| _ -> false)
			| _ ->
				false
		in
		loop t

	let has_variable_semantics t = has_semantics t VariableSemantics
	let has_reference_semantics t = has_semantics t ReferenceSemantics
	let has_value_semantics t = has_semantics t ValueSemantics
end

let no_meta = []

let class_module_type c =
	let path = ([],"Class<" ^ (s_type_path c.cl_path) ^ ">") in
	let t = mk_anon ~fields:c.cl_statics (ref (Statics c)) in
	{ (mk_typedef c.cl_module path c.cl_pos null_pos t) with t_private = true}

let enum_module_type m path p  =
	let path = ([], "Enum<" ^ (s_type_path path) ^ ">") in
	let t = mk_mono() in
	{(mk_typedef m path p null_pos t) with t_private = true}

let abstract_module_type a tl =
	let path = ([],Printf.sprintf "Abstract<%s%s>" (s_type_path a.a_path) (s_type_params (ref []) tl)) in
	let t = mk_anon (ref (AbstractStatics a)) in
	{(mk_typedef a.a_module path a.a_pos null_pos t) with t_private = true}

module TClass = struct
	let get_member_fields' self_too c0 tl =
		let rec loop acc c tl =
			let apply = apply_params c.cl_params tl in
			let maybe_add acc cf =
				if not (PMap.mem cf.cf_name acc) then begin
					let cf = if tl = [] then cf else {cf with cf_type = apply cf.cf_type} in
					PMap.add cf.cf_name (c,cf) acc
				end else acc
			in
			let acc = if self_too || c != c0 then List.fold_left maybe_add acc c.cl_ordered_fields else acc in
			if (has_class_flag c CInterface) then
				List.fold_left (fun acc (i,tl) -> loop acc i (List.map apply tl)) acc c.cl_implements
			else
				match c.cl_super with
				| Some(c,tl) -> loop acc c (List.map apply tl)
				| None -> acc
		in
		loop PMap.empty c0 tl

	let get_all_super_fields c =
		get_member_fields' false c (extract_param_types c.cl_params)

	let get_all_fields c tl =
		get_member_fields' true c tl

	let get_overridden_fields c cf =
		let rec loop acc c = match c.cl_super with
			| None ->
				acc
			| Some(c,_) ->
				begin try
					let cf' = PMap.find cf.cf_name c.cl_fields in
					loop (cf' :: acc) c
				with Not_found ->
					loop acc c
				end
		in
		loop [] c

	let add_field c cf =
		let is_static = has_class_field_flag cf CfStatic in
		if is_static then begin
			c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
			c.cl_ordered_statics <- cf :: c.cl_ordered_statics;
		end else begin
			c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
			c.cl_ordered_fields <- cf :: c.cl_ordered_fields;
		end

	let get_map_function c tl =
		let rec loop map c = match c.cl_super with
			| Some(csup,tl) ->
				let map t = map (apply_params csup.cl_params tl t) in
				loop map csup
			| None ->
				map
		in
		let apply = apply_params c.cl_params tl in
		loop apply c
end

let s_class_path c =
	let path = match c.cl_kind with
		| KAbstractImpl a -> a.a_path
		| _ -> c.cl_path
	in
	s_type_path path