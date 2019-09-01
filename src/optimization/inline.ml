open Globals
open Ast
open Type
open OptimizerTexpr
open Common
open Typecore
open Error

let mk_untyped_call name p params =
	{
		eexpr = TCall({ eexpr = TIdent name; etype = t_dynamic; epos = p }, params);
		etype = t_dynamic;
		epos = p;
	}

let api_inline2 com c field params p =
	match c.cl_path, field, params with
	| ([],"Type"),"enumIndex",[{ eexpr = TField (_,FEnum (en,f)) }] -> (match com.platform with
		| Cs when en.e_extern && not (Meta.has Meta.HxGen en.e_meta) ->
			(* We don't want to optimize enums from external sources; as they might change unexpectedly *)
			(* and since native C# enums don't have the concept of index - they have rather a value, *)
			(* which can't be mapped to a native API - this kind of substitution is dangerous *)
			None
		| _ ->
			Some (mk (TConst (TInt (Int32.of_int f.ef_index))) com.basic.tint p))
	| ([],"Type"),"enumIndex",[{ eexpr = TCall({ eexpr = TField (_,FEnum (en,f)) },pl) }] when List.for_all (fun e -> not (has_side_effect e)) pl ->
		(match com.platform with
			| Cs when en.e_extern && not (Meta.has Meta.HxGen en.e_meta) ->
				(* see comment above *)
				None
			| _ ->
				Some (mk (TConst (TInt (Int32.of_int f.ef_index))) com.basic.tint p))
	| ([],"Std"),"int",[{ eexpr = TConst (TInt _) } as e] ->
		Some { e with epos = p }
	| ([],"String"),"fromCharCode",[{ eexpr = TConst (TInt i) }] when i > 0l && i < 128l ->
		Some (mk (TConst (TString (String.make 1 (char_of_int (Int32.to_int i))))) com.basic.tstring p)
	| ([],"Std"),"string",[{ eexpr = TCast ({ eexpr = TConst c } as e, None)}]
	| ([],"Std"),"string",[{ eexpr = TConst c } as e] ->
		(match c with
		| TString s ->
			Some { e with epos = p }
		| TInt i ->
			Some { eexpr = TConst (TString (Int32.to_string i)); epos = p; etype = com.basic.tstring }
		| TBool b ->
			Some { eexpr = TConst (TString (if b then "true" else "false")); epos = p; etype = com.basic.tstring }
		| _ ->
			None)
	| ([],"Std"),"string",[{ eexpr = TIf (_,{ eexpr = TConst (TString _)},Some { eexpr = TConst (TString _) }) } as e] ->
		Some e
	| ([],"Std"),"string",[{ eexpr = TLocal v | TField({ eexpr = TLocal v },_) } as ev] when (com.platform = Js || com.platform = Flash) && (match v.v_kind with VUser _ -> true | _ -> false) ->
		let pos = ev.epos in
		let stringv() =
			let to_str = mk (TBinop (Ast.OpAdd, mk (TConst (TString "")) com.basic.tstring pos, ev)) com.basic.tstring pos in
			if com.platform = Js || is_nullable ev.etype then
				let chk_null = mk (TBinop (Ast.OpEq, ev, mk (TConst TNull) t_dynamic pos)) com.basic.tbool pos in
				mk (TIf (chk_null, mk (TConst (TString "null")) com.basic.tstring pos, Some to_str)) com.basic.tstring pos
			else
				to_str
		in
		(match follow ev.etype with
		| TInst ({ cl_path = [],"String" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"Float" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"Int" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"UInt" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"Bool" }, []) ->
			Some (stringv())
		| _ ->
			None)
	| ([],"Std"),"int",[{ eexpr = TConst (TFloat f) }] ->
		let f = float_of_string f in
		(match classify_float f with
		| FP_infinite | FP_nan ->
			None
		| _ when f <= Int32.to_float Int32.min_int -. 1. || f >= Int32.to_float Int32.max_int +. 1. ->
			None (* out range, keep platform-specific behavior *)
		| _ ->
			Some { eexpr = TConst (TInt (Int32.of_float f)); etype = com.basic.tint; epos = p })
	| ([],"Math"),"ceil",[{ eexpr = TConst (TFloat f) }] ->
		let f = float_of_string f in
		(match classify_float f with
		| FP_infinite | FP_nan ->
			None
		| _ when f <= Int32.to_float Int32.min_int -. 1. || f >= Int32.to_float Int32.max_int ->
			None (* out range, keep platform-specific behavior *)
		| _ ->
			Some { eexpr = TConst (TInt (Int32.of_float (ceil f))); etype = com.basic.tint; epos = p })
	| ([],"Math"),"floor",[{ eexpr = TConst (TFloat f) }] ->
		let f = float_of_string f in
		(match classify_float f with
		| FP_infinite | FP_nan ->
			None
		| _ when f <= Int32.to_float Int32.min_int || f >= Int32.to_float Int32.max_int +. 1. ->
			None (* out range, keep platform-specific behavior *)
		| _ ->
			Some { eexpr = TConst (TInt (Int32.of_float (floor f))); etype = com.basic.tint; epos = p })
	| (["cs"],"Lib"),("fixed" | "checked" | "unsafe"),[e] ->
			Some (mk_untyped_call ("__" ^ field ^ "__") p [e])
	| (["cs"],"Lib"),("lock"),[obj;block] ->
			Some (mk_untyped_call ("__lock__") p [obj;mk_block block])
	| (["java"],"Lib"),("lock"),[obj;block] ->
			Some (mk_untyped_call ("__lock__") p [obj;mk_block block])
	| _ ->
		None

let api_inline ctx c field params p =
	let mk_typeexpr path =
		let m = (try Hashtbl.find ctx.g.modules path with Not_found -> assert false) in
		add_dependency ctx.m.curmod m;
		ExtList.List.find_map (function
			| TClassDecl cl when cl.cl_path = path -> Some (make_static_this cl p)
			| _ -> None
		) m.m_types
	in

	let eJsSyntax () = mk_typeexpr (["js"],"Syntax") in
	let eJsBoot () = mk_typeexpr (["js"],"Boot") in

	let tstring = ctx.com.basic.tstring in
	let tbool = ctx.com.basic.tbool in
	let tint = ctx.com.basic.tint in

	match c.cl_path, field, params with
	| ([],"Std"),"is",[o;t] | (["js"],"Boot"),"__instanceof",[o;t] when ctx.com.platform = Js ->
		let is_trivial e =
			match e.eexpr with
			| TConst _ | TLocal _ -> true
			| _ -> false
		in

		let typeof t =
			let tof = Texpr.Builder.fcall (eJsSyntax()) "typeof" [o] tstring p in
			mk (TBinop (Ast.OpEq, tof, (mk (TConst (TString t)) tstring p))) tbool p
		in

		(match t.eexpr with
		(* generate simple typeof checks for basic types *)
		| TTypeExpr (TClassDecl ({ cl_path = [],"String" })) -> Some (typeof "string")
		| TTypeExpr (TAbstractDecl ({ a_path = [],"Bool" })) -> Some (typeof "boolean")
		| TTypeExpr (TAbstractDecl ({ a_path = [],"Float" })) -> Some (typeof "number")
		| TTypeExpr (TAbstractDecl ({ a_path = [],"Int" })) when is_trivial o ->
			(* generate typeof(o) == "number" && (o|0) === o check *)
			let lhs = mk (TBinop (Ast.OpOr, o, mk (TConst (TInt Int32.zero)) tint p)) tint p in
			let jscheck = Texpr.Builder.fcall (eJsSyntax()) "strictEq" [lhs;o] tbool p in
			Some(mk (TBinop (Ast.OpBoolAnd, typeof "number", jscheck)) tbool p)
		| TTypeExpr (TClassDecl ({ cl_path = [],"Array" })) ->
			(* generate (o instanceof Array) && o.__enum__ == null check *)
			let iof = Texpr.Builder.fcall (eJsSyntax()) "instanceof" [o;t] tbool p in
			let enum = mk (TField (o, FDynamic "__enum__")) (mk_mono()) p in
			let null = mk (TConst TNull) (mk_mono()) p in
			let not_enum = mk (TBinop (Ast.OpEq, enum, null)) tbool p in
			Some (mk (TBinop (Ast.OpBoolAnd, iof, not_enum)) tbool p)
		| TTypeExpr (TClassDecl cls) ->
			if cls.cl_interface then
				Some (Texpr.Builder.fcall (eJsBoot()) "__implements" [o;t] tbool p)
			else
				Some (Texpr.Builder.fcall (eJsSyntax()) "instanceof" [o;t] tbool p)
		| _ ->
			None)
	| (["js"],"Boot"),"__downcastCheck",[o; {eexpr = TTypeExpr (TClassDecl cls) } as t] when ctx.com.platform = Js ->
		if cls.cl_interface then
			Some (Texpr.Builder.fcall (make_static_this c p) "__implements" [o;t] tbool p)
		else
			Some (Texpr.Builder.fcall (eJsSyntax()) "instanceof" [o;t] tbool p)
	| (["cs" | "java"],"Lib"),("nativeArray"),[{ eexpr = TArrayDecl args } as edecl; _]
	| (["haxe";"ds";"_Vector"],"Vector_Impl_"),("fromArrayCopy"),[{ eexpr = TArrayDecl args } as edecl] -> (try
			let platf = match ctx.com.platform with
				| Cs -> "cs"
				| Java -> "java"
				| _ -> raise Exit
			in
			let mpath = if field = "fromArrayCopy" then
				(["haxe";"ds"],"Vector")
			else
				([platf],"NativeArray")
			in

			let m = ctx.g.do_load_module ctx mpath null_pos in
			let main = List.find (function | TClassDecl _ | TAbstractDecl _ -> true | _ -> false) m.m_types in
			let t = match follow edecl.etype, main with
				| TInst({ cl_path = [],"Array" }, [t]), TClassDecl(cl) ->
					TInst(cl,[t])
				| TInst({ cl_path = [],"Array" }, [t]), TAbstractDecl(a) ->
					TAbstract(a,[t])
				| _ -> assert false
			in
			Some ({ (mk_untyped_call "__array__" p args) with etype = t })
		with | Exit ->
			None)
	| _ ->
		api_inline2 ctx.com c field params p

type in_local = {
	i_var : tvar;
	i_subst : tvar;
	i_outside : bool;
	i_abstract_this : bool;
	mutable i_captured : bool;
	mutable i_write : bool;
	mutable i_read : int;
	mutable i_called : int;
	mutable i_force_temp : bool;
	mutable i_default_value : texpr option;
}

type var_inline_kind =
	| VIInline
	| VIInlineIfCalled
	| VIDoNotInline

let inline_default_config cf t =
	(* type substitution on both class and function type parameters *)
	let rec get_params c pl =
		match c.cl_super with
		| None -> c.cl_params, pl
		| Some (csup,spl) ->
			let spl = (match apply_params c.cl_params pl (TInst (csup,spl)) with
			| TInst (_,pl) -> pl
			| _ -> assert false
			) in
			let ct, cpl = get_params csup spl in
			c.cl_params @ ct, pl @ cpl
	in
	let rec loop t = match follow t with
		| TInst({cl_kind = KTypeParameter tl},_) -> List.fold_left (fun (params',tl') (params,tl) -> (params @ params',tl @ tl')) ([],[]) (List.map loop tl)
		| TInst (c,pl) -> get_params c pl
		| _ -> ([],[])
	in
	let tparams = loop t in
	let pmonos = List.map (fun _ -> mk_mono()) cf.cf_params in
	let tmonos = snd tparams @ pmonos in
	let tparams = fst tparams @ cf.cf_params in
	tparams <> [], apply_params tparams tmonos

let inline_metadata e meta =
	let inline_meta e meta = match meta with
		| (Meta.Deprecated | Meta.Pure),_,_ -> mk (TMeta(meta,e)) e.etype e.epos
		| _ -> e
	in
	List.fold_left inline_meta e meta

class inline_state ctx ethis params cf f p = object(self)
	val locals = Hashtbl.create 0
	val checker = create_affection_checker()
	val mutable _inlined_vars = []
	val mutable _in_local_fun = false
	val mutable _had_side_effect = false
	val mutable _has_return_value = false

	method enter_local_fun =
		let old = _in_local_fun in
		_in_local_fun <- true;
		(fun () -> _in_local_fun <- old)

	method in_local_fun = _in_local_fun

	method inlined_vars = _inlined_vars

	method had_side_effect = _had_side_effect
	method set_side_effect = _had_side_effect <- true

	method has_return_value = _has_return_value
	method set_return_value = _has_return_value <- true

	method private collect_modified_locals e = (snd checker) e
	method might_be_affected e = (fst checker) e

	method declare v =
		try
			Hashtbl.find locals v.v_id
		with Not_found ->
			let v' = alloc_var (match v.v_kind with VUser _ -> VInlined | k -> k) v.v_name v.v_type v.v_pos in
			v'.v_extra <- v.v_extra;
			let i = {
				i_var = v;
				i_subst = v';
				i_outside = false;
				i_abstract_this = Meta.has Meta.This v.v_meta;
				i_captured = false;
				i_write = false;
				i_called = 0;
				i_force_temp = false;
				i_read = 0;
				i_default_value = None;
			} in
			i.i_subst.v_meta <- List.filter (fun (m,_,_) -> m <> Meta.This) v.v_meta;
			Hashtbl.add locals v.v_id i;
			Hashtbl.add locals i.i_subst.v_id i;
			i

	method read v =
		let l = try
			Hashtbl.find locals v.v_id
		with Not_found ->
			{
				i_var = v;
				i_subst = v;
				i_outside = true;
				i_abstract_this = Meta.has Meta.This v.v_meta;
				i_captured = false;
				i_write = false;
				i_called = 0;
				i_force_temp = false;
				i_read = 0;
				i_default_value = None;
			}
		in
		if _in_local_fun then l.i_captured <- true;
		l

	method private get_substitutions p =
		(*
			if variables are not written and used with a const value, let's substitute
			with the actual value, either create a temp var
		*)
		let subst = ref PMap.empty in
		let is_constant e =
			let rec loop e =
				match e.eexpr with
				| TLocal _
				| TConst TThis (* not really, but should not be move inside a function body *)
					-> raise Exit
				| TObjectDecl _ | TArrayDecl _ -> raise Exit
				| TField (_,FEnum _)
				| TTypeExpr _
				| TConst _ -> ()
				| _ ->
					Type.iter loop e
			in
			try loop e; true with Exit -> false
		in
		let rec is_writable e =
			match e.eexpr with
			| TField _ | TEnumParameter _ | TLocal _ | TArray _ -> true
			| TCast(e1,None) -> is_writable e1
			| _  -> false
		in
		let vars = List.fold_left (fun acc (i,e) ->
			let accept vik =
				subst := PMap.add i.i_subst.v_id (vik,e) !subst;
				acc
			in
			let reject () =
				(* mark the replacement local for the analyzer *)
				if (i.i_read + i.i_called) <= 1 && not i.i_write then
					i.i_subst.v_kind <- VGenerated;
				(i.i_subst,Some e) :: acc
			in
			if i.i_abstract_this && i.i_write then begin
				if not (is_writable e) then error "Cannot modify the abstract value, store it into a local first" p;
				accept VIInline
			end else if i.i_force_temp || (i.i_captured && not (is_constant e)) then
				reject()
			else begin
				let vik = match e.eexpr with
					| TLocal _ when i.i_abstract_this -> VIInline
					| TLocal _ | TConst _ ->
						if not i.i_write then VIInline else VIDoNotInline
					| TFunction _ ->
						if i.i_write then error "Cannot modify a closure parameter inside inline method" p;
						if i.i_read <= 1 then VIInline else VIInlineIfCalled
					| _ ->
						if not i.i_write && (i.i_read + i.i_called) <= 1 then VIInline else VIDoNotInline
				in
				match vik with
				| VIInline -> accept vik
				| VIDoNotInline -> reject()
				| VIInlineIfCalled ->
					(* "Accept" it so it is added to the substitutions. *)
					ignore(accept vik);
					if i.i_read > 1 then
						(* If it is read more than once, we still have to reject because we need a local. *)
						reject()
					else
						(* Otherwise we don't! *)
						acc
			end
		) [] _inlined_vars in
		vars,!subst

	method initialize =
		(* use default values for null/unset arguments *)
		let rec loop acc pl al first =
			match pl, al with
			| _, [] ->
				acc
			| e :: pl, (v, opt) :: al ->
				let l = self#declare v in
				(*
					if we pass a Null<T> var to an inlined method that needs a T.
					we need to force a local var to be created on some platforms.
				*)
				if ctx.com.config.pf_static && not (is_nullable v.v_type) && is_null e.etype then l.i_force_temp <- true;
				(*
					if we cast from Dynamic, create a local var as well to do the cast
					once and allow DCE to perform properly.
				*)
				let dynamic_v = follow v.v_type == t_dynamic in
				let dynamic_e = follow e.etype == t_dynamic in
				let e = if dynamic_v <> dynamic_e then mk (TCast(e,None)) v.v_type e.epos else e in
				let e = match e.eexpr, opt with
					| TConst TNull , Some c -> c
					| _ , Some c when (match c.eexpr with TConst TNull -> false | _ -> true) && (not ctx.com.config.pf_static || is_nullable v.v_type) ->
						l.i_force_temp <- true;
						l.i_default_value <- Some c;
						e
					| _ -> e
				in
				if has_side_effect e then begin
					self#collect_modified_locals e;
					_had_side_effect <- true;
					l.i_force_temp <- true;
				end;
				(* We use a null expression because we only care about the type (for abstract casts). *)
				if l.i_abstract_this then l.i_subst.v_extra <- Some ([],Some {e with eexpr = TConst TNull});
				loop ((l,e) :: acc) pl al false
			| [], (v,opt) :: al ->
				let l = self#declare v in
				let e = match opt with
					| None -> mk (TConst TNull) v.v_type v.v_pos
					| Some e -> e
				in
				loop ((l,e) :: acc) [] al false
		in
		(*
			Build the expr/var subst list
		*)
		let ethis = (match ethis.eexpr with TConst TSuper -> { ethis with eexpr = TConst TThis } | _ -> ethis) in
		let vthis = alloc_var VInlined "_this" ethis.etype ethis.epos in
		let args1 = (ethis :: params) in
		let args2 = ((vthis,None) :: f.tf_args) in
		let vars = loop [] args1 args2 true in
		_inlined_vars <- vars; (* Order is reversed due to tail-recursion *)
		vthis

	method finalize config e tl tret p =
		let has_params,map_type = match config with Some config -> config | None -> inline_default_config cf ethis.etype in
		if self#had_side_effect then List.iter (fun (l,e) ->
			if self#might_be_affected e && not (ExtType.has_value_semantics e.etype) then l.i_force_temp <- true;
		) _inlined_vars;
		let vars,subst = self#get_substitutions p in
		let rec inline_params in_call in_assignment e =
			match e.eexpr with
			| TLocal v ->
				begin try
					let vik,e' = PMap.find v.v_id subst in
					begin match vik with
						| VIInline ->
							begin match e'.eexpr with
								(* If we inline a function expression, we have to duplicate its locals. *)
								| TFunction _ -> Texpr.duplicate_tvars e'
								| TCast(e1,None) when in_assignment -> e1
								| _ -> e'
							end
						| VIInlineIfCalled when in_call ->
							(* We allow inlining function expressions into call-places. However, we have to substitute
							   their locals to avoid duplicate declarations. *)
							Texpr.duplicate_tvars e'
						| _ -> e
					end
				with Not_found ->
					e
				end
			| TCall(e1,el) ->
				let e1 = inline_params true false e1 in
				let el = List.map (inline_params false false) el in
				{e with eexpr = TCall(e1,el)}
			| TBinop((OpAssign | OpAssignOp _ as op),e1,e2) ->
				let e1 = inline_params false true e1 in
				let e2 = inline_params false false e2 in
				{e with eexpr = TBinop(op,e1,e2)}
			| _ -> Type.map_expr (inline_params false false) e
		in
		let e = (if PMap.is_empty subst then e else inline_params false false e) in
		let init = match vars with [] -> None | l -> Some l in
		let md = ctx.curclass.cl_module.m_extra.m_display in
		md.m_inline_calls <- (cf.cf_name_pos,{p with pmax = p.pmin + String.length cf.cf_name}) :: md.m_inline_calls;
		let wrap e =
			(* we can't mute the type of the expression because it is not correct to do so *)
			let etype = if has_params then map_type e.etype else e.etype in
			(* if the expression is "untyped" and we don't want to unify it accidentally ! *)
			try (match follow e.etype with
			| TMono _ | TInst ({cl_kind = KTypeParameter _ },_) ->
				(match follow tret with
				| TAbstract ({ a_path = [],"Void" },_) -> e
				| _ -> raise (Unify_error []))
			| _ ->
				type_eq (if ctx.com.config.pf_static then EqDoNotFollowNull else EqStrict) etype tret;
				e)
			with Unify_error _ ->
				mk (TCast (e,None)) tret e.epos
		in
		let e = match init with
			| None when not self#has_return_value ->
				begin match e.eexpr with
					| TBlock _ -> {e with etype = tret}
					| _ -> mk (TBlock [e]) tret e.epos
				end
			| None ->
				begin match e.eexpr with
					| TBlock [e] -> wrap e
					| _ -> wrap e
				end
			| Some vl ->
				let el = DynArray.create () in
				let add = DynArray.add el in
				List.iter (fun (v,eo) ->
					add (mk (TVar (v,eo)) ctx.t.tvoid e.epos);
				) vl;
				List.iter (fun (l,e) -> match l.i_default_value with
					| None -> ()
					| Some e -> add (Texpr.set_default ctx.com.basic l.i_subst e e.epos)
				) _inlined_vars;
				begin match e.eexpr with
					| TBlock el -> List.iter add el
					| _ -> add e
				end;
				mk (TBlock (DynArray.to_list el)) tret e.epos
		in
		let e = inline_metadata e cf.cf_meta in
		let e = Diagnostics.secure_generated_code ctx e in
		if has_params then begin
			let mt = map_type cf.cf_type in
			let unify_func () = unify_raise ctx mt (TFun (tl,tret)) p in
			(match follow ethis.etype with
			| TAnon a -> (match !(a.a_status) with
				| Statics {cl_kind = KAbstractImpl a } when Meta.has Meta.Impl cf.cf_meta ->
					if cf.cf_name <> "_new" then begin
						(* the first argument must unify with a_this for abstract implementation functions *)
						let tb = (TFun(("",false,map_type a.a_this) :: (List.tl tl),tret)) in
						unify_raise ctx mt tb p
					end
				| _ -> unify_func())
			| _ -> unify_func());
		end;
		let vars = Hashtbl.create 0 in
		let rec map_var map_type v =
			if not (Hashtbl.mem vars v.v_id) then begin
				Hashtbl.add vars v.v_id ();
				if not (self#read v).i_outside then begin
					v.v_type <- map_type v.v_type;
					match v.v_extra with
					| Some(tl,Some e) ->
						v.v_extra <- Some(tl,Some (map_expr_type map_type e));
					| _ ->
						()
				end
			end;
			v
		and map_expr_type map_type e =
			(*
				No need to change typing of arguments of inlined call
				because they were already properly typed prior to inlining.
			*)
			let map_type =
				if List.memq e params then (fun t -> t)
				else map_type
			in
			Type.map_expr_type (map_expr_type map_type) map_type (map_var map_type) e
		in
		map_expr_type map_type e
end

let rec type_inline ctx cf f ethis params tret config p ?(self_calling_closure=false) force =
	(* perform some specific optimization before we inline the call since it's not possible to detect at final optimization time *)
	try
		let cl = (match follow ethis.etype with
			| TInst (c,_) -> c
			| TAnon a -> (match !(a.a_status) with Statics c -> c | _ -> raise Exit)
			| _ -> raise Exit
		) in
		(match api_inline ctx cl cf.cf_name params p with
		| None -> raise Exit
		| Some e -> Some e)
	with Exit ->
	let state = new inline_state ctx ethis params cf f p in
	let vthis = state#initialize in
	let opt f = function
		| None -> None
		| Some e -> Some (f e)
	in
	let in_loop = ref false in
	let return_type t el =
		(* If the function return is Dynamic or Void, stick to it. *)
		if follow f.tf_type == t_dynamic || ExtType.is_void (follow f.tf_type) then f.tf_type
		(* If the expression is Void, find common type of its branches. *)
		else if ExtType.is_void t then unify_min ctx el
		else t
	in
	let map_pos =
		if self_calling_closure || Common.defined ctx.com Define.KeepInlinePositions then (fun e -> e)
		else (fun e -> { e with epos = p })
	in
	let rec map term in_call e =
		let po = e.epos in
		let e = map_pos e in
		match e.eexpr with
		| TLocal v ->
			let l = state#read v in
			let i = if !in_loop then 2 else 1 in
			if in_call then
				l.i_called <- l.i_called + i
			else
				l.i_read <- l.i_read + i;
			let e = { e with eexpr = TLocal l.i_subst } in
			if l.i_abstract_this then mk (TCast(e,None)) v.v_type e.epos else e
		| TConst TThis ->
			let l = state#read vthis in
			l.i_read <- l.i_read + (if !in_loop then 2 else 1);
			{ e with eexpr = TLocal l.i_subst }
		| TVar (v,eo) ->
			{ e with eexpr = TVar ((state#declare v).i_subst,opt (map false false) eo)}
		| TReturn eo when not state#in_local_fun ->
			if not term then error "Cannot inline a not final return" po;
			(match eo with
			| None -> mk (TConst TNull) f.tf_type p
			| Some e ->
				state#set_return_value;
				map term false e)
		| TFor (v,e1,e2) ->
			let i = state#declare v in
			let e1 = map false false e1 in
			let old = !in_loop in
			in_loop := true;
			let e2 = map false false e2 in
			in_loop := old;
			{ e with eexpr = TFor (i.i_subst,e1,e2) }
		| TWhile (cond,eloop,flag) ->
			let cond = map false false cond in
			let old = !in_loop in
			in_loop := true;
			let eloop = map false false eloop in
			in_loop := old;
			{ e with eexpr = TWhile (cond,eloop,flag) }
		| TSwitch (e1,cases,def) when term ->
			let term = term && (def <> None || is_exhaustive e1) in
			let cases = List.map (fun (el,e) ->
				let el = List.map (map false false) el in
				el, map term false e
			) cases in
			let def = opt (map term false) def in
			let t = return_type e.etype ((List.map snd cases) @ (match def with None -> [] | Some e -> [e])) in
			{ e with eexpr = TSwitch (map false false e1,cases,def); etype = t }
		| TTry (e1,catches) ->
			let t = if not term then e.etype else return_type e.etype (e1::List.map snd catches) in
			{ e with eexpr = TTry (map term false e1,List.map (fun (v,e) ->
				let lv = (state#declare v).i_subst in
				let e = map term false e in
				lv,e
			) catches); etype = t }
		| TBlock l ->
			let old = save_locals ctx in
			let t = ref e.etype in
			let rec has_term_return e =
				let rec loop e =
					let r = match e.eexpr with
					| TReturn _ -> true
					| TFunction _ -> false
					| TIf (_,_,None) | TSwitch (_,_,None) | TFor _ | TWhile (_,_,NormalWhile) -> false (* we might not enter this code at all *)
					| TTry (a, catches) -> List.for_all has_term_return (a :: List.map snd catches)
					| TIf (cond,a,Some b) -> has_term_return cond || (has_term_return a && has_term_return b)
					| TSwitch (cond,cases,Some def) -> has_term_return cond || List.for_all has_term_return (def :: List.map snd cases)
					| TBinop (OpBoolAnd,a,b) -> has_term_return a && has_term_return b
					| _ -> Type.iter loop e; false
					in
					if r then raise Exit
				in
				try loop e; false with Exit -> true
			in
			let rec loop = function
				| [] when term ->
					t := mk_mono();
					[mk (TConst TNull) (!t) p]
				| [] -> []
				| [e] ->
					let e = map term false e in
					if term then t := e.etype;
					[e]
				| ({ eexpr = TIf (cond,e1,None) } as e) :: l when term && has_term_return e1 ->
					loop [{ e with eexpr = TIf (cond,e1,Some (mk (TBlock l) e.etype e.epos)); epos = punion e.epos (match List.rev l with e :: _ -> e.epos | [] -> assert false) }]
				| e :: l ->
					let e = map false false e in
					e :: loop l
			in
			let l = loop l in
			old();
			{ e with eexpr = TBlock l; etype = !t }
		| TIf (econd,eif,Some eelse) when term ->
			let econd = map false false econd in
			let eif = map term false eif in
			let eelse = map term false eelse in
			let t = return_type e.etype [eif;eelse] in
			{ e with eexpr = TIf(econd,eif,Some eelse); etype = t }
		| TParenthesis e1 ->
			let e1 = map term in_call e1 in
			mk (TParenthesis e1) e1.etype e.epos
		| TUnop ((Increment|Decrement) as op,flag,({ eexpr = TLocal v } as e1)) ->
			state#set_side_effect;
			let l = state#read v in
			l.i_write <- true;
			{e with eexpr = TUnop(op,flag,{e1 with eexpr = TLocal l.i_subst})}
		| TBinop ((OpAssign | OpAssignOp _) as op,({ eexpr = TLocal v } as e1),e2) ->
			state#set_side_effect;
			let l = state#read v in
			l.i_write <- true;
			let e2 = map false false e2 in
			{e with eexpr = TBinop(op,{e1 with eexpr = TLocal l.i_subst},e2)}
		| TObjectDecl fl ->
			let fl = List.map (fun (s,e) -> s,map false false e) fl in
			begin match follow e.etype with
				| TAnon an when (match !(an.a_status) with Const -> true | _ -> false) ->
					{e with eexpr = TObjectDecl fl; etype = TAnon { an with a_status = ref Closed}}
				| _ ->
					{e with eexpr = TObjectDecl fl}
			end
		| TFunction f ->
			let old = save_locals ctx in
			let args = List.map (function(v,c) -> (state#declare v).i_subst, c) f.tf_args in
			let restore = state#enter_local_fun in
			let expr = map false false f.tf_expr in
			restore();
			old();
			{ e with eexpr = TFunction { tf_args = args; tf_expr = expr; tf_type = f.tf_type } }
		| TCall({eexpr = TConst TSuper; etype = t},el) ->
			state#set_side_effect;
			begin match follow t with
			| TInst({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some ({eexpr = TFunction tf})} as cf)} as c,_) ->
				begin match type_inline_ctor ctx c cf tf ethis el po with
				| Some e -> map term false e
				| None -> error "Could not inline super constructor call" po
				end
			| _ -> error "Cannot inline function containing super" po
			end
		| TCall(e1,el) ->
			state#set_side_effect;
			let e1 = map false true e1 in
			let el = List.map (map false false) el in
			{e with eexpr = TCall(e1,el)}
		| TConst TSuper ->
			error "Cannot inline function containing super" po
		| TMeta((Meta.Ast,_,_) as m,e1) when term ->
			(* Special case for @:ast-wrapped TSwitch nodes: If the recursion alters the type of the TSwitch node, we also want
			   to alter the type of the TMeta node. *)
			let e1 = map term in_call e1 in
			{e with eexpr = TMeta(m,e1); etype = e1.etype}
		| TMeta(m,e1) ->
			let e1 = map term in_call e1 in
			{e with eexpr = TMeta(m,e1)}
		| TNew _ | TBinop ((OpAssignOp _ | OpAssign),_,_) | TUnop ((Increment|Decrement),_,_) ->
			state#set_side_effect;
			Type.map_expr (map false false) e
		| _ ->
			Type.map_expr (map false false) e
	in
	let e = map true false f.tf_expr in
	let rec arg_types params tf_args =
		match params, tf_args with
		| e :: rest_params, _ :: rest_args -> ("",false,e.etype) :: arg_types rest_params rest_args
		| [], (_, Some e) :: rest_args -> ("",true,mk_mono()) :: arg_types params rest_args
		| _ -> []
	in
	let tl = arg_types params f.tf_args in
	let e = state#finalize config e tl tret p in
	if Meta.has (Meta.Custom ":inlineDebug") ctx.meta then begin
		let se t = s_expr_pretty true t true (s_type (print_context())) in
		print_endline (Printf.sprintf "Inline %s:\n\tArgs: %s\n\tExpr: %s\n\tResult: %s"
			cf.cf_name
			(String.concat "" (List.map (fun (i,e) -> Printf.sprintf "\n\t\t%s<%i> = %s" (i.i_subst.v_name) (i.i_subst.v_id) (se "\t\t" e)) state#inlined_vars))
			(se "\t" f.tf_expr)
			(se "\t" e)
		);
	end;
	Some e

(* Same as type_inline, but modifies the function body to add field inits *)
and type_inline_ctor ctx c cf tf ethis el po =
	let field_inits =
		let cparams = List.map snd c.cl_params in
		let ethis = mk (TConst TThis) (TInst (c,cparams)) c.cl_pos in
		let el = List.fold_left (fun acc cf ->
			match cf.cf_kind,cf.cf_expr with
			| Var _,Some e ->
				let lhs = mk (TField(ethis,FInstance (c,cparams,cf))) cf.cf_type e.epos in
				let eassign = mk (TBinop(OpAssign,lhs,e)) cf.cf_type e.epos in
				eassign :: acc
			| _ -> acc
		) [] c.cl_ordered_fields in
		List.rev el
	in
	let tf =
		if field_inits = [] then tf
		else
			let bl = match tf.tf_expr with {eexpr = TBlock b } -> b | x -> [x] in
			{tf with tf_expr = mk (TBlock (field_inits @ bl)) ctx.t.tvoid c.cl_pos}
	in
	type_inline ctx cf tf ethis el ctx.t.tvoid None po true
