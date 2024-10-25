open Globals
open Ast
open Type
open Common
open Typecore
open TyperBase
open Fields
open Calls
open Error
open Texpr.Builder

let optimize_for_loop_iterator ctx v e1 e2 p =
	let c,tl =
		let rec get_class_and_params e =
			match follow e.etype with
			| TInst (c,pl) -> c,pl
			| _ ->
				match e.eexpr with
				| TCast (e,None) ->
					get_class_and_params e
				| TCall ({ eexpr = TField (_, FInstance (c,pl,cf)) }, _) ->
					let t = apply_params c.cl_params pl cf.cf_type in
					(match follow t with
					| TFun (_, t) ->
						(match follow t with
						| TInst (c,pl) -> c,pl
						| _ -> raise Exit
						)
					| _ -> raise Exit
					)
				| _ -> raise Exit
		in
		get_class_and_params e1
	in
	let _, _, fhasnext = (try raw_class_field (fun cf -> apply_params c.cl_params tl cf.cf_type) c tl "hasNext" with Not_found -> raise Exit) in
	if fhasnext.cf_kind <> Method MethInline then raise Exit;
	let it_type = TInst(c,tl) in
	let tmp = gen_local ctx it_type e1.epos in
	let eit = mk (TLocal tmp) it_type p in
	let ehasnext = make_call ctx (mk (TField (eit,FInstance (c, tl, fhasnext))) (TFun([],ctx.t.tbool)) p) [] ctx.t.tbool p in
	let fa_next =
		try
			match raw_class_field (fun cf -> apply_params c.cl_params tl cf.cf_type) c tl "next" with
			| _, _, fa -> FInstance (c, tl, fa)
		with Not_found ->
			quick_field_dynamic eit.etype "next"
	in
	let enext = mk (TVar (v,Some (make_call ctx (mk (TField (eit,fa_next)) (TFun ([],v.v_type)) p) [] v.v_type p))) ctx.t.tvoid p in
	let eblock = (match e2.eexpr with
		| TBlock el -> { e2 with eexpr = TBlock (enext :: el) }
		| _ -> mk (TBlock [enext;e2]) ctx.t.tvoid p
	) in
	mk (TBlock [
		mk (TVar (tmp,Some e1)) ctx.t.tvoid p;
		mk (TWhile (ehasnext,eblock,NormalWhile)) ctx.t.tvoid p
	]) ctx.t.tvoid p

type unroll_parameters = {
	expression_weight : int;
}

module IterationKind = struct
	type t_kind =
		| IteratorIntConst of texpr * texpr * bool (* ascending? *)
		| IteratorIntUnroll of int * int * bool * unroll_parameters
		| IteratorInt of texpr * texpr
		| IteratorArrayDecl of texpr list
		| IteratorArray
		| IteratorArrayAccess
		| IteratorGenericStack of tclass
		| IteratorIterator
		| IteratorCustom of (texpr -> texpr -> Type.t -> pos -> texpr) * (texpr -> pos -> texpr)
		| IteratorAbstract of tvar * texpr * texpr
		| IteratorDynamic

	type t = {
		it_kind : t_kind;
		it_type : Type.t;
		it_expr : texpr;
	}

	let type_field_config = {
		Fields.TypeFieldConfig.do_resume = true;
		allow_resolve = false;
	}

	let get_next_array_element arr iexpr pt p =
		(mk (TArray (arr,iexpr)) pt p)

	let check_iterator ?(resume=false) ?last_resort ctx s e p =
		let t,pt = Typeload.t_iterator ctx p in
		let dynamic_iterator = ref None in
		let e1 = try
			let e = AbstractCast.cast_or_unify_raise ctx t e p in
			match Abstract.follow_with_abstracts e.etype with
			| TDynamic _ | TMono _ ->
				(* try to find something better than a dynamic value to iterate on *)
				dynamic_iterator := Some e;
				raise_error_msg (Unify [Unify_custom "Avoid iterating on a dynamic value"]) p
			| _ -> e
		with Error { err_message = Unify _ } ->
			let try_last_resort after =
				try
					match last_resort with
					| Some fn -> fn()
					| None -> raise Not_found
				with Not_found ->
					after()
			in
			let try_acc acc =
				let acc_expr = build_call ctx acc [] WithType.value e.epos in
				try
					unify_raise acc_expr.etype t acc_expr.epos;
					acc_expr
				with Error ({ err_message = Unify _ } as err) ->
					try_last_resort (fun () ->
						match !dynamic_iterator with
						| Some e -> e
						| None ->
							if resume then raise Not_found;
							display_error_ext ctx.com (make_error ~depth:err.err_depth ~sub:[err] (Custom "Field iterator has an invalid type") acc_expr.epos);
							mk (TConst TNull) t_dynamic p
					)
			in
			try
				let acc = type_field ({do_resume = true;allow_resolve = false}) ctx e s e.epos (MCall []) (WithType.with_type t) in
				try_acc acc;
			with Not_found ->
				try_last_resort (fun () ->
					match !dynamic_iterator with
					| Some e -> e
					| None ->
						let acc = type_field ({do_resume = resume;allow_resolve = false}) ctx e s e.epos (MCall []) (WithType.with_type t) in
						try_acc acc
				)
		in
		e1,pt

	let of_texpr_by_array_access ctx e p =
		match follow e.etype with
		| TInst({ cl_array_access = Some pt } as c,pl) when (try match follow (PMap.find "length" c.cl_fields).cf_type with TAbstract ({ a_path = [],"Int" },[]) -> true | _ -> false with Not_found -> false) && not (PMap.mem "iterator" c.cl_fields) ->
			IteratorArrayAccess,e,apply_params c.cl_params pl pt
		| TAbstract({a_impl = Some c} as a,tl) ->
			let cf_length = PMap.find "get_length" c.cl_statics in
			let get_length e p =
				make_static_call ctx c cf_length (apply_params a.a_params tl) [e] ctx.com.basic.tint p
			in
			(match follow cf_length.cf_type with
				| TFun(_,tr) ->
					(match follow tr with
						| TAbstract({a_path = [],"Int"},_) -> ()
						| _ -> raise Not_found
					)
				| _ ->
					raise Not_found
			);
			(try
				(* first try: do we have an @:arrayAccess getter field? *)
				let todo = mk (TConst TNull) ctx.t.tint p in
				let cf,_,r,_ = AbstractCast.find_array_read_access_raise ctx a tl todo p in
				let get_next e_base e_index t p =
					make_static_call ctx c cf (apply_params a.a_params tl) [e_base;e_index] r p
				in
				IteratorCustom(get_next,get_length),e,r
			with Not_found ->
				(* second try: do we have @:arrayAccess on the abstract itself? *)
				if not (Meta.has Meta.ArrayAccess a.a_meta) then raise Not_found;
				(* let's allow this only for core-type abstracts *)
				if not (Meta.has Meta.CoreType a.a_meta) then raise Not_found;
				(* in which case we assume that a singular type parameter is the element type *)
				let t = match tl with [t] -> t | _ -> raise Not_found in
				IteratorCustom(get_next_array_element,get_length),e,t
			)
	 	| _ -> raise Not_found

	let map_unroll_params ctx unroll_params i = match unroll_params with
		| None ->
			None
		| Some unroll_params ->
			let cost = i * unroll_params.expression_weight in
			let max_cost = try
				int_of_string (Common.defined_value ctx.com Define.LoopUnrollMaxCost)
			with Not_found ->
				250
			in
			if cost <= max_cost then
				Some unroll_params
			else
				None

	let of_texpr ?(resume=false) ctx e unroll_params p =
		let dynamic_iterator e =
			display_error ctx.com "You can't iterate on a Dynamic value, please specify Iterator or Iterable" e.epos;
			IteratorDynamic,e,t_dynamic
		in
		let check_iterator () =
			let array_access_result = ref None in
			let last_resort () =
				array_access_result := Some (of_texpr_by_array_access ctx e p);
				mk (TConst TNull) t_dynamic p
			in
			let e1,pt = check_iterator ~resume ~last_resort ctx "iterator" e p in
			match !array_access_result with
			| Some result -> result
			| None ->
				match Abstract.follow_with_abstracts e1.etype with
					| (TMono _ | TDynamic _) -> dynamic_iterator e1;
					| _ -> (IteratorIterator,e1,pt)
		in
		let try_forward_array_iterator () =
			match follow e.etype with
			| TAbstract ({ a_this = (TInst ({ cl_path = [],"Array" }, [_]) as array_type)} as abstr, params) ->
				let forwards_iterator =
					match Meta.get Meta.Forward abstr.a_meta with
					| (_, [], _) -> true
					| (_, args, _) ->
						List.exists (fun (arg,_) -> match arg with EConst (Ident"iterator") -> true | _ -> false ) args
				in
				if forwards_iterator then
					match apply_params abstr.a_params params array_type with
					| TInst({ cl_path = [],"Array" },[pt]) as t -> IteratorArray,(mk_cast e t e.epos),pt
					| _ -> raise Not_found
				else
					raise Not_found
			| _ -> raise Not_found
		in
		let it,e1,pt = match e.eexpr,follow e.etype with
		| TNew ({ cl_path = ([],"IntIterator") },[],[efrom;eto]),_ ->
			let it = match efrom.eexpr,eto.eexpr with
				| TConst (TInt a),TConst (TInt b) ->
					let diff = Int32.to_int (Int32.sub a b) in
					begin match map_unroll_params ctx unroll_params (abs diff) with
					| Some unroll_params ->
						IteratorIntUnroll(Int32.to_int a,abs(diff),diff <= 0,unroll_params)
					| None ->
						IteratorIntConst(efrom,eto,diff <= 0)
					end
				| _ ->
					let eto = match follow eto.etype with
						| TAbstract ({ a_path = ([],"Int") }, []) -> eto
						| _ -> { eto with eexpr = TCast(eto, None); etype = ctx.t.tint }
					in
					IteratorInt(efrom,eto)
			in
			it,e,ctx.t.tint
		| TArrayDecl el,TInst({ cl_path = [],"Array" },[pt]) ->
			let it = match map_unroll_params ctx unroll_params (List.length el) with
				| Some _ -> IteratorArrayDecl el
				| None -> IteratorArray
			in
			(it,e,pt)
		| _,TInst({ cl_path = [],"Array" },[pt])
		| _,TInst({ cl_path = ["flash"],"Vector" },[pt]) ->
			IteratorArray,e,pt
		| _,TAbstract({ a_impl = Some c },_) ->
			(try
				let v_tmp = gen_local ctx e.etype e.epos in
				let e_tmp = make_local v_tmp v_tmp.v_pos in
				let acc_next = type_field type_field_config ctx e_tmp "next" p (MCall []) WithType.value (* WITHTYPETODO *) in
				let acc_hasNext = type_field type_field_config ctx e_tmp "hasNext" p (MCall []) (WithType.with_type ctx.t.tbool) in
				(match acc_next, acc_hasNext with
					| AKExpr({ eexpr = TField(_, FDynamic _)}), _
					| _, AKExpr({ eexpr = TField(_, FDynamic _)}) -> raise Not_found
					| _ -> ()
				);
				let e_next = build_call ctx acc_next [] WithType.value e.epos in
				let e_hasNext = build_call ctx acc_hasNext [] WithType.value e.epos in
				IteratorAbstract(v_tmp,e_next,e_hasNext),e,e_next.etype
			with Not_found ->
				(try try_forward_array_iterator ()
				with Not_found -> check_iterator ())
			)
		| _, TAbstract _ ->
			(try try_forward_array_iterator ()
			with Not_found -> check_iterator ())
		| _,TInst ({ cl_kind = KGenericInstance ({ cl_path = ["haxe";"ds"],"GenericStack" },[pt]) } as c,[]) ->
			IteratorGenericStack c,e,pt
		| _,(TMono _ | TDynamic _) ->
			dynamic_iterator e
		| _ ->
			check_iterator ()
		in
		{
			it_kind = it;
			it_type = pt;
			it_expr = if not ctx.allow_transform then e else e1;
		}

	let to_texpr ctx v iterator e2 p =
		let e1,pt = iterator.it_expr,iterator.it_type in
		let t_void = ctx.t.tvoid in
		let t_int = ctx.t.tint in
		let mk_field e n =
			TField (e,try quick_field e.etype n with Not_found -> Error.raise_msg (Printf.sprintf "Could not find field %s on %s" n (s_type_kind e.etype)) e.epos)
		in
		let get_array_length arr p =
			mk (mk_field arr "length") ctx.com.basic.tint p
		in
		let check_loop_var_modification vl e =
			let rec loop e =
				match e.eexpr with
				| TBinop (OpAssign,{ eexpr = TLocal l },_)
				| TBinop (OpAssignOp _,{ eexpr = TLocal l },_)
				| TUnop (Increment,_,{ eexpr = TLocal l })
				| TUnop (Decrement,_,{ eexpr = TLocal l })  when List.memq l vl ->
					raise_typing_error "Loop variable cannot be modified" e.epos
				| _ ->
					Type.iter loop e
			in
			loop e
		in
		let gen_int_iter e1 pt f_get f_length =
			let index = gen_local ctx t_int v.v_pos in
			index.v_meta <- (Meta.ForLoopVariable,[],null_pos) :: index.v_meta;
			let arr, avars = (match e1.eexpr with
				| TLocal _ -> e1, None
				| _ ->
					let atmp = gen_local ctx e1.etype e1.epos in
					mk (TLocal atmp) e1.etype e1.epos, (Some (atmp,Some e1))
			) in
			let iexpr = mk (TLocal index) t_int p in
			let aget = mk (TVar (v,Some (f_get arr iexpr pt p))) t_void v.v_pos in
			let incr = mk (TUnop (Increment,Prefix,iexpr)) t_int p in
			let block = match e2.eexpr with
				| TBlock el -> mk (TBlock (aget :: incr :: el)) t_void e2.epos
				| _ -> mk (TBlock [aget;incr;e2]) t_void p
			in
			let ivar = Some (mk (TConst (TInt 0l)) t_int p) in
			let elength = f_length arr p in
			let el = [mk (TWhile (
					mk (TBinop (OpLt, iexpr, elength)) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			] in
			let el = match avars with None -> el | Some (v,eo) -> (mk (TVar (v,eo)) t_void p) :: el in
			let el = (mk (TVar (index,ivar)) t_void p) :: el in
			mk (TBlock el) t_void p
		in
		match iterator.it_kind with
		| _ when not ctx.allow_transform ->
			mk (TFor(v,e1,e2)) t_void p
		| IteratorIntUnroll(offset,length,ascending,unroll_params) ->
			check_loop_var_modification [v] e2;
			if not ascending then raise_typing_error "Cannot iterate backwards" p;
			let rec unroll acc i =
				if i = length then
					List.rev acc
				else begin
					let ei = make_int ctx.t (if ascending then i + offset else offset - i) p in
					let local_vars = ref [] in
					let rec loop e = match e.eexpr with
					| TLocal v' when v == v' ->
						{ei with epos = e.epos}
					| TVar(v,eo) when has_var_flag v VStatic ->
						if acc = [] then
							local_vars := {e with eexpr = TVar(v,eo)} :: !local_vars;
						mk (TConst TNull) t_dynamic null_pos
					| _ ->
						map_expr loop e
				in
				let e2 = loop e2 in
				let acc = acc @ !local_vars in
				let e2 = Texpr.duplicate_tvars e_identity e2 in
				unroll (e2 :: acc) (i + 1)
			end in
			let el = unroll [] 0 in
			mk (TBlock el) t_void p
		| IteratorIntConst(a,b,ascending) ->
			check_loop_var_modification [v] e2;
			if not ascending then raise_typing_error "Cannot iterate backwards" p;
			let v_index = gen_local ctx t_int a.epos in
			let evar_index = mk (TVar(v_index,Some a)) t_void a.epos in
			let ev_index = make_local v_index v_index.v_pos in
			let op1,op2 = if ascending then (OpLt,Increment) else (OpGt,Decrement) in
			let econd = binop op1 ev_index b ctx.t.tbool (punion v.v_pos b.epos) in
			let ev_incr = mk (TUnop(op2,Postfix,ev_index)) t_int (punion a.epos b.epos) in
			let evar = mk (TVar(v,Some ev_incr)) t_void (punion v.v_pos a.epos) in
			let e2 = concat evar e2 in
			let ewhile = mk (TWhile(econd,e2,NormalWhile)) t_void p in
			mk (TBlock [
				evar_index;
				ewhile;
			]) t_void p
		| IteratorInt(a,b) ->
			check_loop_var_modification [v] e2;
			let v_index = gen_local ctx t_int a.epos in
			let evar_index = mk (TVar(v_index,Some a)) t_void a.epos in
			let ev_index = make_local v_index v_index.v_pos in
			let v_b = gen_local ctx b.etype b.epos in
			let evar_b = mk (TVar (v_b,Some b)) t_void b.epos in
			let ev_b = make_local v_b b.epos in
			let econd = binop OpLt ev_index ev_b ctx.t.tbool (punion v.v_pos b.epos) in
			let ev_incr = mk (TUnop(Increment,Postfix,ev_index)) t_int (punion a.epos b.epos) in
			let evar = mk (TVar(v,Some ev_incr)) t_void (punion v.v_pos a.epos) in
			let e2 = concat evar e2 in
			let ewhile = mk (TWhile(econd,e2,NormalWhile)) t_void p in
			mk (TBlock [
				evar_index;
				evar_b;
				ewhile;
			]) t_void p
		| IteratorArrayDecl el ->
			let el = List.map (fun e ->
				let ev = mk (TVar(v,Some e)) t_void e.epos in
				let e = concat ev e2 in
				Texpr.duplicate_tvars e_identity e
			) el in
			mk (TBlock el) t_void p
		| IteratorArray | IteratorArrayAccess ->
			gen_int_iter e1 pt get_next_array_element get_array_length
		| IteratorCustom(f_next,f_length) ->
			gen_int_iter e1 pt f_next f_length
		| IteratorIterator ->
			begin try optimize_for_loop_iterator ctx v e1 e2 p
			with Exit -> mk (TFor(v,e1,e2)) t_void p end
		| IteratorGenericStack c ->
			let tcell = (try (PMap.find "head" c.cl_fields).cf_type with Not_found -> die "" __LOC__) in
			let cell = gen_local ctx tcell p in
			let cexpr = mk (TLocal cell) tcell p in
			let evar = mk (TVar (v,Some (mk (mk_field cexpr "elt") pt p))) t_void v.v_pos in
			let enext = mk (TBinop (OpAssign,cexpr,mk (mk_field cexpr "next") tcell p)) tcell p in
			let block = match e2.eexpr with
				| TBlock el -> mk (TBlock (evar :: enext :: el)) t_void e2.epos
				| _ -> mk (TBlock [evar;enext;e2]) t_void p
			in
			mk (TBlock [
				mk (TVar (cell,Some (mk (mk_field e1 "head") tcell p))) t_void p;
				mk (TWhile (
					mk (TBinop (OpNotEq, cexpr, mk (TConst TNull) tcell p)) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p
			]) t_void p
		| IteratorAbstract(v_tmp,e_next,e_hasNext) ->
			let evar = mk (TVar(v,Some e_next)) t_void p in
			let e2 = concat evar e2 in
			let ewhile = mk (TWhile(e_hasNext,e2,NormalWhile)) t_void p in
			mk (TBlock [
				mk (TVar(v_tmp,Some e1)) t_void p;
				ewhile;
			]) t_void p
		| IteratorDynamic ->
			mk (TFor(v,e1,e2)) t_void p
end

let get_unroll_params ctx e2 =
	let num_expr = ref 0 in
	let rec loop e = match fst e with
		| EContinue | EBreak ->
			raise Exit
		| _ ->
			incr num_expr;
			Ast.map_expr loop e
	in
	try
		if ctx.com.display.dms_kind <> DMNone then raise Exit;
		ignore(loop e2);
		Some {
			expression_weight = !num_expr;
		}
	with Exit ->
		None

let get_unroll_params_t ctx e2 =
	let num_expr = ref 0 in
	let rec loop e = match e.eexpr with
		| TContinue | TBreak ->
			raise Exit
		| _ ->
			incr num_expr;
			Type.map_expr loop e
	in
	try
		if ctx.com.display.dms_kind <> DMNone then raise Exit;
		ignore(loop e2);
		Some {
			expression_weight = !num_expr;
		}
	with Exit ->
		None

type iteration_ident = string * pos * display_kind option

type iteration_kind =
	| IKNormal of iteration_ident
	| IKKeyValue of iteration_ident * iteration_ident

let type_for_loop ctx handle_display ik e1 e2 p =
	let old_loop = ctx.e.in_loop in
	let old_locals = save_locals ctx in
	ctx.e.in_loop <- true;
	let e2 = Expr.ensure_block e2 in
	let check_display (i,pi,dko) = match dko with
		| None -> ()
		| Some dk -> ignore(handle_display ctx (EConst(Ident i.v_name),i.v_pos) dk MGet (WithType.with_type i.v_type))
	in
	match ik with
	| IKNormal(i,pi,dko) ->
		let iterator = IterationKind.of_texpr ctx e1 (get_unroll_params ctx e2) p in
		let i = add_local_with_origin ctx TVOForVariable i iterator.it_type pi in
		let e2 = type_expr ctx e2 NoValue in
		check_display (i,pi,dko);
		ctx.e.in_loop <- old_loop;
		old_locals();
		begin try
			IterationKind.to_texpr ctx i iterator e2 p
		with Exit ->
			mk (TFor (i,iterator.it_expr,e2)) ctx.t.tvoid p
		end
	| IKKeyValue((ikey,pkey,dkokey),(ivalue,pvalue,dkovalue)) ->
		(match follow e1.etype with
		| TDynamic _ | TMono _ ->
			display_error ctx.com "You can't iterate on a Dynamic value, please specify KeyValueIterator or KeyValueIterable" e1.epos;
		| _ -> ()
		);
		let e1,pt = IterationKind.check_iterator ctx "keyValueIterator" e1 e1.epos in
		let vtmp = gen_local ctx e1.etype e1.epos in
		let etmp = make_local vtmp vtmp.v_pos in
		let ehasnext = build_call ctx (type_field_default_cfg ctx etmp "hasNext" etmp.epos (MCall []) (WithType.with_type ctx.t.tbool)) [] WithType.value etmp.epos in
		let enext = build_call ctx (type_field_default_cfg ctx etmp "next" etmp.epos (MCall []) WithType.value (* WITHTYPETODO *)) [] WithType.value etmp.epos in
		let v = gen_local ctx pt e1.epos in
		let ev = make_local v v.v_pos in
		let ekey = Calls.acc_get ctx (type_field_default_cfg ctx ev "key" ev.epos MGet WithType.value) in
		let evalue = Calls.acc_get ctx (type_field_default_cfg ctx ev "value" ev.epos MGet WithType.value) in
		let vkey = add_local_with_origin ctx TVOForVariable ikey ekey.etype pkey in
		let vvalue = add_local_with_origin ctx TVOForVariable ivalue evalue.etype pvalue in
		let e2 = type_expr ctx e2 NoValue in
		check_display (vkey,pkey,dkokey);
		check_display (vvalue,pvalue,dkovalue);
		let ebody = mk (TBlock [
			mk (TVar(v,Some enext)) ctx.t.tvoid enext.epos;
			mk (TVar(vkey,Some ekey)) ctx.t.tvoid ekey.epos;
			mk (TVar(vvalue,Some evalue)) ctx.t.tvoid evalue.epos;
			e2;
		]) ctx.t.tvoid e2.epos in
		let e = mk (TBlock [
			mk (TVar(vtmp,Some e1)) ctx.t.tvoid e1.epos;
			mk (TWhile(ehasnext,ebody,NormalWhile)) ctx.t.tvoid p;
		]) ctx.t.tvoid p in
		ctx.e.in_loop <- old_loop;
		old_locals();
		e

let type_for_loop ctx handle_display it e2 p =
	let rec loop_ident dko e1 = match e1 with
		| EConst(Ident i),p -> i,p,dko
		| EDisplay(e1,dk),_ -> loop_ident (Some dk) e1
		| _ -> raise_typing_error "Identifier expected" (pos e1)
	in
	let rec loop dko e1 = match fst e1 with
		| EBinop(OpIn,e1,e2) ->
			begin match fst e1 with
			| EBinop(OpArrow,ei1,ei2) -> IKKeyValue(loop_ident None ei1,loop_ident None ei2),e2
			| _ -> IKNormal (loop_ident dko e1),e2
			end
		| EDisplay(e1,dk) -> loop (Some dk) e1
		| EBinop(OpArrow,ei1,(EBinop(OpIn,ei2,e2),_)) -> IKKeyValue(loop_ident None ei1,loop_ident None ei2),e2
		| _ ->
			begin match dko with
			| Some dk -> ignore(handle_display ctx e1 dk MGet WithType.value);
			| None -> ()
			end;
			raise_typing_error "For expression should be 'v in expr'" (snd it)
	in
	let ik,e1 = loop None it in
	let e1 = type_expr ctx e1 WithType.value in
	type_for_loop ctx handle_display ik e1 e2 p
