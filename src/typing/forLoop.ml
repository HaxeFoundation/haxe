open Globals
open Ast
open Type
open Common
open Typecore
open TyperBase
open Fields
open Error

(* ---------------------------------------------------------------------- *)
(* LOOPS *)

let rec optimize_for_loop ctx (i,pi) e1 e2 p =
	let t_void = ctx.t.tvoid in
	let t_int = ctx.t.tint in
	let lblock el = Some (mk (TBlock el) t_void p) in
	let mk_field e n =
		TField (e,try quick_field e.etype n with Not_found -> assert false)
	in
	let gen_int_iter pt f_get f_length =
		let i = add_local ctx i pt pi in
		let index = gen_local ctx t_int pi in
		index.v_meta <- (Meta.ForLoopVariable,[],null_pos) :: index.v_meta;
		let arr, avars = (match e1.eexpr with
			| TLocal _ -> e1, None
			| _ ->
				let atmp = gen_local ctx e1.etype e1.epos in
				mk (TLocal atmp) e1.etype e1.epos, (Some (atmp,Some e1))
		) in
		let iexpr = mk (TLocal index) t_int p in
		let e2 = type_expr ctx e2 NoValue in
		let aget = mk (TVar (i,Some (f_get arr iexpr pt p))) t_void pi in
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
		lblock el
	in
	let get_next_array_element arr iexpr pt p =
		(mk (TArray (arr,iexpr)) pt p)
	in
	let get_array_length arr p =
		mk (mk_field arr "length") ctx.com.basic.tint p
	in
	match e1.eexpr, follow e1.etype with
	| TNew ({ cl_path = ([],"IntIterator") },[],[i1;i2]) , _ ->
		let max = (match i1.eexpr , i2.eexpr with
			| TConst (TInt a), TConst (TInt b) when Int32.compare b a < 0 -> error "Range operator can't iterate backwards" p
			| _, TConst _ -> None
			| _ -> Some (gen_local ctx t_int e1.epos)
		) in
		let tmp = gen_local ctx t_int pi in
		tmp.v_meta <- (Meta.ForLoopVariable,[],null_pos) :: tmp.v_meta;
		let i = add_local ctx i t_int pi in
		let rec check e =
			match e.eexpr with
			| TBinop (OpAssign,{ eexpr = TLocal l },_)
			| TBinop (OpAssignOp _,{ eexpr = TLocal l },_)
			| TUnop (Increment,_,{ eexpr = TLocal l })
			| TUnop (Decrement,_,{ eexpr = TLocal l })  when l == i ->
				error "Loop variable cannot be modified" e.epos
			| _ ->
				Type.iter check e
		in
		let e2 = type_expr ctx e2 NoValue in
		check e2;
		let etmp = mk (TLocal tmp) t_int p in
		let incr = mk (TUnop (Increment,Postfix,etmp)) t_int p in
		let init = mk (TVar (i,Some incr)) t_void pi in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (init :: el)) t_void e2.epos
			| _ -> mk (TBlock [init;e2]) t_void p
		in
		(*
			force locals to be of Int type (to prevent Int/UInt issues)
		*)
		let i2 = match follow i2.etype with
			| TAbstract ({ a_path = ([],"Int") }, []) -> i2
			| _ -> { i2 with eexpr = TCast(i2, None); etype = t_int }
		in
		(match max with
		| None ->
			lblock [
				mk (TVar (tmp,Some i1)) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, i2)) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			]
		| Some max ->
			lblock [
				mk (TVar (tmp,Some i1)) t_void p;
				mk (TVar (max,Some i2)) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, mk (TLocal max) t_int p)) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			])
	| TArrayDecl el, TInst({ cl_path = [],"Array" },[pt]) ->
		begin try
			let num_expr = ref 0 in
			let rec loop e = match fst e with
				| EContinue | EBreak ->
					raise Exit
				| _ ->
					incr num_expr;
					Ast.map_expr loop e
			in
			ignore(loop e2);
			let cost = (List.length el) * !num_expr in
			let max_cost = try
				int_of_string (Common.defined_value ctx.com Define.LoopUnrollMaxCost)
			with Not_found ->
				250
			in
			if cost > max_cost then raise Exit;
			let el = List.map (fun e ->
				let v = add_local ctx i pt p in
				let ev = mk (TVar(v, None)) ctx.t.tvoid p in
				let typed_e2 = type_expr ctx e2 NoValue in
				let eloc = mk (TLocal v) v.v_type p in
				let e_assign = mk (TBinop(OpAssign,eloc,e)) e.etype e.epos in
				concat ev (concat e_assign typed_e2)
			) el in
			Some (mk (TBlock el) ctx.t.tvoid p)
		with Exit ->
			gen_int_iter pt get_next_array_element get_array_length
		end
	| _ , TInst({ cl_path = [],"Array" },[pt])
	| _ , TInst({ cl_path = ["flash"],"Vector" },[pt]) ->
		gen_int_iter pt get_next_array_element get_array_length
	| _ , TInst({ cl_array_access = Some pt } as c,pl) when (try match follow (PMap.find "length" c.cl_fields).cf_type with TAbstract ({ a_path = [],"Int" },[]) -> true | _ -> false with Not_found -> false) && not (PMap.mem "iterator" c.cl_fields) ->
		gen_int_iter (apply_params c.cl_params pl pt) get_next_array_element get_array_length
	| _, TAbstract({a_impl = Some c} as a,tl) ->
		begin try
			let cf_length = PMap.find "get_length" c.cl_statics in
			let get_length e p =
				make_static_call ctx c cf_length (apply_params a.a_params tl) [e] ctx.com.basic.tint p
			in
			begin match follow cf_length.cf_type with
				| TFun(_,tr) ->
					begin match follow tr with
						| TAbstract({a_path = [],"Int"},_) -> ()
						| _ -> raise Not_found
					end
				| _ ->
					raise Not_found
			end;
			begin try
				(* first try: do we have an @:arrayAccess getter field? *)
				let todo = mk (TConst TNull) ctx.t.tint p in
				let cf,_,r,_,_ = AbstractCast.find_array_access_raise ctx a tl todo None p in
				let get_next e_base e_index t p =
					make_static_call ctx c cf (apply_params a.a_params tl) [e_base;e_index] r p
				in
				gen_int_iter r get_next get_length
			with Not_found ->
				(* second try: do we have @:arrayAccess on the abstract itself? *)
				if not (Meta.has Meta.ArrayAccess a.a_meta) then raise Not_found;
				(* let's allow this only for core-type abstracts *)
				if not (Meta.has Meta.CoreType a.a_meta) then raise Not_found;
				(* in which case we assume that a singular type parameter is the element type *)
				let t = match tl with [t] -> t | _ -> raise Not_found in
				gen_int_iter t get_next_array_element get_length
		end with Not_found ->
			None
		end
	| _ , TInst ({ cl_kind = KGenericInstance ({ cl_path = ["haxe";"ds"],"GenericStack" },[t]) } as c,[]) ->
		let tcell = (try (PMap.find "head" c.cl_fields).cf_type with Not_found -> assert false) in
		let i = add_local ctx i t p in
		let cell = gen_local ctx tcell p in
		let cexpr = mk (TLocal cell) tcell p in
		let e2 = type_expr ctx e2 NoValue in
		let evar = mk (TVar (i,Some (mk (mk_field cexpr "elt") t p))) t_void pi in
		let enext = mk (TBinop (OpAssign,cexpr,mk (mk_field cexpr "next") tcell p)) tcell p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (evar :: enext :: el)) t_void e2.epos
			| _ -> mk (TBlock [evar;enext;e2]) t_void p
		in
		lblock [
			mk (TVar (cell,Some (mk (mk_field e1 "head") tcell p))) t_void p;
			mk (TWhile (
				mk (TBinop (OpNotEq, cexpr, mk (TConst TNull) tcell p)) ctx.t.tbool p,
				block,
				NormalWhile
			)) t_void p
		]
	| _ ->
		None

let optimize_for_loop_iterator ctx v e1 e2 p =
	let c,tl = (match follow e1.etype with TInst (c,pl) -> c,pl | _ -> raise Exit) in
	let _, _, fhasnext = (try raw_class_field (fun cf -> apply_params c.cl_params tl cf.cf_type) c tl "hasNext" with Not_found -> raise Exit) in
	if fhasnext.cf_kind <> Method MethInline then raise Exit;
	let tmp = gen_local ctx e1.etype e1.epos in
	let eit = mk (TLocal tmp) e1.etype p in
	let ehasnext = make_call ctx (mk (TField (eit,FInstance (c, tl, fhasnext))) (TFun([],ctx.t.tbool)) p) [] ctx.t.tbool p in
	let enext = mk (TVar (v,Some (make_call ctx (mk (TField (eit,quick_field_dynamic eit.etype "next")) (TFun ([],v.v_type)) p) [] v.v_type p))) ctx.t.tvoid p in
	let eblock = (match e2.eexpr with
		| TBlock el -> { e2 with eexpr = TBlock (enext :: el) }
		| _ -> mk (TBlock [enext;e2]) ctx.t.tvoid p
	) in
	mk (TBlock [
		mk (TVar (tmp,Some e1)) ctx.t.tvoid p;
		mk (TWhile (ehasnext,eblock,NormalWhile)) ctx.t.tvoid p
	]) ctx.t.tvoid p

let type_for_loop ctx handle_display it e2 p =
	let rec loop_ident dko e1 = match e1 with
		| EConst(Ident i),p -> i,p,dko
		| EDisplay(e1,dk),_ -> loop_ident (Some dk) e1
		| _ -> error "Identifier expected" (pos e1)
	in
	let rec loop dko e1 = match fst e1 with
		| EBinop(OpIn,e1,e2) -> loop_ident dko e1,e2
		| EDisplay(e1,dk) -> loop (Some dk) e1
		| _ -> error "For expression should be 'v in expr'" (snd it)
	in
	let (i, pi, dko), e1 = loop None it in
	let e1 = type_expr ctx e1 Value in
	let old_loop = ctx.in_loop in
	let old_locals = save_locals ctx in
	ctx.in_loop <- true;
	let e2 = Expr.ensure_block e2 in
	let default() =
		let t, pt = Typeload.t_iterator ctx in
		let i = add_local ctx i pt pi in
		let e1 = (match follow e1.etype with
		| TMono _
		| TDynamic _ ->
			display_error ctx "You can't iterate on a Dynamic value, please specify Iterator or Iterable" e1.epos;
			e1
		| TLazy _ ->
			assert false
		| _ ->
			(try
				AbstractCast.cast_or_unify_raise ctx t e1 p
			with Error (Unify _,_) ->
				let acc = !build_call_ref ctx (type_field ctx e1 "iterator" e1.epos MCall) [] Value e1.epos in
				try
					unify_raise ctx acc.etype t acc.epos;
					acc
				with Error (Unify(l),p) ->
					display_error ctx "Field iterator has an invalid type" acc.epos;
					display_error ctx (error_msg (Unify l)) p;
					mk (TConst TNull) t_dynamic p
			)
		) in
		begin match dko with
		| None -> ()
		| Some dk -> ignore(handle_display ctx (EConst(Ident i.v_name),i.v_pos) dk (WithType i.v_type))
		end;
		let e2 = type_expr ctx e2 NoValue in
		(try optimize_for_loop_iterator ctx i e1 e2 p with Exit -> mk (TFor (i,e1,e2)) ctx.t.tvoid p)
	in
	let e = match optimize_for_loop ctx (i,pi) e1 e2 p with
		| Some e ->
			begin match dko with
			| None -> ()
			| Some dk -> ignore(handle_display ctx (EConst(Ident i),pi) dk Value);
			end;
			e
		| None -> default()
	in
	ctx.in_loop <- old_loop;
	old_locals();
	e
