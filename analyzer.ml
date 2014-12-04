open Ast
open Type
open Common

module IntMap = Map.Make(struct type t = int let compare a b = a - b end)

let s_expr = s_expr (s_type (print_context()))
let s_expr_pretty = s_expr_pretty "" (s_type (print_context()))
let debug e = print_endline (s_expr e)
let debug_pretty s e = Printf.printf "%s %s\n" s (s_expr_pretty e)

let flag_no_check = "no_check"
let flag_check = "check"
let flag_no_const_propagation = "no_const_propagation"
let flag_const_propagation = "const_propagation"
let flag_no_local_dce = "no_local_dce"
let flag_local_dce = "local_dce"
let flag_ignore = "ignore"
let flag_no_simplification = "no_simplification"
let flag_check_has_effect = "check_has_effect"
let flag_no_check_has_effect = "no_check_has_effect"

let has_analyzer_option meta s =
	try
		let rec loop ml = match ml with
			| (Meta.Analyzer,el,_) :: ml ->
				if List.exists (fun (e,p) ->
					match e with
						| EConst(Ident s2) when s = s2 -> true
						| _ -> false
				) el then
					true
				else
					loop ml
			| _ :: ml ->
				loop ml
			| [] ->
				false
		in
		loop meta
	with Not_found ->
		false

let rec get_type_meta t = match t with
	| TMono r ->
		begin match !r with
			| None -> raise Not_found
			| Some t -> get_type_meta t
		end
	| TLazy f ->
		get_type_meta (!f())
	| TInst(c,_) ->
		c.cl_meta
	| TEnum(en,_) ->
		en.e_meta
	| TAbstract(a,_) ->
		a.a_meta
	| TType(t,_) ->
		t.t_meta
	| TAnon _ | TFun _ | TDynamic _ ->
		raise Not_found

let type_has_analyzer_option t s =
	try
		has_analyzer_option (get_type_meta t) s
	with Not_found ->
		false

(*
	This module simplifies the AST by introducing temporary variables for complex expressions in many places.
	In particular, it ensures that no branching can occur in value-places so that we can later insert SSA PHI
	nodes without worrying about their placement.
*)
module Simplifier = struct
	let mk_block_context com gen_temp =
		let block_el = ref [] in
		let push e = block_el := e :: !block_el in
		let assign ev e =
			let mk_assign e2 = match e2.eexpr with
				| TBreak | TContinue | TThrow _ | TReturn _ -> e2
				| _ -> mk (TBinop(OpAssign,ev,e2)) e2.etype e2.epos
			in
			let rec loop e = match e.eexpr with
				| TBlock el ->
					begin match List.rev el with
						| e1 :: el ->
							let el = List.rev ((loop e1) :: el) in
							{e with eexpr = TBlock el}
						| _ ->
							mk_assign e
					end
				| TIf(e1,e2,eo) ->
					let e2 = loop e2 in
					let eo = match eo with None -> None | Some e3 -> Some (loop e3) in
					{e with eexpr = TIf(e1,e2,eo)}
				| TSwitch(e1,cases,edef) ->
					let cases = List.map (fun (el,e) ->
						let e = loop e in
						el,e
					) cases in
					let edef = match edef with None -> None | Some edef -> Some (loop edef) in
					{e with eexpr = TSwitch(e1,cases,edef)}
				| TTry(e1,catches) ->
					let e1 = loop e1 in
					let catches = List.map (fun (v,e) ->
						let e = loop e in
						v,e
					) catches in
					{e with eexpr = TTry(e1,catches)}
(* 				| TBinop(OpAssign,({eexpr = TLocal _} as e1),e2) ->
					push e;
					mk_assign e1 *)
(* 				| TBinop(OpAssignOp op,({eexpr = TLocal _} as e1),e2) ->
					push e;
					mk_assign e1 *)
				| TParenthesis e1 | TMeta(_, e1) ->
					loop e1 (* this is weird *)
				| _ ->
					mk_assign e
			in
			loop e
		in
		let declare_temp t eo p =
			let v = gen_temp t in
			v.v_meta <- [Meta.CompilerGenerated,[],p];
			let e_v = mk (TLocal v) t p in
			let declare e_init =
				let e = mk (TVar (v,e_init)) com.basic.tvoid p in
				push e;
			in
			begin match eo with
				| None ->
					declare None
				| Some e1 ->
					begin match e1.eexpr with
						| TThrow _ | TReturn _ | TBreak | TContinue ->
							()
						| _ ->
							let e1 = assign e_v e1 in
							begin match e1.eexpr with
								| TBinop(OpAssign,{eexpr = TLocal v1},e2) when v == v1 ->
									declare (Some e2)
								| _ ->
									declare None;
									push e1
							end
					end
			end;
			e_v
		in
		let rec push_block () =
			let cur = !block_el in
			block_el := [];
			fun () ->
				let added = !block_el in
				block_el := cur;
				List.rev added
		and block f el =
			let close = push_block() in
			List.iter (fun e ->
				push (f e)
			) el;
			close()
		in
		block,declare_temp,fun () -> !block_el

	let apply com gen_temp e =
		let block,declare_temp,close_block = mk_block_context com gen_temp in
		let skip_binding ?(allow_tlocal=false) e =
			let rec loop e =
				match e.eexpr with
				| TConst _ | TTypeExpr _ | TFunction _ -> ()
				| TLocal _ when allow_tlocal -> ()
				| TParenthesis e1 | TCast(e1,None) | TEnumParameter(e1,_,_) -> Type.iter loop e
				| TField(_,(FStatic(c,cf) | FInstance(c,_,cf))) when has_analyzer_option cf.cf_meta flag_no_simplification || has_analyzer_option c.cl_meta flag_no_simplification -> ()
				| TField({eexpr = TLocal _},_) when allow_tlocal -> ()
				| TCall({eexpr = TField(_,(FStatic(c,cf) | FInstance(c,_,cf)))},el) when has_analyzer_option cf.cf_meta flag_no_simplification || has_analyzer_option c.cl_meta flag_no_simplification -> ()
				| _ -> raise Exit
			in
			try
				loop e;
				true
			with Exit ->
				begin match follow e.etype with
					| TAbstract({a_path = [],"Void"},_) -> true
					| _ -> false
				end
		in
		let has_unbound = ref false in
		let rec loop e = match e.eexpr with
			| TCall({eexpr = TLocal v | TField({eexpr = TLocal v},_)},_) | TField({eexpr = TLocal v},_) | TLocal v when Meta.has Meta.Unbound v.v_meta && v.v_name <> "`trace" ->
				has_unbound := true;
				e
			| TBlock el ->
				{e with eexpr = TBlock (block loop el)}
			| TCall({eexpr = TField(_,(FStatic(c,cf) | FInstance(c,_,cf)))},el) when has_analyzer_option cf.cf_meta flag_no_simplification || has_analyzer_option c.cl_meta flag_no_simplification ->
				e
			| TField(_,(FStatic(c,cf) | FInstance(c,_,cf))) when has_analyzer_option cf.cf_meta flag_no_simplification || has_analyzer_option c.cl_meta flag_no_simplification ->
				e
			| TCall(e1,el) ->
				let e1 = loop e1 in
				let check e t =
					if type_has_analyzer_option t flag_no_simplification then e
					else bind e
				in
				let el = match e1.eexpr,follow e1.etype with
					| TConst TSuper,_ when com.platform = Java || com.platform = Cs ->
						(* they hate you if you mess up the super call *)
						el
					| _,TFun _ ->
						Codegen.UnificationCallback.check_call check el e1.etype
					| _ ->
						(* too dangerous *)
						List.map loop el
				in
				{e with eexpr = TCall(e1,el)}
			| TNew(c,tl,el) ->
				{e with eexpr = TNew(c,tl,ordered_list el)}
			| TArrayDecl el ->
				{e with eexpr = TArrayDecl (ordered_list el)}
			| TObjectDecl fl ->
				let el = ordered_list (List.map snd fl) in
				{e with eexpr = TObjectDecl (List.map2 (fun (n,_) e -> n,e) fl el)}
			| TBinop(OpBoolAnd | OpBoolOr as op,e1,e2) ->
				let e1 = loop e1 in
				let e_then = mk (TBlock (block loop [e2])) e2.etype e2.epos in
				let e_if,e_else = if op = OpBoolOr then
					mk (TUnop(Not,Prefix,e1)) com.basic.tbool e.epos,mk (TConst (TBool(true))) com.basic.tbool e.epos
				else
					e1,mk (TConst (TBool(false))) com.basic.tbool e.epos
				in
				loop (mk (TIf(e_if,e_then,Some e_else)) com.basic.tbool e.epos)
			| TBinop((OpAssign | OpAssignOp _) as op,{eexpr = TArray(e11,e12)},e2) ->
				let e1 = match ordered_list [e11;e12] with
					| [e1;e2] ->
						{e with eexpr = TArray(e1,e2)}
					| _ ->
						assert false
				in
				let e2 = loop e2 in
				{e with eexpr = TBinop(op,e1,e2)}
			| TBinop((OpAssign | OpAssignOp _) as op,e1,e2) ->
				let e2 = bind ~allow_tlocal:true e2 in
				let e1 = loop e1 in
				{e with eexpr = TBinop(op,e1,e2)}
			| TBinop(op,e1,e2) ->
				begin match ordered_list [e1;e2] with
					| [e1;e2] ->
						{e with eexpr = TBinop(op,e1,e2)}
					| _ ->
						assert false
				end
			| TArray(e1,e2) ->
				begin match ordered_list [e1;e2] with
					| [e1;e2] ->
						{e with eexpr = TArray(e1,e2)}
					| _ ->
						assert false
				end
			| TWhile(e1,e2,flag) when (match e1.eexpr with TConst(TBool true) | TParenthesis {eexpr = TConst(TBool true)} -> false | _ -> true) ->
				let p = e.epos in
				let e_break = mk TBreak t_dynamic p in
				let e_not = mk (TUnop(Not,Prefix,Codegen.mk_parent e1)) e1.etype e1.epos in
				let e_if eo = mk (TIf(e_not,e_break,eo)) com.basic.tvoid p in
				let rec map_continue e = match e.eexpr with
					| TContinue ->
						(e_if (Some e))
					| TWhile _ | TFor _ ->
						e
					| _ ->
						Type.map_expr map_continue e
				in
				let e2 = if flag = NormalWhile then e2 else map_continue e2 in
				let e_if = e_if None in
				let e_if = mk (TMeta((Meta.Custom ":whileCond",[],e_if.epos), e_if)) e_if.etype e_if.epos in
				let e_block = if flag = NormalWhile then Type.concat e_if e2 else Type.concat e2 e_if in
				let e_true = mk (TConst (TBool true)) com.basic.tbool p in
				let e = mk (TWhile(Codegen.mk_parent e_true,e_block,NormalWhile)) e.etype p in
				loop e
			| TFor(v,e1,e2) ->
				let e1 = bind e1 in
				let e2 = loop e2 in
				{e with eexpr = TFor(v,e1,e2)}
			| TIf(e1,e2,eo) ->
				let e1 = bind e1 in
				let e2 = loop e2 in
				let eo = match eo with None -> None | Some e -> Some (loop e) in
				{e with eexpr = TIf(e1,e2,eo)}
			| TVar(v,Some e1) ->
				let e1 = match e1.eexpr with
					| TFunction _ -> loop e1
					| TArrayDecl [{eexpr = TFunction _}] -> loop e1
					| _ -> bind ~allow_tlocal:true e1
				in
				{e with eexpr = TVar(v,Some e1)}
			| TUnop((Neg | NegBits | Not) as op,flag,e1) ->
				let e1 = bind e1 in
				{e with eexpr = TUnop(op,flag,e1)}
			| TField(e1,fa) ->
				let e1 = bind ~allow_tlocal:true e1 in
				{e with eexpr = TField(e1,fa)}
			| TReturn (Some ({eexpr = TThrow _ | TReturn _} as e1)) ->
				loop e1 (* this is a bit hackish *)
			| TReturn (Some e1) ->
				let e1 = bind e1 in
				{e with eexpr = TReturn (Some e1)}
			| TThrow e1 ->
				let e1 = bind e1 in
				{e with eexpr = TThrow e1}
			| TCast(e1,mto) ->
				let e1 = bind ~allow_tlocal:true e1 in
				{e with eexpr = TCast(e1,mto)}
			| _ ->
				Type.map_expr loop e
		and bind ?(allow_tlocal=false) e =
			let e = loop e in
			if skip_binding ~allow_tlocal e then
				e
			else
				declare_temp e.etype (Some e) e.epos
		and ordered_list el =
			if List.for_all (skip_binding ~allow_tlocal:true) el then
				List.map loop el
			else
				List.map bind el
		in
		let e = loop e in
		!has_unbound,match close_block() with
			| [] ->
				e
			| el ->
				mk (TBlock (List.rev (e :: el))) e.etype e.epos

	let unapply com e =
		let var_map = ref IntMap.empty in
		let rec get_assignment_to v e = match e.eexpr with
			| TBinop(OpAssign,{eexpr = TLocal v2},e2) when v == v2 -> Some e2
			| TBlock [e] -> get_assignment_to v e
			| _ -> None
		in
		let rec loop e = match e.eexpr with
			| TBlock el ->
				let rec loop2 el = match el with
					| e :: el ->
						begin match e.eexpr with
							| TVar(v,Some e1) when Meta.has Meta.CompilerGenerated v.v_meta ->
								var_map := IntMap.add v.v_id (loop e1) !var_map;
								loop2 el
							| TVar(v,None) when not (com.platform = Php) ->
								begin match el with
									| {eexpr = TBinop(OpAssign,{eexpr = TLocal v2},e2)} :: el when v == v2 ->
										let e = {e with eexpr = TVar(v,Some e2)} in
										loop2 (e :: el)
									| ({eexpr = TIf(e1,e2,Some e3)} as e_if) :: el ->
										let e1 = loop e1 in
										let e2 = loop e2 in
										let e3 = loop e3 in
										begin match get_assignment_to v e2,get_assignment_to v e3 with
											| Some e2,Some e3 ->
												let e_if = {e_if with eexpr = TIf(e1,e2,Some e3)} in
												let e = {e with eexpr = TVar(v,Some e_if)} in
												loop2 (e :: el)
											| _ ->
												let e_if = {e_if with eexpr = TIf(e1,e2,Some e3)} in
												e :: e_if :: loop2 el
										end
									| _ ->
										let e = loop e in
										e :: loop2 el
								end
							| _ ->
								let e = loop e in
								e :: loop2 el
						end
					| [] ->
						[]
				in
				let el = loop2 el in
				{e with eexpr = TBlock el}
			| TLocal v when Meta.has Meta.CompilerGenerated v.v_meta ->
				begin try IntMap.find v.v_id !var_map
				with Not_found -> e end
			| TWhile(e1,e2,flag) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				let extract_cond e = match e.eexpr with
					| TIf({eexpr = TUnop(Not,_,e1)},_,_) -> e1
					| TBreak -> raise Exit (* can happen due to optimization, not so easy to deal with because there might be other breaks/continues *)
					| _ -> assert false
				in
				let e1,e2,flag = try
					begin match e2.eexpr with
						| TBlock el ->
							begin match el with
								| {eexpr = TMeta((Meta.Custom ":whileCond",_,_),e1)} :: el ->
									let e1 = extract_cond e1 in
									e1,{e2 with eexpr = TBlock el},NormalWhile
								| _ ->
									begin match List.rev el with
										| {eexpr = TMeta((Meta.Custom ":whileCond",_,_),e1)} :: el ->
											let e1 = extract_cond e1 in
											e1,{e2 with eexpr = TBlock (List.rev el)},DoWhile
										| _ ->
											e1,e2,flag
									end
							end
						| _ ->
							e1,e2,flag
					end with Exit ->
						e1,e2,flag
				in
				{e with eexpr = TWhile(e1,e2,flag)}
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module Ssa = struct

	type var_map = tvar IntMap.t

	type condition =
		| Equal of tvar * texpr
		| NotEqual of tvar * texpr

	type node_data = {
		nd_pos: pos;
		mutable nd_var_map : var_map;
		mutable nd_terminates : bool;
	}

	type join_node = {
		mutable branches : node_data list;
	}

	type ssa_context = {
		com : Common.context;
		mutable cleanup : (unit -> unit) list;
		mutable cur_data : node_data;
		mutable var_values : texpr IntMap.t;
		mutable var_conds : (condition list) IntMap.t;
		mutable loop_stack : (join_node * join_node) list;
		mutable exception_stack : join_node list;
	}

	let s_cond = function
		| Equal(v,e) -> Printf.sprintf "%s == %s" v.v_name (s_expr_pretty e)
		| NotEqual(v,e) -> Printf.sprintf "%s != %s" v.v_name (s_expr_pretty e)

	let s_conds conds =
		String.concat " && " (List.map s_cond conds)

	let mk_loc v p = mk (TLocal v) v.v_type p

	let mk_phi =
		let v_phi = alloc_var "__ssa_phi__" t_dynamic in
		(fun vl p ->
			let e = mk (TCall(mk_loc v_phi p,(List.map (fun (v,p) -> mk_loc v p) vl))) t_dynamic p in
			e
		)

	(* TODO: make sure this is conservative *)
	let can_throw e =
		let rec loop e = match e.eexpr with
			| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ | TBlock _ -> ()
			| TCall _ | TNew _ | TThrow _ | TCast(_,Some _) -> raise Exit
			| _ -> Type.iter loop e
		in
		try
			loop e; false
		with Exit ->
			true

	let mk_join_node() = {
		branches = []
	}

	let mk_node_data p = {
		nd_pos = p;
		nd_var_map = IntMap.empty;
		nd_terminates = false;
	}

	let add_branch join branch p =
		join.branches <- {branch with nd_pos = p} :: join.branches

	let branch ctx p =
		let old_map = ctx.cur_data.nd_var_map in
		let old_term = ctx.cur_data.nd_terminates in
		ctx.cur_data.nd_terminates <- false;
		(fun join ->
			add_branch join ctx.cur_data p;
			ctx.cur_data.nd_var_map <- old_map;
			ctx.cur_data.nd_terminates <- old_term;
		)

	let terminate ctx =
		ctx.cur_data.nd_terminates <- true

	let set_loop_join ctx join_top join_bottom =
		ctx.loop_stack <- (join_top,join_bottom) :: ctx.loop_stack;
		(fun () ->
			ctx.loop_stack <- List.tl ctx.loop_stack
		)

	let set_exception_join ctx join =
		ctx.exception_stack <- join :: ctx.exception_stack;
		(fun () ->
			ctx.exception_stack <- List.tl ctx.exception_stack;
		)

	let declare_var ctx v p =
		let old = v.v_extra in
		ctx.cleanup <- (fun () ->
			v.v_extra <- old
		) :: ctx.cleanup;
		ctx.cur_data.nd_var_map <- IntMap.add v.v_id v ctx.cur_data.nd_var_map;
		v.v_extra <- Some ([],(Some (mk_loc v p)))

	let assign_var ctx v e p =
		if v.v_capture then
			v
		else begin
			let i = match v.v_extra with
				| Some (l,eo) ->
					v.v_extra <- Some (("",t_dynamic) :: l,eo);
					List.length l + 1
				| _ ->
					error "Something went wrong" p
			in
			let v' = alloc_var (Printf.sprintf "%s<%i>" v.v_name i) v.v_type in
			v'.v_meta <- [(Meta.Custom ":ssa"),[],p];
			v'.v_extra <- Some ([],(Some (mk_loc v p)));
			ctx.cur_data.nd_var_map <- IntMap.add v.v_id v' ctx.cur_data.nd_var_map;
			ctx.var_values <- IntMap.add v'.v_id e ctx.var_values;
			v'
		end

	let get_var ctx v p =
		try
			IntMap.find v.v_id ctx.cur_data.nd_var_map
		with Not_found ->
			if not (has_meta Meta.Unbound v.v_meta) then
				ctx.com.warning (Printf.sprintf "Unbound variable %s" v.v_name) p;
			v

	let close_join_node ctx node p =
		let terminates = ref true in
		let branches = List.filter (fun branch ->
			if branch.nd_terminates then false
			else begin
				terminates := false;
				true
			end
		) node.branches in
		match branches with
			| [] ->
				()
			| branch :: branches ->
				let vars = ref (IntMap.map (fun v -> [v,branch.nd_pos]) branch.nd_var_map) in
				let rec handle_branch branch =
					IntMap.iter (fun i v ->
						try
							let vl = IntMap.find i !vars in
							if not (List.exists (fun (v',_) -> v == v') vl) then
								vars := IntMap.add i ((v,p) :: vl) !vars
						with Not_found ->
							()
					) branch.nd_var_map;
				in
				List.iter handle_branch branches;
				ctx.cur_data.nd_terminates <- !terminates;
				IntMap.iter (fun i vl -> match vl with
					| [v,p] ->
						ctx.cur_data.nd_var_map <- IntMap.add i v ctx.cur_data.nd_var_map;
					| ({v_extra = Some (_,Some {eexpr = TLocal v})},p) :: _ ->
						ignore(assign_var ctx v (mk_phi vl p) p)
					| _ ->
						assert false
				) !vars

	let invert_cond = function
		| Equal(v,e) -> NotEqual(v,e)
		| NotEqual(v,e) -> Equal(v,e)

	let invert_conds =
		List.map invert_cond

	let rec eval_cond ctx e = match e.eexpr with
		| TBinop(OpNotEq,{eexpr = TLocal v},e1) ->
			[NotEqual(v,e1)]
		| TBinop(OpEq,{eexpr = TLocal v},e1) ->
			[Equal(v,e1)]
		| TUnop(Not,_,e1) ->
			invert_conds (eval_cond ctx e1)
		| TLocal v ->
			begin try eval_cond ctx (IntMap.find v.v_id ctx.var_values)
			with Not_found -> [] end
		| _ ->
			[]

	let append_cond ctx v cond p =
		begin try
			let conds = IntMap.find v.v_id ctx.var_conds in
			ctx.var_conds <- IntMap.add v.v_id (cond :: conds) ctx.var_conds
		with Not_found ->
			ctx.var_conds <- IntMap.add v.v_id [cond] ctx.var_conds
		end

	let apply_cond ctx = function
		| Equal({v_extra = Some(_,Some {eexpr = TLocal v})} as v0,e1) ->
			let v' = assign_var ctx v (mk_loc v0 e1.epos) e1.epos in
			append_cond ctx v' (Equal(v',e1)) e1.epos
		| NotEqual({v_extra = Some(_,Some {eexpr = TLocal v})} as v0,e1) ->
			let v' = assign_var ctx v (mk_loc v0 e1.epos) e1.epos in
			append_cond ctx v' (NotEqual(v',e1)) e1.epos
		| _ -> ()

	let apply_not_null_cond ctx v p =
		apply_cond ctx (NotEqual(v,(mk (TConst TNull) t_dynamic p)))

	let apply com e =
		let rec handle_if ctx e econd eif eelse =
			let econd = loop ctx econd in
			let cond = eval_cond ctx econd in
			let join = mk_join_node() in
			let close = branch ctx eif.epos in
			List.iter (apply_cond ctx) cond;
			let eif = loop ctx eif in
			close join;
			let eelse = match eelse with
				| None ->
					let cond = invert_conds cond in
					List.iter (apply_cond ctx) cond;
					add_branch join ctx.cur_data e.epos;
					None
				| Some e ->
					let close = branch ctx e.epos in
					let cond = invert_conds cond in
					List.iter (apply_cond ctx) cond;
					let eelse = loop ctx e in
					close join;
					Some eelse
			in
			close_join_node ctx join e.epos;
			let e = {e with eexpr = TIf(econd,eif,eelse)} in
			e
		and handle_loop_body ctx e =
			let join_top = mk_join_node() in
			let join_bottom = mk_join_node() in
			let unset = set_loop_join ctx join_top join_bottom in
			let close = branch ctx e.epos in
			ignore(loop ctx e); (* TODO: I don't know if this is sane. *)
			close join_top;
			add_branch join_top ctx.cur_data e.epos;
			close_join_node ctx join_top e.epos;
			let ebody = loop ctx e in
			ctx.cur_data.nd_terminates <- false;
			unset();
			close_join_node ctx join_bottom e.epos;
			ebody
		and loop ctx e = match e.eexpr with
			(* var declarations *)
			| TVar(v,eo) ->
				declare_var ctx v e.epos;
				let eo = match eo with
					| None -> None
					| Some e ->
						let e = loop ctx e in
						ctx.var_values <- IntMap.add v.v_id e ctx.var_values;
						Some e
				in
				{e with eexpr = TVar(v,eo)}
			| TFunction tf ->
				let close = branch ctx e.epos in
				List.iter (fun (v,co) ->
					declare_var ctx v e.epos;
					match co with
						| Some TNull when (match v.v_type with TType({t_path=["haxe"],"PosInfos"},_) -> false | _ -> true) -> ()
						| _ -> apply_not_null_cond ctx v e.epos
				) tf.tf_args;
				let e' = loop ctx tf.tf_expr in
				close (mk_join_node());
				{e with eexpr = TFunction {tf with tf_expr = e'}}
			(* var modifications *)
			| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) when v.v_name <> "this" ->
				let e2 = loop ctx e2 in
				let _ = assign_var ctx v e2 e1.epos in
				{e with eexpr = TBinop(OpAssign,e1,e2)}
			| TBinop(OpAssignOp op,({eexpr = TLocal v} as e1),e2) ->
				let e1 = loop ctx e1 in
				let e_op = mk (TBinop(op,e1,e2)) e.etype e.epos in
				let _ = assign_var ctx v e_op e1.epos in
				let e2 = loop ctx e2 in
				{e with eexpr = TBinop(OpAssignOp op,e1,e2)}
			| TUnop((Increment | Decrement as op),flag,({eexpr = TLocal v} as e1)) ->
				let op = match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false in
				let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e.epos in
				let e1 = loop ctx e1 in
				let e_op = mk (TBinop(op,e1,e_one)) e.etype e.epos in
				let _ = assign_var ctx v e_op e1.epos in
				e
			(* var user *)
			| TLocal v ->
				let v = get_var ctx v e.epos in
				{e with eexpr = TLocal v}
			(* control flow *)
			| TIf(econd,eif,eelse) ->
				handle_if ctx e econd eif eelse
			| TSwitch(e1,cases,edef) ->
				let e1 = loop ctx e1 in
				let join = mk_join_node() in
				let cases = List.map (fun (el,e) ->
					let close = branch ctx e.epos in
					let el = List.map (loop ctx) el in
					let e = loop ctx e in
					close join;
					el,e
				) cases in
				let edef = match edef with
					| Some e ->
						let close = branch ctx e.epos in
						let e = loop ctx e in
						close join;
						Some e
					| None ->
						begin match e1.eexpr with
							| TMeta((Meta.Exhaustive,_,_),_)
							| TParenthesis({eexpr = TMeta((Meta.Exhaustive,_,_),_)}) ->
								()
							| _ ->
								add_branch join ctx.cur_data e.epos;
						end;
						None
				in
				close_join_node ctx join e.epos;
				let e = {e with eexpr = TSwitch(e1,cases,edef)} in
				e
			| TWhile(econd,ebody,mode) ->
				let econd = loop ctx econd in
				let ebody = handle_loop_body ctx ebody in
				let e = {e with eexpr = TWhile(econd,ebody,mode)} in
				e
			| TFor(v,e1,ebody) ->
				declare_var ctx v e.epos;
				apply_not_null_cond ctx v e1.epos;
				let v' = IntMap.find v.v_id ctx.cur_data.nd_var_map in
				let e1 = loop ctx e1 in
				let ebody = handle_loop_body ctx ebody in
				let e = {e with eexpr = TFor(v',e1,ebody)} in
				e
			| TTry(e1,catches) ->
				let join_ex = mk_join_node() in
				let join_bottom = mk_join_node() in
				let unset = set_exception_join ctx join_ex in
				let e1 = loop ctx e1 in
				unset();
				add_branch join_bottom ctx.cur_data e.epos;
				close_join_node ctx join_ex e.epos;
				let catches = List.map (fun (v,e) ->
					declare_var ctx v e.epos;
					apply_not_null_cond ctx v e.epos;
					let close = branch ctx e.epos in
					let e = loop ctx e in
					close join_bottom;
					v,e
				) catches in
				close_join_node ctx join_bottom e.epos;
				let e = {e with eexpr = TTry(e1,catches)} in
				e
			| TBreak ->
				begin match ctx.loop_stack with
					| [] -> error "Break outside loop" e.epos
					| (_,join) :: _ -> add_branch join ctx.cur_data e.epos
				end;
				terminate ctx;
				e
			| TContinue ->
				begin match ctx.loop_stack with
					| [] -> error "Continue outside loop" e.epos
					| (join,_) :: _ -> add_branch join ctx.cur_data e.epos
				end;
				terminate ctx;
				e
			| TThrow e1 ->
				let e1 = loop ctx e1 in
				begin match ctx.exception_stack with
					| join :: _ -> add_branch join ctx.cur_data e.epos
					| _ -> ()
				end;
				terminate ctx;
				{e with eexpr = TThrow e1}
			| TReturn eo ->
				let eo = match eo with None -> None | Some e -> Some (loop ctx e) in
				terminate ctx;
				{e with eexpr = TReturn eo}
			| TBlock el ->
				let rec loop2 el = match el with
					| [] ->
						[]
					| e :: el ->
						if ctx.cur_data.nd_terminates then begin
							ctx.com.warning (Printf.sprintf "Unreachable code: %s" (s_expr_pretty e)) e.epos;
							[]
						end else
							let e = loop ctx e in
							e :: (loop2 el)
				in
				{e with eexpr = TBlock(loop2 el)}
			| _ ->
				begin match ctx.exception_stack with
					| join :: _ when can_throw e -> add_branch join ctx.cur_data e.epos
					| _ -> ()
				end;
				Type.map_expr (loop ctx) e
		in
		let ctx = {
			com = com;
			cur_data = mk_node_data e.epos;
			var_values = IntMap.empty;
			var_conds = IntMap.empty;
			loop_stack = [];
			exception_stack = [];
			cleanup = [];
		} in
		let e = loop ctx e in
		e,ctx

	let unapply com e =
		let rec loop e = match e.eexpr with
			| TFor(({v_extra = Some([],Some {eexpr = TLocal v'})} as v),e1,e2) when Meta.has (Meta.Custom ":ssa") v.v_meta ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				{e with eexpr = TFor(v',e1,e2)}
			| TLocal ({v_extra = Some([],Some {eexpr = TLocal v'})} as v) when Meta.has (Meta.Custom ":ssa") v.v_meta ->
				{e with eexpr = TLocal v'}
			| TBlock el ->
				let rec filter e = match e.eexpr with
					| TMeta((Meta.Custom ":ssa",_,_),_) ->
						false
					| _ ->
						true
				in
				let el = List.filter filter el in
				let el = List.map loop el in
				{e with eexpr = TBlock el}
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module ConstPropagation = struct
	open Ssa

	let expr_eq e1 e2 = match e1.eexpr,e2.eexpr with
		| TConst ct1, TConst ct2 ->
			ct1 = ct2
		| _ ->
			false

	let can_be_inlined com e = match e.eexpr with
		| TConst ct ->
			begin match ct with
				| TThis | TSuper -> false
				(* Some targets don't like seeing null in certain places and won't even compile. We have to detect `if (x != null)
				   in order for this to work. *)
				| TNull when (match com.platform with Php | Cpp -> true | _ -> false) -> false
				| _ -> true
			end
		| _ ->
			false

	let is_enum_type t = match follow t with
		| TEnum(_) -> true
		| _ -> false

	let rec local ssa v e =
		begin try
			if v.v_capture then raise Not_found;
			if type_has_analyzer_option v.v_type flag_no_const_propagation then raise Not_found;
			begin match follow v.v_type with
				| TDynamic _ -> raise Not_found
				| _ -> ()
			end;
			let e = IntMap.find v.v_id ssa.var_values in
			let reset() =
				ssa.var_values <- IntMap.add v.v_id e ssa.var_values;
			in
			ssa.var_values <- IntMap.remove v.v_id ssa.var_values;
			let e = value ssa e in
			reset();
			e
		with Not_found ->
			e
		end

	and value ssa e = match e.eexpr with
		| TUnop((Increment | Decrement),_,_)
		| TBinop(OpAssignOp _,_,_)
		| TBinop(OpAssign,_,_) ->
			e
		| TBinop(op,e1,e2) ->
			let e1 = value ssa e1 in
			let e2 = value ssa e2 in
			let e = {e with eexpr = TBinop(op,e1,e2)} in
			let e' = Optimizer.optimize_binop e op e1 e2 in
			if e == e' then
				e
			else
				value ssa e'
		| TUnop(op,flag,e1) ->
			let e1 = value ssa e1 in
			let e = {e with eexpr = TUnop(op,flag,e1)} in
			let e' = Optimizer.optimize_unop e op flag e1 in
			if e == e' then
				e
			else
				value ssa e'
		| TCall ({eexpr = TLocal {v_name = "__ssa_phi__"}},el) ->
			let el = List.map (value ssa) el in
			begin match el with
				| [] -> assert false
				| e1 :: el ->
					if List.for_all (fun e2 -> expr_eq e1 e2) el then
						value ssa e1
					else
						e
			end
		| TParenthesis e1 | TMeta(_,e1) ->
			value ssa e1
		| TLocal v ->
			local ssa v e
		| _ ->
			e

	(* TODO: the name is quite accurate *)
	let awkward_get_enum_index ssa e =
		let e = match e.eexpr with
			| TArray(e1,{eexpr = TConst(TInt i)}) when ssa.com.platform = Js && Int32.to_int i = 1 && is_enum_type e1.etype ->
				e1
			| TCall({eexpr = TField(e1, FDynamic "__Index")},[]) when ssa.com.platform = Cpp && is_enum_type e1.etype ->
				e1
			| TField(e1,FDynamic "index") when ssa.com.platform = Neko && is_enum_type e1.etype ->
				e1
			| _ ->
				raise Not_found
		in
		match (value ssa e).eexpr with
			| TField(_,FEnum(_,ef)) -> TInt (Int32.of_int ef.ef_index)
			| _ -> raise Not_found

	let apply ssa e =
		let had_function = ref false in
		let rec loop e = match e.eexpr with
			| TFunction _ when !had_function ->
				e
			| TFunction tf ->
				had_function := true;
				{e with eexpr = TFunction {tf with tf_expr = loop tf.tf_expr}}
			| TLocal v ->
				let e' = local ssa v e in
				if can_be_inlined ssa.com e' then
					e'
				else
					e
			| TCall({eexpr = TField(_,(FStatic(_,cf) | FInstance(_,_,cf) | FAnon cf))},el) when has_analyzer_option cf.cf_meta flag_no_const_propagation ->
				e
			| TCall(e1,el) ->
				let e1 = loop e1 in
				let check e t =
					if type_has_analyzer_option t flag_no_const_propagation then e
					else loop e
				in
				let el = Codegen.UnificationCallback.check_call check el e1.etype in
				{e with eexpr = TCall(e1,el)}
			| TUnop((Increment | Decrement),_,_) ->
				e
			| TBinop(OpAssignOp op,e1,e2) ->
				let e2 = loop e2 in
				{e with eexpr = TBinop(OpAssignOp op,e1,e2)}
			| TBinop(OpAssign,({eexpr = TLocal _} as e1),e2) ->
				let e2 = loop e2 in
				{e with eexpr = TBinop(OpAssign,e1,e2)}
			| TBinop(op,e1,e2) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				let e = {e with eexpr = TBinop(op,e1,e2)} in
				let e' = Optimizer.optimize_binop e op e1 e2 in
				e'
			| TUnop(op,flag,e1) ->
				let e1 = loop e1 in
				let e = {e with eexpr = TUnop(op,flag,e1)} in
				let e' = Optimizer.optimize_unop e op flag e1 in
				e'
			| TIf(e1,e2,eo) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				begin match e1.eexpr with
					| TConst (TBool true) ->
						e2
					| TConst (TBool false) ->
						begin match eo with
							| None ->
								mk (TConst TNull) t_dynamic e.epos
							| Some e ->
								loop e
						end
					| _ ->
						let eo = match eo with None -> None | Some e -> Some (loop e) in
						{e with eexpr = TIf(e1,e2,eo)}
				end;
			| TSwitch(e1,cases,edef) ->
				let e1 = loop e1 in
				let rec check_constant e = match e.eexpr with
					| TConst ct -> ct
					| TParenthesis e1 | TCast(e1,None) | TMeta(_,e1) -> check_constant e1
					| _ -> awkward_get_enum_index ssa e
				in
				begin try
					let ct = check_constant e1 in
					begin try
						let _,e = List.find (fun (el,_) ->
							List.exists (fun e -> match e.eexpr with
								| TConst ct2 -> ct = ct2
								| _ -> false
							) el
						) cases in
						loop e
					with Not_found ->
						begin match edef with None -> raise Not_found | Some e -> loop e end
					end
				with Not_found ->
					let cases = List.map (fun (el,e) -> el,loop e) cases in
					let edef = match edef with None -> None | Some e -> Some (loop e) in
					{e with eexpr = TSwitch(e1,cases,edef)}
				end
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module EffectChecker = struct
	let run com is_var_expression e =
		let has_effect e = match e.eexpr with
			| TVar _ -> true
			| _ -> Optimizer.has_side_effect e
		in
		let e = if is_var_expression then
			(* var initialization expressions are like assignments, so let's cheat a bit here *)
			snd (Simplifier.apply com (alloc_var "tmp") (Codegen.binop OpAssign (mk (TConst TNull) t_dynamic e.epos) e e.etype e.epos))
		else e
		in
		let rec loop e = match e.eexpr with
			| TBlock el ->
				List.iter (fun e ->
					if not (has_effect e) then com.warning "This expression has no effect" e.epos
				) el
			| _ ->
				Type.iter loop e
			in
		loop e
end

module Checker = struct
	open Ssa

	let apply ssa e =
		let given_warnings = ref PMap.empty in
		let add_pos p =
			given_warnings := PMap.add p true !given_warnings
		in
		let resolve_value v =
			let e' = IntMap.find v.v_id ssa.var_values in
			begin match e'.eexpr with
				| TLocal v' when v == v' -> e'
				| _ -> e'
			end
		in
		let rec is_null_expr e = match e.eexpr with
			| TConst TNull ->
				true
			| TLocal v ->
				(try is_null_expr (resolve_value v) with Not_found -> false)
			| _ ->
				false
		in
		let can_be_null v =
			not (has_meta Meta.NotNull v.v_meta)
			&& try not (List.exists (fun cond -> match cond with
				| NotEqual(v',e) when v == v' && is_null_expr e -> true
				| _ -> false
			) (IntMap.find v.v_id ssa.var_conds)) with Not_found -> true
		in
		let return b p =
			if b then add_pos p;
			b
		in
		let rec can_be_null_expr vstack e =
			if PMap.mem e.epos !given_warnings then
				false
			else match e.eexpr with
			| TConst TNull ->
				add_pos e.epos;
				true
			| TBinop((OpAssign | OpAssignOp _),_,e1) ->
				can_be_null_expr vstack e1
			| TBinop _ | TUnop _ ->
				false
			| TConst _ | TTypeExpr _ | TNew _ | TObjectDecl _ | TArrayDecl _ | TEnumParameter _ | TFunction _ | TVar _ ->
				false
			| TFor _ | TWhile _ | TIf _ | TSwitch _ | TTry _ | TReturn _ | TBreak | TContinue | TThrow _ ->
				assert false
			| TField _ | TBlock _ | TArray _ ->
				false (* TODO *)
			| TCall ({eexpr = TLocal {v_name = "__ssa_phi__"}},el) ->
				List.exists (can_be_null_expr vstack) el
			| TLocal v ->
				if List.mem v.v_id vstack then
					false (* not really, but let's not be a nuisance *)
				else
					return (can_be_null v && (try can_be_null_expr (v.v_id :: vstack) (resolve_value v) with Not_found -> true)) e.epos;
			| TMeta(_,e1) | TParenthesis e1 | TCast(e1,_) ->
				can_be_null_expr vstack e1
			| TCall(e1,_) ->
				begin match follow e1.etype with
					| TFun(_,r) -> return (is_explicit_null r) e1.epos
					| _ -> false
				end
		in
		let check_null e p =
			if can_be_null_expr [] e then begin
				ssa.com.warning "Possible null exception" p;
			end
		in
		let rec loop e = match e.eexpr with
			| TField(e1,fa) ->
				let e1 = loop e1 in
				check_null e1 e.epos;
				{e with eexpr = TField(e1,fa)}
			| TMeta((Meta.Analyzer,[EConst(Ident "testIsNull"),_],_),e1) ->
				if not (can_be_null_expr [] e) then error "Analyzer did not find a possible null exception" e.epos;
				e
			| TMeta((Meta.Analyzer,[EConst(Ident "testIsNotNull"),_],_),e1) ->
				if (can_be_null_expr [] e) then error "Analyzer found a possible null exception" e.epos;
				e
			| _ ->
				Type.map_expr loop e
		in
		loop e;
end

module LocalDce = struct
	let apply e =
		let is_used v = Meta.has Meta.Used v.v_meta || type_has_analyzer_option v.v_type flag_no_local_dce in
		let use v = v.v_meta <- (Meta.Used,[],Ast.null_pos) :: v.v_meta in
		let has_side_effect e = match e.eexpr with
			| TVar(v,None) -> is_used v
			| TVar(v,Some e1) -> is_used v || Optimizer.has_side_effect e1
			| TBinop(OpAssign,{eexpr = TLocal v},e2) -> is_used v || Optimizer.has_side_effect e2
			| _ -> true
		in
		let rec collect e = match e.eexpr with
			| TLocal v ->
				use v
			| _ ->
				Type.iter collect e
		in
		let rec loop e = match e.eexpr with
			| TLocal v ->
				use v;
				e
			| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
				let e2 = loop e2 in
				if not (is_used v) then
					e2
				else
					{e with eexpr = TBinop(OpAssign,{e1 with eexpr = TLocal v},e2)}
			| TVar(v,Some e1) when not (is_used v) ->
				let e1 = loop e1 in
				e1
			| TWhile(e1,e2,flag) ->
				collect e2;
				let e2 = loop e2 in
				let e1 = loop e1 in
				{e with eexpr = TWhile(e1,e2,flag)}
			| TFor(v,e1,e2) ->
				collect e2;
				let e2 = loop e2 in
				let e1 = loop e1 in
				{e with eexpr = TFor(v,e1,e2)}
			| TBlock el ->
				let rec block el = match el with
					| e :: el ->
						let el = block el in
						if el <> [] && not (has_side_effect e) then
							el
						else begin
							let e = loop e in
							e :: el
						end
					| [] ->
						[]
				in
				{e with eexpr = TBlock (block el)}
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

type analyzer_config = {
	analyzer_use : bool;
	simplifier_apply : bool;
	ssa_apply : bool;
	const_propagation : bool;
	check : bool;
	check_has_effect : bool;
	local_dce : bool;
	ssa_unapply : bool;
	simplifier_unapply : bool;
}

let run_ssa com config is_var_expression e =
	let rec gen_local t =
		alloc_var "tmp" t
	in
	let do_simplify = (not (Common.defined com Define.NoSimplify) ) && match com.platform with
		| Cpp when Common.defined com Define.Cppia -> false
		| Cpp | Flash8 | Python -> true
		| _ -> false
	in
	let with_timer s f =
		let timer = timer s in
		let r = f() in
		timer();
		r
	in
	try
		let has_unbound,e = if do_simplify || config.analyzer_use then
			with_timer "analyzer-simplify-apply" (fun () -> Simplifier.apply com gen_local e)
		else
			false,e
		in
		let e = if config.analyzer_use && not has_unbound then begin
				if config.check_has_effect then EffectChecker.run com is_var_expression e;
				let e,ssa = with_timer "analyzer-ssa-apply" (fun () -> Ssa.apply com e) in
				let e = if config.const_propagation then with_timer "analyzer-const-propagation" (fun () -> ConstPropagation.apply ssa e) else e in
				(* let e = if config.check then with_timer "analyzer-checker" (fun () -> Checker.apply ssa e) else e in *)
				let e = if config.ssa_unapply then with_timer "analyzer-ssa-unapply" (fun () -> Ssa.unapply com e) else e in
				List.iter (fun f -> f()) ssa.Ssa.cleanup;
				e
		end else
			e
		in
		let e = if not do_simplify && not (Common.raw_defined com "analyzer-no-simplify-unapply") then
			with_timer "analyzer-simplify-unapply" (fun () -> Simplifier.unapply com e)
		else
			e
		in
		let e = if config.local_dce && config.analyzer_use && not has_unbound then with_timer "analyzer-local-dce" (fun () -> LocalDce.apply e) else e in
		e
	with Exit ->
		e

let update_config_from_meta config meta =
	List.fold_left (fun config meta -> match meta with
		| (Meta.Analyzer,el,_) ->
			List.fold_left (fun config e -> match fst e with
				| EConst (Ident s) when s = flag_no_check -> { config with check = false}
				| EConst (Ident s) when s = flag_check -> { config with check = true}
				| EConst (Ident s) when s = flag_no_const_propagation -> { config with const_propagation = false}
				| EConst (Ident s) when s = flag_const_propagation -> { config with const_propagation = true}
				| EConst (Ident s) when s = flag_no_local_dce -> { config with local_dce = false}
				| EConst (Ident s) when s = flag_local_dce -> { config with local_dce = true}
				| EConst (Ident s) when s = flag_no_check_has_effect -> { config with check_has_effect = false}
				| EConst (Ident s) when s = flag_check_has_effect -> { config with check_has_effect = true}
				| _ -> config
			) config el
		| _ ->
			config
	) config meta

let run_expression_filters com config t =
	match t with
	| TClassDecl c when (has_analyzer_option c.cl_meta flag_ignore) ->
		()
	| TClassDecl c ->
		let config = update_config_from_meta config c.cl_meta in
		let process_field cf =
			let is_var_expression = match cf.cf_kind with
				| Var _ -> true
				| _ -> false
			in
			match cf.cf_expr with
			| Some e when not (has_analyzer_option cf.cf_meta flag_ignore) && not (Meta.has Meta.Extern cf.cf_meta) (* TODO: use is_removable_field *) ->
				let config = update_config_from_meta config cf.cf_meta in
				cf.cf_expr <- Some (run_ssa com config is_var_expression e);
			| _ -> ()
		in
		List.iter process_field c.cl_ordered_fields;
		List.iter process_field c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> process_field f);
		(match c.cl_init with
		| None -> ()
		| Some e ->
			(* never optimize init expressions (too messy) *)
			c.cl_init <- Some (run_ssa com {config with analyzer_use = false} false e));
	| TEnumDecl _ -> ()
	| TTypeDecl _ -> ()
	| TAbstractDecl _ -> ()

let apply com =
	let config = {
		analyzer_use = true;
		simplifier_apply = true;
		ssa_apply = true;
		const_propagation = not (Common.raw_defined com "analyzer-no-const-propagation");
		check_has_effect = not (Common.raw_defined com "analyzer-no-check-has-effect");
		check = not (Common.raw_defined com "analyzer-no-check");
		local_dce = not (Common.raw_defined com "analyzer-no-local-dce");
		ssa_unapply = not (Common.raw_defined com "analyzer-no-ssa-unapply");
		simplifier_unapply = not (Common.raw_defined com "analyzer-no-simplify-unapply");
	} in
	List.iter (run_expression_filters com config) com.types
