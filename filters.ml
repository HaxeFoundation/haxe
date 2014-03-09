open Ast
open Common
open Type
open Typecore

(* PASS 1 begin *)

(*
	Wraps implicit blocks in TIf, TFor, TWhile, TFunction and TTry with real ones
*)
let rec blockify_ast e =
	match e.eexpr with
	| TIf(e1,e2,eo) ->
		{e with eexpr = TIf(blockify_ast e1,mk_block (blockify_ast e2),match eo with None -> None | Some e -> Some (mk_block (blockify_ast e)))}
	| TFor(v,e1,e2) ->
		{e with eexpr = TFor(v,blockify_ast e1,mk_block (blockify_ast e2))}
	| TWhile(e1,e2,flag) ->
		{e with eexpr = TWhile(blockify_ast e1,mk_block (blockify_ast e2),flag)}
	| TFunction tf ->
		{e with eexpr = TFunction {tf with tf_expr = mk_block (blockify_ast tf.tf_expr)}}
	| TTry(e1,cl) ->
		{e with eexpr = TTry(blockify_ast e1,List.map (fun (v,e) -> v,mk_block (blockify_ast e)) cl)}
	| _ ->
		Type.map_expr blockify_ast e

(*
	Generates a block context which can be used to add temporary variables. It returns a tuple:

	- a mapping function for expression lists to be used on TBlock elements
	- the function to be called for declaring temporary variables
	- the function to be called for closing the block, returning the block elements
*)
let mk_block_context com gen_temp =
	let block_el = ref [] in
	let push e = block_el := e :: !block_el in
	let declare_temp t eo p =
		let v = gen_temp t in
		let e = mk (TVar (v,eo)) com.basic.tvoid p in
		push e;
		mk (TLocal v) t p
	in
	let push_block () =
		let cur = !block_el in
		block_el := [];
		fun () ->
			let added = !block_el in
			block_el := cur;
			List.rev added
	in
	let rec block f el =
		let close = push_block() in
		List.iter (fun e ->
			push (f e)
		) el;
		close()
	in
	block,declare_temp,fun () -> !block_el

(*
	Moves expressions to temporary variables in order to ensure correct evaluation order. This effects

	- call arguments (from TCall and TNew)
	- array declaration arguments
	- object fields
	- binary operators (respects boolean short-circuit)
	- array access
*)
let handle_side_effects com gen_temp e =
	let block,declare_temp,close_block = mk_block_context com gen_temp in
	let rec loop e =
		match e.eexpr with
		| TBlock el ->
			{e with eexpr = TBlock (block loop el)}
		| TCall({eexpr = TLocal v},_) when Meta.has Meta.Unbound v.v_meta ->
			e
		| TCall(e1,el) ->
			let e1 = loop e1 in
			{e with eexpr = TCall(e1,ordered_list el)}
		| TNew(c,tl,el) ->
			{e with eexpr = TNew(c,tl,ordered_list el)}
		| TArrayDecl el ->
			{e with eexpr = TArrayDecl (ordered_list el)}
		| TObjectDecl fl ->
			let el = ordered_list (List.map snd fl) in
			{e with eexpr = TObjectDecl (List.map2 (fun (n,_) e -> n,e) fl el)}
		| TBinop(OpBoolAnd | OpBoolOr as op,e1,e2) when Optimizer.has_side_effect e1 || Optimizer.has_side_effect e2 ->
			let e1 = loop e1 in
			let e_then = mk (TBlock (block loop [e2])) e2.etype e2.epos in
			let e_if,e_else = if op = OpBoolOr then
				mk (TUnop(Not,Prefix,e1)) com.basic.tbool e.epos,mk (TConst (TBool(true))) com.basic.tbool e.epos
			else
				e1,mk (TConst (TBool(false))) com.basic.tbool e.epos
			in
			mk (TIf(e_if,e_then,Some e_else)) com.basic.tbool e.epos
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
		| _ ->
			Type.map_expr loop e
	and ordered_list el =
		let had_side_effect = ref false in
		let bind e =
			if !had_side_effect then
				declare_temp e.etype (Some (loop e)) e.epos
			else begin
				had_side_effect := true;
				e
			end
		in
		let rec no_side_effect e = match e.eexpr with
			| TNew _ | TCall _ | TArrayDecl _ | TObjectDecl _ | TBinop ((OpAssignOp _ | OpAssign),_,_) | TUnop ((Increment|Decrement),_,_) ->
				bind e;
			| TIf _ | TTry _ | TSwitch _ ->
				(* Technically these are not side-effects, but we have to move them out anyway because their blocks code have side-effects.
				   This also probably improves readability of the generated code. We can ignore TWhile and TFor because their type is Void,
				   so they could never appear in a place where side-effects matter. *)
				bind e
			| TBinop(op,e1,e2) when Optimizer.has_side_effect e1 || Optimizer.has_side_effect e2 ->
				bind e;
			| TConst _ | TLocal _ | TTypeExpr _ | TFunction _
			| TReturn _ | TBreak | TContinue | TThrow _ | TCast (_,Some _) ->
				e
			| TBlock _ ->
				bind e
			| _ ->
				Type.map_expr no_side_effect e
		in
		let rec loop2 acc el = match el with
			| e :: el ->
				let e = no_side_effect e in
				if !had_side_effect then
					(List.map no_side_effect (List.rev el)) @ e :: acc
				else
					loop2 (e :: acc) el
			| [] ->
				acc
		in
		List.map loop (loop2 [] (List.rev el))
	in
	let e = loop e in
	match close_block() with
		| [] ->
			e
		| el ->
			mk (TBlock (List.rev (e :: el))) e.etype e.epos

(*
	Pushes complex right-hand side expression inwards.

	return { exprs; value; } -> { exprs; return value; }
	x = { exprs; value; } -> { exprs; x = value; }
	var x = { exprs; value; } -> { var x; exprs; x = value; }
*)
let promote_complex_rhs ctx e =
	let rec is_complex e = match e.eexpr with
		| TBlock _ | TSwitch _ | TIf _ | TTry _ | TCast(_,Some _) -> true
		| TBinop(_,e1,e2) -> is_complex e1 || is_complex e2
		| TParenthesis e | TMeta(_,e) | TCast(e, None) -> is_complex e
		| _ -> false
	in
	let rec loop f e = match e.eexpr with
		| TBlock(el) ->
			begin match List.rev el with
				| elast :: el -> {e with eexpr = TBlock(block (List.rev ((loop f elast) :: el)))}
				| [] -> e
			end
		| TSwitch(es,cases,edef) ->
			{e with eexpr = TSwitch(es,List.map (fun (el,e) -> List.map find el,loop f e) cases,match edef with None -> None | Some e -> Some (loop f e))}
		| TIf(eif,ethen,eelse) ->
			{e with eexpr = TIf(find eif, loop f ethen, match eelse with None -> None | Some e -> Some (loop f e))}
		| TTry(e1,el) ->
			{e with eexpr = TTry(loop f e1, List.map (fun (el,e) -> el,loop f e) el)}
		| TParenthesis e1 when not (Common.defined ctx Define.As3) ->
			{e with eexpr = TParenthesis(loop f e1)}
		| TMeta(m,e1) ->
			{ e with eexpr = TMeta(m,loop f e1)}
		| TReturn _ | TThrow _ ->
			find e
		| TCast(e1,None) when ctx.config.pf_ignore_unsafe_cast ->
			loop f e1
		| _ ->
			f (find e)
	and block el =
		let r = ref [] in
		List.iter (fun e ->
			match e.eexpr with
			| TVar(v,eo) ->
				begin match eo with
					| Some e when is_complex e ->
						r := (loop (fun e -> mk (TBinop(OpAssign,mk (TLocal v) v.v_type e.epos,e)) v.v_type e.epos) e)
							:: ((mk (TVar (v,None)) ctx.basic.tvoid e.epos))
							:: !r
					| Some e ->
						r := (mk (TVar (v,Some (find e))) ctx.basic.tvoid e.epos) :: !r
					| None -> r := (mk (TVar (v,None)) ctx.basic.tvoid e.epos) :: !r
				end
			| _ -> r := (find e) :: !r
		) el;
		List.rev !r
	and find e = match e.eexpr with
		| TReturn (Some e1) -> loop (fun e -> {e with eexpr = TReturn (Some e)}) e1
		| TBinop(OpAssign | OpAssignOp _ as op, ({eexpr = TLocal _ | TField _ | TArray _} as e1), e2) -> loop (fun er -> {e with eexpr = TBinop(op, e1, er)}) e2
		| TBlock(el) -> {e with eexpr = TBlock (block el)}
		| _ -> Type.map_expr find e
	in
	find e

(* Adds final returns to functions as required by some platforms *)
let rec add_final_return e =
	let rec loop e t =
		let def_return p =
			let c = (match follow t with
				| TAbstract ({ a_path = [],"Int" },_) -> TInt 0l
				| TAbstract ({ a_path = [],"Float" },_) -> TFloat "0."
				| TAbstract ({ a_path = [],"Bool" },_) -> TBool false
				| _ -> TNull
			) in
			{ eexpr = TReturn (Some { eexpr = TConst c; epos = p; etype = t }); etype = t; epos = p }
		in
		match e.eexpr with
		| TBlock el ->
			(match List.rev el with
			| [] -> e
			| elast :: el ->
				match loop elast t with
				| { eexpr = TBlock el2 } -> { e with eexpr = TBlock ((List.rev el) @ el2) }
				| elast -> { e with eexpr = TBlock (List.rev (elast :: el)) })
		| TReturn _ ->
			e
		| _ ->
			{ e with eexpr = TBlock [e;def_return e.epos] }
	in

	let e = Type.map_expr add_final_return e in

	match e.eexpr with
		| TFunction f ->
			let f = (match follow f.tf_type with
				| TAbstract ({ a_path = [],"Void" },[]) -> f
				| t -> { f with tf_expr = loop f.tf_expr t }
			) in
			{ e with eexpr = TFunction f }
		| _ -> e

(* -------------------------------------------------------------------------- *)
(* CHECK LOCAL VARS INIT *)

let check_local_vars_init e =
	let intersect vl1 vl2 =
		PMap.mapi (fun v t -> t && PMap.find v vl2) vl1
	in
	let join vars cvars =
		List.iter (fun v -> vars := intersect !vars v) cvars
	in
	let restore vars old_vars declared =
		(* restore variables declared in this block to their previous state *)
		vars := List.fold_left (fun acc v ->
			try	PMap.add v (PMap.find v old_vars) acc with Not_found -> PMap.remove v acc
		) !vars declared;
	in
	let declared = ref [] in
	let rec loop vars e =
		match e.eexpr with
		| TLocal v ->
			let init = (try PMap.find v.v_id !vars with Not_found -> true) in
			if not init then begin
				if v.v_name = "this" then error "Missing this = value" e.epos
				else error ("Local variable " ^ v.v_name ^ " used without being initialized") e.epos
			end
		| TVar (v,eo) ->
			begin
				match eo with
				| None ->
					declared := v.v_id :: !declared;
					vars := PMap.add v.v_id false !vars
				| Some e ->
					loop vars e
			end
		| TBlock el ->
			let old = !declared in
			let old_vars = !vars in
			declared := [];
			List.iter (loop vars) el;
			restore vars old_vars (List.rev !declared);
			declared := old;
		| TBinop (OpAssign,{ eexpr = TLocal v },e) when PMap.mem v.v_id !vars ->
			loop vars e;
			vars := PMap.add v.v_id true !vars
		| TIf (e1,e2,eo) ->
			loop vars e1;
			let vbase = !vars in
			loop vars e2;
			(match eo with
			| None -> vars := vbase
			(* ignore else false cases (they are added by the side-effect handler) *)
			| Some {eexpr = TConst (TBool(false))} -> ()
			| Some e ->
				let v1 = !vars in
				vars := vbase;
				loop vars e;
				vars := intersect !vars v1)
		| TWhile (cond,e,flag) ->
			(match flag with
			| NormalWhile ->
				loop vars cond;
				let old = !vars in
				loop vars e;
				vars := old;
			| DoWhile ->
				loop vars e;
				loop vars cond)
		| TTry (e,catches) ->
			let cvars = List.map (fun (v,e) ->
				let old = !vars in
				loop vars e;
				let v = !vars in
				vars := old;
				v
			) catches in
			loop vars e;
			join vars cvars;
		| TSwitch (e,cases,def) ->
			loop vars e;
			let cvars = List.map (fun (ec,e) ->
				let old = !vars in
				List.iter (loop vars) ec;
				vars := old;
				loop vars e;
				let v = !vars in
				vars := old;
				v
			) cases in
			(match def with
			| None when (match e.eexpr with TMeta((Meta.Exhaustive,_,_),_) | TParenthesis({eexpr = TMeta((Meta.Exhaustive,_,_),_)}) -> true | _ -> false) ->
				(match cvars with
				| cv :: cvars ->
					PMap.iter (fun i b -> if b then vars := PMap.add i b !vars) cv;
					join vars cvars
				| [] -> ())
			| None -> ()
			| Some e ->
				loop vars e;
				join vars cvars)
		| TPatMatch dt ->
			let cvars = ref [] in
			let rec fdt dt = match dt with
				| DTExpr e ->
					let old = !vars in
					loop vars e;
					restore vars old [];
					cvars := !vars :: !cvars
				| DTSwitch(e,cl,dto) ->
					loop vars e;
					List.iter (fun (_,dt) -> fdt dt) cl;
					(match dto with None -> () | Some dt -> fdt dt)
				| DTGuard(e,dt1,dt2) ->
					fdt dt1;
					(match dt2 with None -> () | Some dt -> fdt dt)
				| DTBind(_,dt) -> fdt dt
				| DTGoto _ -> ()
			in
			Array.iter fdt dt.dt_dt_lookup;
			join vars !cvars
		(* mark all reachable vars as initialized, since we don't exit the block  *)
		| TBreak | TContinue | TReturn None ->
			vars := PMap.map (fun _ -> true) !vars
		| TThrow e | TReturn (Some e) ->
			loop vars e;
			vars := PMap.map (fun _ -> true) !vars
		| _ ->
			Type.iter (loop vars) e
	in
	loop (ref PMap.empty) e;
	e

(* -------------------------------------------------------------------------- *)
(* BLOCK VARIABLES CAPTURE *)

(*
	For some platforms, it will simply mark the variables which are used in closures
	using the v_capture flag so it can be processed in a more optimized

	For Flash/JS platforms, it will ensure that variables used in loop sub-functions
	have an unique scope. It transforms the following expression :

	for( x in array )
		funs.push(function() return x++);

	Into the following :

	for( _x in array ) {
		var x = [_x];
		funs.push(function(x) { function() return x[0]++; }(x));
	}
*)

type usage =
	| Block of ((usage -> unit) -> unit)
	| Loop of ((usage -> unit) -> unit)
	| Function of ((usage -> unit) -> unit)
	| Declare of tvar
	| Use of tvar

let rec local_usage f e =
	match e.eexpr with
	| TLocal v ->
		f (Use v)
	| TVar (v,eo) ->
		(match eo with None -> () | Some e -> local_usage f e);
		f (Declare v);
	| TFunction tf ->
		let cc f =
			List.iter (fun (v,_) -> f (Declare v)) tf.tf_args;
			local_usage f tf.tf_expr;
		in
		f (Function cc)
	| TBlock l ->
		f (Block (fun f -> List.iter (local_usage f) l))
	| TFor (v,it,e) ->
		local_usage f it;
		f (Loop (fun f ->
			f (Declare v);
			local_usage f e;
		))
	| TWhile _ ->
		f (Loop (fun f ->
			iter (local_usage f) e
		))
	| TTry (e,catchs) ->
		local_usage f e;
		List.iter (fun (v,e) ->
			f (Block (fun f ->
				f (Declare v);
				local_usage f e;
			))
		) catchs;
	| TPatMatch dt ->
		List.iter (fun (v,eo) ->
			f (Declare v);
			match eo with None -> () | Some e -> local_usage f e
		) dt.dt_var_init;
		let rec fdt dt = match dt with
			| DTBind(bl,dt) ->
				List.iter (fun ((v,_),e) ->
					f (Declare v);
					local_usage f e
				) bl;
				fdt dt
			| DTExpr e -> local_usage f e
			| DTGuard(e,dt1,dt2) ->
				local_usage f e;
				fdt dt1;
				(match dt2 with None -> () | Some dt -> fdt dt)
			| DTSwitch(e,cl,dto) ->
				local_usage f e;
				List.iter (fun (e,dt) ->
					local_usage f e;
					fdt dt
				) cl;
				(match dto with None -> () | Some dt -> fdt dt)
			| DTGoto _ -> ()
		in
		Array.iter fdt dt.dt_dt_lookup
	| _ ->
		iter (local_usage f) e

let captured_vars com e =

	let t = com.basic in

	let rec mk_init av v pos =
		mk (TVar (av,Some (mk (TArrayDecl [mk (TLocal v) v.v_type pos]) av.v_type pos))) t.tvoid pos

	and mk_var v used =
		let v2 = alloc_var v.v_name (PMap.find v.v_id used) in
		v2.v_meta <- v.v_meta;
		v2

	and wrap used e =
		match e.eexpr with
		| TVar (v,ve) ->
			let v,ve =
				if PMap.mem v.v_id used then
					v, Some (mk (TArrayDecl (match ve with None -> [] | Some e -> [wrap used e])) v.v_type e.epos)
				else
					v, (match ve with None -> None | Some e -> Some (wrap used e))
			 in
			{ e with eexpr = TVar (v,ve) }
		| TLocal v when PMap.mem v.v_id used ->
			mk (TArray ({ e with etype = v.v_type },mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos
		| TFor (v,it,expr) when PMap.mem v.v_id used ->
			let vtmp = mk_var v used in
			let it = wrap used it in
			let expr = wrap used expr in
			mk (TFor (vtmp,it,Codegen.concat (mk_init v vtmp e.epos) expr)) e.etype e.epos
		| TTry (expr,catchs) ->
			let catchs = List.map (fun (v,e) ->
				let e = wrap used e in
				try
					let vtmp = mk_var v used in
					vtmp, Codegen.concat (mk_init v vtmp e.epos) e
				with Not_found ->
					v, e
			) catchs in
			mk (TTry (wrap used expr,catchs)) e.etype e.epos
		(* TODO: find out this does *)
(* 		| TMatch (expr,enum,cases,def) ->
			let cases = List.map (fun (il,vars,e) ->
				let pos = e.epos in
				let e = ref (wrap used e) in
				let vars = match vars with
					| None -> None
					| Some l ->
						Some (List.map (fun v ->
							match v with
							| Some v when PMap.mem v.v_id used ->
								let vtmp = mk_var v used in
								e := concat (mk_init v vtmp pos) !e;
								Some vtmp
							| _ -> v
						) l)
				in
				il, vars, !e
			) cases in
			let def = match def with None -> None | Some e -> Some (wrap used e) in
			mk (TMatch (wrap used expr,enum,cases,def)) e.etype e.epos *)
		| TFunction f ->
			(*
				list variables that are marked as used, but also used in that
				function and which are not declared inside it !
			*)
			let fused = ref PMap.empty in
			let tmp_used = ref used in
			let rec browse = function
				| Block f | Loop f | Function f -> f browse
				| Use v ->
					if PMap.mem v.v_id !tmp_used then fused := PMap.add v.v_id v !fused;
				| Declare v ->
					tmp_used := PMap.remove v.v_id !tmp_used
			in
			local_usage browse e;
			let vars = PMap.fold (fun v acc -> v :: acc) !fused [] in

			(* in case the variable has been marked as used in a parallel scope... *)
			let fexpr = ref (wrap used f.tf_expr) in
			let fargs = List.map (fun (v,o) ->
				if PMap.mem v.v_id used then
					let vtmp = mk_var v used in
					fexpr := Codegen.concat (mk_init v vtmp e.epos) !fexpr;
					vtmp, o
				else
					v, o
			) f.tf_args in
			let e = { e with eexpr = TFunction { f with tf_args = fargs; tf_expr = !fexpr } } in
			(*
				Create a new function scope to make sure that the captured loop variable
				will not be overwritten in next loop iteration
			*)
			if com.config.pf_capture_policy = CPLoopVars then
				mk (TCall (
					Codegen.mk_parent (mk (TFunction {
						tf_args = List.map (fun v -> v, None) vars;
						tf_type = e.etype;
						tf_expr = mk_block (mk (TReturn (Some e)) e.etype e.epos);
					}) (TFun (List.map (fun v -> v.v_name,false,v.v_type) vars,e.etype)) e.epos),
					List.map (fun v -> mk (TLocal v) v.v_type e.epos) vars)
				) e.etype e.epos
			else
				e
		| _ ->
			map_expr (wrap used) e

	and do_wrap used e =
		if PMap.is_empty used then
			e
		else
			let used = PMap.map (fun v ->
				let vt = v.v_type in
				v.v_type <- t.tarray vt;
				v.v_capture <- true;
				vt
			) used in
			wrap used e

	and out_loop e =
		match e.eexpr with
		| TFor _ | TWhile _ ->
			(*
				collect variables that are declared in loop but used in subfunctions
			*)
			let vars = ref PMap.empty in
			let used = ref PMap.empty in
			let depth = ref 0 in
			let rec collect_vars in_loop = function
				| Block f ->
					let old = !vars in
					f (collect_vars in_loop);
					vars := old;
				| Loop f ->
					let old = !vars in
					f (collect_vars true);
					vars := old;
				| Function f ->
					incr depth;
					f (collect_vars false);
					decr depth;
				| Declare v ->
					if in_loop then vars := PMap.add v.v_id !depth !vars;
				| Use v ->
					try
						let d = PMap.find v.v_id !vars in
						if d <> !depth then used := PMap.add v.v_id v !used;
					with Not_found ->
						()
			in
			local_usage (collect_vars false) e;
			do_wrap !used e
		| _ ->
			map_expr out_loop e
	and all_vars e =
		let vars = ref PMap.empty in
		let used = ref PMap.empty in
		let depth = ref 0 in
		let rec collect_vars = function
		| Block f ->
			let old = !vars in
			f collect_vars;
			vars := old;
		| Loop f ->
			let old = !vars in
			f collect_vars;
			vars := old;
		| Function f ->
			incr depth;
			f collect_vars;
			decr depth;
		| Declare v ->
			vars := PMap.add v.v_id !depth !vars;
		| Use v ->
			try
				let d = PMap.find v.v_id !vars in
				if d <> !depth then used := PMap.add v.v_id v !used;
			with Not_found -> ()
		in
		local_usage collect_vars e;
		!used
	in
	(* mark all capture variables - also used in rename_local_vars at later stage *)
	let captured = all_vars e in
	PMap.iter (fun _ v -> v.v_capture <- true) captured;
	match com.config.pf_capture_policy with
	| CPNone -> e
	| CPWrapRef -> do_wrap captured e
	| CPLoopVars -> out_loop e

(* -------------------------------------------------------------------------- *)
(* RENAME LOCAL VARS *)

let rename_local_vars com e =
	let cfg = com.config in
	let all_scope = (not cfg.pf_captured_scope) || (not cfg.pf_locals_scope) in
	let vars = ref PMap.empty in
	let all_vars = ref PMap.empty in
	let vtemp = alloc_var "~" t_dynamic in
	let rebuild_vars = ref false in
	let rebuild m =
		PMap.fold (fun v acc -> PMap.add v.v_name v acc) m PMap.empty
	in
	let save() =
		let old = !vars in
		if cfg.pf_unique_locals || not cfg.pf_locals_scope then (fun() -> ()) else (fun() -> vars := if !rebuild_vars then rebuild old else old)
	in
	let rename vars v =
		let count = ref 1 in
		while PMap.mem (v.v_name ^ string_of_int !count) vars do
			incr count;
		done;
		v.v_name <- v.v_name ^ string_of_int !count;
	in
	let declare v p =
		(match follow v.v_type with
			| TAbstract ({a_path = [],"Void"},_) -> error "Arguments and variables of type Void are not allowed" p
			| _ -> ());
		(* chop escape char for all local variables generated *)
		if String.unsafe_get v.v_name 0 = String.unsafe_get gen_local_prefix 0 then v.v_name <- "_g" ^ String.sub v.v_name 1 (String.length v.v_name - 1);
		let look_vars = (if not cfg.pf_captured_scope && v.v_capture then !all_vars else !vars) in
		(try
			let v2 = PMap.find v.v_name look_vars in
			(*
				block_vars will create some wrapper-functions that are declaring
				the same variable twice. In that case do not perform a rename since
				we are sure it's actually the same variable
			*)
			if v == v2 then raise Not_found;
			rename look_vars v;
		with Not_found ->
			());
		vars := PMap.add v.v_name v !vars;
		if all_scope then all_vars := PMap.add v.v_name v !all_vars;
	in
	(*
		This is quite a rare case, when a local variable would otherwise prevent
		accessing a type because it masks the type value or the package name.
	*)
	let check t =
		match (t_infos t).mt_path with
		| [], name | name :: _, _ ->
			let vars = if cfg.pf_locals_scope then vars else all_vars in
			(try
				let v = PMap.find name !vars in
				if v == vtemp then raise Not_found; (* ignore *)
				rename (!vars) v;
				rebuild_vars := true;
				vars := PMap.add v.v_name v !vars
			with Not_found ->
				());
			vars := PMap.add name vtemp !vars
	in
	let check_type t =
		match follow t with
		| TInst (c,_) -> check (TClassDecl c)
		| TEnum (e,_) -> check (TEnumDecl e)
		| TType (t,_) -> check (TTypeDecl t)
		| TAbstract (a,_) -> check (TAbstractDecl a)
		| TMono _ | TLazy _ | TAnon _ | TDynamic _ | TFun _ -> ()
	in
	let rec loop e =
		match e.eexpr with
		| TVar (v,eo) ->
			if not cfg.pf_locals_scope then declare v e.epos;
			(match eo with None -> () | Some e -> loop e);
			if cfg.pf_locals_scope then declare v e.epos;
		| TFunction tf ->
			let old = save() in
			List.iter (fun (v,_) -> declare v e.epos) tf.tf_args;
			loop tf.tf_expr;
			old()
		| TBlock el ->
			let old = save() in
			List.iter loop el;
			old()
		| TFor (v,it,e1) ->
			loop it;
			let old = save() in
			declare v e.epos;
			loop e1;
			old()
		| TTry (e,catchs) ->
			loop e;
			List.iter (fun (v,e) ->
				let old = save() in
				declare v e.epos;
				check_type v.v_type;
				loop e;
				old()
			) catchs;
		| TPatMatch dt ->
			let rec fdt dt = match dt with
				| DTSwitch(e,cl,dto) ->
					loop e;
					List.iter (fun (_,dt) ->
						let old = save() in
						fdt dt;
						old();
					) cl;
					(match dto with None -> () | Some dt ->
						let old = save() in
						fdt dt;
						old())
				| DTBind(bl,dt) ->
					List.iter (fun ((v,p),e) ->
						declare v e.epos
					) bl;
					fdt dt
				| DTExpr e -> loop e;
				| DTGuard(e,dt1,dt2) ->
					loop e;
					fdt dt1;
					(match dt2 with None -> () | Some dt -> fdt dt)
				| DTGoto _ ->
					()
			in
			Array.iter fdt dt.dt_dt_lookup
		| TTypeExpr t ->
			check t
		| TNew (c,_,_) ->
			Type.iter loop e;
			check (TClassDecl c);
		| TCast (e,Some t) ->
			loop e;
			check t;
		| _ ->
			Type.iter loop e
	in
	declare (alloc_var "this" t_dynamic) Ast.null_pos; (* force renaming of 'this' vars in abstract *)
	loop e;
	e

(* PASS 1 end *)

(* Saves a class state so it can be restored later, e.g. after DCE or native path rewrite *)
let save_class_state ctx t = match t with
	| TClassDecl c ->
		let meta = c.cl_meta and path = c.cl_path and ext = c.cl_extern in
		let fl = c.cl_fields and ofl = c.cl_ordered_fields and st = c.cl_statics and ost = c.cl_ordered_statics in
		let cst = c.cl_constructor and over = c.cl_overrides in
		let oflk = List.map (fun f -> f.cf_kind,f.cf_expr,f.cf_type) ofl in
		let ostk = List.map (fun f -> f.cf_kind,f.cf_expr,f.cf_type) ost in
		c.cl_restore <- (fun() ->
			c.cl_meta <- meta;
			c.cl_extern <- ext;
			c.cl_path <- path;
			c.cl_fields <- fl;
			c.cl_ordered_fields <- ofl;
			c.cl_statics <- st;
			c.cl_ordered_statics <- ost;
			c.cl_constructor <- cst;
			c.cl_overrides <- over;
			(* DCE might modify the cf_kind, so let's restore it as well *)
			List.iter2 (fun f (k,e,t) -> f.cf_kind <- k; f.cf_expr <- e; f.cf_type <- t;) ofl oflk;
			List.iter2 (fun f (k,e,t) -> f.cf_kind <- k; f.cf_expr <- e; f.cf_type <- t;) ost ostk;
		)
	| _ ->
		()

(* PASS 2 begin *)

let is_removable_class c = c.cl_kind = KGeneric && (Codegen.has_ctor_constraint c || Meta.has Meta.Remove c.cl_meta)

let remove_generic_base ctx t = match t with
	| TClassDecl c when is_removable_class c ->
		c.cl_extern <- true
	| _ ->
		()

(* Removes extern and macro fields, also checks for Void fields *)

let is_removable_field ctx f =
	Meta.has Meta.Extern f.cf_meta || Meta.has Meta.Generic f.cf_meta
	|| (match f.cf_kind with
		| Var {v_read = AccRequire (s,_)} -> true
		| Method MethMacro -> not ctx.in_macro
		| _ -> false)

let remove_extern_fields ctx t = match t with
	| TClassDecl c ->
		if not (Common.defined ctx.com Define.DocGen) then begin
			c.cl_ordered_fields <- List.filter (fun f ->
				let b = is_removable_field ctx f in
				if b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
				not b
			) c.cl_ordered_fields;
			c.cl_ordered_statics <- List.filter (fun f ->
				let b = is_removable_field ctx f in
				if b then c.cl_statics <- PMap.remove f.cf_name c.cl_statics;
				not b
			) c.cl_ordered_statics;
		end
	| _ ->
		()

(* PASS 2 end *)

(* PASS 3 begin *)

(* Checks if a private class' path clashes with another path *)
let check_private_path ctx t = match t with
	| TClassDecl c when c.cl_private ->
		let rpath = (fst c.cl_module.m_path,"_" ^ snd c.cl_module.m_path) in
		if Hashtbl.mem ctx.g.types_module rpath then error ("This private class name will clash with " ^ s_type_path rpath) c.cl_pos;
	| _ ->
		()

(* Rewrites class or enum paths if @:native metadata is set *)
let apply_native_paths ctx t =
	let get_real_path meta path =
		let (_,e,mp) = Meta.get Meta.Native meta in
		match e with
		| [Ast.EConst (Ast.String name),p] ->
			(Meta.RealPath,[Ast.EConst (Ast.String (s_type_path path)),p],mp),parse_path name
		| _ ->
			error "String expected" mp
	in
	try
		(match t with
		| TClassDecl c ->
			let meta,path = get_real_path c.cl_meta c.cl_path in
			c.cl_meta <- meta :: c.cl_meta;
			c.cl_path <- path;
		| TEnumDecl e ->
			let meta,path = get_real_path e.e_meta e.e_path in
			e.e_meta <- meta :: e.e_meta;
			e.e_path <- path;
		| TAbstractDecl a ->
			let meta,path = get_real_path a.a_meta a.a_path in
			a.a_meta <- meta :: a.a_meta;
			a.a_path <- path;
		| _ ->
			())
	with Not_found ->
		()

(* Adds the __rtti field if required *)
let add_rtti ctx t =
	let rec has_rtti c =
		Meta.has Meta.Rtti c.cl_meta || match c.cl_super with None -> false | Some (csup,_) -> has_rtti csup
	in
	match t with
	| TClassDecl c when has_rtti c && not (PMap.mem "__rtti" c.cl_statics) ->
		let f = mk_field "__rtti" ctx.t.tstring c.cl_pos in
		let str = Genxml.gen_type_string ctx.com t in
		f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
		c.cl_ordered_statics <- f :: c.cl_ordered_statics;
		c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
	| _ ->
		()

(* Adds member field initializations as assignments to the constructor *)
let add_field_inits ctx t =
	let apply c =
		let ethis = mk (TConst TThis) (TInst (c,List.map snd c.cl_types)) c.cl_pos in
		(* TODO: we have to find a variable name which is not used in any of the functions *)
		let v = alloc_var "_g" ethis.etype in
		let need_this = ref false in
		let inits,fields = List.fold_left (fun (inits,fields) cf ->
			match cf.cf_kind,cf.cf_expr with
			| Var _, Some _ ->
				if Common.defined ctx.com Define.As3 then (inits, cf :: fields) else (cf :: inits, cf :: fields)
			| Method MethDynamic, Some e when Common.defined ctx.com Define.As3 ->
				(* TODO : this would have a better place in genSWF9 I think - NC *)
				(* we move the initialization of dynamic functions to the constructor and also solve the
				   'this' problem along the way *)
				let rec use_this v e = match e.eexpr with
					| TConst TThis ->
						need_this := true;
						mk (TLocal v) v.v_type e.epos
					| _ -> Type.map_expr (use_this v) e
				in
				let e = Type.map_expr (use_this v) e in
				let cf2 = {cf with cf_expr = Some e} in
				(* if the method is an override, we have to remove the class field to not get invalid overrides *)
				let fields = if List.memq cf c.cl_overrides then begin
					c.cl_fields <- PMap.remove cf.cf_name c.cl_fields;
					fields
				end else
					cf2 :: fields
				in
				(cf2 :: inits, fields)
			| _ -> (inits, cf :: fields)
		) ([],[]) c.cl_ordered_fields in
		c.cl_ordered_fields <- (List.rev fields);
		match inits with
		| [] -> ()
		| _ ->
			let cf_ctor = match c.cl_constructor with
				| None ->
					List.iter (fun cf -> display_error ctx "Cannot initialize member fields on classes that do not have a constructor" cf.cf_pos) inits;
					error "Could not initialize member fields" c.cl_pos;
				| Some cf ->
					cf
			in
			let el = List.map (fun cf ->
				match cf.cf_expr with
				| None -> assert false
				| Some e ->
					let lhs = mk (TField(ethis,FInstance (c,cf))) cf.cf_type e.epos in
					cf.cf_expr <- None;
					let eassign = mk (TBinop(OpAssign,lhs,e)) e.etype e.epos in
					if Common.defined ctx.com Define.As3 then begin
						let echeck = mk (TBinop(OpEq,lhs,(mk (TConst TNull) lhs.etype e.epos))) ctx.com.basic.tbool e.epos in
						mk (TIf(echeck,eassign,None)) eassign.etype e.epos
					end else
						eassign;
			) inits in
			let el = if !need_this then (mk (TVar((v, Some ethis))) ethis.etype ethis.epos) :: el else el in
			match cf_ctor.cf_expr with
			| Some { eexpr = TFunction f } ->
				let bl = match f.tf_expr with {eexpr = TBlock b } -> b | x -> [x] in
				let ce = mk (TFunction {f with tf_expr = mk (TBlock (el @ bl)) ctx.com.basic.tvoid c.cl_pos }) cf_ctor.cf_type cf_ctor.cf_pos in
				c.cl_constructor <- Some {cf_ctor with cf_expr = Some ce }
			| _ ->
				assert false
	in
	match t with
	| TClassDecl c ->
		apply c
	| _ ->
		()

(* Adds the __meta__ field if required *)
let add_meta_field ctx t = match t with
	| TClassDecl c ->
		(match Codegen.build_metadata ctx.com t with
		| None -> ()
		| Some e ->
			let f = mk_field "__meta__" t_dynamic c.cl_pos in
			f.cf_expr <- Some e;
			c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			c.cl_statics <- PMap.add f.cf_name f c.cl_statics)
	| _ ->
		()

(* Removes interfaces tagged with @:remove metadata *)
let check_remove_metadata ctx t = match t with
	| TClassDecl c ->
		c.cl_implements <- List.filter (fun (c,_) -> not (Meta.has Meta.Remove c.cl_meta)) c.cl_implements;
	| _ ->
		()

(* Checks for Void class fields *)
let check_void_field ctx t = match t with
	| TClassDecl c ->
		let check f =
			match follow f.cf_type with TAbstract({a_path=[],"Void"},_) -> error "Fields of type Void are not allowed" f.cf_pos | _ -> ();
		in
		List.iter check c.cl_ordered_fields;
		List.iter check c.cl_ordered_statics;
	| _ ->
		()

(* PASS 3 end *)

let run_expression_filters ctx filters t =
	match t with
	| TClassDecl c when is_removable_class c -> ()
	| TClassDecl c ->
		let process_field f =
			match f.cf_expr with
			| Some e when not (is_removable_field ctx f) ->
				Codegen.Abstract.cast_stack := f :: !Codegen.Abstract.cast_stack;
				f.cf_expr <- Some (List.fold_left (fun e f -> f e) e filters);
				Codegen.Abstract.cast_stack := List.tl !Codegen.Abstract.cast_stack;
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
			c.cl_init <- Some (List.fold_left (fun e f -> f e) e filters));
	| TEnumDecl _ -> ()
	| TTypeDecl _ -> ()
	| TAbstractDecl _ -> ()

let pp_counter = ref 1

let post_process ctx filters t =
	(* ensure that we don't process twice the same (cached) module *)
	let m = (t_infos t).mt_module.m_extra in
	if m.m_processed = 0 then m.m_processed <- !pp_counter;
	if m.m_processed = !pp_counter then
	run_expression_filters ctx filters t

let post_process_end() =
	incr pp_counter

let run com tctx main =
	if com.display = DMUsage then
		Codegen.detect_usage com;
	(* PASS 1: general expression filters *)
 	let filters = [
		Codegen.Abstract.handle_abstract_casts tctx;
		blockify_ast;
		(match com.platform with
			| Cpp | Flash8 -> (fun e ->
				let save = save_locals tctx in
				let e = handle_side_effects com (Typecore.gen_local tctx) e in
				save();
				e)
			| _ -> fun e -> e);
		if com.foptimize then (fun e -> Optimizer.reduce_expression tctx (Optimizer.inline_constructors tctx e)) else Optimizer.sanitize tctx;
		check_local_vars_init;
		captured_vars com;
	] in
	List.iter (post_process tctx filters) com.types;
	post_process_end();
	List.iter (fun f -> f()) (List.rev com.filters);
	(* save class state *)
	List.iter (save_class_state tctx) com.types;
	(* PASS 2: destructive type and expression filters *)
	let filters = [
		promote_complex_rhs com;
		if com.config.pf_add_final_return then add_final_return else (fun e -> e);
		rename_local_vars com; (* TODO: it shouldn't be necessary to have this here if promote_complex_rhs can generate proper variable names *)
	] in
	List.iter (fun t ->
		remove_generic_base tctx t;
		remove_extern_fields tctx t;
		run_expression_filters tctx filters t;
	) com.types;
	(* update cache dependencies before DCE is run *)
	Codegen.update_cache_dependencies com;
	(* DCE *)
	let dce_mode = (try Common.defined_value com Define.Dce with _ -> "no") in
	if not (Common.defined com Define.As3 || dce_mode = "no" || Common.defined com Define.DocGen) then Dce.run com main (dce_mode = "full" && not (Common.defined com Define.Interp));
	(* always filter empty abstract implementation classes (issue #1885) *)
	List.iter (fun mt -> match mt with
		| TClassDecl({cl_kind = KAbstractImpl _} as c) when c.cl_ordered_statics = [] && c.cl_ordered_fields = [] && not (Meta.has Meta.Used c.cl_meta) -> c.cl_extern <- true
		| _ -> ()
	) com.types;
	(* PASS 3: type filters *)
	let type_filters = [
		check_private_path;
		apply_native_paths;
		add_rtti;
		(match com.platform with | Java | Cs -> (fun _ _ -> ()) | _ -> add_field_inits);
		add_meta_field;
		check_remove_metadata;
		check_void_field;
	] in
	List.iter (fun t -> List.iter (fun f -> f tctx t) type_filters) com.types