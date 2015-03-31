open Ast
open Common
open Type
open Typecore

(* PASS 1 begin *)

let rec verify_ast ctx e =
	let not_null e e1 = match e1.eexpr with
		| TConst TNull -> display_error ctx ("Invalid null expression: " ^ (s_expr_pretty "" (s_type (print_context())) e)) e.epos
		| _ -> ()
	in
	let rec loop e = match e.eexpr with
	| TField(e1,_) ->
		not_null e e1;
		()
	| TArray(e1,e2) ->
		not_null e e1;
		loop e1;
		loop e2
	| TCall(e1,el) ->
		not_null e e1;
		loop e1;
		List.iter loop el
	| TUnop(_,_,e1) ->
		not_null e e1;
		loop e1
	(* probably too messy *)
(* 	| TBinop((OpEq | OpNotEq),e1,e2) ->
		loop e1;
		loop e2
	| TBinop((OpAssign | OpAssignOp _),e1,e2) ->
		not_null e e1;
		loop e1;
		loop e2
	| TBinop(op,e1,e2) ->
		not_null e e1;
		not_null e e2;
		loop e1;
		loop e2 *)
	| TTypeExpr(TClassDecl {cl_kind = KAbstractImpl a}) when not (Meta.has Meta.RuntimeValue a.a_meta) ->
		error "Cannot use abstract as value" e.epos
	| _ ->
		Type.iter loop e
	in
	loop e

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
		{e with eexpr = TTry(mk_block (blockify_ast e1),List.map (fun (v,e) -> v,mk_block (blockify_ast e)) cl)}
	| TSwitch(e1,cases,def) ->
		let e1 = blockify_ast e1 in
		let cases = List.map (fun (el,e) ->
			el,mk_block (blockify_ast e)
		) cases in
		let def = match def with None -> None | Some e -> Some (mk_block (blockify_ast e)) in
		{e with eexpr = TSwitch(e1,cases,def)}
	| _ ->
		Type.map_expr blockify_ast e

(*
	Pushes complex right-hand side expression inwards.

	return { exprs; value; } -> { exprs; return value; }
	x = { exprs; value; } -> { exprs; x = value; }
	var x = { exprs; value; } -> { var x; exprs; x = value; }
*)
let promote_complex_rhs com e =
	let rec is_complex e = match e.eexpr with
		| TBlock _ | TSwitch _ | TIf _ | TTry _ | TCast(_,Some _) -> true
		| TBinop(_,e1,e2) -> is_complex e1 || is_complex e2
		| TParenthesis e | TMeta(_,e) | TCast(e, None) | TField(e,_) -> is_complex e
		| _ -> false
	in
	let rec loop f e = match e.eexpr with
		| TBlock(el) ->
			begin match List.rev el with
				| elast :: el -> {e with eexpr = TBlock(block (List.rev ((loop f elast) :: el)))}
				| [] -> e
			end
		| TSwitch(es,cases,edef) ->
			{e with eexpr = TSwitch(es,List.map (fun (el,e) -> List.map find el,loop f e) cases,match edef with None -> None | Some e -> Some (loop f e)); }
		| TIf(eif,ethen,eelse) ->
			{e with eexpr = TIf(find eif, loop f ethen, match eelse with None -> None | Some e -> Some (loop f e)); }
		| TTry(e1,el) ->
			{e with eexpr = TTry(loop f e1, List.map (fun (el,e) -> el,loop f e) el); }
		| TParenthesis e1 when not (Common.defined com Define.As3) ->
			{e with eexpr = TParenthesis(loop f e1)}
		| TMeta(m,e1) ->
			{ e with eexpr = TMeta(m,loop f e1)}
		| TReturn _ | TThrow _ ->
			find e
		| TContinue | TBreak ->
			e
		| _ ->
			f (find e)
	and block el =
		let r = ref [] in
		List.iter (fun e ->
			match e.eexpr with
			| TVar(v,eo) ->
				begin match eo with
					| Some e when is_complex e ->
						let e = find e in
						r := (loop (fun e -> mk (TBinop(OpAssign,mk (TLocal v) v.v_type e.epos,e)) v.v_type e.epos) e)
							:: ((mk (TVar (v,None)) com.basic.tvoid e.epos))
							:: !r
					| Some e ->
						r := (mk (TVar (v,Some (find e))) com.basic.tvoid e.epos) :: !r
					| None -> r := (mk (TVar (v,None)) com.basic.tvoid e.epos) :: !r
				end
			| TReturn (Some e1) when (match follow e1.etype with TAbstract({a_path=[],"Void"},_) -> true | _ -> false) ->
				r := ({e with eexpr = TReturn None}) :: e1 :: !r
			| _ -> r := (find e) :: !r
		) el;
		List.rev !r
	and find e = match e.eexpr with
		| TReturn (Some e1) -> loop (fun er -> {e with eexpr = TReturn (Some er)}) e1
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

let rec wrap_js_exceptions com e =
	let rec is_error t =
		match follow t with
		| TInst ({cl_path = (["js"],"Error")},_) -> true
		| TInst ({cl_super = Some (csup,tl)}, _) -> is_error (TInst (csup,tl))
		| _ -> false
	in
	let rec loop e =
		match e.eexpr with
		| TThrow eerr when not (is_error eerr.etype) ->
			let terr = List.find (fun mt -> match mt with TClassDecl {cl_path = ["js";"_Boot"],"HaxeError"} -> true | _ -> false) com.types in
			let cerr = match terr with TClassDecl c -> c | _ -> assert false in
			let ewrap = { eerr with eexpr = TNew (cerr,[],[eerr]) } in
			{ e with eexpr = TThrow ewrap }
		| _ ->
			Type.map_expr loop e
	in

	loop e

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
			| NormalWhile when (match cond.eexpr with TParenthesis {eexpr = TConst (TBool true)} -> false | _ -> true) ->
				loop vars cond;
				let old = !vars in
				loop vars e;
				vars := old;
			| _ ->
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
	| Assign of tvar

let rec local_usage f e =
	match e.eexpr with
	| TBinop ((OpAssign | OpAssignOp _), { eexpr = TLocal v }, e2) ->
		local_usage f e2;
		f (Assign v)
	| TUnop ((Increment | Decrement), _, { eexpr = TLocal v }) ->
		f (Assign v)
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
	| _ ->
		iter (local_usage f) e

let captured_vars com e =

	let t = com.basic in

	let impl = match com.platform with
	(* optimized version for C#/Java - use native arrays *)
	| Cs | Java ->
		let cnativearray =
			match (List.find (fun md -> match md with
					| TClassDecl ({ cl_path = ["cs"|"java"],"NativeArray" }) -> true
					| _ -> false
				) com.types)
			with TClassDecl cl -> cl | _ -> assert false
		in

		object
			method captured_type t = TInst (cnativearray,[t])

			method mk_ref v ve p =
				match ve with
				| None ->
					let eone = mk (TConst (TInt (Int32.of_int 1))) t.tint p in
					let t = match v.v_type with TInst (_, [t]) -> t | _ -> assert false in
					mk (TNew (cnativearray,[t],[eone])) v.v_type p
				| Some e ->
					{ (Optimizer.mk_untyped_call "__array__" p [e]) with etype = v.v_type }

			method mk_ref_access e v =
				mk (TArray ({ e with etype = v.v_type }, mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos

			method mk_init av v pos =
				let elocal = mk (TLocal v) v.v_type pos in
				let earray = { (Optimizer.mk_untyped_call "__array__" pos [elocal]) with etype = av.v_type } in
				mk (TVar (av,Some earray)) t.tvoid pos
		end
	(* default implementation - use haxe array *)
	| _ ->
		object
			method captured_type = t.tarray
			method mk_ref v ve p =
				mk (TArrayDecl (match ve with None -> [] | Some e -> [e])) v.v_type p
			method mk_ref_access e v =
				mk (TArray ({ e with etype = v.v_type }, mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos
			method mk_init av v pos =
				mk (TVar (av,Some (mk (TArrayDecl [mk (TLocal v) v.v_type pos]) av.v_type pos))) t.tvoid pos
		end
	in

	let mk_var v used =
		let v2 = alloc_var v.v_name (PMap.find v.v_id used) in
		v2.v_meta <- v.v_meta;
		v2
	in

	let rec wrap used e =
		match e.eexpr with
		| TVar (v,ve) ->
			let v,ve =
				if PMap.mem v.v_id used then
					v, Some (impl#mk_ref v (Option.map (wrap used) ve) e.epos)
				else
					v, (match ve with None -> None | Some e -> Some (wrap used e))
			 in
			{ e with eexpr = TVar (v,ve) }
		| TLocal v when PMap.mem v.v_id used ->
			impl#mk_ref_access e v
		| TFor (v,it,expr) when PMap.mem v.v_id used ->
			let vtmp = mk_var v used in
			let it = wrap used it in
			let expr = wrap used expr in
			mk (TFor (vtmp,it,Type.concat (impl#mk_init v vtmp e.epos) expr)) e.etype e.epos
		| TTry (expr,catchs) ->
			let catchs = List.map (fun (v,e) ->
				let e = wrap used e in
				try
					let vtmp = mk_var v used in
					vtmp, Type.concat (impl#mk_init v vtmp e.epos) e
				with Not_found ->
					v, e
			) catchs in
			mk (TTry (wrap used expr,catchs)) e.etype e.epos
		| TFunction f ->
			(*
				list variables that are marked as used, but also used in that
				function and which are not declared inside it !
			*)
			let fused = ref PMap.empty in
			let tmp_used = ref used in
			let rec browse = function
				| Block f | Loop f | Function f -> f browse
				| Use v | Assign v ->
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
					fexpr := Type.concat (impl#mk_init v vtmp e.epos) !fexpr;
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
				v.v_type <- impl#captured_type vt;
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
				| Use v | Assign v ->
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
		let assigned = ref PMap.empty in
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
			(try
				let d = PMap.find v.v_id !vars in
				if d <> !depth then used := PMap.add v.v_id v !used;
			with Not_found -> ())
		| Assign v ->
			(try
				let d = PMap.find v.v_id !vars in
				(* different depth - needs wrap *)
				if d <> !depth then begin
					used := PMap.add v.v_id v !used;
					assigned := PMap.add v.v_id v !assigned;
				end
				(* same depth but assigned after being used on a different depth - needs wrap *)
				else if PMap.mem v.v_id !used then
					assigned := PMap.add v.v_id v !assigned;
			with Not_found -> ())
		in
		local_usage collect_vars e;

		(* mark all capture variables - also used in rename_local_vars at later stage *)
		PMap.iter (fun _ v -> v.v_capture <- true) !used;

		!assigned
	in
	let captured = all_vars e in
	match com.config.pf_capture_policy with
	| CPNone -> e
	| CPWrapRef -> do_wrap captured e
	| CPLoopVars -> out_loop e

(* -------------------------------------------------------------------------- *)
(* RENAME LOCAL VARS *)

let rename_local_vars ctx e =
	let cfg = ctx.com.config in
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
		if is_gen_local v then v.v_name <- "_g" ^ String.sub v.v_name 1 (String.length v.v_name - 1);
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
			(* we have to look ahead for vars on these targets (issue #3344) *)
			begin match ctx.com.platform with
				| Js ->
					let rec check_var e = match e.eexpr with
						| TVar (v,eo) ->
							(match eo with None -> () | Some e -> loop e);
							declare v e.epos
						| TBlock _ ->
							()
						| _ ->
							Type.iter check_var e
					in
					List.iter check_var el
				| _ ->
					()
			end;
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
		| TTypeExpr t ->
			check t
		| TNew (c,_,_) ->
			Type.iter loop e;
			check (TClassDecl c);
		| TCast (e,Some t) ->
			loop e;
			check t;
		| TConst TSuper ->
			check_type e.etype
		| _ ->
			Type.iter loop e
	in
	declare (alloc_var "this" t_dynamic) Ast.null_pos; (* force renaming of 'this' vars in abstract *)
	begin match ctx.curclass.cl_path with
		| s :: _,_ | [],s -> declare (alloc_var s t_dynamic) Ast.null_pos
	end;
	loop e;
	e

let check_unification ctx e t =
	begin match e.eexpr,t with
		| TLocal v,TType({t_path = ["cs"],("Ref" | "Out")},_) ->
			(* TODO: this smells of hack, but we have to deal with it somehow *)
			v.v_capture <- true
		| _ ->
			()
	end;
	e

(* PASS 1 end *)

(* Saves a class state so it can be restored later, e.g. after DCE or native path rewrite *)
let save_class_state ctx t = match t with
	| TClassDecl c ->
		let mk_field_restore f =
			let rec mk_overload_restore f =
				f.cf_name,f.cf_kind,f.cf_expr,f.cf_type,f.cf_meta,f.cf_params
			in
			( f,mk_overload_restore f, List.map (fun f -> f,mk_overload_restore f) f.cf_overloads )
		in
		let restore_field (f,res,overloads) =
			let restore_field (f,(name,kind,expr,t,meta,params)) =
				f.cf_name <- name; f.cf_kind <- kind; f.cf_expr <- expr; f.cf_type <- t; f.cf_meta <- meta; f.cf_params <- params;
				f
			in
			let f = restore_field (f,res) in
			f.cf_overloads <- List.map restore_field overloads;
			f
		in
		let mk_pmap lst =
			List.fold_left (fun pmap f -> PMap.add f.cf_name f pmap) PMap.empty lst
		in

		let meta = c.cl_meta and path = c.cl_path and ext = c.cl_extern and over = c.cl_overrides in
		let sup = c.cl_super and impl = c.cl_implements in
		let csr = Option.map (mk_field_restore) c.cl_constructor in
		let ofr = List.map (mk_field_restore) c.cl_ordered_fields in
		let osr = List.map (mk_field_restore) c.cl_ordered_statics in
		let init = c.cl_init in
		c.cl_restore <- (fun() ->
			c.cl_super <- sup;
			c.cl_implements <- impl;
			c.cl_meta <- meta;
			c.cl_extern <- ext;
			c.cl_path <- path;
			c.cl_init <- init;
			c.cl_ordered_fields <- List.map restore_field ofr;
			c.cl_ordered_statics <- List.map restore_field osr;
			c.cl_fields <- mk_pmap c.cl_ordered_fields;
			c.cl_statics <- mk_pmap c.cl_ordered_statics;
			c.cl_constructor <- Option.map restore_field csr;
			c.cl_overrides <- over;
		)
	| _ ->
		()

(* PASS 2 begin *)

let rec is_removable_class c =
	match c.cl_kind with
	| KGeneric ->
		(Meta.has Meta.Remove c.cl_meta ||
		(match c.cl_super with
			| Some (c,_) -> is_removable_class c
			| _ -> false) ||
		List.exists (fun (_,t) -> match follow t with
			| TInst(c,_) ->
				Codegen.has_ctor_constraint c
			| _ ->
				false
		) c.cl_params)
	| KTypeParameter _ ->
		(* this shouldn't happen, have to investigate (see #4092) *)
		true
	| _ ->
		false

let remove_generic_base ctx t = match t with
	| TClassDecl c when is_removable_class c ->
		c.cl_extern <- true
	| _ ->
		()

(* Removes extern and macro fields, also checks for Void fields *)

let remove_extern_fields ctx t = match t with
	| TClassDecl c ->
		if not (Common.defined ctx.com Define.DocGen) then begin
			c.cl_ordered_fields <- List.filter (fun f ->
				let b = Codegen.is_removable_field ctx f in
				if b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
				not b
			) c.cl_ordered_fields;
			c.cl_ordered_statics <- List.filter (fun f ->
				let b = Codegen.is_removable_field ctx f in
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
	let get_native_name meta =
		let rec get_native meta = match meta with
			| [] -> raise Not_found
			| (Meta.Native,[v],p as meta) :: _ ->
				meta
			| _ :: meta ->
				get_native meta
		in
		let (_,e,mp) = get_native meta in
		match e with
		| [Ast.EConst (Ast.String name),p] ->
			name,p
		| [] ->
			raise Not_found
		| _ ->
			error "String expected" mp
	in
	let get_real_name meta name =
		let name',p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (name)), p], p), name'
	in
	let get_real_path meta path =
		let name,p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (s_type_path path)), p], p), parse_path name
	in
	try
		(match t with
		| TClassDecl c ->
			let did_change = ref false in
			let field cf = try
				let meta,name = get_real_name cf.cf_meta cf.cf_name in
				cf.cf_name <- name;
				cf.cf_meta <- meta :: cf.cf_meta;
				List.iter (fun cf -> cf.cf_name <- name) cf.cf_overloads;
				did_change := true
			with Not_found ->
				()
			in
			let fields cfs old_map =
				did_change := false;
				List.iter field cfs;
				if !did_change then
					List.fold_left (fun map f -> PMap.add f.cf_name f map) PMap.empty cfs
				else
					old_map
			in
			c.cl_fields <- fields c.cl_ordered_fields c.cl_fields;
			c.cl_statics <- fields c.cl_ordered_statics c.cl_statics;
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
	let is_as3 = Common.defined ctx.com Define.As3 && not ctx.in_macro in
	let apply c =
		let ethis = mk (TConst TThis) (TInst (c,List.map snd c.cl_params)) c.cl_pos in
		(* TODO: we have to find a variable name which is not used in any of the functions *)
		let v = alloc_var "_g" ethis.etype in
		let need_this = ref false in
		let inits,fields = List.fold_left (fun (inits,fields) cf ->
			match cf.cf_kind,cf.cf_expr with
			| Var _, Some _ ->
				if is_as3 then (inits, cf :: fields) else (cf :: inits, cf :: fields)
			| Method MethDynamic, Some e when is_as3 ->
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
			let el = List.map (fun cf ->
				match cf.cf_expr with
				| None -> assert false
				| Some e ->
					let lhs = mk (TField(ethis,FInstance (c,List.map snd c.cl_params,cf))) cf.cf_type e.epos in
					cf.cf_expr <- None;
					let eassign = mk (TBinop(OpAssign,lhs,e)) e.etype e.epos in
					if is_as3 then begin
						let echeck = mk (TBinop(OpEq,lhs,(mk (TConst TNull) lhs.etype e.epos))) ctx.com.basic.tbool e.epos in
						mk (TIf(echeck,eassign,None)) eassign.etype e.epos
					end else
						eassign;
			) inits in
			let el = if !need_this then (mk (TVar((v, Some ethis))) ethis.etype ethis.epos) :: el else el in
			match c.cl_constructor with
			| None ->
				let ct = TFun([],ctx.com.basic.tvoid) in
				let ce = mk (TFunction {
					tf_args = [];
					tf_type = ctx.com.basic.tvoid;
					tf_expr = mk (TBlock el) ctx.com.basic.tvoid c.cl_pos;
				}) ct c.cl_pos in
				let ctor = mk_field "new" ct c.cl_pos in
				ctor.cf_kind <- Method MethNormal;
				c.cl_constructor <- Some { ctor with cf_expr = Some ce };
			| Some cf ->
				match cf.cf_expr with
				| Some { eexpr = TFunction f } ->
					let bl = match f.tf_expr with {eexpr = TBlock b } -> b | x -> [x] in
					let ce = mk (TFunction {f with tf_expr = mk (TBlock (el @ bl)) ctx.com.basic.tvoid c.cl_pos }) cf.cf_type cf.cf_pos in
					c.cl_constructor <- Some {cf with cf_expr = Some ce }
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

(* Interfaces have no 'super', but can extend many other interfaces.
   This makes the first extended (implemented) interface the super for efficiency reasons (you can get one for 'free')
   and leaves the remaining ones as 'implemented' *)
let promote_first_interface_to_super ctx t = match t with
	| TClassDecl c when c.cl_interface ->
		begin match c.cl_implements with
		| ({ cl_path = ["cpp";"rtti"],_ },_ ) :: _ -> ()
		| first_interface  :: remaining ->
			c.cl_super <- Some first_interface;
			c.cl_implements <- remaining
		| _ -> ()
		end
	| _ ->
		()

let commit_features ctx t =
	let m = (t_infos t).mt_module in
	Hashtbl.iter (fun k v ->
		Common.add_feature ctx.com k;
	) m.m_extra.m_features

let check_reserved_type_paths ctx t =
	let check path pos =
		if List.mem path ctx.com.config.pf_reserved_type_paths then
			ctx.com.warning ("Type path " ^ (s_type_path path) ^ " is reserved on this target") pos
	in
	match t with
	| TClassDecl c when not c.cl_extern -> check c.cl_path c.cl_pos
	| TEnumDecl e when not e.e_extern -> check e.e_path e.e_pos
	| _ -> ()

(* PASS 3 end *)

let run_expression_filters ctx filters t =
	let run e =
		List.fold_left (fun e f -> f e) e filters
	in
	match t with
	| TClassDecl c when is_removable_class c -> ()
	| TClassDecl c ->
		ctx.curclass <- c;
		let rec process_field f =
			(match f.cf_expr with
			| Some e when not (Codegen.is_removable_field ctx f) ->
				Codegen.AbstractCast.cast_stack := f :: !Codegen.AbstractCast.cast_stack;
				f.cf_expr <- Some (run e);
				Codegen.AbstractCast.cast_stack := List.tl !Codegen.AbstractCast.cast_stack;
			| _ -> ());
			List.iter process_field f.cf_overloads
		in
		List.iter process_field c.cl_ordered_fields;
		List.iter process_field c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> process_field f);
		(match c.cl_init with
		| None -> ()
		| Some e ->
			c.cl_init <- Some (run e));
	| TEnumDecl _ -> ()
	| TTypeDecl _ -> ()
	| TAbstractDecl _ -> ()

let pp_counter = ref 1

let is_cached t =
	let m = (t_infos t).mt_module.m_extra in
	if m.m_processed = 0 then m.m_processed <- !pp_counter;
	m.m_processed <> !pp_counter

let apply_filters_once ctx filters t =
	if not (is_cached t) then run_expression_filters ctx filters t

let next_compilation() =
	incr pp_counter

let iter_expressions fl mt =
	match mt with
	| TClassDecl c ->
		let field cf = match cf.cf_expr with
			| None -> ()
			| Some e -> List.iter (fun f -> f e) fl
		in
		List.iter field c.cl_ordered_statics;
		List.iter field c.cl_ordered_fields;
		(match c.cl_constructor with None -> () | Some cf -> field cf)
	| _ ->
		()

let run com tctx main =
	begin match com.display with
		| DMUsage | DMPosition ->
			Codegen.detect_usage com;
		| _ ->
			()
	end;
	if not (Common.defined com Define.NoDeprecationWarnings) then
		Codegen.DeprecationCheck.run com;
	let use_static_analyzer = Common.defined com Define.Analyzer in
	(* this part will be a bit messy until we make the analyzer the default *)
	let new_types = List.filter (fun t -> not (is_cached t)) com.types in
	if use_static_analyzer then begin
		(* PASS 1: general expression filters *)
		let filters = [
			Codegen.UnificationCallback.run (check_unification tctx);
			Codegen.AbstractCast.handle_abstract_casts tctx;
			Optimizer.inline_constructors tctx;
			Optimizer.reduce_expression tctx;
			blockify_ast;
			captured_vars com;
		] in
		List.iter (run_expression_filters tctx filters) new_types;
		Analyzer.Run.run_on_types tctx new_types;
		List.iter (iter_expressions [verify_ast tctx]) new_types;
		let filters = [
			Optimizer.sanitize com;
			if com.config.pf_add_final_return then add_final_return else (fun e -> e);
			if com.platform = Js then wrap_js_exceptions com else (fun e -> e);
			rename_local_vars tctx;
		] in
		List.iter (run_expression_filters tctx filters) new_types;
	end else begin
		(* PASS 1: general expression filters *)
		let filters = [
			Codegen.UnificationCallback.run (check_unification tctx);
			Codegen.AbstractCast.handle_abstract_casts tctx;
			blockify_ast;
			check_local_vars_init;
			( if (Common.defined com Define.NoSimplify) || (Common.defined com Define.Cppia) ||
						( match com.platform with Cpp -> false | _ -> true ) then
					fun e -> e
				else
					fun e ->
						let save = save_locals tctx in
						let timer = timer "analyzer-simplify-apply" in
						let e = try snd (Analyzer.Simplifier.apply com e) with Exit -> e in
						timer();
						save();
					e );
			if com.foptimize then (fun e -> Optimizer.reduce_expression tctx (Optimizer.inline_constructors tctx e)) else Optimizer.sanitize com;
			captured_vars com;
			promote_complex_rhs com;
			if com.config.pf_add_final_return then add_final_return else (fun e -> e);
			if com.platform = Js then wrap_js_exceptions com else (fun e -> e);
			rename_local_vars tctx;
		] in
		List.iter (run_expression_filters tctx filters) new_types;
		List.iter (iter_expressions [verify_ast tctx]) new_types;
	end;
	next_compilation();
	List.iter (fun f -> f()) (List.rev com.filters); (* macros onGenerate etc. *)
	List.iter (save_class_state tctx) new_types;
	List.iter (fun t ->
		remove_generic_base tctx t;
		remove_extern_fields tctx t;
	) com.types;
	(* update cache dependencies before DCE is run *)
	Codegen.update_cache_dependencies com;
	(* check @:remove metadata before DCE so it is ignored there (issue #2923) *)
	List.iter (check_remove_metadata tctx) com.types;
	(* DCE *)
	let dce_mode = if Common.defined com Define.As3 then
		"no"
	else
		(try Common.defined_value com Define.Dce with _ -> "no")
	in
	begin match dce_mode with
		| "full" -> Dce.run com main (not (Common.defined com Define.Interp))
		| "std" -> Dce.run com main false
		| "no" -> Dce.fix_accessors com
		| _ -> failwith ("Unknown DCE mode " ^ dce_mode)
	end;
	(* always filter empty abstract implementation classes (issue #1885) *)
	List.iter (fun mt -> match mt with
		| TClassDecl({cl_kind = KAbstractImpl _} as c) when c.cl_ordered_statics = [] && c.cl_ordered_fields = [] && not (Meta.has Meta.Used c.cl_meta) ->
			c.cl_extern <- true
		| TClassDecl({cl_kind = KAbstractImpl a} as c) when Meta.has Meta.Enum a.a_meta ->
			let is_runtime_field cf =
				not (Meta.has Meta.Enum cf.cf_meta)
			in
			(* also filter abstract implementation classes that have only @:enum fields (issue #2858) *)
			if not (List.exists is_runtime_field c.cl_ordered_statics) then
				c.cl_extern <- true
		| _ -> ()
	) com.types;
	(* PASS 3: type filters *)
	let type_filters = [
		check_private_path;
		apply_native_paths;
		add_rtti;
		(match com.platform with | Java | Cs -> (fun _ _ -> ()) | _ -> add_field_inits);
		add_meta_field;
		check_void_field;
		(match com.platform with | Cpp -> promote_first_interface_to_super | _ -> (fun _ _ -> ()) );
		commit_features;
		(if com.config.pf_reserved_type_paths <> [] then check_reserved_type_paths else (fun _ _ -> ()));
	] in
	List.iter (fun t -> List.iter (fun f -> f tctx t) type_filters) com.types
