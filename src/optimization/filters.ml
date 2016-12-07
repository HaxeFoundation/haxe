(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Ast
open Common
open Type
open Typecore
open Error
open Globals

(* PASS 1 begin *)

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
			{ eexpr = TReturn (Some { eexpr = TConst c; epos = p; etype = t }); etype = t_dynamic; epos = p }
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
				| _ -> { f with tf_expr = loop f.tf_expr f.tf_type }
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
			(match eerr.etype with
			| TDynamic _ ->
				let eterr = Codegen.ExprBuilder.make_static_this cerr e.epos in
				let ewrap = Codegen.fcall eterr "wrap" [eerr] t_dynamic e.epos in
				{ e with eexpr = TThrow ewrap }
			| _ ->
				let ewrap = { eerr with eexpr = TNew (cerr,[],[eerr]); etype = TInst (cerr,[]) } in
				{ e with eexpr = TThrow ewrap }
			)
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
	let outside_vars = ref IntMap.empty in
	let rec loop vars e =
		match e.eexpr with
		| TLocal v ->
			let init = (try PMap.find v.v_id !vars with Not_found -> true) in
			if not init && not (IntMap.mem v.v_id !outside_vars) then begin
				if v.v_name = "this" then error "Missing this = value" e.epos
				else error ("Local variable " ^ v.v_name ^ " used without being initialized") e.epos
			end
		| TVar (v,eo) ->
			begin
				match eo with
				| None when Meta.has Meta.InlineConstructorVariable v.v_meta ->
					()
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
		| TFunction tf ->
			let old = !outside_vars in
			(* Mark all known variables as "outside" so we can ignore their initialization state within the function.
			   We cannot use `vars` directly because we still care about initializations the function might make.
			*)
			PMap.iter (fun i _ -> outside_vars := IntMap.add i true !outside_vars) !vars;
			loop vars tf.tf_expr;
			outside_vars := old;
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
		let v2 = alloc_var v.v_name (PMap.find v.v_id used) v.v_pos in
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
				(* We don't want to duplicate any variable declarations, so let's make copies (issue #3902). *)
				let new_vars = List.map (fun v -> v.v_id,alloc_var v.v_name v.v_type v.v_pos) vars in
				let rec loop e = match e.eexpr with
					| TLocal v ->
						begin try
							let v' = List.assoc v.v_id new_vars in
							v'.v_capture <- true;
							{e with eexpr = TLocal v'}
						with Not_found ->
							e
						end
					| _ ->
						Type.map_expr loop e
				in
				let e = loop e in
				mk (TCall (
					Codegen.mk_parent (mk (TFunction {
						tf_args = List.map (fun (_,v) -> v, None) new_vars;
						tf_type = e.etype;
						tf_expr = mk_block (mk (TReturn (Some e)) e.etype e.epos);
					}) (TFun (List.map (fun (_,v) -> v.v_name,false,v.v_type) new_vars,e.etype)) e.epos),
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
	let vars = ref [] in
	let declare v =
		vars := v :: !vars
	in
	let reserved = ref StringMap.empty in
	let reserve name =
		reserved := StringMap.add name true !reserved
	in
	let check t =
		match (t_infos t).mt_path with
		| [], name | name :: _, _ -> reserve name
	in
	let check_type t =
		match follow t with
		| TInst (c,_) -> check (TClassDecl c)
		| TEnum (e,_) -> check (TEnumDecl e)
		| TType (t,_) -> check (TTypeDecl t)
		| TAbstract (a,_) -> check (TAbstractDecl a)
		| TMono _ | TLazy _ | TAnon _ | TDynamic _ | TFun _ -> ()
	in
	let rec collect e = match e.eexpr with
 		| TVar(v,eo) ->
			declare v;
			(match eo with None -> () | Some e -> collect e)
		| TFor(v,e1,e2) ->
			declare v;
			collect e1;
			collect e2;
		| TTry(e1,catches) ->
			collect e1;
			List.iter (fun (v,e) ->
				declare v;
				check_type v.v_type;
				collect e
			) catches
		| TFunction tf ->
			List.iter (fun (v,_) -> declare v) tf.tf_args;
			collect tf.tf_expr
		| TTypeExpr t ->
			check t
		| TNew (c,_,_) ->
			Type.iter collect e;
			check (TClassDecl c);
		| TCast (e,Some t) ->
			collect e;
			check t;
		| TConst TSuper ->
			check_type e.etype
		| _ ->
			Type.iter collect e
	in
	(* Pass 1: Collect used identifiers and variables. *)
	reserve "this";
	if ctx.com.platform = Java then reserve "_";
	begin match ctx.curclass.cl_path with
		| s :: _,_ | [],s -> reserve s
	end;
	collect e;
	(* Pass 2: Check and rename variables. *)
	let count_table = Hashtbl.create 0 in
	let maybe_rename v =
		(* chop escape char for all local variables generated *)
		if is_gen_local v then v.v_name <- "_g" ^ String.sub v.v_name 1 (String.length v.v_name - 1);
		let name = ref v.v_name in
		let count = ref (try Hashtbl.find count_table v.v_name with Not_found -> 0) in
		while StringMap.mem !name !reserved do
			incr count;
			name := v.v_name ^ (string_of_int !count);
		done;
		reserve !name;
		Hashtbl.replace count_table v.v_name !count;
		if not (Meta.has Meta.RealPath v.v_meta) then
			v.v_meta <- (Meta.RealPath,[EConst (String v.v_name),e.epos],e.epos) :: v.v_meta;
		v.v_name <- !name;
	in
	List.iter maybe_rename (List.rev !vars);
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
				has_ctor_constraint c || Meta.has Meta.Const c.cl_meta
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
		let f = mk_field "__rtti" ctx.t.tstring c.cl_pos null_pos in
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
		let v = alloc_var "_g" ethis.etype ethis.epos in
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
			let cf = match c.cl_constructor with
			| None ->
				let ct = TFun([],ctx.com.basic.tvoid) in
				let ce = mk (TFunction {
					tf_args = [];
					tf_type = ctx.com.basic.tvoid;
					tf_expr = mk (TBlock el) ctx.com.basic.tvoid c.cl_pos;
				}) ct c.cl_pos in
				let ctor = mk_field "new" ct c.cl_pos null_pos in
				ctor.cf_kind <- Method MethNormal;
				{ ctor with cf_expr = Some ce }
			| Some cf ->
				match cf.cf_expr with
				| Some { eexpr = TFunction f } ->
					let bl = match f.tf_expr with {eexpr = TBlock b } -> b | x -> [x] in
					let ce = mk (TFunction {f with tf_expr = mk (TBlock (el @ bl)) ctx.com.basic.tvoid c.cl_pos }) cf.cf_type cf.cf_pos in
					{cf with cf_expr = Some ce };
				| _ ->
					assert false
			in
			let config = AnalyzerConfig.get_field_config ctx.com c cf in
			Analyzer.Run.run_on_field ctx config c cf;
			(match cf.cf_expr with
			| Some e ->
				(* This seems a bit expensive, but hopefully constructor expressions aren't that massive. *)
				let e = rename_local_vars ctx e in
				let e = Optimizer.sanitize ctx.com e in
				cf.cf_expr <- Some e
			| _ ->
				());
			c.cl_constructor <- Some cf
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
			add_feature ctx.com "has_metadata";
			let f = mk_field "__meta__" t_dynamic c.cl_pos null_pos in
			f.cf_expr <- Some e;
			let can_deal_with_interface_metadata () = match ctx.com.platform with
				| Flash when Common.defined ctx.com Define.As3 -> false
				| Php when not (Common.php7 ctx.com) -> false
				| _ -> true
			in
			if c.cl_interface && not (can_deal_with_interface_metadata()) then begin
				(* borrowed from gencommon, but I did wash my hands afterwards *)
				let path = fst c.cl_path,snd c.cl_path ^ "_HxMeta" in
				let ncls = mk_class c.cl_module path c.cl_pos null_pos in
				let cf = mk_field "__meta__" e.etype e.epos null_pos in
				cf.cf_expr <- Some e;
				ncls.cl_statics <- PMap.add "__meta__" cf ncls.cl_statics;
				ncls.cl_ordered_statics <- cf :: ncls.cl_ordered_statics;
				ctx.com.types <- (TClassDecl ncls) :: ctx.com.types;
				c.cl_meta <- (Meta.Custom ":hasMetadata",[],e.epos) :: c.cl_meta
			end else begin
				c.cl_ordered_statics <- f :: c.cl_ordered_statics;
				c.cl_statics <- PMap.add f.cf_name f c.cl_statics
			end)
	| _ ->
		()

(*
	C# events have special semantics:
	if we have an @:event var field, there should also be add_<name> and remove_<name> methods,
	this filter checks for their existence and also adds some metadata for analyzer and C# generator
*)
let check_cs_events com t = match t with
	| TClassDecl cl when not cl.cl_extern ->
		let check fields f =
			match f.cf_kind with
			| Var { v_read = AccNormal; v_write = AccNormal } when Meta.has Meta.Event f.cf_meta ->
				if f.cf_public then error "@:event fields must be private" f.cf_pos;

				(* prevent generating reflect helpers for the event in gencommon *)
				f.cf_meta <- (Meta.SkipReflection, [], f.cf_pos) :: f.cf_meta;

				(* type for both add and remove methods *)
				let tmeth = (tfun [f.cf_type] com.basic.tvoid) in

				let process_event_method name =
					let m = try PMap.find name fields with Not_found -> error ("Missing event method: " ^ name) f.cf_pos in

					(* check method signature *)
					begin
						try
							type_eq EqStrict m.cf_type tmeth
						with Unify_error el ->
							List.iter (fun e -> com.error (unify_error_msg (print_context()) e) m.cf_pos) el
					end;

					(*
						add @:pure(false) to prevent purity inference, because empty add/remove methods
						have special meaning here and they are always impure
					*)
					m.cf_meta <- (Meta.Pure,[EConst(Ident "false"),f.cf_pos],f.cf_pos) :: (Meta.Custom ":cs_event_impl",[],f.cf_pos) :: m.cf_meta;

					(* add @:keep to event methods if the event is kept *)
					if Meta.has Meta.Keep f.cf_meta && not (Meta.has Meta.Keep m.cf_meta) then
						m.cf_meta <- (Meta.Keep,[],f.cf_pos) :: m.cf_meta;
				in
				process_event_method ("add_" ^ f.cf_name);
				process_event_method ("remove_" ^ f.cf_name)
			| _ ->
				()
		in
		List.iter (check cl.cl_fields) cl.cl_ordered_fields;
		List.iter (check cl.cl_statics) cl.cl_ordered_statics
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
			| Some e when not (is_removable_field ctx f) ->
				AbstractCast.cast_stack := f :: !AbstractCast.cast_stack;
				f.cf_expr <- Some (run e);
				AbstractCast.cast_stack := List.tl !AbstractCast.cast_stack;
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
	let new_types = List.filter (fun t -> not (is_cached t)) com.types in
	(* PASS 1: general expression filters *)
	let filters = [
		AbstractCast.handle_abstract_casts tctx;
		Optimizer.inline_constructors tctx;
		check_local_vars_init;
		Optimizer.reduce_expression tctx;
		captured_vars com;
	] in
	List.iter (run_expression_filters tctx filters) new_types;
	(* PASS 1.5: pre-analyzer type filters *)
	List.iter (fun t ->
		if com.platform = Cs then check_cs_events tctx.com t;
	) new_types;
	if com.platform <> Cross then Analyzer.Run.run_on_types tctx new_types;
	let filters = [
		Optimizer.sanitize com;
		if com.config.pf_add_final_return then add_final_return else (fun e -> e);
		if com.platform = Js then wrap_js_exceptions com else (fun e -> e);
		rename_local_vars tctx;
	] in
	List.iter (run_expression_filters tctx filters) new_types;
	next_compilation();
	List.iter (fun f -> f()) (List.rev com.callbacks.before_dce); (* macros onGenerate etc. *)
	List.iter (save_class_state tctx) new_types;
	(* PASS 2: type filters pre-DCE *)
	List.iter (fun t ->
		remove_generic_base tctx t;
		remove_extern_fields tctx t;
		Codegen.update_cache_dependencies t;
		(* check @:remove metadata before DCE so it is ignored there (issue #2923) *)
		check_remove_metadata tctx t;
	) com.types;
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
	(* PASS 3: type filters post-DCE *)
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
