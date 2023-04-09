(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

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
open Globals
open Type
open Common
open LocalUsage

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
let captured_vars com e =
	let t = com.basic in

	let impl = match com.platform with
	(* optimized version for C#/Java - use native arrays *)
	| Jvm ->
		let cnativearray =
			match (List.find (fun md -> match md with
					| TClassDecl ({ cl_path = ["cs"|"java"],"NativeArray" }) -> true
					| _ -> false
				) com.types)
			with TClassDecl cl -> cl | _ -> die "" __LOC__
		in

		object
			method captured_type t = TInst (cnativearray,[t])

			method mk_ref v ve p =
				match ve with
				| None ->
					let eone = mk (TConst (TInt (Int32.of_int 1))) t.tint p in
					let t = match v.v_type with TInst (_, [t]) -> t | _ -> die "" __LOC__ in
					mk (TNew (cnativearray,[t],[eone])) v.v_type p
				| Some e ->
					{ (Inline.mk_untyped_call "__array__" p [e]) with etype = v.v_type }

			method mk_ref_access e v =
				mk (TArray ({ e with etype = v.v_type }, mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos

			method mk_init av v pos =
				let elocal = mk (TLocal v) v.v_type pos in
				let earray = { (Inline.mk_untyped_call "__array__" pos [elocal]) with etype = av.v_type } in
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
		let v2 = alloc_var v.v_kind v.v_name (PMap.find v.v_id used) v.v_pos in
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
				let new_vars = List.map (fun v -> v.v_id,alloc_var v.v_kind v.v_name v.v_type v.v_pos) vars in
				let rec loop e = match e.eexpr with
					| TLocal v ->
						begin try
							let v' = List.assoc v.v_id new_vars in
							add_var_flag v' VCaptured;
							{e with eexpr = TLocal v'}
						with Not_found ->
							e
						end
					| _ ->
						Type.map_expr loop e
				in
				let e = loop e in
				mk (TCall (
					Texpr.Builder.mk_parent (mk (TFunction {
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
				add_var_flag v VCaptured;
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
				if d <> !depth then begin
					used := PMap.add v.v_id v !used;
					if has_var_flag v VAssigned then assigned := PMap.add v.v_id v !assigned;
				end
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
					assigned := PMap.add v.v_id v !assigned
				else
					add_var_flag v VAssigned;
			with Not_found -> ())
		in
		local_usage collect_vars e;

		(* mark all capture variables - also used in rename_local_vars at later stage *)
		PMap.iter (fun _ v -> add_var_flag v VCaptured) !used;

		!assigned
	in
	let captured = all_vars e in
	match com.config.pf_capture_policy with
	| CPNone -> e
	| CPWrapRef -> do_wrap captured e
	| CPLoopVars -> out_loop e
