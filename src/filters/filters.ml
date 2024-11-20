(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
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
open FiltersCommon

let get_native_name = Naming.get_native_name

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

(* -------------------------------------------------------------------------- *)
(* CHECK LOCAL VARS INIT *)

let check_local_vars_init ctx e =
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
	(* Set variables which belong to current function *)
	let set_all_vars vars =
		vars := PMap.mapi (fun id is_set -> if IntMap.mem id !outside_vars then is_set else true) !vars
	in
	let rec loop vars e =
		match e.eexpr with
		| TLocal v ->
			let init = (try PMap.find v.v_id !vars with Not_found -> true) in
			if not init then begin
				if IntMap.mem v.v_id !outside_vars then
					if v.v_name = "this" then warning ctx WVarInit "this might be used before assigning a value to it" e.epos
					else warning ctx WVarInit ("Local variable " ^ v.v_name ^ " might be used before being initialized") e.epos
				else
					if v.v_name = "this" then raise_typing_error "Missing this = value" e.epos
					else raise_typing_error ("Local variable " ^ v.v_name ^ " used without being initialized") e.epos
			end
		| TVar (v,eo) ->
			begin
				match eo with
				| None when (match v.v_kind with VInlinedConstructorVariable _ -> true | _ -> false) ->
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
			begin match (Texpr.skip e).eexpr with
				| TFunction _ ->
					(* We can be sure that the function doesn't execute immediately, so it's fine to
					   consider the local initialized (issue #9919). *)
					vars := PMap.add v.v_id true !vars;
					loop vars e;
				| _ ->
					loop vars e;
					vars := PMap.add v.v_id true !vars
			end
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
		| TSwitch ({switch_subject = e;switch_cases = cases;switch_default = def} as switch) ->
			loop vars e;
			let cvars = List.map (fun {case_patterns = ec;case_expr = e} ->
				let old = !vars in
				List.iter (loop vars) ec;
				vars := old;
				loop vars e;
				let v = !vars in
				vars := old;
				v
			) cases in
			(match def with
			| None when switch.switch_exhaustive ->
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
			set_all_vars vars
		| TThrow e | TReturn (Some e) ->
			loop vars e;
			set_all_vars vars
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

let mark_switch_break_loops e =
	let add_loop_label n e =
		{ e with eexpr = TMeta ((Meta.LoopLabel,[(EConst(Int(string_of_int n, None)),e.epos)],e.epos), e) }
	in
	let in_switch = ref false in
	let did_found = ref (-1) in
	let num = ref 0 in
	let cur_num = ref 0 in
	let rec run e =
		match e.eexpr with
		| TFunction _ ->
			let old_num = !num in
			num := 0;
				let ret = Type.map_expr run e in
			num := old_num;
			ret
		| TWhile _ | TFor _ ->
			let last_switch = !in_switch in
			let last_found = !did_found in
			let last_num = !cur_num in
			in_switch := false;
			incr num;
			cur_num := !num;
			did_found := -1;
				let new_e = Type.map_expr run e in (* assuming that no loop will be found in the condition *)
				let new_e = if !did_found <> -1 then add_loop_label !did_found new_e else new_e in
			did_found := last_found;
			in_switch := last_switch;
			cur_num := last_num;

			new_e
		| TSwitch _ ->
			let last_switch = !in_switch in
			in_switch := true;
				let new_e = Type.map_expr run e in
			in_switch := last_switch;
			new_e
		| TBreak ->
			if !in_switch then (
				did_found := !cur_num;
				add_loop_label !cur_num e
			) else
				e
		| _ -> Type.map_expr run e
	in
	run e

let rec fix_return_dynamic_from_void_function return_is_void e =
	match e.eexpr with
	| TFunction fn ->
		let is_void = ExtType.is_void (follow fn.tf_type) in
		let body = fix_return_dynamic_from_void_function is_void fn.tf_expr in
		{ e with eexpr = TFunction { fn with tf_expr = body } }
	| TReturn (Some return_expr) when return_is_void && t_dynamic == follow return_expr.etype ->
		let return_pos = { e.epos with pmax = return_expr.epos.pmin - 1 } in
		let exprs = [
			fix_return_dynamic_from_void_function return_is_void return_expr;
			{ e with eexpr = TReturn None; epos = return_pos };
		] in
		{ e with
			eexpr = TMeta (
				(Meta.MergeBlock, [], null_pos),
				mk (TBlock exprs) e.etype e.epos
			);
		}
	| _ -> Type.map_expr (fix_return_dynamic_from_void_function return_is_void) e

let check_abstract_as_value e =
	let rec loop e =
		match e.eexpr with
		| TField ({ eexpr = TTypeExpr _ }, _) -> ()
		| TTypeExpr(TClassDecl {cl_kind = KAbstractImpl a}) when not (Meta.has Meta.RuntimeValue a.a_meta) ->
			raise_typing_error "Cannot use abstract as value" e.epos
		| _ -> Type.iter loop e
	in
	loop e;
	e

(* PASS 1 end *)

(* PASS 2 begin *)

(* Applies exclude macro (which turns types into externs) *)

let apply_macro_exclude com t = match t with
	| TClassDecl c when has_class_flag c CExcluded ->
		add_class_flag c CExtern
	| TEnumDecl e when has_enum_flag e EnExcluded ->
		add_enum_flag e EnExtern
	| _ ->
		()

(* Removes extern and macro fields, also checks for Void fields *)

let remove_extern_fields com t = match t with
	| TClassDecl c ->
		if not (Common.defined com Define.DocGen) then begin
			c.cl_ordered_fields <- List.filter (fun f ->
				let b = is_removable_field com f in
				if b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
				not b
			) c.cl_ordered_fields;
			c.cl_ordered_statics <- List.filter (fun f ->
				let b = is_removable_field com f in
				if b then c.cl_statics <- PMap.remove f.cf_name c.cl_statics;
				not b
			) c.cl_ordered_statics;
		end
	| _ ->
		()

(* PASS 2 end *)

(* PASS 3 begin *)

(* Checks if a private class' path clashes with another path *)
let check_private_path com t = match t with
	| TClassDecl c when c.cl_private ->
		let rpath = (fst c.cl_module.m_path,"_" ^ snd c.cl_module.m_path) in
		if com.module_lut#get_type_lut#mem rpath then raise_typing_error ("This private class name will clash with " ^ s_type_path rpath) c.cl_pos;
	| _ ->
		()

(* Adds the __rtti field if required *)
let add_rtti com t =
	let rec has_rtti c =
		Meta.has Meta.Rtti c.cl_meta || match c.cl_super with None -> false | Some (csup,_) -> has_rtti csup
	in
	match t with
	| TClassDecl c when has_rtti c && not (PMap.mem "__rtti" c.cl_statics) ->
		let f = mk_field ~static:true "__rtti" com.basic.tstring c.cl_pos null_pos in
		let str = Genxml.gen_type_string com t in
		f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
		c.cl_ordered_statics <- f :: c.cl_ordered_statics;
		c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
	| _ ->
		()

(* Adds the __meta__ field if required *)
let add_meta_field com t = match t with
	| TClassDecl c ->
		(match Texpr.build_metadata com.basic t with
		| None -> ()
		| Some e ->
			add_feature com "has_metadata";
			let cf = mk_field ~static:true "__meta__" e.etype e.epos null_pos in
			cf.cf_expr <- Some e;
			let can_deal_with_interface_metadata () = match com.platform with
				| Jvm -> false
				| _ -> true
			in
			if (has_class_flag c CInterface) && not (can_deal_with_interface_metadata()) then begin
				(* borrowed from gencommon, but I did wash my hands afterwards *)
				let path = fst c.cl_path,snd c.cl_path ^ "_HxMeta" in
				let ncls = mk_class c.cl_module path c.cl_pos null_pos in
				ncls.cl_ordered_statics <- cf :: ncls.cl_ordered_statics;
				ncls.cl_statics <- PMap.add cf.cf_name cf ncls.cl_statics;
				com.types <- com.types @ [ TClassDecl ncls ];
				c.cl_meta <- (Meta.Custom ":hasMetadata",[],e.epos) :: c.cl_meta
			end else begin
				c.cl_ordered_statics <- cf :: c.cl_ordered_statics;
				c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics
			end)
	| _ ->
		()

(* Removes interfaces tagged with @:remove metadata *)
let check_remove_metadata t = match t with
	| TClassDecl c ->
		c.cl_implements <- List.filter (fun (c,_) -> not (Meta.has Meta.Remove c.cl_meta)) c.cl_implements;
	| _ ->
		()

(* Checks for Void class fields *)
let check_void_field t = match t with
	| TClassDecl c ->
		let check f =
			match follow f.cf_type with TAbstract({a_path=[],"Void"},_) -> raise_typing_error "Fields of type Void are not allowed" f.cf_pos | _ -> ();
		in
		List.iter check c.cl_ordered_fields;
		List.iter check c.cl_ordered_statics;
	| _ ->
		()

(* Interfaces have no 'super', but can extend many other interfaces.
   This makes the first extended (implemented) interface the super for efficiency reasons (you can get one for 'free')
   and leaves the remaining ones as 'implemented' *)
let promote_first_interface_to_super t = match t with
	| TClassDecl c when (has_class_flag c CInterface) ->
		begin match c.cl_implements with
		| ({ cl_path = ["cpp";"rtti"],_ },_ ) :: _ -> ()
		| first_interface  :: remaining ->
			c.cl_super <- Some first_interface;
			c.cl_implements <- remaining
		| _ -> ()
		end
	| _ ->
		()

let commit_features com t =
	let m = (t_infos t).mt_module in
	Hashtbl.iter (fun k v ->
		Common.add_feature com k;
	) m.m_extra.m_features

let check_reserved_type_paths com t =
	let check path pos =
		if List.mem path com.config.pf_reserved_type_paths then begin
			com.warning WReservedTypePath [] ("Type path " ^ (s_type_path path) ^ " is reserved on this target") pos
		end
	in
	match t with
	| TClassDecl c when not (has_class_flag c CExtern) -> check c.cl_path c.cl_pos
	| TEnumDecl e when not (has_enum_flag e EnExtern) -> check e.e_path e.e_pos
	| _ -> ()

(* PASS 3 end *)

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

module ForRemap = struct
	let apply ctx e =
		let rec loop e = match e.eexpr with
		| TFor(v,e1,e2) ->
			let e1 = loop e1 in
			let e2 = loop e2 in
			let iterator = ForLoop.IterationKind.of_texpr ctx e1 (ForLoop.get_unroll_params_t ctx e2) e.epos in
			let restore = save_locals ctx in
			let e = ForLoop.IterationKind.to_texpr ctx v iterator e2 e.epos in
			restore();
			begin match e.eexpr with
			| TFor _ -> for_remap ctx.com.basic v e1 e2 e.epos
			| _ -> e
			end
		| _ ->
			Type.map_expr loop e
		in
		loop e
end

open FilterContext

let destruction tctx detail_times main locals =
	let com = tctx.com in
	with_timer detail_times "type 2" None (fun () ->
		(* PASS 2: type filters pre-DCE *)
		List.iter (fun t ->
			FiltersCommon.remove_generic_base t;
			apply_macro_exclude com t;
			remove_extern_fields com t;
			(* check @:remove metadata before DCE so it is ignored there (issue #2923) *)
			check_remove_metadata t;
		) com.types;
	);
	enter_stage com CDceStart;
	with_timer detail_times "dce" None (fun () ->
		(* DCE *)
		let dce_mode = try Common.defined_value com Define.Dce with _ -> "no" in
		let dce_mode = match dce_mode with
			| "full" -> if Common.defined com Define.Interp then Dce.DceNo else DceFull
			| "std" -> DceStd
			| "no" -> DceNo
			| _ -> failwith ("Unknown DCE mode " ^ dce_mode)
		in
		Dce.run com main dce_mode;
	);
	enter_stage com CDceDone;
	(* PASS 3: type filters post-DCE *)
	List.iter
		(run_expression_filters
			~ignore_processed_status:true
			tctx
			detail_times
			(* This has to run after DCE, or otherwise its condition always holds. *)
			["insert_save_stacks",Exceptions.insert_save_stacks tctx]
		)
		com.types;
	let type_filters = [
		Exceptions.patch_constructors tctx; (* TODO: I don't believe this should load_instance anything at this point... *)
		check_private_path com;
		Naming.apply_native_paths;
		add_rtti com;
		(match com.platform with | Jvm -> (fun _ -> ()) | _ -> (fun mt -> AddFieldInits.add_field_inits tctx.c.curclass.cl_path locals com mt));
		(match com.platform with Hl -> (fun _ -> ()) | _ -> add_meta_field com);
		check_void_field;
		(match com.platform with | Cpp -> promote_first_interface_to_super | _ -> (fun _ -> ()));
		commit_features com;
		(if com.config.pf_reserved_type_paths <> [] then check_reserved_type_paths com else (fun _ -> ()));
	] in
	with_timer detail_times "type 3" None (fun () ->
		List.iter (fun t ->
			begin match t with
			| TClassDecl c ->
				tctx.c.curclass <- c
			| _ ->
				()
			end;
			List.iter (fun f -> f t) type_filters
		) com.types;
	);
	com.callbacks#run com.error_ext com.callbacks#get_after_filters;
	enter_stage com CFilteringDone

let update_cache_dependencies ~close_monomorphs com t =
	let visited_anons = ref [] in
	let rec check_t m t = match t with
		| TInst(c,tl) ->
			add_dependency m c.cl_module MDepFromTyping;
			List.iter (check_t m) tl;
		| TEnum(en,tl) ->
			add_dependency m en.e_module MDepFromTyping;
			List.iter (check_t m) tl;
		| TType(t,tl) ->
			add_dependency m t.t_module MDepFromTyping;
			List.iter (check_t m) tl;
		| TAbstract(a,tl) ->
			add_dependency m a.a_module MDepFromTyping;
			List.iter (check_t m) tl;
		| TFun(targs,tret) ->
			List.iter (fun (_,_,t) -> check_t m t) targs;
			check_t m tret;
		| TAnon an ->
			if not (List.memq an !visited_anons) then begin
				visited_anons := an :: !visited_anons;
				PMap.iter (fun _ cf -> check_t m cf.cf_type) an.a_fields
			end
		| TMono r ->
			begin match r.tm_type with
				| Some t ->
					check_t m t
				| _ ->
					(* Bind any still open monomorph that's part of a signature to Any now (issue #10653) *)
					if close_monomorphs then Monomorph.do_bind r com.basic.tany;
		end
		| TLazy f ->
			check_t m (lazy_type f)
		| TDynamic None ->
			()
		| TDynamic (Some t) ->
			check_t m t
	in
	let rec check_field m cf =
		check_t m cf.cf_type;
		List.iter (check_field m) cf.cf_overloads
	in
	match t with
		| TClassDecl c ->
			List.iter (check_field c.cl_module) c.cl_ordered_statics;
			List.iter (check_field c.cl_module) c.cl_ordered_fields;
			(match c.cl_constructor with None -> () | Some cf -> check_field c.cl_module cf);
		| _ ->
			()

(* Saves a class state so it can be restored later, e.g. after DCE or native path rewrite *)
let save_class_state com t =
	(* Update m_processed here. This means that nothing should add a dependency afterwards because
	   then the module is immediately considered uncached again *)
	(t_infos t).mt_module.m_extra.m_processed <- com.compilation_step;
	match t with
	| TClassDecl c ->
		let vars = ref [] in
		let rec save_vars e =
			let add v = vars := (v, v.v_type) :: !vars in
			match e.eexpr with
				| TFunction fn ->
					List.iter (fun (v, _) -> add v) fn.tf_args;
					save_vars fn.tf_expr
				| TVar (v, e) ->
					add v;
					Option.may save_vars e
				| _ ->
					iter save_vars e
		in
		let mk_field_restore f =
			Option.may save_vars f.cf_expr;
			let mk_overload_restore f =
				add_class_field_flag f CfPostProcessed;
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

		let meta = c.cl_meta and path = c.cl_path and ext = (has_class_flag c CExtern) in
		let sup = c.cl_super and impl = c.cl_implements in
		let csr = Option.map (mk_field_restore) c.cl_constructor in
		let ofr = List.map (mk_field_restore) c.cl_ordered_fields in
		let osr = List.map (mk_field_restore) c.cl_ordered_statics in
		let init = Option.map mk_field_restore c.cl_init in
		c.cl_restore <- (fun() ->
			c.cl_super <- sup;
			c.cl_implements <- impl;
			c.cl_meta <- meta;
			if ext then add_class_flag c CExtern else remove_class_flag c CExtern;
			c.cl_path <- path;
			c.cl_init <- Option.map restore_field init;
			c.cl_ordered_fields <- List.map restore_field ofr;
			c.cl_ordered_statics <- List.map restore_field osr;
			c.cl_fields <- mk_pmap c.cl_ordered_fields;
			c.cl_statics <- mk_pmap c.cl_ordered_statics;
			c.cl_constructor <- Option.map restore_field csr;
			c.cl_descendants <- [];
			List.iter (fun (v, t) -> v.v_type <- t) !vars;
		)
	| TEnumDecl en ->
		let path = en.e_path in
		en.e_restore <- (fun () ->
			let rec loop acc = function
				| [] ->
					en.e_path <- path;
				| (Meta.RealPath,[Ast.EConst (Ast.String(path,_)),_],_) :: l ->
					en.e_path <- Ast.parse_path path;
					en.e_meta <- (List.rev acc) @ l;
				| x :: l -> loop (x::acc) l
			in
			loop [] en.e_meta
		)
	| TTypeDecl td ->
		let path = td.t_path in
		td.t_restore <- (fun () ->
			td.t_path <- path
		);
	| TAbstractDecl a ->
		let path = a.a_path in
		a.a_restore <- (fun () ->
			a.a_path <- path;
			a.a_meta <- List.filter (fun (m,_,_) -> m <> Meta.ValueUsed) a.a_meta
		)

let might_need_cf_unoptimized c cf =
	match cf.cf_kind,c.cl_kind with
	| Method MethInline,_ ->
		true
	| _,KGeneric ->
		true
	| _ ->
		has_class_field_flag cf CfGeneric

let run tctx main before_destruction =
	let com = tctx.com in
	let detail_times = (try int_of_string (Common.defined_value_safe com ~default:"0" Define.FilterTimes) with _ -> 0) in
	let new_types = List.filter (fun t ->
		let cached = is_cached com t in
		begin match t with
			| TClassDecl cls ->
				List.iter (fun (iface,_) -> add_descendant iface cls) cls.cl_implements;
				begin match cls.cl_super with
					| Some (csup,_) -> add_descendant csup cls
					| None -> ()
				end;
				(* Save cf_expr_unoptimized early: We want to inline with the original expression
				   on the next compilation. *)
				if not cached then begin
					let field cf = match cf.cf_expr,cf.cf_expr_unoptimized with
						| Some e,None when might_need_cf_unoptimized cls cf ->
							cf.cf_expr_unoptimized <- Some e
						| _ ->
							()
					in
					List.iter field cls.cl_ordered_fields;
					List.iter field cls.cl_ordered_statics;
					Option.may field cls.cl_constructor;
				end;
			| _ -> ()
		end;
		not cached
	) com.types in
	(* IMPORTANT:
	    There may be types in new_types which have already been post-processed, but then had their m_processed flag unset
		because they received an additional dependency. This could happen in cases such as @:generic methods in #10635.
		It is important that all filters from here up to save_class_state only process fields which do not have the
		CfPostProcessed flag set.

		This is mostly covered by run_expression_filters already, but any new additions which don't utilize that have to
		be aware of this.
	*)
	NullSafety.run com new_types;
	(* PASS 1: general expression filters *)
	let filters = [
		"ForRemap",ForRemap.apply tctx;
		"handle_abstract_casts",AbstractCast.handle_abstract_casts tctx;
	] in
	List.iter (run_expression_filters tctx detail_times filters) new_types;
	let filters = [
		"local_statics",LocalStatic.run tctx;
		"fix_return_dynamic_from_void_function",fix_return_dynamic_from_void_function true;
		"check_local_vars_init",check_local_vars_init tctx;
		"check_abstract_as_value",check_abstract_as_value;
		"Tre",if defined com Define.AnalyzerOptimize then Tre.run tctx else (fun e -> e);
		"reduce_expression",Optimizer.reduce_expression tctx;
		"inline_constructors",InlineConstructors.inline_constructors tctx;
		"Exceptions_filter",Exceptions.filter tctx;
		"captured_vars",CapturedVars.captured_vars com;
	] in
	List.iter (run_expression_filters tctx detail_times filters) new_types;
	(* PASS 1.5: pre-analyzer type filters *)
	let filters =
		match com.platform with
		| Jvm ->
			[
				DefaultArguments.run com;
			]
		| _ ->
			[]
	in
	with_timer detail_times "type 1" None (fun () ->
		List.iter (fun f -> List.iter f new_types) filters;
	);
	enter_stage com CAnalyzerStart;
	if com.platform <> Cross then Analyzer.Run.run_on_types com new_types;
	enter_stage com CAnalyzerDone;
	let locals = RenameVars.init com in
	let filters = [
		"sanitize",Optimizer.sanitize com;
		"add_final_return",if com.config.pf_add_final_return then add_final_return else (fun e -> e);
		"RenameVars",(match com.platform with
		| Eval -> (fun e -> e)
		| Jvm -> (fun e -> e)
		| _ -> (fun e -> RenameVars.run tctx.c.curclass.cl_path locals e));
		"mark_switch_break_loops",mark_switch_break_loops;
	] in
	List.iter (run_expression_filters tctx detail_times filters) new_types;
	with_timer detail_times "callbacks" None (fun () ->
		com.callbacks#run com.error_ext com.callbacks#get_before_save;
	);
	enter_stage com CSaveStart;
	with_timer detail_times "save state" None (fun () ->
		List.iter (fun mt ->
			update_cache_dependencies ~close_monomorphs:true com mt;
			save_class_state com mt
		) new_types;
	);
	enter_stage com CSaveDone;
	with_timer detail_times "callbacks" None (fun () ->
		com.callbacks#run com.error_ext com.callbacks#get_after_save;
	);
	before_destruction();
	destruction tctx detail_times main locals
