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

let get_native_name = TypeloadCheck.get_native_name

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

module LocalStatic = struct
	let promote_local_static ctx lut v eo =
		let name = Printf.sprintf "%s_%s" ctx.curfield.cf_name v.v_name in
		begin try
			let cf = PMap.find name ctx.curclass.cl_statics in
			display_error ctx (Printf.sprintf "The expanded name of this local (%s) conflicts with another static field" name) v.v_pos;
			typing_error "Conflicting field was found here" cf.cf_name_pos;
		with Not_found ->
			let cf = mk_field name ~static:true v.v_type v.v_pos v.v_pos in
			begin match eo with
			| None ->
				()
			| Some e ->
				let rec loop e = match e.eexpr with
					| TLocal _ | TFunction _ ->
						typing_error "Accessing local variables in static initialization is not allowed" e.epos
					| TConst (TThis | TSuper) ->
						typing_error "Accessing `this` in static initialization is not allowed" e.epos
					| TReturn _ | TBreak | TContinue ->
						typing_error "This kind of control flow in static initialization is not allowed" e.epos
					| _ ->
						iter loop e
				in
				loop e;
				cf.cf_expr <- Some e
			end;
			TClass.add_field ctx.curclass cf;
			Hashtbl.add lut v.v_id cf
		end

	let find_local_static lut v =
		Hashtbl.find lut v.v_id

	let run ctx e =
		let local_static_lut = Hashtbl.create 0 in
		let c = ctx.curclass in
		let rec run e = match e.eexpr with
			| TBlock el ->
				let el = ExtList.List.filter_map (fun e -> match e.eexpr with
					| TVar(v,eo) when has_var_flag v VStatic ->
						promote_local_static ctx local_static_lut v eo;
						None
					| _ ->
						Some (run e)
				) el in
				{ e with eexpr = TBlock el }
			| TLocal v when has_var_flag v VStatic ->
				begin try
					let cf = find_local_static local_static_lut v in
					Texpr.Builder.make_static_field c cf e.epos
				with Not_found ->
					typing_error (Printf.sprintf "Could not find local static %s (id %i)" v.v_name v.v_id) e.epos
				end
			| _ ->
				Type.map_expr run e
		in
		run e
end

(* -------------------------------------------------------------------------- *)
(* CHECK LOCAL VARS INIT *)

let check_local_vars_init com e =
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
					if v.v_name = "this" then com.warning WVarInit "this might be used before assigning a value to it" e.epos
					else com.warning WVarInit ("Local variable " ^ v.v_name ^ " might be used before being initialized") e.epos
				else
					if v.v_name = "this" then typing_error "Missing this = value" e.epos
					else typing_error ("Local variable " ^ v.v_name ^ " used without being initialized") e.epos
			end
		| TVar (v,eo) ->
			begin
				match eo with
				| None when v.v_kind = VInlinedConstructorVariable ->
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

let check_unification ctx e t =
	begin match e.eexpr,t with
		| TLocal v,TType({t_path = ["cs"],("Ref" | "Out")},_) ->
			(* TODO: this smells of hack, but we have to deal with it somehow *)
			add_var_flag v VCaptured;
		| _ ->
			()
	end;
	e

let rec fix_return_dynamic_from_void_function ctx return_is_void e =
	match e.eexpr with
	| TFunction fn ->
		let is_void = ExtType.is_void (follow fn.tf_type) in
		let body = fix_return_dynamic_from_void_function ctx is_void fn.tf_expr in
		{ e with eexpr = TFunction { fn with tf_expr = body } }
	| TReturn (Some return_expr) when return_is_void && t_dynamic == follow return_expr.etype ->
		let return_pos = { e.epos with pmax = return_expr.epos.pmin - 1 } in
		let exprs = [
			fix_return_dynamic_from_void_function ctx return_is_void return_expr;
			{ e with eexpr = TReturn None; epos = return_pos };
		] in
		{ e with
			eexpr = TMeta (
				(Meta.MergeBlock, [], null_pos),
				mk (TBlock exprs) e.etype e.epos
			);
		}
	| _ -> Type.map_expr (fix_return_dynamic_from_void_function ctx return_is_void) e

let check_abstract_as_value e =
	let rec loop e =
		match e.eexpr with
		| TField ({ eexpr = TTypeExpr _ }, _) -> ()
		| TTypeExpr(TClassDecl {cl_kind = KAbstractImpl a}) when not (Meta.has Meta.RuntimeValue a.a_meta) ->
			typing_error "Cannot use abstract as value" e.epos
		| _ -> Type.iter loop e
	in
	loop e;
	e

(* PASS 1 end *)

(* Saves a class state so it can be restored later, e.g. after DCE or native path rewrite *)
let save_class_state ctx t = match t with
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

		let meta = c.cl_meta and path = c.cl_path and ext = (has_class_flag c CExtern) in
		let sup = c.cl_super and impl = c.cl_implements in
		let csr = Option.map (mk_field_restore) c.cl_constructor in
		let ofr = List.map (mk_field_restore) c.cl_ordered_fields in
		let osr = List.map (mk_field_restore) c.cl_ordered_statics in
		let init = c.cl_init in
		Option.may save_vars init;
		c.cl_restore <- (fun() ->
			c.cl_super <- sup;
			c.cl_implements <- impl;
			c.cl_meta <- meta;
			if ext then add_class_flag c CExtern else remove_class_flag c CExtern;
			c.cl_path <- path;
			c.cl_init <- init;
			c.cl_ordered_fields <- List.map restore_field ofr;
			c.cl_ordered_statics <- List.map restore_field osr;
			c.cl_fields <- mk_pmap c.cl_ordered_fields;
			c.cl_statics <- mk_pmap c.cl_ordered_statics;
			c.cl_constructor <- Option.map restore_field csr;
			c.cl_descendants <- [];
			List.iter (fun (v, t) -> v.v_type <- t) !vars;
		)
	| _ ->
		()

(* PASS 2 begin *)

let remove_generic_base ctx t = match t with
	| TClassDecl c when is_removable_class c ->
		add_class_flag c CExtern;
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
		if Hashtbl.mem ctx.g.types_module rpath then typing_error ("This private class name will clash with " ^ s_type_path rpath) c.cl_pos;
	| _ ->
		()

(* Rewrites class or enum paths if @:native metadata is set *)
let apply_native_paths ctx t =
	let get_real_name meta name =
		let name',p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (name,SDoubleQuotes)), p], p), name'
	in
	let get_real_path meta path =
		let name,p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (s_type_path path,SDoubleQuotes)), p], p), parse_path name
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
			let did_change = ref false in
			let field _ ef = try
				let meta,name = get_real_name ef.ef_meta ef.ef_name in
				ef.ef_name <- name;
				ef.ef_meta <- meta :: ef.ef_meta;
				did_change := true;
			with Not_found ->
				()
			in
			PMap.iter field e.e_constrs;
			if !did_change then begin
				let names = ref [] in
				e.e_constrs <- PMap.fold
					(fun ef map ->
						names := ef.ef_name :: !names;
						PMap.add ef.ef_name ef map
					)
					e.e_constrs PMap.empty;
				e.e_names <- !names;
			end;
			let meta,path = get_real_path e.e_meta e.e_path in
			e.e_meta <- meta :: e.e_meta;
			e.e_path <- path;
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
		let f = mk_field ~static:true "__rtti" ctx.t.tstring c.cl_pos null_pos in
		let str = Genxml.gen_type_string ctx.com t in
		f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
		c.cl_ordered_statics <- f :: c.cl_ordered_statics;
		c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
	| _ ->
		()

(* Adds member field initializations as assignments to the constructor *)
let add_field_inits locals ctx t =
	let apply c =
		let ethis = mk (TConst TThis) (TInst (c,extract_param_types c.cl_params)) c.cl_pos in
		(* TODO: we have to find a variable name which is not used in any of the functions *)
		let v = alloc_var VGenerated "_g" ethis.etype ethis.epos in
		let need_this = ref false in
		let inits,fields = List.fold_left (fun (inits,fields) cf ->
			match cf.cf_kind,cf.cf_expr with
			| Var _, Some _ -> (cf :: inits, cf :: fields)
			| _ -> (inits, cf :: fields)
		) ([],[]) c.cl_ordered_fields in
		c.cl_ordered_fields <- (List.rev fields);
		match inits with
		| [] -> ()
		| _ ->
			let el = List.map (fun cf ->
				match cf.cf_expr with
				| None -> die "" __LOC__
				| Some e ->
					let lhs = mk (TField({ ethis with epos = cf.cf_pos },FInstance (c,extract_param_types c.cl_params,cf))) cf.cf_type cf.cf_pos in
					cf.cf_expr <- None;
					mk (TBinop(OpAssign,lhs,e)) cf.cf_type e.epos
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
					die "" __LOC__
			in
			let config = AnalyzerConfig.get_field_config ctx.com c cf in
			Analyzer.Run.run_on_field ctx config c cf;
			(match cf.cf_expr with
			| Some e ->
				(* This seems a bit expensive, but hopefully constructor expressions aren't that massive. *)
				let e = RenameVars.run ctx locals e in
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
		(match Texpr.build_metadata ctx.com.basic t with
		| None -> ()
		| Some e ->
			add_feature ctx.com "has_metadata";
			let cf = mk_field ~static:true "__meta__" e.etype e.epos null_pos in
			cf.cf_expr <- Some e;
			let can_deal_with_interface_metadata () = match ctx.com.platform with
				| Cs | Java -> false
				| _ -> true
			in
			if (has_class_flag c CInterface) && not (can_deal_with_interface_metadata()) then begin
				(* borrowed from gencommon, but I did wash my hands afterwards *)
				let path = fst c.cl_path,snd c.cl_path ^ "_HxMeta" in
				let ncls = mk_class c.cl_module path c.cl_pos null_pos in
				ncls.cl_ordered_statics <- cf :: ncls.cl_ordered_statics;
				ncls.cl_statics <- PMap.add cf.cf_name cf ncls.cl_statics;
				ctx.com.types <- ctx.com.types @ [ TClassDecl ncls ];
				c.cl_meta <- (Meta.Custom ":hasMetadata",[],e.epos) :: c.cl_meta
			end else begin
				c.cl_ordered_statics <- cf :: c.cl_ordered_statics;
				c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics
			end)
	| _ ->
		()

(*
	C# events have special semantics:
	if we have an @:event var field, there should also be add_<name> and remove_<name> methods,
	this filter checks for their existence and also adds some metadata for analyzer and C# generator
*)
let check_cs_events com t = match t with
	| TClassDecl cl when not (has_class_flag cl CExtern) ->
		let check fields f =
			match f.cf_kind with
			| Var { v_read = AccNormal; v_write = AccNormal } when Meta.has Meta.Event f.cf_meta ->
				if (has_class_field_flag f CfPublic) then typing_error "@:event fields must be private" f.cf_pos;

				(* prevent generating reflect helpers for the event in gencommon *)
				f.cf_meta <- (Meta.SkipReflection, [], f.cf_pos) :: f.cf_meta;

				(* type for both add and remove methods *)
				let tmeth = (tfun [f.cf_type] com.basic.tvoid) in

				let process_event_method name =
					let m = try PMap.find name fields with Not_found -> typing_error ("Missing event method: " ^ name) f.cf_pos in

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
						m.cf_meta <- (Dce.mk_keep_meta f.cf_pos) :: m.cf_meta;
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
			match follow f.cf_type with TAbstract({a_path=[],"Void"},_) -> typing_error "Fields of type Void are not allowed" f.cf_pos | _ -> ();
		in
		List.iter check c.cl_ordered_fields;
		List.iter check c.cl_ordered_statics;
	| _ ->
		()

(* Interfaces have no 'super', but can extend many other interfaces.
   This makes the first extended (implemented) interface the super for efficiency reasons (you can get one for 'free')
   and leaves the remaining ones as 'implemented' *)
let promote_first_interface_to_super ctx t = match t with
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

let commit_features ctx t =
	let m = (t_infos t).mt_module in
	Hashtbl.iter (fun k v ->
		Common.add_feature ctx.com k;
	) m.m_extra.m_features

let check_reserved_type_paths ctx t =
	let check path pos =
		if List.mem path ctx.com.config.pf_reserved_type_paths then
			ctx.com.warning WReservedTypePath ("Type path " ^ (s_type_path path) ^ " is reserved on this target") pos
	in
	match t with
	| TClassDecl c when not (has_class_flag c CExtern) -> check c.cl_path c.cl_pos
	| TEnumDecl e when not e.e_extern -> check e.e_path e.e_pos
	| _ -> ()

(* PASS 3 end *)

let pp_counter = ref 1

let is_cached t =
	let m = (t_infos t).mt_module.m_extra in
	if m.m_processed = 0 then m.m_processed <- !pp_counter;
	m.m_processed <> !pp_counter

let apply_filters_once ctx filters t =
	if not (is_cached t) then run_expression_filters None ctx filters t

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

let filter_timer detailed s =
	Timer.timer (if detailed then "filters" :: s else ["filters"])

let timer_label detailed s =
	if detailed then Some ("filters" :: s)
	else None

module ForRemap = struct
	let apply ctx e =
		let rec loop e = match e.eexpr with
		| TFor(v,e1,e2) ->
			let e1 = loop e1 in
			let e2 = loop e2 in
			let iterator = ForLoop.IterationKind.of_texpr ctx e1 (ForLoop.is_cheap_enough_t ctx e2) e.epos in
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

let run com tctx main =
	let detail_times = Common.defined com DefineList.FilterTimes in
	let new_types = List.filter (fun t ->
		let cached = is_cached t in
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
					let field cf = match cf.cf_expr with
						| Some {eexpr = TFunction tf} -> cf.cf_expr_unoptimized <- Some tf
						| _ -> ()
					in
					List.iter field cls.cl_ordered_fields;
					List.iter field cls.cl_ordered_statics;
					Option.may field cls.cl_constructor;
				end;
			| _ -> ()
		end;
		not cached
	) com.types in
	NullSafety.run com new_types;
	(* PASS 1: general expression filters *)
	let filters = [
		"ForRemap",ForRemap.apply tctx;
		"VarLazifier",VarLazifier.apply com;
		"handle_abstract_casts",AbstractCast.handle_abstract_casts tctx;
	] in
	List.iter (run_expression_filters (timer_label detail_times ["expr 0"]) tctx filters) new_types;
	let filters = [
		"local_statics",LocalStatic.run tctx;
		"fix_return_dynamic_from_void_function",fix_return_dynamic_from_void_function tctx true;
		"check_local_vars_init",check_local_vars_init tctx.com;
		"check_abstract_as_value",check_abstract_as_value;
		"Tre",if defined com Define.AnalyzerOptimize then Tre.run tctx else (fun e -> e);
		"reduce_expression",Optimizer.reduce_expression tctx;
		"inline_constructors",InlineConstructors.inline_constructors tctx;
		"Exceptions_filter",Exceptions.filter tctx;
		"captured_vars",CapturedVars.captured_vars com;
	] in
	let filters =
		match com.platform with
		| Cs ->
			SetHXGen.run_filter com new_types;
			filters
		| Java when not (Common.defined com Jvm)->
			SetHXGen.run_filter com new_types;
			filters
		| _ -> filters
	in
	List.iter (run_expression_filters (timer_label detail_times ["expr 1"]) tctx filters) new_types;
	(* PASS 1.5: pre-analyzer type filters *)
	let filters =
		match com.platform with
		| Cs ->
			[
				check_cs_events tctx.com;
				DefaultArguments.run com;
			]
		| Java ->
			[
				DefaultArguments.run com;
			]
		| _ ->
			[]
	in
	let t = filter_timer detail_times ["type 1"] in
	List.iter (fun f -> List.iter f new_types) filters;
	t();
	com.stage <- CAnalyzerStart;
	if com.platform <> Cross then Analyzer.Run.run_on_types tctx new_types;
	com.stage <- CAnalyzerDone;
	let locals = RenameVars.init com in
	let filters = [
		"sanitize",Optimizer.sanitize com;
		"add_final_return",if com.config.pf_add_final_return then add_final_return else (fun e -> e);
		"RenameVars",(match com.platform with
		| Eval -> (fun e -> e)
		| _ -> RenameVars.run tctx locals);
		"mark_switch_break_loops",mark_switch_break_loops;
	] in
	List.iter (run_expression_filters (timer_label detail_times ["expr 2"]) tctx filters) new_types;
	next_compilation();
	let t = filter_timer detail_times ["callbacks"] in
	List.iter (fun f -> f()) (List.rev com.callbacks#get_before_save); (* macros onGenerate etc. *)
	t();
	com.stage <- CSaveStart;
	let t = filter_timer detail_times ["save state"] in
	List.iter (save_class_state tctx) new_types;
	t();
	com.stage <- CSaveDone;
	let t = filter_timer detail_times ["callbacks"] in
	List.iter (fun f -> f()) (List.rev com.callbacks#get_after_save); (* macros onGenerate etc. *)
	t();
	let t = filter_timer detail_times ["type 2"] in
	(* PASS 2: type filters pre-DCE *)
	List.iter (fun t ->
		remove_generic_base tctx t;
		remove_extern_fields tctx t;
		Codegen.update_cache_dependencies t;
		(* check @:remove metadata before DCE so it is ignored there (issue #2923) *)
		check_remove_metadata tctx t;
	) com.types;
	t();
	com.stage <- CDceStart;
	let t = filter_timer detail_times ["dce"] in
	(* DCE *)
	let dce_mode = try Common.defined_value com Define.Dce with _ -> "no" in
	let dce_mode = match dce_mode with
		| "full" -> if Common.defined com Define.Interp then Dce.DceNo else DceFull
		| "std" -> DceStd
		| "no" -> DceNo
		| _ -> failwith ("Unknown DCE mode " ^ dce_mode)
	in
	Dce.run com main dce_mode;
	t();
	com.stage <- CDceDone;
	(* PASS 3: type filters post-DCE *)
	List.iter
		(run_expression_filters
			(timer_label detail_times [])
			tctx
			["insert_save_stacks",Exceptions.insert_save_stacks tctx]
		)
		new_types;
	let type_filters = [
		Exceptions.patch_constructors;
		check_private_path;
		apply_native_paths;
		add_rtti;
		(match com.platform with | Java | Cs -> (fun _ _ -> ()) | _ -> add_field_inits locals);
		(match com.platform with Hl -> (fun _ _ -> ()) | _ -> add_meta_field);
		check_void_field;
		(match com.platform with | Cpp -> promote_first_interface_to_super | _ -> (fun _ _ -> ()) );
		commit_features;
		(if com.config.pf_reserved_type_paths <> [] then check_reserved_type_paths else (fun _ _ -> ()));
	] in
	let type_filters = match com.platform with
		| Cs -> type_filters @ [ fun _ t -> InterfaceProps.run t ]
		| _ -> type_filters
	in
	let t = filter_timer detail_times ["type 3"] in
	List.iter (fun t -> List.iter (fun f -> f tctx t) type_filters) com.types;
	t();
	List.iter (fun f -> f()) (List.rev com.callbacks#get_after_filters);
	com.stage <- CFilteringDone
