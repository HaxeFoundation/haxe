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

open Type
open SafeCom
open Error
open Globals
open FiltersCommon

let get_native_name = Native.get_native_name

(* PASS 2 begin *)

(* Applies exclude macro (which turns types into externs) *)

let apply_macro_exclude t = match t with
	| TClassDecl c when has_class_flag c CExcluded ->
		add_class_flag c CExtern
	| TEnumDecl e when has_enum_flag e EnExcluded ->
		add_enum_flag e EnExtern
	| _ ->
		()

(* Removes extern and macro fields, also checks for Void fields *)

let remove_extern_fields scom t = match t with
	| TClassDecl c ->
		if not (Define.defined scom.defines Define.DocGen) then begin
			c.cl_ordered_fields <- List.filter (fun f ->
				let b = FilterContext.is_removable_field scom.is_macro_context f in
				if b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
				not b
			) c.cl_ordered_fields;
			c.cl_ordered_statics <- List.filter (fun f ->
				let b = FilterContext.is_removable_field scom.is_macro_context f in
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
		if com.Common.module_lut#get_type_lut#mem rpath then raise_typing_error ("This private class name will clash with " ^ s_type_path rpath) c.cl_pos;
	| _ ->
		()

(* Adds the __rtti field if required *)
let add_rtti scom t =
	let rec has_rtti c =
		Meta.has Meta.Rtti c.cl_meta || match c.cl_super with None -> false | Some (csup,_) -> has_rtti csup
	in
	match t with
	| TClassDecl c when has_rtti c && not (PMap.mem "__rtti" c.cl_statics) ->
		let f = mk_field ~static:true "__rtti" scom.basic.tstring c.cl_pos null_pos in
		let str = Genxml.gen_type_string t in
		f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
		c.cl_ordered_statics <- f :: c.cl_ordered_statics;
		c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
	| _ ->
		()

(* Adds the __meta__ field if required *)
let add_meta_field (com : Common.context) t = match t with
	| TClassDecl c ->
		(match Texpr.build_metadata com.basic t with
		| None -> ()
		| Some e ->
			Common.add_feature com "has_metadata";
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

let check_reserved_type_paths scom t =
	let check path pos =
		if List.mem path scom.platform_config.pf_reserved_type_paths then begin
			SafeCom.add_warning scom WReservedTypePath ("Type path " ^ (s_type_path path) ^ " is reserved on this target") pos
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

open FilterContext

let destruction_before_dce pool scom all_types_array =
	let filters = [
		FiltersCommon.remove_generic_base;
		(if scom.platform = Hl then (fun t -> ()) else apply_macro_exclude);
		remove_extern_fields scom;
		(* check @:remove metadata before DCE so it is ignored there (issue #2923) *)
		check_remove_metadata;
	] in
	Parallel.ParallelArray.iter pool (fun mt -> List.iter (fun f -> f mt) filters) all_types_array

let destruction_on_scom pool scom ectx rename_locals_config all_types_array =
	let filters1 = [
		SaveStacks.patch_constructors ectx;
		(fun _ -> Native.apply_native_paths);
	] in
	let filters2 = [
		(match scom.platform with | Jvm -> (fun _ _ -> ()) | _ -> (fun scom mt -> AddFieldInits.add_field_inits scom.curclass.cl_path rename_locals_config scom mt));
		(fun _ -> check_void_field);
		(fun _ -> (match scom.platform with | Cpp -> promote_first_interface_to_super | _ -> (fun _ -> ()))); (* accesses cl_super, cl_implements  *)
		(fun _ -> (if scom.platform_config.pf_reserved_type_paths <> [] then check_reserved_type_paths scom else (fun _ -> ())));
	] in
	Parallel.ParallelArray.iter pool (fun mt ->
		let scom = adapt_scom_to_mt scom mt in
		List.iter (fun f -> f scom mt) filters1
	) all_types_array;
	Parallel.ParallelArray.iter pool (fun mt ->
		let scom = adapt_scom_to_mt scom mt in
		List.iter (fun f -> f scom mt) filters2
	) all_types_array

let destruction_on_com scom com types =
	let filters = [
		(fun _ -> add_rtti scom); (* accesses cl_super *)
		(fun _ -> check_private_path com);
		(match com.platform with Hl -> (fun _ _ -> ()) | _ -> (fun _ -> add_meta_field com));
		(fun _ -> commit_features com);
	] in
	(* These aren't actually safe. The logic works fine regardless, we just can't parallelize this at the moment. *)
	SafeCom.run_type_filters_safe scom filters types

let destruction (com : Common.context) scom ectx detail_times main rename_locals_config all_types all_types_array =
	let all_types = Parallel.run_in_new_pool scom.timer_ctx (fun pool ->
		with_timer scom.timer_ctx detail_times "type 2" None (fun () ->
			SafeCom.run_with_scom com scom (fun () ->
				destruction_before_dce pool scom all_types_array
			)
		);

		Common.enter_stage com CDceStart;
		let all_types = with_timer scom.timer_ctx detail_times "dce" None (fun () ->
			(* DCE *)
			let dce_mode = try Define.defined_value scom.defines Define.Dce with _ -> "no" in
			let dce_mode = match dce_mode with
				| "full" -> if Define.defined scom.defines Define.Interp then Dce.DceNo else DceFull
				| "std" -> DceStd
				| "no" -> DceNo
				| _ -> failwith ("Unknown DCE mode " ^ dce_mode)
			in
			let std_paths = com.class_paths#get_std_paths in
			let mscom = Option.map of_com (com.get_macros()) in
			let types = Dce.run pool scom mscom main dce_mode std_paths all_types in
			types
		) in
		Common.enter_stage com CDceDone;

		(* This has to run after DCE, or otherwise its condition always holds. *)
		begin match ectx with
			| Some ectx when Common.has_feature com "haxe.NativeStackTrace.exceptionStack" ->
				Parallel.ParallelArray.iter pool (
					SafeCom.run_expression_filters_safe ~ignore_processed_status:true scom detail_times ["insert_save_stacks",SaveStacks.insert_save_stacks ectx]
				)  all_types_array
			| _ ->
				()
		end;

		with_timer scom.timer_ctx detail_times "type 3" None (fun () ->
			SafeCom.run_with_scom com scom (fun () ->
				destruction_on_scom pool scom ectx rename_locals_config all_types_array
			)
		);
		all_types
	) in
	com.types <- all_types;

	with_timer scom.timer_ctx detail_times "type 4" None (fun () ->
		SafeCom.run_with_scom com scom (fun () ->
			destruction_on_com scom com all_types
		)
	);

	com.callbacks#run com.error_ext com.callbacks#get_after_filters;
	Common.enter_stage com CFilteringDone

let update_cache_dependencies ~close_monomorphs scom t =
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
					if close_monomorphs then Monomorph.do_bind r scom.basic.tany;
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
let save_class_state compilation_step t =
	(* Update m_processed here. This means that nothing should add a dependency afterwards because
	   then the module is immediately considered uncached again *)
	(t_infos t).mt_module.m_extra.m_processed <- compilation_step;
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

let run_safe_filters ectx com (scom : SafeCom.t) all_types_array new_types_array rename_locals_config pool =
	let detail_times = Timer.level_from_define scom.defines Define.FilterTimes in
	let cv_wrapper_impl = com.Common.local_wrapper in
	let filters_before_inlining = [
		"handle_abstract_casts",AbstractCast.handle_abstract_casts;
	] in
	let filters_before_inlining_parallel = [
		"local_statics",LocalStatic.run;
		"fix_return_dynamic_from_void_function",SafeFilters.fix_return_dynamic_from_void_function;
		"check_local_vars_init",CheckVarInit.check_local_vars_init;
		"check_abstract_as_value",SafeFilters.check_abstract_as_value;
		"Tre",if Define.defined scom.defines Define.AnalyzerOptimize then Tre.run else (fun _ e -> e);
	] in

	let filters_before_analyzer = [
		"reduce_expression",Optimizer.reduce_expression;
		"inline_constructors",InlineConstructors.inline_constructors;
		"Exceptions_filter",Exceptions.filter ectx;
		"captured_vars",(fun scom -> CapturedVars.captured_vars scom cv_wrapper_impl);
	] in

	let filters_after_analyzer = [
		"sanitize",(fun scom e -> Sanitize.sanitize scom.SafeCom.platform_config e);
		"add_final_return",(fun _ -> if scom.platform_config.pf_add_final_return then AddFinalReturn.add_final_return else (fun e -> e));
		"RenameVars",(match scom.platform with
			| Eval -> (fun _ e -> e)
			| Jvm -> (fun _ e -> e)
			| _ -> (fun scom e -> RenameVars.run scom.curclass.cl_path rename_locals_config e)
		);
		"mark_switch_break_loops",SafeFilters.mark_switch_break_loops;
	] in

	begin
		let pool = if Common.defined com Define.EnableParallelAbstractCast then pool else None in
		Parallel.ParallelArray.iter pool (SafeCom.run_expression_filters_safe scom detail_times filters_before_inlining) new_types_array;
	end;
	Parallel.ParallelArray.iter pool (SafeCom.run_expression_filters_safe scom detail_times filters_before_inlining_parallel) new_types_array;
	Dump.maybe_generate_dump com AfterCasting;

	Parallel.ParallelArray.iter pool (SafeCom.run_expression_filters_safe scom detail_times filters_before_analyzer) new_types_array;
	Dump.maybe_generate_dump com AfterInlining;

	Common.enter_stage com CAnalyzerStart;
	if scom.platform <> Cross then
		let pool = if Common.defined com Define.EnableParallelAnalyzer then pool else None in
		Analyzer.Run.run_on_types scom pool all_types_array new_types_array;
	Dump.maybe_generate_dump com AfterAnalyzing;
	Common.enter_stage com CAnalyzerDone;

	Parallel.ParallelArray.iter pool (SafeCom.run_expression_filters_safe scom detail_times filters_after_analyzer) new_types_array;
	Dump.maybe_generate_dump com AfterSanitizing

let run com ectx main before_destruction =
	let scom = SafeCom.of_com com in
	let detail_times = Timer.level_from_define com.defines Define.FilterTimes in
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
	let new_types_array = Array.of_list new_types in
	let all_types_array = Array.of_list com.types in

	(* IMPORTANT:
	    There may be types in new_types which have already been post-processed, but then had their m_processed flag unset
		because they received an additional dependency. This could happen in cases such as @:generic methods in #10635.
		It is important that all filters from here up to save_class_state only process fields which do not have the
		CfPostProcessed flag set.

		This is mostly covered by run_expression_filters_safe already, but any new additions which don't utilize that have
		to be aware of this.
	*)
	DeprecationCheck.run com new_types;
	NullSafety.run com new_types;
	let rename_locals_config = RenameVars.init scom.SafeCom.platform_config com.types in
	Parallel.run_in_new_pool scom.timer_ctx (fun pool ->
		SafeCom.run_with_scom com scom (fun () ->
			run_safe_filters ectx com scom all_types_array new_types_array rename_locals_config pool
		)
	);
	with_timer com.timer_ctx detail_times "callbacks" None (fun () ->
		com.callbacks#run com.error_ext com.callbacks#get_before_save;
	);
	Common.enter_stage com CSaveStart;
	with_timer com.timer_ctx detail_times "save state" None (fun () ->
		List.iter (fun mt ->
			update_cache_dependencies ~close_monomorphs:true scom mt;
		) new_types;
	);
	(* Note: We cannot have a thread pool up during the before/after_save callbacks because Eval's thread handling
	   currently does not get along with it. This is why we need a separate pool for this operation. *)
	Parallel.run_in_new_pool scom.timer_ctx (fun pool ->
		Parallel.ParallelArray.iter pool (save_class_state com.compilation_step) new_types_array
	);
	Common.enter_stage com CSaveDone;
	with_timer com.timer_ctx detail_times "callbacks" None (fun () ->
		com.callbacks#run com.error_ext com.callbacks#get_after_save;
	);
	before_destruction();
	destruction com scom ectx detail_times main rename_locals_config com.types all_types_array