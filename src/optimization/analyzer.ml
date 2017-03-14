(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open Type
open Common
open AnalyzerTexpr
open AnalyzerTypes
open OptimizerTexpr
open Globals

(* File organization:
	* analyzer.ml: The controlling file with all graph-based optimizations
	* analyzerConfig.ml: The general configuration which is used in multiple modules
	* analyzerTexpr.ml: Transformations and query functions on texpr, independent of graph/blocks
	* analyzerTexprTransformer.ml: Translation of texpr to graph/blocks
	* analyzerTypes.ml: Definition of graph, block and the analyzer context
*)

(*
	Ssa changes the expressions of a graph to conform to SSA rules. All variables are assigned to only once
	and SSA-phi expressions are created where necessary.

	The first pass inserts SSA-phi expressions for each variable in the dominance frontier of all its defining
	blocks.

	The second pass then creates and renames variables to ensure SSA property.
*)
module Ssa = struct
	open BasicBlock
	open Graph

	let add_phi g bb v =
		let p = bb.bb_pos in
		let ev = mk (TLocal v) v.v_type p in
		let el = List.map (fun _ -> ev) bb.bb_incoming in
		let e_phi = mk (TConst (TString "phi")) t_dynamic p in
		let ec = mk (TCall(e_phi,el)) t_dynamic p in
		let e = mk (TBinop(OpAssign,ev,ec)) t_dynamic p in
		DynArray.add bb.bb_phi e

	let insert_phi ctx =
		DynArray.iter (fun vi ->
			let v = vi.vi_var in
			if vi.vi_bb_declare == ctx.graph.g_unreachable then
				()
			else begin
				let done_list = Hashtbl.create 0 in
				let w = ref vi.vi_writes in
				while !w <> [] do
					let x = List.hd !w in
					w := List.tl !w;
					List.iter (fun y ->
						if not (Hashtbl.mem done_list y.bb_id) then begin
							Hashtbl.add done_list y.bb_id true;
							if in_scope y vi.vi_bb_declare then begin
								add_phi ctx.graph y v;
								if not (List.memq y vi.vi_writes) then
									w := y :: !w
							end
						end
					) x.bb_df;
				done
			end
		) ctx.graph.g_var_infos

	let set_reaching_def g v vo =
		let vi = get_var_info g v in
		vi.vi_reaching_def <- vo

	let get_reaching_def g v =
		(get_var_info g v).vi_reaching_def

	let rec dominates bb_dom bb =
		bb_dom == bb || bb.bb_dominator == bb_dom || (bb.bb_dominator != bb && dominates bb_dom bb.bb_dominator)

	let dominates ctx r bb =
		let l = (get_var_info ctx.graph r).vi_writes in
		List.exists (fun bb' -> dominates bb' bb) l

	let update_reaching_def ctx v bb =
		let rec loop r = match r with
			| Some r ->
				if dominates ctx r bb then
					Some r
				else
					loop (get_reaching_def ctx.graph r)
			| None ->
				None
		in
		let v' = (loop (get_reaching_def ctx.graph v)) in
		set_reaching_def ctx.graph v v'

	let local ctx e v bb =
		update_reaching_def ctx v bb;
		match get_reaching_def ctx.graph v with
			| Some v' -> v'
			| None -> v

	let update_phi ctx edge =
		let bb = edge.cfg_to in
		let rec loop i e =
			match e.eexpr with
			| TBinop(OpAssign,({eexpr = TLocal v0} as e1), ({eexpr = TCall({eexpr = TConst (TString "phi")} as ephi,el)} as ecall)) ->
				let el = List.map2 (fun e inc ->
					let bb_pred = inc.cfg_from in
					if bb_pred != edge.cfg_from then
						e
					else match e.eexpr with
					| TLocal v ->
						let v' = local ctx e v edge.cfg_from in
						add_ssa_edge ctx.graph v' bb true i;
						{e with eexpr = TLocal v'}
					| _ ->
						assert false
				) el edge.cfg_to.bb_incoming in
				let ephi = {ecall with eexpr = TCall(ephi,el)} in
				set_var_value ctx.graph v0 bb true i;
				{e with eexpr = TBinop(OpAssign,e1,ephi)}
			| _ ->
				Type.map_expr (loop i) e
		in
		dynarray_mapi loop bb.bb_phi

	let rec rename_in_block ctx bb =
		let write_var v is_phi i =
			update_reaching_def ctx v bb;
			let v' = alloc_var (v.v_name) v.v_type v.v_pos in
			declare_var ctx.graph v' bb;
			v'.v_meta <- v.v_meta;
			v'.v_capture <- v.v_capture;
			add_var_def ctx.graph bb v';
			set_reaching_def ctx.graph v' (get_reaching_def ctx.graph v);
			set_reaching_def ctx.graph v (Some v');
			set_var_value ctx.graph v' bb is_phi i;
			add_var_origin ctx.graph v' v;
			v'
		in
		let rec loop is_phi i e = match e.eexpr with
			| TLocal v when not (is_unbound v) ->
				let v' = local ctx e v bb in
				add_ssa_edge ctx.graph v' bb is_phi i;
				{e with eexpr = TLocal v'}
			| TVar(v,Some e1) when not (is_unbound v) ->
				let e1 = (loop is_phi i) e1 in
				let v' = write_var v is_phi i in
				{e with eexpr = TVar(v',Some e1)}
			| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) when not (is_unbound v) ->
				let e2 = (loop is_phi i) e2 in
				let v' = write_var v is_phi i in
				{e with eexpr = TBinop(OpAssign,{e1 with eexpr = TLocal v'},e2)};
			| TCall({eexpr = TConst (TString "phi")},_) ->
				e
			| _ ->
				Type.map_expr (loop is_phi i) e
		in
		dynarray_mapi (loop true) bb.bb_phi;
		dynarray_mapi (loop false) bb.bb_el;
		List.iter (update_phi ctx) bb.bb_outgoing;
		List.iter (rename_in_block ctx) bb.bb_dominated

	let apply ctx =
		Graph.infer_dominance_frontier ctx.graph;
		insert_phi ctx;
		rename_in_block ctx ctx.graph.g_root
end

module type DataFlowApi = sig
	type t
	val flag : BasicBlock.cfg_edge_Flag
	val transfer : analyzer_context -> BasicBlock.t -> texpr -> t (* The transfer function *)
	val equals : t -> t -> bool                                   (* The equality function *)
	val bottom : t                                                (* The bottom element of the lattice *)
	val top : t                                                   (* The top element of the lattice *)
	val get_cell : int -> t                                       (* Lattice cell getter *)
	val set_cell : int -> t -> unit                               (* Lattice cell setter *)
	val init : analyzer_context -> unit                           (* The initialization function which is called at the start *)
	val commit : analyzer_context -> unit                         (* The commit function which is called at the end *)
	val conditional : bool                                        (* Whether or not conditional branches are checked *)
end

(*
	DataFlow provides a framework for data flow analysis. It follows CFG edges from the root of the graph
	and visits the expressions and SSA-phi expressions of blocks on its way.

	If such an expression assigns to a variable (TVar or TBinop(OpAsssign)), all uses of that variable are
	checked by following the variable's SSA edges.

	A conditional branch edge (CFGCondBranch and CFGCondElse) is only followed if the available information
	suggests that it might be executable. This causes information from dead branches to not be taken into
	account.

	For SSA-phi nodes, only those incoming edges which are considered to be executable are processed.

	The algorithm continues until no further changes occur.
*)
module DataFlow (M : DataFlowApi) = struct
	open Graph
	open BasicBlock

	let get_ssa_edges_from g v =
		(get_var_info g v).vi_ssa_edges

	let run ctx =
		let g = ctx.graph in
		let ssa_work_list = ref [] in
		let cfg_work_list = ref g.g_root.bb_outgoing in
		let add_ssa_edge edge =
			ssa_work_list := edge :: !ssa_work_list
		in
		let add_cfg_edge edge =
			cfg_work_list := edge :: !cfg_work_list
		in
		let visit_phi bb v el =
			let el = List.fold_left2 (fun acc e edge ->
				if has_flag edge M.flag then e :: acc else acc
			) [] el bb.bb_incoming in
			let el = List.map (fun e -> M.transfer ctx bb e) el in
			match el with
				| e1 :: el when List.for_all (M.equals e1) el ->
					e1;
				| _ ->
					M.bottom;
		in
		let set_lattice_cell v e =
			let e' = M.get_cell v.v_id in
			M.set_cell v.v_id e;
			if not (M.equals e e') then
				List.iter (fun edge -> add_ssa_edge edge) (get_ssa_edges_from g v);
		in
		let visit_assignment bb v e =
			match e.eexpr with
			| TCall({eexpr = TConst (TString "phi")},el) ->
				set_lattice_cell v (visit_phi bb v el)
			| _ ->
				if List.exists (fun edge -> has_flag edge M.flag) bb.bb_incoming then
					set_lattice_cell v (M.transfer ctx bb e)
		in
		let visit_expression bb e =
			match e.eexpr with
			| TBinop(OpAssign,{eexpr = TLocal v},e2) | TVar(v,Some e2) ->
				visit_assignment bb v e2;
				false
			| TMeta((Meta.Custom ":cond-branch",_,_),e1) when M.conditional ->
				let e1 = M.transfer ctx bb e1 in
				let edges = if e1 == M.bottom || e1 == M.top then
					bb.bb_outgoing
				else begin
					let rec loop yes maybe also edges = match edges with
						| edge :: edges ->
							begin match edge.cfg_kind with
							| CFGCondBranch e ->
								let e = M.transfer ctx bb e in
								if M.equals e e1 then
									loop (edge :: yes) maybe also edges
								else
									loop yes maybe also edges
							| CFGCondElse ->
								loop yes (edge :: maybe) also edges
							| CFGGoto | CFGFunction | CFGMaybeThrow ->
								loop yes maybe (edge :: also) edges
							end
						| [] ->
							yes,maybe,also
					in
					let yes,maybe,also = loop [] [] [] bb.bb_outgoing in
					match yes,maybe with
						| [],[] -> bb.bb_outgoing
						| [],maybe -> maybe @ also
						| yes,_ -> yes @ also
				end in
				List.iter add_cfg_edge edges;
				true
			| _ ->
				false
		in
		let visit_expressions bb =
			let b = DynArray.fold_left (fun b e ->
				visit_expression bb e || b
			) false bb.bb_el in
			if not b then List.iter add_cfg_edge bb.bb_outgoing
		in
		let visit_phis bb =
			DynArray.iter (fun e ->
				match e.eexpr with
					| TBinop(OpAssign,{eexpr = TLocal v},{eexpr = TCall({eexpr = TConst (TString "phi")},el)}) ->
						set_lattice_cell v (visit_phi bb v el)
					| _ -> assert false
			) bb.bb_phi
		in
		let rec loop () = match !cfg_work_list,!ssa_work_list with
			| edge :: edges,_ ->
				cfg_work_list := edges;
				if not (has_flag edge M.flag) then begin
					edge.cfg_flags <- M.flag :: edge.cfg_flags;
					visit_phis edge.cfg_to;
					let i = List.fold_left (fun i edge -> i + if has_flag edge M.flag then 1 else 0) 0 edge.cfg_to.bb_incoming in
					if i = 1 || edge.cfg_to == g.g_root then
						visit_expressions edge.cfg_to;
					begin match edge.cfg_to.bb_outgoing with
						| [edge] -> add_cfg_edge edge
						| _ -> ()
					end
				end;
				loop();
			| [],((bb,is_phi,i) :: edges) ->
				ssa_work_list := edges;
				let e = get_texpr bb is_phi i in
				ignore(visit_expression bb e);
				loop()
			| [],[] ->
				()
		in
		loop ()

	let apply ctx =
		M.init ctx;
		run ctx;
		M.commit ctx
end

(*
	ConstPropagation implements sparse conditional constant propagation using the DataFlow algorithm. Its lattice consists of
	constants and enum values, but only the former are propagated. Enum values are treated as immutable data tuples and allow
	extracting constants, their index or other enum values.

	This module also deals with binop/unop optimization and standard API inlining.
*)
module ConstPropagation = DataFlow(struct
	open BasicBlock

	type t =
		| Top
		| Bottom
		| Const of tconstant
		| EnumValue of int * t list

	let conditional = true
	let flag = FlagExecutable

	let lattice = Hashtbl.create 0

	let get_cell i = try Hashtbl.find lattice i with Not_found -> Top
	let set_cell i ct = Hashtbl.replace lattice i ct

	let top = Top
	let bottom = Bottom

	let equals lat1 lat2 = match lat1,lat2 with
		| Top,Top | Bottom,Bottom -> true
		| Const ct1,Const ct2 -> ct1 = ct2
		| EnumValue(i1,_),EnumValue(i2,_) -> i1 = i2
		| _ -> false

	let transfer ctx bb e =
		let rec eval bb e =
			let wrap = function
				| Const ct -> mk (TConst ct) t_dynamic null_pos
				| _ -> raise Exit
			in
			let unwrap e = match e.eexpr with
				| TConst ct -> Const ct
				| _ -> raise Exit
			in
			match e.eexpr with
			| TConst (TSuper | TThis | TNull) ->
				Bottom
			| TConst ct ->
				Const ct
			| TLocal v ->
				if is_unbound v || (follow v.v_type) == t_dynamic || v.v_capture then
					Bottom
				else
					get_cell v.v_id
			| TBinop(OpAssign,_,e2) ->
				eval bb e2
			| TBinop(op,e1,e2) ->
				let cl1 = eval bb e1 in
				let cl2 = eval bb e2 in
				let e1 = wrap cl1 in
				let e2 = wrap cl2 in
				let e = {e with eexpr = TBinop(op,e1,e2)} in
				let e' = optimize_binop e op e1 e2 in
				if e != e' then
					eval bb e'
				else
					unwrap e'
			| TUnop(op,flag,e1) ->
				let cl1 = eval bb e1 in
				let e1 = wrap cl1 in
				let e = {e with eexpr = TUnop(op,flag,e1)} in
				let e' = optimize_unop e op flag e1 in
				if e != e' then
					eval bb e'
				else
					unwrap e'
			| TField(_,FEnum(_,ef)) ->
				EnumValue(ef.ef_index,[])
			| TCall({eexpr = TField(_,FEnum(_,ef))},el) ->
				let cll = List.map (fun e -> try eval bb e with Exit -> Bottom) el in
				EnumValue(ef.ef_index,cll)
			| TEnumParameter(e1,_,i) ->
				begin match eval bb e1 with
					| EnumValue(_,el) -> (try List.nth el i with Failure _ -> raise Exit)
					| _ -> raise Exit
				end;
			| TCall ({ eexpr = TField (_,FStatic(c,cf))},el) ->
				let el = List.map (eval bb) el in
				let el = List.map wrap el in
				begin match Optimizer.api_inline2 ctx.com c cf.cf_name el e.epos with
					| None -> raise Exit
					| Some e -> eval bb e
				end
			| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
				eval bb e1
			| _ ->
				let e1 = match ctx.com.platform,e.eexpr with
					| Js,TArray(e1,{eexpr = TConst(TInt i)}) when Int32.to_int i = 1 -> e1
					| Cpp,TCall({eexpr = TField(e1,FDynamic "__Index")},[]) -> e1
					| Neko,TField(e1,FDynamic "index") -> e1
					| _ -> raise Exit
				in
				begin match follow e1.etype,eval bb e1 with
					| TEnum _,EnumValue(i,_) -> Const (TInt (Int32.of_int i))
					| _ -> raise Exit
				end
		in
		try
			eval bb e
		with Exit ->
			Bottom

	let init ctx =
		Hashtbl.clear lattice

	let commit ctx =
		let inline e i = match get_cell i with
			| Top | Bottom | EnumValue _ ->
				raise Not_found
			| Const ct ->
				let e' = Codegen.type_constant ctx.com (tconst_to_const ct) e.epos in
				if not (type_change_ok ctx.com e'.etype e.etype) then raise Not_found;
				e'
		in
		let is_special_var v = v.v_capture || is_asvar_type v.v_type in
		let rec commit e = match e.eexpr with
			| TLocal v when not (is_special_var v) ->
				begin try
					inline e v.v_id
				with Not_found ->
					e
				end
			| TBinop((OpAssign | OpAssignOp _ as op),({eexpr = TLocal v} as e1),e2) ->
				let e2 = try
					if (has_side_effect e2) then raise Not_found;
					inline e2 v.v_id
				with Not_found ->
					commit e2
				in
				{e with eexpr = TBinop(op,e1,e2)}
			| TVar(v,Some e1) when not (has_side_effect e1) ->
				let e1 = try inline e1 v.v_id with Not_found -> commit e1 in
				{e with eexpr = TVar(v,Some e1)}
			| _ ->
				Type.map_expr commit e
		in
		Graph.iter_dom_tree ctx.graph (fun bb ->
			if not (List.exists (fun edge -> has_flag edge FlagExecutable) bb.bb_incoming) then bb.bb_dominator <- ctx.graph.Graph.g_unreachable;
			dynarray_map commit bb.bb_el
		);
end)

(*
	Propagates local variables to other local variables.

	Respects scopes on targets where it matters (all except JS and As3).
*)
module CopyPropagation = DataFlow(struct
	open BasicBlock
	open Graph

	type t =
		| Top
		| Bottom
		| Local of tvar

	let to_string = function
		| Top -> "Top"
		| Bottom -> "Bottom"
		| Local v -> Printf.sprintf "%s<%i>" v.v_name v.v_id

	let conditional = false
	let flag = FlagCopyPropagation
	let lattice = Hashtbl.create 0

	let get_cell i = try Hashtbl.find lattice i with Not_found -> Top
	let set_cell i ct = Hashtbl.replace lattice i ct

	let top = Top
	let bottom = Bottom

	let equals t1 t2 = match t1,t2 with
		| Top,Top -> true
		| Bottom,Bottom -> true
		| Local v1,Local v2 -> v1.v_id = v2.v_id
		| _ -> false

	let transfer ctx bb e =
		let rec loop e = match e.eexpr with
			| TLocal v when not v.v_capture ->
				Local v
			| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
				loop e1
			| _ ->
				Bottom
		in
		loop e

	let init ctx =
		Hashtbl.clear lattice

	let commit ctx =
		let rec commit bb e = match e.eexpr with
			| TLocal v when not v.v_capture ->
				begin try
					let lat = get_cell v.v_id in
					let leave () =
						Hashtbl.remove lattice v.v_id;
						raise Not_found
					in
					let v' = match lat with Local v -> v | _ -> leave() in
					if not (type_change_ok ctx.com v'.v_type v.v_type) then leave();
					let v'' = get_var_origin ctx.graph v' in
					(* This restriction is in place due to how we currently reconstruct the AST. Multiple SSA-vars may be turned back to
					   the same origin var, which creates interference that is not tracked in the analysis. We address this by only
					   considering variables whose origin-variables are assigned to at most once. *)
					let writes = (get_var_info ctx.graph v'').vi_writes in
					begin match writes with
						| [bb'] when in_scope bb bb' -> ()
						| _ -> leave()
					end;
					commit bb {e with eexpr = TLocal v'}
				with Not_found ->
					e
				end
			| TBinop((OpAssign | OpAssignOp _ as op),({eexpr = TLocal _} as e1),e2) ->
				let e2 = commit bb e2 in
				{e with eexpr = TBinop(op,e1,e2)}
			| _ ->
				Type.map_expr (commit bb) e
		in
		Graph.iter_dom_tree ctx.graph (fun bb ->
			dynarray_map (commit bb) bb.bb_el
		);
end)

(*
	LocalDce implements a mark & sweep dead code elimination. The mark phase follows the CFG edges of the graphs to find
	variable usages and marks variables accordingly. If ConstPropagation was run before, only CFG edges which are
	considered executable are processed.

	If a variable is marked as used, its reaching definition is recursively marked as used too. Furthermore its
	value is processed as an expression.

	The sweep phase removes all variable declarations and assignments of unused variables, keeping only the assigned
	expression in case of side-effects.
*)
module LocalDce = struct
	open BasicBlock
	open Graph
	open AnalyzerConfig

	let rec has_side_effect e =
		let rec loop e =
			match e.eexpr with
			| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ -> ()
			| TCall ({ eexpr = TField(_,FStatic({ cl_path = ([],"Std") },{ cf_name = "string" })) },args) -> Type.iter loop e
			| TCall ({eexpr = TField(_,FEnum _)},_) -> Type.iter loop e
			| TCall ({eexpr = TConst (TString ("phi" | "fun"))},_) -> ()
			| TCall({eexpr = TField(e1,fa)},el) when PurityState.is_pure_field_access fa -> loop e1; List.iter loop el
			| TNew _ | TCall _ | TBinop ((OpAssignOp _ | OpAssign),_,_) | TUnop ((Increment|Decrement),_,_) -> raise Exit
			| TReturn _ | TBreak | TContinue | TThrow _ | TCast (_,Some _) -> raise Exit
			| TFor _ -> raise Exit
			| TArray _ | TEnumParameter _ | TCast (_,None) | TBinop _ | TUnop _ | TParenthesis _ | TMeta _ | TWhile _
			| TField _ | TIf _ | TTry _ | TSwitch _ | TArrayDecl _ | TBlock _ | TObjectDecl _ | TVar _ -> Type.iter loop e
		in
		try
			loop e;
			false
		with Exit ->
			true

	let rec apply ctx =
		let is_used v =
			Meta.has Meta.Used v.v_meta
		in
		let keep v =
			is_used v || (not (Meta.has Meta.CompilerGenerated v.v_meta) && not ctx.config.local_dce) || is_ref_type v.v_type || v.v_capture || Meta.has Meta.This v.v_meta
		in
		let rec use v =
			if not (is_used v) then begin
				v.v_meta <- (Meta.Used,[],null_pos) :: v.v_meta;
				(try expr (get_var_value ctx.graph v) with Not_found -> ());
				begin match Ssa.get_reaching_def ctx.graph v with
					| None -> use (get_var_origin ctx.graph v)
					| Some v -> use v;
				end
			end
		and expr e = match e.eexpr with
			| TLocal v when not (is_unbound v) ->
				use v;
			| TBinop(OpAssign,{eexpr = TLocal v},e1) | TVar(v,Some e1) when not (is_unbound v) ->
				if has_side_effect e1 || keep v then expr e1
				else ()
			| _ ->
				Type.iter expr e
		in
		let bb_marked = ref [] in
		let rec mark bb =
			bb_marked := bb :: !bb_marked;
			DynArray.iter expr bb.bb_el;
			DynArray.iter expr bb.bb_phi;
			List.iter (fun edge ->
				if not (has_flag edge FlagDce) then begin
					edge.cfg_flags <- FlagDce :: edge.cfg_flags;
					if not ctx.config.const_propagation || has_flag edge FlagExecutable then
						mark edge.cfg_from;
				end
			) bb.bb_incoming
		in
		mark ctx.graph.g_exit;
		let rec sweep e = match e.eexpr with
			| TBinop(OpAssign,{eexpr = TLocal v},e2) | TVar(v,Some e2) when not (keep v) ->
				if has_side_effect e2 then
					e2
				else
					mk (TConst TNull) e.etype e.epos
			| TVar(v,None) when not (keep v) ->
				mk (TConst TNull) e.etype e.epos
			| _ ->
				Type.map_expr sweep e
		in
		List.iter (fun bb ->
			dynarray_map sweep bb.bb_el
		) !bb_marked;
end

module Debug = struct
	open BasicBlock
	open Graph

	type node_info =
		| NIExpr
		| NIVars
		| NIPhi
		| NILoopGroups
		| NIScopes

	let s_var v = Printf.sprintf "%s<%i>" v.v_name v.v_id

	let dot_debug_node g ch nil bb =
		let s = Printf.sprintf "(%i)" bb.bb_id in
		let s = List.fold_left (fun s ni -> s ^ match ni with
			| NIExpr -> if DynArray.length bb.bb_el = 0 then "" else "\n" ^  String.concat "\n" (DynArray.to_list (DynArray.map s_expr_pretty bb.bb_el))
			| NIPhi -> if DynArray.length bb.bb_phi = 0 then "" else "\n" ^ String.concat "\n" (DynArray.to_list (DynArray.map (fun e -> s_expr_pretty e) bb.bb_phi))
			| NIVars -> if bb.bb_var_writes = [] then "" else "\n" ^ String.concat ", " (List.map (fun v -> s_var v) bb.bb_var_writes)
			| NILoopGroups -> if bb.bb_loop_groups = [] then "" else "\nLoops: " ^ (String.concat ", " (List.map string_of_int bb.bb_loop_groups))
			| NIScopes -> if bb.bb_scopes = [] then "" else "\nScopes: " ^ (String.concat ", " (List.map string_of_int bb.bb_scopes))
		) s nil in
		let s_kind = match bb.bb_kind with
			| BKRoot -> "<root>\n"
			| BKFunctionBegin _ -> "<function-begin>\n"
			| BKFunctionEnd -> "<function-end>\n"
			| BKLoopHead -> "<loop-head>\n"
			| _ -> ""
		in
		Printf.fprintf ch "n%i [shape=box,label=\"%s%s\"];\n" bb.bb_id s_kind (s_escape s)

	let dot_debug_cfg_edge ch edge =
		let label = match edge.cfg_kind with
			| CFGGoto -> "goto"
			| CFGFunction -> "function"
			| CFGMaybeThrow -> "throw?"
			| CFGCondBranch _ -> "branch"
			| CFGCondElse -> "else"
		in
		let s_edge_flag = function
			| FlagExecutable -> "exe"
			| FlagDce -> "dce"
			| FlagCopyPropagation -> "copy"
		in
		let label = label ^ match edge.cfg_flags with
			| [] -> ""
			| _ -> Printf.sprintf " [%s]" (String.concat ", " (List.map s_edge_flag edge.cfg_flags))
		in
		Printf.fprintf ch "n%i -> n%i[label=\"%s\"];\n" edge.cfg_from.bb_id edge.cfg_to.bb_id (s_escape label)

	let dot_debug_syntax_edge ch bb se =
		let edge bb' label =
			Printf.fprintf ch "n%i -> n%i[style=\"dashed\",color=\"gray\",label=\"%s\"];\n" bb.bb_id bb'.bb_id label;
		in
		match se with
		| SESubBlock(bb_sub,bb_next) ->
			edge bb_sub "sub";
			edge bb_next "next";
		| SEIfThen(bb_then,bb_next,_) ->
			edge bb_then "then";
			edge bb_next "next"
		| SEIfThenElse(bb_then,bb_else,bb_next,_,_) ->
			edge bb_then "then";
			edge bb_else "else";
			edge bb_next "next";
		| SEWhile(bb_head,bb_body,bb_next) ->
			edge bb_head "loop-head";
			edge bb_body "loop-body";
			edge bb_next "next";
		| SEMerge bb_next ->
			edge bb_next "merge"
		| SESwitch(bbl,bo,bb_next,_) ->
			List.iter (fun (el,bb) -> edge bb ("case " ^ (String.concat " | " (List.map s_expr_pretty el)))) bbl;
			(match bo with None -> () | Some bb -> edge bb "default");
			edge bb_next "next";
		| SETry(bb_try,_,bbl,bb_next,_) ->
			edge bb_try "try";
			List.iter (fun (_,bb_catch) -> edge bb_catch "catch") bbl;
			edge bb_next "next";
		| SEEnd ->
			()
		| SENone ->
			()

	let htmlescape s =
		let s = String.concat "&amp;" (ExtString.String.nsplit s "&") in
		let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
		let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
		s

	let generate_cfg_ssa ch g =
		Printf.fprintf ch "\tnode [shape=plaintext];\n";
		let expr_name b i = Printf.sprintf "e%s%i" (if b then "p" else "") i in
		List.iter (fun bb ->
			Printf.fprintf ch "n%i[label=<<table BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n\t<tr><td port=\"in\" bgcolor=\"lightgray\">(%i) %s</td></tr>\n" bb.bb_id bb.bb_id (BasicBlock.s_block_kind bb.bb_kind);
			let s_expr b i e =
				Printf.fprintf ch "\t<tr><td port=\"%s\" align=\"left\">%s</td></tr>\n" (expr_name b i) (s_escape (htmlescape (s_expr_pretty e)))
			in
			DynArray.iteri (s_expr true) bb.bb_phi;
			DynArray.iteri (s_expr false) bb.bb_el;
			Printf.fprintf ch "\t<tr><td port=\"out\"></td></tr>\n</table>>];\n";
		) g.g_nodes;
		Graph.iter_edges g (fun edge ->
			Printf.fprintf ch "n%i:out -> n%i:in[label=\"%s\"];\n" edge.cfg_from.bb_id edge.cfg_to.bb_id (BasicBlock.s_cfg_edge_kind edge.cfg_kind)
		);
		DynArray.iter (fun vi ->
			begin try
				let (bb,is_phi,i) = match vi.vi_value with None -> raise Not_found | Some i -> i in
				let n1 = Printf.sprintf "n%i:%s" bb.bb_id (expr_name is_phi i) in
				List.iter (fun (bb',is_phi',i') ->
					if bb != bb' then begin (* intra-node edges look stupid in dot *)
						let n2 = Printf.sprintf "n%i:%s" bb'.bb_id (expr_name is_phi' i') in
						Printf.fprintf ch "%s -> %s[color=lightblue,constraint=false];\n" n1 n2;
					end
				) vi.vi_ssa_edges;
			with Not_found ->
				()
			end
		) g.g_var_infos

	let get_dump_path ctx c cf =
		"dump" :: [platform_name ctx.com.platform] @ (fst c.cl_path) @ [Printf.sprintf "%s.%s" (snd c.cl_path) cf.cf_name]

	let dot_debug ctx c cf =
		let g = ctx.graph in
		let start_graph ?(graph_config=[]) suffix =
			let ch = Codegen.Dump.create_file suffix [] (get_dump_path ctx c cf) in
			Printf.fprintf ch "digraph graphname {\n";
			List.iter (fun s -> Printf.fprintf ch "%s;\n" s) graph_config;
			ch,(fun () ->
				Printf.fprintf ch "}\n";
				close_out ch
			)
		in
		let ch,f = start_graph "-cfg.dot" in
		List.iter (fun bb -> dot_debug_node g ch [NILoopGroups;NIScopes;NIPhi;NIExpr] bb) g.g_nodes;
		Graph.iter_edges g (dot_debug_cfg_edge ch);
		f();
		let ch,f = start_graph "-cfg-ssa.dot" in
		generate_cfg_ssa ch g;
		f();
		let ch,f = start_graph "-dj.dot" in
		List.iter (fun bb ->
			dot_debug_node g ch [] bb;
			List.iter (fun einc ->
				let bb' = einc.cfg_from in
				let style = if bb' == bb.bb_dominator then "solid" else "dashed" in
				Printf.fprintf ch "n%i -> n%i[style=\"%s\"];\n" bb'.bb_id bb.bb_id style;
			) bb.bb_incoming;
		) g.g_nodes;
		f();
		let ch,f = start_graph "-df.dot" in
		List.iter (fun bb ->
			dot_debug_node g ch [NIVars] bb;
			List.iter (fun bb' -> Printf.fprintf ch "n%i -> n%i;\n" bb.bb_id bb'.bb_id) bb.bb_df;
		) g.g_nodes;
		f();
		let ch,f = start_graph "-dom.dot" in
		List.iter (fun bb ->
			dot_debug_node g ch [NIVars] bb;
			List.iter (fun bb' -> Printf.fprintf ch "n%i -> n%i;\n" bb.bb_id bb'.bb_id) bb.bb_dominated;
		) g.g_nodes;
		f();
		let ch,f = start_graph "-syntax.dot" in
		List.iter (fun bb ->
			dot_debug_node g ch [NIExpr] bb;
			dot_debug_syntax_edge ch bb bb.bb_syntax_edge
		) g.g_nodes;
		f();
		let ch,f = start_graph "-ssa-edges.dot" in
		let nodes = ref PMap.empty in
		let node_name bb is_phi i = Printf.sprintf "e%i_%b_%i" bb.bb_id is_phi i in
		let node_name2 bb is_phi i =
			let n = node_name bb is_phi i in
			nodes := PMap.add n true !nodes;
			n
		in
		DynArray.iter (fun vi ->
			begin try
				let (bb,is_phi,i) = match vi.vi_value with None -> raise Not_found | Some i -> i in
				let n1 = node_name2 bb is_phi i in
				List.iter (fun (bb',is_phi',i') ->
					let n2 = node_name2 bb' is_phi' i' in
					Printf.fprintf ch "%s -> %s;\n" n1 n2
				) vi.vi_ssa_edges
			with Not_found ->
				()
			end
		) g.g_var_infos;
		List.iter (fun bb ->
			let f is_phi acc i e =
				let n = node_name bb is_phi i in
				(i + 1),if PMap.mem n !nodes then
					(n,s_expr_pretty e) :: acc
				else
					acc
			in
			let _,active_nodes = DynArray.fold_left (fun (i,acc) -> f true acc i) (0,[]) bb.bb_phi in
			let _,active_nodes = DynArray.fold_left (fun (i,acc) -> f false acc i) (0,active_nodes) bb.bb_el in
			if active_nodes <> [] then begin
				Printf.fprintf ch "subgraph cluster_%i {\n" bb.bb_id;
				Printf.fprintf ch "label=%i;\n" bb.bb_id;
				Printf.fprintf ch "style=filled;\n";
				Printf.fprintf ch "color=lightblue;\n";
				List.iter (fun (n,s) ->
					Printf.fprintf ch "%s[shape=box,label=\"%s\"];\n" n (s_escape s)
				) active_nodes;
				Printf.fprintf ch "}\n";
			end;
		) g.g_nodes;
		f()
end

module Run = struct
	open AnalyzerConfig
	open Graph

	let with_timer detailed s f =
		let timer = timer (if detailed then "analyzer" :: s else ["analyzer"]) in
		let r = f() in
		timer();
		r

	let create_analyzer_context com config e =
		let g = Graph.create e.etype e.epos in
		let ctx = {
			com = com;
			config = config;
			graph = g;
			(* For CPP we want to use variable names which are "probably" not used by users in order to
			   avoid problems with the debugger, see https://github.com/HaxeFoundation/hxcpp/issues/365 *)
			temp_var_name = (match com.platform with Cpp -> "_hx_tmp" | _ -> "tmp");
			entry = g.g_unreachable;
			has_unbound = false;
			loop_counter = 0;
			loop_stack = [];
			debug_exprs = [];
			name_stack = [];
		} in
		ctx

	let add_debug_expr ctx s e =
		ctx.debug_exprs <- (s,e) :: ctx.debug_exprs

	let there actx e =
		if actx.com.debug then add_debug_expr actx "initial" e;
		let e = with_timer actx.config.detail_times ["->";"var-lazifier"] (fun () -> VarLazifier.apply actx.com e) in
		if actx.com.debug then add_debug_expr actx "after var-lazifier" e;
		let e = with_timer actx.config.detail_times ["->";"filter-apply"] (fun () -> TexprFilter.apply actx.com e) in
		if actx.com.debug then add_debug_expr actx "after filter-apply" e;
		let tf,t,is_real_function = match e.eexpr with
			| TFunction tf ->
				tf,e.etype,true
			| _ ->
				(* Wrap expression in a function so we don't have to treat it as a special case throughout. *)
				let e = mk (TReturn (Some e)) t_dynamic e.epos in
				let tf = { tf_args = []; tf_type = e.etype; tf_expr = e; } in
				tf,tfun [] e.etype,false
		in
		with_timer actx.config.detail_times ["->";"from-texpr"] (fun () -> AnalyzerTexprTransformer.from_tfunction actx tf t e.epos);
		is_real_function

	let back_again actx is_real_function =
		let e = with_timer actx.config.detail_times ["<-";"to-texpr"] (fun () -> AnalyzerTexprTransformer.to_texpr actx) in
		if actx.com.debug then add_debug_expr actx "after to-texpr" e;
		DynArray.iter (fun vi ->
			vi.vi_var.v_extra <- vi.vi_extra;
		) actx.graph.g_var_infos;
		let e = if actx.config.fusion then with_timer actx.config.detail_times ["<-";"fusion"] (fun () -> Fusion.apply actx.com actx.config e) else e in
		if actx.com.debug then add_debug_expr actx "after fusion" e;
		let e = with_timer actx.config.detail_times ["<-";"cleanup"] (fun () -> Cleanup.apply actx.com e) in
		if actx.com.debug then add_debug_expr actx "after cleanup" e;
		let e = if is_real_function then
			e
		else begin
			(* Get rid of the wrapping function and its return expressions. *)
			let rec loop first e = match e.eexpr with
				| TReturn (Some e) -> e
				| TFunction tf when first ->
					begin match loop false tf.tf_expr with
						| {eexpr = TBlock _ | TIf _ | TSwitch _ | TTry _} when actx.com.platform = Cpp || actx.com.platform = Hl ->
							mk (TCall(e,[])) tf.tf_type e.epos
						| e ->
							e
					end
				| TBlock [e] -> loop first e
				| TFunction _ -> e
				| _ -> Type.map_expr (loop first) e
			in
			loop true e
		end in
		e

	let run_on_expr actx e =
		let is_real_function = there actx e in
		with_timer actx.config.detail_times ["->";"idom"] (fun () -> Graph.infer_immediate_dominators actx.graph);
		with_timer actx.config.detail_times ["->";"infer_scopes"] (fun () -> Graph.infer_scopes actx.graph);
		with_timer actx.config.detail_times ["->";"var writes"] (fun () -> Graph.infer_var_writes actx.graph);
		if actx.com.debug then Graph.check_integrity actx.graph;
		if actx.config.optimize && not actx.has_unbound then begin
			with_timer actx.config.detail_times ["optimize";"ssa-apply"] (fun () -> Ssa.apply actx);
			if actx.config.const_propagation then with_timer actx.config.detail_times ["optimize";"const-propagation"] (fun () -> ConstPropagation.apply actx);
			if actx.config.copy_propagation then with_timer actx.config.detail_times ["optimize";"copy-propagation"] (fun () -> CopyPropagation.apply actx);
			with_timer actx.config.detail_times ["optimize";"local-dce"] (fun () -> LocalDce.apply actx);
		end;
		back_again actx is_real_function

	let rec reduce_control_flow ctx e =
		let e = Type.map_expr (reduce_control_flow ctx) e in
		Optimizer.reduce_control_flow ctx e

	let run_on_field ctx config c cf = match cf.cf_expr with
		| Some e when not (is_ignored cf.cf_meta) && not (Typecore.is_removable_field ctx cf) ->
			let config = update_config_from_meta ctx.Typecore.com config cf.cf_meta in
			(match e.eexpr with TFunction tf -> cf.cf_expr_unoptimized <- Some tf | _ -> ());
			let actx = create_analyzer_context ctx.Typecore.com config e in
			let debug() =
				print_endline (Printf.sprintf "While analyzing %s.%s" (s_type_path c.cl_path) cf.cf_name);
				List.iter (fun (s,e) ->
					print_endline (Printf.sprintf "<%s>" s);
					print_endline (Type.s_expr_pretty true "" false (s_type (print_context())) e);
					print_endline (Printf.sprintf "</%s>" s);
				) (List.rev actx.debug_exprs);
				Debug.dot_debug actx c cf;
				print_endline (Printf.sprintf "dot graph written to %s" (String.concat "/" (Debug.get_dump_path actx c cf)));
			in
			let e = try
				run_on_expr actx e
			with
			| Error.Error _ | Abort _ as exc ->
				raise exc
			| exc ->
				debug();
				raise exc
			in
			let e = reduce_control_flow ctx e in
			begin match config.debug_kind with
				| DebugNone -> ()
				| DebugDot -> Debug.dot_debug actx c cf;
				| DebugFull -> debug()
			end;
			cf.cf_expr <- Some e;
		| _ -> ()

	let run_on_class ctx config c =
		let config = update_config_from_meta ctx.Typecore.com config c.cl_meta in
		let process_field stat cf = match cf.cf_kind with
			| Var _ when not stat -> ()
			| _ -> run_on_field ctx config c cf
		in
		List.iter (process_field false) c.cl_ordered_fields;
		List.iter (process_field true) c.cl_ordered_statics;
		begin match c.cl_constructor with
			| None -> ()
			| Some f -> process_field false f;
		end;
		begin match c.cl_init with
			| None ->
				()
			| Some e ->
				let tf = { tf_args = []; tf_type = e.etype; tf_expr = e; } in
				let e = mk (TFunction tf) (tfun [] e.etype) e.epos in
				let actx = create_analyzer_context ctx.Typecore.com {config with optimize = false} e in
				let e = run_on_expr actx e in
				let e = match e.eexpr with
					| TFunction tf -> tf.tf_expr
					| _ -> assert false
				in
				c.cl_init <- Some e
		end

	let run_on_type ctx config t =
		match t with
		| TClassDecl c when (is_ignored c.cl_meta) -> ()
		| TClassDecl c -> run_on_class ctx config c
		| TEnumDecl _ -> ()
		| TTypeDecl _ -> ()
		| TAbstractDecl _ -> ()

	let run_on_types ctx types =
		let com = ctx.Typecore.com in
		let config = get_base_config com in
		with_timer config.detail_times ["other"] (fun () ->
			let cfl = if config.optimize && config.purity_inference then with_timer config.detail_times ["optimize";"purity-inference"] (fun () -> Purity.infer com) else [] in
			List.iter (run_on_type ctx config) types;
			List.iter (fun cf -> cf.cf_meta <- List.filter (fun (m,_,_) -> m <> Meta.Pure) cf.cf_meta) cfl
		)
end
;;
Typecore.analyzer_run_on_expr_ref := (fun com e ->
	let config = AnalyzerConfig.get_base_config com in
	(* We always want to optimize because const propagation might be required to obtain
	   a constant expression for inline field initializations (see issue #4977). *)
	let config = {config with AnalyzerConfig.optimize = true} in
	let actx = Run.create_analyzer_context com config e in
	Run.run_on_expr actx e
)