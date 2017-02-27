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

open Globals
open Ast
open Type
open Common

(*
	A BasicBlock represents a node in the control flow. It has expression elements similar to TBlock in the AST,
	but also holds additional information related to control flow and variables.

	Basic blocks are created whenever it is relevant for control flow. They differ from TBlock in that only their
	final element can be a control flow expression (the terminator). As a consequence, a given TBlock is split up
	into several basic blocks when control flow expressions are encountered.
*)
module BasicBlock = struct
	type block_kind =
		| BKRoot                    (* The unique root block of the graph *)
		| BKNormal                  (* A normal block *)
		| BKFunctionBegin of tfunc  (* Entry block of a function *)
		| BKFunctionEnd             (* Exit block of a function *)
		| BKSub                     (* A sub block *)
		| BKConditional             (* A "then", "else" or "case" block *)
		| BKLoopHead                (* Header block of a loop *)
		| BKException               (* Relay block for exceptions *)
		| BKUnreachable             (* The unique unreachable block *)
		| BKCatch of tvar           (* A catch block *)

	type cfg_edge_Flag =
		| FlagExecutable      (* Used by constant propagation to handle live edges *)
		| FlagDce             (* Used by DCE to keep track of handled edges *)
		| FlagCopyPropagation (* Used by copy propagation to track handled eges *)

	type cfg_edge_kind =
		| CFGGoto                (* An unconditional branch *)
		| CFGFunction            (* Link to a function *)
		| CFGMaybeThrow          (* The block may or may not throw an exception *)
		| CFGCondBranch of texpr (* A conditional branch *)
		| CFGCondElse            (* A conditional alternative (else,default) *)

	and cfg_edge = {
		cfg_from : t;                           (* The source block *)
		cfg_to : t;                             (* The target block *)
		cfg_kind : cfg_edge_kind;               (* The edge kind *)
		mutable cfg_flags : cfg_edge_Flag list; (* Edge flags *)
	}

	and syntax_edge =
		| SEIfThen of t * t * pos                                (* `if` with "then" and "next" *)
		| SEIfThenElse of t * t * t * Type.t * pos               (* `if` with "then", "else" and "next" *)
		| SESwitch of (texpr list * t) list * t option * t * pos (* `switch` with cases, "default" and "next" *)
		| SETry of t * t * (tvar * t) list * t *  pos            (* `try` with "exc", catches and "next" *)
		| SEWhile of t * t * t                                   (* `while` with "head", "body" and "next" *)
		| SESubBlock of t * t                                    (* "sub" with "next" *)
		| SEMerge of t                                           (* Merge to same block *)
		| SEEnd                                                  (* End of syntax *)
		| SENone                                                 (* No syntax exit *)

	and t = {
		bb_id : int;                          (* The unique ID of the block *)
		bb_type : Type.t;                     (* The block type *)
		bb_pos : pos;                         (* The block position *)
		bb_kind : block_kind;                 (* The block kind *)
		mutable bb_closed : bool;             (* Whether or not the block has been closed *)
		(* elements *)
		bb_el : texpr DynArray.t;             (* The block expressions *)
		bb_phi : texpr DynArray.t;            (* SSA-phi expressions *)
		(* relations *)
		mutable bb_outgoing : cfg_edge list;  (* Outgoing edges *)
		mutable bb_incoming : cfg_edge list;  (* Incoming edges *)
		mutable bb_dominator : t;             (* The block's dominator *)
		mutable bb_dominated : t list;        (* The dominated blocks *)
		mutable bb_df : t list;               (* The dominance frontier *)
		mutable bb_syntax_edge : syntax_edge; (* The syntactic edge *)
		mutable bb_loop_groups : int list;    (* The loop groups this block belongs to *)
		mutable bb_scopes : int list;         (* The scopes this block belongs to *)
		(* variables *)
		mutable bb_var_writes : tvar list;    (* List of assigned variables *)
	}

	let s_block_kind = function
		| BKRoot -> "BKRoot"
		| BKNormal -> "BKNormal"
		| BKFunctionBegin _ -> "BKFunctionBegin"
		| BKFunctionEnd -> "BKFunctionEnd"
		| BKSub -> "BKSub"
		| BKConditional -> "BKConditional"
		| BKLoopHead -> "BKLoopHead"
		| BKException -> "BKException"
		| BKUnreachable -> "BKUnreachable"
		| BKCatch _ -> "BKCatch"

	let s_cfg_edge_kind = function
		| CFGGoto -> "CFGGoto"
		| CFGFunction -> "CFGFunction"
		| CFGMaybeThrow -> "CFGMaybeThrow"
		| CFGCondBranch e -> "CFGCondBranch " ^ (s_expr_pretty false "" false (s_type (print_context())) e)
		| CFGCondElse -> "CFGCondElse"

	let has_flag edge flag =
		List.mem flag edge.cfg_flags

	(* expressions *)

	let add_texpr bb e =
		DynArray.add bb.bb_el e

	let get_texpr bb is_phi i =
		DynArray.get (if is_phi then bb.bb_phi else bb.bb_el) i

	(* edges *)

	let set_syntax_edge bb se =
		bb.bb_syntax_edge <- se

	let add_cfg_edge bb_from bb_to kind =
		if bb_from.bb_kind <> BKUnreachable then begin
			let edge = { cfg_from = bb_from; cfg_to = bb_to; cfg_kind = kind; cfg_flags = [] } in
			bb_from.bb_outgoing <- edge :: bb_from.bb_outgoing;
			bb_to.bb_incoming <- edge :: bb_to.bb_incoming;
		end

	let _create id kind t p =
		let rec bb = {
			bb_kind = kind;
			bb_id = id;
			bb_type = t;
			bb_pos = p;
			bb_closed = false;
			bb_el = DynArray.create();
			bb_phi = DynArray.create();
			bb_outgoing = [];
			bb_incoming = [];
			bb_dominator = bb;
			bb_dominated = [];
			bb_df = [];
			bb_syntax_edge = SENone;
			bb_loop_groups = [];
			bb_var_writes = [];
			bb_scopes = [];
		} in
		bb

	let in_scope bb bb' = match bb'.bb_scopes with
		| [] -> abort (Printf.sprintf "Scope-less block (kind: %s)" (s_block_kind bb'.bb_kind)) bb'.bb_pos
		| scope :: _ -> List.mem scope bb.bb_scopes
end

(*
	A Graph contains all relevant information for a given method. It is built from the field expression
	and then refined in subsequent modules such as Ssa.
*)
module Graph = struct
	open BasicBlock

	type texpr_lookup = BasicBlock.t * bool * int
	type tfunc_info = BasicBlock.t * Type.t * pos * tfunc
	type var_write = BasicBlock.t list
	type 'a itbl = (int,'a) Hashtbl.t

	type var_info = {
		vi_var : tvar;                            (* The variable itself *)
		vi_extra : tvar_extra;                    (* The original v_extra *)
		vi_bb_declare : BasicBlock.t;             (* The block where this variable was declared *)
		mutable vi_origin : tvar;                 (* The origin variable of this variable *)
		mutable vi_writes : var_write;            (* A list of blocks that assign to this variable *)
		mutable vi_value : texpr_lookup option;   (* The value of this variable, if known *)
		mutable vi_ssa_edges : texpr_lookup list; (* The expressions this variable influences *)
		mutable vi_reaching_def : tvar option;    (* The current reaching definition variable of this variable *)
	}

	type t = {
		mutable g_root : BasicBlock.t;             (* The unique root block *)
		mutable g_exit : BasicBlock.t;             (* The unique exit block *)
		mutable g_unreachable : BasicBlock.t;      (* The unique unreachable block *)
		mutable g_functions : tfunc_info itbl;     (* A map of functions, indexed by their block IDs *)
		mutable g_nodes : BasicBlock.t list;       (* A list of all blocks *)
		g_var_infos : var_info DynArray.t;         (* A map of variable information *)
		mutable g_loops : BasicBlock.t IntMap.t;   (* A map containing loop information *)
	}

	(* variables *)

	let create_var_info g bb v =
		let vi = {
			vi_var = v;
			vi_extra = v.v_extra;
			vi_bb_declare = bb;
			vi_origin = v;
			vi_writes = [];
			vi_value = None;
			vi_ssa_edges = [];
			vi_reaching_def = None;
		} in
		DynArray.add g.g_var_infos vi;
		let i = DynArray.length g.g_var_infos - 1 in
		v.v_extra <- Some([],Some (mk (TConst (TInt (Int32.of_int i))) t_dynamic null_pos))

	let get_var_info g v = match v.v_extra with
		| Some(_,Some {eexpr = TConst (TInt i32)}) -> DynArray.get g.g_var_infos (Int32.to_int i32)
		| _ ->
			print_endline "Unbound variable, please report this";
			print_endline (Printer.s_tvar v);
			assert false

	let declare_var g v bb =
		create_var_info g bb v

	let add_var_def g bb v =
		if bb.bb_id > 0 then begin
			bb.bb_var_writes <- v :: bb.bb_var_writes;
			let vi = get_var_info g v in
			vi.vi_writes <- bb :: vi.vi_writes;
		end

	let set_var_value g v bb is_phi i =
		(get_var_info g v).vi_value <- Some (bb,is_phi,i)

	let get_var_value g v =
		let value = (get_var_info g v).vi_value in
		let bb,is_phi,i = match value with
			| None -> raise Not_found
			| Some l -> l
		in
		match (get_texpr bb is_phi i).eexpr with
		| TVar(_,Some e) | TBinop(OpAssign,_,e) -> e
		| _ -> assert false

	let add_var_origin g v v_origin =
		(get_var_info g v).vi_origin <- v_origin

	let get_var_origin g v =
		(get_var_info g v).vi_origin

	let add_ssa_edge g v bb is_phi i =
		let vi = get_var_info g v in
		vi.vi_ssa_edges <- (bb,is_phi,i) :: vi.vi_ssa_edges

	(* nodes *)

	let add_function g tf t p bb =
		Hashtbl.add g.g_functions bb.bb_id (bb,t,p,tf)

	let alloc_id =
		let r = ref 1 in
		(fun () ->
			incr r;
			!r
		)

	let create_node g kind t p =
		let bb = BasicBlock._create (alloc_id()) kind t p in
		g.g_nodes <- bb :: g.g_nodes;
		bb

	let close_node g bb =
		if bb.bb_id > 0 then begin
			assert(not bb.bb_closed);
			bb.bb_closed <- true
		end

	let iter_dom_tree_from g bb f =
		let rec loop bb =
			f bb;
			List.iter loop bb.bb_dominated
		in
		loop bb

	let iter_dom_tree g f =
		iter_dom_tree_from g g.g_root f

	let iter_edges_from g bb f =
		iter_dom_tree_from g bb (fun bb -> List.iter f bb.bb_outgoing)

	let iter_edges g f =
		iter_dom_tree g (fun bb -> List.iter f bb.bb_outgoing)

	(* graph *)

	let create t p =
		let bb_root = BasicBlock._create 1 BKRoot t p; in
		let bb_unreachable = BasicBlock._create 0 BKUnreachable t_dynamic null_pos in
		{
			g_root = bb_root;
			g_exit = bb_unreachable;
			g_unreachable = bb_unreachable;
			g_functions = Hashtbl.create 0;
			g_nodes = [bb_root];
			g_var_infos = DynArray.create();
			g_loops = IntMap.empty;
		}

	let check_integrity g =
		List.iter (fun bb ->
			List.iter (fun edge ->
				if edge.cfg_to = g.g_unreachable then
					print_endline (Printf.sprintf "Outgoing edge from %i to the unreachable block" bb.bb_id)
				else if not (List.memq edge edge.cfg_to.bb_incoming) then
					print_endline (Printf.sprintf "Outgoing edge %i -> %i has no matching incoming edge" edge.cfg_from.bb_id edge.cfg_to.bb_id)
			) bb.bb_outgoing;
			List.iter (fun edge ->
				if edge.cfg_from == g.g_unreachable then
					print_endline (Printf.sprintf "Incoming edge to %i from the unreachable block" bb.bb_id)
				else if not (List.memq edge edge.cfg_from.bb_outgoing) then
					print_endline (Printf.sprintf "Incoming edge %i <- %i has no matching outgoing edge" edge.cfg_to.bb_id edge.cfg_from.bb_id)
			) bb.bb_incoming
		) g.g_nodes

	(* inference *)

	type dom_bb_info = {
		bb : BasicBlock.t;
		parent : dom_bb_info;
		mutable idom : dom_bb_info;
		mutable semi : int;
		mutable label : dom_bb_info;
		mutable ancestor : dom_bb_info;
		mutable bucket : dom_bb_info list;
	}

	(* Infers the immediate dominators for all reachable blocks. This function can be run multiple times
	   in case an update is necessary. *)
	let infer_immediate_dominators g =
		let info = Hashtbl.create 0 in
		let nodes = DynArray.create () in
		let get_info i = Hashtbl.find info i in
		let add_info bb bb_parent =
			let rec bbi = {
				bb = bb;
				parent = bbi;
				idom = bbi;
				semi = DynArray.length nodes;
				label = bbi;
				ancestor = bbi;
				bucket = [];
			} in
			let bbi = if bb == bb_parent then bbi else {bbi with parent = get_info bb_parent.bb_id} in
			Hashtbl.add info bb.bb_id bbi;
			DynArray.add nodes bbi;
		in
		let rec loop bb_parent bb =
			bb.bb_dominated <- [];
			add_info bb bb_parent;
			List.iter (fun edge ->
				let bb_to = edge.cfg_to in
				if not (Hashtbl.mem info bb_to.bb_id) then
					loop bb bb_to
			) bb.bb_outgoing
		in
		loop g.g_root g.g_root;
		let compress bbi =
			let rec loop l bbi =
				if bbi.ancestor == bbi then l else loop (bbi :: l) bbi.ancestor
			in
			let worklist = loop [bbi] bbi.ancestor in
			match worklist with
				| a :: worklist ->
					ignore(List.fold_left (fun (a,min_semi) bbi_desc ->
						let bbi = bbi_desc.label in
						if bbi.semi > min_semi then begin
							bbi_desc.label <- a.label;
							(bbi_desc,min_semi)
						end else
							(bbi_desc,bbi.semi)
					) (a,a.label.semi) worklist)
				| [] ->
					assert false
		in
		let eval v =
			let bbi = get_info v in
			if bbi.ancestor != bbi then begin
				compress bbi;
				bbi.label
			end else
				bbi
		in
		let rec loop nodes' = match nodes' with
			| [_] -> ()
			| [] -> assert false
			| w :: nodes' ->
				let semi = List.fold_left (fun acc v ->
					min acc (eval v.cfg_from.bb_id).semi) w.semi w.bb.bb_incoming
				in
				w.semi <- semi;
				let bbi = DynArray.get nodes semi in
				bbi.bucket <- w :: bbi.bucket;
				let bbi_p = w.parent in
				w.ancestor <- bbi_p;
				List.iter (fun v ->
					let u = eval v.bb.bb_id in
					if u.semi < v.semi then
						v.idom <- u
					else
						v.idom <- bbi_p
				) bbi_p.bucket;
				bbi_p.bucket <- [];
				loop nodes'
		in
		let l = DynArray.to_list nodes in
		loop (List.rev l);
		List.iter (fun w ->
			if w.idom != (DynArray.get nodes w.semi) then w.idom <- w.idom.idom
		) (List.tl l);
		DynArray.iter (fun bbi ->
			if bbi.idom != bbi then begin
				let bb = bbi.bb in
				let bb' = bbi.idom.bb in
				if bb != bb' then begin
					bb.bb_dominator <- bb';
					bb'.bb_dominated <- bb :: bb'.bb_dominated
				end
			end
		) nodes

	(* Infers the dominance frontier for all reachable blocks. This function should only be run once. *)
	let infer_dominance_frontier g =
		iter_edges g (fun edge ->
			let rec loop bb =
				if bb != g.g_unreachable && bb != edge.cfg_to && bb != edge.cfg_to.bb_dominator then begin
					if edge.cfg_to != g.g_exit then bb.bb_df <- edge.cfg_to :: bb.bb_df;
					if bb.bb_dominator != bb then loop bb.bb_dominator
				end
			in
			loop edge.cfg_from
		)

	(* Infers variable declarations and definitions. This function should only be run once. *)
	let infer_var_writes g =
		iter_dom_tree g (fun bb ->
			begin match bb.bb_kind with
				| BKCatch v ->
					declare_var g v bb;
					add_var_def g bb v
				| BKFunctionBegin tf ->
					List.iter (fun (v,_) ->
						declare_var g v bb;
						add_var_def g bb v
					) tf.tf_args;
				| _ ->
					()
			end;
			DynArray.iter (fun e -> match e.eexpr with
				| TVar(v,eo) when not (is_unbound v) ->
					declare_var g v bb;
					if eo <> None then add_var_def g bb v;
				| TBinop(OpAssign,{eexpr = TLocal v},_) when not (is_unbound v) ->
					add_var_def g bb v
				| _ ->
					()
			) bb.bb_el
		)

	(* Infers the scopes of all reachable blocks. This function can be run multiple times
	   in case an update is necessary *)
	let infer_scopes g =
		let next_scope_id = ref 0 in
		let next_scope scopes =
			incr next_scope_id;
			!next_scope_id :: scopes
		in
		let rec loop scopes bb =
			bb.bb_scopes <- scopes;
			begin match bb.bb_syntax_edge with
				| SEIfThen(bb_then,bb_next,_) ->
					loop (next_scope scopes) bb_then;
					loop scopes bb_next
				| SEIfThenElse(bb_then,bb_else,bb_next,_,_) ->
					loop (next_scope scopes) bb_then;
					loop (next_scope scopes) bb_else;
					loop scopes bb_next
				| SESwitch(cases,bbo,bb_next,_) ->
					List.iter (fun (_,bb_case) -> loop (next_scope scopes) bb_case) cases;
					(match bbo with None -> () | Some bb -> loop (next_scope scopes) bb);
					loop scopes bb_next;
				| SETry(bb_try,bb_exc,catches,bb_next,_) ->
					let scopes' = next_scope scopes in
					loop scopes' bb_try;
					loop scopes' bb_exc;
					List.iter (fun (_,bb_catch) -> loop (next_scope scopes) bb_catch) catches;
					loop scopes bb_next
				| SEWhile(bb_head,bb_body,bb_next) ->
					let scopes' = next_scope scopes in
					loop scopes' bb_head;
					loop scopes' bb_body;
					loop scopes bb_next;
				| SESubBlock(bb_sub,bb_next) ->
					loop (next_scope scopes) bb_sub;
					loop scopes bb_next
				| SEMerge bb ->
					loop scopes bb
				| SEEnd | SENone ->
					()
			end
		in
		Hashtbl.iter (fun _ (bb,_,_,_) -> loop [0] bb) g.g_functions
end

type analyzer_context = {
	com : Common.context;
	config : AnalyzerConfig.t;
	graph : Graph.t;
	temp_var_name : string;
	mutable entry : BasicBlock.t;
	mutable has_unbound : bool;
	mutable loop_counter : int;
	mutable loop_stack : int list;
	mutable debug_exprs : (string * texpr) list;
	mutable name_stack : string list;
}