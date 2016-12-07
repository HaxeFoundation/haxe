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
open Type
open Common
open OptimizerTexpr
open Globals

let s_expr_pretty e = s_expr_pretty false "" false (s_type (print_context())) e

let rec is_true_expr e1 = match e1.eexpr with
	| TConst(TBool true) -> true
	| TParenthesis e1 -> is_true_expr e1
	| _ -> false

let map_values ?(allow_control_flow=true) f e =
	let branching = ref false in
	let efinal = ref None in
	let f e =
		if !branching then
			f e
		else begin
			efinal := Some e;
			mk (TConst TNull) e.etype e.epos
		end
	in
	let rec loop complex e = match e.eexpr with
		| TIf(e1,e2,Some e3) ->
			branching := true;
			let e2 = loop true e2 in
			let e3 = loop true e3 in
			{e with eexpr = TIf(e1,e2,Some e3)}
		| TSwitch(e1,cases,edef) ->
			branching := true;
			let cases = List.map (fun (el,e) -> el,loop true e) cases in
			let edef = Option.map (loop true) edef in
			{e with eexpr = TSwitch(e1,cases,edef)}
		| TBlock [e1] ->
			loop complex e1
		| TBlock el ->
			begin match List.rev el with
			| e1 :: el ->
				let e1 = loop true e1 in
				let e = {e with eexpr = TBlock (List.rev (e1 :: el))} in
				{e with eexpr = TMeta((Meta.MergeBlock,[],e.epos),e)}
			| [] ->
				f e
			end
		| TTry(e1,catches) ->
			branching := true;
			let e1 = loop true e1 in
			let catches = List.map (fun (v,e) -> v,loop true e) catches in
			{e with eexpr = TTry(e1,catches)}
		| TMeta(m,e1) ->
			{e with eexpr = TMeta(m,loop complex e1)}
		| TParenthesis e1 ->
			{e with eexpr = TParenthesis (loop complex e1)}
		| TBreak | TContinue | TThrow _ | TReturn _ ->
			if not allow_control_flow then raise Exit;
			e
		| _ ->
			if not complex then raise Exit;
			f e
	in
	let e = loop false e in
	e,!efinal

let can_throw e =
	let rec loop e = match e.eexpr with
		| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ | TBlock _ -> ()
		| TCall _ | TNew _ | TThrow _ | TCast(_,Some _) -> raise Exit
		| TField _ | TArray _ -> raise Exit (* sigh *)
		| _ -> Type.iter loop e
	in
	try
		loop e; false
	with Exit ->
		true

let rec can_be_inlined e = match e.eexpr with
	| TConst _ -> true
	| TParenthesis e1 | TMeta(_,e1) -> can_be_inlined e1
	| _ -> false

let target_handles_unops com = match com.platform with
	| Lua | Python -> false
	| _ -> true

let target_handles_assign_ops com = match com.platform with
	(* Technically PHP can handle assign ops, but unfortunately x += y is not always
	   equivalent to x = x + y in case y has side-effects. *)
	| Lua | Php -> false
	| Cpp when not (Common.defined com Define.Cppia) -> false
	| _ -> true

let target_handles_side_effect_order com = match com.platform with
	| Cpp -> Common.defined com Define.Cppia
	| Php -> false
	| _ -> true

let rec can_be_used_as_value com e =
	let rec loop e = match e.eexpr with
		| TBlock [e] -> loop e
		| TBlock _ | TSwitch _ | TTry _ -> raise Exit
		| TCall({eexpr = TConst (TString "phi")},_) -> raise Exit
		(* | TCall _ | TNew _ when (match com.platform with Cpp | Php -> true | _ -> false) -> raise Exit *)
		| TReturn _ | TThrow _ | TBreak | TContinue -> raise Exit
		| TUnop((Increment | Decrement),_,_) when not (target_handles_unops com) -> raise Exit
		| TNew _ when com.platform = Php && not (Common.php7 com) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	try
		begin match com.platform,e.eexpr with
			| (Cs | Cpp | Java | Flash),TConst TNull -> raise Exit
			| _ -> ()
		end;
		loop e;
		true
	with Exit ->
		false

let wrap_meta s e =
	mk (TMeta((Meta.Custom s,[],e.epos),e)) e.etype e.epos

let is_really_unbound v = match v.v_name with
	| "`trace" | "__int__" -> false
	| _ -> is_unbound v

let r = Str.regexp "^\\([A-Za-z0-9_]\\)+$"
let is_unbound_call_that_might_have_side_effects v el = match v.v_name,el with
	| "__js__",[{eexpr = TConst (TString s)}] when Str.string_match r s 0 -> false
	| _ -> true

let is_ref_type = function
	| TType({t_path = ["cs"],("Ref" | "Out")},_) -> true
	| TType({t_path = ["cpp"],("Reference")},_) -> true
	| TAbstract({a_path=["hl";"types"],"Ref"},_) -> true
	| _ -> false

let rec is_asvar_type t =
	let check meta =
		AnalyzerConfig.has_analyzer_option meta "as_var"
	in
	match t with
	| TInst(c,_) -> check c.cl_meta
	| TEnum(en,_) -> check en.e_meta
	| TType(t,tl) -> check t.t_meta || (is_asvar_type (apply_params t.t_params tl t.t_type))
	| TAbstract(a,_) -> check a.a_meta
	| TLazy f -> is_asvar_type (!f())
	| TMono r ->
		(match !r with
		| Some t -> is_asvar_type t
		| _ -> false)
	| _ ->
		false

let type_change_ok com t1 t2 =
	if t1 == t2 then
		true
	else begin
		let rec map t = match t with
			| TMono r -> (match !r with None -> t_dynamic | Some t -> map t)
			| _ -> Type.map map t
		in
		let t1 = map t1 in
		let t2 = map t2 in
		let rec is_nullable_or_whatever = function
			| TMono r ->
				(match !r with None -> false | Some t -> is_nullable_or_whatever t)
			| TType ({ t_path = ([],"Null") },[_]) ->
				true
			| TLazy f ->
				is_nullable_or_whatever (!f())
			| TType (t,tl) ->
				is_nullable_or_whatever (apply_params t.t_params tl t.t_type)
			| TFun _ ->
				false
			| TInst ({ cl_kind = KTypeParameter _ },_) ->
				false
			| TAbstract (a,_) when Meta.has Meta.CoreType a.a_meta ->
				not (Meta.has Meta.NotNull a.a_meta)
			| TAbstract (a,tl) ->
				not (Meta.has Meta.NotNull a.a_meta) && is_nullable_or_whatever (apply_params a.a_params tl a.a_this)
			| _ ->
				true
		in
		(* Check equality again to cover cases where TMono became t_dynamic *)
		t1 == t2 || match follow t1,follow t2 with
			| TDynamic _,_ | _,TDynamic _ -> false
			| _ ->
				if com.config.pf_static && is_nullable_or_whatever t1 <> is_nullable_or_whatever t2 then false
				else type_iseq t1 t2
	end

let dynarray_map f d =
	DynArray.iteri (fun i e -> DynArray.unsafe_set d i (f e)) d

let dynarray_mapi f d =
	DynArray.iteri (fun i e -> DynArray.unsafe_set d i (f i e)) d

(*
	This module rewrites some expressions to reduce the amount of special cases for subsequent analysis. After analysis
	it restores some of these expressions back to their original form.

	The following expressions are removed from the AST after `apply` has run:
	- OpBoolAnd and OpBoolOr binary operations are rewritten to TIf
	- OpAssignOp on a variable is rewritten to OpAssign
	- Prefix increment/decrement operations are rewritten to OpAssign
	- Postfix increment/decrement operations are rewritten to a TBlock with OpAssign and OpAdd/OpSub
	- `do {} while(true)` is rewritten to `while(true) {}`
	- TWhile expressions are rewritten to `while (true)` with appropriate conditional TBreak
	- TFor is rewritten to TWhile
*)
module TexprFilter = struct
	let apply com e =
		let rec loop e = match e.eexpr with
		| TBinop(OpBoolAnd | OpBoolOr as op,e1,e2) ->
			let e_then = e2 in
			let e_if,e_else = if op = OpBoolOr then
				mk (TUnop(Not,Prefix,e1)) com.basic.tbool e.epos,mk (TConst (TBool(true))) com.basic.tbool e.epos
			else
				e1,mk (TConst (TBool(false))) com.basic.tbool e.epos
			in
			loop (mk (TIf(e_if,e_then,Some e_else)) e.etype e.epos)
		| TBinop(OpAssignOp op,({eexpr = TLocal _} as e1),e2) ->
			let e = {e with eexpr = TBinop(op,e1,e2)} in
			loop {e with eexpr = TBinop(OpAssign,e1,e)}
		| TUnop((Increment | Decrement as op),flag,({eexpr = TLocal _} as e1)) ->
			let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e1.epos in
			let e = {e with eexpr = TBinop(OpAssignOp (if op = Increment then OpAdd else OpSub),e1,e_one)} in
			let e = if flag = Prefix then
				e
			else
				mk (TBlock [
					{e with eexpr = TBinop(OpAssignOp (if op = Increment then OpAdd else OpSub),e1,e_one)};
					{e with eexpr = TBinop((if op = Increment then OpSub else OpAdd),e1,e_one)};
				]) e.etype e.epos
			in
			loop e
		| TWhile(e1,e2,DoWhile) when is_true_expr e1 ->
			loop {e with eexpr = TWhile(e1,e2,NormalWhile)}
		| TWhile(e1,e2,flag) when not (is_true_expr e1) ->
			let p = e.epos in
			let e_break = mk TBreak t_dynamic p in
			let e_not = mk (TUnop(Not,Prefix,Codegen.mk_parent e1)) e1.etype e1.epos in
			let e_if eo = mk (TIf(e_not,e_break,eo)) com.basic.tvoid p in
			let rec map_continue e = match e.eexpr with
				| TContinue ->
					Texpr.duplicate_tvars (e_if (Some e))
				| TWhile _ | TFor _ ->
					e
				| _ ->
					Type.map_expr map_continue e
			in
			let e2 = if flag = NormalWhile then e2 else map_continue e2 in
			let e_if = e_if None in
			let e_block = if flag = NormalWhile then Type.concat e_if e2 else Type.concat e2 e_if in
			let e_true = mk (TConst (TBool true)) com.basic.tbool p in
			let e = mk (TWhile(Codegen.mk_parent e_true,e_block,NormalWhile)) e.etype p in
			loop e
		| TFor(v,e1,e2) ->
			let e = Codegen.for_remap com v e1 e2 e.epos in
			loop e
		| _ ->
			Type.map_expr loop e
		in
		loop e
end

module VarLazifier = struct
	let apply com e =
		let rec loop var_inits e = match e.eexpr with
			| TVar(v,Some e1) when (Meta.has (Meta.Custom ":extractorVariable") v.v_meta) ->
				let var_inits,e1 = loop var_inits e1 in
				let var_inits = PMap.add v.v_id e1 var_inits in
				var_inits,{e with eexpr = TVar(v,None)}
			| TLocal v ->
				begin try
					let e_init = PMap.find v.v_id var_inits in
					let e = {e with eexpr = TBinop(OpAssign,e,e_init)} in
					let e = {e with eexpr = TParenthesis e} in
					let var_inits = PMap.remove v.v_id var_inits in
					var_inits,e
				with Not_found ->
					var_inits,e
				end
			| TIf(e1,e2,eo) ->
				let var_inits,e1 = loop var_inits e1 in
				let _,e2 = loop var_inits e2 in
				let eo = match eo with None -> None | Some e -> Some (snd (loop var_inits e)) in
				var_inits,{e with eexpr = TIf(e1,e2,eo)}
			| TSwitch(e1,cases,edef) ->
				let var_inits,e1 = loop var_inits e1 in
				let cases = List.map (fun (el,e) ->
					let _,e = loop var_inits e in
					el,e
				) cases in
				let edef = match edef with None -> None | Some e -> Some (snd (loop var_inits e)) in
				var_inits,{e with eexpr = TSwitch(e1,cases,edef)}
			| _ ->
				Texpr.foldmap loop var_inits e
		in
		snd (loop PMap.empty e)
end

(*
	An InterferenceReport represents in which way a given code may be influenced and
	how it might influence other code itself. It keeps track of read and write operations
	for both variable and fields, as well as a generic state read and write.
*)
module InterferenceReport = struct
	type interference_report = {
		ir_var_reads : (int,bool) Hashtbl.t;
		ir_var_writes : (int,bool) Hashtbl.t;
		ir_field_reads : (string,bool) Hashtbl.t;
		ir_field_writes : (string,bool) Hashtbl.t;
		mutable ir_state_read : bool;
		mutable ir_state_write : bool;
	}

	let create () = {
		ir_var_reads = Hashtbl.create 0;
		ir_var_writes = Hashtbl.create 0;
		ir_field_reads = Hashtbl.create 0;
		ir_field_writes = Hashtbl.create 0;
		ir_state_read = false;
		ir_state_write = false;
	}

	let set_var_read ir v = Hashtbl.replace ir.ir_var_reads v.v_id true
	let set_var_write ir v = Hashtbl.replace ir.ir_var_writes v.v_id true
	let set_field_read ir s = Hashtbl.replace ir.ir_field_reads s true
	let set_field_write ir s = Hashtbl.replace ir.ir_field_writes s true
	let set_state_read ir = ir.ir_state_read <- true
	let set_state_write ir = ir.ir_state_write <- true

	let has_var_read ir v = Hashtbl.mem ir.ir_var_reads v.v_id
	let has_var_write ir v = Hashtbl.mem ir.ir_var_writes v.v_id
	let has_field_read ir s = Hashtbl.mem ir.ir_field_reads s
	let has_field_write ir s = Hashtbl.mem ir.ir_field_writes s
	let has_state_read ir = ir.ir_state_read
	let has_state_write ir = ir.ir_state_write
	let has_any_field_read ir = Hashtbl.length ir.ir_field_reads > 0
	let has_any_field_write ir = Hashtbl.length ir.ir_field_writes > 0
	let has_any_var_read ir = Hashtbl.length ir.ir_var_reads > 0
	let has_any_var_write ir = Hashtbl.length ir.ir_var_writes > 0

	let from_texpr e =
		let ir = create () in
		let rec loop e = match e.eexpr with
			(* vars *)
			| TLocal v ->
				set_var_read ir v
			| TBinop(OpAssign,{eexpr = TLocal v},e2) ->
				set_var_write ir v;
				loop e2
			| TBinop(OpAssignOp _,{eexpr = TLocal v},e2) ->
				set_var_read ir v;
				set_var_write ir v;
				loop e2
			| TUnop((Increment | Decrement),_,{eexpr = TLocal v}) ->
				set_var_read ir v;
				set_var_write ir v;
			(* fields *)
			| TField(e1,fa) ->
				loop e1;
				if not (is_read_only_field_access e1 fa) then set_field_read ir (field_name fa);
			| TBinop(OpAssign,{eexpr = TField(e1,fa)},e2) ->
				set_field_write ir (field_name fa);
				loop e1;
				loop e2;
			| TBinop(OpAssignOp _,{eexpr = TField(e1,fa)},e2) ->
				let name = field_name fa in
				set_field_read ir name;
				set_field_write ir name;
				loop e1;
				loop e2;
			| TUnop((Increment | Decrement),_,{eexpr = TField(e1,fa)}) ->
				let name = field_name fa in
				set_field_read ir name;
				set_field_write ir name;
				loop e1
			(* array *)
			| TArray(e1,e2) ->
				set_state_read ir;
				loop e1;
				loop e2;
			| TBinop(OpAssign,{eexpr = TArray(e1,e2)},e3) ->
				set_state_write ir;
				loop e1;
				loop e2;
				loop e3;
			| TBinop(OpAssignOp _,{eexpr = TArray(e1,e2)},e3) ->
				set_state_read ir;
				set_state_write ir;
				loop e1;
				loop e2;
				loop e3;
			| TUnop((Increment | Decrement),_,{eexpr = TArray(e1,e2)}) ->
				set_state_read ir;
				set_state_write ir;
				loop e1;
				loop e2;
			(* state *)
			| TCall({eexpr = TLocal v},el) when not (is_unbound_call_that_might_have_side_effects v el) ->
				List.iter loop el
			| TNew(c,_,el) when (match c.cl_constructor with Some cf when PurityState.is_pure c cf -> true | _ -> false) ->
				set_state_read ir;
				List.iter loop el;
			| TCall({eexpr = TField(e1,FEnum _)},el) ->
				loop e1;
				List.iter loop el;
			| TCall({eexpr = TField(e1,fa)},el) when PurityState.is_pure_field_access fa ->
				set_state_read ir;
				loop e1;
				List.iter loop el
			| TCall(e1,el) ->
				set_state_read ir;
				set_state_write ir;
				loop e1;
				List.iter loop el
			| TNew(_,_,el) ->
				set_state_read ir;
				set_state_write ir;
				List.iter loop el
			| TBinop(OpAssign,e1,e2) ->
				set_state_write ir;
				loop e1;
				loop e2;
			| TBinop(OpAssignOp _,e1,e2) ->
				set_state_read ir;
				set_state_write ir;
				loop e1;
				loop e2;
			| TUnop((Increment | Decrement),_,e1) ->
				set_state_read ir;
				set_state_write ir;
				loop e1
			| _ ->
				Type.iter loop e
		in
		loop e;
		ir

	let to_string ir =
		let s_hashtbl f h =
			String.concat ", " (Hashtbl.fold (fun k _ acc -> (f k) :: acc) h [])
		in
		Type.Printer.s_record_fields "\t" [
			"ir_var_reads",s_hashtbl string_of_int ir.ir_var_reads;
			"ir_var_writes",s_hashtbl string_of_int ir.ir_var_writes;
			"ir_field_reads",s_hashtbl (fun x -> x) ir.ir_field_reads;
			"ir_field_writes",s_hashtbl (fun x -> x) ir.ir_field_writes;
			"ir_state_read",string_of_bool ir.ir_state_read;
			"ir_state_write",string_of_bool ir.ir_state_write;
		]
	end

class fusion_state = object(self)
	val mutable _changed = false
	val var_reads = Hashtbl.create 0
	val var_writes = Hashtbl.create 0

	method private change map v delta =
		Hashtbl.replace map v.v_id ((try Hashtbl.find map v.v_id with Not_found -> 0) + delta);

	method inc_reads (v : tvar) : unit = self#change var_reads v 1
	method dec_reads (v : tvar) : unit = self#change var_reads v (-1)
	method inc_writes (v : tvar) : unit = self#change var_writes v 1
	method dec_writes (v : tvar) : unit = self#change var_writes v (-1)

	method get_reads (v : tvar) = try Hashtbl.find var_reads v.v_id with Not_found -> 0
	method get_writes (v : tvar) = try Hashtbl.find var_writes v.v_id with Not_found -> 0

	method change_writes (v : tvar) delta = self#change var_writes v delta

	method changed = _changed <- true
	method reset = _changed <- false
	method did_change = _changed

	method infer_from_texpr (e : texpr) =
		let rec loop e = match e.eexpr with
			| TLocal v ->
				self#inc_reads v;
			| TBinop(OpAssign,{eexpr = TLocal v},e2) ->
				self#inc_writes v;
				loop e2
			| _ ->
				Type.iter loop e
		in
		loop e
end

(*
	Fusion tries to join expressions together in order to make the output "look nicer". To that end,
	several transformations occur:

	- `var x; x = e;` is transformed to `var x = e;`
	- `var x; if(e1) x = e2 else x = e3` is transformed to `var x = e1 ? e2 : e3` on targets that
	  deal well with that.
	- `var x = e;` is transformed to `e` if `x` is unused.
	- Some block-level increment/decrement unary operators are put back into value places and the
	  transformation of their postfix variant is reversed.
	- `x = x op y` is transformed (back) to `x op= y` on targets that deal well with that.

	Most importantly, any `var v = e;` might be fused into expressions that follow it in the same
	block if there is no interference.
*)
module Fusion = struct
	open AnalyzerConfig
	open InterferenceReport

	let is_assign_op = function
		| OpAdd
		| OpMult
		| OpDiv
		| OpSub
		| OpAnd
		| OpOr
		| OpXor
		| OpShl
		| OpShr
		| OpUShr
		| OpMod ->
			true
		| OpAssign
		| OpEq
		| OpNotEq
		| OpGt
		| OpGte
		| OpLt
		| OpLte
		| OpBoolAnd
		| OpBoolOr
		| OpAssignOp _
		| OpInterval
		| OpArrow ->
			false

	let use_assign_op com op e1 e2 =
		is_assign_op op && target_handles_assign_ops com && Texpr.equal e1 e2 && not (has_side_effect e1) && match com.platform with
			| Cs when is_null e1.etype || is_null e2.etype -> false (* C# hates OpAssignOp on Null<T> *)
			| _ -> true

	let apply com config e =
		let state = new fusion_state in
		state#infer_from_texpr e;
		(* Handles block-level expressions, e.g. by removing side-effect-free ones and recursing into compound constructs like
		   array or object declarations. The resulting element list is reversed. *)
		let rec block_element acc el = match el with
			| {eexpr = TBinop((OpAssign | OpAssignOp _),_,_) | TUnop((Increment | Decrement),_,_)} as e1 :: el ->
				block_element (e1 :: acc) el
			| {eexpr = TLocal _} as e1 :: el when not config.local_dce ->
				block_element (e1 :: acc) el
			| {eexpr = TLocal v} :: el ->
				state#dec_reads v;
				block_element acc el
			(* no-side-effect *)
			| {eexpr = TEnumParameter _ | TFunction _ | TConst _ | TTypeExpr _} :: el ->
				block_element acc el
			| {eexpr = TMeta((Meta.Pure,_,_),_)} :: el ->
				block_element acc el
			| {eexpr = TCall({eexpr = TField(e1,fa)},el1)} :: el2 when PurityState.is_pure_field_access fa && config.local_dce ->
				block_element acc (e1 :: el1 @ el2)
			| {eexpr = TNew(c,tl,el1)} :: el2 when (match c.cl_constructor with Some cf when PurityState.is_pure c cf -> true | _ -> false) && config.local_dce ->
				block_element acc (el1 @ el2)
			(* no-side-effect composites *)
			| {eexpr = TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) | TField(e1,_) | TUnop(_,_,e1)} :: el ->
				block_element acc (e1 :: el)
			| {eexpr = TArray(e1,e2) | TBinop(_,e1,e2)} :: el ->
				block_element acc (e1 :: e2 :: el)
			| {eexpr = TArrayDecl el1 | TCall({eexpr = TField(_,FEnum _)},el1)} :: el2 -> (* TODO: check e1 of FEnum *)
				block_element acc (el1 @ el2)
			| {eexpr = TObjectDecl fl} :: el ->
				block_element acc ((List.map snd fl) @ el)
			| {eexpr = TIf(e1,{eexpr = TBlock []},(Some {eexpr = TBlock []} | None))} :: el ->
				block_element acc (e1 :: el)
			| {eexpr = TBlock [e1]} :: el ->
				block_element acc (e1 :: el)
			| {eexpr = TBlock []} :: el ->
				block_element acc el
			| e1 :: el ->
				block_element (e1 :: acc) el
			| [] ->
				acc
		in
		let can_be_fused v e =
			let num_uses = state#get_reads v in
			let num_writes = state#get_writes v in
			let can_be_used_as_value = can_be_used_as_value com e in
			let is_compiler_generated = Meta.has Meta.CompilerGenerated v.v_meta in
			let has_type_params = match v.v_extra with Some (tl,_) when tl <> [] -> true | _ -> false in
			let b = num_uses <= 1 &&
			        num_writes = 0 &&
			        can_be_used_as_value &&
					not (is_asvar_type v.v_type) &&
			        (is_compiler_generated || config.optimize && config.fusion && config.user_var_fusion && not has_type_params)
			in
			if config.fusion_debug then begin
				print_endline (Printf.sprintf "FUSION\n\tvar %s<%i> = %s" v.v_name v.v_id (s_expr_pretty e));
				print_endline (Printf.sprintf "\tcan_be_fused:%b: num_uses:%i <= 1 && num_writes:%i = 0 && can_be_used_as_value:%b && (is_compiler_generated:%b || config.optimize:%b && config.fusion:%b && config.user_var_fusion:%b)"
					b num_uses num_writes can_be_used_as_value is_compiler_generated config.optimize config.fusion config.user_var_fusion)
			end;
			b
		in
		let rec fuse acc el = match el with
			| ({eexpr = TVar(v1,None)} as e1) :: {eexpr = TBinop(OpAssign,{eexpr = TLocal v2},e2)} :: el when v1 == v2 ->
				state#changed;
				let e1 = {e1 with eexpr = TVar(v1,Some e2)} in
				state#dec_writes v1;
				fuse (e1 :: acc) el
			| ({eexpr = TVar(v1,None)} as e1) :: ({eexpr = TIf(eif,_,Some _)} as e2) :: el when can_be_used_as_value com e2 && not (ExtType.is_void e2.etype) && (match com.platform with Php -> false | Cpp when not (Common.defined com Define.Cppia) -> false | _ -> true) ->
				begin try
					let i = ref 0 in
					let check_assign e = match e.eexpr with
						| TBinop(OpAssign,{eexpr = TLocal v2},e2) when v1 == v2 -> incr i; e2
						| _ -> raise Exit
					in
					let e,_ = map_values ~allow_control_flow:false check_assign e2 in
					let e1 = {e1 with eexpr = TVar(v1,Some e)} in
					state#changed;
					state#change_writes v1 (- !i);
					fuse (e1 :: acc) el
				with Exit ->
					fuse (e1 :: acc) (e2 :: el)
				end
			| {eexpr = TVar(v1,Some e1)} :: el when config.optimize && config.local_dce && state#get_reads v1 = 0 && state#get_writes v1 = 0 ->
				fuse acc (e1 :: el)
			| ({eexpr = TVar(v1,Some e1)} as ev) :: el when can_be_fused v1 e1 ->
				let found = ref false in
				let blocked = ref false in
				let ir = InterferenceReport.from_texpr e1 in
				if config.fusion_debug then print_endline (Printf.sprintf "\tInterferenceReport: %s\n\t%s"
					 (InterferenceReport.to_string ir) (Type.s_expr_pretty true "\t" false (s_type (print_context())) (mk (TBlock el) t_dynamic null_pos)));
				(* This function walks the AST in order of evaluation and tries to find an occurrence of v1. If successful, that occurrence is
				   replaced with e1. If there's an interference "on the way" the replacement is canceled. *)
				let rec replace e =
					let explore e =
						let old = !blocked in
						blocked := true;
						let e = replace e in
						blocked := old;
						e
					in
					let handle_el el =
						(* This mess deals with the fact that the order of evaluation is undefined for call
							arguments on these targets. Even if we find a replacement, we pretend that we
							didn't in order to find possible interferences in later call arguments. *)
						let temp_found = false in
						let really_found = ref !found in
						let el = List.map (fun e ->
							found := temp_found;
							let e = replace e in
							if !found then really_found := true;
							e
						) el in
						found := !really_found;
						el
					in
					let handle_el = if not (target_handles_side_effect_order com) then handle_el else List.map replace in
					let handle_call e2 el = match com.platform with
						| Neko ->
							(* Neko has this reversed at the moment (issue #4787) *)
							let el = List.map replace el in
							let e2 = replace e2 in
							e2,el
						| Php | Cpp  when not (Common.defined com Define.Cppia) && not (Common.php7 com) ->
							let is_php_safe e1 =
								let rec loop e = match e.eexpr with
									| TCall _ -> raise Exit
									| TCast(e1,_) | TParenthesis e1 | TMeta(_,e1) -> loop e1
									| _ -> ()
								in
								try loop e1; true with Exit -> false
							in
							(* PHP doesn't like call()() expressions. *)
							let e2 = if com.platform = Php && not (is_php_safe e1) then explore e2 else replace e2 in
							let el = handle_el el in
							e2,el
						| _ ->
							let e2 = replace e2 in
							let el = List.map replace el in
							e2,el
					in
					if !found then e else match e.eexpr with
						| TWhile _ | TTry _ ->
							raise Exit
						| TFunction _ ->
							e
						| TIf(e1,e2,eo) ->
							let e1 = replace e1 in
							if not !found && (has_state_write ir || has_any_field_write ir || has_any_var_write ir) then raise Exit;
							let e2 = replace e2 in
							let eo = Option.map replace eo in
							{e with eexpr = TIf(e1,e2,eo)}
						| TSwitch(e1,cases,edef) ->
							let e1 = match com.platform with
								| Lua | Python -> explore e1
								| _ -> replace e1
							in
							if not !found then raise Exit;
							{e with eexpr = TSwitch(e1,cases,edef)}
						(* locals *)
						| TLocal v2 when v1 == v2 && not !blocked ->
							found := true;
							if type_change_ok com v1.v_type e1.etype then e1 else mk (TCast(e1,None)) v1.v_type e.epos
						| TLocal v ->
							if has_var_write ir v || ((v.v_capture || is_ref_type v.v_type) && (has_state_write ir)) then raise Exit;
							e
						| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
							let e2 = replace e2 in
							if not !found && has_var_read ir v then raise Exit;
							{e with eexpr = TBinop(OpAssign,e1,e2)}
						| TBinop(OpAssignOp _ as op,({eexpr = TLocal v} as e1),e2) ->
							let e2 = replace e2 in
							if not !found && (has_var_read ir v || has_var_write ir v) then raise Exit;
							{e with eexpr = TBinop(op,e1,e2)}
						| TUnop((Increment | Decrement),_,{eexpr = TLocal v}) when has_var_read ir v || has_var_write ir v ->
							raise Exit
						(* fields *)
						| TField(e1,fa) ->
							let e1 = replace e1 in
							if not !found && not (is_read_only_field_access e1 fa) && (has_field_write ir (field_name fa) || has_state_write ir) then raise Exit;
							{e with eexpr = TField(e1,fa)}
						| TBinop(OpAssign,({eexpr = TField(e1,fa)} as ef),e2) ->
							let e1 = replace e1 in
							let e2 = replace e2 in
							if not !found && (has_field_read ir (field_name fa) || has_state_read ir) then raise Exit;
							{e with eexpr = TBinop(OpAssign,{ef with eexpr = TField(e1,fa)},e2)}
						| TBinop(OpAssignOp _ as op,({eexpr = TField(e1,fa)} as ef),e2) ->
							let e1 = replace e1 in
							let s = field_name fa in
							if not !found && (has_field_write ir s || has_state_write ir) then raise Exit;
							let e2 = replace e2 in
							if not !found && (has_field_read ir s || has_state_read ir) then raise Exit;
							{e with eexpr = TBinop(op,{ef with eexpr = TField(e1,fa)},e2)}
						| TUnop((Increment | Decrement),_,{eexpr = TField(e1,fa)}) when has_field_read ir (field_name fa) || has_state_read ir
							|| has_field_write ir (field_name fa) || has_state_write ir ->
							raise Exit
						(* state *)
						| TCall({eexpr = TLocal v},el) when not (is_unbound_call_that_might_have_side_effects v el) ->
							e
						| TNew(c,tl,el) when (match c.cl_constructor with Some cf when PurityState.is_pure c cf -> true | _ -> false) ->
							let el = handle_el el in
							if not !found && (has_state_write ir || has_any_field_write ir) then raise Exit;
							{e with eexpr = TNew(c,tl,el)}
						| TNew(c,tl,el) ->
							let el = handle_el el in
							if not !found && (has_state_write ir || has_state_read ir || has_any_field_read ir || has_any_field_write ir) then raise Exit;
							{e with eexpr = TNew(c,tl,el)}
						| TCall({eexpr = TField(_,FEnum _)} as ef,el) ->
							let el = List.map replace el in
							{e with eexpr = TCall(ef,el)}
						| TCall({eexpr = TField(_,fa)} as ef,el) when PurityState.is_pure_field_access fa ->
							let ef,el = handle_call ef el in
							if not !found && (has_state_write ir || has_any_field_write ir) then raise Exit;
							{e with eexpr = TCall(ef,el)}
						| TCall(e1,el) ->
							let e1,el = match e1.eexpr with
								| TLocal v when is_really_unbound v -> e1,el
								| _ -> handle_call e1 el
							in
							if not !found && (((has_state_read ir || has_any_field_read ir)) || has_state_write ir || has_any_field_write ir) then raise Exit;
							{e with eexpr = TCall(e1,el)}
						| TObjectDecl fl ->
							let el = handle_el (List.map snd fl) in
							if not !found && (has_state_write ir || has_any_field_write ir) then raise Exit;
							{e with eexpr = TObjectDecl (List.map2 (fun (s,_) e -> s,e) fl el)}
						| TArrayDecl el ->
							let el = handle_el el in
							(*if not !found && (has_state_write ir || has_any_field_write ir) then raise Exit;*)
							{e with eexpr = TArrayDecl el}
						| TBinop(OpAssign,({eexpr = TArray(e1,e2)} as ea),e3) ->
							let e1 = replace e1 in
							let e2 = replace e2 in
							let e3 = replace e3 in
							if not !found && has_state_read ir then raise Exit;
							{e with eexpr = TBinop(OpAssign,{ea with eexpr = TArray(e1,e2)},e3)}
						| TBinop(op,e1,e2) when (match com.platform with Cpp | Php when not (Common.php7 com) -> true | _ -> false) ->
							let e1 = replace e1 in
							let temp_found = !found in
							found := false;
							let e2 = replace e2 in
							found := !found || temp_found;
							{e with eexpr = TBinop(op,e1,e2)}
						| TArray(e1,e2) ->
							let e1 = replace e1 in
							let e2 = replace e2 in
							if not !found && has_state_write ir then raise Exit;
							{e with eexpr = TArray(e1,e2)}
						| _ ->
							Type.map_expr replace e
				in
				begin try
					let rec loop acc el = match el with
						| e :: el ->
							let e = replace e in
							if !found then (List.rev (e :: acc)) @ el
							else loop (e :: acc) el
						| [] ->
							List.rev acc
					in
					let el = loop [] el in
					if not !found then raise Exit;
					state#changed;
					state#dec_reads v1;
					if config.fusion_debug then print_endline (Printf.sprintf "YES: %s" (s_expr_pretty (mk (TBlock el) t_dynamic null_pos)));
					fuse acc el
				with Exit ->
					if config.fusion_debug then print_endline (Printf.sprintf "NO: %s" (Printexc.get_backtrace()));
					fuse (ev :: acc) el
				end
			| {eexpr = TUnop((Increment | Decrement as op,Prefix,({eexpr = TLocal v} as ev)))} as e1 :: e2 :: el ->
				begin try
					let e2,f = match e2.eexpr with
						| TReturn (Some e2) -> e2,(fun e -> {e2 with eexpr = TReturn (Some e)})
						| TBinop(OpAssign,e21,e22) -> e22,(fun e -> {e2 with eexpr = TBinop(OpAssign,e21,e)})
						| TVar(v,Some e2) -> e2,(fun e -> {e2 with eexpr = TVar(v,Some e)})
						| _ -> raise Exit
					in
					let ops_match op1 op2 = match op1,op2 with
						| Increment,OpSub
						| Decrement,OpAdd ->
							true
						| _ ->
							false
					in
					begin match e2.eexpr with
						| TBinop(op2,{eexpr = TLocal v2},{eexpr = TConst (TInt i32)}) when v == v2 && Int32.to_int i32 = 1 && ops_match op op2 ->
							state#changed;
							state#dec_reads v2;
							let e = (f {e1 with eexpr = TUnop(op,Postfix,ev)}) in
							fuse (e :: acc) el
						| TLocal v2 when v == v2 ->
							state#changed;
							state#dec_reads v2;
							let e = (f {e1 with eexpr = TUnop(op,Prefix,ev)}) in
							fuse (e :: acc) el
						| _ ->
							raise Exit
					end
				with Exit ->
					fuse (e1 :: acc) (e2 :: el)
				end
			| {eexpr = TBinop(OpAssign,e1,{eexpr = TBinop(op,e2,e3)})} as e :: el when use_assign_op com op e1 e2 ->
				let rec loop e = match e.eexpr with
					| TLocal v -> state#dec_reads v;
					| _ -> Type.iter loop e
				in
				loop e1;
				state#changed;
				fuse acc ({e with eexpr = TBinop(OpAssignOp op,e1,e3)} :: el)
			| {eexpr = TBinop(OpAssignOp _,e1,_)} as eop :: ({eexpr = TVar(v,Some e2)} as evar) :: el when Texpr.equal e1 e2 ->
				state#changed;
				fuse ({evar with eexpr = TVar(v,Some eop)} :: acc) el
			| e1 :: el ->
				fuse (e1 :: acc) el
			| [] ->
				acc
		in
		let rec loop e = match e.eexpr with
			| TBlock el ->
				let el = List.rev_map loop el in
				let el = block_element [] el in
				(* fuse flips element order, but block_element doesn't care and flips it back *)
				let el = fuse [] el in
				let el = block_element [] el in
				let rec fuse_loop el =
					state#reset;
					let el = fuse [] el in
					let el = block_element [] el in
					if state#did_change then fuse_loop el else el
				in
				let el = fuse_loop el in
				{e with eexpr = TBlock el}
			| TCall({eexpr = TLocal v},_) when is_really_unbound v ->
				e
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module Cleanup = struct
	let apply com e =
		let if_or_op e e1 e2 e3 = match (Texpr.skip e1).eexpr,(Texpr.skip e3).eexpr with
			| TUnop(Not,Prefix,e1),TConst (TBool true) -> optimize_binop {e with eexpr = TBinop(OpBoolOr,e1,e2)} OpBoolOr e1 e2
			| _,TConst (TBool false) -> optimize_binop {e with eexpr = TBinop(OpBoolAnd,e1,e2)} OpBoolAnd e1 e2
			| _,TBlock [] -> {e with eexpr = TIf(e1,e2,None)}
			| _ -> match (Texpr.skip e2).eexpr with
				| TBlock [] when com.platform <> Cs ->
					let e1' = mk (TUnop(Not,Prefix,e1)) e1.etype e1.epos in
					let e1' = optimize_unop e1' Not Prefix e1 in
					{e with eexpr = TIf(e1',e3,None)}
				| _ ->
					{e with eexpr = TIf(e1,e2,Some e3)}
		in
		let rec loop e = match e.eexpr with
			| TIf(e1,e2,Some e3) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				let e3 = loop e3 in
				if_or_op e e1 e2 e3;
			| TUnop((Increment | Decrement),_,e1) when (match (Texpr.skip e1).eexpr with TConst _ -> true | _ -> false) ->
				loop e1
			| TCall({eexpr = TLocal v},_) when is_really_unbound v ->
				e
			| TBlock el ->
				let el = List.map (fun e ->
					let e = loop e in
					match e.eexpr with
					| TIf _ -> {e with etype = com.basic.tvoid}
					| _ -> e
				) el in
				{e with eexpr = TBlock el}
			| TWhile(e1,e2,NormalWhile) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				begin match e2.eexpr with
					| TBlock ({eexpr = TIf(e1,({eexpr = TBlock[{eexpr = TBreak}]} as eb),None)} :: el2) ->
						let e1 = Texpr.skip e1 in
						let e1 = match e1.eexpr with TUnop(_,_,e1) -> e1 | _ -> {e1 with eexpr = TUnop(Not,Prefix,e1)} in
						{e with eexpr = TWhile(e1,{eb with eexpr = TBlock el2},NormalWhile)}
					| TBlock el ->
						let rec loop2 el = match el with
							| {eexpr = TBreak | TContinue | TReturn _ | TThrow _} as e :: el ->
								[e]
							| e :: el ->
								e :: (loop2 el)
							| [] ->
								[]
						in
						let el = loop2 el in
						{e with eexpr = TWhile(e1,{e2 with eexpr = TBlock el},NormalWhile)}
					| _ ->
						{e with eexpr = TWhile(e1,e2,NormalWhile)}
				end
			| TField({eexpr = TTypeExpr _},_) ->
				e
			| TTypeExpr (TClassDecl c) ->
				List.iter (fun cf -> if not (Meta.has Meta.MaybeUsed cf.cf_meta) then cf.cf_meta <- (Meta.MaybeUsed,[],cf.cf_pos) :: cf.cf_meta;) c.cl_ordered_statics;
				e
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module Purity = struct
	open PurityState

	type purity_node = {
		pn_class : tclass;
		pn_field : tclass_field;
		mutable pn_purity : PurityState.t;
		mutable pn_dependents : purity_node list;
	}

	exception Purity_conflict of purity_node * pos

	let node_lut = Hashtbl.create 0

	let get_field_id c cf = Printf.sprintf "%s.%s" (s_type_path c.cl_path) cf.cf_name

	let get_node c cf =
		try
			Hashtbl.find node_lut (get_field_id c cf)
		with Not_found ->
			let node = {
				pn_class = c;
				pn_field = cf;
				pn_purity = PurityState.get_purity c cf;
				pn_dependents = []
			} in
			Hashtbl.replace node_lut (get_field_id c cf) node;
			node

	let rec taint node = match node.pn_purity with
		| Impure -> ()
		| ExpectPure p -> raise (Purity_conflict(node,p));
		| MaybePure | Pure ->
			node.pn_purity <- Impure;
			List.iter taint node.pn_dependents;
			let rec loop c = match c.cl_super with
				| None -> ()
				| Some(c,_) ->
					begin try
						let cf = PMap.find node.pn_field.cf_name c.cl_fields in
						taint (get_node c cf);
					with Not_found ->
						()
					end;
					loop c
			in
			loop node.pn_class

	let taint_raise node =
		taint node;
		raise Exit

	let apply_to_field com is_ctor c cf =
		let node = get_node c cf in
		let check_field c cf =
			let node' = get_node c cf in
			match node'.pn_purity with
				| Pure | ExpectPure _ -> ()
				| Impure -> taint_raise node;
				| MaybePure -> node'.pn_dependents <- node :: node'.pn_dependents
		in
		let rec check_write e1 =
			begin match e1.eexpr with
				| TLocal v ->
					if is_ref_type v.v_type then taint_raise node; (* Writing to a ref type means impurity. *)
					() (* Writing to locals does not violate purity. *)
				| TField({eexpr = TConst TThis},_) when is_ctor ->
					() (* A constructor can write to its own fields without violating purity. *)
				| _ ->
					taint_raise node
			end
		and loop e = match e.eexpr with
			| TMeta((Meta.Pure,_,_),_) ->
				()
			| TThrow _ ->
				taint_raise node;
			| TBinop((OpAssign | OpAssignOp _),e1,e2) ->
				check_write e1;
				loop e2;
			| TUnop((Increment | Decrement),_,e1) ->
				check_write e1;
			| TCall({eexpr = TField(_,FStatic(c,cf))},el) ->
				List.iter loop el;
				check_field c cf;
			| TNew(c,_,el) ->
				List.iter loop el;
				begin match c.cl_constructor with
					| Some cf -> check_field c cf
					| None -> taint_raise node
				end
			| TCall({eexpr = TConst TSuper},el) ->
				begin match c.cl_super with
					| Some({cl_constructor = Some cf} as c,_) ->
						check_field c cf;
						List.iter loop el
					| _ ->
						taint_raise node (* Can that even happen? *)
				end
			| TCall({eexpr = TLocal v},el) when not (is_unbound_call_that_might_have_side_effects v el) ->
				List.iter loop el;
			| TCall _ ->
				taint_raise node
			| _ ->
				Type.iter loop e
		in
		match cf.cf_kind with
			| Method MethDynamic | Var _ ->
				taint node;
			| _ ->
				match cf.cf_expr with
				| None ->
					if not (is_pure c cf) then taint node
				(* TODO: The function code check shouldn't be here I guess. *)
				| Some _ when (Meta.has Meta.Extern cf.cf_meta || Meta.has Meta.FunctionCode cf.cf_meta || Meta.has (Meta.Custom ":hlNative") cf.cf_meta || Meta.has (Meta.Custom ":hlNative") c.cl_meta) ->
					if not (is_pure c cf) then taint node
				| Some e ->
					try
						begin match node.pn_purity with
							| Impure -> taint_raise node
							| Pure -> raise Exit
							| _ -> loop e
						end
					with Exit ->
						()

	let apply_to_class com c =
		List.iter (apply_to_field com false c) c.cl_ordered_fields;
		List.iter (apply_to_field com false c) c.cl_ordered_statics;
		(match c.cl_constructor with Some cf -> apply_to_field com true c cf | None -> ())

	let infer com =
		Hashtbl.clear node_lut;
		List.iter (fun mt -> match mt with
			| TClassDecl c ->
				begin try
					apply_to_class com c
				with Purity_conflict(impure,p) ->
					com.error "Impure field overrides/implements field which was explicitly marked as @:pure" impure.pn_field.cf_pos;
					Error.error "Pure field is here" p;
				end
			| _ -> ()
		) com.types;
		Hashtbl.fold (fun _ node acc ->
			match node.pn_purity with
			| Pure | MaybePure ->
				node.pn_field.cf_meta <- (Meta.Pure,[EConst(Ident "true"),node.pn_field.cf_pos],node.pn_field.cf_pos) :: node.pn_field.cf_meta;
				node.pn_field :: acc
			| _ ->
				acc
		) node_lut [];
end
