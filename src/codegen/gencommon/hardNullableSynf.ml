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
open Option
open Common
open Type
open Gencommon

(* ******************************************* *)
(* HardNullableSynf *)
(* ******************************************* *)
(*
	This module will handle Null<T> types for languages that offer a way of dealing with
	stack-allocated structures or tuples and generics. Essentialy on those targets a Null<T>
	will be a tuple ( 'a * bool ), where bool is whether the value is null or not.

	At first (configure-time), we will modify the follow function so it can follow correctly nested Null<Null<T>>,
	and do not follow Null<T> to its underlying type

	Then we will run a syntax filter, which will look for casts to Null<T> and replace them by
	a call to the new Null<T> creation;
	Also casts from Null<T> to T or direct uses of Null<T> (call, field access, array access, closure)
	will result in the actual value being accessed
	For compatibility with the C# target, HardNullable will accept both Null<T> and haxe.lang.Null<T> types

	dependencies:
		Needs to be run after all cast detection modules
*)
let name = "hard_nullable"
let priority = solve_deps name [DAfter CastDetect.ReturnCast.priority]

let rec is_null_t gen t = match gen.greal_type t with
	| TAbstract( { a_path = ([], "Null") }, [of_t])
	| TInst( { cl_path = (["haxe";"lang"], "Null") }, [of_t]) ->
		let rec take_off_null t =
			match is_null_t gen t with | None -> t | Some s -> take_off_null s
		in

		Some (take_off_null of_t)
	| TMono r -> (match !r with | Some t -> is_null_t gen t | None -> None)
	| TLazy f -> is_null_t gen (lazy_type f)
	| TType (t, tl) ->
		is_null_t gen (apply_params t.t_params tl t.t_type)
	| _ -> None

let follow_addon gen t =
	let rec strip_off_nullable t =
		let t = gen.gfollow#run_f t in
		match t with
			(* haxe.lang.Null<haxe.lang.Null<>> wouldn't be a valid construct, so only follow Null<> *)
			| TAbstract ( { a_path = ([], "Null") }, [of_t] ) -> strip_off_nullable of_t
			| _ -> t
	in

	match t with
		| TAbstract( ({ a_path = ([], "Null") } as tab), [of_t]) ->
			Some( TAbstract(tab, [ strip_off_nullable of_t ]) )
		| _ -> None

let configure gen unwrap_null wrap_val null_to_dynamic has_value opeq_handler =
	gen.gfollow#add (name ^ "_follow") PZero (follow_addon gen);

	let is_null_t = is_null_t gen in
	let is_string t = match gen.greal_type t with
		| TInst({ cl_path=([],"String") },_) -> true
		| _ -> false
	in
	let handle_unwrap to_t e =
		let e_null_t = get (is_null_t e.etype) in
		match gen.greal_type to_t with
			| TDynamic _ | TMono _ | TAnon _ ->
				(match e_null_t with
					| TDynamic _ | TMono _ | TAnon _ ->
						gen.ghandle_cast to_t e_null_t (unwrap_null e)
					| _ -> null_to_dynamic e
				)
			| _ ->
				gen.ghandle_cast to_t e_null_t (unwrap_null e)
	in

	let handle_wrap e t =
		match e.eexpr with
			| TConst(TNull) ->
				wrap_val e t false
			| _ ->
				wrap_val e t true
	in

	let cur_block = ref [] in
	let add_tmp v e p =
		cur_block := { eexpr = TVar(v,e); etype = gen.gcon.basic.tvoid; epos = p } :: !cur_block
	in
	let get_local e = match e.eexpr with
		| TLocal _ ->
			e, e
		| _ ->
			let v = mk_temp "nulltmp" e.etype in
			add_tmp v (Some (null e.etype e.epos)) e.epos;
			let local = { e with eexpr = TLocal(v) } in
			mk_paren { e with eexpr = TBinop(Ast.OpAssign, local, e) }, local
	in
	let rec run e =
		match e.eexpr with
			| TBlock(bl) ->
				let lst = !cur_block in
				cur_block := [];
				List.iter (fun e ->
					let e = run e in
					cur_block := (e :: !cur_block)
				) bl;
				let ret = !cur_block in
				cur_block := lst;
				{ e with eexpr = TBlock(List.rev ret) }
			| TCast(v, _) ->
				let null_et = is_null_t e.etype in
				let null_vt = is_null_t v.etype in
				(match null_vt, null_et with
					| Some(vt), None when is_string e.etype ->
						let v = run v in
						{ e with eexpr = TCast(null_to_dynamic v,None) }
					| Some(vt), None ->
						(match v.eexpr with
							(* is there an unnecessary cast to Nullable? *)
							| TCast(v2, _) ->
								run { v with etype = e.etype }
							| _ ->
								handle_unwrap e.etype (run v)
						)
					| None, Some(et) ->
						handle_wrap (run v) et
					| Some(vt), Some(et) when not (type_iseq (run_follow gen vt) (run_follow gen et)) ->
						(* check if has value and convert *)
						let vlocal_fst, vlocal = get_local (run v) in
						{
							eexpr = TIf(
								has_value vlocal_fst,
								handle_wrap (mk_cast et (unwrap_null vlocal)) et,
								Some( handle_wrap (null et e.epos) et ));
							etype = e.etype;
							epos = e.epos
						}
					| _ ->
						Type.map_expr run e
				)
			| TField(ef, field) when is_some (is_null_t ef.etype) ->
				let to_t = get (is_null_t ef.etype) in
				{ e with eexpr = TField(handle_unwrap to_t (run ef), field) }
			| TCall(ecall, params) when is_some (is_null_t ecall.etype) ->
				let to_t = get (is_null_t ecall.etype) in
				{ e with eexpr = TCall(handle_unwrap to_t (run ecall), List.map run params) }
			| TArray(earray, p) when is_some (is_null_t earray.etype) ->
				let to_t = get (is_null_t earray.etype) in
				{ e with eexpr = TArray(handle_unwrap to_t (run earray), p) }
			| TBinop(op, e1, e2) ->
				let e1_t = is_null_t e1.etype in
				let e2_t = is_null_t e2.etype in

				(match op with
					| Ast.OpAssign
					| Ast.OpAssignOp _ ->
						(match e1_t, e2_t with
							| Some t1, Some t2 ->
								(match op with
									| Ast.OpAssign ->
										Type.map_expr run e
									| Ast.OpAssignOp op ->
										(match e1.eexpr with
											| TLocal _ ->
												{ e with eexpr = TBinop( Ast.OpAssign, e1, handle_wrap { e with eexpr = TBinop (op, handle_unwrap t1 e1, handle_unwrap t2 (run e2) ) } t1 ) }
											| _ ->
												let v, e1, evars = match e1.eexpr with
													| TField(ef, f) ->
														let v = mk_temp "nullbinop" ef.etype in
														v, { e1 with eexpr = TField(mk_local v ef.epos, f) }, ef
													| _ ->
														let v = mk_temp "nullbinop" e1.etype in
														v, mk_local v e1.epos, e1
												in
												{ e with eexpr = TBlock([
													{ eexpr = TVar(v, Some evars); etype = gen.gcon.basic.tvoid; epos = e.epos };
													{ e with eexpr = TBinop( Ast.OpAssign, e1, handle_wrap { e with eexpr = TBinop (op, handle_unwrap t1 e1, handle_unwrap t2 (run e2) ) } t1 ) }
												]) }
										)
									| _ -> assert false
								)

							| _ ->
								Type.map_expr run e (* casts are already dealt with normal CastDetection module *)
						)
					| Ast.OpEq | Ast.OpNotEq ->
						(match e1.eexpr, e2.eexpr with
							| TConst(TNull), _ when is_some e2_t ->
								let e = has_value (run e2) in
								if op = Ast.OpEq then
									{ e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) }
								else
									e
							| _, TConst(TNull) when is_some e1_t ->
								let e = has_value (run e1) in
								if op = Ast.OpEq then
									{ e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) }
								else
									e
							| _ when is_some e1_t || is_some e2_t ->
									let e1, e2 =
										if not (is_some e1_t) then
											run e2, handle_wrap (run e1) (get e2_t)
										else if not (is_some e2_t) then
											run e1, handle_wrap (run e2) (get e1_t)
										else
											run e1, run e2
									in
									let e = opeq_handler e1 e2 in
									if op = Ast.OpEq then
										{ e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) }
									else
										e
							| _ ->
								Type.map_expr run e
						)
					| Ast.OpAdd when is_string e1.etype || is_string e2.etype ->
						let e1 = if is_some e1_t then
							null_to_dynamic (run e1)
						else
							run e1
						in
						let e2 = if is_some e2_t then
							null_to_dynamic (run e2)
						else
							run e2
						in
						let e_t = is_null_t e.etype in
						if is_some e_t then
							wrap_val { eexpr = TBinop(op,e1,e2); etype = get e_t; epos = e.epos } (get e_t) true
						else
							{ e with eexpr = TBinop(op,e1,e2) }
					| _ ->
						let e1 = if is_some e1_t then
							handle_unwrap (get e1_t) (run e1)
						else run e1 in
						let e2 = if is_some e2_t then
							handle_unwrap (get e2_t) (run e2)
						else
							run e2 in

						(* if it is Null<T>, we need to convert the result again to null *)
						let e_t = (is_null_t e.etype) in
						if is_some e_t then
							wrap_val { eexpr = TBinop(op, e1, e2); etype = get e_t; epos = e.epos } (get e_t) true
						else
							{ e with eexpr = TBinop(op, e1, e2) }
				)
			(*| TUnop( (Ast.Increment as op)*)
			| _ -> Type.map_expr run e
	in
	let run e = match e.eexpr with
		| TFunction tf ->
			run { e with eexpr = TFunction { tf with tf_expr = mk_block tf.tf_expr } }
		| TBlock _ ->
			run e
		| _ -> match run (mk_block e) with
			| { eexpr = TBlock([e]) } -> e
			| e -> e
	in
	gen.gsyntax_filters#add name (PCustom priority) run
