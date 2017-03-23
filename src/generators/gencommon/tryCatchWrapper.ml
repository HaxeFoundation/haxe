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
open Option
open Globals
open Common
open Ast
open Type
open Codegen
open Gencommon

(* ******************************************* *)
(* Try / Catch + throw native types handling *)
(* ******************************************* *)
(*
	Some languages/vm's do not support throwing any kind of value. For them, only
	special kinds of objects can be thrown. Because of this, we must wrap some throw
	statements with an expression, and also we must unwrap it on the catch() phase, and
	maybe manually test with Std.is()

	dependencies:
		must run before dynamic field access (?) TODO review
		It's a syntax filter, as it alters types (throw wrapper)
*)

(*
	should_wrap : does the type should be wrapped? This of course works on the reverse way, so it tells us if the type should be unwrapped as well
	wrap_throw : the wrapper for throw (throw expr->expr inside throw->returning wrapped expression)
	unwrap_expr : the other way around : given the catch var (maybe will need casting to wrapper_type) , return the unwrap expr
	rethrow_expr : how to rethrow ane exception in the platform
	catchall_type : the class used for catchall (e:Dynamic)
	wrapper_type : the wrapper type, so we can test if exception is of type 'wrapper'
	catch_map : maps the catch expression to include some intialization code (e.g. setting up Stack.exceptionStack)
	gen_typecheck : generate Std.is (or similar) check expression for given expression and type
*)
let init com (should_wrap:t->bool) (wrap_throw:texpr->texpr->texpr) (unwrap_expr:tvar->pos->texpr) (rethrow_expr:texpr->texpr) (catchall_type:t) (wrapper_type:t) (catch_map:tvar->texpr->texpr) (gen_typecheck:texpr->t->pos->texpr) =
	let rec run e =
		match e.eexpr with
		| TThrow texpr when should_wrap texpr.etype ->
			wrap_throw e (run texpr)
		| TTry (ttry, catches) ->
			let nowrap_catches, must_wrap_catches, catchall = List.fold_left (fun (nowrap_catches, must_wrap_catches, catchall) (v, catch) ->
				(* first we'll see if the type is Dynamic (catchall) *)
				match follow v.v_type with
				| TDynamic _ ->
					assert (is_none catchall);
					(nowrap_catches, must_wrap_catches, Some(v, run catch))
				(* see if we should unwrap it *)
				| _ when should_wrap (follow v.v_type) ->
					(nowrap_catches, (v,run catch) :: must_wrap_catches, catchall)
				| _ ->
					((v,catch_map v (run catch)) :: nowrap_catches, must_wrap_catches, catchall)
			) ([], [], None) catches in

			(* temp (?) fix for https://github.com/HaxeFoundation/haxe/issues/4134 *)
			let must_wrap_catches = List.rev must_wrap_catches in

			(*
				1st catch all nowrap "the easy way"
				2nd see if there are any must_wrap or catchall. If there is,
					do a catchall first with a temp var.
					then get catchall var (as dynamic) (or create one), and declare it = catchall exception
					then test if it is of type wrapper_type. If it is, unwrap it
					then start doing Std.is() tests for each catch type
					if there is a catchall in the end, end with it. If there isn't, rethrow
			*)
			let dyn_catch = match catchall, must_wrap_catches with
			| Some (v,c), _
			| _, (v, c) :: _ ->
				let pos = c.epos in
				let temp_var = mk_temp "catchallException" catchall_type in
				let temp_local = ExprBuilder.make_local temp_var pos in
				let catchall_var = mk_temp "catchall" t_dynamic in
				let catchall_decl = mk (TVar (catchall_var, Some(temp_local))) com.basic.tvoid pos in
				let catchall_local = ExprBuilder.make_local catchall_var pos in

				(* if it is of type wrapper_type, unwrap it *)
				let mk_std_is t pos = gen_typecheck catchall_local t pos in

				let if_is_wrapper_expr = mk (TIf(mk_std_is wrapper_type pos, Codegen.binop OpAssign catchall_local (unwrap_expr temp_var pos) t_dynamic pos, None)) com.basic.tvoid pos in
				let rec loop must_wrap_catches =
					match must_wrap_catches with
					| (vcatch,catch) :: tl ->
						mk (TIf (mk_std_is vcatch.v_type catch.epos,
							     mk (TBlock [(mk (TVar (vcatch, Some(mk_cast vcatch.v_type catchall_local))) com.basic.tvoid catch.epos); catch]) catch.etype catch.epos,
							     Some (loop tl))
						) catch.etype catch.epos
					| [] ->
						match catchall with
						| Some (v,s) ->
							Type.concat (mk (TVar (v, Some catchall_local)) com.basic.tvoid pos) s
						| None ->
							mk_block (rethrow_expr temp_local)
				in
				[(temp_var, catch_map temp_var { e with eexpr = TBlock [catchall_decl; if_is_wrapper_expr; loop must_wrap_catches] })]
			| _ ->
				[]
			in
			{ e with eexpr = TTry(run ttry, (List.rev nowrap_catches) @ dyn_catch) }
		| _ ->
			Type.map_expr run e
	in
	run

let name = "try_catch"
let priority = solve_deps name [DBefore DynamicFieldAccess.priority]

let configure gen should_wrap wrap_throw unwrap_expr rethrow_expr catchall_type wrapper_type catch_map =
	let gen_typecheck e t pos =
		let std_cl = get_cl (get_type gen ([],"Std")) in
		let std_is = mk_static_field_access_infer std_cl "is" pos [] in
		mk (TCall (std_is, [e; mk_mt_access (t_to_mt t) pos])) gen.gcon.basic.tbool pos
	in
	let run = init gen.gcon should_wrap wrap_throw unwrap_expr rethrow_expr catchall_type wrapper_type catch_map gen_typecheck in
	gen.gsyntax_filters#add name (PCustom priority) run
