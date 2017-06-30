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
open Common
open Ast
open Type
open Codegen
open Codegen.ExprBuilder

(* ******************************************* *)
(* Try / Catch + throw native types handling *)
(* ******************************************* *)
(*
	Some languages/vm's do not support throwing any kind of value. For them, only
	special kinds of objects can be thrown. Because of this, we must wrap some throw
	statements with an expression, and also we must unwrap it on the catch() phase, and
	maybe manually test with Std.is()
*)

(*
	should_wrap : does the type should be wrapped? This of course works on the reverse way, so it tells us if the type should be unwrapped as well
	wrap_throw : the wrapper for throw (throw expr->returning wrapped expression)
	unwrap_expr : the other way around : given the catch var (maybe will need casting to wrapper_type) , return the unwrap expr
	rethrow_expr : how to rethrow ane exception in the platform
	catchall_type : the class used for catchall (e:Dynamic)
	wrapper_type : the wrapper type, so we can test if exception is of type 'wrapper'
	catch_map : maps the catch expression to include some intialization code (e.g. setting up Stack.exceptionStack)
	gen_typecheck : generate Std.is (or similar) check expression for given expression and type
*)
let init com (should_wrap:t->bool) (wrap_throw:texpr->texpr) (unwrap_expr:texpr->texpr) (rethrow_expr:texpr->texpr) (catchall_type:t) (wrapper_type:t) (catch_map:tvar->texpr->texpr) (gen_typecheck:texpr->t->pos->texpr) =
	let rec run e =
		match e.eexpr with
		| TThrow texpr when should_wrap texpr.etype ->
			wrap_throw (run texpr)
		| TTry (ttry, catches) ->
			let nowrap_catches, must_wrap_catches, catchall = List.fold_left (fun (nowrap_catches, must_wrap_catches, catchall) (v, catch) ->
				(* first we'll see if the type is Dynamic (catchall) *)
				match follow v.v_type with
				| TDynamic _ ->
					assert (Option.is_none catchall);
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

				let temp_var = alloc_var "catchallException" catchall_type pos in
				let temp_local = make_local temp_var pos in
				let catchall_var = alloc_var "realException" t_dynamic pos in
				let catchall_local = make_local catchall_var pos in

				(* if it is of type wrapper_type, unwrap it *)
				let catchall_expr = mk (TIf (gen_typecheck temp_local wrapper_type pos, unwrap_expr temp_local, Some temp_local)) t_dynamic pos in
				let catchall_decl = mk (TVar (catchall_var, Some catchall_expr)) com.basic.tvoid pos in

				let rec loop must_wrap_catches =
					match must_wrap_catches with
					| (vcatch,catch) :: tl ->
						mk (TIf (gen_typecheck catchall_local vcatch.v_type catch.epos,
							     mk (TBlock [(mk (TVar (vcatch, Some(mk_cast (* TODO: this should be a fast non-dynamic cast *) catchall_local vcatch.v_type pos))) com.basic.tvoid catch.epos); catch]) catch.etype catch.epos,
							     Some (loop tl))
						) catch.etype catch.epos
					| [] ->
						match catchall with
						| Some (v,s) ->
							Type.concat (mk (TVar (v, Some catchall_local)) com.basic.tvoid pos) s
						| None ->
							mk_block (rethrow_expr temp_local)
				in
				[(temp_var, catch_map temp_var { e with eexpr = TBlock [catchall_decl; loop must_wrap_catches] })]
			| _ ->
				[]
			in
			{ e with eexpr = TTry(run ttry, (List.rev nowrap_catches) @ dyn_catch) }
		| _ ->
			Type.map_expr run e
	in
	run

let find_class com path =
	let mt = List.find (fun mt -> match mt with TClassDecl c -> c.cl_path = path | _ -> false) com.types in
	match mt with TClassDecl c -> c | _ -> assert false

let configure_cs com =
	let base_exception = find_class com (["cs";"system"], "Exception") in
	let base_exception_t = TInst(base_exception, []) in
	let hx_exception = find_class com (["cs";"internal";"_Exceptions"], "HaxeException") in
	let hx_exception_t = TInst (hx_exception, []) in
	let exc_cl = find_class com (["cs";"internal"],"Exceptions") in
	let rec is_exception t =
		match follow t with
		| TInst (cl,_) -> is_parent base_exception cl
		| _ -> false
	in
	let e_rethrow = mk (TIdent "__rethrow__") t_dynamic null_pos in
	let should_wrap t = not (is_exception t) in
	let wrap_throw expr =
		match expr.eexpr with
		| TIdent "__rethrow__" ->
			make_throw expr expr.epos
		| _ ->
			let e_hxexception = make_static_this hx_exception expr.epos in
			let e_wrap = fcall e_hxexception "wrap" [expr] base_exception_t expr.epos in
			make_throw e_wrap expr.epos
	in
	let unwrap_expr local_to_unwrap = Codegen.field (mk_cast local_to_unwrap hx_exception_t local_to_unwrap.epos) "obj" t_dynamic local_to_unwrap.epos in
	let rethrow_expr rethrow = make_throw e_rethrow rethrow.epos in
	let catch_map v e =
		let e_exc = make_static_this exc_cl e.epos in
		let e_field = Codegen.field e_exc "exception" base_exception_t e.epos in
		let e_setstack = binop OpAssign e_field (make_local v e.epos) v.v_type e.epos in
		Type.concat e_setstack e
	in
	let std_cl = find_class com ([],"Std") in
	let gen_typecheck e t pos =
		let std = make_static_this std_cl pos in
		let e_type = make_typeexpr (module_type_of_type t) pos in
		fcall std "is" [e; e_type] com.basic.tbool pos
	in
	init com should_wrap wrap_throw unwrap_expr rethrow_expr base_exception_t hx_exception_t catch_map gen_typecheck

let configure_java com =
	let base_exception = find_class com (["java"; "lang"], "Throwable") in
	let base_exception_t = TInst (base_exception, []) in
	let hx_exception = find_class com (["java";"internal";"_Exceptions"], "HaxeException") in
	let hx_exception_t = TInst (hx_exception, []) in
	let exc_cl = find_class com (["java";"internal"],"Exceptions") in
	let rec is_exception t =
		match follow t with
		| TInst (cl,_) -> is_parent base_exception cl
		| _ -> false
	in
	let should_wrap t = not (is_exception t) in
	let wrap_throw expr =
		let e_hxexception = make_static_this hx_exception expr.epos in
		let e_wrap = fcall e_hxexception "wrap" [expr] base_exception_t expr.epos in
		make_throw e_wrap expr.epos
	in
	let unwrap_expr local_to_unwrap = Codegen.field (mk_cast local_to_unwrap hx_exception_t local_to_unwrap.epos) "obj" t_dynamic local_to_unwrap.epos in
	let rethrow_expr exc = { exc with eexpr = TThrow exc } in
	let catch_map v e =
		let exc = make_static_this exc_cl e.epos in
		let e_setstack = fcall exc "setException" [make_local v e.epos] com.basic.tvoid e.epos in
		Type.concat e_setstack e;
	in
	let std_cl = find_class com ([],"Std") in
	let gen_typecheck e t pos =
		let std = make_static_this std_cl pos in
		let e_type = make_typeexpr (module_type_of_type t) pos in
		fcall std "is" [e; e_type] com.basic.tbool pos
	in
	init com should_wrap wrap_throw unwrap_expr rethrow_expr base_exception_t hx_exception_t catch_map gen_typecheck
