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

(*
	This filter handles everything related to exceptions for the JavaScript target:

	- wrapping non-js.Error types in HaxeError on throwing
	- unwrapping HaxeError on catch
	- transforming series of catches into a single catch with Std.is checks (optimized)
	- re-throwing caught exception with js.Lib.rethrow
	- storing caught exception in haxe.CallStack.lastException (if haxe.CallStack is used)

	Basically it translates this:

	 try throw "fail"
	 catch (e:String) { trace(e); js.Lib.rethrow(); }
	 catch (e:Bool) {}

	into something like this (JS):

	 try {
		 throw new HaxeError("fail");
	 } catch (e) {
		 haxe.CallStack.lastException = e;
		 var e1 = (e instanceof HaxeError) e.val : e;
		 if (typeof e1 == "string") {
			 trace(e1);
			 throw e;
		 } else if (typeof e1 == "boolean") {
		 } else {
			 throw e;
		 }
	 }
*)

open Common
open Type
open Typecore
open Texpr.Builder

let follow = Abstract.follow_with_abstracts

let rec is_js_error c =
	match c with
	| { cl_path = ["js";"lib"],"Error" } -> true
	| { cl_super = Some (csup,_) } -> is_js_error csup
	| _ -> false

let find_cl com path =
	ExtList.List.find_map (function
		| TClassDecl c when c.cl_path = path -> Some c
		| _ -> None
	) com.types

let init ctx =
	let cJsError = find_cl ctx.com (["js";"lib"],"Error") in
	let cHaxeError = find_cl ctx.com (["js";"_Boot"],"HaxeError") in
	let cStd = find_cl ctx.com ([],"Std") in
	let cBoot = find_cl ctx.com (["js"],"Boot") in
	let cSyntax = find_cl ctx.com (["js"],"Syntax") in

	let dynamic_wrap e =
		let eHaxeError = make_static_this cHaxeError e.epos in
		fcall eHaxeError "wrap" [e] (TInst (cJsError, [])) e.epos
	in

	let static_wrap e =
		{ e with eexpr = TNew (cHaxeError,[],[e]); etype = TInst (cHaxeError,[]) }
	in

	let rec loop vrethrow e =
		match e.eexpr with
		| TThrow eexc ->
			let eexc = loop vrethrow eexc in
			let eexc =
				match follow eexc.etype with
				| TDynamic _ | TMono _ ->
					(match eexc.eexpr with
					| TConst (TInt _ | TFloat _ | TString _ | TBool _ | TNull) -> static_wrap eexc
					| _ -> dynamic_wrap eexc)
				| TInst (c,_) when (is_js_error c) ->
					eexc
				| _ ->
					static_wrap eexc
			in
			{ e with eexpr = TThrow eexc }

		| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ["js"],"Lib" }, { cf_name = "getOriginalException" })) }, _) ->
			(match vrethrow with
			| Some erethrowvar -> erethrowvar
			| None -> abort "js.Lib.getOriginalException can only be called inside a catch block" e.epos)

		| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ["js"],"Lib" }, { cf_name = "rethrow" })) }, _) ->
			(match vrethrow with
			| Some erethrowvar -> { e with eexpr = TThrow erethrowvar }
			| None -> abort "js.Lib.rethrow can only be called inside a catch block" e.epos)

		| TTry (etry, catches) ->
			let etry = loop vrethrow etry in

			let catchall_name, catchall_kind = match catches with [(v,_)] -> v.v_name, (VUser TVOCatchVariable) | _ -> "e", VGenerated in
			let vcatchall = alloc_var catchall_kind catchall_name t_dynamic e.epos in
			let ecatchall = make_local vcatchall e.epos in
			let erethrow = mk (TThrow ecatchall) t_dynamic e.epos in

			let eSyntax = make_static_this cSyntax e.epos in
			let eHaxeError = make_static_this cHaxeError e.epos in
			let eInstanceof = fcall eSyntax "instanceof" [ecatchall;eHaxeError] ctx.com.basic.tbool e.epos in
			let eVal = field { ecatchall with etype = TInst (cHaxeError,[]) } "val" t_dynamic e.epos in
			let eunwrap = mk (TIf (eInstanceof, eVal, Some (ecatchall))) t_dynamic e.epos in

			let vunwrapped = alloc_var catchall_kind catchall_name t_dynamic e.epos in
			vunwrapped.v_kind <- VGenerated;
			let eunwrapped = make_local vunwrapped e.epos in

			let ecatch = List.fold_left (fun acc (v,ecatch) ->
				let ecatch = loop (Some ecatchall) ecatch in

				(* it's not really compiler-generated, but it kind of is, since it was used as catch identifier and we add a TVar for it *)
				v.v_kind <- VGenerated;

				match follow v.v_type with
				| TDynamic _ ->
					{ ecatch with
						eexpr = TBlock [
							mk (TVar (v, Some eunwrapped)) ctx.com.basic.tvoid ecatch.epos;
							ecatch;
						]
					}
				| t ->
					let etype = make_typeexpr (module_type_of_type t) e.epos in
					let args = [eunwrapped;etype] in
					let echeck =
						match Inline.api_inline ctx cStd "is" args e.epos with
						| Some e -> e
						| None ->
							let eBoot = make_static_this cBoot e.epos in
							fcall eBoot "__instanceof" [eunwrapped;etype] ctx.com.basic.tbool e.epos
					in
					let ecatch = { ecatch with
						eexpr = TBlock [
							mk (TVar (v, Some eunwrapped)) ctx.com.basic.tvoid ecatch.epos;
							ecatch;
						]
					} in
					mk (TIf (echeck, ecatch, Some acc)) e.etype e.epos
			) erethrow (List.rev catches) in

			let ecatch = { ecatch with
				eexpr = TBlock [
					mk (TVar (vunwrapped, Some eunwrap)) ctx.com.basic.tvoid e.epos;
					ecatch;
				]
			} in
			{ e with eexpr = TTry (etry, [(vcatchall,ecatch)]) }
		| _ ->
			Type.map_expr (loop vrethrow) e
	in
	loop None

let inject_callstack com type_filters =
	let cCallStack =
		if Common.has_dce com then
			if Common.has_feature com "haxe.CallStack.lastException" then
				Some (find_cl com (["haxe"],"CallStack"))
			else
				None
		else
			try Some (find_cl com (["haxe"],"CallStack")) with Not_found -> None
	in
	match cCallStack with
	| Some cCallStack ->
		let run mt e =
			let rec loop e =
				match e.eexpr with
				| TTry (etry,[(v,ecatch)]) ->
					let etry = loop etry in
					let ecatch = loop ecatch in
					add_dependency (t_infos mt).mt_module cCallStack.cl_module;
					let eCallStack = make_static_this cCallStack ecatch.epos in
					let elastException = field eCallStack "lastException" t_dynamic ecatch.epos in
					let elocal = make_local v ecatch.epos in
					let eStoreException = mk (TBinop (Ast.OpAssign, elastException, elocal)) ecatch.etype ecatch.epos in
					let ecatch = Type.concat eStoreException ecatch in
					{ e with eexpr = TTry (etry,[(v,ecatch)]) }
				| TTry _ ->
					(* this should be handled by the filter above *)
					assert false
				| _ ->
					Type.map_expr loop e
			in
			loop e
		in
		type_filters @ [ fun ctx t -> FiltersCommon.run_expression_filters ctx [run t] t ]
	| None ->
		type_filters
