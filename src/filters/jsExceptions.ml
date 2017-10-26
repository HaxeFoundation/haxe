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
	| { cl_path = ["js"],"Error" } -> true
	| { cl_super = Some (csup,_) } -> is_js_error csup
	| _ -> false

let find_cl ctx path =
	ExtList.List.find_map (function
		| TClassDecl c when c.cl_path = path -> Some c
		| _ -> None
	) ctx.com.types

let init ctx =
	let cJsError = find_cl ctx (["js"],"Error") in
	let cHaxeError = find_cl ctx (["js";"_Boot"],"HaxeError") in
	let cStd = find_cl ctx ([],"Std") in
	let cBoot = find_cl ctx (["js"],"Boot") in
	let cSyntax = find_cl ctx (["js"],"Syntax") in
	let cCallStack = try Some (find_cl ctx (["haxe"],"CallStack")) with Not_found -> None in

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

			let catchall_name = match catches with [(v,_)] -> v.v_name | _ -> "e" in
			let vcatchall = alloc_var catchall_name t_dynamic e.epos in
			let ecatchall = make_local vcatchall e.epos in
			let erethrow = mk (TThrow ecatchall) t_dynamic e.epos in

			let eSyntax = make_static_this cSyntax e.epos in
			let eHaxeError = make_static_this cHaxeError e.epos in
			let eInstanceof = fcall eSyntax "instanceof" [ecatchall;eHaxeError] ctx.com.basic.tbool e.epos in
			let eVal = field { ecatchall with etype = TInst (cHaxeError,[]) } "val" t_dynamic e.epos in
			let eunwrap = mk (TIf (eInstanceof, eVal, Some (ecatchall))) t_dynamic e.epos in

			let vunwrapped = alloc_var catchall_name t_dynamic e.epos in
			vunwrapped.v_meta <- (Meta.CompilerGenerated,[],Globals.null_pos) :: vunwrapped.v_meta;
			let eunwrapped = make_local vunwrapped e.epos in

			let ecatch = List.fold_left (fun acc (v,ecatch) ->
				let ecatch = loop (Some ecatchall) ecatch in

				(* it's not really compiler-generated, but it kind of is, since it was used as catch identifier and we add a TVar for it *)
				v.v_meta <- (Meta.CompilerGenerated,[],Globals.null_pos) :: v.v_meta;

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
						match Optimizer.api_inline ctx cStd "is" args e.epos with
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

			let ecatch_block = [
				mk (TVar (vunwrapped, Some eunwrap)) ctx.com.basic.tvoid e.epos;
				ecatch;
			] in

			let ecatch_block =
				match cCallStack with
				| None -> ecatch_block
				| Some c ->
					(*
						TODO: we should use `__feature__` here, but analyzer won't like an unbound identifier,
						so let's wait for some proper way to deal with feature-conditional expressions (@:ifFeature maybe?)

						Alternatively, we could run an additional post-analyzer/dce filter that adds these assignments.
					*)
					let eCallStack = make_static_this c ecatch.epos in
					let elastException = field eCallStack "lastException" t_dynamic ecatch.epos in
					let estore = mk (TBinop (Ast.OpAssign, elastException, ecatchall)) ecatch.etype ecatch.epos in
					estore :: ecatch_block
			in

			let ecatch = { ecatch with eexpr = TBlock ecatch_block } in
			{ e with eexpr = TTry (etry, [(vcatchall,ecatch)]) }
		| _ ->
			Type.map_expr (loop vrethrow) e
	in
	loop None
