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
open Ast
open Type
open Gencommon

(*
	This module will filter every dynamic field access in haxe.

	On platforms that do not support dynamic access, it is with this that you should
	replace dynamic calls with x.field / Reflect.setField calls, and guess what -
	this is the default implemenation!
	Actually there is a problem with Reflect.setField because it returns void, which is a bad thing for us,
	so even in the default implementation, the function call should be specified to a Reflect.setField version that returns
	the value that was set

	(TODO: should it be separated?)
	As a plus, the default implementation adds something that doesn't hurt anybody, it looks for
	TAnon with Statics / EnumStatics field accesses and transforms them into real static calls.
	This means it will take this

	var m = Math;
	for (i in 0...1000) m.cos(10);

	which is an optimization in dynamic platforms, but performs horribly on strongly typed platforms
	and transform into:

	var m = Math;
	for (i in 0...1000) Math.cos(10);

	depends on:
		(ok) must run AFTER Binop/Unop handler - so Unops / Binops are already unrolled
*)
let name = "dynamic_field_access"
let priority = solve_deps name [DAfter DynamicOperators.priority]

(*
	is_dynamic (expr) (field_access_expr) (field) : a function that indicates if the field access should be changed
	change_expr (expr) (field_access_expr) (field) (setting expr) (is_unsafe) : changes the expression
	call_expr (expr) (field_access_expr) (field) (call_params) : changes a call expression
*)
let configure gen (is_dynamic:texpr->Type.tfield_access->bool) (change_expr:texpr->texpr->string->texpr option->bool->texpr) (call_expr:texpr->texpr->string->texpr list->texpr) =
	let is_nondynamic_tparam fexpr f = match follow fexpr.etype with
		| TInst({ cl_kind = KTypeParameter(tl) }, _) ->
			List.exists (fun t -> not (is_dynamic { fexpr with etype = t } f)) tl
		| _ -> false
	in

	let rec run e =
		match e.eexpr with
		(* class types *)
		| TField(fexpr, f) when is_nondynamic_tparam fexpr f ->
			(match follow fexpr.etype with
				| TInst( ({ cl_kind = KTypeParameter(tl) } as tp_cl), tp_tl) ->
					let t = apply_params tp_cl.cl_params tp_tl (List.find (fun t -> not (is_dynamic { fexpr with etype = t } f)) tl) in
					{ e with eexpr = TField(mk_cast t (run fexpr), f) }
				| _ -> Globals.die())

		| TField(fexpr, f) when is_some (anon_class fexpr.etype) ->
			let decl = get (anon_class fexpr.etype) in
			let name = field_name f in
			(try
				match decl with
				| TClassDecl cl ->
					let cf = PMap.find name cl.cl_statics in
					{ e with eexpr = TField ({ fexpr with eexpr = TTypeExpr decl }, FStatic (cl, cf)) }
				| TEnumDecl en ->
					let ef = PMap.find name en.e_constrs in
					{ e with eexpr = TField ({ fexpr with eexpr = TTypeExpr decl }, FEnum (en, ef)) }
				| TAbstractDecl _ (* abstracts don't have TFields *)
				| TTypeDecl _ -> (* anon_class doesn't return TTypeDecl *)
					Globals.die()
			with Not_found ->
				match f with
				| FStatic (cl, cf) when has_class_field_flag cf CfExtern ->
					{ e with eexpr = TField ({ fexpr with eexpr = TTypeExpr decl }, FStatic (cl, cf)) }
				| _ ->
					change_expr e { fexpr with eexpr = TTypeExpr decl } (field_name f) None true)

		| TField (fexpr, f) when is_dynamic fexpr f ->
			change_expr e (run fexpr) (field_name f) None true

		| TCall ({ eexpr = TField (_, FStatic({ cl_path = ([], "Reflect") }, { cf_name = "field" })) }, [obj; { eexpr = TConst (TString field) }]) ->
			let t = match gen.greal_type obj.etype with
			| TDynamic _ | TAnon _ | TMono _ -> t_dynamic
			| t -> t
			in
			change_expr (mk_field_access gen { obj with etype = t } field obj.epos) (run obj) field None false

		| TCall ({ eexpr = TField (_, FStatic({ cl_path = ([], "Reflect") }, { cf_name = "setField" } )) }, [obj; { eexpr = TConst(TString field) }; evalue]) ->
			change_expr (mk_field_access gen obj field obj.epos) (run obj) field (Some (run evalue)) false

		| TBinop (OpAssign, { eexpr = TField(fexpr, f) }, evalue) when is_dynamic fexpr f ->
			change_expr e (run fexpr) (field_name f) (Some (run evalue)) true

		| TBinop (OpAssign, { eexpr = TField(fexpr, f) }, evalue) ->
			(match field_access_esp gen fexpr.etype f with
			| FClassField(_,_,_,cf,false,t,_) when (try PMap.find cf.cf_name gen.gbase_class_fields == cf with Not_found -> false) ->
				change_expr e (run fexpr) (field_name f) (Some (run evalue)) true
			| _ ->
				Type.map_expr run e)

		| TBinop (OpAssignOp _, { eexpr = TField (fexpr, f) }, _) when is_dynamic fexpr f ->
			Globals.die() (* this case shouldn't happen *)
		| TUnop (Increment, _, { eexpr = TField (({ eexpr = TLocal _ } as fexpr), f)})
		| TUnop (Decrement, _, { eexpr = TField (({ eexpr = TLocal _ } as fexpr), f)}) when is_dynamic fexpr f ->
			Globals.die() (* this case shouldn't happen *)

		| TCall ({ eexpr = TField (fexpr, f) }, params) when is_dynamic fexpr f && (not (is_nondynamic_tparam fexpr f)) ->
			call_expr e (run fexpr) (field_name f) (List.map run params)

		| _ ->
			Type.map_expr run e
	in
	gen.gexpr_filters#add name (PCustom priority) run
