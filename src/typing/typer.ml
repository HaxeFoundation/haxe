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
open Extlib_leftovers
open Ast
open DisplayTypes.DisplayMode
open DisplayException
open DisplayTypes.CompletionResultKind
open CompletionItem.ClassFieldOrigin
open Common
open Type
open Typecore
open Error
open Globals
open TyperBase
open Fields
open Calls

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

let check_assign ctx e =
	match e.eexpr with
	| TLocal v when has_var_flag v VFinal ->
		error "Cannot assign to final" e.epos
	| TLocal {v_extra = None} | TArray _ | TField _ | TIdent _ ->
		()
	| TConst TThis | TTypeExpr _ when ctx.untyped ->
		()
	| _ ->
		invalid_assign e.epos

type type_class =
	| KInt
	| KFloat
	| KString
	| KUnk
	| KDyn
	| KOther
	| KNumParam of t
	| KStrParam of t
	| KAbstract of tabstract * t list

let rec classify t =
	match follow t with
	| TInst ({ cl_path = ([],"String") },[]) -> KString
	| TAbstract({a_impl = Some _} as a,tl) -> KAbstract (a,tl)
	| TAbstract ({ a_path = [],"Int" },[]) -> KInt
	| TAbstract ({ a_path = [],"Float" },[]) -> KFloat
	| TAbstract (a,[]) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) a.a_to -> KNumParam t
	| TInst ({ cl_kind = KTypeParameter ctl },_) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) ctl -> KNumParam t
	| TAbstract (a,[]) when List.exists (fun t -> match classify t with KString -> true | _ -> false) a.a_to -> KStrParam t
	| TInst ({ cl_kind = KTypeParameter ctl },_) when List.exists (fun t -> match classify t with KString -> true | _ -> false) ctl -> KStrParam t
	| TMono r when r.tm_type = None -> KUnk
	| TDynamic _ -> KDyn
	| _ -> KOther

let get_iterator_param t =
	match follow t with
	| TAnon a ->
		if !(a.a_status) <> Closed then raise Not_found;
		(match follow (PMap.find "hasNext" a.a_fields).cf_type, follow (PMap.find "next" a.a_fields).cf_type with
		| TFun ([],tb), TFun([],t) when (match follow tb with TAbstract ({ a_path = [],"Bool" },[]) -> true | _ -> false) ->
			if PMap.fold (fun _ acc -> acc + 1) a.a_fields 0 <> 2 then raise Not_found;
			t
		| _ ->
			raise Not_found)
	| _ ->
		raise Not_found

let get_iterable_param t =
	match follow t with
	| TAnon a ->
		if !(a.a_status) <> Closed then raise Not_found;
		(match follow (PMap.find "iterator" a.a_fields).cf_type with
		| TFun ([],it) ->
			let t = get_iterator_param it in
			if PMap.fold (fun _ acc -> acc + 1) a.a_fields 0 <> 1 then raise Not_found;
			t
		| _ ->
			raise Not_found)
	| _ -> raise Not_found

let maybe_type_against_enum ctx f with_type iscall p =
	try
		begin match with_type with
		| WithType.WithType(t,_) ->
			let rec loop stack t = match follow t with
				| TEnum (en,_) ->
					true,en.e_path,en.e_names,TEnumDecl en
				| TAbstract ({a_impl = Some c} as a,_) when has_meta Meta.Enum a.a_meta ->
					let fields = ExtList.List.filter_map (fun cf ->
						if Meta.has Meta.Enum cf.cf_meta then Some cf.cf_name else None
					) c.cl_ordered_statics in
					false,a.a_path,fields,TAbstractDecl a
				| TAbstract (a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					begin match get_abstract_froms a pl with
						| [t2] ->
							if (List.exists (shallow_eq t) stack) then raise Exit;
							loop (t :: stack) t2
						| _ -> raise Exit
					end
				| _ ->
					raise Exit
			in
			let is_enum,path,fields,mt = loop [] t in
			let old = ctx.m.curmod.m_types in
			let restore () = ctx.m.curmod.m_types <- old in
			ctx.m.curmod.m_types <- ctx.m.curmod.m_types @ [mt];
			let e = try
				f()
			with
			| Error (Unknown_ident n,_) ->
				restore();
				raise_or_display_message ctx (StringError.string_error n fields ("Identifier '" ^ n ^ "' is not part of " ^ s_type_path path)) p;
				AKExpr (mk (TConst TNull) (mk_mono()) p)
			| exc ->
				restore();
				raise exc;
			in
			restore();
			begin match e with
				| AKExpr e ->
					begin match follow e.etype with
						| TFun(_,t') when is_enum ->
							(* TODO: this is a dodge for #7603 *)
							(try Type.unify t' t with Unify_error _ -> ());
							AKExpr e
						| _ ->
							if iscall then
								AKExpr e
							else begin
								AKExpr (AbstractCast.cast_or_unify ctx t e e.epos)
							end
					end
				| _ -> e (* ??? *)
			end
		| _ ->
			raise Exit
		end
	with Exit ->
		f()

let check_error ctx err p = match err with
	| Module_not_found ([],name) when Diagnostics.is_diagnostics_run ctx.com p ->
		DisplayToplevel.handle_unresolved_identifier ctx name p true
	| _ ->
		display_error ctx (error_msg err) p

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let rec unify_min_raise ctx (el:texpr list) : t =
	let basic = ctx.com.basic in
	match el with
	| [] -> spawn_monomorph ctx null_pos
	| [e] -> e.etype
	| _ ->
		let rec chk_null e = is_null e.etype || is_explicit_null e.etype ||
			match e.eexpr with
			| TConst TNull -> true
			| TBlock el ->
				(match List.rev el with
				| [] -> false
				| e :: _ -> chk_null e)
			| TParenthesis e | TMeta(_,e) -> chk_null e
			| _ -> false
		in
		(* First pass: Try normal unification and find out if null is involved. *)
		let rec loop t = function
			| [] ->
				false, t
			| e :: el ->
				let t = if chk_null e then basic.tnull t else t in
				try
					Type.unify e.etype t;
					loop t el
				with Unify_error _ -> try
					Type.unify t e.etype;
					loop (if is_null t then basic.tnull e.etype else e.etype) el
				with Unify_error _ ->
					true, t
		in
		let has_error, t = loop (spawn_monomorph ctx null_pos) el in
		if not has_error then
			t
		else try
			(* specific case for const anon : we don't want to hide fields but restrict their common type *)
			let fcount = ref (-1) in
			let field_count a =
				PMap.fold (fun _ acc -> acc + 1) a.a_fields 0
			in
			let expr f = match f.cf_expr with None -> mk (TBlock []) f.cf_type f.cf_pos | Some e -> e in
			let fields = List.fold_left (fun acc e ->
				match follow e.etype with
				| TAnon a when !(a.a_status) = Const ->
					if !fcount = -1 then begin
						fcount := field_count a;
						PMap.map (fun f -> [expr f]) a.a_fields
					end else begin
						if !fcount <> field_count a then raise Not_found;
						PMap.mapi (fun n el -> expr (PMap.find n a.a_fields) :: el) acc
					end
				| _ ->
					raise Not_found
			) PMap.empty el in
			let fields = PMap.foldi (fun n el acc ->
				let t = try unify_min_raise ctx el with Unify_error _ -> raise Not_found in
				PMap.add n (mk_field n t (List.hd el).epos null_pos) acc
			) fields PMap.empty in
			mk_anon ~fields (ref Closed)
		with Not_found -> try
			(* specific case for TFun, see #9579 *)
			let e0,el = match el with
				| e0 :: el -> e0,el
				| _ -> raise Exit
			in
			let args,tr0 = match follow e0.etype with
				| TFun(tl,tr) ->
					Array.of_list tl,tr
				| _ ->
					raise Exit
			in
			let arity = Array.length args in
			let rets = List.map (fun e -> match follow e.etype with
				| TFun(tl,tr) ->
					let ta = Array.of_list tl in
					if Array.length ta <> arity then raise Exit;
					for i = 0 to arity - 1 do
						let (_,_,tcur) = args.(i) in
						let (_,_,tnew) as argnew = ta.(i) in
						if Type.does_unify tnew tcur then
							args.(i) <- argnew
						else if not (Type.does_unify tcur tnew) then
							raise Exit
					done;
					tr
				| _ ->
					raise Exit
			) el in
			let common_types = UnifyMinT.collect_base_types tr0 in
			let tr = match UnifyMinT.unify_min' default_unification_context common_types rets with
			| UnifyMinOk t ->
				t
			| UnifyMinError(l,index) ->
				raise Exit
			in
			TFun(Array.to_list args,tr)
		with Exit ->
			(* Second pass: Get all base types (interfaces, super classes and their interfaces) of most general type.
			   Then for each additional type filter all types that do not unify. *)
			let common_types = UnifyMinT.collect_base_types t in
			let dyn_types = List.fold_left (fun acc t ->
				let rec loop c =
					Meta.has Meta.UnifyMinDynamic c.cl_meta || (match c.cl_super with None -> false | Some (c,_) -> loop c)
				in
				match t with
				| TInst (c,params) when params <> [] && loop c ->
					TInst (c,List.map (fun _ -> t_dynamic) params) :: acc
				| _ -> acc
			) [] common_types in
			let common_types = (match List.rev dyn_types with [] -> common_types | l -> common_types @ l) in
			let el = List.tl el in
			let tl = List.map (fun e -> e.etype) el in
			begin match UnifyMinT.unify_min' default_unification_context common_types tl with
			| UnifyMinOk t ->
				t
			| UnifyMinError(l,index) ->
				raise_error (Unify l) (List.nth el index).epos
			end

let unify_min ctx el =
	try unify_min_raise ctx el
	with Error (Unify l,p) ->
		if not ctx.untyped then display_error ctx (error_msg (Unify l)) p;
		(List.hd el).etype

let unify_min_for_type_source ctx el src =
	match src with
	| Some WithType.ImplicitReturn when List.exists (fun e -> ExtType.is_void (follow e.etype)) el ->
		ctx.com.basic.tvoid
	| _ ->
		unify_min ctx el

let rec type_ident_raise ctx i p mode with_type =
	let is_set = match mode with MSet _ -> true | _ -> false in
	match i with
	| "true" ->
		if mode = MGet then
			AKExpr (mk (TConst (TBool true)) ctx.t.tbool p)
		else
			AKNo i
	| "false" ->
		if mode = MGet then
			AKExpr (mk (TConst (TBool false)) ctx.t.tbool p)
		else
			AKNo i
	| "this" ->
		if is_set then add_class_field_flag ctx.curfield CfModifiesThis;
		(match mode, ctx.curclass.cl_kind with
		| MSet _, KAbstractImpl _ ->
			if not (assign_to_this_is_allowed ctx) then
				error "Abstract 'this' value can only be modified inside an inline function" p;
			AKExpr (get_this ctx p)
		| (MCall _, KAbstractImpl _) | (MGet, _)-> AKExpr(get_this ctx p)
		| _ -> AKNo i)
	| "super" ->
		let t = (match ctx.curclass.cl_super with
			| None -> error "Current class does not have a superclass" p
			| Some (c,params) -> TInst(c,params)
		) in
		(match ctx.curfun with
		| FunMember | FunConstructor -> ()
		| FunMemberAbstract -> error "Cannot access super inside an abstract function" p
		| FunStatic -> error "Cannot access super inside a static function" p;
		| FunMemberClassLocal | FunMemberAbstractLocal -> error "Cannot access super inside a local function" p);
		AKExpr (mk (TConst TSuper) t p)
	| "null" ->
		if mode = MGet then
			AKExpr (null (spawn_monomorph ctx p) p)
		else
			AKNo i
	| _ ->
	try
		let v = PMap.find i ctx.locals in
		(match v.v_extra with
		| Some ve ->
			let (params,e) = (ve.v_params,ve.v_expr) in
			let t = apply_params params (Monomorph.spawn_constrained_monos (fun t -> t) params) v.v_type in
			(match e with
			| Some ({ eexpr = TFunction f } as e) when ctx.com.display.dms_inline ->
				begin match mode with
					| MSet _ -> error "Cannot set inline closure" p
					| MGet -> error "Cannot create closure on inline closure" p
					| MCall _ ->
						(* create a fake class with a fake field to emulate inlining *)
						let c = mk_class ctx.m.curmod (["local"],v.v_name) e.epos null_pos in
						let cf = { (mk_field v.v_name v.v_type e.epos null_pos) with cf_params = params; cf_expr = Some e; cf_kind = Method MethInline } in
						add_class_flag c CExtern;
						c.cl_fields <- PMap.add cf.cf_name cf PMap.empty;
						AKInline (mk (TConst TNull) (TInst (c,[])) p, cf, FInstance(c,[],cf), t)
				end
			| _ ->
				AKExpr (mk (TLocal v) t p))
		| _ ->
			AKExpr (mk (TLocal v) v.v_type p))
	with Not_found -> try
		(* member variable lookup *)
		if ctx.curfun = FunStatic then raise Not_found;
		let c , t , f = class_field ctx ctx.curclass (List.map snd ctx.curclass.cl_params) i p in
		field_access ctx mode f (match c with None -> FAnon f | Some (c,tl) -> FInstance (c,tl,f)) t (get_this ctx p) p
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		if Meta.has Meta.Impl f.cf_meta && not (Meta.has Meta.Impl ctx.curfield.cf_meta) && not (Meta.has Meta.Enum f.cf_meta) then
			error (Printf.sprintf "Cannot access non-static field %s from static method" f.cf_name) p;
		let e = type_type ctx ctx.curclass.cl_path p in
		(* check_locals_masking already done in type_type *)
		field_access ctx mode f (FStatic (ctx.curclass,f)) (field_type ctx ctx.curclass [] f p) e p
	with Not_found -> try
		(* module-level statics *)
		(match ctx.m.curmod.m_statics with
		| None -> raise Not_found
		| Some c ->
			let f = PMap.find i c.cl_statics in
			let e = type_module_type ctx (TClassDecl c) None p in
			field_access ctx mode f (FStatic (c,f)) (field_type ctx c [] f p) e p
		)
	with Not_found -> try
		let wrap e = if is_set then
				AKNo i
			else
				AKExpr e
		in
		(* lookup imported enums *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| (t,pt) :: l ->
				match t with
				| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta ->
					begin try
						let cf = PMap.find i c.cl_statics in
						if not (Meta.has Meta.Enum cf.cf_meta) then
							loop l
						else begin
							let et = type_module_type ctx (TClassDecl c) None p in
							let fa = FStatic(c,cf) in
							let t = monomorphs cf.cf_params cf.cf_type in
							ImportHandling.mark_import_position ctx pt;
							begin match cf.cf_kind with
								| Var {v_read = AccInline} -> AKInline(et,cf,fa,t)
								| _ -> AKExpr (mk (TField(et,fa)) t p)
							end
						end
					with Not_found ->
						loop l
					end
				| TClassDecl _ | TAbstractDecl _ ->
					loop l
				| TTypeDecl t ->
					(match follow t.t_type with
					| TEnum (e,_) -> loop ((TEnumDecl e,pt) :: l)
					| TAbstract (a,_) when Meta.has Meta.Enum a.a_meta -> loop ((TAbstractDecl a,pt) :: l)
					| _ -> loop l)
				| TEnumDecl e ->
					try
						let ef = PMap.find i e.e_constrs in
						let et = type_module_type ctx t None p in
						ImportHandling.mark_import_position ctx pt;
						wrap (mk (TField (et,FEnum (e,ef))) (enum_field_type ctx e ef p) p)
					with
						Not_found -> loop l
		in
		(try loop (List.rev_map (fun t -> t,null_pos) ctx.m.curmod.m_types) with Not_found -> loop ctx.m.module_types)
	with Not_found ->
		(* lookup imported globals *)
		let t, name, pi = PMap.find i ctx.m.module_globals in
		ImportHandling.mark_import_position ctx pi;
		let e = type_module_type ctx t None p in
		type_field_default_cfg ctx e name p mode with_type

(*
	We want to try unifying as an integer and apply side effects.
	However, in case the value is not a normal Monomorph but one issued
	from a Dynamic relaxation, we will instead unify with float since
	we don't want to accidentaly truncate the value
*)
let unify_int ctx e k =
	let is_dynamic t =
		match follow t with
		| TDynamic _ -> true
		| _ -> false
	in
	let is_dynamic_array t =
		match follow t with
		| TInst (_,[p]) -> is_dynamic p
		| _ -> true
	in
	let is_dynamic_field t f =
		match follow t with
		| TAnon a ->
			(try is_dynamic (PMap.find f a.a_fields).cf_type with Not_found -> false)
		| TMono m ->
			begin match Monomorph.classify_constraints m with
			| CStructural(fields,_) ->
				(try is_dynamic (PMap.find f fields).cf_type with Not_found -> false)
			| _ ->
				true
			end
		| TInst (c,tl) ->
			(try is_dynamic (apply_params c.cl_params tl ((let _,t,_ = Type.class_field c tl f in t))) with Not_found -> false)
		| _ ->
			true
	in
	let is_dynamic_return t =
		match follow t with
		| TFun (_,r) -> is_dynamic r
		| _ -> true
	in
	(*
		This is some quick analysis that matches the most common cases of dynamic-to-mono convertions
	*)
	let rec maybe_dynamic_mono e =
		match e.eexpr with
		| TLocal _ -> is_dynamic e.etype
		| TArray({ etype = t } as e,_) -> is_dynamic_array t || maybe_dynamic_rec e t
		| TField({ etype = t } as e,f) -> is_dynamic_field t (field_name f) || maybe_dynamic_rec e t
		| TCall({ etype = t } as e,_) -> is_dynamic_return t || maybe_dynamic_rec e t
		| TParenthesis e | TMeta(_,e) -> maybe_dynamic_mono e
		| TIf (_,a,Some b) -> maybe_dynamic_mono a || maybe_dynamic_mono b
		| _ -> false
	and maybe_dynamic_rec e t =
		match follow t with
		| TMono _ | TDynamic _ -> maybe_dynamic_mono e
		(* we might have inferenced a tmono into a single field *)
		(* TODO: check what this did exactly *)
		(* | TAnon a when !(a.a_status) = Opened -> maybe_dynamic_mono e *)
		| _ -> false
	in
	match k with
	| KUnk | KDyn when maybe_dynamic_mono e ->
		unify ctx e.etype ctx.t.tfloat e.epos;
		false
	| _ ->
		unify ctx e.etype ctx.t.tint e.epos;
		true

let rec type_binop ctx op e1 e2 is_assign_op with_type p =
	let type_non_assign_op abstract_overload_only =
		(* If the with_type is an abstract which has exactly one applicable @:op method, we can promote it
		   to the individual arguments (issue #2786). *)
		let wt = match with_type with
			| WithType.WithType(t,_) ->
				begin match follow t with
					| TAbstract(a,_) ->
						begin match List.filter (fun (o,_) -> o = OpAssignOp(op) || o == op) a.a_ops with
							| [_] -> with_type
							| _ -> WithType.value
						end
					| _ ->
						WithType.value
				end
			| _ ->
				WithType.value
		in
		let e1 = type_expr ctx e1 wt in
		type_binop2 ~abstract_overload_only ctx op e1 e2 is_assign_op wt p
	in
	match op with
	| OpAssign ->
		let e1 = type_access ctx (fst e1) (snd e1) (MSet (Some e2)) with_type in
		let e2 with_type = type_expr ctx e2 with_type in
		(match e1 with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr { eexpr = TLocal { v_kind = VUser TVOLocalFunction; v_name = name } } ->
			error ("Cannot access function " ^ name ^ " for writing") p
		| AKExpr e1  ->
			let e2 = e2 (WithType.with_type e1.etype) in
			let e2 = AbstractCast.cast_or_unify ctx e1.etype e2 p in
			check_assign ctx e1;
			(match e1.eexpr , e2.eexpr with
			| TLocal i1 , TLocal i2 when i1 == i2 -> error "Assigning a value to itself" p
			| TField ({ eexpr = TConst TThis },FInstance (_,_,f1)) , TField ({ eexpr = TConst TThis },FInstance (_,_,f2)) when f1 == f2 ->
				error "Assigning a value to itself" p
			| _ , _ -> ());
			mk (TBinop (op,e1,e2)) e1.etype p
		| AKSet (e,t,cf) ->
			let e2 = e2 (WithType.with_type t) in
			let e2 = AbstractCast.cast_or_unify ctx t e2 p in
			make_call ctx (mk (TField (e,quick_field_dynamic e.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [e2] t p
		| AKAccess(a,tl,c,ebase,ekey) ->
			let e2 = e2 WithType.value in
			mk_array_set_call ctx (AbstractCast.find_array_access ctx a tl ekey (Some e2) p) c ebase p
		| AKFieldSet(ethis,e1,fname,t) ->
			let e2 = e2 (WithType.with_type t) in
			begin match follow e1.etype with
				| TFun([_;_;(_,_,t)],_) -> unify ctx e2.etype t e2.epos;
				| _ -> die "" __LOC__
			end;
			make_call ctx e1 [ethis;Texpr.Builder.make_string ctx.t fname null_pos;e2] t p
		| AKUsing(ef,_,_,et,_) ->
			(* this must be an abstract setter *)
			let e2,ret = match follow ef.etype with
				| TFun([_;(_,_,t)],ret) ->
					let e2 = e2 (WithType.with_type t) in
					AbstractCast.cast_or_unify ctx t e2 p,ret
				| _ ->  error "Invalid field type for abstract setter" p
			in
			make_call ctx ef [et;e2] ret p
		| AKInline _ | AKMacro _ ->
			die "" __LOC__)
	| OpAssignOp (OpBoolAnd | OpBoolOr) ->
		error "The operators ||= and &&= are not supported" p
	| OpAssignOp op ->
		(match type_access ctx (fst e1) (snd e1) (MSet (Some e2)) with_type with
		| AKNo s ->
			(* try abstract operator overloading *)
			(try type_non_assign_op true
			with Not_found -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
			)
		| AKExpr e ->
			let save = save_locals ctx in
			let v = gen_local ctx e.etype e.epos in
			let has_side_effect = OptimizerTexpr.has_side_effect e in
			let e1 = if has_side_effect then (EConst(Ident v.v_name),e.epos) else e1 in
			let eop = type_binop ctx op e1 e2 true with_type p in
			save();
			(match eop.eexpr with
			| TBinop (_,_,e2) ->
				unify ctx eop.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssignOp op,e,e2)) e.etype p;
			| TMeta((Meta.RequiresAssign,_,_),e2) ->
				unify ctx e2.etype e.etype p;
				check_assign ctx e;
				begin match e.eexpr with
					| TArray(ea1,ea2) when has_side_effect ->
						let v1 = gen_local ctx ea1.etype ea1.epos in
						let ev1 = mk (TLocal v1) v1.v_type p in
						let v2 = gen_local ctx ea2.etype ea2.epos in
						let ev2 = mk (TLocal v2) v2.v_type p in
						let e = {e with eexpr = TArray(ev1,ev2)} in
						mk (TBlock [
							mk (TVar(v1,Some ea1)) ctx.t.tvoid p;
							mk (TVar(v2,Some ea2)) ctx.t.tvoid p;
							mk (TVar(v,Some e)) ctx.t.tvoid p;
							mk (TBinop (OpAssign,e,e2)) e.etype p;
						]) e.etype p
					| TField(ea1,fa) when has_side_effect ->
						let v1 = gen_local ctx ea1.etype ea1.epos in
						let ev1 = mk (TLocal v1) v1.v_type p in
						let e = {e with eexpr = TField(ev1,fa)} in
						mk (TBlock [
							mk (TVar(v1,Some ea1)) ctx.t.tvoid p;
							mk (TVar(v,Some e)) ctx.t.tvoid p;
							mk (TBinop (OpAssign,e,e2)) e.etype p;
						]) e.etype p
					| _ ->
						mk (TBinop (OpAssign,e,e2)) e.etype p;
				end
			| _ ->
				(* this must be an abstract cast *)
				check_assign ctx e;
				if has_side_effect then
					mk (TBlock [
						mk (TVar(v,Some e)) ctx.t.tvoid eop.epos;
						eop
					]) eop.etype eop.epos
				else
					eop)
		| AKSet (e,t,cf) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype e.epos in
			let ev = mk (TLocal v) e.etype p in
			let get = type_binop ctx op (EField ((EConst (Ident v.v_name),p),cf.cf_name),p) e2 true with_type p in
			let e' = match get.eexpr with
				| TBinop _ | TMeta((Meta.RequiresAssign,_,_),_) ->
					unify ctx get.etype t p;
					make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [get] t p
				| _ ->
					(* abstract setter *)
					get
			in
			l();
			mk (TBlock [
				mk (TVar (v,Some e)) ctx.t.tvoid p;
				e'
			]) t p
		| AKUsing(ef,c,cf,et,_) ->
			(* abstract setter + getter *)
			let ta = match c.cl_kind with KAbstractImpl a -> TAbstract(a,Monomorph.spawn_constrained_monos (fun t -> t) a.a_params) | _ -> die "" __LOC__ in
			let ret = match follow ef.etype with
				| TFun([_;_],ret) -> ret
				| _ -> error "Invalid field type for abstract setter" p
			in
			let l = save_locals ctx in
			let v,init_exprs,abstr_this_to_modify = match et.eexpr with
				| TLocal v when not (Meta.has Meta.This v.v_meta) -> v,[],None
				| _ ->
					let v = gen_local ctx ta ef.epos in
					(match et.eexpr with
					| TLocal { v_meta = m } -> v.v_meta <- Meta.copy_from_to Meta.This m v.v_meta
					| _ -> ()
					);
					let decl_v e = mk (TVar (v,Some e)) ctx.t.tvoid p in
					let rec needs_temp_var e =
						match e.eexpr with
						| TConst TThis | TTypeExpr _ -> false
						| TField (e1,(FInstance(_,_,cf) | FStatic(_,cf)))
							when has_class_field_flag cf CfFinal ->
							needs_temp_var e1
						| TParenthesis e1 ->
							needs_temp_var e1
						| _ -> true
					in
					if has_class_field_flag cf CfModifiesThis then
						match et.eexpr with
						| TField (target,fa) when needs_temp_var target->
							let tmp = gen_local ctx target.etype target.epos in
							let decl_tmp = mk (TVar (tmp,Some target)) ctx.t.tvoid target.epos in
							let etmp = mk (TLocal tmp) tmp.v_type tmp.v_pos in
							let athis = mk (TField (etmp,fa)) et.etype et.epos in
							v,[decl_tmp; decl_v athis],(Some athis)
						| TArray (target,index) when needs_temp_var target ->
							let tmp = gen_local ctx target.etype target.epos in
							let decl_tmp = mk (TVar (tmp,Some target)) ctx.t.tvoid target.epos in
							let etmp = mk (TLocal tmp) tmp.v_type tmp.v_pos in
							let athis = mk (TArray (etmp,index)) et.etype et.epos in
							v,[decl_tmp; decl_v athis],(Some athis)
						| _ ->
							check_assign ctx et;
							v,[decl_v et],(Some et)
					else
						v,[decl_v et],None
			in
			let ev = mk (TLocal v) ta p in
			(* this relies on the fact that cf_name is set_name *)
			let getter_name = String.sub cf.cf_name 4 (String.length cf.cf_name - 4) in
			let get = type_binop ctx op (EField ((EConst (Ident v.v_name),p),getter_name),p) e2 true with_type p in
			unify ctx get.etype ret p;
			l();
			let e_call = make_call ctx ef [ev;get] ret p in
			let e_call =
				(*
					If this method modifies abstract `this`, we should also apply temp var
					modifications to the original tempvar-ed expression.
					Find code like `v = value` and change it to `et = v = value`,
					where `v` is the temp var and `et` is the original expression stored to the temp var.
				*)
				match abstr_this_to_modify with
				| None ->
					e_call
				| Some athis ->
					let rec loop e =
						match e.eexpr with
						| TBinop(OpAssign,({ eexpr = TLocal v1 } as left),right) when v1 == v ->
							let right = { e with eexpr = TBinop(OpAssign,left,loop right) } in
							mk (TBinop(OpAssign,athis,right)) e.etype e.epos
						| _ ->
							map_expr loop e
					in
					loop e_call
			in
			mk (TBlock (init_exprs @ [e_call])) ret p
		| AKAccess(a,tl,c,ebase,ekey) ->
			let cf_get,tf_get,r_get,ekey,_ = AbstractCast.find_array_access ctx a tl ekey None p in
			(* bind complex keys to a variable so they do not make it into the output twice *)
			let save = save_locals ctx in
			let maybe_bind_to_temp e = match Optimizer.make_constant_expression ctx e with
				| Some e -> e,None
				| None ->
					let v = gen_local ctx e.etype p in
					let e' = mk (TLocal v) e.etype p in
					e', Some (mk (TVar (v,Some e)) ctx.t.tvoid p)
			in
			let ekey,ekey' = maybe_bind_to_temp ekey in
			let ebase,ebase' = maybe_bind_to_temp ebase in
			let eget = mk_array_get_call ctx (cf_get,tf_get,r_get,ekey,None) c ebase p in
			let eget = type_binop2 ctx op eget e2 true (WithType.with_type eget.etype) p in
			unify ctx eget.etype r_get p;
			let cf_set,tf_set,r_set,ekey,eget = AbstractCast.find_array_access ctx a tl ekey (Some eget) p in
			let eget = match eget with None -> die "" __LOC__ | Some e -> e in
			let et = type_module_type ctx (TClassDecl c) None p in
			let e = match cf_set.cf_expr,cf_get.cf_expr with
				| None,None ->
					let ea = mk (TArray(ebase,ekey)) r_get p in
					mk (TBinop(OpAssignOp op,ea,type_expr ctx e2 (WithType.with_type r_get))) r_set p
				| Some _,Some _ ->
					let ef_set = mk (TField(et,(FStatic(c,cf_set)))) tf_set p in
					let el = [make_call ctx ef_set [ebase;ekey;eget] r_set p] in
					let el = match ebase' with None -> el | Some ebase -> ebase :: el in
					let el = match ekey' with None -> el | Some ekey -> ekey :: el in
					begin match el with
						| [e] -> e
						| el -> mk (TBlock el) r_set p
					end
				| _ ->
					error "Invalid array access getter/setter combination" p
			in
			save();
			e
		| AKFieldSet _ ->
			error "Invalid operation" p
		| AKInline _ | AKMacro _ ->
			die "" __LOC__)
	| _ ->
		type_non_assign_op false

and type_binop2 ?(abstract_overload_only=false) ctx op (e1 : texpr) (e2 : Ast.expr) is_assign_op wt p =
	let with_type = match op with
		| OpEq | OpNotEq | OpLt | OpLte | OpGt | OpGte -> WithType.with_type e1.etype
		| _ -> wt
	in
	let e2 = type_expr ctx e2 with_type in
	let tint = ctx.t.tint in
	let tfloat = ctx.t.tfloat in
	let tstring = ctx.t.tstring in
	let to_string e =
		let rec loop t = match classify t with
			| KAbstract ({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
				call_to_string ctx e
			| KInt | KFloat | KString -> e
			| KUnk | KDyn | KNumParam _ | KStrParam _ | KOther ->
				let std = type_type ctx ([],"Std") e.epos in
				let acc = acc_get ctx (type_field_default_cfg ctx std "string" e.epos (MCall []) with_type) e.epos in
				ignore(follow acc.etype);
				let acc = (match acc.eexpr with TField (e,FClosure (Some (c,tl),f)) -> { acc with eexpr = TField (e,FInstance (c,tl,f)) } | _ -> acc) in
				make_call ctx acc [e] ctx.t.tstring e.epos
			| KAbstract (a,tl) ->
				try
					AbstractCast.cast_or_unify_raise ctx tstring e p
				with Error (Unify _,_) ->
					loop (Abstract.get_underlying_type a tl)
		in
		loop e.etype
	in
	let mk_op e1 e2 t =
		if op = OpAdd && (classify t) = KString then
			let e1 = to_string e1 in
			let e2 = to_string e2 in
			mk (TBinop (op,e1,e2)) t p
		else
			mk (TBinop (op,e1,e2)) t p
	in
	let make e1 e2 = match op with
	| OpAdd ->
		mk_op e1 e2 (match classify e1.etype, classify e2.etype with
		| KInt , KInt ->
			tint
		| KFloat , KInt
		| KInt, KFloat
		| KFloat, KFloat ->
			tfloat
		| KUnk , KInt ->
			if unify_int ctx e1 KUnk then tint else tfloat
		| KUnk , KFloat
		| KUnk , KString  ->
			unify ctx e1.etype e2.etype e1.epos;
			e1.etype
		| KInt , KUnk ->
			if unify_int ctx e2 KUnk then tint else tfloat
		| KFloat , KUnk
		| KString , KUnk ->
			unify ctx e2.etype e1.etype e2.epos;
			e2.etype
		| _ , KString
		| KString , _ ->
			tstring
		| _ , KDyn ->
			e2.etype
		| KDyn , _ ->
			e1.etype
		| KUnk , KUnk ->
			let ok1 = unify_int ctx e1 KUnk in
			let ok2 = unify_int ctx e2 KUnk in
			if ok1 && ok2 then tint else tfloat
		| KNumParam t1, KNumParam t2 when Type.type_iseq t1 t2 ->
			t1
		| KNumParam t, KInt | KInt, KNumParam t ->
			t
		| KNumParam _, KFloat | KFloat, KNumParam _ | KNumParam _, KNumParam _ ->
			tfloat
		| KNumParam t, KUnk ->
			unify ctx e2.etype tfloat e2.epos;
			tfloat
		| KUnk, KNumParam t ->
			unify ctx e1.etype tfloat e1.epos;
			tfloat
		| KStrParam _, _
		| _, KStrParam _ ->
			tstring
		| KAbstract _,KFloat ->
			unify ctx e1.etype tfloat e1.epos;
			tfloat
		| KFloat, KAbstract _ ->
			unify ctx e2.etype tfloat e2.epos;
			tfloat
		| KAbstract _,KInt ->
			unify ctx e1.etype ctx.t.tint e1.epos;
			ctx.t.tint
		| KInt, KAbstract _ ->
			unify ctx e2.etype ctx.t.tint e2.epos;
			ctx.t.tint
		| KAbstract _,_
		| _,KAbstract _
		| KNumParam _, _
		| _, KNumParam _
		| KOther, _
		| _ , KOther ->
			let pr = print_context() in
			error ("Cannot add " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		)
	| OpAnd
	| OpOr
	| OpXor
	| OpShl
	| OpShr
	| OpUShr ->
		let i = tint in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk_op e1 e2 i
	| OpMod
	| OpMult
	| OpDiv
	| OpSub ->
		let result = ref (if op = OpDiv then tfloat else tint) in
		(match classify e1.etype, classify e2.etype with
		| KFloat, KFloat ->
			result := tfloat
		| KNumParam t1, KNumParam t2 when Type.type_iseq t1 t2 ->
			if op <> OpDiv then result := t1
		| KNumParam _, KNumParam _ ->
			result := tfloat
		| KNumParam t, KInt | KInt, KNumParam t ->
			if op <> OpDiv then result := t
		| KNumParam _, KFloat | KFloat, KNumParam _ ->
			result := tfloat
		| KFloat, k ->
			ignore(unify_int ctx e2 k);
			result := tfloat
		| k, KFloat ->
			ignore(unify_int ctx e1 k);
			result := tfloat
		| k1 , k2 ->
			let ok1 = unify_int ctx e1 k1 in
			let ok2 = unify_int ctx e2 k2 in
			if not ok1 || not ok2  then result := tfloat;
		);
		mk_op e1 e2 !result
	| OpEq
	| OpNotEq ->
		let e1,e2 = try
			(* we only have to check one type here, because unification fails if one is Void and the other is not *)
			(match follow e2.etype with TAbstract({a_path=[],"Void"},_) -> error "Cannot compare Void" p | _ -> ());
			AbstractCast.cast_or_unify_raise ctx e2.etype e1 p,e2
		with Error (Unify _,_) ->
			e1,AbstractCast.cast_or_unify ctx e1.etype e2 p
		in
		if not ctx.com.config.pf_supports_function_equality then begin match e1.eexpr, e2.eexpr with
		| TConst TNull , _ | _ , TConst TNull -> ()
		| _ ->
			match follow e1.etype, follow e2.etype with
			| TFun _ , _ | _, TFun _ -> ctx.com.warning "Comparison of function values is unspecified on this target, use Reflect.compareMethods instead" p
			| _ -> ()
		end;
		mk_op e1 e2 ctx.t.tbool
	| OpGt
	| OpGte
	| OpLt
	| OpLte ->
		(match classify e1.etype, classify e2.etype with
		| KInt , KInt | KInt , KFloat | KFloat , KInt | KFloat , KFloat | KString , KString -> ()
		| KInt , KUnk -> ignore(unify_int ctx e2 KUnk)
		| KFloat , KUnk | KString , KUnk -> unify ctx e2.etype e1.etype e2.epos
		| KUnk , KInt -> ignore(unify_int ctx e1 KUnk)
		| KUnk , KFloat | KUnk , KString -> unify ctx e1.etype e2.etype e1.epos
		| KUnk , KUnk ->
			ignore(unify_int ctx e1 KUnk);
			ignore(unify_int ctx e2 KUnk);
		| KDyn , KInt | KDyn , KFloat | KDyn , KString -> ()
		| KInt , KDyn | KFloat , KDyn | KString , KDyn -> ()
		| KDyn , KDyn -> ()
		| KNumParam _ , (KInt | KFloat | KNumParam _ | KDyn | KUnk ) -> ()
		| (KInt | KFloat | KDyn | KUnk ), KNumParam _ -> ()
		| KStrParam _ , (KString | KStrParam _ | KUnk | KDyn) -> ()
		| (KString | KUnk | KDyn) , KStrParam _ -> ()
		| KAbstract _,_
		| _,KAbstract _
		| KDyn , KUnk
		| KUnk , KDyn
		| KString , KInt
		| KString , KFloat
		| KInt , KString
		| KFloat , KString
		| KNumParam _ , _
		| _ , KNumParam _
		| KStrParam _ , _
		| _ , KStrParam _
		| KOther , _
		| _ , KOther ->
			let pr = print_context() in
			error ("Cannot compare " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		);
		mk_op e1 e2 ctx.t.tbool
	| OpBoolAnd
	| OpBoolOr ->
		let b = ctx.t.tbool in
		unify ctx e1.etype b p;
		unify ctx e2.etype b p;
		mk_op e1 e2 b
	| OpInterval ->
		let t = Typeload.load_core_type ctx "IntIterator" in
		unify ctx e1.etype tint e1.epos;
		unify ctx e2.etype tint e2.epos;
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> die "" __LOC__),[],[e1;e2])) t p
	| OpArrow ->
		error "Unexpected =>" p
	| OpIn ->
		error "Unexpected in" p
	| OpAssign
	| OpAssignOp _ ->
		die "" __LOC__
	in
	let find_overload a c tl left =
		let map = apply_params a.a_params tl in
		let make op_cf cf e1 e2 tret =
			if cf.cf_expr = None then begin
				if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx "Recursive operator method" p;
				if not (Meta.has Meta.CoreType a.a_meta) then begin
					(* for non core-types we require that the return type is compatible to the native result type *)
					let e' = make {e1 with etype = Abstract.follow_with_abstracts e1.etype} {e1 with etype = Abstract.follow_with_abstracts e2.etype} in
					let t_expected = e'.etype in
					begin try
						unify_raise ctx tret t_expected p
					with Error (Unify _,_) ->
						match follow tret with
							| TAbstract(a,tl) when type_iseq (Abstract.get_underlying_type a tl) t_expected ->
								()
							| _ ->
								let st = s_type (print_context()) in
								error (Printf.sprintf "The result of this operation (%s) is not compatible with declared return type %s" (st t_expected) (st tret)) p
					end;
				end;
				let e = Texpr.Builder.binop op e1 e2 tret p in
				mk_cast e tret p
			end else begin
				let e = make_static_call ctx c cf map [e1;e2] tret p in
				e
			end
		in
		(* special case for == and !=: if the second type is a monomorph, assume that we want to unify
		   it with the first type to preserve comparison semantics. *)
		let is_eq_op = match op with OpEq | OpNotEq -> true | _ -> false in
		if is_eq_op then begin match follow e1.etype,follow e2.etype with
			| TMono _,_ | _,TMono _ ->
				Type.unify e1.etype e2.etype
			| _ ->
				()
		end;
		let rec loop ol = match ol with
			| (op_cf,cf) :: ol when op_cf <> op && (not is_assign_op || op_cf <> OpAssignOp(op)) ->
				loop ol
			| (op_cf,cf) :: ol ->
				let is_impl = Meta.has Meta.Impl cf.cf_meta in
				begin match follow cf.cf_type with
					| TFun([(_,_,t1);(_,_,t2)],tret) ->
						let check e1 e2 swapped =
							let map_arguments () =
								let monos = Monomorph.spawn_constrained_monos (fun t -> t) cf.cf_params in
								let map t = map (apply_params cf.cf_params monos t) in
								let t1 = map t1 in
								let t2 = map t2 in
								let tret = map tret in
								monos,t1,t2,tret
							in
							let monos,t1,t2,tret = map_arguments() in
							let make e1 e2 = make op_cf cf e1 e2 tret in
							let t1 = if is_impl then Abstract.follow_with_abstracts t1 else t1 in
							let e1,e2 = if left || not left && swapped then begin
								Type.type_eq EqStrict (if is_impl then Abstract.follow_with_abstracts e1.etype else e1.etype) t1;
								e1,AbstractCast.cast_or_unify_raise ctx t2 e2 p
							end else begin
								Type.type_eq EqStrict e2.etype t2;
								AbstractCast.cast_or_unify_raise ctx t1 e1 p,e2
							end in
							let check_null e t = if is_eq_op then match e.eexpr with
								| TConst TNull when not (is_explicit_null t) -> raise (Unify_error [])
								| _ -> ()
							in
							(* If either expression is `null` we only allow operator resolving if the argument type
							   is explicitly Null<T> (issue #3376) *)
							if is_eq_op then begin
								check_null e2 t2;
								check_null e1 t1;
							end;
							let e = if not swapped then
								make e1 e2
							else if not (OptimizerTexpr.has_side_effect e1) && not (OptimizerTexpr.has_side_effect e2) then
								make e1 e2
							else
								let v1,v2 = gen_local ctx t1 e1.epos, gen_local ctx t2 e2.epos in
								let ev1,ev2 = mk (TVar(v1,Some e1)) ctx.t.tvoid p,mk (TVar(v2,Some e2)) ctx.t.tvoid p in
								let eloc1,eloc2 = mk (TLocal v1) v1.v_type p,mk (TLocal v2) v2.v_type p in
								let e = make eloc1 eloc2 in
								let e = mk (TBlock [
									ev2;
									ev1;
									e
								]) e.etype e.epos in
								e
							in
							if is_assign_op && op_cf = op then (mk (TMeta((Meta.RequiresAssign,[],p),e)) e.etype e.epos)
							else e
						in
						begin try
							check e1 e2 false
						with Error (Unify _,_) | Unify_error _ -> try
							if not (Meta.has Meta.Commutative cf.cf_meta) then raise Not_found;
							check e2 e1 true
						with Not_found | Error (Unify _,_) | Unify_error _ ->
							loop ol
						end
					| _ ->
						die "" __LOC__
				end
			| [] ->
				raise Not_found
		in
		if left then
			loop a.a_ops
		else
			let not_impl_or_is_commutative (_, cf) =
				not (Meta.has Meta.Impl cf.cf_meta) || Meta.has Meta.Commutative cf.cf_meta
			in
			loop (List.filter not_impl_or_is_commutative a.a_ops)
	in
	try
		begin match follow e1.etype with
			| TAbstract({a_impl = Some c} as a,tl) -> find_overload a c tl true
			| _ -> raise Not_found
		end
	with Not_found -> try
		begin match follow e2.etype with
			| TAbstract({a_impl = Some c} as a,tl) -> find_overload a c tl false
			| _ -> raise Not_found
		end
	with Not_found ->
		if abstract_overload_only then raise Not_found
		else make e1 e2

and type_unop ctx op flag e p =
	let set = (op = Increment || op = Decrement) in
	let acc = type_access ctx (fst e) (snd e) (if set then (MSet None) else MGet) WithType.value (* WITHTYPETODO *) in
	let access e =
		let make e =
			let t = (match op with
			| Not ->
				if flag = Postfix then error "Postfix ! is not supported" p;
				unify ctx e.etype ctx.t.tbool e.epos;
				ctx.t.tbool
			| NegBits ->
				unify ctx e.etype ctx.t.tint e.epos;
				ctx.t.tint
			| Increment
			| Decrement
			| Neg ->
				if set then check_assign ctx e;
				(match classify e.etype with
				| KFloat -> ctx.t.tfloat
				| KNumParam t ->
					unify ctx e.etype ctx.t.tfloat e.epos;
					t
				| k ->
					if unify_int ctx e k then ctx.t.tint else ctx.t.tfloat)
			) in
			mk (TUnop (op,flag,e)) t p
		in
		try (match follow e.etype with
			| TAbstract ({a_impl = Some c} as a,pl) ->
				let rec loop opl = match opl with
					| [] -> raise Not_found
					| (op2,flag2,cf) :: opl when op == op2 && flag == flag2 ->
						let m = spawn_monomorph ctx p in
						let tcf = apply_params a.a_params pl (monomorphs cf.cf_params cf.cf_type) in
						if Meta.has Meta.Impl cf.cf_meta then begin
							if type_iseq (tfun [apply_params a.a_params pl a.a_this] m) tcf then cf,tcf,m else loop opl
						end else
							if type_iseq (tfun [e.etype] m) tcf then cf,tcf,m else loop opl
					| _ :: opl -> loop opl
				in
				let cf,t,r = try loop a.a_unops with Not_found -> raise Not_found in
				(match cf.cf_expr with
				| None ->
					let e = {e with etype = apply_params a.a_params pl a.a_this} in
					let e = mk (TUnop(op,flag,e)) r p in
					(* unify ctx r e.etype p; *) (* TODO: I'm not sure why this was here (related to #2295) *)
					e
				| Some _ ->
					let et = type_module_type ctx (TClassDecl c) None p in
					let ef = mk (TField (et,FStatic (c,cf))) t p in
					make_call ctx ef [e] r p)
			| _ -> raise Not_found
		) with Not_found ->
			make e
	in
	let rec loop acc =
		match acc with
		| AKExpr e -> access e
		| AKInline _ | AKUsing _ when not set -> access (acc_get ctx acc p)
		| AKNo s ->
			error ("The field or identifier " ^ s ^ " is not accessible for " ^ (if set then "writing" else "reading")) p
		| AKAccess(a,tl,c,ebase,ekey) ->
			begin try
				(match op with Increment | Decrement -> () | _ -> raise Not_found);
				let v_key = alloc_var VGenerated "tmp" ekey.etype ekey.epos in
				let evar_key = mk (TVar(v_key,Some ekey)) ctx.com.basic.tvoid ekey.epos in
				let ekey = mk (TLocal v_key) ekey.etype ekey.epos in
				(* get *)
				let e_get = mk_array_get_call ctx (AbstractCast.find_array_access_raise ctx a tl ekey None p) c ebase p in
				let v_get = alloc_var VGenerated "tmp" e_get.etype e_get.epos in
				let ev_get = mk (TLocal v_get) v_get.v_type p in
				let evar_get = mk (TVar(v_get,Some e_get)) ctx.com.basic.tvoid p in
				(* op *)
				let e_one = mk (TConst (TInt (Int32.of_int 1))) ctx.com.basic.tint p in
				let e_op = mk (TBinop((if op = Increment then OpAdd else OpSub),ev_get,e_one)) ev_get.etype p in
				(* set *)
				let e_set = mk_array_set_call ctx (AbstractCast.find_array_access_raise ctx a tl ekey (Some e_op) p) c ebase p in
				let el = evar_key :: evar_get :: e_set :: (if flag = Postfix then [ev_get] else []) in
				mk (TBlock el) e_set.etype p
			with Not_found ->
				let e = mk_array_get_call ctx (AbstractCast.find_array_access ctx a tl ekey None p) c ebase p in
				loop (AKExpr e)
			end
		| AKUsing (emethod,cl,cf,etarget,force_inline) when (op = Decrement || op = Increment) && has_meta Meta.Impl cf.cf_meta ->
			let l = save_locals ctx in
			let init_tmp,etarget,eget =
				match needs_temp_var etarget, fst e with
				| true, EField (_, field_name) ->
					let tmp = gen_local ctx etarget.etype p in
					let tmp_ident = (EConst (Ident tmp.v_name), p) in
					(
						mk (TVar (tmp, Some etarget)) ctx.t.tvoid p,
						mk (TLocal tmp) tmp.v_type p,
						(EField (tmp_ident,field_name), p)
					)
				| _ -> (mk (TBlock []) ctx.t.tvoid p, etarget, e)
			in
			let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> die "" __LOC__) in
			let one = (EConst (Int "1"),p) in
			(match follow cf.cf_type with
			| TFun (_, t) ->
				(match flag with
				| Prefix ->
					let get = type_binop ctx op eget one false WithType.value p in
					unify ctx get.etype t p;
					l();
					let call_setter = make_call ctx emethod [etarget; get] t ~force_inline p in
					mk (TBlock [init_tmp; call_setter]) t p
				| Postfix ->
					let get = type_expr ctx eget WithType.value in
					let tmp_value = gen_local ctx t p in
					let plusone = type_binop ctx op (EConst (Ident tmp_value.v_name),p) one false WithType.value p in
					unify ctx get.etype t p;
					l();
					mk (TBlock [
						init_tmp;
						mk (TVar (tmp_value,Some get)) ctx.t.tvoid p;
						make_call ctx emethod [etarget; plusone] t ~force_inline p;
						mk (TLocal tmp_value) t p;
					]) t p
				)
			| _ ->
				l();
				die "" __LOC__
			)
		| AKInline _ | AKUsing _ | AKMacro _ ->
			error "This kind of operation is not supported" p
		| AKFieldSet _ ->
			error "Invalid operation" p
		| AKSet (e,t,cf) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype p in
			let ev = mk (TLocal v) e.etype p in
			let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> die "" __LOC__) in
			let one = (EConst (Int "1"),p) in
			let eget = (EField ((EConst (Ident v.v_name),p),cf.cf_name),p) in
			match flag with
			| Prefix ->
				let get = type_binop ctx op eget one false WithType.value p in
				unify ctx get.etype t p;
				l();
				mk (TBlock [
					mk (TVar (v,Some e)) ctx.t.tvoid p;
					make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [get] t p
				]) t p
			| Postfix ->
				let v2 = gen_local ctx t p in
				let ev2 = mk (TLocal v2) t p in
				let get = type_expr ctx eget WithType.value in
				let plusone = type_binop ctx op (EConst (Ident v2.v_name),p) one false WithType.value p in
				unify ctx get.etype t p;
				l();
				mk (TBlock [
					mk (TVar (v,Some e)) ctx.t.tvoid p;
					mk (TVar (v2,Some get)) ctx.t.tvoid p;
					make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [plusone.etype] t) p) [plusone] t p;
					ev2
				]) t p
	in
	loop acc

and type_ident ctx i p mode with_type =
	try
		type_ident_raise ctx i p mode with_type
	with Not_found -> try
		(* lookup type *)
		if is_lower_ident i p then raise Not_found;
		let e = (try type_type ctx ([],i) p with Error (Module_not_found ([],name),_) when name = i -> raise Not_found) in
		AKExpr e
	with Not_found ->
		let resolved_to_type_parameter = ref false in
		try
			let t = List.find (fun (i2,_) -> i2 = i) ctx.type_params in
			resolved_to_type_parameter := true;
			let c = match follow (snd t) with TInst(c,_) -> c | _ -> die "" __LOC__ in
			if TypeloadCheck.is_generic_parameter ctx c && Meta.has Meta.Const c.cl_meta then begin
				let e = type_module_type ctx (TClassDecl c) None p in
				AKExpr {e with etype = (snd t)}
			end else
				raise Not_found
		with Not_found ->
			if ctx.untyped then begin
				if i = "__this__" then
					AKExpr (mk (TConst TThis) ctx.tthis p)
				else
					let t = mk_mono() in
					AKExpr ((mk (TIdent i)) t p)
			end else begin
				if ctx.curfun = FunStatic && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
				if !resolved_to_type_parameter then begin
					display_error ctx ("Only @:const type parameters on @:generic classes can be used as value") p;
					AKExpr (mk (TConst TNull) t_dynamic p)
				end else begin
					let err = Unknown_ident i in
					if ctx.in_display then begin
						raise (Error (err,p))
					end;
					match ctx.com.display.dms_kind with
						| DMNone ->
							raise (Error(err,p))
						| DMDiagnostics _ ->
							DisplayToplevel.handle_unresolved_identifier ctx i p false;
							DisplayFields.handle_missing_ident ctx i mode with_type p;
							let t = mk_mono() in
							AKExpr (mk (TIdent i) t p)
						| _ ->
							display_error ctx (error_msg err) p;
							let t = mk_mono() in
							(* Add a fake local for #8751. *)
							if !ServerConfig.legacy_completion then
								ignore(add_local ctx VGenerated i t p);
							AKExpr (mk (TIdent i) t p)
				end
			end

and handle_efield ctx e p0 mode with_type =
	let open TyperDotPath in

	let dot_path first pnext =
		let name,_,p = first in
		try
			(* first, try to resolve the first ident in the chain and access its fields.
			   this doesn't support untyped identifiers yet, because we want to check fully-qualified
			   paths first (even in an untyped block) *)
			field_chain ctx pnext (type_ident_raise ctx name p)
		with Not_found ->
			(* first ident couldn't be resolved, it's probably a fully qualified path - resolve it *)
			let path = (first :: pnext) in
			try
				resolve_dot_path ctx path
			with Not_found ->
				(* dot-path resolution failed, it could be an untyped field access that happens to look like a dot-path, e.g. `untyped __global__.String` *)
				try
					(* TODO: we don't really want to do full type_ident again, just the second part of it *)
					field_chain ctx pnext (type_ident ctx name p)
				with Error (Unknown_ident _,p2) as e when p = p2 ->
					try
						(* try raising a more sensible error if there was an uppercase-first (module name) part *)
						begin
							(* TODO: we should pass the actual resolution error from resolve_dot_path instead of Not_found *)
							let rec loop pack_acc first_uppercase path =
								match path with
								| (name,PLowercase,_) :: rest ->
									(match first_uppercase with
									| None -> loop (name :: pack_acc) None rest
									| Some (n,p) -> List.rev pack_acc, n, None, p)
								| (name,PUppercase,p) :: rest ->
									(match first_uppercase with
									| None -> loop pack_acc (Some (name,p)) rest
									| Some (n,_) -> List.rev pack_acc, n, Some name, p)
								| [] ->
									(match first_uppercase with
									| None -> raise Not_found
									| Some (n,p) -> List.rev pack_acc, n, None, p)
							in
							let pack,name,sub,p = loop [] None path in
							let mpath = (pack,name) in
							if Hashtbl.mem ctx.g.modules mpath then
								let tname = Option.default name sub in
								raise (Error (Type_not_found (mpath,tname,Not_defined),p))
							else
								raise (Error (Module_not_found mpath,p))
						end
					with Not_found ->
						(* if there was no module name part, last guess is that we're trying to get package completion *)
						if ctx.in_display then begin
							let sl = List.map (fun (n,_,_) -> n) path in
							if is_legacy_completion ctx.com then
								raise (Parser.TypePath (sl,None,false,p))
							else
								DisplayToplevel.collect_and_raise ctx TKType WithType.no_value (CRToplevel None) (String.concat "." sl,p0) p0
						end;
						raise e
	in

	(* loop through the given EField expression to figure out whether it's a dot-path that we have to resolve,
	   or a simple field access chain *)
	let rec loop dot_path_acc (e,p) =
		match e with
		| EField (e,s) ->
			(* field access - accumulate and check further *)
			loop ((mk_dot_path_part s p) :: dot_path_acc) e
		| EConst (Ident i) ->
			(* it's a dot-path, so it might be either fully-qualified access (pack.Class.field)
			   or normal field access of a local/global/field identifier, proceed figuring this out *)
			dot_path (mk_dot_path_part i p) dot_path_acc
		| _ ->
			(* non-ident expr occured: definitely NOT a fully-qualified access,
			   resolve the field chain against this expression *)
			let e = type_access ctx e p in
			field_chain ctx dot_path_acc e
	in
	loop [] (e,p0) mode with_type

and type_access ctx e p mode with_type =
	match e with
	| EConst (Ident s) ->
		type_ident ctx s p mode with_type
	| EField (e1,"new") ->
		let e1 = type_expr ctx e1 WithType.value in
		begin match e1.eexpr with
			| TTypeExpr (TClassDecl c) ->
				begin match mode with
				| MSet _ -> error "Cannot set constructor" p;
				| MCall _ -> error ("Cannot call constructor like this, use 'new " ^ (s_type_path c.cl_path) ^ "()' instead") p;
				| MGet -> ()
				end;
				let monos = Monomorph.spawn_constrained_monos (fun t -> t) (match c.cl_kind with KAbstractImpl a -> a.a_params | _ -> c.cl_params) in
				let ct, cf = get_constructor ctx c monos p in
				no_abstract_constructor c p;
				check_constructor_access ctx c cf p;
				let args = match follow ct with TFun(args,ret) -> args | _ -> die "" __LOC__ in
				let vl = List.map (fun (n,_,t) -> alloc_var VGenerated n t c.cl_pos) args in
				let vexpr v = mk (TLocal v) v.v_type p in
				let el = List.map vexpr vl in
				let ec,t = match c.cl_kind with
					| KAbstractImpl a ->
						let e = type_module_type ctx (TClassDecl c) None p in
						let e = mk (TField (e,(FStatic (c,cf)))) ct p in
						let t = TAbstract(a,monos) in
						make_call ctx e el t p,t
					| _ ->
						let t = TInst(c,monos) in
						mk (TNew(c,monos,el)) t p,t
				in
				AKExpr(mk (TFunction {
					tf_args = List.map (fun v -> v,None) vl;
					tf_type = t;
					tf_expr = mk (TReturn (Some ec)) t p;
				}) (TFun ((List.map (fun v -> v.v_name,false,v.v_type) vl),t)) p)
			| _ -> error "Binding new is only allowed on class types" p
		end;
	| EField _ ->
		handle_efield ctx e p mode with_type
	| EArray (e1,e2) ->
		type_array_access ctx e1 e2 p mode
	| EDisplay (e,dk) ->
		let resume_typing = type_expr ~mode in
		AKExpr (TyperDisplay.handle_edisplay ~resume_typing ctx e dk WithType.value)
	| _ ->
		AKExpr (type_expr ~mode ctx (e,p) WithType.value)

and type_array_access ctx e1 e2 p mode =
	let e1 = type_expr ctx e1 WithType.value in
	let e2 = type_expr ctx e2 WithType.value in
	Calls.array_access ctx e1 e2 mode p

and type_vars ctx vl p =
	let vl = List.map (fun ev ->
		let n = fst ev.ev_name
		and pv = snd ev.ev_name in
		DeprecationCheck.check_is ctx.com n ev.ev_meta pv;
		try
			let t = Typeload.load_type_hint ctx p ev.ev_type in
			let e = (match ev.ev_expr with
				| None -> None
				| Some e ->
					let e = type_expr ctx e (WithType.with_type t) in
					let e = AbstractCast.cast_or_unify ctx t e p in
					Some e
			) in
			let v = add_local_with_origin ctx TVOLocalVariable n t pv in
			v.v_meta <- ev.ev_meta;
			if ev.ev_final then add_var_flag v VFinal;
			if ctx.in_display && DisplayPosition.display_position#enclosed_in pv then
				DisplayEmitter.display_variable ctx v pv;
			v,e
		with
			Error (e,p) ->
				check_error ctx e p;
				add_local ctx VGenerated n t_dynamic pv, None (* TODO: What to do with this... *)
	) vl in
	delay ctx PTypeField (fun() ->
		List.iter
			(fun (v,_) ->
				if ExtType.is_void (follow v.v_type) then
					error "Variables of type Void are not allowed" v.v_pos
			)
			vl
	);
	match vl with
	| [v,eo] ->
		mk (TVar (v,eo)) ctx.t.tvoid p
	| _ ->
		let e = mk (TBlock (List.map (fun (v,e) -> (mk (TVar (v,e)) ctx.t.tvoid p)) vl)) ctx.t.tvoid p in
		mk (TMeta((Meta.MergeBlock,[],p), e)) e.etype e.epos

and format_string ctx s p =
	let e = ref None in
	let pmin = ref p.pmin in
	let min = ref (p.pmin + 1) in
	let add_expr (enext,p) len =
		min := !min + len;
		let enext = if ctx.in_display && DisplayPosition.display_position#enclosed_in p then
			Display.ExprPreprocessing.process_expr ctx.com (enext,p)
		else
			enext,p
		in
		match !e with
		| None -> e := Some enext
		| Some prev ->
			e := Some (EBinop (OpAdd,prev,enext),punion (pos prev) p)
	in
	let add enext len =
		let p = { p with pmin = !min; pmax = !min + len } in
		add_expr (enext,p) len
	in
	let add_sub start pos =
		let len = pos - start in
		if len > 0 || !e = None then add (EConst (String (String.sub s start len,SDoubleQuotes))) len
	in
	let len = String.length s in
	let rec parse start pos =
		if pos = len then add_sub start pos else
		let c = String.unsafe_get s pos in
		let pos = pos + 1 in
		if c = '\'' then begin
			incr pmin;
			incr min;
		end;
		if c <> '$' || pos = len then parse start pos else
		match String.unsafe_get s pos with
		| '$' ->
			(* double $ *)
			add_sub start pos;
			parse (pos + 1) (pos + 1)
		| '{' ->
			parse_group start pos '{' '}' "brace"
		| 'a'..'z' | 'A'..'Z' | '_' ->
			add_sub start (pos - 1);
			incr min;
			let rec loop i =
				if i = len then i else
				let c = String.unsafe_get s i in
				match c with
				| 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> loop (i+1)
				| _ -> i
			in
			let iend = loop (pos + 1) in
			let len = iend - pos in
			add (EConst (Ident (String.sub s pos len))) len;
			parse (pos + len) (pos + len)
		| _ ->
			(* keep as-it *)
			parse start pos
	and parse_group start pos gopen gclose gname =
		add_sub start (pos - 1);
		let rec loop groups i =
			if i = len then
				match groups with
				| [] -> die "" __LOC__
				| g :: _ -> error ("Unclosed " ^ gname) { p with pmin = !pmin + g + 1; pmax = !pmin + g + 2 }
			else
				let c = String.unsafe_get s i in
				if c = gopen then
					loop (i :: groups) (i + 1)
				else if c = gclose then begin
					let groups = List.tl groups in
					if groups = [] then i else loop groups (i + 1)
				end else
					loop groups (i + 1)
		in
		let send = loop [pos] (pos + 1) in
		let slen = send - pos - 1 in
		let scode = String.sub s (pos + 1) slen in
		min := !min + 2;
		begin
			let e =
				let ep = { p with pmin = !pmin + pos + 2; pmax = !pmin + send + 1 } in
				let error msg pos =
					if Lexer.string_is_whitespace scode then error "Expression cannot be empty" ep
					else error msg pos
				in
				match ParserEntry.parse_expr_string ctx.com.defines scode ep error true with
					| ParseSuccess(data,_,_) -> data
					| ParseError(_,(msg,p),_) -> error (Parser.error_msg msg) p
			in
			add_expr e slen
		end;
		min := !min + 1;
		parse (send + 1) (send + 1)
	in
	parse 0 0;
	match !e with
	| None -> die "" __LOC__
	| Some e -> e

and type_block ctx el with_type p =
	let merge acc e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), {eexpr = TBlock el}) ->
			List.rev el @ acc
		| _ ->
			e :: acc
	in
	let rec loop acc = function
		| [] -> List.rev acc
		| e :: l ->
			let acc = try merge acc (type_expr ctx e (if l = [] then with_type else WithType.no_value)) with Error (e,p) -> check_error ctx e p; acc in
			loop acc l
	in
	let l = loop [] el in
	let rec loop = function
		| [] -> ctx.t.tvoid
		| [e] -> e.etype
		| _ :: l -> loop l
	in
	mk (TBlock l) (loop l) p

and type_object_decl ctx fl with_type p =
	let dynamic_parameter = ref None in
	let a = (match with_type with
	| WithType.WithType(t,_) ->
		let rec loop seen t =
			match follow t with
			| TAnon a -> ODKWithStructure a
			| TAbstract (a,pl) as t
				when not (Meta.has Meta.CoreType a.a_meta)
					&& not (List.exists (fun t' -> shallow_eq t t') seen) ->
				let froms = get_abstract_froms a pl
				and fold = fun acc t' -> match loop (t :: seen) t' with ODKPlain -> acc | t -> t :: acc in
				(match List.fold_left fold [] froms with
				| [t] -> t
				| _ -> ODKPlain)
			| TDynamic t when (follow t != t_dynamic) ->
				dynamic_parameter := Some t;
				ODKWithStructure {
					a_status = ref Closed;
					a_fields = PMap.empty;
				}
			| TInst(c,tl) when Meta.has Meta.StructInit c.cl_meta ->
				ODKWithClass(c,tl)
			| _ ->
				ODKPlain
		in
		loop [] t
	| _ ->
		ODKPlain
	) in
	let type_fields field_map =
		let fields = ref PMap.empty in
		let extra_fields = ref [] in
		let fl = List.map (fun ((n,pn,qs),e) ->
			let is_valid = Lexer.is_valid_identifier n in
			if PMap.mem n !fields then error ("Duplicate field in object declaration : " ^ n) p;
			let is_final = ref false in
			let e = try
				let t = match !dynamic_parameter with
					| Some t -> t
					| None ->
						let cf = PMap.find n field_map in
						if (has_class_field_flag cf CfFinal) then is_final := true;
						if ctx.in_display && DisplayPosition.display_position#enclosed_in pn then DisplayEmitter.display_field ctx Unknown CFSMember cf pn;
						cf.cf_type
				in
				let e = type_expr ctx e (WithType.with_structure_field t n) in
				let e = AbstractCast.cast_or_unify ctx t e e.epos in
				let e = if is_null t && not (is_null e.etype) then mk (TCast(e,None)) (ctx.t.tnull e.etype) e.epos else e in
				(try type_eq EqStrict e.etype t; e with Unify_error _ -> mk (TCast (e,None)) t e.epos)
			with Not_found ->
				if is_valid then
					extra_fields := n :: !extra_fields;
				type_expr ctx e WithType.value
			in
			if is_valid then begin
				if starts_with n '$' then error "Field names starting with a dollar are not allowed" p;
				let cf = mk_field n e.etype (punion pn e.epos) pn in
				if !is_final then add_class_field_flag cf CfFinal;
				fields := PMap.add n cf !fields;
			end;
			((n,pn,qs),e)
		) fl in
		let t = mk_anon ~fields:!fields (ref Const) in
		if not ctx.untyped then begin
			(match PMap.foldi (fun n cf acc -> if not (Meta.has Meta.Optional cf.cf_meta) && not (PMap.mem n !fields) then n :: acc else acc) field_map [] with
				| [] -> ()
				| [n] -> raise_or_display ctx [Unify_custom ("Object requires field " ^ n)] p
				| nl -> raise_or_display ctx [Unify_custom ("Object requires fields: " ^ (String.concat ", " nl))] p);
			(match !extra_fields with
			| [] -> ()
			| _ -> raise_or_display ctx (List.map (fun n -> has_extra_field t n) !extra_fields) p);
		end;
		t, fl
	in
	let type_plain_fields () =
		let rec loop (l,acc) ((f,pf,qs),e) =
			let is_valid = Lexer.is_valid_identifier f in
			if PMap.mem f acc then error ("Duplicate field in object declaration : " ^ f) p;
			let e = type_expr ctx e (WithType.named_structure_field f) in
			(match follow e.etype with TAbstract({a_path=[],"Void"},_) -> error "Fields of type Void are not allowed in structures" e.epos | _ -> ());
			let cf = mk_field f e.etype (punion pf e.epos) pf in
			if ctx.in_display && DisplayPosition.display_position#enclosed_in pf then DisplayEmitter.display_field ctx Unknown CFSMember cf pf;
			(((f,pf,qs),e) :: l, if is_valid then begin
				if starts_with f '$' then error "Field names starting with a dollar are not allowed" p;
				PMap.add f cf acc
			end else acc)
		in
		let fields , types = List.fold_left loop ([],PMap.empty) fl in
		let x = ref Const in
		ctx.opened <- x :: ctx.opened;
		mk (TObjectDecl (List.rev fields)) (mk_anon ~fields:types x) p
	in
	(match a with
	| ODKPlain -> type_plain_fields()
	| ODKWithStructure a when PMap.is_empty a.a_fields && !dynamic_parameter = None -> type_plain_fields()
	| ODKWithStructure a ->
		let t, fl = type_fields a.a_fields in
		mk (TObjectDecl fl) t p
	| ODKWithClass (c,tl) ->
		let t,ctor = get_constructor ctx c tl p in
		let args = match follow t with
			| TFun(args,_) -> args
			| _ -> die "" __LOC__
		in
		let fields = List.fold_left (fun acc (n,opt,t) ->
			let f = mk_field n t ctor.cf_pos ctor.cf_name_pos in
			if opt then f.cf_meta <- [(Meta.Optional,[],ctor.cf_pos)];
			PMap.add n f acc
		) PMap.empty args in
		let t,fl = type_fields fields in
		let evars,fl,_ = List.fold_left (fun (evars,elocs,had_side_effect) (s,e) ->
			begin match e.eexpr with
			| TConst _ | TTypeExpr _ | TFunction _ ->
				evars,(s,e) :: elocs,had_side_effect
			| _ ->
				if had_side_effect then begin
					let v = gen_local ctx e.etype e.epos in
					let ev = mk (TVar(v,Some e)) e.etype e.epos in
					let eloc = mk (TLocal v) v.v_type e.epos in
					(ev :: evars),((s,eloc) :: elocs),had_side_effect
				end else
					evars,(s,e) :: elocs,OptimizerTexpr.has_side_effect e
			end
		) ([],[],false) (List.rev fl) in
		let el = List.map (fun (n,_,t) ->
			try Expr.field_assoc n fl
			with Not_found ->
				try
					match ctor.cf_expr with
					| Some { eexpr = TFunction fn } ->
						Option.get (snd (List.find (fun (v,e) -> n = v.v_name && Option.is_some e) fn.tf_args))
					| _ ->
						raise Not_found
				with Not_found | Option.No_value ->
					let t =
						if type_has_meta (Abstract.follow_with_abstracts_without_null t) Meta.NotNull then ctx.t.tnull t
						else t
					in
					mk (TConst TNull) t p
		) args in
		let e = mk (TNew(c,tl,el)) (TInst(c,tl)) p in
		mk (TBlock (List.rev (e :: (List.rev evars)))) e.etype e.epos
	)

and type_new ctx path el with_type force_inline p =
	let path =
		if snd path <> null_pos then
			path
		(*
			Since macros don't have placed_type_path structure on Haxe side any ENew will have null_pos in `path`.
			Try to calculate a better pos.
		*)
		else begin
			match el with
			| (_,p1) :: _ when p1.pfile = p.pfile && p.pmin < p1.pmin ->
				let pmin = p.pmin + (String.length "new ")
				and pmax = p1.pmin - 2 (* Additional "1" for an opening bracket *)
				in
				fst path, { p with
					pmin = if pmin < pmax then pmin else p.pmin;
					pmax = pmax;
				}
			| _ -> fst path, p
		end
	in
	let unify_constructor_call c params f ct = match follow ct with
		| TFun (args,r) ->
			(try
				let fcc = unify_field_call ctx (FInstance(c,params,f)) el args r p false in
				check_constructor_access ctx c fcc.fc_field p;
				List.map fst fcc.fc_args
			with Error (e,p) ->
				display_error ctx (error_msg e) p;
				[])
		| _ ->
			error "Constructor is not a function" p
	in
	let t = if (fst path).tparams <> [] then begin
		try
			Typeload.load_instance ctx path false
		with Error _ as exc when ctx.com.display.dms_display ->
			(* If we fail for some reason, process the arguments in case we want to display them (#7650). *)
			List.iter (fun e -> ignore(type_expr ctx e WithType.value)) el;
			raise exc
	end else try
		ctx.call_argument_stack <- el :: ctx.call_argument_stack;
		let t = Typeload.load_instance ctx path true in
		let t_follow = follow t in
		ctx.call_argument_stack <- List.tl ctx.call_argument_stack;
		(* Try to properly build @:generic classes here (issue #2016) *)
		begin match t_follow with
			| TInst({cl_kind = KGeneric } as c,tl) -> follow (Generic.build_generic ctx c p tl)
			| _ -> t
		end
	with
	| Generic.Generic_Exception _ ->
		(* Try to infer generic parameters from the argument list (issue #2044) *)
		begin match resolve_typedef (Typeload.load_type_def ctx p (fst path)) with
		| TClassDecl ({cl_constructor = Some cf} as c) ->
			let monos = Monomorph.spawn_constrained_monos (fun t -> t) c.cl_params in
			let ct, f = get_constructor ctx c monos p in
			no_abstract_constructor c p;
			ignore (unify_constructor_call c monos f ct);
			begin try
				Generic.build_generic ctx c p monos
			with Generic.Generic_Exception _ as exc ->
				(* If we have an expected type, just use that (issue #3804) *)
				begin match with_type with
					| WithType.WithType(t,_) ->
						begin match follow t with
							| TMono _ -> raise exc
							| t -> t
						end
					| _ ->
						raise exc
				end
			end
		| mt ->
			error ((s_type_path (t_infos mt).mt_path) ^ " cannot be constructed") p
		end
	| Error _ as exc when ctx.com.display.dms_display ->
		List.iter (fun e -> ignore(type_expr ctx e WithType.value)) el;
		raise exc
	in
	DisplayEmitter.check_display_type ctx t path;
	let t = follow t in
	let build_constructor_call c tl =
		let ct, f = get_constructor ctx c tl p in
		no_abstract_constructor c p;
		(match f.cf_kind with
		| Var { v_read = AccRequire (r,msg) } -> (match msg with Some msg -> error msg p | None -> error_require r p)
		| _ -> ());
		let el = unify_constructor_call c tl f ct in
		el,f,ct
	in
	try begin match t with
	| TInst ({cl_kind = KTypeParameter tl} as c,params) ->
		if not (TypeloadCheck.is_generic_parameter ctx c) then error "Only generic type parameters can be constructed" p;
 		begin match get_constructible_constraint ctx tl p with
		| None ->
			raise_error (No_constructor (TClassDecl c)) p
		| Some(tl,tr) ->
			let el,_ = unify_call_args ctx el tl tr p false false in
			mk (TNew (c,params,el)) t p
		end
	| TAbstract({a_impl = Some c} as a,tl) when not (Meta.has Meta.MultiType a.a_meta) ->
		let el,cf,ct = build_constructor_call c tl in
		let ta = mk_anon ~fields:c.cl_statics (ref (Statics c)) in
		let e = mk (TTypeExpr (TClassDecl c)) ta p in
		let e = mk (TField (e,(FStatic (c,cf)))) ct p in
		make_call ctx e el t ~force_inline p
	| TInst (c,params) | TAbstract({a_impl = Some c},params) ->
		let el,_,_ = build_constructor_call c params in
		mk (TNew (c,params,el)) t p
	| _ ->
		error (s_type (print_context()) t ^ " cannot be constructed") p
	end with Error(No_constructor _ as err,p) when ctx.com.display.dms_kind <> DMNone ->
		display_error ctx (error_msg err) p;
		Diagnostics.secure_generated_code ctx (mk (TConst TNull) t p)

and type_try ctx e1 catches with_type p =
	let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
	let rec check_unreachable cases t p = match cases with
		| (v,e) :: cases ->
			let unreachable () =
				display_error ctx "This block is unreachable" p;
				let st = s_type (print_context()) in
				display_error ctx (Printf.sprintf "%s can be caught to %s, which is handled here" (st t) (st v.v_type)) e.epos
			in
			begin try
				begin match follow t,follow v.v_type with
					| _, TDynamic _
					| _, TInst({ cl_path = ["haxe"],"Error"},_) ->
						unreachable()
					| _, TInst({ cl_path = path },_) when path = ctx.com.config.pf_exceptions.ec_wildcard_catch ->
						unreachable()
					| TDynamic _,_ ->
						()
					| _ ->
						Type.unify t v.v_type;
						unreachable()
				end
			with Unify_error _ ->
				check_unreachable cases t p
			end
		| [] ->
			()
	in
	let check_catch_type_params params p =
		List.iter (fun pt ->
			if Abstract.follow_with_abstracts pt != t_dynamic then error "Catch class parameter must be Dynamic" p;
		) params
	in
	let catches,el = List.fold_left (fun (acc1,acc2) ((v,pv),t,e_ast,pc) ->
		let th = Option.default (CTPath { tpackage = ["haxe"]; tname = "Exception"; tsub = None; tparams = [] },null_pos) t in
		let t = Typeload.load_complex_type ctx true th in
		let rec loop t = match follow t with
			| TInst ({ cl_kind = KTypeParameter _} as c,_) when not (TypeloadCheck.is_generic_parameter ctx c) ->
				error "Cannot catch non-generic type parameter" p
			| TInst (_,params) | TEnum (_,params) ->
				check_catch_type_params params (snd th);
				t
			| TAbstract(a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
				check_catch_type_params params (snd th);
				t
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				loop (Abstract.get_underlying_type a tl)
			| TDynamic _ -> t
			| _ -> error "Catch type must be a class, an enum or Dynamic" (pos e_ast)
		in
		let t2 = loop t in
		check_unreachable acc1 t2 (pos e_ast);
		let locals = save_locals ctx in
		let v = add_local_with_origin ctx TVOCatchVariable v t pv in
		if ctx.is_display_file && DisplayPosition.display_position#enclosed_in pv then
			DisplayEmitter.display_variable ctx v pv;
		let e = type_expr ctx e_ast with_type in
		(* If the catch position is the display position it means we get completion on the catch keyword or some
		   punctuation. Otherwise we wouldn't reach this point. *)
		if ctx.is_display_file && DisplayPosition.display_position#enclosed_in pc then ignore(TyperDisplay.display_expr ctx e_ast e DKMarked with_type pc);
		v.v_type <- t2;
		locals();
		((v,e) :: acc1),(e :: acc2)
	) ([],[e1]) catches in
	let e1,catches,t = match with_type with
		| WithType.NoValue -> e1,catches,ctx.t.tvoid
		| WithType.Value _ -> e1,catches,unify_min ctx el
		| WithType.WithType(t,src) when (match follow t with TMono _ -> true | t -> ExtType.is_void t) ->
			e1,catches,unify_min_for_type_source ctx el src
		| WithType.WithType(t,_) ->
			let e1 = AbstractCast.cast_or_unify ctx t e1 e1.epos in
			let catches = List.map (fun (v,e) ->
				v,AbstractCast.cast_or_unify ctx t e e.epos
			) catches in
			e1,catches,t
	in
	mk (TTry (e1,List.rev catches)) t p

and type_map_declaration ctx e1 el with_type p =
	let (tkey,tval,has_type) =
		let get_map_params t = match follow t with
			| TAbstract({a_path=["haxe";"ds"],"Map"},[tk;tv]) -> tk,tv,true
			| TInst({cl_path=["haxe";"ds"],"IntMap"},[tv]) -> ctx.t.tint,tv,true
			| TInst({cl_path=["haxe";"ds"],"StringMap"},[tv]) -> ctx.t.tstring,tv,true
			| TInst({cl_path=["haxe";"ds"],("ObjectMap" | "EnumValueMap")},[tk;tv]) -> tk,tv,true
			| _ -> spawn_monomorph ctx p,spawn_monomorph ctx p,false
		in
		match with_type with
		| WithType.WithType(t,_) -> get_map_params t
		| _ -> (spawn_monomorph ctx p,spawn_monomorph ctx p,false)
	in
	let keys = Hashtbl.create 0 in
	let check_key e_key =
		try
			let p = Hashtbl.find keys e_key.eexpr in
			display_error ctx "Duplicate key" e_key.epos;
			error (compl_msg "Previously defined here") p
		with Not_found ->
			begin match e_key.eexpr with
			| TConst _ -> Hashtbl.add keys e_key.eexpr e_key.epos;
			| _ -> ()
			end
	in
	let el = e1 :: el in
	let el_kv = List.map (fun e -> match fst e with
		| EBinop(OpArrow,e1,e2) -> e1,e2
		| EDisplay _ ->
			ignore(type_expr ctx e (WithType.with_type tkey));
			error "Expected a => b" (pos e)
		| _ -> error "Expected a => b" (pos e)
	) el in
	let el_k,el_v,tkey,tval = if has_type then begin
		let el_k,el_v = List.fold_left (fun (el_k,el_v) (e1,e2) ->
			let e1 = type_expr ctx e1 (WithType.with_type tkey) in
			check_key e1;
			let e1 = AbstractCast.cast_or_unify ctx tkey e1 e1.epos in
			let e2 = type_expr ctx e2 (WithType.with_type tval) in
			let e2 = AbstractCast.cast_or_unify ctx tval e2 e2.epos in
			(e1 :: el_k,e2 :: el_v)
		) ([],[]) el_kv in
		el_k,el_v,tkey,tval
	end else begin
		let el_k,el_v = List.fold_left (fun (el_k,el_v) (e1,e2) ->
			let e1 = type_expr ctx e1 WithType.value in
			check_key e1;
			let e2 = type_expr ctx e2 WithType.value in
			(e1 :: el_k,e2 :: el_v)
		) ([],[]) el_kv in
		let unify_min_resume el = try
			unify_min_raise ctx el
		with Error (Unify l,p) when ctx.in_call_args ->
			 raise (WithTypeError(Unify l,p))
		in
		let tkey = unify_min_resume el_k in
		let tval = unify_min_resume el_v in
		el_k,el_v,tkey,tval
	end in
	let m = TypeloadModule.load_module ctx (["haxe";"ds"],"Map") null_pos in
	let a,c = match m.m_types with
		| (TAbstractDecl ({a_impl = Some c} as a)) :: _ -> a,c
		| _ -> die "" __LOC__
	in
	let tmap = TAbstract(a,[tkey;tval]) in
	let cf = PMap.find "set" c.cl_statics in
	let v = gen_local ctx tmap p in
	let ev = mk (TLocal v) tmap p in
	let ec = type_module_type ctx (TClassDecl c) None p in
	let ef = mk (TField(ec,FStatic(c,cf))) (tfun [tkey;tval] ctx.t.tvoid) p in
	let el = ev :: List.map2 (fun e1 e2 -> (make_call ctx ef [ev;e1;e2] ctx.com.basic.tvoid p)) el_k el_v in
	let enew = mk (TNew(c,[tkey;tval],[])) tmap p in
	let el = (mk (TVar (v,Some enew)) t_dynamic p) :: (List.rev el) in
	mk (TBlock el) tmap p

and type_local_function ctx kind f with_type p =
	let name,inline = match kind with FKNamed (name,inline) -> Some name,inline | _ -> None,false in
	let params = TypeloadFunction.type_function_params ctx f (match name with None -> "localfun" | Some (n,_) -> n) p in
	if params <> [] then begin
		if name = None then display_error ctx "Type parameters not supported in unnamed local functions" p;
		if with_type <> WithType.NoValue then error "Type parameters are not supported for rvalue functions" p
	end;
	let v,pname = (match name with
		| None -> None,p
		| Some (v,pn) -> Some v,pn
	) in
	let old_tp,old_in_loop = ctx.type_params,ctx.in_loop in
	ctx.type_params <- params @ ctx.type_params;
	if not inline then ctx.in_loop <- false;
	let rt = Typeload.load_type_hint ctx p f.f_type in
	let args = List.map (fun ((s,_),opt,_,t,c) ->
		let t = Typeload.load_type_hint ctx p t in
		let t, c = TypeloadFunction.type_function_arg ctx t c opt p in
		s, c, t
	) f.f_args in
	(match with_type with
	| WithType.WithType(t,_) ->
		let rec loop t =
			(match follow t with
			| TFun (args2,tr) when List.length args2 = List.length args ->
				List.iter2 (fun (_,_,t1) (_,_,t2) ->
					match follow t1 with
					| TMono _ -> unify ctx t2 t1 p
					| _ -> ()
				) args args2;
				(* unify for top-down inference unless we are expecting Void *)
				begin
					match follow tr,follow rt with
					| TAbstract({a_path = [],"Void"},_),_ when kind <> FKArrow -> ()
					| _,TMono _ -> unify ctx rt tr p
					| _ -> ()
				end
			| TAbstract(a,tl) ->
				loop (Abstract.get_underlying_type a tl)
			| _ -> ())
		in
		loop t
	| WithType.NoValue ->
		if name = None then display_error ctx "Unnamed lvalue functions are not supported" p
	| _ ->
		());
	let ft = TFun (fun_args args,rt) in
	let v = (match v with
		| None -> None
		| Some v ->
			let v = (add_local_with_origin ctx TVOLocalFunction v ft pname) in
			if params <> [] then v.v_extra <- Some (var_extra params None);
			Some v
	) in
	let curfun = match ctx.curfun with
		| FunStatic -> FunStatic
		| FunMemberAbstract
		| FunMemberAbstractLocal -> FunMemberAbstractLocal
		| _ -> FunMemberClassLocal
	in
	let fargs = TypeloadFunction.convert_fargs f in
	let e , fargs = TypeloadFunction.type_function ctx args fargs rt curfun f.f_expr ctx.in_display p in
	ctx.type_params <- old_tp;
	ctx.in_loop <- old_in_loop;
	let tf = {
		tf_args = fargs;
		tf_type = rt;
		tf_expr = e;
	} in
	let e = mk (TFunction tf) ft p in
	match v with
	| None -> e
	| Some v ->
		Typeload.generate_args_meta ctx.com None (fun m -> v.v_meta <- m :: v.v_meta) f.f_args;
		let open LocalUsage in
		if params <> [] || inline then v.v_extra <- Some (var_extra params (if inline then Some e else None));
		if ctx.in_display && DisplayPosition.display_position#enclosed_in v.v_pos then
			DisplayEmitter.display_variable ctx v v.v_pos;
		let rec loop = function
			| LocalUsage.Block f | LocalUsage.Loop f | LocalUsage.Function f -> f loop
			| LocalUsage.Use v2 | LocalUsage.Assign v2 when v == v2 -> raise Exit
			| LocalUsage.Use _ | LocalUsage.Assign _ | LocalUsage.Declare _ -> ()
		in
		let is_rec = (try local_usage loop e; false with Exit -> true) in
		let exprs =
			if with_type <> WithType.NoValue && not inline then [mk (TLocal v) v.v_type p]
			else []
		in
		let exprs =
			if is_rec then begin
				if inline then display_error ctx "Inline function cannot be recursive" e.epos;
				(mk (TVar (v,Some (mk (TConst TNull) ft p))) ctx.t.tvoid p) ::
				(mk (TBinop (OpAssign,mk (TLocal v) ft p,e)) ft p) ::
				exprs
			end else if inline && not ctx.com.display.dms_display then
				(mk (TBlock []) ctx.t.tvoid p) :: exprs (* do not add variable since it will be inlined *)
			else
				(mk (TVar (v,Some e)) ctx.t.tvoid p) :: exprs
		in
		match exprs with
		| [e] -> e
		| _ ->
			let block = mk (TBlock exprs) v.v_type p in
			mk (TMeta ((Meta.MergeBlock, [], null_pos), block)) v.v_type p

and type_array_decl ctx el with_type p =
	let allow_array_dynamic = ref false in
	let tp = (match with_type with
	| WithType.WithType(t,_) ->
		let rec loop seen t =
			(match follow t with
			| TInst ({ cl_path = [],"Array" },[tp]) ->
				(match follow tp with
				| TMono _ -> None
				| _ as t ->
					if t == t_dynamic then allow_array_dynamic := true;
					Some tp)
			| TAnon _ ->
				(try
					Some (get_iterable_param t)
				with Not_found ->
					None)
			| TAbstract (a,pl) as t when not (List.exists (fun t' -> shallow_eq t t') seen) ->
				let types =
					List.fold_left
						(fun acc t' -> match loop (t :: seen) t' with
							| None -> acc
							| Some t -> t :: acc
						)
						[]
						(get_abstract_froms a pl)
				in
				(match types with
				| [t] -> Some t
				| _ -> None)
			| t ->
				if t == t_dynamic then begin
					allow_array_dynamic := true;
					Some t
				end else
					None
			)
		in
		loop [] t
	| _ ->
		None
	) in
	(match tp with
	| None ->
		let el = List.map (fun e -> type_expr ctx e WithType.value) el in
		let t = try
			unify_min_raise ctx el
		with Error (Unify l,p) ->
			if !allow_array_dynamic || ctx.untyped || ctx.com.display.dms_error_policy = EPIgnore then
				t_dynamic
			else begin
				display_error ctx "Arrays of mixed types are only allowed if the type is forced to Array<Dynamic>" p;
				raise (Error (Unify l, p))
			end
		in
		mk (TArrayDecl el) (ctx.t.tarray t) p
	| Some t ->
		let el = List.map (fun e ->
			let e = type_expr ctx e (WithType.with_type t) in
			AbstractCast.cast_or_unify ctx t e p;
		) el in
		mk (TArrayDecl el) (ctx.t.tarray t) p)

and type_array_comprehension ctx e with_type p =
	let v = gen_local ctx (spawn_monomorph ctx p) p in
	let et = ref (EConst(Ident "null"),p) in
	let comprehension_pos = p in
	let rec map_compr (e,p) =
		match e with
		| EFor(it,e2) -> (EFor (it, map_compr e2),p)
		| EWhile(cond,e2,flag) -> (EWhile (cond,map_compr e2,flag),p)
		| EIf (cond,e2,None) -> (EIf (cond,map_compr e2,None),p)
		| EIf (cond,e2,Some e3) -> (EIf (cond,map_compr e2,Some (map_compr e3)),p)
		| EBlock [e] -> (EBlock [map_compr e],p)
		| EBlock el -> begin match List.rev el with
			| e :: el -> (EBlock ((List.rev el) @ [map_compr e]),p)
			| [] -> e,p
			end
		| EParenthesis e2 -> (EParenthesis (map_compr e2),p)
		| EBinop(OpArrow,a,b) ->
			et := (ENew(({tpackage=["haxe";"ds"];tname="Map";tparams=[];tsub=None},null_pos),[]),comprehension_pos);
			(ECall ((EField ((EConst (Ident v.v_name),p),"set"),p),[a;b]),p)
		| _ ->
			et := (EArrayDecl [],comprehension_pos);
			(ECall ((EField ((EConst (Ident v.v_name),p),"push"),p),[(e,p)]),p)
	in
	let e = map_compr e in
	let ea = type_expr ctx !et with_type in
	unify ctx v.v_type ea.etype p;
	let efor = type_expr ctx e WithType.NoValue in
	mk (TBlock [
		mk (TVar (v,Some ea)) ctx.t.tvoid p;
		efor;
		mk (TLocal v) v.v_type p;
	]) v.v_type p

and type_return ?(implicit=false) ctx e with_type p =
	let is_abstract_ctor = ctx.curfun = FunMemberAbstract && ctx.curfield.cf_name = "_new" in
	match e with
	| None when is_abstract_ctor ->
		let e_cast = mk (TCast(get_this ctx p,None)) ctx.ret p in
		mk (TReturn (Some e_cast)) t_dynamic p
	| None ->
		let v = ctx.t.tvoid in
		unify ctx v ctx.ret p;
		let expect_void = match with_type with
			| WithType.WithType(t,_) -> ExtType.is_void (follow t)
			| WithType.Value (Some ImplicitReturn) -> true
			| _ -> false
		in
		mk (TReturn None) (if expect_void then v else t_dynamic) p
	| Some e ->
		if is_abstract_ctor then begin
			match fst e with
			| ECast((EConst(Ident "this"),_),None) -> ()
			| _ -> display_error ctx "Cannot return a value from constructor" p
		end;
		try
			let with_expected_type =
				if implicit then WithType.of_implicit_return ctx.ret
				else WithType.with_type ctx.ret
			in
			let e = type_expr ctx e with_expected_type in
			match follow ctx.ret with
			| TAbstract({a_path=[],"Void"},_) when implicit ->
				e
			| _ ->
				let e = AbstractCast.cast_or_unify ctx ctx.ret e p in
				match follow e.etype with
				| TAbstract({a_path=[],"Void"},_) ->
					begin match (Texpr.skip e).eexpr with
					| TConst TNull -> error "Cannot return `null` from Void-function" p
					| _ -> ()
					end;
					(* if we get a Void expression (e.g. from inlining) we don't want to return it (issue #4323) *)
					mk (TBlock [
						e;
						mk (TReturn None) t_dynamic p
					]) t_dynamic e.epos;
				| _ ->
					mk (TReturn (Some e)) t_dynamic p
		with Error(err,p) ->
			check_error ctx err p;
			(* If we have a bad return, let's generate a return null expression at least. This surpresses various
				follow-up errors that come from the fact that the function no longer has a return expression (issue #6445). *)
			let e_null = mk (TConst TNull) (mk_mono()) p in
			mk (TReturn (Some e_null)) t_dynamic p

and type_cast ctx e t p =
	let tpos = pos t in
	let t = Typeload.load_complex_type ctx true t in
	let check_param pt = match follow pt with
		| TMono _ -> () (* This probably means that Dynamic wasn't bound (issue #4675). *)
		| t when t == t_dynamic -> ()
		| _ -> error "Cast type parameters must be Dynamic" tpos
	in
	let rec loop t = match follow t with
		| TInst (_,params) | TEnum (_,params) ->
			List.iter check_param params;
			(match follow t with
			| TInst (c,_) ->
				(match c.cl_kind with KTypeParameter _ -> error "Can't cast to a type parameter" tpos | _ -> ());
				TClassDecl c
			| TEnum (e,_) -> TEnumDecl e
			| _ -> die "" __LOC__);
		| TAbstract (a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
			List.iter check_param params;
			TAbstractDecl a
		| TAbstract (a,params) ->
			loop (Abstract.get_underlying_type a params)
		| _ ->
			error "Cast type must be a class or an enum" tpos
	in
	let texpr = loop t in
	mk (TCast (type_expr ctx e WithType.value,Some texpr)) t p

and type_if ctx e e1 e2 with_type p =
	let e = type_expr ctx e WithType.value in
	let e = AbstractCast.cast_or_unify ctx ctx.t.tbool e p in
	let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
	(match e2 with
	| None ->
		mk (TIf (e,e1,None)) ctx.t.tvoid p
	| Some e2 ->
		let e2 = type_expr ctx (Expr.ensure_block e2) with_type in
		let e1,e2,t = match with_type with
			| WithType.NoValue -> e1,e2,ctx.t.tvoid
			| WithType.Value _ -> e1,e2,unify_min ctx [e1; e2]
			| WithType.WithType(t,src) when (match follow t with TMono _ -> true | t -> ExtType.is_void t) ->
				e1,e2,unify_min_for_type_source ctx [e1; e2] src
			| WithType.WithType(t,_) ->
				let e1 = AbstractCast.cast_or_unify ctx t e1 e1.epos in
				let e2 = AbstractCast.cast_or_unify ctx t e2 e2.epos in
				e1,e2,t
		in
		mk (TIf (e,e1,Some e2)) t p)

and type_meta ?(mode=MGet) ctx m e1 with_type p =
	if ctx.is_display_file then DisplayEmitter.check_display_metadata ctx [m];
	let old = ctx.meta in
	ctx.meta <- m :: ctx.meta;
	let e () = type_expr ~mode ctx e1 with_type in
	let e = match m with
		| (Meta.ToString,_,_) ->
			let e = e() in
			(match follow e.etype with
				| TAbstract({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics -> call_to_string ctx e
				| _ -> e)
		| (Meta.Markup,_,_) ->
			error "Markup literals must be processed by a macro" p
		| (Meta.This,_,_) ->
			let e = match ctx.this_stack with
				| [] -> error "Cannot type @:this this here" p
				| e :: _ -> e
			in
			let rec loop e = match e.eexpr with
				| TConst TThis -> get_this ctx e.epos
				| _ -> Type.map_expr loop e
			in
			loop e
		| (Meta.Analyzer,_,_) ->
			let e = e() in
			{e with eexpr = TMeta(m,e)}
		| (Meta.MergeBlock,_,_) ->
			begin match fst e1 with
			| EBlock el ->
				let e = type_block ctx el with_type p in
				{e with eexpr = TMeta(m,e)}
			| _ -> e()
			end
		| (Meta.StoredTypedExpr,_,_) ->
			MacroContext.type_stored_expr ctx e1
		| (Meta.NoPrivateAccess,_,_) ->
			ctx.meta <- List.filter (fun(m,_,_) -> m <> Meta.PrivateAccess) ctx.meta;
			e()
		| (Meta.Fixed,_,_) when ctx.com.platform=Cpp ->
			let e = e() in
			{e with eexpr = TMeta(m,e)}
		| (Meta.NullSafety, [(EConst (Ident "Off"), _)],_) ->
			let e = e() in
			{e with eexpr = TMeta(m,e)}
		| (Meta.BypassAccessor,_,p) ->
			let old_counter = ctx.bypass_accessor in
			ctx.bypass_accessor <- old_counter + 1;
			let e = e () in
			(if ctx.bypass_accessor > old_counter then display_error ctx "Field access expression expected after @:bypassAccessor metadata" p);
			e
		| (Meta.Inline,_,_) ->
			begin match fst e1 with
			| ECall(e1,el) ->
				type_call ctx e1 el WithType.value true p
			| ENew (t,el) ->
				let e = type_new ctx t el with_type true p in
				{e with eexpr = TMeta((Meta.Inline,[],null_pos),e)}
			| _ ->
				display_error ctx "Call or function expected after inline keyword" p;
				e();
			end
		| (Meta.ImplicitReturn,_,_) ->
			begin match e1 with
			| (EReturn e, p) -> type_return ~implicit:true ctx e with_type p
			| _ -> e()
			end
		| _ -> e()
	in
	ctx.meta <- old;
	e

and type_call_target ctx e el with_type inline p =
	let e = maybe_type_against_enum ctx (fun () -> type_access ctx (fst e) (snd e) (MCall el) with_type) with_type true p in
	let check_inline cf =
		if (has_class_field_flag cf CfAbstract) then display_error ctx "Cannot force inline on abstract method" p
	in
	if not inline then
		e
	else match e with
		| AKExpr {eexpr = TField(e1,fa); etype = t} ->
			begin match extract_field fa with
			| Some cf ->
				check_inline cf;
				AKInline(e1,cf,fa,t)
			| None -> e
			end;
		| AKUsing(e,c,cf,ef,_) ->
			check_inline cf;
			AKUsing(e,c,cf,ef,true)
		| AKExpr {eexpr = TLocal _} ->
			display_error ctx "Cannot force inline on local functions" p;
			e
		| _ ->
			e

and type_call ?(mode=MGet) ctx e el (with_type:WithType.t) inline p =
	let def () =
		let e = type_call_target ctx e el with_type inline p in
		build_call ~mode ctx e el with_type p;
	in
	match e, el with
	| (EConst (Ident "trace"),p) , e :: el ->
		if Common.defined ctx.com Define.NoTraces then
			null ctx.t.tvoid p
		else
		let mk_to_string_meta e = EMeta((Meta.ToString,[],null_pos),e),pos e in
		let params = (match el with [] -> [] | _ -> [("customParams",null_pos,NoQuotes),(EArrayDecl (List.map mk_to_string_meta el) , p)]) in
		let infos = mk_infos ctx p params in
		if (platform ctx.com Js || platform ctx.com Python) && el = [] && has_dce ctx.com then
			let e = type_expr ctx e WithType.value in
			let infos = type_expr ctx infos WithType.value in
			let e = match follow e.etype with
				| TAbstract({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
					call_to_string ctx e
				| _ ->
					e
			in
			let e_trace = mk (TIdent "`trace") t_dynamic p in
			mk (TCall (e_trace,[e;infos])) ctx.t.tvoid p
		else
			type_expr ctx (ECall ((EField ((EField ((EConst (Ident "haxe"),p),"Log"),p),"trace"),p),[mk_to_string_meta e;infos]),p) WithType.NoValue
	| (EField ((EConst (Ident "super"),_),_),_), _ ->
		(match def() with
			| { eexpr = TCall ({ eexpr = TField (_, FInstance(_, _, { cf_kind = Method MethDynamic; cf_name = name })); epos = p }, _) } as e ->
				ctx.com.error ("Cannot call super." ^ name ^ " since it's a dynamic method") p;
				e
			| e -> e
		)
	| (EField (e,"bind"),p), args ->
		let e = type_expr ctx e WithType.value in
		(match follow e.etype with
			| TFun signature -> type_bind ctx e signature args p
			| _ -> def ())
	| (EConst (Ident "$type"),_) , [e] ->
		let e = type_expr ctx e WithType.value in
		ctx.com.warning (s_type (print_context()) e.etype) e.epos;
		let e = Diagnostics.secure_generated_code ctx e in
		e
	| (EField(e,"match"),p), [epat] ->
		let et = type_expr ctx e WithType.value in
		let rec has_enum_match t = match follow t with
			| TEnum _ -> true
			| TAbstract (a,tl) when (Meta.has Meta.Forward a.a_meta) && not (Meta.has Meta.CoreType a.a_meta) ->
				(match a.a_impl with
					| Some c when (PMap.exists "match" c.cl_statics) && (Meta.has Meta.Impl (PMap.find "match" c.cl_statics).cf_meta) -> false
					| _ -> has_enum_match (Abstract.get_underlying_type ~return_first:true a tl))
			| _ -> false
		in
		if has_enum_match et.etype then
			Matcher.Match.match_expr ctx e [[epat],None,Some (EConst(Ident "true"),p),p] (Some (Some (EConst(Ident "false"),p),p)) (WithType.with_type ctx.t.tbool) true p
		else
			def ()
	| (EConst (Ident "__unprotect__"),_) , [(EConst (String _),_) as e] ->
		let e = type_expr ctx e WithType.value in
		if Common.platform ctx.com Flash then
			let t = tfun [e.etype] e.etype in
			let e_unprotect = mk (TIdent "__unprotect__") t p in
			mk (TCall (e_unprotect,[e])) e.etype e.epos
		else
			e
	| (EDisplay((EConst (Ident "super"),_ as e1),dk),_),_ ->
		TyperDisplay.handle_display ctx (ECall(e1,el),p) dk with_type
	| (EConst (Ident "super"),sp) , el ->
		if ctx.curfun <> FunConstructor then error "Cannot call super constructor outside class constructor" p;
		let el, t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let ct, f = get_constructor ctx c params p in
			if (Meta.has Meta.CompilerGenerated f.cf_meta) then display_error ctx (error_msg (No_constructor (TClassDecl c))) p;
			let el = (match follow ct with
			| TFun (args,r) ->
				let fcc = unify_field_call ctx (FInstance(c,params,f)) el args r p false in
				List.map fst fcc.fc_args
			| _ ->
				error "Constructor is not a function" p
			) in
			el , TInst (c,params)
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) ctx.t.tvoid p
	| _ ->
		def ()

and type_expr ?(mode=MGet) ctx (e,p) (with_type:WithType.t) =
	match e with
	| EField ((EConst (String(s,_)),ps),"code") ->
		if UTF8.length s <> 1 then error "String must be a single UTF8 char" ps;
		mk (TConst (TInt (Int32.of_int (UCharExt.code (UTF8.get s 0))))) ctx.t.tint p
	| EField(_,n) when starts_with n '$' ->
		error "Field names starting with $ are not allowed" p
	| EConst (Ident s) ->
		if s = "super" && with_type <> WithType.NoValue && not ctx.in_display then error "Cannot use super as value" p;
		let e = maybe_type_against_enum ctx (fun () -> type_ident ctx s p mode with_type) with_type false p in
		acc_get ctx e p
	| EField _
	| EArray _ ->
		acc_get ctx (type_access ctx e p mode with_type) p
	| EConst (Regexp (r,opt)) ->
		let str = mk (TConst (TString r)) ctx.t.tstring p in
		let opt = mk (TConst (TString opt)) ctx.t.tstring p in
		let t = Typeload.load_core_type ctx "EReg" in
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> die "" __LOC__),[],[str;opt])) t p
	| EConst (String(s,SSingleQuotes)) when s <> "" ->
		type_expr ctx (format_string ctx s p) with_type
	| EConst c ->
		Texpr.type_constant ctx.com.basic c p
	| EBinop (op,e1,e2) ->
		type_binop ctx op e1 e2 false with_type p
	| EBlock [] when (match with_type with
			| NoValue -> false
			(*
				If expected type is unknown then treat `(...) -> {}` as an empty function
				(just like `function(...) {}`) instead of returning an object.
			*)
			| WithType (t, Some ImplicitReturn) -> not (ExtType.is_mono (follow t))
			| _ -> true
		) ->
		type_expr ctx (EObjectDecl [],p) with_type
	| EBlock l ->
		let locals = save_locals ctx in
		let e = type_block ctx l with_type p in
		locals();
		e
	| EParenthesis e ->
		let e = type_expr ctx e with_type in
		mk (TParenthesis e) e.etype p
	| EObjectDecl fl ->
		type_object_decl ctx fl with_type p
	| EArrayDecl [(EFor _,_) | (EWhile _,_) as e] ->
		type_array_comprehension ctx e with_type p
	| EArrayDecl ((EBinop(OpArrow,_,_),_) as e1 :: el) ->
		type_map_declaration ctx e1 el with_type p
	| EArrayDecl el ->
		begin match with_type with
		| WithType(t,_) ->
			begin match follow t with
			| TAbstract({a_path = (["haxe";"ds"],"Map")},[tk;tv]) ->
				begin match el with
				| [] ->
					type_expr ctx (ENew(({tpackage=["haxe";"ds"];tname="Map";tparams=[];tsub=None},null_pos),[]),p) with_type
				| [(EDisplay _,_) as e1] ->
					(* This must mean we're just typing the first key of a map declaration (issue #9133). *)
					type_expr ctx e1 (WithType.with_type tk)
				| _ ->
					type_array_decl ctx el with_type p
				end
			| _ ->
				type_array_decl ctx el with_type p
			end
		| _ ->
			type_array_decl ctx el with_type p
		end
	| EVars vl ->
		type_vars ctx vl p
	| EFor (it,e2) ->
		ForLoop.type_for_loop ctx TyperDisplay.handle_display it e2 p
	| ETernary (e1,e2,e3) ->
		type_expr ctx (EIf (e1,e2,Some e3),p) with_type
	| EIf (e,e1,e2) ->
		type_if ctx e e1 e2 with_type p
	| EWhile (cond,e,NormalWhile) ->
		let old_loop = ctx.in_loop in
		let cond = type_expr ctx cond WithType.value in
		let cond = AbstractCast.cast_or_unify ctx ctx.t.tbool cond p in
		ctx.in_loop <- true;
		let e = type_expr ctx (Expr.ensure_block e) WithType.NoValue in
		ctx.in_loop <- old_loop;
		mk (TWhile (cond,e,NormalWhile)) ctx.t.tvoid p
	| EWhile (cond,e,DoWhile) ->
		let old_loop = ctx.in_loop in
		ctx.in_loop <- true;
		let e = type_expr ctx (Expr.ensure_block e) WithType.NoValue in
		ctx.in_loop <- old_loop;
		let cond = type_expr ctx cond WithType.value in
		let cond = AbstractCast.cast_or_unify ctx ctx.t.tbool cond cond.epos in
		mk (TWhile (cond,e,DoWhile)) ctx.t.tvoid p
	| ESwitch (e1,cases,def) ->
		let wrap e1 = mk (TMeta((Meta.Ast,[e,p],p),e1)) e1.etype e1.epos in
		let e = Matcher.Match.match_expr ctx e1 cases def with_type false p in
		wrap e
	| EReturn e ->
		if not ctx.in_function then begin
			display_error ctx "Return outside function" p;
			match e with
			| None ->
				Texpr.Builder.make_null t_dynamic p
			| Some e ->
				(* type the return expression to see if there are more errors
				   as well as use its type as if there was no `return`, since
				   that is most likely what was meant *)
				type_expr ctx e WithType.value
		end else
			type_return ctx e with_type p
	| EBreak ->
		if not ctx.in_loop then display_error ctx "Break outside loop" p;
		mk TBreak t_dynamic p
	| EContinue ->
		if not ctx.in_loop then display_error ctx "Continue outside loop" p;
		mk TContinue t_dynamic p
	| ETry (e1,[]) ->
		type_expr ctx e1 with_type
	| ETry (e1,catches) ->
		type_try ctx e1 catches with_type p
	| EThrow e ->
		let e = type_expr ctx e WithType.value in
		mk (TThrow e) (spawn_monomorph ctx p) p
	| ECall (e,el) ->
		type_call ~mode ctx e el with_type false p
	| ENew (t,el) ->
		type_new ctx t el with_type false p
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e p
	| EFunction (kind,f) ->
		type_local_function ctx kind f with_type p
	| EUntyped e ->
		let old = ctx.untyped in
		ctx.untyped <- true;
		if not (Meta.has Meta.HasUntyped ctx.curfield.cf_meta) then ctx.curfield.cf_meta <- (Meta.HasUntyped,[],p) :: ctx.curfield.cf_meta;
		let e = type_expr ctx e with_type in
		ctx.untyped <- old;
		{
			eexpr = e.eexpr;
			etype = mk_mono();
			epos = e.epos;
		}
	| ECast (e,None) ->
		let e = type_expr ctx e WithType.value in
		mk (TCast (e,None)) (spawn_monomorph ctx p) p
	| ECast (e, Some t) ->
		type_cast ctx e t p
	| EDisplay (e,dk) ->
		TyperDisplay.handle_edisplay ctx e dk with_type
	| EDisplayNew t ->
		die "" __LOC__
	| ECheckType (e,t) ->
		let t = Typeload.load_complex_type ctx true t in
		let e = type_expr ctx e (WithType.with_type t) in
		let e = AbstractCast.cast_or_unify ctx t e p in
		if e.etype == t then e else mk (TCast (e,None)) t p
	| EMeta (m,e1) ->
		type_meta ~mode ctx m e1 with_type p

(* ---------------------------------------------------------------------- *)
(* TYPER INITIALIZATION *)

let rec create com =
	let ctx = {
		com = com;
		t = com.basic;
		g = {
			core_api = None;
			macros = None;
			modules = Hashtbl.create 0;
			types_module = Hashtbl.create 0;
			type_patches = Hashtbl.create 0;
			global_metadata = [];
			module_check_policies = [];
			delayed = [];
			debug_delayed = [];
			doinline = com.display.dms_inline && not (Common.defined com Define.NoInline);
			hook_generate = [];
			std = null_module;
			global_using = [];
			complete = false;
			type_hints = [];
			do_inherit = MagicTypes.on_inherit;
			do_create = create;
			do_macro = MacroContext.type_macro;
			do_load_macro = MacroContext.load_macro';
			do_load_module = TypeloadModule.load_module;
			do_load_type_def = Typeload.load_type_def;
			do_optimize = Optimizer.reduce_expression;
			do_build_instance = InstanceBuilder.build_instance;
			do_format_string = format_string;
			do_finalize = Finalization.finalize;
			do_generate = Finalization.generate;
			do_load_core_class = Typeload.load_core_class;
		};
		m = {
			curmod = null_module;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = [];
		};
		is_display_file = false;
		bypass_accessor = 0;
		meta = [];
		this_stack = [];
		with_type_stack = [];
		call_argument_stack = [];
		pass = PBuildModule;
		macro_depth = 0;
		untyped = false;
		curfun = FunStatic;
		in_function = false;
		in_loop = false;
		in_display = false;
		get_build_infos = (fun() -> None);
		in_macro = Common.defined com Define.Macro;
		ret = mk_mono();
		locals = PMap.empty;
		type_params = [];
		curclass = null_class;
		curfield = null_field;
		tthis = mk_mono();
		opened = [];
		vthis = None;
		in_call_args = false;
		monomorphs = {
			perfunction = [];
		};
		on_error = (fun ctx msg p -> ctx.com.error msg p);
		memory_marker = Typecore.memory_marker;
	} in
	ctx.g.std <- (try
		TypeloadModule.load_module ctx ([],"StdTypes") null_pos
	with
		Error (Module_not_found ([],"StdTypes"),_) ->
			try
				let std_path = Sys.getenv "HAXE_STD_PATH" in
				error ("Standard library not found. Please check your `HAXE_STD_PATH` environment variable (current value: \"" ^ std_path ^ "\")") null_pos
			with Not_found ->
				error "Standard library not found. You may need to set your `HAXE_STD_PATH` environment variable" null_pos
	);
	(* We always want core types to be available so we add them as default imports (issue #1904 and #3131). *)
	ctx.m.module_types <- List.map (fun t -> t,null_pos) ctx.g.std.m_types;
	List.iter (fun t ->
		match t with
		| TAbstractDecl a ->
			(match snd a.a_path with
			| "Void" -> ctx.t.tvoid <- TAbstract (a,[]);
			| "Float" -> ctx.t.tfloat <- TAbstract (a,[]);
			| "Int" -> ctx.t.tint <- TAbstract (a,[])
			| "Bool" -> ctx.t.tbool <- TAbstract (a,[])
			| "Dynamic" -> t_dynamic_def := TAbstract(a,List.map snd a.a_params);
			| "Null" ->
				let mk_null t =
					try
						if not (is_null ~no_lazy:true t || is_explicit_null t) then TAbstract (a,[t]) else t
					with Exit ->
						(* don't force lazy evaluation *)
						let r = ref (lazy_available t_dynamic) in
						r := lazy_wait (fun() ->
							let t = (if not (is_null t) then TAbstract (a,[t]) else t) in
							r := lazy_available t;
							t
						);
						TLazy r
				in
				ctx.t.tnull <- mk_null;
			| _ -> ())
		| TEnumDecl _ | TClassDecl _ | TTypeDecl _ ->
			()
	) ctx.g.std.m_types;
	let m = TypeloadModule.load_module ctx ([],"String") null_pos in
	List.iter (fun mt -> match mt with
		| TClassDecl c -> ctx.t.tstring <- TInst (c,[])
		| _ -> ()
	) m.m_types;
	let m = TypeloadModule.load_module ctx ([],"Array") null_pos in
	(try
		List.iter (fun t -> (
			match t with
			| TClassDecl ({cl_path = ([],"Array")} as c) ->
				ctx.t.tarray <- (fun t -> TInst (c,[t]));
				raise Exit
			| _ -> ()
		)) m.m_types;
		die "" __LOC__
	with Exit -> ());
	let m = TypeloadModule.load_module ctx (["haxe"],"EnumTools") null_pos in
	(match m.m_types with
	| [TClassDecl c1;TClassDecl c2] -> ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
	| [TClassDecl c1] ->
		let m = TypeloadModule.load_module ctx (["haxe"],"EnumWithType.valueTools") null_pos in
		(match m.m_types with
		| [TClassDecl c2 ] -> ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
		| _ -> die "" __LOC__);
	| _ -> die "" __LOC__);
	ignore(TypeloadModule.load_module ctx (["haxe"],"Exception") null_pos);
	ctx.g.complete <- true;
	ctx

;;
unify_min_ref := unify_min;
unify_min_for_type_source_ref := unify_min_for_type_source;
make_call_ref := make_call;
build_call_ref := build_call;
type_call_target_ref := type_call_target;
type_block_ref := type_block
