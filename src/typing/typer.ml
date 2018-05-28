(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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
	| TLocal {v_extra = None} | TArray _ | TField _ | TIdent _ ->
		()
	| TConst TThis | TTypeExpr _ when ctx.untyped ->
		()
	| _ ->
		error "Invalid assign" e.epos

type type_class =
	| KInt
	| KFloat
	| KString
	| KUnk
	| KDyn
	| KOther
	| KParam of t
	| KAbstract of tabstract * t list

let rec classify t =
	match follow t with
	| TInst ({ cl_path = ([],"String") },[]) -> KString
	| TAbstract({a_impl = Some _} as a,tl) -> KAbstract (a,tl)
	| TAbstract ({ a_path = [],"Int" },[]) -> KInt
	| TAbstract ({ a_path = [],"Float" },[]) -> KFloat
	| TAbstract (a,[]) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) a.a_to -> KParam t
	| TInst ({ cl_kind = KTypeParameter ctl },_) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) ctl -> KParam t
	| TMono r when !r = None -> KUnk
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

let get_abstract_froms a pl =
	let l = List.map (apply_params a.a_params pl) a.a_from in
	List.fold_left (fun acc (t,f) ->
		match follow (Type.field_type f) with
		| TFun ([_,_,v],t) ->
			(try
				ignore(type_eq EqStrict t (TAbstract(a,List.map dup pl))); (* unify fields monomorphs *)
				v :: acc
			with Unify_error _ ->
				acc)
		| _ ->
			acc
	) l a.a_from_field

let maybe_type_against_enum ctx f with_type iscall p =
	try
		begin match with_type with
		| WithType t ->
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
							if (List.exists (fast_eq t) stack) then raise Exit;
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
							unify ctx t' t e.epos;
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

let merge_core_doc ctx c =
	let c_core = Typeload.load_core_class ctx c in
	if c.cl_doc = None then c.cl_doc <- c_core.cl_doc;
	let maybe_merge cf_map cf =
		if cf.cf_doc = None then try cf.cf_doc <- (PMap.find cf.cf_name cf_map).cf_doc with Not_found -> ()
	in
	List.iter (maybe_merge c_core.cl_fields) c.cl_ordered_fields;
	List.iter (maybe_merge c_core.cl_statics) c.cl_ordered_statics;
	match c.cl_constructor,c_core.cl_constructor with
		| Some ({cf_doc = None} as cf),Some cf2 -> cf.cf_doc <- cf2.cf_doc
		| _ -> ()

let check_error ctx err p = match err with
	| Module_not_found ([],name) when Diagnostics.is_diagnostics_run p ->
		DisplayToplevel.handle_unresolved_identifier ctx name p true
	| _ ->
		display_error ctx (error_msg err) p

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let rec unify_min_raise basic (el:texpr list) : t =
	let rec base_types t =
		let tl = ref [] in
		let rec loop t = (match t with
			| TInst(cl, params) ->
				(match cl.cl_kind with
				| KTypeParameter tl -> List.iter loop tl
				| _ -> ());
				List.iter (fun (ic, ip) ->
					let t = apply_params cl.cl_params params (TInst (ic,ip)) in
					loop t
				) cl.cl_implements;
				(match cl.cl_super with None -> () | Some (csup, pl) ->
					let t = apply_params cl.cl_params params (TInst (csup,pl)) in
					loop t);
				tl := t :: !tl;
			| TType (td,pl) ->
				loop (apply_params td.t_params pl td.t_type);
				(* prioritize the most generic definition *)
				tl := t :: !tl;
			| TLazy f -> loop (lazy_type f)
			| TMono r -> (match !r with None -> () | Some t -> loop t)
			| _ -> tl := t :: !tl)
		in
		loop t;
		!tl
	in
	match el with
	| [] -> mk_mono()
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
		let has_error, t = loop (mk_mono()) el in
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
				let t = try unify_min_raise basic el with Unify_error _ -> raise Not_found in
				PMap.add n (mk_field n t (List.hd el).epos null_pos) acc
			) fields PMap.empty in
			TAnon { a_fields = fields; a_status = ref Closed }
		with Not_found ->
			(* Second pass: Get all base types (interfaces, super classes and their interfaces) of most general type.
			   Then for each additional type filter all types that do not unify. *)
			let common_types = base_types t in
			let dyn_types = List.fold_left (fun acc t ->
				let rec loop c =
					Meta.has Meta.UnifyMinDynamic c.cl_meta || (match c.cl_super with None -> false | Some (c,_) -> loop c)
				in
				match t with
				| TInst (c,params) when params <> [] && loop c ->
					TInst (c,List.map (fun _ -> t_dynamic) params) :: acc
				| _ -> acc
			) [] common_types in
			let common_types = ref (match List.rev dyn_types with [] -> common_types | l -> common_types @ l) in
			let loop e =
				let first_error = ref None in
				let filter t = (try Type.unify e.etype t; true
					with Unify_error l -> if !first_error = None then first_error := Some(Unify l,e.epos); false)
				in
				common_types := List.filter filter !common_types;
				match !common_types, !first_error with
				| [], Some(err,p) -> raise_error err p
				| _ -> ()
			in
			match !common_types with
			| [] ->
				error "No common base type found" (punion (List.hd el).epos (List.hd (List.rev el)).epos)
			| _ ->
				List.iter loop (List.tl el);
				List.hd !common_types

let unify_min ctx el =
	try unify_min_raise ctx.com.basic el
	with Error (Unify l,p) ->
		if not ctx.untyped then display_error ctx (error_msg (Unify l)) p;
		(List.hd el).etype

let rec type_ident_raise ctx i p mode =
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
		(match mode, ctx.curclass.cl_kind with
		| MSet, KAbstractImpl _ ->
			(match ctx.curfield.cf_kind with
			| Method MethInline -> ()
			| Method _ when ctx.curfield.cf_name = "_new" -> ()
			| _ -> error "Abstract 'this' value can only be modified inside an inline function" p);
			AKExpr (get_this ctx p)
		| (MCall, KAbstractImpl _) | (MGet, _)-> AKExpr(get_this ctx p)
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
			AKExpr (null (mk_mono()) p)
		else
			AKNo i
	| _ ->
	try
		let v = PMap.find i ctx.locals in
		(match v.v_extra with
		| Some (params,e) ->
			let t = monomorphs params v.v_type in
			(match e with
			| Some ({ eexpr = TFunction f } as e) when ctx.com.display.dms_full_typing ->
				begin match mode with
					| MSet -> error "Cannot set inline closure" p
					| MGet -> error "Cannot create closure on inline closure" p
					| MCall ->
						(* create a fake class with a fake field to emulate inlining *)
						let c = mk_class ctx.m.curmod (["local"],v.v_name) e.epos null_pos in
						let cf = { (mk_field v.v_name v.v_type e.epos null_pos) with cf_params = params; cf_expr = Some e; cf_kind = Method MethInline } in
						c.cl_extern <- true;
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
		(* lookup using on 'this' *)
		if ctx.curfun = FunStatic then raise Not_found;
		(match using_field ctx mode (mk (TConst TThis) ctx.tthis p) i p with
		| AKUsing (et,c,f,_) -> AKUsing (et,c,f,get_this ctx p)
		| _ -> assert false)
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		if Meta.has Meta.Impl f.cf_meta && not (Meta.has Meta.Impl ctx.curfield.cf_meta) && not (Meta.has Meta.Enum f.cf_meta) then
			error (Printf.sprintf "Cannot access non-static field %s from static method" f.cf_name) p;
		let e = type_type ctx ctx.curclass.cl_path p in
		(* check_locals_masking already done in type_type *)
		field_access ctx mode f (FStatic (ctx.curclass,f)) (field_type ctx ctx.curclass [] f p) e p
	with Not_found -> try
		let wrap e = if mode = MSet then
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
							ImportHandling.maybe_mark_import_position ctx pt;
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
					| _ -> loop l)
				| TEnumDecl e ->
					try
						let ef = PMap.find i e.e_constrs in
						let et = type_module_type ctx t None p in
						let monos = List.map (fun _ -> mk_mono()) e.e_params in
						let monos2 = List.map (fun _ -> mk_mono()) ef.ef_params in
						ImportHandling.maybe_mark_import_position ctx pt;
						wrap (mk (TField (et,FEnum (e,ef))) (enum_field_type ctx e ef monos monos2 p) p)
					with
						Not_found -> loop l
		in
		(try loop (List.rev_map (fun t -> t,null_pos) ctx.m.curmod.m_types) with Not_found -> loop ctx.m.module_types)
	with Not_found ->
		(* lookup imported globals *)
		let t, name, pi = PMap.find i ctx.m.module_globals in
		ImportHandling.maybe_mark_import_position ctx pi;
		let e = type_module_type ctx t None p in
		type_field ctx e name p mode

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
		| TAnon a when !(a.a_status) = Opened -> maybe_dynamic_mono e
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
	match op with
	| OpAssign ->
		let e1 = type_access ctx (fst e1) (snd e1) MSet in
		let tt = (match e1 with AKNo _ | AKInline _ | AKUsing _ | AKMacro _ | AKAccess _ -> Value | AKSet(_,t,_) -> WithType t | AKExpr e -> WithType e.etype) in
		let e2 = type_expr ctx e2 tt in
		(match e1 with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr e1  ->
			let e2 = AbstractCast.cast_or_unify ctx e1.etype e2 p in
			check_assign ctx e1;
			(match e1.eexpr , e2.eexpr with
			| TLocal i1 , TLocal i2 when i1 == i2 -> error "Assigning a value to itself" p
			| TField ({ eexpr = TConst TThis },FInstance (_,_,f1)) , TField ({ eexpr = TConst TThis },FInstance (_,_,f2)) when f1 == f2 ->
				error "Assigning a value to itself" p
			| _ , _ -> ());
			mk (TBinop (op,e1,e2)) e1.etype p
		| AKSet (e,t,cf) ->
			let e2 = AbstractCast.cast_or_unify ctx t e2 p in
			make_call ctx (mk (TField (e,quick_field_dynamic e.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [e2] t p
		| AKAccess(a,tl,c,ebase,ekey) ->
			mk_array_set_call ctx (AbstractCast.find_array_access ctx a tl ekey (Some e2) p) c ebase p
		| AKUsing(ef,_,_,et) ->
			(* this must be an abstract setter *)
			let e2,ret = match follow ef.etype with
				| TFun([_;(_,_,t)],ret) ->
					AbstractCast.cast_or_unify ctx t e2 p,ret
				| _ ->  error "Invalid field type for abstract setter" p
			in
			make_call ctx ef [et;e2] ret p
		| AKInline _ | AKMacro _ ->
			assert false)
	| OpAssignOp (OpBoolAnd | OpBoolOr) ->
		error "The operators ||= and &&= are not supported" p
	| OpAssignOp op ->
		(match type_access ctx (fst e1) (snd e1) MSet with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
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
		| AKUsing(ef,c,cf,et) ->
			(* abstract setter + getter *)
			let ta = match c.cl_kind with KAbstractImpl a -> TAbstract(a, List.map (fun _ -> mk_mono()) a.a_params) | _ -> assert false in
			let ret = match follow ef.etype with
				| TFun([_;_],ret) -> ret
				| _ ->  error "Invalid field type for abstract setter" p
			in
			let l = save_locals ctx in
			let v,is_temp = match et.eexpr with
				| TLocal v when not (v.v_name = "this") -> v,false
				| _ -> gen_local ctx ta ef.epos,true
			in
			let ev = mk (TLocal v) ta p in
			(* this relies on the fact that cf_name is set_name *)
			let getter_name = String.sub cf.cf_name 4 (String.length cf.cf_name - 4) in
			let get = type_binop ctx op (EField ((EConst (Ident v.v_name),p),getter_name),p) e2 true with_type p in
			unify ctx get.etype ret p;
			l();
			let e_call = make_call ctx ef [ev;get] ret p in
			if is_temp then
				mk (TBlock [
					mk (TVar (v,Some et)) ctx.t.tvoid p;
					e_call
				]) ret p
			else
				e_call
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
			let eget = type_binop2 ctx op eget e2 true (WithType eget.etype) p in
			unify ctx eget.etype r_get p;
			let cf_set,tf_set,r_set,ekey,eget = AbstractCast.find_array_access ctx a tl ekey (Some eget) p in
			let eget = match eget with None -> assert false | Some e -> e in
			let et = type_module_type ctx (TClassDecl c) None p in
			let e = match cf_set.cf_expr,cf_get.cf_expr with
				| None,None ->
					let ea = mk (TArray(ebase,ekey)) r_get p in
					mk (TBinop(OpAssignOp op,ea,type_expr ctx e2 (WithType r_get))) r_set p
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
		| AKInline _ | AKMacro _ ->
			assert false)
	| _ ->
		(* If the with_type is an abstract which has exactly one applicable @:op method, we can promote it
		   to the individual arguments (issue #2786). *)
		let wt = match with_type with
			| WithType t ->
				begin match follow t with
					| TAbstract(a,_) ->
						begin match List.filter (fun (o,_) -> o = OpAssignOp(op) || o == op) a.a_ops with
							| [_] -> with_type
							| _ -> Value
						end
					| _ ->
						Value
				end
			| _ ->
				Value
		in
		let e1 = type_expr ctx e1 wt in
		type_binop2 ctx op e1 e2 is_assign_op wt p

and type_binop2 ctx op (e1 : texpr) (e2 : Ast.expr) is_assign_op wt p =
	let e2 = type_expr ctx e2 (if op == OpEq || op == OpNotEq then WithType e1.etype else wt) in
	let tint = ctx.t.tint in
	let tfloat = ctx.t.tfloat in
	let tstring = ctx.t.tstring in
	let to_string e =
		let rec loop t = match classify t with
			| KAbstract ({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
				call_to_string ctx e
			| KInt | KFloat | KString -> e
			| KUnk | KDyn | KParam _ | KOther ->
				let std = type_type ctx ([],"Std") e.epos in
				let acc = acc_get ctx (type_field ctx std "string" e.epos MCall) e.epos in
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
		| KParam t1, KParam t2 when Type.type_iseq t1 t2 ->
			t1
		| KParam t, KInt | KInt, KParam t ->
			t
		| KParam _, KFloat | KFloat, KParam _ | KParam _, KParam _ ->
			tfloat
		| KParam t, KUnk ->
			unify ctx e2.etype tfloat e2.epos;
			tfloat
		| KUnk, KParam t ->
			unify ctx e1.etype tfloat e1.epos;
			tfloat
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
		| KParam _, _
		| _, KParam _
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
		| KParam t1, KParam t2 when Type.type_iseq t1 t2 ->
			if op <> OpDiv then result := t1
		| KParam _, KParam _ ->
			result := tfloat
		| KParam t, KInt | KInt, KParam t ->
			if op <> OpDiv then result := t
		| KParam _, KFloat | KFloat, KParam _ ->
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
		| KParam _ , x when x <> KString && x <> KOther -> ()
		| x , KParam _ when x <> KString && x <> KOther -> ()
		| KAbstract _,_
		| _,KAbstract _
		| KDyn , KUnk
		| KUnk , KDyn
		| KString , KInt
		| KString , KFloat
		| KInt , KString
		| KFloat , KString
		| KParam _ , _
		| _ , KParam _
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
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[e1;e2])) t p
	| OpArrow ->
		error "Unexpected =>" p
	| OpIn ->
		error "Unexpected in" p
	| OpAssign
	| OpAssignOp _ ->
		assert false
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
								let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
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
							check_constraints ctx "" cf.cf_params monos (apply_params a.a_params tl) false cf.cf_pos;
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
						assert false
				end
			| [] ->
				raise Not_found
		in
		loop (if left then a.a_ops else List.filter (fun (_,cf) -> not (Meta.has Meta.Impl cf.cf_meta)) a.a_ops)
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
		make e1 e2

and type_unop ctx op flag e p =
	let set = (op = Increment || op = Decrement) in
	let acc = type_access ctx (fst e) (snd e) (if set then MSet else MGet) in
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
				| KParam t ->
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
						let m = mk_mono() in
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
				let v_key = alloc_var "tmp" ekey.etype ekey.epos in
				let evar_key = mk (TVar(v_key,Some ekey)) ctx.com.basic.tvoid ekey.epos in
				let ekey = mk (TLocal v_key) ekey.etype ekey.epos in
				(* get *)
				let e_get = mk_array_get_call ctx (AbstractCast.find_array_access_raise ctx a tl ekey None p) c ebase p in
				let v_get = alloc_var "tmp" e_get.etype e_get.epos in
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
		| AKInline _ | AKUsing _ | AKMacro _ ->
			error "This kind of operation is not supported" p
		| AKSet (e,t,cf) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype p in
			let ev = mk (TLocal v) e.etype p in
			let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in
			let one = (EConst (Int "1"),p) in
			let eget = (EField ((EConst (Ident v.v_name),p),cf.cf_name),p) in
			match flag with
			| Prefix ->
				let get = type_binop ctx op eget one false Value p in
				unify ctx get.etype t p;
				l();
				mk (TBlock [
					mk (TVar (v,Some e)) ctx.t.tvoid p;
					make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [get] t p
				]) t p
			| Postfix ->
				let v2 = gen_local ctx t p in
				let ev2 = mk (TLocal v2) t p in
				let get = type_expr ctx eget Value in
				let plusone = type_binop ctx op (EConst (Ident v2.v_name),p) one false Value p in
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

and type_ident ctx i p mode =
	try
		type_ident_raise ctx i p mode
	with Not_found -> try
		(* lookup type *)
		if is_lower_ident i then raise Not_found;
		let e = (try type_type ctx ([],i) p with Error (Module_not_found ([],name),_) when name = i -> raise Not_found) in
		AKExpr e
	with Not_found ->
		if ctx.untyped then begin
			if i = "__this__" then
				AKExpr (mk (TConst TThis) ctx.tthis p)
			else
				let t = mk_mono() in
				AKExpr ((mk (TIdent i)) t p)
		end else begin
			if ctx.curfun = FunStatic && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
			begin try
				let t = List.find (fun (i2,_) -> i2 = i) ctx.type_params in
				let c = match follow (snd t) with TInst(c,_) -> c | _ -> assert false in
                if TypeloadCheck.is_generic_parameter ctx c && Meta.has Meta.Const c.cl_meta then begin
                    let e = type_module_type ctx (TClassDecl c) None p in
                    AKExpr {e with etype = (snd t)}
				end else begin
					display_error ctx ("Only @:const type parameters on @:generic classes can be used as value") p;
					AKExpr (mk (TConst TNull) t_dynamic p)
				end
			with Not_found ->
				let err = Unknown_ident i in
				if ctx.in_display then begin
					raise (Error (err,p))
				end;
				match ctx.com.display.dms_kind with
					| DMNone ->
						raise (Error(err,p))
					| DMDiagnostics b when b || ctx.is_display_file ->
						DisplayToplevel.handle_unresolved_identifier ctx i p false;
						let t = mk_mono() in
						AKExpr (mk (TIdent i) t p)
					| _ ->
						display_error ctx (error_msg err) p;
						let t = mk_mono() in
						AKExpr (mk (TIdent i) t p)
			end
		end

(* MORDOR *)
and handle_efield ctx e p mode =
	let p0 = p in
	(*
		given chain of fields as the `path` argument and an `access_mode->access_kind` getter for some starting expression as `e`,
		return a new `access_mode->access_kind` getter for the whole field access chain.

		if `resume` is true, `Not_found` will be raised if the first field in chain fails to resolve, in all other
		cases, normal type errors will be raised if a field can't be accessed.
	*)
	let fields ?(resume=false) path e =
		let resume = ref resume in
		let force = ref false in
		let e = List.fold_left (fun e (f,_,p) ->
			let e = acc_get ctx (e MGet) p in
			let f = type_field ~resume:(!resume) ctx e f p in
			force := !resume;
			resume := false;
			f
		) e path in
		if !force then ignore(e MCall); (* not necessarily a call, but prevent #2602 among others *)
		e
	in

	(*
		given a chain of identifiers (dot-path) represented as a list of (ident,starts_uppercase,pos) tuples,
		resolve it into an `access_mode->access_kind` getter for the resolved expression
	*)
	let type_path path =
		(*
			this is an actual loop for processing a fully-qualified dot-path.
			it relies on the fact that packages start with a lowercase letter, while modules and types
			start with upper-case letters, so it processes path parts, accumulating lowercase package parts in `acc`,
			until it encounters an upper-case part, which can mean either a module access or module's primary type access,
			so it tries to figure out the type and and calls `fields` on it to resolve the rest of field access chain.
		*)
		let rec loop acc path =
			match path with
			| (_,false,_) as x :: path ->
				(* part starts with lowercase - it's a package part, add it the accumulator and proceed *)
				loop (x :: acc) path

			| (name,true,p) as x :: path ->
				(* part starts with uppercase - it either points to a module or its main type *)

				(* acc is contains all the package parts now, so extract package from them *)
				let pack = List.rev_map (fun (x,_,_) -> x) acc in

				(* default behaviour: try loading module's primary type (with the same name as module)
				   and resolve the rest of the field chain against its statics, or the type itself
				   if the rest of chain is empty *)
				let def() =
					try
						let e = type_type ctx (pack,name) p in
						fields path (fun _ -> AKExpr e)
					with
						Error (Module_not_found m,_) when m = (pack,name) ->
							(* if it's not a module path after all, it could be an untyped field access that looks like
							   a dot-path, e.g. `untyped __global__.String`, add the whole path to the accumulator and
							   proceed to the untyped identifier resolution *)
							loop ((List.rev path) @ x :: acc) []
				in

				(match path with
				| (sname,true,p) :: path ->
					(* next part starts with uppercase, meaning it can be either a module sub-type access
					   or static field access for the primary module type, so we have to do some guessing here

					   In this block, `name` is the first first-uppercase part (possibly a module name),
					   and `sname` is the second first-uppsercase part (possibly a subtype name). *)

					(* get static field by `sname` from a given type `t`, if `resume` is true - raise Not_found *)
					let get_static resume t =
						fields ~resume ((sname,true,p) :: path) (fun _ -> AKExpr (type_module_type ctx t None p))
					in

					(* try accessing subtype or main class static field by `sname` in given module with path `m` *)
					let check_module m =
						try
							let md = TypeloadModule.load_module ctx m p in
							(* first look for existing subtype *)
							(try
								let t = List.find (fun t -> not (t_infos t).mt_private && t_path t = (fst m,sname)) md.m_types in
								Some (fields path (fun _ -> AKExpr (type_module_type ctx t None p)))
							with Not_found -> try
							(* then look for main type statics *)
								if fst m = [] then raise Not_found; (* ensure that we use def() to resolve local types first *)
								let t = List.find (fun t -> not (t_infos t).mt_private && t_path t = m) md.m_types in
								Some (get_static false t)
							with Not_found ->
								None)
						with Error (Module_not_found m2,_) when m = m2 ->
							None
					in

					(match pack with
					| [] ->
						(* if there's no package specified... *)
						(try
							(* first try getting a type by `name` in current module types and current imports
							   and try accessing its static field by `sname` *)
							let path_match t = snd (t_infos t).mt_path = name in
							let t =
								try
									List.find path_match ctx.m.curmod.m_types (* types in this modules *)
								with Not_found ->
									let t,p = List.find (fun (t,_) -> path_match t) ctx.m.module_types in (* imported types *)
									ImportHandling.maybe_mark_import_position ctx p;
									t
							in
							get_static true t
						with Not_found ->
							(* if the static field (or the type) wasn't not found, look for a subtype instead - #1916
							   look for subtypes/main-class-statics in modules of current package and its parent packages *)
							let rec loop pack =
								match check_module (pack,name) with
								| Some r -> r
								| None ->
									match List.rev pack with
									| [] -> def()
									| _ :: l -> loop (List.rev l)
							in
							loop (fst ctx.m.curmod.m_path))
					| _ ->
						(* if package was specified, treat it as fully-qualified access to either
						   a module subtype or a static field of module's primary type*)
						match check_module (pack,name) with
						| Some r -> r
						| None -> def());
				| _ ->
					(* no more parts or next part starts with lowercase - it's surely not a type name,
					   so do the default thing: resolve fields against primary module type *)
					def())

			| [] ->
				(* If we get to here, it means that either there were no uppercase-first-letter parts,
				   or we couldn't find the specified module, so it's not a qualified dot-path after all.
				   And it's not a known identifier too, because otherwise `loop` wouldn't be called at all.
				   So this must be an untyped access (or a typo). Try resolving the first identifier with support
				   for untyped and resolve the rest of field chain against it.

				   TODO: extract this into a separate function
				*)
				(match List.rev acc with
				| [] -> assert false
				| (name,flag,p) :: path ->
					try
						fields path (type_ident ctx name p)
					with
						Error (Unknown_ident _,p2) as e when p = p2 ->
							try
								(* try raising a more sensible error if there was an uppercase-first (module name) part *)
								let path = ref [] in
								let name , _ , _ = List.find (fun (name,flag,p) ->
									if flag then
										true
									else begin
										path := name :: !path;
										false
									end
								) (List.rev acc) in
								raise (Error (Module_not_found (List.rev !path,name),p))
							with
								Not_found ->
									(* if there was no module name part, last guess is that we're trying to get package completion *)
									if ctx.in_display then begin
										if ctx.com.json_out = None then raise (Parser.TypePath (List.map (fun (n,_,_) -> n) (List.rev acc),None,false,p))
										else raise_fields (DisplayToplevel.collect ctx None NoValue) CRTypeHint (Some (Parser.cut_pos_at_display p0)) false;
									end;
									raise e)
		in
		match path with
		| [] -> assert false
		| (name,_,p) :: pnext ->
			try
				(*
					first, try to resolve the first ident in the chain and access its fields.
					this doesn't support untyped identifiers yet, because we want to check
					fully-qualified dot paths first even in an untyped block.
				*)
				fields pnext (fun _ -> type_ident_raise ctx name p MGet)
			with Not_found ->
				(* first ident couldn't be resolved, it's probably a fully qualified path - resolve it *)
				loop [] path
	in

	(*
		loop through the given EField expression and behave differently depending on whether it's a simple dot-path
		or a more complex expression, accumulating field access parts in form of (ident,starts_uppercase,pos) tuples.

		if it's a dot-path, then it might be either fully-qualified access (pack.Class.field) or normal field access of
		a local/global/field identifier. we pass the accumulated path to `type_path` and let it figure out what it is.

		if it's NOT a dot-path (anything other than indentifiers appears in EField chain), then we can be sure it's
		normal field access, not fully-qualified access, so we pass the non-ident expr along with the accumulated
		fields chain to the `fields` function and let it type the field access.
	*)
	let rec loop acc (e,p) =
		match e with
		| EField (e,s) ->
			loop ((s,not (is_lower_ident s),p) :: acc) e
		| EConst (Ident i) ->
			type_path ((i,not (is_lower_ident i),p) :: acc)
		| _ ->
			fields acc (type_access ctx e p)
	in
	loop [] (e,p) mode

and type_access ctx e p mode =
	match e with
	| EConst (Ident s) ->
		type_ident ctx s p mode
	| EField (e1,"new") ->
		let e1 = type_expr ctx e1 Value in
		begin match e1.eexpr with
			| TTypeExpr (TClassDecl c) ->
				if mode = MSet then error "Cannot set constructor" p;
				if mode = MCall then error ("Cannot call constructor like this, use 'new " ^ (s_type_path c.cl_path) ^ "()' instead") p;
				let monos = List.map (fun _ -> mk_mono()) c.cl_params in
				let ct, cf = get_constructor ctx c monos p in
				let args = match follow ct with TFun(args,ret) -> args | _ -> assert false in
				let vl = List.map (fun (n,_,t) -> alloc_var n t c.cl_pos) args in
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
		handle_efield ctx e p mode
	| EArray (e1,e2) ->
		type_array_access ctx e1 e2 p mode
	| _ ->
		AKExpr (type_expr ctx (e,p) Value)

and type_array_access ctx e1 e2 p mode =
	let e1 = type_expr ctx e1 Value in
	let e2 = type_expr ctx e2 Value in
	let has_abstract_array_access = ref false in
	try
		(match follow e1.etype with
		| TAbstract ({a_impl = Some c} as a,pl) when a.a_array <> [] ->
			begin match mode with
			| MSet ->
				(* resolve later *)
				AKAccess (a,pl,c,e1,e2)
			| _ ->
				has_abstract_array_access := true;
				let e = mk_array_get_call ctx (AbstractCast.find_array_access ctx a pl e2 None p) c e1 p in
				AKExpr e
			end
		| _ -> raise Not_found)
	with Not_found ->
		unify ctx e2.etype ctx.t.tint e2.epos;
		let rec loop et =
			match follow et with
			| TInst ({ cl_array_access = Some t; cl_params = pl },tl) ->
				apply_params pl tl t
			| TInst ({ cl_super = Some (c,stl); cl_params = pl },tl) ->
				apply_params pl tl (loop (TInst (c,stl)))
			| TInst ({ cl_path = [],"ArrayAccess" },[t]) ->
				t
			| TInst ({ cl_path = [],"Array"},[t]) when t == t_dynamic ->
				t_dynamic
			| TAbstract(a,tl) when Meta.has Meta.ArrayAccess a.a_meta ->
				loop (apply_params a.a_params tl a.a_this)
			| _ ->
				let pt = mk_mono() in
				let t = ctx.t.tarray pt in
				(try unify_raise ctx et t p
				with Error(Unify _,_) -> if not ctx.untyped then begin
					if !has_abstract_array_access then error ("No @:arrayAccess function accepts an argument of " ^ (s_type (print_context()) e2.etype)) e1.epos
					else error ("Array access is not allowed on " ^ (s_type (print_context()) e1.etype)) e1.epos
				end);
				pt
		in
		let pt = loop e1.etype in
		AKExpr (mk (TArray (e1,e2)) pt p)

and type_vars ctx vl p =
	let vl = List.map (fun ((v,pv),t,e) ->
		try
			let t = Typeload.load_type_hint ctx p t in
			let e = (match e with
				| None -> None
				| Some e ->
					let e = type_expr ctx e (WithType t) in
					let e = AbstractCast.cast_or_unify ctx t e p in
					Some e
			) in
			if v.[0] = '$' then display_error ctx "Variables names starting with a dollar are not allowed" p;
			let v = add_local ctx v t pv in
			v.v_meta <- (Meta.UserVariable,[],pv) :: v.v_meta;
			if ctx.in_display && Display.is_display_position pv then
				DisplayEmitter.display_variable ctx v pv;
			v,e
		with
			Error (e,p) ->
				check_error ctx e p;
				add_local ctx v t_dynamic pv, None (* TODO: What to do with this... *)
	) vl in
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
		let enext = if ctx.in_display && Display.is_display_position p then
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
		if len > 0 || !e = None then add (EConst (String (String.sub s start len))) len
	in
	let warn_escape = Common.defined ctx.com Define.FormatWarning in
	let warn pos len =
		ctx.com.warning "This string is formatted" { p with pmin = !pmin + 1 + pos; pmax = !pmin + 1 + pos + len }
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
			if warn_escape then warn pos 1;
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
			if warn_escape then warn pos len;
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
				| [] -> assert false
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
		if warn_escape then warn (pos + 1) slen;
		min := !min + 2;
		if slen > 0 then begin
			let e =
				let ep = { p with pmin = !pmin + pos + 2; pmax = !pmin + send + 1 } in
				try ParserEntry.parse_expr_string ctx.com.defines scode ep error true
				with Exit -> error "Invalid interpolated expression" ep
			in
			add_expr e slen
		end;
		min := !min + 1;
		parse (send + 1) (send + 1)
	in
	parse 0 0;
	match !e with
	| None -> assert false
	| Some e -> e

and type_block ctx el with_type p =
	let merge e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), {eexpr = TBlock el}) ->
			el
		| _ -> [e]
	in
	let rec loop = function
		| [] -> []
		| [e] ->
			(try
				merge (type_expr ctx e with_type)
			with Error (e,p) -> check_error ctx e p; [])
		| e :: l ->
			try
				let e = type_expr ctx e NoValue in
				merge e @ loop l
			with
				Error (e,p) -> check_error ctx e p; loop l
	in
	let l = loop el in
	let rec loop = function
		| [] -> ctx.t.tvoid
		| [e] -> e.etype
		| _ :: l -> loop l
	in
	mk (TBlock l) (loop l) p

and type_object_decl ctx fl with_type p =
	let dynamic_parameter = ref None in
	let a = (match with_type with
	| WithType t ->
		let rec loop in_abstract_from t =
			match follow t with
			| TAnon a when not (PMap.is_empty a.a_fields) && not in_abstract_from -> ODKWithStructure a
			| TAbstract (a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				(match List.fold_left (fun acc t -> match loop true t with ODKPlain -> acc | t -> t :: acc) [] (get_abstract_froms a pl) with
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
		loop false t
	| _ ->
		ODKPlain
	) in
	let type_fields field_map =
		let fields = ref PMap.empty in
		let extra_fields = ref [] in
		let fl = List.map (fun ((n,pn,qs),e) ->
			let is_valid = Lexer.is_valid_identifier n in
			if PMap.mem n !fields then error ("Duplicate field in object declaration : " ^ n) p;
			let e = try
				let t = match !dynamic_parameter with
					| Some t -> t
					| None ->
						let cf = PMap.find n field_map in
						if ctx.in_display && Display.is_display_position pn then DisplayEmitter.display_field ctx Unknown CFSMember cf pn;
						cf.cf_type
				in
				let e = type_expr ctx e (WithType t) in
				let e = AbstractCast.cast_or_unify ctx t e e.epos in
				(try type_eq EqStrict e.etype t; e with Unify_error _ -> mk (TCast (e,None)) t e.epos)
			with Not_found ->
				if is_valid then
					extra_fields := n :: !extra_fields;
				type_expr ctx e Value
			in
			if is_valid then begin
				if String.length n > 0 && n.[0] = '$' then error "Field names starting with a dollar are not allowed" p;
				let cf = mk_field n e.etype (punion pn e.epos) pn in
				fields := PMap.add n cf !fields;
			end;
			((n,pn,qs),e)
		) fl in
		let t = (TAnon { a_fields = !fields; a_status = ref Const }) in
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
	(match a with
	| ODKPlain ->
		let rec loop (l,acc) ((f,pf,qs),e) =
			let is_valid = Lexer.is_valid_identifier f in
			if PMap.mem f acc then error ("Duplicate field in object declaration : " ^ f) p;
			let e = type_expr ctx e Value in
			(match follow e.etype with TAbstract({a_path=[],"Void"},_) -> error "Fields of type Void are not allowed in structures" e.epos | _ -> ());
			let cf = mk_field f e.etype (punion pf e.epos) pf in
			if ctx.in_display && Display.is_display_position pf then DisplayEmitter.display_field ctx Unknown CFSMember cf pf;
			(((f,pf,qs),e) :: l, if is_valid then begin
				if String.length f > 0 && f.[0] = '$' then error "Field names starting with a dollar are not allowed" p;
				PMap.add f cf acc
			end else acc)
		in
		let fields , types = List.fold_left loop ([],PMap.empty) fl in
		let x = ref Const in
		ctx.opened <- x :: ctx.opened;
		mk (TObjectDecl (List.rev fields)) (TAnon { a_fields = types; a_status = x }) p
	| ODKWithStructure a ->
		let t, fl = type_fields a.a_fields in
		if !(a.a_status) = Opened then a.a_status := Closed;
		mk (TObjectDecl fl) t p
	| ODKWithClass (c,tl) ->
		let t,ctor = get_constructor ctx c tl p in
		let args = match follow t with
			| TFun(args,_) -> args
			| _ -> assert false
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
			with Not_found -> mk (TConst TNull) t p
		) args in
		let e = mk (TNew(c,tl,el)) (TInst(c,tl)) p in
		mk (TBlock (List.rev (e :: (List.rev evars)))) e.etype e.epos
	)

and type_new ctx path el with_type p =
	let unify_constructor_call c params f ct = match follow ct with
		| TFun (args,r) ->
			(try
				let el,_,_ = unify_field_call ctx (FInstance(c,params,f)) el args r p false in
				el
			with Error (e,p) ->
				display_error ctx (error_msg e) p;
				[])
		| _ ->
			error "Constructor is not a function" p
	in
	let t = if (fst path).tparams <> [] then
		follow (Typeload.load_instance ctx path false p)
	else try
		ctx.call_argument_stack <- el :: ctx.call_argument_stack;
		let t = follow (Typeload.load_instance ctx path true p) in
		ctx.call_argument_stack <- List.tl ctx.call_argument_stack;
		(* Try to properly build @:generic classes here (issue #2016) *)
		begin match t with
			| TInst({cl_kind = KGeneric } as c,tl) -> follow (Generic.build_generic ctx c p tl)
			| _ -> t
		end
	with Generic.Generic_Exception _ ->
		(* Try to infer generic parameters from the argument list (issue #2044) *)
		match resolve_typedef (Typeload.load_type_def ctx p (fst path)) with
		| TClassDecl ({cl_constructor = Some cf} as c) ->
			let monos = List.map (fun _ -> mk_mono()) c.cl_params in
			let ct, f = get_constructor ctx c monos p in
			ignore (unify_constructor_call c monos f ct);
			begin try
				let t = Generic.build_generic ctx c p monos in
				let map = apply_params c.cl_params monos in
				check_constraints ctx (s_type_path c.cl_path) c.cl_params monos map true p;
				t
			with Generic.Generic_Exception _ as exc ->
				(* If we have an expected type, just use that (issue #3804) *)
				begin match with_type with
					| WithType t ->
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
	in
	DisplayEmitter.check_display_type ctx t (pos path);
	let build_constructor_call c tl =
		let ct, f = get_constructor ctx c tl p in
		if (Meta.has Meta.CompilerGenerated f.cf_meta) then display_error ctx (error_msg (No_constructor (TClassDecl c))) p;
		if not (can_access ctx c f true || is_parent c ctx.curclass) && not ctx.untyped then display_error ctx (Printf.sprintf "Cannot access private constructor of %s" (s_type_path c.cl_path)) p;
		(match f.cf_kind with
		| Var { v_read = AccRequire (r,msg) } -> (match msg with Some msg -> error msg p | None -> error_require r p)
		| _ -> ());
		let el = unify_constructor_call c tl f ct in
		el,f,ct
	in
	try begin match t with
	| TInst ({cl_kind = KTypeParameter tl} as c,params) ->
		if not (TypeloadCheck.is_generic_parameter ctx c) then error "Only generic type parameters can be constructed" p;
		let el = List.map (fun e -> type_expr ctx e Value) el in
		let ct = (tfun (List.map (fun e -> e.etype) el) ctx.t.tvoid) in
		let rec loop t = match follow t with
			| TAnon a ->
				(try
					unify ctx (PMap.find "new" a.a_fields).cf_type ct p;
					true
				with Not_found ->
					 false)
			| TAbstract({a_path = ["haxe"],"Constructible"},_) -> true
			| TInst({cl_kind = KTypeParameter tl},_) -> List.exists loop tl
			| _ -> false
		in
		if not (List.exists loop tl) then raise_error (No_constructor (TClassDecl c)) p;
		mk (TNew (c,params,el)) t p
	| TAbstract({a_impl = Some c} as a,tl) when not (Meta.has Meta.MultiType a.a_meta) ->
		let el,cf,ct = build_constructor_call c tl in
		let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
		let e = mk (TTypeExpr (TClassDecl c)) ta p in
		let e = mk (TField (e,(FStatic (c,cf)))) ct p in
		make_call ctx e el t p
	| TInst (c,params) | TAbstract({a_impl = Some c},params) ->
		let el,_,_ = build_constructor_call c params in
		mk (TNew (c,params,el)) t p
	| _ ->
		error (s_type (print_context()) t ^ " cannot be constructed") p
	end with Error(No_constructor _ as err,p) when ctx.com.display.dms_display ->
		display_error ctx (error_msg err) p;
		Diagnostics.secure_generated_code ctx (mk (TConst TNull) t p)

and type_try ctx e1 catches with_type p =
	let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
	let rec check_unreachable cases t p = match cases with
		| (v,e) :: cases ->
			let unreachable () =
				display_error ctx "This block is unreachable" p;
				let st = s_type (print_context()) in
				display_error ctx (Printf.sprintf "%s can be assigned to %s, which is handled here" (st t) (st v.v_type)) e.epos
			in
			begin try
				begin match follow t,follow v.v_type with
					| TDynamic _, TDynamic _ ->
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
	let check_catch_type path params =
		List.iter (fun pt ->
			if pt != t_dynamic then error "Catch class parameter must be Dynamic" p;
		) params;
		(match path with
		| x :: _ , _ -> x
		| [] , name -> name)
	in
	let catches = List.fold_left (fun acc ((v,pv),t,e_ast,pc) ->
		let t = Typeload.load_complex_type ctx true p t in
		let rec loop t = match follow t with
			| TInst ({ cl_kind = KTypeParameter _} as c,_) when not (TypeloadCheck.is_generic_parameter ctx c) ->
				error "Cannot catch non-generic type parameter" p
			| TInst ({ cl_path = path },params)
			| TEnum ({ e_path = path },params) ->
				check_catch_type path params,t
			| TAbstract(a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
				check_catch_type a.a_path params,t
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				loop (Abstract.get_underlying_type a tl)
			| TDynamic _ -> "",t
			| _ -> error "Catch type must be a class, an enum or Dynamic" (pos e_ast)
		in
		let name,t2 = loop t in
		if v.[0] = '$' then display_error ctx "Catch variable names starting with a dollar are not allowed" p;
		check_unreachable acc t2 (pos e_ast);
		let locals = save_locals ctx in
		let v = add_local ctx v t pv in
		if ctx.is_display_file && Display.is_display_position pv then
			DisplayEmitter.display_variable ctx v pv;
		let e = type_expr ctx e_ast with_type in
		(* If the catch position is the display position it means we get completion on the catch keyword or some
		   punctuation. Otherwise we wouldn't reach this point. *)
		if ctx.is_display_file && Display.is_display_position pc then ignore(TyperDisplay.display_expr ctx e_ast e DKMarked with_type pc);
		v.v_type <- t2;
		locals();
		if with_type <> NoValue then unify ctx e.etype e1.etype e.epos;
		if PMap.mem name ctx.locals then error ("Local variable " ^ name ^ " is preventing usage of this type here") e.epos;
		(v , e) :: acc
	) [] catches in
	mk (TTry (e1,List.rev catches)) (if with_type = NoValue then ctx.t.tvoid else e1.etype) p

and type_map_declaration ctx e1 el with_type p =
	let (tkey,tval,has_type) =
		let get_map_params t = match follow t with
			| TAbstract({a_path=["haxe";"ds"],"Map"},[tk;tv]) -> tk,tv,true
			| TInst({cl_path=["haxe";"ds"],"IntMap"},[tv]) -> ctx.t.tint,tv,true
			| TInst({cl_path=["haxe";"ds"],"StringMap"},[tv]) -> ctx.t.tstring,tv,true
			| TInst({cl_path=["haxe";"ds"],("ObjectMap" | "EnumValueMap")},[tk;tv]) -> tk,tv,true
			| _ -> mk_mono(),mk_mono(),false
		in
		match with_type with
		| WithType t -> get_map_params t
		| _ -> (mk_mono(),mk_mono(),false)
	in
	let keys = Hashtbl.create 0 in
	let check_key e_key =
		try
			let p = Hashtbl.find keys e_key.eexpr in
			display_error ctx "Duplicate key" e_key.epos;
			error "Previously defined here" p
		with Not_found ->
			Hashtbl.add keys e_key.eexpr e_key.epos;
	in
	let el = e1 :: el in
	let el_kv = List.map (fun e -> match fst e with
		| EBinop(OpArrow,e1,e2) -> e1,e2
		| _ -> error "Expected a => b" (pos e)
	) el in
	let el_k,el_v,tkey,tval = if has_type then begin
		let el_k,el_v = List.fold_left (fun (el_k,el_v) (e1,e2) ->
			let e1 = type_expr ctx e1 (WithType tkey) in
			check_key e1;
			let e1 = AbstractCast.cast_or_unify ctx tkey e1 e1.epos in
			let e2 = type_expr ctx e2 (WithType tval) in
			let e2 = AbstractCast.cast_or_unify ctx tval e2 e2.epos in
			(e1 :: el_k,e2 :: el_v)
		) ([],[]) el_kv in
		el_k,el_v,tkey,tval
	end else begin
		let el_k,el_v = List.fold_left (fun (el_k,el_v) (e1,e2) ->
			let e1 = type_expr ctx e1 Value in
			check_key e1;
			let e2 = type_expr ctx e2 Value in
			(e1 :: el_k,e2 :: el_v)
		) ([],[]) el_kv in
		let unify_min_resume el = try
			unify_min_raise ctx.com.basic el
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
		| _ -> assert false
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

and type_local_function ctx name f with_type p =
	let params = TypeloadFunction.type_function_params ctx f (match name with None -> "localfun" | Some n -> n) p in
	if params <> [] then begin
		if name = None then display_error ctx "Type parameters not supported in unnamed local functions" p;
		if with_type <> NoValue then error "Type parameters are not supported for rvalue functions" p
	end;
	List.iter (fun tp -> if tp.tp_constraints <> [] then display_error ctx "Type parameter constraints are not supported for local functions" p) f.f_params;
	let inline, v = (match name with
		| None -> false, None
		| Some v when ExtString.String.starts_with v "inline_" -> true, Some (String.sub v 7 (String.length v - 7))
		| Some v -> false, Some v
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
	| WithType t ->
		let rec loop t =
			(match follow t with
			| TFun (args2,tr) when List.length args2 = List.length args ->
				List.iter2 (fun (_,_,t1) (_,_,t2) ->
					match follow t1 with
					| TMono _ -> unify ctx t2 t1 p
					| _ -> ()
				) args args2;
				(* unify for top-down inference unless we are expecting Void *)
				begin match follow tr,follow rt with
					| TAbstract({a_path = [],"Void"},_),_ -> ()
					| _,TMono _ -> unify ctx rt tr p
					| _ -> ()
				end
			| TAbstract(a,tl) ->
				loop (Abstract.get_underlying_type a tl)
			| _ -> ())
		in
		loop t
	| NoValue ->
		if name = None then display_error ctx "Unnamed lvalue functions are not supported" p
	| _ ->
		());
	let ft = TFun (fun_args args,rt) in
	let v = (match v with
		| None -> None
		| Some v ->
			if v.[0] = '$' then display_error ctx "Variable names starting with a dollar are not allowed" p;
			Some (add_local ctx v ft p) (* TODO: var pos *)
	) in
	let curfun = match ctx.curfun with
		| FunStatic -> FunStatic
		| FunMemberAbstract -> FunMemberAbstractLocal
		| _ -> FunMemberClassLocal
	in
	let e , fargs = TypeloadFunction.type_function ctx args rt curfun f ctx.in_display p in
	ctx.type_params <- old_tp;
	ctx.in_loop <- old_in_loop;
	let f = {
		tf_args = fargs;
		tf_type = rt;
		tf_expr = e;
	} in
	let e = mk (TFunction f) ft p in
	(match v with
	| None -> e
	| Some v ->
		let open LocalUsage in
		if params <> [] || inline then v.v_extra <- Some (params,if inline then Some e else None);
		let rec loop = function
			| LocalUsage.Block f | LocalUsage.Loop f | LocalUsage.Function f -> f loop
			| LocalUsage.Use v2 | LocalUsage.Assign v2 when v == v2 -> raise Exit
			| LocalUsage.Use _ | LocalUsage.Assign _ | LocalUsage.Declare _ -> ()
		in
		let is_rec = (try local_usage loop e; false with Exit -> true) in
		let decl = (if is_rec then begin
			if inline then display_error ctx "Inline function cannot be recursive" e.epos;
			let vnew = add_local ctx v.v_name ft v.v_pos in
			mk (TVar (vnew,Some (mk (TBlock [
				mk (TVar (v,Some (mk (TConst TNull) ft p))) ctx.t.tvoid p;
				mk (TBinop (OpAssign,mk (TLocal v) ft p,e)) ft p;
				mk (TLocal v) ft p
			]) ft p))) ctx.t.tvoid p
		end else if inline && not ctx.in_display then
			mk (TBlock []) ctx.t.tvoid p (* do not add variable since it will be inlined *)
		else
			mk (TVar (v,Some e)) ctx.t.tvoid p
		) in
		if with_type <> NoValue && not inline then mk (TBlock [decl;mk (TLocal v) v.v_type p]) v.v_type p else decl)

and type_array_decl ctx el with_type p =
	let tp = (match with_type with
	| WithType t ->
		let rec loop seen t =
			(match follow t with
			| TInst ({ cl_path = [],"Array" },[tp]) ->
				(match follow tp with
				| TMono _ -> None
				| _ -> Some tp)
			| TAnon _ ->
				(try
					Some (get_iterable_param t)
				with Not_found ->
					None)
			| TAbstract (a,pl) as t when not (List.exists (fun t' -> fast_eq t t') seen) ->
				(match List.fold_left (fun acc t -> match loop (t :: seen) t with None -> acc | Some t -> t :: acc) [] (get_abstract_froms a pl) with
				| [t] -> Some t
				| _ -> None)
			| t ->
				if t == t_dynamic then Some t else None)
		in
		loop [] t
	| _ ->
		None
	) in
	(match tp with
	| None ->
		let el = List.map (fun e -> type_expr ctx e Value) el in
		let t = try
			unify_min_raise ctx.com.basic el
		with Error (Unify l,p) ->
			if ctx.untyped || ctx.com.display.dms_error_policy = EPIgnore then t_dynamic else begin
				display_error ctx "Arrays of mixed types are only allowed if the type is forced to Array<Dynamic>" p;
				raise (Error (Unify l, p))
			end
		in
		mk (TArrayDecl el) (ctx.t.tarray t) p
	| Some t ->
		let el = List.map (fun e ->
			let e = type_expr ctx e (WithType t) in
			AbstractCast.cast_or_unify ctx t e p;
		) el in
		mk (TArrayDecl el) (ctx.t.tarray t) p)

and type_array_comprehension ctx e with_type p =
	let v = gen_local ctx (mk_mono()) p in
	let et = ref (EConst(Ident "null"),p) in
	let rec map_compr (e,p) =
		match e with
		| EFor(it,e2) -> (EFor (it, map_compr e2),p)
		| EWhile(cond,e2,flag) -> (EWhile (cond,map_compr e2,flag),p)
		| EIf (cond,e2,None) -> (EIf (cond,map_compr e2,None),p)
		| EBlock [e] -> (EBlock [map_compr e],p)
		| EBlock el -> begin match List.rev el with
			| e :: el -> (EBlock ((List.rev el) @ [map_compr e]),p)
			| [] -> e,p
			end
		| EParenthesis e2 -> (EParenthesis (map_compr e2),p)
		| EBinop(OpArrow,a,b) ->
			et := (ENew(({tpackage=["haxe";"ds"];tname="Map";tparams=[];tsub=None},null_pos),[]),p);
			(ECall ((EField ((EConst (Ident v.v_name),p),"set"),p),[a;b]),p)
		| _ ->
			et := (EArrayDecl [],p);
			(ECall ((EField ((EConst (Ident v.v_name),p),"push"),p),[(e,p)]),p)
	in
	let e = map_compr e in
	let ea = type_expr ctx !et with_type in
	unify ctx v.v_type ea.etype p;
	let efor = type_expr ctx e NoValue in
	mk (TBlock [
		mk (TVar (v,Some ea)) ctx.t.tvoid p;
		efor;
		mk (TLocal v) v.v_type p;
	]) v.v_type p

and type_return ctx e p =
	match e with
	| None ->
		let v = ctx.t.tvoid in
		unify ctx v ctx.ret p;
		mk (TReturn None) t_dynamic p
	| Some e ->
		try
			let e = type_expr ctx e (WithType ctx.ret) in
			let e = AbstractCast.cast_or_unify ctx ctx.ret e p in
			begin match follow e.etype with
			| TAbstract({a_path=[],"Void"},_) ->
				(* if we get a Void expression (e.g. from inlining) we don't want to return it (issue #4323) *)
				mk (TBlock [
					e;
					mk (TReturn None) t_dynamic p
				]) t_dynamic e.epos;
			| _ ->
				mk (TReturn (Some e)) t_dynamic p
			end
		with Error(err,p) ->
			check_error ctx err p;
			(* If we have a bad return, let's generate a return null expression at least. This surpresses various
				follow-up errors that come from the fact that the function no longer has a return expression (issue #6445). *)
			let e_null = mk (TConst TNull) (mk_mono()) p in
			mk (TReturn (Some e_null)) t_dynamic p

and type_cast ctx e t p =
	let t = Typeload.load_complex_type ctx true p t in
	let check_param pt = match follow pt with
		| TMono _ -> () (* This probably means that Dynamic wasn't bound (issue #4675). *)
		| t when t == t_dynamic -> ()
		| _ ->error "Cast type parameters must be Dynamic" p
	in
	let rec loop t = match follow t with
		| TInst (_,params) | TEnum (_,params) ->
			List.iter check_param params;
			(match follow t with
			| TInst (c,_) ->
				(match c.cl_kind with KTypeParameter _ -> error "Can't cast to a type parameter" p | _ -> ());
				TClassDecl c
			| TEnum (e,_) -> TEnumDecl e
			| _ -> assert false);
		| TAbstract (a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
			List.iter check_param params;
			TAbstractDecl a
		| TAbstract (a,params) ->
			loop (Abstract.get_underlying_type a params)
		| _ ->
			error "Cast type must be a class or an enum" p
	in
	let texpr = loop t in
	mk (TCast (type_expr ctx e Value,Some texpr)) t p

and type_if ctx e e1 e2 with_type p =
	let e = type_expr ctx e Value in
	let e = AbstractCast.cast_or_unify ctx ctx.t.tbool e p in
	let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
	(match e2 with
	| None ->
		mk (TIf (e,e1,None)) ctx.t.tvoid p
	| Some e2 ->
		let e2 = type_expr ctx (Expr.ensure_block e2) with_type in
		let e1,e2,t = match with_type with
			| NoValue -> e1,e2,ctx.t.tvoid
			| Value -> e1,e2,unify_min ctx [e1; e2]
			| WithType t when (match follow t with TMono _ -> true | _ -> false) -> e1,e2,unify_min ctx [e1; e2]
			| WithType t ->
				let e1 = AbstractCast.cast_or_unify ctx t e1 e1.epos in
				let e2 = AbstractCast.cast_or_unify ctx t e2 e2.epos in
				e1,e2,t
		in
		mk (TIf (e,e1,Some e2)) t p)

and type_meta ctx m e1 with_type p =
	if ctx.is_display_file then DisplayEmitter.check_display_metadata ctx [m];
	let old = ctx.meta in
	ctx.meta <- m :: ctx.meta;
	let e () = type_expr ctx e1 with_type in
	let e = match m with
		| (Meta.ToString,_,_) ->
			let e = e() in
			(match follow e.etype with
				| TAbstract({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics -> call_to_string ctx e
				| _ -> e)
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
			let id = match e1 with (EConst (Int s),_) -> int_of_string s | _ -> assert false in
			MacroContext.get_stored_typed_expr ctx.com id
		| (Meta.NoPrivateAccess,_,_) ->
			ctx.meta <- List.filter (fun(m,_,_) -> m <> Meta.PrivateAccess) ctx.meta;
			e()
		| (Meta.Fixed,_,_) when ctx.com.platform=Cpp ->
			let e = e() in
			{e with eexpr = TMeta(m,e)}
		| _ -> e()
	in
	ctx.meta <- old;
	e

and type_call ctx e el (with_type:with_type) p =
	let def () =
		let e = maybe_type_against_enum ctx (fun () -> type_access ctx (fst e) (snd e) MCall) with_type true p in
		let e = build_call ctx e el with_type p in
		e
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
			let e = type_expr ctx e Value in
			let infos = type_expr ctx infos Value in
			let e = match follow e.etype with
				| TAbstract({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
					call_to_string ctx e
				| _ ->
					e
			in
			let e_trace = mk (TIdent "`trace") t_dynamic p in
			mk (TCall (e_trace,[e;infos])) ctx.t.tvoid p
		else
			type_expr ctx (ECall ((EField ((EField ((EConst (Ident "haxe"),p),"Log"),p),"trace"),p),[mk_to_string_meta e;infos]),p) NoValue
	| (EField ((EConst (Ident "super"),_),_),_), _ ->
		def()
	| (EField (e,"bind"),p), args ->
		let e = type_expr ctx e Value in
		(match follow e.etype with
			| TFun signature -> type_bind ctx e signature args p
			| _ -> def ())
	| (EConst (Ident "$type"),_) , [e] ->
		let e = type_expr ctx e Value in
		ctx.com.warning (s_type (print_context()) e.etype) e.epos;
		let e = Diagnostics.secure_generated_code ctx e in
		e
	| (EField(e,"match"),p), [epat] ->
		let et = type_expr ctx e Value in
		(match follow et.etype with
			| TEnum _ ->
				let e = Matcher.Match.match_expr ctx e [[epat],None,Some (EConst(Ident "true"),p),p] (Some (Some (EConst(Ident "false"),p),p)) (WithType ctx.t.tbool) p in
				(* TODO: add that back *)
(* 				let locals = !get_pattern_locals_ref ctx epat t in
				PMap.iter (fun _ (_,p) -> display_error ctx "Capture variables are not allowed" p) locals; *)
				e
			| _ -> def ())
	| (EConst (Ident "__unprotect__"),_) , [(EConst (String _),_) as e] ->
		let e = type_expr ctx e Value in
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
				let el,_,_ = unify_field_call ctx (FInstance(c,params,f)) el args r p false in
				el
			| _ ->
				error "Constructor is not a function" p
			) in
			el , TInst (c,params)
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) ctx.t.tvoid p
	| _ ->
		def ()

and type_expr ctx (e,p) (with_type:with_type) =
	match e with
	| EField ((EConst (String s),ps),"code") ->
		if UTF8.length s <> 1 then error "String must be a single UTF8 char" ps;
		mk (TConst (TInt (Int32.of_int (UChar.code (UTF8.get s 0))))) ctx.t.tint p
	| EField(_,n) when n.[0] = '$' ->
		error "Field names starting with $ are not allowed" p
	| EConst (Ident s) ->
		if s = "super" && with_type <> NoValue && not ctx.in_display then error "Cannot use super as value" p;
		let e = maybe_type_against_enum ctx (fun () -> type_ident ctx s p MGet) with_type false p in
		acc_get ctx e p
	| EField _
	| EArray _ ->
		acc_get ctx (type_access ctx e p MGet) p
	| EConst (Regexp (r,opt)) ->
		let str = mk (TConst (TString r)) ctx.t.tstring p in
		let opt = mk (TConst (TString opt)) ctx.t.tstring p in
		let t = Typeload.load_core_type ctx "EReg" in
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[str;opt])) t p
	| EConst (String s) when s <> "" && Lexer.is_fmt_string p ->
		type_expr ctx (format_string ctx s p) with_type
	| EConst c ->
		Texpr.type_constant ctx.com.basic c p
	| EBinop (op,e1,e2) ->
		type_binop ctx op e1 e2 false with_type p
	| EBlock [] when with_type <> NoValue ->
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
		type_array_decl ctx el with_type p
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
		let cond = type_expr ctx cond Value in
		let cond = AbstractCast.cast_or_unify ctx ctx.t.tbool cond p in
		ctx.in_loop <- true;
		let e = type_expr ctx (Expr.ensure_block e) NoValue in
		ctx.in_loop <- old_loop;
		mk (TWhile (cond,e,NormalWhile)) ctx.t.tvoid p
	| EWhile (cond,e,DoWhile) ->
		let old_loop = ctx.in_loop in
		ctx.in_loop <- true;
		let e = type_expr ctx (Expr.ensure_block e) NoValue in
		ctx.in_loop <- old_loop;
		let cond = type_expr ctx cond Value in
		let cond = AbstractCast.cast_or_unify ctx ctx.t.tbool cond cond.epos in
		mk (TWhile (cond,e,DoWhile)) ctx.t.tvoid p
	| ESwitch (e1,cases,def) ->
		let wrap e1 = mk (TMeta((Meta.Ast,[e,p],p),e1)) e1.etype e1.epos in
		let e = Matcher.Match.match_expr ctx e1 cases def with_type p in
		wrap e
	| EReturn e ->
		type_return ctx e p
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
		let e = type_expr ctx e Value in
		mk (TThrow e) (mk_mono()) p
	| ECall (e,el) ->
		type_call ctx e el with_type p
	| ENew (t,el) ->
		type_new ctx t el with_type p
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e p
	| EFunction (name,f) ->
		type_local_function ctx name f with_type p
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
		let e = type_expr ctx e Value in
		mk (TCast (e,None)) (mk_mono()) p
	| ECast (e, Some t) ->
		type_cast ctx e t p
	| EDisplay (e,dk) ->
		TyperDisplay.handle_edisplay ctx e dk with_type
	| EDisplayNew t ->
		assert false
	| ECheckType (e,t) ->
		let t = Typeload.load_complex_type ctx true p t in
		let e = type_expr ctx e (WithType t) in
		let e = AbstractCast.cast_or_unify ctx t e p in
		if e.etype == t then e else mk (TCast (e,None)) t p
	| EMeta (m,e1) ->
		type_meta ctx m e1 with_type p

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
			delayed_macros = DynArray.create();
			doinline = com.display.dms_inline && not (Common.defined com Define.NoInline);
			hook_generate = [];
			get_build_infos = (fun() -> None);
			std = null_module;
			global_using = [];
			do_inherit = MagicTypes.on_inherit;
			do_create = create;
			do_macro = MacroContext.type_macro;
			do_load_module = TypeloadModule.load_module;
			do_load_type_def = Typeload.load_type_def;
			do_optimize = Optimizer.reduce_expression;
			do_build_instance = InstanceBuilder.build_instance;
			do_format_string = format_string;
			do_finalize = Finalization.finalize;
			do_generate = Finalization.generate;
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
		meta = [];
		this_stack = [];
		with_type_stack = [];
		call_argument_stack = [];
		pass = PBuildModule;
		macro_depth = 0;
		untyped = false;
		curfun = FunStatic;
		in_loop = false;
		in_display = false;
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
		on_error = (fun ctx msg p -> ctx.com.error msg p);
	} in
	ctx.g.std <- (try
		TypeloadModule.load_module ctx ([],"StdTypes") null_pos
	with
		Error (Module_not_found ([],"StdTypes"),_) -> error "Standard library not found" null_pos
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
	(match m.m_types with
	| [TClassDecl c] -> ctx.t.tstring <- TInst (c,[])
	| _ -> assert false);
	let m = TypeloadModule.load_module ctx ([],"Array") null_pos in
	(try
		List.iter (fun t -> (
			match t with
			| TClassDecl ({cl_path = ([],"Array")} as c) ->
				ctx.t.tarray <- (fun t -> TInst (c,[t]));
				raise Exit
			| _ -> ()
		)) m.m_types;
		assert false
	with Exit -> ());
	let m = TypeloadModule.load_module ctx (["haxe"],"EnumTools") null_pos in
	(match m.m_types with
	| [TClassDecl c1;TClassDecl c2] -> ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
	| [TClassDecl c1] ->
		let m = TypeloadModule.load_module ctx (["haxe"],"EnumValueTools") null_pos in
		(match m.m_types with
		| [TClassDecl c2 ] -> ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
		| _ -> assert false);
	| _ -> assert false);
	ctx

;;
unify_min_ref := unify_min;
make_call_ref := make_call;
build_call_ref := build_call;
merge_core_doc_ref := merge_core_doc;
type_block_ref := type_block