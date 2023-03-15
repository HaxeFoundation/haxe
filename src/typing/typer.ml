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
open CallUnification
open Calls
open Operators

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

let mono_or_dynamic ctx with_type p = match with_type with
	| WithType.NoValue ->
		t_dynamic
	| Value _ | WithType _ ->
		spawn_monomorph ctx p

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
				| TAbstract ({a_impl = Some c} as a,_) when a.a_enum ->
					let fields = ExtList.List.filter_map (fun cf ->
						if has_class_field_flag cf CfEnum then Some cf.cf_name else None
					) c.cl_ordered_statics in
					false,a.a_path,fields,TAbstractDecl a
				| TAbstract (a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					begin match get_abstract_froms ctx a pl with
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
							AKExpr e
					end
				| _ -> e (* ??? *)
			end
		| _ ->
			raise Exit
		end
	with Exit ->
		f()

let check_error ctx err p = match err with
	| Module_not_found ([],name) when Diagnostics.error_in_diagnostics_run ctx.com p ->
		DisplayToplevel.handle_unresolved_identifier ctx name p true
	| _ ->
		display_error ctx.com (error_msg err) p

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
				raise_typing_error (Unify l) (List.nth el index).epos
			end

let unify_min ctx el =
	try unify_min_raise ctx el
	with Error (Unify l,p) ->
		if not ctx.untyped then display_error ctx.com (error_msg (Unify l)) p;
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
		let acc = AKExpr (mk (TConst (TBool true)) ctx.t.tbool p) in
		if mode = MGet then
			acc
		else
			AKNo(acc,p)
	| "false" ->
		let acc = AKExpr (mk (TConst (TBool false)) ctx.t.tbool p) in
		if mode = MGet then
			acc
		else
			AKNo(acc,p)
	| "this" ->
		let acc = AKExpr(get_this ctx p) in
		begin match mode with
		| MSet _ ->
			add_class_field_flag ctx.curfield CfModifiesThis;
			begin match ctx.curclass.cl_kind with
			| KAbstractImpl _ ->
				if not (assign_to_this_is_allowed ctx) then
					typing_error "Abstract 'this' value can only be modified inside an inline function" p;
				acc
			| _ ->
				AKNo(acc,p)
			end
		| MCall _ ->
			begin match ctx.curclass.cl_kind with
			| KAbstractImpl _ ->
				acc
			| _ ->
				AKNo(acc,p)
			end
		| MGet ->
			acc
		end;
	| "abstract" ->
		begin match mode, ctx.curclass.cl_kind with
			| MSet _, KAbstractImpl ab -> typing_error "Property 'abstract' is read-only" p;
			| (MGet, KAbstractImpl ab)
			| (MCall _, KAbstractImpl ab) ->
				let tl = extract_param_types ab.a_params in
				let e = get_this ctx p in
				let e = {e with etype = TAbstract (ab,tl)} in
				AKExpr e
			| _ ->
				typing_error "Property 'abstract' is reserved and only available in abstracts" p
		end
	| "super" ->
		let t = (match ctx.curclass.cl_super with
			| None -> typing_error "Current class does not have a superclass" p
			| Some (c,params) -> TInst(c,params)
		) in
		(match ctx.curfun with
		| FunMember | FunConstructor -> ()
		| FunMemberAbstract -> typing_error "Cannot access super inside an abstract function" p
		| FunStatic -> typing_error "Cannot access super inside a static function" p;
		| FunMemberClassLocal | FunMemberAbstractLocal -> typing_error "Cannot access super inside a local function" p);
		AKExpr (mk (TConst TSuper) t p)
	| "null" ->
		let acc =
			(* Hack for #10787 *)
			if ctx.com.platform = Cs then
				AKExpr (null (spawn_monomorph ctx p) p)
			else begin
				let tnull () = ctx.t.tnull (spawn_monomorph ctx p) in
				let t = match with_type with
					| WithType.WithType(t,_) ->
						begin match follow t with
						| TMono r ->
							(* If our expected type is a monomorph, bind it to Null<?>. *)
							Monomorph.do_bind r (tnull())
						| _ ->
							(* Otherwise there's no need to create a monomorph, we can just type the null literal
							the way we expect it. *)
							()
						end;
						t
					| _ ->
						tnull()
				in
				AKExpr (null t p)
			end
		in
		if mode = MGet then acc else AKNo(acc,p)
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
					| MSet _ -> typing_error "Cannot set inline closure" p
					| MGet -> typing_error "Cannot create closure on inline closure" p
					| MCall _ ->
						(* create a fake class with a fake field to emulate inlining *)
						let c = mk_class ctx.m.curmod (["local"],v.v_name) e.epos null_pos in
						let cf = { (mk_field v.v_name v.v_type e.epos null_pos) with cf_params = params; cf_expr = Some e; cf_kind = Method MethInline } in
						add_class_flag c CExtern;
						c.cl_fields <- PMap.add cf.cf_name cf PMap.empty;
						let e = mk (TConst TNull) (TInst (c,[])) p in
						AKField (FieldAccess.create e cf (FHInstance(c,[])) true p)
				end
			| _ ->
				AKExpr (mk (TLocal v) t p))
		| _ ->
			AKExpr (mk (TLocal v) v.v_type p))
	with Not_found -> try
		(* member variable lookup *)
		if ctx.curfun = FunStatic then raise Not_found;
		let c , t , f = class_field ctx ctx.curclass (extract_param_types ctx.curclass.cl_params) i p in
		field_access ctx mode f (match c with None -> FHAnon | Some (c,tl) -> FHInstance (c,tl)) (get_this ctx p) p
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		let is_impl = has_class_field_flag f CfImpl in
		let is_enum = has_class_field_flag f CfEnum in
		if is_impl && not (has_class_field_flag ctx.curfield CfImpl) && not is_enum then
			typing_error (Printf.sprintf "Cannot access non-static field %s from static method" f.cf_name) p;
		let e,fa = match ctx.curclass.cl_kind with
			| KAbstractImpl a when is_impl && not is_enum ->
				let tl = extract_param_types a.a_params in
				let e = get_this ctx p in
				let e = {e with etype = TAbstract(a,tl)} in
				e,FHAbstract(a,tl,ctx.curclass)
			| _ ->
				let e = type_type ctx ctx.curclass.cl_path p in
				e,FHStatic ctx.curclass
		in
		field_access ctx mode f fa e p
	with Not_found -> try
		(* module-level statics *)
		(match ctx.m.curmod.m_statics with
		| None -> raise Not_found
		| Some c ->
			let f = PMap.find i c.cl_statics in
			let e = type_module_type ctx (TClassDecl c) None p in
			field_access ctx mode f (FHStatic c) e p
		)
	with Not_found -> try
		let wrap e =
			let acc = AKExpr e in
			if is_set then
				AKNo(acc,p)
			else
				acc
		in
		(* lookup imported enums *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| (t,pt) :: l ->
				match t with
				| TAbstractDecl ({a_impl = Some c} as a) when a.a_enum ->
					begin try
						let cf = PMap.find i c.cl_statics in
						if not (has_class_field_flag cf CfEnum) then
							loop l
						else begin
							let et = type_module_type ctx (TClassDecl c) None p in
							let inline = match cf.cf_kind with
								| Var {v_read = AccInline} -> true
								|  _ -> false
							in
							let fa = FieldAccess.create et cf (FHAbstract(a,extract_param_types a.a_params,c)) inline p in
							ImportHandling.mark_import_position ctx pt;
							AKField fa
						end
					with Not_found ->
						loop l
					end
				| TClassDecl _ | TAbstractDecl _ ->
					loop l
				| TTypeDecl t ->
					(match follow t.t_type with
					| TEnum (e,_) -> loop ((TEnumDecl e,pt) :: l)
					| TAbstract (a,_) when a.a_enum -> loop ((TAbstractDecl a,pt) :: l)
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
		(try loop (List.rev_map (fun t -> t,null_pos) ctx.m.curmod.m_types) with Not_found -> loop ctx.m.module_imports)
	with Not_found ->
		(* lookup imported globals *)
		let t, name, pi = PMap.find i ctx.m.module_globals in
		ImportHandling.mark_import_position ctx pi;
		let e = type_module_type ctx t None p in
		type_field_default_cfg ctx e name p mode with_type

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
			let t = List.find (fun tp -> tp.ttp_name = i) ctx.type_params in
			resolved_to_type_parameter := true;
			let c = match follow (extract_param_type t) with TInst(c,_) -> c | _ -> die "" __LOC__ in
			if TypeloadCheck.is_generic_parameter ctx c && Meta.has Meta.Const c.cl_meta then begin
				let e = type_module_type ctx (TClassDecl c) None p in
				AKExpr {e with etype = (extract_param_type t)}
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
				if ctx.curfun = FunStatic && PMap.mem i ctx.curclass.cl_fields then typing_error ("Cannot access " ^ i ^ " in static function") p;
				if !resolved_to_type_parameter then begin
					display_error ctx.com ("Only @:const type parameters on @:generic classes can be used as value") p;
					AKExpr (mk (TConst TNull) t_dynamic p)
				end else begin
					let err = Unknown_ident i in
					if ctx.in_display then begin
						raise (Error (err,p))
					end;
					if Diagnostics.error_in_diagnostics_run ctx.com p then begin
						DisplayToplevel.handle_unresolved_identifier ctx i p false;
						DisplayFields.handle_missing_ident ctx i mode with_type p;
						let t = mk_mono() in
						AKExpr (mk (TIdent i) t p)
					end else match ctx.com.display.dms_kind with
						| DMNone ->
							raise (Error(err,p))
						| _ ->
							display_error ctx.com (error_msg err) p;
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
		let {name = name; pos = p} = first in
		try
			(* first, try to resolve the first ident in the chain and access its fields.
			   this doesn't support untyped identifiers yet, because we want to check fully-qualified
			   paths first (even in an untyped block) *)
			field_chain ctx pnext (type_ident_raise ctx name p MGet WithType.value)
		with Not_found ->
			(* first ident couldn't be resolved, it's probably a fully qualified path - resolve it *)
			let path = (first :: pnext) in
			try
				resolve_dot_path ctx path mode with_type
			with Not_found ->
				(* dot-path resolution failed, it could be an untyped field access that happens to look like a dot-path, e.g. `untyped __global__.String` *)
				try
					(* TODO: we don't really want to do full type_ident again, just the second part of it *)
					field_chain ctx pnext (type_ident ctx name p MGet WithType.value)
				with Error (Unknown_ident _,p2) as e when p = p2 ->
					try
						(* try raising a more sensible error if there was an uppercase-first (module name) part *)
						begin
							(* TODO: we should pass the actual resolution error from resolve_dot_path instead of Not_found *)
							let rec loop pack_acc first_uppercase path =
								match path with
								| {name = name; case = PLowercase} :: rest ->
									(match first_uppercase with
									| None -> loop (name :: pack_acc) None rest
									| Some (n,p) -> List.rev pack_acc, n, None, p)
								| {name = name; case = PUppercase; pos = p} :: rest ->
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
							if ctx.com.module_lut#mem mpath then
								let tname = Option.default name sub in
								raise (Error (Type_not_found (mpath,tname,Not_defined),p))
							else
								raise (Error (Module_not_found mpath,p))
						end
					with Not_found ->
						(* if there was no module name part, last guess is that we're trying to get package completion *)
						if ctx.in_display then begin
							let sl = List.map (fun part -> part.name) path in
							if is_legacy_completion ctx.com then
								raise (Parser.TypePath (sl,None,false,p))
							else
								DisplayToplevel.collect_and_raise ctx TKType WithType.no_value (CRToplevel None) (String.concat "." sl,p0) p0
						end;
						raise e
	in

	(* loop through the given EField expression to figure out whether it's a dot-path that we have to resolve,
	   or a field access chain *)
	let rec loop dot_path_acc (e,p) =
		match e with
		| EField (e,s,EFNormal) ->
			(* field access - accumulate and check further *)
			loop ((mk_dot_path_part s p) :: dot_path_acc) e
		| EConst (Ident i) ->
			(* it's a dot-path, so it might be either fully-qualified access (pack.Class.field)
			   or normal field access of a local/global/field identifier, proceed figuring this out *)
			dot_path (mk_dot_path_part i p) dot_path_acc mode with_type
		| EField ((eobj,pobj),s,EFSafe) ->
			(* safe navigation field access - definitely NOT a fully-qualified access,
			   create safe navigation chain from the object expression *)
			let acc_obj = type_access ctx eobj pobj MGet WithType.value in
			let eobj = acc_get ctx acc_obj in
			let eobj, tempvar = match (Texpr.skip eobj).eexpr with
				| TLocal _ | TTypeExpr _ | TConst _ ->
					eobj, None
				| _ ->
					let v = alloc_var VGenerated "tmp" eobj.etype eobj.epos in
					let temp_var = mk (TVar(v, Some eobj)) ctx.t.tvoid v.v_pos in
					let eobj = mk (TLocal v) v.v_type v.v_pos in
					eobj, Some temp_var
			in
			let access = field_chain ctx ((mk_dot_path_part s p) :: dot_path_acc) (AKExpr eobj) mode with_type in
			AKSafeNav {
				sn_pos = p;
				sn_base = eobj;
				sn_temp_var = tempvar;
				sn_access = access;
			}
		| _ ->
			(* non-ident expr occured: definitely NOT a fully-qualified access,
			   resolve the field chain against this expression *)
			(match (type_access ctx e p MGet WithType.value) with
			| AKSafeNav sn ->
				(* further field access continues the safe navigation chain (after a non-field access inside the chain) *)
				AKSafeNav { sn with sn_access = field_chain ctx dot_path_acc sn.sn_access mode with_type }
			| e ->
				field_chain ctx dot_path_acc e mode with_type)
	in
	loop [] (e,p0)

and type_access ctx e p mode with_type =
	match e with
	| EConst (Ident s) ->
		type_ident ctx s p mode with_type
	| EField (e1,"new",efk_todo) ->
		let e1 = type_expr ctx e1 WithType.value in
		begin match e1.eexpr with
			| TTypeExpr (TClassDecl c) ->
				begin match mode with
				| MSet _ -> typing_error "Cannot set constructor" p;
				| MCall _ -> typing_error ("Cannot call constructor like this, use 'new " ^ (s_type_path c.cl_path) ^ "()' instead") p;
				| MGet -> ()
				end;
				let monos = Monomorph.spawn_constrained_monos (fun t -> t) (match c.cl_kind with KAbstractImpl a -> a.a_params | _ -> c.cl_params) in
				let fa = FieldAccess.get_constructor_access c monos p in
				let cf = fa.fa_field in
				no_abstract_constructor c p;
				check_constructor_access ctx c cf p;
				let args = match follow (FieldAccess.get_map_function fa cf.cf_type) with TFun(args,ret) -> args | _ -> die "" __LOC__ in
				let vl = List.map (fun (n,_,t) -> alloc_var VGenerated n t c.cl_pos) args in
				let vexpr v = mk (TLocal v) v.v_type p in
				let el = List.map vexpr vl in
				let ec,t = match c.cl_kind, fa.fa_host with
					| KAbstractImpl a, FHAbstract _ ->
						let t = TAbstract(a,monos) in
						(new call_dispatcher ctx (MCall []) WithType.value p)#field_call fa el [],t
					| KAbstractImpl a, FHInstance (c,pl) ->
						let e_new = mk (TNew(c,monos,el)) (TInst(c,pl)) p in
						let t = TAbstract(a,monos) in
						mk_cast e_new t p, t
					| _ ->
						let t = TInst(c,monos) in
						mk (TNew(c,monos,el)) t p,t
				in
				AKExpr(mk (TFunction {
					tf_args = List.map (fun v -> v,None) vl;
					tf_type = t;
					tf_expr = mk (TReturn (Some ec)) t p;
				}) (TFun ((List.map (fun v -> v.v_name,false,v.v_type) vl),t)) p)
			| _ -> typing_error "Binding new is only allowed on class types" p
		end;
	| EField _ ->
		handle_efield ctx e p mode with_type
	| EArray (e1,e2) ->
		type_array_access ctx e1 e2 p mode
	| ECall (e, el) ->
		type_call_access ctx e el mode with_type None p
	| EDisplay (e,dk) ->
		AKExpr (TyperDisplay.handle_edisplay ctx e dk mode with_type)
	| _ ->
		AKExpr (type_expr ~mode ctx (e,p) with_type)

and type_array_access ctx e1 e2 p mode =
	let e1, p1 = e1 in
	let a1 = type_access ctx e1 p1 MGet WithType.value in
	let e2 = type_expr ctx e2 WithType.value in
	match a1 with
	| AKSafeNav sn ->
		(* pack the array access inside the safe navigation chain *)
		let e1 = acc_get ctx sn.sn_access in
		AKSafeNav { sn with sn_access = Calls.array_access ctx e1 e2 mode p }
	| _ ->
		let e1 = acc_get ctx a1 in
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
					let old_in_loop = ctx.in_loop in
					if ev.ev_static then ctx.in_loop <- false;
					let e = Std.finally (fun () -> ctx.in_loop <- old_in_loop) (type_expr ctx e) (WithType.with_type t) in
					let e = AbstractCast.cast_or_unify ctx t e p in
					Some e
			) in
			let v = add_local_with_origin ctx TVOLocalVariable n t pv in
			v.v_meta <- ev.ev_meta;
			DisplayEmitter.check_display_metadata ctx v.v_meta;
			if ev.ev_final then add_var_flag v VFinal;
			if ev.ev_static then add_var_flag v VStatic;
			if ctx.in_display && DisplayPosition.display_position#enclosed_in pv then
				DisplayEmitter.display_variable ctx v pv;
			v,e
		with
			Error (e,p) ->
				check_error ctx e p;
				add_local ctx VGenerated n t_dynamic pv, None (* TODO: What to do with this... *)
	) vl in
	List.iter (fun (v,_) ->
		delay_if_mono ctx PTypeField v.v_type (fun() ->
			if ExtType.is_void (follow v.v_type) then
				typing_error "Variables of type Void are not allowed" v.v_pos
		)
	) vl;
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
				| g :: _ -> typing_error ("Unclosed " ^ gname) { p with pmin = !pmin + g + 1; pmax = !pmin + g + 2 }
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
					if Lexer.string_is_whitespace scode then typing_error "Expression cannot be empty" ep
					else typing_error msg pos
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
				let froms = get_abstract_froms ctx a pl in
				begin match froms with
				| [] ->
					(* If the abstract has no casts in the first place, we can assume plain typing (issue #10730) *)
					ODKPlain
				| _ ->
					let fold = fun acc t' -> match loop (t :: seen) t' with ODKPlain -> acc | t -> t :: acc in
					begin match List.fold_left fold [] froms with
						| [t] -> t
						| _ -> ODKFailed
					end
				end
			| TDynamic (Some t) ->
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
			if PMap.mem n !fields then typing_error ("Duplicate field in object declaration : " ^ n) p;
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
				if starts_with n '$' then typing_error "Field names starting with a dollar are not allowed" p;
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
			if PMap.mem f acc then typing_error ("Duplicate field in object declaration : " ^ f) p;
			let e = type_expr ctx e (WithType.named_structure_field f) in
			(match follow e.etype with TAbstract({a_path=[],"Void"},_) -> typing_error "Fields of type Void are not allowed in structures" e.epos | _ -> ());
			let cf = mk_field f e.etype (punion pf e.epos) pf in
			if ctx.in_display && DisplayPosition.display_position#enclosed_in pf then DisplayEmitter.display_field ctx Unknown CFSMember cf pf;
			(((f,pf,qs),e) :: l, if is_valid then begin
				if starts_with f '$' then typing_error "Field names starting with a dollar are not allowed" p;
				PMap.add f cf acc
			end else acc)
		in
		let fields , types = List.fold_left loop ([],PMap.empty) fl in
		let x = ref Const in
		ctx.opened <- x :: ctx.opened;
		mk (TObjectDecl (List.rev fields)) (mk_anon ~fields:types x) p
	in
	(match a with
	| ODKPlain | ODKFailed  -> type_plain_fields()
	| ODKWithStructure a when PMap.is_empty a.a_fields && !dynamic_parameter = None -> type_plain_fields()
	| ODKWithStructure a ->
		let t, fl = type_fields a.a_fields in
		mk (TObjectDecl fl) t p
	| ODKWithClass (c,tl) ->
		let fa = FieldAccess.get_constructor_access c tl p in
		let ctor = fa.fa_field in
		let args = match follow (FieldAccess.get_map_function fa ctor.cf_type) with
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
	let unify_constructor_call c fa =
		try
			let fcc = unify_field_call ctx fa [] el p fa.fa_inline in
			check_constructor_access ctx c fcc.fc_field p;
			fcc
		with Error (e,p) ->
			typing_error (error_msg e) p;
	in
	let display_position_in_el () =
		List.exists (fun e -> DisplayPosition.display_position#enclosed_in (pos e)) el
	in
	let t = if (fst path).tparams <> [] then begin
		try
			Typeload.load_instance ctx path false
		with Error _ as exc when display_position_in_el() ->
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
			| TInst({cl_kind = KGeneric } as c,tl) -> follow (Generic.build_generic_class ctx c p tl)
			| _ -> t
		end
	with
	| Generic.Generic_Exception _ ->
		(* Try to infer generic parameters from the argument list (issue #2044) *)
		begin match resolve_typedef (Typeload.load_type_def ctx p (fst path)) with
		| TClassDecl ({cl_constructor = Some cf} as c) ->
			let monos = Monomorph.spawn_constrained_monos (fun t -> t) c.cl_params in
			let fa = FieldAccess.get_constructor_access c monos p in
			no_abstract_constructor c p;
			ignore (unify_constructor_call c fa);
			begin try
				Generic.build_generic_class ctx c p monos
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
			typing_error ((s_type_path (t_infos mt).mt_path) ^ " cannot be constructed") p
		end
	| Error _ as exc when display_position_in_el() ->
		List.iter (fun e -> ignore(type_expr ctx e WithType.value)) el;
		raise exc
	in
	DisplayEmitter.check_display_type ctx t path;
	let t = follow t in
	let build_constructor_call ao c tl =
		let fa = FieldAccess.get_constructor_access c tl p in
		let fa = if force_inline then {fa with fa_inline = true} else fa in
		let cf = fa.fa_field in
		no_abstract_constructor c p;
		begin match cf.cf_kind with
			| Var { v_read = AccRequire (r,msg) } -> (match msg with Some msg -> typing_error msg p | None -> error_require r p)
			| _ -> ()
		end;
		unify_constructor_call c fa
	in
	try begin match Abstract.follow_with_forward_ctor t with
	| TInst ({cl_kind = KTypeParameter tl} as c,params) ->
		if not (TypeloadCheck.is_generic_parameter ctx c) then typing_error "Only generic type parameters can be constructed" p;
 		begin match get_constructible_constraint ctx tl p with
		| None ->
			raise_typing_error (No_constructor (TClassDecl c)) p
		| Some(tl,tr) ->
			let el,_ = unify_call_args ctx el tl tr p false false false in
			mk (TNew (c,params,el)) t p
		end
	| TAbstract({a_impl = Some c} as a,tl) when not (Meta.has Meta.MultiType a.a_meta) ->
		let fcc = build_constructor_call (Some a) c tl in
		{ (fcc.fc_data()) with etype = t }
	| TInst (c,params) | TAbstract({a_impl = Some c},params) ->
		let fcc = build_constructor_call None c params in
		let el = fcc.fc_args in
		mk (TNew (c,params,el)) t p
	| _ ->
		typing_error (s_type (print_context()) t ^ " cannot be constructed") p
	end with Error(No_constructor _ as err,p) when ctx.com.display.dms_kind <> DMNone ->
		display_error ctx.com (error_msg err) p;
		Diagnostics.secure_generated_code ctx (mk (TConst TNull) t p)

and type_try ctx e1 catches with_type p =
	let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
	let rec check_unreachable cases t p = match cases with
		| (v,e) :: cases ->
			let unreachable () =
				display_error ctx.com "This block is unreachable" p;
				let st = s_type (print_context()) in
				display_error ctx.com (Printf.sprintf "%s can be caught to %s, which is handled here" (st t) (st v.v_type)) e.epos
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
			if Abstract.follow_with_abstracts pt != t_dynamic then typing_error "Catch class parameter must be Dynamic" p;
		) params
	in
	let catches,el = List.fold_left (fun (acc1,acc2) ((v,pv),t,e_ast,pc) ->
		let th = Option.default (CTPath { tpackage = ["haxe"]; tname = "Exception"; tsub = None; tparams = [] },null_pos) t in
		let t = Typeload.load_complex_type ctx true th in
		let rec loop t = match follow t with
			| TInst ({ cl_kind = KTypeParameter _} as c,_) when not (TypeloadCheck.is_generic_parameter ctx c) ->
				typing_error "Cannot catch non-generic type parameter" p
			| TInst (_,params) | TEnum (_,params) ->
				check_catch_type_params params (snd th);
				t
			| TAbstract(a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
				check_catch_type_params params (snd th);
				t
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				loop (Abstract.get_underlying_type a tl)
			| TDynamic _ -> t
			| _ -> typing_error "Catch type must be a class, an enum or Dynamic" (pos e_ast)
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
		if ctx.is_display_file && DisplayPosition.display_position#enclosed_in pc then ignore(TyperDisplay.display_expr ctx e_ast e DKMarked MGet with_type pc);
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
			display_error ctx.com "Duplicate key" e_key.epos;
			typing_error (compl_msg "Previously defined here") p
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
			typing_error "Expected a => b" (pos e)
		| _ -> typing_error "Expected a => b" (pos e)
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
		let tkey = unify_min_raise ctx el_k in
		let tval = unify_min_raise ctx el_v in
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
		if name = None then display_error ctx.com "Type parameters not supported in unnamed local functions" p;
		if with_type <> WithType.NoValue then typing_error "Type parameters are not supported for rvalue functions" p
	end;
	let v,pname = (match name with
		| None -> None,p
		| Some (v,pn) -> Some v,pn
	) in
	let old_tp,old_in_loop = ctx.type_params,ctx.in_loop in
	ctx.type_params <- params @ ctx.type_params;
	if not inline then ctx.in_loop <- false;
	let rt = Typeload.load_type_hint ctx p f.f_type in
	let type_arg _ opt t p = Typeload.load_type_hint ~opt ctx p t in
	let args = new FunctionArguments.function_arguments ctx type_arg false ctx.in_display None f.f_args in
	let targs = args#for_type in
	let maybe_unify_arg t1 t2 =
		match follow t1 with
		| TMono _ -> unify ctx t2 t1 p
		| _ -> ()
	in
	let maybe_unify_ret tr = match follow tr,follow rt with
		| TAbstract({a_path = [],"Void"},_),_ when kind <> FKArrow -> ()
		| _,TMono _ -> unify ctx rt tr p
		| _ -> ()
	in
	(* The idea here is: If we have multiple `from Function`, we can
	   1. ignore any that have a different argument arity, and
	   2. still top-down infer any argument or return type that is equal across all candidates.
	*)
	let handle_abstract_matrix l =
		let arity = List.length targs in
		let m = new unification_matrix (arity + 1) in
		let rec loop l = match l with
			| t :: l ->
				begin match follow t with
				| TFun(args,ret) when List.length args = arity ->
					List.iteri (fun i (_,_,t) ->
						(* We don't want to bind monomorphs because we want the widest type *)
						let t = dynamify_monos t in
						m#join t (i + 1);
					) args;
					let ret = dynamify_monos ret in
					m#join ret 0;
				| t ->
					()
				end;
				loop l
			| [] ->
				()
		in
		loop l;
 		List.iteri (fun i (_,_,t1) ->
			match m#get_type (i + 1) with
			| Some t2 ->
				maybe_unify_arg t1 t2
			| None ->
				()
		) targs;
		begin match m#get_type 0 with
		| Some tr ->
			maybe_unify_ret tr
		| None ->
			()
		end
	in
	(match with_type with
	| WithType.WithType(t,_) ->
		let rec loop stack t =
			(match follow t with
			| TFun (args2,tr) when List.length args2 = List.length targs ->
				List.iter2 (fun (_,_,t1) (_,_,t2) ->
					maybe_unify_arg t1 t2
				) targs args2;
				(* unify for top-down inference unless we are expecting Void *)
				maybe_unify_ret tr
			| TAbstract(a,tl) ->
				begin match get_abstract_froms ctx a tl with
					| [t2] ->
						if not (List.exists (shallow_eq t) stack) then loop (t :: stack) t2
					| l ->
						handle_abstract_matrix l
				end
			| _ -> ())
		in
		loop [] t
	| WithType.NoValue ->
		if name = None then display_error ctx.com "Unnamed lvalue functions are not supported" p
	| _ ->
		());
	let ft = TFun (targs,rt) in
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
	let e = TypeloadFunction.type_function ctx args rt curfun f.f_expr ctx.in_display p in
	ctx.type_params <- old_tp;
	ctx.in_loop <- old_in_loop;
	let tf = {
		tf_args = args#for_expr;
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
				if inline then display_error ctx.com "Inline function cannot be recursive" e.epos;
				(mk (TVar (v,Some (mk (TConst TNull) ft p))) ctx.t.tvoid p) ::
				(mk (TBinop (OpAssign,mk (TLocal v) ft p,e)) ft p) ::
				exprs
			end else if inline && not ctx.is_display_file then
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
						(get_abstract_froms ctx a pl)
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
			if !allow_array_dynamic || ctx.untyped || ignore_error ctx.com then
				t_dynamic
			else begin
				display_error ctx.com "Arrays of mixed types are only allowed if the type is forced to Array<Dynamic>" p;
				raise (Error (Unify l, p))
			end
		in
		mk (TArrayDecl el) (ctx.t.tarray t) p
	| Some t ->
		let el = List.map (fun e ->
			let e = type_expr ctx e (WithType.with_type t) in
			AbstractCast.cast_or_unify ctx t e e.epos;
		) el in
		mk (TArrayDecl el) (ctx.t.tarray t) p)

and type_array_comprehension ctx e with_type p =
	let v = gen_local ctx (spawn_monomorph ctx p) p in
	let ev = mk (TLocal v) v.v_type p in
	let e_ref = snd (store_typed_expr ctx.com ev p) in
	let et = ref (EConst(Ident "null"),p) in
	let comprehension_pos = p in
	let rec map_compr (e,p) =
		match e with
		| EFor(it,e2) -> (EFor (it, map_compr e2),p)
		| EWhile(cond,e2,flag) -> (EWhile (cond,map_compr e2,flag),p)
		| EIf (cond,e2,None) -> (EIf (cond,map_compr e2,None),p)
		| EIf (cond,e2,Some e3) -> (EIf (cond,map_compr e2,Some (map_compr e3)),p)
		| EBlock [e] -> (EBlock [map_compr e],p)
		| EBlock [] -> map_compr (EObjectDecl [],p)
		| EBlock el -> begin match List.rev el with
			| e :: el -> (EBlock ((List.rev el) @ [map_compr e]),p)
			| [] -> e,p
			end
		| EParenthesis e2 -> (EParenthesis (map_compr e2),p)
		| EBinop(OpArrow,a,b) ->
			et := (ENew(({tpackage=["haxe";"ds"];tname="Map";tparams=[];tsub=None},null_pos),[]),comprehension_pos);
			(ECall ((efield (e_ref,"set"),p),[a;b]),p)
		| _ ->
			et := (EArrayDecl [],comprehension_pos);
			(ECall ((efield (e_ref,"push"),p),[(e,p)]),p)
	in
	let e = map_compr e in
	let ea = type_expr ctx !et with_type in
	unify ctx v.v_type ea.etype p;
	let efor = type_expr ctx e WithType.NoValue in
	mk (TBlock [
		mk (TVar (v,Some ea)) ctx.t.tvoid p;
		efor;
		ev;
	]) v.v_type p

and type_return ?(implicit=false) ctx e with_type p =
	let is_abstract_ctor = ctx.curfun = FunMemberAbstract && ctx.curfield.cf_name = "_new" in
	match e with
	| None when is_abstract_ctor ->
		let e_cast = mk (TCast(get_this ctx p,None)) ctx.ret p in
		mk (TReturn (Some e_cast)) (mono_or_dynamic ctx with_type p) p
	| None ->
		let v = ctx.t.tvoid in
		unify ctx v ctx.ret p;
		let expect_void = match with_type with
			| WithType.WithType(t,_) -> ExtType.is_void (follow t)
			| WithType.Value (Some ImplicitReturn) -> true
			| _ -> false
		in
		mk (TReturn None) (if expect_void then v else (mono_or_dynamic ctx with_type p)) p
	| Some e ->
		if is_abstract_ctor then begin
			match fst e with
			| ECast((EConst(Ident "this"),_),None) -> ()
			| _ -> display_error ctx.com "Cannot return a value from constructor" p
		end;
		try
			let with_expected_type =
				if ExtType.is_void (follow ctx.ret) then WithType.no_value
				else if implicit then WithType.of_implicit_return ctx.ret
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
					| TConst TNull -> typing_error "Cannot return `null` from Void-function" p
					| _ -> ()
					end;
					(* if we get a Void expression (e.g. from inlining) we don't want to return it (issue #4323) *)
					let t = mono_or_dynamic ctx with_type p in
					mk (TBlock [
						e;
						mk (TReturn None) t p
					]) t e.epos;
				| _ ->
					mk (TReturn (Some e)) (mono_or_dynamic ctx with_type p) p
		with Error(err,p) ->
			check_error ctx err p;
			(* If we have a bad return, let's generate a return null expression at least. This surpresses various
				follow-up errors that come from the fact that the function no longer has a return expression (issue #6445). *)
			let e_null = mk (TConst TNull) (mk_mono()) p in
			mk (TReturn (Some e_null)) (mono_or_dynamic ctx with_type p) p

and type_cast ctx e t p =
	let tpos = pos t in
	let t = Typeload.load_complex_type ctx true t in
	let check_param pt = match follow pt with
		| TMono _ -> () (* This probably means that Dynamic wasn't bound (issue #4675). *)
		| t when t == t_dynamic -> ()
		| _ -> typing_error "Cast type parameters must be Dynamic" tpos
	in
	let rec loop t = match follow t with
		| TInst (_,params) | TEnum (_,params) ->
			List.iter check_param params;
			(match follow t with
			| TInst (c,_) ->
				(match c.cl_kind with KTypeParameter _ -> typing_error "Can't cast to a type parameter" tpos | _ -> ());
				TClassDecl c
			| TEnum (e,_) -> TEnumDecl e
			| _ -> die "" __LOC__);
		| TAbstract (a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
			List.iter check_param params;
			TAbstractDecl a
		| TAbstract (a,params) ->
			loop (Abstract.get_underlying_type a params)
		| _ ->
			typing_error "Cast type must be a class or an enum" tpos
	in
	let texpr = loop t in
	mk (TCast (type_expr ctx e WithType.value,Some texpr)) t p

and make_if_then_else ctx e0 e1 e2 with_type p =
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
	mk (TIf (e0,e1,Some e2)) t p

and type_if ctx e e1 e2 with_type is_ternary p =
	let e = type_expr ctx e WithType.value in
	if is_ternary then begin match e.eexpr with
		| TConst TNull -> typing_error "Cannot use null as ternary condition" e.epos
		| _ -> ()
	end;
	let e = AbstractCast.cast_or_unify ctx ctx.t.tbool e p in
	let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
	match e2 with
	| None ->
		mk (TIf (e,e1,None)) ctx.t.tvoid p
	| Some e2 ->
		let e2 = type_expr ctx (Expr.ensure_block e2) with_type in
		make_if_then_else ctx e e1 e2 with_type p

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
			typing_error "Markup literals must be processed by a macro" p
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
			(if ctx.bypass_accessor > old_counter then display_error ctx.com "Field access expression expected after @:bypassAccessor metadata" p);
			e
		| (Meta.Inline,_,pinline) ->
			begin match fst e1 with
			| ECall(e1,el) ->
				acc_get ctx (type_call_access ctx e1 el MGet WithType.value (Some pinline) p)
			| ENew (t,el) ->
				let e = type_new ctx t el with_type true p in
				{e with eexpr = TMeta((Meta.Inline,[],null_pos),e)}
			| _ ->
				display_error ctx.com "Call or function expected after inline keyword" p;
				e();
			end
		| (Meta.ImplicitReturn,_,_) ->
			begin match e1 with
			| (EReturn e, p) -> type_return ~implicit:true ctx e with_type p
			| _ -> e()
			end
		| (Meta.Dollar s,_,p) ->
			display_error ctx.com (Printf.sprintf "Reification $%s is not allowed outside of `macro` expression" s) p;
			e()
		| _ -> e()
	in
	ctx.meta <- old;
	e

and type_call_target ctx e el with_type p_inline =
	let p = (pos e) in
	let e = maybe_type_against_enum ctx (fun () -> type_access ctx (fst e) (snd e) (MCall el) WithType.value) with_type true p in
	let check_inline cf p =
		if (has_class_field_flag cf CfAbstract) then display_error ctx.com "Cannot force inline on abstract method" p
	in
	match p_inline with
	| None ->
		e
	| Some pinline ->
		let rec loop e =
			match e with
			| AKSafeNav sn ->
				AKSafeNav { sn with sn_access = loop sn.sn_access }
			| AKField fa ->
				check_inline fa.fa_field pinline;
				AKField({fa with fa_inline = true})
			| AKUsingField sea ->
				check_inline sea.se_access.fa_field pinline;
				AKUsingField {sea with se_access = {sea.se_access with fa_inline = true}}
			| AKExpr {eexpr = TLocal _} ->
				display_error ctx.com "Cannot force inline on local functions" pinline;
				e
			| _ ->
				e
		in
		loop e

and type_call_access ctx e el mode with_type p_inline p =
	try
		let e = type_call_builtin ctx e el mode with_type p in
		AKExpr e
	with Exit ->
		let acc = type_call_target ctx e el with_type p_inline in
		build_call_access ctx acc el mode with_type p

and type_call_builtin ctx e el mode with_type p =
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
			type_expr ctx (ECall ((efield ((efield ((EConst (Ident "haxe"),p),"Log"),p),"trace"),p),[mk_to_string_meta e;infos]),p) WithType.NoValue
	| (EField ((EConst (Ident "super"),_),_,_),_), _ ->
		(* no builtins can be applied to super as it can't be a value *)
		raise Exit
	| (EField (e,"bind",efk_todo),p), args ->
		let e = type_expr ctx e WithType.value in
		(match follow e.etype with
			| TFun signature -> type_bind ctx e signature args p
			| _ -> raise Exit)
	| (EConst (Ident "$type"),_) , [e] ->
		begin match fst e with
		| EConst (Ident "_") ->
			warning ctx WInfo (WithType.to_string with_type) p;
			mk (TConst TNull) t_dynamic p
		| _ ->
			let e = type_expr ctx e WithType.value in
			warning ctx WInfo (s_type (print_context()) e.etype) e.epos;
			let e = Diagnostics.secure_generated_code ctx e in
			e
		end
	| (EField(e,"match",efk_todo),p), [epat] ->
		let et = type_expr ctx e WithType.value in
		let rec has_enum_match t = match follow t with
			| TEnum _ -> true
			| TAbstract (a,tl) when (Meta.has Meta.Forward a.a_meta) && not (Meta.has Meta.CoreType a.a_meta) ->
				(match a.a_impl with
					| Some c when (PMap.exists "match" c.cl_statics) && (has_class_field_flag (PMap.find "match" c.cl_statics) CfImpl) -> false
					| _ -> has_enum_match (Abstract.get_underlying_type ~return_first:true a tl))
			| _ -> false
		in
		if has_enum_match et.etype then
			Matcher.Match.match_expr ctx e [[epat],None,Some (EConst(Ident "true"),p),p] (Some (Some (EConst(Ident "false"),p),p)) (WithType.with_type ctx.t.tbool) true p
		else
			raise Exit
	| (EConst (Ident "__unprotect__"),_) , [(EConst (String _),_) as e] ->
		let e = type_expr ctx e WithType.value in
		if Common.platform ctx.com Flash then
			let t = tfun [e.etype] e.etype in
			let e_unprotect = mk (TIdent "__unprotect__") t p in
			mk (TCall (e_unprotect,[e])) e.etype e.epos
		else
			e
	| (EDisplay((EConst (Ident "super"),_ as e1),dk),_),_ ->
		TyperDisplay.handle_display ctx (ECall(e1,el),p) dk mode with_type
	| (EConst (Ident "super"),sp) , el ->
		if ctx.curfun <> FunConstructor then typing_error "Cannot call super constructor outside class constructor" p;
		let el, t = (match ctx.curclass.cl_super with
		| None -> typing_error "Current class does not have a super" p
		| Some (c,params) ->
			let fa = FieldAccess.get_constructor_access c params p in
			let cf = fa.fa_field in
			let t = TInst (c,params) in
			let e = mk (TConst TSuper) t sp in
			if (Meta.has Meta.CompilerGenerated cf.cf_meta) then display_error ctx.com (error_msg (No_constructor (TClassDecl c))) p;
			let fa = FieldAccess.create e cf (FHInstance(c,params)) false p in
			let fcc = unify_field_call ctx fa [] el p false in
			let el = fcc.fc_args in
			el,t
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) ctx.t.tvoid p
	| _ ->
		raise Exit

and type_expr ?(mode=MGet) ctx (e,p) (with_type:WithType.t) =
	match e with
	| EField ((EConst (String(s,_)),ps),"code",EFNormal) ->
		if UTF8.length s <> 1 then typing_error "String must be a single UTF8 char" ps;
		mk (TConst (TInt (Int32.of_int (UCharExt.code (UTF8.get s 0))))) ctx.t.tint p
	| EField(_,n,_) when starts_with n '$' ->
		typing_error "Field names starting with $ are not allowed" p
	| EConst (Ident s) ->
		if s = "super" && with_type <> WithType.NoValue && not ctx.in_display then typing_error "Cannot use super as value" p;
		let e = maybe_type_against_enum ctx (fun () -> type_ident ctx s p mode with_type) with_type false p in
		acc_get ctx e
	| EField _
	| EArray _
	| ECall _ ->
		acc_get ctx (type_access ctx e p mode with_type)
	| EConst (Regexp (r,opt)) ->
		let str = mk (TConst (TString r)) ctx.t.tstring p in
		let opt = mk (TConst (TString opt)) ctx.t.tstring p in
		let t = Typeload.load_core_type ctx "EReg" in
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> die "" __LOC__),[],[str;opt])) t p
	| EConst (String(s,SSingleQuotes)) when s <> "" ->
		type_expr ctx (format_string ctx s p) with_type
	| EConst (Int (s, Some suffix)) ->
		(match suffix with
		| "i32" ->
			(try mk (TConst (TInt (Int32.of_string s))) ctx.com.basic.tint p
			with _ -> typing_error ("Cannot represent " ^ s ^ " with a 32 bit integer") p)
		| "i64" ->
			if String.length s > 18 && String.sub s 0 2 = "0x" then typing_error "Invalid hexadecimal integer" p;

			let i64  = Int64.of_string s in
			let high = Int64.to_int32 (Int64.shift_right i64 32) in
			let low  = Int64.to_int32 i64 in

			let ident = EConst (Ident "haxe"), p in
			let field = efield ((efield (ident, "Int64"), p), "make"), p in

			let arg_high = EConst (Int (Int32.to_string high, None)), p in
			let arg_low  = EConst (Int (Int32.to_string low, None)), p in
			let call     = ECall (field, [ arg_high; arg_low ]), p in
			type_expr ctx call with_type
		| "u32" ->
			let check = ECheckType ((EConst (Int (s, None)), p), (CTPath (mk_type_path ([],"UInt")), p)), p in
			type_expr ctx check with_type
		| other -> typing_error (other ^ " is not a valid integer suffix") p)
	| EConst (Float (s, Some suffix) as c) ->
		(match suffix with
		| "f64" -> Texpr.type_constant ctx.com.basic c p
		| other -> typing_error (other ^ " is not a valid float suffix") p)
	| EConst c ->
		Texpr.type_constant ctx.com.basic c p
	| EBinop (OpNullCoal,e1,e2) ->
		let vr = new value_reference ctx in
		let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
		let e2 = type_expr ctx (Expr.ensure_block e2) (WithType.with_type e1.etype) in
		let e1 = vr#as_var "tmp" {e1 with etype = ctx.t.tnull e1.etype} in
		let e_null = Builder.make_null e1.etype e1.epos in
		let e_cond = mk (TBinop(OpNotEq,e1,e_null)) ctx.t.tbool e1.epos in

		let follow_null_once t =
			match t with
			| TAbstract({a_path = [],"Null"},[t]) -> t
			| _ -> t
		in
		let iftype = if DeadEnd.has_dead_end e2 then
			WithType.with_type (follow_null_once e1.etype)
		else
			WithType.WithType(e2.etype,None)
		in
		let e_if = make_if_then_else ctx e_cond e1 e2 iftype p in
		vr#to_texpr e_if
	| EBinop (OpAssignOp OpNullCoal,e1,e2) ->
		let e_cond = EBinop(OpNotEq,e1,(EConst(Ident "null"), p)) in
		let e_if = EIf ((e_cond, p),e1,Some e2) in
		type_assign ctx e1 (e_if, p) with_type p
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
		type_if ctx e1 e2 (Some e3) with_type true p
	| EIf (e,e1,e2) ->
		type_if ctx e e1 e2 with_type false p
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
			display_error ctx.com "Return outside function" p;
			match e with
			| None ->
				Texpr.Builder.make_null (mono_or_dynamic ctx with_type p) p
			| Some e ->
				(* type the return expression to see if there are more errors
				   as well as use its type as if there was no `return`, since
				   that is most likely what was meant *)
				type_expr ctx e WithType.value
		end else
			type_return ctx e with_type p
	| EBreak ->
		if not ctx.in_loop then display_error ctx.com "Break outside loop" p;
		mk TBreak (mono_or_dynamic ctx with_type p) p
	| EContinue ->
		if not ctx.in_loop then display_error ctx.com "Continue outside loop" p;
		mk TContinue (mono_or_dynamic ctx with_type p) p
	| ETry (e1,[]) ->
		type_expr ctx e1 with_type
	| ETry (e1,catches) ->
		type_try ctx e1 catches with_type p
	| EThrow e ->
		let e = type_expr ctx e WithType.value in
		mk (TThrow e) (mono_or_dynamic ctx with_type p) p
	| ENew (t,el) ->
		type_new ctx t el with_type false p
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e with_type p
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
		TyperDisplay.handle_edisplay ctx e dk mode with_type
	| ECheckType (e,t) ->
		let t = Typeload.load_complex_type ctx true t in
		let e = type_expr ctx e (WithType.with_type t) in
		let e = AbstractCast.cast_or_unify ctx t e p in
		if e.etype == t then e else mk (TCast (e,None)) t p
	| EMeta (m,e1) ->
		type_meta ~mode ctx m e1 with_type p
	| EIs (e,(t,p_t)) ->
		match t with
		| CTPath tp ->
			if tp.tparams <> [] then display_error ctx.com "Type parameters are not supported for the `is` operator" p_t;
			let e = type_expr ctx e WithType.value in
			let mt = Typeload.load_type_def ctx p_t tp in
			if ctx.in_display && DisplayPosition.display_position#enclosed_in p_t then
				DisplayEmitter.display_module_type ctx mt p_t;
			let e_t = type_module_type ctx mt None p_t in
			let e_Std_isOfType =
				match Typeload.load_type_raise ctx ([],"Std") "Std" p with
				| TClassDecl c ->
					let cf =
						try PMap.find "isOfType" c.cl_statics
						with Not_found -> die "" __LOC__
					in
					Texpr.Builder.make_static_field c cf (mk_zero_range_pos p)
				| _ -> die "" __LOC__
			in
			mk (TCall (e_Std_isOfType, [e; e_t])) ctx.com.basic.tbool p
		| _ ->
			display_error ctx.com "Unsupported type for `is` operator" p_t;
			Texpr.Builder.make_bool ctx.com.basic false p

(* ---------------------------------------------------------------------- *)
(* TYPER INITIALIZATION *)

let rec create com =
	let ctx = {
		com = com;
		t = com.basic;
		g = {
			core_api = None;
			macros = None;
			type_patches = Hashtbl.create 0;
			global_metadata = [];
			module_check_policies = [];
			delayed = [];
			debug_delayed = [];
			doinline = com.display.dms_inline && not (Common.defined com Define.NoInline);
			std = null_module;
			global_using = [];
			complete = false;
			type_hints = [];
			load_only_cached_modules = false;
			functional_interface_lut = new pmap_lookup;
			do_inherit = MagicTypes.on_inherit;
			do_create = create;
			do_macro = MacroContext.type_macro;
			do_load_macro = MacroContext.load_macro';
			do_load_module = TypeloadModule.load_module;
			do_load_type_def = Typeload.load_type_def;
			do_build_instance = InstanceBuilder.build_instance;
			do_format_string = format_string;
			do_load_core_class = Typeload.load_core_class;
		};
		m = {
			curmod = null_module;
			module_imports = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			import_statements = [];
		};
		is_display_file = false;
		bypass_accessor = 0;
		meta = [];
		with_type_stack = [];
		call_argument_stack = [];
		pass = PBuildModule;
		macro_depth = 0;
		untyped = false;
		curfun = FunStatic;
		in_function = false;
		in_loop = false;
		in_display = false;
		allow_inline = true;
		allow_transform = true;
		get_build_infos = (fun() -> None);
		ret = mk_mono();
		locals = PMap.empty;
		type_params = [];
		curclass = null_class;
		curfield = null_field;
		tthis = mk_mono();
		opened = [];
		vthis = None;
		in_call_args = false;
		in_overload_call_args = false;
		delayed_display = None;
		monomorphs = {
			perfunction = [];
		};
		memory_marker = Typecore.memory_marker;
	} in
	ctx.g.std <- (try
		TypeloadModule.load_module ctx ([],"StdTypes") null_pos
	with
		Error (Module_not_found ([],"StdTypes"),_) ->
			try
				let std_path = Sys.getenv "HAXE_STD_PATH" in
				typing_error ("Standard library not found. Please check your `HAXE_STD_PATH` environment variable (current value: \"" ^ std_path ^ "\")") null_pos
			with Not_found ->
				typing_error "Standard library not found. You may need to set your `HAXE_STD_PATH` environment variable" null_pos
	);
	(* We always want core types to be available so we add them as default imports (issue #1904 and #3131). *)
	ctx.m.module_imports <- List.map (fun t -> t,null_pos) ctx.g.std.m_types;
	List.iter (fun t ->
		match t with
		| TAbstractDecl a ->
			(match snd a.a_path with
			| "Void" -> ctx.t.tvoid <- TAbstract (a,[]);
			| "Float" -> ctx.t.tfloat <- TAbstract (a,[]);
			| "Int" -> ctx.t.tint <- TAbstract (a,[])
			| "Bool" -> ctx.t.tbool <- TAbstract (a,[])
			| "Dynamic" -> t_dynamic_def := TAbstract(a,extract_param_types a.a_params);
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
type_call_target_ref := type_call_target;
type_access_ref := type_access;
type_block_ref := type_block;
