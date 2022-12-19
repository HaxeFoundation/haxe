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

open Globals
open Ast
open Type
open Common
open Texpr.Builder
open Error
open DisplayTypes

exception Internal_match_failure

let s_type t = s_type (print_context()) t
let s_expr_pretty e = s_expr_pretty false "" false s_type e

let fake_tuple_type = TInst(mk_class null_module ([],"-Tuple") null_pos null_pos, [])

let tuple_type tl =
	tfun tl fake_tuple_type

let make_offset_list left right middle other =
	(ExtList.List.make left other) @ [middle] @ (ExtList.List.make right other)

let type_field_access ctx ?(resume=false) e name =
	Calls.acc_get ctx (Fields.type_field (Fields.TypeFieldConfig.create resume) ctx e name e.epos MGet WithType.value) e.epos

let unapply_type_parameters params monos =
	let unapplied = ref [] in
	List.iter2 (fun tp1 t2 ->
		match t2,follow t2 with
		| TMono m1,TMono m2 ->
			unapplied := (m1,m1.tm_type) :: !unapplied;
			Monomorph.bind m1 tp1.ttp_type;
		| _ -> ()
	) params monos;
	!unapplied

let reapply_type_parameters unapplied =
	List.iter (fun (m,o) -> match o with
		| None -> Monomorph.unbind m
		| Some t -> Monomorph.bind m t
	) unapplied

let get_general_module_type ctx mt p =
	let rec loop = function
		| TClassDecl _ -> "Class"
		| TEnumDecl _ -> "Enum"
		| TAbstractDecl a when Meta.has Meta.RuntimeValue a.a_meta -> "Class"
		| TTypeDecl t ->
			begin match follow (monomorphs t.t_params t.t_type) with
				| TInst(c,_) -> loop (TClassDecl c)
				| TEnum(en,_) -> loop (TEnumDecl en)
				| TAbstract(a,_) -> loop (TAbstractDecl a)
				| _ -> typing_error "Cannot use this type as a value" p
			end
		| _ -> typing_error "Cannot use this type as a value" p
	in
	Typeload.load_instance ctx ({tname=loop mt;tpackage=[];tsub=None;tparams=[]},p) true

module Constructor = struct
	type t_kind =
		| ConConst of tconstant
		| ConEnum of tenum * tenum_field
		| ConStatic of tclass * tclass_field
		| ConTypeExpr of module_type
		| ConFields of string list
		| ConArray of int

	type t = t_kind * pos

	let to_string con = match fst con with
		| ConConst ct -> s_const ct
		| ConEnum(en,ef) -> ef.ef_name
		| ConStatic(c,cf) -> Printf.sprintf "%s.%s" (s_type_path (match c.cl_kind with KAbstractImpl a -> a.a_path | _ -> c.cl_path)) cf.cf_name
		| ConTypeExpr mt -> s_type_path (t_infos mt).mt_path
		| ConFields fields -> Printf.sprintf "{ %s }" (String.concat ", " fields)
		| ConArray i -> Printf.sprintf "<array %i>" i

	let equal con1 con2 = match fst con1,fst con2 with
		| ConConst ct1,ConConst ct2 -> ct1 = ct2
		| ConEnum(en1,ef1),ConEnum(en2,ef2) -> en1 == en2 && ef1 == ef2
		| ConStatic(c1,cf1),ConStatic(c2,cf2) -> c1 == c2 && cf1 == cf2
		| ConTypeExpr mt1,ConTypeExpr mt2 -> mt1 == mt2
		| ConFields _,ConFields _ -> true
		| ConArray i1,ConArray i2 -> i1 = i2
		| _ -> false

	let arity con = match fst con with
		| ConEnum (_,{ef_type = TFun(args,_)}) -> List.length args
		| ConEnum _ -> 0
		| ConConst _ -> 0
		| ConFields fields -> List.length fields
		| ConArray i -> i
		| ConTypeExpr _ -> 0
		| ConStatic _ -> 0

	let compare con1 con2 = match fst con1,fst con2 with
		| ConConst ct1,ConConst ct2 -> compare ct1 ct2
		| ConEnum(en1,ef1),ConEnum(en2,ef2) -> compare ef1.ef_index ef2.ef_index
		| ConStatic(c1,cf1),ConStatic(c2,cf2) -> compare cf1.cf_name cf2.cf_name
		| ConTypeExpr mt1,ConTypeExpr mt2 -> compare (t_infos mt1).mt_path (t_infos mt2).mt_path
		| ConFields _,ConFields _ -> 0
		| ConArray i1,ConArray i2 -> i1 - i2
		| _ -> -1 (* Could assert... *)

	open Typecore

	let to_texpr ctx match_debug con =
		let p = pos con in
		match fst con with
		| ConEnum(en,ef) ->
			if match_debug then mk (TConst (TString ef.ef_name)) ctx.t.tstring p
			else mk (TConst (TInt (Int32.of_int ef.ef_index))) ctx.t.tint p
		| ConConst ct -> make_const_texpr ctx.com.basic ct p
		| ConArray i -> make_int ctx.com.basic i p
		| ConTypeExpr mt -> TyperBase.type_module_type ctx mt None p
		| ConStatic(c,cf) -> make_static_field c cf p
		| ConFields _ -> typing_error "Something went wrong" p

	let hash con = Hashtbl.hash (fst con)
end

module Pattern = struct
	open Typecore
	open Constructor

	type t =
		| PatConstructor of Constructor.t * pattern list
		| PatVariable of tvar
		| PatAny
		| PatBind of tvar * pattern
		| PatOr of pattern * pattern
		| PatTuple of pattern list
		| PatExtractor of tvar * texpr * pattern

	and pattern = t * pos

	type pattern_context = {
		ctx : typer;
		or_locals : (string, tvar * pos) PMap.t option;
		ctx_locals : (string, tvar) PMap.t;
		mutable current_locals : (string, tvar * pos) PMap.t;
		mutable in_reification : bool;
		is_postfix_match : bool;
		unapply_type_parameters : unit -> (tmono * Type.t option) list;
	}

	exception Bad_pattern of string

	let rec to_string pat = match fst pat with
		| PatConstructor(con,patterns) -> Printf.sprintf "%s(%s)" (Constructor.to_string con) (String.concat ", " (List.map to_string patterns))
		| PatVariable v -> Printf.sprintf "%s<%i>" v.v_name v.v_id
		| PatAny -> "_"
		| PatBind(v,pat1) -> Printf.sprintf "%s = %s" v.v_name (to_string pat1)
		| PatOr(pat1,pat2) -> Printf.sprintf "(%s) | (%s)" (to_string pat1) (to_string pat2)
		| PatTuple pl -> Printf.sprintf "[%s]" (String.concat ", " (List.map to_string pl))
		| PatExtractor(v,e,pat1) -> Printf.sprintf "%s => %s" (s_expr_pretty e) (to_string pat1)

	let unify_type_pattern ctx mt t p =
		let tcl = get_general_module_type ctx mt p in
		match tcl with
			| TAbstract(a,_) -> unify ctx (TAbstract(a,[spawn_monomorph ctx p])) t p
			| _ -> die "" __LOC__

	let rec make pctx toplevel t e =
		let ctx = pctx.ctx in
		let p = pos e in
		let fail () =
			typing_error ("Unrecognized pattern: " ^ (Ast.Printer.s_expr e)) p
		in
		let unify_expected t' =
			unify ctx t' t p
		in
		let verror name p =
			typing_error (Printf.sprintf "Variable %s must appear exactly once in each sub-pattern" name) p
		in
		let add_local final name p =
			let is_wildcard_local = name = "_" in
			if not is_wildcard_local && pctx.is_postfix_match then typing_error "Pattern variables are not allowed in .match patterns" p;
			if not is_wildcard_local && PMap.mem name pctx.current_locals then typing_error (Printf.sprintf "Variable %s is bound multiple times" name) p;
			match pctx.or_locals with
			| Some map when not is_wildcard_local ->
				let v,p = try PMap.find name map with Not_found -> verror name p in
				unify ctx t v.v_type p;
				if final then add_var_flag v VFinal;
				pctx.current_locals <- PMap.add name (v,p) pctx.current_locals;
				v
			| _ ->
				let v = alloc_var (VUser TVOPatternVariable) name t p in
				if final then add_var_flag v VFinal;
				pctx.current_locals <- PMap.add name (v,p) pctx.current_locals;
				ctx.locals <- PMap.add name v ctx.locals;
				v
		in
		let con_enum en ef p =
			DeprecationCheck.check_enum pctx.ctx.com en p;
			DeprecationCheck.check_ef pctx.ctx.com ef p;
			ConEnum(en,ef),p
		in
		let con_static c cf p = ConStatic(c,cf),p in
		let con_const ct p = ConConst ct,p in
		let con_type_expr mt p = ConTypeExpr mt,p in
		let con_array i p = ConArray i,p in
		let con_fields fl p = ConFields fl,p in
		let get_enumerable_idents () = match follow t with
			| TEnum(en,_) ->
				en.e_names
			| TAbstract({a_impl = Some c} as a,pl) when a.a_enum ->
				ExtList.List.filter_map (fun cf ->
					if has_class_field_flag cf CfImpl && has_class_field_flag cf CfEnum then Some cf.cf_name else None
				) c.cl_ordered_statics
			| _ ->
				[]
		in
		let check_expr e =
			let rec loop e = match e.eexpr with
				| TField(_,FEnum(en,ef)) ->
					(* Let the unification afterwards fail so we don't recover. *)
					(* (match follow ef.ef_type with TFun _ -> raise Exit | _ -> ()); *)
					PatConstructor(con_enum en ef e.epos,[])
				| TField(_,FStatic(c,({cf_kind = Var {v_write = AccNever}} as cf))) ->
					PatConstructor(con_static c cf e.epos,[])
				| TConst ct ->
					PatConstructor(con_const ct e.epos,[])
				| TCast(e1,None) ->
					loop e1
				| TField (ef,f) ->
					let s = field_name f in
					begin match StringError.get_similar s (get_enumerable_idents()) with
						| [] -> ()
						| l ->
							let tpath = match follow t with
								| TEnum (e,tl) -> s_type_path e.e_path ^ "."
								| TAbstract (a,tl) -> s_type_path a.a_path ^ "."
								| _ -> ""
							in
							let fields = List.map (fun (el) -> tpath ^ el) l in
							warning pctx.ctx WTyper ("Potential typo detected (expected similar values are " ^ (String.concat ", " fields) ^ ")") p
					end;
					raise (Bad_pattern "Only inline or read-only (default, never) fields can be used as a pattern")
				| TTypeExpr mt ->
					PatConstructor(con_type_expr mt e.epos,[])
				| TMeta((Meta.Deprecated,_,_) as m, e1) ->
					DeprecationCheck.check_meta pctx.ctx.com [m] "field" e1.epos;
					loop e1
				| _ ->
					raise Exit
			in
			loop e
		in
		let display_mode () =
			if pctx.is_postfix_match then DKMarked else DKPattern toplevel
		in
		let catch_errors () =
			let old = ctx.com.error in
			let restore_report_mode = disable_report_mode ctx.com in
			ctx.com.error <- (fun _ _ ->
				raise Exit
			);
			(fun () ->
				restore_report_mode();
				ctx.com.error <- old
			)
		in
		let try_typing e =
			let old = ctx.untyped in
			ctx.untyped <- true;
			let restore = catch_errors () in
			let e = try
				type_expr ctx e (WithType.with_type t)
			with exc ->
				restore();
				ctx.untyped <- old;
				raise exc
			in
			restore();
			ctx.untyped <- old;
			let pat = check_expr e in
			begin match pat with
				| PatConstructor((ConTypeExpr mt,_),_) -> unify_type_pattern ctx mt t e.epos;
				| _ -> unify ctx e.etype t p;
			end;
			pat
		in
		let handle_ident s p =
			try
				try_typing (EConst (Ident s),p)
			with
			| Exit | Bad_pattern _ ->
				let restore = catch_errors () in
				begin try
					let mt = module_type_of_type t in
					let e_mt = TyperBase.type_module_type ctx mt None p in
					let e = type_field_access ctx ~resume:true e_mt s in
					restore();
					check_expr e
				with _ ->
					restore();
					if not (is_lower_ident s) && (match s.[0] with '`' | '_' -> false | _ -> true) then begin
						display_error ctx.com ("Unknown identifier : " ^ s ^ ", pattern variables must be lower-case or with `var ` prefix") p;
					end;
					begin match StringError.get_similar s (get_enumerable_idents()) with
						| [] ->
							()
							(* if toplevel then
								warning pctx.ctx (Printf.sprintf "`case %s` has been deprecated, use `case var %s` instead" s s) p *)
						| l -> warning pctx.ctx WTyper ("Potential typo detected (expected similar values are " ^ (String.concat ", " l) ^ "). Consider using `var " ^ s ^ "` instead") p
					end;
					let v = add_local false s p in
					PatVariable v
				end
		in
		let rec loop e = match fst e with
			| EParenthesis e1 | ECast(e1,None) ->
				loop e1
			| ECheckType(e, (CTPath({tpackage=["haxe";"macro"]; tname="Expr"}),_)) ->
				let old = pctx.in_reification in
				pctx.in_reification <- true;
				let e = loop e in
				pctx.in_reification <- old;
				e
			| EConst((Ident ("false" | "true") | Int (_,_) | String _ | Float (_,_)) as ct) ->
				begin match ct with
					| String (value,kind) when kind = Ast.SSingleQuotes ->
						let e = ctx.g.do_format_string ctx value p in
						begin match e with
							| EBinop _, p -> typing_error "String interpolation is not allowed in case patterns" p;
							| _ -> ()
						end;
					| _ -> ()
				end;
				let p = pos e in
				let e = Texpr.type_constant ctx.com.basic ct p in
				unify_expected e.etype;
				let ct = match e.eexpr with TConst ct -> ct | _ -> die "" __LOC__ in
				PatConstructor(con_const ct p,[])
			| EConst (Ident i) ->
				begin match follow t with
					| TFun(ta,tr) when tr == fake_tuple_type ->
						if i = "_" then PatTuple(List.map (fun (_,_,t) -> (PatAny,pos e)) ta)
						else typing_error "Cannot bind matched tuple to variable, use _ instead" p
					| _ ->
						if i = "_" then PatAny
						else handle_ident i (pos e)
				end
			| EVars([{ ev_name = (s,p); ev_final = final; ev_type = None; ev_expr = None; }]) ->
				let v = add_local final s p in
				PatVariable v
			| ECall(e1,el) ->
				let e1 = type_expr ctx e1 (WithType.with_type t) in
				begin match e1.eexpr,follow e1.etype with
					| TField(_, FEnum(en,ef)),TFun(_,TEnum(_,tl)) ->
						let map = apply_params en.e_params tl in
						let monos = Monomorph.spawn_constrained_monos map ef.ef_params in
						let map t = map (apply_params ef.ef_params monos t) in
						unify ctx (map ef.ef_type) e1.etype e1.epos;
						let args = match follow e1.etype with
							| TFun(args,r) ->
								unify_expected r;
								args
							| _ -> die "" __LOC__
						in
						let rec loop el tl = match el,tl with
							| [EConst (Ident "_"),p],(_,_,t) :: tl ->
								(* Allow using final _ to match "multiple" arguments *)
								(PatAny,p) :: (match tl with [] -> [] | _ -> loop el tl)
							| e :: el,(_,_,t) :: tl ->
								make pctx false t e :: loop el tl
							| [],(_,true,t) :: tl ->
								(PatAny,pos e) :: loop [] tl
							| [],[] ->
								[]
							| [],_ ->
								typing_error "Not enough arguments" p
							| _,[] ->
								typing_error "Too many arguments" p
						in
						let patterns = loop el args in
						ignore(unapply_type_parameters ef.ef_params monos);
						PatConstructor(con_enum en ef e1.epos,patterns)
					| _ ->
						fail()
				end
			| EField _ ->
				begin try
					try_typing e
				with
					| Exit -> fail()
					| Bad_pattern s -> typing_error s p
				end
			| EArrayDecl el ->
				let rec pattern seen t = match follow t with
					| TFun(tl,tr) when tr == fake_tuple_type ->
						let rec loop el tl = match el,tl with
							| e :: el,(_,_,t) :: tl ->
								let pat = make pctx false t e in
								pat :: loop el tl
							| [],[] -> []
							| [],_ -> typing_error "Not enough arguments" p
							| (_,p) :: _,[] -> typing_error "Too many arguments" p
						in
						let patterns = loop el tl in
						PatTuple patterns
					| TInst({cl_path=[],"Array"},[t2]) | (TDynamic _ as t2) ->
						let patterns = ExtList.List.mapi (fun i e ->
							make pctx false t2 e
						) el in
						PatConstructor(con_array (List.length patterns) (pos e),patterns)
					| TAbstract(a,tl) as t when not (List.exists (fun t' -> shallow_eq t t') seen) ->
						begin match TyperBase.get_abstract_froms ctx a tl with
							| [t2] -> pattern (t :: seen) t2
							| _ -> fail()
						end
					| _ ->
						fail()
				in
				pattern [] t
			| EObjectDecl fl ->
				let known_fields = ref [] in
				let collect_field cf t filter =	match filter with
					| Some sl when not (List.mem cf.cf_name sl) -> ()
					| _ -> known_fields := (cf,t) :: (List.filter (fun (cf',_) -> cf'.cf_name <> cf.cf_name) !known_fields)
				in
				let rec collect_fields t filter = match follow t with
					| TAnon an ->
						PMap.iter (fun _ cf -> collect_field cf cf.cf_type filter) an.a_fields
					| TInst(c,tl) ->
						let rec loop c tl =
							(match c.cl_super with
								| Some (csup,tlsup) -> loop csup (List.map (apply_params c.cl_params tl) tlsup)
								| _ -> ());
							List.iter (fun cf ->
								if Typecore.can_access ctx c cf false then
									collect_field cf (apply_params c.cl_params tl cf.cf_type) filter
							) c.cl_ordered_fields
						in
						loop c tl
					| TAbstract({a_impl = Some c} as a,tl) ->
						(if Meta.has Meta.Forward a.a_meta then
							let _,el,_ = Meta.get Meta.Forward a.a_meta in
							let sl = ExtList.List.filter_map (fun e -> match fst e with
								| EConst(Ident s) -> Some s
								| _ -> None
							) el in
							let filter = if sl = [] then filter else Some (match filter with
								| Some fsl -> List.filter (fun s -> List.mem s fsl) sl
								| None -> sl
							) in
							collect_fields (Abstract.get_underlying_type a tl) filter);
						List.iter (fun cf ->
							if has_class_field_flag cf CfImpl then
								collect_field cf (apply_params a.a_params tl cf.cf_type) filter
						) c.cl_ordered_statics;
					| _ ->
						typing_error (Printf.sprintf "Cannot field-match against %s" (s_type t)) (pos e)
				in
				collect_fields t None;
				let is_matchable cf =
					match cf.cf_kind with Method _ -> false | _ -> true
				in
				let patterns,fields = List.fold_left (fun (patterns,fields) (cf,t) ->
					try
						if pctx.in_reification && cf.cf_name = "pos" then raise Not_found;
						let e1 = Expr.field_assoc cf.cf_name fl in
						make pctx false t e1 :: patterns,cf.cf_name :: fields
					with Not_found ->
						if is_matchable cf then
							(PatAny,cf.cf_pos) :: patterns,cf.cf_name :: fields
						else
							patterns,fields
				) ([],[]) !known_fields in
				List.iter (fun ((s,_,_),e) -> if not (List.mem s fields) then typing_error (Printf.sprintf "%s has no field %s" (s_type t) s) (pos e)) fl;
				PatConstructor(con_fields fields (pos e),patterns)
			| EBinop(OpOr,e1,e2) ->
				let pctx1 = {pctx with current_locals = PMap.empty} in
				let pat1 = make pctx1 toplevel t e1 in
				let pctx2 = {pctx with current_locals = PMap.empty; or_locals = Some (pctx1.current_locals)} in
				let pat2 = make pctx2 toplevel t e2 in
				PMap.iter (fun name (v,p) ->
					if not (PMap.mem name pctx2.current_locals) && name <> "_" then verror name p;
					pctx.current_locals <- PMap.add name (v,p) pctx.current_locals
				) pctx1.current_locals;
				PatOr(pat1,pat2)
			| EBinop(OpAssign,e1,e2) ->
				let rec loop dko e = match e with
					| (EConst (Ident s),p) ->
						let v = add_local false s p in
						begin match dko with
						| None -> ()
						| Some dk -> ignore(TyperDisplay.display_expr ctx e (mk (TLocal v) v.v_type p) dk (MSet None) (WithType.with_type t) p);
						end;
						let pat = make pctx false t e2 in
						PatBind(v,pat)
					| (EParenthesis e1,_) -> loop dko e1
					| (EDisplay(e1,dk),_) -> loop (Some dk) e1
					| _ -> fail()
				in
				loop None e1
			| EBinop(OpArrow,e1,e2) ->
				let restore = save_locals ctx in
				ctx.locals <- pctx.ctx_locals;
				let v = add_local false "_" null_pos in
				(* Tricky stuff: Extractor expressions are like normal expressions, so we don't want to deal with GADT-applied types here.
				   Let's unapply, then reapply after we're done with the extractor (#5952). *)
				let unapplied = pctx.unapply_type_parameters () in
				let e1 = type_expr ctx e1 WithType.value in
				reapply_type_parameters unapplied;
				v.v_name <- "tmp";
				restore();
				let pat = make pctx toplevel e1.etype e2 in
				PatExtractor(v,e1,pat)
			(* Special case for completion on a pattern local: We don't want to add the local to the context
			   while displaying (#7319) *)
			| EDisplay((EConst (Ident _),_ as e),dk) when pctx.ctx.com.display.dms_kind = DMDefault ->
				let locals = ctx.locals in
				let pat = loop e in
				let locals' = ctx.locals in
				ctx.locals <- locals;
				ignore(TyperDisplay.handle_edisplay ctx e (display_mode()) MGet (WithType.with_type t));
				ctx.locals <- locals';
				pat
			(* For signature completion, we don't want to recurse into the inner pattern because there's probably
			   a EDisplay(_,DMMarked) in there. We can handle display immediately because inner patterns should not
			   matter (#7326) *)
			| EDisplay(e1,DKCall) ->
				ignore(TyperDisplay.handle_edisplay ctx e (display_mode()) MGet (WithType.with_type t));
				loop e1
			| EDisplay(e,dk) ->
				let pat = loop e in
				ignore(TyperDisplay.handle_edisplay ctx e (display_mode()) MGet (WithType.with_type t));
				pat
			| EMeta((Meta.StoredTypedExpr,_,_),e1) ->
				let e1 = MacroContext.type_stored_expr ctx e1 in
				loop (TExprToExpr.convert_expr e1)
			| _ ->
				fail()
		in
		let pat = loop e in
		pat,p
end

module Case = struct
	open Typecore
	open Pattern

	type t = {
		case_guard : texpr option;
		case_expr : texpr option;
		case_pos : pos;
	}

	let make ctx t el eg eo_ast with_type postfix_match p =
		let rec collapse_case el = match el with
			| e :: [] ->
				e
			| e :: el ->
				let e2 = collapse_case el in
				EBinop(OpOr,e,e2),punion (pos e) (pos e2)
			| [] ->
				typing_error "case without pattern" p
		in
		let e = collapse_case el in
		let monos = List.map (fun _ -> mk_mono()) ctx.type_params in
		let map = apply_params ctx.type_params monos in
		let save = save_locals ctx in
		let old_types = PMap.fold (fun v acc ->
			let t_old = v.v_type in
			v.v_type <- map v.v_type;
			(v,t_old) :: acc
		) ctx.locals [] in
		let old_ret = ctx.ret in
		ctx.ret <- map ctx.ret;
		let pctx = {
			ctx = ctx;
			current_locals = PMap.empty;
			ctx_locals = ctx.locals;
			or_locals = None;
			in_reification = false;
			is_postfix_match = postfix_match;
			unapply_type_parameters = (fun () -> unapply_type_parameters ctx.type_params monos);
		} in
		let pat = Pattern.make pctx true (map t) e in
		ignore(unapply_type_parameters ctx.type_params monos);
		let eg = match eg with
			| None -> None
			| Some e ->
				let e = type_expr ctx e WithType.value in
				Some (AbstractCast.cast_or_unify ctx ctx.t.tbool e e.epos)
		in
		let eo = match eo_ast,with_type with
			| None,WithType.WithType(t,_) ->
				unify ctx ctx.t.tvoid t (pos e);
				None
			| None,_ ->
				None
			| Some e,WithType.WithType(t,_) ->
				let e = type_expr ctx e (WithType.with_type (map t)) in
				let e = AbstractCast.cast_or_unify ctx (map t) e e.epos in
				Some e
			| Some e,_ ->
				let e = type_expr ctx e with_type in
				Some e
		in
		ctx.ret <- old_ret;
		List.iter (fun (v,t) -> v.v_type <- t) old_types;
		save();
		{
			case_guard = eg;
			case_expr = eo;
			case_pos = p;
		},[],pat
end

module Decision_tree = struct
	open Case

	type subject = texpr

	type type_finiteness =
		| Infinite          (* type has inifite constructors (e.g. Int, String) *)
		| CompileTimeFinite (* type is considered finite only at compile-time but has inifite possible run-time values (enum abstracts) *)
		| RunTimeFinite     (* type is truly finite (Bool, enums) *)

	type t =
		| Leaf of Case.t
		| Switch of subject * (Constructor.t * bool * dt) list * dt
		| Bind of (tvar * pos * texpr) list * dt
		| Guard of texpr * dt * dt
		| GuardNull of texpr * dt * dt
		| Fail

	and dt = {
		dt_t : t;
		dt_i : int;
		dt_pos : pos;
		mutable dt_goto_target : bool;
		mutable dt_texpr : texpr option;
	}

	let tab_string = "    "

	let to_string dt =
		let buf = Buffer.create 0 in
		let indices = Stack.create () in
		let push_index i = Stack.push i indices in
		let add_line tabs s =
			if Buffer.length buf > 0 then Buffer.add_char buf '\n';
			if not (Stack.is_empty indices) then begin
				Buffer.add_string buf (Printf.sprintf "%2i" (Stack.pop indices));
				Buffer.add_substring buf tabs 0 (String.length tabs - 2);
			end else
				Buffer.add_string buf tabs;
			Buffer.add_string buf s
		in
		let add s =
			Buffer.add_string buf s
		in
		let s_expr tabs e =
			Type.s_expr_pretty false tabs false s_type e
		in
		let print_expr_noblock tabs e = match e.eexpr with
			| TBlock el ->
				List.iter (fun e ->
					add_line tabs (s_expr tabs e) ;
				) el
			| _ ->
				add_line tabs (s_expr tabs e)
		in
		let print_case_expr tabs case = match case.case_expr with
			| None ->
				()
			| Some e ->
				print_expr_noblock tabs e
		in
		let rec loop tabs dt =
			push_index dt.dt_i;
			match dt.dt_t with
			| Leaf case ->
				print_case_expr tabs case
			| Switch(e,cases,dt) ->
				add_line tabs (Printf.sprintf "switch (%s)" (s_expr tabs e));
				List.iter (fun (con,unguarded,dt) ->
					add_line (tabs ^ tab_string) "case ";
					add (Constructor.to_string con);
					add (if unguarded then "(unguarded)" else "guarded");
					add ":";
					loop (tabs ^ tab_string ^ tab_string) dt;
				) cases;
				add_line (tabs ^ tab_string) "default";
				loop (tabs ^ tab_string ^ tab_string) dt;
			| Bind(bl,dt) ->
				List.iter (fun (v,_,e) ->
					add_line tabs "var ";
					add v.v_name;
					add " = ";
					add (s_expr tabs e);
				) bl;
				loop tabs dt
			| Guard(e,dt1,dt2) ->
				print_guard tabs e dt1 dt2 false
			| GuardNull(e,dt1,dt2) ->
				print_guard tabs e dt1 dt2 true
			| Fail ->
				add_line tabs "<fail>";
		and print_guard tabs e dt1 dt2 is_null_guard =
			add_line tabs "if (";
			add (s_expr tabs e);
			if is_null_guard then add " == null";
			add ")";
			loop (tabs ^ tab_string) dt1;
			add_line tabs "else";
			loop (tabs ^ tab_string) dt2;
		in
		loop tab_string dt;
		Buffer.contents buf

	let equal_dt dt1 dt2 = dt1.dt_i = dt2.dt_i

	let equal dt1 dt2 = match dt1,dt2 with
		| Leaf case1,Leaf case2 ->
			case1 == case2
		| Switch(subject1,cases1,dt1),Switch(subject2,cases2,dt2) ->
			Texpr.equal subject1 subject2 &&
			safe_for_all2 (fun (con1,b1,dt1) (con2,b2,dt2) -> Constructor.equal con1 con2 && b1 = b2 && equal_dt dt1 dt2) cases1 cases2 &&
			equal_dt dt1 dt2
		| Bind(l1,dt1),Bind(l2,dt2) ->
			safe_for_all2 (fun (v1,_,e1) (v2,_,e2) -> v1 == v2 && Texpr.equal e1 e2) l1 l2 &&
			equal_dt dt1 dt2
		| Fail,Fail ->
			true
		| (Guard(e1,dt11,dt12),Guard(e2,dt21,dt22)) | (GuardNull(e1,dt11,dt12),GuardNull(e2,dt21,dt22)) ->
			e1 == e2 && equal_dt dt11 dt21 && equal_dt dt12 dt22
		| _ ->
			false

	let hash = Hashtbl.hash
end

module ConTable = Hashtbl.Make(Constructor)

(*
	Implements checks for useless patterns based on http://moscova.inria.fr/~maranget/papers/warn/index.html.
*)
module Useless = struct
	open Pattern
	open Constructor
	open Case

	type useless =
		| False
		| Pos of pos
		| True

	(* U part *)

	let specialize is_tuple con pM =
		let rec loop acc pM = match pM with
			| patterns :: pM ->
				begin match patterns with
					| (PatConstructor(con',patterns1),_) :: patterns2 when not is_tuple && Constructor.equal con con' ->
						loop ((patterns1 @ patterns2) :: acc) pM
					| (PatTuple patterns1,_) :: patterns2 when is_tuple ->
						loop ((patterns1 @ patterns2) :: acc) pM
					| (PatAny,p) :: patterns2 ->
						let patterns1 = ExtList.List.make (arity con) (PatAny,p) in
						loop ((patterns1 @ patterns2) :: acc) pM
					| (PatBind(_,pat1),_) :: patterns2 ->
						loop acc ((pat1 :: patterns2) :: pM)
					| _ ->
						loop acc pM
				end
			| [] ->
				List.rev acc
		in
		loop [] pM

	let default pM =
		let rec loop acc pM = match pM with
			| patterns :: pM ->
				begin match patterns with
					| ((PatVariable _ | PatAny),_) :: patterns ->
						loop (patterns :: acc) pM
					| _ ->
						loop acc pM
				end
			| [] ->
				List.rev acc
		in
		loop [] pM

	let rec u pM q =
		match q,pM with
		| [],[] -> true
		| [],_ -> false
		| (q1 :: ql),_ ->
			let rec loop pat = match fst pat with
				| PatConstructor(con,patterns) ->
					let s = specialize false con pM in
					u s (patterns @ ql)
				| PatTuple patterns ->
					let s = specialize true (ConConst TNull,pos pat) pM in
					u s (patterns @ ql)
				| (PatVariable _ | PatAny) ->
					let d = default pM in
					u d ql
				| PatOr(pat1,pat2) ->
					u pM (pat1 :: ql) || u pM (pat2 :: ql)
				| PatBind(_,pat1) ->
					loop pat1
				| PatExtractor _ ->
					true (* ? *)
			in
			loop q1

	(* U' part *)

	let transfer_column source target =
		let source,target = List.fold_left2 (fun (source,target) patterns1 patterns2 -> match patterns1 with
			| pat :: patterns -> patterns :: source,(pat :: patterns2) :: target
			| [] -> source,target
		) ([],[]) source target in
		List.rev source,List.rev target

	let copy p = List.map (fun _ -> []) p

	let rec specialize' is_tuple con pM qM rM =
		let arity = arity con in
		let rec loop pAcc qAcc rAcc pM qM rM = match pM,qM,rM with
			| p1 :: pM,q1 :: qM,r1 :: rM ->
				let rec loop2 p1 = match p1 with
					| (PatConstructor(con',patterns1),_) :: patterns2 when not is_tuple && Constructor.equal con con' ->
						loop ((patterns1 @ patterns2) :: pAcc) (q1 :: qAcc) (r1 :: rAcc) pM qM rM
					| (PatTuple patterns1,_) :: patterns2 when is_tuple ->
						loop ((patterns1 @ patterns2) :: pAcc) (q1 :: qAcc) (r1 :: rAcc) pM qM rM
					| ((PatVariable _ | PatAny),p) :: patterns2 ->
						let patterns1 = ExtList.List.make arity (PatAny,p) in
						loop ((patterns1 @ patterns2) :: pAcc) (q1 :: qAcc) (r1 :: rAcc) pM qM rM
					| (PatOr(pat1,pat2),_) :: patterns2 ->
						loop pAcc qAcc rAcc (((pat1 :: patterns2) :: (pat2 :: patterns2) :: pM)) (q1 :: q1 :: qM) (r1 :: r1 :: rM)
					| (PatBind(_,pat1),_) :: patterns2 ->
						loop2 (pat1 :: patterns2)
					| _ ->
						loop pAcc qAcc rAcc pM qM rM
				in
				loop2 p1
			| [],_,_ ->
				List.rev pAcc,List.rev qAcc,List.rev rAcc
			| _ ->
				die "" __LOC__
		in
		loop [] [] [] pM qM rM

	let combine et1 et2 = match fst et1,fst et2 with
		| True,True -> True
		| False,False -> False
		| True,False -> Pos (pos et2)
		| False,True -> Pos (pos et1)
		| True,Pos _ -> fst et2
		| Pos _,True -> fst et1
		| False,Pos _ -> Pos (pos et1)
		| Pos _,_ -> fst et1

	let rec u' pM qM rM p q r =
		match p with
		| [] ->
			begin match r with
				| [] -> if u qM q then True else False
				| _ ->
					snd (List.fold_left (fun (i,et) pat -> match fst pat with
						| PatOr(pat1,pat2) ->
						 	let process_row i l q =
						 		let rec loop acc k l = match l with
						 			| x :: l when i = k -> x,(List.rev acc) @ l @ q
						 			| x :: l -> loop (x :: acc) (k + 1) l
						 			| [] -> die "" __LOC__
						 		in
						 		loop [] 0 l
						 	in
							let col,mat = List.fold_left2 (fun (col,mat) r q ->
					 			let x,l = process_row i r q in
					 			([x] :: col,l :: mat)
					 		) ([],[]) rM qM in
					 		let col,mat = List.rev col,List.rev mat in
							let _,r = process_row i r q in
							let et1 = u' col mat (copy mat) [pat1] r [] in
							let qM = (mat @ [r]) in
							let et2 = u' (col @ [[pat1]]) qM (copy qM) [pat2] r [] in
							let et3 = combine (et1,pos pat1) (et2,pos pat2) in
							let p = punion (pos pat1) (pos pat2) in
							let et = combine (et,p) (et3,p) in
							(i + 1,et)
						| _ -> die "" __LOC__
					) (0,True) r)
			end
		| (pat :: pl) ->
			let rec loop pat = match fst pat with
				| PatConstructor(con,patterns) ->
					let pM,qM,rM = specialize' false con pM qM rM in
					u' pM qM rM (patterns @ pl) q r
				| PatTuple patterns ->
					let pM,qM,rM = specialize' true (ConConst TNull,pos pat) pM qM rM in
					u' pM qM rM (patterns @ pl) q r
				| PatAny | PatVariable _ ->
					let pM,qM = transfer_column pM qM in
					u' pM qM rM pl (pat :: q) r
				| PatOr _ ->
					let pM,rM = transfer_column pM rM in
					u' pM qM rM pl q (pat :: r)
				| PatBind(_,pat1) ->
					loop pat1
				| PatExtractor _ ->
					True
			in
			loop pat

	(* Sane part *)

	let check_case ctx p (case,bindings,patterns) =
		let p = List.map (fun (_,_,patterns) -> patterns) p in
		match u' p (copy p) (copy p) patterns [] [] with
			| False -> Typecore.warning ctx WUnusedPattern "This case is unused" case.case_pos
			| Pos p -> Typecore.warning ctx WUnusedPattern "This pattern is unused" p
			| True -> ()

	let check ctx cases =
		ignore(List.fold_left (fun acc (case,bindings,patterns) ->
			check_case ctx acc (case,bindings,patterns);
			if case.case_guard = None then acc @ [case,bindings,patterns] else acc
		) [] cases)
end

module DtTable = Hashtbl.Make(Decision_tree)

module Compile = struct
	open Typecore
	open Decision_tree
	open Case
	open Constructor
	open Pattern

	exception Extractor

	type matcher_context = {
		ctx : typer;
		dt_table : dt DtTable.t;
		match_pos : pos;
		match_debug : bool;
		mutable dt_count : int;
	}

	let rec hashcons mctx dt p =
		try
			DtTable.find mctx.dt_table dt
		with Not_found ->
			let dti = {dt_t = dt; dt_i = mctx.dt_count; dt_pos = p; dt_goto_target = false; dt_texpr = None } in
			DtTable.add mctx.dt_table dt dti;
			mctx.dt_count <- mctx.dt_count + 1;
			dti

	let leaf mctx case = hashcons mctx (Leaf case) case.case_pos
	let fail mctx p = hashcons mctx Fail p
	let switch mctx subject cases default = hashcons mctx (Switch(subject,cases,default)) subject.epos
	let bind mctx bindings dt = hashcons mctx (Bind(bindings,dt)) dt.dt_pos
	let guard mctx e dt1 dt2 = hashcons mctx (Guard({e with epos = mctx.match_pos},dt1,dt2)) (punion dt1.dt_pos dt2.dt_pos)
	let guard_null mctx e dt1 dt2 = hashcons mctx (GuardNull(e,dt1,dt2)) (punion dt1.dt_pos dt2.dt_pos)

	let rec get_sub_subjects mctx e con arg_positions =
		match fst con with
		| ConEnum(en,ef) ->
			let tl = Monomorph.spawn_constrained_monos (fun t -> t) en.e_params in
			let t_en = TEnum(en,tl) in
			let e = if not (type_iseq t_en e.etype) then mk (TCast(e,None)) t_en e.epos else e in
			begin match follow ef.ef_type with
				| TFun(args,_) ->
					let rec combine args positions =
						match (args, positions) with
							| (a :: args, p :: positions) -> (a, p) :: combine args positions
							| (a :: args, []) -> (a, e.epos) :: combine args positions
							| _ -> []
					in
					let arg_and_pos = combine args arg_positions in
					ExtList.List.mapi
						(fun i ((_,_,t), p) ->
							let params = apply_params en.e_params tl (monomorphs ef.ef_params t) in
							mk (TEnumParameter({ e with epos = p },ef,i)) params p
						)
						arg_and_pos
				| _ ->
					[]
			end
		| ConFields sl ->
			List.map (type_field_access mctx.ctx e) sl
		| ConArray 0 -> []
		| ConArray i ->
			ExtList.List.init i (fun i ->
				let ei = make_int mctx.ctx.com.basic i e.epos in
				Calls.acc_get mctx.ctx (Calls.array_access mctx.ctx e ei MGet e.epos) e.epos
			)
		| ConConst _ | ConTypeExpr _ | ConStatic _ ->
			[]

	let specialize subject con cases =
		let arity = arity con in
		let rec specialize (case,bindings,patterns) = match patterns with
			| (PatConstructor(con',patterns1),_) :: patterns2 when Constructor.equal con con' ->
				Some (case,bindings,patterns1 @ patterns2)
			| (PatVariable v,p) :: patterns2 ->
				Some (case,(v,p,subject) :: bindings,ExtList.List.make arity (PatAny,p) @ patterns2)
			| (PatAny,_) as pat :: patterns2 ->
				Some (case,bindings,ExtList.List.make arity pat @ patterns2)
			| (PatBind(v,pat1),p) :: patterns ->
				specialize (case,(v,p,subject) :: bindings,pat1 :: patterns)
			| _ ->
				None
		in
		ExtList.List.filter_map specialize cases

	let default subject cases =
		let rec default (case,bindings,patterns) = match patterns with
			| (PatVariable v,p) :: patterns ->
				Some (case,((v,p,subject) :: bindings),patterns)
			| (PatAny,_) :: patterns ->
				Some (case,bindings,patterns)
			| (PatBind(v,pat1),p) :: patterns ->
				default (case,((v,p,subject) :: bindings),pat1 :: patterns)
			| _ ->
				None
		in
		ExtList.List.filter_map default cases

	let rec is_wildcard_pattern pat = match fst pat with
		| PatVariable _ | PatAny -> true
		| PatBind(_,pat1) -> is_wildcard_pattern pat1
		| _ -> false

	let rec expand cases =
		let rec expand f (case,bindings,patterns) = match patterns with
			| (PatOr(pat1,pat2),_) :: patterns ->
				(expand f (case,bindings,pat1 :: patterns)) @ (expand f (case,bindings,pat2 :: patterns))
			| (PatBind(v,pat1),p) :: patterns ->
				expand (fun pat2 -> f (PatBind(v,pat2),p)) (case,bindings,pat1 :: patterns)
			| (PatTuple patterns1,_) :: patterns2 ->
				expand f (case,bindings,patterns1 @ patterns2)
			| pat :: patterns ->
				[(case,bindings,f pat :: patterns)]
			| [] ->
				[(case,bindings,patterns)]
		in
		List.flatten (List.map (expand (fun pat -> pat)) cases)

	let s_subjects subjects =
		String.concat " " (List.map s_expr_pretty subjects)

	let s_case (case,bindings,patterns) =
		let s_bindings = String.concat ", " (List.map (fun (v,_,e) -> Printf.sprintf "%s<%i> = %s" v.v_name v.v_id (s_expr_pretty e)) bindings) in
		let s_patterns = String.concat " " (List.map Pattern.to_string patterns) in
		let s_expr = match case.case_expr with None -> "" | Some e -> Type.s_expr_pretty false "\t\t" false s_type e in
		let s_guard = match case.case_guard with None -> "" | Some e -> Type.s_expr_pretty false "\t\t" false s_type e in
		Printf.sprintf "\n\t\tbindings: %s\n\t\tpatterns: %s\n\t\tguard: %s\n\t\texpr: %s" s_bindings s_patterns s_guard s_expr

	let s_cases cases =
		String.concat "\n" (List.map s_case cases)

	let select_column subjects cases =
		let rec loop i patterns = match patterns with
			| ((PatVariable _ | PatAny | PatExtractor _),_) :: patterns -> loop (i + 1) patterns
			| (PatBind(_,pat1),_) :: patterns -> loop i (pat1 :: patterns)
			| [] -> 0
			| _ -> i
		in
		let _,_,patterns = List.hd cases in
		let i = loop 0 patterns in
		let subjects,cases = if i = 0 then
			subjects,cases
		else begin
			let rec sort i cur acc l = match l with
				| x :: l ->
					if i = cur then x :: acc @ l
					else sort i (cur + 1) (x :: acc) l
				| [] ->
					acc
			in
			let subjects = sort i 0 [] subjects in
			let cases = List.map (fun (case,bindings,patterns) ->
				let patterns = sort i 0 [] patterns in
				case,bindings,patterns
			) cases in
			subjects,cases
		end in
		subjects,cases

	let rec compile mctx subjects cases = match cases with
		| [] ->
			fail mctx (match subjects with e :: _ -> e.epos | _ -> mctx.match_pos);
		| (_,_,patterns) as case :: cases when List.for_all is_wildcard_pattern patterns ->
			compile_leaf mctx subjects case cases
		| _ ->
			let cases = expand cases in
			let subjects,cases = select_column subjects cases in
			let cases = expand cases in (* TODO: is this really necessary? *)
			try
				compile_switch mctx subjects cases
			with Extractor ->
				compile_extractors mctx subjects cases

	and compile_leaf mctx subjects (case,bindings,patterns) cases =
		if mctx.match_debug then print_endline (Printf.sprintf "compile_leaf:\n\tsubjects: %s\n\tcase: %s\n\tcases: %s" (s_subjects subjects) (s_case (case,bindings,patterns)) (s_cases cases));
		let dt = leaf mctx case in
		let dt = match case.case_guard with
			| None ->
				dt
			| Some e ->
				let dt2 = compile mctx subjects cases in
				guard mctx e dt dt2
		in
		let rec loop patterns el bindings = match patterns,el with
			| [PatAny,_],_ ->
				bindings
			| (PatVariable v,p) :: patterns,e :: el ->
				loop patterns el ((v,p,e) :: bindings)
			| (PatBind(v,pat1),p) :: patterns,e :: el ->
				loop (pat1 :: patterns) (e :: el) ((v,p,e) :: bindings)
			| _ :: patterns,_ :: el ->
				loop patterns el bindings
			| [],[] ->
				bindings
			| [],e :: _ ->
				typing_error "Invalid match: Not enough patterns" e.epos
			| (_,p) :: _,[] ->
				typing_error "Invalid match: Too many patterns" p
		in
		let bindings = loop patterns subjects bindings in
		if bindings = [] then dt else bind mctx (List.rev bindings) dt

	and compile_switch mctx subjects cases =
		let subject,subjects = match subjects with
			| [] -> raise Internal_match_failure
			| subject :: subjects -> subject,subjects
		in
		let get_column_sigma cases =
			let sigma = ConTable.create 0 in
			let unguarded = ConTable.create 0 in
			let null = ref [] in
			List.iter (fun (case,bindings,patterns) ->
				let rec loop bindings pat = match fst pat with
					| PatConstructor((ConConst TNull,_),_) ->
						null := (case,bindings,List.tl patterns) :: !null;
					| PatConstructor(con,patterns) ->
						if case.case_guard = None then ConTable.replace unguarded con true;
						let arg_positions = snd (List.split patterns) in
						ConTable.replace sigma con arg_positions;
					| PatBind(v,pat1) -> loop ((v,pos pat,subject) :: bindings) pat1
					| PatVariable _ | PatAny -> ()
					| PatExtractor _ -> raise Extractor
					| _ -> typing_error ("Unexpected pattern: " ^ (Pattern.to_string pat)) case.case_pos;
				in
				loop bindings (List.hd patterns)
			) cases;
			let sigma = ConTable.fold (fun con arg_positions acc -> (con,ConTable.mem unguarded con,arg_positions) :: acc) sigma [] in
			let sigma = List.sort (fun ((_,p1),_,_)  ((_,p2),_,_) -> p1.pmin - p2.pmin) sigma in
			sigma,List.rev !null
		in
		let sigma,null = get_column_sigma cases in
		if mctx.match_debug then print_endline (Printf.sprintf "compile_switch:\n\tsubject: %s\n\ttsubjects: %s\n\tcases: %s" (s_expr_pretty subject) (s_subjects subjects) (s_cases cases));
		let switch_cases = List.map (fun (con,unguarded,arg_positions) ->
			let sub_subjects = get_sub_subjects mctx subject con arg_positions in
			let rec loop bindings locals sub_subjects = match sub_subjects with
				| e :: sub_subjects ->
					let v = gen_local mctx.ctx e.etype e.epos in
					loop ((v,v.v_pos,e) :: bindings) ((mk (TLocal v) v.v_type v.v_pos) :: locals) sub_subjects
				| [] ->
					List.rev bindings,List.rev locals
			in
			let bindings,sub_subjects = loop [] [] sub_subjects in
			let subjects = sub_subjects @ subjects in
			let spec = specialize subject con cases in
			let dt = compile mctx subjects spec in
			let dt = bind mctx bindings dt in
			con,unguarded,dt
		) sigma in
		let default = default subject cases in
		let switch_default = compile mctx subjects default in
		let dt = if switch_cases = [] then switch_default else switch mctx subject switch_cases switch_default in
		let null_guard dt_null =
			guard_null mctx subject dt_null dt
		in
		match null with
			| [] ->
				if is_explicit_null subject.etype then null_guard switch_default else dt
			| cases ->
				let dt_null = compile mctx subjects (cases @ default) in
				null_guard dt_null

	and compile_extractors mctx subjects cases =
		let subject,subjects = match subjects with
			| [] -> raise Internal_match_failure
			| subject :: subjects -> subject,subjects
		in
		if mctx.match_debug then print_endline (Printf.sprintf "compile_extractor:\n\tsubject: %s\n\ttsubjects: %s\n\tcases: %s" (s_expr_pretty subject) (s_subjects subjects) (s_cases cases));
		let num_extractors,extractors = List.fold_left (fun (i,extractors) (_,_,patterns) ->
			let rec loop bindings pat = match pat with
				| (PatExtractor(v,e1,pat),_) -> i + 1,Some (v,e1,pat,bindings) :: extractors
				| (PatBind(v,pat1),p) -> loop ((v,p,subject) :: bindings) pat1
				| _ -> i,None :: extractors
			in
			loop [] (List.hd patterns)
		) (0,[]) cases in
		let pat_any = (PatAny,null_pos) in
		let _,_,ex_subjects,cases,bindings = List.fold_left2 (fun (left,right,subjects,cases,ex_bindings) (case,bindings,patterns) extractor -> match extractor,patterns with
			| Some(v,e1,pat,bindings1), _ :: patterns ->
				let rec loop e = match e.eexpr with
					| TLocal v' when v' == v -> subject
					| _ -> Type.map_expr loop e
				in
				let e1 = loop e1 in
				let bindings = bindings1 @ bindings in
				begin try
					let v,_,_,left2,right2 = List.find (fun (_,_,e2,_,_) -> Texpr.equal e1 e2) ex_bindings in
					let ev = mk (TLocal v) v.v_type e1.epos in
					let patterns = make_offset_list (left2 + 1) (right2 - 1) pat pat_any @ patterns in
					(left + 1, right - 1,ev :: subjects,((case,bindings,patterns) :: cases),ex_bindings)
				with Not_found ->
					let v = alloc_var VExtractorVariable "_hx_tmp" e1.etype e1.epos in
					let ex_bindings = (v,e1.epos,e1,left,right) :: ex_bindings in
					let patterns = make_offset_list (left + 1) (right - 1) pat pat_any @ patterns in
					let ev = mk (TLocal v) v.v_type e1.epos in
					(left + 1, right - 1,ev :: subjects,((case,bindings,patterns) :: cases),ex_bindings)
				end
			| None,pat :: patterns ->
				let patterns = make_offset_list 0 num_extractors pat pat_any @ patterns in
				(left,right,subjects,((case,bindings,patterns) :: cases),ex_bindings)
			| _,[] ->
				die "" __LOC__
		) (0,num_extractors,[],[],[]) cases (List.rev extractors) in
		let dt = compile mctx ((subject :: List.rev ex_subjects) @ subjects) (List.rev cases) in
		let bindings = List.map (fun (a,b,c,_,_) -> (a,b,c)) bindings in
		bind mctx bindings dt

	let compile ctx match_debug subjects cases p =
		let mctx = {
			ctx = ctx;
			match_debug = match_debug;
			dt_table = DtTable.create 7;
			match_pos = p;
			dt_count = 0;
		} in
		let rec loop (subjects,vars) el = match el with
			| [] ->
				List.rev subjects,List.rev vars
			| e :: el ->
				let subjects,vars = match e.eexpr with
				| TConst _ | TLocal _ ->
					(e :: subjects,vars)
				| _ ->
					let v = gen_local ctx e.etype e.epos in
					let ev = mk (TLocal v) e.etype e.epos in
					(ev :: subjects,(v,e.epos,e) :: vars)
				in
				loop (subjects,vars) el
		in
		let subjects,vars = loop ([],[]) subjects in
		begin match cases,subjects with
		| [],(subject :: _) ->
			let dt_fail = fail mctx subject.epos in
			switch mctx subject [] dt_fail
		| _ ->
			let dt = compile mctx subjects cases in
			Useless.check mctx.ctx cases;
			match vars with
				| [] -> dt
				| _ -> bind mctx vars dt
		end
end

module TexprConverter = struct
	open Typecore
	open Decision_tree
	open Constructor
	open Case

	type match_kind =
		| SKValue
		| SKEnum
		| SKLength

	let s_subject v_lookup s e =
		let rec loop top s e = match e.eexpr with
			| TField(_,FEnum(en,ef)) ->
				s
			| TField(e1,fa) ->
				if top then loop false s e1
				else loop false (Printf.sprintf "{ %s: %s }" (field_name fa) s) e1
			| TEnumParameter(e1,ef,i) ->
				let arity = match follow ef.ef_type with TFun(args,_) -> List.length args | _ -> die "" __LOC__ in
				let l = make_offset_list i (arity - i - 1) s "_" in
				loop false (Printf.sprintf "%s(%s)" ef.ef_name (String.concat ", " l)) e1
			| TLocal v ->
				begin try
					loop top s (IntMap.find v.v_id v_lookup)
				with Not_found ->
					s
				end
			| _ ->
				s
		in
		loop true s e

	let s_match_kind = function
		| SKValue -> "value"
		| SKEnum -> "enum"
		| SKLength -> "length"

	let unify_constructor ctx params t con =
		match fst con with
		| ConEnum(en,ef) ->
			let t_ef = match follow ef.ef_type with TFun(_,t) -> t | _ -> ef.ef_type in
			let t_ef = apply_params ctx.type_params params (monomorphs en.e_params (monomorphs ef.ef_params t_ef)) in
			let monos = List.map (fun t -> match follow t with
				| TInst({cl_kind = KTypeParameter _},_) | TMono _ -> mk_mono()
				| _ -> t
			) params in
			let rec duplicate_monos t = match follow t with
				| TMono _ -> mk_mono()
				| _ -> Type.map duplicate_monos t
			in
			let t_e = apply_params ctx.type_params monos (duplicate_monos t) in
			begin try
				Type.unify t_ef t_e;
				Some(con,monos)
			with Unify_error _ ->
				None
			end
		| _ ->
			Some(con,params)

	let rec extract_const e = match e.eexpr with
		| TConst ct -> Some ct
		| TCast(e1,None) -> extract_const e1
		| _ -> None

	let all_ctors ctx e cases =
		let infer_type() = match cases with
			| [] -> e,e.etype,false
			| (con,_,_) :: _ ->
				let fail() =
					(* error "Could not determine switch kind, make sure the type is known" e.epos; *)
					t_dynamic
				in
				let t = match fst con with
					| ConEnum(en,_) -> TEnum(en,extract_param_types en.e_params)
					| ConArray _ -> ctx.t.tarray t_dynamic
					| ConConst ct ->
						begin match ct with
							| TString _ -> ctx.t.tstring
							| TInt _ -> ctx.t.tint
							| TFloat _ -> ctx.t.tfloat
							| TBool _ -> ctx.t.tbool
							| _ -> fail()
						end
					| ConStatic({cl_kind = KAbstractImpl a},_) -> (TAbstract(a,extract_param_types a.a_params))
					| ConTypeExpr mt -> get_general_module_type ctx mt e.epos
					| ConFields _ | ConStatic _ -> fail()
				in
				e,t,true
		in
		let e,t,inferred = match follow e.etype with
			| TDynamic _ | TMono _ ->
				infer_type()
			| _ ->
				e,e.etype,false
		in
		let h = ConTable.create 0 in
		let add constructor =
			ConTable.replace h constructor true
		in
		let rec loop t = match follow t with
			| TAbstract({a_path = [],"Bool"},_) ->
				add (ConConst(TBool true),null_pos);
				add (ConConst(TBool false),null_pos);
				SKValue,RunTimeFinite
			| TAbstract({a_impl = Some c} as a,pl) when a.a_enum ->
				List.iter (fun cf ->
					ignore(follow cf.cf_type);
					if has_class_field_flag cf CfImpl && has_class_field_flag cf CfEnum then match cf.cf_expr with
						| Some e ->
							begin match extract_const e with
							| Some ct -> if ct <> TNull then add (ConConst ct,null_pos)
							| None -> add (ConStatic(c,cf),null_pos)
							end;
						| _ -> add (ConStatic(c,cf),null_pos)
				) c.cl_ordered_statics;
				SKValue,CompileTimeFinite
			| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				loop (Abstract.get_underlying_type a pl)
			| TInst({cl_path=[],"String"},_)
			| TInst({cl_kind = KTypeParameter _ },_) ->
				SKValue,Infinite
			| TInst({cl_path=[],"Array"},_) ->
				SKLength,Infinite
			| TEnum(en,pl) ->
				PMap.iter (fun _ ef -> add (ConEnum(en,ef),null_pos)) en.e_constrs;
				SKEnum,RunTimeFinite
			| TAnon _ ->
				SKValue,Infinite
			| TInst(_,_) ->
				SKValue,Infinite
			| _ ->
				SKValue,Infinite
		in
		let kind,finiteness = loop t in
		let compatible_kind con = match fst con with
			| ConEnum _ -> kind = SKEnum
			| ConArray _ -> kind = SKLength
			| _ -> kind = SKValue
		in
		List.iter (fun (con,unguarded,dt) ->
			if not (compatible_kind con) then typing_error "Incompatible pattern" dt.dt_pos;
			if unguarded then ConTable.remove h con
		) cases;
		let unmatched = ConTable.fold (fun con _ acc -> con :: acc) h [] in
		e,unmatched,kind,finiteness

	let report_not_exhaustive v_lookup e_subject unmatched =
		let sl = match follow e_subject.etype with
			| TAbstract({a_impl = Some c} as a,tl) when a.a_enum ->
				List.map (fun (con,_) -> match fst con with
					| ConConst ct1 ->
						let cf = List.find (fun cf ->
							match cf.cf_expr with
							| Some e ->
								begin match extract_const e with
								| Some ct2 -> ct1 = ct2
								| None -> false
								end
							| _ -> false
						) c.cl_ordered_statics in
						cf.cf_name
					| _ ->
						Constructor.to_string con
				) unmatched
			| _ ->
				List.map (fun (con,_) -> Constructor.to_string con) unmatched
		in
		let s = match unmatched with
			| [] -> "_"
			| _ -> String.concat " | " (List.sort Pervasives.compare sl)
		in
		typing_error (Printf.sprintf "Unmatched patterns: %s" (s_subject v_lookup s e_subject)) e_subject.epos

	type dt_recursion =
		| Toplevel
		| AfterSwitch
		| Deep

	let to_texpr ctx t_switch match_debug with_type dt =
		let v_lookup = ref IntMap.empty in
		let com = ctx.com in
		let p = dt.dt_pos in
		let c_type = match follow (Typeload.load_instance ctx (mk_type_path (["std"],"Type"),p) true) with TInst(c,_) -> c | t -> die "" __LOC__ in
		let mk_index_call e =
			if not ctx.com.is_macro_context && not ctx.com.display.DisplayMode.dms_full_typing then
				(* If we are in display mode there's a chance that these fields don't exist. Let's just use a
				   (correctly typed) neutral value because it doesn't actually matter. *)
				mk (TConst (TInt (Int32.of_int 0))) ctx.t.tint e.epos
			else
				mk (TEnumIndex e) com.basic.tint e.epos
		in
		let mk_name_call e =
			if not ctx.com.is_macro_context && not ctx.com.display.DisplayMode.dms_full_typing then
				mk (TConst (TString "")) ctx.t.tstring e.epos
			else
				let cf = PMap.find "enumConstructor" c_type.cl_statics in
				make_static_call ctx c_type cf (fun t -> t) [e] com.basic.tstring e.epos
		in
		let rec loop dt_rec params dt = match dt.dt_texpr with
			| Some e ->
				Some e
			| None ->
				let e = match dt.dt_t with
					| Leaf case ->
						begin match case.case_expr with
							| Some e -> Some e
							| None -> Some (mk (TBlock []) ctx.t.tvoid case.case_pos)
						end
					| Switch(_,[(ConFields _,_),_,dt],_) -> (* TODO: Can we improve this by making it more general? *)
						loop dt_rec params dt
					| Switch(e_subject,cases,default) ->
						let dt_rec',toplevel = match dt_rec with
							| Toplevel -> AfterSwitch,true
							| AfterSwitch | Deep -> Deep,false
						in
						let e_subject,unmatched,kind,finiteness = all_ctors ctx e_subject cases in
						let unmatched = ExtList.List.filter_map (unify_constructor ctx params e_subject.etype) unmatched in
						let loop params dt = match loop dt_rec' params dt with
							| None ->
								begin match with_type,finiteness with
								| WithType.NoValue,Infinite when toplevel -> None
								| _,CompileTimeFinite when unmatched = [] -> None
								| _ when ignore_error ctx.com -> None
								| _ -> report_not_exhaustive !v_lookup e_subject unmatched
								end
							| Some e ->
								Some e
						in
						let cases = ExtList.List.filter_map (fun (con,_,dt) -> match unify_constructor ctx params e_subject.etype con with
							| Some(_,params) -> Some (con,dt,params)
							| None -> None
						) cases in
						let group cases =
							let h = DtTable.create 0 in
							List.iter (fun (con,dt,params) ->
								let l,_,_ = try DtTable.find h dt.dt_t with Not_found -> [],dt,params in
								DtTable.replace h dt.dt_t (con :: l,dt,params)
							) cases;
							DtTable.fold (fun _ (cons,dt,params) acc -> (cons,dt,params) :: acc) h []
						in
						let cases = group cases in
						let cases = List.sort (fun (cons1,_,_) (cons2,_,_) -> match cons1,cons2 with
							| (con1 :: _),con2 :: _ -> Constructor.compare con1 con2
							| _ -> -1
						) cases in
						let e_default = match unmatched,finiteness with
							| [],RunTimeFinite ->
								None
							| _ ->
								loop params default
						in
						let cases = ExtList.List.filter_map (fun (cons,dt,params) ->
							let eo = loop params dt in
							begin match eo with
								| None -> None
								| Some e -> Some (List.map (Constructor.to_texpr ctx match_debug) (List.sort Constructor.compare cons),e)
							end
						) cases in
						let is_nullable_subject = is_explicit_null e_subject.etype in
						let e_subject = match kind with
							| SKValue -> e_subject
							| SKEnum -> if match_debug then mk_name_call e_subject else mk_index_call e_subject
							| SKLength -> type_field_access ctx e_subject "length"
						in
						let e = match cases,e_default,with_type with
							| [_,e2],None,_ when (match finiteness with RunTimeFinite -> true | _ -> false) && not is_nullable_subject ->
								{e2 with etype = t_switch}
							| [[e1],e2],Some _,_
							| [[e1],e2],None,NoValue when ctx.com.platform <> Java (* TODO: problem with TestJava.hx:285 *) ->
								let e_op = mk (TBinop(OpEq,e_subject,e1)) ctx.t.tbool e_subject.epos in
								begin match e2.eexpr with
									| TIf(e_op2,e3,e_default2) when (match e_default,e_default2 with Some(e1),Some(e2) when e1 == e2 -> true | _ -> false) ->
										let eand = binop OpBoolAnd e_op e_op2 ctx.t.tbool (punion e_op.epos e_op2.epos) in
										mk (TIf(eand,e3,e_default)) t_switch dt.dt_pos
									| _ ->
										mk (TIf(e_op,e2,e_default)) t_switch dt.dt_pos
								end
							| [([{eexpr = TConst (TBool true)}],e1);([{eexpr = TConst (TBool false)}],e2)],None,_
							| [([{eexpr = TConst (TBool false)}],e2);([{eexpr = TConst (TBool true)}],e1)],None,_ ->
								mk (TIf(e_subject,e1,Some e2)) t_switch dt.dt_pos
							| _ ->
								let e_subject = match finiteness with
									| RunTimeFinite | CompileTimeFinite when e_default = None ->
										let meta = (Meta.Exhaustive,[],dt.dt_pos) in
										mk (TMeta(meta,e_subject)) e_subject.etype e_subject.epos
									| _ ->
										e_subject
								in
								mk (TSwitch(e_subject,cases,e_default)) t_switch dt.dt_pos
						in
						Some e
					| Guard(e,dt1,dt2) ->
						(* Normal guards are considered toplevel if we're in the toplevel switch. *)
						let toplevel = match dt_rec with
							| Toplevel | AfterSwitch -> true
							| Deep -> false
						in
						let e_then = loop dt_rec params dt1 in
						begin match e_then with
						| None ->
							None
						| Some e_then ->
							let e_else = loop dt_rec params dt2 in
							begin match e_else with
							| Some e_else ->
								Some (mk (TIf(e,e_then,Some e_else)) t_switch e.epos)
							| None ->
								if with_type = NoValue && toplevel then
									Some (mk (TIf(e,e_then,None)) ctx.t.tvoid e.epos)
								else
									None
							end
						end
					| GuardNull(e,dt1,dt2) ->
						let toplevel = match dt_rec with
							| Toplevel -> true
							| Deep | AfterSwitch -> false
						in
						let e_null = make_null e.etype e.epos in
						let f_op e = mk (TBinop(OpEq,e,e_null)) ctx.t.tbool e.epos in
						let rec loop2 acc dt = match dt.dt_t with
							| GuardNull(e,dt1,dt3) when Decision_tree.equal_dt dt2 dt3 ->
								loop2 ((f_op e) :: acc) dt1
							| Guard(e,dt1,dt3) when Decision_tree.equal_dt dt2 dt3 ->
								loop2 (e :: acc) dt1
							| _ ->
								List.rev acc,dt
						in
						let conds,dt1 = loop2 [] dt1 in
						let e_cond = List.fold_left (fun e1 e2 -> binop OpBoolAnd e1 e2 ctx.t.tbool (punion e1.epos e2.epos)) (f_op e) conds in
						let e_then = loop dt_rec params dt1 in
						begin match e_then with
						| None ->
							if toplevel then
								loop dt_rec params dt2
							else if ignore_error ctx.com then
								Some (mk (TConst TNull) (mk_mono()) dt2.dt_pos)
							else
								report_not_exhaustive !v_lookup e [(ConConst TNull,dt.dt_pos),dt.dt_pos]
						| Some e_then ->
							let e_else = loop dt_rec params dt2 in
							begin match e_else with
							| None ->
								if toplevel then
									Some (mk (TIf(e_cond,e_then,None)) t_switch e_then.epos)
								else
									report_not_exhaustive !v_lookup e []
							| Some e_else ->
								Some (mk (TIf(e_cond,e_then,Some e_else)) t_switch (punion e_then.epos e_else.epos))
							end
						end
					| Bind(bl,dt) ->
						let el = List.map (fun (v,p,e) ->
							v_lookup := IntMap.add v.v_id e !v_lookup;
							mk (TVar(v,Some e)) com.basic.tvoid p
						) bl in
						let e = loop dt_rec params dt in
						Option.map (fun e -> mk (TBlock (el @ [e])) e.etype dt.dt_pos) e;
					| Fail ->
						None
				in
				dt.dt_texpr <- e;
				e
		in
		let params = extract_param_types ctx.type_params in
		let e = loop Toplevel params dt in
		match e with
		| None ->
			typing_error "Unmatched patterns: _" p;
		| Some e ->
			Texpr.duplicate_tvars e
end

module Match = struct
	open Typecore

	let match_expr ctx e cases def with_type postfix_match p =
		let match_debug = Meta.has (Meta.Custom ":matchDebug") ctx.curfield.cf_meta in
		let rec loop e = match fst e with
			| EArrayDecl el when (match el with [(EFor _ | EWhile _),_] -> false | _ -> true) ->
				let el = List.map (fun e -> type_expr ctx e WithType.value) el in
				let t = tuple_type (List.map (fun e -> e.etype) el) in
				t,el
			| EParenthesis e1 ->
				loop e1
			| _ ->
				let e = type_expr ctx e WithType.value in
				e.etype,[e]
		in
		let t,subjects = loop e in
		let cases = match def with
			| None -> cases
			| Some (eo,p) -> cases @ [[EConst (Ident "_"),p],None,eo,p]
		in
		let tmono,with_type,allow_min_void = match with_type with
			| WithType.WithType(t,src) ->
				(match follow t, src with
				| ((TMono _) | (TAbstract({a_path=[],"Void"},_))), Some ImplicitReturn -> Some t, WithType.Value src, true
				| TMono _, _ -> Some t,WithType.value,false
				| _ -> None,with_type,false)
			| _ -> None,with_type,false
		in
		let cases = List.map (fun (el,eg,eo,p) ->
			let p = match eo with Some e when p = null_pos -> pos e | _ -> p in
			let case,bindings,pat = Case.make ctx t el eg eo with_type postfix_match p in
			case,bindings,[pat]
		) cases in
		let infer_switch_type () =
			match with_type with
				| WithType.NoValue -> ctx.t.tvoid
				| WithType.Value(_) ->
					begin match cases with
					| [] ->
						(* If there are no cases we assume Void. This then causes a "Cannot use Void as value" error.
						   Note that we cannot rely on an exhaustiveness error because the switch could be over an empty enum. *)
						ctx.t.tvoid
					| _ ->
						let el = List.map (fun (case,_,_) ->
							match case.Case.case_expr with
							| Some e ->
								(* If we have a block, use the position of the last element. *)
								begin match e.eexpr with
								| TBlock el when el <> [] -> List.hd (List.rev el)
								| _ -> e
								end
							| None ->
								(* If we have no block we have to use the `case pattern` position because that's all we have. *)
								mk (TBlock []) ctx.t.tvoid case.Case.case_pos
						) cases in
						if allow_min_void then unify_min_for_type_source ctx el (Some WithType.ImplicitReturn)
						else unify_min ctx el
					end
				| WithType.WithType(t,_) -> t
		in
		if match_debug then begin
			print_endline "CASES BEGIN";
			List.iter (fun (case,_,patterns) ->
				print_endline (String.concat "" (List.map (Pattern.to_string) patterns));
			) cases;
			print_endline "CASES END";
		end;
		let dt = Compile.compile ctx match_debug subjects cases p in
		if match_debug then begin
			print_endline "DECISION TREE BEGIN";
			print_endline (Decision_tree.to_string dt);
			print_endline "DECISION TREE END";
		end;
		let e =
			let t_switch = infer_switch_type() in
			(match tmono with
			| Some t when allow_min_void && ExtType.is_void (follow t) -> ()
			| Some t -> unify ctx t_switch t p
			| _ -> ()
			);
			TexprConverter.to_texpr ctx t_switch match_debug with_type dt
		in
		if match_debug then begin
			print_endline "TEXPR BEGIN";
			print_endline (s_expr_pretty e);
			print_endline "TEXPR END";
		end;
		{e with epos = p}

	let match_expr ctx e cases def with_type postfix_match p = match cases,def with
		| [],None when (match with_type with WithType.NoValue -> true | _ -> false) ->
			type_expr ctx e WithType.value
		| _ ->
			match_expr ctx e cases def with_type postfix_match p
end
