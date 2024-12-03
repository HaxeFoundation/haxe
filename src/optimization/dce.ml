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

open Ast
open Common
open Type
open Globals

type dce_mode =
	| DceNo
	| DceStd
	| DceFull

type dce = {
	com : context;
	full : bool;
	std_dirs : string list;
	debug : bool;
	follow_expr : dce -> texpr -> unit;
	dependent_types : (string list * string,module_type list) Hashtbl.t;
	mutable curclass : tclass;
	mutable added_fields : (tclass * tclass_field * class_field_ref_kind) list;
	mutable marked_fields : tclass_field list;
	mutable marked_maybe_fields : tclass_field list;
	mutable t_stack : t list;
	mutable ts_stack : t list;
	mutable features : (string, class_field_ref list ref) Hashtbl.t;
}

let push_class dce c =
	let old = dce.curclass in
	dce.curclass <- c;
	(fun () ->
		dce.curclass <- old
	)

let resolve_class_field_ref ctx cfr =
	let ctx = if cfr.cfr_is_macro && not ctx.is_macro_context then Option.get (ctx.get_macros()) else ctx in
	let m = ctx.module_lut#find_by_type cfr.cfr_path in

	Option.get (ExtList.List.find_map (fun mt -> match mt with
		| TClassDecl c when c.cl_path = cfr.cfr_path ->
			let cf = find_field c cfr.cfr_field cfr.cfr_kind in
			Some (c, cf)
		| _ -> None
	) m.m_types)

(* checking *)

(* check for @:keepSub metadata, which forces @:keep on child classes *)
let rec super_forces_keep c =
	Meta.has Meta.KeepSub c.cl_meta || match c.cl_super with
	| Some (csup,_) -> super_forces_keep csup
	| _ -> false

let overrides_extern_field cf c =
	let is_extern c cf = (has_class_flag c CExtern) && cf.cf_expr = None in
	let rec loop c cf =
		match c.cl_super with
		| None -> false
		| Some (c,_) ->
			try
				let cf = PMap.find cf.cf_name c.cl_fields in
				if is_extern c cf then
					true
				else
					loop c cf
			with Not_found ->
				false
	in
	loop c cf

let is_std_file dce file =
	List.exists (fun dir -> ExtString.String.starts_with file dir) dce.std_dirs

let keep_metas = [Meta.Keep;Meta.Expose]

(* check if a class is kept entirely *)
let keep_whole_class dce c =
	Meta.has_one_of keep_metas c.cl_meta
	|| not (dce.full || is_std_file dce (Path.UniqueKey.lazy_path c.cl_module.m_extra.m_file) || has_meta Meta.Dce c.cl_meta)
	|| super_forces_keep c
	|| (match c with
		| { cl_path = ([],("Math"|"Array"))} when dce.com.platform = Js -> false
		| { cl_path = ["flash";"_Boot"],"RealBoot" } -> true
		| _ when (has_class_flag c CExtern) -> true
		| { cl_path = [],"String" }
		| { cl_path = [],"Array" } -> not (dce.com.platform = Js)
		| _ -> false)

let keep_whole_enum dce en =
	Meta.has_one_of keep_metas en.e_meta
	|| not (dce.full || is_std_file dce (Path.UniqueKey.lazy_path en.e_module.m_extra.m_file) || has_meta Meta.Dce en.e_meta)

let mk_used_meta pos =
	Meta.Used,[],(mk_zero_range_pos pos)

let mk_keep_meta pos =
	Meta.Keep,[],(mk_zero_range_pos pos)

(*
	Check if a field is kept.
	`keep_field` is checked to determine the DCE entry points, i.e. all fields that have `@:keep` or kept for other reasons.
	And then it is used at the end to check which fields can be filtered from their classes.
*)
let rec keep_field dce cf c kind =
	let is_static = kind = CfrStatic in
	Meta.has_one_of keep_metas cf.cf_meta
	|| has_class_field_flag cf CfUsed
	|| cf.cf_name = "__init__"
	|| has_class_field_flag cf CfExtern
	|| (not is_static && overrides_extern_field cf c)
	|| (
		kind = CfrConstructor
		&& match c.cl_super with (* parent class kept constructor *)
			| Some ({ cl_constructor = Some ctor } as csup, _) -> keep_field dce ctor csup CfrConstructor
			| _ -> false
	)
	|| begin
		let check_accessor prefix =
			try
				let fields = if is_static then c.cl_statics else c.cl_fields in
				let accessor = PMap.find (prefix ^ cf.cf_name) fields in
				keep_field dce accessor c kind
			with Not_found -> false
		in
		match cf.cf_kind with
		| Var { v_read = AccCall } -> check_accessor "get_"
		| Var { v_write = AccCall } -> check_accessor "set_"
		| _ -> false
	end

(* marking *)

let rec check_feature dce s =
	try
		let l = Hashtbl.find dce.features s in
		List.iter (fun cfr ->
			let (c, cf) = resolve_class_field_ref dce.com cfr in
			mark_field dce c cf cfr.cfr_kind
		) !l;
		Hashtbl.remove dce.features s;
	with Not_found ->
		()

and check_and_add_feature dce s =
	check_feature dce s;
	assert (dce.curclass != null_class);
	Hashtbl.replace dce.curclass.cl_module.m_extra.m_features s true

(* mark a field as kept *)
and mark_field dce c cf kind =
	let add c' cf =
		if not (has_class_field_flag cf CfUsed) then begin
			add_class_field_flag cf CfUsed;
			dce.added_fields <- (c',cf,kind) :: dce.added_fields;
			dce.marked_fields <- cf :: dce.marked_fields;
			check_feature dce (Printf.sprintf "%s.%s" (s_type_path c.cl_path) cf.cf_name);
		end
	in
	match kind with
	| CfrConstructor ->
		let rec loop c =
			begin match c.cl_constructor with
				| Some cf -> add c cf
				| None -> ()
			end;
			match c.cl_super with
			| Some(csup,_) -> loop csup
			| None -> ()
		in
		loop c
	| CfrInit ->
		begin match c.cl_init with
			| Some cf -> add c cf
			| None -> ()
		end
	| CfrStatic | CfrMember ->
		let stat = kind = CfrStatic in
		if not (PMap.mem cf.cf_name (if stat then c.cl_statics else c.cl_fields)) then begin
			match c.cl_super with
			| None -> add c cf
			| Some (c,_) -> mark_field dce c cf kind
		end else
			add c cf;
		if not stat && is_physical_field cf then
			match c.cl_constructor with
				| None -> ()
				| Some ctor -> mark_field dce c ctor CfrConstructor

let rec update_marked_class_fields dce c =
	let pop = push_class dce c in
	(* mark all :?used fields as surely :used now *)
	List.iter (fun cf ->
		if has_class_field_flag cf CfMaybeUsed then mark_field dce c cf CfrStatic
	) c.cl_ordered_statics;
	List.iter (fun cf ->
		if has_class_field_flag cf CfMaybeUsed then mark_field dce c cf CfrMember
	) c.cl_ordered_fields;
	(* we always have to keep super classes and implemented interfaces *)
	(match TClass.get_cl_init c with None -> () | Some init -> dce.follow_expr dce init);
	List.iter (fun (c,_) -> mark_class dce c) c.cl_implements;
	(match c.cl_super with None -> () | Some (csup,pl) -> mark_class dce csup);
	pop()

(* mark a class as kept. If the class has fields marked as @:?keep, make sure to keep them *)
and mark_class dce c = if not (has_class_flag c CUsed) then begin
	add_class_flag c CUsed;
	check_feature dce (Printf.sprintf "%s.*" (s_type_path c.cl_path));
	update_marked_class_fields dce c;
end

let rec mark_enum dce e = if not (Meta.has Meta.Used e.e_meta) then begin
	e.e_meta <- (mk_used_meta e.e_pos) :: e.e_meta;
	check_and_add_feature dce "has_enum";
	check_feature dce (Printf.sprintf "%s.*" (s_type_path e.e_path));
	PMap.iter (fun _ ef -> mark_t dce ef.ef_pos ef.ef_type) e.e_constrs;
end

and mark_abstract dce a = if not (Meta.has Meta.Used a.a_meta) then begin
	check_feature dce (Printf.sprintf "%s.*" (s_type_path a.a_path));
	a.a_meta <- (mk_used_meta a.a_pos) :: a.a_meta
end

(* mark a type as kept *)
and mark_t dce p t =
	if not (List.exists (fun t2 -> Type.fast_eq t t2) dce.t_stack) then begin
		dce.t_stack <- t :: dce.t_stack;
		begin match follow t with
		| TInst({cl_kind = KTypeParameter ttp} as c,pl) ->
			if not (has_class_flag c CUsed) then begin
				add_class_flag c CUsed;
				List.iter (mark_t dce p) (get_constraints ttp);
			end;
			List.iter (mark_t dce p) pl
		| TInst(c,pl) ->
			mark_class dce c;
			List.iter (mark_t dce p) pl
		| TFun(args,ret) ->
			List.iter (fun (_,_,t) -> mark_t dce p t) args;
			mark_t dce p ret
		| TEnum(e,pl) ->
			mark_enum dce e;
			List.iter (mark_t dce p) pl
		| TAbstract(a,pl) when Meta.has Meta.MultiType a.a_meta ->
			begin try
				mark_t dce p (snd (AbstractCast.find_multitype_specialization dce.com a pl p))
			with Error.Error _ ->
				()
			end
		| TAbstract(a,pl) ->
			mark_abstract dce a;
			List.iter (mark_t dce p) pl;
			if not (Meta.has Meta.CoreType a.a_meta) then
				mark_t dce p (Abstract.get_underlying_type a pl)
		| TLazy _ | TDynamic _ | TType _ | TAnon _ | TMono _ -> ()
		end;
		dce.t_stack <- List.tl dce.t_stack
	end

let mark_mt dce mt = match mt with
	| TClassDecl c ->
		mark_class dce c;
	| TEnumDecl e ->
		mark_enum dce e
	| TAbstractDecl a ->
		(* abstract 'feature' is defined as the abstract type beeing used as a value, not as a type *)
		if not (Meta.has Meta.ValueUsed a.a_meta) then a.a_meta <- (Meta.ValueUsed,[],a.a_pos) :: a.a_meta;
		mark_abstract dce a
	| TTypeDecl _ ->
		()

(* find all dependent fields by checking implementing/subclassing types *)
let mark_dependent_fields dce csup n kind =
	let rec loop c =
		(try
			let stat = kind = CfrStatic in
			let cf = PMap.find n (if stat then c.cl_statics else c.cl_fields) in
			(* if it's clear that the class is kept, the field has to be kept as well. This is also true for
				extern interfaces because we cannot remove fields from them *)
			if has_class_flag c CUsed || ((has_class_flag csup CInterface) && (has_class_flag csup CExtern)) then mark_field dce c cf kind
			(* otherwise it might be kept if the class is kept later, so mark it as :?used *)
			else if not (has_class_field_flag cf CfMaybeUsed) then begin
				add_class_field_flag cf CfMaybeUsed;
				dce.marked_maybe_fields <- cf :: dce.marked_maybe_fields;
			end
		with Not_found ->
			(* if the field is not present on current class, it might come from a base class *)
			(match c.cl_super with None -> () | Some (csup,_) -> loop csup))
	in
	let rec loop_inheritance c =
		loop c;
		List.iter (fun d -> loop_inheritance d) c.cl_descendants;
	in
	loop_inheritance csup

(* expr and field evaluation *)

let opt f e = match e with None -> () | Some e -> f e

let rec to_string dce t = match t with
	| TInst(c,tl) ->
		field dce c "toString" CfrMember;
	| TType(tt,tl) ->
		if not (List.exists (fun t2 -> Type.fast_eq t t2) dce.ts_stack) then begin
			dce.ts_stack <- t :: dce.ts_stack;
			to_string dce (apply_typedef tt tl)
		end
	| TAbstract({a_impl = Some c} as a,tl) ->
		if Meta.has Meta.CoreType a.a_meta then
			field dce c "toString" CfrMember
		else
			to_string dce (Abstract.get_underlying_type a tl)
	| TMono r ->
		(match r.tm_type with
		| Some t -> to_string dce t
		| _ -> ())
	| TLazy f ->
		to_string dce (lazy_type f)
	| TDynamic (Some t) ->
		to_string dce t
	| TEnum _ | TFun _ | TAnon _ | TAbstract({a_impl = None},_) | TDynamic None ->
		(* if we to_string these it does not imply that we need all its sub-types *)
		()

and field dce c n kind =
	(try
		let cf = find_field c n kind in
		mark_field dce c cf kind;
	with Not_found -> try
		if (has_class_flag c CInterface) then begin
			let rec loop cl = match cl with
				| [] -> raise Not_found
				| (c,_) :: cl ->
					try field dce c n kind with Not_found -> loop cl
			in
			loop c.cl_implements
		end else match c.cl_super with Some (csup,_) -> field dce csup n kind | None -> raise Not_found
	with Not_found -> try
		match c.cl_kind with
		| KTypeParameter ttp ->
			let rec loop tl = match tl with
				| [] -> raise Not_found
				| TInst(c,_) :: cl ->
					(try field dce c n kind with Not_found -> loop cl)
				| t :: tl ->
					loop tl
			in
			loop (get_constraints ttp)
		| _ -> raise Not_found
	with Not_found ->
		if dce.debug then prerr_endline ("[DCE] Field " ^ n ^ " not found on " ^ (s_type_path c.cl_path)) else ())

and mark_directly_used_class dce c =
	(* don't add @:directlyUsed if it's used within the class itself. this can happen with extern inline methods *)
	if c != dce.curclass && not (Meta.has Meta.DirectlyUsed c.cl_meta) then
		c.cl_meta <- (Meta.DirectlyUsed,[],c.cl_pos) :: c.cl_meta

and mark_directly_used_enum e =
	if not (Meta.has Meta.DirectlyUsed e.e_meta) then
		e.e_meta <- (Meta.DirectlyUsed,[],e.e_pos) :: e.e_meta

and mark_directly_used_mt dce mt =
	match mt with
	| TClassDecl c ->
		mark_directly_used_class dce c
	| TEnumDecl e ->
		mark_directly_used_enum e
	| _ ->
		()

and mark_directly_used_t dce p t =
	match follow t with
	| TInst({cl_kind = KNormal} as c,pl) ->
		mark_directly_used_class dce c;
		List.iter (mark_directly_used_t dce p) pl
	| TEnum(e,pl) ->
		mark_directly_used_enum e;
		List.iter (mark_directly_used_t dce p) pl
	| TAbstract(a,pl) when Meta.has Meta.MultiType a.a_meta ->
		begin try (* this is copy-pasted from mark_t *)
			mark_directly_used_t dce p (snd (AbstractCast.find_multitype_specialization dce.com a pl p))
		with Error.Error _ ->
			()
		end
	| TAbstract(a,pl) ->
		List.iter (mark_directly_used_t dce p) pl;
		if not (Meta.has Meta.CoreType a.a_meta) then
			mark_directly_used_t dce p (Abstract.get_underlying_type a pl)
	| _ ->
		()

and check_dynamic_write dce fa =
	let n = field_name fa in
	check_and_add_feature dce ("dynamic_write");
	check_and_add_feature dce ("dynamic_write." ^ n)

and check_anon_optional_write dce fa =
	let n = field_name fa in
	check_and_add_feature dce ("anon_optional_write");
	check_and_add_feature dce ("anon_optional_write." ^ n)

and check_anon_write dce fa =
	let n = field_name fa in
	check_and_add_feature dce ("anon_write");
	check_and_add_feature dce ("anon_write." ^ n)

and is_array t = match follow t with
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) -> is_array (Abstract.get_underlying_type a tl)
	| TInst({ cl_path = ([], "Array")},_) -> true
	| _ -> false

and is_dynamic t = match follow t with
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) -> is_dynamic (Abstract.get_underlying_type a tl)
	| TDynamic _ -> true
	| _ -> false

and is_string t = match follow t with
	| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) -> is_string (Abstract.get_underlying_type a tl)
	| TInst( { cl_path = ([], "String")}, _) -> true
	| _ -> false

and is_const_string e = match e.eexpr with
	| TConst(TString(_)) -> true
	| _ -> false

and expr_field dce e fa is_call_expr =
	let do_default = fun () ->
		let n = field_name fa in
			(match fa with
			| FAnon cf ->
				if Meta.has Meta.Optional cf.cf_meta then begin
					check_and_add_feature dce "anon_optional_read";
					check_and_add_feature dce ("anon_optional_read." ^ n);
				end else begin
					check_and_add_feature dce "anon_read";
					check_and_add_feature dce ("anon_read." ^ n);
				end
			| FDynamic _ ->
				check_and_add_feature dce "dynamic_read";
				check_and_add_feature dce ("dynamic_read." ^ n);
			| FClosure _ ->
				check_and_add_feature dce "closure_read";
				check_and_add_feature dce ("closure_read." ^ n);
			| _ -> ());
			begin match follow e.etype, fa with
				| TInst(c,_), _
				| _, FClosure (Some (c, _), _) ->
					mark_class dce c;
					field dce c n CfrMember;
				| TAnon a, _ ->
					(match !(a.a_status) with
					| ClassStatics c ->
						mark_class dce c;
						field dce c n CfrStatic;
					| _ -> ())


				| _ -> ()
			end
	in
	let mark_instance_field_access c cf =
		if (not is_call_expr && dce.com.platform = Python) then begin
			if c.cl_path = ([], "Array") then begin
				check_and_add_feature dce "closure_Array";
				check_and_add_feature dce ("python.internal.ArrayImpl." ^ cf.cf_name);
				check_and_add_feature dce ("python.internal.ArrayImpl")
			end
			else if c.cl_path = ([], "String") then begin
				check_and_add_feature dce "closure_String";
				check_and_add_feature dce ("python.internal.StringImpl." ^ cf.cf_name);
				check_and_add_feature dce ("python.internal.StringImpl")
			end
		end;
	in
	begin match fa with
		| FStatic(c,cf) ->
			mark_class dce c;
			mark_field dce c cf CfrStatic;
		| FInstance(c,_,cf) ->
			(*mark_instance_field_access c cf;*)
			mark_class dce c;
			mark_field dce c cf CfrMember
		| FClosure (Some(c, _), cf) ->
		 	mark_instance_field_access c cf;
			do_default()
		| FClosure _ ->
			do_default()
		| _ ->
			do_default()
	end;
	expr dce e;

and check_op dce op = match op with
	| OpMod ->
		check_and_add_feature dce "binop_%";
	| OpUShr ->
		check_and_add_feature dce "binop_>>>";
	| OpAssignOp op ->
		check_op dce op
	| _ ->
		()

and expr dce e =
	mark_t dce e.epos e.etype;
	match e.eexpr with
	| TNew(c,pl,el) ->
		mark_class dce c;
		mark_directly_used_class dce c;
		field dce c "new" CfrConstructor;
		List.iter (expr dce) el;
		List.iter (mark_t dce e.epos) pl;
	| TVar (v,e1) ->
		opt (expr dce) e1;
		mark_t dce e.epos v.v_type;
	| TCast(e, Some mt) ->
		check_feature dce "typed_cast";
		mark_mt dce mt;
		mark_directly_used_mt dce mt;
		expr dce e;
	| TObjectDecl(vl) ->
		check_and_add_feature dce "has_anon";
		List.iter (fun (_,e) -> expr dce e) vl;
	| TTypeExpr mt ->
		mark_mt dce mt;
		mark_directly_used_mt dce mt;
	| TTry(e, vl) ->
		expr dce e;
		List.iter (fun (v,e) ->
			if v.v_type != t_dynamic then begin
				check_feature dce "typed_catch";
				mark_directly_used_t dce v.v_pos v.v_type;
			end;
			expr dce e;
			mark_t dce e.epos v.v_type;
		) vl;
	| TCall ({eexpr = TIdent "`trace"},[p;{ eexpr = TObjectDecl(v)}]) ->
		check_and_add_feature dce "has_anon_trace";
		List.iter (fun (_,e) -> expr dce e) v;
		expr dce p;
	| TCall ({eexpr = TIdent "__define_feature__"},[{eexpr = TConst (TString ft)};e]) ->
		Hashtbl.replace dce.curclass.cl_module.m_extra.m_features ft true;
		check_feature dce ft;
		expr dce e;

	(* keep toString method of T when array<T>.join() is called *)
	| TCall ({eexpr = TField(_, FInstance({cl_path = ([],"Array")}, pl, {cf_name="join"}))} as ef, args) ->
		List.iter (fun e -> to_string dce e) pl;
		expr dce ef;
		List.iter (expr dce) args;

	(* keep toString method when the class is argument to Std.string or haxe.Log.trace *)
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = (["haxe"],"Log")} as c))},FStatic (_,{cf_name="trace"}))} as ef, ((e2 :: el) as args))
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = ([],"Std")} as c))},FStatic (_,{cf_name="string"}))} as ef, ((e2 :: el) as args)) ->
		mark_class dce c;
		to_string dce e2.etype;
		begin match el with
			| [{eexpr = TObjectDecl fl}] ->
				begin try
					begin match Expr.field_assoc "customParams" fl with
						| {eexpr = TArrayDecl el} ->
							List.iter (fun e -> to_string dce e.etype) el
						| _ ->
							()
					end
				with Not_found ->
					()
				end
			| _ ->
				()
		end;
		expr dce ef;
		List.iter (expr dce) args;
	| TCall ({eexpr = TConst TSuper} as e,el) ->
		mark_t dce e.epos e.etype;
		List.iter (expr dce) el;
	| TUnop((Increment | Decrement),_,({eexpr = TArray _} as e1)) ->
		check_and_add_feature dce "array_write";
		check_and_add_feature dce "array_read";
		expr dce e1;
	| TBinop((OpAdd | OpAssignOp(OpAdd)),e1,e2) when is_dynamic e1.etype || is_dynamic e2.etype ->
		check_and_add_feature dce "add_dynamic";
		expr dce e1;
		expr dce e2;
	| TBinop( (OpAdd | (OpAssignOp OpAdd)) as op,e1,e2) when ((is_string e1.etype || is_string e2.etype) && not ( is_const_string e1 && is_const_string e2)) ->

		(* add array_write if we're doing += to an array element, since this pattern comes before the array_write one *)
		(match op, e1 with (OpAssignOp _, {eexpr = TArray({etype = t},_)}) when is_array t -> check_and_add_feature dce "array_write" | _ -> ());

		check_and_add_feature dce "unsafe_string_concat";
		expr dce e1;
		expr dce e2;
	| TArray(({etype = TDynamic None} as e1),e2) ->
		check_and_add_feature dce "dynamic_array_read";
		expr dce e1;
		expr dce e2;
	| TBinop(OpAssign, ({eexpr = TArray({etype = TDynamic None},_)} as e1), e2) ->
		check_and_add_feature dce "dynamic_array_write";
		expr dce e1;
		expr dce e2;
	| TBinop(OpAssignOp op, ({eexpr = TArray({etype = TDynamic None},_)} as e1), e2) ->
		check_op dce op;
		check_and_add_feature dce "dynamic_array_write";
		expr dce e1;
		expr dce e2;
	| TArray(({etype = t} as e1),e2) when is_array t ->
		check_and_add_feature dce "array_read";
		expr dce e1;
		expr dce e2;
	| TBinop( (OpAssign | OpAssignOp _), ({eexpr = TArray({etype = t},_)} as e1), e2) when is_array t ->
		check_and_add_feature dce "array_write";
		expr dce e1;
		expr dce e2;
	| TBinop(OpAssign,({eexpr = TField(_,(FDynamic _ as fa) )} as e1),e2) ->
		check_dynamic_write dce fa;
		expr dce e1;
		expr dce e2;
	| TBinop(OpAssign,({eexpr = TField(_,(FAnon cf as fa) )} as e1),e2) ->
		if Meta.has Meta.Optional cf.cf_meta then
			check_anon_optional_write dce fa
		else
			check_anon_write dce fa;
		expr dce e1;
		expr dce e2;
	| TBinop(OpAssignOp op,({eexpr = TField(_,(FDynamic _ as fa) )} as e1),e2) ->
		check_op dce op;
		check_dynamic_write dce fa;
		expr dce e1;
		expr dce e2;
	| TBinop(OpAssignOp op,({eexpr = TField(_,(FAnon cf as fa) )} as e1),e2) ->
		check_op dce op;
		if Meta.has Meta.Optional cf.cf_meta then
			check_anon_optional_write dce fa
		else
			check_anon_write dce fa;
		expr dce e1;
		expr dce e2;
	| TBinop(OpEq,({ etype = t1} as e1), ({ etype = t2} as e2) ) when is_dynamic t1 || is_dynamic t2 ->
		check_and_add_feature dce "dynamic_binop_==";
		expr dce e1;
		expr dce e2;
	| TBinop(OpEq,({ etype = t1} as e1), ({ etype = t2} as e2) ) when ExtType.is_type_param (follow t1) || ExtType.is_type_param (follow t2) ->
		check_and_add_feature dce "type_param_binop_==";
		expr dce e1;
		expr dce e2;
	| TBinop(OpNotEq,({ etype = t1} as e1), ({ etype = t2} as e2) ) when is_dynamic t1 || is_dynamic t2 ->
		check_and_add_feature dce "dynamic_binop_!=";
		expr dce e1;
		expr dce e2;
	| TBinop(OpNotEq,({ etype = t1} as e1), ({ etype = t2} as e2) ) when ExtType.is_type_param (follow t1) || ExtType.is_type_param (follow t2) ->
		check_and_add_feature dce "type_param_binop_!=";
		expr dce e1;
		expr dce e2;
	| TBinop(op,e1,e2) ->
		check_op dce op;
		expr dce e1;
		expr dce e2;
	| TCall(({ eexpr = TField(ef, fa) } as e2), el ) ->
		mark_t dce e2.epos e2.etype;
		expr_field dce ef fa true;
		List.iter (expr dce) el;
	| TField(e,fa) ->
		expr_field dce e fa false;
	| TThrow e ->
		check_and_add_feature dce "has_throw";
		expr dce e;
		let rec loop e =
			to_string dce e.etype;
			Type.iter loop e
		in
		loop e
	| _ ->
		Type.iter (expr dce) e

let fix_accessors com =
	List.iter (fun mt -> match mt with
		(* filter empty abstract implementation classes (issue #1885). *)
		| TClassDecl({cl_kind = KAbstractImpl _} as c) when c.cl_ordered_statics = [] && c.cl_ordered_fields = [] && not (has_class_flag c CUsed) ->
			add_class_flag c CExtern;
		| TClassDecl({cl_kind = KAbstractImpl a} as c) when a.a_enum ->
			let is_runtime_field cf =
				not (has_class_field_flag cf CfEnum)
			in
			(* also filter abstract implementation classes that have only @:enum fields (issue #2858) *)
			if not (List.exists is_runtime_field c.cl_ordered_statics) then
				add_class_flag c CExtern
		| (TClassDecl c) ->
			let rec has_accessor c n stat =
				PMap.mem n (if stat then c.cl_statics else c.cl_fields)
				|| match c.cl_super with Some (csup,_) -> has_accessor csup n stat | None -> false
			in
			let check_prop stat cf =
				(match cf.cf_kind with
				| Var {v_read = AccCall; v_write = a} ->
					let s = "get_" ^ cf.cf_name in
					cf.cf_kind <- Var {v_read = if has_accessor c s stat then AccCall else AccNever; v_write = a}
				| _ -> ());
				(match cf.cf_kind with
				| Var {v_write = AccCall; v_read = a} ->
					let s = "set_" ^ cf.cf_name in
					cf.cf_kind <- Var {v_write = if has_accessor c s stat then AccCall else AccNever; v_read = a}
				| _ -> ())
			in
			List.iter (check_prop true) c.cl_ordered_statics;
			List.iter (check_prop false) c.cl_ordered_fields;
		| _ -> ()
	) com.types

let extract_if_feature meta =
	let rec loop = function
		| [] ->
			[]
		| (Meta.IfFeature,el,_) :: _ ->
			List.map (fun (e,p) -> match e with
				| EConst (String(s,_)) -> s
				| _ -> Error.raise_typing_error "String expected" p
			) el
		| _ :: l ->
			loop l
	in
	loop meta

let collect_entry_points dce com =
	let delayed = ref [] in
	let check_feature cf_ref meta =
		List.iter (fun s ->
			try
				let l = Hashtbl.find dce.features s in
				l := cf_ref :: !l
			with Not_found ->
				Hashtbl.add dce.features s (ref [cf_ref])
		) meta;
	in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			remove_class_flag c CUsed;
			let cl_if_feature = extract_if_feature c.cl_meta in
			let keep_class = keep_whole_class dce c && (not (has_class_flag c CExtern) || (has_class_flag c CInterface)) in
			let is_struct = dce.com.platform = Hl && Meta.has Meta.Struct c.cl_meta in
			let loop kind cf =
				let cf_ref = mk_class_field_ref c cf kind com.is_macro_context in
				let cf_if_feature = extract_if_feature cf.cf_meta in
				check_feature cf_ref (cl_if_feature @ cf_if_feature);
				(* Have to delay mark_field so that we see all @:ifFeature *)
				if keep_class || is_struct || keep_field dce cf c kind then delayed := (fun () -> mark_field dce c cf kind) :: !delayed
			in
			List.iter (loop CfrStatic) c.cl_ordered_statics;
			List.iter (loop CfrMember) c.cl_ordered_fields;
			begin match c.cl_constructor with
				| Some cf -> loop CfrConstructor cf
				| None -> ()
			end;
			begin match c.cl_init with
				| Some cf when keep_class || Meta.has Meta.KeepInit c.cl_meta ->
					loop CfrInit cf
				| _ ->
					()
			end;
		| TEnumDecl en when keep_whole_enum dce en ->
			en.e_meta <- Meta.remove Meta.Used en.e_meta;
			delayed := (fun () ->
				let pop = push_class dce {null_class with cl_module = en.e_module} in
				mark_enum dce en;
				pop()
			) :: !delayed;
		| _ ->
			()
	) com.types;
	List.iter (fun f -> f()) !delayed;
	if dce.debug then begin
		List.iter (fun (c,cf,_) -> match cf.cf_expr with
			| None -> ()
			| Some _ -> print_endline ("[DCE] Entry point: " ^ (s_type_path c.cl_path) ^ "." ^ cf.cf_name)
		) dce.added_fields;
	end

let mark dce =
	let rec loop () =
		match dce.added_fields with
		| [] -> ()
		| cfl ->
			dce.added_fields <- [];
			(* extend to dependent (= overriding/implementing) class fields *)
			List.iter (fun (c,cf,stat) -> mark_dependent_fields dce c cf.cf_name stat) cfl;
			(* mark fields as used *)
			List.iter (fun (c,cf,stat) ->
				let pop = push_class dce c in
				if is_physical_field cf then mark_class dce c;
				mark_field dce c cf stat;
				mark_t dce cf.cf_pos cf.cf_type;
				pop()
			) cfl;
			(* follow expressions to new types/fields *)
			List.iter (fun (c,cf,_) ->
				if not (has_class_flag c CExtern) then begin
					let pop = push_class dce c in
					opt (expr dce) cf.cf_expr;
					List.iter (fun cf -> if cf.cf_expr <> None then opt (expr dce) cf.cf_expr) cf.cf_overloads;
					pop()
				end
			) cfl;
			loop ()
	in
	loop ()

let sweep dce com =
	let rec loop acc types =
		match types with
		| (TClassDecl c) as mt :: l when keep_whole_class dce c ->
			loop (mt :: acc) l
		| (TClassDecl c) as mt :: l ->
			let check_property cf stat =
				let add_accessor_metadata cf =
					if not (Meta.has Meta.Accessor cf.cf_meta) then cf.cf_meta <- (Meta.Accessor,[],c.cl_pos) :: cf.cf_meta
				in
				begin match cf.cf_kind with
				| Var {v_read = AccCall} ->
					begin try
						add_accessor_metadata (PMap.find ("get_" ^ cf.cf_name) (if stat then c.cl_statics else c.cl_fields))
					with Not_found ->
						()
					end
				| _ ->
					()
				end;
				begin match cf.cf_kind with
				| Var {v_write = AccCall} ->
					begin try
						add_accessor_metadata (PMap.find ("set_" ^ cf.cf_name) (if stat then c.cl_statics else c.cl_fields))
					with Not_found ->
						()
					end
				| _ ->
					()
				end;
			in
			(* add :keep so subsequent filter calls do not process class fields again *)
			c.cl_meta <- (mk_keep_meta c.cl_pos) :: c.cl_meta;
 			c.cl_ordered_statics <- List.filter (fun cf ->
				let b = keep_field dce cf c CfrStatic in
				if not b then begin
					if dce.debug then print_endline ("[DCE] Removed field " ^ (s_type_path c.cl_path) ^ "." ^ (cf.cf_name));
					check_property cf true;
					c.cl_statics <- PMap.remove cf.cf_name c.cl_statics;
				end;
				b
			) c.cl_ordered_statics;
			c.cl_ordered_fields <- List.filter (fun cf ->
				let b = keep_field dce cf c CfrMember in
				if not b then begin
					if dce.debug then print_endline ("[DCE] Removed field " ^ (s_type_path c.cl_path) ^ "." ^ (cf.cf_name));
					check_property cf false;
					c.cl_fields <- PMap.remove cf.cf_name c.cl_fields;
				end;
				b
			) c.cl_ordered_fields;
			(match c.cl_constructor with Some cf when not (keep_field dce cf c CfrConstructor) -> c.cl_constructor <- None | _ -> ());
			let inef cf = is_physical_field cf in
			let has_non_extern_fields = List.exists inef c.cl_ordered_fields || List.exists inef c.cl_ordered_statics in
			(* we keep a class if it was used or has a used field *)
			if has_class_flag c CUsed || has_non_extern_fields then loop (mt :: acc) l else begin
				(match TClass.get_cl_init c with
				| Some f when Meta.has Meta.KeepInit c.cl_meta ->
					(* it means that we only need the __init__ block *)
					add_class_flag c CExtern;
					loop (mt :: acc) l
				| _ ->
					if dce.debug then print_endline ("[DCE] Removed class " ^ (s_type_path c.cl_path));
					loop acc l)
			end
 		| (TEnumDecl en) as mt :: l when Meta.has Meta.Used en.e_meta || (has_enum_flag en EnExtern) || keep_whole_enum dce en ->
			loop (mt :: acc) l
		| TEnumDecl e :: l ->
			if dce.debug then print_endline ("[DCE] Removed enum " ^ (s_type_path e.e_path));
			loop acc l
		| mt :: l ->
			loop (mt :: acc) l
		| [] ->
			acc
	in
	com.types <- loop [] (List.rev com.types)

let run com main mode =
	let full = mode = DceFull in
	let dce = {
		com = com;
		full = full;
		dependent_types = Hashtbl.create 0;
		std_dirs = if full then [] else List.map (fun path -> Path.get_full_path path#path) com.class_paths#get_std_paths;
		debug = Common.defined com Define.DceDebug;
		added_fields = [];
		follow_expr = expr;
		marked_fields = [];
		marked_maybe_fields = [];
		t_stack = [];
		ts_stack = [];
		features = Hashtbl.create 0;
		curclass = null_class;
	} in

	(* first step: get all entry points, which is the main method and all class methods which are marked with @:keep *)
	collect_entry_points dce com;

	(* second step: initiate DCE passes and keep going until no new fields were added *)
	mark dce;

	(* third step: filter types *)
	if mode <> DceNo then sweep dce com;

	(* extra step to adjust properties that had accessors removed (required for Php and Cpp) *)
	fix_accessors com;

	(* remove "override" from fields that do not override anything anymore *)
	List.iter (fun mt -> match mt with
		| TClassDecl c ->
			List.iter (fun cf ->
				if has_class_field_flag cf CfOverride then begin
					let rec loop c =
						match c.cl_super with
						| Some (csup,_) when PMap.mem cf.cf_name csup.cl_fields -> true
						| Some (csup,_) -> loop csup
						| None -> false
					in
					let b = loop c in
					if not b then remove_class_field_flag cf CfOverride;
				end
			) c.cl_ordered_fields;
		| _ -> ()
	) com.types;

	(*
		Mark extern classes as really used if they are extended by non-extern ones.
	*)
	List.iter (function
		| TClassDecl ({cl_super = Some (csup, _)} as c) when not (has_class_flag c CExtern) && (has_class_flag csup CExtern) ->
			mark_directly_used_class dce csup
		| TClassDecl c when not (has_class_flag c CExtern) && c.cl_implements <> [] ->
			List.iter (fun (iface,_) -> if ((has_class_flag iface CExtern)) then mark_directly_used_class dce iface) c.cl_implements;
		| _ -> ()
	) com.types;

	(* cleanup added fields metadata - compatibility with compilation server *)
	List.iter (fun cf -> remove_class_field_flag cf CfUsed) dce.marked_fields
