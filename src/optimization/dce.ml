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

open Ast
open Common
open Type
open Globals

type dce = {
	com : context;
	full : bool;
	std_dirs : string list;
	debug : bool;
	follow_expr : dce -> texpr -> unit;
	dependent_types : (string list * string,module_type list) Hashtbl.t;
	mutable curclass : tclass;
	mutable added_fields : (tclass * tclass_field * bool) list;
	mutable marked_fields : tclass_field list;
	mutable marked_maybe_fields : tclass_field list;
	mutable t_stack : t list;
	mutable ts_stack : t list;
	mutable features : (string,(tclass * tclass_field * bool) list) Hashtbl.t;
}

(* checking *)

(* check for @:keepSub metadata, which forces @:keep on child classes *)
let rec super_forces_keep c =
	Meta.has Meta.KeepSub c.cl_meta || match c.cl_super with
	| Some (csup,_) -> super_forces_keep csup
	| _ -> false

let is_std_file dce file =
	List.exists (ExtString.String.starts_with file) dce.std_dirs

(* check if a class is kept entirely *)
let keep_whole_class dce c =
	Meta.has Meta.Keep c.cl_meta
	|| not (dce.full || is_std_file dce c.cl_module.m_extra.m_file || has_meta Meta.Dce c.cl_meta)
	|| super_forces_keep c
	|| (match c with
		| { cl_path = ([],("Math"|"Array"))} when dce.com.platform = Js -> false
		| { cl_path = ([],("Math"|"Array"|"String"))} when dce.com.platform = Lua -> false
		| { cl_extern = true }
		| { cl_path = ["flash";"_Boot"],"RealBoot" } -> true
		| { cl_path = [],"String" }
		| { cl_path = [],"Array" } -> not (dce.com.platform = Js)
		| _ -> false)

let keep_whole_enum dce en =
	Meta.has Meta.Keep en.e_meta
	|| not (dce.full || is_std_file dce en.e_module.m_extra.m_file || has_meta Meta.Dce en.e_meta)

(* check if a field is kept *)
let keep_field dce cf =
	Meta.has Meta.Keep cf.cf_meta
	|| Meta.has Meta.Used cf.cf_meta
	|| cf.cf_name = "__init__"
	|| is_extern_field cf

(* marking *)

let rec check_feature dce s =
	try
		let l = Hashtbl.find dce.features s in
		List.iter (fun (c,cf,stat) ->
			mark_field dce c cf stat
		) l;
		Hashtbl.remove dce.features s;
	with Not_found ->
		()

and check_and_add_feature dce s =
	check_feature dce s;
	Hashtbl.replace dce.curclass.cl_module.m_extra.m_features s true

(* mark a field as kept *)
and mark_field dce c cf stat =
	let add cf =
		if not (Meta.has Meta.Used cf.cf_meta) then begin
			cf.cf_meta <- (Meta.Used,[],cf.cf_pos) :: cf.cf_meta;
			dce.added_fields <- (c,cf,stat) :: dce.added_fields;
			dce.marked_fields <- cf :: dce.marked_fields;
			check_feature dce (Printf.sprintf "%s.%s" (s_type_path c.cl_path) cf.cf_name);
		end
	in
	if cf.cf_name = "new" then begin
		let rec loop c =
			begin match c.cl_constructor with
				| Some cf -> add cf
				| None -> ()
			end;
			match c.cl_super with
			| Some(csup,_) -> loop csup
			| None -> ()
		in
		loop c
	end else begin
		if not (PMap.mem cf.cf_name (if stat then c.cl_statics else c.cl_fields)) then begin
			match c.cl_super with
			| None -> add cf
			| Some (c,_) -> mark_field dce c cf stat
		end else
			add cf
	end

let rec update_marked_class_fields dce c =
	(* mark all :?used fields as surely :used now *)
	List.iter (fun cf ->
		if Meta.has Meta.MaybeUsed cf.cf_meta then mark_field dce c cf true
	) c.cl_ordered_statics;
	List.iter (fun cf ->
		if Meta.has Meta.MaybeUsed cf.cf_meta then mark_field dce c cf false
	) c.cl_ordered_fields;
	(* we always have to keep super classes and implemented interfaces *)
	(match c.cl_init with None -> () | Some init -> dce.follow_expr dce init);
	List.iter (fun (c,_) -> mark_class dce c) c.cl_implements;
	(match c.cl_super with None -> () | Some (csup,pl) -> mark_class dce csup)

(* mark a class as kept. If the class has fields marked as @:?keep, make sure to keep them *)
and mark_class dce c = if not (Meta.has Meta.Used c.cl_meta) then begin
	c.cl_meta <- (Meta.Used,[],c.cl_pos) :: c.cl_meta;
	check_feature dce (Printf.sprintf "%s.*" (s_type_path c.cl_path));
	update_marked_class_fields dce c;
end

let rec mark_enum dce e = if not (Meta.has Meta.Used e.e_meta) then begin
	e.e_meta <- (Meta.Used,[],e.e_pos) :: e.e_meta;

	(* do not generate has_enum feature for @:fakeEnum enums since they are not really enums *)
	if not (Meta.has Meta.FakeEnum e.e_meta) then
		check_and_add_feature dce "has_enum";

	check_feature dce (Printf.sprintf "%s.*" (s_type_path e.e_path));
	PMap.iter (fun _ ef -> mark_t dce ef.ef_pos ef.ef_type) e.e_constrs;
end

and mark_abstract dce a = if not (Meta.has Meta.Used a.a_meta) then begin
	check_feature dce (Printf.sprintf "%s.*" (s_type_path a.a_path));
	a.a_meta <- (Meta.Used,[],a.a_pos) :: a.a_meta
end

(* mark a type as kept *)
and mark_t dce p t =
	if not (List.exists (fun t2 -> Type.fast_eq t t2) dce.t_stack) then begin
		dce.t_stack <- t :: dce.t_stack;
		begin match follow t with
		| TInst({cl_kind = KTypeParameter tl} as c,pl) ->
			if not (Meta.has Meta.Used c.cl_meta) then begin
				c.cl_meta <- (Meta.Used,[],c.cl_pos) :: c.cl_meta;
				List.iter (mark_t dce p) tl;
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
				mark_t dce p (snd (Typecore.AbstractCast.find_multitype_specialization dce.com a pl p))
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
let rec mark_dependent_fields dce csup n stat =
	let rec loop c =
		(try
			let cf = PMap.find n (if stat then c.cl_statics else c.cl_fields) in
			(* if it's clear that the class is kept, the field has to be kept as well. This is also true for
				extern interfaces because we cannot remove fields from them *)
			if Meta.has Meta.Used c.cl_meta || (csup.cl_interface && csup.cl_extern) then mark_field dce c cf stat
			(* otherwise it might be kept if the class is kept later, so mark it as :?used *)
			else if not (Meta.has Meta.MaybeUsed cf.cf_meta) then begin
				cf.cf_meta <- (Meta.MaybeUsed,[],cf.cf_pos) :: cf.cf_meta;
				dce.marked_maybe_fields <- cf :: dce.marked_maybe_fields;
			end
		with Not_found ->
			(* if the field is not present on current class, it might come from a base class *)
			(match c.cl_super with None -> () | Some (csup,_) -> loop csup))
	in
	let rec loop_inheritance c =
		loop c;
		Hashtbl.iter (fun _ d -> loop_inheritance d) c.cl_descendants;
	in
	loop_inheritance csup

(* expr and field evaluation *)

let opt f e = match e with None -> () | Some e -> f e

let rec to_string dce t = match t with
	| TInst(c,tl) ->
		field dce c "toString" false;
	| TType(tt,tl) ->
		if not (List.exists (fun t2 -> Type.fast_eq t t2) dce.ts_stack) then begin
			dce.ts_stack <- t :: dce.ts_stack;
			to_string dce (apply_params tt.t_params tl tt.t_type)
		end
	| TAbstract({a_impl = Some c} as a,tl) ->
		if Meta.has Meta.CoreType a.a_meta then
			field dce c "toString" false
		else
			to_string dce (Abstract.get_underlying_type a tl)
	| TMono r ->
		(match !r with
		| Some t -> to_string dce t
		| _ -> ())
	| TLazy f ->
		to_string dce (!f())
	| TDynamic t ->
		if t == t_dynamic then
			()
		else
			to_string dce t
	| TEnum _ | TFun _ | TAnon _ | TAbstract({a_impl = None},_) ->
		(* if we to_string these it does not imply that we need all its sub-types *)
		()

and field dce c n stat =
	let find_field n =
		if n = "new" then match c.cl_constructor with
			| None -> raise Not_found
			| Some cf -> cf
		else PMap.find n (if stat then c.cl_statics else c.cl_fields)
	in
	(try
		let cf = find_field n in
		mark_field dce c cf stat;
	with Not_found -> try
		if c.cl_interface then begin
			let rec loop cl = match cl with
				| [] -> raise Not_found
				| (c,_) :: cl ->
					try field dce c n stat with Not_found -> loop cl
			in
			loop c.cl_implements
		end else match c.cl_super with Some (csup,_) -> field dce csup n stat | None -> raise Not_found
	with Not_found -> try
		match c.cl_kind with
		| KTypeParameter tl ->
			let rec loop tl = match tl with
				| [] -> raise Not_found
				| TInst(c,_) :: cl ->
					(try field dce c n stat with Not_found -> loop cl)
				| t :: tl ->
					loop tl
			in
			loop tl
		| _ -> raise Not_found
	with Not_found ->
		if dce.debug then prerr_endline ("[DCE] Field " ^ n ^ " not found on " ^ (s_type_path c.cl_path)) else ())

and mark_directly_used_class c =
	if not (Meta.has Meta.DirectlyUsed c.cl_meta) then
		c.cl_meta <- (Meta.DirectlyUsed,[],c.cl_pos) :: c.cl_meta

and mark_directly_used_enum e =
	if not (Meta.has Meta.DirectlyUsed e.e_meta) then
		e.e_meta <- (Meta.DirectlyUsed,[],e.e_pos) :: e.e_meta

and mark_directly_used_mt mt =
	match mt with
	| TClassDecl c ->
		mark_directly_used_class c
	| TEnumDecl e ->
		mark_directly_used_enum e
	| _ ->
		()

and mark_directly_used_t com p t =
	match follow t with
	| TInst({cl_kind = KNormal} as c,pl) ->
		mark_directly_used_class c;
		List.iter (mark_directly_used_t com p) pl
	| TEnum(e,pl) ->
		mark_directly_used_enum e;
		List.iter (mark_directly_used_t com p) pl
	| TAbstract(a,pl) when Meta.has Meta.MultiType a.a_meta ->
		begin try (* this is copy-pasted from mark_t *)
			mark_directly_used_t com p (snd (Typecore.AbstractCast.find_multitype_specialization com a pl p))
		with Error.Error _ ->
			()
		end
	| TAbstract(a,pl) ->
		List.iter (mark_directly_used_t com p) pl;
		if not (Meta.has Meta.CoreType a.a_meta) then
			mark_directly_used_t com p (Abstract.get_underlying_type a pl)
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
			| _ -> ());
			begin match follow e.etype with
				| TInst(c,_) ->
					mark_class dce c;
					field dce c n false;
				| TAnon a ->
					(match !(a.a_status) with
					| Statics c ->
						mark_class dce c;
						field dce c n true;
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
			mark_field dce c cf true;
		| FInstance(c,_,cf) ->
			(*mark_instance_field_access c cf;*)
			mark_class dce c;
			mark_field dce c cf false
		| FClosure (Some(c, _), cf) ->
		 	mark_instance_field_access c cf;
			do_default()
		| FClosure _ ->
			do_default()
		| _ ->
			do_default()
	end;
	expr dce e;


and expr dce e =
	mark_t dce e.epos e.etype;
	match e.eexpr with
	| TNew(c,pl,el) ->
		mark_class dce c;
		mark_directly_used_class c;
		field dce c "new" false;
		List.iter (expr dce) el;
		List.iter (mark_t dce e.epos) pl;
	| TVar (v,e1) ->
		opt (expr dce) e1;
		mark_t dce e.epos v.v_type;
	| TCast(e, Some mt) ->
		check_feature dce "typed_cast";
		mark_mt dce mt;
		mark_directly_used_mt mt;
		expr dce e;
	| TObjectDecl(vl) ->
		check_and_add_feature dce "has_anon";
		List.iter (fun (_,e) -> expr dce e) vl;
	| TTypeExpr mt ->
		mark_mt dce mt;
		mark_directly_used_mt mt;
	| TTry(e, vl) ->
		expr dce e;
		List.iter (fun (v,e) ->
			if v.v_type != t_dynamic then begin
				check_feature dce "typed_catch";
				mark_directly_used_t dce.com v.v_pos v.v_type;
			end;
			expr dce e;
			mark_t dce e.epos v.v_type;
		) vl;
	| TCall ({eexpr = TLocal ({v_name = "`trace"})},[p;{ eexpr = TObjectDecl(v)}]) ->
		check_and_add_feature dce "has_anon_trace";
		List.iter (fun (_,e) -> expr dce e) v;
		expr dce p;
	| TCall ({eexpr = TLocal ({v_name = "__define_feature__"})},[{eexpr = TConst (TString ft)};e]) ->
		Hashtbl.replace dce.curclass.cl_module.m_extra.m_features ft true;
		check_feature dce ft;
		expr dce e;

	(* keep toString method when the class is argument to Std.string or haxe.Log.trace *)
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = (["haxe"],"Log")} as c))},FStatic (_,{cf_name="trace"}))} as ef, ((e2 :: el) as args))
	| TCall ({eexpr = TField({eexpr = TTypeExpr (TClassDecl ({cl_path = ([],"Std")} as c))},FStatic (_,{cf_name="string"}))} as ef, ((e2 :: el) as args)) ->
		mark_class dce c;
		to_string dce e2.etype;
		begin match el with
			| [{eexpr = TObjectDecl fl}] ->
				begin try
					begin match List.assoc "customParams" fl with
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
	| TBinop(OpAdd,e1,e2) when is_dynamic e1.etype || is_dynamic e2.etype ->
		check_and_add_feature dce "add_dynamic";
		expr dce e1;
		expr dce e2;
	| TBinop( (OpAdd | (OpAssignOp OpAdd)) as op,e1,e2) when ((is_string e1.etype || is_string e2.etype) && not ( is_const_string e1 && is_const_string e2)) ->

		(* add array_write if we're doing += to an array element, since this pattern comes before the array_write one *)
		(match op, e1 with (OpAssignOp _, {eexpr = TArray({etype = t},_)}) when is_array t -> check_and_add_feature dce "array_write" | _ -> ());

		check_and_add_feature dce "unsafe_string_concat";
		expr dce e1;
		expr dce e2;
	| TArray(({etype = TDynamic t} as e1),e2) when t == t_dynamic ->
		check_and_add_feature dce "dynamic_array_read";
		expr dce e1;
		expr dce e2;
	| TBinop( (OpAssign | OpAssignOp _), ({eexpr = TArray({etype = TDynamic t},_)} as e1), e2) when t == t_dynamic ->
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
		check_dynamic_write dce fa;
		expr dce e1;
		expr dce e2;
	| TBinop(OpAssignOp op,({eexpr = TField(_,(FAnon cf as fa) )} as e1),e2) ->
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
	| TBinop(OpEq,({ etype = t1} as e1), ({ etype = t2} as e2) ) when is_dynamic t1 || is_dynamic t2 ->
		check_and_add_feature dce "dynamic_binop_!=";
		expr dce e1;
		expr dce e2;
	| TBinop(OpMod,e1,e2) ->
		check_and_add_feature dce "binop_%";
		expr dce e1;
		expr dce e2;
	| TBinop((OpUShr | OpAssignOp OpUShr),e1,e2) ->
		check_and_add_feature dce "binop_>>>";
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
		(*
			TODO: Simon, save me! \o
			This is a hack needed to keep toString field of the actual exception objects
			that are thrown, but are wrapped into HaxeError before DCE comes into play.
		*)
		let e = (match e.eexpr with
			| TNew({cl_path=(["js";"_Boot"],"HaxeError")}, _, [eoriginal]) -> eoriginal
			| _ -> e
		) in
		to_string dce e.etype
	| _ ->
		Type.iter (expr dce) e

let fix_accessors com =
	List.iter (fun mt -> match mt with
		(* filter empty abstract implementation classes (issue #1885). *)
		| TClassDecl({cl_kind = KAbstractImpl _} as c) when c.cl_ordered_statics = [] && c.cl_ordered_fields = [] && not (Meta.has Meta.Used c.cl_meta) ->
			c.cl_extern <- true
		| TClassDecl({cl_kind = KAbstractImpl a} as c) when Meta.has Meta.Enum a.a_meta ->
			let is_runtime_field cf =
				not (Meta.has Meta.Enum cf.cf_meta)
			in
			(* also filter abstract implementation classes that have only @:enum fields (issue #2858) *)
			if not (List.exists is_runtime_field c.cl_ordered_statics) then
				c.cl_extern <- true
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

let run com main full =
	let dce = {
		com = com;
		full = full;
		dependent_types = Hashtbl.create 0;
		std_dirs = if full then [] else List.map Path.unique_full_path com.std_path;
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
	begin match main with
		| Some {eexpr = TCall({eexpr = TField(e,(FStatic(c,cf)))},_)} | Some {eexpr = TBlock ({ eexpr = TCall({eexpr = TField(e,(FStatic(c,cf)))},_)} :: _)} ->
			cf.cf_meta <- (Meta.Keep,[],cf.cf_pos) :: cf.cf_meta
		| _ ->
			()
	end;
	List.iter (fun m ->
		List.iter (fun (s,v) ->
			if Hashtbl.mem dce.features s then Hashtbl.replace dce.features s (v :: Hashtbl.find dce.features s)
			else Hashtbl.add dce.features s [v]
		) m.m_extra.m_if_feature;
	) com.modules;
	(* first step: get all entry points, which is the main method and all class methods which are marked with @:keep *)
	List.iter (fun t -> match t with
		| TClassDecl c ->
			let keep_class = keep_whole_class dce c && (not c.cl_extern || c.cl_interface) in
			let loop stat cf =
				if keep_class || keep_field dce cf then mark_field dce c cf stat
			in
			List.iter (loop true) c.cl_ordered_statics;
			List.iter (loop false) c.cl_ordered_fields;
			begin match c.cl_constructor with
				| Some cf -> loop false cf
				| None -> ()
			end;
			begin match c.cl_init with
				| Some e when keep_class || Meta.has Meta.KeepInit c.cl_meta ->
					(* create a fake field to deal with our internal logic (issue #3286) *)
					let cf = mk_field "__init__" e.etype e.epos null_pos in
					cf.cf_expr <- Some e;
					loop true cf
				| _ ->
					()
			end;
		| TEnumDecl en when keep_whole_enum dce en ->
			mark_enum dce en
		| _ ->
			()
	) com.types;
	if dce.debug then begin
		List.iter (fun (c,cf,_) -> match cf.cf_expr with
			| None -> ()
			| Some _ -> print_endline ("[DCE] Entry point: " ^ (s_type_path c.cl_path) ^ "." ^ cf.cf_name)
		) dce.added_fields;
	end;
	(* second step: initiate DCE passes and keep going until no new fields were added *)
	let rec loop () =
		match dce.added_fields with
		| [] -> ()
		| cfl ->
			dce.added_fields <- [];
			(* extend to dependent (= overriding/implementing) class fields *)
			List.iter (fun (c,cf,stat) -> mark_dependent_fields dce c cf.cf_name stat) cfl;
			(* mark fields as used *)
			List.iter (fun (c,cf,stat) ->
				if not (is_extern_field cf) then mark_class dce c;
				mark_field dce c cf stat;
				mark_t dce cf.cf_pos cf.cf_type
			) cfl;
			(* follow expressions to new types/fields *)
			List.iter (fun (c,cf,_) ->
				dce.curclass <- c;
				opt (expr dce) cf.cf_expr;
				List.iter (fun cf -> if cf.cf_expr <> None then opt (expr dce) cf.cf_expr) cf.cf_overloads
			) cfl;
			loop ()
	in
	loop ();
	(* third step: filter types *)
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
			c.cl_meta <- (Meta.Keep,[],c.cl_pos) :: c.cl_meta;
 			c.cl_ordered_statics <- List.filter (fun cf ->
				let b = keep_field dce cf in
				if not b then begin
					if dce.debug then print_endline ("[DCE] Removed field " ^ (s_type_path c.cl_path) ^ "." ^ (cf.cf_name));
					check_property cf true;
					c.cl_statics <- PMap.remove cf.cf_name c.cl_statics;
				end;
				b
			) c.cl_ordered_statics;
			c.cl_ordered_fields <- List.filter (fun cf ->
				let b = keep_field dce cf in
				if not b then begin
					if dce.debug then print_endline ("[DCE] Removed field " ^ (s_type_path c.cl_path) ^ "." ^ (cf.cf_name));
					check_property cf false;
					c.cl_fields <- PMap.remove cf.cf_name c.cl_fields;
				end;
				b
			) c.cl_ordered_fields;
			(match c.cl_constructor with Some cf when not (keep_field dce cf) -> c.cl_constructor <- None | _ -> ());
			let inef cf = not (is_extern_field cf) in
			let has_non_extern_fields = List.exists inef c.cl_ordered_fields || List.exists inef c.cl_ordered_statics in
			(* we keep a class if it was used or has a used field *)
			if Meta.has Meta.Used c.cl_meta || has_non_extern_fields then loop (mt :: acc) l else begin
				(match c.cl_init with
				| Some f when Meta.has Meta.KeepInit c.cl_meta ->
					(* it means that we only need the __init__ block *)
					c.cl_extern <- true;
					loop (mt :: acc) l
				| _ ->
					if dce.debug then print_endline ("[DCE] Removed class " ^ (s_type_path c.cl_path));
					loop acc l)
			end
 		| (TEnumDecl en) as mt :: l when Meta.has Meta.Used en.e_meta || en.e_extern || keep_whole_enum dce en ->
			loop (mt :: acc) l
		| TEnumDecl e :: l ->
			if dce.debug then print_endline ("[DCE] Removed enum " ^ (s_type_path e.e_path));
			loop acc l
		| mt :: l ->
			loop (mt :: acc) l
		| [] ->
			acc
	in
	com.types <- loop [] (List.rev com.types);

	(* extra step to adjust properties that had accessors removed (required for Php and Cpp) *)
	fix_accessors com;

	(* remove "override" from fields that do not override anything anymore *)
	List.iter (fun mt -> match mt with
		| TClassDecl c ->
			c.cl_overrides <- List.filter (fun s ->
				let rec loop c =
					match c.cl_super with
					| Some (csup,_) when PMap.mem s.cf_name csup.cl_fields -> true
					| Some (csup,_) -> loop csup
					| None -> false
				in
				loop c
			) c.cl_overrides;
		| _ -> ()
	) com.types;

	(*
		Mark extern classes as really used if they are extended by non-extern ones.
	*)
	List.iter (function
		| TClassDecl ({cl_extern = false; cl_super = Some ({cl_extern = true} as csup, _)}) ->
			mark_directly_used_class csup
		| TClassDecl ({cl_extern = false} as c) when c.cl_implements <> [] ->
			List.iter (fun (iface,_) -> if (iface.cl_extern) then mark_directly_used_class iface) c.cl_implements;
		| _ -> ()
	) com.types;

	(* cleanup added fields metadata - compatibility with compilation server *)
	let rec remove_meta m = function
		| [] -> []
		| (m2,_,_) :: l when m = m2 -> l
		| x :: l -> x :: remove_meta m l
	in
	List.iter (fun cf -> cf.cf_meta <- remove_meta Meta.Used cf.cf_meta) dce.marked_fields;
	List.iter (fun cf -> cf.cf_meta <- remove_meta Meta.MaybeUsed cf.cf_meta) dce.marked_maybe_fields
