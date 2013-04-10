(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast

type path = string list * string

type field_kind =
	| Var of var_kind
	| Method of method_kind

and var_kind = {
	v_read : var_access;
	v_write : var_access;
}

and var_access =
	| AccNormal
	| AccNo				(* can't be accessed outside of the class itself and its subclasses *)
	| AccNever			(* can't be accessed, even in subclasses *)
	| AccResolve		(* call resolve("field") when accessed *)
	| AccCall of string (* perform a method call when accessed *)
	| AccInline			(* similar to Normal but inline when accessed *)
	| AccRequire of string * string option (* set when @:require(cond) fails *)

and method_kind =
	| MethNormal
	| MethInline
	| MethDynamic
	| MethMacro

type t =
	| TMono of t option ref
	| TEnum of tenum * tparams
	| TInst of tclass * tparams
	| TType of tdef * tparams
	| TFun of (string * bool * t) list * t
	| TAnon of tanon
	| TDynamic of t
	| TLazy of (unit -> t) ref
	| TAbstract of tabstract * tparams

and tparams = t list

and type_params = (string * t) list

and tconstant =
	| TInt of int32
	| TFloat of string
	| TString of string
	| TBool of bool
	| TNull
	| TThis
	| TSuper

and tvar = {
	mutable v_id : int;
	mutable v_name : string;
	mutable v_type : t;
	mutable v_capture : bool;
	mutable v_extra : (type_params * texpr option) option;
}

and tfunc = {
	tf_args : (tvar * tconstant option) list;
	tf_type : t;
	tf_expr : texpr;
}

and anon_status =
	| Closed
	| Opened
	| Const
	| Statics of tclass
	| EnumStatics of tenum
	| AbstractStatics of tabstract

and tanon = {
	mutable a_fields : (string, tclass_field) PMap.t;
	a_status : anon_status ref;
}

and texpr_expr =
	| TConst of tconstant
	| TLocal of tvar
	| TArray of texpr * texpr
	| TBinop of Ast.binop * texpr * texpr
	| TField of texpr * tfield_access
	| TTypeExpr of module_type
	| TParenthesis of texpr
	| TObjectDecl of (string * texpr) list
	| TArrayDecl of texpr list
	| TCall of texpr * texpr list
	| TNew of tclass * tparams * texpr list
	| TUnop of Ast.unop * Ast.unop_flag * texpr
	| TFunction of tfunc
	| TVars of (tvar * texpr option) list
	| TBlock of texpr list
	| TFor of tvar * texpr * texpr
	| TIf of texpr * texpr * texpr option
	| TWhile of texpr * texpr * Ast.while_flag
	| TSwitch of texpr * (texpr list * texpr) list * texpr option
	| TMatch of texpr * (tenum * tparams) * (int list * tvar option list option * texpr) list * texpr option
	| TTry of texpr * (tvar * texpr) list
	| TReturn of texpr option
	| TBreak
	| TContinue
	| TThrow of texpr
	| TCast of texpr * module_type option

and tfield_access =
	| FInstance of tclass * tclass_field
	| FStatic of tclass * tclass_field
	| FAnon of tclass_field
	| FDynamic of string
	| FClosure of tclass option * tclass_field (* None class = TAnon *)
	| FEnum of tenum * tenum_field

and texpr = {
	eexpr : texpr_expr;
	etype : t;
	epos : Ast.pos;
}

and tclass_field = {
	cf_name : string;
	mutable cf_type : t;
	mutable cf_public : bool;
	cf_pos : pos;
	mutable cf_doc : Ast.documentation;
	mutable cf_meta : metadata;
	mutable cf_kind : field_kind;
	mutable cf_params : type_params;
	mutable cf_expr : texpr option;
	mutable cf_overloads : tclass_field list;
}

and tclass_kind =
	| KNormal
	| KTypeParameter of t list
	| KExtension of tclass * tparams
	| KExpr of Ast.expr
	| KGeneric
	| KGenericInstance of tclass * tparams
	| KMacroType
	| KAbstractImpl of tabstract

and metadata = Ast.metadata

and tinfos = {
	mt_path : path;
	mt_module : module_def;
	mt_pos : Ast.pos;
	mt_private : bool;
	mt_doc : Ast.documentation;
	mutable mt_meta : metadata;
	mt_types : type_params;
}

and tclass = {
	mutable cl_path : path;
	mutable cl_module : module_def;
	mutable cl_pos : Ast.pos;
	mutable cl_private : bool;
	mutable cl_doc : Ast.documentation;
	mutable cl_meta : metadata;
	mutable cl_types : type_params;

	mutable cl_kind : tclass_kind;
	mutable cl_extern : bool;
	mutable cl_interface : bool;
	mutable cl_super : (tclass * tparams) option;
	mutable cl_implements : (tclass * tparams) list;
	mutable cl_fields : (string , tclass_field) PMap.t;
	mutable cl_statics : (string, tclass_field) PMap.t;
	mutable cl_ordered_statics : tclass_field list;
	mutable cl_ordered_fields : tclass_field list;
	mutable cl_dynamic : t option;
	mutable cl_array_access : t option;
	mutable cl_constructor : tclass_field option;
	mutable cl_init : texpr option;
	mutable cl_overrides : tclass_field list;

	mutable cl_build : unit -> unit;
	mutable cl_restore : unit -> unit;
}

and tenum_field = {
	ef_name : string;
	ef_type : t;
	ef_pos : Ast.pos;
	ef_doc : Ast.documentation;
	ef_index : int;
	ef_params : type_params;
	mutable ef_meta : metadata;
}

and tenum = {
	mutable e_path : path;
	e_module : module_def;
	e_pos : Ast.pos;
	e_private : bool;
	e_doc : Ast.documentation;
	mutable e_meta : metadata;
	mutable e_types : type_params;

	mutable e_extern : bool;
	mutable e_constrs : (string , tenum_field) PMap.t;
	mutable e_names : string list;
}

and tdef = {
	t_path : path;
	t_module : module_def;
	t_pos : Ast.pos;
	t_private : bool;
	t_doc : Ast.documentation;
	mutable t_meta : metadata;
	mutable t_types : type_params;

	mutable t_type : t;
}

and tabstract = {
	a_path : path;
	a_module : module_def;
	a_pos : Ast.pos;
	a_private : bool;
	a_doc : Ast.documentation;
	mutable a_meta : metadata;
	mutable a_types : type_params;
	mutable a_ops : (Ast.binop * tclass_field) list;
	mutable a_unops : (Ast.unop * unop_flag * tclass_field) list;
	mutable a_impl : tclass option;
	mutable a_this : t;
	mutable a_from : (t * tclass_field option) list;
	mutable a_array : tclass_field list;
	mutable a_to : (t * tclass_field option) list;
}

and module_type =
	| TClassDecl of tclass
	| TEnumDecl of tenum
	| TTypeDecl of tdef
	| TAbstractDecl of tabstract

and module_def = {
	m_id : int;
	m_path : path;
	mutable m_types : module_type list;
	m_extra : module_def_extra;
}

and module_def_extra = {
	m_file : string;
	m_sign : string;
	mutable m_time : float;
	mutable m_dirty : bool;
	mutable m_added : int;
	mutable m_mark : int;
	mutable m_deps : (int,module_def) PMap.t;
	mutable m_processed : int;
	mutable m_kind : module_kind;
	mutable m_binded_res : (string, string) PMap.t;
	mutable m_macro_calls : string list;
}

and module_kind =
	| MCode
	| MMacro
	| MFake

let alloc_var =
	let uid = ref 0 in
	(fun n t -> incr uid; { v_name = n; v_type = t; v_id = !uid; v_capture = false; v_extra = None })

let alloc_mid =
	let mid = ref 0 in
	(fun() -> incr mid; !mid)

let mk e t p = { eexpr = e; etype = t; epos = p }

let mk_block e =
	match e.eexpr with
	| TBlock (_ :: _) -> e
	| _ -> mk (TBlock [e]) e.etype e.epos

let null t p = mk (TConst TNull) t p

let mk_mono() = TMono (ref None)

let rec t_dynamic = TDynamic t_dynamic

let tfun pl r = TFun (List.map (fun t -> "",false,t) pl,r)

let fun_args l = List.map (fun (a,c,t) -> a, c <> None, t) l

let field_name f =
	match f with
	| FAnon f | FInstance (_,f) | FStatic (_,f) | FClosure (_,f) -> f.cf_name
	| FEnum (_,f) -> f.ef_name
	| FDynamic n -> n

let extract_field = function
	| FAnon f | FInstance (_,f) | FStatic (_,f) | FClosure (_,f) -> Some f
	| _ -> None

let mk_class m path pos =
	{
		cl_path = path;
		cl_module = m;
		cl_pos = pos;
		cl_doc = None;
		cl_meta = [];
		cl_private = false;
		cl_kind = KNormal;
		cl_extern = false;
		cl_interface = false;
		cl_types = [];
		cl_super = None;
		cl_implements = [];
		cl_fields = PMap.empty;
		cl_ordered_statics = [];
		cl_ordered_fields = [];
		cl_statics = PMap.empty;
		cl_dynamic = None;
		cl_array_access = None;
		cl_constructor = None;
		cl_init = None;
		cl_overrides = [];
		cl_build = (fun() -> ());
		cl_restore = (fun() -> ());
	}

let module_extra file sign time kind =
	{
		m_file = file;
		m_sign = sign;
		m_dirty = false;
		m_added = 0;
		m_mark = 0;
		m_time = time;
		m_processed = 0;
		m_deps = PMap.empty;
		m_kind = kind;
		m_binded_res = PMap.empty;
		m_macro_calls = [];
	}


let mk_field name t p = {
	cf_name = name;
	cf_type = t;
	cf_pos = p;
	cf_doc = None;
	cf_meta = [];
	cf_public = true;
	cf_kind = Var { v_read = AccNormal; v_write = AccNormal };
	cf_expr = None;
	cf_params = [];
	cf_overloads = [];
}

let null_module = {
		m_id = alloc_mid();
		m_path = [] , "";
		m_types = [];
		m_extra = module_extra "" "" 0. MFake;
	}

let null_class =
	let c = mk_class null_module ([],"") Ast.null_pos in
	c.cl_private <- true;
	c

let null_field = mk_field "" t_dynamic Ast.null_pos

let add_dependency m mdep =
	if m != null_module && m != mdep then m.m_extra.m_deps <- PMap.add mdep.m_id mdep m.m_extra.m_deps

let arg_name (a,_) = a.v_name

let t_infos t : tinfos =
	match t with
	| TClassDecl c -> Obj.magic c
	| TEnumDecl e -> Obj.magic e
	| TTypeDecl t -> Obj.magic t
	| TAbstractDecl a -> Obj.magic a

let t_path t = (t_infos t).mt_path

let print_context() = ref []

let is_closed a = !(a.a_status) <> Opened

let rec s_type ctx t =
	match t with
	| TMono r ->
		(match !r with
		| None -> Printf.sprintf "Unknown<%d>" (try List.assq t (!ctx) with Not_found -> let n = List.length !ctx in ctx := (t,n) :: !ctx; n)
		| Some t -> s_type ctx t)
	| TEnum (e,tl) ->
		Ast.s_type_path e.e_path ^ s_type_params ctx tl
	| TInst (c,tl) ->
		Ast.s_type_path c.cl_path ^ s_type_params ctx tl
	| TType (t,tl) ->
		Ast.s_type_path t.t_path ^ s_type_params ctx tl
	| TAbstract (a,tl) ->
		Ast.s_type_path a.a_path ^ s_type_params ctx tl
	| TFun ([],t) ->
		"Void -> " ^ s_fun ctx t false
	| TFun (l,t) ->
		String.concat " -> " (List.map (fun (s,b,t) ->
			(if b then "?" else "") ^ (if s = "" then "" else s ^ " : ") ^ s_fun ctx t true
		) l) ^ " -> " ^ s_fun ctx t false
	| TAnon a ->
	let fl = PMap.fold (fun f acc -> ((if Meta.has Meta.Optional f.cf_meta then " ?" else " ") ^ f.cf_name ^ " : " ^ s_type ctx f.cf_type) :: acc) a.a_fields [] in
		"{" ^ (if not (is_closed a) then "+" else "") ^  String.concat "," fl ^ " }"
	| TDynamic t2 ->
		"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [t2])
	| TLazy f ->
		s_type ctx (!f())

and s_fun ctx t void =
	match t with
	| TFun _ ->
		"(" ^ s_type ctx t ^ ")"
	| TEnum ({ e_path = ([],"Void") },[]) when void ->
		"(" ^ s_type ctx t ^ ")"
	| TAbstract ({ a_path = ([],"Void") },[]) when void ->
		"(" ^ s_type ctx t ^ ")"
	| TMono r ->
		(match !r with
		| None -> s_type ctx t
		| Some t -> s_fun ctx t void)
	| TLazy f ->
		s_fun ctx (!f()) void
	| _ ->
		s_type ctx t

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

let s_access = function
	| AccNormal -> "default"
	| AccNo -> "null"
	| AccNever -> "never"
	| AccResolve -> "resolve"
	| AccCall m -> m
	| AccInline	-> "inline"
	| AccRequire (n,_) -> "require " ^ n

let s_kind = function
	| Var { v_read = AccNormal; v_write = AccNormal } -> "var"
	| Var v -> "(" ^ s_access v.v_read ^ "," ^ s_access v.v_write ^ ")"
	| Method m ->
		match m with
		| MethNormal -> "method"
		| MethDynamic -> "dynamic method"
		| MethInline -> "inline method"
		| MethMacro -> "macro method"

let rec is_parent csup c =
	if c == csup || List.exists (fun (i,_) -> is_parent csup i) c.cl_implements then
		true
	else match c.cl_super with
		| None -> false
		| Some (c,_) -> is_parent csup c

let map loop t =
	match t with
	| TMono r ->
		(match !r with
		| None -> t
		| Some t -> loop t) (* erase*)
	| TEnum (_,[]) | TInst (_,[]) | TType (_,[]) ->
		t
	| TEnum (e,tl) ->
		TEnum (e, List.map loop tl)
	| TInst (c,tl) ->
		TInst (c, List.map loop tl)
	| TType (t2,tl) ->
		TType (t2,List.map loop tl)
	| TAbstract (a,tl) ->
		TAbstract (a,List.map loop tl)
	| TFun (tl,r) ->
		TFun (List.map (fun (s,o,t) -> s, o, loop t) tl,loop r)
	| TAnon a ->
		TAnon {
			a_fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields;
			a_status = a.a_status;
		}
	| TLazy f ->
		let ft = !f() in
		let ft2 = loop ft in
		if ft == ft2 then t else ft2
	| TDynamic t2 ->
		if t == t2 then	t else TDynamic (loop t2)

(* substitute parameters with other types *)
let apply_params cparams params t =
	match cparams with
	| [] -> t
	| _ ->
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (x,TLazy f) :: l1, _ -> loop ((x,(!f)()) :: l1) l2
		| (_,t1) :: l1 , t2 :: l2 -> (t1,t2) :: loop l1 l2
		| _ -> assert false
	in
	let subst = loop cparams params in
	let rec loop t =
		try
			List.assq t subst
		with Not_found ->
		match t with
		| TMono r ->
			(match !r with
			| None -> t
			| Some t -> loop t)
		| TEnum (e,tl) ->
			(match tl with
			| [] -> t
			| _ -> TEnum (e,List.map loop tl))
		| TType (t2,tl) ->
			(match tl with
			| [] -> t
			| _ -> TType (t2,List.map loop tl))
		| TAbstract (a,tl) ->
			(match tl with
			| [] -> t
			| _ -> TAbstract (a,List.map loop tl))
		| TInst (c,tl) ->
			(match tl with
			| [] ->
				t
			| [TMono r] ->
				(match !r with
				| Some tt when t == tt ->
					(* for dynamic *)
					let pt = mk_mono() in
					let t = TInst (c,[pt]) in
					(match pt with TMono r -> r := Some t | _ -> assert false);
					t
				| _ -> TInst (c,List.map loop tl))
			| _ ->
				TInst (c,List.map loop tl))
		| TFun (tl,r) ->
			TFun (List.map (fun (s,o,t) -> s, o, loop t) tl,loop r)
		| TAnon a ->
			TAnon {
				a_fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields;
				a_status = a.a_status;
			}
		| TLazy f ->
			let ft = !f() in
			let ft2 = loop ft in
			if ft == ft2 then
				t
			else
				ft2
		| TDynamic t2 ->
			if t == t2 then
				t
			else
				TDynamic (loop t2)
	in
	loop t

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (!f())
	| TType (t,tl) ->
		follow (apply_params t.t_types tl t.t_type)
	| _ -> t

let rec is_nullable ?(no_lazy=false) = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_nullable t)
	| TType ({ t_path = ([],"Null") },[_]) ->
		true
	| TLazy f ->
		if no_lazy then raise Exit else is_nullable (!f())
	| TType (t,tl) ->
		is_nullable (apply_params t.t_types tl t.t_type)
	| TFun _ ->
		false
(*
	Type parameters will most of the time be nullable objects, so we don't want to make it hard for users
	to have to specify Null<T> all over the place, so while they could be a basic type, let's assume they will not.

	This will still cause issues with inlining and haxe.rtti.Generic. In that case proper explicit Null<T> is required to
	work correctly with basic types. This could still be fixed by redoing a nullability inference on the typed AST.

	| TInst ({ cl_kind = KTypeParameter },_) -> false
*)
	| TInst ({ cl_path = (["haxe"],"Int32") },[])
	| TInst ({ cl_path = ([],"Int") },[])
	| TInst ({ cl_path = ([],"Float") },[])
	| TEnum ({ e_path = ([],"Bool") },[]) -> false
	| TAbstract (a,_) -> not (Meta.has Meta.NotNull a.a_meta)
	| _ ->
		true

let rec is_null = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_null t)
	| TType ({ t_path = ([],"Null") },[t]) ->
		not (is_nullable t)
	| TLazy f ->
		is_null (!f())
	| TType (t,tl) ->
		is_null (apply_params t.t_types tl t.t_type)
	| _ ->
		false

let rec has_mono t = match t with
	| TMono r ->
		(match !r with None -> true | Some t -> has_mono t)
	| TInst(_,pl) | TEnum(_,pl) | TAbstract(_,pl) | TType(_,pl) ->
		List.exists has_mono pl
	| TDynamic _ ->
		false
	| TFun(args,r) ->
		has_mono r || List.exists (fun (_,_,t) -> has_mono t) args
	| TAnon a ->
		PMap.fold (fun cf b -> has_mono cf.cf_type && b) a.a_fields true
	| TLazy r ->
		has_mono (!r())

let rec link e a b =
	(* tell if setting a == b will create a type-loop *)
	let rec loop t =
		if t == a then
			true
		else match t with
		| TMono t -> (match !t with None -> false | Some t -> loop t)
		| TEnum (_,tl) -> List.exists loop tl
		| TInst (_,tl) | TType (_,tl) | TAbstract (_,tl) -> List.exists loop tl
		| TFun (tl,t) -> List.exists (fun (_,_,t) -> loop t) tl || loop t
		| TDynamic t2 ->
			if t == t2 then
				false
			else
				loop t2
		| TLazy f ->
			loop (!f())
		| TAnon a ->
			try
				PMap.iter (fun _ f -> if loop f.cf_type then raise Exit) a.a_fields;
				false
			with
				Exit -> true
	in
	(* tell is already a ~= b *)
	if loop b then
		(follow b) == a
	else if b == t_dynamic then
		true
	else begin
		e := Some b;
		true
	end

let monomorphs eparams t =
	apply_params eparams (List.map (fun _ -> mk_mono()) eparams) t

let rec fast_eq a b =
	if a == b then
		true
	else match a , b with
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		List.for_all2 (fun (_,_,t1) (_,_,t2) -> fast_eq t1 t2) l1 l2 && fast_eq r1 r2
	| TType (t1,l1), TType (t2,l2) ->
		t1 == t2 && List.for_all2 fast_eq l1 l2
	| TEnum (e1,l1), TEnum (e2,l2) ->
		e1 == e2 && List.for_all2 fast_eq l1 l2
	| TInst (c1,l1), TInst (c2,l2) ->
		c1 == c2 && List.for_all2 fast_eq l1 l2
	| _ , _ ->
		false

(* perform unification with subtyping.
   the first type is always the most down in the class hierarchy
   it's also the one that is pointed by the position.
   It's actually a typecheck of  A :> B where some mutations can happen *)

type unify_error =
	| Cannot_unify of t * t
	| Invalid_field_type of string
	| Has_no_field of t * string
	| Has_no_runtime_field of t * string
	| Has_extra_field of t * string
	| Invalid_kind of string * field_kind * field_kind
	| Invalid_visibility of string
	| Not_matching_optional of string
	| Cant_force_optional
	| Invariant_parameter of t * t
	| Constraint_failure of string
	| Missing_overload of tclass_field * t
	| Unify_custom of string

exception Unify_error of unify_error list

let cannot_unify a b = Cannot_unify (a,b)
let invalid_field n = Invalid_field_type n
let invalid_kind n a b = Invalid_kind (n,a,b)
let invalid_visibility n = Invalid_visibility n
let has_no_field t n = Has_no_field (t,n)
let has_extra_field t n = Has_extra_field (t,n)
let error l = raise (Unify_error l)
let has_meta m ml = List.exists (fun (m2,_,_) -> m = m2) ml
let get_meta m ml = List.find (fun (m2,_,_) -> m = m2) ml
let no_meta = []

(*
	we can restrict access as soon as both are runtime-compatible
*)
let unify_access a1 a2 =
	a1 = a2 || match a1, a2 with
	| _, AccNo | _, AccNever -> true
	| AccInline, AccNormal -> true
	| _ -> false

let direct_access = function
	| AccNo | AccNever | AccNormal | AccInline | AccRequire _ -> true
	| AccResolve | AccCall _ -> false

let unify_kind k1 k2 =
	k1 = k2 || match k1, k2 with
		| Var v1, Var v2 -> unify_access v1.v_read v2.v_read && unify_access v1.v_write v2.v_write
		| Var v, Method m ->
			(match v.v_read, v.v_write, m with
			| AccNormal, _, MethNormal -> true
			| AccNormal, AccNormal, MethDynamic -> true
			| _ -> false)
		| Method m, Var v ->
			(match m with
			| MethDynamic -> direct_access v.v_read && direct_access v.v_write
			| MethMacro -> false
			| MethNormal | MethInline ->
				match v.v_write with
				| AccNo | AccNever -> true
				| _ -> false)
		| Method m1, Method m2 ->
			match m1,m2 with
			| MethInline, MethNormal
			| MethDynamic, MethNormal -> true
			| _ -> false

let eq_stack = ref []

type eq_kind =
	| EqStrict
	| EqCoreType
	| EqRightDynamic
	| EqBothDynamic

let rec type_eq param a b =
	if a == b then
		()
	else match a , b with
	| TLazy f , _ -> type_eq param (!f()) b
	| _ , TLazy f -> type_eq param a (!f())
	| TMono t , _ ->
		(match !t with
		| None -> if param = EqCoreType || not (link t a b) then error [cannot_unify a b]
		| Some t -> type_eq param t b)
	| _ , TMono t ->
		(match !t with
		| None -> if param = EqCoreType || not (link t b a) then error [cannot_unify a b]
		| Some t -> type_eq param a t)
	| TType (t1,tl1), TType (t2,tl2) when (t1 == t2 || (param = EqCoreType && t1.t_path = t2.t_path)) && List.length tl1 = List.length tl2 ->
		List.iter2 (type_eq param) tl1 tl2
	| TType (t,tl) , _ when param <> EqCoreType ->
		type_eq param (apply_params t.t_types tl t.t_type) b
	| _ , TType (t,tl) when param <> EqCoreType ->
		if List.exists (fun (a2,b2) -> fast_eq a a2 && fast_eq b b2) (!eq_stack) then
			()
		else begin
			eq_stack := (a,b) :: !eq_stack;
			try
				type_eq param a (apply_params t.t_types tl t.t_type);
				eq_stack := List.tl !eq_stack;
			with
				Unify_error l ->
					eq_stack := List.tl !eq_stack;
					error (cannot_unify a b :: l)
		end
	| TEnum (e1,tl1) , TEnum (e2,tl2) ->
		if e1 != e2 && not (param = EqCoreType && e1.e_path = e2.e_path) then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		if c1 != c2 && not (param = EqCoreType && c1.cl_path = c2.cl_path) && (match c1.cl_kind, c2.cl_kind with KExpr _, KExpr _ -> false | _ -> true) then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			type_eq param r1 r2;
			List.iter2 (fun (n,o1,t1) (_,o2,t2) ->
				if o1 <> o2 then error [Not_matching_optional n];
				type_eq param t1 t2
			) l1 l2
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TDynamic a , TDynamic b ->
		type_eq param a b
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		if a1 != a2 && not (param = EqCoreType && a1.a_path = a2.a_path) then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f1 ->
				try
					let f2 = PMap.find n a2.a_fields in
					if f1.cf_kind <> f2.cf_kind && (param = EqStrict || param = EqCoreType || not (unify_kind f1.cf_kind f2.cf_kind)) then error [invalid_kind n f1.cf_kind f2.cf_kind];
					try
						type_eq param f1.cf_type f2.cf_type
					with
						Unify_error l -> error (invalid_field n :: l)
				with
					Not_found ->
						if is_closed a2 then error [has_no_field b n];
						if not (link (ref None) b f1.cf_type) then error [cannot_unify a b];
						a2.a_fields <- PMap.add n f1 a2.a_fields
			) a1.a_fields;
			PMap.iter (fun n f2 ->
				if not (PMap.mem n a1.a_fields) then begin
					if is_closed a1 then error [has_no_field a n];
					if not (link (ref None) a f2.cf_type) then error [cannot_unify a b];
					a1.a_fields <- PMap.add n f2 a1.a_fields
				end;
			) a2.a_fields;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| _ , _ ->
		if b == t_dynamic && (param = EqRightDynamic || param = EqBothDynamic) then
			()
		else if a == t_dynamic && param = EqBothDynamic then
			()
		else
			error [cannot_unify a b]

let type_iseq a b =
	try
		type_eq EqStrict a b;
		true
	with
		Unify_error _ -> false

let unify_stack = ref []

let is_extern_field f =
	match f.cf_kind with
	| Method _ -> false
	| Var { v_read = AccNormal | AccNo } | Var { v_write = AccNormal | AccNo } -> false
	| _ -> not (Meta.has Meta.IsVar f.cf_meta)

let field_type f =
	match f.cf_params with
	| [] -> f.cf_type
	| l -> monomorphs l f.cf_type

let rec raw_class_field build_type c i =
	try
		let f = PMap.find i c.cl_fields in
		Some c, build_type f , f
	with Not_found -> try (match c.cl_constructor with
		| Some ctor when i = "new" -> Some c, build_type ctor,ctor
		| _ -> raise Not_found)
	with Not_found -> try
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			let c2 , t , f = raw_class_field build_type c i in
			c2, apply_params c.cl_types tl t , f
	with Not_found ->
		match c.cl_kind with
		| KTypeParameter tl ->
			let rec loop = function
				| [] ->
					raise Not_found
				| t :: ctl ->
					match follow t with
					| TAnon a ->
						(try
							let f = PMap.find i a.a_fields in
							None, build_type f, f
						with
							Not_found -> loop ctl)
					| TInst (c,pl) ->
						(try
							let c2, t , f = raw_class_field build_type c i in
							c2, apply_params c.cl_types pl t, f
						with
							Not_found -> loop ctl)
					| _ ->
						loop ctl
			in
			loop tl
		| _ ->
			if not c.cl_interface then raise Not_found;
			(*
				an interface can implements other interfaces without
				having to redeclare its fields
			*)
			let rec loop = function
				| [] ->
					raise Not_found
				| (c,tl) :: l ->
					try
						let c2, t , f = raw_class_field build_type c i in
						c2, apply_params c.cl_types tl t, f
					with
						Not_found -> loop l
			in
			loop c.cl_implements

let class_field = raw_class_field field_type

let quick_field t n =
	match follow t with
	| TInst (c,_) ->
		let c, _, f = raw_class_field (fun f -> f.cf_type) c n in
		(match c with None -> FAnon f | Some c -> FInstance (c,f))
	| TAnon a ->
		(match !(a.a_status) with
		| EnumStatics e ->
			assert false (* to replace with FEnum later *)
		| Statics c ->
			FStatic (c,PMap.find n c.cl_statics)
		| AbstractStatics _ ->
			assert false
		| _ ->
			FAnon (PMap.find n a.a_fields))
	| TDynamic _ ->
		FDynamic n
	| TEnum _  | TMono _ | TAbstract _ | TFun _ ->
		raise Not_found
	| TLazy _ | TType _ ->
		assert false

let rec get_constructor build_type c =
	match c.cl_constructor, c.cl_super with
	| Some c, _ -> build_type c, c
	| None, None -> raise Not_found
	| None, Some (csup,cparams) ->
		let t, c = get_constructor build_type csup in
		apply_params csup.cl_types cparams t, c

let rec unify a b =
	if a == b then
		()
	else match a, b with
	| TLazy f , _ -> unify (!f()) b
	| _ , TLazy f -> unify a (!f())
	| TMono t , _ ->
		(match !t with
		| None -> if not (link t a b) then error [cannot_unify a b]
		| Some t -> unify t b)
	| _ , TMono t ->
		(match !t with
		| None -> if not (link t b a) then error [cannot_unify a b]
		| Some t -> unify a t)
	| TType (t,tl) , _ ->
		if not (List.exists (fun (a2,b2) -> fast_eq a a2 && fast_eq b b2) (!unify_stack)) then begin
			try
				unify_stack := (a,b) :: !unify_stack;
				unify (apply_params t.t_types tl t.t_type) b;
				unify_stack := List.tl !unify_stack;
			with
				Unify_error l ->
					unify_stack := List.tl !unify_stack;
					error (cannot_unify a b :: l)
		end
	| _ , TType (t,tl) ->
		if not (List.exists (fun (a2,b2) -> fast_eq a a2 && fast_eq b b2) (!unify_stack)) then begin
			try
				unify_stack := (a,b) :: !unify_stack;
				unify a (apply_params t.t_types tl t.t_type);
				unify_stack := List.tl !unify_stack;
			with
				Unify_error l ->
					unify_stack := List.tl !unify_stack;
					error (cannot_unify a b :: l)
		end
	| TEnum (ea,tl1) , TEnum (eb,tl2) ->
		if ea != eb then error [cannot_unify a b];
		unify_types a b tl1 tl2
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) when a1 == a2 ->
		unify_types a b tl1 tl2
	| TAbstract ({a_path=[],"Void"},_) , _
	| _ , TAbstract ({a_path=[],"Void"},_) ->
		error [cannot_unify a b]
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		if not (List.exists (unify_to_field a1 tl1 b) a1.a_to) && not (List.exists (unify_from_field a2 tl2 a b) a2.a_from) then error [cannot_unify a b]
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		let rec loop c tl =
			if c == c2 then begin
				unify_types a b tl tl2;
				true
			end else (match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					loop cs (List.map (apply_params c.cl_types tl) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (apply_params c.cl_types tl) tls)
			) c.cl_implements
			|| (match c.cl_kind with
			| KTypeParameter pl -> List.exists (fun t -> match follow t with TInst (cs,tls) -> loop cs (List.map (apply_params c.cl_types tl) tls) | _ -> false) pl
			| _ -> false)
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			(match r2 with
			| TAbstract ({a_path=[],"Void"},_) -> ()
			| _ -> unify r1 r2);
			List.iter2 (fun (_,o1,t1) (_,o2,t2) ->
				if o1 && not o2 then error [Cant_force_optional];
				unify t1 t2
			) l2 l1 (* contravariance *)
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TInst (c,tl) , TAnon an ->
		if PMap.is_empty an.a_fields then (match c.cl_kind with
			| KTypeParameter pl ->
				(* one of the constraints must unify with { } *)
				if not (List.exists (fun t -> match t with TInst _ | TAnon _ -> true | _ -> false) pl) then error [cannot_unify a b]
			| _ -> ());
		(try
			PMap.iter (fun n f2 ->
				let _, ft, f1 = (try class_field c n with Not_found -> error [has_no_field a n]) in
				if not (unify_kind f1.cf_kind f2.cf_kind) then error [invalid_kind n f1.cf_kind f2.cf_kind];
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];
				(try
					unify_with_access (apply_params c.cl_types tl ft) f2
				with
					Unify_error l -> error (invalid_field n :: l));
				List.iter (fun f2o ->
					if not (List.exists (fun f1o -> type_iseq f1o.cf_type f2o.cf_type) (f1 :: f1.cf_overloads))
					then error [Missing_overload (f1, f2o.cf_type)]
				) f2.cf_overloads;
				(* we mark the field as :?used because it might be used through the structure *)
				if not (Meta.has Meta.MaybeUsed f1.cf_meta) then f1.cf_meta <- (Meta.MaybeUsed,[],f1.cf_pos) :: f1.cf_meta;
				(match f1.cf_kind with
				| Method MethInline ->
					if (c.cl_extern || Meta.has Meta.Extern f1.cf_meta) && not (Meta.has Meta.Runtime f1.cf_meta) then error [Has_no_runtime_field (a,n)];
				| _ -> ());
			) an.a_fields;
			if !(an.a_status) = Opened then an.a_status := Closed;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f2 ->
			try
				let f1 = PMap.find n a1.a_fields in
				if not (unify_kind f1.cf_kind f2.cf_kind) then
					(match !(a1.a_status), f1.cf_kind, f2.cf_kind with
					| Opened, Var { v_read = AccNormal; v_write = AccNo }, Var { v_read = AccNormal; v_write = AccNormal } ->
						f1.cf_kind <- f2.cf_kind;
					| _ -> error [invalid_kind n f1.cf_kind f2.cf_kind]);
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];
				try
					unify_with_access f1.cf_type f2;
					(match !(a1.a_status) with
					| Statics c when not (Meta.has Meta.MaybeUsed f1.cf_meta) -> f1.cf_meta <- (Meta.MaybeUsed,[],f1.cf_pos) :: f1.cf_meta
					| _ -> ());
				with
					Unify_error l -> error (invalid_field n :: l)
			with
				Not_found ->
					match !(a1.a_status) with
					| Opened ->
						if not (link (ref None) a f2.cf_type) then error [];
						a1.a_fields <- PMap.add n f2 a1.a_fields
					| Const when Meta.has Meta.Optional f2.cf_meta ->
						()
					| _ ->
						error [has_no_field a n];
			) a2.a_fields;
			(match !(a1.a_status) with
			| Const when not (PMap.is_empty a2.a_fields) ->
				PMap.iter (fun n _ -> if not (PMap.mem n a2.a_fields) then error [has_extra_field a n]) a1.a_fields;
			| Opened ->
				a1.a_status := Closed
			| _ -> ());
			(match !(a2.a_status) with
			| Statics c -> (match !(a1.a_status) with Statics c2 when c == c2 -> () | _ -> error [])
			| EnumStatics e -> (match !(a1.a_status) with EnumStatics e2 when e == e2 -> () | _ -> error [])
			| Opened -> a2.a_status := Closed
			| _ -> ())
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon an, TAbstract ({ a_path = [],"Class" },[pt]) ->
		(match !(an.a_status) with
		| Statics cl -> unify (TInst (cl,List.map snd cl.cl_types)) pt
		| _ -> error [cannot_unify a b])
	| TAnon an, TAbstract ({ a_path = [],"Enum" },[pt]) ->
		(match !(an.a_status) with
		| EnumStatics e -> unify (TEnum (e,List.map snd e.e_types)) pt
		| _ -> error [cannot_unify a b])
	| TEnum _, TAbstract ({ a_path = [],"EnumValue" },[]) ->
		()
	| TDynamic t , _ ->
		if t == a then
			()
		else (match b with
		| TDynamic t2 ->
			if t2 != b then
				(try
					type_eq EqRightDynamic t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| _ ->
			error [cannot_unify a b])
	| _ , TDynamic t ->
		if t == b then
			()
		else (match a with
		| TDynamic t2 ->
			if t2 != a then
				(try
					type_eq EqRightDynamic t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| TAnon an ->
			(try
				(match !(an.a_status) with
				| Statics _ | EnumStatics _ -> error []
				| Opened -> an.a_status := Closed
				| _ -> ());
				PMap.iter (fun _ f ->
					try
						type_eq EqStrict (field_type f) t
					with Unify_error l ->
						error (invalid_field f.cf_name :: l)
				) an.a_fields
			with Unify_error l ->
				error (cannot_unify a b :: l))
		| _ ->
			error [cannot_unify a b])
	| TAbstract (aa,tl), _  ->
		if not (List.exists (unify_to_field aa tl b) aa.a_to) then error [cannot_unify a b];
	| TInst ({ cl_kind = KTypeParameter ctl } as c,pl), TAbstract _ ->
		(* one of the constraints must satisfy the abstract *)
		if not (List.exists (fun t ->
			let t = apply_params c.cl_types pl t in
			try unify t b; true with Unify_error _ -> false
		) ctl) then error [cannot_unify a b];
	| _, TAbstract (bb,tl) ->
		if not (List.exists (unify_from_field bb tl a b) bb.a_from) then error [cannot_unify a b]
	| _ , _ ->
		error [cannot_unify a b]

and unify_from_field ab tl a b (t,cfo) =
	let unify_func = match follow a with TAbstract({a_impl = Some _},_) when ab.a_impl <> None -> type_eq EqStrict | _ -> unify in
	try begin match cfo with
		| Some cf -> (match follow cf.cf_type with
			| TFun(_,r) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_types tl (apply_params cf.cf_params monos t) in
				unify_func a (map t);
				unify (map r) b;
			| _ -> assert false)
		| _ ->
			unify_func a (apply_params ab.a_types tl t)
		end;
		true
	with Unify_error _ -> false

and unify_to_field ab tl b (t,cfo) =
	let unify_func = match follow b with TAbstract({a_impl = Some _},_) when ab.a_impl <> None -> type_eq EqStrict | _ -> unify in
	try begin match cfo with
		| Some cf -> (match follow cf.cf_type with
			| TFun([_,_,ta],_) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_types tl (apply_params cf.cf_params monos t) in
				let athis = map ab.a_this in
				(* we cannot allow implicit casts when the this type is not completely known yet *)
				if has_mono athis then raise (Unify_error []);
				type_eq EqStrict athis (map ta);
				(* immediate constraints checking is ok here because we know there are no monomorphs *)
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> unify m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
				unify_func (map t) b;
			| _ -> assert false)
		| _ ->
			unify_func (apply_params ab.a_types tl t) b;
		end;
		true
	with Unify_error _ -> false

and unify_types a b tl1 tl2 =
	List.iter2 (fun t1 t2 ->
		try
			type_eq EqRightDynamic t1 t2
		with Unify_error l ->
			let err = cannot_unify a b in
			error (try unify t1 t2; (err :: (Invariant_parameter (t1,t2)) :: l) with _ -> err :: l)
	) tl1 tl2

and unify_with_access t1 f2 =
	match f2.cf_kind with
	(* write only *)
	| Var { v_read = AccNo } | Var { v_read = AccNever } -> unify f2.cf_type t1
	(* read only *)
	| Method MethNormal | Method MethInline | Var { v_write = AccNo } | Var { v_write = AccNever } -> unify t1 f2.cf_type
	(* read/write *)
	| _ -> type_eq EqBothDynamic t1 f2.cf_type

let iter f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TBreak
	| TContinue
	| TTypeExpr _ ->
		()
	| TArray (e1,e2)
	| TBinop (_,e1,e2)
	| TFor (_,e1,e2)
	| TWhile (e1,e2,_) ->
		f e1;
		f e2;
	| TThrow e
	| TField (e,_)
	| TParenthesis e
	| TCast (e,_)
	| TUnop (_,_,e) ->
		f e
	| TArrayDecl el
	| TNew (_,_,el)
	| TBlock el ->
		List.iter f el
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> f e) fl
	| TCall (e,el) ->
		f e;
		List.iter f el
	| TVars vl ->
		List.iter (fun (_,e) -> match e with None -> () | Some e -> f e) vl
	| TFunction fu ->
		f fu.tf_expr
	| TIf (e,e1,e2) ->
		f e;
		f e1;
		(match e2 with None -> () | Some e -> f e)
	| TSwitch (e,cases,def) ->
		f e;
		List.iter (fun (el,e2) -> List.iter f el; f e2) cases;
		(match def with None -> () | Some e -> f e)
	| TMatch (e,_,cases,def) ->
		f e;
		List.iter (fun (_,_,e) -> f e) cases;
		(match def with None -> () | Some e -> f e)
	| TTry (e,catches) ->
		f e;
		List.iter (fun (_,e) -> f e) catches
	| TReturn eo ->
		(match eo with None -> () | Some e -> f e)

let map_expr f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TBreak
	| TContinue
	| TTypeExpr _ ->
		e
	| TArray (e1,e2) ->
		{ e with eexpr = TArray (f e1,f e2) }
	| TBinop (op,e1,e2) ->
		{ e with eexpr = TBinop (op,f e1,f e2) }
	| TFor (v,e1,e2) ->
		{ e with eexpr = TFor (v,f e1,f e2) }
	| TWhile (e1,e2,flag) ->
		{ e with eexpr = TWhile (f e1,f e2,flag) }
	| TThrow e1 ->
		{ e with eexpr = TThrow (f e1) }
	| TField (e1,v) ->
		{ e with eexpr = TField (f e1,v) }
	| TParenthesis e1 ->
		{ e with eexpr = TParenthesis (f e1) }
	| TUnop (op,pre,e1) ->
		{ e with eexpr = TUnop (op,pre,f e1) }
	| TArrayDecl el ->
		{ e with eexpr = TArrayDecl (List.map f el) }
	| TNew (t,pl,el) ->
		{ e with eexpr = TNew (t,pl,List.map f el) }
	| TBlock el ->
		{ e with eexpr = TBlock (List.map f el) }
	| TObjectDecl el ->
		{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el) }
	| TCall (e1,el) ->
		{ e with eexpr = TCall (f e1, List.map f el) }
	| TVars vl ->
		{ e with eexpr = TVars (List.map (fun (v,e) -> v , match e with None -> None | Some e -> Some (f e)) vl) }
	| TFunction fu ->
		{ e with eexpr = TFunction { fu with tf_expr = f fu.tf_expr } }
	| TIf (ec,e1,e2) ->
		{ e with eexpr = TIf (f ec,f e1,match e2 with None -> None | Some e -> Some (f e)) }
	| TSwitch (e1,cases,def) ->
		{ e with eexpr = TSwitch (f e1, List.map (fun (el,e2) -> List.map f el, f e2) cases, match def with None -> None | Some e -> Some (f e)) }
	| TMatch (e1,t,cases,def) ->
		{ e with eexpr = TMatch (f e1, t, List.map (fun (cl,params,e) -> cl, params, f e) cases, match def with None -> None | Some e -> Some (f e)) }
	| TTry (e1,catches) ->
		{ e with eexpr = TTry (f e1, List.map (fun (v,e) -> v, f e) catches) }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)) }
	| TCast (e1,t) ->
		{ e with eexpr = TCast (f e1,t) }

let map_expr_type f ft fv e =
	match e.eexpr with
	| TConst _
	| TBreak
	| TContinue
	| TTypeExpr _ ->
		{ e with etype = ft e.etype }
	| TLocal v ->
		{ e with eexpr = TLocal (fv v); etype = ft e.etype }
	| TArray (e1,e2) ->
		{ e with eexpr = TArray (f e1,f e2); etype = ft e.etype }
	| TBinop (op,e1,e2) ->
		{ e with eexpr = TBinop (op,f e1,f e2); etype = ft e.etype }
	| TFor (v,e1,e2) ->
		{ e with eexpr = TFor (fv v,f e1,f e2); etype = ft e.etype }
	| TWhile (e1,e2,flag) ->
		{ e with eexpr = TWhile (f e1,f e2,flag); etype = ft e.etype }
	| TThrow e1 ->
		{ e with eexpr = TThrow (f e1); etype = ft e.etype }
	| TField (e1,v) ->
		{ e with eexpr = TField (f e1,v); etype = ft e.etype }
	| TParenthesis e1 ->
		{ e with eexpr = TParenthesis (f e1); etype = ft e.etype }
	| TUnop (op,pre,e1) ->
		{ e with eexpr = TUnop (op,pre,f e1); etype = ft e.etype }
	| TArrayDecl el ->
		{ e with eexpr = TArrayDecl (List.map f el); etype = ft e.etype }
	| TNew (_,_,el) ->
		let et = ft e.etype in
		(* make sure that we use the class corresponding to the replaced type *)
		let c, pl = (match follow et with TInst (c,pl) -> (c,pl) | TAbstract({a_impl = Some c},pl) -> c,pl | t -> error [has_no_field t "new"]) in
		{ e with eexpr = TNew (c,pl,List.map f el); etype = et }
	| TBlock el ->
		{ e with eexpr = TBlock (List.map f el); etype = ft e.etype }
	| TObjectDecl el ->
		{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el); etype = ft e.etype }
	| TCall (e1,el) ->
		{ e with eexpr = TCall (f e1, List.map f el); etype = ft e.etype }
	| TVars vl ->
		{ e with eexpr = TVars (List.map (fun (v,e) -> fv v, match e with None -> None | Some e -> Some (f e)) vl); etype = ft e.etype }
	| TFunction fu ->
		let fu = {
			tf_expr = f fu.tf_expr;
			tf_args = List.map (fun (v,o) -> fv v, o) fu.tf_args;
			tf_type = ft fu.tf_type;
		} in
		{ e with eexpr = TFunction fu; etype = ft e.etype }
	| TIf (ec,e1,e2) ->
		{ e with eexpr = TIf (f ec,f e1,match e2 with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TSwitch (e1,cases,def) ->
		{ e with eexpr = TSwitch (f e1, List.map (fun (el,e2) -> List.map f el, f e2) cases, match def with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TMatch (e1,(en,pl),cases,def) ->
		let map_case (cl,params,e) =
			let params = match params with
				| None -> None
				| Some l -> Some (List.map (function None -> None | Some v -> Some (fv v)) l)
			in
			cl, params, f e
		in
		{ e with eexpr = TMatch (f e1, (en,List.map ft pl), List.map map_case cases, match def with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TTry (e1,catches) ->
		{ e with eexpr = TTry (f e1, List.map (fun (v,e) -> fv v, f e) catches); etype = ft e.etype }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TCast (e1,t) ->
		{ e with eexpr = TCast (f e1,t); etype = ft e.etype }

let s_expr_kind e =
	match e.eexpr with
	| TConst _ -> "Const"
	| TLocal _ -> "Local"
	| TArray (_,_) -> "Array"
	| TBinop (_,_,_) -> "Binop"
	| TField (_,_) -> "Field"
	| TTypeExpr _ -> "TypeExpr"
	| TParenthesis _ -> "Parenthesis"
	| TObjectDecl _ -> "ObjectDecl"
	| TArrayDecl _ -> "ArrayDecl"
	| TCall (_,_) -> "Call"
	| TNew (_,_,_) -> "New"
	| TUnop (_,_,_) -> "Unop"
	| TFunction _ -> "Function"
	| TVars _ -> "Vars"
	| TBlock _ -> "Block"
	| TFor (_,_,_) -> "For"
	| TIf (_,_,_) -> "If"
	| TWhile (_,_,_) -> "While"
	| TSwitch (_,_,_) -> "Switch"
	| TMatch (_,_,_,_) -> "Match"
	| TTry (_,_) -> "Try"
	| TReturn _ -> "Return"
	| TBreak -> "Break"
	| TContinue -> "Continue"
	| TThrow _ -> "Throw"
	| TCast _ -> "Cast"

let s_const = function
	| TInt i -> Int32.to_string i
	| TFloat s -> s ^ "f"
	| TString s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)
	| TBool b -> if b then "true" else "false"
	| TNull -> "null"
	| TThis -> "this"
	| TSuper -> "super"

let rec s_expr s_type e =
	let sprintf = Printf.sprintf in
	let slist f l = String.concat "," (List.map f l) in
	let loop = s_expr s_type in
	let s_var v = v.v_name ^ ":" ^ string_of_int v.v_id ^ if v.v_capture then "[c]" else "" in
	let str = (match e.eexpr with
	| TConst c ->
		"Const " ^ s_const c
	| TLocal v ->
		"Local " ^ s_var v
	| TArray (e1,e2) ->
		sprintf "%s[%s]" (loop e1) (loop e2)
	| TBinop (op,e1,e2) ->
		sprintf "(%s %s %s)" (loop e1) (s_binop op) (loop e2)
	| TField (e,f) ->
		let fstr = (match f with
			| FStatic (c,f) -> "static(" ^ s_type_path c.cl_path ^ "." ^ f.cf_name ^ ")"
			| FInstance (c,f) -> "inst(" ^ s_type_path c.cl_path ^ "." ^ f.cf_name ^ ")"
			| FClosure (c,f) -> "closure(" ^ (match c with None -> f.cf_name | Some c -> s_type_path c.cl_path ^ "." ^ f.cf_name)  ^ ")"
			| FAnon f -> "anon(" ^ f.cf_name ^ ")"
			| FEnum (en,f) -> "enum(" ^ s_type_path en.e_path ^ "." ^ f.ef_name ^ ")"
			| FDynamic f -> "dynamic(" ^ f ^ ")"
		) in
		sprintf "%s.%s" (loop e) fstr
	| TTypeExpr m ->
		sprintf "TypeExpr %s" (s_type_path (t_path m))
	| TParenthesis e ->
		sprintf "Parenthesis %s" (loop e)
	| TObjectDecl fl ->
		sprintf "ObjectDecl {%s)" (slist (fun (f,e) -> sprintf "%s : %s" f (loop e)) fl)
	| TArrayDecl el ->
		sprintf "ArrayDecl [%s]" (slist loop el)
	| TCall (e,el) ->
		sprintf "Call %s(%s)" (loop e) (slist loop el)
	| TNew (c,pl,el) ->
		sprintf "New %s%s(%s)" (s_type_path c.cl_path) (match pl with [] -> "" | l -> sprintf "<%s>" (slist s_type l)) (slist loop el)
	| TUnop (op,f,e) ->
		(match f with
		| Prefix -> sprintf "(%s %s)" (s_unop op) (loop e)
		| Postfix -> sprintf "(%s %s)" (loop e) (s_unop op))
	| TFunction f ->
		let args = slist (fun (v,o) -> sprintf "%s : %s%s" (s_var v) (s_type v.v_type) (match o with None -> "" | Some c -> " = " ^ s_const c)) f.tf_args in
		sprintf "Function(%s) : %s = %s" args (s_type f.tf_type) (loop f.tf_expr)
	| TVars vl ->
		sprintf "Vars %s" (slist (fun (v,eo) -> sprintf "%s : %s%s" (s_var v) (s_type v.v_type) (match eo with None -> "" | Some e -> " = " ^ loop e)) vl)
	| TBlock el ->
		sprintf "Block {\n%s}" (String.concat "" (List.map (fun e -> sprintf "%s;\n" (loop e)) el))
	| TFor (v,econd,e) ->
		sprintf "For (%s : %s in %s,%s)" (s_var v) (s_type v.v_type) (loop econd) (loop e)
	| TIf (e,e1,e2) ->
		sprintf "If (%s,%s%s)" (loop e) (loop e1) (match e2 with None -> "" | Some e -> "," ^ loop e)
	| TWhile (econd,e,flag) ->
		(match flag with
		| NormalWhile -> sprintf "While (%s,%s)" (loop econd) (loop e)
		| DoWhile -> sprintf "DoWhile (%s,%s)" (loop e) (loop econd))
	| TSwitch (e,cases,def) ->
		sprintf "Switch (%s,(%s)%s)" (loop e) (slist (fun (cl,e) -> sprintf "case %s: %s" (slist loop cl) (loop e)) cases) (match def with None -> "" | Some e -> "," ^ loop e)
	| TMatch (e,(en,tparams),cases,def) ->
		let args vl = slist (function None -> "_" | Some v -> sprintf "%s : %s" (s_var v) (s_type v.v_type)) vl in
		let cases = slist (fun (il,vl,e) -> sprintf "case %s%s : %s" (slist string_of_int il) (match vl with None -> "" | Some vl -> sprintf "(%s)" (args vl)) (loop e)) cases in
		sprintf "Match %s (%s,(%s)%s)" (s_type (TEnum (en,tparams))) (loop e) cases (match def with None -> "" | Some e -> "," ^ loop e)
	| TTry (e,cl) ->
		sprintf "Try %s(%s) " (loop e) (slist (fun (v,e) -> sprintf "catch( %s : %s ) %s" (s_var v) (s_type v.v_type) (loop e)) cl)
	| TReturn None ->
		"Return"
	| TReturn (Some e) ->
		sprintf "Return %s" (loop e)
	| TBreak ->
		"Break"
	| TContinue ->
		"Continue"
	| TThrow e ->
		"Throw " ^ (loop e)
	| TCast (e,t) ->
		sprintf "Cast %s%s" (match t with None -> "" | Some t -> s_type_path (t_path t) ^ ": ") (loop e)
	) in
	sprintf "(%s : %s)" str (s_type e.etype)

let rec s_expr_pretty tabs s_type e =
	let sprintf = Printf.sprintf in
	let loop = s_expr_pretty tabs s_type in
	let slist f l = String.concat "," (List.map f l) in
	match e.eexpr with
	| TConst c -> s_const c
	| TLocal v -> v.v_name
	| TArray (e1,e2) -> sprintf "%s[%s]" (loop e1) (loop e2)
	| TBinop (op,e1,e2) -> sprintf "%s %s %s" (loop e1) (s_binop op) (loop e2)
	| TField (e1,s) -> sprintf "%s.%s" (loop e1) (field_name s)
	| TTypeExpr mt -> (s_type_path (t_path mt))
	| TParenthesis e1 -> sprintf "(%s)" (loop e1)
	| TObjectDecl fl -> sprintf "{%s}" (slist (fun (f,e) -> sprintf "%s : %s" f (loop e)) fl)
	| TArrayDecl el -> sprintf "[%s]" (slist loop el)
	| TCall (e1,el) -> sprintf "%s(%s)" (loop e1) (slist loop el)
	| TNew (c,pl,el) ->
		sprintf "new %s(%s)" (s_type_path c.cl_path) (slist loop el)
	| TUnop (op,f,e) ->
		(match f with
		| Prefix -> sprintf "%s %s" (s_unop op) (loop e)
		| Postfix -> sprintf "%s %s" (loop e) (s_unop op))
	| TFunction f ->
		let args = slist (fun (v,o) -> sprintf "%s:%s%s" v.v_name (s_type v.v_type) (match o with None -> "" | Some c -> " = " ^ s_const c)) f.tf_args in
		sprintf "function(%s) = %s" args (loop f.tf_expr)
	| TVars vl ->
		sprintf "var %s" (slist (fun (v,eo) -> sprintf "%s%s" v.v_name (match eo with None -> "" | Some e -> " = " ^ loop e)) vl)
	| TBlock el ->
		let ntabs = tabs ^ "\t" in
		let s = sprintf "{\n%s" (String.concat "" (List.map (fun e -> sprintf "%s%s;\n" ntabs (s_expr_pretty ntabs s_type e)) el)) in
		s ^ tabs ^ "}"
	| TFor (v,econd,e) ->
		sprintf "for (%s in %s) %s" v.v_name (loop econd) (loop e)
	| TIf (e,e1,e2) ->
		sprintf "if (%s)%s%s)" (loop e) (loop e1) (match e2 with None -> "" | Some e -> " else " ^ loop e)
	| TWhile (econd,e,flag) ->
		(match flag with
		| NormalWhile -> sprintf "while (%s) %s" (loop econd) (loop e)
		| DoWhile -> sprintf "do (%s) while(%s)" (loop e) (loop econd))
	| TSwitch (e,cases,def) ->
		sprintf "switch (%s) {%s%s}" (loop e) (slist (fun (cl,e) -> sprintf "case %s: %s" (slist loop cl) (loop e)) cases) (match def with None -> "" | Some e -> ",default: " ^ loop e)
	| TMatch (e,(en,tparams),cases,def) ->
		let cases = slist (fun (il,vl,e) ->
			let ctor i = (PMap.find (List.nth en.e_names i) en.e_constrs).ef_name in
			let ctors = String.concat "," (List.map ctor il) in
			begin match vl with
				| None ->
					sprintf "case %s:%s" ctors (loop e)
				| Some vl ->
					sprintf "case %s(%s):%s" ctors (String.concat "," (List.map (fun v -> match v with None -> "_" | Some v -> v.v_name) vl)) (loop e)
			end
		) cases in
		sprintf "switch (%s) {%s%s}" (loop e) cases (match def with None -> "" | Some e -> ",default: " ^ loop e)
	| TTry (e,cl) ->
		sprintf "try %s%s" (loop e) (slist (fun (v,e) -> sprintf "catch( %s : %s ) %s" v.v_name (s_type v.v_type) (loop e)) cl)
	| TReturn None ->
		"return"
	| TReturn (Some e) ->
		sprintf "return %s" (loop e)
	| TBreak ->
		"break"
	| TContinue ->
		"continue"
	| TThrow e ->
		"throw " ^ (loop e)
	| TCast (e,None) ->
		sprintf "cast %s" (loop e)
	| TCast (e,Some mt) ->
		sprintf "cast (%s,%s)" (loop e) (s_type_path (t_path mt))