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
open Globals

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
	| AccNo             (* can't be accessed outside of the class itself and its subclasses *)
	| AccNever          (* can't be accessed, even in subclasses *)
	| AccCtor           (* can only be accessed from the constructor *)
	| AccResolve        (* call resolve("field") when accessed *)
	| AccCall           (* perform a method call when accessed *)
	| AccInline         (* similar to Normal but inline when accessed *)
	| AccRequire of string * string option (* set when @:require(cond) fails *)

and method_kind =
	| MethNormal
	| MethInline
	| MethDynamic
	| MethMacro

type module_check_policy =
	| NoCheckFileTimeModification
	| CheckFileContentModification
	| NoCheckDependencies
	| NoCheckShadowing


type t =
	| TMono of t option ref
	| TEnum of tenum * tparams
	| TInst of tclass * tparams
	| TType of tdef * tparams
	| TFun of tsignature
	| TAnon of tanon
	| TDynamic of t
	| TLazy of tlazy ref
	| TAbstract of tabstract * tparams

and tlazy =
	| LAvailable of t
	| LProcessing of (unit -> t)
	| LWait of (unit -> t)

and tsignature = (string * bool * t) list * t

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

and tvar_extra = (type_params * texpr option) option

and tvar = {
	mutable v_id : int;
	mutable v_name : string;
	mutable v_type : t;
	mutable v_capture : bool;
	mutable v_extra : tvar_extra;
	mutable v_meta : metadata;
	v_pos : pos;
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
	| Extend of t list
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
	| TObjectDecl of ((string * pos * quote_status) * texpr) list
	| TArrayDecl of texpr list
	| TCall of texpr * texpr list
	| TNew of tclass * tparams * texpr list
	| TUnop of Ast.unop * Ast.unop_flag * texpr
	| TFunction of tfunc
	| TVar of tvar * texpr option
	| TBlock of texpr list
	| TFor of tvar * texpr * texpr
	| TIf of texpr * texpr * texpr option
	| TWhile of texpr * texpr * Ast.while_flag
	| TSwitch of texpr * (texpr list * texpr) list * texpr option
	| TTry of texpr * (tvar * texpr) list
	| TReturn of texpr option
	| TBreak
	| TContinue
	| TThrow of texpr
	| TCast of texpr * module_type option
	| TMeta of metadata_entry * texpr
	| TEnumParameter of texpr * tenum_field * int
	| TEnumIndex of texpr
	| TIdent of string

and tfield_access =
	| FInstance of tclass * tparams * tclass_field
	| FStatic of tclass * tclass_field
	| FAnon of tclass_field
	| FDynamic of string
	| FClosure of (tclass * tparams) option * tclass_field (* None class = TAnon *)
	| FEnum of tenum * tenum_field

and texpr = {
	eexpr : texpr_expr;
	etype : t;
	epos : pos;
}

and tclass_field = {
	mutable cf_name : string;
	mutable cf_type : t;
	mutable cf_public : bool;
	cf_pos : pos;
	cf_name_pos : pos;
	mutable cf_doc : Ast.documentation;
	mutable cf_meta : metadata;
	mutable cf_kind : field_kind;
	mutable cf_params : type_params;
	mutable cf_expr : texpr option;
	mutable cf_expr_unoptimized : tfunc option;
	mutable cf_overloads : tclass_field list;
}

and tclass_kind =
	| KNormal
	| KTypeParameter of t list
	| KExpr of Ast.expr
	| KGeneric
	| KGenericInstance of tclass * tparams
	| KMacroType
	| KGenericBuild of class_field list
	| KAbstractImpl of tabstract

and metadata = Ast.metadata

and tinfos = {
	mt_path : path;
	mt_module : module_def;
	mt_pos : pos;
	mt_name_pos : pos;
	mt_private : bool;
	mt_doc : Ast.documentation;
	mutable mt_meta : metadata;
	mt_params : type_params;
}

and tclass = {
	mutable cl_path : path;
	mutable cl_module : module_def;
	mutable cl_pos : pos;
	mutable cl_name_pos : pos;
	mutable cl_private : bool;
	mutable cl_doc : Ast.documentation;
	mutable cl_meta : metadata;
	mutable cl_params : type_params;
	(* do not insert any fields above *)
	mutable cl_kind : tclass_kind;
	mutable cl_extern : bool;
	mutable cl_interface : bool;
	mutable cl_super : (tclass * tparams) option;
	mutable cl_implements : (tclass * tparams) list;
	mutable cl_fields : (string, tclass_field) PMap.t;
	mutable cl_statics : (string, tclass_field) PMap.t;
	mutable cl_ordered_statics : tclass_field list;
	mutable cl_ordered_fields : tclass_field list;
	mutable cl_dynamic : t option;
	mutable cl_array_access : t option;
	mutable cl_constructor : tclass_field option;
	mutable cl_init : texpr option;
	mutable cl_overrides : tclass_field list;

	mutable cl_build : unit -> build_state;
	mutable cl_restore : unit -> unit;
	(*
		These are classes which directly extend or directly implement this class.
		Populated automatically in post-processing step (Filters.run)
	*)
	mutable cl_descendants : tclass list;
}

and tenum_field = {
	ef_name : string;
	ef_type : t;
	ef_pos : pos;
	ef_name_pos : pos;
	ef_doc : Ast.documentation;
	ef_index : int;
	ef_params : type_params;
	mutable ef_meta : metadata;
}

and tenum = {
	mutable e_path : path;
	e_module : module_def;
	e_pos : pos;
	e_name_pos : pos;
	e_private : bool;
	e_doc : Ast.documentation;
	mutable e_meta : metadata;
	mutable e_params : type_params;
	(* do not insert any fields above *)
	e_type : tdef;
	mutable e_extern : bool;
	mutable e_constrs : (string , tenum_field) PMap.t;
	mutable e_names : string list;
}

and tdef = {
	t_path : path;
	t_module : module_def;
	t_pos : pos;
	t_name_pos : pos;
	t_private : bool;
	t_doc : Ast.documentation;
	mutable t_meta : metadata;
	mutable t_params : type_params;
	(* do not insert any fields above *)
	mutable t_type : t;
}

and tabstract = {
	mutable a_path : path;
	a_module : module_def;
	a_pos : pos;
	a_name_pos : pos;
	a_private : bool;
	a_doc : Ast.documentation;
	mutable a_meta : metadata;
	mutable a_params : type_params;
	(* do not insert any fields above *)
	mutable a_ops : (Ast.binop * tclass_field) list;
	mutable a_unops : (Ast.unop * unop_flag * tclass_field) list;
	mutable a_impl : tclass option;
	mutable a_this : t;
	mutable a_from : t list;
	mutable a_from_field : (t * tclass_field) list;
	mutable a_to : t list;
	mutable a_to_field : (t * tclass_field) list;
	mutable a_array : tclass_field list;
	mutable a_resolve : tclass_field option;
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
	mutable m_check_policy : module_check_policy list;
	mutable m_time : float;
	mutable m_dirty : module_def option;
	mutable m_added : int;
	mutable m_mark : int;
	mutable m_deps : (int,module_def) PMap.t;
	mutable m_processed : int;
	mutable m_kind : module_kind;
	mutable m_binded_res : (string, string) PMap.t;
	mutable m_macro_calls : string list;
	mutable m_if_feature : (string *(tclass * tclass_field * bool)) list;
	mutable m_features : (string,bool) Hashtbl.t;
}

and module_kind =
	| MCode
	| MMacro
	| MFake
	| MExtern
	| MImport

and build_state =
	| Built
	| Building of tclass list
	| BuildMacro of (unit -> unit) list ref

(* ======= General utility ======= *)

let alloc_var =
	let uid = ref 0 in
	(fun n t p -> incr uid; { v_name = n; v_type = t; v_id = !uid; v_capture = false; v_extra = None; v_meta = []; v_pos = p })

let alloc_mid =
	let mid = ref 0 in
	(fun() -> incr mid; !mid)

let mk e t p = { eexpr = e; etype = t; epos = p }

let mk_block e =
	match e.eexpr with
	| TBlock _ -> e
	| _ -> mk (TBlock [e]) e.etype e.epos

let mk_cast e t p = mk (TCast(e,None)) t p

let null t p = mk (TConst TNull) t p

let mk_mono() = TMono (ref None)

let rec t_dynamic = TDynamic t_dynamic

let mk_anon fl = TAnon { a_fields = fl; a_status = ref Closed; }

(* We use this for display purposes because otherwise we never see the Dynamic type that
   is defined in StdTypes.hx. This is set each time a typer is created, but this is fine
   because Dynamic is the same in all contexts. If this ever changes we'll have to review
   how we handle this. *)
let t_dynamic_def = ref t_dynamic

let tfun pl r = TFun (List.map (fun t -> "",false,t) pl,r)

let fun_args l = List.map (fun (a,c,t) -> a, c <> None, t) l

let mk_class m path pos name_pos =
	{
		cl_path = path;
		cl_module = m;
		cl_pos = pos;
		cl_name_pos = name_pos;
		cl_doc = None;
		cl_meta = [];
		cl_private = false;
		cl_kind = KNormal;
		cl_extern = false;
		cl_interface = false;
		cl_params = [];
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
		cl_build = (fun() -> Built);
		cl_restore = (fun() -> ());
		cl_descendants = [];
	}

let module_extra file sign time kind policy =
	{
		m_file = file;
		m_sign = sign;
		m_dirty = None;
		m_added = 0;
		m_mark = 0;
		m_time = time;
		m_processed = 0;
		m_deps = PMap.empty;
		m_kind = kind;
		m_binded_res = PMap.empty;
		m_macro_calls = [];
		m_if_feature = [];
		m_features = Hashtbl.create 0;
		m_check_policy = policy;
	}


let mk_field name t p name_pos = {
	cf_name = name;
	cf_type = t;
	cf_pos = p;
	cf_name_pos = name_pos;
	cf_doc = None;
	cf_meta = [];
	cf_public = true;
	cf_kind = Var { v_read = AccNormal; v_write = AccNormal };
	cf_expr = None;
	cf_expr_unoptimized = None;
	cf_params = [];
	cf_overloads = [];
}

let null_module = {
		m_id = alloc_mid();
		m_path = [] , "";
		m_types = [];
		m_extra = module_extra "" "" 0. MFake [];
	}

let null_class =
	let c = mk_class null_module ([],"") null_pos null_pos in
	c.cl_private <- true;
	c

let null_field = mk_field "" t_dynamic null_pos null_pos

let null_abstract = {
	a_path = ([],"");
	a_module = null_module;
	a_pos = null_pos;
	a_name_pos = null_pos;
	a_private = true;
	a_doc = None;
	a_meta = [];
	a_params = [];
	a_ops = [];
	a_unops = [];
	a_impl = None;
	a_this = t_dynamic;
	a_from = [];
	a_from_field = [];
	a_to = [];
	a_to_field = [];
	a_array = [];
	a_resolve = None;
}

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

let rec is_parent csup c =
	if c == csup || List.exists (fun (i,_) -> is_parent csup i) c.cl_implements then
		true
	else match c.cl_super with
		| None -> false
		| Some (c,_) -> is_parent csup c

let add_descendant c descendant =
	c.cl_descendants <- descendant :: c.cl_descendants

let lazy_type f =
	match !f with
	| LAvailable t -> t
	| LProcessing f | LWait f -> f()

let lazy_available t = LAvailable t
let lazy_processing f = LProcessing f
let lazy_wait f = LWait f

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
		let fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields in
		begin match !(a.a_status) with
			| Opened ->
				a.a_fields <- fields;
				t
			| _ ->
				TAnon {
					a_fields = fields;
					a_status = a.a_status;
				}
		end
	| TLazy f ->
		let ft = lazy_type f in
		let ft2 = loop ft in
		if ft == ft2 then t else ft2
	| TDynamic t2 ->
		if t == t2 then	t else TDynamic (loop t2)

let dup t =
	let monos = ref [] in
	let rec loop t =
		match t with
		| TMono { contents = None } ->
			(try
				List.assq t !monos
			with Not_found ->
				let m = mk_mono() in
				monos := (t,m) :: !monos;
				m)
		| _ ->
			map loop t
	in
	loop t

(* substitute parameters with other types *)
let apply_params cparams params t =
	match cparams with
	| [] -> t
	| _ ->
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (x,TLazy f) :: l1, _ -> loop ((x,lazy_type f) :: l1) l2
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
			let fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields in
			begin match !(a.a_status) with
				| Opened ->
					a.a_fields <- fields;
					t
				| _ ->
					TAnon {
						a_fields = fields;
						a_status = a.a_status;
					}
			end
		| TLazy f ->
			let ft = lazy_type f in
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

let monomorphs eparams t =
	apply_params eparams (List.map (fun _ -> mk_mono()) eparams) t

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (lazy_type f)
	| TType (t,tl) ->
		follow (apply_params t.t_params tl t.t_type)
	| TAbstract({a_path = [],"Null"},[t]) ->
		follow t
	| _ -> t

let rec is_nullable = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_nullable t)
	| TAbstract ({ a_path = ([],"Null") },[_]) ->
		true
	| TLazy f ->
		is_nullable (lazy_type f)
	| TType (t,tl) ->
		is_nullable (apply_params t.t_params tl t.t_type)
	| TFun _ ->
		false
(*
	Type parameters will most of the time be nullable objects, so we don't want to make it hard for users
	to have to specify Null<T> all over the place, so while they could be a basic type, let's assume they will not.

	This will still cause issues with inlining and haxe.rtti.Generic. In that case proper explicit Null<T> is required to
	work correctly with basic types. This could still be fixed by redoing a nullability inference on the typed AST.

	| TInst ({ cl_kind = KTypeParameter },_) -> false
*)
	| TAbstract (a,_) when Meta.has Meta.CoreType a.a_meta ->
		not (Meta.has Meta.NotNull a.a_meta)
	| TAbstract (a,tl) ->
		not (Meta.has Meta.NotNull a.a_meta) && is_nullable (apply_params a.a_params tl a.a_this)
	| _ ->
		true

let rec is_null ?(no_lazy=false) = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_null t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		not (is_nullable (follow t))
	| TLazy f ->
		if no_lazy then raise Exit else is_null (lazy_type f)
	| TType (t,tl) ->
		is_null (apply_params t.t_params tl t.t_type)
	| _ ->
		false

(* Determines if we have a Null<T>. Unlike is_null, this returns true even if the wrapped type is nullable itself. *)
let rec is_explicit_null = function
	| TMono r ->
		(match !r with None -> false | Some t -> is_null t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		true
	| TLazy f ->
		is_null (lazy_type f)
	| TType (t,tl) ->
		is_null (apply_params t.t_params tl t.t_type)
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
		PMap.fold (fun cf b -> has_mono cf.cf_type || b) a.a_fields false
	| TLazy f ->
		has_mono (lazy_type f)

let concat e1 e2 =
	let e = (match e1.eexpr, e2.eexpr with
		| TBlock el1, TBlock el2 -> TBlock (el1@el2)
		| TBlock el, _ -> TBlock (el @ [e2])
		| _, TBlock el -> TBlock (e1 :: el)
		| _ , _ -> TBlock [e1;e2]
	) in
	mk e e2.etype (punion e1.epos e2.epos)

let is_closed a = !(a.a_status) <> Opened

let type_of_module_type = function
	| TClassDecl c -> TInst (c,List.map snd c.cl_params)
	| TEnumDecl e -> TEnum (e,List.map snd e.e_params)
	| TTypeDecl t -> TType (t,List.map snd t.t_params)
	| TAbstractDecl a -> TAbstract (a,List.map snd a.a_params)

let rec module_type_of_type = function
	| TInst(c,_) -> TClassDecl c
	| TEnum(en,_) -> TEnumDecl en
	| TType(t,_) -> TTypeDecl t
	| TAbstract(a,_) -> TAbstractDecl a
	| TLazy f -> module_type_of_type (lazy_type f)
	| TMono r ->
		(match !r with
		| Some t -> module_type_of_type t
		| _ -> raise Exit)
	| _ ->
		raise Exit

let tconst_to_const = function
	| TInt i -> Int (Int32.to_string i)
	| TFloat s -> Float s
	| TString s -> String (s,Double) (* not much we can do about preserving quotes kind here *)
	| TBool b -> Ident (if b then "true" else "false")
	| TNull -> Ident "null"
	| TThis -> Ident "this"
	| TSuper -> Ident "super"

let has_ctor_constraint c = match c.cl_kind with
	| KTypeParameter tl ->
		List.exists (fun t -> match follow t with
			| TAnon a when PMap.mem "new" a.a_fields -> true
			| TAbstract({a_path=["haxe"],"Constructible"},_) -> true
			| _ -> false
		) tl;
	| _ -> false

(* ======= Field utility ======= *)

let field_name f =
	match f with
	| FAnon f | FInstance (_,_,f) | FStatic (_,f) | FClosure (_,f) -> f.cf_name
	| FEnum (_,f) -> f.ef_name
	| FDynamic n -> n

let extract_field = function
	| FAnon f | FInstance (_,_,f) | FStatic (_,f) | FClosure (_,f) -> Some f
	| _ -> None

let is_physical_field f =
	match f.cf_kind with
	| Method _ -> true
	| Var { v_read = AccNormal | AccInline | AccNo } | Var { v_write = AccNormal | AccNo } -> true
	| _ -> Meta.has Meta.IsVar f.cf_meta

let field_type f =
	match f.cf_params with
	| [] -> f.cf_type
	| l -> monomorphs l f.cf_type

let rec raw_class_field build_type c tl i =
	let apply = apply_params c.cl_params tl in
	try
		let f = PMap.find i c.cl_fields in
		Some (c,tl), build_type f , f
	with Not_found -> try (match c.cl_constructor with
		| Some ctor when i = "new" -> Some (c,tl), build_type ctor,ctor
		| _ -> raise Not_found)
	with Not_found -> try
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			let c2 , t , f = raw_class_field build_type c (List.map apply tl) i in
			c2, apply_params c.cl_params tl t , f
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
					| TInst (c,tl) ->
						(try
							let c2, t , f = raw_class_field build_type c (List.map apply tl) i in
							c2, apply_params c.cl_params tl t, f
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
						let c2, t , f = raw_class_field build_type c (List.map apply tl) i in
						c2, apply_params c.cl_params tl t, f
					with
						Not_found -> loop l
			in
			loop c.cl_implements

let class_field = raw_class_field field_type

let quick_field t n =
	match follow t with
	| TInst (c,tl) ->
		let c, _, f = raw_class_field (fun f -> f.cf_type) c tl n in
		(match c with None -> FAnon f | Some (c,tl) -> FInstance (c,tl,f))
	| TAnon a ->
		(match !(a.a_status) with
		| EnumStatics e ->
			let ef = PMap.find n e.e_constrs in
			FEnum(e,ef)
		| Statics c ->
			FStatic (c,PMap.find n c.cl_statics)
		| AbstractStatics a ->
			begin match a.a_impl with
				| Some c ->
					let cf = PMap.find n c.cl_statics in
					FStatic(c,cf) (* is that right? *)
				| _ ->
					raise Not_found
			end
		| _ ->
			FAnon (PMap.find n a.a_fields))
	| TDynamic _ ->
		FDynamic n
	| TEnum _  | TMono _ | TAbstract _ | TFun _ ->
		raise Not_found
	| TLazy _ | TType _ ->
		assert false

let quick_field_dynamic t s =
	try quick_field t s
	with Not_found -> FDynamic s

let rec get_constructor build_type c =
	match c.cl_constructor, c.cl_super with
	| Some c, _ -> build_type c, c
	| None, None -> raise Not_found
	| None, Some (csup,cparams) ->
		let t, c = get_constructor build_type csup in
		apply_params csup.cl_params cparams t, c

(* ======= Printing ======= *)

let print_context() = ref []

let rec s_type_kind t =
	let map tl = String.concat ", " (List.map s_type_kind tl) in
	match t with
	| TMono r ->
		begin match !r with
			| None -> "TMono (None)"
			| Some t -> "TMono (Some (" ^ (s_type_kind t) ^ "))"
		end
	| TEnum(en,tl) -> Printf.sprintf "TEnum(%s, [%s])" (s_type_path en.e_path) (map tl)
	| TInst(c,tl) -> Printf.sprintf "TInst(%s, [%s])" (s_type_path c.cl_path) (map tl)
	| TType(t,tl) -> Printf.sprintf "TType(%s, [%s])" (s_type_path t.t_path) (map tl)
	| TAbstract(a,tl) -> Printf.sprintf "TAbstract(%s, [%s])" (s_type_path a.a_path) (map tl)
	| TFun(tl,r) -> Printf.sprintf "TFun([%s], %s)" (String.concat ", " (List.map (fun (n,b,t) -> Printf.sprintf "%s%s:%s" (if b then "?" else "") n (s_type_kind t)) tl)) (s_type_kind r)
	| TAnon an -> "TAnon"
	| TDynamic t2 -> "TDynamic"
	| TLazy _ -> "TLazy"

let s_module_type_kind = function
	| TClassDecl c -> "TClassDecl(" ^ (s_type_path c.cl_path) ^ ")"
	| TEnumDecl en -> "TEnumDecl(" ^ (s_type_path en.e_path) ^ ")"
	| TAbstractDecl a -> "TAbstractDecl(" ^ (s_type_path a.a_path) ^ ")"
	| TTypeDecl t -> "TTypeDecl(" ^ (s_type_path t.t_path) ^ ")"

let rec s_type ctx t =
	match t with
	| TMono r ->
		(match !r with
		| None -> Printf.sprintf "Unknown<%d>" (try List.assq t (!ctx) with Not_found -> let n = List.length !ctx in ctx := (t,n) :: !ctx; n)
		| Some t -> s_type ctx t)
	| TEnum (e,tl) ->
		s_type_path e.e_path ^ s_type_params ctx tl
	| TInst (c,tl) ->
		(match c.cl_kind with
		| KExpr e -> Ast.s_expr e
		| _ -> s_type_path c.cl_path ^ s_type_params ctx tl)
	| TType (t,tl) ->
		s_type_path t.t_path ^ s_type_params ctx tl
	| TAbstract (a,tl) ->
		s_type_path a.a_path ^ s_type_params ctx tl
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
		s_type ctx (lazy_type f)

and s_fun ctx t void =
	match t with
	| TFun _ ->
		"(" ^ s_type ctx t ^ ")"
	| TAbstract ({ a_path = ([],"Void") },[]) when void ->
		"(" ^ s_type ctx t ^ ")"
	| TMono r ->
		(match !r with
		| None -> s_type ctx t
		| Some t -> s_fun ctx t void)
	| TLazy f ->
		s_fun ctx (lazy_type f) void
	| _ ->
		s_type ctx t

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

let s_access is_read = function
	| AccNormal -> "default"
	| AccNo -> "null"
	| AccNever -> "never"
	| AccResolve -> "resolve"
	| AccCall -> if is_read then "get" else "set"
	| AccInline	-> "inline"
	| AccRequire (n,_) -> "require " ^ n
	| AccCtor -> "ctor"

let s_kind = function
	| Var { v_read = AccNormal; v_write = AccNormal } -> "var"
	| Var v -> "(" ^ s_access true v.v_read ^ "," ^ s_access false v.v_write ^ ")"
	| Method m ->
		match m with
		| MethNormal -> "method"
		| MethDynamic -> "dynamic method"
		| MethInline -> "inline method"
		| MethMacro -> "macro method"

let s_expr_kind e =
	match e.eexpr with
	| TConst _ -> "Const"
	| TLocal _ -> "Local"
	| TArray (_,_) -> "Array"
	| TBinop (_,_,_) -> "Binop"
	| TEnumParameter (_,_,_) -> "EnumParameter"
	| TEnumIndex _ -> "EnumIndex"
	| TField (_,_) -> "Field"
	| TTypeExpr _ -> "TypeExpr"
	| TParenthesis _ -> "Parenthesis"
	| TObjectDecl _ -> "ObjectDecl"
	| TArrayDecl _ -> "ArrayDecl"
	| TCall (_,_) -> "Call"
	| TNew (_,_,_) -> "New"
	| TUnop (_,_,_) -> "Unop"
	| TFunction _ -> "Function"
	| TVar _ -> "Vars"
	| TBlock _ -> "Block"
	| TFor (_,_,_) -> "For"
	| TIf (_,_,_) -> "If"
	| TWhile (_,_,_) -> "While"
	| TSwitch (_,_,_) -> "Switch"
	| TTry (_,_) -> "Try"
	| TReturn _ -> "Return"
	| TBreak -> "Break"
	| TContinue -> "Continue"
	| TThrow _ -> "Throw"
	| TCast _ -> "Cast"
	| TMeta _ -> "Meta"
	| TIdent _ -> "Ident"

let s_const = function
	| TInt i -> Int32.to_string i
	| TFloat s -> s
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
	| TEnumIndex e1 ->
		sprintf "EnumIndex %s" (loop e1)
	| TEnumParameter (e1,_,i) ->
		sprintf "%s[%i]" (loop e1) i
	| TField (e,f) ->
		let fstr = (match f with
			| FStatic (c,f) -> "static(" ^ s_type_path c.cl_path ^ "." ^ f.cf_name ^ ")"
			| FInstance (c,_,f) -> "inst(" ^ s_type_path c.cl_path ^ "." ^ f.cf_name ^ " : " ^ s_type f.cf_type ^ ")"
			| FClosure (c,f) -> "closure(" ^ (match c with None -> f.cf_name | Some (c,_) -> s_type_path c.cl_path ^ "." ^ f.cf_name)  ^ ")"
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
		sprintf "ObjectDecl {%s}" (slist (fun ((f,_,qs),e) -> sprintf "%s : %s" (s_object_key_name f qs) (loop e)) fl)
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
	| TVar (v,eo) ->
		sprintf "Vars %s" (sprintf "%s : %s%s" (s_var v) (s_type v.v_type) (match eo with None -> "" | Some e -> " = " ^ loop e))
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
	| TMeta ((n,el,_),e) ->
		sprintf "@%s%s %s" (Meta.to_string n) (match el with [] -> "" | _ -> "(" ^ (String.concat ", " (List.map Ast.s_expr el)) ^ ")") (loop e)
	| TIdent s ->
		"Ident " ^ s
	) in
	sprintf "(%s : %s)" str (s_type e.etype)

let rec s_expr_pretty print_var_ids tabs top_level s_type e =
	let sprintf = Printf.sprintf in
	let loop = s_expr_pretty print_var_ids tabs false s_type in
	let slist c f l = String.concat c (List.map f l) in
	let clist f l = slist ", " f l in
	let local v = if print_var_ids then sprintf "%s<%i>" v.v_name v.v_id else v.v_name in
	match e.eexpr with
	| TConst c -> s_const c
	| TLocal v -> local v
	| TArray (e1,e2) -> sprintf "%s[%s]" (loop e1) (loop e2)
	| TBinop (op,e1,e2) -> sprintf "%s %s %s" (loop e1) (s_binop op) (loop e2)
	| TEnumParameter (e1,_,i) -> sprintf "%s[%i]" (loop e1) i
	| TEnumIndex e1 -> sprintf "enumIndex %s" (loop e1)
	| TField (e1,s) -> sprintf "%s.%s" (loop e1) (field_name s)
	| TTypeExpr mt -> (s_type_path (t_path mt))
	| TParenthesis e1 -> sprintf "(%s)" (loop e1)
	| TObjectDecl fl -> sprintf "{%s}" (clist (fun ((f,_,qs),e) -> sprintf "%s : %s" (s_object_key_name f qs) (loop e)) fl)
	| TArrayDecl el -> sprintf "[%s]" (clist loop el)
	| TCall (e1,el) -> sprintf "%s(%s)" (loop e1) (clist loop el)
	| TNew (c,pl,el) ->
		sprintf "new %s(%s)" (s_type_path c.cl_path) (clist loop el)
	| TUnop (op,f,e) ->
		(match f with
		| Prefix -> sprintf "%s %s" (s_unop op) (loop e)
		| Postfix -> sprintf "%s %s" (loop e) (s_unop op))
	| TFunction f ->
		let args = clist (fun (v,o) -> sprintf "%s:%s%s" (local v) (s_type v.v_type) (match o with None -> "" | Some c -> " = " ^ s_const c)) f.tf_args in
		sprintf "%s(%s) %s" (if top_level then "" else "function") args (loop f.tf_expr)
	| TVar (v,eo) ->
		sprintf "var %s" (sprintf "%s%s" (local v) (match eo with None -> "" | Some e -> " = " ^ loop e))
	| TBlock el ->
		let ntabs = tabs ^ "\t" in
		let s = sprintf "{\n%s" (String.concat "" (List.map (fun e -> sprintf "%s%s;\n" ntabs (s_expr_pretty print_var_ids ntabs top_level s_type e)) el)) in
		(match el with
			| [] -> "{}"
			| _ ->  s ^ tabs ^ "}")
	| TFor (v,econd,e) ->
		sprintf "for (%s in %s) %s" (local v) (loop econd) (loop e)
	| TIf (e,e1,e2) ->
		sprintf "if (%s) %s%s" (loop e) (loop e1) (match e2 with None -> "" | Some e -> " else " ^ loop e)
	| TWhile (econd,e,flag) ->
		(match flag with
		| NormalWhile -> sprintf "while (%s) %s" (loop econd) (loop e)
		| DoWhile -> sprintf "do (%s) while(%s)" (loop e) (loop econd))
	| TSwitch (e,cases,def) ->
		let ntabs = tabs ^ "\t" in
		let s = sprintf "switch (%s) {\n%s%s" (loop e) (slist "" (fun (cl,e) -> sprintf "%scase %s: %s;\n" ntabs (clist loop cl) (s_expr_pretty print_var_ids ntabs top_level s_type e)) cases) (match def with None -> "" | Some e -> ntabs ^ "default: " ^ (s_expr_pretty print_var_ids ntabs top_level s_type e) ^ "\n") in
		s ^ tabs ^ "}"
	| TTry (e,cl) ->
		sprintf "try %s%s" (loop e) (clist (fun (v,e) -> sprintf " catch (%s:%s) %s" (local v) (s_type v.v_type) (loop e)) cl)
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
	| TMeta ((n,el,_),e) ->
		sprintf "@%s%s %s" (Meta.to_string n) (match el with [] -> "" | _ -> "(" ^ (String.concat ", " (List.map Ast.s_expr el)) ^ ")") (loop e)
	| TIdent s ->
		s

let rec s_expr_ast print_var_ids tabs s_type e =
	let sprintf = Printf.sprintf in
	let loop ?(extra_tabs="") = s_expr_ast print_var_ids (tabs ^ "\t" ^ extra_tabs) s_type in
	let tag_args tabs sl = match sl with
		| [] -> ""
		| [s] when not (String.contains s '\n') -> " " ^ s
		| _ ->
			let tabs = "\n" ^ tabs ^ "\t" in
			tabs ^ (String.concat tabs sl)
	in
	let tag s ?(t=None) ?(extra_tabs="") sl =
		let st = match t with
			| None -> s_type e.etype
			| Some t -> s_type t
		in
		sprintf "[%s:%s]%s" s st (tag_args (tabs ^ extra_tabs) sl)
	in
	let var_id v = if print_var_ids then v.v_id else 0 in
	let const c t = tag "Const" ~t [s_const c] in
	let local v = sprintf "[Local %s(%i):%s]" v.v_name (var_id v) (s_type v.v_type) in
	let var v sl = sprintf "[Var %s(%i):%s]%s" v.v_name (var_id v) (s_type v.v_type) (tag_args tabs sl) in
	let module_type mt = sprintf "[TypeExpr %s:%s]" (s_type_path (t_path mt)) (s_type e.etype) in
	match e.eexpr with
	| TConst c -> const c (Some e.etype)
	| TLocal v -> local v
	| TArray (e1,e2) -> tag "Array" [loop e1; loop e2]
	| TBinop (op,e1,e2) -> tag "Binop" [loop e1; s_binop op; loop e2]
	| TUnop (op,flag,e1) -> tag "Unop" [s_unop op; if flag = Postfix then "Postfix" else "Prefix"; loop e1]
	| TEnumParameter (e1,ef,i) -> tag "EnumParameter" [loop e1; ef.ef_name; string_of_int i]
	| TEnumIndex e1 -> tag "EnumIndex" [loop e1]
	| TField (e1,fa) ->
		let sfa = match fa with
			| FInstance(c,tl,cf) -> tag "FInstance" ~extra_tabs:"\t" [s_type (TInst(c,tl)); cf.cf_name]
			| FStatic(c,cf) -> tag "FStatic" ~extra_tabs:"\t" [s_type_path c.cl_path; cf.cf_name]
			| FClosure(co,cf) -> tag "FClosure" ~extra_tabs:"\t" [(match co with None -> "None" | Some (c,tl) -> s_type (TInst(c,tl))); cf.cf_name]
			| FAnon cf -> tag "FAnon" ~extra_tabs:"\t" [cf.cf_name]
			| FDynamic s -> tag "FDynamic" ~extra_tabs:"\t" [s]
			| FEnum(en,ef) -> tag "FEnum" ~extra_tabs:"\t" [s_type_path en.e_path; ef.ef_name]
		in
		tag "Field" [loop e1; sfa]
	| TTypeExpr mt -> module_type mt
	| TParenthesis e1 -> tag "Parenthesis" [loop e1]
	| TObjectDecl fl -> tag "ObjectDecl" (List.map (fun ((s,_,qs),e) -> sprintf "%s: %s" (s_object_key_name s qs) (loop e)) fl)
	| TArrayDecl el -> tag "ArrayDecl" (List.map loop el)
	| TCall (e1,el) -> tag "Call" (loop e1 :: (List.map loop el))
	| TNew (c,tl,el) -> tag "New" ((s_type (TInst(c,tl))) :: (List.map loop el))
	| TFunction f ->
		let arg (v,cto) =
			tag "Arg" ~t:(Some v.v_type) ~extra_tabs:"\t" (match cto with None -> [local v] | Some ct -> [local v;const ct None])
		in
		tag "Function" ((List.map arg f.tf_args) @ [loop f.tf_expr])
	| TVar (v,eo) -> var v (match eo with None -> [] | Some e -> [loop e])
	| TBlock el -> tag "Block" (List.map loop el)
	| TIf (e,e1,e2) -> tag "If" (loop e :: (Printf.sprintf "[Then:%s] %s" (s_type e1.etype) (loop e1)) :: (match e2 with None -> [] | Some e -> [Printf.sprintf "[Else:%s] %s" (s_type e.etype) (loop e)]))
	| TCast (e1,None) -> tag "Cast" [loop e1]
	| TCast (e1,Some mt) -> tag "Cast" [loop e1; module_type mt]
	| TThrow e1 -> tag "Throw" [loop e1]
	| TBreak -> tag "Break" []
	| TContinue -> tag "Continue" []
	| TReturn None -> tag "Return" []
	| TReturn (Some e1) -> tag "Return" [loop e1]
	| TWhile (e1,e2,NormalWhile) -> tag "While" [loop e1; loop e2]
	| TWhile (e1,e2,DoWhile) -> tag "Do" [loop e1; loop e2]
	| TFor (v,e1,e2) -> tag "For" [local v; loop e1; loop e2]
	| TTry (e1,catches) ->
		let sl = List.map (fun (v,e) ->
			sprintf "Catch %s%s" (local v) (tag_args (tabs ^ "\t") [loop ~extra_tabs:"\t" e]);
		) catches in
		tag "Try" ((loop e1) :: sl)
	| TSwitch (e1,cases,eo) ->
		let sl = List.map (fun (el,e) ->
			tag "Case" ~t:(Some e.etype) ~extra_tabs:"\t" ((List.map loop el) @ [loop ~extra_tabs:"\t" e])
		) cases in
		let sl = match eo with
			| None -> sl
			| Some e -> sl @ [tag "Default" ~t:(Some e.etype) ~extra_tabs:"\t" [loop ~extra_tabs:"\t" e]]
		in
		tag "Switch" ((loop e1) :: sl)
	| TMeta ((m,el,_),e1) ->
		let s = Meta.to_string m in
		let s = match el with
			| [] -> s
			| _ -> sprintf "%s(%s)" s (String.concat ", " (List.map Ast.s_expr el))
		in
		tag "Meta" [s; loop e1]
	| TIdent s ->
		tag "Ident" [s]

let s_types ?(sep = ", ") tl =
	let pctx = print_context() in
	String.concat sep (List.map (s_type pctx) tl)

let s_class_kind = function
	| KNormal ->
		"KNormal"
	| KTypeParameter tl ->
		Printf.sprintf "KTypeParameter [%s]" (s_types tl)
	| KExpr _ ->
		"KExpr"
	| KGeneric ->
		"KGeneric"
	| KGenericInstance(c,tl) ->
		Printf.sprintf "KGenericInstance %s<%s>" (s_type_path c.cl_path) (s_types tl)
	| KMacroType ->
		"KMacroType"
	| KGenericBuild _ ->
		"KGenericBuild"
	| KAbstractImpl a ->
		Printf.sprintf "KAbstractImpl %s" (s_type_path a.a_path)

module Printer = struct

	let s_type t =
		s_type (print_context()) t

	let s_pair s1 s2 =
		Printf.sprintf "(%s,%s)" s1 s2

	let s_record_field name value =
		Printf.sprintf "%s = %s;" name value

	let s_pos p =
		Printf.sprintf "%s: %i-%i" p.pfile p.pmin p.pmax

	let s_record_fields tabs fields =
		let sl = List.map (fun (name,value) -> s_record_field name value) fields in
		Printf.sprintf "{\n%s\t%s\n%s}" tabs (String.concat ("\n\t" ^ tabs) sl) tabs

	let s_list sep f l =
		"[" ^ (String.concat sep (List.map f l)) ^ "]"

	let s_opt f o = match o with
		| None -> "None"
		| Some v -> f v

	let s_pmap fk fv pm =
		"{" ^ (String.concat ", " (PMap.foldi (fun k v acc -> (Printf.sprintf "%s = %s" (fk k) (fv v)) :: acc) pm [])) ^ "}"

	let s_doc = s_opt (fun s -> s)

	let s_metadata_entry (s,el,_) =
		Printf.sprintf "@%s%s" (Meta.to_string s) (match el with [] -> "" | el -> "(" ^ (String.concat ", " (List.map Ast.s_expr el)) ^ ")")

	let s_metadata metadata =
		s_list " " s_metadata_entry metadata

	let s_type_param (s,t) = match follow t with
		| TInst({cl_kind = KTypeParameter tl1},tl2) ->
			begin match tl1 with
			| [] -> s
			| _ -> Printf.sprintf "%s:%s" s (String.concat ", " (List.map s_type tl1))
			end
		| _ -> assert false

	let s_type_params tl =
		s_list ", " s_type_param tl

	let s_tclass_field tabs cf =
		s_record_fields tabs [
			"cf_name",cf.cf_name;
			"cf_doc",s_doc cf.cf_doc;
			"cf_type",s_type_kind (follow cf.cf_type);
			"cf_public",string_of_bool cf.cf_public;
			"cf_pos",s_pos cf.cf_pos;
			"cf_name_pos",s_pos cf.cf_name_pos;
			"cf_meta",s_metadata cf.cf_meta;
			"cf_kind",s_kind cf.cf_kind;
			"cf_params",s_type_params cf.cf_params;
			"cf_expr",s_opt (s_expr_ast true "\t\t" s_type) cf.cf_expr;
		]

	let s_tclass tabs c =
		s_record_fields tabs [
			"cl_path",s_type_path c.cl_path;
			"cl_module",s_type_path c.cl_module.m_path;
			"cl_pos",s_pos c.cl_pos;
			"cl_name_pos",s_pos c.cl_name_pos;
			"cl_private",string_of_bool c.cl_private;
			"cl_doc",s_doc c.cl_doc;
			"cl_meta",s_metadata c.cl_meta;
			"cl_params",s_type_params c.cl_params;
			"cl_kind",s_class_kind c.cl_kind;
			"cl_extern",string_of_bool c.cl_extern;
			"cl_interface",string_of_bool c.cl_interface;
			"cl_super",s_opt (fun (c,tl) -> s_type (TInst(c,tl))) c.cl_super;
			"cl_implements",s_list ", " (fun (c,tl) -> s_type (TInst(c,tl))) c.cl_implements;
			"cl_dynamic",s_opt s_type c.cl_dynamic;
			"cl_array_access",s_opt s_type c.cl_array_access;
			"cl_overrides",s_list "," (fun cf -> cf.cf_name) c.cl_overrides;
			"cl_init",s_opt (s_expr_ast true "" s_type) c.cl_init;
			"cl_constructor",s_opt (s_tclass_field (tabs ^ "\t")) c.cl_constructor;
			"cl_ordered_fields",s_list "\n\t" (s_tclass_field (tabs ^ "\t")) c.cl_ordered_fields;
			"cl_ordered_statics",s_list "\n\t" (s_tclass_field (tabs ^ "\t")) c.cl_ordered_statics;
		]

	let s_tdef tabs t =
		s_record_fields tabs [
			"t_path",s_type_path t.t_path;
			"t_module",s_type_path t.t_module.m_path;
			"t_pos",s_pos t.t_pos;
			"t_name_pos",s_pos t.t_name_pos;
			"t_private",string_of_bool t.t_private;
			"t_doc",s_doc t.t_doc;
			"t_meta",s_metadata t.t_meta;
			"t_params",s_type_params t.t_params;
			"t_type",s_type_kind t.t_type
		]

	let s_tenum_field tabs ef =
		s_record_fields tabs [
			"ef_name",ef.ef_name;
			"ef_doc",s_doc ef.ef_doc;
			"ef_pos",s_pos ef.ef_pos;
			"ef_name_pos",s_pos ef.ef_name_pos;
			"ef_type",s_type_kind ef.ef_type;
			"ef_index",string_of_int ef.ef_index;
			"ef_params",s_type_params ef.ef_params;
			"ef_meta",s_metadata ef.ef_meta
		]

	let s_tenum tabs en =
		s_record_fields tabs [
			"e_path",s_type_path en.e_path;
			"e_module",s_type_path en.e_module.m_path;
			"e_pos",s_pos en.e_pos;
			"e_name_pos",s_pos en.e_name_pos;
			"e_private",string_of_bool en.e_private;
			"d_doc",s_doc en.e_doc;
			"e_meta",s_metadata en.e_meta;
			"e_params",s_type_params en.e_params;
			"e_type",s_tdef "\t" en.e_type;
			"e_extern",string_of_bool en.e_extern;
			"e_constrs",s_list "\n\t" (s_tenum_field (tabs ^ "\t")) (PMap.fold (fun ef acc -> ef :: acc) en.e_constrs []);
			"e_names",String.concat ", " en.e_names
		]

	let s_tabstract tabs a =
		s_record_fields tabs [
			"a_path",s_type_path a.a_path;
			"a_modules",s_type_path a.a_module.m_path;
			"a_pos",s_pos a.a_pos;
			"a_name_pos",s_pos a.a_name_pos;
			"a_private",string_of_bool a.a_private;
			"a_doc",s_doc a.a_doc;
			"a_meta",s_metadata a.a_meta;
			"a_params",s_type_params a.a_params;
			"a_ops",s_list ", " (fun (op,cf) -> Printf.sprintf "%s: %s" (s_binop op) cf.cf_name) a.a_ops;
			"a_unops",s_list ", " (fun (op,flag,cf) -> Printf.sprintf "%s (%s): %s" (s_unop op) (if flag = Postfix then "postfix" else "prefix") cf.cf_name) a.a_unops;
			"a_impl",s_opt (fun c -> s_type_path c.cl_path) a.a_impl;
			"a_this",s_type_kind a.a_this;
			"a_from",s_list ", " s_type_kind a.a_from;
			"a_to",s_list ", " s_type_kind a.a_to;
			"a_from_field",s_list ", " (fun (t,cf) -> Printf.sprintf "%s: %s" (s_type_kind t) cf.cf_name) a.a_from_field;
			"a_to_field",s_list ", " (fun (t,cf) -> Printf.sprintf "%s: %s" (s_type_kind t) cf.cf_name) a.a_to_field;
			"a_array",s_list ", " (fun cf -> cf.cf_name) a.a_array;
			"a_resolve",s_opt (fun cf -> cf.cf_name) a.a_resolve;
		]

	let s_tvar_extra (tl,eo) =
		Printf.sprintf "Some(%s, %s)" (s_type_params tl) (s_opt (s_expr_ast true "" s_type) eo)

	let s_tvar v =
		s_record_fields "" [
			"v_id",string_of_int v.v_id;
			"v_name",v.v_name;
			"v_type",s_type v.v_type;
			"v_capture",string_of_bool v.v_capture;
			"v_extra",s_opt s_tvar_extra v.v_extra;
			"v_meta",s_metadata v.v_meta;
		]

	let s_module_kind = function
		| MCode -> "MCode"
		| MMacro -> "MMacro"
		| MFake -> "MFake"
		| MExtern -> "MExtern"
		| MImport -> "MImport"

	let s_module_def_extra tabs me =
		s_record_fields tabs [
			"m_file",me.m_file;
			"m_sign",me.m_sign;
			"m_time",string_of_float me.m_time;
			"m_dirty",s_opt (fun m -> s_type_path m.m_path) me.m_dirty;
			"m_added",string_of_int me.m_added;
			"m_mark",string_of_int me.m_mark;
			"m_deps",s_pmap string_of_int (fun m -> snd m.m_path) me.m_deps;
			"m_processed",string_of_int me.m_processed;
			"m_kind",s_module_kind me.m_kind;
			"m_binded_res",""; (* TODO *)
			"m_macro_calls",String.concat ", " me.m_macro_calls;
			"m_if_feature",""; (* TODO *)
			"m_features",""; (* TODO *)
		]

	let s_module_def m =
		s_record_fields "" [
			"m_id",string_of_int m.m_id;
			"m_path",s_type_path m.m_path;
			"m_extra",s_module_def_extra "\t" m.m_extra
		]

	let s_type_path tp =
		s_record_fields "" [
			"tpackage",s_list "." (fun s -> s) tp.tpackage;
			"tname",tp.tname;
			"tparams","";
			"tsub",s_opt (fun s -> s) tp.tsub;
		]

	let s_class_flag = function
		| HInterface -> "HInterface"
		| HExtern -> "HExtern"
		| HPrivate -> "HPrivate"
		| HExtends tp -> "HExtends " ^ (s_type_path (fst tp))
		| HImplements tp -> "HImplements " ^ (s_type_path (fst tp))

	let s_placed f (x,p) =
		s_pair (f x) (s_pos p)

	let s_class_field cff =
		s_record_fields "" [
			"cff_name",s_placed (fun s -> s) cff.cff_name;
			"cff_doc",s_opt (fun s -> s) cff.cff_doc;
			"cff_pos",s_pos cff.cff_pos;
			"cff_meta",s_metadata cff.cff_meta;
			"cff_access",s_list ", " Ast.s_access cff.cff_access;
		]
end

(* ======= Unification ======= *)

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
			loop (lazy_type f)
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

let link_dynamic a b = match follow a,follow b with
	| TMono r,TDynamic _ -> r := Some b
	| TDynamic _,TMono r -> r := Some a
	| _ -> ()

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
	| TAbstract (a1,l1), TAbstract (a2,l2) ->
		a1 == a2 && List.for_all2 fast_eq l1 l2
	| _ , _ ->
		false

let rec fast_eq_mono ml a b =
	if a == b then
		true
	else match a , b with
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		List.for_all2 (fun (_,_,t1) (_,_,t2) -> fast_eq_mono ml t1 t2) l1 l2 && fast_eq_mono ml r1 r2
	| TType (t1,l1), TType (t2,l2) ->
		t1 == t2 && List.for_all2 (fast_eq_mono ml) l1 l2
	| TEnum (e1,l1), TEnum (e2,l2) ->
		e1 == e2 && List.for_all2 (fast_eq_mono ml) l1 l2
	| TInst (c1,l1), TInst (c2,l2) ->
		c1 == c2 && List.for_all2 (fast_eq_mono ml) l1 l2
	| TAbstract (a1,l1), TAbstract (a2,l2) ->
		a1 == a2 && List.for_all2 (fast_eq_mono ml) l1 l2
	| TMono _, _ ->
		List.memq a ml
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
	| AccNo | AccNever | AccNormal | AccInline | AccRequire _ | AccCtor -> true
	| AccResolve | AccCall -> false

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
				match v.v_read,v.v_write with
				| AccNormal,(AccNo | AccNever) -> true
				| _ -> false)
		| Method m1, Method m2 ->
			match m1,m2 with
			| MethInline, MethNormal
			| MethDynamic, MethNormal -> true
			| _ -> false

let eq_stack = ref []

let rec_stack stack value fcheck frun ferror =
	if not (List.exists fcheck !stack) then begin
		try
			stack := value :: !stack;
			let v = frun() in
			stack := List.tl !stack;
			v
		with
			Unify_error l ->
				stack := List.tl !stack;
				ferror l
			| e ->
				stack := List.tl !stack;
				raise e
	end

let rec_stack_bool stack value fcheck frun =
	if (List.exists fcheck !stack) then false else begin
		try
			stack := value :: !stack;
			frun();
			stack := List.tl !stack;
			true
		with
			Unify_error l ->
				stack := List.tl !stack;
				false
			| e ->
				stack := List.tl !stack;
				raise e
	end

type eq_kind =
	| EqStrict
	| EqCoreType
	| EqRightDynamic
	| EqBothDynamic
	| EqDoNotFollowNull (* like EqStrict, but does not follow Null<T> *)

let rec type_eq param a b =
	let can_follow t = match param with
		| EqCoreType -> false
		| EqDoNotFollowNull -> not (is_explicit_null t)
		| _ -> true
	in
	if a == b then
		()
	else match a , b with
	| TLazy f , _ -> type_eq param (lazy_type f) b
	| _ , TLazy f -> type_eq param a (lazy_type f)
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
	| TType (t,tl) , _ when can_follow a ->
		type_eq param (apply_params t.t_params tl t.t_type) b
	| _ , TType (t,tl) when can_follow b ->
		rec_stack eq_stack (a,b)
			(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
			(fun() -> type_eq param a (apply_params t.t_params tl t.t_type))
			(fun l -> error (cannot_unify a b :: l))
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
	| TAbstract ({a_path=[],"Null"},[t1]),TAbstract ({a_path=[],"Null"},[t2]) ->
		type_eq param t1 t2
	| TAbstract ({a_path=[],"Null"},[t]),_ when param <> EqDoNotFollowNull ->
		type_eq param t b
	| _,TAbstract ({a_path=[],"Null"},[t]) when param <> EqDoNotFollowNull ->
		type_eq param a t
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		if a1 != a2 && not (param = EqCoreType && a1.a_path = a2.a_path) then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f1 ->
				try
					let f2 = PMap.find n a2.a_fields in
					if f1.cf_kind <> f2.cf_kind && (param = EqStrict || param = EqCoreType || not (unify_kind f1.cf_kind f2.cf_kind)) then error [invalid_kind n f1.cf_kind f2.cf_kind];
					let a = f1.cf_type and b = f2.cf_type in
					rec_stack eq_stack (a,b)
						(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
						(fun() -> type_eq param a b)
						(fun l -> error (invalid_field n :: l))
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

let type_iseq_strict a b =
	try
		type_eq EqDoNotFollowNull a b;
		true
	with Unify_error _ ->
		false

let unify_stack = ref []
let abstract_cast_stack = ref []
let unify_new_monos = ref []

let print_stacks() =
	let ctx = print_context() in
	let st = s_type ctx in
	print_endline "unify_stack";
	List.iter (fun (a,b) -> Printf.printf "\t%s , %s\n" (st a) (st b)) !unify_stack;
	print_endline "monos";
	List.iter (fun m -> print_endline ("\t" ^ st m)) !unify_new_monos;
	print_endline "abstract_cast_stack";
	List.iter (fun (a,b) -> Printf.printf "\t%s , %s\n" (st a) (st b)) !abstract_cast_stack

let rec unify a b =
	if a == b then
		()
	else match a, b with
	| TLazy f , _ -> unify (lazy_type f) b
	| _ , TLazy f -> unify a (lazy_type f)
	| TMono t , _ ->
		(match !t with
		| None -> if not (link t a b) then error [cannot_unify a b]
		| Some t -> unify t b)
	| _ , TMono t ->
		(match !t with
		| None -> if not (link t b a) then error [cannot_unify a b]
		| Some t -> unify a t)
	| TType (t,tl) , _ ->
		rec_stack unify_stack (a,b)
			(fun(a2,b2) -> fast_eq a a2 && fast_eq b b2)
			(fun() -> unify (apply_params t.t_params tl t.t_type) b)
			(fun l -> error (cannot_unify a b :: l))
	| _ , TType (t,tl) ->
		rec_stack unify_stack (a,b)
			(fun(a2,b2) -> fast_eq a a2 && fast_eq b b2)
			(fun() -> unify a (apply_params t.t_params tl t.t_type))
			(fun l -> error (cannot_unify a b :: l))
	| TEnum (ea,tl1) , TEnum (eb,tl2) ->
		if ea != eb then error [cannot_unify a b];
		unify_type_params a b tl1 tl2
	| TAbstract ({a_path=[],"Null"},[t]),_ ->
		begin try unify t b
		with Unify_error l -> error (cannot_unify a b :: l) end
	| _,TAbstract ({a_path=[],"Null"},[t]) ->
		begin try unify a t
		with Unify_error l -> error (cannot_unify a b :: l) end
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) when a1 == a2 ->
		begin try
			unify_type_params a b tl1 tl2
		with Unify_error _ as err ->
			(* the type could still have a from/to relation to itself (issue #3494) *)
			begin try
				unify_abstracts a b a1 tl1 a2 tl2
			with Unify_error _ ->
				raise err
			end
		end
	| TAbstract ({a_path=[],"Void"},_) , _
	| _ , TAbstract ({a_path=[],"Void"},_) ->
		error [cannot_unify a b]
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		unify_abstracts a b a1 tl1 a2 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		let rec loop c tl =
			if c == c2 then begin
				unify_type_params a b tl tl2;
				true
			end else (match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					loop cs (List.map (apply_params c.cl_params tl) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (apply_params c.cl_params tl) tls)
			) c.cl_implements
			|| (match c.cl_kind with
			| KTypeParameter pl -> List.exists (fun t ->
				match follow t with
				| TInst (cs,tls) -> loop cs (List.map (apply_params c.cl_params tl) tls)
				| TAbstract(aa,tl) -> List.exists (unify_to aa tl b) aa.a_to
				| _ -> false
			) pl
			| _ -> false)
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		let i = ref 0 in
		(try
			(match r2 with
			| TAbstract ({a_path=[],"Void"},_) -> incr i
			| _ -> unify r1 r2; incr i);
			List.iter2 (fun (_,o1,t1) (_,o2,t2) ->
				if o1 && not o2 then error [Cant_force_optional];
				unify t1 t2;
				incr i
			) l2 l1 (* contravariance *)
		with
			Unify_error l ->
				let msg = if !i = 0 then "Cannot unify return types" else "Cannot unify argument " ^ (string_of_int !i) in
				error (cannot_unify a b :: Unify_custom msg :: l))
	| TInst (c,tl) , TAnon an ->
		if PMap.is_empty an.a_fields then (match c.cl_kind with
			| KTypeParameter pl ->
				(* one of the constraints must unify with { } *)
				if not (List.exists (fun t -> match follow t with TInst _ | TAnon _ -> true | _ -> false) pl) then error [cannot_unify a b]
			| _ -> ());
		(try
			PMap.iter (fun n f2 ->
				(*
					introducing monomorphs while unifying might create infinite loops - see #2315
					let's store these monomorphs and make sure we reach a fixed point
				*)
				let monos = ref [] in
				let make_type f =
					match f.cf_params with
					| [] -> f.cf_type
					| l ->
						let ml = List.map (fun _ -> mk_mono()) l in
						monos := ml;
						apply_params f.cf_params ml f.cf_type
				in
				let _, ft, f1 = (try raw_class_field make_type c tl n with Not_found -> error [has_no_field a n]) in
				let ft = apply_params c.cl_params tl ft in
				if not (unify_kind f1.cf_kind f2.cf_kind) then error [invalid_kind n f1.cf_kind f2.cf_kind];
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];

				(match f2.cf_kind with
				| Var { v_read = AccNo } | Var { v_read = AccNever } ->
					(* we will do a recursive unification, so let's check for possible recursion *)
					let old_monos = !unify_new_monos in
					unify_new_monos := !monos @ !unify_new_monos;
					rec_stack unify_stack (ft,f2.cf_type)
						(fun (a2,b2) -> fast_eq b2 f2.cf_type && fast_eq_mono !unify_new_monos ft a2)
						(fun() -> try unify_with_access ft f2 with e -> unify_new_monos := old_monos; raise e)
						(fun l -> error (invalid_field n :: l));
					unify_new_monos := old_monos;
				| Method MethNormal | Method MethInline | Var { v_write = AccNo } | Var { v_write = AccNever } ->
					(* same as before, but unification is reversed (read-only var) *)
					let old_monos = !unify_new_monos in
					unify_new_monos := !monos @ !unify_new_monos;
					rec_stack unify_stack (f2.cf_type,ft)
						(fun(a2,b2) -> fast_eq_mono !unify_new_monos b2 ft && fast_eq f2.cf_type a2)
						(fun() -> try unify_with_access ft f2 with e -> unify_new_monos := old_monos; raise e)
						(fun l -> error (invalid_field n :: l));
					unify_new_monos := old_monos;
				| _ ->
					(* will use fast_eq, which have its own stack *)
					try
						unify_with_access ft f2
					with
						Unify_error l ->
							error (invalid_field n :: l));

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
			(match !(an.a_status) with
			| Opened -> an.a_status := Closed;
			| Statics _ | EnumStatics _ | AbstractStatics _ -> error []
			| Closed | Extend _ | Const -> ())
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon a1, TAnon a2 ->
		unify_anons a b a1 a2
	| TAnon an, TAbstract ({ a_path = [],"Class" },[pt]) ->
		(match !(an.a_status) with
		| Statics cl -> unify (TInst (cl,List.map (fun _ -> mk_mono()) cl.cl_params)) pt
		| _ -> error [cannot_unify a b])
	| TAnon an, TAbstract ({ a_path = [],"Enum" },[pt]) ->
		(match !(an.a_status) with
		| EnumStatics e -> unify (TEnum (e,List.map (fun _ -> mk_mono()) e.e_params)) pt
		| _ -> error [cannot_unify a b])
	| TEnum _, TAbstract ({ a_path = [],"EnumValue" },[]) ->
		()
	| TEnum(en,_), TAbstract ({ a_path = ["haxe"],"FlatEnum" },[]) when Meta.has Meta.FlatEnum en.e_meta ->
		()
	| TFun _, TAbstract ({ a_path = ["haxe"],"Function" },[]) ->
		()
	| TInst(c,tl),TAbstract({a_path = ["haxe"],"Constructible"},[t1]) ->
		begin try
			begin match c.cl_kind with
				| KTypeParameter tl ->
					(* type parameters require an equal Constructible constraint *)
					if not (List.exists (fun t -> match follow t with TAbstract({a_path = ["haxe"],"Constructible"},[t2]) -> type_iseq t1 t2 | _ -> false) tl) then error [cannot_unify a b]
				| _ ->
					let _,t,cf = class_field c tl "new" in
					if not cf.cf_public then error [invalid_visibility "new"];
					begin try unify t t1
					with Unify_error l -> error (cannot_unify a b :: l) end
			end
		with Not_found ->
			error [has_no_field a "new"]
		end
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
		| TAbstract(bb,tl) when (List.exists (unify_from bb tl a b) bb.a_from) ->
			()
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
		| TAbstract(aa,tl) when (List.exists (unify_to aa tl b) aa.a_to) ->
			()
		| _ ->
			error [cannot_unify a b])
	| TAbstract (aa,tl), _  ->
		if not (List.exists (unify_to aa tl b) aa.a_to) then error [cannot_unify a b];
	| TInst ({ cl_kind = KTypeParameter ctl } as c,pl), TAbstract (bb,tl) ->
		(* one of the constraints must satisfy the abstract *)
		if not (List.exists (fun t ->
			let t = apply_params c.cl_params pl t in
			try unify t b; true with Unify_error _ -> false
		) ctl) && not (List.exists (unify_from bb tl a b) bb.a_from) then error [cannot_unify a b];
	| _, TAbstract (bb,tl) ->
		if not (List.exists (unify_from bb tl a b) bb.a_from) then error [cannot_unify a b]
	| _ , _ ->
		error [cannot_unify a b]

and unify_abstracts a b a1 tl1 a2 tl2 =
	let f1 = unify_to a1 tl1 b in
		let f2 = unify_from a2 tl2 a b in
		if (List.exists (f1 ~allow_transitive_cast:false) a1.a_to)
		|| (List.exists (f2 ~allow_transitive_cast:false) a2.a_from)
		|| (((Meta.has Meta.CoreType a1.a_meta) || (Meta.has Meta.CoreType a2.a_meta))
			&& ((List.exists f1 a1.a_to) || (List.exists f2 a2.a_from))) then
			()
		else
			error [cannot_unify a b]

and unify_anons a b a1 a2 =
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
				unify_with_access (field_type f1) f2;
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
		| AbstractStatics a -> (match !(a1.a_status) with AbstractStatics a2 when a == a2 -> () | _ -> error [])
		| Opened -> a2.a_status := Closed
		| Const | Extend _ | Closed -> ())
	with
		Unify_error l -> error (cannot_unify a b :: l))

and unify_from ab tl a b ?(allow_transitive_cast=true) t =
	rec_stack_bool abstract_cast_stack (a,b)
		(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let t = apply_params ab.a_params tl t in
			let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
			unify_func a t)

and unify_to ab tl b ?(allow_transitive_cast=true) t =
	let t = apply_params ab.a_params tl t in
	let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
	try
		unify_func t b;
		true
	with Unify_error _ ->
		false

and unify_from_field ab tl a b ?(allow_transitive_cast=true) (t,cf) =
	rec_stack_bool abstract_cast_stack (a,b)
		(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
			match follow cf.cf_type with
			| TFun(_,r) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_params tl (apply_params cf.cf_params monos t) in
				unify_func a (map t);
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> unify m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
				unify_func (map r) b;
				true
			| _ -> assert false)

and unify_to_field ab tl b ?(allow_transitive_cast=true) (t,cf) =
	let a = TAbstract(ab,tl) in
	rec_stack_bool abstract_cast_stack (b,a)
		(fun (b2,a2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let unify_func = if allow_transitive_cast then unify else type_eq EqStrict in
			match follow cf.cf_type with
			| TFun((_,_,ta) :: _,_) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_params tl (apply_params cf.cf_params monos t) in
				let athis = map ab.a_this in
				(* we cannot allow implicit casts when the this type is not completely known yet *)
				(* if has_mono athis then raise (Unify_error []); *)
				with_variance (type_eq EqStrict) athis (map ta);
				(* immediate constraints checking is ok here because we know there are no monomorphs *)
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> unify m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
				unify_func (map t) b;
			| _ -> assert false)

and unify_with_variance f t1 t2 =
	let allows_variance_to t tf = type_iseq tf t in
	match follow t1,follow t2 with
	| TInst(c1,tl1),TInst(c2,tl2) when c1 == c2 ->
		List.iter2 f tl1 tl2
	| TEnum(en1,tl1),TEnum(en2,tl2) when en1 == en2 ->
		List.iter2 f tl1 tl2
	| TAbstract(a1,tl1),TAbstract(a2,tl2) when a1 == a2 && Meta.has Meta.CoreType a1.a_meta ->
		List.iter2 f tl1 tl2
	| TAbstract(a1,pl1),TAbstract(a2,pl2) ->
		if (Meta.has Meta.CoreType a1.a_meta) && (Meta.has Meta.CoreType a2.a_meta) then begin
			let ta1 = apply_params a1.a_params pl1 a1.a_this in
			let ta2 = apply_params a2.a_params pl2 a2.a_this in
			type_eq EqStrict ta1 ta2;
		end;
		if not (List.exists (allows_variance_to t2) a1.a_to) && not (List.exists (allows_variance_to t1) a2.a_from) then
			error [cannot_unify t1 t2]
	| TAbstract(a,pl),t ->
		type_eq EqBothDynamic (apply_params a.a_params pl a.a_this) t;
		if not (List.exists (fun t2 -> allows_variance_to t (apply_params a.a_params pl t2)) a.a_to) then error [cannot_unify t1 t2]
	| t,TAbstract(a,pl) ->
		type_eq EqBothDynamic t (apply_params a.a_params pl a.a_this);
		if not (List.exists (fun t2 -> allows_variance_to t (apply_params a.a_params pl t2)) a.a_from) then error [cannot_unify t1 t2]
	| (TAnon a1 as t1), (TAnon a2 as t2) ->
		rec_stack unify_stack (t1,t2)
			(fun (a,b) -> fast_eq a t1 && fast_eq b t2)
			(fun() -> unify_anons t1 t2 a1 a2)
			(fun l -> error l)
	| _ ->
		error [cannot_unify t1 t2]

and unify_type_params a b tl1 tl2 =
	List.iter2 (fun t1 t2 ->
		try
			with_variance (type_eq EqRightDynamic) t1 t2
		with Unify_error l ->
			let err = cannot_unify a b in
			error (err :: (Invariant_parameter (t1,t2)) :: l)
	) tl1 tl2

and with_variance f t1 t2 =
	try
		f t1 t2
	with Unify_error l -> try
		unify_with_variance (with_variance f) t1 t2
	with Unify_error _ ->
		raise (Unify_error l)

and unify_with_access t1 f2 =
	match f2.cf_kind with
	(* write only *)
	| Var { v_read = AccNo } | Var { v_read = AccNever } -> unify f2.cf_type t1
	(* read only *)
	| Method MethNormal | Method MethInline | Var { v_write = AccNo } | Var { v_write = AccNever } -> unify t1 f2.cf_type
	(* read/write *)
	| _ -> with_variance (type_eq EqBothDynamic) t1 f2.cf_type

(* ======= Mapping and iterating ======= *)

let iter f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TBreak
	| TContinue
	| TTypeExpr _
	| TIdent _ ->
		()
	| TArray (e1,e2)
	| TBinop (_,e1,e2)
	| TFor (_,e1,e2)
	| TWhile (e1,e2,_) ->
		f e1;
		f e2;
	| TThrow e
	| TField (e,_)
	| TEnumParameter (e,_,_)
	| TEnumIndex e
	| TParenthesis e
	| TCast (e,_)
	| TUnop (_,_,e)
	| TMeta(_,e) ->
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
	| TVar (v,eo) ->
		(match eo with None -> () | Some e -> f e)
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
	| TTypeExpr _
	| TIdent _ ->
		e
	| TArray (e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TArray (e1,f e2) }
	| TBinop (op,e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TBinop (op,e1,f e2) }
	| TFor (v,e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TFor (v,e1,f e2) }
	| TWhile (e1,e2,flag) ->
		let e1 = f e1 in
		{ e with eexpr = TWhile (e1,f e2,flag) }
	| TThrow e1 ->
		{ e with eexpr = TThrow (f e1) }
	| TEnumParameter (e1,ef,i) ->
		{ e with eexpr = TEnumParameter(f e1,ef,i) }
	| TEnumIndex e1 ->
		{ e with eexpr = TEnumIndex (f e1) }
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
		let e1 = f e1 in
		{ e with eexpr = TCall (e1, List.map f el) }
	| TVar (v,eo) ->
		{ e with eexpr = TVar (v, match eo with None -> None | Some e -> Some (f e)) }
	| TFunction fu ->
		{ e with eexpr = TFunction { fu with tf_expr = f fu.tf_expr } }
	| TIf (ec,e1,e2) ->
		let ec = f ec in
		let e1 = f e1 in
		{ e with eexpr = TIf (ec,e1,match e2 with None -> None | Some e -> Some (f e)) }
	| TSwitch (e1,cases,def) ->
		let e1 = f e1 in
		let cases = List.map (fun (el,e2) -> List.map f el, f e2) cases in
		{ e with eexpr = TSwitch (e1, cases, match def with None -> None | Some e -> Some (f e)) }
	| TTry (e1,catches) ->
		let e1 = f e1 in
		{ e with eexpr = TTry (e1, List.map (fun (v,e) -> v, f e) catches) }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)) }
	| TCast (e1,t) ->
		{ e with eexpr = TCast (f e1,t) }
	| TMeta (m,e1) ->
		 {e with eexpr = TMeta(m,f e1)}

let map_expr_type f ft fv e =
	match e.eexpr with
	| TConst _
	| TBreak
	| TContinue
	| TTypeExpr _
	| TIdent _ ->
		{ e with etype = ft e.etype }
	| TLocal v ->
		{ e with eexpr = TLocal (fv v); etype = ft e.etype }
	| TArray (e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TArray (e1,f e2); etype = ft e.etype }
	| TBinop (op,e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TBinop (op,e1,f e2); etype = ft e.etype }
	| TFor (v,e1,e2) ->
		let v = fv v in
		let e1 = f e1 in
		{ e with eexpr = TFor (v,e1,f e2); etype = ft e.etype }
	| TWhile (e1,e2,flag) ->
		let e1 = f e1 in
		{ e with eexpr = TWhile (e1,f e2,flag); etype = ft e.etype }
	| TThrow e1 ->
		{ e with eexpr = TThrow (f e1); etype = ft e.etype }
	| TEnumParameter (e1,ef,i) ->
		{ e with eexpr = TEnumParameter (f e1,ef,i); etype = ft e.etype }
	| TEnumIndex e1 ->
		{ e with eexpr = TEnumIndex (f e1); etype = ft e.etype }
	| TField (e1,v) ->
		let e1 = f e1 in
		let v = try
			let n = match v with
				| FClosure _ -> raise Not_found
				| FAnon f | FInstance (_,_,f) | FStatic (_,f) -> f.cf_name
				| FEnum (_,f) -> f.ef_name
				| FDynamic n -> n
			in
			quick_field e1.etype n
		with Not_found ->
			v
		in
		{ e with eexpr = TField (e1,v); etype = ft e.etype }
	| TParenthesis e1 ->
		{ e with eexpr = TParenthesis (f e1); etype = ft e.etype }
	| TUnop (op,pre,e1) ->
		{ e with eexpr = TUnop (op,pre,f e1); etype = ft e.etype }
	| TArrayDecl el ->
		{ e with eexpr = TArrayDecl (List.map f el); etype = ft e.etype }
	| TNew (c,pl,el) ->
		let et = ft e.etype in
		(* make sure that we use the class corresponding to the replaced type *)
		let t = match c.cl_kind with
			| KTypeParameter _ | KGeneric ->
				et
			| _ ->
				ft (TInst(c,pl))
		in
		let c, pl = (match follow t with TInst (c,pl) -> (c,pl) | TAbstract({a_impl = Some c},pl) -> c,pl | t -> error [has_no_field t "new"]) in
		{ e with eexpr = TNew (c,pl,List.map f el); etype = et }
	| TBlock el ->
		{ e with eexpr = TBlock (List.map f el); etype = ft e.etype }
	| TObjectDecl el ->
		{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el); etype = ft e.etype }
	| TCall (e1,el) ->
		let e1 = f e1 in
		{ e with eexpr = TCall (e1, List.map f el); etype = ft e.etype }
	| TVar (v,eo) ->
		{ e with eexpr = TVar (fv v, match eo with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TFunction fu ->
		let fu = {
			tf_expr = f fu.tf_expr;
			tf_args = List.map (fun (v,o) -> fv v, o) fu.tf_args;
			tf_type = ft fu.tf_type;
		} in
		{ e with eexpr = TFunction fu; etype = ft e.etype }
	| TIf (ec,e1,e2) ->
		let ec = f ec in
		let e1 = f e1 in
		{ e with eexpr = TIf (ec,e1,match e2 with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TSwitch (e1,cases,def) ->
		let e1 = f e1 in
		let cases = List.map (fun (el,e2) -> List.map f el, f e2) cases in
		{ e with eexpr = TSwitch (e1, cases, match def with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TTry (e1,catches) ->
		let e1 = f e1 in
		{ e with eexpr = TTry (e1, List.map (fun (v,e) -> fv v, f e) catches); etype = ft e.etype }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TCast (e1,t) ->
		{ e with eexpr = TCast (f e1,t); etype = ft e.etype }
	| TMeta (m,e1) ->
		{e with eexpr = TMeta(m, f e1); etype = ft e.etype }

let resolve_typedef t =
	match t with
	| TClassDecl _ | TEnumDecl _ | TAbstractDecl _ -> t
	| TTypeDecl td ->
		match follow td.t_type with
		| TEnum (e,_) -> TEnumDecl e
		| TInst (c,_) -> TClassDecl c
		| TAbstract (a,_) -> TAbstractDecl a
		| _ -> t

module TExprToExpr = struct
	let tpath p mp pl =
		if snd mp = snd p then
			CTPath {
				tpackage = fst p;
				tname = snd p;
				tparams = List.map (fun t -> TPType t) pl;
				tsub = None;
			}
		else CTPath {
				tpackage = fst mp;
				tname = snd mp;
				tparams = List.map (fun t -> TPType t) pl;
				tsub = Some (snd p);
			}

	let rec convert_type = function
		| TMono r ->
			(match !r with
			| None -> raise Exit
			| Some t -> convert_type t)
		| TInst ({cl_private = true; cl_path=_,name},tl)
		| TEnum ({e_private = true; e_path=_,name},tl)
		| TType ({t_private = true; t_path=_,name},tl)
		| TAbstract ({a_private = true; a_path=_,name},tl) ->
			CTPath {
				tpackage = [];
				tname = name;
				tparams = List.map (fun t -> TPType (convert_type' t)) tl;
				tsub = None;
			}
		| TEnum (e,pl) ->
			tpath e.e_path e.e_module.m_path (List.map convert_type' pl)
		| TInst({cl_kind = KTypeParameter _} as c,pl) ->
			tpath ([],snd c.cl_path) ([],snd c.cl_path) (List.map convert_type' pl)
		| TInst (c,pl) ->
			tpath c.cl_path c.cl_module.m_path (List.map convert_type' pl)
		| TType (t,pl) as tf ->
			(* recurse on type-type *)
			if (snd t.t_path).[0] = '#' then convert_type (follow tf) else tpath t.t_path t.t_module.m_path (List.map convert_type' pl)
		| TAbstract (a,pl) ->
			tpath a.a_path a.a_module.m_path (List.map convert_type' pl)
		| TFun (args,ret) ->
			CTFunction (List.map (fun (_,_,t) -> convert_type' t) args, (convert_type' ret))
		| TAnon a ->
			begin match !(a.a_status) with
			| Statics c -> tpath ([],"Class") ([],"Class") [tpath c.cl_path c.cl_path [],null_pos]
			| EnumStatics e -> tpath ([],"Enum") ([],"Enum") [tpath e.e_path e.e_path [],null_pos]
			| _ ->
				CTAnonymous (PMap.foldi (fun _ f acc ->
					{
						cff_name = f.cf_name,null_pos;
						cff_kind = FVar (mk_type_hint f.cf_type null_pos,None);
						cff_pos = f.cf_pos;
						cff_doc = f.cf_doc;
						cff_meta = f.cf_meta;
						cff_access = [];
					} :: acc
				) a.a_fields [])
			end
		| (TDynamic t2) as t ->
			tpath ([],"Dynamic") ([],"Dynamic") (if t == t_dynamic then [] else [convert_type' t2])
		| TLazy f ->
			convert_type (lazy_type f)

	and convert_type' t =
		convert_type t,null_pos

	and mk_type_hint t p =
		match follow t with
		| TMono _ -> None
		| _ -> (try Some (convert_type t,p) with Exit -> None)

	let rec convert_expr e =
		let full_type_path t =
			let mp,p = match t with
			| TClassDecl c -> c.cl_module.m_path,c.cl_path
			| TEnumDecl en -> en.e_module.m_path,en.e_path
			| TAbstractDecl a -> a.a_module.m_path,a.a_path
			| TTypeDecl t -> t.t_module.m_path,t.t_path
			in
			if snd mp = snd p then p else (fst mp) @ [snd mp],snd p
		in
		let mk_path = expr_of_type_path in
		let mk_ident = function
			| "`trace" -> Ident "trace"
			| n -> Ident n
		in
		let eopt = function None -> None | Some e -> Some (convert_expr e) in
		((match e.eexpr with
		| TConst c ->
			EConst (tconst_to_const c)
		| TLocal v -> EConst (mk_ident v.v_name)
		| TArray (e1,e2) -> EArray (convert_expr e1,convert_expr e2)
		| TBinop (op,e1,e2) -> EBinop (op, convert_expr e1, convert_expr e2)
		| TField (e,f) -> EField (convert_expr e, field_name f)
		| TTypeExpr t -> fst (mk_path (full_type_path t) e.epos)
		| TParenthesis e -> EParenthesis (convert_expr e)
		| TObjectDecl fl -> EObjectDecl (List.map (fun (k,e) -> k, convert_expr e) fl)
		| TArrayDecl el -> EArrayDecl (List.map convert_expr el)
		| TCall (e,el) -> ECall (convert_expr e,List.map convert_expr el)
		| TNew (c,pl,el) -> ENew ((match (try convert_type (TInst (c,pl)) with Exit -> convert_type (TInst (c,[]))) with CTPath p -> p,null_pos | _ -> assert false),List.map convert_expr el)
		| TUnop (op,p,e) -> EUnop (op,p,convert_expr e)
		| TFunction f ->
			let arg (v,c) = (v.v_name,v.v_pos), false, v.v_meta, mk_type_hint v.v_type null_pos, (match c with None -> None | Some c -> Some (EConst (tconst_to_const c),e.epos)) in
			EFunction (None,{ f_params = []; f_args = List.map arg f.tf_args; f_type = mk_type_hint f.tf_type null_pos; f_expr = Some (convert_expr f.tf_expr) })
		| TVar (v,eo) ->
			EVars ([(v.v_name,v.v_pos), mk_type_hint v.v_type v.v_pos, eopt eo])
		| TBlock el -> EBlock (List.map convert_expr el)
		| TFor (v,it,e) ->
			let ein = (EBinop (OpIn,(EConst (Ident v.v_name),it.epos),convert_expr it),it.epos) in
			EFor (ein,convert_expr e)
		| TIf (e,e1,e2) -> EIf (convert_expr e,convert_expr e1,eopt e2)
		| TWhile (e1,e2,flag) -> EWhile (convert_expr e1, convert_expr e2, flag)
		| TSwitch (e,cases,def) ->
			let cases = List.map (fun (vl,e) ->
				List.map convert_expr vl,None,(match e.eexpr with TBlock [] -> None | _ -> Some (convert_expr e)),e.epos
			) cases in
			let def = match eopt def with None -> None | Some (EBlock [],_) -> Some (None,null_pos) | Some e -> Some (Some e,pos e) in
			ESwitch (convert_expr e,cases,def)
		| TEnumIndex _
		| TEnumParameter _ ->
			(* these are considered complex, so the AST is handled in TMeta(Meta.Ast) *)
			assert false
		| TTry (e,catches) ->
			let e1 = convert_expr e in
			let catches = List.map (fun (v,e) ->
				let ct = try convert_type v.v_type,null_pos with Exit -> assert false in
				let e = convert_expr e in
				(v.v_name,v.v_pos),ct,e,(pos e)
			) catches in
			ETry (e1,catches)
		| TReturn e -> EReturn (eopt e)
		| TBreak -> EBreak
		| TContinue -> EContinue
		| TThrow e -> EThrow (convert_expr e)
		| TCast (e,t) ->
			let t = (match t with
				| None -> None
				| Some t ->
					let t = (match t with TClassDecl c -> TInst (c,[]) | TEnumDecl e -> TEnum (e,[]) | TTypeDecl t -> TType (t,[]) | TAbstractDecl a -> TAbstract (a,[])) in
					Some (try convert_type t,null_pos with Exit -> assert false)
			) in
			ECast (convert_expr e,t)
		| TMeta ((Meta.Ast,[e1,_],_),_) -> e1
		| TMeta (m,e) -> EMeta(m,convert_expr e)
		| TIdent s -> EConst (Ident s))
		,e.epos)

end

module Texpr = struct
	let equal_fa fa1 fa2 = match fa1,fa2 with
		| FStatic(c1,cf1),FStatic(c2,cf2) -> c1 == c2 && cf1 == cf2
		| FInstance(c1,tl1,cf1),FInstance(c2,tl2,cf2) -> c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1 == cf2
		(* TODO: This is technically not correct but unfortunately the compiler makes a distinct tclass_field for each anon field access. *)
		| FAnon cf1,FAnon cf2 -> cf1.cf_name = cf2.cf_name
		| FDynamic s1,FDynamic s2 -> s1 = s2
		| FClosure(None,cf1),FClosure(None,cf2) -> cf1 == cf2
		| FClosure(Some(c1,tl1),cf1),FClosure(Some(c2,tl2),cf2) -> c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1 == cf2
		| FEnum(en1,ef1),FEnum(en2,ef2) -> en1 == en2 && ef1 == ef2
		| _ -> false

	let rec equal e1 e2 = match e1.eexpr,e2.eexpr with
		| TConst ct1,TConst ct2 -> ct1 = ct2
		| TLocal v1,TLocal v2 -> v1 == v2
		| TArray(eb1,ei1),TArray(eb2,ei2) -> equal eb1 eb2 && equal ei1 ei2
		| TBinop(op1,lhs1,rhs1),TBinop(op2,lhs2,rhs2) -> op1 = op2 && equal lhs1 lhs2 && equal rhs1 rhs2
		| TField(e1,fa1),TField(e2,fa2) -> equal e1 e2 && equal_fa fa1 fa2
		| TTypeExpr mt1,TTypeExpr mt2 -> mt1 == mt2
		| TParenthesis e1,TParenthesis e2 -> equal e1 e2
		| TObjectDecl fl1,TObjectDecl fl2 -> safe_for_all2 (fun (s1,e1) (s2,e2) -> s1 = s2 && equal e1 e2) fl1 fl2
		| (TArrayDecl el1,TArrayDecl el2) | (TBlock el1,TBlock el2) -> safe_for_all2 equal el1 el2
		| TCall(e1,el1),TCall(e2,el2) -> equal e1 e2 && safe_for_all2 equal el1 el2
		| TNew(c1,tl1,el1),TNew(c2,tl2,el2) -> c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && safe_for_all2 equal el1 el2
		| TUnop(op1,flag1,e1),TUnop(op2,flag2,e2) -> op1 = op2 && flag1 = flag2 && equal e1 e2
		| TFunction tf1,TFunction tf2 -> tf1 == tf2
		| TVar(v1,None),TVar(v2,None) -> v1 == v2
		| TVar(v1,Some e1),TVar(v2,Some e2) -> v1 == v2 && equal e1 e2
		| TFor(v1,ec1,eb1),TFor(v2,ec2,eb2) -> v1 == v2 && equal ec1 ec2 && equal eb1 eb2
		| TIf(e1,ethen1,None),TIf(e2,ethen2,None) -> equal e1 e2 && equal ethen1 ethen2
		| TIf(e1,ethen1,Some eelse1),TIf(e2,ethen2,Some eelse2) -> equal e1 e2 && equal ethen1 ethen2 && equal eelse1 eelse2
		| TWhile(e1,eb1,flag1),TWhile(e2,eb2,flag2) -> equal e1 e2 && equal eb2 eb2 && flag1 = flag2
		| TSwitch(e1,cases1,eo1),TSwitch(e2,cases2,eo2) ->
			equal e1 e2 &&
			safe_for_all2 (fun (el1,e1) (el2,e2) -> safe_for_all2 equal el1 el2 && equal e1 e2) cases1 cases2 &&
			(match eo1,eo2 with None,None -> true | Some e1,Some e2 -> equal e1 e2 | _ -> false)
		| TTry(e1,catches1),TTry(e2,catches2) -> equal e1 e2 && safe_for_all2 (fun (v1,e1) (v2,e2) -> v1 == v2 && equal e1 e2) catches1 catches2
		| TReturn None,TReturn None -> true
		| TReturn(Some e1),TReturn(Some e2) -> equal e1 e2
		| TThrow e1,TThrow e2 -> equal e1 e2
		| TCast(e1,None),TCast(e2,None) -> equal e1 e2
		| TCast(e1,Some mt1),TCast(e2,Some mt2) -> equal e1 e2 && mt1 == mt2
		| TMeta((m1,el1,_),e1),TMeta((m2,el2,_),e2) -> m1 = m2 && safe_for_all2 (fun e1 e2 -> (* TODO: cheating? *) (Ast.s_expr e1) = (Ast.s_expr e2)) el1 el2 && equal e1 e2
		| (TBreak,TBreak) | (TContinue,TContinue) -> true
		| TEnumParameter(e1,ef1,i1),TEnumParameter(e2,ef2,i2) -> equal e1 e2 && ef1 == ef2 && i1 = i2
		| _ -> false

	let duplicate_tvars e =
		let vars = Hashtbl.create 0 in
		let copy_var v =
			let v2 = alloc_var v.v_name v.v_type v.v_pos in
			v2.v_meta <- v.v_meta;
			v2.v_extra <- v.v_extra;
			Hashtbl.add vars v.v_id v2;
			v2;
		in
		let rec build_expr e =
			match e.eexpr with
			| TVar (v,eo) ->
				let v2 = copy_var v in
				{e with eexpr = TVar(v2, Option.map build_expr eo)}
			| TFor (v,e1,e2) ->
				let v2 = copy_var v in
				{e with eexpr = TFor(v2, build_expr e1, build_expr e2)}
			| TTry (e1,cl) ->
				let cl = List.map (fun (v,e) ->
					let v2 = copy_var v in
					v2, build_expr e
				) cl in
				{e with eexpr = TTry(build_expr e1, cl)}
			| TFunction f ->
				let args = List.map (fun (v,c) -> copy_var v, c) f.tf_args in
				let f = {
					tf_args = args;
					tf_type = f.tf_type;
					tf_expr = build_expr f.tf_expr;
				} in
				{e with eexpr = TFunction f}
			| TLocal v ->
				(try
					let v2 = Hashtbl.find vars v.v_id in
					{e with eexpr = TLocal v2}
				with _ ->
					e)
			| _ ->
				map_expr build_expr e
		in
		build_expr e

	let rec skip e = match e.eexpr with
		| TParenthesis e1 | TMeta(_,e1) | TBlock [e1] | TCast(e1,None) -> skip e1
		| _ -> e

	let foldmap_list f acc el =
		let rec loop acc el acc2 = (match el with
			| [] -> acc,(List.rev acc2)
			| e1 :: el ->
				let acc,e1 = f acc e1 in
				loop acc el (e1 :: acc2))
		in loop acc el []

	let foldmap_opt f acc eo = match eo with
		| Some(e) -> let acc,e = f acc e in acc,Some(e)
		| None    -> acc,eo

	let foldmap_pairs f acc pairs =
		let acc,pairs = List.fold_left
			(fun (acc,el) (v,e) -> let acc,e = f acc e in (acc,(v,e) :: el))
			(acc,[])
			pairs
		in acc,(List.rev pairs)

	let foldmap f acc e =
		begin match e.eexpr with
		| TConst _
		| TLocal _
		| TBreak
		| TContinue
		| TTypeExpr _
		| TIdent _ ->
			acc,e
		| TArray (e1,e2) ->
			let acc,e1 = f acc e1 in
			let acc,e2 = f acc e2 in
			acc,{ e with eexpr = TArray (e1, e2) }
		| TBinop (op,e1,e2) ->
			let acc,e1 = f acc e1 in
			let acc,e2 = f acc e2 in
			acc,{ e with eexpr = TBinop (op,e1,e2) }
		| TFor (v,e1,e2) ->
			let acc,e1 = f acc e1 in
			let acc,e2 = f acc e2 in
			acc,{ e with eexpr = TFor (v,e1,e2) }
		| TWhile (e1,e2,flag) ->
			let acc,e1 = f acc e1 in
			let acc,e2 = f acc e2 in
			acc,{ e with eexpr = TWhile (e1,e2,flag) }
		| TThrow e1 ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TThrow (e1) }
		| TEnumParameter (e1,ef,i) ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TEnumParameter(e1,ef,i) }
		| TEnumIndex e1 ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TEnumIndex e1 }
		| TField (e1,v) ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TField (e1,v) }
		| TParenthesis e1 ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TParenthesis (e1) }
		| TUnop (op,pre,e1) ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TUnop (op,pre,e1) }
		| TArrayDecl el ->
			let acc,el = foldmap_list f acc el in
			acc,{ e with eexpr = TArrayDecl el }
		| TNew (t,pl,el) ->
			let acc,el = foldmap_list f acc el in
			acc,{ e with eexpr = TNew (t,pl,el) }
		| TBlock el ->
			let acc,el = foldmap_list f acc el in
			acc,{ e with eexpr = TBlock (el) }
		| TObjectDecl el ->
			let acc,el = foldmap_pairs f acc el in
			acc,{ e with eexpr = TObjectDecl el }
		| TCall (e1,el) ->
			let acc,e1 = f acc e1 in
			let acc,el = foldmap_list f acc el in
			acc,{ e with eexpr = TCall (e1,el) }
		| TVar (v,eo) ->
			let acc,eo = foldmap_opt f acc eo in
			acc,{ e with eexpr = TVar (v, eo) }
		| TFunction fu ->
			let acc,e1 = f acc fu.tf_expr in
			acc,{ e with eexpr = TFunction { fu with tf_expr = e1 } }
		| TIf (ec,e1,eo) ->
			let acc,ec = f acc ec in
			let acc,e1 = f acc e1 in
			let acc,eo = foldmap_opt f acc eo in
			acc,{ e with eexpr = TIf (ec,e1,eo)}
		| TSwitch (e1,cases,def) ->
			let acc,e1 = f acc e1 in
			let acc,cases = List.fold_left (fun (acc,cases) (el,e2) ->
				let acc,el = foldmap_list f acc el in
				let acc,e2 = f acc e2 in
				acc,((el,e2) :: cases)
			) (acc,[]) cases in
			let acc,def = foldmap_opt f acc def in
			acc,{ e with eexpr = TSwitch (e1, cases, def) }
		| TTry (e1,catches) ->
			let acc,e1 = f acc e1 in
			let acc,catches = foldmap_pairs f acc catches in
			acc,{ e with eexpr = TTry (e1, catches) }
		| TReturn eo ->
			let acc,eo = foldmap_opt f acc eo in
			acc,{ e with eexpr = TReturn eo }
		| TCast (e1,t) ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TCast (e1,t) }
		| TMeta (m,e1) ->
			let acc,e1 = f acc e1 in
			acc,{ e with eexpr = TMeta(m,e1)}
		end
end

module ExtType = struct
	let is_void = function
		| TAbstract({a_path=[],"Void"},_) -> true
		| _ -> false
end

module StringError = struct
	(* Source: http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance#OCaml *)
	let levenshtein a b =
		let x = Array.init (String.length a) (fun i -> a.[i]) in
		let y = Array.init (String.length b) (fun i -> b.[i]) in
		let minimum (x:int) y z =
			let m' (a:int) b = if a < b then a else b in
			m' (m' x y) z
		in
		let init_matrix n m =
			let init_col = Array.init m in
				Array.init n (function
				| 0 -> init_col (function j -> j)
				| i -> init_col (function 0 -> i | _ -> 0)
			)
		in
		match Array.length x, Array.length y with
			| 0, n -> n
			| m, 0 -> m
			| m, n ->
				let matrix = init_matrix (m + 1) (n + 1) in
				for i = 1 to m do
					let s = matrix.(i) and t = matrix.(i - 1) in
					for j = 1 to n do
						let cost = abs (compare x.(i - 1) y.(j - 1)) in
						s.(j) <- minimum (t.(j) + 1) (s.(j - 1) + 1) (t.(j - 1) + cost)
					done
				done;
				matrix.(m).(n)

	let filter_similar f cl =
		let rec loop sl = match sl with
			| (x,i) :: sl when f x i -> x :: loop sl
			| _ -> []
		in
		loop cl

	let get_similar s sl =
		if sl = [] then [] else
		let cl = List.map (fun s2 -> s2,levenshtein s s2) sl in
		let cl = List.sort (fun (_,c1) (_,c2) -> compare c1 c2) cl in
		let cl = filter_similar (fun s2 i -> i <= (min (String.length s) (String.length s2)) / 3) cl in
		cl

	let string_error_raise s sl msg =
		if sl = [] then msg else
		let cl = get_similar s sl in
		match cl with
			| [] -> raise Not_found
			| [s] -> Printf.sprintf "%s (Suggestion: %s)" msg s
			| sl -> Printf.sprintf "%s (Suggestions: %s)" msg (String.concat ", " sl)

	let string_error s sl msg =
		try string_error_raise s sl msg
		with Not_found -> msg
end
