open Globals
open Ast
open Type
open Typecore
open Error

type field_host =
	| FAStatic of tclass
	| FAInstance of tclass * tparams
	| FAAbstract of tabstract * tparams * tclass
	| FAAnon

type field_access = {
	fa_on : texpr;
	fa_field : tclass_field;
	fa_host : field_host;
	fa_inline : bool;
	fa_pos : pos;
}

type static_extension_access = {
	se_this : texpr;
	se_access : field_access;
}

type access_kind =
	| AKNo of string
	| AKExpr of texpr
	| AKField of field_access
	| AKGetter of field_access (* fa_field is the property, not the accessor *)
	| AKSetter of field_access (* fa_field is the property, not the accessor *)
	| AKUsingField of static_extension_access
	| AKUsingGetter of static_extension_access * tclass_field
	| AKUsingSetter of static_extension_access * tclass_field
	| AKAccess of tabstract * tparams * tclass * texpr * texpr
	| AKFieldSet of texpr * texpr * string * t

type object_decl_kind =
	| ODKWithStructure of tanon
	| ODKWithClass of tclass * tparams
	| ODKPlain

let build_call_ref : (typer -> access_kind -> expr list -> WithType.t -> pos -> texpr) ref = ref (fun _ _ _ _ _ -> die "" __LOC__)
let type_call_target_ref : (typer -> expr -> expr list -> WithType.t -> bool -> pos -> access_kind) ref = ref (fun _ _ _ _ _ -> die "" __LOC__)

let relative_path ctx file =
	let slashes path = String.concat "/" (ExtString.String.nsplit path "\\") in
	let fpath = slashes (Path.get_full_path file) in
	let fpath_lower = String.lowercase fpath in
	let flen = String.length fpath_lower in
	let rec loop = function
		| [] -> file
		| path :: l ->
			let spath = String.lowercase (slashes path) in
			let slen = String.length spath in
			if slen > 0 && slen < flen && String.sub fpath_lower 0 slen = spath then String.sub fpath slen (flen - slen) else loop l
	in
	loop ctx.com.Common.class_path

let mk_infos ctx p params =
	let file = if ctx.in_macro then p.pfile else if Common.defined ctx.com Define.AbsolutePath then Path.get_full_path p.pfile else relative_path ctx p.pfile in
	(EObjectDecl (
		(("fileName",null_pos,NoQuotes) , (EConst (String(file,SDoubleQuotes)) , p)) ::
		(("lineNumber",null_pos,NoQuotes) , (EConst (Int (string_of_int (Lexer.get_error_line p))),p)) ::
		(("className",null_pos,NoQuotes) , (EConst (String (s_type_path ctx.curclass.cl_path,SDoubleQuotes)),p)) ::
		if ctx.curfield.cf_name = "" then
			params
		else
			(("methodName",null_pos,NoQuotes), (EConst (String (ctx.curfield.cf_name,SDoubleQuotes)),p)) :: params
	) ,p)

let rec is_pos_infos = function
	| TMono r ->
		(match r.tm_type with
		| Some t -> is_pos_infos t
		| _ -> false)
	| TLazy f ->
		is_pos_infos (lazy_type f)
	| TType ({ t_path = ["haxe"] , "PosInfos" },[]) ->
		true
	| TType (t,tl) ->
		is_pos_infos (apply_params t.t_params tl t.t_type)
	| TAbstract({a_path=[],"Null"},[t]) ->
		is_pos_infos t
	| _ ->
		false

let is_lower_ident s p =
	try Ast.is_lower_ident s
	with Invalid_argument msg -> error msg p

let get_this ctx p =
	match ctx.curfun with
	| FunStatic ->
		error "Cannot access this from a static function" p
	| FunMemberClassLocal | FunMemberAbstractLocal ->
		let v = match ctx.vthis with
			| None ->
				let v = if ctx.curfun = FunMemberAbstractLocal then
					PMap.find "this" ctx.locals
				else
					add_local ctx VGenerated "`this" ctx.tthis p
				in
				ctx.vthis <- Some v;
				v
			| Some v ->
				ctx.locals <- PMap.add v.v_name v ctx.locals;
				v
		in
		mk (TLocal v) ctx.tthis p
	| FunMemberAbstract ->
		let v = (try PMap.find "this" ctx.locals with Not_found -> die "" __LOC__) in
		mk (TLocal v) v.v_type p
	| FunConstructor | FunMember ->
		mk (TConst TThis) ctx.tthis p

let assign_to_this_is_allowed ctx =
	match ctx.curclass.cl_kind with
		| KAbstractImpl _ ->
			(match ctx.curfield.cf_kind with
				| Method MethInline -> true
				| Method _ when ctx.curfield.cf_name = "_new" -> true
				| _ -> false
			)
		| _ -> false

let rec type_module_type ctx t tparams p =
	match t with
	| TClassDecl {cl_kind = KGenericBuild _} ->
		let _,_,f = InstanceBuilder.build_instance ctx t p in
		let t = f (match tparams with None -> [] | Some tl -> tl) in
		let mt = try
			module_type_of_type t
		with Exit ->
			if follow t == t_dynamic then Typeload.load_type_def ctx p (mk_type_path ([],"Dynamic"))
			else error "Invalid module type" p
		in
		type_module_type ctx mt None p
	| TClassDecl c ->
		let t_tmp = class_module_type c in
		mk (TTypeExpr (TClassDecl c)) (TType (t_tmp,[])) p
	| TEnumDecl e ->
		let types = (match tparams with None -> Monomorph.spawn_constrained_monos (fun t -> t) e.e_params | Some l -> l) in
		mk (TTypeExpr (TEnumDecl e)) (TType (e.e_type,types)) p
	| TTypeDecl s ->
		let t = apply_params s.t_params (List.map (fun _ -> spawn_monomorph ctx p) s.t_params) s.t_type in
		DeprecationCheck.check_typedef ctx.com s p;
		(match follow t with
		| TEnum (e,params) ->
			type_module_type ctx (TEnumDecl e) (Some params) p
		| TInst (c,params) ->
			type_module_type ctx (TClassDecl c) (Some params) p
		| TAbstract (a,params) ->
			type_module_type ctx (TAbstractDecl a) (Some params) p
		| _ ->
			error (s_type_path s.t_path ^ " is not a value") p)
	| TAbstractDecl { a_impl = Some c } ->
		type_module_type ctx (TClassDecl c) tparams p
	| TAbstractDecl a ->
		if not (Meta.has Meta.RuntimeValue a.a_meta) then error (s_type_path a.a_path ^ " is not a value") p;
		let t_tmp = abstract_module_type a [] in
		mk (TTypeExpr (TAbstractDecl a)) (TType (t_tmp,[])) p

let type_type ctx tpath p =
	type_module_type ctx (Typeload.load_type_def ctx p (mk_type_path tpath)) None p

let mk_module_type_access ctx t p : access_mode -> WithType.t -> access_kind =
	let e = type_module_type ctx t None p in
	(fun _ _ -> AKExpr e)

let s_field_access tabs fa =
	let st = s_type (print_context()) in
	let se = s_expr_pretty true "" false st in
	let sfa = function
		| FAStatic c -> Printf.sprintf "FAStatic(%s)" (s_type_path c.cl_path)
		| FAInstance(c,tl) -> Printf.sprintf "FAInstance(%s, %s)" (s_type_path c.cl_path) (s_types tl)
		| FAAbstract(a,tl,c) -> Printf.sprintf "FAAbstract(%s, %s, %s)" (s_type_path a.a_path) (s_types tl) (s_type_path c.cl_path)
		| FAAnon -> Printf.sprintf "FAAnon"
	in
	Printer.s_record_fields tabs [
		"fa_on",se fa.fa_on;
		"fa_field",fa.fa_field.cf_name;
		"fa_host",sfa fa.fa_host;
		"fa_inline",string_of_bool fa.fa_inline
	]

let s_static_extension_access sea =
	Printer.s_record_fields "" [
		"se_this",s_expr_pretty true "" false (s_type (print_context())) sea.se_this;
		"se_access",s_field_access "\t" sea.se_access
	]

let s_access_kind acc =
	let st = s_type (print_context()) in
	let se = s_expr_pretty true "" false st in
	match acc with
	| AKNo s -> "AKNo " ^ s
	| AKExpr e -> "AKExpr " ^ (se e)
	| AKField fa -> Printf.sprintf "AKField(%s)" (s_field_access "" fa)
	| AKGetter fa -> Printf.sprintf "AKGetter(%s)" (s_field_access "" fa)
	| AKSetter fa -> Printf.sprintf "AKSetter(%s)" (s_field_access "" fa)
	| AKUsingField sea -> Printf.sprintf "AKUsingField(%s)" (s_static_extension_access sea)
	| AKUsingGetter(sea,_) -> Printf.sprintf "AKUsingGetter(%s)" (s_static_extension_access sea)
	| AKUsingSetter(sea,_) -> Printf.sprintf "AKUsingSetter(%s)" (s_static_extension_access sea)
	| AKAccess(a,tl,c,e1,e2) -> Printf.sprintf "AKAccess(%s, [%s], %s, %s, %s)" (s_type_path a.a_path) (String.concat ", " (List.map st tl)) (s_type_path c.cl_path) (se e1) (se e2)
	| AKFieldSet(_) -> ""

let get_constructible_constraint ctx tl p =
	let extract_function t = match follow t with
		| TFun(tl,tr) -> tl,tr
		| _ -> error "Constructible type parameter should be function" p
	in
	let rec loop tl = match tl with
		| [] -> None
		| t :: tl ->
			begin match follow t with
			| TAnon a ->
				begin try
					Some (extract_function (PMap.find "new" a.a_fields).cf_type);
				with Not_found ->
					loop tl
				end;
			| TAbstract({a_path = ["haxe"],"Constructible"},[t1]) ->
				Some (extract_function t1)
			| TInst({cl_kind = KTypeParameter tl1},_) ->
				begin match loop tl1 with
				| None -> loop tl
				| Some _ as t -> t
				end
			| _ ->
				loop tl
			end
	in
	loop tl

let unify_static_extension ctx e t p =
	let multitype_involed t1 t2 =
		let check t = match follow t with
			| TAbstract(a,_) when Meta.has Meta.MultiType a.a_meta -> true
			| _ -> false
		in
		check t1 || check t2
	in
	if multitype_involed e.etype t then
		AbstractCast.cast_or_unify_raise ctx t e p
	else begin
		Type.unify_custom {default_unification_context with allow_dynamic_to_cast = false} e.etype t;
		e
	end

let get_abstract_froms a pl =
	let l = List.map (apply_params a.a_params pl) a.a_from in
	List.fold_left (fun acc (t,f) ->
		match follow (Type.field_type f) with
		| TFun ([_,_,v],t) ->
			(try
				ignore(type_eq EqStrict t (TAbstract(a,List.map duplicate pl))); (* unify fields monomorphs *)
				v :: acc
			with Unify_error _ ->
				acc)
		| _ ->
			acc
	) l a.a_from_field

module FieldAccess = struct
	type field_host =
		| FGet (* get the plain expression with applied field type parameters *)
		| FCall (* does not apply field type parameters *)
		| FRead (* actual reading, for FClosure and such *)
		| FWrite (* used as lhs, no semantic difference to FGet *)

	let create e cf fh inline p = {
		fa_on = e;
		fa_field = cf;
		fa_host = fh;
		fa_inline = inline;
		fa_pos = p;
	}

	let apply_fa cf = function
		| FAStatic c -> FStatic(c,cf)
		| FAInstance(c,tl) -> FInstance(c,tl,cf)
		| FAAbstract(a,tl,c) -> FStatic(c,cf)
		| FAAnon -> FAnon cf

	let get_map_function fa = match fa.fa_host with
		| FAStatic _ | FAAnon -> (fun t -> t)
		| FAInstance(c,tl) -> TClass.get_map_function c tl
		| FAAbstract(a,tl,_) -> apply_params a.a_params tl

	let get_field_expr fa mode =
		let cf = fa.fa_field in
		let t = match mode with
			| FCall -> cf.cf_type
			| FGet | FRead | FWrite -> Type.field_type cf
		in
		let fa',t = match fa.fa_host with
			| FAStatic c ->
				FStatic(c,cf),t
			| FAInstance(c,tl) ->
				let fa = match cf.cf_kind with
				| Method _ when mode = FRead ->
					FClosure(Some(c,tl),cf)
				| _ ->
					FInstance(c,tl,cf)
				in
				let t = TClass.get_map_function c tl t in
				fa,t
			| FAAbstract(a,tl,c) ->
				FStatic(c,cf),apply_params a.a_params tl t
			| FAAnon ->
				let fa = match cf.cf_kind with
				| Method _ when mode = FRead ->
					FClosure(None,cf)
				| _ ->
					FAnon cf
				in
				fa,t
		in
		mk (TField(fa.fa_on,fa')) t fa.fa_pos
end

let make_static_extension_access c cf e_this inline p =
	let e_static = Texpr.Builder.make_static_this c p in
	{
		se_this = e_this;
		se_access = FieldAccess.create e_static cf (FAStatic c) inline p
	}

let make_abstract_static_extension_access a tl c cf e_this inline p =
	let e_static = Texpr.Builder.make_static_this c p in
	{
		se_this = e_this;
		se_access = FieldAccess.create e_static cf (FAAbstract(a,tl,c)) inline p
	}
