open Globals
open Ast
open Type
open Typecore
open Error

type field_host =
	| FHStatic of tclass
	| FHInstance of tclass * tparams
	| FHAbstract of tabstract * tparams * tclass
	| FHAnon

type field_access = {
	(* The expression on which the field is accessed. For abstracts, this is a type expression
	   to the implementation class. *)
	fa_on     : texpr;
	(* The field being accessed. *)
	fa_field  : tclass_field;
	(* The host of the field. *)
	fa_host   : field_host;
	(* Whether or not to inline the access. This can be set for non-inline fields via `inline call()` syntax. *)
	fa_inline : bool;
	(* The position of the field access expression in syntax. *)
	fa_pos    : pos;
}

type static_extension_access = {
	(* The `this` expression which should be passed as first argument. *)
	se_this   : texpr;
	(* The field access information. *)
	se_access : field_access;
}

type access_kind =
	(* Access is not possible or allowed. *)
	| AKNo of string
	(* Access on arbitrary expression. *)
	| AKExpr of texpr
	(* Access on non-property field. *)
	| AKField of field_access
	(* Access on property field. The field is the property, not the accessor. *)
	| AKAccessor of field_access
	(* Access via static extension. *)
	| AKUsingField of static_extension_access
	(* Access via static extension on property field. The field is the property, not the accessor.
	   This currently only happens on abstract properties. *)
	| AKUsingAccessor of static_extension_access
	(* Access on abstract via array overload. *)
	| AKAccess of tabstract * tparams * tclass * texpr * texpr
	(* Access on abstract via resolve method. *)
	| AKResolve of static_extension_access * string

type object_decl_kind =
	| ODKWithStructure of tanon
	| ODKWithClass of tclass * tparams
	| ODKPlain

let type_call_target_ref : (typer -> expr -> expr list -> WithType.t -> bool -> pos -> access_kind) ref = ref (fun _ _ _ _ _ -> die "" __LOC__)
let type_access_ref : (typer -> expr_def -> pos -> access_mode -> WithType.t -> access_kind) ref = ref (fun _ _ _ _ _ -> assert false)

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

let mk_module_type_access ctx t p =
	AKExpr (type_module_type ctx t None p)

let s_field_access tabs fa =
	let st = s_type (print_context()) in
	let se = s_expr_pretty true "" false st in
	let sfa = function
		| FHStatic c -> Printf.sprintf "FHStatic(%s)" (s_type_path c.cl_path)
		| FHInstance(c,tl) -> Printf.sprintf "FHInstance(%s, %s)" (s_type_path c.cl_path) (s_types tl)
		| FHAbstract(a,tl,c) -> Printf.sprintf "FHAbstract(%s, %s, %s)" (s_type_path a.a_path) (s_types tl) (s_type_path c.cl_path)
		| FHAnon -> Printf.sprintf "FHAnon"
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
	| AKAccessor fa -> Printf.sprintf "AKAccessor(%s)" (s_field_access "" fa)
	| AKUsingField sea -> Printf.sprintf "AKUsingField(%s)" (s_static_extension_access sea)
	| AKUsingAccessor sea -> Printf.sprintf "AKUsingAccessor(%s)" (s_static_extension_access sea)
	| AKAccess(a,tl,c,e1,e2) -> Printf.sprintf "AKAccess(%s, [%s], %s, %s, %s)" (s_type_path a.a_path) (String.concat ", " (List.map st tl)) (s_type_path c.cl_path) (se e1) (se e2)
	| AKResolve(_) -> ""

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
		(* Get the plain expression with applied field type parameters. *)
		| FGet
		(* Does not apply field type parameters. *)
		| FCall
		(* Actual reading, for FClosure and such. *)
		| FRead
		(* Used as lhs, no semantic difference to FGet. *)
		| FWrite

	type accessor_resolution =
		(* Accessor was found. *)
		| AccessorFound of field_access
		(* Accessor was not found, but access was made on anonymous structure. *)
		| AccessorAnon
		(* Accessor was not found. *)
		| AccessorNotFound
		(* Accessor resolution was attempted on a non-property. *)
		| AccessorInvalid

	let create e cf fh inline p = {
		fa_on     = e;
		fa_field  = cf;
		fa_host   = fh;
		fa_inline = inline;
		fa_pos    = p;
	}

	(* Creates the `tfield_access` corresponding to this field access, using the provided field. *)
	let apply_fa cf = function
		| FHStatic c -> FStatic(c,cf)
		| FHInstance(c,tl) -> FInstance(c,tl,cf)
		| FHAbstract(a,tl,c) -> FStatic(c,cf)
		| FHAnon -> FAnon cf

	(* Returns the mapping function to apply type parameters. *)
	let get_map_function fa = match fa.fa_host with
		| FHStatic _ | FHAnon -> (fun t -> t)
		| FHInstance(c,tl) -> TClass.get_map_function c tl
		| FHAbstract(a,tl,_) -> apply_params a.a_params tl

	(* Converts the field access to a `TField` node, using the provided `mode`. *)
	let get_field_expr fa mode =
		let cf = fa.fa_field in
		let t = match mode with
			| FCall -> cf.cf_type
			| FGet | FRead | FWrite -> Type.field_type cf
		in
		let fa',t = match fa.fa_host with
			| FHStatic c ->
				FStatic(c,cf),t
			| FHInstance(c,tl) ->
				let fa = match cf.cf_kind with
				| Method _ when mode = FRead ->
					FClosure(Some(c,tl),cf)
				| _ ->
					FInstance(c,tl,cf)
				in
				let t = TClass.get_map_function c tl t in
				fa,t
			| FHAbstract(a,tl,c) ->
				FStatic(c,cf),apply_params a.a_params tl t
			| FHAnon ->
				let fa = match cf.cf_kind with
				| Method _ when mode = FRead ->
					FClosure(None,cf)
				| _ ->
					FAnon cf
				in
				fa,t
		in
		mk (TField(fa.fa_on,fa')) t fa.fa_pos

	(* Resolves the accessor on the field access, using the provided `mode`. *)
	let resolve_accessor fa mode = match fa.fa_field.cf_kind with
		| Var v ->
			begin match (match mode with MSet _ -> v.v_write | _ -> v.v_read) with
				| AccCall ->
					let name = (match mode with MSet _ -> "set_" | _ -> "get_") ^ fa.fa_field.cf_name in
					let forward cf_acc new_host =
						create fa.fa_on cf_acc new_host fa.fa_inline fa.fa_pos
					in
					begin match fa.fa_host with
					| FHStatic c ->
						begin try
							AccessorFound (forward (PMap.find name c.cl_statics) fa.fa_host)
						with Not_found ->
							(* TODO: Check if this is correct, there's a case in hxcpp's VirtualArray *)
							AccessorAnon
						end
					| FHInstance(c,tl) ->
						begin try
							(* Accessors can be overridden, so we have to check the actual type. *)
							let c,tl = match follow fa.fa_on.etype with
								| TInst(c,tl) -> c,tl
								| _ -> c,tl
							in
							let (c2,_,cf_acc) = raw_class_field (fun f -> f.cf_type) c tl name in
							let new_host = match c2 with
								| None -> FHAnon
								| Some(c,tl) -> FHInstance(c,tl)
							in
							AccessorFound (forward cf_acc new_host)
						with Not_found ->
							AccessorAnon
						end
					| FHAbstract(a,tl,c) ->
						begin try
							AccessorFound (forward (PMap.find name c.cl_statics) fa.fa_host)
						with Not_found ->
							AccessorAnon
						end
					| FHAnon ->
						AccessorAnon
					end
				| _ ->
					AccessorInvalid
			end
		| _ ->
			AccessorInvalid

	let get_constructor_access c params p =
		match c.cl_kind with
		| KAbstractImpl a ->
			let cf = (try PMap.find "_new" c.cl_statics with Not_found -> raise_error (No_constructor (TAbstractDecl a)) p) in
			create (Builder.make_static_this c p) cf (FHAbstract(a,params,c)) false p
		| _ ->
			let cf = (try Type.get_constructor c with Not_found -> raise_error (No_constructor (TClassDecl c)) p) in
			create (Builder.make_static_this c p) cf (FHInstance(c,params)) false p
end

let make_static_extension_access c cf e_this inline p =
	let e_static = Texpr.Builder.make_static_this c p in
	{
		se_this = e_this;
		se_access = FieldAccess.create e_static cf (FHStatic c) inline p
	}

let make_abstract_static_extension_access a tl c cf e_this inline p =
	let e_static = Texpr.Builder.make_static_this c p in
	{
		se_this = e_this;
		se_access = FieldAccess.create e_static cf (FHAbstract(a,tl,c)) inline p
	}
