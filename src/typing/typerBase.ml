open Globals
open Ast
open Type
open Typecore
open Error

type access_kind =
	(* Access is not possible or allowed. *)
	| AKNo of access_kind * pos
	(* Access on arbitrary expression. *)
	| AKExpr of texpr
	(* Safe navigation access chain *)
	| AKSafeNav of safe_nav_access
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

and safe_nav_access = {
	(* position of the safe navigation chain start (the initial ?.field expression) *)
	sn_pos : pos;
	(* starting value to be checked for null *)
	sn_base : texpr;
	(* temp var declaration to store complex base expression *)
	sn_temp_var : texpr option;
	(* safe navigation access to be done if the base value is not null *)
	sn_access : access_kind;
}

type object_decl_kind =
	| ODKWithStructure of tanon
	| ODKWithClass of tclass * tparams
	| ODKPlain
	| ODKFailed

let type_call_target_ref : (typer -> expr -> expr list -> WithType.t -> pos option -> access_kind) ref = ref (fun _ _ _ _ -> die "" __LOC__)
let type_access_ref : (typer -> expr_def -> pos -> access_mode -> WithType.t -> access_kind) ref = ref (fun _ _ _ _ _ -> assert false)

class value_reference (ctx : typer) =

object(self)
	val vars = DynArray.create ()

	method get_vars = DynArray.to_list vars

	method as_var name e =
		let v = alloc_var VGenerated name e.etype e.epos in
		DynArray.add vars (v,e);
		mk (TLocal v) v.v_type v.v_pos

	method private get_expr_aux depth name e =
		let rec loop depth name e = match (Texpr.skip e).eexpr with
			| TLocal _ | TTypeExpr _ | TConst _ ->
				e
			| TField(ef,fa) when depth = 0 ->
				let ef = loop (depth + 1) "fh" ef in
				{e with eexpr = TField(ef,fa)}
			| TArray(e1,e2) when depth = 0 ->
				let e1 = loop (depth + 1) "base" e1 in
				let e2 = loop (depth + 1) "index" e2 in
				{e with eexpr = TArray(e1,e2)}
			| _ ->
				self#as_var name e
		in
		loop depth name e

	method get_expr name e =
		self#get_expr_aux 0 name e

	method get_expr_part name e =
		self#get_expr_aux 1 name e

	method to_texpr e =
		begin match self#get_vars with
		| [] ->
			e
		| vl ->
			let el = List.map (fun (v,e) ->
				mk (TVar(v,Some e)) ctx.t.tvoid v.v_pos
			) vl in
			let e = mk (TBlock (el @ [e])) e.etype e.epos in
			{e with eexpr = TMeta((Meta.MergeBlock,[],null_pos),e)}
		end

	method to_texpr_el el e =
		let vl = self#get_vars in
		let el_vars = List.map (fun (v,e) ->
			mk (TVar(v,Some e)) ctx.t.tvoid v.v_pos
		) vl in
		let e = mk (TBlock (el_vars @ el @ [e])) e.etype e.epos in
		{e with eexpr = TMeta((Meta.MergeBlock,[],null_pos),e)}
end

let is_lower_ident s p =
	try Ast.is_lower_ident s
	with Invalid_argument msg -> typing_error msg p

let get_this ctx p =
	match ctx.curfun with
	| FunStatic ->
		typing_error "Cannot access this from a static function" p
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
		let v = (try PMap.find "this" ctx.locals with Not_found -> typing_error "Cannot reference this abstract here" p) in
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
			else typing_error "Invalid module type" p
		in
		type_module_type ctx mt None p
	| TClassDecl c ->
		let t_tmp = class_module_type c in
		mk (TTypeExpr (TClassDecl c)) (TType (t_tmp,[])) p
	| TEnumDecl e ->
		let types = (match tparams with None -> Monomorph.spawn_constrained_monos (fun t -> t) e.e_params | Some l -> l) in
		mk (TTypeExpr (TEnumDecl e)) (TType (e.e_type,types)) p
	| TTypeDecl s ->
		let t = apply_typedef s (List.map (fun _ -> spawn_monomorph ctx p) s.t_params) in
		DeprecationCheck.check_typedef ctx.com s p;
		(match follow t with
		| TEnum (e,params) ->
			type_module_type ctx (TEnumDecl e) (Some params) p
		| TInst (c,params) ->
			type_module_type ctx (TClassDecl c) (Some params) p
		| TAbstract (a,params) ->
			type_module_type ctx (TAbstractDecl a) (Some params) p
		| _ ->
			typing_error (s_type_path s.t_path ^ " is not a value") p)
	| TAbstractDecl { a_impl = Some c } ->
		type_module_type ctx (TClassDecl c) tparams p
	| TAbstractDecl a ->
		if not (Meta.has Meta.RuntimeValue a.a_meta) then typing_error (s_type_path a.a_path ^ " is not a value") p;
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
		"fa_inline",string_of_bool fa.fa_inline;
		"fa_pos",(Printf.sprintf "%s(%i-%i)" fa.fa_pos.pfile fa.fa_pos.pmin fa.fa_pos.pmax);
	]

let s_static_extension_access sea =
	Printer.s_record_fields "" [
		"se_this",s_expr_pretty true "" false (s_type (print_context())) sea.se_this;
		"se_access",s_field_access "\t" sea.se_access
	]

let rec s_access_kind acc =
	let st = s_type (print_context()) in
	let se = s_expr_pretty true "" false st in
	match acc with
	| AKNo(acc,_) -> "AKNo " ^ (s_access_kind acc)
	| AKExpr e -> "AKExpr " ^ (se e)
	| AKSafeNav sn -> Printf.sprintf  "AKSafeNav(%s)" (s_safe_nav_access sn)
	| AKField fa -> Printf.sprintf "AKField(%s)" (s_field_access "" fa)
	| AKAccessor fa -> Printf.sprintf "AKAccessor(%s)" (s_field_access "" fa)
	| AKUsingField sea -> Printf.sprintf "AKUsingField(%s)" (s_static_extension_access sea)
	| AKUsingAccessor sea -> Printf.sprintf "AKUsingAccessor(%s)" (s_static_extension_access sea)
	| AKAccess(a,tl,c,e1,e2) -> Printf.sprintf "AKAccess(%s, [%s], %s, %s, %s)" (s_type_path a.a_path) (String.concat ", " (List.map st tl)) (s_type_path c.cl_path) (se e1) (se e2)
	| AKResolve(_) -> ""

and s_safe_nav_access sn =
	let st = s_type (print_context()) in
	let se = s_expr_pretty true "" false st in
	Printer.s_record_fields "" [
		"sn_base",se sn.sn_base;
		"sn_temp_var",Option.map_default (fun e -> "Some " ^ (se e)) "None" sn.sn_temp_var;
		"sn_access",s_access_kind sn.sn_access
	]

let s_dot_path_part part =
	Printer.s_record_fields "" [
		"name",part.name;
		"case",(match part.case with PUppercase -> "PUppercase" | PLowercase -> "PLowercase");
		"pos",(Printf.sprintf "%s(%i-%i)" part.pos.pfile part.pos.pmin part.pos.pmax);
	]

let get_constructible_constraint ctx tl p =
	let extract_function t = match follow t with
		| TFun(tl,tr) -> tl,tr
		| _ -> typing_error "Constructible type parameter should be function" p
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

let get_abstract_froms ctx a pl =
	let l = List.map (apply_params a.a_params pl) a.a_from in
	List.fold_left (fun acc (t,f) ->
		(* We never want to use the @:from we're currently in because that's recursive (see #10604) *)
		if f == ctx.curfield then
			acc
		else match follow (Type.field_type f) with
		| TFun ([_,_,v],t) ->
			(try
				ignore(type_eq EqStrict t (TAbstract(a,List.map duplicate pl))); (* unify fields monomorphs *)
				v :: acc
			with Unify_error _ ->
				acc)
		| _ ->
			acc
	) l a.a_from_field
