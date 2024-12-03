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

module MetaConfig = struct

	let as_bool e = match fst e with
		| EConst (Ident "true") -> true
		| EConst (Ident "false") -> false
		| _ -> raise (Invalid_argument "bool")

	let read_arg_config meta f l =
		List.iter (fun (meta',el,_) ->
			if meta' = meta then
				List.iter (fun e -> match fst e with
					| EConst (Ident s) ->
						f (s,((EConst (Ident "true"),null_pos)))
					| EBinop(OpAssign,(EConst (Ident s),_),e2) ->
						f (s, e2)
					| _ ->
						()
				) el
		) l
end

module AbstractFromConfig = struct
	type t = {
		mutable ignored_by_inference : bool;
	}

	let make () = {
		ignored_by_inference = false;
	}

	let update_config_from_meta config ml =
		MetaConfig.read_arg_config Meta.From (fun (s,e) -> match s with
			| "ignoredByInference" ->
				begin try
					config.ignored_by_inference <- MetaConfig.as_bool e
				with Invalid_argument _ ->
					()
				end
			| _ ->
				()
		) ml;
		config
end

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
	with Invalid_argument msg -> raise_typing_error msg p

let get_this ctx p =
	match ctx.e.curfun with
	| FunStatic ->
		raise_typing_error "Cannot access this from a static function" p
	| FunMemberClassLocal | FunMemberAbstractLocal ->
		let v = match ctx.f.vthis with
			| None ->
				let v = if ctx.e.curfun = FunMemberAbstractLocal then begin
					let v = PMap.find "this" ctx.f.locals in
					add_var_flag v VUsedByTyper;
					v
				end else
					add_local ctx VGenerated (Printf.sprintf "%sthis" gen_local_prefix) ctx.c.tthis p
				in
				ctx.f.vthis <- Some v;
				v
			| Some v ->
				ctx.f.locals <- PMap.add v.v_name v ctx.f.locals;
				v
		in
		mk (TLocal v) ctx.c.tthis p
	| FunMemberAbstract ->
		let v = (try PMap.find "this" ctx.f.locals with Not_found -> raise_typing_error "Cannot reference this abstract here" p) in
		mk (TLocal v) v.v_type p
	| FunConstructor | FunMember ->
		mk (TConst TThis) ctx.c.tthis p

let get_stored_typed_expr ctx id =
	let e = ctx.com.stored_typed_exprs#find id in
	Texpr.duplicate_tvars (fun e -> get_this ctx e.epos) e

let type_stored_expr ctx e1 =
	let id = match e1 with (EConst (Int (s, _)),_) -> int_of_string s | _ -> die "" __LOC__ in
	get_stored_typed_expr ctx id

let assign_to_this_is_allowed ctx =
	match ctx.c.curclass.cl_kind with
		| KAbstractImpl _ ->
			(match ctx.f.curfield.cf_kind with
				| Method MethInline -> true
				| Method _ when ctx.f.curfield.cf_name = "_new" -> true
				| _ -> false
			)
		| _ -> false

let type_module_type ctx t p =
	let rec loop t tparams =
		match t with
		| TClassDecl {cl_kind = KGenericBuild _} ->
			let info = InstanceBuilder.get_build_info ctx t p in
			let t = info.build_apply (match tparams with None -> [] | Some tl -> tl) in
			let mt = try
				module_type_of_type t
			with Exit ->
				if follow t == t_dynamic then Typeload.load_type_def ctx p (mk_type_path ([],"Dynamic"))
				else raise_typing_error "Invalid module type" p
			in
			loop mt None
		| TClassDecl c ->
			mk (TTypeExpr (TClassDecl c)) c.cl_type p
		| TEnumDecl e ->
			mk (TTypeExpr (TEnumDecl e)) e.e_type p
		| TTypeDecl s ->
			let t = apply_typedef s (List.map (fun _ -> spawn_monomorph ctx.e p) s.t_params) in
			DeprecationCheck.check_typedef (create_deprecation_context ctx) s p;
			(match follow t with
			| TEnum (e,params) ->
				loop (TEnumDecl e) (Some params)
			| TInst (c,params) ->
				loop (TClassDecl c) (Some params)
			| TAbstract (a,params) ->
				loop (TAbstractDecl a) (Some params)
			| _ ->
				raise_typing_error (s_type_path s.t_path ^ " is not a value") p)
		| TAbstractDecl { a_impl = Some c } ->
			loop (TClassDecl c) tparams
		| TAbstractDecl a ->
			if not (Meta.has Meta.RuntimeValue a.a_meta) then raise_typing_error (s_type_path a.a_path ^ " is not a value") p;
			let t_tmp = abstract_module_type a [] in
			mk (TTypeExpr (TAbstractDecl a)) (TType (t_tmp,[])) p
	in
	loop t None

let mk_module_type_access ctx t p =
	AKExpr (type_module_type ctx t p)

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
		| _ -> raise_typing_error "Constructible type parameter should be function" p
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
			| TInst({cl_kind = KTypeParameter ttp},_) ->
				begin match loop (get_constraints ttp) with
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

type from_kind =
	| FromType
	| FromField

let get_abstract_froms ctx a pl =
	let l = List.map (fun t -> FromType,apply_params a.a_params pl t) a.a_from in
	List.fold_left (fun acc (t,f) ->
		(* We never want to use the @:from we're currently in because that's recursive (see #10604) *)
		if f == ctx.f.curfield then
			acc
		else if (AbstractFromConfig.update_config_from_meta (AbstractFromConfig.make ()) f.cf_meta).ignored_by_inference then
			acc
		else match follow (Type.field_type f) with
		| TFun ([_,_,v],t) ->
			(try
				ignore(type_eq EqStrict t (TAbstract(a,List.map duplicate pl))); (* unify fields monomorphs *)
				(FromField,v) :: acc
			with Unify_error _ ->
				acc)
		| _ ->
			acc
	) l a.a_from_field

let safe_nav_branch ctx sn f_then =
	(* generate null-check branching for the safe navigation chain *)
	let eobj = sn.sn_base in
	let enull = Builder.make_null eobj.etype sn.sn_pos in
	let eneq = Builder.binop OpNotEq eobj enull ctx.t.tbool sn.sn_pos in
	let ethen = f_then () in
	let tnull = ctx.t.tnull ethen.etype in
	let ethen = if not (is_nullable ethen.etype) then
		mk (TCast(ethen,None)) tnull ethen.epos
	else
		ethen
	in
	let eelse = Builder.make_null tnull sn.sn_pos in
	let eif = mk (TIf(eneq,ethen,Some eelse)) tnull sn.sn_pos in
	(match sn.sn_temp_var with
	| None -> eif
	| Some evar -> { eif with eexpr = TBlock [evar; eif] })

let get_safe_nav_base ctx eobj =
	match (Texpr.skip eobj).eexpr with
	| TLocal _ | TTypeExpr _ | TConst _ ->
		eobj, None
	| _ ->
		let v = alloc_var VGenerated "tmp" eobj.etype eobj.epos in
		let temp_var = mk (TVar(v, Some eobj)) ctx.t.tvoid v.v_pos in
		let eobj = mk (TLocal v) v.v_type v.v_pos in
		eobj, Some temp_var