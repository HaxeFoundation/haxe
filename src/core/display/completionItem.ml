open Globals
open Ast
open Type
open Genjson

module CompletionModuleKind = struct
	type t =
		| Class
		| Interface
		| Enum
		| Abstract
		| EnumAbstract
		| TypeAlias
		| Struct
		| TypeParameter

	let to_int = function
		| Class -> 0
		| Interface -> 1
		| Enum -> 2
		| Abstract -> 3
		| EnumAbstract -> 4
		| TypeAlias -> 5
		| Struct -> 6
		| TypeParameter -> 7
end

module ImportStatus = struct
	type t =
		| Imported
		| Unimported
		| Shadowed

	let to_int = function
		| Imported -> 0
		| Unimported -> 1
		| Shadowed -> 2
end

module CompletionModuleType = struct
	open CompletionModuleKind

	type not_bool =
		| Yes
		| No
		| Maybe

	type module_type_source =
		| Syntax of type_def (* TODO: do we really want to keep this? *)
		| Typed of module_type

	type t = {
		pack : string list;
		name : string;
		module_name : string;
		pos : pos;
		is_private : bool;
		params : Ast.type_param list;
		meta: metadata;
		doc : documentation;
		is_extern : bool;
		kind : CompletionModuleKind.t;
		has_constructor : not_bool;
		source : module_type_source;
	}

	let of_type_decl pack module_name (td,p) = match td with
		| EClass d ->
			let ctor = if (List.exists (fun cff -> fst cff.cff_name = "new") d.d_data) then Yes
				else if (List.exists (function HExtends _ -> true | _ -> false) d.d_flags) then Maybe
				else No
			in
			{
				pack = pack;
				name = fst d.d_name;
				module_name = module_name;
				pos = p;
				is_private = List.mem HPrivate d.d_flags;
				params = d.d_params;
				meta = d.d_meta;
				doc = d.d_doc;
				is_extern = List.mem HExtern d.d_flags;
				kind = if List.mem HInterface d.d_flags then Interface else Class;
				has_constructor = ctor;
				source = Syntax td;
			}
		| EEnum d -> {
				pack = pack;
				name = fst d.d_name;
				module_name = module_name;
				pos = p;
				is_private = List.mem EPrivate d.d_flags;
				params = d.d_params;
				meta = d.d_meta;
				doc = d.d_doc;
				is_extern = List.mem EExtern d.d_flags;
				kind = Enum;
				has_constructor = No;
				source = Syntax td;
			}
		| ETypedef d ->
			let kind = match fst d.d_data with CTAnonymous _ -> Struct | _ -> TypeAlias in
			{
				pack = pack;
				name = fst d.d_name;
				module_name = module_name;
				pos = p;
				is_private = List.mem EPrivate d.d_flags;
				params = d.d_params;
				meta = d.d_meta;
				doc = d.d_doc;
				is_extern = List.mem EExtern d.d_flags;
				kind = kind;
				has_constructor = if kind = Struct then No else Maybe;
				source = Syntax td;
			}
		| EAbstract d -> {
				pack = pack;
				name = fst d.d_name;
				module_name = module_name;
				pos = p;
				is_private = List.mem AbPrivate d.d_flags;
				params = d.d_params;
				meta = d.d_meta;
				doc = d.d_doc;
				is_extern = List.mem AbExtern d.d_flags;
				kind = if Meta.has Meta.Enum d.d_meta then EnumAbstract else Abstract;
				has_constructor = if (List.exists (fun cff -> fst cff.cff_name = "new") d.d_data) then Yes else No;
				source = Syntax td;
			}
		| EImport _ | EUsing _ ->
			raise Exit

	let of_module_type mt =
		let has_ctor a = match a.a_impl with
			| None -> false
			| Some c -> PMap.mem "_new" c.cl_statics
		in
		let is_extern,kind,has_ctor = match mt with
			| TClassDecl c ->
				c.cl_extern,(if c.cl_interface then Interface else Class),has_constructor c
			| TEnumDecl en ->
				en.e_extern,Enum,false
			| TTypeDecl td ->
				let kind,has_ctor = match follow td.t_type with
					| TAnon _ -> Struct,false
					| TInst(c,_) -> TypeAlias,has_constructor c
					| TAbstract(a,_) -> TypeAlias,has_ctor a
					| _ -> TypeAlias,false
				in
				false,kind,has_ctor
			| TAbstractDecl a ->
				false,(if Meta.has Meta.Enum a.a_meta then EnumAbstract else Abstract),has_ctor a
		in
		let infos = t_infos mt in
		let convert_type_param (s,t) = match follow t with
			| TInst(c,_) -> {
				tp_name = s,null_pos;
				tp_params = [];
				tp_constraints = None; (* TODO? *)
				tp_meta = c.cl_meta
			}
			| _ ->
				assert false
		in
		{
			pack = fst infos.mt_path;
			name = snd infos.mt_path;
			module_name = snd infos.mt_module.m_path;
			pos = infos.mt_pos;
			is_private = infos.mt_private;
			params = List.map convert_type_param infos.mt_params;
			meta = infos.mt_meta;
			doc = infos.mt_doc;
			is_extern = is_extern;
			kind = kind;
			has_constructor = if has_ctor then Yes else No;
			source = Typed mt;
		}

	let get_path cm = (cm.pack,cm.name)

	let to_json ctx cm is =
		let fields =
			("path",jobject [
				("pack",jlist jstring cm.pack);
				("moduleName",jstring cm.module_name);
				("typeName",jstring cm.name);
				("importStatus",jint (ImportStatus.to_int is));
			]) ::
			("kind",jint (to_int cm.kind)) ::
			(match ctx.generation_mode with
			| GMFull | GMWithoutDoc ->
				("pos",generate_pos ctx cm.pos) ::
				("params",jlist (generate_ast_type_param ctx) cm.params) ::
				("meta",generate_metadata ctx cm.meta) ::
				("isExtern",jbool cm.is_extern) ::
				(if ctx.generation_mode = GMFull then ["doc",jopt jstring cm.doc] else [])
			| GMMinimum ->
				[]
			)
		in
		jobject fields
end

module ClassFieldOrigin = struct
	type t =
		| Self of module_type
		| StaticImport of module_type
		| Parent of module_type
		| StaticExtension of module_type
		| AnonymousStructure of tanon
		| BuiltIn
		| Unknown

	let to_json ctx cfo =
		let i,args = match cfo with
		| Self mt -> 0,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| StaticImport mt -> 1,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| Parent mt -> 2,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| StaticExtension mt -> 3,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| AnonymousStructure an -> 4,if ctx.generation_mode = GMMinimum then None else Some (generate_anon ctx an)
		| BuiltIn -> 5,None
		| Unknown -> 6,None
		in
		jobject (
			("kind",jint i) :: (match args with None -> [] | Some arg -> ["args",arg])
		)
end

module CompletionClassField = struct
	type t = {
		field : tclass_field;
		scope : class_field_scope;
		origin : ClassFieldOrigin.t;
		is_qualified : bool;
	}

	let make cf scope origin is_qualified = {
		field = cf;
		scope = scope;
		origin = origin;
		is_qualified = is_qualified;
	}
end

module CompletionEnumField = struct
	type t = {
		efield : tenum_field;
		eorigin : ClassFieldOrigin.t;
		eis_qualified : bool;
	}

	let make ef origin is_qualified = {
		efield = ef;
		eorigin = origin;
		eis_qualified = is_qualified;
	}
end

module PackageContentKind = struct
	type t =
		| PCKModule
		| PCKPackage

	let to_int = function
		| PCKModule -> 0
		| PCKPackage -> 1
end

module CompletionType = struct

	type ct_path_with_params = {
		ct_pack : string list;
		ct_type_name : string;
		ct_module_name : string;
		ct_params : t list;
		ct_import_status : ImportStatus.t;
	}

	and ct_function_argument = {
		ct_name : string;
		ct_optional : bool;
		ct_type : t;
		ct_value : Ast.expr option;
	}

	and ct_function = {
		ct_args : ct_function_argument list;
		ct_return : t;
	}

	and ct_anonymous_field = {
		ctf_field : tclass_field;
		ctf_type : t;
	}

	and ct_anonymous = {
		ct_fields : ct_anonymous_field list;
		ct_status : anon_status;
	}

	and t =
		| CTMono
		| CTInst of ct_path_with_params
		| CTEnum of ct_path_with_params
		| CTTypedef of ct_path_with_params
		| CTAbstract of ct_path_with_params
		| CTFunction of ct_function
		| CTAnonymous of ct_anonymous
		| CTDynamic of t option

	let rec generate_path_with_params ctx pwp = jobject [
		"path",jobject [
			"pack",jlist jstring pwp.ct_pack;
			"moduleName",jstring pwp.ct_module_name;
			"typeName",jstring pwp.ct_type_name;
			"importStatus",jint (ImportStatus.to_int pwp.ct_import_status);
		];
		"params",jlist (generate_type ctx) pwp.ct_params;
	]

	and generate_function_argument ctx cfa = jobject [
		"name",jstring cfa.ct_name;
		"opt",jbool cfa.ct_optional;
		"t",generate_type ctx cfa.ct_type;
		"value",jopt (fun e -> jobject [
			"string",jstring (Ast.s_expr e);
		]) cfa.ct_value;
	]

	and generate_function' ctx ctf = [
		"args",jlist (generate_function_argument ctx) ctf.ct_args;
		"ret",generate_type ctx ctf.ct_return;
	]

	and generate_function ctx ctf = jobject (generate_function' ctx ctf)

	and generate_anon_field ctx af =
		let fields = generate_class_field' ctx CFSMember af.ctf_field in
		let fields = List.filter (fun (n,_) -> n <> "type") fields in
		let fields = ("type",generate_type ctx af.ctf_type) :: fields in
		jobject fields

	and generate_anon ctx cta = jobject [
		"status",generate_anon_status ctx cta.ct_status;
		"fields",jlist (generate_anon_field ctx) cta.ct_fields;
	]
	and generate_type ctx ct =
		let name,args = match ct with
			| CTMono -> "TMono",None
			| CTInst pwp -> "TInst",Some (generate_path_with_params ctx pwp)
			| CTEnum pwp -> "TEnum",Some (generate_path_with_params ctx pwp)
			| CTTypedef pwp -> "TType",Some (generate_path_with_params ctx pwp)
			| CTAbstract pwp -> "TAbstract",Some (generate_path_with_params ctx pwp)
			| CTFunction ctf -> "TFun",Some (generate_function ctx ctf)
			| CTAnonymous cta -> "TAnonymous",Some (generate_anon ctx cta)
			| CTDynamic cto -> "TDynamic",Option.map (generate_type ctx) cto;
		in
		generate_adt ctx None name args

	let to_json ctx ct =
		generate_type ctx ct
end

open CompletionModuleType
open CompletionClassField
open CompletionEnumField

type t_kind =
	| ITLocal of tvar
	| ITClassField of CompletionClassField.t
	| ITEnumField of CompletionEnumField.t
	| ITEnumAbstractField of tabstract * CompletionClassField.t
	| ITType of CompletionModuleType.t * ImportStatus.t
	| ITPackage of path * (string * PackageContentKind.t) list
	| ITModule of path
	| ITLiteral of string
	| ITTimer of string * string
	| ITMetadata of string * documentation
	| ITKeyword of keyword
	| ITAnonymous of tanon
	| ITExpression of texpr
	| ITTypeParameter of tclass

type t = {
	ci_kind : t_kind;
	ci_type : (Type.t * CompletionType.t) option;
}

let make kind t = {
	ci_kind = kind;
	ci_type = t;
}

let make_ci_local v t = make (ITLocal v) (Some t)
let make_ci_class_field ccf t = make (ITClassField ccf) (Some t)
let make_ci_enum_abstract_field a ccf t = make (ITEnumAbstractField(a,ccf)) (Some t)
let make_ci_enum_field cef t = make (ITEnumField cef) (Some t)
let make_ci_type mt import_status t = make (ITType(mt,import_status)) t
let make_ci_package path l = make (ITPackage(path,l)) None
let make_ci_module path = make (ITModule path) None
let make_ci_literal lit t = make (ITLiteral lit) (Some t)
let make_ci_timer name value = make (ITTimer(name,value)) None
let make_ci_metadata s doc = make (ITMetadata(s,doc)) None
let make_ci_keyword kwd = make (ITKeyword kwd) None
let make_ci_anon an t = make (ITAnonymous an) (Some t)
let make_ci_expr e t = make (ITExpression e) (Some t)
let make_ci_type_param c t = make (ITTypeParameter c) (Some t)

let get_index item = match item.ci_kind with
	| ITLocal _ -> 0
	| ITClassField _ -> 1
	| ITEnumField _ -> 2
	| ITEnumAbstractField _ -> 3
	| ITType _ -> 4
	| ITPackage _ -> 5
	| ITModule _ -> 6
	| ITLiteral _ -> 7
	| ITTimer _ -> 8
	| ITMetadata _ -> 9
	| ITKeyword _ -> 10
	| ITAnonymous _ -> 11
	| ITExpression _ -> 12
	| ITTypeParameter _ -> 13

let get_sort_index item p = match item.ci_kind with
	| ITLocal v ->
		let i = p.pmin - v.v_pos.pmin in
		let i = if i < 0 then 0 else i in
		0,(Printf.sprintf "%05i" i)
	| ITEnumField ef ->
		10,(Printf.sprintf "%04i" ef.efield.ef_index)
	| ITEnumAbstractField(_,ccf) ->
		11,ccf.field.cf_name
	| ITClassField ccf ->
		let open ClassFieldOrigin in
		let i = match ccf.origin,ccf.scope with
			| Self _,(CFSMember | CFSConstructor) -> 20
			| Parent _,(CFSMember | CFSConstructor) -> 21
			| StaticExtension _,_ -> 22
			| Self _,CFSStatic -> 23
			| StaticImport _,_ -> 24
			| _ -> 25
		in
		i,ccf.field.cf_name
	| ITTypeParameter c ->
		30,snd c.cl_path
	| ITType(cmt,is) ->
		let open ImportStatus in
		let i = match is with
			| Imported -> 31
			| Unimported -> 32
			| Shadowed -> 33
		in
		i,(s_type_path (cmt.pack,cmt.name))
	| ITPackage(path,_) ->
		40,s_type_path path
	| ITModule path ->
		40,s_type_path path
	| ITLiteral name ->
		50,name
	| ITKeyword name ->
		60,s_keyword name
	| ITAnonymous _
	| ITExpression _
	| ITTimer _
	| ITMetadata _ ->
		500,""

let legacy_sort item = match item.ci_kind with
	| ITClassField(cf) | ITEnumAbstractField(_,cf) ->
		begin match cf.field.cf_kind with
		| Var _ -> 0,cf.field.cf_name
		| Method _ -> 1,cf.field.cf_name
		end
	| ITEnumField ef ->
		let ef = ef.efield in
		begin match follow ef.ef_type with
		| TFun _ -> 1,ef.ef_name
		| _ -> 0,ef.ef_name
		end
	| ITType(cm,_) -> 2,cm.name
	| ITModule path -> 3,snd path
	| ITPackage(path,_) -> 4,snd path
	| ITMetadata(s,_) -> 5,s
	| ITTimer(s,_) -> 6,s
	| ITLocal v -> 7,v.v_name
	| ITLiteral s -> 9,s
	| ITKeyword kwd -> 10,s_keyword kwd
	| ITAnonymous _ -> 11,""
	| ITExpression _ -> 12,""
	| ITTypeParameter _ -> 13,""

let get_name item = match item.ci_kind with
	| ITLocal v -> v.v_name
	| ITClassField(cf) | ITEnumAbstractField(_,cf) -> cf.field.cf_name
	| ITEnumField ef -> ef.efield.ef_name
	| ITType(cm,_) -> cm.name
	| ITPackage(path,_) -> snd path
	| ITModule path -> snd path
	| ITLiteral s -> s
	| ITTimer(s,_) -> s
	| ITMetadata(s,_) -> s
	| ITKeyword kwd -> s_keyword kwd
	| ITAnonymous _ -> ""
	| ITExpression _ -> ""
	| ITTypeParameter c -> snd c.cl_path

let get_type item = item.ci_type

let get_documentation item = match item.ci_kind with
	| ITClassField cf | ITEnumAbstractField(_,cf) -> cf.field.cf_doc
	| ITEnumField ef -> ef.efield.ef_doc
	| ITType(mt,_) -> mt.doc
	| _ -> None

let to_json ctx item =
	let open ClassFieldOrigin in
	let kind,data = match item.ci_kind with
		| ITLocal v -> "Local",generate_tvar ctx v
		| ITClassField(cf) | ITEnumAbstractField(_,cf) ->
			let name = match item.ci_kind with
				| ITClassField _ -> "ClassField"
				| _ ->  "EnumAbstractField"
			in
			let qualifier = match cf.scope,cf.origin with
				| CFSStatic,(Self mt | Parent mt | StaticExtension mt | StaticImport mt) ->
					let infos = t_infos mt in
					jstring (s_type_path (Path.full_dot_path (fst infos.mt_module.m_path) (snd infos.mt_module.m_path) (snd infos.mt_path)))
				| CFSMember,Self _ ->
					jstring "this"
				| CFSMember,Parent _->
					jstring "super"
				| _ ->
					jnull
			in
			name,jobject [
			"field",generate_class_field ctx cf.scope cf.field;
			"origin",ClassFieldOrigin.to_json ctx cf.origin;
			"resolution",jobject [
				"isQualified",jbool cf.is_qualified;
				"qualifier",qualifier;
			]
		]
		| ITEnumField ef ->
			let qualifier = match ef.eorigin with
				| Self mt | StaticImport mt ->
					let infos = t_infos mt in
					jstring (s_type_path (Path.full_dot_path (fst infos.mt_module.m_path) (snd infos.mt_module.m_path) (snd infos.mt_path)))
				| _ ->
					jnull
			in
			"EnumField",jobject [
				"field",generate_enum_field ctx ef.efield;
				"origin",ClassFieldOrigin.to_json ctx ef.eorigin;
				"resolution",jobject [
					"isQualified",jbool ef.eis_qualified;
					"qualifier",qualifier;
				]
			]
		| ITType(kind,is) -> "Type",CompletionModuleType.to_json ctx kind is
		| ITPackage(path,contents) ->
			let generate_package_content (name,kind) = jobject [
				"name",jstring name;
				"kind",jint (PackageContentKind.to_int kind);
			] in
			"Package",jobject [
				"path",generate_package_path (fst path @ [snd path]);
				"contents",jlist generate_package_content contents;
			]
		| ITModule path -> "Module",jobject [
			"path",generate_module_path path;
		]
		| ITLiteral s -> "Literal",jobject [
			"name",jstring s;
		]
		| ITTimer(s,value) -> "Timer",jobject [
			"name",jstring s;
			"value",jstring value;
		]
		| ITMetadata(s,doc) -> "Metadata",jobject [
			"name",jstring s;
			"doc",jopt jstring doc;
		]
		| ITKeyword kwd ->"Keyword",jobject [
			"name",jstring (s_keyword kwd)
		]
		| ITAnonymous an -> "AnonymousStructure",generate_anon ctx an
		| ITExpression e -> "Expression",generate_texpr ctx e
		| ITTypeParameter c ->
			begin match c.cl_kind with
			| KTypeParameter tl ->
				"TypeParameter",jobject [
					"name",jstring (snd c.cl_path);
					"meta",generate_metadata ctx c.cl_meta;
					"constraints",jlist (generate_type ctx) tl;
				]
			| _ -> assert false
			end
	in
	jobject (
		("kind",jstring kind) ::
		("args",data) ::
		(match item.ci_type with None -> [] | Some t -> ["type",CompletionType.to_json ctx (snd t)])
	)