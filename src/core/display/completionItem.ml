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
			}
		| EImport _ | EUsing _ ->
			raise Exit

	let of_module_type mt =
		let is_extern,kind,has_ctor = match mt with
			| TClassDecl c ->
				c.cl_extern,(if c.cl_interface then Interface else Class),has_constructor c
			| TEnumDecl en ->
				en.e_extern,Enum,false
			| TTypeDecl td ->
				false,(match follow td.t_type with TAnon _ -> Struct | _ -> TypeAlias),false
			| TAbstractDecl a ->
				let has_ctor = match a.a_impl with
					| None -> false
					| Some c -> PMap.mem "_new" c.cl_statics
				in
				false,(if Meta.has Meta.Enum a.a_meta then EnumAbstract else Abstract),has_ctor
		in
		let infos = t_infos mt in
		let convert_type_param (s,t) = match follow t with
			| TInst(c,_) -> {
				tp_name = s,null_pos;
				tp_params = [];
				tp_constraints = []; (* TODO? *)
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
		}

	let get_path cm = (cm.pack,cm.name)

	let to_json ctx cm is =
		let fields =
			("pack",jlist jstring cm.pack) ::
			("name",jstring cm.name) ::
			("moduleName",jstring cm.module_name) ::
			("isPrivate",jbool cm.is_private) ::
			("kind",jint (to_int cm.kind)) ::
			("importStatus",jint (ImportStatus.to_int is)) ::
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

	let to_json ctx cfo =
		let i,args = match cfo with
		| Self mt -> 0,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| StaticImport mt -> 1,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| Parent mt -> 2,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| StaticExtension mt -> 3,if ctx.generation_mode = GMMinimum then None else Some (generate_module_type ctx mt)
		| AnonymousStructure an -> 4,if ctx.generation_mode = GMMinimum then None else Some (generate_anon ctx an)
		| BuiltIn -> 5,None
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

open CompletionModuleType
open CompletionClassField
open CompletionEnumField

type t =
	| ITLocal of tvar
	| ITClassField of CompletionClassField.t
	| ITEnumField of CompletionEnumField.t
	| ITEnumAbstractField of tabstract * CompletionClassField.t
	| ITType of CompletionModuleType.t * ImportStatus.t
	| ITPackage of path * (string * PackageContentKind.t) list
	| ITModule of string
	| ITLiteral of string * Type.t
	| ITTimer of string * string
	| ITMetadata of string * documentation
	| ITKeyword of keyword
	| ITAnonymous of tanon
	| ITExpression of texpr

let get_index = function
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

let get_sort_index = function
	| ITLocal _ -> 0
	| ITClassField _ -> 0
	| ITEnumField ef -> ef.efield.ef_index
	| ITEnumAbstractField _ -> 0
	| ITType _ -> 0
	| ITPackage _ -> 0
	| ITModule _ -> 0
	| ITLiteral _ -> 0
	| ITTimer _ -> 0
	| ITMetadata _ -> 0
	| ITKeyword _ -> 0
	| ITAnonymous _ -> 0
	| ITExpression _ -> 0

let legacy_sort = function
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
	| ITModule s -> 3,s
	| ITPackage(path,_) -> 4,snd path
	| ITMetadata(s,_) -> 5,s
	| ITTimer(s,_) -> 6,s
	| ITLocal v -> 7,v.v_name
	| ITLiteral(s,_) -> 9,s
	| ITKeyword kwd -> 10,s_keyword kwd
	| ITAnonymous _ -> 11,""
	| ITExpression _ -> 12,""

let get_name = function
	| ITLocal v -> v.v_name
	| ITClassField(cf) | ITEnumAbstractField(_,cf) -> cf.field.cf_name
	| ITEnumField ef -> ef.efield.ef_name
	| ITType(cm,_) -> cm.name
	| ITPackage(path,_) -> snd path
	| ITModule s -> s
	| ITLiteral(s,_) -> s
	| ITTimer(s,_) -> s
	| ITMetadata(s,_) -> s
	| ITKeyword kwd -> s_keyword kwd
	| ITAnonymous _ -> ""
	| ITExpression _ -> ""

let get_type = function
	| ITLocal v -> v.v_type
	| ITClassField(cf) | ITEnumAbstractField(_,cf) -> cf.field.cf_type
	| ITEnumField ef -> ef.efield.ef_type
	| ITType(_,_) -> t_dynamic (* TODO: might want a type here, not sure *)
	| ITPackage _ -> t_dynamic
	| ITModule _ -> t_dynamic
	| ITLiteral(_,t) -> t
	| ITTimer(_,_) -> t_dynamic
	| ITMetadata(_,_) -> t_dynamic
	| ITKeyword _ -> t_dynamic
	| ITAnonymous an -> TAnon an
	| ITExpression e -> e.etype

let to_json ctx ck =
	let kind,data = match ck with
		| ITLocal v -> "Local",generate_tvar ctx v
		| ITClassField(cf) | ITEnumAbstractField(_,cf) ->
			let name = match ck with
				| ITClassField _ -> "ClassField"
				| _ ->  "EnumAbstractField"
			in
			name,jobject [
			"field",generate_class_field ctx cf.scope cf.field;
			"origin",ClassFieldOrigin.to_json ctx cf.origin;
			"resolution",jobject [
				"isQualified",jbool cf.is_qualified;
			]
		]
		| ITEnumField ef -> "EnumField",jobject [
			"field",generate_enum_field ctx ef.efield;
			"origin",ClassFieldOrigin.to_json ctx ef.eorigin;
			"resolution",jobject [
				"isQualified",jbool ef.eis_qualified;
			]
		]
		| ITType(kind,is) -> "Type",CompletionModuleType.to_json ctx kind is
		| ITPackage(path,contents) ->
			let generate_package_content (name,kind) = jobject [
				"name",jstring name;
				"kind",jint (PackageContentKind.to_int kind);
			] in
			"Package",jobject [
				"path",generate_path path;
				"contents",jlist generate_package_content contents;
			]
		| ITModule s -> "Module",jstring s
		| ITLiteral(s,t) -> "Literal",jobject [
			"name",jstring s;
			"type",generate_type ctx t;
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
	in
	generate_adt ctx None kind (Some data)