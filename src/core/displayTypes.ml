open Globals
open Path
open Ast
open Type
open Json
open Genjson

module SymbolKind = struct
	type t =
		| Class
		| Interface
		| Enum
		| TypeAlias
		| Abstract
		| Field
		| Property
		| Method
		| Constructor
		| Function
		| Variable
		| Struct
		| EnumAbstract
		| Operator
		| EnumMember
		| Constant

	let to_int = function
		| Class -> 1
		| Interface -> 2
		| Enum -> 3
		| TypeAlias -> 4
		| Abstract -> 5
		| Field -> 6
		| Property -> 7
		| Method -> 8
		| Constructor -> 9
		| Function -> 10
		| Variable -> 11
		| Struct -> 12
		| EnumAbstract -> 13
		| Operator -> 14
		| EnumMember -> 15
		| Constant -> 16
end

module SymbolInformation = struct
	type t = {
		name : string;
		kind : SymbolKind.t;
		pos : Globals.pos;
		container_name : string option;
		deprecated : bool;
	}

	let make name kind pos container_name deprecated = {
		name = name;
		kind = kind;
		pos = pos;
		container_name = container_name;
		deprecated = deprecated;
	}
end

module DiagnosticsSeverity = struct
	type t =
		| Error
		| Warning
		| Information
		| Hint

	let to_int = function
		| Error -> 1
		| Warning -> 2
		| Information -> 3
		| Hint -> 4
end

module DiagnosticsKind = struct
	type t =
		| DKUnusedImport
		| DKUnresolvedIdentifier
		| DKCompilerError
		| DKRemovableCode
		| DKParserError
		| DKDeprecationWarning
		| DKInactiveBlock

	let to_int = function
		| DKUnusedImport -> 0
		| DKUnresolvedIdentifier -> 1
		| DKCompilerError -> 2
		| DKRemovableCode -> 3
		| DKParserError -> 4
		| DKDeprecationWarning -> 5
		| DKInactiveBlock -> 6
end

module CompletionResultKind = struct
	type t =
		| CRField of CompletionItem.t * pos * Type.t option * (Type.t * Type.t) option
		| CRStructureField
		| CRToplevel of (CompletionItem.CompletionType.t * CompletionItem.CompletionType.t) option
		| CRMetadata
		| CRTypeHint
		| CRExtends
		| CRImplements
		| CRStructExtension of bool
		| CRImport
		| CRUsing
		| CRNew
		| CRPattern of (CompletionItem.CompletionType.t * CompletionItem.CompletionType.t) option * bool
		| CROverride
		| CRTypeRelation
		| CRTypeDecl

	let to_json ctx kind =
		let expected_type_fields t = match t with
			| None -> []
			| Some(ct1,ct2) -> [
					"expectedType",CompletionItem.CompletionType.to_json ctx ct1;
					"expectedTypeFollowed",CompletionItem.CompletionType.to_json ctx ct2;
				]
		in
		let i,args = match kind with
			| CRField(item,p,iterator,keyValueIterator) ->
				let t = CompletionItem.get_type item in
				let t = match t with
					| None ->
						None
					| Some (t,ct) ->
						try
							let mt = module_type_of_type t in
							let ctx = {ctx with generate_abstract_impl = true} in
							let make mt = generate_module_type ctx mt in
							let j_mt = make mt in
							let j_mt_followed = if t == follow t then jnull else make (module_type_of_type (follow t)) in
							Some (j_mt,j_mt_followed,CompletionItem.CompletionType.to_json ctx ct)
						with _ ->
							None
				in
				let fields =
					("item",CompletionItem.to_json ctx None item) ::
					("range",generate_pos_as_range p) ::
					("iterator", match iterator with
						| None -> jnull
						| Some t -> jobject ["type",generate_type ctx t]
					) ::
					("keyValueIterator", match keyValueIterator with
						| None -> jnull
						| Some (key,value) -> jobject [
							"key",generate_type ctx key;
							"value",generate_type ctx value
						]
					) ::
					(match t with
						| None -> []
						| Some (mt,mt_followed,ct) -> ["type",ct;"moduleType",mt;"moduleTypeFollowed",mt_followed]
					)
				in
				0,Some (jobject fields)
			| CRStructureField -> 1,None
			| CRToplevel t -> 2,Some (jobject (expected_type_fields t))
			| CRMetadata -> 3,None
			| CRTypeHint -> 4,None
			| CRExtends -> 5,None
			| CRImplements -> 6,None
			| CRStructExtension isIntersectionType -> 7,Some (jobject [
					"isIntersectionType",jbool isIntersectionType
				])
			| CRImport -> 8,None
			| CRUsing -> 9,None
			| CRNew -> 10,None
			| CRPattern (t,isOutermostPattern) ->
				let fields =
					("isOutermostPattern",jbool isOutermostPattern) ::
					(expected_type_fields t)
				in
				11,Some (jobject fields)
			| CROverride -> 12,None
			| CRTypeRelation -> 13,None
			| CRTypeDecl -> 14,None
		in
		jobject (
			("kind",jint i) :: (match args with None -> [] | Some arg -> ["args",arg])
		)

end

module DisplayMode = struct
	type t =
		| DMNone
		| DMDefault
		| DMUsage of bool (* true = also report definition *)
		| DMDefinition
		| DMTypeDefinition
		| DMImplementation
		| DMResolve of string
		| DMPackage
		| DMHover
		| DMModuleSymbols of string option
		| DMDiagnostics of string list
		| DMStatistics
		| DMSignature

	type error_policy =
		| EPIgnore
		| EPCollect
		| EPShow

	type display_file_policy =
		| DFPOnly
		| DFPAlso
		| DFPNo

	type settings = {
		dms_kind : t;
		dms_display : bool;
		dms_full_typing : bool;
		dms_force_macro_typing : bool;
		dms_error_policy : error_policy;
		dms_check_core_api : bool;
		dms_inline : bool;
		dms_display_file_policy : display_file_policy;
		dms_exit_during_typing : bool;
		dms_per_file : bool;
	}

	let default_display_settings = {
		dms_kind = DMDefault;
		dms_display = true;
		dms_full_typing = false;
		dms_force_macro_typing = false;
		dms_error_policy = EPIgnore;
		dms_check_core_api = false;
		dms_inline = false;
		dms_display_file_policy = DFPOnly;
		dms_exit_during_typing = true;
		dms_per_file = false;
	}

	let default_compilation_settings = {
		dms_kind = DMNone;
		dms_display = false;
		dms_full_typing = true;
		dms_force_macro_typing = true;
		dms_error_policy = EPShow;
		dms_check_core_api = true;
		dms_inline = true;
		dms_display_file_policy = DFPNo;
		dms_exit_during_typing = false;
		dms_per_file = false;
	}

	let create dm =
		let settings = { default_display_settings with dms_kind = dm } in
		match dm with
		| DMNone -> default_compilation_settings
		| DMDefault | DMDefinition | DMTypeDefinition | DMResolve _ | DMPackage | DMHover | DMSignature -> settings
		| DMUsage _ | DMImplementation -> { settings with
				dms_full_typing = true;
				dms_force_macro_typing = true;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false
			}
		| DMModuleSymbols filter -> { settings with
				dms_display_file_policy = if filter = None then DFPOnly else DFPNo;
				dms_exit_during_typing = false;
				dms_force_macro_typing = false;
				dms_per_file = true;
			}
		| DMDiagnostics files -> { default_compilation_settings with
				dms_kind = DMDiagnostics files;
				dms_error_policy = EPCollect;
				dms_display_file_policy = if files = [] then DFPNo else DFPAlso;
				dms_per_file = true;
			}
		| DMStatistics -> { settings with
				dms_full_typing = true;
				dms_inline = false;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false;
				dms_force_macro_typing = true;
				dms_per_file = true;
			}

	let to_string = function
		| DMNone -> "none"
		| DMDefault -> "field"
		| DMDefinition -> "position"
		| DMTypeDefinition -> "type-definition"
		| DMImplementation -> "implementation"
		| DMResolve s -> "resolve " ^ s
		| DMPackage -> "package"
		| DMHover -> "type"
		| DMUsage true -> "rename"
		| DMUsage false -> "references"
		| DMModuleSymbols None -> "module-symbols"
		| DMModuleSymbols (Some s) -> "workspace-symbols " ^ s
		| DMDiagnostics _ -> "diagnostics"
		| DMStatistics -> "statistics"
		| DMSignature -> "signature"
end

type symbol =
	| SKClass of tclass
	| SKInterface of tclass
	| SKEnum of tenum
	| SKTypedef of tdef
	| SKAbstract of tabstract
	| SKField of tclass_field
	| SKConstructor of tclass_field
	| SKEnumField of tenum_field
	| SKVariable of tvar
	| SKOther

type completion_subject = {
	s_name : string option;
	s_start_pos : pos;
	s_insert_pos : pos;
}

let make_subject name ?(start_pos=None) insert_pos = {
	s_name = name;
	s_start_pos = (match start_pos with None -> insert_pos | Some p -> p);
	s_insert_pos = insert_pos;
}

let string_of_symbol = function
	| SKClass c | SKInterface c -> snd c.cl_path
	| SKEnum en -> snd en.e_path
	| SKTypedef td -> snd td.t_path
	| SKAbstract a -> snd a.a_path
	| SKField cf | SKConstructor cf -> cf.cf_name
	| SKEnumField ef -> ef.ef_name
	| SKVariable v -> v.v_name
	| SKOther -> ""