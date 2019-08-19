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
		| Typedef
		| Abstract
		| Field
		| Property
		| Method
		| Constructor
		| Function
		| Variable

	let to_int = function
		| Class -> 1
		| Interface -> 2
		| Enum -> 3
		| Typedef -> 4
		| Abstract -> 5
		| Field -> 6
		| Property -> 7
		| Method -> 8
		| Constructor -> 9
		| Function -> 10
		| Variable -> 11
end

module SymbolInformation = struct
	type t = {
		name : string;
		kind : SymbolKind.t;
		pos : Globals.pos;
		container_name : string option;
	}

	let make name kind pos container_name = {
		name = name;
		kind = kind;
		pos = pos;
		container_name = container_name;
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

	let to_int = function
		| DKUnusedImport -> 0
		| DKUnresolvedIdentifier -> 1
		| DKCompilerError -> 2
		| DKRemovableCode -> 3
		| DKParserError -> 4
		| DKDeprecationWarning -> 5
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
		| DMResolve of string
		| DMPackage
		| DMHover
		| DMModuleSymbols of string option
		| DMDiagnostics of bool (* true = global, false = only in display file *)
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
		dms_collect_data : bool;
		dms_check_core_api : bool;
		dms_inline : bool;
		dms_display_file_policy : display_file_policy;
		dms_exit_during_typing : bool;
	}

	let default_display_settings = {
		dms_kind = DMDefault;
		dms_display = true;
		dms_full_typing = false;
		dms_force_macro_typing = false;
		dms_error_policy = EPIgnore;
		dms_collect_data = false;
		dms_check_core_api = false;
		dms_inline = false;
		dms_display_file_policy = DFPOnly;
		dms_exit_during_typing = true;
	}

	let default_compilation_settings = {
		dms_kind = DMNone;
		dms_display = false;
		dms_full_typing = true;
		dms_force_macro_typing = true;
		dms_error_policy = EPShow;
		dms_collect_data = false;
		dms_check_core_api = true;
		dms_inline = true;
		dms_display_file_policy = DFPNo;
		dms_exit_during_typing = false;
	}

	let create dm =
		let settings = { default_display_settings with dms_kind = dm } in
		match dm with
		| DMNone -> default_compilation_settings
		| DMDefault | DMDefinition | DMTypeDefinition | DMResolve _ | DMPackage | DMHover | DMSignature -> settings
		| DMUsage _ -> { settings with
				dms_full_typing = true;
				dms_force_macro_typing = true;
				dms_collect_data = true;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false
			}
		| DMModuleSymbols filter -> { settings with
				dms_display_file_policy = if filter = None then DFPOnly else DFPNo;
				dms_exit_during_typing = false;
				dms_force_macro_typing = false;
			}
		| DMDiagnostics global -> { default_compilation_settings with
				dms_kind = DMDiagnostics global;
				dms_error_policy = EPCollect;
				dms_collect_data = true;
				dms_display_file_policy = if global then DFPNo else DFPAlso;
			}
		| DMStatistics -> { settings with
				dms_full_typing = true;
				dms_collect_data = true;
				dms_inline = false;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false;
				dms_force_macro_typing = true;
			}

	let to_string = function
		| DMNone -> "none"
		| DMDefault -> "field"
		| DMDefinition -> "position"
		| DMTypeDefinition -> "type-definition"
		| DMResolve s -> "resolve " ^ s
		| DMPackage -> "package"
		| DMHover -> "type"
		| DMUsage true -> "rename"
		| DMUsage false -> "references"
		| DMModuleSymbols None -> "module-symbols"
		| DMModuleSymbols (Some s) -> "workspace-symbols " ^ s
		| DMDiagnostics b -> (if b then "global " else "") ^ "diagnostics"
		| DMStatistics -> "statistics"
		| DMSignature -> "signature"
end

type reference_kind =
	| KVar
	| KIdent
	| KAnyField
	| KClassField
	| KEnumField
	| KModuleType
	| KConstructor

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