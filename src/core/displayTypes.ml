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
		| Module

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
		| Module -> 17
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
		| DKMissingFields

	let to_int = function
		| DKUnusedImport -> 0
		| DKUnresolvedIdentifier -> 1
		| DKCompilerError -> 2
		| DKRemovableCode -> 3
		| DKParserError -> 4
		| DKDeprecationWarning -> 5
		| DKInactiveBlock -> 6
		| DKMissingFields -> 7
end

module CompletionResultKind = struct
	type expected_type_completion = {
		expected_type : CompletionItem.CompletionType.t;
		expected_type_followed : CompletionItem.CompletionType.t;
		compatible_types : CompletionItem.CompletionType.t list;
	}

	type t =
		| CRField of CompletionItem.t * pos * Type.t option * (Type.t * Type.t) option
		| CRStructureField
		| CRToplevel of expected_type_completion option
		| CRMetadata
		| CRTypeHint
		| CRExtends
		| CRImplements
		| CRStructExtension of bool
		| CRImport
		| CRUsing
		| CRNew
		| CRPattern of expected_type_completion option * bool
		| CROverride
		| CRTypeRelation
		| CRTypeDecl

	let to_json ctx kind =
		let expected_type_fields t = match t with
			| None -> []
			| Some ext -> [
					"expectedType",CompletionItem.CompletionType.to_json ctx ext.expected_type;
					"expectedTypeFollowed",CompletionItem.CompletionType.to_json ctx ext.expected_type_followed;
					"compatibleTypes",jarray (List.map (CompletionItem.CompletionType.to_json ctx) ext.compatible_types);
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
		(**
			Find usages/references of the requested symbol.
			@param bool - add symbol definition to the response
			@param bool - also find usages of descendants of the symbol (e.g methods, which override the requested one)
			@param bool - look for a base method if requested for a method with `override` accessor.
		*)
		| DMUsage of bool * bool * bool
		| DMDefinition
		| DMTypeDefinition
		| DMImplementation
		| DMPackage
		| DMHover
		| DMModuleSymbols of string option
		| DMSignature

	type error_policy =
		| EPIgnore
		| EPShow

	type display_file_policy =
		| DFPOnly
		| DFPAlso
		| DFPNo

	type settings = {
		dms_kind : t;
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
		| DMDefault | DMDefinition | DMTypeDefinition | DMPackage | DMHover | DMSignature -> settings
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

	let to_string = function
		| DMNone -> "none"
		| DMDefault -> "field"
		| DMDefinition -> "position"
		| DMTypeDefinition -> "type-definition"
		| DMImplementation -> "implementation"
		| DMPackage -> "package"
		| DMHover -> "type"
		| DMUsage (true,_,_) -> "rename"
		| DMUsage (false,_,_) -> "references"
		| DMModuleSymbols None -> "module-symbols"
		| DMModuleSymbols (Some s) -> "workspace-symbols " ^ s
		| DMSignature -> "signature"
end

type symbol =
	| SKClass of tclass
	| SKInterface of tclass
	| SKEnum of tenum
	| SKTypedef of tdef
	| SKAbstract of tabstract
	| SKField of tclass_field * path option (* path - class path *)
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
	| SKField (cf,_) | SKConstructor cf -> cf.cf_name
	| SKEnumField ef -> ef.ef_name
	| SKVariable v -> v.v_name
	| SKOther -> ""

type hover_result = {
	hitem : CompletionItem.t;
	hpos : pos;
	hexpected : WithType.t option;
}

type fields_result = {
	fitems : CompletionItem.t list;
	fkind : CompletionResultKind.t;
	fsubject : completion_subject;
}

type signature_kind =
	| SKCall
	| SKArrayAccess

(* diagnostics *)

type missing_field_cause =
	| AbstractParent of tclass * tparams
	| ImplementedInterface of tclass * tparams
	| PropertyAccessor of tclass_field * bool (* true = getter *)
	| FieldAccess
	| FinalFields of tclass_field list

and missing_fields_diagnostics = {
	mf_pos : pos;
	mf_on : module_type;
	mf_fields : (tclass_field * Type.t * CompletionItem.CompletionType.t) list;
	mf_cause : missing_field_cause;
}

and module_diagnostics =
	| MissingFields of missing_fields_diagnostics

type diagnostics_context = {
	mutable removable_code : (string * pos * pos) list;
	mutable import_positions : (pos,bool ref) PMap.t;
	mutable dead_blocks : (Path.UniqueKey.t,(pos * expr) list) Hashtbl.t;
	mutable unresolved_identifiers : (string * pos * (string * CompletionItem.t * int) list) list;
	mutable diagnostics_messages : (string * pos * DiagnosticsKind.t * DiagnosticsSeverity.t) list;
	mutable missing_fields : (pos,(module_type * (missing_fields_diagnostics list ref))) PMap.t;
}

type display_exception_kind =
	| ModuleSymbols of string
	| Metadata of string
	| DisplaySignatures of (((tsignature * CompletionItem.CompletionType.ct_function) * documentation) list * int * int * signature_kind) option
	| DisplayHover of hover_result option
	| DisplayPositions of pos list
	| DisplayFields of fields_result option
	| DisplayPackage of string list