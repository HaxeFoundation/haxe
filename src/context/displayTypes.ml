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

module CompletionKind = struct
	type resolution_mode =
		| RMLocalModule
		| RMImport
		| RMUsing
		| RMTypeParameter
		| RMClassPath
		| RMOtherModule of path

	type t =
		| ITLocal of tvar
		| ITClassMember of tclass_field
		| ITClassStatic of tclass_field
		| ITEnumField of tenum * tenum_field
		| ITEnumAbstractField of tabstract * tclass_field
		| ITGlobal of module_type * string * Type.t
		| ITType of module_type * resolution_mode
		| ITPackage of string
		| ITModule of string
		| ITLiteral of string * Type.t
		| ITTimer of string * string
		| ITMetadata of string * documentation

	let legacy_sort = function
		| ITClassMember cf | ITClassStatic cf | ITEnumAbstractField(_,cf) ->
			begin match cf.cf_kind with
			| Var _ -> 0,cf.cf_name
			| Method _ -> 1,cf.cf_name
			end
		| ITEnumField(_,ef) ->
			begin match follow ef.ef_type with
			| TFun _ -> 1,ef.ef_name
			| _ -> 0,ef.ef_name
			end
		| ITType(mt,_) ->
			2,(snd (t_infos mt).mt_path)
		| ITModule s -> 3,s
		| ITPackage s -> 4,s
		| ITMetadata(s,_) -> 5,s
		| ITTimer(s,_) -> 6,s
		| ITLocal v -> 7,v.v_name
		| ITGlobal(_,s,_) -> 8,s
		| ITLiteral(s,_) -> 9,s

	let get_name = function
		| ITLocal v -> v.v_name
		| ITClassMember cf | ITClassStatic cf | ITEnumAbstractField(_,cf) -> cf.cf_name
		| ITEnumField(_,ef) -> ef.ef_name
		| ITGlobal(_,s,_) -> s
		| ITType(mt,_) -> snd (t_infos mt).mt_path
		| ITPackage s -> s
		| ITModule s -> s
		| ITLiteral(s,_) -> s
		| ITTimer(s,_) -> s
		| ITMetadata(s,_) -> s

	let get_type = function
		| ITLocal v -> v.v_type
		| ITClassMember cf | ITClassStatic cf | ITEnumAbstractField(_,cf) -> cf.cf_type
		| ITEnumField(_,ef) -> ef.ef_type
		| ITGlobal(_,_,t) -> t
		| ITType(mt,_) -> t_dynamic (* TODO: hmm *)
		| ITPackage _ -> t_dynamic
		| ITModule _ -> t_dynamic
		| ITLiteral(_,t) -> t
		| ITTimer(_,_) -> t_dynamic
		| ITMetadata(_,_) -> t_dynamic

	let to_json ctx ck =
		let kind,data = match ck with
			| ITLocal v -> "Local",generate_tvar ctx v
			| ITClassMember cf -> "Member",generate_class_field ctx cf
			| ITClassStatic cf -> "Static",generate_class_field ctx cf
			| ITEnumField(_,ef) -> "EnumField",generate_enum_field ctx ef
			| ITEnumAbstractField(_,cf) -> "EnumAbstractField",generate_class_field ctx cf
			| ITGlobal(mt,s,t) -> "Global",jobject [
				"modulePath",generate_path (t_infos mt).mt_path;
				"name",jstring s;
				"type",generate_type ctx t
			]
			| ITType(mt,rm) -> "Type",generate_module_type (Genjson.create_context()) mt (* TODO: resolution mode *)
			| ITPackage s -> "Package",jstring s
			| ITModule s -> "Module",jstring s
			| ITLiteral(s,_) -> "Literal",jstring s
			| ITTimer(s,value) -> "Timer",jobject [
				"name",jstring s;
				"value",jstring value;
			]
			| ITMetadata(s,doc) -> "Metadata",jobject [
				"name",jstring s;
				"doc",jopt jstring doc;
			]
		in
		generate_adt ctx None kind (Some data)
end

module DisplayMode = struct
	type t =
		| DMNone
		| DMDefault
		| DMUsage of bool (* true = also report definition *)
		| DMDefinition
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
		| DMDefault | DMDefinition | DMResolve _ | DMPackage | DMHover | DMSignature -> settings
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
		| DMDiagnostics global -> { settings with
				dms_full_typing = true;
				dms_error_policy = EPCollect;
				dms_collect_data = true;
				dms_inline = true;
				dms_force_macro_typing = true;
				dms_display_file_policy = if global then DFPNo else DFPAlso;
				dms_exit_during_typing = false;
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