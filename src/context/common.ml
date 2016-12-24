(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Ast
open Type
open Globals

type package_rule =
	| Forbidden
	| Directory of string
	| Remap of string

type pos = Globals.pos

type basic_types = {
	mutable tvoid : t;
	mutable tint : t;
	mutable tfloat : t;
	mutable tbool : t;
	mutable tnull : t -> t;
	mutable tstring : t;
	mutable tarray : t -> t;
}

type stats = {
	s_files_parsed : int ref;
	s_classes_built : int ref;
	s_methods_typed : int ref;
	s_macros_called : int ref;
}

(**
	The capture policy tells which handling we make of captured locals
	(the locals which are referenced in local functions)

	See details/implementation in Codegen.captured_vars
*)
type capture_policy =
	(** do nothing, let the platform handle it *)
	| CPNone
	(** wrap all captured variables into a single-element array to allow modifications *)
	| CPWrapRef
	(** similar to wrap ref, but will only apply to the locals that are declared in loops *)
	| CPLoopVars

type platform_config = {
	(** has a static type system, with not-nullable basic types (Int/Float/Bool) *)
	pf_static : bool;
	(** has access to the "sys" package *)
	pf_sys : bool;
	(** captured variables handling (see before) *)
	pf_capture_policy : capture_policy;
	(** when calling a method with optional args, do we replace the missing args with "null" constants *)
	pf_pad_nulls : bool;
	(** add a final return to methods not having one already - prevent some compiler warnings *)
	pf_add_final_return : bool;
	(** does the platform natively support overloaded functions *)
	pf_overload : bool;
	(** can the platform use default values for non-nullable arguments *)
	pf_can_skip_non_nullable_argument : bool;
	(** type paths that are reserved on the platform *)
	pf_reserved_type_paths : path list;
}

module DisplayMode = struct
	type t =
		| DMNone
		| DMField
		| DMUsage of bool (* true = also report definition *)
		| DMPosition
		| DMToplevel
		| DMResolve of string
		| DMPackage
		| DMType
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
		dms_kind = DMField;
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
		| DMField | DMPosition | DMResolve _ | DMPackage | DMType | DMSignature -> settings
		| DMUsage _ -> { settings with
				dms_full_typing = true;
				dms_collect_data = true;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false
			}
		| DMToplevel -> { settings with dms_full_typing = true; }
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
				dms_display_file_policy = if global then DFPNo else DFPAlso;
				dms_exit_during_typing = false;
			}
		| DMStatistics -> { settings with
				dms_full_typing = true;
				dms_collect_data = true;
				dms_inline = false;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false
			}

	let to_string = function
		| DMNone -> "none"
		| DMField -> "field"
		| DMPosition -> "position"
		| DMResolve s -> "resolve " ^ s
		| DMPackage -> "package"
		| DMType -> "type"
		| DMUsage true -> "rename"
		| DMUsage false -> "references"
		| DMToplevel -> "toplevel"
		| DMModuleSymbols None -> "module-symbols"
		| DMModuleSymbols (Some s) -> "workspace-symbols " ^ s
		| DMDiagnostics b -> (if b then "global " else "") ^ "diagnostics"
		| DMStatistics -> "statistics"
		| DMSignature -> "signature"
end

type compiler_callback = {
	mutable after_typing : (module_type list -> unit) list;
	mutable before_dce : (unit -> unit) list;
	mutable after_generation : (unit -> unit) list;
}

module IdentifierType = struct
	type t =
		| ITLocal of tvar
		| ITMember of tclass * tclass_field
		| ITStatic of tclass * tclass_field
		| ITEnum of tenum * tenum_field
		| ITEnumAbstract of tabstract * tclass_field
		| ITGlobal of module_type * string * Type.t
		| ITType of module_type
		| ITPackage of string
		| ITLiteral of string
		| ITTimer of string

	let get_name = function
		| ITLocal v -> v.v_name
		| ITMember(_,cf) | ITStatic(_,cf) | ITEnumAbstract(_,cf) -> cf.cf_name
		| ITEnum(_,ef) -> ef.ef_name
		| ITGlobal(_,s,_) -> s
		| ITType mt -> snd (t_infos mt).mt_path
		| ITPackage s -> s
		| ITLiteral s -> s
		| ITTimer s -> s
end

type shared_display_information = {
	mutable import_positions : (pos,bool ref * placed_name list) PMap.t;
	mutable diagnostics_messages : (string * pos * DisplayTypes.DiagnosticsSeverity.t) list;
	mutable type_hints : (pos,Type.t) Hashtbl.t;
	mutable document_symbols : (string * DisplayTypes.SymbolInformation.t DynArray.t) list;
	mutable removable_code : (string * pos * pos) list;
}

type display_information = {
	mutable unresolved_identifiers : (string * pos * (string * IdentifierType.t) list) list;
	mutable interface_field_implementations : (tclass * tclass_field * tclass * tclass_field option) list;
}

(* This information is shared between normal and macro context. *)
type shared_context = {
	shared_display_information : shared_display_information;
}

type context = {
	(* config *)
	version : int;
	args : string list;
	shared : shared_context;
	display_information : display_information;
	mutable sys_args : string list;
	mutable display : DisplayMode.settings;
	mutable debug : bool;
	mutable verbose : bool;
	mutable foptimize : bool;
	mutable platform : platform;
	mutable config : platform_config;
	mutable std_path : string list;
	mutable class_path : string list;
	mutable main_class : Type.path option;
	mutable defines : (string,string) PMap.t;
	mutable package_rules : (string,package_rule) PMap.t;
	mutable error : string -> pos -> unit;
	mutable warning : string -> pos -> unit;
	mutable load_extern_type : (path -> pos -> (string * Ast.package) option) list; (* allow finding types which are not in sources *)
	callbacks : compiler_callback;
	mutable defines_signature : string option;
	mutable print : string -> unit;
	mutable get_macros : unit -> context option;
	mutable run_command : string -> int;
	file_lookup_cache : (string,string option) Hashtbl.t;
	parser_cache : (string,(type_def * pos) list) Hashtbl.t;
	cached_macros : (path * string,((string * bool * t) list * t * tclass * Type.tclass_field)) Hashtbl.t;
	mutable stored_typed_exprs : (int, texpr) PMap.t;
	(* output *)
	mutable file : string;
	mutable flash_version : float;
	mutable features : (string,bool) Hashtbl.t;
	mutable modules : Type.module_def list;
	mutable main : Type.texpr option;
	mutable types : Type.module_type list;
	mutable resources : (string,string) Hashtbl.t;
	mutable neko_libs : string list;
	mutable include_files : (string * string) list;
	mutable php_front : string option;
	mutable php_lib : string option;
	mutable php_prefix : string option;
	mutable swf_libs : (string * (unit -> Swf.swf) * (unit -> ((string list * string),As3hl.hl_class) Hashtbl.t)) list;
	mutable java_libs : (string * bool * (unit -> unit) * (unit -> (path list)) * (path -> ((JData.jclass * string * string) option))) list; (* (path,std,close,all_files,lookup) *)
	mutable net_libs : (string * bool * (unit -> path list) * (path -> IlData.ilclass option)) list; (* (path,std,all_files,lookup) *)
	mutable net_std : string list;
	net_path_map : (path,string list * string list * string) Hashtbl.t;
	mutable c_args : string list;
	mutable js_gen : (unit -> unit) option;
	(* typing *)
	mutable basic : basic_types;
	memory_marker : float array;
}

exception Abort of string * pos

let display_default = ref DisplayMode.DMNone

let get_signature com =
	match com.defines_signature with
	| Some s -> s
	| None ->
		let defines = PMap.foldi (fun k v acc ->
			(* don't make much difference between these special compilation flags *)
			match String.concat "_" (ExtString.String.nsplit k "-") with
			(* If we add something here that might be used in conditional compilation it should be added to
			   Parser.parse_macro_ident as well (issue #5682). *)
			| "display" | "use_rtti_doc" | "macro_times" | "display_details" | "no_copt" | "display_stdin" -> acc
			| _ -> (k ^ "=" ^ v) :: acc
		) com.defines [] in
		let str = String.concat "@" (List.sort compare defines) in
		let s = Digest.string str in
		com.defines_signature <- Some s;
		s

let php7 com = com.platform = Php && PMap.exists "php7" com.defines

module CompilationServer = struct
	type cache = {
		c_haxelib : (string list, string list) Hashtbl.t;
		c_files : ((string * string), float * Ast.package) Hashtbl.t;
		c_modules : (path * string, module_def) Hashtbl.t;
		c_directories : (string, (string * float ref) list) Hashtbl.t;
	}

	type t = {
		cache : cache;
		mutable signs : (string * string) list;
	}

	type context_options =
		| NormalContext
		| MacroContext
		| NormalAndMacroContext

	let instance : t option ref = ref None

	let create_cache () = {
		c_haxelib = Hashtbl.create 0;
		c_files = Hashtbl.create 0;
		c_modules = Hashtbl.create 0;
		c_directories = Hashtbl.create 0;
	}

	let create () =
		let cs = {
			cache = create_cache();
			signs = [];
		} in
		instance := Some cs;
		cs

	let get () =
		!instance

	let runs () =
		!instance <> None

	let get_context_files cs signs =
		Hashtbl.fold (fun (file,sign) (_,data) acc ->
			if (List.mem sign signs) then (file,data) :: acc
			else acc
		) cs.cache.c_files []

	(* signatures *)

	let get_sign cs sign =
		List.assoc sign cs.signs

	let add_sign cs sign =
		let i = string_of_int (List.length cs.signs) in
		cs.signs <- (sign,i) :: cs.signs;
		i

	(* modules *)

	let find_module cs key =
		Hashtbl.find cs.cache.c_modules key

	let cache_module cs key value =
		Hashtbl.replace cs.cache.c_modules key value

	let taint_modules cs file =
		Hashtbl.iter (fun _ m -> if m.m_extra.m_file = file then m.m_extra.m_dirty <- Some m) cs.cache.c_modules

	(* files *)

	let find_file cs key =
		Hashtbl.find cs.cache.c_files key

	let cache_file cs key value =
		Hashtbl.replace cs.cache.c_files key value

	let remove_file cs key =
		Hashtbl.remove cs.cache.c_files key

	let remove_files cs file =
		List.iter (fun (sign,_) -> remove_file cs (sign,file)) cs.signs

	(* haxelibs *)

	let find_haxelib cs key =
		Hashtbl.find cs.cache.c_haxelib key

	let cache_haxelib cs key value =
		Hashtbl.replace cs.cache.c_haxelib key value

	(* directories *)

	let find_directories cs key =
		Hashtbl.find cs.cache.c_directories key

	let add_directories cs key value =
		Hashtbl.replace cs.cache.c_directories key value

	let remove_directory cs key value =
		try
			let current = find_directories cs key in
			Hashtbl.replace cs.cache.c_directories key (List.filter (fun (s,_) -> s <> value) current);
		with Not_found ->
			()

	let has_directory cs key value =
		try
			List.mem_assoc value (find_directories cs key)
		with Not_found ->
			false

	let add_directory cs key value =
		try
			let current = find_directories cs key in
			add_directories cs key (value :: current)
		with Not_found ->
			add_directories cs key [value]

	let clear_directories cs key =
		Hashtbl.remove cs.cache.c_directories key
end

module Define = struct

	type strict_defined =
		| AbsolutePath
		| AdvancedTelemetry
		| AnnotateSource
		(* | Analyzer *)
		| As3
		| CheckXmlProxy
		| CoreApi
		| CoreApiSerialize
		| Cppia
		| Dce
		| DceDebug
		| Debug
		| Display
		| DisplayStdin
		| DllExport
		| DllImport
		| DocGen
		| Dump
		| DumpDependencies
		| DumpIgnoreVarIds
		| DynamicInterfaceClosures
		| EraseGenerics
		| FastCast
		| Fdb
		| FileExtension
		| FlashStrict
		| FlashUseStage
		| ForceLibCheck
		| ForceNativeProperty
		| FormatWarning
		| GencommonDebug
		| HaxeBoot
		| HaxeVer
		| HxcppApiLevel
		| HxcppGcGenerational
		| IncludePrefix
		| Interp
		| JavaVer
		| JqueryVer
		| JsClassic
		| JsEs
		| JsUnflatten
		| JsSourceMap
		| KeepOldOutput
		| LoopUnrollMaxCost
		| LuaVer
		| LuaJit
		| Macro
		| MacroDebug
		| MacroTimes
		| NekoSource
		| NekoV1
		| NetworkSandbox
		| NetVer
		| NetTarget
		| NoAnalyzer
		| NoCompilation
		| NoCOpt
		| NoDeprecationWarnings
		| NoFlashOverride
		| NoDebug
		| NoInline
		| NoOpt
		| NoPatternMatching
		| NoRoot
		| NoSwfCompress
		| NoTraces
		| Objc
		| PhpPrefix
		| RealPosition
		| ReplaceFiles
		| Scriptable
		| ShallowExpose
		| SourceHeader
		| SourceMapContent
		| Swc
		| SwfCompressLevel
		| SwfDebugPassword
		| SwfDirectBlit
		| SwfGpu
		| SwfMetadata
		| SwfPreloaderFrame
		| SwfProtected
		| SwfScriptTimeout
		| SwfUseDoAbc
		| Sys
		| Unsafe
		| UseNekoc
		| UseRttiDoc
		| Vcproj
		| NoMacroCache
		| Last (* must be last *)

	let infos = function
		| AbsolutePath -> ("absolute_path","Print absolute file path in trace output")
		| AdvancedTelemetry -> ("advanced-telemetry","Allow the SWF to be measured with Monocle tool")
		| AnnotateSource -> ("annotate_source","Add additional comments to generated source code")
		(* | Analyzer -> ("analyzer","Use static analyzer for optimization (experimental)") *)
		| As3 -> ("as3","Defined when outputing flash9 as3 source code")
		| CheckXmlProxy -> ("check_xml_proxy","Check the used fields of the xml proxy")
		| CoreApi -> ("core_api","Defined in the core api context")
		| CoreApiSerialize -> ("core_api_serialize","Mark some generated core api classes with the Serializable attribute on C#")
		| Cppia -> ("cppia", "Generate cpp instruction assembly")
		| Dce -> ("dce","<mode:std|full|no> Set the dead code elimination mode (default std)")
		| DceDebug -> ("dce_debug","Show DCE log")
		| Debug -> ("debug","Activated when compiling with -debug")
		| Display -> ("display","Activated during completion")
		| DisplayStdin -> ("display_stdin","Read the contents of a file specified in --display from standard input")
		| DllExport -> ("dll_export", "GenCPP experimental linking")
		| DllImport -> ("dll_import", "GenCPP experimental linking")
		| DocGen -> ("doc_gen","Do not perform any removal/change in order to correctly generate documentation")
		| Dump -> ("dump","<mode:pretty|record|legacy> Dump typed AST in dump subdirectory using specified mode or non-prettified default")
		| DumpDependencies -> ("dump_dependencies","Dump the classes dependencies in a dump subdirectory")
		| DumpIgnoreVarIds -> ("dump_ignore_var_ids","Remove variable IDs from non-pretty dumps (helps with diff)")
		| DynamicInterfaceClosures -> ("dynamic_interface_closures","Use slow path for interface closures to save space")
		| EraseGenerics -> ("erase_generics","Erase generic classes on C#")
		| FastCast -> ("fast_cast","Enables an experimental casts cleanup on C# and Java")
		| Fdb -> ("fdb","Enable full flash debug infos for FDB interactive debugging")
		| FileExtension -> ("file_extension","Output filename extension for cpp source code")
		| FlashStrict -> ("flash_strict","More strict typing for flash target")
		| FlashUseStage -> ("flash_use_stage","Keep the SWF library initial stage")
		(* force_lib_check is only here as a debug facility - compiler checking allows errors to be found more easily *)
		| ForceLibCheck -> ("force_lib_check","Force the compiler to check -net-lib and -java-lib added classes (internal)")
		| ForceNativeProperty -> ("force_native_property","Tag all properties with :nativeProperty metadata for 3.1 compatibility")
		| FormatWarning -> ("format_warning","Print a warning for each formated string, for 2.x compatibility")
		| GencommonDebug -> ("gencommon_debug","GenCommon internal")
		| HaxeBoot -> ("haxe_boot","Given the name 'haxe' to the flash boot class instead of a generated name")
		| HaxeVer -> ("haxe_ver","The current Haxe version value")
		| HxcppApiLevel -> ("hxcpp_api_level","Provided to allow compatibility between hxcpp versions")
		| HxcppGcGenerational -> ("HXCPP_GC_GENERATIONAL","Experimental Garbage Collector")
		| IncludePrefix -> ("include_prefix","prepend path to generated include files")
		| Interp -> ("interp","The code is compiled to be run with --interp")
		| JavaVer -> ("java_ver", "<version:5-7> Sets the Java version to be targeted")
		| JqueryVer -> ("jquery_ver", "The jQuery version supported by js.jquery.*. The version is encoded as an interger. e.g. 1.11.3 is encoded as 11103")
		| JsClassic -> ("js_classic","Don't use a function wrapper and strict mode in JS output")
		| JsEs -> ("js_es","Generate JS compilant with given ES standard version (default 5)")
		| JsUnflatten -> ("js_unflatten","Generate nested objects for packages and types")
		| JsSourceMap -> ("js_source_map","Generate JavaScript source map even in non-debug mode")
		| KeepOldOutput -> ("keep_old_output","Keep old source files in the output directory (for C#/Java)")
		| LoopUnrollMaxCost -> ("loop_unroll_max_cost","Maximum cost (number of expressions * iterations) before loop unrolling is canceled (default 250)")
		| LuaJit -> ("lua_jit","Enable the jit compiler for lua (version 5.2 only")
		| LuaVer -> ("lua_ver","The lua version to target")
		| Macro -> ("macro","Defined when code is compiled in the macro context")
		| MacroDebug -> ("macro_debug","Show warnings for potential macro problems (e.g. macro-in-macro calls)")
		| MacroTimes -> ("macro_times","Display per-macro timing when used with --times")
		| NetVer -> ("net_ver", "<version:20-45> Sets the .NET version to be targeted")
		| NetTarget -> ("net_target", "<name> Sets the .NET target. Defaults to \"net\". xbox, micro (Micro Framework), compact (Compact Framework) are some valid values")
		| NekoSource -> ("neko_source","Output neko source instead of bytecode")
		| NekoV1 -> ("neko_v1","Keep Neko 1.x compatibility")
		| NetworkSandbox -> ("network-sandbox","Use local network sandbox instead of local file access one")
		| NoAnalyzer -> ("no-analyzer","Disable the static analyzer")
		| NoCompilation -> ("no-compilation","Disable final compilation for Cs, Cpp and Java")
		| NoCOpt -> ("no_copt","Disable completion optimization (for debug purposes)")
		| NoDebug -> ("no_debug","Remove all debug macros from cpp output")
		| NoDeprecationWarnings -> ("no-deprecation-warnings","Do not warn if fields annotated with @:deprecated are used")
		| NoFlashOverride -> ("no-flash-override", "Change overrides on some basic classes into HX suffixed methods, flash only")
		| NoOpt -> ("no_opt","Disable optimizations")
		| NoPatternMatching -> ("no_pattern_matching","Disable pattern matching")
		| NoInline -> ("no_inline","Disable inlining")
		| NoRoot -> ("no_root","Generate top-level types into haxe.root namespace")
		| NoMacroCache -> ("no_macro_cache","Disable macro context caching")
		| NoSwfCompress -> ("no_swf_compress","Disable SWF output compression")
		| NoTraces -> ("no_traces","Disable all trace calls")
		| Objc -> ("objc","Sets the hxcpp output to objective-c++ classes. Must be defined for interop")
		| PhpPrefix -> ("php_prefix","Compiled with --php-prefix")
		| RealPosition -> ("real_position","Disables Haxe source mapping when targetting C#, removes position comments in Java output")
		| ReplaceFiles -> ("replace_files","GenCommon internal")
		| Scriptable -> ("scriptable","GenCPP internal")
		| ShallowExpose -> ("shallow-expose","Expose types to surrounding scope of Haxe generated closure without writing to window object")
		| SourceHeader -> ("source-header","Print value as comment on top of generated files, use '' value to disable")
		| SourceMapContent -> ("source-map-content","Include the hx sources as part of the JS source map")
		| Swc -> ("swc","Output a SWC instead of a SWF")
		| SwfCompressLevel -> ("swf_compress_level","<level:1-9> Set the amount of compression for the SWF output")
		| SwfDebugPassword -> ("swf_debug_password", "Set a password for debugging")
		| SwfDirectBlit -> ("swf_direct_blit", "Use hardware acceleration to blit graphics")
		| SwfGpu -> ("swf_gpu", "Use GPU compositing features when drawing graphics")
		| SwfMetadata -> ("swf_metadata", "<file> Include contents of <file> as metadata in the swf")
		| SwfPreloaderFrame -> ("swf_preloader_frame", "Insert empty first frame in swf")
		| SwfProtected -> ("swf_protected","Compile Haxe private as protected in the SWF instead of public")
		| SwfScriptTimeout -> ("swf_script_timeout", "Maximum ActionScript processing time before script stuck dialog box displays (in seconds)")
		| SwfUseDoAbc -> ("swf_use_doabc", "Use DoAbc swf-tag instead of DoAbcDefine")
		| Sys -> ("sys","Defined for all system platforms")
		| Unsafe -> ("unsafe","Allow unsafe code when targeting C#")
		| UseNekoc -> ("use_nekoc","Use nekoc compiler instead of internal one")
		| UseRttiDoc -> ("use_rtti_doc","Allows access to documentation during compilation")
		| Vcproj -> ("vcproj","GenCPP internal")
		| Last -> assert false
end

let short_platform_name = function
	| Cross -> "x"
	| Js -> "js"
	| Lua -> "lua"
	| Neko -> "n"
	| Flash -> "swf"
	| Php -> "php"
	| Cpp -> "cpp"
	| Cs -> "cs"
	| Java -> "jav"
	| Python -> "py"
	| Hl -> "hl"

let stats =
	{
		s_files_parsed = ref 0;
		s_classes_built = ref 0;
		s_methods_typed = ref 0;
		s_macros_called = ref 0;
	}

let default_config =
	{
		pf_static = true;
		pf_sys = true;
		pf_capture_policy = CPNone;
		pf_pad_nulls = false;
		pf_add_final_return = false;
		pf_overload = false;
		pf_can_skip_non_nullable_argument = true;
		pf_reserved_type_paths = [];
	}

let get_config com =
	let defined f = PMap.mem (fst (Define.infos f)) com.defines in
	match com.platform with
	| Cross ->
		default_config
	| Js ->
		{
			default_config with
			pf_static = false;
			pf_sys = false;
			pf_capture_policy = CPLoopVars;
			pf_reserved_type_paths = [([],"Object");([],"Error")];
		}
	| Lua ->
		{
			default_config with
			pf_static = false;
			pf_capture_policy = CPLoopVars;
		}
	| Neko ->
		{
			default_config with
			pf_static = false;
			pf_pad_nulls = true;
		}
	| Flash when defined Define.As3 ->
		{
			default_config with
			pf_sys = false;
			pf_capture_policy = CPLoopVars;
			pf_add_final_return = true;
			pf_can_skip_non_nullable_argument = false;
		}
	| Flash ->
		{
			default_config with
			pf_sys = false;
			pf_capture_policy = CPLoopVars;
			pf_can_skip_non_nullable_argument = false;
			pf_reserved_type_paths = [([],"Object");([],"Error")];
		}
	| Php ->
		if php7 com then
			{
				default_config with
				pf_static = false;
			}
		else
			{
				default_config with
				pf_static = false;
				pf_pad_nulls = true;
			}
	| Cpp ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_add_final_return = true;
		}
	| Cs ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_overload = true;
		}
	| Java ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_overload = true;
		}
	| Python ->
		{
			default_config with
			pf_static = false;
			pf_capture_policy = CPLoopVars;
		}
	| Hl ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_can_skip_non_nullable_argument = false;
		}

let memory_marker = [|Unix.time()|]

let create_callbacks () =
	{
		after_typing = [];
		before_dce = [];
		after_generation = [];
	}

let create version s_version args =
	let m = Type.mk_mono() in
	let defines =
		PMap.add "true" "1" (
		PMap.add "source-header" ("Generated by Haxe " ^ s_version) (
		if !display_default <> DisplayMode.DMNone then PMap.add "display" "1" PMap.empty else PMap.empty))
	in
	{
		version = version;
		args = args;
		shared = {
			shared_display_information = {
				import_positions = PMap.empty;
				diagnostics_messages = [];
				type_hints = Hashtbl.create 0;
				document_symbols = [];
				removable_code = [];
			}
		};
		display_information = {
			unresolved_identifiers = [];
			interface_field_implementations = [];
		};
		sys_args = args;
		debug = false;
		display = DisplayMode.create !display_default;
		verbose = false;
		foptimize = true;
		features = Hashtbl.create 0;
		platform = Cross;
		config = default_config;
		print = (fun s -> print_string s; flush stdout);
		run_command = Sys.command;
		std_path = [];
		class_path = [];
		main_class = None;
		defines = defines;
		package_rules = PMap.empty;
		file = "";
		types = [];
		callbacks = create_callbacks();
		modules = [];
		main = None;
		flash_version = 10.;
		resources = Hashtbl.create 0;
		php_front = None;
		php_lib = None;
		swf_libs = [];
		java_libs = [];
		net_libs = [];
		net_std = [];
		net_path_map = Hashtbl.create 0;
		c_args = [];
		neko_libs = [];
		include_files = [];
		php_prefix = None;
		js_gen = None;
		load_extern_type = [];
		defines_signature = None;
		get_macros = (fun() -> None);
		warning = (fun _ _ -> assert false);
		error = (fun _ _ -> assert false);
		basic = {
			tvoid = m;
			tint = m;
			tfloat = m;
			tbool = m;
			tnull = (fun _ -> assert false);
			tstring = m;
			tarray = (fun _ -> assert false);
		};
		file_lookup_cache = Hashtbl.create 0;
		stored_typed_exprs = PMap.empty;
		cached_macros = Hashtbl.create 0;
		memory_marker = memory_marker;
		parser_cache = Hashtbl.create 0;
	}

let log com str =
	if com.verbose then com.print (str ^ "\n")

let clone com =
	let t = com.basic in
	{ com with
		basic = { t with tvoid = t.tvoid };
		main_class = None;
		features = Hashtbl.create 0;
		file_lookup_cache = Hashtbl.create 0;
		parser_cache = Hashtbl.create 0 ;
		callbacks = create_callbacks();
		display_information = {
			unresolved_identifiers = [];
			interface_field_implementations = [];
		};
	}

let file_time file = Extc.filetime file

let file_extension file =
	match List.rev (ExtString.String.nsplit file ".") with
	| e :: _ -> String.lowercase e
	| [] -> ""

let flash_versions = List.map (fun v ->
	let maj = int_of_float v in
	let min = int_of_float (mod_float (v *. 10.) 10.) in
	v, string_of_int maj ^ (if min = 0 then "" else "_" ^ string_of_int min)
) [9.;10.;10.1;10.2;10.3;11.;11.1;11.2;11.3;11.4;11.5;11.6;11.7;11.8;11.9;12.0;13.0;14.0;15.0;16.0;17.0]

let flash_version_tag = function
	| 6. -> 6
	| 7. -> 7
	| 8. -> 8
	| 9. -> 9
	| 10. | 10.1 -> 10
	| 10.2 -> 11
	| 10.3 -> 12
	| 11. -> 13
	| 11.1 -> 14
	| 11.2 -> 15
	| 11.3 -> 16
	| 11.4 -> 17
	| 11.5 -> 18
	| 11.6 -> 19
	| 11.7 -> 20
	| 11.8 -> 21
	| 11.9 -> 22
	| v when v >= 12.0 && float_of_int (int_of_float v) = v -> int_of_float v + 11
	| v -> failwith ("Invalid SWF version " ^ string_of_float v)

let raw_defined ctx v =
	PMap.mem v ctx.defines

let defined ctx v =
	raw_defined ctx (fst (Define.infos v))

let raw_defined_value ctx k =
	PMap.find k ctx.defines

let defined_value ctx v =
	raw_defined_value ctx (fst (Define.infos v))

let defined_value_safe ctx v =
	try defined_value ctx v
	with Not_found -> ""

let raw_define ctx v =
	let k,v = try ExtString.String.split v "=" with _ -> v,"1" in
	ctx.defines <- PMap.add k v ctx.defines;
	let k = String.concat "_" (ExtString.String.nsplit k "-") in
	ctx.defines <- PMap.add k v ctx.defines;
	ctx.defines_signature <- None

let define_value ctx k v =
	raw_define ctx (fst (Define.infos k) ^ "=" ^ v)

let define ctx v =
	raw_define ctx (fst (Define.infos v))

let init_platform com pf =
	com.platform <- pf;
	let name = platform_name pf in
	let forbid acc p = if p = name || PMap.mem p acc then acc else PMap.add p Forbidden acc in
	com.package_rules <- List.fold_left forbid com.package_rules (List.map platform_name platforms);
	com.config <- get_config com;
(*	if com.config.pf_static then define com "static"; *)
	if com.config.pf_sys then define com Define.Sys else com.package_rules <- PMap.add "sys" Forbidden com.package_rules;
	raw_define com name

let add_feature com f =
	Hashtbl.replace com.features f true

let has_dce com =
	(try defined_value com Define.Dce <> "no" with Not_found -> false)

(*
	TODO: The has_dce check is there because we mark types with @:directlyUsed in the DCE filter,
	which is not run in dce=no and thus we can't know if a type is used directly or not,
	so we just assume that they are.

	If we had dce filter always running (even with dce=no), we would have types marked with @:directlyUsed
	and we wouldn't need to generate unnecessary imports in dce=no, but that's good enough for now.
*)
let is_directly_used com meta =
	not (has_dce com) || Meta.has Meta.DirectlyUsed meta

let rec has_feature com f =
	try
		Hashtbl.find com.features f
	with Not_found ->
		if com.types = [] then not (has_dce com) else
		match List.rev (ExtString.String.nsplit f ".") with
		| [] -> assert false
		| [cl] -> has_feature com (cl ^ ".*")
		| meth :: cl :: pack ->
			let r = (try
				let path = List.rev pack, cl in
				(match List.find (fun t -> t_path t = path && not (Meta.has Meta.RealPath (t_infos t).mt_meta)) com.types with
				| t when meth = "*" -> (match t with TAbstractDecl a -> Meta.has Meta.ValueUsed a.a_meta | _ ->
					Meta.has Meta.Used (t_infos t).mt_meta)
				| TClassDecl ({cl_extern = true} as c) when com.platform <> Js || cl <> "Array" && cl <> "Math" ->
					Meta.has Meta.Used (try PMap.find meth c.cl_statics with Not_found -> PMap.find meth c.cl_fields).cf_meta
				| TClassDecl c ->
					PMap.exists meth c.cl_statics || PMap.exists meth c.cl_fields
				| _ ->
					false)
			with Not_found ->
				false
			) in
			let r = r || not (has_dce com) in
			Hashtbl.add com.features f r;
			r

let allow_package ctx s =
	try
		if (PMap.find s ctx.package_rules) = Forbidden then ctx.package_rules <- PMap.remove s ctx.package_rules
	with Not_found ->
		()

let abort msg p = raise (Abort (msg,p))

let platform ctx p = ctx.platform = p

let add_typing_filter ctx f =
	ctx.callbacks.after_typing <- f :: ctx.callbacks.after_typing

let add_filter ctx f =
	ctx.callbacks.before_dce <- f :: ctx.callbacks.before_dce

let add_final_filter ctx f =
	ctx.callbacks.after_generation <- f :: ctx.callbacks.after_generation

let find_file ctx f =
	try
		(match Hashtbl.find ctx.file_lookup_cache f with
		| None -> raise Exit
		| Some f -> f)
	with Exit ->
		raise Not_found
	| Not_found ->
		let rec loop had_empty = function
			| [] when had_empty -> raise Not_found
			| [] -> loop true [""]
			| p :: l ->
				let file = p ^ f in
				if Sys.file_exists file then begin
					(try
						let ext = String.rindex file '.' in
						let file_pf = String.sub file 0 (ext + 1) ^ platform_name ctx.platform ^ String.sub file ext (String.length file - ext) in
						if not (defined ctx Define.CoreApi) && Sys.file_exists file_pf then file_pf else file
					with Not_found ->
						file)
				end else
					loop (had_empty || p = "") l
		in
		let r = (try Some (loop false ctx.class_path) with Not_found -> None) in
		Hashtbl.add ctx.file_lookup_cache f r;
		(match r with
		| None -> raise Not_found
		| Some f -> f)

let rec mkdir_recursive base dir_list =
	match dir_list with
	| [] -> ()
	| dir :: remaining ->
		let path = match base with
				   | "" ->  dir
				   | "/" -> "/" ^ dir
				   | _ -> base ^ "/" ^ dir
		in
		if not ( (path = "") || ( ((String.length path) = 2) && ((String.sub path 1 1) = ":") ) ) then
			if not (Sys.file_exists path) then
				Unix.mkdir path 0o755;
		mkdir_recursive (if (path = "") then "/" else path) remaining

let mkdir_from_path path =
	let parts = Str.split_delim (Str.regexp "[\\/]+") path in
	match parts with
		| [] -> (* path was "" *) ()
		| _ ->
			let dir_list = List.rev (List.tl (List.rev parts)) in
			mkdir_recursive "" dir_list

let mem_size v =
	Objsize.size_with_headers (Objsize.objsize v [] [])

(* ------------------------- TIMERS ----------------------------- *)

type timer_infos = {
	id : string list;
	mutable start : float list;
	mutable total : float;
	mutable calls : int;
}

let get_time = Extc.time
let htimers = Hashtbl.create 0

let new_timer id =
	let key = String.concat "." id in
	try
		let t = Hashtbl.find htimers key in
		t.start <- get_time() :: t.start;
		t.calls <- t.calls + 1;
		t
	with Not_found ->
		let t = { id = id; start = [get_time()]; total = 0.; calls = 1; } in
		Hashtbl.add htimers key t;
		t

let curtime = ref []

let close t =
	let start = (match t.start with
		| [] -> assert false
		| s :: l -> t.start <- l; s
	) in
	let now = get_time() in
	let dt = now -. start in
	t.total <- t.total +. dt;
	let rec loop() =
		match !curtime with
		| [] -> failwith ("Timer " ^ (String.concat "." t.id) ^ " closed while not active")
		| tt :: l -> curtime := l; if t != tt then loop()
	in
	loop();
	(* because of rounding errors while adding small times, we need to make sure that we don't have start > now *)
	List.iter (fun ct -> ct.start <- List.map (fun t -> let s = t +. dt in if s > now then now else s) ct.start) !curtime

let timer id =
	let t = new_timer id in
	curtime := t :: !curtime;
	(function() -> close t)

let rec close_times() =
	match !curtime with
	| [] -> ()
	| t :: _ -> close t; close_times()

;;

(*  Taken from OCaml source typing/oprint.ml

	This is a better version of string_of_float which prints without loss of precision
	so that float_of_string (float_repres x) = x for all floats x
*)
let valid_float_lexeme s =
	let l = String.length s in
	let rec loop i =
		if i >= l then s ^ "." else
		match s.[i] with
		| '0' .. '9' | '-' -> loop (i+1)
		| _ -> s
	in loop 0

let float_repres f =
	match classify_float f with
	| FP_nan -> "nan"
	| FP_infinite ->
		if f < 0.0 then "neg_infinity" else "infinity"
	| _ ->
		let float_val =
			let s1 = Printf.sprintf "%.12g" f in
			if f = float_of_string s1 then s1 else
			let s2 = Printf.sprintf "%.15g" f in
			if f = float_of_string s2 then s2 else
			Printf.sprintf "%.18g" f
		in valid_float_lexeme float_val

let add_diagnostics_message com s p sev =
	let di = com.shared.shared_display_information in
	di.diagnostics_messages <- (s,p,sev) :: di.diagnostics_messages

open Printer

let dump_context com = s_record_fields "" [
	"version",string_of_int com.version;
	"args",s_list ", " (fun s -> s) com.args;
	"debug",string_of_bool com.debug;
	"platform",platform_name com.platform;
	"std_path",s_list ", " (fun s -> s) com.std_path;
	"class_path",s_list ", " (fun s -> s) com.class_path;
	"defines",s_pmap (fun s -> s) (fun s -> s) com.defines;
	"defines_signature",s_opt (fun s -> Digest.to_hex s) com.defines_signature;
]
