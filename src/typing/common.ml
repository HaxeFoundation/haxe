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

type package_rule =
	| Forbidden
	| Directory of string
	| Remap of string

type pos = Ast.pos

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

type platform =
	| Cross
	| Js
	| Lua
	| Neko
	| Flash
	| Php
	| Cpp
	| Cs
	| Java
	| Python
	| Hl

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

type display_mode =
	| DMNone
	| DMDefault
	| DMUsage
	| DMPosition
	| DMToplevel
	| DMResolve of string
	| DMPackage
	| DMType
	| DMModuleSymbols
	| DMDiagnostics

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
		| ITGlobal of module_type * string * Type.t
		| ITType of module_type
		| ITPackage of string

	let get_name = function
		| ITLocal v -> v.v_name
		| ITMember(_,cf) | ITStatic(_,cf) -> cf.cf_name
		| ITEnum(_,ef) -> ef.ef_name
		| ITGlobal(_,s,_) -> s
		| ITType mt -> snd (t_infos mt).mt_path
		| ITPackage s -> s
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

type shared_display_information = {
	mutable import_positions : (pos,bool ref) PMap.t;
	mutable diagnostics_messages : (string * pos * DiagnosticsSeverity.t) list;
}

type display_information = {
	mutable unresolved_identifiers : (string * pos * (string * IdentifierType.t) list) list;
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
	mutable display : display_mode;
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

exception Abort of string * Ast.pos

let display_default = ref DMNone

type cache = {
	mutable c_haxelib : (string list, string list) Hashtbl.t;
	mutable c_files : (string, float * Ast.package) Hashtbl.t;
	mutable c_modules : (path * string, module_def) Hashtbl.t;
}

let global_cache : cache option ref = ref None

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
		| IncludePrefix
		| Interp
		| JavaVer
		| JqueryVer
		| JsClassic
		| JsEs
		| JsUnflatten
		| KeepOldOutput
		| LoopUnrollMaxCost
		| LuaVer
		| LuaJit
		| Macro
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
		| IncludePrefix -> ("include_prefix","prepend path to generated include files")
		| Interp -> ("interp","The code is compiled to be run with --interp")
		| JavaVer -> ("java_ver", "<version:5-7> Sets the Java version to be targeted")
		| JqueryVer -> ("jquery_ver", "The jQuery version supported by js.jquery.*. The version is encoded as an interger. e.g. 1.11.3 is encoded as 11103")
		| JsClassic -> ("js_classic","Don't use a function wrapper and strict mode in JS output")
		| JsEs -> ("js_es","Generate JS compilant with given ES standard version (default 5)")
		| JsUnflatten -> ("js_unflatten","Generate nested objects for packages and types")
		| KeepOldOutput -> ("keep_old_output","Keep old source files in the output directory (for C#/Java)")
		| LoopUnrollMaxCost -> ("loop_unroll_max_cost","Maximum cost (number of expressions * iterations) before loop unrolling is canceled (default 250)")
		| LuaJit -> ("lua_jit","Enable the jit compiler for lua (version 5.2 only")
		| LuaVer -> ("lua_ver","The lua version to target")
		| Macro -> ("macro","Defined when code is compiled in the macro context")
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

module MetaInfo = struct
	open Meta
	type meta_usage =
		| TClass
		| TClassField
		| TAbstract
		| TAbstractField
		| TEnum
		| TTypedef
		| TAnyField
		| TExpr
		| TTypeParameter

	type meta_parameter =
		| HasParam of string
		| Platform of platform
		| Platforms of platform list
		| UsedOn of meta_usage
		| UsedOnEither of meta_usage list
		| Internal

	let to_string = function
		| Abi -> ":abi",("Function ABI/calling convention",[Platforms [Cpp]])
		| Abstract -> ":abstract",("Sets the underlying class implementation as 'abstract'",[Platforms [Java;Cs]])
		| Access -> ":access",("Forces private access to package, type or field",[HasParam "Target path";UsedOnEither [TClass;TClassField]])
		| Accessor -> ":accessor",("Used internally by DCE to mark property accessors",[UsedOn TClassField;Internal])
		| Allow -> ":allow",("Allows private access from package, type or field",[HasParam "Target path";UsedOnEither [TClass;TClassField]])
		| Analyzer -> ":analyzer",("Used to configure the static analyzer",[])
		| Annotation -> ":annotation",("Annotation (@interface) definitions on -java-lib imports will be annotated with this metadata. Has no effect on types compiled by Haxe",[Platform Java; UsedOn TClass])
		| ArrayAccess -> ":arrayAccess",("Allows [] access on an abstract",[UsedOnEither [TAbstract;TAbstractField]])
		| Ast -> ":ast",("Internally used to pass the AST source into the typed AST",[Internal])
		| AstSource -> ":astSource",("Filled by the compiler with the parsed expression of the field",[UsedOn TClassField])
		| AutoBuild -> ":autoBuild",("Extends @:build metadata to all extending and implementing classes",[HasParam "Build macro call";UsedOn TClass])
		| Bind -> ":bind",("Override Swf class declaration",[Platform Flash;UsedOn TClass])
		| Bitmap -> ":bitmap",("Embeds given bitmap data into the class (must extend flash.display.BitmapData)",[HasParam "Bitmap file path";UsedOn TClass;Platform Flash])
		| BridgeProperties -> ":bridgeProperties",("Creates native property bridges for all Haxe properties in this class",[UsedOn TClass;Platform Cs])
		| Build -> ":build",("Builds a class or enum from a macro",[HasParam "Build macro call";UsedOnEither [TClass;TEnum]])
		| BuildXml -> ":buildXml",("Specify xml data to be injected into Build.xml",[Platform Cpp])
		| Callable -> ":callable",("Abstract forwards call to its underlying type",[UsedOn TAbstract])
		| Class -> ":class",("Used internally to annotate an enum that will be generated as a class",[Platforms [Java;Cs]; UsedOn TEnum; Internal])
		| ClassCode -> ":classCode",("Used to inject platform-native code into a class",[Platforms [Java;Cs]; UsedOn TClass])
		| Commutative -> ":commutative",("Declares an abstract operator as commutative",[UsedOn TAbstractField])
		| CompilerGenerated -> ":compilerGenerated",("Marks a field as generated by the compiler. Shouldn't be used by the end user",[Platforms [Java;Cs]])
		| Const -> ":const",("Allows a type parameter to accept expression values",[UsedOn TTypeParameter])
		| CoreApi -> ":coreApi",("Identifies this class as a core api class (forces Api check)",[UsedOnEither [TClass;TEnum;TTypedef;TAbstract]])
		| CoreType -> ":coreType",("Identifies an abstract as core type so that it requires no implementation",[UsedOn TAbstract])
		| CppFileCode -> ":cppFileCode",("Code to be injected into generated cpp file",[Platform Cpp])
		| CppInclude -> ":cppInclude",("File to be included in generated cpp file",[Platform Cpp])
		| CppNamespaceCode -> ":cppNamespaceCode",("",[Platform Cpp])
		| CsNative -> ":csNative",("Automatically added by -net-lib on classes generated from .NET DLL files",[Platform Cs; UsedOnEither[TClass;TEnum]; Internal])
		| Dce -> ":dce",("Forces dead code elimination even when -dce full is not specified",[UsedOnEither [TClass;TEnum]])
		| Debug -> ":debug",("Forces debug information to be generated into the Swf even without -debug",[UsedOnEither [TClass;TClassField]; Platform Flash])
		| Decl -> ":decl",("",[Platform Cpp])
		| DefParam -> ":defParam",("?",[])
		| Delegate -> ":delegate",("Automatically added by -net-lib on delegates",[Platform Cs; UsedOn TAbstract])
		| Depend -> ":depend",("",[Platform Cpp])
		| Deprecated -> ":deprecated",("Mark a type or field as deprecated",[])
		| DirectlyUsed -> ":directlyUsed",("Marks types that are directly referenced by non-extern code",[Internal])
		| DynamicObject -> ":dynamicObject",("Used internally to identify the Dynamic Object implementation",[Platforms [Java;Cs]; UsedOn TClass; Internal])
		| Eager -> ":eager",("Forces typedefs to be followed early",[UsedOn TTypedef])
		| Enum -> ":enum",("Defines finite value sets to abstract definitions",[UsedOn TAbstract])
		| EnumConstructorParam -> ":enumConstructorParam",("Used internally to annotate GADT type parameters",[UsedOn TClass; Internal])
		| Event -> ":event",("Automatically added by -net-lib on events. Has no effect on types compiled by Haxe",[Platform Cs; UsedOn TClassField])
		| Exhaustive -> ":exhaustive",("",[Internal])
		| Expose -> ":expose",("Makes the class available on the window object",[HasParam "?Name=Class path";UsedOn TClass;Platform Js])
		| Extern -> ":extern",("Marks the field as extern so it is not generated",[UsedOn TClassField])
		| FakeEnum -> ":fakeEnum",("Treat enum as collection of values of the specified type",[HasParam "Type name";UsedOn TEnum])
		| File -> ":file",("Includes a given binary file into the target Swf and associates it with the class (must extend flash.utils.ByteArray)",[HasParam "File path";UsedOn TClass;Platform Flash])
		| FileXml -> ":fileXml",("Include xml attribute snippet in Build.xml entry for file",[UsedOn TClass;Platform Cpp])
		| Final -> ":final",("Prevents a class from being extended",[UsedOn TClass])
		| Fixed -> ":fixed",("Delcares an anonymous object to have fixed fields",[ (*UsedOn TObjectDecl(_)*)])
		| FlatEnum -> ":flatEnum",("Internally used to mark an enum as being flat, i.e. having no function constructors",[UsedOn TEnum; Internal])
		| Font -> ":font",("Embeds the given TrueType font into the class (must extend flash.text.Font)",[HasParam "TTF path";HasParam "Range String";UsedOn TClass])
		| Forward -> ":forward",("Forwards field access to underlying type",[HasParam "List of field names";UsedOn TAbstract])
		| ForwardStatics -> ":forwardStatics",("Forwards static field access to underlying type",[HasParam "List of field names";UsedOn TAbstract])
		| From -> ":from",("Specifies that the field of the abstract is a cast operation from the type identified in the function",[UsedOn TAbstractField])
		| FunctionCode -> ":functionCode",("Used to inject platform-native code into a function",[Platforms [Cpp;Java;Cs]])
		| FunctionTailCode -> ":functionTailCode",("",[Platform Cpp])
		| Generic -> ":generic",("Marks a class or class field as generic so each type parameter combination generates its own type/field",[UsedOnEither [TClass;TClassField]])
		| GenericBuild -> ":genericBuild",("Builds instances of a type using the specified macro",[UsedOn TClass])
		| GenericInstance -> ":genericInstance",("Internally used to mark instances of @:generic methods",[UsedOn TClassField;Internal])
		| Getter -> ":getter",("Generates a native getter function on the given field",[HasParam "Class field name";UsedOn TClassField;Platform Flash])
		| Hack -> ":hack",("Allows extending classes marked as @:final",[UsedOn TClass])
		| HasUntyped -> (":has_untyped",("Used by the typer to mark fields that have untyped expressions",[Internal]))
		| HaxeGeneric -> ":haxeGeneric",("Used internally to annotate non-native generic classes",[Platform Cs; UsedOnEither[TClass;TEnum]; Internal])
		| HeaderClassCode -> ":headerClassCode",("Code to be injected into the generated class, in the header",[Platform Cpp])
		| HeaderCode -> ":headerCode",("Code to be injected into the generated header file",[Platform Cpp])
		| HeaderInclude -> ":headerInclude",("File to be included in generated header file",[Platform Cpp])
		| HeaderNamespaceCode -> ":headerNamespaceCode",("",[Platform Cpp])
		| HxGen -> ":hxGen",("Annotates that an extern class was generated by Haxe",[Platforms [Java;Cs]; UsedOnEither [TClass;TEnum]])
		| IfFeature -> ":ifFeature",("Causes a field to be kept by DCE if the given feature is part of the compilation",[HasParam "Feature name";UsedOn TClassField])
		| Impl -> ":impl",("Used internally to mark abstract implementation fields",[UsedOn TAbstractField; Internal])
		| PythonImport -> ":pythonImport",("Generates python import statement for extern classes",[Platforms [Python]; UsedOn TClass])
		| ImplicitCast -> ":implicitCast",("Generated automatically on the AST when an implicit abstract cast happens",[Internal; UsedOn TExpr])
		| Include -> ":include",("",[Platform Cpp])
		| InitPackage -> ":initPackage",("?",[])
		| InlineConstructorVariable -> ":inlineConstructorVariable",("Internally used to mark variables that come from inlined constructors",[Internal])
		| Meta.Internal -> ":internal",("Generates the annotated field/class with 'internal' access",[Platforms [Java;Cs]; UsedOnEither[TClass;TEnum;TClassField]])
		| IsVar -> ":isVar",("Forces a physical field to be generated for properties that otherwise would not require one",[UsedOn TClassField])
		| JavaCanonical -> ":javaCanonical",("Used by the Java target to annotate the canonical path of the type",[HasParam "Output type package";HasParam "Output type name";UsedOnEither [TClass;TEnum]; Platform Java])
		| JavaNative -> ":javaNative",("Automatically added by -java-lib on classes generated from JAR/class files",[Platform Java; UsedOnEither[TClass;TEnum]; Internal])
		| JsRequire -> ":jsRequire",("Generate javascript module require expression for given extern",[Platform Js; UsedOn TClass])
		| LuaRequire -> ":luaRequire",("Generate lua module require expression for given extern",[Platform Lua; UsedOn TClass])
		| Keep -> ":keep",("Causes a field or type to be kept by DCE",[])
		| KeepInit -> ":keepInit",("Causes a class to be kept by DCE even if all its field are removed",[UsedOn TClass])
		| KeepSub -> ":keepSub",("Extends @:keep metadata to all implementing and extending classes",[UsedOn TClass])
		| LibType -> ":libType",("Used by -net-lib and -java-lib to mark a class that shouldn't be checked (overrides, interfaces, etc) by the type loader",[Internal; UsedOn TClass; Platforms [Java;Cs]])
		| Meta -> ":meta",("Internally used to mark a class field as being the metadata field",[])
		| Macro -> ":macro",("(deprecated)",[])
		| MaybeUsed -> ":maybeUsed",("Internally used by DCE to mark fields that might be kept",[Internal])
		| MergeBlock -> ":mergeBlock",("Merge the annotated block into the current scope",[UsedOn TExpr])
		| MultiType -> ":multiType",("Specifies that an abstract chooses its this-type from its @:to functions",[UsedOn TAbstract; HasParam "Relevant type parameters"])
		| Native -> ":native",("Rewrites the path of a class or enum during generation",[HasParam "Output type path";UsedOnEither [TClass;TEnum]])
		| NativeChildren -> ":nativeChildren",("Annotates that all children from a type should be treated as if it were an extern definition - platform native",[Platforms [Java;Cs]; UsedOn TClass])
		| NativeGen -> ":nativeGen",("Annotates that a type should be treated as if it were an extern definition - platform native",[Platforms [Java;Cs;Python]; UsedOnEither[TClass;TEnum]])
		| NativeGeneric -> ":nativeGeneric",("Used internally to annotate native generic classes",[Platform Cs; UsedOnEither[TClass;TEnum]; Internal])
		| NativeProperty -> ":nativeProperty",("Use native properties which will execute even with dynamic usage",[Platform Cpp])
		| NativeStaticExtension -> ":nativeStaticExtension",("Converts static function syntax into member call",[Platform Cpp])
		| NoCompletion -> ":noCompletion",("Prevents the compiler from suggesting completion on this field",[UsedOn TClassField])
		| NoDebug -> ":noDebug",("Does not generate debug information into the Swf even if -debug is set",[UsedOnEither [TClass;TClassField];Platform Flash])
		| NoDoc -> ":noDoc",("Prevents a type from being included in documentation generation",[])
		| NoExpr -> ":noExpr",("Internally used to mark abstract fields which have no expression by design",[Internal])
		| NoImportGlobal -> ":noImportGlobal",("Prevents a static field from being imported with import Class.*",[UsedOn TAnyField])
		| NonVirtual -> ":nonVirtual",("Declares function to be non-virtual in cpp",[Platform Cpp])
		| NoPackageRestrict -> ":noPackageRestrict",("Allows a module to be accessed across all targets if found on its first type",[Internal])
		| NoPrivateAccess -> ":noPrivateAccess",("Disallow private access to anything for the annotated expression",[UsedOn TExpr])
		| NoStack -> ":noStack",("",[Platform Cpp])
		| NotNull -> ":notNull",("Declares an abstract type as not accepting null values",[UsedOn TAbstract])
		| NoUsing -> ":noUsing",("Prevents a field from being used with 'using'",[UsedOn TClassField])
		| Ns -> ":ns",("Internally used by the Swf generator to handle namespaces",[Platform Flash])
		| Objc -> ":objc",("Declares a class or interface that is used to interoperate with Objective-C code",[Platform Cpp;UsedOn TClass])
		| Op -> ":op",("Declares an abstract field as being an operator overload",[HasParam "The operation";UsedOn TAbstractField])
		| Optional -> ":optional",("Marks the field of a structure as optional",[UsedOn TClassField])
		| Overload -> ":overload",("Allows the field to be called with different argument types",[HasParam "Function specification (no expression)";UsedOn TClassField])
		| PhpConstants -> ":phpConstants",("Marks the static fields of a class as PHP constants, without $",[Platform Php;UsedOn TClass])
		| PhpGlobal -> ":phpGlobal",("Puts the static fields of a class in the global PHP namespace",[Platform Php;UsedOn TClass])
		| Public -> ":public",("Marks a class field as being public",[UsedOn TClassField;Internal])
		| PublicFields -> ":publicFields",("Forces all class fields of inheriting classes to be public",[UsedOn TClass])
		| QuotedField -> ":quotedField",("Used internally to mark structure fields which are quoted in syntax",[Internal])
		| PrivateAccess -> ":privateAccess",("Allow private access to anything for the annotated expression",[UsedOn TExpr])
		| Protected -> ":protected",("Marks a class field as being protected",[UsedOn TClassField;Platforms [Cs;Java;Flash]])
		| Property -> ":property",("Marks a property field to be compiled as a native C# property",[UsedOn TClassField;Platform Cs])
		| Pure -> ":pure",("Marks a class field, class or expression as pure (side-effect free)",[UsedOnEither [TClass;TClassField;TExpr]])
		| ReadOnly -> ":readOnly",("Generates a field with the 'readonly' native keyword",[Platform Cs; UsedOn TClassField])
		| RealPath -> ":realPath",("Internally used on @:native types to retain original path information",[Internal])
		| Remove -> ":remove",("Causes an interface to be removed from all implementing classes before generation",[UsedOn TClass])
		| Require -> ":require",("Allows access to a field only if the specified compiler flag is set",[HasParam "Compiler flag to check";UsedOn TClassField])
		| RequiresAssign -> ":requiresAssign",("Used internally to mark certain abstract operator overloads",[Internal])
		| Resolve -> ":resolve",("Abstract fields marked with this metadata can be used to resolve unknown fields",[UsedOn TClassField])
		| ReplaceReflection -> ":replaceReflection",("Used internally to specify a function that should replace its internal __hx_functionName counterpart",[Platforms [Java;Cs]; UsedOnEither[TClass;TEnum]; Internal])
		| Rtti -> ":rtti",("Adds runtime type informations",[UsedOn TClass])
		| Runtime -> ":runtime",("?",[])
		| RuntimeValue -> ":runtimeValue",("Marks an abstract as being a runtime value",[UsedOn TAbstract])
		| Scalar -> ":scalar",("Used by hxcpp to mark a custom coreType abstract",[UsedOn TAbstract; Platform Cpp])
		| SelfCall -> ":selfCall",("Translates method calls into calling object directly",[UsedOn TClassField; Platform Js])
		| Setter -> ":setter",("Generates a native setter function on the given field",[HasParam "Class field name";UsedOn TClassField;Platform Flash])
		| StackOnly -> ":stackOnly",("Instances of this type can only appear on the stack",[Platform Cpp])
		| StoredTypedExpr -> ":storedTypedExpr",("Used internally to reference a typed expression returned from a macro",[Internal])
		| SkipCtor -> ":skipCtor",("Used internally to generate a constructor as if it were a native type (no __hx_ctor)",[Platforms [Java;Cs]; Internal])
		| SkipReflection -> ":skipReflection",("Used internally to annotate a field that shouldn't have its reflection data generated",[Platforms [Java;Cs]; UsedOn TClassField; Internal])
		| Sound -> ":sound",( "Includes a given .wav or .mp3 file into the target Swf and associates it with the class (must extend flash.media.Sound)",[HasParam "File path";UsedOn TClass;Platform Flash])
		| SourceFile -> ":sourceFile",("Source code filename for external class",[Platform Cpp])
		| Strict -> ":strict",("Used to declare a native C# attribute or a native Java metadata. Is type checked",[Platforms [Java;Cs]])
		| Struct -> ":struct",("Marks a class definition as a struct",[Platform Cs; UsedOn TClass])
		| StructAccess -> ":structAccess",("Marks an extern class as using struct access('.') not pointer('->')",[Platform Cpp; UsedOn TClass])
		| StructInit -> ":structInit",("Allows to initialize the class with a structure that matches constructor parameters",[UsedOn TClass])
		| SuppressWarnings -> ":suppressWarnings",("Adds a SuppressWarnings annotation for the generated Java class",[Platform Java; UsedOn TClass])
		| TemplatedCall -> ":templatedCall",("Indicates that the first parameter of static call should be treated as a template arguement",[Platform Cpp; UsedOn TClassField])
		| Throws -> ":throws",("Adds a 'throws' declaration to the generated function",[HasParam "Type as String"; Platform Java; UsedOn TClassField])
		| This -> ":this",("Internally used to pass a 'this' expression to macros",[Internal; UsedOn TExpr])
		| To -> ":to",("Specifies that the field of the abstract is a cast operation to the type identified in the function",[UsedOn TAbstractField])
		| ToString -> ":toString",("Internally used",[Internal])
		| Transient -> ":transient",("Adds the 'transient' flag to the class field",[Platform Java; UsedOn TClassField])
		| ValueUsed -> ":valueUsed",("Internally used by DCE to mark an abstract value as used",[Internal])
		| Volatile -> ":volatile",("",[Platforms [Java;Cs]])
		| Unbound -> ":unbound", ("Compiler internal to denote unbounded global variable",[])
		| UnifyMinDynamic -> ":unifyMinDynamic",("Allows a collection of types to unify to Dynamic",[UsedOn TClassField])
		| Unreflective -> ":unreflective",("",[Platform Cpp])
		| Unsafe -> ":unsafe",("Declares a class, or a method with the C#'s 'unsafe' flag",[Platform Cs; UsedOnEither [TClass;TClassField]])
		| Usage -> ":usage",("?",[])
		| Used -> ":used",("Internally used by DCE to mark a class or field as used",[Internal])
		| UserVariable -> ":userVariable",("Internally used to mark variables that come from user code",[Internal])
		| Value -> ":value",("Used to store default values for fields and function arguments",[UsedOn TClassField])
		| Void -> ":void",("Use Cpp native 'void' return type",[Platform Cpp])
		| Last -> assert false
		(* do not put any custom metadata after Last *)
		| Dollar s -> "$" ^ s,("",[])
		| Custom s -> s,("",[])

	let hmeta =
		let h = Hashtbl.create 0 in
		let rec loop i =
			let m = Obj.magic i in
			if m <> Last then begin
				Hashtbl.add h (fst (to_string m)) m;
				loop (i + 1);
			end;
		in
		loop 0;
		h

	let parse s = try Hashtbl.find hmeta (":" ^ s) with Not_found -> Custom (":" ^ s)

	let from_string s =
		if s = "" then Custom "" else match s.[0] with
		| ':' -> (try Hashtbl.find hmeta s with Not_found -> Custom s)
		| '$' -> Dollar (String.sub s 1 (String.length s - 1))
		| _ -> Custom s
end

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
		if !display_default <> DMNone then PMap.add "display" "1" PMap.empty else PMap.empty))
	in
	{
		version = version;
		args = args;
		shared = {
			shared_display_information = {
				import_positions = PMap.empty;
				diagnostics_messages = [];
			}
		};
		display_information = {
			unresolved_identifiers = [];
		};
		sys_args = args;
		debug = false;
		display = !display_default;
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
		callbacks = create_callbacks();
	}

let file_time file =
	try (Unix.stat file).Unix.st_mtime with _ -> 0.

let get_signature com =
	match com.defines_signature with
	| Some s -> s
	| None ->
		let defines = PMap.foldi (fun k v acc ->
			(* don't make much difference between these special compilation flags *)
			match String.concat "_" (ExtString.String.nsplit k "-") with
			| "display" | "use_rtti_doc" | "macro_times" | "display_details" | "no_copt" -> acc
			| _ -> (k ^ "=" ^ v) :: acc
		) com.defines [] in
		let str = String.concat "@" (List.sort compare defines) in
		let s = Digest.string str in
		com.defines_signature <- Some s;
		s

let file_extension file =
	match List.rev (ExtString.String.nsplit file ".") with
	| e :: _ -> String.lowercase e
	| [] -> ""

let platforms = [
	Js;
	Lua;
	Neko;
	Flash;
	Php;
	Cpp;
	Cs;
	Java;
	Python;
	Hl;
]

let platform_name = function
	| Cross -> "cross"
	| Js -> "js"
	| Lua -> "lua"
	| Neko -> "neko"
	| Flash -> "flash"
	| Php -> "php"
	| Cpp -> "cpp"
	| Cs -> "cs"
	| Java -> "java"
	| Python -> "python"
	| Hl -> "hl"

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
	not (has_dce com) || Ast.Meta.has Ast.Meta.DirectlyUsed meta

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
				(match List.find (fun t -> t_path t = path && not (Ast.Meta.has Ast.Meta.RealPath (t_infos t).mt_meta)) com.types with
				| t when meth = "*" -> (match t with TAbstractDecl a -> Ast.Meta.has Ast.Meta.ValueUsed a.a_meta | _ ->
					Ast.Meta.has Ast.Meta.Used (t_infos t).mt_meta)
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

let error msg p = raise (Abort (msg,p))

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

let get_full_path f = try Extc.get_full_path f with _ -> f

let unique_full_path = if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then (fun f -> String.lowercase (get_full_path f)) else get_full_path

let get_path_parts f =
	(*
		this function is quite weird: it tries to determine whether the given
		argument is a .hx file path with slashes or a dotted module path and
		based on that it returns path "parts", which are basically a list of
		either folders or packages (which are folders too) appended by the module name

		TODO: i started doubting my sanity while writing this comment, let's somehow
		refactor this stuff so it doesn't mix up file and module paths and doesn't introduce
		the weird "path part" entity.
	*)
	let l = String.length f in
	if l > 3 && (String.sub f (l-3) 3) = ".hx" then
		let f = String.sub f 0 (l-3) in (* strip the .hx *)
		ExtString.String.nsplit (String.concat "/" (ExtString.String.nsplit f "\\")) "/" (* TODO: wouldn't it be faster to Str.split here? *)
	else
		ExtString.String.nsplit f "."

let add_trailing_slash p =
	let l = String.length p in
	if l = 0 then
		"./"
	else match p.[l-1] with
		| '\\' | '/' -> p
		| _ -> p ^ "/"

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
	name : string;
	mutable start : float list;
	mutable total : float;
}

let get_time = Extc.time
let htimers = Hashtbl.create 0

let new_timer name =
	try
		let t = Hashtbl.find htimers name in
		t.start <- get_time() :: t.start;
		t
	with Not_found ->
		let t = { name = name; start = [get_time()]; total = 0.; } in
		Hashtbl.add htimers name t;
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
		| [] -> failwith ("Timer " ^ t.name ^ " closed while not active")
		| tt :: l -> curtime := l; if t != tt then loop()
	in
	loop();
	(* because of rounding errors while adding small times, we need to make sure that we don't have start > now *)
	List.iter (fun ct -> ct.start <- List.map (fun t -> let s = t +. dt in if s > now then now else s) ct.start) !curtime

let timer name =
	let t = new_timer name in
	curtime := t :: !curtime;
	(function() -> close t)

let rec close_times() =
	match !curtime with
	| [] -> ()
	| t :: _ -> close t; close_times()

;;
Ast.Meta.to_string_ref := fun m -> fst (MetaInfo.to_string m)

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
